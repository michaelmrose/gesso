(ns gesso.assurance.proof.xtdb-gateway-issued-erasure-audit-segment
  "v1881: Verify a bounded hash-linked operator-supplied erasure-audit SEGMENT
   against a distinct, process-pinned auditor Ed25519 signed checkpoint.

   This establishes tamper evidence WITHIN the supplied segment and its
   authenticated checkpoint, NOT independent capture at XTDB's erasure/write
   boundary, persistent storage, prevention, completeness of logging, absence
   of omitted segments, or proof that any reported event actually occurred.
   The caller-supplied expected binding is source-relative unless obtained
   by the original checked private witness collector (not wired here)."
  (:require [gesso.assurance.proof.xtdb-gateway-issued-operator-claim-intake :as intake]
            [gesso.assurance.proof.xtdb-gateway-issued-operator-audit-cosign :as auditor])
  (:import [java.io ByteArrayOutputStream DataOutputStream]
           [java.security KeyFactory MessageDigest PublicKey Signature]
           [java.security.spec X509EncodedKeySpec]
           [java.time Instant]
           [java.util Arrays Base64 HexFormat UUID]))

(def report-type :gesso.assurance.proof.xtdb-gateway-issued-erasure-audit-segment/sealed-range)
(def report-version 1)
(def checkpoint-format "gesso.xtdb.history.erase-audit-segment.v1")
(def checkpoint-domain "gesso/xtdb/history/erase-audit-checkpoint/v1")
(def event-domain "gesso/xtdb/history/erase-audit-event/v1")
(def max-records 64)
(def ^:private original-claim-bindings intake/binding-fields)
(def ^:private original-operator-env intake/operator-key-env)
(def ^:private original-auditor-env auditor/auditor-key-env)
(def ^:private event-fields
  #{:sequence :stream-id :database-instance :physical-table :physical-row-id
    :event-id :event-kind :recorded-at :previous-sha256})
(def ^:private allowed-kinds
  #{:erase-attempt-reported :erase-commit-reported :erase-denial-reported})
(def ^:private checkpoint-fields
  #{:format :binding :stream-id :start-sequence :end-sequence
    :start-sha256 :head-sha256 :record-count :signed-at :nonce})

(defn- reject! [reason]
  (throw (ex-info "Pinned auditor erase-segment evidence not established."
                  {:reason reason})))

(defn- plain-map? [m] (and (map? m) (nil? (meta m))))
(defn- bounded? [x] (and (string? x) (<= 1 (count x) 1024)))
(defn- hex256? [x] (and (string? x) (boolean (re-matches #"[0-9a-f]{64}" x))))
(defn- table? [x]
  (and (keyword? x) (nil? (namespace x))
       (boolean (re-matches #"[a-z][a-z0-9_]*" (name x)))))
(defn- index? [x] (and (integer? x) (<= 0 x Long/MAX_VALUE)))

(defn- valid-binding? [binding]
  (and (plain-map? binding)
       (= (set original-claim-bindings) (set (keys binding)))
       (qualified-keyword? (:operation-id binding))
       (every? #(bounded? (get binding %))
               [:proof-fingerprint :proof-source-fingerprint
                :proof-subject-fingerprint :dependency-graph-fingerprint
                :transition-fingerprint :database-instance :physical-row-id])
       (table? (:physical-table binding))
       (instance? Instant (:system-from binding))
       (instance? Instant (:system-to binding))
       (neg? (compare (:system-from binding) (:system-to binding)))))

(defn- valid-event? [event]
  (and (plain-map? event)
       (= event-fields (set (keys event)))
       (index? (:sequence event))
       (bounded? (:stream-id event))
       (bounded? (:database-instance event))
       (table? (:physical-table event))
       (bounded? (:physical-row-id event))
       (instance? UUID (:event-id event))
       (contains? allowed-kinds (:event-kind event))
       (instance? Instant (:recorded-at event))
       (hex256? (:previous-sha256 event))))

(defn- write-value! [^DataOutputStream out value]
  ;; Field order is fixed, all strings and keyword/Instant/UUID encodings
  ;; are bounded and validated before calling this function.
  (.writeUTF out (str value)))

(defn- bytes! [domain values]
  (let [buffer (ByteArrayOutputStream.)
        out (DataOutputStream. buffer)]
    (.writeUTF out domain)
    (doseq [value values] (write-value! out value))
    (.flush out)
    (.toByteArray buffer)))

(defn event-sha256!
  "Public event encoding helper for external audit-record producers. A hash
   over a caller-created event is NOT evidence that an XTDB erase occurred."
  [event]
  (when-not (valid-event? event)
    (reject! :invalid-supplied-audit-event))
  (let [encoded (bytes! event-domain
                        (map event [:sequence :stream-id :database-instance
                                    :physical-table :physical-row-id :event-id
                                    :event-kind :recorded-at :previous-sha256]))]
    (.formatHex (HexFormat/of)
                (.digest (MessageDigest/getInstance "SHA-256") encoded))))

(defn- valid-checkpoint? [checkpoint]
  (and (plain-map? checkpoint)
       (= checkpoint-fields (set (keys checkpoint)))
       (= checkpoint-format (:format checkpoint))
       (valid-binding? (:binding checkpoint))
       (bounded? (:stream-id checkpoint))
       (index? (:start-sequence checkpoint))
       (index? (:end-sequence checkpoint))
       (<= (:start-sequence checkpoint) (:end-sequence checkpoint))
       (hex256? (:start-sha256 checkpoint))
       (hex256? (:head-sha256 checkpoint))
       (index? (:record-count checkpoint))
       (<= (:record-count checkpoint) max-records)
       (= (:end-sequence checkpoint)
          (+ (:start-sequence checkpoint) (:record-count checkpoint)))
       (instance? Instant (:signed-at checkpoint))
       (instance? UUID (:nonce checkpoint))))

(defn canonical-checkpoint-bytes!
  "Fixed-order, versioned checkpoint data for the independently pinned
   auditor to sign. Only the signed HEAD hash (plus anchoring metadata) is
   signed; hash-chain validation binds the supplied records to that head."
  [checkpoint]
  (when-not (valid-checkpoint? checkpoint)
    (reject! :invalid-supplied-audit-checkpoint))
  (bytes! checkpoint-domain
          (concat [(:format checkpoint)]
                  (map (:binding checkpoint) original-claim-bindings)
                  (map checkpoint [:stream-id :start-sequence :end-sequence
                                   :start-sha256 :head-sha256 :record-count
                                   :signed-at :nonce]))))

(defn- pinned-public-key! [env-name reason]
  (let [encoded (System/getenv env-name)]
    (when-not (bounded? encoded) (reject! reason))
    (try
      (.generatePublic (KeyFactory/getInstance "Ed25519")
                       (X509EncodedKeySpec.
                        (.decode (Base64/getDecoder) ^String encoded)))
      (catch Exception _ (reject! :invalid-audit-segment-pinned-key)))))

(defn inspect-pinned-erase-audit-segment!
  "Verify bounded signed chain against an independently configured auditor
   key and original operator key with DISTINCT public key bytes. 'expected'
   must be sourced from an independently checked physical witness by its
   caller; this standalone API cannot authenticate an expected map.

   No segment (even empty and signed) proves audit producer coverage, no
   unseen erasures, no ERASE-path enforcement, or complete history."
  [expected envelope]
  (when-not (and (valid-binding? expected)
                 (plain-map? envelope)
                 (= #{:checkpoint :records :auditor-signature-base64}
                    (set (keys envelope)))
                 (vector? (:records envelope))
                 (<= (count (:records envelope)) max-records))
    (reject! :invalid-audit-segment-request))
  (let [{:keys [checkpoint records auditor-signature-base64]} envelope
        encoded (canonical-checkpoint-bytes! checkpoint)]
    (when-not (= expected (:binding checkpoint))
      (reject! :audit-segment-source-binding-mismatch))
    (when-not (= (count records) (:record-count checkpoint))
      (reject! :audit-segment-record-count-mismatch))
    (let [operator-root (pinned-public-key! original-operator-env
                                            :operator-key-not-configured)
          auditor-root (pinned-public-key! original-auditor-env
                                           :auditor-key-not-configured)]
      (when (Arrays/equals (.getEncoded ^PublicKey operator-root)
                           (.getEncoded ^PublicKey auditor-root))
        (reject! :audit-segment-operator-auditor-keys-must-differ))
      (when-not (and (string? auditor-signature-base64)
                     (<= 1 (count auditor-signature-base64) 256))
        (reject! :invalid-auditor-checkpoint-signature))
      (let [sig-bytes (try (.decode (Base64/getDecoder) ^String auditor-signature-base64)
                           (catch IllegalArgumentException _
                             (reject! :invalid-auditor-checkpoint-signature)))]
        (when-not (= 64 (alength sig-bytes))
          (reject! :invalid-auditor-checkpoint-signature))
        (when-not (try
                    (let [v (Signature/getInstance "Ed25519")]
                      (.initVerify v auditor-root)
                      (.update v encoded)
                      (.verify v sig-bytes))
                    (catch Exception _ false))
          (reject! :invalid-auditor-checkpoint-signature)))
      ;; A valid checkpoint signature is NOT proof of completeness. Check
      ;; every supplied event's strict, gap-free internal sequence and digest.
      (let [binding (:binding checkpoint)
            from (:system-from binding)
            to (:system-to binding)
            result (reduce
                    (fn [{:keys [sequence digest previous-time] :as state}
                         [_index {:keys [event digest-sha256] :as record}]]
                      (when-not (and (plain-map? record)
                                     (= #{:event :digest-sha256} (set (keys record)))
                                     (valid-event? event)
                                     (hex256? digest-sha256))
                        (reject! :invalid-supplied-audit-record))
                      (when-not (and (= (:sequence event) (inc sequence))
                                     (= (:previous-sha256 event) digest)
                                     (= (:stream-id event) (:stream-id checkpoint))
                                     (= (:database-instance event)
                                        (:database-instance binding))
                                     (not (neg? (compare (:recorded-at event) from)))
                                     (not (pos? (compare (:recorded-at event) to)))
                                     (or (nil? previous-time)
                                         (not (neg? (compare (:recorded-at event)
                                                             previous-time)))))
                        (reject! :audit-segment-gap-target-or-time-mismatch))
                      (when (contains? (:seen-events state) (:event-id event))
                        (reject! :audit-segment-duplicate-event-id))
                      (when-not (= digest-sha256 (event-sha256! event))
                        (reject! :audit-segment-entry-digest-mismatch))
                      {:sequence (:sequence event)
                       :digest digest-sha256
                       :previous-time (:recorded-at event)
                       :seen-events (conj (:seen-events state) (:event-id event))
                       :selected-erase?
                       (or (:selected-erase? state)
                           (and (= :erase-commit-reported (:event-kind event))
                                (= (:physical-table event) (:physical-table binding))
                                (= (:physical-row-id event) (:physical-row-id binding))))})
                    {:sequence (:start-sequence checkpoint)
                     :digest (:start-sha256 checkpoint)
                     :previous-time nil
                     :seen-events #{}
                     :selected-erase? false}
                    (map-indexed vector records))]
        (when-not (and (= (:sequence result) (:end-sequence checkpoint))
                       (= (:digest result) (:head-sha256 checkpoint)))
          (reject! :audit-segment-final-checkpoint-mismatch))
        {:type report-type :version report-version
         :signature-integrity :valid-distinct-auditor-ed25519-checkpoint
         :segment-integrity :complete-hash-links-in-supplied-signed-segment-only
         :selected-row-erase-record
         (if (:selected-erase? result)
           :erase-commit-record-present-in-supplied-segment
           :no-erase-commit-record-in-supplied-segment)
         :audit-stream-start-anchor :caller-supplied-in-signed-checkpoint
         :audit-producer-authenticity :not-established
         :actual-xtdb-erase-correspondence :not-established
         :log-ingestion-completeness :not-established
         :missing-segment-exclusion :not-established
         :retention-enforcement :not-established
         :erasure-exclusion :not-established
         :history-completeness :not-established
         :discharged-conditions #{}
         :certificate :not-issued :transaction-authority :none
         :retry-authority :none :authority :none}))))
