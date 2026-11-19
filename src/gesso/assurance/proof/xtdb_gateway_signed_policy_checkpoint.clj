(ns gesso.assurance.proof.xtdb-gateway-signed-policy-checkpoint
  "v1901: Portable, signed, hash-linked process-policy CHECKPOINT CANDIDATES.

   Snapshot material is read from v1900's actual private, revision-serialized
   policy, including its complete retired-key and grant-ID sets. A separately
   configured Ed25519 key signs a deterministic checkpoint document; a
   versioned verifier checks its signature, pinned source snapshot, exact
   next revision, expected predecessor digest and previously checked sets.
   This is a bounded portability and rollback-detection *interface*.

   The signing authority, policy object and expected predecessor digest are
   configured by the CALLER, not independently provisioned. No durable store
   is provided; a fresh process can be fed a stale checkpoint and matching
   stale anchor. No cross-process revocation, authenticated root custody,
   transaction causality, physical erasure evidence, certificate or other
   authority follows from a valid signature or a matching local state."
  (:import [java.nio.charset StandardCharsets]
           [java.security KeyFactory MessageDigest PublicKey Signature]
           [java.security.spec X509EncodedKeySpec]
           [java.util Arrays Base64 UUID]))

(def checkpoint-type
  :gesso.assurance.proof.xtdb-gateway-signed-policy-checkpoint/checkpoint)
(def report-type
  :gesso.assurance.proof.xtdb-gateway-signed-policy-checkpoint/check)
(def ^:private domain "gesso/xtdb/local-policy-checkpoint/v1\n")
(def ^:private genesis "genesis")
(def ^:private hex-digest #"sha256:[0-9a-f]{64}")
(def ^:private fingerprint #"sha256:[0-9a-f]{64}")
(def ^:private snapshot-keys
  #{:type :version :policy-id :policy-revision :root-generation
    :delegation-generation :root-lifecycle :delegation-lifecycle
    :root-fingerprint :delegate-fingerprint :delegation-id
    :seen-root-fingerprints :retired-delegate-fingerprints
    :retired-delegation-ids})
(def ^:private snapshot-type
  :gesso.assurance.proof.xtdb-gateway-local-key-lifecycle/checkpoint-material)
(def ^:private witness-limit 128)

(defn- reject! [reason]
  (throw (ex-info "Invalid signed local policy checkpoint candidate."
                  {:xtdb-signed-policy-checkpoint/rejection true
                   :reason reason})))

(defn- uuid-text? [s]
  (and (string? s)
       (<= 1 (count s) 64)
       (try (= s (str (UUID/fromString s)))
            (catch IllegalArgumentException _ false))))

(defn- sorted-distinct-strings? [xs match?]
  (and (vector? xs) (<= (count xs) witness-limit)
       (every? match? xs)
       (= xs (vec (sort (distinct xs))))))

(defn- check-snapshot! [s]
  (when-not
   (and (map? s) (= snapshot-keys (set (keys s)))
        (= snapshot-type (:type s)) (= 1 (:version s))
        (uuid-text? (:policy-id s))
        (uuid-text? (:delegation-id s))
        (every? #(and (integer? %) (<= 1 % 1000000))
                ((juxt :policy-revision :root-generation
                       :delegation-generation) s))
        (<= (:root-generation s) (:delegation-generation s)
            (:policy-revision s))
        (contains? #{:active :revoked} (:root-lifecycle s))
        (contains? #{:active :revoked} (:delegation-lifecycle s))
        (or (= :active (:root-lifecycle s))
            (= :revoked (:delegation-lifecycle s)))
        (string? (:root-fingerprint s))
        (re-matches fingerprint (:root-fingerprint s))
        (string? (:delegate-fingerprint s))
        (re-matches fingerprint (:delegate-fingerprint s))
        (sorted-distinct-strings? (:seen-root-fingerprints s)
                                  #(and (string? %) (re-matches fingerprint %)))
        (sorted-distinct-strings? (:retired-delegate-fingerprints s)
                                  #(and (string? %) (re-matches fingerprint %)))
        (sorted-distinct-strings? (:retired-delegation-ids s) uuid-text?)
        (= (:root-generation s) (count (:seen-root-fingerprints s)))
        (some #{(:root-fingerprint s)} (:seen-root-fingerprints s))
        (not (and (= :active (:delegation-lifecycle s))
                  (or (some #{(:delegation-id s)} (:retired-delegation-ids s))
                      (some #{(:delegate-fingerprint s)}
                            (:retired-delegate-fingerprints s))))))
    (reject! :malformed-or-inconsistent-checkpoint-snapshot))
  s)

(defn- canonical-checkpoint! [checkpoint]
  (when-not (and (map? checkpoint)
                 (= #{:type :version :previous-digest :snapshot}
                    (set (keys checkpoint)))
                 (= checkpoint-type (:type checkpoint))
                 (= 1 (:version checkpoint))
                 (string? (:previous-digest checkpoint))
                 (or (= genesis (:previous-digest checkpoint))
                     (re-matches hex-digest (:previous-digest checkpoint))))
    (reject! :invalid-checkpoint-envelope))
  (check-snapshot! (:snapshot checkpoint))
  ;; Fixed field ordering and individually bounded, fully checked scalars.
  ;; No arbitrary EDN parsing, user-controlled print handlers or eval.
  (let [snapshot (:snapshot checkpoint)
        fields (sort (keys snapshot))
        canonical (into (sorted-map) (for [k fields] [k (get snapshot k)]))
        document (pr-str (sorted-map :type (:type checkpoint)
                                     :version (:version checkpoint)
                                     :previous-digest (:previous-digest checkpoint)
                                     :snapshot canonical))]
    (.getBytes (str domain document) StandardCharsets/UTF_8)))

(defn checkpoint-signing-bytes
  "Public deterministic bytes to be signed by an OUT-OF-PROCESS authority.
   The caller may also possess that key: signing does not confer trust."
  [checkpoint]
  (canonical-checkpoint! checkpoint))

(defn- sha256-hex [^bytes x]
  (str "sha256:"
       (apply str (map #(format "%02x" (bit-and 0xff %))
                       (.digest (MessageDigest/getInstance "SHA-256") x)))))

(defn- canonical-ed25519 [encoded]
  (when-not (and (string? encoded) (<= 1 (count encoded) 512))
    (reject! :invalid-checkpoint-authority-key))
  (let [der (try (.decode (Base64/getDecoder) ^String encoded)
                 (catch IllegalArgumentException _
                   (reject! :invalid-checkpoint-authority-key)))
        ^PublicKey key
        (try (.generatePublic (KeyFactory/getInstance "Ed25519")
                              (X509EncodedKeySpec. der))
             (catch java.security.GeneralSecurityException _
               (reject! :invalid-checkpoint-authority-key)))]
    (when-not (and (<= 32 (alength der) 128)
                   (= encoded (.encodeToString (Base64/getEncoder) der))
                   (= "EdDSA" (.getAlgorithm key))
                   (Arrays/equals ^bytes der ^bytes (.getEncoded key)))
      (reject! :invalid-checkpoint-authority-key))
    {:key key :fingerprint (sha256-hex der)}))

(defn- check-extension! [old-snapshot new-snapshot]
  (when old-snapshot
    (when-not (and (= (:policy-id old-snapshot) (:policy-id new-snapshot))
                   (<= (:root-generation old-snapshot)
                       (:root-generation new-snapshot)
                       (inc (:root-generation old-snapshot)))
                   (<= (:delegation-generation old-snapshot)
                       (:delegation-generation new-snapshot)
                       (inc (:delegation-generation old-snapshot)))
                   (every? (set (:seen-root-fingerprints new-snapshot))
                           (:seen-root-fingerprints old-snapshot))
                   (every? (set (:retired-delegate-fingerprints new-snapshot))
                           (:retired-delegate-fingerprints old-snapshot))
                   (every? (set (:retired-delegation-ids new-snapshot))
                           (:retired-delegation-ids old-snapshot))
                   (or (not= (:root-generation old-snapshot)
                             (:root-generation new-snapshot))
                       (= (:root-fingerprint old-snapshot)
                          (:root-fingerprint new-snapshot)))
                   (or (not= (:delegation-generation old-snapshot)
                             (:delegation-generation new-snapshot))
                       (= (:delegate-fingerprint old-snapshot)
                          (:delegate-fingerprint new-snapshot))))
      (reject! :nonmonotonic-retirement-or-generation))))

(defn prepare-checkpoint-candidate-verifier!
  "Pin one policy source and signing key, and one *caller-provided* chain
   anchor. Requires exactly:
     {:policy <v1900 local policy descriptor>
      :authority-ed25519-spki-base64 <canonical SPKI>
      :next-revision <positive integer>
      :expected-previous-digest <genesis or sha256:...>}

   :accept! [signed-envelope] checks {:checkpoint .. :signature-base64 ..}.
   It verifies the real local checkpoint material anew before and after the
   signature check, then atomically advances this verifier's expected hash
   and revision. It does not change the original policy, write to disk,
   authenticate the key's provisioning, or prove that the history prior to
   its supplied boot anchor was complete or irreversible."
  [config]
  (when-not (and (map? config)
                 (= #{:policy :authority-ed25519-spki-base64
                      :next-revision :expected-previous-digest}
                    (set (keys config)))
                 (and (integer? (:next-revision config))
                      (<= 1 (:next-revision config) 1000000))
                 (string? (:expected-previous-digest config))
                 (or (and (= 1 (:next-revision config))
                          (= genesis (:expected-previous-digest config)))
                     (and (> (:next-revision config) 1)
                          (re-matches hex-digest (:expected-previous-digest config))))
                 (map? (:policy config))
                 (fn? (get-in config [:policy :checkpoint-material!])))
    (reject! :invalid-checkpoint-verifier-configuration))
  (let [{:keys [^PublicKey key fingerprint]}
        (canonical-ed25519 (:authority-ed25519-spki-base64 config))
        material! (get-in config [:policy :checkpoint-material!])
        monitor (Object.)
        current (atom {:expected-previous-digest
                       (:expected-previous-digest config)
                       :next-revision (:next-revision config)
                       :last-snapshot nil})
        accept! (fn [signed]
                  (locking monitor
                    (when-not (and (map? signed)
                                   (= #{:checkpoint :signature-base64}
                                      (set (keys signed)))
                                   (string? (:signature-base64 signed))
                                   (<= 1 (count (:signature-base64 signed)) 128))
                      (reject! :invalid-signed-checkpoint))
                    (let [checkpoint (:checkpoint signed)
                          bytes (canonical-checkpoint! checkpoint)
                          snapshot (:snapshot checkpoint)
                          now @current]
                      (when-not (and (= (:expected-previous-digest now)
                                        (:previous-digest checkpoint))
                                     (= (:next-revision now)
                                        (:policy-revision snapshot)))
                        (reject! :checkpoint-rollback-gap-or-wrong-anchor))
                      (check-extension! (:last-snapshot now) snapshot)
                      (when-not (= snapshot (material!))
                        (reject! :checkpoint-does-not-match-current-local-policy))
                      (let [raw (try (.decode (Base64/getDecoder)
                                              ^String (:signature-base64 signed))
                                     (catch IllegalArgumentException _
                                       (reject! :invalid-checkpoint-signature)))
                            valid? (and (= 64 (alength raw))
                                        (= (:signature-base64 signed)
                                           (.encodeToString (Base64/getEncoder) raw))
                                        (try (let [v (Signature/getInstance "Ed25519")]
                                               (.initVerify v key)
                                               (.update v ^bytes bytes)
                                               (.verify v ^bytes raw))
                                             (catch java.security.GeneralSecurityException _ false)))]
                        (when-not valid?
                          (reject! :checkpoint-signature-not-verified)))
                      (when-not (= snapshot (material!))
                        (reject! :checkpoint-local-policy-changed-during-check))
                      (let [digest (sha256-hex bytes)]
                        (reset! current {:expected-previous-digest digest
                                         :next-revision (inc (:next-revision now))
                                         :last-snapshot snapshot})
                        {:type report-type
                         :version 1
                         :checked-policy-revision (:policy-revision snapshot)
                         :checkpoint-digest digest
                         :configured-authority-fingerprint fingerprint
                         :signature-status :valid-for-process-configured-key
                         :checkpoint-local-match :at-time-of-inspection
                         :chain-continuity :contiguous-from-caller-provided-anchor
                         :past-anchor-integrity :not-established
                         :signer-independent-provisioning :not-established
                         :durable-storage :not-established
                         :cross-process-revocation :not-established
                         :physical-transaction-causality :not-established
                         :witness-admission :not-admitted
                         :proof-discharge :none
                         :certificate :not-issued
                         :transaction-authority :none
                         :retry-authority :none
                         :authority :none}))))
        status! (fn []
                  (locking monitor
                    (let [{:keys [expected-previous-digest next-revision]} @current]
                      {:type :gesso.assurance.proof.xtdb-gateway-signed-policy-checkpoint/verifier-status
                       :next-revision next-revision
                       :expected-previous-digest expected-previous-digest
                       :configured-authority-fingerprint fingerprint
                       :anchor-provenance :caller-supplied-not-independently-verified
                       :durable-storage :not-established
                       :authority :none})))]
    {:type :gesso.assurance.proof.xtdb-gateway-signed-policy-checkpoint/verifier
     :version 1
     :status! status!
     :accept! accept!
     :key-provisioning :process-configured-not-authenticated
     :anchor-provenance :caller-supplied-not-independently-verified
     :durable-storage :not-established
     :witness-admission :not-admitted
     :proof-discharge :none
     :certificate :not-issued
     :authority :none}))
