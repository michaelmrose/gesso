(ns gesso.assurance.proof.xtdb-gateway-issued-erasure-audit-segment-test
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.xtdb-gateway-issued-erasure-audit-segment :as segment]
            [gesso.assurance.proof.xtdb-gateway-issued-history-discharge-boundary :as boundary])
  (:import [java.security KeyFactory Signature]
           [java.security.spec PKCS8EncodedKeySpec]
           [java.time Instant]
           [java.util Base64 UUID]))

(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))

(defn- test-key [kind]
  (when-let [encoded (System/getenv
                      (case kind
                        :auditor "GESSO_TEST_AUDITOR_PRIVATE_KEY_B64"
                        :operator "GESSO_TEST_OPERATOR_PRIVATE_KEY_B64"))]
    (.generatePrivate (KeyFactory/getInstance "Ed25519")
                      (PKCS8EncodedKeySpec.
                       (.decode (Base64/getDecoder) ^String encoded)))))

(defn- sign-checkpoint [checkpoint role]
  (let [s (Signature/getInstance "Ed25519")]
    (.initSign s (test-key role))
    (.update s (segment/canonical-checkpoint-bytes! checkpoint))
    (.encodeToString (Base64/getEncoder) (.sign s))))

(defn- binding-data [start end]
  {:operation-id :request/claim
   :proof-fingerprint "checked-proof-9"
   :proof-source-fingerprint "checked-source-9"
   :proof-subject-fingerprint "checked-subject-9"
   :dependency-graph-fingerprint "checked-graph-9"
   :transition-fingerprint "checked-transition-9"
   :database-instance "reported-xtdb-instance"
   :physical-table :requests
   :physical-row-id "selected-request-9"
   :system-from start :system-to end})

(defn- mk-event [previous sequence binding kind row-id at]
  (let [event {:sequence sequence
               :stream-id "auditor-erasure-segment-9"
               :database-instance (:database-instance binding)
               :physical-table (:physical-table binding)
               :physical-row-id row-id
               :event-id (UUID/randomUUID)
               :event-kind kind
               :recorded-at at
               :previous-sha256 previous}]
    {:event event :digest-sha256 (segment/event-sha256! event)}))

(defn- sample [kinds]
  (let [now (Instant/now)
        start (.minusSeconds now 300)
        end (.minusSeconds now 5)
        expected (binding-data start end)
        seed (apply str (repeat 64 "0"))
        records (loop [i 0, previous seed, output []]
                  (if-let [[kind row-id] (nth kinds i nil)]
                    (let [record (mk-event previous (inc i) expected kind row-id
                                           (.plusSeconds start (* 30 (inc i))))]
                      (recur (inc i) (:digest-sha256 record) (conj output record)))
                    output))
        checkpoint {:format segment/checkpoint-format
                    :binding expected
                    :stream-id "auditor-erasure-segment-9"
                    :start-sequence 0
                    :end-sequence (count records)
                    :start-sha256 seed
                    :head-sha256 (if (seq records)
                                   (:digest-sha256 (last records)) seed)
                    :record-count (count records)
                    :signed-at now
                    :nonce (UUID/randomUUID)}]
    {:expected expected
     :checkpoint checkpoint
     :records records}))

(defn- envelope [sample]
  (let [{:keys [checkpoint records]} sample]
    {:checkpoint checkpoint
     :records records
     :auditor-signature-base64 (sign-checkpoint checkpoint :auditor)}))

(deftest v1881-auditor-sealed-erase-records-are-not-xtdb-enforcement-proof
  (let [keyed? (and (test-key :operator) (test-key :auditor))
        supplied (sample [[:erase-attempt-reported "selected-request-9"]
                          [:erase-commit-reported "selected-request-9"]])
        expected (:expected supplied)]
    (if keyed?
      (let [valid (envelope supplied)
            inspect! #(segment/inspect-pinned-erase-audit-segment! %1 %2)
            result (inspect! expected valid)]
        (testing "positive signed-log presence, not proven DB erase or full log capture"
          (is (= segment/report-type (:type result)))
          (is (= segment/report-version (:version result)))
          (is (= :valid-distinct-auditor-ed25519-checkpoint
                 (:signature-integrity result)))
          (is (= :complete-hash-links-in-supplied-signed-segment-only
                 (:segment-integrity result)))
          (is (= :erase-commit-record-present-in-supplied-segment
                 (:selected-row-erase-record result)))
          (is (= :caller-supplied-in-signed-checkpoint
                 (:audit-stream-start-anchor result)))
          (is (= :not-established
                 (:audit-producer-authenticity result)
                 (:actual-xtdb-erase-correspondence result)
                 (:log-ingestion-completeness result)
                 (:missing-segment-exclusion result)
                 (:retention-enforcement result)
                 (:erasure-exclusion result)
                 (:history-completeness result)))
          (is (= #{} (:discharged-conditions result)))
          (is (= :not-issued (:certificate result)))
          (is (= :none (:authority result)
                 (:transaction-authority result)
                 (:retry-authority result)))
          (is (not-any? #(contains? result %)
                        [:records :checkpoint :event-id :physical-row-id
                         :stream-id :head-sha256 :signature-base64
                         :auditor-signature-base64 :database-instance
                         :system-from :system-to]))
          (is (contains? boundary/insufficient-observations
                         :sealed-auditor-signed-erasure-log-segment-without-proven-log-completeness)))
        (testing "wrong expected source or physical identity must fail closed"
          (is (= :audit-segment-source-binding-mismatch
                 (reason #(inspect! (assoc expected :physical-row-id "wrong") valid))))
          (is (= :audit-segment-source-binding-mismatch
                 (reason #(inspect! (assoc expected :proof-fingerprint "wrong") valid)))))
        (testing "signature, record count, hash order, and gap checks"
          (is (= :invalid-auditor-checkpoint-signature
                 (reason #(inspect! expected
                                    (assoc valid :auditor-signature-base64
                                           (sign-checkpoint (:checkpoint supplied) :operator)))))
              "operator key cannot sign an auditor checkpoint")
          (is (= :invalid-auditor-checkpoint-signature
                 (reason #(inspect! expected
                                    (assoc-in valid [:checkpoint :head-sha256]
                                              (apply str (repeat 64 "f"))))))
              "head cannot be changed after auditor signing")
          (is (= :audit-segment-record-count-mismatch
                 (reason #(inspect! expected
                                    (assoc valid :records (pop (:records valid)))))))
          (is (= :audit-segment-gap-target-or-time-mismatch
                 (reason #(inspect! expected
                                    (assoc valid :records (vec (reverse (:records valid))))))))
          (is (= :audit-segment-entry-digest-mismatch
                 (reason #(inspect! expected
                                    (assoc-in valid [:records 1 :event :event-kind]
                                              :erase-denial-reported)))))
          (is (= :audit-segment-gap-target-or-time-mismatch
                 (reason #(inspect! expected
                                    (assoc-in valid [:records 1 :event :recorded-at]
                                              (Instant/parse "2030-01-01T00:00:00Z"))))))
          (is (= :invalid-audit-segment-request
                 (reason #(inspect! expected (assoc valid :authority :allowed)))))
          (is (= :invalid-supplied-audit-checkpoint
                 (reason #(inspect! expected
                                    (assoc-in valid [:checkpoint :discharged-conditions]
                                              #{:all})))))
          (is (= :invalid-supplied-audit-record
                 (reason #(inspect! expected
                                    (assoc-in valid [:records 1 :event :authorized] true)))))
          (let [first-record (first (:records valid))
                second-record (-> (second (:records valid))
                                  (assoc-in [:event :event-id]
                                            (get-in first-record [:event :event-id])))
                second-record (assoc second-record :digest-sha256
                                     (segment/event-sha256! (:event second-record)))
                amended (assoc (:checkpoint valid)
                               :head-sha256 (:digest-sha256 second-record))
                duplicate-envelope {:checkpoint amended
                                    :records [first-record second-record]
                                    :auditor-signature-base64
                                    (sign-checkpoint amended :auditor)}]
            (is (= :audit-segment-duplicate-event-id
                   (reason #(inspect! expected duplicate-envelope)))
                "Even a re-signed chain may not repeat an event ID")))
        (testing "a signed omission can only mean no record in the supplied segment"
          (let [other (sample [[:erase-commit-reported "unrelated-request"]])
                report (inspect! (:expected other) (envelope other))]
            (is (= :no-erase-commit-record-in-supplied-segment
                   (:selected-row-erase-record report)))
            (is (= :not-established (:missing-segment-exclusion report)
                   (:actual-xtdb-erase-correspondence report))))
          (let [empty-sample (sample [])
                report (inspect! (:expected empty-sample) (envelope empty-sample))]
            (is (= :no-erase-commit-record-in-supplied-segment
                   (:selected-row-erase-record report)))
            (is (= :not-established (:log-ingestion-completeness report)))))
        (testing "signatures never attest supplied events were genuinely emitted by XTDB"
          ;; A test author who has the auditor private key can create a valid
          ;; signed erase record WITHOUT executing any XTDB operation. The
          ;; independent producer/ingestion proof remains explicitly missing.
          (is (= :not-established (:actual-xtdb-erase-correspondence result)))))
      (is (= (if (System/getenv "GESSO_XTDB_HISTORY_OPERATOR_ED25519_PUBLIC_KEY_B64")
               :auditor-key-not-configured
               :operator-key-not-configured)
             (reason #(segment/inspect-pinned-erase-audit-segment!
                       expected
                       {:checkpoint (:checkpoint supplied)
                        :records (:records supplied)
                        :auditor-signature-base64 (apply str (repeat 88 "X"))})))))))
