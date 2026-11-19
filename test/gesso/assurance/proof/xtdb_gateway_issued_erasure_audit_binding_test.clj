(ns gesso.assurance.proof.xtdb-gateway-issued-erasure-audit-binding-test
  "v1882: Real XTDB: auditor-sealed segments derive expected bindings from
   the collector's genuine original executor and later pinned read, not caller
   supplied source/row/basis identity. No event producer is authenticated."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-issued-application-observation
             :as observation]
            [gesso.assurance.proof.xtdb-gateway-issued-application-observation-test]
            [gesso.assurance.proof.xtdb-gateway-issued-erasure-audit-segment
             :as segment]
            [gesso.assurance.proof.xtdb-gateway-issued-snapshot-identity
             :as snapshot]
            [gesso.model.command-resolution :as resolution]
            [xtdb.api :as xt]
            [xtdb.basis :as basis])
  (:import [java.security KeyFactory Signature]
           [java.security.spec PKCS8EncodedKeySpec]
           [java.time Instant]
           [java.util Base64 UUID]))

(def ^:private fixture-ns
  'gesso.assurance.proof.xtdb-gateway-issued-application-observation-test)
(defn- original [symbol]
  (var-get (or (ns-resolve fixture-ns symbol)
               (throw (ex-info "Missing real XTDB fixture" {:symbol symbol})))))
(defn- environment [] ((original 'v1867-alias-case-env)))
(defn- db [] ((original 'db)))
(defn- seed! [& values] (apply (original 'seed!) values))
(defn- read-command [id] ((original 'read-command) id))
(defn- reason [f] ((original 'reason) f))
(use-fixtures :once (original 'parent-fixture))

(defn- test-point []
  (let [rows (xt/q (db) "SHOW SNAPSHOT_TOKEN")]
    (snapshot/comparable-point
     (basis/<-time-basis-str (:snapshot-token (first rows))))))

(defn- sign-checkpoint [checkpoint]
  (let [encoded (System/getenv "GESSO_TEST_AUDITOR_PRIVATE_KEY_B64")
        private (.generatePrivate (KeyFactory/getInstance "Ed25519")
                                 (PKCS8EncodedKeySpec.
                                  (.decode (Base64/getDecoder) ^String encoded)))
        signer (Signature/getInstance "Ed25519")]
    (.initSign signer private)
    (.update signer (segment/canonical-checkpoint-bytes! checkpoint))
    (.encodeToString (Base64/getEncoder) (.sign signer))))

(defn- sealed-segment [expected]
  ;; The test deliberately manufactures this erase-commit EVENT without
  ;; ever erasing the row in XTDB: signing it is NOT correspondence proof.
  (let [stream "v1882-externally-asserted-audit-segment"
        prior (apply str (repeat 64 "0"))
        event {:sequence 1
               :stream-id stream
               :database-instance (:database-instance expected)
               :physical-table (:physical-table expected)
               :physical-row-id (:physical-row-id expected)
               :event-id (UUID/randomUUID)
               :event-kind :erase-commit-reported
               :recorded-at (:system-to expected)
               :previous-sha256 prior}
        digest (segment/event-sha256! event)
        checkpoint {:format segment/checkpoint-format
                    :binding expected
                    :stream-id stream
                    :start-sequence 0 :end-sequence 1
                    :start-sha256 prior :head-sha256 digest
                    :record-count 1
                    :signed-at (Instant/now)
                    :nonce (UUID/randomUUID)}]
    {:checkpoint checkpoint
     :records [{:event event :digest-sha256 digest}]
     :auditor-signature-base64 (sign-checkpoint checkpoint)}))

(deftest v1882-sealed-erasure-segment-is-bound-to-private-original-xtdb-witness
  (let [operation :request/claim
        {:keys [data] :as env} (environment)
        collector (observation/issue-current-issued-application-observer!
                   (:contract env) (:catalog-source env))
        inspect! (:inspect-erasure-audit-segment! collector)
        summary! (:summary! collector)
        original-request (:request data)
        ctx {:biff.xtdb/node (db)}
        invocation {:binding (:binding data) :mode :submit}]
    (is (fn? inspect!))
    (is (= {operation :not-eligible}
           (:per-operation-erasure-audit-intake (summary!))))
    (is (= {operation :not-eligible}
           (:per-operation-signed-erasure-claim-pinned-visibility (summary!))))
    (is (= :original-observation-required-for-erasure-audit
           (reason #(inspect! operation {}))))
    (is (= :operation-not-originally-certified
           (reason #(inspect! :request/audit {}))))
    (seed! [:requests original-request])
    (let [start (test-point)]
      (is (snapshot/valid-private-point? start))
      (let [observed ((:observe! collector) operation ctx invocation)]
        (is (= #{operation} (:observed-operation-ids observed)))
        (is (= {operation :not-checked}
               (:per-operation-erasure-audit-intake observed)))
        (is (= :completed-later-pinned-read-required-for-erasure-audit
               (reason #(inspect! operation {})))))
      (seed! [:requests {:xt/id (str "unrelated-audit-" (random-uuid))
                         :owner "new-system-basis"}])
      (let [end (test-point)
            reread ((:read-only-recheck! collector) operation)
            origin (get-in reread [:certified-operation-proof-origins operation])
            expected (merge
                      {:operation-id operation
                       :database-instance (:database start)
                       :physical-table :requests
                       :physical-row-id (:xt/id original-request)
                       :system-from (:system-time start)
                       :system-to (:system-time end)}
                      (select-keys origin
                                   [:proof-fingerprint :proof-source-fingerprint
                                    :proof-subject-fingerprint
                                    :dependency-graph-fingerprint
                                    :transition-fingerprint]))
            signed (sealed-segment expected)]
        (is (= :later-reported-database-basis
               (snapshot/reported-basis-relation start end)))
        (is (= :not-checked
               (get-in reread [:per-operation-erasure-audit-intake operation])))
        (let [accepted (inspect! operation signed)]
          (is (= :erase-commit-record-present-in-supplied-segment
                 (get-in accepted [:per-operation-erasure-audit-intake operation])))
          (is (= :signed-erase-claim-with-current-valid-row-at-pinned-read
                 (get-in accepted
                         [:per-operation-signed-erasure-claim-pinned-visibility
                          operation])))
          (is (= :diagnostic-point-in-time-comparison-not-erase-causality
                 (:signed-erasure-claim-pinned-visibility-meaning accepted)))
          (is (false? (:signed-erasure-claim-proves-physical-erase? accepted)))
          (is (= :private-original-and-later-pinned-binding-only
                 (:erasure-audit-intake-scope accepted)))
          (is (= :not-established (:erasure-audit-producer-completeness accepted)))
          (is (= :none (:erasure-audit-discharge accepted)
                 (:transaction-authority accepted) (:retry-authority accepted)
                 (:authority accepted)))
          (is (= :not-issued (:certificate accepted)))
          (is (= #{} (get-in accepted [:diagnostic-open-history-conditions
                                      :discharged-conditions])))
          (is (= :not-established
                 (get-in accepted [:diagnostic-required-independent-history-evidence
                                   :history-continuity])))
          (is (= original-request ((original 'read-request) (:xt/id original-request)))
              "A signed audit event is not evidence that an erase took place")
          (is (not-any? #(contains? accepted %)
                        [:expected-binding :checkpoint :records :private-original-before
                         :private-later-pinned-point :snapshot-token :database-instance
                         :physical-row-id :system-from :system-to :head-sha256])))
        ;; A correctly re-signed envelope for the WRONG physical row, proof
        ;; source, database or interval still fails against private evidence.
        (doseq [[field value] [[:physical-row-id "another-row"]
                               [:proof-fingerprint "another-proof"]
                               [:database-instance "other-database"]
                               [:system-from (.minusSeconds ^Instant (:system-from expected) 1)]
                               [:system-to (.plusSeconds ^Instant (:system-to expected) 1)]]]
          (is (= :audit-segment-source-binding-mismatch
                 (reason #(inspect! operation
                                    (sealed-segment (assoc expected field value)))))
              (str "Signed wrong binding: " field)))
        (is (= :failed
               (get-in (summary!) [:per-operation-erasure-audit-intake operation])))
        (is (= :erase-commit-record-present-in-supplied-segment
               (get-in (inspect! operation signed)
                       [:per-operation-erasure-audit-intake operation])))
        (with-redefs [segment/inspect-pinned-erase-audit-segment!
                      (fn [& _] {:type segment/report-type})]
          (is (= :substituted-original-erasure-audit-checker
                 (reason #(inspect! operation signed)))))
        ;; A new pinned read invalidates all earlier interval-bound evidence.
        (seed! [:requests {:xt/id (str "unrelated-audit-" (random-uuid))
                           :owner "even-later-basis"}])
        (let [next-read ((:read-only-recheck! collector) operation)]
          (is (= :not-checked
                 (get-in next-read [:per-operation-erasure-audit-intake operation])))
          (is (= :not-checked
                 (get-in next-read
                         [:per-operation-signed-erasure-claim-pinned-visibility
                          operation])))
          (is (= :audit-segment-source-binding-mismatch
                 (reason #(inspect! operation signed)))))
        (is (= 1 @(:constructs (:environment env))))
        (is (nil? (read-command (resolution/record-id (:binding data)))))))))


(defn- sealed-part [expected stream start-seq prior event-id kind at]
  (let [event {:sequence (inc start-seq)
               :stream-id stream
               :database-instance (:database-instance expected)
               :physical-table (:physical-table expected)
               :physical-row-id (:physical-row-id expected)
               :event-id event-id
               :event-kind kind
               :recorded-at at
               :previous-sha256 prior}
        digest (segment/event-sha256! event)
        checkpoint {:format segment/checkpoint-format
                    :binding expected :stream-id stream
                    :start-sequence start-seq :end-sequence (inc start-seq)
                    :start-sha256 prior :head-sha256 digest
                    :record-count 1 :signed-at (Instant/now)
                    :nonce (UUID/randomUUID)}]
    {:checkpoint checkpoint
     :records [{:event event :digest-sha256 digest}]
     :auditor-signature-base64 (sign-checkpoint checkpoint)}))

(defn- two-sealed-parts [expected]
  (let [stream "v1883-auditor-supplied-range"
        seed (apply str (repeat 64 "0"))
        first-part (sealed-part expected stream 0 seed (UUID/randomUUID)
                                :erase-attempt-reported (:system-from expected))
        second-part (sealed-part expected stream 1
                                 (get-in first-part [:checkpoint :head-sha256])
                                 (UUID/randomUUID)
                                 :erase-commit-reported (:system-to expected))]
    [first-part second-part]))

(deftest v1883-multiple-sealed-audit-parts-bind-to-private-original-witness
  ;; Signed log continuity is a strictly bounded *supplied-range* guarantee.
  ;; Even an intact 2-part audit that asserts an ERASE without a physical
  ;; operation must not grant history discharge or transaction authority.
  (let [op :request/claim
        {:keys [data] :as env} (environment)
        collector (observation/issue-current-issued-application-observer!
                   (:contract env) (:catalog-source env))
        inspect! (:inspect-erasure-audit-segment-range! collector)
        summary! (:summary! collector)
        original-request (:request data)
        ctx {:biff.xtdb/node (db)}
        invocation {:binding (:binding data) :mode :submit}]
    (is (fn? inspect!))
    (is (= :original-observation-required-for-erasure-audit
           (reason #(inspect! op []))))
    (is (= :operation-not-originally-certified
           (reason #(inspect! :request/audit []))))
    (seed! [:requests original-request])
    (let [start (test-point)]
      (is (= #{op} (:observed-operation-ids
                    ((:observe! collector) op ctx invocation))))
      (is (= :completed-later-pinned-read-required-for-erasure-audit
             (reason #(inspect! op []))))
      (seed! [:requests {:xt/id (str "unrelated-range-" (random-uuid))
                         :owner "new-basis"}])
      (let [end (test-point)
            reread ((:read-only-recheck! collector) op)
            origin (get-in reread [:certified-operation-proof-origins op])
            expected (merge
                      {:operation-id op
                       :database-instance (:database start)
                       :physical-table :requests
                       :physical-row-id (:xt/id original-request)
                       :system-from (:system-time start)
                       :system-to (:system-time end)}
                      (select-keys origin
                                   [:proof-fingerprint :proof-source-fingerprint
                                    :proof-subject-fingerprint
                                    :dependency-graph-fingerprint
                                    :transition-fingerprint]))
            [first-part second-part :as signed-range]
            (two-sealed-parts expected)
            standalone (segment/inspect-pinned-erase-audit-segment-range!
                        expected signed-range)
            summary (inspect! op signed-range)]
        (is (= :later-reported-database-basis
               (snapshot/reported-basis-relation start end)))
        (is (= segment/range-report-type (:type standalone)))
        (is (= :complete-hash-links-in-supplied-signed-range-only
               (:range-integrity standalone)))
        (is (= 2 (:verified-segment-count standalone)
               (:verified-record-count standalone)))
        (is (= :not-established
               (:actual-xtdb-erase-correspondence standalone)
               (:missing-segment-exclusion standalone)))
        (is (not-any? #(contains? standalone %)
                      [:checkpoint :records :event-id :physical-row-id
                       :head-sha256 :start-sha256 :system-from]))
        (is (= :erase-commit-record-present-in-supplied-segment
               (get-in summary [:per-operation-erasure-audit-intake op])))
        (is (= :signed-erase-claim-with-current-valid-row-at-pinned-read
               (get-in summary
                       [:per-operation-signed-erasure-claim-pinned-visibility op])))
        (is (false? (:signed-erasure-claim-proves-physical-erase? summary)))
        (is (= :private-original-and-later-pinned-binding-only
               (:erasure-audit-intake-scope summary)))
        (is (= :not-established (:erasure-audit-producer-completeness summary)
               (:historical-continuity (:diagnostic-open-history-conditions summary))))
        (is (= #{} (get-in summary
                           [:diagnostic-open-history-conditions
                            :discharged-conditions])))
        (is (= :not-issued (:certificate summary)))
        (is (= :none (:erasure-audit-discharge summary)
               (:transaction-authority summary)
               (:retry-authority summary) (:authority summary)))
        (is (= original-request
               ((original 'read-request) (:xt/id original-request)))
            "Two signed erasure reports cannot prove a physical erase")
        (is (not-any? #(contains? summary %)
                      [:checkpoint :records :private-original-before
                       :private-later-pinned-point :snapshot-token
                       :physical-row-id :head-sha256 :stream-id]))
        (is (= :invalid-audit-segment-range-request
               (reason #(inspect! op []))))
        (is (= :invalid-audit-segment-range-request
               (reason #(inspect! op (vec (repeat 9 first-part))))))
        ;; Both segments are separately signed and individually valid, but
        ;; are not a contiguous range if the stream, sequence or link differs.
        (let [stream (get-in first-part [:checkpoint :stream-id])
              head (get-in first-part [:checkpoint :head-sha256])
              second-id (get-in second-part [:records 0 :event :event-id])
              second-at (:system-to expected)
              broken [[first-part
                       (sealed-part expected "other-stream" 1 head second-id
                                    :erase-commit-reported second-at)]
                      [first-part
                       (sealed-part expected stream 9 head second-id
                                    :erase-commit-reported second-at)]
                      [first-part
                       (sealed-part expected stream 1
                                    (apply str (repeat 64 "0")) second-id
                                    :erase-commit-reported second-at)]]]
          (doseq [candidate broken]
            (is (= :audit-segment-range-checkpoint-discontinuity
                   (reason #(inspect! op candidate)))))
          (is (= :audit-segment-range-duplicate-event-id
                 (reason #(inspect!
                           op [first-part
                               (sealed-part expected stream 1 head
                                            (get-in first-part
                                                    [:records 0 :event :event-id])
                                            :erase-commit-reported second-at)]))))
          (let [late-first (sealed-part expected stream 0
                                        (apply str (repeat 64 "0"))
                                        (UUID/randomUUID)
                                        :erase-attempt-reported second-at)
                late-head (get-in late-first [:checkpoint :head-sha256])]
            (is (= :audit-segment-range-time-regression
                   (reason #(inspect!
                             op [late-first
                                 (sealed-part expected stream 1 late-head
                                              second-id :erase-commit-reported
                                              (:system-from expected))])))))
          (is (= :audit-segment-source-binding-mismatch
                 (reason #(inspect! op
                                    (two-sealed-parts
                                     (assoc expected :physical-row-id "wrong-row")))))))
        (with-redefs [segment/inspect-pinned-erase-audit-segment-range!
                      (fn [& _] {:type segment/range-report-type})]
          (is (= :substituted-original-erasure-audit-checker
                 (reason #(inspect! op signed-range)))))
        (seed! [:requests {:xt/id (str "later-range-" (random-uuid))
                           :owner "basis-advanced-again"}])
        (let [newer ((:read-only-recheck! collector) op)]
          (is (= :not-checked
                 (get-in newer [:per-operation-erasure-audit-intake op])))
          (is (= :not-checked
                 (get-in newer
                         [:per-operation-signed-erasure-claim-pinned-visibility op]))))
        (is (= :audit-segment-source-binding-mismatch
               (reason #(inspect! op signed-range))))
        (is (= 1 @(:constructs (:environment env))))
        (is (nil? (read-command (resolution/record-id (:binding data)))))))))

(deftest v1884-signed-erase-claim-with-absent-row-still-does-not-prove-erase
  ;; The REAL physical transition here is DELETE, not ERASE. The independently
  ;; signed event lies about an ERASE; an absent row after this transaction
  ;; therefore cannot be elevated into erase-correspondence evidence.
  (let [op :request/claim
        {:keys [data] :as env} (environment)
        collector (observation/issue-current-issued-application-observer!
                   (:contract env) (:catalog-source env))
        inspect! (:inspect-erasure-audit-segment! collector)
        row (:request data)
        id (:xt/id row)
        ctx {:biff.xtdb/node (db)}
        invocation {:binding (:binding data) :mode :submit}]
    (seed! [:requests row])
    (let [start (test-point)]
      ((:observe! collector) op ctx invocation)
      (is (= :not-checked
             (get-in ((:summary! collector))
                     [:per-operation-signed-erasure-claim-pinned-visibility op])))
      (xt/execute-tx (db) [[:delete-docs :requests id]])
      (let [end (test-point)
            reread ((:read-only-recheck! collector) op)
            origin (get-in reread [:certified-operation-proof-origins op])
            expected (merge
                      {:operation-id op
                       :database-instance (:database start)
                       :physical-table :requests
                       :physical-row-id id
                       :system-from (:system-time start)
                       :system-to (:system-time end)}
                      (select-keys origin
                                   [:proof-fingerprint :proof-source-fingerprint
                                    :proof-subject-fingerprint
                                    :dependency-graph-fingerprint
                                    :transition-fingerprint]))
            signed (sealed-segment expected)]
        (is (= :no-current-valid-row
               (get-in reread [:per-operation-selected-row-current-visibility op])))
        (is (nil? ((original 'read-request) id)))
        (let [summary (inspect! op signed)]
          (is (= :signed-erase-claim-with-no-current-valid-row-at-pinned-read
                 (get-in summary
                         [:per-operation-signed-erasure-claim-pinned-visibility op])))
          (is (false? (:signed-erasure-claim-proves-physical-erase? summary)))
          (is (= :not-established (:physical-application-correspondence summary)
                 (:erasure-audit-producer-completeness summary)))
          (is (= :none (:erasure-audit-discharge summary)
                 (:transaction-authority summary) (:authority summary)))
          (is (= :not-issued (:certificate summary))))))))
