(ns gesso.assurance.proof.xtdb-gateway-issued-operator-witness-binding-test
  "v1879: Real-XTDB original and later pinned witness binding to an operator-
   pinned Ed25519 statement; independently rejects wrong signed identities."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-issued-application-observation
             :as application-observation]
            [gesso.assurance.proof.xtdb-gateway-issued-application-observation-test]
            [gesso.assurance.proof.xtdb-gateway-issued-operator-claim-intake
             :as operator-claim]
            [gesso.assurance.proof.xtdb-gateway-issued-snapshot-identity :as snapshot]
            [gesso.model.command-resolution :as resolution]
            [xtdb.api :as xt]
            [xtdb.basis :as basis])
  (:import (java.security KeyFactory Signature)
           (java.security.spec PKCS8EncodedKeySpec)
           (java.time Instant)
           (java.util Base64 UUID)))

(def ^:private fixture-ns
  'gesso.assurance.proof.xtdb-gateway-issued-application-observation-test)
(defn- original [symbol]
  (var-get (or (ns-resolve fixture-ns symbol)
               (throw (ex-info "Missing original XTDB fixture" {:symbol symbol})))))
(defn- v1867-alias-case-env [] ((original 'v1867-alias-case-env)))
(defn- db [] ((original 'db)))
(defn- seed! [& xs] (apply (original 'seed!) xs))
(defn- read-command [id] ((original 'read-command) id))
(defn- reason [f] ((original 'reason) f))
(use-fixtures :once (original 'parent-fixture))

(defn- pinned-test-point []
  (let [tokens (xt/q (db) "SHOW SNAPSHOT_TOKEN")]
    (snapshot/comparable-point
     (basis/<-time-basis-str (:snapshot-token (first tokens))))))

(defn- test-signed-envelope [claim]
  (let [encoded (System/getenv "GESSO_TEST_OPERATOR_PRIVATE_KEY_B64")
        private (.generatePrivate
                 (KeyFactory/getInstance "Ed25519")
                 (PKCS8EncodedKeySpec.
                  (.decode (Base64/getDecoder) ^String encoded)))
        signer (Signature/getInstance "Ed25519")]
    (.initSign signer private)
    (.update signer (operator-claim/canonical-claim-bytes! claim))
    {:claim claim
     :signature-base64 (.encodeToString (Base64/getEncoder) (.sign signer))}))

(deftest v1879-signed-operator-claim-binds-private-original-and-later-pinned-reads
  ;; The collector does NOT accept an expected source binding. The test
  ;; reconstructs an externally signed statement from the database's reported
  ;; points and PUBLIC source proof fingerprints, never from the collector's
  ;; private ledger. The independently running collector must match it to its
  ;; own original checked prestate, exact target and later pinned read.
  (let [op :request/claim
        {:keys [data environment] :as env} (v1867-alias-case-env)
        collector (application-observation/issue-current-issued-application-observer!
                   (:contract env) (:catalog-source env))
        summarize (:summary! collector)
        inspect! (:inspect-operator-claim! collector)
        initial (summarize)
        ctx {:biff.xtdb/node (db)}
        invocation {:binding (:binding data) :mode :submit}
        original (:request data)]
    (is (= {op :not-eligible} (:per-operation-operator-claim-intake initial)))
    (is (= :original-observation-required-for-operator-claim
           (reason #(inspect! op {}))))
    (is (= :operation-not-originally-certified
           (reason #(inspect! :request/audit {}))))
    (seed! [:requests original])
    (let [start (pinned-test-point)
          observed ((:observe! collector) op ctx invocation)]
      (is (snapshot/valid-private-point? start))
      (is (= {op :not-checked} (:per-operation-operator-claim-intake observed)))
      (is (= :completed-later-pinned-read-required-for-operator-claim
             (reason #(inspect! op {}))))
      ;; An unrelated write advances the database basis but does not modify
      ;; the selected physical row, construct a domain command or grant proof.
      (seed! [:requests {:xt/id (str "unrelated-" (random-uuid))
                         :owner "operator-bound-snapshot"}])
      (let [end (pinned-test-point)
            later ((:read-only-recheck! collector) op)
            origin (get-in later [:certified-operation-proof-origins op])
            expected (merge
                      {:operation-id op
                       :database-instance (:database start)
                       :physical-table :requests
                       :physical-row-id (:xt/id original)
                       :system-from (:system-time start)
                       :system-to (:system-time end)}
                      (select-keys origin
                                   [:proof-fingerprint :proof-source-fingerprint
                                    :proof-subject-fingerprint
                                    :dependency-graph-fingerprint
                                    :transition-fingerprint]))
            now (Instant/now)
            claim (merge expected
                         {:format operator-claim/envelope-format
                          :retention-policy-reference "test-operator-retention-policy"
                          :retention-assertion :operator-asserts-retention-enforced
                          :erasure-audit-reference "test-operator-erasure-audit"
                          :erasure-audit-assertion :operator-asserts-no-erasure
                          :signed-at now :expires-at (.plusSeconds now 600)
                          :nonce (UUID/randomUUID)})
            private-configured? (and (System/getenv "GESSO_TEST_OPERATOR_PRIVATE_KEY_B64")
                                     (System/getenv operator-claim/operator-key-env))]
        (is (= :later-reported-database-basis
               (snapshot/reported-basis-relation start end)))
        (is (= :not-checked
               (get-in later [:per-operation-operator-claim-intake op])))
        (is (= :source-document-matched-at-read
               (get-in later [:per-operation-read-only-rechecks op])))
        (is (= :none (:operator-claim-discharge later)))
        (if private-configured?
          (let [signed (test-signed-envelope claim)
                accepted (inspect! op signed)]
            (is (= :valid-operator-signature-only
                   (get-in accepted [:per-operation-operator-claim-intake op])))
            (is (= :none (:operator-claim-discharge accepted)
                   (:transaction-authority accepted) (:retry-authority accepted)
                   (:authority accepted)))
            (is (= :not-issued (:certificate accepted)))
            (is (= #{} (get-in accepted
                               [:diagnostic-open-history-conditions
                                :discharged-conditions])))
            (is (= :not-established
                   (get-in accepted
                           [:diagnostic-required-independent-history-evidence
                            :history-continuity])))
            (is (not-any? #(contains? accepted %)
                          [:expected-binding :private-original-before
                           :private-later-pinned-point :target :snapshot-token
                           :signature-base64 :claim :nonce :system-from
                           :system-to :database-instance :physical-row-id]))
            ;; The same signer also signs plausible WRONG statements. Because
            ;; binding comes from the private ORIGINAL observations, changing
            ;; row, proof, or original/later basis must not be accepted.
            (doseq [[field value] [[:physical-row-id "other-row"]
                                   [:proof-fingerprint "other-proof"]
                                   [:database-instance "another-database"]
                                   [:system-from (.minusSeconds
                                                  ^Instant (:system-from expected) 1)]
                                   [:system-to (.plusSeconds
                                                ^Instant (:system-to expected) 1)]]]
              (is (= :operator-claim-source-binding-mismatch
                     (reason #(inspect! op
                                        (test-signed-envelope
                                         (assoc claim field value)))))
                  (str "Reject signed but wrong private field " field)))
            (is (= :failed
                   (get-in (summarize)
                           [:per-operation-operator-claim-intake op])))
            (is (= :valid-operator-signature-only
                   (get-in (inspect! op signed)
                           [:per-operation-operator-claim-intake op])))
            ;; A NEW read with a NEW physical basis invalidates the earlier
            ;; signed interval even though the original observation stays.
            (seed! [:requests {:xt/id (str "unrelated-" (random-uuid))
                               :owner "another-later-basis"}])
            (let [updated ((:read-only-recheck! collector) op)]
              (is (= :not-checked
                     (get-in updated [:per-operation-operator-claim-intake op])))
              (is (= :operator-claim-source-binding-mismatch
                     (reason #(inspect! op signed))))))
          (is (= :independent-operator-trust-root-not-configured
                 (reason #(inspect! op {:not-a-claim true})))
              "Without operator-pinned trust root, no signed claim can pass"))
        (is (= 1 @(:constructs environment)))
        (is (nil? (read-command (resolution/record-id (:binding data)))))
        (is (= #{op} (:observed-operation-ids (summarize))))))))

