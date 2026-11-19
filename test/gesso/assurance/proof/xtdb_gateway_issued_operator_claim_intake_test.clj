(ns gesso.assurance.proof.xtdb-gateway-issued-operator-claim-intake-test
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.xtdb-gateway-issued-operator-claim-intake
             :as intake]
            [gesso.assurance.proof.xtdb-gateway-issued-operator-witness-binding
             :as witness-binding])
  (:import (java.security KeyPairGenerator Signature)
           (java.time Instant)
           (java.util Base64 UUID)))

(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))

(defn- signing-key []
  (.generateKeyPair (KeyPairGenerator/getInstance "Ed25519")))

(defn- sign [private-key claim]
  (let [s (Signature/getInstance "Ed25519")]
    (.initSign s private-key)
    (.update s (intake/canonical-claim-bytes! claim))
    {:claim claim
     :signature-base64 (.encodeToString (Base64/getEncoder) (.sign s))}))

(defn- binding-data [now]
  {:operation-id :request/claim
   :proof-fingerprint "proof-origin-101"
   :proof-source-fingerprint "source-101"
   :proof-subject-fingerprint "subject-101"
   :dependency-graph-fingerprint "graph-101"
   :transition-fingerprint "transition-101"
   :database-instance "operator-declared-xtdb-node"
   :physical-table :requests
   :physical-row-id "row-a"
   :system-from (.minusSeconds ^Instant now 120)
   :system-to now})

(defn- signed-claim [binding now]
  (merge {:format intake/envelope-format
          :retention-policy-reference "operator-retention-policy-17"
          :retention-assertion :operator-asserts-retention-enforced
          :erasure-audit-reference "operator-audit-log-03"
          :erasure-audit-assertion :operator-asserts-no-erasure
          :signed-at now
          :expires-at (.plusSeconds ^Instant now 600)
          :nonce (UUID/randomUUID)}
         binding))

(deftest v1878-detached-operator-signature-is-not-retention-or-erasure-proof
  (let [now (Instant/now)
        keypair (signing-key)
        other-pair (signing-key)
        expected (binding-data now)
        claim (signed-claim expected now)
        signed (sign (.getPrivate keypair) claim)
        ;; Test-only key: signed integrity does not establish operator trust.
        inspect! (var-get (ns-resolve
                           'gesso.assurance.proof.xtdb-gateway-issued-operator-claim-intake
                           'inspect-under-key!))
        inspect-under! (fn [key scope envelope]
                         (inspect! key scope envelope :unanchored-test-key))
        public (.getPublic keypair)
        valid (inspect-under! public expected signed)]
    (testing "a valid signature proves neither enforcement nor audit completeness"
      (is (= intake/report-type (:type valid)))
      (is (= intake/report-version (:version valid)))
      (is (= :valid-ed25519-under-selected-key (:signature-integrity valid)))
      (is (= :unanchored-test-key (:trust-root-source valid)))
      (is (= :matches-source-relative-expected-fields (:scope-binding valid)))
      (is (= :signed-assertion-not-verified-enforcement
             (:retention-policy-claim valid)))
      (is (= :signed-assertion-not-verified-completeness
             (:erasure-audit-claim valid)))
      (is (every? #(= :not-established (get valid %))
                  [:retention-enforcement :erasure-exclusion
                   :history-completeness :claimed-target-authenticity
                   :claimed-snapshot-authenticity
                   :replay-exclusion
                   :original-kernel-proof-discharge]))
      (is (= :not-independently-established
             (:signing-key-deployment-provenance valid)))
      (is (= #{} (:discharged-conditions valid)))
      (is (= :not-issued (:certificate valid)))
      (is (= :none (:transaction-authority valid)
             (:retry-authority valid) (:authority valid)))
      (is (not-any? #(contains? valid %)
                    [:claim :signature-base64 :physical-row-id :proof-fingerprint
                     :database-instance :system-from :system-to :nonce
                     :retention-policy-reference :erasure-audit-reference])))
    (testing "tampering, wrong signer, wrong binding and fake discharge are rejected"
      (let [cases
            [[:invalid-operator-claim-signature
              #(.getPublic other-pair) expected signed]
             [:invalid-operator-claim-signature
              (fn [] public) expected
              (assoc signed :claim (assoc claim :retention-policy-reference "switched"))]
             [:invalid-operator-claim-signature
              (fn [] public) expected
              (assoc signed :claim (assoc claim :erasure-audit-reference "changed"))]
             [:operator-claim-source-binding-mismatch
              (fn [] public) (assoc expected :physical-row-id "other-row") signed]
             [:operator-claim-source-binding-mismatch
              (fn [] public) (assoc expected :proof-fingerprint "other-proof") signed]
             [:invalid-operator-claim-signature
              (fn [] public) expected
              (assoc signed :claim (assoc claim :nonce (UUID/randomUUID)))]
             [:malformed-operator-claim-envelope
              (fn [] public) expected (assoc signed :verified? true)]
             [:malformed-operator-history-claim
              (fn [] public) expected
              (assoc signed :claim (assoc claim :discharged-conditions #{:yes}))]
             [:malformed-operator-claim-signature
              (fn [] public) expected (assoc signed :signature-base64 "garbage")]
             [:invalid-source-relative-history-binding
              (fn [] public) (assoc expected :extra :fake-source) signed]
             [:expired-or-future-operator-history-claim
              (fn [] public) expected
              (sign (.getPrivate keypair)
                    (assoc claim :signed-at (.minusSeconds now 900)
                                 :expires-at (.minusSeconds now 1)))]
             [:expired-or-future-operator-history-claim
              (fn [] public) expected
              (sign (.getPrivate keypair)
                    (assoc claim :signed-at (.plusSeconds now 180)
                                 :expires-at (.plusSeconds now 240)))]]]
        (doseq [[why public-fn binding envelope] cases]
          (is (= why (reason #(inspect-under! (public-fn) binding envelope)))
              (str "expected fail-closed: " why)))))
    (testing "production entry cannot use a request-provided public key"
      (when (nil? (System/getenv intake/operator-key-env))
        (is (= :independent-operator-trust-root-not-configured
               (reason #(intake/inspect-operator-history-claim! expected signed))))))))

(deftest v1879-private-original-pinned-witness-binding-is-strictly-scoped
  ;; This PURE helper is not an evidence authenticator; only the collector's
  ;; completed original and later read callbacks are allowed to supply its
  ;; arguments at the real signed-claim intake boundary.
  (let [t0 (Instant/parse "2026-01-01T00:00:00Z")
        t1 (.plusSeconds t0 5)
        operation :request/claim
        origin {:proof-fingerprint "fingerprint-a"
                :proof-source-fingerprint "source-a"
                :proof-subject-fingerprint "subject-a"
                :dependency-graph-fingerprint "graph-a"
                :transition-fingerprint "transition-a"
                :route-ids #{:route/claim}}
        original {:operation-id operation
                  :target [:requests "request-a"]
                  :before {:xt/id "request-a" :owner "before"}
                  :original-snapshot-point {:database "reported-database" :system-time t0}
                  :source-identity {:proof-fingerprint "fingerprint-a"
                                    :transition-fingerprint "transition-a"
                                    :route-ids #{:route/claim}}}
        later {:target [:requests "request-a"]
               :point {:database "reported-database" :system-time t1}}
        derive! #(witness-binding/expected-private-operator-binding!
                  operation %1 %2 %3)
        expected (derive! origin original later)]
    (is (= (set intake/binding-fields) (set (keys expected))))
    (is (= "request-a" (:physical-row-id expected)))
    (is (= :requests (:physical-table expected)))
    (is (= "reported-database" (:database-instance expected)))
    (is (= t0 (:system-from expected)))
    (is (= t1 (:system-to expected)))
    (is (= :incomplete-private-original-and-later-pinned-witness
           (reason #(derive! origin (assoc original :target [:requests "other-row"])
                             later))))
    (is (= :incomplete-private-original-and-later-pinned-witness
           (reason #(derive! origin original
                             (assoc later :target [:requests "other-row"])))))
    (is (= :incomplete-private-original-and-later-pinned-witness
           (reason #(derive! (assoc origin :proof-fingerprint "other-proof")
                             original later))))
    (is (= :operator-claim-requires-advanced-same-database-pinned-interval
           (reason #(derive! origin original
                             (assoc-in later [:point :system-time] t0)))))
    (is (= :operator-claim-requires-advanced-same-database-pinned-interval
           (reason #(derive! origin original
                             (assoc-in later [:point :system-time]
                                       (.minusSeconds t0 1))))))
    (is (= :operator-claim-requires-advanced-same-database-pinned-interval
           (reason #(derive! origin original
                             (assoc-in later [:point :database] "other-database")))))
    (is (= :incomplete-private-original-and-later-pinned-witness
           (reason #(derive! origin (assoc original :extra :forged) later))))))
