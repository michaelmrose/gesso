(ns gesso.assurance.proof.xtdb-gateway-issued-operator-audit-cosign-test
  "v1880: Separate Ed25519 issuer and auditor keys/cosignature; cryptographic
   role separation is NOT evidence that retention/erasure controls worked."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.xtdb-gateway-issued-operator-claim-intake
             :as intake]
            [gesso.assurance.proof.xtdb-gateway-issued-operator-audit-cosign
             :as cosign]
            [gesso.assurance.proof.xtdb-gateway-issued-history-discharge-boundary
             :as boundary])
  (:import (java.security KeyFactory Signature)
           (java.security.spec PKCS8EncodedKeySpec)
           (java.time Instant)
           (java.util Base64 UUID)))

(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))

(defn- private-key [role]
  (let [v (System/getenv (case role
                           :operator "GESSO_TEST_OPERATOR_PRIVATE_KEY_B64"
                           :auditor "GESSO_TEST_AUDITOR_PRIVATE_KEY_B64"))]
    (when v
      (.generatePrivate (KeyFactory/getInstance "Ed25519")
                        (PKCS8EncodedKeySpec.
                         (.decode (Base64/getDecoder) ^String v))))))

(defn- sign [role bytes]
  (let [signature (Signature/getInstance "Ed25519")]
    (.initSign signature (private-key role))
    (.update signature bytes)
    (.encodeToString (Base64/getEncoder) (.sign signature))))

(defn- envelope [claim]
  (let [primary (sign :operator (intake/canonical-claim-bytes! claim))]
    {:claim claim
     :signature-base64 primary
     :auditor-signature-base64
     (sign :auditor (cosign/canonical-auditor-cosign-bytes! claim primary))}))

(defn- fixture-claim []
  (let [now (Instant/now)
        expected {:operation-id :request/claim
                  :proof-fingerprint "proof-abc"
                  :proof-source-fingerprint "source-abc"
                  :proof-subject-fingerprint "subject-abc"
                  :dependency-graph-fingerprint "dependencies-abc"
                  :transition-fingerprint "transition-abc"
                  :database-instance "source-reported-xtdb"
                  :physical-table :requests
                  :physical-row-id "request-abc"
                  :system-from (.minusSeconds now 125)
                  :system-to (.minusSeconds now 20)}]
    {:expected expected
     :claim (merge expected
                   {:format intake/envelope-format
                    :retention-policy-reference "operator-policy-id"
                    :retention-assertion :operator-asserts-retention-enforced
                    :erasure-audit-reference "auditor-audit-ref"
                    :erasure-audit-assertion :operator-asserts-no-erasure
                    :signed-at now :expires-at (.plusSeconds now 600)
                    :nonce (UUID/randomUUID)})}))

(deftest v1880-distinct-signers-must-cosign-exact-operator-signature
  (let [{:keys [expected claim]} (fixture-claim)
        inspect! cosign/inspect-independent-auditor-cosign!
        op-key? (some? (private-key :operator))
        audit-key? (some? (private-key :auditor))]
    (if (and op-key? audit-key?)
      (let [good (envelope claim)
            verified (inspect! expected good)]
        (testing "two separate pinned key roles establish signature integrity only"
          (is (= intake/report-type (:type verified)))
          (is (= :valid-ed25519-under-selected-key
                 (:signature-integrity verified)))
          (is (= :distinct-pinned-ed25519-public-keys
                 (:operator-and-auditor-key-separation verified)))
          (is (= :valid-domain-separated-ed25519-cosignature
                 (:auditor-cosignature verified)))
          (is (= :exact-primary-signature-and-claim
                 (:audit-cosignature-binds verified)))
          (is (= :not-established
                 (:independent-key-custody verified)
                 (:independent-evidence-production verified)
                 (:retention-enforcement verified)
                 (:erasure-exclusion verified)
                 (:history-completeness verified)))
          (is (contains? boundary/insufficient-observations
                         :dual-operator-and-auditor-signatures-without-verified-control-enforcement))
          (is (= :not-issued (:certificate verified)))
          (is (= #{} (:discharged-conditions verified)))
          (is (= :none (:transaction-authority verified)
                 (:retry-authority verified) (:authority verified)))
          (is (not-any? #(contains? verified %)
                        [:claim :signature-base64 :auditor-signature-base64
                         :proof-fingerprint :physical-row-id :nonce
                         :database-instance :system-from :system-to])))
        (testing "signature roles cannot be swapped or their payloads substituted"
          (is (= :malformed-independent-auditor-envelope
                 (reason #(inspect! expected (dissoc good :auditor-signature-base64)))))
          (is (= :malformed-independent-auditor-envelope
                 (reason #(inspect! expected (assoc good :proof-discharged true)))))
          (is (= :invalid-operator-claim-signature
                 (reason #(inspect! expected (assoc good
                                                   :signature-base64
                                                   (:auditor-signature-base64 good))))))
          (is (= :invalid-independent-auditor-signature
                 (reason #(inspect! expected (assoc good
                                                   :auditor-signature-base64
                                                   (:signature-base64 good))))))
          (is (= :malformed-independent-auditor-signature
                 (reason #(inspect! expected (assoc good
                                                   :auditor-signature-base64 "garbage")))))
          (is (= :operator-claim-source-binding-mismatch
                 (reason #(inspect! (assoc expected :physical-row-id "other-row") good))))
          (is (= :invalid-independent-auditor-signature
                 (reason #(let [altered-claim (assoc claim :nonce (UUID/randomUUID))
                                altered-primary (sign :operator
                                                      (intake/canonical-claim-bytes!
                                                       altered-claim))]
                            (inspect! expected
                                      (assoc good
                                             :claim altered-claim
                                             :signature-base64 altered-primary))))))
              "Auditor signs the exact operator signature, not just any valid primary claim")
          (is (= :invalid-independent-auditor-signature
                 (reason #(inspect! expected
                                    (assoc good :auditor-signature-base64
                                           (sign :operator
                                                 (cosign/canonical-auditor-cosign-bytes!
                                                  claim (:signature-base64 good)))))))
              "Operator's original private key is not the separately pinned auditor")
        (testing "substituting the original verifier must fail closed"
          (with-redefs [intake/inspect-operator-history-claim!
                        (fn [& _] {:operator-and-auditor-key-separation :forged})]
            (is (= :substituted-original-operator-claim-checker
                   (reason #(inspect! expected good)))))))
      (is (= :independent-operator-trust-root-not-configured
             (reason #(inspect! expected
                                {:claim claim
                                 :signature-base64 "invalid"
                                 :auditor-signature-base64 "invalid"})))))))
