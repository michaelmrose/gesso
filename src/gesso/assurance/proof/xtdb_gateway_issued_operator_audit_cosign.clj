(ns gesso.assurance.proof.xtdb-gateway-issued-operator-audit-cosign
  "v1880: A separately pinned AUDITOR Ed25519 key must countersign the exact
   operator's claim AND its primary signature under an audit-only domain.
   The two pinned public keys must differ. This is cryptographic separation
   of signing roles, NOT verified independent operators, evidence producers,
   retention-policy enforcement, complete erase auditing or history proof.

   The collector, never the caller, supplies the expected physical/proof
   interval. Public reports omit signatures, keys, rows and timestamps."
  (:require [gesso.assurance.proof.xtdb-gateway-issued-operator-claim-intake
             :as intake])
  (:import (java.io ByteArrayOutputStream DataOutputStream)
           (java.security KeyFactory PublicKey Signature)
           (java.security.spec X509EncodedKeySpec)
           (java.util Arrays Base64)))

(def auditor-key-env "GESSO_XTDB_HISTORY_AUDITOR_ED25519_PUBLIC_KEY_B64")
(def cosignature-domain "gesso/xtdb/issued-history/independent-auditor-cosign/v1")
(def ^:private original-claim-encoding intake/canonical-claim-bytes!)
(def ^:private original-primary-intake intake/inspect-operator-history-claim!)

(defn- reject! [reason]
  (throw (ex-info "Distinctly pinned auditor countersignature not established."
                  {:reason reason})))

(defn- pinned! []
  (when-not (and (identical? original-claim-encoding
                             (var-get #'intake/canonical-claim-bytes!))
                 (identical? original-primary-intake
                             (var-get #'intake/inspect-operator-history-claim!)))
    (reject! :substituted-original-operator-claim-checker)))

(defn- decode-signature! [encoded reason]
  (when-not (and (string? encoded) (<= 1 (count encoded) 256))
    (reject! reason))
  (let [bs (try (.decode (Base64/getDecoder) ^String encoded)
                (catch IllegalArgumentException _ (reject! reason)))]
    (when-not (= 64 (alength bs))
      (reject! reason))
    bs))

(defn canonical-auditor-cosign-bytes!
  "Public, versioned binary format for an independent AUDITOR to sign.
   Binds the exact signed claim plus the ORIGINAL operator signature.
   The retained primary signature must be independently verified as well."
  [claim primary-signature-base64]
  (pinned!)
  (let [claim-bytes (original-claim-encoding claim)
        primary-bytes (decode-signature!
                       primary-signature-base64
                       :malformed-primary-signature-for-cosign)
        output (ByteArrayOutputStream.)
        stream (DataOutputStream. output)]
    (.writeUTF stream cosignature-domain)
    (.writeInt stream (alength claim-bytes))
    (.write stream claim-bytes)
    (.writeInt stream (alength primary-bytes))
    (.write stream primary-bytes)
    (.flush stream)
    (.toByteArray output)))

(defn- pinned-public-key! [env-name unconfigured-reason invalid-reason]
  (let [encoded (System/getenv env-name)]
    (when-not (and (string? encoded) (<= 1 (count encoded) 1024))
      (reject! unconfigured-reason))
    (try
      (let [der (.decode (Base64/getDecoder) ^String encoded)
            key (.generatePublic (KeyFactory/getInstance "Ed25519")
                                 (X509EncodedKeySpec. der))]
        (when-not (= "EdDSA" (.getAlgorithm key))
          (reject! invalid-reason))
        key)
      (catch Exception _ (reject! invalid-reason)))))

(defn inspect-independent-auditor-cosign!
  "Opt-in dual-key integrity check. Trust roots are loaded ONLY from two
   distinct process-environment variables, never the caller's envelope.
   The operator verifies the claim; the auditor verifies a DOMAIN-SEPARATED
   signature over those exact claim bytes AND the primary signature bytes.

   Both valid signatures prove neither independent control of the two keys
   nor the claimed policy, audit or history. No proof is discharged."
  [expected envelope]
  (pinned!)
  (when-not (and (map? envelope) (nil? (meta envelope))
                 (= #{:claim :signature-base64 :auditor-signature-base64}
                    (set (keys envelope))))
    (reject! :malformed-independent-auditor-envelope))
  ;; Check configured roots BEFORE accepting any signature. Equal keys are
  ;; forbidden even when both signatures are mathematically valid.
  (let [retention-root (pinned-public-key!
                        intake/operator-key-env
                        :independent-operator-trust-root-not-configured
                        :invalid-independent-operator-trust-root)
        audit-root (pinned-public-key!
                    auditor-key-env
                    :independent-auditor-trust-root-not-configured
                    :invalid-independent-auditor-trust-root)]
    (when (Arrays/equals (.getEncoded ^PublicKey retention-root)
                         (.getEncoded ^PublicKey audit-root))
      (reject! :operator-and-auditor-must-have-distinct-pinned-keys))
    (let [primary (original-primary-intake
                   expected (select-keys envelope [:claim :signature-base64]))
          signed (canonical-auditor-cosign-bytes!
                  (:claim envelope) (:signature-base64 envelope))
          audit-signature (decode-signature!
                           (:auditor-signature-base64 envelope)
                           :malformed-independent-auditor-signature)
          verified? (try
                      (let [verifier (Signature/getInstance "Ed25519")]
                        (.initVerify verifier audit-root)
                        (.update verifier signed)
                        (.verify verifier audit-signature))
                      (catch Exception _ false))]
      (when-not verified?
        (reject! :invalid-independent-auditor-signature))
      (pinned!)
      ;; Retain every non-authoritative v1878 field. No signature, public key,
      ;; private source binding or operator/auditor identifying data leaks.
      (assoc primary
             :operator-and-auditor-key-separation :distinct-pinned-ed25519-public-keys
             :auditor-cosignature :valid-domain-separated-ed25519-cosignature
             :audit-cosignature-binds :exact-primary-signature-and-claim
             :independent-key-custody :not-established
             :independent-evidence-production :not-established
             :retention-enforcement :not-established
             :erasure-exclusion :not-established
             :history-completeness :not-established
             :discharged-conditions #{}
             :certificate :not-issued
             :transaction-authority :none
             :retry-authority :none
             :authority :none))))
