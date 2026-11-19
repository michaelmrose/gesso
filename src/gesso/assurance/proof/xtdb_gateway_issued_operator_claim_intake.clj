(ns gesso.assurance.proof.xtdb-gateway-issued-operator-claim-intake
  "v1878: An independently configured, Ed25519-verified intake for external
   retention/erasure ASSERTIONS. This verifies statement integrity and the
   signer's possession of the private key corresponding to an OPERATOR-pinned
   public key, NOT the truth, enforcement, completeness, freshness, or
   deployment provenance of the assertions. It cannot discharge any history
   obligation or authorize a transaction. No operator evidence producer or
   erasure-audit verifier is installed here.

   The production verification entry obtains its trust root solely from a
   process-level environment variable, never a caller's request, application
   catalog, signature envelope or supplied expected-source identity. The
   operating environment's configuration integrity is an EXTERNAL assumption."
  (:import (java.io ByteArrayOutputStream DataOutputStream)
           (java.security KeyFactory PublicKey Signature)
           (java.security.spec X509EncodedKeySpec)
           (java.time Instant)
           (java.util Base64 UUID)))

(def report-type
  :gesso.assurance.proof.xtdb-gateway-issued-operator-claim-intake/signed-assertion)
(def report-version 1)
(def envelope-format "gesso.xtdb.history.operator-claims.v1")
(def operator-key-env "GESSO_XTDB_HISTORY_OPERATOR_ED25519_PUBLIC_KEY_B64")

;; All binding fields are signed. They are source-relative until some
;; INDEPENDENT mechanism attests the original executor and physical target.
(def binding-fields
  [:operation-id :proof-fingerprint :proof-source-fingerprint
   :proof-subject-fingerprint :dependency-graph-fingerprint
   :transition-fingerprint :database-instance :physical-table :physical-row-id
   :system-from :system-to])

(def ^:private signed-fields
  (vec (concat [:format] binding-fields
               [:retention-policy-reference :retention-assertion
                :erasure-audit-reference :erasure-audit-assertion
                :signed-at :expires-at :nonce])))

(defn- reject! [reason]
  (throw (ex-info "Independent operator-signed history claim not established."
                  {:reason reason})))

(defn- plain-map? [m]
  (and (map? m) (nil? (meta m))))

(defn- bounded-string? [x]
  (and (string? x) (<= 1 (count x) 1024)))

(defn- valid-binding? [m]
  (and (plain-map? m)
       (= (set binding-fields) (set (keys m)))
       (qualified-keyword? (:operation-id m))
       (every? #(bounded-string? (get m %))
               [:proof-fingerprint :proof-source-fingerprint
                :proof-subject-fingerprint :dependency-graph-fingerprint
                :transition-fingerprint :database-instance :physical-row-id])
       (keyword? (:physical-table m))
       (nil? (namespace (:physical-table m)))
       (re-matches #"[a-z][a-z0-9_]*" (name (:physical-table m)))
       (instance? Instant (:system-from m))
       (instance? Instant (:system-to m))
       (neg? (compare (:system-from m) (:system-to m)))))

(defn- valid-signed-claim? [claim]
  (and (plain-map? claim)
       (= (set signed-fields) (set (keys claim)))
       (= envelope-format (:format claim))
       (valid-binding? (select-keys claim binding-fields))
       (bounded-string? (:retention-policy-reference claim))
       (bounded-string? (:erasure-audit-reference claim))
       (= :operator-asserts-retention-enforced (:retention-assertion claim))
       (= :operator-asserts-no-erasure (:erasure-audit-assertion claim))
       (instance? Instant (:signed-at claim))
       (instance? Instant (:expires-at claim))
       (instance? UUID (:nonce claim))
       (neg? (compare (:signed-at claim) (:expires-at claim)))
       (not (pos? (.compareTo (:expires-at claim)
                             (.plusSeconds ^Instant (:signed-at claim) 86400))))))

(defn canonical-claim-bytes!
  "Unambiguous, length-delimited, versioned binary serialization for an
   external issuer to sign with Ed25519. This is a PUBLIC encoding utility,
   not an authorization or verification operation. No Clojure reader/eval."
  [claim]
  (when-not (valid-signed-claim? claim)
    (reject! :malformed-operator-history-claim))
  (let [bytes (ByteArrayOutputStream.)
        stream (DataOutputStream. bytes)]
    (.writeUTF stream "gesso/xtdb/issued-history/operator-claim/v1")
    (doseq [field signed-fields]
      (.writeUTF stream (str (get claim field))))
    (.flush stream)
    (.toByteArray bytes)))

(defn- decode-sig! [encoded]
  (when-not (and (string? encoded)
                 (<= 1 (count encoded) 256))
    (reject! :malformed-operator-claim-signature))
  (let [bytes (try (.decode (Base64/getDecoder) ^String encoded)
                   (catch IllegalArgumentException _
                     (reject! :malformed-operator-claim-signature)))]
    (when-not (= 64 (alength bytes))
      (reject! :malformed-operator-claim-signature))
    bytes))

(defn- pinned-operator-root!
  []
  (let [encoded (System/getenv operator-key-env)]
    (when-not (bounded-string? encoded)
      (reject! :independent-operator-trust-root-not-configured))
    (try
      (let [bytes (.decode (Base64/getDecoder) ^String encoded)
            public (.generatePublic (KeyFactory/getInstance "Ed25519")
                                    (X509EncodedKeySpec. bytes))]
        (when-not (= "EdDSA" (.getAlgorithm public))
          (reject! :invalid-independent-operator-trust-root))
        public)
      (catch Exception _
        (reject! :invalid-independent-operator-trust-root)))))

(defn- inspect-under-key!
  "Private crypto primitive; a test-supplied key proves only possession of
   that key, NOT independent operator provenance. The production entry alone
   loads its key from an operator-controlled runtime environment."
  [^PublicKey public-key expected envelope anchor]
  (when-not (valid-binding? expected)
    (reject! :invalid-source-relative-history-binding))
  (when-not (and (plain-map? envelope)
                 (= #{:claim :signature-base64} (set (keys envelope))))
    (reject! :malformed-operator-claim-envelope))
  (let [claim (:claim envelope)
        bytes (canonical-claim-bytes! claim)
        signature-bytes (decode-sig! (:signature-base64 envelope))
        now (Instant/now)]
    (when-not (= expected (select-keys claim binding-fields))
      (reject! :operator-claim-source-binding-mismatch))
    (when-not (and (not (pos? (compare (:signed-at claim)
                                       (.plusSeconds now 60))))
                   (pos? (compare (:expires-at claim) now)))
      (reject! :expired-or-future-operator-history-claim))
    (let [verified? (try (let [v (Signature/getInstance "Ed25519")]
                           (.initVerify v public-key)
                           (.update v bytes)
                           (.verify v signature-bytes))
                         (catch Exception _ false))]
      (when-not verified?
        (reject! :invalid-operator-claim-signature))
      {:type report-type :version report-version
       :claim-kind :operator-signed-retention-and-erasure-assertions
       :signature-integrity :valid-ed25519-under-selected-key
       :trust-root-source anchor
       :scope-binding :matches-source-relative-expected-fields
       :signing-key-deployment-provenance :not-independently-established
       :retention-policy-claim :signed-assertion-not-verified-enforcement
       :erasure-audit-claim :signed-assertion-not-verified-completeness
       :claimed-target-authenticity :not-established
       :claimed-snapshot-authenticity :not-established
       :retention-enforcement :not-established
       :erasure-exclusion :not-established
       :history-completeness :not-established
       :replay-exclusion :not-established
       :discharged-conditions #{}
       :original-kernel-proof-discharge :not-established
       :certificate :not-issued
       :transaction-authority :none
       :retry-authority :none
       :authority :none})))

(defn inspect-operator-history-claim!
  "Verify a detached Ed25519 claim under the ONE operator-pinned public key
   installed through the process environment. Expected binding MUST come
   from original, independently rechecked source/context, never the claim.
   This checks authenticity of a signed STATEMENT only; it is NOT evidence
   that the retention policy or erasure audit was enforced or exhaustive.

   No configured independent operator key => fail closed. A signature cannot
   create history assurance, proof discharge or physical write authority."
  [expected envelope]
  (let [key (pinned-operator-root!)]
    (inspect-under-key! key expected envelope
                        :process-environment-operator-pinned-ed25519-key)))
