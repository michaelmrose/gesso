(ns gesso.assurance.proof.xtdb-gateway-pinned-candidate-key
  "v1897: Process-configured key pin for the v1896 signed witness *candidate*.

   The public verification key is parsed and frozen when a verifier is
   constructed, BEFORE a challenge is issued. The caller cannot supply or
   swap the verification key at challenge inspection time. Each verifier
   owns an identity-keyed set of its own issued challenges, refusing a
   challenge copied or issued by another verifier.

   The constructor's CONFIGURATION IS NOT an authenticated trust root:
   a caller can provision its own key, hold its private counterpart, and
   self-sign a challenge. A valid pinned-key signature is NOT evidence of
   signer independence, a physically observed transaction, complete writer
   coverage, or an admissible witness. No proof obligation is discharged,
   no certificate or execution authority is conferred."
  (:require [gesso.assurance.proof.xtdb-gateway-witness-candidate :as candidate])
  (:import [java.nio.charset StandardCharsets]
           [java.security KeyFactory MessageDigest PublicKey]
           [java.security.spec X509EncodedKeySpec]
           [java.util Arrays Base64 IdentityHashMap]))

(def verifier-type
  :gesso.assurance.proof.xtdb-gateway-pinned-candidate-key/process-pinned-verifier)
(def report-type
  :gesso.assurance.proof.xtdb-gateway-pinned-candidate-key/pinned-candidate-report)
(def ^:private max-per-verifier 16)
(def ^:private original-issue candidate/issue-candidate-challenge!)
(def ^:private original-inspect candidate/inspect-signed-candidate!)

(defn- reject! [reason]
  (throw (ex-info "Invalid pinned candidate public-key verification."
                  {:xtdb-pinned-candidate/rejection true :reason reason})))

(defn- configured-public-key [config]
  (when-not (and (map? config)
                 (= #{:ed25519-spki-base64} (set (keys config)))
                 (string? (:ed25519-spki-base64 config))
                 (<= 1 (count (:ed25519-spki-base64 config)) 512))
    (reject! :invalid-pinned-key-configuration))
  (let [encoded (:ed25519-spki-base64 config)
        der (try (.decode (Base64/getDecoder) ^String encoded)
                 (catch IllegalArgumentException _
                   (reject! :invalid-pinned-ed25519-spki)))
        ^PublicKey key (try (.generatePublic (KeyFactory/getInstance "Ed25519")
                                             (X509EncodedKeySpec. der))
                            (catch java.security.GeneralSecurityException _
                              (reject! :invalid-pinned-ed25519-spki)))]
    (when-not (and (= encoded (.encodeToString (Base64/getEncoder) der))
                   (<= 32 (alength der) 128)
                   (.equals "EdDSA" (.getAlgorithm key))
                   (Arrays/equals ^bytes der ^bytes (.getEncoded key)))
      (reject! :invalid-pinned-ed25519-spki))
    ;; Hash the immutable decoded SPKI, not a label or the caller's role
    ;; declaration. This is a *key identifier*, not a custody attestation.
    {:key key
     :fingerprint (str "sha256:"
                       (apply str (map #(format "%02x" (bit-and 0xff %))
                                       (.digest (MessageDigest/getInstance "SHA-256")
                                                ^bytes der))))}))

(defn prepare-process-pinned-verifier!
  "Freeze an Ed25519 public key from verifier CONFIGURATION at construction.
   Input is exactly {:ed25519-spki-base64 <canonical X.509 SPKI base64>}.

   :issue! requires the original private, admitted XTDB readback; :inspect!
   requires a challenge that THIS verifier actually issued and a strict
   signed-response envelope. No per-inspection public-key input exists.
   Configuration origin and signer independence remain UNVERIFIED even
   after successful pinned-key verification. This function is not a secure
   deployment provisioning or independent observer attestation mechanism."
  [config]
  (let [{:keys [^PublicKey key fingerprint]} (configured-public-key config)
        owned (IdentityHashMap.)
        issue! (fn [gateway original-return]
                 (when-not (identical? original-issue
                                       (var-get #'candidate/issue-candidate-challenge!))
                   (reject! :substituted-candidate-seam))
                 (locking owned
                   (when (>= (.size owned) max-per-verifier)
                     (reject! :too-many-pinned-challenges))
                   ;; The original candidate module still enforces the real
                   ;; gateway return and complete pinned correspondence.
                   (let [issued (original-issue gateway original-return)]
                     (.put owned issued Boolean/TRUE)
                     issued)))
        inspect! (fn [issued response]
                   ;; Consume even on a failed signature attempt. Another
                   ;; verifier's or a copied challenge cannot be borrowed.
                   (when-not (locking owned (.remove owned issued))
                     (reject! :not-issued-by-this-pinned-verifier))
                   (when-not (identical? original-inspect
                                         (var-get #'candidate/inspect-signed-candidate!))
                     (reject! :substituted-candidate-seam))
                   (let [checked (original-inspect issued response key)]
                     (when-not (= :ed25519-valid-for-caller-supplied-public-key
                                  (:cryptographic-check checked))
                       (reject! :unexpected-original-candidate-result))
                     (assoc checked
                            :type report-type
                            :version 1
                            :cryptographic-check
                            :ed25519-valid-for-process-configured-key
                            :pinned-spki-fingerprint fingerprint
                            :per-inspection-public-key-substitution :excluded
                            :key-configuration-origin
                            :process-configuration-not-authenticated
                            :key-custody :not-established
                            :key-provisioning-independence :not-established
                            :signer-observation-of-physical-event :not-established
                            :separately-attested-witness-identity :not-established
                            :witness-checked-for-obligation? false
                            :proof-discharge :none
                            :certificate :not-issued
                            :transaction-authority :none
                            :retry-authority :none
                            :authority :none)))]
    {:type verifier-type
     :version 1
     :pinned-spki-fingerprint fingerprint
     :configuration-source :process-configured-unattested
     :challenge-cap max-per-verifier
     :issue! issue!
     :inspect! inspect!
     :trusted-key-provisioning :not-established
     :signer-independence :not-established
     :physical-execution-causality :not-established
     :witness-admission :not-admitted
     :certificate :not-issued
     :authority :none}))
