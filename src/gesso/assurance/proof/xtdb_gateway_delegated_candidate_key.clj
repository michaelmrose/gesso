(ns gesso.assurance.proof.xtdb-gateway-delegated-candidate-key
  "v1898: A bounded Ed25519 key-delegation envelope for witness CANDIDATES.

   A configured root key verifies a signed, domain-separated grant for a
   DIFFERENT candidate key, limited to one explicit witness family, role,
   audience and validity interval. That candidate key is subsequently frozen
   inside the original v1897 pinned verifier. There is NO per-inspection key
   substitution and no new physical writer or trusted witness admission.

   IMPORTANT: The root is supplied by process configuration, not established
   as an independently provisioned trust anchor. A caller can manufacture a
   root, delegate and both signatures. Consequently a correctly checked grant
   establishes cryptographic DELEGATION UNDER THAT CONFIGURED KEY only, not
   independent custody, physical transaction causality, audit completeness,
   revocation, authenticated identity, proof discharge, or certification."
  (:require [gesso.assurance.proof.xtdb-gateway-pinned-candidate-key :as pinned])
  (:import [java.io ByteArrayOutputStream DataOutputStream]
           [java.nio.charset StandardCharsets]
           [java.security KeyFactory MessageDigest PublicKey Signature]
           [java.security.spec X509EncodedKeySpec]
           [java.time Instant Duration]
           [java.util Arrays Base64 UUID]))

(def verifier-type
  :gesso.assurance.proof.xtdb-gateway-delegated-candidate-key/delegated-candidate-verifier)
(def report-type
  :gesso.assurance.proof.xtdb-gateway-delegated-candidate-key/delegated-candidate-check)
(def ^:private signing-domain "gesso/xtdb/candidate-key-delegation/v1")
(def ^:private audience "gesso.xtdb.transaction-causality-candidate.v1")
(def ^:private obligation :exclusive-transaction-causality)
(def ^:private witness-family :transaction-coupled-apply-provenance)
(def ^:private max-grant-duration (Duration/ofHours 24))

(defn- reject! [reason]
  (throw (ex-info "Invalid candidate key delegation."
                  {:xtdb-delegated-candidate/rejection true :reason reason})))

(defn- parse-key [encoded]
  (when-not (and (string? encoded) (<= 1 (count encoded) 512))
    (reject! :invalid-ed25519-spki))
  (let [der (try (.decode (Base64/getDecoder) ^String encoded)
                 (catch IllegalArgumentException _
                   (reject! :invalid-ed25519-spki)))
        key (try (.generatePublic (KeyFactory/getInstance "Ed25519")
                                  (X509EncodedKeySpec. der))
                 (catch java.security.GeneralSecurityException _
                   (reject! :invalid-ed25519-spki)))]
    (when-not (and (<= 32 (alength der) 128)
                   (= encoded (.encodeToString (Base64/getEncoder) der))
                   (= "EdDSA" (.getAlgorithm ^PublicKey key))
                   (Arrays/equals ^bytes der ^bytes (.getEncoded ^PublicKey key)))
      (reject! :invalid-ed25519-spki))
    {:key key :der der
     :fingerprint (str "sha256:"
                       (apply str (map #(format "%02x" (bit-and 0xff %))
                                       (.digest (MessageDigest/getInstance "SHA-256")
                                                ^bytes der))))}))

(def ^:private grant-fields
  #{:version :delegation-id :audience :obligation :witness-family
    :delegated-ed25519-spki-base64 :valid-from :valid-until})

(defn- check-grant [grant]
  (when-not (and (map? grant)
                 (= grant-fields (set (keys grant)))
                 (= 1 (:version grant))
                 (= audience (:audience grant))
                 (= obligation (:obligation grant))
                 (= witness-family (:witness-family grant))
                 (string? (:delegation-id grant))
                 (<= 1 (count (:delegation-id grant)) 64))
    (reject! :invalid-delegation-scope))
  (let [id (try (UUID/fromString ^String (:delegation-id grant))
                (catch IllegalArgumentException _ nil))
        from (try (Instant/parse ^String (:valid-from grant))
                  (catch Exception _ nil))
        until (try (Instant/parse ^String (:valid-until grant))
                   (catch Exception _ nil))]
    (when-not (and id (= (str id) (:delegation-id grant))
                   from until
                   (= (str from) (:valid-from grant))
                   (= (str until) (:valid-until grant))
                   (.isBefore ^Instant from ^Instant until)
                   (not (pos? (.compareTo (Duration/between from until)
                                           max-grant-duration))))
      (reject! :invalid-delegation-window-or-id))
    {:from from :until until
     :delegate (parse-key (:delegated-ed25519-spki-base64 grant))}))

(defn delegation-signing-bytes
  "Canonical bytes the separately authorized ROOT would sign.
   Public data serialization, not a signing service or trust anchor.
   Validates the exact purpose, audience, ID, window and Ed25519 SPKI.
   These bytes include every admitted grant field and a domain separator."
  [grant]
  (check-grant grant)
  (let [buffer (ByteArrayOutputStream.)]
    (with-open [out (DataOutputStream. buffer)]
      (.writeUTF out signing-domain)
      (.writeInt out 1)
      (doseq [field [:delegation-id :audience :obligation :witness-family
                     :delegated-ed25519-spki-base64 :valid-from :valid-until]]
        (.writeUTF out (str (get grant field)))))
    (.toByteArray buffer)))

(defn- valid-at! [from until]
  (let [now (Instant/now)]
    (when-not (and (not (.isBefore ^Instant now ^Instant from))
                   (.isBefore ^Instant now ^Instant until))
      (reject! :delegation-not-currently-valid))))

(defn prepare-delegated-candidate-verifier!
  "Prepare a role-scoped witness CANDIDATE verifier from strict config:
   {:root-ed25519-spki-base64 <canonical root SPKI>
    :delegation <exact signed-grant map>
    :root-signature-base64 <64-byte signature, canonical base64>}.

   Verify the signature over delegation-signing-bytes with the configured
   root, require distinct root/delegate keys and a current <=24h validity
   window, then construct v1897's private delegate-key pinned verifier.
   :issue! and :inspect! preserve its real pinned-XTDB prerequisite,
   challenge ownership, single use, and immutable signature key; expiry is
   checked on each issuance and again after each consumed inspection.

   No verified role attestation here can establish the ROOT's independent
   provisioning or the delegate's actual physical observation of XTDB."
  [config]
  (when-not (and (map? config)
                 (= #{:root-ed25519-spki-base64 :delegation
                      :root-signature-base64}
                    (set (keys config))))
    (reject! :invalid-delegation-configuration))
  (let [root (parse-key (:root-ed25519-spki-base64 config))
        grant (:delegation config)
        {:keys [from until delegate]} (check-grant grant)
        bytes (delegation-signing-bytes grant)
        encoded (:root-signature-base64 config)
        signature-bytes
        (try (when (and (string? encoded) (<= 1 (count encoded) 128))
               (.decode (Base64/getDecoder) ^String encoded))
             (catch IllegalArgumentException _ nil))]
    (when (Arrays/equals ^bytes (:der root) ^bytes (:der delegate))
      (reject! :root-must-differ-from-delegate))
    (when-not (and signature-bytes
                   (= 64 (alength ^bytes signature-bytes))
                   (= encoded (.encodeToString (Base64/getEncoder)
                                                ^bytes signature-bytes))
                   (try
                     (let [verifier (Signature/getInstance "Ed25519")]
                       (.initVerify verifier ^PublicKey (:key root))
                       (.update verifier ^bytes bytes)
                       (.verify verifier ^bytes signature-bytes))
                     (catch java.security.GeneralSecurityException _ false)))
      (reject! :root-signature-not-verified))
    (valid-at! from until)
    (let [p (pinned/prepare-process-pinned-verifier!
             {:ed25519-spki-base64 (:delegated-ed25519-spki-base64 grant)})
          issue! (fn [gateway original-return]
                   (valid-at! from until)
                   ((:issue! p) gateway original-return))
          inspect! (fn [issued response]
                     (let [verified ((:inspect! p) issued response)]
                       (valid-at! from until)
                       (assoc verified
                              :type report-type
                              :version 1
                              :delegation-signature-check
                              :valid-for-configured-root-key
                              :configured-root-spki-fingerprint
                              (:fingerprint root)
                              :delegated-key-spki-fingerprint
                              (:fingerprint delegate)
                              :delegation-purpose :exclusive-transaction-causality
                              :delegation-audience :gesso-xtdb-candidate-only
                              :delegation-root-independent-provisioning
                              :not-established
                              :delegate-key-custody :not-established
                              :physical-event-correspondence :not-established
                              :witness-admission :not-admitted
                              :proof-discharge :none
                              :certificate :not-issued
                              :transaction-authority :none
                              :retry-authority :none
                              :authority :none)))]
      {:type verifier-type
       :version 1
       :delegation-scope :exclusive-transaction-causality-candidate-only
       :delegation-signature-check :valid-for-configured-root-key
       :configured-root-spki-fingerprint (:fingerprint root)
       :delegated-key-spki-fingerprint (:fingerprint delegate)
       :issue! issue!
       :inspect! inspect!
       :root-configuration-origin :process-supplied-not-authenticated
       :root-independent-provisioning :not-established
       :delegate-independent-physical-observation :not-established
       :revocation-and-complete-issuance-inventory :not-established
       :witness-admission :not-admitted
       :proof-discharge :none
       :certificate :not-issued
       :authority :none})))
