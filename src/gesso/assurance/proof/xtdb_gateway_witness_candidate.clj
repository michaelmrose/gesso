(ns gesso.assurance.proof.xtdb-gateway-witness-candidate
  "v1896: One-shot, cryptographically checkable external *candidate* statement.

   A private, exact original gateway return and an independent pinned XTDB
   readback are prerequisites for issuing a bounded signing challenge for
   ONE witness family: transaction-coupled apply provenance. The challenge
   is domain-separated, non-reusable and bound to the original in-process
   attempt plus the redacted observational premises.

   A valid Ed25519 signature establishes only possession of the caller-
   supplied public key's private counterpart. It does NOT establish key
   custody, node authentication, signer independence, producer completeness,
   transaction causality, or the contents of any actual physical event.
   Neither a successful nor unsuccessful response can discharge ANY open
   obligation, issue a certificate, or grant authority. There is no ERASE
   or transaction path here. This is a candidate-handshake, NOT a witness
   admission or proof checker."
  (:require [gesso.assurance.proof.xtdb-gateway-atomic-write-intent :as intent])
  (:import [java.io ByteArrayOutputStream DataOutputStream]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest PublicKey Signature]
           [java.util Base64 IdentityHashMap UUID]))

(def challenge-type
  :gesso.assurance.proof.xtdb-gateway-witness-candidate/signing-challenge)
(def report-type
  :gesso.assurance.proof.xtdb-gateway-witness-candidate/checked-candidate)
(def ^:private domain "gesso/xtdb/transaction-causality-candidate/v1")
(def ^:private obligation :exclusive-transaction-causality)
(def ^:private witness-family :transaction-coupled-apply-provenance)
(def ^:private required-node :protected-before-after-history-correlation)
(def ^:private original-inspect intent/inspect-pinned-intent!)
(def ^:private max-outstanding 64)
(defonce ^:private ^IdentityHashMap pending (IdentityHashMap.))

(defn- reject! [reason]
  (throw (ex-info "Invalid transaction-causality witness candidate."
                  {:xtdb-witness-candidate/rejection true :reason reason})))

(defn- challenge-bytes [nonce report]
  (let [out (ByteArrayOutputStream.)
        digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes (pr-str (:correspondence-premises report))
                                   StandardCharsets/UTF_8))]
    (with-open [writer (DataOutputStream. out)]
      (.writeUTF writer domain)
      (.writeUTF writer (str nonce))
      (.writeUTF writer (str (:original-local-attempt-id report)))
      (.writeUTF writer (str obligation))
      (.writeUTF writer (str witness-family))
      (.writeUTF writer (str required-node))
      (.writeInt writer (alength digest))
      (.write writer digest))
    (.toByteArray out)))

(defn issue-candidate-challenge!
  "Issue a challenge ONLY after a real private original-gateway readback
   supports the maximal BOUNDED observation. The returned base64 is the exact
   bytes for an outside producer to sign, not proof the producer is outside.
   At most 64 challenges may be outstanding in this JVM; each is single-use.
   No raw XTDB row, transaction key, snapshot token or private document is
   returned. The private challenge registry is local, volatile and NOT a
   deployment trust root or durable provenance record."
  [gateway original-return]
  (when-not (identical? original-inspect
                         (var-get #'intent/inspect-pinned-intent!))
    (reject! :substituted-original-observation))
  (let [report (original-inspect gateway original-return)
        premises (:correspondence-premises report)
        frontier (:assurance-frontier premises)]
    (when-not (and (= :bounded-private-before-after-intent-ledger-coobservation
                      (:protected-before-after-intent-ledger-correlation report))
                   (= required-node (:strongest-bounded-node premises))
                   (= :unmet
                      (get-in frontier [:requirements obligation :status]))
                   (= [] (:closed-obligations premises)
                      (:closed-obligations frontier))
                   (= :not-issued (:certificate frontier))
                   (= :none (:proof-discharge frontier)))
      (reject! :incomplete-private-observational-premises))
    (let [bytes (challenge-bytes (UUID/randomUUID) report)
          encoded (.encodeToString (Base64/getEncoder) bytes)
          descriptor {:type challenge-type
                      :version 1
                      :obligation obligation
                      :witness-family witness-family
                      :signing-bytes-base64 encoded
                      :candidate-scope :single-original-attempt-and-pinned-readback
                      :signer-independence :not-established
                      :proof-discharge :none
                      :certificate :not-issued
                      :authority :none}]
      (locking pending
        (when (>= (.size pending) max-outstanding)
          (reject! :too-many-outstanding-challenges))
        (.put pending descriptor bytes))
      descriptor)))

(defn inspect-signed-candidate!
  "Verify a one-use candidate signature over the EXACT issued challenge.
   The caller-provided PublicKey is NOT authenticated or independently
   provisioned by this function; signature success cannot meet the witness
   frontier's trust or completeness requirement. A failed attempt consumes
   the challenge. No original readback is replayed or reauthorized."
  [challenge response ^PublicKey public-key]
  (let [issued (locking pending (.remove pending challenge))]
    (when-not issued (reject! :not-an-original-outstanding-challenge))
    (when-not (and (map? response)
                   (= #{:signature-base64} (set (keys response)))
                   (string? (:signature-base64 response))
                   (<= 1 (count (:signature-base64 response)) 128)
                   (instance? PublicKey public-key)
                   (contains? #{"Ed25519" "EdDSA"} (.getAlgorithm public-key)))
      (reject! :invalid-candidate-response))
    (let [raw (try (.decode (Base64/getDecoder)
                            ^String (:signature-base64 response))
                   (catch IllegalArgumentException _
                     (reject! :invalid-candidate-signature)))
          verified (try
                     (and (= 64 (alength raw))
                          (let [v (Signature/getInstance "Ed25519")]
                            (.initVerify v public-key)
                            (.update v ^bytes issued)
                            (.verify v ^bytes raw)))
                     (catch java.security.GeneralSecurityException _ false))]
      (when-not verified (reject! :candidate-signature-not-verified))
      {:type report-type
       :version 1
       :scope :one-original-private-gateway-readback-challenge
       :obligation obligation
       :witness-family witness-family
       :cryptographic-check :ed25519-valid-for-caller-supplied-public-key
       :challenge-consumption :single-use
       :public-key-trust :not-established
       :signer-independence :not-established
       :physical-event-production :not-established
       :transaction-causality :not-established
       :witness-admission :not-admitted
       :witness-checked-for-obligation? false
       :open-obligations-remain-open? true
       :proof-discharge :none
       :certificate :not-issued
       :transaction-authority :none
       :retry-authority :none
       :authority :none})))
