(ns gesso.assurance.proof.xtdb-gateway-externally-anchored-floor
  "v1903: Fail-closed signed external rollback-floor *protocol adapter*.

   A separately configured Ed25519 key signs nonce-bound read responses and
   compare-and-advance receipts from a caller-supplied external register.
   Before each operation the signed high-water mark MUST equal the exact
   head of the independently reverified v1902 local file. On append, the
   local file commits first; the adapter then requests a signed monotonic
   external advance, verifies the receipt, and makes a NEW nonce-bound read.
   Any uncertain outcome after the local append permanently quarantines the
   instance; reopening permits recovery only when the signed external head
   and the physically verified local head already agree exactly.

   Neither the callbacks nor their public key are independently provisioned
   or authenticated HERE. The adapter does not ship a trusted register or
   rescue a split commit automatically. No two-resource atomicity, complete
   revocation, physical XTDB causality, proof discharge or certificate exists.

   v1908: After local commit, retain the verified client distinction between
   a SIGNED, request-bound stale CAS rejection and a genuinely uncertain
   external CAS outcome. Both irrevocably quarantine this adapter: a remote
   rejection does NOT roll back the local signed checkpoint or permit retry.
   Classification trusts only the explicitly configured client callback to
   have performed the v1907 signature check; user-supplied callback exception
   data alone is NOT an independently authenticated witness. A new adapter
   and fresh signed read must still establish exact-head recovery.

   v1909: A quarantined instance can create a READ-ONLY, redacted recovery
   assessment, separately revalidating its original committed local head
   and a newly nonce-bound signed register read. An observation never clears
   quarantine, mutates either store, retries CAS, issues admission, or
   substitutes for constructing a new adapter. Reports are ephemeral and
   audit-ready, NOT persisted audit events or attestations."
  (:require [gesso.assurance.proof.xtdb-gateway-durable-checkpoint-store :as durable])
  (:import [java.io ByteArrayOutputStream DataOutputStream]
           [java.security KeyFactory MessageDigest PublicKey Signature]
           [java.security.spec X509EncodedKeySpec]
           [java.util Arrays Base64 UUID]
           [java.nio.file Path]))

(def statement-type
  :gesso.assurance.proof.xtdb-gateway-externally-anchored-floor/signed-floor-statement)
(def adapter-type
  :gesso.assurance.proof.xtdb-gateway-externally-anchored-floor/adapter)
(def ^:private domain "gesso/xtdb/external-policy-floor/v1")
(def ^:private digest-pattern #"sha256:[0-9a-f]{64}")

(defn- reject! [reason]
  (throw (ex-info "Invalid external rollback-floor protocol."
                  {:xtdb-external-floor/rejection true :reason reason})))

(defn- valid-floor? [f]
  (and (map? f) (= #{:revision :checkpoint-digest} (set (keys f)))
       (integer? (:revision f)) (<= 0 (:revision f) 1000000)
       (if (zero? (:revision f))
         (= "genesis" (:checkpoint-digest f))
         (and (string? (:checkpoint-digest f))
              (boolean (re-matches digest-pattern (:checkpoint-digest f)))))))

(defn- valid-nonce? [s]
  (and (string? s)
       (<= 1 (count s) 64)
       (try (= s (str (UUID/fromString s)))
            (catch IllegalArgumentException _ false))))

(defn- valid-statement? [s]
  (and (map? s)
       (= statement-type (:type s)) (= 1 (:version s))
       (valid-nonce? (:nonce s))
       (case (:kind s)
         :read (and (= #{:type :version :kind :nonce :floor} (set (keys s)))
                    (valid-floor? (:floor s)))
         :advance (and (= #{:type :version :kind :nonce :expected-floor
                             :next-floor :accepted?} (set (keys s)))
                       (true? (:accepted? s))
                       (valid-floor? (:expected-floor s))
                       (valid-floor? (:next-floor s))
                       (= (inc (:revision (:expected-floor s)))
                          (:revision (:next-floor s)))
                       (not= (:checkpoint-digest (:expected-floor s))
                             (:checkpoint-digest (:next-floor s))))
         false)))

(defn floor-signing-bytes
  "Canonical, domain-separated signed external floor statement (public format).
   This method only validates and serializes plain data; signing it with a
   local key cannot establish independent register service or key custody."
  [statement]
  (when-not (valid-statement? statement)
    (reject! :invalid-floor-statement))
  (let [buffer (ByteArrayOutputStream.)]
    (with-open [out (DataOutputStream. buffer)]
      (.writeUTF out domain)
      (.writeInt out 1)
      (.writeUTF out (name (:kind statement)))
      (.writeUTF out (:nonce statement))
      (doseq [f (case (:kind statement)
                  :read [(:floor statement)]
                  :advance [(:expected-floor statement) (:next-floor statement)])]
        (.writeInt out (int (:revision f)))
        (.writeUTF out (:checkpoint-digest f)))
      (when (= :advance (:kind statement))
        (.writeBoolean out true)))
    (.toByteArray buffer)))

(defn- pinned-key [encoded]
  (when-not (and (string? encoded) (<= 1 (count encoded) 512))
    (reject! :invalid-external-signing-key))
  (let [der (try (.decode (Base64/getDecoder) ^String encoded)
                 (catch IllegalArgumentException _
                   (reject! :invalid-external-signing-key)))
        ^PublicKey key (try (.generatePublic (KeyFactory/getInstance "Ed25519")
                                            (X509EncodedKeySpec. der))
                            (catch java.security.GeneralSecurityException _
                              (reject! :invalid-external-signing-key)))]
    (when-not (and (<= 32 (alength der) 128)
                   (= encoded (.encodeToString (Base64/getEncoder) der))
                   (= "EdDSA" (.getAlgorithm key))
                   (Arrays/equals ^bytes der ^bytes (.getEncoded key)))
      (reject! :invalid-external-signing-key))
    {:key key :der der
     :fingerprint
     (str "sha256:" (apply str (map #(format "%02x" (bit-and 0xff %))
                                    (.digest (MessageDigest/getInstance "SHA-256")
                                             ^bytes der))))}))

(defn- verify-signed! [envelope ^PublicKey key expected]
  (when-not (and (map? envelope)
                 (= #{:statement :signature-base64} (set (keys envelope)))
                 (= expected (:statement envelope))
                 (string? (:signature-base64 envelope))
                 (<= 1 (count (:signature-base64 envelope)) 128))
    (reject! :external-floor-response-not-request-bound))
  (let [encoded (:signature-base64 envelope)
        raw (try (.decode (Base64/getDecoder) ^String encoded)
                 (catch IllegalArgumentException _
                   (reject! :invalid-external-floor-signature)))
        valid (and (= 64 (alength raw))
                   (= encoded (.encodeToString (Base64/getEncoder) raw))
                   (try (let [v (Signature/getInstance "Ed25519")]
                          (.initVerify v key)
                          (.update v ^bytes (floor-signing-bytes expected))
                          (.verify v ^bytes raw))
                        (catch java.security.GeneralSecurityException _ false)))]
    (when-not valid (reject! :external-floor-signature-not-verified))
    true))

(defn prepare-external-floor-adapter!
  "Create a synchronous external-register protocol adapter.

   Exact config: {:directory Path :policy v1900-policy
    :checkpoint-authority-ed25519-spki-base64 canonical-SPKI
    :external-floor-ed25519-spki-base64 DIFFERENT canonical-SPKI
    :read-external-floor! (fn [{:kind :read :nonce UUID}] signed-envelope)
    :advance-external-floor! (fn [strict-advance-statement] signed-envelope)
    :initialize-empty? boolean}.

   External :read must sign a strict :read statement bearing the new nonce;
   external :advance must implement actual monotonic compare-and-swap and
   sign its exact accepted request. These assumptions are NOT proved by the
   callback or its own signature. On startup and before each append, local
   head must equal signed external head (not merely contain the floor).
   After a local append, failure to confirm external advance quarantines the
   adapter; explicit operator recovery may align the register and local
   store, then construct a NEW adapter. No automatic rollback/rollforward."
  [config]
  (when-not (and (map? config)
                 (= #{:directory :policy :checkpoint-authority-ed25519-spki-base64
                      :external-floor-ed25519-spki-base64 :read-external-floor!
                      :advance-external-floor! :initialize-empty?}
                    (set (keys config)))
                 (instance? Path (:directory config))
                 (map? (:policy config))
                 (fn? (get-in config [:policy :checkpoint-material!]))
                 (fn? (:read-external-floor! config))
                 (fn? (:advance-external-floor! config))
                 (boolean? (:initialize-empty? config)))
    (reject! :invalid-external-floor-configuration))
  (let [{external-key :key external-fingerprint :fingerprint external-der :der}
        (pinned-key (:external-floor-ed25519-spki-base64 config))
        {local-der :der}
        (pinned-key (:checkpoint-authority-ed25519-spki-base64 config))
        _ (when (Arrays/equals ^bytes local-der ^bytes external-der)
            (reject! :external-floor-key-must-differ-from-checkpoint-key))
        poisoned (atom false)
        ;; Original two-resource attempt is frozen at LOCAL commit. Keep
        ;; its exact before/after heads even if another actor later changes
        ;; either resource. No callback or caller can rewrite this binding.
        unresolved (atom nil)
        monitor (Object.)
        read-floor! (fn []
                      (let [nonce (str (UUID/randomUUID))
                            request {:kind :read :nonce nonce}
                            response (try ((:read-external-floor! config) request)
                                          (catch Throwable _
                                            (reject! :external-floor-read-unavailable)))
                            statement (:statement response)]
                        (when-not (and (valid-statement? statement)
                                       (= :read (:kind statement))
                                       (= nonce (:nonce statement)))
                          (reject! :external-floor-response-not-request-bound))
                        (verify-signed! response external-key statement)
                        (:floor statement)))
        checked-local! (fn []
                         (let [floor (read-floor!)
                               local (durable/prepare-local-durable-store!
                                      {:directory (:directory config)
                                       :policy (:policy config)
                                       :authority-ed25519-spki-base64
                                       (:checkpoint-authority-ed25519-spki-base64 config)
                                       :external-rollback-floor floor
                                       :initialize-empty? (:initialize-empty? config)})
                               head ((:status! local))]
                           (when-not (= floor {:revision (:head-revision head)
                                               :checkpoint-digest (:head-digest head)})
                             (reject! :external-floor-local-head-divergence))
                           {:floor floor :local local :head head}))
        status! (fn []
                  (locking monitor
                    (when @poisoned (reject! :unconfirmed-external-floor-advance))
                    (let [{:keys [floor head]} (checked-local!)]
                      {:type adapter-type :version 1
                       :head-revision (:revision floor)
                       :head-digest (:checkpoint-digest floor)
                       :signed-external-floor :verified-for-fresh-random-nonce
                       :configured-external-key-fingerprint external-fingerprint
                       :local-chain :matched-exact-external-high-water-mark
                       :external-register-independence :not-established
                       :external-register-monotonicity :assumed-not-verified
                       :external-key-provisioning :process-configured-not-authenticated
                       :cross-resource-atomicity :not-established
                       :proof-discharge :none :certificate :not-issued
                       :authority :none})))
        append! (fn [signed-checkpoint]
                  (locking monitor
                    (when @poisoned (reject! :unconfirmed-external-floor-advance))
                    (let [{:keys [floor local]} (checked-local!)
                          local-result ((:append! local) signed-checkpoint)
                          new-floor {:revision (:head-revision local-result)
                                     :checkpoint-digest (:head-digest local-result)}
                          request {:type statement-type :version 1
                                   :kind :advance :nonce (str (UUID/randomUUID))
                                   :expected-floor floor :next-floor new-floor
                                   :accepted? true}]
                      ;; This local file is already committed. From now on
                      ;; even a malformed response or callback exception is
                      ;; an uncertain 2-resource outcome, never a retryable
                      ;; failed checkpoint submission in this instance.
                      (reset! unresolved
                              {:attempt-id (str (UUID/randomUUID))
                               :prior-floor floor
                               :committed-floor new-floor
                               :client-reported-classification :pending})
                      (reset! poisoned true)
                      (try
                        (let [response ((:advance-external-floor! config) request)]
                          (verify-signed! response external-key request)
                          (when-not (= new-floor (read-floor!))
                            (reject! :external-advance-not-visible-on-fresh-read))
                          (reset! unresolved nil)
                          (reset! poisoned false)
                          {:type adapter-type :version 1
                           :head-revision (:revision new-floor)
                           :head-digest (:checkpoint-digest new-floor)
                           :external-advance :signed-compare-and-advance-confirmed
                           :post-advance-read :freshly-signed-and-matches-local-head
                           :cross-resource-atomicity :not-established
                           :external-register-independence :not-established
                           :external-register-monotonicity :assumed-not-verified
                           :proof-discharge :none :certificate :not-issued
                           :authority :none})
                        (catch Throwable ex
                          ;; A v1907 client can tell a signature-verified
                          ;; stale CAS refusal from an unknown transport
                          ;; outcome. The callback is configuration-supplied;
                          ;; these are *reported* facts, not a second
                          ;; independent check of its signature here.
                          ;; The durable LOCAL append already happened.
                          ;; Never reset poisoned, retry, roll back or
                          ;; promote either outcome to checkpoint acceptance.
                          (let [data (ex-data ex)
                                client-reported-rejection?
                                (and (= :register-client-authenticated-stale-cas
                                        (:reason data))
                                     (= :rejected-not-applied (:outcome data))
                                     (true? (:signature-verified-for-pinned-register-key?
                                             data))
                                     (true? (:no-automatic-retry data)))]
                            (swap! unresolved assoc
                                   :client-reported-classification
                                   (if client-reported-rejection?
                                     :client-reported-verified-stale-cas
                                     :unconfirmed-outcome))
                            (throw
                             (ex-info
                              "External register update is not confirmed for the locally committed checkpoint."
                              {:xtdb-external-floor/rejection true
                               :reason (if client-reported-rejection?
                                         :external-floor-advance-client-reported-verified-rejection
                                         :external-floor-advance-unconfirmed)
                               :local-checkpoint :already-committed
                               :reported-external-advance-outcome
                               (if client-reported-rejection?
                                 :rejected-not-applied :unknown)
                               :client-signature-check
                               (if client-reported-rejection?
                                 :reported-verified-by-configured-client
                                 :not-established)
                               :adapter-state :permanently-quarantined
                               :recovery :new-adapter-requires-fresh-signed-exact-head
                               :no-automatic-retry true
                               :proof-discharge :none
                               :certificate :not-issued
                               :authority :none}
                              ex))))))))
        inspect-recovery! (fn []
                            (locking monitor
                              (when-not (and @poisoned @unresolved)
                                (reject! :recovery-inspection-requires-quarantined-attempt))
                              (let [{:keys [attempt-id prior-floor committed-floor
                                            client-reported-classification]}
                                    @unresolved
                                    ;; Re-open the complete signed history at a
                                    ;; genesis floor solely for observation.
                                    ;; It must NOT validate or admit remote
                                    ;; recovery on its own. `read-floor!`
                                    ;; independently validates a fresh nonce
                                    ;; and the configured register signature.
                                    observed
                                    (try
                                      (let [remote (read-floor!)
                                            store (durable/prepare-local-durable-store!
                                                   {:directory (:directory config)
                                                    :policy (:policy config)
                                                    :authority-ed25519-spki-base64
                                                    (:checkpoint-authority-ed25519-spki-base64 config)
                                                    :external-rollback-floor
                                                    {:revision 0
                                                     :checkpoint-digest "genesis"}
                                                    :initialize-empty? false})
                                            head ((:status! store))
                                            local {:revision (:head-revision head)
                                                   :checkpoint-digest (:head-digest head)}]
                                        {:remote remote :local local
                                         :observation :fresh-signed-read-and-local-signed-chain})
                                      (catch Throwable _
                                        ;; An unavailable or invalid signed
                                        ;; read is NOT evidence of a remote
                                        ;; refusal/acceptance. No retry here.
                                        {:observation :unable-to-observe-both-heads}))
                                    remote (:remote observed)
                                    local (:local observed)
                                    result (cond
                                             (not= :fresh-signed-read-and-local-signed-chain
                                                   (:observation observed))
                                             :reconciliation-observation-unavailable
                                             (not= local committed-floor)
                                             :local-history-diverges-from-committed-attempt
                                             (= remote committed-floor)
                                             :fresh-signed-head-matches-committed-local-head
                                             (= remote prior-floor)
                                             :fresh-signed-register-still-at-prior-head
                                             :else
                                             :fresh-signed-register-head-diverges)]
                                {:type
                                 :gesso.assurance.proof.xtdb-gateway-externally-anchored-floor/recovery-assessment
                                 :version 1
                                 :attempt-id attempt-id
                                 :original-advance-classification client-reported-classification
                                 :original-local-checkpoint :already-committed
                                 :original-prior-revision (:revision prior-floor)
                                 :original-committed-revision (:revision committed-floor)
                                 :observation (:observation observed)
                                 :reconciliation result
                                 :signed-register-exact-head-match?
                                 (and (= :fresh-signed-read-and-local-signed-chain
                                         (:observation observed))
                                      (= committed-floor local remote))
                                 :quarantine :remains-permanent
                                 :next-step :new-adapter-required-with-fresh-signed-exact-head
                                 :automatic-cas-retry? false
                                 :write-or-repair-performed? false
                                 :persistent-audit-record :not-written
                                 :remote-execution-causality :not-established
                                 :external-register-independence :not-established
                                 :proof-discharge :none
                                 :certificate :not-issued
                                 :authority :none}))) ]
    ;; Verify the external read and local head immediately, not merely on the
    ;; first status call; fail closed on split-brain during restart.
    (checked-local!)
    {:type adapter-type :version 1 :status! status! :append! append!
     :inspect-quarantined-recovery! inspect-recovery!
     :configured-external-key-fingerprint external-fingerprint
     :external-signature-freshness :nonce-bound-on-every-read
     :external-register-independence :not-established
     :external-register-monotonicity :assumed-not-verified
     :external-key-provisioning :process-configured-not-authenticated
     :durable-revocation-across-deployment :not-established
     :proof-discharge :none :certificate :not-issued :authority :none}))
