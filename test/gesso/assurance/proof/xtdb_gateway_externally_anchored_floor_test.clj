(ns gesso.assurance.proof.xtdb-gateway-externally-anchored-floor-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.xtdb-gateway-externally-anchored-floor :as floor]
            [gesso.assurance.proof.xtdb-gateway-durable-checkpoint-store :as store]
            [gesso.assurance.proof.xtdb-gateway-signed-policy-checkpoint :as signed]
            [gesso.assurance.proof.xtdb-gateway-local-key-lifecycle :as lifecycle]
            [gesso.assurance.proof.xtdb-gateway-atomic-write-intent :as intent]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.proof.xtdb-restricted-submission-gateway-test])
  (:import [java.nio.file Files Path]
           [java.security KeyPairGenerator Signature MessageDigest]
           [java.time Instant]
           [java.util Base64 UUID]))

(defn- pair [] (.generateKeyPair (KeyPairGenerator/getInstance "Ed25519")))
(defn- spki [p] (.encodeToString (Base64/getEncoder) (.getEncoded (.getPublic p))))
(defn- sign [^bytes b p]
  (let [s (Signature/getInstance "Ed25519")]
    (.initSign s (.getPrivate p))
    (.update s b)
    (.encodeToString (Base64/getEncoder) (.sign s))))
(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))
(defn- delegation [root delegate]
  (let [now (Instant/now)
        g {:version 1
           :delegation-id (str (UUID/randomUUID))
           :audience "gesso.xtdb.transaction-causality-candidate.v1"
           :obligation :exclusive-transaction-causality
           :witness-family :transaction-coupled-apply-provenance
           :delegated-ed25519-spki-base64 (spki delegate)
           :valid-from (str (.minusSeconds now 60))
           :valid-until (str (.plusSeconds now 3600))}
        sign-grant (requiring-resolve
                    'gesso.assurance.proof.xtdb-gateway-delegated-candidate-key/delegation-signing-bytes)]
    {:root-ed25519-spki-base64 (spki root)
     :delegation g
     :root-signature-base64 (sign (sign-grant g) root)}))
(defn- policy [] (let [root (pair) delegate (pair)]
                   (lifecycle/prepare-process-local-root-policy!
                    (delegation root delegate))))
(defn- checkpoint [p previous signer]
  (let [data {:type signed/checkpoint-type
              :version 1
              :previous-digest previous
              :snapshot ((:checkpoint-material! p))}]
    {:checkpoint data
     :signature-base64 (sign (signed/checkpoint-signing-bytes data) signer)}))
(defn- with-directory [f]
  (let [^Path path (Files/createTempDirectory
                    "gesso-external-floor-"
                    (make-array java.nio.file.attribute.FileAttribute 0))]
    (try (f path)
         (finally
           (doseq [entry (reverse (file-seq (.toFile path)))]
             (Files/deleteIfExists (.toPath entry)))))))
(defn- signed-response [statement signer]
  {:statement statement
   :signature-base64 (sign (floor/floor-signing-bytes statement) signer)})
(defn- external-register [signer]
  (let [current (atom {:revision 0 :checkpoint-digest "genesis"})
        fail-once (atom nil)
        last-read (atom nil)
        read! (fn [{:keys [kind nonce] :as req}]
                (when-not (and (= :read kind) (= #{:kind :nonce} (set (keys req))))
                  (throw (ex-info "unexpected request" {})))
                (if (= :reuse-old-read @fail-once)
                  (do (reset! fail-once nil) @last-read)
                  (let [response (signed-response
                                  {:type floor/statement-type :version 1
                                   :kind :read :nonce nonce :floor @current} signer)]
                    (reset! last-read response)
                    response)))
        advance! (fn [{:keys [kind expected-floor next-floor] :as statement}]
                   (when-not (= :advance kind)
                     (throw (ex-info "not an advance" {})))
                   (let [failure @fail-once]
                     (reset! fail-once nil)
                     (when (= :reject-before-cas failure)
                       (throw (ex-info "register unavailable" {})))
                     (when-not (compare-and-set! current expected-floor next-floor)
                       (throw (ex-info "compare-and-swap rejected" {})))
                     (when (= :lost-receipt-after-cas failure)
                       (throw (ex-info "receipt lost after commit" {})))
                     (signed-response statement signer)))]
    {:current current :fail-once fail-once :read! read! :advance! advance!}))
(defn- config [dir p checkpoint-key external-key external initialize?]
  {:directory dir :policy p
   :checkpoint-authority-ed25519-spki-base64 (spki checkpoint-key)
   :external-floor-ed25519-spki-base64 (spki external-key)
   :read-external-floor! (:read! external)
   :advance-external-floor! (:advance! external)
   :initialize-empty? initialize?})
(defn- open [dir p cp ep ext initialize?]
  (floor/prepare-external-floor-adapter! (config dir p cp ep ext initialize?)))

(deftest v1903-canonical-external-attestation-is-purpose-and-nonce-bound
  (let [nonce (str (UUID/randomUUID))
        genesis {:revision 0 :checkpoint-digest "genesis"}
        next-floor {:revision 1 :checkpoint-digest
                    (str "sha256:" (apply str (repeat 64 "a")))}
        rd {:type floor/statement-type :version 1
            :kind :read :nonce nonce :floor genesis}
        adv {:type floor/statement-type :version 1
             :kind :advance :nonce nonce :expected-floor genesis
             :next-floor next-floor :accepted? true}]
    (is (pos? (alength ^bytes (floor/floor-signing-bytes rd))))
    (is (not= (seq (floor/floor-signing-bytes rd))
              (seq (floor/floor-signing-bytes adv))))
    (doseq [bad [(assoc rd :authority :claimed)
                 (assoc rd :nonce "replayed")
                 (assoc rd :floor {:revision 0 :checkpoint-digest "other"})
                 (assoc adv :next-floor genesis)
                 (assoc adv :accepted? false)
                 (assoc adv :kind :read)
                 (assoc adv :expected-floor next-floor)
                 (assoc adv :unexpected true)]]
      (is (= :invalid-floor-statement
             (reason #(floor/floor-signing-bytes bad)))))))

(deftest v1903-external-advance-persists-across-new-local-adapters
  (with-directory
    (fn [dir]
      (let [p (policy) cp (pair) ep (pair) ext (external-register ep)
            a (open dir p cp ep ext true)
            genesis ((:status! a))
            h1 ((:append! a) (checkpoint p "genesis" cp))
            b (open dir p cp ep ext false)]
        (is (= 0 (:head-revision genesis)))
        (is (= :verified-for-fresh-random-nonce
               (:signed-external-floor genesis)))
        (is (= 1 (:head-revision h1) (:head-revision ((:status! b)))))
        (is (= :signed-compare-and-advance-confirmed
               (:external-advance h1)))
        (is (= :freshly-signed-and-matches-local-head
               (:post-advance-read h1)))
        (is (= (:head-digest h1) (:checkpoint-digest @(:current ext))))
        (is (= :not-established (:cross-resource-atomicity h1)
               (:external-register-independence h1)
               (:external-register-independence b)))
        (is (= :assumed-not-verified (:external-register-monotonicity b)))
        (is (= :process-configured-not-authenticated
               (:external-key-provisioning b)))
        (is (= :not-issued (:certificate h1)))
        (is (= :none (:authority h1) (:proof-discharge h1)))
        ((:revoke-delegation! p) 1)
        (let [h2 ((:append! b) (checkpoint p (:head-digest h1) cp))]
          (is (= 2 (:head-revision h2)))
          (is (= 2 (:head-revision ((:status! (open dir p cp ep ext false))))))
          (is (= :not-issued (:certificate h2)))
          ;; Restore the exact valid first signed checkpoint, without
          ;; rolling the external register back. Reopen must fail closed.
          (let [file (.resolve ^Path dir "policy-checkpoints.edn")
                data (edn/read-string (slurp (.toFile file)))]
            (spit (.toFile file) (pr-str (assoc data :entries
                                               [(first (:entries data))])))
            (is (#{:external-rollback-floor-not-in-persisted-history
                   :external-floor-local-head-divergence}
                 (reason #(open dir p cp ep ext false))))))))))

(deftest v1903-replay-wrong-signing-key-and-wrong-register-head-are-rejected
  (with-directory
    (fn [dir]
      (let [p (policy) cp (pair) ep (pair) ext (external-register ep)
            cfg (config dir p cp ep ext true)]
        (is (= :external-floor-key-must-differ-from-checkpoint-key
               (reason #(floor/prepare-external-floor-adapter!
                         (assoc cfg :external-floor-ed25519-spki-base64
                                (spki cp))))))
        (is (= :external-floor-signature-not-verified
               (reason #(floor/prepare-external-floor-adapter!
                         (assoc cfg :external-floor-ed25519-spki-base64
                                (spki (pair)))))))
        (let [a (floor/prepare-external-floor-adapter! cfg)]
          (is (= 0 (:head-revision ((:status! a)))))
          (reset! (:fail-once ext) :reuse-old-read)
          (is (= :external-floor-response-not-request-bound
                 (reason #((:status! a)))))
          (is (= 0 (:head-revision ((:status! a)))))
          (reset! (:current ext) {:revision 1 :checkpoint-digest
                                  (str "sha256:" (apply str (repeat 64 "b")))})
          (is (#{:expected-checkpoint-history-missing
                 :explicit-genesis-initialization-required
                 :external-rollback-floor-not-in-persisted-history}
               (reason #((:status! a))))))))))

(deftest v1903-failed-external-cas-quarantines-after-local-commit
  (with-directory
    (fn [dir]
      (let [p (policy) cp (pair) ep (pair) ext (external-register ep)
            a (open dir p cp ep ext true)
            first-signed (checkpoint p "genesis" cp)]
        (reset! (:fail-once ext) :reject-before-cas)
        (is (= :external-floor-advance-unconfirmed
               (reason #((:append! a) first-signed))))
        (is (= :unconfirmed-external-floor-advance
               (reason #((:status! a)))))
        (is (= {:revision 0 :checkpoint-digest "genesis"}
               @(:current ext)))
        ;; This valid signed local write has reached disk already; a new
        ;; adapter MUST NOT reopen against the stale external floor.
        (is (= :external-floor-local-head-divergence
               (reason #(open dir p cp ep ext false))))
        ;; Explicit operator repair, simulated only. The external register
        ;; must be aligned to the signed local head before a fresh adapter.
        (let [local (store/prepare-local-durable-store!
                     {:directory dir :policy p
                      :authority-ed25519-spki-base64 (spki cp)
                      :external-rollback-floor {:revision 0
                                                :checkpoint-digest "genesis"}
                      :initialize-empty? false})
              current ((:status! local))]
          (reset! (:current ext) {:revision (:head-revision current)
                                  :checkpoint-digest (:head-digest current)})
          (is (= 1 (:head-revision
                    ((:status! (open dir p cp ep ext false))))))
          (is (= :unconfirmed-external-floor-advance
                 (reason #((:status! a))))))))))

(deftest v1903-lost-receipt-reopens-only-when-both-sides-agree
  (with-directory
    (fn [dir]
      (let [p (policy) cp (pair) ep (pair) ext (external-register ep)
            a (open dir p cp ep ext true)]
        (reset! (:fail-once ext) :lost-receipt-after-cas)
        (is (= :external-floor-advance-unconfirmed
               (reason #((:append! a) (checkpoint p "genesis" cp)))))
        (is (= :unconfirmed-external-floor-advance
               (reason #((:status! a)))))
        (is (= 1 (:revision @(:current ext))))
        (let [reopened (open dir p cp ep ext false)]
          (is (= 1 (:head-revision ((:status! reopened)))))
          (is (= :not-established (:durable-revocation-across-deployment reopened)))
          (is (= :none (:proof-discharge reopened) (:authority reopened))))))))

(deftest v1903-unsigned-local-register-cannot-claim-physical-xtdb-trust
  (physical/with-proving-application!
   (fn [app]
     (with-directory
       (fn [dir]
         (let [p (policy) cp (pair) ep (pair) ext (external-register ep)
               source-fn (var-get
                          (ns-resolve
                           'gesso.assurance.proof.xtdb-restricted-submission-gateway-test
                           'source-for))
               gateway (intent/prepare-one-shot!
                        (:node app) (source-fn [((:read! app))]))
               original-return ((:submit! gateway))
               report (intent/inspect-pinned-intent! gateway original-return)
               a (open dir p cp ep ext true)
               local-signed (checkpoint p "genesis" cp)
               checked ((:append! a) local-signed)]
           (is (= :bounded-private-before-after-intent-ledger-coobservation
                  (:protected-before-after-intent-ledger-correlation report)))
           (is (= 7 (get-in report
                            [:correspondence-premises :assurance-frontier
                             :open-count])))
           (is (= 1 (:head-revision checked)))
           (is (= :none (:proof-discharge checked)
                  (:transaction-authority report) (:authority checked)))
           (is (= :not-issued (:certificate checked)))
           (is (= :not-established (:external-register-independence a)
                  (:durable-revocation-across-deployment a)))))))))

(deftest v1903-incorrect-signed-advance-receipt-quarantines-without-false-success
  (with-directory
    (fn [dir]
      (let [p (policy) cp (pair) ep (pair) ext (external-register ep)
            original-advance (:advance! ext)
            bad-advance (fn [request]
                          ;; The register really does move, but the signed
                          ;; response is bound to another request nonce.
                          (original-advance request)
                          (signed-response
                           (assoc request :nonce (str (UUID/randomUUID))) ep))
            cfg (assoc (config dir p cp ep ext true)
                       :advance-external-floor! bad-advance)
            a (floor/prepare-external-floor-adapter! cfg)]
        (is (= :external-floor-advance-unconfirmed
               (reason #((:append! a) (checkpoint p "genesis" cp)))))
        (is (= :unconfirmed-external-floor-advance
               (reason #((:status! a)))))
        (is (= 1 (:revision @(:current ext))))
        ;; Once a separate process reopens, both signed heads agree and
        ;; the original unknown acknowledgement need not be replayed.
        (let [reopened (open dir p cp ep ext false)]
          (is (= 1 (:head-revision ((:status! reopened)))))
          (is (= :not-established (:external-register-independence reopened)
                 (:durable-revocation-across-deployment reopened)))
          (is (= :not-issued (:certificate reopened)))
          (is (= :none (:authority reopened))))))))
