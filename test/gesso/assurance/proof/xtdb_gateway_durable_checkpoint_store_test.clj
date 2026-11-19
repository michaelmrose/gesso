(ns gesso.assurance.proof.xtdb-gateway-durable-checkpoint-store-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.xtdb-gateway-durable-checkpoint-store :as store]
            [gesso.assurance.proof.xtdb-gateway-signed-policy-checkpoint :as signed]
            [gesso.assurance.proof.xtdb-gateway-local-key-lifecycle :as lifecycle]
            [gesso.assurance.proof.xtdb-gateway-atomic-write-intent :as intent]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.proof.xtdb-restricted-submission-gateway-test])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files LinkOption Path]
           [java.security KeyPairGenerator Signature]
           [java.time Instant]
           [java.util Base64 UUID]))

(defn- pair [] (.generateKeyPair (KeyPairGenerator/getInstance "Ed25519")))
(defn- spki [kp]
  (.encodeToString (Base64/getEncoder) (.getEncoded (.getPublic kp))))
(defn- sign [^bytes raw kp]
  (let [s (Signature/getInstance "Ed25519")]
    (.initSign s (.getPrivate kp))
    (.update s raw)
    (.encodeToString (Base64/getEncoder) (.sign s))))
(defn- reason [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

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
        signing! (requiring-resolve
                  'gesso.assurance.proof.xtdb-gateway-delegated-candidate-key/delegation-signing-bytes)]
    {:root-ed25519-spki-base64 (spki root)
     :delegation g
     :root-signature-base64 (sign (signing! g) root)}))

(defn- fixture []
  (let [root (pair) a (pair) b (pair)]
    {:root root :delegate a
     :policy (lifecycle/prepare-process-local-root-policy!
              (delegation root a))
     :replacement (delegation root b)
     :authority (pair)}))

(defn- checkpoint [policy previous authority]
  (let [c {:type signed/checkpoint-type
           :version 1
           :previous-digest previous
           :snapshot ((:checkpoint-material! policy))}]
    {:checkpoint c
     :signature-base64 (sign (signed/checkpoint-signing-bytes c) authority)}))

(defn- floor-at [revision digest]
  {:revision revision :checkpoint-digest digest})

(defn- open-store [dir {:keys [policy authority]} floor initialize?]
  (store/prepare-local-durable-store!
   {:directory dir :policy policy
    :authority-ed25519-spki-base64 (spki authority)
    :external-rollback-floor floor
    :initialize-empty? initialize?}))

(defn- with-temporary-directory [f]
  (let [^Path dir (Files/createTempDirectory
                   "gesso-durable-policy-checkpoint-"
                   (make-array java.nio.file.attribute.FileAttribute 0))]
    (try (f dir)
         (finally
           (doseq [file (reverse (file-seq (.toFile dir)))]
             (Files/deleteIfExists (.toPath file)))))))

(deftest v1902-durable-signed-chain-can-be-reopened-with-caller-floor
  (with-temporary-directory
    (fn [dir]
      (let [{:keys [policy authority replacement] :as fixture} (fixture)
            genesis (floor-at 0 "genesis")
            s (open-store dir fixture genesis true)
            initial ((:status! s))
            e1 (checkpoint policy "genesis" authority)
            h1 ((:append! s) e1)
            reopened (open-store dir fixture genesis false)]
        (is (= 0 (:head-revision initial)))
        (is (= 1 (:head-revision h1)))
        (is (= (:head-digest h1) (:head-digest ((:status! reopened)))))
        (is (= 1 (:signed-checkpoint-count ((:status! reopened)))))
        (is (= :file-and-directory-forced (:successful-append-durability h1)))
        (is (= :caller-supplied-not-authenticated
               (:rollback-floor-source ((:status! reopened)))))
        (is (= :not-established (:trusted-storage s)
               (:durable-cross-process-revocation s)))
        (is (= :process-configured-not-authenticated
               (:signing-key-provisioning s)))
        (is (= :none (:authority h1) (:proof-discharge h1)))
        (is (= :not-issued (:certificate h1)))
        ((:revoke-delegation! policy) 1)
        (let [e2 (checkpoint policy (:head-digest h1) authority)
              h2 ((:append! reopened) e2)
              floor-one (floor-at 1 (:head-digest h1))
              pinned (open-store dir fixture floor-one false)]
          (is (= 2 (:head-revision h2)))
          (is (= 2 (:signed-checkpoint-count ((:status! pinned)))))
          ((:replace-delegation! policy) 2 replacement)
          (let [e3 (checkpoint policy (:head-digest h2) authority)
                h3 ((:append! pinned) e3)
                expected (floor-at 2 (:head-digest h2))]
            (is (= 3 (:head-revision h3)))
            (is (= 3 (:signed-checkpoint-count ((:status! s)))))
            (is (= 3 (:head-revision ((:status! (open-store dir fixture expected false))))))
            (is (= :external-rollback-floor-not-in-persisted-history
                   (reason #(open-store dir fixture
                                        (floor-at 2 (:head-digest h1)) false))))
            (is (= :checkpoint-rollback-gap-or-wrong-anchor
                   (reason #((:append! s) e3))))))))))

(deftest v1902-rejects-missing-corrupt-truncated-or-symlinked-store
  (with-temporary-directory
    (fn [dir]
      (let [{:keys [policy authority] :as fixture} (fixture)
            genesis (floor-at 0 "genesis")
            history (.resolve ^Path dir "policy-checkpoints.edn")]
        (is (= :explicit-genesis-initialization-required
               (reason #(open-store dir fixture genesis false))))
        (is (= :explicit-genesis-initialization-required
               (reason #(open-store dir fixture (floor-at 1
                  (str "sha256:" (apply str (repeat 64 "1")))) true))))
        (let [s (open-store dir fixture genesis true)
              e1 (checkpoint policy "genesis" authority)
              h1 ((:append! s) e1)
              floor-one (floor-at 1 (:head-digest h1))]
          (Files/write history (.getBytes "{:oops" StandardCharsets/UTF_8)
                       (make-array java.nio.file.OpenOption 0))
          (is (= :malformed-persisted-history-file
                 (reason #((:status! s)))))
          (is (= :malformed-persisted-history-file
                 (reason #(open-store dir fixture floor-one false))))
          ;; Recreate from an explicitly supplied genesis floor only. This
          ;; is not evidence a rolled-back history was never present.
          (Files/delete history)
          (is (= :explicit-genesis-initialization-required
                 (reason #(open-store dir fixture floor-one false))))
          (let [link (.resolve ^Path dir "elsewhere")]
            (Files/write link (.getBytes "x" StandardCharsets/UTF_8)
                         (make-array java.nio.file.OpenOption 0))
            (Files/createSymbolicLink history link (make-array java.nio.file.attribute.FileAttribute 0))
            (is (= :symbolic-link-not-accepted
                   (reason #((:status! s)))))))))))

(deftest v1902-signed-but-stale-or-changed-data-cannot-append
  (with-temporary-directory
    (fn [dir]
      (let [{:keys [policy authority] :as fixture} (fixture)
            other (pair)
            s (open-store dir fixture (floor-at 0 "genesis") true)
            e1 (checkpoint policy "genesis" authority)
            h1 ((:append! s) e1)
            source-contents (slurp (.toFile (.resolve ^Path dir "policy-checkpoints.edn")))]
        (is (= :checkpoint-rollback-gap-or-wrong-anchor
               (reason #((:append! s) e1))))
        (is (= :checkpoint-rollback-gap-or-wrong-anchor
               (reason #((:append! s) (checkpoint policy "genesis" authority)))))
        ((:revoke-delegation! policy) 1)
        (let [e2 (checkpoint policy (:head-digest h1) authority)
              forged (assoc e2 :signature-base64
                            (sign (signed/checkpoint-signing-bytes
                                   (:checkpoint e2)) other))]
          (is (= :persisted-signature-not-verified
                 (reason #((:append! s) forged))))
          (is (= source-contents
                 (slurp (.toFile (.resolve ^Path dir "policy-checkpoints.edn")))))
          (let [forged-checkpoint
                (assoc-in (:checkpoint e2) [:snapshot :policy-id]
                          (str (UUID/randomUUID)))
                falsely-signed {:checkpoint forged-checkpoint
                                :signature-base64
                                (sign (signed/checkpoint-signing-bytes
                                       forged-checkpoint) authority)}]
            (is (= :checkpoint-does-not-match-current-local-policy
                   (reason #((:append! s) falsely-signed)))))
          (is (= 2 (:head-revision ((:append! s) e2))))
          (is (= :checkpoint-rollback-gap-or-wrong-anchor
                 (reason #((:append! s) e2)))))))))

(deftest v1902-fail-closed-on-persisted-truncation-even-with-valid-old-signature
  (with-temporary-directory
    (fn [dir]
      (let [{:keys [policy authority] :as fixture} (fixture)
            genesis (floor-at 0 "genesis")
            s (open-store dir fixture genesis true)
            e1 (checkpoint policy "genesis" authority)
            h1 ((:append! s) e1)]
        ((:revoke-delegation! policy) 1)
        (let [e2 (checkpoint policy (:head-digest h1) authority)
              h2 ((:append! s) e2)
              floor-two (floor-at 2 (:head-digest h2))
              file (.resolve ^Path dir "policy-checkpoints.edn")
              document (edn/read-string (slurp (.toFile file)))]
          ;; Remove the signed second revision and its retirement history,
          ;; preserving the first entry's exact valid signature.
          (spit (.toFile file) (pr-str (assoc document :entries [e1])))
          (is (= :external-rollback-floor-not-in-persisted-history
                 (reason #(open-store dir fixture floor-two false))))
          ;; A caller can provide an old genesis floor: the valid signed
          ;; prefix is accepted. Hence this is NOT independently anchored
          ;; rollback protection against somebody replacing the file.
          (is (= 1 (:head-revision
                    ((:status! (open-store dir fixture genesis false))))))
          (is (= :not-established
                 (:durable-cross-process-revocation s))))))))

(deftest v1902-rejects-tampered-signed-history-and-wrong-configured-key
  (with-temporary-directory
    (fn [dir]
      (let [{:keys [policy authority] :as fixture} (fixture)
            genesis (floor-at 0 "genesis")
            s (open-store dir fixture genesis true)
            e1 (checkpoint policy "genesis" authority)
            h1 ((:append! s) e1)
            one (floor-at 1 (:head-digest h1))
            file (.resolve ^Path dir "policy-checkpoints.edn")
            original (slurp (.toFile file))
            wrong-authority (assoc fixture :authority (pair))]
        (is (= :persisted-signature-not-verified
               (reason #(open-store dir wrong-authority one false))))
        (is (= :invalid-caller-provided-rollback-floor
               (reason #(open-store dir fixture
                                    (floor-at 1 "sha256:NOT-HEX") false))))
        (let [document (edn/read-string original)
              changed (assoc-in document [:entries 0 :checkpoint :snapshot
                                          :delegation-id]
                                (str (UUID/randomUUID)))]
          (spit (.toFile file) (pr-str changed))
          (is (= :persisted-signature-not-verified
                 (reason #((:status! s)))))
          (is (= :persisted-signature-not-verified
                 (reason #(open-store dir fixture one false)))))
        ;; A staged partial record is never considered committed history.
        ;; This simulates a leftover temp file, NOT an actual power cut.
        (spit (.toFile file) (str original "\n{:trailing :uncommitted}"))
        (is (= :malformed-persisted-history-file
               (reason #((:status! s)))))
        (spit (.toFile file) original)
        (spit (.toFile (.resolve ^Path dir ".checkpoint-staging-leftover.tmp"))
              "{:partial")
        (is (= 1 (:head-revision ((:status! s)))))
        (is (= 1 (:head-revision
                  ((:status! (open-store dir fixture one false))))))))))

(deftest v1902-checked-local-file-does-not-prove-real-xtdb-causality
  (physical/with-proving-application!
   (fn [app]
     (with-temporary-directory
       (fn [dir]
         (let [{:keys [policy authority] :as fixture} (fixture)
               src (var-get
                    (ns-resolve
                     'gesso.assurance.proof.xtdb-restricted-submission-gateway-test
                     'source-for))
               before ((:read! app))
               gateway (intent/prepare-one-shot! (:node app)
                                                 (src [before]))
               returned ((:submit! gateway))
               original (intent/inspect-pinned-intent! gateway returned)
               s (open-store dir fixture (floor-at 0 "genesis") true)
               signed (checkpoint policy "genesis" authority)
               stored ((:append! s) signed)]
           (is (= :bounded-private-before-after-intent-ledger-coobservation
                  (:protected-before-after-intent-ledger-correlation original)))
           (is (= 7 (get-in original
                            [:correspondence-premises :assurance-frontier
                             :open-count])))
           (is (= 1 (:head-revision stored)))
           (is (= :not-established (:trusted-storage s)
                  (:durable-cross-process-revocation s)))
           (is (= :none (:transaction-authority original)
                  (:proof-discharge stored) (:authority stored)))
           (is (= :not-issued (:certificate stored)))))))))
