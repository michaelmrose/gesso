(ns gesso.assurance.proof.xtdb-gateway-recovery-audit-store-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.proof.xtdb-gateway-recovery-audit-store :as audit]
            [gesso.assurance.proof.xtdb-gateway-externally-anchored-floor :as floor]
            [gesso.assurance.proof.xtdb-gateway-local-key-lifecycle :as lifecycle]
            [gesso.assurance.proof.xtdb-gateway-signed-policy-checkpoint :as signed]
            [gesso.assurance.proof.xtdb-gateway-durable-checkpoint-store :as durable]
            [gesso.assurance.proof.xtdb-gateway-atomic-write-intent :as intent]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.proof.xtdb-restricted-submission-gateway-test])
  (:import [java.nio.file Files Path]
           [java.security KeyPairGenerator Signature]
           [java.time Instant]
           [java.util Base64 UUID]))

(defn- pair [] (.generateKeyPair (KeyPairGenerator/getInstance "Ed25519")))
(defn- spki [p] (.encodeToString (Base64/getEncoder) (.getEncoded (.getPublic p))))
(defn- sign [^bytes b p]
  (let [s (Signature/getInstance "Ed25519")]
    (.initSign s (.getPrivate p)) (.update s b)
    (.encodeToString (Base64/getEncoder) (.sign s))))
(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
(defn- policy []
  (let [root (pair) delegate (pair) now (Instant/now)
        grant {:version 1 :delegation-id (str (UUID/randomUUID))
               :audience "gesso.xtdb.transaction-causality-candidate.v1"
               :obligation :exclusive-transaction-causality
               :witness-family :transaction-coupled-apply-provenance
               :delegated-ed25519-spki-base64 (spki delegate)
               :valid-from (str (.minusSeconds now 60))
               :valid-until (str (.plusSeconds now 3600))}]
    (lifecycle/prepare-process-local-root-policy!
     {:root-ed25519-spki-base64 (spki root)
      :delegation grant
      :root-signature-base64
      (sign ((requiring-resolve
              'gesso.assurance.proof.xtdb-gateway-delegated-candidate-key/delegation-signing-bytes)
             grant) root)})))
(defn- checkpoint [p previous signer]
  (let [body {:type signed/checkpoint-type :version 1
              :previous-digest previous
              :snapshot ((:checkpoint-material! p))}]
    {:checkpoint body :signature-base64
     (sign (signed/checkpoint-signing-bytes body) signer)}))
(defn- signed-response [statement key]
  {:statement statement
   :signature-base64 (sign (floor/floor-signing-bytes statement) key)})
(defn- register [key]
  (let [value (atom {:revision 0 :checkpoint-digest "genesis"})
        fail? (atom false)]
    {:head value :fail? fail?
     :read! (fn [{:keys [kind nonce]}]
              (when-not (= :read kind) (throw (ex-info "invalid read" {})))
              (signed-response {:type floor/statement-type :version 1
                                :kind :read :nonce nonce :floor @value} key))
     :advance! (fn [{:keys [expected-floor next-floor] :as statement}]
                 (if @fail?
                   (do (reset! fail? false)
                       (throw (ex-info "lost response" {})))
                   (if (compare-and-set! value expected-floor next-floor)
                     (signed-response statement key)
                     (throw (ex-info "stale" {})))))}))
(defn- adapter [dir p cp ep reg initialize?]
  (floor/prepare-external-floor-adapter!
   {:directory dir :policy p
    :checkpoint-authority-ed25519-spki-base64 (spki cp)
    :external-floor-ed25519-spki-base64 (spki ep)
    :read-external-floor! (:read! reg)
    :advance-external-floor! (:advance! reg)
    :initialize-empty? initialize?}))
(def genesis {:sequence 0 :digest "genesis"})
(defn- journal [dir a floor initialize?]
  (audit/prepare-recovery-audit-journal!
   {:directory dir :adapter a :audit-floor floor
    :initialize-empty? initialize?}))
(defn- with-directory [f]
  (let [^Path p (Files/createTempDirectory "gesso-recovery-audit-"
                                           (make-array java.nio.file.attribute.FileAttribute 0))]
    (try (f p)
         (finally
           (doseq [entry (reverse (file-seq (.toFile p)))]
             (Files/deleteIfExists (.toPath entry)))))))

(deftest v1910-lost-receipt-produces-durable-read-only-recovery-audit
  (with-directory
    (fn [dir]
      (let [p (policy) cp (pair) ep (pair) ext (register ep)
            a (adapter dir p cp ep ext true)
            j (journal dir a genesis true)]
        (is (= :recovery-inspection-requires-quarantined-attempt
               (reason #((:record! j)))))
        (reset! (:fail? ext) true)
        (is (= :external-floor-advance-unconfirmed
               (reason #((:append! a) (checkpoint p "genesis" cp)))))
        (let [first-result ((:record! j))
              first-report (first ((:records! j)))
              first-status ((:status! j))]
          (is (= :fresh-signed-register-still-at-prior-head
                 (:assessment-reconciliation first-result)
                 (:reconciliation first-report)))
          (is (= :unconfirmed-outcome
                 (:original-advance-classification first-report)))
          (is (= (:attempt-id first-report) (:attempt-id first-result)
                 (:attempt-id first-status)))
          (is (= 1 (:head-sequence first-status) (:record-count first-status)))
          (is (= (:head-digest first-status) (:digest first-result)))
          (is (= :not-established (:record-authenticity first-status)
                 (:audit-completeness first-status)))
          (is (= :none (:authority first-result) (:proof-discharge first-result)))
          ;; A second assessment may observe later operator action, but
          ;; neither the assessment nor journal repairs or writes the floor.
          (let [local (durable/prepare-local-durable-store!
                       {:directory dir :policy p
                        :authority-ed25519-spki-base64 (spki cp)
                        :external-rollback-floor {:revision 0
                                                  :checkpoint-digest "genesis"}
                        :initialize-empty? false})
                head ((:status! local))]
            (reset! (:head ext) {:revision (:head-revision head)
                                 :checkpoint-digest (:head-digest head)}))
          (let [second-result ((:record! j))
                second-report (second ((:records! j)))
                reopened (journal dir a {:sequence 1
                                         :digest (:digest first-result)} false)]
            (is (= 2 (:sequence second-result)
                   (:head-sequence ((:status! reopened)))))
            (is (= :fresh-signed-head-matches-committed-local-head
                   (:reconciliation second-report)))
            (is (true? (:signed-register-exact-head-match? second-report)))
            (is (= 2 (count ((:records! reopened)))))
            (is (= :remains-permanent (:quarantine second-result)))
            (is (= :unconfirmed-external-floor-advance
                   (reason #((:status! a)))))
            (is (= :none (:authority reopened)
                   (:proof-discharge reopened)))
            (is (= :not-issued (:certificate reopened)))
            (is (not-any? #(contains? second-report %)
                          [:node :xtdb-key :private-key :original-document
                           :snapshot-token :signed-envelope :checkpoint-digest]))))))))

(deftest v1910-rejects-corrupt-history-symlink-and-supplied-rollback
  (with-directory
    (fn [dir]
      (let [p (policy) cp (pair) ep (pair) ext (register ep)
            a (adapter dir p cp ep ext true)
            j (journal dir a genesis true)
            file (.resolve ^Path dir "recovery-assessments.edn")]
        (reset! (:fail? ext) true)
        (is (= :external-floor-advance-unconfirmed
               (reason #((:append! a) (checkpoint p "genesis" cp)))))
        (let [row ((:record! j))
              document (edn/read-string (slurp (.toFile file)))
              anchored {:sequence 1 :digest (:digest row)}]
          (is (= 1 (:head-sequence ((:status! (journal dir a anchored false))))))
          (is (= :audit-rollback-floor-missing
                 (reason #(journal dir a
                                   {:sequence 1
                                    :digest (str "sha256:" (apply str (repeat 64 "f")))}
                                   false))))
          (spit (.toFile file) (str (pr-str document) "\n:trailing"))
          (is (= :malformed-audit-file (reason #((:status! j)))))
          (spit (.toFile file) (pr-str (assoc-in document
                                                    [:entries 0 :entry :assessment
                                                     :authority] :write)))
          (is (= :invalid-audit-hash-chain-or-assessment
                 (reason #((:status! j)))))
          ;; Restoring a valid old prefix with caller-supplied GENESIS still
          ;; works; this is intentionally NOT an authenticated rollback root.
          (spit (.toFile file) (pr-str (assoc document :entries [])))
          (is (= 0 (:head-sequence ((:status! j)))))
          (is (= :audit-rollback-floor-missing
                 (reason #(journal dir a anchored false))))
          (Files/delete file)
          (is (= :explicit-audit-initialization-required
                 (reason #(journal dir a genesis false))))
          (Files/createSymbolicLink file (.resolve ^Path dir "nonexistent")
                                    (make-array java.nio.file.attribute.FileAttribute 0))
          (is (= :symbolic-link-not-accepted
                 (reason #((:status! j))))))))))

(deftest v1910-audit-is-not-an-authenticated-producer-and-never-certifies
  (with-directory
    (fn [dir]
      ;; This is deliberately an entirely caller-constructed 'adapter'. Its
      ;; valid-looking reports can be journaled. The API never treats this as
      ;; independently authenticated witness evidence or writer coverage.
      (let [attempt (str (UUID/randomUUID))
            report {:type :gesso.assurance.proof.xtdb-gateway-externally-anchored-floor/recovery-assessment
                    :version 1 :attempt-id attempt
                    :original-advance-classification :unconfirmed-outcome
                    :original-local-checkpoint :already-committed
                    :original-prior-revision 0
                    :original-committed-revision 1
                    :observation :unable-to-observe-both-heads
                    :reconciliation :reconciliation-observation-unavailable
                    :signed-register-exact-head-match? false
                    :quarantine :remains-permanent
                    :next-step :new-adapter-required-with-fresh-signed-exact-head
                    :automatic-cas-retry? false
                    :write-or-repair-performed? false
                    :persistent-audit-record :not-written
                    :remote-execution-causality :not-established
                    :external-register-independence :not-established
                    :proof-discharge :none :certificate :not-issued
                    :authority :none}
            current (atom report)
            j (journal dir {:inspect-quarantined-recovery! (fn [] @current)}
                       genesis true)]
        (is (= :reconciliation-observation-unavailable
               (:assessment-reconciliation ((:record! j)))))
        (is (= :not-established (:audit-producer-independence j)
               (:physical-execution-causality j)
               (:complete-audit-event-ingestion j)))
        (is (= :not-supported (:quarantine-clearance j)))
        (is (= :none (:authority j) (:proof-discharge j)))
        (is (= :not-issued (:certificate j)))
        (reset! current (assoc report :authority :granted))
        (is (= :invalid-or-nonquarantined-assessment
               (reason #((:record! j)))))
        (reset! current (assoc report :attempt-id (str (UUID/randomUUID))))
        (is (= :audit-attempt-binding-mismatch
               (reason #((:record! j)))))
        (is (= 1 (:record-count ((:status! j)))))))))

(deftest v1910-audit-write-failure-remains-unknown-and-no-retry
  (with-directory
    (fn [dir]
      (let [p (policy) cp (pair) ep (pair) ext (register ep)
            a (adapter dir p cp ep ext true)
            j (journal dir a genesis true)]
        (reset! (:fail? ext) true)
        (is (= :external-floor-advance-unconfirmed
               (reason #((:append! a) (checkpoint p "genesis" cp)))))
        (let [target (ns-resolve
                      'gesso.assurance.proof.xtdb-gateway-recovery-audit-store
                      'atomic-write!)
              attempts (atom 0)
              result (with-redefs-fn
                       {target (fn [& _] (swap! attempts inc)
                                 (throw (java.io.IOException. "simulated lost fsync")))}
                       #(try ((:record! j)) nil
                             (catch clojure.lang.ExceptionInfo e (ex-data e))))]
          (is (= 1 @attempts))
          (is (= :audit-append-unconfirmed (:reason result)))
          (is (= :unknown (:audit-write-outcome result)))
          (is (true? (:no-automatic-retry result)))
          (is (= :none (:authority result) (:proof-discharge result)))
          (is (= 0 (:head-sequence ((:status! j)))))
          (is (= :unconfirmed-external-floor-advance
                 (reason #((:status! a))))))))))

(deftest v1910-real-xtdb-and-audit-stay-separate
  (physical/with-proving-application!
   (fn [app]
     (with-directory
       (fn [dir]
         (let [p (policy) cp (pair) ep (pair) ext (register ep)
               a (adapter dir p cp ep ext true)
               source-for (var-get
                           (ns-resolve
                            'gesso.assurance.proof.xtdb-restricted-submission-gateway-test
                            'source-for))
               g (intent/prepare-one-shot!
                  (:node app) (source-for [((:read! app))]))
               result ((:submit! g))
               observed (intent/inspect-pinned-intent! g result)
               j (journal dir a genesis true)]
           (is (= 7 (get-in observed
                            [:correspondence-premises :assurance-frontier :open-count])))
           (reset! (:fail? ext) true)
           (is (= :external-floor-advance-unconfirmed
                  (reason #((:append! a) (checkpoint p "genesis" cp)))))
           (is (= :fresh-signed-register-still-at-prior-head
                  (:assessment-reconciliation ((:record! j)))))
           (is (= 1 (:record-count ((:status! j)))))
           (is (= :none (:authority ((:status! j)))))
           (is (= :not-established (:remote-execution-causality
                                   (first ((:records! j))))))
           (is (= :not-issued (:certificate ((:status! j)))))))))))
