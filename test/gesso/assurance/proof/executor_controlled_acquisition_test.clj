(ns gesso.assurance.proof.executor-controlled-acquisition-test
  "v1383: Permanent adversarial checks for the v1382 executor-owned acquisition
   boundary. These tests use the real v1372 checked writer-source fixture and
   the real resolved-operation executor; only the physical tx call is stubbed.
   They establish bounded entry-point behavior, NOT authenticated provenance,
   complete mediation, physical commits, or universal refinement."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.executor-controlled-acquisition :as controlled]
            [gesso.assurance.proof.transaction-bound-evidence-acquisition :as acquisition]
            [gesso.assurance.proof.physical-execution-provenance-boundary-test :as fixture]
            [gesso.model.command :as command]
            [gesso.model.tx :as tx]
            [gesso.model.resolved-operation :as resolved]))

;; The v1372 fixture constructs an issued, source-checked application catalog.
;; It is expensive and deliberately shared once across this test namespace.
(use-fixtures :once
  (first (:clojure.test/once-fixtures
          (meta (the-ns 'gesso.assurance.proof.physical-execution-provenance-boundary-test)))))

(def ^:private hook-keys
  [resolved/observation-context-key
   resolved/construction-input-context-key])
(def ^:private expected-open
  #{:typed-input-binding
    :transaction-internal-prestate
    :guard-authorization
    :assignment-write-footprint
    :frame-and-unmodeled-fields
    :serial-concurrency-validation
    :committed-outcome-incorporation
    :writer-provenance-and-deployment
    :universal-execution-refinement})
(def ^:private callback-keys
  [:read-record! :may-disclose? :construct! :target-contract!
   :predicate-contract! :command-operation-contract!
   :post-state-contract! :outcome-contract!])

(defn- observation []
  (get-in fixture/*source* [:observed-source :transition-source :observation]))

(defn- source
  ([] (source (fn [_ _] nil)))
  ([saw!]
   (let [observed (observation)
         plan (:declared-plan observed)
         commands (filterv #(not= :gesso_commands (first (command/target %)))
                           (:commands plan))
         record (:persisted-command-record observed)]
     {:writer-source (:writer-source fixture/*source*)
      :operation-id :example/claim
      :binding (:command-binding observed)
      :configuration
      {:operation :example/claim
       :read-record! (fn [ctx _] (saw! :read ctx) nil)
       :may-disclose? (fn [ctx _ _] (saw! :disclose ctx) true)
       :construct! (fn [ctx _]
                     (saw! :construct ctx)
                     {:now (:gesso.command/created-at record)
                      :outcome (:gesso.command/outcome record)
                      :plan {:commands commands :emit false}})}})))

(defn- reason [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo error
         (:reason (ex-data error)))))

(defn- error-kind [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo error
         (:error/kind (ex-data error)))))

(defn- committed-tx! [calls]
  (fn [_ctx _plan]
    (swap! calls inc)
    {:commit/status :committed :tx-result {:tx-id @calls}}))

(defn- no-authority! [result]
  (is (= controlled/boundary-type (:type result)))
  (is (= controlled/boundary-version (:version result)))
  (is (= :one-invocation-no-automatic-retry (:attempt-scope result)))
  (is (= :controller-created-per-invocation (:injection-origin result)))
  (is (false? (:caller-supplied-observer-accepted? result)))
  (is (false? (:application-callbacks-received-observer? result)))
  (is (false? (:observer-origin-authenticated? result)))
  (is (false? (:physical-submission-authenticated? result)))
  (is (= :not-established (:issued-writer-instance-identified? result)))
  (is (= :not-established (:deployed-handler-identity result)))
  (is (= :not-established (:other-physical-writers-excluded? result)))
  (is (= :not-established (:transaction-internal-prestate result)))
  (is (= :not-established (:universal-execution-refinement result)))
  (is (= :executor-controlled-hook-diagnostic-only (:proof-status result)))
  (is (= :not-admitted (:admission result)))
  (is (= :not-issued (:certificate result)))
  (is (= :none (:transaction-authority result)))
  (is (= :none (:authority result)))
  (is (= expected-open
         (set (map :id (:physical-refinement-obligations result)))))
  (is (every? #(= :open (:status %)) (:physical-refinement-obligations result))))

(deftest successful-submit-hides-both-hooks-and-preserves-original-result
  (let [seen (atom []) calls (atom 0)
        src (source (fn [phase ctx]
                      (swap! seen conj [phase (:app/value ctx)])
                      (is (every? #(not (contains? ctx %)) hook-keys))))]
    (with-redefs [tx/transact! (committed-tx! calls)]
      (let [result (controlled/run-controlled-submit! src {:app/value :kept})]
        (is (= 1 @calls))
        (is (= [[:read :kept] [:construct :kept]] @seen))
        (is (= :incorporated (get-in result [:resolution :status])))
        (is (= :complete-paired-hook-observation (:capture-status result)))
        (is (= :committed (get-in result [:acquisition-report :captured-physical-status])))
        (is (true? (:executor-call-site-observed? result)))
        (is (uuid? (get-in result [:acquisition-report :attempt-id])))
        (no-authority! result)))))

(deftest distinct-submissions-own-distinct-observation-attempts
  (let [calls (atom 0) src (source)]
    (with-redefs [tx/transact! (committed-tx! calls)]
      (let [a (controlled/run-controlled-submit! src {})
            b (controlled/run-controlled-submit! src {})
            attempt-a (get-in a [:acquisition-report :attempt-id])
            attempt-b (get-in b [:acquisition-report :attempt-id])]
        (is (= 2 @calls))
        (is (uuid? attempt-a))
        (is (uuid? attempt-b))
        (is (not= attempt-a attempt-b))
        (is (= (get-in a [:acquisition-report :command-binding])
               (get-in b [:acquisition-report :command-binding])))
        (is (= :none (:authority a)))
        (is (= :none (:authority b)))))))

(deftest reported-transaction-identifier-is-never-authenticated
  (with-redefs [tx/transact! (fn [_ _] {:commit/status :committed
                                       :tx-result {:tx-id 999999}})]
    (let [result (controlled/run-controlled-submit! (source) {})]
      (is (= :committed (get-in result [:acquisition-report :captured-physical-status])))
      (is (false? (get-in result [:acquisition-report :transaction-id-authenticated?])))
      (is (= :not-established
             (get-in result [:acquisition-report :actual-submitted-payload-provenance])))
      (is (= :not-issued (:certificate result))))))

(deftest already-durable-command-does-not-submit-or-mint-evidence
  (let [calls (atom 0) seen (atom [])
        record (:persisted-command-record (observation))
        src (-> (source (fn [phase ctx]
                          (swap! seen conj phase)
                          (is (every? #(not (contains? ctx %)) hook-keys))))
                (assoc-in [:configuration :read-record!]
                          (fn [ctx _]
                            (swap! seen conj :read)
                            (is (every? #(not (contains? ctx %)) hook-keys))
                            record)))]
    (with-redefs [tx/transact! (fn [& _] (swap! calls inc)
                                (throw (ex-info "must not submit" {})))]
      (let [result (controlled/run-controlled-submit! src {})]
        (is (= 0 @calls))
        (is (= :no-complete-evidence (:capture-status result)))
        (is (= :incomplete-physical-attempt (:capture-reason result)))
        (is (nil? (:acquisition-report result)))
        (is (#{:replay :incorporated :incompatible :unavailable}
             (get-in result [:resolution :status])))
        (is (= [:read :disclose] @seen))
        (is (false? (:executor-call-site-observed? result)))
        (no-authority! result)))))

(deftest nondefinitive-tx-return-does-not-create-committed-evidence
  (let [calls (atom 0)]
    (with-redefs [tx/transact! (fn [& _] (swap! calls inc)
                                {:commit/status :not-committed})]
      (let [result (controlled/run-controlled-submit! (source) {})]
        (is (= 1 @calls))
        (is (= :unresolved (get-in result [:resolution :status])))
        (is (= :no-complete-evidence (:capture-status result)))
        (is (= :nondefinitive-physical-outcome (:capture-reason result)))
        (is (nil? (:acquisition-report result)))
        (no-authority! result)))))

(deftest indeterminate-exception-does-not-automatically-retry
  (let [calls (atom 0)]
    (with-redefs [tx/transact! (fn [& _] (swap! calls inc)
                                (throw (ex-info "ambiguous failure" {})))]
      (let [result (controlled/run-controlled-submit! (source) {})]
        (is (= 1 @calls))
        (is (= :unresolved (get-in result [:resolution :status])))
        (is (not= :committed (get-in result [:acquisition-report :captured-physical-status])))
        (is (nil? (:acquisition-report result)))
        (no-authority! result)))))

(deftest explicit-assertion-rejection-remains-physical-rejection
  (let [calls (atom 0)]
    (with-redefs [tx/transact! (fn [& _] (swap! calls inc)
                                (throw (ex-info "rejected" {:xtdb.error/code :xtdb/assert-failed})))]
      (let [result (controlled/run-controlled-submit! (source) {})]
        (is (= 1 @calls))
        (is (= :rejected (get-in result [:acquisition-report :captured-physical-status])))
        (is (= :unresolved (get-in result [:resolution :status])))
        (is (= :complete-paired-hook-observation (:capture-status result)))
        (no-authority! result)))))

(deftest later-visible-record-does-not-rewrite-physical-rejection
  (let [read-count (atom 0)
        record (:persisted-command-record (observation))
        src (assoc-in (source) [:configuration :read-record!]
                      (fn [_ _] (when (= 2 (swap! read-count inc)) record)))
        calls (atom 0)]
    (with-redefs [tx/transact! (fn [& _] (swap! calls inc)
                                (throw (ex-info "rejected" {:xtdb.error/code :xtdb/assert-failed})))]
      (let [result (controlled/run-controlled-submit! src {})]
        (is (= 1 @calls))
        (is (= 2 @read-count))
        (is (= :rejected (get-in result [:acquisition-report :captured-physical-status])))
        (is (= :complete-paired-hook-observation (:capture-status result)))
        (is (#{:replay :incorporated :incompatible :unavailable}
             (get-in result [:resolution :status])))
        (no-authority! result)))))

(deftest unexpected-constructor-failure-propagates-without-transaction
  (let [calls (atom 0)
        src (assoc-in (source) [:configuration :construct!]
                      (fn [& _] (throw (ex-info "constructor failed" {:internal :secret}))))]
    (with-redefs [tx/transact! (fn [& _] (swap! calls inc))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"constructor failed"
                            (controlled/run-controlled-submit! src {})))
      (is (= 0 @calls)))))

(deftest unexpected-authoritative-read-failure-propagates-without-transaction
  (let [calls (atom 0)
        src (assoc-in (source) [:configuration :read-record!]
                      (fn [& _] (throw (ex-info "authoritative read failed" {}))))]
    (with-redefs [tx/transact! (fn [& _] (swap! calls inc))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"authoritative read failed"
                            (controlled/run-controlled-submit! src {})))
      (is (= 0 @calls)))))

(deftest injected-hook-contexts-refused-before-application-callbacks
  (let [seen (atom 0)
        src (source (fn [& _] (swap! seen inc)))]
    (doseq [key hook-keys
            value [nil (fn [& _] nil) :forged-observer]]
      (is (= :preexisting-observation-hook
             (reason #(controlled/run-controlled-submit! src {key value}))))
      (is (= 0 @seen)))))

(deftest source-shape-is-closed-to-caller-supplied-session-material
  (let [src (source)]
    (doseq [k [:observe! :complete! :session :acquisition-report :capture-status]]
      (is (= :invalid-source-shape
             (reason #(controlled/run-controlled-submit! (assoc src k (fn [& _] nil)) {})))))
    (is (= :invalid-source-shape
           (reason #(controlled/run-controlled-submit! (dissoc src :writer-source) {}))))
    (is (= :invalid-source-shape
           (reason #(controlled/run-controlled-submit!
                     (with-meta src {:claim :source-authentic}) {}))))))

(deftest contexts-must-be-plain-metadata-free-maps
  (let [src (source)]
    (doseq [ctx [nil [] (java.util.HashMap.) (with-meta {} {:trusted true})]]
      (is (= :invalid-invocation-context
             (reason #(controlled/run-controlled-submit! src ctx)))))))

(deftest operation-identity-and-config-callbacks-are-validated
  (let [src (source)
        seen (atom 0)]
    (is (= :operation-source-mismatch
           (reason #(controlled/run-controlled-submit!
                     (assoc-in src [:configuration :operation] :forged/not-claim) {}))))
    (doseq [callback [:read-record! :may-disclose? :construct!]
            bad [nil 42 "not-a-function"]]
      (let [invalid (assoc-in src [:configuration callback] bad)]
        (is (= :invalid-callback
               (reason #(controlled/run-controlled-submit! invalid {}))))))
    (is (= :invalid-configuration
           (error-kind #(controlled/run-controlled-submit!
                     (assoc src :configuration
                            (assoc (:configuration src) :observe! (fn [& _] (swap! seen inc)))) {}))))
    (is (= 0 @seen))))

(deftest optional-and-required-callback-wrappers-remove-hooks
  ;; Directly test the v1382 isolation helper for optional callbacks that
  ;; cannot run without otherwise valid closed contracts in the public path.
  ;; Public submit tests above check the real :read-record!/:construct! path.
  (let [seen (atom [])
        callbacks (zipmap callback-keys
                          (repeat (fn [ctx & args]
                                    (swap! seen conj [ctx args])
                                    :callback-result)))
        isolated (#'controlled/isolated-configuration
                  (assoc callbacks :operation :example/claim))
        injected (assoc {:app/token :preserved}
                        resolved/observation-context-key :forged
                        resolved/construction-input-context-key :forged)]
    (doseq [callback callback-keys]
      (is (= :callback-result ((get isolated callback) injected :arg))))
    (is (= (count callback-keys) (count @seen)))
    (is (every? (fn [[ctx args]]
                  (and (= {:app/token :preserved} ctx)
                       (= [:arg] (vec args)))) @seen))
    (is (= :example/claim (:operation isolated)))))

(deftest substituted-executor-cannot-mint-controlled-evidence
  (let [seen (atom 0)]
    (with-redefs [resolved/build-operation
                  (fn [& _] (swap! seen inc)
                    (throw (ex-info "substitute builder executed" {})))]
      (is (= :substituted-entry-point
             (reason #(controlled/run-controlled-submit! (source) {}))))
      (is (= 0 @seen)))))

(deftest substituted-acquisition-cannot-mint-controlled-evidence
  (let [seen (atom 0)]
    (with-redefs [acquisition/start-acquisition!
                  (fn [& _] (swap! seen inc)
                    (throw (ex-info "substitute acquisition executed" {})))]
      (is (= :substituted-entry-point
             (reason #(controlled/run-controlled-submit! (source) {}))))
      (is (= 0 @seen)))))

(deftest rejection-errors-exclude-sensitive-context-and-plan
  (let [src (source)
        error (try
                (controlled/run-controlled-submit!
                 (assoc src :injected :secret-plan) {:principal/password "private"})
                nil
                (catch clojure.lang.ExceptionInfo e e))]
    (is (instance? clojure.lang.ExceptionInfo error))
    (is (= :invalid-source-shape (:reason (ex-data error))))
    (is (= #{:executor-controlled-acquisition/rejection :reason :at}
           (set (keys (ex-data error)))))
    (is (not (.contains (.getMessage error) "private")))
    (is (not (.contains (.getMessage error) "secret-plan")))))
