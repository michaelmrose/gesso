(ns gesso.assurance.proof.issued-executor-plan-mutation-test
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.issued-executor-physical-plan-correspondence :as c]
            [gesso.live.consistency.xtdb :as adapter]
            [gesso.model.certified-plan :as plan]
            [gesso.model.certified-plan-test]
            [gesso.model.tx :as tx])
  (:import [java.time Instant]
           [xtdb.api TransactionKey]))

(defn fixture [mode]
  (let [environment ((var-get (ns-resolve
                               'gesso.model.certified-plan-test 'environment)))
        executor (plan/build-certified-plan-operation! (:source environment))
        input ((var-get (ns-resolve 'gesso.model.certified-plan-test
                                    'invocation)) mode)
        req {:user :test :request/id "keep-private"}]
    {:executor executor
     :current-source (:source environment)
     :request req
     :request->input! (fn [_] input)
     :result->response! (fn [_ result] {:status 200 :body result})}))

(defn fake-key []
  (reify TransactionKey
    (getTxId [_] 42)
    (getSystemTime [_] (Instant/parse "2026-11-05T20:00:00Z"))))

(defn with-fake-physical [mutate f]
  (let [adapter-api-var (ns-resolve 'gesso.live.consistency.xtdb '*execute-tx*)]
    (with-redefs-fn
      {#'tx/transact!
       (fn [ctx plan]
         (let [expected (:tx-ops (tx/prepare ctx plan))
               actual (mutate expected)]
           (adapter/execute-tx! :controlled-test-node actual)
           {:commit/status :committed}))
       adapter-api-var (fn [_ _ _] (fake-key))}
      f)))

(defn run [s]
  (c/observe-issued-submission! s))

(defn status [r] (get-in r [:report :status]))

(defn failure [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest one-original-issued-executor-produces-matching-adapter-call
  (with-fake-physical identity
    (fn []
      (let [source (fixture :submit)
            result (run source)
            report (:report result)]
        (is (= :bounded-issued-executor-to-adapter-plan-agreement (status result)))
        (is (= :returned (get-in result [:execution :outcome])))
        (is (c/issued-submission-witness? (:handle result)))
        (is (= report
               (c/require-current-issued-submission! (:handle result) source report)))
        (is (true? (:prepared-operations-agree? report)))
        (is (true? (:one-adapter-call-observed? report)))
        (is (true? (:reported-commit-and-execute-key-agree? report)))
        (is (= :not-established (:physical-collector-same-execution report)))
        (is (= :none (:authority report)))
        (is (= :not-issued (:certificate report)))
        (is (not (re-find #"keep-private" (pr-str report))))
        (is (not (contains? report :tx-ops)))))))

(deftest mismatching-actual-adapter-argument-does-not-issue-handle
  (with-fake-physical #(conj % [:bogus :different])
    (fn []
      (let [result (run (fixture :submit))]
        (is (= :prepared-plan-diverges-from-adapter-operations (status result)))
        (is (nil? (:handle result)))
        (is (false? (get-in result [:report :prepared-operations-agree?])))))))

(deftest missing-adapter-capture-does-not-issue-handle
  (with-redefs [tx/transact! (fn [_ _] {:commit/status :committed})]
    (let [r (run (fixture :submit))]
      (is (= :missing-or-invalid-adapter-call (status r)))
      (is (nil? (:handle r))))))

(deftest execute-tx-more-than-once-is-never-positive
  (with-redefs-fn
    {(ns-resolve 'gesso.live.consistency.xtdb '*execute-tx*)
     (fn [_ _ _] (fake-key))
     #'tx/transact!
     (fn [ctx plan]
       (let [ops (:tx-ops (tx/prepare ctx plan))]
         (adapter/execute-tx! :node ops)
         (adapter/execute-tx! :node ops)
         {:commit/status :committed}))}
    (fn []
      (let [r (run (fixture :submit))]
        (is (= :missing-or-invalid-adapter-call (status r)))
        (is (nil? (:handle r)))))))

(deftest forged-and-substituted-recheck-are-rejected
  (with-fake-physical identity
    (fn []
      (let [source (fixture :submit)
            result (run source)
            handle (:handle result)
            report (:report result)]
        (is (= :unknown-or-forged-witness
               (:reason (failure #(c/require-current-issued-submission!
                                   (c/->IssuedSubmissionHandle) source report)))))
        (is (= :request-substitution
               (:reason (failure #(c/require-current-issued-submission!
                                   handle (assoc source :request
                                                 (into {} (:request source))) report)))))
        (is (= :altered-or-stale-report
               (:reason (failure #(c/require-current-issued-submission!
                                   handle source (assoc report :certificate :issued))))))
        (is (= :stale-plan-source
               (:reason (failure #(c/require-current-issued-submission!
                                   handle (assoc source :executor
                                                 (:executor (fixture :submit))) report)))))))))

(deftest no-observer-injection-into-original-request
  (with-fake-physical identity
    (fn []
      (let [seen (atom [])
            source (-> (fixture :submit)
                       (assoc :request->input! (fn [ctx]
                                                 (swap! seen conj ctx)
                                                 ((var-get (ns-resolve
                                                            'gesso.model.certified-plan-test
                                                            'invocation)) :submit))))]
        (run source)
        (is (= [(:request source)] @seen))))))

(deftest preinstalled-observer-is-refused-before-any-write
  (let [source (assoc-in (fixture :submit)
                         [:request :gesso.model.resolved-operation/observe-transaction!]
                         (fn [_] nil))]
    (is (= :invalid-source-or-preexisting-observer
           (:reason (failure #(run source)))))))

(deftest source-callback-substitution-is-rejected
  (with-fake-physical identity
    (fn []
      (let [source (fixture :submit)
            {:keys [handle report]} (run source)]
        (is (= :source-substitution
               (:reason (failure
                         #(c/require-current-issued-submission!
                           handle (assoc source :request->input!
                                         (fn [_] :forged)) report)))))))))

(deftest reconcile-does-not-claim-physical-submission
  (with-fake-physical identity
    (fn []
      (let [r (run (fixture :reconcile))]
        (is (= :missing-or-invalid-resolved-executor-step (status r)))
        (is (nil? (:handle r)))
        (is (false? (get-in r [:report :reported-commit-and-execute-key-agree?])))))))

(deftest submit-only-acknowledgment-never-counts-as-commit
  (with-redefs-fn
    {(ns-resolve 'gesso.live.consistency.xtdb '*submit-tx*)
     (fn [_ _ _] {:tx-id 42})
     #'tx/transact!
     (fn [ctx plan]
       (adapter/submit-tx! :node (:tx-ops (tx/prepare ctx plan)))
       ;; Even a dishonest :committed status from this caller is insufficient.
       {:commit/status :committed})}
    (fn []
      (let [r (run (fixture :submit))]
        (is (= :submit-only-not-definitive-commit (status r)))
        (is (nil? (:handle r)))
        (is (false? (get-in r [:report :reported-commit-and-execute-key-agree?])))))))

(deftest unrecognized-execute-tx-return-does-not-establish-commit
  (with-redefs-fn
    {(ns-resolve 'gesso.live.consistency.xtdb '*execute-tx*)
     (fn [_ _ _] {:tx-id 42})
     #'tx/transact!
     (fn [ctx plan]
       (adapter/execute-tx! :node (:tx-ops (tx/prepare ctx plan)))
       {:commit/status :committed})}
    (fn []
      (let [r (run (fixture :submit))]
        (is (= :no-definitive-execute-key (status r)))
        (is (nil? (:handle r)))))))

(deftest separate-invocations-own-distinct-witnesses
  (with-fake-physical identity
    (fn []
      (let [source (fixture :submit)
            a (run source)
            b (run source)]
        ;; Identical redacted reports are not physical transaction receipts.
        (is (= (:report a) (:report b)))
        (is (not (identical? (:handle a) (:handle b))))
        (is (c/issued-submission-witness? (:handle a)))
        (is (c/issued-submission-witness? (:handle b)))
        (is (= (:report a)
               (c/require-current-issued-submission!
                (:handle a) source (:report a))))
        (is (= (:report b)
               (c/require-current-issued-submission!
                (:handle b) source (:report b))))))))

(deftest report-and-copied-map-never-serve-as-live-witnesses
  (with-fake-physical identity
    (fn []
      (let [source (fixture :submit)
            {:keys [report handle]} (run source)]
        (doseq [pretender [report (into {} report) (c/->IssuedSubmissionHandle)
                           (Object.) nil]]
          (is (false? (c/issued-submission-witness? pretender)))
          (is (= :unknown-or-forged-witness
                 (:reason (failure #(c/require-current-issued-submission!
                                     pretender source report))))))
        (is (c/issued-submission-witness? handle))))))

(deftest report-cannot-claim-production-route-or-physical-receipt
  (with-fake-physical identity
    (fn []
      (let [source (fixture :submit)
            {:keys [handle report]} (run source)]
        (is (= :one-process-local-fresh-issued-handler-invocation (:scope report)))
        (is (= :not-established (:deployed-handler-provenance report)))
        (is (false? (:physical-xtdb-receipt-authenticated? report)))
        (is (= :not-established (:complete-writer-coverage report)))
        (is (= :not-established (:physical-collector-same-execution report)))
        (is (= :none (:transaction-authority report)))
        (is (= :not-admitted (:admission report)))
        (is (= :not-issued (:certificate report)))
        (doseq [obligation [:actual-deployed-route-uses-this-issued-handler
                            :original-xtdb-api-and-adapter-binding-authenticity
                            :actual-database-node-and-committed-transaction-receipt
                            :atomic-transaction-internal-prestate-and-guards
                            :complete-physical-writer-inventory
                            :universal-xtdb-execution-refinement]]
          (is (contains? (:open-obligations report) obligation)))
        (doseq [change [{:physical-xtdb-receipt-authenticated? true}
                        {:deployed-handler-provenance :authenticated}
                        {:certificate :issued}
                        {:transaction-authority :write}
                        {:open-obligations #{}}
                        {:status :certified}
                        {:complete-writer-coverage :proven}]]
          (is (= :altered-or-stale-report
                 (:reason (failure #(c/require-current-issued-submission!
                                     handle source (merge report change)))))))))))

(deftest false-commit-report-does-not-override-observed-call
  (with-redefs-fn
    {(ns-resolve 'gesso.live.consistency.xtdb '*execute-tx*)
     (fn [_ _ _] (fake-key))
     #'tx/transact!
     (fn [ctx plan]
       (adapter/execute-tx! :node (:tx-ops (tx/prepare ctx plan)))
       {:commit/status :rejected})}
    (fn []
      (let [r (run (fixture :submit))]
        (is (= :execution-outcome-not-established (status r)))
        (is (nil? (:handle r)))
        (is (false? (get-in r [:report :reported-commit-and-execute-key-agree?])))))))

(deftest physical-adapter-exception-is-observed-once-and-never-retried
  (let [calls (atom 0)
        boom (ex-info "secret transaction exception" {:private-token "do-not-print"})]
    (with-redefs-fn
      {(ns-resolve 'gesso.live.consistency.xtdb '*execute-tx*)
       (fn [_ _ _]
         (swap! calls inc)
         (throw boom))
       #'tx/transact!
       (fn [ctx plan]
         (adapter/execute-tx! :node (:tx-ops (tx/prepare ctx plan)))
         {:commit/status :committed})}
      (fn []
        (let [r (run (fixture :submit))]
          (is (= 1 @calls))
          ;; The issued handler may catch and classify the adapter error.
          ;; Regardless, its error path must never create a positive witness.
          (is (#{:threw :returned} (get-in r [:execution :outcome])))
          (when (= :threw (get-in r [:execution :outcome]))
            (is (identical? boom (get-in r [:execution :error]))))
          (is (nil? (:handle r)))
          (is (not= :bounded-issued-executor-to-adapter-plan-agreement (status r)))
          (is (not (re-find #"secret transaction exception|do-not-print"
                            (pr-str (:report r))))))))))

(deftest missing-physical-step-remains-diagnostic-only
  (with-redefs [tx/transact! (fn [_ _] {:commit/status :committed})]
    (let [{:keys [report handle]} (run (fixture :submit))]
      (is (nil? handle))
      (is (false? (:one-adapter-call-observed? report)))
      (is (= :missing-or-invalid-adapter-call (:status report)))
      (is (= :none (:authority report)))
      (is (= :not-issued (:certificate report))))))

(deftest altered-function-sources-rejected-before-handler-executes
  (doseq [[to-redefine replacement]
          [[#'plan/require-current-plan-operation! (fn [& _] true)]
           [#'plan/describe-plan-operation (fn [& _] {:operation :fake})]
           [#'tx/prepare (fn [& _] {:tx-ops []})]
           [#'adapter/capture-tx-api-calls! (fn [& _] {:events []})]]]
    (let [source (fixture :submit)
          input-calls (atom 0)
          source (assoc source :request->input!
                        (fn [_] (swap! input-calls inc) {:mode :submit}))]
      (with-redefs-fn
        {to-redefine replacement}
        (fn []
          (is (= :substituted-execution-boundary
                 (:reason (failure #(run source)))))
          (is (= 0 @input-calls)))))))

(deftest function-substitution-invalidates-a-previously-live-witness
  (with-fake-physical identity
    (fn []
      (let [source (fixture :submit)
            {:keys [handle report]} (run source)]
        (with-redefs-fn
          {#'plan/describe-plan-operation (fn [& _] {:operation :fake})}
          (fn []
            (is (= :substituted-execution-boundary
                   (:reason (failure
                             #(c/require-current-issued-submission!
                               handle source report)))))))))))

(deftest all-five-source-fields-rechecked
  (with-fake-physical identity
    (fn []
      (let [source (fixture :submit)
            {:keys [handle report]} (run source)
            another (fixture :submit)]
        (doseq [field [:executor :current-source :request
                       :request->input! :result->response!]]
          (let [replacement (if (= field :request)
                              (assoc (:request source) :test/nonce (random-uuid))
                              (get another field))
                changed (assoc source field replacement)]
            (is (map? (failure #(c/require-current-issued-submission!
                                 handle changed report)))
                (str "source substitution must fail: " field))))
        (is (= :altered-or-stale-report
               (:reason (failure #(c/require-current-issued-submission!
                                   handle source (dissoc report :open-obligations))))))))))

(deftest concurrent-reentrant-call-cannot-share-observation-scope
  (with-fake-physical identity
    (fn []
      (let [source (fixture :submit)
            one-shot (atom nil)
            scoped (assoc source :request->input!
                          (fn [_]
                            (reset! one-shot
                                    (failure #(c/observe-issued-submission!
                                               (fixture :submit))))
                            ((var-get (ns-resolve
                                       'gesso.model.certified-plan-test
                                       'invocation)) :submit)))
            result (run scoped)]
        (is (= :nested-issued-executor-observation (:reason @one-shot)))
        (is (= :bounded-issued-executor-to-adapter-plan-agreement (status result)))
        (is (c/issued-submission-witness? (:handle result)))))))

(deftest malformed-inputs-never-run-callbacks
  (doseq [modify [(fn [s] (assoc s :request nil))
                  (fn [s] (assoc s :request->input! :forged))
                  (fn [s] (assoc s :result->response! :forged))
                  (fn [s] (assoc s :extra-claim :certified))
                  (fn [s] (with-meta s {:certificate :issued}))]]
    (let [calls (atom 0)
          source (assoc (fixture :submit)
                        :request->input! (fn [_] (swap! calls inc) :invalid))]
      (is (= :invalid-source-or-preexisting-observer
             (:reason (failure #(run (modify source))))))
      (is (zero? @calls)))))

(deftest redaction-persists-across-rechecking
  (with-fake-physical identity
    (fn []
      (let [source (assoc (fixture :submit)
                          :request {:password "never-print-me"
                                    :xt/id 9911})
            {:keys [handle report]} (run source)
            original (pr-str report)
            rechecked (pr-str
                       (c/require-current-issued-submission!
                        handle source report))]
        (is (not (re-find #"never-print-me|9911|:tx-ops|:tx-result|:request"
                          original)))
        (is (= original rechecked))
        (is (not (contains? report :plan)))
        (is (not (contains? report :handler)))))))
