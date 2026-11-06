(ns gesso.assurance.proof.issued-executor-ledger-join-mutation-test
  "v1474: adversarial, source-rechecked, process-local issued/XTDB ledger join.

   Exercise the v1473 public join rather than comparing separately manufactured
   reports. All synthetic transaction overrides are explicit controlled test
   seams; embedded XTDB verifies actual commit and pinned ledger readback.
   Neither a local handle nor a ledger row proves a deployed production route,
   independent transaction receipt, physical snapshot refinement, or complete
   writer coverage."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.issued-executor-ledger-join :as join]
            [gesso.assurance.proof.issued-executor-physical-plan-correspondence :as issued]
            [gesso.live.consistency.xtdb :as adapter]
            [gesso.model.certified-plan :as certified-plan]
            [gesso.model.certified-plan-test]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(defn- fixture [node]
  (let [environment ((var-get (ns-resolve
                               'gesso.model.certified-plan-test 'environment)))
        executor (certified-plan/build-certified-plan-operation!
                  (:source environment))]
    {:issued-source
     {:executor executor
      :current-source (:source environment)
      :request {:route :controlled :private "do-not-disclose-v1474"}
      :request->input! (fn [_]
                         ((var-get (ns-resolve
                                    'gesso.model.certified-plan-test 'invocation))
                          :submit))
      :result->response! (fn [_ result] {:status 200 :body result})}
     :node node}))

(defn- rejection [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- run [source]
  (join/observe-issued-ledger-join! source))

(defn- seed-prestate! [node]
  ;; This is TEST SETUP, a separate transaction performed BEFORE the issued
  ;; invocation. The certified claim operation requires version 2 to exist.
  ;; It must never be counted as the claimed same-execution transaction.
  (xt/execute-tx node
                 [[:put-docs {:into :requests}
                   {:xt/id 10 :request/status :open
                    :request/version 2 :request/score 12}]])
  node)

(defn- controlled-tx! [node]
  ;; This is a diagnostic test seam, NOT the production transaction wrapper.
  ;; Send exactly what the original tx/prepare produced, through the original
  ;; adapter call site, which v1473 forwards to xt/execute-tx once.
  (fn [ctx plan]
    (adapter/execute-tx! node (:tx-ops (tx/prepare ctx plan)))
    {:commit/status :committed}))

(defn- with-controlled-tx [node f]
  (seed-prestate! node)
  (with-redefs [tx/transact! (controlled-tx! node)]
    (f)))

(defn- positive! [r]
  (let [{:keys [handle report]} r]
    (is (join/issued-ledger-witness? handle))
    (is (= :bounded-issued-plan-and-physical-ledger-agreement
           (:status report)))
    (is (= :one-issued-handler-and-one-xtdb-call
           (:same-physical-execution report)))
    (is (= :matching-committed-ledger-row (:ledger-readback-status report)))
    (is (every? true? (vals (:ledger-checks report))))
    (is (:one-physical-call-attempted? report))
    (is (:original-xtdb-api-invoked? report))
    (is (:issued-plan-adapter-correspondence? report))
    r))

(deftest real-embedded-xtdb-ledger-match-is-source-bound
  (with-open [node (xtn/start-node {})]
    (with-controlled-tx
      node
      (fn []
        (let [source (fixture node)
              {:keys [handle report] :as result} (run source)]
          (positive! result)
          (is (= report
                 (join/require-current-issued-ledger-join!
                  handle source report)))
          (is (= :not-executed (:controlled-v1467-collector report)))
          (is (= :not-authenticated (:independent-physical-receipt report)))
          (is (= :none (:transaction-authority report)))
          (is (= :not-issued (:certificate report)))
          (is (= :none (:authority report)))
          (is (set/subset?
               #{:actual-authorization-and-principal-input-binding
                 :physical-transaction-internal-prestate-not-observed
                 :complete-physical-writer-inventory
                 :original-xtdb-api-and-system-ledger-trust
                 :universal-xtdb-execution-refinement}
               (:open-obligations report))))))))

(deftest rechecking-does-not-submit-another-transaction
  (with-open [node (xtn/start-node {})]
    (seed-prestate! node)
    (let [attempts (atom 0)]
      (with-redefs [tx/transact! (fn [ctx plan]
                                  (swap! attempts inc)
                                  ((controlled-tx! node) ctx plan))]
        (let [source (fixture node)
              {:keys [handle report]} (run source)]
          (is (join/issued-ledger-witness? handle))
          (dotimes [_ 5]
            (is (= report
                   (join/require-current-issued-ledger-join!
                    handle source report))))
          (is (= 1 @attempts)))))))

(deftest forged-and-copied-handles-have-no-authority
  (with-open [node (xtn/start-node {})]
    (with-controlled-tx
      node
      (fn []
        (let [source (fixture node)
              {:keys [handle report]} (run source)]
          (is (join/issued-ledger-witness? handle))
          (doseq [pretender [(join/->IssuedLedgerHandle)
                             report (into {} report) (Object.) nil]]
            (is (not (join/issued-ledger-witness? pretender)))
            (is (= :unknown-or-forged-witness
                   (rejection #(join/require-current-issued-ledger-join!
                                pretender source report))))))))))

(deftest changed-node-and-issued-source-reject-live-witness
  (with-open [node (xtn/start-node {})]
    (with-controlled-tx
      node
      (fn []
        (let [source (fixture node)
              {:keys [handle report]} (run source)
              original (:issued-source source)]
          (is (= :node-substitution
                 (rejection #(join/require-current-issued-ledger-join!
                              handle (assoc source :node (Object.)) report))))
          (is (= :request-substitution
                 (rejection #(join/require-current-issued-ledger-join!
                              handle (assoc source :issued-source
                                            (assoc original :request
                                                   (into {} (:request original))))
                              report))))
          (is (= :source-substitution
                 (rejection #(join/require-current-issued-ledger-join!
                              handle (assoc source :issued-source
                                            (assoc original :request->input!
                                                   (fn [_] :forged)))
                              report)))))))))

(deftest changed-evidence-and-upgraded-claims-rejected
  (with-open [node (xtn/start-node {})]
    (with-controlled-tx
      node
      (fn []
        (let [source (fixture node)
              {:keys [handle report]} (run source)]
          (doseq [alter [(fn [r] (assoc r :certificate :issued))
                         (fn [r] (assoc r :authority :full))
                         (fn [r] (assoc r :transaction-authority :write))
                         (fn [r] (assoc r :independent-physical-receipt
                                        :authenticated))
                         (fn [r] (assoc r :same-physical-execution :proven))
                         (fn [r] (assoc r :ledger-checks
                                        (assoc (:ledger-checks r) :committed false)))
                         (fn [r] (assoc r :open-obligations #{}))
                         (fn [r] (dissoc r :ledger-readback-status))
                         (fn [r] (with-meta r {:certificate :issued}))]]
            (is (= :stale-or-altered-join-report
                   (rejection #(join/require-current-issued-ledger-join!
                                handle source (alter report)))))))))))

(deftest malformed-sources-fail-before-handler-input
  (let [calls (atom 0)
        basis {:issued-source {:executor :fake
                               :current-source :fake
                               :request {}
                               :request->input! (fn [_] (swap! calls inc))
                               :result->response! identity}
               :node (Object.)}]
    (doseq [alter [(fn [s] (dissoc s :node))
                   (fn [s] (assoc s :node nil))
                   (fn [s] (assoc s :extra :certificate))
                   (fn [s] (assoc s :issued-source :forged))
                   (fn [s] (with-meta s {:certificate :issued}))]]
      (is (= :invalid-source (rejection #(run (alter basis)))))
      (is (zero? @calls)))))

(deftest altered-original-execution-boundaries-reject-before-write
  (let [replacements [[#'issued/observe-issued-submission! (fn [_] {})]
                      [#'issued/require-current-issued-submission! (fn [& _] nil)]
                      [#'xt/execute-tx (fn [& _] nil)]
                      [#'xt/q (fn [& _] [])]]]
    (doseq [[v replacement] replacements]
      (let [calls (atom 0)
            source {:node (Object.)
                    :issued-source {:no-credentials true}}]
        (with-redefs-fn {v replacement}
          (fn []
            (is (= :substituted-observation-or-xtdb-boundary
                   (rejection #(run source))))
            (is (zero? @calls))))))))

(deftest prebound-adapter-or-nested-scope-rejected-before-execution
  (let [source {:node (Object.) :issued-source {}}
        api-var (ns-resolve 'gesso.live.consistency.xtdb '*execute-tx*)]
    (with-bindings {api-var (fn [& _] nil)}
      (is (= :nested-or-prebound-execute-boundary
             (rejection #(run source)))))))

(deftest missing-physical-submission-does-not-prove-ledger-join
  (with-open [node (xtn/start-node {})]
    (with-redefs [tx/transact! (fn [_ _] {:commit/status :committed})]
      (let [source (fixture node)
            {:keys [handle report]} (run source)]
        (is (nil? handle))
        (is (= :missing-or-repeated-physical-call (:status report)))
        (is (= :not-read (:ledger-readback-status report)))
        (is (= :not-established (:same-physical-execution report)))
        (is (false? (:one-physical-call-attempted? report)))
        (is (= :none (:authority report)))))))

(deftest mismatched-node-rejected-before-physical-write
  (with-open [node (xtn/start-node {})]
    (with-redefs [tx/transact! (fn [ctx plan]
                                ;; The join must reject the WRONG node before
                                ;; touching the original XTDB execute-tx API.
                                (adapter/execute-tx!
                                 (Object.) (:tx-ops (tx/prepare ctx plan)))
                                {:commit/status :committed})]
      (let [source (fixture node)
            result (run source)]
        (is (nil? (:handle result)))
        (is (= :issued-plan-correspondence-incomplete
               (get-in result [:report :status])))
        (is (false? (get-in result [:report :original-xtdb-api-invoked?])))
        (is (= :not-established
               (get-in result [:report :same-physical-execution])))))))

(deftest duplicate-physical-attempt-is-refused-before-second-write
  (with-open [node (xtn/start-node {})]
    (let [second-rejection (atom nil)]
      (seed-prestate! node)
      (with-redefs [tx/transact! (fn [ctx plan]
                                  (let [ops (:tx-ops (tx/prepare ctx plan))]
                                    (adapter/execute-tx! node ops)
                                    (reset! second-rejection
                                            (rejection #(adapter/execute-tx!
                                                          node ops)))
                                    {:commit/status :committed}))]
        (let [result (run (fixture node))]
          (is (= :duplicate-physical-call-before-submission @second-rejection))
          (is (nil? (:handle result)))
          (is (= :missing-or-repeated-physical-call
                 (get-in result [:report :status])))
          (is (= :not-established
                 (get-in result [:report :same-physical-execution]))))))))

(deftest pre-submission-exception-is-not-retried
  (let [calls (atom 0)
        exception (ex-info "private pre-submit failure"
                           {:secret "never-print-this"})
        source (fixture (Object.))]
    (with-redefs [tx/transact! (fn [_ _]
                                (swap! calls inc)
                                (throw exception))]
      (let [r (run source)]
        (is (= 1 @calls))
        (is (nil? (:handle r)))
        (is (= :missing-or-repeated-physical-call
               (get-in r [:report :status])))
        (is (= :not-read (get-in r [:report :ledger-readback-status])))
        (is (= :not-established
               (get-in r [:report :same-physical-execution])))
        (is (not (re-find #"private pre-submit failure|never-print-this"
                          (pr-str (:report r)))))))))

(deftest post-commit-exception-does-not-retry-or-issue-join-witness
  ;; This is a REAL committed XTDB transaction followed by an exception
  ;; deliberately raised by the TEST wrapper. A post-submit failure does not
  ;; imply rollback, and v1473 must not execute a second transaction.
  (with-open [node (xtn/start-node {})]
    (seed-prestate! node)
    (let [calls (atom 0)
          exception (ex-info "private post-commit failure"
                             {:secret "never-print-this"})]
      (with-redefs [tx/transact! (fn [ctx plan]
                                  (swap! calls inc)
                                  (adapter/execute-tx!
                                   node (:tx-ops (tx/prepare ctx plan)))
                                  (throw exception))]
        (let [r (run (fixture node))
              report (:report r)]
          (is (= 1 @calls))
          (is (nil? (:handle r)))
          (is (:one-physical-call-attempted? report))
          (is (:original-xtdb-api-invoked? report))
          (is (= :issued-plan-correspondence-incomplete (:status report)))
          (is (= :not-established (:same-physical-execution report)))
          (is (= :not-read (:ledger-readback-status report)))
          (is (not (re-find #"private post-commit failure|never-print-this"
                            (pr-str report)))))))))

(deftest public-join-report-never-contains-private-identifiers
  (with-open [node (xtn/start-node {})]
    (with-controlled-tx
      node
      (fn []
        (let [source (fixture node)
              {:keys [report]} (run source)
              printed (pr-str report)]
          (is (not (re-find #"do-not-disclose-v1474|:tx-ops|:tx-result|:request"
                            printed)))
          (is (not (contains? report :node)))
          (is (not (contains? report :source)))
          (is (not (contains? report :transaction-key)))
          (is (not (contains? report :issued-handle)))
          (is (= :bounded-readback-diagnostic-only (:proof-status report)))
          (is (= :one-controlled-single-node-process-local-execution
                 (:scope report))))))))
