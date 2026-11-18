(ns gesso.model.canonical-transaction-site-test
  "v1768: Distinguish Gesso's canonical resolved-operation transaction
   call site from application callback writes and independent typed gateways."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.model.canonical-transaction-site :as site]
            [gesso.model.resolved-operation :as resolved]
            [gesso.model.resolved-operation-test]
            [gesso.model.tx :as tx]))

(def ^:private originals 'gesso.model.resolved-operation-test)
(defn- helper [sym]
  (var-get (or (ns-resolve originals sym)
               (throw (ex-info "Missing original fixture helper" {:name sym})))))
(defn- e [] ((helper 'fake-environment)))
(defn- call [environment mode] ((helper 'call) environment mode))
(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))

(deftest canonical-transact-site-observed-but-not-a-typed-gateway
  (let [environment (e)
        calls (atom 0)]
    (with-redefs [tx/transact! (fn [_ _]
                                 (swap! calls inc)
                                 {:commit/status :committed})]
      (let [{:keys [execution report]}
            (site/observe-canonical-transaction-site!
             :request/claim #(call environment :submit))]
        (is (= :returned (:outcome execution)))
        (is (= :incorporated (get-in execution [:result :status])))
        (is (= :one-canonical-model-tx-acknowledged (:status report)))
        (is (= :observed (:model-tx-callsite report)))
        (is (= :committed-ack-observed (:model-tx-acknowledgment report)))
        (is (= 1 @calls))
        (is (= :not-established (:typed-complete-cas-gateway-used report)))
        (is (= :not-established (:gateway-to-canonical-tx-causality report)))
        (is (= :not-established (:xtdb-receipt-authenticity report)))
        (is (= :not-issued (:certificate report)))
        (is (= :none (:authority report)))
        (is (not-any? #(contains? report %)
                      [:events :token :result :error :execution :plan
                       :transaction :binding :ctx :request :node]))))))

(deftest reconciliation-never-observes-canonical-transaction-site
  (let [{:keys [report]} (site/observe-canonical-transaction-site!
                          :request/claim #(call (e) :reconcile))]
    (is (= :no-canonical-model-tx-site (:status report)))
    (is (= :not-established (:model-tx-callsite report)))
    (is (= :not-established (:model-tx-acknowledgment report)))))

(deftest callback-direct-transaction-cannot-impersonate-canonical-callsite
  (let [{:keys [configuration binding]} (e)
        side-effects (atom 0)
        operation (resolved/build-operation
                   (assoc configuration :construct!
                          (fn [_ _]
                            ;; A trusted callback submits separately and then
                            ;; fails; Gesso never reaches its canonical site.
                            (tx/transact! {} {:commands [] :guards [] :emit false})
                            (throw (ex-info "callback stopped" {})))))]
    (with-redefs [tx/transact! (fn [_ _]
                                 (swap! side-effects inc)
                                 {:commit/status :committed})]
      (let [{:keys [execution report]}
            (site/observe-canonical-transaction-site!
             :request/claim
             #(operation {} {:binding binding :mode :submit}))]
        (is (= :threw (:outcome execution)))
        (is (= 1 @side-effects))
        (is (= :no-canonical-model-tx-site (:status report)))
        (is (= :not-established (:model-tx-acknowledgment report)))))))

(deftest wrong-operation-or-multiple-resolved-operation-submissions-reject
  (with-redefs [tx/transact! (fn [_ _] {:commit/status :committed})]
    (let [{:keys [report]}
          (site/observe-canonical-transaction-site!
           :request/other #(call (e) :submit))]
      (is (= :ambiguous-or-mismatched-canonical-site (:status report)))
      (is (= :not-established (:model-tx-acknowledgment report))))
    (let [{:keys [report]}
          (site/observe-canonical-transaction-site!
           :request/claim #(do (call (e) :submit) (call (e) :submit)))]
      (is (= :ambiguous-or-mismatched-canonical-site (:status report)))
      (is (= :not-established (:model-tx-acknowledgment report))))))

(deftest cross-thread-observation-rejected-even-when-model-tx-returned
  (with-redefs [tx/transact! (fn [_ _] {:commit/status :committed})]
    (let [{:keys [report]}
          (site/observe-canonical-transaction-site!
           :request/claim #(deref (future (call (e) :submit)) 60000 :timed-out))]
      (is (= :ambiguous-or-mismatched-canonical-site (:status report)))
      (is (= :not-established (:model-tx-acknowledgment report))))))

(deftest failed-ack-or-guard-rejection-never-qualifies
  (doseq [[fn-tx expected-status]
          [[(fn [_ _] {:commit/status :unknown}) :canonical-model-tx-unconfirmed]
           [(fn [_ _] (throw (ex-info "guard rejected"
                                      {:xtdb.error/code :xtdb/assert-failed})))
            :canonical-model-tx-rejected]]]
    (with-redefs [tx/transact! fn-tx]
      (let [{:keys [report]}
            (site/observe-canonical-transaction-site!
             :request/claim #(call (e) :submit))]
        (is (= expected-status (:status report)))
        (is (= :not-established (:model-tx-acknowledgment report)))
        (is (= :not-established (:typed-complete-cas-gateway-used report)))))))

(deftest nested-capture-does-not-pretend-to-observe-submission
  (let [{:keys [report]}
        (site/observe-canonical-transaction-site!
         :request/claim
         (fn [] (is (= :nested-canonical-site-observation
                     (reason #(site/observe-canonical-transaction-site!
                               :request/claim (fn [] :forged)))))
              :done))]
    (is (= :no-canonical-model-tx-site (:status report)))))

(deftest actual-embedded-xtdb-canonical-submission-is-observed
  ((helper 'with-real-xtdb)
   (fn []
     (let [data ((helper 'fixture-data))
           env ((helper 'real-environment) data nil)]
       ((helper 'seed!) [:requests (:request data)])
       (let [{:keys [execution report]}
             (site/observe-canonical-transaction-site!
              :request/claim #((helper 'real-call) env :submit))]
         (is (= :returned (:outcome execution)))
         (is (= :incorporated (get-in execution [:result :status])))
         (is (= :one-canonical-model-tx-acknowledged (:status report)))
         (is (= :committed-ack-observed (:model-tx-acknowledgment report)))
         (is (= :not-established (:typed-complete-cas-gateway-used report)))
         (is (= :not-established (:xtdb-receipt-authenticity report)))
         (is (= :not-issued (:certificate report)))
         (is (= 1 @(:constructs env)))
         (is (= :replay (:status ((helper 'real-call) env :reconcile)))))))))

(deftest broken-diagnostic-publisher-does-not-affect-original-tx
  (let [counted (atom 0)]
    (with-redefs [tx/transact! (fn [_ _] (swap! counted inc)
                                 {:commit/status :committed})
                  site/record-site! (fn [& _]
                                      (throw (ex-info "diagnostic failed" {})))]
      (is (= :incorporated (:status (call (e) :submit))))
      (is (= 1 @counted)))))
