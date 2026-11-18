(ns gesso.assurance.proof.typed-host-gateway-same-invocation-test
  "v1766: The same Biff handler invocation must return from both its issued
   certified executor and exactly one real source-bound XTDB gateway call.
   Demonstrates a gateway call from a trusted input callback; this is NOT
   proof that the certified executor itself performed the physical write."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.typed-host-gateway-same-invocation :as seam]
            [gesso.assurance.proof.checked-biff-host-adoption :as host]
            [gesso.assurance.proof.xtdb-gateway-typed-transition-seam :as typed]
            [gesso.assurance.proof.xtdb-gateway-typed-transition-seam-test]
            [gesso.live.application-preflight :as app]
            [gesso.live.application-preflight.certified-contract :as certified]))

(def ^:private typed-test
  'gesso.assurance.proof.xtdb-gateway-typed-transition-seam-test)
(def ^:private physical-test
  'gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test)
(def ^:private plan-test 'gesso.model.certified-plan-test)

(defn- helper [ns-name symbol]
  (var-get (or (ns-resolve ns-name symbol)
               (throw (ex-info "Missing test fixture helper" {:name symbol})))))

(use-fixtures :once (helper physical-test 'with-certified-xtdb))

(def ^:dynamic *modules* [])

(defn- before [] ((helper physical-test 'before)))
(defn- after [b] ((helper physical-test 'after) b))
(defn- candidate [b] ((helper physical-test 'candidate) b))
(defn- put! [b] ((helper physical-test 'put!) b))
(defn- physical [] ((helper physical-test 'physical)))
(defn- typed-preparer [] ((helper typed-test 'bound-preparer)))
(defn- typed-prepare [binder source]
  ((helper typed-test 'bound-prepare) binder source))
(defn- declared-catalog [] (helper typed-test 'declared-catalog))
(defn- contract [] (:contract (helper physical-test '*case*)))
(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo e
         (or (:reason (ex-data e)) (throw e)))))

(defn- observe-fixture [input-hook]
  (let [original (contract)
        operation :example/claim
        {:keys [executor current-source]}
        (get-in original [:certified-operations operation])
        issued-handler
        (certified/issue-certified-handler!
         {:executor executor
          :current-source current-source
          :request->input!
          (fn [ctx]
            (when input-hook (input-hook ctx))
            ((helper plan-test 'invocation) :reconcile))
          :result->response! (fn [_ result] {:status 200 :body result})})
        source (assoc-in original [:semantic-route-handlers operation]
                         issued-handler)
        started (host/issue-checked-biff-host-module! source)
        modules [app/application-handler-module (:module started)]
        system (binding [*modules* modules]
                 (app/start-biff-application!
                  (:application-assembly source) {} #'*modules*
                  [app/application-handler-module-id host/module-id]))]
    {:observer (seam/issue-same-invocation-observer!
                (:handle started) source system (declared-catalog) operation)
     :system system}))

(defn- request [] {:request-method :post :uri "/operations/example/claim"})

(deftest genuine-gateway-returns-during-one-biff-handler-invocation
  (let [b (before)]
    (put! b)
    (let [prepared (typed-prepare (typed-preparer) (candidate b))
          calls (atom 0)
          {:keys [observer]} (observe-fixture
                              (fn [_] (swap! calls inc) ((:submit! prepared))))
          start ((:summary! observer))
          report ((:observe-once! observer) (request))]
      (is (= :not-attempted (:status start)))
      (is (= :bounded-biff-handler-and-typed-gateway-return
             (:status report) (:status ((:summary! observer)))))
      (is (= :one-exact-issued-handler-executor-returned
             (:issued-executor-return report)))
      (is (= :one-same-thread-return-observed
             (:source-bound-gateway-return report)))
      (is (= 1 @calls))
      (is (= (after b) (physical)))
      (is (= :not-established (:gateway-in-certified-executor report)))
      (is (= :not-established (:gateway-to-reported-command-causality report)))
      (is (= :not-established (:actual-xtdb-commit report)))
      (is (= :not-established (:deployed-http-handler-adoption report)))
      (is (= :not-issued (:certificate report)))
      (is (= :none (:authority report)))
      (is (not-any? #(contains? report %)
                    [:request :response :execution :gateway :handler
                     :result :witness :node :tx :token]))
      (is (= :same-invocation-already-attempted
             (reason #((:observe-once! observer) (request))))))))

(deftest successful-handler-without-typed-gateway-is-not-qualified
  (let [b (before)]
    (put! b)
    (let [calls (atom 0)
          {:keys [observer]} (observe-fixture (fn [_] (swap! calls inc)))
          report ((:observe-once! observer) (request))]
      (is (= :no-qualified-same-invocation-gateway-return
             (:status report)))
      (is (= :one-exact-issued-handler-executor-returned
             (:issued-executor-return report)))
      (is (= :not-established (:source-bound-gateway-return report)))
      (is (= 1 @calls))
      (is (= b (physical)))
      (is (= :same-invocation-already-attempted
             (reason #((:observe-once! observer) (request))))))))

(deftest gateway-outside-handler-cannot-be-replayed-as-observed
  (let [b (before)]
    (put! b)
    (let [prepared (typed-prepare (typed-preparer) (candidate b))
          _ ((:submit! prepared))
          {:keys [observer]} (observe-fixture nil)
          report ((:observe-once! observer) (request))]
      (is (= :no-qualified-same-invocation-gateway-return (:status report)))
      (is (= :not-established (:source-bound-gateway-return report)))
      (is (= (after b) (physical))))))

(deftest wrong-operation-route-is-rejected-without-dispatch
  (let [b (before)]
    (put! b)
    (let [calls (atom 0)
          {:keys [observer]} (observe-fixture (fn [_] (swap! calls inc)))]
      (is (= :checked-biff-handler-did-not-return
             (reason #((:observe-once! observer)
                       {:request-method :get
                        :uri "/operations/example/claim"}))))
      (is (= :same-invocation-already-attempted
             (reason #((:observe-once! observer) (request)))))
      (is (zero? @calls))
      (is (= b (physical))))))

(deftest failed-typed-cas-cannot-be-promoted-to-gateway-return
  (let [b (before)]
    (put! b)
    (let [prepared (typed-prepare (typed-preparer) (candidate b))
          intervened (assoc b :note "v1766-intervened")
          {:keys [observer]} (observe-fixture (fn [_] ((:submit! prepared))))]
      (put! intervened)
      (is (= :checked-biff-handler-did-not-return
             (reason #((:observe-once! observer) (request)))))
      (is (= intervened (physical)))
      (is (= :same-invocation-already-attempted
             (reason #((:observe-once! observer) (request))))))))

(deftest substituted-trace-checker-consumes-one-shot-observation
  (let [b (before)]
    (put! b)
    (let [{:keys [observer]} (observe-fixture nil)]
      (with-redefs [typed/observe-source-bound-typed-submission!
                    (fn [& _] {:execution {:outcome :returned}
                               :report {:status :one-source-bound-typed-submit-return-observed}})]
        (is (= :substituted-host-or-typed-trace-observer
               (reason #((:observe-once! observer) (request))))))
      (is (= :same-invocation-already-attempted
             (reason #((:observe-once! observer) (request)))))
      (is (= b (physical))))))

(deftest multiple-typed-submissions-inside-handler-are-ambiguous
  (let [b (before)]
    (put! b)
    (let [binder (typed-preparer)
          first-gateway (typed-prepare binder (candidate b))
          stale-gateway (typed-prepare binder (candidate b))
          calls (atom 0)
          {:keys [observer]}
          (observe-fixture
           (fn [_]
             (swap! calls inc)
             ((:submit! first-gateway))
             (try ((:submit! stale-gateway))
                  (catch Throwable _ :failed-cas))))
          report ((:observe-once! observer) (request))]
      (is (= :no-qualified-same-invocation-gateway-return (:status report)))
      (is (= :not-established (:source-bound-gateway-return report)))
      (is (= 1 @calls))
      (is (= (after b) (physical)))
      (is (= :same-invocation-already-attempted
             (reason #((:observe-once! observer) (request))))))))

(deftest cross-thread-typed-gateway-return-is-not-synchronous-handler-evidence
  (let [b (before)]
    (put! b)
    (let [gateway (typed-prepare (typed-preparer) (candidate b))
          {:keys [observer]}
          (observe-fixture (fn [_] @(future ((:submit! gateway)))))
          report ((:observe-once! observer) (request))]
      (is (= :no-qualified-same-invocation-gateway-return (:status report)))
      (is (= :not-established (:source-bound-gateway-return report)))
      (is (= (after b) (physical)))
      (is (= :not-issued (:certificate report))))))
