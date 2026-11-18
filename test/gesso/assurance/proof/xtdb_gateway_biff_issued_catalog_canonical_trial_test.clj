(ns gesso.assurance.proof.xtdb-gateway-biff-issued-catalog-canonical-trial-test
  "v1777: An original catalog-bound full atomic XTDB command is submitted
   through the actual locally adopted checked Biff handler, exactly once."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-biff-issued-catalog-canonical-trial
             :as joined]
            [gesso.assurance.proof.checked-biff-host-adoption :as host]
            [gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test
             :as previous]
            [gesso.live.application-preflight :as app]
            [gesso.live.application-preflight.fixture :as app-fixture]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.resolved-operation-test]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]))

(def ^:private previous-ns
  'gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test)
(def ^:private model-ns 'gesso.model.resolved-operation-test)
(defn- helper [n s]
  (var-get (or (ns-resolve n s)
               (throw (ex-info "Missing test fixture" {:ns n :symbol s})))))
(defn- model [s] (helper model-ns s))
(defn- previous-helper [s] (helper previous-ns s))
(def ^:dynamic *modules* [])

(use-fixtures :once
  (fn [run]
    ((model 'with-real-xtdb)
     (fn []
       (app-fixture/with-closed-application
        {:operations {:request/claim
                      (app-fixture/trusted-operation
                       :request/claim {:published-change-topics #{}})}}
        (fn [{:keys [assembly]}]
          (binding [previous/*assembly* assembly] (run))))))))

(defn- case!
  ([] (case! ((previous-helper 'case-env))))
  ([env]
   (let [contract (:contract env)
        started (host/issue-checked-biff-host-module! contract)
        modules [app/application-handler-module (:module started)]
        system (binding [*modules* modules]
                 (app/start-biff-application!
                  (:application-assembly contract) {} #'*modules*
                  [app/application-handler-module-id host/module-id]))]
     (assoc env :host-handle (:handle started) :system system))))
(defn- ring-request [env]
  {:request-method :post :uri "/operations/request/claim"
   :biff.xtdb/node ((model 'database))
   :request-id :v1777-observed})
(defn- trial [env]
  (joined/issue-biff-issued-catalog-canonical-trial!
   (:host-handle env) (:contract env) (:system env)
   (:catalog-source env) :request/claim {:input/owner "helper-a"}))
(defn- seed! [& items] (apply (model 'seed!) items))
(defn- read-request [id] ((model 'read-request) id))
(defn- read-command [id] ((model 'read-command) id))
(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo e
         (or (:reason (ex-data e)) (throw e)))))

(deftest biff-handler-executes-original-catalog-atomic-operation-on-xtdb
  (let [{:keys [data environment] :as env} (case!)
        {:keys [request binding]} data
        observed (trial env)]
    (seed! [:requests request])
    (is (= :not-attempted (:status ((:summary! observed)))))
    (let [result ((:observe-once! observed) (ring-request env))]
      (is (= joined/report-type (:type result)))
      (is (= joined/report-version (:version result)))
      (is (= :bounded-biff-issued-catalog-atomic-acknowledgment
             (:status result) (:status ((:summary! observed)))))
      (is (= :one-original-adopted-biff-handler-returned
             (:host-handler-return result)))
      (is (= :one-original-issued-executor-returned
             (:certified-executor-return result)))
      (is (= :bounded-catalog-bound-atomic-acknowledgment
             (:catalog-derived-atomic-outcome result)))
      (is (= :one-source-relative-model-transaction-acknowledged
             (:canonical-physical-execution result)))
      (is (= :checked-locally-adopted-system-map
             (:host-slot-provenance result)))
      (is (= :not-established (:deployed-http-handler-adoption result)))
      (is (= :not-established (:authenticated-request-principal result)))
      (is (= :not-issued (:certificate result)))
      (is (= :none (:authority result) (:retry-authority result)))
      (is (= ((model 'next-version) request {:owner "helper-a"})
             (read-request (:xt/id request))))
      (is (= (resolution/command-record
              binding {:status :claimed :owner "helper-a"} "t1")
             (read-command (resolution/record-id binding))))
      (is (= 1 @(:constructs environment)))
      (is (not-any? #(contains? result %)
                    [:ctx :request :response :source :catalog :contract :plan
                     :tx-ops :receipt :executor :handler :system :submit!]))
      (is (= :biff-issued-catalog-trial-already-attempted
             (reason #((:observe-once! observed) (ring-request env))))))))

(deftest wrong-route-consumes-permit-without-physical-write
  (let [{:keys [data environment] :as env} (case!)
        request-doc (:request data)
        observed (trial env)]
    (seed! [:requests request-doc])
    (is (= :request-does-not-select-issued-certified-route
           (reason #((:observe-once! observed)
                     (assoc (ring-request env) :request-method :get)))))
    (is (= request-doc (read-request (:xt/id request-doc))))
    (is (nil? (read-command (resolution/record-id (:binding data)))))
    (is (zero? @(:constructs environment)))
    (is (= :biff-issued-catalog-trial-already-attempted
           (reason #((:observe-once! observed) (ring-request env)))))))

(deftest unissued-host-or-replaced-handler-slot-does-not-qualify
  (let [env (case!)
        args [(:contract env) (:system env)
              (:catalog-source env) :request/claim {:input/owner "helper-a"}]]
    (is (= :unissued-host-adoption
           (reason #(apply joined/issue-biff-issued-catalog-canonical-trial!
                           (host/->HostAdoptionHandle) args))))
    (is (= :host-handler-slot-mismatch
           (reason #(joined/issue-biff-issued-catalog-canonical-trial!
                     (:host-handle env) (:contract env)
                     (assoc (:system env) :biff.ring/handler
                            (fn [_] {:status 200}))
                     (:catalog-source env) :request/claim
                     {:input/owner "helper-a"}))))))

(deftest unrelated-checked-catalog-cannot-observe-adopted-operation
  (let [env (case!)
        altered (assoc-in (:catalog-source env)
                          [:catalog :operations 0 :construction :guard :expression]
                          [:= [:read :doc/owner] [:literal "forged"]])]
    (is (not= :no-error
              (reason #(joined/issue-biff-issued-catalog-canonical-trial!
                        (:host-handle env) (:contract env) (:system env)
                        altered :request/claim {:input/owner "helper-a"}))))))

(deftest stale-guard-rejects-domain-and-ledger-in-same-biff-invocation
  (let [d ((previous-helper 'data))
        {:keys [request membership binding]} d
        env (case! ((previous-helper 'case-env) d {:guard-membership? true}))
        observed (trial env)]
    (seed! [:requests request] [:memberships membership])
    (xt/execute-tx ((model 'database))
                   (:tx-ops (tx/prepare {}
                                        {:commands [((model 'revoke-command)
                                                     membership)]
                                         :emit false})))
    (let [result ((:observe-once! observed) (ring-request env))]
      (is (= :biff-issued-catalog-canonical-rejected (:status result)))
      (is (= request (read-request (:xt/id request))))
      (is (nil? (read-command (resolution/record-id binding))))
      (is (= :none (:retry-authority result))))))

(deftest lost-acknowledgment-remains-uncertain-without-replaying-handler
  (let [{:keys [data environment] :as env} (case!)
        {:keys [request binding]} data
        observed (trial env)
        original tx/transact!
        writes (atom 0)]
    (seed! [:requests request])
    (let [result (with-redefs [tx/transact!
                              (fn [ctx plan]
                                (swap! writes inc)
                                (original ctx plan)
                                (throw (ex-info "lost acknowledgment" {})))]
                   ((:observe-once! observed) (ring-request env)))]
      (is (= :not-qualified-or-uncertain (:status result)))
      (is (= 1 @writes))
      (is (some? (read-command (resolution/record-id binding))))
      (is (= :none (:retry-authority result)))
      (is (= :biff-issued-catalog-trial-already-attempted
             (reason #((:observe-once! observed) (ring-request env))))))))

(deftest replaced-executor-scope-fails-closed-before-handler-invocation
  (let [{:keys [data environment] :as env} (case!)
        request-doc (:request data)
        observed (trial env)]
    (seed! [:requests request-doc])
    (with-redefs [gesso.live.application-preflight.certified-contract/observe-issued-executor-scope!
                  (fn [& _] {:executor-scope :one-original-issued-executor-returned})]
      (is (= :substituted-host-catalog-or-executor-scope-boundary
             (reason #((:observe-once! observed) (ring-request env))))))
    (is (= request-doc (read-request (:xt/id request-doc))))
    (is (zero? @(:constructs environment)))
    (is (= :biff-issued-catalog-trial-already-attempted
           (reason #((:observe-once! observed) (ring-request env)))))))
