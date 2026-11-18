(ns gesso.assurance.proof.checked-biff-loopback-http-typed-trial-test
  "v1779: One REAL loopback HTTP request, one original Biff handler, the
   issued certified-plan executor, original catalog-derived typed checks,
   and one authoritative embedded-XTDB domain+ledger transaction."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.checked-biff-loopback-http-server :as http]
            [gesso.assurance.proof.checked-biff-host-adoption :as host]
            [gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test
             :as previous]
            [gesso.live.application-preflight :as app]
            [gesso.live.application-preflight.fixture :as app-fixture]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.resolved-operation-test]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]
           [java.time Duration]))

(def ^:private previous-ns
  'gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test)
(def ^:private model-ns 'gesso.model.resolved-operation-test)
(defn- helper [n s]
  (var-get (or (ns-resolve n s)
               (throw (ex-info "Missing v1779 fixture" {:ns n :symbol s})))))
(defn- model [s] (helper model-ns s))
(defn- earlier [s] (helper previous-ns s))
(def ^:dynamic *modules* [])
(def ^:dynamic *client* nil)
(use-fixtures :once
  (fn [run]
    ((model 'with-real-xtdb)
     (fn []
       (app-fixture/with-closed-application
        {:operations {:request/claim
                      (app-fixture/trusted-operation
                       :request/claim {:published-change-topics #{}})}}
        (fn [{:keys [assembly]}]
          (binding [previous/*assembly* assembly
                    *client* (-> (HttpClient/newBuilder)
                                 (.connectTimeout (Duration/ofSeconds 5))
                                 (.build))]
            (run))))))))

(defn- seed! [& xs] (apply (model 'seed!) xs))
(defn- data [] ((model 'fixture-data)))
(defn- read-request [id] ((model 'read-request) id))
(defn- read-command [id] ((model 'read-command) id))
(defn- reason [thunk]
  (try (thunk) :no-error
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- with-typed-server! [env f]
  (let [contract (:contract env)
        adopted (host/issue-checked-biff-host-module! contract)
        opts {:catalog-source (:catalog-source env)
              :operation :request/claim
              :inputs {:input/owner "helper-a"}}
        server (http/issue-loopback-http-server-module!
                (:handle adopted) contract ((model 'database)) opts)
        modules [app/application-handler-module
                 (:module adopted) (:module server)]]
    (try
      (let [system (binding [*modules* modules]
                     (app/start-biff-application!
                      (:application-assembly contract) {} #'*modules*
                      [app/application-handler-module-id
                       host/module-id http/module-id]))]
        (f {:server server :env env :system system
            :base ((:base-url! server))}))
      (finally ((:stop! server))))))

(defn- request! [base method path & {:keys [body]}]
  (let [req (HttpRequest/newBuilder (URI/create (str base path)))
        _ (.timeout req (Duration/ofSeconds 6))
        publisher (if (some? body)
                    (HttpRequest$BodyPublishers/ofString (str body))
                    (HttpRequest$BodyPublishers/noBody))
        built (.method req (.toUpperCase (name method)) publisher)
        response (.send ^HttpClient *client* (.build built)
                        (HttpResponse$BodyHandlers/ofString))]
    {:status (.statusCode response) :body (.body response)}))

(deftest real-http-socket-enters-original-typed-catalog-trial-in-same-request
  (let [{:keys [request binding] :as d} (data)
        env ((earlier 'case-env) d nil)]
    (seed! [:requests request])
    (with-typed-server! env
      (fn [{:keys [server base]}]
        (let [start ((:summary! server))]
          (is (= :started (:state start)))
          (is (= :exact-checked-handler-at-server-start
                 (:server-captured-biff-handler start)))
          (is (true? (:typed-trial-enabled start)))
          (is (= :not-established (:same-http-request-typed-trial start))))
        (is (= 200 (:status (request! base :post "/operations/request/claim"))))
        (let [report ((:summary! server))]
          (is (= :one-loopback-http-request-with-typed-catalog-atomic-ack
                 (:same-http-request-typed-trial report)))
          (is (= :one-original-issued-canonical-executor-observed
                 (:original-canonical-plan-adoption report)))
          (is (= :bounded-biff-issued-catalog-atomic-acknowledgment
                 (:typed-trial-last-outcome report)))
          (is (= 1 (:typed-trial-attempts report)))
          (is (= 1 (:typed-trial-qualified report)))
          (is (= 1 (:http-socket-requests-observed report)))
          (is (= 1 (:server-handler-dispatch-attempts report)))
          (is (= :not-established (:external-server-deployment report)))
          (is (= :not-established (:authenticated-principal report)))
          (is (= :not-issued (:certificate report)))
          (is (= :none (:retry-authority report) (:authority report)))
          (is (not-any? #(contains? report %)
                        [:handler :catalog :request :response :executor
                         :source :transaction :receipt :xtdb-node :tx-ops])))
        (is (= ((model 'next-version) request {:owner "helper-a"})
               (read-request (:xt/id request))))
        (is (= (resolution/command-record
                binding {:status :claimed :owner "helper-a"} "t1")
               (read-command (resolution/record-id binding))))
        (is (= 1 @(get-in env [:environment :constructs])))))))

(deftest repeated-http-request-is-not-a-second-model-write
  (let [{:keys [request binding] :as d} (data)
        env ((earlier 'case-env) d nil)]
    (seed! [:requests request])
    (with-typed-server! env
      (fn [{:keys [server base]}]
        (is (= 200 (:status (request! base :post "/operations/request/claim"))))
        (is (= 500 (:status (request! base :post "/operations/request/claim"))))
        (is (= 2 (:typed-trial-attempts ((:summary! server)))))
        (is (= 1 (:typed-trial-qualified ((:summary! server)))))
        (is (= 1 @(get-in env [:environment :constructs])))
        (is (= ((model 'next-version) request {:owner "helper-a"})
               (read-request (:xt/id request))))
        (is (some? (read-command (resolution/record-id binding))))))))

(deftest unsupported-route-and-client-forgery-never-consume-typed-trial
  (let [{:keys [request binding] :as d} (data)
        env ((earlier 'case-env) d nil)]
    (seed! [:requests request])
    (with-typed-server! env
      (fn [{:keys [server base]}]
        (is (= 404 (:status (request! base :post "/wrong"))))
        (is (= 404 (:status (request! base :get "/operations/request/claim"))))
        (is (= 400 (:status (request! base :post "/operations/request/claim"
                                     :body "{:biff.xtdb/node :forged}"))))
        (is (= 400 (:status (request! base :post
                                     "/operations/request/claim?principal=admin"))))
        (is (zero? (:typed-trial-attempts ((:summary! server)))))
        (is (= request (read-request (:xt/id request))))
        (is (nil? (read-command (resolution/record-id binding))))
        ;; The first accepted network request alone consumes the permit.
        (is (= 200 (:status (request! base :post "/operations/request/claim"))))
        (is (= 1 (:typed-trial-qualified ((:summary! server)))))))))

(deftest stale-dependency-cas-rejects-both-domain-and-ledger-over-http
  (let [{:keys [request membership binding] :as d} (data)
        env ((earlier 'case-env) d {:guard-membership? true})]
    (seed! [:requests request] [:memberships membership])
    (xt/execute-tx ((model 'database))
                   (:tx-ops (tx/prepare {}
                                        {:commands [((model 'revoke-command)
                                                     membership)]
                                         :emit false})))
    (with-typed-server! env
      (fn [{:keys [server base]}]
        (is (= 200 (:status (request! base :post "/operations/request/claim"))))
        (let [report ((:summary! server))]
          (is (= :biff-issued-catalog-canonical-rejected
                 (:typed-trial-last-outcome report)))
          (is (= :not-established (:same-http-request-typed-trial report)))
          (is (= 0 (:typed-trial-qualified report))))
        (is (= request (read-request (:xt/id request))))
        (is (nil? (read-command (resolution/record-id binding))))))))

(deftest lost-acknowledgment-remains-unqualified-and-no-retry-permit
  (let [{:keys [request binding] :as d} (data)
        env ((earlier 'case-env) d nil)
        original tx/transact!
        writes (atom 0)]
    (seed! [:requests request])
    (with-typed-server! env
      (fn [{:keys [server base]}]
        (is (= 500
               (:status (with-redefs [tx/transact!
                                      (fn [ctx plan]
                                        (swap! writes inc)
                                        (original ctx plan)
                                        (throw (ex-info "lost ack" {})))]
                          (request! base :post "/operations/request/claim")))))
        (is (= 1 @writes))
        (let [report ((:summary! server))]
          (is (= :not-qualified-or-uncertain
                 (:typed-trial-last-outcome report)))
          (is (= :not-established (:same-http-request-typed-trial report)))
          (is (= :none (:retry-authority report))))
        (is (some? (read-command (resolution/record-id binding))))
        (is (= 500 (:status (request! base :post "/operations/request/claim"))))
        (is (= 1 @writes))))))

(deftest inconsistent-checked-catalog-prevents-server-from-starting
  (let [d (data)
        env ((earlier 'case-env) d nil)
        contract (:contract env)
        bad (assoc-in (:catalog-source env)
                      [:catalog :operations 0 :construction :guard :expression]
                      [:= [:read :doc/owner] [:literal "impossible"]])
        adopted (host/issue-checked-biff-host-module! contract)
        opts {:catalog-source bad :operation :request/claim
              :inputs {:input/owner "helper-a"}}
        issued (http/issue-loopback-http-server-module!
                (:handle adopted) contract ((model 'database)) opts)
        modules [app/application-handler-module (:module adopted) (:module issued)]]
    (try
      (is (thrown? clojure.lang.ExceptionInfo
                   (binding [*modules* modules]
                     (app/start-biff-application!
                      (:application-assembly contract) {} #'*modules*
                      [app/application-handler-module-id
                       host/module-id http/module-id]))))
      (is (= :failed (:state ((:summary! issued)))))
      (is (= :loopback-server-not-running
             (reason (:base-url! issued))))
      (finally ((:stop! issued))))))

(deftest invalid-trial-source-never-issues-server
  (let [env ((earlier 'case-env))
        contract (:contract env)
        adopted (host/issue-checked-biff-host-module! contract)]
    (is (= :invalid-source-bound-network-trial
           (reason #(http/issue-loopback-http-server-module!
                     (:handle adopted) contract ((model 'database))
                     {:catalog-source {} :operation :request/claim
                      :inputs {:input/owner "helper-a"}
                      :submit! (fn [] :forgery)}))))))
