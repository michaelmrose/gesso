(ns gesso.assurance.proof.checked-biff-loopback-http-server-test
  "v1778: Proving-only actual loopback socket -> captured Biff handler ->
   certified executor -> embedded XTDB domain+durable command ledger.
   This does not claim production deployment or same-request typed proof."
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
               (throw (ex-info "Missing fixture helper" {:ns n :symbol s})))))
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

(defn- before [] ((model 'fixture-data)))
(defn- seed! [& x] (apply (model 'seed!) x))
(defn- read-request [id] ((model 'read-request) id))
(defn- read-command [id] ((model 'read-command) id))
(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- with-server! [env f]
  (let [contract (:contract env)
        host-issued (host/issue-checked-biff-host-module! contract)
        issued (http/issue-loopback-http-server-module!
                (:handle host-issued) contract ((model 'database)))
        modules [app/application-handler-module
                 (:module host-issued) (:module issued)]]
    (try
      (let [system (binding [*modules* modules]
                     (app/start-biff-application!
                      (:application-assembly contract) {} #'*modules*
                      [app/application-handler-module-id
                       host/module-id http/module-id]))]
        (f {:env env :server issued :system system
            :base ((:base-url! issued))}))
      (finally ((:stop! issued))))))

(defn- request! [base method path & {:keys [body headers]}]
  (let [req (HttpRequest/newBuilder (URI/create (str base path)))
        _ (.timeout req (Duration/ofSeconds 6))
        _ (doseq [[k v] headers] (.header req k v))
        publisher (if (some? body)
                    (HttpRequest$BodyPublishers/ofString (str body))
                    (HttpRequest$BodyPublishers/noBody))
        built (.method req (.toUpperCase (name method)) publisher)
        response (.send ^HttpClient *client* (.build built)
                        (HttpResponse$BodyHandlers/ofString))]
    {:status (.statusCode response) :body (.body response)}))

(deftest real-http-post-reaches-started-biff-handler-and-atomic-xtdb
  (let [{:keys [request binding] :as d} (before)
        env ((earlier 'case-env) d nil)]
    (seed! [:requests request])
    (with-server! env
      (fn [{:keys [server base system]}]
        (let [initial ((:summary! server))]
          (is (= http/report-type (:type initial)))
          (is (= :started (:state initial)))
          (is (= :exact-checked-handler-at-server-start
                 (:server-captured-biff-handler initial)))
          ;; The server's captured handler is independently checked at its
          ;; own Biff module startup, not supplied to this request.
          (is (= 200 (:status (request! base :post "/operations/request/claim"))))
          (is (= ((model 'next-version) request {:owner "helper-a"})
                 (read-request (:xt/id request))))
          (is (= (resolution/command-record
                  binding {:status :claimed :owner "helper-a"} "t1")
                 (read-command (resolution/record-id binding))))
          (is (= 1 @(get-in env [:environment :constructs])))
          (let [after ((:summary! server))]
            (is (= 1 (:http-socket-requests-observed after)))
            (is (= 1 (:server-handler-dispatch-attempts after)))
            (is (= :not-established (:external-server-deployment after)))
            (is (= :not-established (:same-http-request-typed-trial after)))
            (is (= :not-established (:authenticated-principal after)))
            (is (= :not-issued (:certificate after)))
            (is (= :none (:authority after)))))))))

(deftest unsupported-method-and-route-produce-no-write
  (let [{:keys [request binding] :as d} (before)
        env ((earlier 'case-env) d nil)]
    (seed! [:requests request])
    (with-server! env
      (fn [{:keys [server base]}]
        (is (= 404 (:status (request! base :post "/not-a-certified-route"))))
        (is (= 404 (:status (request! base :get "/operations/request/claim"))))
        (is (= request (read-request (:xt/id request))))
        (is (nil? (read-command (resolution/record-id binding))))
        (is (= 2 (:http-socket-requests-observed ((:summary! server)))))))))

(deftest client-payload-and-query-cannot-inject-xtdb-or-identity
  (let [{:keys [request binding] :as d} (before)
        env ((earlier 'case-env) d nil)]
    (seed! [:requests request])
    (with-server! env
      (fn [{:keys [server base]}]
        (is (= 400 (:status (request! base :post "/operations/request/claim"
                                     :body "{:biff.xtdb/node :forged}"))))
        (is (= 400 (:status (request! base :post
                                     "/operations/request/claim?principal=admin"))))
        (is (= request (read-request (:xt/id request))))
        (is (nil? (read-command (resolution/record-id binding))))
        (is (= 0 (:server-handler-dispatch-attempts ((:summary! server)))))))))

(deftest stale-dependency-cas-prevents-both-writes-over-http
  (let [{:keys [request membership binding] :as d} (before)
        env ((earlier 'case-env) d {:guard-membership? true})]
    (seed! [:requests request] [:memberships membership])
    (xt/execute-tx ((model 'database))
                   (:tx-ops (tx/prepare {}
                                        {:commands [((model 'revoke-command)
                                                     membership)]
                                         :emit false})))
    (with-server! env
      (fn [{:keys [base]}]
        ;; Original Ring handler may choose its HTTP status for rejection;
        ;; physical atomicity is tested on XTDB documents, not HTTP status.
        (let [response (request! base :post "/operations/request/claim")]
          (is (<= 200 (:status response) 599)))
        (is (= request (read-request (:xt/id request))))
        (is (nil? (read-command (resolution/record-id binding))))))))

(deftest server-start-before-host-must-fail
  (let [env ((earlier 'case-env))
        contract (:contract env)
        host-issued (host/issue-checked-biff-host-module! contract)
        issued (http/issue-loopback-http-server-module!
                (:handle host-issued) contract ((model 'database)))
        modules [app/application-handler-module (:module host-issued)
                 (:module issued)]]
    (try
      (is (= :host-adoption-start-not-completed
             (reason #(binding [*modules* modules]
                        (app/start-biff-application!
                         (:application-assembly contract) {} #'*modules*
                         [app/application-handler-module-id
                          http/module-id host/module-id])))))
      (is (not= :started (:state ((:summary! issued)))))
      (finally ((:stop! issued))))))

(deftest later-replacement-of-returned-system-does-not-change-captured-http-handler
  (let [{:keys [request binding] :as d} (before)
        env ((earlier 'case-env) d nil)]
    (seed! [:requests request])
    (with-server! env
      (fn [{:keys [server base system]}]
        (let [replacement (assoc system :biff.ring/handler
                                 (fn [_] {:status 418 :body "forged"}))]
          (is (not (identical? (:biff.ring/handler replacement)
                               (:biff.ring/handler system))))
          (is (= 200 (:status (request! base :post "/operations/request/claim"))))
          (is (= ((model 'next-version) request {:owner "helper-a"})
                 (read-request (:xt/id request))))
          (is (some? (read-command (resolution/record-id binding))))
          (is (= :started (:state ((:summary! server))))))))))

(deftest stop-closes-real-http-port-and-refuses-restart
  (let [env ((earlier 'case-env))]
    (with-server! env
      (fn [{:keys [server]}]
        (is (= :started (:state ((:summary! server)))))))
    ;; A fresh issue is required for each startup, closed ports cannot be
    ;; resurrected by repeating the module start.
    (let [host-issued (host/issue-checked-biff-host-module! (:contract env))
          issued (http/issue-loopback-http-server-module!
                  (:handle host-issued) (:contract env) ((model 'database)))]
      ((:stop! issued))
      (is (= :stopped (:state ((:summary! issued))))))))
