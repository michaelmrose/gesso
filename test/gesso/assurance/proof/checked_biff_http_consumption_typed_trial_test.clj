(ns gesso.assurance.proof.checked-biff-http-consumption-typed-trial-test
  "v1781: A selected independently started Biff HTTP adapter first passes
   the final-wrapper challenge, then routes ONE real HTTP POST through the
   same wrapper and v1777 catalog-bound canonical typed atomic execution.
   The two HTTP requests are distinct; positive evidence is source-relative
   and local to the chosen adapter, not production route attestation."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.checked-biff-http-consumption-probe :as probe]
            [gesso.assurance.proof.checked-biff-http-consumption-probe-test]
            [gesso.assurance.proof.checked-biff-host-adoption :as host]
            [gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test
             :as previous]
            [gesso.live.application-preflight :as app]
            [gesso.live.application-preflight.fixture :as app-fixture]
            [gesso.model.resolved-operation-test]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.tx :as tx]
            [com.biffweb.core :as biff]
            [xtdb.api :as xt])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]
           [java.time Duration]))

(def ^:private older-ns
  'gesso.assurance.proof.checked-biff-http-consumption-probe-test)
(def ^:private prior-ns
  'gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test)
(def ^:private model-ns 'gesso.model.resolved-operation-test)
(defn- fixture [ns-name sym]
  (var-get (or (ns-resolve ns-name sym)
               (throw (ex-info "Missing test fixture" {:ns ns-name :symbol sym})))))
(defn- model [sym] (fixture model-ns sym))
(defn- earlier [sym] (fixture prior-ns sym))
(defn- original-adapter [] (fixture older-ns 'adapter-module))
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

(defn- data [] ((model 'fixture-data)))
(defn- seed! [& items] (apply (model 'seed!) items))
(defn- read-request [id] ((model 'read-request) id))
(defn- read-command [id] ((model 'read-command) id))
(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- with-server!
  ([env mode f] (with-server! env mode nil nil f))
  ([env mode options start-order f]
   (let [contract (:contract env)
         adopted (host/issue-checked-biff-host-module! contract)
         trial-options (merge {:catalog-source (:catalog-source env)
                               :operation :request/claim
                               :inputs {:input/owner "helper-a"}}
                              options)
         probe-issued (probe/issue-http-consumption-probe-module!
                       (:handle adopted) contract trial-options)
         server-state (atom nil)
         adapter ((original-adapter) mode server-state)
         modules [app/application-handler-module (:module adopted)
                  (:module probe-issued) adapter]
         order (or start-order [app/application-handler-module-id
                                host/module-id probe/module-id
                                (:biff.core/id adapter)])
         system (atom nil)]
     (try
       (reset! system (binding [*modules* modules]
                        (app/start-biff-application!
                         (:application-assembly contract) {} #'*modules* order)))
       (f {:env env :system @system :probe probe-issued
           :base (str "http://127.0.0.1:" (:port @server-state))})
       (finally
         (when-let [started @system]
           (try (biff/stop started) (catch Throwable _ nil)))
         (when-let [{:keys [server executor]} @server-state]
           (try (.stop server 0) (catch Throwable _ nil))
           (.shutdownNow executor)))))))

(defn- post! [base]
  (let [req (-> (HttpRequest/newBuilder
                 (URI/create (str base "/operations/request/claim")))
                (.timeout (Duration/ofSeconds 6))
                (.POST (HttpRequest$BodyPublishers/noBody))
                (.build))]
    (.statusCode (.send (HttpClient/newHttpClient) req
                        (HttpResponse$BodyHandlers/ofString)))))

(deftest selected-adapter-challenge-and-typed-atomic-http-request-correspond
  (let [{:keys [request binding] :as d} (data)
        env ((earlier 'case-env) d nil)]
    (seed! [:requests request])
    (with-server! env :honest
      (fn [{:keys [system probe base]}]
        (is (= 503 (post! base)) "No canonical write before adapter consumption challenge")
        (is (= request (read-request (:xt/id request))))
        (is (= 0 (:typed-trial-attempts ((:summary! probe)))))
        (is (= :one-final-checked-wrapper-http-challenge-returned
               (:status ((:verify-once! probe) system base))))
        (is (= 200 (post! base)))
        (let [r ((:summary! probe))]
          (is (= probe/report-type (:type r)))
          (is (= :one-selected-tested-route-through-final-wrapper
                 (:normal-http-route-consumption r)))
          (is (= :one-original-catalog-atomic-ack-in-selected-wrapper-call
                 (:same-request-typed-catalog-execution r)))
          (is (= :bounded-challenge-and-one-typed-canonical-execution
                 (:selected-adapter-typed-atomic-correspondence r)))
          (is (= :bounded-biff-issued-catalog-atomic-acknowledgment
                 (:typed-trial-last-outcome r)))
          (is (= 1 (:typed-trial-attempts r)))
          (is (= 1 (:challenge-handler-observations r)))
          (is (= :not-established (:external-server-deployment r)))
          (is (= :not-established (:xtdb-commit-authenticity r)))
          (is (= :not-issued (:certificate r)))
          (is (= :none (:transaction-authority r) (:retry-authority r)))
          (is (not-any? #(contains? r %)
                        [:token :secret :handler :catalog :request :response
                         :source :tx-ops :receipt :executor :node :principal])))
        (is (= ((model 'next-version) request {:owner "helper-a"})
               (read-request (:xt/id request))))
        (is (= (resolution/command-record
                binding {:status :claimed :owner "helper-a"} "t1")
               (read-command (resolution/record-id binding))))
        (is (= 1 @(get-in env [:environment :constructs])))
        (is (= 500 (post! base)) "Trial is one-use: no second write")
        (is (= 1 @(get-in env [:environment :constructs])))))))

(deftest ignored-adapter-cannot-qualify-typed-route-or-challenge
  (let [{:keys [request binding] :as d} (data)
        env ((earlier 'case-env) d nil)]
    (seed! [:requests request])
    (with-server! env :ignored
      (fn [{:keys [system probe base]}]
        (is (= :http-consumption-challenge-unconfirmed
               (:status ((:verify-once! probe) system base))))
        ;; Fabricated HTTP 200 remains insufficient even on operation path.
        (is (= 200 (post! base)))
        (let [r ((:summary! probe))]
          (is (= :not-established (:selected-adapter-typed-atomic-correspondence r)))
          (is (= 0 (:typed-trial-attempts r))))
        (is (= request (read-request (:xt/id request))))
        (is (nil? (read-command (resolution/record-id binding))))))))

(deftest stale-adapter-can-execute-original-but-cannot-qualify-combined-proof
  (let [{:keys [request binding] :as d} (data)
        env ((earlier 'case-env) d nil)
        order [app/application-handler-module-id host/module-id
               :gesso.assurance.proof.checked-biff-http-consumption-probe-test/adapter
               probe/module-id]]
    (seed! [:requests request])
    (with-server! env :honest nil order
      (fn [{:keys [system probe base]}]
        (is (= :http-consumption-challenge-unconfirmed
               (:status ((:verify-once! probe) system base))))
        ;; An independent server that captured an EARLIER handler can still
        ;; perform a write. The local observation cannot claim to cover it.
        (is (= 200 (post! base)))
        (is (some? (read-command (resolution/record-id binding))))
        (is (= :not-established
               (:selected-adapter-typed-atomic-correspondence ((:summary! probe)))))))))

(deftest selected-adapter-must-pass-challenge-before-domain-submission
  (let [{:keys [request binding] :as d} (data)
        env ((earlier 'case-env) d nil)]
    (seed! [:requests request])
    (with-server! env :honest
      (fn [{:keys [probe base]}]
        (is (= 503 (post! base)))
        (is (= 503 (post! base)))
        (is (= request (read-request (:xt/id request))))
        (is (nil? (read-command (resolution/record-id binding))))
        (is (= 0 (:typed-trial-attempts ((:summary! probe)))))))))

(deftest bad-catalog-input-fails-closed-before-atomic-xtdb-write
  (let [{:keys [request binding] :as d} (data)
        env ((earlier 'case-env) d nil)]
    (seed! [:requests request])
    (with-server! env :honest {:inputs {:input/owner "wrong-owner"}} nil
      (fn [{:keys [system probe base]}]
        (is (= :one-final-checked-wrapper-http-challenge-returned
               (:status ((:verify-once! probe) system base))))
        (is (= 500 (post! base)))
        (is (= :not-established
               (:selected-adapter-typed-atomic-correspondence ((:summary! probe)))))
        (is (= request (read-request (:xt/id request))))
        (is (nil? (read-command (resolution/record-id binding))))))))

(deftest stale-dependency-cas-blocks-both-records-after-http-challenge
  (let [{:keys [request membership binding] :as d} (data)
        env ((earlier 'case-env) d {:guard-membership? true})]
    (seed! [:requests request] [:memberships membership])
    (xt/execute-tx ((model 'database))
                   (:tx-ops (tx/prepare {}
                                        {:commands [((model 'revoke-command) membership)]
                                         :emit false})))
    (with-server! env :honest
      (fn [{:keys [system probe base]}]
        (is (= :one-final-checked-wrapper-http-challenge-returned
               (:status ((:verify-once! probe) system base))))
        (is (= 500 (post! base)))
        (is (= :not-established
               (:selected-adapter-typed-atomic-correspondence ((:summary! probe)))))
        (is (= request (read-request (:xt/id request))))
        (is (nil? (read-command (resolution/record-id binding))))))))

(deftest lost-acknowledgment-is-unqualified-and-never-retried
  (let [{:keys [request binding] :as d} (data)
        env ((earlier 'case-env) d nil)
        original tx/transact!
        transactions (atom 0)]
    (seed! [:requests request])
    (with-server! env :honest
      (fn [{:keys [system probe base]}]
        (is (= :one-final-checked-wrapper-http-challenge-returned
               (:status ((:verify-once! probe) system base))))
        (with-redefs [tx/transact! (fn [ctx plan]
                                    (swap! transactions inc)
                                    (original ctx plan)
                                    (throw (ex-info "ack lost" {})))]
          (is (= 500 (post! base))))
        (is (= 1 @transactions))
        (is (= :not-established
               (:selected-adapter-typed-atomic-correspondence ((:summary! probe)))))
        (is (some? (read-command (resolution/record-id binding))))
        (is (= 500 (post! base)))
        (is (= 1 @transactions))
        (is (= :none (:retry-authority ((:summary! probe)))))))))
