(ns gesso.assurance.example.certified-claim-http-boot-test
  "v1785: Concrete claim-example boot owns v1784 sealed startup; its normal
   boot!, not a separately invoked proving helper, starts the real HTTP
   adapter and processes the typed/guarded XTDB domain+ledger transaction."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.example.certified-claim-http-boot :as boot]
            [gesso.assurance.proof.checked-biff-http-application-configuration :as configured]
            [gesso.assurance.proof.checked-biff-http-consumption-probe-test]
            [gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test
             :as previous]
            [gesso.live.application-preflight.fixture :as fixture]
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
(def ^:private adapter-ns
  'gesso.assurance.proof.checked-biff-http-consumption-probe-test)
(def ^:private model-ns 'gesso.model.resolved-operation-test)
(defn- helper [n s]
  (var-get (or (ns-resolve n s)
               (throw (ex-info "Missing original test fixture"
                               {:ns n :name s})))))
(defn- model [s] (helper model-ns s))
(defn- previous-fixture [s] (helper previous-ns s))
(def ^:dynamic *client* nil)
(use-fixtures :once
  (fn [run]
    ((model 'with-real-xtdb)
     (fn []
       (fixture/with-closed-application
        {:operations {:request/claim
                      (fixture/trusted-operation
                       :request/claim {:published-change-topics #{}})}}
        (fn [{:keys [assembly]}]
          (binding [previous/*assembly* assembly
                    *client* (-> (HttpClient/newBuilder)
                                 (.connectTimeout (Duration/ofSeconds 5))
                                 (.build))]
            (run))))))))

(defn- environment
  ([] (environment nil :honest))
  ([options mode]
   (let [d ((model 'fixture-data))
         env ((previous-fixture 'case-env) d options)
         state (atom nil)
         adapter ((helper adapter-ns 'adapter-module) mode state)
         source {:contract-source (:contract env)
                 :catalog-source (:catalog-source env)
                 :inputs {:input/owner "helper-a"}
                 :adapter-module adapter
                 :base-url! (fn [_]
                              (str "http://127.0.0.1:" (:port @state)))}]
     {:env env :data d :server-state state :adapter adapter
      :source source})))
(defn- seed! [& xs] (apply (model 'seed!) xs))
(defn- read-request [id] ((model 'read-request) id))
(defn- read-command [id] ((model 'read-command) id))
(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))
(defn- request! [env]
  (let [port (:port @(:server-state env))
        req (-> (HttpRequest/newBuilder
                 (URI/create (str "http://127.0.0.1:" port
                                  "/operations/request/claim")))
                (.timeout (Duration/ofSeconds 6))
                (.POST (HttpRequest$BodyPublishers/noBody))
                (.build))]
    (.statusCode (.send ^HttpClient *client* req
                        (HttpResponse$BodyHandlers/ofString)))))
(defn- cleanup! [env issued]
  (when issued ((:stop! issued)))
  ;; The existing selected test adapter has idempotent cleanup. Always
  ;; request it even if startup itself threw or Biff never returned a system.
  ((:biff.core/stop (:adapter env)) {}))

(deftest ordinary-example-boot-executes-real-http-catalog-and-atomic-ledger
  (let [{:keys [data source env] :as e} (environment)
        {:keys [request binding]} data
        issued (boot/issue-claim-http-boot! source)]
    (seed! [:requests request])
    (try
      (is (= :issued (:status ((:summary! issued)))))
      (let [r ((:boot! issued))]
        (is (= boot/report-type (:type r)))
        (is (= :verified-configured-start (:status r)))
        (is (= :concrete-claim-app-configured-closed-start (:boot-entry r)))
        (is (= :derived-only-by-v1784 (:module-inventory r)))
        (is (= :verified-during-actual-application-boot
               (:local-http-adapter-consumption r)))
        (is (= :not-established (:deployed-server-identity r)))
        (is (= :not-issued (:certificate r)))
        (is (= :none (:authority r) (:retry-authority r)))
        (is (not-any? #(contains? r %)
                      [:modules :system :adapter :handler :contract :catalog
                       :request :executor :tx-ops :receipt :node])))
      (is (= request (read-request (:xt/id request))))
      (is (nil? (read-command (resolution/record-id binding))))
      (is (= 200 (request! e)))
      (is (= ((model 'next-version) request {:owner "helper-a"})
             (read-request (:xt/id request))))
      (is (= (resolution/command-record
              binding {:status :claimed :owner "helper-a"} "t1")
             (read-command (resolution/record-id binding))))
      (is (= 1 @(get-in env [:environment :constructs])))
      (is (= 500 (request! e)))
      (is (= 1 @(get-in env [:environment :constructs])))
      (finally (cleanup! e issued)))
    (is (= :stopped (:status ((:summary! issued)))))))

(deftest boot-entry-does-not-accept-biff-module-or-handler-overrides
  (let [{:keys [source server-state]} (environment)]
    (doseq [extra [{:modules [:raw/server]}
                   {:start-order [:raw/server]}
                   {:initial-system {:biff.ring/handler (fn [_] {:status 200})}}
                   {:alternate-executor! (fn [] :fake)}
                   {:typed-trial {:submit! (fn [] :fake)}}]]
      (is (= :invalid-claim-example-boot-source
             (reason #(boot/issue-claim-http-boot! (merge source extra))))))
    (is (nil? @server-state))))

(deftest boot-entry-is-closed-to-wrong-route-or-additional-operation
  (let [{:keys [source server-state]} (environment)
        wrong (assoc-in source [:contract-source :certified-operations
                                :request/cancel]
                        (get-in source [:contract-source :certified-operations
                                         :request/claim]))]
    (is (= :claim-certified-contract-not-checked
           (reason #(boot/issue-claim-http-boot! wrong))))
    (let [wrong-catalog (assoc-in source [:catalog-source :catalog
                                          :declared-operation-ids]
                                  #{:request/cancel})]
      (is (= :claim-original-catalog-not-checked
             (reason #(boot/issue-claim-http-boot! wrong-catalog)))))
    (is (nil? @server-state))))

(deftest rejected-application-adapter-never-reports-successful-boot
  (let [{:keys [source server-state] :as e} (environment nil :ignored)
        issued (boot/issue-claim-http-boot! source)]
    (try
      (is (= :adapter-start-http-consumption-unverified
             (reason (:boot! issued))))
      (is (= :start-failed-physical-state-unknown
             (:status ((:summary! issued)))))
      (is (.isShutdown (:executor @server-state)))
      (is (= :configured-start-already-attempted (reason (:boot! issued))))
      (finally (cleanup! e issued)))))

(deftest wrong-typed-input-is-rejected-before-domain-or-ledger-write
  (let [{:keys [data source] :as e} (environment)
        {:keys [request binding]} data
        issued (boot/issue-claim-http-boot!
                (assoc source :inputs {:input/owner "wrong-owner"}))]
    (seed! [:requests request])
    (try
      (is (= :verified-configured-start (:status ((:boot! issued)))))
      (is (= 500 (request! e)))
      (is (= request (read-request (:xt/id request))))
      (is (nil? (read-command (resolution/record-id binding))))
      (finally (cleanup! e issued)))))

(deftest stale-guard-blocks-atomic-domain-and-ledger-over-application-boot
  (let [{:keys [data source] :as e} (environment {:guard-membership? true} :honest)
        {:keys [request membership binding]} data
        issued (boot/issue-claim-http-boot! source)]
    (seed! [:requests request] [:memberships membership])
    (xt/execute-tx ((model 'database))
                   (:tx-ops (tx/prepare {}
                                        {:commands [((model 'revoke-command)
                                                     membership)]
                                         :emit false})))
    (try
      (is (= :verified-configured-start (:status ((:boot! issued)))))
      (is (= 500 (request! e)))
      (is (= request (read-request (:xt/id request))))
      (is (nil? (read-command (resolution/record-id binding))))
      (finally (cleanup! e issued)))))

(deftest unknown-acknowledgment-never-authorizes-request-retry
  (let [{:keys [data source] :as e} (environment)
        {:keys [request binding]} data
        issued (boot/issue-claim-http-boot! source)
        original tx/transact!
        calls (atom 0)]
    (seed! [:requests request])
    (try
      ((:boot! issued))
      (with-redefs [tx/transact! (fn [ctx plan]
                                  (swap! calls inc)
                                  (original ctx plan)
                                  (throw (ex-info "lost ack" {})))]
        (is (= 500 (request! e))))
      (is (= 1 @calls))
      (is (some? (read-command (resolution/record-id binding))))
      (is (= 500 (request! e)))
      (is (= 1 @calls))
      (is (= :none (:retry-authority ((:summary! issued)))))
      (finally (cleanup! e issued)))))

(deftest example-boot-can-be-stopped-before-start-but-not-resurrected
  (let [{:keys [source server-state] :as e} (environment)
        issued (boot/issue-claim-http-boot! source)]
    (try
      (is (= :stopped-without-successful-start (:status ((:stop! issued)))))
      (is (= :configured-start-stopped-before-use (reason (:boot! issued))))
      (is (nil? @server-state))
      (finally (cleanup! e issued)))))

(deftest example-boot-start-is-one-use-and-stop-idempotent
  (let [{:keys [source] :as e} (environment)
        issued (boot/issue-claim-http-boot! source)]
    (try
      (is (= :verified-configured-start (:status ((:boot! issued)))))
      (is (= :configured-start-already-attempted (reason (:boot! issued))))
      (is (= :stopped (:status ((:stop! issued)))))
      (is (= :stopped (:status ((:stop! issued)))))
      (finally (cleanup! e issued)))))

(deftest substituted-configured-start-issuer-blocked-before-http-init
  (let [{:keys [source server-state] :as e} (environment)
        issued (boot/issue-claim-http-boot! source)]
    (try
      (with-redefs [configured/issue-configured-checked-http-application!
                    (fn [& _] {:boot! (fn [] {:status :forged})})]
        (is (= :substituted-original-claim-boot-boundary
               (reason (:boot! issued)))))
      (is (nil? @server-state))
      (finally (cleanup! e issued)))))
