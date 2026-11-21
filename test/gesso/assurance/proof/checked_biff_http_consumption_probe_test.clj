(ns gesso.assurance.proof.checked-biff-http-consumption-probe-test
  "v1805: A separate Biff server adapter must actually consume the checked
   FINAL Ring handler over a real HTTP loopback socket, not just accept a
   copied Biff system or return a synthetic success status."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.string :as str]
            [gesso.assurance.proof.checked-biff-http-consumption-probe :as probe]
            [gesso.assurance.proof.checked-biff-host-adoption :as host]
            [gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test
             :as previous]
            [gesso.live.application-preflight :as app]
            [gesso.live.application-preflight.fixture :as app-fixture]
            [gesso.model.resolved-operation-test]
            [gesso.model.command-resolution :as resolution]
            [com.biffweb.core :as biff])
  (:import [com.sun.net.httpserver HttpHandler HttpServer]
           [java.net InetSocketAddress URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]
           [java.nio.charset StandardCharsets]
           [java.time Duration]
           [java.util.concurrent Executors TimeUnit]))

(def ^:private earlier 'gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test)
(def ^:private model-ns 'gesso.model.resolved-operation-test)
(defn- helper [n s]
  (var-get (or (ns-resolve n s)
               (throw (ex-info "Missing existing fixture" {:ns n :symbol s})))))
(defn- model [s] (helper model-ns s))
(defn- earlier-helper [s] (helper earlier s))
(def ^:dynamic *modules* [])
;; Test-only hook for suspending the real HTTP adapter after its final
;; issued handler returns, but before its response reaches verify-once!.
(def ^:dynamic *after-http-handler!* nil)
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

(defn- error-reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
(deftest direct-http-probe-refuses-disabled-declared-principal-gate
  ;; The lower-level consumption probe is separately public. It cannot trust
  ;; the configured-start entry point to have validated an optional policy.
  (let [contract (:contract ((earlier-helper 'case-env)))
        host (host/issue-checked-biff-host-module! contract)]
    (is (map? (:module (probe/issue-http-consumption-probe-module!
                       (:handle host) contract nil)))
        "A deliberately absent typed trial remains supported")
    (doseq [disabled [nil false]]
      (is (= :invalid-typed-adapter-trial-source
             (error-reason
              #(probe/issue-http-consumption-probe-module!
                (:handle host) contract
                {:catalog-source {:catalog {}}
                 :operation :request/claim
                 :inputs {}
                 :principal-policy disabled
                 :authorization-source {}})))))))

(defn- data [] ((model 'fixture-data)))
(defn- seed! [& xs] (apply (model 'seed!) xs))
(defn- read-request [id] ((model 'read-request) id))
(defn- read-command [id] ((model 'read-command) id))

(defn- adapter-module [mode server-state]
  {:biff.core/id :gesso.assurance.proof.checked-biff-http-consumption-probe-test/adapter
   :biff.core/start
   (fn [system]
     (let [real-handler (:biff.ring/handler system)
           response-hook *after-http-handler!*
           served-handler (case mode
                            :honest real-handler
                            :ignored (fn [_] {:status 200 :body "fake 200"})
                            :different (fn [_] {:status 404 :body "ignored"}))
           server (HttpServer/create (InetSocketAddress. "127.0.0.1" (int 0)) 0)
           executor (Executors/newSingleThreadExecutor)
           context (reify HttpHandler
                     (handle [_ exchange]
                       (try
                         (let [method (keyword (str/lower-case (.getRequestMethod exchange)))
                               uri (.getRequestURI exchange)
                               headers (into {}
                                             (for [[k values] (.entrySet (.getRequestHeaders exchange))]
                                               [(str/lower-case k) (first values)]))
                               response (served-handler
                                         {:request-method method
                                          :uri (.getPath uri)
                                          :headers headers
                                          :biff.xtdb/node ((model 'database))})
                               _ (when response-hook
                                   (response-hook headers response))
                               status (or (:status response) 500)
                               bytes (.getBytes (str (:body response))
                                                StandardCharsets/UTF_8)]
                           (.sendResponseHeaders exchange (int status) (alength bytes))
                           (with-open [out (.getResponseBody exchange)]
                             (.write out bytes)))
                         (catch Throwable _
                           (try (.sendResponseHeaders exchange (int 500) (long -1))
                                (catch Throwable _ nil)))
                         (finally (.close exchange)))))]
       (.createContext server "/" context)
       (.setExecutor server executor)
       (.start server)
       (reset! server-state {:server server :executor executor
                             :port (.getPort (.getAddress server))})
       system))
   :biff.core/stop
   (fn [_]
     (when-let [{:keys [^HttpServer server executor]} @server-state]
       (.stop server 0)
       (.shutdownNow executor)
       (.awaitTermination executor 3 TimeUnit/SECONDS)))})

(defn- with-adapter!
  ([mode start-order f] (with-adapter! mode start-order nil f))
  ([mode start-order typed-config f]
  (let [env ((earlier-helper 'case-env))
        contract (:contract env)
        adopted (host/issue-checked-biff-host-module! contract)
        probe-issued (probe/issue-http-consumption-probe-module!
                      (:handle adopted) contract typed-config)
        server-state (atom nil)
        adapter (adapter-module mode server-state)
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
      (let [port (:port @server-state)]
        (f {:env env :system @system :probe probe-issued
            :base (str "http://127.0.0.1:" port)
            :server-state server-state}))
      (finally
        (when-let [started @system]
          (try (biff/stop started) (catch Throwable _ nil)))
        (when-let [{:keys [^HttpServer server executor]} @server-state]
          (try (.stop server 0) (catch Throwable _ nil))
          (.shutdownNow executor)))))))

(defn- post! [base]
  (let [request (-> (HttpRequest/newBuilder
                     (URI/create (str base "/operations/request/claim")))
                    (.timeout (Duration/ofSeconds 6))
                    (.POST (HttpRequest$BodyPublishers/noBody))
                    (.build))
        client (HttpClient/newHttpClient)]
    (.statusCode (.send client request (HttpResponse$BodyHandlers/ofString)))))

(deftest unchecked-http-wrapper-fences-business-before-activation
  ;; Biff's real socket starts before the local loopback challenge completes.
  ;; A retained unqualified handler must not mutate durable state during
  ;; that installation gap, even when the typed trial was not selected.
  (with-adapter! :honest nil
    (fn [{:keys [env probe system base]}]
      (let [{:keys [request binding]} (:data env)]
        (seed! [:requests request])
        (is (= :not-attempted (:status ((:summary! probe)))))
        (is (= 503 (post! base)))
        (is (= request (read-request (:xt/id request))))
        (is (nil? (read-command (resolution/record-id binding))))
        (is (zero? @(get-in env [:environment :constructs])))))))

(deftest stopped-http-wrapper-cannot-execute-retained-business-handler
  ;; Physical listener cleanup can lag the probe module's retirement.
  ;; A listener still holding the issued handler must not get another
  ;; protected write after its probe generation has been stopped.
  (with-adapter! :honest nil
    (fn [{:keys [env probe system base]}]
      (let [{:keys [request binding]} (:data env)]
        (seed! [:requests request])
        (is (= :one-final-checked-wrapper-http-challenge-returned
               (:status ((:verify-once! probe) system base))))
        ;; Stop ONLY the probe, deliberately leaving the actual HTTP
        ;; listener alive to exercise the retained issued Ring wrapper.
        ((:biff.core/stop (:module probe)) system)
        (is (= :stopped (:module-state ((:summary! probe)))))
        (is (= 503 (post! base)))
        (is (= request (read-request (:xt/id request))))
        (is (nil? (read-command (resolution/record-id binding))))
        (is (zero? @(get-in env [:environment :constructs])))))))

(deftest probe-retirement-waits-for-in-flight-business-dispatch
  ;; v2071: The v2070 entry fence is not a retirement barrier if a request
  ;; already passed it. Pause the real Claim handler before its XTDB write,
  ;; initiate probe shutdown, then require stop to wait for that accepted
  ;; handler. A listener retaining the wrapper remains alive throughout.
  (with-adapter! :honest nil
    (fn [{:keys [env probe system base]}]
      (let [{:keys [request binding]} (:data env)
            constructs (get-in env [:environment :constructs])
            entered (promise)
            release (promise)
            stop-started (promise)
            watch-key ::pause-before-claim-write]
        (seed! [:requests request])
        (is (= :one-final-checked-wrapper-http-challenge-returned
               (:status ((:verify-once! probe) system base))))
        ;; This is the real plan's construction atom. Its watch runs on the
        ;; executing HTTP thread, after dispatch but BEFORE the DB mutation.
        (add-watch constructs watch-key
                   (fn [_ _ _ next]
                     (when (= 1 next)
                       (deliver entered true)
                       (deref release 20000 :timed-out))))
        (try
          (let [business (future (post! base))]
            (try
              (is (true? (deref entered 15000 false))
                  "The real business handler must reach the execution barrier")
              (is (= request (read-request (:xt/id request))))
              (is (nil? (read-command (resolution/record-id binding))))
              (let [stopping (future
                               (deliver stop-started true)
                               ((:biff.core/stop (:module probe)) system)
                               :retired)]
                (is (true? (deref stop-started 5000 false)))
                (is (= :in-flight (deref stopping 400 :in-flight))
                    "Stop must not return while an admitted handler can still write")
                (deliver release :continue)
                (is (= 200 (deref business 20000 :timed-out)))
                (is (= :retired (deref stopping 20000 :timed-out)))
                (is (= ((model 'next-version) request {:owner "helper-a"})
                       (read-request (:xt/id request))))
                (is (some? (read-command (resolution/record-id binding))))
                (is (= 503 (post! base))
                    "The retained listener must still deny new business"))
              (finally (deliver release :continue)
                       (deref business 20000 :timed-out))))
          (finally (remove-watch constructs watch-key)))))))

(deftest probe-retirement-drains-in-flight-http-activation
  ;; The first real HTTP challenge can pass through the final handler while
  ;; the response is still delayed by the independent server adapter. The
  ;; retired probe must never subsequently report a new successful startup.
  (let [entered (promise)
        release (promise)]
    (binding [*after-http-handler!*
              (fn [headers response]
                (when (contains? headers "x-gesso-consumption-probe")
                  (deliver entered (= 200 (:status response)))
                  (deref release 15000 :timed-out)))]
      (with-adapter! :honest nil
        (fn [{:keys [probe system base]}]
          (let [verifying (future ((:verify-once! probe) system base))]
            (try
              (is (true? (deref entered 12000 false))
                  "The original startup challenge must be pending on the wire")
              (let [stop-started (promise)
                    stopping (future
                               (deliver stop-started true)
                               ((:biff.core/stop (:module probe)) system)
                               :retired)]
                (is (true? (deref stop-started 5000 false)))
                (is (= :in-flight (deref stopping 350 :in-flight))
                    "Stop must wait for the in-flight activation verifier")
                (deliver release :continue)
                (let [verification (deref verifying 12000 :timed-out)]
                  (is (map? verification))
                  (is (= :stopped (:module-state verification)))
                  (is (not= :one-final-checked-wrapper-http-challenge-returned
                            (:status verification))
                      "Retired verification must never publish successful startup"))
                (is (= :retired (deref stopping 12000 :timed-out)))
                (is (= 503 (post! base))
                    "Business remains fenced after retirement"))
              (finally
                (deliver release :continue)
                (deref verifying 12000 :timed-out)))))))))

(deftest independent-server-consumes-the-final-biff-handler-over-real-http
  (with-adapter! :honest nil
    (fn [{:keys [env probe system base]}]
      (let [d (:data env)
            request (:request d)
            binding (:binding d)]
        (seed! [:requests request])
        (is (= :not-attempted (:status ((:summary! probe)))))
        (let [proof ((:verify-once! probe) system base)]
          (is (= probe/report-type (:type proof)))
          (is (= :one-final-checked-wrapper-http-challenge-returned (:status proof)))
          (is (= :observed-on-selected-loopback-adapter
                 (:final-checked-handler-consumed-over-http proof)))
          (is (= 1 (:challenge-handler-observations proof)))
          (is (= :not-established (:normal-http-route-consumption proof)))
          (is (= :not-established (:external-server-deployment proof)))
          (is (= :not-established (:same-request-typed-catalog-execution proof)))
          (is (= :not-established (:deployed-server-identity proof)))
          (is (= :none (:authority proof) (:retry-authority proof)))
          (is (= :not-issued (:certificate proof)))
          (is (not-any? #(contains? proof %)
                        [:request :uri :base-url :secret :nonce :token
                         :handler :system :xtdb-node :tx-ops :receipt])))
        ;; Challenge route is read-only, does not execute the operation.
        (is (= request (read-request (:xt/id request))))
        (is (nil? (read-command (resolution/record-id binding))))
        ;; Nonchallenge calls transparently reach the ORIGINAL checked Ring
        ;; handler as configured by the independent Biff server adapter.
        (is (= 200 (post! base)))
        (is (= ((model 'next-version) request {:owner "helper-a"})
               (read-request (:xt/id request))))
        (is (some? (read-command (resolution/record-id binding))))
        (is (= 1 @(get-in env [:environment :constructs])))
        (is (= :http-consumption-probe-already-attempted
               (error-reason #((:verify-once! probe) system base))))))))

(deftest server-which-ignores-the-final-wrapper-is-not-qualified
  (with-adapter! :ignored nil
    (fn [{:keys [probe system base]}]
      (let [r ((:verify-once! probe) system base)]
        (is (= :http-consumption-challenge-unconfirmed (:status r)))
        (is (= 0 (:challenge-handler-observations r)))
        (is (= :not-established
               (:final-checked-handler-consumed-over-http r)))))))

(deftest independently-started-server-before-probe-captures-stale-handler
  (let [adapter-id :gesso.assurance.proof.checked-biff-http-consumption-probe-test/adapter]
    (with-adapter! :honest [app/application-handler-module-id host/module-id
                             adapter-id probe/module-id]
      (fn [{:keys [probe system base]}]
        ;; Final system contains the probe wrapper, but the server captured
        ;; the EARLIER checked handler before the probe's module ran.
        (is (= :http-consumption-challenge-unconfirmed
               (:status ((:verify-once! probe) system base))))
        (is (zero? (:challenge-handler-observations ((:summary! probe)))))))))

(deftest replacing-returned-system-handler-is-not-deployment-evidence
  (with-adapter! :honest nil
    (fn [{:keys [probe system base]}]
      (is (= :final-biff-system-did-not-retain-probe
             (error-reason #((:verify-once! probe)
                             (assoc system :biff.ring/handler
                                    (fn [_] {:status 200})) base))))
      (is (= :http-consumption-probe-already-attempted
             (error-reason #((:verify-once! probe) system base)))))))

(deftest nonloopback-or-unsafe-target-is-rejected-without-network
  (with-adapter! :honest nil
    (fn [{:keys [probe system]}]
      (is (= :nonloopback-or-unsafe-probe-target
             (error-reason #((:verify-once! probe) system
                             "http://example.com:80"))))
      (is (= :http-consumption-probe-already-attempted
             (error-reason #((:verify-once! probe) system
                             "http://127.0.0.1:1234")))))))

(deftest adapter-that-returns-a-fake-http-200-cannot-fabricate-consumption
  (with-adapter! :ignored nil
    (fn [{:keys [probe system base]}]
      (is (= :http-consumption-challenge-unconfirmed
             (:status ((:verify-once! probe) system base))))
      (is (= :not-established
             (:final-checked-handler-consumed-over-http ((:summary! probe))))))))

(deftest unstarted-probe-and-unsafe-issuance-are-not-qualifying
  (let [env ((earlier-helper 'case-env))
        contract (:contract env)
        adopted (host/issue-checked-biff-host-module! contract)
        issued (probe/issue-http-consumption-probe-module!
                (:handle adopted) contract)]
    (is (= :issued (:module-state ((:summary! issued)))))
    (is (= :final-biff-system-did-not-retain-probe
           (error-reason #((:verify-once! issued)
                           {:biff.ring/handler (fn [_] :fake)}
                           "http://127.0.0.1:1111"))))
    (is (= :invalid-original-host-source
           (error-reason #(probe/issue-http-consumption-probe-module!
                           (host/->HostAdoptionHandle) contract))))))


(deftest final-issued-probe-route-inventory-is-source-bound-and-read-only
  (with-adapter! :honest nil
    (fn [{:keys [env probe system base]}]
      (let [contract (:contract env)
            inspect! (fn [p src sys]
                       (probe/require-current-final-probe-route-inventory!
                        p src sys))]
        ;; Simply starting the Biff modules doesn't prove the adapter uses
        ;; its final handler. The real loopback challenge must complete.
        (is (= :final-probe-wrapper-or-http-challenge-not-current
               (error-reason #(inspect! probe contract system))))
        (is (= :one-final-checked-wrapper-http-challenge-returned
               (:status ((:verify-once! probe) system base))))
        (let [checked (inspect! probe contract system)]
          (is (= probe/final-route-inventory-report-type (:type checked)))
          (is (= probe/report-version (:version checked)))
          (is (= :one-returned-in-process-probe-handler-slot
                 (:scope checked)))
          (is (= :exact-issued-wrapper (:final-probe-handler-slot checked)))
          (is (= :verified-on-selected-loopback-adapter
                 (:one-real-http-consumption-challenge checked)))
          (is (= #{:request/claim} (:checked-certified-route-ids checked)))
          (is (contains? (:checked-route-ids checked) :request/claim))
          (is (= :checked-not-individually-probed
                 (:normal-http-route-inventory checked)))
          (is (= :not-established (:host-deployment checked)))
          (is (= :not-issued (:certification checked)))
          (is (= :none (:authority checked) (:transaction-authority checked)))
          (is (not-any? #(contains? checked %)
                        [:handler :host-system :request :secret :http-token
                         :router :xtdb-node :tx-ops :receipt :certificate-handle])))
        (is (= :final-probe-wrapper-or-http-challenge-not-current
               (error-reason #(inspect! probe contract
                                        (assoc system :biff.ring/handler
                                               (fn [_] {:status 200}))))))
        (is (= :final-probe-wrapper-or-http-challenge-not-current
               (error-reason #(inspect! probe contract
                                        (dissoc system :biff.ring/handler)))))
        (is (= :final-probe-wrapper-or-http-challenge-not-current
               (error-reason #(inspect! probe (assoc contract :false-proof true)
                                        system))))
        (is (= :unissued-or-substituted-http-consumption-probe
               (error-reason #(inspect! (assoc probe :verify-once! (fn [& _] nil))
                                        contract system))))))))


(deftest selected-post-method-path-is-observed-read-only-over-actual-http
  (let [env ((earlier-helper 'case-env))
        config {:catalog-source (:catalog-source env)
                :operation :request/claim
                :inputs {:input/owner "helper-a"}}]
    (with-adapter! :honest nil config
      (fn [{:keys [env probe system base]}]
        (let [{:keys [request binding]} (:data env)
              client (HttpClient/newHttpClient)
              marked-post (fn [method token]
                            (let [req (-> (HttpRequest/newBuilder
                                           (URI/create
                                            (str base "/operations/request/claim")))
                                          (.timeout (Duration/ofSeconds 5))
                                          (.header "x-gesso-operation-route-probe" token)
                                          (.method method
                                                   (HttpRequest$BodyPublishers/noBody))
                                          (.build))]
                              (.statusCode (.send client req
                                                  (HttpResponse$BodyHandlers/ofString)))))]
          (seed! [:requests request])
          ;; Any public attempt with a reserved marker is rejected BEFORE
          ;; business routing, even before the ephemeral challenge is armed.
          (is (= 404 (marked-post "POST" "forged")))
          (is (= 404 (marked-post "GET" "forged")))
          (is (= request (read-request (:xt/id request))))
          (is (nil? (read-command (resolution/record-id binding))))
          (is (= 0 @(get-in env [:environment :constructs])))
          (let [r ((:verify-once! probe) system base)]
            (is (= :one-final-checked-wrapper-http-challenge-returned
                   (:status r)))
            (is (= :one-selected-route-matched-exact-original-handler-read-only
                   (:selected-route-http-reachability r)))
            (is (= 1 (:selected-route-challenge-observations r)))
            (is (= :all-selected-certified-routes-observed-through-original-router
                   (:catalog-read-only-route-dispatch r)))
            (is (= #{:request/claim} (:catalog-expected-certified-route-ids r)))
            (is (= #{:request/claim} (:catalog-observed-certified-route-ids r)))
            (is (= #{} (:catalog-parameterized-witness-route-ids r)))
            (is (= :not-established (:parameterized-route-generalization r)))
            (is (= :one-checked-router-matched-selected-issued-handler
                   (:selected-route-inner-router-dispatch r)))
            (is (= :not-invoked-by-read-only-probe
                   (:selected-route-business-handler-execution r)))
            (is (not-any? #(contains? r %)
                          [:route-secret :route-response :selected-route
                           :route-probe-header :request :uri :token])))
          ;; The actual POST path was challenged, but NEVER passed through
          ;; the declared operation handler or the XTDB transaction path.
          (is (= request (read-request (:xt/id request))))
          (is (nil? (read-command (resolution/record-id binding))))
          (is (= 0 @(get-in env [:environment :constructs])))
          (is (= 404 (marked-post "POST" "still-forged")))
          (is (= 404 (marked-post "GET" "still-forged")))
          (is (= 1 (:selected-route-challenge-observations
                    ((:summary! probe)))))
          ;; An ordinary request with no private probe header retains the
          ;; existing intentional one-shot typed atomic business behavior.
          (is (= 200 (post! base)))
          (is (= ((model 'next-version) request {:owner "helper-a"})
                 (read-request (:xt/id request))))
          (is (some? (read-command (resolution/record-id binding))))
          (is (= 1 @(get-in env [:environment :constructs]))))))))


(deftest catalog-route-targets-enumerate-each-declared-operation-and-route
  ;; Isolate only the deterministic enumeration function with a controlled
  ;; two-operation summary. The real HTTP fixture above still exercises the
  ;; original router/handler path; this unit check does NOT certify a second
  ;; physical adapter or invent an issued handler for a synthetic route.
  (let [source {:application-assembly {:operation-acquisition-assembly :fixture}}
        catalog {:catalog {:operations [{:id :request/claim}
                                        {:id :request/cancel}]}}
        host-report {:checked-certified-route-ids
                     #{:route/claim :route/claim-alt :route/cancel}}
        summary {:request/claim {:routes [{:route-id :route/claim
                                            :method :post :path "/claim"}
                                           {:route-id :route/claim-alt
                                            :method :get :path "/claim/:id"}]}
                 :request/cancel {:routes [{:route-id :route/cancel
                                             :method :post :path "/cancel"}]}}
        enumerator (var-get #'probe/catalog-route-targets!)]
    (with-redefs [app/operation-summary (fn [_] summary)]
      (let [targets (enumerator source catalog host-report)]
        (is (= #{:route/claim :route/claim-alt :route/cancel}
               (set (map :route-id targets))))
        (is (= "/claim/gesso-route-witness"
               (:path (first (filter #(= :route/claim-alt (:route-id %)) targets)))))
        (is (= "/claim/:id"
               (:template (first (filter #(= :route/claim-alt (:route-id %)) targets)))))
        (is (= #{:request/claim :request/cancel}
               (set (map :operation targets))))))
    (with-redefs [app/operation-summary (fn [_] summary)]
      (is (= :catalog-route-selection-does-not-match-original-host
             (error-reason #(enumerator source catalog
                                        (assoc host-report :checked-certified-route-ids
                                               #{:route/claim :route/cancel})))))
      (is (= :catalog-route-selection-does-not-match-original-host
             (error-reason #(enumerator source
                                        {:catalog {:operations [{:id :request/claim}]}}
                                        host-report))))
      (is (= :catalog-route-selection-does-not-match-original-host
             (error-reason #(enumerator source
                                        {:catalog {:operations [{:id :request/claim}
                                                                {:id :request/claim}]}}
                                        host-report)))))))


(deftest final-catalog-dispatch-inspects-original-issuance-and-returned-http-system
  (let [env ((earlier-helper 'case-env))
        config {:catalog-source (:catalog-source env)
                :operation :request/claim
                :inputs {:input/owner "helper-a"}}]
    (with-adapter! :honest nil config
      (fn [{:keys [env probe system base]}]
        (let [contract (:contract env)
              selected #{:request/claim}
              inspect! (fn [cap src sys ids]
                         (probe/require-current-final-catalog-dispatch!
                          cap src sys ids))
              data (:data env)
              request (:request data)]
          (seed! [:requests request])
          (is (= :final-probe-wrapper-or-http-challenge-not-current
                 (error-reason #(inspect! probe contract system selected))))
          (is (= :one-final-checked-wrapper-http-challenge-returned
                 (:status ((:verify-once! probe) system base))))
          (let [checked (inspect! probe contract system selected)]
            (is (= probe/final-catalog-dispatch-report-type (:type checked)))
            (is (= :one-original-probe-one-returned-system-and-host (:scope checked)))
            (is (= selected (:checked-certified-route-ids checked)))
            (is (= selected (:catalog-expected-certified-route-ids checked)))
            (is (= selected (:catalog-observed-certified-route-ids checked)))
            (is (= 1 (:selected-route-challenge-observations checked)))
            (is (= :all-selected-certified-routes-observed-through-original-router
                   (:catalog-read-only-route-dispatch checked)))
            (is (= :not-established (:parameterized-route-generalization checked)))
            (is (= :none (:authority checked) (:retry-authority checked))))
          (is (= request (read-request (:xt/id request))))
          (is (nil? (read-command (resolution/record-id (:binding data)))))
          (is (= :final-catalog-dispatch-not-observed-on-original-probe
                 (error-reason #(inspect! probe contract system
                                          #{:request/claim :request/cancel}))))
          (is (= :final-probe-wrapper-or-http-challenge-not-current
                 (error-reason #(inspect! probe contract
                                          (assoc system :biff.ring/handler (fn [_] {:status 200}))
                                          selected))))
          (is (= :final-probe-wrapper-or-http-challenge-not-current
                 (error-reason #(inspect! probe (assoc contract :fake true)
                                          system selected))))
          (is (= :unissued-or-substituted-http-consumption-probe
                 (error-reason #(inspect! (assoc probe :summary! (fn [] {:status :pass}))
                                          contract system selected))))
          (with-redefs [probe/require-current-final-probe-route-inventory!
                        (fn [& _] {:type probe/final-route-inventory-report-type
                                   :final-probe-handler-slot :exact-issued-wrapper})]
            (is (= :substituted-original-final-dispatch-boundary
                   (error-reason #(inspect! probe contract system selected)))))
          ;; The authorization callback and source map are not proof of a
          ;; second instance. The original issued handle remains necessary.
          (is (= :unissued-or-substituted-http-consumption-probe
                 (error-reason #(inspect! (assoc probe :verify-once! (fn [& _] nil))
                                          contract system selected)))))))))


(deftest fresh-local-liveness-challenges-are-read-only-and-never-replay-startup
  (let [env ((earlier-helper 'case-env))
        config {:catalog-source (:catalog-source env)
                :operation :request/claim
                :inputs {:input/owner "helper-a"}}]
    (with-adapter! :honest nil config
      (fn [{:keys [env probe system base]}]
        (let [contract (:contract env)
              {:keys [request binding]} (:data env)
              live! (fn []
                      (probe/require-fresh-local-http-liveness!
                       probe contract system base))
              client (HttpClient/newHttpClient)
              forged (fn []
                       (let [req (-> (HttpRequest/newBuilder
                                      (URI/create (str base "/operations/request/claim")))
                                     (.timeout (Duration/ofSeconds 5))
                                     (.header "x-gesso-local-liveness-probe" "forged")
                                     (.POST (HttpRequest$BodyPublishers/noBody))
                                     (.build))]
                         (.statusCode (.send client req
                                             (HttpResponse$BodyHandlers/ofString)))))]
          (seed! [:requests request])
          (is (= :final-probe-wrapper-or-http-challenge-not-current
                 (error-reason live!)))
          (is (= 404 (forged)))
          (is (= :one-final-checked-wrapper-http-challenge-returned
                 (:status ((:verify-once! probe) system base))))
          (doseq [_ (range 2)]
            (let [observed (live!)]
              (is (= probe/fresh-local-http-liveness-report-type (:type observed)))
              (is (= :responded-to-current-inspection-challenge (:status observed)))
              (is (= :not-forwarded-to-business-router (:read-only-request observed)))
              (is (= :observed-at-inspection-only (:selected-local-listener observed)))
              (is (= :not-established (:future-liveness observed)))
              (is (= :none (:authority observed) (:transaction-authority observed)))
              (is (not-any? #(contains? observed %)
                            [:nonce :token :response :url :request :system :handler]))))
          (is (= 404 (forged)))
          (is (= request (read-request (:xt/id request))))
          (is (nil? (read-command (resolution/record-id binding))))
          (is (= 0 @(get-in env [:environment :constructs])))
          ;; Fresh health observations do not replay the one-time startup
          ;; probe and cannot change the ordinary typed POST semantics.
          (is (= :http-consumption-probe-already-attempted
                 (error-reason #((:verify-once! probe) system base))))
          (is (= 200 (post! base)))
          (is (= ((model 'next-version) request {:owner "helper-a"})
                 (read-request (:xt/id request))))
          (is (= 1 @(get-in env [:environment :constructs]))))))))


(deftest current-local-liveness-rejects-repointed-loopback-port
  ;; The original issued probe, not a mutable later locator, owns the port
  ;; that proved real HTTP consumption at startup. No new endpoint can be
  ;; learned from a later liveness inspection.
  (with-adapter! :honest nil
    (fn [{:keys [env probe system base]}]
      (let [contract (:contract env)
            port (.getPort (URI/create base))
            other-port (if (= port 65535) 65534 (inc port))
            redirected (str "http://127.0.0.1:" other-port)
            inspect! (fn [url]
                       (probe/require-fresh-local-http-liveness!
                        probe contract system url))]
        (is (= :one-final-checked-wrapper-http-challenge-returned
               (:status ((:verify-once! probe) system base))))
        (is (= :responded-to-current-inspection-challenge
               (:status (inspect! base))))
        (is (= :local-checked-http-endpoint-changed
               (error-reason #(inspect! redirected))))
        (is (= :responded-to-current-inspection-challenge
               (:status (inspect! (str base "/"))))
            "A harmless trailing slash does not change the socket endpoint")
        (is (= :responded-to-current-inspection-challenge
               (:status (inspect! base))))
        (let [report (inspect! base)]
          (is (= :same-verified-loopback-port
                 (:original-start-listener-endpoint report)))
          (is (not-any? #(contains? report %)
                        [:port :url :base-url :server :system :nonce :request])))))))
