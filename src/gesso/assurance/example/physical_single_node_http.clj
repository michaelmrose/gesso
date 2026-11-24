(ns gesso.assurance.example.physical-single-node-http
  "v2334: Real Ring/Biff entry points for the v1494 embedded-XTDB reservation
   pilot. This is an *uncertified* physical application fixture, not an issued
   certified-plan executor, a v1478 checked host adoption, or a deployable
   authenticated service.

   The v1494 transaction contains explicit application-owned XTDB ASSERTs and
   unqualified persisted fields. v1251 certified-plan currently admits a
   restricted single-command plan with qualified physical field mappings and
   no extra assertions. Misrepresenting the v1494 callback as a certified-plan
   executor would erase an actual assurance obligation. The HTTP bridge
   deliberately preserves that boundary while exercising real Biff startup,
   Ring dispatch, Gesso HTML rendering and single-shot model transactions.

   By default no HTTP listener is opened. An opt-in, JDK-only loopback
   listener is installed AS A SEPARATE Biff module and managed by the same
   original start/stop lifecycle. An additional opt-in REAL Jetty HTTP/1
   adapter is now available; both bind 127.0.0.1 on an ephemeral port. These
   are test-only transport selections, NOT the production Biff/Aleph release. Callbacks remain
   valid only during the managed application lifetime. There is no intrinsic
   principal or CSRF verification: NEVER expose the unguarded diagnostic
   handler on a public network. This namespace
   does not register a canonical Gesso ApplicationAssembly, or claim a proof
   certificate, complete writer coverage, physical refinement, or a real
   deployed host. The bootstrap transaction remains a separate writer path."
  (:require [com.biffweb.core :as biff]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.http :as http])
  (:import (com.sun.net.httpserver HttpHandler HttpServer)
           (java.net InetSocketAddress)
           (java.nio.charset StandardCharsets)))

(def module-id :gesso.assurance.example.physical-single-node-http/install)
(def ^:private view-route "/__gesso_proving/slot")
(def ^:private operation-routes
  {"/__gesso_proving/slot/reserve" :slot/reserve
   "/__gesso_proving/slot/release" :slot/release})
(def ^:private open-obligations
  #{:v1494-operations-not-issued-certified-plan-executors
    :gesso-application-assembly-and-canonical-html-preflight-not-installed
    :request-principal-authorization-and-csrf-not-checked
    :http-server-consumption-of-biff-handler-not-authenticated
    :dynamic-uri-rewriting-and-deployment-not-attested
    :transaction-prestate-and-write-footprint-not-proved
    :complete-physical-writer-coverage-not-established
    :universal-refinement-not-established})

(def ^:dynamic *biff-modules* nil)

(defn- reject! [reason]
  (throw (ex-info "Invalid physical single-node HTTP proving fixture."
                  {:physical-single-node-http/rejection true :reason reason})))

(defn- checked-application! [app]
  (when-not (and (map? app)
                 (fn? (:read! app))
                 (fn? (:abstract! app))
                 (fn? (:execute! app))
                 (some? (:node app))
                 (= physical/operation-ids (get-in app [:report :operation-ids]))
                 (= :not-issued (get-in app [:report :certificate])))
    (reject! :invalid-or-already-certified-physical-application))
  app)

(defn- safe-render [app]
  ;; A response must describe one coherent XTDB observation. Calling
  ;; :abstract! separately would issue a second physical read, and a
  ;; concurrent writer could change the slot between those two snapshots.
  (let [row ((:read! app))
        state (physical/physical->abstract row)]
    (when-not (and (= (:slot/active-claims state) (:claims row))
                   (#{:available :reserved} (:slot/status state)))
      (reject! :physical-readback-mismatch))
    (http/html-response
     [:section {:id "gesso-proving-slot"
                :data-gesso-proving-physical true}
      [:h2 "Single-node reservation proving fixture"]
      [:p {:data-slot-status (name (:slot/status state))}
       "Status: " (name (:slot/status state))]
      [:p {:data-slot-revision (:revision row)}
       "Revision: " (str (:revision row))]])))

(def ^:private readback-header "x-gesso-proving-readback")

(defn- postcommit-readback-unknown []
  ;; The delegated source reported :committed, but an independently performed
  ;; read/interpretation/HTML rendering failed. Do not turn that into a failed
  ;; HTTP command: a client might submit the same mutation a second time.
  ;; Equally, do not fabricate a state or an authenticated commit receipt.
  ;; HTMX 2.0.7 can request a fresh authoritative page GET from HX-Refresh.
  {:status 200
   :headers {"content-type" "text/plain; charset=utf-8"
             "cache-control" "no-store"
             "HX-Refresh" "true"
             readback-header "unknown"}
   :body (str "The delegated operation reported a commit, but authoritative "
              "presentation is unavailable. Refresh before further actions.")})

(defn- render-after-reported-commit [app]
  (try
    (assoc-in (safe-render app) [:headers readback-header] "current")
    (catch Exception _
      ;; This only catches failures AFTER the delegated :committed outcome.
      ;; An XTDB submission failure or :commit-unknown must still propagate;
      ;; it cannot be promoted to commit success by a subsequent GET.
      ;; A failed readback may also signal bad source or corrupted data,
      ;; so :unknown is diagnostic, not a silently accepted model fact.
      (postcommit-readback-unknown))))

(defn- unsupported [status]
  {:status status
   :headers {"content-type" "text/plain; charset=utf-8"
             "cache-control" "no-store"}
   :body (case status
           400 "Unsupported Ring request"
           404 "Unknown proving route"
           405 "Method not allowed"
           "Request rejected")})

(defn- ring-handler! [app]
  (let [execute! (:execute! app)]
    (fn [request]
      (when-not (map? request)
        (reject! :invalid-ring-request))
      (let [method (:request-method request)
            uri (:uri request)]
        (cond
          (not (and (keyword? method) (string? uri)))
          (unsupported 400)

          (or (contains? request :path-params)
              (contains? request :route-params))
          (unsupported 400)

          (= uri view-route)
          (if (= :get method) (safe-render app) (unsupported 405))

          (contains? operation-routes uri)
          (if (= :post method)
            (let [outcome (execute! (get operation-routes uri))]
              ;; Exactly one call. Only a definitive committed result allows
              ;; a successful HTTP response. Unknown/rejected outcomes must
              ;; never become success merely because readback appears fresh.
              ;; An error propagates without an automatic second submission.
              (when-not (and (map? outcome)
                             (= :committed (:commit/status outcome)))
                (reject! :nondefinitive-physical-outcome))
              (render-after-reported-commit app))
            (unsupported 405))

          :else
          (unsupported 404))))))

(defn- install-module! [handler]
  (let [init-count (atom 0)
        start-count (atom 0)]
    {:biff.core/id module-id
     :biff.core/init
     (fn [_]
       (when-not (= 1 (swap! init-count inc))
         (reject! :repeated-module-init))
       {:biff.ring/handler handler})
     :biff.core/start
     (fn [system]
       (when-not (= 1 (swap! start-count inc))
         (reject! :repeated-module-start))
       (when-not (and (= 1 @init-count)
                      (identical? handler (:biff.ring/handler system)))
         (reject! :handler-replaced-before-start))
       system)}))

(def ^:private loopback-module-id
  :gesso.assurance.example.physical-single-node-http/loopback-listener)

(defn- biff-loopback-module! [handler state]
  ;; A real socket owned by Biff's module lifecycle, rather than an unrelated
  ;; test bridge launched after Biff start. This diagnostic JDK transport only
  ;; supports short, string-backed Ring responses. It does not provide Netty,
  ;; chunked SSE delivery, TLS, authenticated host middleware or deployment.
  {:biff.core/id loopback-module-id
   :biff.core/start
   (fn [system]
     (when-not (identical? handler (:biff.ring/handler system))
       (reject! :loopback-not-serving-original-biff-handler))
     (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]
       (try
         (.createContext
          server "/"
          (reify HttpHandler
            (handle [_ exchange]
              (try
                (let [method (keyword (.toLowerCase (.getRequestMethod exchange)))
                      uri (.getPath (.getRequestURI exchange))
                      response (handler {:request-method method :uri uri})
                      body (:body response)]
                  ;; Never pretend to have verified streaming/SSE on a JDK
                  ;; short-response adapter. Tests must use the qualified
                  ;; separate real SSE transport to claim stream behavior.
                  (if (and (map? response) (integer? (:status response))
                           (<= 100 (:status response) 599)
                           (string? body))
                    (let [bytes (.getBytes ^String body StandardCharsets/UTF_8)]
                      (doseq [[k v] (:headers response)
                              :when (some? v)]
                        (.add (.getResponseHeaders exchange) (str k) (str v)))
                      (.sendResponseHeaders exchange (int (:status response))
                                            (long (alength bytes)))
                      (with-open [output (.getResponseBody exchange)]
                        (.write output bytes)))
                    (.sendResponseHeaders exchange 501 -1)))
                (catch Exception _
                  ;; The JDK bridge must not fabricate a successful command
                  ;; response on exceptions or a non-string SSE body.
                  (try (.sendResponseHeaders exchange 500 -1)
                       (catch Exception _ nil)))
                (finally (.close exchange))))))
         (.start server)
         (reset! state {:server server
                        :port (.getPort (.getAddress server))
                        :handler handler})
         system
         (catch Throwable t
           (.stop server 0)
           (throw t)))))
   :biff.core/stop
   (fn [_]
     (when-let [server (:server @state)]
       (.stop ^HttpServer server 0)
       (reset! state nil)))})

(def ^:private jetty-module-id
  :gesso.assurance.example.physical-single-node-http/jetty-http1)

(defn- biff-jetty-module! [handler state]
  ;; Test the real Ring Jetty server adapter via the same Biff module lifecycle
  ;; as the selected handler. Biff 2.0.2 owns modules and :biff.ring/handler;
  ;; it does not itself ship a production HTTP listener. Jetty is bound to
  ;; loopback only and is not an Aleph/Netty or streaming-SSE qualification.
  {:biff.core/id jetty-module-id
   :biff.core/start
   (fn [system]
     (when-not (identical? handler (:biff.ring/handler system))
       (reject! :jetty-not-serving-original-biff-handler))
     ;; This dependency is an explicit :test extra-dep, NOT required for the
     ;; ordinary Gesso application classpath or :no-listener fixture.
     (let [start! (try (requiring-resolve 'ring.adapter.jetty9/run-jetty)
                       (catch Exception _ (reject! :jetty-test-adapter-unavailable)))
           server (start! handler {:host "127.0.0.1"
                                   :port 0
                                   :join? false})]
       (try
         (let [;; Reflection deliberately stays inside this opt-in branch:
               ;; the ordinary Gesso classpath must not need Jetty classes.
               connectors (vec (.getConnectors server))
               connector (first connectors)
               port (when connector (.getLocalPort connector))]
           (when-not (and (= 1 (count connectors))
                          (integer? port) (pos? port))
             (reject! :jetty-loopback-port-unavailable))
           (reset! state {:server server :port port :handler handler})
           system)
         (catch Throwable t
           (.stop server)
           (throw t)))))
   :biff.core/stop
   (fn [_]
     (when-let [server (:server @state)]
       (try
         (.stop server)
         (finally
           (reset! state nil)))))})

(defn with-http-proving-application!
  "Execute a local embedded-XTDB proving application under one Biff start.
   The default three-arg form installs NO listener. The test-only four-arg
   form opts in with literal :biff-loopback or :biff-jetty-http1 (not
   caller-supplied server functions). Each starts and stops an explicit test
   HTTP module on 127.0.0.1, port 0. The latter uses a REAL Ring Jetty
   server to verify actual adapter/HTTP behavior. Neither selection proves
   production deployment, Aleph streaming SSE, authenticated principal,
   CSRF protection, source-completeness or release eligibility. The system
   is stopped on both normal and exceptional callback exit."
  ([f] (with-http-proving-application! (str "v1495-" (random-uuid)) f))
  ([slot-id f]
   (with-http-proving-application!
    slot-id (fn [_ original] original) f))
  ([slot-id handler-extension! f]
   (with-http-proving-application! slot-id handler-extension! :no-listener f))
  ([slot-id handler-extension! listener-selection f]
   (when-not (and (fn? handler-extension!) (fn? f))
     (reject! :invalid-test-callback))
   (when-not (contains? #{:no-listener :biff-loopback :biff-jetty-http1}
                        listener-selection)
     (reject! :invalid-test-listener-selection))
   ;; Fail before opening embedded XTDB or starting Biff if the explicitly
   ;; selected adapter is absent. The normal no-listener route never loads it.
   (when (= :biff-jetty-http1 listener-selection)
     (try
       (requiring-resolve 'ring.adapter.jetty9/run-jetty)
       (catch Exception _
         (reject! :jetty-test-adapter-unavailable))))
   (physical/with-proving-application!
    slot-id
    (fn [app]
      (checked-application! app)
      (let [original-handler (ring-handler! app)
            routes (into {view-route :read-only} operation-routes)
            handler (handler-extension! (assoc app :handler original-handler
                                                :http-routes routes)
                                        original-handler)]
        (when-not (fn? handler)
          (reject! :invalid-prestart-handler-extension))
        (let [module (install-module! handler)
              listener-state (atom nil)
              listener-module (case listener-selection
                                :no-listener nil
                                :biff-loopback
                                (biff-loopback-module! handler listener-state)
                                :biff-jetty-http1
                                (biff-jetty-module! handler listener-state))
              modules (cond-> [module] listener-module (conj listener-module))
              order (cond-> [module-id]
                      listener-module (conj (:biff.core/id listener-module)))
              ;; Listener startup happens AFTER Biff installs its immutable
              ;; chosen Ring function, and Biff stop owns socket teardown.
              system (binding [*biff-modules* modules]
                       (biff/start (:biff-context app) #'*biff-modules* order))]
          (try
            (when-not (identical? handler (:biff.ring/handler system))
              (reject! :adopted-handler-not-identical))
            (when (and listener-module
                       (not (identical? handler (:handler @listener-state))))
              (reject! :loopback-not-serving-original-biff-handler))
            (let [report
                  {:type :gesso.assurance.example.physical-single-node-http/local-biff-fixture
                   :version 1
                   :routes routes
                   :physical-table physical/table
                   :module-start :one-observed-biff-start
                   :handler-in-returned-system :identical
                   :handler-extension :test-only-not-checked-or-certified
                   :physical-transactions :delegated-to-v1494-once-per-post
                   :postcommit-rendering :independent-physical-readback-may-be-unknown
                   :postcommit-render-failure :preserve-reported-commit-and-request-refresh
                   :biff-owned-loopback-listener
                   (case listener-selection
                     :no-listener :not-selected
                     :biff-loopback :jdk-short-http-only
                     :biff-jetty-http1 :ring-jetty-http1-test-adapter)
                   :issued-certified-plan-executors :not-established
                   :certified-plan-compatibility
                   {:persisted-fields :unqualified-v1494-fields
                    :current-v1251-required-fields :qualified-and-exhaustive
                    :application-extra-assertions :not-admitted-by-v1251
                    :safe-next-step :derive-a-supported-physical-plan-without-losing-guards}
                   :installed-checked-router :not-established
                   :canonical-application-preflight :not-established
                   :host-deployment :not-established
                   :writer-completeness :not-established
                   :physical-refinement :not-established
                   :open-obligations open-obligations
                   :certificate :not-issued
                   :admission :not-admitted
                   :authority :none}]
              (f (cond-> (assoc app :handler handler
                                  :delegate-handler original-handler
                                  :http-routes routes
                                  :system system :module module
                                  :http-report report)
                   listener-module
                   (assoc :loopback-base-url
                          (str "http://127.0.0.1:" (:port @listener-state))
                          :loopback-listener-state listener-state))))
            (finally
              (biff/stop system)))))))))
