(ns gesso.assurance.proof.checked-biff-loopback-http-server
  "v1778: Proving-only REAL loopback HTTP server started as a Biff module
   AFTER original Gesso application and checked host modules.

   The module captures the exact locally issued Biff handler from its own
   :biff.core/start input and dispatches real socket HTTP requests to that
   handler, using server-owned request context. The caller cannot replace
   the captured handler after startup by altering a returned Biff system map.

   This establishes only THIS started loopback server's consumption of the
   handler, NOT deployment of another server, Biff's production server
   module, principal authentication, typed proof for each request, physical
   writer closure, durable XTDB receipt or universal refinement.
   No certification, transaction permission or automatic retry is issued."
  (:require [clojure.string :as str]
            [gesso.assurance.proof.checked-biff-host-adoption :as host]
            [gesso.assurance.proof.xtdb-gateway-biff-issued-catalog-canonical-trial
             :as catalog-trial]
            [gesso.live.application-preflight :as application])
  (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.net InetSocketAddress]
           [java.nio.charset StandardCharsets]
           [java.util WeakHashMap]
           [java.util.concurrent Executors ThreadFactory TimeUnit]
           [java.util.concurrent.atomic AtomicBoolean AtomicLong]))

(def report-type
  :gesso.assurance.proof.checked-biff-loopback-http-server/real-loopback-server)
(def report-version 1)
(def module-id
  :gesso.assurance.proof.checked-biff-loopback-http-server/start)

(deftype LoopbackHandle []
  Object
  (toString [_] "#<gesso.assurance/loopback-http-server>"))

(def ^:private ^WeakHashMap issued (WeakHashMap.))
(def ^:private original-host-check host/require-current-biff-host-adoption!)
(def ^:private original-trial-issuer
  catalog-trial/issue-biff-issued-catalog-canonical-trial!)
(def ^:private original-handler-key application/biff-ring-handler-key)

(defn- reject! [reason]
  (throw (ex-info "Loopback HTTP server adoption not established."
                  {:checked-biff-loopback-http-server/rejection true
                   :reason reason})))

(defn- current! [handle]
  (let [record (when (instance? LoopbackHandle handle)
                 (locking issued (.get issued handle)))]
    (or record (reject! :unissued-loopback-server))))

(defn- pinned! []
  (when-not (and (identical? original-host-check
                             (var-get #'host/require-current-biff-host-adoption!))
                 (identical? original-trial-issuer
                             (var-get #'catalog-trial/issue-biff-issued-catalog-canonical-trial!)))
    (reject! :substituted-checked-host-or-catalog-trial-boundary)))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- trial-route-keys! [source operation]
  (let [rows (get-in (application/operation-summary
                      (get-in source [:application-assembly
                                      :operation-acquisition-assembly]))
                     [operation :routes])]
    (when-not (and (seq rows)
                   (every? (fn [{:keys [method path]}]
                             (and (keyword? method) (string? path)
                                  (.startsWith ^String path "/")
                                  (not (re-find #"[{}:*]" path))))
                           rows))
      (reject! :missing-literal-typed-trial-routes))
    (set (map (juxt :method :path) rows))))

(defn- send! [^HttpExchange exchange status]
  (let [status (if (and (integer? status) (<= 200 status 599)) status 500)
        body (if (<= 200 status 299) "Gesso loopback handler returned"
                 "Gesso loopback handler did not succeed")
        bytes (.getBytes ^String body StandardCharsets/UTF_8)]
    (.add (.getResponseHeaders exchange) "Content-Type"
          "text/plain; charset=utf-8")
    (.sendResponseHeaders exchange status (alength bytes))
    (with-open [out (.getResponseBody exchange)]
      (.write out bytes))))

(defn- handle! [record handler trial ^HttpExchange exchange]
  (let [calls ^AtomicLong (:handled record)
        operations ^AtomicLong (:operation-attempts record)
        method (keyword (str/lower-case (.getRequestMethod exchange)))
        uri (.getRequestURI exchange)]
    (.incrementAndGet calls)
    (try
      ;; The proving transport accepts no client-controlled context or input
      ;; payload. In particular it cannot accept a forged XTDB node/principal.
      (let [body-byte (with-open [body (.getRequestBody exchange)] (.read body))
            valid? (and (= -1 body-byte)
                        (nil? (.getRawQuery uri))
                        (contains? #{:get :post :put :delete :patch} method))]
        (if-not valid?
          (send! exchange 400)
          (do
            (.incrementAndGet operations)
            (let [request {:request-method method
                           :uri (.getPath uri)
                           :biff.xtdb/node (:node record)
                           :request-id (random-uuid)}]
              (if-not trial
                (send! exchange (:status (handler request)))
                ;; v1779: the REAL network exchange selects exactly one
                ;; v1777 catalog-bound observation, which itself calls the
                ;; same Biff system handler captured by THIS server at start.
                ;; Never invoke handler a second time to obtain a report.
                (if-not (contains? (:routes trial) [method (.getPath uri)])
                  (send! exchange 404)
                  (do
                    (.incrementAndGet ^AtomicLong (:trial-attempts record))
                    (let [observed ((:observe-once! (:issued trial)) request)
                          status (:status observed)
                          qualified?
                          (and (= catalog-trial/report-type (:type observed))
                               (= catalog-trial/report-version (:version observed))
                               (= (:operation trial) (:operation-id observed))
                               (= :one-original-adopted-biff-handler-returned
                                  (:host-handler-return observed))
                               (= :one-original-issued-executor-returned
                                  (:certified-executor-return observed))
                               (= :none (:retry-authority observed)
                                  (:authority observed))
                               (= :not-issued (:certificate observed)))
                          accepted? (and qualified?
                                         (= :bounded-biff-issued-catalog-atomic-acknowledgment
                                            status))
                          rejected? (and qualified?
                                         (contains? #{:biff-issued-catalog-canonical-rejected
                                                      :biff-issued-catalog-typed-check-rejected}
                                                    status))]
                      ;; Only a report produced while handling THIS HTTP
                      ;; request can update this redacted state. No raw Ring
                      ;; context, response or transaction receipt is retained.
                      (when qualified?
                        (reset! (:trial-outcome record) status))
                      (when accepted?
                        (.incrementAndGet ^AtomicLong (:trial-qualified record)))
                      ;; A 200 here means the ORIGINAL Biff Ring response was
                      ;; observed as 200. Unknown/uncertain observations are
                      ;; 500, NOT evidence that XTDB failed or safe to retry.
                      (send! exchange (if (or accepted? rejected?) 200 500))))))))))
      (catch Throwable _
        ;; An exception can occur after an authoritative write. Never retry,
        ;; and never represent an exception as a definite failed transaction.
        (try (send! exchange 500) (catch Throwable _ nil)))
      (finally (.close exchange)))))

(defn- stop-server! [{:keys [^HttpServer server executor]}]
  (when server (.stop server 0))
  (when executor
    (.shutdownNow executor)
    (.awaitTermination executor 3 TimeUnit/SECONDS)))

(defn issue-loopback-http-server-module!
  "Create one REAL loopback Biff server-start module. Requires original host
   issuance and trusted server-owned XTDB node. The returned :module must run
   AFTER host/module-id in canonical Biff lifecycle start order.

   :base-url! is a local test-only endpoint locator, not proof of deployment;
   :summary! gives only redacted evidence and :stop! always closes the port."
  ([host-handle contract-source trusted-xtdb-node]
   (issue-loopback-http-server-module!
    host-handle contract-source trusted-xtdb-node nil))
  ([host-handle contract-source trusted-xtdb-node trial-config]
  (pinned!)
  (when-not (or (nil? trial-config)
                (and (plain-map? trial-config)
                     (= #{:catalog-source :operation :inputs}
                        (set (keys trial-config)))
                     (plain-map? (:catalog-source trial-config))
                     (plain-map? (:catalog (:catalog-source trial-config)))
                     (qualified-keyword? (:operation trial-config))
                     (plain-map? (:inputs trial-config))))
    (reject! :invalid-source-bound-network-trial))
  (when-not (and (host/checked-biff-host-adoption? host-handle)
                 (map? contract-source)
                 (some? trusted-xtdb-node))
    (reject! :invalid-server-source-or-node))
  (let [handle (LoopbackHandle.)
        lifecycle (atom :issued)
        record {:source contract-source
                :host-handle host-handle
                :node trusted-xtdb-node
                :lifecycle lifecycle
                :handled (AtomicLong. 0)
                :operation-attempts (AtomicLong. 0)
                :trial-attempts (AtomicLong. 0)
                :trial-qualified (AtomicLong. 0)
                :trial-outcome (atom :not-established)}
        server-module
        {:biff.core/id module-id
         :biff.core/start
         (fn [system]
           (when-not (compare-and-set! lifecycle :issued :starting)
             (reject! :repeated-or-stopped-loopback-server-start))
           (pinned!)
           (let [host-report (try (original-host-check host-handle contract-source system)
                                  (catch Throwable error
                                    (reset! lifecycle :failed)
                                    (throw error)))
                 handler (get system original-handler-key)]
             (when-not (and (= host/report-type (:type host-report))
                            (= :identical-to-guarded-checked-router-wrapper
                               (:handler-slot host-report))
                            (fn? handler))
               (reset! lifecycle :failed)
               (reject! :not-the-original-adopted-biff-handler))
             (let [trial (when trial-config
                           (try
                             (let [operation (:operation trial-config)
                                   routes (trial-route-keys! contract-source operation)
                                   observed (original-trial-issuer
                                             host-handle contract-source system
                                             (:catalog-source trial-config)
                                             operation (:inputs trial-config))]
                               {:issued observed :routes routes
                                :operation operation})
                             (catch Throwable error
                               (reset! lifecycle :failed)
                               (throw error))))
                   server (HttpServer/create
                           (InetSocketAddress. "127.0.0.1" (int 0)) 0)
                   counter (AtomicLong. 0)
                   factory (reify ThreadFactory
                             (newThread [_ task]
                               (doto (Thread. ^Runnable task
                                              (str "gesso-v1778-http-"
                                                   (.incrementAndGet counter)))
                                 (.setDaemon true))))
                   executor (Executors/newCachedThreadPool factory)]
               (try
                 (.createContext server "/"
                                 (reify HttpHandler
                                   (handle [_ exchange]
                                     (handle! record handler trial exchange))))
                 (.setExecutor server executor)
                 (.start server)
                 (let [started {:server server :executor executor
                                :system system :handler handler
                                :port (.getPort (.getAddress server))}]
                   (when-not (compare-and-set! lifecycle :starting started)
                     (reject! :loopback-server-start-state-changed))
                   ;; Return the unchanged canonical Biff handler slot.
                   system)
                 (catch Throwable error
                   (stop-server! {:server server :executor executor})
                   (reset! lifecycle :failed)
                   (throw error))))))}
        summary! (fn []
                   (pinned!)
                   (let [state @lifecycle]
                     (when (map? state)
                       (original-host-check host-handle contract-source
                                            (:system state))
                       (when-not (identical? (:handler state)
                                             (get (:system state)
                                                  original-handler-key))
                         (reject! :captured-server-handler-changed)))
                     {:type report-type :version report-version
                      :scope :real-in-process-loopback-http-socket
                      :state (if (map? state) :started state)
                      :server-captured-biff-handler
                      (if (map? state) :exact-checked-handler-at-server-start
                          :not-established)
                      :http-socket-requests-observed (.get ^AtomicLong (:handled record))
                      :server-handler-dispatch-attempts
                      (.get ^AtomicLong (:operation-attempts record))
                      :original-canonical-plan-adoption
                      (if (pos? (.get ^AtomicLong (:trial-qualified record)))
                        :one-original-issued-canonical-executor-observed
                        :not-established)
                      :same-http-request-typed-trial
                      (if (pos? (.get ^AtomicLong (:trial-qualified record)))
                        :one-loopback-http-request-with-typed-catalog-atomic-ack
                        :not-established)
                      :typed-trial-enabled (boolean trial-config)
                      :typed-trial-attempts (.get ^AtomicLong (:trial-attempts record))
                      :typed-trial-qualified (.get ^AtomicLong (:trial-qualified record))
                      :typed-trial-last-outcome @(:trial-outcome record)
                      :external-server-deployment :not-established
                      :authenticated-principal :not-established
                      :physical-xtdb-acknowledgment :not-established
                      :complete-writer-coverage :not-established
                      :actual-live-delivery :not-established
                      :certificate :not-issued :admission :not-admitted
                      :retry-authority :none :authority :none}))
        base-url! (fn []
                    (let [state @lifecycle]
                      (when-not (map? state)
                        (reject! :loopback-server-not-running))
                      (str "http://127.0.0.1:" (:port state))))
        stop! (fn []
                (loop []
                  (let [state @lifecycle]
                    (cond
                      (= state :issued)
                      (if (compare-and-set! lifecycle :issued :stopped)
                        nil (recur))

                      (map? state)
                      (if (compare-and-set! lifecycle state :stopped)
                        (stop-server! state)
                        (recur))

                      (= state :starting)
                      (reject! :server-start-in-progress)

                      :else nil))))]
    (locking issued (.put issued handle record))
    {:module server-module :handle handle
     :summary! summary! :base-url! base-url! :stop! stop!})))
