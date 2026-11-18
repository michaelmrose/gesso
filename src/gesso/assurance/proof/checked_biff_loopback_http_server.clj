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
  (when-not (identical? original-host-check
                         (var-get #'host/require-current-biff-host-adoption!))
    (reject! :substituted-checked-host-boundary)))

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

(defn- handle! [record handler ^HttpExchange exchange]
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
                           :request-id (random-uuid)}
                  response (handler request)]
              (send! exchange (:status response))))))
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
  [host-handle contract-source trusted-xtdb-node]
  (pinned!)
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
                :operation-attempts (AtomicLong. 0)}
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
             (let [server (HttpServer/create
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
                                     (handle! record handler exchange))))
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
                      :original-canonical-plan-adoption :not-established
                      :same-http-request-typed-trial :not-established
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
     :summary! summary! :base-url! base-url! :stop! stop!}))
