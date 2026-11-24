(ns gesso.live.outbox.host
  "Concrete opt-in single-node Gesso/Biff application host with an exact,
   application-owned Biff module inventory and genuine XTDB/Live providers.

   Unlike test-only provider fixtures, these modules actually start and own
   an XTDB node and Gesso Live system. The checked Gesso application handler
   starts first, then XTDB, then Live, and finally the managed outbox worker.
   Reverse Biff teardown stops the worker before Live and XTDB. Each provider
   registers acquisition-time cleanup under the v2104 checked-startup owner.

   The host requires a CURRENT ApplicationAssembly, a real Ring handler and
   explicit source/worker/XTDB/Live configuration. Callers cannot supply or
   reorder modules, replace the managed source, or pass an independently
   owned Live source/dispatcher to confuse resource ownership. This is a
   reusable local host with an explicitly selected JDK HTTP listener, not a proof
   of authorized source provenance, or certification of all application
   writers. Empty XTDB config is explicitly an in-memory node; durable
   restarts require a caller-provided persistent XTDB configuration."
  (:require [clojure.string :as str]
            [com.biffweb.core :as biff]
            [gesso.live.application-preflight :as application]
            [gesso.live.core :as live]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.outbox.http :as http]
            [gesso.live.outbox.http-jdk :as jdk]
            [gesso.live.outbox.lifecycle :as lifecycle]
            [xtdb.node :as xtn]))

(def xtdb-module-id :gesso.live.outbox.host/xtdb)
(def live-module-id :gesso.live.outbox.host/live)
(def config-key :gesso.live.outbox.host/config)
(def listener-module-id :gesso.live.outbox.host/jdk-http)
(def listener-key :gesso.live.outbox.host/listener)
(def sse-status-key :gesso.live.outbox.host/sse-status)

(def start-order
  [application/application-handler-module-id
   xtdb-module-id
   live-module-id
   managed/module-id])

(defn- reject! [reason data]
  (throw (ex-info "Gesso single-node outbox host not admitted."
                  (assoc data :error/type :gesso.live.outbox/invalid-host
                         :reason reason))))

(def ^:private allowed-config
  #{:xtdb-config :live-options :source-id :worker-options :rules-context
    :stop-timeout-ms :http-jdk :authorized-sse})
(def ^:private allowed-live-options
  #{:rules :source-options :dispatch-options})

(defn- validate-config! [config]
  (when-not (map? config)
    (reject! :invalid-config {}))
  (when-let [unknown (seq (remove allowed-config (keys config)))]
    (reject! :unknown-config-fields {:fields (vec unknown)}))
  (when (and (contains? config :http-jdk) (not (map? (:http-jdk config))))
    (reject! :invalid-http-jdk-options {}))
  (when (and (contains? config :authorized-sse)
             (not (map? (:authorized-sse config))))
    (reject! :invalid-authorized-sse-options {}))
  (when-not (map? (:xtdb-config config))
    (reject! :invalid-xtdb-config {}))
  (let [options (:live-options config)]
    (when-not (map? options)
      (reject! :invalid-live-options {}))
    (when-let [extra (seq (remove allowed-live-options (keys options)))]
      (reject! :unowned-live-component {:fields (vec extra)}))
    (when-not (vector? (:rules options))
      (reject! :invalid-live-rules {}))
    (doseq [key [:source-options :dispatch-options]
            :when (contains? options key)]
      (when-not (map? (get options key))
        (reject! :invalid-live-options {:field key}))))
  (when-not (and (string? (:source-id config))
                 (not (str/blank? (:source-id config))))
    (reject! :invalid-source-id {}))
  (when-not (map? (:rules-context config))
    (reject! :invalid-rules-context {}))
  (let [worker (:worker-options config)]
    (when-not (map? worker)
      (reject! :invalid-worker-options {}))
    (try
      (lifecycle/validate-worker-options! worker)
      (catch clojure.lang.ExceptionInfo ex
        (reject! :invalid-worker-options
                 {:worker-reason (:reason (ex-data ex))}))))
  (when (contains? config :stop-timeout-ms)
    (let [timeout (:stop-timeout-ms config)]
      (when-not (and (integer? timeout) (<= 0 timeout 120000))
        (reject! :invalid-stop-timeout {}))))
  config)

(defn- start-xtdb! [system]
  (let [node (managed/acquire-startup-resource!
              :gesso.live.outbox.host/node
              #(xtn/start-node (get-in system [config-key :xtdb-config]))
              #(.close ^java.lang.AutoCloseable %))]
    (assoc system :xtdb/connectable node)))

(defn- stop-xtdb! [system]
  (.close ^java.lang.AutoCloseable (:xtdb/connectable system)))

(defn- start-live! [system]
  (when-not (:xtdb/connectable system)
    (reject! :missing-xtdb-provider {}))
  (let [source (managed/acquire-startup-resource!
                :gesso.live.outbox.host/live
                #(live/create (get-in system [config-key :live-options]))
                live/close!)]
    (assoc system :gesso.live/system source)))

(defn- stop-live! [system]
  (live/close! (:gesso.live/system system)))

(def xtdb-module
  {:biff.core/id xtdb-module-id
   :biff.core/start start-xtdb!
   :biff.core/stop stop-xtdb!})
(def live-module
  {:biff.core/id live-module-id
   :biff.core/start start-live!
   :biff.core/stop stop-live!})

(defn- stop-listener! [listener]
  (let [result ((:stop-listener! listener))]
    (when-not (= {:outcome :drained} result)
      (reject! :listener-not-drained {:outcome (:outcome result)}))))

(def ^:private listener-module
  {:biff.core/id listener-module-id
   :biff.core/start
   (fn [system]
     ;; Install the checked Biff handler itself, not the pre-wrapper delegate.
     ;; Admission remains :starting until every module has completed startup.
     (assoc system listener-key
            (managed/acquire-startup-resource!
             listener-module-id
             #(jdk/start! (:biff.ring/handler system)
                          (get-in system [config-key :http-jdk]))
             stop-listener!)))
   :biff.core/stop (fn [system] (stop-listener! (get system listener-key)))})

;; This is the exact, host-owned inventory, not a caller-supplied fixture.
(def ^:private modules
  [application/application-handler-module xtdb-module live-module managed/module])

(def ^:private network-modules
  [application/application-handler-module xtdb-module live-module
   listener-module managed/module])

(defn module-ids
  "Return the exact host inventory; true selects the owned JDK listener."
  ([] (module-ids false))
  ([network?] (mapv :biff.core/id (if network? network-modules modules))))

(defn- request-admission [handler timeout-ms]
  ;; Own synchronous Ring invocations, NOT returned asynchronous bodies or
  ;; physical HTTP writers. The same gate protects direct calls to the
  ;; installed canonical handler and calls through a future listener.
  (let [state (atom {:phase :starting :active 0})
        drained (promise)
        signal! (fn [{:keys [phase active]}]
                  (when (and (= :closed phase) (zero? active))
                    (deliver drained true)))
        unavailable {:status 503
                     :headers {"content-type" "text/plain; charset=utf-8"
                               "cache-control" "no-store"}
                     :body "Gesso host is not accepting requests."}]
    {:handler
     (fn [request]
       (let [[before _] (swap-vals! state
                                   #(if (= :open (:phase %))
                                      (update % :active inc) %))]
         (if (= :open (:phase before))
           (try
             (when-not (map? request)
               (reject! :invalid-ring-request {}))
             ;; The request cannot select a different XTDB provider through
             ;; any recognized alias. Do not expose the worker/stop handle.
             (handler (merge (dissoc request :xtdb/conn :xtdb/node
                                     :biff.xtdb/connection-pool)
                             (:context before)))
             (finally (signal! (swap! state update :active dec))))
           unavailable)))
     :open!
     (fn [system]
       (let [node (:xtdb/connectable system)
             source (:gesso.live/system system)
             worker (get system managed/handle-key)]
         (when-not (and node source worker (not (live/closed? source)))
           (reject! :missing-started-providers {}))
         (let [[before _] (swap-vals!
                           state
                           #(if (= :starting (:phase %))
                              (assoc % :phase :open
                                     :context {:xtdb/connectable node
                                               :biff.xtdb/node node
                                               :gesso.live/system source
                                               managed/source-id-key
                                               (get system managed/source-id-key)})
                              %))]
           (when-not (= :starting (:phase before))
             (reject! :invalid-request-admission-open {})))))
     :close-and-await!
     (fn []
       ;; Permanently fence new invocations even when drain times out. In
       ;; flight handlers have captured their own exact provider context.
       (signal! (swap! state #(-> % (assoc :phase :closed) (dissoc :context))))
       (try
         (when-not (true? (deref drained timeout-ms false))
           (reject! :request-drain-timed-out {:active (:active @state)}))
         (catch InterruptedException error
           (.interrupt (Thread/currentThread))
           (throw error))))}))

(defn- retire-route! [route timeout-ms]
  (when route
    ((:close! route))
    (let [result ((:await-drained! route) timeout-ms)]
      (when-not (= {:outcome :drained} result)
        (reject! :sse-route-not-drained {:outcome (:outcome result)})))))

(defn start!
  "Start a concrete canonical Gesso/Biff host with the exact module order.

   :assembly must be the current Gesso ApplicationAssembly. :handler must
   return actual Ring responses suitable for the canonical HTML preflight.
   :xtdb-config is explicitly required; {} requests volatile XTDB storage.
   :live-options allows only owned :rules, :source-options, :dispatch-options.
   :source-id, :worker-options and :rules-context are mandatory. The worker
   is installed AFTER providers, and cannot be omitted or re-ordered.

   The installed handler receives the exact owned XTDB and Live providers,
   overriding request-supplied provider aliases. Admission opens only after
   checked startup returns. Both stop! and native Biff stop fence new calls
   and drain synchronous invocations before retiring the worker/providers.
   A timed-out or interrupted request drain leaves providers open for retry.
   Returned asynchronous response bodies are outside this drain guarantee.

   Returns a native Biff system; call stop! on that exact system before
   discarding it. Checked startup failures retain the v2103/v2105 rollback
   and :cleanup/handle semantics. With :http-jdk (an options map for the
   existing JDK adapter), Biff owns a real listener before the worker starts.
   The listener is drained before Live/XTDB stop, including physical writers
   that outlive Ring return. A listener timeout retains providers for retry.
   Without :http-jdk no socket is opened. Optional :authorized-sse selects
   the existing authorized SSE adapter options (fixed path and trusted
   authorization callback). Its dispatch runs INSIDE checked request
   admission with the same provider context as the application handler.
   Install after managed startup but before opening admission. On shutdown,
   drain Ring requests, retire route-owned SSE sessions, then run native
   stop (worker, listener writers, Live, XTDB). Failed SSE retirement retains
   all providers for retry. Arbitrary application streams remain the caller's
   responsibility. The returned SSE status exposes no independent handler."
  [assembly handler config]
  (validate-config! config)
  (when-not (fn? handler)
    (reject! :invalid-handler {}))
  (when-not (application/application-assembly? assembly)
    (reject! :invalid-application-assembly {}))
  (let [route* (atom nil)
        timeout-ms (get config :stop-timeout-ms 10000)
        admission (request-admission
                   (fn [request]
                     ((or (:handler @route*) handler) request))
                   timeout-ms)
        initial (cond-> {config-key config
                         :biff.ring/handler (:handler admission)
                         managed/source-id-key (:source-id config)
                         managed/options-key (:worker-options config)
                         managed/context-key (:rules-context config)}
                  (contains? config :stop-timeout-ms)
                  (assoc managed/stop-timeout-key (:stop-timeout-ms config)))]
    (let [network? (contains? config :http-jdk)
          started (managed/start-biff-application!
                   assembly initial
                   (if network? #'network-modules #'modules)
                   (module-ids network?))
          native-stop (:biff.core/stop-system started)
          stop-lock (Object.)
          stop-owned! (fn []
                        (locking stop-lock
                          ((:close-and-await! admission))
                          (retire-route! @route* timeout-ms)
                          (native-stop)))]
      ;; Wrap the actual native stop chain, not just host/stop!: a caller of
      ;; biff/stop on this returned system must encounter the same fence.
      (try
        (when (contains? config :authorized-sse)
          ;; The route's fallback must be the original delegate, not the
          ;; admission wrapper (which would recurse). Its invocation remains
          ;; inside the canonical HTML guard and owned request context.
          (reset! route* (http/wrap-authorized-sse
                          (assoc started :biff.ring/handler handler)
                          (:authorized-sse config))))
        ((:open! admission) started)
        (cond-> (assoc started :biff.core/stop-system stop-owned!)
          @route* (assoc sse-status-key
                         (select-keys @route* [:active-count :occupied-count
                                              :pending-authorization-count
                                              :disconnected-count :closed? :drained?])))
        (catch Throwable startup-error
          ;; No traffic has been handed to the caller. Keep the original
          ;; error when cleanup also fails; retain the native owner for retry.
          (try
            (stop-owned!)
            (catch Throwable cleanup-error
              (let [error (ex-info "Host startup cleanup is blocked."
                                   {:error/type :gesso.live.outbox/host-cleanup-blocked
                                    :cleanup/stop! stop-owned!}
                                   startup-error)]
                (.addSuppressed error cleanup-error)
                (throw error))))
          (throw startup-error))))))

(defn stop!
  "Use the exact Biff system returned by start!. A drain timeout propagates
   and prevents closing Live and XTDB; retry with the SAME system."
  [system]
  (when-not (and (map? system) (some? (get system managed/handle-key)))
    (reject! :invalid-running-system {}))
  (biff/stop system))
