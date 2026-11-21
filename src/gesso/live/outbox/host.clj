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
   reusable local host, NOT an automatically deployed HTTP listener, a proof
   of authorized source provenance, or certification of all application
   writers. Empty XTDB config is explicitly an in-memory node; durable
   restarts require a caller-provided persistent XTDB configuration."
  (:require [clojure.string :as str]
            [com.biffweb.core :as biff]
            [gesso.live.application-preflight :as application]
            [gesso.live.core :as live]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.outbox.lifecycle :as lifecycle]
            [xtdb.node :as xtn]))

(def xtdb-module-id :gesso.live.outbox.host/xtdb)
(def live-module-id :gesso.live.outbox.host/live)
(def config-key :gesso.live.outbox.host/config)

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
    :stop-timeout-ms})
(def ^:private allowed-live-options
  #{:rules :source-options :dispatch-options})

(defn- validate-config! [config]
  (when-not (map? config)
    (reject! :invalid-config {}))
  (when-let [unknown (seq (remove allowed-config (keys config)))]
    (reject! :unknown-config-fields {:fields (vec unknown)}))
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

;; This is the exact, host-owned inventory, not a caller-supplied fixture.
(def ^:private modules
  [application/application-handler-module xtdb-module live-module managed/module])

(defn module-ids
  "Return the four immutable module IDs this concrete host installs."
  []
  (mapv :biff.core/id modules))

(defn start!
  "Start a concrete canonical Gesso/Biff host with the exact module order.

   :assembly must be the current Gesso ApplicationAssembly. :handler must
   return actual Ring responses suitable for the canonical HTML preflight.
   :xtdb-config is explicitly required; {} requests volatile XTDB storage.
   :live-options allows only owned :rules, :source-options, :dispatch-options.
   :source-id, :worker-options and :rules-context are mandatory. The worker
   is installed AFTER providers, and cannot be omitted or re-ordered.

   Returns a native Biff system; call stop! on that exact system before
   discarding it. Checked startup failures retain the v2103/v2105 rollback
   and :cleanup/handle semantics. No HTTP listener is opened by this host."
  [assembly handler config]
  (validate-config! config)
  (when-not (fn? handler)
    (reject! :invalid-handler {}))
  (when-not (application/application-assembly? assembly)
    (reject! :invalid-application-assembly {}))
  (let [initial (cond-> {config-key config
                         :biff.ring/handler handler
                         managed/source-id-key (:source-id config)
                         managed/options-key (:worker-options config)
                         managed/context-key (:rules-context config)}
                  (contains? config :stop-timeout-ms)
                  (assoc managed/stop-timeout-key (:stop-timeout-ms config)))]
    (managed/start-biff-application!
     assembly initial #'modules start-order)))

(defn stop!
  "Use the exact Biff system returned by start!. A drain timeout propagates
   and prevents closing Live and XTDB; retry with the SAME system."
  [system]
  (when-not (and (map? system) (some? (get system managed/handle-key)))
    (reject! :invalid-running-system {}))
  (biff/stop system))
