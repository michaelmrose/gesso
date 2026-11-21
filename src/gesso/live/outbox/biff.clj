(ns gesso.live.outbox.biff
  "Opt-in Biff 2 lifecycle integration for a source-bound single-node Live
   outbox worker. The native Biff module is started AFTER provider modules,
   and its :biff.core/stop callback drains BEFORE those providers stop.

   This module never creates or owns XTDB or the Live source. Its worker only
   uses explicitly supplied, host-trusted source identity, rules context and
   worker options. An explicit canonical startup wrapper validates the exact
   module and last-in-start-order placement before delegating to Gesso's
   existing application-preflight Biff startup.

   Biff's native stop-system calls stop callbacks in reverse start order. A
   timed-out drain THROWS, preventing that stop chain from closing XTDB/Live;
   the host must keep the original Biff system and retry biff/stop. Worker
   failure still permits resource teardown once the worker thread has ended.

   This is an actual Biff lifecycle boundary, NOT proof of certified source
   provenance, complete writer coverage, browser receipt, or deployed host
   adoption. Direct biff/start without the checked wrapper may omit ordering
   checks, so this module does not claim to certify arbitrary Biff startup."
  (:require [clojure.string :as str]
            [gesso.live.application-preflight :as application]
            [gesso.live.consistency.xtdb :as live.xtdb]
            [gesso.live.core :as live]
            [gesso.live.outbox.lifecycle :as lifecycle]))

(def module-id :gesso.live.outbox/managed-worker)
(def handle-key :gesso.live.outbox/managed-handle)
(def source-id-key :gesso.live.outbox/source-id)
(def options-key :gesso.live.outbox/worker-options)
(def context-key :gesso.live.outbox/rules-context)
(def stop-timeout-key :gesso.live.outbox/stop-timeout-ms)

(def ^:private default-stop-timeout-ms 10000)

(defn- reject! [reason info]
  (throw (ex-info "Invalid managed Gesso/Biff outbox lifecycle."
                  (assoc info :error/type :gesso.live.outbox/invalid-biff-lifecycle
                         :reason reason))))

(defn- validate-config!
  "Check host-owned settings without touching XTDB or starting a thread."
  [system]
  (when-not (map? system)
    (reject! :invalid-system {}))
  (when (contains? system handle-key)
    (reject! :preexisting-handle {}))
  (when-not (and (string? (get system source-id-key))
                 (not (str/blank? (get system source-id-key))))
    (reject! :invalid-source-id {:source-id (get system source-id-key)}))
  (try
    (lifecycle/validate-worker-options! (get system options-key))
    (catch clojure.lang.ExceptionInfo error
      (reject! :invalid-worker-options
               {:worker-reason (:reason (ex-data error))})))
  (when-not (map? (get system context-key))
    (reject! :invalid-rules-context {}))
  (let [timeout (get system stop-timeout-key default-stop-timeout-ms)]
    (when-not (and (integer? timeout) (<= 0 timeout 120000))
      (reject! :invalid-stop-timeout {:timeout-ms timeout})))
  system)

(defn- require-providers! [system]
  (let [live-system (:gesso.live/system system)
        node (live.xtdb/connectable-from system)]
    (when-not (and (map? live-system)
                   (some? (:source live-system))
                   (instance? clojure.lang.IDeref (:closed? live-system))
                   (not (live/closed? live-system)))
      (reject! :missing-or-closed-live-provider {}))
    (when-not node
      (reject! :missing-xtdb-provider {}))
    [node live-system]))

(defn start-module!
  "Run after the host's XTDB and Live provider modules. No resources are
   constructed until host configuration and provider presence are checked.
   The returned Biff map owns ONLY the managed worker handle."
  [system]
  (validate-config! system)
  (let [[node live-system] (require-providers! system)
        handle (lifecycle/start! node live-system (get system context-key)
                                 (get system source-id-key)
                                 (get system options-key))]
    (assoc system handle-key handle)))

(defn stop-module!
  "Biff's reverse-order stop callback. A timeout is a *hard teardown fence*:
   throwing prevents Biff from invoking later provider stops. Retry biff/stop
   on the SAME system, after keeping XTDB and Live open."
  [system]
  (let [handle (get system handle-key)]
    (when-not handle
      (reject! :missing-managed-handle {}))
    (let [result (lifecycle/stop!
                  handle
                  (get system stop-timeout-key default-stop-timeout-ms))]
      (when (= :timed-out (:outcome result))
        (reject! :drain-timed-out {:stop-outcome :timed-out
                                   :stop-requested? true}))
      (when-not (#{:drained :failed} (:outcome result))
        (reject! :unrecognized-stop-outcome {:outcome (:outcome result)}))
      nil)))

(def module
  "The singleton, actual Biff 2 module. Include it exactly once in a modules
   Var with module-id LAST in start-order, after the XTDB and Live owners.
   Host-supplied config is required; no implicit worker is installed."
  {:biff.core/id module-id
   :biff.core/start start-module!
   :biff.core/stop stop-module!})

(defn- require-canonical-module-and-order! [modules-var start-order]
  (when-not (var? modules-var)
    (reject! :invalid-modules-var {}))
  (when-not (and (sequential? start-order)
                 (= module-id (last start-order))
                 (= 1 (count (filter #{module-id} start-order))))
    (reject! :worker-module-not-last {:start-order start-order}))
  (let [modules @modules-var
        candidates (filterv #(= module-id (:biff.core/id %))
                            (if (sequential? modules) modules []))]
    (when-not (and (= 1 (count candidates))
                   (identical? (:biff.core/start module)
                               (:biff.core/start (first candidates)))
                   (identical? (:biff.core/stop module)
                               (:biff.core/stop (first candidates))))
      (reject! :invalid-worker-module-inventory
               {:matching-module-count (count candidates)}))))

(defn start-biff-application!
  "Checked opt-in canonical Biff startup with the managed outbox module LAST.

   The modules Var must include Gesso's canonical application-handler module
   and this exact worker module. The start order places the application
   handler FIRST (enforced by the existing Gesso application-preflight
   startup), and the worker LAST, so Biff's reverse stop drains the worker
   before XTDB/Live resource providers. The initial trusted system must
   supply :gesso.live.outbox/source-id, :worker-options (including worker-id),
   :rules-context and optionally :stop-timeout-ms. Provider modules must
   supply the actual :xtdb/connectable (or released Biff 2 XTDB keys) and
   :gesso.live/system before the worker module starts.

   This API does not patch the canonical Gesso application startup or
   automatically enroll other applications. Use it explicitly instead of
   a direct biff/start for the source-bound outbox profile."
  [assembly initial-system modules-var start-order]
  (validate-config! initial-system)
  (require-canonical-module-and-order! modules-var start-order)
  (application/start-biff-application!
   assembly initial-system modules-var start-order))
