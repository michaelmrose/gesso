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
   the host must keep the original Biff system and retry biff/stop. The checked
   startup additionally rolls back previously completed modules when a later
   start fails; blocked rollback is retryable without closing dependencies. Worker
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
(def init-policy-key :gesso.live.outbox/init-policy)

(def ^:private default-stop-timeout-ms 10000)

(defn- restore-interrupt!
  "A captured provider interruption must not clear the host's JVM cancellation
   signal, even when checked startup reports a rollback or retryable fence."
  [error]
  (when (instance? InterruptedException error)
    (.interrupt (Thread/currentThread))))

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
  "Biff's reverse-order stop permanently retires the original worker AND
   every successor created through explicit recovery. A timeout anywhere in
   the restart lineage is a hard provider-teardown fence. A concurrent restart
   cannot escape retirement: retry biff/stop on the SAME original system."
  [system]
  (let [handle (get system handle-key)]
    (when-not handle
      (reject! :missing-managed-handle {}))
    (let [result (lifecycle/retire!
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

(deftype StartupCleanup [state]
  Object
  ;; Preserve retry authority without accidentally printing the stopped
  ;; system map, which may contain provider handles and private credentials.
  (toString [_] "#<GessoOutboxStartupCleanup>"))

(def ^:dynamic *startup-ownership* nil)

(defn- register-owned-resource!
  [expected-phase resource-id acquire! close!]
  (let [{:keys [cleanup token module-id phase]} *startup-ownership*]
    (when-not (and (= expected-phase phase)
                   (instance? StartupCleanup cleanup) (some? token))
      (reject! :no-active-startup-ownership {:phase expected-phase}))
    (when-not (qualified-keyword? resource-id)
      (reject! :invalid-owned-resource-id {:resource-id resource-id}))
    (when-not (and (fn? acquire!) (fn? close!))
      (reject! :invalid-resource-functions {:resource-id resource-id}))
    (let [resource (acquire!)]
      (try
        (swap! (.-state ^StartupCleanup cleanup)
               (fn [s]
                 (when-not (contains? (:open-scopes s) token)
                   (reject! :closed-startup-ownership
                            {:module-id module-id :resource-id resource-id}))
                 (when (some #(and (identical? token (:scope-token %))
                                   (= resource-id (:resource-id %)))
                             (:remaining s))
                   (reject! :duplicate-owned-resource
                            {:module-id module-id :resource-id resource-id}))
                 (update s :remaining conj
                         {:module-id module-id
                          :resource-id resource-id
                          :scope-token token
                          :ownership (if (= phase :init) :init :partial)
                          :stop (fn [_] (close! resource))
                          :system nil})))
        resource
        (catch Throwable registration-error
          (try
            (close! resource)
            (catch Throwable close-error
              (.addSuppressed ^Throwable registration-error close-error)
              ;; Registration failed AFTER allocation, and immediate
              ;; compensation failed too. Without a journal entry the host
              ;; can declare rollback drained and close older dependencies
              ;; while this physical resource is still live. Preserve its
              ;; cleanup as the newest obligation, ahead of prior resources,
              ;; and let checked startup expose a retryable fence on failure.
              (swap! (.-state ^StartupCleanup cleanup)
                     update :remaining conj
                     {:module-id module-id
                      :resource-id resource-id
                      :scope-token token
                      :ownership (if (= phase :init) :init :partial)
                      :stop (fn [_] (close! resource))
                      :system nil})
              (restore-interrupt! close-error)))
          (throw registration-error))))))

(defn acquire-init-resource!
  "Acquire one init-phase resource inside an explicitly :tracked Biff init.
   The resource is recorded before the remaining init callback executes.
   On startup failure, rollback closes it after all started modules; on
   success, Biff stop closes it AFTER all module stop callbacks. A blocked
   cleanup retains a retryable handle. This helper does not trace arbitrary
   side effects or a constructor that throws before returning."
  [resource-id acquire! close!]
  (register-owned-resource! :init resource-id acquire! close!))

(defn acquire-startup-resource!
  "Acquire a resource during an opted-in Biff module's checked :start.

   The caller provides a qualified resource ID, zero-arg acquisition function
   and one-arg compensating close function. Once acquisition returns, its
   cleanup is journaled IMMEDIATELY, before subsequent initialization steps.
   If that same :start later throws, rollback closes these partial resources
   in reverse acquisition order, before earlier completed Biff modules.

   A successful :start TRANSFERS their ownership to the module's ordinary
   :biff.core/stop callback. Such modules MUST supply a real :stop which
   closes the resources they acquired; the startup helper cannot verify that
   callback's implementation. If journaling itself fails, the newly acquired
   resource is compensated immediately and the failure propagates.

   Only a monitored module inside start-biff-application! has this scope.
   Unchecked biff/start and arbitrary module callbacks have no implicit
   acquisition owner. Allocation that throws before returning, and resources
   allocated without this helper, remain the allocating module's obligation."
  [resource-id acquire! close!]
  (register-owned-resource! :start resource-id acquire! close!))

(defn- record-started!
  [^StartupCleanup cleanup module system token]
  ;; Transfer one module's partial registrations atomically to the *returned*
  ;; Biff module stop. If it has acquired resources but lacks a stop callback,
  ;; reject the returned system instead of silently releasing its ownership.
  ;; The surrounding wrapper then rolls back its partial acquisitions.
  (swap! (.-state cleanup)
         (fn [s]
           (when-not (contains? (:open-scopes s) token)
             (reject! :closed-startup-ownership
                      {:module-id (:biff.core/id module)}))
           (let [partials (filter #(identical? token (:scope-token %))
                                  (:remaining s))]
             (when (and (seq partials) (not (fn? (:biff.core/stop module))))
               (reject! :missing-owned-module-stop
                        {:module-id (:biff.core/id module)}))
             (-> s
                 (assoc :remaining
                        (conj (vec (remove #(identical? token (:scope-token %))
                                           (:remaining s)))
                              {:module-id (:biff.core/id module)
                               :stop (:biff.core/stop module)
                               :system system}))
                 (update :open-scopes disj token))))))

(defn- cleanup-started!
  [^StartupCleanup cleanup]
  (locking cleanup
    (loop []
      (let [remaining (:remaining @(.-state cleanup))]
        (if (empty? remaining)
          (do
            (swap! (.-state cleanup) assoc :status :drained)
            {:status :drained :remaining-module-ids []})
          (let [{:keys [module-id resource-id ownership stop system]}
                (peek remaining)]
            ;; Reverse-order teardown, just like Biff's native stop-system.
            ;; A failed stop fences earlier dependencies instead of closing
            ;; them underneath a possibly still-running worker or service.
            (let [failure (try
                            (when stop (stop system))
                            nil
                            (catch Throwable cause
                              (restore-interrupt! cause)
                              cause))]
              (if failure
                (do
                  (swap! (.-state cleanup) assoc :status :blocked)
                  {:status :blocked
                   :blocked-module-id module-id
                   :blocked-resource-id (when (#{:partial :init} ownership) resource-id)
                   :remaining-module-ids (mapv :module-id remaining)
                   :cause failure})
                (do
                  (swap! (.-state cleanup)
                         (fn [s]
                           (-> s (update :remaining pop)
                               (update :stopped-module-ids conj module-id))))
                  (recur))))))))))

(defn retry-startup-cleanup!
  "Retry a blocked checked-startup rollback without closing dependencies out
   of order. Accepts ONLY the opaque :cleanup/handle from the checked startup
   exception. Repeated calls after :drained are no-ops. If the failing stop
   still cannot complete, returns :blocked and retains its dependent modules.

   The host MUST retain this handle and keep its resources alive until cleanup
   drains. Only declared init resources acquired via acquire-init-resource!
   are tracked. Arbitrary init effects, unregistered resources and constructors
   that throw before returning remain outside the guarantee."
  [handle]
  (when-not (instance? StartupCleanup handle)
    (reject! :invalid-startup-cleanup-handle {}))
  (cleanup-started! handle))

(defn- admit-init-modules!
  "Admission is a declared host policy, not a proof that any callback is pure."
  [modules-var]
  (doseq [m @modules-var :when (contains? m :biff.core/init)]
    (let [policy (get m init-policy-key)
          init (:biff.core/init m)]
      (when-not (#{:declared-pure :tracked} policy)
        (reject! :unadmitted-init {:module-id (:biff.core/id m)}))
      (when (and (= :tracked policy) (not (fn? init)))
        (reject! :tracked-init-must-be-callable
                 {:module-id (:biff.core/id m)}))))
  modules-var)

(defn- monitored-modules-var
  [modules-var ^StartupCleanup cleanup]
  ;; Biff :init and :on-tx capture the modules Var. Use a stable anonymous
  ;; Var rooted in our wrapped inventory, NOT a temporary dynamic binding or
  ;; mutation of the host's modules Var. This preserves native Biff stop-chain
  ;; construction and the canonical application-handler function identity.
  ;; It is intentionally a startup snapshot: subsequent edits to the host
  ;; module Var do not hot-reload this checked lifecycle's wrapped inventory.
  (clojure.lang.Var/create
   (mapv
    (fn [module]
      (let [id (:biff.core/id module)
            init (:biff.core/init module)
            start (:biff.core/start module)
            policy (get module init-policy-key)
            watched-init
            (when (and (fn? init) (= :tracked policy))
              (fn [modules-var]
                (let [token (Object.)]
                  (swap! (.-state cleanup) update :open-scopes conj token)
                  (binding [*startup-ownership* {:cleanup cleanup
                                                 :token token
                                                 :module-id id
                                                 :phase :init}]
                    (try
                      (let [result (init modules-var)]
                        (swap! (.-state cleanup) update :open-scopes disj token)
                        result)
                      (catch Throwable error
                        (swap! (.-state cleanup) update :open-scopes disj token)
                        (throw error)))))))
            watched-start
            (when (and (fn? start)
                       (not= application/application-handler-module-id id))
              (fn [system]
                (let [token (Object.)]
                  (swap! (.-state cleanup) update :open-scopes conj token)
                  (binding [*startup-ownership* {:cleanup cleanup
                                                 :token token
                                                 :module-id id
                                                 :phase :start}]
                    (try
                      (let [next-system (start system)]
                        (record-started! cleanup module next-system token)
                        next-system)
                      (catch Throwable error
                        (swap! (.-state cleanup) update :open-scopes disj token)
                        (throw error)))))))]
        (cond-> module
          watched-init (assoc :biff.core/init watched-init)
          watched-start (assoc :biff.core/start watched-start))))
    @modules-var)))

(defn- retain-init-on-success!
  [^StartupCleanup cleanup system]
  ;; Successful startup owns module resources via native Biff stop callbacks.
  ;; The journal retains ONLY :init acquisitions, which precede all :start
  ;; resources and therefore must close AFTER all module stop callbacks.
  (if-not (map? system)
    (do
      ;; Keep compatibility with non-Biff mock start results that contain no
      ;; tracked init resources, but never let real acquired resources vanish.
      (when (seq (filter #(= :init (:ownership %))
                         (:remaining @(.-state cleanup))))
        (reject! :invalid-startup-result {}))
      system)
    (let [native-stop (:biff.core/stop-system system)]
      (when-not (ifn? native-stop)
        (reject! :missing-native-stop-chain {}))
      (swap! (.-state cleanup)
             (fn [s]
               (when (seq (:open-scopes s))
                 (reject! :unfinished-ownership-scopes {}))
               (assoc s :status :started
                      :remaining (filterv #(= :init (:ownership %))
                                          (:remaining s)))))
      (let [native-stopped? (atom false)]
        (assoc system :biff.core/stop-system
               (fn []
                 (locking cleanup
                   (when-not @native-stopped?
                     ;; Keep initialized resources alive if a provider's
                     ;; stop refuses to drain. Retrying Biff stop remains the
                     ;; host's responsibility under the native Biff contract.
                     (try
                       (native-stop)
                       (catch InterruptedException error
                         ;; The provider stop may have cleared the caller's
                         ;; interrupt flag before throwing. It has NOT drained:
                         ;; retain init resources and allow explicit retry.
                         (restore-interrupt! error)
                         (throw error)))
                     (reset! native-stopped? true))
                   (let [{:keys [status blocked-module-id blocked-resource-id
                                 cause]} (cleanup-started! cleanup)]
                     (when (= :blocked status)
                       (throw (ex-info
                               "Biff stopped its modules, but init cleanup is blocked."
                               {:error/type :gesso.live.outbox/init-cleanup-blocked
                                :blocked-module-id blocked-module-id
                                :blocked-resource-id blocked-resource-id
                                :cleanup/handle cleanup}
                               cause)))))))))))

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
   a direct biff/start for the source-bound outbox profile.

   Unlike raw Biff 2 start, this checked startup journals each completed
   provider :start and compensates in reverse order if a later startup fails.
   If a stop fails, the checked startup throws with :rollback/status :blocked
   and :cleanup/handle for retry-startup-cleanup!. Earlier providers remain
   open until the failed stop drains. Module-local partial allocation BEFORE
   :start returns are now covered ONLY when that provider uses
   acquire-startup-resource!. Initialization functions must declare either
   :declared-pure or :tracked under init-policy-key. Only explicitly registered
   init resources have guaranteed rollback and post-stop teardown; an arbitrary
   callback declaring itself pure is a trusted assertion, not a purity proof.
   The tracked modules Var is fixed at startup; dynamic hot reload is outside
   this profile."
  [assembly initial-system modules-var start-order]
  (validate-config! initial-system)
  (require-canonical-module-and-order! modules-var start-order)
  ;; Reject unadmitted init callbacks BEFORE Biff can run any of them.
  (admit-init-modules! modules-var)
  (let [cleanup (->StartupCleanup
                 (atom {:status :starting :remaining []
                        :stopped-module-ids [] :open-scopes #{}}))
        tracked-modules (monitored-modules-var modules-var cleanup)]
    (try
      (retain-init-on-success!
       cleanup
       (application/start-biff-application!
        assembly initial-system tracked-modules start-order))
      (catch Throwable startup-cause
        (let [{:keys [status blocked-module-id blocked-resource-id
                      remaining-module-ids cause]}
              (cleanup-started! cleanup)
              error (ex-info
                     "Checked Biff startup failed; completed modules were rolled back or fenced."
                     (cond-> {:error/type :gesso.live.outbox/checked-startup-failed
                              :rollback/status status
                              :startup/cause-class (.getName (class startup-cause))}
                       (= :blocked status)
                       (assoc :rollback/blocked-module-id blocked-module-id
                              :rollback/blocked-resource-id blocked-resource-id
                              :rollback/remaining-module-ids remaining-module-ids
                              :rollback/stop-cause-class (.getName (class cause))
                              :cleanup/handle cleanup))
                     startup-cause)]
          ;; Roll back physical resources before reinstating the startup
          ;; thread's interrupt. An interrupted rollback stop already restored
          ;; its own flag in cleanup-started!, even when cleanup was blocked.
          (restore-interrupt! startup-cause)
          ;; Preserve both failures for operators: the startup exception is
          ;; the cause; a failed teardown is a suppressed exception.
          (when cause
            (.addSuppressed ^Throwable error ^Throwable cause))
          (throw error))))))
