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
            [gesso.assurance.operational-report :as operational-report]
            [gesso.assurance.reference-mapped-observed-recovery :as observed-recovery]
            [gesso.assurance.reference-physical-envelope-argument :as physical-envelope]
            [gesso.assurance.reference-installed-writer-history :as writer-history]
            [gesso.assurance.reference-configured-recovery-inventory :as recovery-inventory]
            [gesso.assurance.reference-retained-acquisition :as retained-acquisition]
            [gesso.assurance.reference-storage-identity :as storage]
            [gesso.assurance.reference-writer-isolation :as isolation]
            [gesso.assurance.reference-xtdb-provider-contract :as provider-contract]
            [gesso.assurance.reference-xtdb-provider-trust :as provider-trust]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.affordance :as affordance]
            [gesso.live.core :as live]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.outbox.command-authority-owner :as command-authority]
            [gesso.live.outbox.http :as http]
            [gesso.live.outbox.history-controller :as history-controller]
            [gesso.live.outbox.http-jdk :as jdk]
            [gesso.live.outbox.http-fragment :as fragment-http]
            [gesso.live.outbox.http-operation :as operation-http]
            [gesso.live.outbox.http-recovery :as recovery]
            [gesso.live.outbox.http-sse :as assembled-sse]
            [gesso.live.outbox.lifecycle :as lifecycle]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.live.outbox.publication-manifest :as manifest]
            [gesso.live.outbox.writer-policy :as writer-policy]
            [gesso.live.outbox.worker-capture :as worker-capture]
            [gesso.model.inductive-plan :as inductive]
            [xtdb.node :as xtn]))

(def xtdb-module-id :gesso.live.outbox.host/xtdb)
(def live-module-id :gesso.live.outbox.host/live)
(def config-key :gesso.live.outbox.host/config)
(def listener-module-id :gesso.live.outbox.host/jdk-http)
(def listener-key :gesso.live.outbox.host/listener)
(def sse-status-key :gesso.live.outbox.host/sse-status)
(def recovery-status-key :gesso.live.outbox.host/recovery-status)

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
  #{:xtdb-config :xtdb-submission-policy! :mapped-writer-policy :history-audit :live-options :source-id :worker-options :rules-context
    :stop-timeout-ms :http-jdk :authorized-sse :sse-recovery :assembled-operations :assembled-fragments :assembled-sse :assembled-publication :storage-trust :writer-isolation :provider-trust})
(def ^:private allowed-live-options
  #{:rules :source-options :dispatch-options})

(def ^:private storage-policy-keys
  #{:comparison-id :relation :identity-policy :origin-boundary
    :acquisition-boundary :identity-boundary})

(defn- validate-storage-trust! [config]
  (when (contains? config :storage-trust)
    (let [{:keys [deployment-id acceptance policy] :as trust} (:storage-trust config)]
      (when-not (and (map? trust) (not (record? trust)) (nil? (meta trust))
                     (= #{:deployment-id :acceptance :policy} (set (keys trust)))
                     (qualified-keyword? deployment-id)
                     (= :trust-open-premises acceptance)
                     (map? policy) (not (record? policy)) (nil? (meta policy))
                     (= storage-policy-keys (set (keys policy)))
                     (contains? storage/relations (:relation policy))
                     (every? qualified-keyword? (vals (dissoc policy :relation))))
        (reject! :invalid-storage-trust {}))
      (when-not (:mapped-writer-policy config)
        (reject! :storage-trust-requires-mapped-writer {})))))

(def ^:private isolation-premise-ids
  #{::isolation/database-owner-binding ::isolation/in-process-writer-exclusion
    ::isolation/external-writer-exclusion ::isolation/storage-and-log-writer-exclusion
    ::isolation/lifecycle-isolation})

(defn- validate-writer-isolation! [config]
  (when (contains? config :writer-isolation)
    (let [{:keys [acceptance premise-ids] :as trust} (:writer-isolation config)]
      (when-not (and (map? trust) (not (record? trust)) (nil? (meta trust))
                     (= #{:deployment-id :database-id :isolation-boundary :acceptance :premise-ids}
                        (set (keys trust)))
                     (every? qualified-keyword? (map trust [:deployment-id :database-id :isolation-boundary]))
                     (= :trust-open-premises acceptance)
                     (set? premise-ids) (nil? (meta premise-ids))
                     (= isolation-premise-ids premise-ids))
        (reject! :invalid-writer-isolation {}))
      (when-not (:mapped-writer-policy config)
        (reject! :writer-isolation-requires-mapped-writer {})))))

(def ^:private writer-isolation-hosts (java.util.WeakHashMap.))
(def ^:private original-isolation-prepare isolation/prepare!)
(def ^:private original-isolation-declare isolation/declare!)
(def ^:private original-isolation-cases outcome/derive-cases!)
(def ^:private original-isolation-admission inductive/require-current-mapped-admission!)

(defn- register-writer-isolation! [system token lock active trust]
  (locking writer-isolation-hosts
    (.put writer-isolation-hosts token
          {:system (java.lang.ref.WeakReference. system)
           :lock lock :active active :trust trust})))

(def ^:private provider-premise-ids
  #{::provider-contract/decoded-assertions ::provider-contract/ordered-transaction-basis
    ::provider-contract/put-replacement ::provider-contract/commit-abort
    ::provider-contract/worker-clock ::provider-contract/pinned-read
    ::provider-contract/retained-history ::provider-contract/valid-time-boundaries})
(def ^:private reviewed-provider-target provider-contract/reviewed-provider)

(defn- validate-provider-trust! [config]
  (when (contains? config :provider-trust)
    (let [{:keys [provider acceptance premise-ids] :as trust} (:provider-trust config)]
      (when-not (and (map? trust) (not (record? trust)) (nil? (meta trust))
                     (= #{:provider :acceptance :premise-ids} (set (keys trust)))
                     (= reviewed-provider-target provider)
                     (= :trust-open-premises acceptance)
                     (set? premise-ids) (nil? (meta premise-ids))
                     (= provider-premise-ids premise-ids))
        (reject! :invalid-provider-trust {}))
      (when-not (:mapped-writer-policy config)
        (reject! :provider-trust-requires-mapped-writer {})))))

(def ^:private provider-trust-hosts (java.util.WeakHashMap.))
(def ^:private original-provider-prepare provider-trust/prepare!)
(def ^:private original-provider-declare provider-trust/declare!)
(def ^:private original-provider-token xtdb/current-submission-policy-token)
(def ^:private original-provider-state xtdb/owned-submission-policy-state)

(defn- register-provider-trust! [system token lock active trust]
  (locking provider-trust-hosts
    (.put provider-trust-hosts token
          {:system (java.lang.ref.WeakReference. system)
           :lock lock :active active :trust trust})))

(def ^:private storage-trust-hosts (java.util.WeakHashMap.))
(def ^:private original-target-match observed-recovery/conditional-recovery-target-matches?)
(def ^:private original-scope-reader observed-recovery/read-conditional-recovery-scope!)
(def ^:private original-trust-description observed-recovery/declare-conditional-recovery-trust!)
(def ^:private original-storage-obligations storage/describe-obligations)

(defn- register-storage-trust! [system token lock active trust]
  (locking storage-trust-hosts
    (.put storage-trust-hosts token
          {:system (java.lang.ref.WeakReference. system)
           :lock lock :active active :trust trust}))
  system)

;; Registration is internal to the installed host startup path, not an
;; application-facing method for minting host ownership.
(def ^:private register-worker-capture-host! @#'worker-capture/register-worker-capture-host!)
(def ^:private register-command-authority! @#'command-authority/register!)
(def read-command-authority-binding! command-authority/read-binding!)
(def qualify-command-authority! command-authority/qualify!)
(def adopt-command-authority! command-authority/adopt!)
(def capture-worker-outcomes! worker-capture/capture-worker-outcomes!)
(def worker-outcome-capture-matches? worker-capture/worker-outcome-capture-matches?)

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
  (when (and (contains? config :authorized-sse) (contains? config :assembled-sse))
    (reject! :conflicting-sse-profiles {}))
  (when (contains? config :sse-recovery)
    (when-not (or (contains? config :authorized-sse) (contains? config :assembled-sse))
      (reject! :recovery-requires-authorized-sse {}))
    (let [options (:sse-recovery config)]
      (when-not (and (map? options)
                     (every? #{:poll-ms :min-interval-ms :max-attempts
                               :max-failure-interval-ms} (keys options)))
        (reject! :invalid-sse-recovery-options {}))))
  (when (and (contains? config :xtdb-submission-policy!)
             (not (fn? (:xtdb-submission-policy! config))))
    (reject! :invalid-xtdb-submission-policy {}))
  (when (contains? config :mapped-writer-policy)
    (when (contains? config :xtdb-submission-policy!)
      (reject! :conflicting-xtdb-submission-policies {}))
    (writer-policy/validate-config! (:mapped-writer-policy config)))
  (when (contains? config :history-audit)
    (when-not (:mapped-writer-policy config)
      (reject! :history-audit-requires-mapped-writer {}))
    (history-controller/validate-options! (:history-audit config)))
  (validate-storage-trust! config)
  (validate-writer-isolation! config)
  (validate-provider-trust! config)
  (when-not (map? (:xtdb-config config))
    (reject! :invalid-xtdb-config {}))
  (let [options (:live-options config)]
    (when-not (map? options)
      (reject! :invalid-live-options {}))
    (when-let [extra (seq (remove allowed-live-options (keys options)))]
      (reject! :unowned-live-component {:fields (vec extra)}))
    (if (contains? config :assembled-publication)
      (when (contains? options :rules)
        (reject! :conflicting-publication-rules {}))
      (when-not (vector? (:rules options))
        (reject! :invalid-live-rules {})))
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

(defn- require-unshadowed-sse-path! [assembly config]
  ;; The SSE adapter owns ALL methods at its literal path (including 405
  ;; responses), not just GET. Compare against the admitted route products,
  ;; never against a caller-provided list of supposedly installed routes.
  ;; Stream routes are intentionally not excluded: this adapter may realize
  ;; one. This is a collision check in the existing simple template language,
  ;; not proof of arbitrary router semantics or complete stream adoption.
  (when-let [path (get-in config [:authorized-sse :path])]
    (when (string? path)
      (let [acquisition (:operation-acquisition-assembly assembly)
            operations (get-in acquisition [:execution-assembly :route-assembly :routes])
            fragments (get-in acquisition [:acquisition-assembly :realizations])
            protected (concat
                       (for [[id route] operations]
                         {:kind :operation :id id :path (:path route)
                          :method (:method route)})
                       (for [[id realization] fragments
                             :let [route (:fragment-route realization)]]
                         {:kind :fragment :id id :path (:path route)
                          :method (:method route)}))
            conflicts (->> protected
                           (filter #(affordance/route-template-matches? (:path %) path))
                           (sort-by (juxt :kind (comp pr-str :id)))
                           vec)]
        (when (seq conflicts)
          (reject! :sse-path-shadows-application-route
                   {:path path :conflicts conflicts}))))))

(defn- start-xtdb! [system]
  (when (and (get-in system [config-key :mapped-writer-policy])
             (not (instance? gesso.live.outbox.lifecycle.ManagedLineage
                             (get system managed/managed-lineage-key))))
    (reject! :missing-managed-worker-lineage {}))
  (let [release (atom nil)
        owned-policy (atom nil)
        provider-closed? (atom false)
        policy-token (volatile! nil)
        audit (atom nil)
        timeout-ms (get-in system [config-key :stop-timeout-ms] 10000)
        close! (fn [node]
                 (when-let [controller @audit] ((:close! controller) timeout-ms))
                 ;; The worker, request, SSE and audit owners have drained
                 ;; before this callback. Also fence direct adapter callers
                 ;; that are not part of those owners. Startup rollback uses
                 ;; the same close callback. Timeout retains the node and
                 ;; registration for retry.
                 (when-not @provider-closed?
                   (when-let [owner @owned-policy]
                     ((:fence-and-await! owner) timeout-ms))
                   (.close ^java.lang.AutoCloseable node)
                   (reset! provider-closed? true))
                 (when-let [f @release] (f)))
        node (managed/acquire-startup-resource!
              :gesso.live.outbox.host/node
              #(xtn/start-node (get-in system [config-key :xtdb-config]))
              close!)]
    (let [config (get system config-key)
          policy (if-let [mapped (:mapped-writer-policy config)]
                   (writer-policy/build-policy!
                    node mapped {:source-id (:source-id config)
                                 :worker-id (get-in config [:worker-options :worker-id])
                                 :require-worker-call-chain? true
                                 :managed-lineage (get system managed/managed-lineage-key)
                                 :descriptor (::publication-manifest system)})
                   (:xtdb-submission-policy! config))]
      (when policy
        (if (:mapped-writer-policy config)
          (let [owner (xtdb/register-owned-submission-policy! node policy)]
            (reset! owned-policy owner)
            (reset! release (:release! owner)))
          (reset! release (xtdb/register-submission-policy! node policy)))
        (vreset! policy-token (xtdb/submission-policy-token node policy)))
      (manifest/require-compatible! node (:source-id config)
                                    (::publication-manifest system))
      (when (:mapped-writer-policy config)
        (writer-policy/activate-policy! policy))
      (when-let [options (:history-audit config)]
        (reset! audit (history-controller/start! node (get-in config [:mapped-writer-policy :admission]) options))))
    (cond-> (assoc system :xtdb/connectable node ::close-node! close!)
      (get-in system [config-key :mapped-writer-policy :admission])
      (assoc ::mapped-admission (get-in system [config-key :mapped-writer-policy :admission])
             ::mapped-policy-token @policy-token)
      @audit (assoc ::history-controller @audit))))

(defn- stop-xtdb! [system]
  ((::close-node! system) (:xtdb/connectable system)))

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
             (handler (merge (dissoc request :xtdb/read-connectable :xtdb/conn :xtdb/node
                                     :biff.xtdb/connection-pool publication/preparation-key)
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
         (let [provider (if (get-in system [config-key :mapped-writer-policy])
                          (xtdb/adapter-connectable! node)
                          node)
               [before _] (swap-vals!
                           state
                           #(if (= :starting (:phase %))
                              (assoc % :phase :open
                                     :context {:xtdb/connectable provider
                                               :biff.xtdb/node provider
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

(defn- retire-recovery! [scheduler timeout-ms]
  (when scheduler
    (let [result ((:stop! scheduler) timeout-ms)]
      (when-not (and (= {:outcome :drained} result)
                     (true? ((:drained? scheduler))))
        (reject! :sse-recovery-not-drained {:outcome (:outcome result)})))))

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
   Optional :xtdb-submission-policy! is a trusted admission predicate installed
   on the owned node before manifest establishment and retained until close.
   Alternatively :mapped-writer-policy binds the selected framed executor
   to the exact owned node, with durable one-time startup initialization and restricted
   framework-owned maintenance admission. The two selections are exclusive.
   Both cover adapter submit/execute calls, not direct or external XTDB writes.
   Optional :storage-trust selects one exact recovery policy and deployment
   name, explicitly trusting its open premises. Adoption is owner-only on
   the exact running host, not authentication of external lifecycle events.
   Optional :writer-isolation selects an explicit deployment/database boundary
   and exact open environment premise IDs. Adoption is a separate owner-only
   diagnostic on the running mapped generation, not verified writer exclusion.
   Optional :provider-trust selects the exact reviewed provider target and
   all open semantic premise IDs. Explicit owner adoption is separate from
   startup, storage trust and writer isolation.
   Optional :history-audit requires the mapped profile and trusted owner
   :load!, :save!, :key, :deployment-id and :max-steps options. Restore and
   persist a first bounded chunk before request admission. Further chunks
   are owner-driven via audit-history! or optional :schedule with explicit
   :catch-up-ms, :idle-ms, :failure-ms and :max-failure-ms pacing.
   Startup transport errors propagate;
   unresolved findings remain diagnostic. Stop fences and drains audit work
   before native provider shutdown, including the optional scheduler.
   :live-options allows only owned :rules, :source-options, :dispatch-options.
   :source-id, :worker-options and :rules-context are mandatory. The worker
   is installed AFTER providers, and cannot be omitted or re-ordered.

   The installed handler receives the exact owned XTDB and Live providers,
   overriding request-supplied provider aliases. The mapped writer profile
   exposes an opaque Gesso adapter handle instead of the raw XTDB node;
   application queries must use Gesso adapters, not direct XTDB/Biff JDBC.
   The returned owner system still holds the raw node. Admission opens only after
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
   Install after managed startup but before opening admission. Every owned
   SSE route also gets a bounded scheduled orphan-recovery controller;
   :sse-recovery optionally supplies its numeric pacing and batch options.
   Optional :assembled-operations selects the admitted literal or simple parameterized HTMX POST
   operation transport with an explicit trusted :authorize-request? callback.
   Optional :assembled-fragments mounts the compiled model fragment GETs
   using a trusted :resolve-id callback and mandatory model authorization.
   Optional :assembled-sse mounts the compiled acquisition stream inventory,
   using the model scope authorization and optional trusted :session-authorized?
   predicate on the prepared request context at admission and every frame.
   Applications requiring session revocation must consult current session
   validity, not merely the original request identity. Publication rules remain explicit
   unless :assembled-publication is selected. That profile requires an
   explicit :subject-keys mapping for the admitted graph and replaces
   :live-options :rules (which must be absent). Only unconditional graphs
   reconstructible from one durable subject per topic are admitted. A durable
   source manifest fences changes to that expansion vocabulary and refuses
   automatic adoption of preexisting unclassified intents. When
   assembled operations are also selected, their trusted executors receive
   a command-bound publication preparer using that same subject mapping.
   Recognized publication/build-executor callbacks require both profiles
   and compatible declared topics before provider startup. All
   streams share one connection budget and recovery controller. Do not
   combine :assembled-sse with the legacy :authorized-sse profile.
   Other routes retain their existing trusted delegate.
   No SSE route means no scheduler. On shutdown, drain Ring requests, stop and
   drain recovery, retire route-owned SSE sessions, then run native stop
   (worker, listener writers, Live, XTDB). Failed recovery drain or SSE
   retirement retains providers for retry. Arbitrary application streams
   remain the caller's responsibility. Returned status maps expose neither
   an independent handler nor independent recovery or retirement controls."
  [assembly handler config]
  (validate-config! config)
  (when-not (fn? handler)
    (reject! :invalid-handler {}))
  (when-not (application/application-assembly? assembly)
    (reject! :invalid-application-assembly {}))
  (require-unshadowed-sse-path! assembly config)
  (publication/require-host-wiring! assembly config)
  (let [profile (when (contains? config :assembled-publication)
                  (publication/profile! assembly (:assembled-publication config)))
        config (if profile
                 (assoc-in config [:live-options :rules] (:rules profile))
                 config)
        sse-plan (when (contains? config :assembled-sse)
                   (assembled-sse/prepare! assembly (:assembled-sse config)))
        handler (if (contains? config :assembled-fragments)
                  (fragment-http/wrap-handler assembly handler (:assembled-fragments config))
                  handler)
        handler (if (contains? config :assembled-operations)
                  (operation-http/wrap-handler
                   assembly handler (:assembled-operations config)
                   (when profile
                     (fn [{:keys [command principal]}]
                       ((:bind-command profile) (:source-id config)
                        (assoc command :principal principal)))))
                  handler)
        route* (atom nil)
        recovery* (atom nil)
        timeout-ms (get config :stop-timeout-ms 10000)
        admission (request-admission
                   (fn [request]
                     ((or (:handler @route*) handler) request))
                   timeout-ms)
        initial (cond-> {config-key config
                         ::publication-manifest (:manifest profile)
                         :biff.ring/handler (:handler admission)
                         managed/source-id-key (:source-id config)
                         managed/options-key (:worker-options config)
                         managed/context-key (:rules-context config)}
                  (:mapped-writer-policy config)
                  (assoc managed/managed-lineage-key (lifecycle/new-managed-lineage))
                  (contains? config :stop-timeout-ms)
                  (assoc managed/stop-timeout-key (:stop-timeout-ms config)))]
    (let [network? (contains? config :http-jdk)
          started (managed/start-biff-application!
                   assembly initial
                   (if network? #'network-modules #'modules)
                   (module-ids network?))
          native-stop (:biff.core/stop-system started)
          stop-lock (Object.)
          trust-token (Object.)
          isolation-token (Object.)
          provider-trust-token (Object.)
          worker-capture-token (Object.)
          trust-active (atom true)
          stop-owned! (fn []
                        (locking stop-lock
                          (reset! trust-active false)
                          ((:close-and-await! admission))
                          (retire-recovery! @recovery* timeout-ms)
                          (retire-route! @route* timeout-ms)
                          (when-let [controller (::history-controller started)]
                            ((:close! controller) timeout-ms))
                          (native-stop)))]
      ;; Wrap the actual native stop chain, not just host/stop!: a caller of
      ;; biff/stop on this returned system must encounter the same fence.
      (try
        (when (or sse-plan (contains? config :authorized-sse))
          ;; The route's fallback must be the original delegate, not the
          ;; admission wrapper (which would recurse). Its invocation remains
          ;; inside the canonical HTML guard and owned request context.
          (let [owned (assoc started :biff.ring/handler handler)]
            (reset! route* (if sse-plan
                            ((:wrap! sse-plan) owned)
                            (http/wrap-authorized-sse owned (:authorized-sse config)))))
          (reset! recovery* (recovery/start-scheduled!
                             @route* (get config :sse-recovery {}))))
        ((:open! admission) started)
        (let [system (cond-> (assoc started :biff.core/stop-system stop-owned!)
          @route* (assoc sse-status-key
                         (select-keys @route* [:active-count :occupied-count
                                              :pending-authorization-count
                                              :disconnected-count :closed? :drained?]))
          @recovery* (assoc recovery-status-key
                            (select-keys @recovery* [:status :stats :drained?]))
                       (:storage-trust config) (assoc ::storage-trust-token trust-token)
                       (:writer-isolation config) (assoc ::writer-isolation-token isolation-token)
                       (:provider-trust config) (assoc ::provider-trust-token provider-trust-token)
                       (get-in config [:assembled-operations :authority-policy]) (assoc command-authority/token-key (Object.))
                       (::mapped-policy-token started) (assoc ::worker-capture-token worker-capture-token))]
          (when (get system command-authority/token-key)
            (register-command-authority! system stop-lock trust-active handler assembly (:assembled-operations config)))
          (when (::worker-capture-token system)
            (register-worker-capture-host! system worker-capture-token trust-active))
          (when-let [trust (:writer-isolation config)]
            (register-writer-isolation! system isolation-token stop-lock trust-active trust))
          (when-let [trust (:provider-trust config)]
            (register-provider-trust! system provider-trust-token stop-lock trust-active trust))
          (if-let [trust (:storage-trust config)]
            (register-storage-trust! system trust-token stop-lock trust-active trust)
            system))
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

(defn history-status
  "Observe the last successfully persisted history report and controller phase.
   Nil means the owner did not select historical auditing. No cursor or key."
  [system]
  (when-let [controller (::history-controller system)] ((:status controller))))

(defn assurance-status
  "Portable owner-facing operational assurance report. No provider query,
   transaction authority or certification. Coverage is relative to the last
   persisted audit sample, including after host shutdown."
  [system]
  (operational-report/describe-history (history-status system)))

(defn combined-assurance-status
  "Owner-facing static/operational diagnostic. Recheck the original abstract
   source and live handles of the admission captured at startup, then compare
   with one history-controller snapshot. No provider query or certification."
  [system]
  (operational-report/describe-application
   (when-let [admission (::mapped-admission system)]
     (inductive/recheck-mapped-induction! admission))
   (history-status system)))

(defn audit-history!
  "Advance one bounded chunk on the exact owner system, not a Ring context."
  [system]
  (if-let [controller (::history-controller system)]
    ((:advance! controller))
    (reject! :history-audit-not-selected {})))

(defn stop!
  "Use the exact Biff system returned by start!. A drain timeout propagates
   and retains the undrained provider and its dependencies. At the late
   mapped adapter fence, Live and the worker may already be stopped, but
   XTDB stays open and fenced. Retry with the SAME system."
  [system]
  (when-not (and (map? system) (some? (get system managed/handle-key)))
    (reject! :invalid-running-system {}))
  (biff/stop system))

(defn adopt-storage-trust!
  "Adopt the startup-selected trust declaration for an issued comparison on
   the exact running owner system. No request context, caller declaration,
   or reconstructed system is accepted. This establishes local configured
   adoption only, not authenticated lifecycle evidence or proof discharge.
   Returns sensitive internal assurance material, not a public receipt."
  [system comparison]
  (let [entry (locking storage-trust-hosts
                (.get storage-trust-hosts (::storage-trust-token system)))]
    (when-not (and entry (identical? system (.get ^java.lang.ref.WeakReference (:system entry))))
      (reject! :unissued-storage-trust-host {}))
    (locking (:lock entry)
      (when-not @(:active entry)
        (reject! :storage-trust-host-retired {}))
      (when-not (and (identical? original-target-match observed-recovery/conditional-recovery-target-matches?)
                     (identical? original-scope-reader observed-recovery/read-conditional-recovery-scope!)
                     (identical? original-trust-description observed-recovery/declare-conditional-recovery-trust!)
                     (identical? original-storage-obligations storage/describe-obligations))
        (reject! :storage-trust-boundary-replaced {}))
      (let [{:keys [deployment-id acceptance policy]} (:trust entry)
            scope (original-scope-reader comparison)]
        (when-not (original-target-match comparison (:xtdb/connectable system)
                                         (::mapped-admission system)
                                         (get-in system [config-key :source-id]))
          (reject! :storage-trust-target-mismatch {}))
        (when-not (= policy (select-keys scope storage-policy-keys))
          (reject! :storage-trust-policy-mismatch {}))
        (let [premises (mapv :assumption (:obligations (original-storage-obligations scope)))
              result (original-trust-description comparison
                                                 {:deployment-id deployment-id
                                                  :identity-policy (:identity-policy policy)
                                                  :acceptance acceptance :premises premises})]
          (if (= :conditional (:status result))
            (assoc result :deployment-adoption :configured-local-host)
            result))))))


(defn adopt-writer-isolation!
  "Apply the startup-selected isolation declaration to the exact running
   owner system and original mapped source. Recheck source and closed outcome
   cases, and issue a fresh generation-scoped diagnostic. No request context,
   caller-supplied deployment or copied host is accepted. The configured
   trust is not actual exclusion, continuous mediation or storage lineage."
  [system source declarations]
  (let [entry (locking writer-isolation-hosts
                (.get writer-isolation-hosts (::writer-isolation-token system)))]
    (when-not (and entry (identical? system (.get ^java.lang.ref.WeakReference (:system entry))))
      (reject! :unissued-writer-isolation-host {}))
    (locking (:lock entry)
      (when-not @(:active entry)
        (reject! :writer-isolation-host-retired {}))
      (when-not (and (identical? original-isolation-prepare isolation/prepare!)
                     (identical? original-isolation-declare isolation/declare!)
                     (identical? original-isolation-cases outcome/derive-cases!)
                     (identical? original-isolation-admission inductive/require-current-mapped-admission!))
        (reject! :writer-isolation-host-boundary-replaced {}))
      (let [{:keys [acceptance premise-ids] :as trust} (:trust entry)
            node (:xtdb/connectable system)
            admission (::mapped-admission system)
            description (original-isolation-admission admission source)
            cases (:cases (original-isolation-cases source declarations))
            prepared (original-isolation-prepare node admission source declarations
                                                 (:catalog-fingerprint description) cases
                                                 (dissoc trust :acceptance :premise-ids))]
        (when-not (and (= :open-isolation-requirements (:status prepared))
                       (= premise-ids (set (map #(get-in % [:assumption :id]) (:obligations prepared)))))
          (reject! :writer-isolation-host-scope-mismatch {:reason/detail (:reason prepared)}))
        (let [result (original-isolation-declare node admission (:handle prepared)
                                                {:scope (:scope prepared) :acceptance acceptance
                                                 :premises (mapv :assumption (:obligations prepared))})]
          (if (= :conditional-declared-writer-isolation (:status result))
            (assoc result :deployment-adoption :configured-local-host)
            result))))))


(defn- provider-adoption-boundaries! []
  (when-not (and (= reviewed-provider-target provider-contract/reviewed-provider)
                 (identical? original-provider-prepare provider-trust/prepare!)
                 (identical? original-provider-declare provider-trust/declare!)
                 (identical? original-provider-token xtdb/current-submission-policy-token)
                 (identical? original-provider-state xtdb/owned-submission-policy-state)
                 (identical? original-isolation-cases outcome/derive-cases!)
                 (identical? original-isolation-admission inductive/require-current-mapped-admission!))
    (reject! :provider-trust-host-boundary-replaced {})))

(defn- require-open-provider-generation! [system]
  (let [node (:xtdb/connectable system) token (::mapped-policy-token system)]
    (when-not (and token (identical? token (original-provider-token node))
                   (= :open (:phase (original-provider-state node token))))
      (reject! :provider-trust-generation-not-open {}))))

(defn adopt-provider-trust!
  "Apply the startup-selected provider contract to the original source
   on the exact running mapped owner. Each adoption prepares a fresh scope.
   Contract fingerprints are stable across equivalent generations, not a
   transferable host credential. This is explicit conditional trust, not
   provider attestation, writer isolation or storage-lineage evidence."
  [system source declarations]
  (let [entry (locking provider-trust-hosts
                (.get provider-trust-hosts (::provider-trust-token system)))]
    (when-not (and entry (identical? system (.get ^java.lang.ref.WeakReference (:system entry))))
      (reject! :unissued-provider-trust-host {}))
    (locking (:lock entry)
      (when-not @(:active entry)
        (reject! :provider-trust-host-retired {}))
      (provider-adoption-boundaries!)
      (require-open-provider-generation! system)
      (let [{:keys [provider acceptance premise-ids]} (:trust entry)
            admission (::mapped-admission system)
            description (original-isolation-admission admission source)
            cases (:cases (original-isolation-cases source declarations))
            prepared (original-provider-prepare admission source declarations
                                                (:catalog-fingerprint description) cases)]
        (when-not (and (= :open-provider-trust-requirements (:status prepared))
                       (= provider (get-in prepared [:scope :contract-scope :provider]))
                       (= premise-ids (set (keys (:premises prepared)))))
          (reject! :provider-trust-host-scope-mismatch {:reason/detail (:reason prepared)}))
        (let [result (original-provider-declare admission (:handle prepared)
                                               {:scope (:scope prepared) :acceptance acceptance
                                                :premises (mapv :assumption (vals (:premises prepared)))})]
          (provider-adoption-boundaries!)
          (require-open-provider-generation! system)
          (if (= :conditional-declared-provider-trust (:status result))
            (assoc result :deployment-adoption :configured-local-host
                   :lifecycle-evidence :open-owned-generation-at-adoption)
            result))))))

(def ^:private original-isolation-adopt adopt-writer-isolation!)
(def ^:private original-provider-adopt adopt-provider-trust!)
(def ^:private original-combined-root writer-policy/activated-policy-root)

(defn- combined-boundaries! []
  (provider-adoption-boundaries!)
  (when-not (and (identical? original-isolation-adopt adopt-writer-isolation!)
                 (identical? original-provider-adopt adopt-provider-trust!)
                 (identical? original-combined-root writer-policy/activated-policy-root))
    (reject! :combined-trust-boundary-replaced {})))

(defn- compose-implementation-trust! [description root isolation provider]
  ;; Internal diagnostic join only; the public entry point rederives both
  ;; declarations under the exact owner's shared retirement lock.
  (let [contract (get-in provider [:origin-report :contract])
        inventory (:original-inventory contract)
        binding (:original-binding inventory)
        subject (select-keys description [:catalog-fingerprint :induction-fingerprint])
        conditions (:remaining-conditions binding)
        i-fps (set (map :premise-fingerprint (:obligations isolation)))
        p-fps (set (map :premise-fingerprint (vals (:premises provider))))
        i-conditions (:related-condition-ids isolation)]
    (when-not (and (= :conditional-declared-writer-isolation (:status isolation))
                   (= :conditional-declared-provider-trust (:status provider))
                   (= :configured-local-host (:deployment-adoption isolation) (:deployment-adoption provider))
                   (= :explicit-declaration-only (:premise-acceptance isolation) (:premise-acceptance provider))
                   (= :not-performed (:proof-discharge isolation) (:proof-discharge provider) (:proof-discharge binding))
                   (= :not-established (:writer-coverage isolation) (:writer-coverage provider)
                      (:continuous-mediation isolation) (:provider-correspondence provider)
                      (:loaded-provider-identity provider) (:universal-refinement binding))
                   (= :open-provider-contract (:status contract))
                   (= :complete-open-condition-review (:status inventory))
                   (= :conditional-original-clm-r2-binding (:status binding))
                   (= :open (:original-obligation-status binding))
                   (= :not-performed (:premise-acceptance binding))
                   (= subject (select-keys (:scope isolation) (keys subject))
                      (select-keys (:scope contract) (keys subject)) (select-keys binding (keys subject)))
                   (= (:scope contract) (get-in provider [:scope :contract-scope]))
                   (= root (get-in isolation [:scope :activation-root]))
                   (= (:original-open-obligations description) (:original-open-obligation-ids-by-operation binding))
                   (= conditions (:remaining-conditions inventory) (:remaining-conditions contract)
                      (get-in provider [:scope :remaining-conditions]))
                   (= i-conditions (get-in inventory [:groups :writer-mediation :conditions]))
                   (= (:linked-conditions contract) (set (keys (:condition-links contract))))
                   (= (:condition-links contract) (get-in provider [:scope :condition-links]))
                   (= isolation-premise-ids (set (map #(get-in % [:assumption :id]) (:obligations isolation))))
                   (= provider-premise-ids (set (keys (:premises provider))))
                   (= (:premises contract) (:premises provider))
                   (= 5 (count i-fps)) (= 8 (count p-fps))
                   (= i-fps (:trusted-assumptions isolation)) (= p-fps (:trusted-assumptions provider))
                   (every? #(= :open (:discharge-state %)) (concat (:obligations isolation) (vals (:premises provider)))))
      (reject! :combined-trust-subject-or-premise-mismatch {}))
    {:status :conditional-configured-implementation-argument
     :profile :configured-clm-r2-implementation-argument-v1
     :subject subject :activation-root root
     :writer-isolation isolation :provider-trust provider :original-clm-r2-binding binding
     :trusted-assumptions-by-contract {:writer-isolation i-fps :provider-trust p-fps}
     :condition-links-by-contract {:writer-isolation i-conditions :provider-trust (:condition-links contract)}
     :remaining-conditions conditions :condition-link-semantics :related-not-entailing
     :original-obligation-status :open :premise-acceptance :explicit-declarations-only
     :continuous-mediation :not-established :provider-correspondence :not-established
     :writer-coverage :not-established :universal-refinement :not-established
     :proof-discharge :not-performed :authority :none :certificate :not-issued}))

(defn adopt-implementation-trust!
  "Compose fresh configured isolation and provider declarations at the exact
   mapped owner. Reuse the original CLM-R2 binding rederived by provider
   adoption. No supplied reports, storage-lineage trust or selected-history
   evidence enter this argument. All original conditions remain open."
  [system source declarations]
  (let [i (locking writer-isolation-hosts (.get writer-isolation-hosts (::writer-isolation-token system)))
        p (locking provider-trust-hosts (.get provider-trust-hosts (::provider-trust-token system)))]
    (when-not (and i p (identical? system (.get ^java.lang.ref.WeakReference (:system i)))
                   (identical? system (.get ^java.lang.ref.WeakReference (:system p)))
                   (identical? (:lock i) (:lock p)) (identical? (:active i) (:active p)))
      (reject! :unissued-combined-trust-host {}))
    (locking (:lock i)
      (when-not @(:active i) (reject! :combined-trust-host-retired {}))
      (combined-boundaries!)
      (require-open-provider-generation! system)
      (let [node (:xtdb/connectable system) admission (::mapped-admission system)
            token (::mapped-policy-token system)
            activation (original-combined-root node admission token)
            root (some-> activation :observation
                         (select-keys [:source-id :manifest-tx-id :initialization-tx-id :system-time :initialization-fingerprint])
                         (update :system-time str))]
        (when-not activation (reject! :combined-trust-missing-activation {}))
        (let [description (original-isolation-admission admission source)
              isolation (original-isolation-adopt system source declarations)
              provider (original-provider-adopt system source declarations)
              result (compose-implementation-trust! description root isolation provider)]
          (combined-boundaries!)
          (require-open-provider-generation! system)
          (when-not (= activation (original-combined-root node admission token))
            (reject! :combined-trust-activation-changed {}))
          result)))))

(def ^:private original-configured-adopt adopt-implementation-trust!)
(def ^:private original-configured-storage-adopt adopt-storage-trust!)
(def ^:private original-recovery-inventory recovery-inventory/compose!)
(def ^:private original-command-inventory command-authority/extend-inventory!)
(def ^:private original-physical-envelope physical-envelope/compare-recovery!)
(def ^:private original-activated-recovery observed-recovery/compare-activated!)
(def ^:private original-interleaved-envelope physical-envelope/compare-worker-interleaved!)
(def ^:private original-local-activated-recovery observed-recovery/compare-local-activated!)
(def ^:private original-recovery-acquisition retained-acquisition/read-acquisition!)
(def ^:private original-qualified-history writer-history/read-qualified-history!)
(def ^:private original-history-join writer-history/join-configured-envelope!)
(def ^:private original-refined-judgment writer-history/declare-refined-recovery-trust!)
(def ^:private original-recovery-worker-match worker-capture/worker-outcome-capture-matches?)
(def ^:private original-framed-observation publication/read-framed-submission-observation!)

(defn- configured-recovery-boundaries! []
  (combined-boundaries!)
  (when-not (and (identical? original-configured-adopt adopt-implementation-trust!)
                 (identical? original-configured-storage-adopt adopt-storage-trust!)
                 (identical? original-recovery-inventory recovery-inventory/compose!)
                 (identical? original-command-inventory command-authority/extend-inventory!)
                 (identical? original-physical-envelope physical-envelope/compare-recovery!)
                 (identical? original-activated-recovery observed-recovery/compare-activated!)
                 (identical? original-interleaved-envelope physical-envelope/compare-worker-interleaved!)
                 (identical? original-local-activated-recovery observed-recovery/compare-local-activated!)
                 (identical? original-recovery-acquisition retained-acquisition/read-acquisition!)
                 (identical? original-qualified-history writer-history/read-qualified-history!)
                 (identical? original-history-join writer-history/join-configured-envelope!)
                 (identical? original-refined-judgment writer-history/declare-refined-recovery-trust!)
                 (identical? original-recovery-worker-match worker-capture/worker-outcome-capture-matches?)
                 (identical? original-framed-observation publication/read-framed-submission-observation!))
    (reject! :configured-recovery-boundary-replaced {})))

(defn- configured-storage-declaration! [system owner comparison]
  (let [entry (locking storage-trust-hosts
                (.get storage-trust-hosts (::storage-trust-token system)))]
    (when-not (and entry (identical? system (.get ^java.lang.ref.WeakReference (:system entry)))
                   (identical? (:lock owner) (:lock entry))
                   (identical? (:active owner) (:active entry)) @(:active entry))
      (reject! :unissued-combined-storage-trust-host {}))
    (let [adopted (original-configured-storage-adopt system comparison)
          {:keys [deployment-id acceptance policy]} (:trust entry)
          scope (:judgment-scope adopted)
          declaration {:deployment-id deployment-id :acceptance acceptance
                       :identity-policy (:identity-policy policy)
                       :premises (mapv :assumption (:obligations adopted))}]
      (when-not (and (= :conditional (:status adopted))
                     (= :configured-local-host (:deployment-adoption adopted))
                     (= policy (select-keys scope storage-policy-keys))
                     (= deployment-id (get-in adopted [:deployment-acceptance :deployment-id]))
                     (= (:identity-policy policy) (get-in adopted [:deployment-acceptance :identity-policy]))
                     (= :not-established (get-in adopted [:deployment-acceptance :authentication]))
                     (= :not-performed (:proof-discharge adopted)))
        (reject! :configured-storage-judgment-mismatch {:storage-reason (:reason adopted)}))
      {:adopted adopted :declaration declaration :policy policy})))

(defn- join-historical-judgment! [history configured physical refined]
  (let [original (:historical-judgment refined)
        scope (:judgment-scope original)
        binding (:original-clm-r2-binding configured)]
    (when-not (and (= :conditional (:status refined) (:status original))
                   (= writer-history/refined-judgment-profile (:judgment-profile refined))
                   (= observed-recovery/judgment-profile (:judgment-profile original))
                   (= binding (:original-clm-r2-binding refined))
                   (= (:historical-cut history) (:historical-cut refined) (:historical-cut scope))
                   (= (get-in configured [:activation-root :source-id]) (:publication-source-id scope))
                   (= (:subject configured) (select-keys (:judgment-subject refined) (keys (:subject configured))))
                   (= (:selected-tx-ids physical) (get-in original [:original-recovery :selected-tx-ids]))
                   (= (:selected-physical-refinement history) (:selected-physical-refinement refined))
                   (= (:selected-temporal-coverage history) (:selected-temporal-coverage refined))
                   (= (:remaining-conditions history) (:selected-remaining-conditions refined))
                   (= (:remaining-conditions binding) (:general-remaining-conditions refined))
                   (= (:obligation-inventory original) (:obligation-inventory refined))
                   (= :explicit-declaration-only (:premise-acceptance original))
                   (= :not-performed (:new-premise-acceptance refined) (:proof-discharge refined) (:proof-discharge original))
                   (= :not-established (:universal-refinement refined) (:writer-coverage refined)
                      (:temporal-coverage refined) (:observation-storage-identity original)
                      (:authorized-historical-lineage original))
                   (seq (:trusted-assumptions original))
                   (= (:trusted-assumptions original) (:open-premise-fingerprints original)))
      (reject! :configured-historical-judgment-mismatch {}))
    {:status :conditional-configured-historical-judgment-binding
     :refined-judgment refined
     :storage-declaration-scope scope
     :storage-trusted-assumptions (:trusted-assumptions original)
     :storage-trust-adoption :explicit-historical-declaration-only
     :current-owner-trust-scope :separate-current-exact-owner-only
     :historical-mediation :not-established :storage-lineage :not-established
     :proof-discharge :not-performed :authority :none :certificate :not-issued}))

(defn- compare-configured-envelope-recovery*
  [system tables source declarations steps observation worker-scope]
  (let [i (locking writer-isolation-hosts (.get writer-isolation-hosts (::writer-isolation-token system)))
        p (locking provider-trust-hosts (.get provider-trust-hosts (::provider-trust-token system)))]
    (when-not (and i p (identical? system (.get ^java.lang.ref.WeakReference (:system i)))
                   (identical? system (.get ^java.lang.ref.WeakReference (:system p)))
                   (identical? (:lock i) (:lock p)) (identical? (:active i) (:active p)))
      (reject! :unissued-combined-trust-host {}))
    (locking (:lock i)
      (when-not @(:active i) (reject! :combined-trust-host-retired {}))
      (configured-recovery-boundaries!)
      (require-open-provider-generation! system)
      (let [node (:xtdb/connectable system) admission (::mapped-admission system)
            token (::mapped-policy-token system)
            activation (original-combined-root node admission token)
            observed (original-framed-observation admission observation)
            acquisition (:acquisition worker-scope)
            capture (:capture worker-scope)
            sample (when worker-scope (original-recovery-acquisition node acquisition))]
        (when-not (and activation (= activation (:activation-root observed)))
          (reject! :configured-recovery-activation-mismatch {}))
        (when (and worker-scope (not (and sample (original-recovery-worker-match system capture))))
          (reject! :configured-recovery-worker-origin-mismatch {}))
        (let [history-cut (:history-cut worker-scope)
              read-history #(original-qualified-history system tables admission source declarations steps observation acquisition capture history-cut)
              qualified (when (contains? worker-scope :history-cut) (read-history))
              historical (:historical worker-scope)
              storage-adoption (when (:configured-storage? historical)
                                 (configured-storage-declaration! system i (:comparison historical)))
              refined (when historical
                        (original-refined-judgment admission observation history-cut
                                                   (:comparison historical)
                                                   (if storage-adoption (:declaration storage-adoption)
                                                       (:declaration historical))))
              _ (when (and historical (not (= :conditional (:status refined))))
                  (reject! :configured-historical-judgment-mismatch {:historical-reason (:reason refined)}))
              configured (original-configured-adopt system source declarations)
              retained (if worker-scope
                         (original-local-activated-recovery node tables admission source declarations steps observation acquisition true)
                         (original-activated-recovery node tables admission source declarations steps observation))
              physical (if worker-scope
                         (original-interleaved-envelope system tables admission source declarations steps observation acquisition capture)
                         (original-physical-envelope node tables admission source declarations steps observation))
              maintenance (:maintenance-correspondence physical)
              general (:general-argument physical)
              binding (:original-clm-r2-binding configured)
              root (:activation-root configured)]
          (when-not (and (= :conditional-configured-implementation-argument (:status configured))
                         (= :matched (:status retained))
                         (= :matched-local-activated-retained-root (:activation-root-binding retained))
                         (= :conditional-physical-source-envelope-recovery (:status physical))
                         (= :conditional-selected-general-argument-binding (:status general))
                         (= (:subject configured) (select-keys (:induction-identity physical) (keys (:subject configured))))
                         (= root (-> activation :observation
                                     (select-keys [:source-id :manifest-tx-id :initialization-tx-id :system-time :initialization-fingerprint])
                                     (update :system-time str)))
                         (= (select-keys root [:manifest-tx-id :initialization-tx-id])
                            (select-keys retained [:manifest-tx-id :initialization-tx-id]))
                         (= (:selected-tx-ids retained) (:selected-tx-ids physical))
                         (if worker-scope
                           (and (= observed-recovery/activated-interleaved-profile (:activated-recovery-profile retained))
                                (= :matched-issued-origin-and-retained-sample (:local-acquisition-binding retained))
                                (= :matched-local-issued-host-capture (:worker-observation-provenance maintenance))
                                (= (:payload-fingerprint sample) (:retained-payload-fingerprint maintenance))
                                (seq (:intervening-maintenance retained))
                                (= (:intervening-maintenance retained)
                                   (mapv #(select-keys % [:tx-id :intent-id :maintenance]) (:steps maintenance)))
                                (every? #(= :matched-mapped-stutter-and-single-outbox-replacement (:effect %)) (:steps maintenance)))
                           (and (= observed-recovery/activated-profile (:activated-recovery-profile retained))
                                (= :not-applicable maintenance)))
                         (= (:abstract-report-fingerprint binding) (:abstract-report-fingerprint general))
                         (= (:original-open-obligation-ids-by-operation binding) (:original-open-obligations general))
                         (= (:remaining-conditions configured) (:general-conditions general))
                         (every? (:remaining-conditions physical) (:general-conditions general))
                         (= (:condition-links-by-contract configured)
                            {:writer-isolation (:related-writer-isolation-conditions general)
                             :provider-trust (:related-provider-conditions general)})
                         (= :related-not-entailing (:condition-link-semantics configured) (:condition-link-semantics general))
                         (= :not-performed (:proof-discharge configured) (:proof-discharge physical) (:proof-discharge general)))
            (reject! :configured-recovery-correspondence-mismatch {:physical-reason (:reason physical) :retained-reason (:reason retained)}))
          (configured-recovery-boundaries!)
          (require-open-provider-generation! system)
          (when-not (= activation (original-combined-root node admission token))
            (reject! :combined-trust-activation-changed {}))
          (when (and worker-scope (not (and (= sample (original-recovery-acquisition node acquisition))
                                            (original-recovery-worker-match system capture))))
            (reject! :configured-recovery-worker-origin-mismatch {}))
          (let [history (when qualified (original-history-join qualified physical root sample))
                historical-binding (when historical (join-historical-judgment! history configured physical refined))]
            (when (and storage-adoption
                       (not= (dissoc (:adopted storage-adoption) :deployment-adoption)
                             (:historical-judgment refined)))
              (reject! :configured-storage-judgment-mismatch {}))
            (when (and qualified (not= qualified (read-history)))
              (reject! :configured-writer-history-changed {}))
            (configured-recovery-boundaries!)
            (require-open-provider-generation! system)
            (let [inventory (when storage-adoption
                              (original-command-inventory system (:comparison historical)
                                (original-recovery-inventory configured physical history refined (:adopted storage-adoption)) refined))]
              (configured-recovery-boundaries!)
              (cond-> {:status :conditional-configured-envelope-recovery
           :profile :configured-clm-r2-envelope-recovery-v1
           :recovery-scope (if worker-scope :worker-interleaved :adjacent)
           :subject (:subject configured) :activation-root root
           :activation-root-binding :matched-current-owner-and-issued-retained-root
           :selected-tx-ids (:selected-tx-ids physical)
           :physical-correspondence physical
           :adoption-scopes {:writer-isolation (get-in configured [:writer-isolation :scope])
                             :provider-trust (get-in configured [:provider-trust :scope])}
           :trusted-assumptions-by-contract (:trusted-assumptions-by-contract configured)
           :condition-links-by-contract (:condition-links-by-contract configured)
           :original-open-obligations (:original-open-obligations general)
           :remaining-general-conditions (:general-conditions general)
           :remaining-conditions (:remaining-conditions physical)
           :condition-link-semantics :related-not-entailing
           :configured-trust-adoption :current-exact-owner-only
           :historical-mediation :not-established :continuous-mediation :not-established
           :storage-lineage :not-established :writer-coverage :not-established
           :provider-refinement :not-established :universal-refinement :not-established
           :premise-acceptance :explicit-declarations-only :proof-discharge :not-performed
           :authority :none :certificate :not-issued}
              history (assoc :qualified-writer-history history
                             :remaining-conditions (into (:remaining-conditions physical) (:remaining-conditions history)))
              historical-binding (assoc :historical-judgment-binding historical-binding)
              storage-adoption (assoc :configured-storage-adoption
                                      {:status :configured-local-host
                                       :policy (:policy storage-adoption)
                                       :scope (:judgment-scope (:adopted storage-adoption))
                                       :deployment-acceptance (:deployment-acceptance (:adopted storage-adoption))
                                       :trusted-assumptions (:trusted-assumptions (:adopted storage-adoption))
                                       :storage-lineage :not-established
                                       :proof-discharge :not-performed})
              inventory (assoc :configured-obligation-inventory inventory)))))))))

(defn compare-configured-envelope-recovery!
  "Compare adjacent recovery on the exact currently configured owner.
   Reacquire both declarations under its retirement lock and match the
   issued and retained activation root. Not historical mediation evidence."
  [system tables source declarations steps observation]
  (compare-configured-envelope-recovery* system tables source declarations steps observation nil))

(defn compare-configured-worker-envelope-recovery!
  "Compare the selected worker-interleaved recovery on its exact current
   owner using the same issued retained acquisition for root and physical
   comparison, and the original host worker capture. Historical diagnostic,
   not continuous mediation or worker authentication."
  [system tables source declarations steps observation acquisition capture]
  (compare-configured-envelope-recovery* system tables source declarations steps observation
                                        {:acquisition acquisition :capture capture}))


(defn compare-configured-worker-history-recovery!
  "Join the interleaved envelope comparison to the exact previously issued
   writer-history cut for this owner, observation, acquisition and capture.
   Finite selected prefix only; no universal coverage or new trust."
  [system tables source declarations steps observation acquisition capture history-cut]
  (compare-configured-envelope-recovery* system tables source declarations steps observation
                                        {:acquisition acquisition :capture capture :history-cut history-cut}))


(defn compare-configured-worker-historical-recovery!
  "Join the exact configured owner and issued prefix to a separately
   declared historical storage judgment. The declaration is not configured
   owner adoption of storage trust and does not establish lineage."
  [system tables source declarations steps observation acquisition capture history-cut comparison declaration]
  (compare-configured-envelope-recovery* system tables source declarations steps observation
                                        {:acquisition acquisition :capture capture :history-cut history-cut
                                         :historical {:comparison comparison :declaration declaration}}))


(defn compare-configured-worker-storage-recovery!
  "Join selected recovery to all three startup-selected trust contracts.
   Storage premises come from the exact owner's policy, not a caller's
   declaration. Local adoption does not authenticate historical lineage."
  [system tables source declarations steps observation acquisition capture history-cut comparison]
  (compare-configured-envelope-recovery* system tables source declarations steps observation
                                        {:acquisition acquisition :capture capture :history-cut history-cut
                                         :historical {:comparison comparison :configured-storage? true}}))
