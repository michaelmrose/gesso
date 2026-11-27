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
                       (::mapped-policy-token started) (assoc ::worker-capture-token worker-capture-token))]
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
