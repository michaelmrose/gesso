(ns gesso.live.core
  "Public orchestration facade for gesso.live.

   This namespace wires the lower-level live pieces together:

   - invalidation rules
   - async dispatch
   - hot source broadcast
   - per-client flow construction
   - SSE transport startup
   - fragment render protection
   - app-facing XTDB2 consistency helpers
   - app-facing HTMX attribute helpers

   It intentionally stays thin. The specialized namespaces still own their own
   behavior:

     gesso.live.invalidation
       expands primary app changes into invalidations

     gesso.live.dispatch
       runs async jobs

     gesso.live.source
       owns hot Manifold broadcast

     gesso.live.flow
       owns per-client Missionary filtering/coalescing

     gesso.live.transport.sse
       owns SSE frame formatting and response stream lifecycle

     gesso.live.fragment
       owns render singleflight/cache protection

     gesso.live.consistency.xtdb
       owns XTDB2 query/transaction consistency helpers

     gesso.live.htmx
       owns browser-facing HTMX attribute builders"
  (:require
   [gesso.live.consistency.xtdb :as live.xtdb]
   [gesso.live.dispatch :as dispatch]
   [gesso.live.flow :as flow]
   [gesso.live.fragment :as fragment]
   [gesso.live.htmx :as htmx]
   [gesso.live.invalidation :as invalidation]
   [gesso.live.source :as source]
   [gesso.live.synced :as synced]
   [gesso.live.transport.sse :as sse]))

;; -----------------------------------------------------------------------------
;; Debug
;; -----------------------------------------------------------------------------

(defmacro ^:private debug!
  "Emit a debug event only when debug-fn is truthy.

   The data expression is inside the guard, so with debugging off it is not
   evaluated."
  [debug-fn event data]
  `(when-let [f# ~debug-fn]
     (f# (assoc ~data :event ~event))))

;; -----------------------------------------------------------------------------
;; Defaults
;; -----------------------------------------------------------------------------

(def default-options
  {:source nil
   :source-options nil

   :dispatcher nil
   :dispatch-options nil

   :rules []

   :fragment-manager nil
   :fragment-options nil

   :debug-fn nil})

(def ^:private valid-emit-modes
  #{:async :sync false})

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn- now-ms
  []
  (System/currentTimeMillis))

(defn- ex
  ([message data]
   (ex-info message data))
  ([message data cause]
   (ex-info message data cause)))

(defn- opts
  [options]
  (merge default-options options))

(defn- require-fn-option!
  [options k]
  (when-let [f (get options k)]
    (when-not (fn? f)
      (throw
       (ex (str "gesso.live core " k " must be a function.")
           {k f}))))
  options)

(defn- require-map-option!
  [options k]
  (when-let [m (get options k)]
    (when-not (map? m)
      (throw
       (ex (str "gesso.live core " k " must be a map.")
           {k m}))))
  options)

(defn- require-rules-option!
  [options]
  (when-not (sequential? (:rules options))
    (throw
     (ex "gesso.live core :rules must be sequential."
         {:rules (:rules options)})))
  options)

(defn prepare-options!
  "Merge defaults and validate core options."
  [options]
  (let [options' (opts options)]
    (require-fn-option! options' :debug-fn)
    (require-map-option! options' :source-options)
    (require-map-option! options' :dispatch-options)
    (require-map-option! options' :fragment-options)
    (require-rules-option! options')
    options'))

(defn- call-safely
  [f & args]
  (when f
    (try
      (apply f args)
      (catch Exception _
        nil))))

(defn- with-inherited-debug
  "Attach the system debug function unless options explicitly contains
   :debug-fn.

   Passing {:debug-fn nil} explicitly disables inherited debugging for that
   child operation."
  [system options]
  (let [options' (or options {})]
    (if (contains? options' :debug-fn)
      options'
      (assoc options' :debug-fn (get-in system [:options :debug-fn])))))

(defn- close-created!
  [created]
  (doseq [close! (reverse @created)]
    (call-safely close!)))

(defn- create-owned-source!
  [options' created]
  (let [src (source/create (:source-options options'))]
    (swap! created conj #(source/close! src))
    src))

(defn- create-owned-dispatcher!
  [options' created]
  (let [dispatcher (dispatch/create (:dispatch-options options'))]
    (swap! created conj #(dispatch/close! dispatcher))
    dispatcher))

(defn- normalize-emit-mode
  [emit]
  (let [emit' (if (nil? emit) :async emit)]
    (when-not (contains? valid-emit-modes emit')
      (throw
       (ex "Unsupported gesso.live transact-and-notify! emit mode."
           {:emit emit
            :normalized emit'
            :valid valid-emit-modes})))
    emit'))

(defn- normalize-changes
  [{:keys [change changes]}]
  (cond
    (some? changes)
    (vec changes)

    (some? change)
    [change]

    :else
    []))

(defn- dispatch-entry-for
  [entry entry-fn change]
  (cond
    entry-fn
    (entry-fn change)

    entry
    entry

    :else
    nil))

;; -----------------------------------------------------------------------------
;; System lifecycle
;; -----------------------------------------------------------------------------

(defn create
  "Create a gesso.live system map.

   Options:
     :source
       Optional existing source. If omitted, core creates one.

     :source-options
       Options passed to source/create when :source is omitted.

     :dispatcher
       Optional existing dispatcher. If omitted, core creates one.

     :dispatch-options
       Options passed to dispatch/create when :dispatcher is omitted.

     :rules
       Invalidation expansion rules, passed through invalidation/compile-rules.

     :fragment-manager
       Optional existing fragment manager. If omitted, core creates one.

     :fragment-options
       Options passed to fragment/create when :fragment-manager is omitted.

     :debug-fn
       Optional pay-for-play debug hook.

   Core only closes source/dispatcher resources it created itself.

   Construction is exception-safe: if a later construction step fails, any
   already-created owned source/dispatcher is closed before the exception is
   rethrown."
  ([] (create nil))
  ([options]
   (let [options' (prepare-options! options)
         debug-fn (:debug-fn options')
         created (atom [])]
     (try
       ;; Compile pure configuration before creating resources where possible.
       (let [rules (invalidation/compile-rules (:rules options'))

             source-supplied? (some? (:source options'))
             dispatcher-supplied? (some? (:dispatcher options'))
             fragment-supplied? (some? (:fragment-manager options'))

             src (or (:source options')
                     (create-owned-source! options' created))

             dispatcher (or (:dispatcher options')
                            (create-owned-dispatcher! options' created))

             ;; Fragment manager currently does not own closeable resources, but
             ;; core still records whether it created it for stats/debug clarity.
             fragment-manager (or (:fragment-manager options')
                                  (fragment/create
                                   (with-inherited-debug
                                     {:options options'}
                                     (:fragment-options options'))))

             system {:source src
                     :dispatcher dispatcher
                     :rules rules
                     :fragment-manager fragment-manager
                     :options options'
                     :owned {:source (not source-supplied?)
                             :dispatcher (not dispatcher-supplied?)
                             :fragment-manager (not fragment-supplied?)}
                     :closed? (atom false)}]

         ;; Ownership has moved to the returned system. Do not auto-cleanup now.
         (reset! created [])

         (debug!
          debug-fn
          :gesso.live.core/created
          {:owned (:owned system)
           :rule-count (count rules)
           :at (now-ms)})

         system)

       (catch Throwable e
         (close-created! created)
         (throw e))))))

(defn closed?
  "Return true when the core system has been closed."
  [system]
  (true? @(:closed? system)))

(defn close!
  "Close resources owned by the core system.

   Existing source/dispatcher/fragment-manager values supplied by the caller are
   not closed by core because core does not own them.

   Fragment managers currently have no close operation; their cache can be
   cleared explicitly with clear-fragment-cache!."
  [system]
  (let [debug-fn (get-in system [:options :debug-fn])]
    (when (compare-and-set! (:closed? system) false true)
      (debug!
       debug-fn
       :gesso.live.core/closing
       {:owned (:owned system)
        :at (now-ms)})

      ;; Stop async jobs before closing the source they may publish to.
      (when (get-in system [:owned :dispatcher])
        (call-safely dispatch/close! (:dispatcher system)))

      (when (get-in system [:owned :source])
        (call-safely source/close! (:source system)))

      (debug!
       debug-fn
       :gesso.live.core/closed
       {:owned (:owned system)
        :at (now-ms)})))
  system)

(defn stats
  "Return a combined stats map for the live system."
  [system]
  {:closed? (closed? system)
   :source (source/stats (:source system))
   :fragment (fragment/stats (:fragment-manager system))})

;; -----------------------------------------------------------------------------
;; XTDB2 consistency facade
;; -----------------------------------------------------------------------------

(defn consistency
  "Return explicit gesso.live/XTDB2 consistency from ctx.

   This delegates to gesso.live.consistency.xtdb/consistency-from and does not
   inspect or mutate shared XTDB node/DataSource state."
  [ctx]
  (live.xtdb/consistency-from ctx))

(defn with-consistency
  "Assoc explicit consistency onto ctx.

   The consistency value is normalized before being stored under
   :gesso.live/consistency. Passing nil or an empty consistency map stores an
   empty normalized map."
  [ctx consistency]
  (assoc ctx
         :gesso.live/consistency
         (live.xtdb/normalize-consistency consistency)))

(defn attach-consistency
  "Attach explicit consistency to a change/invalidation map.

   This is plain immutable data. It is not used to mutate XTDB state.

   The attached value is intentionally namespaced so app rules can opt into
   reading it without colliding with domain keys."
  [change consistency]
  (let [consistency' (live.xtdb/normalize-consistency consistency)]
    (if (seq consistency')
      (assoc change :gesso.live/consistency consistency')
      change)))

(defn q
  "App-facing XTDB2 read helper.

   Uses the ctx-aware, consistency-aware read path. Prefer this for live fragment
   reads so a ctx carrying :gesso.live/consistency, :xtdb/consistency, or
   :consistency automatically applies that read basis to the individual query."
  ([ctx query]
   (live.xtdb/q-consistent-from ctx query))
  ([ctx query opts]
   (live.xtdb/q-consistent-from ctx query opts)))

(defn execute-tx!
  "Execute XTDB2 tx ops using a raw connectable or app ctx.

   Returns the result from gesso.live.consistency.xtdb/execute-tx-from!:

     {:tx-result ...
      :consistency ...}

   execute-tx! is preferred for write paths that need immediate live
   read-after-write consistency because XTDB2 execute-tx returns tx-id and
   system-time, from which the XTDB helper derives :snapshot-time."
  ([ctx tx-ops]
   (live.xtdb/execute-tx-from! ctx tx-ops))
  ([ctx tx-ops opts]
   (live.xtdb/execute-tx-from! ctx tx-ops opts)))

(defn submit-tx!
  "Submit XTDB2 tx ops using a raw connectable or app ctx.

   Public XTDB2 submit-tx returns only :tx-id. The returned consistency is
   therefore metadata-only unless XTDB changes that public result shape."
  ([ctx tx-ops]
   (live.xtdb/submit-tx-from! ctx tx-ops))
  ([ctx tx-ops opts]
   (live.xtdb/submit-tx-from! ctx tx-ops opts)))

(def put-docs-op
  "Build an XTDB2 :put-docs tx op.

   Re-export of gesso.live.consistency.xtdb/put-docs-op."
  live.xtdb/put-docs-op)

(def delete-docs-op
  "Build an XTDB2 :delete-docs tx op.

   Re-export of gesso.live.consistency.xtdb/delete-docs-op."
  live.xtdb/delete-docs-op)

;; -----------------------------------------------------------------------------
;; HTMX facade
;; -----------------------------------------------------------------------------

(def fragment-root-attrs
  "Build attrs for the outer live wrapper.

   Re-export of gesso.live.htmx/fragment-root-attrs."
  htmx/fragment-root-attrs)

(def fragment-target-attrs
  "Build attrs for a live fragment refresh target.

   Re-export of gesso.live.htmx/fragment-target-attrs."
  htmx/fragment-target-attrs)

(def post-form-attrs
  "Build standard attrs for a POST form that refreshes a target fragment.

   Re-export of gesso.live.htmx/post-form-attrs."
  htmx/post-form-attrs)

;; -----------------------------------------------------------------------------
;; Invalidation and source emission
;; -----------------------------------------------------------------------------

(defn expand
  "Expand one primary app change into invalidations using the system rules."
  [system ctx change]
  (let [debug-fn (get-in system [:options :debug-fn])
        expanded (invalidation/expand (:rules system) ctx change)]
    (debug!
     debug-fn
     :gesso.live.core/expanded
     {:change change
      :count (count expanded)
      :at (now-ms)})
    expanded))

(defn emit!
  "Emit one already-expanded invalidation directly to the system source."
  [system invalidation]
  (let [debug-fn (get-in system [:options :debug-fn])]
    (debug!
     debug-fn
     :gesso.live.core/emit
     {:invalidation invalidation
      :at (now-ms)})
    (source/emit! (:source system) invalidation)))

(defn emit-many!
  "Emit many already-expanded invalidations directly to the system source."
  [system invalidations]
  (let [debug-fn (get-in system [:options :debug-fn])]
    (debug!
     debug-fn
     :gesso.live.core/emit-many
     {:count (count invalidations)
      :at (now-ms)})
    (source/emit-many! (:source system) invalidations)))

(defn emit-expanded!
  "Expand a primary app change and emit the resulting invalidations.

   Returns the source emit-many! result."
  [system ctx change]
  (let [expanded (expand system ctx change)
        result (emit-many! system expanded)
        debug-fn (get-in system [:options :debug-fn])]
    (debug!
     debug-fn
     :gesso.live.core/emit-expanded
     {:change change
      :count (count expanded)
      :result result
      :at (now-ms)})
    result))

(defn submit-expanded!
  "Submit an async job that expands and emits one primary app change.

   This is the usual write-path helper: after an app write commits, submit an
   app-level primary change here.

   `entry` may contain dispatch metadata, but :run is owned by this helper."
  ([system ctx change]
   (submit-expanded! system ctx change nil))
  ([system ctx change entry]
   (let [debug-fn (get-in system [:options :debug-fn])
         entry' (assoc (or entry {})
                       :run
                       (fn []
                         (emit-expanded! system ctx change)))]
     (debug!
      debug-fn
      :gesso.live.core/submit-expanded
      {:change change
       :entry-keys (set (keys (or entry {})))
       :at (now-ms)})
     (dispatch/submit! (:dispatcher system) entry'))))

(defn transact-and-notify!
  "Common XTDB2 write + live invalidation workflow.

   Steps:
     1. execute XTDB2 tx ops with execute-tx!
     2. capture returned consistency
     3. assoc consistency onto ctx
     4. attach consistency to change maps
     5. optionally emit or submit expanded invalidations
     6. return tx/ctx/change/emission metadata

   Arguments:
     system
       gesso.live system map.

     ctx
       app ctx or XTDB connectable-containing ctx.

     options map:
       :tx-ops
         Required XTDB2 tx ops.

       :change
         One primary app change.

       :changes
         Multiple primary app changes.

       :tx-options
         Optional XTDB tx options passed to execute-tx!.

       :emit
         :async | :sync | false. Defaults to :async.

         :async submits each change through submit-expanded!.
         :sync expands and emits each change on the caller thread.
         false performs the transaction and returns metadata without emitting.

       :entry
         Optional dispatch entry metadata used for every async submission.

       :entry-fn
         Optional function of attached change -> dispatch entry metadata.

   Returns:
     {:tx-result ...
      :consistency ...
      :ctx ...
      :changes ...
      :emit ...
      :emit-results ...}"
  [system ctx {:keys [tx-ops tx-options emit entry entry-fn] :as options}]
  (when-not tx-ops
    (throw
     (ex "gesso.live transact-and-notify! requires :tx-ops."
         {:options options})))
  (let [debug-fn (get-in system [:options :debug-fn])
        emit-mode (normalize-emit-mode emit)
        tx-result-map (execute-tx! ctx tx-ops tx-options)
        consistency' (:consistency tx-result-map)
        ctx' (with-consistency ctx consistency')
        changes' (mapv #(attach-consistency % consistency')
                       (normalize-changes options))
        emit-results
        (case emit-mode
          false
          []

          :sync
          (mapv #(emit-expanded! system ctx' %) changes')

          :async
          (mapv (fn [change]
                  (submit-expanded!
                   system
                   ctx'
                   change
                   (dispatch-entry-for entry entry-fn change)))
                changes'))]
    (debug!
     debug-fn
     :gesso.live.core/transact-and-notify
     {:tx-result (:tx-result tx-result-map)
      :consistency consistency'
      :change-count (count changes')
      :emit emit-mode
      :emit-result-count (count emit-results)
      :at (now-ms)})
    (merge tx-result-map
           {:ctx ctx'
            :changes changes'
            :emit emit-mode
            :emit-results emit-results})))


;; -----------------------------------------------------------------------------
;; Synced value facade
;; -----------------------------------------------------------------------------

(def ->synced
  "Define one persisted live value.

   Re-export of gesso.live.synced/->synced."
  synced/->synced)

(defn- system-from
  [ctx options]
  (or (:system options)
      (:gesso.live/system options)
      (:gesso.live/system ctx)
      (:live/system ctx)
      (throw
       (ex "gesso.live synced write requires a live system."
           {:ctx-keys (when (map? ctx) (set (keys ctx)))
            :options-keys (when (map? options) (set (keys options)))
            :expected-one-of [:system
                              :gesso.live/system
                              [:ctx :gesso.live/system]
                              [:ctx :live/system]]}))))

(defn live-read
  "Read a synced value through the ctx-aware XTDB consistency path.

   Example:

     (def counter
       (live/->synced
        {:table :demo_counters
         :id \"global-shared-counter\"
         :col :demo/value
         :topic :demo-counter
         :default 0}))

     (live/live-read ctx counter)"
  ([ctx synced-value]
   (synced/live-read ctx synced-value))
  ([ctx synced-value opts]
   (synced/live-read ctx synced-value opts)))

(defn live-set!
  "Set a synced value, execute an XTDB tx, and optionally notify live subscribers.

   Requires the live system under one of:

     (:system options)
     (:gesso.live/system options)
     (:gesso.live/system ctx)
     (:live/system ctx)

   Options:
     :system
       Explicit live system.

     :emit
       :async | :sync | false. Defaults to :async.

     :tx-options
       XTDB tx options.

     :entry
       Dispatch entry metadata. Defaults to the synced descriptor's
       coalesce-key entry.

     :entry-fn
       Optional function of attached change -> dispatch entry metadata.

     :change
       Explicit primary change. Defaults to synced/change.

     :data
       Optional data included in the generated change.

     :change/kind
       Override generated change kind.

   Returns the transact-and-notify! result, assoc'd with :value."
  ([ctx synced-value value]
   (live-set! ctx synced-value value nil))
  ([ctx synced-value value options]
   (let [options' (or options {})
         system (system-from ctx options')
         change' (or (:change options')
                     (synced/change synced-value value options'))
         entry' (if (contains? options' :entry)
                  (:entry options')
                  (synced/entry synced-value))
         result (transact-and-notify!
                 system
                 ctx
                 {:tx-ops (synced/tx-ops synced-value value)
                  :change change'
                  :tx-options (:tx-options options')
                  :emit (:emit options')
                  :entry entry'
                  :entry-fn (:entry-fn options')})]
     (assoc result :value value))))

(defn live-swap!
  "Read a synced value, apply f to it, set the result, and notify subscribers.

   f is called as a unary function with the current value.

   Example:

     (live/live-swap! ctx counter inc)"
  ([ctx synced-value f]
   (live-swap! ctx synced-value f nil))
  ([ctx synced-value f options]
   (let [old-value (live-read ctx synced-value (:read-options options))
         new-value (f old-value)
         options' (assoc (or options {})
                         :old-value old-value
                         :new-value new-value)]
     (live-set! ctx synced-value new-value options'))))

;; -----------------------------------------------------------------------------
;; Flow and SSE
;; -----------------------------------------------------------------------------

(defn live-flow
  "Build a Missionary flow of live events for one subscription.

   Options are passed to flow/flow-for-source. :debug-fn defaults to the system
   debug hook unless explicitly supplied."
  ([system subscription]
   (live-flow system subscription nil))
  ([system subscription options]
   (let [options' (merge
                   (with-inherited-debug system options)
                   {:subscription subscription})
         debug-fn (:debug-fn options')]
     (debug!
      debug-fn
      :gesso.live.core/live-flow
      {:subscription subscription
       :at (now-ms)})
     (flow/flow-for-source (:source system) options'))))

(defn start-sse!
  "Start an SSE stream for one subscription.

   Options:
     :flow-options
       Passed to flow/flow-for-source.

     :sse-options
       Passed to transport.sse/start!.

   Returns the map from sse/start!:

     {:stream ...
      :response ...
      :cancel! ...
      :closed? ...}

   Keep this returned map and call cancel-sse! or sse/cancel! when the client
   disconnects."
  ([system subscription]
   (start-sse! system subscription nil))
  ([system subscription {:keys [flow-options sse-options] :as options}]
   (let [debug-fn (get-in system [:options :debug-fn])
         flow-options' (with-inherited-debug system flow-options)
         sse-options' (with-inherited-debug system sse-options)
         live-events (live-flow system subscription flow-options')
         started (sse/start! live-events sse-options')]
     (debug!
      debug-fn
      :gesso.live.core/sse-started
      {:subscription subscription
       :options-keys (set (keys (or options {})))
       :at (now-ms)})
     started)))

(defn cancel-sse!
  "Cancel a started SSE stream."
  [started]
  (sse/cancel! started))

;; -----------------------------------------------------------------------------
;; Fragment render protection
;; -----------------------------------------------------------------------------

(defn fragment-manager
  "Return the system fragment manager."
  [system]
  (:fragment-manager system))

(defn fragment-key
  "Build a generic fragment key.

   Re-export of gesso.live.fragment/fragment-key."
  ([base]
   (fragment/fragment-key base))
  ([base dimensions]
   (fragment/fragment-key base dimensions)))

(defn strict-fragment-key
  "Build a stricter fragment key requiring :fragment, :scope, and :user-key.

   Re-export of gesso.live.fragment/strict-fragment-key."
  [m]
  (fragment/strict-fragment-key m))

(defn render-task
  "Return a protected fragment render task using the system fragment manager.

   Options are passed to fragment/render-task. :debug-fn defaults to the system
   debug hook unless explicitly supplied."
  ([system key render-fn]
   (render-task system key render-fn nil))
  ([system key render-fn options]
   (fragment/render-task
    (:fragment-manager system)
    key
    render-fn
    (with-inherited-debug system options))))

(defn protect-task
  "Return a protected Missionary task using the system fragment manager.

   Lower-level companion to render-task."
  ([system key task]
   (protect-task system key task nil))
  ([system key task options]
   (fragment/protect-task
    (:fragment-manager system)
    key
    task
    (with-inherited-debug system options))))

(defn clear-fragment-cache!
  "Clear all cached fragment values."
  [system]
  (fragment/clear-cache! (:fragment-manager system))
  system)

(defn clear-fragment-key!
  "Clear cached and in-flight state for one fragment key."
  [system key]
  (fragment/clear-key! (:fragment-manager system) key)
  system)

(defn purge-expired-fragments!
  "Purge expired fragment cache entries and return the number removed."
  [system]
  (fragment/purge-expired! (:fragment-manager system)))
