(ns gesso.live.core
  "Public orchestration facade for gesso.live.

   This namespace wires the lower-level live pieces together:

   - invalidation rules
   - async dispatch
   - hot source broadcast
   - per-client flow construction
   - SSE transport startup
   - fragment render protection

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
       owns render singleflight/cache protection"
  (:require
   [gesso.live.dispatch :as dispatch]
   [gesso.live.flow :as flow]
   [gesso.live.fragment :as fragment]
   [gesso.live.invalidation :as invalidation]
   [gesso.live.source :as source]
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
