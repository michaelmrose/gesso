(ns gesso.live.fragment
  "Fragment render protection for gesso.live.

   This namespace protects expensive HTML fragment rendering from stampedes.

   It provides:

   - singleflight: concurrent renders for the same key share one in-flight result
   - short TTL cache: near-simultaneous requests can reuse fresh rendered output
   - optional cache size bound
   - authoritative-progression-aware key helpers
   - Missionary render tasks using m/via-call
   - conditional debug tracing
   - model-backed adapters from gesso.live.model fragment descriptors to
     gesso.live.ui fragment descriptors
   - passthrough of generic runtime UI options such as client continuity

   The render-protection machinery does not:

   - know HTMX
   - know SSE
   - know XTDB
   - know Ring response maps
   - decide subscription interests

   The model-backed adapter section at the bottom delegates to gesso.live.ui.
   It still does not own routes, streams, Ring handlers, browser lifecycle
   behavior, or subscriptions beyond constructing the current UI fragment
   descriptor from compiled model metadata.

   The caller is responsible for choosing a key that includes every value that
   can affect the rendered HTML, such as user/scope/params/theme/locale and any
   authoritative progression requirement."
  (:require
   [gesso.live.model :as model]
   [gesso.live.progression :as progression]
   [gesso.live.ui :as ui]
   [missionary.core :as m]))

;; -----------------------------------------------------------------------------
;; Debug
;; -----------------------------------------------------------------------------

(defmacro ^:private debug!
  "Emit a debug event only when debug-fn is truthy.

   The data expression is inside the guard, so with debugging off it is not
   evaluated."
  [debug-fn event data]
  ;; Diagnostic callbacks are observational. A failing tracing hook must
  ;; never suppress rendering, poison the cache, strand a singleflight owner,
  ;; or replace a genuine renderer exception.
  `(when-let [f# ~debug-fn]
     (try
       (f# (assoc ~data :event ~event))
       (catch Exception _#
         nil))))

;; -----------------------------------------------------------------------------
;; Defaults
;; -----------------------------------------------------------------------------

(def default-options
  {:ttl-ms nil
   :cache? true
   :max-cache-entries nil
   :singleflight? true
   :executor :blk
   :clock nil
   :debug-fn nil})

(def miss
  ::miss)

;; Internal cache-hit envelopes separate legitimate rendered values from the
;; public miss sentinel. The lower-level render API accepts arbitrary values.
(deftype ^:private CacheHit [value])

(def key-dimensions
  [:scope
   :user-key
   :variant
   :params
   :locale
   :theme
   :progression])

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

(defn- compact-map
  [m]
  (into {}
        (remove (comp nil? val))
        m))

(defn- require-fn-option!
  [options k]
  (when-let [f (get options k)]
    (when-not (fn? f)
      (throw
       (ex (str "gesso.live fragment " k " must be a function.")
           {k f}))))
  options)

(defn- require-render-fn!
  [render-fn]
  (when-not (fn? render-fn)
    (throw
     (ex "gesso.live fragment render-fn must be a function."
         {:render-fn render-fn})))
  render-fn)

(defn- require-task!
  [task]
  (when-not (fn? task)
    (throw
     (ex "gesso.live fragment task must be a Missionary task function."
         {:task task})))
  task)

(defn- require-boolean-option!
  [options k]
  (let [v (get options k)]
    (when-not (or (true? v) (false? v))
      (throw
       (ex (str "gesso.live fragment " k " must be a boolean.")
           {k v}))))
  options)

(defn- require-non-negative-int-option!
  [options k]
  (when-let [v (get options k)]
    (when-not (and (integer? v) (not (neg? v)))
      (throw
       (ex (str "gesso.live fragment " k " must be a non-negative integer.")
           {k v}))))
  options)

(defn- require-positive-int-option!
  [options k]
  (when-let [v (get options k)]
    (when-not (and (integer? v) (pos? v))
      (throw
       (ex (str "gesso.live fragment " k " must be a positive integer.")
           {k v}))))
  options)

(defn- require-executor-option!
  [options]
  (let [executor (:executor options)]
    (when-not (or (= :blk executor)
                  (= :cpu executor)
                  (instance? java.util.concurrent.Executor executor))
      (throw
       (ex "gesso.live fragment :executor must be :blk, :cpu, or a java.util.concurrent.Executor."
           {:executor executor}))))
  options)

(defn prepare-options!
  "Merge defaults and validate fragment options."
  [options]
  (let [options' (opts options)]
    (require-non-negative-int-option! options' :ttl-ms)
    (require-positive-int-option! options' :max-cache-entries)
    (require-boolean-option! options' :cache?)
    (require-boolean-option! options' :singleflight?)
    (require-fn-option! options' :clock)
    (require-fn-option! options' :debug-fn)
    (require-executor-option! options')
    (cond-> options'
      (nil? (:clock options'))
      (assoc :clock now-ms))))

(defn- effective-options
  [manager options]
  (prepare-options!
   (merge (:options manager) options)))

(defn- validate-key!
  [key]
  (when (nil? key)
    (throw
     (ex "gesso.live fragment key must not be nil."
         {:key key})))
  key)

(defn- validate-required-key-dimension!
  [m k]
  (when-not (contains? m k)
    (throw
     (ex "gesso.live strict fragment key is missing required dimension."
         {:dimension k
          :input m})))
  (when (nil? (get m k))
    (throw
     (ex "gesso.live strict fragment key dimension must not be nil."
         {:dimension k
          :input m})))
  m)

(defn- executor-value
  [executor]
  (case executor
    :blk m/blk
    :cpu m/cpu
    executor))

(defn- result-value
  [result]
  (case (:status result)
    :success
    (:value result)

    :failure
    (throw (:error result))

    (throw
     (ex "Invalid gesso.live fragment singleflight result."
         {:result result}))))

;; -----------------------------------------------------------------------------
;; Fragment keys
;; -----------------------------------------------------------------------------

(def ^:private legacy-consistency-key
  :consistency-token)

(defn- normalize-key-dimensions!
  [dimensions]
  (let [dimensions' (or dimensions {})]
    (when (contains? dimensions' legacy-consistency-key)
      (throw
       (ex
        "gesso.live fragment keys no longer accept :consistency-token; use a normalized :progression requirement."
        {:unsupported-dimension legacy-consistency-key
         :dimensions dimensions'})))
    (if (contains? dimensions' :progression)
      (let [requirement
            (progression/normalize-requirement (:progression dimensions'))]
        (cond-> (dissoc dimensions' :progression)
          (some? requirement)
          (assoc :progression requirement)))
      dimensions')))

(defn fragment-key
  "Build a stable vector key for a rendered fragment.

   `base` should identify the fragment itself.

   `dimensions` should include every value that can change the rendered HTML.
   Common dimensions:

     :scope
     :user-key
     :variant
     :params
     :locale
     :theme
     :progression

   :progression, when present, must be a normalized gesso.live.progression
   requirement. It is normalized again at this boundary so equivalent orderless
   requirements produce equal keys. Including it prevents a refresh carrying an
   authoritative minimum-read requirement from sharing cached or in-flight
   output with a render that did not carry that requirement.

   The retired :consistency-token dimension is rejected rather than silently
   ignored so stale callers cannot accidentally weaken render isolation."
  ([base]
   (fragment-key base nil))
  ([base dimensions]
   (let [dimensions' (normalize-key-dimensions! dimensions)]
     (into [:gesso.live.fragment base]
           (mapcat
            (fn [k]
              (when (contains? dimensions' k)
                [k (get dimensions' k)])))
           key-dimensions))))

(defn strict-fragment-key
  "Build a fragment key from a map and require the most important dimensions.

   Required keys:
     :fragment
     :scope
     :user-key

   Optional render-affecting dimensions:
     :variant
     :params
     :locale
     :theme
     :progression

   This helper is intended for higher-level app/core APIs where accidentally
   sharing HTML across users/scopes would be dangerous."
  [m]
  (-> m
      (validate-required-key-dimension! :fragment)
      (validate-required-key-dimension! :scope)
      (validate-required-key-dimension! :user-key))
  (fragment-key (:fragment m) m))

;; -----------------------------------------------------------------------------
;; Manager
;; -----------------------------------------------------------------------------

(defn create
  "Create a fragment render manager.

   Options:
     :ttl-ms
       Non-negative integer. nil or 0 disables TTL caching.

     :cache?
       Boolean, default true.

     :max-cache-entries
       Optional positive integer. Bounds the cache by evicting oldest entries
       after a store.

     :singleflight?
       Boolean, default true.

     :executor
       :blk, :cpu, or java.util.concurrent.Executor. Used by render-task.

     :clock
       Zero-arity function returning current milliseconds. Defaults to system
       clock.

     :debug-fn
       Optional pay-for-play debug hook."
  ([] (create nil))
  ([options]
   {:cache (atom {})
    :cache-generation (atom 0)
    ;; Track key-specific invalidations only while renders are active.
    ;; A permanent per-key epoch map would grow without bound for long-lived
    ;; managers that invalidate many distinct fragment keys.
    :active-key-renders (atom {})
    :inflight (atom {})
    :options (prepare-options! options)}))

(defn stats
  "Return simple manager stats."
  [manager]
  {:cache-count (count @(:cache manager))
   :inflight-count (count @(:inflight manager))})

;; -----------------------------------------------------------------------------
;; Cache
;; -----------------------------------------------------------------------------

(defn- cache-enabled?
  [options]
  (and (:cache? options)
       (integer? (:ttl-ms options))
       (pos? (:ttl-ms options))))

(defn- cache-entry
  [value now ttl-ms]
  {:value value
   :stored-at now
   :expires-at (+ now ttl-ms)
   :ttl-ms ttl-ms})

(defn- cache-entry-valid?
  [now entry]
  (< now (:expires-at entry)))

(defn- prune-cache
  [cache now max-cache-entries]
  (let [without-expired (into {}
                              (filter
                               (fn [[_ entry]]
                                 (cache-entry-valid? now entry)))
                              cache)
        expired-count (- (count cache) (count without-expired))]
    (if (and (integer? max-cache-entries)
             (pos? max-cache-entries)
             (> (count without-expired) max-cache-entries))
      (let [evict-count (- (count without-expired) max-cache-entries)
            kept (->> without-expired
                      (sort-by (fn [[_ entry]]
                                 ;; The timestamp is captured before the
                                 ;; atomic cache update. Concurrent renders can
                                 ;; publish in the opposite order from their
                                 ;; captured times, so publication order must
                                 ;; determine eviction, not just break ties.
                                 ;; Unnumbered legacy/test entries still sort
                                 ;; by their original timestamps.
                                 [(or (:write-order entry) 0)
                                  (:stored-at entry)]))
                      (drop evict-count)
                      (into {}))]
        {:cache kept
         :expired-count expired-count
         :evicted-count evict-count})
      {:cache without-expired
       :expired-count expired-count
       :evicted-count 0})))

(defn- cache-lookup
  [manager key options]
  (let [{:keys [clock debug-fn]} options]
    (if-not (cache-enabled? options)
      (do
        (debug!
         debug-fn
         :gesso.live.fragment/cache-disabled
         {:key key
          :at (clock)})
        miss)

      (let [now (clock)
            entry (get @(:cache manager) key)]
        (cond
          (nil? entry)
          (do
            (debug!
             debug-fn
             :gesso.live.fragment/cache-miss
             {:key key
              :at now})
            miss)

          (and (cache-entry-valid? now entry)
               ;; A caller may request a stricter TTL than the one used to
               ;; store this entry. Respect both the stored expiry and the
               ;; current render's maximum acceptable age.
               (< now (+ (:stored-at entry) (:ttl-ms options))))
          (do
            (debug!
             debug-fn
             :gesso.live.fragment/cache-hit
             {:key key
              :stored-at (:stored-at entry)
              :expires-at (:expires-at entry)
              :at now})
            (CacheHit. (:value entry)))

          :else
          (do
            ;; A stricter per-call TTL can make a still-live stored entry
            ;; unsuitable for this request. Only evict when the entry's own
            ;; stored expiration has passed: less restrictive callers may
            ;; still reuse it, even if this caller's rerender fails.
            ;; Identity checking prevents removing a concurrently stored entry.
            (when-not (cache-entry-valid? now entry)
              (swap! (:cache manager)
                     (fn [cache]
                       (if (identical? entry (get cache key))
                         (dissoc cache key)
                         cache))))
            (debug!
             debug-fn
             :gesso.live.fragment/cache-expired
             {:key key
              :stored-at (:stored-at entry)
              :expires-at (:expires-at entry)
              :at now})
            miss))))))

(defn- cache-store!
  ([manager key value options]
   (cache-store! manager key value options nil nil nil))
  ([manager key value options flight]
   (cache-store! manager key value options flight nil nil))
  ([manager key value options flight generation]
   (cache-store! manager key value options flight generation nil))
  ([manager key value options flight generation key-generation]
   (when (cache-enabled? options)
     (let [{:keys [clock ttl-ms max-cache-entries debug-fn]} options
           now (clock)
           entry (cache-entry value now ttl-ms)
           prune-result (atom nil)
           store! (fn []
                    (swap! (:cache manager)
                           (fn [cache]
                             (let [;; Recompute publication order on CAS retries.
                                   next-order (inc (reduce max 0
                                                           (keep :write-order (vals cache))))
                                   entry' (assoc entry :write-order next-order)
                                   result (prune-cache (assoc cache key entry')
                                                       now max-cache-entries)]
                               (reset! prune-result result)
                               (:cache result)))))]
       ;; Publication and clear-cache! share this lock. A render started in
       ;; an earlier cache generation cannot repopulate a cleared cache,
       ;; even if singleflight is disabled. For clear-key!, a cancelled
       ;; singleflight owner must likewise never publish obsolete markup.
       (let [stored? (if (or flight (some? generation) (some? key-generation))
                       (locking (:cache manager)
                         (when (and (or (nil? generation)
                                        (= generation @(:cache-generation manager)))
                                    (or (nil? key-generation)
                                        (= key-generation
                                           (get-in @(:active-key-renders manager)
                                                   [key :generation])))
                                    (or (nil? flight)
                                        (not @(:cancel-requested? flight))))
                           (store!)
                           true))
                       ;; Trusted direct cache-store! calls have no render
                       ;; generation and retain their original nonblocking
                       ;; atom-only publication behavior.
                       (do (store!) true))]
         (when stored?
           (debug!
            debug-fn
            :gesso.live.fragment/cache-stored
            {:key key
             :stored-at (:stored-at entry)
             :expires-at (:expires-at entry)
             :ttl-ms ttl-ms
             :expired-count (:expired-count @prune-result)
             :evicted-count (:evicted-count @prune-result)
             :cache-count (count @(:cache manager))
             :at now})))))
   value))

(defn- begin-key-render!
  "Capture both cache generations and register an active render atomically
   with clear-key! and clear-cache!. The key epoch is retained only until
   the last render for that key finishes or is cancelled."
  [manager key]
  (locking (:cache manager)
    (let [[_ active] (swap-vals! (:active-key-renders manager)
                                 update key
                                 (fn [state]
                                   (update (or state {:generation 0 :count 0})
                                           :count inc)))]
      [@(:cache-generation manager) (get-in active [key :generation])])))

(defn- end-key-render!
  [manager key]
  ;; Completion must not wait for the invalidation lock: a render that has
  ;; already attempted publication should be able to notify its old caller
  ;; while clear-key! holds that lock (including a paused cancel hook).
  ;; CAS cleanup cannot race with a later cache write from this task.
  (swap! (:active-key-renders manager)
         (fn [entries]
           (let [state (get entries key)]
             (if (= 1 (:count state))
               (dissoc entries key)
               (update-in entries [key :count] dec))))))

(defn clear-cache!
  "Clear all cached fragments and invalidate publication from running renders.

   In-flight tasks may finish for their already-waiting callers, but results
   begun before this clear cannot repopulate the cache."
  [manager]
  (locking (:cache manager)
    (swap! (:cache-generation manager) inc)
    (reset! (:cache manager) {})
    ;; Existing waiters may finish their old flights, but a post-clear
    ;; request must never join a render started in the previous generation.
    ;; release-flight! removes only matching owners, so detached completions
    ;; cannot unregister replacements acquired after this clear.
    (reset! (:inflight manager) {}))
  manager)

(defn- cancel-once
  "Wrap an in-flight task canceller so competing shutdown paths call it once."
  [cancel!]
  (let [called? (atom false)]
    (fn []
      (when (compare-and-set! called? false true)
        (cancel!)))))

(defn- cancel-flight!
  [flight]
  ;; An owner can acquire its slot before it publishes the underlying task's
  ;; canceller. Remember the request, then allow the installer to honor it.
  (reset! (:cancel-requested? flight) true)
  (when-let [cancel! @(:cancel-ref flight)]
    (cancel!)))

(defn- reserve-flight-result!
  "Claim the single terminal outcome without invoking shared-flight callbacks."
  [flight]
  (compare-and-set! (:resolved? flight) false true))

(defn- assign-flight!
  "Resolve a shared flight at most once. Cancellation and physical render
   completion may both try to finish it under concurrent invalidation."
  [flight result]
  (when (reserve-flight-result! flight)
    ((:cell flight) result)
    true))

(defn clear-key!
  "Clear cached and in-flight state for one key.

   If a singleflight render exists, its running process is cancelled.
   Non-singleflight renders may finish for their existing callers, but
   cannot restore an invalidated cache entry after this boundary.

   This is mostly useful for tests or explicit invalidation. Normal live refresh
   paths usually rely on short TTLs and progression-aware keys instead."
  [manager key]
  (let [inflight (:inflight manager)
        ;; Reserve invalidation while holding the same cache lock as render
        ;; publication. Once this boundary wins, a concurrently finishing
        ;; render must not publish a successful outcome to old waiters.
        [flight reserved?] (locking (:cache manager)
                             (let [flight (get @inflight key)
                                   reserved? (when flight
                                               (reserve-flight-result! flight))]
                               (when flight
                                 (reset! (:cancel-requested? flight) true)
                                 ;; Detach before invoking a possibly blocking
                                 ;; physical cancellation callback.
                                 (swap! inflight
                                        (fn [flights]
                                          (if (identical? flight (get flights key))
                                            (dissoc flights key)
                                            flights))))
                               ;; A non-singleflight render also needs a
                               ;; key-scoped publication fence. Keep its epoch
                               ;; only while such renders are outstanding.
                               (swap! (:active-key-renders manager)
                                      (fn [entries]
                                        (if (contains? entries key)
                                          (update-in entries [key :generation] inc)
                                          entries)))
                               (swap! (:cache manager) dissoc key)
                               [flight reserved?]))]
    ;; Deliver outside the cache lock: resuming waiting tasks can trigger
    ;; arbitrary application callbacks, including new cache operations.
    (when flight
      (when reserved?
        ((:cell flight)
         {:status :failure
          :error (ex "gesso.live fragment render invalidated."
                     {:reason :fragment-invalidated
                      :key key})}))
      ;; A Missionary task may suppress completion callbacks when cancelled.
      ;; The shared outcome has already been selected for all waiting callers.
      ;; Cancelling an external render may itself throw. The invalidation
      ;; committed above must still complete, and an obsolete render is
      ;; already fenced from cache publication even if physical cancellation
      ;; failed. Record the failure without making clear-key! look uncommitted.
      (try
        (cancel-flight! flight)
        (catch Exception e
          (when (instance? InterruptedException e)
            (.interrupt (Thread/currentThread)))
          (debug!
           (get-in manager [:options :debug-fn])
           :gesso.live.fragment/physical-cancel-failed
           {:key key
            :error e
            :at ((get-in manager [:options :clock]))})))))
  manager)

(defn purge-expired!
  "Remove expired cache entries and return the number removed."
  [manager]
  (let [clock (get-in manager [:options :clock])
        now (clock)
        removed (atom 0)]
    (swap! (:cache manager)
           (fn [cache]
             (let [result (prune-cache cache now nil)]
               (reset! removed (:expired-count result))
               (:cache result))))
    @removed))

;; -----------------------------------------------------------------------------
;; Singleflight
;; -----------------------------------------------------------------------------

(defn- new-flight
  [_manager]
  {:cell (m/dfv)
   :resolved? (atom false)
   :cancel-ref (atom nil)
   :cancel-requested? (atom false)})

(defn- acquire-flight!
  [manager key flight]
  ;; Acquiring a flight and clearing the cache use one lifecycle boundary.
  ;; Otherwise an owner could register midway through clear-cache!, only to
  ;; be detached by that same clear. Capture generation at this acquisition
  ;; point, not at construction or delayed render start.
  (locking (:cache manager)
    (let [inflight (:inflight manager)]
      (loop []
        (let [m @inflight]
          (if-let [existing (get m key)]
            [:join existing]
            (let [owner (assoc flight :cache-generation
                               @(:cache-generation manager))]
              (if (compare-and-set! inflight m (assoc m key owner))
                [:owner owner]
                (recur)))))))))

(defn- release-flight!
  [manager key flight]
  (swap! (:inflight manager)
         (fn [m]
           (if (identical? flight (get m key))
             (dissoc m key)
             m))))

(defn- await-flight-task
  [key options flight role]
  (let [{:keys [clock debug-fn]} options]
    (m/sp
      (debug!
       debug-fn
       :gesso.live.fragment/singleflight-waiting
       {:key key
        :role role
        :at (clock)})
      (let [value (result-value (m/? (:cell flight)))]
        (debug!
         debug-fn
         :gesso.live.fragment/singleflight-reused
         {:key key
          :role role
          :at (clock)})
        value))))

(defn- render-store-task
  ([manager key task options]
   (render-store-task manager key task options nil))
  ([manager key task options flight]
   (let [{:keys [clock debug-fn]} options]
     (m/sp
       ;; A per-key epoch is needed even without singleflight: clear-key!
       ;; cannot cancel unregistered renders, but it can prevent obsolete
       ;; cache publication. Register under the same lock as invalidation.
       (let [[current-generation key-generation]
             (begin-key-render! manager key)
             generation (if flight
                          (:cache-generation flight)
                          current-generation)]
         (try
           (debug!
            debug-fn
            :gesso.live.fragment/render-started
            {:key key
             :at (clock)})
           (try
             (let [value (m/? task)]
               (cache-store! manager key value options flight generation
                             key-generation)
               (debug!
                debug-fn
                :gesso.live.fragment/render-succeeded
                {:key key
                 :at (clock)})
               value)
             (catch Throwable e
               (debug!
                debug-fn
                :gesso.live.fragment/render-failed
                {:key key
                 :error e
                 :at (clock)})
               (throw e)))
           (finally
             (end-key-render! manager key))))))))
(defn- start-flight!
  [manager key task options flight]
  (let [{:keys [clock debug-fn]} options
        render-task (render-store-task manager key task options flight)
        cancel!
        (render-task
         (fn [value]
           (release-flight! manager key flight)
           (assign-flight! flight {:status :success
                                   :value value})
           (debug!
            debug-fn
            :gesso.live.fragment/singleflight-released
            {:key key
             :status :success
             :at (clock)}))
         (fn [error]
           (release-flight! manager key flight)
           (assign-flight! flight {:status :failure
                                   :error error})
           (debug!
            debug-fn
            :gesso.live.fragment/singleflight-released
            {:key key
             :status :failure
             :error error
             :at (clock)})))]
    (let [cancel-once! (cancel-once cancel!)]
      (reset! (:cancel-ref flight) cancel-once!)
      (when @(:cancel-requested? flight)
        (cancel-once!)))
    flight))

(defn- protect-task*
  [manager key task options]
  (validate-key! key)
  (require-task! task)
  (m/sp
    (let [cached (cache-lookup manager key options)]
      (cond
        (instance? CacheHit cached)
        (.-value ^CacheHit cached)

        (not (:singleflight? options))
        (m/? (render-store-task manager key task options))

        :else
        (let [flight (new-flight manager)
              [role flight'] (acquire-flight! manager key flight)
              debug-fn (:debug-fn options)
              clock (:clock options)]
          (case role
            :owner
            (do
              (debug!
               debug-fn
               :gesso.live.fragment/singleflight-owner
               {:key key
                :at (clock)})
              (start-flight! manager key task options flight')
              (m/? (await-flight-task key options flight' :owner)))

            :join
            (do
              (debug!
               debug-fn
               :gesso.live.fragment/singleflight-joined
               {:key key
                :at (clock)})
              (m/? (await-flight-task key options flight' :join)))))))))

;; -----------------------------------------------------------------------------
;; Public render helpers
;; -----------------------------------------------------------------------------

(defn protect-task
  "Return a Missionary task protected by cache and singleflight.

   `task` should be a Missionary task that produces the rendered fragment value,
   usually an HTML string or Hiccup/rendered markup depending on your app.

   This is the lower-level API. Prefer render-task when you have a plain
   zero-arity render function."
  ([manager key task]
   (protect-task manager key task nil))
  ([manager key task options]
   (protect-task* manager key task (effective-options manager options))))

(defn call-task
  "Wrap a zero-arity function in a Missionary task using the selected executor.

   Executor may be:
     :blk
     :cpu
     java.util.concurrent.Executor"
  ([f]
   (call-task :blk f))
  ([executor f]
   (require-render-fn! f)
   (m/via-call (executor-value executor) f)))

(defn render-task
  "Return a protected Missionary task for a zero-arity render function.

   Example:

     (m/? (fragment/render-task manager
                                key
                                #(render-fragment ctx)
                                {:ttl-ms 50}))

   Use :executor :blk for blocking DB/read-heavy render work.
   Use :executor :cpu for CPU-heavy pure rendering."
  ([manager key render-fn]
   (render-task manager key render-fn nil))
  ([manager key render-fn options]
   (require-render-fn! render-fn)
   (let [options' (effective-options manager options)
         task (call-task (:executor options') render-fn)]
     (protect-task* manager key task options'))))

;; -----------------------------------------------------------------------------
;; Model-backed fragment adapters
;; -----------------------------------------------------------------------------

(def ^:private unmanaged-runtime-fragment-refresh-option-keys
  [:trigger :jitter-ms :jitter-delay-ms])

(defn- reject-unmanaged-runtime-fragment-refresh-options!
  [opts]
  (let [unsupported
        (into {}
              (keep
               (fn [k]
                 (let [value (get opts k)]
                   (when (some? value)
                     [k value]))))
              unmanaged-runtime-fragment-refresh-option-keys)]
    (when (seq unsupported)
      (throw
       (ex-info
        (str
         "gesso.live model-backed fragments no longer accept direct HTMX "
         "refresh trigger/jitter options. Refresh intent must enter the "
         "browser adapter before HTMX requests are emitted.")
        {:unsupported-options unsupported})))
    opts))

(defn- runtime-fragment-extra-options
  "Generic managed-fragment passthrough options accepted by
   gesso.live.ui/->fragment.

   These are intentionally domain-neutral. App-specific needs such as
   {:root-attrs {:hx-include \"#some-form\"}} belong in the app layer; this
   adapter merely preserves and forwards generic UI fragment opts that do not
   create an independent refresh path.

   Client continuity is passed through here as runtime UI metadata, but the
   capture/restore mechanics belong to gesso.live.ui / gesso.live.htmx and the
   browser runtime, not to fragment render protection."
  [opts]
  (select-keys opts
               [:attrs
                :root-attrs
                :target-attrs
                :event
                :client-continuity]))

(defn- normalize-fragment-swap
  [swap]
  (cond
    (keyword? swap) (name swap)
    (symbol? swap)  (name swap)
    (nil? swap)     nil
    :else           swap))

(defn fragment->runtime-fragment
  "Build the current Gesso Live UI fragment descriptor from a compiled model
   fragment.

   The app must pass explicit URLs. This function does not own routing.

   Required opts:
     :fragment-url
     :stream-url

   Generic managed-fragment passthrough opts:
     :attrs
     :root-attrs
     :target-attrs
     :event
     :client-continuity

   :swap may be supplied by opts to override the compiled fragment descriptor's
   :swap value.

   Direct refresh :trigger / jitter options are rejected. Adapter-managed
   fragments obtain refresh intent through the browser adapter and only issue an
   HTMX GET when that adapter emits gesso:live-refresh.

   Client continuity is runtime UI metadata and is therefore passed through
   unchanged. Browser request scheduling and authoritative-read interpretation
   remain outside this render-protection namespace."
  [compiled fragment-name id {:keys [fragment-url
                                     stream-url
                                     swap]
                              :as opts}]
  (reject-unmanaged-runtime-fragment-refresh-options! opts)
  (when-not (model/present? fragment-url)
    (throw
     (ex-info
      "fragment-url is required to build a live fragment panel."
      {:fragment fragment-name
       :id id
       :opts opts})))
  (when-not (model/present? stream-url)
    (throw
     (ex-info
      "stream-url is required to build a live fragment panel."
      {:fragment fragment-name
       :id id
       :opts opts})))
  (let [fragment (model/fragment-descriptor compiled fragment-name)]
    (ui/->fragment
     (compact-map
      (merge
       {:id (model/fragment-dom-id compiled fragment-name id)
        :src fragment-url
        :stream-url stream-url
        :subscription (model/fragment-scope-instance compiled fragment-name id)
        :swap (or swap (:swap fragment :outerHTML))}
       (runtime-fragment-extra-options opts))))))

(defn model-fragment-panel
  "Render a client-side live fragment panel using a compiled model fragment.

   Routes remain explicit in the app. This is a convenience adapter around
   gesso.live.ui/fragment-panel.

   Runtime fragment descriptors may keep model-level keyword swap values.
   Before rendering browser-facing HTMX attrs, normalize the swap to a string."
  [compiled fragment-name id opts]
  (ui/fragment-panel
   (update
    (fragment->runtime-fragment compiled fragment-name id opts)
    :swap
    normalize-fragment-swap)))
