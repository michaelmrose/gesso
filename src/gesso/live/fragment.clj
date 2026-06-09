(ns gesso.live.fragment
  "Fragment render protection for gesso.live.

   This namespace protects expensive HTML fragment rendering from stampedes.

   It provides:

   - singleflight: concurrent renders for the same key share one in-flight result
   - short TTL cache: near-simultaneous requests can reuse fresh rendered output
   - optional cache size bound
   - consistency-token-aware key helpers
   - Missionary render tasks using m/via-call
   - conditional debug tracing
   - model-backed adapters from gesso.live.model fragment descriptors to
     gesso.live.ui fragment descriptors

   The render-protection machinery does not:

   - know HTMX
   - know SSE
   - know XTDB
   - know Ring response maps
   - decide subscription interests

   The model-backed adapter section at the bottom delegates to gesso.live.ui.
   It still does not own routes, streams, Ring handlers, or subscriptions beyond
   constructing the current UI fragment descriptor from compiled model metadata.

   The caller is responsible for choosing a key that includes every value that
   can affect the rendered HTML, such as user/scope/params/theme/locale and any
   consistency token."
  (:require
   [gesso.live.model :as model]
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
  `(when-let [f# ~debug-fn]
     (f# (assoc ~data :event ~event))))

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

(def key-dimensions
  [:scope
   :user-key
   :variant
   :params
   :locale
   :theme
   :consistency-token])

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
    (when-not (and (integer? v) (pos? v)))
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
     :consistency-token

   The consistency token is just another render-affecting dimension. Including
   it means strict read-after-write requests do not share stale cached output
   with requests from an older visibility point."
  ([base]
   (fragment-key base nil))
  ([base dimensions]
   (let [dimensions' (or dimensions {})]
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
     :consistency-token

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
                                 (:stored-at entry)))
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

          (cache-entry-valid? now entry)
          (do
            (debug!
             debug-fn
             :gesso.live.fragment/cache-hit
             {:key key
              :stored-at (:stored-at entry)
              :expires-at (:expires-at entry)
              :at now})
            (:value entry))

          :else
          (do
            (swap! (:cache manager) dissoc key)
            (debug!
             debug-fn
             :gesso.live.fragment/cache-expired
             {:key key
              :stored-at (:stored-at entry)
              :expires-at (:expires-at entry)
              :at now})
            miss))))))

(defn- cache-store!
  [manager key value options]
  (when (cache-enabled? options)
    (let [{:keys [clock ttl-ms max-cache-entries debug-fn]} options
          now (clock)
          entry (cache-entry value now ttl-ms)
          prune-result (atom nil)]
      (swap! (:cache manager)
             (fn [cache]
               (let [result (prune-cache (assoc cache key entry)
                                         now
                                         max-cache-entries)]
                 (reset! prune-result result)
                 (:cache result))))
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
        :at now})))
  value)

(defn clear-cache!
  "Clear all cached fragment values."
  [manager]
  (reset! (:cache manager) {})
  manager)

(defn- cancel-flight!
  [flight]
  (when-let [cancel! @(:cancel-ref flight)]
    (cancel!)))

(defn clear-key!
  "Clear cached and in-flight state for one key.

   If an in-flight render exists, its running process is cancelled.

   This is mostly useful for tests or explicit invalidation. Normal live refresh
   paths usually rely on short TTLs and consistency-token-aware keys instead."
  [manager key]
  (swap! (:cache manager) dissoc key)
  (when-let [flight (get @(:inflight manager) key)]
    (cancel-flight! flight))
  (swap! (:inflight manager) dissoc key)
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
  []
  {:cell (m/dfv)
   :cancel-ref (atom nil)})

(defn- acquire-flight!
  [manager key flight]
  (let [inflight (:inflight manager)]
    (loop []
      (let [m @inflight]
        (if-let [existing (get m key)]
          [:join existing]
          (if (compare-and-set! inflight m (assoc m key flight))
            [:owner flight]
            (recur)))))))

(defn- release-flight!
  [manager key flight]
  (swap! (:inflight manager)
         (fn [m]
           (if (identical? flight (get m key))
             (dissoc m key)
             m))))

(defn- assign-flight!
  [flight result]
  ((:cell flight) result))

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
  [manager key task options]
  (let [{:keys [clock debug-fn]} options]
    (m/sp
      (debug!
       debug-fn
       :gesso.live.fragment/render-started
       {:key key
        :at (clock)})
      (try
        (let [value (m/? task)]
          (cache-store! manager key value options)
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
          (throw e))))))

(defn- start-flight!
  [manager key task options flight]
  (let [{:keys [clock debug-fn]} options
        render-task (render-store-task manager key task options)
        cancel!
        (render-task
         (fn [value]
           (assign-flight! flight {:status :success
                                   :value value})
           (release-flight! manager key flight)
           (debug!
            debug-fn
            :gesso.live.fragment/singleflight-released
            {:key key
             :status :success
             :at (clock)}))
         (fn [error]
           (assign-flight! flight {:status :failure
                                   :error error})
           (release-flight! manager key flight)
           (debug!
            debug-fn
            :gesso.live.fragment/singleflight-released
            {:key key
             :status :failure
             :error error
             :at (clock)})))]
    (reset! (:cancel-ref flight) cancel!)
    flight))

(defn- protect-task*
  [manager key task options]
  (validate-key! key)
  (require-task! task)
  (m/sp
    (let [cached (cache-lookup manager key options)]
      (if-not (= miss cached)
        cached

        (if-not (:singleflight? options)
          (m/? (render-store-task manager key task options))

          (let [flight (new-flight)
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
                (m/? (await-flight-task key options flight' :join))))))))))

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

(defn- runtime-fragment-extra-options
  "Generic passthrough options accepted by gesso.live.ui/->fragment.

   These are intentionally domain-neutral. App-specific needs such as
   {:target-attrs {:hx-include \"#some-form\"}} belong in the app layer; this
   adapter merely preserves and forwards the caller's generic UI fragment opts."
  [opts]
  (select-keys opts
               [:attrs
                :root-attrs
                :target-attrs
                :event
                :trigger
                :jitter-ms
                :jitter-delay-ms]))

(defn fragment->runtime-fragment
  "Build the current Gesso Live UI fragment descriptor from a compiled model
   fragment.

   The app must pass explicit URLs. This function does not own routing.

   Required opts:
     :fragment-url
     :stream-url

   Generic UI passthrough opts:
     :attrs
       General fragment attrs, when supported by gesso.live.ui.

     :root-attrs
       Attrs merged onto the outer SSE wrapper.

     :target-attrs
       Attrs merged onto the inner HTMX refresh target. This is useful for
       ordinary app-local HTMX behavior such as hx-include, hx-indicator, or
       app-specific data attrs.

     :event
     :trigger
     :jitter-ms
     :jitter-delay-ms

   :swap may be supplied by opts to override the compiled fragment descriptor's
   :swap value.

   Note: :request-policy and :consistency remain model metadata. They are not
   passed to ui/->fragment here."
  [compiled fragment-name id {:keys [fragment-url
                                     stream-url
                                     swap]
                              :as opts}]
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
   gesso.live.ui/fragment-panel."
  [compiled fragment-name id opts]
  (ui/fragment-panel
   (fragment->runtime-fragment compiled fragment-name id opts)))
