(ns gesso.live.flow
  "Per-client flow construction for gesso.live.

   This namespace bridges a source tap into a Missionary flow and applies
   client-local policy:

   - subscription filtering
   - invalidation -> live-event wrapping
   - optional stale invalidation relief/coalescing
   - conditional debug tracing

   It does not:
   - expand primary changes
   - publish to source
   - build SSE frames
   - render HTMX attrs
   - read XTDB

   source.clj is the hot in-process broadcaster.
   flow.clj is the cold per-client recipe."
  (:require
   [gesso.live.htmx :as htmx]
   [gesso.live.schema :as schema]
   [gesso.live.source :as source]
   [manifold.deferred :as d]
   [manifold.stream :as s]
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

(def default-event
  htmx/default-event)

(def default-options
  {:event default-event
   :data nil
   :consistency-token nil
   :relieve? true
   :debug-fn nil
   :on-close nil})

(def closed-sentinel
  ::closed)

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

(defn default-interested?
  "Default subscription matcher.

   A subscription is interested in an invalidation when :topic and :id match."
  [subscription invalidation]
  (= (select-keys subscription [:topic :id])
     (select-keys invalidation [:topic :id])))

(defn invalidation-key
  "Return the default coalescing/matching key for an invalidation."
  [invalidation]
  (select-keys invalidation [:topic :id]))

(defn- flow-option-contract
  [options]
  (compact-map
   (select-keys options
                [:subscription
                 :interested?])))

(defn- event-option-contract
  [options]
  (compact-map
   (select-keys options
                [:event
                 :data])))

(defn- require-fn-option!
  [options k]
  (when-let [f (get options k)]
    (when-not (fn? f)
      (throw
       (ex (str "gesso.live flow " k " must be a function.")
           {k f}))))
  options)

(defn- require-boolean-option!
  [options k]
  (let [v (get options k)]
    (when-not (or (true? v) (false? v))
      (throw
       (ex (str "gesso.live flow " k " must be a boolean.")
           {k v}))))
  options)

(defn prepare-options!
  "Merge defaults, normalize optional public options, validate contracts, and
   return the normalized option map.

   Public API note:

   :interested? is optional for callers. Internally it is normalized to
   default-interested? before schema validation so downstream code can treat it
   as present."
  [options]
  (let [options'  (opts options)
        options'' (update options' :interested? #(or % default-interested?))]
    (schema/validate! :gesso.live/flow-for-subscription-options
                      (flow-option-contract options''))
    (schema/validate! :gesso.live/invalidation-event-options
                      (event-option-contract options''))
    (require-fn-option! options'' :debug-fn)
    (require-fn-option! options'' :on-close)
    (require-boolean-option! options'' :relieve?)
    options''))

(defn- call-safely
  [f & args]
  (when f
    (try
      (apply f args)
      (catch Exception _
        nil))))

(defn- data-value
  [data invalidation]
  (cond
    (fn? data)
    (data invalidation)

    (some? data)
    data

    :else
    nil))

;; -----------------------------------------------------------------------------
;; Event wrapping
;; -----------------------------------------------------------------------------

(defn invalidation-event
  "Wrap an invalidation in the canonical live-event shape.

   Options:
     :event
       Event name/ref. Keywords and symbols are normalized via htmx/event-name.

     :data
       Optional static value or function of invalidation.

     :consistency-token
       Optional consistency token attached to the live event."
  ([invalidation]
   (invalidation-event invalidation nil))
  ([invalidation options]
   (let [options' (opts options)
         event'   (htmx/event-name (:event options'))
         data'    (data-value (:data options') invalidation)
         token    (:consistency-token options')
         value    (cond-> {:event event'
                           :invalidation invalidation}
                    (some? data')
                    (assoc :data data')

                    (some? token)
                    (assoc :consistency-token token))]
     (schema/validate-live-event! value))))

(defn- safe-interested?
  [interested? subscription invalidation debug-fn]
  (try
    (boolean (interested? subscription invalidation))
    (catch Exception e
      (debug!
       debug-fn
       :gesso.live.flow/interested-predicate-failed
       {:subscription subscription
        :invalidation invalidation
        :error e
        :at (now-ms)})
      (throw
       (ex "gesso.live flow interested? predicate failed."
           {:subscription subscription
            :invalidation invalidation}
           e)))))

(defn- safe-event
  [invalidation options debug-fn]
  (try
    (invalidation-event invalidation options)
    (catch Exception e
      (debug!
       debug-fn
       :gesso.live.flow/event-build-failed
       {:invalidation invalidation
        :error e
        :at (now-ms)})
      (throw
       (ex "gesso.live flow failed to build live event."
           {:invalidation invalidation}
           e)))))

;; -----------------------------------------------------------------------------
;; Manifold deferred -> Missionary task
;; -----------------------------------------------------------------------------

(defn deferred-task
  "Adapt a Manifold deferred to a Missionary task."
  [deferred]
  (fn [success failure]
    (let [cancelled? (atom false)]
      (d/on-realized
       deferred
       (fn [value]
         (when-not @cancelled?
           (success value)))
       (fn [error]
         (when-not @cancelled?
           (failure error))))
      (fn cancel []
        (reset! cancelled? true)))))

;; -----------------------------------------------------------------------------
;; Manifold stream -> Missionary flow
;; -----------------------------------------------------------------------------

(defn stream-flow
  "Convert a Manifold stream into a Missionary discrete flow.

   The flow pulls one value at a time from the Manifold stream using s/take!.
   This avoids the eager s/consume firehose.

   The next s/take! is not installed until the previous value has crossed the
   Missionary observe boundary. On the JVM, m/observe's emit callback blocks
   while a transfer is pending, so this preserves downstream pressure better
   than s/consume.

   The flow terminates when s/take! returns closed-sentinel.

   Options:
     :debug-fn
     :on-close

   The stream is closed when the Missionary flow is cancelled or terminates."
  ([stream]
   (stream-flow stream nil))
  ([stream {:keys [debug-fn on-close]}]
   (let [close-once!
         (let [closed? (atom false)]
           (fn []
             (when (compare-and-set! closed? false true)
               (call-safely on-close)
               (s/close! stream)
               (debug!
                debug-fn
                :gesso.live.flow/stream-flow-closed
                {:at (now-ms)}))))

         raw-flow
         (m/observe
          (fn [emit!]
            (let [cancelled? (atom false)]
              (letfn [(fail! [error]
                        (when-not @cancelled?
                          (reset! cancelled? true)
                          (try
                            (emit! {::stream-error error})
                            (finally
                              (close-once!)))))

                      (finish! []
                        (when-not @cancelled?
                          (debug!
                           debug-fn
                           :gesso.live.flow/stream-closed
                           {:at (now-ms)})
                          (reset! cancelled? true)
                          (try
                            (emit! closed-sentinel)
                            (finally
                              (close-once!)))))

                      (pull! []
                        (when-not @cancelled?
                          (d/on-realized
                           (s/take! stream closed-sentinel)

                           (fn [value]
                             (when-not @cancelled?
                               (if (= closed-sentinel value)
                                 (finish!)
                                 (try
                                   (debug!
                                    debug-fn
                                    :gesso.live.flow/stream-value
                                    {:value value
                                     :at (now-ms)})

                                   ;; Critical bit:
                                   ;; emit! may block until downstream accepts
                                   ;; the value, so we do not schedule the next
                                   ;; s/take! until this returns.
                                   (emit! value)

                                   (when-not @cancelled?
                                     (pull!))
                                   (catch Throwable e
                                     (fail! e))))))

                           (fn [error]
                             (fail! error)))))]

                (pull!)

                (fn cancel []
                  (reset! cancelled? true)
                  (close-once!))))))]

     (m/eduction
      (comp
       (take-while #(not= closed-sentinel %))
       (map
        (fn [value]
          (if-let [error (::stream-error value)]
            (throw error)
            value))))
      raw-flow))))



;; -----------------------------------------------------------------------------
;; Flow transforms
;; -----------------------------------------------------------------------------

(defn subscription-xf
  "Return a transducer that filters invalidations for a subscription and maps
   matching invalidations to live events."
  [{:keys [subscription interested? debug-fn] :as options}]
  (let [interested-fn (or interested? default-interested?)]
    (mapcat
     (fn [invalidation]
       (debug!
        debug-fn
        :gesso.live.flow/invalidation-received
        {:subscription subscription
         :invalidation invalidation
         :at (now-ms)})

       (if (safe-interested? interested-fn subscription invalidation debug-fn)
         (let [event (safe-event invalidation options debug-fn)]
           (debug!
            debug-fn
            :gesso.live.flow/invalidation-matched
            {:subscription subscription
             :invalidation invalidation
             :live-event event
             :at (now-ms)})
           [event])
         (do
           (debug!
            debug-fn
            :gesso.live.flow/invalidation-ignored
            {:subscription subscription
             :invalidation invalidation
             :at (now-ms)})
           []))))))

(defn relieve
  "Apply Missionary relief/coalescing to a flow.

   With m/relieve, stale pending values can be discarded when the consumer is
   slower than the producer. This is appropriate for cheap invalidation-first
   wakeups.

   This does not cancel expensive render/data-load work. Rendered stream paths
   should use m/?<= around the render branch when newer invalidations make older
   render work stale."
  [flow]
  (m/relieve flow))

(defn maybe-relieve
  "Apply relief when :relieve? is truthy."
  [flow {:keys [relieve?]}]
  (if relieve?
    (relieve flow)
    flow))

;; -----------------------------------------------------------------------------
;; Public API
;; -----------------------------------------------------------------------------

(defn- flow-for-stream*
  [stream options]
  (let [debug-fn (:debug-fn options)]
    (debug!
     debug-fn
     :gesso.live.flow/flow-for-stream-created
     {:subscription (:subscription options)
      :relieve? (:relieve? options)
      :at (now-ms)})

    (->> (stream-flow stream
                      {:debug-fn debug-fn
                       :on-close (:on-close options)})
         (m/eduction (subscription-xf options))
         (#(maybe-relieve % options)))))

(defn flow-for-stream
  "Build a client live-event flow from a Manifold stream tap.

   Options:
     :subscription       required
     :interested?        optional predicate, defaults to topic/id equality
     :event              optional event name/ref
     :data               optional static value or fn of invalidation
     :consistency-token  optional token attached to events
     :relieve?           default true
     :debug-fn           optional pay-for-play debug hook
     :on-close           optional internal cleanup/debug hook

   Returns a Missionary flow of live-event maps."
  [stream options]
  (flow-for-stream* stream (prepare-options! options)))

(defn flow-for-source
  "Open a source tap and build a client live-event flow from it.

   This validates options before opening the tap so invalid options do not leak
   a registered source tap.

   This is the usual handoff from source.clj to flow.clj."
  [src options]
  (let [options'  (prepare-options! options)
        debug-fn  (:debug-fn options')
        tap       (source/changes src)
        options'' (assoc options'
                         :on-close
                         (fn []
                           (call-safely (:on-close options'))
                           (debug!
                            debug-fn
                            :gesso.live.flow/source-tap-closed
                            {:source/id (:id src)
                             :subscription (:subscription options')
                             :at (now-ms)})))]
    (try
      (debug!
       debug-fn
       :gesso.live.flow/source-tap-opened
       {:source/id (:id src)
        :subscription (:subscription options')
        :at (now-ms)})

      (flow-for-stream* tap options'')
      (catch Throwable e
        (s/close! tap)
        (throw e)))))

(defn collect-task
  "Return a Missionary task that collects a finite flow into a vector.

   This is mostly useful for tests."
  [flow]
  (m/reduce conj [] flow))
