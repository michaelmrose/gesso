(ns gesso.live.source
  "In-process invalidation source for gesso.live.

   A source is an app-local hot source of already-expanded invalidations.

   It does not:
   - expand primary changes
   - know app rules
   - inspect database writes
   - know about HTMX
   - know about SSE
   - know about XTDB

   Writers/app helpers emit expanded invalidations into the source.

   Normal consumers call subscribe to receive only invalidations matching their
   subscription scope. This is the indexed-interest hot path.

   Consumers may still call changes to receive a broadcast tap of future
   invalidations. That path is kept as a compatibility/fallback mechanism for
   unusual consumers, tests, or custom predicates that cannot be indexed.

   Source-level coalescing happens before fanout when :coalesce-window-ms is a
   positive integer. Repeated invalidations for the same scope within that window
   are collapsed to the latest invalidation and fanned out once."
  (:require
   [gesso.live.schema :as schema]
   [manifold.deferred :as d]
   [manifold.stream :as s])
  (:import
   [java.util.concurrent ConcurrentHashMap ScheduledThreadPoolExecutor TimeUnit]
   [java.util.concurrent.atomic AtomicLong LongAdder]))

;; -----------------------------------------------------------------------------
;; Defaults
;; -----------------------------------------------------------------------------

(def default-options
  {:id nil
   :coalesce-window-ms nil
   :on-error nil})

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn- now-ms
  []
  (System/currentTimeMillis))

(defn- ex
  [message data]
  (ex-info message data))

(defn- opts
  [options]
  (merge default-options options))

(defn- compact-map
  [m]
  (into {}
        (remove (comp nil? val))
        m))

(defn- source-option-contract
  [options]
  (compact-map
   (select-keys options
                [:id
                 :coalesce-window-ms
                 :on-error])))

(defn- validate-options!
  [options]
  (schema/validate! :gesso.live/source-options
                    (source-option-contract options))
  options)

(defn- source-id
  [options]
  (or (:id options)
      (random-uuid)))

(defn- call-safely
  [f & args]
  (when f
    (try
      (apply f args)
      (catch Exception _
        nil))))

(defn closed?
  "Return true if source has been closed."
  [source]
  (true? @(:closed? source)))

(defn- ensure-open!
  [source]
  (when (closed? source)
    (throw
     (ex "gesso.live source is closed."
         {:source/id (:id source)})))
  source)

(defn- validate-invalidation!
  [invalidation]
  (schema/validate-invalidation! invalidation))

(defn scope-key
  "Return the default source routing/coalescing key for a subscription or
   invalidation.

   Gesso Live keeps source routing generic. App-specific expansion rules are
   responsible for producing already-expanded topic/id invalidations. The source
   only compares the scope key of an invalidation with the scope key of a
   subscription."
  [x]
  (let [k (select-keys x [:topic :id])]
    (when-not (and (contains? k :topic)
                   (contains? k :id))
      (throw
       (ex "gesso.live source values must contain :topic and :id for indexed routing."
           {:value x})))
    k))

(defn- next-tap-id!
  [source]
  (.incrementAndGet ^AtomicLong (:tap-counter source)))

(defn- next-subscription-id!
  [source]
  (.incrementAndGet ^AtomicLong (:subscription-counter source)))

(defn- positive-coalesce-window-ms
  [source]
  (let [v (:coalesce-window-ms source)]
    (when (and (integer? v)
               (pos? v))
      v)))

(defn- tap-count
  [source]
  (.size ^ConcurrentHashMap (:taps source)))

(defn- subscription-count
  [source]
  (.size ^ConcurrentHashMap (:subscriptions source)))

(defn- indexed-scope-count
  [source]
  (.size ^ConcurrentHashMap (:by-scope source)))

(defn- pending-count
  [source]
  (.size ^ConcurrentHashMap (:pending source)))

(defn- scheduled-count
  [source]
  (.size ^ConcurrentHashMap (:scheduled source)))

;; -----------------------------------------------------------------------------
;; Error/rejection recording
;; -----------------------------------------------------------------------------

(defn errors
  "Return recorded source put errors/rejections."
  [source]
  @(:errors source))

(defn clear-errors!
  "Clear recorded source errors."
  [source]
  (reset! (:errors source) [])
  :cleared)

(defn- record-error!
  [source entry]
  (let [entry' (assoc entry
                      :source/id (:id source)
                      :recorded-at (now-ms))]
    (swap! (:errors source) conj entry')
    (call-safely (:on-error source) entry')
    entry'))

(defn- record-put-error!
  [source consumer-kind consumer-id invalidation error]
  (record-error!
   source
   {:kind :put-error
    :consumer/kind consumer-kind
    :consumer/id consumer-id
    :invalidation invalidation
    :error error}))

(defn- record-put-rejected!
  [source consumer-kind consumer-id invalidation]
  (record-error!
   source
   {:kind :put-rejected
    :consumer/kind consumer-kind
    :consumer/id consumer-id
    :invalidation invalidation}))

;; -----------------------------------------------------------------------------
;; Broadcast tap helpers
;; -----------------------------------------------------------------------------

(defn- remove-tap-if-same!
  [source tap-id stream]
  (.remove ^ConcurrentHashMap (:taps source) tap-id stream)
  nil)

(defn- track-tap-put-result!
  [source tap-id stream invalidation put-result]
  (d/on-realized
   put-result
   (fn [accepted?]
     (when-not accepted?
       (remove-tap-if-same! source tap-id stream)
       (record-put-rejected! source :broadcast-tap tap-id invalidation)))
   (fn [error]
     (remove-tap-if-same! source tap-id stream)
     (record-put-error! source :broadcast-tap tap-id invalidation error)))
  nil)

(defn- put-tap!
  [source tap-id stream invalidation]
  (cond
    (s/closed? stream)
    (do
      (remove-tap-if-same! source tap-id stream)
      false)

    :else
    (try
      (let [put-result (s/put! stream invalidation)]
        (track-tap-put-result! source tap-id stream invalidation put-result)
        true)
      (catch Throwable e
        (remove-tap-if-same! source tap-id stream)
        (record-put-error! source :broadcast-tap tap-id invalidation e)
        false))))

;; -----------------------------------------------------------------------------
;; Indexed subscription helpers
;; -----------------------------------------------------------------------------

(defn- new-bucket
  []
  (ConcurrentHashMap.))

(defn- scope-bucket
  [source k]
  (.computeIfAbsent
   ^ConcurrentHashMap (:by-scope source)
   k
   (reify java.util.function.Function
     (apply [_ _]
       (new-bucket)))))

(defn- remove-subscription-if-same!
  [source subscription-id stream]
  (when (.remove ^ConcurrentHashMap (:subscriptions source) subscription-id stream)
    (when-let [k (.remove ^ConcurrentHashMap (:subscription-keys source)
                          subscription-id)]
      (when-let [bucket (.get ^ConcurrentHashMap (:by-scope source) k)]
        (.remove ^ConcurrentHashMap bucket subscription-id stream)
        (when (zero? (.size ^ConcurrentHashMap bucket))
          (.remove ^ConcurrentHashMap (:by-scope source) k bucket)))))
  nil)

(defn- track-subscription-put-result!
  [source subscription-id stream invalidation put-result]
  (d/on-realized
   put-result
   (fn [accepted?]
     (when-not accepted?
       (remove-subscription-if-same! source subscription-id stream)
       (record-put-rejected! source :indexed-subscription subscription-id invalidation)))
   (fn [error]
     (remove-subscription-if-same! source subscription-id stream)
     (record-put-error! source :indexed-subscription subscription-id invalidation error)))
  nil)

(defn- put-subscription!
  [source subscription-id stream invalidation]
  (cond
    (s/closed? stream)
    (do
      (remove-subscription-if-same! source subscription-id stream)
      false)

    :else
    (try
      (let [put-result (s/put! stream invalidation)]
        (track-subscription-put-result! source subscription-id stream invalidation put-result)
        true)
      (catch Throwable e
        (remove-subscription-if-same! source subscription-id stream)
        (record-put-error! source :indexed-subscription subscription-id invalidation e)
        false))))

;; -----------------------------------------------------------------------------
;; Creation/lifecycle
;; -----------------------------------------------------------------------------

(defn create
  "Create an in-process invalidation source.

   Options:
     :id
       Optional source id.

     :coalesce-window-ms
       Optional positive integer. When set, repeated invalidations for the same
       topic/id scope within this window are collapsed before fanout. The latest
       invalidation wins.

       nil or 0 disables source-level coalescing.

     :on-error
       Optional function called with source error entries.

   Normal subscribers should use subscribe. changes remains available as a
   broadcast fallback."
  ([] (create nil))
  ([options]
   (let [options'  (validate-options! (opts options))
         scheduler (ScheduledThreadPoolExecutor. 1)]
     (.setRemoveOnCancelPolicy scheduler true)
     {:id (source-id options')
      :created-at (now-ms)
      :coalesce-window-ms (:coalesce-window-ms options')
      :on-error (:on-error options')
      :closed? (atom false)

      ;; Broadcast/fallback taps. These receive every delivered invalidation.
      :tap-counter (AtomicLong. 0)
      :taps (ConcurrentHashMap.)

      ;; Indexed subscriptions. These receive only matching scope invalidations.
      :subscription-counter (AtomicLong. 0)
      :subscriptions (ConcurrentHashMap.)
      :subscription-keys (ConcurrentHashMap.)
      :by-scope (ConcurrentHashMap.)

      ;; Source-level pre-fanout coalescing.
      :pending (ConcurrentHashMap.)
      :scheduled (ConcurrentHashMap.)
      :scheduler scheduler

      ;; Counters.
      :accepted-count (LongAdder.)
      :fanout-count (LongAdder.)
      :attempted-count (LongAdder.)
      :coalesced-count (LongAdder.)
      :errors (atom [])})))

(defn close!
  "Close source and all active streams.

   Idempotent."
  [source]
  (when (compare-and-set! (:closed? source) false true)
    (let [taps ^ConcurrentHashMap (:taps source)
          subscriptions ^ConcurrentHashMap (:subscriptions source)
          scheduler ^ScheduledThreadPoolExecutor (:scheduler source)]
      (doseq [stream (.values taps)]
        (s/close! stream))
      (doseq [stream (.values subscriptions)]
        (s/close! stream))
      (.clear taps)
      (.clear subscriptions)
      (.clear ^ConcurrentHashMap (:subscription-keys source))
      (.clear ^ConcurrentHashMap (:by-scope source))
      (.clear ^ConcurrentHashMap (:pending source))
      (.clear ^ConcurrentHashMap (:scheduled source))
      (.shutdownNow scheduler)))
  :closed)

(defn stats
  "Return a small source status map."
  [source]
  {:id (:id source)
   :created-at (:created-at source)
   :closed? (closed? source)
   :tap-count (tap-count source)
   :subscription-count (subscription-count source)
   :indexed-scope-count (indexed-scope-count source)
   :pending-count (pending-count source)
   :scheduled-count (scheduled-count source)
   :accepted-count (.sum ^LongAdder (:accepted-count source))
   :fanout-count (.sum ^LongAdder (:fanout-count source))
   :attempted-count (.sum ^LongAdder (:attempted-count source))
   :coalesced-count (.sum ^LongAdder (:coalesced-count source))
   :error-count (count @(:errors source))
   :coalesce-window-ms (:coalesce-window-ms source)})

;; -----------------------------------------------------------------------------
;; Consumers
;; -----------------------------------------------------------------------------

(defn changes
  "Create a broadcast Manifold stream tap of future expanded invalidations.

   This compatibility/fallback path receives every delivered invalidation. Prefer
   subscribe for ordinary topic/id subscriptions so unrelated invalidations are
   never sent to uninterested client flows."
  [source]
  (ensure-open! source)
  (let [tap-id (next-tap-id! source)
        stream (s/stream)
        taps ^ConcurrentHashMap (:taps source)]
    (.put taps tap-id stream)
    (s/on-closed stream #(remove-tap-if-same! source tap-id stream))
    stream))

(defn subscribe
  "Create an indexed Manifold stream for a topic/id subscription.

   The subscription and invalidations are matched by the default source
   scope-key, currently (select-keys value [:topic :id]).

   This is the normal handoff point from source.clj to gesso.live.flow."
  [source subscription]
  (ensure-open! source)
  (let [k (scope-key subscription)
        subscription-id (next-subscription-id! source)
        stream (s/stream)
        bucket (scope-bucket source k)]
    (.put ^ConcurrentHashMap (:subscriptions source) subscription-id stream)
    (.put ^ConcurrentHashMap (:subscription-keys source) subscription-id k)
    (.put ^ConcurrentHashMap bucket subscription-id stream)
    (s/on-closed stream #(remove-subscription-if-same! source subscription-id stream))
    stream))

;; -----------------------------------------------------------------------------
;; Delivery
;; -----------------------------------------------------------------------------

(defn- deliver-now!
  [source invalidation]
  (let [k (scope-key invalidation)
        taps ^ConcurrentHashMap (:taps source)
        bucket (.get ^ConcurrentHashMap (:by-scope source) k)
        broadcast-attempted
        (loop [attempted 0
               it (.iterator (.entrySet taps))]
          (if (.hasNext it)
            (let [entry (.next it)
                  tap-id (.getKey entry)
                  stream (.getValue entry)]
              (recur
               (if (put-tap! source tap-id stream invalidation)
                 (inc attempted)
                 attempted)
               it))
            attempted))
        indexed-attempted
        (if bucket
          (loop [attempted 0
                 it (.iterator (.entrySet ^ConcurrentHashMap bucket))]
            (if (.hasNext it)
              (let [entry (.next it)
                    subscription-id (.getKey entry)
                    stream (.getValue entry)]
                (recur
                 (if (put-subscription! source subscription-id stream invalidation)
                   (inc attempted)
                   attempted)
                 it))
              attempted))
          0)
        attempted (+ broadcast-attempted indexed-attempted)]
    (.increment ^LongAdder (:fanout-count source))
    (.add ^LongAdder (:attempted-count source) attempted)
    {:status :delivered
     :source/id (:id source)
     :scope-key k
     :tap-count (.size taps)
     :subscription-count (if bucket
                           (.size ^ConcurrentHashMap bucket)
                           0)
     :attempted attempted
     :invalidation invalidation}))

(defn- flush-scope!
  [source k]
  ;; Remove the scheduled marker first. If a concurrent emit for the same key
  ;; arrives while this flush is running, it will be allowed to schedule a fresh
  ;; flush. In the benign race where this flush also delivers that newer pending
  ;; value immediately, the later scheduled flush becomes a no-op.
  (.remove ^ConcurrentHashMap (:scheduled source) k)
  (when-not (closed? source)
    (when-let [invalidation (.remove ^ConcurrentHashMap (:pending source) k)]
      (deliver-now! source invalidation)))
  nil)

(defn- schedule-flush!
  [source k window-ms]
  (when (nil? (.putIfAbsent ^ConcurrentHashMap (:scheduled source) k Boolean/TRUE))
    (.schedule ^ScheduledThreadPoolExecutor (:scheduler source)
               ^Runnable (fn []
                           (try
                             (flush-scope! source k)
                             (catch Throwable e
                               (record-error!
                                source
                                {:kind :coalesce-flush-error
                                 :scope-key k
                                 :error e}))))
               (long window-ms)
               TimeUnit/MILLISECONDS))
  nil)

(defn- enqueue-coalesced!
  [source invalidation window-ms]
  (let [k (scope-key invalidation)
        previous (.put ^ConcurrentHashMap (:pending source) k invalidation)
        coalesced? (some? previous)]
    (when coalesced?
      (.increment ^LongAdder (:coalesced-count source)))
    (schedule-flush! source k window-ms)
    {:status :queued
     :source/id (:id source)
     :scope-key k
     :coalesced? coalesced?
     :attempted 0
     :invalidation invalidation}))

;; -----------------------------------------------------------------------------
;; Emission
;; -----------------------------------------------------------------------------

(defn emit!
  "Emit one already-expanded invalidation into source.

   source/emit! validates against :gesso.live/invalidation, not
   :gesso.live/primary-change.

   With no positive :coalesce-window-ms, delivery is immediate.

   With :coalesce-window-ms, delivery is delayed until the per-scope coalescing
   window flushes. Multiple invalidations for the same topic/id scope collapse
   to the latest invalidation before fanout.

   Manifold put! is asynchronous. The returned :attempted count means the source
   attempted to put the invalidation onto open consumer streams. It does not
   mean the consumer has synchronously processed the value.

   In coalescing mode, :attempted is 0 because fanout happens later."
  [source invalidation]
  (ensure-open! source)
  (let [invalidation' (validate-invalidation! invalidation)]
    (.increment ^LongAdder (:accepted-count source))
    (if-let [window-ms (positive-coalesce-window-ms source)]
      (enqueue-coalesced! source invalidation' window-ms)
      (deliver-now! source invalidation'))))

(defn emit-many!
  "Emit many already-expanded invalidations into source.

   All invalidations are validated before emission starts, preventing ordinary
   malformed-input partial emission.

   This operation is not atomic with respect to concurrent close!. A concurrent
   close! may stop later emissions from occurring."
  [source invalidations]
  (ensure-open! source)
  (let [invalidations' (mapv validate-invalidation! invalidations)
        results (mapv #(emit! source %) invalidations')]
    {:status :emitted-many
     :source/id (:id source)
     :count (count invalidations')
     :results results}))
