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
   positive integer. The source uses leading-edge + trailing-edge per-scope
   throttling: the first invalidation for a scope is delivered immediately,
   repeated invalidations during the cooldown are collapsed to the latest value,
   and one trailing invalidation is delivered when the cooldown expires."
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
  (.size ^ConcurrentHashMap (:cooldowns source)))

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
       Optional positive integer. When set, source delivery uses leading-edge +
       trailing-edge per-scope throttling. The first invalidation for a scope is
       delivered immediately. Repeated invalidations for that scope during the
       cooldown window are suppressed and collapsed to the latest pending value.
       If anything changed during the cooldown, one trailing invalidation is
       delivered when the window expires.

       nil or 0 disables source-level coalescing/throttling.

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

      ;; Source-level pre-fanout coalescing/throttling.
      :pending (ConcurrentHashMap.)
      :cooldowns (ConcurrentHashMap.)
      :coalesce-lock (Object.)
      :scheduler scheduler

      ;; Counters.
      :accepted-count (LongAdder.)
      :fanout-count (LongAdder.)
      :attempted-count (LongAdder.)
      :coalesced-count (LongAdder.)
      :leading-count (LongAdder.)
      :trailing-count (LongAdder.)
      :suppressed-count (LongAdder.)
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
      (.clear ^ConcurrentHashMap (:cooldowns source))
      (.shutdownNow scheduler)))
  :closed)

(defn stats
  "Return a small source status map.

   Compatibility note: :tap-count intentionally reports all active consumer
   streams, including indexed subscriptions. Older tests and callers used
   :tap-count as the upstream-consumer count. Newer diagnostics can use
   :broadcast-tap-count and :subscription-count when they need the split."
  [source]
  (let [broadcast-count (tap-count source)
        subscription-count (subscription-count source)
        accepted-count (.sum ^LongAdder (:accepted-count source))]
    {:id (:id source)
     :created-at (:created-at source)
     :closed? (closed? source)
     :tap-count (+ broadcast-count subscription-count)
     :broadcast-tap-count broadcast-count
     :subscription-count subscription-count
     :indexed-scope-count (indexed-scope-count source)
     :pending-count (pending-count source)
     :scheduled-count (scheduled-count source)
     :emitted-count accepted-count
     :accepted-count accepted-count
     :fanout-count (.sum ^LongAdder (:fanout-count source))
     :attempted-count (.sum ^LongAdder (:attempted-count source))
     :coalesced-count (.sum ^LongAdder (:coalesced-count source))
     :leading-count (.sum ^LongAdder (:leading-count source))
     :trailing-count (.sum ^LongAdder (:trailing-count source))
     :suppressed-count (.sum ^LongAdder (:suppressed-count source))
     :error-count (count @(:errors source))
     :coalesce-window-ms (:coalesce-window-ms source)}))

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

(declare flush-cooldown!)

(defn- schedule-cooldown-flush!
  [source k window-ms]
  (when-not (closed? source)
    (.schedule ^ScheduledThreadPoolExecutor (:scheduler source)
               ^Runnable
               (fn []
                 (try
                   (flush-cooldown! source k window-ms)
                   (catch Throwable e
                     (record-error!
                      source
                      {:kind :coalesce-flush-error
                       :scope-key k
                       :error e}))))
               (long window-ms)
               TimeUnit/MILLISECONDS))
  nil)

(defn- flush-cooldown!
  [source k window-ms]
  ;; Leading/trailing throttle semantics:
  ;; - cooldown marker remains active while a trailing value is delivered and
  ;;   the next cooldown window is scheduled.
  ;; - cooldown marker is removed only when the window expires with no pending
  ;;   invalidation.
  ;;
  ;; The coalesce-lock protects the small pending/cooldown state transition so a
  ;; concurrent emit cannot strand a pending invalidation while a flush removes
  ;; the cooldown marker. Fanout happens outside the lock.
  (let [invalidation
        (locking (:coalesce-lock source)
          (when-not (closed? source)
            (if-let [pending (.remove ^ConcurrentHashMap (:pending source) k)]
              pending
              (do
                (.remove ^ConcurrentHashMap (:cooldowns source) k)
                nil))))]
    (when invalidation
      (.increment ^LongAdder (:trailing-count source))
      (deliver-now! source invalidation)
      (schedule-cooldown-flush! source k window-ms)))
  nil)

(defn- throttle-coalesced!
  [source invalidation window-ms]
  (let [k (scope-key invalidation)
        action
        (locking (:coalesce-lock source)
          (if (nil? (.putIfAbsent ^ConcurrentHashMap (:cooldowns source) k Boolean/TRUE))
            :leading
            (do
              (.put ^ConcurrentHashMap (:pending source) k invalidation)
              :suppressed)))]
    (case action
      :leading
      (do
        (.increment ^LongAdder (:leading-count source))
        (let [result (deliver-now! source invalidation)]
          (schedule-cooldown-flush! source k window-ms)
          (assoc result
                 :status :emitted
                 :coalescing :leading
                 :coalesced? false)))

      :suppressed
      (do
        ;; Count every suppressed logical invalidation, not only replacements of
        ;; an already-pending value. This matches the benchmark's idea of
        ;; logical wakeups that do not become separate physical SSE wakeups.
        (.increment ^LongAdder (:coalesced-count source))
        (.increment ^LongAdder (:suppressed-count source))
        {:status :queued
         :source/id (:id source)
         :scope-key k
         :coalescing :suppressed
         :coalesced? true
         :attempted 0
         :invalidation invalidation}))))

;; -----------------------------------------------------------------------------
;; Emission
;; -----------------------------------------------------------------------------

(defn emit!
  "Emit one already-expanded invalidation into source.

   source/emit! validates against :gesso.live/invalidation, not
   :gesso.live/primary-change.

   With no positive :coalesce-window-ms, delivery is immediate.

   With :coalesce-window-ms, delivery uses leading-edge + trailing-edge
   per-scope throttling. The first invalidation for a scope is delivered
   immediately. Later invalidations for that scope during the cooldown window
   are suppressed and collapsed to the latest value; one trailing fanout is
   delivered when the cooldown expires if anything changed.

   Manifold put! is asynchronous. The returned :attempted count means the source
   attempted to put the invalidation onto open consumer streams. It does not
   mean the consumer has synchronously processed the value.

   Suppressed invalidations return :status :queued and :attempted 0 because
   their fanout is represented by a later trailing delivery."
  [source invalidation]
  (ensure-open! source)
  (let [invalidation' (validate-invalidation! invalidation)]
    (.increment ^LongAdder (:accepted-count source))
    (if-let [window-ms (positive-coalesce-window-ms source)]
      (throttle-coalesced! source invalidation' window-ms)
      (assoc (deliver-now! source invalidation') :status :emitted))))

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
