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
   repeated invalidations during the cooldown are collapsed to the latest
   descriptive value while their authoritative progression requirements are
   composed conservatively, and one trailing invalidation is delivered when the
   cooldown expires."
  (:require
   [gesso.live.progression :as progression]
   [gesso.live.schema :as schema]
   [manifold.deferred :as d]
   [manifold.stream :as s])
  (:import
   [java.util.concurrent ConcurrentHashMap ScheduledThreadPoolExecutor TimeUnit]
   [java.util.concurrent.atomic AtomicBoolean AtomicInteger AtomicLong LongAdder]))

;; -----------------------------------------------------------------------------
;; Defaults
;; -----------------------------------------------------------------------------

(def default-options
  {:id nil
   :coalesce-window-ms nil
   :max-recorded-errors 256
   :max-active-coalesce-scopes 4096
   :max-pending-puts-per-consumer 64
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
  ;; The bounded retention setting must still be validated when explicitly
  ;; overridden with nil; omitting it from compact-map would silently restore
  ;; unbounded history or defer a failure until diagnostic recording.
  (assoc (compact-map
          (select-keys options [:id :coalesce-window-ms :on-error]))
         :max-recorded-errors (:max-recorded-errors options)
         :max-active-coalesce-scopes (:max-active-coalesce-scopes options)
         :max-pending-puts-per-consumer (:max-pending-puts-per-consumer options)))

(defn- validate-options!
  [options]
  (schema/validate! :gesso.live/source-options
                    (source-option-contract options))
  options)

(defn- source-id
  [options]
  (or (:id options)
      (random-uuid)))

(defn- restore-interrupt!
  "A suppressed source callback or physical put interruption must preserve
   the JVM thread's cancellation signal."
  [error]
  (when (instance? InterruptedException error)
    (.interrupt (Thread/currentThread))))

(defn- call-safely
  [f & args]
  (when f
    (try
      (apply f args)
      ;; Diagnostics supplied by applications must never abort fanout or
      ;; strand the coalescing timer. AssertionError and other Error subtypes
      ;; are not Exceptions, but are equally outside source delivery's trust
      ;; boundary when thrown by this optional hook.
      (catch Throwable error
        (restore-interrupt! error)
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

(defn- coalesce-pending-invalidation
  "Collapse two already-validated invalidations for the same source scope.

   The newer invalidation owns ordinary descriptive payload fields because source
   coalescing has historically been latest-value for those fields. Authoritative
   progression is different: it is a refresh requirement, so dropping an older
   distinct requirement would weaken the eventual trailing wakeup. Compose the
   two optional requirements conservatively and attach the result to the newer
   invalidation.

   This function intentionally does not compare opaque bases or use arrival order
   as authority ordering; gesso.live.progression/compose is orderless set union."
  [pending incoming]
  (let [requirement
        (progression/compose (:progression pending)
                             (:progression incoming))]
    (cond-> incoming
      requirement
      (assoc :progression requirement)

      (nil? requirement)
      (dissoc :progression))))

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
  "Return the bounded, oldest-first vector of retained source errors.

   A long-running source never keeps more than :max-recorded-errors records.
   Evicted errors remain visible in aggregate counters and through the
   optional :on-error hook at the time they occur. Source/response acceptance
   is not proof of eventual browser delivery."
  [source]
  (:retained @(:errors source)))

(defn clear-errors!
  "Discard retained error objects without resetting lifetime counters."
  [source]
  (swap! (:errors source) assoc :retained [])
  :cleared)

(defn- record-error!
  [source entry]
  (let [entry' (assoc entry
                      :source/id (:id source)
                      :recorded-at (now-ms))
        limit (:max-recorded-errors source)]
    ;; Keep diagnostics bounded even when a broken sink fails continuously.
    ;; One pure atomic transition owns both retention and counters so CAS
    ;; retries never overcount and concurrent put callbacks cannot lose work.
    ;; Rebuild the capped vector instead of retaining a SubVector backed by
    ;; an arbitrarily long historical vector.
    (swap! (:errors source)
           (fn [{:keys [retained] :as history}]
             (let [next-retained (conj retained entry')
                   evicted (max 0 (- (count next-retained) limit))]
               (-> history
                   (assoc :retained (if (pos? evicted)
                                      (vec (drop evicted next-retained))
                                      next-retained))
                   (update :total inc)
                   (update :evicted + evicted)))))
    ;; This callback is observational and receives EACH failure, including
    ;; those omitted from or evicted out of the bounded diagnostic history.
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
       ;; A retired consumer cannot receive future invalidations. Close its
       ;; stream so Live/SSE observers see termination rather than hang.
       (s/close! stream)
       (record-put-rejected! source :broadcast-tap tap-id invalidation)))
   (fn [error]
     (remove-tap-if-same! source tap-id stream)
     (s/close! stream)
     (record-put-error! source :broadcast-tap tap-id invalidation error)))
  nil)

(defn- put-consumer!
  "Place an invalidation onto one Manifold consumer. Kept as a small
   boundary so rejected and failed physical puts can be tested reliably:
   manifold.stream/put! is compiler-inlined and cannot be with-redefs'd."
  [stream invalidation]
  (s/put! stream invalidation))

(defn- release-pending-put!
  [^AtomicInteger pending]
  ;; -1 means the consumer has been retired for overload. Completion
  ;; callbacks racing with retirement cannot revive it or undo its fence.
  (loop []
    (let [n (.get pending)]
      (when (pos? n)
        (when-not (.compareAndSet pending n (dec n))
          (recur))))))

(defn- reserve-pending-put!
  [source kind id stream invalidation]
  (when-let [^AtomicInteger pending
             (.get ^ConcurrentHashMap (:consumer-pending-puts source) stream)]
    (loop []
      (let [n (.get pending)]
        (cond
          (neg? n) nil
          (< n (:max-pending-puts-per-consumer source))
          (if (.compareAndSet pending n (inc n))
            pending
            (recur))

          (.compareAndSet pending n -1)
          (do
            ;; A slow stream must be CLOSED rather than silently dropping an
            ;; invalidation that is still required for authoritative refresh.
            ;; SSE's normal disconnect/rejoin performs a fresh acquisition.
            ;; Retirement is one-shot even under concurrent source emissions.
            (when (.remove ^ConcurrentHashMap (:consumer-pending-puts source)
                           stream pending)
              (.increment ^LongAdder (:slow-consumer-retire-count source))
              (s/close! stream)
              (record-error! source
                             {:kind :slow-consumer-retired
                              :consumer/kind kind
                              :consumer/id id
                              :pending-puts n
                              :invalidation invalidation}))
            nil)
          :else (recur))))))

(defn- bounded-consumer-put!
  "Allow at most N unresolved Manifold put! deferreds per consumer. A fully
   queued/unread stream is retired before another put is attempted. Returns
   nil on retirement, otherwise the normal Manifold put deferred."
  [source kind id stream invalidation]
  (when-let [^AtomicInteger pending
             (reserve-pending-put! source kind id stream invalidation)]
    (let [released? (AtomicBoolean. false)
          release! (fn []
                     (when (.compareAndSet released? false true)
                       (release-pending-put! pending)))]
      (try
        (let [put-result (put-consumer! stream invalidation)]
          (d/on-realized put-result
                         (fn [_] (release!))
                         (fn [_] (release!)))
          put-result)
        (catch Throwable e
          (release!)
          (throw e))))))

(defn- put-tap!
  [source tap-id stream invalidation]
  (cond
    (s/closed? stream)
    (do
      (remove-tap-if-same! source tap-id stream)
      false)

    :else
    (try
      (if-let [put-result (bounded-consumer-put!
                           source :broadcast-tap tap-id stream invalidation)]
        (do
          (track-tap-put-result! source tap-id stream invalidation put-result)
          true)
        false)
      (catch Throwable e
        (restore-interrupt! e)
        (remove-tap-if-same! source tap-id stream)
        (s/close! stream)
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
  ;; Registration and bucket retirement must share a lifecycle lock. Otherwise
  ;; a new subscriber can insert into a bucket after the last old subscriber
  ;; tests it empty but before that bucket is removed from :by-scope.
  (locking (:coalesce-lock source)
    (when (.remove ^ConcurrentHashMap (:subscriptions source) subscription-id stream)
      (when-let [k (.remove ^ConcurrentHashMap (:subscription-keys source)
                            subscription-id)]
        (when-let [bucket (.get ^ConcurrentHashMap (:by-scope source) k)]
          (.remove ^ConcurrentHashMap bucket subscription-id stream)
          (when (zero? (.size ^ConcurrentHashMap bucket))
            (.remove ^ConcurrentHashMap (:by-scope source) k bucket)))))
    nil))

(defn- track-subscription-put-result!
  [source subscription-id stream invalidation put-result]
  (d/on-realized
   put-result
   (fn [accepted?]
     (when-not accepted?
       (remove-subscription-if-same! source subscription-id stream)
       (s/close! stream)
       (record-put-rejected! source :indexed-subscription subscription-id invalidation)))
   (fn [error]
     (remove-subscription-if-same! source subscription-id stream)
     (s/close! stream)
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
      (if-let [put-result (bounded-consumer-put!
                           source :indexed-subscription subscription-id
                           stream invalidation)]
        (do
          (track-subscription-put-result!
           source subscription-id stream invalidation put-result)
          true)
        false)
      (catch Throwable e
        (restore-interrupt! e)
        (remove-subscription-if-same! source subscription-id stream)
        (s/close! stream)
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
       cooldown window are suppressed and collapsed to the latest pending
       descriptive value while progression requirements are composed. If anything
       changed during the cooldown, one trailing invalidation is delivered when
       the window expires.

       nil or 0 disables source-level coalescing/throttling.

     :max-recorded-errors
       Optional integer 0..4096 (default 256). Bounds retained diagnostic
       entries, including invalidations/throwables from failed puts. Zero
       disables object retention while :on-error and lifetime counts remain.

     :max-active-coalesce-scopes
       Optional integer 0..65536 (default 4096). Bounds the number of
       distinct per-scope cooldown timers and pending coalesced values. If
       the limit is reached, a NEW scope bypasses coalescing and emits its
       notification immediately without dropping its progression requirement.
       An already tracked scope still receives leading/trailing coalescing.
       Zero bypasses all coalescing even when a window is configured.

     :max-pending-puts-per-consumer
       Optional integer 1..4096 (default 64). A Manifold put! can remain
       unresolved when the reader stalls. Before the pending count reaches
       unbounded growth, retire/close that consumer with a diagnostic instead
       of issuing another put. Healthy consumers continue receiving events.
       A disconnected SSE client must reacquire current authoritative state;
       source emission is NOT proof of browser receipt.

     :on-error
       Optional function called for every source error, even if its record is
       evicted from the bounded history.

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
      :max-recorded-errors (:max-recorded-errors options')
      :max-active-coalesce-scopes (:max-active-coalesce-scopes options')
      :max-pending-puts-per-consumer (:max-pending-puts-per-consumer options')
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
      ;; A registered consumer owns a bounded number of unresolved Manifold
      ;; put! operations. The map is drained on stream close or source close.
      :consumer-pending-puts (ConcurrentHashMap.)

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
      :coalesce-bypass-count (LongAdder.)
      :slow-consumer-retire-count (LongAdder.)
      :errors (atom {:retained [] :total 0 :evicted 0})})))

(defn close!
  "Close source and all active streams.

   Idempotent."
  [source]
  ;; Registration and retirement share the lifecycle lock: a stream is either
  ;; captured by this close or cannot register after the closed flag is set.
  ;; Manifold stream close hooks are external callbacks, however, so run them
  ;; only AFTER releasing that lock. Hooks can wait for other threads that
  ;; must themselves acquire the registration lock.
  (let [streams (locking (:coalesce-lock source)
                  (when (compare-and-set! (:closed? source) false true)
                    (let [taps ^ConcurrentHashMap (:taps source)
                          subscriptions ^ConcurrentHashMap (:subscriptions source)
                          scheduler ^ScheduledThreadPoolExecutor (:scheduler source)
                          streams (into [] (concat (.values taps)
                                                   (.values subscriptions)))]
                      (.clear taps)
                      (.clear subscriptions)
                      (.clear ^ConcurrentHashMap (:subscription-keys source))
                      (.clear ^ConcurrentHashMap (:by-scope source))
                      (.clear ^ConcurrentHashMap (:consumer-pending-puts source))
                      (.clear ^ConcurrentHashMap (:pending source))
                      (.clear ^ConcurrentHashMap (:cooldowns source))
                      (.shutdownNow scheduler)
                      streams)))]
    (doseq [stream streams]
      (s/close! stream)))
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
     :coalesce-bypass-count (.sum ^LongAdder (:coalesce-bypass-count source))
     :max-active-coalesce-scopes (:max-active-coalesce-scopes source)
     :max-pending-puts-per-consumer (:max-pending-puts-per-consumer source)
     :pending-consumer-put-count
     (reduce + 0 (map (fn [^AtomicInteger n] (max 0 (.get n)))
                      (.values ^ConcurrentHashMap
                               (:consumer-pending-puts source))))
     :slow-consumer-retire-count
     (.sum ^LongAdder (:slow-consumer-retire-count source))
     :error-count (count (:retained @(:errors source)))
     :total-error-count (:total @(:errors source))
     :evicted-error-count (:evicted @(:errors source))
     :max-recorded-errors (:max-recorded-errors source)
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
  (locking (:coalesce-lock source)
    (ensure-open! source)
    (let [tap-id (next-tap-id! source)
          stream (s/stream)
          taps ^ConcurrentHashMap (:taps source)]
      (.put taps tap-id stream)
      (.put ^ConcurrentHashMap (:consumer-pending-puts source)
            stream (AtomicInteger. 0))
      (s/on-closed stream
                   #(do
                      (.remove ^ConcurrentHashMap
                               (:consumer-pending-puts source) stream)
                      (remove-tap-if-same! source tap-id stream)))
      stream)))

(defn subscribe
  "Create an indexed Manifold stream for a topic/id subscription.

   The subscription and invalidations are matched by the default source
   scope-key, currently (select-keys value [:topic :id]).

   This is the normal handoff point from source.clj to gesso.live.flow."
  [source subscription]
  (locking (:coalesce-lock source)
    (ensure-open! source)
    ;; A missing key is not the only invalid interest. Validate the full
    ;; public subscription contract before installing indexed resources.
    ;; Otherwise unmatchable topics/ids become silent live consumers.
    (schema/validate-subscription! subscription)
    (let [k (scope-key subscription)
          subscription-id (next-subscription-id! source)
          stream (s/stream)
          bucket (scope-bucket source k)]
      (.put ^ConcurrentHashMap (:subscriptions source) subscription-id stream)
      (.put ^ConcurrentHashMap (:subscription-keys source) subscription-id k)
      (.put ^ConcurrentHashMap bucket subscription-id stream)
      (.put ^ConcurrentHashMap (:consumer-pending-puts source)
            stream (AtomicInteger. 0))
      (s/on-closed stream
                   #(do
                      (.remove ^ConcurrentHashMap
                               (:consumer-pending-puts source) stream)
                      (remove-subscription-if-same! source subscription-id stream)))
      stream)))

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
  ;; The open check and scheduler submission share close!'s lifecycle lock.
  ;; Otherwise close! may shut down the scheduler after the check but before
  ;; schedule, turning an already accepted leading invalidation into a
  ;; RejectedExecutionException after its fanout has occurred.
  (locking (:coalesce-lock source)
    (when-not (closed? source)
      (.schedule ^ScheduledThreadPoolExecutor (:scheduler source)
                 ^Runnable
                 (fn []
                   (try
                     (flush-cooldown! source k window-ms)
                     (catch Throwable e
                       (restore-interrupt! e)
                       (record-error!
                        source
                        {:kind :coalesce-flush-error
                         :scope-key k
                         :error e}))))
                 (long window-ms)
                 TimeUnit/MILLISECONDS)))
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
          ;; Acceptance and coalescing must be atomic with close!'s cleanup.
          ;; The initial open check in emit! alone can race with validation.
          (ensure-open! source)
          (.increment ^LongAdder (:accepted-count source))
          (let [cooldowns ^ConcurrentHashMap (:cooldowns source)]
            (cond
              (.containsKey cooldowns k)
              (do
                (let [pending-map ^ConcurrentHashMap (:pending source)
                      pending (.get pending-map k)
                      coalesced (if pending
                                  (coalesce-pending-invalidation pending invalidation)
                                  invalidation)]
                  (.put pending-map k coalesced))
                :suppressed)

              ;; This decision and cooldown admission are serialized with
              ;; concurrent emitters, timer expiry, and source close!. No
              ;; rejected notification disappears: a new untracked scope
              ;; falls back to immediate physical source fanout.
              (>= (.size cooldowns) (:max-active-coalesce-scopes source))
              :capacity-bypass

              :else
              (do
                (.put cooldowns k Boolean/TRUE)
                :leading))))]
    (case action
      :capacity-bypass
      (do
        (.increment ^LongAdder (:coalesce-bypass-count source))
        (assoc (deliver-now! source invalidation)
               :status :emitted
               :coalescing :capacity-bypass
               :coalesced? false))

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
   are suppressed and collapsed to the latest descriptive value while
   authoritative progression requirements are composed conservatively; one
   trailing fanout is delivered when the cooldown expires if anything changed.

   Manifold put! is asynchronous. The returned :attempted count means the source
   attempted to put the invalidation onto open consumer streams. It does not
   mean the consumer has synchronously processed the value.

   Suppressed invalidations return :status :queued and :attempted 0 because
   their fanout is represented by a later trailing delivery."
  [source invalidation]
  (ensure-open! source)
  (let [invalidation' (validate-invalidation! invalidation)]
    (if-let [window-ms (positive-coalesce-window-ms source)]
      (throttle-coalesced! source invalidation' window-ms)
      (do
        ;; Do not hold the lifecycle lock across consumer fanout. But the
        ;; transition to accepted must serialize with close!, otherwise a
        ;; paused validation can accept work after shutdown completes.
        (locking (:coalesce-lock source)
          (ensure-open! source)
          (.increment ^LongAdder (:accepted-count source)))
        (assoc (deliver-now! source invalidation') :status :emitted)))))

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
