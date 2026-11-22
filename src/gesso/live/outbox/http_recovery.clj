(ns gesso.live.outbox.http-recovery
  "Host-driven, opt-in pacing for explicit cleanup of disconnected SSE sessions.

   This does not start a worker, timer, thread or retry loop. A trusted owner
   calls :step! from its own bounded scheduler, and retains responsibility for
   scheduling, logging errors, and eventual route/provider shutdown. The
   managed HTTP route remains the authority on physical source release and
   occupied capacity."
  (:require [gesso.live.outbox.http :as http])
  (:import [java.util.concurrent ScheduledThreadPoolExecutor TimeUnit
            ThreadFactory]))

(def ^:private default-min-interval-ms 1000)
(def ^:private default-max-attempts 16)
(def ^:private default-max-failure-interval-ms 60000)

(defn- failure-interval-ms
  "Wait for the minimum interval after one failure, then double after each
   consecutive failure until the configured cap. Saturate before multiplying
   again; long failure streaks cannot overflow a JVM integer or create an
   unbounded calculation. A successful physical pass resets the streak."
  [minimum maximum failures]
  (loop [remaining (dec failures)
         interval minimum]
    (if (or (<= remaining 0) (>= interval maximum))
      interval
      (recur (dec remaining) (min maximum (* 2 interval))))))

(defn- reject! [reason]
  (throw (ex-info "Invalid managed SSE recovery controller."
                  {:error/type :gesso.live.outbox/invalid-http-recovery
                   :reason reason})))

(defn prepare!
  "Create an explicit host-owned recovery controller for a managed SSE route.

   Options: :min-interval-ms (1..120000, default 1000), :max-attempts
   (1..1024, default 16), :max-failure-interval-ms (at least the minimum,
   at most 120000, default max(minimum, 60000)), and optional trusted
   :now-ms monotonic clock.
   :now-ms exists for deterministic testing; production uses nanoTime.

   :step! attempts no cleanup if the route has no disconnected obligations,
   is already closed, or the minimum interval since the last attempted pass
   has not elapsed. It serializes concurrent calls to itself, and updates the
   clock fence BEFORE physical cleanup, including failures; otherwise repeated
   errors could create an unbounded hot retry loop. Consecutive physical
   failures progressively increase the interval after the second failure,
   capped by :max-failure-interval-ms. The first failure retains the minimum
   interval; a successful physical pass resets the streak. A failed batch
   can have released other source interests: pacing never claims otherwise.
   An idle observation (zero outstanding debt) also clears the streak so
   future, unrelated disconnected sessions do not inherit stale backoff.
   No scheduling is implicit.

   :step! returns {:outcome :idle/:deferred/:route-closed/:attempted ...},
   or throws the original physical cleanup error. :stats reports this
   controller's attempts and errors, not source-level evidence of delivery.
   The per-call attempt limit is NOT a wall-clock deadline; physical cleanup
   callbacks can block. Operators must not treat skipped/failed attempts as
   proof of provider safety or lost-event recovery."
  [route options]
  (when-not (and (map? route)
                 (every? #(fn? (get route %))
                         [:retry-disconnected! :disconnected-count :closed?]))
    (reject! :invalid-route))
  (when-not (and (map? options)
                 (every? #{:min-interval-ms :max-attempts
                             :max-failure-interval-ms :now-ms}
                         (keys options)))
    (reject! :invalid-options))
  (let [min-interval-ms (get options :min-interval-ms default-min-interval-ms)
        max-attempts (get options :max-attempts default-max-attempts)
        max-failure-interval-ms
        (get options :max-failure-interval-ms
             (if (integer? min-interval-ms)
               (max min-interval-ms default-max-failure-interval-ms)
               default-max-failure-interval-ms))
        now-ms (get options :now-ms
                    #(quot (System/nanoTime) 1000000))]
    (when-not (and (integer? min-interval-ms)
                   (<= 1 min-interval-ms 120000))
      (reject! :invalid-min-interval-ms))
    (when-not (and (integer? max-attempts)
                   (<= 1 max-attempts 1024))
      (reject! :invalid-max-attempts))
    (when-not (and (integer? max-failure-interval-ms)
                   (<= min-interval-ms max-failure-interval-ms 120000))
      (reject! :invalid-max-failure-interval-ms))
    (when-not (fn? now-ms)
      (reject! :invalid-clock))
    (let [state (atom {:last-attempt-ms nil
                       :attempt-count 0
                       :failure-count 0
                       :consecutive-failures 0})
          guard (Object.)]
      {:step!
       (fn []
         (locking guard
           (cond
             ((:closed? route))
             {:outcome :route-closed}

             :else
             (let [pending ((:disconnected-count route))]
               (when-not (and (integer? pending) (<= 0 pending))
                 (reject! :invalid-pending-count))
               (if (zero? pending)
                 (do (swap! state assoc :consecutive-failures 0)
                     {:outcome :idle :pending 0})
                 (let [now (now-ms)
                       last-attempt (:last-attempt-ms @state)
                       wait-ms (failure-interval-ms
                                min-interval-ms max-failure-interval-ms
                                (:consecutive-failures @state))]
                   (when-not (integer? now)
                     (reject! :invalid-clock-result))
                   (when (and last-attempt (< now last-attempt))
                     (reject! :nonmonotonic-clock))
                   (let [elapsed (when last-attempt (- now last-attempt))]
                     (if (and elapsed (< elapsed wait-ms))
                       {:outcome :deferred
                        :pending pending
                        :retry-after-ms (- wait-ms elapsed)}
                       (do
                         (swap! state (fn [s]
                                        (-> s
                                            (assoc :last-attempt-ms now)
                                            (update :attempt-count inc))))
                         (try
                           (let [result ((:retry-disconnected! route)
                                         max-attempts)]
                             (when-not (and (map? result)
                                            (integer? (:attempted result))
                                            (integer? (:recovered result))
                                            (<= 0 (:recovered result)
                                                (:attempted result)
                                                max-attempts))
                               (reject! :invalid-retry-result))
                             (let [remaining ((:disconnected-count route))]
                               (swap! state assoc :consecutive-failures 0)
                               {:outcome :attempted
                                :attempted (:attempted result)
                                :recovered (:recovered result)
                                :remaining remaining}))
                           (catch Throwable error
                             (swap! state (fn [s]
                                            (-> s
                                                (update :failure-count inc)
                                                (update :consecutive-failures inc))))
                             (throw error))))))))))))
       :stats (fn [] @state)})))

(def ^:private default-poll-ms 1000)
(def ^:private default-stop-timeout-ms 5000)

(defn start-scheduled!
  "Explicitly own periodic cleanup ticks for one managed Ring SSE route.

   This is an opt-IN daemon scheduler, never started by route construction or
   prepare!. The owning HTTP host MUST stop and confirm drainage of this
   handle before retiring the route, listener, Live and XTDB providers.

   Options: :poll-ms (10..120000, default 1000), plus the :min-interval-ms
   :max-attempts and :max-failure-interval-ms options accepted by prepare!.
   One executor thread is
   owned by this handle, and only one step! runs at a time. Cleanup errors
   are retained in bounded diagnostics; a failed tick must not cancel future
   ticks. :last-outcome describes the most recent scheduler tick, while
   :last-physical-outcome retains the most recent attempted cleanup or error
   across intervening deferred/idle ticks. :last-error is historical, not a
   claim that cleanup is still failing; consult :consecutive-failures and the
   route-owned outstanding debt. No delivery/release outcome is inferred from
   a caught exception.

   :stop! requests cancellation without interrupting in-flight physical
   cleanup and awaits termination for at most the supplied milliseconds
   (default 5000). A timeout means provider shutdown is NOT admitted.
   Repeated :stop! calls may finish a previously timed-out drain. Neither
   the poll interval nor a bounded retry batch bounds callback duration."
  [route options]
  (when-not (and (map? options)
                 (every? #{:poll-ms :min-interval-ms :max-attempts
                             :max-failure-interval-ms}
                         (keys options)))
    (reject! :invalid-scheduled-options))
  (let [poll-ms (get options :poll-ms default-poll-ms)]
    (when-not (and (integer? poll-ms) (<= 10 poll-ms 120000))
      (reject! :invalid-poll-ms))
    (let [controller (prepare! route (dissoc options :poll-ms))
          stopping? (atom false)
          observations (atom {:tick-count 0 :failure-count 0
                              :last-outcome nil :last-physical-outcome nil
                              :last-error nil})
          factory (reify ThreadFactory
                    (newThread [_ runnable]
                      (doto (Thread. ^Runnable runnable
                                     "gesso-live-sse-recovery")
                        (.setDaemon true))))
          executor (ScheduledThreadPoolExecutor. 1 factory)
          task (reify Runnable
                 (run [_]
                   (when-not @stopping?
                     (swap! observations update :tick-count inc)
                     (try
                       (let [result ((:step! controller))]
                         (swap! observations
                                (fn [snapshot]
                                  (cond-> (assoc snapshot :last-outcome result)
                                    (= :attempted (:outcome result))
                                    (assoc :last-physical-outcome result)))))
                       (catch Throwable error
                         ;; Schedule-with-fixed-delay otherwise silently
                         ;; suppresses every future run after an exception.
                         ;; Preserve one failure for the trusted host owner.
                         (swap! observations
                                (fn [s]
                                  (-> s
                                      (update :failure-count inc)
                                      (assoc :last-outcome {:outcome :failed}
                                             :last-physical-outcome
                                             {:outcome :failed}
                                             :last-error error))))
                         (when (instance? InterruptedException error)
                           (.interrupt (Thread/currentThread))))))))]
      (.setRemoveOnCancelPolicy executor true)
      (.setContinueExistingPeriodicTasksAfterShutdownPolicy executor false)
      (let [scheduled (.scheduleWithFixedDelay
                       executor task 0 (long poll-ms) TimeUnit/MILLISECONDS)]
        (letfn [(stop! ([] (stop! default-stop-timeout-ms))
                  ([timeout-ms]
                   (when-not (and (integer? timeout-ms)
                                  (<= 0 timeout-ms 120000))
                     (reject! :invalid-stop-timeout-ms))
                   (when (compare-and-set! stopping? false true)
                     (.cancel scheduled false)
                     (.shutdown executor))
                   (try
                     {:outcome (if (.awaitTermination
                                    executor (long timeout-ms)
                                    TimeUnit/MILLISECONDS)
                                 :drained :timed-out)}
                     (catch InterruptedException error
                       (.interrupt (Thread/currentThread))
                       (throw error)))))]
          {:stop! stop!
           :drained? (fn [] (.isTerminated executor))
           :status (fn [] (cond
                            (.isTerminated executor) :drained
                            @stopping? :stopping
                            :else :running))
           :stats (fn [] (assoc @observations
                               :controller ((:stats controller))))})))))

(defn prepare-owned-shutdown
  "Compose scheduler drainage ahead of existing provider-safe HTTP shutdown.

   The host owns both the recovery handle and the exact ordinary shutdown
   plan. A scheduler timeout leaves the listener, route and providers intact;
   retry the SAME coordinator once the in-flight tick has finished. On drain,
   delegate to the existing serialized, once-only HTTP shutdown coordinator.
   Its listener and host acknowledgments retain their original requirements.

   This never starts a network listener or creates a Biff host. The owner
   must call :stop! rather than bypassing this coordinator for safe ordering."
  [recovery-handle shutdown-plan]
  (when-not (and (map? recovery-handle)
                 (fn? (:stop! recovery-handle))
                 (fn? (:status recovery-handle)))
    (reject! :invalid-recovery-handle))
  ;; Preflight the physical shutdown plan before starting side effects.
  (let [shutdown (http/prepare-managed-http-shutdown shutdown-plan)]
    {:status (fn [] {:recovery ((:status recovery-handle))
                     :shutdown ((:status shutdown))})
     :stop!
     (fn []
       (let [result ((:stop! recovery-handle)
                     (:drain-timeout-ms shutdown-plan))]
         (case (:outcome result)
           :drained ((:stop! shutdown))
           :timed-out {:outcome :recovery-timed-out}
           (reject! :invalid-recovery-drain-outcome))))}))
