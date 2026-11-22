(ns gesso.live.outbox.http-recovery
  "Host-driven, opt-in pacing for explicit cleanup of disconnected SSE sessions.

   This does not start a worker, timer, thread or retry loop. A trusted owner
   calls :step! from its own bounded scheduler, and retains responsibility for
   scheduling, logging errors, and eventual route/provider shutdown. The
   managed HTTP route remains the authority on physical source release and
   occupied capacity.")

(def ^:private default-min-interval-ms 1000)
(def ^:private default-max-attempts 16)

(defn- reject! [reason]
  (throw (ex-info "Invalid managed SSE recovery controller."
                  {:error/type :gesso.live.outbox/invalid-http-recovery
                   :reason reason})))

(defn prepare!
  "Create an explicit host-owned recovery controller for a managed SSE route.

   Options: :min-interval-ms (1..120000, default 1000), :max-attempts
   (1..1024, default 16), and optional trusted :now-ms monotonic clock.
   :now-ms exists for deterministic testing; production uses nanoTime.

   :step! attempts no cleanup if the route has no disconnected obligations,
   is already closed, or the minimum interval since the last attempted pass
   has not elapsed. It serializes concurrent calls to itself, and updates the
   clock fence BEFORE physical cleanup, including failures; otherwise repeated
   errors could create an unbounded hot retry loop. No scheduling is implicit.

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
                 (every? #{:min-interval-ms :max-attempts :now-ms}
                         (keys options)))
    (reject! :invalid-options))
  (let [min-interval-ms (get options :min-interval-ms default-min-interval-ms)
        max-attempts (get options :max-attempts default-max-attempts)
        now-ms (get options :now-ms
                    #(quot (System/nanoTime) 1000000))]
    (when-not (and (integer? min-interval-ms)
                   (<= 1 min-interval-ms 120000))
      (reject! :invalid-min-interval-ms))
    (when-not (and (integer? max-attempts)
                   (<= 1 max-attempts 1024))
      (reject! :invalid-max-attempts))
    (when-not (fn? now-ms)
      (reject! :invalid-clock))
    (let [state (atom {:last-attempt-ms nil
                       :attempt-count 0
                       :failure-count 0})
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
                 {:outcome :idle :pending 0}
                 (let [now (now-ms)
                       last-attempt (:last-attempt-ms @state)]
                   (when-not (integer? now)
                     (reject! :invalid-clock-result))
                   (when (and last-attempt (< now last-attempt))
                     (reject! :nonmonotonic-clock))
                   (let [elapsed (when last-attempt (- now last-attempt))]
                     (if (and elapsed (< elapsed min-interval-ms))
                       {:outcome :deferred
                        :pending pending
                        :retry-after-ms (- min-interval-ms elapsed)}
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
                             {:outcome :attempted
                              :attempted (:attempted result)
                              :recovered (:recovered result)
                              :remaining ((:disconnected-count route))})
                           (catch Throwable error
                             (swap! state update :failure-count inc)
                             (throw error))))))))))))
       :stats (fn [] @state)})))
