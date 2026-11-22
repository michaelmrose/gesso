(ns gesso.live.outbox.http-listener
  "Explicit physical HTTP dispatch ownership for single-node provider shutdown.

   A Ring handler can return before an adapter has written the response body.
   Therefore a Ring-only request counter is NOT evidence of physical listener
   quiescence. Mount :handle! at the HTTP adapter's *outer dispatch boundary*.
   A synchronous :dispatch! must run through the final server-side write;
   alternatively :dispatch-async! receives a terminal callback that must be
   invoked only after every asynchronous response write has ceased. In
   particular, a Ring return, successful Manifold put! or upstream SSE source
   close is NOT proof of downstream Netty writer completion. Both ordinary
   and SSE handlers must be covered. This guard does not claim TCP receipt
   or browser adoption and does not itself create a listener.")

(defn- reject! [reason]
  (throw (ex-info "Invalid owned HTTP listener drain."
                  {:error/type :gesso.live.outbox/invalid-http-listener
                   :reason reason})))

(defn prepare!
  "Prepare the adapter boundary before opening the physical HTTP listener.

   Input: {:dispatch! fn :on-rejected! fn :stop-accept! fn} for synchronous
   writers, OR {:dispatch-async! fn :on-rejected! fn :stop-accept! fn
                 :max-in-flight integer (optional)} for asynchronous writers.

   :dispatch! must synchronously perform the complete physical response
   write. :dispatch-async! receives (exchange terminal!) and may return
   before the response is written. The adapter must call terminal! with no
   arguments exactly when no physical writer or provider-using continuation
   remains for that exchange, whether successful, failed, disconnected, or
   canceled. Duplicates are harmless. A thrown asynchronous dispatch with
   no later terminal! is conservatively retained as an uncertain obligation;
   it MUST NOT be mistaken for a safely canceled writer. :uncertain-count
   reports such outstanding dispatches for diagnosis. An asynchronous owner
   limits admissions to :max-in-flight (1..65536, default 4096), with overload
   and closed-listener requests going to :on-rejected!, which must not use
   host providers. No Ring-return, stream-close, or Manifold put! event may
   be substituted for the physical completion callback.

   :stop-accept! is the actual listener owner's idempotent socket shutdown;
   it must return exactly {:outcome :stopped} once new physical admissions
   have ceased. A failed/unconfirmed result permanently marks stop uncertain
   and prohibits declaring provider-safe drain. External listeners must not
   be stopped outside this owner while its host providers remain in use.

   :handle! admits work atomically against :stop!. Synchronous dispatches
   retire in finally; asynchronous dispatches retain ownership until their
   physical-terminal callback fires, even after :handle! returns.
   :stop! closes admission, invokes physical stop-accept! at most once and
   waits at most timeout-ms (0..120000) for all previously admitted writes.
   Returns {:outcome :drained} or {:outcome :timed-out}. A timeout is
   retryable without stopping the socket again. The timeout does not bound
   :stop-accept! itself, which must have its own operational termination
   guarantee. Errors from physical dispatch are preserved, not interpreted
   as delivery success. Use :stop! as :stop-listener! in the managed HTTP
   shutdown plan with the same timeout, and never upgrade a timeout to drain."
  [{:keys [dispatch! dispatch-async! on-rejected! stop-accept!
           max-in-flight] :as options}]
  (let [async? (contains? options :dispatch-async!)
        required (if async?
                   #{:dispatch-async! :on-rejected! :stop-accept!}
                   #{:dispatch! :on-rejected! :stop-accept!})
        allowed (if async? (conj required :max-in-flight) required)
        limit (if async? (get options :max-in-flight 4096) nil)]
    (when-not (and (map? options)
                   (every? allowed (keys options))
                   (every? #(contains? options %) required)
                   (fn? (if async? dispatch-async! dispatch!))
                   (fn? on-rejected!) (fn? stop-accept!))
      (reject! :invalid-listener-plan))
    (when (and async?
               (not (and (integer? limit) (<= 1 limit 65536))))
      (reject! :invalid-in-flight-limit))
  (let [monitor (Object.)
        stop-monitor (Object.)
        state (atom {:phase :open :active 0 :uncertain 0 :capacity-rejections 0})
        drain (promise)]
    (letfn [(drained? [] (let [{:keys [phase active]} @state]
                         (and (= phase :stopped) (zero? active))))
            (complete! []
              (locking monitor
                (swap! state update :active dec)
                (when (drained?) (deliver drain true))))
            (stop! [timeout-ms]
              (when-not (and (integer? timeout-ms)
                             (<= 0 timeout-ms 120000))
                (reject! :invalid-listener-drain-timeout))
              (locking stop-monitor
                (let [phase (locking monitor
                              (let [current (:phase @state)]
                                (when (= :open current)
                                  (swap! state assoc :phase :stopping))
                                current))]
                  (when (= :stop-unknown phase)
                    (reject! :physical-listener-stop-unknown))
                  (when (= :open phase)
                    (try
                      (let [result (stop-accept!)]
                        (if (= {:outcome :stopped} result)
                          (locking monitor
                            (swap! state assoc :phase :stopped)
                            (when (drained?) (deliver drain true)))
                          (do
                            (locking monitor
                              (swap! state assoc :phase :stop-unknown))
                            (reject! :unconfirmed-physical-listener-stop))))
                      (catch Throwable error
                        (locking monitor
                          (swap! state assoc :phase :stop-unknown))
                        (when (instance? InterruptedException error)
                          (.interrupt (Thread/currentThread)))
                        (throw error))))
                  {:outcome (if (or (drained?)
                                    (true? (deref drain timeout-ms false)))
                              :drained :timed-out)})))]
      {:handle!
       (fn [exchange]
         (let [admitted? (locking monitor
                           (let [{:keys [phase active]} @state]
                             (cond
                               (not= :open phase) false
                               (and async? (>= active limit))
                               (do (swap! state update :capacity-rejections inc)
                                   false)
                               :else
                               (do (swap! state update :active inc) true))))]
           (if admitted?
             (if async?
               ;; No handler-return-based retirement for asynchronous
               ;; adapters. Only the adapter's physical terminal signal
               ;; may release this provider-using exchange.
               (let [done? (atom false)
                     uncertain? (atom false)
                     terminal! (fn []
                                 (locking monitor
                                   (when-not @done?
                                     (reset! done? true)
                                     (when @uncertain?
                                       (swap! state update :uncertain dec))
                                     (complete!))))]
                 (try
                   (dispatch-async! exchange terminal!)
                   (catch Throwable error
                     ;; The dispatcher may have launched an async writer
                     ;; before throwing. Retain ownership, not an invented
                     ;; cancellation, until its terminal callback arrives.
                     (locking monitor
                       (when-not @done?
                         (reset! uncertain? true)
                         (swap! state update :uncertain inc)))
                     (throw error))))
               (try (dispatch! exchange)
                    (finally (complete!))))
             (on-rejected! exchange))))
       :stop! stop!
       :active-count (fn [] (:active @state))
       :uncertain-count (fn [] (:uncertain @state))
       :capacity-rejection-count (fn [] (:capacity-rejections @state))
       :drained? drained?
       :status (fn [] (:phase @state))}))))
