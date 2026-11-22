(ns gesso.live.outbox.http-listener
  "Explicit physical HTTP dispatch ownership for single-node provider shutdown.

   A Ring handler can return before an adapter has written the response body.
   Therefore a Ring-only request counter is NOT evidence of physical listener
   quiescence. Mount :handle! at the HTTP adapter's *outer dispatch boundary*:
   its :dispatch! callback must run through the final server-side response
   write (or its failure) before returning. Both ordinary and SSE handlers
   must be covered. This guard does not claim TCP acknowledgement or browser
   adoption and does not itself create a listener.")

(defn- reject! [reason]
  (throw (ex-info "Invalid owned HTTP listener drain."
                  {:error/type :gesso.live.outbox/invalid-http-listener
                   :reason reason})))

(defn prepare!
  "Prepare the adapter boundary before opening the physical HTTP listener.

   Input: {:dispatch! fn :on-rejected! fn :stop-accept! fn}.
   :dispatch! must take one accepted request/exchange AND perform the whole
   physical server-side response write before it returns. Merely returning a
   Ring response is insufficient. :on-rejected! handles an exchange rejected
   after listener shutdown; it MUST NOT invoke host providers.

   :stop-accept! is the actual listener owner's idempotent socket shutdown;
   it must return exactly {:outcome :stopped} once new physical admissions
   have ceased. A failed/unconfirmed result permanently marks stop uncertain
   and prohibits declaring provider-safe drain. External listeners must not
   be stopped outside this owner while its host providers remain in use.

   :handle! admits work atomically against :stop!, tracks until :dispatch!
   has fully returned or thrown, and always retires the request in finally.
   :stop! closes admission, invokes physical stop-accept! at most once and
   waits at most timeout-ms (0..120000) for all previously admitted writes.
   Returns {:outcome :drained} or {:outcome :timed-out}. A timeout is
   retryable without stopping the socket again. The timeout does not bound
   :stop-accept! itself, which must have its own operational termination
   guarantee. Errors from physical dispatch are preserved, not interpreted
   as delivery success. Use :stop! as :stop-listener! in the managed HTTP
   shutdown plan with the same timeout, and never upgrade a timeout to drain."
  [{:keys [dispatch! on-rejected! stop-accept!] :as options}]
  (when-not (and (map? options)
                 (= #{:dispatch! :on-rejected! :stop-accept!}
                    (set (keys options)))
                 (fn? dispatch!) (fn? on-rejected!) (fn? stop-accept!))
    (reject! :invalid-listener-plan))
  (let [monitor (Object.)
        stop-monitor (Object.)
        state (atom {:phase :open :active 0})
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
                           (if (= :open (:phase @state))
                             (do (swap! state update :active inc) true)
                             false))]
           (if admitted?
             (try (dispatch! exchange)
                  (finally (complete!)))
             (on-rejected! exchange))))
       :stop! stop!
       :active-count (fn [] (:active @state))
       :drained? drained?
       :status (fn [] (:phase @state))})))
