(ns gesso.live.outbox.http-netty-terminal
  "Netty response-write disposition to physical-listener ownership bridge.

   Aleph 0.9.11 HTTP/1 send-message and HTTP/2 send-message internally
   obtain Netty write futures, but the public Ring handler return is not
   that future. This is an opt-in adapter primitive for use AT those internal
   completion sites; it does NOT patch or automatically instrument Aleph.

   A successful ChannelFuture proves only that the final write was accepted
   by Netty's outbound pipeline, not TCP delivery, browser adoption, or that
   the response's producer/continuations are finished. The listener may
   release provider ownership only once BOTH the final outbound write is
   disposed (or its cancellation is confirmed) AND every provider-using
   producer/continuation has ceased. On an absent or failed signal, retain
   ownership and expose uncertainty rather than guessing."
  (:import [io.netty.channel ChannelFuture ChannelFutureListener]))

(defn- reject! [reason]
  (throw (ex-info "Invalid Netty HTTP terminal evidence."
                  {:error/type :gesso.live.outbox/invalid-netty-terminal
                   :reason reason})))

(defn prepare!
  "Return evidence callbacks for a single asynchronous HTTP response.

   The owning adapter calls :watch-final-write! with the actual ChannelFuture
   returned by the LAST response write (HTTP/1 LastHttpContent or HTTP/2
   endStream frame). A completed or failed *earlier* chunk is not sufficient.
   The callback attaches a Netty listener for the future's terminal outcome.
   :producer-ceased! must be called only once all source reads, callbacks,
   channel-close cleanup, and provider-using continuations are quiescent;
   upstream source close alone is not evidence. On a path without a final
   write future, :writer-aborted! is permitted ONLY after the adapter has
   proved all outstanding writes are cancelled or ceased and the channel
   cannot resume the response. Both write disposition and producer
   quiescence are required regardless of success/failure/cancellation.

   The supplied terminal! should be the v2247 listener's per-exchange
   callback. It fires at most once, outside internal locks. The status map
   is diagnostic and distinguishes success, write failure and abort but none
   implies a browser received the response.

   Duplicate evidence is idempotent. Invalid inputs and attempts to
   register a second final future are typed refusals, without releasing
   ownership. A failed listener registration remains unresolved/uncertain."
  [terminal!]
  (when-not (fn? terminal!) (reject! :invalid-terminal-callback))
  (let [guard (Object.)
        state (atom {:producer-ceased? false
                     :write-outcome :unregistered
                     :future-registered? false
                     :registration-uncertain? false
                     :terminal? false
                     :cause nil})]
    (letfn [(advance! [change]
              (let [send? (locking guard
                            (swap! state change)
                            (when (and (not (:terminal? @state))
                                       (:producer-ceased? @state)
                                       (#{:succeeded :failed :aborted}
                                        (:write-outcome @state)))
                              (swap! state assoc :terminal? true)
                              true))]
                (when send? (terminal!))))
            (write-ended! [^ChannelFuture future]
              (advance!
               (fn [s]
                 (if (#{:succeeded :failed :aborted} (:write-outcome s))
                   s
                   (assoc s
                          :write-outcome (if (.isSuccess future)
                                           :succeeded :failed)
                          :cause (when-not (.isSuccess future)
                                   (.cause future)))))))]
      {:watch-final-write!
       (fn [future]
         (when-not (instance? ChannelFuture future)
           (reject! :invalid-final-write-future))
         (locking guard
           (when (:future-registered? @state)
             (reject! :duplicate-final-write-future))
           (when (= :aborted (:write-outcome @state))
             (reject! :final-write-after-abort))
           (swap! state assoc :future-registered? true
                  :write-outcome :pending))
         (try
           (.addListener ^ChannelFuture future
                         (reify ChannelFutureListener
                           (operationComplete [_ completed]
                             (write-ended! completed))))
           (catch Throwable error
             ;; No synthetic release: this ChannelFuture may already have
             ;; side effects, but its terminal notification wasn't attached.
             (locking guard
               (swap! state assoc :registration-uncertain? true))
             (throw error)))
         :watching)
       :producer-ceased!
       (fn [] (advance! #(assoc % :producer-ceased? true)))
       :writer-aborted!
       (fn [] (advance!
               (fn [s]
                 (if (#{:succeeded :failed :aborted} (:write-outcome s))
                   s
                   (assoc s :write-outcome :aborted)))))
       :status (fn [] @state)})))
