(ns gesso.assurance.example.physical-single-node-aleph-writes
  "v2351: Observe the actual Aleph HTTP/1 final writes of the reservation
   fixture. Only its eager string responses establish local producer
   completion on Ring return. Streaming final-write disposition is recorded
   separately and NEVER establishes source or producer retirement.

   This is diagnostic instrumentation, not a physical listener owner, a
   transport drain, TCP receipt, or release evidence. Unobserved responses,
   handler errors and unmatched/orphaned capture invalidate complete pairing."
  (:require [gesso.live.outbox.http-aleph-pipeline :as pipeline]
            [gesso.live.outbox.http-netty-terminal :as terminal]
            [manifold.stream :as stream])
  (:import [io.netty.channel Channel ChannelFuture ChannelFutureListener]))

(defn prepare! [handler]
  (when-not (fn? handler)
    (throw (ex-info "Original fixture handler required" {})))
  (let [capture (pipeline/prepare!)
        counts (atom {:finite-registered 0 :finite-terminal 0
                      :finite-succeeded 0 :finite-failed 0
                      :stream-registered 0 :stream-final-succeeded 0
                      :stream-final-failed 0 :unobserved 0})
        observe!
        (fn [request response]
          (let [channel (:aleph/channel request)
                body (:body response)]
            (try
              (when-not (and (instance? Channel channel)
                             (.get (.pipeline ^Channel channel) "gesso-final-http1"))
                (throw (ex-info "Missing physical response capture" {})))
              (cond
                (and (map? response) (integer? (:status response)) (string? body))
                (let [bridge* (atom nil)
                      bridge (terminal/prepare!
                              (fn []
                                (let [outcome (:write-outcome ((:status @bridge*)))]
                                  (swap! counts (fn [s]
                                                  (-> s (update :finite-terminal inc)
                                                      (update (case outcome
                                                                :succeeded :finite-succeeded
                                                                :failed :finite-failed) inc)))))))]
                  (reset! bridge* bridge)
                  ((:register-response! capture) channel bridge)
                  (swap! counts update :finite-registered inc)
                  ;; Only this fixture's completely rendered string responses
                  ;; have no provider-using response producer after return.
                  ((:producer-ceased! bridge)))

                (and (map? response) (integer? (:status response)) (stream/stream? body))
                (do
                  ((:register-response! capture) channel
                   {:watch-final-write!
                    (fn [^ChannelFuture future]
                      (try
                        (.addListener future
                                      (reify ChannelFutureListener
                                        (operationComplete [_ completed]
                                          (swap! counts update
                                                 (if (.isSuccess ^ChannelFuture completed)
                                                   :stream-final-succeeded
                                                   :stream-final-failed) inc))))
                        (catch Exception _
                          (swap! counts update :unobserved inc))))})
                  (swap! counts update :stream-registered inc))

                :else (swap! counts update :unobserved inc))
              (catch Exception _
                ;; Observability failure must not change a completed command
                ;; into a failed response. Pairing is now incomplete.
                (swap! counts update :unobserved inc)))
            response))]
    {:handler (fn [request]
                (let [response (try (handler request)
                                    (catch Throwable error
                                      (swap! counts update :unobserved inc)
                                      (throw error)))]
                  (observe! request response)))
     :http1-pipeline-transform (:http1-pipeline-transform capture)
     :stats (fn [] (assoc @counts :capture ((:stats capture))))}))
