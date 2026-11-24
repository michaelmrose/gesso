(ns gesso.assurance.example.physical-single-node-aleph-writes
  "v2352: Observe the actual Aleph HTTP/1 final writes of the reservation
   fixture. Only its eager string responses establish local producer
   completion on Ring return. Streaming final-write disposition is recorded
   separately and NEVER establishes source or producer retirement.

   Own instrumented HTTP/1 channels and synchronous adapter invocations
   through lexical shutdown. Confirmed channel closure disposes remaining
   physical writes, but does NOT prove SSE producer quiescence or delivery.
   Unobserved responses and unmatched/orphaned capture invalidate complete
   final-frame pairing. This is not a general managed listener or release."
  (:require [gesso.live.outbox.http-aleph-pipeline :as pipeline]
            [gesso.live.outbox.http-netty-terminal :as terminal]
            [manifold.stream :as stream])
  (:import [io.netty.channel Channel ChannelFuture ChannelFutureListener ChannelPipeline]))

(defn- channel-owner []
  ;; The pipeline transform owns the actual child Channel, including idle
  ;; keep-alive connections. Close admission before snapshotting it. A new
  ;; channel racing the seal is closed and cannot enter the delegate.
  (let [guard (Object.)
        state (atom {:accepting? true :active 0 :channels #{}})
        invocations-drained (promise)
        channels-drained (promise)
        signal! (fn [{:keys [accepting? active channels]}]
                  (when-not accepting?
                    (when (zero? active) (deliver invocations-drained true))
                    (when (empty? channels) (deliver channels-drained true))))
        retire! (fn [^Channel channel]
                  ;; The close future, not a call to close(), proves closure.
                  (when (and (.isDone (.closeFuture channel))
                             (not (.isActive channel)))
                    (locking guard
                      (signal! (swap! state update :channels disj channel)))))]
    {:register! (fn [^Channel channel]
                  (let [admitted? (locking guard
                                    (when (:accepting? @state)
                                      (swap! state update :channels conj channel)
                                      true))]
                    (if admitted?
                      (.addListener (.closeFuture channel)
                                    (reify ChannelFutureListener
                                      (operationComplete [_ _] (retire! channel))))
                      (.close channel))))
     :invoke! (fn [handler request]
                (let [admitted? (locking guard
                                  (when (:accepting? @state)
                                    (swap! state update :active inc)
                                    true))]
                  (if admitted?
                    (try (handler request)
                         (finally (locking guard
                                    (signal! (swap! state update :active dec)))))
                    {:status 503 :headers {"content-type" "text/plain"}
                     :body "Transport admission closed"})))
     :retire! (fn []
                ;; Like the fixture's Ring and worker drains, this is lexical
                ;; ownership, not a bounded shutdown SLA. Never infer closure
                ;; from interruption or merely initiating a Netty close.
                (let [interrupted? (volatile! (Thread/interrupted))
                      await! (fn [p]
                               (loop []
                                 (when-not (try @p
                                                (catch InterruptedException _
                                                  (vreset! interrupted? true)
                                                  false))
                                   (recur))))]
                  (try
                    (locking guard (signal! (swap! state assoc :accepting? false)))
                    (await! invocations-drained)
                    (doseq [^Channel channel (:channels @state)]
                      (.close channel))
                    (await! channels-drained)
                    (finally
                      (when @interrupted? (.interrupt (Thread/currentThread)))))))
     :status (fn [] (let [{:keys [accepting? active channels]} @state]
                      {:accepting? accepting? :active active
                       :owned-channels (count channels)}))}))

(defn prepare! [handler]
  (when-not (fn? handler)
    (throw (ex-info "Original fixture handler required" {})))
  (let [owner (channel-owner)
        capture (pipeline/prepare!)
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
                ((:invoke! owner)
                 (fn [req]
                   (let [response (try (handler req)
                                       (catch Throwable error
                                         (swap! counts update :unobserved inc)
                                         (throw error)))]
                     (observe! req response)))
                 request))
     :http1-pipeline-transform
     (fn [^ChannelPipeline p]
       (let [result ((:http1-pipeline-transform capture) p)]
         ((:register! owner) (.channel p))
         result))
     :retire-channels! (:retire! owner)
     :channel-status (:status owner)
     :stats (fn [] (assoc @counts :capture ((:stats capture))))}))
