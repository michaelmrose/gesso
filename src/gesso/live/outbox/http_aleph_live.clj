(ns gesso.live.outbox.http-aleph-live
  "Fail-closed physical-disconnect handoff for a real source-backed Gesso Live
   SSE session, an Aleph Netty Channel, and the v2248 terminal bridge.

   This does NOT own HTTP request authentication, the route's capacity ledger,
   or a production Aleph listener. Bind only an already-authorized session
   returned by gesso.live.core/start-sse!. Channel close is evidence that a
   response cannot resume writing, NOT evidence that the Live source or the
   SSE producer has stopped. Dispatch cancellation off the Netty event loop.
   Failure or rejected scheduling retains a retryable ownership obligation.

   A successfully returned session cancel! and independently closed response
   stream and eager source tap are the explicit local release evidence. This
   does not prove arbitrary application-owned asynchronous continuations have
   finished: those must be separately accounted for by their owner."
  (:require [gesso.live.core :as live]
            [manifold.stream :as stream])
  (:import [io.netty.channel Channel ChannelFutureListener]
           [java.util.concurrent Executor]))

(defn- invalid! [reason]
  (throw (ex-info "Invalid Aleph Live disconnect ownership."
                  {:error/type :gesso.live.outbox/invalid-aleph-live
                   :reason reason})))

(defn attach!
  "Register physical disconnect cleanup for a source-backed SSE session.

   Required map keys:
     :channel    the actual Aleph request's :aleph/channel (Netty Channel)
     :session    the live/start-sse! result, with cancel!, stream, response,
                 source-closed? and its eager subscription release handle
     :bridge     the request's prepared http-netty-terminal evidence bridge
     :executor   a host-owned bounded Executor; the Netty event loop MUST NOT
                 perform potentially blocking cancellation itself

   Once the channel closeFuture completes, execute the session's real cancel!
   off-loop, verify both the response body and eager source tap are closed,
   certify irreversible writer abort and then producer quiescence. The bridge
   requires both signals, so incomplete release must not drain its listener.
   The returned :retry! is legal only after channel closure and may be used by
   a host owner after a failed callback/overloaded executor. It never claims
   cancellation merely because a stream closed. :status exposes unresolved
   cancellation and scheduling errors; no automatic infinite retries.

   The result is *per SSE session*. A surrounding HTTP route must independently
   retain its own failed physical-release obligations and perform ordered
   shutdown; the bridge does not bypass that route's capacity ledger."
  [{:keys [channel session bridge executor] :as plan}]
  (when-not (and (map? plan)
                 (= #{:channel :session :bridge :executor} (set (keys plan)))
                 (instance? Channel channel)
                 (instance? Executor executor)
                 (map? session)
                 (fn? (:cancel! session))
                 (fn? (:source-closed? session))
                 (stream/stream? (:stream session))
                 (identical? (:stream session)
                             (get-in session [:response :body]))
                 (map? bridge)
                 (fn? (:writer-aborted! bridge))
                 (fn? (:producer-ceased! bridge)))
    (invalid! :invalid-disconnect-plan))
  (let [guard (Object.)
        state (atom {:release-attempts 0 :released? false
                     :scheduled? false :last-error nil})
        close-future (.closeFuture ^Channel channel)]
    (letfn [(release! []
              (locking guard
                (cond
                  (:released? @state) :released
                  (not (and (.isDone close-future)
                            (not (.isActive ^Channel channel))))
                  (invalid! :channel-still-active)
                  :else
                  (do
                    (swap! state update :release-attempts inc)
                    (try
                      ;; This is the SAME physical cancellation and eager
                      ;; source-release callback used by the managed route.
                      (live/cancel-sse! session)
                      (when-not (stream/closed? (:stream session))
                        (invalid! :response-not-closed))
                      (when-not (true? ((:source-closed? session)))
                        (invalid! :source-not-released))
                      ;; After the close future, no write can resume on this
                      ;; channel. Any already-observed final write disposition
                      ;; is preserved by the bridge's idempotent callback.
                      ((:writer-aborted! bridge))
                      ((:producer-ceased! bridge))
                      (swap! state assoc :released? true :last-error nil)
                      :released
                      (catch Throwable error
                        (when (instance? InterruptedException error)
                          (.interrupt (Thread/currentThread)))
                        (swap! state assoc :last-error error)
                        (throw error)))))))
            (schedule! []
              ;; Netty callback does only this bounded admission; it NEVER
              ;; blocks inside a source cancellation or takes the release lock.
              (swap! state assoc :scheduled? true)
              (try
                (.execute ^Executor executor
                          ^Runnable (fn []
                                      (try (release!)
                                           ;; Failure remains recorded for
                                           ;; explicit host-owned retry.
                                           (catch Throwable _ nil))))
                (catch Throwable error
                  (when (instance? InterruptedException error)
                    (.interrupt (Thread/currentThread)))
                  (swap! state assoc :last-error error))))]
      (.addListener close-future
                    (reify ChannelFutureListener
                      (operationComplete [_ _] (schedule!))))
      {:retry! release!
       :status (fn [] (assoc @state :channel-closed?
                             (and (.isDone close-future)
                                  (not (.isActive ^Channel channel)))))})))
