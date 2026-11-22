(ns gesso.live.outbox.http-aleph-live
  "Fail-closed physical-disconnect handoff for a real source-backed Gesso Live
   SSE session, an Aleph Netty Channel, and the v2248 terminal bridge.

   This does NOT own HTTP request authentication, the route's capacity ledger,
   or a production Aleph listener. Bind only an already-authorized session
   returned by gesso.live.core/start-sse!. Channel close is evidence that a
   response cannot resume writing, NOT evidence that the Live source or the
   SSE producer has stopped. Dispatch cancellation off the Netty event loop.
   Failure or rejected scheduling retains a retryable ownership obligation.
   The separate :reconcile! callback never repeats cancellation: it can
   discharge that obligation only after independent source, response, and
   (when supplied) managed-route ledger release are all confirmed.

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
   a host owner after a failed callback/overloaded executor. :reconcile!
   performs ONLY evidence checking after an external route-owned recovery
   pass, never calls the route's :cancel! again. For a managed route,
   :route-released? MUST establish that its session slot is no longer held;
   source-close alone can precede the end of the cancellation callback.
   :reconcile! returns :released or :waiting-* and never upgrades a pending
   route slot to terminal evidence. :status exposes unresolved
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
                 (or (not (contains? session :route-released?))
                     (fn? (:route-released? session)))
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
    (letfn [(channel-closed? []
              (and (.isDone close-future)
                   (not (.isActive ^Channel channel))))
            (reconcile! []
              ;; Evidence-only. In particular this cannot re-enter an
              ;; already retired route or its XTDB/Live providers.
              (locking guard
                (cond
                  (:released? @state) :released
                  (not (channel-closed?)) :waiting-channel
                  (not (stream/closed? (:stream session))) :waiting-response
                  (not (true? ((:source-closed? session)))) :waiting-source
                  (and (:route-released? session)
                       (not (true? ((:route-released? session)))))
                  :waiting-route
                  :else
                  (try
                    ;; Channel close proves output cannot resume. The
                    ;; response, eager source tap and route ledger were
                    ;; independently verified before terminal admission.
                    ((:writer-aborted! bridge))
                    ((:producer-ceased! bridge))
                    (swap! state assoc :released? true :last-error nil)
                    :released
                    (catch Throwable error
                      (when (instance? InterruptedException error)
                        (.interrupt (Thread/currentThread)))
                      (swap! state assoc :last-error error)
                      (throw error))))))
            (release! []
              (locking guard
                (cond
                  (:released? @state) :released
                  (not (channel-closed?))
                  (invalid! :channel-still-active)
                  :else
                  (do
                    (swap! state update :release-attempts inc)
                    (try
                      ;; Cancellation is route-mediated, not an independent
                      ;; Aleph physical-release path. Reconciliation checks
                      ;; the route's full ledger AFTER cancellation returns.
                      (live/cancel-sse! session)
                      (case (reconcile!)
                        :released :released
                        :waiting-response (invalid! :response-not-closed)
                        :waiting-source (invalid! :source-not-released)
                        :waiting-route (invalid! :route-not-released)
                        (invalid! :incomplete-release))
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
       :reconcile! reconcile!
       :status (fn [] (assoc @state :channel-closed? (channel-closed?)))})))

(defn prepare-reconciler!
  "Prepare bounded host-owned reconciliation of Aleph terminal obligations.

   The caller installs each attach! result with :register! and passes
   :after-step! to http-recovery/start-scheduled! as the scheduler's
   :after-step! option. That callback runs AFTER each route-owned recovery
   step, even after a failed or idle step, so actual physical route release
   and the independent Netty terminal obligation can converge without a
   manual per-session :retry!. :after-step! only checks evidence; it does
   NOT invoke the session's cancel! or mutate route capacity.

   Limit each pass to :max-per-step (1..64, default 16) and registrations
   to :max-pending (1..4096, default 1024). A rotating cursor ensures an
   unreleased earlier session cannot starve later completions. A callback
   error leaves its hook registered and records bounded diagnostics. The
   scheduler's own stop/drain barrier also fences all these callbacks.
   The host MUST register a hook before the Ring response is returned and
   MUST not shut down providers until both the route and listener are drained."
  ([] (prepare-reconciler! {}))
  ([{:keys [max-pending max-per-step] :as options}]
   (when-not (and (map? options)
                  (every? #{:max-pending :max-per-step} (keys options)))
     (invalid! :invalid-reconciler-options))
   (let [max-pending (get options :max-pending 1024)
         max-per-step (get options :max-per-step 16)]
     (when-not (and (integer? max-pending) (<= 1 max-pending 4096)
                    (integer? max-per-step) (<= 1 max-per-step 64))
       (invalid! :invalid-reconciler-limit))
     (let [monitor (Object.)
           state (atom {:entries {} :cursor nil :attempts 0
                        :reconciled 0 :errors 0 :last-error nil})]
       {:register!
        (fn [hook]
          (when-not (and (map? hook)
                         (fn? (:reconcile! hook)) (fn? (:status hook)))
            (invalid! :invalid-reconciliation-hook))
          (locking monitor
            (when (>= (count (:entries @state)) max-pending)
              (invalid! :reconciler-capacity))
            (let [id (random-uuid)]
              (swap! state assoc-in [:entries id] hook)
              id)))
        :after-step!
        (fn [_route-step]
          (locking monitor
            (let [{:keys [entries cursor]} @state
                  ordered (sort-by (comp str key) entries)
                  [before after]
                  (if cursor
                    (split-with #(not (pos? (compare (str (key %)) cursor)))
                                ordered)
                    [[] ordered])
                  selected (vec (take max-per-step (concat after before)))
                  _ (when-let [[id _] (peek selected)]
                      (swap! state assoc :cursor (str id)))
                  completed
                  (reduce
                   (fn [count-completed [id hook]]
                     (swap! state update :attempts inc)
                     (try
                       (let [outcome ((:reconcile! hook))]
                         (when-not (#{:released :waiting-channel
                                      :waiting-response :waiting-source
                                      :waiting-route} outcome)
                           (invalid! :invalid-reconciliation-result))
                         (if (= :released outcome)
                           (do (swap! state
                                      (fn [s] (-> s
                                                  (update :entries dissoc id)
                                                  (update :reconciled inc))))
                               (inc count-completed))
                           count-completed))
                       (catch Throwable error
                         ;; One bad completion must not starve other hooks
                         ;; or suppress the scheduler's future retry ticks.
                         (swap! state (fn [s]
                                        (-> s (update :errors inc)
                                            (assoc :last-error error))))
                         (when (instance? InterruptedException error)
                           (.interrupt (Thread/currentThread)))
                         count-completed)))
                   0 selected)]
              {:attempted (count selected)
               :reconciled completed
               :remaining (count (:entries @state))})))
        :pending-count (fn [] (count (:entries @state)))
        :stats (fn [] (let [s @state]
                        (-> (dissoc s :entries :cursor)
                            (assoc :pending (count (:entries s))))))}))))
