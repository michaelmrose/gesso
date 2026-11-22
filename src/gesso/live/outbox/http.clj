(ns gesso.live.outbox.http
  "Opt-in authorized Ring SSE route for a running concrete Gesso/Biff host.

   This adapter does not start a network listener. Mount its :handler into a
   trusted Ring server; serve only behind the application's authentication,
   CSRF/session and authorization boundaries. In particular, a query parameter
   is NEVER considered sufficient authority to subscribe to a Live scope.
   The host supplies :authorize-subscription, which must return an admitted
   {:topic keyword :id scalar} scope, or nil to deny. A malformed scope fails
   closed instead of installing a broad source interest.

   The adapter delegates frames to Gesso's existing Missionary/Manifold SSE
   transport. Its own :close! retires every currently registered SSE producer
   before the caller stops the host's Live and XTDB providers. The returned
   Ring response and source acceptance prove neither TCP receipt nor browser
   refresh; those require separate client-side evidence."
  (:require [clojure.string :as str]
            [gesso.live.core :as live]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.transport.sse :as sse]
            [manifold.stream :as stream]))

(defn- reject! [reason data]
  (throw (ex-info "Invalid managed Live HTTP/SSE route."
                  (assoc data :error/type :gesso.live.outbox/invalid-http-route
                         :reason reason))))

(def ^:private default-max-connections 1024)
(def ^:private default-managed-keepalive-ms 15000)
(def ^:private default-max-pending-authorizations 1024)

(defn- cancel-physical-sse! [session]
  ;; A transport cancellation handle may already have been consumed by a
  ;; failing first attempt. A subsequent no-op must not be mistaken for
  ;; physical stream cleanup and grant permission to stop Live providers.
  (live/cancel-sse! session)
  (when-not (stream/closed? (get-in session [:response :body]))
    (reject! :incomplete-sse-cancellation {}))
  ;; The transport owns an eager Live source subscription as well as the
  ;; response body. Stream closure is necessary but not sufficient: a failed
  ;; release hook must not permit the HTTP owner to stop Live/XTDB.
  (when-let [source-closed? (:source-closed? session)]
    (when-not (true? (source-closed?))
      (reject! :incomplete-sse-source-release {}))))

(defn- occupied-sse-slots
  "Count starting reservations and retained sessions in constant time.

   The occupied-session set is maintained at each admission and cleanup
   transition. Building a union of the full connection/retry registries on
   every GET would allocate and scan O(active connections) entries even when
   all other admission decisions are constant-time."
  [{:keys [starting occupied-session-ids]}]
  (+ starting (count occupied-session-ids)))

(defn- retire-untracked-session [state key]
  ;; Cleanup callbacks may arrive before or during cancellation. Retire an
  ;; occupied slot only after NO owner of that session remains, including a
  ;; cancellation that is still executing after its close hooks have fired.
  (if (or (contains? (:connections state) key)
          (contains? (:retry-cancellations state) key)
          (contains? (:disconnect-cleanups state) key)
          (contains? (:cancelling-session-ids state) key))
    state
    (update state :occupied-session-ids disj key)))

(defn- cancel-registered-sse! [state key session]
  ;; Hold the capacity slot while cancel! executes: the physical response and
  ;; source tap may close *before* the cancel callback returns or throws.
  (swap! state update :cancelling-session-ids conj key)
  (try
    (cancel-physical-sse! session)
    (swap! state
           (fn [s]
             (-> s
                 (update :connections dissoc key)
                 (update :retry-cancellations dissoc key)
                 (update :disconnect-cleanups dissoc key)
                 (update :cancelling-session-ids disj key)
                 (retire-untracked-session key))))
    (catch Throwable e
      ;; The stream-close callback may already have removed the connection.
      ;; Preserve a durable retry and keep this session's occupied slot.
      (swap! state
             (fn [s]
               (-> s
                   (assoc-in [:retry-cancellations key] session)
                   (update :cancelling-session-ids disj key))))
      (throw e))))

(defn- drained-state? [{:keys [closed? close-completed? connections starting
                              retry-cancellations disconnect-cleanups
                              cancelling-session-ids occupied-session-ids
                              pending-authorizations]}]
  ;; A disconnect hook can remove the last connection while close! is still
  ;; inside live/cancel-sse!. Do not declare provider-safe quiescence until
  ;; the ordered route cancellation pass itself has returned successfully.
  (and closed? close-completed?
       (zero? pending-authorizations)
       (zero? starting)
       (empty? connections) (empty? retry-cancellations)
       (empty? disconnect-cleanups)
       (empty? cancelling-session-ids)
       (empty? occupied-session-ids)))

(defn- reserve-pending-authorization! [state live-system limit]
  ;; Session construction and close! hold the state monitor. A fresh GET must
  ;; still reject overload promptly while that monitor is occupied. CAS is
  ;; deliberately separate from locking; the state atom serializes changes
  ;; against both route closure and release of other pending requests.
  (loop []
    (let [snapshot @state]
      (cond
        (or (:closed? snapshot) (live/closed? live-system))
        {:status 503 :headers {} :body "Live shutting down"}

        (>= (:pending-authorizations snapshot) limit)
        {:status 503 :headers {}
         :body "Live authorization capacity reached"}

        (compare-and-set! state snapshot
                          (update snapshot :pending-authorizations inc))
        nil

        :else (recur)))))

(def ^:private reserved-sse-response-headers
  ;; The managed route owns stream framing, representation metadata, cache
  ;; safety and proxy buffering. The transport emits uncompressed SSE text:
  ;; a user-supplied Content-Encoding would misdescribe those bytes, while
  ;; Content-Range would incorrectly describe a partial streamed response.
  ;; Case variants must not bypass these restrictions.
  #{"cache-control" "connection" "content-encoding" "content-range"
    "keep-alive" "proxy-connection" "te" "trailer"
    "transfer-encoding" "upgrade" "x-accel-buffering"})

(defn- valid-scope? [subscription]
  (and (map? subscription)
       (= #{:topic :id} (set (keys subscription)))
       (keyword? (:topic subscription))
       (or (string? (:id subscription))
           (keyword? (:id subscription))
           (integer? (:id subscription))
           (uuid? (:id subscription)))
       (not (and (string? (:id subscription))
                 (str/blank? (:id subscription))))))

(defn wrap-authorized-sse
  "Return {:handler Ring-handler :close! callback :active-count callback
           :occupied-count callback :pending-authorization-count callback :closed? callback
           :drained? callback :await-drained! callback}.

   Requires a started concrete host, a fixed canonical :path (absolute URL
   path with no query string, repeated slash, or dot traversal segment), and
   a trusted :authorize-subscription callback. Authorization is bounded by
   :max-pending-authorizations (1024 by default). The managed route also
   enables SSE keepalive comments every 15 seconds by default, allowing an
   HTTP writer to observe disconnected idle clients and reclaim slots when
   its adapter closes the stream. A caller may override :keepalive-ms in
   :sse-options, including explicitly nil to opt out when its HTTP adapter
   supplies equivalent disconnect detection. The reservation covers
   both the application callback and any wait for session admission, so a
   stalled SSE startup cannot queue unlimited GETs. Excess requests receive
   HTTP 503 without invoking the callback. The callback authenticates the
   Ring request and decides whether the requester may subscribe to the exact
   returned scope. Nil denies with 403. Unknown paths are passed to the
   existing canonical Ring handler.
   Non-GET requests to the SSE path receive 405. Managed EventSource routes
   require an HTTP 200 response status: another accepted transport status
   would register a Live interest but cannot establish EventSource delivery.
   A fixed Content-Length is also forbidden: EventSource is an indefinite
   streamed body, whose framing must be managed by the HTTP server itself.
   Other server-owned framing, body-encoding/range metadata and managed
   cache/proxy-buffering headers cannot be overridden by the application,
   including case variants. Managed SSE emits uncompressed full text frames;
   any actual on-wire compression is the HTTP server's responsibility.
   Reject these misconfigurations before authorization or subscription.
   Invalid callback results and callback exceptions propagate before a
   subscription can be installed. Once closed, the managed GET route
   returns 503 without consulting authorization or registering interests;
   the inner locked check still prevents admission when shutdown races an
   authorization already in progress. Shutdown also takes precedence over
   a late denial, malformed scope, or ordinary callback exception: all yield
   503 once closed, without revealing provider failure during teardown.
   Exceptions while the route is open retain their original identity.
   If an authorization callback raises, its pending slot is released
   before the exception propagates. A pending callback cannot install a
   session after the route closes. Starting SSE streams happens *outside*
   the admission lock, but reserves one active-connection slot first. This
   lets :close! retire registered streams without waiting for a stalled
   startup; a late starter must cancel its stream at the final admission
   fence and cannot return HTTP 200 after shutdown. A starting session is
   charged against :max-connections before its source interest exists.
   Unresolved disconnect and failed-cancellation cleanup obligations also
   retain capacity until source release is confirmed, even when the HTTP
   response is already closed. :active-count reports current HTTP sessions;
   :occupied-count additionally includes in-flight starters, cancellation
   operations, and unreleased cleanup obligations (counting each session once).
   Slot accounting is constant-time regardless of how many active SSE sessions
   are registered. A slot remains occupied until all session cleanup owners
   have relinquished it.
   Since :close! can return while authorization/startup is still in flight,
   :drained? reports the stronger lifecycle condition: route close! has
   completed its cancellation pass successfully, no authorization or startup
   is outstanding, and neither a session nor a failed-cancellation retry
   remains registered. For source-backed SSE the cancellation handoff must
   also confirm the eager Live source interest was physically released. A
   stream disconnect callback can remove a connection before cancellation
   finishes; a cancellation that then throws keeps a retry obligation.
   Client-initiated disconnects also retain unresolved source-release work
   until the source interest closes or a close! retry succeeds.
   A completed :close! alone does not establish full quiescence.
   :await-drained! takes a bounded timeout in milliseconds (0..120000),
   waits for the same terminal state without polling, and returns either
   {:outcome :drained} or {:outcome :timed-out}. It does NOT initiate close!,
   interrupt application work, stop the network listener, or release the
   host's providers. A timeout is retryable. To safely stop the host, close
   the managed route, stop the owning HTTP listener, await drain, and call
   host/stop! only after the result is :drained. An unbounded callback can
   still prevent drain; hosts must not mistake a timeout for safe teardown.
   If the owning Live provider has already closed, return the same 503
   before application authorization or subscription admission. The provider
   check cannot replace the host's required ordered listener teardown.

   Call :close! and stop the owning HTTP listener before host/stop!. If a
   client disconnects, the SSE Manifold stream should close at the Ring
   adapter boundary; the source subscription is then released. The explicit
   :close! provides bounded, idempotent server-side cleanup even if the
   transport fails to observe a disconnected peer. Cancellation failures
   are reported after attempting all sessions; failed entries remain tracked
   so a repeated :close! can retry them. A failure to install a disconnect
   callback after SSE startup also triggers immediate session rollback; failed
   rollback stays tracked for the same retry path. If Live shuts down
   during session creation, the route returns 503 instead of surfacing a
   source-closed error or returning an already-dead EventSource stream.
   A synchronously terminated SSE stream is likewise rejected before HTTP
   handoff, even if the owning Live provider remains open. SSE options with
   unrecognized keys or invalid shapes fail typed preflight instead of
   silently ignoring a misspelled operational setting. Duplicate HTTP header
   names that differ only by case also fail preflight; Ring maps distinguish
   them, but HTTP does not."
  [started {:keys [path authorize-subscription sse-options] :as options}]
  (when-not (and (map? options)
                 (= (set (keys options))
                    (set (filter #(contains? options %)
                                 [:path :authorize-subscription :sse-options
                                  :max-connections
                                  :max-pending-authorizations]))))
    (reject! :unknown-options {}))
  (when-not (and (string? path)
                 (re-matches #"/[A-Za-z0-9._~/-]+" path)
                 (not= "/" path)
                 ;; Proxies and HTTP servers may normalize repeated separators
                 ;; or dot segments before Ring receives :uri. A managed SSE
                 ;; route must have one unambiguous literal path at both ends.
                 (not (str/includes? path "//"))
                 (not-any? #{"." ".."} (rest (str/split path #"/" -1))))
    (reject! :invalid-path {:path path}))
  (when-not (fn? authorize-subscription)
    (reject! :missing-authorization-callback {}))
  (doseq [[option default reason]
          [[:max-connections default-max-connections :invalid-max-connections]
           [:max-pending-authorizations default-max-pending-authorizations
            :invalid-max-pending-authorizations]]]
    (let [limit (get options option default)]
      (when-not (and (integer? limit) (pos? limit)
                     (<= limit Long/MAX_VALUE))
        (reject! reason {option limit}))))
  (when-not (and (map? started)
                 (get started managed/handle-key)
                 (fn? (:biff.ring/handler started))
                 (map? (:gesso.live/system started))
                 (not (live/closed? (:gesso.live/system started))))
    (reject! :missing-or-closed-host {}))
  ;; The generic SSE transport permits additional option keys for its own
  ;; callers. A managed browser route has a narrower, explicit contract:
  ;; misspelled keys must not silently disable keepalives or error hooks.
  ;; Check the caller's raw options before defaults make them look complete.
  (when-not (or (nil? sse-options) (map? sse-options))
    (reject! :invalid-sse-options {:sse-options-type (type sse-options)}))
  (when-let [unknown (seq (remove (set (keys sse/default-options))
                                  (keys sse-options)))]
    (reject! :unknown-sse-options {:keys (set unknown)}))
  ;; Validate the EventSource HTTP response contract before registering any
  ;; interest or exposing a route. The lower-level SSE transport intentionally
  ;; permits arbitrary valid HTTP statuses, but browser EventSource does not.
  (let [max-connections (get options :max-connections default-max-connections)
        max-pending-authorizations (get options :max-pending-authorizations
                                        default-max-pending-authorizations)
        ;; Generic SSE flows may intentionally be event-only, but an HTTP
        ;; managed route owns a finite session budget. Without periodic writes,
        ;; its HTTP adapter may not discover an idle client disconnect and the
        ;; abandoned session may occupy a slot until another event arrives.
        ;; Explicit caller choices still override this managed-route default.
        validated-sse (sse/prepare-options!
                       (merge {:keepalive-ms default-managed-keepalive-ms}
                              sse-options))
        _ (when (not= 200 (:status validated-sse))
            (reject! :invalid-sse-status {:status (:status validated-sse)}))
        ;; A fixed HTTP length can truncate an indefinitely streamed body.
        ;; The server/adapter, not an application override, owns framing.
        _ (when-let [header (some (fn [^String field]
                                    (when (.equalsIgnoreCase field "content-length")
                                      field))
                                  (keys (:headers validated-sse)))]
            (reject! :invalid-sse-content-length {:header header}))
        ;; User overrides must not defeat cache or proxy buffering protection,
        ;; mislabel the actual uncompressed SSE bytes, claim a partial range,
        ;; or set hop-by-hop framing fields owned by the HTTP server.
        _ (when-let [header (some (fn [^String field]
                                    (when (contains? reserved-sse-response-headers
                                                     (str/lower-case field))
                                      field))
                                  (keys (:headers validated-sse)))]
            (reject! :invalid-sse-reserved-header {:header header}))
        ;; Ring maps retain differently cased keys, but HTTP header field
        ;; names are case-insensitive. Two overrides for the same field could
        ;; produce contradictory Vary/CORS/cookie metadata after serialization.
        ;; A sequence of values under ONE key remains valid (e.g. Set-Cookie).
        _ (when-let [field (->> (keys (:headers validated-sse))
                                (group-by str/lower-case)
                                (keep (fn [[name variants]]
                                        (when (< 1 (count variants)) name)))
                                sort
                                first)]
            (reject! :duplicate-sse-header {:header field}))
        base-handler (:biff.ring/handler started)
        live-system (:gesso.live/system started)
        state (atom {:closed? false :close-completed? false
                     :connections {} :retry-cancellations {}
                     :disconnect-cleanups {} :cancelling-session-ids #{}
                     :occupied-session-ids #{} :starting 0
                     :pending-authorizations 0})
        drained-signal (promise)
        _ (add-watch state ::drain
                     (fn [_ _ _ updated]
                       ;; Once closed and empty, admission is permanently
                       ;; fenced. A single promise is sufficient for all
                       ;; waiters, and cannot miss a transition to drained.
                       (when (drained-state? updated)
                         (deliver drained-signal true))))
        close! (fn []
                 ;; A session may still be starting without the monitor.
                 ;; Publish closure first; the in-flight starter will cancel
                 ;; its newly created stream at the final handoff fence.
                 ;; Thus shutdown does not await a stalled SSE startup.
                 (swap! state assoc :closed? true)
                 (locking state
                   ;; Disconnect hooks may clear :connections *during* a
                   ;; failed cancellation. Track failed attempts separately
                   ;; so a later close! can retry even an already-closed
                   ;; stream whose provider cleanup may be incomplete.
                   (let [failures
                         (reduce-kv
                          (fn [failures key session]
                            (try
                              (cancel-registered-sse! state key session)
                              failures
                              (catch Throwable e
                                (conj failures e))))
                          []
                          (merge (:connections @state)
                                 (:disconnect-cleanups @state)
                                 (:retry-cancellations @state)))]
                     ;; Never abandon later sessions because an earlier
                     ;; cancellation failed. Preserve the original error
                     ;; identity and report additional failures as suppressed.
                     (when-let [failure (first failures)]
                       (doseq [additional (rest failures)]
                         (when-not (identical? failure additional)
                           (.addSuppressed ^Throwable failure
                                           ^Throwable additional)))
                       (throw failure))
                     ;; Mark completion after *all* cancellation callbacks
                     ;; have returned, not merely after a stream's on-closed
                     ;; handler removed the last registry entry. A failed
                     ;; pass stays incomplete until a successful retry.
                     (swap! state assoc :close-completed? true)
                     :closed)))
        handler
        (fn [request]
          (if (not= path (:uri request))
            (base-handler request)
            (cond
              (not= :get (:request-method request))
              {:status 405 :headers {"allow" "GET"} :body "Method Not Allowed"}

              ;; Closing this route must stop authorization-side work as
              ;; well as source admission. In particular, callbacks may use
              ;; providers which the owning host is about to shut down.
              (or (:closed? @state) (live/closed? live-system))
              {:status 503 :headers {} :body "Live shutting down"}

              :else
              ;; Reserve without the session admission monitor. If an SSE
              ;; startup stalls while holding that monitor, excess GETs must
              ;; still fail quickly instead of waiting unboundedly for it.
              (let [refusal (reserve-pending-authorization!
                             state live-system max-pending-authorizations)]
                (if refusal
                  refusal
                  (try
                    (let [[outcome value]
                          (try
                            [:authorized (authorize-subscription request)]
                            ;; Do not suppress thread interruption or VM-level
                            ;; errors during shutdown. Only ordinary callback
                            ;; failures may be superseded by the route's 503.
                            (catch InterruptedException e (throw e))
                            (catch Exception e [:failed e]))]
                      ;; Hold the reservation until the response has passed
                      ;; the admission fence, not merely until authorization
                      ;; returns. This also bounds GETs waiting on that lock.
                      ;; First reserve a connection slot under the monitor.
                      ;; An in-progress start counts toward the same finite
                      ;; budget as an established connection. No unbounded
                      ;; queue can form behind a slow source-backed startup.
                      (let [decision
                            (locking state
                              (cond
                                (or (:closed? @state) (live/closed? live-system))
                                {:status 503 :headers {}
                                 :body "Live shutting down"}

                                (= :failed outcome)
                                (throw value)

                                (nil? value)
                                {:status 403 :headers {} :body "Forbidden"}

                                (not (valid-scope? value))
                                (reject! :invalid-authorized-subscription {})

                                ;; Do not reveal connection capacity until
                                ;; the request has passed authorization.
                                (>= (occupied-sse-slots @state)
                                    max-connections)
                                {:status 503 :headers {}
                                 :body "Live connection capacity reached"}

                                :else
                                (do (swap! state update :starting inc)
                                    ::start)))]
                        (if (not= ::start decision)
                          decision
                          (let [startup-slot? (volatile! true)]
                            (try
                            ;; Starting a source-backed stream may block on
                            ;; provider work: never hold the shutdown monitor.
                            (let [[start-outcome session-or-error]
                                  (try
                                    [:started (live/start-sse! live-system value
                                                               {:sse-options validated-sse})]
                                    (catch InterruptedException e (throw e))
                                    (catch Exception e [:failed e]))]
                              ;; Only the final handoff is locked, so close!
                              ;; can retire established streams while an
                              ;; unrelated startup is stalled outside.
                              (locking state
                                (if (= :failed start-outcome)
                                  (if (or (:closed? @state)
                                          (live/closed? live-system))
                                    {:status 503 :headers {}
                                     :body "Live shutting down"}
                                    (throw session-or-error))
                                  (let [session session-or-error
                                        key (random-uuid)]
                                    ;; Transfer one reserved startup slot
                                    ;; into its registered session atomically.
                                    ;; Otherwise :occupied-count transiently
                                    ;; counts one session twice during callback
                                    ;; registration, violating the stated cap.
                                    (swap! state
                                           (fn [s]
                                             (-> s
                                                 (update :starting dec)
                                                 (assoc-in [:connections key]
                                                           session)
                                                 (update :occupied-session-ids
                                                         conj key))))
                                    (vreset! startup-slot? false)
                                    (try
                                      ;; A disconnected HTTP stream may leave
                                      ;; its eager Live interest registered if
                                      ;; the transport release hook fails. Keep
                                      ;; only unresolved disconnects; remove
                                      ;; them when the source tap actually closes.
                                      (when-let [on-source-closed!
                                                 (:source-on-closed! session)]
                                        (on-source-closed!
                                         #(swap! state
                                                 (fn [s]
                                                   (-> s
                                                       (update :disconnect-cleanups
                                                               dissoc key)
                                                       (retire-untracked-session
                                                        key))))))
                                      (stream/on-closed
                                       (:stream session)
                                       (fn []
                                         (if-let [source-closed?
                                                  (:source-closed? session)]
                                           (do
                                             ;; Move atomically: no transient
                                             ;; false quiescence between removing
                                             ;; an active stream and retaining an
                                             ;; unverified source release.
                                             (swap! state
                                                    (fn [s]
                                                      (-> s
                                                          (update :connections dissoc key)
                                                          (assoc-in [:disconnect-cleanups key]
                                                                    session))))
                                             ;; If the source closed just before
                                             ;; registration, its hook cannot
                                             ;; clear an obligation added later.
                                             ;; Recheck *after* publication.
                                             (when (true? (source-closed?))
                                               (swap! state
                                                      (fn [s]
                                                        (-> s
                                                            (update :disconnect-cleanups
                                                                    dissoc key)
                                                            (retire-untracked-session
                                                             key))))))
                                           (swap! state
                                                  (fn [s]
                                                    (-> s
                                                        (update :connections dissoc key)
                                                        (retire-untracked-session key)))))))
                                      ;; The route may have closed *while*
                                      ;; startup was running without the lock.
                                      ;; Never hand back an admitted 200 in
                                      ;; that case, even for a live provider.
                                      (let [route-closed? (:closed? @state)
                                            provider-closed? (live/closed? live-system)]
                                        (if (or route-closed? provider-closed?
                                                (stream/closed? (:stream session)))
                                          (do
                                            (cancel-registered-sse!
                                             state key session)
                                            {:status 503 :headers {}
                                             :body (if (or route-closed?
                                                           provider-closed?)
                                                     "Live shutting down"
                                                     "Live stream unavailable")})
                                          (:response session)))
                                      (catch Throwable admission-error
                                        ;; Preserve a failed cancellation for
                                        ;; an explicit close! retry, and retain
                                        ;; the original error as the cause.
                                        (try
                                          (cancel-registered-sse! state key session)
                                          (catch Throwable cancel-error
                                            (when-not (identical? admission-error
                                                                  cancel-error)
                                              (.addSuppressed ^Throwable admission-error
                                                              ^Throwable cancel-error))))
                                        (throw admission-error)))))))
                            (finally
                              ;; Startup errors have no connection entry to
                              ;; own the capacity. Only untransferred starts
                              ;; release their reservation in this finally.
                              (when @startup-slot?
                                (swap! state update :starting dec))))))))
                    (finally
                      ;; Includes ordinary failure and thread interruption.
                      (swap! state update :pending-authorizations dec))))))))]
    {:handler handler
     :close! close!
     :active-count (fn [] (count (:connections @state)))
     :occupied-count (fn [] (occupied-sse-slots @state))
     :pending-authorization-count (fn [] (:pending-authorizations @state))
     :closed? (fn [] (:closed? @state))
     :drained? (fn [] (drained-state? @state))
     :await-drained!
     (fn [timeout-ms]
       (when-not (and (integer? timeout-ms)
                      (<= 0 timeout-ms 120000))
         (reject! :invalid-drain-timeout-ms {:timeout-ms timeout-ms}))
       ;; The watch observes every state transition. Unlike polling, the
       ;; waiter is signaled when the final in-flight request is released.
       ;; A zero timeout is a nonblocking snapshot of terminal state.
       {:outcome (if (or (drained-state? @state)
                         (true? (deref drained-signal timeout-ms false)))
                   :drained :timed-out)})}))

(defn- validate-managed-http-shutdown-plan!
  [{:keys [route stop-listener! stop-host! drain-timeout-ms] :as options}]
  (when-not (and (map? options)
                 (= #{:route :stop-listener! :stop-host! :drain-timeout-ms}
                    (set (keys options)))
                 (map? route)
                 (fn? (:close! route))
                 (fn? (:await-drained! route))
                 (fn? stop-listener!)
                 (fn? stop-host!))
    (reject! :invalid-http-shutdown-plan {}))
  (when-not (and (integer? drain-timeout-ms)
                 (<= 0 drain-timeout-ms 120000))
    (reject! :invalid-drain-timeout-ms {:timeout-ms drain-timeout-ms}))
  options)

(defn stop-managed-http-host!
  "Perform caller-owned HTTP shutdown before stopping a Gesso host.

   Requires {:route managed-route :stop-listener! fn :stop-host! fn
             :drain-timeout-ms integer}, with a finite timeout in 0..120000.
   The route is closed first, then the caller's listener is stopped, then
   :await-drained! verifies that no source-backed sessions or unfinished
   admissions remain. Only a confirmed drain permits :stop-host!.

   The host stop callback must explicitly return {:outcome :stopped}
   (optionally with :host-result) after completing provider teardown.
   This callback acknowledgment is an external assertion, not independent
   proof that host providers have stopped. A normal return of nil, a
   timeout, or any other result is ambiguous and
   fails closed with :unconfirmed-host-stop-outcome. A coordinator retains
   :host-stop-unknown after this ambiguity, prohibiting automatic replay.
   Returns {:outcome :stopped :host-result ...} only upon confirmation,
   or {:outcome :timed-out} without touching the host's providers. The
   listener callback must return {:outcome :drained} ONLY after its server
   has stopped admitting new requests AND all relevant HTTP handlers (not
   merely SSE sessions) have finished using host providers. A listener
   {:outcome :timed-out} returns {:outcome :listener-timed-out} without
   stopping the host; an unrecognized outcome fails closed. These are
   explicit caller-owned assertions, not independently proven facts.
   Invalid listener, SSE-drain, and host-stop acknowledgments expose only
   fixed rejection reasons, never the external callback's result payload.

   The caller may retry once in-flight work finishes, provided its
   listener-stop callback is idempotent. This is a single-attempt primitive;
   use prepare-managed-http-shutdown for serialized, once-only retries.
   Otherwise, the owner must serialize invocations and prevent duplicates.
   A route-close failure does not suppress listener cleanup, but neither
   failure can authorize host stop. Invalid plans fail before effects.

   The timeout bounds only the SSE route drain wait, not arbitrary caller
   callbacks, listener drain, or provider shutdown. This helper never
   creates, owns, or implicitly drains the HTTP listener."
  [{:keys [route stop-listener! stop-host! drain-timeout-ms] :as options}]
  (validate-managed-http-shutdown-plan! options)
  ;; Failure of one cleanup step must not skip the listener's shutdown.
  ;; Preserve the route-close failure, with any listener error suppressed.
  (let [route-error (try ((:close! route)) nil
                         (catch Throwable e e))
        listener-result (try [:returned (stop-listener!)]
                             (catch Throwable e [:failed e]))]
    (when route-error
      (when (and (= :failed (first listener-result))
                 (not (identical? route-error (second listener-result))))
        (.addSuppressed ^Throwable route-error
                        ^Throwable (second listener-result)))
      (throw route-error))
    (when (= :failed (first listener-result))
      (throw (second listener-result)))
    ;; The listener owner must explicitly certify the entire handler drain.
    ;; A stopped socket alone says nothing about in-flight ordinary requests.
    (case (:outcome (second listener-result))
      :drained (let [drain ((:await-drained! route) drain-timeout-ms)]
                 (case (:outcome drain)
                   :drained (let [host-outcome (stop-host!)]
                              ;; A normal return is not proof of successful
                              ;; teardown. In particular, nil or a reported
                              ;; timeout may mean providers remain active.
                              ;; Never upgrade those outcomes to :stopped.
                              (if (and (map? host-outcome)
                                       (= :stopped (:outcome host-outcome)))
                                {:outcome :stopped
                                 :host-result (:host-result host-outcome)}
                                ;; External callback results may contain
                                ;; credentials or other application state,
                                ;; including within an unrecognized :outcome.
                                ;; Report the fixed reason only.
                                (reject! :unconfirmed-host-stop-outcome {})))
                   :timed-out {:outcome :timed-out}
                   (reject! :invalid-drain-outcome {})))
      :timed-out {:outcome :listener-timed-out}
      (reject! :invalid-listener-drain-outcome {}))))

(defn prepare-managed-http-shutdown
  "Create a caller-owned, serialized shutdown coordinator for one host.

   Accepts the same immutable plan as stop-managed-http-host! and rejects an
   invalid plan before any effects. Returns {:stop! callback :status callback}.
   Concurrent callers of :stop! share one monitor. A confirmed :stopped
   result is returned unchanged to later callers: listener and host cleanup
   cannot run twice through this coordinator.

   An SSE or listener drain timeout, or a cleanup error *before* stop-host!
   begins, restores :ready so the owner can retry. Both cleanup callbacks
   must be idempotent across such retries. If stop-host! throws or
   returns without the explicit :stopped outcome, Gesso cannot know whether
   provider teardown occurred; status becomes
   :host-stop-unknown and subsequent attempts reject with a typed error.
   This avoids treating an ambiguous external side effect as safe to replay.

   :status returns :ready, :running, :stopped, or :host-stop-unknown. The
   coordinator does not own the listener, automatically interrupt work, or
   infer that an external listener's drain acknowledgement is truthful."
  [plan]
  (validate-managed-http-shutdown-plan! plan)
  (let [monitor (Object.)
        phase (atom :ready)
        completed (atom nil)]
    {:status (fn [] @phase)
     :stop!
     (fn []
       (locking monitor
         (case @phase
           :stopped @completed
           :host-stop-unknown
           (reject! :host-shutdown-outcome-unknown {})
           :running
           (reject! :reentrant-managed-http-shutdown {})
           :ready
           (let [host-stop-entered? (volatile! false)
                 guarded-plan
                 (assoc plan :stop-host!
                        (fn []
                          ;; Publish the uncertainty before invoking code that
                          ;; may stop providers and then raise an exception.
                          (vreset! host-stop-entered? true)
                          ((:stop-host! plan))))]
             (reset! phase :running)
             (try
               (let [result (stop-managed-http-host! guarded-plan)]
                 (if (= :stopped (:outcome result))
                   (do
                     (reset! completed result)
                     (reset! phase :stopped))
                   (reset! phase :ready))
                 result)
               (catch Throwable error
                 (reset! phase (if @host-stop-entered?
                                 :host-stop-unknown :ready))
                 (throw error)))))))}))
