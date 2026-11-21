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
  "Return {:handler Ring-handler :close! callback :active-count callback}.

   Requires a started concrete host, a fixed canonical :path (absolute URL
   path with no query string, repeated slash, or dot traversal segment), and
   a trusted :authorize-subscription callback. The callback
   is responsible for authenticating the Ring request and deciding whether
   this requester may subscribe to the returned exact scope. Nil denies with
   403. Unknown paths are passed to the existing canonical Ring handler.
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
                                 [:path :authorize-subscription :sse-options]))))
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
  (let [validated-sse (sse/prepare-options! sse-options)
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
        state (atom {:closed? false :connections {}})
        close! (fn []
                 (locking state
                   ;; Mark closed before cancellation so no request can admit
                   ;; another session. Retain failures in :connections: a
                   ;; later close! must be able to retry them, even if one
                   ;; cancellation aborts before its stream is closed.
                   (swap! state assoc :closed? true)
                   (let [failures
                         (reduce-kv
                          (fn [failures key session]
                            (try
                              (live/cancel-sse! session)
                              (swap! state update :connections dissoc key)
                              failures
                              (catch Throwable e
                                (conj failures e))))
                          []
                          (:connections @state))]
                     ;; Never abandon later sessions because an earlier
                     ;; cancellation failed. Preserve the original error
                     ;; identity and report additional failures as suppressed.
                     (when-let [failure (first failures)]
                       (doseq [additional (rest failures)]
                         (when-not (identical? failure additional)
                           (.addSuppressed ^Throwable failure
                                           ^Throwable additional)))
                       (throw failure))
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
              (let [[outcome value]
                    (try
                      [:authorized (authorize-subscription request)]
                      ;; Do not suppress thread interruption or VM-level
                      ;; errors during shutdown. Only ordinary callback
                      ;; failures may be superseded by the route's 503.
                      (catch InterruptedException e (throw e))
                      (catch Exception e [:failed e]))]
                ;; Authorization runs outside the lock to avoid blocking
                ;; shutdown. Interpret both success and ordinary failure
                ;; inside the admission lock, where closure has precedence.
                (locking state
                  (cond
                    (or (:closed? @state) (live/closed? live-system))
                    {:status 503 :headers {} :body "Live shutting down"}

                    (= :failed outcome)
                    (throw value)

                    (nil? value)
                    {:status 403 :headers {} :body "Forbidden"}

                    (not (valid-scope? value))
                    (reject! :invalid-authorized-subscription {})

                    :else
                    (let [[start-outcome session-or-error]
                          (try
                            [:started (live/start-sse! live-system value
                                                       {:sse-options validated-sse})]
                            ;; A provider shutdown cannot suppress thread
                            ;; interruption or fatal JVM failure.
                            (catch InterruptedException e (throw e))
                            (catch Exception e [:failed e]))]
                      (if (= :failed start-outcome)
                        (if (live/closed? live-system)
                          {:status 503 :headers {} :body "Live shutting down"}
                          (throw session-or-error))
                        (let [session session-or-error
                              key (random-uuid)]
                          (swap! state assoc-in [:connections key] session)
                          (try
                            (stream/on-closed
                             (:stream session)
                             #(swap! state update :connections dissoc key))
                            ;; Either the provider or the SSE transport can
                            ;; terminate during startup. A synchronously
                            ;; closed body is not an admissible HTTP 200
                            ;; EventSource response even when Live is healthy.
                            (let [provider-closed? (live/closed? live-system)]
                              (if (or provider-closed?
                                      (stream/closed? (:stream session)))
                                (do
                                  (live/cancel-sse! session)
                                  (swap! state update :connections dissoc key)
                                  {:status 503 :headers {}
                                   :body (if provider-closed?
                                           "Live shutting down"
                                           "Live stream unavailable")})
                                (:response session)))
                            (catch Throwable admission-error
                              ;; A failed HTTP hook or rollback cannot orphan
                              ;; a source interest. Failed cancellation stays
                              ;; in :connections for the next close! retry.
                              (try
                                (live/cancel-sse! session)
                                (swap! state update :connections dissoc key)
                                (catch Throwable cancel-error
                                  (when-not (identical? admission-error cancel-error)
                                    (.addSuppressed ^Throwable admission-error
                                                    ^Throwable cancel-error))))
                              (throw admission-error))))))))))))]
    {:handler handler
     :close! close!
     :active-count (fn [] (count (:connections @state)))
     :closed? (fn [] (:closed? @state))}))
