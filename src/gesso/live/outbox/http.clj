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

   Requires a started concrete host, a fixed :path (absolute URL path with no
   query string), and a trusted :authorize-subscription callback. The callback
   is responsible for authenticating the Ring request and deciding whether
   this requester may subscribe to the returned exact scope. Nil denies with
   403. Unknown paths are passed to the existing canonical Ring handler.
   Non-GET requests to the SSE path receive 405. Managed EventSource routes
   require an HTTP 200 response status: another accepted transport status
   would register a Live interest but cannot establish EventSource delivery.
   Reject this misconfiguration before invoking authorization or subscribing.
   Invalid callback results and callback exceptions propagate before a
   subscription can be installed. Once closed, the managed GET route
   returns 503 without consulting authorization or registering interests;
   the inner locked check still prevents admission when shutdown races an
   authorization already in progress. Shutdown also takes precedence over
   a late denial, malformed scope, or ordinary callback exception: all yield
   503 once closed, without revealing provider failure during teardown.
   Exceptions while the route is open retain their original identity.

   Call :close! and stop the owning HTTP listener before host/stop!. If a
   client disconnects, the SSE Manifold stream should close at the Ring
   adapter boundary; the source subscription is then released. The explicit
   :close! provides bounded, idempotent server-side cleanup even if the
   transport fails to observe a disconnected peer."
  [started {:keys [path authorize-subscription sse-options] :as options}]
  (when-not (and (map? options)
                 (= (set (keys options))
                    (set (filter #(contains? options %)
                                 [:path :authorize-subscription :sse-options]))))
    (reject! :unknown-options {}))
  (when-not (and (string? path)
                 (re-matches #"/[A-Za-z0-9._~/-]+" path)
                 (not= "/" path))
    (reject! :invalid-path {:path path}))
  (when-not (fn? authorize-subscription)
    (reject! :missing-authorization-callback {}))
  (when-not (and (map? started)
                 (get started managed/handle-key)
                 (fn? (:biff.ring/handler started))
                 (map? (:gesso.live/system started))
                 (not (live/closed? (:gesso.live/system started))))
    (reject! :missing-or-closed-host {}))
  ;; Validate the EventSource HTTP response contract before registering any
  ;; interest or exposing a route. The lower-level SSE transport intentionally
  ;; permits arbitrary valid HTTP statuses, but browser EventSource does not.
  (let [validated-sse (sse/prepare-options! sse-options)
        _ (when (not= 200 (:status validated-sse))
            (reject! :invalid-sse-status {:status (:status validated-sse)}))
        base-handler (:biff.ring/handler started)
        live-system (:gesso.live/system started)
        state (atom {:closed? false :connections {}})
        close! (fn []
                 (locking state
                   (let [{:keys [connections]} @state]
                     (swap! state assoc :closed? true :connections {})
                     (doseq [session (vals connections)]
                       (live/cancel-sse! session)))
                   :closed))
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
              (:closed? @state)
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
                    (:closed? @state)
                    {:status 503 :headers {} :body "Live shutting down"}

                    (= :failed outcome)
                    (throw value)

                    (nil? value)
                    {:status 403 :headers {} :body "Forbidden"}

                    (not (valid-scope? value))
                    (reject! :invalid-authorized-subscription {})

                    :else
                    (let [session (live/start-sse! live-system value
                                                   {:sse-options validated-sse})
                          key (random-uuid)]
                      (swap! state assoc-in [:connections key] session)
                      (stream/on-closed
                       (:stream session)
                       #(swap! state update :connections dissoc key))
                      (:response session))))))))]
    {:handler handler
     :close! close!
     :active-count (fn [] (count (:connections @state)))
     :closed? (fn [] (:closed? @state))}))
