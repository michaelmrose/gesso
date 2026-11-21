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
   Non-GET requests to the SSE path receive 405. Invalid callback results and
   callback exceptions propagate before a subscription can be installed.

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
  ;; Validate headers/encoder before registering an interest or handler.
  (let [validated-sse (sse/prepare-options! sse-options)
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

              :else
              (let [subscription (authorize-subscription request)]
                (cond
                  (nil? subscription)
                  {:status 403 :headers {} :body "Forbidden"}

                  (not (valid-scope? subscription))
                  (reject! :invalid-authorized-subscription {})

                  :else
                  (locking state
                    (if (:closed? @state)
                      {:status 503 :headers {} :body "Live shutting down"}
                      (let [session (live/start-sse! live-system subscription
                                                     {:sse-options validated-sse})
                            key (random-uuid)]
                        (swap! state assoc-in [:connections key] session)
                        (stream/on-closed
                         (:stream session)
                         #(swap! state update :connections dissoc key))
                        (:response session)))))))))]
    {:handler handler
     :close! close!
     :active-count (fn [] (count (:connections @state)))
     :closed? (fn [] (:closed? @state))}))
