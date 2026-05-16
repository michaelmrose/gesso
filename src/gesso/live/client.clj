(ns gesso.live.client
  "Connected browser-client delivery mechanics for gesso.live.

   This namespace owns generic ephemeral client delivery:

   - connected client channels
   - generated browser client ids
   - SSE listener markup
   - SSE stream responses
   - pending OOB fragment queues
   - targeted send helpers
   - connected client introspection

   It does not own app policy:

   - route placement
   - middleware
   - user identity
   - authorization
   - client scopes
   - response wrapping

   App namespaces should wrap this with their own policy adapter."
  (:require
   [clojure.string :as str]
   [gesso.live.htmx :as htmx]
   [manifold.deferred :as d]
   [manifold.stream :as s]))

;; -----------------------------------------------------------------------------
;; Defaults
;; -----------------------------------------------------------------------------

(def default-event
  "client-oob")

(def default-endpoint
  {:base-path "/gesso/live/client"
   :stream-path "/gesso/live/client/stream"
   :pending-path "/gesso/live/client/pending"
   :client-id-param :client-id})

(def default-options
  {:id nil
   :event default-event
   :endpoint default-endpoint
   :client (fn [_ctx]
             {:client/scopes #{}})})

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn- now-ms
  []
  (System/currentTimeMillis))

(defn- ex
  [message data]
  (ex-info message data))

(defn- opts
  [options]
  (merge default-options options))

(defn- normalize-endpoint
  [endpoint]
  (merge default-endpoint endpoint))

(defn- require-fn!
  [k f]
  (when-not (fn? f)
    (throw
     (ex "gesso.live client channel option must be a function."
         {:key k
          :value f})))
  f)

(defn- event-name
  [event]
  (htmx/event-name event))

(defn- param-name
  [k]
  (cond
    (keyword? k) (name k)
    (symbol? k)  (name k)
    :else        (str k)))

(defn- request-param
  [ctx k]
  (let [ks [k (param-name k)]]
    (some
     (fn [path]
       (some #(get-in ctx (conj path %)) ks))
     [[:params]
      [:query-params]
      [:path-params]
      [:form-params]])))

(defn- encode-query-value
  [x]
  (java.net.URLEncoder/encode (str x) "UTF-8"))

(defn- append-query-param
  [url k v]
  (str url
       (if (str/includes? url "?") "&" "?")
       (param-name k)
       "="
       (encode-query-value v)))

(defn- sse-frame
  [event data]
  (str "event: " (event-name event) "\n"
       "data: " (or data "1") "\n\n"))

(defn- strip-runtime-fields
  [client]
  (dissoc client :stream))

(defn- close-stream!
  [stream]
  (when stream
    (try
      (s/close! stream)
      (catch Throwable _
        nil))))

(defn- call-client-fn
  [f ctx]
  (or (f ctx) {}))

(defn- connected-client
  [channel ctx client-id stream]
  (let [client-fn (:client channel)
        app-client (call-client-fn client-fn ctx)
        now (now-ms)]
    (merge
     app-client
     {:client/id client-id
      :client/scopes (set (:client/scopes app-client))
      :stream stream
      :connected-at now
      :last-seen-at now})))

(defn- client-id-param
  [channel]
  (get-in channel [:endpoint :client-id-param]))

(defn- client-id-from-ctx
  [channel ctx]
  (request-param ctx (client-id-param channel)))

(defn- stream-url
  [channel client-id]
  (append-query-param
   (get-in channel [:endpoint :stream-path])
   (client-id-param channel)
   client-id))

(defn- pending-url
  [channel client-id]
  (append-query-param
   (get-in channel [:endpoint :pending-path])
   (client-id-param channel)
   client-id))

;; -----------------------------------------------------------------------------
;; Channel lifecycle
;; -----------------------------------------------------------------------------

(defn channel
  "Create a connected-client delivery channel.

   Options:
     :id
       Optional channel id.

     :event
       SSE event name used to wake pending OOB fetches. Defaults to
       \"client-oob\".

     :endpoint
       Map with:
         :stream-path
         :pending-path
         :client-id-param

     :client
       Function of ctx -> app client descriptor.

       The descriptor may include:
         :client/user-id
         :client/scopes

       Scopes are opaque. The app is responsible for only registering scopes
       that the current user is authorized to receive."
  ([] (channel nil))
  ([options]
   (let [options'  (opts options)
         endpoint' (normalize-endpoint (:endpoint options'))
         client-fn (require-fn! :client (:client options'))]
     {:id (or (:id options') (random-uuid))
      :event (event-name (:event options'))
      :endpoint endpoint'
      :client client-fn
      :state (atom {:clients {}
                    :pending {}
                    :latest-client-id nil
                    :created-at (now-ms)
                    :sent-count 0
                    :wakeup-count 0
                    :dropped-count 0})})))

(defn reset-channel!
  "Close all connected client streams and clear channel state."
  [channel]
  (let [old-state @(:state channel)]
    (doseq [client (vals (:clients old-state))]
      (close-stream! (:stream client)))
    (reset! (:state channel)
            {:clients {}
             :pending {}
             :latest-client-id nil
             :created-at (now-ms)
             :sent-count 0
             :wakeup-count 0
             :dropped-count 0}))
  :reset)

(defn new-client-id
  "Return a new opaque browser client id."
  []
  (str (random-uuid)))

;; -----------------------------------------------------------------------------
;; Browser listener
;; -----------------------------------------------------------------------------

(defn listener
  "Render the browser-side listener for one connected client.

   Call shapes:

     (listener channel ctx)

     (listener channel ctx
       {:client/id \"...\"
        :id \"listener-id\"
        :attrs {...}
        :trigger-attrs {...}})

   The returned Hiccup opens an SSE connection and uses the HTMX SSE extension to
   fetch pending OOB fragments whenever the channel wakes this client."
  ([channel ctx]
   (listener channel ctx nil))
  ([channel ctx options]
   (let [client-id (or (:client/id options)
                       (new-client-id))
         id        (or (:id options)
                       (str "gesso-live-client-listener-" client-id))
         attrs     (:attrs options)
         trigger-attrs (:trigger-attrs options)]
     (htmx/sse-callback
      {:id id
       :stream-url (stream-url channel client-id)
       :event (:event channel)
       :get (pending-url channel client-id)
       :swap "none"
       :attrs (merge {:data-gesso-live-client-listener true
                      :data-gesso-live-client-id client-id
                      :data-gesso-live-channel-id (str (:id channel))}
                     attrs)
       :trigger-attrs (merge {:data-gesso-live-client-pending-trigger true}
                             trigger-attrs)}))))

;; -----------------------------------------------------------------------------
;; Stream registration
;; -----------------------------------------------------------------------------

(defn- remove-client-if-same-stream!
  [channel client-id stream]
  (swap! (:state channel)
         (fn [state]
           (let [client (get-in state [:clients client-id])]
             (if (identical? stream (:stream client))
               (update state :clients dissoc client-id)
               state))))
  nil)

(defn- register-client!
  [channel ctx client-id stream]
  (let [client (connected-client channel ctx client-id stream)]
    (swap! (:state channel)
           (fn [state]
             (assoc state
                    :latest-client-id client-id
                    :clients (assoc (:clients state) client-id client))))
    client))

(defn- wake-client-stream!
  [channel client-id stream]
  (let [put-result (s/put! stream (sse-frame (:event channel) client-id))]
    (d/on-realized
     put-result
     (fn [accepted?]
       (if accepted?
         (swap! (:state channel) update :wakeup-count inc)
         (do
           (remove-client-if-same-stream! channel client-id stream)
           (swap! (:state channel) update :dropped-count inc))))
     (fn [_error]
       (remove-client-if-same-stream! channel client-id stream)
       (swap! (:state channel) update :dropped-count inc)))
    put-result))

(defn stream-response
  "Return a Ring response for the client SSE stream.

   The request must include the configured client-id query param. If absent, a
   new id is generated, but normal listener markup always supplies one.

   On stream open, the channel sends one wake event. This lets reconnecting
   clients drain any pending fragments that were queued shortly before the
   connection dropped."
  [channel ctx]
  (let [client-id (or (client-id-from-ctx channel ctx)
                      (new-client-id))
        stream    (s/stream 32)]
    (register-client! channel ctx client-id stream)
    (s/on-closed stream
                 #(remove-client-if-same-stream! channel client-id stream))

    ;; Initial wake is harmless when pending is empty and useful after reconnect.
    (wake-client-stream! channel client-id stream)

    {:status 200
     :headers {"content-type" "text/event-stream; charset=utf-8"
               "cache-control" "no-cache, no-transform"
               "connection" "keep-alive"
               "x-accel-buffering" "no"}
     :body stream}))

;; -----------------------------------------------------------------------------
;; Pending OOB fragments
;; -----------------------------------------------------------------------------

(defn- enqueue-pending!
  [channel client-id fragments]
  (swap! (:state channel)
         (fn [state]
           (-> state
               (update-in [:pending client-id]
                          (fnil into [])
                          fragments)
               (update :sent-count inc))))
  client-id)

(defn drain-fragments!
  "Drain and return pending fragments for client-id.

   Returns nil when no fragments are pending."
  [channel client-id]
  (let [drained (atom nil)]
    (swap! (:state channel)
           (fn [state]
             (let [fragments (get-in state [:pending client-id])]
               (reset! drained fragments)
               (update state :pending dissoc client-id))))
    (when (seq @drained)
      (vec @drained))))

(defn drain-fragment!
  "Drain pending fragments for the client identified by ctx.

   Returns a wrapper Hiccup node containing all pending OOB fragments, or nil
   when none are pending.

   Wrapping is intentional: HTMX still processes nested hx-swap-oob content, and
   the wrapper gives response renderers a single Hiccup root."
  [channel ctx]
  (when-let [client-id (client-id-from-ctx channel ctx)]
    (when-let [fragments (drain-fragments! channel client-id)]
      (into [:div {:data-gesso-live-client-pending true
                   :data-gesso-live-client-id client-id}]
            fragments))))

;; -----------------------------------------------------------------------------
;; Targeting
;; -----------------------------------------------------------------------------

(defn connected-clients
  "Return a map of connected client-id -> app-safe client descriptor."
  [channel]
  (into {}
        (map (fn [[client-id client]]
               [client-id (strip-runtime-fields client)]))
        (:clients @(:state channel))))

(defn connected-client-ids
  "Return connected client ids."
  [channel]
  (vec (keys (:clients @(:state channel)))))

(defn latest-client-id
  "Return the most recently connected client id, if any."
  [channel]
  (:latest-client-id @(:state channel)))

(defn- clients-matching-target
  [channel to]
  (let [clients (:clients @(:state channel))]
    (cond
      (= :all to)
      clients

      (and (vector? to) (= :client (first to)))
      (select-keys clients [(second to)])

      (and (vector? to) (= :user (first to)))
      (let [user-id (str (second to))]
        (into {}
              (filter (fn [[_ client]]
                        (= user-id (str (:client/user-id client)))))
              clients))

      (and (vector? to) (= :scope (first to)))
      (let [scope (second to)]
        (into {}
              (filter (fn [[_ client]]
                        (contains? (:client/scopes client) scope)))
              clients))

      :else
      (throw
       (ex "Unsupported gesso.live client delivery target."
           {:to to
            :valid-targets [:all
                            [:client 'client-id]
                            [:user 'user-id]
                            [:scope 'scope]]})))))

;; -----------------------------------------------------------------------------
;; Sending
;; -----------------------------------------------------------------------------

(defn send!
  "Send complete OOB fragments to a target.

   Target forms:
     :all
     [:client client-id]
     [:user user-id]
     [:scope scope]

   Fragments should already be HTMX OOB Hiccup, for example:
     (g/oob-inner-html \"notification-count\" \"3\")
     (g/render-toast-oob toast)

   Returns:
     {:sent ...
      :woke ...
      :woke? ...
      :target ...}"
  [channel {:keys [to fragments] :as request}]
  (let [fragments' (vec fragments)
        targets    (clients-matching-target channel to)]
    (doseq [[client-id _client] targets]
      (enqueue-pending! channel client-id fragments'))

    (let [woke
          (reduce-kv
           (fn [n client-id client]
             (if-let [stream (:stream client)]
               (do
                 (wake-client-stream! channel client-id stream)
                 (inc n))
               n))
           0
           targets)]
      {:sent (count targets)
       :woke woke
       :woke? (pos? woke)
       :target to
       :fragment-count (count fragments')
       :request request})))

(defn send-to-client!
  "Send complete OOB fragments to one connected browser client."
  [channel client-id & fragments]
  (send! channel {:to [:client client-id]
                  :fragments fragments}))

(defn send-to-user!
  "Send complete OOB fragments to every connected browser client for user-id."
  [channel user-id & fragments]
  (send! channel {:to [:user (str user-id)]
                  :fragments fragments}))

(defn send-to-scope!
  "Send complete OOB fragments to every connected browser client in scope."
  [channel scope & fragments]
  (send! channel {:to [:scope scope]
                  :fragments fragments}))

(defn broadcast!
  "Send complete OOB fragments to every connected browser client.

   This should be explicit and rare."
  [channel & fragments]
  (send! channel {:to :all
                  :fragments fragments}))

;; -----------------------------------------------------------------------------
;; Introspection
;; -----------------------------------------------------------------------------

(defn pending-counts
  "Return pending fragment counts by client id."
  [channel]
  (into {}
        (map (fn [[client-id fragments]]
               [client-id (count fragments)]))
        (:pending @(:state channel))))

(defn state-summary
  "Return a small introspection map for the channel."
  [channel]
  (let [state @(:state channel)]
    {:id (:id channel)
     :event (:event channel)
     :connected-count (count (:clients state))
     :connected-client-ids (vec (keys (:clients state)))
     :latest-client-id (:latest-client-id state)
     :pending-counts (pending-counts channel)
     :sent-count (:sent-count state)
     :wakeup-count (:wakeup-count state)
     :dropped-count (:dropped-count state)
     :created-at (:created-at state)}))
