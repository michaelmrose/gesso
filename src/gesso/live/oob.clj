(ns gesso.live.oob
  "Connected-client delivery for arbitrary HTMX OOB fragments.

   This namespace owns reusable in-process OOB delivery mechanics:

   - channel state
   - connected browser client registry
   - per-client pending OOB mailbox
   - SSE stream response for one connected client
   - browser listener markup
   - generic send! targeting by client, user, scope, or all
   - pending fragment draining

   It intentionally does not know about:
   - routes
   - middleware
   - Biff/Reitit modules
   - app response helpers
   - toasts
   - notifications
   - XTDB
   - app authorization policy

   Applications own URL layout, route definitions, middleware, identity, scopes,
   and the choice to wrap drained fragments with g/html-response or equivalent."
  (:require
   [clojure.string :as str]
   [gesso.live.htmx :as live-htmx]
   [gesso.live.transport.sse :as sse])
  (:import
   [java.net URLEncoder]
   [java.nio.charset StandardCharsets]))

;; -----------------------------------------------------------------------------
;; Defaults
;; -----------------------------------------------------------------------------

(def default-event
  "client-oob")

(def default-client-id-param
  :client-id)

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn new-client-id
  "Return a fresh browser-client id."
  []
  (str (random-uuid)))

(defn- now-ms
  []
  (System/currentTimeMillis))

(defn- ensure-present
  [value k]
  (when (nil? value)
    (throw (ex-info (str "Missing required live OOB option: " k)
                    {:missing-key k})))
  value)

(defn- id-part
  [x]
  (let [s (-> (str x)
              (str/replace #"[^A-Za-z0-9_-]+" "-")
              (str/replace #"^-+" "")
              (str/replace #"-+$" ""))]
    (if (str/blank? s)
      "oob"
      s)))

(defn- url-encode
  [x]
  (URLEncoder/encode (str x) (.name StandardCharsets/UTF_8)))

(defn- append-query-param
  [url k v]
  (let [joiner (if (str/includes? url "?") "&" "?")]
    (str url
         joiner
         (url-encode (name k))
         "="
         (url-encode v))))

(defn- request-param
  [params k]
  (or (get params k)
      (get params (name k))
      (get params (keyword k))))

(defn- hiccup-node?
  [x]
  (and (vector? x)
       (let [tag (first x)]
         (or (keyword? tag)
             (symbol? tag)
             (string? tag)))))

(defn- normalize-fragments
  [fragments]
  (cond
    (nil? fragments)
    []

    (hiccup-node? fragments)
    [fragments]

    (sequential? fragments)
    (->> fragments
         (remove nil?)
         vec)

    :else
    [fragments]))

(defn- normalize-scopes
  [scopes]
  (cond
    (nil? scopes) #{}
    (set? scopes) scopes
    :else (set scopes)))

(defn- normalize-client
  [client]
  (let [client (or client {})]
    (assoc client
           :client/scopes
           (normalize-scopes (:client/scopes client)))))

(defn- default-client
  [_ctx]
  {})

;; -----------------------------------------------------------------------------
;; Channel construction
;; -----------------------------------------------------------------------------

(defn channel
  "Create an in-process connected-client OOB channel.

   Required:
   - :id

   Optional:
   - :event
       SSE event name used to wake clients.
       Defaults to \"client-oob\".

   - :endpoint
       App-supplied URL config:
         {:stream-path \"/app/client-plumbing/stream\"
          :pending-path \"/app/client-plumbing/pending\"
          :client-id-param :client-id}

       Gesso uses this to build listener URLs and to read client ids from ctx.
       The app still owns actual route definitions.

   - :client
       fn [ctx] => {:client/user-id ...
                    :client/scopes #{...}}

       The app supplies identity and scopes. Gesso treats scopes as opaque
       comparable values.

   - :drop-pending-on-close?
       If true, pending fragments for a client are removed when that exact
       stream connection closes.
       Defaults to true.

   The returned channel is stateful. Define it once in an app adapter namespace."
  [{:keys [id event endpoint client drop-pending-on-close?]
    :or {event default-event
         client default-client
         drop-pending-on-close? true}}]
  {:channel/id (ensure-present id :id)
   :event event
   :endpoint (merge {:client-id-param default-client-id-param}
                    endpoint)
   :client client
   :drop-pending-on-close? drop-pending-on-close?
   :clients (atom {})
   :pending-fragments (atom {})})

(defn reset-channel!
  "Clear all connected clients and pending OOB fragments for channel.

   Useful during REPL-driven development."
  [channel]
  (reset! (:clients channel) {})
  (reset! (:pending-fragments channel) {})
  :reset)

;; -----------------------------------------------------------------------------
;; Endpoint helpers
;; -----------------------------------------------------------------------------

(defn client-id-param
  [channel]
  (get-in channel [:endpoint :client-id-param] default-client-id-param))

(defn client-id-from-ctx
  [channel {:keys [params]}]
  (or (request-param params (client-id-param channel))
      (new-client-id)))

(defn- client-id-value
  [channel ctx-or-client-id]
  (if (and (map? ctx-or-client-id)
           (contains? ctx-or-client-id :params))
    (client-id-from-ctx channel ctx-or-client-id)
    ctx-or-client-id))

(defn stream-path
  [channel]
  (ensure-present (get-in channel [:endpoint :stream-path])
                  :endpoint/stream-path))

(defn pending-path
  [channel]
  (ensure-present (get-in channel [:endpoint :pending-path])
                  :endpoint/pending-path))

(defn stream-url
  [channel client-id]
  (append-query-param (stream-path channel)
                      (client-id-param channel)
                      client-id))

(defn pending-url
  [channel client-id]
  (append-query-param (pending-path channel)
                      (client-id-param channel)
                      client-id))

;; -----------------------------------------------------------------------------
;; Browser listener markup
;; -----------------------------------------------------------------------------

(defn listener
  "Render the browser-side SSE listener for this OOB channel.

   Arity 2:
     Creates a fresh client id and uses the channel endpoint config.

   Arity 3 options:
     :client/id
     :id
     :stream-url
     :pending-url
     :attrs
     :trigger-attrs

   Gesso does not own routes. The app either supplies :endpoint in channel or
   passes explicit :stream-url and :pending-url."
  ([channel ctx]
   (listener channel ctx {}))
  ([channel _ctx opts]
   (let [client-id    (or (:client/id opts)
                          (new-client-id))
         stream-url'  (or (:stream-url opts)
                          (stream-url channel client-id))
         pending-url' (or (:pending-url opts)
                          (pending-url channel client-id))
         root-id      (or (:id opts)
                          (str (id-part (:channel/id channel))
                               "-"
                               (id-part client-id)
                               "-listener"))]
     (live-htmx/sse-callback
      {:id root-id
       :stream-url stream-url'
       :event (:event channel)
       :get pending-url'
       :swap "none"
       :attrs (:attrs opts)
       :trigger-attrs (:trigger-attrs opts)}))))

;; -----------------------------------------------------------------------------
;; Client registry
;; -----------------------------------------------------------------------------

(defn- build-client
  [channel ctx client-id queue]
  (let [client-fn (:client channel)
        client    (normalize-client (client-fn ctx))]
    (merge client
           {:client/id (ensure-present client-id :client/id)
            :client/queue queue
            :client/connected-at (now-ms)})))

(defn- register-client!
  [channel client]
  (swap! (:clients channel)
         assoc
         (:client/id client)
         client)
  client)

(defn- unregister-client!
  [channel client-id queue]
  (let [removed? (atom false)]
    (swap! (:clients channel)
           (fn [m]
             ;; Avoid removing a newer connection that reused the same client id.
             (if (= queue (get-in m [client-id :client/queue]))
               (do
                 (reset! removed? true)
                 (dissoc m client-id))
               m)))
    @removed?))

(defn connected-clients
  "Return connected client maps, sorted by connection time."
  [channel]
  (->> @(:clients channel)
       vals
       (sort-by :client/connected-at)
       vec))

(defn connected-client-ids
  [channel]
  (mapv :client/id (connected-clients channel)))

(defn latest-client-id
  "Return the most recently connected client id, if any."
  [channel]
  (some-> (connected-clients channel)
          last
          :client/id))

(defn client-by-id
  [channel client-id]
  (get @(:clients channel) client-id))

(defn clients-for-user
  "Return all connected clients whose :client/user-id equals user-id."
  [channel user-id]
  (->> @(:clients channel)
       vals
       (filter #(= user-id (:client/user-id %)))
       (sort-by :client/connected-at)
       vec))

(defn clients-for-scope
  "Return all connected clients registered for scope.

   Scopes are app-defined opaque values, commonly vectors such as:
     [:user user-id]
     [:store store-id]
     [:request request-id]"
  [channel scope]
  (->> @(:clients channel)
       vals
       (filter #(contains? (:client/scopes %) scope))
       (sort-by :client/connected-at)
       vec))

(defn- client-summary
  [client]
  (select-keys client
               [:client/id
                :client/user-id
                :client/scopes
                :client/connected-at]))

(defn pending-counts
  [channel]
  (->> @(:pending-fragments channel)
       (map (fn [[client-id fragments]]
              [client-id (count fragments)]))
       (into {})))

(defn state-summary
  [channel]
  {:channel/id (:channel/id channel)
   :connected (mapv client-summary (connected-clients channel))
   :pending (pending-counts channel)})

;; -----------------------------------------------------------------------------
;; SSE stream response
;; -----------------------------------------------------------------------------

(defn stream-response
  "Return an SSE stream response for one connected client.

   Arity 2:
     Extracts client id from ctx using channel endpoint config.

   Arity 3:
     Uses caller-supplied client id.

   Arity 4 options:
     :on-register fn [client]
     :on-close    fn [client]"
  ([channel ctx]
   (stream-response channel ctx (client-id-from-ctx channel ctx)))
  ([channel ctx client-id]
   (stream-response channel ctx client-id {}))
  ([channel ctx client-id {:keys [on-register on-close]}]
   (let [queue  (sse/new-queue)
         client (build-client channel ctx client-id queue)]
     (register-client! channel client)
     (when on-register
       (on-register client))
     (sse/queue-stream-response
      {:queue queue
       :on-close
       (fn []
         (let [removed? (unregister-client! channel client-id queue)]
           (when (and removed?
                      (:drop-pending-on-close? channel))
             (swap! (:pending-fragments channel) dissoc client-id))
           (when on-close
             (on-close client))))}))))

;; -----------------------------------------------------------------------------
;; Pending fragment mailbox
;; -----------------------------------------------------------------------------

(defn- enqueue-pending-fragments!
  [channel client-id fragments]
  (swap! (:pending-fragments channel)
         update
         client-id
         (fnil into [])
         fragments))

(defn drain!
  "Drain and return pending OOB fragments.

   Arity 2 accepts either ctx or a client id:
     (drain! channel ctx)
     (drain! channel client-id)

   Returns a vector of fragments, possibly empty.

   This function intentionally returns data, not an HTTP response. The app owns
   whether to wrap the result with g/html-response, g/no-content, or something
   else."
  [channel ctx-or-client-id]
  (let [client-id (client-id-value channel ctx-or-client-id)]
    (loop []
      (let [m         @(:pending-fragments channel)
            fragments (get m client-id [])]
        (if (compare-and-set! (:pending-fragments channel)
                              m
                              (dissoc m client-id))
          (vec fragments)
          (recur))))))

(defn drain-fragment!
  "Drain pending OOB fragments and return a Hiccup fragment, or nil if empty."
  [channel ctx-or-client-id]
  (let [fragments (drain! channel ctx-or-client-id)]
    (when (seq fragments)
      (into [:<>] (remove nil?) fragments))))

;; -----------------------------------------------------------------------------
;; Targeting
;; -----------------------------------------------------------------------------

(defn target-clients
  "Return connected clients matching target.

   Target forms:
     :all
     [:client client-id]
     [:user user-id]
     [:scope scope]"
  [channel to]
  (cond
    (= to :all)
    (connected-clients channel)

    (and (vector? to)
         (= :client (first to)))
    (if-let [client (client-by-id channel (second to))]
      [client]
      [])

    (and (vector? to)
         (= :user (first to)))
    (clients-for-user channel (second to))

    (and (vector? to)
         (= :scope (first to)))
    (clients-for-scope channel (second to))

    :else
    (throw (ex-info "Invalid live OOB target."
                    {:to to
                     :expected [:all
                                [:client :client-id]
                                [:user :user-id]
                                [:scope :scope-value]]}))))

(defn- wake-client!
  [channel client-id]
  (if-let [queue (get-in @(:clients channel) [client-id :client/queue])]
    (sse/offer! queue {:event (:event channel)
                       :data "{}"})
    false))

;; -----------------------------------------------------------------------------
;; Sending
;; -----------------------------------------------------------------------------

(defn send!
  "Send arbitrary complete OOB fragments to connected clients.

   Required:
   - :to
       :all
       [:client client-id]
       [:user user-id]
       [:scope scope]

   - :fragments
       one complete OOB Hiccup fragment, or a sequence of complete OOB fragments

   Compatibility:
   - :oob is also accepted as an alias for :fragments.

   Returns a delivery summary.

   This is best-effort, in-process delivery. It is not durable and does not
   cross app server processes."
  [channel {:keys [to fragments oob] :as opts}]
  (let [to'        (ensure-present to :to)
        raw        (if (contains? opts :fragments) fragments oob)
        fragments' (normalize-fragments raw)
        targets    (target-clients channel to')]
    (when-not (seq fragments')
      (throw (ex-info "Missing live OOB fragments."
                      {:missing-key :fragments
                       :to to'})))
    (doseq [{:client/keys [id]} targets]
      (enqueue-pending-fragments! channel id fragments'))
    {:to to'
     :sent (count targets)
     :results
     (mapv
      (fn [{:client/keys [id]}]
        {:client/id id
         :woke? (wake-client! channel id)
         :pending (count (get @(:pending-fragments channel) id))})
      targets)}))

(defn send-to-client!
  "Send arbitrary complete OOB fragments to one connected browser client."
  [channel client-id & fragments]
  (send! channel
         {:to [:client client-id]
          :fragments fragments}))

(defn send-to-user!
  "Send arbitrary complete OOB fragments to every connected browser client for user-id."
  [channel user-id & fragments]
  (send! channel
         {:to [:user user-id]
          :fragments fragments}))

(defn send-to-scope!
  "Send arbitrary complete OOB fragments to every connected browser client in scope."
  [channel scope & fragments]
  (send! channel
         {:to [:scope scope]
          :fragments fragments}))

(defn broadcast!
  "Send arbitrary complete OOB fragments to every connected browser client."
  [channel & fragments]
  (send! channel
         {:to :all
          :fragments fragments}))
