(ns gesso.live.oob
  "Connected-client delivery for arbitrary HTMX OOB fragments.

   This namespace owns reusable in-process OOB delivery mechanics:

   - channel state
   - connected browser client registry
   - per-client pending OOB mailbox
   - SSE stream response for one connected client
   - browser listener markup when given explicit stream/pending URLs
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
   [gesso.live.transport.sse :as sse]))

;; -----------------------------------------------------------------------------
;; Defaults
;; -----------------------------------------------------------------------------

(def default-event
  "client-oob")

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn new-client-id
  "Return a fresh browser-client id.

   App code can use this when rendering a listener that should be targetable
   later, for example in a REPL/demo section."
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
  (-> (str x)
      (str/replace #"[^A-Za-z0-9_-]+" "-")
      (str/replace #"^-+" "")
      (str/replace #"-+$" "")))

(defn- hiccup-node?
  [x]
  (and (vector? x)
       (let [tag (first x)]
         (or (keyword? tag)
             (symbol? tag)
             (string? tag)))))

(defn- normalize-oob-nodes
  [oob]
  (cond
    (nil? oob)
    []

    (hiccup-node? oob)
    [oob]

    (sequential? oob)
    (->> oob
         (remove nil?)
         vec)

    :else
    [oob]))

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
  [{:keys [id event client drop-pending-on-close?]
    :or {event default-event
         client default-client
         drop-pending-on-close? true}}]
  {:channel/id (ensure-present id :id)
   :event event
   :client client
   :drop-pending-on-close? drop-pending-on-close?
   :clients (atom {})
   :pending-oob (atom {})})

(defn reset-channel!
  "Clear all connected clients and pending OOB fragments for channel.

   Useful during REPL-driven development."
  [channel]
  (reset! (:clients channel) {})
  (reset! (:pending-oob channel) {})
  :reset)

;; -----------------------------------------------------------------------------
;; Browser listener markup
;; -----------------------------------------------------------------------------

(defn listener
  "Render the browser-side SSE listener for this OOB channel.

   Gesso does not own routes, so the app must provide explicit URLs.

   Required opts:
   - :stream-url
   - :pending-url

   Optional opts:
   - :client/id
   - :id
   - :attrs
   - :trigger-attrs

   Example:
     (listener channel
      {:stream-url \"/app/client-plumbing/stream?client-id=abc\"
       :pending-url \"/app/client-plumbing/pending?client-id=abc\"})"
  [channel {:keys [stream-url pending-url id attrs trigger-attrs] :as opts}]
  (let [client-id (:client/id opts)
        root-id   (or id
                      (str (id-part (:channel/id channel))
                           (when client-id
                             (str "-" (id-part client-id)))
                           "-listener"))]
    (live-htmx/sse-callback
     {:id root-id
      :stream-url (ensure-present stream-url :stream-url)
      :event (:event channel)
      :get (ensure-present pending-url :pending-url)
      :swap "none"
      :attrs attrs
      :trigger-attrs trigger-attrs})))

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
  (->> @(:pending-oob channel)
       (map (fn [[client-id nodes]]
              [client-id (count nodes)]))
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

   The app owns the route handler and calls this with the client id it extracted
   from the request.

   Example app route handler:
     (defn stream [ctx]
       (let [client-id (client-id-from-ctx ctx)]
         (oob/stream-response channel ctx client-id)))"
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
             (swap! (:pending-oob channel) dissoc client-id))
           (when on-close
             (on-close client))))}))))

;; -----------------------------------------------------------------------------
;; Pending OOB mailbox
;; -----------------------------------------------------------------------------

(defn- enqueue-pending-oobs!
  [channel client-id nodes]
  (swap! (:pending-oob channel)
         update
         client-id
         (fnil into [])
         nodes))

(defn drain!
  "Drain and return pending OOB nodes for client-id.

   Returns a vector of nodes, possibly empty.

   This function intentionally returns data, not an HTTP response. The app owns
   whether to wrap the result with g/html-response, g/no-content, or something
   else."
  [channel client-id]
  (loop []
    (let [m     @(:pending-oob channel)
          nodes (get m client-id [])]
      (if (compare-and-set! (:pending-oob channel)
                            m
                            (dissoc m client-id))
        (vec nodes)
        (recur)))))

(defn drain-fragment!
  "Drain pending OOB nodes and return a Hiccup fragment, or nil if empty.

   Example app pending handler:
     (defn pending [ctx]
       (let [client-id (client-id-from-ctx ctx)]
         (if-let [fragment (oob/drain-fragment! channel client-id)]
           (g/html-response fragment)
           (g/no-content))))"
  [channel client-id]
  (let [nodes (drain! channel client-id)]
    (when (seq nodes)
      (into [:<>] (remove nil?) nodes))))

;; -----------------------------------------------------------------------------
;; Targeting and sending
;; -----------------------------------------------------------------------------

(defn- target-clients
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

(defn send!
  "Send arbitrary OOB fragments to connected clients.

   Required:
   - :to
       :all
       [:client client-id]
       [:user user-id]
       [:scope scope]

   - :oob
       one Hiccup OOB node, or a sequence of OOB nodes

   Returns a delivery summary.

   This is best-effort, in-process delivery. It is not durable and does not
   cross app server processes."
  [channel {:keys [to oob]}]
  (let [to'     (ensure-present to :to)
        nodes   (normalize-oob-nodes oob)
        targets (target-clients channel to')]
    (when-not (seq nodes)
      (throw (ex-info "Missing live OOB fragments."
                      {:missing-key :oob
                       :to to'})))
    (doseq [{:client/keys [id]} targets]
      (enqueue-pending-oobs! channel id nodes))
    {:to to'
     :sent (count targets)
     :results
     (mapv
      (fn [{:client/keys [id]}]
        {:client/id id
         :woke? (wake-client! channel id)
         :pending (count (get @(:pending-oob channel) id))})
      targets)}))
