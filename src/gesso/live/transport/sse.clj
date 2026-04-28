(ns gesso.live.transport.sse
  "SSE transport helpers for gesso.live.

   This namespace owns low-level SSE mechanics:
   - text/event-stream response headers
   - event/comment frame formatting
   - explicit flushing
   - queue-backed Ring streaming response bodies
   - the existing gesso.live.bus subscriber bridge

   It intentionally does not know about toasts, notifications, HTMX fragments,
   XTDB, or app-specific domains."
  (:require
   [clojure.string :as str]
   [gesso.live.bus :as bus]
   [ring.core.protocols :as protocols])
  (:import
   [java.io BufferedWriter OutputStreamWriter]
   [java.nio.charset StandardCharsets]
   [java.util UUID]
   [java.util.concurrent LinkedBlockingQueue TimeUnit]))

(def default-path
  "/gesso/live/stream")

(def default-event
  "live-update")

(def default-keepalive-ms
  15000)

(def default-headers
  {"content-type" "text/event-stream; charset=utf-8"
   "cache-control" "no-cache, no-transform"
   "connection" "keep-alive"
   "x-accel-buffering" "no"})

;; -----------------------------------------------------------------------------
;; Queue helpers
;; -----------------------------------------------------------------------------

(defn new-queue
  "Create a queue suitable for a queue-backed SSE stream."
  []
  (LinkedBlockingQueue.))

(defn offer!
  "Offer an event onto a queue.

   Generic event shape:
     {:event \"notifications-changed\"
      :data \"{}\"}

   Existing gesso.live.bus streams may also enqueue already-encoded SSE frame
   strings."
  [queue event]
  (.offer queue event))

;; -----------------------------------------------------------------------------
;; Event/frame encoding
;; -----------------------------------------------------------------------------

(defn event-name
  [event]
  (cond
    (map? event)
    (or (:event event) (:name event) default-event)

    (keyword? event)
    (name event)

    (symbol? event)
    (name event)

    (nil? event)
    default-event

    :else
    (str event)))

(defn event-payload
  [{:keys [changed consistency-token data]}]
  {:changed changed
   :consistency-token consistency-token
   :data data})

(defn encode-payload
  [payload]
  (pr-str payload))

(defn event-frame
  "Return a complete SSE event frame string.

   Multiline data is written as multiple data: lines, as required by the SSE
   format."
  [event data]
  (let [event-name' (event-name event)
        data'       (str (or data ""))
        lines       (str/split-lines data')
        lines       (if (seq lines) lines [""])]
    (str "event: " event-name' "\n"
         (apply str
                (map (fn [line]
                       (str "data: " line "\n"))
                     lines))
         "\n")))

(defn event->sse
  "Encode an existing gesso.live event map as an SSE frame.

   This preserves the original gesso.live event payload convention:
     {:changed ...
      :consistency-token ...
      :data ...}"
  [event]
  (let [payload (event-payload event)
        encoded (encode-payload payload)]
    (event-frame (event-name event) encoded)))

(defn keepalive-frame
  []
  ": keepalive\n\n")

(defn stream-url
  [subscription]
  (str default-path "?subscription=" subscription))

;; -----------------------------------------------------------------------------
;; Low-level writing
;; -----------------------------------------------------------------------------

(defn write-frame!
  "Write an already-encoded SSE frame and flush immediately."
  [^BufferedWriter writer frame]
  (.write writer (str frame))
  (.flush writer))

(defn write-event!
  "Write one SSE event and flush immediately."
  [^BufferedWriter writer event data]
  (write-frame! writer (event-frame event data)))

(defn write-comment!
  "Write one SSE comment/keepalive and flush immediately."
  [^BufferedWriter writer comment]
  (write-frame! writer (str ": " comment "\n\n")))

(defn default-on-open
  [writer]
  (write-event! writer "open" "{}"))

(defn default-on-event
  "Default event writer for generic queue streams.

   If the queue item is a string, it is assumed to be an already-encoded SSE
   frame.

   If the queue item is a map, :event/:name is used as the event name and :data
   is used as the payload."
  [writer event]
  (cond
    (string? event)
    (write-frame! writer event)

    (map? event)
    (write-event! writer
                  (event-name event)
                  (or (:data event) "{}"))

    :else
    (write-event! writer default-event (str event))))

(defn default-on-keepalive
  [writer]
  (write-frame! writer (keepalive-frame)))

;; -----------------------------------------------------------------------------
;; Generic queue-backed SSE response
;; -----------------------------------------------------------------------------

(defn queue-stream-body
  "Return a Ring StreamableResponseBody backed by a blocking queue.

   Options:
     :queue          required LinkedBlockingQueue
     :keepalive-ms   default 15000
     :closed?        optional atom; when true, stream loop stops
     :on-open        fn [writer]
     :on-event       fn [writer event]
     :on-keepalive   fn [writer]
     :on-error       fn [exception]
     :on-close       fn []

   This writes directly to the HTTP output stream and flushes each SSE frame.
   That is important for tiny SSE events, which otherwise may be delayed by
   server-side buffering."
  [{:keys [queue keepalive-ms closed?
           on-open on-event on-keepalive on-error on-close]
    :or {keepalive-ms default-keepalive-ms
         on-open default-on-open
         on-event default-on-event
         on-keepalive default-on-keepalive}}]
  (when-not queue
    (throw (ex-info "Missing required SSE queue." {:missing-key :queue})))
  (reify protocols/StreamableResponseBody
    (write-body-to-stream [_body _response out]
      (with-open [writer (BufferedWriter.
                          (OutputStreamWriter. out StandardCharsets/UTF_8))]
        (try
          (when on-open
            (on-open writer))

          (loop []
            (when-not (and closed? @closed?)
              (let [event (.poll ^LinkedBlockingQueue
                                 queue
                                 keepalive-ms
                                 TimeUnit/MILLISECONDS)]
                (if event
                  (on-event writer event)
                  (when on-keepalive
                    (on-keepalive writer)))
                (recur))))

          (catch Throwable e
            (when on-error
              (on-error e)))

          (finally
            (when on-close
              (on-close))))))))

(defn queue-stream-response
  "Return a Ring SSE response backed by a queue.

   Example:
     (queue-stream-response
      {:queue queue
       :on-close #(remove-client! client-id queue)})"
  [{:keys [headers] :as opts}]
  {:status 200
   :headers (merge default-headers headers)
   :body (queue-stream-body opts)})

;; -----------------------------------------------------------------------------
;; gesso.live.bus subscriber bridge
;; -----------------------------------------------------------------------------

(defn- subscriber-id
  []
  (str (UUID/randomUUID)))

(defn- queue-send-fn
  [q closed?]
  (fn [event]
    (when-not @closed?
      (.offer q (event->sse event)))))

(defn build-subscriber
  [{:keys [subscription queue meta closed?]}]
  (let [closed? (or closed? (atom false))]
    {:subscriber/id (subscriber-id)
     :subscription subscription
     :send! (queue-send-fn queue closed?)
     :meta meta
     :closed? closed?}))

(defn response
  [{:keys [live-bus subscriber queue keepalive-ms]}]
  (let [sub-id  (:subscriber/id subscriber)
        closed? (or (:closed? subscriber) (atom false))]
    (queue-stream-response
     {:queue queue
      :keepalive-ms (or keepalive-ms default-keepalive-ms)
      :closed? closed?

      :on-open
      (fn [writer]
        (bus/subscribe! live-bus subscriber)
        (write-frame! writer (keepalive-frame)))

      :on-event
      (fn [writer frame]
        (write-frame! writer frame))

      :on-keepalive
      (fn [writer]
        (write-frame! writer (keepalive-frame)))

      :on-error
      (fn [_e]
        nil)

      :on-close
      (fn []
        (reset! closed? true)
        (bus/unsubscribe! live-bus sub-id))})))

(defn handler
  [{:keys [ctx subscription-fn keepalive-ms]
    :or {keepalive-ms default-keepalive-ms}}]
  (let [live-bus     (bus/bus-from-ctx ctx)
        subscription (subscription-fn ctx)]
    (when-not live-bus
      (throw (ex-info "Missing :gesso.live/bus in ctx" {})))
    (when-not subscription
      (throw (ex-info "Missing or invalid live subscription" {})))
    (let [queue      (new-queue)
          closed?    (atom false)
          subscriber (build-subscriber
                      {:subscription subscription
                       :queue queue
                       :meta {:transport :sse}
                       :closed? closed?})]
      (response {:live-bus live-bus
                 :subscriber subscriber
                 :queue queue
                 :keepalive-ms keepalive-ms}))))
