(ns gesso.live.transport.sse
  "SSE transport for gesso.live.

   This namespace does four real things:
   - formats SSE frames
   - opens a long-lived Ring streaming response
   - registers/unregisters a subscriber with the live bus
   - writes matched events to the client as they arrive

   Assumptions:
   - request ctx contains :gesso.live/bus
   - caller supplies a `subscription-fn` that turns ctx into an app subscription
   - gesso.live.bus provides subscribe!/unsubscribe!"
  (:require
   [clojure.java.io :as io]
   [ring.util.io :as ring-io]
   [gesso.live.bus :as bus])
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

(defn event-name
  "Return the SSE event name for a normalized live event."
  [event]
  (or (:event event) default-event))

(defn event-payload
  "Return the payload value encoded into the SSE data field."
  [{:keys [changed consistency-token data]}]
  {:changed changed
   :consistency-token consistency-token
   :data data})

(defn encode-payload
  "Encode an SSE payload value.

   Initial implementation uses pr-str for simplicity."
  [payload]
  (pr-str payload))

(defn event->sse
  "Convert a normalized live event into an SSE frame string."
  [event]
  (let [event-name' (event-name event)
        payload     (event-payload event)
        encoded     (encode-payload payload)]
    (str "event: " event-name' "\n"
         "data: " encoded "\n\n")))

(defn keepalive-frame
  "Return an SSE keepalive frame."
  []
  ": keepalive\n\n")

(defn stream-url
  "Build an SSE stream URL from an already-encoded subscription token.

   This version assumes the caller has already encoded the value if needed."
  [subscription]
  (str default-path "?subscription=" subscription))

(defn- subscriber-id []
  (str (UUID/randomUUID)))

(defn- queue-send-fn
  [q]
  (fn [event]
    (.offer q (event->sse event))))

(defn build-subscriber
  "Build a bus subscriber backed by a blocking queue."
  [{:keys [subscription queue meta]}]
  {:subscriber/id (subscriber-id)
   :subscription subscription
   :send! (queue-send-fn queue)
   :meta meta})

(defn- write-frame!
  [^BufferedWriter writer frame]
  (.write writer frame)
  (.flush writer))

(defn- stream-loop!
  [^BufferedWriter writer q keepalive-ms]
  (write-frame! writer (keepalive-frame))
  (loop []
    (let [frame (.poll ^LinkedBlockingQueue q keepalive-ms TimeUnit/MILLISECONDS)]
      (if frame
        (write-frame! writer frame)
        (write-frame! writer (keepalive-frame)))
      (recur))))

(defn response
  "Build a Ring SSE response for one subscriber.

   The stream stays open until the client disconnects or writing fails."
  [{:keys [live-bus subscriber queue keepalive-ms]}]
  {:status 200
   :headers {"content-type" "text/event-stream; charset=utf-8"
             "cache-control" "no-cache, no-transform"
             "connection" "keep-alive"
             "x-accel-buffering" "no"}
   :body
   (ring-io/piped-input-stream
    (fn [out]
      (let [sub-id (:subscriber/id subscriber)]
        (bus/subscribe! live-bus subscriber)
        (try
          (with-open [writer (BufferedWriter.
                              (OutputStreamWriter. out StandardCharsets/UTF_8))]
            (stream-loop! writer queue keepalive-ms))
          (catch Throwable _
            nil)
          (finally
            (bus/unsubscribe! live-bus sub-id))))))})

(defn handler
  "Open an SSE stream and subscribe it to the live bus.

   Required opts:
   - :ctx
   - :subscription-fn  ; (fn [ctx] -> subscription-or-nil)

   Optional opts:
   - :keepalive-ms"
  [{:keys [ctx subscription-fn keepalive-ms]
    :or {keepalive-ms default-keepalive-ms}}]
  (let [live-bus     (bus/bus-from-ctx ctx)
        subscription (subscription-fn ctx)]
    (when-not live-bus
      (throw (ex-info "Missing :gesso.live/bus in ctx" {})))
    (when-not subscription
      (throw (ex-info "Missing or invalid live subscription" {})))
    (let [queue      (LinkedBlockingQueue.)
          subscriber (build-subscriber
                      {:subscription subscription
                       :queue queue
                       :meta {:transport :sse}})]
      (response {:live-bus live-bus
                 :subscriber subscriber
                 :queue queue
                 :keepalive-ms keepalive-ms}))))
