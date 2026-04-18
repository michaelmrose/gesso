(ns gesso.live.transport.sse
  (:require
   [gesso.live.bus :as bus]
   [ring.core.protocols :as protocols])
  (:import
   [java.io BufferedWriter OutputStream OutputStreamWriter]
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
  [event]
  (or (:event event) default-event))

(defn event-payload
  [{:keys [changed consistency-token data]}]
  {:changed changed
   :consistency-token consistency-token
   :data data})

(defn encode-payload
  [payload]
  (pr-str payload))

(defn event->sse
  [event]
  (let [event-name' (event-name event)
        payload     (event-payload event)
        encoded     (encode-payload payload)]
    (str "event: " event-name' "\n"
         "data: " encoded "\n\n")))

(defn keepalive-frame
  []
  ": keepalive\n\n")

(defn stream-url
  [subscription]
  (str default-path "?subscription=" subscription))

(defn- subscriber-id []
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

(defn- write-frame!
  [^BufferedWriter writer frame]
  (.write writer frame)
  ;; Directly flush to the socket, bypassing Ring's stream buffer
  (.flush writer))

(defn- stream-loop!
  [^BufferedWriter writer q keepalive-ms closed?]
  (write-frame! writer (keepalive-frame))
  (loop []
    (when-not @closed?
      (let [frame (.poll ^LinkedBlockingQueue q keepalive-ms TimeUnit/MILLISECONDS)]
        (if frame
          (write-frame! writer frame)
          (write-frame! writer (keepalive-frame)))
        (recur)))))

(defn response
  [{:keys [live-bus subscriber queue keepalive-ms]}]
  (let [sub-id  (:subscriber/id subscriber)
        closed? (or (:closed? subscriber) (atom false))]
    {:status 200
     :headers {"content-type" "text/event-stream; charset=utf-8"
               "cache-control" "no-cache, no-transform"
               "connection" "keep-alive"
               "x-accel-buffering" "no"}
     ;; Reify the protocol, omitting the type hint on `out`
     :body (reify protocols/StreamableResponseBody
             (write-body-to-stream [_ _ out]
               (bus/subscribe! live-bus subscriber)
               (try
                 (with-open [writer (BufferedWriter.
                                     (OutputStreamWriter. out StandardCharsets/UTF_8))]
                   (stream-loop! writer queue keepalive-ms closed?))
                 (catch Throwable _
                   nil)
                 (finally
                   (reset! closed? true)
                   (bus/unsubscribe! live-bus sub-id)))))}))

(defn handler
  [{:keys [ctx subscription-fn keepalive-ms]
    :or {keepalive-ms default-keepalive-ms}}]
  (let [live-bus     (bus/bus-from-ctx ctx)
        subscription (subscription-fn ctx)]
    (when-not live-bus
      (throw (ex-info "Missing :gesso.live/bus in ctx" {})))
    (when-not subscription
      (throw (ex-info "Missing or invalid live subscription" {})))
    (let [queue      (LinkedBlockingQueue.)
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
