(ns gesso.live.transport.sse
  "SSE transport helpers for gesso.live.")

(def default-path
  "/gesso/live/stream")

(def default-event
  "live-update")

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
