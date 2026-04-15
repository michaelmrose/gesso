(ns gesso.live.core
  "Thin generic entrypoint for gesso.live.

   This namespace owns only:
   - minimal live fragment rendering helpers
   - normalized changed-event construction
   - publication of changed events to the configured live bus
   - extraction of the current propagated consistency token

   It does not own:
   - application/domain subscription semantics
   - subscriber matching rules
   - transport framing
   - backend-specific consistency behavior"
  (:require
   [gesso.live.bus :as bus]
   [gesso.live.htmx :as htmx]
   [gesso.live.token :as token]))

(def default-event
  "Default event name used for live change notifications."
  "live-update")

(def default-swap
  "Default HTMX swap strategy."
  "outerHTML")

(def default-trigger
  "Default HTMX trigger prefix."
  "load")

(defn- ensure-present
  "Throw with a helpful message if `value` is nil."
  [value k]
  (when (nil? value)
    (throw (ex-info (str "Missing required live option: " k)
                    {:missing-key k})))
  value)

(defn- event-name
  "Return the event name to use for a fragment or event payload."
  [event]
  (or event default-event))

(defn- swap-mode
  "Return the HTMX swap mode to use for a fragment."
  [swap]
  (or swap default-swap))

(defn- trigger-mode
  "Return the HTMX trigger prefix to use for a fragment."
  [trigger]
  (or trigger default-trigger))

(defn- normalize-fragment-opts
  "Validate and normalize the public fragment options."
  [{:keys [id src stream-url event swap trigger attrs inner-attrs] :as opts}]
  (let [id'         (ensure-present id :id)
        src'        (ensure-present src :src)
        stream-url' (ensure-present stream-url :stream-url)
        event'      (event-name event)
        swap'       (swap-mode swap)
        trigger'    (trigger-mode trigger)]
    (assoc opts
           :id id'
           :src src'
           :stream-url stream-url'
           :event event'
           :swap swap'
           :trigger trigger'
           :attrs (or attrs {})
           :inner-attrs (or inner-attrs {}))))

(defn fragment-root-attrs
  "Build attrs for the outer live fragment wrapper.

   Required input keys:
   - :stream-url

   Optional input keys:
   - :attrs"
  [{:keys [stream-url attrs] :as opts}]
  (let [{:keys [stream-url attrs]} (normalize-fragment-opts opts)
        base-attrs (htmx/fragment-root-attrs {:stream-url stream-url})]
    (merge base-attrs attrs)))

(defn fragment-target-attrs
  "Build attrs for the live fragment refresh target.

   Required input keys:
   - :id
   - :src
   - :stream-url

   Optional input keys:
   - :event
   - :swap
   - :trigger
   - :inner-attrs"
  [{:keys [id src event swap trigger inner-attrs] :as opts}]
  (let [{:keys [id src event swap trigger inner-attrs]}
        (normalize-fragment-opts opts)
        base-attrs (htmx/fragment-target-attrs
                    {:id id
                     :src src
                     :event event
                     :swap swap
                     :trigger trigger})]
    (merge base-attrs inner-attrs)))

(defn fragment
  "Render a minimal live fragment shell.

   Example:
   (fragment
    {:id \"request-panel\"
     :src \"/app/requests/req-123/fragment\"
     :stream-url \"/gesso/live/stream?subscription=...\"})"
  [opts]
  (let [root-attrs   (fragment-root-attrs opts)
        target-attrs (fragment-target-attrs opts)]
    [:div root-attrs
     [:div target-attrs]]))

(defn current-consistency-token
  "Return the propagated consistency token from request context, if present."
  [ctx]
  (token/extract-request-token ctx))

(defn live-request?
  "Return true when the request appears to participate in live consistency flow."
  [ctx]
  (boolean (current-consistency-token ctx)))

(defn- normalize-changed
  "Validate and normalize the changed payload.

   This remains opaque to core, but it must be present."
  [changed]
  (ensure-present changed :changed))

(defn build-event
  "Normalize a changed-event payload.

   Required input keys:
   - :changed

   Optional input keys:
   - :event
   - :consistency-token
   - :data"
  [{:keys [changed event consistency-token data]}]
  (let [changed' (normalize-changed changed)
        event'   (event-name event)]
    {:event event'
     :changed changed'
     :consistency-token consistency-token
     :data data}))

(defn- resolve-bus
  "Resolve the live bus from ctx or throw if missing."
  [ctx]
  (let [live-bus (bus/bus-from-ctx ctx)]
    (ensure-present live-bus :gesso.live/bus)))

(defn publish-change!
  "Publish a normalized changed event to the configured live bus.

   Returns the normalized event map."
  [ctx opts]
  (let [event    (build-event opts)
        live-bus (resolve-bus ctx)]
    (bus/publish! live-bus event)
    event))
