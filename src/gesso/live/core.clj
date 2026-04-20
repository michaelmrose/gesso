(ns gesso.live.core
  "Thin generic entrypoint for gesso.live.

   This namespace owns only:
   - minimal live fragment rendering helpers
   - normalized changed-event construction
   - publication of changed events to the configured live bus
   - extraction of the current propagated consistency token
   - small generic HTMX/POST helper shapes for live fragments"
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
  [{:keys [stream-url attrs] :as opts}]
  (let [{:keys [stream-url attrs]} (normalize-fragment-opts opts)
        base-attrs (htmx/fragment-root-attrs {:stream-url stream-url})]
    (merge base-attrs attrs)))

(defn fragment-target-attrs
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
  [opts]
  (let [root-attrs   (fragment-root-attrs opts)
        target-attrs (fragment-target-attrs opts)]
    [:div root-attrs
     [:div target-attrs]]))

(defn fragment-panel
  "Renders the initial container for a live fragment.
   Now accepts ctx so it can pre-render the fragment with live data."
  ([config]
   ;; Fallback for static panels that don't need initial data
   (fragment-panel {} config))
  ([ctx config]
   (let [{:keys [subscription/token entry fragment/id fragment/src fragment/swap]} config]
     [:div {:id id
            :hx-get src
            :hx-trigger "load" ;; This ensures it loads the fragment immediately
            :hx-swap swap
            :path (str "/app/gesso/live/stream?subscription=" token)}
      ;; You can also explicitly call the fragment here if you want
      ;; it rendered server-side on the very first byte.
      ])))

(defn anti-forgery-token
  [ctx]
  (or (:anti-forgery-token ctx)
      (:biff/anti-forgery-token ctx)))

(defn anti-forgery-input
  [ctx]
  (when-let [token (anti-forgery-token ctx)]
    [:input {:type "hidden"
             :name "__anti-forgery-token"
             :value token}]))

(defn post-form-attrs
  [{:keys [to target swap attrs]
    :or {swap "innerHTML"}}]
  (let [to' (ensure-present to :to)]
    (htmx/post-form-attrs
     {:to to'
      :target target
      :swap swap
      :attrs attrs})))

(defn post-form
  [ctx opts & children]
  (into
   [:form (post-form-attrs opts)]
   (concat
    (keep identity [(anti-forgery-input ctx)])
    children)))

(defn post-button
  [ctx {:keys [to label children target swap form-attrs button-attrs]
        :or {swap "innerHTML"}}]
  (post-form
   ctx
   {:to to
    :target target
    :swap swap
    :attrs form-attrs}
   (into
    [:button (merge {:type "submit"} button-attrs)]
    (or children [label]))))

(defn current-consistency-token
  [ctx]
  (token/extract-request-token ctx))

(defn live-request?
  [ctx]
  (boolean (current-consistency-token ctx)))

(defn- normalize-changed
  [changed]
  (ensure-present changed :changed))

(defn build-event
  [{:keys [changed event consistency-token data]}]
  (let [changed' (normalize-changed changed)
        event'   (event-name event)]
    {:event event'
     :changed changed'
     :consistency-token consistency-token
     :data data}))

(defn- resolve-bus
  [ctx]
  (let [live-bus (bus/bus-from-ctx ctx)]
    (ensure-present live-bus :gesso.live/bus)))

(defn publish-change!
  [ctx opts]
  (let [event    (build-event opts)
        live-bus (resolve-bus ctx)]
    (bus/publish! live-bus event)
    event))
