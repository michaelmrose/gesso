(ns gesso.live.core
  "Thin generic entrypoint for gesso.live.

   This namespace owns only:
   - minimal live fragment rendering helpers
   - normalized changed-event construction
   - publication of changed events to the configured live bus
   - extraction of the current propagated consistency token
   - small generic HTMX/POST helper shapes for live fragments

   It does not own:
   - application/domain subscription semantics
   - subscriber matching rules
   - transport framing
   - backend-specific consistency behavior"
  (:require
   [clojure.string :as str]
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

(def default-post-swap
  "Default HTMX swap strategy for helper POST forms/buttons."
  "innerHTML")

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

(defn anti-forgery-token
  "Return the anti-forgery token from ctx.

   Supports the common plain Ring/Biff keys."
  [ctx]
  (or (:anti-forgery-token ctx)
      (:biff/anti-forgery-token ctx)))

(defn anti-forgery-input
  "Return a hidden anti-forgery input when a token is present."
  [ctx]
  (when-let [token (anti-forgery-token ctx)]
    [:input {:type "hidden"
             :name "__anti-forgery-token"
             :value token}]))

(defn normalize-target
  "Normalize an hx-target value.

   - nil stays nil
   - common selector-like values are left unchanged
   - bare ids become CSS id selectors"
  [target]
  (cond
    (nil? target) nil

    (and (string? target)
         (or (= target "this")
             (str/starts-with? target "#")
             (str/starts-with? target ".")
             (str/starts-with? target "closest ")
             (str/starts-with? target "find ")
             (str/starts-with? target "next ")
             (str/starts-with? target "previous ")
             (str/includes? target " ")))
    target

    :else
    (str "#" target)))

(defn post-form-attrs
  "Build standard attrs for a POST form that refreshes a target fragment.

   Required:
   - :to

   Optional:
   - :target
   - :swap (defaults to \"innerHTML\")
   - :attrs (merged last)"
  [{:keys [to target swap attrs]
    :or {swap default-post-swap}}]
  (let [to' (ensure-present to :to)]
    (merge
     {:method "post"
      :action to'
      :hx-post to'
      :hx-swap swap}
     (when-let [target' (normalize-target target)]
       {:hx-target target'})
     attrs)))

(defn post-form
  "Return a standard POST form with an anti-forgery input when available.

   opts:
   - :to      required
   - :target  optional
   - :swap    optional, defaults to \"innerHTML\"
   - :attrs   optional extra attrs merged into the form

   Any children are appended after the anti-forgery input."
  [ctx opts & children]
  (into
   [:form (post-form-attrs opts)]
   (concat
    (keep identity [(anti-forgery-input ctx)])
    children)))

(defn post-button
  "Return a standard POST form containing a single submit button.

   opts:
   - :to            required
   - :label         button contents when :children omitted
   - :children      optional explicit button contents
   - :target        optional
   - :swap          optional, defaults to \"innerHTML\"
   - :form-attrs    optional attrs merged into the form
   - :button-attrs  optional attrs merged into the button"
  [ctx {:keys [to label children target swap form-attrs button-attrs]
        :or {swap default-post-swap}}]
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
