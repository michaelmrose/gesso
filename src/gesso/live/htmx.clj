(ns gesso.live.htmx
  "HTMX-facing helpers for gesso.live.

   This namespace owns:
   - wrapper attrs for the live transport connection
   - target attrs for HTMX refresh
   - the canonical request header used for propagated consistency tokens
   - low-level HTMX attribute builders for live POST helpers
   - generic HTMX/SSE callback markup."
  (:require
   [clojure.string :as str]))

(def default-event
  "Default SSE event name used by live fragments."
  "live-update")

(def default-fragment-swap
  "Default HTMX swap mode for live fragment refresh."
  "outerHTML")

(def default-fragment-trigger
  "Default HTMX trigger prefix for live fragment refresh."
  "load")

(def default-post-swap
  "Default HTMX swap mode for helper POST forms/buttons."
  "innerHTML")

(def default-sse-callback-swap
  "Default HTMX swap mode for SSE callback requests.

   For callback requests that return only OOB fragments, \"none\" is usually the
   right default."
  "none")

;; -----------------------------------------------------------------------------
;; Shared normalization
;; -----------------------------------------------------------------------------

(defn token-header-name
  "Return the canonical request header used for the propagated consistency token."
  []
  "x-gesso-live-consistency-token")

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

(defn- event-name
  [event]
  (cond
    (keyword? event) (name event)
    (symbol? event)  (name event)
    (nil? event)     default-event
    :else            (str event)))

(defn sse-trigger
  "Return an HTMX SSE trigger string for event.

   Examples:
     (sse-trigger \"toast\") => \"sse:toast\"
     (sse-trigger :toast)   => \"sse:toast\""
  [event]
  (str "sse:" (event-name event)))

;; -----------------------------------------------------------------------------
;; Live fragment refresh helpers
;; -----------------------------------------------------------------------------

(defn fragment-root-attrs
  "Build attrs for the outer live wrapper."
  [{:keys [stream-url]}]
  {:hx-ext "sse"
   :sse-connect stream-url})

(defn fragment-trigger
  "Build the canonical fragment trigger string."
  [{:keys [event trigger]
    :or {event default-event
         trigger default-fragment-trigger}}]
  (str trigger ", " (sse-trigger event)))

(defn fragment-target-attrs
  "Build attrs for the live fragment refresh target."
  [{:keys [id src event swap trigger]
    :or {event default-event
         swap default-fragment-swap
         trigger default-fragment-trigger}}]
  {:id id
   :hx-get src
   :hx-trigger (fragment-trigger {:event event
                                  :trigger trigger})
   :hx-swap swap})

;; -----------------------------------------------------------------------------
;; POST helper attrs
;; -----------------------------------------------------------------------------

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
  (merge
   {:method "post"
    :action to
    :hx-post to
    :hx-swap swap}
   (when-let [target' (normalize-target target)]
     {:hx-target target'})
   attrs))

;; -----------------------------------------------------------------------------
;; SSE callback helpers
;; -----------------------------------------------------------------------------

(defn sse-callback-root-attrs
  "Attrs for an HTMX SSE connection root.

   This element owns the EventSource connection. The child trigger element should
   usually own the HTMX request that reacts to a named SSE event.

   Options:
   - :id
   - :stream-url
   - :attrs merged last"
  [{:keys [id stream-url attrs]}]
  (merge
   (cond-> {:hx-ext "sse"
            :sse-connect stream-url
            :aria-hidden "true"}
     id (assoc :id id))
   attrs))

(defn sse-callback-trigger-attrs
  "Attrs for a child element that reacts to an SSE event by making an HTMX
   request.

   This follows the parent SSE connection + child hx-trigger pattern.

   Options:
   - :event
   - :get
   - :post
   - :target
   - :swap defaults to \"none\"
   - :attrs merged last"
  [{:keys [event get post target swap attrs]
    :or {event default-event
         swap default-sse-callback-swap}}]
  (merge
   (cond-> {:hx-trigger (sse-trigger event)
            :hx-swap swap}
     get    (assoc :hx-get get)
     post   (assoc :hx-post post)
     target (assoc :hx-target (normalize-target target)))
   attrs))

(defn sse-callback
  "Render a parent SSE connection and a child HTMX callback trigger.

   This is useful when an SSE event should wake the browser and cause it to fetch
   ordinary server-rendered HTML, usually OOB fragments.

   Example:
     (sse-callback
      {:id \"client-listener\"
       :stream-url \"/app/client-plumbing/stream?client-id=...\"
       :event \"client-oob\"
       :get \"/app/client-plumbing/pending?client-id=...\"
       :swap \"none\"})

   Root options:
   - :id
   - :stream-url
   - :attrs

   Trigger options:
   - :event
   - :get
   - :post
   - :target
   - :swap
   - :trigger-attrs"
  [{:keys [trigger-attrs] :as opts}]
  [:div (sse-callback-root-attrs opts)
   [:div (sse-callback-trigger-attrs
          (assoc opts :attrs trigger-attrs))]])
