(ns gesso.live.htmx
  "HTMX-facing helpers for gesso.live.

   This namespace owns browser-facing attribute construction only:

   - SSE connection attrs
   - live fragment refresh attrs
   - SSE trigger strings
   - optional static refresh jitter
   - POST helper attrs
   - SSE callback attrs
   - direct SSE swap/OOB listener attrs
   - canonical consistency-token header naming

   It intentionally does not know about:

   - Missionary
   - Manifold
   - Aleph
   - XTDB
   - app domains
   - authorization
   - source implementation

   Higher-level callers such as gesso.live.core should validate full public
   configs with gesso.live.schema. These low-level attr builders still validate
   obvious required options so they do not silently produce broken HTMX markup."
  (:require
   [clojure.string :as str]))

;; -----------------------------------------------------------------------------
;; Defaults
;; -----------------------------------------------------------------------------

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

(def default-sse-direct-swap
  "Default swap mode for direct SSE swap/OOB listener elements.

   \"none\" is intended for payloads that contain only hx-swap-oob content.

   This direct-OOB path should be verified with browser/integration tests before
   application code depends on it."
  "none")

;; -----------------------------------------------------------------------------
;; Small attr helpers
;; -----------------------------------------------------------------------------

(defn clean-attrs
  "Remove nil values from an attr map.

   False, empty strings, zeroes, and empty collections are preserved."
  [attrs]
  (into {}
        (remove (comp nil? val))
        attrs))

(defn- non-blank-string?
  [x]
  (and (string? x)
       (not (str/blank? x))))

(defn- require-non-blank!
  [opts k label]
  (let [v (get opts k)]
    (when-not (non-blank-string? v)
      (throw
       (ex-info (str label " is required and must be a non-blank string.")
                {:key k
                 :value v
                 :opts opts})))
    v))

(defn- require-present!
  [opts k label]
  (let [v (get opts k)]
    (when-not (some? v)
      (throw
       (ex-info (str label " is required.")
                {:key k
                 :value v
                 :opts opts})))
    v))

(defn- ensure-one-method!
  "Throw unless exactly one of get/post is supplied."
  [get post]
  (cond
    (and get post)
    (throw
     (ex-info "HTMX SSE callback trigger may not include both :get and :post."
              {:get get
               :post post}))

    (not (or get post))
    (throw
     (ex-info "HTMX SSE callback trigger requires either :get or :post."
              {:get get
               :post post}))

    :else
    nil))

;; -----------------------------------------------------------------------------
;; hx-ext composition
;; -----------------------------------------------------------------------------

(defn- extension-name
  [x]
  (cond
    (keyword? x) (name x)
    (symbol? x)  (name x)
    :else        (str x)))

(defn- split-hx-ext
  [x]
  (cond
    (nil? x)
    []

    (sequential? x)
    (mapcat split-hx-ext x)

    :else
    (->> (str/split (extension-name x) #"[,\s]+")
         (remove str/blank?))))

(defn hx-ext-value
  "Build a normalized comma-separated hx-ext value.

   Examples:
     (hx-ext-value \"sse\" \"path-deps\")
     => \"sse, path-deps\"

     (hx-ext-value \"sse,path-deps\" :debug)
     => \"sse, path-deps, debug\"

   Duplicate extensions are removed while preserving first occurrence order."
  [& xs]
  (let [exts (->> xs
                  (mapcat split-hx-ext)
                  distinct
                  vec)]
    (when (seq exts)
      (str/join ", " exts))))

(defn merge-attrs
  "Merge attr maps, remove nil values, and compose :hx-ext instead of letting
   caller attrs accidentally replace required extensions.

   Caller attrs still override ordinary attributes."
  [& maps]
  (let [merged (apply merge maps)
        ext    (apply hx-ext-value (map :hx-ext maps))]
    (clean-attrs
     (cond-> merged
       ext (assoc :hx-ext ext)))))

(defn merge-sse-attrs
  "Merge attrs while ensuring the HTMX SSE extension remains present."
  [& maps]
  (apply merge-attrs
         {:hx-ext "sse"}
         maps))

;; -----------------------------------------------------------------------------
;; Shared normalization
;; -----------------------------------------------------------------------------

(defn token-header-name
  "Return the canonical request header used for propagated consistency tokens."
  []
  "x-gesso-live-consistency-token")

(defn event-name
  "Normalize an app-facing event reference to an SSE event name.

   Examples:
     :live-update   => \"live-update\"
     'live-update   => \"live-update\"
     \"live-update\" => \"live-update\"
     nil            => \"live-update\"
     \"\"            => \"live-update\"

   Blank values fall back to default-event."
  [event]
  (let [s (cond
            (keyword? event) (name event)
            (symbol? event)  (name event)
            (nil? event)     default-event
            :else            (str event))]
    (if (str/blank? s)
      default-event
      s)))

(defn normalize-target
  "Normalize an hx-target value.

   - nil stays nil
   - common selector-like values are left unchanged
   - bare ids become CSS id selectors"
  [target]
  (cond
    (nil? target)
    nil

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

(defn sse-trigger
  "Return an HTMX SSE trigger string for event.

   Examples:
     (sse-trigger \"toast\") => \"sse:toast\"
     (sse-trigger :toast)   => \"sse:toast\""
  [event]
  (str "sse:" (event-name event)))

;; -----------------------------------------------------------------------------
;; Jitter helpers
;; -----------------------------------------------------------------------------

(defn random-jitter-delay-ms
  "Return a static per-render jitter delay.

   If jitter-ms is positive, returns an integer in [0, jitter-ms].
   Otherwise returns nil.

   This is intentionally static per render. Per-event randomized jitter would
   require a small client-side helper."
  [jitter-ms]
  (when (and (int? jitter-ms)
             (pos? jitter-ms))
    (rand-int (inc jitter-ms))))

(defn delay-modifier
  "Return an HTMX delay modifier, or nil for nil/zero delays."
  [delay-ms]
  (when (and (int? delay-ms)
             (pos? delay-ms))
    (str " delay:" delay-ms "ms")))

(defn trigger-with-delay
  "Attach an HTMX delay modifier to one trigger event."
  [trigger delay-ms]
  (str trigger (or (delay-modifier delay-ms) "")))

(defn effective-jitter-delay-ms
  "Return the caller-provided deterministic delay or a random static delay.

   :jitter-delay-ms is useful for deterministic tests.
   :jitter-ms chooses a static random delay during rendering."
  [{:keys [jitter-ms jitter-delay-ms]}]
  (if (some? jitter-delay-ms)
    jitter-delay-ms
    (random-jitter-delay-ms jitter-ms)))

;; -----------------------------------------------------------------------------
;; Live fragment refresh helpers
;; -----------------------------------------------------------------------------

(defn fragment-root-attrs
  "Build attrs for the outer live wrapper.

   Options:
   - :stream-url
   - :attrs merged last for ordinary attrs

   :hx-ext is composed rather than overwritten, so caller attrs like
   {:hx-ext \"path-deps\"} become \"sse, path-deps\"."
  [{:keys [attrs] :as opts}]
  (let [stream-url (require-non-blank! opts :stream-url "SSE stream URL")]
    (merge-sse-attrs
     {:sse-connect stream-url}
     attrs)))

(defn fragment-trigger
  "Build the canonical live fragment trigger string.

   Options:
   - :event
   - :trigger
   - :jitter-ms
   - :jitter-delay-ms

   Initial load is not delayed. Only the SSE-triggered refresh receives the
   delay modifier.

   :jitter-delay-ms is useful for deterministic tests.
   :jitter-ms chooses a static random delay during rendering."
  [{:keys [event trigger] :as opts
    :or {event default-event
         trigger default-fragment-trigger}}]
  (let [delay-ms (effective-jitter-delay-ms opts)]
    (str trigger
         ", "
         (trigger-with-delay (sse-trigger event) delay-ms))))

(defn fragment-target-attrs
  "Build attrs for a live fragment refresh target.

   Options:
   - :id
   - :src
   - :event
   - :swap
   - :trigger
   - :jitter-ms
   - :jitter-delay-ms
   - :attrs merged last"
  [{:keys [event swap trigger attrs] :as opts
    :or {event default-event
         swap default-fragment-swap
         trigger default-fragment-trigger}}]
  (let [id  (require-non-blank! opts :id "Fragment id")
        src (require-non-blank! opts :src "Fragment source URL")]
    (merge-attrs
     {:id id
      :hx-get src
      :hx-trigger (fragment-trigger
                   (assoc opts
                          :event event
                          :trigger trigger))
      :hx-swap swap}
     attrs)))

;; -----------------------------------------------------------------------------
;; POST helper attrs
;; -----------------------------------------------------------------------------

(defn post-form-attrs
  "Build standard attrs for a POST form that refreshes a target fragment.

   Required:
   - :to

   Optional:
   - :target
   - :swap defaults to \"innerHTML\"
   - :attrs merged last"
  [{:keys [target swap attrs] :as opts
    :or {swap default-post-swap}}]
  (let [to (require-non-blank! opts :to "POST URL")]
    (merge-attrs
     {:method "post"
      :action to
      :hx-post to
      :hx-swap swap}
     (when-let [target' (normalize-target target)]
       {:hx-target target'})
     attrs)))

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
   - :attrs merged last for ordinary attrs

   :hx-ext is composed rather than overwritten."
  [{:keys [id attrs] :as opts}]
  (let [stream-url (require-non-blank! opts :stream-url "SSE stream URL")]
    (merge-sse-attrs
     (cond-> {:sse-connect stream-url
              :aria-hidden "true"}
       id (assoc :id id))
     attrs)))

(defn sse-callback-trigger-attrs
  "Attrs for a child element that reacts to an SSE event by making an HTMX
   request.

   This follows the parent SSE connection + child hx-trigger pattern.

   Options:
   - :event
   - exactly one of :get or :post
   - :target
   - :swap defaults to \"none\"
   - :jitter-ms
   - :jitter-delay-ms
   - :attrs merged last

   Supplying both :get and :post throws.
   Supplying neither :get nor :post throws."
  [{:keys [event get post target swap attrs] :as opts
    :or {event default-event
         swap default-sse-callback-swap}}]
  (ensure-one-method! get post)
  (let [delay-ms (effective-jitter-delay-ms opts)]
    (merge-attrs
     (cond-> {:hx-trigger (trigger-with-delay (sse-trigger event) delay-ms)
              :hx-swap swap}
       get    (assoc :hx-get get)
       post   (assoc :hx-post post)
       target (assoc :hx-target (normalize-target target)))
     attrs)))

(defn sse-callback
  "Render a parent SSE connection and a child HTMX callback trigger.

   This is useful when an SSE event should wake the browser and cause it to fetch
   ordinary server-rendered HTML, usually OOB fragments or side-effect responses.

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
   - :jitter-ms
   - :jitter-delay-ms
   - :trigger-attrs"
  [{:keys [trigger-attrs] :as opts}]
  [:div (sse-callback-root-attrs opts)
   [:div (sse-callback-trigger-attrs
          (assoc opts :attrs trigger-attrs))]])

;; -----------------------------------------------------------------------------
;; Direct SSE swap / OOB helpers
;; -----------------------------------------------------------------------------

(defn sse-swap-attrs
  "Attrs for an element that directly swaps SSE event payloads.

   This is useful for direct SSE payload delivery where the SSE event data is
   intended to be treated as HTML.

   For direct OOB delivery, the payload is expected to contain hx-swap-oob
   content. In that case, use :swap \"none\" so normal content is not inserted
   into the listener element while OOB swaps can be processed.

   This direct-OOB behavior must be verified with browser/integration tests
   before app code depends on it.

   Options:
   - :id
   - :stream-url
   - :event
   - :swap defaults to \"none\"
   - :attrs merged last for ordinary attrs

   :hx-ext is composed rather than overwritten."
  [{:keys [id event swap attrs] :as opts
    :or {event default-event
         swap default-sse-direct-swap}}]
  (let [stream-url (require-non-blank! opts :stream-url "SSE stream URL")]
    (merge-sse-attrs
     (cond-> {:sse-connect stream-url
              :sse-swap (event-name event)
              :hx-swap swap
              :aria-hidden "true"
              :data-gesso-live-direct-sse true
              :data-gesso-live-direct-sse-experimental true}
       id (assoc :id id))
     attrs)))

(defn sse-swap-listener
  "Render an invisible direct SSE swap listener.

   This helper is transport-markup only. Direct payload behavior must be covered
   by browser/integration tests."
  [opts]
  [:div (sse-swap-attrs opts)])

(defn sse-oob-listener
  "Render an invisible direct SSE OOB listener.

   The incoming SSE data is expected to contain HTMX OOB markup.

   This helper is intentionally marked with data attributes so tests and app code
   can identify that it is using the direct SSE/OOB path.

   This path remains experimental until browser/integration tests prove that
   HTMX processes hx-swap-oob content from SSE payloads as expected."
  [opts]
  (let [marker-attrs {:data-gesso-live-oob-listener true
                      :data-gesso-live-oob-experimental true}
        opts'        (update opts :attrs #(merge % marker-attrs))]
    (sse-swap-listener
     (merge {:swap "none"} opts'))))
