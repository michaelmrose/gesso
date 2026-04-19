(ns gesso.live.htmx
  "HTMX-facing helpers for gesso.live.

   This namespace owns:
   - wrapper attrs for the live transport connection
   - target attrs for HTMX refresh
   - the canonical request header used for propagated consistency tokens
   - low-level HTMX attribute builders for live POST helpers."
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
  (str trigger ", sse:" event))

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
