(ns gesso.htmx
  "Generic HTMX helpers.

   This namespace is for HTMX helpers that are not specifically live-related,
   validation-related, or component-specific.

   Current focus:
   - small constructors for HTMX out-of-band fragments."
  (:require
   [clojure.string :as str]
   [gesso.util :refer [merge-attrs]]))

;; -----------------------------------------------------------------------------
;; HTMX swap values
;; -----------------------------------------------------------------------------

(def swap-aliases
  {:true         "true"
   true          "true"

   :outer-html   "outerHTML"
   :outerHTML    "outerHTML"
   :replace      "outerHTML"

   :inner-html   "innerHTML"
   :innerHTML    "innerHTML"

   :text-content "textContent"
   :textContent  "textContent"

   :before-begin "beforebegin"
   :beforebegin  "beforebegin"

   :after-begin  "afterbegin"
   :afterbegin   "afterbegin"

   :before-end   "beforeend"
   :beforeend    "beforeend"

   :after-end    "afterend"
   :afterend     "afterend"

   :delete       "delete"
   :none         "none"})

(defn swap-value
  "Normalize an HTMX swap value.

   Examples:
     :outer-html   => \"outerHTML\"
     :inner-html   => \"innerHTML\"
     :beforeend    => \"beforeend\"
     \"beforeend\" => \"beforeend\""
  [swap]
  (cond
    (nil? swap)
    "outerHTML"

    (contains? swap-aliases swap)
    (get swap-aliases swap)

    (keyword? swap)
    (name swap)

    (symbol? swap)
    (name swap)

    :else
    (str swap)))

(defn oob-swap-value
  "Normalize an hx-swap-oob value.

   If selector is supplied, append it with HTMX's selector form:

     beforeend:#target"
  ([swap]
   (swap-value swap))
  ([swap selector]
   (let [swap' (swap-value swap)]
     (if (some? selector)
       (str swap' ":" selector)
       swap'))))

;; -----------------------------------------------------------------------------
;; Target/id helpers
;; -----------------------------------------------------------------------------

(defn target-id
  "Normalize a target id for id-based OOB swaps.

   Accepts:
     \"app-toaster\"
     \"#app-toaster\"
     :app-toaster

   Returns:
     \"app-toaster\""
  [target]
  (let [s (cond
            (keyword? target) (name target)
            (symbol? target)  (name target)
            :else            (str target))
        s (if (str/starts-with? s "#")
            (subs s 1)
            s)]
    (when (str/blank? s)
      (throw (ex-info "HTMX OOB target id cannot be blank."
                      {:target target})))
    s))

(defn oob-attrs
  "Build attrs for an HTMX OOB fragment.

   Options:
     :id        target id, bare or #prefixed
     :target    alias for :id
     :swap      defaults to :outer-html
     :selector  optional HTMX OOB selector suffix
     :attrs     merged last

   Examples:
     (oob-attrs {:id \"count\" :swap :inner-html})
     => {:id \"count\" :hx-swap-oob \"innerHTML\"}

     (oob-attrs {:swap :beforeend :selector \"#items\"})
     => {:hx-swap-oob \"beforeend:#items\"}"
  [{:keys [id target swap selector attrs]
    :or {swap :outer-html}}]
  (let [id'  (or id target)
        base (cond-> {:hx-swap-oob (oob-swap-value swap selector)}
               id' (assoc :id (target-id id')))]
    (merge-attrs base attrs)))

;; -----------------------------------------------------------------------------
;; Hiccup node attrs
;; -----------------------------------------------------------------------------

(defn- merge-node-attrs
  [node attrs]
  (when-not (vector? node)
    (throw (ex-info "Expected a Hiccup vector node."
                    {:node node
                     :attrs attrs})))
  (let [[tag & rest] node
        [existing-attrs children] (if (map? (first rest))
                                    [(first rest) (rest rest)]
                                    [{} rest])]
    (into [tag (merge-attrs existing-attrs attrs)]
          children)))

(defn with-oob
  "Attach hx-swap-oob attrs to an existing Hiccup node.

   Arity 2:
     (with-oob node :outer-html)

   Arity 3:
     (with-oob \"target-id\" :outer-html node)

   The 3-arity form also ensures the node has the target id."
  ([node swap]
   (merge-node-attrs node (oob-attrs {:swap swap})))
  ([target swap node]
   (merge-node-attrs node (oob-attrs {:id target
                                      :swap swap}))))

;; -----------------------------------------------------------------------------
;; OOB fragment constructors
;; -----------------------------------------------------------------------------

(defn oob-wrapper
  "Wrap children in a tag configured for an HTMX OOB swap.

   This is useful when the default :div wrapper is not appropriate, such as
   table/tbody/list cases.

   Example:
     (oob-wrapper :tbody \"rows\" :beforeend
       [:tr [:td \"New row\"]])"
  [tag target swap & children]
  (into [tag (oob-attrs {:id target
                         :swap swap})]
        children))

(defn oob-inner-html
  "Replace the innerHTML of target with children."
  [target & children]
  (apply oob-wrapper :div target :inner-html children))

(defn oob-beforeend
  "Append children to the end of target."
  [target & children]
  (apply oob-wrapper :div target :beforeend children))

(defn oob-afterbegin
  "Prepend children to the beginning of target."
  [target & children]
  (apply oob-wrapper :div target :afterbegin children))

(defn oob-beforebegin
  "Insert children before target."
  [target & children]
  (apply oob-wrapper :div target :beforebegin children))

(defn oob-afterend
  "Insert children after target."
  [target & children]
  (apply oob-wrapper :div target :afterend children))

(defn oob-delete
  "Remove target from the DOM."
  [target]
  [:div (oob-attrs {:id target
                    :swap :delete})])

(defn oob-outer-html
  "Replace target using outerHTML.

   Arity 1 expects node to already have the correct :id:
     (oob-outer-html [:div {:id \"card\"} ...])

   Arity 2 ensures node has target as :id:
     (oob-outer-html \"card\" [:div ...])"
  ([node]
   (with-oob node :outer-html))
  ([target node]
   (with-oob target :outer-html node)))

(def oob-replace
  "Alias for oob-outer-html."
  oob-outer-html)
