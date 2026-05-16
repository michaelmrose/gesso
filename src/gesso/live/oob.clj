(ns gesso.live.oob
  "HTMX out-of-band fragment helpers for gesso.live.

   This namespace builds/marks Hiccup nodes for HTMX OOB swaps.

   It provides:

   - hx-swap-oob value construction
   - helpers for marking existing Hiccup nodes as OOB fragments
   - explicit OOB operation helpers such as replace-oob, append-oob, etc.
   - small helpers for carrying opaque live/consistency metadata in data attrs

   It does not:

   - render Hiccup to strings
   - know Ring response maps
   - know SSE
   - know XTDB
   - publish invalidations
   - perform authorization

   This keeps OOB as a pure view/helper layer. Higher-level code decides when
   OOB fragments are returned."
  (:require
   [clojure.string :as str]))

;; -----------------------------------------------------------------------------
;; Constants
;; -----------------------------------------------------------------------------

(def oob-attr
  :hx-swap-oob)

(def valid-swap-styles
  #{:true
    :outerHTML
    :innerHTML
    :beforebegin
    :afterbegin
    :beforeend
    :afterend
    :delete
    :none})

(def valid-swap-style-strings
  #{"true"
    "outerHTML"
    "innerHTML"
    "beforebegin"
    "afterbegin"
    "beforeend"
    "afterend"
    "delete"
    "none"})

(def default-swap
  :true)

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn- ex
  [message data]
  (ex-info message data))

(defn- blank-string?
  [s]
  (and (string? s)
       (str/blank? s)))

(defn- require-nonblank!
  [k value]
  (when (or (nil? value)
            (blank-string? value))
    (throw
     (ex (str "gesso.live OOB " k " must not be blank.")
         {k value})))
  value)

(defn- normalize-name
  [x]
  (cond
    (keyword? x)
    (name x)

    (symbol? x)
    (name x)

    (string? x)
    x

    :else
    (str x)))

(defn- safe-id-value?
  "Return true for an acceptable explicit HTML id value.

   This intentionally does not enforce a CSS identifier regex. HTML ids may
   start with digits, and UUID-style ids are common in application code.

   We only reject nil and blank/whitespace-only strings."
  [s]
  (and (string? s)
       (not (str/blank? s))))

(defn- require-safe-id!
  [id]
  (let [id'  (require-nonblank! :id (normalize-name id))
        id'' (if (str/starts-with? id' "#")
               (subs id' 1)
               id')]
    (when-not (safe-id-value? id'')
      (throw
       (ex "gesso.live OOB id must be a non-blank string."
           {:id id
            :normalized id'})))
    id''))

(defn- hiccup-node?
  [x]
  (and (vector? x)
       (seq x)
       (or (keyword? (first x))
           (symbol? (first x))
           (string? (first x)))))

(defn- split-node
  [node]
  (when-not (hiccup-node? node)
    (throw
     (ex "gesso.live OOB expected a Hiccup node vector."
         {:node node})))
  (let [[tag & xs] node]
    (if (map? (first xs))
      [tag (first xs) (rest xs)]
      [tag {} xs])))

(defn- rebuild-node
  [tag attrs children]
  (into [tag attrs] children))

(defn- update-attrs
  [node f & args]
  (let [[tag attrs children] (split-node node)]
    (rebuild-node tag
                  (apply f attrs args)
                  children)))

(defn- assoc-attr
  [node k v]
  (update-attrs node assoc k v))

(defn- dissoc-attr
  [node k]
  (update-attrs node dissoc k))

;; -----------------------------------------------------------------------------
;; Selectors and swap values
;; -----------------------------------------------------------------------------

(defn id-selector
  "Return a CSS id selector for an id.

   Examples:

     (id-selector \"request-1\")
     => \"#request-1\"

     (id-selector :request-1)
     => \"#request-1\"

     (id-selector \"123e4567-e89b-12d3-a456-426614174000\")
     => \"#123e4567-e89b-12d3-a456-426614174000\"

   If the input already starts with #, the id part is normalized.

   This helper intentionally does not enforce an artificial HTML id regex.
   It only rejects nil and blank values.

   For complex selectors, use selector instead."
  [id]
  (str "#" (require-safe-id! id)))

(defn selector
  "Normalize a CSS selector value.

   Strings are used as-is. Keywords/symbols are converted with name.

   Unlike id-selector, this function does not add # and intentionally permits
   complex selectors."
  [value]
  (require-nonblank! :selector (normalize-name value)))

(defn normalize-swap
  "Normalize a swap style.

   Accepted values:
     true
     :true
     :outerHTML
     :innerHTML
     :beforebegin
     :afterbegin
     :beforeend
     :afterend
     :delete
     :none
     string/symbol equivalents

   `true` and :true become \"true\"."
  [swap]
  (let [swap' (if (nil? swap) default-swap swap)]
    (cond
      (true? swap')
      "true"

      (= :true swap')
      "true"

      (keyword? swap')
      (do
        (when-not (contains? valid-swap-styles swap')
          (throw
           (ex "Unsupported gesso.live OOB swap style."
               {:swap swap'
                :valid valid-swap-styles})))
        (name swap'))

      (symbol? swap')
      (normalize-swap (keyword (name swap')))

      (string? swap')
      (let [s (require-nonblank! :swap swap')]
        (when-not (contains? valid-swap-style-strings s)
          (throw
           (ex "Unsupported gesso.live OOB swap style."
               {:swap swap'
                :valid valid-swap-style-strings})))
        s)

      :else
      (throw
       (ex "Unsupported gesso.live OOB swap style."
           {:swap swap'
            :type (type swap')})))))

(defn swap-value
  "Build an hx-swap-oob value.

   Examples:

     (swap-value)
     => \"true\"

     (swap-value :outerHTML \"#request-panel\")
     => \"outerHTML:#request-panel\"

     (swap-value :beforeend \"#messages\")
     => \"beforeend:#messages\"

   If target is supplied with true/:true, the swap is normalized to outerHTML
   because HTMX's target form uses a swap strategy plus selector."
  ([] (swap-value default-swap nil))
  ([swap] (swap-value swap nil))
  ([swap target]
   (let [swap' (normalize-swap swap)]
     (if target
       (let [strategy (if (= "true" swap') "outerHTML" swap')]
         (str strategy ":" (selector target)))
       swap'))))

(defn oob-attrs
  "Return attrs for an HTMX out-of-band swap."
  ([] (oob-attrs nil))
  ([{:keys [swap target]}]
   {oob-attr (swap-value swap target)}))

;; -----------------------------------------------------------------------------
;; Marking Hiccup nodes
;; -----------------------------------------------------------------------------

(defn with-oob
  "Mark an existing Hiccup node as an HTMX out-of-band fragment.

   Options:
     :swap
       Swap strategy. Defaults to true.

     :target
       Optional CSS selector target. When supplied, the resulting attribute is
       strategy:selector.

   Example:

     (with-oob [:div {:id \"panel\"} \"Hi\"] {:swap :outerHTML})
     => [:div {:id \"panel\" :hx-swap-oob \"outerHTML\"} \"Hi\"]"
  ([node]
   (with-oob node nil))
  ([node options]
   (update-attrs node merge (oob-attrs options))))

(defn without-oob
  "Remove hx-swap-oob from a Hiccup node."
  [node]
  (dissoc-attr node oob-attr))

(defn oob?
  "Return true when a Hiccup node has an hx-swap-oob attr."
  [node]
  (let [[_tag attrs _children] (split-node node)]
    (contains? attrs oob-attr)))

(defn oob-value
  "Return the hx-swap-oob value from a Hiccup node."
  [node]
  (let [[_tag attrs _children] (split-node node)]
    (get attrs oob-attr)))

(defn ensure-id
  "Ensure a Hiccup node has an explicit :id attr.

   This does not parse id shorthand from tags such as :div#foo; it only checks
   the explicit attr map. Prefer explicit ids for OOB fragments."
  [node id]
  (assoc-attr node :id (require-safe-id! id)))

;; -----------------------------------------------------------------------------
;; Common OOB operations
;; -----------------------------------------------------------------------------

(defn replace-oob
  "Replace the matching element.

   With no target, HTMX uses the node's own id. With target, emits
   outerHTML:target."
  ([node]
   (with-oob node {:swap :outerHTML}))
  ([target node]
   (with-oob node {:swap :outerHTML
                   :target target})))

(defn inner-oob
  "Replace the innerHTML of target."
  [target node]
  (with-oob node {:swap :innerHTML
                  :target target}))

(defn append-oob
  "Append node inside target."
  [target node]
  (with-oob node {:swap :beforeend
                  :target target}))

(defn prepend-oob
  "Prepend node inside target."
  [target node]
  (with-oob node {:swap :afterbegin
                  :target target}))

(defn before-oob
  "Insert node before target."
  [target node]
  (with-oob node {:swap :beforebegin
                  :target target}))

(defn after-oob
  "Insert node after target."
  [target node]
  (with-oob node {:swap :afterend
                  :target target}))

(defn delete-oob
  "Delete target using an OOB delete swap.

   HTMX delete OOB swaps do not need meaningful children, but the response still
   needs an element carrying hx-swap-oob."
  [target]
  [:div (oob-attrs {:swap :delete
                    :target target})])

(defn no-op-oob
  "Return an OOB no-op marker.

   Useful in tests or conditional render branches where a Hiccup node is
   expected but no DOM mutation should occur."
  []
  [:div (oob-attrs {:swap :none})])

;; -----------------------------------------------------------------------------
;; Fragment collections
;; -----------------------------------------------------------------------------

(defn fragments
  "Return a vector of OOB fragments after dropping nil values.

   This intentionally does not wrap the fragments in a parent element, because
   callers/renderers differ in how they handle top-level fragments."
  [& nodes]
  (into [] (remove nil?) nodes))

(defn ensure-all-oob!
  "Validate that all nodes in a collection are marked OOB.

   Returns the original collection."
  [nodes]
  (doseq [node nodes]
    (when-not (oob? node)
      (throw
       (ex "Expected every gesso.live OOB fragment to have hx-swap-oob."
           {:node node}))))
  nodes)

(defn only-oob
  "Return only nodes marked with hx-swap-oob."
  [nodes]
  (filterv oob? nodes))

;; -----------------------------------------------------------------------------
;; Opaque metadata attrs
;; -----------------------------------------------------------------------------

(defn with-live-id
  "Attach an opaque live id as a data attr.

   This is only metadata for callers/tests/debugging. It does not participate in
   subscription matching."
  [node live-id]
  (assoc-attr node :data-gesso-live-id (normalize-name live-id)))

(defn with-consistency-token
  "Attach an opaque consistency token string as a data attr.

   This helper expects a browser-safe opaque token string. Do not place raw XTDB2
   consistency maps here; wrap/sign/encode them first in token.clj or a
   browser-token bridge."
  [node token]
  (assoc-attr node :data-gesso-consistency-token
              (require-nonblank! :consistency-token token)))

(defn with-trigger
  "Attach an HX-Trigger header-equivalent marker as data.

   This does not set response headers. It is only useful for Hiccup-level
   metadata or client-side hooks."
  [node event-name]
  (assoc-attr node :data-gesso-trigger (normalize-name event-name)))
