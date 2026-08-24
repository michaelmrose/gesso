(ns gesso.live.browser.dom
  "Physical DOM primitives for Gesso Live.

   This namespace is deliberately below browser semantic policy. It may inspect,
   clone, parse, sanitize, copy, and replace DOM nodes, but it does not decide:

   - whether markup is authoritative or provisional
   - whether one revision/basis supersedes another
   - whether an optimistic execution may recover
   - which optimistic template or scope is semantically applicable
   - whether a stale browser callback still has authority
   - when continuity, settlement, or fragment lifecycle work should occur

   Those decisions belong to gesso.live.browser.adapter, the thin browser/core
   boundary, and gesso.live.optimistic policy. Host DOM objects returned from this
   namespace are physical values and must never enter portable AdapterState."
  (:require
   [clojure.string :as str]))

;; =============================================================================
;; Public identity
;; =============================================================================

(def dom-version 2)

(def structural-snapshot-type
  :gesso.live.browser.dom/structural-snapshot)

;; DOM Standard nodeType constants. Keeping these local makes the small physical
;; layer usable in controlled non-browser hosts without requiring a global Node
;; constructor merely to recognize an element or text node.
(def ^:private element-node-type 1)
(def ^:private text-node-type 3)

;; =============================================================================
;; Errors
;; =============================================================================

(defn- dom-error
  [kind message data]
  (ex-info
   message
   (merge
    {:error/type :gesso.live.browser.dom/error
     :error/kind kind}
    data)))

(defn- throw-dom!
  [kind message data]
  (throw (dom-error kind message data)))

;; =============================================================================
;; Attribute names
;; =============================================================================

(defn attr-name
  "Return an HTML attribute name from a keyword, symbol, string, or other value."
  [attribute]
  (cond
    (keyword? attribute) (name attribute)
    (symbol? attribute) (name attribute)
    (string? attribute) attribute
    :else (str attribute)))

;; =============================================================================
;; Node predicates and lookup
;; =============================================================================

(defn element?
  [value]
  (boolean
   (and value
        (= element-node-type
           (.-nodeType value)))))

(defn template?
  [value]
  (boolean
   (and (element? value)
        (= "TEMPLATE"
           (.-tagName value)))))

(defn connected?
  [node]
  (boolean
   (and node
        (.-isConnected node))))

(defn by-id
  "Return an element by DOM id.

   The one-argument form uses the current browser document. The two-argument
   form accepts an explicit document-like host for tests and embedded runtimes."
  ([id]
   (by-id js/document id))
  ([document id]
   (when (and document
              (string? id)
              (not (str/blank? id)))
     (.getElementById document id))))

(defn query-one
  "Run querySelector safely.

   Invalid selectors, blank selectors, and missing roots return nil."
  ([selector]
   (query-one js/document selector))
  ([root selector]
   (when (and root
              (string? selector)
              (not (str/blank? selector)))
     (try
       (.querySelector root selector)
       (catch :default _
         nil)))))

(defn query-all
  "Run querySelectorAll safely and return a vector.

   Invalid selectors, blank selectors, and missing roots return an empty vector."
  ([selector]
   (query-all js/document selector))
  ([root selector]
   (if (and root
            (string? selector)
            (not (str/blank? selector)))
     (try
       (vec
        (array-seq
         (.querySelectorAll root selector)))
       (catch :default _
         []))
     [])))

(defn require-element!
  "Return element when it is a DOM element, otherwise throw."
  ([element]
   (require-element!
    "Expected a Gesso Live DOM element."
    element
    nil))
  ([message element data]
   (when-not (element? element)
     (throw-dom!
      :missing-element
      message
      (merge
       {:element element}
       (or data {}))))
   element))

(defn require-connected!
  "Return element when it is a connected DOM element, otherwise throw."
  ([element]
   (require-connected! element nil))
  ([element data]
   (require-element! element)
   (when-not (connected? element)
     (throw-dom!
      :detached-target
      "Gesso Live DOM target is no longer connected."
      (merge
       {:element element}
       (or data {}))))
   element))

(defn require-query-one!
  "Resolve selector below root and throw when no element exists.

   Invalid selectors and missing targets intentionally share one physical lookup
   failure. Optional lookup should use query-one instead."
  ([selector]
   (require-query-one! js/document selector))
  ([root selector]
   (or (query-one root selector)
       (throw-dom!
        :missing-target
        "Gesso Live DOM target could not be resolved."
        {:selector selector
         :root root}))))

(defn matches?
  "Return true when element matches selector. Invalid selectors return false."
  [element selector]
  (boolean
   (and (element? element)
        (string? selector)
        (not (str/blank? selector))
        (try
          (.matches element selector)
          (catch :default _
            false)))))

(defn closest
  "Return nearest element matching selector. Invalid selectors return nil."
  [element selector]
  (when (and (element? element)
             (string? selector)
             (not (str/blank? selector)))
    (try
      (.closest element selector)
      (catch :default _
        nil))))

(defn closest-with-attr
  "Return the nearest ancestor-or-self carrying attribute."
  [element attribute]
  (let [attribute-name (attr-name attribute)]
    (loop [node element]
      (cond
        (not (element? node))
        nil

        (.hasAttribute node attribute-name)
        node

        :else
        (recur (.-parentElement node))))))

(defn contains-node?
  "Return true when root is node or contains node."
  [root node]
  (boolean
   (and root
        node
        (or (identical? root node)
            (.contains root node)))))

;; =============================================================================
;; Attribute helpers
;; =============================================================================

(defn attr
  "Read an element attribute. Missing attributes/non-elements return nil."
  [element attribute]
  (when (element? element)
    (.getAttribute element
                   (attr-name attribute))))

(defn has-attr?
  [element attribute]
  (boolean
   (and (element? element)
        (.hasAttribute element
                       (attr-name attribute)))))

(defn set-attr!
  "Set attribute and return element. nil removes the attribute."
  [element attribute value]
  (when (element? element)
    (if (nil? value)
      (.removeAttribute element
                        (attr-name attribute))
      (.setAttribute element
                     (attr-name attribute)
                     (str value))))
  element)

(defn remove-attr!
  "Remove attribute and return element. Non-elements are ignored."
  [element attribute]
  (when (element? element)
    (.removeAttribute element
                      (attr-name attribute)))
  element)

(defn truthy-attr?
  "Interpret a marker-style HTML attribute.

   Presence is true except for explicit false-like values: false, 0, and no
   (case-insensitive). Missing attributes return nil."
  [element attribute]
  (when (has-attr? element attribute)
    (let [value (some-> (attr element attribute)
                        str/lower-case)]
      (not
       (contains? #{"false" "0" "no"}
                  value)))))

(defn attributes
  "Return element attributes as a vector of [name value] pairs."
  [element]
  (require-element!
   "Gesso Live attribute capture requires an element."
   element
   {:operation :attributes})
  (mapv
   (fn [attribute]
     [(.-name attribute)
      (.-value attribute)])
   (array-seq
    (.-attributes element))))

(defn restore-attributes!
  "Replace all target attributes with attribute-pairs and return target."
  [target attribute-pairs]
  (require-element!
   "Gesso Live attribute restoration requires an element target."
   target
   {:operation :restore-attributes})
  (doseq [attribute
          (vec
           (array-seq
            (.-attributes target)))]
    (.removeAttribute target
                      (.-name attribute)))
  (doseq [[name value] attribute-pairs]
    (.setAttribute target
                   (str name)
                   (str value)))
  target)

(defn remove-attrs!
  "Remove attributes from element and return element."
  [element attribute-names]
  (require-element!
   "Gesso Live attribute removal requires an element."
   element
   {:operation :remove-attrs})
  (doseq [attribute attribute-names]
    (remove-attr! element attribute))
  element)

(defn remove-attrs-deep!
  "Remove the supplied attributes from element and all descendant elements.

   The caller owns the policy deciding which attributes are transport-only,
   provisional, or otherwise unwanted; this function only performs mutation."
  [element attribute-names]
  (require-element!
   "Gesso Live deep attribute removal requires an element."
   element
   {:operation :remove-attrs-deep})
  (remove-attrs! element attribute-names)
  (doseq [descendant (query-all element "*")]
    (remove-attrs! descendant attribute-names))
  element)

;; =============================================================================
;; Root shape and parsing
;; =============================================================================

(defn tag-name
  "Return the DOM's normalized upper-case tag name for an element."
  [element]
  (when (element? element)
    (.-tagName element)))

(defn same-root-tag?
  [left right]
  (boolean
   (and (element? left)
        (element? right)
        (= (tag-name left)
           (tag-name right)))))

(defn require-same-root-tag!
  "Require two element roots to have the same tag.

   This is a mechanical requirement for in-place copying because an existing DOM
   object's tag name cannot be changed. Whole-node replacement does not impose
   this restriction."
  [target source]
  (require-element!
   "Gesso Live in-place copy requires an element target."
   target
   {:operation :copy-element-into
    :role :target})
  (require-element!
   "Gesso Live in-place copy requires an element source."
   source
   {:operation :copy-element-into
    :role :source})
  (when-not (same-root-tag? target source)
    (throw-dom!
     :incompatible-copy-root
     "Gesso Live cannot copy a different root tag into an existing DOM element."
     {:target-tag (tag-name target)
      :source-tag (tag-name source)}))
  source)

(defn- meaningful-child-nodes
  [parent]
  (->> (array-seq (.-childNodes parent))
       (remove
        (fn [node]
          (and (= text-node-type
                  (.-nodeType node))
               (str/blank?
                (or (.-textContent node)
                    "")))))
       vec))

(defn one-element-root
  "Return the single element root contained by parent.

   Whitespace-only text nodes are ignored. Any other text, comment, or multi-root
   content is rejected."
  [parent]
  (when-not parent
    (throw-dom!
     :missing-parent
     "Gesso Live one-root parsing requires a parent node."
     {}))
  (let [nodes (meaningful-child-nodes parent)]
    (when-not (and (= 1 (count nodes))
                   (element? (first nodes)))
      (throw-dom!
       :not-one-root
       "Gesso Live markup must contain exactly one element root."
       {:child-count (count nodes)}))
    (first nodes)))

(defn template-root
  "Return a deep detached clone of a template's one element root."
  [template]
  (when-not (template? template)
    (throw-dom!
     :not-template
     "Gesso Live template source did not resolve to a template element."
     {:node template}))
  (.cloneNode
   (one-element-root (.-content template))
   true))

(defn parse-one-root
  "Parse HTML text and return one detached element root.

   The returned node belongs to document and is not installed. The two-argument
   form accepts an explicit document-like host."
  ([html]
   (parse-one-root js/document html))
  ([document html]
   (when-not (string? html)
     (throw-dom!
      :invalid-html
      "Gesso Live HTML input must be a string."
      {:value html}))
   (when-not document
     (throw-dom!
      :missing-document
      "Gesso Live HTML parsing requires a document."
      {}))
   (let [template (.createElement document "template")]
     (set! (.-innerHTML template) html)
     (.cloneNode
      (one-element-root (.-content template))
      true))))

;; =============================================================================
;; Structural snapshots
;; =============================================================================

(defn snapshot
  "Capture a detached structural clone of element.

   This is a physical resource only. It contains no authority, basis, scope,
   revision, generation, or recovery permission. Ownership/authorization must be
   tracked outside the DOM object by the browser adapter/shell."
  [element]
  (require-element!
   "Gesso Live structural snapshot requires an element."
   element
   {:operation :snapshot})
  {:gesso.live.browser.dom/type structural-snapshot-type
   :node (.cloneNode element true)})

(defn snapshot?
  [value]
  (boolean
   (and (map? value)
        (= structural-snapshot-type
           (:gesso.live.browser.dom/type value))
        (element? (:node value)))))

(defn require-snapshot!
  [value]
  (when-not (snapshot? value)
    (throw-dom!
     :invalid-snapshot
     "Invalid Gesso Live structural snapshot."
     {:snapshot value}))
  value)

(defn snapshot-node
  "Return a fresh deep clone from structural snapshot."
  [snapshot]
  (require-snapshot! snapshot)
  (.cloneNode (:node snapshot) true))

;; =============================================================================
;; Physical in-place copying
;; =============================================================================

(defn replace-children-from!
  "Replace target children with deep clones of source children."
  [target source]
  (require-element!
   "Gesso Live child copying requires an element target."
   target
   {:operation :replace-children
    :role :target})
  (require-element!
   "Gesso Live child copying requires an element source."
   source
   {:operation :replace-children
    :role :source})
  (while (.-firstChild target)
    (.removeChild target
                  (.-firstChild target)))
  (doseq [child
          (array-seq
           (.-childNodes source))]
    (.appendChild target
                  (.cloneNode child true)))
  target)

(defn copy-element-into!
  "Copy source attributes and children into the existing target DOM object.

   The root tags must match because DOM tag names are immutable. No semantic
   identity or authority checks occur here: ids, data attributes, optimistic
   markers, and canonical markers are copied exactly like any other attributes.
   Policy that forbids an id change or requires a canonical marker must run before
   this physical operation."
  [target source]
  (require-same-root-tag! target source)
  (restore-attributes! target (attributes source))
  (replace-children-from! target source)

  ;; DETAILS.open is reflected by the open attribute in browsers, but assign the
  ;; property as well so controlled/fake DOM hosts observe the same physical state.
  (when (= "DETAILS" (tag-name target))
    (set! (.-open target)
          (boolean (.-open source))))
  target)

;; =============================================================================
;; Whole-node replacement
;; =============================================================================

(defn replace!
  "Replace current with replacement and return the installed replacement.

   Unlike in-place copying, ordinary DOM replacement may change the root tag or
   DOM id. Whether a particular Live/optimistic protocol permits such a change is
   policy and must be decided by that caller, not by this mechanical primitive."
  [current replacement]
  (require-element!
   "Gesso Live replacement requires an element current node."
   current
   {:operation :replace
    :role :current})
  (require-element!
   "Gesso Live replacement requires an element replacement node."
   replacement
   {:operation :replace
    :role :replacement})
  (let [parent (.-parentNode current)]
    (when-not parent
      (throw-dom!
       :detached-target
       "Gesso Live cannot replace a node that no longer has a parent."
       {:current current}))
    (.replaceChild parent replacement current)
    replacement))

(defn restore-snapshot!
  "Physically replace current with a fresh clone from structural snapshot.

   This function intentionally performs no recovery-authorization check. A caller
   must establish that restoration is still semantically permitted before invoking
   it."
  [current snapshot]
  (replace! current
            (snapshot-node snapshot)))
