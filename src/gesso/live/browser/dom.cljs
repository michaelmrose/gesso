(ns gesso.live.browser.dom
  "Small browser DOM primitives used by Gesso Live.

   This namespace owns mechanics, not lifecycle policy. It may locate, inspect,
   clone, sanitize, and replace DOM nodes, but it does not decide when an
   optimistic execution should reconcile, recover, settle, or restore
   continuity.

   Canonical authority is explicit. A node is authoritative only when it carries
   the shared Gesso canonical marker; matching scope metadata alone is not
   sufficient."
  (:require
   [clojure.string :as str]
   [gesso.live.optimistic.protocol :as protocol]))

;; -----------------------------------------------------------------------------
;; Attribute names
;; -----------------------------------------------------------------------------

(defn attr-name
  "Return an HTML attribute name from a keyword or string protocol constant."
  [attr]
  (cond
    (keyword? attr) (name attr)
    (string? attr) attr
    :else (str attr)))

(def ^:private canonical-attr-name
  (attr-name protocol/canonical-attr))

(def ^:private scope-attr-name
  (attr-name protocol/scope-attr))

(def ^:private revision-attr-name
  (attr-name protocol/revision-attr))

(def ^:private template-attr-name
  (attr-name protocol/template-attr))


(def transport-only-attrs
  "Attributes meaningful only while transporting markup into the document.

   These must not survive when raw response markup is promoted into installed
   canonical DOM."
  #{"hx-swap-oob"
    "data-hx-swap-oob"})

;; -----------------------------------------------------------------------------
;; Node predicates and lookup
;; -----------------------------------------------------------------------------

(defn element?
  [x]
  (and x
       (= (.-nodeType x)
          js/Node.ELEMENT_NODE)))

(defn template?
  [x]
  (and (element? x)
       (= "TEMPLATE" (.-tagName x))))

(defn connected?
  [node]
  (boolean
   (and node
        (.-isConnected node))))

(defn by-id
  "Return an element by DOM id."
  [id]
  (when (and (string? id)
             (not (str/blank? id)))
    (.getElementById js/document id)))

(defn query-one
  "Run querySelector safely.

   Invalid selectors return nil instead of throwing through the runtime."
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
  "Run querySelectorAll safely and return a vector."
  ([selector]
   (query-all js/document selector))
  ([root selector]
   (if (and root
            (string? selector)
            (not (str/blank? selector)))
     (try
       (vec (array-seq (.querySelectorAll root selector)))
       (catch :default _
         []))
     [])))


(defn require-element!
  "Return element when it is a DOM element, otherwise throw.

   Use this at trusted-runtime boundaries where absence is an error rather than
   a recoverable protocol disposition."
  ([element]
   (require-element! "Expected a Gesso Live DOM element." element nil))
  ([message element data]
   (when-not (element? element)
     (throw
      (ex-info
       message
       (merge
        {:error/type :gesso.live.browser.dom/missing-element
         :element element}
        (or data {})))))
   element))

(defn require-connected!
  "Return element when it is still connected to the document, otherwise throw."
  [element data]
  (require-element! element)
  (when-not (connected? element)
    (throw
     (ex-info
      "Gesso Live DOM target is no longer connected."
      (merge
       {:error/type :gesso.live.browser.dom/detached-target
        :element element}
       (or data {})))))
  element)

(defn require-query-one!
  "Resolve selector below root and throw when no element exists.

   Invalid selectors and missing targets are both explicit runtime failures;
   callers that need optional lookup should use query-one instead."
  ([selector]
   (require-query-one! js/document selector))
  ([root selector]
   (or (query-one root selector)
       (throw
        (ex-info
         "Gesso Live DOM target could not be resolved."
         {:error/type :gesso.live.browser.dom/missing-target
          :selector selector
          :root root})))))

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
  "Return the nearest element matching selector. Invalid selectors return nil."
  [element selector]
  (when (and (element? element)
             (string? selector)
             (not (str/blank? selector)))
    (try
      (.closest element selector)
      (catch :default _
        nil))))

(defn closest-with-attr
  "Return the nearest ancestor-or-self carrying attr."
  [element attr]
  (let [attr' (attr-name attr)]
    (loop [node element]
      (cond
        (not (element? node))
        nil

        (.hasAttribute node attr')
        node

        :else
        (recur (.-parentElement node))))))


;; -----------------------------------------------------------------------------
;; Attribute helpers
;; -----------------------------------------------------------------------------

(defn attr
  "Read an element attribute. Missing attributes return nil."
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
  [element attribute]
  (when (element? element)
    (.removeAttribute element
                      (attr-name attribute)))
  element)

(defn truthy-attr?
  "Interpret a marker-style HTML attribute.

   Presence with empty value, the attribute name itself, \"true\", or \"1\" is
   true. Explicit \"false\" and \"0\" are false."
  [element attribute]
  (when (has-attr? element attribute)
    (let [attribute' (attr-name attribute)
          value (some-> (attr element attribute)
                        str/lower-case)]
      (not (contains? #{"false" "0" "no"}
                      value)))))

;; -----------------------------------------------------------------------------
;; Canonical metadata
;; -----------------------------------------------------------------------------

(defn canonical?
  "True only for explicitly marked authoritative server content."
  [element]
  (boolean
   (and (element? element)
        (truthy-attr?
         element
         canonical-attr-name))))

(defn scope
  "Return opaque optimistic scope identity carried by element."
  [element]
  (attr element scope-attr-name))

(defn revision-wire
  "Return encoded semantic revision carried by element."
  [element]
  (attr element revision-attr-name))

(defn revision
  "Decode semantic revision carried by element.

   Malformed revisions throw because accepting malformed authoritative metadata
   would make stale-state protection unreliable."
  [element]
  (some-> (revision-wire element)
          protocol/wire->revision))

(defn canonical-for-scope?
  [element expected-scope]
  (and (canonical? element)
       (= expected-scope
          (scope element))))

(defn canonical-elements
  "Return all explicitly canonical elements below root, optionally restricted to
   one opaque scope identity.

   root itself is included when it is canonical."
  ([root]
   (canonical-elements root nil))
  ([root expected-scope]
   (if-not root
     []
     (let [selector
           (str "[" canonical-attr-name "]")
           descendants
           (query-all root selector)
           candidates
           (cond-> descendants
             (and (element? root)
                  (canonical? root))
             (conj root))]
       (->> candidates
            distinct
            (filter
             (fn [element]
               (or (nil? expected-scope)
                   (= expected-scope
                      (scope element)))))
            vec)))))

(defn compare-element-revisions
  "Compare semantic revisions carried by two elements.

   Opaque revisions support equality only and therefore return :incomparable
   when distinct."
  [left right]
  (protocol/compare-revisions
   (revision left)
   (revision right)))

(defn newest-canonical
  "Select a unique newest canonical candidate when ordering is knowable.

   Returns:
     {:status :none}
     {:status :selected :element e}
     {:status :ambiguous :elements [...] :reason ...}

   Distinct opaque revisions are deliberately ambiguous. The caller must apply
   protocol knowledge rather than invent lexical or arrival-order authority."
  [elements]
  (let [elements' (vec (filter canonical? elements))]
    (cond
      (empty? elements')
      {:status :none}

      (= 1 (count elements'))
      {:status :selected
       :element (first elements')}

      :else
      (loop [best (first elements')
             remaining (next elements')]
        (if-not remaining
          {:status :selected
           :element best}
          (let [candidate (first remaining)
                order
                (compare-element-revisions
                 candidate
                 best)]
            (case order
              :newer
              (recur candidate
                     (next remaining))

              :older
              (recur best
                     (next remaining))

              :same
              ;; Same semantic revision with two different canonical roots is
              ;; ambiguous unless they are literally the same node.
              (if (identical? candidate best)
                (recur best
                       (next remaining))
                {:status :ambiguous
                 :reason :duplicate-revision
                 :elements elements'})

              :incomparable
              {:status :ambiguous
               :reason :incomparable-revisions
               :elements elements'})))))))

(defn incoming-revision-status
  "Compare incoming canonical element with currently installed canonical state.

   Returns :no-current, :newer, :same, :older, or :incomparable."
  [current incoming]
  (cond
    (nil? current)
    :no-current

    (not (canonical? current))
    :no-current

    (not (canonical? incoming))
    :not-canonical

    :else
    (protocol/compare-revisions
     (revision incoming)
     (revision current))))

;; -----------------------------------------------------------------------------
;; Root shape
;; -----------------------------------------------------------------------------

(defn tag-name
  "Return normalized upper-case DOM tag name."
  [element]
  (when (element? element)
    (.-tagName element)))

(defn same-root-tag?
  [left right]
  (and (element? left)
       (element? right)
       (= (tag-name left)
          (tag-name right))))

(defn assert-compatible-root!
  "Require replacement to have the same root tag as current element.

   Gesso's stable-fragment and optimistic replacement paths rely on predictable
   root identity. A root-tag mismatch is treated as a framework/protocol error
   instead of allowing subtle browser behavior."
  [current replacement]
  (when-not (and (element? current)
                 (element? replacement))
    (throw
     (ex-info
      "Gesso Live DOM replacement requires element roots."
      {:error/type :gesso.live.browser.dom/invalid-root
       :current current
       :replacement replacement})))
  (when-not (same-root-tag? current replacement)
    (throw
     (ex-info
      "Gesso Live DOM replacement root tag changed."
      {:error/type :gesso.live.browser.dom/root-tag-mismatch
       :current-tag (tag-name current)
       :replacement-tag (tag-name replacement)})))
  replacement)

;; -----------------------------------------------------------------------------
;; Template and HTML parsing
;; -----------------------------------------------------------------------------

(defn- meaningful-child-nodes
  [parent]
  (->> (array-seq (.-childNodes parent))
       (remove
        (fn [node]
          (and (= (.-nodeType node)
                  js/Node.TEXT_NODE)
               (str/blank?
                (or (.-textContent node)
                    "")))))
       vec))

(defn one-element-root
  "Return the single element root contained by parent.

   Whitespace-only text nodes are ignored. Any other multi-root/text/comment
   content is rejected."
  [parent]
  (let [nodes (meaningful-child-nodes parent)]
    (when-not (and (= 1 (count nodes))
                   (element? (first nodes)))
      (throw
       (ex-info
        "Gesso Live markup must contain exactly one element root."
        {:error/type :gesso.live.browser.dom/not-one-root
         :child-count (count nodes)})))
    (first nodes)))

(defn template-root
  "Return a deep clone of a template's one element root."
  [template]
  (when-not (template? template)
    (throw
     (ex-info
      "Gesso Live optimistic source did not resolve to a template element."
      {:error/type :gesso.live.browser.dom/not-template
       :node template})))
  (.cloneNode
   (one-element-root (.-content template))
   true))

(defn templates
  "Return all Gesso optimistic templates beneath root."
  ([]
   (templates js/document))
  ([root]
   (query-all
    root
    (str "template[" template-attr-name "]"))))

(defn template-by-name
  "Find exactly one optimistic template carrying template-name.

   Equality is performed after lookup instead of interpolating template-name into
   a CSS selector, avoiding CSS escaping bugs for application-supplied names."
  ([template-name]
   (template-by-name js/document
                     template-name))
  ([root template-name]
   (let [matches
         (->> (templates root)
              (filter
               #(= (str template-name)
                   (attr %
                         template-attr-name)))
              vec)]
     (case (count matches)
       0 nil
       1 (first matches)
       (throw
        (ex-info
         "Multiple Gesso Live optimistic templates have the same template name."
         {:error/type :gesso.live.browser.dom/duplicate-template
          :template-name template-name
          :count (count matches)}))))))

(defn parse-one-root
  "Parse HTML text and return its one detached element root.

   The returned node belongs to the current document and is not installed."
  [html]
  (when-not (string? html)
    (throw
     (ex-info
      "Gesso Live HTML input must be a string."
      {:error/type :gesso.live.browser.dom/invalid-html
       :value html})))
  (let [template
        (.createElement js/document
                        "template")]
    (set! (.-innerHTML template)
          html)
    (.cloneNode
     (one-element-root
      (.-content template))
     true)))

;; -----------------------------------------------------------------------------
;; Sanitization
;; -----------------------------------------------------------------------------

(defn remove-attrs!
  [element attributes]
  (doseq [attribute attributes]
    (remove-attr! element attribute))
  element)

(defn sanitize-transport-attrs!
  "Remove transport-only attributes from element and all descendants.

   This is intended for detached response markup before it is installed as
   canonical DOM."
  [element]
  (when-not (element? element)
    (throw
     (ex-info
      "Gesso Live transport sanitization requires an element."
      {:error/type :gesso.live.browser.dom/invalid-sanitize-root
       :value element})))
  (remove-attrs!
   element
   transport-only-attrs)
  (doseq [descendant
          (query-all element "*")]
    (remove-attrs!
     descendant
     transport-only-attrs))
  element)

;; -----------------------------------------------------------------------------
;; Structural snapshots
;; -----------------------------------------------------------------------------

(defn snapshot
  "Capture a detached structural clone suitable for failure recovery.

   Snapshot data deliberately does not contain continuity state. Focus, scroll,
   inputs, details-open state, and related browser-local state belong to
   gesso.live.browser.continuity."
  [element]
  (when-not (element? element)
    (throw
     (ex-info
      "Gesso Live structural snapshot requires an element."
      {:error/type :gesso.live.browser.dom/invalid-snapshot-root
       :value element})))
  {:node (.cloneNode element true)
   :tag-name (tag-name element)
   :scope (scope element)
   :revision-wire (revision-wire element)
   :canonical? (canonical? element)})

(defn snapshot-node
  "Return a fresh clone from snapshot.

   A fresh clone prevents one recovery attempt from mutating the snapshot stored
   in the execution journal."
  [snapshot]
  (when-not (and (map? snapshot)
                 (element? (:node snapshot)))
    (throw
     (ex-info
      "Invalid Gesso Live structural snapshot."
      {:error/type :gesso.live.browser.dom/invalid-snapshot
       :snapshot snapshot})))
  (.cloneNode (:node snapshot)
              true))

;; -----------------------------------------------------------------------------
;; In-place projection/canonical copy
;; -----------------------------------------------------------------------------

(defn attributes
  "Return element attributes as [name value] pairs."
  [element]
  (when-not (element? element)
    (throw
     (ex-info
      "Gesso Live attribute capture requires an element."
      {:error/type :gesso.live.browser.dom/invalid-attribute-root
       :value element})))
  (mapv
   (fn [attribute]
     [(.-name attribute)
      (.-value attribute)])
   (array-seq
    (.-attributes element))))

(defn restore-attributes!
  "Replace target attributes with attribute-pairs."
  [target attribute-pairs]
  (when-not (element? target)
    (throw
     (ex-info
      "Gesso Live attribute restoration requires an element target."
      {:error/type :gesso.live.browser.dom/invalid-attribute-target
       :value target})))
  (doseq [attribute
          (vec
           (array-seq
            (.-attributes target)))]
    (.removeAttribute
     target
     (.-name attribute)))
  (doseq [[name value]
          attribute-pairs]
    (.setAttribute
     target
     name
     value))
  target)

(defn replace-children-from!
  "Replace target children with deep clones of source children."
  [target source]
  (when-not (and (element? target)
                 (element? source))
    (throw
     (ex-info
      "Gesso Live child copying requires element roots."
      {:error/type :gesso.live.browser.dom/invalid-copy-root
       :target target
       :source source})))
  (while (.-firstChild target)
    (.removeChild
     target
     (.-firstChild target)))
  (doseq [child
          (array-seq
           (.-childNodes source))]
    (.appendChild
     target
     (.cloneNode child true)))
  target)

(defn assert-stable-identity!
  "Require source to preserve target DOM id when both roots declare one.

   A source with no id inherits the target id. A different non-blank id is a
   protocol/framework error because HTMX and continuity may already hold the
   target identity."
  [target source]
  (let [target-id
        (or (.-id target) "")
        source-id
        (or (.-id source) "")]
    (when (and (not (str/blank? target-id))
               (not (str/blank? source-id))
               (not= target-id source-id))
      (throw
       (ex-info
        "Gesso Live replacement may not change target DOM identity."
        {:error/type :gesso.live.browser.dom/target-id-mismatch
         :target-id target-id
         :source-id source-id})))
    true))

(defn copy-element-into!
  "Copy source element contents and attributes into the existing target node.

   This is the preferred primitive for an optimistic projection and for a
   settlement canonicalization tied to an already-running HTMX request. Keeping
   the target object itself alive prevents HTMX from retaining a detached target
   reference.

   Root tag and explicit DOM identity are checked before mutation. If source
   omits id, target's existing id is preserved."
  [target source]
  (assert-compatible-root!
   target
   source)
  (assert-stable-identity!
   target
   source)
  (let [target-id
        (or (.-id target) "")]
    (restore-attributes!
     target
     (attributes source))
    (when (and (not (str/blank? target-id))
               (str/blank?
                (or (.-id source) "")))
      (set! (.-id target)
            target-id))
    (replace-children-from!
     target
     source)
    (when (= "DETAILS"
             (tag-name target))
      (set! (.-open target)
            (boolean
             (.-open source)))))
  target)

(defn copy-canonical-into!
  "Copy explicitly canonical detached source into the existing target node.

   Transport-only attributes are removed before mutation. Authority/order
   decisions remain the caller's responsibility."
  [target source]
  (when-not (canonical? source)
    (throw
     (ex-info
      "Gesso Live canonical source is not explicitly marked canonical."
      {:error/type :gesso.live.browser.dom/not-canonical
       :source source})))
  (sanitize-transport-attrs!
   source)
  (copy-element-into!
   target
   source))

;; -----------------------------------------------------------------------------
;; Replacement
;; -----------------------------------------------------------------------------

(defn replace!
  "Replace current with replacement and return the installed replacement.

   Both nodes must be element roots with the same tag name. replacement may be
   detached. current must still have a parent when replacement occurs."
  [current replacement]
  (assert-compatible-root!
   current
   replacement)
  (let [parent (.-parentNode current)]
    (when-not parent
      (throw
       (ex-info
        "Gesso Live cannot replace a node that no longer has a parent."
        {:error/type :gesso.live.browser.dom/detached-target
         :current current})))
    (.replaceChild parent
                   replacement
                   current)
    replacement))

(defn replace-canonical!
  "Install explicitly canonical replacement after transport sanitization.

   This function validates mechanics only. It intentionally does not decide
   whether the incoming revision is authoritative enough to replace current;
   the optimistic browser layer must make that protocol decision before calling
   this primitive."
  [current replacement]
  (when-not (canonical? replacement)
    (throw
     (ex-info
      "Gesso Live canonical replacement is not explicitly marked canonical."
      {:error/type :gesso.live.browser.dom/not-canonical
       :replacement replacement})))
  (sanitize-transport-attrs!
   replacement)
  (replace! current replacement))

(defn restore-snapshot!
  "Restore structural snapshot over current and return the installed node.

   This primitive does not decide whether snapshot recovery is still authorized.
   The optimistic browser layer must first establish that no newer
   canonical state superseded the execution."
  [current snapshot]
  (let [replacement
        (snapshot-node snapshot)]
    (replace! current replacement)))

;; -----------------------------------------------------------------------------
;; Canonical lookup around a target
;; -----------------------------------------------------------------------------

(defn canonical-ancestor
  "Return nearest canonical ancestor-or-self, optionally for expected-scope."
  ([element]
   (canonical-ancestor element nil))
  ([element expected-scope]
   (loop [node element]
     (cond
       (not (element? node))
       nil

       (and (canonical? node)
            (or (nil? expected-scope)
                (= expected-scope
                   (scope node))))
       node

       :else
       (recur (.-parentElement node))))))

(defn canonical-in-root
  "Find canonical state for scope within root.

   Returns the same status map as newest-canonical so callers must confront
   incomparable or duplicate authoritative candidates explicitly."
  [root expected-scope]
  (newest-canonical
   (canonical-elements
    root
    expected-scope)))
