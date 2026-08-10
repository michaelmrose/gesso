(ns gesso.live.continuity
  "JVM-side description and wire encoding for Gesso Live browser continuity.

   This namespace owns the server-side half of continuity: plain-data box and
   preservation constructors, app-facing normalization, deterministic JSON wire
   encoding, the continuity data-attribute vocabulary, and hx-preserve helpers.

   It does not own HTMX request mechanics, DOM capture/restore, optimistic
   lifecycle policy, or browser event listeners. gesso.live.browser.continuity
   is the single implementation that captures and restores browser-local state."
  (:require
   [clojure.string :as str]))

;; -----------------------------------------------------------------------------
;; Wire identity
;; -----------------------------------------------------------------------------

(def continuity-attr
  "Marker placed on the stable fragment wrapper that owns continuity."
  :data-gesso-live-continuity)

(def continuity-config-attr
  "Attribute carrying normalized continuity configuration JSON."
  :data-gesso-live-continuity-config)

(def continuity-fragment-attr
  "Attribute naming the replaceable fragment target owned by the stable wrapper."
  :data-gesso-live-continuity-fragment)

(def continuity-attrs
  "Framework-owned continuity attributes.

   Useful to higher-level attr-merging code that must ensure framework metadata
   wins over conflicting caller attributes."
  #{continuity-attr
    continuity-config-attr
    continuity-fragment-attr})

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn- ex
  [message data]
  (ex-info message data))

(defn- blank-string?
  [x]
  (and (string? x)
       (str/blank? x)))

(defn- present?
  [x]
  (not (or (nil? x)
           (blank-string? x))))

(defn- require-present!
  [k value]
  (when-not (present? value)
    (throw
     (ex (str "gesso.live continuity requires " k ".")
         {k value})))
  value)

(defn- require-map!
  [label value]
  (when-not (map? value)
    (throw
     (ex (str "gesso.live continuity " label " must be a map.")
         {:value value})))
  value)

(defn- normalize-name
  [x]
  (cond
    (keyword? x) (name x)
    (symbol? x)  (name x)
    (nil? x)     nil
    :else        (str x)))

(defn- compact-map
  [m]
  (into {}
        (remove (comp nil? val))
        m))

(defn- selector-options
  [x]
  (cond
    (nil? x)
    {}

    (string? x)
    {:selector x}

    (keyword? x)
    {:selector (name x)}

    (symbol? x)
    {:selector (str x)}

    (map? x)
    x

    :else
    (throw
     (ex "gesso.live continuity option must be nil, a selector string, keyword, symbol, or map."
         {:value x}))))

(defn- normalize-box-type
  [type]
  (let [type' (normalize-name type)]
    (require-present! :type type')
    type'))

(defn- normalize-details-open-options
  [opts]
  (let [single (if (contains? opts :single?)
                 (:single? opts)
                 (:single opts))]
    (cond-> (dissoc opts :single?)
      (contains? opts :single?) (assoc :single single))))

;; -----------------------------------------------------------------------------
;; Box constructors
;; -----------------------------------------------------------------------------

(defn box
  "Build a generic client-continuity box.

   `type` is normalized to the unqualified browser-runtime key. For example,
   :anchor-scroll becomes \"anchor-scroll\" in the emitted JSON.

   `opts` should be plain Clojure data that can be JSON-encoded by
   gesso.live.htmx/client-continuity-attrs."
  ([type]
   (box type nil))
  ([type opts]
   (let [opts' (or opts {})]
     (require-map! "box opts" opts')
     (merge {:type (normalize-box-type type)}
            opts'))))

(defn anchor-scroll
  "Preserve scroll position by anchoring to a stable element inside the
   replaceable fragment target.

   Required:
     :selector or :anchor-selector
       Selector for candidate anchor elements, such as request cards or rows.

   Optional:
     :container-selector / :container
       Selector for an explicit scroll container. If absent, the browser runtime
       finds the nearest scrollable ancestor and falls back to window.

     :key-attr / :keyAttribute
       Attribute used to identify the same anchor after replacement. If absent,
       the runtime uses id, data-gesso-continuity-key, data-key, or name when
       available.

   Shorthand:
     (anchor-scroll \"[data-row]\")"
  [selector-or-opts]
  (let [opts (selector-options selector-or-opts)]
    (when-not (or (present? (:selector opts))
                  (present? (:anchor-selector opts)))
      (throw
       (ex "gesso.live continuity anchor-scroll requires :selector or :anchor-selector."
           {:opts opts})))
    (box :anchor-scroll opts)))

(defn focus
  "Preserve focus, and text selection/caret where the focused element supports
   selection ranges.

   Optional:
     :selector
       Restrict focus preservation to matching elements.

     :key-attr / :keyAttribute
       Attribute used to identify the same element after replacement."
  ([] (focus nil))
  ([selector-or-opts]
   (box :focus (selector-options selector-or-opts))))

(defn inputs
  "Preserve input/textarea/select values inside the fragment target.

   Optional:
     :selector
       Defaults in the browser runtime to \"input, textarea, select\".

     :key-attr / :keyAttribute
       Attribute used to identify the same input after replacement. If absent,
       the runtime uses id, data-gesso-continuity-key, data-key, name, or
       position as a last resort."
  ([] (inputs nil))
  ([selector-or-opts]
   (box :inputs (selector-options selector-or-opts))))

(defn details-open
  "Preserve the open state of native <details> elements inside a replaceable
   fragment target.

   Required:
     :selector
       Selector for candidate <details> elements. The selector should match the
       details elements themselves, not their summaries or contents.

   Optional:
     :key-attr / :keyAttribute
       Attribute used to identify the same details element after replacement.
       If absent, the browser runtime should use id, data-gesso-continuity-key,
       data-key, name, or position as a last resort.

     :single? / :single
       When true, restore at most one open details element. The Clojure-facing
       :single? key is normalized to browser-facing :single.

   Shorthand:
     (details-open \"details[data-accordion-value]\")"
  [selector-or-opts]
  (let [opts (selector-options selector-or-opts)]
    (when-not (present? (:selector opts))
      (throw
       (ex "gesso.live continuity details-open requires :selector."
           {:opts opts})))
    (box :details-open
         (normalize-details-open-options opts))))

(defn event
  "Create an event-backed custom box.

   The browser runtime dispatches:

     gesso:live-continuity:capture-box:<name>
     gesso:live-continuity:restore-box:<name>

   against the stable fragment root. Handlers may set event.detail.state during
   capture and read event.detail.state during restore.

   This is the most HTML/Hyperscript-friendly custom-box mechanism because it
   does not require global JavaScript functions."
  ([name]
   (event name nil))
  ([name opts]
   (let [name' (normalize-name name)]
     (require-present! :name name')
     (box :event
          (assoc (or opts {})
                 :name name')))))

(defn hyperscript
  "Create a Hyperscript-friendly custom box.

   The current browser runtime treats Hyperscript boxes as event-backed boxes.
   That means the capture/restore behavior should be installed as Hyperscript
   event handlers on the stable fragment root or an ancestor.

   Example event names for (hyperscript :selected-row):

     gesso:live-continuity:capture-box:selected-row
     gesso:live-continuity:restore-box:selected-row

   Optional :capture and :restore strings may be carried as metadata for
   component libraries or future runtimes, but the current runtime does not
   execute those strings directly."
  ([name]
   (hyperscript name nil))
  ([name opts]
   (let [name' (normalize-name name)]
     (require-present! :name name')
     (box :hyperscript
          (assoc (or opts {})
                 :name name')))))

(defn js
  "Create a JavaScript-backed custom box.

   This is the escape hatch for complex widgets. Prefer built-ins, event boxes,
   or Hyperscript boxes when possible.

   Required:
     :capture
       Dotted browser function name, e.g. \"myapp.grid.capture\".

     :restore
       Dotted browser function name, e.g. \"myapp.grid.restore\".

   The browser runtime calls:
     capture(root, target, box)
     restore(root, target, box, state)"
  [opts]
  (let [opts' (require-map! "js opts" opts)]
    (require-present! :capture (:capture opts'))
    (require-present! :restore (:restore opts'))
    (box :js opts')))

;; -----------------------------------------------------------------------------
;; Top-level :client-continuity constructors
;; -----------------------------------------------------------------------------

(defn boxes
  "Build a :client-continuity config from explicit boxes.

   Example:

     (boxes
      (anchor-scroll {:selector \"[data-card]\"})
      (details-open {:selector \"details[data-card]\"
                     :key-attr \"data-card-id\"
                     :single? true})
      (focus))"
  [& boxes]
  {:enabled true
   :boxes (vec boxes)})

(defn preserve
  "Build a :client-continuity config using the runtime's built-in preservation
   sugar.

   Supported keys:
     :scroll
       true or a scroll-anchor options map. Since anchor scroll normally needs a
       selector, prefer a map such as {:selector \"[data-card]\"}.

     :focus
       true or a focus options map.

     :inputs
       true or an inputs options map.

     :boxes
       Optional additional explicit boxes.

   Example:

     (preserve
      {:scroll {:selector \"[data-card]\"}
       :focus true
       :inputs {:selector \"[data-preserve-input]\"}})"
  [opts]
  (let [opts' (require-map! "preserve opts" opts)
        preserve' (compact-map
                   {:scroll (:scroll opts')
                    :focus (:focus opts')
                    :inputs (:inputs opts')})
        boxes' (:boxes opts')]
    (cond-> {:enabled true
             :preserve preserve'}
      (seq boxes') (assoc :boxes (vec boxes')))))

(defn with-boxes
  "Add explicit boxes to an existing :client-continuity config."
  [client-continuity & boxes]
  (let [client-continuity' (cond
                             (nil? client-continuity)
                             {:enabled true}

                             (true? client-continuity)
                             {:enabled true}

                             (map? client-continuity)
                             client-continuity

                             :else
                             (throw
                              (ex "gesso.live continuity with-boxes expects nil, true, or a map."
                                  {:client-continuity client-continuity})))
        existing (vec (:boxes client-continuity'))]
    (assoc client-continuity'
           :enabled true
           :boxes (into existing boxes))))

;; -----------------------------------------------------------------------------
;; App-facing normalization
;; -----------------------------------------------------------------------------

(defn- json-name
  [x]
  (cond
    (keyword? x)
    (if-let [ns (namespace x)]
      (str ns "/" (name x))
      (name x))

    (symbol? x)
    (str x)

    :else
    (str x)))

(defn- box-type-name
  "Normalize a Clojure-facing box type to the browser registry key.

   Built-in box types are intentionally unqualified in the browser runtime.
   Raw strings are preserved so applications may register custom box types."
  [x]
  (cond
    (nil? x) nil
    (keyword? x) (name x)
    (symbol? x) (name x)
    :else (str x)))

(defn- normalize-continuity-box
  [box]
  (cond
    (map? box)
    (cond-> box
      (contains? box :type)
      (update :type box-type-name)

      (contains? box :name)
      (update :name json-name))

    (keyword? box)
    {:type (box-type-name box)}

    (symbol? box)
    {:type (box-type-name box)}

    (string? box)
    {:type box}

    :else
    (throw
     (ex "gesso.live continuity :boxes entries must be maps, keywords, symbols, or strings."
         {:box box}))))

(defn- normalize-continuity-boxes
  [boxes]
  (cond
    (nil? boxes)
    nil

    (sequential? boxes)
    (mapv normalize-continuity-box boxes)

    :else
    (throw
     (ex "gesso.live continuity :boxes must be a sequential collection."
         {:boxes boxes}))))

(defn- normalize-preserve
  [preserve]
  (cond
    (nil? preserve) {}
    (false? preserve) {}
    (true? preserve) {:focus true}
    (map? preserve) preserve
    :else
    (throw
     (ex "gesso.live continuity :preserve must be nil, false, true, or a map."
         {:preserve preserve}))))

(def ^:private preserve-sugar
  {:preserve-scroll :scroll
   :preserve-focus :focus
   :preserve-inputs :inputs})

(defn- apply-preserve-sugar
  [preserve client-continuity]
  (reduce-kv
   (fn [preserve' public-k preserve-k]
     (if (contains? client-continuity public-k)
       (let [value (get client-continuity public-k)]
         (if (false? value)
           (dissoc preserve' preserve-k)
           (assoc preserve' preserve-k value)))
       preserve'))
   preserve
   preserve-sugar))

(defn- normalize-continuity-map
  [client-continuity]
  (if (false? (:enabled client-continuity))
    nil
    (let [preserve' (-> (:preserve client-continuity)
                        normalize-preserve
                        (apply-preserve-sugar client-continuity))
          boxes' (normalize-continuity-boxes (:boxes client-continuity))
          base (apply dissoc client-continuity (keys preserve-sugar))]
      (cond-> (assoc base :enabled true)
        (seq preserve') (assoc :preserve preserve')
        (empty? preserve') (dissoc :preserve)
        boxes' (assoc :boxes boxes')))))

(defn normalize-client-continuity
  "Normalize app-facing continuity configuration for the browser runtime.

   Accepted shapes:

     nil / false
       disabled; returns nil

     true
       preserve window scroll and focus/caret

     {:preserve {:scroll ... :focus ... :inputs ...}
      :boxes [...]}
       data-first configuration

     {:preserve-scroll ...
      :preserve-focus ...
      :preserve-inputs ...}
       convenience sugar folded into :preserve

     [{:type :anchor-scroll ...} ...]
       shorthand for {:boxes [...]}

   A map with :enabled false is treated the same as false. Unknown map keys are
   preserved so higher-level components can extend continuity configuration
   without changing this low-level wire encoder."
  [client-continuity]
  (cond
    (or (nil? client-continuity)
        (false? client-continuity))
    nil

    (true? client-continuity)
    {:enabled true
     :preserve {:scroll true
                :focus true}}

    (map? client-continuity)
    (normalize-continuity-map client-continuity)

    (sequential? client-continuity)
    {:enabled true
     :boxes (normalize-continuity-boxes client-continuity)}

    :else
    (throw
     (ex "gesso.live continuity must be nil, false, true, a map, or a sequential collection of boxes."
         {:client-continuity client-continuity}))))

;; -----------------------------------------------------------------------------
;; Deterministic browser JSON
;; -----------------------------------------------------------------------------

(defn- json-string-escape
  [s]
  (let [sb (StringBuilder.)]
    (doseq [ch (str s)]
      (case ch
        \\ (.append sb "\\\\")
        \" (.append sb "\\\"")
        \backspace (.append sb "\\b")
        \formfeed (.append sb "\\f")
        \newline (.append sb "\\n")
        \return (.append sb "\\r")
        \tab (.append sb "\\t")
        (if (< (int ch) 32)
          (.append sb (format "\\u%04x" (int ch)))
          (.append sb ch))))
    (str sb)))

(declare json-value)

(defn- json-array
  [xs]
  (str "[" (str/join "," (map json-value xs)) "]"))

(defn- json-object
  [m]
  (str "{"
       (str/join
        ","
        (map (fn [[k v]]
               (str "\""
                    (json-string-escape (json-name k))
                    "\":"
                    (json-value v)))
             (sort-by (comp json-name key) m)))
       "}"))

(defn- finite-number?
  [x]
  (cond
    (instance? Double x) (Double/isFinite ^double x)
    (instance? Float x) (Float/isFinite ^float x)
    :else true))

(defn- json-value
  [x]
  (cond
    (nil? x)
    "null"

    (string? x)
    (str "\"" (json-string-escape x) "\"")

    (keyword? x)
    (str "\"" (json-string-escape (json-name x)) "\"")

    (symbol? x)
    (str "\"" (json-string-escape (json-name x)) "\"")

    (or (true? x) (false? x))
    (if x "true" "false")

    (number? x)
    (do
      (when-not (finite-number? x)
        (throw
         (ex "gesso.live continuity JSON cannot encode non-finite numbers."
             {:value x})))
      (str x))

    (map? x)
    (json-object x)

    (sequential? x)
    (json-array x)

    (set? x)
    (json-array (sort-by pr-str x))

    (fn? x)
    (throw
     (ex "gesso.live continuity config cannot contain Clojure functions. Use Clojure data, event/Hyperscript boxes, or browser function names instead."
         {:value x}))

    :else
    (str "\"" (json-string-escape (str x)) "\"")))

(defn client-continuity-json
  "Normalize and encode continuity configuration as deterministic JSON.

   Returns nil when continuity is disabled."
  [client-continuity]
  (some-> client-continuity
          normalize-client-continuity
          json-value))

;; -----------------------------------------------------------------------------
;; Browser-facing attrs
;; -----------------------------------------------------------------------------

(defn- require-fragment-id!
  [fragment-id]
  (when-not (and (string? fragment-id)
                 (not (str/blank? fragment-id)))
    (throw
     (ex "gesso.live continuity fragment id must be a non-blank string."
         {:fragment-id fragment-id})))
  fragment-id)

(defn client-continuity-attrs
  "Build the continuity attrs for one stable live-fragment wrapper.

   Required:
     :fragment-id
       DOM id of the replaceable inner target.

   Optional:
     :client-continuity
       App-facing continuity configuration. Disabled values return {}.

   These are framework wire attributes, not HTMX attributes. gesso.live.htmx may
   merge them onto its stable fragment root, but their vocabulary and encoding
   are owned here."
  [{:keys [fragment-id client-continuity]}]
  (if-some [config (normalize-client-continuity client-continuity)]
    {continuity-attr "true"
     continuity-fragment-attr (require-fragment-id! fragment-id)
     continuity-config-attr (json-value config)}
    {}))

;; -----------------------------------------------------------------------------
;; hx-preserve helpers
;; -----------------------------------------------------------------------------

(def hx-preserve-attrs
  "Attrs that ask htmx to preserve an element by stable id when an ancestor is
   swapped.

   This is separate from client-continuity boxes because hx-preserve is native
   htmx behavior applied directly to the element being preserved."
  {:hx-preserve true})

(defn hx-preserve
  "Return attrs with hx-preserve enabled.

   Usage:

     [:input (continuity/hx-preserve
              {:id \"search\"
               :name \"q\"})]

   The preserved element must have a stable id, and the server response should
   include an element with the same id."
  ([] hx-preserve-attrs)
  ([attrs]
   (assoc (or attrs {}) :hx-preserve true)))
