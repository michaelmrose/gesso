(ns gesso.live.continuity
  "Small Clojure/data constructors for Gesso Live client continuity.

   This namespace does not own HTMX attrs, browser lifecycle handling, or
   rendering. It only builds plain data maps suitable for :client-continuity on
   gesso.live.ui fragments.

   The browser runtime consumes the normalized JSON emitted by gesso.live.htmx.

   Typical use:

     (live/->fragment
      {:id \"request-list\"
       :src \"/app/requests/fragment\"
       :subscription {:topic :requests}
       :client-continuity
       (continuity/boxes
        (continuity/anchor-scroll
         {:selector \"[data-humanhelp-request-card]\"})
        (continuity/focus))})

   For simpler built-in preservation, raw data also remains valid:

     {:client-continuity
      {:preserve {:scroll {:selector \"[data-humanhelp-request-card]\"}
                  :focus true}}}

   Custom boxes should normally prefer event-backed or Hyperscript-backed boxes
   before falling back to named JavaScript functions."
  (:require
   [clojure.string :as str]))

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
