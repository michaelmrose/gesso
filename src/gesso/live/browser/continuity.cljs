(ns gesso.live.browser.continuity
  "The single browser implementation of Gesso Live continuity.

   Continuity is browser-owned interaction state that survives DOM replacement.
   It is intentionally separate from optimistic structural snapshots:

   - continuity preserves focus, caret, scroll, inputs, details-open state, and
     application-defined continuity boxes across any replacement
   - structural snapshots restore old authoritative structure only when an
     optimistic execution is still authorized to recover

   Normal HTMX swaps, OOB swaps, Live/SSE refreshes, and optimistic replacement
   all call this namespace. There must not be a second optimistic-only
   continuity implementation.

   Server-side gesso.live.continuity describes what to preserve. This namespace
   owns how that preservation is performed in the browser."
  (:require
   [clojure.string :as str]
   [gesso.live.browser.dom :as dom]))

;; -----------------------------------------------------------------------------
;; Wire/config identity
;; -----------------------------------------------------------------------------

(def continuity-attr
  "Stable fragment wrapper marker emitted by the JVM Live layer."
  :data-gesso-live-continuity)

(def continuity-config-attr-key
  "Attribute carrying normalized continuity configuration JSON."
  :data-gesso-live-continuity-config)

(def continuity-fragment-attr-key
  "Attribute naming the stable replaceable fragment target."
  :data-gesso-live-continuity-fragment)

(def continuity-root-selector
  (str "["
       (dom/attr-name continuity-attr)
       "='true']"))

(def continuity-config-attr
  (dom/attr-name continuity-config-attr-key))

(def continuity-config-script-selector
  (str "script[type='application/json']["
       continuity-config-attr
       "]"))

(def continuity-fragment-attr
  (dom/attr-name continuity-fragment-attr-key))

(def continuity-event-prefix
  "gesso:live-continuity:")

;; -----------------------------------------------------------------------------
;; Runtime state
;; -----------------------------------------------------------------------------

;; Stable continuity target id -> captured continuity slot.
(defonce slots
  (atom {}))

;; Continuity box type string -> {:capture fn :restore fn}.
(defonce boxes
  (atom {}))

;; -----------------------------------------------------------------------------
;; Generic browser helpers
;; -----------------------------------------------------------------------------

(defn now-ms
  []
  (.getTime (js/Date.)))

(defn js-function?
  [x]
  (= "function"
     (js* "typeof ~{}" x)))

(defn contains-node?
  [root node]
  (boolean
   (and root
        node
        (or (identical? root node)
            (.contains root node)))))

(declare emit!)

(defn parse-json
  [root raw source]
  (when (and (string? raw)
             (not (str/blank? raw)))
    (try
      (js->clj (.parse js/JSON raw)
               :keywordize-keys true)
      (catch :default error
        (emit!
         root
         "error"
         #js {:phase "parse-config"
              :source source
              :error error
              :raw raw})
        nil))))

(defn custom-event
  [name detail]
  (try
    (js/CustomEvent.
     name
     #js {:bubbles true
          :cancelable false
          :detail detail})
    (catch :default _
      (let [event
            (.createEvent
             js/document
             "CustomEvent")]
        (.initCustomEvent
         event
         name
         true
         false
         detail)
        event))))

(defn dispatch!
  [target name detail]
  (when (and target
             (.-dispatchEvent target))
    (try
      (.dispatchEvent
       target
       (custom-event name detail))
      (catch :default _
        nil)))
  detail)

(defn emit!
  [root name detail]
  (dispatch!
   (or root
       (.-documentElement js/document))
   (str continuity-event-prefix name)
   detail))

(defn after-layout!
  "Run f after two animation frames.

   One frame is often insufficient for HTMX/OOB replacement plus layout. The
   second frame is Gesso's continuity restoration boundary. Choreography must
   not treat continuity as restored until work scheduled through this function
   has actually run."
  [f]
  (js/requestAnimationFrame
   (fn []
     (js/requestAnimationFrame
      f))))

(defn resolve-global-path
  [path]
  (when (and (string? path)
             (not (str/blank? path)))
    (reduce
     (fn [value part]
       (when value
         (aget value part)))
     js/window
     (str/split path #"\."))))

;; -----------------------------------------------------------------------------
;; Element identity
;; -----------------------------------------------------------------------------

(defn css-escape
  [value]
  (let [value (str value)]
    (if (and (.-CSS js/window)
             (.-escape (.-CSS js/window)))
      (.escape (.-CSS js/window)
               value)
      (str/replace
       value
       #"[^a-zA-Z0-9_-]"
       (fn [ch]
         (str "\\"
              (.toString
               (.charCodeAt ch 0)
               16)
              " "))))))

(defn find-by-id
  [root id]
  (when (and root id)
    (if (= (.-id root)
           (str id))
      root
      (dom/query-one
       root
       (str "#"
            (css-escape id))))))

(defn find-by-attr
  [root attribute value]
  (some
   (fn [candidate]
     (when (= (dom/attr candidate attribute)
              (str value))
       candidate))
   (dom/query-all
    root
    (str "["
         (dom/attr-name attribute)
         "]"))))

(defn key-for-element
  "Construct a stable lookup key for one element.

   Explicit application keys win, followed by DOM id, Gesso continuity key,
   data-key, and name. Positional fallback is deliberately supplied only by
   callers such as input/details capture where the selector is known."
  ([element]
   (key-for-element element nil))
  ([element opts]
   (when element
     (let [key-attr
           (or (:key-attr opts)
               (:key-attribute opts)
               (:keyAttr opts)
               (:keyAttribute opts))]
       (cond
         (and key-attr
              (dom/attr element key-attr))
         {:kind :attr
          :attr key-attr
          :value (dom/attr element key-attr)}

         (not (str/blank?
               (or (.-id element) "")))
         {:kind :id
          :value (.-id element)}

         (dom/attr
          element
          "data-gesso-continuity-key")
         {:kind :attr
          :attr "data-gesso-continuity-key"
          :value
          (dom/attr
           element
           "data-gesso-continuity-key")}

         (dom/attr element "data-key")
         {:kind :attr
          :attr "data-key"
          :value
          (dom/attr
           element
           "data-key")}

         (not (str/blank?
               (or (.-name element) "")))
         {:kind :name
          :value (.-name element)}

         :else
         nil)))))

(defn find-by-key
  [root key]
  (case (:kind key)
    :id
    (find-by-id root
                (:value key))

    :attr
    (find-by-attr root
                  (:attr key)
                  (:value key))

    :name
    (find-by-attr root
                  "name"
                  (:value key))

    :index
    (nth
     (dom/query-all
      root
      (or (:selector key) "*"))
     (:value key)
     nil)

    nil))

;; -----------------------------------------------------------------------------
;; Raw scroll
;; -----------------------------------------------------------------------------

(defn window-scroll-state
  []
  {:kind :window
   :x (or (.-pageXOffset js/window)
          (.-scrollLeft
           (.-documentElement js/document))
          0)
   :y (or (.-pageYOffset js/window)
          (.-scrollTop
           (.-documentElement js/document))
          0)})

(defn restore-window-scroll!
  [state]
  (when (= :window
           (:kind state))
    (.scrollTo
     js/window
     (or (:x state) 0)
     (or (:y state) 0))
    true))

(defn scroll-container-for
  [element root box]
  (let [selector
        (or (:container-selector box)
            (:containerSelector box)
            (:container box))]
    (or
     (when selector
       (or (dom/query-one root selector)
           (dom/query-one
            js/document
            selector)))
     (loop [current
            (.-parentElement element)]
       (when (and current
                  (not
                   (identical?
                    current
                    (.-documentElement js/document)))
                  (not
                   (identical?
                    current
                    (.-body js/document))))
         (let [style
               (.getComputedStyle
                js/window
                current)
               overflow-y
               (.-overflowY style)]
           (if (and
                (#{"auto" "scroll"}
                 overflow-y)
                (> (.-scrollHeight current)
                   (.-clientHeight current)))
             current
             (recur
              (.-parentElement current))))))
     js/window)))

(defn capture-raw-scroll
  [root target box]
  (let [scroller
        (scroll-container-for
         target
         root
         box)]
    (if (identical?
         scroller
         js/window)
      (window-scroll-state)
      {:kind :element
       :key (key-for-element
             scroller
             box)
       :top (.-scrollTop scroller)
       :left (.-scrollLeft scroller)
       ;; This is intentionally transient. If the same scroll container
       ;; survived replacement it is more reliable than requiring a key.
       :transient-element scroller})))

(defn restore-raw-scroll!
  [root state]
  (case (:kind state)
    :window
    (restore-window-scroll!
     state)

    :element
    (when-some
        [element
         (or
          (find-by-key
           root
           (:key state))
          (when
              (dom/connected?
               (:transient-element state))
            (:transient-element state)))]
      (set! (.-scrollTop element)
            (or (:top state) 0))
      (set! (.-scrollLeft element)
            (or (:left state) 0))
      true)

    false))

;; -----------------------------------------------------------------------------
;; Anchor scroll
;; -----------------------------------------------------------------------------

(defn visible-anchor
  [elements scroller]
  (let [viewport
        (if (identical?
             scroller
             js/window)
          {:top 0
           :bottom
           (or (.-innerHeight js/window)
               (.-clientHeight
                (.-documentElement js/document))
               0)}
          (let [rect
                (.getBoundingClientRect
                 scroller)]
            {:top (.-top rect)
             :bottom (.-bottom rect)}))]
    (or
     (first
      (sort-by
       (fn [element]
         (let [rect
               (.getBoundingClientRect
                element)]
           (if (and
                (>= (.-bottom rect)
                    (:top viewport))
                (<= (.-top rect)
                    (:bottom viewport)))
             (js/Math.abs
              (- (.-top rect)
                 (:top viewport)))
             js/Infinity)))
       elements))
     (first elements))))

(defn capture-anchor-scroll
  [root target box]
  (let [selector
        (or (:selector box)
            (:anchor-selector box)
            (:anchorSelector box))
        candidates
        (if selector
          (dom/query-all
           target
           selector)
          [])
        initial
        (or (first candidates)
            target)
        scroller
        (scroll-container-for
         initial
         root
         box)
        raw
        (capture-raw-scroll
         root
         initial
         box)]
    (if-let [anchor
             (and selector
                  (visible-anchor
                   candidates
                   scroller))]
      (if-let [key
               (key-for-element
                anchor
                box)]
        {:key key
         :top
         (.-top
          (.getBoundingClientRect
           anchor))
         :raw raw}
        {:raw-only? true
         :raw raw
         :reason :no-anchor-key})
      {:raw-only? true
       :raw raw
       :reason
       (if selector
         :no-anchor
         :no-selector)})))

(defn restore-anchor-scroll!
  [root target box state]
  (if (and state
           (not (:raw-only? state))
           (:key state))
    (if-some [anchor
              (find-by-key
               target
               (:key state))]
      (let [after
            (.-top
             (.getBoundingClientRect
              anchor))
            delta
            (- after
               (:top state))
            scroller
            (scroll-container-for
             anchor
             root
             box)]
        (when-not (zero? delta)
          (if (identical?
               scroller
               js/window)
            (.scrollBy
             js/window
             0
             delta)
            (set!
             (.-scrollTop scroller)
             (+ (.-scrollTop scroller)
                delta))))
        true)
      (restore-raw-scroll!
       root
       (:raw state)))
    (restore-raw-scroll!
     root
     (:raw state))))

;; -----------------------------------------------------------------------------
;; Inputs
;; -----------------------------------------------------------------------------

(defn input-value
  [element]
  (let [tag
        (some-> (.-tagName element)
                str/lower-case)
        type
        (some-> (.-type element)
                str/lower-case)]
    (cond
      (#{"checkbox" "radio"} type)
      {:checked
       (boolean
        (.-checked element))}

      (and (= "select" tag)
           (.-multiple element))
      {:selected-values
       (->> (array-seq
             (.-options element))
            (filter
             #(.-selected %))
            (mapv
             #(.-value %)))}

      (some? (.-value element))
      {:value (.-value element)}

      :else
      nil)))

(defn restore-input-value!
  [element state]
  (when (and element state)
    (cond
      (contains? state :checked)
      (set! (.-checked element)
            (boolean
             (:checked state)))

      (contains? state :selected-values)
      (let [selected
            (set
             (map str
                  (:selected-values state)))]
        (doseq [option
                (array-seq
                 (.-options element))]
          (set!
           (.-selected option)
           (contains?
            selected
            (str
             (.-value option))))))

      (contains? state :value)
      (set! (.-value element)
            (:value state))))
  element)

(defn capture-inputs
  [target selector]
  (let [selector
        (or selector
            "input, textarea, select")]
    (mapv
     (fn [index element]
       {:key
        (or
         (key-for-element element)
         {:kind :index
          :selector selector
          :value index})
        :value
        (input-value element)})
     (range)
     (dom/query-all
      target
      selector))))

(defn restore-inputs!
  [target states]
  (doseq [{:keys [key value]}
          states]
    (when-some [element
                (find-by-key
                 target
                 key)]
      (restore-input-value!
       element
       value)))
  target)

;; -----------------------------------------------------------------------------
;; <details> open state
;; -----------------------------------------------------------------------------

(defn details-elements
  [target selector]
  (let [selector
        (or selector
            "details")
        descendants
        (dom/query-all
         target
         selector)]
    (if (and
         (= "details"
            (some-> (.-tagName target)
                    str/lower-case))
         (dom/matches?
          target
          selector))
      (into [target]
            descendants)
      descendants)))

(defn capture-details
  [target selector single?]
  (let [selector
        (or selector
            "details")]
    {:single?
     (boolean single?)
     :items
     (mapv
      (fn [index element]
        {:key
         (or
          (key-for-element element)
          {:kind :index
           :selector selector
           :value index})
         :open?
         (boolean
          (.-open element))})
      (range)
      (details-elements
       target
       selector))}))

(defn restore-details!
  [target selector {:keys [single? items]}]
  (let [elements
        (details-elements
         target
         selector)]
    (if single?
      (do
        (doseq [element elements]
          (set!
           (.-open element)
           false))
        (when-some
            [{:keys [key]}
             (first
              (filter :open?
                      items))]
          (when-some
              [element
               (find-by-key
                target
                key)]
            (set!
             (.-open element)
             true))))
      (doseq [{:keys [key open?]}
              items]
        (when-some
            [element
             (find-by-key
              target
              key)]
          (set!
           (.-open element)
           (boolean open?))))))
  target)

;; -----------------------------------------------------------------------------
;; Focus and caret
;; -----------------------------------------------------------------------------

(defn capture-focus
  [root target opts]
  (let [active
        (.-activeElement
         js/document)
        selector
        (:selector opts)
        allow-non-editable?
        (boolean
         (or (:allow-non-editable opts)
             (:allowNonEditable opts)
             (:include-buttons opts)
             (:includeButtons opts)))
        tag
        (some-> active
                .-tagName
                str/lower-case)
        type
        (some-> active
                .-type
                str/lower-case)
        editable?
        (or
         (#{"input" "textarea" "select"}
          tag)
         (boolean
          (.-isContentEditable active)))
        button-input?
        (and
         (= "input" tag)
         (#{"button" "submit" "reset"}
          type))]
    (when (and
           (dom/element? active)
           (contains-node?
            root
            active)
           (contains-node?
            target
            active)
           (or
            (nil? selector)
            (dom/matches?
             active
             selector))
           (or
            allow-non-editable?
            (and editable?
                 (not button-input?))))
      (when-some [key
                  (key-for-element
                   active
                   opts)]
        (cond->
            {:key key
             :window-scroll
             (window-scroll-state)}
          (number?
           (.-selectionStart active))
          (assoc
           :selection-start
           (.-selectionStart active)
           :selection-end
           (.-selectionEnd active)
           :selection-direction
           (.-selectionDirection active)))))))

(defn restore-focus!
  [root target state]
  (when-some
      [element
       (and state
            (find-by-key
             target
             (:key state)))]
    (try
      (.focus
       element
       #js {:preventScroll true})
      (catch :default _
        (.focus element)
        (restore-window-scroll!
         (:window-scroll state))))
    (when (and
           (number?
            (:selection-start state))
           (.-setSelectionRange
            element))
      (try
        (.setSelectionRange
         element
         (:selection-start state)
         (:selection-end state)
         (or
          (:selection-direction state)
          "none"))
        (catch :default _
          nil)))
    ;; Some browsers still move the viewport despite preventScroll. Restore the
    ;; captured window position on the following frame as a safety net.
    (when (:window-scroll state)
      (js/requestAnimationFrame
       #(restore-window-scroll!
         (:window-scroll state)))))
  root)

;; -----------------------------------------------------------------------------
;; Custom/event boxes
;; -----------------------------------------------------------------------------

(defn capture-event-box
  [root target box]
  (let [detail
        #js {:root root
             :target target
             :box (clj->js box)
             :state nil}
        name
        (or (:name box)
            (:event box)
            "custom")]
    (emit!
     root
     (str "capture-box:" name)
     detail)
    (js->clj
     (aget detail "state")
     :keywordize-keys true)))

(defn restore-event-box!
  [root target box state]
  (let [name
        (or (:name box)
            (:event box)
            "custom")]
    (emit!
     root
     (str "restore-box:" name)
     #js {:root root
          :target target
          :box (clj->js box)
          :state (clj->js state)})))

(defn register-box!
  "Register or replace one continuity box implementation.

   implementation may be a ClojureScript map or a JS object with capture and
   restore functions."
  [type implementation]
  (when (and type implementation)
    (let [type
          (if (keyword? type)
            (name type)
            (str type))
          implementation
          (if (map? implementation)
            implementation
            {:capture
             (aget implementation "capture")
             :restore
             (aget implementation "restore")})]
      (swap!
       boxes
       assoc
       type
       implementation)))
  true)

(defn register-built-in-boxes!
  []
  (reset!
   boxes
   {"raw-scroll"
    {:capture capture-raw-scroll
     :restore restore-raw-scroll!}

    "anchor-scroll"
    {:capture capture-anchor-scroll
     :restore restore-anchor-scroll!}

    "focus"
    {:capture
     (fn [root target box]
       (capture-focus
        root
        target
        box))
     :restore
     (fn [root target _box state]
       (restore-focus!
        root
        target
        state))}

    "inputs"
    {:capture
     (fn [_root target box]
       (capture-inputs
        target
        (:selector box)))
     :restore
     (fn [_root target _box state]
       (restore-inputs!
        target
        state))}

    "details-open"
    {:capture
     (fn [_root target box]
       (capture-details
        target
        (:selector box)
        (or (:single? box)
            (:single box)
            (:single-open box)
            (:singleOpen box))))
     :restore
     (fn [_root target box state]
       (restore-details!
        target
        (:selector box)
        state))}

    "js"
    {:capture
     (fn [root target box]
       (when-some [f
                   (resolve-global-path
                    (:capture box))]
         (when (js-function? f)
           (js->clj
            (f root
               target
               (clj->js box))
            :keywordize-keys true))))
     :restore
     (fn [root target box state]
       (when-some [f
                   (resolve-global-path
                    (:restore box))]
         (when (js-function? f)
           (f root
              target
              (clj->js box)
              (clj->js state)))))}

    "event"
    {:capture capture-event-box
     :restore restore-event-box!}

    ;; Hyperscript continuity remains event-mediated. The JVM constructor can
    ;; describe it without this runtime depending on _hyperscript directly.
    "hyperscript"
    {:capture capture-event-box
     :restore restore-event-box!}})
  true)

(defn normalize-box
  [box]
  (cond
    (string? box)
    {:type box}

    (keyword? box)
    {:type (name box)}

    (map? box)
    (let [box
          (cond-> box
            (keyword? (:type box))
            (update :type name)

            (and (nil? (:type box))
                 (keyword? (:name box)))
            (assoc
             :type
             (name (:name box))))
          type
          (or
           (:type box)
           (when
               (contains?
                @boxes
                (:name box))
             (:name box))
           "event")]
      (assoc box
             :type type))

    :else
    nil))

(defn boxes-from-config
  [config]
  (let [preserve
        (or (:preserve config)
            {})
        explicit
        (keep
         normalize-box
         (let [value
               (:boxes config)]
           (cond
             (nil? value)
             []

             (sequential? value)
             value

             :else
             [value])))]
    (cond->
        (vec explicit)
      (:scroll preserve)
      (conj
       (let [scroll
             (:scroll preserve)]
         (if (= true scroll)
           {:type "raw-scroll"}
           (let [mode
                 (or (:mode scroll)
                     (:type scroll))]
             (assoc
              scroll
              :type
              (if
                  (#{"raw"
                     "position"
                     "scroll"
                     "raw-scroll"}
                   mode)
                "raw-scroll"
                "anchor-scroll"))))))

      (:focus preserve)
      (conj
       (assoc
        (if (= true
               (:focus preserve))
          {}
          (:focus preserve))
        :type
        "focus"))

      (:inputs preserve)
      (conj
       (assoc
        (if (= true
               (:inputs preserve))
          {}
          (:inputs preserve))
        :type
        "inputs")))))

(defn capture-box
  [root target box]
  (let [box
        (normalize-box box)
        type
        (:type box)
        implementation
        (get @boxes type)]
    (if-not
        (and implementation
             (:capture implementation))
      (do
        (emit!
         root
         "error"
         #js {:phase "capture"
              :reason "unknown-box-type"
              :box (clj->js box)})
        nil)
      (try
        {:type type
         :name
         (or (:name box)
             type)
         :box box
         :state
         ((:capture implementation)
          root
          target
          box)}
        (catch :default error
          (emit!
           root
           "error"
           #js {:phase "capture"
                :box (clj->js box)
                :error error})
          nil)))))

(defn restore-box!
  [root target captured]
  (when-some
      [implementation
       (get @boxes
            (:type captured))]
    (when-some [restore
                (:restore implementation)]
      (try
        (restore
         root
         target
         (:box captured)
         (:state captured))
        (catch :default error
          (emit!
           root
           "error"
           #js {:phase "restore"
                :box
                (clj->js
                 (:box captured))
                :error error})))))
  target)

;; -----------------------------------------------------------------------------
;; Configuration and stable roots
;; -----------------------------------------------------------------------------

(defn child-config-script
  [root]
  (some
   #(when
        (dom/matches?
         %
         continuity-config-script-selector)
      %)
   (array-seq
    (.-children root))))

(defn parse-config
  [root]
  (when root
    (or
     (when-some [raw
                 (dom/attr
                  root
                  continuity-config-attr)]
       (parse-json
        root
        raw
        "attr"))
     (when-some [script
                 (child-config-script
                  root)]
       (parse-json
        root
        (or
         (.-textContent script)
         (.-innerText script)
         "")
        "script")))))

(defn enabled?
  [config]
  (and config
       (not= false
             (:enabled config))))

(defn root
  "Return nearest stable continuity root for element."
  [element]
  (cond
    (dom/matches?
     element
     continuity-root-selector)
    element

    (dom/element? element)
    (dom/closest
     element
     continuity-root-selector)

    :else
    nil))

(defn target-id
  "Return stable replaceable target id owned by continuity root."
  [root]
  (or
   (dom/attr
    root
    continuity-fragment-attr)
   (let [target
         (dom/attr
          root
          "hx-target")]
     (when (and target
                (str/starts-with?
                 target
                 "#"))
       (subs target 1)))))

(defn target
  "Resolve the current replaceable target for continuity root."
  [root]
  (when-some [id
              (target-id root)]
    (or
     (find-by-id root id)
     (find-by-id
      js/document
      id))))

(defn root-for-target-id
  [wanted-target-id]
  (some
   (fn [candidate]
     (when (= wanted-target-id
              (target-id candidate))
       candidate))
   (dom/query-all
    js/document
    continuity-root-selector)))

(defn- add-distinct-root
  [roots candidate]
  (if (or
       (nil? candidate)
       (some
        #(identical?
          %
          candidate)
        roots))
    roots
    (conj roots
          candidate)))

;; -----------------------------------------------------------------------------
;; Height stability
;; -----------------------------------------------------------------------------

(defn lock-height!
  "Prevent a stable fragment root from collapsing while its replaceable target
   is temporarily smaller during a swap."
  [root target]
  (when (and root target)
    (let [rect
          (.getBoundingClientRect
           root)
          previous
          (.-minHeight
           (.-style root))
          height
          (max
           (.-height rect)
           (.-offsetHeight root))]
      (when (pos? height)
        (set!
         (.-minHeight
          (.-style root))
         (str height "px"))
        {:height height
         :previous previous}))))

(defn release-height-lock!
  [root height-lock]
  (when (and root
             height-lock)
    (set!
     (.-minHeight
      (.-style root))
     (or (:previous height-lock)
         "")))
  root)

;; -----------------------------------------------------------------------------
;; Capture and restore slots
;; -----------------------------------------------------------------------------

(defn release-slot!
  "Release one captured slot without attempting restoration.

   Used when the stable root is being removed or when a caller explicitly
   abandons continuity. Height stabilization is always released."
  [slot]
  (when slot
    (release-height-lock!
     (:root slot)
     (:height-lock slot))
    (swap!
     slots
     dissoc
     (:target-id slot)))
  slot)

(defn capture!
  "Capture continuity for one stable root.

   Re-capturing the same target first releases any stale prior slot. This avoids
   leaking a height lock if overlapping framework events reach the same stable
   root before the earlier cycle restores."
  ([root]
   (capture! root nil))
  ([root source]
   (let [config
         (parse-config root)
         target
         (target root)]
     (when (and target
                (enabled? config))
       (let [target-id
             (target-id root)]
         (when-some [old
                     (get @slots target-id)]
           (release-slot! old))
         (let [captured
               (keep
                #(capture-box
                  root
                  target
                  %)
                (boxes-from-config
                 config))
               slot
               {:source source
                :root root
                :target-id target-id
                :captured-at (now-ms)
                :config config
                :captured
                (vec captured)
                :fallback-scroll
                (window-scroll-state)
                :height-lock
                (lock-height!
                 root
                 target)}]
           (swap!
            slots
            assoc
            target-id
            slot)
           (emit!
            root
            "captured"
            #js {:source source
                 :root root
                 :target target
                 :targetId target-id
                 :count
                 (count captured)
                 :heightLocked
                 (boolean
                  (:height-lock slot))})
           slot))))))

(defn captured-slot
  "Return current captured slot for root or target id."
  [root-or-target-id]
  (let [target-id
        (if (string? root-or-target-id)
          root-or-target-id
          (target-id root-or-target-id))]
    (get @slots
         target-id)))

(defn restore-immediate!
  "Restore continuity that must be visible immediately after replacement.

   Native details-open state is intentionally restored before delayed
   scroll/focus/layout work to avoid a visible collapse."
  [root]
  (let [slot
        (captured-slot root)
        target
        (target root)]
    (when slot
      (if target
        (doseq [captured
                (:captured slot)
                :when
                (= "details-open"
                   (:type captured))]
          (restore-box!
           root
           target
           captured))
        ;; If the target vanished completely, retain the user's page position.
        (restore-window-scroll!
         (:fallback-scroll slot))))
    slot))

(defn restore-after-layout!
  "Complete restoration and consume the captured continuity slot.

   This function is idempotent with respect to already-restored details state.
   Scroll/focus/input boxes are restored after layout, a raw window-scroll
   fallback is applied if the browser unexpectedly clamps to the top, and the
   height lock is released one frame later."
  [root]
  (let [slot
        (captured-slot root)
        target
        (target root)]
    (when slot
      (if target
        (doseq [captured
                (:captured slot)]
          (restore-box!
           root
           target
           captured))
        (restore-window-scroll!
         (:fallback-scroll slot)))
      ;; Catch the important HTMX/OOB failure mode where replacement/layout
      ;; clamps the page to y=0 after our first restoration attempt.
      (when (and
             (:fallback-scroll slot)
             (pos?
              (or
               (:y
                (:fallback-scroll slot))
               0)))
        (js/requestAnimationFrame
         (fn []
           (when
               (zero?
                (or
                 (.-pageYOffset js/window)
                 0))
             (restore-window-scroll!
              (:fallback-scroll slot))))))
      ;; Keep min-height through the current restoration frame.
      (when (:height-lock slot)
        (js/requestAnimationFrame
         #(release-height-lock!
           root
           (:height-lock slot))))
      (swap!
       slots
       dissoc
       (:target-id slot))
      (emit!
       root
       "restored"
       #js {:source (:source slot)
            :root root
            :target target
            :targetId (:target-id slot)
            :count
            (count (:captured slot))
            :heightLocked
            (boolean
             (:height-lock slot))}))
    slot))

(defn restore!
  "Restore immediate visual state now and full continuity after layout.

   Optional on-restored is invoked only after the two-frame restoration boundary
   has run. The callback is invoked even when there was no captured slot: in
   that case there was simply no browser-local state to restore.

   Optimistic choreography uses this callback to emit its modeled
   :continuity/restored event. This prevents target authority from being released
   merely because restoration was scheduled."
  ([root]
   (restore! root nil))
  ([root on-restored]
   (when-not (or (nil? on-restored)
                 (ifn? on-restored))
     (throw
      (ex-info
       "Gesso Live continuity completion callback must be callable or nil."
       {:error/type :gesso.live.continuity/invalid-completion-callback
        :callback on-restored})))
   (let [slot (captured-slot root)]
     (restore-immediate! root)
     (after-layout!
      (fn []
        (let [restored (restore-after-layout! root)]
          (when on-restored
            (on-restored
             {:root root
              :target (target root)
              :slot (or restored slot)})))))
     slot)))

;; -----------------------------------------------------------------------------
;; Event adaptation
;; -----------------------------------------------------------------------------

(defn event-detail
  [event]
  (.-detail event))

(defn detail-field
  [event field]
  (let [detail
        (event-detail event)]
    (when detail
      (aget detail field))))

(defn event-elements
  [event]
  (let [detail
        (event-detail event)
        fields
        ["target"
         "elt"
         "source"
         "fragment"
         "oobElement"
         "swappedElement"
         "oobTarget"]
        request-config
        (when detail
          (aget detail
                "requestConfig"))
        request-element
        (when request-config
          (aget request-config
                "elt"))
        candidates
        (concat
         (keep
          #(when detail
             (aget detail %))
          fields)
         [request-element
          (.-target event)])]
    (reduce
     (fn [result candidate]
       (if (and
            (dom/element? candidate)
            (not-any?
             #(identical?
               %
               candidate)
             result))
         (conj result
               candidate)
         result))
     []
     candidates)))

(defn event-source
  [event]
  (or
   (some->
    (detail-field event "elt")
    (#(when
          (dom/element? %)
        %)))
   (some->
    (detail-field event "source")
    (#(when
          (dom/element? %)
        %)))
   (some->
    (detail-field event "requestConfig")
    (aget "elt")
    (#(when
          (dom/element? %)
        %)))
   (some
    #(when
         (dom/element? %)
       %)
    [(.-target event)])))

(defn- candidate-id
  [candidate]
  (cond
    (dom/element? candidate)
    (not-empty
     (.-id candidate))

    (and
     (string? candidate)
     (str/starts-with?
      candidate
      "#"))
    (not-empty
     (subs candidate 1))

    :else
    nil))

(defn event-target-ids
  [event]
  (let [detail
        (event-detail event)
        fields
        ["target"
         "oobTarget"
         "swappedElement"
         "oobElement"]]
    (into
     #{}
     (keep candidate-id)
     (concat
      (keep
       #(when detail
          (aget detail %))
       fields)
      (event-elements event)))))

(defn roots-from-event
  "Resolve continuity roots affected by an HTMX/SSE/OOB event.

   Captured roots are included even when replacement detached the old target
   from the live document."
  [event]
  (let [direct-roots
        (reduce
         (fn [roots element]
           (add-distinct-root
            roots
            (root element)))
         []
         (event-elements event))
        target-ids
        (event-target-ids event)
        document-roots
        (keep
         root-for-target-id
         target-ids)
        captured-roots
        (keep
         (fn [[target-id slot]]
           (when
               (contains?
                target-ids
                target-id)
             (:root slot)))
         @slots)]
    (reduce
     add-distinct-root
     direct-roots
     (concat
      document-roots
      captured-roots))))

(defn capture-from-event!
  [event]
  (doseq [root
          (roots-from-event event)]
    (capture!
     root
     (event-source event)))
  true)

(defn restore-immediate-from-event!
  [event]
  (doseq [root
          (roots-from-event event)]
    (restore-immediate!
     root))
  true)

(defn restore-after-layout-from-event!
  [event]
  (doseq [root
          (roots-from-event event)]
    (after-layout!
     #(restore-after-layout!
       root)))
  true)

(defn restore-from-event!
  [event]
  (restore-immediate-from-event!
   event)
  (restore-after-layout-from-event!
   event)
  true)

(defn cleanup-element!
  "Release continuity state owned by roots being removed from the document.

   This is intended for HTMX beforeCleanupElement-style integration."
  [element]
  (when element
    (let [nested
          (dom/query-all
           element
           continuity-root-selector)
          roots
          (cond-> nested
            (dom/matches?
             element
             continuity-root-selector)
            (conj element))]
      (doseq [root roots]
        (when-some [slot
                    (captured-slot root)]
          (release-slot! slot)))))
  true)

;; -----------------------------------------------------------------------------
;; Initialization / diagnostics
;; -----------------------------------------------------------------------------

(defn initialize!
  "Install built-in continuity box implementations.

   Event listener registration belongs to the small top-level runtime entrypoint;
   this namespace exposes event adapters but does not attach global listeners
   itself."
  []
  (register-built-in-boxes!)
  true)

(defn slot-summaries
  "Return DOM-light continuity diagnostics."
  []
  (mapv
   (fn [[target-id slot]]
     {:target-id target-id
      :captured-at (:captured-at slot)
      :source
      (some-> (:source slot)
              .-tagName
              str/lower-case)
      :box-types
      (mapv :type
            (:captured slot))
      :height-locked?
      (boolean
       (:height-lock slot))})
   @slots))