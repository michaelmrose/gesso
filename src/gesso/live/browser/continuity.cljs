(ns gesso.live.browser.continuity
  "Physical browser-local continuity for Gesso Live.

   This namespace preserves interaction state across DOM replacement. It is
   deliberately below the semantic browser adapter:

   - gesso.live.browser.adapter owns continuity slot identity/generation
   - gesso.live.browser.shell owns the opaque captured resource
   - this namespace only captures/restores/releases browser-local state

   Continuity therefore has no global slot registry, no logical target locks,
   no stale-generation policy, and no Choreo lifecycle. DOM nodes and other host
   objects returned by capture! are intentionally opaque physical resources and
   must never enter portable AdapterState.

   The JVM gesso.live.continuity namespace remains the data/wire description of
   what should be preserved. This namespace implements those descriptions in
   the browser."
  (:require
   [clojure.string :as str]
   [gesso.live.browser.dom :as dom]))

;; =============================================================================
;; Public identity / wire vocabulary
;; =============================================================================

(def runtime-version 3)

(def runtime-type
  :gesso.live.browser.continuity/runtime)

(def resource-type
  :gesso.live.browser.continuity/resource)

(def continuity-attr
  :data-gesso-live-continuity)

(def continuity-config-attr-key
  :data-gesso-live-continuity-config)

(def continuity-fragment-attr-key
  :data-gesso-live-continuity-fragment)

(def continuity-root-selector
  (str "[" (dom/attr-name continuity-attr) "='true']"))

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

(def option-keys
  #{:boxes
    :on-diagnostic
    :now-ms
    :request-animation-frame!})

;; =============================================================================
;; Errors / validation
;; =============================================================================

(defn- continuity-error
  ([kind message data]
   (continuity-error kind message data nil))
  ([kind message data cause]
   (ex-info
    message
    (merge
     {:error/type :gesso.live.browser.continuity/error
      :error/kind kind}
     data)
    cause)))

(defn- require-map!
  [label value]
  (when-not (map? value)
    (throw
     (continuity-error
      :invalid-map
      (str label " must be a map.")
      {:label label
       :value value})))
  value)

(defn- require-optional-callable!
  [label value]
  (when (and (some? value)
             (not (fn? value)))
    (throw
     (continuity-error
      :invalid-callable
      (str label " must be callable when supplied.")
      {:label label
       :value value})))
  value)

(defn- check-option-keys!
  [options]
  (let [unknown (seq (remove option-keys (keys options)))]
    (when unknown
      (throw
       (continuity-error
        :unknown-options
        "Gesso Live continuity options contain unsupported keys."
        {:unknown-keys (set unknown)
         :allowed-keys option-keys}))))
  options)

;; =============================================================================
;; Generic browser helpers
;; =============================================================================

(defn- default-now-ms
  []
  (.getTime (js/Date.)))

(defn- default-request-animation-frame!
  [f]
  (js/requestAnimationFrame f))

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

(defn custom-event
  [name detail]
  (try
    (js/CustomEvent.
     name
     #js {:bubbles true
          :cancelable false
          :detail detail})
    (catch :default _
      (let [event (.createEvent js/document "CustomEvent")]
        (.initCustomEvent event name true false detail)
        event))))

(defn dispatch-dom-event!
  [target name detail]
  (when (and target
             (.-dispatchEvent target))
    (try
      (.dispatchEvent target (custom-event name detail))
      (catch :default _
        nil)))
  detail)

(defn- runtime-diagnostic!
  [runtime kind data]
  (when-let [observer (:on-diagnostic runtime)]
    (try
      (observer
       (merge
        {:gesso.live.browser.continuity/type :diagnostic
         :kind kind}
        data))
      (catch :default _
        nil)))
  nil)

(defn emit!
  [runtime root name detail]
  (dispatch-dom-event!
   (or root
       (.-documentElement js/document))
   (str continuity-event-prefix name)
   detail)
  (runtime-diagnostic!
   runtime
   :dom-event
   {:name name})
  detail)

(defn parse-json
  [runtime root raw source]
  (when (and (string? raw)
             (not (str/blank? raw)))
    (try
      (js->clj (.parse js/JSON raw)
               :keywordize-keys true)
      (catch :default error
        ;; Configuration is presentation metadata. Malformed metadata disables
        ;; continuity for this replacement instead of turning continuity into an
        ;; authority gate for canonical DOM installation.
        (runtime-diagnostic!
         runtime
         :invalid-config-json
         {:source source
          :message (.-message error)})
        (emit!
         runtime
         root
         "error"
         #js {:phase "parse-config"
              :source source
              :error error
              :raw raw})
        nil))))

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

(defn- raf!
  [runtime f]
  ((:request-animation-frame! runtime) f))

(defn after-layout!
  "Return a Promise resolved after two animation frames and f has run.

   This is physical rendering completion only. The browser shell translates the
   resolved Promise to the adapter's generation-correlated continuity completion
   event; this namespace never does so directly."
  [runtime f]
  (js/Promise.
   (fn [resolve reject]
     (try
       (raf!
        runtime
        (fn []
          (try
            (raf!
             runtime
             (fn []
               (try
                 (resolve (f))
                 (catch :default error
                   (reject error)))))
            (catch :default error
              (reject error)))))
       (catch :default error
         (reject error))))))

;; =============================================================================
;; Element identity
;; =============================================================================

(defn css-escape
  [value]
  (let [value (str value)]
    (if (and (.-CSS js/window)
             (.-escape (.-CSS js/window)))
      (.escape (.-CSS js/window) value)
      (str/replace
       value
       #"[^a-zA-Z0-9_-]"
       (fn [ch]
         (str "\\"
              (.toString (.charCodeAt ch 0) 16)
              " "))))))

(defn find-by-id
  [root id]
  (when (and root id)
    (if (= (.-id root) (str id))
      root
      (dom/query-one root (str "#" (css-escape id))))))

(defn find-by-attr
  [root attribute value]
  (some
   (fn [candidate]
     (when (= (dom/attr candidate attribute)
              (str value))
       candidate))
   (dom/query-all
    root
    (str "[" (dom/attr-name attribute) "]"))))

(defn key-for-element
  ([element]
   (key-for-element element nil))
  ([element opts]
   (when (dom/element? element)
     (let [opts (or opts {})
           configured
           (or (:key-attr opts)
               (:keyAttribute opts))]
       (or
        (when (and configured
                   (dom/attr element configured))
          {:kind :attr
           :attr (dom/attr-name configured)
           :value (dom/attr element configured)})
        (when-not (str/blank? (or (.-id element) ""))
          {:kind :id
           :value (.-id element)})
        (when (dom/attr element "data-gesso-continuity-key")
          {:kind :attr
           :attr "data-gesso-continuity-key"
           :value (dom/attr element "data-gesso-continuity-key")})
        (when (dom/attr element "data-key")
          {:kind :attr
           :attr "data-key"
           :value (dom/attr element "data-key")})
        (when-not (str/blank? (or (.-name element) ""))
          {:kind :name
           :value (.-name element)}))))))

(defn find-by-key
  [root key]
  (case (:kind key)
    :id
    (find-by-id root (:value key))

    :attr
    (find-by-attr root (:attr key) (:value key))

    :name
    (find-by-attr root "name" (:value key))

    :index
    (nth
     (dom/query-all root (or (:selector key) "*"))
     (:value key)
     nil)

    nil))

;; =============================================================================
;; Stable continuity roots/configuration
;; =============================================================================

(defn root
  "Return the nearest stable continuity root for element."
  [element]
  (cond
    (dom/matches? element continuity-root-selector)
    element

    (dom/element? element)
    (dom/closest element continuity-root-selector)

    :else
    nil))

(defn target-id
  "Return the replaceable DOM target id declared by root."
  [root]
  (or
   (dom/attr root continuity-fragment-attr)
   (let [target-value (dom/attr root "hx-target")]
     (when (and target-value
                (str/starts-with? target-value "#"))
       (subs target-value 1)))))

(defn target
  "Resolve the current replaceable target for continuity root."
  [root]
  (when-some [id (target-id root)]
    (or (find-by-id root id)
        (find-by-id js/document id))))

(defn child-config-script
  [root]
  (some
   #(when (dom/matches? % continuity-config-script-selector)
      %)
   (array-seq (.-children root))))

(defn parse-config
  [runtime root]
  (when root
    (or
     (when-some [raw (dom/attr root continuity-config-attr)]
       (parse-json runtime root raw "attr"))
     (when-some [script (child-config-script root)]
       (parse-json
        runtime
        root
        (or (.-textContent script)
            (.-innerText script)
            "")
        "script")))))

(defn enabled?
  [config]
  (and config
       (not= false (:enabled config))))

;; =============================================================================
;; Raw scroll
;; =============================================================================

(defn window-scroll-state
  []
  {:kind :window
   :x (or (.-pageXOffset js/window)
          (.-scrollLeft (.-documentElement js/document))
          0)
   :y (or (.-pageYOffset js/window)
          (.-scrollTop (.-documentElement js/document))
          0)})

(defn restore-window-scroll!
  [state]
  (when (= :window (:kind state))
    (.scrollTo js/window
               (or (:x state) 0)
               (or (:y state) 0))
    true))

(defn scroll-container-for
  [element root box]
  (let [selector (or (:container-selector box)
                     (:containerSelector box)
                     (:container box))]
    (or
     (when selector
       (or (dom/query-one root selector)
           (dom/query-one js/document selector)))
     (loop [current (.-parentElement element)]
       (when (and current
                  (not (identical? current (.-documentElement js/document)))
                  (not (identical? current (.-body js/document))))
         (let [style (.getComputedStyle js/window current)
               overflow-y (.-overflowY style)]
           (if (and (#{"auto" "scroll"} overflow-y)
                    (> (.-scrollHeight current)
                       (.-clientHeight current)))
             current
             (recur (.-parentElement current))))))
     js/window)))

(defn capture-raw-scroll
  [root target box]
  (let [scroller (scroll-container-for target root box)]
    (if (identical? scroller js/window)
      (window-scroll-state)
      {:kind :element
       :key (key-for-element scroller box)
       :top (.-scrollTop scroller)
       :left (.-scrollLeft scroller)
       ;; A surviving element is a physical optimization only. It stays inside
       ;; the shell-owned resource and never becomes semantic identity.
       :transient-element scroller})))

(defn restore-raw-scroll!
  [root state]
  (case (:kind state)
    :window
    (restore-window-scroll! state)

    :element
    (when-some [element
                (or (find-by-key root (:key state))
                    (when (dom/connected? (:transient-element state))
                      (:transient-element state)))]
      (set! (.-scrollTop element) (or (:top state) 0))
      (set! (.-scrollLeft element) (or (:left state) 0))
      true)

    false))

;; =============================================================================
;; Anchor scroll
;; =============================================================================

(defn visible-anchor
  [elements scroller]
  (let [viewport
        (if (identical? scroller js/window)
          {:top 0
           :bottom (or (.-innerHeight js/window)
                       (.-clientHeight (.-documentElement js/document))
                       0)}
          (let [rect (.getBoundingClientRect scroller)]
            {:top (.-top rect)
             :bottom (.-bottom rect)}))]
    (or
     (first
      (sort-by
       (fn [element]
         (let [rect (.getBoundingClientRect element)]
           (if (and (>= (.-bottom rect) (:top viewport))
                    (<= (.-top rect) (:bottom viewport)))
             (js/Math.abs (- (.-top rect) (:top viewport)))
             js/Infinity)))
       elements))
     (first elements))))

(defn capture-anchor-scroll
  [root target box]
  (let [selector (or (:selector box)
                     (:anchor-selector box)
                     (:anchorSelector box))
        candidates (if selector
                     (dom/query-all target selector)
                     [])
        initial (or (first candidates) target)
        scroller (scroll-container-for initial root box)
        raw (capture-raw-scroll root initial box)]
    (if-let [anchor (and selector
                         (visible-anchor candidates scroller))]
      (if-let [key (key-for-element anchor box)]
        {:key key
         :top (.-top (.getBoundingClientRect anchor))
         :raw raw}
        {:raw-only? true
         :raw raw
         :reason :no-anchor-key})
      {:raw-only? true
       :raw raw
       :reason (if selector :no-anchor :no-selector)})))

(defn restore-anchor-scroll!
  [root target box state]
  (if (and state
           (not (:raw-only? state))
           (:key state))
    (if-some [anchor (find-by-key target (:key state))]
      (let [after (.-top (.getBoundingClientRect anchor))
            delta (- after (:top state))
            scroller (scroll-container-for anchor root box)]
        (when-not (zero? delta)
          (if (identical? scroller js/window)
            (.scrollBy js/window 0 delta)
            (set! (.-scrollTop scroller)
                  (+ (.-scrollTop scroller) delta))))
        true)
      (restore-raw-scroll! root (:raw state)))
    (restore-raw-scroll! root (:raw state))))

;; =============================================================================
;; Inputs
;; =============================================================================

(defn input-value
  [element]
  (let [tag (some-> (.-tagName element) str/lower-case)
        type (some-> (.-type element) str/lower-case)]
    (cond
      (#{"checkbox" "radio"} type)
      {:checked (boolean (.-checked element))}

      (and (= "select" tag)
           (.-multiple element))
      {:selected-values
       (->> (array-seq (.-options element))
            (filter #(.-selected %))
            (mapv #(.-value %)))}

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
            (boolean (:checked state)))

      (contains? state :selected-values)
      (let [selected (set (map str (:selected-values state)))]
        (doseq [option (array-seq (.-options element))]
          (set! (.-selected option)
                (contains? selected (str (.-value option))))))

      (contains? state :value)
      (set! (.-value element) (:value state))))
  element)

(defn capture-inputs
  [target selector]
  (let [selector (or selector "input, textarea, select")]
    (mapv
     (fn [index element]
       {:key (or (key-for-element element)
                 {:kind :index
                  :selector selector
                  :value index})
        :value (input-value element)})
     (range)
     (dom/query-all target selector))))

(defn restore-inputs!
  [target states]
  (doseq [{:keys [key value]} states]
    (when-some [element (find-by-key target key)]
      (restore-input-value! element value)))
  target)

;; =============================================================================
;; <details> open state
;; =============================================================================

(defn details-elements
  [target selector]
  (let [selector (or selector "details")
        descendants (dom/query-all target selector)]
    (if (and (= "details" (some-> (.-tagName target) str/lower-case))
             (dom/matches? target selector))
      (into [target] descendants)
      descendants)))

(defn capture-details
  [target selector single?]
  (let [selector (or selector "details")]
    {:single? (boolean single?)
     :items
     (mapv
      (fn [index element]
        {:key (or (key-for-element element)
                  {:kind :index
                   :selector selector
                   :value index})
         :open? (boolean (.-open element))})
      (range)
      (details-elements target selector))}))

(defn restore-details!
  [target selector state]
  (let [{:keys [single? items]} state]
    (if single?
      (do
        (doseq [element (details-elements target selector)]
          (set! (.-open element) false))
        (when-let [{:keys [key]}
                   (first (filter :open? items))]
          (when-some [element (find-by-key target key)]
            (set! (.-open element) true))))
      (doseq [{:keys [key open?]} items]
        (when-some [element (find-by-key target key)]
          (set! (.-open element) (boolean open?))))))
  target)

;; =============================================================================
;; Focus and caret
;; =============================================================================

(defn capture-focus
  [root target opts]
  (let [active (.-activeElement js/document)
        selector (:selector opts)
        allow-non-editable?
        (boolean
         (or (:allow-non-editable opts)
             (:allowNonEditable opts)
             (:include-buttons opts)
             (:includeButtons opts)))
        tag (some-> active .-tagName str/lower-case)
        type (some-> active .-type str/lower-case)
        editable? (or (#{"input" "textarea" "select"} tag)
                      (boolean (.-isContentEditable active)))
        button-input? (and (= "input" tag)
                           (#{"button" "submit" "reset"} type))]
    (when (and (dom/element? active)
               (contains-node? root active)
               (contains-node? target active)
               (or (nil? selector)
                   (dom/matches? active selector))
               (or allow-non-editable?
                   (and editable?
                        (not button-input?))))
      (when-some [key (key-for-element active opts)]
        (cond->
         {:key key
          :window-scroll (window-scroll-state)}
          (number? (.-selectionStart active))
          (assoc
           :selection-start (.-selectionStart active)
           :selection-end (.-selectionEnd active)
           :selection-direction (.-selectionDirection active)))))))

(defn restore-focus!
  [root target state]
  (when-some [element
              (and state
                   (find-by-key target (:key state)))]
    (try
      (.focus element #js {:preventScroll true})
      (catch :default _
        (.focus element)
        (restore-window-scroll! (:window-scroll state))))
    (when (and (number? (:selection-start state))
               (.-setSelectionRange element))
      (try
        (.setSelectionRange
         element
         (:selection-start state)
         (:selection-end state)
         (or (:selection-direction state) "none"))
        (catch :default _
          nil)))
    ;; Keep focus physical: restore the viewport synchronously here. The outer
    ;; restore Promise already waits for the post-layout boundary.
    (when (:window-scroll state)
      (restore-window-scroll! (:window-scroll state))))
  root)

;; =============================================================================
;; Custom/event boxes
;; =============================================================================

(defn capture-event-box
  [runtime root target box]
  (let [detail #js {:root root
                    :target target
                    :box (clj->js box)
                    :state nil}
        name (or (:name box)
                 (:event box)
                 "custom")]
    (emit! runtime root (str "capture-box:" name) detail)
    (js->clj (aget detail "state")
             :keywordize-keys true)))

(defn restore-event-box!
  [runtime root target box state]
  (let [name (or (:name box)
                 (:event box)
                 "custom")]
    (emit!
     runtime
     root
     (str "restore-box:" name)
     #js {:root root
          :target target
          :box (clj->js box)
          :state (clj->js state)})))

;; =============================================================================
;; Box implementation registry
;; =============================================================================

(defn normalize-box
  [box]
  (cond
    (string? box)
    {:type box}

    (keyword? box)
    {:type (name box)}

    (map? box)
    (let [box (cond-> box
                (keyword? (:type box))
                (update :type name)

                (and (nil? (:type box))
                     (keyword? (:name box)))
                (assoc :type (name (:name box))))]
      (assoc box :type (or (:type box) "event")))

    :else
    nil))

(defn boxes-from-config
  [config]
  (let [preserve (or (:preserve config) {})
        explicit
        (keep
         normalize-box
         (let [value (:boxes config)]
           (cond
             (nil? value) []
             (sequential? value) value
             :else [value])))]
    (cond-> (vec explicit)
      (:scroll preserve)
      (conj
       (let [scroll (:scroll preserve)]
         (if (= true scroll)
           {:type "raw-scroll"}
           (let [mode (or (:mode scroll)
                          (:type scroll))]
             (assoc scroll
                    :type
                    (if (#{"raw" "position" "scroll" "raw-scroll"} mode)
                      "raw-scroll"
                      "anchor-scroll"))))))

      (:focus preserve)
      (conj
       (assoc
        (if (= true (:focus preserve))
          {}
          (:focus preserve))
        :type "focus"))

      (:inputs preserve)
      (conj
       (assoc
        (if (= true (:inputs preserve))
          {}
          (:inputs preserve))
        :type "inputs")))))

(defn- built-in-boxes
  []
  {"raw-scroll"
   {:capture
    (fn [_runtime root target box]
      (capture-raw-scroll root target box))
    :restore
    (fn [_runtime root _target _box state]
      (restore-raw-scroll! root state))}

   "anchor-scroll"
   {:capture
    (fn [_runtime root target box]
      (capture-anchor-scroll root target box))
    :restore
    (fn [_runtime root target box state]
      (restore-anchor-scroll! root target box state))}

   "focus"
   {:capture
    (fn [_runtime root target box]
      (capture-focus root target box))
    :restore
    (fn [_runtime root target _box state]
      (restore-focus! root target state))}

   "inputs"
   {:capture
    (fn [_runtime _root target box]
      (capture-inputs target (:selector box)))
    :restore
    (fn [_runtime _root target _box state]
      (restore-inputs! target state))}

   "details-open"
   {:capture
    (fn [_runtime _root target box]
      (capture-details
       target
       (:selector box)
       (or (:single? box)
           (:single box)
           (:single-open box)
           (:singleOpen box))))
    :restore
    (fn [_runtime _root target box state]
      (restore-details! target (:selector box) state))}

   "js"
   {:capture
    (fn [_runtime root target box]
      (when-some [f (resolve-global-path (:capture box))]
        (when (js-function? f)
          (js->clj
           (f root target (clj->js box))
           :keywordize-keys true))))
    :restore
    (fn [_runtime root target box state]
      (when-some [f (resolve-global-path (:restore box))]
        (when (js-function? f)
          (f root target (clj->js box) (clj->js state)))))}

   "event"
   {:capture
    (fn [runtime root target box]
      (capture-event-box runtime root target box))
    :restore
    (fn [runtime root target box state]
      (restore-event-box! runtime root target box state))}

   ;; Hyperscript remains event-mediated. Continuity never evals application
   ;; strings or depends on _hyperscript directly.
   "hyperscript"
   {:capture
    (fn [runtime root target box]
      (capture-event-box runtime root target box))
    :restore
    (fn [runtime root target box state]
      (restore-event-box! runtime root target box state))}})

(defn- normalize-implementation
  [implementation]
  (let [implementation
        (if (map? implementation)
          implementation
          {:capture (aget implementation "capture")
           :restore (aget implementation "restore")})]
    (when-not (map? implementation)
      (throw
       (continuity-error
        :invalid-box-implementation
        "Continuity box implementation must be a map or JS object."
        {:implementation implementation})))
    (doseq [k [:capture :restore]
            :let [f (get implementation k)]
            :when (some? f)]
      (when-not (fn? f)
        (throw
         (continuity-error
          :invalid-box-function
          "Continuity box capture/restore values must be callable."
          {:key k
           :value f}))))
    implementation))

;; =============================================================================
;; Runtime construction / box registration
;; =============================================================================

(defn create
  "Create one physical continuity runtime.

   Runtime state contains only the per-runtime box implementation registry.
   Captured replacement state is never stored here; it is returned to the
   browser shell as an opaque generation-keyed resource."
  ([]
   (create nil))
  ([options]
   (let [options (or options {})
         _ (require-map! "Continuity options" options)
         _ (check-option-keys! options)
         _ (require-optional-callable! "Continuity :on-diagnostic"
                                       (:on-diagnostic options))
         _ (require-optional-callable! "Continuity :now-ms"
                                       (:now-ms options))
         _ (require-optional-callable! "Continuity :request-animation-frame!"
                                       (:request-animation-frame! options))
         custom-boxes (or (:boxes options) {})
         _ (require-map! "Continuity :boxes" custom-boxes)
         boxes
         (merge
          (built-in-boxes)
          (into {}
                (map
                 (fn [[type implementation]]
                   [(if (keyword? type)
                      (name type)
                      (str type))
                    (normalize-implementation implementation)]))
                custom-boxes))]
     {:gesso.live.browser.continuity/type runtime-type
      :version runtime-version
      :boxes (atom boxes)
      :on-diagnostic (:on-diagnostic options)
      :now-ms (or (:now-ms options) default-now-ms)
      :request-animation-frame!
      (or (:request-animation-frame! options)
          default-request-animation-frame!)})))

(defn runtime?
  [value]
  (and (map? value)
       (= runtime-type
          (:gesso.live.browser.continuity/type value))))

(defn require-runtime!
  [value]
  (when-not (runtime? value)
    (throw
     (continuity-error
      :invalid-runtime
      "Expected a Gesso Live continuity runtime."
      {:value value})))
  value)

(defn register-box!
  "Register/replace one box implementation in this runtime only."
  [runtime type implementation]
  (let [runtime (require-runtime! runtime)
        type (cond
               (keyword? type) (name type)
               (string? type) type
               :else (str type))]
    (when (str/blank? type)
      (throw
       (continuity-error
        :invalid-box-type
        "Continuity box type must be non-blank."
        {:type type})))
    (swap! (:boxes runtime)
           assoc
           type
           (normalize-implementation implementation))
    true))

(defn registered-box-types
  [runtime]
  (-> (require-runtime! runtime)
      :boxes
      deref
      keys
      sort
      vec))

;; =============================================================================
;; Box capture/restore
;; =============================================================================

(defn capture-box
  [runtime root target box]
  (let [runtime (require-runtime! runtime)
        box (normalize-box box)
        type (:type box)
        implementation (get @(:boxes runtime) type)]
    (if-not (and implementation
                 (:capture implementation))
      (do
        (runtime-diagnostic!
         runtime
         :unknown-box-type
         {:phase :capture
          :box-type type})
        nil)
      (try
        {:type type
         :name (or (:name box) type)
         :box box
         :state ((:capture implementation)
                 runtime root target box)}
        (catch :default error
          ;; One broken optional box must not make browser-local continuity an
          ;; authority gate for the canonical swap.
          (runtime-diagnostic!
           runtime
           :box-capture-failed
           {:box-type type
            :message (.-message error)})
          (emit!
           runtime
           root
           "error"
           #js {:phase "capture"
                :box (clj->js box)
                :error error})
          nil)))))

(defn restore-box!
  [runtime root target captured]
  (when-some [implementation
              (get @(:boxes (require-runtime! runtime))
                   (:type captured))]
    (when-some [restore (:restore implementation)]
      (try
        (restore runtime
                 root
                 target
                 (:box captured)
                 (:state captured))
        (catch :default error
          (runtime-diagnostic!
           runtime
           :box-restore-failed
           {:box-type (:type captured)
            :message (.-message error)})
          (emit!
           runtime
           root
           "error"
           #js {:phase "restore"
                :box (clj->js (:box captured))
                :error error})))))
  target)

;; =============================================================================
;; Height stability
;; =============================================================================

(defn lock-height!
  [root target]
  (when (and root target)
    (let [rect (.getBoundingClientRect root)
          previous (.-minHeight (.-style root))
          height (max (.-height rect)
                      (.-offsetHeight root))]
      (when (pos? height)
        (set! (.-minHeight (.-style root))
              (str height "px"))
        {:height height
         :previous previous}))))

(defn release-height-lock!
  [root height-lock]
  (when (and root height-lock)
    (set! (.-minHeight (.-style root))
          (or (:previous height-lock) "")))
  root)

;; =============================================================================
;; Opaque shell resource capture/restore/release
;; =============================================================================

(defn resource?
  [value]
  (and (map? value)
       (= resource-type
          (:gesso.live.browser.continuity/type value))))

(defn resource-summary
  "Return diagnostics that deliberately exclude DOM/host references."
  [resource]
  (when (resource? resource)
    {:gesso.live.browser.continuity/type resource-type
     :enabled? (:enabled? resource)
     :target-id (:target-id resource)
     :captured-at (:captured-at resource)
     :box-types (mapv :type (:captured resource))
     :height-locked? (boolean (:height-lock resource))}))

(defn- physical-root
  [context]
  (get-in context [:physical :fragment-root]))

(defn capture!
  "Shell :continuity/capture handler.

   Returns one opaque physical resource. A disabled/malformed continuity config
   still returns a resource so presentation metadata failure does not cancel an
   otherwise-authoritative swap."
  [runtime context]
  (let [runtime (require-runtime! runtime)
        root (physical-root context)
        config (when root (parse-config runtime root))
        current-target (when root (target root))
        active? (and root
                     current-target
                     (enabled? config))
        captured
        (if active?
          (->> (boxes-from-config config)
               (keep #(capture-box runtime root current-target %))
               vec)
          [])
        resource
        {:gesso.live.browser.continuity/type resource-type
         :enabled? (boolean active?)
         :root root
         :target-id (when root (target-id root))
         :captured-at ((:now-ms runtime))
         :config config
         :captured captured
         :fallback-scroll (when active?
                            (window-scroll-state))
         :height-lock (when active?
                        (lock-height! root current-target))}]
    (when active?
      (emit!
       runtime
       root
       "captured"
       #js {:root root
            :target current-target
            :targetId (:target-id resource)
            :count (count captured)
            :heightLocked (boolean (:height-lock resource))}))
    resource))

(defn- restore-details-immediate!
  [runtime resource current-target]
  (doseq [captured (:captured resource)
          :when (= "details-open" (:type captured))]
    (restore-box!
     runtime
     (:root resource)
     current-target
     captured)))

(defn- restore-all!
  [runtime resource current-target]
  ;; Value-bearing and structural browser state must settle before focus/caret.
  ;; In real Chromium, assigning an input value after restoring selection moves
  ;; the caret to the end. Keep capture/config order for every other box, but
  ;; make focus the final physical restoration dependency.
  (let [captured (:captured resource)]
    (doseq [entry captured
            :when (not= "focus" (:type entry))]
      (restore-box!
       runtime
       (:root resource)
       current-target
       entry))
    (doseq [entry captured
            :when (= "focus" (:type entry))]
      (restore-box!
       runtime
       (:root resource)
       current-target
       entry))))

(defn- final-restore!
  [runtime resource current-target]
  (let [root (:root resource)
        fallback (:fallback-scroll resource)]
    (if current-target
      (restore-all! runtime resource current-target)
      (when fallback
        (restore-window-scroll! fallback)))
    ;; Replacement/layout can clamp the viewport after focus/details changes.
    ;; Reapply the captured page position at the completion boundary when it is
    ;; non-zero. This remains physical browser state only.
    (when (and fallback
               (pos? (or (:y fallback) 0))
               (zero? (or (.-pageYOffset js/window) 0)))
      (restore-window-scroll! fallback))
    (release-height-lock! root (:height-lock resource))
    (emit!
     runtime
     root
     "restored"
     #js {:root root
          :target current-target
          :targetId (:target-id resource)
          :count (count (:captured resource))
          :heightLocked (boolean (:height-lock resource))})
    true))

(defn restore!
  "Shell :continuity/restore handler.

   Immediate details state is restored synchronously to avoid visible collapse;
   the returned Promise resolves only after the two-frame post-layout boundary
   and final browser-local restoration. The shell alone translates that Promise
   completion into a generation-correlated adapter event."
  [runtime {:keys [resource] :as _context}]
  (let [runtime (require-runtime! runtime)]
    (if-not (resource? resource)
      (js/Promise.resolve true)
      (let [root (:root resource)
            current-target (when root (target root))]
        (if-not (:enabled? resource)
          (js/Promise.resolve true)
          (do
            (if current-target
              (restore-details-immediate!
               runtime resource current-target)
              (when-let [fallback (:fallback-scroll resource)]
                (restore-window-scroll! fallback)))
            (after-layout!
             runtime
             (fn []
               ;; Resolve the target again after replacement/layout. The stable
               ;; continuity root may survive while its replaceable child does
               ;; not.
               (final-restore!
                runtime
                resource
                (when root (target root)))))))))))

(defn release!
  "Shell :continuity/release handler.

   Release is best-effort physical cleanup only. It never emits semantic
   completion and never consults logical generations; shell resource identity
   already selected the exact resource to release."
  [runtime {:keys [resource] :as _context}]
  (let [runtime (require-runtime! runtime)]
    (when (resource? resource)
      (release-height-lock!
       (:root resource)
       (:height-lock resource))
      (emit!
       runtime
       (:root resource)
       "released"
       #js {:targetId (:target-id resource)
            :heightLocked (boolean (:height-lock resource))}))
    true))

(defn handlers
  "Return shell/core physical handler seams for this continuity runtime."
  [runtime]
  (let [runtime (require-runtime! runtime)]
    {:continuity-capture!
     (fn [context]
       (capture! runtime context))

     :continuity-restore!
     (fn [context]
       (restore! runtime context))

     :continuity-release!
     (fn [context]
       (release! runtime context))}))

(defn diagnostics
  [runtime]
  {:gesso.live.browser.continuity/type runtime-type
   :version runtime-version
   :registered-box-types (registered-box-types runtime)})
