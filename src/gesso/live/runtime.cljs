(ns gesso.live.runtime
  "Complete browser entry point for Gesso Live.

   This namespace is compiled to the generated gesso-live.js resource. It owns
   only browser mechanics:

   - HTMX and SSE event ingestion
   - DOM lookup and mutation
   - continuity capture and restoration
   - optimistic execution journaling
   - target locking and request correlation
   - browser effect handlers for gesso.live.optimistic-machine

   Application policy remains server-side. The runtime treats transition names,
   scopes, revisions, settlement reasons, and rendered content as opaque values."
  (:require
   [clojure.string :as str]
   [gesso.live.optimistic-machine :as machine]))

;; -----------------------------------------------------------------------------
;; Runtime identity and protocol names
;; -----------------------------------------------------------------------------

(def runtime-version "1.0.0")
(def optimistic-protocol-version "1")

(def continuity-root-selector
  "[data-gesso-live-continuity='true']")

(def continuity-config-attr
  "data-gesso-live-continuity-config")

(def continuity-config-script-selector
  "script[type='application/json'][data-gesso-live-continuity-config]")

(def continuity-fragment-attr
  "data-gesso-live-continuity-fragment")

(def optimistic-protocol-attr
  "data-gesso-optimistic-protocol")

(def optimistic-machine-attr
  "data-gesso-optimistic-machine")

(def optimistic-transition-attr
  "data-gesso-optimistic-transition")

(def optimistic-template-attr
  "data-gesso-optimistic-template")

(def optimistic-target-attr
  "data-gesso-optimistic-target")

(def optimistic-scope-attr
  "data-gesso-optimistic-scope")

(def optimistic-base-revision-attr
  "data-gesso-optimistic-base-revision")

(def optimistic-revision-attr
  "data-gesso-optimistic-revision")

(def optimistic-label-attr
  "data-gesso-optimistic-label")

(def optimistic-mode-attr
  "data-gesso-optimistic-mode")

(def optimistic-settlement-attr
  "data-gesso-optimistic-settlement")

(def optimistic-execution-attr
  "data-gesso-optimistic-execution")

(def optimistic-outcome-attr
  "data-gesso-optimistic-outcome")

(def optimistic-command-applied-attr
  "data-gesso-optimistic-command-applied")

(def optimistic-reason-attr
  "data-gesso-optimistic-reason")

(def optimistic-active-attr
  "data-gesso-optimistic-active")

(def optimistic-pending-attr
  "data-gesso-optimistic-pending")

(def optimistic-pressed-attr
  "data-gesso-optimistic-pressed")

(def optimistic-locked-attr
  "data-gesso-optimistic-locked")

(def optimistic-accepted-attr
  "data-gesso-optimistic-accepted")

(def optimistic-request-header
  "Gesso-Optimistic-Execution")

(def optimistic-error-event
  "gesso:optimistic:error")

(def default-settlement-timeout-ms 15000)
(def pre-request-timeout-ms 2000)
(def scroll-shield-frames 10)

(def optimistic-source-selector
  (str "[" optimistic-protocol-attr "]["
       optimistic-machine-attr "]["
       optimistic-template-attr "]["
       optimistic-target-attr "]"))

;; -----------------------------------------------------------------------------
;; Runtime stores
;; -----------------------------------------------------------------------------

(defonce initialized? (atom false))

;; Execution id -> suspended machine execution.
(defonce executions (atom {}))

;; Execution id -> current request source element. Kept outside machine data
;; so public diagnostics can inspect the journal without traversing DOM-rich
;; execution context.
(defonce execution-sources (atom {}))

;; HTMX request source element uid -> execution id.
(defonce request-executions (atom {}))

;; Execution id -> target element. The target also carries a string expando
;; so synchronous duplicate events can be rejected without an atom scan.
(defonce target-locks (atom {}))

;; Execution id -> browser timeout handle.
(defonce timeout-handles (atom {}))

;; Execution id -> immediate pre-request press state.
(defonce press-states (atom {}))

;; Stable continuity target id -> captured continuity slot.
(defonce continuity-slots (atom {}))

;; Continuity box type string -> {:capture fn :restore fn}.
(defonce continuity-boxes (atom {}))

(defonce last-user-scroll-intent-at (atom 0))

;; -----------------------------------------------------------------------------
;; Generic browser helpers
;; -----------------------------------------------------------------------------

(defn now-ms []
  (.getTime (js/Date.)))

(defn element?
  [x]
  (and x (= 1 (.-nodeType x))))

(defn js-function?
  [x]
  (= "function" (js* "typeof ~{}" x)))

(defn connected?
  [element]
  (and element
       (if (boolean? (.-isConnected element))
         (.-isConnected element)
         (and (.-documentElement js/document)
              (.contains (.-documentElement js/document) element)))))

(defn contains-node?
  [root node]
  (boolean
   (and root
        node
        (or (identical? root node)
            (.contains root node)))))

(defn query
  [root selector]
  (when (and root
             (string? selector)
             (not (str/blank? selector)))
    (try
      (.querySelector root selector)
      (catch :default _
        nil))))

(defn query-all
  [root selector]
  (if (and root
           (string? selector)
           (not (str/blank? selector)))
    (try
      (vec (array-seq (.querySelectorAll root selector)))
      (catch :default _
        []))
    []))

(defn matches?
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
  [element selector]
  (when (and (element? element)
             (.-closest element))
    (try
      (.closest element selector)
      (catch :default _
        nil))))

(defn attr
  [element name]
  (when (and element (.-getAttribute element))
    (.getAttribute element name)))

(defn has-attr?
  [element name]
  (boolean
   (and element
        (.-hasAttribute element)
        (.hasAttribute element name))))

(defn set-attr!
  [element name value]
  (when (and element (.-setAttribute element))
    (.setAttribute element name (if (nil? value) "" (str value))))
  element)

(defn remove-attr!
  [element name]
  (when (and element (.-removeAttribute element))
    (.removeAttribute element name))
  element)

(defn parse-int
  [x]
  (when (some? x)
    (let [n (js/parseInt (str x) 10)]
      (when-not (js/isNaN n)
        n))))

(defn parse-bool
  [x]
  (= "true" (some-> x str str/lower-case)))

(defn custom-event
  [name detail]
  (try
    (js/CustomEvent. name
                     #js {:bubbles true
                          :cancelable false
                          :detail detail})
    (catch :default _
      (let [event (.createEvent js/document "CustomEvent")]
        (.initCustomEvent event name true false detail)
        event))))

(defn dispatch!
  [target name detail]
  (when (and target (.-dispatchEvent target))
    (try
      (.dispatchEvent target (custom-event name detail))
      (catch :default _
        nil)))
  detail)

(defn emit-continuity!
  [root name detail]
  (dispatch! (or root (.-documentElement js/document))
             (str "gesso:live-continuity:" name)
             detail))

(defn emit-optimistic!
  [root name detail]
  (dispatch! (or root (.-documentElement js/document))
             (str "gesso:optimistic:" name)
             detail))

(defn emit-optimistic-error!
  [root detail]
  (emit-optimistic! root "error" detail)
  (dispatch! js/document optimistic-error-event detail)
  nil)

(defn event-detail
  [event]
  (.-detail event))

(defn detail-field
  [event field]
  (let [detail (event-detail event)]
    (when detail
      (aget detail field))))

(defn event-elements
  [event]
  (let [detail (event-detail event)
        fields ["target"
                "elt"
                "source"
                "fragment"
                "oobElement"
                "swappedElement"
                "oobTarget"]
        request-config (when detail (aget detail "requestConfig"))
        request-elt (when request-config (aget request-config "elt"))
        candidates (concat
                    (keep #(when detail (aget detail %)) fields)
                    [request-elt (.-target event)])]
    (reduce
     (fn [result candidate]
       (if (and (element? candidate)
                (not-any? #(identical? % candidate) result))
         (conj result candidate)
         result))
     []
     candidates)))

(defn event-target
  [event]
  (or (some-> (detail-field event "target")
              (#(when (element? %) %)))
      (when (element? (.-target event))
        (.-target event))))

(defn event-source
  [event]
  (or (some-> (detail-field event "elt")
              (#(when (element? %) %)))
      (some-> (detail-field event "source")
              (#(when (element? %) %)))
      (some-> (detail-field event "requestConfig")
              (aget "elt")
              (#(when (element? %) %)))
      (event-target event)))

(defn candidate-id
  [candidate]
  (cond
    (element? candidate)
    (not-empty (.-id candidate))

    (and (string? candidate)
         (str/starts-with? candidate "#"))
    (not-empty (subs candidate 1))

    :else
    nil))

(defn event-target-ids
  [event]
  (let [detail (event-detail event)
        fields ["target" "oobTarget" "swappedElement" "oobElement"]]
    (into #{}
          (keep candidate-id)
          (concat
           (keep #(when detail (aget detail %)) fields)
           (event-elements event)))))

(defn after-layout!
  [f]
  (js/requestAnimationFrame
   (fn []
     (js/requestAnimationFrame f))))

(defn node-uid
  [element]
  (when element
    (or (aget element "__gessoLiveUid")
        (let [uid (str (random-uuid))]
          (aset element "__gessoLiveUid" uid)
          uid))))

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
         (str "\\" (.toString (.charCodeAt ch 0) 16) " "))))))

(defn find-by-id
  [root id]
  (when (and root id)
    (if (= (.-id root) (str id))
      root
      (query root (str "#" (css-escape id))))))

(defn find-by-attr
  [root attribute value]
  (some
   (fn [candidate]
     (when (= (attr candidate attribute) (str value))
       candidate))
   (query-all root (str "[" attribute "]"))))

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
;; Element identity, values, and snapshots
;; -----------------------------------------------------------------------------

(defn key-for-element
  ([element]
   (key-for-element element nil))
  ([element opts]
   (when element
     (let [key-attr (or (:key-attr opts)
                        (:key-attribute opts)
                        (:keyAttr opts)
                        (:keyAttribute opts))]
       (cond
         (and key-attr (attr element key-attr))
         {:kind :attr
          :attr key-attr
          :value (attr element key-attr)}

         (not (str/blank? (or (.-id element) "")))
         {:kind :id :value (.-id element)}

         (attr element "data-gesso-continuity-key")
         {:kind :attr
          :attr "data-gesso-continuity-key"
          :value (attr element "data-gesso-continuity-key")}

         (attr element "data-key")
         {:kind :attr
          :attr "data-key"
          :value (attr element "data-key")}

         (not (str/blank? (or (.-name element) "")))
         {:kind :name :value (.-name element)}

         :else
         nil)))))

(defn find-by-key
  [root key]
  (case (:kind key)
    :id (find-by-id root (:value key))
    :attr (find-by-attr root (:attr key) (:value key))
    :name (find-by-attr root "name" (:value key))
    :index (nth (query-all root (or (:selector key) "*"))
                (:value key)
                nil)
    nil))

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
    (.scrollTo js/window (or (:x state) 0) (or (:y state) 0))
    true))

(defn input-value
  [element]
  (let [tag (some-> (.-tagName element) str/lower-case)
        type (some-> (.-type element) str/lower-case)]
    (cond
      (#{"checkbox" "radio"} type)
      {:checked (boolean (.-checked element))}

      (and (= "select" tag) (.-multiple element))
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
      (set! (.-checked element) (boolean (:checked state)))

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
     (query-all target selector))))

(defn restore-inputs!
  [target states]
  (doseq [{:keys [key value]} states]
    (restore-input-value! (find-by-key target key) value))
  target)

(defn details-elements
  [target selector]
  (let [selector (or selector "details")
        descendants (query-all target selector)]
    (if (and (= "details" (some-> (.-tagName target) str/lower-case))
             (matches? target selector))
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
  [target selector {:keys [single? items]}]
  (let [elements (details-elements target selector)]
    (if single?
      (do
        (doseq [element elements]
          (set! (.-open element) false))
        (when-some [{:keys [key]} (first (filter :open? items))]
          (when-some [element (find-by-key target key)]
            (set! (.-open element) true))))
      (doseq [{:keys [key open?]} items]
        (when-some [element (find-by-key target key)]
          (set! (.-open element) (boolean open?))))))
  target)

(defn capture-focus
  [root target opts]
  (let [active (.-activeElement js/document)
        selector (:selector opts)
        allow-non-editable? (boolean
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
    (when (and (element? active)
               (contains-node? root active)
               (contains-node? target active)
               (or (nil? selector) (matches? active selector))
               (or allow-non-editable?
                   (and editable? (not button-input?))))
      (when-some [key (key-for-element active opts)]
        (cond->
         {:key key
          :window-scroll (window-scroll-state)}
          (number? (.-selectionStart active))
          (assoc :selection-start (.-selectionStart active)
                 :selection-end (.-selectionEnd active)
                 :selection-direction (.-selectionDirection active)))))))

(defn restore-focus!
  [root target state]
  (when-some [element (and state (find-by-key target (:key state)))]
    (try
      (.focus element #js {:preventScroll true})
      (catch :default _
        (.focus element)
        (restore-window-scroll! (:window-scroll state))))
    (when (and (number? (:selection-start state))
               (.-setSelectionRange element))
      (try
        (.setSelectionRange element
                            (:selection-start state)
                            (:selection-end state)
                            (or (:selection-direction state) "none"))
        (catch :default _
          nil)))
    (when (:window-scroll state)
      (js/requestAnimationFrame
       #(restore-window-scroll! (:window-scroll state)))))
  root)

(defn attributes
  [element]
  (mapv
   (fn [attribute]
     [(.-name attribute) (.-value attribute)])
   (array-seq (.-attributes element))))

(defn restore-attributes!
  [element attribute-pairs]
  (doseq [attribute (array-seq (.-attributes element))]
    (.removeAttribute element (.-name attribute)))
  (doseq [[name value] attribute-pairs]
    (.setAttribute element name value))
  element)

(defn capture-local-state
  [target]
  {:inputs (capture-inputs target nil)
   :details (capture-details target nil false)
   :focus (capture-focus target target {})
   :window-scroll (window-scroll-state)})

(defn restore-local-state!
  [target state]
  (when (and target state)
    (restore-inputs! target (:inputs state))
    (restore-details! target nil (:details state))
    (restore-focus! target target (:focus state))
    (restore-window-scroll! (:window-scroll state)))
  target)

(defn element-snapshot
  [element]
  {:tag-name (.-tagName element)
   :id (.-id element)
   :clone (.cloneNode element true)
   :local-state (capture-local-state element)})

(defn same-tag?
  [a b]
  (= (some-> (.-tagName a) str/lower-case)
     (some-> (.-tagName b) str/lower-case)))

(defn validate-projection-root!
  [target source]
  (when-not (and (element? target) (element? source))
    (throw
     (ex-info "Optimistic projection requires element roots."
              {:target target :source source})))
  (when-not (same-tag? target source)
    (throw
     (ex-info "Optimistic projection root tag does not match its target."
              {:target-tag (.-tagName target)
               :source-tag (.-tagName source)})))
  (let [target-id (.-id target)
        source-id (.-id source)]
    (when (and (not (str/blank? target-id))
               (not (str/blank? source-id))
               (not= target-id source-id))
      (throw
       (ex-info "Optimistic projection may not change target identity."
                {:target-id target-id
                 :source-id source-id}))))
  true)

(defn replace-children-from!
  [target source]
  (while (.-firstChild target)
    (.removeChild target (.-firstChild target)))
  (doseq [child (array-seq (.-childNodes source))]
    (.appendChild target (.cloneNode child true)))
  target)

(defn copy-element-into-target!
  [target source]
  (validate-projection-root! target source)
  (let [target-id (.-id target)]
    (restore-attributes! target (attributes source))
    (when (and (not (str/blank? target-id))
               (str/blank? (.-id source)))
      (set! (.-id target) target-id))
    (replace-children-from! target source)
    (when (= "details" (some-> (.-tagName target) str/lower-case))
      (set! (.-open target) (boolean (.-open source)))))
  target)

(defn restore-snapshot!
  [target snapshot]
  (when (and target snapshot)
    (let [source (:clone snapshot)]
      (validate-projection-root! target source)
      (copy-element-into-target! target source)
      (restore-local-state! target (:local-state snapshot))))
  target)

(defn htmx-process!
  [element]
  (when (and element
             (.-htmx js/window)
             (.-process (.-htmx js/window)))
    (.process (.-htmx js/window) element))
  element)

;; -----------------------------------------------------------------------------
;; Continuity configuration and boxes
;; -----------------------------------------------------------------------------

(defn parse-json
  [root raw source]
  (when (and (string? raw) (not (str/blank? raw)))
    (try
      (js->clj (.parse js/JSON raw) :keywordize-keys true)
      (catch :default error
        (emit-continuity!
         root
         "error"
         #js {:phase "parse-config"
              :source source
              :error error
              :raw raw})
        nil))))

(defn child-config-script
  [root]
  (some
   #(when (matches? % continuity-config-script-selector) %)
   (array-seq (.-children root))))

(defn parse-continuity-config
  [root]
  (when root
    (or (when-some [raw (attr root continuity-config-attr)]
          (parse-json root raw "attr"))
        (when-some [script (child-config-script root)]
          (parse-json root
                      (or (.-textContent script)
                          (.-innerText script)
                          "")
                      "script")))))

(defn config-enabled?
  [config]
  (and config (not= false (:enabled config))))

(defn normalize-box
  [box]
  (cond
    (string? box)
    {:type box}

    (keyword? box)
    {:type (name box)}

    (map? box)
    (let [box (cond-> box
                (keyword? (:type box)) (update :type name)
                (and (nil? (:type box))
                     (keyword? (:name box)))
                (assoc :type (name (:name box))))
          type (or (:type box)
                   (when (contains? @continuity-boxes (:name box))
                     (:name box))
                   "event")]
      (assoc box :type type))

    :else
    nil))

(defn boxes-from-config
  [config]
  (let [preserve (or (:preserve config) {})
        boxes (keep normalize-box
                    (let [value (:boxes config)]
                      (cond
                        (nil? value) []
                        (sequential? value) value
                        :else [value])))
        boxes (cond-> (vec boxes)
                (:scroll preserve)
                (conj
                 (let [scroll (:scroll preserve)]
                   (if (= true scroll)
                     {:type "raw-scroll"}
                     (let [mode (or (:mode scroll) (:type scroll))]
                       (assoc scroll
                              :type
                              (if (#{"raw" "position" "scroll" "raw-scroll"}
                                   mode)
                                "raw-scroll"
                                "anchor-scroll"))))))

                (:focus preserve)
                (conj
                 (assoc (if (= true (:focus preserve))
                          {}
                          (:focus preserve))
                        :type "focus"))

                (:inputs preserve)
                (conj
                 (assoc (if (= true (:inputs preserve))
                          {}
                          (:inputs preserve))
                        :type "inputs")))]
    boxes))

(defn scroll-container-for
  [element root box]
  (let [selector (or (:container-selector box)
                     (:containerSelector box)
                     (:container box))]
    (or (when selector
          (or (query root selector)
              (query js/document selector)))
        (loop [current (.-parentElement element)]
          (when (and current
                     (not (identical? current (.-documentElement js/document)))
                     (not (identical? current (.-body js/document))))
            (let [style (.getComputedStyle js/window current)
                  overflow-y (.-overflowY style)]
              (if (and (#{"auto" "scroll"} overflow-y)
                       (> (.-scrollHeight current) (.-clientHeight current)))
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
       :transient-element scroller})))

(defn restore-raw-scroll!
  [root state]
  (case (:kind state)
    :window
    (restore-window-scroll! state)

    :element
    (when-some [element (or (find-by-key root (:key state))
                            (when (connected? (:transient-element state))
                              (:transient-element state)))]
      (set! (.-scrollTop element) (or (:top state) 0))
      (set! (.-scrollLeft element) (or (:left state) 0))
      true)

    false))

(defn visible-anchor
  [elements scroller]
  (let [viewport (if (identical? scroller js/window)
                   {:top 0
                    :bottom (or (.-innerHeight js/window)
                                (.-clientHeight (.-documentElement js/document))
                                0)}
                   (let [rect (.getBoundingClientRect scroller)]
                     {:top (.-top rect) :bottom (.-bottom rect)}))]
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
        candidates (if selector (query-all target selector) [])
        initial (or (first candidates) target)
        scroller (scroll-container-for initial root box)
        raw (capture-raw-scroll root initial box)]
    (if-let [anchor (and selector (visible-anchor candidates scroller))]
      (if-let [key (key-for-element anchor box)]
        {:key key
         :top (.-top (.getBoundingClientRect anchor))
         :raw raw}
        {:raw-only? true :raw raw :reason :no-anchor-key})
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

(defn capture-event-box
  [root target box]
  (let [detail #js {:root root
                    :target target
                    :box (clj->js box)
                    :state nil}
        name (or (:name box) (:event box) "custom")]
    (emit-continuity! root (str "capture-box:" name) detail)
    (js->clj (aget detail "state") :keywordize-keys true)))

(defn restore-event-box!
  [root target box state]
  (let [name (or (:name box) (:event box) "custom")]
    (emit-continuity!
     root
     (str "restore-box:" name)
     #js {:root root
          :target target
          :box (clj->js box)
          :state (clj->js state)})))

(defn register-built-in-boxes!
  []
  (reset!
   continuity-boxes
   {"raw-scroll"
    {:capture capture-raw-scroll
     :restore restore-raw-scroll!}

    "anchor-scroll"
    {:capture capture-anchor-scroll
     :restore restore-anchor-scroll!}

    "focus"
    {:capture (fn [root target box]
                (capture-focus root target box))
     :restore (fn [root target _box state]
                (restore-focus! root target state))}

    "inputs"
    {:capture (fn [_root target box]
                (capture-inputs target (:selector box)))
     :restore (fn [_root target _box state]
                (restore-inputs! target state))}

    "details-open"
    {:capture (fn [_root target box]
                (capture-details
                 target
                 (:selector box)
                 (or (:single? box)
                     (:single box)
                     (:single-open box)
                     (:singleOpen box))))
     :restore (fn [_root target box state]
                (restore-details! target (:selector box) state))}

    "js"
    {:capture (fn [root target box]
                (when-some [f (resolve-global-path (:capture box))]
                  (when (js-function? f)
                    (js->clj (f root target (clj->js box))
                             :keywordize-keys true))))
     :restore (fn [root target box state]
                (when-some [f (resolve-global-path (:restore box))]
                  (when (js-function? f)
                    (f root target (clj->js box) (clj->js state)))))}

    "event"
    {:capture capture-event-box
     :restore restore-event-box!}

    "hyperscript"
    {:capture capture-event-box
     :restore restore-event-box!}}))

(defn capture-box
  [root target box]
  (let [box (normalize-box box)
        type (:type box)
        implementation (get @continuity-boxes type)]
    (if-not (and implementation (:capture implementation))
      (do
        (emit-continuity!
         root
         "error"
         #js {:phase "capture"
              :reason "unknown-box-type"
              :box (clj->js box)})
        nil)
      (try
        {:type type
         :name (or (:name box) type)
         :box box
         :state ((:capture implementation) root target box)}
        (catch :default error
          (emit-continuity!
           root
           "error"
           #js {:phase "capture"
                :box (clj->js box)
                :error error})
          nil)))))

(defn restore-box!
  [root target captured]
  (when-some [implementation (get @continuity-boxes (:type captured))]
    (when-some [restore (:restore implementation)]
      (try
        (restore root target (:box captured) (:state captured))
        (catch :default error
          (emit-continuity!
           root
           "error"
           #js {:phase "restore"
                :box (clj->js (:box captured))
                :error error})))))
  target)

(defn continuity-root
  [element]
  (cond
    (matches? element continuity-root-selector) element
    (element? element) (closest element continuity-root-selector)
    :else nil))

(defn continuity-target-id
  [root]
  (or (attr root continuity-fragment-attr)
      (let [target (attr root "hx-target")]
        (when (and target (str/starts-with? target "#"))
          (subs target 1)))))

(defn continuity-target
  [root]
  (when-some [id (continuity-target-id root)]
    (or (find-by-id root id)
        (find-by-id js/document id))))

(defn continuity-root-for-target-id
  [target-id]
  (some
   (fn [root]
     (when (= target-id (continuity-target-id root))
       root))
   (query-all js/document continuity-root-selector)))

(defn add-distinct-root
  [roots root]
  (if (or (nil? root)
          (some #(identical? % root) roots))
    roots
    (conj roots root)))

(defn roots-from-event
  [event]
  (let [direct-roots
        (reduce
         (fn [roots element]
           (add-distinct-root roots (continuity-root element)))
         []
         (event-elements event))

        target-ids (event-target-ids event)

        document-roots
        (keep continuity-root-for-target-id target-ids)

        captured-roots
        (keep
         (fn [[target-id slot]]
           (when (contains? target-ids target-id)
             (:root slot)))
         @continuity-slots)]
    (reduce add-distinct-root
            direct-roots
            (concat document-roots captured-roots))))

(defn lock-height!
  [root target]
  (when (and root target)
    (let [rect (.getBoundingClientRect root)
          previous (.-minHeight (.-style root))
          height (max (.-height rect) (.-offsetHeight root))]
      (when (pos? height)
        (set! (.-minHeight (.-style root)) (str height "px"))
        {:height height :previous previous}))))

(defn release-height-lock!
  [root height-lock]
  (when (and root height-lock)
    (set! (.-minHeight (.-style root)) (or (:previous height-lock) "")))
  root)

(defn capture-continuity-root!
  [root source]
  (let [config (parse-continuity-config root)
        target (continuity-target root)]
    (when (and target (config-enabled? config))
      (let [target-id (continuity-target-id root)
            captured (keep #(capture-box root target %)
                           (boxes-from-config config))
            slot {:source source
                  :root root
                  :target-id target-id
                  :captured-at (now-ms)
                  :config config
                  :captured (vec captured)
                  :fallback-scroll (window-scroll-state)
                  :height-lock (lock-height! root target)}]
        (swap! continuity-slots assoc target-id slot)
        (emit-continuity!
         root
         "captured"
         #js {:source source
              :root root
              :target target
              :targetId target-id
              :count (count captured)
              :heightLocked (boolean (:height-lock slot))})
        slot))))

(defn restore-continuity-root!
  [root immediate-types]
  (let [target-id (continuity-target-id root)
        slot (get @continuity-slots target-id)
        target (continuity-target root)]
    (when slot
      (if target
        (doseq [captured (:captured slot)
                :when (or (nil? immediate-types)
                          (contains? immediate-types (:type captured)))]
          (restore-box! root target captured))
        (restore-window-scroll! (:fallback-scroll slot)))
      (when (nil? immediate-types)
        (when (and (:fallback-scroll slot)
                   (pos? (or (:y (:fallback-scroll slot)) 0)))
          (js/requestAnimationFrame
           (fn []
             (when (zero? (or (.-pageYOffset js/window) 0))
               (restore-window-scroll! (:fallback-scroll slot))))))
        (when (:height-lock slot)
          (js/requestAnimationFrame
           #(release-height-lock! root (:height-lock slot))))
        (swap! continuity-slots dissoc target-id)
        (emit-continuity!
         root
         "restored"
         #js {:source (:source slot)
              :root root
              :target target
              :targetId target-id
              :count (count (:captured slot))})))
    slot))

(defn capture-continuity-from-event!
  [event]
  (doseq [root (roots-from-event event)]
    (capture-continuity-root! root (event-source event))))

(defn restore-continuity-immediately-from-event!
  [event]
  (doseq [root (roots-from-event event)]
    (restore-continuity-root! root #{"details-open"})))

(defn restore-continuity-after-layout-from-event!
  [event]
  (doseq [root (roots-from-event event)]
    (after-layout!
     #(restore-continuity-root! root nil))))

(defn restore-continuity-from-event!
  [event]
  (restore-continuity-immediately-from-event! event)
  (restore-continuity-after-layout-from-event! event))

;; -----------------------------------------------------------------------------
;; Optimistic descriptors, targets, and settlements
;; -----------------------------------------------------------------------------

(defn source-descriptor
  [source]
  (when source
    {:protocol-version (attr source optimistic-protocol-attr)
     :machine (attr source optimistic-machine-attr)
     :transition (attr source optimistic-transition-attr)
     :template-name (attr source optimistic-template-attr)
     :target (attr source optimistic-target-attr)
     :scope (attr source optimistic-scope-attr)
     :base-revision (attr source optimistic-base-revision-attr)
     :pending-label (attr source optimistic-label-attr)
     :projection-mode (attr source optimistic-mode-attr)}))

(defn optimistic-source
  [element]
  (cond
    (matches? element optimistic-source-selector) element
    (element? element) (closest element optimistic-source-selector)
    :else nil))

(defn optimistic-source-from-event
  [event]
  (or (optimistic-source (event-source event))
      (some optimistic-source (event-elements event))))

(defn following-sibling
  [source selector direction]
  (loop [candidate (direction source)]
    (cond
      (nil? candidate) nil
      (matches? candidate selector) candidate
      :else (recur (direction candidate)))))

(defn resolve-extended-selector
  [source selector]
  (let [selector (str/trim (or selector ""))]
    (cond
      (= selector "this")
      source

      (str/starts-with? selector "closest ")
      (closest source (subs selector 8))

      (str/starts-with? selector "find ")
      (query source (subs selector 5))

      (str/starts-with? selector "next ")
      (following-sibling source
                         (subs selector 5)
                         #(.-nextElementSibling %))

      (str/starts-with? selector "previous ")
      (following-sibling source
                         (subs selector 9)
                         #(.-previousElementSibling %))

      :else
      (query js/document selector))))

(defn resolve-optimistic-target
  [source descriptor]
  (resolve-extended-selector source (:target descriptor)))

(defn find-template-in
  [root template-name]
  (when (and root template-name)
    (some
     (fn [template]
       (when (= (attr template optimistic-template-attr)
                template-name)
         template))
     (query-all root
                (str "template[" optimistic-template-attr "]")))))

(defn find-optimistic-template
  [source target descriptor]
  (or (find-template-in (.-parentElement source) (:template-name descriptor))
      (find-template-in (continuity-root source) (:template-name descriptor))
      (find-template-in (continuity-root target) (:template-name descriptor))
      (find-template-in js/document (:template-name descriptor))))

(defn template-root
  [template]
  (when template
    (or (some-> template .-content .-firstElementChild)
        (let [wrapper (.createElement js/document "div")]
          (set! (.-innerHTML wrapper) (or (.-innerHTML template) ""))
          (.-firstElementChild wrapper)))))

(defn execution-id
  []
  (str "gesso-optimistic-" (random-uuid)))

(defn current-target-lock
  [target]
  (when target
    (aget target "__gessoOptimisticExecution")))

(defn reserve-target!
  [target id]
  (let [existing (current-target-lock target)]
    (cond
      (or (nil? existing) (= existing id))
      (do
        (aset target "__gessoOptimisticExecution" id)
        (swap! target-locks assoc id target)
        true)

      :else
      false)))

(defn release-target-reservation!
  [target id]
  (when (and target (= id (current-target-lock target)))
    (js-delete target "__gessoOptimisticExecution"))
  (swap! target-locks dissoc id)
  true)

(defn simple-label-element
  [source]
  (or (query source "[data-gesso-button-label]")
      (query source "[data-gesso-optimistic-label-target]")
      (when (and (= "input" (some-> (.-tagName source) str/lower-case))
                 (#{"submit" "button"} (some-> (.-type source) str/lower-case)))
        source)))

(defn mark-source-pressed!
  [source descriptor id]
  (let [label-element (simple-label-element source)
        state {:source source
               :descriptor descriptor
               :execution-id id
               :disabled (when (some? (.-disabled source))
                           (boolean (.-disabled source)))
               :aria-disabled (attr source "aria-disabled")
               :label-element label-element
               :label (when label-element
                        (if (= "input" (some-> (.-tagName label-element)
                                               str/lower-case))
                          (.-value label-element)
                          (.-textContent label-element)))}]
    (set-attr! source optimistic-pressed-attr id)
    (set-attr! source "aria-busy" "true")
    ;; Do not set the native disabled property during pointerdown. A disabled
    ;; button may suppress the click that HTMX needs in order to submit the
    ;; request. The request-start effect applies the real disabled state.
    (set-attr! source "aria-disabled" "true")
    (when-let [pending-label (:pending-label descriptor)]
      (when label-element
        (if (= "input" (some-> (.-tagName label-element) str/lower-case))
          (set! (.-value label-element) pending-label)
          (set! (.-textContent label-element) pending-label))))
    state))

(defn clear-source-pressed!
  [state]
  (when-some [source (:source state)]
    (remove-attr! source optimistic-pressed-attr)
    (remove-attr! source "aria-busy")
    (when (some? (:disabled state))
      (set! (.-disabled source) (:disabled state)))
    (if (nil? (:aria-disabled state))
      (remove-attr! source "aria-disabled")
      (set-attr! source "aria-disabled" (:aria-disabled state)))
    (when-some [label-element (:label-element state)]
      (when (connected? label-element)
        (if (= "input" (some-> (.-tagName label-element) str/lower-case))
          (set! (.-value label-element) (:label state))
          (set! (.-textContent label-element) (:label state))))))
  true)

(defn prevent-event!
  [event]
  (when (.-preventDefault event)
    (.preventDefault event))
  (when (.-stopImmediatePropagation event)
    (.stopImmediatePropagation event))
  false)

(defn ensure-press-state!
  [source]
  (let [descriptor (source-descriptor source)
        target (resolve-optimistic-target source descriptor)
        existing-id (aget source "__gessoOptimisticPressExecution")]
    (cond
      (and existing-id (get @press-states existing-id))
      (get @press-states existing-id)

      (nil? target)
      (do
        (emit-optimistic-error!
         (continuity-root source)
         #js {:phase "press"
              :reason "no-target"
              :source source})
        nil)

      (current-target-lock target)
      nil

      :else
      (let [id (execution-id)
            pressed (mark-source-pressed! source descriptor id)
            state (assoc pressed
                         :target target
                         :snapshot (element-snapshot target))
            watchdog (js/setTimeout
                      (fn []
                        (when-let [stale (get @press-states id)]
                          (clear-source-pressed! stale)
                          (release-target-reservation! target id)
                          (swap! press-states dissoc id)
                          (js-delete source "__gessoOptimisticPressExecution")))
                      pre-request-timeout-ms)
            state (assoc state :watchdog watchdog)]
        (when (reserve-target! target id)
          (aset source "__gessoOptimisticPressExecution" id)
          (swap! press-states assoc id state)
          state)))))

(defn settlement-marker?
  [element]
  (and (element? element)
       (= "template" (some-> (.-tagName element) str/lower-case))
       (= "true" (attr element optimistic-settlement-attr))))

(defn marker->settlement
  [marker]
  (when (settlement-marker? marker)
    {:execution-id (attr marker optimistic-execution-attr)
     :transition (attr marker optimistic-transition-attr)
     :scope (attr marker optimistic-scope-attr)
     :outcome (some-> (attr marker optimistic-outcome-attr) keyword)
     :revision (attr marker optimistic-revision-attr)
     :command-applied? (parse-bool
                        (attr marker optimistic-command-applied-attr))
     :reason (attr marker optimistic-reason-attr)}))

(defn parse-response-root
  [event]
  (let [xhr (detail-field event "xhr")
        text (when xhr (.-responseText xhr))]
    (when (and (string? text) (not (str/blank? text)))
      (let [template (.createElement js/document "template")]
        (set! (.-innerHTML template) text)
        (.-content template)))))

(defn settlement-from-response
  [event expected-id]
  (when-some [root (parse-response-root event)]
    (some
     (fn [marker]
       (let [settlement (marker->settlement marker)]
         (when (= expected-id (:execution-id settlement))
           (assoc settlement :response-root root))))
     (query-all root
                (str "template[" optimistic-settlement-attr "='true']")))))

(defn scope-elements
  [root scope]
  (filter
   #(= scope (attr % optimistic-scope-attr))
   (query-all root (str "[" optimistic-scope-attr "]"))))

(defn canonical-element-from-settlement
  [settlement]
  (let [scope (:scope settlement)
        response-root (:response-root settlement)]
    (or
     (some
      #(when-not (settlement-marker? %) %)
      (scope-elements response-root scope))
     (some
      #(when-not (= "true" (attr % optimistic-pending-attr)) %)
      (scope-elements js/document scope)))))

(defn current-scope-target
  [ctx]
  (let [prepared (get ctx machine/prepared-key)
        descriptor (:descriptor prepared)
        source (:source prepared)
        target (:target prepared)
        scope (:scope descriptor)]
    (or
     (when (and target (connected? target)) target)
     (some
      #(when (= (attr % optimistic-active-attr)
                (machine/execution-id-key ctx))
         %)
      (scope-elements js/document scope))
     (some
      #(when (= scope (attr % optimistic-scope-attr)) %)
      (scope-elements js/document scope))
     (when (connected? source)
       (resolve-optimistic-target source descriptor)))))

(defn revision-newer-or-equal?
  [actual expected]
  (cond
    (nil? expected) true
    (nil? actual) false

    (and (some? (parse-int actual))
         (some? (parse-int expected)))
    (>= (parse-int actual) (parse-int expected))

    :else
    (= (str actual) (str expected))))

;; -----------------------------------------------------------------------------
;; Browser effect handlers
;; -----------------------------------------------------------------------------

(defn prepare-effect-handler
  [ctx]
  (let [source (:source ctx)
        descriptor (:descriptor ctx)
        target (or (:target ctx)
                   (resolve-optimistic-target source descriptor))
        template (find-optimistic-template source target descriptor)
        projection (template-root template)
        root (or (continuity-root source)
                 (continuity-root target)
                 (.-documentElement js/document))]
    (when-not (= optimistic-protocol-version (:protocol-version descriptor))
      (throw
       (ex-info "Unsupported Gesso optimistic protocol version."
                {:expected optimistic-protocol-version
                 :actual (:protocol-version descriptor)})))
    (when-not target
      (throw
       (ex-info "Optimistic target could not be resolved."
                {:descriptor descriptor})))
    (when-not projection
      (throw
       (ex-info "Optimistic projection template could not be resolved."
                {:descriptor descriptor})))
    (validate-projection-root! target projection)
    {:source source
     :descriptor descriptor
     :target target
     :template template
     :projection projection
     :root root}))

(defn acquire-lock-effect-handler
  [ctx]
  (let [prepared (get ctx machine/prepared-key)
        target (:target prepared)
        id (machine/execution-id-key ctx)]
    {:acquired? (reserve-target! target id)
     :target target}))

(defn capture-snapshot-effect-handler
  [ctx]
  (or (:snapshot ctx)
      (some-> (get ctx machine/prepared-key)
              :target
              element-snapshot)))

(defn capture-continuity-effect-handler
  [ctx]
  (let [{:keys [root target]} (get ctx machine/prepared-key)
        config (parse-continuity-config root)]
    (when (and root target (config-enabled? config))
      {:root root
       :target target
       :captured
       (vec
        (keep #(capture-box root target %)
              (boxes-from-config config)))})))

(defn mark-pending-effect-handler
  [ctx]
  (let [{:keys [source target descriptor]} (get ctx machine/prepared-key)
        id (machine/execution-id-key ctx)]
    (set-attr! target optimistic-active-attr id)
    (set-attr! target optimistic-pending-attr "true")
    (set-attr! target optimistic-locked-attr "true")
    (set-attr! target "aria-busy" "true")
    (remove-attr! target optimistic-accepted-attr)
    (set-attr! source optimistic-execution-attr id)
    (when (some? (.-disabled source))
      (set! (.-disabled source) true))
    {:execution-id id
     :mode (:projection-mode descriptor)}))

(defn install-projection-effect-handler
  [ctx]
  (let [{:keys [target projection]} (get ctx machine/prepared-key)
        id (machine/execution-id-key ctx)]
    (copy-element-into-target! target projection)
    (set-attr! target optimistic-active-attr id)
    (set-attr! target optimistic-pending-attr "true")
    (set-attr! target optimistic-locked-attr "true")
    (set-attr! target "aria-busy" "true")
    (htmx-process! target)
    {:installed? true
     :target target}))

(defn restore-continuity-effect-handler
  [ctx]
  (when-let [{:keys [root target captured]}
             (get ctx machine/continuity-key)]
    (doseq [item captured]
      (restore-box! root target item)))
  true)

(declare dispatch-machine-event!)

(defn schedule-timeout-effect-handler
  [ctx]
  (let [id (machine/execution-id-key ctx)
        handle (js/setTimeout
                #(dispatch-machine-event!
                  id
                  machine/timeout-event
                  {machine/failure-reason-key :settlement-timeout})
                default-settlement-timeout-ms)]
    (when-some [old (get @timeout-handles id)]
      (js/clearTimeout old))
    (swap! timeout-handles assoc id handle)
    handle))

(defn observe-canonical-effect-handler
  [_ctx data]
  data)

(defn apply-settlement-effect-handler
  [ctx settlement]
  (let [id (machine/execution-id-key ctx)
        prepared (get ctx machine/prepared-key)
        descriptor (:descriptor prepared)
        target (current-scope-target ctx)
        canonical (canonical-element-from-settlement settlement)
        expected-revision (:revision settlement)
        canonical-revision (when canonical
                             (attr canonical optimistic-revision-attr))]
    (when-not (= id (:execution-id settlement))
      (throw
       (ex-info "Optimistic settlement execution does not match."
                {:execution-id id
                 :settlement settlement})))
    (when-not (= (:transition descriptor) (:transition settlement))
      (throw
       (ex-info "Optimistic settlement transition does not match."
                {:descriptor descriptor
                 :settlement settlement})))
    (when-not (= (:scope descriptor) (:scope settlement))
      (throw
       (ex-info "Optimistic settlement scope does not match."
                {:descriptor descriptor
                 :settlement settlement})))
    (cond
      (and target
           canonical
           (revision-newer-or-equal? canonical-revision expected-revision))
      (do
        (copy-element-into-target! target canonical)
        (htmx-process! target)
        {:applied? true
         :source :response
         :target target
         :revision canonical-revision})

      (and target
           (not (#{:rejected :failed} (:outcome settlement))))
      (do
        ;; A confirmed/reconciled response may intentionally settle the exact
        ;; projected subtree while authoritative OOB replacements update only
        ;; larger affected scopes. In that case the projection is accepted, but
        ;; diagnostics still record the absence of canonical scope markup.
        (set-attr! target optimistic-accepted-attr "true")
        {:applied? true
         :source :accepted-projection
         :target target
         :missing-canonical? true})

      target
      (do
        (restore-snapshot! target (get ctx machine/snapshot-key))
        (htmx-process! target)
        {:applied? true
         :source :snapshot-fallback
         :target target
         :missing-canonical? true})

      :else
      {:applied? false
       :reason :no-current-target})))

(defn recover-effect-handler
  [ctx recovery]
  (let [target (current-scope-target ctx)
        id (machine/execution-id-key ctx)]
    (when (and target
               (= id (attr target optimistic-active-attr)))
      (restore-snapshot! target (get ctx machine/snapshot-key))
      (htmx-process! target))
    (assoc recovery :restored? (boolean target))))

(defn cancel-timeout-effect-handler
  [ctx]
  (let [id (machine/execution-id-key ctx)]
    (when-some [handle (get @timeout-handles id)]
      (js/clearTimeout handle))
    (swap! timeout-handles dissoc id)
    true))

(defn clear-pending-effect-handler
  [ctx]
  (let [id (machine/execution-id-key ctx)
        target (current-scope-target ctx)
        source (or (get @execution-sources id)
                   (some-> (get ctx machine/prepared-key) :source))]
    (when target
      (when (= id (attr target optimistic-active-attr))
        (remove-attr! target optimistic-active-attr)
        (remove-attr! target optimistic-pending-attr)
        (remove-attr! target optimistic-locked-attr)
        (remove-attr! target optimistic-accepted-attr)
        (remove-attr! target "aria-busy")))
    (when source
      (remove-attr! source optimistic-execution-attr))
    (when-some [press-state (get @press-states id)]
      (clear-source-pressed! press-state)
      (when-some [watchdog (:watchdog press-state)]
        (js/clearTimeout watchdog))
      (swap! press-states dissoc id)
      (when-some [press-source (:source press-state)]
        (js-delete press-source "__gessoOptimisticPressExecution")))
    true))

(defn release-lock-effect-handler
  [ctx]
  (let [id (machine/execution-id-key ctx)
        target (or (get @target-locks id)
                   (current-scope-target ctx))]
    (release-target-reservation! target id)))

(def browser-effect-handlers
  {machine/prepare-effect prepare-effect-handler
   machine/acquire-lock-effect acquire-lock-effect-handler
   machine/capture-snapshot-effect capture-snapshot-effect-handler
   machine/capture-continuity-effect capture-continuity-effect-handler
   machine/mark-pending-effect mark-pending-effect-handler
   machine/install-projection-effect install-projection-effect-handler
   machine/restore-continuity-effect restore-continuity-effect-handler
   machine/schedule-timeout-effect schedule-timeout-effect-handler
   machine/observe-canonical-effect observe-canonical-effect-handler
   machine/apply-settlement-effect apply-settlement-effect-handler
   machine/recover-effect recover-effect-handler
   machine/cancel-timeout-effect cancel-timeout-effect-handler
   machine/clear-pending-effect clear-pending-effect-handler
   machine/release-lock-effect release-lock-effect-handler})

;; -----------------------------------------------------------------------------
;; Machine journal
;; -----------------------------------------------------------------------------

(defn complete-execution!
  [id execution]
  (swap! executions dissoc id)
  (swap! execution-sources dissoc id)
  (emit-optimistic!
   (some-> (machine/context-key execution)
           :source
           continuity-root)
   "completed"
   #js {:executionId id
        :result (clj->js (machine/execution-result execution))})
  execution)

(defn store-execution!
  [id source execution]
  (if (machine/suspended? execution)
    (do
      (swap! executions assoc id execution)
      (swap! execution-sources assoc id source)
      execution)
    (complete-execution! id execution)))

(defn start-execution!
  [source press-state event]
  (let [id (:execution-id press-state)
        descriptor (:descriptor press-state)
        context {:source source
                 :target (:target press-state)
                 :snapshot (:snapshot press-state)
                 :descriptor descriptor
                 :request-event event}
        execution (machine/start
                   machine/default-machine
                   {:execution-id id
                    :context context
                    :handlers browser-effect-handlers})]
    (store-execution! id source execution)))

(defn dispatch-machine-event!
  [id event-type data]
  (when-some [execution (get @executions id)]
    (when (machine/accepts-event? execution event-type)
      (try
        (let [next-execution
              (machine/resume
               machine/default-machine
               execution
               (machine/event event-type data)
               {:handlers browser-effect-handlers})]
          (store-execution!
           id
           (get @execution-sources id)
           next-execution))
        (catch :default error
          (emit-optimistic-error!
           (some-> (get @execution-sources id) continuity-root)
           #js {:phase "resume"
                :executionId id
                :eventType (str event-type)
                :error error})
          ;; A machine exception must not orphan the target lock.
          (let [ctx (machine/context-key execution)]
            (cancel-timeout-effect-handler
             (assoc ctx machine/execution-id-key id))
            (clear-pending-effect-handler
             (assoc ctx machine/execution-id-key id))
            (release-lock-effect-handler
             (assoc ctx machine/execution-id-key id)))
          (swap! executions dissoc id)
          (swap! execution-sources dissoc id))))))

(defn cancel-execution!
  ([id]
   (cancel-execution! id :cancelled))
  ([id reason]
   (dispatch-machine-event!
    id
    machine/cancel-event
    {machine/failure-reason-key reason})))

(defn execution-for-source
  [source]
  (when-some [id (or (aget source "__gessoOptimisticPressExecution")
                     (get @request-executions (node-uid source)))]
    [id (get @executions id)]))

;; -----------------------------------------------------------------------------
;; Canonical observations
;; -----------------------------------------------------------------------------

(defn observed-scopes
  [event]
  (reduce
   (fn [result element]
     (let [candidates (cond-> (query-all element
                                         (str "[" optimistic-scope-attr "]"))
                        (has-attr? element optimistic-scope-attr)
                        (conj element))]
       (reduce
        (fn [result candidate]
          (let [scope (attr candidate optimistic-scope-attr)]
            (if scope
              (assoc result scope
                     {:scope scope
                      :revision (attr candidate optimistic-revision-attr)
                      :element candidate})
              result)))
        result
        candidates)))
   {}
   (event-elements event)))

(defn notify-canonical-observations!
  [event]
  (let [observed (observed-scopes event)]
    (doseq [[id execution] @executions
            :let [ctx (machine/context-key execution)
                  descriptor (:descriptor ctx)
                  observation (get observed (:scope descriptor))]
            :when observation]
      (dispatch-machine-event!
       id
       machine/canonical-replaced-event
       observation))))

;; -----------------------------------------------------------------------------
;; HTMX and browser event adapters
;; -----------------------------------------------------------------------------

(defn on-pointerdown!
  [event]
  (when-some [source (optimistic-source (.-target event))]
    (when-not (ensure-press-state! source)
      (when (current-target-lock
             (resolve-optimistic-target source (source-descriptor source)))
        (prevent-event! event)))))

(defn on-click-capture!
  [event]
  (when-some [source (optimistic-source (.-target event))]
    (when-not (ensure-press-state! source)
      (prevent-event! event))))

(defn on-submit-capture!
  [event]
  (when-some [source (optimistic-source (.-target event))]
    (when-not (ensure-press-state! source)
      (prevent-event! event))))

(defn on-config-request!
  [event]
  (when-some [source (optimistic-source-from-event event)]
    (when-some [press-state (ensure-press-state! source)]
      (let [detail (event-detail event)
            headers (or (aget detail "headers")
                        (let [headers #js {}]
                          (aset detail "headers" headers)
                          headers))]
        (aset headers optimistic-request-header (:execution-id press-state))))))

(defn on-before-request!
  [event]
  (if-some [source (optimistic-source-from-event event)]
    (if-some [press-state (ensure-press-state! source)]
      (let [id (:execution-id press-state)]
        (when-some [watchdog (:watchdog press-state)]
          (js/clearTimeout watchdog))
        (swap! press-states update id dissoc :watchdog)
        (swap! request-executions assoc (node-uid source) id)
        (try
          (start-execution! source press-state event)
          (catch :default error
            (emit-optimistic-error!
             (continuity-root source)
             #js {:phase "start"
                  :executionId id
                  :error error})
            (clear-source-pressed! press-state)
            (release-target-reservation! (:target press-state) id)
            (swap! press-states dissoc id)
            (js-delete source "__gessoOptimisticPressExecution"))))
      (prevent-event! event))
    (capture-continuity-from-event! event)))

(defn htmx-request-successful?
  [event]
  (let [detail (event-detail event)
        explicit (when detail (aget detail "successful"))
        xhr (when detail (aget detail "xhr"))
        status (when xhr (.-status xhr))]
    (if (boolean? explicit)
      explicit
      (and (number? status) (<= 200 status 399)))))

(defn failure-reason
  [event]
  (keyword
   (case (.-type event)
     "htmx:responseError" "response-error"
     "htmx:sendError" "send-error"
     "htmx:timeout" "timeout"
     "htmx:abort" "aborted"
     "request-failed")))

(defn on-after-request!
  [event]
  (when-some [source (optimistic-source-from-event event)]
    (when-some [[id execution] (execution-for-source source)]
      (when execution
        (let [successful? (htmx-request-successful? event)
              settlement (when successful?
                           (settlement-from-response event id))]
          (dispatch-machine-event!
           id
           machine/request-finished-event
           {machine/successful-key successful?
            machine/settlement-key settlement
            machine/failure-reason-key
            (when-not successful? (failure-reason event))})))
      (swap! request-executions dissoc (node-uid source)))))

(defn on-request-failed!
  [event]
  (when-some [source (optimistic-source-from-event event)]
    (when-some [[id execution] (execution-for-source source)]
      (when execution
        (dispatch-machine-event!
         id
         machine/request-finished-event
         {machine/successful-key false
          machine/failure-reason-key (failure-reason event)}))
      (swap! request-executions dissoc (node-uid source)))))

(defn on-before-swap!
  [event]
  (capture-continuity-from-event! event))

(defn on-after-swap!
  [event]
  (notify-canonical-observations! event)
  (restore-continuity-immediately-from-event! event))

(defn on-after-settle!
  [event]
  (notify-canonical-observations! event)
  (restore-continuity-after-layout-from-event! event))

(defn on-oob-before-swap!
  [event]
  (capture-continuity-from-event! event))

(defn on-oob-after-swap!
  [event]
  (notify-canonical-observations! event)
  (restore-continuity-from-event! event))

(defn on-sse-before-message!
  [event]
  (capture-continuity-from-event! event))

(defn on-sse-message!
  [event]
  (notify-canonical-observations! event)
  (restore-continuity-from-event! event))

(defn on-before-cleanup!
  [event]
  (let [target (event-target event)]
    (doseq [[id execution] @executions
            :let [ctx (machine/context-key execution)
                  current (current-scope-target
                           (assoc ctx machine/execution-id-key id))]
            :when (and target current (contains-node? target current))]
      (cancel-execution! id :target-cleanup))
    (doseq [root (cond-> (query-all target continuity-root-selector)
                   (matches? target continuity-root-selector)
                   (conj target))]
      (when-some [slot (get @continuity-slots (continuity-target-id root))]
        (release-height-lock! root (:height-lock slot))
        (swap! continuity-slots dissoc (continuity-target-id root))))))

(defn mark-user-scroll-intent!
  [_event]
  (reset! last-user-scroll-intent-at (now-ms)))

(defn scrolling-key?
  [event]
  (contains?
   #{"ArrowUp"
     "ArrowDown"
     "ArrowLeft"
     "ArrowRight"
     "PageUp"
     "PageDown"
     "Home"
     "End"
     " "
     "Spacebar"}
   (.-key event)))

(defn on-keydown!
  [event]
  (when (scrolling-key? event)
    (mark-user-scroll-intent! event)))

;; -----------------------------------------------------------------------------
;; Public browser API
;; -----------------------------------------------------------------------------

(defn register-box!
  [type implementation]
  (when (and type implementation)
    (let [type (if (keyword? type) (name type) (str type))
          implementation
          (if (map? implementation)
            implementation
            {:capture (aget implementation "capture")
             :restore (aget implementation "restore")})]
      (swap! continuity-boxes assoc type implementation)))
  true)

(defn execution-summary
  [[id execution]]
  {:execution-id id
   :machine (get execution machine/machine-name-key)
   :state (get execution machine/state-key)
   :status (get execution machine/status-key)
   :awaiting (get execution machine/awaiting-key)})

(defn execution-summaries
  []
  (mapv execution-summary @executions))

(defn runtime-state
  []
  {:version runtime-version
   :executions (execution-summaries)
   :target-lock-count (count @target-locks)
   :continuity-slot-count (count @continuity-slots)})

(defn install-public-api!
  []
  (let [gesso-live (or (aget js/window "gessoLive") #js {})
        continuity #js {:version runtime-version
                        :parseConfig parse-continuity-config
                        :boxesFromConfig boxes-from-config
                        :captureFromEvent capture-continuity-from-event!
                        :restoreFromEvent restore-continuity-from-event!
                        :registerBox register-box!}
        optimistic #js {:version runtime-version
                        :state runtime-state
                        :cancel cancel-execution!
                        :dispatch dispatch-machine-event!
                        :executions (fn [] (clj->js (execution-summaries)))}]
    (aset gesso-live "continuity" continuity)
    (aset gesso-live "optimistic" optimistic)
    (aset gesso-live "version" runtime-version)
    (aset js/window "gessoLive" gesso-live)))

(defn add-document-listener!
  ([name handler]
   (.addEventListener js/document name handler))
  ([name handler capture?]
   (.addEventListener js/document name handler capture?)))

(defn add-window-listener!
  [name handler options]
  (.addEventListener js/window name handler options))

(defn ^:export init!
  []
  (when (compare-and-set! initialized? false true)
    (register-built-in-boxes!)

    (add-window-listener!
     "wheel"
     mark-user-scroll-intent!
     #js {:passive true :capture true})
    (add-window-listener!
     "touchmove"
     mark-user-scroll-intent!
     #js {:passive true :capture true})
    (add-window-listener!
     "pointerdown"
     mark-user-scroll-intent!
     #js {:passive true :capture true})
    (add-window-listener! "keydown" on-keydown! true)

    (add-document-listener! "pointerdown" on-pointerdown! true)
    (add-document-listener! "click" on-click-capture! true)
    (add-document-listener! "submit" on-submit-capture! true)

    (add-document-listener! "htmx:configRequest" on-config-request!)
    (add-document-listener! "htmx:beforeRequest" on-before-request!)
    (add-document-listener! "htmx:afterRequest" on-after-request!)
    (add-document-listener! "htmx:responseError" on-request-failed!)
    (add-document-listener! "htmx:sendError" on-request-failed!)
    (add-document-listener! "htmx:timeout" on-request-failed!)
    (add-document-listener! "htmx:abort" on-request-failed!)

    (add-document-listener! "htmx:beforeSwap" on-before-swap!)
    (add-document-listener! "htmx:afterSwap" on-after-swap!)
    (add-document-listener! "htmx:afterSettle" on-after-settle!)
    (add-document-listener! "htmx:oobBeforeSwap" on-oob-before-swap!)
    (add-document-listener! "htmx:oobAfterSwap" on-oob-after-swap!)
    (add-document-listener! "htmx:sseBeforeMessage" on-sse-before-message!)
    (add-document-listener! "htmx:sseMessage" on-sse-message!)
    (add-document-listener! "htmx:beforeCleanupElement" on-before-cleanup!)

    (install-public-api!))
  true)

(init!)
