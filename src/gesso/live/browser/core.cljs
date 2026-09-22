(ns gesso.live.browser.core
  "Thin HTMX/browser normalization boundary for Gesso Live.

   Semantic browser state lives in gesso.live.browser.adapter. Imperative
   effect execution and opaque host resources live in gesso.live.browser.shell.
   This namespace owns only the physical integration between documented HTMX
   lifecycle observations and those two layers.

   In particular, core owns:

   - locating the stable Gesso Live fragment root associated with an HTMX event
   - correlating one adapter-issued fragment request generation with one
     physical HTMX request
   - normalizing HTMX lifecycle callbacks into plain adapter events
   - physically allowing/cancelling HTMX requests and swaps when the adapter
     requests that disposition
   - triggering an HTMX-owned refresh from :fragment/refresh effects
   - translating fragment removal into :fragment/retire
   - exposing a small explicit invalidation entry point for SSE/other wakeups
   - turning browser recovery boundaries into advisory managed-fragment repair
   - installing/removing the documented browser listeners on their native
     Document/Window owners

   It deliberately does not:

   - decide whether a request, swap, callback, or basis is current
   - compare authoritative bases or progression requirements
   - advance Choreo machines directly
   - own optimistic policy
   - own continuity semantics
   - retain raw DOM/HTMX/XHR objects in AdapterState
   - issue XMLHttpRequest/fetch directly
   - recreate HTMX request, target, swap, include, or synchronization behavior
   - treat lifecycle recovery as authoritative progression evidence

   Physical request correlation is intentionally outside AdapterState. A stable
   fragment root may temporarily be associated with adapter generation data and
   an XHR object in WeakMaps, but those host values are used only to normalize
   later HTMX callbacks. The adapter remains the sole semantic arbiter.

   The adapter emits :fragment/refresh. Core responds by asking HTMX to trigger
   `gesso:live-refresh` on the stable fragment root. Core never synthesizes an
   HTTP request itself.

   Browser lifecycle recovery follows the same rule. `online`, persisted
   `pageshow`, and foreground `visibilitychange` are advisory evidence that the
   browser may have missed invalidations. Core therefore notifies AdapterState
   for each currently managed root; AdapterState still owns generation and
   single-flight/coalescing semantics.

   Continuity defaults to one per-core physical gesso.live.browser.continuity
   runtime. That runtime captures/restores only browser-local presentation
   state; adapter generation ownership and semantic completion remain in
   adapter/shell. Applications may configure the physical runtime or replace
   individual continuity handler seams without changing the adapter protocol."
  (:require
   [cljs.reader :as reader]
   [clojure.string :as str]
   [gesso.live.browser.continuity :as continuity]
   [gesso.live.browser.shell :as shell]
   [gesso.live.progression :as progression]
   [gesso.live.progression.http :as progression.http]))

;; =============================================================================
;; Public identity / browser vocabulary
;; =============================================================================

(def runtime-version
  "3.2.0-dev")

(def runtime-type
  :gesso.live.browser.core/runtime)

(def fragment-attribute
  "data-gesso-live-fragment")

(def fragment-selector
  (str "[" fragment-attribute "]"))

(def refresh-event-name
  "gesso:live-refresh")

(def invalidated-event-name
  "gesso:live-invalidated")

(def invalidation-listener-attribute
  "data-gesso-live-invalidation")

(def sse-before-message-event-name
  "htmx:sseBeforeMessage")

(def sse-open-event-name
  "htmx:sseOpen")

(def managed-request-marker
  "gesso-live-managed-request")

(def failure-event-types
  #{"htmx:responseError"
    "htmx:sendError"
    "htmx:timeout"
    "htmx:abort"})

(def listener-specs
  "Document-owned lifecycle events observed by the browser core.

   Core remains the single owner of Document/HTMX listener registration.
   Higher browser realizations may register read/write event observers through
   register-event-observer! without installing competing document listeners.

   `visibilitychange` is included here because its native owner is Document.
   Window-owned recovery events (`online` and `pageshow`) are deliberately
   registered separately and are not part of the shared HTMX observer seam."
  [["htmx:configRequest" :config-request]
   ["htmx:beforeRequest" :before-request]
   ["htmx:beforeSend" :observe-only]
   ["htmx:beforeSwap" :before-swap]
   ["htmx:afterSwap" :after-swap]
   ["htmx:afterRequest" :after-request]
   ["htmx:responseError" :request-failed]
   ["htmx:sendError" :request-failed]
   ["htmx:timeout" :request-failed]
   ["htmx:abort" :request-failed]
   ["htmx:beforeCleanupElement" :before-cleanup]
   [sse-before-message-event-name :sse-before-message]
   [sse-open-event-name :sse-open]
   [invalidated-event-name :invalidated]
   ["visibilitychange" :visibility-change]])

(def window-lifecycle-listener-specs
  "Window-owned advisory convergence-repair events.

   These do not expose a generic raw-event observation seam. They exist only to
   re-enter the managed fragment adapter after browser lifecycle boundaries
   where advisory invalidations may have been missed."
  [["online" :online]
   ["pageshow" :pageshow]])

(def option-keys
  #{:document
    :htmx
    :request-id-fn
    :authoritative-from-event
    :handlers
    :continuity-options
    :continuity-capture!
    :continuity-restore!
    :continuity-release!
    :shell-options})

(def protected-handler-kinds
  "Physical effects whose implementation belongs to this HTMX normalization
   boundary and cannot be replaced by application handlers."
  #{:fragment/refresh
    :htmx/allow-request
    :htmx/cancel-request
    :htmx/allow-swap
    :htmx/cancel-swap
    :continuity/capture
    :continuity/restore
    :continuity/release})

;; =============================================================================
;; Errors / validation
;; =============================================================================

(defn- core-error
  ([kind message data]
   (core-error kind message data nil))
  ([kind message data cause]
   (ex-info
    message
    (merge
     {:error/type :gesso.live.browser.core/error
      :error/kind kind}
     data)
    cause)))

(defn- require-map!
  [label value]
  (when-not (map? value)
    (throw
     (core-error
      :invalid-map
      (str label " must be a map.")
      {:label label
       :value value})))
  value)

(defn- require-callable!
  [label value]
  (when-not (fn? value)
    (throw
     (core-error
      :invalid-callable
      (str label " must be callable.")
      {:label label
       :value value})))
  value)

(defn- require-optional-callable!
  [label value]
  (when (some? value)
    (require-callable! label value))
  value)

(defn- check-option-keys!
  [options]
  (let [unknown (seq (remove option-keys (keys options)))]
    (when unknown
      (throw
       (core-error
        :unknown-options
        "Gesso Live browser core options contain unsupported keys."
        {:unknown-keys (set unknown)
         :allowed-keys option-keys}))))
  options)

(defn- require-nonblank-string!
  [label value]
  (when-not (and (string? value)
                 (not (str/blank? value)))
    (throw
     (core-error
      :invalid-string
      (str label " must be a non-blank string.")
      {:label label
       :value value})))
  value)

;; =============================================================================
;; Small browser helpers
;; =============================================================================

(defn event-detail
  [event]
  (when event
    (.-detail event)))

(defn detail-field
  [event field]
  (when-let [detail (event-detail event)]
    (aget detail field)))

(defn event-type
  [event]
  (some-> event .-type str))

(defn xhr-from-event
  [event]
  (detail-field event "xhr"))

(defn request-successful?
  "Normalize HTMX request success without treating HTTP status as semantic
   command success. This function says only whether the physical HTTP request
   completed successfully enough for HTMX."
  [event]
  (let [detail (event-detail event)
        explicit (when detail
                   (aget detail "successful"))
        xhr (when detail
              (aget detail "xhr"))
        status (when xhr
                 (.-status xhr))]
    (if (boolean? explicit)
      explicit
      (boolean
       (and (number? status)
            (<= 200 status 399))))))

(defn failure-reason
  "Return a transport/lifecycle reason only. It is not a domain outcome."
  [event]
  (case (event-type event)
    "htmx:responseError" :http/response-error
    "htmx:sendError" :http/send-error
    "htmx:timeout" :http/timeout
    "htmx:abort" :http/aborted
    :http/request-failed))

(defn prevent-event!
  [event]
  (when-let [prevent-default (some-> event .-preventDefault)]
    (when (= "function"
             (js* "typeof ~{}" prevent-default))
      (.call prevent-default event)))
  false)

(defn cancel-swap-event!
  "Physically suppress one HTMX swap."
  [event]
  (when-let [detail (event-detail event)]
    (aset detail "shouldSwap" false))
  (prevent-event! event)
  false)

(defn- node-element?
  [value]
  (boolean
   (and value
        (= 1 (.-nodeType value)))))

(defn- element-attr
  [element name]
  (when (node-element? element)
    (.getAttribute element name)))

(defn fragment-id-from-root
  [root]
  (some-> (element-attr root fragment-attribute)
          str
          not-empty))

(defn fragment-root?
  [element]
  (boolean
   (and (node-element? element)
        (some? (fragment-id-from-root element)))))

(defn managed-fragment-root?
  "Return true when root is a Gesso-managed fragment whose HTMX request path
   is owned by the adapter.

   Managed server markup deliberately gives the stable root one request trigger:
   gesso:live-refresh. Legacy fragments with direct sse:* triggers therefore do
   not enter this path while migration is in progress."
  [root]
  (boolean
   (and (fragment-root? root)
        (= refresh-event-name
           (some-> (element-attr root "hx-trigger")
                   str
                   str/trim)))))

(defn invalidation-listener?
  "Return true when element is the non-swapping SSE listener owned by one
   managed fragment. The attribute value is the logical fragment id."
  [element]
  (boolean
   (and (node-element? element)
        (some-> (element-attr element invalidation-listener-attribute)
                str
                not-empty))))

(defn fragment-root-from-element
  "Return the nearest stable Gesso Live fragment root for element."
  [element]
  (cond
    (fragment-root? element)
    element

    (node-element? element)
    (let [closest (.-closest element)]
      (when (= "function"
               (js* "typeof ~{}" closest))
        (.call closest element fragment-selector)))

    :else
    nil))

(defn event-elements
  "Candidate physical elements carried by documented HTMX event shapes."
  [event]
  (let [candidates [(detail-field event "elt")
                    (detail-field event "target")
                    (when event (.-target event))]]
    (reduce
     (fn [result candidate]
       (if (and (node-element? candidate)
                (not-any? #(identical? % candidate) result))
         (conj result candidate)
         result))
     []
     candidates)))

(defn fragment-root-from-event
  "Resolve the stable behavior-owning fragment root associated with an HTMX
   lifecycle event. Returns nil for unrelated HTMX traffic."
  [event]
  (some fragment-root-from-element
        (event-elements event)))

(defn- nested-fragment-roots
  [element]
  (if-not (node-element? element)
    []
    (let [nested
          (try
            (vec (array-seq (.querySelectorAll element fragment-selector)))
            (catch :default _
              []))]
      (if (fragment-root? element)
        (into [element]
              (remove #(identical? element %))
              nested)
        nested))))

;; =============================================================================
;; Default host seams
;; =============================================================================

(defn- default-document
  []
  js/document)

(defn- default-htmx
  []
  (aget js/window "htmx"))

(defn- default-request-id
  []
  (str (random-uuid)))

(defn- default-authoritative-from-event
  [_event]
  nil)

;; =============================================================================
;; Core runtime construction / accessors
;; =============================================================================

(defn core?
  [value]
  (and (map? value)
       (= runtime-type
          (:gesso.live.browser.core/type value))))

(defn require-core!
  [value]
  (when-not (core? value)
    (throw
     (core-error
      :invalid-runtime
      "Expected a Gesso Live browser core runtime."
      {:value value})))
  value)

(defn shell-runtime
  [runtime]
  (:shell (require-core! runtime)))

(defn continuity-runtime
  "Return this core runtime's physical continuity runtime."
  [runtime]
  (:continuity (require-core! runtime)))

(defn state
  [runtime]
  (shell/state (shell-runtime runtime)))

(declare event-observers)

(defn diagnostics
  "Plain diagnostics. WeakMap/XHR/DOM/Window resources are deliberately absent."
  [runtime]
  (let [runtime (require-core! runtime)]
    {:gesso.live.browser.core/type runtime-type
     :gesso.live.browser.core/version runtime-version
     :started? @(:started? runtime)
     :event-observers (event-observers runtime)
     :continuity (continuity/diagnostics (:continuity runtime))
     :shell (shell/diagnostics (:shell runtime))}))

;; =============================================================================
;; Shared document-event observation seam
;; =============================================================================

(defn event-observers
  "Return event-name -> observer-id set for registered physical observers."
  [runtime]
  (let [runtime (require-core! runtime)]
    (into {}
          (keep (fn [[event-name observers]]
                  (when (seq observers)
                    [event-name (set (keys observers))])))
          @(:event-observers runtime))))

(defn register-event-observer!
  "Register one observer on an event already owned by Core's Document listener
   set.

   Window-only lifecycle repair is framework-owned and intentionally not exposed
   through this generic raw-event observer seam."
  [runtime event-name observer-id handler]
  (let [runtime (require-core! runtime)
        event-name (require-nonblank-string!
                    "Browser core observed event name"
                    event-name)
        supported (set (map first listener-specs))]
    (when-not (contains? supported event-name)
      (throw
       (core-error
        :unsupported-observed-event
        "Browser core observer event is not part of the supported listener contract."
        {:event-name event-name
         :supported-events supported})))
    (when-not (keyword? observer-id)
      (throw
       (core-error
        :invalid-observer-id
        "Browser core observer id must be a keyword."
        {:observer-id observer-id})))
    (require-callable! "Browser core event observer" handler)
    (swap! (:event-observers runtime)
           update event-name
           (fnil assoc {})
           observer-id
           handler)
    observer-id))

(defn unregister-event-observer!
  "Remove exactly one observer owned by observer-id from event-name."
  [runtime event-name observer-id]
  (let [runtime (require-core! runtime)]
    (swap! (:event-observers runtime)
           (fn [observers]
             (let [event-observers'
                   (dissoc (get observers event-name {}) observer-id)]
               (if (seq event-observers')
                 (assoc observers event-name event-observers')
                 (dissoc observers event-name)))))
    observer-id))

(defn- notify-event-observers!
  [runtime event-name event]
  (doseq [[_ handler] (get @(:event-observers runtime) event-name {})]
    (handler event))
  true)

(defn- option-document
  [options]
  (or (:document options)
      (default-document)))

(defn- option-htmx
  [options]
  (or (:htmx options)
      (default-htmx)))

(defn- document-window
  "Return the Window naturally associated with this runtime's Document.

   Custom host-independent Documents may intentionally have no defaultView; in
   that case Window-only lifecycle listeners are simply unavailable while all
   Document/HTMX behavior remains testable."
  [document]
  (when document
    (.-defaultView document)))

(defn- htmx-trigger!
  [runtime root event-name detail]
  (let [htmx (:htmx runtime)
        trigger (when htmx
                  (aget htmx "trigger"))]
    (when-not (= "function"
                 (js* "typeof ~{}" trigger))
      (throw
       (core-error
        :missing-htmx-trigger
        "Managed Gesso Live fragment refresh requires window.htmx.trigger."
        {:event-name event-name
         :fragment-id (fragment-id-from-root root)})))
    (.call trigger htmx root event-name detail)))

(defn- fragment-roots
  [runtime]
  (let [document (:document runtime)]
    (if document
      (try
        (vec (array-seq (.querySelectorAll document fragment-selector)))
        (catch :default _
          []))
      [])))

(defn- managed-fragment-roots
  [runtime]
  (filterv managed-fragment-root?
           (fragment-roots runtime)))

(defn- fragment-root-by-id
  [runtime fragment-id]
  (some
   (fn [root]
     (when (= fragment-id
              (fragment-id-from-root root))
       root))
   (fragment-roots runtime)))

(defn- weak-get
  [^js weak-map key]
  (when (and weak-map key)
    (.get weak-map key)))

(defn- weak-set!
  [^js weak-map key value]
  (.set weak-map key value)
  value)

(defn- weak-delete!
  [^js weak-map key]
  (when (and weak-map key)
    (.delete weak-map key)))

(defn- delete-if-same!
  "Identity-safe physical cleanup."
  [^js weak-map key expected]
  (when (and key
             (identical? expected
                         (weak-get weak-map key)))
    (weak-delete! weak-map key))
  nil)

(defn- matching-active-request
  [runtime root effect-data]
  (let [record (weak-get (:active-requests runtime) root)]
    (when (and record
               (= (:fragment-id effect-data)
                  (:fragment-id record))
               (= (:request-generation effect-data)
                  (:request-generation record))
               (= (:request-id effect-data)
                  (:request-id record)))
      record)))

(defn- register-active-request!
  [runtime root record]
  (weak-set! (:active-requests runtime) root record)
  (when-let [xhr (:xhr record)]
    (weak-set! (:requests-by-xhr runtime)
               xhr
               {:root root
                :record record}))
  record)

(defn- unregister-active-request!
  [runtime root record]
  (delete-if-same! (:active-requests runtime) root record)
  (when-let [xhr (:xhr record)]
    (when-let [entry (weak-get (:requests-by-xhr runtime) xhr)]
      (when (identical? record (:record entry))
        (weak-delete! (:requests-by-xhr runtime) xhr))))
  nil)

(defn- active-request-from-event
  "Resolve the exact physical managed request represented by an HTMX event."
  [runtime event]
  (let [xhr (xhr-from-event event)]
    (if xhr
      (when-let [{:keys [root record]}
                 (weak-get (:requests-by-xhr runtime) xhr)]
        (when (identical? record
                          (weak-get (:active-requests runtime) root))
          {:root root
           :record record}))
      (when-let [root (fragment-root-from-event event)]
        (when-let [record (weak-get (:active-requests runtime) root)]
          {:root root
           :record record})))))

(defn- abort-xhr!
  [xhr]
  (when xhr
    (let [abort (.-abort xhr)]
      (when (= "function"
               (js* "typeof ~{}" abort))
        (.call abort xhr))))
  true)

(defn- physical-event
  [physical]
  (:event physical))

(defn- physical-root
  [physical]
  (:fragment-root physical))

(defn- built-in-handlers
  [runtime options]
  (let [continuity-handlers (continuity/handlers (:continuity runtime))
        capture! (or (:continuity-capture! options)
                     (:continuity-capture! continuity-handlers))
        restore! (or (:continuity-restore! options)
                     (:continuity-restore! continuity-handlers))
        release! (or (:continuity-release! options)
                     (:continuity-release! continuity-handlers))]
    {:fragment/refresh
     (fn [{:keys [effect]}]
       (let [fragment-id (:fragment-id effect)
             root (or (fragment-root-by-id runtime fragment-id)
                      (throw
                       (core-error
                        :missing-fragment-root
                        "Adapter requested refresh for a fragment root that is not present."
                        {:fragment-id fragment-id
                         :effect effect})))
             pending
             {:fragment-id fragment-id
              :request-generation (:request-generation effect)
              :requirements (set (:requirements effect))}]
         (weak-set! (:pending-refreshes runtime) root pending)
         (try
           (htmx-trigger!
            runtime
            root
            refresh-event-name
            (clj->js
             {:fragmentId fragment-id
              :requestGeneration (:request-generation effect)}))
           :triggered
           (catch :default error
             (delete-if-same!
              (:pending-refreshes runtime)
              root
              pending)
             (throw error)))))

     :htmx/allow-request
     (fn [{:keys [physical]}]
       (when-let [disposition (:disposition physical)]
         (reset! disposition :allowed))
       :allowed)

     :htmx/cancel-request
     (fn [{:keys [effect physical]}]
       (when-let [disposition (:disposition physical)]
         (reset! disposition :cancelled))
       (when-let [event (physical-event physical)]
         (prevent-event! event))
       (let [root (or (physical-root physical)
                      (fragment-root-by-id runtime (:fragment-id effect)))]
         (when-let [record (and root
                                (matching-active-request runtime root effect))]
           (abort-xhr! (:xhr record))))
       :cancelled)

     :htmx/allow-swap
     (fn [{:keys [physical]}]
       (when-let [disposition (:disposition physical)]
         (reset! disposition :allowed))
       :allowed)

     :htmx/cancel-swap
     (fn [{:keys [physical]}]
       (when-let [disposition (:disposition physical)]
         (reset! disposition :cancelled))
       (when-let [event (physical-event physical)]
         (cancel-swap-event! event))
       :cancelled)

     :continuity/capture
     (fn [context]
       (capture! context))

     :continuity/restore
     (fn [context]
       (restore! context))

     :continuity/release
     (fn [context]
       (release! context))}))

(defn create
  "Create one HTMX/browser normalization runtime.

   The returned value owns one shell runtime plus only physical correlation
   WeakMaps and listener registrations. No raw browser object enters the shell's
   AdapterState."
  ([]
   (create nil))
  ([options]
   (let [options (or options {})
         _ (require-map! "Browser core options" options)
         _ (check-option-keys! options)
         request-id-fn (or (:request-id-fn options)
                           default-request-id)
         authoritative-from-event
         (or (:authoritative-from-event options)
             default-authoritative-from-event)
         custom-handlers (or (:handlers options) {})
         _ (require-map! "Browser core handlers" custom-handlers)
         _ (require-callable! "Browser core :request-id-fn" request-id-fn)
         _ (require-callable! "Browser core :authoritative-from-event"
                              authoritative-from-event)
         continuity-options (or (:continuity-options options) {})
         _ (require-map! "Browser core :continuity-options" continuity-options)
         continuity-runtime (continuity/create continuity-options)
         _ (require-optional-callable! "Browser core :continuity-capture!"
                                       (:continuity-capture! options))
         _ (require-optional-callable! "Browser core :continuity-restore!"
                                       (:continuity-restore! options))
         _ (require-optional-callable! "Browser core :continuity-release!"
                                       (:continuity-release! options))
         collisions (seq (filter protected-handler-kinds
                                 (keys custom-handlers)))
         _ (when collisions
             (throw
              (core-error
               :protected-handler-override
               "Application handlers may not replace framework-owned HTMX or continuity effect handlers."
               {:effect-kinds (set collisions)})))
         shell-options (or (:shell-options options) {})
         _ (require-map! "Browser core :shell-options" shell-options)
         _ (when (contains? shell-options :handlers)
             (throw
              (core-error
               :nested-shell-handlers
               "Supply shell effect handlers through browser core :handlers."
               {})))
         runtime-base
         {:gesso.live.browser.core/type runtime-type
          :document (option-document options)
          :htmx (option-htmx options)
          :request-id-fn request-id-fn
          :authoritative-from-event authoritative-from-event
          :continuity continuity-runtime
          :pending-refreshes (js/WeakMap.)
          :active-requests (js/WeakMap.)
          :requests-by-xhr (js/WeakMap.)
          :listeners (atom [])
          :window-listeners (atom [])
          :event-observers (atom {})
          :started? (atom false)}
         built-ins (built-in-handlers runtime-base options)
         handlers (merge built-ins custom-handlers)
         shell-runtime
         (shell/create
          (assoc shell-options :handlers handlers))]
     (assoc runtime-base
            :shell shell-runtime))))

;; =============================================================================
;; Physical request correlation
;; =============================================================================

(defn pending-refresh
  "Return DOM-light pending correlation data for tests/diagnostics, or nil."
  [runtime root]
  (when-let [pending
             (weak-get (:pending-refreshes (require-core! runtime)) root)]
    (select-keys pending
                 [:fragment-id :request-generation :requirements])))

(defn active-request
  "Return DOM-light active request correlation data, excluding XHR."
  [runtime root]
  (when-let [record
             (weak-get (:active-requests (require-core! runtime)) root)]
    (select-keys record
                 [:fragment-id
                  :request-generation
                  :request-id
                  :requirements])))

(defn- physical-context
  [runtime event root record disposition]
  {:event event
   :fragment-root root
   :request
   (when record
     (select-keys record
                  [:fragment-id
                   :request-generation
                   :request-id
                   :requirements]))
   :disposition disposition})

(defn- normalized-request-event
  [kind record]
  {:event kind
   :fragment-id (:fragment-id record)
   :request-generation (:request-generation record)
   :request-id (:request-id record)})

(defn- event-authoritative-candidate
  [runtime event]
  (let [value ((:authoritative-from-event runtime) event)]
    (when (some? value)
      (when-not (map? value)
        (throw
         (core-error
          :invalid-authoritative-parser-result
          "Browser authoritative parser must return nil or a plain map."
          {:value value})))
      value)))

;; =============================================================================
;; Explicit invalidation / lifecycle recovery boundary
;; =============================================================================

(defn- read-managed-sse-payload
  "Decode one managed Gesso Live SSE payload from HTMX's MessageEvent detail."
  [event]
  (let [data (detail-field event "data")]
    (when (some? data)
      (when-not (string? data)
        (throw
         (core-error
          :invalid-sse-payload
          "Managed Gesso Live SSE payload data must be an EDN string."
          {:data data})))
      (let [payload
            (try
              (reader/read-string data)
              (catch :default error
                (throw
                 (core-error
                  :invalid-sse-payload
                  "Managed Gesso Live SSE payload is not readable EDN."
                  {:data data}
                  error))))]
        (when-not (map? payload)
          (throw
           (core-error
            :invalid-sse-payload
            "Managed Gesso Live SSE payload must decode to a map."
            {:data data
             :payload payload})))
        payload))))

(defn- decode-progression-wire!
  [wire location]
  (try
    (progression/wire->requirement wire)
    (catch :default error
      (throw
       (core-error
        :invalid-sse-progression
        "Managed Gesso Live SSE payload contains invalid progression wire data."
        {:location location
         :wire wire}
        error)))))

(defn- payload-progression
  "Return {:present? boolean :requirement normalized-or-nil} for one decoded
   managed LiveEvent payload."
  [payload]
  (if (nil? payload)
    {:present? false
     :requirement nil}
    (let [top-present? (contains? payload :progression)
          invalidation (:invalidation payload)
          nested-present? (and (map? invalidation)
                               (contains? invalidation :progression))]
      (cond
        (and (not top-present?)
             (not nested-present?))
        {:present? false
         :requirement nil}

        (not= top-present? nested-present?)
        (throw
         (core-error
          :inconsistent-sse-progression
          "Managed Gesso Live SSE progression must be present at both transport locations or neither."
          {:top-level-present? top-present?
           :nested-present? nested-present?
           :payload payload}))

        :else
        (let [top (decode-progression-wire! (:progression payload) :event)
              nested (decode-progression-wire!
                      (:progression invalidation)
                      :invalidation)]
          (when-not (= top nested)
            (throw
             (core-error
              :inconsistent-sse-progression
              "Managed Gesso Live SSE progression copies disagree."
              {:event-progression top
               :invalidation-progression nested})))
          {:present? true
           :requirement top})))))

(defn- require-refresh-requirement!
  [requirement]
  (try
    (progression/require-requirement! requirement)
    (catch :default error
      (throw
       (core-error
        :invalid-progression-requirement
        "Managed fragment refresh requirement must be a canonical Gesso Live progression requirement."
        {:requirement requirement}
        error)))))

(defn notify-fragment!
  "Notify the pure adapter that a logical Live fragment must refresh.

   The two-argument runtime/fragment form is advisory and carries no
   authoritative minimum-read requirement."
  ([runtime fragment-id]
   (notify-fragment! runtime fragment-id nil false))
  ([runtime fragment-id requirement]
   (notify-fragment! runtime fragment-id requirement true))
  ([runtime fragment-id requirement requirement-present?]
   (let [runtime (require-core! runtime)
         fragment-id (require-nonblank-string! "Fragment id" fragment-id)
         requirement
         (when requirement-present?
           (require-refresh-requirement! requirement))
         event
         (cond-> {:event :live/invalidated
                  :fragment-id fragment-id}
           requirement-present?
           (assoc :requirement requirement))]
     (shell/dispatch! (:shell runtime) event))))

(defn- repair-managed-fragments!
  "Advisory convergence repair for every currently managed stable root.

   Lifecycle events say only that the browser may have missed invalidations.
   They never carry or synthesize progression requirements."
  [runtime]
  (let [runtime (require-core! runtime)]
    (doseq [root (managed-fragment-roots runtime)
            :let [fragment-id (fragment-id-from-root root)]
            :when fragment-id]
      (notify-fragment! runtime fragment-id)))
  true)

(defn on-online!
  "Treat Window online as an advisory convergence-repair boundary."
  [runtime _event]
  (repair-managed-fragments! runtime))

(defn on-pageshow!
  "Repair after bfcache restoration only.

   Ordinary pageshow participates in normal page startup and must not create a
   duplicate refresh merely because the page became visible."
  [runtime event]
  (when (true? (when event
                 (.-persisted event)))
    (repair-managed-fragments! runtime))
  true)

(defn on-visibility-change!
  "Repair when the runtime Document becomes visible again.

   Transitioning into hidden/background state is not itself permission to issue
   work."
  [runtime _event]
  (let [runtime (require-core! runtime)
        document (:document runtime)]
    (when (= "visible"
             (some-> document .-visibilityState str))
      (repair-managed-fragments! runtime)))
  true)

(defn on-sse-before-message!
  "Normalize one managed HTMX SSE message into a fragment invalidation."
  [runtime event]
  (let [listener (detail-field event "elt")]
    (when (invalidation-listener? listener)
      (prevent-event! event)
      (let [root (fragment-root-from-element listener)
            listener-id (some-> (element-attr listener
                                              invalidation-listener-attribute)
                                str
                                not-empty)
            fragment-id (fragment-id-from-root root)]
        (when-not (and (managed-fragment-root? root)
                       fragment-id
                       (= fragment-id listener-id))
          (throw
           (core-error
            :invalid-sse-invalidation-listener
            "Managed SSE invalidation listener must belong to the matching managed fragment root."
            {:listener-fragment-id listener-id
             :root-fragment-id fragment-id
             :managed-root? (managed-fragment-root? root)})))
        (let [{:keys [present? requirement]}
              (payload-progression (read-managed-sse-payload event))]
          (if present?
            (notify-fragment! runtime fragment-id requirement)
            (notify-fragment! runtime fragment-id))))))
  true)

(defn on-sse-open!
  "Treat opening or reopening a managed fragment's EventSource as an
   invalidation."
  [runtime event]
  (when-let [root (some-> (detail-field event "elt")
                          fragment-root-from-element)]
    (when (managed-fragment-root? root)
      (when-let [fragment-id (fragment-id-from-root root)]
        (notify-fragment! runtime fragment-id))))
  true)

(defn on-invalidated!
  "Normalize one explicit DOM invalidation event."
  [runtime event]
  (let [fragment-id (detail-field event "fragmentId")
        detail (event-detail event)
        requirement-present?
        (boolean
         (and detail
              (.call (.-hasOwnProperty (.-prototype js/Object))
                     detail
                     "requirement")))
        requirement
        (when requirement-present?
          (js->clj
           (aget detail "requirement")
           :keywordize-keys true))]
    (when fragment-id
      (notify-fragment!
       runtime
       (str fragment-id)
       requirement
       requirement-present?)))
  true)

;; =============================================================================
;; HTMX lifecycle normalization
;; =============================================================================

(defn- compose-refresh-requirements
  [requirements]
  (apply progression/compose requirements))

(defn- request-headers-from-event
  [event]
  (some-> (event-detail event)
          (aget "headers")))

(defn on-config-request!
  "Attach the adapter-approved authoritative progression requirement to one
   managed HTMX fragment refresh."
  [runtime event]
  (let [runtime (require-core! runtime)
        root (fragment-root-from-event event)
        pending (and root
                     (weak-get (:pending-refreshes runtime) root))]
    (when pending
      (try
        (when-some [requirement
                    (compose-refresh-requirements (:requirements pending))]
          (let [headers (request-headers-from-event event)]
            (when-not headers
              (throw
               (core-error
                :missing-config-request-headers
                "Managed HTMX configRequest did not expose mutable request headers."
                {:fragment-id (:fragment-id pending)
                 :request-generation (:request-generation pending)})))
            (aset headers
                  progression.http/request-header-name
                  (progression.http/encode-request-progression requirement))))
        (catch :default error
          (prevent-event! event)
          (throw error)))))
  true)

(defn on-before-request!
  "Bind the adapter-issued request generation to one physical HTMX request."
  [runtime event]
  (let [runtime (require-core! runtime)
        root (fragment-root-from-event event)
        pending (and root
                     (weak-get (:pending-refreshes runtime) root))]
    (when pending
      (let [disposition (atom :undecided)]
        (try
          (let [request-id ((:request-id-fn runtime))
                _ (require-nonblank-string!
                   "Physical HTMX request id"
                   request-id)
                record (assoc pending
                              :request-id request-id
                              :xhr (xhr-from-event event))
                physical
                (physical-context runtime event root record disposition)
                result
                (shell/dispatch!
                 (:shell runtime)
                 (normalized-request-event :htmx/before-request record)
                 physical)]
            (delete-if-same! (:pending-refreshes runtime) root pending)
            (if (= :cancelled @disposition)
              (unregister-active-request! runtime root record)
              (register-active-request! runtime root record))
            result)
          (catch :default error
            (prevent-event! event)
            (throw error))))))
  true)

(defn on-before-swap!
  "Normalize HTMX beforeSwap for the exact currently correlated managed request."
  [runtime event]
  (let [runtime (require-core! runtime)
        {:keys [root record]} (active-request-from-event runtime event)]
    (when record
      (let [disposition (atom :undecided)
            physical
            (physical-context runtime event root record disposition)]
        (try
          (let [candidate (event-authoritative-candidate runtime event)
                normalized
                (cond-> (normalized-request-event :htmx/before-swap record)
                  candidate
                  (assoc :authoritative candidate))]
            (shell/dispatch!
             (:shell runtime)
             normalized
             physical))
          (catch :default error
            (cancel-swap-event! event)
            (throw error))))))
  true)

(defn on-after-swap!
  "Normalize HTMX afterSwap."
  [runtime event]
  (let [runtime (require-core! runtime)
        {:keys [root record]} (active-request-from-event runtime event)]
    (when record
      (shell/dispatch!
       (:shell runtime)
       (normalized-request-event :htmx/after-swap record)
       (physical-context runtime event root record (atom :observed)))))
  true)

(defn- finish-active-request!
  [runtime event failure?]
  (let [{:keys [root record]}
        (active-request-from-event runtime event)]
    (when record
      (let [normalized
            (if failure?
              (assoc
               (normalized-request-event :http/failed record)
               :reason (failure-reason event))
              (normalized-request-event :htmx/after-request record))
            result
            (shell/dispatch!
             (:shell runtime)
             normalized
             (physical-context runtime event root record (atom :observed)))]
        (unregister-active-request! runtime root record)
        result))))

(defn on-after-request!
  "Finish one managed HTMX request."
  [runtime event]
  (finish-active-request!
   (require-core! runtime)
   event
   (not (request-successful? event)))
  true)

(defn on-request-failed!
  "Normalize HTMX transport/error events."
  [runtime event]
  (finish-active-request!
   (require-core! runtime)
   event
   true)
  true)

(defn on-before-cleanup!
  "Retire managed fragments whose stable roots are being removed."
  [runtime event]
  (let [runtime (require-core! runtime)
        element (or (detail-field event "elt")
                    (when event (.-target event)))]
    (doseq [root (nested-fragment-roots element)
            :let [fragment-id (fragment-id-from-root root)
                  record (weak-get (:active-requests runtime) root)]
            :when fragment-id]
      (shell/dispatch!
       (:shell runtime)
       {:event :fragment/retire
        :fragment-id fragment-id
        :reason :dom-cleanup}
       {:event event
        :fragment-root root})
      (weak-delete! (:pending-refreshes runtime) root)
      (when record
        (unregister-active-request! runtime root record))))
  true)

;; =============================================================================
;; Listener installation / removal
;; =============================================================================

(defn add-document-listener!
  ([document name handler]
   (add-document-listener! document name handler false))
  ([document name handler capture?]
   (.addEventListener document name handler capture?)
   [name handler capture?]))

(defn remove-document-listener!
  [document name handler capture?]
  (.removeEventListener document name handler capture?)
  true)

(defn- add-window-listener!
  [window name handler capture?]
  (.addEventListener window name handler capture?)
  [window name handler capture?])

(defn- remove-window-listener!
  [window name handler capture?]
  (.removeEventListener window name handler capture?)
  true)

(defn- built-in-handler-for
  [runtime handler-id]
  (case handler-id
    :observe-only nil
    :config-request #(on-config-request! runtime %)
    :before-request #(on-before-request! runtime %)
    :before-swap #(on-before-swap! runtime %)
    :after-swap #(on-after-swap! runtime %)
    :after-request #(on-after-request! runtime %)
    :request-failed #(on-request-failed! runtime %)
    :before-cleanup #(on-before-cleanup! runtime %)
    :sse-before-message #(on-sse-before-message! runtime %)
    :sse-open #(on-sse-open! runtime %)
    :invalidated #(on-invalidated! runtime %)
    :visibility-change #(on-visibility-change! runtime %)
    :online #(on-online! runtime %)
    :pageshow #(on-pageshow! runtime %)))

(defn- handler-for
  [runtime event-name handler-id]
  (let [built-in (built-in-handler-for runtime handler-id)]
    (fn [event]
      (notify-event-observers! runtime event-name event)
      (when built-in
        (built-in event))
      true)))

(defn- window-lifecycle-handler-for
  [runtime handler-id]
  (or (built-in-handler-for runtime handler-id)
      (throw
       (core-error
        :missing-window-lifecycle-handler
        "Window lifecycle listener has no framework handler."
        {:handler-id handler-id}))))

(defn- remove-installed-listeners!
  [runtime]
  (let [document (:document runtime)]
    (doseq [[name handler capture?] @(:listeners runtime)]
      (try
        (remove-document-listener! document name handler capture?)
        (catch :default _
          nil)))
    (doseq [[window name handler capture?] @(:window-listeners runtime)]
      (try
        (remove-window-listener! window name handler capture?)
        (catch :default _
          nil)))
    (reset! (:listeners runtime) [])
    (reset! (:window-listeners runtime) [])
    true))

(defn start!
  "Install Core's Document/HTMX and browser lifecycle listeners exactly once.

   `visibilitychange` is installed on Document. `online` and `pageshow` are
   installed on Document.defaultView when that Window seam exists. Host-
   independent fake Documents may omit defaultView; this preserves portable
   Node tests without pretending those Window events were installed elsewhere."
  [runtime]
  (let [runtime (require-core! runtime)]
    (when (compare-and-set! (:started? runtime) false true)
      (let [document (:document runtime)]
        (when-not document
          (reset! (:started? runtime) false)
          (throw
           (core-error
            :missing-document
            "Browser core requires a document to install lifecycle listeners."
            {})))
        (try
          (doseq [[name handler-id] listener-specs]
            (let [handler (handler-for runtime name handler-id)
                  registration
                  (add-document-listener!
                   document
                   name
                   handler
                   false)]
              (swap! (:listeners runtime) conj registration)))

          (when-let [window (document-window document)]
            (doseq [[name handler-id] window-lifecycle-listener-specs]
              (let [handler
                    (window-lifecycle-handler-for runtime handler-id)
                    registration
                    (add-window-listener!
                     window
                     name
                     handler
                     false)]
                (swap! (:window-listeners runtime)
                       conj
                       registration))))

          (catch :default error
            ;; Partial physical acquisition must never survive a failed start.
            (remove-installed-listeners! runtime)
            (reset! (:started? runtime) false)
            (throw error)))))
    runtime))

(defn stop!
  "Remove installed listeners and shut down the shell runtime.

   Shell shutdown semantically retires owned work before best-effort physical
   cleanup. Listener removal itself carries no semantic authority."
  [runtime]
  (let [runtime (require-core! runtime)]
    (when @(:started? runtime)
      (remove-installed-listeners! runtime)
      (reset! (:started? runtime) false))
    (shell/shutdown! (:shell runtime))
    :stopped))

;; =============================================================================
;; Small browser-global entry point
;; =============================================================================

(defonce ^:private default-runtime*
  (atom nil))

(defn default-runtime
  []
  @default-runtime*)

(defn- install-public-api!
  [runtime]
  (let [api
        #js {:version runtime-version
             :state (fn []
                      (clj->js (diagnostics runtime)))
             :notifyFragment
             (fn
               ([fragment-id]
                (notify-fragment! runtime fragment-id))
               ([fragment-id requirement]
                (notify-fragment!
                 runtime
                 fragment-id
                 (js->clj requirement))))
             :shutdown (fn []
                         (stop! runtime))}]
    (aset js/window "gessoLive" api)
    true))

(defn ^:export init!
  "Create/start the default browser runtime once."
  ([]
   (init! nil))
  ([options]
   (or @default-runtime*
       (let [runtime (create options)]
         (if (compare-and-set! default-runtime* nil runtime)
           (do
             (try
               (start! runtime)
               (install-public-api! runtime)
               runtime
               (catch :default error
                 (reset! default-runtime* nil)
                 (try
                   (stop! runtime)
                   (catch :default _
                     nil))
                 (throw error))))
           @default-runtime*)))))

(defn ^:export shutdown!
  "Stop and forget the default runtime. Intended for hot reload/tests and full
   page runtime teardown."
  []
  (when-let [runtime @default-runtime*]
    (reset! default-runtime* nil)
    (stop! runtime))
  :stopped)
