(ns gesso.live.browser.core
  "Browser entry point for Gesso Live.

   The top-level runtime is intentionally an adapter. Core behavior lives in:

     gesso.live.browser.dom
       trusted DOM mechanics

     gesso.live.browser.continuity
       the single continuity implementation used by all replacement paths

     gesso.live.browser.choreo
       generic long-lived choreography execution

     gesso.live.browser.optimistic
       browser effects for the verified optimistic choreography

   This namespace owns only browser-framework integration:

   - HTMX/SSE event listener registration
   - request execution-id/header correlation
   - starting an optimistic choreography at HTMX's request boundary
   - settlement delivery and environmental request failure delivery
   - ordinary continuity capture/restore around HTMX/OOB/SSE replacement
   - observation of externally installed canonical state
   - small public diagnostics API

   It contains no optimistic state machine and no independent continuity logic."
  (:require
   [clojure.string :as str]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.browser.choreo :as choreo-runtime]
   [gesso.live.browser.continuity :as continuity]
   [gesso.live.browser.dom :as dom]
   [gesso.live.browser.optimistic :as optimistic]))

;; -----------------------------------------------------------------------------
;; Runtime identity
;; -----------------------------------------------------------------------------

(def runtime-version
  "2.0.0")

(defonce initialized?
  (atom false))

;; Detached-or-live optimistic source uid -> preflight request information.
;; Entries are allocated at configRequest so the execution id can be attached
;; before HTMX sends, then consumed when the choreography starts at beforeRequest.
(defonce pending-requests
  (atom {}))

;; Source uids whose current HTMX response has already been semantically
;; consumed by optimistic choreography. This prevents the same response
;; lifecycle from recapturing over the execution's continuity slot.
(defonce settled-response-sources
  (atom #{}))

;; -----------------------------------------------------------------------------
;; Generic event helpers
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

(defn event-source
  "Return HTMX request source when present."
  [event]
  (continuity/event-source event))

(defn event-target
  [event]
  (or
   (some->
    (detail-field event "target")
    (#(when (dom/element? %) %)))
   (some->
    (detail-field event "elt")
    (#(when (dom/element? %) %)))
   (when (dom/element?
          (.-target event))
     (.-target event))))

(defn optimistic-source-from-event
  [event]
  (or
   (some->
    (event-source event)
    optimistic/optimistic-source)
   (some
    optimistic/optimistic-source
    (continuity/event-elements event))))

(defn prevent-event!
  [event]
  (when (.-preventDefault event)
    (.preventDefault event))
  false)

(defn- source-uid
  [source]
  (when source
    (or
     (aget source
           "__gessoLiveRequestUid")
     (let [uid
           (str
            "gesso-request-source-"
            (random-uuid))]
       (aset source
             "__gessoLiveRequestUid"
             uid)
       uid))))

(defn- pending-request
  [source]
  (get @pending-requests
       (source-uid source)))

(defn- put-pending-request!
  [source value]
  (swap!
   pending-requests
   assoc
   (source-uid source)
   value)
  value)

(defn- remove-pending-request!
  [source]
  (when source
    (swap!
     pending-requests
     dissoc
     (source-uid source)))
  true)

(defn- mark-settled-response!
  [source]
  (when source
    (swap!
     settled-response-sources
     conj
     (source-uid source)))
  true)

(defn- settled-response?
  [source]
  (boolean
   (and source
        (contains?
         @settled-response-sources
         (source-uid source)))))

(defn- clear-settled-response-later!
  [source]
  (when source
    (let [uid
          (source-uid source)]
      (js/setTimeout
       #(swap!
         settled-response-sources
         disj
         uid)
       0)))
  true)

;; -----------------------------------------------------------------------------
;; HTMX headers
;; -----------------------------------------------------------------------------

(def optimistic-request-header
  "Optimistic execution-correlation request header owned by the shared protocol."
  protocol/execution-header-name)

(def consistency-request-header
  "Browser HTTP spelling for Gesso's consistency token header."
  "X-Gesso-Live-Consistency-Token")

(defn- ensure-headers!
  [event]
  (let [detail
        (event-detail event)]
    (when detail
      (or
       (aget detail "headers")
       (let [headers #js {}]
         (aset detail
               "headers"
               headers)
         headers)))))

(defn- header-value
  "Read a JS header object case-insensitively."
  [headers wanted-name]
  (when headers
    (let [wanted
          (str/lower-case
           wanted-name)]
      (some
       (fn [key]
         (when (= wanted
                  (str/lower-case
                   key))
           (aget headers key)))
       (array-seq
        (.keys js/Object
               headers))))))

(defn- set-header!
  [headers name value]
  (when (and headers
             (some? value))
    (aset headers
          name
          (str value)))
  headers)

;; -----------------------------------------------------------------------------
;; Request status / failure
;; -----------------------------------------------------------------------------

(defn request-successful?
  [event]
  (let [detail
        (event-detail event)
        explicit
        (when detail
          (aget detail
                "successful"))
        xhr
        (when detail
          (aget detail
                "xhr"))
        status
        (when xhr
          (.-status xhr))]
    (if (boolean? explicit)
      explicit
      (and (number? status)
           (<= 200 status 399)))))

(defn failure-reason
  [event]
  (keyword
   "gesso.live.optimistic.request"
   (case (.-type event)
     "htmx:responseError"
     "response-error"

     "htmx:sendError"
     "send-error"

     "htmx:timeout"
     "timeout"

     "htmx:abort"
     "aborted"

     "request-failed")))

(defn xhr-from-event
  [event]
  (detail-field event "xhr"))

;; -----------------------------------------------------------------------------
;; Optimistic request preflight
;; -----------------------------------------------------------------------------

(defn- source-consistency-token
  [event]
  (let [headers
        (ensure-headers! event)]
    (header-value
     headers
     consistency-request-header)))

(defn on-config-request!
  "Allocate optimistic execution identity and attach it to the request.

   No DOM projection happens here. HTMX may still be finishing request
   preparation and must not lose its live action source prematurely."
  [event]
  (when-some [source
              (optimistic-source-from-event
               event)]
    (let [headers
          (ensure-headers! event)
          existing
          (pending-request source)
          execution-id
          (or (:execution-id existing)
              (optimistic/execution-id))
          consistency-token
          (or (:consistency-token existing)
              (source-consistency-token
               event))]
      (put-pending-request!
       source
       {:execution-id execution-id
        :consistency-token
        consistency-token})
      (set-header!
       headers
       optimistic-request-header
       execution-id)))
  true)

(defn- optimistic-target-from-event
  [event]
  (let [candidate
        (detail-field event "target")]
    (when (dom/element? candidate)
      candidate)))

(defn on-before-request!
  "Start the verified browser choreography immediately before network send.

   configRequest has already assigned the correlation header, so starting here
   may safely replace the action's target contents without disrupting HTMX
   request construction."
  [event]
  (when-some [source
              (optimistic-source-from-event
               event)]
    (let [preflight
          (pending-request source)]
      (when-not preflight
        (prevent-event! event)
        (let [error
              (ex-info
               "Optimistic HTMX request reached beforeRequest without configRequest correlation."
               {:error/type
                :gesso.live.browser.core/missing-optimistic-preflight})]
          (optimistic/emit!
           (continuity/root source)
           "error"
           #js {:phase "before-request"
                :reason "missing-config-request"
                :error error})
          (throw error)))
      (let [execution-id
            (:execution-id preflight)]
        (try
          (optimistic/start!
         source
         {:target
          (optimistic-target-from-event
           event)
          :execution-id execution-id
          :consistency-token
          (:consistency-token preflight)})
        (remove-pending-request!
         source)
        (catch :default error
          (remove-pending-request!
           source)
          ;; Never allow an uncorrelated authoritative command to leave the
          ;; browser after local optimistic protocol startup failed.
          (prevent-event! event)
          (optimistic/emit!
           (continuity/root source)
           "error"
           #js {:phase "before-request"
                :executionId execution-id
                :error error})
          (throw error))))))
  true)

;; -----------------------------------------------------------------------------
;; Settlement delivery
;; -----------------------------------------------------------------------------

(defn- active-execution-for-source
  [source]
  (when-some [execution-id
              (optimistic/execution-for-source
               source)]
    (when (optimistic/active?
           execution-id)
      execution-id)))

(defn- settle-from-event!
  "Attempt semantic settlement delivery from an HTMX response.

   Returns one of:
     :settled
     :no-settlement
     :inactive
     :not-optimistic

   This is safe to call at beforeSwap and again at afterRequest. Once the first
   delivery completes the choreography, the later call observes an inactive
   execution and cannot resurrect it."
  [event]
  (if-some [source
            (optimistic-source-from-event
             event)]
    (if-some [execution-id
              (active-execution-for-source
               source)]
      (if-some [xhr
                (xhr-from-event event)]
        (if (optimistic/settle-from-xhr!
             execution-id
             xhr)
          (do
            (mark-settled-response!
             source)
            (optimistic/cleanup-source-if-terminal!
             source)
            :settled)
          :no-settlement)
        :no-settlement)
      :inactive)
    :not-optimistic))

(defn on-after-request!
  [event]
  (when-some [source
              (optimistic-source-from-event
               event)]
    (when-some [execution-id
                (active-execution-for-source
                 source)]
      (let [settlement-status
            (settle-from-event!
             event)]
        (when (and
               (= :no-settlement
                  settlement-status)
               (not
                (request-successful?
                 event)))
          (optimistic/request-failed!
           execution-id
           (failure-reason event)))
        ;; A successful correlated optimistic response without a settlement is a
        ;; server/framework protocol error, not semantic command failure. Keep
        ;; the execution alive so its modeled timeout recovers safely, and emit
        ;; a diagnostic instead of pretending the network failed.
        (when (and
               (= :no-settlement
                  settlement-status)
               (request-successful?
                event))
          (optimistic/emit!
           (continuity/root source)
           "error"
           #js {:phase "after-request"
                :reason "missing-settlement"
                :executionId execution-id}))))
    (optimistic/cleanup-source-if-terminal!
     source)
    (remove-pending-request!
     source)
    (clear-settled-response-later!
     source))
  true)

(defn on-request-failed!
  [event]
  (when-some [source
              (optimistic-source-from-event
               event)]
    (when-some [execution-id
                (active-execution-for-source
                 source)]
      (optimistic/request-failed!
       execution-id
       (failure-reason event)))
    (optimistic/cleanup-source-if-terminal!
     source)
    (remove-pending-request!
     source)
    (clear-settled-response-later!
     source))
  true)

;; -----------------------------------------------------------------------------
;; Continuity + canonical observation
;; -----------------------------------------------------------------------------

(defn- event-roots
  [event]
  (continuity/roots-from-event
   event))

(defn- observe-canonical-event!
  [event]
  (doseq [element
          (continuity/event-elements
           event)]
    (optimistic/observe-canonical-tree!
     element))
  true)

(defn on-before-swap!
  [event]
  ;; Settlement is consumed before HTMX gets a chance to install the same
  ;; response. This preserves semantic outcome and lets the choreography make
  ;; the canonical authority decision itself. HTMX may subsequently process
  ;; equivalent OOB markup; ordinary continuity will handle that later pass.
  (let [source
        (optimistic-source-from-event
         event)
        settlement-status
        (settle-from-event!
         event)]
    (when-not (or
               (= :settled
                  settlement-status)
               (settled-response?
                source))
      (continuity/capture-from-event!
       event)))
  true)

(defn on-after-swap!
  [event]
  (observe-canonical-event!
   event)
  (continuity/restore-immediate-from-event!
   event)
  true)

(defn on-after-settle!
  [event]
  (observe-canonical-event!
   event)
  (continuity/restore-after-layout-from-event!
   event)
  true)

(defn on-oob-before-swap!
  [event]
  (let [source
        (optimistic-source-from-event
         event)
        settlement-status
        (settle-from-event!
         event)]
    (when-not (or
               (= :settled
                  settlement-status)
               (settled-response?
                source))
      (continuity/capture-from-event!
       event)))
  true)

(defn on-oob-after-swap!
  [event]
  (observe-canonical-event!
   event)
  (continuity/restore-from-event!
   event)
  true)

(defn on-sse-before-message!
  [event]
  (continuity/capture-from-event!
   event)
  true)

(defn on-sse-message!
  [event]
  ;; Live invalidation/refetch delivery is authoritative only where the server
  ;; explicitly marked resulting DOM canonical.
  (observe-canonical-event!
   event)
  (continuity/restore-from-event!
   event)
  true)

;; -----------------------------------------------------------------------------
;; Duplicate activation
;; -----------------------------------------------------------------------------

(defn- busy-source?
  [source]
  (try
    (let [descriptor
          (optimistic/source-descriptor
           source)]
      (optimistic/scope-busy?
       (:scope descriptor)))
    (catch :default _
      false)))

(defn on-click-capture!
  [event]
  (when-some [source
              (optimistic/optimistic-source
               (.-target event))]
    (when (busy-source? source)
      (prevent-event! event)))
  true)

(defn on-submit-capture!
  [event]
  (when-some [source
              (optimistic/optimistic-source
               (.-target event))]
    (when (busy-source? source)
      (prevent-event! event)))
  true)

;; -----------------------------------------------------------------------------
;; Cleanup
;; -----------------------------------------------------------------------------

(defn on-before-cleanup!
  [event]
  (let [target
        (event-target event)]
    (when target
      ;; Normal active optimistic executions are not forcibly aborted merely
      ;; because their provisional target is being replaced: canonical
      ;; observation/settlement is the authority mechanism. Whole subtree
      ;; teardown, however, must release browser-only continuity resources.
      (continuity/cleanup-element!
       target))
    (when-some [source
                (optimistic-source-from-event
                 event)]
      (optimistic/cleanup-source-if-terminal!
       source)
      (remove-pending-request!
       source)))
  true)

;; -----------------------------------------------------------------------------
;; Public browser API
;; -----------------------------------------------------------------------------

(defn runtime-state
  []
  {:version runtime-version
   :protocol-version
   protocol/version
   :pending-request-count
   (count @pending-requests)
   :settled-response-count
   (count @settled-response-sources)
   :choreo
   (choreo-runtime/diagnostics)
   :continuity
   {:slots
    (continuity/slot-summaries)}
   :optimistic
   (optimistic/diagnostics)})

(defn- js-register-box!
  [type implementation]
  (continuity/register-box!
   type
   implementation))

(defn- js-runtime-state
  []
  (clj->js
   (runtime-state)))

(defn install-public-api!
  []
  (let [gesso-live
        (or
         (aget js/window
               "gessoLive")
         #js {})
        continuity-api
        #js {:version runtime-version
             :parseConfig
             continuity/parse-config
             :boxesFromConfig
             continuity/boxes-from-config
             :captureFromEvent
             continuity/capture-from-event!
             :restoreFromEvent
             continuity/restore-from-event!
             :registerBox
             js-register-box!}
        choreo-api
        #js {:version runtime-version
             :state
             (fn []
               (clj->js
                (choreo-runtime/diagnostics)))
             :active
             (fn []
               (clj->js
                (choreo-runtime/active-summaries)))
             :terminal
             (fn []
               (clj->js
                (choreo-runtime/terminal-summaries)))}
        optimistic-api
        #js {:version runtime-version
             :state
             (fn []
               (clj->js
                (optimistic/diagnostics)))
             :active
             optimistic/active?
             :scopeBusy
             optimistic/scope-busy?
             :commandPayload
             (fn [execution-id]
               (clj->js
                (optimistic/command-payload
                 execution-id)))}]
    (aset gesso-live
          "version"
          runtime-version)
    (aset gesso-live
          "protocolVersion"
          protocol/version)
    (aset gesso-live
          "state"
          js-runtime-state)
    (aset gesso-live
          "continuity"
          continuity-api)
    (aset gesso-live
          "choreo"
          choreo-api)
    (aset gesso-live
          "optimistic"
          optimistic-api)
    (aset js/window
          "gessoLive"
          gesso-live))
  true)

;; -----------------------------------------------------------------------------
;; Listener installation
;; -----------------------------------------------------------------------------

(defn add-document-listener!
  ([name handler]
   (.addEventListener
    js/document
    name
    handler))
  ([name handler capture?]
   (.addEventListener
    js/document
    name
    handler
    capture?)))

(defn ^:export init!
  []
  (when (compare-and-set!
         initialized?
         false
         true)
    (continuity/initialize!)
    (optimistic/initialize!)

    ;; Duplicate suppression is a capture-phase concern. Unlike the old runtime,
    ;; no provisional DOM mutation is performed on pointerdown.
    (add-document-listener!
     "click"
     on-click-capture!
     true)
    (add-document-listener!
     "submit"
     on-submit-capture!
     true)

    ;; Request correlation and lifecycle.
    (add-document-listener!
     "htmx:configRequest"
     on-config-request!)
    (add-document-listener!
     "htmx:beforeRequest"
     on-before-request!)
    (add-document-listener!
     "htmx:afterRequest"
     on-after-request!)
    (add-document-listener!
     "htmx:responseError"
     on-request-failed!)
    (add-document-listener!
     "htmx:sendError"
     on-request-failed!)
    (add-document-listener!
     "htmx:timeout"
     on-request-failed!)
    (add-document-listener!
     "htmx:abort"
     on-request-failed!)

    ;; Ordinary HTMX replacement continuity and canonical observation.
    (add-document-listener!
     "htmx:beforeSwap"
     on-before-swap!)
    (add-document-listener!
     "htmx:afterSwap"
     on-after-swap!)
    (add-document-listener!
     "htmx:afterSettle"
     on-after-settle!)
    (add-document-listener!
     "htmx:oobBeforeSwap"
     on-oob-before-swap!)
    (add-document-listener!
     "htmx:oobAfterSwap"
     on-oob-after-swap!)

    ;; htmx-ext-sse replacement lifecycle.
    (add-document-listener!
     "htmx:sseBeforeMessage"
     on-sse-before-message!)
    (add-document-listener!
     "htmx:sseMessage"
     on-sse-message!)

    (add-document-listener!
     "htmx:beforeCleanupElement"
     on-before-cleanup!)

    (install-public-api!))
  true)

(init!)