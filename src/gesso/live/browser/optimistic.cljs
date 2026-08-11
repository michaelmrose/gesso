(ns gesso.live.browser.optimistic
  "Browser implementation of the built-in Gesso Live optimistic choreography.

   This namespace is the browser policy adapter for the built-in optimistic
   choreography. The semantic lifecycle lives in gesso.live.optimistic.choreo
   and is projected at compile time. gesso.choreo.machine owns protocol position;
   gesso.live.browser.choreo owns the long-lived browser execution process; this
   namespace binds the optimistic protocol to trusted browser FX.

   Owned here:

   - optimistic source/target/template resolution
   - target single-flight ownership
   - structural snapshot capture and authorized recovery
   - projection installation
   - settlement parsing and correlated delivery
   - canonical authority/revision checks
   - pending UI markers
   - timeout scheduling/cancellation
   - canonical-supersession observation

   Not owned here:

   - continuity mechanics (gesso.live.browser.continuity)
   - generic DOM mechanics (gesso.live.browser.dom)
   - generic choreography execution (gesso.live.browser.choreo)
   - HTMX/SSE listener registration (gesso.live.browser.core)
   - server/domain transition policy

   A projection is never canonical. A snapshot is never allowed to overwrite
   explicitly canonical state. Distinct opaque revisions are never ordered."
  (:require
   [clojure.string :as str]
   [gesso.choreo.machine :as machine]
   [gesso.live.optimistic.choreo :as optimistic
    :include-macros true]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.browser.choreo :as runtime]
   [gesso.live.browser.continuity :as continuity]
   [gesso.live.browser.fx :as fx]
   [gesso.live.browser.dom :as dom]))

;; -----------------------------------------------------------------------------
;; Compiled protocol product
;; -----------------------------------------------------------------------------

(def browser-plan
  "Verified browser projection emitted at CLJS compile time.

   The verifier and global projector stay on the JVM side of compilation."
  (optimistic/browser-plan-form))

;; -----------------------------------------------------------------------------
;; Browser-only runtime attributes
;; -----------------------------------------------------------------------------

(def active-attr
  "data-gesso-optimistic-active")

(def pending-attr
  "data-gesso-optimistic-pending")

(def locked-attr
  "data-gesso-optimistic-locked")

(def pending-source-attr
  "data-gesso-optimistic-source-pending")

(def default-settlement-timeout-ms
  15000)

(def settlement-timer-key
  :optimistic/settlement-timeout)

(def optimistic-event-prefix
  "gesso:optimistic:")

;; -----------------------------------------------------------------------------
;; Private execution-context keys
;; -----------------------------------------------------------------------------

(def ^:private source-key
  ::source)

(def ^:private target-key
  ::target)

(def ^:private target-id-key
  ::target-id)

(def ^:private descriptor-key
  ::descriptor)

(def ^:private root-key
  ::root)

(def ^:private snapshot-key
  ::snapshot)

(def ^:private continuity-slot-key
  ::continuity-slot)

(def ^:private projection-key
  ::projection)

(def ^:private pending-source-state-key
  ::pending-source-state)

(def ^:private canonical-source-key
  ::canonical-source)

;; -----------------------------------------------------------------------------
;; Runtime ownership
;; -----------------------------------------------------------------------------

;; Opaque optimistic scope -> {:execution-id ... :target ... :target-id ...}.
;; Scope is the logical lock key rather than DOM-node identity, so ownership
;; survives canonical replacement of the target while the command is pending.
(defonce target-locks
  (atom {}))

;; DOM source uid -> active execution id. Used only to correlate HTMX lifecycle
;; events that retain the source element but not our request header.
(defonce executions-by-source
  (atom {}))

;; Execution id -> projected optimistic command send action. The HTMX adapter
;; consumes this exact choreography-produced action/payload.
(defonce outgoing-actions
  (atom {}))

;; -----------------------------------------------------------------------------
;; Small browser helpers
;; -----------------------------------------------------------------------------

(defn now-ms
  []
  (.getTime (js/Date.)))

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

(defn emit!
  [target name detail]
  (let [target
        (or target
            (.-documentElement js/document))]
    (when (and target
               (.-dispatchEvent target))
      (try
        (.dispatchEvent
         target
         (custom-event
          (str optimistic-event-prefix name)
          detail))
        (catch :default _
          nil))))
  detail)

(defn htmx-process!
  "Ask HTMX to process newly installed markup when HTMX is present."
  [element]
  (when (and element
             (.-htmx js/window)
             (.-process
              (.-htmx js/window)))
    (.process
     (.-htmx js/window)
     element))
  element)

(defn- node-uid
  [element]
  (when element
    (or
     (aget element
           "__gessoLiveOptimisticUid")
     (let [uid
           (str
            "gesso-node-"
            (random-uuid))]
       (aset element
             "__gessoLiveOptimisticUid"
             uid)
       uid))))

(defn execution-id
  []
  (str
   "gesso-optimistic-"
   (random-uuid)))

(defn- non-blank?
  [x]
  (and (string? x)
       (not (str/blank? x))))

(defn- attr-name
  [protocol-attr]
  (dom/attr-name
   protocol-attr))

;; -----------------------------------------------------------------------------
;; Descriptor / source resolution
;; -----------------------------------------------------------------------------

(def optimistic-source-selector
  (str "["
       (attr-name
        protocol/protocol-attr)
       "]["
       (attr-name
        protocol/transition-attr)
       "]["
       (attr-name
        protocol/template-attr)
       "]["
       (attr-name
        protocol/target-attr)
       "]["
       (attr-name
        protocol/scope-attr)
       "]"))

(defn optimistic-source
  "Return nearest element that owns one optimistic command."
  [element]
  (cond
    (dom/matches?
     element
     optimistic-source-selector)
    element

    (dom/element? element)
    (dom/closest
     element
     optimistic-source-selector)

    :else
    nil))

(defn source-descriptor
  "Decode one server-rendered optimistic source descriptor.

   Scope remains its opaque wire identity. Revisions are decoded into the shared
   typed representation so numeric and opaque string revisions remain distinct."
  [source]
  (when source
    (let [protocol-version
          (dom/attr
           source
           protocol/protocol-attr)
          transition
          (dom/attr
           source
           protocol/transition-attr)
          template-name
          (dom/attr
           source
           protocol/template-attr)
          target
          (dom/attr
           source
           protocol/target-attr)
          scope
          (dom/attr
           source
           protocol/scope-attr)
          base-wire
          (dom/attr
           source
           protocol/base-revision-attr)
          pending-label
          (dom/attr
           source
           protocol/pending-label-attr)
          projection-mode-wire
          (dom/attr
           source
           protocol/projection-mode-attr)
          projection-mode
          (when projection-mode-wire
            (protocol/wire->projection-mode
             projection-mode-wire))]
      (when-not (= protocol/version
                   protocol-version)
        (throw
         (ex-info
          "Unsupported Gesso Live optimistic protocol version."
          {:error/type
           :gesso.live.optimistic/unsupported-protocol
           :expected
           protocol/version
           :actual protocol-version})))
      (doseq [[field value]
              [[:transition transition]
               [:template template-name]
               [:target target]
               [:scope scope]]]
        (when-not (non-blank? value)
          (throw
           (ex-info
            "Gesso Live optimistic source descriptor is incomplete."
            {:error/type
             :gesso.live.optimistic/incomplete-descriptor
             :field field
             :value value
             :source source}))))
      {:protocol-version protocol-version
       :transition transition
       :template-name template-name
       :target target
       :scope scope
       :base-revision
       (when base-wire
         (protocol/wire->revision
          base-wire))
       :pending-label pending-label
       :projection-mode
       (or projection-mode
           :provisional)})))

(defn- following-sibling
  [source selector direction]
  (loop [candidate
         (direction source)]
    (cond
      (nil? candidate)
      nil

      (dom/matches?
       candidate
       selector)
      candidate

      :else
      (recur
       (direction candidate)))))

(defn resolve-extended-selector
  "Resolve the small HTMX-style selector vocabulary Gesso uses for optimistic
   targets."
  [source selector]
  (let [selector
        (str/trim
         (or selector ""))]
    (cond
      (= selector "this")
      source

      (str/starts-with?
       selector
       "closest ")
      (dom/closest
       source
       (subs selector 8))

      (str/starts-with?
       selector
       "find ")
      (dom/query-one
       source
       (subs selector 5))

      (str/starts-with?
       selector
       "next ")
      (following-sibling
       source
       (subs selector 5)
       #(.-nextElementSibling %))

      (str/starts-with?
       selector
       "previous ")
      (following-sibling
       source
       (subs selector 9)
       #(.-previousElementSibling %))

      :else
      (dom/query-one
       js/document
       selector))))

(defn resolve-target
  [source descriptor]
  (resolve-extended-selector
   source
   (:target descriptor)))

(defn- template-in
  [root template-name]
  (when (and root
             template-name)
    (some
     (fn [template]
       (when (= template-name
                (dom/attr
                 template
                 protocol/template-attr))
         template))
     (dom/templates root))))

(defn resolve-template
  "Resolve exactly one projection template in the natural local-to-global search
   order."
  [source target descriptor]
  (let [template-name
        (:template-name descriptor)]
    (or
     (template-in
      (.-parentElement source)
      template-name)
     (template-in
      (continuity/root source)
      template-name)
     (template-in
      (continuity/root target)
      template-name)
     (template-in
      js/document
      template-name))))

(defn prepare
  "Resolve and validate browser objects needed to start an execution."
  [source explicit-target]
  (let [descriptor
        (source-descriptor source)
        target
        (or explicit-target
            (resolve-target
             source
             descriptor))
        template
        (resolve-template
         source
         target
         descriptor)
        projection
        (when template
          (dom/template-root
           template))
        root
        (or
         (continuity/root source)
         (continuity/root target))]
    (when-not target
      (throw
       (ex-info
        "Gesso Live optimistic target could not be resolved."
        {:error/type
         :gesso.live.optimistic/no-target
         :descriptor descriptor})))
    (when-not template
      (throw
       (ex-info
        "Gesso Live optimistic projection template could not be resolved."
        {:error/type
         :gesso.live.optimistic/no-template
         :descriptor descriptor})))
    (dom/assert-compatible-root!
     target
     projection)
    (dom/assert-stable-identity!
     target
     projection)
    {:source source
     :descriptor descriptor
     :target target
     :target-id
     (not-empty
      (.-id target))
     :template template
     :projection projection
     :root root}))

;; -----------------------------------------------------------------------------
;; Logical target ownership
;; -----------------------------------------------------------------------------

(defn current-lock
  [scope]
  (get @target-locks scope))

(defn execution-lock
  [execution-id]
  (some
   (fn [[scope lock]]
     (when (= execution-id
              (:execution-id lock))
       (assoc lock
              :scope scope)))
   @target-locks))

(defn reserve-target!
  [scope execution-id target target-id]
  (let [existing
        (current-lock scope)]
    (cond
      (nil? existing)
      (do
        (swap!
         target-locks
         assoc
         scope
         {:execution-id execution-id
          :target target
          :target-id target-id})
        true)

      (= execution-id
         (:execution-id existing))
      true

      :else
      false)))

(defn release-target!
  [scope execution-id]
  (swap!
   target-locks
   (fn [locks]
     (if (= execution-id
            (get-in locks
                    [scope :execution-id]))
       (dissoc locks
               scope)
       locks)))
  true)

(defn- current-target
  "Resolve the current DOM target for one active execution.

   Stable id is preferred when the original target was replaced. The original
   node is used only while connected."
  [ctx]
  (let [target
        (get ctx target-key)
        target-id
        (get ctx target-id-key)
        source
        (get ctx source-key)
        descriptor
        (get ctx descriptor-key)]
    (or
     (when (dom/connected? target)
       target)
     (when target-id
       (dom/by-id target-id))
     (when (and source
                (dom/connected? source)
                descriptor)
       (resolve-target
        source
        descriptor)))))

;; -----------------------------------------------------------------------------
;; Pending source UI
;; -----------------------------------------------------------------------------

(defn- label-element
  [source]
  (or
   (dom/query-one
    source
    "[data-gesso-button-label]")
   (dom/query-one
    source
    "[data-gesso-optimistic-label-target]")
   (when (and
          (= "INPUT"
             (dom/tag-name source))
          (#{"submit" "button"}
           (some->
            (.-type source)
            str/lower-case)))
     source)))

(defn- mark-source-pending!
  [source descriptor execution-id]
  (let [label
        (label-element source)
        state
        {:source source
         :disabled
         (when (some?
                (.-disabled source))
           (boolean
            (.-disabled source)))
         :aria-disabled
         (dom/attr
          source
          "aria-disabled")
         :aria-busy
         (dom/attr
          source
          "aria-busy")
         :label-element label
         :label
         (when label
           (if (= "INPUT"
                  (dom/tag-name label))
             (.-value label)
             (.-textContent label)))}]
    (dom/set-attr!
     source
     protocol/execution-attr
     execution-id)
    (dom/set-attr!
     source
     pending-source-attr
     "true")
    (dom/set-attr!
     source
     "aria-busy"
     "true")
    (dom/set-attr!
     source
     "aria-disabled"
     "true")
    ;; At configRequest/request-start time the click has already been accepted by
    ;; HTMX, so native disabling can now safely prevent duplicate activation.
    (when (some?
           (.-disabled source))
      (set!
       (.-disabled source)
       true))
    (when-some [pending-label
                (:pending-label descriptor)]
      (when label
        (if (= "INPUT"
               (dom/tag-name label))
          (set! (.-value label)
                pending-label)
          (set! (.-textContent label)
                pending-label))))
    state))

(defn- clear-source-pending!
  [state]
  (when-some [source
              (:source state)]
    (dom/remove-attr!
     source
     protocol/execution-attr)
    (dom/remove-attr!
     source
     pending-source-attr)
    (if (nil?
         (:aria-busy state))
      (dom/remove-attr!
       source
       "aria-busy")
      (dom/set-attr!
       source
       "aria-busy"
       (:aria-busy state)))
    (if (nil?
         (:aria-disabled state))
      (dom/remove-attr!
       source
       "aria-disabled")
      (dom/set-attr!
       source
       "aria-disabled"
       (:aria-disabled state)))
    (when (some?
           (:disabled state))
      (set!
       (.-disabled source)
       (:disabled state)))
    (when-some [label
                (:label-element state)]
      (when (dom/connected? label)
        (if (= "INPUT"
               (dom/tag-name label))
          (set! (.-value label)
                (:label state))
          (set! (.-textContent label)
                (:label state))))))
  true)

;; -----------------------------------------------------------------------------
;; Browser FX operations
;; -----------------------------------------------------------------------------

(defn acquire-target!
  [ctx]
  (let [execution-id (get ctx optimistic/execution-id-key)
        scope (get ctx optimistic/scope-key)
        target (get ctx target-key)
        target-id (get ctx target-id-key)]
    (when-not (reserve-target! scope execution-id target target-id)
      (throw
       (ex-info
        "Gesso Live optimistic target is already owned by another execution."
        {:error/type :gesso.live.optimistic/target-busy
         :execution-id execution-id
         :scope scope
         :owner (:execution-id (current-lock scope))})))
    {:optimistic/target-acquired? true}))

(defn capture-continuity!
  [ctx]
  (let [root (get ctx root-key)
        source (get ctx source-key)
        slot (when root
               (continuity/capture! root source))]
    {continuity-slot-key slot}))

(defn capture-snapshot!
  [ctx]
  (let [target (current-target ctx)]
    (when-not target
      (throw
       (ex-info
        "Gesso Live optimistic snapshot target disappeared before projection."
        {:error/type :gesso.live.optimistic/no-snapshot-target
         :execution-id (get ctx optimistic/execution-id-key)})))
    {snapshot-key (dom/snapshot target)}))

(defn install-projection!
  [ctx]
  (let [target (current-target ctx)
        projection (get ctx projection-key)
        descriptor (get ctx descriptor-key)
        source (get ctx source-key)
        execution-id (get ctx optimistic/execution-id-key)]
    (when-not target
      (throw
       (ex-info
        "Gesso Live optimistic target disappeared before projection installation."
        {:error/type :gesso.live.optimistic/no-projection-target
         :execution-id execution-id})))
    ;; Keep the existing target object alive: HTMX may already retain it for the
    ;; request currently being configured.
    (dom/copy-element-into! target projection)
    ;; Projection authority is always explicitly non-canonical.
    (dom/remove-attr! target protocol/canonical-attr)
    (dom/set-attr! target protocol/scope-attr (:scope descriptor))
    (dom/set-attr! target active-attr execution-id)
    (dom/set-attr! target pending-attr "true")
    (dom/set-attr! target locked-attr "true")
    (dom/set-attr! target "aria-busy" "true")
    (htmx-process! target)
    {pending-source-state-key
     (mark-source-pending! source descriptor execution-id)}))

(defn schedule-timeout!
  [ctx]
  (let [execution-id (get ctx optimistic/execution-id-key)]
    (runtime/schedule-event!
     execution-id
     settlement-timer-key
     default-settlement-timeout-ms
     optimistic/timeout-event
     {optimistic/reason-key :settlement-timeout})
    {:optimistic/timeout-scheduled? true}))

(defn- canonical-scope!
  [canonical expected-scope]
  (when-not (dom/canonical? canonical)
    (throw
     (ex-info
      "Optimistic settlement canonical payload is not explicitly canonical."
      {:error/type :gesso.live.optimistic/noncanonical-settlement
       :canonical canonical})))
  (when-not (= expected-scope (dom/scope canonical))
    (throw
     (ex-info
      "Optimistic settlement canonical scope does not match execution scope."
      {:error/type :gesso.live.optimistic/canonical-scope-mismatch
       :expected expected-scope
       :actual (dom/scope canonical)})))
  canonical)

(defn- installed-canonical-disposition
  "Decide whether detached settlement canonical may replace current target.

   Existing explicit canonical state wins on equal, older, or incomparable
   incoming revisions. This prevents a correlated POST response from overwriting
   authoritative canonical state that reached the browser first."
  [target incoming]
  (if-not (dom/canonical? target)
    :install
    (case (protocol/compare-revisions
           (dom/revision incoming)
           (dom/revision target))
      :newer :install
      :same :canonical-wins
      :older :canonical-wins
      :incomparable :canonical-wins)))

(defn install-canonical!
  [ctx]
  (let [execution-id (get ctx optimistic/execution-id-key)
        scope (get ctx optimistic/scope-key)
        canonical (canonical-scope!
                   (get ctx optimistic/canonical-key)
                   scope)
        target (current-target ctx)]
    (when-not target
      (throw
       (ex-info
        "Gesso Live cannot reconcile optimistic execution because its target no longer exists."
        {:error/type :gesso.live.optimistic/no-canonical-target
         :execution-id execution-id
         :scope scope})))
    (case (installed-canonical-disposition target canonical)
      :install
      (do
        (dom/copy-canonical-into! target (.cloneNode canonical true))
        (htmx-process! target)
        {optimistic/canonical-disposition-key :installed
         canonical-source-key :settlement})

      :canonical-wins
      {optimistic/canonical-disposition-key :canonical-wins
       canonical-source-key :already-installed})))

(defn discard-snapshot!
  [_ctx]
  ;; Explicit nil revokes recovery authority in the accumulated choreography
  ;; context even though the former snapshot may remain in diagnostics/trace.
  {snapshot-key nil})

(defn recover-snapshot!
  [ctx]
  (let [execution-id (get ctx optimistic/execution-id-key)
        snapshot (get ctx snapshot-key)
        target (current-target ctx)]
    (cond
      ;; A missing target is not evidence that canonical won. It is a trusted
      ;; runtime/DOM failure and must remain visible as such.
      (nil? target)
      (throw
       (ex-info
        "Gesso Live optimistic recovery target no longer exists."
        {:error/type :gesso.live.optimistic/no-recovery-target
         :execution-id execution-id}))

      ;; Explicit canonical state always outranks the old structural snapshot.
      (dom/canonical? target)
      {optimistic/recovery-disposition-key :canonical-wins}

      (nil? snapshot)
      (throw
       (ex-info
        "Optimistic recovery has no authorized structural snapshot."
        {:error/type :gesso.live.optimistic/no-recovery-snapshot
         :execution-id execution-id}))

      ;; Only this execution's still-provisional target may be rolled back.
      (not= execution-id (dom/attr target active-attr))
      (throw
       (ex-info
        "Optimistic recovery target is neither canonical nor owned by this execution."
        {:error/type :gesso.live.optimistic/recovery-authority-lost
         :execution-id execution-id
         :active (dom/attr target active-attr)}))

      :else
      (do
        (dom/copy-element-into! target (dom/snapshot-node snapshot))
        (htmx-process! target)
        {optimistic/recovery-disposition-key :recovered}))))

(defn restore-continuity!
  [ctx]
  (let [execution-id (get ctx optimistic/execution-id-key)
        scope (get ctx optimistic/scope-key)
        root (get ctx root-key)
        complete!
        (fn [detail]
          ;; This callback runs after the continuity two-frame layout boundary.
          ;; Late delivery after retirement is harmless in browser.choreo.
          (runtime/resume-event!
           execution-id
           optimistic/continuity-restored-event
           {:scope scope
            :continuity detail}))]
    (if root
      (continuity/restore! root complete!)
      ;; There is no configured continuity root, but the choreography still owns
      ;; an explicit restoration barrier. Preserve the same post-layout timing.
      (continuity/after-layout!
       #(complete! {:root nil
                    :target nil
                    :slot nil})))
    {:optimistic/continuity-restore-scheduled? true}))

(defn cancel-timeout!
  [ctx]
  (runtime/cancel-timer!
   (get ctx optimistic/execution-id-key)
   settlement-timer-key)
  {:optimistic/timeout-cancelled? true})

(defn clear-pending!
  [ctx]
  (let [execution-id (get ctx optimistic/execution-id-key)
        target (current-target ctx)
        source-state (get ctx pending-source-state-key)]
    (when (and target
               (= execution-id (dom/attr target active-attr)))
      (dom/remove-attr! target active-attr)
      (dom/remove-attr! target pending-attr)
      (dom/remove-attr! target locked-attr)
      (dom/remove-attr! target "aria-busy"))
    (clear-source-pending! source-state)
    (swap! outgoing-actions dissoc execution-id)
    (when-some [source (get ctx source-key)]
      (swap! executions-by-source dissoc (node-uid source)))
    {:optimistic/pending-cleared? true}))

(defn release-target-operation!
  [ctx]
  (release-target!
   (get ctx optimistic/scope-key)
   (get ctx optimistic/execution-id-key))
  {:optimistic/target-released? true})

;; One choreography :fx state names a whole participant-local FX machine. These
;; machines are intentionally tiny wrappers around the trusted operation
;; handlers above; browser.fx provides the Biff-compatible local execution
;; semantics while choreography remains responsible for async boundaries.
(def ^:private fx-result-key
  ::fx-result)

(defn- operation-machine
  [machine-id]
  (fx/machine
   machine-id
   :start
   (fn [_ctx]
     {fx-result-key [machine-id]
      fx/next-key :return})
   :return
   (fn [ctx]
     {fx/return-key (get ctx fx-result-key)})))

(def browser-machine-handlers
  {optimistic/browser-acquire-target-machine acquire-target!
   optimistic/browser-capture-continuity-machine capture-continuity!
   optimistic/browser-capture-snapshot-machine capture-snapshot!
   optimistic/browser-install-projection-machine install-projection!
   optimistic/browser-schedule-timeout-machine schedule-timeout!
   optimistic/browser-install-canonical-machine install-canonical!
   optimistic/browser-discard-snapshot-machine discard-snapshot!
   optimistic/browser-recover-snapshot-machine recover-snapshot!
   optimistic/browser-restore-continuity-machine restore-continuity!
   optimistic/browser-cancel-timeout-machine cancel-timeout!
   optimistic/browser-clear-pending-machine clear-pending!
   optimistic/browser-release-target-machine release-target-operation!})

(defn install-fx!
  "Register the optimistic choreography's browser-local FX machines and their
   trusted effect handlers."
  []
  (doseq [[machine-id handler] browser-machine-handlers]
    (runtime/register-fx-handler! machine-id handler)
    (runtime/register-fx-machine! machine-id (operation-machine machine-id)))
  true)

(defn- record-command-send!
  [action execution]
  (when-not (and (= :send (:kind action))
                 (= optimistic/browser-role (:from action))
                 (= optimistic/server-role (:to action))
                 (= protocol/command-event (:event action)))
    (throw
     (ex-info
      "Optimistic browser choreography produced an unexpected send boundary."
      {:error/type :gesso.live.optimistic/unexpected-send
       :action action
       :execution-id (:execution-id execution)})))
  (swap! outgoing-actions assoc (:execution-id action) action)
  true)

(defn install-transport-handoff!
  "Install the optimistic command handoff used by the surrounding HTMX adapter."
  []
  (runtime/set-send-handler! record-command-send!)
  true)

;; -----------------------------------------------------------------------------
;; Starting an optimistic execution
;; -----------------------------------------------------------------------------

(defn- initial-context
  [prepared execution-id consistency-token]
  (let [{:keys
         [source
          target
          target-id
          descriptor
          projection
          root]}
        prepared]
    (cond->
        {optimistic/execution-id-key
         execution-id

         optimistic/transition-key
         (:transition descriptor)

         optimistic/scope-key
         (:scope descriptor)

         source-key source
         target-key target
         target-id-key target-id
         descriptor-key descriptor
         projection-key projection
         root-key root}

      (some?
       (:base-revision descriptor))
      (assoc
       optimistic/base-revision-key
       (:base-revision descriptor))

      (some? consistency-token)
      (assoc
       optimistic/consistency-token-key
       consistency-token))))

(defn start!
  "Start one browser optimistic execution.

   Required:
     source - actual HTMX request owner

   Options:
     :execution-id       caller-provided id; otherwise generated
     :consistency-token  optional DB visibility token already associated with
                         the request

   Returns the execution id, suspended endpoint execution, and the exact
   choreography-produced command send action recorded by the transport handoff."
  ([source]
   (start! source nil))
  ([source {:keys [execution-id consistency-token]}]
   (let [execution-id (or execution-id
                          (gesso.live.browser.optimistic/execution-id))
         prepared (prepare source nil)
         source-uid (node-uid source)
         execution
         (runtime/start!
          browser-plan
          {:execution-id execution-id
           :context
           (initial-context prepared execution-id consistency-token)
           :metadata
           {:kind :optimistic
            :source source-uid}})
         command (get @outgoing-actions execution-id)]
     (when-not command
       (throw
        (ex-info
         "Optimistic browser choreography reached suspension without handing off its command send."
         {:error/type :gesso.live.optimistic/missing-command-handoff
          :execution-id execution-id
          :state (:state execution)})))
     (when (machine/suspended? execution)
       (swap! executions-by-source assoc source-uid execution-id))
     (let [ctx (machine/execution-context execution)]
       (emit!
        (:root prepared)
        "started"
        #js {:executionId execution-id
             :transition (get ctx optimistic/transition-key)
             :scope (get ctx optimistic/scope-key)})
       {:execution-id execution-id
        :execution execution
        :command command}))))

;; -----------------------------------------------------------------------------
;; Settlement response parsing
;; -----------------------------------------------------------------------------

(defn settlement-marker?
  [element]
  (and
   (dom/template? element)
   (dom/truthy-attr?
    element
    protocol/settlement-attr)))

(defn marker->settlement
  "Strictly decode one inert settlement marker.

   Missing/malformed command-applied metadata is an error, and the flag must
   agree with the semantic outcome. Canonical content is attached separately
   after explicit authority selection."
  [marker]
  (when (settlement-marker? marker)
    (let [version (dom/attr marker protocol/protocol-attr)
          execution-id (dom/attr marker protocol/execution-attr)
          scope (dom/attr marker protocol/scope-attr)
          outcome
          (protocol/wire->settlement-outcome
           (dom/attr marker protocol/outcome-attr))
          command-applied?
          (protocol/wire->command-applied
           (dom/attr marker protocol/command-applied-attr))
          revision-wire (dom/attr marker protocol/revision-attr)]
      (when-not (= protocol/version version)
        (throw
         (ex-info
          "Optimistic settlement marker uses an unsupported protocol version."
          {:error/type :gesso.live.optimistic/unsupported-settlement-protocol
           :expected protocol/version
           :actual version
           :execution-id execution-id})))
      (doseq [[field value] [[:execution-id execution-id]
                             [:scope scope]]]
        (when-not (non-blank? value)
          (throw
           (ex-info
            "Optimistic settlement marker is missing required correlation metadata."
            {:error/type :gesso.live.optimistic/incomplete-settlement
             :field field
             :value value}))))
      (protocol/assert-settlement-consistent! outcome command-applied?)
      {optimistic/execution-id-key execution-id
       optimistic/scope-key scope
       optimistic/outcome-key outcome
       optimistic/command-applied-key command-applied?
       optimistic/revision-key
       (when revision-wire
         (protocol/wire->revision revision-wire))
       optimistic/reason-key
       (dom/attr marker protocol/reason-attr)})))

(defn- settlement-markers
  [root]
  (let [selector
        (str "template["
             (attr-name
              protocol/settlement-attr)
             "]")]
    (cond-> (dom/query-all
             root
             selector)
      (and (dom/template? root)
           (settlement-marker? root))
      (conj root))))

(defn- marker-for-execution
  [root expected-execution-id]
  (let [matches
        (->> (settlement-markers root)
             (filter
              #(=
                expected-execution-id
                (dom/attr
                 %
                 protocol/execution-attr)))
             vec)]
    (case (count matches)
      0
      nil

      1
      (first matches)

      (throw
       (ex-info
        "HTMX response contains multiple settlement markers for one optimistic execution."
        {:error/type
         :gesso.live.optimistic/duplicate-settlement
         :execution-id expected-execution-id
         :count (count matches)})))))

(defn- canonical-from-response
  [root scope]
  (let [selection
        (dom/newest-canonical
         (dom/canonical-elements
          root
          scope))]
    (case (:status selection)
      :selected
      (:element selection)

      :none
      nil

      :ambiguous
      (throw
       (ex-info
        "Optimistic settlement response contains ambiguous canonical state."
        {:error/type
         :gesso.live.optimistic/ambiguous-canonical
         :scope scope
         :reason
         (:reason selection)})))))

(defn settlement-from-root
  "Extract one complete authoritative settlement from detached response markup.

   The marker and canonical root must agree exactly on scope and revision wire
   identity. A matching marker without authoritative canonical content is a
   protocol error, never an implicit success/failure fallback."
  [root expected-execution-id]
  (when-some [marker (marker-for-execution root expected-execution-id)]
    (let [settlement (marker->settlement marker)
          scope (get settlement optimistic/scope-key)
          canonical (canonical-from-response root scope)]
      (when-not canonical
        (throw
         (ex-info
          "Optimistic settlement response does not contain explicitly canonical content for its scope."
          {:error/type :gesso.live.optimistic/missing-canonical
           :execution-id expected-execution-id
           :scope scope})))
      (when-not (= protocol/version
                   (dom/attr canonical protocol/protocol-attr))
        (throw
         (ex-info
          "Optimistic canonical response root uses an unsupported protocol version."
          {:error/type :gesso.live.optimistic/unsupported-canonical-protocol
           :execution-id expected-execution-id
           :expected protocol/version
           :actual (dom/attr canonical protocol/protocol-attr)})))
      (when-not (= scope (dom/scope canonical))
        (throw
         (ex-info
          "Optimistic settlement marker scope disagrees with canonical root scope."
          {:error/type :gesso.live.optimistic/settlement-scope-mismatch
           :execution-id expected-execution-id
           :marker-scope scope
           :canonical-scope (dom/scope canonical)})))
      (let [marker-revision-wire
            (dom/attr marker protocol/revision-attr)
            canonical-revision-wire
            (dom/revision-wire canonical)]
        (when-not (= marker-revision-wire canonical-revision-wire)
          (throw
           (ex-info
            "Optimistic settlement marker revision disagrees with canonical root revision."
            {:error/type :gesso.live.optimistic/settlement-revision-mismatch
             :execution-id expected-execution-id
             :scope scope
             :marker-revision marker-revision-wire
             :canonical-revision canonical-revision-wire}))))
      (assoc settlement optimistic/canonical-key canonical))))

(defn response-root
  "Parse an HTMX XHR response into a detached DocumentFragment."
  [xhr]
  (let [text
        (when xhr
          (.-responseText xhr))]
    (when (and
           (string? text)
           (not
            (str/blank? text)))
      (let [template
            (.createElement
             js/document
             "template")]
        (set!
         (.-innerHTML template)
         text)
        (.-content template)))))

;; -----------------------------------------------------------------------------
;; Correlated protocol delivery
;; -----------------------------------------------------------------------------

(def settlement-message-descriptor
  {:from optimistic/server-role
   :to optimistic/browser-role
   :event optimistic/settlement-event
   :via :http})

(defn settle!
  "Deliver one already-parsed authoritative settlement to its execution."
  [settlement]
  (let [execution-id
        (get settlement
             optimistic/execution-id-key)]
    (runtime/resume-message!
     execution-id
     settlement-message-descriptor
     settlement)))

(defn settle-from-xhr!
  "Parse and deliver settlement for expected execution from one XHR response.

   Returns nil when the response has no marker for this execution. A malformed
   matching settlement throws rather than being silently interpreted as success."
  [expected-execution-id xhr]
  (when-some [root
              (response-root xhr)]
    (when-some [settlement
                (settlement-from-root
                 root
                 expected-execution-id)]
      (settle!
       settlement))))

(defn request-failed!
  "Deliver environmental request failure.

   This is not semantic :failed settlement. With no authoritative response, the
   choreography follows its guarded snapshot-recovery path."
  ([execution-id]
   (request-failed!
    execution-id
    nil))
  ([execution-id reason]
   (runtime/resume-event!
    execution-id
    optimistic/request-failed-event
    {optimistic/reason-key
     reason})))

;; -----------------------------------------------------------------------------
;; Canonical supersession
;; -----------------------------------------------------------------------------

(defn- execution-superseded-by?
  [execution canonical]
  (let [ctx
        (machine/execution-context
         execution)
        scope
        (get ctx
             optimistic/scope-key)
        base-revision
        (get ctx
             optimistic/base-revision-key)]
    (and
     (dom/canonical? canonical)
     (= scope
        (dom/scope canonical))
     ;; Without a base revision there is no proof that an unrelated canonical
     ;; render outranks the in-flight command.
     (some? base-revision)
     (= :newer
        (protocol/compare-revisions
         (dom/revision canonical)
         base-revision)))))

(defn canonical-installed!
  "Observe newly installed canonical DOM and notify any pending optimistic
   execution it provably supersedes.

   Only explicit canonical state with a strictly newer comparable revision than
   the execution's base revision can emit canonical-superseded. Equality,
   older revisions, distinct opaque revisions, and missing revisions do not
   guess authority order."
  [canonical]
  (when (dom/canonical? canonical)
    (doseq [execution-id
            (runtime/active-execution-ids)
            :let [execution
                  (runtime/execution
                   execution-id)]
            :when
            (and execution
                 (runtime/accepts-event?
                  execution-id
                  optimistic/canonical-superseded-event)
                 (execution-superseded-by?
                  execution
                  canonical))]
      (runtime/resume-event!
       execution-id
       optimistic/canonical-superseded-event
       {optimistic/revision-key
        (dom/revision canonical)
        optimistic/scope-key
        (dom/scope canonical)})))
  canonical)

(defn observe-canonical-tree!
  "Observe every explicit canonical element at or beneath root."
  [root]
  (doseq [canonical
          (dom/canonical-elements root)]
    (canonical-installed!
     canonical))
  root)

;; -----------------------------------------------------------------------------
;; Request/source correlation helpers
;; -----------------------------------------------------------------------------

(defn execution-for-source
  [source]
  (get @executions-by-source
       (node-uid source)))

(defn forget-source!
  [source]
  (when-some [uid
              (node-uid source)]
    (swap!
     executions-by-source
     dissoc
     uid))
  true)

(defn request-header
  "HTTP header name/value pair for the optimistic execution correlation id.

   The name is emitted in ordinary HTTP spelling; Ring receives the lower-case
   name declared in gesso.live.protocol."
  [execution-id]
  ["Gesso-Optimistic-Execution"
   execution-id])

(defn command-action
  "Return the projected optimistic command send action for execution-id."
  [execution-id]
  (get @outgoing-actions execution-id))

(defn command-payload
  "Return the exact command payload produced by choreography projection."
  [execution-id]
  (some-> (command-action execution-id)
          :payload))

;; -----------------------------------------------------------------------------
;; Completion/cleanup observations
;; -----------------------------------------------------------------------------

(defn active?
  [execution-id]
  (runtime/active?
   execution-id))

(defn scope-busy?
  [scope]
  (boolean
   (current-lock scope)))

(defn cleanup-source-if-terminal!
  "Drop source correlation after generic runtime retirement.

   Target/pending cleanup itself is choreography-owned and has already happened
   before a normal terminal return."
  [source]
  (let [execution-id
        (execution-for-source source)]
    (when (and execution-id
               (not
                (runtime/active?
                 execution-id)))
      (forget-source!
       source))
    true))

(defn abort!
  "Whole-browser teardown escape hatch.

   Normal network/timeout/canonical races must use modeled events. This function
   is only for cases where the browser object graph itself is being destroyed."
  [execution-id reason]
  (when-some [lock
              (execution-lock
               execution-id)]
    (release-target!
     (:scope lock)
     execution-id))
  (runtime/cancel-all-timers!
   execution-id)
  (swap! outgoing-actions dissoc execution-id)
  (swap!
   executions-by-source
   (fn [by-source]
     (into {}
           (remove (fn [[_source-id active-id]]
                     (= execution-id active-id)))
           by-source)))
  (runtime/abort!
   execution-id
   reason))

;; -----------------------------------------------------------------------------
;; Initialization / diagnostics
;; -----------------------------------------------------------------------------

(defn initialize!
  "Register optimistic browser FX and the command transport handoff.

   Global HTMX/SSE listener registration belongs to gesso.live.browser.core.
   Repeated initialization is safe."
  []
  (install-fx!)
  (install-transport-handoff!)
  true)

(defn diagnostics
  []
  {:active-scopes
   (into {}
         (map
          (fn [[scope lock]]
            [scope
             (select-keys
              lock
              [:execution-id
               :target-id])]))
         @target-locks)
   :source-correlations
   (count @executions-by-source)
   :browser-plan
   {:name (:name browser-plan)
    :role (:role browser-plan)
    :state-count
    (count
     (:states browser-plan))}})
