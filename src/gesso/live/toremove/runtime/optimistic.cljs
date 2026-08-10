(ns gesso.live.runtime.optimistic
  "Browser implementation of the built-in Gesso Live optimistic choreography.

   This namespace is deliberately an effect/event adapter, not a second state
   machine. The semantic lifecycle lives in gesso.live.optimistic-choreo and is
   projected to the browser at compile time. gesso.live.choreo.machine executes
   that plan; this namespace supplies the trusted browser effects.

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

   - continuity mechanics (gesso.live.runtime.continuity)
   - generic DOM mechanics (gesso.live.runtime.dom)
   - generic choreography execution (gesso.live.runtime.choreo)
   - HTMX/SSE listener registration (top-level gesso.live.runtime)
   - server/domain transition policy

   A projection is never canonical. A snapshot is never allowed to overwrite
   explicitly canonical state. Distinct opaque revisions are never ordered."
  (:require
   [clojure.string :as str]
   [gesso.live.choreo.machine :as machine]
   [gesso.live.optimistic-choreo :as optimistic
    :include-macros true]
   [gesso.live.protocol :as protocol]
   [gesso.live.runtime.choreo :as runtime]
   [gesso.live.runtime.continuity :as continuity]
   [gesso.live.runtime.dom :as dom]))

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

(defonce target-locks
  "Opaque optimistic scope -> {:execution-id ... :target ... :target-id ...}.

   Scope is the logical lock key rather than DOM-node identity. This survives a
   canonical outerHTML replacement of the target while the command remains
   outstanding."
  (atom {}))

(defonce executions-by-source
  "DOM source uid -> active execution id.

   Kept only to correlate HTMX request lifecycle events that retain the source
   element but do not carry our header back in detail data."
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

(defn- parse-bool
  [x]
  (= "true"
     (some-> x
             str
             str/lower-case)))

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
        protocol/optimistic-protocol-attr)
       "]["
       (attr-name
        protocol/optimistic-transition-attr)
       "]["
       (attr-name
        protocol/optimistic-template-attr)
       "]["
       (attr-name
        protocol/optimistic-target-attr)
       "]["
       (attr-name
        protocol/optimistic-scope-attr)
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
           protocol/optimistic-protocol-attr)
          transition
          (dom/attr
           source
           protocol/optimistic-transition-attr)
          template-name
          (dom/attr
           source
           protocol/optimistic-template-attr)
          target
          (dom/attr
           source
           protocol/optimistic-target-attr)
          scope
          (dom/attr
           source
           protocol/optimistic-scope-attr)
          base-wire
          (dom/attr
           source
           protocol/optimistic-base-revision-attr)
          pending-label
          (dom/attr
           source
           protocol/optimistic-pending-label-attr)
          projection-mode
          (some->
           (dom/attr
            source
            protocol/optimistic-projection-mode-attr)
           keyword)]
      (when-not (= protocol/optimistic-protocol-version
                   protocol-version)
        (throw
         (ex-info
          "Unsupported Gesso Live optimistic protocol version."
          {:error/type
           :gesso.live.optimistic/unsupported-protocol
           :expected
           protocol/optimistic-protocol-version
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
      (when (and projection-mode
                 (not
                  (contains?
                   protocol/projection-modes
                   projection-mode)))
        (throw
         (ex-info
          "Gesso Live optimistic projection mode is invalid."
          {:error/type
           :gesso.live.optimistic/invalid-projection-mode
           :projection-mode projection-mode
           :allowed protocol/projection-modes})))
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
                 protocol/optimistic-template-attr))
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
     protocol/optimistic-execution-attr
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
     protocol/optimistic-execution-attr)
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
;; Browser effects
;; -----------------------------------------------------------------------------

(defn acquire-target-effect
  [ctx _args]
  (let [execution-id
        (get ctx
             optimistic/execution-id-key)
        scope
        (get ctx
             optimistic/scope-key)
        target
        (get ctx
             target-key)
        target-id
        (get ctx
             target-id-key)]
    (when-not
        (reserve-target!
         scope
         execution-id
         target
         target-id)
      (throw
       (ex-info
        "Gesso Live optimistic target is already owned by another execution."
        {:error/type
         :gesso.live.optimistic/target-busy
         :execution-id execution-id
         :scope scope
         :owner
         (:execution-id
          (current-lock scope))})))
    {:optimistic/target-acquired? true}))

(defn capture-continuity-effect
  [ctx _args]
  (let [root
        (get ctx root-key)
        source
        (get ctx source-key)
        slot
        (when root
          (continuity/capture!
           root
           source))]
    {continuity-slot-key slot}))

(defn capture-snapshot-effect
  [ctx _args]
  (let [target
        (current-target ctx)]
    (when-not target
      (throw
       (ex-info
        "Gesso Live optimistic snapshot target disappeared before projection."
        {:error/type
         :gesso.live.optimistic/no-snapshot-target
         :execution-id
         (get ctx
              optimistic/execution-id-key)})))
    {snapshot-key
     (dom/snapshot target)}))

(defn install-projection-effect
  [ctx _args]
  (let [target
        (current-target ctx)
        projection
        (get ctx projection-key)
        descriptor
        (get ctx descriptor-key)
        source
        (get ctx source-key)
        execution-id
        (get ctx
             optimistic/execution-id-key)]
    (when-not target
      (throw
       (ex-info
        "Gesso Live optimistic target disappeared before projection installation."
        {:error/type
         :gesso.live.optimistic/no-projection-target
         :execution-id execution-id})))
    ;; Copy into the existing target object. HTMX may already retain this exact
    ;; object for the request being configured.
    (dom/copy-element-into!
     target
     projection)
    ;; Projection authority is explicitly negative even if an application
    ;; accidentally put canonical markup inside the optimistic template.
    (dom/remove-attr!
     target
     protocol/optimistic-canonical-attr)
    (dom/set-attr!
     target
     protocol/optimistic-scope-attr
     (:scope descriptor))
    (dom/set-attr!
     target
     active-attr
     execution-id)
    (dom/set-attr!
     target
     pending-attr
     "true")
    (dom/set-attr!
     target
     locked-attr
     "true")
    (dom/set-attr!
     target
     "aria-busy"
     "true")
    (htmx-process!
     target)
    {pending-source-state-key
     (mark-source-pending!
      source
      descriptor
      execution-id)}))

(defn schedule-timeout-effect
  [ctx _args]
  (let [execution-id
        (get ctx
             optimistic/execution-id-key)]
    (runtime/schedule-event!
     execution-id
     settlement-timer-key
     default-settlement-timeout-ms
     optimistic/timeout-event
     {optimistic/reason-key
      :settlement-timeout})
    {:optimistic/timeout-scheduled? true}))

(defn- canonical-scope!
  [canonical expected-scope]
  (when-not (dom/canonical? canonical)
    (throw
     (ex-info
      "Optimistic settlement canonical payload is not explicitly canonical."
      {:error/type
       :gesso.live.optimistic/noncanonical-settlement
       :canonical canonical})))
  (when-not (= expected-scope
               (dom/scope canonical))
    (throw
     (ex-info
      "Optimistic settlement canonical scope does not match execution scope."
      {:error/type
       :gesso.live.optimistic/canonical-scope-mismatch
       :expected expected-scope
       :actual
       (dom/scope canonical)})))
  canonical)

(defn- installed-canonical-disposition
  "Decide whether detached settlement canonical may replace current target.

   Existing explicit canonical state wins on equal or incomparable revisions.
   That rule prevents an older correlated POST response from overwriting a
   canonical Live refresh that reached the browser first."
  [target incoming]
  (if-not (dom/canonical? target)
    :install
    (case
        (protocol/compare-revisions
         (dom/revision incoming)
         (dom/revision target))
      :newer
      :install

      :same
      :canonical-wins

      :older
      :canonical-wins

      :incomparable
      :canonical-wins)))

(defn install-canonical-effect
  [ctx _args]
  (let [execution-id
        (get ctx
             optimistic/execution-id-key)
        scope
        (get ctx
             optimistic/scope-key)
        canonical
        (canonical-scope!
         (get ctx
              optimistic/canonical-key)
         scope)
        target
        (current-target ctx)]
    (when-not target
      (throw
       (ex-info
        "Gesso Live cannot reconcile optimistic execution because its target no longer exists."
        {:error/type
         :gesso.live.optimistic/no-canonical-target
         :execution-id execution-id
         :scope scope})))
    (let [disposition
          (installed-canonical-disposition
           target
           canonical)]
      (case disposition
        :install
        (do
          (dom/copy-canonical-into!
           target
           (.cloneNode
            canonical
            true))
          (htmx-process!
           target)
          {optimistic/canonical-disposition-key
           :installed
           canonical-source-key
           :settlement})

        :canonical-wins
        {optimistic/canonical-disposition-key
         :canonical-wins
         canonical-source-key
         :already-installed}))))

(defn discard-snapshot-effect
  [_ctx _args]
  ;; Machine contexts are accumulated maps; nil explicitly denotes that the
  ;; structural snapshot is no longer authorized even though its former value
  ;; may appear in earlier trace/diagnostic material.
  {snapshot-key nil})

(defn recover-snapshot-effect
  [ctx _args]
  (let [execution-id
        (get ctx
             optimistic/execution-id-key)
        snapshot
        (get ctx
             snapshot-key)
        target
        (current-target ctx)]
    (cond
      ;; Any explicit canonical state is authoritative. Recovery never writes an
      ;; old structural snapshot over it, regardless of revision comparability.
      (and target
           (dom/canonical? target))
      {optimistic/recovery-disposition-key
       :canonical-wins}

      (nil? target)
      {optimistic/recovery-disposition-key
       :canonical-wins}

      (nil? snapshot)
      (throw
       (ex-info
        "Optimistic recovery has no authorized structural snapshot."
        {:error/type
         :gesso.live.optimistic/no-recovery-snapshot
         :execution-id execution-id}))

      ;; Only this execution's still-provisional target may be rolled back.
      (not=
       execution-id
       (dom/attr
        target
        active-attr))
      (throw
       (ex-info
        "Optimistic recovery target is neither canonical nor owned by this execution."
        {:error/type
         :gesso.live.optimistic/recovery-authority-lost
         :execution-id execution-id
         :active
         (dom/attr
          target
          active-attr)}))

      :else
      (let [source
            (dom/snapshot-node
             snapshot)]
        (dom/copy-element-into!
         target
         source)
        (htmx-process!
         target)
        {optimistic/recovery-disposition-key
         :recovered}))))

(defn restore-continuity-effect
  [ctx _args]
  (let [root
        (get ctx root-key)]
    (when root
      ;; Use the exact same two-phase continuity path as ordinary HTMX/OOB
      ;; replacement: details-open is restored immediately and the full
      ;; scroll/focus/input pass runs after layout settles.
      (continuity/restore!
       root))
    {:optimistic/continuity-restored? true}))

(defn cancel-timeout-effect
  [ctx _args]
  (runtime/cancel-timer!
   (get ctx
        optimistic/execution-id-key)
   settlement-timer-key)
  {:optimistic/timeout-cancelled? true})

(defn clear-pending-effect
  [ctx _args]
  (let [execution-id
        (get ctx
             optimistic/execution-id-key)
        target
        (current-target ctx)
        source-state
        (get ctx
             pending-source-state-key)]
    (when (and target
               (= execution-id
                  (dom/attr
                   target
                   active-attr)))
      (dom/remove-attr!
       target
       active-attr)
      (dom/remove-attr!
       target
       pending-attr)
      (dom/remove-attr!
       target
       locked-attr)
      (dom/remove-attr!
       target
       "aria-busy"))
    (clear-source-pending!
     source-state)
    ;; This effect is on every normal terminal path, including timeout. Source
    ;; correlation therefore belongs here rather than in an HTMX afterRequest
    ;; adapter that may never run after an environmental timeout.
    (when-some [source
                (get ctx source-key)]
      (swap!
       executions-by-source
       dissoc
       (node-uid source)))
    {:optimistic/pending-cleared? true}))

(defn release-target-effect
  [ctx _args]
  (release-target!
   (get ctx
        optimistic/scope-key)
   (get ctx
        optimistic/execution-id-key))
  {:optimistic/target-released? true})

(def browser-effect-handlers
  {optimistic/acquire-target-effect
   acquire-target-effect

   optimistic/capture-continuity-effect
   capture-continuity-effect

   optimistic/capture-snapshot-effect
   capture-snapshot-effect

   optimistic/install-projection-effect
   install-projection-effect

   optimistic/schedule-timeout-effect
   schedule-timeout-effect

   optimistic/install-canonical-effect
   install-canonical-effect

   optimistic/discard-snapshot-effect
   discard-snapshot-effect

   optimistic/recover-snapshot-effect
   recover-snapshot-effect

   optimistic/restore-continuity-effect
   restore-continuity-effect

   optimistic/cancel-timeout-effect
   cancel-timeout-effect

   optimistic/clear-pending-effect
   clear-pending-effect

   optimistic/release-target-effect
   release-target-effect})

(defn install-effect-handlers!
  []
  (doseq [[effect-id handler]
          browser-effect-handlers]
    (runtime/register-effect!
     effect-id
     handler))
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
     :target             already-resolved HTMX target, when available
     :execution-id       caller-provided id; otherwise generated
     :consistency-token  optional DB visibility token already associated with
                         the request

   Returns a map containing the execution id, suspended/completed machine
   execution, and semantic outgoing command descriptor recorded by the generic
   send effect. The top-level HTMX adapter uses that descriptor to attach the
   execution id/header to the request; it does not invent command semantics."
  ([source]
   (start! source nil))
  ([source {:keys
            [target
             execution-id
             consistency-token]}]
   (let [execution-id
         (or execution-id
             (gesso.live.runtime.optimistic/execution-id))
         prepared
         (prepare
          source
          target)
         source-uid
         (node-uid source)
         execution
         (runtime/start!
          browser-plan
          {:execution-id execution-id
           :context
           (initial-context
            prepared
            execution-id
            consistency-token)
           :metadata
           {:kind :optimistic
            :source source-uid}})]
     (when (machine/suspended?
            execution)
       (swap!
        executions-by-source
        assoc
        source-uid
        execution-id))
     (let [ctx
           (machine/execution-context
            execution)]
       (emit!
        (:root prepared)
        "started"
        #js {:executionId execution-id
             :transition
             (get ctx
                  optimistic/transition-key)
             :scope
             (get ctx
                  optimistic/scope-key)})
       {:execution-id execution-id
        :execution execution
        :command
        (get ctx
             runtime/outgoing-send-key)}))))

;; -----------------------------------------------------------------------------
;; Settlement response parsing
;; -----------------------------------------------------------------------------

(defn settlement-marker?
  [element]
  (and
   (dom/template? element)
   (dom/truthy-attr?
    element
    protocol/optimistic-settlement-attr)))

(defn marker->settlement
  "Decode one inert settlement marker into the choreography payload fields that
   are actually carried by the marker.

   Canonical content is attached separately after explicit authority selection."
  [marker]
  (when (settlement-marker?
         marker)
    (let [execution-id
          (dom/attr
           marker
           protocol/optimistic-execution-attr)
          scope
          (dom/attr
           marker
           protocol/optimistic-scope-attr)
          outcome
          (some->
           (dom/attr
            marker
            protocol/optimistic-outcome-attr)
           keyword)
          revision-wire
          (dom/attr
           marker
           protocol/optimistic-revision-attr)]
      (when-not
          (contains?
           optimistic/settlement-outcomes
           outcome)
        (throw
         (ex-info
          "Gesso Live settlement marker carries an invalid outcome."
          {:error/type
           :gesso.live.optimistic/invalid-settlement-outcome
           :outcome outcome})))
      {optimistic/execution-id-key
       execution-id
       optimistic/scope-key
       scope
       optimistic/outcome-key
       outcome
       optimistic/command-applied-key
       (parse-bool
        (dom/attr
         marker
         protocol/optimistic-command-applied-attr))
       optimistic/revision-key
       (when revision-wire
         (protocol/wire->revision
          revision-wire))
       optimistic/reason-key
       (dom/attr
        marker
        protocol/optimistic-reason-attr)})))

(defn- settlement-markers
  [root]
  (let [selector
        (str "template["
             (attr-name
              protocol/optimistic-settlement-attr)
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
                 protocol/optimistic-execution-attr)))
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
  "Extract one complete semantic settlement payload from detached response root.

   The response is valid only when both its correlated settlement marker and one
   explicitly canonical element for the same scope are present."
  [root expected-execution-id]
  (when-some [marker
              (marker-for-execution
               root
               expected-execution-id)]
    (let [settlement
          (marker->settlement
           marker)
          canonical
          (canonical-from-response
           root
           (get settlement
                optimistic/scope-key))]
      (when-not canonical
        (throw
         (ex-info
          "Optimistic settlement response does not contain explicitly canonical content for its scope."
          {:error/type
           :gesso.live.optimistic/missing-canonical
           :execution-id expected-execution-id
           :scope
           (get settlement
                optimistic/scope-key)})))
      (assoc settlement
             optimistic/canonical-key
             canonical))))

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

(defn command-payload
  "Return the semantic command payload produced by the compiled choreography."
  [execution-id]
  (some->
   (runtime/execution-context
    execution-id)
   (get runtime/outgoing-send-key)
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
  (runtime/abort!
   execution-id
   reason))

;; -----------------------------------------------------------------------------
;; Initialization / diagnostics
;; -----------------------------------------------------------------------------

(defn initialize!
  "Register optimistic browser effects with the generic choreography runtime.

   Listener registration remains in gesso.live.runtime."
  []
  (install-effect-handlers!)
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
