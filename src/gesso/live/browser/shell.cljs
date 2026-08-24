(ns gesso.live.browser.shell
  "Narrow imperative browser shell for the pure Gesso Live adapter.

   The semantic browser runtime lives in gesso.live.browser.adapter. This
   namespace is deliberately not another state machine. Its job is only to:

   - hold one mutable AdapterState for one browser runtime instance
   - feed normalized events through adapter/step
   - execute the returned abstract effects
   - own opaque physical timer/transport/continuity resources
   - translate physical completion/failure back into normalized adapter events
   - provide an ephemeral physical context for HTMX/DOM lifecycle effects

   Raw DOM nodes, HTMX events, XHR objects, AbortControllers, timeout handles,
   captured continuity values, Promises, and other host objects never enter the
   portable AdapterState.

   Effect handlers receive one map:

     {:effect   <abstract-effect-data>
      :physical <ephemeral physical context or nil>
      :resource <opaque physical resource when applicable>}

   The :physical value is valid only during interpretation of the current
   dispatch. It is never retained by this namespace.

   Handlers for :machine/local and :machine/send may return either an immediate
   value or a Promise. Their successful results become normalized
   :machine/local-completed and :machine/send-requested events respectively.
   Failure retires that execution generation through the adapter instead of
   mutating adapter state directly.

   The :transport/send handler may return:

     <immediate-or-promise-completion>

   or

     {:completion <immediate-or-promise-completion>
      :cancel!    <optional zero-arity function>}

   Transport completion is translated to :transport/succeeded or
   :transport/failed. Adapter-emitted :transport/cancel invokes only the
   cancellation function owned by the exact physical transport generation.

   Continuity capture is intentionally synchronous: the opaque value returned
   by the :continuity/capture handler is stored only in this shell, keyed by the
   adapter-issued slot identity/generation. :continuity/restore receives that
   opaque value as :resource and may complete synchronously or asynchronously.
   Only successful restoration produces :continuity/completed.

   HTMX remains HTMX-owned. This shell does not issue requests, perform swaps,
   parse responses, or know HTMX event shapes. browser/core.cljs will normalize
   documented HTMX lifecycle observations and provide the physical handlers for
   allow/cancel/refresh effects."
  (:require
   [gesso.live.browser.adapter :as adapter]))

;; =============================================================================
;; Identity / option vocabulary
;; =============================================================================

(def shell-version 1)

(def shell-type
  :gesso.live.browser.shell/runtime)

(def option-keys
  #{:state
    :handlers
    :on-error
    :on-transition
    :on-diagnostic
    :set-timeout!
    :clear-timeout!})

(def observer-effect-kinds
  "Effects whose handlers are observational only. Handler failure must not alter
   adapter semantics or block cleanup."
  #{:execution/completed
    :execution/retired
    :fragment/request-failed
    :authoritative/installed
    :htmx/allow-request
    :htmx/allow-swap
    :diagnostic/ignored})

(def synchronous-required-handler-kinds
  "Effects whose physical action must happen synchronously in the lifecycle call
   that produced them. Missing/throwing handlers fail closed."
  #{:fragment/refresh
    :htmx/cancel-request
    :htmx/cancel-swap
    :continuity/capture})

(def async-completion-handler-kinds
  #{:machine/local
    :machine/send
    :transport/send
    :continuity/restore})

;; =============================================================================
;; Errors / validation
;; =============================================================================

(defn- shell-error
  ([kind message data]
   (shell-error kind message data nil))
  ([kind message data cause]
   (ex-info
    message
    (merge
     {:error/type :gesso.live.browser.shell/error
      :error/kind kind}
     data)
    cause)))

(defn- require-map!
  [label value]
  (when-not (map? value)
    (throw
     (shell-error
      :invalid-map
      (str label " must be a map.")
      {:label label
       :value value})))
  value)

(defn- require-callable!
  [label value]
  (when-not (fn? value)
    (throw
     (shell-error
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
       (shell-error
        :unknown-options
        "Gesso Live browser shell options contain unsupported keys."
        {:unknown-keys (set unknown)
         :allowed-keys option-keys}))))
  options)

(defn- require-handler-map!
  [handlers]
  (require-map! "Browser shell handlers" handlers)
  (doseq [[effect-kind handler] handlers]
    (when-not (contains? adapter/abstract-effect-kinds effect-kind)
      (throw
       (shell-error
        :unknown-effect-handler
        "Browser shell handler names an unknown adapter effect kind."
        {:effect-kind effect-kind
         :known-effect-kinds adapter/abstract-effect-kinds})))
    (require-callable! "Browser shell effect handler" handler))
  handlers)

(defn- promise-like?
  [value]
  (and (some? value)
       (let [then-fn (.-then value)]
         (= "function" (js* "typeof ~{}" then-fn)))))

(defn- error-message
  [error]
  (cond
    (nil? error)
    nil

    (instance? js/Error error)
    (.-message error)

    :else
    (str error)))

(defn- error-name
  [error]
  (when (instance? js/Error error)
    (.-name error)))

(defn- physical-failure-reason
  [effect-kind error]
  (cond->
   {:kind :physical-effect-failed
    :effect effect-kind}
    (some? (error-name error))
    (assoc :error-name (error-name error))
    (some? (error-message error))
    (assoc :message (error-message error))))

;; =============================================================================
;; Default host seams
;; =============================================================================

(defn- default-set-timeout!
  [callback delay-ms]
  (js/setTimeout callback delay-ms))

(defn- default-clear-timeout!
  [handle]
  (js/clearTimeout handle))

;; =============================================================================
;; Runtime construction / predicates / accessors
;; =============================================================================

(defn create
  "Create one browser shell runtime.

   No singleton/global runtime is created by this namespace. Multiple shell
   instances may coexist safely in tests, hot reload, embedded applications, or
   multiple isolated roots.

   Options:

     :state
       Initial pure adapter state. Defaults to adapter/initial-state.

     :handlers
       Abstract-effect-kind -> physical handler map.

     :on-error
       Optional diagnostic observer. Receives a plain map describing a physical
       or shell integration failure. Errors thrown by this observer are ignored.

     :on-transition
       Optional read-only observer called after each successful adapter step as
       {:event ... :effects ... :state ...}. Raw physical context is excluded.
       Observer failure is diagnostic-only.

     :on-diagnostic
       Optional observer for adapter :diagnostic/ignored effects.

     :set-timeout! / :clear-timeout!
       Injectable browser timer seams."
  ([]
   (create nil))
  ([options]
   (let [options (or options {})
         _ (require-map! "Browser shell options" options)
         _ (check-option-keys! options)
         initial-state (or (:state options)
                           (adapter/initial-state))
         _ (adapter/require-state! initial-state)
         handlers (or (:handlers options) {})
         _ (require-handler-map! handlers)
         on-error (:on-error options)
         on-transition (:on-transition options)
         on-diagnostic (:on-diagnostic options)
         set-timeout! (or (:set-timeout! options)
                          default-set-timeout!)
         clear-timeout! (or (:clear-timeout! options)
                            default-clear-timeout!)]
     (require-optional-callable! ":on-error" on-error)
     (require-optional-callable! ":on-transition" on-transition)
     (require-optional-callable! ":on-diagnostic" on-diagnostic)
     (require-callable! ":set-timeout!" set-timeout!)
     (require-callable! ":clear-timeout!" clear-timeout!)
     {:gesso.live.browser.shell/type shell-type
      :gesso.live.browser.shell/version shell-version
      :state (atom initial-state)
      :handlers (atom handlers)
      :resources (atom {:timers {}
                        :transports {}
                        :continuity {}})
      :closed? (atom false)
      :on-error on-error
      :on-transition on-transition
      :on-diagnostic on-diagnostic
      :set-timeout! set-timeout!
      :clear-timeout! clear-timeout!})))

(defn shell?
  [value]
  (and
   (map? value)
   (= shell-type (:gesso.live.browser.shell/type value))
   (= shell-version (:gesso.live.browser.shell/version value))
   (some? (:state value))
   (some? (:handlers value))
   (some? (:resources value))
   (some? (:closed? value))))

(defn require-shell!
  [runtime]
  (when-not (shell? runtime)
    (throw
     (shell-error
      :invalid-runtime
      "Expected a Gesso Live browser shell runtime."
      {:runtime runtime})))
  runtime)

(defn state
  "Return the current pure AdapterState."
  [runtime]
  (let [runtime (require-shell! runtime)]
    @(:state runtime)))

(defn closed?
  [runtime]
  (true? @(:closed? (require-shell! runtime))))

(defn handlers
  "Return the current physical effect-handler map."
  [runtime]
  @(:handlers (require-shell! runtime)))

(defn resources
  "Return the opaque physical-resource registry.

   Intended for tests/diagnostics only. Production semantic decisions must never
   depend on host objects stored here."
  [runtime]
  @(:resources (require-shell! runtime)))

(defn resource-counts
  [runtime]
  (let [{:keys [timers transports continuity]}
        (resources runtime)]
    {:timers (count timers)
     :transports (count transports)
     :continuity (count continuity)}))

;; =============================================================================
;; Handler registration (physical implementation only)
;; =============================================================================

(defn register-handler!
  [runtime effect-kind handler]
  (let [runtime (require-shell! runtime)]
    (when-not (contains? adapter/abstract-effect-kinds effect-kind)
      (throw
       (shell-error
        :unknown-effect-handler
        "Cannot register a handler for an unknown adapter effect kind."
        {:effect-kind effect-kind
         :known-effect-kinds adapter/abstract-effect-kinds})))
    (require-callable! "Browser shell effect handler" handler)
    (swap! (:handlers runtime) assoc effect-kind handler)
    effect-kind))

(defn unregister-handler!
  [runtime effect-kind]
  (let [runtime (require-shell! runtime)]
    (swap! (:handlers runtime) dissoc effect-kind)
    effect-kind))

;; =============================================================================
;; Diagnostic observers
;; =============================================================================

(defn- call-observer-safely!
  [observer value]
  (when observer
    (try
      (observer value)
      (catch :default _
        nil))))

(defn- report-error!
  [runtime phase effect-kind effect-data error]
  (call-observer-safely!
   (:on-error runtime)
   {:phase phase
    :effect effect-kind
    :effect-data effect-data
    :error-name (error-name error)
    :message (error-message error)}))

(defn- observe-transition!
  [runtime event next-state effects]
  (call-observer-safely!
   (:on-transition runtime)
   {:event event
    :effects effects
    :state next-state}))

(defn- observe-diagnostic!
  [runtime effect-data]
  (call-observer-safely!
   (:on-diagnostic runtime)
   effect-data))

;; =============================================================================
;; Physical resource identities
;; =============================================================================

(defn- timer-resource-key
  [{:keys [execution-id generation timer-id timer-generation]}]
  [execution-id generation timer-id timer-generation])

(defn- transport-resource-key
  [{:keys [execution-id generation effect-generation]}]
  [execution-id generation effect-generation])

(defn- continuity-resource-key
  [{:keys [slot-id slot-generation]}]
  [slot-id slot-generation])

;; =============================================================================
;; Handler invocation
;; =============================================================================

(defn- effect-handler
  [runtime effect-kind]
  (get @(:handlers runtime) effect-kind))

(defn- invoke-handler
  [runtime effect-kind effect-data physical resource]
  (if-let [handler (effect-handler runtime effect-kind)]
    (handler {:effect effect-data
              :physical physical
              :resource resource})
    ::missing-handler))

(defn- require-handler-result!
  [value effect-kind]
  (when (= ::missing-handler value)
    (throw
     (shell-error
      :missing-effect-handler
      "Browser shell has no physical handler for a required adapter effect."
      {:effect-kind effect-kind})))
  value)

;; =============================================================================
;; Forward declarations
;; =============================================================================

(declare dispatch!)
(declare interpret-effects!)

;; =============================================================================
;; Completion helpers
;; =============================================================================

(defn- retire-after-physical-failure!
  [runtime effect-kind effect-data error]
  (report-error!
   runtime
   :effect-failed
   effect-kind
   effect-data
   error)
  (dispatch!
   runtime
   {:event :execution/retire
    :execution-id (:execution-id effect-data)
    :generation (:generation effect-data)
    :reason (physical-failure-reason effect-kind error)}))

(defn- complete-machine-local!
  [runtime effect-data outputs]
  (dispatch!
   runtime
   {:event :machine/local-completed
    :execution-id (:execution-id effect-data)
    :generation (:generation effect-data)
    :effect-generation (:effect-generation effect-data)
    :outputs (or outputs {})}))

(defn- complete-machine-send!
  [runtime effect-data payload]
  (dispatch!
   runtime
   {:event :machine/send-requested
    :execution-id (:execution-id effect-data)
    :generation (:generation effect-data)
    :effect-generation (:effect-generation effect-data)
    :payload (or payload {})}))

(defn- transport-succeeded!
  [runtime effect-data]
  (dispatch!
   runtime
   {:event :transport/succeeded
    :execution-id (:execution-id effect-data)
    :generation (:generation effect-data)
    :effect-generation (:effect-generation effect-data)}))

(defn- transport-failed!
  [runtime effect-data error]
  (report-error!
   runtime
   :transport-failed
   :transport/send
   effect-data
   error)
  (dispatch!
   runtime
   {:event :transport/failed
    :execution-id (:execution-id effect-data)
    :generation (:generation effect-data)
    :effect-generation (:effect-generation effect-data)
    :reason (physical-failure-reason :transport/send error)}))

(defn- continuity-completed!
  [runtime effect-data]
  (dispatch!
   runtime
   {:event :continuity/completed
    :slot-id (:slot-id effect-data)
    :slot-generation (:slot-generation effect-data)}))

;; =============================================================================
;; Async result settlement
;; =============================================================================

(defn- settle-result!
  [value on-success on-failure]
  (if (promise-like? value)
    (do
      (.then value on-success on-failure)
      :pending)
    (do
      (on-success value)
      :completed)))

;; =============================================================================
;; Machine effect interpretation
;; =============================================================================

(defn- interpret-machine-local!
  [runtime effect-data physical]
  (try
    (let [result
          (-> (invoke-handler runtime :machine/local effect-data physical nil)
              (require-handler-result! :machine/local))]
      (settle-result!
       result
       #(complete-machine-local! runtime effect-data %)
       #(retire-after-physical-failure!
         runtime :machine/local effect-data %)))
    (catch :default error
      (retire-after-physical-failure!
       runtime :machine/local effect-data error)
      :failed)))

(defn- interpret-machine-send!
  [runtime effect-data physical]
  (try
    (let [result
          (-> (invoke-handler runtime :machine/send effect-data physical nil)
              (require-handler-result! :machine/send))]
      (settle-result!
       result
       #(complete-machine-send! runtime effect-data %)
       #(retire-after-physical-failure!
         runtime :machine/send effect-data %)))
    (catch :default error
      (retire-after-physical-failure!
       runtime :machine/send effect-data error)
      :failed)))

;; =============================================================================
;; Transport effect interpretation
;; =============================================================================

(defn- transport-result
  [value]
  (if (and (map? value)
           (or (contains? value :completion)
               (contains? value :cancel!)))
    (do
      (when-not (contains? value :completion)
        (throw
         (shell-error
          :invalid-transport-result
          "Structured transport result requires :completion."
          {:transport-result value})))
      (when-let [cancel! (:cancel! value)]
        (require-callable! "Transport :cancel!" cancel!))
      {:completion (:completion value)
       :cancel! (:cancel! value)})
    {:completion value
     :cancel! nil}))

(defn- remove-transport-resource!
  [runtime resource-key]
  (swap! (:resources runtime)
         update :transports dissoc resource-key))

(defn- interpret-transport-send!
  [runtime effect-data physical]
  (let [resource-key (transport-resource-key effect-data)]
    (try
      (let [{:keys [completion cancel!]}
            (-> (invoke-handler runtime :transport/send effect-data physical nil)
                (require-handler-result! :transport/send)
                transport-result)]
        (when cancel!
          (swap! (:resources runtime)
                 assoc-in [:transports resource-key]
                 {:cancel! cancel!}))
        (settle-result!
         completion
         (fn [_]
           (remove-transport-resource! runtime resource-key)
           (transport-succeeded! runtime effect-data))
         (fn [error]
           (remove-transport-resource! runtime resource-key)
           (transport-failed! runtime effect-data error))))
      (catch :default error
        (remove-transport-resource! runtime resource-key)
        (transport-failed! runtime effect-data error)
        :failed))))

(defn- interpret-transport-cancel!
  [runtime effect-data]
  (let [resource-key (transport-resource-key effect-data)
        resource (get-in @(:resources runtime)
                         [:transports resource-key])]
    (remove-transport-resource! runtime resource-key)
    (when-let [cancel! (:cancel! resource)]
      (try
        (cancel!)
        (catch :default error
          (report-error!
           runtime
           :transport-cancel-failed
           :transport/cancel
           effect-data
           error))))
    :cancelled))

;; =============================================================================
;; Timer effect interpretation
;; =============================================================================

(defn- remove-timer-resource!
  [runtime resource-key]
  (swap! (:resources runtime)
         update :timers dissoc resource-key))

(defn- interpret-timer-start!
  [runtime effect-data]
  (let [resource-key (timer-resource-key effect-data)
        callback
        (fn []
          (remove-timer-resource! runtime resource-key)
          (when-not (closed? runtime)
            (dispatch!
             runtime
             {:event :timer/fired
              :execution-id (:execution-id effect-data)
              :generation (:generation effect-data)
              :timer-id (:timer-id effect-data)
              :timer-generation (:timer-generation effect-data)})))]
    (try
      (let [handle ((:set-timeout! runtime)
                    callback
                    (:delay-ms effect-data))]
        (swap! (:resources runtime)
               assoc-in [:timers resource-key]
               {:handle handle})
        :started)
      (catch :default error
        (report-error!
         runtime
         :timer-start-failed
         :timer/start
         effect-data
         error)
        (retire-after-physical-failure!
         runtime :timer/start effect-data error)
        :failed))))

(defn- interpret-timer-cancel!
  [runtime effect-data]
  (let [resource-key (timer-resource-key effect-data)
        resource (get-in @(:resources runtime)
                         [:timers resource-key])]
    (remove-timer-resource! runtime resource-key)
    (when (contains? resource :handle)
      (try
        ((:clear-timeout! runtime) (:handle resource))
        (catch :default error
          (report-error!
           runtime
           :timer-cancel-failed
           :timer/cancel
           effect-data
           error))))
    :cancelled))

;; =============================================================================
;; Continuity physical resources
;; =============================================================================

(defn- interpret-continuity-capture!
  [runtime effect-data physical]
  (let [resource-key (continuity-resource-key effect-data)
        captured
        (-> (invoke-handler
             runtime :continuity/capture effect-data physical nil)
            (require-handler-result! :continuity/capture))]
    (when (promise-like? captured)
      (throw
       (shell-error
        :async-continuity-capture
        "Continuity capture must complete synchronously before HTMX swap permission is returned."
        {:effect-data effect-data})))
    (swap! (:resources runtime)
           assoc-in [:continuity resource-key]
           captured)
    :captured))

(defn- interpret-continuity-restore!
  [runtime effect-data physical]
  (let [resource-key (continuity-resource-key effect-data)
        resource (get-in @(:resources runtime)
                         [:continuity resource-key])]
    (try
      (let [result
            (-> (invoke-handler
                 runtime :continuity/restore effect-data physical resource)
                (require-handler-result! :continuity/restore))]
        (settle-result!
         result
         (fn [_]
           (swap! (:resources runtime)
                  update :continuity dissoc resource-key)
           (continuity-completed! runtime effect-data))
         (fn [error]
           (report-error!
            runtime
            :continuity-restore-failed
            :continuity/restore
            effect-data
            error))))
      (catch :default error
        (report-error!
         runtime
         :continuity-restore-failed
         :continuity/restore
         effect-data
         error)
        :failed))))

(defn- interpret-continuity-release!
  [runtime effect-data physical]
  (let [resource-key (continuity-resource-key effect-data)
        resource (get-in @(:resources runtime)
                         [:continuity resource-key])]
    (swap! (:resources runtime)
           update :continuity dissoc resource-key)
    (when-let [handler (effect-handler runtime :continuity/release)]
      (try
        (handler {:effect effect-data
                  :physical physical
                  :resource resource})
        (catch :default error
          (report-error!
           runtime
           :continuity-release-failed
           :continuity/release
           effect-data
           error))))
    :released))

;; =============================================================================
;; Delegated one-way effects
;; =============================================================================

(defn- interpret-required-synchronous!
  [runtime effect-kind effect-data physical]
  (let [result
        (-> (invoke-handler runtime effect-kind effect-data physical nil)
            (require-handler-result! effect-kind))]
    (when (promise-like? result)
      (throw
       (shell-error
        :async-synchronous-effect
        "This browser effect must complete synchronously in its lifecycle callback."
        {:effect-kind effect-kind
         :effect-data effect-data})))
    result))

(defn- interpret-observer-effect!
  [runtime effect-kind effect-data physical]
  (when-let [handler (effect-handler runtime effect-kind)]
    (try
      (handler {:effect effect-data
                :physical physical
                :resource nil})
      (catch :default error
        (report-error!
         runtime
         :observer-effect-failed
         effect-kind
         effect-data
         error))))
  :observed)

(defn- interpret-fragment-refresh!
  [runtime effect-data physical]
  (try
    (interpret-required-synchronous!
     runtime :fragment/refresh effect-data physical)
    (catch :default error
      (report-error!
       runtime
       :fragment-refresh-failed
       :fragment/refresh
       effect-data
       error)
      ;; The adapter has already allocated the logical inflight generation. If
      ;; the physical HTMX trigger cannot even be issued, retire this fragment
      ;; rather than leaving an immortal inflight generation.
      (dispatch!
       runtime
       {:event :fragment/retire
        :fragment-id (:fragment-id effect-data)
        :reason (physical-failure-reason :fragment/refresh error)})
      :failed)))

;; =============================================================================
;; Abstract effect interpreter
;; =============================================================================

(defn- require-effect!
  [value]
  (when-not (and (vector? value)
                 (= 2 (count value))
                 (contains? adapter/abstract-effect-kinds (first value))
                 (map? (second value)))
    (throw
     (shell-error
      :invalid-abstract-effect
      "Adapter emitted an invalid abstract browser effect."
      {:effect value
       :known-effect-kinds adapter/abstract-effect-kinds})))
  value)

(defn- interpret-effect!
  [runtime effect-value physical]
  (let [[effect-kind effect-data]
        (require-effect! effect-value)]
    (case effect-kind
      :machine/local
      (interpret-machine-local! runtime effect-data physical)

      :machine/send
      (interpret-machine-send! runtime effect-data physical)

      :transport/send
      (interpret-transport-send! runtime effect-data physical)

      :transport/cancel
      (interpret-transport-cancel! runtime effect-data)

      :timer/start
      (interpret-timer-start! runtime effect-data)

      :timer/cancel
      (interpret-timer-cancel! runtime effect-data)

      :fragment/refresh
      (interpret-fragment-refresh! runtime effect-data physical)

      :htmx/cancel-request
      (interpret-required-synchronous!
       runtime effect-kind effect-data physical)

      :htmx/cancel-swap
      (interpret-required-synchronous!
       runtime effect-kind effect-data physical)

      :continuity/capture
      (interpret-continuity-capture! runtime effect-data physical)

      :continuity/restore
      (interpret-continuity-restore! runtime effect-data physical)

      :continuity/release
      (interpret-continuity-release! runtime effect-data physical)

      :diagnostic/ignored
      (do
        (observe-diagnostic! runtime effect-data)
        (interpret-observer-effect!
         runtime effect-kind effect-data physical))

      (:execution/completed
       :execution/retired
       :fragment/request-failed
       :authoritative/installed
       :htmx/allow-request
       :htmx/allow-swap)
      (interpret-observer-effect!
       runtime effect-kind effect-data physical)

      (throw
       (shell-error
        :unhandled-abstract-effect
        "Browser shell has no interpreter branch for adapter effect."
        {:effect-kind effect-kind
         :effect-data effect-data})))))

(defn interpret-effects!
  "Interpret adapter abstract effects in order.

   physical is optional ephemeral host context associated with the normalized
   event that produced these effects. It is never stored.

   Returns one result per effect."
  ([runtime effects]
   (interpret-effects! runtime effects nil))
  ([runtime effects physical]
   (let [runtime (require-shell! runtime)]
     (mapv
      #(interpret-effect! runtime % physical)
      effects))))

;; =============================================================================
;; Normalized event dispatch
;; =============================================================================

(defn dispatch!
  "Feed one normalized event to the pure adapter and interpret its effects.

   The adapter state is installed before any physical effect runs. Therefore a
   synchronous callback produced by an effect handler can re-enter dispatch!
   without observing the pre-transition state.

   physical is an optional ephemeral browser/HTMX context. It is passed only to
   effect handlers triggered by this transition and is never included in
   AdapterState, transition observers, or completion events.

   Returns:

     {:status           :dispatched | :closed
      :event            normalized-event
      :transition-state state immediately produced by adapter/step
      :state            current state after effect interpretation/re-entry
      :effects          abstract effects
      :effect-results   physical interpreter results}"
  ([runtime event]
   (dispatch! runtime event nil))
  ([runtime event physical]
   (let [runtime (require-shell! runtime)]
     (if (closed? runtime)
       {:status :closed
        :event event
        :state (state runtime)
        :effects []
        :effect-results []}
       (try
         (let [[next-state effects]
               (adapter/step (state runtime) event)
               _ (adapter/require-state! next-state)
               _ (reset! (:state runtime) next-state)
               _ (observe-transition! runtime event next-state effects)
               effect-results
               (interpret-effects! runtime effects physical)]
           {:status :dispatched
            :event event
            :transition-state next-state
            :state (state runtime)
            :effects effects
            :effect-results effect-results})
         (catch :default error
           (report-error!
            runtime
            :dispatch-failed
            nil
            {:event event}
            error)
           (throw error)))))))

;; =============================================================================
;; Teardown
;; =============================================================================

(defn- active-execution-identities
  [adapter-state]
  (mapv
   (fn [[execution-id record]]
     [execution-id (:generation record)])
   (:executions adapter-state)))

(defn- active-fragment-ids
  [adapter-state]
  (vec (keys (:fragments adapter-state))))

(defn- best-effort-clear-orphan-resources!
  [runtime]
  (let [{:keys [timers transports]}
        @(:resources runtime)]
    (doseq [[_ {:keys [handle]}] timers]
      (try
        ((:clear-timeout! runtime) handle)
        (catch :default error
          (report-error!
           runtime
           :shutdown-timer-cancel-failed
           :timer/cancel
           {}
           error))))
    (doseq [[_ {:keys [cancel!]}] transports
            :when cancel!]
      (try
        (cancel!)
        (catch :default error
          (report-error!
           runtime
           :shutdown-transport-cancel-failed
           :transport/cancel
           {}
           error))))
    (reset! (:resources runtime)
            {:timers {}
             :transports {}
             :continuity {}})))

(defn shutdown!
  "Semantically retire current adapter-owned work, then tear down any leftover
   opaque physical resources.

   Cleanup is best effort. New ownership never waits on physical cancellation.
   Late callbacks after shutdown are harmless because dispatch! returns :closed."
  [runtime]
  (let [runtime (require-shell! runtime)]
    (when-not (closed? runtime)
      (doseq [[execution-id generation]
              (active-execution-identities (state runtime))]
        (dispatch!
         runtime
         {:event :execution/retire
          :execution-id execution-id
          :generation generation
          :reason :runtime-shutdown}))
      (doseq [fragment-id
              (active-fragment-ids (state runtime))]
        (dispatch!
         runtime
         {:event :fragment/retire
          :fragment-id fragment-id
          :reason :runtime-shutdown}))
      (best-effort-clear-orphan-resources! runtime)
      (reset! (:closed? runtime) true))
    :closed))

(defn diagnostics
  "Return plain browser-shell diagnostics without exposing host resources."
  [runtime]
  (let [runtime (require-shell! runtime)
        adapter-state (state runtime)]
    {:gesso.live.browser.shell/type shell-type
     :gesso.live.browser.shell/version shell-version
     :closed? (closed? runtime)
     :adapter-version (:gesso.live.browser.adapter/version adapter-state)
     :active-executions (count (:executions adapter-state))
     :active-targets (count (:targets adapter-state))
     :active-fragments (count (:fragments adapter-state))
     :active-continuity-slots (count (:continuity adapter-state))
     :resource-counts (resource-counts runtime)
     :registered-handlers (set (keys (handlers runtime)))
     :invariant-errors (adapter/invariant-errors adapter-state)}))
