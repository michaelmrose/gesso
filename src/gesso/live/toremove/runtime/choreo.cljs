(ns gesso.live.runtime.choreo
  "Browser adapter for the generic Gesso Live choreography machine.

   This namespace owns browser-process execution mechanics:

   - active execution storage
   - effect-handler registration
   - start, suspend, resume, and retirement
   - correlated message/event delivery
   - browser timers that resume executions with modeled events
   - bounded terminal diagnostics

   It deliberately does not know optimistic semantics, DOM replacement,
   continuity policy, HTMX request construction, SSE implementation, or
   application commands. Those are effect/event adapters layered above this
   namespace.

   The generic machine remains pure shared CLJC. This namespace is the small
   stateful bridge between that machine and a long-lived browser process."
  (:require
   [gesso.live.choreo.machine :as machine]))

;; -----------------------------------------------------------------------------
;; Runtime identity
;; -----------------------------------------------------------------------------

(def runtime-type
  :gesso.live.runtime.choreo/runtime)

(def outgoing-send-key
  "Context key used by the default browser send handler.

   The generic browser adapter does not itself perform transport. A projected
   :send records the semantic send descriptor here so the HTMX/transport adapter
   can bind the already-existing browser request to the choreography."
  :gesso.live.choreo/outgoing-send)

(def default-terminal-history-limit
  64)

;; -----------------------------------------------------------------------------
;; Runtime stores
;; -----------------------------------------------------------------------------

(defonce executions
  "Execution id -> suspended machine execution.

   Completed executions are never retained here."
  (atom {}))

(defonce execution-metadata
  "Execution id -> browser-process metadata kept separately from machine context.

   Keeping DOM objects and callbacks out of machine execution data preserves
   inspectability and prevents accidental serialization of browser objects."
  (atom {}))

(defonce effect-handlers
  "Effect keyword -> browser implementation fn.

   Handler shape is the machine contract:

     (fn [accumulated-context args] -> map-or-nil)"
  (atom {}))

(defonce timers
  "Execution id -> timer-key -> browser timeout handle."
  (atom {}))

(defonce terminal-history
  "Bounded vector of DOM-light terminal execution summaries."
  (atom []))

(defonce error-handler
  "Optional runtime-level error callback.

   It receives one map describing the failed runtime operation and throwable.
   Errors are still rethrown unless the public function explicitly documents an
   ignored late-delivery result."
  (atom nil))

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn now-ms
  []
  (.getTime (js/Date.)))

(defn- non-blank-string?
  [x]
  (and (string? x)
       (not= "" (.trim x))))

(defn- require-execution-id!
  [execution-id]
  (when-not (or (keyword? execution-id)
                (uuid? execution-id)
                (non-blank-string? execution-id))
    (throw
     (ex-info
      "Browser choreography execution id must be a keyword, UUID, or non-blank string."
      {:error/type :gesso.live.runtime.choreo/invalid-execution-id
       :execution-id execution-id})))
  execution-id)

(defn- require-nonnegative-number!
  [label value]
  (when-not (and (number? value)
                 (not (js/isNaN value))
                 (not (neg? value)))
    (throw
     (ex-info
      (str label " must be a non-negative number.")
      {:error/type :gesso.live.runtime.choreo/invalid-number
       :label label
       :value value})))
  value)

(defn- notify-error!
  [operation execution-id throwable data]
  (let [payload
        (merge
         {:operation operation
          :execution-id execution-id
          :error throwable}
         data)]
    (when-some [handler @error-handler]
      (try
        (handler payload)
        (catch :default _
          nil)))
    payload))

(defn- trim-terminal-history
  [history limit]
  (let [history (vec history)
        excess (- (count history) limit)]
    (if (pos? excess)
      (subvec history excess)
      history)))

(defn set-error-handler!
  "Install an optional runtime error observer.

   Pass nil to clear it. This callback is diagnostic only; it does not convert
   machine/protocol errors into successful execution."
  [handler]
  (when-not (or (nil? handler)
                (ifn? handler))
    (throw
     (ex-info
      "Browser choreography error handler must be callable or nil."
      {:error/type :gesso.live.runtime.choreo/invalid-error-handler
       :handler handler})))
  (reset! error-handler handler)
  true)

;; -----------------------------------------------------------------------------
;; Effect registration
;; -----------------------------------------------------------------------------

(defn register-effect!
  "Register or replace one browser effect implementation."
  [effect-id handler]
  (when-not (keyword? effect-id)
    (throw
     (ex-info
      "Browser choreography effect id must be a keyword."
      {:error/type :gesso.live.runtime.choreo/invalid-effect-id
       :effect effect-id})))
  (when-not (ifn? handler)
    (throw
     (ex-info
      "Browser choreography effect handler must be callable."
      {:error/type :gesso.live.runtime.choreo/invalid-effect-handler
       :effect effect-id
       :handler handler})))
  (swap! effect-handlers assoc effect-id handler)
  effect-id)

(defn unregister-effect!
  [effect-id]
  (swap! effect-handlers dissoc effect-id)
  effect-id)

(defn registered-effects
  []
  (set (keys @effect-handlers)))

(defn handler
  [effect-id]
  (get @effect-handlers effect-id))

(defn handlers
  "Return the current immutable handler map passed to a machine run/resume."
  []
  @effect-handlers)

(defn record-send
  "Default transport-neutral handler for machine/send-effect.

   It records the projected send descriptor in accumulated context. The actual
   browser transport adapter (for example HTMX) is responsible for binding its
   request to this descriptor.

   This is intentionally not a network implementation."
  [_ctx descriptor]
  {outgoing-send-key descriptor})

(defn install-default-handlers!
  "Install browser-generic handlers.

   Optimistic and continuity handlers are registered by their own namespaces."
  []
  (register-effect!
   machine/send-effect
   record-send)
  true)

;; -----------------------------------------------------------------------------
;; Execution lookup and summaries
;; -----------------------------------------------------------------------------

(defn execution
  [execution-id]
  (get @executions execution-id))

(defn execution-context
  [execution-id]
  (some-> (execution execution-id)
          machine/execution-context))

(defn active?
  [execution-id]
  (contains? @executions execution-id))

(defn active-execution-ids
  []
  (set (keys @executions)))

(defn execution-count
  []
  (count @executions))

(defn metadata
  [execution-id]
  (get @execution-metadata execution-id))

(defn- execution-summary
  [execution]
  {:execution-id (:execution-id execution)
   :plan-name (:plan-name execution)
   :role (:role execution)
   :status (:status execution)
   :state (:state execution)
   :awaiting (:awaiting execution)
   :held-resources
   (machine/held-resources execution)
   :result
   (machine/execution-result execution)
   :trace
   (machine/execution-trace execution)})

(defn active-summaries
  "Return DOM-light summaries of active executions."
  []
  (mapv execution-summary
        (vals @executions)))

(defn terminal-summaries
  []
  @terminal-history)

;; -----------------------------------------------------------------------------
;; Timer ownership
;; -----------------------------------------------------------------------------

(defn timer
  [execution-id timer-key]
  (get-in @timers
          [execution-id timer-key]))

(defn cancel-timer!
  "Cancel one browser timer owned by an execution.

   Returns true whether or not a timer existed, making cleanup idempotent."
  [execution-id timer-key]
  (when-some [handle
              (timer execution-id timer-key)]
    (js/clearTimeout handle))
  (swap!
   timers
   (fn [all]
     (let [remaining
           (dissoc
            (get all execution-id {})
            timer-key)]
       (if (seq remaining)
         (assoc all execution-id remaining)
         (dissoc all execution-id)))))
  true)

(defn cancel-all-timers!
  "Cancel every browser timer owned by execution-id."
  [execution-id]
  (doseq [[_timer-key handle]
          (get @timers execution-id)]
    (js/clearTimeout handle))
  (swap! timers dissoc execution-id)
  true)

(declare resume-event!)

(defn schedule-event!
  "Schedule one modeled environmental event for an execution.

   This may be called from an effect handler while machine/start is still
   running, before the resulting suspended execution has been journaled. Browser
   timeout callbacks cannot run until the current JavaScript stack returns; by
   then a successful start has committed the execution. If start throws after
   scheduling, start! cancels all timers for the execution id.

   Scheduling the same timer-key replaces the prior timer. The timer callback
   removes its own handle before delivery so completion cleanup cannot attempt
   to clear an already-fired handle.

   Late delivery after execution retirement is a harmless :ignored result."
  ([execution-id timer-key delay-ms event-id]
   (schedule-event!
    execution-id
    timer-key
    delay-ms
    event-id
    nil))
  ([execution-id timer-key delay-ms event-id data]
   (require-execution-id! execution-id)
   (when-not (keyword? timer-key)
     (throw
      (ex-info
       "Browser choreography timer key must be a keyword."
       {:error/type :gesso.live.runtime.choreo/invalid-timer-key
        :execution-id execution-id
        :timer-key timer-key})))
   (require-nonnegative-number!
    "Browser choreography timer delay"
    delay-ms)
   (when-not (keyword? event-id)
     (throw
      (ex-info
       "Browser choreography timer event must be a keyword."
       {:error/type :gesso.live.runtime.choreo/invalid-event-id
        :execution-id execution-id
        :event event-id})))
   (cancel-timer!
    execution-id
    timer-key)
   (let [handle
         (js/setTimeout
          (fn []
            (swap!
             timers
             (fn [all]
               (let [remaining
                     (dissoc
                      (get all execution-id {})
                      timer-key)]
                 (if (seq remaining)
                   (assoc all
                          execution-id
                          remaining)
                   (dissoc all
                           execution-id)))))
            (resume-event!
             execution-id
             event-id
             data))
          delay-ms)]
     (swap!
      timers
      assoc-in
      [execution-id timer-key]
      handle)
     handle)))

;; -----------------------------------------------------------------------------
;; Execution lifecycle
;; -----------------------------------------------------------------------------

(defn- remember-terminal!
  [execution metadata]
  (swap!
   terminal-history
   (fn [history]
     (trim-terminal-history
      (conj
       history
       (assoc
        (execution-summary execution)
        :completed-at (now-ms)
        :metadata
        (select-keys
         metadata
         [:started-at
          :source
          :kind])))
      default-terminal-history-limit))))

(defn- retire!
  [execution-id execution]
  (let [metadata
        (get @execution-metadata
             execution-id)]
    (cancel-all-timers!
     execution-id)
    (swap! executions
           dissoc
           execution-id)
    (swap! execution-metadata
           dissoc
           execution-id)
    (remember-terminal!
     execution
     metadata)
    execution))

(defn- commit-machine-result!
  [execution-id execution]
  (cond
    (machine/suspended? execution)
    (do
      (swap! executions
             assoc
             execution-id
             execution)
      (swap! execution-metadata
             update
             execution-id
             assoc
             :updated-at
             (now-ms))
      execution)

    (machine/completed? execution)
    (retire!
     execution-id
     execution)

    :else
    (throw
     (ex-info
      "Choreography machine returned neither suspended nor completed execution."
      {:error/type :gesso.live.runtime.choreo/invalid-machine-result
       :execution-id execution-id
       :execution execution}))))

(defn start!
  "Start one projected browser plan.

   Required options:
     :execution-id

   Optional:
     :context
     :metadata
     :now-fn
     :trace-limit
     :max-immediate-steps

   An execution id may not already be active. The machine runs synchronously
   until its first await or terminal return."
  [plan {:keys [execution-id
                context
                metadata
                now-fn
                trace-limit
                max-immediate-steps]}]
  (require-execution-id!
   execution-id)
  (when (active? execution-id)
    (throw
     (ex-info
      "Browser choreography execution id is already active."
      {:error/type :gesso.live.runtime.choreo/duplicate-execution
       :execution-id execution-id})))
  (swap!
   execution-metadata
   assoc
   execution-id
   (merge
    {:started-at (now-ms)
     :updated-at (now-ms)}
    (or metadata {})))
  (try
    (let [result
          (machine/start
           plan
           (cond->
               {:execution-id execution-id
                :context (or context {})
                :handlers (handlers)}
             now-fn
             (assoc :now-fn now-fn)

             trace-limit
             (assoc :trace-limit trace-limit)

             max-immediate-steps
             (assoc
              :max-immediate-steps
              max-immediate-steps)))]
      (commit-machine-result!
       execution-id
       result))
    (catch :default error
      (cancel-all-timers!
       execution-id)
      (swap! executions
             dissoc
             execution-id)
      (swap! execution-metadata
             dissoc
             execution-id)
      (notify-error!
       :start
       execution-id
       error
       {:plan-name (:name plan)})
      (throw error))))

(defn resume!
  "Resume an active execution with a machine event/message envelope.

   Returns:
     {:status :resumed   :execution e}
     {:status :completed :execution e :result ...}
     {:status :ignored   :reason :inactive-execution}

   Late delivery is deliberately harmless. An inactive id cannot resurrect an
   execution because machine/resume is never called for it.

   An event that reaches an active execution but is not legal for its current
   await state remains a protocol/runtime error and is rethrown."
  [execution-id envelope]
  (require-execution-id!
   execution-id)
  (if-some [current
            (execution execution-id)]
    (try
      (let [result
            (machine/resume
             current
             envelope
             {:handlers (handlers)})
            committed
            (commit-machine-result!
             execution-id
             result)]
        (if (machine/completed?
             committed)
          {:status :completed
           :execution committed
           :result
           (machine/execution-result
            committed)}
          {:status :resumed
           :execution committed}))
      (catch :default error
        (notify-error!
         :resume
         execution-id
         error
         {:event envelope
          :state (:state current)})
        (throw error)))
    {:status :ignored
     :reason :inactive-execution
     :execution-id execution-id}))

(defn resume-event!
  "Resume execution with one modeled environmental event."
  ([execution-id event-id]
   (resume-event!
    execution-id
    event-id
    nil))
  ([execution-id event-id data]
   (resume!
    execution-id
    (machine/event
     event-id
     data))))

(defn resume-message!
  "Resume execution with one participant message.

   execution-id is explicit even when the payload also carries correlation
   identity. This prevents transport adapters from selecting an execution by a
   weak or ambiguous payload field."
  [execution-id descriptor payload]
  (resume!
   execution-id
   (machine/message
    descriptor
    payload)))

(defn accepts?
  "True when the active execution is currently waiting for envelope."
  [execution-id envelope]
  (boolean
   (when-some [execution
               (execution execution-id)]
     (machine/accepts-event?
      execution
      envelope))))

(defn accepts-event?
  [execution-id event-id]
  (accepts?
   execution-id
   (machine/event event-id)))

(defn accepts-message?
  [execution-id descriptor payload]
  (accepts?
   execution-id
   (machine/message
    descriptor
    payload)))

(defn abort!
  "Forget one browser execution without running protocol cleanup.

   This is intentionally a low-level process-lifecycle escape hatch, not normal
   optimistic recovery. Normal recoverable failures must be modeled as events so
   the choreography releases its resources and performs its effects.

   Appropriate uses are limited to cases where the browser object graph itself
   is being destroyed or framework teardown makes protocol cleanup impossible."
  [execution-id reason]
  (require-execution-id!
   execution-id)
  (if-some [current
            (execution execution-id)]
    (do
      (cancel-all-timers!
       execution-id)
      (swap! executions
             dissoc
             execution-id)
      (let [metadata
            (get @execution-metadata
                 execution-id)]
        (swap! execution-metadata
               dissoc
               execution-id)
        (swap!
         terminal-history
         (fn [history]
           (trim-terminal-history
            (conj
             history
             {:execution-id execution-id
              :plan-name (:plan-name current)
              :role (:role current)
              :status :aborted
              :reason reason
              :state (:state current)
              :held-resources
              (machine/held-resources
               current)
              :trace
              (machine/execution-trace
               current)
              :completed-at (now-ms)
              :metadata
              (select-keys
               metadata
               [:started-at
                :source
                :kind])})
            default-terminal-history-limit))))
      {:status :aborted
       :execution-id execution-id
       :reason reason})
    {:status :ignored
     :reason :inactive-execution
     :execution-id execution-id}))

;; -----------------------------------------------------------------------------
;; Runtime reset and diagnostics
;; -----------------------------------------------------------------------------

(defn reset-runtime!
  "Clear process-local choreography runtime state.

   Intended for hot reload, tests, or whole-runtime teardown. This aborts active
   executions; it is not a protocol recovery mechanism."
  []
  (doseq [execution-id
          (keys @executions)]
    (cancel-all-timers!
     execution-id))
  (reset! executions {})
  (reset! execution-metadata {})
  (reset! timers {})
  (reset! terminal-history [])
  true)

(defn initialize!
  "Initialize browser-generic choreography runtime handlers.

   Repeated calls are safe."
  []
  (install-default-handlers!)
  true)

(defn diagnostics
  "Return DOM-light runtime diagnostics suitable for window.gessoLive."
  []
  {:gesso.live.runtime/type runtime-type
   :active-count (execution-count)
   :active (active-summaries)
   :terminal (terminal-summaries)
   :registered-effects
   (registered-effects)
   :timer-count
   (reduce +
           0
           (map count
                (vals @timers)))})
