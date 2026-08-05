(ns gesso.live.optimistic-machine
  "Shared pure execution kernel for Gesso Live optimistic transitions.

   This namespace is compiled for both the JVM and the browser. It owns:

   - the state-function/effect-map execution model
   - ordered effect processing
   - execution traces and contextual errors
   - suspension at external-event boundaries
   - resumption of long-lived browser executions
   - the framework-owned default optimistic lifecycle

   It deliberately does not know about HTMX, the DOM, application commands,
   assignment policy, or authoritative rendering. Browser effects are supplied
   by gesso.live.runtime as handler functions.

   The execution model is closely based on com.biffweb.fx:

   - a state function receives context and returns a map or sequence of maps
   - vectors whose first item names a registered handler are effects
   - an effect result is associated with the map key that contained the effect
   - sequential result maps establish effect ordering
   - state output is accumulated into the long-lived execution context
   - ::next continues immediately
   - ::return completes

   Gesso adds ::await and ::resume so an execution can suspend until a later
   browser event. Accumulation is an intentional difference from Biff FX: a
   suspended browser execution must retain snapshots, locks, and other prior
   effect results across any number of external events."
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]))

;; -----------------------------------------------------------------------------
;; Machine protocol
;; -----------------------------------------------------------------------------

(def machine-type ::machine)

(def execution-type ::execution)

(def next-key ::next)

(def await-key ::await)

(def resume-key ::resume)

(def return-key ::return)

(def event-key ::event)

(def event-type-key ::event-type)

(def event-data-key ::event-data)

(def now-key ::now)

(def machine-name-key ::machine-name)

(def execution-id-key ::execution-id)

(def state-key ::state)

(def trace-key ::trace)

(def status-key ::status)

(def result-key ::result)

(def awaiting-key ::awaiting)

(def input-key ::input)

(def context-key ::context)

(def default-trace-limit 64)

(def execution-statuses #{:suspended :completed})

(def ^:private control-keys
  #{next-key await-key resume-key return-key})

;; -----------------------------------------------------------------------------
;; Default optimistic event protocol
;; -----------------------------------------------------------------------------

(def request-finished-event :gesso.live.optimistic.event/request-finished)

(def canonical-replaced-event :gesso.live.optimistic.event/canonical-replaced)

(def timeout-event :gesso.live.optimistic.event/timeout)

(def cancel-event :gesso.live.optimistic.event/cancel)

(def pending-events
  #{request-finished-event
    canonical-replaced-event
    timeout-event
    cancel-event})

(def successful-key ::successful?)

(def settlement-key ::settlement)

(def failure-reason-key ::failure-reason)

;; -----------------------------------------------------------------------------
;; Default optimistic browser effects
;; -----------------------------------------------------------------------------

(def prepare-effect :gesso.live.optimistic.fx/prepare)

(def acquire-lock-effect :gesso.live.optimistic.fx/acquire-lock)

(def capture-snapshot-effect :gesso.live.optimistic.fx/capture-snapshot)

(def capture-continuity-effect :gesso.live.optimistic.fx/capture-continuity)

(def mark-pending-effect :gesso.live.optimistic.fx/mark-pending)

(def install-projection-effect :gesso.live.optimistic.fx/install-projection)

(def restore-continuity-effect :gesso.live.optimistic.fx/restore-continuity)

(def schedule-timeout-effect :gesso.live.optimistic.fx/schedule-timeout)

(def observe-canonical-effect :gesso.live.optimistic.fx/observe-canonical)

(def apply-settlement-effect :gesso.live.optimistic.fx/apply-settlement)

(def recover-effect :gesso.live.optimistic.fx/recover)

(def cancel-timeout-effect :gesso.live.optimistic.fx/cancel-timeout)

(def clear-pending-effect :gesso.live.optimistic.fx/clear-pending)

(def release-lock-effect :gesso.live.optimistic.fx/release-lock)

;; -----------------------------------------------------------------------------
;; Internal result keys used by the default machine
;; -----------------------------------------------------------------------------

(def prepared-key ::prepared)

(def lock-key ::lock)

(def snapshot-key ::snapshot)

(def continuity-key ::continuity)

(def pending-key ::pending)

(def projection-key ::projection)

(def continuity-restored-key ::continuity-restored)

(def timeout-key ::timeout)

(def canonical-observation-key ::canonical-observation)

(def settlement-result-key ::settlement-result)

(def recovery-key ::recovery)

(def timeout-cancelled-key ::timeout-cancelled)

(def pending-cleared-key ::pending-cleared)

(def lock-released-key ::lock-released)

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn- ex
  [message data]
  (ex-info message data))

(defn- present?
  [x]
  (not (or (nil? x)
           (and (string? x)
                (str/blank? x)))))

(defn- truncate-str
  [s n]
  (if (<= (count s) n)
    s
    (str (subs s 0 (dec n)) "…")))

(defn- truncate
  [data]
  (walk/postwalk
   #(if (string? %)
      (truncate-str % 500)
      %)
   data))

(defn- throwable-message
  [e]
  #?(:clj  (.getMessage ^Throwable e)
     :cljs (or (.-message e)
               (str e))))

(defn- default-now
  []
  #?(:clj  (java.time.Instant/now)
     :cljs (js/Date.)))

(defn- require-map!
  [label value]
  (when-not (map? value)
    (throw
     (ex (str label " must be a map.")
         {:label label
          :value value
          :type (type value)})))
  value)

(defn- require-function!
  [label value]
  (when-not (ifn? value)
    (throw
     (ex (str label " must be callable.")
         {:label label
          :value value
          :type (type value)})))
  value)

(defn- require-state!
  [label value]
  (when-not (keyword? value)
    (throw
     (ex (str label " must be a keyword.")
         {:label label
          :value value})))
  value)

(defn- require-trace-limit!
  [value]
  (when-not (and (integer? value)
                 (pos? value))
    (throw
     (ex "Optimistic machine trace limit must be a positive integer."
         {:trace-limit value})))
  value)

(defn- append-trace
  [trace entry limit]
  (let [trace' (conj (vec trace) entry)
        excess (- (count trace') limit)]
    (if (pos? excess)
      (subvec trace' excess)
      trace')))

(defn- state-result?
  [x]
  (or (nil? x)
      (map? x)))

(defn- normalize-state-results
  [result]
  (cond
    (state-result? result)
    [result]

    (sequential? result)
    (let [results (vec result)]
      (when-not (every? state-result? results)
        (throw
         (ex "Optimistic state result sequences may contain only maps or nil."
             {:result result})))
      results)

    :else
    (throw
     (ex "Optimistic state functions must return a map, nil, or a sequence of maps/nil."
         {:result result
          :type (type result)}))))

(defn- normalize-awaiting
  [awaiting]
  (let [events (cond
                 (nil? awaiting)
                 (throw
                  (ex "::await must identify one or more non-nil event types."
                      {:await awaiting}))

                 (set? awaiting)
                 awaiting

                 (and (coll? awaiting)
                      (not (map? awaiting))
                      (not (string? awaiting)))
                 (set awaiting)

                 :else
                 #{awaiting})]
    (when (and events
               (or (empty? events)
                   (some nil? events)))
      (throw
       (ex "::await must identify one or more non-nil event types."
           {:await awaiting})))
    events))

(defn- public-output
  [output]
  (apply dissoc output control-keys))

(defn- accumulated-input
  [input output]
  (merge (dissoc input event-key)
         (public-output output)))

(defn event
  "Create an external event envelope accepted by `resume`.

   `type` is intentionally opaque to the generic machine runner. The default
   optimistic machine recognizes the event constants in this namespace."
  ([type]
   (event type nil))
  ([type data]
   (when-not (present? type)
     (throw
      (ex "Optimistic machine event type must be present."
          {:event-type type})))
   {event-type-key type
    event-data-key data}))

(defn event-type
  "Return the current resumed event type from a state context."
  [ctx]
  (get-in ctx [event-key event-type-key]))

(defn event-data
  "Return the current resumed event data from a state context."
  [ctx]
  (get-in ctx [event-key event-data-key]))

;; -----------------------------------------------------------------------------
;; Machine construction
;; -----------------------------------------------------------------------------

(defn machine
  "Create a prepared optimistic FX machine.

   `state->fn` must map keyword states to functions and must include :start.
   The returned value is data containing functions; it is shared source, not a
   value intended for transmission over the wire."
  [machine-name state->fn]
  (when-not (present? machine-name)
    (throw
     (ex "Optimistic machine name must be present."
         {:machine-name machine-name})))
  (require-map! "Optimistic machine state map" state->fn)
  (when-not (contains? state->fn :start)
    (throw
     (ex "Optimistic machine must define :start."
         {:machine-name machine-name
          :available-states (keys state->fn)})))
  (doseq [[state state-fn] state->fn]
    (require-state! "Optimistic machine state" state)
    (require-function! (str "State function for " state) state-fn))
  {::type machine-type
   machine-name-key machine-name
   ::state->fn state->fn})

(defn machine?
  [x]
  (and (map? x)
       (= machine-type (::type x))
       (map? (::state->fn x))))

(defn- require-machine!
  [machine']
  (when-not (machine? machine')
    (throw
     (ex "Expected a prepared optimistic machine."
         {:machine machine'})))
  machine')

(defn call-state
  "Invoke one state function directly without running effects or transitions.

   This mirrors the useful direct-state testing affordance from Biff FX."
  [machine' state ctx]
  (let [machine' (require-machine! machine')
        state-fn (get (::state->fn machine') state)]
    (when-not state-fn
      (throw
       (ex "Invalid optimistic machine state."
           {machine-name-key (machine-name-key machine')
            state-key state
            :available-states (keys (::state->fn machine'))})))
    (state-fn ctx)))

;; -----------------------------------------------------------------------------
;; Effect execution
;; -----------------------------------------------------------------------------

(defn- error!
  [{:keys [machine-name execution-id state trace]}
   message
   extra
   cause]
  (throw
   (ex-info
    message
    (truncate
     (merge
      {machine-name-key machine-name
       execution-id-key execution-id
       state-key state
       trace-key trace}
      extra))
    cause)))

(defn- step
  [{:keys [machine-name
           execution-id
           state->fn
           handlers
           context
           now-fn
           state
           input
           trace]}]
  (let [state-fn (or (get state->fn state)
                     (error!
                      {:machine-name machine-name
                       :execution-id execution-id
                       :state state
                       :trace trace}
                      "Invalid optimistic machine state."
                      {:available-states (keys state->fn)}
                      nil))
        injected {now-key (now-fn)
                  machine-name-key machine-name
                  execution-id-key execution-id
                  state-key state
                  trace-key trace}
        state-input (merge context input injected)
        result (try
                 (state-fn state-input)
                 (catch #?(:clj Exception :cljs :default) e
                   (error!
                    {:machine-name machine-name
                     :execution-id execution-id
                     :state state
                     :trace trace}
                    "Optimistic state function threw an exception."
                    {:injected injected
                     :exception-message (throwable-message e)}
                    e)))
        results (try
                  (normalize-state-results result)
                  (catch #?(:clj Exception :cljs :default) e
                    (error!
                     {:machine-name machine-name
                      :execution-id execution-id
                      :state state
                      :trace trace}
                     "Optimistic state function returned an invalid result."
                     {:result result
                      :exception-message (throwable-message e)}
                     e)))]
    (reduce
     (fn [output result-map]
       (if (nil? result-map)
         output
         (let [effect-keys
               (filterv
                (fn [k]
                  (let [value (get result-map k)]
                    (and (not (contains? control-keys k))
                         (vector? value)
                         (contains? handlers (first value)))))
                (keys result-map))

               ordinary-output
               (apply dissoc result-map effect-keys)

               output'
               (merge output ordinary-output)

               ;; Effects in the same map see the complete state input and all
               ;; output accumulated before this map, but not sibling effect
               ;; results. Use sequential result maps when ordering matters.
               handler-context
               (merge state-input output')]
           (into
            output'
            (map
             (fn [k]
               (let [[handler-key & args] (get result-map k)
                     handler (get handlers handler-key)]
                 [k
                  (try
                    (apply handler handler-context args)
                    (catch #?(:clj Exception :cljs :default) e
                      (error!
                       {:machine-name machine-name
                        :execution-id execution-id
                        :state state
                        :trace trace}
                       "Optimistic effect handler threw an exception."
                       {:handler handler-key
                        :handler-args args
                        :output output'
                        :exception-message (throwable-message e)}
                       e)))]))
             effect-keys))))
     {}
     results))))

;; -----------------------------------------------------------------------------
;; Execution records
;; -----------------------------------------------------------------------------

(defn execution?
  [x]
  (and (map? x)
       (= execution-type (::type x))
       (contains? execution-statuses (status-key x))))

(defn suspended?
  [x]
  (and (execution? x)
       (= :suspended (status-key x))))

(defn completed?
  [x]
  (and (execution? x)
       (= :completed (status-key x))))

(defn execution-result
  "Return a completed execution's result, or nil for a suspended execution."
  [execution]
  (when (completed? execution)
    (result-key execution)))

(defn accepts-event?
  "Return true when a suspended execution is waiting for `event-or-type`."
  [execution event-or-type]
  (let [type (if (map? event-or-type)
               (event-type-key event-or-type)
               event-or-type)]
    (and (suspended? execution)
         (contains? (awaiting-key execution) type))))

(defn- execution-record
  [{:keys [machine-name
           execution-id
           status
           state
           awaiting
           context
           input
           trace
           trace-limit
           result]}]
  (cond->
   {::type execution-type
    machine-name-key machine-name
    execution-id-key execution-id
    status-key status
    state-key state
    context-key context
    input-key input
    trace-key trace
    ::trace-limit trace-limit}
    awaiting
    (assoc awaiting-key awaiting)

    (= :completed status)
    (assoc result-key result)))

(defn- validate-control-flow!
  [output machine-name execution-id state trace]
  (let [next? (contains? output next-key)
        await? (contains? output await-key)
        resume? (contains? output resume-key)
        return? (contains? output return-key)]
    (when (and next? return?)
      (error!
       {:machine-name machine-name
        :execution-id execution-id
        :state state
        :trace trace}
       "An optimistic state cannot set ::next and ::return together."
       {:output output}
       nil))
    (when (and await? (or next? return?))
      (error!
       {:machine-name machine-name
        :execution-id execution-id
        :state state
        :trace trace}
       "An optimistic state cannot combine ::await with ::next or ::return."
       {:output output}
       nil))
    (when (not= await? resume?)
      (error!
       {:machine-name machine-name
        :execution-id execution-id
        :state state
        :trace trace}
       "An optimistic state must set ::await and ::resume together."
       {:output output}
       nil))))

(defn- run-loop
  [machine'
   {:keys [execution-id
           context
           handlers
           now-fn
           trace-limit
           state
           input
           trace]}]
  (let [machine' (require-machine! machine')
        machine-name (machine-name-key machine')
        state->fn (::state->fn machine')
        handlers (require-map! "Optimistic effect handlers" (or handlers {}))
        now-fn (require-function! "Optimistic now function"
                                  (or now-fn default-now))
        trace-limit (require-trace-limit!
                     (or trace-limit default-trace-limit))]
    (loop [state state
           input input
           trace (vec trace)]
      (let [output
            (step
             {:machine-name machine-name
              :execution-id execution-id
              :state->fn state->fn
              :handlers handlers
              :context context
              :now-fn now-fn
              :state state
              :input input
              :trace trace})

            trace'
            (append-trace
             trace
             {state-key state
              ::output output}
             trace-limit)

            _
            (validate-control-flow!
             output machine-name execution-id state trace')

            next-state
            (get output next-key)

            awaiting
            (when (contains? output await-key)
              (normalize-awaiting (get output await-key)))

            resume-state
            (get output resume-key)

            data-output
            (accumulated-input input output)]
        (cond
          (contains? output next-key)
          (do
            (require-state! "::next" next-state)
            (recur next-state data-output trace'))

          awaiting
          (do
            (require-state! "::resume" resume-state)
            (execution-record
             {:machine-name machine-name
              :execution-id execution-id
              :status :suspended
              :state resume-state
              :awaiting awaiting
              :context context
              :input data-output
              :trace trace'
              :trace-limit trace-limit}))

          (contains? output return-key)
          (execution-record
           {:machine-name machine-name
            :execution-id execution-id
            :status :completed
            :state state
            :context context
            :input data-output
            :trace trace'
            :trace-limit trace-limit
            :result (get output return-key)})

          :else
          (execution-record
           {:machine-name machine-name
            :execution-id execution-id
            :status :completed
            :state state
            :context context
            :input data-output
            :trace trace'
            :trace-limit trace-limit
            :result data-output}))))))

(defn start
  "Start `machine'` at :start and run until completion or suspension.

   Options:

     :execution-id
       Browser-generated correlation identity. The generic runner permits nil
       for isolated tests, though the default browser runtime always supplies
       one.

     :context
       Stable base context merged into every state input.

     :handlers
       Effect-handler map. Handler functions receive current state input as
       their first argument followed by the effect vector's arguments.

     :now-fn
       Optional zero-argument clock for deterministic tests.

     :trace-limit
       Maximum retained state trace entries. Defaults to 64."
  [machine' {:keys [execution-id context handlers now-fn trace-limit]}]
  (run-loop
   machine'
   {:execution-id execution-id
    :context (require-map! "Optimistic execution context" (or context {}))
    :handlers handlers
    :now-fn now-fn
    :trace-limit trace-limit
    :state :start
    :input {}
    :trace []}))

(defn resume
  "Resume a suspended execution with an external event.

   Unexpected events are rejected rather than silently ignored. Call
   `accepts-event?` before broadcasting an event to multiple executions."
  [machine' execution event' {:keys [handlers now-fn]}]
  (require-machine! machine')
  (when-not (suspended? execution)
    (throw
     (ex "Only suspended optimistic executions can be resumed."
         {:execution execution})))
  (let [event' (if (and (map? event')
                        (contains? event' event-type-key))
                 event'
                 (throw
                  (ex "Optimistic resume requires an event envelope from `event`."
                      {:event event'})))
        expected-machine (machine-name-key execution)
        actual-machine (machine-name-key machine')]
    (when-not (= expected-machine actual-machine)
      (throw
       (ex "Suspended execution belongs to a different optimistic machine."
           {:execution-machine expected-machine
            :provided-machine actual-machine
            execution-id-key (execution-id-key execution)})))
    (when-not (accepts-event? execution event')
      (throw
       (ex "Optimistic execution is not waiting for this event."
           {execution-id-key (execution-id-key execution)
            :event-type (event-type-key event')
            :awaiting (awaiting-key execution)})))
    (run-loop
     machine'
     {:execution-id (execution-id-key execution)
      :context (context-key execution)
      :handlers handlers
      :now-fn now-fn
      :trace-limit (::trace-limit execution)
      :state (state-key execution)
      :input (assoc (input-key execution)
                    event-key
                    event')
      :trace (trace-key execution)})))

;; -----------------------------------------------------------------------------
;; Default optimistic lifecycle
;; -----------------------------------------------------------------------------

(def default-machine-name :gesso.live.optimistic/default)

(defn- lock-acquired?
  [lock-result]
  (cond
    (true? lock-result)
    true

    (false? lock-result)
    false

    (map? lock-result)
    (true? (:acquired? lock-result))

    :else
    false))

(defn- completion
  [status & {:as data}]
  (merge {:status status} data))

(defn- semantic-settlement-results
  [settlement]
  [{settlement-result-key
    [apply-settlement-effect settlement]}

   {timeout-cancelled-key
    [cancel-timeout-effect]}

   {pending-cleared-key
    [clear-pending-effect]}

   {lock-released-key
    [release-lock-effect]

    return-key
    (completion
     :settled
     :outcome (:outcome settlement)
     :command-applied? (:command-applied? settlement)
     :settlement settlement)}])

(defn- recovery-results
  [reason event-data']
  [{recovery-key
    [recover-effect
     {:reason reason
      :event-data event-data'}]}

   {continuity-restored-key
    [restore-continuity-effect]}

   {timeout-cancelled-key
    [cancel-timeout-effect]}

   {pending-cleared-key
    [clear-pending-effect]}

   {lock-released-key
    [release-lock-effect]

    return-key
    (completion
     :failed
     :reason reason)}])

(defn- pending-state
  [ctx]
  (let [type (event-type ctx)
        data (event-data ctx)]
    (case type
      :gesso.live.optimistic.event/canonical-replaced
      [{canonical-observation-key
        [observe-canonical-effect data]}

       {await-key pending-events
        resume-key :pending}]

      :gesso.live.optimistic.event/request-finished
      (let [successful? (true? (get data successful-key))
            settlement (get data settlement-key)]
        (cond
          (not successful?)
          (recovery-results
           (or (get data failure-reason-key)
               :request-failed)
           data)

          (map? settlement)
          (semantic-settlement-results settlement)

          :else
          (recovery-results :missing-settlement data)))

      :gesso.live.optimistic.event/timeout
      (recovery-results :timeout data)

      :gesso.live.optimistic.event/cancel
      (recovery-results
       (or (get data failure-reason-key)
           :cancelled)
       data)

      (throw
       (ex "Default optimistic machine received an unsupported event."
           {:event-type type
            :event-data data
            :supported-events pending-events})))))

(def default-machine
  "Framework-owned optimistic browser lifecycle.

   Browser handlers determine how each effect touches the DOM and HTMX. This
   machine determines ordering and settlement behavior:

   1. prepare and acquire a target lock
   2. capture rollback/continuity state
   3. install the application-rendered projection
   4. suspend for request/canonical/timeout/cancel events
   5. install explicit semantic settlement, or recover only when no canonical
      settlement was obtained
   6. clear pending state and release the target lock

   Domain policy remains entirely outside this machine."
  (machine
   default-machine-name
   {:start
    (fn [_ctx]
      [{prepared-key
        [prepare-effect]}

       {lock-key
        [acquire-lock-effect]

        next-key
        :after-lock}])

    :after-lock
    (fn [ctx]
      (if (lock-acquired? (get ctx lock-key))
        [{snapshot-key
          [capture-snapshot-effect]}

         {continuity-key
          [capture-continuity-effect]}

         {pending-key
          [mark-pending-effect]}

         {projection-key
          [install-projection-effect]}

         {continuity-restored-key
          [restore-continuity-effect]}

         {timeout-key
          [schedule-timeout-effect]

          await-key
          pending-events

          resume-key
          :pending}]

        {return-key
         (completion :ignored :reason :target-locked)}))

    :pending
    pending-state}))
