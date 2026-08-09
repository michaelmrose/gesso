(ns gesso.live.choreo.machine
  "Generic FX-style execution kernel for projected Gesso Live choreographies.

   This namespace is runtime code shared by JVM Clojure and ClojureScript. It
   executes one role-local projected plan until the plan either returns or
   reaches an await state, at which point execution is suspended and can later
   be resumed with a correlated message or environmental event.

   The model is intentionally close to Biff FX:
   - effects are data identified by keywords
   - registered handlers perform effects
   - handlers receive the current accumulated context
   - handler results are accumulated into that context
   - effect ordering follows explicit choreography state ordering
   - execution may suspend and later resume without losing accumulated context

   The kernel deliberately does not know about:
   - optimistic policy
   - DOM or continuity
   - HTMX or SSE
   - Ring or XTDB
   - choreography verification or endpoint projection

   In particular, this namespace does not require the verifier or projector, so
   compiler machinery need not become reachable from the production CLJS
   bundle."
  (:require
   [clojure.set :as set]))

;; -----------------------------------------------------------------------------
;; Runtime identity and injected context
;; -----------------------------------------------------------------------------

(def execution-type
  :gesso.live.choreo.machine/execution)

(def send-effect
  "Handler key used for projected participant sends."
  :gesso.live.choreo.fx/send)

(def execution-id-key
  :gesso.live.choreo/execution-id)

(def role-key
  :gesso.live.choreo/role)

(def plan-name-key
  :gesso.live.choreo/plan-name)

(def state-key
  :gesso.live.choreo/state)

(def now-key
  :gesso.live.choreo/now)

(def event-key
  :gesso.live.choreo/event)

(def default-trace-limit 64)

(def default-max-immediate-steps 1024)

(def execution-statuses
  #{:suspended :completed})

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn- ex
  [message data]
  (ex-info message data))

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
          :value value})))
  value)

(defn- require-function!
  [label value]
  (when-not (ifn? value)
    (throw
     (ex (str label " must be callable.")
         {:label label
          :value value})))
  value)

(defn- require-positive-integer!
  [label value]
  (when-not (and (integer? value)
                 (pos? value))
    (throw
     (ex (str label " must be a positive integer.")
         {:label label
          :value value})))
  value)

(defn- append-trace
  [trace entry limit]
  (let [trace' (conj (vec trace) entry)
        excess (- (count trace') limit)]
    (if (pos? excess)
      (subvec trace' excess)
      trace')))

(defn- state-at
  [plan state-id]
  (get (:states plan) state-id))

(defn- resource-descriptor
  [plan resource-id]
  (get (:resources plan) resource-id))

(defn- linear-resource?
  [descriptor]
  (not= false (:linear? descriptor)))

(defn- terminal-release?
  [descriptor]
  (if (contains? descriptor :terminal-release?)
    (true? (:terminal-release? descriptor))
    (linear-resource? descriptor)))

;; -----------------------------------------------------------------------------
;; Projected-plan runtime boundary
;; -----------------------------------------------------------------------------

(defn plan?
  "Return true when x has the minimal shape required by the runtime machine.

   Deliberately does not depend on gesso.live.choreo.project. The compiler owns
   full projected-plan validation; the runtime checks only the trusted boundary
   it actually consumes."
  [x]
  (and (map? x)
       (keyword? (:role x))
       (map? (:states x))
       (contains? (:states x) (:initial x))
       (map? (:resources x))))

(defn- require-plan!
  [plan]
  (when-not (plan? plan)
    (throw
     (ex "Expected a projected Gesso Live choreography plan."
         {:plan plan})))
  plan)

(defn- require-state!
  [plan state-id]
  (or (state-at plan state-id)
      (throw
       (ex "Projected choreography references an unknown state."
           {:role (:role plan)
            :state state-id}))))

;; -----------------------------------------------------------------------------
;; External event envelopes
;; -----------------------------------------------------------------------------

(defn event
  "Create an environmental event envelope accepted by resume.

   Environmental events are events such as timeout, request failure, or
   disconnection. Participant-to-participant communication should use message."
  ([event-id]
   (event event-id nil))
  ([event-id data]
   (when-not (keyword? event-id)
     (throw
      (ex "Choreography event id must be a keyword."
          {:event event-id})))
   {:kind :event
    :event event-id
    :data data}))

(defn message
  "Create one participant message envelope accepted by resume.

   Required descriptor keys:
     :from, :to, :event

   Optional:
     :via

   payload must be a map because projected communication contracts reason about
   named required, optional, correlation, and remote-choice fields."
  [{:keys [from to event via] :as descriptor} payload]
  (when-not (keyword? from)
    (throw
     (ex "Choreography message :from must be a keyword."
         {:descriptor descriptor})))
  (when-not (keyword? to)
    (throw
     (ex "Choreography message :to must be a keyword."
         {:descriptor descriptor})))
  (when-not (keyword? event)
    (throw
     (ex "Choreography message :event must be a keyword."
         {:descriptor descriptor})))
  (when (and (some? via)
             (not (keyword? via)))
    (throw
     (ex "Choreography message :via must be a keyword when present."
         {:descriptor descriptor})))
  (require-map! "Choreography message payload" payload)
  (cond-> {:kind :message
           :from from
           :to to
           :event event
           :payload payload}
    (some? via)
    (assoc :via via)))

(defn event-envelope?
  [x]
  (and (map? x)
       (case (:kind x)
         :event
         (keyword? (:event x))

         :message
         (and (keyword? (:from x))
              (keyword? (:to x))
              (keyword? (:event x))
              (map? (:payload x)))

         false)))

;; -----------------------------------------------------------------------------
;; Execution records
;; -----------------------------------------------------------------------------

(defn execution?
  [x]
  (and (map? x)
       (= execution-type (:gesso.live.choreo.machine/type x))
       (contains? execution-statuses (:status x))))

(defn suspended?
  [x]
  (and (execution? x)
       (= :suspended (:status x))))

(defn completed?
  [x]
  (and (execution? x)
       (= :completed (:status x))))

(defn execution-context
  "Return the effective accumulated execution context.

   This is intentionally public. Long-lived choreography executions accumulate
   effect results and received data over time; callers must not accidentally
   inspect only the original start context."
  [execution]
  (when-not (execution? execution)
    (throw
     (ex "Expected a choreography execution."
         {:execution execution})))
  (:context execution))

(defn execution-result
  "Return a completed execution's terminal result, or nil while suspended."
  [execution]
  (when (completed? execution)
    (:result execution)))

(defn execution-trace
  "Return the retained execution trace."
  [execution]
  (when-not (execution? execution)
    (throw
     (ex "Expected a choreography execution."
         {:execution execution})))
  (:trace execution))

(defn held-resources
  "Return resource-id -> acquisition count for the execution."
  [execution]
  (when-not (execution? execution)
    (throw
     (ex "Expected a choreography execution."
         {:execution execution})))
  (:resources execution))

(defn- execution-record
  [{:keys [plan
           execution-id
           status
           state
           context
           resources
           awaiting
           trace
           trace-limit
           max-immediate-steps
           result]}]
  (cond->
   {:gesso.live.choreo.machine/type execution-type
    :plan plan
    :plan-name (:name plan)
    :role (:role plan)
    :execution-id execution-id
    :status status
    :state state
    :context context
    :resources resources
    :trace trace
    :trace-limit trace-limit
    :max-immediate-steps max-immediate-steps}
    awaiting
    (assoc :awaiting awaiting)

    (= :completed status)
    (assoc :result result)))

;; -----------------------------------------------------------------------------
;; Handler execution and accumulation
;; -----------------------------------------------------------------------------

(defn- handler-context
  [{:keys [plan execution-id state context trace now-fn current-event]}]
  (cond->
   (merge
    context
    {execution-id-key execution-id
     role-key (:role plan)
     plan-name-key (:name plan)
     state-key state
     now-key (now-fn)
     :gesso.live.choreo/trace trace})
    current-event
    (assoc event-key current-event)))

(defn- merge-handler-result
  [context result handler-key state-id]
  (cond
    (nil? result)
    context

    (map? result)
    (merge context result)

    :else
    (throw
     (ex "Choreography effect handlers must return a map or nil."
         {:state state-id
          :handler handler-key
          :result result}))))

(defn- invoke-handler
  [{:keys [handlers] :as env} handler-key args]
  (let [handler (get handlers handler-key)]
    (when-not handler
      (throw
       (ex "No choreography effect handler is registered."
           {:state (:state env)
            :handler handler-key
            :available-handlers (set (keys handlers))})))
    (require-function! "Choreography effect handler" handler)
    (try
      (handler (handler-context env) args)
      (catch #?(:clj Exception :cljs :default) e
        (throw
         (ex-info
          "Choreography effect handler threw an exception."
          {:state (:state env)
           :handler handler-key
           :args args
           :execution-id (:execution-id env)
           :role (get-in env [:plan :role])
           :exception-message (throwable-message e)}
          e))))))

(defn- outgoing-payload
  [context state]
  (let [required (:required state #{})
        optional (:optional state #{})
        missing (set (remove #(contains? context %) required))]
    (when (seq missing)
      (throw
       (ex "Projected send is missing required values in execution context."
           {:state-id (:state-id state)
            :event (:event state)
            :missing missing
            :required required})))
    (select-keys context
                 (set/union required optional))))

(defn- send-descriptor
  [plan state context]
  (cond-> {:from (:role plan)
           :to (:to state)
           :event (:event state)
           :required (:required state #{})
           :optional (:optional state #{})
           :correlation (:correlation state #{})
           :payload (outgoing-payload context state)}
    (contains? state :via)
    (assoc :via (:via state))))

;; -----------------------------------------------------------------------------
;; Resource accounting
;; -----------------------------------------------------------------------------

(defn- acquire-resource
  [plan resources state-id resource-id]
  (let [descriptor (or (resource-descriptor plan resource-id)
                       (throw
                        (ex "Projected acquire references an unknown resource."
                            {:state state-id
                             :resource resource-id})))
        held (get resources resource-id 0)]
    (when (and (linear-resource? descriptor)
               (pos? held))
      (throw
       (ex "Linear choreography resource is already held."
           {:state state-id
            :resource resource-id
            :held held})))
    (assoc resources resource-id (inc held))))

(defn- release-resource
  [plan resources state-id resource-id]
  (let [_descriptor (or (resource-descriptor plan resource-id)
                        (throw
                         (ex "Projected release references an unknown resource."
                             {:state state-id
                              :resource resource-id})))
        held (get resources resource-id 0)]
    (when-not (pos? held)
      (throw
       (ex "Choreography resource is released when it is not held."
           {:state state-id
            :resource resource-id
            :held held})))
    (let [remaining (dec held)]
      (if (zero? remaining)
        (dissoc resources resource-id)
        (assoc resources resource-id remaining)))))

(defn- assert-terminal-resources!
  [plan resources state-id]
  (let [leaked
        (into {}
              (filter
               (fn [[resource-id count]]
                 (and (pos? count)
                      (terminal-release?
                       (resource-descriptor
                        plan
                        resource-id)))))
              resources)]
    (when (seq leaked)
      (throw
       (ex "Choreography reached a terminal state while required resources are still held."
           {:state state-id
            :resources leaked}))))
  nil)

;; -----------------------------------------------------------------------------
;; Await matching
;; -----------------------------------------------------------------------------

(defn- message-identity-matches?
  [alternative envelope]
  (and (= (:from alternative)
          (:from envelope))
       (= (:to alternative)
          (:to envelope))
       (= (:event alternative)
          (:event envelope))
       (= (:via alternative)
          (:via envelope))))

(defn- required-payload-present?
  [alternative payload]
  (every? #(contains? payload %)
          (:required alternative #{})))

(defn- choice-match?
  [alternative payload]
  (every?
   (fn [[k expected]]
     (and (contains? payload k)
          (= expected (get payload k))))
   (:match alternative {})))

(defn- correlation-match?
  [alternative context payload]
  (every?
   (fn [k]
     (and (contains? context k)
          (contains? payload k)
          (= (get context k)
             (get payload k))))
   (:correlation alternative #{})))

(defn- receive-matches?
  [alternative context envelope]
  (let [payload (:payload envelope)]
    (and (= :message (:kind envelope))
         (message-identity-matches?
          alternative
          envelope)
         (required-payload-present?
          alternative
          payload)
         (choice-match?
          alternative
          payload)
         (correlation-match?
          alternative
          context
          payload))))

(defn- matching-receives
  [state context envelope]
  (vec
   (filter
    #(receive-matches? % context envelope)
    (:receives state []))))

(defn- resolve-await
  [state context envelope]
  (case (:kind envelope)
    :event
    (when-some [target
                (get (:events state {})
                     (:event envelope))]
      {:next target
       :context
       (cond-> context
         (:bind state)
         (assoc (:bind state)
                (:data envelope)))})

    :message
    (let [matches
          (matching-receives
           state
           context
           envelope)]
      (cond
        (empty? matches)
        nil

        (> (count matches) 1)
        (throw
         (ex "Incoming choreography message matches multiple receive alternatives."
             {:state (:state-id state)
              :message (dissoc envelope :payload)
              :matches matches}))

        :else
        (let [alternative (first matches)
              payload (:payload envelope)]
          {:next (:next alternative)
           :context
           (cond-> (merge context payload)
             (:bind alternative)
             (assoc (:bind alternative)
                    payload))})))

    nil))

(defn accepts-event?
  "Return true when suspended execution can consume envelope.

   This performs matching only; it does not mutate or resume execution."
  [execution envelope]
  (boolean
   (and
    (suspended? execution)
    (event-envelope? envelope)
    (let [state
          (state-at (:plan execution)
                    (:state execution))]
      (and state
           (= :await (:op state))
           (resolve-await
            (assoc state :state-id (:state execution))
            (:context execution)
            envelope))))))

;; -----------------------------------------------------------------------------
;; Trace helpers
;; -----------------------------------------------------------------------------

(defn- trace-entry
  [state-id state now detail]
  (merge
   {:state state-id
    :op (:op state)
    :at now}
   detail))

(defn- trace-detail
  [state context]
  (case (:op state)
    :effect
    {:effect (:effect state)}

    :send
    {:event (:event state)
     :to (:to state)
     :via (:via state)}

    :choice
    {:choice-key (:key state)
     :choice-value (get context (:key state))}

    :await
    {:events (set (keys (:events state {})))
     :receive-events
     (set (map :event
               (:receives state [])))}

    :acquire
    {:resource (:resource state)}

    :release
    {:resource (:resource state)}

    :return
    {:outcome (:outcome state)}

    {}))

;; -----------------------------------------------------------------------------
;; Execution loop
;; -----------------------------------------------------------------------------

(defn- run-loop
  [plan
   {:keys [execution-id
           context
           resources
           handlers
           now-fn
           trace-limit
           max-immediate-steps
           state
           trace
           resume-event]}]
  (require-plan! plan)
  (let [handlers (require-map!
                  "Choreography effect handlers"
                  (or handlers {}))
        now-fn (require-function!
                "Choreography clock"
                (or now-fn default-now))
        trace-limit
        (require-positive-integer!
         "Choreography trace limit"
         (or trace-limit default-trace-limit))
        max-immediate-steps
        (require-positive-integer!
         "Choreography max immediate steps"
         (or max-immediate-steps
             default-max-immediate-steps))]
    (loop [state-id state
           context context
           resources resources
           trace (vec trace)
           current-event resume-event
           immediate-steps 0]
      (when (>= immediate-steps
                max-immediate-steps)
        (throw
         (ex "Choreography exceeded the maximum number of immediate steps without suspending or returning."
             {:role (:role plan)
              :execution-id execution-id
              :state state-id
              :max-immediate-steps max-immediate-steps})))
      (let [state (assoc (require-state! plan state-id)
                         :state-id state-id)
            now (now-fn)
            trace'
            (append-trace
             trace
             (trace-entry
              state-id
              state
              now
              (trace-detail state context))
             trace-limit)
            env {:plan plan
                 :execution-id execution-id
                 :state state-id
                 :context context
                 :trace trace'
                 :current-event current-event
                 :handlers handlers
                 :now-fn now-fn}]
        (case (:op state)
          :effect
          (let [result
                (invoke-handler
                 env
                 (:effect state)
                 (:args state))
                context'
                (merge-handler-result
                 context
                 result
                 (:effect state)
                 state-id)]
            (recur (:next state)
                   context'
                   resources
                   trace'
                   nil
                   (inc immediate-steps)))

          :send
          (let [descriptor
                (send-descriptor
                 plan
                 state
                 context)
                result
                (invoke-handler
                 env
                 send-effect
                 descriptor)
                context'
                (merge-handler-result
                 context
                 result
                 send-effect
                 state-id)]
            (recur (:next state)
                   context'
                   resources
                   trace'
                   nil
                   (inc immediate-steps)))

          :choice
          (let [choice-key (:key state)]
            (when-not (contains? context choice-key)
              (throw
               (ex "Projected choreography choice value is absent from execution context."
                   {:state state-id
                    :choice-key choice-key
                    :available-keys
                    (set (keys context))})))
            (let [choice-value (get context choice-key)
                  target
                  (get (:branches state)
                       choice-value
                       ::not-found)]
              (when (= ::not-found target)
                (throw
                 (ex "Projected choreography has no branch for the current choice value."
                     {:state state-id
                      :choice-key choice-key
                      :choice-value choice-value
                      :branches
                      (set (keys (:branches state)))})))
              (recur target
                     context
                     resources
                     trace'
                     nil
                     (inc immediate-steps))))

          :await
          (execution-record
           {:plan plan
            :execution-id execution-id
            :status :suspended
            :state state-id
            :context context
            :resources resources
            :awaiting
            {:events
             (set (keys (:events state {})))
             :receives
             (mapv
              #(select-keys
                %
                [:from
                 :to
                 :event
                 :via
                 :required
                 :correlation
                 :match])
              (:receives state []))}
            :trace trace'
            :trace-limit trace-limit
            :max-immediate-steps
            max-immediate-steps})

          :acquire
          (recur (:next state)
                 context
                 (acquire-resource
                  plan
                  resources
                  state-id
                  (:resource state))
                 trace'
                 nil
                 (inc immediate-steps))

          :release
          (recur (:next state)
                 context
                 (release-resource
                  plan
                  resources
                  state-id
                  (:resource state))
                 trace'
                 nil
                 (inc immediate-steps))

          :return
          (do
            (assert-terminal-resources!
             plan
             resources
             state-id)
            (execution-record
             {:plan plan
              :execution-id execution-id
              :status :completed
              :state state-id
              :context context
              :resources resources
              :trace trace'
              :trace-limit trace-limit
              :max-immediate-steps
              max-immediate-steps
              :result
              (if-some [value-key
                        (:value-key state)]
                (get context value-key)
                {:outcome (:outcome state)})}))

          (throw
           (ex "Projected choreography contains an unsupported runtime operation."
               {:state state-id
                :op (:op state)
                :role (:role plan)})))))))

(defn start
  "Start a projected choreography plan and run until suspension or completion.

   Options:

     :execution-id
       Correlation identity. May be nil for isolated tests; real distributed
       executions should provide one.

     :context
       Initial accumulated FX context.

     :handlers
       Effect id -> handler. Handlers receive [context args] and return a map of
       context additions or nil. Projected sends use the reserved send-effect
       handler and receive a complete send descriptor as args.

     :now-fn
       Optional zero-argument clock for deterministic tests.

     :trace-limit
       Retained trace entries. Defaults to 64.

     :max-immediate-steps
       Guard against a projected cycle that neither awaits nor returns. Defaults
       to 1024."
  [plan {:keys [execution-id
                context
                handlers
                now-fn
                trace-limit
                max-immediate-steps]}]
  (require-plan! plan)
  (run-loop
   plan
   {:execution-id execution-id
    :context
    (require-map!
     "Choreography execution context"
     (or context {}))
    :resources {}
    :handlers handlers
    :now-fn now-fn
    :trace-limit trace-limit
    :max-immediate-steps max-immediate-steps
    :state (:initial plan)
    :trace []
    :resume-event nil}))

(defn resume
  "Resume a suspended execution with one environmental event or participant
   message.

   Unexpected events/messages are rejected rather than silently consumed. Late
   delivery to a completed execution is therefore a caller-level no-op decision,
   not something that can resurrect the execution."
  [execution envelope {:keys [handlers now-fn]}]
  (when-not (suspended? execution)
    (throw
     (ex "Only suspended choreography executions can be resumed."
         {:execution execution})))
  (when-not (event-envelope? envelope)
    (throw
     (ex "Choreography resume requires an event or message envelope."
         {:event envelope})))
  (let [plan (:plan execution)
        state-id (:state execution)
        state
        (assoc (require-state! plan state-id)
               :state-id state-id)
        resolution
        (resolve-await
         state
         (:context execution)
         envelope)]
    (when-not resolution
      (throw
       (ex "Suspended choreography execution is not waiting for this event or message."
           {:execution-id (:execution-id execution)
            :role (:role execution)
            :state state-id
            :event envelope
            :awaiting (:awaiting execution)})))
    (run-loop
     plan
     {:execution-id (:execution-id execution)
      :context (:context resolution)
      :resources (:resources execution)
      :handlers handlers
      :now-fn now-fn
      :trace-limit (:trace-limit execution)
      :max-immediate-steps
      (:max-immediate-steps execution)
      :state (:next resolution)
      :trace (:trace execution)
      :resume-event envelope})))
