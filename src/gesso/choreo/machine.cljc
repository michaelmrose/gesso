(ns gesso.choreo.machine
  "Portable execution kernel for projected Gesso choreographies.

   This namespace advances one role-local projected plan. It owns protocol
   state, choices, waits, message matching, correlation, linear resources, and
   terminality. It deliberately does not execute FX or perform transport.

   Local FX and sends are explicit execution boundaries:

     :waiting-fx    endpoint must run the named local FX machine
     :waiting-send  endpoint must deliver the prepared participant message
     :suspended     endpoint is waiting for an incoming/environment event
     :completed     endpoint has reached a terminal return

   The endpoint adapter performs the external operation and then calls
   complete-fx, complete-send, or resume. This keeps the choreography machine
   portable while allowing JVM endpoints to use Biff FX and browser endpoints
   to use Gesso's small compatible CLJS FX runner.

   This namespace deliberately does not know about verification, projection,
   Biff, DOM, HTMX, SSE, Ring, XTDB, optimism, or continuity."
  (:require
   [clojure.set :as set]))

;; -----------------------------------------------------------------------------
;; Runtime identity
;; -----------------------------------------------------------------------------

(def execution-type
  :gesso.choreo.machine/execution)

(def execution-statuses
  #{:waiting-fx
    :waiting-send
    :suspended
    :completed})

(def default-trace-limit 64)
(def default-max-immediate-steps 1024)

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn- ex
  [message data]
  (ex-info message data))

(defn- require-map!
  [label value]
  (when-not (map? value)
    (throw
     (ex (str label " must be a map.")
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

(defn- default-now
  []
  #?(:clj  (java.time.Instant/now)
     :cljs (js/Date.)))

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
  "True when x has the minimal shape consumed by this runtime.

   Full projected-plan validation belongs to the compiler. Keeping this check
   small prevents verifier/projector code from becoming a runtime dependency."
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
     (ex "Expected a projected Gesso choreography plan."
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
  "Create an environmental-event envelope accepted by resume."
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
  "Create a participant-message envelope accepted by resume.

   descriptor requires :from, :to, and :event and may contain :via. payload is
   a map because projected receive contracts reason about named fields."
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
       (= execution-type
          (:gesso.choreo.machine/type x))
       (contains? execution-statuses (:status x))))

(defn waiting-fx?
  [x]
  (and (execution? x)
       (= :waiting-fx (:status x))))

(defn waiting-send?
  [x]
  (and (execution? x)
       (= :waiting-send (:status x))))

(defn suspended?
  [x]
  (and (execution? x)
       (= :suspended (:status x))))

(defn completed?
  [x]
  (and (execution? x)
       (= :completed (:status x))))

(defn execution-context
  "Return the accumulated protocol context."
  [execution]
  (when-not (execution? execution)
    (throw
     (ex "Expected a choreography execution."
         {:execution execution})))
  (:context execution))

(defn execution-result
  "Return terminal result when completed, otherwise nil."
  [execution]
  (when (completed? execution)
    (:result execution)))

(defn execution-trace
  "Return retained protocol trace entries."
  [execution]
  (when-not (execution? execution)
    (throw
     (ex "Expected a choreography execution."
         {:execution execution})))
  (:trace execution))

(defn held-resources
  "Return resource-id -> acquisition count for this execution."
  [execution]
  (when-not (execution? execution)
    (throw
     (ex "Expected a choreography execution."
         {:execution execution})))
  (:resources execution))

(defn pending-action
  "Return the external action currently required by the endpoint adapter.

   Returns an :fx or :send descriptor while waiting on one of those boundaries,
   otherwise nil."
  [execution]
  (when-not (execution? execution)
    (throw
     (ex "Expected a choreography execution."
         {:execution execution})))
  (:action execution))

(defn- execution-record
  [{:keys [plan
           execution-id
           status
           state
           context
           resources
           action
           awaiting
           trace
           trace-limit
           max-immediate-steps
           result]}]
  (cond->
   {:gesso.choreo.machine/type execution-type
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
    action
    (assoc :action action)

    awaiting
    (assoc :awaiting awaiting)

    (= :completed status)
    (assoc :result result)))

;; -----------------------------------------------------------------------------
;; Send preparation
;; -----------------------------------------------------------------------------

(defn- outgoing-payload
  [context state]
  (let [required (:required state #{})
        optional (:optional state #{})
        missing (set (remove #(contains? context %) required))]
    (when (seq missing)
      (throw
       (ex "Projected send is missing required values in execution context."
           {:state (:state-id state)
            :event (:event state)
            :missing missing
            :required required})))
    (select-keys context
                 (set/union required optional))))

(defn- send-action
  [plan execution-id state-id state context]
  (cond-> {:kind :send
           :execution-id execution-id
           :state state-id
           :from (:role plan)
           :to (:to state)
           :event (:event state)
           :required (:required state #{})
           :optional (:optional state #{})
           :correlation (:correlation state #{})
           :payload (outgoing-payload context
                                      (assoc state :state-id state-id))}
    (contains? state :via)
    (assoc :via (:via state))))

(defn- fx-action
  [plan execution-id state-id state]
  (cond-> {:kind :fx
           :execution-id execution-id
           :state state-id
           :role (:role plan)
           :machine (:machine state)}
    (contains? state :input)
    (assoc :input (:input state))))

;; -----------------------------------------------------------------------------
;; Resource accounting
;; -----------------------------------------------------------------------------

(defn- acquire-resource
  [plan resources state-id resource-id]
  (let [descriptor
        (or (resource-descriptor plan resource-id)
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
  (let [_descriptor
        (or (resource-descriptor plan resource-id)
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
                       (resource-descriptor plan resource-id)))))
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
  (and (= (:from alternative) (:from envelope))
       (= (:to alternative) (:to envelope))
       (= (:event alternative) (:event envelope))
       (= (:via alternative) (:via envelope))))

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
         (message-identity-matches? alternative envelope)
         (required-payload-present? alternative payload)
         (choice-match? alternative payload)
         (correlation-match? alternative context payload))))

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
    (when-some [target (get (:events state {})
                            (:event envelope))]
      {:next target
       :context
       (cond-> context
         (:bind state)
         (assoc (:bind state)
                (:data envelope)))})

    :message
    (let [matches (matching-receives state context envelope)]
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
             (assoc (:bind alternative) payload))})))

    nil))

(defn accepts-event?
  "True when suspended execution can consume envelope without resuming it."
  [execution envelope]
  (boolean
   (and (suspended? execution)
        (event-envelope? envelope)
        (let [state (state-at (:plan execution)
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

(defn- trace-detail
  [state context]
  (case (:op state)
    :fx
    {:machine (:machine state)}

    :send
    {:event (:event state)
     :to (:to state)
     :via (:via state)}

    :choice
    {:choice-key (:key state)
     :choice-value (get context (:key state))}

    :await
    {:events (set (keys (:events state {})))
     :receive-events (set (map :event (:receives state [])))}

    :acquire
    {:resource (:resource state)}

    :release
    {:resource (:resource state)}

    :return
    {:outcome (:outcome state)}

    {}))

(defn- trace-entry
  [state-id state now context]
  (merge
   {:state state-id
    :op (:op state)
    :at now}
   (trace-detail state context)))

;; -----------------------------------------------------------------------------
;; Protocol advancement
;; -----------------------------------------------------------------------------

(defn- advance
  [plan
   {:keys [execution-id
           state
           context
           resources
           trace
           trace-limit
           max-immediate-steps
           now-fn]}]
  (require-plan! plan)
  (let [now-fn (or now-fn default-now)]
    (loop [state-id state
           context context
           resources resources
           trace (vec trace)
           immediate-steps 0]
      (when (>= immediate-steps max-immediate-steps)
        (throw
         (ex "Choreography exceeded the maximum number of immediate protocol steps without reaching an external boundary."
             {:role (:role plan)
              :execution-id execution-id
              :state state-id
              :max-immediate-steps max-immediate-steps})))
      (let [state (assoc (require-state! plan state-id)
                         :state-id state-id)
            trace' (append-trace trace
                                 (trace-entry state-id state (now-fn) context)
                                 trace-limit)]
        (case (:op state)
          :fx
          (execution-record
           {:plan plan
            :execution-id execution-id
            :status :waiting-fx
            :state state-id
            :context context
            :resources resources
            :action (fx-action plan execution-id state-id state)
            :trace trace'
            :trace-limit trace-limit
            :max-immediate-steps max-immediate-steps})

          :send
          (execution-record
           {:plan plan
            :execution-id execution-id
            :status :waiting-send
            :state state-id
            :context context
            :resources resources
            :action (send-action plan execution-id state-id state context)
            :trace trace'
            :trace-limit trace-limit
            :max-immediate-steps max-immediate-steps})

          :choice
          (let [choice-key (:key state)]
            (when-not (contains? context choice-key)
              (throw
               (ex "Projected choreography choice value is absent from execution context."
                   {:state state-id
                    :choice-key choice-key
                    :available-keys (set (keys context))})))
            (let [choice-value (get context choice-key)
                  target (get (:branches state)
                              choice-value
                              ::not-found)]
              (when (= ::not-found target)
                (throw
                 (ex "Projected choreography has no branch for the current choice value."
                     {:state state-id
                      :choice-key choice-key
                      :choice-value choice-value
                      :branches (set (keys (:branches state)))})))
              (recur target
                     context
                     resources
                     trace'
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
            {:events (set (keys (:events state {})))
             :receives
             (mapv
              #(select-keys %
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
            :max-immediate-steps max-immediate-steps})

          :acquire
          (recur (:next state)
                 context
                 (acquire-resource plan
                                   resources
                                   state-id
                                   (:resource state))
                 trace'
                 (inc immediate-steps))

          :release
          (recur (:next state)
                 context
                 (release-resource plan
                                   resources
                                   state-id
                                   (:resource state))
                 trace'
                 (inc immediate-steps))

          :return
          (do
            (assert-terminal-resources! plan resources state-id)
            (execution-record
             {:plan plan
              :execution-id execution-id
              :status :completed
              :state state-id
              :context context
              :resources resources
              :trace trace'
              :trace-limit trace-limit
              :max-immediate-steps max-immediate-steps
              :result
              (if-some [value-key (:value-key state)]
                (get context value-key)
                {:outcome (:outcome state)})}))

          (throw
           (ex "Projected choreography contains an unsupported runtime operation."
               {:state state-id
                :op (:op state)
                :role (:role plan)})))))))

(defn- continuation-options
  [execution state context]
  {:execution-id (:execution-id execution)
   :state state
   :context context
   :resources (:resources execution)
   :trace (:trace execution)
   :trace-limit (:trace-limit execution)
   :max-immediate-steps (:max-immediate-steps execution)})

;; -----------------------------------------------------------------------------
;; Public advancement API
;; -----------------------------------------------------------------------------

(defn start
  "Start projected endpoint execution and advance to its first boundary.

   Options:

     :execution-id
       Opaque correlation identity. Real distributed executions should provide
       one, though nil is allowed for isolated tests.

     :context
       Initial protocol context map.

     :trace-limit
       Number of retained trace entries, default 64.

     :max-immediate-steps
       Guard against local protocol cycles that never reach FX, send, await, or
       return, default 1024."
  [plan {:keys [execution-id
                context
                trace-limit
                max-immediate-steps
                now-fn]}]
  (require-plan! plan)
  (let [trace-limit
        (require-positive-integer!
         "Choreography trace limit"
         (or trace-limit default-trace-limit))
        max-immediate-steps
        (require-positive-integer!
         "Choreography max immediate steps"
         (or max-immediate-steps default-max-immediate-steps))]
    (advance
     plan
     {:execution-id execution-id
      :state (:initial plan)
      :context (require-map! "Choreography execution context"
                             (or context {}))
      :resources {}
      :trace []
      :trace-limit trace-limit
      :max-immediate-steps max-immediate-steps
      :now-fn now-fn})))

(defn complete-fx
  "Continue after the endpoint adapter completes the pending local FX machine.

   result must be a map or nil. Map results are merged into choreography context;
   nil means the FX segment produced no protocol-visible values. The FX runner
   itself remains entirely outside this namespace."
  ([execution result]
   (complete-fx execution result nil))
  ([execution result {:keys [now-fn]}]
   (when-not (waiting-fx? execution)
     (throw
      (ex "Only an execution waiting for FX can complete FX."
          {:execution execution})))
   (when-not (or (nil? result) (map? result))
     (throw
      (ex "Choreography FX result must be a map or nil."
          {:execution-id (:execution-id execution)
           :state (:state execution)
           :result result})))
   (let [plan (:plan execution)
         state-id (:state execution)
         state (require-state! plan state-id)
         context (if (map? result)
                   (merge (:context execution) result)
                   (:context execution))]
     (advance plan
              (assoc (continuation-options execution
                                           (:next state)
                                           context)
                     :now-fn now-fn)))))

(defn complete-send
  "Continue after the endpoint adapter successfully starts/delivers the send.

   This means the send boundary itself succeeded; it does not mean a remote
   response has arrived. A later failure or response should enter through a
   projected await as an environment event or participant message."
  ([execution]
   (complete-send execution nil))
  ([execution {:keys [now-fn]}]
   (when-not (waiting-send? execution)
     (throw
      (ex "Only an execution waiting for send can complete a send."
          {:execution execution})))
   (let [plan (:plan execution)
         state-id (:state execution)
         state (require-state! plan state-id)]
     (advance plan
              (assoc (continuation-options execution
                                           (:next state)
                                           (:context execution))
                     :now-fn now-fn)))))

(defn resume
  "Resume an await with one environmental event or participant message.

   Unexpected envelopes are rejected. A completed or externally-blocked
   execution cannot be resurrected by resume."
  ([execution envelope]
   (resume execution envelope nil))
  ([execution envelope {:keys [now-fn]}]
   (when-not (suspended? execution)
     (throw
      (ex "Only a suspended choreography execution can be resumed."
          {:execution execution})))
   (when-not (event-envelope? envelope)
     (throw
      (ex "Choreography resume requires an event or message envelope."
          {:event envelope})))
   (let [plan (:plan execution)
         state-id (:state execution)
         state (assoc (require-state! plan state-id)
                      :state-id state-id)
         resolution (resolve-await state
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
     (advance plan
              (assoc (continuation-options execution
                                           (:next resolution)
                                           (:context resolution))
                     :now-fn now-fn)))))
