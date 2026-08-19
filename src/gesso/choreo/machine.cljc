(ns gesso.choreo.machine
  "Portable role-local execution for projected Gesso choreographies.

   The machine consumes only the compact local vocabulary emitted by
   gesso.choreo.project:

     :local
       endpoint-local semantic work

     :authoritative
       trusted endpoint realization of one public authoritative semantic
       operation

     :branch
       immediate deterministic control flow from one role-local semantic value

     :send
       endpoint must emit one participant message

     :receive
       endpoint waits for one of several participant messages

     :await
       endpoint waits for one of several role-local environment events

     :return
       local execution is complete

   The machine owns protocol state and event matching. It deliberately does not
   perform local work, transport messages, listen for environment events, touch
   browser APIs, call model code, or know HTMX/XTDB.

   Local work, authoritative work, and sends are explicit external boundaries:

     :waiting-local
       endpoint must perform the named local action and call complete-local

     :waiting-authoritative
       trusted endpoint must invoke the named public authoritative operation and
       call complete-authoritative with exactly its declared semantic outputs

     :waiting-send
       endpoint must emit the pending message and call complete-send

     :waiting-receive
       endpoint is suspended for a participant message

     :waiting-environment
       endpoint is suspended for a role-local environment event

     :completed
       endpoint has no further protocol work

   The machine has a small explicit semantic value store rather than the old
   arbitrary execution-context merge. Local and authoritative actions declare
   required inputs and closed output key sets; deterministic :branch states
   consume established values. This is dataflow only, not yet
   knowledge/provenance.

   An :authoritative boundary is intentionally not executable inside this
   machine. The trusted adapter must realize the public semantic operation. The
   machine neither authenticates the caller nor performs persistence; those are
   obligations of the authoritative realization and later proof/runtime
   contracts.

   Payload is currently opaque map data carried by participant messages. Later
   message/type work must close and validate that boundary before payload fields
   become semantic knowledge."

  (:require
   [clojure.set :as set])
  (:refer-clojure :exclude [await]))

;; -----------------------------------------------------------------------------
;; Identity
;; -----------------------------------------------------------------------------

(def execution-type
  :gesso.choreo.machine/execution)

(def projected-plan-type
  :gesso.choreo/projected-plan)

(def projected-plan-version
  1)

(def supported-ops
  #{:local
    :authoritative
    :branch
    :send
    :receive
    :await
    :return})

(def statuses
  #{:waiting-local
    :waiting-authoritative
    :waiting-send
    :waiting-receive
    :waiting-environment
    :completed})

(def envelope-kinds
  #{:message
    :environment})

(def default-max-immediate-steps
  1024)

;; -----------------------------------------------------------------------------
;; Errors
;; -----------------------------------------------------------------------------

(defn- machine-error
  [kind message data]
  (throw
   (ex-info
    message
    (merge
     {:error/type :gesso.choreo.machine/error
      :error/kind kind}
     data))))

;; -----------------------------------------------------------------------------
;; Small validation helpers
;; -----------------------------------------------------------------------------

(defn- require-keyword!
  [label value]
  (when-not (keyword? value)
    (machine-error
     :invalid-value
     (str label " must be a keyword.")
     {:label label
      :value value}))
  value)

(defn- require-map!
  [label value]
  (when-not (map? value)
    (machine-error
     :invalid-value
     (str label " must be a map.")
     {:label label
      :value value}))
  value)

(defn- require-keyword-set!
  [label value]
  (when-not (and (set? value)
                 (every? keyword? value))
    (machine-error
     :invalid-value-set
     (str label " must be a set of keywords.")
     {:label label
      :value value}))
  value)

(defn- require-positive-integer!
  [label value]
  (when-not (and (integer? value)
                 (pos? value))
    (machine-error
     :invalid-value
     (str label " must be a positive integer.")
     {:label label
      :value value}))
  value)

(defn- state-at
  [plan state-id]
  (get (:states plan)
       state-id))

(defn- require-state!
  [plan state-id]
  (or (state-at plan state-id)
      (machine-error
       :unknown-state
       "Projected plan references an unknown local state."
       {:role (:role plan)
        :state state-id})))

(defn- require-successor!
  [plan state-id state next-state]
  (when-not (contains? (:states plan)
                       next-state)
    (machine-error
     :unknown-successor
     "Projected local state references an unknown successor."
     {:role (:role plan)
      :state state-id
      :op (:op state)
      :next next-state}))
  next-state)

;; -----------------------------------------------------------------------------
;; Projected-plan validation
;; -----------------------------------------------------------------------------

(defn plan?
  "True when x has the shallow identity/shape of a projected plan.

   require-plan! performs the structural checks used by this runtime."
  [x]
  (and (map? x)
       (= projected-plan-type
          (:gesso.choreo/type x))
       (= projected-plan-version
          (:gesso.choreo/version x))
       (keyword? (:role x))
       (map? (:states x))
       (contains? (:states x)
                  (:initial x))))

(defn- validate-receive-alternative!
  [plan state-id alternative]
  (require-map!
   "Projected receive alternative"
   alternative)

  (require-keyword!
   "Projected receive :from"
   (:from alternative))

  (require-keyword!
   "Projected receive :event"
   (:event alternative))

  (when (and (contains? alternative :via)
             (not (keyword? (:via alternative))))
    (machine-error
     :invalid-value
     "Projected receive :via must be a keyword when present."
     {:role (:role plan)
      :state state-id
      :alternative alternative}))

  (require-successor!
   plan
   state-id
   {:op :receive}
   (:next alternative))

  alternative)

(defn- validate-state!
  [plan state-id state]
  (require-map!
   "Projected state"
   state)

  (when-not (contains? supported-ops
                       (:op state))
    (machine-error
     :unsupported-op
     "Projected plan contains an unsupported local operation."
     {:role (:role plan)
      :state state-id
      :op (:op state)
      :supported supported-ops}))

  (case (:op state)
    :local
    (do
      (require-keyword!
       "Projected local :action"
       (:action state))
      (require-keyword-set!
       "Projected local :requires"
       (or (:requires state) #{}))
      (require-keyword-set!
       "Projected local :outputs"
       (or (:outputs state) #{}))
      (require-successor!
       plan
       state-id
       state
       (:next state)))

    :authoritative
    (do
      (require-keyword!
       "Projected authoritative :operation"
       (:operation state))
      (require-keyword-set!
       "Projected authoritative :requires"
       (or (:requires state) #{}))
      (require-keyword-set!
       "Projected authoritative :outputs"
       (or (:outputs state) #{}))
      (require-successor!
       plan
       state-id
       state
       (:next state)))

    :branch
    (do
      (require-keyword!
       "Projected branch :on"
       (:on state))
      (let [cases (:cases state)]
        (when-not (and (map? cases)
                       (seq cases))
          (machine-error
           :invalid-branch
           "Projected :branch requires a non-empty :cases map."
           {:role (:role plan)
            :state state-id
            :cases cases}))
        (doseq [[value next-state] cases]
          (when (nil? value)
            (machine-error
             :invalid-branch-value
             "Projected branch case values may not be nil."
             {:role (:role plan)
              :state state-id
              :value value}))
          (require-successor!
           plan
           state-id
           state
           next-state))))

    :send
    (do
      (require-keyword!
       "Projected send :to"
       (:to state))
      (require-keyword!
       "Projected send :event"
       (:event state))
      (when (= (:role plan)
               (:to state))
        (machine-error
         :same-role-send
         "Projected send must target another role."
         {:role (:role plan)
          :state state-id
          :to (:to state)}))
      (when (and (contains? state :via)
                 (not (keyword? (:via state))))
        (machine-error
         :invalid-value
         "Projected send :via must be a keyword when present."
         {:role (:role plan)
          :state state-id
          :via (:via state)}))
      (require-successor!
       plan
       state-id
       state
       (:next state)))

    :receive
    (let [alternatives
          (:alternatives state)]
      (when-not (and (vector? alternatives)
                     (seq alternatives))
        (machine-error
         :invalid-receive
         "Projected :receive requires a non-empty vector of alternatives."
         {:role (:role plan)
          :state state-id
          :alternatives alternatives}))
      (doseq [alternative alternatives]
        (validate-receive-alternative!
         plan
         state-id
         alternative)))

    :await
    (let [events
          (:events state)]
      (when-not (and (map? events)
                     (seq events))
        (machine-error
         :invalid-await
         "Projected :await requires a non-empty :events map."
         {:role (:role plan)
          :state state-id
          :events events}))
      (doseq [[event next-state] events]
        (require-keyword!
         "Projected await event"
         event)
        (require-successor!
         plan
         state-id
         state
         next-state)))

    :return
    (require-keyword!
     "Projected return :outcome"
     (:outcome state)))

  state)

(defn- require-plan!
  [plan]
  (when-not (plan? plan)
    (machine-error
     :invalid-plan
     "Expected a projected Gesso choreography plan."
     {:plan plan}))

  (doseq [[state-id state]
          (:states plan)]
    (validate-state!
     plan
     state-id
     state))

  plan)

;; -----------------------------------------------------------------------------
;; External envelopes
;; -----------------------------------------------------------------------------

(defn message
  "Construct one participant-message envelope.

   Payload is an opaque map in this first local machine. The machine matches only
   communication identity; later type/knowledge work will define which payload
   values may cross and what receiving them establishes."
  ([from to event payload]
   (message from to event payload nil))
  ([from to event payload {:keys [via]}]
   (let [from'
         (require-keyword!
          "Message :from"
          from)

         to'
         (require-keyword!
          "Message :to"
          to)

         event'
         (require-keyword!
          "Message :event"
          event)]

     (when (= from' to')
       (machine-error
        :same-role-message
        "Participant message must cross roles."
        {:role from'}))

     (require-map!
      "Message payload"
      payload)

     (cond->
      {:kind :message
       :from from'
       :to to'
       :event event'
       :payload payload}
       (some? via)
       (assoc
        :via
        (require-keyword!
         "Message :via"
         via))))))

(defn environment-event
  "Construct one role-local environment-event envelope.

   Participant messages and environment events are different envelope kinds and
   cannot cross-satisfy one another."
  ([role event]
   (environment-event
    role
    event
    nil))
  ([role event data]
   {:kind :environment
    :role
    (require-keyword!
     "Environment event :role"
     role)
    :event
    (require-keyword!
     "Environment event :event"
     event)
    :data data}))

(defn envelope?
  "True when x has one of the envelope kinds understood by this machine."
  [x]
  (and (map? x)
       (contains? envelope-kinds
                  (:kind x))))

;; -----------------------------------------------------------------------------
;; Execution records
;; -----------------------------------------------------------------------------

(defn execution?
  "True when x is a role-local machine execution."
  [x]
  (and (map? x)
       (= execution-type
          (:gesso.choreo.machine/type x))
       (contains? statuses
                  (:status x))
       (plan?
        (:plan x))
       (map?
        (:values x))
       (vector?
        (:history x))))

(defn- require-execution!
  [execution]
  (when-not (execution? execution)
    (machine-error
     :invalid-execution
     "Expected a Gesso choreography machine execution."
     {:execution execution}))
  execution)

(defn waiting-local?
  [execution]
  (= :waiting-local
     (:status
      (require-execution!
       execution))))

(defn waiting-authoritative?
  [execution]
  (= :waiting-authoritative
     (:status
      (require-execution!
       execution))))

(defn waiting-send?
  [execution]
  (= :waiting-send
     (:status
      (require-execution!
       execution))))

(defn waiting-receive?
  [execution]
  (= :waiting-receive
     (:status
      (require-execution!
       execution))))

(defn waiting-environment?
  [execution]
  (= :waiting-environment
     (:status
      (require-execution!
       execution))))

(defn suspended?
  "True for either participant-message or environment suspension."
  [execution]
  (let [status
        (:status
         (require-execution!
          execution))]
    (contains?
     #{:waiting-receive
       :waiting-environment}
     status)))

(defn completed?
  [execution]
  (= :completed
     (:status
      (require-execution!
       execution))))

(defn current-state-id
  [execution]
  (:state
   (require-execution!
    execution)))

(defn current-state
  [execution]
  (let [execution'
        (require-execution!
         execution)]
    (state-at
     (:plan execution')
     (:state execution'))))

(defn pending-action
  "Return the endpoint action required at the current boundary.

   Returns a :local, :authoritative, or :send descriptor, otherwise nil."
  [execution]
  (:action
   (require-execution!
    execution)))

(defn awaiting
  "Return the receive/environment alternatives while suspended, otherwise nil."
  [execution]
  (:awaiting
   (require-execution!
    execution)))

(defn execution-values
  "Return the role-local semantic value store.

   These values are execution data, not yet a proof of knowledge provenance."
  [execution]
  (:values
   (require-execution!
    execution)))

(defn execution-value
  "Return one role-local semantic value by key, or nil when absent."
  [execution k]
  (get (execution-values execution)
       k))

(defn has-execution-value?
  "True when k is present in the role-local semantic value store."
  [execution k]
  (contains? (execution-values execution)
             k))

(defn execution-history
  "Return deterministic local semantic history.

   No wall-clock timestamps are included."
  [execution]
  (:history
   (require-execution!
    execution)))

(defn result
  "Return the local terminal result when completed, otherwise nil."
  [execution]
  (when (completed? execution)
    (:result execution)))

(defn- execution-record
  [{:keys [plan
           execution-id
           status
           state
           action
           awaiting
           values
           history
           max-immediate-steps
           result]}]
  (cond->
   {:gesso.choreo.machine/type
    execution-type

    :plan
    plan

    :role
    (:role plan)

    :execution-id
    execution-id

    :status
    status

    :state
    state

    :values
    (or values {})

    :history
    (vec history)

    :max-immediate-steps
    max-immediate-steps}

    action
    (assoc :action action)

    awaiting
    (assoc :awaiting awaiting)

    (= :completed status)
    (assoc :result result)))

;; -----------------------------------------------------------------------------
;; Boundary descriptors
;; -----------------------------------------------------------------------------

(defn- local-action
  [plan execution-id state-id state values]
  (let [requires (or (:requires state) #{})
        outputs (or (:outputs state) #{})]
    (cond->
     {:kind :local
      :execution-id execution-id
      :state state-id
      :role (:role plan)
      :action (:action state)}
      (seq requires)
      (assoc :inputs
             (select-keys values requires))

      (seq outputs)
      (assoc :outputs outputs))))

(defn- authoritative-action
  [plan execution-id state-id state values]
  (let [requires (or (:requires state) #{})
        outputs (or (:outputs state) #{})]
    (cond->
     {:kind :authoritative
      :execution-id execution-id
      :state state-id
      :role (:role plan)
      :operation (:operation state)}
      (seq requires)
      (assoc :inputs
             (select-keys values requires))

      (seq outputs)
      (assoc :outputs outputs))))

(defn- send-action
  [plan execution-id state-id state]
  (cond->
   {:kind :send
    :execution-id execution-id
    :state state-id
    :from (:role plan)
    :to (:to state)
    :event (:event state)}
    (contains? state :via)
    (assoc :via
           (:via state))))

(defn- receive-awaiting
  [plan state]
  {:kind :receive
   :role (:role plan)
   :alternatives
   (mapv
    #(select-keys
      %
      [:from
       :event
       :via])
    (:alternatives state))})

(defn- environment-awaiting
  [plan state]
  {:kind :environment
   :role (:role plan)
   :events
   (set
    (keys
     (:events state)))})

;; -----------------------------------------------------------------------------
;; Advancement
;; -----------------------------------------------------------------------------

(defn- enter
  [plan
   {:keys [execution-id
           state
           values
           history
           max-immediate-steps]}]
  (require-plan! plan)

  (loop [state-id state
         values' (or values {})
         history' (vec history)
         immediate-steps 0]

    (when (>= immediate-steps
              max-immediate-steps)
      (machine-error
       :immediate-step-limit
       "Projected execution exceeded the maximum number of immediate local transitions."
       {:role (:role plan)
        :execution-id execution-id
        :state state-id
        :max-immediate-steps max-immediate-steps}))

    (let [state'
          (require-state!
           plan
           state-id)]

      (case (:op state')
        :local
        (let [requires
              (or (:requires state') #{})

              missing
              (set
               (remove
                #(contains? values' %)
                requires))]

          (when (seq missing)
            (machine-error
             :missing-local-inputs
             "Projected local action requires semantic values not present in this execution."
             {:role (:role plan)
              :execution-id execution-id
              :state state-id
              :action (:action state')
              :missing missing
              :available (set (keys values'))}))

          (execution-record
           {:plan plan
            :execution-id execution-id
            :status :waiting-local
            :state state-id
            :action
            (local-action
             plan
             execution-id
             state-id
             state'
             values')
            :values values'
            :history history'
            :max-immediate-steps max-immediate-steps}))

        :authoritative
        (let [requires
              (or (:requires state') #{})

              missing
              (set
               (remove
                #(contains? values' %)
                requires))]

          (when (seq missing)
            (machine-error
             :missing-authoritative-inputs
             "Projected authoritative operation requires semantic values not present in this execution."
             {:role (:role plan)
              :execution-id execution-id
              :state state-id
              :operation (:operation state')
              :missing missing
              :available (set (keys values'))}))

          (execution-record
           {:plan plan
            :execution-id execution-id
            :status :waiting-authoritative
            :state state-id
            :action
            (authoritative-action
             plan
             execution-id
             state-id
             state'
             values')
            :values values'
            :history history'
            :max-immediate-steps max-immediate-steps}))

        :branch
        (let [on (:on state')]
          (when-not (contains? values' on)
            (machine-error
             :missing-branch-value
             "Projected branch selector is absent from the role-local semantic value store."
             {:role (:role plan)
              :execution-id execution-id
              :state state-id
              :on on
              :available (set (keys values'))}))

          (let [selected
                (get values' on)

                next-state
                (get (:cases state')
                     selected)]

            (when-not next-state
              (machine-error
               :branch-value-not-covered
               "Projected branch selector value does not name a declared case."
               {:role (:role plan)
                :execution-id execution-id
                :state state-id
                :on on
                :value selected
                :cases (set (keys (:cases state')))}))

            (recur
             next-state
             values'
             (conj
              history'
              {:kind :branch
               :state state-id
               :role (:role plan)
               :on on
               :value selected})
             (inc immediate-steps))))

        :send
        (execution-record
         {:plan plan
          :execution-id execution-id
          :status :waiting-send
          :state state-id
          :action
          (send-action
           plan
           execution-id
           state-id
           state')
          :values values'
          :history history'
          :max-immediate-steps max-immediate-steps})

        :receive
        (execution-record
         {:plan plan
          :execution-id execution-id
          :status :waiting-receive
          :state state-id
          :awaiting
          (receive-awaiting
           plan
           state')
          :values values'
          :history history'
          :max-immediate-steps max-immediate-steps})

        :await
        (execution-record
         {:plan plan
          :execution-id execution-id
          :status :waiting-environment
          :state state-id
          :awaiting
          (environment-awaiting
           plan
           state')
          :values values'
          :history history'
          :max-immediate-steps max-immediate-steps})

        :return
        (execution-record
         {:plan plan
          :execution-id execution-id
          :status :completed
          :state state-id
          :values values'
          :history
          (conj
           history'
           {:kind :terminal
            :state state-id
            :outcome (:outcome state')})
          :max-immediate-steps max-immediate-steps
          :result
          {:outcome (:outcome state')}})

        (machine-error
         :unsupported-op
         "Projected execution reached an unsupported operation."
         {:role (:role plan)
          :execution-id execution-id
          :state state-id
          :op (:op state')})))))

;; -----------------------------------------------------------------------------
;; Start
;; -----------------------------------------------------------------------------

(defn start
  "Start one projected role-local execution.

   Options:

     :execution-id
       Opaque execution identity. Nil remains permitted while command/execution
       identity semantics are still being completed.

     :values
       Initial role-local semantic values. This is execution data only; later
       knowledge/provenance verification must establish why this role may know
       each value.

     :max-immediate-steps
       Guard against accidental immediate branch loops. Defaults to 1024."
  ([plan]
   (start plan nil))
  ([plan {:keys [execution-id
                 values
                 max-immediate-steps]
          :or {values {}}}]
   (let [plan'
         (require-plan! plan)

         values'
         (require-map!
          "Machine initial :values"
          values)

         max-immediate-steps'
         (require-positive-integer!
          "Machine :max-immediate-steps"
          (or max-immediate-steps
              default-max-immediate-steps))]

     (enter
      plan'
      {:execution-id execution-id
       :state (:initial plan')
       :values values'
       :history []
       :max-immediate-steps max-immediate-steps'}))))

;; -----------------------------------------------------------------------------
;; Local completion
;; -----------------------------------------------------------------------------

(defn complete-local
  "Continue after the endpoint successfully performs the pending local action.

   outputs must contain exactly the keys declared by the projected local state.
   The values are merged into the role-local semantic value store. No undeclared
   value may leak into later control flow.

   The one-argument form is retained for local actions that declare no outputs."
  ([execution]
   (complete-local execution {}))
  ([execution outputs]
   (let [execution'
         (require-execution! execution)]

     (when-not (waiting-local? execution')
       (machine-error
        :not-waiting-local
        "Only an execution waiting on a local action can complete that action."
        {:execution-id (:execution-id execution')
         :role (:role execution')
         :state (:state execution')
         :status (:status execution')}))

     (require-map!
      "Local action outputs"
      outputs)

     (let [state-id
           (:state execution')

           state
           (current-state execution')

           declared
           (or (:outputs state) #{})

           actual
           (set (keys outputs))]

       (when-not (= declared actual)
         (machine-error
          :local-output-mismatch
          "Local action returned keys different from its declared closed output set."
          {:execution-id (:execution-id execution')
           :role (:role execution')
           :state state-id
           :action (:action state)
           :declared declared
           :actual actual
           :missing (set/difference declared actual)
           :unexpected (set/difference actual declared)}))

       (let [history-entry
             (cond->
              {:kind :local
               :state state-id
               :role (:role execution')
               :action (:action state)}
               (seq outputs)
               (assoc :outputs outputs))]

         (enter
          (:plan execution')
          {:execution-id (:execution-id execution')
           :state (:next state)
           :values (merge (:values execution')
                          outputs)
           :history (conj (:history execution')
                          history-entry)
           :max-immediate-steps
           (:max-immediate-steps execution')}))))))

;; -----------------------------------------------------------------------------
;; Authoritative completion
;; -----------------------------------------------------------------------------

(defn complete-authoritative
  "Continue after the trusted endpoint realizes the pending public
   authoritative semantic operation.

   outputs must contain exactly the keys declared by the projected
   :authoritative state. Only those declared outputs enter the role-local
   semantic value store.

   This function records semantic completion of the authoritative operation; it
   does not itself authenticate a principal, authorize the operation, reread
   model state, commit a transaction, or decide whether a commit occurred.
   Those responsibilities belong to the trusted authoritative adapter and the
   public operation result contract.

   The one-argument form is valid only for authoritative operations declaring no
   outputs."
  ([execution]
   (complete-authoritative execution {}))
  ([execution outputs]
   (let [execution'
         (require-execution!
          execution)]

     (when-not (waiting-authoritative?
                execution')
       (machine-error
        :not-waiting-authoritative
        "Only an execution waiting on an authoritative operation can complete that operation."
        {:execution-id
         (:execution-id execution')
         :role
         (:role execution')
         :state
         (:state execution')
         :status
         (:status execution')}))

     (require-map!
      "Authoritative operation outputs"
      outputs)

     (let [state-id
           (:state execution')

           state
           (current-state execution')

           declared
           (or (:outputs state)
               #{})

           actual
           (set
            (keys outputs))]

       (when-not (= declared
                    actual)
         (machine-error
          :authoritative-output-mismatch
          "Authoritative operation returned keys different from its declared closed output set."
          {:execution-id
           (:execution-id execution')
           :role
           (:role execution')
           :state
           state-id
           :operation
           (:operation state)
           :declared
           declared
           :actual
           actual
           :missing
           (set/difference
            declared
            actual)
           :unexpected
           (set/difference
            actual
            declared)}))

       (let [history-entry
             (cond->
              {:kind :authoritative
               :state state-id
               :role (:role execution')
               :operation
               (:operation state)}
               (seq outputs)
               (assoc
                :outputs
                outputs))]

         (enter
          (:plan execution')
          {:execution-id
           (:execution-id execution')
           :state
           (:next state)
           :values
           (merge
            (:values execution')
            outputs)
           :history
           (conj
            (:history execution')
            history-entry)
           :max-immediate-steps
           (:max-immediate-steps
            execution')}))))))

;; -----------------------------------------------------------------------------
;; Send completion
;; -----------------------------------------------------------------------------

(defn pending-message
  "Construct the participant message represented by the current :send boundary.

   This does not advance execution. The endpoint may inspect/transport this
   envelope and call complete-send only after the send boundary has succeeded.

   Payload is supplied by the endpoint because value/dataflow semantics have not
   yet been introduced into the projected language."
  [execution payload]
  (let [execution'
        (require-execution!
         execution)]

    (when-not (waiting-send? execution')
      (machine-error
       :not-waiting-send
       "Only an execution waiting to send has a pending participant message."
       {:execution-id
        (:execution-id execution')
        :role
        (:role execution')
        :state
        (:state execution')
        :status
        (:status execution')}))

    (let [action
          (:action execution')]

      (message
       (:from action)
       (:to action)
       (:event action)
       payload
       (when (contains? action :via)
         {:via
          (:via action)})))))

(defn complete-send
  "Continue after the endpoint successfully emits the pending participant
   message.

   payload is recorded in deterministic local history so later correspondence
   work can compare the sender's emitted communication with the receiver's
   consumed communication.

   A successful send boundary means only that the endpoint accepted/performed
   the send operation according to its transport contract. It does not mean the
   receiver has consumed the message."
  [execution payload]
  (let [execution'
        (require-execution!
         execution)]

    (when-not (waiting-send? execution')
      (machine-error
       :not-waiting-send
       "Only an execution waiting to send can complete a send."
       {:execution-id
        (:execution-id execution')
        :role
        (:role execution')
        :state
        (:state execution')
        :status
        (:status execution')}))

    (let [state-id
          (:state execution')

          state
          (current-state execution')

          envelope
          (pending-message
           execution'
           payload)

          history-entry
          (cond->
           {:kind :send
            :state state-id
            :from (:from envelope)
            :to (:to envelope)
            :event (:event envelope)
            :payload (:payload envelope)}
            (contains? envelope :via)
            (assoc :via
                   (:via envelope)))

          next-execution
          (enter
           (:plan execution')
           {:execution-id
            (:execution-id execution')

            :state
            (:next state)

            :values
            (:values execution')

            :history
            (conj
             (:history execution')
             history-entry)

            :max-immediate-steps
            (:max-immediate-steps
             execution')})]

      {:execution
       next-execution

       :message
       envelope})))

;; -----------------------------------------------------------------------------
;; Receive matching
;; -----------------------------------------------------------------------------

(defn- receive-identity-matches?
  [plan alternative envelope]
  (and (= :message
          (:kind envelope))
       (= (:from alternative)
          (:from envelope))
       (= (:role plan)
          (:to envelope))
       (= (:event alternative)
          (:event envelope))
       (= (:via alternative)
          (:via envelope))))

(defn- matching-receive-alternatives
  [execution envelope]
  (let [plan
        (:plan execution)

        state
        (current-state execution)]

    (vec
     (filter
      #(receive-identity-matches?
        plan
        %
        envelope)
      (:alternatives state)))))

(defn accepts-message?
  "True when a suspended :receive execution can consume envelope.

   This function does not advance the execution."
  [execution envelope]
  (let [execution'
        (require-execution!
         execution)]

    (boolean
     (and
      (waiting-receive?
       execution')

      (envelope?
       envelope)

      (= :message
         (:kind envelope))

      (= 1
         (count
          (matching-receive-alternatives
           execution'
           envelope)))))))

(defn receive
  "Consume one participant message at a projected :receive gate.

   Unexpected messages are rejected. A participant message can never satisfy a
   projected environment :await."
  [execution envelope]
  (let [execution'
        (require-execution!
         execution)]

    (when-not (waiting-receive?
               execution')
      (machine-error
       :not-waiting-receive
       "Only an execution waiting for participant communication can receive a message."
       {:execution-id
        (:execution-id execution')
        :role
        (:role execution')
        :state
        (:state execution')
        :status
        (:status execution')
        :envelope envelope}))

    (when-not (and (envelope? envelope)
                   (= :message
                      (:kind envelope)))
      (machine-error
       :invalid-message
       "Projected receive requires a participant-message envelope."
       {:execution-id
        (:execution-id execution')
        :role
        (:role execution')
        :state
        (:state execution')
        :envelope envelope}))

    (let [matches
          (matching-receive-alternatives
           execution'
           envelope)]

      (cond
        (empty? matches)
        (machine-error
         :message-not-enabled
         "Participant message is not enabled at the current projected receive."
         {:execution-id
          (:execution-id execution')
          :role
          (:role execution')
          :state
          (:state execution')
          :awaiting
          (:awaiting execution')
          :message
          (dissoc envelope
                  :payload)})

        (> (count matches)
           1)
        (machine-error
         :ambiguous-message
         "Participant message matches multiple projected receive alternatives."
         {:execution-id
          (:execution-id execution')
          :role
          (:role execution')
          :state
          (:state execution')
          :matches matches
          :message
          (dissoc envelope
                  :payload)})

        :else
        (let [alternative
              (first matches)

              state-id
              (:state execution')

              history-entry
              (cond->
               {:kind :receive
                :state state-id
                :from (:from envelope)
                :to (:to envelope)
                :event (:event envelope)
                :payload (:payload envelope)}
                (contains? envelope :via)
                (assoc :via
                       (:via envelope)))]

          (enter
           (:plan execution')
           {:execution-id
            (:execution-id execution')

            :state
            (:next alternative)

            :values
            (:values execution')

            :history
            (conj
             (:history execution')
             history-entry)

            :max-immediate-steps
            (:max-immediate-steps
             execution')}))))))

;; -----------------------------------------------------------------------------
;; Environment waits
;; -----------------------------------------------------------------------------

(defn accepts-environment-event?
  "True when a suspended :await execution can consume envelope.

   Participant messages are never accepted here."
  [execution envelope]
  (let [execution'
        (require-execution!
         execution)]

    (boolean
     (and
      (waiting-environment?
       execution')

      (envelope?
       envelope)

      (= :environment
         (:kind envelope))

      (= (:role execution')
         (:role envelope))

      (contains?
       (:events
        (current-state execution'))
       (:event envelope))))))

(defn resume-environment
  "Resume one projected :await from a role-local environment event."
  [execution envelope]
  (let [execution'
        (require-execution!
         execution)]

    (when-not (waiting-environment?
               execution')
      (machine-error
       :not-waiting-environment
       "Only an execution waiting on its environment can consume an environment event."
       {:execution-id
        (:execution-id execution')
        :role
        (:role execution')
        :state
        (:state execution')
        :status
        (:status execution')
        :envelope envelope}))

    (when-not (and (envelope? envelope)
                   (= :environment
                      (:kind envelope)))
      (machine-error
       :invalid-environment-event
       "Projected await requires an environment-event envelope."
       {:execution-id
        (:execution-id execution')
        :role
        (:role execution')
        :state
        (:state execution')
        :envelope envelope}))

    (let [state-id
          (:state execution')

          state
          (current-state execution')

          next-state
          (when (= (:role execution')
                   (:role envelope))
            (get (:events state)
                 (:event envelope)))]

      (when-not next-state
        (machine-error
         :environment-event-not-enabled
         "Environment event is not enabled at the current projected await."
         {:execution-id
          (:execution-id execution')
          :role
          (:role execution')
          :state
          state-id
          :awaiting
          (:awaiting execution')
          :event envelope}))

      (enter
       (:plan execution')
       {:execution-id
        (:execution-id execution')

        :state
        next-state

        :values
        (:values execution')

        :history
        (conj
         (:history execution')
         {:kind :environment
          :state state-id
          :role (:role execution')
          :event (:event envelope)
          :data (:data envelope)})

        :max-immediate-steps
        (:max-immediate-steps
         execution')}))))

;; -----------------------------------------------------------------------------
;; Generic external-event API
;; -----------------------------------------------------------------------------

(defn accepts?
  "True when the current suspended execution can consume envelope.

   Local and send boundaries are completed explicitly and therefore never accept
   an incoming envelope."
  [execution envelope]
  (let [execution'
        (require-execution!
         execution)]

    (case (:status execution')
      :waiting-receive
      (accepts-message?
       execution'
       envelope)

      :waiting-environment
      (accepts-environment-event?
       execution'
       envelope)

      false)))

(defn resume
  "Resume a suspended execution with one external envelope.

   Dispatches strictly by the current suspension kind. This keeps participant
   communication and environment events semantically separate."
  [execution envelope]
  (let [execution'
        (require-execution!
         execution)]

    (case (:status execution')
      :waiting-receive
      (receive
       execution'
       envelope)

      :waiting-environment
      (resume-environment
       execution'
       envelope)

      (machine-error
       :not-suspended
       "Only a receive/environment suspension can be resumed by an external envelope."
       {:execution-id
        (:execution-id execution')
        :role
        (:role execution')
        :state
        (:state execution')
        :status
        (:status execution')
        :envelope envelope}))))

;; -----------------------------------------------------------------------------
;; Inspection
;; -----------------------------------------------------------------------------

(defn explain
  "Return a compact stable summary suitable for REPL diagnostics."
  [execution]
  (let [execution'
        (require-execution!
         execution)]

    {:role
     (:role execution')

     :execution-id
     (:execution-id execution')

     :status
     (:status execution')

     :state
     (:state execution')

     :op
     (:op
      (current-state execution'))

     :action
     (:action execution')

     :awaiting
     (:awaiting execution')

     :result
     (:result execution')

     :value-keys
     (set
      (keys
       (:values execution')))

     :history-count
     (count
      (:history execution'))}))
