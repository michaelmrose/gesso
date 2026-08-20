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

   The machine owns one explicit role-local knowledge state rather than an
   unqualified value bag. Local and authoritative actions declare required
   inputs and closed output key sets; deterministic :branch states consume
   values that the role currently knows. execution-values remains a plain-map
   projection of this richer state for callers that only need values.

   An :authoritative boundary is intentionally not executable inside this
   machine. The trusted adapter must realize the public semantic operation. The
   machine neither authenticates the caller nor performs persistence; those are
   obligations of the authoritative realization and later proof/runtime
   contracts.

   Participant-message payload shape is enforced from the projected message
   contract. Required keys must be present; undeclared keys are rejected unless
   :open-payload? is explicitly true; optional keys may be omitted. Correlation
   keys are part of the required contract.

   Receiving a valid participant message establishes only its declared
   required/optional fields as :communicated role-local knowledge. Undeclared
   fields in an explicitly open payload remain transport data and do not enter
   knowledge.

   Entry values are :input knowledge. Trusted authoritative outputs become
   :authoritative knowledge. Local outputs are conservatively recorded as
   :asserted by the named local action; that records origin without silently
   upgrading local computation to authoritative truth.

   Outbound semantic payload fields are checked against sender knowledge.
   Every declared required field, and every declared optional field that is
   actually present, must have the exact value currently known by the sending
   role. Explicitly open undeclared fields remain transport-only and are not
   subject to this semantic knowledge check.

   Machine execution identity is explicit and separate from semantic values.
   A machine may carry sparse identity bindings for principal, actor, authority,
   host, command-id, and execution-id. Bound command-id and execution-id are
   distinct tagged identity references; a raw scalar cannot stand in for either.

   The bound role must equal the projected plan role. Other relationships are
   not inferred: principal does not imply actor, host does not imply authority,
   and constructing a binding does not authenticate or authorize anyone.

   Machine identity metadata is local execution context. It is never silently
   injected into participant-message payloads. If command or execution identity
   must cross a protocol boundary, the choreography must declare that semantic
   message field explicitly.

   This machine still does not prove authentication, authorization, provenance
   quality, or purity of local computation."

  (:require
   [clojure.set :as set]
   [gesso.choreo.identity :as identity]
   [gesso.choreo.knowledge :as knowledge])
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

(defn- contract-required
  [contract]
  (or (:required contract)
      #{}))

(defn- contract-optional
  [contract]
  (or (:optional contract)
      #{}))

(defn- contract-correlation
  [contract]
  (or (:correlation contract)
      #{}))

(defn- contract-open-payload?
  [contract]
  (true?
   (:open-payload? contract)))

(defn- contract-allowed
  [contract]
  (set/union
   (contract-required contract)
   (contract-optional contract)))

(defn- validate-message-contract!
  [label contract context]
  (let [required
        (require-keyword-set!
         (str label " :required")
         (contract-required contract))

        optional
        (require-keyword-set!
         (str label " :optional")
         (contract-optional contract))

        correlation
        (require-keyword-set!
         (str label " :correlation")
         (contract-correlation contract))

        overlap
        (set/intersection
         required
         optional)]

    (when (and (contains? contract :open-payload?)
               (not (boolean?
                     (:open-payload? contract))))
      (machine-error
       :invalid-open-payload
       (str label " :open-payload? must be boolean when present.")
       (assoc context
              :open-payload?
              (:open-payload? contract))))

    (when (seq overlap)
      (machine-error
       :ambiguous-message-key
       (str label " may not declare a payload key as both required and optional.")
       (assoc context
              :overlap overlap
              :required required
              :optional optional)))

    (when-not (set/subset?
               correlation
               required)
      (machine-error
       :optional-correlation-key
       (str label " correlation keys must also be required payload keys.")
       (assoc context
              :correlation correlation
              :required required)))

    contract))

(defn- payload-contract-result
  [contract payload]
  (let [payload-keys
        (set
         (keys payload))

        required
        (contract-required contract)

        allowed
        (contract-allowed contract)

        missing
        (set/difference
         required
         payload-keys)

        undeclared
        (if (contract-open-payload? contract)
          #{}
          (set/difference
           payload-keys
           allowed))]

    {:valid?
     (and
      (empty? missing)
      (empty? undeclared))

     :missing
     missing

     :undeclared
     undeclared}))

(defn- payload-matches-contract?
  [contract payload]
  (and
   (map? payload)
   (:valid?
    (payload-contract-result
     contract
     payload))))

(defn- require-payload-contract!
  [contract payload context]
  (require-map!
   "Message payload"
   payload)

  (let [{:keys [valid?
                missing
                undeclared]}
        (payload-contract-result
         contract
         payload)]

    (when-not valid?
      (machine-error
       :invalid-message-payload
       "Participant-message payload violates the projected message contract."
       (merge
        context
        {:missing missing
         :undeclared undeclared
         :required
         (contract-required contract)
         :optional
         (contract-optional contract)
         :correlation
         (contract-correlation contract)
         :open-payload?
         (contract-open-payload? contract)
         :payload-keys
         (set
          (keys payload))}))))

  payload)

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

  (validate-message-contract!
   "Projected receive"
   alternative
   {:role (:role plan)
    :state state-id
    :alternative alternative})

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
      (validate-message-contract!
       "Projected send"
       state
       {:role (:role plan)
        :state state-id})
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

   This constructor validates only envelope shape because it is not tied to a
   projected state. The active :send or :receive contract is enforced when the
   envelope crosses that machine boundary."
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
;; Machine identity bindings
;; -----------------------------------------------------------------------------

(defn- require-command-id!
  [value]
  (when-not (identity/command-id? value)
    (machine-error
     :invalid-command-id
     "Machine :command-id must be a tagged Choreo command identity."
     {:command-id value}))
  value)

(defn- require-execution-id!
  [value]
  (when-not (identity/execution-id? value)
    (machine-error
     :invalid-execution-id
     "Machine :execution-id must be a tagged Choreo execution identity."
     {:execution-id value}))
  value)

(defn- merge-explicit-binding
  [bindings key value]
  (if (nil? value)
    bindings
    (let [existing
          (get bindings key ::absent)]
      (when (and (not= ::absent existing)
                 (not= existing value))
        (machine-error
         :identity-binding-conflict
         "Machine identity option conflicts with the same explicit identity binding."
         {:binding key
          :binding-value existing
          :option-value value}))

      (assoc bindings key value))))

(defn- normalize-machine-bindings
  [plan
   bindings
   command-id
   execution-id]
  (let [bindings'
        (try
          (identity/bindings
           (or bindings {}))
          (catch #?(:clj Throwable
                    :cljs :default) ex
            (machine-error
             :invalid-identity-bindings
             "Machine :identity-bindings are malformed."
             {:identity-bindings bindings
              :cause
              (ex-data ex)})))

        command-id'
        (when (some? command-id)
          (require-command-id!
           command-id))

        execution-id'
        (when (some? execution-id)
          (require-execution-id!
           execution-id))

        with-explicit
        (-> bindings'
            (merge-explicit-binding
             :command-id
             command-id')
            (merge-explicit-binding
             :execution-id
             execution-id'))

        bound-role
        (:role with-explicit)

        plan-role
        (:role plan)]

    (when (and (some? bound-role)
               (not= bound-role
                     plan-role))
      (machine-error
       :identity-role-mismatch
       "Machine identity binding :role must equal the projected plan role."
       {:plan-role plan-role
        :bound-role bound-role}))

    ;; Every execution has an explicit role binding even when all runtime
    ;; identities remain otherwise anonymous.
    (identity/bindings
     (assoc with-explicit
            :role
            plan-role))))

(defn- non-role-bindings
  [bindings]
  (dissoc bindings :role))

(defn- descriptor-identities
  [descriptor bindings]
  (let [runtime-identities
        (non-role-bindings bindings)

        command-id
        (:command-id bindings)

        execution-id
        (:execution-id bindings)]

    (cond->
     (assoc descriptor
            :execution-id execution-id)

      (some? command-id)
      (assoc
       :command-id
       command-id)

      (seq runtime-identities)
      (assoc
       :identity-bindings
       bindings))))

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
       (identity/bindings?
        (:identity-bindings x))
       (= (:role x)
          (:role
           (:identity-bindings x)))
       (= (:role x)
          (:role
           (:plan x)))
       (= (:execution-id x)
          (:execution-id
           (:identity-bindings x)))
       (= (:command-id x)
          (:command-id
           (:identity-bindings x)))
       (knowledge/knowledge?
        (:knowledge x))
       (= (:role x)
          (knowledge/role
           (:knowledge x)))
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

(defn identity-bindings
  "Return the validated explicit identity bindings for this execution."
  [execution]
  (:identity-bindings
   (require-execution!
    execution)))

(defn command-id
  "Return the bound tagged command identity, or nil when none is bound."
  [execution]
  (:command-id
   (require-execution!
    execution)))

(defn execution-id
  "Return the bound tagged execution identity, or nil when none is bound."
  [execution]
  (:execution-id
   (require-execution!
    execution)))

(defn execution-knowledge
  "Return the role-local knowledge/provenance state owned by this execution."
  [execution]
  (:knowledge
   (require-execution!
    execution)))

(defn execution-values
  "Return current role-local semantic values as a plain key->value map.

   This is a projection of execution-knowledge, not a separately maintained
   source of truth."
  [execution]
  (knowledge/values
   (execution-knowledge
    execution)))

(defn execution-value
  "Return one current role-local semantic value by key, or nil when absent."
  [execution k]
  (knowledge/value
   (execution-knowledge execution)
   k))

(defn has-execution-value?
  "True when k is currently known by this role-local execution."
  [execution k]
  (knowledge/known?
   (execution-knowledge execution)
   k))

(defn execution-provenance
  "Return provenance records for one current role-local value, or nil when the
   key is unknown."
  [execution k]
  (knowledge/provenance
   (execution-knowledge execution)
   k))

(defn execution-provenance-kinds
  "Return provenance kinds justifying one current role-local value."
  [execution k]
  (knowledge/provenance-kinds-for
   (execution-knowledge execution)
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
           identity-bindings
           status
           state
           action
           awaiting
           knowledge
           history
           max-immediate-steps
           result]}]
  (let [command-id
        (:command-id identity-bindings)

        execution-id
        (:execution-id identity-bindings)]

    (cond->
     {:gesso.choreo.machine/type
      execution-type

      :plan
      plan

      :role
      (:role plan)

      :identity-bindings
      identity-bindings

      :command-id
      command-id

      :execution-id
      execution-id

      :status
      status

    :state
    state

    :knowledge
    knowledge

    :history
    (vec history)

    :max-immediate-steps
    max-immediate-steps}

    action
    (assoc :action action)

    awaiting
    (assoc :awaiting awaiting)

      (= :completed status)
      (assoc :result result))))

;; -----------------------------------------------------------------------------
;; Boundary descriptors
;; -----------------------------------------------------------------------------

(defn- local-action
  [plan identity-bindings state-id state values]
  (let [requires (or (:requires state) #{})
        outputs (or (:outputs state) #{})]
    (cond->
     (descriptor-identities
      {:kind :local
       :state state-id
       :role (:role plan)
       :action (:action state)}
      identity-bindings)
      (seq requires)
      (assoc :inputs
             (select-keys values requires))

      (seq outputs)
      (assoc :outputs outputs))))

(defn- authoritative-action
  [plan identity-bindings state-id state values]
  (let [requires (or (:requires state) #{})
        outputs (or (:outputs state) #{})]
    (cond->
     (descriptor-identities
      {:kind :authoritative
       :state state-id
       :role (:role plan)
       :operation (:operation state)}
      identity-bindings)
      (seq requires)
      (assoc :inputs
             (select-keys values requires))

      (seq outputs)
      (assoc :outputs outputs))))

(defn- send-action
  [plan identity-bindings state-id state]
  (cond->
   (descriptor-identities
    {:kind :send
     :state state-id
     :from (:role plan)
     :to (:to state)
     :event (:event state)}
    identity-bindings)
    (contains? state :via)
    (assoc :via
           (:via state))

    (seq (contract-required state))
    (assoc :required
           (contract-required state))

    (seq (contract-optional state))
    (assoc :optional
           (contract-optional state))

    (seq (contract-correlation state))
    (assoc :correlation
           (contract-correlation state))

    (contract-open-payload? state)
    (assoc :open-payload?
           true)))

(defn- receive-awaiting
  [plan identity-bindings state]
  (cond->
   {:kind :receive
    :role (:role plan)
    :alternatives
    (mapv
    #(select-keys
      %
      [:from
       :event
       :via
       :required
       :optional
       :correlation
       :open-payload?])
     (:alternatives state))}
    (seq
     (non-role-bindings
      identity-bindings))
    (assoc
     :identity-bindings
     identity-bindings)))

(defn- environment-awaiting
  [plan identity-bindings state]
  (cond->
   {:kind :environment
    :role (:role plan)
    :events
    (set
     (keys
      (:events state)))}
    (seq
     (non-role-bindings
      identity-bindings))
    (assoc
     :identity-bindings
     identity-bindings)))

;; -----------------------------------------------------------------------------
;; Advancement
;; -----------------------------------------------------------------------------

(defn- enter
  [plan
   {:keys [identity-bindings
           state
           knowledge
           history
           max-immediate-steps]}]
  (require-plan! plan)

  (let [execution-id
        (:execution-id identity-bindings)]

    (loop [state-id state
         knowledge' knowledge
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
           state-id)

          values'
          (knowledge/values
           knowledge')]

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
            :identity-bindings identity-bindings
            :status :waiting-local
            :state state-id
            :action
            (local-action
             plan
             identity-bindings
             state-id
             state'
             values')
            :knowledge knowledge'
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
            :identity-bindings identity-bindings
            :status :waiting-authoritative
            :state state-id
            :action
            (authoritative-action
             plan
             identity-bindings
             state-id
             state'
             values')
            :knowledge knowledge'
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
             knowledge'
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
          :identity-bindings identity-bindings
          :status :waiting-send
          :state state-id
          :action
          (send-action
           plan
           identity-bindings
           state-id
           state')
          :knowledge knowledge'
          :history history'
          :max-immediate-steps max-immediate-steps})

        :receive
        (execution-record
         {:plan plan
          :identity-bindings identity-bindings
          :status :waiting-receive
          :state state-id
          :awaiting
          (receive-awaiting
           plan
           identity-bindings
           state')
          :knowledge knowledge'
          :history history'
          :max-immediate-steps max-immediate-steps})

        :await
        (execution-record
         {:plan plan
          :identity-bindings identity-bindings
          :status :waiting-environment
          :state state-id
          :awaiting
          (environment-awaiting
           plan
           identity-bindings
           state')
          :knowledge knowledge'
          :history history'
          :max-immediate-steps max-immediate-steps})

        :return
        (execution-record
         {:plan plan
          :identity-bindings identity-bindings
          :status :completed
          :state state-id
          :knowledge knowledge'
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
          :op (:op state')}))))))

;; -----------------------------------------------------------------------------
;; Start
;; -----------------------------------------------------------------------------

(defn start
  "Start one projected role-local execution.

   Options:

     :identity-bindings
       Sparse explicit identity bindings. Supported keys are defined by
       gesso.choreo.identity. A supplied :role must equal the projected plan
       role. The resulting execution always contains its role binding.

     :command-id
       Optional convenience binding. When present it must already be a tagged
       identity/command-id. It must agree with :identity-bindings when both
       provide :command-id.

     :execution-id
       Optional convenience binding. When present it must already be a tagged
       identity/execution-id. It must agree with :identity-bindings when both
       provide :execution-id.

       Raw strings, UUIDs, and keywords are intentionally rejected here; callers
       must choose whether a raw identifier is a command or an execution.

     :values
       Initial role-local semantic values. Each supplied key/value is
       established as :input knowledge for this role. Semantic values remain
       separate from machine identity bindings.

     :max-immediate-steps
       Guard against accidental immediate branch loops. Defaults to 1024.

   Anonymous portable executions remain valid: command-id, execution-id,
   principal, actor, authority, and host are all optional at this layer."
  ([plan]
   (start plan nil))
  ([plan {:keys [identity-bindings
                 command-id
                 execution-id
                 values
                 max-immediate-steps]
          :or {values {}}}]
   (let [plan'
         (require-plan! plan)

         bindings'
         (normalize-machine-bindings
          plan'
          identity-bindings
          command-id
          execution-id)

         values'
         (require-map!
          "Machine initial :values"
          values)

         max-immediate-steps'
         (require-positive-integer!
          "Machine :max-immediate-steps"
          (or max-immediate-steps
              default-max-immediate-steps))

         initial-knowledge
         (knowledge/establish-inputs
          (knowledge/empty-knowledge
           (:role plan'))
          values')]

     (enter
      plan'
      {:identity-bindings bindings'
       :state (:initial plan')
       :knowledge initial-knowledge
       :history []
       :max-immediate-steps max-immediate-steps'}))))

;; -----------------------------------------------------------------------------
;; Local completion
;; -----------------------------------------------------------------------------

(defn complete-local
  "Continue after the endpoint successfully performs the pending local action.

   outputs must contain exactly the keys declared by the projected local state.
   The values become role-local :asserted knowledge attributed to the named
   local action. No undeclared value may leak into later control flow.

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
          {:identity-bindings
           (identity-bindings execution')
           :state (:next state)
           :knowledge
           (knowledge/establish-many
            (execution-knowledge execution')
            outputs
            (knowledge/asserted-provenance
             (:action state)
             {:metadata
              {:state state-id}})
            {:replace? true})
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
   :authoritative state. Only those declared outputs enter role-local
   knowledge, with :authoritative provenance naming the public operation.

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
          {:identity-bindings
           (identity-bindings execution')
           :state
           (:next state)
           :knowledge
           (knowledge/establish-many
            (execution-knowledge execution')
            outputs
            (knowledge/authoritative-provenance
             (:operation state)
             {:state state-id})
            {:replace? true})
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

(defn- semantic-payload-keys
  [contract payload]
  (set/intersection
   (set
    (keys payload))
   (contract-allowed contract)))

(defn- sender-payload-knowledge-result
  [execution contract payload]
  (let [semantic-keys
        (semantic-payload-keys
         contract
         payload)

        unknown
        (set
         (remove
          #(has-execution-value?
            execution
            %)
          semantic-keys))

        mismatched
        (into
         {}
         (keep
          (fn [key]
            (when (and
                   (has-execution-value?
                    execution
                    key)
                   (not=
                    (execution-value
                     execution
                     key)
                    (get payload key)))
              [key
               {:known
                (execution-value
                 execution
                 key)
                :payload
                (get payload key)}])))
         semantic-keys)]

    {:valid?
     (and
      (empty? unknown)
      (empty? mismatched))

     :semantic-keys
     semantic-keys

     :unknown
     unknown

     :mismatched
     mismatched}))

(defn- require-sender-payload-knowledge!
  [execution contract payload context]
  (let [{:keys [valid?
                semantic-keys
                unknown
                mismatched]}
        (sender-payload-knowledge-result
         execution
         contract
         payload)]

    (when-not valid?
      (machine-error
       :invalid-message-knowledge
       "Participant-message semantic payload does not match the sender's role-local knowledge."
       (merge
        context
        {:semantic-payload-keys
         semantic-keys

         :unknown
         unknown

         :mismatched
         mismatched

         :known-value-keys
         (set
          (keys
           (execution-values
            execution)))})))))

(defn pending-message
  "Construct the participant message represented by the current :send boundary.

   This does not advance execution. The endpoint may inspect/transport this
   envelope and call complete-send only after the send boundary has succeeded.

   Payload is supplied by the endpoint and must satisfy the projected send
   contract before an envelope can be returned.

   Every declared semantic field that is actually present must also equal the
   sender's current role-local knowledge. Required fields therefore must be both
   present and known. Optional declared fields are checked when supplied.
   Undeclared fields admitted by :open-payload? remain transport-only."
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
          (:action execution')

          state
          (current-state execution')]

      (let [context
            {:execution-id
             (:execution-id execution')
             :role
             (:role execution')
             :state
             (:state execution')
             :direction
             :send
             :event
             (:event action)}]

        (require-payload-contract!
         state
         payload
         context)

        (require-sender-payload-knowledge!
         execution'
         state
         payload
         context))

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
           {:identity-bindings
            (identity-bindings execution')

            :state
            (:next state)

            :knowledge
            (execution-knowledge execution')

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
      #(and
        (receive-identity-matches?
         plan
         %
         envelope)

        (payload-matches-contract?
         %
         (:payload envelope)))
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

   Identity and payload contract must select exactly one receive alternative.
   Unexpected, contract-invalid, or ambiguous messages are rejected. A
   participant message can never satisfy a projected environment :await."
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
                      (:kind envelope))
                   (map?
                    (:payload envelope)))
      (machine-error
       :invalid-message
       "Projected receive requires a participant-message envelope with a map payload."
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
           envelope)

          plan
          (:plan execution')

          state
          (current-state execution')

          identity-matches
          (vec
           (filter
            #(receive-identity-matches?
              plan
              %
              envelope)
            (:alternatives state)))]

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
                  :payload)
          :payload-keys
          (set
           (keys
            (:payload envelope)))
          :identity-matches
          (mapv
           #(select-keys
             %
             [:from
              :event
              :via
              :required
              :optional
              :correlation
              :open-payload?])
           identity-matches)})

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
           {:identity-bindings
            (identity-bindings execution')

            :state
            (:next alternative)

            :knowledge
            (knowledge/establish-communicated
             (execution-knowledge execution')
             (:from envelope)
             (:event envelope)
             (set/union
              (contract-required alternative)
              (contract-optional alternative))
             (:payload envelope)
             (cond->
              {:state state-id
               :replace? true}
               (contains? envelope :via)
               (assoc
                :via
                (:via envelope))))

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
       {:identity-bindings
        (identity-bindings execution')

        :state
        next-state

        :knowledge
        (execution-knowledge execution')

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

     :identity-bindings
     (identity-bindings execution')

     :command-id
     (command-id execution')

     :execution-id
     (execution-id execution')

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
       (execution-values execution')))

     :provenance-kinds-by-key
     (into
      {}
      (map
       (fn [key]
         [key
          (execution-provenance-kinds
           execution'
           key)]))
      (keys
       (execution-values execution')))

     :history-count
     (count
      (:history execution'))}))
