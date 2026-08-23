(ns gesso.choreo.machine
  "Portable role-local execution for Gesso Choreo ExecutablePlans.

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
   keys are part of the required contract. A declared correlation value must
   agree with any value the receiving execution already knows for that key and
   with any same-named explicit identity binding. An as-yet-unknown correlation
   key may be established by the accepted message.

   Receiving a valid participant message establishes only its declared
   required/optional fields as :communicated role-local knowledge. Undeclared
   fields in an explicitly open payload remain transport data and do not enter
   knowledge.

   Environment-event data is likewise closed by default. Projected :await states
   retain per-event required/optional/open-data contracts. Only declared fields
   enter role-local knowledge or deterministic history; undeclared fields admitted
   by an explicitly open event contract remain adapter data. Environment fields
   are conservatively recorded as :asserted by the named environment event; this
   records their origin without upgrading an arbitrary environment observation to
   authoritative truth.

   Authoritative observations additionally carry an opaque authoritative basis.
   Once a role has an authoritative frontier for an observation scope, an event at
   a distinct basis is admissible only with explicit
   :authoritative-basis-progression envelope metadata accepted by
   gesso.choreo.knowledge. The progression decision is policy evidence about basis
   ordering, not semantic event data: it never enters execution-values or
   deterministic environment history. Arrival order and generic replacement do not
   establish progression.

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

   The bound role must equal the ExecutablePlan role. Other relationships are
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
   [gesso.choreo.knowledge :as knowledge]
   [gesso.choreo.project :as project])
  (:refer-clojure :exclude [await]))

;; -----------------------------------------------------------------------------
;; Identity
;; -----------------------------------------------------------------------------

(def execution-type
  :gesso.choreo.machine/execution)

(def executable-plan-type
  project/executable-plan-type)

(def executable-plan-version
  project/executable-plan-version)

(def supported-ops
  project/projected-ops)

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

(defn- event-contract
  [state event]
  (get-in state
          [:event-contracts event]
          {}))

(defn- event-required
  [state event]
  (or (:required
       (event-contract state event))
      #{}))

(defn- event-optional
  [state event]
  (or (:optional
       (event-contract state event))
      #{}))

(defn- event-open-data?
  [state event]
  (true?
   (:open-data?
    (event-contract state event))))

(def ^:private authoritative-observation-contract-keys
  #{:authority
    :observation
    :basis-key})

(defn- event-authoritative-observation
  [state event]
  (:authoritative-observation
   (event-contract state event)))

(defn- validate-authoritative-observation-contract!
  [event contract required context]
  (when (contains? contract :authoritative-observation)
    (let [descriptor
          (:authoritative-observation contract)]
      (when-not (map? descriptor)
        (machine-error
         :invalid-authoritative-observation-contract
         "Projected authoritative observation descriptor must be a map."
         (assoc context
                :event event
                :authoritative-observation descriptor)))

      (let [descriptor-keys
            (set (keys descriptor))

            missing
            (set/difference
             authoritative-observation-contract-keys
             descriptor-keys)

            unknown
            (set/difference
             descriptor-keys
             authoritative-observation-contract-keys)]
        (when (or (seq missing)
                  (seq unknown))
          (machine-error
           :invalid-authoritative-observation-contract
           "Projected authoritative observation descriptor must contain exactly :authority, :observation, and :basis-key."
           (assoc context
                  :event event
                  :missing missing
                  :unknown unknown
                  :authoritative-observation descriptor))))

      (doseq [[field value]
              (select-keys
               descriptor
               authoritative-observation-contract-keys)]
        (when-not (keyword? value)
          (machine-error
           :invalid-authoritative-observation-contract
           "Projected authoritative observation descriptor fields must be keywords."
           (assoc context
                  :event event
                  :field field
                  :value value
                  :authoritative-observation descriptor))))

      (when-not (contains? required
                           (:basis-key descriptor))
        (machine-error
         :invalid-authoritative-observation-contract
         "Projected authoritative observation :basis-key must be required semantic event data."
         (assoc context
                :event event
                :basis-key (:basis-key descriptor)
                :required required
                :authoritative-observation descriptor))))))

(defn- event-allowed
  [state event]
  (set/union
   (event-required state event)
   (event-optional state event)))

(defn- validate-environment-event-contract!
  [event contract context]
  (require-map!
   "Projected await event contract"
   contract)

  (let [required
        (require-keyword-set!
         "Projected await event contract :required"
         (or (:required contract) #{}))

        optional
        (require-keyword-set!
         "Projected await event contract :optional"
         (or (:optional contract) #{}))

        overlap
        (set/intersection
         required
         optional)]

    (when (and (contains? contract :open-data?)
               (not (boolean?
                     (:open-data? contract))))
      (machine-error
       :invalid-open-data
       "Projected await event contract :open-data? must be boolean when present."
       (assoc context
              :event event
              :open-data?
              (:open-data? contract))))

    (when (seq overlap)
      (machine-error
       :ambiguous-event-data-key
       "Projected await event contract may not declare a data key as both required and optional."
       (assoc context
              :event event
              :overlap overlap
              :required required
              :optional optional)))

    (validate-authoritative-observation-contract!
     event
     contract
     required
     context)

    contract))

(def ^:private invalid-environment-data
  ::invalid-environment-data)

(defn- normalize-environment-data
  [data]
  (cond
    (nil? data)
    {}

    (map? data)
    data

    :else
    invalid-environment-data))

(defn- environment-data-contract-result
  [state event data]
  (let [data'
        (normalize-environment-data data)]

    (if (= invalid-environment-data
           data')
      {:valid? false
       :invalid-shape? true
       :missing #{}
       :undeclared #{}}

      (let [data-keys
            (set (keys data'))

            required
            (event-required state event)

            allowed
            (event-allowed state event)

            missing
            (set/difference
             required
             data-keys)

            undeclared
            (if (event-open-data? state event)
              #{}
              (set/difference
               data-keys
               allowed))]

        {:valid?
         (and
          (empty? missing)
          (empty? undeclared))

         :invalid-shape? false
         :missing missing
         :undeclared undeclared
         :data data'}))))

(defn- environment-data-matches-contract?
  [state event data]
  (:valid?
   (environment-data-contract-result
    state
    event
    data)))

(defn- require-environment-data-contract!
  [state event data context]
  (let [{:keys [valid?
                invalid-shape?
                missing
                undeclared
                data]}
        (environment-data-contract-result
         state
         event
         data)]

    (when-not valid?
      (machine-error
       :invalid-environment-data
       "Environment-event data violates the projected await event contract."
       (merge
        context
        {:event event
         :invalid-shape? invalid-shape?
         :missing missing
         :undeclared undeclared
         :required
         (event-required state event)
         :optional
         (event-optional state event)
         :open-data?
         (event-open-data? state event)
         :data-keys
         (if invalid-shape?
           #{}
           (set (keys data)))})))

    data))

(defn- semantic-environment-data
  [state event data]
  (let [data'
        (or (normalize-environment-data data)
            {})]
    (select-keys
     (if (= invalid-environment-data data')
       {}
       data')
     (event-allowed state event))))

(defn- authoritative-observation-basis
  [state event data]
  (when-let [descriptor
             (event-authoritative-observation
              state
              event)]
    (get data
         (:basis-key descriptor))))

(def ^:private authoritative-basis-progression-envelope-key
  :authoritative-basis-progression)

(defn- authoritative-basis-progression
  [envelope]
  (get envelope
       authoritative-basis-progression-envelope-key))

(defn- authoritative-basis-progression-present?
  [envelope]
  (contains? envelope
             authoritative-basis-progression-envelope-key))

(defn- require-authoritative-observation-basis!
  [state event data context]
  (when-let [descriptor
             (event-authoritative-observation
              state
              event)]
    (let [basis
          (authoritative-observation-basis
           state
           event
           data)]
      (when (nil? basis)
        (machine-error
         :invalid-authoritative-observation
         "Authoritative observation environment event requires a non-nil authoritative basis."
         (merge
          context
          {:event event
           :authority (:authority descriptor)
           :observation (:observation descriptor)
           :basis-key (:basis-key descriptor)})))
      basis)))

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
       "ExecutablePlan references an unknown local state."
       {:role (:role plan)
        :state state-id})))

;; -----------------------------------------------------------------------------
;; ExecutablePlan validation
;; -----------------------------------------------------------------------------

(defn executable-plan?
  "True exactly when x satisfies the canonical ExecutablePlan contract owned by
   gesso.choreo.project.

   The machine deliberately does not maintain a second plan-format predicate.
   Projection/compiler format validation happens before execution semantics."
  [x]
  (project/executable-plan? x))

(defn- require-executable-plan!
  [plan]
  (when-not (project/executable-plan? plan)
    (machine-error
     :invalid-plan
     "Expected a canonical Gesso Choreo ExecutablePlan."
     {:plan plan}))
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
   cannot cross-satisfy one another. Data is intentionally not interpreted by
   this constructor; the active projected :await event contract validates it at
   accepts/resume time.

   Authoritative basis progression is intentionally not part of :data. A trusted
   adapter may associate :authoritative-basis-progression with an authoritative
   observation envelope before acceptance/resume. The machine validates and
   consumes that evidence without admitting it into semantic values."
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
       "Machine identity binding :role must equal the ExecutablePlan role."
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
       (executable-plan?
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

    (seq (:event-contracts state))
    (assoc
     :event-contracts
     (:event-contracts state))

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
  (require-executable-plan! plan)

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
  "Start one role-local execution from an ExecutablePlan.

   Options:

     :identity-bindings
       Sparse explicit identity bindings. Supported keys are defined by
       gesso.choreo.identity. A supplied :role must equal the ExecutablePlan
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
         (require-executable-plan! plan)

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

(defn- correlation-expectations
  [execution correlation-key]
  (let [bindings
        (identity-bindings execution)]
    (cond-> []
      (has-execution-value?
       execution
       correlation-key)
      (conj
       (execution-value
        execution
        correlation-key))

      (and
       (contains?
        identity/binding-keys
        correlation-key)
       (contains?
        bindings
        correlation-key))
      (conj
       (get bindings
            correlation-key)))))

(defn- correlation-matches-execution?
  [execution alternative envelope]
  (let [payload
        (:payload envelope)]
    (every?
     (fn [correlation-key]
       (let [actual
             (get payload
                  correlation-key)]
         (every?
          #(= % actual)
          (correlation-expectations
           execution
           correlation-key))))
     (contract-correlation
      alternative))))

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
         (:payload envelope))

        (correlation-matches-execution?
         execution
         %
         envelope))
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

(defn- next-environment-knowledge
  [execution state state-id event normalized-data envelope]
  (let [semantic-data
        (select-keys
         normalized-data
         (event-allowed state event))

        observation
        (event-authoritative-observation
         state
         event)

        progression-present?
        (authoritative-basis-progression-present?
         envelope)

        progression
        (authoritative-basis-progression
         envelope)]

    (when (and (not observation)
               progression-present?)
      (machine-error
       :unexpected-authoritative-progression
       "Ordinary environment events may not carry authoritative basis progression evidence."
       {:execution-id
        (:execution-id execution)
        :role
        (:role execution)
        :state state-id
        :event event
        :authoritative-basis-progression progression}))

    (if (seq semantic-data)
      (if observation
        (let [basis
              (require-authoritative-observation-basis!
               state
               event
               normalized-data
               {:execution-id
                (:execution-id execution)
                :role
                (:role execution)
                :state state-id})]
          (knowledge/establish-authoritative-observation
           (execution-knowledge execution)
           semantic-data
           (:authority observation)
           (:observation observation)
           basis
           (cond->
            {:state state-id
             :metadata
             {:origin :environment
              :event event}
             :replace? true}
             progression-present?
             (assoc
              :basis-progression progression))))
        (knowledge/establish-many
         (execution-knowledge execution)
         semantic-data
         (knowledge/asserted-provenance
          event
          {:metadata
           {:origin :environment
            :state state-id}})
         {:replace? true}))
      (execution-knowledge execution))))

(defn- environment-transition-valid?
  [execution state envelope]
  (try
    (let [event
          (:event envelope)

          normalized-data
          (require-environment-data-contract!
           state
           event
           (:data envelope)
           {:execution-id
            (:execution-id execution)
            :role
            (:role execution)
            :state
            (:state execution)})]
      (next-environment-knowledge
       execution
       state
       (:state execution)
       event
       normalized-data
       envelope)
      true)
    (catch #?(:clj clojure.lang.ExceptionInfo
              :cljs cljs.core.ExceptionInfo) _
      false)))

(defn accepts-environment-event?
  "True when a suspended :await execution can consume envelope.

   Participant messages are never accepted here. The environment event must
   select a declared event edge, its data must satisfy that event's projected
   closed/open data contract, and any authoritative-basis progression evidence
   must make the resulting knowledge transition admissible. Acceptance therefore
   agrees with resume about stale, incomparable, malformed, or mismatched
   authoritative rereads."
  [execution envelope]
  (let [execution'
        (require-execution!
         execution)

        state
        (when (waiting-environment?
               execution')
          (current-state execution'))]

    (boolean
     (and
      state

      (envelope?
       envelope)

      (= :environment
         (:kind envelope))

      (= (:role execution')
         (:role envelope))

      (contains?
       (:events state)
       (:event envelope))

      (environment-data-matches-contract?
       state
       (:event envelope)
       (:data envelope))

      (environment-transition-valid?
       execution'
       state
       envelope)))))

(defn resume-environment
  "Resume one projected :await from a role-local environment event.

   Event data is validated against the projected per-event contract. Only
   declared required/optional fields enter portable role-local knowledge and
   deterministic history. Undeclared fields admitted by :open-data? remain
   adapter data and are discarded at this semantic boundary.

   Ordinary declared environment fields are recorded as :asserted knowledge
   attributed to the environment event keyword. A projected
   :authoritative-observation contract instead establishes only declared semantic
   fields as :authoritative knowledge after requiring its explicit non-nil basis.
   When an existing authoritative frontier is at a distinct basis, progression
   evidence must be supplied as top-level :authoritative-basis-progression
   metadata and accepted by gesso.choreo.knowledge. That evidence is never copied
   into semantic data or deterministic environment history. Undeclared open-data
   extras remain adapter data in either case."
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

      (let [event
            (:event envelope)

            normalized-data
            (require-environment-data-contract!
             state
             event
             (:data envelope)
             {:execution-id
              (:execution-id execution')
              :role
              (:role execution')
              :state
              state-id})

            semantic-data
            (select-keys
             normalized-data
             (event-allowed state event))

            next-knowledge
            (next-environment-knowledge
             execution'
             state
             state-id
             event
             normalized-data
             envelope)

            history-data
            (if (and (nil? (:data envelope))
                     (empty? semantic-data))
              nil
              semantic-data)]

        (enter
         (:plan execution')
         {:identity-bindings
          (identity-bindings execution')

          :state
          next-state

          :knowledge
          next-knowledge

          :history
          (conj
           (:history execution')
           {:kind :environment
            :state state-id
            :role (:role execution')
            :event event
            :data history-data})

          :max-immediate-steps
          (:max-immediate-steps
           execution')})))))

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
