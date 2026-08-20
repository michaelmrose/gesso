(ns gesso.choreo.semantics
  "Executable global semantics for the current Gesso Choreo core.

   This namespace is intentionally independent of gesso.choreo.core,
   gesso.choreo.verify, gesso.choreo.project, and gesso.choreo.machine. It
   defines the behavior those layers must represent, verify, project, and
   execute.

   The current language has six operations:

   :local
     A role-local semantic action. It may require previously established
     semantic values and may produce a closed set of declared outputs. Local
     means distributed-unobservable; it must not hide an authoritative model
     transition.

   :authoritative
     Invocation/result of one public authoritative semantic operation. The
     choreography names the semantic operation, not a Biff FX machine or XTDB
     implementation. Its declared inputs and outputs form a closed boundary.

   :branch
     A role-local deterministic branch on one already-established semantic
     value. Branching is semantic control flow, not arbitrary runtime choice.

   :communicate
     One atomic semantic communication from one role to another. Global
     choreography does not model separate send and receive states; those are
     projected realization concerns.

     Message payloads are closed by default. A communication may declare
     :required, :optional, and :correlation payload keys. Undeclared keys are
     rejected unless :open-payload? is explicitly true. Correlation keys must
     also be required keys.

   :await
     A role-local event supplied by the environment. Environment events are not
     participant messages.

   :return
     A global terminal outcome.

   The configuration contains a global semantic value store. That store is NOT
   yet a knowledge model. It records which values exist in the abstract global
   execution so dataflow and deterministic branching can be stated precisely.
   Later verification must separately prove which actors know which values and
   why.

   Local and authoritative outputs are closed by default: their completion
   events must return exactly the keys declared by the state.

   Participant messages are also closed by default. A communication event must
   contain every declared required key and, unless explicitly open, may contain
   only declared required/optional keys. This enforces the boundary shape only.
   It does NOT yet prove that the sender knows the facts it transmits or that
   receiving them establishes justified role-local knowledge; those are later
   knowledge/provenance obligations.

   Execution records both full semantic history and a semantic observable
   trace. Local actions, branch decisions, and environment events appear in
   history but not in that trace. Authoritative operation results, participant
   communication, and terminal outcome appear in both.

   An :authoritative event means the trusted authoritative adapter completed the
   named public semantic operation and obtained its semantic result. The result
   does not by itself imply that a write committed: rejected/failed operations
   may legitimately produce an authoritative result without changing the world.
   Whether a particular result denotes a committed transition belongs to that
   public operation's contract. Once such a contract says commit succeeded, later
   rendering, publication, SSE, or browser failure cannot retroactively turn it
   into an uncommitted operation."
  (:refer-clojure :exclude [await])
  (:require
   [clojure.set :as set]))

;; -----------------------------------------------------------------------------
;; Identity
;; -----------------------------------------------------------------------------

(def semantics-version 4)

(def program-type
  :gesso.choreo.semantics/program)

(def configuration-type
  :gesso.choreo.semantics/configuration)

(def supported-ops
  #{:local
    :authoritative
    :branch
    :communicate
    :await
    :return})

(def event-kinds
  #{:local
    :authoritative
    :branch
    :communication
    :environment})

(def statuses
  #{:running
    :completed})

;; -----------------------------------------------------------------------------
;; Errors
;; -----------------------------------------------------------------------------

(defn- fail!
  [kind message data]
  (throw
   (ex-info
    message
    (merge
     {:error/type
      :gesso.choreo.semantics/error

      :error/kind
      kind}
     data))))

;; -----------------------------------------------------------------------------
;; Small validation helpers
;; -----------------------------------------------------------------------------

(defn- require-keyword!
  [label value]
  (when-not (keyword? value)
    (fail!
     :invalid-value
     (str label " must be a keyword.")
     {:label label
      :value value}))
  value)

(defn- require-map!
  [label value]
  (when-not (map? value)
    (fail!
     :invalid-value
     (str label " must be a map.")
     {:label label
      :value value}))
  value)

(defn- require-keyword-set!
  [label value]
  (when-not (and (set? value)
                 (every? keyword? value))
    (fail!
     :invalid-value-set
     (str label " must be a set of keywords.")
     {:label label
      :value value}))
  value)

(defn- require-successor!
  [states state-id target]
  (when-not (contains? states target)
    (fail!
     :unknown-successor
     "Semantic state references an unknown successor."
     {:state state-id
      :next target}))
  target)

(defn- declared-requires
  [state]
  (or (:requires state)
      #{}))

(defn- declared-outputs
  [state]
  (or (:outputs state)
      #{}))

(defn- declared-required
  [state]
  (or (:required state)
      #{}))

(defn- declared-optional
  [state]
  (or (:optional state)
      #{}))

(defn- declared-correlation
  [state]
  (or (:correlation state)
      #{}))

(defn- open-payload?
  [state]
  (true? (:open-payload? state)))

;; -----------------------------------------------------------------------------
;; Program validation
;; -----------------------------------------------------------------------------

(defn- validate-local-state!
  [states state-id state]
  (require-keyword!
   "Local :role"
   (:role state))

  (require-keyword!
   "Local :action"
   (:action state))

  (require-keyword-set!
   "Local :requires"
   (declared-requires state))

  (require-keyword-set!
   "Local :outputs"
   (declared-outputs state))

  (require-successor!
   states
   state-id
   (:next state))

  state)

(defn- validate-authoritative-state!
  [states state-id state]
  (require-keyword!
   "Authoritative :role"
   (:role state))

  (require-keyword!
   "Authoritative :operation"
   (:operation state))

  (require-keyword-set!
   "Authoritative :requires"
   (declared-requires state))

  (require-keyword-set!
   "Authoritative :outputs"
   (declared-outputs state))

  (require-successor!
   states
   state-id
   (:next state))

  state)

(defn- validate-branch-state!
  [states state-id state]
  (require-keyword!
   "Branch :role"
   (:role state))

  (require-keyword!
   "Branch :on"
   (:on state))

  (let [cases
        (:cases state)]
    (when-not (and (map? cases)
                   (seq cases))
      (fail!
       :invalid-branch
       "Branch requires a non-empty :cases map."
       {:state state-id
        :cases cases}))

    (doseq [[value target] cases]
      (require-successor!
       states
       state-id
       target)

      (when (nil? value)
        (fail!
         :invalid-branch-value
         "Branch case values may not be nil."
         {:state state-id
          :value value}))))

  state)

(defn- validate-communication-state!
  [states state-id state]
  (let [from
        (require-keyword!
         "Communication :from"
         (:from state))

        to
        (require-keyword!
         "Communication :to"
         (:to state))

        required
        (require-keyword-set!
         "Communication :required"
         (declared-required state))

        optional
        (require-keyword-set!
         "Communication :optional"
         (declared-optional state))

        correlation
        (require-keyword-set!
         "Communication :correlation"
         (declared-correlation state))]

    (when (= from to)
      (fail!
       :same-role-communication
       "Communication must cross roles."
       {:state state-id
        :role from}))

    (require-keyword!
     "Communication :event"
     (:event state))

    (when (contains? state :via)
      (require-keyword!
       "Communication :via"
       (:via state)))

    (when (and (contains? state :open-payload?)
               (not (boolean? (:open-payload? state))))
      (fail!
       :invalid-open-payload
       "Communication :open-payload? must be boolean when present."
       {:state state-id
        :open-payload?
        (:open-payload? state)}))

    (let [overlap
          (set/intersection
           required
           optional)]

      (when (seq overlap)
        (fail!
         :ambiguous-message-key
         "A communication key may not be both required and optional."
         {:state state-id
          :overlap overlap})))

    (when-not (set/subset?
               correlation
               required)
      (fail!
       :optional-correlation-key
       "Communication correlation keys must be required payload keys."
       {:state state-id
        :correlation correlation
        :required required}))

    (require-successor!
     states
     state-id
     (:next state))

    state))

(defn- validate-await-state!
  [states state-id state]
  (require-keyword!
   "Await :role"
   (:role state))

  (let [events
        (:events state)]
    (when-not (and (map? events)
                   (seq events))
      (fail!
       :invalid-await
       "Await requires a non-empty :events map."
       {:state state-id
        :events events}))

    (doseq [[event target] events]
      (require-keyword!
       "Await event"
       event)

      (require-successor!
       states
       state-id
       target)))

  state)

(defn- validate-return-state!
  [state-id state]
  (require-keyword!
   "Return :outcome"
   (:outcome state))

  (when (contains? state :next)
    (fail!
     :terminal-has-successor
     "Return may not have a successor."
     {:state state-id
      :next (:next state)}))

  state)

(defn- validate-state!
  [states state-id state]
  (require-map!
   "Semantic state"
   state)

  (when-not (contains? supported-ops
                       (:op state))
    (fail!
     :unsupported-op
     "Semantic state has an unsupported operation."
     {:state state-id
      :op (:op state)
      :supported supported-ops}))

  (case (:op state)
    :local
    (validate-local-state!
     states
     state-id
     state)

    :authoritative
    (validate-authoritative-state!
     states
     state-id
     state)

    :branch
    (validate-branch-state!
     states
     state-id
     state)

    :communicate
    (validate-communication-state!
     states
     state-id
     state)

    :await
    (validate-await-state!
     states
     state-id
     state)

    :return
    (validate-return-state!
     state-id
     state)))

(defn ->program
  "Normalize and structurally validate a semantic program.

   State ids are opaque EDN map keys.

   Structural validation here is intentionally limited to what the transition
   relation itself requires. Rich graph diagnostics, knowledge proofs, authority
   checks, and projection obligations belong in later layers."
  [{:keys [initial states] :as program}]
  (require-map!
   "Semantic program"
   program)

  (require-map!
   "Semantic program :states"
   states)

  (when (empty? states)
    (fail!
     :missing-states
     "Semantic program requires states."
     {}))

  (when-not (contains? states initial)
    (fail!
     :unknown-initial-state
     "Semantic program :initial must name a declared state."
     {:initial initial}))

  (doseq [[state-id state] states]
    (validate-state!
     states
     state-id
     state))

  (assoc program
         :gesso.choreo.semantics/type
         program-type

         :gesso.choreo.semantics/version
         semantics-version))

(defn program?
  [x]
  (and (map? x)
       (= program-type
          (:gesso.choreo.semantics/type x))
       (= semantics-version
          (:gesso.choreo.semantics/version x))
       (map? (:states x))
       (contains? (:states x)
                  (:initial x))))

(defn ensure-program
  [program]
  (->program
   (dissoc program
           :gesso.choreo.semantics/type
           :gesso.choreo.semantics/version)))

;; -----------------------------------------------------------------------------
;; Events
;; -----------------------------------------------------------------------------

(defn local-event
  "Construct occurrence/completion of one local semantic action.

   outputs must be a map. Whether its keys are exactly the declared output keys
   is checked against the current state by enabled?/transition."
  ([role action]
   (local-event
    role
    action
    {}))
  ([role action outputs]
   (require-map!
    "Local event :outputs"
    outputs)

   {:kind :local
    :role
    (require-keyword!
     "Local event :role"
     role)
    :action
    (require-keyword!
     "Local event :action"
     action)
    :outputs outputs}))

(defn authoritative-event
  "Construct completion/result of one public authoritative semantic operation.

   outputs must be a map and must exactly match the state's declared :outputs
   set when this event is consumed. The trusted realization is responsible for
   invoking the operation through the authoritative server/model boundary; this
   constructor itself does not confer authority."
  ([role operation]
   (authoritative-event
    role
    operation
    {}))
  ([role operation outputs]
   (require-map!
    "Authoritative event :outputs"
    outputs)

   {:kind :authoritative
    :role
    (require-keyword!
     "Authoritative event :role"
     role)
    :operation
    (require-keyword!
     "Authoritative event :operation"
     operation)
    :outputs outputs}))

(defn branch-event
  "Construct one deterministic local branch occurrence.

   value must equal the current semantic value stored under on. The event does
   not establish a new value; it records that role followed the continuation
   determined by an already-established value."
  [role on value]
  {:kind :branch
   :role
   (require-keyword!
    "Branch event :role"
    role)
   :on
   (require-keyword!
    "Branch event :on"
    on)
   :value value})

(defn communication-event
  "Construct one semantic participant communication.

   Payload is validated against the current communication state's closed
   contract by enabled?/transition. Constructing an envelope alone does not
   confer knowledge or authority."
  ([from to event payload]
   (communication-event
    from
    to
    event
    payload
    nil))
  ([from to event payload {:keys [via]}]
   (require-map!
    "Communication payload"
    payload)

   (let [from'
         (require-keyword!
          "Communication event :from"
          from)

         to'
         (require-keyword!
          "Communication event :to"
          to)]

     (when (= from' to')
       (fail!
        :same-role-communication
        "Communication must cross roles."
        {:role from'}))

     (cond->
      {:kind :communication
       :from from'
       :to to'
       :event
       (require-keyword!
        "Communication event :event"
        event)
       :payload payload}
       (some? via)
       (assoc
        :via
        (require-keyword!
         "Communication event :via"
         via))))))

(defn environment-event
  "Construct one role-local environment event.

   A participant message can never satisfy an await merely because it uses the
   same event keyword."
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

(defn semantic-event?
  [x]
  (and (map? x)
       (contains? event-kinds
                  (:kind x))))

;; -----------------------------------------------------------------------------
;; Configuration
;; -----------------------------------------------------------------------------

(defn configuration?
  [x]
  (and (map? x)
       (= configuration-type
          (:gesso.choreo.semantics/type x))
       (= semantics-version
          (:gesso.choreo.semantics/version x))
       (contains? statuses
                  (:status x))
       (program? (:program x))
       (map? (:values x))
       (vector? (:history x))
       (vector? (:trace x))))

(defn- require-configuration!
  [x]
  (when-not (configuration? x)
    (fail!
     :invalid-configuration
     "Expected a semantic configuration."
     {:configuration x}))
  x)

(defn current-state-id
  [configuration]
  (:state
   (require-configuration!
    configuration)))

(defn current-state
  [configuration]
  (let [configuration'
        (require-configuration!
         configuration)]
    (get-in configuration'
            [:program
             :states
             (:state configuration')])))

(defn running?
  [configuration]
  (= :running
     (:status
      (require-configuration!
       configuration))))

(defn completed?
  [configuration]
  (= :completed
     (:status
      (require-configuration!
       configuration))))

(defn outcome
  [configuration]
  (when (completed? configuration)
    (:outcome configuration)))

(defn values
  "Return the current abstract semantic value store.

   This is global execution data, not actor-local knowledge."
  [configuration]
  (:values
   (require-configuration!
    configuration)))

(defn value
  "Return one semantic value by key, or nil when absent."
  [configuration k]
  (get (values configuration)
       k))

(defn knows-value?
  "True only in the narrow sense that the abstract global execution currently
   contains k.

   This function intentionally does NOT mean that any particular role knows k."
  [configuration k]
  (contains? (values configuration)
             k))

(defn history
  "All semantic transitions, without wall-clock timestamps."
  [configuration]
  (:history
   (require-configuration!
    configuration)))

(defn observable-trace
  "Distributed observations: communication and terminal outcome only."
  [configuration]
  (:trace
   (require-configuration!
    configuration)))

(defn- settle-terminal
  [configuration]
  (let [state-id
        (:state configuration)

        state
        (get-in configuration
                [:program
                 :states
                 state-id])]

    (if (= :return
           (:op state))
      (let [entry
            {:kind :terminal
             :state state-id
             :outcome (:outcome state)}]

        (-> configuration
            (assoc
             :status :completed
             :outcome (:outcome state))
            (update
             :history
             conj
             entry)
            (update
             :trace
             conj
             entry)))

      configuration)))

(defn start
  "Create the initial semantic configuration.

   Options:

     :values
       Initial abstract semantic values. These are execution inputs only; this
       function does not yet assign role knowledge or provenance."
  ([program]
   (start
    program
    nil))
  ([program {:keys [values]
             :or {values {}}}]
   (require-map!
    "Initial semantic :values"
    values)

   (let [program'
         (ensure-program program)]
     (settle-terminal
      {:gesso.choreo.semantics/type
       configuration-type

       :gesso.choreo.semantics/version
       semantics-version

       :program
       program'

       :status
       :running

       :state
       (:initial program')

       :values
       values

       :history
       []

       :trace
       []}))))

;; -----------------------------------------------------------------------------
;; Enabled-event relation
;; -----------------------------------------------------------------------------

(defn- requires-satisfied?
  [configuration state]
  (every?
   #(contains?
     (:values configuration)
     %)
   (declared-requires state)))

(defn- outputs-match?
  [state event]
  (= (declared-outputs state)
     (set
      (keys
       (:outputs event)))))

(defn expected-event
  "Describe the event shape capable of advancing the current state.

   For :branch the selected value is included because branch execution is
   deterministic from the current semantic value store."
  [configuration]
  (let [configuration'
        (require-configuration!
         configuration)]

    (when (running? configuration')
      (let [state
            (current-state configuration')]

        (case (:op state)
          :local
          {:kind :local
           :role (:role state)
           :action (:action state)
           :requires
           (declared-requires state)
           :outputs
           (declared-outputs state)}

          :authoritative
          {:kind :authoritative
           :role (:role state)
           :operation (:operation state)
           :requires
           (declared-requires state)
           :outputs
           (declared-outputs state)}

          :branch
          (let [on
                (:on state)]
            {:kind :branch
             :role (:role state)
             :on on
             :value
             (get (:values configuration')
                  on)
             :value-present?
             (contains?
              (:values configuration')
              on)
             :cases
             (set
              (keys
               (:cases state)))})

          :communicate
          (cond->
           {:kind :communication
            :from (:from state)
            :to (:to state)
            :event (:event state)
            :required
            (declared-required state)
            :optional
            (declared-optional state)
            :correlation
            (declared-correlation state)
            :open-payload?
            (open-payload? state)}
            (contains? state :via)
            (assoc
             :via
             (:via state)))

          :await
          {:kind :environment
           :role (:role state)
           :events
           (set
            (keys
             (:events state)))}

          :return
          nil)))))

(defn- local-enabled?
  [configuration state event]
  (and (= :local
          (:kind event))
       (= (:role state)
          (:role event))
       (= (:action state)
          (:action event))
       (requires-satisfied?
        configuration
        state)
       (outputs-match?
        state
        event)))

(defn- authoritative-enabled?
  [configuration state event]
  (and (= :authoritative
          (:kind event))
       (= (:role state)
          (:role event))
       (= (:operation state)
          (:operation event))
       (requires-satisfied?
        configuration
        state)
       (outputs-match?
        state
        event)))

(defn- branch-enabled?
  [configuration state event]
  (let [on
        (:on state)

        values'
        (:values configuration)]

    (and (= :branch
            (:kind event))
         (= (:role state)
            (:role event))
         (= on
            (:on event))
         (contains? values'
                    on)
         (= (get values' on)
            (:value event))
         (contains?
          (:cases state)
          (:value event)))))

(defn- communication-payload-valid?
  [state payload]
  (let [payload-keys
        (set
         (keys payload))

        required
        (declared-required state)

        optional
        (declared-optional state)

        allowed
        (set/union
         required
         optional)]

    (and
     (set/subset?
      required
      payload-keys)

     (or
      (open-payload? state)

      (set/subset?
       payload-keys
       allowed)))))

(defn- communication-enabled?
  [state event]
  (and (= :communication
          (:kind event))
       (= (:from state)
          (:from event))
       (= (:to state)
          (:to event))
       (= (:event state)
          (:event event))
       (= (:via state)
          (:via event))
       (map?
        (:payload event))
       (communication-payload-valid?
        state
        (:payload event))))

(defn enabled?
  "True when event is admitted by the current global state.

   :local additionally requires:
   - all declared :requires keys already exist in the semantic value store;
   - event output keys exactly equal the state's declared :outputs set.

   :authoritative has the same closed requires/outputs rules as :local, but is
   semantically externally significant and therefore appears in the observable
   trace.

   :branch additionally requires:
   - the selected value already exists under :on;
   - the event value exactly matches that stored value;
   - the value names a declared case.

   :communicate additionally requires:
   - every declared :required payload key is present;
   - undeclared keys are absent unless :open-payload? is true;
   - :optional keys may be omitted."
  [configuration event]
  (let [configuration'
        (require-configuration!
         configuration)]

    (boolean
     (and
      (running?
       configuration')

      (semantic-event?
       event)

      (let [state
            (current-state configuration')]

        (case (:op state)
          :local
          (local-enabled?
           configuration'
           state
           event)

          :authoritative
          (authoritative-enabled?
           configuration'
           state
           event)

          :branch
          (branch-enabled?
           configuration'
           state
           event)

          :communicate
          (communication-enabled?
           state
           event)

          :await
          (and (= :environment
                  (:kind event))
               (= (:role state)
                  (:role event))
               (contains?
                (:events state)
                (:event event)))

          :return
          false))))))

;; -----------------------------------------------------------------------------
;; Global transition relation
;; -----------------------------------------------------------------------------

(defn- transition-target
  [state event]
  (case (:op state)
    :local
    (:next state)

    :authoritative
    (:next state)

    :branch
    (get (:cases state)
         (:value event))

    :communicate
    (:next state)

    :await
    (get (:events state)
         (:event event))))

(defn- history-entry
  [state-id state event]
  (case (:op state)
    :local
    (cond->
     {:kind :local
      :state state-id
      :role (:role state)
      :action (:action state)}
      (seq (:outputs event))
      (assoc
       :outputs
       (:outputs event)))

    :authoritative
    (cond->
     {:kind :authoritative
      :state state-id
      :role (:role state)
      :operation (:operation state)}
      (seq (:outputs event))
      (assoc
       :outputs
       (:outputs event)))

    :branch
    {:kind :branch
     :state state-id
     :role (:role state)
     :on (:on state)
     :value (:value event)}

    :communicate
    (cond->
     {:kind :communication
      :state state-id
      :from (:from state)
      :to (:to state)
      :event (:event state)
      :payload (:payload event)}
      (contains? state :via)
      (assoc
       :via
       (:via state)))

    :await
    {:kind :environment
     :state state-id
     :role (:role state)
     :event (:event event)
     :data (:data event)}))

(defn- apply-event-values
  [configuration state event]
  (case (:op state)
    :local
    (update
     configuration
     :values
     merge
     (:outputs event))

    :authoritative
    (update
     configuration
     :values
     merge
     (:outputs event))

    configuration))

(defn transition
  "Apply one GlobalStep.

   Result:

     {:configuration next-configuration
      :effects []
      :observations [...]}

   The current slice still has no physical effects. It defines semantic
   occurrence and abstract value flow only. An :authoritative event records the
   result of an authoritative operation after the trusted adapter has realized
   it; this function does not itself run model code or transactions. Transport,
   browser effects, and role-local knowledge remain realization/proof concerns."
  [configuration event]
  (let [configuration'
        (require-configuration!
         configuration)]

    (when-not (running?
               configuration')
      (fail!
       :terminal-transition
       "Completed configuration cannot transition."
       {:outcome
        (:outcome configuration')
        :event event}))

    (when-not (semantic-event?
               event)
      (fail!
       :invalid-event
       "GlobalStep requires a semantic event."
       {:event event}))

    (when-not (enabled?
               configuration'
               event)
      (fail!
       :event-not-enabled
       "Semantic event is not enabled in the current state."
       {:state
        (:state configuration')
        :values
        (:values configuration')
        :expected
        (expected-event configuration')
        :event event}))

    (let [state-id
          (:state configuration')

          state
          (current-state configuration')

          entry
          (history-entry
           state-id
           state
           event)

          trace-count
          (count
           (:trace configuration'))

          next-configuration
          (-> configuration'
              (apply-event-values
               state
               event)
              (assoc
               :state
               (transition-target
                state
                event))
              (update
               :history
               conj
               entry)
              (cond->
               (contains?
                #{:authoritative
                  :communication}
                (:kind entry))
                (update
                 :trace
                 conj
                 entry))
              settle-terminal)]

      {:configuration
       next-configuration

       :effects
       []

       :observations
       (subvec
        (:trace next-configuration)
        trace-count)})))

(defn step
  "Apply one GlobalStep and return only the next configuration."
  [configuration event]
  (:configuration
   (transition
    configuration
    event)))

;; -----------------------------------------------------------------------------
;; Inspection
;; -----------------------------------------------------------------------------

(defn explain
  "Compact REPL view of a semantic configuration."
  [configuration]
  (let [configuration'
        (require-configuration!
         configuration)]

    {:name
     (get-in configuration'
             [:program :name])

     :status
     (:status configuration')

     :state
     (:state configuration')

     :op
     (:op
      (current-state configuration'))

     :expected
     (expected-event configuration')

     :outcome
     (:outcome configuration')

     :value-keys
     (set
      (keys
       (:values configuration')))

     :history-count
     (count
      (:history configuration'))

     :observable-count
     (count
      (:trace configuration'))}))
