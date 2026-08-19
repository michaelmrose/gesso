(ns gesso.choreo.core
  "Authoring API for the current v4.5 Gesso Choreo semantic core.

   This namespace describes global choreographies as ordinary Clojure data and
   delegates executable semantic validation to gesso.choreo.semantics.

   The current language is deliberately small:

   :local
     Genuinely role-local work. A local action may declare semantic values it
     requires and a closed set of semantic values it produces. Local actions
     are not distributed observations.

   :authoritative
     Invocation/result of one public authoritative semantic operation. It may
     declare semantic inputs and a closed set of authoritative outputs. The
     operation name identifies the public semantic operation, not an internal
     Biff FX machine, XTDB transaction function, route, or transport handler.

   :branch
     Deterministic role-local control flow selected by one previously
     established semantic value.

   :communicate
     One atomic semantic participant-to-participant communication. Separate
     send and receive states are projected realization details, not global
     choreography operations.

   :await
     A role-local event produced by that role's environment. Participant
     messages do not satisfy :await.

   :return
     Global terminal outcome. Terminality is not owned by one participant.

   The value store used by local/authoritative actions and branches is not yet a
   knowledge model. Declaring that a state belongs to a role does not by itself
   prove that role knows every value it requires. That becomes a verifier/proof
   obligation when the knowledge/provenance layer is introduced.

   Likewise, :authoritative identifies semantic authority but does not itself
   authenticate a principal, grant permission, or prescribe persistence. Its
   trusted realization must invoke the public model boundary and return only the
   declared semantic outputs.

   This file intentionally does not preserve the old Choreo API. In particular
   it does not define the old :fx, paired :send/:receive, :choice,
   :acquire/:release, or :goto vocabulary. Those ideas may return in different
   semantic forms when real requirements justify them.

   Metadata is retained as authoring/compiler data but has no semantic effect."
  (:refer-clojure :exclude [await])
  (:require
   [clojure.set :as set]
   [gesso.choreo.semantics :as semantics]))

;; -----------------------------------------------------------------------------
;; Identity
;; -----------------------------------------------------------------------------

(def choreography-version
  semantics/semantics-version)

(def choreography-type
  semantics/program-type)

(def state-ops
  semantics/supported-ops)

(def terminal-ops
  #{:return})

(def communication-ops
  #{:communicate})

;; -----------------------------------------------------------------------------
;; Errors / small normalization helpers
;; -----------------------------------------------------------------------------

(defn- fail!
  [kind message data]
  (throw
   (ex-info
    message
    (merge
     {:error/type :gesso.choreo.core/error
      :error/kind kind}
     data))))

(defn- require-map!
  [label value]
  (when-not (map? value)
    (fail!
     :invalid-value
     (str label " must be a map.")
     {:label label
      :value value}))
  value)

(defn- require-keyword!
  [label value]
  (when-not (keyword? value)
    (fail!
     :invalid-value
     (str label " must be a keyword.")
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

(defn- normalize-metadata
  [metadata]
  (cond
    (nil? metadata)
    {}

    (map? metadata)
    metadata

    :else
    (fail!
     :invalid-metadata
     "Choreography metadata must be a map."
     {:metadata metadata})))

(defn- action-options
  [label options]
  (require-map!
   label
   (or options {}))

  (let [{:keys [requires outputs metadata]
         :or {requires #{}
              outputs #{}}}
        (or options {})]

    {:requires
     (require-keyword-set!
      (str label " :requires")
      requires)

     :outputs
     (require-keyword-set!
      (str label " :outputs")
      outputs)

     :metadata
     (when (some? metadata)
       (normalize-metadata
        metadata))}))

(defn- with-action-contract
  [state {:keys [requires outputs metadata]}]
  (cond-> state
    (seq requires)
    (assoc :requires requires)

    (seq outputs)
    (assoc :outputs outputs)

    (some? metadata)
    (assoc :metadata metadata)))

;; -----------------------------------------------------------------------------
;; State constructors
;; -----------------------------------------------------------------------------

(defn local
  "Construct one genuinely local participant action.

   A :local action contributes to semantic history but is hidden from the
   distributed observable trace.

   Options:

     :requires
       Set of semantic value keys that must already exist before the action may
       occur. Defaults to #{}.

     :outputs
       Closed set of semantic value keys the action must produce. At runtime a
       local event must return exactly this key set. Defaults to #{}.

     :metadata
       Compiler/development metadata with no semantic effect.

   :local must not be used to hide an authoritative model transition or another
   externally significant operation merely because that work happens on one
   host."
  ([role action next]
   (local role action next nil))
  ([role action next options]
   (with-action-contract
    {:op :local
     :role
     (require-keyword!
      "Local role"
      role)
     :action
     (require-keyword!
      "Local action"
      action)
     :next next}
    (action-options
     "Local options"
     options))))

(defn authoritative
  "Construct one public authoritative semantic operation.

   role is the trusted role that realizes the authority boundary.

   operation is the public semantic operation identity, for example:

     :request/claim
     :membership/approve
     :organization/close-location

   It must not name an implementation detail such as:

     :request.fx/claim
     :execute-tx
     :http/post
     :optimistic/execute-command

   Options:

     :requires
       Set of semantic value keys the authoritative operation requires.

     :outputs
       Closed set of semantic value keys established by the authoritative
       operation result.

     :metadata
       Compiler/development metadata with no semantic effect.

   The semantic operation's implementation remains responsible for
   authentication, authorization, authoritative reread/revalidation, atomic
   persistence where required, and the meaning of its result. Merely authoring
   an :authoritative state confers no authority."
  ([role operation next]
   (authoritative
    role
    operation
    next
    nil))
  ([role operation next options]
   (with-action-contract
    {:op :authoritative
     :role
     (require-keyword!
      "Authoritative role"
      role)
     :operation
     (require-keyword!
      "Authoritative operation"
      operation)
     :next next}
    (action-options
     "Authoritative options"
     options))))

(defn branch
  "Construct deterministic role-local control flow.

   on is the semantic value key that selects the continuation.
   cases maps concrete semantic values to successor state ids.

   A branch does not compute, fetch, or establish its selector. It may execute
   only when the semantic value already exists. Later knowledge verification
   must additionally establish that role is permitted to know/use that value.

   nil case values are rejected by the semantic layer because absence of a value
   and a concrete branch value must remain distinct.

   Optional :metadata has no semantic effect."
  ([role on cases]
   (branch role on cases nil))
  ([role on cases {:keys [metadata] :as options}]
   (require-map!
    "Branch options"
    (or options {}))

   (require-map!
    "Branch cases"
    cases)

   (when (empty? cases)
     (fail!
      :empty-branch
      "Branch requires at least one case."
      {:role role
       :on on}))

   (when (contains? cases nil)
     (fail!
      :invalid-branch-value
      "Branch case values may not be nil."
      {:role role
       :on on
       :cases cases}))

   (cond->
    {:op :branch
     :role
     (require-keyword!
      "Branch role"
      role)
     :on
     (require-keyword!
      "Branch value key"
      on)
     :cases cases}

     (some? metadata)
     (assoc
      :metadata
      (normalize-metadata metadata)))))

(defn communicate
  "Construct one atomic semantic communication from from-role to to-role.

   The global choreography records one semantic communication occurrence. It
   does not split that occurrence into separate sender and receiver states.
   Projection determines what each endpoint must do locally to realize it.

   Payload remains uninterpreted at this stage. Closed payload contracts and
   communication-derived knowledge are intentionally separate later work.

   Options:

     :via
       Optional semantic channel identifier.

     :metadata
       Compiler/development metadata with no semantic effect."
  ([from-role to-role event next]
   (communicate
    from-role
    to-role
    event
    next
    nil))
  ([from-role to-role event next {:keys [via metadata] :as options}]
   (require-map!
    "Communication options"
    (or options {}))

   (let [from-role'
         (require-keyword!
          "Communication from-role"
          from-role)

         to-role'
         (require-keyword!
          "Communication to-role"
          to-role)]

     (when (= from-role'
              to-role')
       (fail!
        :same-role-communication
        "Communication must cross roles."
        {:role from-role'}))

     (cond->
      {:op :communicate
       :from from-role'
       :to to-role'
       :event
       (require-keyword!
        "Communication event"
        event)
       :next next}

       (some? via)
       (assoc
        :via
        (require-keyword!
         "Communication via"
         via))

       (some? metadata)
       (assoc
        :metadata
        (normalize-metadata metadata))))))

(defn await
  "Construct a role-local wait for one of several environment events.

   events maps environment-event keyword -> successor state id.

   Participant communication is deliberately not represented here. A message
   from another role cannot satisfy this state merely because it uses the same
   event keyword.

   Optional :metadata is compiler/development metadata only."
  ([role events]
   (await role events nil))
  ([role events {:keys [metadata] :as options}]
   (require-map!
    "Await options"
    (or options {}))

   (require-map!
    "Await events"
    events)

   (when (empty? events)
     (fail!
      :empty-await
      "Await requires at least one environment event."
      {:role role}))

   (doseq [[event _target] events]
     (require-keyword!
      "Await event"
      event))

   (cond->
    {:op :await
     :role
     (require-keyword!
      "Await role"
      role)
     :events events}

     (some? metadata)
     (assoc
      :metadata
      (normalize-metadata metadata)))))

(defn return
  "Construct one global terminal outcome.

   Terminality belongs to the global choreography rather than to one role.
   Projection decides when a local endpoint has no further protocol work.

   outcome is a semantic terminal disposition keyword."
  ([outcome]
   (return outcome nil))
  ([outcome {:keys [metadata] :as options}]
   (require-map!
    "Return options"
    (or options {}))

   (cond->
    {:op :return
     :outcome
     (require-keyword!
      "Return outcome"
      outcome)}

     (some? metadata)
     (assoc
      :metadata
      (normalize-metadata metadata)))))

;; -----------------------------------------------------------------------------
;; Choreography construction
;; -----------------------------------------------------------------------------

(defn ->choreography
  "Construct and validate one choreography against the current semantic core.

   Recognized authoring fields:

     :name
       Optional choreography keyword.

     :initial
       Initial state id. State ids are opaque EDN values.

     :states
       Map state-id -> semantic state.

     :metadata
       Optional compiler/development metadata with no semantic effect.

   Additional top-level fields may be retained for current compiler inputs such
   as explicit entry-value assumptions, but they acquire no semantic meaning
   merely by being present."
  [{:keys [name metadata] :as choreography}]
  (require-map!
   "Choreography"
   choreography)

  (when (and (some? name)
             (not (keyword? name)))
    (fail!
     :invalid-name
     "Choreography :name must be a keyword when present."
     {:name name}))

  (semantics/->program
   (cond-> choreography
     (some? metadata)
     (assoc
      :metadata
      (normalize-metadata metadata)))))

(defn choreography?
  "True when x is normalized for the current semantic version."
  [x]
  (semantics/program? x))

(defn ensure-choreography
  "Normalize and validate choreography."
  [x]
  (semantics/ensure-program x))

;; -----------------------------------------------------------------------------
;; Graph / state inspection
;; -----------------------------------------------------------------------------

(defn state
  "Return state-id from choreography, or nil when absent."
  [choreography state-id]
  (get (:states
        (ensure-choreography choreography))
       state-id))

(defn state-op
  "Return a state's semantic operation."
  [state]
  (:op state))

(defn terminal-state?
  "True when state is globally terminal."
  [state]
  (= :return
     (state-op state)))

(defn communication-state?
  "True when state is a semantic participant communication."
  [state]
  (= :communicate
     (state-op state)))

(defn local-state?
  "True when state is a genuinely local semantic action."
  [state]
  (= :local
     (state-op state)))

(defn authoritative-state?
  "True when state is a public authoritative semantic operation."
  [state]
  (= :authoritative
     (state-op state)))

(defn branch-state?
  "True when state is deterministic role-local semantic branching."
  [state]
  (= :branch
     (state-op state)))

(defn state-roles
  "Return roles directly participating in state.

   :local, :authoritative, :branch, and :await involve one role.
   :communicate involves sender and receiver.
   :return has no directly acting role."
  [state]
  (case (state-op state)
    :local
    #{(:role state)}

    :authoritative
    #{(:role state)}

    :branch
    #{(:role state)}

    :communicate
    #{(:from state)
      (:to state)}

    :await
    #{(:role state)}

    :return
    #{}

    #{}))

(defn state-owner
  "Return the single role locally responsible for state, or nil.

   :authoritative has a single trusted realization role at this semantic layer.
   Communication intentionally has no single owner globally."
  [state]
  (case (state-op state)
    :local
    (:role state)

    :authoritative
    (:role state)

    :branch
    (:role state)

    :await
    (:role state)

    nil))

(defn successors
  "Return all direct global successor state ids."
  [state]
  (case (state-op state)
    :local
    #{(:next state)}

    :authoritative
    #{(:next state)}

    :branch
    (set
     (vals
      (:cases state)))

    :communicate
    #{(:next state)}

    :await
    (set
     (vals
      (:events state)))

    :return
    #{}

    #{}))

(defn action-requires
  "Return the declared semantic input keys of a local or authoritative action."
  [state]
  (if (contains?
       #{:local :authoritative}
       (state-op state))
    (or (:requires state)
        #{})
    #{}))

(defn action-outputs
  "Return the declared semantic output keys of a local or authoritative action."
  [state]
  (if (contains?
       #{:local :authoritative}
       (state-op state))
    (or (:outputs state)
        #{})
    #{}))

(defn local-requires
  "Return the declared semantic input keys of a :local state."
  [state]
  (if (local-state? state)
    (action-requires state)
    #{}))

(defn local-outputs
  "Return the declared semantic output keys of a :local state."
  [state]
  (if (local-state? state)
    (action-outputs state)
    #{}))

(defn authoritative-requires
  "Return the declared semantic input keys of an :authoritative state."
  [state]
  (if (authoritative-state? state)
    (action-requires state)
    #{}))

(defn authoritative-outputs
  "Return the declared semantic output keys of an :authoritative state."
  [state]
  (if (authoritative-state? state)
    (action-outputs state)
    #{}))

(defn authoritative-operation
  "Return the public semantic operation identity of an :authoritative state."
  [state]
  (when (authoritative-state? state)
    (:operation state)))

(defn branch-key
  "Return the semantic selector key of a :branch state, otherwise nil."
  [state]
  (when (branch-state? state)
    (:on state)))

(defn state-ids
  "Return all declared state ids."
  [choreography]
  (set
   (keys
    (:states
     (ensure-choreography choreography)))))

(defn roles
  "Infer roles directly mentioned by choreography states.

   Rich role declarations can be introduced later when the type/authority model
   gives them information that cannot be inferred from control flow."
  [choreography]
  (reduce
   set/union
   #{}
   (map
    state-roles
    (vals
     (:states
      (ensure-choreography choreography))))))

(defn explain
  "Return a compact stable summary for REPL inspection."
  [choreography]
  (let [choreography'
        (ensure-choreography choreography)

        states
        (:states choreography')]

    {:name
     (:name choreography')

     :version
     (:gesso.choreo.semantics/version
      choreography')

     :roles
     (roles choreography')

     :initial
     (:initial choreography')

     :state-count
     (count states)

     :states-by-op
     (frequencies
      (map
       (comp :op val)
       states))}))
