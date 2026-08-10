(ns gesso.live.choreo
  "Plain-data choreography model for Gesso Live.

   A choreography describes one interaction globally. It names the participating
   roles, the protocol states, communication between roles, local effects,
   authoritative choices, resource ownership, external waits/interrupts, and
   terminal outcomes.

   This namespace deliberately does not:
   - verify choreography correctness
   - project role-local plans
   - execute effects
   - know about DOM, HTMX, SSE, Ring, XTDB, Manifold, or Missionary
   - know optimistic-specific policy

   The representation is intentionally explicit. Verification and projection
   should operate over ordinary inspectable Clojure data instead of reconstructing
   control flow from an opaque DSL."
  (:refer-clojure :exclude [await send])
  (:require
   [clojure.set :as set]))

;; -----------------------------------------------------------------------------
;; Identity
;; -----------------------------------------------------------------------------

(def choreography-version
  "Version of the in-memory choreography representation.

   This is not a browser wire-protocol version."
  1)

(def choreography-type
  :gesso.live.choreo/choreography)

(def state-ops
  "Operations understood by the choreography compiler.

   The verifier owns the detailed semantic rules for each operation."
  #{:effect
    :send
    :receive
    :choice
    :await
    :acquire
    :release
    :goto
    :return})

(def terminal-ops
  #{:return})

(def communication-ops
  #{:send :receive})

(def role-local-ops
  #{:effect
    :choice
    :await
    :acquire
    :release
    :return})

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn- ex
  [message data]
  (ex-info message data))

(defn- require-map!
  [label x]
  (when-not (map? x)
    (throw
     (ex (str label " must be a map.")
         {:label label
          :value x})))
  x)

(defn- require-keyword!
  [label x]
  (when-not (keyword? x)
    (throw
     (ex (str label " must be a keyword.")
         {:label label
          :value x})))
  x)

(defn- require-role!
  [role]
  (require-keyword! "Choreography role" role))

(defn- require-state-id!
  [state-id]
  (require-keyword! "Choreography state id" state-id))

(defn- require-effect-id!
  [effect-id]
  (require-keyword! "Choreography effect id" effect-id))

(defn- require-event-id!
  [event-id]
  (require-keyword! "Choreography event id" event-id))

(defn- require-resource-id!
  [resource-id]
  (require-keyword! "Choreography resource id" resource-id))

(defn- normalize-role-set
  [roles]
  (cond
    (nil? roles)
    #{}

    (set? roles)
    roles

    (sequential? roles)
    (set roles)

    :else
    (throw
     (ex "Choreography :roles must be a set or sequential collection."
         {:roles roles}))))

(defn- normalize-state-map
  [states]
  (if (nil? states)
    {}
    (require-map! "Choreography :states" states)))

(defn- normalize-resource-map
  [resources]
  (if (nil? resources)
    {}
    (require-map! "Choreography :resources" resources)))

(defn- normalize-environment-events
  [events]
  (cond
    (nil? events)
    #{}

    (set? events)
    events

    (sequential? events)
    (set events)

    :else
    (throw
     (ex "Choreography :environment-events must be a set or sequential collection."
         {:environment-events events}))))

(defn- normalize-metadata
  [metadata]
  (if (nil? metadata)
    {}
    (require-map! "Choreography :metadata" metadata)))

(defn- normalize-branch-map
  [label branches]
  (let [branches' (require-map! label branches)]
    (into {}
          (map
           (fn [[branch target]]
             [branch (require-state-id! target)]))
          branches')))

(defn- normalize-interrupts
  [interrupts]
  (when (some? interrupts)
    (into {}
          (map
           (fn [[event target]]
             [(require-event-id! event)
              (require-state-id! target)]))
          (require-map! "Choreography :interrupts" interrupts))))

(defn- with-optional
  [m k value]
  (if (some? value)
    (assoc m k value)
    m))

;; -----------------------------------------------------------------------------
;; Choreography construction
;; -----------------------------------------------------------------------------

(defn ->choreography
  "Normalize a plain choreography map.

   Required semantic fields are intentionally not exhaustively validated here;
   gesso.live.choreo.verify owns semantic verification and should produce the
   useful graph-aware diagnostics.

   Recognized top-level fields:

     :name
       Choreography identifier.

     :roles
       Set or sequential collection of role keywords.

     :initial
       Initial state id.

     :states
       Map of state id -> state map.

     :resources
       Optional map of resource id -> descriptor.

     :environment-events
       Events that can arise from outside the controlled participant protocol,
       such as timeout, disconnect, or request failure.

     :metadata
       Optional compiler/development metadata. Runtime projection is free to
       discard it."
  [{:keys [name
           roles
           initial
           states
           resources
           environment-events
           metadata]
    :as choreography}]
  (require-map! "Choreography" choreography)
  (cond-> (assoc choreography
                 :gesso.live.choreo/type choreography-type
                 :gesso.live.choreo/version choreography-version
                 :roles (normalize-role-set roles)
                 :states (normalize-state-map states)
                 :resources (normalize-resource-map resources)
                 :environment-events
                 (normalize-environment-events environment-events)
                 :metadata (normalize-metadata metadata))
    (some? name)
    (assoc :name name)

    (some? initial)
    (assoc :initial initial)))

(defn choreography?
  "True when x is a normalized Gesso Live choreography."
  [x]
  (and (map? x)
       (= choreography-type
          (:gesso.live.choreo/type x))
       (= choreography-version
          (:gesso.live.choreo/version x))))

(defn ensure-choreography
  "Return a normalized choreography.

   Plain maps are normalized with ->choreography. Already-normalized
   choreographies are returned unchanged."
  [x]
  (if (choreography? x)
    x
    (->choreography x)))

;; -----------------------------------------------------------------------------
;; Resource descriptors
;; -----------------------------------------------------------------------------

(defn resource
  "Construct a resource descriptor.

   A resource is a protocol-level capability whose lifecycle may be checked by
   the verifier.

   Options:

     :owner
       Role that normally owns the resource. Optional because some resources may
       be acquired dynamically.

     :linear?
       Defaults to true. A linear resource may not be multiply acquired or
       released and must not leak through a required terminal path.

     :terminal-release?
       Defaults to true for linear resources. When true, terminal states may not
       retain the resource.

     :metadata
       Optional descriptive/compiler metadata."
  ([]
   (resource nil))
  ([{:keys [owner linear? terminal-release? metadata]
     :as opts}]
   (require-map! "Choreography resource options" (or opts {}))
   (let [linear?' (if (contains? opts :linear?)
                    (boolean linear?)
                    true)
         terminal-release?'
         (if (contains? opts :terminal-release?)
           (boolean terminal-release?)
           linear?')]
     (cond-> {:linear? linear?'
              :terminal-release? terminal-release?'
              :metadata (normalize-metadata metadata)}
       (some? owner)
       (assoc :owner (require-role! owner))))))

;; -----------------------------------------------------------------------------
;; State constructors
;; -----------------------------------------------------------------------------

(defn effect
  "Construct one role-local FX effect state.

   :effect is a semantic effect id interpreted by the participant runtime.
   :next is the following choreography state.

   Optional opts are opaque effect input data under :args plus compiler/runtime
   metadata under :metadata."
  ([role effect-id next]
   (effect role effect-id next nil))
  ([role effect-id next {:keys [args metadata] :as opts}]
   (require-map! "Choreography effect options" (or opts {}))
   (cond-> {:op :effect
            :role (require-role! role)
            :effect (require-effect-id! effect-id)
            :next (require-state-id! next)}
     (contains? opts :args)
     (assoc :args args)

     (some? metadata)
     (assoc :metadata (normalize-metadata metadata)))))

(defn send
  "Construct one inter-role send state.

   The choreography declares communication semantically; a later projection may
   map :via to HTTP, Live/SSE, client OOB, or another framework transport.

   Optional opts:

     :via
       Transport identifier.

     :required
       Set of required payload keys.

     :optional
       Set of optional payload keys.

     :correlation
       Set of payload keys required to correlate the message with a protocol
       execution or scope.

     :interrupts
       Map of external event -> state id. Interrupts represent environmental
       events that may become observable while this communication is outstanding,
       without pretending those events are produced by the remote participant.

     :metadata
       Optional descriptive/compiler metadata."
  ([from to event next]
   (send from to event next nil))
  ([from to event next
    {:keys [via required optional correlation interrupts metadata] :as opts}]
   (require-map! "Choreography send options" (or opts {}))
   (cond-> {:op :send
            :from (require-role! from)
            :to (require-role! to)
            :event (require-event-id! event)
            :next (require-state-id! next)}
     (some? via)
     (assoc :via via)

     (contains? opts :required)
     (assoc :required (set required))

     (contains? opts :optional)
     (assoc :optional (set optional))

     (contains? opts :correlation)
     (assoc :correlation (set correlation))

     (some? interrupts)
     (assoc :interrupts (normalize-interrupts interrupts))

     (some? metadata)
     (assoc :metadata (normalize-metadata metadata)))))

(defn receive
  "Construct one inter-role receive state.

   :from, :to, :event, and optional :via describe the communication this state
   accepts. The verifier/projector will pair communication and enforce the
   participant contract.

   Optional :bind names the context key under which the received event/message
   value should be made available to the projected FX machine."
  ([from to event next]
   (receive from to event next nil))
  ([from to event next {:keys [via bind metadata] :as opts}]
   (require-map! "Choreography receive options" (or opts {}))
   (cond-> {:op :receive
            :from (require-role! from)
            :to (require-role! to)
            :event (require-event-id! event)
            :next (require-state-id! next)}
     (some? via)
     (assoc :via via)

     (some? bind)
     (assoc :bind (require-keyword! "Choreography receive :bind" bind))

     (some? metadata)
     (assoc :metadata (normalize-metadata metadata)))))

(defn choice
  "Construct one role-owned authoritative choice.

   :key identifies the value in accumulated execution context that chooses a
   branch. :branches maps choice value -> next state id.

   The verifier/projector is responsible for ensuring that another participant
   whose behavior depends on this choice can actually learn it."
  ([role key branches]
   (choice role key branches nil))
  ([role key branches {:keys [metadata] :as opts}]
   (require-map! "Choreography choice options" (or opts {}))
   (cond-> {:op :choice
            :role (require-role! role)
            :key (require-keyword! "Choreography choice key" key)
            :branches (normalize-branch-map
                       "Choreography choice :branches"
                       branches)}
     (some? metadata)
     (assoc :metadata (normalize-metadata metadata)))))

(defn await
  "Construct a role-local wait for one of several external or incoming events.

   :events maps event id -> next state id.

   This is intentionally distinct from receive:
   - receive describes a specific participant-to-participant message
   - await describes suspension on an event set, including environmental events

   Optional :bind names the context key under which the selected event value is
   made available to the projected FX machine."
  ([role events]
   (await role events nil))
  ([role events {:keys [bind metadata] :as opts}]
   (require-map! "Choreography await options" (or opts {}))
   (let [events' (into {}
                       (map
                        (fn [[event target]]
                          [(require-event-id! event)
                           (require-state-id! target)]))
                       (require-map! "Choreography await :events" events))]
     (cond-> {:op :await
              :role (require-role! role)
              :events events'}
       (some? bind)
       (assoc :bind (require-keyword! "Choreography await :bind" bind))

       (some? metadata)
       (assoc :metadata (normalize-metadata metadata))))))

(defn acquire
  "Construct acquisition of one protocol resource by a role."
  ([role resource-id next]
   (acquire role resource-id next nil))
  ([role resource-id next {:keys [metadata] :as opts}]
   (require-map! "Choreography acquire options" (or opts {}))
   (cond-> {:op :acquire
            :role (require-role! role)
            :resource (require-resource-id! resource-id)
            :next (require-state-id! next)}
     (some? metadata)
     (assoc :metadata (normalize-metadata metadata)))))

(defn release
  "Construct release/consumption of one protocol resource by a role."
  ([role resource-id next]
   (release role resource-id next nil))
  ([role resource-id next {:keys [metadata] :as opts}]
   (require-map! "Choreography release options" (or opts {}))
   (cond-> {:op :release
            :role (require-role! role)
            :resource (require-resource-id! resource-id)
            :next (require-state-id! next)}
     (some? metadata)
     (assoc :metadata (normalize-metadata metadata)))))

(defn goto
  "Construct an unconditional compiler-level transition.

   goto has no participant owner and exists only to make explicit graph
   structure convenient."
  [next]
  {:op :goto
   :next (require-state-id! next)})

(defn return
  "Construct a terminal state owned by role.

   outcome is a semantic terminal disposition. Optional :value-key identifies
   accumulated context to expose as the machine return value."
  ([role outcome]
   (return role outcome nil))
  ([role outcome {:keys [value-key metadata] :as opts}]
   (require-map! "Choreography return options" (or opts {}))
   (cond-> {:op :return
            :role (require-role! role)
            :outcome outcome}
     (some? value-key)
     (assoc :value-key
            (require-keyword! "Choreography return :value-key" value-key))

     (some? metadata)
     (assoc :metadata (normalize-metadata metadata)))))

;; -----------------------------------------------------------------------------
;; Graph inspection
;; -----------------------------------------------------------------------------

(defn state
  "Return state-id from choreography, or nil when absent."
  [choreography state-id]
  (get (:states (ensure-choreography choreography))
       state-id))

(defn state-op
  "Return a state's operation keyword."
  [state]
  (:op state))

(defn terminal-state?
  "True when state is terminal according to the core choreography model."
  [state]
  (contains? terminal-ops (state-op state)))

(defn communication-state?
  "True for send/receive states."
  [state]
  (contains? communication-ops (state-op state)))

(defn state-role
  "Return the participant that locally acts at state, when exactly one role owns
   the local action.

   For send, the acting role is :from.
   For receive, the acting role is :to.
   goto has no role."
  [state]
  (case (state-op state)
    :send (:from state)
    :receive (:to state)
    :goto nil
    (:role state)))

(defn branch-successors
  "Return branch/event successors from a state without including :next."
  [state]
  (case (state-op state)
    :choice (set (vals (:branches state)))
    :await (set (vals (:events state)))
    #{}))

(defn interrupt-successors
  "Return external interrupt successors declared on state."
  [state]
  (set (vals (:interrupts state))))

(defn successors
  "Return the set of all state ids directly reachable from state.

   The verifier uses this as graph structure only. It does not imply that all
   successor kinds have identical runtime semantics."
  [state]
  (let [next-state (when (contains? state :next)
                     #{(:next state)})]
    (-> #{}
        (set/union (or next-state #{}))
        (set/union (branch-successors state))
        (set/union (interrupt-successors state))
        (disj nil))))

(defn state-ids
  "Return all declared choreography state ids."
  [choreography]
  (set (keys (:states (ensure-choreography choreography)))))

(defn roles
  "Return declared choreography roles."
  [choreography]
  (:roles (ensure-choreography choreography)))

(defn resource-descriptors
  "Return declared resource descriptors."
  [choreography]
  (:resources (ensure-choreography choreography)))

(defn environment-events
  "Return events declared as environment-produced."
  [choreography]
  (:environment-events (ensure-choreography choreography)))

(defn explain
  "Return a small, stable summary suitable for REPL inspection.

   Detailed verifier/projector diagnostics belong in their respective
   namespaces."
  [choreography]
  (let [choreography' (ensure-choreography choreography)
        states (:states choreography')]
    {:name (:name choreography')
     :version (:gesso.live.choreo/version choreography')
     :roles (:roles choreography')
     :initial (:initial choreography')
     :state-count (count states)
     :states-by-op (frequencies (map (comp :op val) states))
     :resources (set (keys (:resources choreography')))
     :environment-events (:environment-events choreography')}))
