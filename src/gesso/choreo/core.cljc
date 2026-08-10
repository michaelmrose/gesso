(ns gesso.choreo.core
  "Plain-data choreography model for Gesso.

   A choreography describes one distributed interaction globally. It names the
   participating roles, protocol states, communication between roles, local FX
   machines, authoritative choices, resource ownership, external waits and
   interrupts, and terminal outcomes.

   Local computation is deliberately represented as an :fx state. Choreography
   does not execute FX handlers itself. An endpoint runtime resolves the state's
   :machine id to the local FX implementation for that endpoint.

   This namespace deliberately does not:
   - verify choreography correctness
   - project role-local plans
   - execute FX machines or handlers
   - implement transport
   - know about DOM, HTMX, SSE, Ring, XTDB, Manifold, or Missionary
   - know application-specific protocol policy

   The representation is intentionally explicit. Verification and projection
   operate over ordinary inspectable Clojure data instead of reconstructing
   distributed control flow from an opaque DSL."
  (:refer-clojure :exclude [await send])
  (:require
   [clojure.set :as set]))

;; -----------------------------------------------------------------------------
;; Identity
;; -----------------------------------------------------------------------------

(def choreography-version
  "Version of the in-memory choreography representation.

   This is not an application wire-protocol version."
  1)

(def choreography-type
  :gesso.choreo/choreography)

(def state-ops
  "Operations understood by the choreography compiler.

   The verifier owns the detailed semantic rules for each operation."
  #{:fx
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
  #{:fx
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

(defn- require-fx-machine-id!
  [machine-id]
  (require-keyword! "Choreography FX machine id" machine-id))

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

;; -----------------------------------------------------------------------------
;; Choreography construction
;; -----------------------------------------------------------------------------

(defn ->choreography
  "Normalize a plain choreography map.

   Required semantic fields are intentionally not exhaustively validated here;
   gesso.choreo.verify owns semantic verification and graph-aware diagnostics.

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
       Events that can arise outside the controlled participant protocol, such
       as timeout, disconnect, or request failure.

     :metadata
       Optional compiler/development metadata. Projection may discard it."
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
                 :gesso.choreo/type choreography-type
                 :gesso.choreo/version choreography-version
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
  "True when x is a normalized Gesso choreography."
  [x]
  (and (map? x)
       (= choreography-type
          (:gesso.choreo/type x))
       (= choreography-version
          (:gesso.choreo/version x))))

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

(defn fx
  "Construct one role-local FX-machine state.

   :machine is a semantic FX machine id resolved by the endpoint runtime. The
   choreography layer neither implements FX nor assumes a platform-specific FX
   runner. A JVM endpoint may resolve the id to Biff FX while another endpoint
   may use a compatible local implementation.

   :next is the following choreography state.

   Optional :input is opaque data made available to the endpoint's local FX
   adapter. Optional :metadata is compiler/development metadata."
  ([role machine-id next]
   (fx role machine-id next nil))
  ([role machine-id next {:keys [input metadata] :as opts}]
   (require-map! "Choreography FX options" (or opts {}))
   (cond-> {:op :fx
            :role (require-role! role)
            :machine (require-fx-machine-id! machine-id)
            :next (require-state-id! next)}
     (contains? opts :input)
     (assoc :input input)

     (some? metadata)
     (assoc :metadata (normalize-metadata metadata)))))

(defn send
  "Construct one inter-role send state.

   The choreography declares communication semantically; endpoint integration
   maps :via to HTTP, SSE, a queue, or another transport.

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
   accepts. The verifier/projector pair communication and enforce the participant
   contract.

   Optional :bind names the execution-context key under which the received
   message value becomes available to later local FX and choice states."
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

   :key identifies a value in accumulated execution context that chooses a
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
  "Construct a role-local wait for one of several incoming/external events.

   :events maps event id -> next state id.

   This is intentionally distinct from receive:
   - receive describes a specific participant-to-participant message
   - await describes suspension on an event set, including environment events

   Optional :bind names the execution-context key under which the selected event
   value becomes available to later local FX and choice states."
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
   accumulated execution context to expose as the endpoint return value."
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
  "Return the participant that locally acts at state, when one role owns it.

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

   Detailed verifier/projector diagnostics belong in their namespaces."
  [choreography]
  (let [choreography' (ensure-choreography choreography)
        states (:states choreography')]
    {:name (:name choreography')
     :version (:gesso.choreo/version choreography')
     :roles (:roles choreography')
     :initial (:initial choreography')
     :state-count (count states)
     :states-by-op (frequencies (map (comp :op val) states))
     :resources (set (keys (:resources choreography')))
     :environment-events (:environment-events choreography')}))
