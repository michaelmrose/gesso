(ns gesso.choreo.realization
  "Deterministic multi-role realization harness for projected Gesso Choreo.

   This namespace is the first place where independently projected role-local
   machines are executed together. It deliberately remains portable CLJC and
   contains no browser, HTMX, transport, XTDB, optimism, threads, timers, or
   wall-clock behavior.

   A realization owns:

     - one verified global choreography;
     - one projected plan and one independent machine execution per role;
     - an explicit in-memory transport queue containing only messages actually
       emitted by projected sender machines;
     - deterministic realization history.

   The harness does NOT automatically choose local results, authoritative
   results, optional message fields, environment events, or message delivery
   order. Those choices are exactly the things tests and later adapters must be
   able to control.

   Typical operation is therefore explicit:

     complete-local
       Supply the declared result of one role-local action.

     complete-authoritative
       Supply the declared result of one trusted authoritative boundary.

     complete-send
       Supply the payload selected by the endpoint. The sender machine validates
       it against its projected message contract and role-local knowledge, then
       the exact envelope emitted by machine/complete-send is queued.

     deliver-message
       Deliver one queued envelope to its declared receiver. Delivery order is
       selected by message id rather than queue position so tests may reorder
       messages without modifying them.

     environment
       Supply one role-local environment event. The projected machine validates
       declared semantic event data; undeclared data admitted by an explicitly
       open event contract remains nonsemantic adapter data.

   Transport fault experiments are explicit too: queued messages may be dropped
   or duplicated. Duplication copies an envelope that was genuinely emitted by
   a sender; this namespace intentionally exposes no arbitrary message-forging
   API.

   This is a realization harness, not yet a refinement proof. Its purpose is to
   make the composition of verify -> project -> independent machines executable
   and inspectable so correspondence properties can be tested against the global
   semantics in a separate layer."
  (:require
   [clojure.set :as set]
   [gesso.choreo.machine :as machine]
   [gesso.choreo.project :as project]
   [gesso.choreo.semantics :as semantics]
   [gesso.choreo.verify :as verify]))

;; -----------------------------------------------------------------------------
;; Identity
;; -----------------------------------------------------------------------------

(def realization-type
  :gesso.choreo.realization/realization)

(def realization-version
  1)

;; -----------------------------------------------------------------------------
;; Errors
;; -----------------------------------------------------------------------------

(defn- realization-error
  [kind message data]
  (throw
   (ex-info
    message
    (merge
     {:error/type :gesso.choreo.realization/error
      :error/kind kind}
     data))))

;; -----------------------------------------------------------------------------
;; Validation helpers
;; -----------------------------------------------------------------------------

(defn- require-map!
  [label value]
  (when-not (map? value)
    (realization-error
     :invalid-value
     (str label " must be a map.")
     {:label label
      :value value}))
  value)

(defn- require-role!
  [role]
  (when-not (keyword? role)
    (realization-error
     :invalid-role
     "Realization role must be a keyword."
     {:role role}))
  role)

(defn- require-nonnegative-integer!
  [label value]
  (when-not (and (integer? value)
                 (not (neg? value)))
    (realization-error
     :invalid-value
     (str label " must be a non-negative integer.")
     {:label label
      :value value}))
  value)

(defn- role-map!
  [label roles value]
  (let [value'
        (require-map!
         label
         (or value {}))

        unknown
        (set/difference
         (set (keys value'))
         roles)]

    (when (seq unknown)
      (realization-error
       :unknown-role
       (str label " contains roles not present in the choreography.")
       {:label label
        :unknown-roles unknown
        :roles roles}))

    value'))

(defn- entry-knowledge-from-values
  [entry-values-by-role]
  (into
   {}
   (keep
    (fn [[role values]]
      (let [values'
            (require-map!
             "Role entry values"
             values)

            keys'
            (set (keys values'))]
        (when (seq keys')
          [role keys']))))
   entry-values-by-role))

(defn- verified-entry-knowledge
  [verified]
  (or
   (get-in verified
           [:verification
            :analysis
            :entry-knowledge])
   {}))

(defn- require-verified-entry-values!
  [verified entry-values-by-role]
  (let [assumed
        (verified-entry-knowledge verified)

        missing
        (into
         {}
         (keep
          (fn [[role required-keys]]
            (let [actual-keys
                  (set
                   (keys
                    (get entry-values-by-role
                         role
                         {})))

                  missing-keys
                  (set/difference
                   required-keys
                   actual-keys)]
              (when (seq missing-keys)
                [role missing-keys]))))
         assumed)]

    (when (seq missing)
      (realization-error
       :missing-entry-values
       "Concrete realization entry values do not satisfy the verified entry-knowledge assumptions."
       {:missing-by-role missing
        :assumed-entry-knowledge assumed
        :actual-entry-value-keys
        (into {}
              (map
               (fn [[role values]]
                 [role (set (keys values))]))
              entry-values-by-role)})))

  verified)

(defn- verified-artifact?
  [value]
  (or (verify/verified? value)
      (verify/verification? value)))

(defn- prepare-verified
  [choreography-or-verified entry-values-by-role]
  (if (verified-artifact?
       choreography-or-verified)
    (-> choreography-or-verified
        verify/ensure-verified
        (require-verified-entry-values!
         entry-values-by-role))

    (verify/ensure-verified
     choreography-or-verified
     {:entry-knowledge
      (entry-knowledge-from-values
       entry-values-by-role)})))

(defn realization?
  "True when value has the complete shallow shape of a realization state."
  [value]
  (and
   (map? value)
   (= realization-type
      (:gesso.choreo.realization/type value))
   (= realization-version
      (:gesso.choreo.realization/version value))
   (verify/verified?
    (:verified value))
   (map? (:plans value))
   (map? (:executions value))
   (= (set (keys (:plans value)))
      (set (keys (:executions value))))
   (every? project/projected-plan?
           (vals (:plans value)))
   (every? machine/execution?
           (vals (:executions value)))
   (vector? (:messages value))
   (vector? (:history value))
   (integer? (:next-message-id value))
   (not (neg? (:next-message-id value)))))

(defn- require-realization!
  [realization]
  (when-not (realization? realization)
    (realization-error
     :invalid-realization
     "Expected a Gesso choreography realization state."
     {:realization realization}))
  realization)

;; -----------------------------------------------------------------------------
;; Start
;; -----------------------------------------------------------------------------

(defn start
  "Start one independent projected machine for every choreography role.

   Options:

     :entry-values-by-role
       Map of role -> semantic value map. For plain choreography input these
       concrete values also become the verifier's precise :entry-knowledge
       assumptions. For an existing verified artifact they must satisfy every
       entry-knowledge assumption already recorded by that artifact.

     :machine-options-by-role
       Map of role -> options passed to machine/start. :values is deliberately
       forbidden here; :entry-values-by-role is the single source of concrete
       role-local entry values. Identity bindings, command-id, execution-id, and
       max-immediate-steps remain available through the machine options.

   The result contains independent machine executions and an empty explicit
   transport queue. No endpoint boundary is performed automatically."
  ([choreography-or-verified]
   (start choreography-or-verified nil))
  ([choreography-or-verified
    {:keys [entry-values-by-role
            machine-options-by-role]
     :or {entry-values-by-role {}
          machine-options-by-role {}}}]
   (let [entry-values-by-role'
         (require-map!
          "Realization :entry-values-by-role"
          entry-values-by-role)

         verified
         (prepare-verified
          choreography-or-verified
          entry-values-by-role')

         plans
         (project/project-all
          verified)

         roles
         (set (keys plans))

         entry-values-by-role''
         (role-map!
          "Realization :entry-values-by-role"
          roles
          entry-values-by-role')

         machine-options-by-role'
         (role-map!
          "Realization :machine-options-by-role"
          roles
          machine-options-by-role)

         executions
         (into
          {}
          (map
           (fn [role]
             (let [machine-options
                   (require-map!
                    "Role machine options"
                    (get machine-options-by-role'
                         role
                         {}))]

               (when (contains? machine-options
                                :values)
                 (realization-error
                  :duplicate-entry-values
                  "Role machine options may not contain :values; use :entry-values-by-role."
                  {:role role
                   :machine-options machine-options}))

               [role
                (machine/start
                 (get plans role)
                 (assoc machine-options
                        :values
                        (get entry-values-by-role''
                             role
                             {})))])))
          (sort-by pr-str roles))]

     {:gesso.choreo.realization/type
      realization-type

      :gesso.choreo.realization/version
      realization-version

      :verified
      verified

      :plans
      plans

      :executions
      executions

      :messages
      []

      :next-message-id
      0

      :history
      []})))

;; -----------------------------------------------------------------------------
;; Inspection
;; -----------------------------------------------------------------------------

(defn roles
  "Return the set of independently executing roles."
  [realization]
  (set
   (keys
    (:executions
     (require-realization!
      realization)))))

(defn execution
  "Return one role-local execution, or nil when role is not present."
  [realization role]
  (get (:executions
        (require-realization!
         realization))
       (require-role! role)))

(defn plan
  "Return one role-local projected plan, or nil when role is not present."
  [realization role]
  (get (:plans
        (require-realization!
         realization))
       (require-role! role)))

(defn history
  "Return deterministic realization-level scheduling/transport history."
  [realization]
  (:history
   (require-realization!
    realization)))

(defn distributed-observation
  "Project one realization-history entry into the canonical distributed
   observation alphabet owned by gesso.choreo.semantics.

   The projected realization history contains both semantic occurrences and
   physical scheduling/transport occurrences. Only trusted authoritative
   completion and receiver consumption of one genuinely sender-emitted
   participant message contribute distributed observations.

   :local and :environment are semantic but distributed-hidden. :send, :drop,
   and :duplicate are physical realization operations and are also hidden:
   communication becomes the atomic semantic distributed occurrence only when
   the receiver consumes the emitted envelope.

   Projected role-local completion is deliberately not synthesized here as a
   global :terminal observation. A local projected :return means only that this
   role has no further protocol work; it does not establish which global return
   outcome occurred. Terminal compatibility remains a separate projection/proof
   obligation.

   Canonical observation shape is delegated to semantics/distributed-observation
   rather than redefined here. Unknown realization-history kinds fail closed so
   adding a new occurrence forces an explicit observability decision."
  [entry]
  (require-map!
   "Realization history entry"
   entry)

  (case (:kind entry)
    :authoritative
    (semantics/distributed-observation
     entry)

    :deliver
    (let [message
          (:message entry)]
      (when-not (map? message)
        (realization-error
         :invalid-delivery-history
         "Realization delivery history must retain the consumed participant-message envelope."
         {:entry entry}))

      (semantics/distributed-observation
       (cond->
        {:kind :communication
         :from (:from message)
         :to (:to message)
         :event (:event message)
         :payload (:payload message)}
         (contains? message :via)
         (assoc
          :via
          (:via message)))))

    ;; Distributed-hidden semantic realization occurrences.
    :local
    nil

    :environment
    nil

    ;; Physical realization operations. A send is not yet the semantic atomic
    ;; communication because the receiver has not consumed it.
    :send
    nil

    :drop
    nil

    :duplicate
    nil

    (realization-error
     :unknown-history-kind
     "Realization history kind has no declared distributed-observation semantics."
     {:kind (:kind entry)
      :entry entry
      :known-kinds
      #{:local
        :authoritative
        :send
        :deliver
        :drop
        :duplicate
        :environment}})))

(defn distributed-observable-trace
  "Return the canonical distributed observations produced so far by this
   independently projected realization.

   The result inhabits the same authoritative/communication observation shapes
   as semantics/distributed-observable-trace, contains no semantic source-state
   ids or realization transport diagnostics, and deliberately does not invent a
   global terminal outcome from role-local completion."
  [realization]
  (into
   []
   (keep distributed-observation)
   (history realization)))

(defn messages
  "Return queued transport entries in enqueue order.

   Each entry has :message-id and :message. A duplicate additionally records
   :origin-message-id."
  [realization]
  (:messages
   (require-realization!
    realization)))

(defn message-count
  [realization]
  (count
   (messages realization)))

(defn queued-message
  "Return one queued transport entry by stable message id, or nil."
  [realization message-id]
  (require-nonnegative-integer!
   "Realization message id"
   message-id)
  (some
   #(when (= message-id
             (:message-id %))
      %)
   (messages realization)))

(defn- root-message-id
  [entry]
  (or (:origin-message-id entry)
      (:message-id entry)))

(defn- delivered-root-message-ids
  [realization]
  (into
   #{}
   (keep
    (fn [entry]
      (when (= :deliver (:kind entry))
        (root-message-id entry))))
   (:history realization)))

(defn- delivered-root-message?
  [realization entry]
  (contains?
   (delivered-root-message-ids realization)
   (root-message-id entry)))

(defn boundary
  "Return one role's current explicit endpoint boundary, or nil.

   Only :local, :authoritative, and :send are endpoint boundaries. Receive and
   environment waits are suspension points and completed roles have no boundary."
  [realization role]
  (some-> (execution realization role)
          machine/pending-action))

(defn boundaries
  "Return role -> current endpoint boundary for every runnable role."
  [realization]
  (let [realization'
        (require-realization!
         realization)]
    (into
     {}
     (keep
      (fn [[role execution]]
        (when-some [action
                    (machine/pending-action
                     execution)]
          [role action])))
     (:executions realization'))))

(defn role-completed?
  [realization role]
  (boolean
   (when-some [execution'
               (execution realization role)]
     (machine/completed?
      execution'))))

(defn roles-completed?
  "True when every role-local machine has completed, regardless of queued
   transport messages."
  [realization]
  (let [realization'
        (require-realization!
         realization)]
    (every?
     machine/completed?
     (vals
      (:executions realization')))))

(defn completed?
  "True when every role is complete and no emitted message remains queued."
  [realization]
  (let [realization'
        (require-realization!
         realization)]
    (and
     (roles-completed?
      realization')
     (empty?
      (:messages realization')))))

(defn deliverable-message-ids
  "Return queued message ids currently accepted by their declared receiver.

   A queued message may be temporarily undeliverable because its receiver has
   not yet reached the corresponding receive gate. A duplicate whose root
   emission has already been delivered is permanently stale and is never
   reported as deliverable, even if a later receive gate has an identical
   message contract."
  [realization]
  (let [realization'
        (require-realization!
         realization)]
    (->> (:messages realization')
         (keep
          (fn [{:keys [message-id message]
                :as entry}]
            (when-some [receiver
                        (get (:executions realization')
                             (:to message))]
              (when (and
                     (not
                      (delivered-root-message?
                       realization'
                       entry))
                     (machine/accepts-message?
                      receiver
                      message))
                message-id))))
         vec)))

(defn waiting-environment-roles
  "Return roles currently suspended on local environment events."
  [realization]
  (let [realization'
        (require-realization!
         realization)]
    (set
     (keep
      (fn [[role execution]]
        (when (machine/waiting-environment?
               execution)
          role))
      (:executions realization')))))

(defn quiescent?
  "True when no synchronous endpoint boundary or currently deliverable queued
   message can advance the realization.

   Quiescence is not completion: the realization may legitimately be waiting on
   an environment event, or it may expose a deadlock/problem for later tests to
   diagnose."
  [realization]
  (let [realization'
        (require-realization!
         realization)]
    (and
     (empty?
      (boundaries realization'))
     (empty?
      (deliverable-message-ids
       realization')))))

;; -----------------------------------------------------------------------------
;; Internal state updates
;; -----------------------------------------------------------------------------

(defn- require-role-execution!
  [realization role]
  (or
   (execution realization role)
   (realization-error
    :unknown-role
    "Realization does not contain the requested role."
    {:role role
     :roles (roles realization)})))

(defn- update-execution
  [realization role next-execution history-entry]
  (-> realization
      (assoc-in
       [:executions role]
       next-execution)
      (update
       :history
       conj
       history-entry)))

(defn- next-message-id
  [realization]
  (:next-message-id
   (require-realization!
    realization)))

(defn- enqueue
  [realization message entry-data]
  (let [message-id
        (next-message-id realization)

        entry
        (merge
         {:message-id message-id
          :message message}
         entry-data)]
    [(-> realization
         (update :messages conj entry)
         (update :next-message-id inc))
     entry]))

(defn- remove-queued-message
  [realization message-id]
  (update
   realization
   :messages
   (fn [entries]
     (into
      []
      (remove
       #(= message-id
           (:message-id %)))
      entries))))

;; -----------------------------------------------------------------------------
;; Explicit endpoint boundaries
;; -----------------------------------------------------------------------------

(defn complete-local
  "Complete one role's current local action with exactly its declared outputs."
  ([realization role]
   (complete-local realization role {}))
  ([realization role outputs]
   (let [realization'
         (require-realization!
          realization)

         role'
         (require-role! role)

         current
         (require-role-execution!
          realization'
          role')

         state-id
         (machine/current-state-id
          current)

         action
         (machine/pending-action
          current)

         next-execution
         (machine/complete-local
          current
          outputs)]

     (update-execution
      realization'
      role'
      next-execution
      {:kind :local
       :role role'
       :state state-id
       :action (:action action)
       :outputs outputs}))))

(defn complete-authoritative
  "Complete one role's trusted authoritative boundary with declared outputs.

   This harness does not confer authority. The caller is the trusted premise
   supplying the result of the named public operation."
  ([realization role]
   (complete-authoritative
    realization
    role
    {}))
  ([realization role outputs]
   (let [realization'
         (require-realization!
          realization)

         role'
         (require-role! role)

         current
         (require-role-execution!
          realization'
          role')

         state-id
         (machine/current-state-id
          current)

         action
         (machine/pending-action
          current)

         next-execution
         (machine/complete-authoritative
          current
          outputs)]

     (update-execution
      realization'
      role'
      next-execution
      {:kind :authoritative
       :role role'
       :state state-id
       :operation (:operation action)
       :outputs outputs}))))

(defn complete-send
  "Complete one sender boundary and enqueue the exact machine-emitted message.

   payload selection remains explicit endpoint policy. machine/complete-send
   validates payload shape and sender knowledge before this realization changes.

   Returns {:realization next-state :message-id id :message envelope}."
  [realization role payload]
  (let [realization'
        (require-realization!
         realization)

        role'
        (require-role! role)

        current
        (require-role-execution!
         realization'
         role')

        state-id
        (machine/current-state-id
         current)

        {next-execution :execution
         message :message}
        (machine/complete-send
         current
         payload)

        realization-with-sender
        (assoc-in realization'
                  [:executions role']
                  next-execution)

        [realization-with-message
         queue-entry]
        (enqueue
         realization-with-sender
         message
         {:sent-by role'
          :sender-state state-id})

        message-id
        (:message-id queue-entry)

        next-realization
        (update
         realization-with-message
         :history
         conj
         {:kind :send
          :role role'
          :state state-id
          :message-id message-id
          :message message})]

    {:realization next-realization
     :message-id message-id
     :message message}))

;; -----------------------------------------------------------------------------
;; Transport scheduling/fault operations
;; -----------------------------------------------------------------------------

(defn deliver-message
  "Deliver one queued message to the receiver named by the emitted envelope.

   The queue entry is removed only after machine/receive succeeds. A message
   that is early, stale, contract-invalid for the current receive gate, or made
   ambiguous by the projected plan therefore remains visible for diagnosis.

   Physical duplicates retain the root id of the original emitted message. Once
   any copy of one root emission has been consumed successfully, another copy of
   that same root may not become a second semantic communication occurrence.
   This remains true if the receiver later reaches an otherwise identical
   receive gate.

   Returns the next realization state."
  [realization message-id]
  (let [realization'
        (require-realization!
         realization)

        message-id'
        (require-nonnegative-integer!
         "Realization message id"
         message-id)

        entry
        (or
         (queued-message
          realization'
          message-id')
         (realization-error
          :unknown-message
          "Realization transport queue does not contain this message id."
          {:message-id message-id'
           :queued-message-ids
           (set
            (map :message-id
                 (:messages realization')))}))

        message
        (:message entry)

        receiver-role
        (:to message)

        receiver
        (or
         (get (:executions realization')
              receiver-role)
         (realization-error
          :unknown-message-receiver
          "Emitted participant message names a receiver absent from this realization."
          {:message-id message-id'
           :message message
           :receiver receiver-role
           :roles (roles realization')}))

        receiver-state
        (machine/current-state-id
         receiver)

        stale-enabled-duplicate?
        (and
         (delivered-root-message?
          realization'
          entry)
         (machine/accepts-message?
          receiver
          message))

        _
        (when stale-enabled-duplicate?
          (realization-error
           :stale-duplicate-message
           "A physical duplicate of an already-delivered root message cannot become a second semantic communication occurrence."
           {:message-id message-id'
            :root-message-id (root-message-id entry)
            :receiver receiver-role
            :receiver-state receiver-state
            :message message}))

        next-receiver
        (machine/receive
         receiver
         message)]

    (-> realization'
        (assoc-in
         [:executions receiver-role]
         next-receiver)
        (remove-queued-message
         message-id')
        (update
         :history
         conj
         (cond->
          {:kind :deliver
           :role receiver-role
           :state receiver-state
           :message-id message-id'
           :message message}
           (contains? entry :origin-message-id)
           (assoc
            :origin-message-id
            (:origin-message-id entry)))))))

;; -----------------------------------------------------------------------------
;; Transport fault scheduling
;; -----------------------------------------------------------------------------

(defn drop-message
  "Drop one queued transport message without delivering it.

   Dropping models transport loss after a sender successfully emitted the
   participant message. The sender execution is not rewound and the receiver
   learns nothing. The removed envelope remains present in deterministic
   realization history for correspondence/fault diagnostics.

   Returns the next realization state."
  [realization message-id]
  (let [realization'
        (require-realization!
         realization)

        message-id'
        (require-nonnegative-integer!
         "Realization message id"
         message-id)

        entry
        (or
         (queued-message
          realization'
          message-id')
         (realization-error
          :unknown-message
          "Realization transport queue does not contain this message id."
          {:message-id message-id'
           :queued-message-ids
           (set
            (map :message-id
                 (:messages realization'))) }))

        history-entry
        (cond->
         {:kind :drop
          :message-id message-id'
          :message (:message entry)}
          (contains? entry :origin-message-id)
          (assoc
           :origin-message-id
           (:origin-message-id entry)))]

    (-> realization'
        (remove-queued-message
         message-id')
        (update
         :history
         conj
         history-entry))))

(defn duplicate-message
  "Duplicate one queued transport message without modifying its envelope.

   Duplication is allowed only for a message that was genuinely emitted and is
   still present in this realization's transport queue. The duplicate receives
   a fresh deterministic message id and records the root emitted message id as
   :origin-message-id. Re-duplicating a duplicate preserves that root origin.

   The semantic participant-message bytes are copied exactly; callers cannot use
   this operation to forge or modify a payload.

   Returns {:realization next-state :message-id duplicate-id :message envelope}."
  [realization message-id]
  (let [realization'
        (require-realization!
         realization)

        source-id
        (require-nonnegative-integer!
         "Realization message id"
         message-id)

        source-entry
        (or
         (queued-message
          realization'
          source-id)
         (realization-error
          :unknown-message
          "Realization transport queue does not contain this message id."
          {:message-id source-id
           :queued-message-ids
           (set
            (map :message-id
                 (:messages realization'))) }))

        message
        (:message source-entry)

        origin-id
        (or (:origin-message-id source-entry)
            source-id)

        [with-duplicate duplicate-entry]
        (enqueue
         realization'
         message
         {:origin-message-id origin-id})

        duplicate-id
        (:message-id duplicate-entry)

        next-realization
        (update
         with-duplicate
         :history
         conj
         {:kind :duplicate
          :source-message-id source-id
          :message-id duplicate-id
          :origin-message-id origin-id
          :message message})]

    {:realization next-realization
     :message-id duplicate-id
     :message message}))

;; -----------------------------------------------------------------------------
;; Environment scheduling
;; -----------------------------------------------------------------------------

(def ^:private environment-option-keys
  #{:authoritative-basis-progression})

(defn- require-environment-options!
  [options]
  (let [options'
        (if (nil? options)
          {}
          (if (map? options)
            options
            (realization-error
             :invalid-environment-options
             "Realization environment options must be a map."
             {:options options})))

        unknown
        (set/difference
         (set (keys options'))
         environment-option-keys)]

    (when (seq unknown)
      (realization-error
       :invalid-environment-options
       "Realization environment options contain unsupported keys."
       {:options options'
        :unknown-options unknown
        :allowed-options environment-option-keys}))

    options'))

(defn- realization-semantic-environment-data
  [execution event data]
  (let [awaiting
        (machine/awaiting execution)

        contract
        (get (:event-contracts awaiting)
             event
             {})

        allowed
        (set/union
         (or (:required contract) #{})
         (or (:optional contract) #{}))]

    (select-keys
     (or data {})
     allowed)))

(defn environment
  "Deliver one explicit role-local environment event.

   The projected machine owns event-contract validation and role-local knowledge
   establishment. This harness passes the supplied event through that boundary
   unchanged, then records only the event's declared semantic data in its own
   deterministic history.

   The optional five-argument form accepts one closed options map. Its only
   supported key is :authoritative-basis-progression, which is attached to the
   machine envelope as control evidence. It is deliberately not merged into
   event :data and therefore cannot become semantic knowledge or deterministic
   realization history. The machine remains responsible for validating whether
   that witness applies to the active authoritative-observation boundary.

   Undeclared fields admitted by :open-data? are adapter/test-harness data. They
   may influence the surrounding host fixture, but they do not become portable
   Choreo knowledge or deterministic realization history. A browser adapter must
   likewise keep DOM nodes, XHR objects, timer handles, and other host attachments
   outside the portable event data contract."
  ([realization role event]
   (environment
    realization
    role
    event
    nil
    nil))
  ([realization role event data]
   (environment
    realization
    role
    event
    data
    nil))
  ([realization role event data options]
   (let [realization'
         (require-realization!
          realization)

         role'
         (require-role! role)

         options'
         (require-environment-options!
          options)

         current
         (require-role-execution!
          realization'
          role')

         state-id
         (machine/current-state-id
          current)

         envelope
         (cond->
          (machine/environment-event
           role'
           event
           data)
          (contains? options'
                     :authoritative-basis-progression)
          (assoc
           :authoritative-basis-progression
           (:authoritative-basis-progression
            options')))

         next-execution
         (machine/resume-environment
          current
          envelope)

         semantic-data
         (realization-semantic-environment-data
          current
          event
          data)

         history-data
         (if (and (nil? data)
                  (empty? semantic-data))
           nil
           semantic-data)]

     (update-execution
      realization'
      role'
      next-execution
      {:kind :environment
       :role role'
       :state state-id
       :event event
       :data history-data}))))

;; -----------------------------------------------------------------------------
;; Diagnostics
;; -----------------------------------------------------------------------------

(defn explain
  "Return a compact deterministic summary suitable for tests and REPL use."
  [realization]
  (let [realization'
        (require-realization!
         realization)]
    {:roles
     (roles realization')

     :completed?
     (completed? realization')

     :roles-completed?
     (roles-completed? realization')

     :quiescent?
     (quiescent? realization')

     :boundaries
     (boundaries realization')

     :waiting-environment-roles
     (waiting-environment-roles realization')

     :queued-message-ids
     (mapv :message-id
           (:messages realization'))

     :deliverable-message-ids
     (deliverable-message-ids realization')

     :role-status
     (into
      {}
      (map
       (fn [[role execution]]
         [role
          (:status
           (machine/explain
            execution))]))
      (:executions realization'))

     :history-count
     (count
      (:history realization'))}))
