(ns gesso.live.choreo.project
  "Endpoint projection for verified Gesso Live choreographies.

   Projection turns one verified global interaction into a compact role-local
   plan suitable for the generic choreography machine.

   The projector is deliberately mechanical:
   - local effects remain local effects
   - local choices remain local choices
   - local sends remain sends
   - remote implementation steps disappear
   - communication arriving at the role becomes an await gate
   - remote choice guards become message-match requirements when needed
   - foreign completion becomes local completion when no further local action
     is required

   Projection does not re-run the choreography verifier and does not know about
   DOM, HTMX, Ring, XTDB, SSE implementation, or optimistic policy.

   A projected plan contains only behavior observable or executable by one role.
   Compiler/verifier machinery should not be required by the production browser
   runtime once the projected plan has been emitted as data."
  (:refer-clojure :exclude [await send])
  (:require
   [clojure.set :as set]
   [gesso.live.choreo :as choreo]
   [gesso.live.choreo.verify :as verify]))

;; -----------------------------------------------------------------------------
;; Projected representation
;; -----------------------------------------------------------------------------

(def projected-plan-version
  1)

(def projected-plan-type
  :gesso.live.choreo/projected-plan)

(def projected-ops
  "Operations consumed by the role-local choreography machine.

   :await is the single suspension primitive. It may wait for participant
   messages, environment events, or both."
  #{:effect
    :send
    :choice
    :await
    :acquire
    :release
    :return})

(def projected-complete-outcome
  "Terminal outcome used when the global choreography has no further observable
   work for this role."
  :gesso.live.choreo/complete)

;; -----------------------------------------------------------------------------
;; Errors
;; -----------------------------------------------------------------------------

(defn- projection-error
  [kind message data]
  (throw
   (ex-info
    message
    (merge
     {:error/type :gesso.live.choreo/projection-failed
      :error/kind kind}
     data))))

;; -----------------------------------------------------------------------------
;; Projected plan predicates
;; -----------------------------------------------------------------------------

(defn projected-plan?
  "True when x is a role-local projected plan."
  [x]
  (and (map? x)
       (= projected-plan-type
          (:gesso.live.choreo/type x))
       (= projected-plan-version
          (:gesso.live.choreo/version x))
       (keyword? (:role x))
       (map? (:states x))))

(defn ensure-projected-plan
  "Return x when it is a projected plan, otherwise throw."
  [x]
  (when-not (projected-plan? x)
    (projection-error
     :invalid-projected-plan
     "Expected a Gesso Live projected choreography plan."
     {:value x}))
  x)

;; -----------------------------------------------------------------------------
;; Global communication helpers
;; -----------------------------------------------------------------------------

(defn- communication-key
  [state]
  (select-keys state [:from :to :event :via]))

(defn- matching-communication?
  [send-state receive-state]
  (and (= :send (:op send-state))
       (= :receive (:op receive-state))
       (= (communication-key send-state)
          (communication-key receive-state))))

(defn- projected-message-contract
  [send-state receive-state guards]
  (let [required (:required send-state #{})
        match-keys (set (keys guards))]
    (when-not (set/subset? match-keys required)
      (projection-error
       :uncommunicated-choice
       "Projected receive requires remote choice values that are not required by the sending message."
       {:send (communication-key send-state)
        :required required
        :choice-keys match-keys
        :missing (set/difference match-keys required)}))
    (cond-> {:from (:from send-state)
             :to (:to send-state)
             :event (:event send-state)
             :required required
             :optional (:optional send-state #{})
             :correlation (:correlation send-state #{})
             :match guards}
      (contains? send-state :via)
      (assoc :via (:via send-state))

      (contains? receive-state :bind)
      (assoc :bind (:bind receive-state)))))

;; -----------------------------------------------------------------------------
;; Observable frontier
;; -----------------------------------------------------------------------------

(defn- local-state?
  [state role]
  (= role (choreo/state-role state)))

(defn- frontier-entry-key
  [entry]
  (case (:kind entry)
    :state
    [:state (:state entry)]

    :receive
    [:receive
     (:receive-state entry)
     (:send-state entry)
     (:guards entry)]

    :done
    [:done]

    entry))

(defn- distinct-frontier
  [entries]
  (->> entries
       (reduce
        (fn [acc entry]
          (assoc acc (frontier-entry-key entry) entry))
        {})
       vals
       vec))

(defn- observable-frontier
  "Find the first behavior observable by role from global start-state.

   Foreign implementation states are traversed without being copied into the
   endpoint plan.

   Foreign authoritative choices contribute guards. When different branches
   later communicate through the same message identity, those guards become
   payload match requirements in the local await gate.

   A foreign send directly to role is remembered until its matching local
   receive is reached. This preserves the exact message contract for that
   receive rather than guessing from all graph predecessors."
  [choreography role start-state]
  (let [states (:states choreography)]
    (loop [pending [{:state start-state
                     :guards {}
                     :pending-send nil}]
           index 0
           seen #{}
           frontier []]
      (if (= index (count pending))
        (distinct-frontier frontier)
        (let [{state-id :state
               guards :guards
               pending-send :pending-send
               :as cursor}
              (nth pending index)
              visit-key [state-id guards pending-send]]
          (cond
            (contains? seen visit-key)
            (recur pending
                   (inc index)
                   seen
                   frontier)

            (not (contains? states state-id))
            (projection-error
             :unknown-state
             "Verified choreography projection encountered an unknown state."
             {:role role
              :state state-id})

            :else
            (let [state (get states state-id)
                  op (:op state)
                  local? (local-state? state role)
                  seen' (conj seen visit-key)]
              (cond
                ;; Local receive is not emitted as an executable state. It is
                ;; folded into an await gate carrying the exact remote send
                ;; contract that made this state reachable.
                (and local?
                     (= :receive op))
                (let [send-state
                      (when pending-send
                        (get states pending-send))]
                  (when-not (and send-state
                                 (matching-communication?
                                  send-state
                                  state))
                    (projection-error
                     :unmatched-projected-receive
                     "Local receive was reached without its matching remote send."
                     {:role role
                      :receive-state state-id
                      :pending-send pending-send
                      :receive (communication-key state)
                      :send (when send-state
                              (communication-key send-state))}))
                  (recur pending
                         (inc index)
                         seen'
                         (conj frontier
                               {:kind :receive
                                :receive-state state-id
                                :send-state pending-send
                                :guards guards})))

                ;; Any other local state is directly executable/observable.
                local?
                (do
                  (when pending-send
                    (projection-error
                     :invalid-communication-frontier
                     "Remote send reached a local state other than its matching receive."
                     {:role role
                      :send-state pending-send
                      :state state-id
                      :op op}))
                  (recur pending
                         (inc index)
                         seen'
                         (conj frontier
                               {:kind :state
                                :state state-id})))

                ;; Global completion by another role means this endpoint has no
                ;; further work. The remote terminal outcome is intentionally
                ;; not revealed to a role that was not told that outcome.
                (= :return op)
                (recur pending
                       (inc index)
                       seen'
                       (conj frontier {:kind :done}))

                ;; A foreign choice is traversed branch-by-branch. Its semantic
                ;; value is remembered as a guard so the local endpoint can
                ;; select correctly if branch identity must later be learned
                ;; from a message payload.
                (= :choice op)
                (let [choice-key (:key state)
                      cursors
                      (mapv
                       (fn [[branch target]]
                         {:state target
                          :guards (assoc guards choice-key branch)
                          :pending-send nil})
                       (:branches state))]
                  (recur (into pending cursors)
                         (inc index)
                         seen'
                         frontier))

                ;; A foreign send to this role must flow immediately into the
                ;; matching local receive. Interrupt paths do not produce that
                ;; message and therefore clear pending-send.
                (= :send op)
                (let [normal
                      {:state (:next state)
                       :guards guards
                       :pending-send
                       (when (= role (:to state))
                         state-id)}
                      interrupts
                      (mapv
                       (fn [[_event target]]
                         {:state target
                          :guards guards
                          :pending-send nil})
                       (:interrupts state))]
                  (recur (into pending
                               (cons normal interrupts))
                         (inc index)
                         seen'
                         frontier))

                ;; Foreign await branches represent remote/environmental
                ;; nondeterminism. The local endpoint does not know which event
                ;; occurred; only later communication may make that distinction
                ;; observable.
                (= :await op)
                (let [cursors
                      (mapv
                       (fn [[_event target]]
                         {:state target
                          :guards guards
                          :pending-send nil})
                       (:events state))]
                  (recur (into pending cursors)
                         (inc index)
                         seen'
                         frontier))

                ;; Foreign receive/effect/acquire/release/goto are invisible
                ;; local implementation steps and have one normal successor.
                (contains?
                 #{:receive :effect :acquire :release :goto}
                 op)
                (recur (conj pending
                             {:state (:next state)
                              :guards guards
                              :pending-send nil})
                       (inc index)
                       seen'
                       frontier)

                :else
                (projection-error
                 :unsupported-op
                 "Projection encountered an unsupported choreography operation."
                 {:role role
                  :state state-id
                  :op op})))))))))

;; -----------------------------------------------------------------------------
;; Await alternative ambiguity
;; -----------------------------------------------------------------------------

(defn- message-identity
  [alternative]
  (select-keys alternative [:from :to :event :via]))

(defn- mutually-exclusive-match?
  [left right]
  (let [left-match (:match left {})
        right-match (:match right {})
        shared (set/intersection
                (set (keys left-match))
                (set (keys right-match)))]
    (boolean
     (some
      (fn [k]
        (not= (get left-match k)
              (get right-match k)))
      shared))))

(defn- same-continuation?
  [left right]
  (and (= (:next left)
          (:next right))
       (= (:bind left)
          (:bind right))))

(defn- ambiguous-pair?
  [left right]
  (and (= (message-identity left)
          (message-identity right))
       (not (same-continuation? left right))
       (not (mutually-exclusive-match?
             left
             right))))

(defn- assert-unambiguous-receives!
  [role alternatives]
  (doseq [i (range (count alternatives))
          j (range (inc i) (count alternatives))
          :let [left (nth alternatives i)
                right (nth alternatives j)]
          :when (ambiguous-pair? left right)]
    (projection-error
     :ambiguous-receive
     "Two projected incoming-message alternatives can match the same message but continue differently."
     {:role role
      :left left
      :right right})))

;; -----------------------------------------------------------------------------
;; Projection builder
;; -----------------------------------------------------------------------------

(defn- synthetic-id
  [kind role source]
  [:gesso.live.choreo.project/synthetic
   kind
   role
   source])

(defn- local-resource-ids
  [states]
  (set
   (keep
    (fn [[_state-id state]]
      (when (contains? #{:acquire :release}
                       (:op state))
        (:resource state)))
    states)))

(defn- local-environment-events
  [states]
  (reduce
   (fn [events [_state-id state]]
     (if (= :await (:op state))
       (into events
             (keys (:events state)))
       events))
   #{}
   states))

(defn project
  "Project verified choreography to one role-local executable plan.

   choreography-or-verified may be a plain choreography or a value returned by
   gesso.live.choreo.verify/verify!. Plain choreography is verified first.

   Projected state ids are opaque EDN values. Original locally executable states
   retain their original keyword ids; compiler-created await/completion states
   use deterministic vector ids.

   The projected plan contains no foreign local effects or foreign resource
   operations."
  [choreography-or-verified role]
  (let [verified (verify/ensure-verified choreography-or-verified)
        choreography (:choreography verified)
        roles (:roles choreography)
        global-states (:states choreography)
        global-resources (:resources choreography)
        projected-states (atom {})
        continuation-cache (atom {})]
    (when-not (contains? roles role)
      (projection-error
       :unknown-role
       "Cannot project choreography to an undeclared role."
       {:role role
        :roles roles}))

    (letfn [(reserve-state!
              [state-id]
              (when-not (contains? @projected-states state-id)
                (swap! projected-states
                       assoc
                       state-id
                       {:op :gesso.live.choreo.project/building}))
              state-id)

            (install-state!
              [state-id state]
              (swap! projected-states assoc state-id state)
              state-id)

            (ensure-done!
              [source]
              (let [state-id
                    (synthetic-id :complete role source)]
                (when-not (contains? @projected-states state-id)
                  (install-state!
                   state-id
                   {:op :return
                    :role role
                    :outcome projected-complete-outcome}))
                state-id))

            (ensure-receive-await!
              [source entries]
              (let [state-id
                    (synthetic-id :receive role source)]
                (if (contains? @projected-states state-id)
                  state-id
                  (do
                    ;; Reserve before recursively compiling post-receive
                    ;; continuations so cycles can point back to this gate.
                    (reserve-state! state-id)
                    (let [alternatives
                          (mapv
                           (fn [{:keys
                                 [receive-state
                                  send-state
                                  guards]}]
                             (let [send (get global-states send-state)
                                   receive (get global-states
                                                receive-state)
                                   next-id
                                   (ensure-continuation!
                                    (:next receive))]
                               (assoc
                                (projected-message-contract
                                 send
                                 receive
                                 guards)
                                :next next-id)))
                           entries)]
                      (assert-unambiguous-receives!
                       role
                       alternatives)
                      (install-state!
                       state-id
                       {:op :await
                        :role role
                        :receives alternatives})
                      state-id)))))

            (classify-frontier
              [source frontier]
              (let [kinds (set (map :kind frontier))]
                (cond
                  (empty? frontier)
                  (projection-error
                   :empty-frontier
                   "Projection found no observable continuation for a verified reachable state."
                   {:role role
                    :state source})

                  (= kinds #{:done})
                  [:done nil]

                  (= kinds #{:receive})
                  [:receive frontier]

                  ;; A role may have no endpoint execution at all when the
                  ;; choreography terminates before its first incoming message.
                  ;; This is safe only at the global entry frontier: if the
                  ;; message arrives, the endpoint starts at the receive gate;
                  ;; if the remote side terminates first, this endpoint was
                  ;; never activated. Once a role has begun executing, silently
                  ;; dropping a completion branch would turn a protocol error
                  ;; into an unbounded wait.
                  (and (= source (:initial choreography))
                       (= kinds #{:receive :done}))
                  [:receive
                   (vec (filter #(= :receive (:kind %)) frontier))]

                  (= kinds #{:state})
                  (let [state-ids
                        (set (map :state frontier))]
                    (if (= 1 (count state-ids))
                      [:state (first state-ids)]
                      (projection-error
                       :uncommunicated-control-flow
                       "Remote control flow reaches different local states without first communicating which continuation was chosen."
                       {:role role
                        :state source
                        :local-frontier state-ids})))

                  :else
                  (projection-error
                   :mixed-observable-frontier
                   "Remote control flow mixes completion, incoming communication, and/or direct local execution in a way the endpoint cannot observe safely."
                   {:role role
                    :state source
                    :frontier frontier}))))

            (ensure-continuation!
              [start-state]
              (if-some [cached
                        (get @continuation-cache start-state)]
                cached
                (let [frontier
                      (observable-frontier
                       choreography
                       role
                       start-state)
                      [kind value]
                      (classify-frontier
                       start-state
                       frontier)
                      target-id
                      (case kind
                        :done
                        (ensure-done! start-state)

                        :receive
                        (do
                          ;; Install cache before the await body recursively
                          ;; compiles post-receive continuations.
                          (let [await-id
                                (synthetic-id
                                 :receive
                                 role
                                 start-state)]
                            (swap! continuation-cache
                                   assoc
                                   start-state
                                   await-id)
                            (ensure-receive-await!
                             start-state
                             value)))

                        :state
                        (do
                          (swap! continuation-cache
                                 assoc
                                 start-state
                                 value)
                          (ensure-local-state! value))

                        (projection-error
                         :internal-frontier-kind
                         "Projection produced an unknown frontier classification."
                         {:role role
                          :state start-state
                          :kind kind}))]
                  (swap! continuation-cache
                         assoc
                         start-state
                         target-id)
                  target-id)))

            (merge-send-interrupts!
              [send-id normal-target interrupts]
              (if (empty? interrupts)
                normal-target
                (let [normal-state
                      (get @projected-states normal-target)]
                  (if (= :await (:op normal-state))
                    (let [wait-id
                          (synthetic-id
                           :send-wait
                           role
                           send-id)
                          event-targets
                          (into {}
                                (map
                                 (fn [[event target]]
                                   [event
                                    (ensure-continuation!
                                     target)]))
                                interrupts)
                          existing-events
                          (:events normal-state {})]
                      (when (seq
                             (set/intersection
                              (set (keys existing-events))
                              (set (keys event-targets))))
                        (projection-error
                         :duplicate-await-event
                         "Send interrupt event collides with an event already present in the projected await gate."
                         {:role role
                          :state send-id
                          :events
                          (set/intersection
                           (set (keys existing-events))
                           (set (keys event-targets)))}))
                      (install-state!
                       wait-id
                       (cond-> {:op :await
                                :role role}
                         (seq (:receives normal-state))
                         (assoc
                          :receives
                          (:receives normal-state))

                         (or (seq existing-events)
                             (seq event-targets))
                         (assoc
                          :events
                          (merge
                           existing-events
                           event-targets))))
                      wait-id)
                    ;; No incoming wait follows this send. The local endpoint
                    ;; has no reason to stay alive merely to observe a failure
                    ;; after it has already become protocol-independent.
                    normal-target))))

            (ensure-local-state!
              [state-id]
              (if (contains? @projected-states state-id)
                state-id
                (let [state (get global-states state-id)
                      op (:op state)]
                  (when-not (= role
                               (choreo/state-role state))
                    (projection-error
                     :foreign-local-state
                     "Projection attempted to install a state owned by another role."
                     {:role role
                      :state state-id
                      :state-role
                      (choreo/state-role state)}))
                  (when (= :receive op)
                    (projection-error
                     :raw-receive-state
                     "Global receive states must be folded into projected await gates."
                     {:role role
                      :state state-id}))
                  ;; Reserve before compiling outgoing edges so local cycles are
                  ;; represented by ordinary state-id references.
                  (reserve-state! state-id)
                  (install-state!
                   state-id
                   (case op
                     :effect
                     (cond-> {:op :effect
                              :role role
                              :effect (:effect state)
                              :next
                              (ensure-continuation!
                               (:next state))}
                       (contains? state :args)
                       (assoc :args (:args state)))

                     :send
                     (let [normal
                           (ensure-continuation!
                            (:next state))
                           next-id
                           (merge-send-interrupts!
                            state-id
                            normal
                            (:interrupts state))]
                       (cond-> {:op :send
                                :role role
                                :to (:to state)
                                :event (:event state)
                                :required
                                (:required state #{})
                                :optional
                                (:optional state #{})
                                :correlation
                                (:correlation state #{})
                                :next next-id}
                         (contains? state :via)
                         (assoc :via (:via state))))

                     :choice
                     {:op :choice
                      :role role
                      :key (:key state)
                      :branches
                      (into {}
                            (map
                             (fn [[branch target]]
                               [branch
                                (ensure-continuation!
                                 target)]))
                            (:branches state))}

                     :await
                     (cond-> {:op :await
                              :role role
                              :events
                              (into {}
                                    (map
                                     (fn [[event target]]
                                       [event
                                        (ensure-continuation!
                                         target)]))
                                    (:events state))}
                       (contains? state :bind)
                       (assoc :bind (:bind state)))

                     :acquire
                     {:op :acquire
                      :role role
                      :resource (:resource state)
                      :next
                      (ensure-continuation!
                       (:next state))}

                     :release
                     {:op :release
                      :role role
                      :resource (:resource state)
                      :next
                      (ensure-continuation!
                       (:next state))}

                     :return
                     (cond-> {:op :return
                              :role role
                              :outcome (:outcome state)}
                       (contains? state :value-key)
                       (assoc
                        :value-key
                        (:value-key state)))

                     (projection-error
                      :unsupported-local-op
                      "Projection encountered a local operation that cannot appear in a role-local plan."
                      {:role role
                       :state state-id
                       :op op})))
                  state-id)))]
      (let [initial
            (ensure-continuation!
             (:initial choreography))
            states @projected-states
            leaked-building
            (set
             (for [[state-id state] states
                   :when
                   (= :gesso.live.choreo.project/building
                      (:op state))]
               state-id))]
        (when (seq leaked-building)
          (projection-error
           :incomplete-projection
           "Projection left compiler placeholder states in the emitted plan."
           {:role role
            :states leaked-building}))
        (let [resource-ids
              (local-resource-ids states)
              resources
              (select-keys
               global-resources
               resource-ids)
              environment-events
              (set/intersection
               (:environment-events choreography)
               (local-environment-events states))]
          {:gesso.live.choreo/type projected-plan-type
           :gesso.live.choreo/version projected-plan-version
           :name (:name choreography)
           :role role
           :initial initial
           :states states
           :resources resources
           :environment-events environment-events})))))

(defn project-all
  "Project choreography once for every declared role.

   Returns role -> projected plan."
  [choreography-or-verified]
  (let [verified
        (verify/ensure-verified choreography-or-verified)
        roles
        (get-in verified [:choreography :roles])]
    (into {}
          (map
           (fn [role]
             [role
              (project verified role)]))
          roles)))

(defn state
  "Return projected state by id."
  [plan state-id]
  (get (:states (ensure-projected-plan plan))
       state-id))

(defn explain
  "Return a compact, stable summary of one projected plan."
  [plan]
  (let [plan' (ensure-projected-plan plan)
        states (:states plan')]
    {:name (:name plan')
     :role (:role plan')
     :version (:gesso.live.choreo/version plan')
     :initial (:initial plan')
     :state-count (count states)
     :states-by-op
     (frequencies
      (map (comp :op val) states))
     :resources (set (keys (:resources plan')))
     :environment-events
     (:environment-events plan')}))
