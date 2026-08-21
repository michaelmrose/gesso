(ns gesso.choreo.project
  "Projection for the first v4.5 Gesso Choreo semantic core.

   Projection turns one verified global choreography into one role-local plan.

   The current global language is deliberately small:

     :local
     :authoritative
     :branch
     :communicate
     :await
     :return

   The current projected language is likewise small:

     :local
       semantically local work owned by this role, including declared value
       inputs and outputs

     :authoritative
       this role realizes one public authoritative semantic operation, with
       explicit declared inputs and closed outputs

     :branch
       deterministic role-local branching on one established semantic value

     :send
       this role realizes the sending side of a global :communicate, including
       the declared closed payload contract

     :receive
       this role waits for one of one-or-more participant communications, each
       retaining its declared closed payload contract

     :await
       this role waits for one of its own environment events

     :return
       this role has no further work in this choreography

   Projection deliberately distinguishes participant communication from
   environment events. A projected :receive is never satisfied by an
   environment event, and a projected :await is never satisfied by a participant
   message merely because the event keyword is the same.

   Communication contracts are compiler data that survive projection. Required,
   optional, correlation, and explicit open-payload declarations are copied to
   the projected sender and receiver sides. Receive alternatives sharing one
   sender/event/channel must accept disjoint payload shapes; statically knowable
   overlap is rejected during projection rather than deferred to the runtime
   machine. Projection does not reinterpret those declarations as knowledge or
   authority.

   Foreign local actions disappear from a role projection because :local is,
   by definition, distributed-unobservable. Foreign :branch and foreign
   environment waits are traversed through every possible continuation.

   Foreign :authoritative is different. An authoritative operation is a global
   observable semantic step. Another role may not simply run later local work as
   though that authoritative predecessor had already occurred. Projection tracks
   such a causal barrier until this role receives a participant communication
   that occurs after the authority step. If direct local work would otherwise be
   reached first, projection rejects the choreography as unrealizable for that
   role. A role with no remaining work may still complete locally; local
   completion does not claim the global choreography has terminated.

   Projection also rejects remote control flow that reaches different direct
   local actions before this role can distinguish which continuation occurred.
   Multiple remote branches may instead converge on distinct incoming
   participant communications; those communications become alternatives of one
   projected :receive gate.

   This is intentionally only a first projection semantics. It does not yet
   claim a refinement proof. The later local-machine semantics and independent
   execution work must establish what a projected :send/:receive pair means
   operationally and whether this projection actually realizes the global
   transition semantics.

   This namespace contains no browser, HTMX, transport, XTDB implementation,
   authentication, knowledge/provenance proof, optimism, or model-specific
   behavior."
  (:require
   [gesso.choreo.core :as choreo]
   [gesso.choreo.verify :as verify]))

;; -----------------------------------------------------------------------------
;; Identity
;; -----------------------------------------------------------------------------

(def projected-plan-version
  1)

(def projected-plan-type
  :gesso.choreo/projected-plan)

(def projected-ops
  #{:local
    :authoritative
    :branch
    :send
    :receive
    :await
    :return})

(def complete-outcome
  :gesso.choreo/complete)

;; -----------------------------------------------------------------------------
;; Errors
;; -----------------------------------------------------------------------------

(defn- projection-error
  [kind message data]
  (throw
   (ex-info
    message
    (merge
     {:error/type :gesso.choreo/projection-failed
      :error/kind kind}
     data))))

;; -----------------------------------------------------------------------------
;; Projected-plan predicates
;; -----------------------------------------------------------------------------

(defn projected-plan?
  "True when x has the shallow identity/shape of a projected plan.

   Full correctness of a plan is a compiler/local-machine concern. This
   predicate intentionally does not duplicate the projector."
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

(defn ensure-projected-plan
  "Return x when it is a projected plan; otherwise throw."
  [x]
  (when-not (projected-plan? x)
    (projection-error
     :invalid-projected-plan
     "Expected a Gesso projected choreography plan."
     {:value x}))
  x)

;; -----------------------------------------------------------------------------
;; Global observable frontier
;; -----------------------------------------------------------------------------

(defn- frontier-entry-key
  [entry]
  (case (:kind entry)
    :state
    [:state
     (:state entry)]

    :receive
    [:receive
     (:state entry)]

    :blocked
    [:blocked
     (:state entry)
     (:authority-state entry)]

    :done
    [:done]

    entry))

(defn- distinct-frontier
  [entries]
  (->> entries
       (reduce
        (fn [by-key entry]
          (assoc by-key
                 (frontier-entry-key entry)
                 entry))
        {})
       vals
       vec))

(defn- role-direct-state?
  [state role]
  (case (:op state)
    :local
    (= role
       (:role state))

    :authoritative
    (= role
       (:role state))

    :branch
    (= role
       (:role state))

    :await
    (= role
       (:role state))

    :communicate
    (= role
       (:from state))

    false))

(defn- incoming-communication?
  [state role]
  (and (= :communicate
          (:op state))
       (= role
          (:to state))))

(defn- observable-frontier
  "Return the first global states that are locally relevant to role.

   Traversal carries a causal-barrier marker after a foreign :authoritative
   state. While that barrier is present, direct work by role is not projectable:
   the role has no observation establishing that the authoritative predecessor
   occurred. An incoming participant communication clears the barrier because
   receiving that message is an observable causal successor.

   Frontier entries:

     {:kind :state :state id}
       Direct work role may perform now.

     {:kind :receive :state id}
       Incoming participant communication role may receive now.

     {:kind :blocked :state id :authority-state id}
       Direct role work is causally after a foreign authoritative operation but
       no communication has yet made that predecessor observable to role.

     {:kind :done}
       This branch has no further local work for role.

   Foreign :local, :branch, :communicate between other roles, and :await remain
   unobservable to role and are traversed. Foreign :authoritative is traversed
   while setting the causal barrier."
  [choreography role start-state]
  (let [states
        (:states choreography)]

    (loop [pending
           [{:state start-state
             :authority-state nil}]

           index
           0

           seen
           #{}

           frontier
           []]

      (if (= index
             (count pending))
        (distinct-frontier frontier)

        (let [{state-id :state
               authority-state :authority-state}
              (nth pending index)

              seen-key
              [state-id authority-state]]

          (if (contains? seen seen-key)
            (recur pending
                   (inc index)
                   seen
                   frontier)

            (let [state
                  (get states state-id)

                  seen'
                  (conj seen seen-key)

                  enqueue
                  (fn [pending target authority-state']
                    (conj pending
                          {:state target
                           :authority-state authority-state'}))]

              (when-not state
                (projection-error
                 :unknown-state
                 "Projection encountered an unknown global state."
                 {:role role
                  :state state-id}))

              (cond
                (role-direct-state?
                 state
                 role)
                (recur pending
                       (inc index)
                       seen'
                       (conj frontier
                             (if authority-state
                               {:kind :blocked
                                :state state-id
                                :authority-state authority-state}
                               {:kind :state
                                :state state-id})))

                (incoming-communication?
                 state
                 role)
                ;; The communication is causally after everything traversed to
                ;; reach it. Receiving it therefore gives this role the first
                ;; observable synchronization point after a foreign authority
                ;; barrier.
                (recur pending
                       (inc index)
                       seen'
                       (conj frontier
                             {:kind :receive
                              :state state-id}))

                (= :return
                   (:op state))
                (recur pending
                       (inc index)
                       seen'
                       (conj frontier
                             {:kind :done}))

                (= :local
                   (:op state))
                (recur
                 (enqueue pending
                          (:next state)
                          authority-state)
                 (inc index)
                 seen'
                 frontier)

                (= :authoritative
                   (:op state))
                ;; A foreign authority transition is globally observable and
                ;; cannot be silently reordered before this role's later work.
                ;; Remember the nearest such predecessor until an incoming
                ;; communication to role synchronizes it.
                (recur
                 (enqueue pending
                          (:next state)
                          (or authority-state
                              state-id))
                 (inc index)
                 seen'
                 frontier)

                (= :branch
                   (:op state))
                (recur
                 (into pending
                       (map
                        (fn [target]
                          {:state target
                           :authority-state authority-state})
                        (vals
                         (:cases state))))
                 (inc index)
                 seen'
                 frontier)

                (= :communicate
                   (:op state))
                ;; Communication between other roles does not synchronize this
                ;; role, so any foreign-authority barrier remains outstanding.
                (recur
                 (enqueue pending
                          (:next state)
                          authority-state)
                 (inc index)
                 seen'
                 frontier)

                (= :await
                   (:op state))
                (recur
                 (into pending
                       (map
                        (fn [target]
                          {:state target
                           :authority-state authority-state})
                        (vals
                         (:events state))))
                 (inc index)
                 seen'
                 frontier)

                :else
                (projection-error
                 :unsupported-op
                 "Projection encountered an unsupported global operation."
                 {:role role
                  :state state-id
                  :op (:op state)})))))))))
;; -----------------------------------------------------------------------------
;; Frontier classification
;; -----------------------------------------------------------------------------

(defn- classify-frontier
  [role source frontier]
  (let [kinds
        (set
         (map :kind frontier))]

    (cond
      (empty? frontier)
      (projection-error
       :empty-frontier
       "Projection found no locally meaningful continuation."
       {:role role
        :state source})

      (contains? kinds :blocked)
      (projection-error
       :unobserved-authoritative-predecessor
       "Role-local behavior is causally after a foreign authoritative operation, but no incoming communication establishes that the authoritative predecessor occurred."
       {:role role
        :state source
        :frontier frontier
        :authority-states
        (set
         (keep :authority-state frontier))})

      (= kinds
         #{:done})
      {:kind :done}

      (= kinds
         #{:receive})
      {:kind :receive
       :entries frontier}

      (= kinds
         #{:state})
      (let [state-ids
            (set
             (map :state frontier))]

        (if (= 1
               (count state-ids))
          {:kind :state
           :state
           (first state-ids)}

          (projection-error
           :uncommunicated-control-flow
           "Remote control flow reaches different direct local states before this role can distinguish which continuation occurred."
           {:role role
            :state source
            :local-frontier state-ids})))

      :else
      (projection-error
       :mixed-observable-frontier
       "Remote control flow mixes completion, incoming communication, and/or direct local execution in a way this role cannot distinguish."
       {:role role
        :state source
        :frontier frontier}))))

;; -----------------------------------------------------------------------------
;; Receive alternatives
;; -----------------------------------------------------------------------------

(defn- receive-route-identity
  "Return the transport-visible identity used to choose candidate receives.

   Payload contracts are intentionally excluded. Two alternatives with the same
   sender/event/channel therefore compete at one receive gate and must have
   disjoint accepted payload languages unless they are literally the same
   projected alternative."
  [alternative]
  (select-keys
   alternative
   [:from
    :event
    :via]))

(defn- receive-contract-identity
  [alternative]
  (select-keys
   alternative
   [:required
    :optional
    :correlation
    :open-payload?]))

(defn- contract-required
  [alternative]
  (or (:required alternative)
      #{}))

(defn- contract-allowed
  [alternative]
  (into (contract-required alternative)
        (or (:optional alternative)
            #{})))

(defn- contract-open?
  [alternative]
  (true?
   (:open-payload? alternative)))

(defn- contracts-overlap?
  "True when at least one payload key-set is accepted by both alternatives.

   Message contracts currently constrain key presence only. For a closed
   contract, accepted key sets satisfy:

     required <= payload-keys <= required U optional

   For an open contract there is no upper bound. Correlation keys do not add an
   independent matching predicate because the authoring layer already requires
   them to be required payload keys."
  [left right]
  (let [required
        (into (contract-required left)
              (contract-required right))

        left-open?
        (contract-open? left)

        right-open?
        (contract-open? right)]

    (and
     (or left-open?
         (every? (contract-allowed left)
                 required))
     (or right-open?
         (every? (contract-allowed right)
                 required)))))

(defn- overlapping-alternatives
  [alternatives]
  (first
   (for [left-index (range (count alternatives))
         right-index (range (inc left-index)
                            (count alternatives))
         :let [left (nth alternatives left-index)
               right (nth alternatives right-index)]
         :when (contracts-overlap? left right)]
     [left right])))

(defn- normalize-receive-alternatives
  [role source alternatives]
  (let [alternatives'
        ;; The same global continuation can be discovered more than once while
        ;; traversing converged remote control flow. Exact projected duplicates
        ;; are one alternative, not an ambiguity.
        (vec
         (distinct alternatives))

        by-route
        (group-by
         receive-route-identity
         alternatives')]

    (doseq [[route same-route] by-route]
      (when-some [[left right]
                  (overlapping-alternatives
                   (vec same-route))]
        (projection-error
         :ambiguous-receive
         "Projected receive alternatives with the same sender/event/channel accept an overlapping payload shape."
         {:role role
          :state source
          :route route
          :left left
          :right right
          :left-contract
          (receive-contract-identity left)
          :right-contract
          (receive-contract-identity right)})))

    (->> alternatives'
         (sort-by
          (fn [alternative]
            (pr-str
             [(receive-route-identity alternative)
              (receive-contract-identity alternative)
              (:next alternative)])))
         vec)))

;; -----------------------------------------------------------------------------
;; Projection builder
;; -----------------------------------------------------------------------------

(defn- synthetic-id
  [kind role source]
  [:gesso.choreo.project/synthetic
   kind
   role
   source])

(defn project
  "Project one choreography to one role-local plan.

   choreography-or-verified may be:

   - a plain/normalized choreography;
   - a successful verification result;
   - a successful verified wrapper.

   Projection currently accepts only roles inferred from the choreography.

   The returned plan is compiler output, not yet a final v4.5 ExecutablePlan
   format."
  [choreography-or-verified role]
  (let [verified
        (verify/ensure-verified
         choreography-or-verified)

        choreography
        (:choreography verified)

        roles
        (choreo/roles choreography)

        global-states
        (:states choreography)

        projected-states
        (atom {})

        continuation-cache
        (atom {})]

    (when-not (contains? roles role)
      (projection-error
       :unknown-role
       "Cannot project choreography to a role not present in the choreography."
       {:role role
        :roles roles}))

    (letfn
        [(reserve-state!
           [state-id]
           (when-not (contains?
                      @projected-states
                      state-id)
             (swap! projected-states
                    assoc
                    state-id
                    {:op
                     :gesso.choreo.project/building}))
           state-id)

         (install-state!
           [state-id state]
           (swap! projected-states
                  assoc
                  state-id
                  state)
           state-id)

         (ensure-complete!
           [source]
           (let [state-id
                 (synthetic-id
                  :complete
                  role
                  source)]

             (when-not
                 (contains?
                  @projected-states
                  state-id)
               (install-state!
                state-id
                {:op :return
                 :outcome complete-outcome}))

             state-id))

         (ensure-receive-gate!
           [source entries]
           (let [state-id
                 (synthetic-id
                  :receive
                  role
                  source)]

             (if (contains?
                  @projected-states
                  state-id)
               state-id

               (do
                 ;; Reserve before compiling post-receive continuations so a
                 ;; cycle may point back to this gate.
                 (reserve-state!
                  state-id)

                 (let [alternatives
                       (->> entries
                            (mapv
                             (fn [{global-state-id
                                   :state}]
                               (let [global-state
                                     (get global-states
                                          global-state-id)]

                                 (cond->
                                  {:from
                                   (:from global-state)

                                   :event
                                   (:event global-state)

                                   :next
                                   (ensure-continuation!
                                    (:next global-state))}
                                   (contains?
                                    global-state
                                    :via)
                                   (assoc
                                    :via
                                    (:via global-state))

                                   (seq
                                    (:required global-state))
                                   (assoc
                                    :required
                                    (:required global-state))

                                   (seq
                                    (:optional global-state))
                                   (assoc
                                    :optional
                                    (:optional global-state))

                                   (seq
                                    (:correlation global-state))
                                   (assoc
                                    :correlation
                                    (:correlation global-state))

                                   (true?
                                    (:open-payload? global-state))
                                   (assoc
                                    :open-payload?
                                    true)))))
                            (normalize-receive-alternatives
                             role
                             source))]

                   (install-state!
                    state-id
                    {:op :receive
                     :alternatives alternatives})

                   state-id)))))

         (ensure-continuation!
           [start-state]
           (if-some [cached
                     (get
                      @continuation-cache
                      start-state)]

             cached

             (let [frontier
                   (observable-frontier
                    choreography
                    role
                    start-state)

                   classification
                   (classify-frontier
                    role
                    start-state
                    frontier)]

               (case (:kind classification)
                 :done
                 (let [state-id
                       (ensure-complete!
                        start-state)]
                   (swap! continuation-cache
                          assoc
                          start-state
                          state-id)
                   state-id)

                 :receive
                 (let [state-id
                       (synthetic-id
                        :receive
                        role
                        start-state)]

                   ;; Publish the continuation identity before recursively
                   ;; compiling alternatives so cycles can return here.
                   (swap! continuation-cache
                          assoc
                          start-state
                          state-id)

                   (ensure-receive-gate!
                    start-state
                    (:entries
                     classification)))

                 :state
                 (let [state-id
                       (:state
                        classification)]

                   ;; Publish before recursively compiling this local state.
                   (swap! continuation-cache
                          assoc
                          start-state
                          state-id)

                   (ensure-local-state!
                    state-id))

                 (projection-error
                  :internal-frontier-kind
                  "Projection produced an unknown frontier classification."
                  {:role role
                   :state start-state
                   :classification
                   classification})))))

         (ensure-local-state!
           [state-id]
           (if (contains?
                @projected-states
                state-id)

             state-id

             (let [state
                   (get global-states
                        state-id)]

               (when-not state
                 (projection-error
                  :unknown-state
                  "Projection attempted to compile an unknown global state."
                  {:role role
                   :state state-id}))

               (reserve-state!
                state-id)

               (case (:op state)
                 :local
                 (do
                   (when-not (= role
                                (:role state))
                     (projection-error
                      :foreign-local-state
                      "Projection attempted to install another role's local action."
                      {:role role
                       :state state-id
                       :state-role
                       (:role state)}))

                   (install-state!
                    state-id
                    (cond->
                     {:op :local
                      :action (:action state)
                      :next
                      (ensure-continuation!
                       (:next state))}

                      (seq (choreo/local-requires state))
                      (assoc
                       :requires
                       (choreo/local-requires state))

                      (seq (choreo/local-outputs state))
                      (assoc
                       :outputs
                       (choreo/local-outputs state)))))

                 :authoritative
                 (do
                   (when-not (= role
                                (:role state))
                     (projection-error
                      :foreign-authoritative-state
                      "Projection attempted to install another role's authoritative operation."
                      {:role role
                       :state state-id
                       :state-role
                       (:role state)
                       :operation
                       (:operation state)}))

                   (install-state!
                    state-id
                    (cond->
                     {:op :authoritative
                      :operation
                      (choreo/authoritative-operation state)
                      :next
                      (ensure-continuation!
                       (:next state))}

                      (seq (choreo/authoritative-requires state))
                      (assoc
                       :requires
                       (choreo/authoritative-requires state))

                      (seq (choreo/authoritative-outputs state))
                      (assoc
                       :outputs
                       (choreo/authoritative-outputs state)))))

                 :branch
                 (do
                   (when-not (= role
                                (:role state))
                     (projection-error
                      :foreign-branch-state
                      "Projection attempted to install another role's branch."
                      {:role role
                       :state state-id
                       :state-role
                       (:role state)}))

                   (install-state!
                    state-id
                    {:op :branch
                     :on (:on state)
                     :cases
                     (into {}
                           (map
                            (fn [[value target]]
                              [value
                               (ensure-continuation!
                                target)]))
                           (:cases state))}))

                 :communicate
                 (do
                   (when-not (= role
                                (:from state))
                     (projection-error
                      :foreign-send
                      "Only the sender side of a global communication is installed as a projected :send state."
                      {:role role
                       :state state-id
                       :from (:from state)
                       :to (:to state)}))

                   (install-state!
                    state-id
                    (cond->
                     {:op :send
                      :to (:to state)
                      :event (:event state)
                      :next
                      (ensure-continuation!
                       (:next state))}
                      (contains?
                       state
                       :via)
                      (assoc
                       :via
                       (:via state))

                      (seq
                       (:required state))
                      (assoc
                       :required
                       (:required state))

                      (seq
                       (:optional state))
                      (assoc
                       :optional
                       (:optional state))

                      (seq
                       (:correlation state))
                      (assoc
                       :correlation
                       (:correlation state))

                      (true?
                       (:open-payload? state))
                      (assoc
                       :open-payload?
                       true))))

                 :await
                 (do
                   (when-not (= role
                                (:role state))
                     (projection-error
                      :foreign-await
                      "Projection attempted to install another role's environment wait."
                      {:role role
                       :state state-id
                       :state-role
                       (:role state)}))

                   (install-state!
                    state-id
                    {:op :await
                     :events
                     (into {}
                           (map
                            (fn [[event target]]
                              [event
                               (ensure-continuation!
                                target)]))
                           (:events state))}))

                 :return
                 (projection-error
                  :raw-global-return
                  "Global return states are compiled to local completion states."
                  {:role role
                   :state state-id})

                 (projection-error
                  :unsupported-local-op
                  "Projection cannot install this global operation as a local state."
                  {:role role
                   :state state-id
                   :op (:op state)})))))]

      (let [initial
            (ensure-continuation!
             (:initial choreography))

            states
            @projected-states

            leaked-building
            (set
             (for [[state-id state]
                   states
                   :when
                   (= :gesso.choreo.project/building
                      (:op state))]
               state-id))]

        (when (seq leaked-building)
          (projection-error
           :incomplete-projection
           "Projection left compiler placeholder states in the emitted plan."
           {:role role
            :states leaked-building}))

        {:gesso.choreo/type
         projected-plan-type

         :gesso.choreo/version
         projected-plan-version

         :name
         (:name choreography)

         :role
         role

         :initial
         initial

         :states
         states}))))

(defn project-all
  "Project choreography once for every inferred role.

   Returns role -> projected plan."
  [choreography-or-verified]
  (let [verified
        (verify/ensure-verified
         choreography-or-verified)

        choreography
        (:choreography verified)]

    (into {}
          (map
           (fn [role]
             [role
              (project
               verified
               role)]))
          (sort-by pr-str
                   (choreo/roles
                    choreography)))))

;; -----------------------------------------------------------------------------
;; Inspection
;; -----------------------------------------------------------------------------

(defn state
  "Return one projected state by id."
  [plan state-id]
  (get (:states
        (ensure-projected-plan plan))
       state-id))

(defn explain
  "Return a compact stable projected-plan summary."
  [plan]
  (let [plan'
        (ensure-projected-plan plan)

        states
        (:states plan')]

    {:name
     (:name plan')

     :role
     (:role plan')

     :version
     (:gesso.choreo/version plan')

     :initial
     (:initial plan')

     :state-count
     (count states)

     :states-by-op
     (frequencies
      (map
       (comp :op val)
       states))}))
