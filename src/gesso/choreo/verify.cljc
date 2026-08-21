(ns gesso.choreo.verify
  "Static verification for the current Gesso Choreo semantic core.

   This verifier deliberately proves only properties defined by the current
   portable language. It currently checks:

   - structural validity through gesso.choreo.core/semantics;
   - graph reachability and terminal-path structure;
   - definite protocol-value availability for local/authoritative :requires,
     branch selectors, and required communicated fields;
   - role-local definite-knowledge availability;
   - sender knowledge for required communicated fields;
   - receiver knowledge established by required communicated fields;
   - awaiting-role knowledge established by required environment-event fields;
   - edge-sensitive definite-value/knowledge transfer for :await alternatives;
   - basic value producer/use analysis;
   - explicit identification of authoritative semantic operations in analysis.

   Definite protocol-value availability is a small forward must-analysis. A
   value is considered established at a state only when it is available on every
   graph path reaching that state. Local and authoritative outputs establish
   values, required communicated fields establish values at the communication
   boundary, and required environment-event fields establish values on the
   specific event edge that was taken. Optional communicated/environment fields
   are not definite because they may be omitted.

   A second must-analysis tracks role-local knowledge. Local/authoritative outputs
   become known to their owner. Required communicated fields must already be
   known by the sender and become known by the receiver. Required environment
   fields become known only to the role awaiting that event. This is a static
   role-local knowledge check; it does not yet prove provenance quality,
   authentication, authorization, or optional-field sender knowledge.

   Entry assumptions may be supplied in two forms:

     {:entry-value-keys #{...}}

   means low-level protocol values available to every role. It is intentionally
   broad and remains useful for tiny tests and compiler internals.

     {:entry-knowledge
      {:server #{:principal :request-id}
       :browser #{:request-id}}}

   is the precise role-local form and should be preferred when role knowledge
   matters. Keys appearing in :entry-knowledge also count as globally available
   protocol values, but only the named role knows them.

   Likewise, a graph path to a terminal is not a liveness proof. Environment
   events may never occur and a cycle may be taken forever.

   This namespace does NOT yet claim to verify:

   - projection/refinement correctness;
   - provenance quality beyond static role/key flow;
   - authentication, authorization, or correctness of authoritative realization;
   - optional communicated fields are known by the sender when actually sent;
   - optional environment fields as definite knowledge;
   - open-payload/open-event extras as semantic knowledge;
   - resource ownership;
   - browser execution;
   - arbitrary liveness.

   Verification results are compiler/static-analysis data. They confer no
   runtime authority."
  (:require
   [clojure.set :as set]
   [gesso.choreo.core :as choreo]))

;; -----------------------------------------------------------------------------
;; Identity
;; -----------------------------------------------------------------------------

(def verification-version
  5)

(def verification-type
  :gesso.choreo/verification)

(def verified-type
  :gesso.choreo/verified)

;; -----------------------------------------------------------------------------
;; Problems / errors
;; -----------------------------------------------------------------------------

(defn problem
  "Construct one stable verifier problem."
  ([kind path message]
   (problem kind path message nil))
  ([kind path message data]
   (cond->
    {:kind kind
     :path (vec path)
     :message message}
     (some? data)
     (assoc :data data))))

(defn- fail!
  [kind message data]
  (throw
   (ex-info
    message
    (merge
     {:error/type :gesso.choreo.verify/error
      :error/kind kind}
     data))))

(defn- verification-error
  [message result]
  (throw
   (ex-info
    message
    {:error/type :gesso.choreo/verification-failed
     :verification result
     :errors (:errors result)
     :warnings (:warnings result)})))

;; -----------------------------------------------------------------------------
;; Verification options
;; -----------------------------------------------------------------------------

(defn- normalize-entry-value-keys
  [value]
  (let [value'
        (or value #{})]
    (when-not (and (set? value')
                   (every? keyword? value'))
      (fail!
       :invalid-entry-value-keys
       "Verifier :entry-value-keys must be a set of keywords."
       {:entry-value-keys value}))
    value'))

(defn- normalize-entry-knowledge
  [value]
  (let [value'
        (or value {})]
    (when-not (map? value')
      (fail!
       :invalid-entry-knowledge
       "Verifier :entry-knowledge must be a map of role keyword to keyword set."
       {:entry-knowledge value}))

    (into {}
          (map
           (fn [[role value-keys]]
             (when-not (keyword? role)
               (fail!
                :invalid-entry-knowledge
                "Verifier :entry-knowledge role keys must be keywords."
                {:entry-knowledge value
                 :role role}))
             [role
              (normalize-entry-value-keys
               value-keys)]))
          value')))

(defn- normalize-options
  [options]
  (let [options'
        (or options {})]
    (when-not (map? options')
      (fail!
       :invalid-options
       "Verifier options must be a map."
       {:options options}))

    {:entry-value-keys
     (normalize-entry-value-keys
      (:entry-value-keys options'))

     :entry-knowledge
     (normalize-entry-knowledge
      (:entry-knowledge options'))}))

;; -----------------------------------------------------------------------------
;; Graph
;; -----------------------------------------------------------------------------

(defn- successor-map
  [states]
  (into {}
        (map
         (fn [[state-id state]]
           [state-id
            (choreo/successors state)]))
        states))

(defn- predecessor-map
  [states successors]
  (reduce-kv
   (fn [predecessors state-id targets]
     (reduce
      (fn [predecessors target]
        (update predecessors
                target
                (fnil conj #{})
                state-id))
      predecessors
      targets))
   (zipmap
    (keys states)
    (repeat #{}))
   successors))

(defn- reachable-state-ids
  [initial successors]
  (loop [pending [initial]
         index 0
         seen #{}]
    (if (= index
           (count pending))
      seen

      (let [state-id
            (nth pending index)]
        (if (contains? seen state-id)
          (recur pending
                 (inc index)
                 seen)

          (recur
           (into pending
                 (remove seen
                         (get successors
                              state-id
                              #{})))
           (inc index)
           (conj seen state-id)))))))

(defn- terminal-state-ids
  [states reachable]
  (set
   (for [state-id reachable
         :let [state (get states state-id)]
         :when (choreo/terminal-state? state)]
     state-id)))

(defn- reverse-reachable
  [starts predecessors allowed]
  (loop [pending (vec starts)
         index 0
         seen #{}]
    (if (= index
           (count pending))
      seen

      (let [state-id
            (nth pending index)]
        (if (or (contains? seen state-id)
                (not (contains? allowed state-id)))
          (recur pending
                 (inc index)
                 seen)

          (recur
           (into pending
                 (get predecessors
                      state-id
                      #{}))
           (inc index)
           (conj seen state-id)))))))

;; -----------------------------------------------------------------------------
;; Reachability diagnostics
;; -----------------------------------------------------------------------------

(defn- unreachable-state-warnings
  [states reachable]
  (vec
   (for [state-id
         (sort-by pr-str
                  (set/difference
                   (set (keys states))
                   reachable))]
     (problem
      :unreachable-state
      [:states state-id]
      "Declared state is unreachable from the choreography initial state."
      {:state state-id}))))

(defn- missing-terminal-errors
  [reachable terminals]
  (when (and (seq reachable)
             (empty? terminals))
    [(problem
      :missing-reachable-terminal
      [:states]
      "The reachable choreography graph contains no global terminal state."
      {:reachable-state-ids reachable})]))

(defn- no-terminal-path-errors
  [reachable can-reach-terminal]
  (vec
   (for [state-id
         (sort-by pr-str
                  (set/difference
                   reachable
                   can-reach-terminal))]
     (problem
      :no-terminal-path
      [:states state-id]
      "Reachable state has no graph path to a global terminal outcome."
      {:state state-id}))))

;; -----------------------------------------------------------------------------
;; Semantic-value use / production
;; -----------------------------------------------------------------------------

(defn- await-state?
  [state]
  (= :await
     (:op state)))

(defn- await-event-contract
  [state event]
  (get-in state
          [:event-contracts event]
          {}))

(defn- await-event-required
  [state event]
  (or (:required
       (await-event-contract
        state
        event))
      #{}))

(defn- await-event-optional
  [state event]
  (or (:optional
       (await-event-contract
        state
        event))
      #{}))

(defn- await-events-for-target
  [state target]
  (keep
   (fn [[event successor]]
     (when (= target successor)
       event))
   (:events state)))

(defn- await-produced-value-keys
  "Return values that this await may establish on at least one event edge.

   This is a may-produce summary used for universe/diagnostic construction. The
   definite analysis below is edge-sensitive and therefore does not treat this
   union as established on every outgoing path."
  [state]
  (reduce
   set/union
   #{}
   (map
    #(await-event-required
      state
      %)
    (keys (:events state)))))

(defn- await-definite-value-keys-for-target
  "Return values established on every environment event from state to target.

   Multiple event labels may converge directly on the same successor. A fact is
   definite at that successor only when every such event contract requires it."
  [state target]
  (let [events
        (vec
         (await-events-for-target
          state
          target))]
    (if (seq events)
      (reduce
       set/intersection
       (await-event-required
        state
        (first events))
       (map
        #(await-event-required
          state
          %)
        (rest events)))
      #{})))

(defn- state-produced-value-keys
  [state]
  (cond
    (choreo/local-state? state)
    (choreo/local-outputs state)

    (choreo/authoritative-state? state)
    (choreo/authoritative-outputs state)

    (choreo/communication-state? state)
    ;; Required fields are present on every successful communication and become
    ;; definite receiver knowledge. Optional fields may be omitted.
    (choreo/communication-required state)

    (await-state? state)
    (await-produced-value-keys state)

    :else
    #{}))

(defn- edge-produced-value-keys
  "Return values definitely produced by taking state -> target.

   Most states have path-independent production. :await is different: each
   environment event may establish a different required field set, so its
   transfer is computed per successor edge."
  [state target]
  (if (await-state? state)
    (await-definite-value-keys-for-target
     state
     target)
    (state-produced-value-keys state)))

(defn- state-required-value-keys
  [state]
  (cond
    (choreo/local-state? state)
    (choreo/local-requires state)

    (choreo/authoritative-state? state)
    (choreo/authoritative-requires state)

    (choreo/branch-state? state)
    #{(choreo/branch-key state)}

    (choreo/communication-state? state)
    ;; A sender cannot construct a required semantic field out of nothing.
    (choreo/communication-required state)

    :else
    #{}))

(defn- produced-value-keys
  [states state-ids]
  (reduce
   set/union
   #{}
   (map
    #(state-produced-value-keys
      (get states %))
    state-ids)))

(defn- required-value-keys
  [states state-ids]
  (reduce
   set/union
   #{}
   (map
    #(state-required-value-keys
      (get states %))
    state-ids)))

(defn- value-producers
  [states state-ids]
  (reduce
   (fn [producers state-id]
     (reduce
      (fn [producers value-key]
        (update producers
                value-key
                (fnil conj #{})
                state-id))
      producers
      (state-produced-value-keys
       (get states state-id))))
   {}
   state-ids))

(defn- value-consumers
  [states state-ids]
  (reduce
   (fn [consumers state-id]
     (reduce
      (fn [consumers value-key]
        (update consumers
                value-key
                (fnil conj #{})
                state-id))
      consumers
      (state-required-value-keys
       (get states state-id))))
   {}
   state-ids))

;; -----------------------------------------------------------------------------
;; Definite-value dataflow
;; -----------------------------------------------------------------------------

(defn- transfer-values-to-target
  [state target incoming]
  (set/union
   incoming
   (edge-produced-value-keys
    state
    target)))

(defn- intersect-all
  [sets universe]
  (if (seq sets)
    (reduce set/intersection
            universe
            sets)
    universe))

(defn- predecessor-value-contribution
  [states
   predecessor-id
   target-id
   in-values
   universe]
  (transfer-values-to-target
   (get states predecessor-id)
   target-id
   (get in-values
        predecessor-id
        universe)))

(defn- incoming-values-for-state
  [states
   state-id
   initial
   entry-value-keys
   predecessors
   in-values
   universe]
  (let [predecessor-values
        (map
         #(predecessor-value-contribution
           states
           %
           state-id
           in-values
           universe)
         (get predecessors state-id #{}))

        incoming-from-predecessors
        (intersect-all
         predecessor-values
         universe)]

    (if (= state-id initial)
      ;; Initial state has an implicit operation-entry predecessor. Even when a
      ;; graph back-edge reaches initial later, a value cannot be considered
      ;; definitely available before the first execution merely because that
      ;; back-edge happens to produce it.
      (set/intersection
       entry-value-keys
       incoming-from-predecessors)

      incoming-from-predecessors)))

(defn- values-after-state
  [state incoming]
  (let [targets
        (choreo/successors state)]
    (if (seq targets)
      (intersect-all
       (map
        #(transfer-values-to-target
          state
          %
          incoming)
        targets)
       (set/union
        incoming
        (state-produced-value-keys state)))
      incoming)))

(defn- definite-value-analysis
  [states
   initial
   reachable
   predecessors
   entry-value-keys]
  (let [all-produced
        (produced-value-keys
         states
         reachable)

        universe
        (set/union
         entry-value-keys
         all-produced)

        initial-in
        (into {}
              (map
               (fn [state-id]
                 [state-id
                  (if (= state-id initial)
                    entry-value-keys
                    universe)]))
              reachable)]

    (loop [in-values initial-in]
      (let [next-in
            (into {}
                  (map
                   (fn [state-id]
                     [state-id
                      (incoming-values-for-state
                       states
                       state-id
                       initial
                       entry-value-keys
                       predecessors
                       in-values
                       universe)]))
                  reachable)]

        (if (= in-values next-in)
          {:entry-value-keys
           entry-value-keys

           :universe
           universe

           :in
           next-in

           :out
           (into {}
                 (map
                  (fn [[state-id incoming]]
                    [state-id
                     (values-after-state
                      (get states state-id)
                      incoming)]))
                 next-in)}

          (recur next-in))))))

(defn- definite-value-errors
  [states reachable in-values]
  (vec
   (mapcat
    (fn [state-id]
      (let [state
            (get states state-id)

            required
            (state-required-value-keys state)

            established
            (get in-values state-id #{})

            missing
            (set/difference
             required
             established)]

        (for [value-key
              (sort-by pr-str missing)]
          (problem
           :value-not-definitely-established
           [:states state-id]
           "State consumes a semantic value that is not established on every path reaching it."
           {:state state-id
            :op (:op state)
            :role (choreo/state-owner state)
            :value-key value-key
            :required required
            :definitely-established established}))))
    (sort-by pr-str reachable))))

;; -----------------------------------------------------------------------------
;; Role-local definite knowledge
;; -----------------------------------------------------------------------------

(defn- empty-role-knowledge
  [roles]
  (zipmap
   roles
   (repeat #{})))

(defn- normalize-role-knowledge
  [roles knowledge-by-role]
  (merge
   (empty-role-knowledge roles)
   knowledge-by-role))

(defn- entry-knowledge-by-role
  [roles entry-value-keys entry-knowledge]
  (into {}
        (map
         (fn [role]
           [role
            (set/union
             entry-value-keys
             (get entry-knowledge role #{}))]))
        roles))

(defn- state-required-knowledge
  [state]
  (cond
    (choreo/local-state? state)
    {(choreo/state-owner state)
     (choreo/local-requires state)}

    (choreo/authoritative-state? state)
    {(choreo/state-owner state)
     (choreo/authoritative-requires state)}

    (choreo/branch-state? state)
    {(choreo/state-owner state)
     #{(choreo/branch-key state)}}

    (choreo/communication-state? state)
    {(:from state)
     (choreo/communication-required state)}

    :else
    {}))

(defn- await-produced-knowledge
  [state]
  {(choreo/state-owner state)
   (await-produced-value-keys state)})

(defn- state-produced-knowledge
  [state]
  (cond
    (choreo/local-state? state)
    {(choreo/state-owner state)
     (choreo/local-outputs state)}

    (choreo/authoritative-state? state)
    {(choreo/state-owner state)
     (choreo/authoritative-outputs state)}

    (choreo/communication-state? state)
    {(:to state)
     (choreo/communication-required state)}

    (await-state? state)
    (await-produced-knowledge state)

    :else
    {}))

(defn- edge-produced-knowledge
  [state target]
  (if (await-state? state)
    {(choreo/state-owner state)
     (await-definite-value-keys-for-target
      state
      target)}
    (state-produced-knowledge state)))

(defn- transfer-knowledge-to-target
  [state target incoming]
  (reduce-kv
   (fn [knowledge role keys]
     (update knowledge
             role
             (fnil set/union #{})
             keys))
   incoming
   (edge-produced-knowledge
    state
    target)))

(defn- intersect-role-knowledge
  [roles maps universe-by-role]
  (if (seq maps)
    (into {}
          (map
           (fn [role]
             [role
              (reduce
               set/intersection
               (get universe-by-role role #{})
               (map
                #(get % role #{})
                maps))]))
          roles)
    universe-by-role))

(defn- predecessor-knowledge-contribution
  [states
   predecessor-id
   target-id
   in-knowledge
   universe-by-role]
  (transfer-knowledge-to-target
   (get states predecessor-id)
   target-id
   (get in-knowledge
        predecessor-id
        universe-by-role)))

(defn- incoming-knowledge-for-state
  [states
   state-id
   initial
   roles
   entry-by-role
   predecessors
   in-knowledge
   universe-by-role]
  (let [predecessor-knowledge
        (map
         #(predecessor-knowledge-contribution
           states
           %
           state-id
           in-knowledge
           universe-by-role)
         (get predecessors state-id #{}))

        incoming-from-predecessors
        (intersect-role-knowledge
         roles
         predecessor-knowledge
         universe-by-role)]

    (if (= state-id initial)
      ;; As with global value flow, a back-edge into initial cannot establish a
      ;; fact before the first execution.
      (into {}
            (map
             (fn [role]
               [role
                (set/intersection
                 (get entry-by-role role #{})
                 (get incoming-from-predecessors role #{}))]))
            roles)

      incoming-from-predecessors)))

(defn- knowledge-after-state
  [roles state incoming universe-by-role]
  (let [targets
        (choreo/successors state)]
    (if (seq targets)
      (intersect-role-knowledge
       roles
       (map
        #(transfer-knowledge-to-target
          state
          %
          incoming)
        targets)
       universe-by-role)
      incoming)))

(defn- definite-knowledge-analysis
  [states
   initial
   reachable
   predecessors
   roles
   entry-value-keys
   entry-knowledge]
  (let [entry-by-role
        (entry-knowledge-by-role
         roles
         entry-value-keys
         entry-knowledge)

        all-protocol-values
        (set/union
         entry-value-keys
         (reduce
          set/union
          #{}
          (vals entry-knowledge))
         (produced-value-keys
          states
          reachable))

        universe-by-role
        (zipmap
         roles
         (repeat all-protocol-values))

        initial-in
        (into {}
              (map
               (fn [state-id]
                 [state-id
                  (if (= state-id initial)
                    entry-by-role
                    universe-by-role)]))
              reachable)]

    (loop [in-knowledge initial-in]
      (let [next-in
            (into {}
                  (map
                   (fn [state-id]
                     [state-id
                      (incoming-knowledge-for-state
                       states
                       state-id
                       initial
                       roles
                       entry-by-role
                       predecessors
                       in-knowledge
                       universe-by-role)]))
                  reachable)]

        (if (= in-knowledge next-in)
          {:entry
           entry-by-role

           :universe
           universe-by-role

           :in
           next-in

           :out
           (into {}
                 (map
                  (fn [[state-id incoming]]
                    [state-id
                     (knowledge-after-state
                      roles
                      (get states state-id)
                      incoming
                      universe-by-role)]))
                 next-in)}

          (recur next-in))))))

(defn- definite-knowledge-errors
  [states reachable global-in role-in]
  (vec
   (mapcat
    (fn [state-id]
      (let [state
            (get states state-id)

            globally-established
            (get global-in state-id #{})

            required-by-role
            (state-required-knowledge state)]

        (mapcat
         (fn [[role required]]
           (let [known
                 (get-in role-in
                         [state-id role]
                         #{})

                 ;; When a value is missing globally, the existing global
                 ;; diagnostic is the clearer root cause. Emit a knowledge error
                 ;; only for the genuinely distributed case: the protocol has
                 ;; the value, but this role does not.
                 missing
                 (set/difference
                  required
                  known
                  (set/difference
                   required
                   globally-established))]

             (for [value-key
                   (sort-by pr-str missing)]
               (problem
                :knowledge-not-definitely-established
                [:states state-id]
                "Role consumes or transmits a semantic value that the protocol may have, but this role does not definitely know on every path reaching the state."
                {:state state-id
                 :op (:op state)
                 :role role
                 :value-key value-key
                 :required required
                 :definitely-known known
                 :globally-established
                 globally-established}))))
         required-by-role)))
    (sort-by pr-str reachable))))

(defn- knowledge-producers-by-role
  [states state-ids]
  (reduce
   (fn [result state-id]
     (let [additions
           (state-produced-knowledge
            (get states state-id))]

       (reduce-kv
        (fn [result role keys]
          (reduce
           (fn [result key]
             (update-in result
                        [role key]
                        (fnil conj #{})
                        state-id))
           result
           keys))
        result
        additions)))
   {}
   state-ids))

(defn- knowledge-consumers-by-role
  [states state-ids]
  (reduce
   (fn [result state-id]
     (reduce-kv
      (fn [result role keys]
        (reduce
         (fn [result key]
           (update-in result
                      [role key]
                      (fnil conj #{})
                      state-id))
         result
         keys))
      result
      (state-required-knowledge
       (get states state-id))))
   {}
   state-ids))

;; -----------------------------------------------------------------------------
;; Semantic summaries
;; -----------------------------------------------------------------------------

(defn- states-by-op
  [states state-ids]
  (frequencies
   (map
    (fn [state-id]
      (:op
       (get states state-id)))
    state-ids)))

(defn- roles-by-state
  [states state-ids]
  (into {}
        (map
         (fn [state-id]
           [state-id
            (choreo/state-roles
             (get states state-id))]))
        state-ids))

(defn- ids-by-op
  [states state-ids op]
  (set
   (filter
    (fn [state-id]
      (= op
         (:op
          (get states state-id))))
    state-ids)))

;; -----------------------------------------------------------------------------
;; Verification
;; -----------------------------------------------------------------------------

(defn verify
  "Analyze choreography and return a non-throwing verification result after
   structural choreography normalization succeeds.

   Options:

     :entry-value-keys
       Broad low-level entry assumptions treated as known by every role.

     :entry-knowledge
       Precise map of role -> set of keys known by that role at entry.

   A valid result means only that the checks named by this namespace passed. It
   is not a proof of projection/refinement, provenance truth, authoritative
   realization, authorization, or liveness."
  ([choreography]
   (verify choreography nil))
  ([choreography options]
   (let [{:keys [entry-value-keys
                   entry-knowledge]}
         (normalize-options options)

         choreography'
         (choreo/ensure-choreography choreography)

         states
         (:states choreography')

         initial
         (:initial choreography')

         successors
         (successor-map states)

         predecessors
         (predecessor-map
          states
          successors)

         reachable
         (reachable-state-ids
          initial
          successors)

         unreachable
         (set/difference
          (set (keys states))
          reachable)

         terminals
         (terminal-state-ids
          states
          reachable)

         can-reach-terminal
         (reverse-reachable
          terminals
          predecessors
          reachable)

         roles
         (choreo/roles choreography')

         protocol-entry-value-keys
         (reduce
          set/union
          entry-value-keys
          (vals entry-knowledge))

         value-analysis
         (definite-value-analysis
          states
          initial
          reachable
          predecessors
          protocol-entry-value-keys)

         knowledge-analysis
         (definite-knowledge-analysis
          states
          initial
          reachable
          predecessors
          roles
          entry-value-keys
          entry-knowledge)

         graph-errors
         (concat
          (missing-terminal-errors
           reachable
           terminals)

          (no-terminal-path-errors
           reachable
           can-reach-terminal))

         dataflow-errors
         (definite-value-errors
          states
          reachable
          (:in value-analysis))

         knowledge-errors
         (definite-knowledge-errors
          states
          reachable
          (:in value-analysis)
          (:in knowledge-analysis))

         errors
         (vec
          (concat
           graph-errors
           dataflow-errors
           knowledge-errors))

         warnings
         (unreachable-state-warnings
          states
          reachable)

         reachable-produced
         (produced-value-keys
          states
          reachable)

         reachable-required
         (required-value-keys
          states
          reachable)]

     {:gesso.choreo/type
      verification-type

      :gesso.choreo/version
      verification-version

      :valid?
      (empty? errors)

      :choreography
      choreography'

      :options
      {:entry-value-keys
       entry-value-keys

       :entry-knowledge
       entry-knowledge}

      :errors
      errors

      :warnings
      warnings

      :analysis
      {:initial
       initial

       :roles
       roles

       :reachable-state-ids
       reachable

       :unreachable-state-ids
       unreachable

       :terminal-state-ids
       terminals

       :can-reach-terminal-state-ids
       can-reach-terminal

       :successors
       successors

       :predecessors
       predecessors

       :states-by-op
       (states-by-op
        states
        reachable)

       :roles-by-state
       (roles-by-state
        states
        reachable)

       :local-state-ids
       (ids-by-op
        states
        reachable
        :local)

       :authoritative-state-ids
       (ids-by-op
        states
        reachable
        :authoritative)

       :branch-state-ids
       (ids-by-op
        states
        reachable
        :branch)

       :communication-state-ids
       (ids-by-op
        states
        reachable
        :communicate)

       :await-state-ids
       (ids-by-op
        states
        reachable
        :await)

       :authoritative-operations-by-state
       (into {}
             (keep
              (fn [state-id]
                (let [state
                      (get states state-id)]
                  (when (choreo/authoritative-state? state)
                    [state-id
                     (choreo/authoritative-operation state)]))))
             reachable)

       :entry-value-keys
       entry-value-keys

       :entry-knowledge
       (:entry knowledge-analysis)

       :protocol-entry-value-keys
       protocol-entry-value-keys

       :produced-value-keys
       reachable-produced

       :required-value-keys
       reachable-required

       :value-producers
       (value-producers
        states
        reachable)

       :value-consumers
       (value-consumers
        states
        reachable)

       :definitely-established-before-state
       (:in value-analysis)

       :definitely-established-after-state
       (:out value-analysis)

       :definitely-known-before-state
       (:in knowledge-analysis)

       :definitely-known-after-state
       (:out knowledge-analysis)

       :knowledge-producers-by-role
       (knowledge-producers-by-role
        states
        reachable)

       :knowledge-consumers-by-role
       (knowledge-consumers-by-role
        states
        reachable)

       :communicated-required-keys-by-state
       (into {}
             (keep
              (fn [state-id]
                (let [state
                      (get states state-id)]
                  (when (choreo/communication-state? state)
                    [state-id
                     (choreo/communication-required state)]))))
             reachable)

       :communicated-optional-keys-by-state
       (into {}
             (keep
              (fn [state-id]
                (let [state
                      (get states state-id)]
                  (when (choreo/communication-state? state)
                    [state-id
                     (choreo/communication-optional state)]))))
             reachable)

       :environment-required-keys-by-state
       (into {}
             (keep
              (fn [state-id]
                (let [state
                      (get states state-id)]
                  (when (await-state? state)
                    [state-id
                     (into {}
                           (map
                            (fn [event]
                              [event
                               (await-event-required
                                state
                                event)]))
                           (keys (:events state)))]))))
             reachable)

       :environment-optional-keys-by-state
       (into {}
             (keep
              (fn [state-id]
                (let [state
                      (get states state-id)]
                  (when (await-state? state)
                    [state-id
                     (into {}
                           (map
                            (fn [event]
                              [event
                               (await-event-optional
                                state
                                event)]))
                           (keys (:events state)))]))))
             reachable)}})))

(defn verification?
  "True when x is a verifier result emitted by this verifier version."
  [x]
  (and (map? x)
       (= verification-type
          (:gesso.choreo/type x))
       (= verification-version
          (:gesso.choreo/version x))
       (boolean?
        (:valid? x))
       (vector?
        (:errors x))
       (vector?
        (:warnings x))
       (map?
        (:analysis x))))

(defn verified?
  "True when x is a successful verified wrapper."
  [x]
  (and (map? x)
       (= verified-type
          (:gesso.choreo/type x))
       (= verification-version
          (:gesso.choreo/version x))
       (choreo/choreography?
        (:choreography x))
       (verification?
        (:verification x))
       (true?
        (get-in x
                [:verification :valid?]))))

(defn verify!
  "Verify choreography or throw ExceptionInfo containing the complete result."
  ([choreography]
   (verify! choreography nil))
  ([choreography options]
   (let [result
         (verify choreography options)]
     (if (:valid? result)
       {:gesso.choreo/type
        verified-type

        :gesso.choreo/version
        verification-version

        :choreography
        (:choreography result)

        :verification
        result}

       (verification-error
        "Gesso choreography verification failed."
        result)))))

(defn ensure-verified
  "Return a successful verified wrapper.

   With one argument accepts:

   - a successful verified wrapper;
   - a successful verification result;
   - choreography data, verified with no entry values.

   The two-argument form is intended for choreography data and applies explicit
   verifier options. Passing options alongside an already-produced verification
   artifact is rejected so assumptions cannot silently change after analysis."
  ([x]
   (cond
     (verified? x)
     x

     (verification? x)
     (if (:valid? x)
       {:gesso.choreo/type
        verified-type

        :gesso.choreo/version
        verification-version

        :choreography
        (:choreography x)

        :verification
        x}

       (verification-error
        "Cannot use an invalid Gesso choreography verification result."
        x))

     :else
     (verify! x)))

  ([x options]
   (when (or (verified? x)
             (verification? x))
     (fail!
      :options-with-verification-artifact
      "Verifier options cannot be reapplied to an existing verification artifact."
      {:options options
       :value x}))

   (verify! x options)))

;; -----------------------------------------------------------------------------
;; Inspection
;; -----------------------------------------------------------------------------

(defn explain
  "Return a compact verifier summary suitable for REPL use."
  [verification-or-choreography]
  (let [verification
        (cond
          (verified? verification-or-choreography)
          (:verification
           verification-or-choreography)

          (verification? verification-or-choreography)
          verification-or-choreography

          :else
          (verify
           verification-or-choreography))

        analysis
        (:analysis verification)]

    {:valid?
     (:valid? verification)

     :error-count
     (count
      (:errors verification))

     :warning-count
     (count
      (:warnings verification))

     :roles
     (:roles analysis)

     :reachable-state-count
     (count
      (:reachable-state-ids analysis))

     :terminal-state-count
     (count
      (:terminal-state-ids analysis))

     :states-by-op
     (:states-by-op analysis)}))
