(ns gesso.choreo.verify
  "Static verification for the current Gesso Choreo semantic core.

   This verifier deliberately proves only properties defined by the current
   portable language. It currently checks:

   - structural validity through gesso.choreo.core/semantics;
   - graph reachability and terminal-path structure;
   - definite semantic-value availability for local/authoritative :requires and
     :branch selectors;
   - basic value producer/use analysis useful to later knowledge/type work;
   - explicit identification of authoritative semantic operations in analysis.

   Definite value availability is a small forward must-analysis. A value is
   considered established at a state only when it is available on every graph
   path reaching that state. Local and authoritative outputs monotonically add
   established value keys; this semantic slice has no value deletion.

   Entry values are supplied explicitly to verification with

     {:entry-value-keys #{...}}

   rather than inferred from arbitrary runtime context. This is intentionally a
   temporary low-level contract until the richer choreography type vocabulary
   gives operation inputs schemas, provenance, and role knowledge.

   Importantly, this is still GLOBAL value availability, not actor knowledge.
   Passing this verifier does not prove that the role owning a local action or
   branch justifiably knows the values it consumes. Knowledge/provenance remains
   a later and separate proof obligation.

   Likewise, a graph path to a terminal is not a liveness proof. Environment
   events may never occur and a cycle may be taken forever.

   This namespace does NOT yet claim to verify:

   - projection/refinement correctness;
   - role-local knowledge or provenance;
   - authentication, authorization, or correctness of authoritative realization;
   - closed message payload contracts;
   - communication value transfer;
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
  3)

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
      (:entry-value-keys options'))}))

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

(defn- state-produced-value-keys
  [state]
  (cond
    (choreo/local-state? state)
    (choreo/local-outputs state)

    (choreo/authoritative-state? state)
    (choreo/authoritative-outputs state)

    :else
    #{}))

(defn- state-required-value-keys
  [state]
  (cond
    (choreo/local-state? state)
    (choreo/local-requires state)

    (choreo/authoritative-state? state)
    (choreo/authoritative-requires state)

    (choreo/branch-state? state)
    #{(choreo/branch-key state)}

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

(defn- transfer-values
  [state incoming]
  (set/union
   incoming
   (state-produced-value-keys state)))

(defn- intersect-all
  [sets universe]
  (if (seq sets)
    (reduce set/intersection
            universe
            sets)
    universe))

(defn- incoming-values-for-state
  [state-id
   initial
   entry-value-keys
   predecessors
   out-values
   universe]
  (let [predecessor-values
        (map
         #(get out-values % universe)
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
              reachable)

        initial-out
        (into {}
              (map
               (fn [[state-id incoming]]
                 [state-id
                  (transfer-values
                   (get states state-id)
                   incoming)]))
              initial-in)]

    (loop [in-values initial-in
           out-values initial-out]
      (let [next-in
            (into {}
                  (map
                   (fn [state-id]
                     [state-id
                      (incoming-values-for-state
                       state-id
                       initial
                       entry-value-keys
                       predecessors
                       out-values
                       universe)]))
                  reachable)

            next-out
            (into {}
                  (map
                   (fn [[state-id incoming]]
                     [state-id
                      (transfer-values
                       (get states state-id)
                       incoming)]))
                  next-in)]

        (if (and (= in-values next-in)
                 (= out-values next-out))
          {:entry-value-keys
           entry-value-keys

           :universe
           universe

           :in
           next-in

           :out
           next-out}

          (recur next-in
                 next-out))))))

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
       Set of semantic value keys established at operation entry for purposes of
       this low-level definite-value analysis. Defaults to #{}.

   A valid result means only that the checks named by this namespace passed. It
   is not a proof of projection, role knowledge, authoritative realization, or
   liveness."
  ([choreography]
   (verify choreography nil))
  ([choreography options]
   (let [{:keys [entry-value-keys]}
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

         value-analysis
         (definite-value-analysis
          states
          initial
          reachable
          predecessors
          entry-value-keys)

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

         errors
         (vec
          (concat
           graph-errors
           dataflow-errors))

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
       entry-value-keys}

      :errors
      errors

      :warnings
      warnings

      :analysis
      {:initial
       initial

       :roles
       (choreo/roles choreography')

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
       (:out value-analysis)}})))

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
