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
       this role waits for one of its own environment events, retaining any
       declared closed semantic event-data contracts

     :return
       this role has no further work in this choreography

   Projection deliberately distinguishes participant communication from
   environment events. A projected :receive is never satisfied by an
   environment event, and a projected :await is never satisfied by a participant
   message merely because the event keyword is the same. Declared environment
   event-data contracts survive projection so the role-local machine can enforce
   the same semantic boundary and establish only declared event data.

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

   The emitted artifact is a canonical ExecutablePlan. Semantic/source state
   identities are compiler-only: production plans use compact non-negative
   integer runtime locators and contain no proof or diagnostic sidecar data.
   The exact source/proof mapping belongs to the sibling compiler sidecar, not
   to this runtime artifact.

   Projection still does not claim a refinement theorem merely by emitting a
   canonical plan. Correspondence and proof namespaces state their narrower
   current guarantees separately.

   This namespace contains no browser, HTMX, transport, XTDB implementation,
   authentication, knowledge/provenance proof, optimism, or model-specific
   behavior."
  (:require
   [clojure.set :as set]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.type :as type]
   [gesso.choreo.verify :as verify]))

;; -----------------------------------------------------------------------------
;; Identity
;; -----------------------------------------------------------------------------

(def executable-plan-version
  1)

(def executable-plan-type
  :gesso.choreo/executable-plan)

(def compiler-projection-version
  3)

(def compiler-projection-type
  :gesso.choreo/compiler-projection)

;; Transitional names retained for current compiler/runtime callers while the
;; remaining Choreo namespaces move to the explicit ExecutablePlan vocabulary.
;; They are aliases only; emitted data uses executable-plan-type.
(def projected-plan-version
  executable-plan-version)

(def projected-plan-type
  executable-plan-type)

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
;; Executable-plan predicates
;; -----------------------------------------------------------------------------

(def ^:private executable-plan-keys
  #{:gesso.choreo/type
    :gesso.choreo/version
    :role
    :initial
    :states})

(def ^:private projected-state-keys
  {:local
   #{:op :action :next :requires :outputs}

   :authoritative
   #{:op :operation :next :requires :outputs}

   :branch
   #{:op :on :cases}

   :send
   #{:op :to :event :next :via :required :optional :correlation :open-payload?}

   :receive
   #{:op :alternatives}

   :await
   #{:op :events :event-contracts}

   :return
   #{:op :outcome}})

(def ^:private receive-alternative-keys
  #{:from :event :next :via :required :optional :correlation :open-payload?})

(def ^:private environment-contract-keys
  #{:required :optional :open-data? :authoritative-observation})

(def ^:private authoritative-observation-keys
  #{:authority :observation :basis-key})

(defn- nat-int-locator?
  [value]
  (and (integer? value)
       (not (neg? value))))

(defn- fact-key-set?
  [value]
  (and (set? value)
       (every? #(type/valid? ::type/fact-key %)
               value)))

(defn- closed-map?
  [value allowed-keys]
  (and (map? value)
       (every? allowed-keys
               (keys value))))

(defn- present-nonempty-fact-key-set?
  [value key]
  (or (not (contains? value key))
      (let [items (get value key)]
        (and (fact-key-set? items)
             (seq items)))))

(defn- canonical-message-contract?
  [contract]
  (let [required
        (or (:required contract) #{})

        optional
        (or (:optional contract) #{})

        correlation
        (or (:correlation contract) #{})]
    (and
     (present-nonempty-fact-key-set? contract :required)
     (present-nonempty-fact-key-set? contract :optional)
     (present-nonempty-fact-key-set? contract :correlation)
     (empty? (set/intersection required optional))
     (set/subset? correlation required)
     (or (not (contains? contract :open-payload?))
         (true? (:open-payload? contract))))))

(defn- canonical-authoritative-observation?
  [contract observation]
  (and
   (map? observation)
   (= authoritative-observation-keys
      (set (keys observation)))
   (type/valid? ::type/authority-name
                (:authority observation))
   (type/valid? ::type/fact-key
                (:observation observation))
   (type/valid? ::type/fact-key
                (:basis-key observation))
   (contains? (or (:required contract) #{})
              (:basis-key observation))))

(defn- canonical-environment-contract?
  [contract]
  (and
   (closed-map? contract environment-contract-keys)
   (present-nonempty-fact-key-set? contract :required)
   (present-nonempty-fact-key-set? contract :optional)
   (empty?
    (set/intersection
     (or (:required contract) #{})
     (or (:optional contract) #{})))
   (or (not (contains? contract :open-data?))
       (true? (:open-data? contract)))
   (or (not (contains? contract :authoritative-observation))
       (canonical-authoritative-observation?
        contract
        (:authoritative-observation contract)))))

(defn- successor-locators
  [state]
  (case (:op state)
    :local
    [(:next state)]

    :authoritative
    [(:next state)]

    :send
    [(:next state)]

    :branch
    (vals (:cases state))

    :receive
    (map :next (:alternatives state))

    :await
    (vals (:events state))

    :return
    []

    []))

(defn- canonical-receive-alternative?
  [plan alternative]
  (and
   (closed-map? alternative receive-alternative-keys)
   (type/valid? ::type/role
                (:from alternative))
   (not= (:role plan) (:from alternative))
   (keyword? (:event alternative))
   (or (not (contains? alternative :via))
       (keyword? (:via alternative)))
   (canonical-message-contract? alternative)
   (nat-int-locator? (:next alternative))))

(defn- canonical-projected-state?
  [plan state]
  (and
   (map? state)
   (contains? projected-ops (:op state))
   (closed-map? state
                (get projected-state-keys
                     (:op state)
                     #{}))
   (case (:op state)
     :local
     (and
      (keyword? (:action state))
      (present-nonempty-fact-key-set? state :requires)
      (present-nonempty-fact-key-set? state :outputs)
      (nat-int-locator? (:next state)))

     :authoritative
     (and
      (keyword? (:operation state))
      (present-nonempty-fact-key-set? state :requires)
      (present-nonempty-fact-key-set? state :outputs)
      (nat-int-locator? (:next state)))

     :branch
     (and
      (type/valid? ::type/fact-key
                   (:on state))
      (map? (:cases state))
      (seq (:cases state))
      (every? (fn [[value target]]
                (and (some? value)
                     (nat-int-locator? target)))
              (:cases state)))

     :send
     (and
      (type/valid? ::type/role
                   (:to state))
      (not= (:role plan) (:to state))
      (keyword? (:event state))
      (or (not (contains? state :via))
          (keyword? (:via state)))
      (canonical-message-contract? state)
      (nat-int-locator? (:next state)))

     :receive
     (and
      (vector? (:alternatives state))
      (seq (:alternatives state))
      (every? #(canonical-receive-alternative? plan %)
              (:alternatives state)))

     :await
     (let [events (:events state)
           contracts (or (:event-contracts state) {})]
       (and
        (map? events)
        (seq events)
        (every? (fn [[event target]]
                  (and (keyword? event)
                       (nat-int-locator? target)))
                events)
        (or (not (contains? state :event-contracts))
            (and
             (map? contracts)
             (seq contracts)
             (set/subset? (set (keys contracts))
                          (set (keys events)))
             (every? (fn [[event contract]]
                       (and (keyword? event)
                            (canonical-environment-contract? contract)))
                     contracts)))))

     :return
     (type/valid? ::type/outcome
                   (:outcome state))

     false)))

(defn- canonical-runtime-layout?
  [plan]
  (let [states (:states plan)
        locator-set (set (keys states))
        expected-locators (set (range (count states)))]
    (and
     (seq states)
     (= expected-locators locator-set)
     (nat-int-locator? (:initial plan))
     (contains? locator-set (:initial plan))
     (every? (fn [[locator state]]
               (and
                (nat-int-locator? locator)
                (canonical-projected-state? plan state)
                (every? locator-set
                        (successor-locators state))))
             states)
     ;; Canonical projection contains no unreachable runtime states.  This is
     ;; stronger than merely requiring compact locator numbers: a convincing
     ;; tagged lookalike must not smuggle inert executable states into a plan.
     (= locator-set
        (loop [pending [(:initial plan)]
               index 0
               seen #{}]
          (if (= index (count pending))
            seen
            (let [locator (nth pending index)]
              (if (contains? seen locator)
                (recur pending (inc index) seen)
                (recur (into pending
                             (successor-locators
                              (get states locator)))
                       (inc index)
                       (conj seen locator))))))))))

(defn executable-plan?
  "True exactly when x is a canonical current ExecutablePlan.

   This is the public executable-format boundary, not a shallow tag predicate.
   It validates the closed top-level/state shapes emitted by projection, compact
   non-negative runtime locators, local contract shapes, successor integrity,
   and reachability.  Runtime consumers may therefore depend on this predicate
   instead of maintaining a weaker competing definition of plan validity."
  [x]
  (and
   (closed-map? x executable-plan-keys)
   (= executable-plan-type
      (:gesso.choreo/type x))
   (= executable-plan-version
      (:gesso.choreo/version x))
   (type/valid? ::type/role
                (:role x))
   (map? (:states x))
   (canonical-runtime-layout? x)))

(defn ensure-executable-plan
  "Return x when it is a canonical current ExecutablePlan; otherwise throw a
   deterministic projection error."
  [x]
  (when-not (executable-plan? x)
    (projection-error
     :invalid-executable-plan
     "Expected a canonical Gesso Choreo ExecutablePlan."
     {:value x}))
  x)

(defn projected-plan?
  "Transitional alias predicate for executable-plan?."
  [x]
  (executable-plan? x))

(defn ensure-projected-plan
  "Transitional alias for ensure-executable-plan."
  [x]
  (ensure-executable-plan x))

;; -----------------------------------------------------------------------------
;; Compiler-only projection provenance
;; -----------------------------------------------------------------------------

(def ^:private compiler-projection-keys
  #{:gesso.choreo/type
    :gesso.choreo/version
    :executable-plan
    :semantic-locations
    :semantic-continuations
    :runtime-origins})

(def ^:private semantic-location-endpoints
  #{:owner :sender :receiver})

(def ^:private runtime-origin-kinds
  #{:authored-boundary
    :synthetic-receive
    :synthetic-completion})

(defn- authored-origin-endpoint
  [runtime-state]
  (case (:op runtime-state)
    :local :owner
    :authoritative :owner
    :branch :owner
    :await :owner
    :send :sender
    nil))

(defn- runtime-origin?
  [compiled runtime-locator origin]
  (let [plan (:executable-plan compiled)
        runtime-state (get-in plan [:states runtime-locator])
        semantic-locations' (:semantic-locations compiled)
        semantic-continuations' (:semantic-continuations compiled)]
    (and
     (map? origin)
     (contains? runtime-origin-kinds (:kind origin))
     (case (:kind origin)
       :authored-boundary
       (let [semantic-state (:semantic-state origin)
             endpoint (authored-origin-endpoint runtime-state)]
         (and
          (= #{:kind :semantic-state} (set (keys origin)))
          (some? semantic-state)
          (some? endpoint)
          (some #(= {:runtime-locator runtime-locator :endpoint endpoint} %)
                (get semantic-locations' semantic-state []))))

       :synthetic-receive
       (let [source (:source-semantic-state origin)
             alternative-sources (:alternative-semantic-states origin)
             alternatives (:alternatives runtime-state)]
         (and
          (= #{:kind :source-semantic-state :alternative-semantic-states}
             (set (keys origin)))
          (some? source)
          (= :receive (:op runtime-state))
          (= runtime-locator (get semantic-continuations' source))
          (vector? alternative-sources)
          (= (count alternatives) (count alternative-sources))
          (every? vector? alternative-sources)
          (every? seq alternative-sources)
          (every?
           true?
           (map-indexed
            (fn [alternative-index semantic-states]
              (and
               (= (count semantic-states) (count (distinct semantic-states)))
               (every?
                (fn [semantic-state]
                  (some #(= {:runtime-locator runtime-locator
                             :endpoint :receiver
                             :alternative-index alternative-index} %)
                        (get semantic-locations' semantic-state [])))
                semantic-states)))
            alternative-sources))))

       :synthetic-completion
       (let [source (:source-semantic-state origin)]
         (and
          (= #{:kind :source-semantic-state} (set (keys origin)))
          (some? source)
          (= :return (:op runtime-state))
          (= complete-outcome (:outcome runtime-state))
          (= runtime-locator (get semantic-continuations' source))))

       false))))

(defn- compiler-location?
  [plan location]
  (and
   (map? location)
   (contains? semantic-location-endpoints
              (:endpoint location))
   (nat-int-locator? (:runtime-locator location))
   (contains? (:states plan)
              (:runtime-locator location))
   (case (:endpoint location)
     :owner
     (and
      (= #{:runtime-locator :endpoint}
         (set (keys location)))
      (contains? #{:local :authoritative :branch :await}
                 (get-in plan
                         [:states
                          (:runtime-locator location)
                          :op])))

     :sender
     (and
      (= #{:runtime-locator :endpoint}
         (set (keys location)))
      (= :send
         (get-in plan
                 [:states
                  (:runtime-locator location)
                  :op])))

     :receiver
     (let [alternative-index
           (:alternative-index location)

           alternatives
           (get-in plan
                   [:states
                    (:runtime-locator location)
                    :alternatives])]
       (and
        (= #{:runtime-locator :endpoint :alternative-index}
           (set (keys location)))
        (= :receive
           (get-in plan
                   [:states
                    (:runtime-locator location)
                    :op]))
        (nat-int? alternative-index)
        (< alternative-index
           (count alternatives))))

     false)))

(defn compiler-projection?
  "True when value is one current compiler-only projection product.

   A CompilerProjection contains the canonical ExecutablePlan plus exact
   semantic-state -> runtime-boundary, semantic-entry -> runtime-continuation,
   and total runtime-locator -> semantic/synthetic-origin provenance captured
   while projection is constructed. The provenance is
   compiler/diagnostic data and is deliberately not part of ExecutablePlan or
   runtime semantics."
  [value]
  (and
   (map? value)
   (= compiler-projection-keys
      (set (keys value)))
   (= compiler-projection-type
      (:gesso.choreo/type value))
   (= compiler-projection-version
      (:gesso.choreo/version value))
   (executable-plan? (:executable-plan value))
   (map? (:semantic-locations value))
   (every?
    (fn [[semantic-state locations]]
      (and
       (some? semantic-state)
       (vector? locations)
       (seq locations)
       (= (count locations)
          (count (distinct locations)))
       (every? #(compiler-location?
                 (:executable-plan value)
                 %)
               locations)))
    (:semantic-locations value))
   (map? (:semantic-continuations value))
   (every?
    (fn [[semantic-state runtime-locator]]
      (and
       (some? semantic-state)
       (nat-int-locator? runtime-locator)
       (contains?
        (get-in value [:executable-plan :states])
        runtime-locator)))
    (:semantic-continuations value))
   (map? (:runtime-origins value))
   (= (set (keys (get-in value [:executable-plan :states])))
      (set (keys (:runtime-origins value))))
   (every?
    (fn [[runtime-locator origin]]
      (runtime-origin? value runtime-locator origin))
    (:runtime-origins value))))

(defn ensure-compiler-projection
  "Return value when it is a valid current CompilerProjection; otherwise throw.

   This is a compiler/debug boundary only. Runtime code should continue to
   accept ExecutablePlan and must not require this provenance product."
  [value]
  (when-not (compiler-projection? value)
    (projection-error
     :invalid-compiler-projection
     "Expected a valid Gesso Choreo CompilerProjection."
     {:value value}))
  value)

(defn semantic-locations
  "Return the exact compiler-recorded runtime boundary locations for semantic-state.

   The result may contain more than one location when distinct projected
   receive alternatives intentionally collapse to the same runtime alternative.
   Returns an empty vector when the semantic state has no runtime boundary in
   this role projection."
  [compiled semantic-state]
  (get (:semantic-locations
        (ensure-compiler-projection compiled))
       semantic-state
       []))

(defn semantic-continuation
  "Return the exact runtime locator reached by this role when execution enters
   semantic-state, or nil when the compiler never needed that semantic entry as
   a projected continuation.

   Unlike semantic-locations, this records continuation identity even when the
   role immediately skips foreign/unobservable semantic work before reaching its
   next local boundary. It is compiler/proof provenance only and is deliberately
   absent from ExecutablePlan."
  [compiled semantic-state]
  (get (:semantic-continuations
        (ensure-compiler-projection compiled))
       semantic-state))

(defn runtime-origins
  "Return the total compiler-recorded runtime-locator -> origin map.

   Every emitted runtime state has exactly one origin classification:

     :authored-boundary
       a role-owned/sender boundary copied from one exact semantic state;

     :synthetic-receive
       a compiler-generated receive gate, with exact semantic communication
       states recorded per emitted alternative; or

     :synthetic-completion
       a compiler-generated role-local completion state, recording the semantic
       entry whose continuation reached completion.

   This inverse provenance is compiler/proof data only. Runtime execution must
   continue to depend solely on ExecutablePlan."
  [compiled]
  (:runtime-origins
   (ensure-compiler-projection compiled)))

(defn runtime-origin
  "Return the exact compiler-recorded origin of runtime-locator, or nil when the
   locator is not part of this CompilerProjection."
  [compiled runtime-locator]
  (get (runtime-origins compiled)
       runtime-locator))

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

(defn- authoritative-observation-event?
  [state event]
  (some?
   (get-in state
           [:event-contracts
            event
            :authoritative-observation])))

(defn- authoritative-observation-events
  [state]
  (->> (:events state)
       keys
       (filter #(authoritative-observation-event?
                 state
                 %))
       set))

(defn- pure-authoritative-observation-await?
  [state role]
  (and (= :await
          (:op state))
       (= role
          (:role state))
       (= (set (keys (:events state)))
          (authoritative-observation-events state))))

(defn- mixed-authoritative-observation-await?
  [state role]
  (when (and (= :await
                (:op state))
             (= role
                (:role state)))
    (let [all-events
          (set (keys (:events state)))

          observation-events
          (authoritative-observation-events state)]
      (and (seq observation-events)
           (not= all-events
                 observation-events)))))

(defn- observable-frontier
  "Return the first global states that are locally relevant to role.

   Traversal carries a causal-barrier marker after a foreign :authoritative
   state. While that barrier is present, ordinary direct work by role is not
   projectable: the role has no observation establishing that the authoritative
   predecessor occurred. The barrier may be discharged in either of two explicit
   ways:

   - an incoming participant communication, because receiving it is an observable
     causal successor;
   - a role-local :await whose every alternative is declared as an
     :authoritative-observation, because completing that trusted reread is itself
     the observation frontier.

   An ordinary environment event never clears the barrier merely because it
   arrives later. Mixed waits containing both authoritative-observation and
   ordinary events are rejected while a barrier is outstanding: the current
   compact ExecutablePlan has no hidden per-alternative causal-barrier state, so
   accepting such a wait would let an ordinary event erase authority ordering.

   Frontier entries:

     {:kind :state :state id}
       Direct work role may perform now.

     {:kind :receive :state id}
       Incoming participant communication role may receive now.

     {:kind :blocked :state id :authority-state id}
       Direct role work is causally after a foreign authoritative operation but
       no communication or authoritative observation has yet made that
       predecessor observable to role.

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
                (and authority-state
                     (mixed-authoritative-observation-await?
                      state
                      role))
                (projection-error
                 :mixed-authoritative-observation-await
                 "A wait reached behind a foreign-authority barrier mixes authoritative-observation and ordinary environment events. The compact projection cannot let the ordinary alternatives erase the barrier."
                 {:role role
                  :state state-id
                  :authority-state authority-state
                  :events (set (keys (:events state)))
                  :authoritative-observation-events
                  (authoritative-observation-events state)})

                (and authority-state
                     (pure-authoritative-observation-await?
                      state
                      role))
                ;; Waiting is permitted while the foreign-authority barrier is
                ;; outstanding because every way out of this state is an
                ;; explicitly declared authoritative reread. ensure-local-state!
                ;; preserves the event contracts in ExecutablePlan; completing
                ;; one of those events is the synchronization point, so its
                ;; successor is compiled without carrying the old barrier.
                (recur pending
                       (inc index)
                       seen'
                       (conj frontier
                             {:kind :state
                              :state state-id}))

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
       "Role-local behavior is causally after a foreign authoritative operation, but no incoming communication or authoritative observation establishes that the authoritative predecessor occurred."
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
;; Canonical executable layout
;; -----------------------------------------------------------------------------

(defn- ordered-successors
  "Return projected successor identities in semantic, source-id-independent order."
  [state]
  (case (:op state)
    :local
    [(:next state)]

    :authoritative
    [(:next state)]

    :send
    [(:next state)]

    :branch
    (->> (:cases state)
         (sort-by (comp pr-str key))
         (mapv val))

    :receive
    (->> (:alternatives state)
         (sort-by
          (fn [alternative]
            (pr-str
             (dissoc alternative :next))))
         (mapv :next))

    :await
    (->> (:events state)
         (sort-by (comp pr-str key))
         (mapv val))

    :return
    []

    (projection-error
     :unsupported-projected-op
     "Cannot canonicalize an unsupported projected operation."
     {:state state
      :op (:op state)})))

(defn- reachable-state-order
  [initial states]
  (loop [pending [initial]
         index 0
         seen #{}
         order []]
    (if (= index (count pending))
      order
      (let [state-id (nth pending index)]
        (if (contains? seen state-id)
          (recur pending
                 (inc index)
                 seen
                 order)
          (let [state (get states state-id)]
            (when-not state
              (projection-error
               :unknown-projected-successor
               "Projected state points to an unknown successor."
               {:state state-id}))
            (recur (into pending
                         (ordered-successors state))
                   (inc index)
                   (conj seen state-id)
                   (conj order state-id))))))))

(defn- rewrite-successors
  [state locator-by-state]
  (let [locator!
        (fn [state-id]
          (if-some [locator
                    (get locator-by-state state-id)]
            locator
            (projection-error
             :unknown-projected-successor
             "Projected state points outside the canonical executable layout."
             {:state state-id})))]
    (case (:op state)
      :local
      (update state :next locator!)

      :authoritative
      (update state :next locator!)

      :send
      (update state :next locator!)

      :branch
      (update state :cases
              (fn [cases]
                (into {}
                      (map (fn [[value target]]
                             [value (locator! target)]))
                      cases)))

      :receive
      (update state :alternatives
              (fn [alternatives]
                (mapv #(update % :next locator!)
                      alternatives)))

      :await
      (update state :events
              (fn [events]
                (into {}
                      (map (fn [[event target]]
                             [event (locator! target)]))
                      events)))

      :return
      state

      (projection-error
       :unsupported-projected-op
       "Cannot rewrite successors for an unsupported projected operation."
       {:state state
        :op (:op state)}))))

(defn- build-executable-plan
  [choreography role initial states]
  (let [state-order
        (reachable-state-order initial states)

        reachable
        (set state-order)

        all-state-ids
        (set (keys states))]

    (when-not (= reachable all-state-ids)
      (projection-error
       :unreachable-projected-state
       "Projection produced runtime states unreachable from its initial state."
       {:role role
        :unreachable
        (set (remove reachable all-state-ids))}))

    (let [locator-by-state
          (zipmap state-order
                  (range))

          runtime-states
          (into (sorted-map)
                (map-indexed
                 (fn [locator state-id]
                   [locator
                    (rewrite-successors
                     (get states state-id)
                     locator-by-state)]))
                state-order)

          plan
          (ensure-executable-plan
           {:gesso.choreo/type
            executable-plan-type

            :gesso.choreo/version
            executable-plan-version

            :role
            role

            :initial
            (get locator-by-state initial)

            :states
            runtime-states})]

      {:executable-plan plan
       :locator-by-state locator-by-state})))

(defn- direct-semantic-locations
  [global-states projected-states locator-by-state]
  (reduce-kv
   (fn [locations semantic-state _global-state]
     (if-some [runtime-locator
               (get locator-by-state semantic-state)]
       (let [projected-op
             (:op (get projected-states semantic-state))

             endpoint
             (case projected-op
               :local :owner
               :authoritative :owner
               :branch :owner
               :await :owner
               :send :sender
               nil)]
         (if endpoint
           (update locations
                   semantic-state
                   (fnil conj [])
                   {:runtime-locator runtime-locator
                    :endpoint endpoint})
           locations))
       locations))
   {}
   global-states))

(defn- receive-semantic-locations
  [locations locator-by-state receive-provenance]
  (reduce-kv
   (fn [locations projected-state-id source-sets]
     (let [runtime-locator
           (get locator-by-state projected-state-id)]
       (reduce-kv
        (fn [locations alternative-index semantic-states]
          (reduce
           (fn [locations semantic-state]
             (update locations
                     semantic-state
                     (fnil conj [])
                     {:runtime-locator runtime-locator
                      :endpoint :receiver
                      :alternative-index alternative-index}))
           locations
           semantic-states))
        locations
        (vec source-sets))))
   locations
   receive-provenance))

(defn- canonical-semantic-locations
  [global-states projected-states locator-by-state receive-provenance]
  (let [direct
        (direct-semantic-locations
         global-states
         projected-states
         locator-by-state)

        all-locations
        (receive-semantic-locations
         direct
         locator-by-state
         receive-provenance)]
    (into {}
          (map
           (fn [[semantic-state locations]]
             [semantic-state
              (->> locations
                   distinct
                   (sort-by
                    (juxt :runtime-locator
                          (comp pr-str :endpoint)
                          #(or (:alternative-index %) -1)))
                   vec)]))
          all-locations)))

(defn- canonical-semantic-continuations
  [continuation-cache locator-by-state]
  (into {}
        (map
         (fn [[semantic-state projected-state-id]]
           (let [runtime-locator
                 (get locator-by-state projected-state-id)]
             (when-not (nat-int-locator? runtime-locator)
               (projection-error
                :missing-continuation-provenance
                "Compiler continuation provenance did not resolve to an emitted runtime locator."
                {:semantic-state semantic-state
                 :projected-state projected-state-id
                 :runtime-locator runtime-locator}))
             [semantic-state runtime-locator])))
        (sort-by
         (comp pr-str key)
         continuation-cache)))

(defn- synthetic-state-id-parts
  [state-id role]
  (when (and
         (vector? state-id)
         (= 4 (count state-id))
         (= :gesso.choreo.project/synthetic (nth state-id 0))
         (= role (nth state-id 2)))
    {:kind (nth state-id 1)
     :source (nth state-id 3)}))

(defn- canonical-runtime-origins
  [role global-states projected-states locator-by-state receive-provenance]
  (into
   (sorted-map)
   (map
    (fn [[projected-state-id runtime-locator]]
      (let [runtime-state (get projected-states projected-state-id)
            synthetic (synthetic-state-id-parts projected-state-id role)
            origin
            (if synthetic
              (case (:kind synthetic)
                :receive
                {:kind :synthetic-receive
                 :source-semantic-state (:source synthetic)
                 :alternative-semantic-states
                 (->> (get receive-provenance projected-state-id)
                      (mapv (fn [semantic-states]
                              (->> semantic-states (sort-by pr-str) vec))))}

                :complete
                {:kind :synthetic-completion
                 :source-semantic-state (:source synthetic)}

                (projection-error
                 :unknown-synthetic-runtime-origin
                 "Projection emitted a synthetic runtime state with an unsupported origin kind."
                 {:role role
                  :projected-state projected-state-id
                  :runtime-locator runtime-locator
                  :synthetic-origin synthetic}))

              (do
                (when-not (contains? global-states projected-state-id)
                  (projection-error
                   :unknown-runtime-origin
                   "Projection emitted a runtime state with neither an authored semantic origin nor a recognized synthetic origin."
                   {:role role
                    :projected-state projected-state-id
                    :runtime-locator runtime-locator
                    :runtime-state runtime-state}))
                {:kind :authored-boundary
                 :semantic-state projected-state-id}))]
        [runtime-locator origin]))
    (sort-by (comp pr-str key) locator-by-state))))

;; -----------------------------------------------------------------------------
;; Projection builder
;; -----------------------------------------------------------------------------

(defn- synthetic-id
  [kind role source]
  [:gesso.choreo.project/synthetic
   kind
   role
   source])

(defn compile-role
  "Compile one choreography to one role-local ExecutablePlan plus exact compiler provenance.

   choreography-or-verified may be:

   - a plain/normalized choreography;
   - a successful verification result;
   - a successful verified wrapper.

   Projection currently accepts only roles inferred from the choreography.

   The returned CompilerProjection contains the canonical portable
   ExecutablePlan plus compiler-only semantic-state -> runtime-location
   provenance. Runtime code must consume only :executable-plan. The provenance
   exists so proof/diagnostic sidecars do not have to reconstruct semantic
   identity by heuristically matching executable boundary shapes."
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

        receive-provenance
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

                 (let [raw-alternatives
                       (mapv
                        (fn [{global-state-id
                              :state}]
                          (let [global-state
                                (get global-states
                                     global-state-id)]
                            {:semantic-state global-state-id
                             :alternative
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
                                true))}))
                        entries)

                       alternatives
                       (normalize-receive-alternatives
                        role
                        source
                        (mapv :alternative
                              raw-alternatives))

                       source-sets
                       (mapv
                        (fn [alternative]
                          (->> raw-alternatives
                               (keep
                                (fn [{:keys [semantic-state]
                                      candidate :alternative}]
                                  (when (= alternative candidate)
                                    semantic-state)))
                               (sort-by pr-str)
                               vec))
                        alternatives)]

                   (swap! receive-provenance
                          assoc
                          state-id
                          source-sets)

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
                    (cond->
                     {:op :await
                      :events
                      (into {}
                            (map
                             (fn [[event target]]
                               [event
                                (ensure-continuation!
                                 target)]))
                            (:events state))}

                      (seq (:event-contracts state))
                      (assoc
                       :event-contracts
                       (:event-contracts state)))))

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

        (let [{:keys [executable-plan
                      locator-by-state]}
              (build-executable-plan
               choreography
               role
               initial
               states)

              semantic-locations'
              (canonical-semantic-locations
               global-states
               states
               locator-by-state
               @receive-provenance)

              semantic-continuations'
              (canonical-semantic-continuations
               @continuation-cache
               locator-by-state)

              runtime-origins'
              (canonical-runtime-origins
               role
               global-states
               states
               locator-by-state
               @receive-provenance)]

          (ensure-compiler-projection
           {:gesso.choreo/type
            compiler-projection-type

            :gesso.choreo/version
            compiler-projection-version

            :executable-plan
            executable-plan

            :semantic-locations
            semantic-locations'

            :semantic-continuations
            semantic-continuations'

            :runtime-origins
            runtime-origins'}))))))

(defn project
  "Project one choreography to the canonical role-local ExecutablePlan.

   This is the runtime-facing API. Exact semantic provenance is available from
   compile-role and is intentionally excluded from the returned plan."
  [choreography-or-verified role]
  (:executable-plan
   (compile-role choreography-or-verified role)))

(defn compile-all
  "Compile every inferred role, returning role -> CompilerProjection."
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
              (compile-role verified role)]))
          (sort-by pr-str
                   (choreo/roles choreography)))))

(defn project-all
  "Project choreography once for every inferred role.

   Returns role -> canonical ExecutablePlan. Compiler-only provenance is
   available from compile-all."
  [choreography-or-verified]
  (into {}
        (map
         (fn [[role compiled]]
           [role (:executable-plan compiled)]))
        (compile-all choreography-or-verified)))

;; -----------------------------------------------------------------------------
;; Inspection
;; -----------------------------------------------------------------------------

(defn state
  "Return one executable state by runtime locator."
  [plan locator]
  (get (:states
        (ensure-executable-plan plan))
       locator))

(defn explain-compiler-projection
  "Return a compact summary of one compiler-only projection product."
  [compiled]
  (let [compiled'
        (ensure-compiler-projection compiled)

        plan
        (:executable-plan compiled')]
    {:role (:role plan)
     :compiler-projection-version
     (:gesso.choreo/version compiled')
     :executable-plan-version
     (:gesso.choreo/version plan)
     :semantic-state-count
     (count (:semantic-locations compiled'))
     :location-count
     (reduce +
             0
             (map count
                  (vals (:semantic-locations compiled'))))
     :semantic-continuation-count
     (count (:semantic-continuations compiled'))
     :runtime-origin-count
     (count (:runtime-origins compiled'))}))

(defn explain
  "Return a compact stable ExecutablePlan summary."
  [plan]
  (let [plan'
        (ensure-executable-plan plan)

        states
        (:states plan')]

    {:role
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
