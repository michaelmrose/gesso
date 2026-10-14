(ns gesso.model.invariant-check
  "Bounded checking of independent application invariants over Gesso's
   existing, normalized authoritative transaction plans.

   Developers supply ordinary Clojure predicates describing what must hold
   for an entire *finite modeled state*. Gesso explores every serial order of
   one to six already prepared plans using gesso.model.tx-model, including
   rejected transactions, and checks each rule initially and after every
   attempted commit. The result contains the earliest counterexample for
   each violated rule, with the precise plan index and execution prefix.

   The rule predicates are HOST FUNCTIONS, not proved or translated into
   formal logic. They MUST be pure and total for the modeled states; Gesso
   cannot verify their meaning, dependencies or purity. The state universe
   and prepared plans are supplied by the caller: Gesso cannot infer absent
   dependencies, future commands, other writers, query phantoms, or the
   completeness of a domain policy from them. This is useful finite
   counterexample discovery, NOT an application certificate, generated
   transaction-guard admission, or an unbounded inductive proof.

   Example:

     (check-prepared
       {[:requests request-id] request
        [:memberships membership-id] membership}
       [prepared-claim-plan prepared-revocation-plan]
       [{:id :request/owner-has-active-membership
         :holds? (fn [state] ...)}])

   The rule is deliberately independent of the plans under examination.
   This namespace does not modify the production transaction or resolved-
   operation paths and never executes a transaction or domain callback."
  (:require [gesso.model.tx-model :as reference]))

(def assurance-scope
  {:checker :gesso.model.invariant-check/finite-prepared-serial-v1
   :checked #{:initial-state-rule-evaluation
              :rule-preservation-at-every-explored-step
              :all-serial-orderings-of-admitted-prepared-plans
              :named-first-counterexamples}
   :assumed #{:pure-total-truthful-rule-predicates
              :complete-finite-state-universe
              :representative-already-prepared-plans
              :serial-atomic-commit-semantics}
   :not-established #{:truth-of-authored-business-rules
                      :arbitrary-clojure-predicate-verification
                      :source-derived-completeness-of-policy-dependencies
                      :unmodeled-commands-or-state
                      :real-xtdb-execution-correspondence
                      :unbounded-invariant-preservation
                      :concurrent-execution-linearizability
                      :authorization-of-actual-principals
                      :application-certification}})

(defn- fail! [kind message details]
  (throw (ex-info message
                  {:error/type ::invalid-invariant-check
                   :error/kind kind
                   :error/details details})))

(defn- require-rules [rules]
  (when-not (and (vector? rules) (seq rules))
    (fail! :invalid-rules
           "Invariant checking requires a nonempty vector of named rules."
           {:rules rules}))
  (doseq [[index rule] (map-indexed vector rules)]
    (when-not (and (map? rule)
                   (= #{:id :holds?} (set (keys rule)))
                   (qualified-keyword? (:id rule))
                   (fn? (:holds? rule)))
      (fail! :invalid-rule
             "Each rule must have exactly a qualified :id and a function :holds?."
             {:index index :rule rule})))
  (when-not (= (count rules) (count (set (map :id rules))))
    (fail! :duplicate-rule
           "Rule identifiers must be unique within an invariant check."
           {:ids (mapv :id rules)}))
  rules)

(defn- evaluate-rule! [{:keys [id holds?]} state]
  (let [answer (try
                 (holds? state)
                 (catch Exception exception
                   (throw (ex-info
                           "A domain invariant predicate failed; no check result is established."
                           {:error/type ::rule-evaluation-failed
                            :rule-id id}
                           exception))))]
    (when-not (or (true? answer) (false? answer))
      (fail! :non-boolean-rule-result
             "A domain invariant must return exactly true or false."
             {:rule-id id :result answer}))
    answer))

(defn- first-violations [rules initial-state paths]
  ;; Preserve rule order even if failures occur at different steps. All
  ;; visited states are checked; keeping one witness per rule bounds the
  ;; diagnostic output without concealing the number of explored paths.
  (let [initial (into {}
                      (map (fn [rule]
                             [(:id rule) (evaluate-rule! rule initial-state)]))
                      rules)
        initial-failures
        (into {}
              (keep (fn [rule]
                      (when (false? (get initial (:id rule)))
                        [(:id rule)
                         {:rule-id (:id rule)
                          :at :initial-state
                          :order nil
                          :plan-index nil
                          :trace []
                          :state initial-state}]))
                    rules))]
    (reduce
     (fn [found {:keys [order trace]}]
       (reduce
        (fn [found [position step]]
          (reduce
           (fn [found rule]
             (let [ok? (evaluate-rule! rule (:state step))
                   id (:id rule)]
               (if (or ok? (contains? found id))
                 found
                 (assoc found id
                        {:rule-id id
                         :at :after-plan
                         :order order
                         :plan-index (:plan-index step)
                         :status (:status step)
                         :trace (subvec trace 0 (inc position))
                         :state (:state step)}))))
           found rules))
        found (map-indexed vector trace)))
     initial-failures paths)))

(defn check-prepared
  "Exhaustively check named invariants against up to six PREPARED transaction
   plans, under the strict abstract serial-commit reference semantics.

   Returns {:status :holds-in-explored-model|:counterexample
            :violations [...] :exploration {...} :evaluated-state-visits n
            :scope ...}.

   All plans must fit the existing tx-model admitted subset. Unsupported SQL
   assertions or malformed plans throw rather than being ignored. Rule
   failures and non-Boolean rule results throw rather than becoming an
   apparently successful or failed domain assertion. Even when the status is
   :holds-in-explored-model, no rule is certified for unmodeled states."
  [initial-state plans rules]
  (let [rules (require-rules rules)
        {:keys [paths exploration]}
        (reference/explore-orders initial-state plans)
        by-id (first-violations rules initial-state paths)
        violations (into [] (keep by-id) (map :id rules))]
    {:status (if (seq violations)
               :counterexample
               :holds-in-explored-model)
     :violations violations
     :rule-ids (mapv :id rules)
     :exploration exploration
     :evaluated-state-visits
     ;; Initial state evaluated once, then every attempted step in every
     ;; ordering, including rejected transactions (which preserve state).
     (+ 1 (reduce + (map (comp count :trace) paths)))
     :scope assurance-scope}))

;; Unlike check-prepared, the following bounded search re-invokes each enabled
;; operation's plan builder from the state actually reached by preceding steps.
;; It intentionally models serial, atomic attempts. It does not infer the set
;; of application operations, callers, parameters, or enabled states.
(def reachable-assurance-scope
  {:checker :gesso.model.invariant-check/finite-reachable-serial-v1
   :checked #{:initial-state-rule-evaluation
              :rule-preservation-after-every-attempted-operation
              :all-enabled-choices-through-declared-depth
              :state-dependent-plan-reconstruction
              :per-declared-operation-exercise-counts
              :redacted-bounded-transition-prefix-tree
              :private-exact-state-witnesses-for-reachable-occurrences
              :shortest-named-counterexamples}
   :assumed #{:pure-total-truthful-rule-predicates
              :pure-total-truthful-operation-builders
              :complete-modeled-operation-and-argument-instances
              :truthful-operation-enablement
              :complete-finite-state-universe
              :serial-atomic-commit-semantics}
   :not-established #{:replay-correspondence-to-production-executors
                      :concrete-state-identity-or-deduplication
                      :truth-of-authored-business-rules
                      :completeness-of-available-operations-or-arguments
                      :source-derived-completeness-of-policy-dependencies
                      :arbitrary-clojure-predicate-verification
                      :unbounded-invariant-preservation
                      :real-xtdb-execution-correspondence
                      :authenticity-of-caller-supplied-observed-states
                      :concurrent-execution-linearizability
                      :durable-application-command-replay
                      :authorization-of-actual-principals
                      :application-certification}})

(defn- require-operations [operations]
  (when-not (and (vector? operations)
                 (<= 1 (count operations) 6))
    (fail! :invalid-operations
           "Reachability checking requires one to six operation instances in a vector."
           {:operations operations :maximum 6}))
  (doseq [[index operation] (map-indexed vector operations)]
    (when-not (and (map? operation)
                   (= #{:id :enabled? :prepare} (set (keys operation)))
                   (qualified-keyword? (:id operation))
                   (fn? (:enabled? operation))
                   (fn? (:prepare operation)))
      (fail! :invalid-operation
             "Each operation requires a qualified :id, an :enabled? function, and a :prepare function."
             {:index index :operation operation})))
  (when-not (= (count operations) (count (set (map :id operations))))
    (fail! :duplicate-operation
           "Modeled operation instance identifiers must be unique."
           {:ids (mapv :id operations)}))
  operations)

(defn- require-reachable-options [options]
  (when-not (and (map? options)
                 (= #{:max-depth :max-transitions} (set (keys options)))
                 (integer? (:max-depth options))
                 (<= 1 (:max-depth options) 6)
                 (integer? (:max-transitions options))
                 (<= 1 (:max-transitions options) 10000))
    (fail! :invalid-reachability-bound
           "Specify :max-depth (1–6) and :max-transitions (1–10000); exceeding the latter aborts the check."
           {:options options}))
  options)

(defn- invoke-operation! [operation phase callback state]
  (try
    (callback state)
    (catch Exception cause
      (throw (ex-info
              "Modeled operation evaluation failed; no reachability verdict is established."
              {:error/type ::operation-evaluation-failed
               :operation-id (:id operation)
               :phase phase}
              cause)))))

(defn- admitted-operation-plan [operation state]
  (let [plan (invoke-operation! operation :prepare (:prepare operation) state)]
    (when-not (and (map? plan) (seq (:commands plan)))
      (fail! :invalid-operation-plan
             "An enabled operation must prepare a nonempty authoritative document-command plan."
             {:operation-id (:id operation) :plan plan}))
    ;; Preserve tx-model's strict rejection of uninterpreted assertions,
    ;; transaction hooks, and custom transaction options.
    (reference/admitted-plan plan)))

(defn- evaluate-operations-enabled! [operation state]
  (let [answer (invoke-operation! operation :enabled? (:enabled? operation) state)]
    (when-not (or (true? answer) (false? answer))
      (fail! :non-boolean-operation-enablement
             "Modeled operation enablement must return exactly true or false."
             {:operation-id (:id operation) :result answer}))
    answer))

(defn- initial-rule-violations [rules initial-state]
  (reduce (fn [found rule]
            (if (evaluate-rule! rule initial-state)
              found
              (assoc found (:id rule)
                     {:rule-id (:id rule)
                      :at :initial-state
                      :depth 0
                      :operation-id nil
                      :status nil
                      :trace []
                      :state initial-state})))
          {}
          rules))

(defn- next-rule-violations [found rules trace state]
  ;; Evaluate EVERY rule at EVERY reachable occurrence, including on an
  ;; assertion-rejected (unchanged) state. A previously found violation
  ;; remains the first/shortest one because exploration is breadth-first.
  (reduce (fn [found rule]
            (let [valid? (evaluate-rule! rule state)
                  id (:id rule)]
              (if (or valid? (contains? found id))
                found
                (assoc found id
                       {:rule-id id
                        :at :after-operation
                        :depth (count trace)
                        :operation-id (:operation-id (peek trace))
                        :status (:status (peek trace))
                        :trace trace
                        :state state}))))
          found rules))

(defn- zero-operation-exercise [operations]
  ;; Store only counts keyed by already validated operation identifiers.
  ;; No callback, transaction plan, or modeled document enters the summary.
  (into {}
        (map (fn [{:keys [id]}]
               [id {:enablement-checks 0
                    :disabled-checks 0
                    :committed-attempts 0
                    :rejected-attempts 0}]))
        operations))

(defn- operation-exercise-summary [operations counters]
  ;; Preserve declaration order, not map iteration order. An enabled attempt
  ;; includes BOTH committed and rejected serial-atomic reference outcomes.
  (mapv (fn [{:keys [id]}]
          (let [{:keys [enablement-checks disabled-checks
                        committed-attempts rejected-attempts]}
                (get counters id)]
            {:operation-id id
             :enablement-checks enablement-checks
             :disabled-checks disabled-checks
             :enabled-attempts (+ committed-attempts rejected-attempts)
             :committed-attempts committed-attempts
             :rejected-attempts rejected-attempts}))
        operations))

;; An occurrence's modeled documents must never be placed in the public
;; transition tree or its reports. Retain the *actual* state at each explored
;; occurrence, instead of reconstructing it by invoking application-supplied
;; plan builders a second time. A weak, identity-based object key means that
;; dropping the checker result also permits its private state snapshots to be
;; collected. The value never references its key.
(def ^:private occurrence-state-witnesses (java.util.WeakHashMap.))

(defn- retain-occurrence-states! [verdict states]
  (let [tree (:transition-tree verdict)
        exploration (:exploration verdict)]
    (when-not (= (:node-count tree) (count states)
                 (:visited-state-occurrences exploration))
      (fail! :incomplete-occurrence-state-witnesses
             "Every explored node must retain exactly one private modeled state."
             {:node-count (:node-count tree)
              :state-count (count states)}))
    (let [handle (Object.)]
      (locking occurrence-state-witnesses
        ;; The original core fields are bound to this exact witness. Higher
        ;; layers may *add* registry or relational reports, but cannot edit
        ;; a checker field and retain its comparison capability.
        (.put occurrence-state-witnesses handle
              {:states states :verdict verdict}))
      handle)))

(defn compare-reachable-occurrence-state!
  "Compare one modeled node's private state to an EXACT caller-supplied
   document map. The node must belong to an actual, completed reachable check;
   it is not regenerated from modeled callbacks or an invented graph node.

   Only an equality classification is returned, never modeled documents,
   hashes, transaction plans or predicates. A forged/detached result, stale
   process-local handle, malformed tree, or unknown node fails closed. The
   observed document map is NOT authenticated here: use a trusted executor-
   bound observation boundary before claiming any production correspondence.
   Even exact equality proves neither execution chronology nor that this
   observed state was reached by the displayed modeled prefix."
  [reachable-result node-id observed-state]
  (let [handle (:occurrence-state-witness reachable-result)
        retained (locking occurrence-state-witnesses
                   (.get occurrence-state-witnesses handle))
        {:keys [states verdict]} retained]
    (when-not (and retained
                   (instance? Object handle)
                   ;; The registry and relational layers add their own keys
                   ;; with assoc. Compare ALL original checker-owned fields
                   ;; while permitting these extra non-authorizing reports.
                   (= verdict (select-keys reachable-result (keys verdict)))
                   (integer? node-id)
                   (<= 0 node-id)
                   (< node-id (count states)))
      (fail! :invalid-occurrence-state-witness
             "A current completed check and one of its actual node IDs are required."
             {:node-id node-id}))
    (let [observed (reference/require-state observed-state)]
      {:status (if (= (nth states node-id) observed)
                 :matches-modeled-occurrence-state
                 :differs-from-modeled-occurrence-state)
       :model-node-id node-id
       :scope :one-finite-modeled-occurrence-versus-caller-supplied-document-map
       :relation :exact-clojure-domain-document-map-equality
       :state-contents-exposed? false
       :model-callbacks-reexecuted? false
       :observed-state-authenticity :not-established
       :model-prefix-observed-in-production :not-established
       :execution-chronology :not-established
       :production-trace-refinement :not-established})))

(defn check-reachable
  "Exhaustively explore *state-dependent* operation instances through a
   finite serial depth, checking named application invariants initially and
   after EVERY attempted transaction (committed or rejected).

   Each operation is ordinary Clojure data:
     {:id :request/claim-helper-a
      :enabled? (fn [current-state] ... boolean ...)
      :prepare (fn [current-state] ... gesso.model.tx plan ...)}

   An operation instance represents ONE particular choice of arguments and
   principal. The caller must enumerate every choice it wants analyzed; an
   omitted instance or falsely disabled operation is invisible to this model.
   Enabled operations MUST prepare admitted, nonempty document-command plans.
   Plans are rebuilt for each reached state, not reused from the root.

   Options must be {:max-depth 1..6 :max-transitions 1..10000}. The latter
   is a strict fail-closed budget for attempted transitions, not permission
   to return a partial successful verdict. A redacted, source-order
   :operation-exercise summary counts enablement checks, disabled choices,
   committed attempts, and rejected attempts for each modeled operation.
   This can reveal declared choices never attempted inside the finite search;
   it does NOT establish completeness of real operations or their domains.
   The :transition-tree is an exhaustive prefix tree of *attempted*
   transitions within the declared depth and budget: root node 0, with
   breadth-first occurrence IDs, parent IDs, synthetic operation IDs,
   outcomes, and depths. It contains NO modeled documents, transaction plans,
   callbacks, principals, or arguments. Identical concrete states are NOT
   merged; this is not an automatically minimized state-machine graph, a
   production trace, or independent implementation-correspondence evidence.
   The exploration is breadth-first
   and deterministic in declared operation order; it records one shortest
   counterexample per rule. Repeated states are NOT deduplicated, so no
   transitions are pruned merely because a state has appeared elsewhere.

   This function neither executes XTDB transactions nor proves its host
   predicates, builders, enablement decisions, or coverage of possible
   operations. A passing verdict refers ONLY to the fully explored finite
   search tree under the explicit assumptions in :scope."
  [initial-state operations rules options]
  (let [initial-state (reference/require-state initial-state)
        operations (require-operations operations)
        rules (require-rules rules)
        {:keys [max-depth max-transitions]} (require-reachable-options options)
        initial-found (initial-rule-violations rules initial-state)]
    (loop [depth 0
           frontier [{:state initial-state :trace [] :node-id 0}]
           transitions 0
           found initial-found
           exercise (zero-operation-exercise operations)
           edges []
           occurrence-states [initial-state]]
      (if (or (= depth max-depth) (empty? frontier))
        (let [status (if (seq found) :counterexample :holds-in-explored-model)
              tree {:kind :bounded-reachable-transition-prefix-tree
                    :root-node-id 0
                    :node-count (inc transitions)
                    :edges edges
                    :state-identity :occurrences-not-deduplicated
                    :production-correspondence :not-established}
              exploration {:kind :bounded-reachable-serial-operations
                           :operation-ids (mapv :id operations)
                           :max-depth max-depth
                           :max-transitions max-transitions
                           :reached-depth depth
                           :attempted-transitions transitions
                           :visited-state-occurrences (inc transitions)
                           :frontier-exhausted? (empty? frontier)}]
        (let [verdict {:status status
                       :violations (into [] (keep found) (map :id rules))
                       :rule-ids (mapv :id rules)
                       :operation-exercise
                       (operation-exercise-summary operations exercise)
                       :transition-tree tree
                       :exploration exploration
                       :scope reachable-assurance-scope}]
          ;; Opaque, process-local witness identity; NEVER a modeled state.
          (assoc verdict :occurrence-state-witness
                 (retain-occurrence-states! verdict occurrence-states))))
        (let [{next-frontier :frontier
               next-transitions :transitions
               next-found :found
               next-exercise :exercise
               next-edges :edges
               next-occurrence-states :occurrence-states}
              (reduce
               (fn [acc node]
                 (reduce
                  (fn [acc operation]
                    (let [id (:id operation)
                          enabled? (evaluate-operations-enabled!
                                    operation (:state node))
                          acc (update-in acc [:exercise id :enablement-checks] inc)]
                      (if-not enabled?
                        (update-in acc [:exercise id :disabled-checks] inc)
                        (do
                          (when (>= (:transitions acc) max-transitions)
                            (fail! :exploration-budget-exhausted
                                   "The reachability budget was exhausted; no verdict is established."
                                   {:max-depth max-depth
                                    :max-transitions max-transitions
                                    :attempted-transitions (:transitions acc)
                                    :depth depth}))
                          (let [plan (admitted-operation-plan operation (:state node))
                                outcome (reference/step (:state node) plan)
                                new-state (:after outcome)
                                new-node-id (inc (:transitions acc))
                                trace (conj (:trace node)
                                            {:operation-id id
                                             :status (:status outcome)
                                             :plan plan
                                             :failed-precondition (:failed-precondition outcome)
                                             :state new-state})
                                status-key (case (:status outcome)
                                             :committed :committed-attempts
                                             :rejected :rejected-attempts)]
                            (-> acc
                                (update :frontier conj {:state new-state
                                                        :trace trace
                                                        :node-id new-node-id})
                                (update :occurrence-states conj new-state)
                                (update :edges conj
                                        {:parent-node-id (:node-id node)
                                         :node-id new-node-id
                                         :depth (inc depth)
                                         :operation-id id
                                         :status (:status outcome)})
                                (update :transitions inc)
                                (update :found next-rule-violations rules trace new-state)
                                (update-in [:exercise id status-key] inc)))))))
                  acc operations))
               {:frontier [] :transitions transitions :found found
                :exercise exercise :edges edges
                :occurrence-states occurrence-states}
               frontier)]
          (recur (inc depth) next-frontier next-transitions next-found
                 next-exercise next-edges next-occurrence-states))))))
