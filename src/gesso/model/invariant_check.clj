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
