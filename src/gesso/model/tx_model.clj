(ns gesso.model.tx-model
  "Experimental, pure reference semantics for the *existing* gesso.model.tx
   document-command subset. This is not an application command protocol or a
   new authoring DSL.

   Input is an ordinary gesso.model.tx plan and a finite map of current
   documents keyed by [entity-type xt/id]. Every decision is evaluated against
   one pre-write state; writes either all occur or none occur.

   Supported: canonical create/update commands, canonical version guards,
   and their duplicate/conflict normalization. Unsupported: arbitrary SQL
   assertions, custom transaction options, custom entry construction, and
   application policy. These are rejected, never silently assumed true.

   The resulting model is an *abstract serial-commit machine*. Its rules do
   not prove XTDB isolation, HoneySQL lowering, durable command recording,
   idempotent replay, authorization, Live publication, or business invariants.
   It is intended as a small executable subject for later correspondence
   tests and finite interleaving exploration, NOT a certification artifact."
  (:require
   [gesso.model.command :as command]
   [gesso.model.tx :as tx]))

(def reference-scope
  "Fixed statement of the model's scope, included in every execution result."
  {:model :gesso.model.tx/serial-commit-reference-v1
   :checked #{:canonical-command-preconditions
              :canonical-version-guards
              :atomic-document-write-set}
   :assumed #{:serialized-atomic-commit}
   :not-established #{:real-xtdb-correspondence
                      :business-authorization
                      :completeness-of-decision-dependencies
                      :arbitrary-sql-assertions
                      :durable-application-command-replay
                      :delivery-and-live-effects
                      :temporal-validity-and-isolation}})

(defn- fail!
  [type message details]
  (throw
   (ex-info message
            {:error/type type
             :error/details details})))

(defn require-state
  "Accepts only a finite current-document map. Each key is
   [entity-type document-id]; each value's :xt/id must agree with that key.
   Absent documents are represented by absent keys, never by nil values."
  [state]
  (when-not (map? state)
    (fail! ::invalid-state
           "Reference state must be a map of [entity-type id] to documents."
           {:state state}))
  (doseq [[target document] state]
    (when-not (and (vector? target)
                   (= 2 (count target))
                   (keyword? (first target))
                   (some? (second target))
                   (map? document)
                   (= (second target) (:xt/id document)))
      (fail! ::invalid-state
             "Reference state contains an invalid target or inconsistent document identity."
             {:target target :document document})))
  state)

(defn admitted-plan
  "Returns an existing normalized model transaction plan in the supported
   abstract subset, otherwise fails with a typed *unsupported* diagnostic.

   Raw SQL assertions must not be dropped: their semantics can determine
   whether a transaction commits. Similarly, custom XTDB transaction options
   and entry hooks cannot be interpreted by this deliberately small model.
   Semantic :changes and :emit are preserved by normalization but explicitly
   excluded from the reference result's established guarantees."
  [plan]
  (let [plan (tx/normalize-plan plan)
        unsupported (cond-> []
                      (seq (:assertions plan))
                      (conj :assertions)
                      (some? (:entry plan))
                      (conj :entry)
                      (some? (:entry-fn plan))
                      (conj :entry-fn)
                      (some? (:tx-options plan))
                      (conj :tx-options))]
    (when (seq unsupported)
      (fail! ::unsupported-plan
             "The reference model cannot establish the meaning of these transaction features."
             {:unsupported unsupported
              :supported #{:commands :guards}
              :plan plan}))
    plan))

(defn- matches-expected?
  [document {:model/keys [id checks]}]
  (and (map? document)
       (= id (:xt/id document))
       (every? (fn [[key value]]
                 (and (contains? document key)
                      (= value (get document key))))
               checks)))

(defn- preconditions
  "Reuses the transaction's established guard normalization; each returned
   check is evaluated against the *same* pre-write state."
  [{:keys [commands guards]}]
  (into
   (mapv (fn [guard]
           {:kind :dependency-version
            :target (command/guard-target guard)
            :expected (:model/expected guard)})
         (tx/effective-guards commands guards))
   (map (fn [model-command]
          (if (command/create? model-command)
            {:kind :document-absent
             :target (command/target model-command)}
            {:kind :command-version
             :target (command/target model-command)
             :expected (:model/expected model-command)}))
        commands)))

(defn- assess-precondition
  [state {:keys [kind target expected] :as check}]
  (let [actual (get state target)]
    (assoc check
           :satisfied?
           (case kind
             :document-absent
             (not (contains? state target))

             (:command-version :dependency-version)
             (matches-expected? actual expected)))))

(defn step
  "Executes one admitted plan against a pure reference state.

   Results are either :committed with the whole document write set applied,
   or :rejected with the state unchanged. A rejected plan returns the first
   failed precondition; all checks still describe the *original* state.
   Both results carry their exact verification scope.

   In particular, this does NOT deduplicate a repeated application command
   ID: a compatible retry of a previously incorporated domain command would
   need a durable command record not present in these model plans."
  [state plan]
  (let [state (require-state state)
        plan (admitted-plan plan)
        checks (mapv (partial assess-precondition state)
                     (preconditions plan))
        failed (first (remove :satisfied? checks))
        after (if failed
                state
                (reduce (fn [current model-command]
                          (assoc current
                                 (command/target model-command)
                                 (command/after model-command)))
                        state
                        (:commands plan)))]
    {:status (if failed :rejected :committed)
     :before state
     :after after
     :checks checks
     :failed-precondition failed
     :scope reference-scope}))

(defn- orderings
  "All permutations of a finite vector of *indices*, in deterministic order."
  [indices]
  (if (empty? indices)
    [[]]
    (mapcat (fn [index]
              (map (fn [suffix] (into [index] suffix))
                   (orderings (filterv #(not= index %) indices))))
            indices)))

(defn explore-orders
  "Explore every serialization of up to six *already prepared* plans.

   All plans are admitted before exploration. The same prepared input is used
   in every ordering: decisions are NOT recomputed after competing commits.
   Each path records the per-step outcome and after-state so a caller can
   observe stale decisions, conflicts, and possible invariant counterexamples.

   This is finite exhaustive exploration of serial commit orderings, not a
   model of arbitrary network schedules, retries, database isolation, or a
   general state-space theorem. No application-specific policy is inferred."
  [initial-state plans]
  (let [initial-state (require-state initial-state)]
    (when-not (vector? plans)
      (fail! ::invalid-plans
             "Finite exploration requires a vector of prepared plans."
             {:plans plans}))
    (when-not (<= 1 (count plans) 6)
      (fail! ::invalid-plans
             "Finite exploration supports between one and six prepared plans."
             {:count (count plans) :maximum 6}))
    (let [plans (mapv admitted-plan plans)]
      {:scope reference-scope
       :exploration {:kind :all-serial-orders
                     :prepared-plans (count plans)
                     :orderings (reduce *' 1 (range 1 (inc (count plans))))}
       :paths
       (mapv (fn [order]
               (loop [state initial-state
                      remaining order
                      trace []]
                 (if-let [index (first remaining)]
                   (let [result (step state (nth plans index))]
                     (recur (:after result)
                            (rest remaining)
                            (conj trace
                                  {:plan-index index
                                   :status (:status result)
                                   :state (:after result)
                                   :failed-precondition
                                   (:failed-precondition result)})))
                   {:order order
                    :trace trace
                    :final-state state})))
             (orderings (vec (range (count plans)))))})))
