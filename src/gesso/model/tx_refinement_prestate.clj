(ns gesso.model.tx-refinement-prestate
  "The bounded PREWRITE obligation for Gesso's admitted XTDB transaction subset.

   XTDB's documented DML semantics serialize transactions and evaluate their
   statements sequentially. Gesso's admitted tx-model semantics, however,
   require ALL guards to be evaluated against one pre-write state. This
   module verifies the *local, inspectable lowering prerequisite* for that
   claim: every admitted ASSERT precedes every put-docs operation, in precisely
   the order required by the finite reference semantics, and no other
   transaction operations are admitted.

   The result explicitly separates a checked Gesso construction from the
   still-external obligations: XTDB must interpret ASSERT and put-docs as
   assumed, the transaction must be atomic and serialized, and its INTERNAL
   prestate must be related to the intended abstract state. Pinned reads,
   ledger adjacency, and repeated observations alone cannot discharge that
   last obligation. This file creates neither a proof certificate nor a
   substitute backend model. It adds no developer-facing specification DSL."
  (:require [gesso.model.command :as command]
            [gesso.model.tx :as tx]
            [gesso.model.tx-model :as model]))

(def contract-scope
  {:obligation :gesso.model.tx/prewrite-serial-refinement-v1
   :checked #{:admitted-canonical-transaction-subset
              :normalized-plan-to-unformatted-ops-correspondence
              :assertions-precede-all-writes
              :precondition-order-matches-finite-reference
              :dependency-and-command-targets-match-reference
              :single-put-docs-write-per-command
              :formatted-transaction-operation-kind-and-count}
   :external-premises #{:xtdb-dml-is-serialized-in-transaction-log-order
                        :xtdb-asserts-evaluate-against-current-transaction-state
                        :xtdb-assert-failure-rolls-back-all-transaction-writes
                        :xtdb-put-docs-atomically-realizes-command-after-documents
                        :xtdb-sql-predicates-refine-finite-guard-semantics
                        :concrete-transaction-internal-prestate-relates-to-abstract-prestate
                        :relevant-external-writers-and-policy-dependencies-are-closed
                        :transaction-valid-time-and-value-representation-is-admitted}
   :not-established #{:internal-prestate-from-pinned-external-observations
                      :backend-premises-proved-from-xtdb-source
                      :production-execution-or-ledger-attestation
                      :arbitrary-sql-or-noncanonical-transactions
                      :durable-command-replay-or-uncertain-outcome-resolution
                      :application-authorization-or-business-rule-proof
                      :browser-or-choreo-refinement
                      :universal-physical-to-abstract-step-refinement}})

(defn- assert-sql?
  "Formatted canonical assertion statements are SQL vectors, not XTDB writes.
   An unknown formatted operation must never count as a verified ASSERT."
  [operation]
  (and (vector? operation)
       (string? (first operation))
       (boolean (re-find #"(?i)^\s*ASSERT\b" (first operation)))))

(defn- put-op?
  [operation]
  (and (vector? operation)
       (= 3 (count operation))
       (= :put-docs (first operation))
       (keyword? (second operation))
       (map? (nth operation 2))))

(defn derive-obligation
  "Derive a LOCAL lowering obligation from an admitted model transaction plan.

   The returned :preconditions identify the ordered abstract checks by index;
   each index is also the position of its physical ASSERT in the prepared
   transaction. All those ASSERTs must precede the first put-docs write.
   The result contains only counts, check kinds, and target TABLES, not IDs,
   expected guard values, SQL parameters, or entire application documents.

   This does not evaluate any predicate against XTDB's actual internal read
   state. A true :local-structure-valid? establishes the emitted operation
   ORDER and recognized operation SHAPES only. It is not an executable
   refinement theorem or a claim that external backend premises hold.

   Unsupported plans fail using tx-model's existing typed admission checks."
  [plan]
  (let [plan (model/admitted-plan plan)
        commands (:commands plan)
        guards (tx/effective-guards commands (:guards plan))
        explicit (:assertions plan)
        expected-assertions (vec (concat explicit
                                         (map tx/guard-assertion guards)
                                         (map tx/command-precondition commands)))
        expected-writes (mapv tx/command->tx-op commands)
        expected-unformatted (into expected-assertions expected-writes)
        actual-unformatted (tx/transaction-ops plan)
        prepared (:tx-ops (tx/prepare {} plan))
        assertion-count (count expected-assertions)
        write-count (count expected-writes)
        [formatted-asserts formatted-writes] (split-at assertion-count prepared)
        [raw-asserts raw-writes] (split-at assertion-count actual-unformatted)
        ;; step's checks are constructed independent of any concrete state;
        ;; using the empty state exposes ALL of their ordered kinds/targets.
        reference-checks (:checks (model/step {} plan))
        explicit-count (count explicit)
        guard-count (count guards)
        reference-guard-checks (take guard-count (drop explicit-count reference-checks))
        reference-command-checks (drop (+ explicit-count guard-count) reference-checks)
        expected-kinds (vec (concat (repeat explicit-count :document-field-count)
                                    (repeat guard-count :dependency-version)
                                    (map (fn [item]
                                           (if (command/create? item)
                                             :document-absent
                                             :command-version))
                                         commands)))
        checks {:canonical-operation-sequence
                (= expected-unformatted actual-unformatted)
                :precondition-count
                (= assertion-count (count reference-checks))
                :precondition-kinds-and-order
                (= expected-kinds (mapv :kind reference-checks))
                :dependency-guard-targets
                (= (mapv command/guard-target guards)
                   (mapv :target reference-guard-checks))
                :command-targets
                (= (mapv command/target commands)
                   (mapv :target reference-command-checks))
                :prewrite-assertion-prefix
                (= expected-assertions (vec raw-asserts))
                :write-only-suffix
                (= expected-writes (vec raw-writes))
                :formatted-operation-count
                (= (+ assertion-count write-count) (count prepared))
                :formatted-assertion-prefix
                (and (= assertion-count (count formatted-asserts))
                     (every? assert-sql? formatted-asserts))
                :formatted-put-only-suffix
                (and (= write-count (count formatted-writes))
                     (every? put-op? formatted-writes)
                     (= expected-writes (vec formatted-writes)))}
        positions (mapv (fn [i check]
                          {:assertion-index i
                           :kind (:kind check)
                           :table (first (:target check))})
                        (range (count reference-checks))
                        reference-checks)]
    {:obligation (:obligation contract-scope)
     :local-structure-valid? (every? true? (vals checks))
     :checks checks
     :preconditions positions
     :assertion-count assertion-count
     :write-count write-count
     :first-write-index assertion-count
     :backend-premises (:external-premises contract-scope)
     :undischarged-premises (:external-premises contract-scope)
     :contract-scope contract-scope
     :certifies-physical-execution? false
     :proves-internal-prestate? false
     :proves-general-refinement? false}))
