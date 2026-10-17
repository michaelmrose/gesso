(ns gesso.model.tx-refinement-xtdb
  "Controlled physical XTDB2 observation for the finite tx-refinement relation.

   observe-and-compare! accepts an admitted model transaction and a complete
   target domain, reads each target directly from the supplied XTDB node,
   submits exactly the operations produced by production tx/prepare through
   xtdb.api/execute-tx, and reads the targets again. The model's reference
   prestate is derived from the actual *before* reads; callers cannot provide
   a separate fabricated reference prestate, submission record, or outcome.

   This is an active, write-performing DIAGNOSTIC intended for an exclusively
   controlled database. It is not a production Live execution wrapper, a
   transaction interceptor, a passive auditor, or a safe retry/reconciliation
   API. Reads are separate SELECT queries, not one pinned database snapshot.
   No other writer may change these targets between the first and last read.
   A transaction error other than the recognized XTDB assertion rejection is
   propagated as unknown; no second submission is made.

   Matching observations are not proof of XTDB serialization, an authenticated
   execution receipt, completeness of policy dependencies, authorization,
   or general physical-to-abstract refinement. In particular, a caller still
   supplies the node, plan, and tracked target domain; this API does not
   identify or attest the real application production boundary."
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [gesso.model.command :as command]
   [gesso.model.tx :as tx]
   [gesso.model.tx-model :as model]
   [gesso.model.tx-refinement :as refinement]
   [xtdb.api :as xt]))

(def collection-scope
  {:collector :gesso.model.tx/direct-xtdb-step-observation-v1
   :collected #{:reference-prestate-from-physical-reads
                :complete-explicit-target-reads
                :operations-from-production-tx-prepare
                :same-operations-passed-to-xtdb-execute-tx
                :definitive-xtdb-outcome-classification
                :physical-poststate-reads
                :recorded-step-comparison}
   :assumed #{:exclusive-control-of-tracked-documents-during-collection
              :all-semantic-read-and-write-dependencies-identified
              :consistent-series-of-separate-xtdb-reads
              :synchronous-execute-tx-outcome-is-definitive
              :supplied-node-is-the-intended-controlled-database}
   :not-established #{:authenticated-physical-submission-receipt
                      :production-live-execution-correspondence
                      :one-pinned-snapshot-across-all-target-reads
                      :physical-read-completeness-outside-declared-targets
                      :absence-of-external-writers
                      :backend-isolation-and-temporal-history
                      :general-xtdb-step-refinement
                      :safe-unknown-outcome-reconciliation
                      :authorization-and-business-invariants
                      :durable-command-incorporation-and-replay
                      :browser-or-choreography-correspondence}})

(defn- fail!
  [kind message details]
  (throw (ex-info message {:error/type kind :error/details details})))

(defn- require-input!
  [input]
  (let [expected #{:node :plan :targets}]
    (when-not (and (map? input) (= expected (set (keys input))))
      (fail! ::invalid-input
             "The physical collector requires exactly node, plan, and targets."
             {:received (if (map? input) (set (keys input)) #{})}))
    (when (nil? (:node input))
      (fail! ::missing-node "An XTDB node must be supplied." {}))
    (refinement/require-targets (:targets input))
    input))

(defn- safe-sql-table!
  [table]
  (let [parts (if-let [schema (namespace table)]
                [schema (name table)]
                [(name table)])]
    (when-not (every? #(re-matches #"[A-Za-z_][A-Za-z0-9_]*" %) parts)
      (fail! ::unsupported-table
             "The XTDB collector supports only simple SQL table identifiers."
             {:table table}))
    (str/join "." parts)))

(defn- required-targets
  [plan]
  ;; The checks are computed even if every check fails in the empty state;
  ;; their targets are independent of the current database contents.
  (let [checks (:checks (model/step {} plan))]
    (set (concat (map :target checks)
                 (map command/target (:commands plan))))))

(defn- read-target!
  [node [table id]]
  (let [query (str "SELECT * FROM " (safe-sql-table! table)
                   " WHERE _id = ? LIMIT 2")
        rows (xt/q node [query id])]
    (when-not (and (sequential? rows) (<= (count rows) 1))
      (fail! ::invalid-physical-read
             "Expected at most one row for each queried document."
             {:table table}))
    (first rows)))

(defn- observe!
  [node targets]
  ;; Exact keys, including nil for queried-absent documents.
  (into {} (map (fn [target] [target (read-target! node target)])) targets))

(defn- execute-definitively!
  [node operations]
  (try
    (xt/execute-tx node operations)
    {:status :committed}
    (catch Throwable error
      (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data error)))
        {:status :rejected :error/code :xtdb/assert-failed}
        ;; A timeout or unknown exception is NOT evidence of rejection.
        ;; Never automatically retry a potentially incorporated command.
        (throw error)))))

(defn observe-and-compare!
  "Execute one admitted finite transaction on a CONTROLLED XTDB node.

   The input is exactly {:node node :plan admitted-plan :targets [[table id] ...]}.
   The targets must include all command, guard, and ASSERT dependencies;
   additional targets can be observed to detect unintended changes within a
   declared physical footprint. Plan admission, footprint validation, and SQL
   table-name checks occur BEFORE the first read or transaction submission.

   The collector itself obtains the physical before/after documents, captures
   the exact canonical operation vector it submits, classifies the definitive
   XTDB outcome, and delegates comparison to compare-recorded-step. It derives
   :reference-before from the physical before-observation. This makes the
   result stronger than an arbitrary caller-composed seven-field comparison,
   but it remains controlled diagnostic evidence, never a certification.

   The returned report deliberately excludes document bodies and operation
   payloads. It carries a bounded comparison, known physical outcome, and
   collection scope. For unknown outcomes the exception propagates instead
   of returning a misleading comparison or attempting reconciliation."
  [input]
  (let [{:keys [node plan targets]} (require-input! input)
        plan (model/admitted-plan plan)
        targets (refinement/require-targets targets)
        needed (required-targets plan)
        missing (set/difference needed (set targets))]
    (when (seq missing)
      (fail! ::untracked-dependencies
             "Physical observations omit required transaction targets."
             {:untracked missing}))
    ;; Reject unsupported table names before the first physical action.
    (doseq [[table _] targets]
      (safe-sql-table! table))
    (let [operations (:tx-ops (tx/prepare {} plan))
          before (observe! node targets)
          reference-before (refinement/observation-state targets before)
          execution (execute-definitively! node operations)
          after (observe! node targets)
          comparison (refinement/compare-recorded-step
                      {:reference-before reference-before
                       :plan plan
                       :targets targets
                       :observed-before before
                       :recorded-tx-ops operations
                       :execution execution
                       :observed-after after})]
      {:comparison comparison
       :execution execution
       :collected-target-count (count targets)
       :submitted-operation-count (count operations)
       :collection-scope collection-scope
       :evidence-kind :direct-controlled-xtdb-observation
       :certifies-physical-execution? false})))
