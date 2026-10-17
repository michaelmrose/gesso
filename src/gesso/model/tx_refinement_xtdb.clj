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
   API. Before-reads share one explicit XTDB snapshot token and clock time;
   after-reads share another. The two phases intentionally do NOT share one
   snapshot. They are not a snapshot of the transaction's internal read set.
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
   [xtdb.api :as xt])
  (:import [java.time Instant]))

(def collection-scope
  {:collector :gesso.model.tx/direct-xtdb-step-observation-v1
   :collected #{:phase-local-pinned-snapshot-token-and-clock
                :reference-prestate-from-physical-reads
                :complete-explicit-target-reads
                :operations-from-production-tx-prepare
                :same-operations-passed-to-xtdb-execute-tx
                :definitive-xtdb-outcome-classification
                :physical-poststate-reads
                :recorded-step-comparison}
   :assumed #{:exclusive-control-of-tracked-documents-during-collection
              :all-semantic-read-and-write-dependencies-identified
              ;; Still required BETWEEN the two distinct observation phases:
              ;; each phase is pinned, but an external writer can run before
              ;; submission, during the transaction, or before after-reads.
              :consistent-series-of-separate-xtdb-reads
              :synchronous-execute-tx-outcome-is-definitive
              :supplied-node-is-the-intended-controlled-database}
   :not-established #{:transaction-internal-snapshot-equals-observed-prestate
                      :same-snapshot-for-before-and-after-phases
                      :authenticated-physical-submission-receipt
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

(defn- phase-basis!
  "Acquire a database snapshot token and a fixed valid-time clock for ONE
   observation phase. No unpinned fallback is allowed: partial/coincidentally
   matching cross-table reads are not acceptable refinement evidence.

   SHOW SNAPSHOT_TOKEN is supported by the XTDB2 2.2 Clojure SQL interface.
   The token is intentionally kept internal and never included in reports."
  [node]
  (let [rows (xt/q node "SHOW SNAPSHOT_TOKEN")
        token (when (and (sequential? rows) (= 1 (count rows)))
                (:snapshot-token (first rows)))]
    (when-not (and (string? token) (seq token))
      (fail! ::missing-snapshot-token
             "XTDB did not return exactly one usable snapshot token."
             {:row-count (when (sequential? rows) (count rows))}))
    {:snapshot-token token
     :current-time (Instant/now)}))

(defn- read-target!
  [node basis [table id]]
  (let [query (str "SELECT * FROM " (safe-sql-table! table)
                   " WHERE _id = ? LIMIT 2")
        rows (xt/q node [query id] basis)]
    (when-not (and (sequential? rows) (<= (count rows) 1))
      (fail! ::invalid-physical-read
             "Expected at most one row for each queried document."
             {:table table}))
    (first rows)))

(defn- observe-phase!
  [node targets]
  ;; The basis belongs to this entire phase, including any optional ledger
  ;; query. A later query must not silently move to the newest XTDB snapshot.
  (let [basis (phase-basis! node)]
    {:basis basis
     :documents (into {}
                      (map (fn [target]
                             [target (read-target! node basis target)]))
                      targets)}))

(defn- execute-definitively!
  [node operations]
  (try
    ;; Both this successful return and a recognized assertion conflict carry
    ;; an XTDB tx-key in 2.2.0-beta1. Retain it *internally* for the optional
    ;; independent xt.txs check, without passing it to the finite comparison.
    {:execution {:status :committed}
     :tx-key (xt/execute-tx node operations)}
    (catch Throwable error
      (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data error)))
        {:execution {:status :rejected :error/code :xtdb/assert-failed}
         :tx-key (:tx-key (ex-data error))}
        ;; A timeout or unknown exception is NOT evidence of rejection.
        ;; Never automatically retry a potentially incorporated command.
        (throw error)))))

(def ledger-collection-scope
  "The opt-in ledger-linked diagnostic adds a read of XTDB's own transaction
   record at precisely the same pinned post-observation basis. It checks the
   returned/exception-carried tx-key against the ledger ID, system time, and
   committed flag. This improves evidence about *which* physical transaction
   was observed; it does not certify its submitted payload, internal snapshot,
   authorization, or application-wide write isolation."
  (-> collection-scope
      (assoc :collector :gesso.model.tx/direct-xtdb-ledger-linked-step-v1)
      (update :collected into
              #{:tx-key-from-physical-execute-tx
                :xt-txs-record-from-pinned-post-observation
                :ledger-id-system-time-and-commit-status-comparison})
      (update :assumed conj :trustworthy-xtdb-tx-key-and-system-ledger)))

(defn- tx-key!
  [tx-key]
  (when-not (and (map? tx-key)
                 (= #{:tx-id :system-time} (set (keys tx-key)))
                 (integer? (:tx-id tx-key))
                 (<= 0 (:tx-id tx-key))
                 (instance? Instant (:system-time tx-key)))
    (fail! ::missing-tx-key
           "XTDB did not supply an identifiable transaction key for the definitive outcome."
           {}))
  tx-key)

(defn- ledger-evidence!
  [node basis {:keys [tx-id system-time]} execution]
  ;; Select only non-sensitive transaction metadata. Query at the SAME basis
  ;; as poststate rather than letting a new query select a different history.
  (let [rows (xt/q node
                   ["SELECT _id, committed, system_time FROM xt.txs WHERE _id = ?"
                    tx-id]
                   basis)
        row (when (and (sequential? rows) (= 1 (count rows)))
              (first rows))
        present? (map? row)
        id? (and present? (= tx-id (:xt/id row)))
        status? (and present? (boolean? (:committed row))
                     (= (= :committed (:status execution)) (:committed row)))
        time? (and present?
                   (instance? java.time.ZonedDateTime (:system-time row))
                   (= system-time (.toInstant ^java.time.ZonedDateTime
                                              (:system-time row))))
        checks {:tx-ledger-record-present present?
                :tx-ledger-id id?
                :tx-ledger-commit-status status?
                :tx-ledger-system-time time?}]
    {:checks checks
     :matches? (every? true? (vals checks))}))

(defn- collect-step!
  "Execute one admitted finite transaction on a CONTROLLED XTDB node.

   The input is exactly {:node node :plan admitted-plan :targets [[table id] ...]}.
   The targets must include all command, guard, and ASSERT dependencies;
   additional targets can be observed to detect unintended changes within a
   declared physical footprint. Plan admission, footprint validation, and SQL
   table-name checks occur BEFORE the first read or transaction submission.

   Each physical observation phase pins all target reads to a single XTDB
   snapshot token and fixed clock time, acquired by the collector itself.
   The collector captures the canonical operation vector it submits,
   classifies the definitive XTDB outcome, and delegates comparison to
   compare-recorded-step. It derives
   :reference-before from the physical before-observation. This makes the
   result stronger than an arbitrary caller-composed seven-field comparison,
   but it remains controlled diagnostic evidence, never a certification.
   Even pinned before-reads may differ from the database snapshot seen by the
   submitted transaction if external writers are admitted. Pinned after-reads
   may include subsequent external writes. Those cross-phase guarantees still
   require controlled isolation and are not claimed by this collector.

   The returned report deliberately excludes document bodies and operation
   payloads. It carries a bounded comparison, known physical outcome, and
   collection scope. For unknown outcomes the exception propagates instead
   of returning a misleading comparison or attempting reconciliation."
  [input ledger?]
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
          before (observe-phase! node targets)
          reference-before (refinement/observation-state
                            targets (:documents before))
          {:keys [execution tx-key]} (execute-definitively! node operations)
          ;; A ledger-linked request must have a definite physical identity.
          ;; If absent after the submission, fail rather than fabricate a
          ;; rejected/committed comparison or try the operation again.
          tx-key (when ledger? (tx-key! tx-key))
          after (observe-phase! node targets)
          comparison (refinement/compare-recorded-step
                      {:reference-before reference-before
                       :plan plan
                       :targets targets
                       :observed-before (:documents before)
                       :recorded-tx-ops operations
                       :execution execution
                       :observed-after (:documents after)})
          base-report {:comparison comparison
                       :execution execution
                       :collected-target-count (count targets)
                       :submitted-operation-count (count operations)
                       :collection-scope collection-scope
                       :evidence-kind :direct-controlled-xtdb-observation
                       :observation-basis :separate-pinned-before-and-after-phases
                       :certifies-physical-execution? false}]
      (if-not ledger?
        base-report
        (let [ledger (ledger-evidence! node (:basis after) tx-key execution)
              checks (merge (:checks comparison) (:checks ledger))]
          (-> base-report
              (assoc :comparison
                     (assoc comparison
                            :checks checks
                            :corresponds? (every? true? (vals checks))))
              (assoc :physical-tx-id (:tx-id tx-key)
                     :transaction-ledger-matches? (:matches? ledger)
                     :collection-scope ledger-collection-scope
                     :evidence-kind :direct-controlled-xtdb-ledger-linked-observation)))))))

(defn observe-and-compare!
  "Original v1006 diagnostic API. Pin the two observation phases, submit once,
   and compare physical documents, lowering, and definitive outcome. No
   transaction-ledger query is performed by this compatibility entry point."
  [input]
  (collect-step! input false))

(defn observe-and-compare-ledger!
  "Stronger, opt-in controlled-XTDB diagnostic, with the SAME exact three-key
   input as observe-and-compare!. In addition to the bounded step comparison,
   link XTDB's success tx-key or assertion-conflict tx-key to xt.txs at the
   pinned poststate basis. Confirm ID, timestamp, and committed status.

   Missing/invalid tx-keys and XTDB query errors fail closed after submission:
   they NEVER trigger retries. Missing or contradictory ledger rows make
   :comparison/:corresponds? false with individual false ledger checks.

   A passing result is not an authenticated receipt or a refinement proof.
   In particular, the tx ledger does not attest the transaction's operations,
   the internal read snapshot, external-writer exclusion, or production-node
   identity; the controlled-DB assumptions remain explicit. The returned
   report excludes raw tokens, document bodies, and transaction payloads."
  [input]
  (collect-step! input true))
