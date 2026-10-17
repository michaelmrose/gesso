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
   or general physical-to-abstract refinement. The optional
   observe-and-compare-bracketed! further checks that the submitted tx ID
   exceeds a ledger high watermark read from the pinned before-snapshot and
   appears in the pinned after-snapshot. This does not exclude intervening
   external transactions. The further optional ledger-window comparison
   requires the transaction ID to be exactly the next integer ID and the
   pinned after-ledger watermark to equal that transaction ID. It detects
   ledger-visible intervening transactions in the observed interval, but
   is still not a proof of the transaction-internal read snapshot.
   The optional repeatability check re-reads each phase at its EXACT
   original snapshot token and clock after submission. A change to either
   observed target documents or ledger watermarks fails the comparison.
   This establishes only observed query-basis repeatability, not an internal
   transaction snapshot or historical certainty beyond the samples.
   In particular, a caller still supplies the node,
   plan, and tracked target domain; this API does not identify or attest the
   real application production boundary."
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [gesso.model.command :as command]
   [gesso.model.tx :as tx]
   [gesso.model.tx-model :as model]
   [gesso.model.tx-refinement :as refinement]
   [gesso.model.tx-refinement-prestate :as prestate]
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

(defn- read-target-domain!
  "Re-read all targets at an already acquired, unchanged observation basis."
  [node targets basis]
  (into {}
        (map (fn [target]
               [target (read-target! node basis target)]))
        targets))

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

(def bracketed-ledger-collection-scope
  "The opt-in ledger-bracketed diagnostic captures the highest transaction
   ID *already visible in the pinned before-snapshot*, before submitting the
   new transaction. It checks that the execute-tx transaction has a strictly
   greater ID and is present in the pinned after-snapshot's ledger. This
   gives a concrete, two-sided physical transaction boundary provided that
   XTDB ledger IDs are monotonically allocated. It does not establish the
   transaction's internal read snapshot, the absence of intervening writers,
   or general refinement."
  (-> ledger-collection-scope
      (assoc :collector :gesso.model.tx/direct-xtdb-ledger-bracketed-step-v1)
      (update :collected into #{:pinned-before-ledger-high-watermark
                                :executed-tx-id-greater-than-before-watermark})
      (update :assumed conj :monotonic-xtdb-transaction-ids)
      (update :not-established conj :no-intervening-external-transactions)))

(def ledger-window-collection-scope
  "Opt-in, fail-closed LEDGER-WINDOW diagnostic. In addition to a valid
   bracketed result, require the executed transaction ID to be the immediate
   integer successor of the pinned before-ledger watermark and the largest
   ID visible in the pinned after-ledger to be that same transaction ID.

   This detects ledger-visible transactions in the observation window, and
   conservatively rejects ID gaps even if they were not caused by another
   writer. It cannot establish which snapshot the submitted transaction
   used internally, policy dependency closure, future commits of in-flight
   transactions, or the absence of activity outside the pinned window."
  (-> bracketed-ledger-collection-scope
      (assoc :collector :gesso.model.tx/direct-xtdb-ledger-window-step-v1)
      (update :collected into
              #{:pinned-after-ledger-high-watermark
                :executed-tx-immediate-successor-of-before-watermark
                :executed-tx-equals-after-ledger-watermark})
      (update :assumed into
              #{:ledger-high-watermarks-include-all-visible-tx-ids
                :unique-monotonic-xtdb-transaction-ids})
      (update :not-established into
              #{:no-writer-outside-pinned-observation-window
                :no-in-flight-or-future-external-commits
                :transaction-snapshot-attestation})))

(def repeatable-ledger-window-collection-scope
  "Opt-in observation replay at EXACTLY the original pinned before/after
   bases, including both snapshot-token and clock. The collector re-reads
   each target and each ledger watermark after submission, and reports false
   checks if any differ from the first read at that basis.

   A snapshot token must never be silently reinterpreted with a later clock:
   XTDB 2.2.0-beta1 can expose later rows when :current-time changes even
   though :snapshot-token is unchanged. Replay deliberately reuses both
   original values. This is only a bounded observation-consistency check."
  (-> ledger-window-collection-scope
      (assoc :collector :gesso.model.tx/direct-xtdb-repeatable-ledger-window-v1)
      (update :collected into
              #{:before-document-replay-at-identical-basis
                :after-document-replay-at-identical-basis
                :before-ledger-watermark-replay-at-identical-basis
                :after-ledger-watermark-replay-at-identical-basis})
      (update :not-established into
              #{:transaction-internal-read-basis-attested
                :all-possible-replays-are-repeatable
                :concurrent-writers-are-excluded
                :retroactive-basis-clock-rebinding-safe})))

(defn- ledger-watermark!
  "Capture the highest ID visible at the actual pinned observation basis.
   Empty history uses -1. Malformed before evidence fails before submission;
   malformed after evidence fails AFTER submission with no retry."
  [node basis phase]
  (let [rows (xt/q node
                   "SELECT _id FROM xt.txs ORDER BY _id DESC LIMIT 1"
                   basis)]
    (cond
      (= [] rows) -1
      (and (sequential? rows)
           (= 1 (count rows))
           (map? (first rows))
           (integer? (:xt/id (first rows)))
           (<= 0 (:xt/id (first rows))))
      (:xt/id (first rows))
      :else
      (fail! (if (= phase :before)
               ::invalid-before-ledger-watermark
               ::invalid-after-ledger-watermark)
             "XTDB returned an invalid pinned ledger watermark."
             {:phase phase
              :row-count (when (sequential? rows) (count rows))}))))

(def composed-step-scope
  "Local step derivation and physical collection are performed in ONE controlled
   execution.  Only the captured physical before-read is used as the finite
   prestate.  Agreement of both diagnostics is still conditional on all named
   backend and collection premises; it is never a certification or an internal
   snapshot witness.  No application author supplies a second specification."
  {:composition :gesso.model.tx/collected-conditional-step-v1
   :checked #{:finite-prestate-derived-from-same-physical-before-observation
              :local-lowering-and-predicate-argument-valid-before-submission
              :direct-physical-submission-and-repeatable-window-observation
              :separate-local-and-observed-correspondence-outcomes}
   :undischarged-premises
   (set/union (:undischarged-premises prestate/conditional-step-scope)
              (:assumed repeatable-ledger-window-collection-scope))
   :not-established
   (set/union (:not-established prestate/conditional-step-scope)
              (:not-established repeatable-ledger-window-collection-scope)
              #{:backend-premises-discharged-by-observational-agreement
                :transaction-internal-prestate-attested
                :universal-physical-to-abstract-refinement})})

(defn- combine-conditional-observations
  "Do NOT infer physical correctness from a well-formed local conditional.
   Inconsistent physical observations suppress joint agreement."
  [report conditional]
  (let [observed? (true? (get-in report [:comparison :corresponds?]))
        local? (true? (:conditional-argument-admissible? conditional))
        joint? (and local? observed?)]
    (-> report
        ;; The original observation comparison identifies targets by their
        ;; private IDs.  The composed report exposes only mismatch counts.
        (update :comparison
                (fn [comparison]
                  (-> comparison
                      (assoc :prestate-mismatch-count
                             (count (:prestate-mismatches comparison))
                             :poststate-mismatch-count
                             (count (:poststate-mismatches comparison)))
                      (dissoc :prestate-mismatches :poststate-mismatches))))
        (assoc :conditional-step conditional
               :composed-assurance
               {:status (if joint?
                          :locally-valid-and-observationally-consistent
                          :observation-mismatch)
                :local-argument-valid? local?
                :physical-observations-correspond? observed?
                :joint-evidence-consistent? joint?
                :undischarged-premises (:undischarged-premises composed-step-scope)
                :not-established (:not-established composed-step-scope)
                :scope composed-step-scope
                :certifies-physical-execution? false
                :proves-internal-prestate? false
                :proves-general-refinement? false}))))

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
  [input ledger-mode]
  (let [composed? (= :composed-repeatable-window ledger-mode)
        ledger? (not= :none ledger-mode)
        replay? (contains? #{:repeatable-window :composed-repeatable-window} ledger-mode)
        window? (contains? #{:window :repeatable-window :composed-repeatable-window} ledger-mode)
        bracketed? (contains? #{:bracketed :window :repeatable-window :composed-repeatable-window} ledger-mode)
        {:keys [node plan targets]} (require-input! input)
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
          ;; Capture the high watermark before submission; never query an old
          ;; snapshot for a future tx ID after execute-tx has returned.
          before-ledger-watermark (when bracketed?
                                    (ledger-watermark! node (:basis before) :before))
          reference-before (refinement/observation-state
                            targets (:documents before))
          ;; The conditional argument is derived from EXACTLY these physical
          ;; before-reads, not a second caller-supplied state or later query.
          ;; If its local proof checks fail, do not submit a transaction.
          conditional (when composed?
                        (prestate/derive-conditional-step reference-before plan))
          _ (when (and composed?
                       (not (:conditional-argument-admissible? conditional)))
              (fail! ::invalid-local-argument
                     "Local conditional checks failed before physical submission."
                     {:failed-checks (:failed-local-checks conditional)}))
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
              bracket-checks (when bracketed?
                               {:executed-tx-after-before-watermark
                                (< before-ledger-watermark (:tx-id tx-key))})
              ;; This read is pinned to the EXACT after basis used for the
              ;; document observations and the transaction-specific ledger
              ;; row. A later, unpinned SELECT must never supply its value.
              after-watermark (when window?
                                (ledger-watermark! node (:basis after) :after))
              window-checks (when window?
                              {:executed-tx-immediate-successor
                               (= (inc before-ledger-watermark) (:tx-id tx-key))
                               :executed-tx-equals-after-watermark
                               (= (:tx-id tx-key) after-watermark)})
              ;; Replaying both pinned observations uses the ORIGINAL basis,
              ;; not a newly captured token or a modified valid-time clock.
              ;; Failure is a false comparison, never an execution retry.
              replay-checks (when replay?
                              {:before-documents-repeat-at-pinned-basis
                               (= (:documents before)
                                  (read-target-domain! node targets (:basis before)))
                               :after-documents-repeat-at-pinned-basis
                               (= (:documents after)
                                  (read-target-domain! node targets (:basis after)))
                               :before-ledger-watermark-repeats-at-pinned-basis
                               (= before-ledger-watermark
                                  (ledger-watermark! node (:basis before) :before))
                               :after-ledger-watermark-repeats-at-pinned-basis
                               (= after-watermark
                                  (ledger-watermark! node (:basis after) :after))})
              checks (merge (:checks comparison)
                            (:checks ledger)
                            bracket-checks
                            window-checks
                            replay-checks)]
          (cond-> (-> base-report
              (assoc :comparison
                     (assoc comparison
                            :checks checks
                            :corresponds? (every? true? (vals checks))))
              (assoc :physical-tx-id (:tx-id tx-key)
                     :transaction-ledger-matches? (:matches? ledger)
                     :collection-scope (cond
                                         replay? repeatable-ledger-window-collection-scope
                                         window? ledger-window-collection-scope
                                         bracketed? bracketed-ledger-collection-scope
                                         :else ledger-collection-scope)
                     :evidence-kind (cond
                                      replay? :direct-controlled-xtdb-repeatable-ledger-window-observation
                                      window? :direct-controlled-xtdb-ledger-window-observation
                                      bracketed? :direct-controlled-xtdb-ledger-bracketed-observation
                                      :else :direct-controlled-xtdb-ledger-linked-observation)))
            composed? (combine-conditional-observations conditional)))))))

(defn observe-and-compare!
  "Original v1006 diagnostic API. Pin the two observation phases, submit once,
   and compare physical documents, lowering, and definitive outcome. No
   transaction-ledger query is performed by this compatibility entry point."
  [input]
  (collect-step! input :none))

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
  (collect-step! input :ledger))

(defn observe-and-compare-bracketed!
  "Opt-in stricter controlled-XTDB diagnostic. It extends the ledger-linked
   comparison with one additional check: the identified tx-id must exceed
   the highest tx-id visible at the pinned BEFORE basis and be independently
   recorded at the pinned AFTER basis. The returned :comparison/:checks includes
   :executed-tx-after-before-watermark. Any inability to check this
   additional boundary is NOT silently promoted to correspondence.

   The input shape is unchanged: {:node ... :plan ... :targets ...}.
   The transaction is submitted only once. This API is unsuitable for
   production or retry/reconciliation and must not be treated as proof of
   backend isolation, an authenticated submission, or universal refinement."
  [input]
  (collect-step! input :bracketed))

(defn observe-and-compare-ledger-window!
  "Strictly stronger opt-in diagnostic than observe-and-compare-bracketed!.

   Verify the returned tx ID is the *immediate integer successor* of the
   pinned before-ledger high watermark and equals the pinned after-ledger high
   watermark. Both ledger high-watermark queries use the same respective
   snapshot and clock as that phase's target reads. An observed intervening
   transaction, an ID gap, or a later committed transaction makes
   :comparison/:corresponds? false, even if document observations agree.
   Missing or malformed pre-submission evidence fails before any submission;
   post-submission evidence failure propagates without retry.

   This is evidence only for the ledger-visible interval between these two
   pinned bases. It is neither authenticated production evidence nor proof
   of the transaction-internal read snapshot, exclusive control of the
   database, or general XTDB refinement. Same strict three-key input."
  [input]
  (collect-step! input :window))

(defn observe-and-compare-repeatable-window!
  "Opt-in extension of observe-and-compare-ledger-window!. After submitting
   the transaction ONCE, repeat the before- and after-phase document queries
   and ledger watermark queries at each phase's EXACT original pinned basis.
   Any changed result makes :comparison/:corresponds? false. Unknown XTDB
   errors still propagate; no retry or claim of an authenticated receipt.

   The snapshot-token and current-time fields must be preserved together:
   a token reused at a different clock is NOT asserted to preserve visibility.
   This diagnostic does not establish the execution's internal read snapshot,
   complete policy dependencies, or general physical-to-abstract refinement.
   The input remains exactly {:node ... :plan ... :targets ...}."
  [input]
  (collect-step! input :repeatable-window))

(defn observe-and-derive-conditional!
  "Opt-in ONE-execution composition of the v1025 conditional step argument
   with the v1016 repeatable-window XTDB comparison. Takes exactly
   {:node ... :plan ... :targets ...}; the finite prestate is derived from the
   collector's OWN pinned physical before-read. Local checks are performed
   BEFORE submitting the canonical XTDB transaction and fail closed if invalid.
   Execute-tx is called at most once; unknown outcomes propagate unchanged.

   The returned :conditional-step and :comparison remain separate and are
   joined only as :composed-assurance/:joint-evidence-consistent?.  This flag
   asserts observed agreement of two bounded checks, NOT backend truth,
   independent SQL equivalence, an internal transaction snapshot, or proof.
   All still-needed backend and controlled-database premises remain named.
   Mismatched private target identities are redacted in this opt-in report.
   This is an active diagnostic; never use as production claim processing."
  [input]
  (collect-step! input :composed-repeatable-window))
