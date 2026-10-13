(ns gesso.model.resolution-passive-observation
  "Passive, finite, authoritative-document observation around real application
   executions. Unlike resolution-observation/observe-and-check!, this namespace
   NEVER submits a transaction or invokes an application operation.

   Call begin-observation! BEFORE an independently initiated operation, and
   complete-observation! AFTER its physical XTDB outcome is known. Both phases
   read the relevant documents directly from the supplied XTDB node. The
   caller still supplies the exact plans it expects to have been submitted
   and one *physical* :committed/:rejected status per plan. A Gesso Live
   :reconciled, :already-incorporated, :failed or :unresolved settlement is
   NOT a physical transaction status. In particular, an unknown commit outcome
   must never be guessed from a Live settlement or from a missing record.

   This is a CONTROLLED-EXECUTION diagnostic, not a production auditing or
   certification subsystem. Reads are separate, unpinned XTDB queries, and
   external writes to relevant targets invalidate the serial-model premise.
   Neither a supplied status nor the capture handle is authenticated; a
   consistent witness does not establish that the supplied plans were really
   the ones executed, that the Live projection was correct, or that the
   business policy contains all necessary guards. An exception from an
   authoritative read always propagates instead of generating a witness."
  (:require
   [clojure.set :as set]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.resolution-evidence :as evidence]
   [gesso.model.tx :as tx]
   [gesso.model.tx-model :as model]
   [xtdb.api :as xt]))

(def ^:private request-keys #{:node :incorporations})
(def ^:private incorporation-keys #{:plan :record :scope})
(def ^:private capture-keys
  #{:kind :node :incorporations :inspected-targets :initial-state :scope})
(def ^:private capture-kind ::capture-v1)

(def observation-scope
  {:collector :gesso.model.resolution-passive-observation/xtdb-before-after-v1
   :collected #{:physical-initial-documents-from-xtdb
                :physical-final-documents-from-xtdb
                :guard-and-write-target-coverage
                :finite-serial-reference-comparison
                :no-transaction-submission-by-collector}
   :assumed #{:trusted-physical-transaction-status-inputs
              :supplied-plans-equal-executed-plans
              :controlled-execution-without-unrelated-writes
              :consistent-series-of-unpinned-xtdb-reads
              :synchronous-physical-outcomes-settled-before-final-read
              :supplied-node-is-the-relevant-authoritative-database}
   :not-established #{:authenticated-status-or-capture-provenance
                      :automatic-observation-of-application-transaction-statuses
                      :single-snapshot-cross-table-read
                      :application-execution-correspondence-without-trusted-inputs
                      :live-settlement-or-projection-correctness
                      :business-authorization-or-invariant-correctness
                      :policy-guard-completeness
                      :safe-retry-after-indeterminate-delivery
                      :unbounded-concurrency-or-linearizability
                      :formal-refinement-proof}})

(defn- fail! [kind message details]
  (throw (ex-info message
                  {:error/type ::invalid-passive-observation
                   :error/kind kind
                   :error/details details})))

(defn- require-exact-map! [value expected kind]
  (when-not (and (map? value) (= expected (set (keys value))))
    (fail! kind "Expected a closed passive-observation map."
           {:expected expected
            :actual-keys (when (map? value) (set (keys value)))}))
  value)

(defn- sql-table! [table]
  ;; Guard all dynamic SQL identifiers before any physical reads.
  ;; Reject qualified keywords: (name :foo/bar) would silently select :bar.
  (when-not (and (keyword? table) (nil? (namespace table))
                 (re-matches #"[A-Za-z_][A-Za-z0-9_]*" (name table)))
    (fail! :unsupported-table
           "The passive XTDB observer supports simple unqualified SQL tables."
           {:table table}))
  (name table))

(defn- require-target! [target]
  (when-not (and (vector? target) (= 2 (count target))
                 (some? (second target)))
    (fail! :invalid-target "Target must be [table document-id]."
           {:target target}))
  (sql-table! (first target))
  target)

(defn- require-incorporation! [incorporation]
  (require-exact-map! incorporation incorporation-keys :invalid-incorporation)
  (when-not (= resolution/assurance-scope (:scope incorporation))
    (fail! :invalid-incorporation-scope
           "Expected a canonical durable-command incorporation scope." {}))
  (let [plan (model/admitted-plan (:plan incorporation))
        record (resolution/require-record (:record incorporation))
        target [resolution/command-entity-type (:xt/id record)]
        record-commands (filterv #(= resolution/command-entity-type
                                     (first (command/target %)))
                                 (:commands plan))]
    (when-not (and (= 1 (count record-commands))
                   (command/create? (first record-commands))
                   (= target (command/target (first record-commands)))
                   (= record (command/after (first record-commands))))
      (fail! :invalid-command-record
             "The plan must atomically create exactly its declared durable command record."
             {:target target}))
    plan))

(defn- plan-targets [plan]
  (set/union
   (into #{} (map command/target) (:commands plan))
   (into #{} (map command/guard-target)
         (tx/effective-guards (:commands plan) (:guards plan)))))

(defn- admitted [incorporations]
  (when-not (and (vector? incorporations)
                 (<= 1 (count incorporations) 6))
    (fail! :invalid-incorporations
           "The finite observer requires one to six prepared incorporations."
           {:count (when (sequential? incorporations) (count incorporations))}))
  (let [plans (mapv require-incorporation! incorporations)
        targets (reduce set/union #{} (map plan-targets plans))]
    (doseq [target targets] (require-target! target))
    {:plans plans :targets targets}))

(defn- read-target! [node [table id :as target]]
  (let [rows (vec (xt/q node [(str "SELECT * FROM " (sql-table! table)
                                  " WHERE _id = ?") id]))]
    (when (> (count rows) 1)
      (fail! :duplicate-document
             "A physical target query returned more than one current row."
             {:target target :count (count rows)}))
    (first rows)))

(defn- read-state! [node targets]
  (model/require-state
   (into {}
         (keep (fn [target]
                 (when-let [document (read-target! node target)]
                   [target document])))
         (sort-by pr-str targets))))

(defn begin-observation!
  "Capture the initial physical target documents, BEFORE application commands
   are executed elsewhere. Does not submit, execute or plan a transaction.

     (begin-observation! {:node embedded-xtdb
                          :incorporations [canonical-incorporation ...]})

   Use this only in a controlled test/diagnostic window. Pre-admission includes
   all guarded and written targets, even command records not yet created.
   Returns a non-authenticated capture handle for complete-observation!."
  [request]
  (require-exact-map! request request-keys :invalid-request)
  (let [{:keys [node incorporations]} request]
    (when (nil? node)
      (fail! :missing-node "An XTDB node is required." {}))
    (let [{:keys [targets]} (admitted incorporations)]
      {:kind capture-kind
       :node node
       :incorporations incorporations
       :inspected-targets targets
       :initial-state (read-state! node targets)
       :scope observation-scope})))

(defn- require-capture! [capture]
  (require-exact-map! capture capture-keys :invalid-capture)
  (when-not (and (= capture-kind (:kind capture))
                 (= observation-scope (:scope capture))
                 (some? (:node capture)))
    (fail! :invalid-capture "Unrecognized passive-observation capture."
           {:kind (:kind capture)}))
  (let [{:keys [targets]} (admitted (:incorporations capture))]
    (when-not (= targets (:inspected-targets capture))
      (fail! :capture-target-mismatch
             "Capture targets no longer match the admitted incorporation plans."
             {:expected targets :received (:inspected-targets capture)}))
    (model/require-state (:initial-state capture))
    (when-not (set/subset? (set (keys (:initial-state capture))) targets)
      (fail! :capture-state-mismatch
             "Capture initial state contains uninspected targets." {})))
  capture)

(defn- require-statuses! [statuses n]
  (when-not (and (vector? statuses)
                 (= n (count statuses))
                 (every? #{:committed :rejected} statuses))
    (fail! :invalid-statuses
           "Supply one definitive physical :committed or :rejected result per prepared transaction. Unknown outcomes and Live settlements cannot be checked."
           {:statuses statuses :expected n}))
  statuses)

(defn complete-observation!
  "Read the FINAL XTDB documents after real application executions have
   settled and check them against a finite serial reference model.

     (complete-observation! capture [:committed :rejected])

   Statuses are indexed by :incorporations, NOT execution/settlement order.
   They MUST come from an external trusted observation of each physical XTDB
   submission, not inferred from a Live settlement. This function does not
   call the application, submit a transaction, retry, or convert uncertainty
   into rejection. It reads documents and returns :evidence as either a
   :consistent witness or a :counterexample; the latter is not hidden.

   The result does not certify provenance, the actual submitted plans or
   correctness of application policy. It is for controlled observations only."
  [capture statuses]
  (let [{:keys [node incorporations inspected-targets initial-state]}
        (require-capture! capture)
        statuses (require-statuses! statuses (count incorporations))
        observations {:statuses statuses
                      :state (read-state! node inspected-targets)
                      :inspected-targets inspected-targets}
        witness (evidence/check-execution
                 {:initial-state initial-state
                  :incorporations incorporations
                  :observations observations})]
    {:collection observation-scope
     :initial-state initial-state
     :observations observations
     :evidence witness}))
