(ns gesso.model.resolution-observation
  "Test/diagnostic collection of *physical* XTDB execution observations for
   the finite durable-command checker in gesso.model.resolution-evidence.

   Unlike a caller-built witness input, observe-and-check! obtains its initial
   and final document maps directly from XTDB, submits the exact admitted
   prepared plans via gesso.model.tx/prepare, and captures physical outcomes
   from xtdb.api/execute-tx. The finite checker independently determines
   whether the result agrees with some serial ordering of those plans.

   This is a CONTROLLED-DATABASE facility: all target reads are separate XTDB
   queries, not one explicitly pinned XTDB snapshot. No other writer may alter
   relevant documents from the first read through the final read. Calling
   this function itself performs database writes. It is not a passive audit
   of an application's previously completed Live command and must not be used
   to resubmit production commands, to reconcile uncertain delivery, or to
   infer business authorization. A successful check is a bounded observation
   consistency result, not a cryptographic attestation or a proof of XTDB
   serializability. Unknown transaction errors always propagate."
  (:require
   [clojure.set :as set]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.resolution-evidence :as evidence]
   [gesso.model.tx :as tx]
   [gesso.model.tx-model :as model]
   [xtdb.api :as xt]))

(def input-keys #{:node :incorporations :submission-order})
(def incorporation-keys #{:plan :record :scope})

(def observation-scope
  {:collector :gesso.model.resolution-observation/direct-xtdb-v1
   :collected #{:initial-documents-read-from-xtdb
                :transaction-submission-through-model-tx-prepare
                :physical-xtdb-commit-and-assertion-rejection-outcomes
                :final-documents-read-from-xtdb
                :checked-against-finite-serial-reference}
   :assumed #{:exclusive-control-of-relevant-documents-during-collection
              :consistent-series-of-xtdb-reads
              :synchronous-execute-tx-outcome-is-definitive
              :the-supplied-node-is-the-authoritative-test-database}
   :not-established #{:authenticated-observation-provenance
                      :single-snapshot-cross-table-read
                      :production-live-execution-correspondence
                      :safe-retry-of-indeterminate-delivery
                      :authorization-or-domain-invariant-correctness
                      :unbounded-concurrency-or-linearizability
                      :formal-refinement-proof}})

(defn- fail! [kind message details]
  (throw (ex-info message
                  {:error/type ::invalid-observation-request
                   :error/kind kind
                   :error/details details})))

(defn- require-target [target]
  (when-not (and (vector? target)
                 (= 2 (count target))
                 (keyword? (first target))
                 (some? (second target)))
    (fail! :invalid-target "Expected a document target [table id]."
           {:target target}))
  target)

(defn- target-table-sql [table]
  ;; Do not interpolate arbitrary identifiers into SQL. This subset matches
  ;; the simple keyword-backed tables used by the finite XTDB model.
  (let [table-name (name table)]
    (when-not (re-matches #"[A-Za-z_][A-Za-z0-9_]*" table-name)
      (fail! :unsupported-table-name
             "Cannot safely query this table in the finite XTDB collector."
             {:table table}))
    table-name))

(defn- require-incorporation [incorporation]
  (when-not (and (map? incorporation)
                 (= incorporation-keys (set (keys incorporation)))
                 (= resolution/assurance-scope (:scope incorporation)))
    (fail! :invalid-incorporation
           "Expected exactly one canonical durable-incorporation construction."
           {:incorporation incorporation}))
  (let [plan (model/admitted-plan (:plan incorporation))
        record (resolution/require-record (:record incorporation))
        target [resolution/command-entity-type (:xt/id record)]
        record-creates (filter #(= resolution/command-entity-type
                                   (first (command/target %)))
                               (:commands plan))]
    (when-not (and (= 1 (count record-creates))
                   (command/create? (first record-creates))
                   (= target (command/target (first record-creates)))
                   (= record (command/after (first record-creates))))
      (fail! :invalid-durable-incorporation
             "A prepared incorporation must create its exact command record atomically."
             {:target target}))
    plan))

(defn- tracked-targets [plans]
  (reduce (fn [tracked {:keys [commands guards]}]
            (-> tracked
                (set/union (into #{} (map command/target) commands))
                (set/union (into #{} (map command/guard-target)
                                     (tx/effective-guards commands guards)))))
          #{}
          plans))

(defn- require-input [input]
  (when-not (and (map? input)
                 (= input-keys (set (keys input))))
    (fail! :invalid-input
           "Expected :node, :incorporations and :submission-order only."
           {:received (when (map? input) (set (keys input)))}))
  (let [{:keys [node incorporations submission-order]} input]
    (when (nil? node)
      (fail! :missing-node "An XTDB node is required." {}))
    (when-not (and (vector? incorporations)
                   (<= 1 (count incorporations) 6))
      (fail! :invalid-incorporations
             "The collector supports one to six prepared incorporations."
             {:count (when (sequential? incorporations)
                       (count incorporations))}))
    (let [indices (set (range (count incorporations)))]
      (when-not (and (vector? submission-order)
                     (= (count incorporations) (count submission-order))
                     (= indices (set submission-order)))
        (fail! :invalid-submission-order
               "Submission order must be a permutation of incorporation indices."
               {:submission-order submission-order :required-indices indices})))
    (let [plans (mapv require-incorporation incorporations)
          tracked (tracked-targets plans)]
      ;; Admission and all table-name checks happen before any submitted write.
      (doseq [target tracked]
        (require-target target)
        (target-table-sql (first target)))
      {:node node :incorporations incorporations
       :plans plans :tracked tracked :order submission-order})))

(defn- read-target [node [table id]]
  (let [sql (str "SELECT * FROM " (target-table-sql table) " WHERE _id = ?")
        rows (xt/q node [sql id])]
    (when-not (<= (count rows) 1)
      (fail! :multiple-rows "Expected at most one current document for a target."
             {:target [table id] :count (count rows)}))
    (first rows)))

(defn- inspect-state [node tracked]
  (model/require-state
   (into {}
         (keep (fn [target]
                 (when-let [document (read-target node target)]
                   [target document])))
         (sort-by pr-str tracked))))

(defn- execute-physical! [node plan]
  (try
    (xt/execute-tx node (:tx-ops (tx/prepare {} plan)))
    :committed
    (catch Throwable exception
      (if (= :xtdb/assert-failed
             (:xtdb.error/code (ex-data exception)))
        :rejected
        ;; Unknown errors (including indeterminate outcomes) are never
        ;; evidence of a rejected or committed physical transaction.
        (throw exception)))))

(defn observe-and-check!
  "Submit the given 1–6 *previously prepared* durable incorporations to the
   supplied, exclusively controlled XTDB node in :submission-order.

   Example input:
     {:node node
      :incorporations [resolution/incorporation-plan-result ...]
      :submission-order [1 0]}

   All inputs are admitted before the first write. Read the complete initial
   target state, submit via the real Gesso transaction lowering and XTDB API,
   then read all targets again and run resolution-evidence/check-execution.

   Returns {:collection ... :initial-state ... :observations ... :evidence ...}
   without certifying the identity of the database or the absence of external
   writes. If any status cannot be determined, it throws immediately; it must
   never be treated as a rejection or permission to re-submit a command.

   This serial-submission collector is a FIRST STEP toward independently
   collected evidence, not a substitute for v788/v802's concurrent tests."
  [input]
  (let [{:keys [node incorporations plans tracked order]} (require-input input)
        initial-state (inspect-state node tracked)
        statuses (reduce (fn [statuses idx]
                           (assoc statuses idx
                                  (execute-physical! node (nth plans idx))))
                         (vec (repeat (count plans) nil))
                         order)
        observations {:statuses statuses
                      :state (inspect-state node tracked)
                      :inspected-targets tracked}
        witness (evidence/check-execution
                 {:initial-state initial-state
                  :incorporations incorporations
                  :observations observations})]
    {:collection observation-scope
     :submission-order order
     :initial-state initial-state
     :observations observations
     :evidence witness}))
