(ns gesso.assurance.reference-provider
  "Bounded comparison of independently expected execution and supplied observations."
  (:require [clojure.string :as str]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.assurance.reference-source-coverage :as coverage]
            [gesso.assurance.xtdb-envelope :as envelope]
            [xtdb.api :as xt])
  (:import (java.time Instant ZonedDateTime)))

(def profile :selected-provider-execution-comparison-v1)
(def ^:private base {:profile profile :authority :none :certificate :not-issued
                     :provider-refinement :not-established})
(def ^:private unsupported (assoc base :status :unsupported))

(defn- canonical-rectangles [rectangles]
  ;; Adjacent valid-axis partitions may differ without changing history.
  ;; Do not erase overlapping multiplicity or merge system-time origins.
  (frequencies
   (mapcat (fn [[_ rs]]
             (reduce (fn [acc r]
                       (if (and (seq acc) (= (:valid-to (peek acc)) (:valid-from r)))
                         (conj (pop acc) (assoc (peek acc) :valid-to (:valid-to r)))
                         (conj acc r)))
                     [] (sort-by :valid-from rs)))
           (group-by #(dissoc % :valid-from :valid-to) rectangles))))

(defn- rectangle? [r tables]
  (and (map? r) (= #{:table :document :valid-from :valid-to :system-from :system-to} (set (keys r)))
       (contains? tables (:table r)) (map? (:document r)) (contains? (:document r) :xt/id)
       (every? (fn [[a b]] (and (instance? Instant a)
                                (or (nil? b) (and (instance? Instant b) (.isBefore ^Instant a ^Instant b)))))
               [[(:valid-from r) (:valid-to r)] [(:system-from r) (:system-to r)]])))

(defn compare-execution
  "Compare a supplied complete bounded transaction sequence with observed
   ledger rows, per-transaction envelope checks and all-time rectangles for
   the explicitly selected tables. Observation acquisition/completeness and
   correct envelope-check invocation remain premises. No provider data is
   used to predict the outcome. This is not a live audit or a certificate."
  [transactions tables {:keys [ledger envelopes rectangles] :as observed}]
  (let [prediction (temporal/replay transactions)
        history (:history prediction)]
    (if-not (and (= :predicted (:status prediction))
                 (set? tables) (seq tables) (every? simple-keyword? tables)
                 (every? tables (map :table (:rectangles prediction)))
                 (every? tables (for [tx transactions op (:ops tx) :when (= :put-docs (first op))]
                                  (if (map? (second op)) (:into (second op)) (second op))))
                 (map? observed) (= #{:ledger :envelopes :rectangles} (set (keys observed)))
                 (vector? ledger) (<= (count ledger) 256)
                 (every? #(and (map? %) (= #{:tx-id :system-time :committed?} (set (keys %)))
                               (integer? (:tx-id %)) (<= 0 (:tx-id %))
                               (instance? Instant (:system-time %)) (boolean? (:committed? %))) ledger)
                 (every? (fn [[a b]] (< (:tx-id a) (:tx-id b))) (partition 2 1 ledger))
                 (vector? envelopes) (= (count ledger) (count envelopes))
                 (every? #(and (map? %) (= envelope/profile (:profile %))
                               (#{:matched :mismatch :unsupported :unavailable} (:status %))) envelopes)
                 (vector? rectangles) (<= (count rectangles) 65536)
                 (every? #(rectangle? % tables) rectangles))
      unsupported
      (let [ledger-match? (= (mapv #(select-keys % [:system-time :committed?]) history)
                             (mapv #(dissoc % :tx-id) ledger))
            envelopes-match? (every? (fn [[row e]]
                                       (and (= :matched (:status e)) (= (:tx-id row) (:tx-id e))
                                            (= (:committed? row) (:committed? e))))
                                     (map vector ledger envelopes))
            history-match? (= (canonical-rectangles (:rectangles prediction))
                              (canonical-rectangles rectangles))
            checks {:ledger ledger-match? :retained-envelopes envelopes-match? :selected-history history-match?}
            status (cond (or (not ledger-match?) (not history-match?) (some #(= :mismatch (:status %)) envelopes)) :mismatch
                         (some #(= :unsupported (:status %)) envelopes) :unsupported
                         (some #(= :unavailable (:status %)) envelopes) :unavailable
                         envelopes-match? :matched
                         :else :mismatch)]
        (assoc base :status status :checks checks :transaction-count (count history))))))

(def acquisition-profile :selected-quiescent-provider-observation-v1)

(defn- acquisition-result [status reason]
  (assoc base :acquisition-profile acquisition-profile :status status :reason reason))

(defn- observation-reject! [reason]
  (throw (ex-info "Unsupported provider observation" {::observation-rejected true :reason reason})))

(defn- table-name? [table]
  (and (simple-keyword? table) (re-matches #"[a-z][a-z0-9_]*" (name table))
       (not (str/starts-with? (name table) "xt_"))))

(defn- observed-instant! [v]
  (cond (instance? ZonedDateTime v) (.toInstant ^ZonedDateTime v)
        (instance? Instant v) v
        :else (observation-reject! :invalid-observed-timestamp)))

(defn- read-ledger! [node]
  (let [rows (xt/q node ["SELECT _id, committed, system_time FROM xt.txs ORDER BY _id LIMIT 257"])]
    (when (> (count rows) 256) (observation-reject! :ledger-limit))
    (let [ledger (mapv (fn [{:keys [xt/id committed system-time] :as row}]
                         (when-not (and (= #{:xt/id :committed :system-time} (set (keys row)))
                                        (integer? id) (<= 0 id) (boolean? committed))
                           (observation-reject! :invalid-ledger-row))
                         {:tx-id id :committed? committed :system-time (observed-instant! system-time)}) rows)]
      (when-not (every? (fn [[a b]] (< (:tx-id a) (:tx-id b))) (partition 2 1 ledger))
        (observation-reject! :invalid-ledger-order))
      ledger)))

(def ^:private worker-assertions
  #{"ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ?)"
    "ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ? AND CURRENT_TIMESTAMP >= ?)"
    "ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ? AND CURRENT_TIMESTAMP < ?)"
    "ASSERT CURRENT_TIMESTAMP < ?" "ASSERT CURRENT_TIMESTAMP + INTERVAL '5' MINUTE >= ?"})

(defn- read-envelope! [node row tx entry]
  ((if (some #(contains? worker-assertions (first %)) (:ops tx))
     envelope/check-worker-attempt-envelope!
     envelope/check-envelope!)
   node (:tx-id row) (:ops tx) (:committed? entry)))

(defn- read-rectangles! [node tables]
  (reduce (fn [acc table]
            (let [remaining (- 65536 (count acc))
                  rows (xt/q node [(str "SELECT *, _valid_from, _valid_to, _system_from, _system_to FROM \"public\".\""
                                        (name table) "\" FOR VALID_TIME ALL FOR SYSTEM_TIME ALL LIMIT " (inc remaining))])]
              (when (> (count rows) remaining) (observation-reject! :rectangle-limit))
              (into acc (map (fn [row]
                               (let [end (fn [k] (when-some [v (get row k)] (observed-instant! v)))
                                     r {:table table :document (dissoc row :xt/valid-from :xt/valid-to :xt/system-from :xt/system-to)
                                        :valid-from (observed-instant! (:xt/valid-from row)) :valid-to (end :xt/valid-to)
                                        :system-from (observed-instant! (:xt/system-from row)) :system-to (end :xt/system-to)}]
                                 (when-not (rectangle? r tables) (observation-reject! :invalid-history-row))
                                 r))) rows)))
          [] (sort tables)))

(defn- compare-quiescent*
  "Acquire ledger, retained envelopes and all-time selected table history
   directly from an embedded node. Caller must supply a quiescent node with
   a complete history from empty state, no outstanding transactions and
   the expected transaction sequence. Re-read the bounded ledger to detect
   visible changes, not to establish quiescence or an atomic snapshot.
   Select the restricted worker envelope checker from expected operations;
   unsupported envelope forms stay unsupported. Limits bound returned rows,
   not provider query resource use. No raw evidence or authority is returned."
  [node transactions tables expected-ids]
  (let [prediction (temporal/replay transactions)
        history (:history prediction)]
    (if-not (and (= :predicted (:status prediction))
                 (set? tables) (<= 1 (count tables) 128) (every? table-name? tables)
                 (every? tables (for [tx transactions op (:ops tx) :when (= :put-docs (first op))]
                                  (if (map? (second op)) (:into (second op)) (second op)))))
      (acquisition-result :unsupported :unsupported-expectation)
      (try
        (let [ledger (read-ledger! node)]
          (cond
            (and expected-ids (not= expected-ids (mapv :tx-id ledger)))
            (acquisition-result :mismatch :source-identity-sequence-mismatch)
            (not= (mapv #(select-keys % [:system-time :committed?]) history)
                  (mapv #(dissoc % :tx-id) ledger))
            (acquisition-result :mismatch :ledger-sequence-mismatch)
            :else
            (let [envelopes (mapv #(read-envelope! node %1 %2 %3) ledger transactions history)
                  rectangles (read-rectangles! node tables)
                  final-ledger (read-ledger! node)]
              (if (not= ledger final-ledger)
                (acquisition-result :unavailable :ledger-changed-during-observation)
                (assoc (compare-execution transactions tables {:ledger ledger :envelopes envelopes :rectangles rectangles})
                       :acquisition-profile acquisition-profile)))))
        (catch InterruptedException e
          (.interrupt (Thread/currentThread))
          (throw e))
        (catch clojure.lang.ExceptionInfo e
          (if (::observation-rejected (ex-data e))
            (acquisition-result :unsupported (:reason (ex-data e)))
            (acquisition-result :unavailable :observation-failed)))
        (catch Exception _ (acquisition-result :unavailable :observation-failed))))))

(defn compare-quiescent-execution!
  "Compare a selected quiescent history using positional ledger correlation.
   This diagnostic does not bind caller-supplied source transaction IDs."
  [node transactions tables]
  (compare-quiescent* node transactions tables nil))

(def source-binding-profile :selected-source-bound-execution-v1)

(defn- source-expectations? [expectations]
  (and (vector? expectations) (nil? (meta expectations))
                 (<= 1 (count expectations) 256)
                 (every? #(and (map? %) (nil? (meta %))
                               (= #{:tx-id :system-time :ops} (set (keys %)))
                               (integer? (:tx-id %)) (<= 0 (:tx-id %) (dec Long/MAX_VALUE))) expectations)
                 (every? (fn [[a b]] (< (:tx-id a) (:tx-id b))) (partition 2 1 expectations))))

(defn compare-source-bound-execution!
  "Compare a complete selected quiescent history from empty state against
   explicit ordered source transaction identities. Each expectation is
   {:tx-id nonnegative-long :system-time Instant :ops vector}. IDs are
   supplied independently of the ledger being compared, e.g. captured at
   submission. Never substitute discovered ledger IDs for these inputs.
   Identity binding is not authenticated submission provenance, a complete
   source log traversal, a live frontier or a refinement certificate."
  [node expectations tables]
  (let [bound #(assoc % :source-binding-profile source-binding-profile)]
    (if-not (source-expectations? expectations)
      (bound (acquisition-result :unsupported :unsupported-source-expectation))
      (bound (compare-quiescent* node (mapv #(dissoc % :tx-id) expectations) tables
                                 (mapv :tx-id expectations))))))

(defn- compare-source-coverage* [node expectations tables observe! tag]
  (let [fail #(tag (acquisition-result %1 %2))]
    (if-not (source-expectations? expectations)
      (fail :unsupported :unsupported-source-expectation)
      (let [before (observe! node)]
        (cond
          (not= :observed (:status before)) (fail (:status before) (:reason before))
          (not= (mapv :tx-id expectations) (:transaction-ids before))
          (fail :mismatch :source-transaction-coverage-mismatch)
          :else
          (let [comparison (compare-source-bound-execution! node expectations tables)
                after (observe! node)]
            (cond
              (not= :observed (:status after)) (fail (:status after) (:reason after))
              (not= before after) (fail :unavailable :source-log-changed-during-comparison)
              :else (tag (assoc comparison :source-coverage :matched)))))))))

(defn compare-source-covered-execution!
  "Require bounded source-log transaction coverage as well as the existing
   source-bound quiescent execution comparison. Maintenance records do not
   consume expected transaction slots. Reobserve coverage afterward; no
   processed frontier, authentication or refinement authority is implied."
  [node expectations tables]
  (compare-source-coverage* node expectations tables coverage/observe-coverage!
                            #(assoc % :source-coverage-profile coverage/profile)))

(defn compare-processed-source-execution!
  "Additionally require the captured submitted end to have been processed,
   before and after the quiescent execution comparison. No wait, live
   snapshot, authenticated source provenance or refinement authority."
  [node expectations tables]
  (let [r (compare-source-coverage* node expectations tables coverage/observe-processed-coverage!
                                    #(assoc % :source-coverage-profile coverage/profile
                                              :processed-profile coverage/processed-profile))]
    (cond-> r (= :matched (:source-coverage r)) (assoc :processed-frontier :matched))))
