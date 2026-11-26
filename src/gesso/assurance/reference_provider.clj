(ns gesso.assurance.reference-provider
  "Bounded comparison of independently expected execution and supplied observations."
  (:require [clojure.string :as str]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.assurance.proof.mapped-prestate-lowering :as lowering]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [gesso.assurance.reference-source-coverage :as coverage]
            [gesso.assurance.reference-operation-correspondence :as operation]
            [gesso.assurance.reference-replay-input :as replay-input]
            [gesso.assurance.xtdb-envelope :as envelope]
            [xtdb.api :as xt])
  (:import (java.time Instant ZonedDateTime)))

(def profile :selected-provider-execution-comparison-v4)
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

(defn- temporal-put? [tx]
  (some #(and (= :put-docs (first %)) (map? (second %))) (:ops tx)))

(defn- expected-envelope-profile [tx]
  (if (temporal-put? tx) envelope/temporal-profile envelope/profile))

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
                 (let [required (temporal/required-observation-tables transactions)]
                   (and (set? required) (every? tables required)))
                 (map? observed) (= #{:ledger :envelopes :rectangles} (set (keys observed)))
                 (vector? ledger) (<= (count ledger) 256)
                 (every? #(and (map? %) (= #{:tx-id :system-time :committed?} (set (keys %)))
                               (integer? (:tx-id %)) (<= 0 (:tx-id %))
                               (instance? Instant (:system-time %)) (boolean? (:committed? %))) ledger)
                 (every? (fn [[a b]] (< (:tx-id a) (:tx-id b))) (partition 2 1 ledger))
                 (vector? envelopes) (= (count ledger) (count envelopes))
                 (= (count transactions) (count envelopes))
                 (every? (fn [[tx e]]
                           (and (map? e) (= (expected-envelope-profile tx) (:profile e))
                                (#{:matched :mismatch :unsupported :unavailable} (:status e))))
                         (map vector transactions envelopes))
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

(def acquisition-profile :selected-quiescent-provider-observation-v5)

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
  (let [rows (xt/q node ["SELECT _id, committed, system_time FROM xt.txs FOR VALID_TIME ALL FOR SYSTEM_TIME ALL ORDER BY _id LIMIT 257"])]
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
  (let [kind (cond
               (some #(contains? worker-assertions (first %)) (:ops tx)) :worker
               (temporal-put? tx) :temporal
               :else :ordinary)
        r (envelope/check-diagnostic-envelope! node (:tx-id row) (:ops tx) (:committed? entry) kind)]
    (if (and (= :matched (:status r))
             (not= envelope/diagnostic-outcome-profile (:outcome-observation-profile r)))
      {:status :unsupported :reason :unqualified-outcome-observation
       :profile (expected-envelope-profile tx)}
      r)))

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
  "Acquire all-time ledger, retained envelopes and all-time selected table history
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
                 (let [required (temporal/required-observation-tables transactions)]
                   (and (set? required) (every? tables required))))
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

(def source-binding-profile :selected-source-bound-execution-v5)

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

(defn compare-local-source-covered-execution!
  "Require independently framed retained coverage for the installed local
   persistent source log, before and after source-bound comparison. No
   processed frontier or cross-process retention guarantee is implied."
  [node expectations tables]
  (compare-source-coverage* node expectations tables coverage/observe-local-coverage!
                            #(assoc % :source-coverage-profile coverage/local-profile)))

(defn compare-local-processed-source-execution!
  "Require retained local byte-frame coverage and stable processed source
   coordinates before and after source-bound execution comparison. No wait,
   atomic snapshot, cross-process retention or refinement authority."
  [node expectations tables]
  (let [r (compare-source-coverage* node expectations tables coverage/observe-local-processed-coverage!
                                    #(assoc % :source-coverage-profile coverage/local-profile
                                              :processed-profile coverage/local-processed-profile))]
    (cond-> r (= :matched (:source-coverage r)) (assoc :processed-frontier :matched))))

(defn- valid-payload-binding? [sample]
  (and (= coverage/local-profile (:profile sample))
       (= coverage/local-payload-profile (:payload-profile sample))
       (= coverage/payload-binding-profile (:payload-binding-profile sample))
       (= :matched (:payload-correspondence sample))
       (string? (:payload-fingerprint sample))
       (re-matches #"[0-9a-f]{64}" (:payload-fingerprint sample))))

(defn- observe-bound-payload! [node]
  (let [sample (coverage/observe-local-payload-bound-coverage! node)]
    (if (and (= :observed (:status sample)) (not (valid-payload-binding? sample)))
      {:status :unavailable :reason :invalid-source-payload-binding}
      sample)))

(defn compare-local-payload-source-execution!
  "Require independent selected envelope correspondence and equal framed
   content fingerprints before and after source-bound execution comparison.
   No fingerprint or raw payload escapes in the comparison result. This is
   bounded diagnostic stability, not atomic observation, authenticated
   provenance, a processed frontier or universal provider refinement."
  [node expectations tables]
  (let [r (compare-source-coverage* node expectations tables observe-bound-payload!
                                    #(assoc % :source-coverage-profile coverage/local-profile
                                              :payload-profile coverage/local-payload-profile
                                              :payload-binding-profile coverage/payload-binding-profile))]
    (cond-> r (= :matched (:source-coverage r)) (assoc :payload-correspondence :matched))))

(defn- observe-processed-bound-payload! [node]
  (let [sample (coverage/observe-local-processed-payload-bound-coverage! node)]
    (if (and (= :observed (:status sample))
             (not (and (valid-payload-binding? sample)
                       (= coverage/local-processed-profile (:processed-profile sample))
                       (= coverage/local-processed-payload-profile (:processed-payload-profile sample))
                       (integer? (:processed-message-id sample))
                       (<= -1 (:processed-message-id sample))
                       (vector? (:record-offsets sample))
                       (= (:processed-message-id sample) (:last-message-id sample)
                          (or (peek (:record-offsets sample)) -1)))))
      {:status :unavailable :reason :invalid-processed-source-payload-binding}
      sample)))

(defn compare-local-processed-payload-source-execution!
  "Require selected independent payload correspondence, stable content and
   fully processed source frontiers around source-bound execution. No raw
   payload or fingerprint in the report; no wait, atomic acquisition,
   authenticated provenance, independent Arrow semantics or refinement."
  [node expectations tables]
  (let [r (compare-source-coverage* node expectations tables observe-processed-bound-payload!
                                    #(assoc % :source-coverage-profile coverage/local-profile
                                              :processed-profile coverage/local-processed-profile
                                              :payload-profile coverage/local-payload-profile
                                              :payload-binding-profile coverage/payload-binding-profile
                                              :processed-payload-profile coverage/local-processed-payload-profile))]
    (cond-> r (= :matched (:source-coverage r))
      (assoc :payload-correspondence :matched :processed-frontier :matched))))

(def retained-execution-profile :selected-processed-retained-execution-v5)

(defn- valid-bound-history? [sample]
  (let [ids (:transaction-ids sample)
        end (:processed-message-id sample)]
    (and (valid-payload-binding? sample)
         (= coverage/local-processed-profile (:processed-profile sample))
         (= coverage/retained-history-profile (:retained-history-profile sample))
         (= coverage/local-processed-retained-history-profile (:processed-retained-history-profile sample))
         (= coverage/processed-retained-history-binding-profile (:processed-retained-history-binding-profile sample))
         (= operation/profile (:operation-profile sample))
         (= :none (:authority sample)) (= :not-issued (:certificate sample))
         (vector? ids) (= ids (:operation-message-ids sample))
         (= (if (seq ids) :matched :not-applicable) (:operation-correspondence sample))
         (integer? end) (<= -1 end)
         (vector? (:record-offsets sample))
         (= end (:last-message-id sample) (or (peek (:record-offsets sample)) -1))
         (= (count ids) (count (distinct ids)))
         (= ids (filterv (set ids) (:record-offsets sample)))
         (every? #(and (integer? %) (<= 0 % end)) ids))))

(defn- observe-bound-history! [node]
  (let [sample (coverage/observe-local-processed-retained-history-bound! node)]
    (if (and (= :observed (:status sample)) (not (valid-bound-history? sample)))
      {:status :unavailable :reason :invalid-processed-retained-history-binding}
      sample)))

(defn compare-local-processed-retained-execution!
  "Compare independent temporal replay with the provider ledger, expected
   operation envelopes and selected rectangles, bracketed by identical
   qualified whole-history operation and payload samples. Expectations
   must be independently supplied from empty state. No atomic snapshot,
   authenticated provenance, or universal refinement claim."
  [node expectations tables]
  (let [r (compare-source-coverage* node expectations tables observe-bound-history!
                                    #(assoc % :retained-execution-profile retained-execution-profile))]
    (if (and (= :matched (:status r)) (= :matched (:source-coverage r))
             (= profile (:profile r)) (= acquisition-profile (:acquisition-profile r))
             (= source-binding-profile (:source-binding-profile r))
             (= {:ledger true :retained-envelopes true :selected-history true} (:checks r))
             (= (count expectations) (:transaction-count r))
             (= :none (:authority r)) (= :not-issued (:certificate r))
             (= :not-established (:provider-refinement r)))
      (assoc r :operation-correspondence :matched :payload-correspondence :matched
             :processed-frontier :matched :outcome-correspondence :matched
             :operation-profile operation/profile
             :processed-retained-history-binding-profile coverage/processed-retained-history-binding-profile)
      (if (= :matched (:status r))
        (assoc (acquisition-result :unsupported :invalid-retained-execution-comparison)
               :retained-execution-profile retained-execution-profile)
        (dissoc r :source-coverage)))))

(def retained-replay-execution-profile :selected-processed-retained-replay-execution-v17)

(defn- valid-replay-history? [sample]
  (let [inputs (:replay-inputs sample)]
    (and (valid-bound-history? sample)
         (= coverage/processed-retained-replay-inputs-profile (:processed-retained-replay-inputs-profile sample))
         (= replay-input/profile (:replay-input-profile sample))
         (source-expectations? inputs)
         (= (:transaction-ids sample) (mapv :tx-id inputs))
         (every? #(and (instance? Instant (:system-time %)) (vector? (:ops %))
                       (<= 1 (count (:ops %)) 256)) inputs)
         (every? (fn [[a b]] (.isBefore ^Instant (:system-time a) ^Instant (:system-time b)))
                 (partition 2 1 inputs)))))

(defn- observe-replay-history! [node]
  (let [sample (coverage/observe-local-processed-retained-replay-inputs! node)]
    (cond
      (not= :observed (:status sample)) sample
      (and (valid-bound-history? sample) (= [] (:transaction-ids sample) (:replay-inputs sample))
           (= coverage/processed-retained-replay-inputs-profile (:processed-retained-replay-inputs-profile sample))
           (= replay-input/profile (:replay-input-profile sample)))
      {:status :unsupported :reason :empty-retained-replay-history}
      (not (valid-replay-history? sample))
      {:status :unavailable :reason :invalid-processed-retained-replay-binding}
      :else sample)))

(defn- compare-retained-replay*
  [node tables expectation prestate-check]
  (let [tag #(assoc % :retained-replay-execution-profile retained-replay-execution-profile)
        fail (fn [status reason]
               (tag (acquisition-result (if (#{:unsupported :unavailable :mismatch} status) status :unavailable)
                                        (if (keyword? reason) reason :retained-replay-comparison-failed))))
        before (observe-replay-history! node)]
    (if-not (= :observed (:status before))
      (fail (:status before) (:reason before))
      (let [inputs (:replay-inputs before)
            comparison (compare-source-bound-execution! node inputs tables)
            after (observe-replay-history! node)]
        (cond
          (not= :observed (:status after)) (fail (:status after) (:reason after))
          (not= before after) (fail :unavailable :source-log-changed-during-comparison)
          (and expectation (not= expectation (some #(when (= (:tx-id expectation) (:tx-id %)) %) inputs)))
          (fail :mismatch :selected-retained-transaction-mismatch)
          (not= :matched (:status comparison)) (fail (:status comparison) (:reason comparison))
          (not (and (= profile (:profile comparison)) (= acquisition-profile (:acquisition-profile comparison))
                    (= source-binding-profile (:source-binding-profile comparison))
                    (= {:ledger true :retained-envelopes true :selected-history true} (:checks comparison))
                    (= (count inputs) (:transaction-count comparison))
                    (= :none (:authority comparison)) (= :not-issued (:certificate comparison))
                    (= :not-established (:provider-refinement comparison))))
          (fail :unsupported :invalid-retained-replay-execution-comparison)
          :else
          (if-let [failure (when prestate-check (prestate-check inputs))]
            (fail (:status failure) (:reason failure))
            (tag (assoc base :status :matched :acquisition-profile acquisition-profile
                      :source-binding-profile source-binding-profile
                      :checks (:checks comparison) :transaction-count (count inputs)
                      :source-coverage :matched :operation-correspondence :matched
                      :payload-correspondence :matched :processed-frontier :matched
                      :outcome-correspondence :matched :replay-input-profile replay-input/profile
                      :operation-profile operation/profile
                      :processed-retained-replay-inputs-profile coverage/processed-retained-replay-inputs-profile))))))))

(defn compare-local-processed-retained-replay-execution!
  "Derive all selected replay transactions from qualified retained bytes,
   predict independently, and compare provider ledger, envelopes and selected
   all-time rectangles. Require identical qualified samples before and after.
   The caller selects tables, not transactions; empty replay is unsupported.
   No raw samples, atomic acquisition, authentication or refinement claim."
  [node tables]
  (compare-retained-replay* node tables nil nil))

(def mapped-retained-execution-profile :selected-mapped-retained-execution-v1)

(def mapped-retained-prestate-profile :selected-mapped-retained-prestate-v1)

(defn- historical-prestate-failure [inputs expectation groups]
  ;; Use the predecessor system snapshot but the selected transaction's
  ;; valid instant. Neither its own writes nor a later commit may supply
  ;; the prepared rows. The empty predecessor is the initial empty state.
  (let [prior (vec (take-while #(not= (:tx-id expectation) (:tx-id %)) inputs))
        prediction (when (seq prior) (temporal/replay (mapv #(dissoc % :tx-id) prior)))
        visible (if (empty? prior) {}
                    (temporal/visible-at prediction (:system-time (peek prior)) (:system-time expectation)))]
    (if-not (map? visible)
      {:status :unsupported :reason :unsupported-retained-prestate}
      (try
        (let [slots (when (some #(= :journal-slot (first (:target %))) groups)
                      (group-by (fn [row]
                                  (let [binding (:gesso.command/binding (resolution/require-record row))]
                                    [:journal-slot (identity/raw-value (:principal binding)) (:operation binding)]))
                                (vals (get visible :gesso_commands))))
              matches? (fn [{[kind a b] :target [mode row] :predicate}]
                         (let [actual (if (= :journal-slot kind)
                                        (get slots [kind a b] [])
                                        (if-let [doc (get-in visible [a b])] [doc] []))]
                           (case mode
                             :absent (empty? actual)
                             :exact-row (= [row] actual)
                             false)))]
          (when-not (every? matches? groups)
            {:status :mismatch :reason :selected-retained-prestate-mismatch}))
        (catch clojure.lang.ExceptionInfo _
          {:status :unsupported :reason :unsupported-retained-prestate})))))

(defn- compare-mapped-retained*
  [node tables original target-map docs journal expectation prefix-count prestate?]
  (let [tag #(cond-> (assoc % :mapped-retained-execution-profile mapped-retained-execution-profile)
               prestate? (assoc :mapped-retained-prestate-profile mapped-retained-prestate-profile))
        invalid #(tag (acquisition-result :unsupported :invalid-mapped-retained-prefix))
        checked (when (and (source-expectations? [expectation])
                           (instance? Instant (:system-time expectation))
                           (vector? (:ops expectation)) (nil? (meta (:ops expectation)))
                           (integer? prefix-count) (<= 1 prefix-count (count (:ops expectation)) 256))
                  (try
                    (lowering/check-prefix! original target-map docs journal
                                            (subvec (:ops expectation) 0 prefix-count))
                    (catch clojure.lang.ExceptionInfo _ nil)))]
    (if-not (and (= :checked-exact-prefix (:status checked))
                 (= :gesso.assurance.proof.mapped-prestate-lowering/mapped-prestate-lowering (:type checked))
                 (= 5 (:version checked)) (= prefix-count (:assertion-count checked))
                 (= :source-relative-syntactic-prefix (:scope checked))
                 (= :not-established (:sql-refinement checked))
                 (= :none (:authority checked)) (= :not-issued (:certificate checked)))
      (invalid)
      (let [report (tag (compare-retained-replay* node tables expectation
                                                 (when prestate? #(historical-prestate-failure % expectation (:groups checked)))))]
        (if (= :matched (:status report))
          (cond-> (assoc report :source-lowering :checked-exact-prefix
                         :selected-transaction-binding :matched
                         :selected-transaction-commitment :not-asserted
                         :mapped-prestate-lowering-version 5)
            prestate? (assoc :prepared-prestate :matched-retained-predecessor))
          report)))))

(defn compare-local-processed-retained-mapped-execution!
  "Recheck the source-relative exact prefix of a caller-supplied complete
   transaction expectation, then bind that expectation to one retained
   transaction in the SAME stable, independently replayed history. Prefix
   begins at operation zero; no intervening operations are skipped. The
   caller supplies the original construction, prepared rows and independent
   transaction identity/time/operations. A match can describe an aborted
   transaction. No acquisition faithfulness, suffix lowering, successful
   guard execution, authenticated provenance or refinement is established.
   Return only diagnostic status and profile metadata, not sensitive inputs."
  [node tables original target-map docs journal expectation prefix-count]
  (compare-mapped-retained* node tables original target-map docs journal expectation prefix-count false))

(defn compare-local-processed-retained-mapped-prestate!
  "Additionally compare every complete prepared target row or absence to
   the selected transaction's independently reconstructed predecessor at
   its transaction valid instant, within the same stable retained sample.
   Unrelated visible rows are not prepared targets. Malformed visible
   journal records fail closed. Does not authenticate acquisition, prove
   commitment, qualify suffix lowering or establish universal refinement."
  [node tables original target-map docs journal expectation prefix-count]
  (compare-mapped-retained* node tables original target-map docs journal expectation prefix-count true))

(def retained-clocked-execution-profile :selected-processed-retained-clocked-execution-v2)

(defn- observe-clocked-history! [node]
  (let [sample (coverage/observe-local-processed-retained-clocked-inputs! node)
        inputs (:clocked-replay-inputs sample)]
    (cond
      (not= :observed (:status sample)) sample
      (not (and (valid-bound-history? sample)
                (= coverage/processed-retained-clocked-inputs-profile (:processed-retained-clocked-inputs-profile sample))
                (= replay-input/clocked-input-profile (:clocked-replay-input-profile sample))
                (vector? inputs) (<= (count inputs) 256)
                (= (:transaction-ids sample) (mapv #(get-in % [:transaction :tx-id]) inputs))))
      {:status :unavailable :reason :invalid-processed-retained-clocked-binding}
      (empty? inputs) {:status :unsupported :reason :empty-retained-clocked-history}
      :else sample)))

(defn- predicted-clocked-outcomes [history]
  (mapv (fn [entry]
          (cond
            (:committed? entry) {:committed? true}
            (= :invalid-system-time (:rejection entry))
            {:committed? false :error-code :invalid-system-time}
            :else {:committed? false :error-code :xtdb/assert-failed
                   :failed-assertion-index (:failed-assertion-index entry)})) history))

(defn- read-clocked-outcomes! [node ids]
  (mapv (fn [id]
          (let [rows (xt/q node ["SELECT committed, error FROM xt.txs FOR VALID_TIME ALL FOR SYSTEM_TIME ALL WHERE _id = ? LIMIT 2" id])
                {:keys [committed error]} (first rows)
                data (ex-data error)]
            (when-not (and (= 1 (count rows)) (boolean? committed)
                           (if committed (nil? error) (keyword? (:xtdb.error/code data))))
              (observation-reject! :invalid-clocked-outcome-row))
            (cond-> {:committed? committed}
              (not committed) (assoc :error-code (:xtdb.error/code data))
              (= :xtdb/assert-failed (:xtdb.error/code data))
              (assoc :failed-assertion-index (:tx-op-idx data))))) ids))

(defn- compare-clocked-observations! [node inputs tables]
  (let [prediction (temporal/replay-clocked inputs)
        history (:history prediction)
        transactions (mapv :transaction inputs)
        ids (mapv :tx-id transactions)]
    (if-not (and (= :predicted (:status prediction)) (= temporal/clocked-profile (:profile prediction))
                 (= :none (:authority prediction)) (= :not-issued (:certificate prediction))
                 (= ids (mapv :tx-id history))
                 (set? tables) (<= 1 (count tables) 128) (every? table-name? tables)
                 (every? tables (map :table (:rectangles prediction)))
                 (let [required (temporal/required-observation-tables transactions)]
                   (and (set? required) (every? tables required))))
      (acquisition-result :unsupported :unsupported-clocked-expectation)
      (try
        (let [ledger (read-ledger! node)]
          (if-not (= ledger (mapv #(select-keys % [:tx-id :system-time :committed?]) history))
            (acquisition-result :mismatch :ledger-sequence-mismatch)
            (let [outcomes (read-clocked-outcomes! node ids)
                  envelopes (mapv #(read-envelope! node %1 %2 %3) ledger transactions history)
                  rectangles (read-rectangles! node tables)
                  final-outcomes (read-clocked-outcomes! node ids)
                  final-ledger (read-ledger! node)]
              (cond
                (or (not= ledger final-ledger) (not= outcomes final-outcomes))
                (acquisition-result :unavailable :ledger-changed-during-observation)
                (not= (predicted-clocked-outcomes history) outcomes)
                (acquisition-result :mismatch :clocked-outcome-mismatch)
                (not= (canonical-rectangles (:rectangles prediction)) (canonical-rectangles rectangles))
                (acquisition-result :mismatch :selected-history-mismatch)
                (some #(= :mismatch (:status %)) envelopes)
                (acquisition-result :mismatch :retained-envelope-mismatch)
                (some #(= :unavailable (:status %)) envelopes)
                (acquisition-result :unavailable :retained-envelope-unavailable)
                (not (and (= (count transactions) (count envelopes))
                          (every? (fn [[tx row e]]
                                    (and (= :matched (:status e)) (= (expected-envelope-profile tx) (:profile e))
                                         (= envelope/diagnostic-outcome-profile (:outcome-observation-profile e))
                                         (= :none (:authority e)) (= :not-issued (:certificate e))
                                         (= (:tx-id row) (:tx-id e)) (= (:committed? row) (:committed? e))))
                                  (map vector transactions ledger envelopes))))
                (acquisition-result :unsupported :unqualified-clocked-envelope)
                :else (assoc base :status :matched :acquisition-profile acquisition-profile
                             :checks {:ledger true :retained-envelopes true :selected-history true :outcome-kind true}
                             :transaction-count (count inputs))))))
        (catch InterruptedException e
          (.interrupt (Thread/currentThread))
          (throw e))
        (catch clojure.lang.ExceptionInfo e
          (if (::observation-rejected (ex-data e))
            (acquisition-result :unsupported (:reason (ex-data e)))
            (acquisition-result :unavailable :observation-failed)))
        (catch Exception _ (acquisition-result :unavailable :observation-failed))))))

(defn compare-local-processed-retained-clocked-execution!
  "Independently predict clock-qualified selected retained history and compare
   exact ledger, rejection/abort classification, attempted operation envelopes
   and selected all-time rectangles. Bracket with identical qualified samples.
   Quiescence remains a premise; no raw samples, authentication or certificate."
  [node tables]
  (let [tag #(assoc % :retained-clocked-execution-profile retained-clocked-execution-profile)
        fail (fn [status reason]
               (tag (acquisition-result (if (#{:unsupported :unavailable :mismatch} status) status :unavailable)
                                        (if (keyword? reason) reason :clocked-execution-comparison-failed))))
        before (observe-clocked-history! node)]
    (if-not (= :observed (:status before))
      (fail (:status before) (:reason before))
      (let [inputs (:clocked-replay-inputs before)
            r (compare-clocked-observations! node inputs tables)
            after (observe-clocked-history! node)]
        (cond
          (not= :observed (:status after)) (fail (:status after) (:reason after))
          (not= before after) (fail :unavailable :source-log-changed-during-comparison)
          (not= :matched (:status r)) (fail (:status r) (:reason r))
          (not (and (= profile (:profile r)) (= acquisition-profile (:acquisition-profile r))
                    (= {:ledger true :retained-envelopes true :selected-history true :outcome-kind true} (:checks r))
                    (= (count inputs) (:transaction-count r)) (= :none (:authority r))
                    (= :not-issued (:certificate r)) (= :not-established (:provider-refinement r))))
          (fail :unsupported :invalid-clocked-execution-comparison)
          :else (tag (assoc r :source-coverage :matched :operation-correspondence :matched
                            :payload-correspondence :matched :processed-frontier :matched
                            :outcome-correspondence :matched :clock-correspondence :matched
                            :temporal-profile temporal/clocked-profile
                            :processed-retained-clocked-inputs-profile coverage/processed-retained-clocked-inputs-profile)))))))
