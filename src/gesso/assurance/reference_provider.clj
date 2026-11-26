(ns gesso.assurance.reference-provider
  "Bounded comparison of independently expected execution and supplied observations."
  (:require [clojure.string :as str]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.assurance.reference-mapped-poststate :as poststate]
            [gesso.assurance.reference-recovery-trajectory :as trajectory]
            [gesso.model.certified-plan :as plan]
            [gesso.assurance.proof.mapped-prestate-lowering :as lowering]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [gesso.assurance.reference-source-coverage :as coverage]
            [gesso.assurance.reference-retained-acquisition :as acquisition]
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

(def retained-replay-execution-profile :selected-processed-retained-replay-execution-v22)

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

;; Only binds an additional equality constraint during synchronous composition.
;; It cannot substitute a supplied sample for a real provider observation.
(def ^:dynamic ^:private *required-retained-sample* nil)

(def ^:private original-replay-observer observe-replay-history!)
(def ^:private original-acquisition-reader acquisition/read-acquisition!)
(def ^:private original-retained-reader coverage/observe-local-processed-retained-replay-inputs!)

(defn- acquisition-readers-current? []
  (and (identical? original-replay-observer observe-replay-history!)
       (identical? original-acquisition-reader acquisition/read-acquisition!)
       (identical? original-retained-reader coverage/observe-local-processed-retained-replay-inputs!)))

(defn compare-with-issued-retained-acquisition!
  "Constrain trusted synchronous diagnostic code to the complete locally
   issued sample, not a caller-supplied map. Still acquire fresh observations;
   nested recovery/root checks must match the entire issued sample. This is
   content equality under a local origin binding, not durable store identity.
   The callback is trusted code; this helper issues no positive assurance tag."
  [node handle compare!]
  (let [fail #(acquisition-result %1 %2)]
    (if-not (acquisition-readers-current?)
      (fail :unsupported :retained-acquisition-reader-replaced)
      (if-let [sample (original-acquisition-reader node handle)]
        (if-not (and (valid-replay-history? sample)
                     (or (nil? *required-retained-sample*) (= sample *required-retained-sample*)))
          (fail :unsupported :invalid-issued-retained-sample)
          (let [before (original-replay-observer node)]
            (if-not (= sample before)
              (fail :unavailable :issued-retained-sample-changed)
              (let [result (binding [*required-retained-sample* sample] (compare!))
                    after (original-replay-observer node)]
                (cond
                  (not (acquisition-readers-current?)) (fail :unsupported :retained-acquisition-reader-replaced)
                  (not= sample after) (fail :unavailable :issued-retained-sample-changed)
                  (not= :matched (:status result)) (fail (:status result) (:reason result))
                  :else result)))))
        (fail :unsupported :unissued-retained-acquisition)))))

(defn- compare-retained-replay*
  [node tables expectation prestate-check]
  (let [tag #(assoc % :retained-replay-execution-profile retained-replay-execution-profile)
        fail (fn [status reason]
               (tag (acquisition-result (if (#{:unsupported :unavailable :mismatch} status) status :unavailable)
                                        (if (keyword? reason) reason :retained-replay-comparison-failed))))
        before (observe-replay-history! node)]
    (cond
      (not= :observed (:status before)) (fail (:status before) (:reason before))
      (and *required-retained-sample* (not= *required-retained-sample* before))
      (fail :unavailable :source-log-changed-during-recovery-comparison)
      :else
      (let [inputs (:replay-inputs before)
            comparison (compare-source-bound-execution! node inputs tables)
            selected-failure (when (and prestate-check (= :matched (:status comparison))
                                        (or (nil? expectation) (= expectation (some #(when (= (:tx-id expectation) (:tx-id %)) %) inputs))))
                               (prestate-check inputs))
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
          (if-let [failure selected-failure]
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

(defn compare-local-processed-retained-transaction!
  "Bind a caller-captured exact selected transaction to the same stable
   retained history used for independent physical execution comparison.
   Capture provenance, source-derived semantics and selected commitment
   remain separate obligations."
  [node tables expectation]
  (if (and (source-expectations? [expectation])
           (instance? Instant (:system-time expectation))
           (vector? (:ops expectation)) (nil? (meta (:ops expectation)))
           (<= 1 (count (:ops expectation)) 256))
    (let [r (compare-retained-replay* node tables expectation nil)]
      (if (= :matched (:status r))
        (assoc r :selected-transaction-binding :matched
                 :selected-transaction-commitment :not-asserted)
        r))
    (acquisition-result :unsupported :unsupported-source-expectation)))

(def selected-commitment-profile :selected-retained-commitment-v1)

(defn- selected-commitment-failure [node inputs expectation]
  ;; All observations are inside compare-retained-replay*'s stable bracket.
  ;; Replay only through the selected transaction, never infer its outcome
  ;; from a later successful retry under the same command identity.
  (let [through (conj (vec (take-while #(not= (:tx-id expectation) (:tx-id %)) inputs)) expectation)
        prediction (temporal/replay (mapv #(dissoc % :tx-id) through))
        entry (peek (:history prediction))]
    (cond
      (not= :predicted (:status prediction))
      (acquisition-result :unsupported :unsupported-selected-commitment-prediction)
      (not (true? (:committed? entry)))
      (acquisition-result :mismatch :selected-retained-commitment-not-predicted)
      :else
      (try
        (let [rows (xt/q node ["SELECT committed, error FROM xt.txs FOR VALID_TIME ALL FOR SYSTEM_TIME ALL WHERE _id = ? LIMIT 2" (:tx-id expectation)])]
          (cond
            (not= 1 (count rows)) (acquisition-result :unavailable :invalid-retained-commitment-outcome)
            (not (and (true? (:committed (first rows))) (nil? (:error (first rows)))))
            (acquisition-result :mismatch :selected-retained-commitment-outcome-mismatch)))
        (catch InterruptedException e (.interrupt (Thread/currentThread)) (throw e))
        (catch Exception _ (acquisition-result :unavailable :retained-commitment-outcome-unavailable))))))

(defn- tag-commitment [report]
  (cond-> (assoc report :selected-commitment-profile selected-commitment-profile)
    (= :matched (:status report))
    (assoc :selected-transaction-binding :matched :selected-transaction-commitment :committed
           :selected-commitment-outcome :matched)))

(defn compare-local-processed-retained-committed-transaction!
  "Require successful selected physical commitment, independently predicted
   and observed in the same stable retained comparison. Does not establish
   source-relative construction or abstract transition correspondence."
  [node tables expectation]
  (tag-commitment
   (if (and (source-expectations? [expectation])
            (instance? Instant (:system-time expectation))
            (vector? (:ops expectation)) (nil? (meta (:ops expectation)))
            (<= 1 (count (:ops expectation)) 256))
     (compare-retained-replay* node tables expectation #(selected-commitment-failure node % expectation))
     (acquisition-result :unsupported :unsupported-source-expectation))))

(def mapped-retained-execution-profile :selected-mapped-retained-execution-v1)

(def mapped-retained-prestate-profile :selected-mapped-retained-prestate-v1)

(defn- historical-prestate-failure [inputs expectation groups command-id evidence? publication-ids]
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
          (cond
            (not (every? matches? groups))
            {:status :mismatch :reason :selected-retained-prestate-mismatch}
            (and command-id (not-every? string? (keys (get visible :gesso_commands))))
            {:status :unsupported :reason :unsupported-retained-command-id-domain}
            (and command-id (contains? (get visible :gesso_commands) command-id))
            {:status :mismatch :reason :selected-retained-command-id-present}
            (and evidence? (not-every? string? (keys (get visible :gesso_transition_evidence))))
            {:status :unsupported :reason :unsupported-retained-evidence-id-domain}
            (and evidence? (contains? (get visible :gesso_transition_evidence) command-id))
            {:status :mismatch :reason :selected-retained-evidence-id-present}
            (and (seq publication-ids) (not-every? uuid? (keys (get visible :gesso_live_outbox))))
            {:status :unsupported :reason :unsupported-retained-publication-id-domain}
            (some #(contains? (get visible :gesso_live_outbox) %) publication-ids)
            {:status :mismatch :reason :selected-retained-publication-id-present}))
        (catch clojure.lang.ExceptionInfo _
          {:status :unsupported :reason :unsupported-retained-prestate})))))

(defn- mapped-observation-tables [original targets]
  (try
                   (into #{} (map (fn [[kind table]] (if (= :journal-slot kind) :gesso_commands table)))
                         (vals (:target-map (plan/check-target-mapping! original targets))))
                   (catch clojure.lang.ExceptionInfo _ nil)))

(defn- abstract-projection-failure [inputs expectation original targets tables spec]
  (let [required (mapped-observation-tables original targets)]
    (if-not (and (set? tables) (some? required) (every? tables required))
      (acquisition-result :unsupported :incomplete-abstract-projection-scope)
      (let [r (poststate/compare-selected! inputs expectation original targets spec)]
        (when-not (and (= :matched (:status r)) (= poststate/profile (:profile r))
                       (= :matched-retained-projection (:abstract-transition r))
                       (= :none (:authority r)) (= :not-issued (:certificate r))
                       (= :not-established (:provider-refinement r)))
          (if (#{:unsupported :mismatch} (:status r)) r
              (acquisition-result :unsupported :invalid-abstract-projection-result)))))))

(defn- compare-mapped-retained*
  [node tables original target-map docs journal expectation prefix-count prestate? command-id evidence? publication-ids & [commit? abstract-spec]]
  (let [offset (if evidence? 1 0)
        tag #(cond-> (assoc % :mapped-retained-execution-profile mapped-retained-execution-profile)
               commit? tag-commitment
               (some? abstract-spec) (assoc :selected-abstract-transition-profile poststate/profile)
               (and (some? abstract-spec) (= :matched (:status %))) (assoc :abstract-transition :matched-retained-projection)
               prestate? (assoc :mapped-retained-prestate-profile mapped-retained-prestate-profile)
               command-id (assoc :mapped-retained-command-absence-profile :selected-mapped-retained-command-absence-v1)
               evidence? (assoc :mapped-retained-evidence-absence-profile :selected-mapped-retained-evidence-absence-v1)
               (some? publication-ids) (assoc :mapped-retained-publication-absence-profile :selected-mapped-retained-publication-absence-v1))
        invalid #(tag (acquisition-result :unsupported :invalid-mapped-retained-prefix))
        checked (when (and (source-expectations? [expectation])
                           (instance? Instant (:system-time expectation))
                           (vector? (:ops expectation)) (nil? (meta (:ops expectation)))
                           (integer? prefix-count) (<= 1 prefix-count (- (count (:ops expectation)) offset) 256))
                  (try
                    (lowering/check-prefix! original target-map docs journal
                                            (subvec (:ops expectation) offset (+ offset prefix-count)))
                    (catch clojure.lang.ExceptionInfo _ nil)))]
    (if-not (and (= :checked-exact-prefix (:status checked))
                 (= :gesso.assurance.proof.mapped-prestate-lowering/mapped-prestate-lowering (:type checked))
                 (= 5 (:version checked)) (= prefix-count (:assertion-count checked))
                 (= :source-relative-syntactic-prefix (:scope checked))
                 (= :not-established (:sql-refinement checked))
                 (= :none (:authority checked)) (= :not-issued (:certificate checked)))
      (invalid)
      (let [report (tag (compare-retained-replay* node tables expectation
                                                 (when (or prestate? commit?)
                                                   (fn [inputs]
                                                     (or (when prestate? (historical-prestate-failure inputs expectation (:groups checked) command-id evidence? publication-ids))
                                                         (when commit? (selected-commitment-failure node inputs expectation))
                                                         (when (some? abstract-spec)
                                                           (abstract-projection-failure inputs expectation original target-map tables abstract-spec)))))))]
        (if (= :matched (:status report))
          (cond-> (assoc report :source-lowering :checked-exact-prefix
                         :selected-transaction-binding :matched
                         :selected-transaction-commitment (if commit? :committed :not-asserted)
                         :mapped-prestate-lowering-version 5)
            prestate? (assoc :prepared-prestate :matched-retained-predecessor)
            command-id (assoc :global-command-id :absent-at-retained-predecessor)
            evidence? (assoc :global-evidence-id :absent-at-retained-predecessor)
            (some? publication-ids) (assoc :publication-ids :absent-at-retained-predecessor))
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
  (compare-mapped-retained* node tables original target-map docs journal expectation prefix-count false nil false nil))

(defn compare-local-processed-retained-mapped-prestate!
  "Additionally compare every complete prepared target row or absence to
   the selected transaction's independently reconstructed predecessor at
   its transaction valid instant, within the same stable retained sample.
   Unrelated visible rows are not prepared targets. Malformed visible
   journal records fail closed. Does not authenticate acquisition, prove
   commitment, qualify suffix lowering or establish universal refinement."
  [node tables original target-map docs journal expectation prefix-count]
  (compare-mapped-retained* node tables original target-map docs journal expectation prefix-count true nil false nil))

(defn compare-local-processed-retained-mapped-command-absence!
  "Additionally require string command identity absence across the entire
   visible journal at the selected prestate, not just the mapped slot. The
   caller supplies the ID; this does not check guard placement or writes."
  [node tables original target-map docs journal expectation prefix-count command-id]
  (if (and (set? tables) (contains? tables :gesso_commands)
           (string? command-id) (not (str/blank? command-id)) (<= (count command-id) 1024))
    (compare-mapped-retained* node tables original target-map docs journal expectation prefix-count true command-id false nil)
    (assoc (acquisition-result :unsupported :invalid-retained-command-id)
           :mapped-retained-command-absence-profile :selected-mapped-retained-command-absence-v1)))

(defn compare-local-processed-retained-mapped-evidence-absence!
  "Require the leading evidence-ID absence guard and check the following
   mapped prefix. Bind both global string identities to the same retained
   predecessor. This does not check the evidence body or remaining writes."
  [node tables original target-map docs journal expectation prefix-count command-id]
  (if (and (set? tables) (every? tables [:gesso_commands :gesso_transition_evidence])
           (string? command-id) (not (str/blank? command-id)) (<= (count command-id) 1024)
           (= ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_transition_evidence WHERE _id = ?)" command-id]
              (first (:ops expectation))))
    (compare-mapped-retained* node tables original target-map docs journal expectation prefix-count true command-id true nil)
    (assoc (acquisition-result :unsupported :invalid-retained-evidence-id-guard)
           :mapped-retained-evidence-absence-profile :selected-mapped-retained-evidence-absence-v1)))

(defn compare-local-processed-retained-mapped-publication-absence!
  "Check the leading evidence guard, mapped prefix and consecutive exact
   publication identity guards, and bind their absence to the same retained
   predecessor as command/evidence identities. An explicit empty ID vector
   still requires the outbox observation scope. Optional residual-ops is
   the exact preceding fragment supplied by a separate derivation checker;
   this function checks placement, not residual source correspondence.
   The final arity selects publication-first or residual-first placement.
   The next arity optionally requires selected successful commitment.
   The final arity additionally checks the abstract projection when supplied.
   No publication body check."
  ([node tables original target-map docs journal expectation prefix-count command-id publication-ids]
   (compare-local-processed-retained-mapped-publication-absence!
    node tables original target-map docs journal expectation prefix-count command-id publication-ids []))
  ([node tables original target-map docs journal expectation prefix-count command-id publication-ids residual-ops]
   (compare-local-processed-retained-mapped-publication-absence!
    node tables original target-map docs journal expectation prefix-count command-id publication-ids residual-ops :residual-first))
  ([node tables original target-map docs journal expectation prefix-count command-id publication-ids residual-ops guard-order]
   (compare-local-processed-retained-mapped-publication-absence!
    node tables original target-map docs journal expectation prefix-count command-id publication-ids residual-ops guard-order false))
  ([node tables original target-map docs journal expectation prefix-count command-id publication-ids residual-ops guard-order commit?]
   (compare-local-processed-retained-mapped-publication-absence!
    node tables original target-map docs journal expectation prefix-count command-id publication-ids residual-ops guard-order commit? nil))
  ([node tables original target-map docs journal expectation prefix-count command-id publication-ids residual-ops guard-order commit? abstract-spec]
  (let [ops (:ops expectation)]
    (if (and (boolean? commit?) (or (nil? abstract-spec) (and commit? (map? abstract-spec))) (set? tables) (every? tables [:gesso_commands :gesso_transition_evidence :gesso_live_outbox])
             (string? command-id) (not (str/blank? command-id)) (<= (count command-id) 1024)
             (vector? publication-ids) (nil? (meta publication-ids)) (<= (count publication-ids) 128)
             (every? uuid? publication-ids) (= (count publication-ids) (count (set publication-ids)))
             (vector? ops) (integer? prefix-count) (<= 1 prefix-count 256)
             (vector? residual-ops) (nil? (meta residual-ops)) (<= (count residual-ops) 256)
             (every? #(and (vector? %) (string? (first %))) residual-ops)
             (<= (+ 1 prefix-count (count residual-ops) (count publication-ids)) (count ops))
             (= ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_transition_evidence WHERE _id = ?)" command-id]
                (first ops))
             (#{:residual-first :publication-first} guard-order)
             (let [guards (mapv #(vector "ASSERT 0 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ?)" %) publication-ids)
                   expected-guards (if (= :publication-first guard-order)
                                     (into guards residual-ops) (into residual-ops guards))]
               (= expected-guards (subvec ops (inc prefix-count)
                                          (+ 1 prefix-count (count expected-guards))))))
      (compare-mapped-retained* node tables original target-map docs journal expectation prefix-count true command-id true publication-ids commit? abstract-spec)
      (assoc (acquisition-result :unsupported :invalid-retained-publication-id-guards)
             :mapped-retained-publication-absence-profile :selected-mapped-retained-publication-absence-v1)))))

(def mapped-retained-abort-profile :selected-mapped-retained-abort-v1)

(defn compare-local-processed-retained-mapped-abort!
  "Check an exact source-relative prefix against a stale retained predecessor,
   predict its assertion abort and unchanged bitemporal rectangles, and check
   the provider's recorded error kind and physical index within the same
   bracketed replay observation. Does not derive the remaining envelope."
  [node tables original target-map docs journal expectation prefix-count command-id]
  (let [tag #(assoc % :mapped-retained-abort-profile mapped-retained-abort-profile)
        fail #(acquisition-result %1 %2)
        checked (when (and (set? tables) (every? tables [:gesso_commands :gesso_transition_evidence :gesso_live_outbox])
                           (source-expectations? [expectation]) (instance? Instant (:system-time expectation))
                           (vector? (:ops expectation)) (nil? (meta (:ops expectation)))
                           (string? command-id) (not (str/blank? command-id)) (<= (count command-id) 1024)
                           (integer? prefix-count) (<= 1 prefix-count (dec (count (:ops expectation))) 256)
                           (= ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_transition_evidence WHERE _id = ?)" command-id]
                              (first (:ops expectation))))
                  (try (lowering/check-prefix! original target-map docs journal
                                                (subvec (:ops expectation) 1 (inc prefix-count)))
                       (catch clojure.lang.ExceptionInfo _ nil)))
        index (volatile! nil)]
    (if-not (and (= :checked-exact-prefix (:status checked))
                 (= :gesso.assurance.proof.mapped-prestate-lowering/mapped-prestate-lowering (:type checked))
                 (= 5 (:version checked)) (= prefix-count (:assertion-count checked))
                 (= :source-relative-syntactic-prefix (:scope checked))
                 (= :not-established (:sql-refinement checked))
                 (= :none (:authority checked)) (= :not-issued (:certificate checked)))
      (tag (fail :unsupported :invalid-mapped-retained-abort-prefix))
      (let [check! (fn [inputs]
                     (let [stale (historical-prestate-failure inputs expectation (:groups checked) nil false nil)
                           prior (vec (take-while #(not= (:tx-id expectation) (:tx-id %)) inputs))
                           replay #(temporal/replay (mapv (fn [tx] (dissoc tx :tx-id)) %))
                           before (when (seq prior) (replay prior))
                           after (replay (conj prior expectation))
                           entry (peek (:history after)) i (:failed-assertion-index entry)]
                       (cond
                         (and stale (not= :mismatch (:status stale))) stale
                         (not= :selected-retained-prestate-mismatch (:reason stale))
                         (fail :mismatch :selected-retained-prestate-not-stale)
                         (not (and (= :predicted (:status before) (:status after))
                                   (false? (:committed? entry)) (integer? i) (<= 1 i prefix-count)))
                         (fail :mismatch :selected-retained-mapped-prefix-abort-not-predicted)
                         (not (and (= (:segments (peek (:history before))) (:segments entry))
                                   (= (:rectangles before) (:rectangles after))))
                         (fail :mismatch :selected-retained-abort-effects)
                         :else
                         (try
                           (let [rows (xt/q node ["SELECT committed, error FROM xt.txs FOR VALID_TIME ALL FOR SYSTEM_TIME ALL WHERE _id = ? LIMIT 2" (:tx-id expectation)])
                                 {:keys [committed error]} (first rows) data (ex-data error)]
                             (cond
                               (not= 1 (count rows)) (fail :unavailable :invalid-retained-abort-outcome)
                               (not (and (false? committed) (= :xtdb/assert-failed (:xtdb.error/code data))
                                         (= i (:tx-op-idx data))))
                               (fail :mismatch :selected-retained-abort-error-mismatch)
                               :else (do (vreset! index i) nil)))
                           (catch InterruptedException e (.interrupt (Thread/currentThread)) (throw e))
                           (catch Exception _ (fail :unavailable :retained-abort-outcome-unavailable))))))
            report (tag (compare-retained-replay* node tables expectation check!))]
        (if (= :matched (:status report))
          (assoc report :source-lowering :checked-exact-prefix
                 :selected-transaction-binding :matched :selected-transaction-commitment :aborted
                 :prepared-prestate :stale-at-retained-predecessor
                 :mapped-prefix-failure-index @index :assertion-error :matched
                 :durable-effects :none-at-selected-abort)
          report)))))

(def mapped-recovery-profile :selected-mapped-retained-recovery-v1)

(def mapped-interleaved-recovery-profile :selected-mapped-retained-worker-interleaved-recovery-v1)

(defn- compare-mapped-recovery*
  [node tables steps expected interleaved? source-id]
  (let [tag #(assoc % :mapped-recovery-profile (if interleaved? mapped-interleaved-recovery-profile mapped-recovery-profile))
        fail #(tag (acquisition-result %1 %2))
        roles [:winner :abort :recovery]
        common #{:original :targets :docs :journal :expectation :prefix-count :command-id}
        commit-keys (into common [:publication-ids :residual-ops :abstract-spec])
        valid? (and (vector? steps) (nil? (meta steps)) (= 3 (count steps))
                    (map? expected) (nil? (meta expected)) (= (set roles) (set (keys expected)))
                    (every? (fn [[step required role]]
                              (and (map? step) (nil? (meta step)) (= required (set (keys step)))
                                   (= (:command-id step) (:xt/id (get expected role)))
                                   (or (= :abort role) (map? (:abstract-spec step)))))
                            (map vector steps [commit-keys common commit-keys] roles)))]
    (cond
      (not valid?) (fail :unsupported :invalid-mapped-recovery-source)
      (not (and (set? tables) (every? tables [:gesso_commands :gesso_transition_evidence :gesso_live_outbox])
                (every? (fn [{:keys [original targets]}]
                          (let [required (mapped-observation-tables original targets)]
                            (and (some? required) (every? tables required)))) steps)))
      (fail :unsupported :incomplete-mapped-recovery-scope)
      :else
      (let [before (observe-replay-history! node)]
        (cond
          (not= :observed (:status before)) (fail (:status before) (:reason before))
          (and *required-retained-sample* (not= *required-retained-sample* before))
          (fail :unavailable :source-log-changed-during-recovery-comparison)
          :else
          (let [selected (mapv :expectation steps)
                linkage (if interleaved?
                          (trajectory/compare-worker-interleaved! (:replay-inputs before) selected expected source-id)
                          (trajectory/compare-selected! (:replay-inputs before) selected expected))
                check-step (fn [{:keys [original targets docs journal expectation prefix-count command-id
                                        publication-ids residual-ops abstract-spec]} role]
                             (if (= :abort role)
                               (compare-local-processed-retained-mapped-abort!
                                node tables original targets docs journal expectation prefix-count command-id)
                               (compare-local-processed-retained-mapped-publication-absence!
                                node tables original targets docs journal expectation prefix-count command-id
                                publication-ids residual-ops :publication-first true abstract-spec)))
                reports (when (= :matched (:status linkage))
                          (binding [*required-retained-sample* before]
                            (loop [remaining (map vector steps roles) results []]
                              (if-let [[step role] (first remaining)]
                                (let [r (check-step step role) rs (conj results r)]
                                  (if (= :matched (:status r)) (recur (next remaining) rs) rs))
                                results))))
                after (observe-replay-history! node)
                failure (first (remove #(= :matched (:status %)) reports))]
            (cond
              (not= :observed (:status after)) (fail (:status after) (:reason after))
              (not= before after) (fail :unavailable :source-log-changed-during-recovery-comparison)
              (not= :matched (:status linkage)) (fail (:status linkage) (:reason linkage))
              failure (fail (:status failure) (:reason failure))
              (not (and (= 3 (count reports))
                        (= [:committed :aborted :committed] (mapv :selected-transaction-commitment reports))
                        (= [:matched-retained-projection nil :matched-retained-projection] (mapv :abstract-transition reports))
                        (= :none-at-selected-abort (:durable-effects (second reports)))
                        (= :matched-retained-journal-trajectory (:recovery-linkage linkage))
                        (= (if interleaved? trajectory/interleaved-profile trajectory/profile) (:profile linkage))
                        (= (mapv :tx-id selected) (:selected-tx-ids linkage))
                        (or (not interleaved?)
                            (and (vector? (:intervening-maintenance linkage))
                                 (= (count (:replay-inputs before)) (:replayed-transaction-count linkage))))))
              (fail :unsupported :invalid-mapped-recovery-comparison)
              :else (tag (cond-> (assoc base :status :matched
                                :recovery-linkage :matched-retained-journal-trajectory
                                :selected-tx-ids (:selected-tx-ids linkage)
                                :selected-commitments [:committed :aborted :committed]
                                :abstract-transitions [:matched-retained-projection :not-committed :matched-retained-projection])
                           interleaved? (assoc :intervening-maintenance (:intervening-maintenance linkage)
                                               :replayed-transaction-count (:replayed-transaction-count linkage)))))))))))

(defn compare-local-processed-retained-mapped-recovery!
  "Bind three adjacent selected attempts to one observed retained sample.
   Check both committed abstract projections, the stale mapped-prefix abort,
   and exact journal linkage. Each nested comparison must observe that same
   sample, not merely a separately stable history. Supplied mapped expectations
   are premises; this does not derive the complete executor envelopes."
  [node tables steps expected]
  (compare-mapped-recovery* node tables steps expected false nil))

(defn compare-local-processed-retained-mapped-worker-interleaved-recovery!
  "Bind nonadjacent selected attempts to one complete observed retained sample.
   Require exact intervening worker envelopes and protected-state stuttering,
   then the existing selected physical and abstract comparisons over the same
   full history. Source identity and mapped expectations remain premises;
   no authenticated writer or provider refinement is established."
  [node tables steps expected source-id]
  (compare-mapped-recovery* node tables steps expected true source-id))

(defn compare-with-retained-root!
  "Constrain a synchronous recovery comparison to an exact two-transaction
   initialization prefix and its immediate successor. Acquire independently;
   the nested recovery must match this sample. The callback is trusted
   diagnostic code, not an authority or substitute acquisition."
  [node manifest initializer first-selected-id compare!]
  (let [fail #(acquisition-result %1 %2)
        before (observe-replay-history! node)
        [m i first-step] (:replay-inputs before)]
    (cond
      (not= :observed (:status before)) (fail (:status before) (:reason before))
      (and *required-retained-sample* (not= *required-retained-sample* before))
      (fail :unavailable :source-log-changed-during-recovery-comparison)
      (not (and (= manifest (select-keys m [:tx-id :ops]))
                (= initializer i) (= first-selected-id (:tx-id first-step))))
      (fail :mismatch :selected-activation-root-mismatch)
      :else
      (let [result (binding [*required-retained-sample* before] (compare!))
            after (observe-replay-history! node)]
        (cond
          (not= :observed (:status after)) (fail (:status after) (:reason after))
          (not= before after) (fail :unavailable :source-log-changed-during-recovery-comparison)
          (not= :matched (:status result)) (fail (:status result) (:reason result))
          :else result)))))

(def retained-clocked-execution-profile :selected-processed-retained-clocked-execution-v3)

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
