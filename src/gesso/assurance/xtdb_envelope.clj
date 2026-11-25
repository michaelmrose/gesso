(ns gesso.assurance.xtdb-envelope
  "Diagnostic correspondence for the restricted mapped ASSERT/put-docs
   transaction language. Compare caller-supplied expectations with the
   retained decoded source log and a separate provider outcome. Neither
   an authenticated receipt nor proof of execution or writer identity."
  (:require [clojure.string :as str]
            [gesso.assurance.subject :as subject]
            [gesso.assurance.xtdb-source-log :as source-log]
            [xtdb.api :as xt])
  (:import (java.time Instant ZonedDateTime)
           (java.util HexFormat)
           (xtdb.util Iid)))

(def profile :xtdb-2.2.0-beta1/restricted-envelope-v1)
(def ^:private log-profile :xtdb-2.2.0-beta1/decoded-source-log-v1)

(defn- reject! [reason]
  (throw (ex-info "Unsupported expected envelope." {::unsupported true :reason reason})))

(defn- expected-operations! [ops]
  (when-not (and (vector? ops) (<= 1 (count ops) 16384))
    (reject! :unsupported-operation-sequence))
  (let [remaining (volatile! 262144)
        chars (volatile! (* 16 1024 1024))
        charge! (fn [n]
                  (when (neg? (vswap! remaining - n)) (reject! :expected-value-limit)))
        text! (fn [s]
                (when (neg? (vswap! chars - (count s))) (reject! :expected-text-limit))
                s)]
    (letfn [(value! [v depth document?]
              (charge! 1)
              (when (> depth 64) (reject! :expected-depth-limit))
              (cond
                (string? v) (text! v)
                (keyword? v) (do (text! (str v)) v)
                (or (boolean? v) (integer? v) (uuid? v)) v
                (vector? v) (mapv #(value! % (inc depth) document?) v)
                (and document? (map? v))
                (into {} (map (fn [[k child]]
                                (when-not (keyword? k) (reject! :unsupported-document-key))
                                (charge! 1)
                                (text! (str k))
                                [k (value! child (inc depth) true)])) v)
                (and document? (or (instance? Instant v) (instance? ZonedDateTime v))) v
                :else (reject! :unsupported-expected-value)))
            (sql! [[sql & args]]
              (text! sql)
              ;; Only generated assertion text: quoted identifier segments
              ;; may contain $, but no quoted literals, comments, numbered
              ;; parameters, semicolons or ambiguous question marks.
              (let [unquoted (str/replace sql #"\"[a-zA-Z_][a-zA-Z0-9_$]*\"" "identifier")]
                (when-not (and (re-matches #"ASSERT [a-zA-Z0-9_$ \t\r\n(),.*?=<>!+\-\[\]]+" unquoted)
                               (not (re-find #"(?<![a-zA-Z0-9_$])\$" unquoted))
                               (not (str/includes? unquoted "--")))
                  (reject! :unsupported-assertion-text)))
              (let [n (volatile! 0)
                    query (str/replace sql #"\?" (fn [_] (str "$" (vswap! n inc))))]
                (when-not (= @n (count args)) (reject! :unsupported-placeholder-shape))
                {:kind :sql :query query :args (mapv #(value! % 0 false) args)}))
            (put! [op]
              (let [table (second op)]
                (when-not (and (simple-keyword? table)
                               (re-matches #"[a-z][a-z0-9_]*" (name table))
                               (< 2 (count op)))
                  (reject! :unsupported-put-shape))
                (text! (name table))
                (let [docs (mapv (fn [doc]
                                   (when-not (and (map? doc) (contains? doc :xt/id)
                                                  (not-any? #(contains? doc %) [:xt/valid-from :xt/valid-to]))
                                     (reject! :unsupported-document-shape))
                                   (value! doc 0 true)) (subvec op 2))]
                  {:kind :put-docs :table (keyword "public" (name table)) :documents docs
                   :iids (mapv #(.formatHex (HexFormat/of) (Iid/getAsIid (:xt/id %))) docs)
                   :bounds {}})))]
      (mapv (fn [op]
              (charge! 1)
              (when-not (and (vector? op) (seq op)) (reject! :unsupported-operation))
              (cond
                (string? (first op)) (sql! op)
                (= :put-docs (first op)) (put! op)
                :else (reject! :unsupported-operation))) ops))))

(def binding-profile :xtdb-2.2.0-beta1/expected-transition-envelope-v1)
(def binding-keys [:envelope-profile :envelope-fingerprint])

(defn binding-status
  "Shape only. Older witnesses have no envelope binding; a present digest
   is not yet evidence of actual submission or execution."
  [witness]
  (cond
    (not-any? #(contains? witness %) binding-keys) :not-recorded
    (and (= binding-profile (:envelope-profile witness))
         (string? (:envelope-fingerprint witness))
         (re-matches #"sha256:[0-9a-f]{64}" (:envelope-fingerprint witness))) :recorded-unchecked
    :else :invalid))

(defn- digest-value [v]
  ;; Tag containers as well as non-EDN leaves: a literal vector cannot
  ;; alias the encoding of a UUID or temporal value.
  (cond
    (map? v) [:map (into {} (map (fn [[k child]] [k (digest-value child)])) v)]
    (vector? v) [:vector (mapv digest-value v)]
    (uuid? v) [:uuid (str v)]
    (instance? Instant v) [:instant (str v)]
    (instance? ZonedDateTime v) [:zoned-date-time (str v)]
    :else v))

(defn- transition-witness! [ops]
  (let [op (when (vector? ops) (peek ops))]
    (when-not (and (vector? op) (= 3 (count op))
                   (= [:put-docs :gesso_transition_evidence] (subvec op 0 2))
                   (map? (peek op)) (= 4 (:profile-version (peek op))))
      (reject! :invalid-transition-envelope-witness))
    (peek op)))

(defn- binding-evidence! [ops]
  {:envelope-profile binding-profile
   :envelope-fingerprint
   (:fingerprint (subject/describe-subject
                  {:kind :rule :id ::transition-envelope
                   :interface {:profile binding-profile
                               :operations (digest-value (expected-operations! ops))}}))})

(defn bind-transition-envelope!
  "Bind the expected ordered restricted envelope to its final v4 witness
   before submission. The digest excludes ONLY its own two binding fields
   in that final witness. Subject-encoder limits also apply. No transaction
   metadata, source-log acquisition or writer authentication is asserted."
  [ops]
  (let [witness (transition-witness! ops)]
    (when-not (= :not-recorded (binding-status witness))
      (reject! :already-bound-transition-envelope))
    (assoc-in ops [(dec (count ops)) 2] (merge witness (binding-evidence! ops)))))

(defn bound-transition-envelope?
  "Recompute the expected-envelope binding; not a source-log check."
  [ops]
  (let [witness (transition-witness! ops)]
    (and (= :recorded-unchecked (binding-status witness))
         (= (select-keys witness binding-keys)
            (binding-evidence! (assoc-in ops [(dec (count ops)) 2]
                                         (apply dissoc witness binding-keys)))))))

(defn- check-envelope*
  [node tx-id ops committed? expected! normalize-observed]
  (let [report (fn [status reason]
                 (cond-> {:status status :profile profile :authority :none :certificate :not-issued}
                   reason (assoc :reason reason)))]
    (try
      (if-not (boolean? committed?)
        (report :unsupported :invalid-expected-outcome)
        (let [expected (expected! ops)
              observed (source-log/observe-source-message! node tx-id)]
          (case (:status observed)
            :unavailable (report :unavailable (:reason observed))
            :unsupported (report :unsupported (:reason observed))
            :source-message-observed
            (cond
              (not= log-profile (:provider-profile observed)) (report :unsupported :source-profile-mismatch)
              (not= tx-id (:tx-id observed)) (report :mismatch :source-transaction-identity-mismatch)
              (not= expected (normalize-observed (:operations observed))) (report :mismatch :operation-sequence-mismatch)
              :else
              (let [outcomes (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" tx-id])]
                (cond
                  (empty? outcomes) (report :unavailable :transaction-outcome-unavailable)
                  (not (and (= 1 (count outcomes)) (boolean? (:committed (first outcomes)))))
                  (report :unsupported :invalid-transaction-outcome)
                  (not= [{:committed committed?}] outcomes) (report :mismatch :transaction-outcome-mismatch)
                  :else (assoc (report :matched nil) :tx-id tx-id :committed? committed?
                               :operation-count (count expected)))))
            (report :unsupported :unexpected-source-observation))))
      (catch InterruptedException e
        (.interrupt (Thread/currentThread))
        (throw e))
      (catch clojure.lang.ExceptionInfo e
        (if (::unsupported (ex-data e))
          (report :unsupported (:reason (ex-data e)))
          (report :unavailable :envelope-check-failed)))
      (catch Exception _ (report :unavailable :envelope-check-failed)))))

(defn check-envelope!
  "Compare a nonempty vector of generated ASSERT and canonical public
   put-docs operations (without temporal options) against one exact
   transaction and expected boolean commit outcome. Returns :matched,
   :mismatch, :unsupported or :unavailable; no raw values or SQL in the
   report. Match is decoded operation equality only; does not cover
   transaction metadata, caller bytes, writer identity or certification."
  [node tx-id ops committed?]
  (check-envelope* node tx-id ops committed? expected-operations! identity))

(def ^:private worker-guard-arities
  {"ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ?)" 2
   "ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ? AND CURRENT_TIMESTAMP >= ?)" 3
   "ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ? AND CURRENT_TIMESTAMP < ?)" 3
   "ASSERT CURRENT_TIMESTAMP < ?" 1
   "ASSERT CURRENT_TIMESTAMP + INTERVAL '5' MINUTE >= ?" 1})

(defn- timestamp-instant [v]
  (if (instance? ZonedDateTime v) (.toInstant ^ZonedDateTime v) v))

(defn- normalize-worker-operations [operations]
  ;; SQL timestamp arguments have instant semantics. Arrow represents the
  ;; same timestamp with a different UTC zone for SQL and document values.
  ;; Do not normalize documents, bounds, IIDs, query text or other values.
  (mapv #(if (= :sql (:kind %)) (update % :args (fn [args] (mapv timestamp-instant args))) %) operations))

(defn- expected-worker-operations! [ops]
  (when-not (and (vector? ops) (<= 2 (count ops) 4)
                 (vector? (peek ops)) (= 3 (count (peek ops)))
                 (= [:put-docs :gesso_live_outbox] (subvec (peek ops) 0 2)))
    (reject! :invalid-worker-envelope))
  (let [guards (mapv (fn [op]
                       (let [[sql & args] op
                             n (get worker-guard-arities sql)
                             args (mapv timestamp-instant args)]
                         (when-not (and (vector? op) n (= n (count args))
                                        (if (= 1 n)
                                          (instance? Instant (first args))
                                          (and (uuid? (first args)) (every? #(instance? Instant %) (rest args)))))
                           (reject! :invalid-worker-guard))
                         (let [i (volatile! 0)]
                           {:kind :sql :query (str/replace sql #"\?" (fn [_] (str "$" (vswap! i inc)))) :args args})))
                     (pop ops))]
    (into guards (expected-operations! [(peek ops)]))))

(defn check-worker-envelope!
  "Compare reconstructed worker guards and the complete pinned replacement
   with the retained committed operation sequence. Only the fixed worker
   SQL forms admit timestamp arguments; mapped assertion grammar is unchanged.
   SQL timestamps are compared as instants, not caller representations."
  [node tx-id ops]
  (check-envelope* node tx-id ops true expected-worker-operations! normalize-worker-operations))

(defn- observed-restricted-operations! [operations]
  ;; Require an exact round trip through the restricted caller language.
  ;; Never discard bounds, IID, placeholder order or extra fields.
  (when-not (and (vector? operations) (<= 1 (count operations) 16384))
    (reject! :unsupported-operation-sequence))
  (let [ops (mapv (fn [{:keys [kind query args table documents]}]
                    (case kind
                      :sql (do
                             (when-not (and (string? query) (vector? args))
                               (reject! :unsupported-observed-sql))
                             (into [(str/replace query #"(?<![a-zA-Z0-9_$])\$[0-9]+" "?")] args))
                      :put-docs (do
                                  (when-not (and (keyword? table) (= "public" (namespace table))
                                                 (vector? documents))
                                    (reject! :unsupported-observed-put))
                                  (into [:put-docs (keyword (name table))] documents))
                      (reject! :unsupported-observed-operation))) operations)]
    (when-not (= operations (expected-operations! ops))
      (reject! :unsupported-observed-envelope))
    ops))

(defn- check-recorded-stuttering-attempt!
  [node tx-id committed?]
  (let [report (fn [status reason]
                 (cond-> {:status status :profile profile :authority :none :certificate :not-issued}
                   reason (assoc :reason reason)))]
    (try
      (let [observed (source-log/observe-source-message! node tx-id)]
        (case (:status observed)
          :unavailable (report :unavailable (:reason observed))
          :unsupported (report :unsupported (:reason observed))
          :source-message-observed
          (cond
            (not= log-profile (:provider-profile observed)) (report :unsupported :source-profile-mismatch)
            (not= tx-id (:tx-id observed)) (report :mismatch :source-transaction-identity-mismatch)
            (and committed? (some #(= :put-docs (:kind %)) (:operations observed))) (report :mismatch :stutter-contains-write)
            :else
            (let [ops (observed-restricted-operations! (:operations observed))
                  outcomes (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" tx-id])]
              (cond
                (and committed? (not (every? #(string? (first %)) ops))) (report :mismatch :stutter-contains-write)
                (empty? outcomes) (report :unavailable :transaction-outcome-unavailable)
                (not (and (= 1 (count outcomes)) (boolean? (:committed (first outcomes)))))
                (report :unsupported :invalid-transaction-outcome)
                (not= [{:committed committed?}] outcomes) (report :mismatch :transaction-outcome-mismatch)
                :else (if committed?
                        (assoc (report :matched nil) :tx-id tx-id :stutter-operations :assertion-only)
                        (assoc (report :matched nil) :tx-id tx-id :committed? false
                               :aborted-operations :restricted-envelope)))))
          (report :unsupported :unexpected-source-observation)))
      (catch InterruptedException e
        (.interrupt (Thread/currentThread))
        (throw e))
      (catch clojure.lang.ExceptionInfo e
        (if (::unsupported (ex-data e))
          (report :unsupported (:reason (ex-data e)))
          (report :unavailable :envelope-check-failed)))
      (catch Exception _ (report :unavailable :envelope-check-failed)))))

(defn check-recorded-stutter!
  "Qualify a committed, retained assertion-only transaction. No document
   writes, other SQL or unsupported operations are admitted. This is a
   decoded operation check, conditional on provider semantics and retention."
  [node tx-id]
  (check-recorded-stuttering-attempt! node tx-id true))

(defn check-recorded-abort!
  "Qualify an exact retained restricted ASSERT/put-docs attempt and a
   separately observed aborted outcome. Attempted writes need not be
   authorized: this is neither intended-envelope correspondence nor proof
   of rollback, abort cause, or provider execution semantics."
  [node tx-id]
  (check-recorded-stuttering-attempt! node tx-id false))

(defn- selected-put-entries [operations tables]
  ;; Compare full decoded documents with multiplicity. Grouping disjoint
  ;; puts is immaterial here; duplicate writes are not silently collapsed.
  (frequencies (for [{:keys [kind table documents]} operations
                     :when (and (= :put-docs kind) (contains? tables table))
                     doc documents]
                 [table doc])))

(def ^:private outbox-absence-sql
  "ASSERT 0 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ?)")

(defn- outbox-guard-entries [ops]
  (frequencies (filter #(= outbox-absence-sql (first %)) ops)))

(defn- expected-selected-puts! [{:keys [tables puts] :as expectation}]
  (when-not (and (map? expectation) (= #{:tables :puts} (set (keys expectation)))
                 (set? tables) (seq tables) (contains? tables :gesso_commands)
                 (every? #(and (simple-keyword? %) (re-matches #"[a-z][a-z0-9_]*" (name %))) tables)
                 (not (contains? tables :gesso_transition_evidence))
                 (vector? puts) (seq puts)
                 (every? #(and (vector? %) (= :put-docs (first %)) (contains? tables (second %))) puts))
    (reject! :invalid-intended-mapped-writes))
  (let [tables (set (map #(keyword "public" (name %)) tables))
        operations (expected-operations! puts)
        ids (for [{:keys [table documents]} operations doc documents] [table (:xt/id doc)])]
    (when-not (= (count ids) (count (distinct ids)))
      (reject! :invalid-intended-mapped-writes))
    (let [outbox? (contains? tables :public/gesso_live_outbox)
          mapped-tables (disj tables :public/gesso_live_outbox)
          outbox-entries (selected-put-entries operations #{:public/gesso_live_outbox})]
      {:tables mapped-tables :entries (selected-put-entries operations mapped-tables)
       :outbox? outbox? :outbox-entries outbox-entries
       :outbox-guards (frequencies (for [[[_ doc] n] outbox-entries
                                        _ (range n)]
                                    [outbox-absence-sql (:xt/id doc)]))})))

(defn- expected-residual-assertions! [assertions writes]
  (when-not (and (:outbox? writes) (vector? assertions)
                 (<= (count assertions) 16384)
                 (every? #(and (vector? %) (string? (first %))
                               (not= outbox-absence-sql (first %))) assertions))
    (reject! :invalid-intended-residual-assertions))
  ;; Validate the same restricted SQL/argument language as the retained log.
  (when (seq assertions) (expected-operations! assertions))
  (merge-with + (frequencies assertions) (:outbox-guards writes)))

(defn- residual-assertions-match? [ops prefix-count expected]
  (let [tail (subvec ops prefix-count)
        sql? #(string? (first %))]
    (= expected
       (frequencies (take-while sql? tail))
       (frequencies (filter sql? tail)))))

(defn- check-recorded-transition*
  "Check a committed transition's decoded source-log envelope against its
   independently acquired, pinned durable v4 witness. No caller operation
   vector is required. The final logged witness must match exactly, its
   binding must recompute, and the ledger must independently show commit.
   This checks decoded correspondence, not writer authority or execution."
  [node tx-id witness require-prefix? prefix require-writes? writes require-residual? assertions close-writes?]
  (let [report (fn [status reason]
                 (cond-> {:status status :profile binding-profile :authority :none :certificate :not-issued}
                   reason (assoc :reason reason)))]
    (try
      (cond
        (not (and (map? witness) (= 4 (:profile-version witness)) (some? (:xt/id witness))))
        (report :unsupported :invalid-transition-envelope-witness)
        (not= :recorded-unchecked (binding-status witness))
        (report :unsupported :missing-or-invalid-envelope-binding)
        (and require-prefix?
             (not (and (vector? prefix) (seq prefix)
                       (every? #(and (vector? %) (string? (first %))) prefix))))
        (report :unsupported :invalid-intended-guard-prefix)
        :else
        (let [expected-prefix (when require-prefix? (expected-operations! prefix))
              expected-writes (when require-writes? (expected-selected-puts! writes))
              expected-residual (when require-residual? (expected-residual-assertions! assertions expected-writes))
              allowed-tables (when close-writes?
                               (when-not (:outbox? expected-writes)
                                 (reject! :incomplete-closed-write-expectation))
                               (conj (:tables expected-writes) :public/gesso_live_outbox
                                     :public/gesso_transition_evidence))
              observed (source-log/observe-source-message! node tx-id)]
          (case (:status observed)
            :unavailable (report :unavailable (:reason observed))
            :unsupported (report :unsupported (:reason observed))
            :source-message-observed
            (cond
              (not= log-profile (:provider-profile observed)) (report :unsupported :source-profile-mismatch)
              (not= tx-id (:tx-id observed)) (report :mismatch :source-transaction-identity-mismatch)
              :else
              (let [ops (observed-restricted-operations! (:operations observed))]
                (cond
                  (not= witness (transition-witness! ops)) (report :mismatch :source-witness-mismatch)
                  (not (bound-transition-envelope? ops)) (report :mismatch :envelope-fingerprint-mismatch)
                  (not= {[:public/gesso_transition_evidence witness] 1}
                        (selected-put-entries (:operations observed) #{:public/gesso_transition_evidence}))
                  (report :mismatch :transition-evidence-write-mismatch)
                  (and require-prefix?
                       (or (>= (count expected-prefix) (count ops))
                           (not= expected-prefix (subvec (:operations observed) 0 (count expected-prefix)))))
                  (report :mismatch :intended-guard-prefix-mismatch)
                  (and require-writes?
                       (not= (:entries expected-writes)
                             (selected-put-entries (:operations observed) (:tables expected-writes))))
                  (report :mismatch :intended-mapped-writes-mismatch)
                  (and (:outbox? expected-writes)
                       (not= (:outbox-entries expected-writes)
                             (selected-put-entries (:operations observed) #{:public/gesso_live_outbox})))
                  (report :mismatch :publication-outbox-write-mismatch)
                  (and (:outbox? expected-writes)
                       (not= (:outbox-guards expected-writes)
                             (outbox-guard-entries (take-while #(string? (first %)) ops))
                             (outbox-guard-entries ops)))
                  (report :mismatch :publication-outbox-guard-mismatch)
                  (and close-writes?
                       (some #(and (= :put-docs (:kind %))
                                   (not (contains? allowed-tables (:table %)))) (:operations observed)))
                  (report :mismatch :unexpected-write-table)
                  (and require-residual?
                       (not (residual-assertions-match? ops (count expected-prefix) expected-residual)))
                  (report :mismatch :intended-residual-assertions-mismatch)
                  :else
                  (let [outcomes (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" tx-id])]
                    (cond
                      (empty? outcomes) (report :unavailable :transaction-outcome-unavailable)
                      (not (and (= 1 (count outcomes)) (boolean? (:committed (first outcomes)))))
                      (report :unsupported :invalid-transaction-outcome)
                      (not= [{:committed true}] outcomes) (report :mismatch :transaction-outcome-mismatch)
                      :else (cond-> (assoc (report :matched nil) :tx-id tx-id :operation-count (count ops)
                                           :envelope-fingerprint (:envelope-fingerprint witness))
                              require-prefix? (assoc :intended-guard-prefix :matched)
                              require-writes? (assoc :intended-mapped-writes :matched
                                                     :transition-evidence-write :matched)
                              (:outbox? expected-writes) (assoc :publication-outbox :matched)
                              require-residual? (assoc :intended-residual-assertions :matched)
                              close-writes? (assoc :write-table-closure :matched)))))))
            (report :unsupported :unexpected-source-observation))))
      (catch InterruptedException e
        (.interrupt (Thread/currentThread))
        (throw e))
      (catch clojure.lang.ExceptionInfo e
        (if (::unsupported (ex-data e))
          (report :unsupported (:reason (ex-data e)))
          (report :unavailable :envelope-check-failed)))
      (catch Exception _ (report :unavailable :envelope-check-failed)))))

(defn check-recorded-transition!
  "Check a retained committed transition against its recorded binding.
   Exactly one evidence document write is allowed: the final witness.
   The four-argument form additionally requires an independently derived,
   nonempty ordered guard prefix at the start of the decoded transaction.
   The five-argument form also checks complete selected-table puts with
   multiplicity against independent expectations. Including the outbox
   table also requires exactly its expected prewrite absence guards.
   The six-argument form additionally requires all remaining assertions
   (except the separately derived outbox guards) as a complete vector.
   Their multiplicity and prewrite placement must match; order within this
   residual group is not checked. This does not derive assertion intent,
   establish semantic equivalence or prove writer authority."
  ([node tx-id witness]
   (check-recorded-transition* node tx-id witness false nil false nil false nil false))
  ([node tx-id witness prefix]
   (check-recorded-transition* node tx-id witness true prefix false nil false nil false))
  ([node tx-id witness prefix writes]
   (check-recorded-transition* node tx-id witness true prefix true writes false nil false))
  ([node tx-id witness prefix writes assertions]
   (check-recorded-transition* node tx-id witness true prefix true writes true assertions false)))

(defn check-closed-recorded-transition!
  "Require complete write-table closure as well as the selected write checks.
   The expectation must include the outbox table even when empty. The final
   evidence table is accounted for separately. Nil assertions explicitly
   select undeclared residual semantics; a vector selects residual checking.
   This closes decoded puts for this transaction, not all database writers."
  [node tx-id witness prefix writes assertions]
  (check-recorded-transition* node tx-id witness true prefix true writes
                              (some? assertions) assertions true))
