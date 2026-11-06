(ns gesso.assurance.proof.xtdb-conditional-write-semantics
  "v1511: Restricted, inspectable semantics for Gesso's canonical XTDB
   ASSERT/ASSERT/PUT transaction shape.

   This is a *model of an explicitly admitted SQL slice*, not a SQL parser,
   XTDB implementation proof, or authenticated internal-snapshot observer.
   Exact SQL/parameter checks bind the candidate to the documented shape:
   business-field count assertion, version/CAS count assertion, put-docs.
   All predicates precede the replacement write. This model can expose
   unguarded fields that the replacement could overwrite even if both
   assertions succeed, and it refuses to call partial guards a full prestate
   or frame guarantee.

   Caller-provided rows and operation vectors are NOT authenticated. A
   positive diagnostic is conditional on correct canonical lowering, XTDB's
   tested sequential DML/ASSERT semantics, faithful data readback, and an
   unchanged submitted plan. It is NOT an issued executor, physical receipt,
   transaction-internal snapshot, writer-closure claim, or certificate."
  (:require [clojure.set :as set]))

(def report-type
  :gesso.assurance.proof.xtdb-conditional-write-semantics/restricted-plan)
(def model-type
  :gesso.assurance.proof.xtdb-conditional-write-semantics/modeled-step)

(defn- reject! [reason]
  (throw (ex-info "Unsupported XTDB conditional-write model input."
                  {:xtdb-conditional-write-semantics/rejection true
                   :reason reason})))

(defn- plain-map? [v]
  (and (or (instance? clojure.lang.PersistentArrayMap v)
           (instance? clojure.lang.PersistentHashMap v))
       (nil? (meta v))))

(defn- field? [field]
  (and (keyword? field)
       (nil? (namespace field))
       (<= 1 (count (name field)) 64)
       (re-matches #"[a-z][a-z0-9-]*" (name field))))

(defn- scalar? [v]
  (or (string? v) (integer? v) (boolean? v)))

(defn- supported-row? [row]
  (and (plain-map? row)
       (<= 2 (count row) 24)
       (string? (:xt/id row))
       (<= 1 (count (:xt/id row)) 128)
       (every? (fn [[k v]]
                 (and (or (= k :xt/id) (field? k))
                      (scalar? v))) row)))

(defn- supported-fields? [fields row]
  (and (vector? fields)
       (<= 1 (count fields) 12)
       (= (count fields) (count (distinct fields)))
       (every? #(and (field? %) (contains? row %)) fields)))

(defn- assert-sql [table row fields]
  (into [(str "ASSERT ? = (SELECT COUNT(*) FROM " (name table)
              " WHERE (_id = ?)"
              (apply str
                     (map (fn [field]
                            (str " AND ("
                                 (.replace (name field) "-" "_")
                                 " = ?)"))
                          fields))
              ")")
         1 (:xt/id row)]
        (map row fields)))

(defn- checked-input! [{:keys [table before after guard-fields cas-fields tx-ops]
                        :as input}]
  (when-not (and (plain-map? input)
                 (= #{:table :before :after :guard-fields :cas-fields :tx-ops}
                    (set (keys input)))
                 (keyword? table)
                 (nil? (namespace table))
                 (<= 1 (count (name table)) 96)
                 (re-matches #"[a-z][a-z0-9_]*" (name table))
                 (supported-row? before)
                 (supported-row? after)
                 (= (set (keys before)) (set (keys after)))
                 (= (:xt/id before) (:xt/id after))
                 (not= before after)
                 (supported-fields? guard-fields before)
                 (supported-fields? cas-fields before)
                 (empty? (set/intersection (set guard-fields)
                                           (set cas-fields)))
                 (vector? tx-ops)
                 (= 3 (count tx-ops)))
    (reject! :invalid-restricted-plan-input))
  (when-not (= [(assert-sql table before guard-fields)
                (assert-sql table before cas-fields)
                [:put-docs table after]]
               tx-ops)
    (reject! :noncanonical-or-reordered-conditional-write))
  input)

(defn inspect-plan!
  "Fail-closed exact-shape check for the restricted two-ASSERT/one-PUT slice.

   :before and :after are supplied physical rows, *not* XTDB attestations.
   :guard-fields and :cas-fields describe which fields the two count
   assertions check. SQL vectors must match the exact canonical shape.
   The report intentionally contains no row contents, SQL, or document IDs.

   Every persisted field NOT compared by either assertion is identified as
   unguarded. Even a successful conditional transaction may replace it from
   the stale prepared :after document. No proof or application admission is
   conferred by an empty unguarded set, either."
  [input]
  (let [{:keys [before guard-fields cas-fields]} (checked-input! input)
        protected (conj (set (concat guard-fields cas-fields)) :xt/id)
        unguarded (set/difference (set (keys before)) protected)]
    {:type report-type
     :version 1
     :scope :exact-two-sql-assertions-followed-by-one-put-docs
     :canonical-shape-matches? true
     :all-assertions-precede-write? true
     :guarded-field-count (count protected)
     :unguarded-persisted-fields unguarded
     :complete-declared-row-compared? (empty? unguarded)
     :statement-state-model :sequential-count-assertions-before-replacement
     :physical-prestate :not-authenticated
     :executed-plan-binding :not-established
     :external-writer-coverage :not-established
     :backend-semantics-proof :not-established
     :physical-frame-preservation :not-established
     :universal-refinement :not-established
     :proof-status :not-proved
     :certificate :not-issued
     :admission :not-admitted
     :authority :none}))

(defn model-step!
  "Pure *conditional* model for one restricted DML plan at statement time.

   For an externally supplied :current row, both count assertions match
   exactly when the ID and every guarded field match :before. Failure leaves
   :current unchanged in the model; success replaces it with the *entire*
   supplied :after row (XTDB put-docs replacement semantics).

   :overwritten-unguarded-fields makes a stale-frame overwrite observable.
   This is not an XTDB execute-tx, a receipt, or even a verified simulation
   of arbitrary SQL or arbitrary concurrency. Do not retry based on this
   model or report a physically committed outcome from it."
  [input current]
  (let [{:keys [before after guard-fields cas-fields]}
        (checked-input! input)
        _ (when-not (or (nil? current) (supported-row? current))
            (reject! :unsupported-current-document))
        guarded (conj (set (concat guard-fields cas-fields)) :xt/id)
        eligible? (and current
                       (every? #(= (get before %) (get current %)) guarded))
        unguarded (set/difference
                   (set/union (set (keys before))
                              (if current (set (keys current)) #{})
                              (set (keys after)))
                   guarded)
        overwritten (if eligible?
                      (set (filter #(not= (get current %) (get after %))
                                   unguarded))
                      #{})]
    {:type model-type
     :version 1
     :modeled-outcome (if eligible? :would-commit :assertion-would-fail)
     :next-document (if eligible? after current)
     :overwritten-unguarded-fields overwritten
     :external-writer-coverage :not-established
     :physical-outcome :not-observed
     :physical-prestate :not-authenticated
     :backend-semantics-proof :not-established
     :certificate :not-issued
     :proof-status :not-proved}))
