(ns gesso.assurance.proof.xtdb-guarded-prestate-acquisition
  "v1484: Acquire a bounded EXTERNAL XTDB read snapshot, then derive additional
   prewrite assertions over each observed document for a canonical Gesso plan.

   This is an opt-in, read-only *plan constructor*. It NEVER submits a DML
   transaction and never asserts that a later execution used this plan. In
   XTDB 2.2.0-beta1 the transaction-internal DML basis is not exposed. If
   the exact returned guarded operations execute in order on a compatible
   backend, their leading ASSERTs constrain the *prewrite state seen by
   those assertions* to match the observed supported top-level fields, or
   reject the transaction atomically. They do not attest the entire internal
   snapshot, nested field structure, unenumerated dependencies, or writers.

   The resulting private report contains raw documents, plans, and parameterized
   operations; never log it. A report is not an issued executor, a certificate,
   a retry authorization, or an application-admission decision. In particular,
   a successful read followed by a changed physical writer path gives no
   assurance, even if this constructor returned without error."
  (:require [gesso.assurance.proof.xtdb-internal-prestate-capability :as cap]
            [gesso.model.command :as command]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]
            [xtdb.basis :as xt.basis])
  (:import [java.time Instant ZonedDateTime OffsetDateTime LocalDate]
           [java.math BigDecimal]
           [java.util UUID]))

(def report-type
  :gesso.assurance.proof.xtdb-guarded-prestate-acquisition/diagnostic)
(def report-version 1)
(def ^:private max-targets 32)
(def ^:private max-fields 128)
(def ^:private original-capabilities cap/capability-report!)
(def ^:private original-query xt/q)
(def ^:private original-decode xt.basis/<-time-basis-str)
(def ^:private original-prepare tx/prepare)
(def ^:private original-normalize tx/normalize-plan)
(def ^:private original-all-fields tx/assert-document-current)
(def ^:private original-field-count tx/assert-document-field-count)
(def ^:private original-absent tx/assert-document-absent)

(defn- reject! [reason at]
  ;; No document values, IDs, SQL or token material in exceptions.
  (throw (ex-info "Unsupported XTDB guarded-prestate acquisition."
                  {:xtdb-guarded-prestate/rejection true
                   :reason reason :at at})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentHashMap x)
           (instance? clojure.lang.PersistentArrayMap x))
       (nil? (meta x))))

(defn- exact! [x ks at]
  (when-not (and (plain-map? x) (= ks (set (keys x))))
    (reject! :invalid-source-shape at))
  x)

(defn- seams! []
  (when-not (and (identical? original-capabilities (var-get #'cap/capability-report!))
                 (identical? original-query (var-get #'xt/q))
                 (identical? original-decode (var-get #'xt.basis/<-time-basis-str))
                 (identical? original-prepare (var-get #'tx/prepare))
                 (identical? original-normalize (var-get #'tx/normalize-plan))
                 (identical? original-all-fields (var-get #'tx/assert-document-current))
                 (identical? original-field-count (var-get #'tx/assert-document-field-count))
                 (identical? original-absent (var-get #'tx/assert-document-absent)))
    (reject! :substituted-checker-or-acquisition-seam [])))

(defn- identifier? [s]
  (and (string? s) (boolean (re-matches #"[A-Za-z_][A-Za-z0-9_-]*" s))))

(defn- sql-table! [k i]
  (when-not (and (keyword? k)
                 (boolean (re-matches #"[A-Za-z_][A-Za-z0-9_]*" (name k)))
                 (or (nil? (namespace k))
                     (boolean (re-matches #"[A-Za-z_][A-Za-z0-9_]*" (namespace k)))))
    (reject! :unsupported-table-identifier [:targets i]))
  (if-let [schema (namespace k)]
    (str schema "." (name k))
    (name k)))

(defn- simple-id? [id]
  (or (string? id) (instance? UUID id) (integer? id)))

(defn- supported-value? [v]
  (and (some? v)
       (or (string? v) (boolean? v) (instance? UUID v)
           (integer? v) (instance? BigDecimal v)
           (instance? Instant v) (instance? ZonedDateTime v)
           (instance? OffsetDateTime v) (instance? LocalDate v))))

(defn- targets! [plan requested]
  (when-not (and (vector? requested) (nil? (meta requested))
                 (<= 1 (count requested) max-targets)
                 (= (count requested) (count (distinct requested))))
    (reject! :invalid-or-repeated-targets [:targets]))
  (doseq [[i target] (map-indexed vector requested)]
    (when-not (and (vector? target) (= 2 (count target))
                   (simple-id? (second target)))
      (reject! :unsupported-target [:targets i]))
    (sql-table! (first target) i))
  (let [required (set (concat (map command/target (:commands plan))
                              (map command/guard-target (:guards plan))))]
    (when-not (every? (set requested) required)
      (reject! :unobserved-command-or-guard-target [:targets])))
  requested)

(defn- snapshot-token! [node]
  (let [result (try (original-query node "SHOW SNAPSHOT_TOKEN")
                    (catch Throwable _ (reject! :snapshot-acquisition-failed [:token])))
        token (when (and (sequential? result) (= 1 (count result))
                         (plain-map? (first result))
                         (= #{:snapshot-token} (set (keys (first result)))))
                (:snapshot-token (first result)))]
    (when-not (and (string? token) (seq token)
                   (try (map? (original-decode token))
                        (catch Throwable _ false)))
      (reject! :invalid-read-snapshot-token [:token]))
    token))

(defn- row! [node target basis i]
  (let [[table id] target
        sql (str "SELECT * FROM " (sql-table! table i)
                 " WHERE _id = ? LIMIT 2")
        rows (try (original-query node [sql id] basis)
                  (catch Throwable _ (reject! :snapshot-row-read-failed [:targets i])))]
    (when-not (and (sequential? rows) (<= (count rows) 1))
      (reject! :ambiguous-snapshot-row [:targets i]))
    (when-let [document (first rows)]
      (when-not (and (plain-map? document)
                     (<= 1 (count document) max-fields)
                     (every? (fn [[k v]]
                               (and (keyword? k)
                                    (or (= k :xt/id)
                                        (and (nil? (namespace k))
                                             (identifier? (name k)))
                                        (and (namespace k)
                                             (identifier? (namespace k))
                                             (identifier? (name k))))
                                    (supported-value? v)))
                             document)
                     (or (not (contains? document :xt/id))
                         (= id (:xt/id document))))
        (reject! :unsupported-snapshot-row [:targets i])))
    {:target target :document (first rows)}))

(defn- assertions-for [{:keys [target document]}]
  (let [[table id] target]
    (if (nil? document)
      [(original-absent table id)]
      (let [doc (assoc document :xt/id id)
            checks (->> doc
                        (remove (fn [[k _]] (= k :xt/id)))
                        (sort-by (comp str key))
                        (mapv (fn [[k v]] [k v])))]
        (when (empty? checks)
          (reject! :no-assertable-document-fields [:targets]))
        (when (> (count doc) max-fields)
          (reject! :too-many-observed-fields [:targets]))
        [(original-all-fields
          table {:model/id id :model/checks checks})
         (original-field-count table id (count doc))]))))

(defn acquire-guarded-prestate!
  "Read each declared target from ONE pinned external XTDB snapshot and
   return a NEW canonical plan with leading per-document ASSERTs.

   Input: node, {:plan <canonical Gesso model transaction plan>
                 :targets [[entity-type id] ...]}.

   Every command/guard target must be enumerated; extra explicit dependency
   targets are permitted. All returned statements are assertions before any
   write (via gesso.model.tx/prepare). No DML is executed here.

   The report does NOT authenticate the node, the internal DML read basis,
   caller's business guard meaning, arbitrary SQL assertion dependencies,
   or actual execution of the guarded plan. A committed execution of the
   exact returned operations would conditionally establish only the supported
   enumerated top-level field comparisons in the prewrite ASSERT phase."
  [node source]
  (exact! source #{:plan :targets} [])
  (seams!)
  (when-not (and (= :pinned-api-and-gesso-option-surface
                    (:compatibility (original-capabilities)))
                 (some? node))
    (reject! :unsupported-backend-interface []))
  (let [plan (try (original-normalize (:plan source))
                  (catch Throwable _ (reject! :invalid-canonical-plan [:plan])))
        targets (targets! plan (:targets source))
        token (snapshot-token! node)
        basis {:snapshot-token token :current-time (Instant/now)}
        rows (mapv (fn [i target] (row! node target basis i))
                   (range) targets)
        guards (vec (mapcat assertions-for rows))
        guarded-plan (assoc plan :assertions
                            (into guards (:assertions plan)))
        prepared (try (original-prepare nil guarded-plan)
                      (catch Throwable _ (reject! :unable-to-prepare-guarded-plan [])))
        tx-ops (:tx-ops prepared)]
    (when-not (and (= guarded-plan (:plan prepared))
                   (= (count guards)
                      (count (take-while
                              #(and (vector? %) (string? (first %))
                                    (re-find #"(?i)^ASSERT\b" (first %)))
                              (take (count guards) tx-ops)))))
      (reject! :guard-order-not-preserved []))
    {:type report-type
     :version report-version
     :scope :one-external-read-and-unexecuted-guarded-plan
     :external-read-basis basis
     :observed-rows rows
     :observed-target-count (count rows)
     :observed-present-count (count (filter :document rows))
     :original-plan plan
     :guarded-plan guarded-plan
     :guarded-tx-ops tx-ops
     :inserted-assertion-count (count guards)
     :conditional-prewrite-check :all-supported-top-level-values-and-field-presence
     :guard-evaluation-premise :exact-guarded-ops-executed-in-order-on-compatible-xtdb
     :actual-execution-observed? false
     :external-read-node-provenance :caller-supplied-unattested
     :guarded-ops-execution-binding :not-established
     :transaction-internal-prestate :not-authenticated
     :complete-physical-prestate :not-established
     :unknown-assertion-dependencies? (boolean (seq (:assertions plan)))
     :protected-domain-and-other-writers :not-established
     :valid-time-correspondence :not-established
     :proof-status :conditional-unexecuted-plan-diagnostic
     :certificate :not-issued
     :admission :not-admitted
     :transaction-authority :none
     :retry-authority :none
     :settlement-authority :none
     :authority :none}))

(defn redacted-summary
  "Return a safe summary with no IDs, rows, SQL, tokens, or plan contents."
  [report]
  (when-not (and (plain-map? report) (= report-type (:type report))
                 (= report-version (:version report))
                 (= :conditional-unexecuted-plan-diagnostic (:proof-status report))
                 (= :not-issued (:certificate report))
                 (= :none (:authority report)))
    (reject! :invalid-diagnostic-summary []))
  (select-keys report
               [:type :version :scope :observed-target-count
                :observed-present-count :inserted-assertion-count
                :conditional-prewrite-check :guard-evaluation-premise
                :actual-execution-observed? :external-read-node-provenance
                :guarded-ops-execution-binding :transaction-internal-prestate
                :complete-physical-prestate :unknown-assertion-dependencies?
                :protected-domain-and-other-writers :valid-time-correspondence
                :proof-status :certificate :admission :transaction-authority
                :retry-authority :settlement-authority :authority]))
