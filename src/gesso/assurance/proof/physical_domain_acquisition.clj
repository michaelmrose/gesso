(ns gesso.assurance.proof.physical-domain-acquisition
  "v1492: Read the v1491 declared finite physical domain using ONE pinned,
   external, read-only XTDB snapshot. The source is the ORIGINAL v1491
   projection declaration, not a caller-supplied proof or projection report.

   Read every enumerated protected document and check all explicitly mapped
   abstract fields. Require the mapped physical fields to be present; reject
   unexpected fields *inside* the declared protection. With :fields :all,
   an extra persisted field cannot be ignored. With partial protection,
   fields outside that set are reported as unprotected observations rather
   than being silently promoted into certified coverage. An unassigned,
   frame-only protected document is observed, but has no abstract values.

   Results include sensitive rows, IDs, tokens, and source declarations:
   do not log them. Use redacted-summary for diagnostics. This process-local
   read does NOT attest XTDB's later DML prestate, the caller-supplied node,
   valid-time correspondence, writer completeness, physical value semantics,
   or frame preservation. It issues no operation, transaction, certificate,
   application admission, retry authority, or browser settlement authority."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.physical-domain-projection :as projection]
            [gesso.assurance.proof.xtdb-internal-prestate-capability :as cap]
            [xtdb.api :as xt]
            [xtdb.basis :as xt.basis])
  (:import [java.time Instant]
           [java.util UUID]))

(def acquisition-type
  :gesso.assurance.proof.physical-domain-acquisition/pinned-external-observation)
(def acquisition-version 1)
(def ^:private max-targets 128)
(def ^:private max-row-fields 256)
(def ^:private original-projection projection/describe-physical-domain-projection!)
(def ^:private original-projection-check projection/require-current-physical-domain-projection!)
(def ^:private original-capability cap/capability-report!)
(def ^:private original-query xt/q)
(def ^:private original-decode xt.basis/<-time-basis-str)

(defn- reject! [reason at]
  ;; Never put source, IDs, tokens, raw SQL, row values, or exceptions in ex-data.
  (throw (ex-info "Unsupported XTDB physical domain acquisition."
                  {:physical-domain-acquisition/rejection true
                   :reason reason :at at})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- check-seams! []
  ;; In-process accidental substitution protection; not JVM attestation.
  (when-not (and (identical? original-projection
                             (var-get #'projection/describe-physical-domain-projection!))
                 (identical? original-projection-check
                             (var-get #'projection/require-current-physical-domain-projection!))
                 (identical? original-capability
                             (var-get #'cap/capability-report!))
                 (identical? original-query (var-get #'xt/q))
                 (identical? original-decode (var-get #'xt.basis/<-time-basis-str)))
    (reject! :substituted-source-or-read-seam [])))

(defn- sql-table! [table index]
  (when-not (and (keyword? table)
                 (re-matches #"[A-Za-z_][A-Za-z0-9_]*" (name table))
                 (or (nil? (namespace table))
                     (re-matches #"[A-Za-z_][A-Za-z0-9_]*"
                                 (namespace table))))
    (reject! :unsupported-sql-table [:targets index]))
  (if-let [schema (namespace table)]
    (str schema "." (name table))
    (name table)))

(defn- supported-id? [id]
  (or (string? id) (instance? UUID id) (integer? id)))

(defn- checked-source! [source]
  (check-seams!)
  (let [r (try (let [fresh (original-projection source)]
                 (original-projection-check fresh source))
               (catch Exception _
                 (reject! :invalid-or-stale-physical-projection [:source])))]
    (when-not (and (= projection/projection-type (:type r))
                   (= projection/projection-version (:version r))
                   (= :declarations-only (:proof-status r))
                   (= :not-issued (:certificate r))
                   (= :none (:authority r))
                   (= :pinned-api-and-gesso-option-surface
                      (:compatibility (original-capability))))
      (reject! :unsupported-projection-or-backend []))
    r))

(defn- checked-targets! [source projection-report]
  (let [protected (get-in source [:protected-domain-source :protected-documents])
        locations (:field-locations projection-report)
        mapped (reduce-kv (fn [acc abstract-field {:keys [target physical-field]}]
                            (assoc-in acc [target physical-field] abstract-field))
                          {} locations)]
    (when-not (and (vector? protected)
                   (<= 1 (count protected) max-targets)
                   (= (set (map :target protected))
                      (:declared-protected-targets projection-report)))
      (reject! :unsupported-protected-target-count [:source]))
    (mapv (fn [i {:keys [target fields]}]
            (let [[table id] target
                  _ (sql-table! table i)
                  _ (when-not (supported-id? id)
                      (reject! :unsupported-target-id [:targets i]))
                  abstract-fields (get mapped target {})
                  projected-fields (set (keys abstract-fields))]
              (when-not (and (or (= :all fields) (set? fields))
                             (or (= :all fields)
                                 (set/subset? projected-fields fields)))
                (reject! :projection-outside-protected-fields [:targets i]))
              {:target target :fields fields :abstract-fields abstract-fields}))
          (range) protected)))

(defn- snapshot! [node]
  (let [rows (try (original-query node "SHOW SNAPSHOT_TOKEN")
                  (catch Throwable _
                    (reject! :snapshot-token-read-failed [:snapshot-token])))
        token (when (and (sequential? rows) (= 1 (count rows))
                         (plain-map? (first rows))
                         (= #{:snapshot-token} (set (keys (first rows)))))
                (:snapshot-token (first rows)))]
    (when-not (and (string? token) (seq token)
                   (try (map? (original-decode token))
                        (catch Throwable _ false)))
      (reject! :invalid-snapshot-token [:snapshot-token]))
    {:snapshot-token token :current-time (Instant/now)}))

(defn- read-target! [node basis i {:keys [target fields abstract-fields]}]
  (let [[table id] target
        sql (str "SELECT * FROM " (sql-table! table i)
                 " WHERE _id = ? LIMIT 2")
        rows (try (original-query node [sql id] basis)
                  (catch Throwable _ (reject! :target-read-failed [:targets i])))]
    (when-not (and (sequential? rows) (<= (count rows) 1))
      (reject! :ambiguous-target-read [:targets i]))
    ;; Compare the keys XTDB actually returns. XTDB can normalize SQL
    ;; identifiers (e.g. created_at -> :created-at); never guess aliases.
    (let [document (first rows)
          required (set (keys abstract-fields))]
      (when-not (or (nil? document)
                    (and (plain-map? document)
                         (<= 1 (count document) max-row-fields)
                         (every? keyword? (keys document))
                         (or (not (contains? document :xt/id))
                             (= (:xt/id document) id))))
        (reject! :invalid-or-contradictory-physical-row [:targets i]))
      (when (and (seq required) (nil? document))
        (reject! :missing-mapped-document [:targets i]))
      (let [physical-fields (disj (set (keys document)) :xt/id)
            missing (set/difference required physical-fields)
            protected-observed (if (= :all fields)
                                 physical-fields
                                 (set/intersection physical-fields fields))
            unexpected (set/difference protected-observed required)
            unprotected (if (= :all fields)
                          #{}
                          (set/difference physical-fields fields))]
        (when (seq missing)
          (reject! :missing-mapped-physical-field [:targets i]))
        (when (seq unexpected)
          (reject! :unexpected-protected-physical-field [:targets i]))
        {:target target
         :document document
         :mapped-values (into {}
                              (map (fn [[field abstract]]
                                     [abstract (get document field)]))
                              abstract-fields)
         :mapped-field-count (count required)
         :unprotected-observed-field-count (count unprotected)
         :protection-mode (if (= :all fields) :whole-document :declared-fields)
         :observed-document-status (if document :present :absent)}))))

(defn acquire-physical-domain!
  "Read every v1491 protected target using a single externally pinned XTDB
   basis. Input is the original v1491 source map (no separate field registry).

   All mapped fields must be present. Additional physical fields within the
   declared protected domain are rejected, including any extra top-level
   field when :fields is :all. Unprotected fields on partially protected
   documents do not count as mapped or certified; their counts are reported.
   An unmapped, absent protected target is allowed as a frame-only observation.

   This function performs ONLY xtdb.api/q reads. Source and rows are sensitive.
   A successful result is a bounded external observation, NOT an authenticated
   transaction prestate, state refinement, or complete-writer certificate."
  [node source]
  (when-not (some? node)
    (reject! :missing-observation-node [:node]))
  (let [projection-report (checked-source! source)
        entries (checked-targets! source projection-report)
        basis (snapshot! node)
        observed (mapv (fn [i entry] (read-target! node basis i entry))
                       (range) entries)
        abstract-values (reduce (fn [m entry]
                                  (merge m (:mapped-values entry)))
                                {} observed)
        mapped (set (keys (:field-locations projection-report)))]
    (when-not (= mapped (set (keys abstract-values)))
      (reject! :incomplete-abstract-field-observation []))
    {:type acquisition-type
     :version acquisition-version
     :checking :fresh-v1491-projection-and-one-pinned-external-read
     :scope :finite-declared-protected-documents-only
     :source source
     :declared-projection-fingerprint (:obligation-fingerprint projection-report)
     :external-read-basis basis
     :observed-rows observed
     :abstract-physical-values abstract-values
     :observed-protected-target-count (count observed)
     :observed-mapped-field-count (count abstract-values)
     :observed-absent-unmapped-target-count
     (count (filter #(= :absent (:observed-document-status %)) observed))
     :observed-unprotected-field-count
     (reduce + (map :unprotected-observed-field-count observed))
     :unmapped-protected-targets (:unmapped-protected-targets projection-report)
     :status :bounded-pinned-external-field-observation
     :read-node-provenance :caller-supplied-unattested
     :read-basis-authenticity :not-established
     :physical-value-and-type-translation :not-established
     :transaction-internal-prestate :not-authenticated
     :complete-writer-coverage :not-established
     :physical-frame-preservation :not-established
     :universal-physical-refinement :not-established
     :proof-status :external-read-diagnostic-only
     :certificate :not-issued
     :admission :not-admitted
     :transaction-authority :none
     :retry-authority :none
     :settlement-authority :none
     :authority :none}))

(defn redacted-summary
  "Return diagnostic counts and open assumptions, omitting IDs, field names,
   raw values, source, token, and SQL. Does not authenticate stored rows."
  [report]
  (when-not (and (plain-map? report)
                 (= acquisition-type (:type report))
                 (= acquisition-version (:version report))
                 (= :external-read-diagnostic-only (:proof-status report))
                 (= :not-issued (:certificate report))
                 (= :none (:authority report)))
    (reject! :invalid-acquisition-report []))
  (select-keys report
               [:type :version :checking :scope
                :observed-protected-target-count :observed-mapped-field-count
                :observed-absent-unmapped-target-count
                :observed-unprotected-field-count
                :status :read-node-provenance :read-basis-authenticity
                :physical-value-and-type-translation
                :transaction-internal-prestate :complete-writer-coverage
                :physical-frame-preservation :universal-physical-refinement
                :proof-status :certificate :admission :transaction-authority
                :retry-authority :settlement-authority :authority]))
