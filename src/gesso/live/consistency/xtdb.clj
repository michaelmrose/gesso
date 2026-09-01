(ns gesso.live.consistency.xtdb
  "XTDB2 consistency helpers for gesso.live.

   This namespace is deliberately narrow.

   It provides:

   - XTDB2 connectable/context helpers
   - explicit read-consistency/query option helpers
   - thin q/plan-q task wrappers
   - thin submit-tx/execute-tx task wrappers
   - portable XTDB-backed authoritative progression bases/requirements
   - trusted XTDB progression comparison and query-option translation
   - opaque consistency-token encoding for optional transport/event metadata
   - small tx-op constructors

   It does not:

   - listen to XTDB transaction logs
   - use XTDB v1 tx listeners
   - use open-tx-log
   - publish live invalidations
   - depend on gesso.live.core
   - decide what changed in the application
   - expose or use mutable await-token-source helpers
   - call .setAwaitToken or .getAwaitToken

   Important XTDB2 behavior:

   XTDB's own public submit-tx/execute-tx mutate a DataSource's await-token
   internally when the connectable is a DataSource. XTDB q/plan-q also use a
   DataSource's current await-token as the default query await-token.

   This namespace does not add another mutable consistency layer. For
   request-specific read-after-write behavior, pass explicit immutable
   consistency data to q-consistent / plan-q-consistent.

   execute-tx! is the preferred write helper when the immediate live fragment
   must read at/after the write, because XTDB2 execute-tx returns a
   TransactionKey containing tx-id and system-time. We derive :snapshot-time
   from that system-time for per-query consistency.

   submit-tx! returns only the public metadata available from XTDB submit-tx,
   currently :tx-id. That is useful metadata, but not a complete per-query read
   basis by itself."
  (:require
   [clojure.string :as str]
   [gesso.live.progression :as progression]
   [missionary.core :as m]
   [xtdb.api :as xt]
   [xtdb.basis :as xt.basis])
  (:import
   [java.time Instant]
   [xtdb.api TransactionKey]))

;; -----------------------------------------------------------------------------
;; Internal XTDB call seams
;; -----------------------------------------------------------------------------

(def ^:private ^:dynamic *q*
  xt/q)

(def ^:private ^:dynamic *plan-q*
  xt/plan-q)

(def ^:private ^:dynamic *submit-tx*
  xt/submit-tx)

(def ^:private ^:dynamic *execute-tx*
  xt/execute-tx)

;; -----------------------------------------------------------------------------
;; Debug
;; -----------------------------------------------------------------------------

(defmacro ^:private debug!
  "Emit a debug event only when debug-fn is truthy.

   The data expression is inside the guard, so with debugging off it is not
   evaluated."
  [debug-fn event data]
  `(when-let [f# ~debug-fn]
     (f# (assoc ~data :event ~event))))

;; -----------------------------------------------------------------------------
;; Constants/defaults
;; -----------------------------------------------------------------------------

(def query-consistency-keys
  [:await-token
   :snapshot-token
   :snapshot-time
   :current-time
   :default-tz])

(def tx-metadata-keys
  [:tx-id
   :system-time])

(def tx-consistency-keys
  (vec
   (distinct
    (concat tx-metadata-keys
            query-consistency-keys))))

(def query-option-keys
  [:await-token
   :snapshot-token
   :snapshot-time
   :current-time
   :default-tz
   :key-fn
   :database])

(def tx-option-keys
  [:database
   :system-time
   :default-tz
   :metadata
   :authn])

(def adapter-option-keys
  [:debug-fn])

(def default-options
  {:debug-fn nil})

;; -----------------------------------------------------------------------------
;; XTDB-backed authoritative progression
;; -----------------------------------------------------------------------------

(def xtdb-basis-version 1)

(def xtdb-basis-type
  :gesso.live.consistency.xtdb/basis)

(def default-database-name
  "XTDB's default database name used by basis/snapshot-token construction."
  "xtdb")

(def xtdb-basis-keys
  #{:gesso.live.consistency.xtdb/type
    :gesso.live.consistency.xtdb/version
    :database
    :tx-id
    :snapshot-token})

(declare normalize-consistency tx-result-consistency consistent-query-opts)

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn- now-ms
  []
  (System/currentTimeMillis))

(defn- ex
  ([message data]
   (ex-info message data))
  ([message data cause]
   (ex-info message data cause)))

(defn- compact-map
  [m]
  (into {}
        (remove (comp nil? val))
        m))

(defn- select-compact
  [m ks]
  (compact-map (select-keys (or m {}) ks)))

(defn- require-fn-option!
  [options k]
  (when-let [f (get options k)]
    (when-not (fn? f)
      (throw
       (ex (str "gesso.live XTDB consistency " k " must be a function.")
           {k f}))))
  options)

(defn prepare-options!
  "Merge defaults and validate adapter-only options.

   XTDB query/tx opts are extracted separately and are not validated here."
  [options]
  (let [options' (merge default-options
                        (select-keys (or options {}) adapter-option-keys))]
    (require-fn-option! options' :debug-fn)
    options'))

(defn require-connectable!
  [connectable]
  (when-not connectable
    (throw
     (ex "Missing XTDB2 connectable."
         {:expected-one-of [:xtdb/connectable
                            :xtdb/read-connectable
                            :xtdb/conn
                            :xtdb/node
                            :biff.xtdb/node
                            :biff/conn
                            :biff/node]})))
  connectable)

(defn- transaction-key?
  [x]
  (instance? TransactionKey x))

(defn- transaction-key->map
  [tx-key]
  {:tx-id (.getTxId ^TransactionKey tx-key)
   :system-time (.getSystemTime ^TransactionKey tx-key)})

(defn- tx-result-map
  "Return a plain map view of known public XTDB tx result shapes.

   XTDB submit-tx returns a map such as {:tx-id ...}.
   XTDB execute-tx returns a TransactionKey with tx-id/system-time."
  [tx-result]
  (cond
    (map? tx-result)
    tx-result

    (transaction-key? tx-result)
    (transaction-key->map tx-result)

    :else
    {}))

;; -----------------------------------------------------------------------------
;; Portable XTDB authoritative bases
;; -----------------------------------------------------------------------------

(defn- normalize-database-name
  [database]
  (cond
    (nil? database)
    default-database-name

    (keyword? database)
    (name database)

    (and (string? database)
         (not (str/blank? database)))
    database

    :else
    (throw
     (ex "XTDB progression database identity must be a keyword or non-blank string."
         {:database database}))))

(defn- tx-id->wire
  [value]
  (when-not (and (integer? value)
                 (<= 0 value))
    (throw
     (ex "XTDB progression tx-id must be a non-negative integer."
         {:tx-id value})))
  (str (long value)))

(defn- wire->tx-id
  [value]
  (when-not (string? value)
    (throw
     (ex "XTDB progression wire tx-id must be a decimal string."
         {:tx-id value})))
  (try
    (let [tx-id (Long/parseLong value)]
      (when (neg? tx-id)
        (throw
         (ex "XTDB progression wire tx-id must be non-negative."
             {:tx-id value})))
      tx-id)
    (catch clojure.lang.ExceptionInfo e
      (throw e))
    (catch Throwable e
      (throw
       (ex "Invalid XTDB progression wire tx-id."
           {:tx-id value}
           e)))))

(defn- snapshot-token-for
  [database-name system-time]
  (when-not (instance? Instant system-time)
    (throw
     (ex "XTDB progression system-time must be java.time.Instant."
         {:system-time system-time})))
  (xt.basis/->time-basis-str
   {database-name [system-time]}))

(defn- snapshot-token-system-time
  [database-name snapshot-token]
  (when-not (and (string? snapshot-token)
                 (not (str/blank? snapshot-token)))
    (throw
     (ex "XTDB progression snapshot-token must be a non-blank string."
         {:snapshot-token snapshot-token})))
  (try
    (let [decoded (xt.basis/<-time-basis-str snapshot-token)
          expected-keys #{database-name}
          actual-keys (set (keys decoded))
          times (get decoded database-name)]
      (when-not (= expected-keys actual-keys)
        (throw
         (ex "XTDB progression snapshot-token must identify exactly one database."
             {:database database-name
              :token-databases actual-keys})))
      (when-not (and (= 1 (count times))
                     (instance? Instant (first times)))
        (throw
         (ex "XTDB progression snapshot-token has an invalid time basis."
             {:database database-name
              :times times})))
      (first times))
    (catch clojure.lang.ExceptionInfo e
      (throw e))
    (catch Throwable e
      (throw
       (ex "Invalid XTDB progression snapshot-token."
           {:database database-name
            :snapshot-token snapshot-token}
           e)))))

(defn xtdb-basis?
  "True for the closed portable XTDB basis representation owned here.

   tx-id is encoded as decimal text so a CLJS carrier cannot lose 64-bit
   precision. snapshot-token is XTDB's own portable time-basis token. Generic
   progression/browser code treats the whole basis map as opaque."
  [value]
  (and
   (map? value)
   (= xtdb-basis-keys (set (keys value)))
   (= xtdb-basis-type
      (:gesso.live.consistency.xtdb/type value))
   (= xtdb-basis-version
      (:gesso.live.consistency.xtdb/version value))
   (string? (:database value))
   (try
     (wire->tx-id (:tx-id value))
     (snapshot-token-system-time
      (:database value)
      (:snapshot-token value))
     true
     (catch Throwable _
       false))))

(defn require-xtdb-basis!
  [value]
  (when-not (xtdb-basis? value)
    (throw
     (ex "Expected a closed portable XTDB authoritative basis."
         {:basis value})))
  value)

(defn basis
  "Construct one portable XTDB AuthoritativeBasis from committed tx metadata.

   TransactionKey/Instant remain server-side. The resulting map contains only
   portable text plus closed type/version identity."
  ([tx-id system-time]
   (basis nil tx-id system-time))
  ([database tx-id system-time]
   (let [database-name (normalize-database-name database)]
     {:gesso.live.consistency.xtdb/type xtdb-basis-type
      :gesso.live.consistency.xtdb/version xtdb-basis-version
      :database database-name
      :tx-id (tx-id->wire tx-id)
      :snapshot-token (snapshot-token-for database-name system-time)})))

(defn basis-tx-id
  "Return the JVM long transaction id encoded by basis-value."
  [basis-value]
  (wire->tx-id (:tx-id (require-xtdb-basis! basis-value))))

(defn basis-database
  [basis-value]
  (:database (require-xtdb-basis! basis-value)))

(defn basis-snapshot-token
  [basis-value]
  (:snapshot-token (require-xtdb-basis! basis-value)))

(defn basis-system-time
  "Decode the XTDB snapshot token back to its single database system-time."
  [basis-value]
  (let [basis' (require-xtdb-basis! basis-value)]
    (snapshot-token-system-time
     (:database basis')
     (:snapshot-token basis'))))

(defn consistency->basis
  "Translate complete XTDB consistency metadata into a portable basis.

   Returns nil for incomplete consistency. A basis requires both :tx-id and a
   transaction/snapshot time. tx-id alone (the public submit-tx result) cannot
   establish a query requirement this adapter can later honor."
  ([consistency]
   (consistency->basis consistency nil))
  ([consistency database]
   (let [{:keys [tx-id system-time snapshot-time]}
         (normalize-consistency consistency)
         time (or system-time snapshot-time)]
     (when (and (some? tx-id)
                (some? time))
       (basis database tx-id time)))))

(defn tx-result-basis
  "Translate a complete public XTDB tx result into a portable basis, or nil.

   execute-tx TransactionKey results are complete. submit-tx's tx-id-only map
   intentionally returns nil."
  ([tx-result]
   (tx-result-basis tx-result nil))
  ([tx-result database]
   (consistency->basis
    (tx-result-consistency tx-result)
    database)))

(defn tx-result-progression
  "Return a singleton generic progression requirement established by tx-result,
   or nil when the public XTDB result lacks enough basis data."
  ([tx-result]
   (tx-result-progression tx-result nil))
  ([tx-result database]
   (some-> (tx-result-basis tx-result database)
           progression/requirement)))

(defn- same-database!
  [left right]
  (let [left-db (basis-database left)
        right-db (basis-database right)]
    (when-not (= left-db right-db)
      (throw
       (ex "XTDB progression bases from different databases are incomparable."
           {:left-database left-db
            :right-database right-db
            :left left
            :right right})))
    left-db))

(defn compare-bases
  "Trusted XTDB comparison for portable bases from one database.

   XTDB 2.2.0-beta1 TransactionKey Comparable ordering is tx-id ordering. This
   adapter is the trusted storage-specific layer allowed to use that fact.
   Equal tx-id with different basis payload is rejected as conflicting evidence."
  [left right]
  (require-xtdb-basis! left)
  (require-xtdb-basis! right)
  (same-database! left right)
  (let [comparison (compare (basis-tx-id left)
                            (basis-tx-id right))]
    (when (and (zero? comparison)
               (not= left right))
      (throw
       (ex "Conflicting XTDB progression bases share one tx-id."
           {:left left
            :right right})))
    comparison))

(defn basis-advances?
  "True when right is strictly later than left in the same XTDB transaction log."
  [left right]
  (neg? (compare-bases left right)))

(defn progression-witness
  "Return trusted generic :advances evidence for left -> right.

   Equal bases need no witness and return nil. Backward movement is rejected."
  [left right]
  (let [comparison (compare-bases left right)]
    (cond
      (neg? comparison)
      (progression/advance left right)

      (zero? comparison)
      nil

      :else
      (throw
       (ex "XTDB progression cannot witness a backward basis transition."
           {:from left
            :to right})))))

(defn strongest-required-basis
  "Collapse one generic requirement using trusted XTDB tx ordering.

   Generic Live preserves incomparable bases. Once the requirement reaches this
   XTDB adapter, bases from one database are safely reducible to the greatest
   transaction id. Mixed/non-XTDB bases fail closed."
  [requirement-value]
  (when-some [requirement'
              (progression/normalize-requirement requirement-value)]
    (reduce
     (fn [strongest candidate]
       (require-xtdb-basis! candidate)
       (if (nil? strongest)
         candidate
         (if (neg? (compare-bases strongest candidate))
           candidate
           strongest)))
     nil
     (progression/required-bases requirement'))))

(defn progression-consistency
  "Translate a generic XTDB progression requirement into explicit consistency."
  [requirement-value]
  (when-some [required (strongest-required-basis requirement-value)]
    (let [system-time (basis-system-time required)]
      {:tx-id (basis-tx-id required)
       :system-time system-time
       :snapshot-time system-time
       :snapshot-token (basis-snapshot-token required)})))

(defn progression-query-opts
  "Translate progression into XTDB query opts using XTDB's snapshot token.

   The token fixes the read to a basis known to include the strongest required
   transaction. Non-default database identity is carried explicitly."
  [requirement-value]
  (when-some [required (strongest-required-basis requirement-value)]
    (cond-> {:snapshot-token (basis-snapshot-token required)}
      (not= default-database-name (basis-database required))
      (assoc :database (basis-database required)))))

(defn- enforce-progression-query-opts
  [opts requirement-value]
  (if-some [required (strongest-required-basis requirement-value)]
    (let [required-db (basis-database required)
          explicit-db (when (contains? opts :database)
                        (normalize-database-name (:database opts)))]
      (when (and explicit-db
                 (not= explicit-db required-db))
        (throw
         (ex "Explicit XTDB query database conflicts with authoritative progression."
             {:explicit-database (:database opts)
              :required-database required-db})))
      ;; Progression is a correctness requirement, not an ordinary preference.
      ;; Remove caller snapshot coordinates and install the exact required XTDB
      ;; token. Preserve a compatible explicit database representation; otherwise
      ;; add the required non-default database.
      (cond-> (-> opts
                  (dissoc :snapshot-time :snapshot-token)
                  (assoc :snapshot-token (basis-snapshot-token required)))
        (and (not explicit-db)
             (not= default-database-name required-db))
        (assoc :database required-db)))
    opts))

(defn progression-from
  "Return optional generic progression from a request/context map.

   :gesso.live/progression is canonical. :progression is accepted as a narrow
   request-local convenience while the public Live facade is migrated."
  [x]
  (when (map? x)
    (progression/normalize-requirement
     (or (:gesso.live/progression x)
         (:progression x)))))

(defn read-query-opts
  "Build query opts from explicit consistency, optional authoritative progression,
   and caller opts. Progression constraints are applied last and cannot be
   weakened by caller query options."
  ([consistency progression-value]
   (read-query-opts consistency progression-value nil))
  ([consistency progression-value opts]
   (-> (consistent-query-opts consistency opts)
       (enforce-progression-query-opts progression-value))))

;; -----------------------------------------------------------------------------
;; Context helpers
;; -----------------------------------------------------------------------------

(defn connectable-from
  "Return a general XTDB2 connectable from either a raw connectable or a context
   map.

   Preferred context keys:

     :xtdb/connectable
     :xtdb/conn
     :xtdb/node

   Biff-compatible fallback keys:

     :biff.xtdb/node
     :biff/conn
     :biff/node"
  [x]
  (if (map? x)
    (or (:xtdb/connectable x)
        (:xtdb/conn x)
        (:xtdb/node x)
        (:biff.xtdb/node x)
        (:biff/conn x)
        (:biff/node x))
    x))

(defn read-connectable-from
  "Return the preferred XTDB2 connectable for live/read-after-write queries.

   Prefer explicit read connectables and request-scoped connections first.
   Shared DataSource/node values are valid XTDB connectables, but XTDB q/plan-q
   may implicitly use their shared await-token state.

   Preferred context keys:

     :xtdb/read-connectable
     :xtdb/conn
     :biff/conn
     :xtdb/connectable
     :xtdb/node
     :biff.xtdb/node
     :biff/node"
  [x]
  (if (map? x)
    (or (:xtdb/read-connectable x)
        (:xtdb/conn x)
        (:biff/conn x)
        (:xtdb/connectable x)
        (:xtdb/node x)
        (:biff.xtdb/node x)
        (:biff/node x))
    x))

;; -----------------------------------------------------------------------------
;; Consistency maps and query options
;; -----------------------------------------------------------------------------

(defn normalize-consistency
  "Normalize a user/app consistency map.

   Accepted keys:

     Query consistency:
       :await-token
       :snapshot-token
       :snapshot-time
       :current-time
       :default-tz

     Transaction metadata:
       :tx-id
       :system-time

   Extra keys are discarded.

   :tx-id and :system-time are preserved as metadata, but only
   query-consistency keys are passed to XTDB query opts."
  [consistency]
  (select-compact consistency tx-consistency-keys))

(defn consistency-from
  "Return explicit consistency from a context map.

   Context precedence:

     :gesso.live/consistency
     :gesso.live.xtdb/consistency
     :xtdb/consistency
     :consistency

   This function intentionally does not inspect or mutate XTDB DataSource/node
   state. A context without explicit consistency returns an empty map."
  [x]
  (normalize-consistency
   (when (map? x)
     (or (:gesso.live/consistency x)
         (:gesso.live.xtdb/consistency x)
         (:xtdb/consistency x)
         (:consistency x)))))

(defn query-consistency
  "Return only consistency fields that XTDB2 query opts can use."
  [consistency]
  (select-compact (normalize-consistency consistency)
                  query-consistency-keys))

(defn query-opts
  "Build XTDB2 query opts from explicit query opts only.

   This is intentionally plain. For consistency-aware reads, use
   consistent-query-opts or q-consistent."
  [opts]
  (select-compact opts query-option-keys))

(defn consistent-query-opts
  "Build XTDB2 query opts from consistency plus explicit opts.

   Consistency-derived values are applied first. Explicit opts win.

   This is the only place this namespace applies read consistency. It is
   per-query data, not mutation on a shared XTDB object."
  ([consistency]
   (consistent-query-opts consistency nil))
  ([consistency opts]
   (merge
    (query-consistency consistency)
    (query-opts opts))))

(defn tx-opts
  "Return only options intended for XTDB2 submit-tx/execute-tx.

   Adapter-only options such as :debug-fn are removed."
  [opts]
  (select-compact opts tx-option-keys))

(defn consistency-token
  "Return an opaque stable token for normalized XTDB read consistency.

   This token is optional transport/event metadata. It is not an authoritative
   progression requirement and must not be used to partition fragment cache or
   singleflight identity. Fragment freshness is represented by canonical
   gesso.live.progression requirements instead.

   Returns nil when the normalized consistency map is empty."
  [consistency]
  (let [c (normalize-consistency consistency)]
    (when (seq c)
      [:xtdb2/read-consistency c])))

;; -----------------------------------------------------------------------------
;; Query wrappers
;; -----------------------------------------------------------------------------

(defn q
  "Plain thin XTDB2 q wrapper.

   Arity mirrors xtdb.api/q. It does not reinterpret the third argument as a
   consistency map. For consistency-aware reads, use q-consistent."
  ([connectable query]
   (*q* (require-connectable! connectable) query))
  ([connectable query opts]
   (*q* (require-connectable! connectable)
        query
        (query-opts opts))))

(defn q-from
  "Run plain q against a raw connectable or context map.

   This uses connectable-from. For live fragment reads, prefer
   q-consistent-from."
  ([ctx-or-connectable query]
   (q (connectable-from ctx-or-connectable) query))
  ([ctx-or-connectable query opts]
   (q (connectable-from ctx-or-connectable) query opts)))

(defn q-consistent
  "Run XTDB2 q with explicit consistency.

   This avoids the ambiguous map arity problem of trying to make q's third
   argument mean both XTDB opts and consistency."
  ([connectable query consistency]
   (q-consistent connectable query consistency nil))
  ([connectable query consistency opts]
   (*q* (require-connectable! connectable)
        query
        (consistent-query-opts consistency opts))))

(defn q-consistent-from
  "Run a request-scoped consistency/progression-aware XTDB2 query.

   Explicit consistency remains supported. When ctx carries
   :gesso.live/progression, the XTDB adapter translates it into a required
   snapshot/database and applies that requirement after ordinary query opts so
   callers cannot weaken an authoritative refresh requirement."
  ([ctx-or-connectable query]
   (q-consistent-from ctx-or-connectable query nil))
  ([ctx-or-connectable query opts]
   (*q* (require-connectable! (read-connectable-from ctx-or-connectable))
        query
        (read-query-opts
         (consistency-from ctx-or-connectable)
         (progression-from ctx-or-connectable)
         opts))))

(defn q-task
  "Return a Missionary task that runs plain q on m/blk."
  ([connectable query]
   (m/via-call m/blk
               #(q connectable query)))
  ([connectable query opts]
   (m/via-call m/blk
               #(q connectable query opts))))

(defn q-consistent-task
  "Return a Missionary task that runs q-consistent on m/blk."
  ([connectable query consistency]
   (m/via-call m/blk
               #(q-consistent connectable query consistency)))
  ([connectable query consistency opts]
   (m/via-call m/blk
               #(q-consistent connectable query consistency opts))))

(defn q-consistent-task-from
  "Return a Missionary task that runs q-consistent-from on m/blk."
  ([ctx-or-connectable query]
   (m/via-call m/blk
               #(q-consistent-from ctx-or-connectable query)))
  ([ctx-or-connectable query opts]
   (m/via-call m/blk
               #(q-consistent-from ctx-or-connectable query opts))))

(defn plan-q
  "Plain thin XTDB2 plan-q wrapper.

   Returns XTDB2's reducible result. This is useful for large result sets.
   This function does not realize the result set."
  ([connectable query]
   (*plan-q* (require-connectable! connectable) query))
  ([connectable query opts]
   (*plan-q* (require-connectable! connectable)
             query
             (query-opts opts))))

(defn plan-q-from
  "Run plain plan-q against a raw connectable or context map."
  ([ctx-or-connectable query]
   (plan-q (connectable-from ctx-or-connectable) query))
  ([ctx-or-connectable query opts]
   (plan-q (connectable-from ctx-or-connectable) query opts)))

(defn plan-q-consistent
  "Run XTDB2 plan-q with explicit consistency."
  ([connectable query consistency]
   (plan-q-consistent connectable query consistency nil))
  ([connectable query consistency opts]
   (*plan-q* (require-connectable! connectable)
             query
             (consistent-query-opts consistency opts))))

(defn plan-q-consistent-from
  "Run request-scoped plan-q with explicit consistency and optional progression."
  ([ctx-or-connectable query]
   (plan-q-consistent-from ctx-or-connectable query nil))
  ([ctx-or-connectable query opts]
   (*plan-q* (require-connectable! (read-connectable-from ctx-or-connectable))
             query
             (read-query-opts
              (consistency-from ctx-or-connectable)
              (progression-from ctx-or-connectable)
              opts))))

;; -----------------------------------------------------------------------------
;; Transaction result normalization
;; -----------------------------------------------------------------------------

(defn tx-result-consistency
  "Extract consistency-relevant fields from an XTDB2 transaction result.

   Known public XTDB2 shapes:

   - submit-tx returns a map with :tx-id.
   - execute-tx returns a TransactionKey with tx-id/system-time.

   When system-time is present, this helper also derives :snapshot-time. XTDB2
   q/plan-q support :snapshot-time as a per-query option, so execute-tx! results
   can be used for per-query read-after-write consistency without manually
   mutating DataSource await-token state."
  [tx-result]
  (let [{:keys [tx-id system-time]} (tx-result-map tx-result)]
    (compact-map
     {:tx-id tx-id
      :system-time system-time
      :snapshot-time system-time})))

(defn tx-consistency
  "Build a consistency map from a tx result.

   The second arity is intentionally not provided. There is no await-token-source
   argument because this namespace does not inspect shared XTDB state."
  [tx-result]
  (tx-result-consistency tx-result))

;; -----------------------------------------------------------------------------
;; Transaction wrappers
;; -----------------------------------------------------------------------------

(defn submit-tx!
  "Submit XTDB2 tx ops.

   Returns:

     {:tx-result ...
      :consistency ...}

   XTDB public submit-tx returns only :tx-id. The returned consistency is
   therefore metadata only unless XTDB changes that public result shape.

   Options may include XTDB2 submit-tx opts plus adapter opts:

     :debug-fn
       Optional pay-for-play debug hook.

   This function does not emit live invalidations and does not manually mutate
   shared XTDB consistency state."
  ([connectable tx-ops]
   (submit-tx! connectable tx-ops nil))
  ([connectable tx-ops opts]
   (let [connectable'     (require-connectable! connectable)
         adapter-options  (prepare-options! opts)
         debug-fn         (:debug-fn adapter-options)
         tx-options       (tx-opts opts)]
     (debug!
      debug-fn
      :gesso.live.xtdb/submit-tx-started
      {:tx-opts tx-options
       :at (now-ms)})
     (try
       (let [tx-result   (*submit-tx* connectable' tx-ops tx-options)
             consistency (tx-consistency tx-result)
             progression-value
             (tx-result-progression tx-result (:database tx-options))]
         (debug!
          debug-fn
          :gesso.live.xtdb/submit-tx-succeeded
          (cond-> {:tx-result tx-result
                   :consistency consistency
                   :at (now-ms)}
            progression-value
            (assoc :progression progression-value)))
         (cond-> {:tx-result tx-result
                  :consistency consistency}
           progression-value
           (assoc :progression progression-value)))
       (catch Throwable e
         (debug!
          debug-fn
          :gesso.live.xtdb/submit-tx-failed
          {:error e
           :at (now-ms)})
         (throw e))))))

(defn submit-tx-from!
  "Submit tx ops using a context/raw connectable.

   Uses connectable-from for the write. Returned consistency is extracted only
   from the public tx result."
  ([ctx-or-connectable tx-ops]
   (submit-tx-from! ctx-or-connectable tx-ops nil))
  ([ctx-or-connectable tx-ops opts]
   (submit-tx! (connectable-from ctx-or-connectable)
               tx-ops
               opts)))

(defn execute-tx!
  "Execute XTDB2 tx ops and wait for the receiving node to index them.

   Returns:

     {:tx-result ...
      :consistency ...
      :progression ...} ; when complete basis data is available

   XTDB public execute-tx returns a TransactionKey containing tx-id and
   system-time. The returned consistency includes:

     :tx-id
     :system-time
     :snapshot-time

   :snapshot-time is derived from :system-time and can be passed to q-consistent
   / plan-q-consistent as per-query consistency data.

   Options may include XTDB2 execute-tx opts plus adapter opts:

     :debug-fn
       Optional pay-for-play debug hook.

   This function does not emit live invalidations and does not manually mutate
   shared XTDB consistency state."
  ([connectable tx-ops]
   (execute-tx! connectable tx-ops nil))
  ([connectable tx-ops opts]
   (let [connectable'     (require-connectable! connectable)
         adapter-options  (prepare-options! opts)
         debug-fn         (:debug-fn adapter-options)
         tx-options       (tx-opts opts)]
     (debug!
      debug-fn
      :gesso.live.xtdb/execute-tx-started
      {:tx-opts tx-options
       :at (now-ms)})
     (try
       (let [tx-result   (*execute-tx* connectable' tx-ops tx-options)
             consistency (tx-consistency tx-result)
             progression-value
             (tx-result-progression tx-result (:database tx-options))]
         (debug!
          debug-fn
          :gesso.live.xtdb/execute-tx-succeeded
          (cond-> {:tx-result tx-result
                   :consistency consistency
                   :at (now-ms)}
            progression-value
            (assoc :progression progression-value)))
         (cond-> {:tx-result tx-result
                  :consistency consistency}
           progression-value
           (assoc :progression progression-value)))
       (catch Throwable e
         (debug!
          debug-fn
          :gesso.live.xtdb/execute-tx-failed
          {:error e
           :at (now-ms)})
         (throw e))))))

(defn execute-tx-from!
  "Execute tx ops using a context/raw connectable.

   Uses connectable-from for the write. Returned consistency is extracted only
   from the public tx result."
  ([ctx-or-connectable tx-ops]
   (execute-tx-from! ctx-or-connectable tx-ops nil))
  ([ctx-or-connectable tx-ops opts]
   (execute-tx! (connectable-from ctx-or-connectable)
                tx-ops
                opts)))

(defn submit-tx-task
  "Return a Missionary task that runs submit-tx! on m/blk."
  ([connectable tx-ops]
   (m/via-call m/blk
               #(submit-tx! connectable tx-ops)))
  ([connectable tx-ops opts]
   (m/via-call m/blk
               #(submit-tx! connectable tx-ops opts))))

(defn execute-tx-task
  "Return a Missionary task that runs execute-tx! on m/blk."
  ([connectable tx-ops]
   (m/via-call m/blk
               #(execute-tx! connectable tx-ops)))
  ([connectable tx-ops opts]
   (m/via-call m/blk
               #(execute-tx! connectable tx-ops opts))))

;; -----------------------------------------------------------------------------
;; Small XTDB2 tx-op helpers
;; -----------------------------------------------------------------------------

(defn put-docs-op
  "Build an XTDB2 :put-docs tx op."
  [table & docs]
  (when-not table
    (throw
     (ex "Missing XTDB2 table for :put-docs."
         {:table table})))
  (when-not (seq docs)
    (throw
     (ex "Missing XTDB2 docs for :put-docs."
         {:table table})))
  (into [:put-docs table] docs))

(defn delete-docs-op
  "Build an XTDB2 :delete-docs tx op."
  [table & doc-ids]
  (when-not table
    (throw
     (ex "Missing XTDB2 table for :delete-docs."
         {:table table})))
  (when-not (seq doc-ids)
    (throw
     (ex "Missing XTDB2 doc ids for :delete-docs."
         {:table table})))
  (into [:delete-docs table] doc-ids))

(defn put-doc!
  "Convenience wrapper around execute-tx! for one document.

   This waits for the receiving node to index the tx.

   It does not publish live invalidations."
  ([connectable table doc]
   (put-doc! connectable table doc nil))
  ([connectable table doc opts]
   (execute-tx! connectable [(put-docs-op table doc)] opts)))

(defn put-doc-from!
  "Convenience wrapper around execute-tx-from! for one document/context."
  ([ctx-or-connectable table doc]
   (put-doc-from! ctx-or-connectable table doc nil))
  ([ctx-or-connectable table doc opts]
   (execute-tx-from! ctx-or-connectable
                     [(put-docs-op table doc)]
                     opts)))