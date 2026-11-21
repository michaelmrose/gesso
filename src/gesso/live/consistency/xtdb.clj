(ns gesso.live.consistency.xtdb
  "XTDB2 consistency helpers for gesso.live.

   This namespace is deliberately narrow.

   It provides:

   - XTDB2 connectable/context helpers
   - explicit read-consistency/query option helpers
   - thin q/plan-q task wrappers
   - thin submit-tx/execute-tx task wrappers
   - opt-in diagnostic capture at the actual adapter submission call sites
   - portable XTDB-backed authoritative progression bases/requirements
   - authoritative request-frontier binding for Biff + Gesso contexts
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
   [java.time Instant ZonedDateTime]
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

(def ^:private ^:dynamic *status*
  xt/status)

(def ^:private ^:dynamic *submission-capture* nil)

(def submission-capture-type
  :gesso.live.consistency.xtdb/adapter-submission-capture)
(def submission-capture-version 1)

(defn capture-tx-api-calls!
  "Execute a zero-argument thunk while capturing THIS adapter's actual calls to
   its bound XTDB submit-tx/execute-tx functions, including the exact tx-ops
   and filtered options passed at the call site. Capture is opt-in, local to
   this invocation, and disabled when the thunk completes. No caller supplies
   a recorded operation vector, and ordinary transactions are unchanged.

   The result is {:type ... :version 1 :outcome :returned/:threw
                  :result .../:error ... :events [...]}.
   It intentionally includes raw submitted operations, possible sensitive
   document values and exception objects: NEVER publish or log the report.

   This is process-local ADAPTER evidence, not authenticated evidence that
   XTDB received the payload, no guarantee that the bound functions are
   authentic, and no universal writer coverage. Futures/async writes outside
   the live capture window may be absent. Test var-redefinitions remain
   supported; this helper does not authenticate them. A separate controlled
   executor can correlate this with its existing attempt observations later."
  [thunk]
  (when-not (fn? thunk)
    (throw (ex-info "Submission capture requires a zero-argument function."
                    {:submission-capture/rejection true
                     :reason :invalid-thunk})))
  (when *submission-capture*
    (throw (ex-info "Nested XTDB submission capture is unsupported."
                    {:submission-capture/rejection true
                     :reason :nested-capture})))
  (let [events (atom [])
        active (atom true)
        session {:events events :active active}
        execution (binding [*submission-capture* session]
                    (try
                      {:outcome :returned :result (thunk)}
                      (catch Throwable e
                        {:outcome :threw :error e})
                      (finally (reset! active false))))]
    (merge {:type submission-capture-type
            :version submission-capture-version
            :observation-origin :instrumented-xtdb-adapter-call-site
            :actual-xtdb-api-authenticated? false
            :complete-writer-coverage :not-established
            :transaction-internal-prestate :not-established
            :proof-status :submission-diagnostic-only
            :certificate :not-issued
            :authority :none
            :events @events}
           execution)))

(defn- capture-api-event! [event]
  (when-let [{:keys [events active]} *submission-capture*]
    (when @active
      ;; The diagnostic must not change the transaction's outcome. In practice
      ;; ordinary persistent-vector swap! cannot fail except for resource issues.
      (try (swap! events conj event)
           (catch Throwable _ nil)))))

(defn- invoke-xtdb-api! [api operation connectable tx-ops tx-options]
  ;; Capture immediately adjacent to the *actual* adapter API invocation, not
  ;; by replaying prepare or accepting an application-reported operation list.
  ;; The recorded argument object is the exact argument handed to the bound
  ;; XTDB API function. This remains spoofable by alternate writer paths,
  ;; with-redefs, and malicious code inside the same process.
  (let [capture? (boolean *submission-capture*)
        call-id (when capture? (random-uuid))]
    (when capture?
      (capture-api-event! {:call-id call-id :api api :phase :invoked
                           :tx-ops tx-ops :tx-options tx-options}))
    (try
      (let [result (operation connectable tx-ops tx-options)]
        (when capture?
          (capture-api-event! {:call-id call-id :api api :phase :returned
                               :tx-result result}))
        result)
      (catch Throwable e
        (when capture?
          (capture-api-event! {:call-id call-id :api api :phase :threw
                               :exception-class (.getName (class e))}))
        (throw e)))))

;; -----------------------------------------------------------------------------
;; Debug
;; -----------------------------------------------------------------------------

(defmacro ^:private debug!
  "Emit a debug event only when debug-fn is truthy.

   The data expression is inside the guard, so with debugging off it is not
   evaluated."
  [debug-fn event data]
  ;; Diagnostic hooks are observational. A broken trace callback must not
  ;; suppress submission, misreport an already completed transaction, or
  ;; replace an authoritative XTDB failure with a diagnostic exception.
  ;; Application assertions may throw Error rather than Exception.
  `(when-let [f# ~debug-fn]
     (try
       (f# (assoc ~data :event ~event))
       (catch Throwable _#
         nil))))

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

(declare normalize-consistency tx-result-consistency consistent-query-opts
         query-consistency query-opts decode-canonical-biff-snapshot!)

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

(defn- require-options-map!
  [options]
  ;; An explicitly supplied false or vector is not an absent option map.
  ;; Silently discarding it could submit a transaction without the database
  ;; or other XTDB options the caller intended to supply.
  (when-not (or (nil? options)
                (map? options))
    (throw
     (ex "XTDB options must be a map or nil."
         {:options options})))
  options)

(defn- select-compact
  [m ks]
  (compact-map (select-keys (or (require-options-map! m) {}) ks)))

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
                        (select-keys (or (require-options-map! options) {})
                                     adapter-option-keys))]
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
                            :biff.xtdb/connection-pool
                            :biff.xtdb/node]})))
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
    (do
      ;; Clojure's `name` drops a keyword namespace. Without this guard,
      ;; :tenant-a/shared and :tenant-b/shared silently select the same
      ;; XTDB database named "shared" and alias authoritative progression.
      ;; For a database name containing a slash, pass its exact string.
      (when (namespace database)
        (throw
         (ex "XTDB progression database identity must be an unqualified keyword or exact string."
             {:database database})))
      ;; Clojure permits (keyword "") and whitespace-only keyword names.
      ;; Keyword names must satisfy the same nonblank identity invariant,
      ;; or construction can produce a basis that xtdb-basis? later rejects.
      (let [database-name (name database)]
        (when (str/blank? database-name)
          (throw
           (ex "XTDB progression database keyword name must be non-blank."
               {:database database})))
        database-name))

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
      ;; A portable authoritative basis is compared as closed data, not just
      ;; by its parsed id. Alternate encodings such as "09", "+9", or "-0"
      ;; would make equivalent coordinates appear to be conflicting evidence.
      (when-not (= value (str tx-id))
        (throw
         (ex "XTDB progression wire tx-id must use canonical decimal text."
             {:tx-id value :canonical (str tx-id)})))
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
      ;; XTDB's protobuf decoder ignores unknown fields. Two different token
      ;; strings may therefore decode to the same database/time basis, while
      ;; Gesso's closed progression evidence compares the original strings.
      ;; Admit only XTDB's canonical serialization of that decoded basis so
      ;; one authoritative coordinate has exactly one portable identity.
      (let [canonical (xt.basis/->time-basis-str decoded)]
        (when-not (= snapshot-token canonical)
          (throw
           (ex "XTDB progression snapshot-token must use canonical encoding."
               {:database database-name
                :snapshot-token snapshot-token
                :canonical canonical}))))
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
   ;; A browser-supplied closed basis must obey the same nonblank database
   ;; identity contract as basis construction. XTDB can serialize an empty
   ;; database key in a snapshot token, so token roundtripping alone is not
   ;; sufficient to validate this identity.
   (not (str/blank? (:database value)))
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
         ;; An authoritative tx basis cannot be established from mutually
         ;; contradictory metadata. Both fields originate from the same
         ;; committed transaction in the normal execute-tx result; choosing
         ;; system-time with `or` would silently discard the other constraint.
         _ (when (and (some? system-time)
                      (some? snapshot-time)
                      (not= system-time snapshot-time))
             (throw
              (ex "XTDB consistency has conflicting system-time and snapshot-time coordinates."
                  {:system-time system-time
                   :snapshot-time snapshot-time})))
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
  "True when right strictly advances left's transaction id without regressing
   its authoritative snapshot time. Different databases are incomparable."
  [left right]
  (and (neg? (compare-bases left right))
       (not (.isBefore ^Instant (basis-system-time right)
                       ^Instant (basis-system-time left)))))

(defn progression-witness
  "Return trusted generic :advances evidence for left -> right.

   Equal bases need no witness and return nil. Backward movement is rejected."
  [left right]
  (let [comparison (compare-bases left right)]
    (cond
      (neg? comparison)
      (do
        ;; Generic progression deliberately accepts an exact witness from a
        ;; trusted adapter without rechecking physical ordering. An increase
        ;; in tx-id cannot certify that an earlier snapshot-time satisfies an
        ;; existing later read basis. Refuse to issue a false advance witness.
        (when (.isBefore ^Instant (basis-system-time right)
                         ^Instant (basis-system-time left))
          (throw
           (ex "XTDB progression witness cannot certify a regressing snapshot-time."
               {:from left
                :to right
                :reason :regressing-progression-snapshot-time})))
        (progression/advance left right))

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
   XTDB adapter, same-database bases are reducible to the greatest transaction
   id only when their snapshot times do not regress. Contradictory times and
   mixed/non-XTDB bases fail closed."
  [requirement-value]
  (when-some [requirement'
              (progression/normalize-requirement requirement-value)]
    (reduce
     (fn [strongest candidate]
       (require-xtdb-basis! candidate)
       (if (nil? strongest)
         candidate
         (let [comparison (compare-bases strongest candidate)
               newer (if (neg? comparison) candidate strongest)
               older (if (neg? comparison) strongest candidate)]
           ;; Transaction-id ordering alone cannot prove that the selected
           ;; basis satisfies a later system-time requirement. Reject an
           ;; inversion instead of silently dropping the later snapshot.
           (when (.isBefore ^Instant (basis-system-time newer)
                            ^Instant (basis-system-time older))
             (throw
              (ex "XTDB progression requirements conflict in transaction and snapshot-time order."
                  {:older-tx-basis older
                   :newer-tx-basis newer
                   :reason :incompatible-required-snapshot-times})))
           newer)))
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

(defn- combine-progression-snapshot-bases!
  "Compose separately supplied consistency/caller tokens without losing either.

   The selected database is ultimately fixed to the authoritative progression
   frontier, but every source must first be checked for an unsatisfied later
   coordinate. Independent databases retain their bases unless two sources
   disagree; without additional evidence, choosing either would be unsafe."
  [required consistency-token caller-token]
  (let [required-db (basis-database required)
        observed-time (basis-system-time required)]
    (reduce
     (fn [combined token]
       (let [decoded (decode-canonical-biff-snapshot! required-db token)]
         (when (some #(.isAfter ^Instant % observed-time)
                     (get decoded required-db))
           (throw
            (ex "Explicit XTDB snapshot token is ahead of authoritative progression."
                {:snapshot-token token
                 :required-basis required
                 :database required-db
                 :reason :explicit-snapshot-token-ahead-of-progression})))
         (reduce-kv
          (fn [acc database times]
            (when (and (not= database required-db)
                       (contains? acc database)
                       (not= (get acc database) times))
              (throw
               (ex "Independent XTDB snapshot tokens disagree on a database time basis."
                   {:database database
                    :existing-times (get acc database)
                    :incoming-times times
                    :reason :conflicting-independent-snapshot-bases})))
            (assoc acc database times))
          combined decoded)))
     {}
     (filter some? [consistency-token caller-token]))))

(defn- enforce-progression-query-opts
  [opts requirement-value consistency-options caller-options]
  (if-some [required (strongest-required-basis requirement-value)]
    (let [required-db (basis-database required)
          observed-time (basis-system-time required)
          explicit-db (when (contains? opts :database)
                        (normalize-database-name (:database opts)))]
      ;; Both sources may independently pin the read's temporal context.
      ;; Unlike progression's transaction frontier, :current-time and
      ;; :default-tz do not have a trusted stronger/weaker ordering. A caller
      ;; option must not silently replace an admitted value once a query has
      ;; authoritative progression; reject the conflict before XTDB I/O.
      ;; Without progression, normal explicit query-option precedence remains.
      (doseq [field [:current-time :default-tz]
              :when (and (contains? consistency-options field)
                         (contains? caller-options field)
                         (not= (get consistency-options field)
                               (get caller-options field)))]
        (throw
         (ex "XTDB consistency and caller temporal read context conflict under authoritative progression."
             {:field field
              :consistency-value (get consistency-options field)
              :caller-value (get caller-options field)
              :reason :conflicting-temporal-read-context})))
      ;; Await tokens are also independently supplied read obligations. We
      ;; cannot infer which of two different opaque tokens is stronger here.
      ;; Ordinary map precedence would erase the consistency await-token,
      ;; so reject the ambiguity rather than silently dropping a wait.
      (when (and (contains? consistency-options :await-token)
                 (contains? caller-options :await-token)
                 (not= (:await-token consistency-options)
                       (:await-token caller-options)))
        (throw
         (ex "XTDB consistency and caller await tokens conflict under authoritative progression."
             {:consistency-await-token (:await-token consistency-options)
              :caller-await-token (:await-token caller-options)
              :reason :conflicting-await-tokens})))
      ;; Both consistency metadata and explicit opts are independently supplied
      ;; read constraints. The ordinary opts merge lets callers override
      ;; convenience values, but under authoritative progression neither
      ;; snapshot-time can be silently discarded before its safety check.
      (doseq [explicit-time (keep :snapshot-time
                                 [consistency-options caller-options])]
        (let [requested-time
              (cond
                (instance? Instant explicit-time)
                explicit-time

                (instance? ZonedDateTime explicit-time)
                (.toInstant ^ZonedDateTime explicit-time)

                :else
                (throw
                 (ex "XTDB explicit snapshot-time must be an Instant or ZonedDateTime when progression is required."
                     {:snapshot-time explicit-time
                      :database required-db
                      :reason :invalid-explicit-snapshot-time})))]
          (when (.isAfter ^Instant requested-time ^Instant observed-time)
            (throw
             (ex "Explicit XTDB snapshot-time is ahead of authoritative progression."
                 {:snapshot-time explicit-time
                  :required-basis required
                  :database required-db
                  :reason :explicit-snapshot-ahead-of-progression})))))
      (when (and explicit-db
                 (not= explicit-db required-db))
        (throw
         (ex "Explicit XTDB query database conflicts with authoritative progression."
             {:explicit-database (:database opts)
              :required-database required-db})))
      ;; Validate each supplied canonical token independently. Merge distinct
      ;; database coordinates instead of losing one to ordinary option-map
      ;; precedence. A selected-database basis later than progression is never
      ;; satisfied; older selected coordinates advance to the required time.
      (let [existing-bases (combine-progression-snapshot-bases!
                            required
                            (:snapshot-token consistency-options)
                            (:snapshot-token caller-options))
            snapshot-token (xt.basis/->time-basis-str
                            (assoc existing-bases required-db [observed-time]))]
        (cond-> (-> opts
                    (dissoc :snapshot-time :snapshot-token)
                    (assoc :snapshot-token snapshot-token))
          (and (not explicit-db)
               (not= default-database-name required-db))
          (assoc :database required-db))))
    opts))

(defn progression-from
  "Return optional generic progression from a request/context map.

   :gesso.live/progression is canonical. :progression is accepted as a narrow
   request-local convenience while the public Live facade is migrated."
  [x]
  (when (map? x)
    ;; A supplied false value is malformed, not an absent requirement. Using
    ;; `or` would silently erase it (or replace it with a legacy value), so
    ;; use nil-only fallback before enforcing the canonical requirement shape.
    (progression/normalize-requirement
     (if-some [canonical (:gesso.live/progression x)]
       canonical
       (:progression x)))))

(defn read-query-opts
  "Build query opts from explicit consistency, optional authoritative progression,
   and caller opts. Progression constraints are applied last and cannot be
   weakened by caller query options."
  ([consistency progression-value]
   (read-query-opts consistency progression-value nil))
  ([consistency progression-value opts]
   (let [consistency-options (query-consistency consistency)
         caller-options (query-opts opts)]
     (enforce-progression-query-opts
      (merge consistency-options caller-options)
      progression-value
      consistency-options
      caller-options))))

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

   Released Biff 2 fallback keys:

     :biff.xtdb/connection-pool
     :biff.xtdb/node"
  [x]
  (cond
    ;; XTDB's concrete node is itself map-like. It is a raw connectable, not
    ;; a Ring/Biff context; extracting context keys would discard the node.
    (instance? xtdb.api.Xtdb x)
    x

    (map? x)
    ;; Presence of false is an invalid connectable, not permission to fall
    ;; through to a different connection or node. Only nil means absent.
    (first (filter some?
                   (map #(get x %)
                        [:xtdb/connectable
                         :xtdb/conn
                         :xtdb/node
                         :biff.xtdb/connection-pool
                         :biff.xtdb/node])))

    :else
    x))

(defn read-connectable-from
  "Return the preferred XTDB2 connectable for live/read-after-write queries.

   Prefer explicit read connectables and request-scoped connections first.
   Shared DataSource/node values are valid XTDB connectables, but XTDB q/plan-q
   may implicitly use their shared await-token state.

   Preferred context keys:

     :xtdb/read-connectable
     :xtdb/conn
     :xtdb/connectable
     :biff.xtdb/connection-pool
     :xtdb/node
     :biff.xtdb/node"
  [x]
  (cond
    ;; A direct XTDB node also implements IPersistentMap; do not mistake it
    ;; for a request context or lose the raw read connectable.
    (instance? xtdb.api.Xtdb x)
    x

    (map? x)
    ;; An explicitly invalid request-scoped read connectable must not be
    ;; bypassed by silently selecting a shared fallback connection.
    (first (filter some?
                   (map #(get x %)
                        [:xtdb/read-connectable
                         :xtdb/conn
                         :xtdb/connectable
                         :biff.xtdb/connection-pool
                         :xtdb/node
                         :biff.xtdb/node])))

    :else
    x))


;; -----------------------------------------------------------------------------
;; Authoritative request frontier
;; -----------------------------------------------------------------------------

(defn node-from
  "Return the XTDB2 node required to establish an authoritative request frontier.

   Unlike connectable-from/read-connectable-from, this helper deliberately does
   not accept request-scoped connections or arbitrary DataSources. XTDB status is
   node-owned state, and one status snapshot is the trusted source used to bind
   a matching transaction id and system time.

   Preferred context keys:

     :xtdb/node
     :biff.xtdb/node

   A raw XTDB node may also be supplied directly. XTDB's concrete node currently
   implements IPersistentMap, so node identity must be recognized before treating
   a map-like value as an application context."
  [x]
  (cond
    (instance? xtdb.api.Xtdb x)
    x

    (map? x)
    ;; The two supported node keys are aliases for the same status authority,
    ;; not independent node selections. If both contain different concrete
    ;; nodes, Biff could execute using the legacy node while Gesso publishes
    ;; the primary node's transaction frontier. Reject before status I/O.
    ;; An explicitly false primary key retains its existing fail-closed path;
    ;; it must not silently fall back to the legacy node.
    (let [primary (:xtdb/node x)
          legacy (:biff.xtdb/node x)]
      (when (and primary
                 (some? legacy)
                 (not (identical? primary legacy)))
        (throw
         (ex "Conflicting XTDB node identities cannot establish one authoritative request frontier."
             {:reason :conflicting-admitted-xtdb-nodes
              :xtdb-node primary
              :biff-xtdb-node legacy})))
      (if-some [node primary]
        node
        legacy))

    :else
    nil))

(defn- status-system-time->instant
  [system-time]
  (cond
    (instance? Instant system-time)
    system-time

    (instance? ZonedDateTime system-time)
    (.toInstant ^ZonedDateTime system-time)

    :else
    (throw
     (ex "XTDB latest-completed transaction has an unsupported system-time type."
         {:system-time system-time
          :system-time-class (some-> system-time class .getName)
          :expected-one-of ["java.time.Instant"
                            "java.time.ZonedDateTime"]}))))

(defn latest-completed-basis
  "Return one portable basis for the node's latest completed XTDB transaction.

   The transaction id and system time are read from one xtdb.api/status result,
   so the resulting basis cannot accidentally pair coordinates observed at two
   different moments. XTDB 2.2 status currently exposes completed transaction
   system time as java.time.ZonedDateTime; execute-tx exposes java.time.Instant.
   Both are normalized to the same Instant coordinate before basis construction.
   Returns nil only when XTDB reports no completed transaction for the selected
   database.

   This is a trusted server-side observation boundary. It must not be implemented
   from wall-clock time, application revisions, browser data, or separate mutable
   reads of transaction id and system time."
  ([ctx-or-node]
   (latest-completed-basis ctx-or-node nil))
  ([ctx-or-node database]
   (let [node (node-from ctx-or-node)
         database-name (normalize-database-name database)]
     (when-not node
       (throw
        (ex "Cannot establish an XTDB request frontier without an XTDB node."
            {:expected-one-of [:xtdb/node
                               :biff.xtdb/node]})))
     (let [status (*status* node)
           by-database (:latest-completed-txs status)]
       ;; An absent/malformed status observation is not evidence that a
       ;; database has no completed transactions. XTDB represents a known
       ;; empty database by an absent entry, empty completed-tx list, or its
       ;; singleton nil-coordinate TransactionKey sentinel in a present
       ;; :latest-completed-txs map. Never silently return an
       ;; unconstrained Biff request merely because node status is invalid.
       (when-not (map? by-database)
         (throw
          (ex "XTDB latest-completed status lacks a usable transaction map."
              {:database database-name
               :status status})))
       ;; XTDB status indexes completed transactions by exact string database
       ;; identities. A keyword/nil/blank key can hide a populated database
       ;; from a valid string lookup, making a malformed observation look like
       ;; an empty database and leaving the Biff request snapshot unpinned.
       ;; Validate the complete status keyspace before treating absence as
       ;; evidence that no transaction has completed for the selected database.
       (when-not (every? (fn [database-id]
                           (and (string? database-id)
                                (not (str/blank? database-id))))
                         (keys by-database))
         (throw
          (ex "XTDB latest-completed status contains malformed database identities."
              {:database database-name
               :status status})))
       (let [entries (get by-database database-name)]
         ;; In particular, a present nil entry or [nil] would otherwise be
         ;; read as an empty database by first/when-some. This must fail
         ;; closed rather than silently discard authoritative snapshot state.
         (when (and (contains? by-database database-name)
                    (or (not (or (sequential? entries)
                                 (instance? java.util.List entries)))
                        (and (seq entries)
                             (nil? (first entries)))))
           (throw
            (ex "XTDB latest-completed status contains malformed transaction entries."
                {:database database-name
                 :status status})))
         ;; Status is a single trusted observation, not just its first
         ;; element. A malformed trailing transaction must not be ignored
         ;; while its first element is promoted to an authoritative frontier.
         ;; Keep multiple valid completed keys acceptable, but validate each
         ;; remaining coordinate using the same portable basis rules.
         (let [trailing-bases
               (mapv
                (fn [trailing]
                  (try
                    (when-not (map? trailing)
                      (throw
                       (ex "Trailing XTDB transaction must be map-like."
                           {:entry trailing})))
                    (basis database-name
                           (:tx-id trailing)
                           (status-system-time->instant (:system-time trailing)))
                    (catch Throwable cause
                      (throw
                       (ex "XTDB latest-completed status contains malformed trailing transaction evidence."
                           {:database database-name
                            :status status
                            :reason :malformed-completed-transaction-tail
                            :entry trailing}
                           cause)))))
                (rest entries))]
           (when-some [completed (first entries)]
             (let [tx-id (:tx-id completed)
                   system-time (:system-time completed)]
               (cond
                 ;; An untouched XTDB 2.2 node reports one typed TxKey with
                 ;; both coordinates nil. This is its empty-database sentinel,
                 ;; not a partially observed committed transaction. Requiring
                 ;; the exact singleton, map-like TransactionKey prevents an
                 ;; ordinary malformed status map from masquerading as empty.
                 (and (= 1 (count entries))
                      (transaction-key? completed)
                      (map? completed)
                      (= #{:tx-id :system-time} (set (keys completed)))
                      (nil? tx-id)
                      (nil? system-time))
                 nil

                 (or (nil? tx-id) (nil? system-time))
                 (throw
                  (ex "XTDB latest-completed transaction has incomplete authoritative coordinates."
                      {:database database-name
                       :latest-completed completed
                       :tx-id tx-id
                       :system-time system-time}))

                 :else
                 (let [selected (basis database-name
                                       tx-id
                                       (status-system-time->instant system-time))
                       selected-id (basis-tx-id selected)
                       selected-time (basis-system-time selected)]
                   ;; All entries belong to the same completed-status
                   ;; observation. The first claims to be the latest; a
                   ;; subsequent key cannot be newer in either coordinate,
                   ;; nor assign a different time to the same transaction id.
                   ;; Reject inconsistency rather than silently pinning Biff
                   ;; to a frontier contradicted by the observed status.
                   (doseq [trailing trailing-bases]
                     (let [other-id (basis-tx-id trailing)
                           other-time (basis-system-time trailing)]
                       (when (or (> other-id selected-id)
                                 (.isAfter ^Instant other-time selected-time)
                                 (and (= other-id selected-id)
                                      (not= other-time selected-time)))
                         (throw
                          (ex "XTDB latest-completed status contradicts its selected transaction frontier."
                              {:database database-name
                               :status status
                               :reason :contradictory-completed-transaction-frontier
                               :selected selected
                               :contradictory trailing})))))
                   ;; The selected entry need not be contradicted directly
                   ;; for the status observation to be inconsistent. Two
                   ;; historical keys may identify the same tx with different
                   ;; times, or invert tx-id and system-time ordering, while
                   ;; each separately remains behind the selected frontier.
                   ;; Compare neighboring ids in sorted order so a status
                   ;; collection is not required to arrive in history order.
                   (doseq [[older newer]
                           (partition 2 1
                                      (sort-by basis-tx-id trailing-bases))]
                     (let [older-id (basis-tx-id older)
                           newer-id (basis-tx-id newer)
                           older-time (basis-system-time older)
                           newer-time (basis-system-time newer)]
                       (when (or (and (= older-id newer-id)
                                      (not= older-time newer-time))
                                 (and (< older-id newer-id)
                                      (.isAfter ^Instant older-time newer-time)))
                         (throw
                          (ex "XTDB latest-completed status contains contradictory historical transaction evidence."
                              {:database database-name
                               :status status
                               :reason :contradictory-completed-transaction-history
                               :older older
                               :newer newer})))))
                   selected))))))))))

(defn- decode-canonical-biff-snapshot!
  "Validate the existing Biff token before preserving any of its database bases.

   On a populated selected database, the selected coordinate is replaced by
   the authoritative status frontier. Other databases' valid time bases are
   independent and must not be silently discarded. This validation also
   serves the empty-selected-database path, which must fail closed when a
   token claims a snapshot for a database that has no completed transaction."
  [database-name snapshot-token]
  (try
    (when-not (and (string? snapshot-token)
                   (not (str/blank? snapshot-token)))
      (throw (ex "Invalid Biff snapshot token."
                 {:snapshot-token snapshot-token})))
    (let [decoded (xt.basis/<-time-basis-str snapshot-token)]
      (when-not (and (map? decoded)
                     (= snapshot-token
                        (xt.basis/->time-basis-str decoded)))
        (throw (ex "Noncanonical Biff snapshot token."
                   {:snapshot-token snapshot-token})))
      ;; XTDB will canonically serialize a time basis with an empty or blank
      ;; database name. Such a key is not a valid Gesso/XTDB database identity
      ;; and must not survive when composing independent Biff snapshots.
      (when-not (every? (fn [database-id]
                          (and (string? database-id)
                               (not (str/blank? database-id))))
                        (keys decoded))
        (throw (ex "Biff snapshot token contains an invalid database identity."
                   {:snapshot-token snapshot-token
                    :databases (keys decoded)})))
      ;; XTDB also roundtrips a canonical database entry with no timestamps.
      ;; Such an entry cannot pin a read to any snapshot. Do not preserve it
      ;; as though it were an independently established Biff time basis.
      ;; Keep nonempty multi-timestamp entries: XTDB can represent attached
      ;; database state with multiple coordinates for a single database.
      (when-not (every? (fn [[_ times]]
                          (and (sequential? times)
                               (seq times)
                               (every? #(instance? Instant %) times)))
                        decoded)
        (throw (ex "Biff snapshot token contains a database without a usable time basis."
                   {:snapshot-token snapshot-token
                    :databases (keys decoded)})))
      decoded)
    (catch Throwable cause
      (throw
       (ex "XTDB request frontier cannot preserve a preexisting Biff snapshot token."
           {:database database-name
            :snapshot-token snapshot-token
            :reason :invalid-snapshot-token}
           cause)))))

(defn- require-wrapped-xtdb-node!
  "Reject an execution-time node that differs from frontier observation.

   The binding's transaction ID and system-time describe one specific XTDB
   node. An outer Biff wrapper or later Ring request may supply its own node
   keys. It cannot use that different node with the first node's authoritative
   snapshot and progression. Connection pools and other connectables are not
   authenticated by this narrow node-identity check."
  [bound-node wrapped-ctx]
  (doseq [node-key [:xtdb/node :biff.xtdb/node]
          :let [runtime-node (get wrapped-ctx node-key)]
          :when (some? runtime-node)]
    (when-not (identical? bound-node runtime-node)
      (throw
       (ex "Deferred XTDB request node differs from the node that established the authoritative frontier."
           {:reason :conflicting-runtime-xtdb-node
            :node-key node-key
            :bound-node bound-node
            :runtime-node runtime-node}))))
  wrapped-ctx)

(defn- preserve-admitted-xtdb-node
  "Carry the node used for frontier admission into fresh Biff handler requests.

   A Ring request need not repeat the binding context's node. Preserve a
   matching explicitly supplied node and keep request-scoped read connectables
   intact; they remain preferred by read-connectable-from. Conflicting
   explicit node identities are rejected separately before this fallback."
  [bound-node wrapped-ctx]
  (if (some? (node-from wrapped-ctx))
    wrapped-ctx
    (assoc wrapped-ctx :xtdb/node bound-node)))

(declare frontier-xtdb-required-bases
         require-frontier-satisfies-existing-xtdb!
         consistency-from)

(defn- compose-wrapped-xtdb-consistency!
  "Preserve independently admitted XTDB query consistency into a fresh request.

   Biff may execute the handler with a new Ring request lacking the original
   explicit :await-token or other consistency metadata. Runtime constraints
   can add independent fields, but an opaque value supplied at admission must
   not be silently replaced by a different value for the same key."
  [admitted-consistency wrapped-ctx]
  (let [runtime-consistency (consistency-from wrapped-ctx)]
    (doseq [[field value] admitted-consistency
            :when (and (contains? runtime-consistency field)
                       (not= value (get runtime-consistency field)))]
      (throw
       (ex "Deferred XTDB query consistency conflicts with an admitted read constraint."
           {:reason :conflicting-runtime-xtdb-consistency
            :field field
            :admitted-value value
            :runtime-value (get runtime-consistency field)})))
    (merge admitted-consistency runtime-consistency)))

(defn- compose-wrapped-biff-progression!
  "Preserve admitted progression when a Biff wrapper supplies the handler ctx.

   A wrapper may receive a fresh Ring request with no canonical progression, or
   introduce an additional one. The latter must not replace the admission-time
   requirement or claim a selected-database basis beyond the observed frontier."
  [database-name frontier admitted-requirement wrapped-ctx]
  (let [combined (progression/compose
                  admitted-requirement
                  (progression-from wrapped-ctx))
        required-bases (frontier-xtdb-required-bases combined)]
    (if frontier
      (require-frontier-satisfies-existing-xtdb! frontier required-bases)
      (when-some [unsatisfied
                  (some (fn [candidate]
                          (when (= database-name (basis-database candidate))
                            candidate))
                        required-bases)]
        (throw
         (ex "XTDB has no completed transaction and cannot satisfy an existing authoritative progression requirement."
             {:database database-name
              :required unsatisfied}))))
    combined))

(defn- compose-fixed-biff-snapshot!
  "Preserve deferred Biff snapshot obligations without weakening the frontier.

   The fixed token was admitted from the node status and original request
   context. Another Biff wrapper (or the request itself) may provide an
   independent token only when the body executes. Its selected-database
   coordinates must not exceed the observed frontier; foreign coordinates
   cannot conflict with an already preserved fixed snapshot."
  [database-name fixed-bases snapshot-token]
  (if-some [incoming-token snapshot-token]
    (let [incoming (decode-canonical-biff-snapshot!
                    database-name incoming-token)
          frontier-time (first (get fixed-bases database-name))]
      (when (some #(.isAfter ^Instant % frontier-time)
                  (get incoming database-name))
        (throw
         (ex "Deferred Biff snapshot is ahead of the observed XTDB request frontier."
             {:database database-name
              :snapshot-token incoming-token
              :observed-snapshot-time frontier-time
              :reason :selected-snapshot-ahead-of-frontier})))
      (doseq [[other-database times] incoming
              :when (and (not= database-name other-database)
                         (contains? fixed-bases other-database)
                         (not= (get fixed-bases other-database) times))]
        (throw
         (ex "Deferred Biff snapshot conflicts with an independent database frontier."
             {:database other-database
              :selected-database database-name
              :snapshot-token incoming-token
              :existing-times (get fixed-bases other-database)
              :incoming-times times
              :reason :conflicting-independent-snapshot-bases})))
      ;; Preserve both sets of independent database constraints. The selected
      ;; database always remains pinned to Gesso's server-observed frontier.
      (xt.basis/->time-basis-str
       (merge fixed-bases (dissoc incoming database-name))))
    (xt.basis/->time-basis-str fixed-bases)))

(defn- fixed-biff-snapshot-wrapper
  [existing-wrapper database-name snapshot-token frontier admitted-requirement
   admitted-consistency bound-node]
  ;; Validate the outer Biff wrapper at request-frontier admission. In
  ;; particular, an explicitly supplied false must not be treated like nil
  ;; and silently bypass the wrapper whose behavior the caller relies on.
  (when (and (some? existing-wrapper)
             (not (ifn? existing-wrapper)))
    (throw
     (ex "Existing Biff request snapshot wrapper must be callable."
         {:wrapper existing-wrapper})))
  (let [fixed-bases (decode-canonical-biff-snapshot!
                     database-name snapshot-token)]
    (fn [f]
      (when-not (ifn? f)
        (throw
         (ex "Biff request snapshot wrapper requires a callable body."
             {:body f})))
      (let [fixed-body
            (fn [ctx]
              (when-not (map? ctx)
                (throw
                 (ex "Existing Biff wrapper must supply a request context map."
                     {:ctx ctx})))
              (require-wrapped-xtdb-node! bound-node ctx)
              (let [combined-consistency
                    (compose-wrapped-xtdb-consistency!
                     admitted-consistency ctx)
                    combined-requirement
                    (compose-wrapped-biff-progression!
                     database-name frontier admitted-requirement ctx)
                    composed-token
                    (compose-fixed-biff-snapshot!
                     database-name fixed-bases
                     (:biff.xtdb/snapshot-token ctx))]
                (f (preserve-admitted-xtdb-node
                    bound-node
                    (cond-> (assoc ctx
                                   :biff.xtdb/snapshot-token composed-token
                                   :gesso.live/progression combined-requirement)
                      (seq combined-consistency)
                      (assoc :gesso.live/consistency combined-consistency))))))]
        (if-some [outer-wrapper existing-wrapper]
          (outer-wrapper fixed-body)
          fixed-body)))))

(defn- compose-empty-biff-snapshot!
  "Compose foreign snapshot bases across an empty selected database boundary.

   The selected database has no observed transaction, so it must never be
   assigned a snapshot. Independent databases can already have fixed time
   bases at request admission or gain them from a deferred Biff wrapper;
   neither source may silently discard or contradict the other."
  [database-name admitted-bases deferred-token]
  (let [deferred-bases (if-some [token deferred-token]
                         (decode-canonical-biff-snapshot! database-name token)
                         {})]
    (when (contains? deferred-bases database-name)
      (throw
       (ex "XTDB has no completed transaction and cannot preserve a preexisting Biff snapshot token."
           {:database database-name
            :snapshot-token deferred-token
            :reason :unobserved-selected-database})))
    (doseq [[foreign-database times] deferred-bases
            :when (and (contains? admitted-bases foreign-database)
                       (not= (get admitted-bases foreign-database) times))]
      (throw
       (ex "Deferred Biff snapshot conflicts with an admitted independent database frontier."
           {:database foreign-database
            :selected-database database-name
            :snapshot-token deferred-token
            :existing-times (get admitted-bases foreign-database)
            :incoming-times times
            :reason :conflicting-independent-snapshot-bases})))
    (let [combined (merge admitted-bases deferred-bases)]
      (when (seq combined)
        (xt.basis/->time-basis-str combined)))))

(defn- guarded-empty-biff-snapshot-wrapper
  "Preserve admitted foreign snapshots while guarding deferred Biff execution.

   The selected database has no completed transaction. An existing Biff
   wrapper may introduce or replace a token after initial frontier binding.
   Recheck its snapshot at execution time and retain admitted independent
   database bases instead of silently losing them."
  [existing-wrapper database-name admitted-token admitted-requirement
   admitted-consistency bound-node]
  ;; A request may have admitted foreign snapshots/progression even when Biff
  ;; has no preexisting wrapper. Install the same execution-time guard for
  ;; those obligations instead of letting a fresh handler request erase them.
  (when (and (some? existing-wrapper)
             (not (ifn? existing-wrapper)))
    (throw
     (ex "Existing Biff request snapshot wrapper must be callable."
         {:wrapper existing-wrapper})))
  (let [admitted-bases (if-some [token admitted-token]
                         (decode-canonical-biff-snapshot! database-name token)
                         {})]
    (fn [body]
      (when-not (ifn? body)
        (throw
         (ex "Biff request snapshot wrapper requires a callable body."
             {:body body})))
      (let [guarded-body
            (fn [wrapped-ctx]
              (when-not (map? wrapped-ctx)
                (throw
                 (ex "Existing Biff wrapper must supply a request context map."
                     {:ctx wrapped-ctx})))
              (require-wrapped-xtdb-node! bound-node wrapped-ctx)
              (let [combined-consistency
                    (compose-wrapped-xtdb-consistency!
                     admitted-consistency wrapped-ctx)
                    combined-requirement
                    (compose-wrapped-biff-progression!
                     database-name nil admitted-requirement wrapped-ctx)
                    composed-token (compose-empty-biff-snapshot!
                                    database-name admitted-bases
                                    (:biff.xtdb/snapshot-token wrapped-ctx))]
                (body (preserve-admitted-xtdb-node
                       bound-node
                       (cond-> wrapped-ctx
                         composed-token
                         (assoc :biff.xtdb/snapshot-token composed-token)
                         combined-requirement
                         (assoc :gesso.live/progression combined-requirement)
                         (seq combined-consistency)
                         (assoc :gesso.live/consistency combined-consistency))))))]
        (if-some [outer-wrapper existing-wrapper]
          (outer-wrapper guarded-body)
          guarded-body)))))

(defn- frontier-xtdb-required-bases
  "Validate every basis that explicitly claims XTDB authority.

   Generic progression permits other opaque authorities, but a malformed
   XTDB-tagged basis cannot be silently treated as unrelated evidence and
   skipped during request-frontier enforcement. Validation is necessary even
   when the selected XTDB database has not completed its first transaction."
  [existing-requirement]
  (into []
        (comp
         (filter #(and (map? %)
                       (= xtdb-basis-type
                          (:gesso.live.consistency.xtdb/type %))))
         (map require-xtdb-basis!))
        (progression/required-bases existing-requirement)))

(defn- require-frontier-satisfies-existing-xtdb!
  [frontier xtdb-required-bases]
  (doseq [required xtdb-required-bases
          :when (= (basis-database frontier)
                   (basis-database required))]
    ;; A higher transaction id does not by itself prove that the observed
    ;; snapshot-time satisfies a preexisting later requested time. Reject
    ;; either missing part of the frontier before publishing a Biff snapshot.
    (when (or (neg? (compare-bases frontier required))
              (.isBefore ^Instant (basis-system-time frontier)
                         ^Instant (basis-system-time required)))
      (throw
       (ex "Current XTDB request frontier is behind an existing authoritative progression requirement."
           {:frontier frontier
            :required required
            :database (basis-database frontier)}))))
  frontier)

(defn bind-request-frontier
  "Bind one authoritative XTDB request frontier into a Ring/Biff context.

   A successful binding establishes all of the following from one latest-completed
   XTDB transaction observation:

   - :biff.xtdb/snapshot-token fixes ordinary Biff reads to the same XTDB time;
   - :gesso.live/progression contains that authoritative basis as a minimum read
     requirement, conservatively composed with any existing canonical requirement;
   - :biff.core/wrap-db-snapshot is preserved and wrapped with a fixed frontier
     so later Biff Graph executions cannot silently drift to a newer snapshot.

   An existing basis tagged as XTDB evidence is validated before the status
   observation, whether or not the database has completed any transactions.
   Invalid claimed XTDB evidence is rejected rather than silently ignored.

   If XTDB has no completed transaction for the selected database, the
   helper preserves existing foreign read obligations with a guarded wrapper
   even when Biff had no prior wrapper. Even unconstrained empty startup
   receives a guard against snapshot/progression claims introduced only when
   a fresh request reaches the handler; it fabricates no selected-db basis.
   An existing same-database requirement fails closed:
   without a completed frontier, ordinary Biff reads cannot be pinned to a
   snapshot satisfying that requirement. It never fabricates an initial basis.

   If an HTTP/browser progression requirement is relevant, bind it into canonical
   context before calling this helper. The trusted current XTDB basis is then
   composed with that pre-existing minimum requirement rather than replacing it.
   If the node's current frontier is behind an existing requirement for the same
   XTDB database, binding fails closed instead of installing a stale Biff snapshot.

   The fixed Biff snapshot is the server-observed current frontier. Gesso-aware
   model reads must continue to apply :gesso.live/progression last, as
   read-query-opts does, so a stronger pre-existing minimum requirement cannot be
   weakened by the Biff baseline."
  ([ctx]
   (bind-request-frontier ctx nil))
  ([ctx database]
   (when-not (map? ctx)
     (throw
      (ex "XTDB request frontier binding requires a context map."
          {:ctx ctx})))
   (let [bound-node (node-from ctx)
         admitted-consistency (consistency-from ctx)
         existing-requirement (progression-from ctx)
         xtdb-required-bases (frontier-xtdb-required-bases existing-requirement)]
     (if-some [basis-value (latest-completed-basis ctx database)]
       (let [_ (require-frontier-satisfies-existing-xtdb!
                basis-value
                xtdb-required-bases)
             ;; Preserve time bases for unrelated databases in the existing
             ;; Biff snapshot token, while replacing only this database with
             ;; the node-observed authoritative frontier. A malformed token
             ;; must not be silently discarded when a frontier is present.
             snapshot-token
             (if-some [existing-token (:biff.xtdb/snapshot-token ctx)]
               (let [database-name (basis-database basis-value)
                     observed-time (basis-system-time basis-value)
                     previous-bases (decode-canonical-biff-snapshot!
                                     database-name existing-token)]
                 ;; The node's latest completed tx is authoritative for the
                 ;; selected database. A preexisting Biff snapshot claiming a
                 ;; later time is not satisfied by that observation. Replacing
                 ;; it would downgrade an existing selected-database read
                 ;; constraint without evidence; reject instead. Independent
                 ;; database time bases are not compared with this frontier.
                 (when (some #(.isAfter ^Instant % observed-time)
                             (get previous-bases database-name))
                   (throw
                    (ex "Existing Biff snapshot is ahead of the observed XTDB request frontier."
                        {:database database-name
                         :snapshot-token existing-token
                         :observed-frontier basis-value
                         :reason :selected-snapshot-ahead-of-frontier})))
                 (xt.basis/->time-basis-str
                  (assoc previous-bases database-name [observed-time])))
               (basis-snapshot-token basis-value))
             current-requirement (progression/requirement basis-value)
             combined-requirement
             (progression/compose
              existing-requirement
              current-requirement)]
         (assoc ctx
                :biff.xtdb/snapshot-token snapshot-token
                :gesso.live/progression combined-requirement
                :biff.core/wrap-db-snapshot
                (fixed-biff-snapshot-wrapper
                 (:biff.core/wrap-db-snapshot ctx)
                 (basis-database basis-value)
                 snapshot-token
                 basis-value
                 combined-requirement
                 admitted-consistency
                 bound-node)))
       ;; With no completed selected-database transaction, an existing browser
       ;; minimum for that database cannot be satisfied by any Biff snapshot.
       ;; Returning an unchanged context would silently permit unconstrained
       ;; ordinary reads. Keep the initial empty-db case usable, and leave
       ;; independent requirements for other databases in the context.
       (let [database-name (normalize-database-name database)
             required (some (fn [candidate]
                              (when (= database-name
                                       (basis-database candidate))
                                candidate))
                            xtdb-required-bases)]
         (when required
           (throw
            (ex "XTDB has no completed transaction and cannot satisfy an existing authoritative progression requirement."
                {:database database-name
                 :required required})))
         ;; With no selected-database frontier, a preexisting Biff snapshot
         ;; must not claim a timestamp for this unobserved database. A valid
         ;; foreign-database token is independent evidence and may remain in
         ;; the context. Reject malformed/noncanonical tokens rather than
         ;; treating them as foreign, which would silently trust bad state.
         (when-some [snapshot-token (:biff.xtdb/snapshot-token ctx)]
           (let [token-databases
                 (set (keys (decode-canonical-biff-snapshot!
                             database-name snapshot-token)))]
             (when (contains? token-databases database-name)
               (throw
                (ex "XTDB has no completed transaction and cannot preserve a preexisting Biff snapshot token."
                    {:database database-name
                     :snapshot-token snapshot-token
                     :reason :unobserved-selected-database})))))
         ;; Even with no admitted constraint or existing Biff wrapper, a
         ;; fresh handler request can introduce a fabricated selected-db token
         ;; or progression after binding. Guard that execution boundary for
         ;; every empty database while leaving its snapshot/progression unset.
         (assoc ctx :biff.core/wrap-db-snapshot
                (guarded-empty-biff-snapshot-wrapper
                 (:biff.core/wrap-db-snapshot ctx)
                 database-name
                 (:biff.xtdb/snapshot-token ctx)
                 existing-requirement
                 admitted-consistency
                 bound-node)))))))

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
  ;; A malformed supplied constraint must not silently turn into an empty
  ;; query basis. nil alone denotes absent optional read consistency.
  (when-not (or (nil? consistency)
                (map? consistency))
    (throw
     (ex "XTDB consistency must be a map or nil."
         {:consistency consistency})))
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
     ;; A present false value is invalid rather than missing. Truthiness-based
     ;; precedence would silently replace it with a weaker legacy constraint.
     (first
      (filter some?
              (map #(get x %)
                   [:gesso.live/consistency
                    :gesso.live.xtdb/consistency
                    :xtdb/consistency
                    :consistency]))))))

(defn query-consistency
  "Return only consistency fields that XTDB2 query opts can use."
  [consistency]
  (select-compact (normalize-consistency consistency)
                  query-consistency-keys))

(defn query-opts
  "Build XTDB2 query opts from explicit query opts only.

   Reject malformed or ambiguous database identities before invoking an
   XTDB read, using the same authority-identity contract as tx-opts.
   Preserve the exact representation of valid caller-supplied database names.
   For consistency-aware reads, use consistent-query-opts or q-consistent."
  [opts]
  (let [read-options (select-compact opts query-option-keys)]
    (when (contains? read-options :database)
      (normalize-database-name (:database read-options)))
    read-options))

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

   Validate an explicitly selected database before invoking XTDB, so an
   ambiguous or malformed identity cannot be rejected only after execution
   while Gesso constructs progression from the returned transaction result.
   Preserve the exact caller-supplied database representation for XTDB.
   Adapter-only options such as :debug-fn are removed."
  [opts]
  (let [tx-options (select-compact opts tx-option-keys)]
    (when (contains? tx-options :database)
      (normalize-database-name (:database tx-options)))
    tx-options))

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

(def post-execute-result-failure-type
  "An XTDB execute-tx call returned, but its result could not establish
   usable transaction metadata. A valid transaction receipt followed by
   conversion failure is :committed; a malformed/unidentifiable returned
   result is :unknown. Neither outcome permits a blind mutation retry."
  ::post-execute-result-failure)

(defn post-execute-result-failure?
  "True when a returned XTDB execute-tx result failed admission/conversion.

   :committed records a valid returned transaction whose metadata failed
   conversion; :unknown records a result with no usable transaction identity.
   Neither is evidence that the XTDB operation failed before execution."
  [error]
  (and (instance? Throwable error)
       (= post-execute-result-failure-type (:error/type (ex-data error)))
       (= :post-execute-result (:failure/stage (ex-data error)))
       (contains? #{:committed :unknown}
                  (:commit/status (ex-data error)))))

(defn- post-execute-result-error
  [cause tx-result]
  (ex-info
   "XTDB execution returned a committed transaction, but result metadata derivation failed."
   {:error/type post-execute-result-failure-type
    :failure/stage :post-execute-result
    :commit/status :committed
    :tx-result tx-result}
   cause))

(defn- require-execute-system-time!
  "A successful XTDB execute-tx result must establish a complete snapshot.

   Unlike submit-tx's accepted-only receipt, execute-tx promises a committed
   TransactionKey with a java.time.Instant system-time. Reject identified
   but incomplete results as committed post-result failures so callers never
   silently render a fragment without the write's required read basis."
  [tx-result]
  (let [system-time (:system-time (tx-result-map tx-result))]
    (when-not (instance? Instant system-time)
      (throw
       (post-execute-result-error
        (ex "XTDB execute-tx result lacks a usable Instant system-time."
            {:system-time system-time})
        tx-result)))
    tx-result))

(def post-submit-result-failure-type
  "XTDB submit-tx returned, but its result could not establish usable
   submission metadata. A valid receipt is :submitted even if its metadata
   conversion fails; an unidentifiable receipt is :unknown, not rejected."
  ::post-submit-result-failure)

(defn post-submit-result-failure?
  "True when an XTDB submission result could not be interpreted.

   :submitted means a usable receipt was returned. :unknown means no usable
   receipt was returned. Neither establishes a committed transaction."
  [error]
  (and (instance? Throwable error)
       (= post-submit-result-failure-type (:error/type (ex-data error)))
       (= :post-submit-result (:failure/stage (ex-data error)))
       (contains? #{:submitted :unknown}
                  (:submission/status (ex-data error)))))

(defn- post-submit-result-error
  [cause tx-result]
  (ex-info
   "XTDB submission returned a receipt, but result metadata derivation failed."
   {:error/type post-submit-result-failure-type
    :failure/stage :post-submit-result
    :submission/status :submitted
    :tx-result tx-result}
   cause))

(defn- require-returned-tx-id!
  "Check whether a returned XTDB API result identifies a transaction.

   XTDB's public submit-tx receipt and execute-tx result have a nonnegative,
   JVM-long transaction id. A missing/malformed id cannot confirm acceptance
   or commit. Preserve the original return in a typed UNKNOWN outcome instead
   of silently reporting success with empty consistency metadata. A caller
   must reconcile the transaction rather than blindly retrying."
  [api tx-result]
  (let [usable? (try
                  (let [tx-id (:tx-id (tx-result-map tx-result))]
                    (and (integer? tx-id)
                         (<= 0 tx-id Long/MAX_VALUE)))
                  (catch Throwable _ false))]
    (when-not usable?
      (throw
       (ex-info
        (str "XTDB " (name api) " returned an unidentifiable transaction result.")
        (merge
         {:tx-result tx-result}
         (case api
           :execute-tx
           {:error/type post-execute-result-failure-type
            :failure/stage :post-execute-result
            :commit/status :unknown}
           :submit-tx
           {:error/type post-submit-result-failure-type
            :failure/stage :post-submit-result
            :submission/status :unknown})))))
    tx-result))

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

   If XTDB returned an identifiable receipt but its metadata cannot be
   converted, post-submit-result-failure? classifies it as :submitted. An
   unidentifiable return is :unknown instead. Neither asserts commit/indexing,
   and neither should be confused with an XTDB API execution exception.

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
       (let [tx-result (invoke-xtdb-api! :submit-tx *submit-tx*
                                         connectable' tx-ops tx-options)
             _ (require-returned-tx-id! :submit-tx tx-result)
             ;; A usable returned receipt establishes submission, not commit. Any
             ;; subsequent metadata failure must preserve that distinction
             ;; instead of looking like the API rejected the transaction.
             {:keys [consistency progression-value]}
             (try
               {:consistency (tx-consistency tx-result)
                :progression-value
                (tx-result-progression tx-result (:database tx-options))}
               (catch Throwable cause
                 (throw (post-submit-result-error cause tx-result))))]
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
          (if (post-submit-result-failure? e)
            :gesso.live.xtdb/submit-tx-result-failed
            :gesso.live.xtdb/submit-tx-failed)
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
   system-time. An identified result missing a java.time.Instant system-time
   is classified as a committed post-execute-result failure; it cannot be
   returned as a successful write without its authoritative read basis.
   The returned consistency includes:

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
       (let [tx-result (invoke-xtdb-api! :execute-tx *execute-tx*
                                         connectable' tx-ops tx-options)
             _ (require-returned-tx-id! :execute-tx tx-result)
             _ (require-execute-system-time! tx-result)
             ;; The XTDB call has returned: any failure deriving its public
             ;; result metadata is post-execution, not an ASSERT conflict.
             {:keys [consistency progression-value]}
             (try
               {:consistency (tx-consistency tx-result)
                :progression-value
                (tx-result-progression tx-result (:database tx-options))}
               (catch Throwable cause
                 (throw (post-execute-result-error cause tx-result))))]
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
          (if (post-execute-result-failure? e)
            :gesso.live.xtdb/execute-tx-result-failed
            :gesso.live.xtdb/execute-tx-failed)
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