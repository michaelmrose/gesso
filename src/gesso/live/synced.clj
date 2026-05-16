(ns gesso.live.synced
  "Ergonomic helpers for simple persisted live values.

   This namespace intentionally does not depend on gesso.live.core, so core can
   safely require and re-export these helpers.

   It provides the low-level descriptor/read/tx/change helpers behind the
   app-facing core facade:

     live/->synced
     live/live-read
     live/live-set!
     live/live-swap!

   A synced value describes one persisted XTDB document attribute that can be
   read, set, swapped, and announced through gesso.live."
  (:require
   [clojure.string :as str]
   [gesso.live.consistency.xtdb :as live.xtdb]))

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn- ex
  [message data]
  (ex-info message data))

(defn- require-present!
  [k value]
  (when (nil? value)
    (throw
     (ex (str "gesso.live synced value requires " k ".")
         {k value})))
  value)

(defn- normalize-name
  [x]
  (cond
    (keyword? x) (name x)
    (symbol? x)  (name x)
    (string? x)  x
    :else        (str x)))

(defn- sql-ident?
  [s]
  (and (string? s)
       (boolean
        (re-matches #"[A-Za-z_][A-Za-z0-9_]*" s))))

(defn- require-sql-ident!
  [kind x]
  (let [s (normalize-name x)]
    (when-not (sql-ident? s)
      (throw
       (ex "gesso.live synced value requires a simple SQL identifier."
           {:kind kind
            :value x
            :normalized s})))
    s))

(defn- attr-sql-name
  [attr]
  (cond
    (keyword? attr)
    (if-let [ns (namespace attr)]
      (str ns "$" (name attr))
      (name attr))

    (symbol? attr)
    (if-let [ns (namespace attr)]
      (str ns "$" (name attr))
      (name attr))

    (string? attr)
    attr

    :else
    (str attr)))

(defn- require-sql-column!
  [attr]
  (let [s (attr-sql-name attr)]
    (when-not (boolean
               (re-matches #"[A-Za-z_][A-Za-z0-9_$]*" s))
      (throw
       (ex "gesso.live synced value requires a simple SQL column identifier."
           {:attr attr
            :normalized s})))
    s))

(defn- sql-column-key
  [attr]
  (keyword (attr-sql-name attr)))

(defn- possible-row-keys
  [attr]
  (distinct
   (remove nil?
           [attr
            (sql-column-key attr)
            (when (keyword? attr)
              (keyword (name attr)))
            (when (keyword? attr)
              (keyword (namespace attr) (name attr)))])))

(defn- compact-map
  [m]
  (into {}
        (remove (comp nil? val))
        m))

;; -----------------------------------------------------------------------------
;; Descriptor
;; -----------------------------------------------------------------------------

(defn ->synced
  "Define one persisted live value.

   Required keys:
     :table
       XTDB table keyword/symbol/string.

     :id
       XTDB document id.

     :col
       Document attribute to read/write, e.g. :demo/value.

   Optional keys:
     :topic
       Live invalidation topic. Defaults to :entity-type, then :table.

     :entity-type
       Compatibility alias for :topic.

     :default
       Value returned by live-read when the row/column is absent. Defaults nil.

     :change/kind
       Defaults to :updated.

     :coalesce-key
       Dispatch coalescing key. Defaults to [topic id].

   Example:

     (live/->synced
      {:table :demo_counters
       :id \"global-shared-counter\"
       :col :demo/value
       :topic :demo-counter
       :default 0})"
  [{:keys [table id col topic entity-type default coalesce-key]
    :change/keys [kind]
    :as spec}]
  (let [table' (require-present! :table table)
        id'    (require-present! :id id)
        col'   (require-present! :col col)
        topic' (or topic entity-type table')
        kind'  (or kind :updated)]
    ;; Validate now so descriptor bugs fail at definition/eval time.
    (require-sql-ident! :table table')
    (require-sql-column! col')
    {:gesso.live.synced/type :gesso.live.synced/value
     :table table'
     :id id'
     :col col'
     :topic topic'
     :default default
     :change/kind kind'
     :coalesce-key (or coalesce-key [topic' id'])
     :spec spec}))

(defn synced?
  [x]
  (= :gesso.live.synced/value
     (:gesso.live.synced/type x)))

(defn require-synced!
  [x]
  (when-not (synced? x)
    (throw
     (ex "Expected a gesso.live synced value descriptor."
         {:value x})))
  x)

;; -----------------------------------------------------------------------------
;; Query/read helpers
;; -----------------------------------------------------------------------------

(defn query
  "Return the XTDB SQL query for a synced value."
  [synced]
  (let [{:keys [table id col]} (require-synced! synced)
        table-sql (require-sql-ident! :table table)
        col-sql   (require-sql-column! col)]
    [(str "SELECT _id, " col-sql
          " FROM " table-sql
          " WHERE _id = ?")
     id]))

(defn value-from-row
  "Extract the synced value from one XTDB query row.

   Handles both XTDB-normalized :demo/value style keys and SQL-column
   :demo$value style keys."
  [synced row]
  (let [{:keys [col default]} (require-synced! synced)]
    (if row
      (let [sentinel ::missing
            value (reduce
                   (fn [_ k]
                     (let [v (get row k sentinel)]
                       (if (identical? sentinel v)
                         sentinel
                         (reduced v))))
                   sentinel
                   (possible-row-keys col))]
        (if (identical? sentinel value)
          default
          value))
      default)))

(defn live-read
  "Read the current value through the explicit XTDB consistency path.

   This uses gesso.live.consistency.xtdb/q-consistent-from directly. The public
   facade re-export is live/live-read."
  ([ctx synced]
   (live-read ctx synced nil))
  ([ctx synced opts]
   (let [rows (if opts
                (live.xtdb/q-consistent-from ctx (query synced) opts)
                (live.xtdb/q-consistent-from ctx (query synced)))]
     (value-from-row synced (first rows)))))

;; -----------------------------------------------------------------------------
;; Write/change helpers
;; -----------------------------------------------------------------------------

(defn doc
  "Build the XTDB document for setting a synced value."
  [synced value]
  (let [{:keys [id col]} (require-synced! synced)]
    {:xt/id id
     col value}))

(defn tx-ops
  "Build XTDB tx ops for setting a synced value."
  [synced value]
  (let [{:keys [table]} (require-synced! synced)]
    [(live.xtdb/put-docs-op table (doc synced value))]))

(defn change
  "Build the primary live change for a synced value.

   Options:
     :data
       Optional change data.

     :change/kind
       Override descriptor change kind.

     :old-value
       Optional old value.

     :new-value
       Optional new value. Defaults to value."
  ([synced value]
   (change synced value nil))
  ([synced value options]
   (let [{:keys [topic id] :change/keys [kind]} (require-synced! synced)
         kind' (or (:change/kind options) kind)
         data' (:data options)
         old-value (:old-value options)
         new-value (if (contains? options :new-value)
                     (:new-value options)
                     value)]
     (compact-map
      {:topic topic
       :id id
       :change/kind kind'
       :old-value old-value
       :new-value new-value
       :data data'}))))

(defn entry
  "Build default dispatch entry metadata for a synced value."
  [synced]
  (let [{:keys [coalesce-key]} (require-synced! synced)]
    (when coalesce-key
      {:coalesce-key coalesce-key})))
