(ns gesso.live.consistency.xtdb
  "XTDB2 consistency helpers for gesso.live.

   This namespace is deliberately narrow.

   It provides:

   - XTDB2 connectable/context helpers
   - explicit read-consistency/query option helpers
   - thin q/plan-q task wrappers
   - thin submit-tx/execute-tx task wrappers
   - fragment-key consistency dimensions
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
   [missionary.core :as m]
   [xtdb.api :as xt])
  (:import
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

     :biff/conn
     :biff/node"
  [x]
  (if (map? x)
    (or (:xtdb/connectable x)
        (:xtdb/conn x)
        (:xtdb/node x)
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
     :biff/node"
  [x]
  (if (map? x)
    (or (:xtdb/read-connectable x)
        (:xtdb/conn x)
        (:biff/conn x)
        (:xtdb/connectable x)
        (:xtdb/node x)
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

(defn consistency-fragment-dimension
  "Return a stable value suitable for a fragment key's :consistency-token
   dimension.

   Returns nil when the consistency map is empty."
  [consistency]
  (let [c (normalize-consistency consistency)]
    (when (seq c)
      [:xtdb2/read-consistency c])))

(defn with-consistency-dimension
  "Assoc a consistency dimension into a fragment key dimension map when present."
  [dimensions consistency]
  (if-let [dimension (consistency-fragment-dimension consistency)]
    (assoc (or dimensions {}) :consistency-token dimension)
    (or dimensions {})))

(defn with-consistency-dimension-from
  "Assoc explicit consistency from a context into a fragment key dimension map."
  [dimensions ctx-or-source]
  (with-consistency-dimension dimensions
    (consistency-from ctx-or-source)))

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
  "Run q-consistent against a raw connectable or context map.

   Uses read-connectable-from and consistency-from."
  ([ctx-or-connectable query]
   (q-consistent (read-connectable-from ctx-or-connectable)
                 query
                 (consistency-from ctx-or-connectable)))
  ([ctx-or-connectable query opts]
   (q-consistent (read-connectable-from ctx-or-connectable)
                 query
                 (consistency-from ctx-or-connectable)
                 opts)))

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
  "Run plan-q-consistent against a raw connectable or context map.

   Uses read-connectable-from and consistency-from."
  ([ctx-or-connectable query]
   (plan-q-consistent (read-connectable-from ctx-or-connectable)
                      query
                      (consistency-from ctx-or-connectable)))
  ([ctx-or-connectable query opts]
   (plan-q-consistent (read-connectable-from ctx-or-connectable)
                      query
                      (consistency-from ctx-or-connectable)
                      opts)))

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
             consistency (tx-consistency tx-result)]
         (debug!
          debug-fn
          :gesso.live.xtdb/submit-tx-succeeded
          {:tx-result tx-result
           :consistency consistency
           :at (now-ms)})
         {:tx-result tx-result
          :consistency consistency})
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
      :consistency ...}

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
             consistency (tx-consistency tx-result)]
         (debug!
          debug-fn
          :gesso.live.xtdb/execute-tx-succeeded
          {:tx-result tx-result
           :consistency consistency
           :at (now-ms)})
         {:tx-result tx-result
          :consistency consistency})
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
