(ns gesso.live.consistency.xtdb
  "XTDB2 consistency helpers for gesso.live.

   This namespace is deliberately narrow.

   It provides:

   - XTDB2 connectable/context helpers
   - explicit read-consistency/query option helpers
   - await-token helpers for XTDB2 DataSource values
   - thin q/plan-q task wrappers
   - thin submit-tx/execute-tx task wrappers
   - fragment-key consistency dimensions

   It does not:

   - listen to XTDB transaction logs
   - use XTDB v1 tx listeners
   - use open-tx-log
   - publish live invalidations
   - depend on gesso.live.core
   - decide what changed in the application

   The app write path should remain explicit:

     write to XTDB2
     -> app constructs a primary change
     -> app calls gesso.live.core/submit-expanded! or emit-expanded!

   Live fragment reads should use q-consistent-from / plan-q-consistent-from
   rather than a request-scoped connection when read-after-write visibility
   matters."
  (:require
   [missionary.core :as m]
   [xtdb.api :as xt])
  (:import
   [xtdb.api DataSource]))

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

(def tx-consistency-keys
  [:tx-id
   :system-time
   :await-token])

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
  [:debug-fn
   :await-token-source])

(def default-options
  {:debug-fn nil
   :await-token-source nil})

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

(defn xtdb-datasource?
  "Return true when x is an XTDB2 DataSource that supports await-token methods."
  [x]
  (instance? DataSource x))

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

;; -----------------------------------------------------------------------------
;; Context helpers
;; -----------------------------------------------------------------------------

(defn connectable-from
  "Return a general XTDB2 connectable from either a raw connectable or a context
   map.

   This is the raw/general lookup. It may return a request-scoped connection
   such as :biff/conn or :xtdb/conn.

   Preferred context keys:

     :xtdb/connectable
     :xtdb/conn
     :xtdb/node

   Biff-compatible fallback keys:

     :biff/conn
     :biff/node

   For live fragment reads where read-after-write visibility matters, prefer
   read-connectable-from or q-consistent-from."
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

   This intentionally prefers node/DataSource-like values over request-scoped
   connections, because request ctx connections can carry stale read state.

   Preferred context keys:

     :xtdb/read-connectable
     :xtdb/node
     :xtdb/connectable
     :biff/node
     :xtdb/conn
     :biff/conn"
  [x]
  (if (map? x)
    (or (:xtdb/read-connectable x)
        (:xtdb/node x)
        (:xtdb/connectable x)
        (:biff/node x)
        (:xtdb/conn x)
        (:biff/conn x))
    x))

(defn await-token-source-from
  "Return the preferred XTDB2 DataSource used for await-token tracking.

   Preferred context keys:

     :xtdb/await-token-source
     :xtdb/node
     :xtdb/connectable
     :xtdb/read-connectable

   Biff-compatible fallback:

     :biff/node

   Request-scoped connections such as :biff/conn are intentionally not used
   here."
  [x]
  (if (map? x)
    (or (:xtdb/await-token-source x)
        (:xtdb/node x)
        (:xtdb/connectable x)
        (:xtdb/read-connectable x)
        (:biff/node x))
    x))

;; -----------------------------------------------------------------------------
;; Await token helpers
;; -----------------------------------------------------------------------------

(defn can-use-await-token?
  "Return true if x can store/read XTDB2 await tokens."
  [x]
  (xtdb-datasource? x))

(defn current-await-token
  "Return the current await token from an XTDB2 DataSource, or nil."
  [await-token-source]
  (when (can-use-await-token? await-token-source)
    (.getAwaitToken ^DataSource await-token-source)))

(defn apply-await-token!
  "Strictly apply an await token to an XTDB2 DataSource.

   Passing nil token is a no-op. Passing a non-nil token with a non-XTDB2
   DataSource is an error."
  [await-token-source token]
  (cond
    (nil? token)
    await-token-source

    (can-use-await-token? await-token-source)
    (do
      (.setAwaitToken ^DataSource await-token-source token)
      await-token-source)

    :else
    (throw
     (ex "Cannot apply XTDB2 await token to non-XTDB2 DataSource."
         {:await-token-source await-token-source
          :token token}))))

(defn try-apply-await-token!
  "Best-effort await-token application.

   Returns true when the token was applied, false when no compatible
   await-token source was available.

   Passing nil token returns false."
  [await-token-source token]
  (boolean
   (when (and token (can-use-await-token? await-token-source))
     (.setAwaitToken ^DataSource await-token-source token)
     true)))

(defn current-consistency
  "Return the current read consistency visible from an await-token source.

   Currently this captures only :await-token because that is the mutable
   consistency value exposed by XTDB2 DataSource."
  [await-token-source]
  (compact-map
   {:await-token (current-await-token await-token-source)}))

;; -----------------------------------------------------------------------------
;; Consistency maps and query options
;; -----------------------------------------------------------------------------

(defn normalize-consistency
  "Normalize a user/app consistency map.

   Accepted keys:

     :await-token
     :snapshot-token
     :snapshot-time
     :current-time
     :default-tz
     :tx-id
     :system-time

   Extra keys are discarded."
  [consistency]
  (select-compact consistency
                  (concat query-consistency-keys
                          tx-consistency-keys)))

(defn consistency-from
  "Return consistency from a raw await-token source or context map.

   Context precedence:

     :gesso.live/consistency
     :gesso.live.xtdb/consistency
     :xtdb/consistency
     :consistency
     current await-token from await-token-source-from

   This is the helper live fragment reads should use when the browser/request
   does not provide an explicit consistency token."
  [x]
  (normalize-consistency
   (if (map? x)
     (or (:gesso.live/consistency x)
         (:gesso.live.xtdb/consistency x)
         (:xtdb/consistency x)
         (:consistency x)
         (current-consistency (await-token-source-from x)))
     (current-consistency x))))

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

   Consistency-derived values are applied first. Explicit opts win."
  ([consistency]
   (consistent-query-opts consistency nil))
  ([consistency opts]
   (merge
    (query-consistency consistency)
    (query-opts opts))))

(defn tx-opts
  "Return only options intended for XTDB2 submit-tx/execute-tx.

   Adapter-only options such as :debug-fn and :await-token-source are removed."
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
  "Assoc consistency from a context/source into a fragment key dimension map."
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
   (xt/q (require-connectable! connectable) query))
  ([connectable query opts]
   (xt/q (require-connectable! connectable)
         query
         (query-opts opts))))

(defn q-from
  "Run plain q against a raw connectable or context map.

   This uses connectable-from and may use request-scoped connections. For live
   fragment reads, prefer q-consistent-from."
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
   (xt/q (require-connectable! connectable)
         query
         (consistent-query-opts consistency opts))))

(defn q-consistent-from
  "Run q-consistent against a raw connectable or context map.

   Unlike q-from, this uses read-connectable-from and consistency-from, making it
   the safer default for live fragment reads."
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
   (xt/plan-q (require-connectable! connectable) query))
  ([connectable query opts]
   (xt/plan-q (require-connectable! connectable)
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
   (xt/plan-q (require-connectable! connectable)
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

   submit-tx returns at least :tx-id.
   execute-tx returns transaction details including tx/system-time in XTDB2.

   This helper intentionally avoids XTDB v1 tx/basis concepts."
  [tx-result]
  (select-compact tx-result
                  [:tx-id
                   :system-time]))

(defn tx-consistency
  "Build a consistency map from a tx result and optional await-token source."
  ([tx-result]
   (tx-consistency tx-result nil))
  ([tx-result await-token-source]
   (compact-map
    (merge
     (tx-result-consistency tx-result)
     {:await-token (current-await-token await-token-source)}))))

(defn- tx-await-token-source
  [connectable opts]
  (let [explicit (:await-token-source opts)]
    (cond
      (can-use-await-token? explicit)
      explicit

      (can-use-await-token? connectable)
      connectable

      :else
      nil)))

;; -----------------------------------------------------------------------------
;; Transaction wrappers
;; -----------------------------------------------------------------------------

(defn submit-tx!
  "Submit XTDB2 tx ops.

   Returns:

     {:tx-result ...
      :consistency ...}

   Options may include XTDB2 submit-tx opts plus adapter opts:

     :await-token-source
       Optional XTDB2 DataSource used to capture the current await token after
       the write. Useful when connectable is a JDBC Connection.

     :debug-fn
       Optional pay-for-play debug hook.

   This function does not emit live invalidations."
  ([connectable tx-ops]
   (submit-tx! connectable tx-ops nil))
  ([connectable tx-ops opts]
   (let [connectable' (require-connectable! connectable)
         adapter-options (prepare-options! opts)
         debug-fn (:debug-fn adapter-options)]
     (debug!
      debug-fn
      :gesso.live.xtdb/submit-tx-started
      {:at (now-ms)})
     (try
       (let [tx-result (xt/submit-tx connectable' tx-ops (tx-opts opts))
             await-source (tx-await-token-source connectable' adapter-options)
             consistency (tx-consistency tx-result await-source)]
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

   Uses connectable-from for the write and await-token-source-from for returned
   consistency unless :await-token-source is explicitly supplied."
  ([ctx-or-connectable tx-ops]
   (submit-tx-from! ctx-or-connectable tx-ops nil))
  ([ctx-or-connectable tx-ops opts]
   (let [opts' (if (contains? (or opts {}) :await-token-source)
                 opts
                 (assoc (or opts {})
                        :await-token-source
                        (await-token-source-from ctx-or-connectable)))]
     (submit-tx! (connectable-from ctx-or-connectable)
                 tx-ops
                 opts'))))

(defn execute-tx!
  "Execute XTDB2 tx ops and wait for the receiving node to index them.

   Returns:

     {:tx-result ...
      :consistency ...}

   Options may include XTDB2 execute-tx opts plus adapter opts:

     :await-token-source
       Optional XTDB2 DataSource used to capture the current await token after
       the write. Useful when connectable is a JDBC Connection.

     :debug-fn
       Optional pay-for-play debug hook.

   This function does not emit live invalidations."
  ([connectable tx-ops]
   (execute-tx! connectable tx-ops nil))
  ([connectable tx-ops opts]
   (let [connectable' (require-connectable! connectable)
         adapter-options (prepare-options! opts)
         debug-fn (:debug-fn adapter-options)]
     (debug!
      debug-fn
      :gesso.live.xtdb/execute-tx-started
      {:at (now-ms)})
     (try
       (let [tx-result (xt/execute-tx connectable' tx-ops (tx-opts opts))
             await-source (tx-await-token-source connectable' adapter-options)
             consistency (tx-consistency tx-result await-source)]
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

   Uses connectable-from for the write and await-token-source-from for returned
   consistency unless :await-token-source is explicitly supplied."
  ([ctx-or-connectable tx-ops]
   (execute-tx-from! ctx-or-connectable tx-ops nil))
  ([ctx-or-connectable tx-ops opts]
   (let [opts' (if (contains? (or opts {}) :await-token-source)
                 opts
                 (assoc (or opts {})
                        :await-token-source
                        (await-token-source-from ctx-or-connectable)))]
     (execute-tx! (connectable-from ctx-or-connectable)
                  tx-ops
                  opts'))))

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
  "Build a XTDB2 :put-docs tx op."
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
  "Build a XTDB2 :delete-docs tx op."
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
