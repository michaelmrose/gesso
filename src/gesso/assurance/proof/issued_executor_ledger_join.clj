(ns gesso.assurance.proof.issued-executor-ledger-join
  "v1473: Same-invocation, read-only XTDB ledger join for an issued executor.

   v1467's controlled collector EXECUTES its own transaction. Calling that
   collector after the v1471 issued handler would create TWO transactions and
   could not establish same-execution correspondence. This bridge instead
   invokes v1471 ONCE, installs a synchronous diagnostic callback at the
   adapter's dynamic execute-tx seam, forwards precisely ONE call to the
   originally captured XTDB execute-tx function, then reads XTDB's system
   ledger at a pinned post-submission basis. The readback is not a write.

   Positive local evidence requires v1471's source-checked prepared-plan / 
   adapter-call match, the original physical API's TransactionKey, the same
   caller-supplied node OBJECT, and an exact matching committed ledger row.
   No separate collector execution is submitted. Unknown outcomes are never
   retried. A duplicate callback is rejected BEFORE a second physical write.

   The actual issued handler is newly issued for this diagnostic, not proven
   installed in a router. Adapter bindings and the test node are caller-owned;
   this does not independently authenticate the XTDB backend, transaction
   payload, transaction-internal prestate, application authorization, or all
   external writers. The FULL v1467 snapshot/transition collector is not
   joined or silently credited. This is not a certificate or permission to
   submit transactions. Use only with an explicitly controlled test node."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.issued-executor-physical-plan-correspondence :as issued]
            [gesso.live.consistency.xtdb :as adapter]
            [xtdb.api :as xt])
  (:import (java.time Instant ZonedDateTime)
           (java.util WeakHashMap)
           (xtdb.api TransactionKey)))

(def join-type :gesso.assurance.proof.issued-executor-ledger-join/process-local)
(def join-version 1)

(deftype IssuedLedgerHandle []
  Object
  (toString [_] "#<gesso.assurance/issued-ledger-join-witness>"))

(def ^:private ^WeakHashMap live-witnesses (WeakHashMap.))
(def ^:dynamic ^:private *in-join* false)
(def ^:private original-issued issued/observe-issued-submission!)
(def ^:private original-recheck issued/require-current-issued-submission!)
(def ^:private original-api xt/execute-tx)
(def ^:private original-q xt/q)
(def ^:private adapter-execute-var
  (ns-resolve 'gesso.live.consistency.xtdb '*execute-tx*))
(def ^:private original-adapter-api (var-get adapter-execute-var))

(def ^:private open-obligations
  #{:freshly-issued-handler-not-verified-installed-in-production-router
    :caller-supplied-node-not-authenticated-as-production
    :original-xtdb-api-and-system-ledger-trust
    :physical-payload-and-adapter-binding-not-independently-attested
    :physical-transaction-internal-prestate-not-observed
    :v1467-controlled-full-snapshot-collector-not-joined
    :actual-authorization-and-principal-input-binding
    :complete-physical-writer-inventory
    :unknown-outcome-reconciliation
    :unobserved-alternate-or-asynchronous-write-paths
    :application-transaction-wrapper-and-callback-integrity
    :universal-xtdb-execution-refinement})

(defn- reject! [reason]
  ;; Never expose private request, submitted operations, XTDB key or node.
  (throw (ex-info "Invalid issued-executor physical ledger join."
                  {:issued-executor-ledger-join/rejection true
                   :reason reason})))

(defn- plain-map? [m]
  (and (or (instance? clojure.lang.PersistentHashMap m)
           (instance? clojure.lang.PersistentArrayMap m))
       (nil? (meta m))))

(defn- validate-source! [source]
  (when-not (and (plain-map? source)
                 (= #{:issued-source :node} (set (keys source)))
                 (plain-map? (:issued-source source))
                 (some? (:node source)))
    (reject! :invalid-source))
  (when-not (and (identical? original-issued
                             (var-get #'issued/observe-issued-submission!))
                 (identical? original-recheck
                             (var-get #'issued/require-current-issued-submission!))
                 (identical? original-api (var-get #'xt/execute-tx))
                 (identical? original-q (var-get #'xt/q))
                 (identical? original-adapter-api
                             (.getRawRoot ^clojure.lang.Var adapter-execute-var)))
    (reject! :substituted-observation-or-xtdb-boundary))
  (when (or *in-join* (thread-bound? adapter-execute-var))
    (reject! :nested-or-prebound-execute-boundary))
  source)

(defn- tx-key? [key]
  (and (instance? TransactionKey key)
       (integer? (.getTxId ^TransactionKey key))
       (<= 0 (.getTxId ^TransactionKey key))
       (instance? Instant (.getSystemTime ^TransactionKey key))))

(defn- read-ledger-once! [node ^TransactionKey key]
  ;; One pinned post basis shared by the ledger query. A query failure is
  ;; UNKNOWN readback, not evidence of rollback or permission to retry.
  (try
    (let [tokens (original-q node "SHOW SNAPSHOT_TOKEN")
          token (when (and (sequential? tokens) (= 1 (count tokens)))
                  (:snapshot-token (first tokens)))]
      (if-not (and (string? token) (seq token))
        {:status :missing-pinned-snapshot}
        (let [basis {:snapshot-token token :current-time (Instant/now)}
              rows (original-q node
                               ["SELECT _id, committed, system_time FROM xt.txs WHERE _id = ?"
                                (.getTxId key)]
                               basis)
              row (when (and (sequential? rows) (= 1 (count rows)))
                    (first rows))
              checks {:record-present (map? row)
                      :matching-transaction-id (and (map? row)
                                                    (= (.getTxId key)
                                                       (:xt/id row)))
                      :committed (and (map? row)
                                      (true? (:committed row)))
                      :matching-system-time
                      (and (map? row)
                           (instance? ZonedDateTime (:system-time row))
                           (= (.getSystemTime key)
                              (.toInstant ^ZonedDateTime (:system-time row))))}]
          {:status (if (every? true? (vals checks))
                     :matching-committed-ledger-row
                     :ledger-row-mismatch)
           :checks (update-vals checks boolean)})))
    (catch Throwable _
      {:status :ledger-readback-unavailable})))

(defn- entry [handle]
  (when (instance? IssuedLedgerHandle handle)
    (locking live-witnesses (.get live-witnesses handle))))

(defn issued-ledger-witness? [handle]
  "Is this a live process-local handle? Never use it as authorization."
  (boolean (entry handle)))

(defn observe-issued-ledger-join!
  "ONE newly issued handler invocation and ONE forwarded physical XTDB write,
   followed by a read-only pinned ledger observation on the same node.

   Returns only a REDACTED report and optional live handle. A missing/failed
   report never authorizes retry: even an inconclusive write may have committed.
   The original v1471 return/exception is not exposed because it can contain
   private data. This is a controlled diagnostic, NOT a production writer."
  [source]
  (validate-source! source)
  (let [node (:node source)
        thread (Thread/currentThread)
        attempts (atom 0)
        physical (atom nil)
        observed-execute
        (fn [actual-node tx-ops tx-options]
          (let [count (swap! attempts inc)]
            (when-not (identical? thread (Thread/currentThread))
              (reject! :cross-thread-physical-call))
            (when-not (= 1 count)
              (reject! :duplicate-physical-call-before-submission))
            (when-not (identical? node actual-node)
              (reject! :different-physical-node))
            ;; Invoke the ORIGINAL XTDB function once, not the controlled
            ;; collector's active submission path. Do not catch/retry.
            (let [key (original-api actual-node tx-ops tx-options)]
              (reset! physical {:key key})
              key)))
        issued-result (binding [*in-join* true]
                        (with-bindings {adapter-execute-var observed-execute}
                          (original-issued (:issued-source source))))
        issued-report (:report issued-result)
        issued-handle (:handle issued-result)
        key (:key @physical)
        ledger (if (and (= 1 @attempts) issued-handle (tx-key? key))
                 (read-ledger-once! node key)
                 {:status :not-read})
        good? (and (= 1 @attempts)
                   (some? issued-handle)
                   (tx-key? key)
                   (= :matching-committed-ledger-row (:status ledger))
                   (= :returned (get-in issued-result [:execution :outcome])))
        report {:type join-type
                :version join-version
                :operation-id (:operation-id issued-report)
                :status (cond
                          (not= 1 @attempts) :missing-or-repeated-physical-call
                          (nil? issued-handle) :issued-plan-correspondence-incomplete
                          (not (tx-key? key)) :no-physical-transaction-key
                          (not= :matching-committed-ledger-row (:status ledger))
                          :ledger-readback-not-established
                          (not good?) :handler-outcome-not-established
                          :else :bounded-issued-plan-and-physical-ledger-agreement)
                :one-physical-call-attempted? (= 1 @attempts)
                :issued-plan-adapter-correspondence?
                (= :bounded-issued-executor-to-adapter-plan-agreement
                   (:status issued-report))
                :original-xtdb-api-invoked? (boolean @physical)
                :ledger-readback-status (:status ledger)
                :ledger-checks (:checks ledger)
                :same-physical-execution (if good?
                                           :one-issued-handler-and-one-xtdb-call
                                           :not-established)
                :controlled-v1467-collector :not-executed
                :independent-physical-receipt :not-authenticated
                :original-assurance-report-status (:status issued-report)
                :open-obligations (set/union open-obligations
                                             (:open-obligations issued-report))
                :scope :one-controlled-single-node-process-local-execution
                :proof-status :bounded-readback-diagnostic-only
                :certificate :not-issued
                :admission :not-admitted
                :transaction-authority :none
                :authority :none}
        handle (when good? (IssuedLedgerHandle.))]
    (when handle
      (locking live-witnesses
        (.put live-witnesses handle
              {:source source
               :issued-handle issued-handle
               :issued-report issued-report
               :report report})))
    {:report report :handle handle}))

(defn require-current-issued-ledger-join!
  "Validate the local handle and original source identity without new XTDB
   reads or writes. The report is a COPYABLE diagnostic, not a credential."
  [handle source prior-report]
  (let [witness (or (entry handle) (reject! :unknown-or-forged-witness))]
    (validate-source! source)
    (when-not (identical? (:node (:source witness)) (:node source))
      (reject! :node-substitution))
    (original-recheck (:issued-handle witness)
                      (:issued-source source)
                      (:issued-report witness))
    (when-not (and (plain-map? prior-report)
                   (= prior-report (:report witness)))
      (reject! :stale-or-altered-join-report))
    (:report witness)))
