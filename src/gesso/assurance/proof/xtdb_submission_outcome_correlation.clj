(ns gesso.assurance.proof.xtdb-submission-outcome-correlation
  "v1392: Correlate ONE adapter-observed XTDB call with public transaction
   identity and a pinned, bounded xt.txs read. This is diagnostic acquisition:
   neither the adapter observation nor the caller-supplied node is authenticated.

   submit-tx's tx-id alone never establishes commit. execute-tx's returned
   TransactionKey exposes tx-id/system-time; a matching xt.txs committed row
   corroborates those reported values, not the submitted payload, internal
   prestate, authorizing guards, deployed handler, or other writers. Exception
   outcomes remain indeterminate (including assertion rejections) because the
   v1390 :threw event deliberately records only the exception class.

   The wrapper runs the submitted function EXACTLY ONCE, without retrying or
   replacing its result. Post-execution diagnostic failures do not transform an
   authoritative return/exception into a different execution outcome."
  (:require [gesso.live.consistency.xtdb :as live]
            [xtdb.api :as xt])
  (:import [java.time Instant ZonedDateTime]
           [xtdb.api TransactionKey]))

(def report-type
  :gesso.assurance.proof.xtdb-submission-outcome-correlation/report)
(def report-version 1)

(def ^:private open-obligations
  #{:authenticated-submitted-payload
    :authenticated-transaction-internal-prestate
    :actual-inputs-and-authorization-to-issued-executor-binding
    :complete-executed-physical-write-footprint
    :transaction-commit-and-conflict-outcome-authenticity
    :deployed-handler-and-route-adoption
    :all-physical-writer-paths-covered
    :independent-abstract-to-physical-semantics-equivalence
    :universal-physical-execution-refinement})

(defn- reject! [reason]
  (throw (ex-info "Invalid submission-outcome observation invocation."
                  {:submission-outcome-correlation/rejection true
                   :reason reason})))

(defn- tx-id? [x]
  (and (integer? x) (<= 0 x) (<= x Long/MAX_VALUE)))

(defn- known-key [api result]
  ;; The adapter passes the original public API result, not a reconstructed
  ;; client receipt. Keep unrecognized shapes UNKNOWN; never guess identifiers.
  (cond
    (and (= api :submit-tx) (map? result)
         (tx-id? (:tx-id result)))
    {:tx-id (:tx-id result) :system-time nil}

    (and (= api :execute-tx) (instance? TransactionKey result))
    (let [id (.getTxId ^TransactionKey result)
          time (.getSystemTime ^TransactionKey result)]
      (when (and (tx-id? id) (instance? Instant time))
        {:tx-id id :system-time time}))

    :else nil))

(defn- event-pair [capture]
  (let [events (:events capture)]
    (when (and (= live/submission-capture-type (:type capture))
               (= live/submission-capture-version (:version capture))
               (= :instrumented-xtdb-adapter-call-site
                  (:observation-origin capture))
               (false? (:actual-xtdb-api-authenticated? capture))
               (vector? events) (= 2 (count events)))
      (let [[started finished] events]
        (when (and (map? started) (map? finished)
                   (#{:submit-tx :execute-tx} (:api started))
                   (= (:api started) (:api finished))
                   (= :invoked (:phase started))
                   (#{:returned :threw} (:phase finished))
                   (uuid? (:call-id started))
                   (= (:call-id started) (:call-id finished))
                   (vector? (:tx-ops started))
                   (map? (:tx-options started))
                   (if (= :returned (:phase finished))
                     (contains? finished :tx-result)
                     (and (string? (:exception-class finished))
                          (not (contains? finished :tx-result)))))
          [started finished])))))

(defn- ledger-row [node {:keys [tx-id]}]
  ;; Reuse a fixed snapshot token and clock for this ledger phase. This read
  ;; follows execution and does not attest the transaction's INTERNAL basis.
  (let [token-rows (xt/q node "SHOW SNAPSHOT_TOKEN")
        token (when (and (sequential? token-rows) (= 1 (count token-rows)))
                (:snapshot-token (first token-rows)))]
    (if-not (and (string? token) (seq token))
      {:status :unavailable-snapshot-token}
      (let [rows (xt/q node
                       ["SELECT _id, committed, system_time FROM xt.txs WHERE _id = ?"
                        tx-id]
                       {:snapshot-token token :current-time (Instant/now)})]
        (cond
          (not (sequential? rows)) {:status :invalid-ledger-result}
          (empty? rows) {:status :not-visible}
          (not= 1 (count rows)) {:status :ambiguous-ledger-result}
          (not (map? (first rows))) {:status :invalid-ledger-result}
          :else {:status :observed :row (first rows)})))))

(defn- ledger-comparison [node {:keys [tx-id system-time] :as key}]
  (try
    (let [{:keys [status row]} (ledger-row node key)]
      (if (= :observed status)
        (let [observed-time (:system-time row)
              normalized-time (cond
                                (instance? Instant observed-time) observed-time
                                (instance? ZonedDateTime observed-time)
                                (.toInstant ^ZonedDateTime observed-time)
                                :else nil)
              checks {:tx-id (= tx-id (:xt/id row))
                      :committed (= true (:committed row))
                      :system-time (= system-time normalized-time)}]
          {:status (if (every? true? (vals checks))
                     :reported-key-matches-committed-ledger-row
                     :reported-key-diverges-from-ledger-row)
           :checks checks})
        {:status status :checks {}}))
    (catch Throwable _
      ;; Do not convert a query exception after submission into an apparent
      ;; submission failure, or leak potentially sensitive exception details.
      {:status :ledger-query-failed :checks {}})))

(defn- correlate [node capture]
  (let [events (:events capture)
        pair (event-pair capture)
        [started finished] pair
        api (:api started)
        key (when (= :returned (:phase finished))
              (known-key api (:tx-result finished)))
        outer-outcome (:outcome capture)
        outer-result (get-in capture [:result :tx-result])
        outer-agrees? (and (= :returned outer-outcome)
                           (map? (:result capture))
                           (= (:tx-result finished) outer-result))
        ledger (when (and pair (= api :execute-tx) key outer-agrees?)
                 (ledger-comparison node key))
        status (cond
                 (not (and (vector? events) (= 2 (count events))))
                 :not-one-complete-api-call
                 (not pair) :malformed-api-event-pair
                 (= :threw (:phase finished)) :api-threw-outcome-unresolved
                 (nil? key) :unrecognized-public-transaction-identity
                 (not outer-agrees?) :outer-result-diverges-from-api-return
                 (= api :submit-tx) :submission-id-only
                 (= :reported-key-matches-committed-ledger-row (:status ledger))
                 :reported-execute-ledger-agrees
                 :else :reported-execute-ledger-unconfirmed)]
    {:type report-type
     :version report-version
     :status status
     :api api
     :call-id (:call-id started)
     :reported-tx-id (:tx-id key)
     :reported-system-time (:system-time key)
     :api-completion (if pair (:phase finished) :unverified)
     :thunk-outcome outer-outcome
     :ledger-observation (or ledger {:status :not-applicable :checks {}})
     :observation-origin :process-local-adapter-and-caller-node
     :actual-xtdb-api-authenticated? false
     :node-identical-to-submission-connectable :not-established
     :transaction-internal-prestate :not-established
     :complete-writer-coverage :not-established
     :open-obligations open-obligations
     :proof-status :diagnostic-only
     :certificate :not-issued
     :admission :not-admitted
     :authority :none}))

(defn observe-one-submission!
  "Run thunk once inside an adapter call-site capture and correlate one
   observed XTDB API call with only its public result/identity. For a returned
   execute-tx TransactionKey, inspect xt.txs at a pinned subsequent basis.

   Returns {:execution {:outcome :returned :result x} | {:outcome :threw
   :error throwable}, :diagnostic <sanitized map>}. The execution value is
   returned separately so callers can preserve the original result/exception.
   It may contain private app data; do not log or publish it. The diagnostic
   intentionally contains no operation payloads or exception text.

   Node comes from the caller and is NOT attested to be the same connection
   used by thunk. Other writers and async calls remain outside this scope."
  [node thunk]
  (when (nil? node) (reject! :missing-observation-node))
  (when-not (fn? thunk) (reject! :invalid-submission-thunk))
  (let [captured (live/capture-tx-api-calls! thunk)
        execution (if (= :returned (:outcome captured))
                    {:outcome :returned :result (:result captured)}
                    {:outcome :threw :error (:error captured)})]
    {:execution execution
     :diagnostic (correlate node captured)}))
