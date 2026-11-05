(ns gesso.assurance.proof.committed-write-ledger-correspondence
  "v1423: Correlate one controlled v1421 write/frame acquisition with a
   SEPARATELY PINNED, subsequent xt.txs ledger read. The complete public
   execute-tx TransactionKey and the observed committed ledger row must agree
   before a bounded, diagnostic co-observation can be reported.

   The pre/post document read bases and ledger basis are separate snapshots.
   Comparing their reported times does NOT prove the transaction executed
   against the pre-read state, that this attempt caused the poststate, or that
   no other writer modified an unobserved document. NEVER retry a submission.
   Detailed :source contains private command payloads and snapshot tokens;
   publish ONLY redacted-summary. No certificate or settlement authority."
  (:require [gesso.assurance.proof.controlled-xtdb-write-accounting :as accounting]
            [gesso.live.consistency.xtdb :as live]
            [xtdb.api :as xt]
            [xtdb.basis :as basis])
  (:import [java.time Instant ZonedDateTime]
           [xtdb.api TransactionKey]))

(def report-type :gesso.assurance.proof.committed-write-ledger-correspondence/diagnostic)
(def report-version 1)
(def ^:private original-acquire accounting/acquire-controlled-xtdb-write-accounting!)
(def ^:private original-check accounting/require-current-controlled-xtdb-write-accounting!)
(def ^:private original-query xt/q)
(def ^:private original-decode basis/<-time-basis-str)
(def ^:private original-public-consistency live/tx-result-consistency)

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

(defn- reject! [reason at]
  (throw (ex-info "Invalid committed write/ledger correspondence diagnostic."
                  {:committed-write-ledger-correspondence/rejection true
                   :reason reason :at at})))

(defn- plain-map? [m]
  (and (or (instance? clojure.lang.PersistentArrayMap m)
           (instance? clojure.lang.PersistentHashMap m))
       (nil? (meta m))))

(defn- shape! [m ks at]
  (when-not (and (plain-map? m) (= ks (set (keys m))))
    (reject! :invalid-map-shape at))
  m)

(defn- seams! []
  (when-not (and (identical? original-acquire
                             (var-get #'accounting/acquire-controlled-xtdb-write-accounting!))
                 (identical? original-check
                             (var-get #'accounting/require-current-controlled-xtdb-write-accounting!))
                 (identical? original-query (var-get #'xt/q))
                 (identical? original-decode (var-get #'basis/<-time-basis-str))
                 (identical? original-public-consistency
                             (var-get #'live/tx-result-consistency)))
    (reject! :substituted-acquisition-or-validation-seam [])))

(defn- token-point [token]
  ;; An invalid token is rejected; tokens with multiple databases or multiple
  ;; times are valid but NOT comparable for a positive ordering observation.
  (when-not (and (string? token) (seq token))
    (reject! :invalid-snapshot-token [:read-basis]))
  (let [decoded (try (original-decode token)
                     (catch Throwable _ (reject! :invalid-snapshot-token [:read-basis])))]
    (when-not (map? decoded)
      (reject! :invalid-snapshot-token [:read-basis]))
    (when (= 1 (count decoded))
      (let [[database times] (first decoded)]
        (when (and (some? times) (= 1 (count times))
                   (instance? Instant (first times)))
          {:database database :system-time (first times)})))))

(defn- instant [x]
  (cond (instance? Instant x) x
        (instance? ZonedDateTime x) (.toInstant ^ZonedDateTime x)
        :else nil))

(defn- valid-tx-id? [x]
  (and (integer? x) (<= 0 x) (<= x Long/MAX_VALUE)))

(defn- captured-key [capture]
  (let [{:keys [events outcome result]} capture]
    (if-not (and (vector? events) (= 2 (count events))
                 (= :returned outcome))
      {:status :no-completed-attempt-key}
      (let [[invoked returned] events
            returned-value (:tx-result returned)
            api (:api invoked)]
        (cond
          (not (and (plain-map? invoked) (plain-map? returned)
                    (= :invoked (:phase invoked))
                    (= :returned (:phase returned))
                    (uuid? (:call-id invoked))
                    (= (:call-id invoked) (:call-id returned))
                    (= api (:api returned))
                    (plain-map? result)
                    (= returned-value (:tx-result result))))
          {:status :incoherent-adapter-return}

          (= api :submit-tx)
          {:status :submission-id-only}

          (not= api :execute-tx)
          {:status :unsupported-adapter-api}

          (not (instance? TransactionKey returned-value))
          {:status :not-public-transaction-key}

          :else
          (let [reported (try (original-public-consistency returned-value)
                              (catch Throwable _ nil))
                id (.getTxId ^TransactionKey returned-value)
                system-time (.getSystemTime ^TransactionKey returned-value)]
            (if (and (valid-tx-id? id) (instance? Instant system-time)
                     (= id (:tx-id reported))
                     (= system-time (:system-time reported)))
              {:status :complete-execute-key
               :tx-id id :system-time system-time}
              {:status :not-public-transaction-key})))))))

(defn- read-ledger! [node key]
  ;; No transaction is invoked here. The ledger is read AFTER the controlled
  ;; accounting helper has already completed its own poststate reads.
  (if-not (= :complete-execute-key (:status key))
    {:status :not-applicable :snapshot-token nil :rows []}
    (try
      (let [token-result (original-query node "SHOW SNAPSHOT_TOKEN")
            token (when (and (sequential? token-result)
                             (= 1 (count token-result))
                             (plain-map? (first token-result)))
                    (:snapshot-token (first token-result)))]
        (if-not (and (string? token) (seq token))
          {:status :snapshot-unavailable :snapshot-token nil :rows []}
          (let [rows (original-query node
                                     ["SELECT _id, committed, system_time FROM xt.txs WHERE _id = ?"
                                      (:tx-id key)]
                                     {:snapshot-token token :current-time (Instant/now)})]
            (if (and (sequential? rows) (<= (count rows) 2))
              {:status :queried :snapshot-token token :rows (vec rows)}
              {:status :invalid-ledger-result :snapshot-token nil :rows []}))))
      (catch Throwable _
        ;; Never re-submit after a ledger read failure or unknown result.
        {:status :ledger-query-failed :snapshot-token nil :rows []}))))

(defn- ledger-match [observation key]
  (shape! observation #{:status :snapshot-token :rows} [:ledger-observation])
  (let [{:keys [status snapshot-token rows]} observation]
    (when-not (and (#{:not-applicable :snapshot-unavailable :queried
                     :invalid-ledger-result :ledger-query-failed} status)
                   (vector? rows) (nil? (meta rows))
                   (if (= :queried status)
                     (and (string? snapshot-token) (seq snapshot-token) (<= (count rows) 2))
                     (and (nil? snapshot-token) (empty? rows))))
      (reject! :invalid-ledger-observation [:ledger-observation]))
    (cond
      (not= :complete-execute-key (:status key))
      (if (= :not-applicable status) :not-applicable :unexpected-ledger-for-nonexecute)

      (not= :queried status) status
      (empty? rows) :not-visible
      (not= 1 (count rows)) :ambiguous-ledger-row
      (not (plain-map? (first rows))) :invalid-ledger-row
      :else
      (let [row (first rows)]
        (cond
          (not= #{:xt/id :committed :system-time} (set (keys row))) :invalid-ledger-row
          (not= (:tx-id key) (:xt/id row)) :ledger-tx-id-diverged
          (not= true (:committed row)) :ledger-not-committed
          (not= (:system-time key) (instant (:system-time row))) :ledger-system-time-diverged
          :else :reported-key-matches-committed-ledger-row)))))

(defn- phase-order [accounting-report ledger key]
  (let [source (:source accounting-report)
        post-token (get-in source [:post-phase :token])
        pre-token (get-in source [:pre-phase :token])
        pre (token-point pre-token)
        post (token-point post-token)
        later (when (= :queried (:status ledger))
                (token-point (:snapshot-token ledger)))
        tx-time (:system-time key)]
    (cond
      (not= :complete-execute-key (:status key)) :not-applicable
      (not (and post later)) :uncomparable-post-or-ledger-basis
      (not= (:database post) (:database later)) :different-post-and-ledger-databases
      (and pre (not= (:database pre) (:database post))) :different-pre-and-post-databases
      (and pre (pos? (compare (:system-time pre) (:system-time post))))
      :post-read-before-pre-read
      (pos? (compare (:system-time post) (:system-time later))) :ledger-read-before-post-read
      (pos? (compare tx-time (:system-time post))) :post-read-before-reported-tx
      (pos? (compare tx-time (:system-time later))) :ledger-read-before-reported-tx
      (nil? pre) :reported-post-and-ledger-cover-tx-pre-time-unavailable
      :else :reported-read-order-and-tx-time-compatible)))

(defn compare-committed-write-ledger!
  "Pure recomputation from *previously acquired* v1421 readbacks and ledger
   observation. This does not query XTDB and cannot attest the supplied node
   or transaction causality. Use acquire-committed-write-ledger! for controlled
   reads. Exact source {:accounting-report ... :ledger-observation ...}."
  [source]
  (seams!)
  (shape! source #{:accounting-report :ledger-observation} [:source])
  (let [{:keys [accounting-report ledger-observation]} source
        _ (when-not (plain-map? accounting-report)
            (reject! :invalid-accounting-report [:accounting-report]))
        _ (original-check (:source accounting-report) accounting-report)
        capture (get-in accounting-report [:source :capture-report])
        key (captured-key capture)
        ledger-status (ledger-match ledger-observation key)
        read-order (phase-order accounting-report ledger-observation key)
        accounting-agree? (= :bounded-queried-write-and-frame-agreement
                             (:status accounting-report))
        ledger-agree? (= :reported-key-matches-committed-ledger-row ledger-status)
        order-agree? (#{:reported-read-order-and-tx-time-compatible
                         :reported-post-and-ledger-cover-tx-pre-time-unavailable}
                       read-order)
        status (cond
                 (not accounting-agree?) :write-or-frame-accounting-incomplete
                 (not= :complete-execute-key (:status key)) :no-complete-execute-tx-key
                 (not ledger-agree?) :ledger-commit-corroboration-incomplete
                 (not order-agree?) :reported-snapshot-order-incompatible
                 :else :bounded-committed-write-ledger-coobservation)]
    {:type report-type
     :version report-version
     :source source
     :status status
     :accounting-status (:status accounting-report)
     :public-key-status (:status key)
     :ledger-status ledger-status
     :read-order-status read-order
     :pre-read-time-comparable? (= :reported-read-order-and-tx-time-compatible
                                   read-order)
     :bounded-commit-and-write-agreement?
     (= :bounded-committed-write-ledger-coobservation status)
     :observation-origin :controlled-xtdb-reads-on-caller-supplied-node
     :ledger-read-after-post-document-phase? true
     :ledger-basis-authenticated? false
     :node-identity-authenticated? false
     :transaction-submission-causality-authenticated? false
     :transaction-internal-prestate :not-established
     :all-physical-writer-paths-covered :not-established
     :global-physical-frame :not-established
     :physical-refinement-obligations open-obligations
     :physical-refinement-obligation-statuses :open
     :safe-to-retry? false
     :safe-to-settle? false
     :authority :none
     :transaction-authority :none
     :live-settlement-authority :none
     :certificate :not-issued
     :admission :not-admitted
     :proof-status :bounded-committed-readback-diagnostic-only}))

(defn acquire-committed-write-ledger!
  "Acquire one v1421 controlled write-accounting observation (one submit!
   attempt) followed by one pinned xt.txs read. Does not retry, submit twice,
   or claim an authenticated physical commit even if all diagnostics agree."
  [input]
  (shape! input #{:node :domain-source :submit!} [:input])
  (seams!)
  (let [{:keys [node domain-source submit!]} input
        accounting-report (original-acquire
                           {:node node :domain-source domain-source :submit! submit!})
        key (captured-key (get-in accounting-report [:source :capture-report]))
        ledger (read-ledger! node key)]
    (compare-committed-write-ledger!
     {:accounting-report accounting-report :ledger-observation ledger})))

(defn require-current-committed-write-ledger! [source report]
  "Recalculate diagnostic from supplied observations and reject report edits.
   This is not a second physical ledger or document read."
  (let [current (compare-committed-write-ledger! source)]
    (when-not (and (plain-map? report) (= report current))
      (reject! :stale-or-forged-committed-write-report [:report]))
    current))

(defn redacted-summary [report]
  "Only categorical statuses and booleans; omit private command payloads,
   transaction IDs, ledger rows and all snapshot-token strings."
  (dissoc (require-current-committed-write-ledger!
           (:source report) report) :source))
