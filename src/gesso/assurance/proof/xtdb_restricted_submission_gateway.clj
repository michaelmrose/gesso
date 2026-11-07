(ns gesso.assurance.proof.xtdb-restricted-submission-gateway
  "v1525: Experimental, opt-in, ONE-SHOT physical XTDB submission boundary.

   This gateway owns a real xtdb.api/execute-tx call. A caller supplies an
   ordinary Gesso model plan and a finite protected-document declaration,
   NEVER a transaction vector or a submission callback. The gateway derives
   the exact lowered operations, re-admits their complete finite replacement
   footprint, and submits that SAME vector once. No additional mutation forms
   can be inserted through this gateway without failing the admission check.

   This is NOT an installed Biff/Gesso Live writer, route, or certified
   executor. It intentionally does not publish Live changes. It cannot stop
   code elsewhere from calling XTDB directly, authenticate the supplied node
   or principal, establish transaction-internal prestate/frame preservation,
   or prove complete deployment-wide writer closure. Its receipt only
   describes one locally owned call-site and the public XTDB API result.
   Exception outcomes are indeterminate and are rethrown unchanged.

   Do not pass gateway summaries as certificates. The returned TransactionKey
   is kept separate from redacted diagnostics and is public API evidence, not
   proof that the caller's abstract business transition was refined."
  (:require [gesso.assurance.proof.xtdb-write-footprint-accounting :as footprint]
            [gesso.model.tx :as tx]
            [gesso.model.tx-model :as tx-model]
            [xtdb.api :as xt])
  (:import [xtdb.api TransactionKey]))

(def gateway-type
  :gesso.assurance.proof.xtdb-restricted-submission-gateway/one-shot)
(def gateway-version 1)

(def ^:private original-prepare tx/prepare)
(def ^:private original-admit tx-model/admitted-plan)
(def ^:private original-inspect footprint/inspect-footprint!)
(def ^:private original-recheck footprint/require-current-footprint!)
(def ^:private original-execute xt/execute-tx)
(def ^:private source-keys #{:plan :protected-documents})

(defn- plain-map? [value]
  (and (or (instance? clojure.lang.PersistentArrayMap value)
           (instance? clojure.lang.PersistentHashMap value))
       (nil? (meta value))))

(defn- reject! [reason at]
  (throw (ex-info "Unsupported restricted XTDB submission gateway request."
                  {:xtdb-restricted-submission-gateway/rejection true
                   :reason reason :at at})))

(defn- seams! []
  ;; Detect ordinary in-process Var substitutions; this is not tamper-proof
  ;; deployment provenance, code signing, or a JVM sandbox.
  (when-not (and (identical? original-prepare (var-get #'tx/prepare))
                 (identical? original-admit (var-get #'tx-model/admitted-plan))
                 (identical? original-inspect
                             (var-get #'footprint/inspect-footprint!))
                 (identical? original-recheck
                             (var-get #'footprint/require-current-footprint!))
                 (identical? original-execute (var-get #'xt/execute-tx)))
    (reject! :substituted-gateway-or-upstream-seam [])))

(defn- immutable-wire-value?
  "A narrow, bounded set of immutable Clojure/Java XTDB input values.
   In particular reject byte arrays, Dates, Java mutable Maps/Lists, lazy
   sequences, arbitrary records and user-defined objects. Equality of two
   vectors that share a mutable object is NOT evidence of unchanged bytes."
  [value depth]
  (and (<= depth 24)
       (cond
         (or (nil? value) (string? value) (keyword? value)
             (symbol? value) (boolean? value)
             (instance? java.util.UUID value)
             (instance? java.time.Instant value)
             (instance? java.time.LocalDate value)
             (instance? java.time.LocalTime value)
             (instance? java.time.LocalDateTime value)
             (instance? java.time.OffsetDateTime value)
             (instance? java.time.ZonedDateTime value)
             (instance? java.time.Duration value)
             (instance? java.time.Period value)
             (instance? java.math.BigDecimal value)
             (instance? java.math.BigInteger value)
             (instance? clojure.lang.BigInt value)
             (instance? java.lang.Byte value)
             (instance? java.lang.Short value)
             (instance? java.lang.Integer value)
             (instance? java.lang.Long value)
             (instance? java.lang.Float value)
             (instance? java.lang.Double value)) true

         (and (instance? clojure.lang.APersistentMap value)
              (nil? (meta value)) (<= (count value) 4096))
         (every? (fn [[k v]]
                   (and (immutable-wire-value? k (inc depth))
                        (immutable-wire-value? v (inc depth))))
                 value)

         (and (instance? clojure.lang.APersistentVector value)
              (nil? (meta value)) (<= (count value) 4096))
         (every? #(immutable-wire-value? % (inc depth)) value)

         (and (instance? clojure.lang.APersistentSet value)
              (nil? (meta value)) (<= (count value) 4096))
         (every? #(immutable-wire-value? % (inc depth)) value)

         :else false)))

(defn- derived-source! [input]
  (when-not (and (plain-map? input)
                 (= source-keys (set (keys input))))
    (reject! :invalid-gateway-source []))
  (seams!)
  (when-not (= false (get-in input [:plan :emit]))
    (reject! :live-effects-not-supported [:plan]))
  (let [admitted (try
                   (original-admit (:plan input))
                   (catch Exception _
                     (reject! :unsupported-plan [:plan])))
        ;; No caller-controlled Live effects, entry functions or tx options
        ;; may be silently ignored by this direct XTDB path.
        _ (when-not (and (= false (:emit admitted))
                         (empty? (:changes admitted)))
            (reject! :live-effects-not-supported [:plan]))
        lowered (try
                  (original-prepare nil (:plan input))
                  (catch Exception _
                    (reject! :transaction-preparation-failed [:plan])))
        _ (when-not (immutable-wire-value? (:tx-ops lowered) 0)
            (reject! :mutable-or-unsupported-transaction-value [:plan]))
        candidate {:plan (:plan input)
                   :protected-documents (:protected-documents input)
                   :candidate-tx-ops (:tx-ops lowered)}
        inspected (try
                    (original-inspect candidate)
                    (catch Exception _
                      (reject! :inadmissible-write-footprint
                               [:protected-documents])))]
    (when-not (and (true? (:all-candidate-operations-accounted-for? inspected))
                   (true? (:all-declared-write-targets-covered-in-finite-domain?
                           inspected))
                   (true? (:all-declared-writes-whole-document-replacements?
                           inspected))
                   (= :bounded-declared-transaction-vector-only
                      (:proof-status inspected))
                   (= :not-issued (:certificate inspected))
                   (= :none (:authority inspected)))
      (reject! :unexpected-footprint-contract []))
    {:candidate candidate :footprint inspected}))

(defn prepare-one-shot!
  "Validate a complete finite, supported write vector WITHOUT executing it.

   Input: node (a caller-supplied XTDB node) and exactly
     {:plan <ordinary Gesso model transaction plan, :emit false>
      :protected-documents <finite v1412 protection entries>}.

   Returns ONLY {:type, :version, :submit!, :summary!}. The plan, prepared
   operations, node, and physical identifiers remain closed over; clients
   cannot supply replacement transaction operations to :submit!. The closure
   rejects every second attempt, including retries after an exception or an
   uncertain result. Preparation issues no XTDB query or transaction.

   :submit! returns {:tx-key <original public XTDB result>
                     :diagnostic <redacted summary>} on normal return.
   Unexpected API results are reported UNKNOWN, never automatically called
   committed. Any original XTDB exception is propagated unchanged, with
   :summary! retaining only a safe status, never its message or payload.

   No caller may infer registered host authority, complete writer coverage,
   transaction causality, Live publication, or physical refinement."
  [node input]
  (when (nil? node)
    (reject! :missing-xtdb-node []))
  (let [{:keys [candidate footprint]} (derived-source! input)
        ;; :prepared -> :checking is an atomic reservation. Each exported
        ;; closure may submit at most once, even when invoked concurrently.
        status (atom :prepared)
        stats (select-keys footprint
                           [:candidate-operation-count
                            :prewrite-assertion-count
                            :whole-document-replacement-count
                            :declared-command-count
                            :declared-frame-only-target-count])
        summary! (fn []
                   (merge {:type gateway-type
                           :version gateway-version
                           :status @status
                           :gateway-owned-submission-call-site? true
                           :submitted-operation-origin
                           :source-derived-finite-admitted-vector
                           :public-key-to-ledger-join :not-checked
                           :transaction-internal-prestate :not-authenticated
                           :physical-frame-preservation :not-established
                           :installed-host-adoption :not-established
                           :caller-node-authenticity :not-established
                           :principal-authorization :not-established
                           :outside-gateway-writer-closure :not-established
                           :physical-refinement :not-established
                           :proof-status :local-boundary-only
                           :admission :not-admitted
                           :certificate :not-issued
                           :authority :none}
                          stats))
        submit! (fn []
                  (when-not (compare-and-set! status :prepared :checking)
                    (reject! :one-shot-already-attempted [:submit!]))
                  ;; Recheck while still pre-submission. If this fails, NO
                  ;; physical API invocation occurs, and the attempt is spent.
                  (try
                    (seams!)
                    (original-recheck candidate footprint)
                    (let [again (try
                                  (original-prepare nil (:plan input))
                                  (catch Exception _
                                    (reject! :transaction-repreparation-failed
                                             [:plan])))]
                      (when-not (and (immutable-wire-value? (:tx-ops again) 0)
                                     (= (:candidate-tx-ops candidate)
                                        (:tx-ops again)))
                        (reject! :changed-prepared-transaction [:plan])))
                    (catch Throwable failure
                      (reset! status :preflight-rejected)
                      (throw failure)))
                  (reset! status :submitting)
                  ;; EXACTLY one direct call with the checked vector. Do not
                  ;; use an arbitrary caller thunk or tx/transact! (which
                  ;; recompiles the plan and adds Live side effects).
                  (try
                    (let [key (original-execute node
                                                (:candidate-tx-ops candidate))]
                      (reset! status
                              (if (instance? TransactionKey key)
                                :returned-transaction-key
                                :returned-unrecognized))
                      {:tx-key key :diagnostic (summary!)})
                    (catch Throwable failure
                      (reset! status :threw-outcome-unknown)
                      (throw failure))))]
    {:type gateway-type
     :version gateway-version
     :submit! submit!
     :summary! summary!}))
