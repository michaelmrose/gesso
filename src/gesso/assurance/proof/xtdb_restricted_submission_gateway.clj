(ns gesso.assurance.proof.xtdb-restricted-submission-gateway
  "v1529: Experimental, opt-in, ONE-SHOT physical XTDB submission boundary.

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

   v1527 adds a weak, process-local registry of freshly prepared gateway
   functions and the exact response object returned by their one physical
   submission call. require-local-return! accepts only that exact response
   together with the original two gateway functions, then reports its
   source-local binding and scoped observation. The registry is NOT an
   authenticated receipt, persisted audit trail, anti-tamper guarantee,
   XTDB commit witness or proof of physical causality.

   v1529 records a monotonic process-local attempt lifecycle in one atom,
   binding the prepared gateway to preflight reservation, entry into its
   execute-tx call-site, and return/exception observations. The added
   require-local-attempt! API checks the original local gateway functions
   before exposing a redacted attempt snapshot. The journal is in-memory,
   non-durable, and not a cryptographically authenticated execution record.
   A call-site entry proves neither XTDB receipt nor transaction commit.

   Do not pass gateway summaries as certificates. The returned TransactionKey
   is kept separate from redacted diagnostics and is public API evidence, not
   proof that the caller's abstract business transition was refined."
  (:require [gesso.assurance.proof.xtdb-write-footprint-accounting :as footprint]
            [gesso.model.tx :as tx]
            [gesso.model.tx-model :as tx-model]
            [xtdb.api :as xt])
  (:import [java.util WeakHashMap]
           [xtdb.api TransactionKey]))

(def gateway-type
  :gesso.assurance.proof.xtdb-restricted-submission-gateway/one-shot)
(def gateway-version 2)
(def local-return-type
  :gesso.assurance.proof.xtdb-restricted-submission-gateway/local-return)
(def local-return-version 1)
(def local-attempt-type
  :gesso.assurance.proof.xtdb-restricted-submission-gateway/local-attempt)
(def local-attempt-version 1)

;; The weak key is the unique :submit! function, not a source string or a
;; caller-provided transaction key. Values must NEVER hold the submit closure
;; (directly or indirectly), or the weak registry would retain it forever.
;; This only protects local bookkeeping from simple caller-made maps; arbitrary
;; code with JVM introspection/access to private Vars remains outside scope.
(def ^:private ^WeakHashMap local-gateways (WeakHashMap.))

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
   The exact returned map may be checked with require-local-return! while
   the original gateway remains reachable in the same process.
   Unexpected API results are reported UNKNOWN, never automatically called
   committed. Any original XTDB exception is propagated unchanged, with
   :summary! retaining only a safe status, never its message or payload.

   No caller may infer registered host authority, complete writer coverage,
   transaction causality, Live publication, or physical refinement."
  [node input]
  (when (nil? node)
    (reject! :missing-xtdb-node []))
  (let [{:keys [candidate footprint]} (derived-source! input)
        ;; One atomic, monotonic process-local record: callers cannot observe
        ;; a returned state with zero owned entries or a preflight rejection
        ;; with a purported physical invocation. No ledger fact is recorded.
        initial-attempt {:phase :prepared
                         :owned-call-entries 0
                         :observed-result-kind :none
                         :events [:prepared]}
        attempt (atom initial-attempt)
        returned (atom nil)
        attempt-id (str (java.util.UUID/randomUUID))
        source-binding (:process-local-source-binding footprint)
        _ (when-not (and (string? source-binding)
                         (not (empty? source-binding)))
            (reject! :missing-process-local-source-binding []))
        stats (select-keys footprint
                           [:candidate-operation-count
                            :prewrite-assertion-count
                            :whole-document-replacement-count
                            :declared-command-count
                            :declared-frame-only-target-count])
        summary! (fn []
                   (let [{:keys [phase owned-call-entries]} @attempt]
                     (merge {:type gateway-type
                           :version gateway-version
                           :status phase
                           :owned-execute-tx-call-entries owned-call-entries
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
                          stats)))
        submit! (fn []
                  (when-not (compare-and-set!
                             attempt initial-attempt
                             (-> initial-attempt
                                 (assoc :phase :checking)
                                 (update :events conj :checking)))
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
                      (swap! attempt (fn [a]
                                       (-> a
                                           (assoc :phase :preflight-rejected)
                                           (update :events conj :preflight-rejected))))
                      (throw failure)))
                  ;; The transition records entry into OUR call-site, not
                  ;; receipt by XTDB, a ledger record, or a durable commit.
                  (swap! attempt (fn [a]
                                   (-> a
                                       (assoc :phase :submitting
                                              :owned-call-entries 1)
                                       (update :events conj :submitting))))
                  ;; EXACTLY one direct call with the checked vector. Do not
                  ;; use an arbitrary caller thunk or tx/transact! (which
                  ;; recompiles the plan and adds Live side effects).
                  (try
                    (let [key (original-execute node
                                                (:candidate-tx-ops candidate))]
                      (swap! attempt
                             (fn [a]
                               (let [recognized?
                                     (instance? TransactionKey key)
                                     phase (if recognized?
                                             :returned-transaction-key
                                             :returned-unrecognized)]
                                 (-> a
                                     (assoc :phase phase
                                            :observed-result-kind
                                            (if recognized?
                                              :public-transaction-key
                                              :unrecognized-xtdb-api-return))
                                     (update :events conj phase)))))
                      (let [response {:tx-key key :diagnostic (summary!)}]
                        ;; Preserve object identity, not just structural
                        ;; equality: a copied/edited response is not the
                        ;; exact response emitted by this owned invocation.
                        (reset! returned response)
                        response))
                    (catch Throwable failure
                      (swap! attempt (fn [a]
                                       (-> a
                                           (assoc :phase :threw-outcome-unknown
                                                  :observed-result-kind :none)
                                           (update :events conj :threw-outcome-unknown))))
                      (throw failure))))]
    (locking local-gateways
      (.put local-gateways submit!
            {:summary! summary!
             :returned returned
             :attempt attempt
             :attempt-id attempt-id
             :source-binding source-binding
             :stats stats}))
    {:type gateway-type
     :version gateway-version
     :submit! submit!
     :summary! summary!}))

(defn require-local-return!
  "Check the EXACT in-process result map returned by THIS prepared gateway.

   A caller-supplied TransactionKey, a map copied from a genuine return, or
   functions from a different gateway cannot substitute for that result.
   The returned source binding came from the finite write-footprint checker
   at preparation and was rechecked before the sole execute-tx call. This
   reports a process-local linkage from admitted source to call-site result;
   it does not establish database commit, a ledger match, source authenticity,
   actual per-field causality, or the absence of writers outside this gateway.

   Only a normal API return can be checked here. On a thrown exception, the
   original :summary! remains :threw-outcome-unknown, and no key may be
   inferred. Neither reports nor accepted keys are certificates."
  [gateway result]
  (when-not (and (plain-map? gateway)
                 (= #{:type :version :submit! :summary!}
                    (set (keys gateway)))
                 (= gateway-type (:type gateway))
                 (= gateway-version (:version gateway)))
    (reject! :invalid-local-gateway []))
  (let [registration (locking local-gateways
                       (.get local-gateways (:submit! gateway)))
        {:keys [summary! returned attempt attempt-id source-binding stats]}
        registration
        observed (when returned @returned)
        snapshot (when attempt @attempt)
        phase (:phase snapshot)]
    (when-not (and registration
                   (identical? summary! (:summary! gateway))
                   (map? result)
                   (identical? observed result)
                   (= 1 (:owned-call-entries snapshot))
                   (#{:returned-transaction-key :returned-unrecognized} phase)
                   (= phase (get-in observed [:diagnostic :status]))
                   (= (:diagnostic observed) (summary!)))
      (reject! :not-this-gateway-local-return []))
    {:type local-return-type
     :version local-return-version
     :source-binding-scope :process-local-nonauthenticating
     :process-local-source-binding source-binding
     :gateway-version gateway-version
     :process-local-attempt-id attempt-id
     :owned-execute-tx-call-entries (:owned-call-entries snapshot)
     :observed-result-kind (if (instance? TransactionKey (:tx-key observed))
                             :public-transaction-key
                             :unrecognized-xtdb-api-return)
     :local-return-linkage :exact-object-from-owned-call-site
     :operation-origin :readmitted-source-derived-finite-vector
     :candidate-operation-count (:candidate-operation-count stats)
     :prewrite-assertion-count (:prewrite-assertion-count stats)
     :whole-document-replacement-count (:whole-document-replacement-count stats)
     :committed-transaction :not-established
     :public-key-to-ledger-join :not-checked
     :physical-poststate-causality :not-established
     :transaction-internal-prestate :not-authenticated
     :complete-deployment-writer-coverage :not-established
     :principal-authorization :not-established
     :physical-refinement :not-established
     :proof-status :process-local-result-correspondence-only
     :admission :not-admitted
     :certificate :not-issued
     :authority :none}))


(defn require-local-attempt!
  "Recheck a genuine gateway's local, redacted submission-attempt journal.

   The original :submit!/:summary! functions must be registered together.
   The bounded event sequence tracks this process' control flow: :prepared,
   :checking, then either :preflight-rejected or :submitting followed by a
   return/exception observation. An entered call-site is NOT proof XTDB
   received or committed the transaction. Exceptions leave the physical
   outcome unknown and never produce an invented TransactionKey.

   This is a transient observation, not a transferable certificate, durable
   audit log, authenticated source identity, causal evidence, or deployment
   writer-closure claim. It contains neither transaction operations nor
   physical identifiers, and accepts no caller-authored event log."
  [gateway]
  (when-not (and (plain-map? gateway)
                 (= #{:type :version :submit! :summary!}
                    (set (keys gateway)))
                 (= gateway-type (:type gateway))
                 (= gateway-version (:version gateway)))
    (reject! :invalid-local-gateway []))
  (let [registration (locking local-gateways
                       (.get local-gateways (:submit! gateway)))]
    (when-not (and registration
                   (identical? (:summary! registration)
                               (:summary! gateway)))
      (reject! :not-this-gateway-local-attempt []))
    (let [{:keys [attempt attempt-id source-binding stats]} registration
          {:keys [phase owned-call-entries observed-result-kind events]}
          @attempt]
      {:type local-attempt-type
       :version local-attempt-version
       :gateway-version gateway-version
       :process-local-attempt-id attempt-id
       :source-binding-scope :process-local-nonauthenticating
       :process-local-source-binding source-binding
       :status phase
       :observed-local-events events
       :owned-execute-tx-call-entries owned-call-entries
       :call-site-entry (if (pos? owned-call-entries)
                          :entered-locally
                          :not-entered)
       :observed-result-kind observed-result-kind
       :physical-outcome (if (= phase :preflight-rejected)
                           :no-gateway-call
                           :not-established)
       :candidate-operation-count (:candidate-operation-count stats)
       :prewrite-assertion-count (:prewrite-assertion-count stats)
       :whole-document-replacement-count
       (:whole-document-replacement-count stats)
       :committed-transaction :not-established
       :xtdb-receipt :not-established
       :public-key-to-ledger-join :not-checked
       :physical-poststate-causality :not-established
       :transaction-internal-prestate :not-authenticated
       :principal-authorization :not-established
       :complete-deployment-writer-coverage :not-established
       :physical-refinement :not-established
       :proof-status :process-local-attempt-observation-only
       :admission :not-admitted
       :certificate :not-issued
       :authority :none})))
