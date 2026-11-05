(ns gesso.assurance.proof.transaction-outcome-progression-correspondence
  "v1396: Classify ONE controlled submission's physical outcome, durable-command
   resolution, and reported authoritative progression without conflating them.

   :incorporated is a RESOLUTION status, not a proof of a new physical write;
   :replay can result from prior incorporation or post-error reconciliation;
   :unresolved after a thrown/indeterminate attempt is NOT business rejection.
   A reported XTDB basis is a separate opaque, authority-dependent value.

   This classification is intentionally a bounded PROCESS-LOCAL DIAGNOSTIC.
   Even a matching adapter/ledger observation and a matching basis cannot
   authenticate the submitted payload, its prestate, causal attribution,
   application authorization, abstract transition, or deployed writer set.
   All nine physical-refinement obligations stay open. No certificate, Live
   settlement authorization, or permission to retry is issued.

   :execution preserves the original nested result or exception, including
   private application data. Never send or log that value as an HTTP response."
  (:require [gesso.assurance.proof.classified-physical-acquisition :as acquisition]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.progression :as progression]))

(def report-type
  :gesso.assurance.proof.transaction-outcome-progression-correspondence/report)
(def report-version 1)

(def ^:private original-acquire acquisition/run-classified-acquisition!)
(def ^:private physical-open
  #{:authenticated-submitted-payload
    :authenticated-transaction-internal-prestate
    :actual-inputs-and-authorization-to-issued-executor-binding
    :complete-executed-physical-write-footprint
    :transaction-commit-and-conflict-outcome-authenticity
    :deployed-handler-and-route-adoption
    :all-physical-writer-paths-covered
    :independent-abstract-to-physical-semantics-equivalence
    :universal-physical-execution-refinement})

(defn- plain-map? [value]
  (and (or (instance? clojure.lang.PersistentHashMap value)
           (instance? clojure.lang.PersistentArrayMap value))
       (nil? (meta value))))

(defn- reject! [reason]
  ;; Do not include the potentially sensitive source, execution or receipt.
  (throw (ex-info "Invalid transaction outcome/progression diagnostic."
                  {:transaction-outcome-progression/rejection true
                   :reason reason})))

(defn- acquisition-diagnostic-valid? [execution report]
  (and (plain-map? execution)
       (#{:returned :threw} (:outcome execution))
       (if (= :returned (:outcome execution))
         (contains? execution :result)
         (instance? Throwable (:error execution)))
       (plain-map? report)
       (= acquisition/report-type (:type report))
       (= acquisition/report-version (:version report))
       (= (:outcome execution) (:execution-outcome report))
       (= :classified-coacquisition-diagnostic-only (:proof-status report))
       (= :not-issued (:certificate report))
       (= :not-admitted (:admission report))
       (= :none (:authority report))
       (= :none (:transaction-authority report))
       (= :not-established (:actual-payload-accepted-by-xtdb? report))
       (= :not-established (:transaction-internal-prestate report))
       (= :not-established (:causal-attribution-to-executor-attempt report))
       (= :not-established (:authenticated-request-or-authorization report))
       (= :not-established (:other-physical-writers-excluded? report))
       (= :not-established (:deployed-handler-or-route-identity report))
       (= physical-open (:all-physical-refinement-obligations report))
       (= :open (:all-physical-refinement-obligation-statuses report))
       (instance? Boolean
                  (:bounded-executor-adapter-ledger-agreement? report))
       (plain-map? (:executor-observation report))
       (#{:committed :rejected :indeterminate :unobserved}
        (get-in report [:executor-observation :physical-outcome]))))

(defn- resolution-result [execution]
  (when (= :returned (:outcome execution))
    (get-in execution [:result :controlled-result :resolution])))

(defn- progression-diagnostic [resolution]
  ;; Inspect only public metadata in the nested transaction. The source of
  ;; these values is still the reported result, never authenticated by XTDB.
  (let [transaction (:transaction resolution)
        present? (and (map? transaction) (contains? transaction :progression))
        requirement (:progression transaction)]
    (if-not (= :incorporated (:status resolution))
      :not-applicable
      (cond
        (not present?) :not-reported
        (not (progression/requirement? requirement))
        :malformed-reported-progression
        :else
        (try
          (let [bases (progression/required-bases requirement)
                consistency (xtdb/tx-result-consistency (:tx-result transaction))
                basis (when (= 1 (count bases)) (first bases))]
            (cond
              (not (and basis (xtdb/xtdb-basis? basis)))
              :unsupported-or-nonsingleton-basis
              (not (and (some? (:tx-id consistency))
                        (some? (:system-time consistency))))
              :incomplete-public-transaction-key
              (and (= (xtdb/basis-tx-id basis) (:tx-id consistency))
                   (= (xtdb/basis-system-time basis)
                      (:system-time consistency)))
              :reported-basis-matches-public-transaction-key
              :else :reported-basis-diverges-from-public-transaction-key))
          (catch Throwable _ :uncheckable-reported-progression))))))

(defn- classify-status [execution-outcome resolution-status physical-status
                        bounded-agreement? committed-ack? progression-status]
  (cond
    (= :threw execution-outcome) :execution-threw-outcome-unresolved
    (= :replay resolution-status) :durable-replay-no-new-commit-attribution
    (#{:incompatible :unavailable} resolution-status)
    :binding-or-disclosure-blocked
    (= :unresolved resolution-status)
    (case physical-status
      :rejected :physical-assertion-rejected-command-unresolved
      :committed :physical-commit-observed-command-unresolved
      :indeterminate :commit-unknown-reconciliation-required
      :unobserved :commit-unknown-no-definitive-observation
      :commit-unknown-reconciliation-required)
    (= :incorporated resolution-status)
    (cond
      (not committed-ack?) :inconsistent-reported-incorporation
      (not= :committed physical-status) :inconsistent-reported-incorporation
      (not (#{:not-reported
              :reported-basis-matches-public-transaction-key}
            progression-status)) :reported-progression-diverged
      (not bounded-agreement?) :incorporation-without-coobserved-ledger-agreement
      :else :bounded-incorporation-and-ledger-coobservation)
    :else :unclassified-or-missing-resolution))

(defn classify-observation
  "Classify an ALREADY-ACQUIRED v1394 execution plus diagnostic. This pure
   function also accepts detached/copyable maps; it performs structural
   checks, NOT provenance authentication. Positive labels remain diagnostic.

   Never use this API to settle an optimistic command or grant retry authority.
   For a real controlled invocation use run-outcome-progression! instead."
  [observation]
  (when-not (and (plain-map? observation)
                 (= #{:execution :diagnostic} (set (keys observation))))
    (reject! :invalid-acquisition-shape))
  (let [{:keys [execution diagnostic]} observation]
    (when-not (acquisition-diagnostic-valid? execution diagnostic)
      (reject! :invalid-or-unchecked-acquisition-diagnostic))
    (let [resolution (resolution-result execution)
          resolution-status (if (map? resolution)
                              (:status resolution) :not-observed)
          physical-status (get-in diagnostic
                                  [:executor-observation :physical-outcome])
          tx (:transaction resolution)
          committed-ack? (and (= :incorporated resolution-status)
                              (map? tx)
                              (= :committed (:commit/status tx))
                              (contains? tx :tx-result))
          basis-status (progression-diagnostic resolution)
          ;; An isolated copied boolean is never sufficient even for a
          ;; diagnostic agreement label. Require the bounded v1394 fields to
          ;; remain mutually consistent; all can still be spoofed in-process.
          bounded? (and (true? (:bounded-executor-adapter-ledger-agreement?
                               diagnostic))
                        (= :bounded-coobserved-metadata-agreement
                           (:status diagnostic))
                        (= :process-local-executor-observations-agree
                           (get-in diagnostic [:executor-observation :status]))
                        (= :committed physical-status)
                        (= :reported-execute-ledger-agrees
                           (:adapter-api-status diagnostic))
                        (= :reported-key-matches-committed-ledger-row
                           (:ledger-observation-status diagnostic)))
          status (classify-status (:outcome execution) resolution-status
                                  physical-status bounded? committed-ack?
                                  basis-status)]
      {:type report-type
       :version report-version
       :status status
       :execution-outcome (:outcome execution)
       :durable-command-resolution-status resolution-status
       :observed-physical-attempt-status physical-status
       :adapter-ledger-status (:adapter-api-status diagnostic)
       :bounded-adapter-ledger-agreement? bounded?
       :reported-authoritative-progression-status basis-status
       :claimed-abstract-effect
       (case resolution-status
         :incorporated :one-durable-command-incorporation-reported
         :replay :previous-incorporation-reconciled-no-new-attempt-attribution
         :unresolved :no-resolved-command-incorporation
         :incompatible :incompatible-command-binding
         :unavailable :command-disclosure-unavailable
         :not-established)
       :abstract-transition-state-pair-observed? false
       :reported-basis-authenticated? false
       :physical-outcome-authenticated? false
       :commit-unknown-safe-to-retry? false
       :business-rejection-established? false
       :physical-refinement-obligations physical-open
       :physical-refinement-obligation-statuses :open
       :proof-status :conditional-execution-progression-diagnostic-only
       :certificate :not-issued
       :admission :not-admitted
       :live-settlement-authority :none
       :transaction-authority :none
       :authority :none})))

(defn run-outcome-progression!
  "Run the v1394 controlled acquisition exactly once, then classify its
   observed result without submitting another transaction or rereading XTDB.
   The original :execution is preserved byte-for-byte as a Clojure value;
   exception objects remain identical. The diagnostic never contains command
   bindings, documents, operation payloads, or a complete physical proof."
  [source ctx]
  (when-not (identical? original-acquire
                        (var-get #'acquisition/run-classified-acquisition!))
    (reject! :substituted-acquisition-entry-point))
  (let [observation (original-acquire source ctx)]
    {:execution (:execution observation)
     :diagnostic (classify-observation observation)}))
