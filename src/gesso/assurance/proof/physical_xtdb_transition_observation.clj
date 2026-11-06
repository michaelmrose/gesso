(ns gesso.assurance.proof.physical-xtdb-transition-observation
  "v1465: Redacted phase contract for ONE controlled XTDB transition.

   Reuse the existing model/tx-refinement-xtdb collector. Its source-derived
   conditional step, once-only execute-tx, two separately pinned observations,
   transaction-ledger window and basis replay are not recreated here. This
   layer classifies their DIFFERENT evidentiary meanings for a later physical
   refinement checker. It neither captures a deployed handler nor proves that
   any physical writer or node belongs to an application's production path.

   observe-controlled-transition! actively submits a transaction: use only on
   an exclusively controlled test node. A post-submit diagnostic exception is
   NOT evidence of rejection and must never cause a retry. The pure classifier
   accepts COPYABLE collector reports; its checks do not authenticate their
   provenance. Returned reports exclude targets, documents, plan contents,
   snapshot tokens, public transaction IDs and raw throwable data."
  (:require [clojure.set :as set]
            [gesso.model.tx-refinement :as refinement]
            [gesso.model.tx-refinement-xtdb :as collector]))

(def observation-version 1)
(def observation-type
  :gesso.assurance.proof.physical-xtdb-transition-observation/phase-contract)

(def ^:private original-collector collector/observe-and-derive-conditional!)
(def ^:private original-scope collector/composed-step-scope)
(def ^:private original-collection-scope
  collector/repeatable-ledger-window-collection-scope)
(def ^:private original-comparison-scope refinement/submission-comparison-scope)

(def ^:private collector-keys
  #{:comparison :execution :collected-target-count
    :submitted-operation-count :collection-scope :evidence-kind
    :observation-basis :certifies-physical-execution? :physical-tx-id
    :transaction-ledger-matches? :conditional-step :composed-assurance})
(def ^:private mandatory-comparison-checks
  #{:prestate :poststate :outcome-status :recognized-conflict
    :recorded-submission :tx-ledger-record-present :tx-ledger-id
    :tx-ledger-commit-status :tx-ledger-system-time
    :executed-tx-after-before-watermark
    :executed-tx-immediate-successor
    :executed-tx-equals-after-watermark
    :before-documents-repeat-at-pinned-basis
    :after-documents-repeat-at-pinned-basis
    :before-ledger-watermark-repeats-at-pinned-basis
    :after-ledger-watermark-repeats-at-pinned-basis})
(def ^:private additional-open-obligations
  #{:production-node-and-handler-provenance
    :physical-submission-payload-authenticity
    :transaction-internal-read-snapshot-equivalence
    :all-physical-writer-paths-accounted-for
    :independent-backend-semantics-refinement
    :deployed-authorization-and-policy-correspondence
    :unknown-outcome-reconciliation
    :universal-execution-refinement})

(defn- reject! [reason]
  ;; Never put source values or private documents in exception data.
  (throw (ex-info "Invalid controlled physical transition observation."
                  {:physical-xtdb-transition-observation/rejection true
                   :reason reason})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- count? [n]
  (and (integer? n) (<= 1 n 256)))

(defn- original-checkers! []
  (when-not (and (identical? original-collector
                             (var-get #'collector/observe-and-derive-conditional!))
                 (= original-scope collector/composed-step-scope)
                 (= original-collection-scope
                    collector/repeatable-ledger-window-collection-scope)
                 (= original-comparison-scope
                    refinement/submission-comparison-scope))
    (reject! :substituted-upstream-collector-or-scope)))

(defn- valid-execution? [execution]
  (and (plain-map? execution)
       (case (:status execution)
         :committed (= #{:status} (set (keys execution)))
         :rejected (and (= #{:status :error/code} (set (keys execution)))
                        (= :xtdb/assert-failed (:error/code execution)))
         false)))

(defn- valid-comparison? [comparison execution]
  (let [checks (:checks comparison)]
    (and (plain-map? comparison)
         (plain-map? checks)
         (set/subset? mandatory-comparison-checks (set (keys checks)))
         (every? boolean? (vals checks))
         (boolean? (:corresponds? comparison))
         (= (:corresponds? comparison) (every? true? (vals checks)))
         (= (:observed-status comparison) (:status execution))
         (#{:committed :rejected} (:expected-status comparison))
         (= false (:certifies-physical-execution? comparison))
         (= :caller-supplied-recorded-step-comparison
            (:evidence-kind comparison))
         (= original-comparison-scope (:scope comparison))
         (count? (:recorded-op-count comparison))
         (count? (:canonical-op-count comparison))
         (vector? (:mismatched-op-indices comparison))
         (if (:recorded-submission checks)
           (and (= (:recorded-op-count comparison)
                   (:canonical-op-count comparison))
                (empty? (:mismatched-op-indices comparison)))
           true))))

(defn- valid-conditional? [conditional comparison]
  (and (plain-map? conditional)
       (plain-map? (:composed-assurance conditional))
       ;; This validator is called with the whole raw collector report. A
       ;; locally invalid argument is refused BEFORE XTDB submission.
       (true? (get-in conditional [:conditional-step
                                   :conditional-argument-admissible?]))
       (let [assurance (:composed-assurance conditional)
             corresponds? (true? (:corresponds? comparison))]
         (and (= original-scope (:scope assurance))
              (= (:undischarged-premises original-scope)
                 (:undischarged-premises assurance))
              (= (:not-established original-scope)
                 (:not-established assurance))
              (true? (:local-argument-valid? assurance))
              (= corresponds? (:physical-observations-correspond? assurance))
              (= corresponds? (:joint-evidence-consistent? assurance))
              (= (if corresponds?
                   :locally-valid-and-observationally-consistent
                   :observation-mismatch)
                 (:status assurance))
              (false? (:certifies-physical-execution? assurance))
              (false? (:proves-internal-prestate? assurance))
              (false? (:proves-general-refinement? assurance))))))

(defn classify-controlled-report!
  "Classify a complete, copyable v1027 controlled-collector report.

   This is a STRICT SHAPE/CROSS-FIELD check, not provenance authentication.
   A caller can fabricate an internally consistent collector report. Use
   observe-controlled-transition! to run the existing collector once on a
   controlled node. This pure classifier NEVER queries or writes XTDB."
  [raw]
  (original-checkers!)
  (when-not (and (plain-map? raw)
                 (= collector-keys (set (keys raw)))
                 (= original-collection-scope (:collection-scope raw))
                 (= :direct-controlled-xtdb-repeatable-ledger-window-observation
                    (:evidence-kind raw))
                 (= :separate-pinned-before-and-after-phases
                    (:observation-basis raw))
                 (false? (:certifies-physical-execution? raw))
                 (count? (:collected-target-count raw))
                 (count? (:submitted-operation-count raw))
                 (integer? (:physical-tx-id raw))
                 (<= 0 (:physical-tx-id raw) Long/MAX_VALUE)
                 (boolean? (:transaction-ledger-matches? raw))
                 (valid-execution? (:execution raw))
                 (valid-comparison? (:comparison raw) (:execution raw))
                 (valid-conditional? raw (:comparison raw)))
    (reject! :malformed-or-inconsistent-controlled-report))
  (let [checks (get-in raw [:comparison :checks])
        ledger-checks (select-keys checks
                                   [:tx-ledger-record-present :tx-ledger-id
                                    :tx-ledger-commit-status
                                    :tx-ledger-system-time])
        ledger? (every? true? (vals ledger-checks))
        _ (when-not (= ledger? (:transaction-ledger-matches? raw))
            (reject! :ledger-status-disagrees-with-checks))
        local? (get-in raw [:composed-assurance :local-argument-valid?])
        corresponds? (get-in raw [:comparison :corresponds?])
        status (if corresponds?
                 (case (get-in raw [:execution :status])
                   :committed :bounded-committed-step-agreement
                   :rejected :bounded-rejected-step-agreement)
                 :bounded-observations-diverge)]
    {:type observation-type
     :version observation-version
     :status status
     :submission {:phase :collector-reported-one-execute-tx-attempt
                  :operation-count (:submitted-operation-count raw)
                  :recorded-operations-agree? (:recorded-submission checks)
                  :physical-payload-provenance :not-authenticated}
     :outcome {:phase :definitive-collector-outcome
               :reported-status (get-in raw [:execution :status])
               :recognized-assertion-rejection?
               (= :rejected (get-in raw [:execution :status]))
               :ledger-row-agrees? ledger?
               :reported-tx-id-exists? true
               :physical-receipt-authenticated? false}
     :readback {:phase :separate-pinned-before-and-after-snapshots
                :target-count (:collected-target-count raw)
                :prestate-agrees? (:prestate checks)
                :poststate-agrees? (:poststate checks)
                :pinned-phase-replay-agrees?
                (every? true? (map checks
                     [:before-documents-repeat-at-pinned-basis
                      :after-documents-repeat-at-pinned-basis
                      :before-ledger-watermark-repeats-at-pinned-basis
                      :after-ledger-watermark-repeats-at-pinned-basis]))
                :transaction-internal-prestate :not-established}
     :abstract-correspondence
     {:phase :finite-serial-model-comparison
      :conditional-local-argument? local?
      :observed-step-agreement? corresponds?
      :physical-refinement :not-established}
     :open-obligations
     (set/union additional-open-obligations
                (:undischarged-premises original-scope)
                (:not-established original-scope)
                (:assumed original-collection-scope)
                (:not-established original-collection-scope))
     :observation-origin :controlled-collector-report-unattested
     :scope :one-controlled-single-node-finite-transaction
     :proof-status :bounded-diagnostic-only
     :certificate :not-issued
     :admission :not-admitted
     :transaction-authority :none
     :authority :none}))

(defn observe-controlled-transition!
  "Actively run ONE admitted finite transaction against the supplied CONTROLLED
   XTDB node, through Gesso's existing v1027 collector, then classify it.

   Input has exactly {:node <controlled xtdb node> :plan <admitted finite plan>
                      :targets <complete finite target vector>}.
   WARNING: this function WRITES to XTDB. It is not a production executor.
   Unknown outcomes or post-submission acquisition errors propagate unchanged;
   this wrapper never retries or re-executes. Do NOT call it to refresh the
   report. No production-node, transaction-internal basis or writer identity
   is authenticated by this contract."
  [input]
  (original-checkers!)
  (when-not (and (plain-map? input)
                 (= #{:node :plan :targets} (set (keys input))))
    (reject! :invalid-observation-input))
  (classify-controlled-report! (original-collector input)))
