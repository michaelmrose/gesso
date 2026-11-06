(ns gesso.assurance.example.bounded-physical-history-replay
  "v1504: Replay v1503's ORIGINAL, source-derived abstract history prefixes
   against fresh v1494 embedded-XTDB applications. Each prefix receives its
   own resource-managed node. Each step uses the v1499 one-attempt physical
   outcome observer and is checked against the generated before/after states.

   This is a bounded, readback-based DIAGNOSTIC, not a physical refinement
   certificate. It does not authenticate a transaction's cause, the complete
   writer set, the internal XTDB snapshot, the execution principal, or the
   physical-to-abstract mapping. In particular it does not issue an executor,
   replay an uncertain operation, or make observed data authoritative. Only
   redacted first-mismatch coordinates are returned."
  (:require [gesso.assurance.example.bounded-abstract-histories :as histories]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.example.physical-single-node-outcome-correspondence :as outcome]))

(def report-type
  :gesso.assurance.example.bounded-physical-history-replay/report)
(def report-version 1)
(def max-replay-histories 16)
(def max-replay-steps 64)

(def ^:private obligations
  #{:physical-initial-state-origin-not-authenticated
    :readback-is-not-an-authenticated-transaction-causality-proof
    :transaction-internal-prestate-not-authenticated
    :physical-to-abstract-projection-not-independently-certified
    :complete-writer-coverage-not-established
    :issued-executor-and-installed-route-not-bound
    :authorization-and-principal-provenance-not-established
    :only-enumerated-finite-histories-replayed
    :universal-physical-refinement-not-established})

;; Capture the expected source functions. JVM identity checking is a limited
;; defense against accidental substitution, not an integrity/deployment proof.
(def ^:private original-require-current histories/require-current-histories!)
(def ^:private original-with-app physical/with-proving-application!)
(def ^:private original-initial-document physical/initial-document)
(def ^:private original-project physical/physical->abstract)
(def ^:private original-observe outcome/observe-operation!)

(defn- reject! [reason]
  (throw (ex-info "Invalid bounded physical history replay."
                  {:bounded-physical-history-replay/rejection true
                   :reason reason})))

(defn- check-source-seams! []
  (when-not (and (identical? original-require-current
                             (var-get #'histories/require-current-histories!))
                 (identical? original-with-app
                             (var-get #'physical/with-proving-application!))
                 (identical? original-initial-document
                             (var-get #'physical/initial-document))
                 (identical? original-project
                             (var-get #'physical/physical->abstract))
                 (identical? original-observe
                             (var-get #'outcome/observe-operation!)))
    (reject! :substituted-history-or-execution-source)))

(defn- checked-history-report! [report]
  (check-source-seams!)
  ;; Recompute the entire catalog-derived enumeration before opening an XTDB
  ;; node. Caller-authored histories or mutated reports cannot authorize a
  ;; different physical sequence.
  (original-require-current report)
  (let [initial (original-project (original-initial-document "v1504-reference"))
        hs (:histories report)
        steps (reduce + 0 (map (comp count :steps) hs))]
    (when-not (= initial (get-in report [:source :initial-state]))
      (reject! :initial-state-not-supported-by-physical-fixture))
    (when-not (and (vector? hs)
                   (= (count hs) (:history-count report))
                   (<= 1 (count hs) max-replay-histories)
                   (<= steps max-replay-steps))
      (reject! :physical-replay-resource-bound-exceeded))
    report))

(defn- physical-state! [app]
  ;; The application-owned projection is not itself certified. The v1499
  ;; observer independently queries XTDB for before/after evidence.
  (original-project ((:read! app))))

(defn- error-kind [^Exception ex]
  (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data ex)))
    :xtdb-assertion-rejected
    :execution-or-readback-exception))

(defn- check-step! [app {:keys [operation-id before after] :as step}]
  (try
    (let [physical-before (physical-state! app)]
      (if-not (= before physical-before)
        {:matches? false :reason :physical-prestate-disagrees}
        (let [observation (original-observe app operation-id)
              physical-after (physical-state! app)]
          (cond
            (not= after physical-after)
            {:matches? false :reason :physical-poststate-disagrees}

            (not (and (= operation-id (:operation-id observation))
                      (= :slot/reservation-lifecycle (:catalog-id observation))
                      (= :committed (:reported-commit-status observation))
                      (:public-transaction-key-shape? observation)
                      (:abstract-guard-checked? observation)
                      (:declared-invariants-evaluated? observation)
                      (:reported-outcome-and-observed-poststate-agree?
                       observation)
                      (:physical-poststate-matches-declared-transition?
                       observation)
                      (:abstract-poststate-matches-declared-transition?
                       observation)
                      (= :not-established (:physical-refinement observation))
                      (= :not-established (:authenticated-commit observation))
                      (= :not-issued (:certificate observation))
                      (= :not-admitted (:admission observation))
                      (= :none (:authority observation))))
            {:matches? false :reason :physical-outcome-evidence-incomplete}

            :else {:matches? true}))))
    (catch Exception ex
      {:matches? false :reason (error-kind ex)})))

(defn- replay-prefix! [history history-index]
  (original-with-app
   (fn [app]
     (let [initial-observed (try (physical-state! app)
                                 (catch Exception _ ::unreadable))
           initial-expected (first (:states history))]
       (if-not (= initial-expected initial-observed)
         {:completed-steps 0
          :first-mismatch {:history-index history-index
                           :step-index 0
                           :operation-id nil
                           :reason :physical-initial-state-disagrees}}
         (loop [step-index 0]
           (if (= step-index (count (:steps history)))
             {:completed-steps step-index :first-mismatch nil}
             (let [step (nth (:steps history) step-index)
                   observed (check-step! app step)]
               (if (:matches? observed)
                 (recur (inc step-index))
                 {:completed-steps step-index
                  :first-mismatch {:history-index history-index
                                   :step-index step-index
                                   :operation-id (:operation-id step)
                                   :reason (:reason observed)}})))))))))

(defn replay-histories!
  "Replay ALL source-derived prefixes against independent embedded XTDB nodes.
   Validate before any physical initialization. Stop at the FIRST discrepancy.
   No physical rows, IDs, exception messages, or abstract states are returned.

   A :bounded-agreement result describes observations only: repeated trace
   agreement does NOT certify transaction causality or refinement."
  [report]
  (checked-history-report! report)
  (let [hs (:histories report)]
    (loop [history-index 0
           checked-steps 0]
      (if (= history-index (count hs))
        (do
          (check-source-seams!)
          {:type report-type
           :version report-version
           :catalog-id (:catalog-id report)
           :catalog-fingerprint (:catalog-fingerprint report)
           :checked-prefixes history-index
           :completed-steps checked-steps
           :first-mismatch nil
           :status :bounded-external-readback-agreement
           :scope :independent-embedded-xtdb-node-per-abstract-prefix
           :checking :original-catalog-abstract-traces-and-v1499-physical-observer
           :replay-coverage :all-source-derived-prefixes-within-requested-bound
           :authenticated-commit :not-established
           :complete-writer-coverage :not-established
           :physical-refinement :not-established
           :proof-status :not-proved
           :certificate :not-issued
           :admission :not-admitted
           :authority :none
           :open-obligations obligations})
        (let [{:keys [completed-steps first-mismatch]}
              (replay-prefix! (nth hs history-index) history-index)
              total (+ checked-steps completed-steps)]
          (if first-mismatch
            (do
              (check-source-seams!)
              {:type report-type
               :version report-version
               :catalog-id (:catalog-id report)
               :catalog-fingerprint (:catalog-fingerprint report)
               :checked-prefixes history-index
               :completed-steps total
               :first-mismatch first-mismatch
               :status :bounded-physical-trace-disagreement
               :scope :independent-embedded-xtdb-node-per-abstract-prefix
               :checking :original-catalog-abstract-traces-and-v1499-physical-observer
               :replay-coverage :stopped-at-first-mismatch
               :authenticated-commit :not-established
               :complete-writer-coverage :not-established
               :physical-refinement :not-established
               :proof-status :not-proved
               :certificate :not-issued
               :admission :not-admitted
               :authority :none
               :open-obligations obligations})
            (recur (inc history-index) total)))))))

(defn redacted-summary
  "Pass through only classification and bounded counts; never return states."
  [replay-report]
  (when-not (and (map? replay-report)
                 (= report-type (:type replay-report))
                 (= report-version (:version replay-report))
                 (#{:bounded-external-readback-agreement
                    :bounded-physical-trace-disagreement}
                  (:status replay-report))
                 (= :not-established (:physical-refinement replay-report))
                 (= :not-issued (:certificate replay-report))
                 (= :not-admitted (:admission replay-report)))
    (reject! :invalid-replay-summary-source))
  (select-keys replay-report
               [:type :version :catalog-id :checked-prefixes
                :completed-steps :first-mismatch :status :scope
                :replay-coverage :physical-refinement :proof-status
                :certificate :admission :authority]))
