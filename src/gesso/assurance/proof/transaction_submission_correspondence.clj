(ns gesso.assurance.proof.transaction-submission-correspondence
  "v1375: Source-rechecked comparison of a v1373 acquired transaction plan
   with separately RECORDED operations attributed to an XTDB submission.

   Replays the ORIGINAL v1373 event contract against a freshly checked writer
   source and regenerates production XTDB operations with gesso.model.tx/prepare.
   The result establishes structural agreement or divergence between this
   deterministic lowering and a supplied vector. The recorded vector is
   STILL CALLER-SUPPLIED: no proof exists that XTDB received those bytes, that
   the record is from an actual execution, or that the submitted transaction
   has the claimed pre-state, write set, identity, or outcome. A real controlled
   instrumented run is useful bounded evidence, never universal refinement.

   This audit never submits a transaction. It does not disclose document or
   command contents in its error diagnostics or output (only mismatch indices
   and operation counts). Consumers must not turn this report into authority."
  (:require [gesso.assurance.proof.transaction-bound-evidence-acquisition :as acq]
            [gesso.model.resolved-operation :as resolved]
            [gesso.model.tx :as tx]
            [gesso.model.tx-model :as tx-model]))

(def comparison-version 1)
(def comparison-type
  :gesso.assurance.proof.transaction-submission-correspondence/comparison)

(def ^:private input-keys
  #{:acquisition-source :acquisition-report :recorded-tx-ops})
(def ^:private original-acquire acq/start-acquisition!)
(def ^:private original-prepare tx/prepare)
(def ^:private original-admitted tx-model/admitted-plan)
(def ^:private max-recorded-ops 128)

(defn- reject! [reason at]
  ;; No command, plan, document, operation, receipt, or source data in ex-data.
  (throw (ex-info "Invalid transaction-submission correspondence source."
                  {:transaction-submission-correspondence/rejection true
                   :reason reason :at at})))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- exact! [x expected at]
  (when-not (and (plain-map? x) (nil? (meta x))
                 (= expected (set (keys x))))
    (reject! :invalid-source-shape at))
  x)

(defn- original-checkers! []
  (when-not (and (identical? original-acquire
                             (var-get #'acq/start-acquisition!))
                 (identical? original-prepare (var-get #'tx/prepare))
                 (identical? original-admitted
                             (var-get #'tx-model/admitted-plan)))
    (reject! :substituted-upstream-checker [])))

(defn- acquisition-report! [report]
  ;; A complete source-replay check follows; this guard is deliberately strict
  ;; about the proof/authority boundary BEFORE using report data in a replay.
  (when-not (and (plain-map? report) (nil? (meta report))
                 (= acq/acquisition-type (:type report))
                 (= acq/acquisition-version (:version report))
                 (= :process-local-correlated-resolved-operation-events
                    (:checking report))
                 (= :one-injected-hook-attempt (:scope report))
                 (uuid? (:attempt-id report))
                 (#{:committed :rejected} (:captured-physical-status report))
                 (boolean? (:reported-receipt-present? report))
                 (= :injected-process-local-hook (:observation-origin report))
                 (false? (:hook-origin-authenticated? report))
                 (= :not-established (:captured-plan-to-issued-plan-correspondence report))
                 (= :not-established (:submitted-xtdb-ops-equivalence report))
                 (= :not-established (:actual-submitted-payload-provenance report))
                 (= :not-established (:transaction-internal-state-provenance report))
                 (= :not-established (:universal-execution-refinement report))
                 (= :not-established (:cross-source-transition-equivalence report))
                 (= :hook-observation-diagnostic-only (:proof-status report))
                 (= :not-issued (:certificate report))
                 (= :not-admitted (:admission report))
                 (= :none (:transaction-authority report))
                 (= :none (:authority report)))
    (reject! :invalid-acquisition-report [:acquisition-report]))
  report)

(defn- replay! [source report]
  ;; Never trust a persisted acquired report merely because its fields look
  ;; correct: rebuild a real v1373 session using CURRENT writer source.
  (let [session (original-acquire source)
        common {:scope resolved/execution-observation-scope
                :attempt-id (:attempt-id report)
                :operation (:operation-id report)
                :binding (:command-binding report)
                :record (:captured-command-record report)
                :plan (:captured-prepared-plan report)}
        prepared (assoc common :phase :prepared)
        outcome (cond-> (assoc common
                               :phase :physical-outcome
                               :physical/status (:captured-physical-status report)
                               :physical/source (:captured-physical-source report))
                  (:reported-receipt-present? report)
                  (assoc :physical/receipt (:captured-physical-receipt report)))
        rebuilt (try
                  ((:observe! session) prepared)
                  ((:observe! session) outcome)
                  ((:complete! session))
                  (catch clojure.lang.ExceptionInfo _
                    (reject! :acquisition-replay-rejected [:acquisition-report])))]
    (when-not (= report rebuilt)
      (reject! :stale-or-altered-acquisition-report [:acquisition-report]))
    rebuilt))

(defn- recorded-ops! [value]
  (when-not (and (vector? value) (nil? (meta value))
                 (<= 1 (count value) max-recorded-ops))
    (reject! :invalid-recorded-operations [:recorded-tx-ops]))
  value)

(defn audit-recorded-submission!
  "Compare the canonical XTDB ops rederived from a freshly rechecked v1373
   acquisition against a SEPARATELY SUPPLIED recorded operation vector.

   Input: {:acquisition-source <original v1373 source>
           :acquisition-report <complete v1373 report>
           :recorded-tx-ops <vector attributed to low-level submission>}.

   Returned evidence is BOUNDED and NON-AUTHENTICATED, even when every op
   matches and the acquisition reports a committed outcome. In particular,
   recorded-tx-ops is NOT independently extracted from XTDB or cryptographically
   bound to a transaction. No tx is submitted or retried here."
  [input]
  (exact! input input-keys [])
  (original-checkers!)
  (let [{:keys [acquisition-source acquisition-report recorded-tx-ops]} input
        _ (acquisition-report! acquisition-report)
        _ (recorded-ops! recorded-tx-ops)
        acquired (replay! acquisition-source acquisition-report)
        plan (original-admitted (:captured-prepared-plan acquired))
        canonical (:tx-ops (original-prepare {} plan))
        absent (Object.)
        mismatches (->> (range (max (count canonical) (count recorded-tx-ops)))
                        (filter (fn [idx]
                                  (not= (get canonical idx absent)
                                        (get recorded-tx-ops idx absent))))
                        vec)
        matches? (empty? mismatches)]
    ;; A truly empty canonical operation list cannot demonstrate a transaction
    ;; submission, even when an independently fabricated record is nonempty.
    (when-not (and (vector? canonical) (seq canonical))
      (reject! :empty-canonical-transaction [:captured-prepared-plan]))
    {:type comparison-type
     :version comparison-version
     :status (if matches? :recorded-operations-agree
                 :recorded-operations-diverge)
     :checking :fresh-acquisition-replay-and-production-plan-lowering
     :operation-id (:operation-id acquired)
     :attempt-id (:attempt-id acquired)
     :issued-application-fingerprint (:issued-application-fingerprint acquired)
     :issued-plan-transition-fingerprint
     (:issued-plan-transition-fingerprint acquired)
     :independent-transition-fingerprint
     (:independent-transition-fingerprint acquired)
     :captured-physical-status (:captured-physical-status acquired)
     :canonical-op-count (count canonical)
     :recorded-op-count (count recorded-tx-ops)
     :mismatched-op-indices mismatches
     :recorded-operations-match? matches?
     :recorded-operation-origin :caller-supplied-unattested
     :comparison-scope :canonical-model-tx-prepare-vs-recorded-xtdb-ops
     :actual-low-level-submission-authenticated? false
     :actual-submitted-payload-provenance :not-established
     :transaction-id-authenticated? false
     :transaction-internal-prestate-provenance :not-established
     :complete-physical-write-footprint :not-established
     :issued-abstract-transition-equivalence :not-established
     :universal-execution-refinement :not-established
     :original-executable-writer-obligations :still-open
     :proof-status :recorded-submission-diagnostic-only
     :admission :not-admitted
     :certificate :not-issued
     :transaction-authority :none
     :authority :none}))

(defn require-current-recorded-submission!
  "Freshly reconstruct all premises and reject stale/forged comparison reports.
   Success does not turn caller-supplied recorded operations into a receipt."
  [input previously-reported]
  (let [current (audit-recorded-submission! input)]
    (when-not (= current previously-reported)
      (reject! :stale-or-altered-comparison-report [:comparison-report]))
    current))
