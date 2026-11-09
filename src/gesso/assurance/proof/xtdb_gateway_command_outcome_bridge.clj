(ns gesso.assurance.proof.xtdb-gateway-command-outcome-bridge
  "v1622: An opt-in, read-only join of an issued plan-bound atomic XTDB
   submission, its *exact* process-local return, pinned physical readback,
   canonical command reconciliation, and a caller-reported request outcome.

   The v1616 lower gateway already performs a single physical two-write
   transaction and checks that the pinned snapshot contains the complete
   canonical command record. We do not perform a second read at a different
   basis: reconciliation uses the immutable canonical record only AFTER the
   lower pinned observer has established exact record equality.

   An application-supplied disclosure predicate gates result comparison. The
   reported request outcome, predicate, command arguments, principal, XTDB
   node and installed plan are NOT independently authenticated here. A positive
   report is bounded co-observation and value agreement, NOT an authoritative
   Ring response, commit causality, operation refinement, permission to retry,
   or Live/browser settlement. An exception after submission leaves the
   authoritative outcome unknown to this caller, never rejected."
  (:require [gesso.assurance.proof.xtdb-gateway-atomic-submitted-readback
             :as submitted]
            [gesso.model.command-resolution :as resolution])
  (:import [java.util.concurrent.atomic AtomicBoolean]))

(def gateway-type
  :gesso.assurance.proof.xtdb-gateway-command-outcome-bridge/one-shot)
(def gateway-version 1)
(def report-type
  :gesso.assurance.proof.xtdb-gateway-command-outcome-bridge/observation)
(def report-version 1)

(def ^:private original-prepare
  submitted/prepare-plan-bound-atomic-return-readback-one-shot!)
(def ^:private original-command-record resolution/command-record)
(def ^:private original-reconcile resolution/reconcile)

(defn- reject! [reason]
  (throw (ex-info "Unsupported source-bound command-outcome reconciliation."
                  {:xtdb-gateway-command-outcome-bridge/rejection true
                   :reason reason})))

(defn- seams! []
  (when-not (and (identical? original-prepare
                             (var-get #'submitted/prepare-plan-bound-atomic-return-readback-one-shot!))
                 (identical? original-command-record
                             (var-get #'resolution/command-record))
                 (identical? original-reconcile
                             (var-get #'resolution/reconcile)))
    (reject! :substituted-command-outcome-seam)))

(defn- require-lower! [gateway]
  (let [summary (try ((:summary! gateway))
                     (catch Throwable _ (reject! :invalid-underlying-gateway)))]
    (when-not (and (map? gateway)
                   (= #{:type :version :submit! :observe-return! :summary!}
                      (set (keys gateway)))
                   (= submitted/plan-bound-gateway-type (:type gateway))
                   (= submitted/plan-bound-gateway-version (:version gateway))
                   (every? fn? ((juxt :submit! :observe-return! :summary!) gateway))
                   (= :prepared (:submission-status summary))
                   (= :not-observed (:readback-status summary))
                   (= :one-certified-plan-bound-monotonic-atomic-two-write-gateway
                      (:underlying-submission summary))
                   (= :requires-exact-local-return-object
                      (:public-key-origin summary))
                   (= :not-established (:transaction-causality summary))
                   (false? (:retry-authorized? summary))
                   (= :not-issued (:certificate summary))
                   (= :none (:authority summary)))
      (reject! :invalid-underlying-gateway))
    gateway))

(defn- require-physical-report! [report]
  (when-not
    (and (map? report)
         (= submitted/report-type (:type report))
         (= submitted/report-version (:version report))
         (#{:bounded-local-return-and-pinned-readback-coobservation
            :postsubmission-correspondence-not-established} (:status report))
         (= :exact-object-from-this-issued-submission
            (:process-local-return report))
         (= :same-captured-submission-inputs
            (:readback-node-and-inputs report))
         (true? (:three-observations-share-pinned-basis? report))
         (= :bounded-process-local-return-and-pinned-coobservation
            (:proof-status report))
         (= :not-established (:transaction-causality report))
         (= :not-issued (:certificate report))
         (= :not-admitted (:admission report))
         (= :none (:authority report) (:transaction-authority report)
            (:live-settlement-authority report))
         (false? (:retry-authorized? report)))
    (reject! :invalid-source-bound-physical-report))
  report)

(defn prepare-plan-bound-command-outcome-bridge-one-shot!
  "Prepare exactly one v1616 plan-bound atomic submission/readback gateway.

   Arguments are the v1616 plan-bound inputs followed by a TRUSTED
   `(fn [binding canonical-command-record] -> boolean)` disclosure predicate:
     [node contract-source resolver-issuance selected-operation request
      candidate-source revision-field incorporation-input may-disclose?]

   The returned callbacks are {:submit! (fn [])
                               :reconcile-return! (fn [exact-return
                                                        reported-request-outcome])
                               :summary! (fn [])}.

   `reported-request-outcome` is an application-reported command outcome, not
   a Ring response automatically authenticated by this constructor. Gesso
   checks exact value agreement with the already pinned-matched command
   record, subject to disclosure. It neither exposes the stored outcome nor
   decides whether to return HTTP success, retry, replay, or settle Live.
   A mismatch is a diagnostic, not a claim that the physical write failed."
  [node contract-source resolver-issuance selected-operation request
   candidate-source revision-field incorporation-input may-disclose?]
  (seams!)
  (when-not (fn? may-disclose?)
    (reject! :missing-trusted-disclosure-predicate))
  (let [{:keys [binding outcome now]} incorporation-input
        canonical (try (original-command-record binding outcome now)
                       (catch Throwable _ (reject! :invalid-command-outcome-input)))
        lower (require-lower!
               (original-prepare node contract-source resolver-issuance
                                 selected-operation request candidate-source
                                 revision-field incorporation-input))
        local-return (atom nil)
        observed (AtomicBoolean. false)
        status (atom :not-observed)
        submit! (fn []
                  (seams!)
                  (let [response ((:submit! lower))]
                    (reset! local-return response)
                    response))
        reconcile-return!
        (fn [response reported-request-outcome]
          ;; Forged returns never consume the genuine observation permit.
          (when-not (and (some? @local-return)
                         (identical? response @local-return))
            (reject! :not-this-submission-local-return))
          (when-not (.compareAndSet observed false true)
            (reject! :observation-already-attempted))
          ;; The transaction may have committed even if any following step
          ;; throws. Never classify such an exception as a rejected write.
          (reset! status :outcome-unknown-or-unobserved)
          (seams!)
          (let [physical (require-physical-report!
                          ((:observe-return! lower) response))
                physical? (= :bounded-local-return-and-pinned-readback-coobservation
                             (:status physical))
                reported? (= outcome reported-request-outcome)
                ;; Disclosure takes precedence over comparison. Otherwise a
                ;; caller could probe a confidential recorded outcome by
                ;; supplying guesses and inspecting :mismatch vs :match.
                decision (when physical?
                           (try (original-reconcile binding canonical may-disclose?)
                                (catch Throwable _
                                  (reject! :disclosure-or-reconciliation-unavailable))))
                admitted? (and physical? reported?
                               (= :replay (:status decision))
                               (= reported-request-outcome (:outcome decision)))
                decision-status (if (nil? decision)
                                  :not-evaluated
                                  (case (:status decision)
                                    :replay :compatible-and-disclosed
                                    :unavailable :disclosure-unavailable
                                    :incompatible :incompatible
                                    :unresolved :unresolved
                                    :invalid))
                _ (when (and (some? decision)
                             (= :invalid decision-status))
                    (reject! :invalid-canonical-reconciliation-result))
                result-status (cond
                                (not physical?) :physical-coobservation-not-established
                                (= :disclosure-unavailable decision-status)
                                :disclosure-unavailable
                                (not reported?) :reported-request-outcome-mismatch
                                admitted? :bounded-reported-outcome-and-command-record-agreement
                                :else :command-outcome-correspondence-not-established)
                report {:type report-type
                        :version report-version
                        :status result-status
                        :source-bound-return :exact-object-from-issued-submission
                        :physical-readback-status (:status physical)
                        :record-status (:command-record-status physical)
                        :ledger-status (:ledger-status physical)
                        :reported-request-outcome-status
                        (cond
                          (not physical?) :not-evaluated
                          (= :disclosure-unavailable decision-status) :withheld
                          reported? :matches-captured-outcome
                          :else :differs-from-captured-outcome)
                        :canonical-reconciliation-status decision-status
                        :record-acquisition :lower-pinned-readback-exact-canonical-equality
                        :request-outcome-origin :application-reported-unattested
                        :disclosure-policy-origin :trusted-application-premise
                        :command-arguments-and-principal :application-premise
                        :physical-transaction-causality :not-established
                        :record-immutability :not-established
                        :external-writer-closure :not-established
                        :universal-operation-refinement :not-established
                        :retry-authorized? false
                        :automatic-replay-authorized? false
                        :transaction-authority :none
                        :live-settlement-authority :none
                        :certificate :not-issued
                        :admission :not-admitted
                        :authority :none
                        :proof-status :bounded-source-bound-outcome-coobservation}]
            (seams!)
            (reset! status result-status)
            report))
        summary! (fn []
                   (seams!)
                   {:type gateway-type
                    :version gateway-version
                    :submission-status (:submission-status ((:summary! lower)))
                    :outcome-reconciliation-status @status
                    :underlying-submission :one-certified-plan-bound-atomic-two-write-gateway
                    :request-outcome-origin :application-reported-unattested
                    :physical-transaction-causality :not-established
                    :retry-authorized? false
                    :live-settlement-authority :none
                    :certificate :not-issued
                    :authority :none})]
    (seams!)
    {:type gateway-type
     :version gateway-version
     :submit! submit!
     :reconcile-return! reconcile-return!
     :summary! summary!}))
