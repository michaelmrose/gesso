(ns gesso.assurance.proof.xtdb-gateway-atomic-submitted-readback
  "v1614: Opt-in, read-only join of v1612's authenticated PROCESS-LOCAL
   two-write submission return with v1608's single-basis XTDB observation.

   Prepare exactly ONE canonical atomic domain UPDATE + command-record CREATE
   gateway. A successful submit returns its ORIGINAL map unchanged. The
   observer accepts ONLY that identical, locally captured map; it derives
   the public key from it and reads the domain document, command record and
   ledger on the SAME node and from the SAME source/incorporation values
   captured at preparation. It never accepts an independently supplied key,
   node, expected poststate, command record, or observation callback.

   The lower v1612 gateway has already checked the restricted execute-tx
   return and its one-shot attempt journal. The new linkage is strictly
   process-local. The captured node and application declarations are still
   caller supplied; no proof establishes physical transaction causality,
   command authorization, record immutability, external-writer closure,
   universal refinement, retry safety, or Live settlement. If observation
   fails after submission, the transaction's outcome is UNKNOWN, not rejected.
   Reports are redacted internal diagnostics, not certificates or authority."
  (:require [gesso.assurance.proof.xtdb-gateway-atomic-readback :as readback]
            [gesso.assurance.proof.xtdb-gateway-complete-guard-execution :as complete])
  (:import [java.util.concurrent.atomic AtomicBoolean]
           [xtdb.api TransactionKey]))

(def report-type
  :gesso.assurance.proof.xtdb-gateway-atomic-submitted-readback/observation)
(def report-version 1)
(def gateway-type
  :gesso.assurance.proof.xtdb-gateway-atomic-submitted-readback/one-shot)
(def gateway-version 1)

(def ^:private original-incorporation
  complete/prepare-complete-guard-command-incorporation-one-shot!)
(def ^:private original-readback readback/acquire-atomic-command-readback!)

(defn- reject! [reason]
  (throw (ex-info "Unsupported XTDB source-bound atomic return/readback."
                  {:xtdb-gateway-atomic-submitted-readback/rejection true
                   :reason reason})))

(defn- seams! []
  ;; Detect ordinary Var replacement, NOT JVM instrumentation, hostile
  ;; reflection, arbitrary third-party writes, or cryptographic provenance.
  (when-not (and (identical? original-incorporation
                             (var-get #'complete/prepare-complete-guard-command-incorporation-one-shot!))
                 (identical? original-readback
                             (var-get #'readback/acquire-atomic-command-readback!)))
    (reject! :substituted-submission-or-readback-seam)))

(defn- require-observation! [report]
  (when-not
    (and (map? report)
         (= readback/report-type (:type report))
         (= readback/report-version (:version report))
         (contains? #{:bounded-poststate-command-and-ledger-coobservation
                      :postsubmission-correspondence-not-established}
                    (:status report))
         (= :bounded-postsubmission-read-only-coobservation
            (:proof-status report))
         (true? (:three-observations-share-pinned-basis? report))
         (false? (:key-attributed-to-command-creation? report))
         (false? (:submitted-transaction-contents-authenticated? report))
         (false? (:retry-authorized? report))
         (= :not-established (:transaction-causality report))
         (= :none (:transaction-authority report)
            (:live-settlement-authority report)
            (:authority report))
         (= :not-issued (:certificate report))
         (= :not-admitted (:admission report))
         (every? keyword?
                 ((juxt :domain-poststate-status :command-record-status
                        :ledger-status :basis-order-status) report)))
    (reject! :invalid-pinned-readback-report))
  report)

(defn prepare-source-bound-atomic-return-readback-one-shot!
  "[node original-one-update-source incorporation-input] ->
   {:type :version :submit! :observe-return! :summary!}.

   :submit! is one-shot and returns the EXACT underlying v1612 submission map.
   :observe-return! must be called with THAT map, exactly once, after the
   successful local return. It queries the same captured node/source/input,
   never an independently supplied key or expected document. A reported
   failure after submission is an INDETERMINATE physical outcome.

   The result is a redacted, bounded co-observation with an additional local
   return-origin claim. It does NOT imply the returned key caused the writes.
   This is a building block, not yet installed in the certified Biff writer."
  [node source incorporation-input]
  (seams!)
  (let [prepared (original-incorporation node source incorporation-input)
        initial (try ((:summary! prepared))
                     (catch Throwable _
                       (reject! :invalid-atomic-gateway-summary)))
        _ (when-not
            (and (= complete/incorporation-type (:type prepared))
                 (= complete/incorporation-version (:version prepared))
                 (fn? (:submit! prepared)) (fn? (:summary! prepared))
                 (= :prepared (:status initial))
                 (= :prepared (:underlying-gateway-status initial))
                 (= 0 (:owned-execute-tx-call-entries initial))
                 (= 2 (:admitted-transaction-write-count initial))
                 (= :one-derived-xtdb-vector
                    (:single-restricted-submission initial))
                 (= :not-issued (:certificate initial))
                 (= :not-admitted (:admission initial)))
            (reject! :unexpected-atomic-gateway-contract))
        attempted (AtomicBoolean. false)
        observation-attempted (AtomicBoolean. false)
        ;; Keep the exact returned object private, as v1527 does. It must
        ;; not be reconstructed from its public :tx-key or supplied by host.
        local-return (atom nil)
        observation-state (atom :not-observed)
        submit! (fn []
                  (when-not (.compareAndSet attempted false true)
                    (reject! :one-shot-already-attempted))
                  (seams!)
                  ;; v1612 checks source-bound restricted return and journal
                  ;; before it permits this exact object to propagate.
                  (let [response ((:submit! prepared))]
                    (when-not (and (map? response)
                                   (instance? TransactionKey (:tx-key response)))
                      (reject! :invalid-local-return))
                    (reset! local-return response)
                    response))
        observe-return!
        (fn [response]
          ;; Checking exact identity BEFORE any physical read rules out a
          ;; foreign or reconstructed public TransactionKey as readback input.
          (when-not (and (some? @local-return)
                         (identical? response @local-return))
            (reject! :not-this-submission-local-return))
          (when-not (.compareAndSet observation-attempted false true)
            (reject! :readback-already-attempted))
          (reset! observation-state :outcome-unknown-or-unobserved)
          (seams!)
          (let [r (require-observation!
                   (original-readback node source incorporation-input
                                      (:tx-key response)))
                agreement? (= :bounded-poststate-command-and-ledger-coobservation
                              (:status r))
                report
                {:type report-type
                 :version report-version
                 :status (if agreement?
                           :bounded-local-return-and-pinned-readback-coobservation
                           :postsubmission-correspondence-not-established)
                 :process-local-return :exact-object-from-this-issued-submission
                 :submitted-gateway :source-bound-two-write-one-shot
                 :readback-node-and-inputs :same-captured-submission-inputs
                 :readback-status (:status r)
                 :domain-poststate-status (:domain-poststate-status r)
                 :command-record-status (:command-record-status r)
                 :ledger-status (:ledger-status r)
                 :basis-order-status (:basis-order-status r)
                 :three-observations-share-pinned-basis? true
                 :public-key-origin :process-local-submission-return
                 :backend-node-authenticity :caller-supplied-unattested
                 :transaction-causality :not-established
                 :record-immutability :not-established
                 :principal-and-command-authenticity :application-premise
                 :external-writer-closure :not-established
                 :universal-refinement :not-established
                 :retry-authorized? false
                 :live-settlement-authority :none
                 :transaction-authority :none
                 :certificate :not-issued
                 :admission :not-admitted
                 :authority :none
                 :proof-status :bounded-process-local-return-and-pinned-coobservation}]
            (seams!)
            (reset! observation-state (:status report))
            report))
        summary!
        (fn []
          (seams!)
          {:type gateway-type
           :version gateway-version
           :submission-status (if (.get attempted) :permit-consumed :prepared)
           :readback-status @observation-state
           :underlying-submission :one-source-derived-atomic-two-write-gateway
           :public-key-origin :requires-exact-local-return-object
           :readback-identity :same-captured-node-and-source
           :transaction-causality :not-established
           :backend-node-authenticity :caller-supplied-unattested
           :external-writer-closure :not-established
           :retry-authorized? false
           :live-settlement-authority :none
           :certificate :not-issued
           :admission :not-admitted
           :authority :none})]
    (seams!)
    {:type gateway-type
     :version gateway-version
     :submit! submit!
     :observe-return! observe-return!
     :summary! summary!}))
