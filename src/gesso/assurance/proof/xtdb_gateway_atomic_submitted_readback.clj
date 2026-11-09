(ns gesso.assurance.proof.xtdb-gateway-atomic-submitted-readback
  "v1614/v1616: Opt-in, read-only join of v1612's authenticated PROCESS-LOCAL
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
   v1616 adds the stronger v1602 certified-plan-bound monotonic two-write
   constructor, still without an identity-registered Biff request writer.
   Reports are redacted internal diagnostics, not certificates or authority."
  (:require [gesso.assurance.proof.xtdb-gateway-atomic-readback :as readback]
            [gesso.assurance.proof.xtdb-gateway-complete-guard-execution :as complete]
            [gesso.assurance.proof.xtdb-gateway-monotonic-revision-execution :as monotonic])
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
(def ^:private original-plan-bound
  monotonic/prepare-plan-bound-monotonic-command-incorporation-one-shot!)

(def plan-bound-gateway-type
  :gesso.assurance.proof.xtdb-gateway-atomic-submitted-readback/plan-bound-one-shot)
(def plan-bound-gateway-version 1)

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


(defn- plan-bound-seams! []
  (seams!)
  (when-not (identical?
             original-plan-bound
             (var-get #'monotonic/prepare-plan-bound-monotonic-command-incorporation-one-shot!))
    (reject! :substituted-certified-plan-incorporation-seam)))

(defn- require-prepared! [kind prepared]
  (let [initial (try ((:summary! prepared))
                     (catch Throwable _
                       (reject! :invalid-atomic-gateway-summary)))]
    (when-not
      (and (map? prepared)
           (= #{:type :version :submit! :summary!} (set (keys prepared)))
           (fn? (:submit! prepared)) (fn? (:summary! prepared))
           (case kind
             :ordinary
             (and (= complete/incorporation-type (:type prepared))
                  (= complete/incorporation-version (:version prepared))
                  (= :prepared (:status initial))
                  (= :prepared (:underlying-gateway-status initial))
                  (= 0 (:owned-execute-tx-call-entries initial))
                  (= 2 (:admitted-transaction-write-count initial))
                  (= :one-derived-xtdb-vector
                     (:single-restricted-submission initial)))
             :plan-bound
             (and (= monotonic/plan-bound-incorporation-type (:type prepared))
                  (= monotonic/plan-bound-incorporation-version (:version prepared))
                  (= :prepared (:status initial))
                  (= :prepared (:underlying-gateway-status initial))
                  (= 0 (:owned-execute-tx-call-entries initial))
                  (= 1 (:physical-submission-gateways-prepared initial))
                  (= 2 (:admitted-transaction-write-count initial))
                  (= :complete-top-level-cas (:domain-update initial))
                  (= :canonical-absence-precondition-prepared
                     (:command-record-create initial))
                  (= :conditional-on-xtdb-assert-and-put-semantics
                     (:atomicity initial))
                  (= :source-checked-exactly-one-nonwrapping-successor
                     (:revision-policy initial))
                  (= :exact-current-issued-callback-checked-at-preparation
                     (:plan-target-correspondence initial))
                  (= :same-node-pinned-read-matched-at-preparation
                     (:external-prestate initial))
                  (= :bound-to-selected-route
                     (:command-record-operation initial))
                  (= :conditional-plan-bound-atomic-incorporation
                     (:proof-status initial)))
             false)
           (= :not-issued (:certificate initial))
           (= :not-admitted (:admission initial)))
      (reject! :unexpected-atomic-gateway-contract))))

(defn- wrap-submission! [kind node source incorporation-input prepared]
  ;; Only one already prepared gateway is accepted. No disposable second
  ;; gateway may be prepared for a preflight or a readback operation.
  (let [check! (if (= kind :plan-bound) plan-bound-seams! seams!)
        _ (check!)
        _ (require-prepared! kind prepared)
        attempted (AtomicBoolean. false)
        observation-attempted (AtomicBoolean. false)
        local-return (atom nil)
        observation-state (atom :not-observed)
        gate-type (if (= kind :plan-bound) plan-bound-gateway-type gateway-type)
        gate-version (if (= kind :plan-bound) plan-bound-gateway-version gateway-version)
        submission-name (if (= kind :plan-bound)
                          :certified-plan-bound-monotonic-atomic-two-write-one-shot
                          :source-bound-two-write-one-shot)
        submit! (fn []
                  (when-not (.compareAndSet attempted false true)
                    (reject! :one-shot-already-attempted))
                  (check!)
                  ;; v1602's submission rechecks its issued certified plan
                  ;; target and exact-successor policy after this permit
                  ;; has been irrevocably consumed.
                  (let [response ((:submit! prepared))]
                    (when-not (and (map? response)
                                   (instance? TransactionKey (:tx-key response)))
                      (reject! :invalid-local-return))
                    (reset! local-return response)
                    response))
        observe-return!
        (fn [response]
          (when-not (and (some? @local-return)
                         (identical? response @local-return))
            (reject! :not-this-submission-local-return))
          (when-not (.compareAndSet observation-attempted false true)
            (reject! :readback-already-attempted))
          (reset! observation-state :outcome-unknown-or-unobserved)
          (check!)
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
                 :submitted-gateway submission-name
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
            (check!)
            (reset! observation-state (:status report))
            report))
        summary!
        (fn []
          (check!)
          {:type gate-type
           :version gate-version
           :submission-status (if (.get attempted) :permit-consumed :prepared)
           :readback-status @observation-state
           :underlying-submission (if (= kind :plan-bound)
                                    :one-certified-plan-bound-monotonic-atomic-two-write-gateway
                                    :one-source-derived-atomic-two-write-gateway)
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
    (check!)
    {:type gate-type
     :version gate-version
     :submit! submit!
     :observe-return! observe-return!
     :summary! summary!}))

(defn prepare-source-bound-atomic-return-readback-one-shot!
  "v1614: Read back the exact local return from one canonical atomic
   two-write gateway, on the captured node/source/command inputs. This
   preexisting constructor does NOT certify a plan or monotonic policy."
  [node source incorporation-input]
  (seams!)
  (wrap-submission! :ordinary node source incorporation-input
                    (original-incorporation node source incorporation-input)))

(defn prepare-plan-bound-atomic-return-readback-one-shot!
  "v1616: Prepare ONE certified-plan-bound, monotonic, complete-CAS,
   two-write atomic incorporation gateway, and observe only its EXACT local
   submission return on a pinned XTDB snapshot. The certified operation and
   issued target resolver are rechecked by the v1602 gateway on submission.

   [node contract-source resolver-issuance selected-operation request
    candidate-source revision-field incorporation-input]

   This is a building block for request-scoped registration, not an issued
   Biff callback itself. It does NOT authenticate the caller-supplied node,
   principal, command arguments, outcomes, transaction causality or deployed
   writer coverage. Observation failure after submission is UNKNOWN, never
   evidence of rejection or a permission to retry."
  [node contract-source resolver-issuance selected-operation request
   candidate-source revision-field incorporation-input]
  (plan-bound-seams!)
  (wrap-submission!
   :plan-bound node candidate-source incorporation-input
   (original-plan-bound node contract-source resolver-issuance
                        selected-operation request candidate-source
                        revision-field incorporation-input)))
