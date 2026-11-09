(ns gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate
  "v1586: opt-in ONE-document bridge from the exact Gesso-issued target
   callback installed in a current certified plan to an XTDB-acquired
   external prestate and a canonical, complete top-level guarded UPDATE.

   This composes, rather than reimplements, v1584 plan-bound route target
   provenance, v1574 command/target comparison, and v1539 acquired-prestate
   complete-CAS submission. It refuses the weaker startup-only target resolver.
   A one-use wrapper checks source correspondence again immediately before
   forwarding to the existing one-shot guarded gateway. The inner submit!
   and gateway descriptor never escape independently through this API.

   This is NOT an application installation certificate. The plan's literal
   document table remains application-authored, the XTDB node is supplied by
   the caller, its pinned read is external to the transaction, and the guard
   relies on the reviewed XTDB ASSERT/PUT semantics. ABA histories, actual
   transaction-internal read bases, unrelated writers, principal authorization,
   business-rule refinement, and physical commit causality are NOT proved.
   No source maps, documents, targets, SQL, tokens, or transaction keys appear
   in our diagnostics. The :submit! closure is real, narrow transaction
   authority, not a proof of the business operation."
  (:require
   [gesso.assurance.proof.xtdb-gateway-route-target-provenance :as provenance]
   [gesso.assurance.proof.xtdb-gateway-operation-target-binding :as binding]
   [gesso.assurance.proof.xtdb-gateway-acquired-complete-guard-execution :as acquired])
  (:import (java.util.concurrent.atomic AtomicBoolean)))

(def report-type
  :gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate/one-shot)
(def report-version 1)

(def ^:private original-check
  provenance/require-current-certified-literal-route-target-resolver!)
(def ^:private original-target-check
  binding/require-declared-operation-target!)
(def ^:private original-acquire
  acquired/prepare-acquired-complete-guard-one-shot!)

(defn- reject! [reason]
  (throw (ex-info "Unsupported plan-bound guarded XTDB prestate."
                  {:xtdb-gateway-plan-bound-guarded-prestate/rejection true
                   :reason reason})))

(defn- plain-map? [m]
  (and (or (instance? clojure.lang.PersistentHashMap m)
           (instance? clojure.lang.PersistentArrayMap m))
       (nil? (meta m))))

(defn- pinned! []
  ;; Detect ordinary Var replacement; this is not an authenticated runtime.
  (when-not (and (identical? original-check
                             (var-get #'provenance/require-current-certified-literal-route-target-resolver!))
                 (identical? original-target-check
                             (var-get #'binding/require-declared-operation-target!))
                 (identical? original-acquire
                             (var-get #'acquired/prepare-acquired-complete-guard-one-shot!)))
    (reject! :substituted-target-or-prestate-checker)))

(defn- checked-correspondence!
  [contract-source issued-resolver selected-operation request candidate-source]
  (pinned!)
  (let [report (try
                 (original-check issued-resolver contract-source)
                 (catch Exception _
                   (reject! :unissued-or-changed-target-resolver)))]
    (when-not (and (= provenance/report-type (:type report))
                   (= provenance/report-version (:version report))
                   (= :closed-issued-certified-plan-target-callback
                      (:target-identity report))
                   (= :exact-issued-source-callback-checked
                      (:certified-plan-target-callback-identity report))
                   (= :not-issued (:certificate report))
                   (= :none (:transaction-authority report)))
      (reject! :requires-exact-issued-plan-bound-resolver)))
  (let [expected (try
                   (issued-resolver request)
                   (catch Exception _
                     (reject! :plan-bound-target-resolution-failed)))
        result (try
                 (original-target-check selected-operation expected
                                        candidate-source)
                 (catch Exception _
                   (reject! :command-or-protection-disagrees-with-plan-target)))]
    (when-not (and (= binding/report-type (:type result))
                   (= binding/report-version (:version result))
                   (= :conditional-declared-command-target-match
                      (:status result))
                   (true? (:selected-route-operation-checked? result))
                   (true? (:canonical-command-operation-checked? result))
                   (true? (:declared-command-and-protected-document-match-expected-target?
                           result))
                   (= :local-structural-comparison-only (:proof-status result)))
      (reject! :unexpected-command-target-check-result)))
  (pinned!)
  true)

(defn prepare-plan-bound-acquired-one-shot!
  "For one certified literal route, join its exact issued plan :target!
   callback to the candidate command/protection, then independently acquire
   the candidate's complete, supported physical prestate from `node`.

   `resolver-issuance` MUST be the exact
   {:expected-target! <Gesso-issued plan-bound resolver>} obtained via
   issue-certified-plan-bound-literal-route-target-resolver!. `contract-source`
   is the corresponding currently checked application contract, and
   `selected-operation` is the independently selected qualified route
   operation. `request` is the original Ring request; it is never used as an
   XTDB query. `candidate-source` is the one-UPDATE source accepted by the
   existing acquired complete-guard gateway.

   No write occurs during preparation. No submit capability escapes if target
   correspondence or the external prestate read fails. A successful result
   returns only {:type :version :submit! :summary!}. The wrapper's one permit
   is consumed BEFORE revalidation on its first submission attempt, so a
   failed pre-submit check cannot be retried through the same wrapper.

   The transaction's complete top-level ASSERTs can reject a conflicting
   later prewrite state; they do not authenticate the external read or detect
   ABA, guarantee commit, prove target authorization, or control raw writers."
  [node contract-source resolver-issuance selected-operation request
   candidate-source]
  (pinned!)
  (when-not (and (some? node)
                 (plain-map? resolver-issuance)
                 (= #{:expected-target!} (set (keys resolver-issuance)))
                 (fn? (:expected-target! resolver-issuance))
                 (plain-map? request)
                 (keyword? selected-operation)
                 (some? (namespace selected-operation)))
    (reject! :invalid-plan-bound-prestate-input))
  (let [resolver (:expected-target! resolver-issuance)]
    (checked-correspondence! contract-source resolver selected-operation
                             request candidate-source)
    ;; The existing acquired gateway FIRST constructs a complete top-level
    ;; CAS and only THEN reads the candidate's :model/before from one pinned
    ;; same-node external snapshot. It does not submit at preparation.
    (let [prepared (try
                     (original-acquire node candidate-source)
                     (catch Exception _
                       (reject! :guarded-external-prestate-acquisition-failed)))
          underlying-summary (try ((:summary! prepared))
                                  (catch Exception _
                                    (reject! :invalid-acquired-gateway-summary)))
          _ (when-not (and (= acquired/acquired-type (:type prepared))
                           (= acquired/acquired-version (:version prepared))
                           (fn? (:submit! prepared))
                           (fn? (:summary! prepared))
                           (= :prepared (:status underlying-summary))
                           (= :same-node-pinned-read-matched-at-preparation
                              (:external-before underlying-summary))
                           (true? (:source-before-matched-external-read?
                                   underlying-summary))
                           (= :canonical-complete-top-level-cas
                              (:transaction-time-preconditions underlying-summary)))
              (reject! :unexpected-acquired-prestate-contract))
          _ (checked-correspondence! contract-source resolver
                                     selected-operation request candidate-source)
          attempted (AtomicBoolean. false)
          submit! (fn []
                    (when-not (.compareAndSet attempted false true)
                      (reject! :one-shot-already-attempted))
                    ;; Recheck the exact currently installed plan target, not
                    ;; just the expectation captured at issuance. This is a
                    ;; local revalidation, NOT a deployed-writer attestation.
                    (checked-correspondence! contract-source resolver
                                             selected-operation request
                                             candidate-source)
                    ((:submit! prepared)))
          summary! (fn []
                     (let [current ((:summary! prepared))]
                       {:type report-type
                        :version report-version
                        :status (if (.get attempted)
                                  :wrapper-permit-consumed :prepared)
                        :underlying-gateway-status (:status current)
                        :plan-target-correspondence
                        :exact-current-issued-callback-checked-at-preparation
                        :physical-command-target
                        :matches-issued-certified-plan-target
                        :external-before
                        :same-node-pinned-read-matched-at-preparation
                        :transaction-preconditions
                        :canonical-complete-top-level-cas
                        :pre-submit-plan-target-recheck :required
                        :physical-write-executed-at-preparation? false
                        :external-basis-authenticity :caller-node-unattested
                        :transaction-internal-read-basis :not-authenticated
                        :aba-history-detection :not-established
                        :physical-commit-causality :not-established
                        :principal-authorization :not-established
                        :complete-deployed-writer-coverage :not-established
                        :abstract-operation-refinement :not-established
                        :target-table-business-truth :application-premise
                        :proof-status :conditional-plan-bound-acquired-prestate
                        :certificate :not-issued
                        :admission :not-admitted
                        :transaction-authority :one-shot-submission-capability
                        :open-obligations
                        #{:application-literal-target-table-is-correct-and-authorized
                          :ring-route-selection-is-authentic
                          :supplied-node-is-the-intended-deployment-node
                          :transaction-assert-and-put-semantics-remain-valid
                          :no-unmediated-writers-guaranteed
                          :physical-commit-causality-not-proved}}))]
      (pinned!)
      {:type report-type
       :version report-version
       :submit! submit!
       :summary! summary!})))
