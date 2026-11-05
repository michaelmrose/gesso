(ns gesso.assurance.proof.classified-physical-acquisition
  "v1394: Acquire and classify one nested executor/adapter/ledger diagnostic.

   A controlled typed, guard-corresponded Gesso submit is executed once inside
   the v1392 adapter API-call capture. The enclosing capture compares the
   public XTDB API result with the committed resolution's tx-result and, when
   supported, checks the reported key against a later pinned xt.txs read.
   Both are acquired in ONE process-local invocation, not joined from two
   caller-supplied reports. No retry or alternate transaction path is added.

   These observations are NOT authenticated. Registered application callbacks,
   reader-to-physical-check links, the observation node, bound XTDB functions,
   and other writers remain trusted/unverified. Even a matching ledger entry
   does not establish that the intended payload reached XTDB or caused it.
   An unresolved or rejected operation is never promoted into commit.

   :execution contains the original controlled result or throwable and may
   contain sensitive values. Never return/log it or the source as HTTP data.
   :diagnostic is a bounded, redacted classification, NOT a certificate."
  (:require [gesso.assurance.proof.executed-guard-authorization-correspondence
             :as guard]
            [gesso.assurance.proof.executor-controlled-acquisition :as controlled]
            [gesso.assurance.proof.xtdb-submission-outcome-correlation :as submission]))

(def report-type :gesso.assurance.proof.classified-physical-acquisition/report)
(def report-version 1)

(def ^:private source-keys #{:guard-source :observation-node})
(def ^:private original-guard guard/run-guard-corresponded-typed-submit!)
(def ^:private original-submission submission/observe-one-submission!)

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

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentHashMap x)
           (instance? clojure.lang.PersistentArrayMap x))
       (nil? (meta x))))

(defn- reject! [reason]
  (throw (ex-info "Invalid classified physical acquisition request."
                  {:classified-physical-acquisition/rejection true
                   :reason reason})))

(defn- validate-source! [source]
  (when-not (and (plain-map? source)
                 (= source-keys (set (keys source)))
                 (plain-map? (:guard-source source))
                 (some? (:observation-node source)))
    (reject! :invalid-acquisition-source))
  (when-not (and (identical? original-guard
                             (var-get #'guard/run-guard-corresponded-typed-submit!))
                 (identical? original-submission
                             (var-get #'submission/observe-one-submission!)))
    (reject! :substituted-acquisition-entry-point))
  source)

(defn- controlled-observation [guard-result]
  (let [controlled-result (:controlled-result guard-result)
        acquisition (:acquisition-report controlled-result)
        typed (:typed-input-observation controlled-result)
        guard-check (:guard-observation guard-result)
        valid-shape? (and (plain-map? guard-result)
                          (= guard/guard-report-type (:type guard-result))
                          (= guard/guard-report-version (:version guard-result))
                          (= :guard-plan-correlation-diagnostic-only
                             (:proof-status guard-result))
                          (= :not-issued (:certificate guard-result))
                          (= :none (:authority guard-result))
                          (plain-map? controlled-result)
                          (= controlled/boundary-type (:type controlled-result))
                          (= controlled/boundary-version (:version controlled-result))
                          (= :executor-controlled-hook-diagnostic-only
                             (:proof-status controlled-result))
                          (= :not-issued (:certificate controlled-result))
                          (= :none (:authority controlled-result))
                          (= (:operation-id guard-result)
                             (:operation-id controlled-result))
                          (uuid? (:attempt-id guard-result))
                          (= (:attempt-id guard-result) (:attempt-id acquisition))
                          (= :complete-paired-hook-observation
                             (:capture-status controlled-result))
                          (false? (:observer-origin-authenticated? controlled-result))
                          (false? (:physical-submission-authenticated? controlled-result))
                          (= :not-established
                             (:actual-submitted-payload-provenance acquisition))
                          (= :none (:transaction-authority guard-result))
                          (every? #(= :open (:status %))
                                  (:physical-refinement-obligations guard-result)))
        typed-match? (= :matched (:construction-site-correlation typed))
        guards-match? (true? (:linked-guard-plan-agreement? guard-check))]
    {:shape-consistent? (boolean valid-shape?)
     :status (cond
               (not valid-shape?) :missing-or-inconsistent-controlled-observation
               (not typed-match?) :typed-input-observation-diverged
               (not guards-match?) :guard-observation-diverged
               :else :process-local-executor-observations-agree)
     :typed-input-status (or (:construction-site-correlation typed) :missing)
     :guard-plan-status (or (:status guard-check) :missing)
     :physical-outcome (or (:captured-physical-status acquisition) :unobserved)
     :attempt-observed? (boolean (uuid? (:attempt-id acquisition)))}))

(defn- classified-diagnostic [guard-result submission-report execution-outcome]
  (let [controlled (controlled-observation guard-result)
        submission-valid?
        (and (plain-map? submission-report)
             (= submission/report-type (:type submission-report))
             (= submission/report-version (:version submission-report))
             (= :diagnostic-only (:proof-status submission-report))
             (= :not-issued (:certificate submission-report))
             (= :not-admitted (:admission submission-report))
             (= :none (:authority submission-report))
             (false? (:actual-xtdb-api-authenticated? submission-report))
             (= physical-open (:open-obligations submission-report)))
        adapter-status (if submission-valid?
                         (:status submission-report)
                         :invalid-or-missing-submission-diagnostic)
        ledger-status (if submission-valid?
                        (get-in submission-report [:ledger-observation :status])
                        :not-verified)
        executor-and-ledger?
        (and (= :returned execution-outcome)
             (:shape-consistent? controlled)
             (= :process-local-executor-observations-agree (:status controlled))
             (= :committed (:physical-outcome controlled))
             submission-valid?
             (= :execute-tx (:api submission-report))
             (= :returned (:api-completion submission-report))
             (= :reported-execute-ledger-agrees adapter-status)
             (= :reported-key-matches-committed-ledger-row ledger-status))
        status (cond
                 (not= :returned execution-outcome) :executor-threw-or-incomplete
                 (not (:shape-consistent? controlled)) :invalid-controlled-observation
                 (not submission-valid?) :invalid-submission-observation
                 (not (= :process-local-executor-observations-agree
                         (:status controlled))) :controlled-observation-diverged
                 (not= :committed (:physical-outcome controlled))
                 :no-confirmed-controlled-commit
                 (= :submission-id-only adapter-status)
                 :adapter-reported-id-only
                 executor-and-ledger? :bounded-coobserved-metadata-agreement
                 :else :incomplete-or-divergent-submission-correlation)]
    {:type report-type
     :version report-version
     :status status
     :execution-outcome execution-outcome
     :executor-observation controlled
     :adapter-api-status adapter-status
     :ledger-observation-status ledger-status
     :bounded-executor-adapter-ledger-agreement? (boolean executor-and-ledger?)
     :classifications
     {:executor {:origin :controlled-process-local-resolved-operation
                 :trust :process-local-unattested
                 :status (:status controlled)}
      :typed-input {:origin :trusted-invocation-and-construction-hook
                    :trust :application-path-mapping-unattested
                    :status (:typed-input-status controlled)}
      :authorization-guard {:origin :registered-callback-and-admitted-guard-links
                            :trust :application-reader-links-unattested
                            :status (:guard-plan-status controlled)}
      :xtdb-api {:origin :instrumented-adapter-call-site
                 :trust :process-local-bound-function-unattested
                 :status adapter-status}
      :transaction-ledger {:origin :pinned-query-on-caller-provided-node
                           :trust :later-node-read-not-payload-attestation
                           :status ledger-status}}
     :one-process-local-invocation? true
     :actual-payload-accepted-by-xtdb? :not-established
     :transaction-internal-prestate :not-established
     :causal-attribution-to-executor-attempt :not-established
     :authenticated-request-or-authorization :not-established
     :other-physical-writers-excluded? :not-established
     :deployed-handler-or-route-identity :not-established
     :all-physical-refinement-obligations physical-open
     :all-physical-refinement-obligation-statuses :open
     :proof-status :classified-coacquisition-diagnostic-only
     :certificate :not-issued
     :admission :not-admitted
     :transaction-authority :none
     :authority :none}))

(defn run-classified-acquisition!
  "Run one controlled typed+guarded submit within the v1392 adapter/ledger
   acquisition. `source` must contain exactly :guard-source (the v1388 source)
   and :observation-node. The adapter capture is dynamically scoped to this
   one synchronous invocation. No second executor submission or reconciliation
   is initiated here. Async writes or direct XTDB calls bypassing the adapter
   are not observed.

   Returns {:execution {:outcome :returned :result <original v1388 result>}
                        | {:outcome :threw :error <original throwable>}
            :diagnostic <redacted classification>}. Callers must preserve the
   original result/exception semantics; neither execution nor source is safe
   to log. A matching status is NEVER a physical-refinement certificate."
  [source ctx]
  (validate-source! source)
  (let [result (volatile! nil)
        observed (original-submission
                  (:observation-node source)
                  (fn []
                    (let [controlled-result
                          (original-guard (:guard-source source) ctx)]
                      (vreset! result controlled-result)
                      ;; v1392 requires the outer :tx-result to equal the
                      ;; adapter's actual returned TransactionKey. Do not
                      ;; fabricate a key for rejected or unknown outcomes.
                      {:tx-result
                       (get-in controlled-result
                               [:controlled-result :resolution
                                :transaction :tx-result])})))
        outer (:execution observed)
        execution (if (= :threw (:outcome outer))
                    {:outcome :threw :error (:error outer)}
                    {:outcome :returned :result @result})]
    {:execution execution
     :diagnostic (classified-diagnostic @result (:diagnostic observed)
                                        (:outcome execution))}))
