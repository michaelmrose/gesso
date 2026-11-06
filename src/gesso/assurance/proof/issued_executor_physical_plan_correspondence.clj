(ns gesso.assurance.proof.issued-executor-physical-plan-correspondence
  "v1471: One-shot, source-rechecked v1251 executor-to-XTDB-adapter plan diagnostic.

   The boundary freshly issues a v1253 handler for the ORIGINAL checked v1251
   executor, calls it ONCE, captures the actual resolved-operation :prepared and
   :physical-outcome events, and independently captures XTDB adapter call-site
   operations in that SAME synchronous invocation. Only tx/prepare applied to
   the captured canonical plan can supply the expected physical operations.
   A complete, ordered, identical pair is bounded process-local evidence that
   THIS issued executor's prepared plan was passed to THIS adapter call site.

   A poisoned/missing event, extra adapter call, or divergent payload cannot
   yield positive agreement, even if the handler returns normally. A swallowed
   observer exception is recorded as invalid. An uncertain/throwing invocation
   is never retried. The private live handle rechecks executor, source and
   request identities; copyable reports have NO authority.

   Critically: this is a FRESH diagnostic handler, NOT the deployed router's
   installed callable. The adapter's bound XTDB function, callbacks, physical
   node, actual commit receipt, transaction-internal prestate, and all other
   writers are unverified. It does NOT join a v1469 controlled collector run
   to the same physical execution. Neither this module nor its handle certifies
   a deployment or permits execution of another transaction."
  (:require [gesso.live.application-preflight.certified-contract :as issued]
            [gesso.live.consistency.xtdb :as adapter]
            [gesso.model.certified-plan :as plan]
            [gesso.model.resolved-operation :as resolved]
            [gesso.model.tx :as tx])
  (:import (java.time Instant)
           (java.util WeakHashMap)
           (xtdb.api TransactionKey)))

(def correspondence-type
  :gesso.assurance.proof.issued-executor-physical-plan-correspondence/local)
(def correspondence-version 1)
(deftype IssuedSubmissionHandle []
  Object
  (toString [_] "#<gesso.assurance/issued-adapter-plan-witness>"))

(def ^:private ^WeakHashMap live-witnesses (WeakHashMap.))
(def ^:dynamic ^:private *inside-observation* false)
(def ^:private original-check plan/require-current-plan-operation!)
(def ^:private original-describe plan/describe-plan-operation)
(def ^:private original-issue issued/issue-certified-handler!)
(def ^:private original-capture adapter/capture-tx-api-calls!)
(def ^:private original-prepare tx/prepare)

(def ^:private source-keys
  #{:executor :current-source :request :request->input! :result->response!})
(def ^:private open-obligations
  #{:actual-deployed-route-uses-this-issued-handler
    :callback-honesty-and-observation-hook-integrity
    :original-xtdb-api-and-adapter-binding-authenticity
    :actual-database-node-and-committed-transaction-receipt
    :atomic-transaction-internal-prestate-and-guards
    :actual-principal-inputs-and-authorization
    :complete-physical-writer-inventory
    :v1469-controlled-collector-same-execution-correlation
    :universal-xtdb-execution-refinement})

(defn- reject! [reason]
  ;; In particular, never emit the request, source, plan, target, or tx-ops.
  (throw (ex-info "Invalid issued-executor physical-plan observation."
                  {:issued-executor-physical-plan/rejection true
                   :reason reason})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- checked-source! [source]
  (when-not (and (plain-map? source)
                 (= source-keys (set (keys source)))
                 (plain-map? (:request source))
                 (not (contains? (:request source)
                                 resolved/observation-context-key))
                 (fn? (:request->input! source))
                 (fn? (:result->response! source)))
    (reject! :invalid-source-or-preexisting-observer))
  (when-not (and (identical? original-check
                             (var-get #'plan/require-current-plan-operation!))
                 (identical? original-describe
                             (var-get #'plan/describe-plan-operation))
                 (identical? original-issue
                             (var-get #'issued/issue-certified-handler!))
                 (identical? original-capture
                             (var-get #'adapter/capture-tx-api-calls!))
                 (identical? original-prepare
                             (var-get #'tx/prepare)))
    (reject! :substituted-execution-boundary))
  (original-check (:executor source) (:current-source source))
  (original-describe (:executor source)))

(defn- resolved-pair [events operation]
  (when (and (vector? events) (= 2 (count events)))
    (let [[prepared outcome] events
          common (fn [event]
                   (and (plain-map? event)
                        (= resolved/execution-observation-scope (:scope event))
                        (= operation (:operation event))
                        (uuid? (:attempt-id event))))]
      (when (and (common prepared) (common outcome)
                 (= :prepared (:phase prepared))
                 (= :physical-outcome (:phase outcome))
                 (= (:attempt-id prepared) (:attempt-id outcome))
                 (= (select-keys prepared [:operation :attempt-id :binding
                                            :record :plan])
                    (select-keys outcome [:operation :attempt-id :binding
                                           :record :plan]))
                 (map? (:plan prepared))
                 (#{:committed :rejected :indeterminate}
                  (:physical/status outcome)))
        [prepared outcome]))))

(defn- adapter-pair [capture]
  (let [events (:events capture)]
    (when (and (= adapter/submission-capture-type (:type capture))
               (= adapter/submission-capture-version (:version capture))
               (= :instrumented-xtdb-adapter-call-site
                  (:observation-origin capture))
               (false? (:actual-xtdb-api-authenticated? capture))
               (vector? events) (= 2 (count events)))
      (let [[begun ended] events]
        (when (and (plain-map? begun) (plain-map? ended)
                   (= :invoked (:phase begun))
                   (#{:returned :threw} (:phase ended))
                   (#{:submit-tx :execute-tx} (:api begun))
                   (= (:api begun) (:api ended))
                   (uuid? (:call-id begun))
                   (= (:call-id begun) (:call-id ended))
                   (vector? (:tx-ops begun))
                   (plain-map? (:tx-options begun)))
          [begun ended])))))

(defn- classified [operation events invalid? capture request]
  (let [resolved (when-not invalid? (resolved-pair events operation))
        adapter (adapter-pair capture)
        [prepared finished] resolved
        [begun ended] adapter
        expected (when resolved
                   (try (original-prepare request (:plan prepared))
                        (catch Throwable _ nil)))
        ops-agree? (and (vector? (:tx-ops expected)) adapter
                        (= (:tx-ops expected) (:tx-ops begun)))
        options-agree? (and expected adapter
                            (= (or (get-in expected [:plan :tx-options]) {})
                               (:tx-options begun)))
        definitive-key? (and (= :execute-tx (:api begun))
                             (= :returned (:phase ended))
                             (instance? TransactionKey (:tx-result ended))
                             (let [^TransactionKey key (:tx-result ended)]
                               (and (integer? (.getTxId key))
                                    (<= 0 (.getTxId key))
                                    (instance? Instant (.getSystemTime key)))))
        paired? (and resolved adapter ops-agree? options-agree?
                     definitive-key?
                     (= :returned (:outcome capture))
                     (= :committed (:physical/status finished)))]
    {:type correspondence-type
     :version correspondence-version
     :operation-id operation
     :status (cond
               invalid? :observer-poisoned
               (not resolved) :missing-or-invalid-resolved-executor-step
               (not adapter) :missing-or-invalid-adapter-call
               (not ops-agree?) :prepared-plan-diverges-from-adapter-operations
               (not options-agree?) :prepared-options-diverge-from-adapter-options
               (= :submit-tx (:api begun)) :submit-only-not-definitive-commit
               (not definitive-key?) :no-definitive-execute-key
               (not paired?) :execution-outcome-not-established
               :else :bounded-issued-executor-to-adapter-plan-agreement)
     :actual-issued-executor-checked? true
     :resolved-step-observed? (boolean resolved)
     :one-adapter-call-observed? (boolean adapter)
     :prepared-operations-agree? (boolean ops-agree?)
     :submitted-options-agree? (boolean options-agree?)
     :reported-commit-and-execute-key-agree? (boolean paired?)
     :adapter-api (when adapter (:api begun))
     :deployed-handler-provenance :not-established
     :physical-xtdb-receipt-authenticated? false
     :physical-collector-same-execution :not-established
     :complete-writer-coverage :not-established
     :open-obligations open-obligations
     :scope :one-process-local-fresh-issued-handler-invocation
     :proof-status :source-rechecked-conditional-adapter-diagnostic-only
     :certificate :not-issued
     :admission :not-admitted
     :transaction-authority :none
     :authority :none}))

(defn- entry [handle]
  (when (instance? IssuedSubmissionHandle handle)
    (locking live-witnesses (.get live-witnesses handle))))

(defn issued-submission-witness? [handle]
  "An in-process origin handle, NOT a certificate or transaction permission."
  (boolean (entry handle)))

(defn observe-issued-submission!
  "Issue a fresh checked v1253 handler and invoke it ONCE inside the v1390
   adapter capture. Returns {:execution {:outcome ... :result/:error ...}
   :handle <opaque handle> :report <redacted diagnostic>}.

   EXECUTION AND ERROR VALUES CAN CONTAIN PRIVATE APPLICATION DATA. Do not
   publish or log them. A throwing handler is not retried, and cannot issue a
   successful witness; its original exception remains available in :execution."
  [source]
  (when *inside-observation*
    (reject! :nested-issued-executor-observation))
  (let [description (checked-source! source)
        {:keys [executor current-source request request->input! result->response!]}
        source
        observed (atom [])
        poisoned (atom false)
        observer (fn [event]
                   (try (swap! observed conj event)
                        (catch Throwable _ (reset! poisoned true))))
        ;; The hook is never handed to application-owned input/response
        ;; callbacks. Opaque executor callbacks remain an open trust boundary.
        callback-view (fn [ctx] (dissoc ctx resolved/observation-context-key))
        handler (original-issue
                 {:executor executor :current-source current-source
                  :request->input! (fn [ctx] (request->input! (callback-view ctx)))
                  :result->response! (fn [ctx result]
                                       (result->response! (callback-view ctx)
                                                          result))})
        observed-request (assoc request resolved/observation-context-key observer)
        captured (binding [*inside-observation* true]
                   (original-capture #(handler observed-request)))
        report (classified (:operation description) @observed @poisoned
                           captured observed-request)
        ;; An incomplete trace is NOT a successful witness. The execution
        ;; remains available for diagnosis, without re-executing the handler.
        good? (= :bounded-issued-executor-to-adapter-plan-agreement
                 (:status report))
        handle (when good? (IssuedSubmissionHandle.))]
    (when handle
      (locking live-witnesses
        (.put live-witnesses handle
              {:executor executor :current-source current-source
               :request request
               :request->input! request->input!
               :result->response! result->response!
               :report report})))
    {:execution (select-keys captured [:outcome :result :error])
     :handle handle :report report}))

(defn require-current-issued-submission!
  "Recheck a registered handle against the original executor, exact request
   object, original source and report. Does not invoke handler or XTDB again."
  [handle source prior-report]
  (let [witness (or (entry handle) (reject! :unknown-or-forged-witness))
        description (checked-source! source)]
    (when-not (identical? (:executor witness) (:executor source))
      (reject! :executor-substitution))
    (when-not (identical? (:request witness) (:request source))
      (reject! :request-substitution))
    (when-not (and (= (:current-source witness) (:current-source source))
                   (identical? (:request->input! witness)
                               (:request->input! source))
                   (identical? (:result->response! witness)
                               (:result->response! source)))
      (reject! :source-substitution))
    (when-not (and (plain-map? prior-report)
                   (= prior-report (:report witness))
                   (= (:operation description) (:operation-id prior-report)))
      (reject! :altered-or-stale-report))
    (:report witness)))
