(ns gesso.assurance.proof.commit-unknown-reconciliation
  "v1398: Compare an uncertain controlled submission with a LATER, separately
   reported durable-command reconciliation and optional progression requirement.

   The caller supplies a prior v1394 observation envelope and a later result
   from the canonical gesso.model.command-resolution/reconcile shape. This
   function reruns v1396's structural diagnostic checks; it does NOT execute
   reconciliation, reread XTDB, submit, retry, or settle anything. Neither
   the prior observation nor the later report is authenticated by this API.

   A later :replay can report that a compatible command result was previously
   incorporated. It NEVER attributes the durable result to the earlier
   uncertain attempt. :unresolved is absence of a visible matching record at
   one observation point, NOT evidence of noncommit or permission to retry.
   A later reported progression requirement cannot settle a physical outcome.

   This is a REDACTED PROCESS-LOCAL DIAGNOSTIC, not certification, authority,
   a retry decision, or an executable recovery protocol. Keep the input
   envelopes private: they may contain bindings, documents, and exceptions."
  (:require [gesso.assurance.proof.transaction-outcome-progression-correspondence
             :as prior]
            [gesso.live.progression :as progression]
            [gesso.model.command-resolution :as resolution]))

(def report-type
  :gesso.assurance.proof.commit-unknown-reconciliation/report)
(def report-version 1)

(def ^:private original-classify prior/classify-observation)
(def ^:private original-reconcile resolution/reconcile)

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

(def ^:private unresolved-prior-statuses
  #{:execution-threw-outcome-unresolved
    :commit-unknown-reconciliation-required
    :commit-unknown-no-definitive-observation
    :physical-commit-observed-command-unresolved
    :physical-assertion-rejected-command-unresolved})

(defn- plain-map? [value]
  (and (or (instance? clojure.lang.PersistentArrayMap value)
           (instance? clojure.lang.PersistentHashMap value))
       (nil? (meta value))))

(defn- reject! [reason]
  ;; Never include input values, command IDs, or exception messages.
  (throw (ex-info "Invalid commit-unknown reconciliation diagnostic."
                  {:commit-unknown-reconciliation/rejection true
                   :reason reason})))

(defn- closed-map? [value ks]
  (and (plain-map? value) (= ks (set (keys value)))))

(defn- valid-later-resolution? [value]
  ;; Exact shapes returned by command-resolution/reconcile, but caller-supplied
  ;; here; a matching :scope is not authenticated read or disclosure provenance.
  (and (plain-map? value)
       (= resolution/assurance-scope (:scope value))
       (case (:status value)
         :replay (and (closed-map? value #{:status :scope :outcome})
                      (plain-map? (:outcome value))
                      (seq (:outcome value)))
         (:unresolved :unavailable :incompatible)
         (closed-map? value #{:status :scope})
         false)))

(defn- valid-prior? [report]
  (and (plain-map? report)
       (= prior/report-type (:type report))
       (= prior/report-version (:version report))
       (contains? unresolved-prior-statuses (:status report))
       (= :conditional-execution-progression-diagnostic-only
          (:proof-status report))
       (= :not-issued (:certificate report))
       (= :not-admitted (:admission report))
       (= :none (:authority report))
       (= :none (:transaction-authority report))
       (= :none (:live-settlement-authority report))
       (false? (:abstract-transition-state-pair-observed? report))
       (false? (:reported-basis-authenticated? report))
       (false? (:physical-outcome-authenticated? report))
       (false? (:commit-unknown-safe-to-retry? report))
       (false? (:business-rejection-established? report))
       (= physical-open (:physical-refinement-obligations report))
       (= :open (:physical-refinement-obligation-statuses report))
       (case (:status report)
         :execution-threw-outcome-unresolved
         (= :threw (:execution-outcome report))
         :commit-unknown-reconciliation-required
         (and (= :returned (:execution-outcome report))
              (= :unresolved (:durable-command-resolution-status report))
              (= :indeterminate (:observed-physical-attempt-status report)))
         :commit-unknown-no-definitive-observation
         (and (= :returned (:execution-outcome report))
              (= :unresolved (:durable-command-resolution-status report))
              (= :unobserved (:observed-physical-attempt-status report)))
         :physical-commit-observed-command-unresolved
         (and (= :returned (:execution-outcome report))
              (= :unresolved (:durable-command-resolution-status report))
              (= :committed (:observed-physical-attempt-status report)))
         :physical-assertion-rejected-command-unresolved
         (and (= :returned (:execution-outcome report))
              (= :unresolved (:durable-command-resolution-status report))
              (= :rejected (:observed-physical-attempt-status report)))
         false)))

(defn classify-later-reconciliation
  "Classify one prior UNCERTAIN controlled observation and one later reported
   canonical command reconciliation result. Input is exactly:

     {:prior-observation {:execution ... :diagnostic ...} ; v1394
      :later-resolution {:status :unresolved|:replay|:incompatible|:unavailable
                         :scope resolution/assurance-scope ...}
      :later-progression nil|<Gesso progression requirement>}

   Returns only bounded classifications, never raw values or inferred retry,
   browser settlement, or transaction causality. A later :replay describes a
   compatible previously durable record (if the supplied read is genuine),
   NOT which submission created it. This function does not invoke callbacks."
  [request]
  (when-not (closed-map? request
                         #{:prior-observation :later-resolution
                           :later-progression})
    (reject! :invalid-request-shape))
  (when-not (and (identical? original-classify
                             (var-get #'prior/classify-observation))
                 (identical? original-reconcile
                             (var-get #'resolution/reconcile)))
    (reject! :substituted-reconciliation-checker))
  (let [prior-report (try
                       (original-classify (:prior-observation request))
                       (catch clojure.lang.ExceptionInfo _
                         (reject! :invalid-prior-observation)))
        later (:later-resolution request)
        later-progression (:later-progression request)]
    (when-not (valid-prior? prior-report)
      (reject! :not-an-uncertain-controlled-outcome))
    (when-not (valid-later-resolution? later)
      (reject! :invalid-later-resolution))
    (when-not (or (nil? later-progression)
                  (progression/requirement? later-progression))
      (reject! :invalid-later-progression))
    {:type report-type
     :version report-version
     :status (case (:status later)
               :replay :later-replay-reported-prior-attempt-unattributed
               :unresolved :later-command-record-not-visible-noncommit-unproved
               :incompatible :later-binding-incompatible-no-disclosure
               :unavailable :later-record-disclosure-unavailable)
     :prior-uncertain-status (:status prior-report)
     :prior-observed-physical-attempt-status
     (:observed-physical-attempt-status prior-report)
     :later-reported-resolution-status (:status later)
     :later-reported-progression-status
     (if (nil? later-progression)
       :not-reported
       :reported-unattributed-and-unauthenticated)
     :later-command-incorporation-reported? (= :replay (:status later))
     :later-reconciliation-origin :caller-supplied-unattested
     :prior-attempt-commit-attributed? false
     :later-record-absence-proves-noncommit? false
     :same-command-correspondence-authenticated? false
     :reported-later-progression-authenticated? false
     :business-rejection-established? false
     :safe-to-retry? false
     :safe-to-settle? false
     :automatic-retry :forbidden-by-this-diagnostic
     :live-settlement-authority :none
     :transaction-authority :none
     :physical-refinement-obligations physical-open
     :physical-refinement-obligation-statuses :open
     :proof-status :later-reconciliation-correlation-diagnostic-only
     :certificate :not-issued
     :admission :not-admitted
     :authority :none}))
