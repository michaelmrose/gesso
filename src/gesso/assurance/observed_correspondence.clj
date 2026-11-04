(ns gesso.assurance.observed-correspondence
  "v1302: join a CURRENT source-bound Gesso application-induction assembly
   with ONE bounded v1300 observed abstract/physical transition comparison.

   Both sibling checks are independently rebuilt from their ORIGINAL sources.
   The observed operation must have the SAME typed guarded construction and
   field mapping as the exact induction-bound executor installed behind its
   issued Ring handler. Merely matching an operation name is insufficient.

   This is a DIAGNOSTIC join, not authenticated physical evidence. A copied or
   fabricated but well-formed v1298 witness can still compare favorably. No
   claim about actual submission identity, deployed-router adoption, complete
   physical writers, transaction-internal prestate, or universal XTDB
   refinement follows. The result is copyable explanation only; no proof
   certificate or transaction permission is issued."
  (:require [clojure.set :as set]
            [gesso.assurance.application-induction :as application]
            [gesso.model.transition-conformance :as conformance]))

(def report-version 1)
(def report-type :gesso.assurance.observed-correspondence/application-step-report)
(def ^:private source-keys #{:application-source :transition-source})
(def ^:private comparison-source-keys
  #{:observation :construction :field-map :inputs :target :command-operation})
(def ^:private checked-fields
  #{:declared-prestate :complete-physical-domain-projection
    :abstract-guard :abstract-assignments :abstract-frame
    :canonical-serial-commit :model-poststate :command-incorporation})
(def ^:private additional-open
  #{:physical-observation-origin-is-unauthenticated
    :observed-transaction-payload-provenance-is-unauthenticated
    :actual-submitted-inputs-and-plan-are-not-established
    :observation-window-and-xtdb-ledger-are-not-independently-authenticated
    :single-observation-does-not-establish-universal-refinement
    :all-physical-writers-are-not-established
    :transaction-internal-prestate-is-not-established
    :production-router-and-executor-adoption-is-not-established})

(defn- reject! [reason at]
  (throw (ex-info "Invalid application/observed-transition correspondence."
                  {:error/type ::invalid-observed-correspondence
                   :reason reason :at at})))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- exact! [x expected at]
  (when-not (and (plain-map? x) (nil? (meta x))
                 (= expected (set (keys x))))
    (reject! :invalid-source-shape at))
  x)

;; Sibling reports are never a caller-provided authority. Reject replacement
;; of their public Vars instead of accepting a forged success description.
;; As in v1294, hostile mutation of private JVM state is outside this boundary.
(def ^:private original-application-check
  application/check-application-induction!)
(def ^:private original-transition-check
  conformance/check-observed-transition!)

(defn- require-sibling-checkers! []
  (when-not (and (identical? original-application-check
                             (var-get #'application/check-application-induction!))
                 (identical? original-transition-check
                             (var-get #'conformance/check-observed-transition!)))
    (reject! :substituted-upstream-checker [])))

(defn- checked-application! [source]
  (let [r (original-application-check source)]
    (when-not (and (= application/report-type (:type r))
                   (= application/report-version (:version r))
                   (= :fresh-induction-route-coverage-and-issued-plans-rechecked
                      (:checking r))
                   (= :exact-issued-executor-and-abstract-construction
                      (:plan-correspondence r))
                   (= :source-bound-conditionally-inductive
                      (:abstract-invariant r))
                   (= (:abstract-operation-ids r)
                      (:issued-inductive-plan-ids r))
                   (= :all-original-obligations-remain-open
                      (:original-obligation-status r))
                   (= :not-established
                      (:abstract-to-physical-transition-refinement r))
                   (= :not-established (:physical-write-path-completeness r))
                   (= :not-established (:production-certification r))
                   (= :none (:transaction-authority r))
                   (= :none (:authority r)))
      (reject! :invalid-application-assurance [:application-source]))
    r))

(defn- checked-observation! [source]
  (let [r (original-transition-check source)]
    (when-not (and (= conformance/report-type (:type r))
                   (= conformance/conformance-version (:version r))
                   (= :diagnostic-bounded-observed-transition-comparison
                      (:evidence-class r))
                   (= :source-derived-model-and-typed-abstract-relation
                      (:checking r))
                   (= checked-fields (set (keys (:checks r))))
                   (every? boolean? (vals (:checks r)))
                   (= (:corresponds? r) (every? true? (vals (:checks r))))
                   (= :caller-supplied-copyable-v1298-report
                      (:observation-provenance r))
                   (= :not-established (:general-refinement r))
                   (= :none (:certification r))
                   (= :none (:transaction-authority r))
                   (= conformance/comparison-scope (:scope r)))
      (reject! :invalid-physical-comparison [:transition-source]))
    r))

(defn check-observed-correspondence!
  "Recheck one source-owned application and one supplied physical observation.

   Required input:
     {:application-source <exact v1294 application source>
      :transition-source <exact v1300 comparison source>}

   Reject missing or mismatched application operation / plan constructions.
   A well-formed but physically divergent execution is REPORTED as a
   diagnostic mismatch, not silently accepted or upgraded to a proof."
  [source]
  (exact! source source-keys [])
  (let [{:keys [application-source transition-source]} source]
    (exact! transition-source comparison-source-keys [:transition-source])
    (require-sibling-checkers!)
    (let [app (checked-application! application-source)
          operation (get-in transition-source [:construction :subject :id])
          declared (:abstract-operation-ids app)
          row (get-in app [:checked-inductive-plans operation])
          plan-source (get-in application-source
                              [:inductive-plans operation :current-source
                               :certified-plan-source])]
      (when-not (and (contains? declared operation)
                     (contains? (:issued-inductive-plan-ids app) operation)
                     (contains? (:semantic-route-ids app) operation)
                     (plain-map? row)
                     (plain-map? plan-source))
        (reject! :operation-not-bound-to-current-application
                 [:transition-source :construction :subject]))
      ;; First reject source mismatch, before expensive physical checking.
      ;; v1294 has already checked the plan against both the certificate and
      ;; the exact issued executor behind the Ring handler.
      (when-not (and (= (:construction transition-source)
                        (:construction plan-source))
                     (= (:field-map transition-source)
                        (:field-map plan-source))
                     (= (:command-operation transition-source)
                        (:command-operation plan-source)))
        (reject! :observation-not-bound-to-issued-plan
                 [:transition-source]))
      ;; Only now compare the physical observation with the original typed
      ;; construction. It must independently rederive its exact fingerprints.
      (let [observed (checked-observation! transition-source)
            _ (when-not (and (= operation (:operation observed))
                             (= (:transition-fingerprint row)
                                (:transition-fingerprint observed))
                             (= (:subject-fingerprint row)
                                (:subject-fingerprint observed)))
                (reject! :observed-step-fingerprint-mismatch
                         [:transition-source]))
            open (set/union additional-open
                            (:open-obligations app)
                            (:assumed conformance/comparison-scope)
                            (:not-established conformance/comparison-scope))]
        {:type report-type
         :version report-version
         :checking :current-application-and-declared-physical-step-rechecked
         :scope :one-supplied-unauthenticated-physical-observation
         :evidence-class :non-authoritative-observed-application-step-diagnostic
         :operation operation
         :catalog-fingerprint (:catalog-fingerprint app)
         :induction-fingerprint (:induction-fingerprint app)
         :certificate-fingerprint (:certificate-fingerprint app)
         :dependency-fingerprint (:dependency-fingerprint app)
         :transition-fingerprint (:transition-fingerprint observed)
         :subject-fingerprint (:subject-fingerprint observed)
         :physical-tx-id (:physical-tx-id observed)
         :observed-target-count (:observed-target-count observed)
         :observed-checks (:checks observed)
         :diagnostic-correspondence (if (:corresponds? observed)
                                      :corresponds-to-declared-step
                                      :diverges-from-declared-step)
         :diagnostic-conforms? (:corresponds? observed)
         :physical-observation-provenance :caller-supplied-unauthenticated
         :plan-correspondence :exact-source-and-issued-executor-rechecked
         :declared-abstract-operation-ids declared
         :observed-application-operation-ids #{operation}
         :unobserved-declared-operation-ids (disj declared operation)
         :unclassified-entry-point-ids
         (:observed-unclassified-entry-point-ids app)
         :original-open-obligation-ids-by-operation
         (:original-open-obligation-ids-by-operation app)
         :original-obligation-status :all-original-obligations-remain-open
         :open-premises (:open-premises app)
         :open-obligations open
         :physical-write-path-completeness :not-established
         :authenticated-physical-correspondence :not-established
         :universal-xtdb-refinement :not-established
         :application-invariant-physical-preservation :not-established
         :deployed-router-adoption :not-established
         :independent-checker-soundness :not-established
         :production-certification :not-established
         :transaction-authority :none
         :authority :none}))))

(defn require-current-observed-correspondence!
  "Compare a COPYABLE prior report with a fresh recheck of its original sources.
   This is freshness of a diagnostic description, not physical authentication."
  [prior current-source]
  (let [current (check-observed-correspondence! current-source)]
    (when-not (and (plain-map? prior) (nil? (meta prior))
                   (= prior current))
      (reject! :stale-or-forged-diagnostic-report []))
    current))
