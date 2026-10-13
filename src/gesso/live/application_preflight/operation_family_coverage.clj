(ns gesso.live.application-preflight.operation-family-coverage
  "Source-derived operation-ID coverage for the finite operation-family checker.

   Given a CURRENT ApplicationAssembly, this bridge obtains the actual
   route-exposed operation registry through canonical whole-application
   preflight. It requires the supplied finite families to identify EXACTLY
   those registered operations before running state-dependent checking.

   This proves only that the declarations cover the checked registry's
   OPERATION IDENTIFIERS. The family callbacks, principal and argument
   domains, initial state, and invariants remain independently supplied and
   trusted. In particular, equal names do not establish that :enabled? and
   :prepare implement the registered Gesso Live :execute! functions, nor that
   the modeled registry includes raw/bypassed application entry points.

   Full registry matching deliberately fails when the finite six-instance
   model bound cannot accommodate an application. There is no implicit
   subset, sampling, truncation, or success-by-exclusion mode.

   This diagnostic reads the current application artifact as part of
   preflight, but never invokes authoritative executors or submits database
   transactions. Its verdict is NOT a deployable application certificate."
  (:require [clojure.set :as set]
            [gesso.live.application-preflight :as application]
            [gesso.model.operation-family :as family]))

(def assurance-scope
  {:checker :gesso.live.application-preflight.operation-family-coverage/registered-ids-v1
   :checked #{:fresh-current-application-assembly
              :source-derived-route-exposed-operation-ids
              :exact-declared-family-to-registry-identity-coverage
              :complete-declared-finite-principal-argument-expansion
              :bounded-state-dependent-invariant-exploration}
   :assumed #{:truthful-operation-family-callbacks
              :truthful-operation-enablement
              :representative-complete-principal-and-argument-domains
              :complete-modeled-initial-state
              :truthful-pure-total-invariant-predicates
              :serial-atomic-commit-semantics}
   :not-established #{:correspondence-of-modeled-families-to-registered-executors
                      :completeness-of-runtime-entry-points-outside-checked-registry
                      :business-policy-guard-completeness
                      :authorization-of-actual-principals
                      :truth-of-developer-authored-invariants
                      :real-xtdb-execution-correspondence
                      :unbounded-invariant-preservation
                      :whole-application-certification}})

(defn- fail! [kind details]
  (throw (ex-info
          "Finite operation families must exactly cover a current application's registered operation identifiers."
          {:error/type ::invalid-registry-coverage
           :error/kind kind
           :error/details details})))

(defn check-registered-families
  "Run finite family checking only after exact comparison with the operation
   IDs obtained from a freshly rechecked ApplicationAssembly.

   Arguments: (application-assembly initial-state families rules options).
   Returns the ordinary family/check-families verdict plus :registry-coverage
   and :registry-scope. The coverage report intentionally says nothing about
   the fidelity of the supplied callbacks to the registered operation code.

   Both omitted registered operations and invented family identifiers FAIL
   CLOSED before any family callback runs. The same six-instance expansion
   ceiling and reachability budgets as operation-family/check-families apply.
   An empty application registry is rejected rather than called covered.

   A passing result establishes only identifier coverage and bounded checking
   of the user-supplied abstract operation implementations. No callbacks are
   extracted from Gesso Live's registered executors and no business policies
   are inferred from arbitrary Clojure."
  [assembly initial-state families rules options]
  ;; Do not accept a plausible report map in lieu of an actual current
  ;; ApplicationAssembly. `application/explain` independently rederives the
  ;; report, including the physical browser-artifact receipt.
  (when-not (and (map? assembly)
                 (= application/application-assembly-type
                    (:gesso.live.application-preflight/type assembly)))
    (fail! :application-assembly-required
           {:expected application/application-assembly-type}))
  (let [description (application/explain assembly)
        registered (set (keys (:operations description)))
        ;; Validate the whole family declaration *before* looking at callbacks;
        ;; expansion is pure and never evaluates :enabled? or :prepare.
        expanded (family/expand-families families)
        declared (set (map :id families))
        missing (set/difference registered declared)
        foreign (set/difference declared registered)]
    (when (empty? registered)
      (fail! :empty-application-operation-registry
             {:registered-operation-count 0}))
    (when (or (seq missing) (seq foreign))
      (fail! :operation-identity-coverage-mismatch
             {:registered-operation-ids registered
              :declared-family-ids declared
              :missing-registered-operations missing
              :unregistered-family-ids foreign}))
    (let [verdict (family/check-families
                   initial-state families rules options)]
      (assoc verdict
             :registry-coverage
             {:status :exact-registered-operation-id-coverage
              :source :current-application-assembly-operation-summary
              :registered-operation-ids
              (vec (sort-by str registered))
              :declared-family-ids
              (vec (sort-by str declared))
              :expanded-instance-count
              (count (:choices expanded))
              :family-to-executor-correspondence :not-established
              :domain-completeness :not-established}
             :registry-scope assurance-scope))))
