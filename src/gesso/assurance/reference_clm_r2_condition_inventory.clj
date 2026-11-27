(ns gesso.assurance.reference-clm-r2-condition-inventory
  "Review the exact remaining general CLM-R2 conditions. An evidence
   route is neither an accepted trust premise nor a discharge."
  (:require [clojure.set :as set]
            [gesso.assurance.reference-clm-r2-binding :as binding]))

(def profile :open-clm-r2-condition-inventory-v1)
(def ^:private original-binding binding/derive!)

;; Joint responsibilities are intentional. A database contract cannot
;; establish who may submit; adapter admission cannot establish atomicity.
(def ^:private review-groups
  {:writer-mediation
   {:conditions #{:all-physical-writers-mediated-by-exact-installed-policy
                  :continuous-exact-installed-policy-mediation
                  :complete-admitted-writer-and-temporal-boundary-coverage
                  :every-physical-step-instantiates-qualified-schema
                  :complete-committed-aborted-and-temporal-boundary-classification}
    :requires #{:implementation-enforcement :deployment-isolation :coverage-proof :provider-contract}
    :next-evidence :closed-writer-ingress-and-lifecycle-argument
    :limitation :adapter-policy-does-not-intercept-raw-provider-calls}
   :provider-effects
   {:conditions #{:successful-guard-establishes-exact-prepared-image
                  :commit-applies-exact-source-derived-put-image
                  :abort-preserves-complete-mapped-image
                  :qualified-maintenance-preserves-complete-mapped-image
                  :maintenance-exact-footprint-and-provider-effect-correspondence}
    :requires #{:implementation-enforcement :source-correspondence :provider-contract}
    :next-evidence :versioned-transaction-semantics-and-exact-envelope-refinement
    :limitation :selected-rollback-and-commit-experiments-are-not-universal-provider-proof}
   :read-and-time
   {:conditions #{:faithful-pinned-provider-readbacks
                  :faithful-authoritative-record-and-historical-evidence-readback
                  :faithful-single-cut-record-and-state-projection
                  :exact-actual-endpoint-and-attributed-observation-boundaries
                  :protected-image-constant-across-time-advance
                  :faithful-complete-clock-and-valid-time-boundaries}
    :requires #{:provider-contract :implementation-enforcement :coverage-proof}
    :next-evidence :pinned-read-and-valid-time-boundary-contract
    :limitation :selected-diagonal-coverage-is-not-arbitrary-bitemporal-coverage}
   :attribution-and-admission
   {:conditions #{:faithful-command-and-envelope-attribution
                  :faithful-maintenance-attribution
                  :exact-admitted-envelope-and-alternative-attribution
                  :original-typed-guard-holds-and-inputs-admitted
                  :fresh-journal-slot-and-command-id
                  :authenticated-exact-command-binding-and-permitted-disclosure
                  :no-replay-attributed-mutations-or-submissions
                  :local-refusal-preserves-complete-attributed-image
                  :no-local-refusal-attributed-mutations-or-submissions}
    :requires #{:implementation-enforcement :source-correspondence :deployment-isolation :provider-contract}
    :next-evidence :complete-attributed-command-and-worker-call-paths
    :limitation :supplied-reports-and-selected-observations-do-not-exclude-hidden-effects}
   :projection-and-lowering
   {:conditions #{:faithful-declared-coordinate-meaning
                  :faithful-declared-concrete-outcome-meaning
                  :faithful-canonical-record-validation
                  :faithful-closed-prestate-and-coordinate-projection
                  :faithful-declared-outcome-and-record-construction
                  :disjoint-supplemental-effects-and-physical-identifiers
                  :faithful-functional-authoritative-projection
                  :well-typed-complete-projected-endpoints
                  :faithful-original-abstract-transition-semantics}
    :requires #{:semantic-proof :source-correspondence :implementation-enforcement :application-contract}
    :next-evidence :typed-projection-and-source-lowering-refinement
    :limitation :reference-fixture-does-not-establish-humanhelp-callback-correspondence}
   :loaded-code
   {:conditions #{:faithful-loaded-projection-semantics
                  :faithful-reviewed-source-map-semantics-and-loaded-code
                  :faithful-reviewed-source-and-loaded-replay-code
                  :faithful-reviewed-policy-and-loaded-code}
    :requires #{:source-correspondence :runtime-integrity :deployment-isolation}
    :next-evidence :reviewed-artifact-to-executing-code-correspondence
    :limitation :source-digests-and-var-identity-are-not-tamper-proof-attestation}
   :initial-root
   {:conditions #{:qualified-initialization-and-manifest-root}
    :requires #{:implementation-enforcement :semantic-proof :provider-contract :deployment-isolation}
    :next-evidence :qualified-root-establishment-and-storage-lineage
    :limitation :selected-root-observation-is-not-universal-initialization-proof}})

(defn- reject! [reason data]
  (throw (ex-info "Unqualified CLM-R2 condition inventory" (assoc data :reason reason))))

(defn- classify! [conditions groups]
  (let [counts (frequencies (mapcat :conditions (vals groups)))
        classified (set (keys counts))]
    (when-not (and (set? conditions) (seq conditions)
                   (every? #(and (set? (:conditions %)) (seq (:conditions %))
                                 (set? (:requires %)) (seq (:requires %))
                                 (keyword? (:next-evidence %)) (keyword? (:limitation %))) (vals groups))
                   (= conditions classified) (every? #{1} (vals counts)))
      (reject! :incomplete-or-overlapping-condition-review
               {:unclassified (set/difference (set conditions) classified)
                :extraneous (set/difference classified (set conditions))
                :overlapping (into #{} (keep (fn [[k n]] (when (> n 1) k))) counts)}))
    (into {} (map (fn [[id row]] [id (assoc row :status :open :trust-acceptance :not-performed
                                               :proof-discharge :not-performed)])) groups)))

(defn derive!
  "Re-derive the original issued CLM-R2 binding and require a total,
   disjoint review of its general conditions. Preserve the binding and
   original operation obligations. No selected history is consumed.
   Requirements describe future evidence routes, not proven dependencies."
  [admission source declarations fingerprint cases]
  (let [base {:profile profile :scope :issued-closed-claim-fixture-general-conditions-only
              :original-obligation-status :open :universal-refinement :not-established
              :proof-discharge :not-performed :premise-acceptance :not-performed
              :authority :none :certificate :not-issued}]
    (try
      (when-not (identical? original-binding binding/derive!)
        (reject! :condition-inventory-boundary-replaced {}))
      (let [b (original-binding admission source declarations fingerprint cases)]
        (when-not (and (= :conditional-original-clm-r2-binding (:status b))
                       (= fingerprint (:catalog-fingerprint b))
                       (= :open (:original-obligation-status b))
                       (= :not-performed (:proof-discharge b) (:premise-acceptance b))
                       (= :not-established (:universal-refinement b))
                       (= (:remaining-conditions b) (get-in b [:argument :remaining-conditions])))
          (reject! :unqualified-original-clm-r2-binding {}))
        (let [groups (classify! (:remaining-conditions b) review-groups)]
          (when-not (identical? original-binding binding/derive!)
            (reject! :condition-inventory-boundary-replaced {}))
          (merge base {:status :complete-open-condition-review
                       :catalog-fingerprint fingerprint :induction-fingerprint (:induction-fingerprint b)
                       :remaining-conditions (:remaining-conditions b)
                       :groups groups :priority [:writer-mediation :provider-effects]
                       :original-binding b})))
      (catch clojure.lang.ExceptionInfo e
        (merge base {:status :not-established} (select-keys (ex-data e) [:reason :unclassified :extraneous :overlapping]))))))
