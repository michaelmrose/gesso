(ns gesso.assurance.reference-transition-argument
  "Conditional equational simulation, not a proof of provider assumptions.
   A selected inventory can locate related obligations, not establish the
   general assumptions for every future admitted transition."
  (:require [clojure.walk :as walk]
            [gesso.assurance.proof.propositional-semantics :as logic]
            [gesso.assurance.reference-recovery-inventory :as inventory]
            [gesso.assurance.reference-transition-refinement :as refinement]))

(def profile :conditional-mapped-transition-argument-v1)
(def binding-profile :selected-transition-argument-binding-v1)
(def ^:private original-entailment logic/enumerate-entailment!)
(def ^:private original-transfer inventory/describe-transfer-binding)

(defn- reject! [reason]
  (throw (ex-info "Unqualified conditional transition argument" {:reason reason})))

(defn- relation [before after]
  [:abstract-transition :alternative :inputs [:project before] [:project after]])

(defn- case-arguments []
  {:committed-abstract-transition
   {:equalities {:actual-before :prepared
                 :actual-after [:put-image :prepared :suffix]}
    :facts #{(relation :prepared [:put-image :prepared :suffix])}
    :goal (relation :actual-before :actual-after)
    :conditions #{:successful-guard-establishes-exact-prepared-image
                  :commit-applies-exact-source-derived-put-image
                  :source-derived-put-image-satisfies-original-abstract-relation
                  :faithful-command-and-envelope-attribution
                  :faithful-functional-authoritative-projection}}
   :aborted-stutter
   {:equalities {:actual-after :actual-before}
    :facts #{} :goal [:= [:project :actual-before] [:project :actual-after]]
    :conditions #{:abort-preserves-complete-mapped-image
                  :faithful-command-and-envelope-attribution
                  :faithful-functional-authoritative-projection}}
   :maintenance-stutter
   {:equalities {:actual-after :actual-before}
    :facts #{} :goal [:= [:project :actual-before] [:project :actual-after]]
    :conditions #{:qualified-maintenance-preserves-complete-mapped-image
                  :faithful-maintenance-attribution
                  :faithful-functional-authoritative-projection}}})

(defn- check-case! [{:keys [equalities facts goal] :as argument}]
  ;; Orient equalities from actual row images to their hypothesized denotations.
  ;; Substitution is valid for a fixed projection and original relation/input.
  ;; Do not equate the prepared and actual images of an aborted attempt.
  (let [normalized (walk/postwalk-replace equalities goal)]
    (when-not (or (contains? facts normalized)
                  (and (= := (first normalized)) (= 3 (count normalized))
                       (= (second normalized) (nth normalized 2))))
      (reject! :equational-goal-not-derived))
    (assoc argument :normalized-goal normalized :status :derived-by-equality-substitution)))

(defn- case-split! [cases]
  (let [atom-for (fn [id] [:atom {:readers #{} :expression [:opaque id]}])
        atoms (mapv atom-for (sort (keys cases)))
        goal (atom-for :abstract-transition-or-stutter)
        premises (into {::coverage (into [:or] atoms)}
                       (map-indexed (fn [i a] [(keyword "transition-argument" (str "case-" i)) [:implies a goal]])) atoms)
        result (original-entailment premises goal)]
    (when-not (and (= :entails (:result result)) (= 16 (:valuations-checked result))
                   (= 7 (:premise-satisfying-valuations result)))
      (reject! :case-split-not-derived))
    ;; The coverage premise is essential; the local derivations do not prove it.
    (when-not (= :counterexample (:result (original-entailment (dissoc premises ::coverage) goal)))
      (reject! :invalid-coverage-necessity-check))
    {:premises premises :goal goal :entailment result :coverage :assumed-not-derived}))

(defn derive!
  "Check the implication for arbitrary images satisfying the named conditions.
   Equality substitution is a mathematical rule, not loaded-code attestation.
   No universal correctness of source-derived writes or provider semantics is
   inferred from the finite selected witnesses."
  []
  (let [base {:profile profile :scope :conditional-closed-mapped-step-schema
              :universal-refinement :not-established :proof-discharge :not-performed
              :authority :none :certificate :not-issued}]
    (try
      (when-not (identical? original-entailment logic/enumerate-entailment!)
        (reject! :argument-boundary-replaced))
      (let [cases (into {} (map (fn [[k v]] [k (check-case! v)])) (case-arguments))]
        (assoc base :status :conditional-equational-derivation :cases cases
               :case-split (case-split! cases)
               :remaining-general-conditions
               (into #{:complete-admitted-writer-and-temporal-boundary-coverage
                       :faithful-original-abstract-transition-semantics}
                     (mapcat :conditions) (vals cases))))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :status :not-established :reason (:reason (ex-data e)))))))

(def ^:private original-argument derive!)

(defn- related-premises [maintenance?]
  (cond-> {:successful-guard-establishes-exact-prepared-image #{:reference-physical/guarded-atomic-write}
           :commit-applies-exact-source-derived-put-image #{:reference-physical/guarded-atomic-write}
           :abort-preserves-complete-mapped-image #{:reference-physical/abort-atomicity}
           :faithful-command-and-envelope-attribution #{:reference-physical/submission-attribution
                                                       :reference-physical/aborted-attempt-attribution}
           :faithful-functional-authoritative-projection #{:reference-physical/authoritative-projection}
           :complete-admitted-writer-and-temporal-boundary-coverage #{:reference-physical/writer-coverage}}
    maintenance? (assoc :qualified-maintenance-preserves-complete-mapped-image
                        #{:reference-physical/writer-coverage :reference-physical/guarded-atomic-write}
                        :faithful-maintenance-attribution
                        #{:reference-physical/worker-clock :reference-physical/writer-coverage})))

(defn bind-selected!
  "Locate related open premises for the original selected simulation only.
   Intended for internal issued evidence; supplied reports are not credentials.
   Neither the selected premise scope nor finite witnesses imply the general
   conditions. The source-write and abstract transition semantics conditions
   have no direct physical-premise equivalent and stay explicitly unbound."
  [checked physical inv abstract]
  (let [base {:profile binding-profile :scope :original-selected-transition-witnesses-only
              :premise-acceptance :not-performed :proof-discharge :not-performed
              :universal-refinement :not-established :authority :none :certificate :not-issued}
        fail #(assoc base :status :not-established :reason %)]
    (try
      (when-not (and (identical? original-transfer inventory/describe-transfer-binding)
                     (identical? original-argument derive!))
        (reject! :argument-boundary-replaced))
      (let [transfer (original-transfer checked physical inv abstract)
            argument (original-argument)
            report (get-in checked [:physical-readbacks :transition-correspondence])
            interface (:scope physical)
            steps (:steps report)
            boundaries (subvec (vec (:checked-boundaries checked)) 1)
            maintenance? (seq (:selected-maintenance interface))
            by-id (into {} (map (fn [d] [(get-in d [:assumption :id]) (:premise-fingerprint d)]))
                        (:obligations physical))
            links (related-premises maintenance?)
            bindings (into {} (map (fn [[k ids]] [k (set (map by-id ids))])) links)]
        (when-not (and (= :bound (:status transfer))
                       (= :conditional-equational-derivation (:status argument))
                       (= refinement/selected-profile (:profile report))
                       (= :conditional-selected-transition-correspondence (:status report))
                       (= :original-selected-mapped-row-boundaries-only (:scope report))
                       (= :exact-complete-mapped-tables (:row-correspondence report))
                       (= :exact-retained-expectation-with-qualified-issued-envelope-premise (:envelope-binding report))
                       (= :mapped-tables-only (:maintenance-scope report))
                       (= :not-established (:provider-refinement report) (:writer-coverage report) (:universal-refinement report))
                       (= :not-performed (:proof-discharge report))
                       (= :none (:authority report)) (= :not-issued (:certificate report))
                       (= (:selected-tx-ids checked) (:selected-tx-ids report) (:selected-tx-ids interface))
                       (= (get-in checked [:conditional-physical-invariant :checked-coordinates])
                          (:checked-coordinates report) (get-in checked [:physical-readbacks :checked-coordinates]))
                       (seq steps) (vector? steps) (= (count steps) (count boundaries))
                       (= (mapv :alternative-id (remove #(= :maintenance-stutter (:relation %)) steps))
                          (mapv :alternative-id (get-in checked [:judgment-subject :selected-operations])))
                       (= (set (map :tx-id (filter #(= :maintenance-stutter (:relation %)) steps)))
                          (set (map :tx-id (:selected-maintenance interface))))
                       (= (:selected-tx-ids checked) (mapv :tx-id (remove #(= :maintenance-stutter (:relation %)) steps)))
                       (= [:committed-abstract-transition :aborted-stutter :committed-abstract-transition]
                          (mapv :relation (remove #(= :maintenance-stutter (:relation %)) steps)))
                       (= (mapv #(select-keys % [:tx-id :relation]) steps)
                          (mapv #(select-keys % [:tx-id :relation]) boundaries))
                       (every? (fn [{:keys [relation checked-row-effect]}]
                                 (= checked-row-effect (case relation
                                                        :committed-abstract-transition :exact-source-derived-puts
                                                        :aborted-stutter :exact-row-stutter
                                                        :maintenance-stutter :exact-mapped-row-stutter
                                                        ::unsupported))) steps)
                       (= (boolean maintenance?) (boolean (some #(= :maintenance-stutter (:relation %)) steps)))
                       (every? string? (mapcat identity (vals bindings)))
                       (every? (:unaccepted-premise-fingerprints inv) (mapcat identity (vals bindings))))
          (reject! :unqualified-selected-transition-argument))
        (assoc base :status :conditional-selected-argument-bound
               :recovery-cut-ref (:recovery-cut-ref checked) :catalog-fingerprint (:catalog-fingerprint checked)
               :checked-coordinates (:checked-coordinates report)
               :related-selected-premise-fingerprints bindings
               :original-open-physical-premise-fingerprints (:open-premise-fingerprints physical)
               :general-conditions (:remaining-general-conditions argument)
               :conditions-without-direct-selected-binding
               (reduce disj (:remaining-general-conditions argument) (keys bindings))
               :selected-premises-entail-general-conditions :not-established
               :argument argument))
      (catch clojure.lang.ExceptionInfo e (fail (or (:reason (ex-data e)) :unqualified-transition-argument)))
      (catch Exception _ (fail :invalid-selected-transition-argument)))))
