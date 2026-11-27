(ns gesso.assurance.reference-source-transition-argument
  "Conditional composition for original journal-writing alternatives only.
   Re-derive both arguments from source; reports are not credentials."
  (:require [clojure.set :as set]
            [clojure.walk :as walk]
            [gesso.assurance.reference-recovery-projection :as projection]
            [gesso.assurance.reference-source-write-argument :as writes]
            [gesso.assurance.reference-transition-argument :as transition]
            [gesso.assurance.transition :as construction]))

(def profile :conditional-source-transition-composition-v1)
(def ^:private original-projection projection/check-catalog!)
(def ^:private original-writes writes/derive-catalog!)
(def ^:private original-transition transition/derive!)
(def ^:private original-build construction/build-guarded-replacement!)
(def ^:private source-condition :source-derived-put-image-satisfies-original-abstract-relation)

(defn- reject! [reason]
  (throw (ex-info "Unqualified source-transition composition" {:reason reason})))

(defn- boundaries! []
  (when-not (and (identical? original-projection projection/check-catalog!)
                 (identical? original-writes writes/derive-catalog!)
                 (identical? original-transition transition/derive!)
                 (identical? original-build construction/build-guarded-replacement!))
    (reject! :composition-boundary-replaced)))

(defn- target [[kind a b]]
  (if (= :journal-slot kind) [kind a b] [:document a b]))

(defn- original-term [original field]
  (if-let [[op operand] (get-in original [:replacements field])]
    (case op
      :literal [:literal operand]
      :read [(if (contains? (:state-types original) operand) :pre :input) operand]
      (reject! :unsupported-composition-rhs))
    [:pre field]))

(defn- equations! [original coordinates report case-key]
  ;; Reconstruct the relation's required field equations independently of
  ;; the symbolic writer's RHS helper; require the complete projection.
  (let [groups (group-by (comp target val) coordinates)
        equations (:equations report)]
    (when-not (= (set (keys groups)) (set (keys equations)))
      (reject! :incomplete-composition-equations))
    (doseq [[address entries] groups]
      (let [fields (set (map key entries))
            expected (into {} (map (fn [f] [f (original-term original f)])) fields)
            eq (equations address)]
        (if (= :journal-slot (first address))
          (let [written? (seq (set/intersection fields (:writes original)))]
            (when-not (and (= expected (:projected eq))
                           (if written?
                             (and (= :absent (:required-prestate eq)) (= case-key (:case eq)))
                             (= :unchanged-slot (:effect eq))))
              (reject! :composition-journal-equation-mismatch)))
          (let [presence (if (= :nullable-document (first (val (first entries))))
                           [:present :absent] [:present])]
            (when-not (and (vector? eq) (= presence (mapv :presence eq)))
              (reject! :composition-presence-coverage-mismatch))
            (doseq [branch eq]
              (let [terms (if (= :absent (:presence branch))
                            (walk/postwalk-replace (into {} (map (fn [f] [[:pre f] [:literal nil]])) fields) expected)
                            expected)]
                (when-not (= terms (:expected branch) (:projected branch))
                  (reject! :composition-field-equation-mismatch))))))))))

(defn- compose-alternative! [original id checked report generic case-key]
  (let [built (original-build original)
        mapping (get-in checked [:operation-mappings id])
        commit (get-in generic [:cases :committed-abstract-transition])
        relation-id {:catalog-id (:catalog-id checked)
                     :catalog-fingerprint (:catalog-fingerprint checked)
                     :alternative-id id :transition-fingerprint (:transition-fingerprint built)
                     :mapping-fingerprint (:mapping-fingerprint mapping)}
        ;; The same symbolic inputs occur in the fact and the goal.
        inputs [:admitted-inputs id (:input-types original)]
        fact [:abstract-transition :alternative :inputs [:project :prepared]
              [:project [:put-image :prepared :suffix]]]
        goal [:abstract-transition :alternative :inputs [:project :actual-before] [:project :actual-after]]]
    (when-not (and (= id (get-in original [:subject :id]) (:alternative-id report))
                   (= :conditional-symbolic-write-correspondence (:status report))
                   (= (:mapping-fingerprint mapping) (:mapping-fingerprint report))
                   (= (:guard original) (:guard report))
                   (= (:replacements original) (:assignments report))
                   (= (:frame original) (:frame report))
                   (= :derived-by-equality-substitution (:status commit))
                   (= #{fact} (:facts commit)) (= goal (:goal commit))
                   (= fact (:normalized-goal commit))
                   (= {:actual-before :prepared :actual-after [:put-image :prepared :suffix]} (:equalities commit))
                   (contains? (:conditions commit) source-condition))
      (reject! :composition-alternative-mismatch))
    (equations! original (:field-coordinates checked) report case-key)
    ;; Specialize both the projection and put-image to this exact mapping
    ;; and alternative. Substitute only endpoint images, not source data.
    (let [put-image [:put-image relation-id :prepared inputs case-key :suffix]
          project (fn [image] [:project (:mapping-fingerprint mapping) image])
          specialize-image #(if (= [:put-image :prepared :suffix] %) put-image %)
          instantiate (fn [[_ _ _ [_ before] [_ after]]]
                        [:abstract-transition relation-id inputs
                         (project (specialize-image before))
                         (project (specialize-image after))])
          equalities {:actual-before :prepared :actual-after put-image}
          fact (instantiate fact) goal (instantiate goal)
          normalized (-> goal (update-in [3 2] equalities) (update-in [4 2] equalities))]
      (when-not (= fact normalized)
        (reject! :composition-substitution-mismatch))
      {:identity relation-id :case-key case-key :guard (:guard original)
       :assignments (:replacements original) :frame (:frame original)
       :source-equation-fact fact :goal goal :equalities equalities
       :normalized-goal normalized
       :status :conditional-committed-transition-derivation})))

(defn derive!
  "Compose the field-equation relation under the source and projection
   assumptions with the committed case's equality substitution. This is a
   conditional argument for the journal-writing alternatives, not replay,
   all-writer coverage, provider semantics, or an issued historical judgment."
  [source declarations fingerprint cases]
  (let [base {:profile profile :scope :original-declared-journal-writing-commit-case-only
              :universal-refinement :not-established :writer-coverage :not-established
              :proof-discharge :not-performed :authority :none :certificate :not-issued}]
    (try
      (boundaries!)
      (let [checked (original-projection source declarations fingerprint cases)
            write-report (original-writes source declarations fingerprint cases)
            generic (original-transition)
            operations (get-in source [:inductive-source :catalog :operations])
            by-id (into {} (map (juxt val key)) (:case-alternatives checked))
            writers (set (keys by-id))
            outside (set/difference (set (map :id operations)) writers)]
        (when-not (and (= :matched (:status checked))
                       (= :conditional-symbolic-catalog-derivation (:status write-report))
                       (= :conditional-equational-derivation (:status generic))
                       (= fingerprint (:catalog-fingerprint checked) (:catalog-fingerprint write-report))
                       (= :projected-put-image-satisfies-original-replacement-and-frame-equations (:conclusion write-report))
                       (seq writers) (= writers (set (keys (:alternatives write-report))))
                       (= outside (:outside-fragment write-report))
                       (= :not-performed (:proof-discharge checked) (:proof-discharge write-report) (:proof-discharge generic)))
          (reject! :composition-catalog-mismatch))
        (let [alternatives (into {} (keep (fn [{:keys [id construction]}]
                                            (when (contains? writers id)
                                              [id (compose-alternative! construction id checked
                                                                        (get-in write-report [:alternatives id]) generic (by-id id))]))) operations)]
          (boundaries!)
          (assoc base :status :conditional-catalog-commit-composition
                 :catalog-id (:catalog-id checked) :catalog-fingerprint fingerprint
                 :alternatives alternatives :outside-fragment outside
                 :remaining-conditions (set/union (disj (:remaining-general-conditions generic) source-condition)
                                                  (:remaining-conditions write-report) (:remaining-conditions checked)
                                                  #{:well-typed-complete-projected-endpoints})
                 :source-condition-expansion (:remaining-conditions write-report))))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-composition))))))
