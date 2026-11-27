(ns gesso.assurance.proof.literal-partition-reduction
  "Compose abstract scalar coverage and congruence with a checked truth table.
   Executable refinement on arbitrary original valuations remains open."
  (:require [gesso.assurance.proof.literal-partition :as partition]
            [gesso.assurance.proof.literal-partition-coverage :as coverage]
            [gesso.assurance.proof.literal-partition-congruence :as congruence]
            [gesso.assurance.proof.typed-presence-semantics :as presence]
            [gesso.assurance.rule-eval :as production]))

(def profile :conditional-abstract-literal-reduction-v1)
(def ^:private original-partition partition/audit-rule!)
(def ^:private original-coverage coverage/check-coverage!)
(def ^:private original-congruence congruence/derive!)
(def ^:private original-independent presence/evaluate-expression!)
(def ^:private original-production production/evaluate-rule!)

(defn- reject! [reason]
  (throw (ex-info "Unqualified abstract literal reduction" {:reason reason})))

(defn- abstract-value! [node cells]
  (let [[op a] (:source node)]
    (case (:rule node)
      :same-boolean-constant a
      :same-comparison-observations
      (let [observation (get-in cells [(:reader node) :observations (:literal node)])]
        (when-not (#{:equal :different :below :above} observation)
          (reject! :missing-cell-observation))
        (case op
          := (= :equal observation)
          :not= (not= :equal observation)
          :< (= :below observation)
          :<= (contains? #{:below :equal} observation)
          :> (= :above observation)
          :>= (contains? #{:above :equal} observation)))
      :child-congruence
      (let [xs (mapv #(abstract-value! % cells) (:children node))]
        (case op
          :not (not (first xs))
          :implies (or (not (first xs)) (second xs))
          :and (every? true? xs)
          :or (boolean (some true? xs))))
      (reject! :unsupported-derivation-node))))

(defn- checked-row! [rule types derivation cells]
  (let [bindings (into {} (map (fn [[id cell]] [id (:representative cell)])) cells)
        abstract (abstract-value! derivation cells)
        p (original-production rule types bindings)
        i (original-independent (:expression rule) types bindings)]
    (when-not (and (= production/evaluated-type (:type p))
                   (= production/evaluator-version (:version p))
                   (= presence/result-type (:type i))
                   (= presence/semantics-version (:version i))
                   (= (:expression rule) (:expression p) (:expression i))
                   (= (:readers rule) (:readers p) (:source-readers i))
                   (= :not-proved (:proof-status p) (:proof-status i))
                   (= :not-admitted (:admission p) (:admission i))
                   (= :none (:authority p) (:authority i))
                   (instance? Boolean abstract)
                   (= abstract (:value p) (:value i)))
      (reject! :representative-truth-mismatch))
    {:bindings bindings :value abstract}))

(defn- cell-product [cells]
  (reduce (fn [rows id]
            (for [row rows cell (get cells id)] (assoc row id cell)))
          [{}] (sort (keys cells))))

(defn derive!
  "Recompute all evidence from the same original rule and typed footprint.
   Each Cartesian cell tuple fixes a truth value in abstract semantics.
   Check that value against both executable evaluators on its representative."
  [rule state-types]
  (let [base {:profile profile :scope :original-abstract-typed-expression-only
              :executable-evaluator-refinement :not-established
              :universal-evaluator-correspondence :not-established
              :proof-discharge :not-performed :authority :none :certificate :not-issued}]
    (try
      (when-not (and (identical? original-partition partition/audit-rule!)
                     (identical? original-coverage coverage/check-coverage!)
                     (identical? original-congruence congruence/derive!)
                     (identical? original-independent presence/evaluate-expression!)
                     (identical? original-production production/evaluate-rule!))
        (reject! :reduction-boundary-replaced))
      (let [p (original-partition rule state-types)
            c (original-coverage rule state-types)
            g (original-congruence rule state-types)
            types (select-keys state-types (:readers rule))
            cells (:cells c) derivation (:derivation g)
            domains (into {} (map (fn [[id cs]] [id (mapv :representative cs)])) cells)
            expected (reduce *' 1 (map count (vals cells)))]
        (when-not (and (= partition/profile (:profile p)) (= :matched (:status p))
                       (= coverage/profile (:profile c)) (= :conditional-abstract-coverage (:status c))
                       (= congruence/profile (:profile g)) (= :conditional-abstract-congruence (:status g))
                       (= rule (:rule p) (:rule c) (:rule g))
                       (= types (:reader-types p) (:reader-types c) (:reader-types g))
                       (= (:readers rule) (set (keys cells)) (set (keys types)))
                       (= domains (:representative-domains p))
                       (= :unique-represented-cell-with-constant-literal-observations (:conclusion c))
                       (= :equal-original-comparison-observations (:premise g))
                       (= :equal-abstract-expression-truth (:conclusion g))
                       (= (:expression rule) (:source derivation))
                       (<= 1 expected 8192) (= expected (:cases-checked p)))
          (reject! :unqualified-composition))
        (let [rows (mapv #(checked-row! rule types derivation %) (cell-product cells))
              true-count (count (filter :value rows))]
          (when-not (and (= expected (count rows) (count (set (map :bindings rows))))
                         (= true-count (:true-cases p))
                         (= (- expected true-count) (:false-cases p)))
            (reject! :incomplete-representative-truth-table))
          (assoc base :status :conditional-abstract-reduction
                 :rule rule :reader-types types :conditions (:conditions c)
                 :conclusion :abstract-truth-factors-through-representative-cells
                 :coverage c :congruence g :partition p :truth-table rows
                 :cases-checked expected :true-cases true-count
                 :false-cases (- expected true-count)
                 :representative-evaluator-agreement :checked-per-valuation)))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-reduction))))))
