(ns gesso.assurance.proof.literal-partition-congruence
  "Source-derived structural congruence in abstract comparison semantics.
   Local entailments exhaust all opaque Boolean valuations. Neither the
   scalar-to-comparison abstraction nor either JVM evaluator is proved here."
  (:require [gesso.assurance.proof.literal-partition :as partition]
            [gesso.assurance.proof.propositional-semantics :as semantics]))

(def profile :abstract-literal-partition-congruence-v1)
(def ^:private original-audit partition/audit-rule!)
(def ^:private original-entailment semantics/enumerate-entailment!)

(defn- reject! [reason]
  (throw (ex-info "Unqualified abstract congruence" {:reason reason})))

(defn- atom-form [side i]
  ;; Opaque propositions, never executable rule declarations.
  [:atom {:readers #{} :expression [:congruence side i]}])

(defn- iff [a b]
  [:and [:implies a b] [:implies b a]])

(defn- local! [arity left right]
  (let [a (mapv #(atom-form :original %) (range arity))
        b (mapv #(atom-form :representative %) (range arity))
        premises (into {} (map-indexed (fn [i [x y]]
                                         [(keyword "congruence" (str i)) (iff x y)]))
                       (map vector a b))
        goal (iff (left a) (right b))
        result (original-entailment premises goal)]
    (when-not (and (= semantics/interpretation-type (:type result))
                   (= semantics/semantics-version (:version result))
                   (= :entails (:result result))
                   (= :complete-opaque-boolean-valuations-only (:scope result))
                   (= (* 2 arity) (:atom-count result))
                   (= (bit-shift-left 1 (* 2 arity)) (:valuations-checked result))
                   (= (bit-shift-left 1 arity) (:premise-satisfying-valuations result)))
      (reject! :local-congruence-not-established))
    {:premises premises :goal goal :entailment result}))

(defn- comparison [op]
  ;; The flags are abstract observations of a value against one literal.
  ;; Integer order uses below/equal/above; equality needs only equal.
  ;; Requiring the same flags is weaker than requiring the same scalar.
  (case op
    := [[:equal] first]
    :not= [[:equal] (fn [[e]] [:not e])]
    :< [[:below] first]
    :<= [[:below :equal] (fn [xs] (into [:or] xs))]
    :> [[:above] first]
    :>= [[:above :equal] (fn [xs] (into [:or] xs))]))

(defn- connective [op]
  (fn [xs] (into [op] xs)))

(defn- derive-node! [form types literals]
  (let [[op a b] form]
    (case op
      :literal
      (do (when-not (and (= 2 (count form)) (boolean? a))
            (reject! :unsupported-constant))
          {:source form :rule :same-boolean-constant})

      (:= :not= :< :<= :> :>=)
      (let [field (second a) literal (second b)
            descriptor (types field)
            [observations formula] (comparison op)]
        (when-not (and descriptor (contains? (literals field) literal)
                       (= [:read field] a) (= [:literal literal] b))
          (reject! :unbound-comparison))
        {:source form :rule :same-comparison-observations
         :reader field :reader-type descriptor :literal literal
         :observations observations
         :local-proof (local! (count observations) formula formula)})

      (:not :implies :and :or)
      (let [children (mapv #(derive-node! % types literals) (rest form))
            n (count children)
            f (connective op)]
        (when-not (case op :not (= n 1) :implies (= n 2) (<= 2 n))
          (reject! :unsupported-connective-arity))
        {:source form :rule :child-congruence :children children
         ;; Abstract n-ary conjunction/disjunction are left folds of the
         ;; binary truth functions. The same local proof applies at each
         ;; fold step, without enumerating the whole original formula.
         :composition (if (#{:and :or} op) :binary-left-fold :direct)
         :applications (if (#{:and :or} op) (dec n) 1)
         :local-proof (local! (if (= :not op) 1 2) f f)})
      (reject! :unsupported-expression))))

(defn derive!
  "Recheck original typed partition source, then derive abstract truth
   preservation under equal comparison observations by structural recursion.
   Every local Boolean entailment is exhaustive and nonvacuous. Constants
   are identical by construction. The conclusion is conditional on the
   abstract semantics, not a claim about arbitrary executable evaluators."
  [rule state-types]
  (let [base {:profile profile :scope :original-abstract-expression-only
              :scalar-observation-refinement :not-established
              :executable-evaluator-refinement :not-established
              :proof-discharge :not-performed :authority :none :certificate :not-issued}]
    (try
      (when-not (and (identical? original-audit partition/audit-rule!)
                     (identical? original-entailment semantics/enumerate-entailment!))
        (reject! :congruence-boundary-replaced))
      (let [{:keys [status reader-types comparison-literals] :as report}
            (original-audit rule state-types)]
        (when-not (and (= :matched status) (= partition/profile (:profile report))
                       (= rule (:rule report))
                       (= (select-keys state-types (:readers rule)) reader-types))
          (reject! :unqualified-partition))
        (assoc base :status :conditional-abstract-congruence
               :rule rule :reader-types reader-types
               :premise :equal-original-comparison-observations
               :conclusion :equal-abstract-expression-truth
               :derivation (derive-node! (:expression rule) reader-types comparison-literals)
               :checking :source-structural-local-boolean-entailment))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-congruence))))))
