(ns gesso.assurance.proof.literal-partition-substitution
  "Check one typed valuation's substitution into a source-derived partition.
   The independent, total Boolean tree uses only scalar comparison atoms.
   This is a checked instance of congruence, not a universal evaluator proof."
  (:require [gesso.assurance.proof.literal-partition :as partition]
            [gesso.assurance.proof.typed-presence-semantics :as presence]
            [gesso.assurance.rule-eval :as production]))

(def profile :literal-partition-substitution-v1)
(def ^:private original-audit partition/audit-rule!)
(def ^:private original-independent presence/evaluate-expression!)
(def ^:private original-production production/evaluate-rule!)

(defn- reject! [reason]
  (throw (ex-info "Unqualified literal partition substitution" {:reason reason})))

(defn- signature [descriptor constants value]
  ;; Separate nil from nonnil even when no nil literal occurs. Integer
  ;; ordering distinguishes both sides of each cutpoint, not just equality.
  (cond
    (nil? value) [:nil]
    (= :boolean (:type descriptor)) [:boolean value]
    :else [:scalar (into {} (map (fn [literal]
                            [literal (cond
                                       (nil? literal) :different
                                       (= :integer (:type descriptor))
                                       (cond (< value literal) :below
                                             (> value literal) :above
                                             :else :equal)
                                       (= value literal) :equal
                                       :else :different)])) constants)]))

(defn- representative! [descriptor constants domain value]
  (let [class (signature descriptor constants value)
        matches (filterv #(= class (signature descriptor constants %)) domain)]
    (when-not (= 1 (count matches))
      (reject! :nonunique-or-missing-representative))
    (first matches)))

(defn- comparison-value [op value literal]
  (case op
    := (= value literal)
    :not= (not= value literal)
    :< (< value literal)
    :<= (<= value literal)
    :> (> value literal)
    :>= (>= value literal)))

(defn- truth-tree [expression bindings]
  ;; The qualified fragment has no partial atoms: both branches can be
  ;; evaluated for this diagnostic even when runtime short-circuits them.
  (let [[op a b] expression]
    (case op
      :literal {:op op :value a}
      (:= :not= :< :<= :> :>=)
      {:op op :value (comparison-value op (get bindings (second a)) (second b))}
      (:and :or :not :implies)
      (let [children (mapv #(truth-tree % bindings) (rest expression))
            values (mapv :value children)]
        {:op op :children children
         :value (case op
                  :and (every? true? values)
                  :or (boolean (some true? values))
                  :not (not (first values))
                  :implies (or (not (first values)) (second values)))})
      (reject! :unsupported-truth-tree))))

(defn- evaluated-value! [rule types bindings]
  (let [p (original-production rule types bindings)
        i (original-independent (:expression rule) types bindings)]
    (when-not (and (= production/evaluated-type (:type p))
                   (= production/evaluator-version (:version p))
                   (= presence/result-type (:type i))
                   (= presence/semantics-version (:version i))
                   (= (:expression rule) (:expression p) (:expression i))
                   (= (:readers rule) (:readers p) (:source-readers i))
                   (= :not-proved (:proof-status p) (:proof-status i))
                   (instance? Boolean (:value p))
                   (= (:value p) (:value i)))
      (reject! :unqualified-or-disagreeing-evaluators))
    (:value p)))

(defn check-substitution!
  "Rebuild and audit the partition from original source, select exactly one
   representative per typed reader, and compare the full Boolean truth tree
   and both executable evaluator results before and after substitution.
   No caller-supplied partition, domain or proof report is accepted."
  [rule state-types bindings]
  (let [base {:profile profile :scope :one-typed-valuation-only
              :universal-evaluator-correspondence :not-established
              :proof-discharge :not-performed :authority :none :certificate :not-issued}]
    (try
      (when-not (and (identical? original-audit partition/audit-rule!)
                     (identical? original-independent presence/evaluate-expression!)
                     (identical? original-production production/evaluate-rule!))
        (reject! :substitution-boundary-replaced))
      (let [{:keys [status reader-types comparison-literals representative-domains]}
            (original-audit rule state-types)]
        (when-not (= :matched status) (reject! :unqualified-partition))
        ;; Validate the original valuation with both typed interpreters before
        ;; inspecting values. Extra, missing and ill-typed readers are rejected.
        (let [original-value (evaluated-value! rule reader-types bindings)
              representative (into {} (map (fn [[field descriptor]]
                                             [field (representative! descriptor
                                                                     (comparison-literals field)
                                                                     (representative-domains field)
                                                                     (get bindings field))])) reader-types)
              original-tree (truth-tree (:expression rule) bindings)
              representative-tree (truth-tree (:expression rule) representative)
              representative-value (evaluated-value! rule reader-types representative)]
          (when-not (and (= original-tree representative-tree)
                         (= original-value representative-value (:value original-tree)))
            (reject! :partition-substitution-mismatch))
          (assoc base :status :matched :rule rule :reader-types reader-types
                 :representative-bindings representative :value original-value
                 :atom-and-connective-agreement :checked-full-truth-tree)))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-substitution))))))
