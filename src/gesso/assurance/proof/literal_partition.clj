(ns gesso.assurance.proof.literal-partition
  "Derive a finite literal-comparison partition from original typed rules.
   This covers the Claim fixture's scalar read/literal fragment. Agreement
   on representatives is not proof that executable evaluators factor through
   the partition, nor proof of invariant preservation."
  (:require [gesso.assurance.proof.typed-atom-oracle :as oracle]))

(def profile :source-derived-literal-partition-v1)
(def ^:private original-audit oracle/audit-typed-atom!)
(def ^:private max-cases 8192)

(defn- unsupported! [reason]
  (throw (ex-info "Unsupported literal partition" {::unsupported true :reason reason})))

(defn- literals! [expression types]
  (let [nodes (volatile! 0)]
    (letfn [(walk [form depth]
              (when-not (and (vector? form) (<= depth 32)
                             (<= (vswap! nodes inc) 1024))
                (unsupported! :expression-budget-or-shape))
              (let [[op a b] form]
                (case op
                  (:and :or) (if (<= 3 (count form))
                               (apply merge-with into (map #(walk % (inc depth)) (rest form)))
                               (unsupported! :connective-arity))
                  :not (if (= 2 (count form)) (walk a (inc depth)) (unsupported! :connective-arity))
                  :implies (if (= 3 (count form)) (merge-with into (walk a (inc depth)) (walk b (inc depth)))
                               (unsupported! :connective-arity))
                  :literal (if (and (= 2 (count form)) (boolean? a)) {} (unsupported! :nonboolean-literal))
                  (:= :not= :< :<= :> :>=)
                  (let [field (when (vector? a) (second a))
                        value (when (vector? b) (second b)) descriptor (types field)]
                    (when-not (and (= 3 (count form)) (vector? a) (vector? b)
                                   (= [:read field] a) (= [:literal value] b)
                                   descriptor (not (:optional? descriptor))
                                   (if (#{:= :not=} op)
                                     (if (nil? value) (true? (:nullable? descriptor))
                                       (case (:type descriptor)
                                         :integer (integer? value) :keyword (keyword? value)
                                         :string (string? value) :boolean (boolean? value) false))
                                     (and (= :integer (:type descriptor)) (integer? value)
                                          (not (:nullable? descriptor)))))
                      (unsupported! :unsupported-comparison))
                    {field #{value}})
                  (unsupported! :unsupported-expression))))]
      (walk expression 0))))

(defn- fresh-value [kind constants]
  (first (remove constants (map (fn [i] (case kind
                                          :keyword (keyword "gesso.partition" (str "other-" i))
                                          :string (str "gesso.partition/other-" i))) (range)))))

(defn- representatives [descriptor constants]
  (let [non-nil (disj constants nil)
        values (case (:type descriptor)
                 :boolean [false true]
                 (:keyword :string) (conj (vec (sort-by str non-nil))
                                          (fresh-value (:type descriptor) non-nil))
                 :integer (let [ordered (vec (sort non-nil))]
                            (if (empty? ordered) [0]
                              (into [(dec' (first ordered))]
                                    (concat ordered
                                            (keep (fn [[a b]] (when (< (inc' a) b) (inc' a)))
                                                  (partition 2 1 ordered))
                                            [(inc' (peek ordered))]))))
                 (unsupported! :unsupported-reader-type))]
    (cond-> values (:nullable? descriptor) (conj nil))))

(defn audit-rule!
  "Construct comparison-class representatives without caller-supplied
   domains. The returned diagnostic preserves the exact source and types.
   Only the read/literal fragment is supported; no read/read, optional reads,
   or general arithmetic. Literal partitions preserve the abstract truth
   of the admitted comparisons, but executable partition invariance is open."
  [rule state-types]
  (let [base {:profile profile :scope :source-derived-representatives-only
              :universal-evaluator-correspondence :not-established
              :proof-discharge :not-performed :authority :none}]
    (try
      (when-not (and (identical? original-audit oracle/audit-typed-atom!)
                     (map? rule) (= #{:readers :expression} (set (keys rule)))
                     (set? (:readers rule)) (map? state-types))
        (unsupported! :unqualified-source-or-boundary))
      (let [types (select-keys state-types (:readers rule))
            literals (literals! (:expression rule) types)]
        (when-not (= (:readers rule) (set (keys literals)) (set (keys types)))
          (unsupported! :reader-footprint-mismatch))
        (let [domains (into {} (map (fn [[field constants]]
                                     [field (representatives (types field) constants)])) literals)
              cases (reduce *' 1 (map count (vals domains)))
              result (original-audit {:rule rule :reader-types types :binding-domains domains
                                      :max-cases max-cases})]
          (if (and (= :bounded-agreement (:status result))
                   (= oracle/report-type (:type result))
                   (= oracle/oracle-version (:version result))
                   (= (:readers rule) (:reader-ids result))
                   (= cases (:cases-planned result) (:cases-checked result))
                   (= cases (+ (:true-cases result) (:false-cases result))))
            (assoc base :status :matched :rule rule :reader-types types
                   :comparison-literals literals :representative-domains domains
                   :cases-checked cases :true-cases (:true-cases result) :false-cases (:false-cases result)
                   :executable-partition-invariance :not-established)
            (assoc base :status :not-established :reason (:reason result)
                   :oracle-status (:status result)))))
      (catch clojure.lang.ExceptionInfo e
        (if (::unsupported (ex-data e))
          (assoc base :status :unsupported :reason (:reason (ex-data e)))
          (throw e))))))
