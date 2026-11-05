(ns gesso.assurance.proof.typed-comparison-semantics
  "Independent, restricted interpretation of S1 equality, mathematical
   integer comparisons, and finite-set membership (v1342).

   (evaluate-atom! [:> [:read :request/score] [:literal 7]]
                   {:request/score {:type :integer}}
                   {:request/score 8})

   The language is a *direct atomic expression*: the two operands are each
   [:read qualified-id] or [:literal scalar-or-homogeneous-finite-set].
   It does not interpret :and/:or/:implies or infer branch-sensitive presence
   and nonnil facts. All optional reads and nullable integer/member reads
   therefore fail closed unless separately established as safe by a later
   branch-sensitive semantics layer.

   Reader values are validated using the independent v1341 partial-map
   semantics. Operator compatibility and denotation are defined here, without
   calling rule, rule-type, rule-eval, or a proof kernel. A nominal enum may
   compare with its own enum type or with a keyword *literal from its domain*;
   a keyword-valued reader does not acquire nominal enum identity.

   Returned data is copyable diagnostic information only. This does not prove
   type-checker/evaluator correspondence, portability, source authority,
   runtime enforcement, or certificate admission."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.typed-value-semantics :as values]))

(def semantics-version 1)
(def result-type :gesso.assurance.proof.typed-comparison-semantics/atomic-value)

(def ^:private operations #{:= :not= :< :<= :> :>= :in})
(def ^:private equality-operations #{:= :not=})
(def ^:private ordering-operations #{:< :<= :> :>=})
(def ^:private max-set-members 256)

(defn- reject!
  [reason at data]
  (throw (ex-info "Invalid independent typed-comparison semantics source."
                  (merge {:typed-comparison-semantics/rejection true
                          :reason reason :at at} data))))

(defn- plain-vector? [v]
  (instance? clojure.lang.APersistentVector v))

(defn- plain-set? [v]
  (instance? clojure.lang.PersistentHashSet v))

(defn- metadata-free! [v at]
  (when (and (instance? clojure.lang.IObj v) (some? (meta v)))
    (reject! :metadata-not-allowed at {}))
  v)

(defn- literal!
  [v at]
  (if (plain-set? v)
    (do
      (metadata-free! v at)
      (when-not (<= 1 (count v) max-set-members)
        (reject! :invalid-literal-domain at {}))
      (let [kinds (set (map values/value-kind! v))]
        (when-not (= 1 (count kinds))
          (reject! :heterogeneous-literal-domain at {:kinds kinds}))
        {:type :set :member-type (first kinds) :value v
         :origin :literal :nullable? false}))
    (if (set? v)
      (reject! :unsupported-set-representation at {})
      {:type (values/value-kind! v)
       :value v :origin :literal :nullable? false})))

(defn- operand!
  [expression reader-types observations at]
  (when-not (and (plain-vector? expression)
                 (= 2 (count expression)))
    (reject! :invalid-operand at {}))
  (metadata-free! expression at)
  (let [[op value] expression]
    (case op
      :literal (literal! value (conj at 1))
      :read
      (do
        (when-not (and (keyword? value) (some? (namespace value)))
          (reject! :invalid-reader-id at {}))
        (let [observation (get observations value)
              descriptor (get reader-types value)]
          (when-not descriptor
            (reject! :undeclared-reader at {:reader value}))
          (when (true? (:optional? descriptor))
            (reject! :unguarded-optional-reader at {:reader value}))
          (when-not (= :present (:presence observation))
            (reject! :absent-reader-evaluated at {:reader value}))
          {:type (:type descriptor)
           :name (:name descriptor)
           :domain (:values descriptor)
           :nullable? (true? (:nullable? descriptor))
           :origin :reader
           :value (:value observation)}))
      (reject! :unsupported-operand at {:operator op}))))

(defn- effective-kind [operand]
  (if (= :enum (:type operand)) :keyword (:type operand)))

(defn- compatible-equality?
  [left right]
  (let [a (effective-kind left)
        b (effective-kind right)]
    (or (and (= a b) (not= a :set)
             (cond
               (and (= :enum (:type left)) (= :enum (:type right)))
               (and (= (:name left) (:name right))
                    (= (:domain left) (:domain right)))
               (= :enum (:type left)) (= :literal (:origin right))
               (= :enum (:type right)) (= :literal (:origin left))
               :else true))
        (and (= :nil a) (:nullable? right))
        (and (= :nil b) (:nullable? left)))))

(defn- enum-literal-compatible!
  [left right at]
  (doseq [[enum-side literal-side] [[left right] [right left]]]
    (when (and (= :enum (:type enum-side))
               (= :literal (:origin literal-side))
               (= :keyword (:type literal-side))
               (not (contains? (:domain enum-side) (:value literal-side))))
      (reject! :enum-literal-outside-domain at {}))))

(defn- equal-values? [left right]
  ;; All admitted operands are strict scalars at this point. Unlike Clojure's
  ;; generalized equality, the admissibility of their *types* was checked
  ;; separately, before comparing their denotations.
  (= (:value left) (:value right)))

(defn- interpreted-value!
  [operator left right at]
  (cond
    (equality-operations operator)
    (do
      (when-not (compatible-equality? left right)
        (reject! :incompatible-equality-types at {}))
      (enum-literal-compatible! left right at)
      (if (= operator :=)
        (equal-values? left right)
        (not (equal-values? left right))))

    (ordering-operations operator)
    (do
      (when-not (and (= :integer (:type left))
                     (= :integer (:type right))
                     (not (:nullable? left))
                     (not (:nullable? right)))
        (reject! :expected-nonnullable-integers at {}))
      ;; Integer values are exact JVM integral scalars, not floating point.
      (let [relation (compare (:value left) (:value right))]
        (case operator
          :< (neg? relation)
          :<= (not (pos? relation))
          :> (pos? relation)
          :>= (not (neg? relation)))))

    (= :in operator)
    (do
      (when-not (and (= :set (:type right))
                     (= (effective-kind left) (:member-type right))
                     (not (:nullable? left)))
        (reject! :incompatible-membership-types at {}))
      (when (and (= :enum (:type left))
                 (not (set/subset? (:value right) (:domain left))))
        (reject! :enum-membership-outside-domain at {}))
      (contains? (:value right) (:value left)))

    :else (reject! :unsupported-operator at {:operator operator})))

(defn evaluate-atom!
  "Independently interpret one well-formed *direct* typed comparison.
   Revalidates the complete declared state via v1341, so missing required
   readers, extra keys and bad enum values fail closed even when the atom
   does not read them. A direct read of an optional field fails without
   a previously established presence guard, even if currently present.

   The result is diagnostic data, never a proof or admission handle."
  [expression reader-types state]
  (when-not (and (plain-vector? expression)
                 (= 3 (count expression)))
    (reject! :invalid-comparison-expression [:expression] {}))
  (metadata-free! expression [:expression])
  (let [operator (first expression)]
    (when-not (operations operator)
      (reject! :unsupported-operator [:expression] {:operator operator}))
    ;; This call rejects invalid reader descriptors and incomplete snapshots
    ;; independently of the production type checker and runtime evaluator.
    (let [observations (:fields (values/observe-state! reader-types state))
          left (operand! (second expression) reader-types observations [:expression 1])
          right (operand! (nth expression 2) reader-types observations [:expression 2])
          evaluated (interpreted-value! operator left right [:expression])]
      {:type result-type
       :version semantics-version
       :expression expression
       :value evaluated
       :semantics :independent-direct-atomic
       :checking :typed-value-and-operand-compatibility
       :proof-status :not-proved
       :admission :not-admitted
       :runtime-enforcement :not-established
       :jvm-cljs-equivalence :not-established
       :authority :none})))
