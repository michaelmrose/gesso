(ns gesso.assurance.rule-eval
  "Pure, closed interpreter for v1219 expressions checked by v1221 types.

   (evaluate-rule! declaration reader-types bindings) requires:
   * The exact v1219 declarative rule and v1221 typed reader footprint;
   * A PLAIN persistent map with exactly the required reader bindings and any
     PRESENT optional bindings (no undeclared keys);
   * Scalar runtime values satisfying their declared nonnullable, nominal-enum,
     and mathematical-integer contracts. Missing and present-nil are distinct.

   Interpretation is strict Boolean, with left-to-right short-circuit :and,
   :or, and :implies. :present tests whether a key exists, NEVER truthiness;
   :read of an absent field fails closed. Integer order is exact over Clojure
   integer values, including BigInt/BigInteger (no fixed-width arithmetic).

   This is a pure value interpreter, NOT a runtime admission gate: an
   evaluation of true is neither a theorem, a certificate, an implementation
   correspondence claim, nor permission to commit a transaction. The supplied
   map is assumed to represent a trustworthy snapshot: this namespace cannot
   establish where it came from or isolate opaque computations and effects."
  (:require [clojure.set :as set]
            [gesso.assurance.rule-type :as rule-type]))

(def evaluator-version 1)
(def evaluated-type :gesso.assurance.rule-eval/interpreted-value)

(def ^:private max-text-length 2048)

(defn- reject!
  [reason at data]
  (throw (ex-info "Invalid Gesso assurance rule evaluation."
                  (merge {:error/type ::invalid-rule-evaluation
                          :reason reason
                          :at at}
                         data))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- metadata-free! [x at]
  (when (and (instance? clojure.lang.IObj x) (some? (meta x)))
    (reject! :metadata-not-allowed at {})))

(defn- value-type [value]
  (cond
    (nil? value) :nil
    (instance? Boolean value) :boolean
    (integer? value) :integer
    (string? value) :string
    (keyword? value) :keyword
    :else :unsupported))

(defn- bounded-scalar! [id value]
  (let [at [:bindings id]
        kind (value-type value)]
    (when (= kind :unsupported)
      (reject! :unsupported-binding-value at {:value-type (some-> value class str)}))
    (when (and (#{:integer :string :keyword} kind)
               (> (count (str value)) max-text-length))
      (reject! :binding-size-limit at {:maximum max-text-length}))
    (when (instance? clojure.lang.IObj value)
      (metadata-free! value at))
    kind))

(defn- check-bindings!
  [checked bindings]
  (when-not (plain-map? bindings)
    (reject! :expected-binding-map [:bindings] {}))
  (metadata-free! bindings [:bindings])
  (let [types (:reader-types checked)
        readers (:readers checked)
        provided (set (keys bindings))
        extra (set/difference provided readers)
        absent (set/difference readers provided)
        missing (set (remove #(true? (:optional? (get types %))) absent))]
    (when (seq extra)
      (reject! :undeclared-runtime-readers [:bindings] {:unexpected extra}))
    (when (seq missing)
      (reject! :missing-required-bindings [:bindings] {:missing missing}))
    (doseq [[id value] bindings]
      (let [descriptor (get types id)
            actual (bounded-scalar! id value)
            expected (:type descriptor)]
        (when (and (= :nil actual) (not (:nullable? descriptor)))
          (reject! :nil-for-nonnullable-binding [:bindings id] {:reader id}))
        (when (and (not= :nil actual)
                   (not= actual (if (= expected :enum) :keyword expected)))
          (reject! :wrong-binding-type [:bindings id]
                   {:reader id :expected expected :actual actual}))
        (when (and (= expected :enum) (not= :nil actual)
                   (not (contains? (:values descriptor) value)))
          (reject! :enum-value-outside-domain [:bindings id]
                   {:reader id})))))
  bindings)

(defn- strict-boolean! [value at]
  (when-not (instance? Boolean value)
    (reject! :nonboolean-evaluation at {:value-type (value-type value)}))
  value)

(declare interpret!)

(defn- interpret!
  [expression bindings at]
  (let [[op a b] expression]
    (case op
      :literal a
      :read (if (contains? bindings a)
              (get bindings a)
              (reject! :absent-reader-evaluated at {:reader a}))
      ;; The expression operand was syntactically restricted to a direct read.
      ;; No value is read, so a missing binding and a nil binding are distinct.
      :present (contains? bindings (second a))
      :not (not (strict-boolean! (interpret! a bindings (conj at 1))
                                 (conj at 1)))
      :and (loop [index 1]
             (if (= index (count expression))
               true
               (if (strict-boolean!
                    (interpret! (nth expression index) bindings (conj at index))
                    (conj at index))
                 (recur (inc index))
                 false)))
      :or (loop [index 1]
            (if (= index (count expression))
              false
              (if (strict-boolean!
                   (interpret! (nth expression index) bindings (conj at index))
                   (conj at index))
                true
                (recur (inc index)))))
      :implies (if (strict-boolean! (interpret! a bindings (conj at 1))
                                     (conj at 1))
                 (strict-boolean! (interpret! b bindings (conj at 2))
                                  (conj at 2))
                 true)
      := (= (interpret! a bindings (conj at 1))
            (interpret! b bindings (conj at 2)))
      :not= (not= (interpret! a bindings (conj at 1))
                   (interpret! b bindings (conj at 2)))
      :< (neg? (compare (interpret! a bindings (conj at 1))
                        (interpret! b bindings (conj at 2))))
      :<= (not (pos? (compare (interpret! a bindings (conj at 1))
                             (interpret! b bindings (conj at 2)))))
      :> (pos? (compare (interpret! a bindings (conj at 1))
                        (interpret! b bindings (conj at 2))))
      :>= (not (neg? (compare (interpret! a bindings (conj at 1))
                             (interpret! b bindings (conj at 2)))))
      :in (contains? (interpret! b bindings (conj at 2))
                     (interpret! a bindings (conj at 1)))
      (reject! :unrecognized-interpreter-operator at {:operator op}))))

(defn evaluate-rule!
  "Evaluate the Boolean value of a CLOSED, well-typed rule over a declared
   binding snapshot. Rechecks syntax AND types every time; copied check reports
   are not accepted as proof or admission handles.

   Returns a copyable observation report with :value true/false and explicit
   non-proof, non-admission, non-enforcement classification. The evaluated
   Boolean is not authoritative transaction acceptance. Throws typed ex-info
   for malformed syntax/types, missing/wrong binding values, or unexpected
   interpreter failure modes. It never executes callbacks."
  [declaration reader-types bindings]
  (let [checked (rule-type/check-rule-type! declaration reader-types)
        _ (check-bindings! checked bindings)
        value (strict-boolean!
               (interpret! (:expression checked) bindings [:expression])
               [:expression])]
    {:type evaluated-type
     :version evaluator-version
     :rule-version (:rule-version checked)
     :type-version (:version checked)
     :readers (:readers checked)
     :expression (:expression checked)
     :value value
     :checking :syntax-and-types
     :evaluation :pure-interpreted
     :proof-status :not-proved
     :admission :not-admitted
     :runtime-enforcement :not-established
     :authority :none}))
