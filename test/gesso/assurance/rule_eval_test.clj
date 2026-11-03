(ns gesso.assurance.rule-eval-test
  "v1224: permanent, adversarial regression coverage for the v1223 pure
   interpreter. Rechecked syntax, nominal types, precise absence/nil
   semantics, guarded short-circuit order, exact integer comparisons, and
   rejection of hostile binding snapshots are required. The interpreter's
   Boolean observation is not a proof, an admitted assertion, an authoritative
   database read, or permission to commit."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.rule-eval :as rule-eval]))

(def ^:private score :result/score)
(def ^:private status :result/status)
(def ^:private active :result/active)
(def ^:private label :result/label)
(def ^:private int-type {:type :integer})
(def ^:private optional-int {:type :integer :optional? true})
(def ^:private optional-nullable-int
  {:type :integer :optional? true :nullable? true})
(def ^:private enum-status
  {:type :enum :name :example/request-status
   :values #{:new :active :done}})

(defn- declaration [expression types]
  {:readers (set (keys types)) :expression expression})

(defn- observed! [expression types bindings]
  (rule-eval/evaluate-rule! (declaration expression types) types bindings))

(defn- value! [expression types bindings]
  (:value (observed! expression types bindings)))

(defn- rejection [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- rejects!
  ([expression types bindings error-type reason]
   (let [data (rejection #(observed! expression types bindings))]
     (is (map? data) (str "Expected typed rejection of " (pr-str expression)))
     (is (= error-type (:error/type data)) (pr-str data))
     (is (= reason (:reason data)) (pr-str data))
     (is (vector? (:at data)) (pr-str data))
     data)))

(defn- rejects-runtime! [expression types bindings reason]
  (rejects! expression types bindings
            :gesso.assurance.rule-eval/invalid-rule-evaluation reason))

(defn- rejects-type! [expression types bindings reason]
  (rejects! expression types bindings
            :gesso.assurance.rule-type/invalid-rule-type reason))

(deftest report-is-a-copyable-observation-not-an-admission-or-proof
  (let [expr [:literal true]
        result (observed! expr {} {})]
    (is (= true (:value result)))
    (is (= rule-eval/evaluated-type (:type result)))
    (is (= rule-eval/evaluator-version (:version result)))
    (is (= 1 (:rule-version result)))
    (is (= 1 (:type-version result)))
    (is (= #{} (:readers result)))
    (is (= expr (:expression result)))
    (is (= :syntax-and-types (:checking result)))
    (is (= :pure-interpreted (:evaluation result)))
    (is (= :not-proved (:proof-status result)))
    (is (= :not-admitted (:admission result)))
    (is (= :not-established (:runtime-enforcement result)))
    (is (= :none (:authority result)))
    (is (= #{:type :version :rule-version :type-version :readers :expression
             :value :checking :evaluation :proof-status :admission
             :runtime-enforcement :authority}
           (set (keys result))))
    (is (= result (observed! expr {} {})))
    (doseq [forbidden [:certificate :proof :handle :valid? :proved?
                       :discharged? :committed? :enforced?]]
      (is (not (contains? result forbidden))))
    (is (= :not-admitted (:admission (assoc result :admission :not-admitted))))
    (is (= :proved (:proof-status (assoc result :proof-status :proved))
           ) "Copyable maps can be forged; no code must treat them as authority")))

(deftest closed-boolean-operator-truth-tables
  (doseq [a [false true] b [false true]]
    (testing (str a " and " b)
      (is (= (and a b) (value! [:and [:literal a] [:literal b]] {} {})))
      (is (= (or a b) (value! [:or [:literal a] [:literal b]] {} {})))
      (is (= (or (not a) b)
             (value! [:implies [:literal a] [:literal b]] {} {})))
      (is (= (= a b) (value! [:= [:literal a] [:literal b]] {} {})))
      (is (= (not= a b) (value! [:not= [:literal a] [:literal b]] {} {})))))
  (is (true? (value! [:not [:literal false]] {} {})))
  (is (false? (value! [:not [:literal true]] {} {})))
  (is (true? (value! [:and [:literal true] [:literal true]
                      [:literal true]] {} {})))
  (is (false? (value! [:and [:literal true] [:literal false]
                       [:literal true]] {} {})))
  (is (true? (value! [:or [:literal false] [:literal false]
                     [:literal true]] {} {})))
  (is (false? (value! [:or [:literal false] [:literal false]
                      [:literal false]] {} {}))))

(deftest equality-and-inequality-obey-declared-types
  (doseq [[a b] [[7 7N] [0 0N] [-17 -17N] [99N 100]
                 ["ok" "ok"] ["ok" "no"] [:ready :ready]
                 [:ready :other] [nil nil]]]
    (let [equal? (= a b)]
      (is (= equal? (value! [:= [:literal a] [:literal b]] {} {})))
      (is (= (not equal?) (value! [:not= [:literal a] [:literal b]] {} {})))))
  (rejects-type! [:= [:literal 7] [:literal "7"]] {} {}
                 :incompatible-equality-types))

(deftest exact-integer-order-truth-tables
  (let [values [-9223372036854775809N -9223372036854775808N
                -100 -1 0 1 7 8 999999999999999999999999999999N]]
    (doseq [a values b values]
      (is (= (< a b) (value! [:< [:literal a] [:literal b]] {} {})))
      (is (= (<= a b) (value! [:<= [:literal a] [:literal b]] {} {})))
      (is (= (> a b) (value! [:> [:literal a] [:literal b]] {} {})))
      (is (= (>= a b) (value! [:>= [:literal a] [:literal b]] {} {}))))))

(deftest binding-integer-comparisons-are-exact-across-long-boundaries
  (let [expr [:> [:read score] [:literal 7]]
        types {score int-type}]
    (doseq [v [-9999999999999999999999999999N -9N -1 0 6 7 7N 8N
               Long/MAX_VALUE (inc (bigint Long/MAX_VALUE))
               999999999999999999999999999999N]]
      (is (= (> v 7) (value! expr types {score v})) (str v)))
    (is (true? (value! [:= [:read score] [:literal 7N]]
                       types {score 7})))
    (is (false? (value! [:< [:read score] [:literal 7]]
                        types {score 7N})))))

(deftest finite-membership-restricts-results
  (doseq [v [:new :active :done :cancelled]]
    (is (= (contains? #{:new :active :done} v)
           (value! [:in [:literal v] [:literal #{:new :active :done}]]
                   {} {}))))
  (doseq [v [-3 -1 0 1 7 8 9]]
    (is (= (contains? #{-1 0 7 9} v)
           (value! [:in [:literal v] [:literal #{-1 0 7 9}]]
                   {} {}))))
  (is (true? (value! [:in [:literal nil] [:literal #{nil}]] {} {})))
  (is (false? (value! [:in [:literal true] [:literal #{false}]] {} {}))))

(deftest nominal-enum-readers-validate-the-whole-runtime-domain
  (let [types {status enum-status}
        expr [:in [:read status] [:literal #{:new :active}]]]
    (is (true? (value! expr types {status :new})))
    (is (true? (value! expr types {status :active})))
    (is (false? (value! expr types {status :done})))
    (rejects-runtime! expr types {status :deleted} :enum-value-outside-domain)
    (rejects-runtime! expr types {status "active"} :wrong-binding-type)
    (rejects-runtime! expr types {status 0} :wrong-binding-type)))

(deftest present-is-presence-not-truthiness-or-non-nil
  (let [read [:read active]
        expr [:present read]
        types {active {:type :boolean :optional? true :nullable? true}}]
    (is (false? (value! expr types {})))
    (is (true? (value! expr types {active false})))
    (is (true? (value! expr types {active true})))
    (is (true? (value! expr types {active nil}))))
  (let [expr [:present [:read label]]
        types {label {:type :string :optional? true :nullable? true}}]
    (is (false? (value! expr types {})))
    (is (true? (value! expr types {label ""})))
    (is (true? (value! expr types {label nil})))))

(deftest guarded-optional-integer-handles-missing-and-present-nil
  (let [r [:read score]
        types {score optional-nullable-int}
        expr [:and [:present r]
              [:not= r [:literal nil]]
              [:> r [:literal 7]]]]
    (is (false? (value! expr types {})))
    (is (false? (value! expr types {score nil})))
    (is (false? (value! expr types {score 7})))
    (is (true? (value! expr types {score 8})))
    (is (true? (value! expr types {score 999999999999999999999999N}))))
  (let [r [:read score]
        types {score optional-int}
        expr [:and [:present r] [:> r [:literal 7]]]]
    (is (false? (value! expr types {})))
    (is (false? (value! expr types {score 0})))
    (is (true? (value! expr types {score 8})))
    (rejects-runtime! expr types {score nil} :nil-for-nonnullable-binding)))

(deftest guarded-or-evaluates-the-right-branch-only-when-present
  (let [r [:read score]
        types {score optional-int}
        expr [:or [:not [:present r]] [:> r [:literal 7]]]]
    (is (true? (value! expr types {})))
    (is (false? (value! expr types {score 1})))
    (is (true? (value! expr types {score 8})))))

(deftest guarded-implication-does-not-read-missing-fields
  (let [r [:read score]
        types {score optional-int}
        expr [:implies [:present r] [:> r [:literal 7]]]]
    (is (true? (value! expr types {})))
    (is (false? (value! expr types {score 0})))
    (is (true? (value! expr types {score 8})))))

(deftest nullable-required-field-can-be-refined-before-order-comparison
  (let [r [:read score]
        types {score {:type :integer :nullable? true}}
        expr [:implies [:not= r [:literal nil]] [:> r [:literal 7]]]]
    (is (true? (value! expr types {score nil})))
    (is (false? (value! expr types {score 7})))
    (is (true? (value! expr types {score 8})))))

(deftest incorrect-guard-order-is-rejected-at-type-check-time
  (let [r [:read score]
        types {score optional-int}]
    (rejects-type! [:and [:> r [:literal 7]] [:present r]] types {}
                   :unguarded-optional-reader)
    (rejects-type! [:or [:> r [:literal 7]] [:not [:present r]]]
                   types {} :unguarded-optional-reader)
    (rejects-type! [:implies [:> r [:literal 7]] [:present r]]
                   types {} :unguarded-optional-reader)))

(deftest unguarded-nullable-ordering-is-rejected
  (let [r [:read score]
        types {score {:type :integer :nullable? true}}]
    (rejects-type! [:> r [:literal 7]] types {score 8}
                   :expected-nonnullable-integers)))

(deftest runtime-bindings-exactly-match-the-declared-reader-footprint
  (let [types {score int-type}
        expr [:> [:read score] [:literal 7]]]
    (rejects-runtime! expr types {} :missing-required-bindings)
    (rejects-runtime! expr types {score 8 :extra/field 3}
                      :undeclared-runtime-readers)
    (rejects-runtime! expr types {score nil} :nil-for-nonnullable-binding)
    (rejects-runtime! expr types {score "8"} :wrong-binding-type)
    (rejects-runtime! expr types {score false} :wrong-binding-type)
    (is (true? (value! expr types {score 8})))))

(deftest each-supported-nominal-runtime-type-is-checked
  (let [cases [[{:type :boolean} true :not-a-boolean]
               [{:type :string} "yes" :yes]
               [{:type :keyword} :yes "yes"]
               [{:type :integer} 5 5.0]
               [enum-status :new :not-in-domain]]]
    (doseq [[descriptor valid invalid] cases]
      (let [types {active descriptor}
            expr [:= [:read active] [:read active]]]
        (is (true? (value! expr types {active valid})))
        (rejects-runtime! expr types {active invalid}
                          (cond
                            (and (= :enum (:type descriptor))
                                 (keyword? invalid)) :enum-value-outside-domain
                            (number? invalid) :unsupported-binding-value
                            :else :wrong-binding-type))))))

(deftest nonplain-binding-maps-cannot-run-comparators-or-custom-accessors
  (let [expr [:literal true]]
    (doseq [bindings [(sorted-map) (java.util.HashMap.) nil []
                      (reify clojure.lang.ILookup
                        (valAt [_ _] (throw (ex-info "accessed" {})))
                        (valAt [_ _ _] (throw (ex-info "accessed" {}))))]]
      (rejects-runtime! expr {} bindings :expected-binding-map))))

(deftest metadata-is-not-proof-and-cannot-enter-bindings
  (let [expr [:literal true]]
    (rejects-runtime! expr {} (with-meta {} {:proved? true})
                      :metadata-not-allowed)
    (rejects-runtime! expr {} (with-meta {} {:kernel-checked true})
                      :metadata-not-allowed)))

(deftest arbitrary-functions-and-unsupported-binding-values-never-execute
  (let [called (atom 0)
        callback (fn [] (swap! called inc) 8)
        expr [:present [:read score]]
        types {score {:type :integer :optional? true}}]
    (rejects-runtime! expr types {score callback} :unsupported-binding-value)
    (is (= 0 @called))
    (rejects-runtime! expr types {score {:call callback}} :unsupported-binding-value)
    (is (= 0 @called))))

(deftest unsupported-numeric-values-are-rejected-instead-of-coerced
  (let [expr [:> [:read score] [:literal 7]]
        types {score int-type}]
    (doseq [value [7.0 7.1M 7/2 (double 8) (int-array [8])]]
      (rejects-runtime! expr types {score value} :unsupported-binding-value))
    (rejects-runtime! expr types {score "8"} :wrong-binding-type)
    ;; Ratios whose denominator is one normalize to genuine integers.
    (is (true? (value! expr types {score 8})))))

(deftest oversized-binding-scalars-fail-closed
  (let [large-text (apply str (repeat 2049 "x"))
        large-integer (bigint (apply str (repeat 2049 "9")))
        expr [:= [:read label] [:literal "ok"]]
        types {label {:type :string}}]
    (rejects-runtime! expr types {label large-text} :binding-size-limit)
    (rejects-runtime! [:> [:read score] [:literal 7]]
                      {score int-type} {score large-integer}
                      :binding-size-limit)))

(deftest wrong-declarations-recheck-syntax-and-types-before-evaluation
  (let [expr [:> [:read score] [:literal 7]]
        types {score int-type}]
    (rejects-type! [:= [:read score] [:literal "seven"]]
                   types {score 8} :incompatible-equality-types)
    (rejects-type! [:> [:read status] [:literal 7]]
                   {status enum-status} {status :new}
                   :expected-nonnullable-integers)
    (let [bad-decl (assoc (declaration expr types) :proved? true)
          result (rejection #(rule-eval/evaluate-rule! bad-decl types {score 8}))]
      (is (= :gesso.assurance.rule/invalid-rule (:error/type result)))
      (is (= :wrong-declaration-keys (:reason result))))))

(deftest valid-composition-of-enum-membership-and-integer-guard
  (let [types {status enum-status score int-type}
        expr [:and
              [:in [:read status] [:literal #{:new :active}]]
              [:> [:read score] [:literal 7]]]]
    (doseq [s [:new :active :done]
            n [0 7 8 9 9999999999999999N]]
      (is (= (and (contains? #{:new :active} s) (> n 7))
             (value! expr types {status s score n}))))))

(deftest pure-evaluation-does-not-claim-snapshot-or-transaction-correspondence
  (let [expr [:> [:read score] [:literal 7]]
        report (observed! expr {score int-type} {score 8})]
    (is (true? (:value report)))
    (is (= :not-proved (:proof-status report)))
    (is (= :not-admitted (:admission report)))
    (is (= :not-established (:runtime-enforcement report)))
    (is (= :none (:authority report)))
    (is (nil? (:commit-status report)))
    (is (nil? (:transaction-id report)))))
