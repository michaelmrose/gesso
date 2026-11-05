(ns gesso.assurance.proof.typed-comparison-semantics-test
  "v1343: permanent, deterministic correspondence tests for the *direct* S1
   atomic comparison semantics introduced in v1342. The independent interpreter
   is checked against the separately implemented production rule evaluator on
   concrete admitted typed states. These are executable correspondences, not
   universal semantic proofs, browser/CLJS guarantees, certificates, or
   authenticated snapshots. Branch-sensitive presence/non-nil refinement is
   intentionally outside the direct-atom vocabulary."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.typed-comparison-semantics :as sem]
            [gesso.assurance.rule-eval :as production]))

(def ^:private int-d {:type :integer})
(def ^:private bool-d {:type :boolean})
(def ^:private string-d {:type :string})
(def ^:private keyword-d {:type :keyword})
(def ^:private status-d {:type :enum
                         :name :request/status
                         :values #{:request/new :request/claimed :request/done}})
(def ^:private a :sample/a)
(def ^:private b :sample/b)
(def ^:private big-positive 999999999999999999999999999999999999N)
(def ^:private big-negative -999999999999999999999999999999999999N)

(defn- production-report [expression reader-types state]
  (production/evaluate-rule!
   {:readers (set (keys reader-types)) :expression expression}
   reader-types state))

(defn- independent-report [expression reader-types state]
  (sem/evaluate-atom! expression reader-types state))

(defn- check-agreement!
  [expression reader-types state expected]
  (let [actual (independent-report expression reader-types state)
        prod (production-report expression reader-types state)
        context (pr-str [expression reader-types state])]
    (is (= expected (:value actual)) context)
    (is (= expected (:value prod)) context)
    (is (= (:value actual) (:value prod)) context)
    (is (instance? Boolean (:value actual)) context)
    (is (= :not-proved (:proof-status actual)) context)
    (is (= :not-admitted (:admission actual)) context)
    (is (= :none (:authority actual)) context)
    (is (not (contains? actual :certificate)) context)
    actual))

(defn- exception-data [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo ex (ex-data ex))))

(defn- reject-both!
  [expression reader-types state]
  (let [independent (exception-data #(independent-report expression reader-types state))
        production (exception-data #(production-report expression reader-types state))
        context (pr-str [expression reader-types state])]
    (is (map? independent) (str "Independent interpreter should reject " context))
    (is (map? production) (str "Production interpreter should reject " context))
    (when (map? independent)
      (is (or (true? (:typed-comparison-semantics/rejection independent))
              (true? (:typed-value-semantics/rejection independent)))
          (str "Independent typed rejection expected " context)))
    (when (map? production)
      (is (keyword? (:reason production)) context))))

(deftest full-literal-integer-ordering-small-domain
  (let [values [-3 -2 -1 0 1 2 3]]
    (doseq [x values, y values
            [op relation] [[:< <] [:<= <=] [:> >] [:>= >=]]]
      (check-agreement! [op [:literal x] [:literal y]] {} {}
                        (boolean (relation x y))))))

(deftest read-left-and-read-right-ordering
  (doseq [x [-5 -1 0 1 5]
          y [-4 0 4]
          [op relation] [[:< <] [:<= <=] [:> >] [:>= >=]]]
    (check-agreement! [op [:read a] [:literal y]] {a int-d} {a x}
                      (boolean (relation x y)))
    (check-agreement! [op [:literal x] [:read a]] {a int-d} {a y}
                      (boolean (relation x y)))))

(deftest read-read-integer-ordering-and-equality
  (doseq [x [-2 0 2] y [-2 0 2]
          [op relation] [[:< <] [:<= <=] [:> >] [:>= >=]
                         [:= =] [:not= not=]]]
    (check-agreement! [op [:read a] [:read b]]
                      {a int-d b int-d} {a x b y}
                      (boolean (relation x y)))))

(deftest mathematical-integers-cross-long-boundaries
  (let [values [big-negative (dec (bigint Long/MIN_VALUE))
                Long/MIN_VALUE -1N 0 1N Long/MAX_VALUE
                (inc (bigint Long/MAX_VALUE)) big-positive]]
    (doseq [x values y values
            [op relation] [[:< <] [:<= <=] [:> >] [:>= >=]
                           [:= =] [:not= not=]]]
      (check-agreement! [op [:literal x] [:literal y]] {} {}
                        (boolean (relation x y))))))

(deftest numeric-representation-does-not-change-integer-equality
  (doseq [[x y] [[0 0N] [7 7N] [Long/MAX_VALUE (bigint Long/MAX_VALUE)]
                 [9223372036854775808N (bigint "9223372036854775808")]
                 [-5N -5]]]
    (check-agreement! [:= [:literal x] [:literal y]] {} {} true)
    (check-agreement! [:not= [:literal x] [:literal y]] {} {} false)))

(deftest scalar-literal-equality-and-inequality
  (doseq [values [[true false] ["" "hi" "there"]
                  [:request/new :request/done] [nil]]
          x values y values]
    (check-agreement! [:= [:literal x] [:literal y]] {} {} (= x y))
    (check-agreement! [:not= [:literal x] [:literal y]] {} {} (not= x y))))

(deftest scalar-reader-equality-and-inequality
  (doseq [[descriptor values] [[bool-d [true false]]
                               [string-d ["" "a" "xy"]]
                               [keyword-d [:request/new :request/done]]
                               [int-d [-2 0 2]]]
          x values y values
          op [:= :not=]]
    (check-agreement! [op [:read a] [:literal y]]
                      {a descriptor} {a x}
                      (if (= op :=) (= x y) (not= x y)))))

(deftest scalar-two-reader-equality
  (doseq [[descriptor values] [[bool-d [true false]]
                               [string-d ["a" "b"]]
                               [keyword-d [:request/new :request/done]]]
          x values y values]
    (check-agreement! [:= [:read a] [:read b]]
                      {a descriptor b descriptor} {a x b y}
                      (= x y))))

(deftest finite-integer-membership
  (let [domains [#{0} #{-2 2} #{-3 -1 0 1 3}]]
    (doseq [domain domains x [-4 -3 -2 -1 0 1 2 3 4]]
      (check-agreement! [:in [:read a] [:literal domain]]
                        {a int-d} {a x}
                        (contains? domain x))
      (check-agreement! [:in [:literal x] [:literal domain]]
                        {} {}
                        (contains? domain x)))))

(deftest finite-boolean-string-and-keyword-membership
  (doseq [[descriptor domain samples]
          [[bool-d #{true} [false true]]
           [string-d #{"red" "blue"} ["red" "blue" "green"]]
           [keyword-d #{:request/new :request/done}
            [:request/new :request/claimed :request/done]]]
          value samples]
    (check-agreement! [:in [:read a] [:literal domain]]
                      {a descriptor} {a value}
                      (contains? domain value))))

(deftest literal-nil-membership-has-explicit-kind
  ;; This is accepted for a literal, not a potentially nullable reader.
  (check-agreement! [:in [:literal nil] [:literal #{nil}]] {} {} true)
  (reject-both! [:in [:literal nil] [:literal #{true}]] {} {}))

(deftest nominal-enum-literal-equality-and-domain-subset
  (doseq [value [:request/new :request/claimed :request/done]
          candidate [:request/new :request/claimed :request/done]]
    (check-agreement! [:= [:read a] [:literal candidate]]
                      {a status-d} {a value} (= value candidate)))
  (doseq [subset-domain [#{:request/new}
                         #{:request/new :request/done}
                         #{:request/new :request/claimed :request/done}]
          value [:request/new :request/claimed :request/done]]
    (check-agreement! [:in [:read a] [:literal subset-domain]]
                      {a status-d} {a value}
                      (contains? subset-domain value))))

(deftest nominal-enum-reader-identity
  (let [compatible {a status-d b status-d}
        same-value {a :request/new b :request/new}
        different-value {a :request/new b :request/done}]
    (check-agreement! [:= [:read a] [:read b]] compatible same-value true)
    (check-agreement! [:= [:read a] [:read b]] compatible different-value false)
    (check-agreement! [:not= [:read a] [:read b]] compatible different-value true)
    (reject-both! [:= [:read a] [:read b]]
                  {a status-d
                   b (assoc status-d :name :other/status)}
                  same-value)
    (reject-both! [:= [:read a] [:read b]]
                  {a status-d
                   b (assoc status-d :values #{:request/new :request/done})}
                  same-value)))

(deftest enum-keyword-reader-is-not-implicitly-an-enum
  ;; Two arbitrary keyword readers are not a nominal enum comparison.
  ;; Their primitive keyword equality is separately well typed.
  (check-agreement! [:= [:read a] [:read b]]
                    {a keyword-d b keyword-d}
                    {a :request/new b :request/new} true)
  (check-agreement! [:= [:read a] [:read b]]
                    {a keyword-d b keyword-d}
                    {a :request/new b :request/done} false))

(deftest declared-nil-equality-for-nullable-reader
  (let [descriptor {:type :integer :nullable? true}]
    (doseq [value [nil 0 7 big-positive]]
      (check-agreement! [:= [:read a] [:literal nil]]
                        {a descriptor} {a value} (nil? value))
      (check-agreement! [:not= [:read a] [:literal nil]]
                        {a descriptor} {a value} (some? value))
      (check-agreement! [:= [:literal nil] [:read a]]
                        {a descriptor} {a value} (nil? value)))))

(deftest optional-reader-requires-control-flow-refinement
  (doseq [value [nil 0 7]
          state [{} {a value}]]
    (reject-both! [:= [:read a] [:literal nil]]
                  {a {:type :integer :optional? true :nullable? true}} state)
    (reject-both! [:> [:read a] [:literal 0]]
                  {a {:type :integer :optional? true}} state)))

(deftest nullable-reader-ordering-and-membership-need-nonnil-refinement
  (doseq [value [nil -1 0 10]]
    (reject-both! [:> [:read a] [:literal 0]]
                  {a {:type :integer :nullable? true}} {a value})
    (reject-both! [:in [:read a] [:literal #{-1 0 1}]]
                  {a {:type :integer :nullable? true}} {a value})))

(deftest invalid-operand-and-domain-types-are-rejected
  (doseq [expr [[:= [:literal 7] [:literal "7"]]
                [:not= [:literal true] [:literal 1]]
                [:< [:literal false] [:literal 1]]
                [:>= [:literal 5] [:literal nil]]
                [:in [:literal 5] [:literal #{"5"}]]
                [:in [:literal :request/new] [:literal #{1 2}]]
                [:in [:literal 7] [:literal #{7 "7"}]]
                [:in [:literal 3] [:literal #{}]]
                [:in [:literal 4] [:literal (sorted-set 3 4)]]
                [:unknown [:literal 1] [:literal 2]]]]
    (reject-both! expr {} {})))

(deftest invalid-enum-source-is-rejected
  (doseq [expr [[:= [:read a] [:literal :other/unlisted]]
                [:in [:read a] [:literal #{:other/unlisted}]]]]
    (reject-both! expr {a status-d} {a :request/new}))
  (reject-both! [:= [:read a] [:literal :request/new]]
                {a status-d} {a :other/unlisted}))

(deftest malformed-reader-and-closed-state-rejections
  (reject-both! [:= [:read a] [:literal true]]
                {a bool-d} {})
  (reject-both! [:= [:read a] [:literal true]]
                {a bool-d} {a true :extra/field 1})
  (reject-both! [:= [:read a] [:literal true]]
                {a bool-d} {a nil})
  (reject-both! [:= [:read a] [:literal true]]
                {a {:type :boolean :nullable? :yes}} {a true})
  (reject-both! [:= [:read a] [:literal true]]
                {a {:type :float}} {a true})
  ;; v1342 interprets the entire supplied state footprint, but does not
  ;; own v1219's exact *syntactic usage* check. Production rejects an unused
  ;; declared reader; independent semantics is not an admission validator.
  (let [expression [:= [:read a] [:literal true]]
        types {a bool-d b bool-d}
        state {a true b true}]
    (is (true? (:value (independent-report expression types state))))
    (is (map? (exception-data #(production-report expression types state)))))
  (reject-both! [:= [:read :other/undeclared] [:literal true]] {} {}))

(deftest invalid-executable-and-metadata-inputs
  (reject-both! [:= [:literal (fn [] 1)] [:literal 1]] {} {})
  (reject-both! [:= [:literal 3.5] [:literal 3.5]] {} {})
  (reject-both! (with-meta [:= [:literal 1] [:literal 1]] {:unsafe true}) {} {})
  (reject-both! [:= (with-meta [:literal 1] {:unsafe true}) [:literal 1]] {} {})
  (reject-both! [:= [:read a] [:literal true]]
                {a bool-d} (with-meta {a true} {:unsafe true})))

(deftest diagnostic-reports-never-grant-proof-or-transaction-authority
  (let [report (check-agreement! [:>= [:read a] [:literal 2]]
                                 {a int-d} {a 3} true)]
    (is (= sem/result-type (:type report)))
    (is (= sem/semantics-version (:version report)))
    (is (= :independent-direct-atomic (:semantics report)))
    (is (= :not-established (:runtime-enforcement report)))
    (is (= :not-established (:jvm-cljs-equivalence report)))
    (is (not (contains? report :proof)))
    (is (not (contains? report :handle)))
    (is (not (contains? report :committed?)))
    (is (= report (independent-report [:>= [:read a] [:literal 2]]
                                    {a int-d} {a 3})))))
