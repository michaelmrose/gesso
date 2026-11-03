(ns gesso.assurance.rule-type-test
  "Permanent regression tests for the v1221 closed typed-expression checker.

   These tests deliberately distinguish checked typing from evaluation,
   proposition proof, and admission. No test in this namespace issues evidence."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.rule-type :as rule-type]))

(def status {:type :enum
             :name :humanhelp/request-status
             :values #{:new :active :done}})
(def int-type {:type :integer})
(def bool-type {:type :boolean})
(def optional-int {:type :integer :optional? true})
(def nullable-int {:type :integer :nullable? true})
(def optional-nullable-int {:type :integer :optional? true :nullable? true})

(defn- declare [expr types]
  {:readers (set (keys types)) :expression expr})

(defn- type-check! [expr types]
  (rule-type/check-rule-type! (declare expr types) types))

(defn- rejection [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- rejects! [expr types reason]
  (let [data (rejection #(type-check! expr types))]
    (is (map? data) (str "expected typed rejection: " (pr-str expr)))
    (is (= :gesso.assurance.rule-type/invalid-rule-type (:error/type data))
        (pr-str data))
    (is (= reason (:reason data)) (pr-str data))
    (is (vector? (:at data)) (pr-str data))))

(defn- accepted! [expr types]
  (let [decl (declare expr types)
        report (type-check! expr types)]
    (is (= :gesso.assurance.rule-type/checked-types (:type report)))
    (is (= 1 (:version report)))
    (is (= 1 (:rule-version report)))
    (is (= :syntax-and-types (:checking report)))
    (is (= :not-performed (:evaluation report)))
    (is (= :not-proved (:proof-status report)))
    (is (= :not-admitted (:admission report)))
    (is (= :none (:authority report)))
    (is (= :boolean (:result-type report)))
    (is (= (:expression decl) (:expression report)))
    (is (= (:readers decl) (:readers report)))
    (is (= types (:reader-types report)))
    (is (= #{:type :version :rule-version :expression :readers :reader-types
             :result-type :checking :evaluation :proof-status :admission :authority}
           (set (keys report))))
    report))

(deftest type-check-is-descriptive-not-proof-or-runtime-admission
  (let [report (accepted! [:literal true] {})]
    (is (= report (type-check! [:literal true] {})))
    (doseq [k [:certificate :valid? :proved? :discharged? :enforced? :handle]]
      (is (not (contains? report k))))))

(deftest all-supported-boolean-operators-and-comparisons
  (doseq [expr [[:literal true] [:literal false]
                [:read :result/ok]
                [:present [:read :result/ok]]
                [:not [:literal false]]
                [:and [:literal true] [:literal false] [:literal true]]
                [:or [:literal false] [:literal true]]
                [:implies [:literal true] [:literal false]]
                [:= [:literal 4] [:literal 4N]]
                [:not= [:literal 4] [:literal 5]]
                [:< [:literal -1] [:literal 7]]
                [:<= [:literal 7] [:literal 7]]
                [:> [:literal 9N] [:literal 7]]
                [:>= [:literal 8] [:literal 7]]
                [:in [:literal 8] [:literal #{7 8 9}]]]]
    (testing (pr-str expr)
      (let [reads (if (re-find #":result/ok" (pr-str expr))
                    {:result/ok bool-type} {})]
        (is (= :syntax-and-types (:checking (type-check! expr reads))))))))

(deftest top-level-expression-must-be-nonnullable-boolean
  (doseq [[expr types] [[[:literal 7] {}]
                        [[:literal nil] {}]
                        [[:literal :active] {}]
                        [[:read :result/count] {:result/count int-type}]
                        [[:read :result/ok] {:result/ok {:type :boolean :nullable? true}}]
                        [[:and [:literal true] [:literal 7]] {}]
                        [[:not [:literal 3]] {}]
                        [[:implies [:literal false] [:literal 2]] {}]]]
    (testing (pr-str expr)
      (rejects! expr types :expected-nonnullable-boolean))))

(deftest enum-domain-membership-and-equality
  (doseq [expr [[:in [:read :result/status] [:literal #{:new :done}]]
                [:= [:read :result/status] [:literal :active]]
                [:not= [:read :result/status] [:literal :new]]
                [:in [:literal :new] [:literal #{:new :done}]]]]
    (accepted! expr (if (re-find #":read " (pr-str expr))
                      {:result/status status} {}))))

(deftest finite-enum-domains-reject-out-of-domain-expressions
  (rejects! [:= [:read :result/status] [:literal :invalid]]
            {:result/status status} :enum-literal-outside-domain)
  (rejects! [:in [:read :result/status] [:literal #{:done :invalid}]]
            {:result/status status} :enum-domain-not-subset)
  (rejects! [:> [:read :result/status] [:literal 7]]
            {:result/status status} :expected-nonnullable-integers)
  (rejects! [:= [:read :result/status] [:literal 7]]
            {:result/status status} :incompatible-equality-types))

(deftest nominal-equality-refuses-unrelated-enumerations
  (let [expr [:= [:read :left/status] [:read :right/status]]
        left status
        right (assoc status :name :another/status)]
    (accepted! expr {:left/status left :right/status left})
    (rejects! expr {:left/status left :right/status right}
              :incompatible-equality-types)
    (rejects! expr {:left/status left :right/status (assoc status :values #{:new :done})}
              :incompatible-equality-types)
    (rejects! expr {:left/status left :right/status {:type :keyword}}
              :incompatible-equality-types)))

(deftest unequal-primitive-types-and-nonnumeric-ordering-rejected
  (doseq [[expr reason] [[[:= [:literal 7] [:literal "7"]] :incompatible-equality-types]
                         [[:= [:literal 7] [:literal true]] :incompatible-equality-types]
                         [[:< [:literal :new] [:literal 7]] :expected-nonnullable-integers]
                         [[:<= [:literal "8"] [:literal "9"]] :expected-nonnullable-integers]
                         [[:>= [:literal nil] [:literal 0]] :expected-nonnullable-integers]
                         [[:in [:literal :new] [:literal #{1 2}]] :incompatible-membership-types]
                         [[:in [:literal 1] [:literal #{1 :new}]] :heterogeneous-set]]]
    (testing (pr-str expr) (rejects! expr {} reason))))

(deftest optional-field-requires-a-dominating-presence-test
  (let [read [:read :result/score]
        present [:present read]
        comparison [:> read [:literal 7]]
        types {:result/score optional-int}]
    (accepted! [:and present comparison] types)
    (accepted! [:implies present comparison] types)
    (accepted! [:or [:not present] comparison] types)
    (rejects! comparison types :unguarded-optional-reader)
    (rejects! [:and comparison present] types :unguarded-optional-reader)
    (rejects! [:or present comparison] types :unguarded-optional-reader)
    (rejects! [:implies comparison present] types :unguarded-optional-reader)
    (rejects! [:not comparison] types :unguarded-optional-reader)))

(deftest optional-and-nullable-are-independent-contracts
  (let [read [:read :result/score]
        present [:present read]
        nonnull [:not= read [:literal nil]]
        cmp [:> read [:literal 7]]]
    (accepted! [:and present nonnull cmp]
               {:result/score optional-nullable-int})
    (accepted! [:and nonnull cmp] {:result/score nullable-int})
    (accepted! [:or [:= read [:literal nil]] cmp]
               {:result/score nullable-int})
    (accepted! [:implies [:and present nonnull] cmp]
               {:result/score optional-nullable-int})
    (rejects! [:and present cmp] {:result/score optional-nullable-int}
              :expected-nonnullable-integers)
    (rejects! [:and nonnull cmp] {:result/score optional-nullable-int}
              :unguarded-optional-reader)
    (rejects! cmp {:result/score nullable-int}
              :expected-nonnullable-integers)))

(deftest boolean-fields-cannot-be-used-implicitly-as-nil-safe
  (accepted! [:read :result/ok] {:result/ok bool-type})
  (accepted! [:or [:= [:read :result/ok] [:literal nil]]
              [:read :result/ok]]
             {:result/ok {:type :boolean :nullable? true}})
  (rejects! [:read :result/ok] {:result/ok {:type :boolean :nullable? true}}
            :expected-nonnullable-boolean)
  (rejects! [:and [:literal true] [:read :result/ok]]
            {:result/ok {:type :boolean :nullable? true}}
            :expected-nonnullable-boolean))

(deftest type-footprint-must-match-declared-reader-footprint
  (let [expr [:> [:read :result/score] [:literal 7]]
        declaration {:readers #{:result/score} :expression expr}]
    (doseq [types [{} {:result/score int-type :result/other int-type}]]
      (let [e (rejection #(rule-type/check-rule-type! declaration types))]
        (is (= :reader-type-footprint-mismatch (:reason e)))))
    (is (= :syntax-and-types
           (:checking (rule-type/check-rule-type! declaration
                                                  {:result/score int-type}))))))

(deftest malformed-type-descriptors-rejected
  (let [expr [:> [:read :result/score] [:literal 7]]]
    (doseq [[descriptor reason]
            [[{:type :unknown} :unknown-reader-type]
             [{:type :integer :optional? :yes} :invalid-type-flag]
             [{:type :integer :nullable? nil} :invalid-type-flag]
             [{:type :integer :name :made/up} :unexpected-enum-name]
             [{:type :integer :values #{1}} :unexpected-enum-domain]
             [{:type :integer :certificate {:proved true}} :unknown-descriptor-keys]
             [{:type :enum :values #{:new}} :missing-enum-name]
             [{:type :enum :name :domain/status :values #{}} :invalid-enum-domain]
             [{:type :enum :name :domain/status :values #{1}} :invalid-enum-member]
             [{:type :enum :name :domain/status :values #{:new} :proof true}
              :unknown-descriptor-keys]]]
      (testing (pr-str descriptor)
        (rejects! expr {:result/score descriptor} reason)))))

(deftest declarative-type-maps-must-be-plain-and-metadata-free
  (let [expr [:> [:read :result/score] [:literal 7]]]
    (doseq [[types reason]
            [[(sorted-map :result/score int-type) :expected-reader-type-map]
             [{:result/score (sorted-map :type :integer)} :invalid-type-descriptor]
             [{:result/score (fn [] int-type)} :invalid-type-descriptor]
             [(with-meta {:result/score int-type} {:admitted true}) :metadata-not-allowed]
             [{:result/score (with-meta int-type {:proved true})} :metadata-not-allowed]]]
      (let [data (rejection #(type-check! expr types))]
        (is (= reason (:reason data)) (pr-str data))))))

(deftest unsupported-syntax-is-not-accepted-by-typing
  (doseq [expr [[:eval [:literal "(+ 1 2)"]]
                [:invoke [:literal true]]
                [:literal (fn [] true)]
                [:> [:read :undeclared/score] [:literal 7]]]]
    (let [data (rejection #(type-check! expr {}))]
      (is (some? data) (pr-str expr))
      (is (= :gesso.assurance.rule/invalid-rule (:error/type data))))))

(deftest repeated-descriptions-remain-identical-and-unadmitted
  (let [expr [:and [:present [:read :result/score]]
              [:> [:read :result/score] [:literal 7]]]
        types {:result/score optional-int}
        one (type-check! expr types)
        two (type-check! expr types)]
    (is (= one two))
    (is (= :not-proved (:proof-status one)))
    (is (not (contains? one :certificate)))
    (is (not (contains? one :valid?)))
    (is (not (contains? one :discharge-status)))))

(deftest many-big-integer-literals-stay-in-mathematical-integer-domain
  (doseq [n [0 1 -1 Long/MAX_VALUE Long/MIN_VALUE
             9223372036854775808N -9223372036854775809N
             999999999999999999999999999999999999N]]
    (accepted! [:>= [:literal n] [:literal -1]] {})))
