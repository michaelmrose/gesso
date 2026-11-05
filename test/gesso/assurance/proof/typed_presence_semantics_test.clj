(ns gesso.assurance.proof.typed-presence-semantics-test
  "v1345: permanent adversarial tests for the independent, branch-sensitive
   S1 presence and nullable-value interpreter introduced in v1344.

   Compare admitted closed expressions against the production rule evaluator
   on concrete typed partial states. Reject unsafe expressions regardless of
   whether a particular runtime would short-circuit a branch. Assert exact
   presence, nil, false, and execution-path distinctions. This is finite
   correspondence evidence, NOT universal S1 soundness, proof admission,
   authoritative state provenance, or JVM/CLJS equivalence."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.typed-presence-semantics :as sem]
            [gesso.assurance.rule-eval :as production]))

(def ^:private score :request/score)
(def ^:private flag :request/flag)
(def ^:private status :request/status)
(def ^:private other :request/other)
(def ^:private nullable-optional-integer
  {:type :integer :optional? true :nullable? true})
(def ^:private nullable-integer {:type :integer :nullable? true})
(def ^:private optional-boolean {:type :boolean :optional? true})
(def ^:private nullable-boolean {:type :boolean :nullable? true})
(def ^:private optional-nullable-boolean
  {:type :boolean :optional? true :nullable? true})
(def ^:private optional-enum
  {:type :enum :name :request/status :optional? true
   :values #{:request/new :request/claimed :request/done}})
(def ^:private max-integer 999999999999999999999999999999999999N)

(defn- production-report [expression reader-types state]
  (production/evaluate-rule!
   {:readers (set (keys reader-types)) :expression expression}
   reader-types state))

(defn- independent-report [expression reader-types state]
  (sem/evaluate-expression! expression reader-types state))

(defn- failure [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo ex (ex-data ex))))

(defn- both-agree!
  [expression reader-types state expected]
  (let [independent (independent-report expression reader-types state)
        actual (production-report expression reader-types state)
        context (pr-str [expression reader-types state])]
    (is (= expected (:value independent)) context)
    (is (= expected (:value actual)) context)
    (is (= (:value independent) (:value actual)) context)
    (is (instance? Boolean (:value independent)) context)
    (is (= (set (keys reader-types)) (:source-readers independent)) context)
    (is (= :not-proved (:proof-status independent)) context)
    (is (= :not-admitted (:admission independent)) context)
    (is (= :none (:authority independent)) context)
    independent))

(defn- both-reject!
  [expression reader-types state]
  (let [ind (failure #(independent-report expression reader-types state))
        prod (failure #(production-report expression reader-types state))
        context (pr-str [expression reader-types state])]
    (is (map? ind) (str "Independent semantics must reject: " context))
    (is (map? prod) (str "Production must reject: " context))
    (when (map? ind)
      (is (or (true? (:typed-presence-semantics/rejection ind))
              (true? (:typed-value-semantics/rejection ind))) context))
    (when (map? prod)
      (is (keyword? (:reason prod)) context))))

(defn- independent-rejects-reason!
  [expression reader-types state expected-reason]
  (let [data (failure #(independent-report expression reader-types state))]
    (is (= expected-reason (:reason data))
        (pr-str [expected-reason expression reader-types state data]))))

(def ^:private presence [:present [:read score]])
(def ^:private nonnil [:not= [:read score] [:literal nil]])
(def ^:private nil-value [:= [:read score] [:literal nil]])
(def ^:private positive [:> [:read score] [:literal 0]])
(def ^:private guarded-positive [:and presence nonnil positive])

(deftest present-tests-key-membership-not-value-truthiness
  (let [types {flag optional-nullable-boolean}
        expr [:present [:read flag]]]
    (doseq [[state expected] [[{} false] [{flag nil} true]
                              [{flag false} true] [{flag true} true]]]
      (let [r (both-agree! expr types state expected)]
        (is (= (if expected #{flag} #{})
               (get-in r [:observed-presence :present-readers])))
        (is (= (if expected #{} #{flag})
               (get-in r [:observed-presence :absent-readers])))))))

(deftest optional-is-not-nullable
  (both-agree! [:present [:read flag]] {flag optional-boolean} {} false)
  (both-agree! [:present [:read flag]] {flag optional-boolean} {flag false} true)
  (both-reject! [:present [:read flag]] {flag optional-boolean} {flag nil}))

(deftest nullable-is-not-optional
  (both-reject! [:present [:read flag]] {flag nullable-boolean} {})
  (both-agree! [:present [:read flag]] {flag nullable-boolean} {flag nil} true)
  (both-agree! [:present [:read flag]] {flag nullable-boolean} {flag false} true))

(deftest guarded-optional-nullable-integer-comparison
  (doseq [[state expected] [[{} false] [{score nil} false]
                            [{score -1} false] [{score 0} false]
                            [{score 1} true] [{score max-integer} true]]]
    (both-agree! guarded-positive {score nullable-optional-integer} state expected)))

(deftest guard-evaluation-path-skips-absent-reader
  (let [r (both-agree! guarded-positive {score nullable-optional-integer} {} false)]
    (is (some #{[:expression 1]} (:evaluated-paths r)))
    (is (not-any? #{[:expression 2] [:expression 3]}
                  (:evaluated-paths r)))))

(deftest guard-evaluation-path-skips-nil-comparison
  (let [r (both-agree! guarded-positive {score nullable-optional-integer}
                       {score nil} false)]
    (is (some #{[:expression 2]} (:evaluated-paths r)))
    (is (not-any? #{[:expression 3]} (:evaluated-paths r)))))

(deftest guard-evaluation-path-includes-visited-branch
  (let [r (both-agree! guarded-positive {score nullable-optional-integer}
                       {score 9} true)]
    (is (every? (set (:evaluated-paths r))
                [[:expression] [:expression 1] [:expression 2]
                 [:expression 3] [:expression 3 1] [:expression 3 2]]))))

(deftest nested-negation-presence-refinement
  (let [expr [:and [:not [:not presence]] positive]
        types {score {:type :integer :optional? true}}]
    (doseq [[state expected] [[{} false] [{score -1} false]
                              [{score 2} true]]]
      (both-agree! expr types state expected))))

(deftest disjunction-false-branch-presence-refinement
  (let [expr [:or [:not presence] positive]
        types {score {:type :integer :optional? true}}]
    (doseq [[state expected] [[{} true] [{score -1} false]
                              [{score 2} true]]]
      (both-agree! expr types state expected))))

(deftest disjunction-false-branch-nonnil-refinement
  (let [expr [:or nil-value positive]
        types {score nullable-integer}]
    (doseq [[state expected] [[{score nil} true] [{score -1} false]
                              [{score 2} true]]]
      (both-agree! expr types state expected))))

(deftest disjunction-nested-false-refinement
  (let [expr [:or [:or [:not presence] nil-value] positive]
        types {score nullable-optional-integer}]
    (doseq [[state expected] [[{} true] [{score nil} true]
                              [{score -2} false] [{score 4} true]]]
      (both-agree! expr types state expected))))

(deftest implication-antecedent-licenses-optional-read
  (let [expr [:implies presence [:> [:read score] [:literal 2]]]
        types {score {:type :integer :optional? true}}]
    (doseq [[state expected] [[{} true] [{score 1} false]
                              [{score 3} true]]]
      (both-agree! expr types state expected))))

(deftest implication-with-nested-guards
  (let [expr [:implies [:and presence nonnil] positive]
        types {score nullable-optional-integer}]
    (doseq [[state expected] [[{} true] [{score nil} true]
                              [{score -2} false] [{score 4} true]]]
      (both-agree! expr types state expected))))

(deftest conjunction-guards-are-left-to-right
  (both-reject! [:and positive presence]
                {score {:type :integer :optional? true}} {score 2})
  (both-reject! [:and nonnil presence]
                {score nullable-optional-integer} {score 2})
  (both-reject! [:and positive nonnil]
                {score nullable-integer} {score 2}))

(deftest optional-read-rejected-even-when-actually-present
  (both-reject! positive {score {:type :integer :optional? true}} {score 9})
  (both-reject! nonnil {score nullable-optional-integer} {score nil})
  (both-reject! nil-value {score nullable-optional-integer} {score nil}))

(deftest null-comparison-without-optional-is-legal
  (doseq [state [{score nil} {score -1} {score 0}]]
    (both-agree! nil-value {score nullable-integer} state (nil? (get state score)))
    (both-agree! nonnil {score nullable-integer} state (some? (get state score)))))

(deftest reversed-nil-comparison-establishes-nonnil
  (let [expr [:and presence [:not= [:literal nil] [:read score]] positive]]
    (doseq [[state expected] [[{} false] [{score nil} false]
                              [{score -2} false] [{score 3} true]]]
      (both-agree! expr {score nullable-optional-integer} state expected))))

(deftest membership-requires-a-valid-nonnil-guard
  (let [expr [:and presence nonnil [:in [:read score] [:literal #{0 1 5}]]]]
    (doseq [[state expected] [[{} false] [{score nil} false]
                              [{score -1} false] [{score 5} true]]]
      (both-agree! expr {score nullable-optional-integer} state expected))
    (both-reject! [:and presence [:in [:read score] [:literal #{0 1 5}]]]
                  {score nullable-optional-integer} {score 5})))

(deftest guarded-nominal-enum-membership
  (let [expr [:and [:present [:read status]]
               [:in [:read status] [:literal #{:request/new :request/done}]]]]
    (doseq [[state expected] [[{} false] [{status :request/new} true]
                              [{status :request/claimed} false]]]
      (both-agree! expr {status optional-enum} state expected))))

(deftest guarded-boolean-read-and-false-value
  (let [expr [:and [:present [:read flag]] [:read flag]]]
    (doseq [[state expected] [[{} false] [{flag false} false] [{flag true} true]]]
      (both-agree! expr {flag optional-boolean} state expected))))

(deftest nullable-boolean-needs-nonnil-guard
  (let [expr [:and [:not= [:read flag] [:literal nil]] [:read flag]]]
    (doseq [[state expected] [[{flag nil} false] [{flag false} false]
                              [{flag true} true]]]
      (both-agree! expr {flag nullable-boolean} state expected))
    (both-reject! [:read flag] {flag nullable-boolean} {flag true})))

(deftest two-readers-must-be-guarded-independently
  (let [types {score {:type :integer :optional? true}
               other {:type :integer :optional? true}}
        expr [:and [:present [:read score]]
              [:present [:read other]]
              [:< [:read score] [:read other]]]]
    (doseq [[state expected] [[{} false] [{score 1} false]
                              [{score 1 other 2} true]
                              [{score 4 other 2} false]]]
      (both-agree! expr types state expected))
    (both-reject! [:and [:present [:read score]]
                   [:< [:read score] [:read other]]]
                  types {score 1 other 2})))

(deftest never-infer-safety-from-and-false
  (both-reject! [:or [:and presence positive] positive]
                {score {:type :integer :optional? true}} {score 9}))

(deftest never-infer-safety-from-or-true
  (both-reject! [:and [:or presence [:literal true]] positive]
                {score {:type :integer :optional? true}} {score 9}))

(deftest never-infer-safety-from-implication-true
  (both-reject! [:and [:implies presence [:literal true]] positive]
                {score {:type :integer :optional? true}} {score 9}))

(deftest no-presence-refinement-from-unrelated-reader
  (let [types {score {:type :integer :optional? true}
               other {:type :integer :optional? true}}]
    (both-reject! [:and [:present [:read other]] positive]
                  types {score 9 other 2})))

(deftest both-branches-typechecked-despite-short-circuit
  (both-reject! [:and [:literal false] positive]
                {score {:type :integer :optional? true}} {})
  (both-reject! [:or [:literal true] positive]
                {score {:type :integer :optional? true}} {})
  (both-reject! [:implies [:literal false] positive]
                {score {:type :integer :optional? true}} {}))

(deftest complete-reader-footprint-is-required
  (independent-rejects-reason!
   [:present [:read score]]
   {score {:type :integer :optional? true} other {:type :integer :optional? true}}
   {} :reader-footprint-mismatch)
  (both-reject! [:present [:read :request/absent]] {} {})
  (both-reject! [:present [:read score]] {score {:type :integer :optional? true}}
                {other 3}))

(deftest present-requires-a-direct-reader
  (doseq [expr [[:present [:literal nil]]
                [:present [:> [:read score] [:literal 0]]]
                [:present :request/score]
                [:present [:read score] :extra]]]
    (both-reject! expr {score {:type :integer :optional? true}} {})))

(deftest invalid-state-values-rejected-before-evaluation
  (let [types {score nullable-optional-integer}]
    (both-reject! guarded-positive types {score false})
    (both-reject! guarded-positive types {score 3.5})
    (both-reject! guarded-positive types {score (fn [] 1)})
    (both-reject! guarded-positive types {score 5 other 1})))

(deftest invalid-enum-values-fail-closed
  (both-reject! [:present [:read status]] {status optional-enum}
                {status :request/unknown})
  (both-reject! [:and [:present [:read status]]
                 [:= [:read status] [:literal :outside/value]]]
                {status optional-enum} {status :request/new}))

(deftest malformed-descriptors-and-literals-rejected
  (doseq [types [{score {:type :float :optional? true}}
                 {score {:type :integer :optional? :yes}}
                 {score {:type :integer :nullable? nil}}]]
    (both-reject! presence types {}))
  (both-reject! [:and presence [:in [:read score] [:literal #{1 "1"}]]]
                {score {:type :integer :optional? true}} {score 1}))

(deftest metadata-and-executable-sources-fail-closed
  (both-reject! (with-meta presence {:untrusted true})
                {score {:type :integer :optional? true}} {})
  (both-reject! [:present (with-meta [:read score] {:untrusted true})]
                {score {:type :integer :optional? true}} {})
  (both-reject! [:and presence
                 (with-meta positive {:untrusted true})]
                {score {:type :integer :optional? true}} {score 1})
  (both-reject! presence
                {score {:type :integer :optional? true}}
                (with-meta {} {:untrusted true})))

(deftest unsupported-operators-and-expression-shapes
  (doseq [expr [[:xor [:literal true] [:literal false]]
                [:and [:literal true]]
                [:implies [:literal true]]
                [:not [:literal true] [:literal false]]
                [:and [:literal true] [:literal true] [:read :unknown/r]]]]
    (both-reject! expr {} {})))

(deftest report-is-diagnostic-and-deterministic
  (let [types {score nullable-optional-integer}
        state {score 9}
        a (both-agree! guarded-positive types state true)
        b (independent-report guarded-positive types state)]
    (is (= a b))
    (is (= sem/result-type (:type a)))
    (is (= sem/semantics-version (:version a)))
    (is (= :independent-branch-sensitive-typing (:checking a)))
    (is (= :independent-short-circuit-semantics (:evaluation a)))
    (is (= :not-established (:runtime-enforcement a)))
    (is (= :not-established (:snapshot-provenance a)))
    (is (= :not-established (:jvm-cljs-equivalence a)))
    (doseq [field [:certificate :proof :handle :transaction-id :committed?]]
      (is (not (contains? a field)) (str "No authoritative " field)))))
