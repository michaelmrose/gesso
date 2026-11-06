(ns gesso.assurance.proof.kernel-typed-atom-evaluator-mutation-test
  "v1430: adversarial faults at the normalized proof-atom / typed evaluator
   boundary. This is deliberately different from v1324's operator fuzzing:
   every witness uses a source proposition, its canonical typed atom, and
   an independently executed, branch-sensitive typed denotation.

   Mutants are injected into the real production evaluator or type-checker;
   they do not alter the independent interpreter or invent a checker report.
   Activated mutations must produce a concrete disagreement or rejection.
   Static invalidity is checked even when the bad branch is unreachable.

   Evidence is finite and diagnostic. It is NOT a universal soundness proof,
   certified typed-atom denotation, JVM/CLJS equivalence, or transaction
   authority."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.proposition :as proposition]
            [gesso.assurance.proof.propositional-normalization-audit :as audit]
            [gesso.assurance.proof.typed-presence-semantics :as independent]
            [gesso.assurance.rule-eval :as production]
            [gesso.assurance.rule-type :as rule-type]))

(def ^:private status :request/status)
(def ^:private other-status :organization/status)
(def ^:private score :request/score)
(def ^:private enabled :request/enabled)
(def ^:private status-desc
  {:type :enum :name :request/status
   :values #{:request/new :request/claimed :request/done}})
(def ^:private other-desc
  (assoc status-desc :name :organization/status))
(def ^:private optional-int
  {:type :integer :optional? true :nullable? true})
(def ^:private optional-bool
  {:type :boolean :optional? true :nullable? true})
(def ^:private rscore [:read score])
(def ^:private renabled [:read enabled])
(def ^:private pscore [:present rscore])
(def ^:private penabled [:present renabled])
(def ^:private nonnil-score [:not= rscore [:literal nil]])
(def ^:private nonnil-enabled [:not= renabled [:literal nil]])

(def ^:private subject
  {:kind :operation :id :example/kernel-typed-atom-mutations
   :interface {:operation :example/kernel-typed-atom-mutations}})

(defn- source [expr types]
  {:subject subject
   :variables types
   :proposition [:atom {:readers (set (keys types))
                        :expression expr}]})

(defn- rule [expr types]
  {:readers (set (keys types)) :expression expr})

(defn- thrown-data [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- checked-atom! [expr types]
  (let [s (source expr types)
        normalized (proposition/describe-proposition! s)
        atom-data (get-in normalized [:semantics :proposition 1])
        audit-report (audit/audit-proposition! s)]
    (is (= :preserved-for-all-opaque-valuations (:status audit-report)))
    (is (= :not-established
           (:typed-atomic-denotational-equivalence audit-report)))
    (is (= :not-issued (:certificate audit-report)))
    (is (= :none (:authority audit-report)))
    (is (= (set (keys types)) (:readers atom-data)))
    {:source s :canonical atom-data :audit audit-report}))

;; Independent, intentionally small denotation for the witnesses below.
;; Neither production, rule-type, nor typed-presence-semantics is consulted.
;; This test interpreter interprets only well-typed expressions, which are
;; admitted separately by checked-atom! and independent/evaluate-expression!.
(defn- local-denotation [expr bindings]
  (let [paths (atom [])]
    (letfn [(walk [x at]
              (swap! paths conj at)
              (let [[op a b] x]
                (case op
                  :literal a
                  :read (if (contains? bindings a)
                          (get bindings a)
                          (throw (ex-info "Absent local reader"
                                          {:reason :absent-local-reader :at at})))
                  :present (contains? bindings (second a))
                  :not (not (walk a (conj at 1)))
                  :and (loop [i 1]
                         (cond (= i (count x)) true
                               (walk (nth x i) (conj at i)) (recur (inc i))
                               :else false))
                  :or (loop [i 1]
                        (cond (= i (count x)) false
                              (walk (nth x i) (conj at i)) true
                              :else (recur (inc i))))
                  :implies (if (walk a (conj at 1))
                             (walk b (conj at 2)) true)
                  := (= (walk a (conj at 1)) (walk b (conj at 2)))
                  :not= (not= (walk a (conj at 1)) (walk b (conj at 2)))
                  :> (> (walk a (conj at 1)) (walk b (conj at 2)))
                  :in (contains? (walk b (conj at 2))
                                 (walk a (conj at 1)))
                  (throw (ex-info "Unmodeled test operator"
                                  {:operator op :at at})))))]
      {:value (walk expr [:expression]) :paths @paths})))

(defn- establish-baseline! [expr types states]
  (let [{:keys [canonical audit]} (checked-atom! expr types)]
    (is (= :not-established
           (:typed-atomic-denotational-equivalence audit)))
    (doseq [state states]
      (let [expected (local-denotation expr state)
            canonical-value (local-denotation (:expression canonical) state)
            separate (independent/evaluate-expression! expr types state)
            evaluated (production/evaluate-rule! (rule expr types) types state)]
        (is (= expected canonical-value) (pr-str [expr state]))
        (is (= (:value expected) (:value separate)) (pr-str [expr state]))
        (is (= (:value expected) (:value evaluated)) (pr-str [expr state]))
        (is (= (:paths expected) (:evaluated-paths separate))
            (pr-str [expr state]))
        (is (= :not-proved (:proof-status evaluated)))
        (is (= :not-admitted (:admission evaluated)))
        (is (= :none (:authority evaluated)))))))

(def ^:private interpreter-var
  (or (ns-resolve 'gesso.assurance.rule-eval 'interpret!)
      (throw (ex-info "Missing production interpreter"
                      {:reason :missing-evaluator}))))

(defn- with-operator-mutant
  "Mutate an actual production interpreter call. Count only executed target
   operators; an unreachable target must not count as a killed mutant."
  [target replacement thunk]
  (let [original @interpreter-var
        calls (atom [])
        mutated (fn [expr bindings at]
                  (let [value (original expr bindings at)]
                    (if (= (first expr) target)
                      (do (swap! calls conj {:expr expr :bindings bindings :at at})
                          (replacement original expr bindings at value))
                      value)))
        outcome (try {:value (with-redefs-fn {interpreter-var mutated} thunk)}
                     (catch clojure.lang.ExceptionInfo e
                       {:error (ex-data e)}))]
    (assoc outcome :calls @calls)))

(defn- assert-killed-value! [expr types bindings target replacement]
  (let [expected (local-denotation expr bindings)
        separate (independent/evaluate-expression! expr types bindings)
        baseline (production/evaluate-rule! (rule expr types) types bindings)
        mutant (with-operator-mutant target replacement
                 #(production/evaluate-rule! (rule expr types) types bindings))
        restored (production/evaluate-rule! (rule expr types) types bindings)]
    (is (= (:value expected) (:value separate)))
    (is (= (:value expected) (:value baseline)))
    (is (= (:paths expected) (:evaluated-paths separate)))
    (is (seq (:calls mutant)) (str "Target was not executed: " target))
    (is (= nil (:error mutant)) (pr-str mutant))
    (is (not= (:value expected) (get-in mutant [:value :value]))
        (pr-str [expr bindings target mutant]))
    (is (= baseline restored) "Mutant leaked past scoped redefinition")
    (is (= :not-proved (get-in mutant [:value :proof-status])))
    (is (= :not-admitted (get-in mutant [:value :admission])))
    (is (= :none (get-in mutant [:value :authority])))
    mutant))

(defn- flip-boolean [_ _ _ _ value] (not value))

(deftest v1430-present-nil-and-present-false-kill-distinct-mutants
  (let [expr penabled
        types {enabled optional-bool}
        states [{} {enabled nil} {enabled false} {enabled true}]]
    (establish-baseline! expr types states)
    (testing "incorrect presence = non-nullness"
      (let [mutant (assert-killed-value!
                    expr types {enabled nil} :present
                    (fn [_ _ bindings _ _] (some? (get bindings enabled))))]
        (is (= {enabled nil} (:bindings (first (:calls mutant)))))))
    (testing "incorrect presence = truthiness"
      (let [mutant (assert-killed-value!
                    expr types {enabled false} :present
                    (fn [_ _ bindings _ _] (boolean (get bindings enabled))))]
        (is (= {enabled false} (:bindings (first (:calls mutant)))))))))

(deftest v1430-nullable-optional-reader-and-comparison-kill-mutants
  (let [expr [:and pscore nonnil-score [:> rscore [:literal 7]]]
        types {score optional-int}
        states [{} {score nil} {score 7} {score 8}]]
    (establish-baseline! expr types states)
    (testing "the comparison must be true at 8"
      (assert-killed-value! expr types {score 8} :>
                            flip-boolean))
    (testing "direct nullable comparison distinguishes nil from non-nil"
      (let [nullable {score {:type :integer :nullable? true}}
            comparison [:not= rscore [:literal nil]]]
        (establish-baseline! comparison nullable
                             [{score nil} {score 0} {score 8}])
        (assert-killed-value! comparison nullable {score nil} :not=
                              flip-boolean)))))

(deftest v1430-nominal-enum-equality-and-membership-mutations
  (let [types {status status-desc}
        equality [:= [:read status] [:literal :request/claimed]]
        membership [:in [:read status]
                    [:literal #{:request/claimed :request/done}]]
        states [{status :request/new} {status :request/claimed}
                {status :request/done}]]
    (establish-baseline! equality types states)
    (establish-baseline! membership types states)
    (assert-killed-value! equality types {status :request/claimed}
                          := flip-boolean)
    (assert-killed-value! membership types {status :request/new}
                          :in flip-boolean)))

(deftest v1430-nominal-identity-erasure-is-caught-at-independent-type-boundary
  (let [expr [:= [:read status] [:read other-status]]
        valid-types {status status-desc other-status status-desc}
        invalid-types {status status-desc other-status other-desc}
        bindings {status :request/claimed other-status :request/claimed}
        checked (rule-type/check-rule-type! (rule expr valid-types)
                                             valid-types)
        independent-error (thrown-data #(independent/evaluate-expression!
                                         expr invalid-types bindings))
        normal-error (thrown-data #(production/evaluate-rule!
                                    (rule expr invalid-types)
                                    invalid-types bindings))]
    (is (= true (:value (local-denotation expr bindings))))
    (is (= :incompatible-equality-types (:reason independent-error)))
    (is (= :incompatible-equality-types (:reason normal-error)))
    (is (map? (thrown-data #(proposition/describe-proposition!
                            (source expr invalid-types)))))
    ;; Deliberately replace the production type check with a successful
    ;; result from a DIFFERENT declaration. This models illicit erasure of
    ;; the second enum's nominal identity; it is not an admitted execution.
    (let [forged (with-redefs [rule-type/check-rule-type!
                               (fn [_ _] checked)]
                   (production/evaluate-rule! (rule expr invalid-types)
                                              invalid-types bindings))]
      (is (true? (:value forged)))
      (is (= :none (:authority forged)))
      (is (= :not-proved (:proof-status forged)))
      (is (= :incompatible-equality-types
             (:reason (thrown-data #(independent/evaluate-expression!
                                     expr invalid-types bindings))))))))

(defn- eager-trailing-operand
  "Model a broken eager Boolean operator by attempting its last child after
   the original operator completed. The original interpreter is still used
   for the child, so an absent optional read raises its actual typed error."
  [original expr bindings at value]
  (original (last expr) bindings (conj at (dec (count expr))))
  value)

(deftest v1430-short-circuit-mutants-expose-unsafe-optional-read
  (let [types {score optional-int}
        guarded [:or [:= rscore [:literal nil]]
                 [:> rscore [:literal 7]]]
        cases [[:and pscore nonnil-score [:> rscore [:literal 7]]]
               [:or [:not pscore] guarded]
               [:implies pscore guarded]]]
    (doseq [expr cases]
      (testing (str "eager " (first expr))
        (establish-baseline! expr types [{} {score 8}])
        (let [expected (independent/evaluate-expression! expr types {})
              mutant (with-operator-mutant
                       (first expr) eager-trailing-operand
                       #(production/evaluate-rule! (rule expr types) types {}))]
          (is (instance? Boolean (:value expected)))
          (is (not-any? #{[:expression (dec (count expr))]}
                        (:evaluated-paths expected)))
          (is (seq (:calls mutant)))
          (is (= :absent-reader-evaluated (get-in mutant [:error :reason])))
          (is (= :none (:authority expected))))))))

(deftest v1430-static-rejection-survives-unreachable-branches
  (let [types {score optional-int}
        unsafe [[:and [:literal false] [:> rscore [:literal 7]]]
                [:or [:literal true] [:> rscore [:literal 7]]]
                [:implies [:literal false] [:> rscore [:literal 7]]]]]
    (doseq [expr unsafe]
      (testing (str "unsafe dead branch " expr)
        (is (some? (thrown-data #(independent/evaluate-expression!
                                  expr types {}))))
        (is (some? (thrown-data #(production/evaluate-rule!
                                  (rule expr types) types {}))))
        (is (some? (thrown-data #(proposition/describe-proposition!
                                  (source expr types))))))))
  ;; A checker mutant that blindly accepts a pre-checked branch cannot
  ;; manufacture a successful certified proposition. The unmutated
  ;; independent and source-normalization checks still reject its input.
  (let [types {score optional-int}
        expr [:or [:literal true] [:> rscore [:literal 7]]]
        good (rule-type/check-rule-type! (rule [:or [:present rscore] [:literal true]] types) types)
        forged (with-redefs [rule-type/check-rule-type!
                             (fn [_ _] good)]
                 (production/evaluate-rule! (rule expr types) types {}))]
    (is (true? (:value forged)))
    (is (= :none (:authority forged)))
    (is (some? (thrown-data #(independent/evaluate-expression!
                              expr types {}))))
    (is (some? (thrown-data #(proposition/describe-proposition!
                              (source expr types)))))))

(deftest v1430-sampled-correspondence-is-not-proof-authority
  (let [expr [:and penabled nonnil-enabled renabled]
        types {enabled optional-bool}
        checked (checked-atom! expr types)
        evaluated (independent/evaluate-expression! expr types {enabled true})]
    (establish-baseline! expr types
                         [{} {enabled nil} {enabled false} {enabled true}])
    (is (= :not-established
           (get-in checked [:audit :typed-atomic-denotational-equivalence])))
    (is (= :not-proved (:proof-status evaluated)))
    (is (= :not-admitted (:admission evaluated)))
    (is (= :none (:authority evaluated)))))
