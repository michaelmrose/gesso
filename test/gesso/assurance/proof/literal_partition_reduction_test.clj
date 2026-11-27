(ns gesso.assurance.proof.literal-partition-reduction-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.proof.literal-partition :as partition]
            [gesso.assurance.proof.literal-partition-coverage :as coverage]
            [gesso.assurance.proof.literal-partition-congruence :as congruence]
            [gesso.assurance.proof.literal-partition-reduction :as reduction]
            [gesso.assurance.proof.typed-presence-semantics :as presence]
            [gesso.assurance.rule-eval :as production]
            [gesso.live.outbox.claim-semantic-fixture :as claim]))

(deftest original-claim-abstract-reduction
  (let [catalog (claim/catalog)
        rules (assoc (:invariants catalog) :fixture/init (:initial-condition catalog))
        reports (into {} (map (fn [[id rule]] [id (reduction/derive! rule claim/state-types)])) rules)]
    (doseq [[id r] reports]
      (is (= :conditional-abstract-reduction (:status r)) (str id " " (:reason r)))
      (is (= (rules id) (:rule r)))
      (is (= :abstract-truth-factors-through-representative-cells (:conclusion r)))
      (is (= :not-established (:executable-evaluator-refinement r) (:universal-evaluator-correspondence r)))
      (is (= :not-performed (:proof-discharge r)))
      (is (= :checked-per-valuation (:representative-evaluator-agreement r)))
      (is (= (:cases-checked r) (count (:truth-table r))))
      (is (= (:cases-checked r) (count (set (map :bindings (:truth-table r)))))))
    (is (= 819 (reduce + (map :cases-checked (vals reports)))))
    (is (= 8 (reduce + (map :true-cases (vals reports)))))))

(deftest exact-row-truth-and-empty-footprint
  (let [types {:fixture/x {:type :integer}}
        rule {:readers #{:fixture/x} :expression [:>= [:read :fixture/x] [:literal 0]]}
        r (reduction/derive! rule types)]
    (is (= :conditional-abstract-reduction (:status r)))
    (is (= {-1 false 0 true 1 true}
           (into {} (map (fn [{:keys [bindings value]}] [(:fixture/x bindings) value])) (:truth-table r))))
    (doseq [v [true false]]
      (let [r (reduction/derive! {:readers #{} :expression [:literal v]} {})]
        (is (= :conditional-abstract-reduction (:status r)))
        (is (= [{:bindings {} :value v}] (:truth-table r)))))
    (let [original @#'reduction/abstract-value!]
      (with-redefs-fn {#'reduction/abstract-value! (fn [node cells] (not (original node cells)))}
        #(let [bad (reduction/derive! rule types)]
           (is (= :representative-truth-mismatch (:reason bad)))
           (is (not (contains? bad :truth-table))))))))

(deftest unqualified-composition-is-rejected
  (let [types {:fixture/x {:type :integer}}
        rule {:readers #{:fixture/x} :expression [:>= [:read :fixture/x] [:literal 0]]}
        run #(reduction/derive! rule types)]
    (doseq [v [#'partition/audit-rule! #'coverage/check-coverage! #'congruence/derive!
               #'presence/evaluate-expression! #'production/evaluate-rule!]]
      (with-redefs-fn {v (constantly {})}
        #(is (= :reduction-boundary-replaced (:reason (run))))))
    (is (= :not-established (:status (reduction/derive! (assoc rule :expression [:= [:read :fixture/x] [:read :fixture/x]]) types))))
    (is (= :conditional-abstract-reduction (:status (run))))))
