(ns gesso.assurance.proof.literal-partition-congruence-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.proof.literal-partition-congruence :as congruence]
            [gesso.assurance.proof.literal-partition :as partition]
            [gesso.assurance.proof.propositional-semantics :as semantics]
            [gesso.live.outbox.claim-semantic-fixture :as claim]))

(defn- nodes [tree]
  (tree-seq #(seq (:children %)) :children tree))

(deftest original-claim-source-has-abstract-congruence-derivations
  (let [catalog (claim/catalog)]
    (doseq [[id rule] (assoc (:invariants catalog) :fixture/init (:initial-condition catalog))]
      (let [r (congruence/derive! rule claim/state-types)
            tree (:derivation r)]
        (is (= :conditional-abstract-congruence (:status r)) (str id " " (pr-str r)))
        (is (= rule (:rule r)))
        (is (= (:expression rule) (:source tree)))
        (is (= :equal-abstract-expression-truth (:conclusion r)))
        (is (= :not-established (:scalar-observation-refinement r)
               (:executable-evaluator-refinement r)))
        (is (= :not-performed (:proof-discharge r)))
        (doseq [{:keys [source children local-proof rule]} (nodes tree)]
          (when (= :child-congruence rule)
            (is (= (vec (rest source)) (mapv :source children))))
          (when local-proof
            (let [{:keys [premises goal entailment]} local-proof]
              (is (= :entails (:result entailment)))
              (is (= entailment (semantics/enumerate-entailment! premises goal))))))))))

(deftest comparison-and-connective-proof-shapes
  (let [types {:fixture/x {:type :integer}}
        atoms (mapv #(vector % [:read :fixture/x] [:literal 0]) [:= :not= :< :<= :> :>=])
        expression (into [:and [:literal true] [:not (first atoms)]
                          [:implies (second atoms) (nth atoms 2)]] atoms)
        rule {:readers #{:fixture/x} :expression expression}
        r (congruence/derive! rule types)
        tree (:derivation r)
        comparisons (filter #(= :same-comparison-observations (:rule %)) (nodes tree))]
    (is (= :conditional-abstract-congruence (:status r)) (pr-str r))
    (is (= :binary-left-fold (:composition tree)))
    (is (= (- (count expression) 2) (:applications tree)))
    (is (= {:= [:equal] :not= [:equal] :< [:below] :<= [:below :equal]
            :> [:above] :>= [:above :equal]}
           (into {} (map (fn [n] [(first (:source n)) (:observations n)])) comparisons)))
    (is (every? #(= (:source %) [(first (:source %)) [:read (:reader %)] [:literal (:literal %)]]) comparisons))
    (is (every? #(= :entails (get-in % [:local-proof :entailment :result])) comparisons))))

(deftest local-proof-rejects-noncongruence-and-unqualified-boundaries
  (let [rule {:readers #{:fixture/x} :expression [:>= [:read :fixture/x] [:literal 0]]}
        types {:fixture/x {:type :integer}}
        run #(congruence/derive! rule types)]
    (is (thrown? clojure.lang.ExceptionInfo
                 (#'congruence/local! 2 #(into [:and] %) #(into [:or] %))))
    (is (= :not-established (:status (congruence/derive!
                                      (assoc rule :expression [:= [:read :fixture/x] [:read :fixture/x]]) types))))
    (doseq [v [#'partition/audit-rule! #'semantics/enumerate-entailment!]]
      (with-redefs-fn {v (constantly {:result :entails :status :matched})}
        #(is (= :congruence-boundary-replaced (:reason (run))))))
    (with-redefs-fn {#'congruence/original-entailment (constantly {:result :entails})}
      #(is (= :congruence-boundary-replaced (:reason (run)))))
    (is (= :conditional-abstract-congruence (:status (run))))))
