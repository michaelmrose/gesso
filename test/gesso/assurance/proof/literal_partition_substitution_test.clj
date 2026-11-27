(ns gesso.assurance.proof.literal-partition-substitution-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.proof.literal-partition :as partition]
            [gesso.assurance.proof.literal-partition-substitution :as substitution]
            [gesso.assurance.proof.typed-presence-semantics :as presence]
            [gesso.assurance.rule-eval :as production]
            [gesso.live.outbox.claim-semantic-fixture :as claim]))

(deftest integer-substitution-preserves-complete-truth-tree
  (let [types {:fixture/x {:type :integer}}
        rule {:readers #{:fixture/x}
              :expression [:implies [:not [:< [:read :fixture/x] [:literal 0]]]
                           [:and [:<= [:read :fixture/x] [:literal 10]]
                            [:or [:> [:read :fixture/x] [:literal 0]]
                             [:>= [:read :fixture/x] [:literal 10]]
                             [:= [:read :fixture/x] [:literal 0]]]
                            [:not= [:read :fixture/x] [:literal 10]]]]}]
    (doseq [[x rep] [[Long/MIN_VALUE -1] [-5 -1] [0 0] [1 1] [9 1] [10 10]
                     [11 11] [Long/MAX_VALUE 11] [922337203685477580800N 11]]]
      (let [r (substitution/check-substitution! rule types {:fixture/x x})]
        (is (= :matched (:status r)) (pr-str r))
        (is (= {:fixture/x rep} (:representative-bindings r)))
        (is (= (< x 10) (:value r)))
        (is (= :checked-full-truth-tree (:atom-and-connective-agreement r)))
        (is (= :not-established (:universal-evaluator-correspondence r)))
        (is (= :not-performed (:proof-discharge r)))))))

(deftest original-claim-expressions-substitute-outside-named-values
  (let [catalog (claim/catalog)
        state {:request/status :unexpected :request/revision 500000000000000000000N
               :assignment/helper "outside-helpers" :journal/one nil
               :journal/two :unrecognized :membership/one true :membership/two false}]
    (doseq [[id rule] (assoc (:invariants catalog) :fixture/init (:initial-condition catalog))]
      (let [bindings (select-keys state (:readers rule))
            r (substitution/check-substitution! rule claim/state-types bindings)
            expected (:value (production/evaluate-rule! rule (select-keys claim/state-types (:readers rule)) bindings))]
        (is (= :matched (:status r)) (str id " " (pr-str r)))
        (is (= expected (:value r)))
        (is (= (:readers rule) (set (keys (:representative-bindings r)))))
        (when (contains? bindings :journal/one)
          (is (contains? (:representative-bindings r) :journal/one))
          (is (nil? (get-in r [:representative-bindings :journal/one]))))))))

(deftest substitution-is-fail-closed
  (let [types {:fixture/x {:type :string :nullable? true}}
        rule {:readers #{:fixture/x} :expression [:= [:read :fixture/x] [:literal "gesso.partition/other-0"]]}
        run #(substitution/check-substitution! rule types %)]
    (doseq [[v expected] [[nil nil] ["gesso.partition/other-0" "gesso.partition/other-0"]
                          ["unlisted" "gesso.partition/other-1"]]]
      (let [r (run {:fixture/x v})]
        (is (= :matched (:status r)) (pr-str r))
        (is (= {:fixture/x expected} (:representative-bindings r)))))
    (doseq [bindings [{} {:fixture/x 2} {:fixture/x "x" :fixture/y true}]]
      (let [r (run bindings)]
        (is (= :not-established (:status r)))
        (is (not (contains? r :value)))))
    (doseq [v [#'partition/audit-rule! #'presence/evaluate-expression! #'production/evaluate-rule!]]
      (with-redefs-fn {v (constantly {:status :matched :value true})}
        #(is (= :substitution-boundary-replaced (:reason (run {:fixture/x "x"}))))))
    (is (= :not-established (:status (substitution/check-substitution!
                                      rule (assoc-in types [:fixture/x :optional?] true) {}))))))

(deftest boolean-nil-and-masked-atom-substitution
  (let [types {:fixture/b {:type :boolean :nullable? true}}
        rule {:readers #{:fixture/b} :expression [:not= [:read :fixture/b] [:literal nil]]}]
    (doseq [v [true false nil]]
      (let [r (substitution/check-substitution! rule types {:fixture/b v})]
        (is (= :matched (:status r)) (pr-str r))
        (is (= {:fixture/b v} (:representative-bindings r)))
        (is (= (some? v) (:value r))))))
  (let [types {:fixture/x {:type :integer}}
        rule {:readers #{:fixture/x}
              :expression [:or [:literal true] [:= [:read :fixture/x] [:literal 0]]]}]
    ;; Equal root results do not excuse disagreement in a skipped atom.
    (with-redefs-fn {#'substitution/representative! (constantly 0)}
      #(let [r (substitution/check-substitution! rule types {:fixture/x 5})]
         (is (= :not-established (:status r)))
         (is (= :partition-substitution-mismatch (:reason r)))
         (is (not (contains? r :value)))))))
