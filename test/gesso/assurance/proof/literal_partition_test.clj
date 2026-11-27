(ns gesso.assurance.proof.literal-partition-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.proof.literal-partition :as partition]
            [gesso.assurance.proof.typed-atom-oracle :as oracle]
            [gesso.assurance.rule-eval :as production]
            [gesso.live.outbox.claim-semantic-fixture :as claim]))

(deftest original-claim-rules-cover-literal-partitions
  (let [catalog (claim/catalog)
        results (into {} (map (fn [[id rule]] [id (partition/audit-rule! rule claim/state-types)]))
                      (assoc (:invariants catalog) :fixture/init (:initial-condition catalog)))
        ownership (:claim/ownership-and-journal results)
        revision (:claim/nonnegative-revision results)]
    (doseq [[id report] results]
      (is (= :matched (:status report)) (str id " " (pr-str report)))
      (is (= :not-established (:universal-evaluator-correspondence report)))
      (is (= :not-established (:executable-partition-invariance report)))
      (is (= :not-performed (:proof-discharge report)))
      (is (pos? (:true-cases report)))
      (is (pos? (:false-cases report))))
    (is (= (:claim/ownership-and-journal (:invariants catalog)) (:rule ownership)))
    (is (= #{:open :claimed} (get-in ownership [:comparison-literals :request/status])))
    (is (= 3 (count (get-in ownership [:representative-domains :request/status]))))
    (is (= 4 (count (get-in ownership [:representative-domains :assignment/helper]))))
    (is (= #{-1 0 1} (set (get-in revision [:representative-domains :request/revision]))))))

(deftest literal-partition-is-source-derived-and-fail-closed
  (let [types {:fixture/x {:type :integer}}
        rule {:readers #{:fixture/x}
              :expression [:or [:< [:read :fixture/x] [:literal 0]]
                           [:>= [:read :fixture/x] [:literal 10]]]}
        audit #(partition/audit-rule! % types)
        checked (audit rule)]
    (is (= :matched (:status checked)))
    (is (= #{-1 0 1 10 11} (set (get-in checked [:representative-domains :fixture/x]))))
    (is (= 5 (:cases-checked checked)))
    (is (= 3 (:true-cases checked)))
    (doseq [expression [[:= [:read :fixture/x] [:read :fixture/x]]
                        [:>= [:literal 0] [:read :fixture/x]]
                        [:present [:read :fixture/x]]
                        [:>= [:read :fixture/x] [:literal "0"]]
                        [:= 1 2]]]
      (is (= :unsupported (:status (audit (assoc rule :expression expression))))))
    (is (= :unsupported (:status (partition/audit-rule! rule (assoc-in types [:fixture/x :optional?] true)))))
    (with-redefs [oracle/audit-typed-atom! (constantly {:status :bounded-agreement})]
      (is (= :unsupported (:status (audit rule)))))
    (let [original production/evaluate-rule!]
      (with-redefs [production/evaluate-rule! (fn [& args] (update (apply original args) :value not))]
        (is (= :not-established (:status (audit rule))))))))
