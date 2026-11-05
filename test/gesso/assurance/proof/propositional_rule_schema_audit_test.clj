(ns gesso.assurance.proof.propositional-rule-schema-audit-test
  (:require [clojure.test :refer :all]
            [gesso.assurance.proof.propositional-rule-schema-audit :as audit]
            [gesso.assurance.proof.propositional-semantics :as sem]))

(def p [:atom {:readers #{:state/p} :expression [:present [:read :state/p]]}])
(def q [:atom {:readers #{:state/q} :expression [:present [:read :state/q]]}])
(def r [:atom {:readers #{:state/r} :expression [:present [:read :state/r]]}])
(def examples
  [{:rule :premise :premise-id :premise/p :formula p}
   {:rule :truth-introduction}
   {:rule :implication-reflexivity :formula [:and p [:not q]]}
   {:rule :conjunction-introduction :sources [p q r]}
   {:rule :conjunction-elimination :conjunction [:and p q r] :index 1}
   {:rule :implication-elimination :implication [:implies p q] :antecedent p}
   {:rule :disjunction-introduction :source q :disjunction [:or p q r] :index 1}])
(defn reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest seven-correct-source-instances
  (doseq [source examples]
    (let [report (audit/audit-instance! source)
          ev (:checked-semantics report)]
      (is (= :complete-for-instance (:semantic-status report)) (:rule source))
      (is (= :entails (:result ev)) (:rule source))
      (is (pos? (:valuations-checked ev)))
      (is (= :none (:authority report)))
      (is (= :not-established (:universal-schema-soundness report)))
      (is (= :not-established (:production-kernel-correspondence report)))
      (is (= :not-issued (:certificate report)))
      (is (= :not-admitted (:admission report)))
      (is (= :not-interpreted (:typed-atomic-semantics report)))
      (is (not (contains? report :checker))))))

(deftest premise-remains-open
  (let [report (audit/audit-instance! (first examples))]
    (is (= :selected-open-premise (:dependency-policy report)))
    (is (= #{:premise/p} (:open-premises report)))
    (is (= :not-performed (:premise-discharge report)))
    (is (= {:premise/p p} (:hypotheses report)))))

(deftest all-rule-negative-controls
  (let [counterfactuals
        [;; premise reference has been altered to an unrelated conclusion
         {:hypotheses {:premise/p p} :conclusion q}
         ;; truth-introduction has an invalid falsity conclusion
         {:hypotheses {} :conclusion [:false]}
         ;; reflexivity was mutated into a non-tautological implication
         {:hypotheses {} :conclusion [:implies p q]}
         ;; conjunction-introduction adds a conjunct not established
         {:hypotheses {:hypothesis/source-0 p :hypothesis/source-1 q}
          :conclusion [:and p q r]}
         ;; conjunction-elimination selects a non-member
         {:hypotheses {:hypothesis/source-0 [:and p q]}
          :conclusion r}
         ;; implication-elimination draws an unrelated conclusion
         {:hypotheses {:hypothesis/source-0 [:implies p q]
                       :hypothesis/source-1 p}
          :conclusion r}
         ;; disjunction-introduction drops the source
         {:hypotheses {:hypothesis/source-0 p}
          :conclusion [:or q r]}]]
    (doseq [[i counterfactual] (map-indexed vector counterfactuals)]
      (let [report (audit/audit-hypothetical! counterfactual)
            checked (:checked-semantics report)]
        (is (= :counterexample (:semantic-status report)) i)
        (is (= :counterexample (:result checked)) i)
        (is (false? (sem/entailment-at! (:hypotheses counterfactual)
                                        (:conclusion counterfactual)
                                        (:valuation checked))) i)
        (is (= :counterfactual-opaque-boolean-entailment-only
               (:scope report)))
        (is (= :none (:rule report)))
        (is (= :none (:authority report)))))))

(deftest vacuous-is-not-positive
  (let [report (audit/audit-hypothetical!
                 {:hypotheses {:premise/contradiction [:false]}
                  :conclusion p})]
    (is (= :vacuous-hypotheses (:semantic-status report)))
    (is (= :vacuously-entails
           (get-in report [:checked-semantics :result])))
    (is (zero? (get-in report [:checked-semantics
                               :premise-satisfying-valuations])))))

(deftest over-budget-is-not-positive
  (let [atoms (mapv (fn [i]
                      [:atom {:readers #{(keyword "s" (str i))}
                              :expression [:present [:read (keyword "s" (str i))]]}])
                    (range 13))
        report (audit/audit-instance!
                {:rule :implication-reflexivity :formula (into [:and] atoms)})]
    (is (= :not-checked (:semantic-status report)))
    (is (= :not-checked (get-in report [:checked-semantics :result])))
    (is (= :atom-budget (get-in report [:checked-semantics :reason])))
    (is (zero? (get-in report [:checked-semantics :valuations-checked])))))

(deftest seven-rule-catalog-check
  (let [catalog (into {} (map (fn [ex] [(:rule ex) [ex]]) examples))
        report (audit/audit-catalog! catalog)]
    (is (= :all-supplied-instances-complete (:result report)))
    (is (= 7 (:instances-checked report)))
    (is (= {:complete-for-instance 7} (:status-counts report)))
    (is (= 7 (count (:rules report))))
    (is (= 7 (count (:instance-reports report))))
    (is (= :not-established (:universal-schema-soundness report)))
    (is (= :none (:authority report)))))

(deftest catalog-fail-closed-on-unchecked
  (let [atoms (mapv (fn [i]
                      [:atom {:readers #{(keyword "s" (str i))}
                              :expression [:present [:read (keyword "s" (str i))]]}])
                    (range 13))
        catalog (into {} (map (fn [ex] [(:rule ex) [ex]]) examples))
        amended (assoc catalog :implication-reflexivity
                       [{:rule :implication-reflexivity :formula (into [:and] atoms)}])
        report (audit/audit-catalog! amended)]
    (is (= :not-checked (:result report)))
    (is (= 1 (get-in report [:status-counts :not-checked])))
    (is (= :none (:authority report)))))

(deftest strict-boundary-rejections
  (let [catalog (into {} (map (fn [ex] [(:rule ex) [ex]]) examples))]
    (is (= :wrong-keys (reason #(audit/audit-catalog! (dissoc catalog :premise)))))
    (is (= :wrong-keys (reason #(audit/audit-catalog! (assoc catalog :unknown [])))))
    (is (= :invalid-rule-instances
           (reason #(audit/audit-catalog! (assoc catalog :premise [])))))
    (is (= :wrong-rule-for-catalog-entry
           (reason #(audit/audit-catalog!
                      (assoc catalog :premise [{:rule :truth-introduction}])))))
    (is (= :wrong-source-keys (reason #(audit/audit-instance!
                                 {:rule :truth-introduction :semantic-status :approved}))))
    (is (= :wrong-keys (reason #(audit/audit-hypothetical!
                                 {:hypotheses {} :conclusion [:false] :rule :premise}))))
    (is (= :metadata-not-allowed
           (reason #(audit/audit-hypothetical!
                      (with-meta {:hypotheses {} :conclusion [:false]} {:forged true})))))
    (is (= :wrong-keys (reason #(audit/audit-catalog! (assoc catalog :injected [])))))))

(deftest complete-instance-check-is-not-general-rule-proof
  (let [r (audit/audit-instance! {:rule :implication-reflexivity :formula p})]
    (is (= :complete-for-instance (:semantic-status r)))
    (is (= :trusted-v1332-generator-not-independently-proved
           (:source-rule-construction r)))
    (is (= :not-established (:universal-schema-soundness r)))
    (is (= :not-established (:production-kernel-correspondence r)))))

;; v1334: permanent adversarial regression coverage for the v1333 audit.
;; These tests check finite Boolean instances, not general rule theorems,
;; typed-atom truth, the production kernel, or certificate admission.

(defn source-catalog
  []
  (into {} (map (fn [source] [(:rule source) [source]]) examples)))

(defn independent-atom
  [index]
  (let [k (keyword "audit" (str "field-" index))]
    [:atom {:readers #{k} :expression [:present [:read k]]}]))

(deftest nested-satisfiable-instances-across-all-seven-rules
  (doseq [f [p [:not p] [:and p q] [:or q [:not r]]]]
    (let [related [:or f q]
          sources [{:rule :premise :premise-id :premise/p :formula f}
                   {:rule :truth-introduction}
                   {:rule :implication-reflexivity :formula f}
                   {:rule :conjunction-introduction :sources [f related]}
                   {:rule :conjunction-elimination
                    :conjunction [:and f related] :index 0}
                   {:rule :implication-elimination
                    :implication [:implies f related] :antecedent f}
                   {:rule :disjunction-introduction
                    :source f :disjunction [:or q f] :index 1}]]
      (doseq [source sources]
        (let [report (audit/audit-instance! source)]
          (is (= :complete-for-instance (:semantic-status report))
              (pr-str source))
          (is (= :entails (get-in report [:checked-semantics :result])))
          (is (= :none (:authority report)))
          (is (= :not-established (:universal-schema-soundness report))))))))

(deftest repeated-atoms-are-one-boolean-variable
  (let [instance (audit/audit-instance!
                  {:rule :implication-reflexivity
                   :formula [:and p p [:not p]]})]
    (is (= :complete-for-instance (:semantic-status instance)))
    (is (= 1 (get-in instance [:checked-semantics :atom-count])))
    (is (= 2 (get-in instance [:checked-semantics :valuations-checked])))
    (is (= :not-interpreted (:typed-atomic-semantics instance)))))

(deftest open-false-premise-is-vacuous-not-proof
  (let [instance (audit/audit-instance!
                  {:rule :premise :premise-id :premise/false
                   :formula [:false]})
        true-instance (audit/audit-instance! {:rule :truth-introduction})]
    (is (= :vacuous-hypotheses (:semantic-status instance)))
    (is (= #{:premise/false} (:open-premises instance)))
    (is (= 0 (get-in instance [:checked-semantics
                              :premise-satisfying-valuations])))
    (is (= :not-performed (:premise-discharge instance)))
    (is (= :complete-for-instance (:semantic-status true-instance)))
    (is (= 0 (get-in true-instance [:checked-semantics :atom-count])))
    (is (= 1 (get-in true-instance [:checked-semantics :valuations-checked])))))

(deftest catalog-reports-vacuous-source-instances
  (let [catalog (assoc (source-catalog) :premise
                       [{:rule :premise :premise-id :premise/false
                         :formula [:false]}])
        report (audit/audit-catalog! catalog)]
    (is (= :vacuous-instances (:result report)))
    (is (= {:complete-for-instance 6 :vacuous-hypotheses 1}
           (:status-counts report)))
    (is (= :vacuous-hypotheses
           (get-in report [:instance-reports :premise 0 :semantic-status])))
    (is (= :none (:authority report)))
    (is (= :not-issued (:certificate report)))))

(deftest catalog-rejects-missing-extra-and-relabelled-rule-families
  (let [catalog (source-catalog)]
    (is (= :wrong-keys
           (reason #(audit/audit-catalog! (dissoc catalog :implication-elimination)))))
    (is (= :wrong-keys
           (reason #(audit/audit-catalog! (assoc catalog :extra [])))))
    (is (= :wrong-rule-for-catalog-entry
           (reason #(audit/audit-catalog!
                      (assoc catalog :premise [{:rule :truth-introduction}])))))
    (is (= :invalid-rule-instances
           (reason #(audit/audit-catalog! (assoc catalog :premise [])))))
    (is (= :invalid-instance-source
           (reason #(audit/audit-catalog! (assoc catalog :premise [[:not-a-map]])))))))

(deftest catalog-enforces-sixteen-instances-per-rule
  (let [catalog (source-catalog)
        at-limit (assoc catalog :truth-introduction
                        (vec (repeat 16 {:rule :truth-introduction})))
        beyond (assoc catalog :truth-introduction
                      (vec (repeat 17 {:rule :truth-introduction})))
        result (audit/audit-catalog! at-limit)]
    (is (= :all-supplied-instances-complete (:result result)))
    (is (= 22 (:instances-checked result)))
    (is (= {:complete-for-instance 22} (:status-counts result)))
    (is (= :invalid-rule-instances (reason #(audit/audit-catalog! beyond))))))

(deftest catalog-result-does-not-depend-on-map-insertion-order
  (let [pairs (map (fn [source] [(:rule source) [source]]) examples)
        first-report (audit/audit-catalog! (into (array-map) pairs))
        other-report (audit/audit-catalog! (into (array-map) (reverse pairs)))]
    (is (= first-report other-report))
    (is (= (vec (sort (map :rule examples))) (:rules first-report)))))

(deftest twelve-atom-work-budget-is-not-misreported-as-success
  (let [atoms (mapv independent-atom (range 12))
        ;; 12 distinct atoms fit the atom budget. Repeating a subformula
        ;; crosses only the separate evaluation-work budget.
        repeated (into [:and] (repeat 126 (first atoms)))
        f (into [:and repeated] (subvec atoms 1))
        instance (audit/audit-instance!
                  {:rule :implication-reflexivity :formula f})]
    (is (= 12 (get-in instance [:checked-semantics :atom-count])))
    (is (= :not-checked (:semantic-status instance)))
    (is (= :work-budget (get-in instance [:checked-semantics :reason])))
    (is (= 0 (get-in instance [:checked-semantics :valuations-checked])))
    (is (= :none (:authority instance)))))

(deftest thirteen-atom-budget-dominates-vacuous-catalog-instances
  (let [large (into [:and] (mapv independent-atom (range 13)))
        catalog (-> (source-catalog)
                    (assoc :premise [{:rule :premise
                                      :premise-id :premise/false
                                      :formula [:false]}])
                    (assoc :implication-reflexivity
                           [{:rule :implication-reflexivity :formula large}]))
        report (audit/audit-catalog! catalog)]
    (is (= :not-checked (:result report)))
    (is (= 1 (get-in report [:status-counts :not-checked])))
    (is (= 1 (get-in report [:status-counts :vacuous-hypotheses])))
    (is (= :not-established (:universal-schema-soundness report)))))

(deftest source-order-and-selected-index-are-preserved
  (let [introduction (audit/audit-instance!
                      {:rule :conjunction-introduction :sources [r p q]})
        elimination (audit/audit-instance!
                     {:rule :conjunction-elimination
                      :conjunction [:and r p q] :index 2})]
    (is (= [:and r p q] (:conclusion introduction)))
    (is (= [:hypothesis/source-0
            :hypothesis/source-1
            :hypothesis/source-2]
           (:hypothesis-order introduction)))
    (is (= r (get-in introduction [:hypotheses :hypothesis/source-0])))
    (is (= p (get-in introduction [:hypotheses :hypothesis/source-1])))
    (is (= q (get-in introduction [:hypotheses :hypothesis/source-2])))
    (is (= q (:conclusion elimination)))
    (is (= :complete-for-instance (:semantic-status elimination)))))

(deftest malformed-sources-do-not-become-hypothetical-reports
  (doseq [[source expected-reason]
          [[{:rule :premise :premise-id :unqualified :formula p}
            :invalid-premise-id]
           [{:rule :truth-introduction :semantic-status :complete-for-instance}
            :wrong-source-keys]
           [{:rule :implication-reflexivity :formula [:xor p q]}
            :invalid-formula]
           [{:rule :conjunction-introduction :sources [p]}
            :invalid-formula-count]
           [{:rule :conjunction-elimination
             :conjunction [:and p q] :index 9}
            :invalid-index]
           [{:rule :implication-elimination
             :implication [:implies p q] :antecedent q}
            :antecedent-mismatch]
           [{:rule :disjunction-introduction
             :source p :disjunction [:or q r] :index 0}
            :disjunct-mismatch]]]
    (is (= expected-reason (reason #(audit/audit-instance! source)))
        (pr-str source))))

(deftest metadata-and-executable-payloads-fail-closed
  (let [catalog (source-catalog)
        malicious-atom [:atom {:readers #{:state/p}
                               :expression [:call (fn [] true)]}]]
    (is (= :metadata-not-allowed
           (reason #(audit/audit-catalog!
                      (assoc catalog :premise
                             (with-meta [(first examples)] {:forged true}))))))
    (is (= :metadata-not-allowed
           (reason #(audit/audit-catalog!
                      (with-meta catalog {:forged true})))))
    (is (= :metadata-not-allowed
           (reason #(audit/audit-hypothetical!
                      {:hypotheses (with-meta {:premise/p p} {:forged true})
                       :conclusion p}))))
    (is (= :invalid-formula
           (reason #(audit/audit-instance!
                      {:rule :implication-reflexivity
                       :formula (with-meta p {:forged true})}))))
    (is (= :invalid-formula
           (reason #(audit/audit-instance!
                      {:rule :implication-reflexivity
                       :formula malicious-atom}))))
    (is (= :executable-or-unsupported-atom
           (reason #(audit/audit-hypothetical!
                      {:hypotheses {} :conclusion malicious-atom}))))))

(deftest hypothetical-mutation-controls-are-never-admitted
  (let [mutation {:hypotheses {:premise/p p}
                  :conclusion q}
        first-report (audit/audit-hypothetical! mutation)
        second-report (audit/audit-hypothetical! mutation)
        valuation (get-in first-report [:checked-semantics :valuation])]
    (is (= first-report second-report))
    (is (= :counterexample (:semantic-status first-report)))
    (is (= true (get valuation (second p))))
    (is (= false (get valuation (second q))))
    (is (false? (sem/entailment-at! (:hypotheses mutation)
                                   (:conclusion mutation) valuation)))
    (is (= :none (:rule first-report)))
    (is (= :none (:authority first-report)))
    (is (= :not-admitted (:admission first-report)))
    (is (= :not-issued (:certificate first-report)))
    (is (= :wrong-keys
           (reason #(audit/audit-hypothetical!
                      (assoc mutation :authority :certified)))))
    (is (= :wrong-source-keys
           (reason #(audit/audit-instance!
                      (assoc (second examples)
                             :semantic-status :complete-for-instance)))))))

(deftest atomic-identity-distinguishes-declarations
  (let [p-other [:atom {:readers #{:state/other}
                        :expression [:present [:read :state/p]]}]
        report (audit/audit-hypothetical!
                {:hypotheses {:premise/p p} :conclusion p-other})
        check (:checked-semantics report)]
    (is (= 2 (:atom-count check)))
    (is (= :counterexample (:result check)))
    (is (false? (sem/entailment-at! {:premise/p p} p-other
                                    (:valuation check))))
    (is (= :not-interpreted (:typed-atomic-semantics report)))))
