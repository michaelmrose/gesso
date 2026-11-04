(ns gesso.assurance.proof.propositional-soundness-test
  "Permanent adversarial tests for v1318's INDEPENDENT finite Boolean audit.

   These tests use original raw kernel derivation sources. Kernel agreement is
   checked only where the source also satisfies its stricter typed rules;
   Boolean validity alone does not imply kernel admission. The oracle treats
   every typed atom as an opaque total Boolean, and cannot certify atomic
   typing, mathematical soundness, or any runtime/transaction authority.

   Each counterexample is checked against the offending formula, not merely
   against a reported status. Enumeration/budget counts are checked as well."
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.proof.kernel :as kernel]
            [gesso.assurance.proof.propositional-soundness :as audit]))

(def ^:private subject
  {:kind :operation :id :example/advance
   :interface {:operation :example/advance}})
(def ^:private p
  [:atom {:readers #{:result/a}
          :expression [:> [:read :result/a] [:literal 0]]}])
(def ^:private q
  [:atom {:readers #{:result/b}
          :expression [:> [:read :result/b] [:literal 0]]}])
(def ^:private r
  [:atom {:readers #{:result/c}
          :expression [:> [:read :result/c] [:literal 0]]}])
(def ^:private types
  {:result/a {:type :integer} :result/b {:type :integer}
   :result/c {:type :integer}})

(defn- source
  [goal premises steps]
  (let [nodes (tree-seq coll? seq [goal premises steps])
        readers (into #{} (mapcat :readers) (filter #(and (map? %)
                                                           (contains? % :readers))
                                                     nodes))]
    {:subject subject :variables (select-keys types readers)
     :premises premises :steps steps :goal goal}))

(defn- prem [id premise conclusion]
  {:id id :rule :premise :premise premise :conclusion conclusion})

(defn- infer [id rule conclusion & args]
  (merge {:id id :rule rule :conclusion conclusion}
         (apply hash-map args)))

(defn- audit! [src] (audit/audit-derivation! src))

(defn- rejection [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- rejected-as!
  [reason at f]
  (let [failure (rejection f)]
    (is (map? failure))
    (is (= ::audit/invalid-source (:error/type failure)))
    (is (= reason (:reason failure)))
    (is (= at (:at failure)))))

(defn- no-authority!
  [result]
  (is (= audit/report-type (:type result)))
  (is (= audit/oracle-version (:version result)))
  (is (= :independent-bounded-propositional-skeleton-only (:scope result)))
  (is (= :opaque-total-booleans-assumed (:atomic-semantics result)))
  (is (= :not-performed (:source-type-validation result)))
  (is (= :not-performed (:assumption-discharge result)))
  (is (= :not-established (:physical-refinement result)))
  (is (= :not-established (:checker-soundness result)))
  (is (= :not-issued (:certificate result)))
  (is (= :not-established (:runtime-enforcement result)))
  (is (= :none (:authority result)))
  (is (not (contains? result :admission)))
  result)

(defn- valid-modus-ponens []
  (source q
          {:premise/implication [:implies p q] :premise/antecedent p}
          [(prem :step/implication :premise/implication [:implies p q])
           (prem :step/antecedent :premise/antecedent p)
           (infer :step/consequent :implication-elimination q
                  :from [:step/implication :step/antecedent])]))

(defn- validated-by-both!
  [src expected-valuations expected-steps]
  (let [report (no-authority! (audit! src))
        checked (kernel/check-derivation! src)]
    (is (= :bounded-no-counterexample (:result report)))
    (is (= expected-valuations (:valuations-checked report)))
    (is (= (* expected-valuations expected-steps)
           (:step-implications-tested report)))
    (is (= expected-valuations (:goal-implications-tested report)))
    (is (= :none (:authority checked)))
    (is (= :not-issued (:certificate checked)))
    report))

(deftest unconditional-true-is-a-complete-zero-atom-enumeration
  (let [src (source [:true] {} [(infer :step/true :truth-introduction [:true])])
        report (validated-by-both! src 1 1)]
    (is (= 0 (:atom-count report)))
    (is (= [] (:atoms report)))
    (is (= 1 (:premise-satisfying-valuations report)))))

(deftest premise-introduction-preserves-its-open-assumption
  (let [src (source p {:premise/p p} [(prem :step/p :premise/p p)])
        report (validated-by-both! src 2 1)
        checked (kernel/check-derivation! src)]
    (is (= 1 (:atom-count report)))
    (is (= 1 (:premise-satisfying-valuations report)))
    (is (= #{:premise/p} (:open-premises checked)))
    (is (= :conditional-on-open-premises (:conclusion-status checked)))))

(deftest implication-reflexivity-covers-true-and-false-atom
  (let [src (source [:implies p p] {}
                    [(infer :step/refl :implication-reflexivity [:implies p p])])
        report (validated-by-both! src 2 1)]
    (is (= 2 (:premise-satisfying-valuations report)))
    (is (= 1 (:atom-count report)))))

(deftest modus-ponens-is-valid-under-all-four-assignments
  (let [report (validated-by-both! (valid-modus-ponens) 4 3)]
    (is (= 2 (:atom-count report)))
    (is (= 1 (:premise-satisfying-valuations report)))
    (is (= (sort-by pr-str (:atoms report)) (:atoms report)))))

(deftest conjunction-introduction-uses-every-input-step
  (let [src (source [:and p q] {:premise/p p :premise/q q}
                    [(prem :step/p :premise/p p)
                     (prem :step/q :premise/q q)
                     (infer :step/pq :conjunction-introduction [:and p q]
                            :from [:step/p :step/q])])
        report (validated-by-both! src 4 3)]
    (is (= 1 (:premise-satisfying-valuations report)))))

(deftest conjunction-elimination-selects-the-correct-child
  (let [both [:and p q r]
        src (source q {:premise/all both}
                    [(prem :step/all :premise/all both)
                     (infer :step/q :conjunction-elimination q
                            :from [:step/all] :index 1)])]
    (is (= 1 (:premise-satisfying-valuations
              (validated-by-both! src 8 2))))))

(deftest disjunction-introduction-uses-declared-index
  (let [conclusion [:or [:false] p q]
        src (source conclusion {:premise/p p}
                    [(prem :step/p :premise/p p)
                     (infer :step/or :disjunction-introduction conclusion
                            :from [:step/p] :index 1)])]
    (is (= 2 (:premise-satisfying-valuations
              (validated-by-both! src 4 2))))))

(deftest chained-implication-elimination-composes
  (let [src (source r
                    {:premise/p p
                     :premise/pq [:implies p q]
                     :premise/qr [:implies q r]}
                    [(prem :step/pq :premise/pq [:implies p q])
                     (prem :step/p :premise/p p)
                     (infer :step/q :implication-elimination q
                            :from [:step/pq :step/p])
                     (prem :step/qr :premise/qr [:implies q r])
                     (infer :step/r :implication-elimination r
                            :from [:step/qr :step/q])])]
    (is (= 1 (:premise-satisfying-valuations
              (validated-by-both! src 8 5))))))

(deftest combinations-of-boolean-connectives-use-truth-functions
  (let [tautology [:implies [:and p [:not q]] [:or p r]]
        src (source tautology {}
                    [(infer :step/semantic-tautology
                            :implication-reflexivity tautology)])
        report (no-authority! (audit! src))]
    (is (= :bounded-no-counterexample (:result report)))
    (is (= 8 (:valuations-checked report)))
    (is (= 8 (:premise-satisfying-valuations report)))
    (is (= :invalid-implication-reflexivity
           (:reason (rejection #(kernel/check-derivation! src)))))))

(deftest falsity-cannot-be-introduced-unconditionally
  (let [src (source [:false] {}
                    [(infer :step/false :truth-introduction [:false])])
        report (no-authority! (audit! src))]
    (is (= :counterexample (:result report)))
    (is (= :invalid-step-entailment (:failure-kind report)))
    (is (= :step/false (:step-id report)))
    (is (= 0 (:step-index report)))
    (is (= {} (:valuation report)))
    (is (= 0 (:valuation-index report)))
    (is (= 1 (:valuations-checked report)))
    (is (= 1 (:step-implications-tested report)))
    (is (= 0 (:goal-implications-tested report)))))

(deftest false-reflexivity-has-a-reproducible-countervaluation
  (let [src (source [:implies p q] {}
                    [(infer :step/bad :implication-reflexivity [:implies p q])])
        report (audit! src)
        valuation (:valuation report)]
    (is (= :counterexample (:result report)))
    (is (= :step/bad (:step-id report)))
    (is (true? (get valuation (second p))))
    (is (false? (get valuation (second q))))
    (is (= [:implies p q] (:conclusion report)))
    (is (= report (audit! src)))))

(deftest premise-substitution-produces-a-concrete-countermodel
  (let [src (source q {:premise/p p}
                    [(prem :step/wrong :premise/p q)])
        report (audit! src)]
    (is (= :counterexample (:result report)))
    (is (= :step/wrong (:step-id report)))
    (is (= [p] (:premises report)))
    (is (true? (get (:valuation report) (second p))))
    (is (false? (get (:valuation report) (second q))))
    (is (= :wrong-premise-conclusion
           (:reason (rejection #(kernel/check-derivation! src)))))))

(deftest invalid-conjunction-introduction-cannot-invent-third-atom
  (let [src (source [:and p r] {:premise/p p :premise/q q}
                    [(prem :step/p :premise/p p)
                     (prem :step/q :premise/q q)
                     (infer :step/fake :conjunction-introduction [:and p r]
                            :from [:step/p :step/q])])
        report (audit! src)]
    (is (= :counterexample (:result report)))
    (is (= :step/fake (:step-id report)))
    (is (= [p q] (:premises report)))
    (is (true? (get (:valuation report) (second p))))
    (is (true? (get (:valuation report) (second q))))
    (is (false? (get (:valuation report) (second r))))))

(deftest invalid-conjunction-elimination-cannot-pick-unrelated-atom
  (let [src (source p {:premise/qr [:and q r]}
                    [(prem :step/qr :premise/qr [:and q r])
                     (infer :step/fake :conjunction-elimination p
                            :from [:step/qr] :index 0)])
        report (audit! src)]
    (is (= :counterexample (:result report)))
    (is (= :step/fake (:step-id report)))
    (is (false? (get (:valuation report) (second p))))
    (is (true? (get (:valuation report) (second q))))
    (is (true? (get (:valuation report) (second r))))))

(deftest implication-elimination-needs-its-actual-implication
  (let [src (assoc-in (valid-modus-ponens) [:steps 2 :from]
                      [:step/antecedent :step/antecedent])
        report (audit! src)]
    (is (= :counterexample (:result report)))
    (is (= :step/consequent (:step-id report)))
    (is (true? (get (:valuation report) (second p))))
    (is (false? (get (:valuation report) (second q))))
    (is (= [p p] (:premises report)))))

(deftest disjunction-introduction-cannot-omit-the-introduced-proposition
  (let [src (source [:or q r] {:premise/p p}
                    [(prem :step/p :premise/p p)
                     (infer :step/fake :disjunction-introduction [:or q r]
                            :from [:step/p] :index 0)])
        report (audit! src)]
    (is (= :counterexample (:result report)))
    (is (= :step/fake (:step-id report)))
    (is (true? (get (:valuation report) (second p))))
    (is (false? (get (:valuation report) (second q))))
    (is (false? (get (:valuation report) (second r))))))

(deftest contradictions-are-reported-as-vacuous-not-proved
  (let [src (source [:false] {:premise/contradiction [:false]}
                    [(prem :step/contradiction :premise/contradiction [:false])])
        report (no-authority! (audit! src))]
    (is (= :vacuous-premises (:result report)))
    (is (= 0 (:premise-satisfying-valuations report)))
    (is (= 1 (:valuations-checked report)))
    (is (= 1 (:step-implications-tested report)))
    (is (= 1 (:goal-implications-tested report)))))

(deftest contradictory-global-premise-does-not-mask-invalid-local-step
  (let [src (source q {:premise/false [:false]}
                    [(prem :step/false :premise/false [:false])
                     (infer :step/fake :truth-introduction q)])
        report (no-authority! (audit! src))]
    (is (= :counterexample (:result report)))
    (is (= :invalid-step-entailment (:failure-kind report)))
    (is (= :step/fake (:step-id report)))
    (is (= 0 (:premise-satisfying-valuations report)))))

(deftest deterministic-valuations-and-step-indices-across-map-order
  (let [original (valid-modus-ponens)
        reversed (assoc original :premises
                        (array-map :premise/antecedent p
                                   :premise/implication [:implies p q]))
        bad (assoc-in original [:steps 2 :from]
                      [:step/antecedent :step/antecedent])
        bad-reordered (assoc bad :premises (:premises reversed))]
    (is (= (audit! original) (audit! reversed)))
    (is (= (audit! bad) (audit! bad-reordered)))
    (is (= 2 (count (:atoms (audit! original)))))))

(deftest exact-valuation-limit-is-allowed-but-one-less-fails
  (let [src (source [:implies p [:or p q r]] {}
                    [(infer :step/tautology :implication-reflexivity
                            [:implies p [:or p q r]])])
        complete (audit/audit-derivation! src {:max-valuations 8})
        partial (audit/audit-derivation! src {:max-valuations 7})]
    (is (= :bounded-no-counterexample (:result complete)))
    (is (= 8 (:valuations-checked complete)))
    (is (= :budget-exceeded (:result partial)))
    (is (= 8 (:required-valuations partial)))
    (is (= 7 (:valuation-limit partial)))
    (is (= 0 (:valuations-checked partial)))
    (is (= 0 (:step-implications-tested partial)))
    (is (= 0 (:goal-implications-tested partial)))
    (no-authority! partial)))

(deftest default-valuation-budget-refuses-thirteen-independent-atoms
  (let [fields (mapv #(keyword "result" (str "field" %)) (range 13))
        atoms (mapv (fn [field]
                      [:atom {:readers #{field}
                              :expression [:present [:read field]]}]) fields)
        goal (into [:and] atoms)
        src {:subject subject
             :variables (zipmap fields (repeat {:type :boolean}))
             :premises {:premise/all goal}
             :steps [(prem :step/all :premise/all goal)] :goal goal}
        report (no-authority! (audit! src))]
    (is (= :budget-exceeded (:result report)))
    (is (= 13 (:atom-count report)))
    (is (= 8192 (:required-valuations report)))
    (is (= 4096 (:valuation-limit report)))
    (is (= 0 (:valuations-checked report)))))

(deftest too-many-atoms-use-an-explicit-overflow-status
  (let [fields (mapv #(keyword "result" (str "field" %)) (range 21))
        atoms (mapv (fn [field]
                      [:atom {:readers #{field}
                              :expression [:present [:read field]]}]) fields)
        goal (into [:and] atoms)
        src {:subject subject :variables (zipmap fields (repeat {:type :boolean}))
             :premises {:premise/all goal}
             :steps [(prem :step/all :premise/all goal)] :goal goal}
        report (audit! src)]
    (is (= :budget-exceeded (:result report)))
    (is (= :above-max-atoms (:required-valuations report)))
    (is (= 21 (:atom-count report)))
    (is (= 0 (:valuations-checked report)))))

(deftest malformed-source-shape-is-rejected-before-enumeration
  (let [src (valid-modus-ponens)]
    (rejected-as! :wrong-keys [] #(audit! (assoc src :proved? true)))
    (rejected-as! :expected-plain-map [] #(audit! []))
    (rejected-as! :invalid-variables [:variables]
                  #(audit! (assoc src :variables [])))
    (rejected-as! :invalid-premises [:premises]
                  #(audit! (assoc src :premises [])))
    (rejected-as! :invalid-steps [:steps]
                  #(audit! (assoc src :steps [])))
    (rejected-as! :invalid-id [:variables :unqualified]
                  #(audit! (assoc-in src [:variables :unqualified] {:type :integer})))))

(deftest proof-status-injection-and-unrecognized-step-keys-fail-closed
  (let [src (valid-modus-ponens)]
    (rejected-as! :wrong-keys [] #(audit! (assoc src :certificate :issued)))
    (rejected-as! :wrong-keys [:steps 2]
                  #(audit! (assoc-in src [:steps 2 :authority] :proof)))
    (rejected-as! :wrong-keys [:steps 0]
                  #(audit! (assoc-in src [:steps 0 :checked] true)))))

(deftest unknown-rules-and-references-are-not-axioms
  (let [src (valid-modus-ponens)]
    (rejected-as! :unknown-rule [:steps 2 :rule]
                  #(audit! (assoc-in src [:steps 2 :rule] :proof/axiom)))
    (rejected-as! :unknown-premise [:steps 0 :premise]
                  #(audit! (assoc-in src [:steps 0 :premise] :premise/phantom)))
    (rejected-as! :unknown-or-forward-step [:steps 2 :from]
                  #(audit! (assoc-in src [:steps 2 :from]
                                    [:step/consequent :step/antecedent])))
    (rejected-as! :unknown-or-forward-step [:steps 2 :from]
                  #(audit! (assoc-in src [:steps 2 :from]
                                    [:step/phantom :step/antecedent])))))

(deftest duplicate-step-and-mismatched-final-goal-fail-closed
  (let [src (valid-modus-ponens)]
    (rejected-as! :duplicate-step-id [:steps 2 :id]
                  #(audit! (assoc-in src [:steps 2 :id] :step/implication)))
    (rejected-as! :goal-mismatch [:goal] #(audit! (assoc src :goal p)))))

(deftest malformed-reference-collections-and-indices-fail-closed
  (let [src (valid-modus-ponens)
        src-index (source [:and p q] {:premise/pq [:and p q]}
                          [(prem :step/pq :premise/pq [:and p q])
                           (infer :step/all :conjunction-elimination [:and p q]
                                  :from [:step/pq] :index 0)])]
    (rejected-as! :invalid-sources [:steps 2 :from]
                  #(audit! (assoc-in src [:steps 2 :from] '(:step/implication :step/antecedent))))
    (rejected-as! :invalid-sources [:steps 2 :from]
                  #(audit! (assoc-in src [:steps 2 :from] [:step/implication])))
    (rejected-as! :invalid-index [:steps 1 :index]
                  #(audit! (assoc-in src-index [:steps 1 :index] -1)))
    (rejected-as! :invalid-index [:steps 1 :index]
                  #(audit! (assoc-in src-index [:steps 1 :index] 1.5)))))

(deftest invalid-propositional-connectives-and-arities-fail-closed
  (let [src (valid-modus-ponens)]
    (rejected-as! :unknown-connective [:goal]
                  #(audit! (assoc src :goal [:exclusive-or p q])))
    (rejected-as! :invalid-arity [:goal]
                  #(audit! (assoc src :goal [:implies p])))
    (rejected-as! :invalid-arity [:goal]
                  #(audit! (assoc src :goal [:not p q])))
    (rejected-as! :invalid-arity [:goal]
                  #(audit! (assoc src :goal [:and p])))
    (rejected-as! :invalid-formula [:goal]
                  #(audit! (assoc src :goal true)))))

(deftest invalid-opaque-atoms-and-reader-footprints-fail-closed
  (let [src (valid-modus-ponens)]
    (rejected-as! :variable-footprint-mismatch [:variables]
                  #(audit! (assoc src :variables {})))
    (rejected-as! :invalid-atom-readers [:goal]
                  #(audit! (assoc-in src [:goal 1 :readers] [:result/b])))
    (rejected-as! :invalid-opaque-atom [:goal]
                  #(audit! (assoc-in src [:goal 1 :expression] true)))
    (rejected-as! :invalid-arity [:goal]
                  #(audit! (assoc src :goal [:atom])))))

(deftest root-and-step-metadata-are-not-evidence
  (let [src (valid-modus-ponens)]
    (rejected-as! :metadata-not-allowed []
                  #(audit! (with-meta src {:certified true})))
    (rejected-as! :metadata-not-allowed [:steps 2]
                  #(audit! (update-in src [:steps 2]
                                     with-meta {:certified true})))
    (rejected-as! :metadata-not-allowed [:steps]
                  #(audit! (update src :steps with-meta {:certified true})))
    (rejected-as! :metadata-not-allowed [:premises]
                  #(audit! (update src :premises with-meta {:certified true})))))

(deftest nested-metadata-and-executable-atoms-are-rejected
  (let [src (valid-modus-ponens)]
    (rejected-as! :metadata-not-allowed [:goal 1 :expression]
                  #(audit! (update-in src [:goal 1 :expression]
                                      with-meta {:proof true})))
    (rejected-as! :metadata-not-allowed [:goal 1 :readers]
                  #(audit! (update-in src [:goal 1 :readers]
                                      with-meta {:proof true})))
    (rejected-as! :non-data-atom [:goal 1 :expression 1]
                  #(audit! (assoc-in src [:goal 1 :expression 1]
                                     (fn [] true))))))

(deftest option-keys-types-and-bounds-are-validated
  (let [src (valid-modus-ponens)]
    (rejected-as! :wrong-keys [:options]
                  #(audit/audit-derivation! src {:prove-all true}))
    (doseq [n [nil -1 0 4097 1.25 :all "4096"]]
      (rejected-as! :invalid-valuation-limit [:options :max-valuations]
                    #(audit/audit-derivation! src {:max-valuations n})))
    (rejected-as! :expected-plain-map [:options]
                  #(audit/audit-derivation! src []))))

(deftest the-audit-is-not-the-typed-kernel
  (let [original (source p {:premise/p p}
                         [(prem :step/p :premise/p p)])
        semantically-opaque (assoc-in original
                                      [:premises :premise/p 1 :expression]
                                      [:not-a-typed-gesso-rule])
        semantically-opaque (-> semantically-opaque
                                (assoc-in [:steps 0 :conclusion 1 :expression]
                                          [:not-a-typed-gesso-rule])
                                (assoc-in [:goal 1 :expression]
                                          [:not-a-typed-gesso-rule]))
        report (no-authority! (audit! semantically-opaque))]
    (is (= :bounded-no-counterexample (:result report)))
    (is (= 2 (:valuations-checked report)))
    (is (some? (rejection #(kernel/check-derivation! semantically-opaque))))))

(deftest independent-semantics-may-accept-unused-premise-not-kernel-source
  (let [src (source [:true] {:premise/p p}
                    [(infer :step/true :truth-introduction [:true])])
        report (no-authority! (audit! src))]
    (is (= :bounded-no-counterexample (:result report)))
    (is (= 1 (:premise-satisfying-valuations report)))
    (is (= :unused-premises
           (:reason (rejection #(kernel/check-derivation! src)))))))

(deftest independent-semantics-may-accept-unused-step-not-kernel-source
  (let [src (source [:true] {}
                    [(infer :step/unused :truth-introduction [:true])
                     (infer :step/final :truth-introduction [:true])])
        report (no-authority! (audit! src))]
    (is (= :bounded-no-counterexample (:result report)))
    (is (= 2 (:step-implications-tested report)))
    (is (= :unused-proof-steps
           (:reason (rejection #(kernel/check-derivation! src)))))))

(deftest semantics-do-not-confuse-implication-premise-order-with-soundness
  (let [src (assoc-in (valid-modus-ponens) [:steps 2 :from]
                      [:step/antecedent :step/implication])
        report (no-authority! (audit! src))]
    (is (= :bounded-no-counterexample (:result report)))
    (is (= :invalid-implication-elimination
           (:reason (rejection #(kernel/check-derivation! src)))))))

(deftest copied-audit-reports-have-no-provenance-or-capability
  (let [src (valid-modus-ponens)
        first-report (no-authority! (audit! src))
        copied (assoc first-report :result :bounded-no-counterexample
                                   :authority :issued
                                   :certificate :issued)]
    (is (= :bounded-no-counterexample (:result first-report)))
    (is (= :issued (:authority copied)))
    (is (not (contains? first-report :source-fingerprint)))
    (is (not (contains? first-report :proof-handle)))
    (is (= first-report (audit! src)))
    (is (= :none (:authority (audit! src))))
    (is (= :not-issued (:certificate (audit! src))))))

(deftest independent-audit-cannot-be-bypassed-through-kernel-stubs
  (let [src (valid-modus-ponens)]
    (with-redefs [kernel/check-derivation!
                  (fn [_] (throw (ex-info "Must never be called by oracle"
                                          {:error/type :test/unexpected-call})))]
      (is (= :bounded-no-counterexample (:result (audit! src))))
      (is (= 4 (:valuations-checked (audit! src)))))))

(deftest inert-atomic-data-can-never-run-a-callback
  (let [called? (atom false)
        callback (fn [] (reset! called? true))
        src (assoc-in (valid-modus-ponens)
                      [:goal 1 :expression 1] callback)]
    (rejected-as! :non-data-atom [:goal 1 :expression 1]
                  #(audit! src))
    (is (false? @called?))))
