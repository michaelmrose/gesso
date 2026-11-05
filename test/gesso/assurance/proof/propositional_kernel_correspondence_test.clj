(ns gesso.assurance.proof.propositional-kernel-correspondence-test
  "v1336: permanent adversarial regressions for source-relative correspondence.

   These tests compare the production kernel's admitted source derivations to
   independently reconstructed v1332/v1333 rule instances. They do not prove
   universal kernel soundness, source-normalization correctness, or certificate
   authority. Premises remain OPEN and supplied reports remain copyable data."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.kernel :as kernel]
            [gesso.assurance.proof.propositional-kernel-correspondence :as corr]
            [gesso.assurance.proof.propositional-rule-obligations :as obligations]
            [gesso.assurance.proof.propositional-rule-schema-audit :as audit]))

(def ^:private subject
  {:kind :operation :id :example/advance
   :interface {:operation :example/advance
               :guards [[:in :request/status #{:new :active}]]
               :effects [[:set :result/score [:input :score]]]}})
(def ^:private p
  [:atom {:readers #{:result/score}
          :expression [:> [:read :result/score] [:literal 7]]}])
(def ^:private q
  [:atom {:readers #{:request/priority}
          :expression [:< [:read :request/priority] [:literal 20]]}])
(def ^:private r
  [:atom {:readers #{:request/priority}
          :expression [:> [:read :request/priority] [:literal 3]]}])
(def ^:private variables
  {:result/score {:type :integer}
   :request/priority {:type :integer}})

(defn- source [vars premises steps goal]
  {:subject subject :variables vars :premises premises :steps steps :goal goal})
(defn- step [id rule conclusion & attributes]
  (merge {:id id :rule rule :conclusion conclusion}
         (apply hash-map attributes)))
(defn- premise [id selected formula]
  (step id :premise formula :premise selected))

(defn- implication-proof []
  (source variables
          {:premise/imp [:implies p q] :premise/p p}
          [(premise :step/imp :premise/imp [:implies p q])
           (premise :step/p :premise/p p)
           (step :step/q :implication-elimination q
                 :from [:step/imp :step/p])]
          q))

(defn- conjunction-proof []
  (source variables {:premise/p p :premise/q q}
          [(premise :step/p :premise/p p)
           (premise :step/q :premise/q q)
           (step :step/pq :conjunction-introduction [:and p q]
                 :from [:step/p :step/q])
           (step :step/select :conjunction-elimination p
                 :from [:step/pq] :index 0)]
          p))

(defn- all-rules-proof []
  (let [joined [:and p q]
        offer [:or r [:false]]
        reflexive [:implies p p]
        goal [:and offer [:true] reflexive]]
    (source variables
            {:premise/p p :premise/q q
             :premise/imp [:implies joined r]}
            [(premise :step/p :premise/p p)
             (premise :step/q :premise/q q)
             (step :step/join :conjunction-introduction joined
                   :from [:step/p :step/q])
             (premise :step/imp :premise/imp [:implies joined r])
             (step :step/r :implication-elimination r
                   :from [:step/imp :step/join])
             (step :step/offer :disjunction-introduction offer
                   :from [:step/r] :index 0)
             (step :step/truth :truth-introduction [:true])
             (step :step/reflexive :implication-reflexivity reflexive)
             (step :step/aggregate :conjunction-introduction goal
                   :from [:step/offer :step/truth :step/reflexive])
             (step :step/result :conjunction-elimination offer
                   :from [:step/aggregate] :index 0)]
            offer)))

(defn- reason [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
(defn- rejects! [src]
  (let [r (reason #(corr/audit-derivation! src))]
    (is (keyword? r) (str "Expected source rejection: " (pr-str src)))
    r))
(defn- report! [src]
  (corr/audit-derivation! src))

(deftest exact-seven-rule-source-correspondence
  (let [src (all-rules-proof)
        result (report! src)
        steps (:step-reports result)]
    (is (= 10 (:source-step-count result)))
    (is (= 10 (:checked-step-count result)))
    (is (= 7 (count (set (map :rule steps)))))
    (is (= :all-step-shapes-match (:source-rule-correspondence result)))
    (is (= :exact-match (:open-premise-correspondence result)))
    (is (= #{:premise/p :premise/q :premise/imp}
           (:kernel-open-premises result)))
    (is (= (mapv :id (:steps src)) (mapv :id steps)))
    (is (= (mapv :conclusion (:steps src)) (mapv :conclusion steps)))
    (is (= (vec (range 10)) (mapv :source-index steps)))
    (is (= :complete-for-supplied-source-instances (:status result)))
    (is (= {:complete-for-instance 10} (:status-counts result)))))

(deftest every-step-has-precise-inherited-premise-provenance
  (let [actual (into {} (map (juxt :id :premises)
                             (:step-reports (report! (all-rules-proof)))))
        expected {:step/p #{:premise/p}
                  :step/q #{:premise/q}
                  :step/join #{:premise/p :premise/q}
                  :step/imp #{:premise/imp}
                  :step/r #{:premise/imp :premise/p :premise/q}
                  :step/offer #{:premise/imp :premise/p :premise/q}
                  :step/truth #{}
                  :step/reflexive #{}
                  :step/aggregate #{:premise/imp :premise/p :premise/q}
                  :step/result #{:premise/imp :premise/p :premise/q}}]
    (is (= expected actual))
    (is (= (get actual :step/result)
           (:kernel-open-premises (report! (all-rules-proof)))))))

(deftest premise-dependencies-remain-open-under-modus-ponens
  (let [result (report! (implication-proof))]
    (is (= :complete-for-supplied-source-instances (:status result)))
    (is (= #{:premise/imp :premise/p} (:kernel-open-premises result)))
    (is (= [#{:premise/imp} #{:premise/p} #{:premise/imp :premise/p}]
           (mapv :premises (:step-reports result))))
    (is (= :not-performed (:assumption-discharge result)))
    (is (= :none (:authority result)))))

(deftest conjunction-preserves-ordered-sources-and-dependency-union
  (let [result (report! (conjunction-proof))
        steps (:step-reports result)]
    (is (= [:premise :premise :conjunction-introduction
            :conjunction-elimination]
           (mapv :rule steps)))
    (is (= #{:premise/p :premise/q} (:premises (nth steps 2))))
    (is (= #{:premise/p :premise/q} (:premises (nth steps 3))))
    (is (= :complete-for-supplied-source-instances (:status result)))))

(deftest premise-free-theorems-carry-no-implicit-context
  (let [examples [(source {} {} [(step :step/true :truth-introduction [:true])]
                          [:true])
                  (source {:result/score (:result/score variables)} {}
                          [(step :step/id :implication-reflexivity [:implies p p])]
                          [:implies p p])]]
    (doseq [src examples]
      (let [result (report! src)]
        (is (= #{} (:kernel-open-premises result)))
        (is (= #{} (:premises (first (:step-reports result)))))
        (is (= :complete-for-supplied-source-instances (:status result)))
        (is (= :none (:authority result)))))))

(deftest contradictory-premise-is-not-discharged-as-a-theorem
  (let [src (source {} {:premise/false [:false]}
                    [(premise :step/false :premise/false [:false])]
                    [:false])
        result (report! src)]
    (is (= :vacuous-instances (:status result)))
    (is (= #{:premise/false} (:kernel-open-premises result)))
    (is (= :vacuous-hypotheses
           (:local-semantic-status (first (:step-reports result)))))
    (is (= :not-performed (:assumption-discharge result)))
    (is (= :not-issued (:certificate result)))))

(deftest duplicate-premise-usage-does-not-double-count-assumptions
  (let [src (source {:result/score (:result/score variables)}
                    {:premise/p p}
                    [(premise :step/one :premise/p p)
                     (premise :step/two :premise/p p)
                     (step :step/join :conjunction-introduction [:and p p]
                           :from [:step/one :step/two])]
                    [:and p p])
        result (report! src)]
    (is (= #{:premise/p} (:kernel-open-premises result)))
    (is (= #{:premise/p} (:premises (last (:step-reports result)))))
    (is (= :complete-for-supplied-source-instances (:status result)))))

(deftest selected-premise-name-is-part-of-provenance
  (let [src (source {:result/score (:result/score variables)}
                    {:premise/a p :premise/b p}
                    [(premise :step/a :premise/a p)
                     (premise :step/b :premise/b p)
                     (step :step/join :conjunction-introduction [:and p p]
                           :from [:step/a :step/b])]
                    [:and p p])
        result (report! src)]
    (is (= #{:premise/a :premise/b} (:kernel-open-premises result)))
    (is (= #{:premise/a :premise/b} (:premises (last (:step-reports result)))))))

(deftest missing-premise-cannot-be-made-implicit
  (is (= :unknown-premise
         (rejects! (update (implication-proof) :premises dissoc :premise/p)))))

(deftest wrong-premise-reference-fails-closed
  (is (= :unknown-premise
         (rejects! (assoc-in (implication-proof) [:steps 1 :premise]
                             :premise/absent)))))

(deftest swapped-premise-formula-fails-closed
  (is (= :wrong-premise-conclusion
         (rejects! (assoc-in (implication-proof) [:steps 1 :conclusion] q)))))

(deftest unused-declared-premise-fails-closed
  (is (= :unused-premises
         (rejects! (assoc-in (implication-proof)
                             [:premises :premise/unused] [:true])))))

(deftest unused-proof-step-fails-closed
  (let [src (implication-proof)
        stray (step :step/stray :truth-introduction [:true])]
    (is (= :unused-proof-steps
           (rejects! (update src :steps #(vec (cons stray %))))))))

(deftest invalid-forward-reference-fails-closed
  (let [src (implication-proof)]
    (is (= :unknown-or-forward-step
           (rejects! (assoc-in src [:steps 2 :from]
                               [:step/imp :step/future]))))))

(deftest self-reference-fails-closed
  (let [src (implication-proof)]
    (is (= :unknown-or-forward-step
           (rejects! (assoc-in src [:steps 2 :from]
                               [:step/imp :step/q]))))))

(deftest duplicate-step-identity-fails-closed
  (let [src (implication-proof)]
    (is (= :duplicate-step-id
           (rejects! (assoc-in src [:steps 2 :id] :step/p))))))

(deftest wrong-implication-source-order-fails-closed
  (let [src (implication-proof)]
    (is (= :invalid-implication-elimination
           (rejects! (assoc-in src [:steps 2 :from]
                               [:step/p :step/imp]))))))

(deftest fabricated-implication-result-fails-closed
  (is (= :invalid-implication-elimination
         (rejects! (assoc-in (implication-proof) [:steps 2 :conclusion] p)))))

(deftest reversed-conjunction-source-order-fails-closed
  (is (= :invalid-conjunction-introduction
         (rejects! (assoc-in (conjunction-proof) [:steps 2 :from]
                             [:step/q :step/p])))))

(deftest invalid-conjunction-index-fails-closed
  (is (= :invalid-index
         (rejects! (assoc-in (conjunction-proof) [:steps 3 :index] 2)))))

(deftest unsupported-rule-fails-closed
  (is (= :unknown-rule
         (rejects! (assoc-in (implication-proof) [:steps 2 :rule]
                             :modus-ponens-anything)))))

(deftest forged-root-and-step-authority-is-not-source
  (let [src (implication-proof)]
    (doseq [key [:certificate :valid? :authority :admission :checked-report]]
      (is (= :wrong-keys (rejects! (assoc src key true))))
      (is (= :wrong-keys
             (rejects! (assoc-in src [:steps 2 key] true)))))))

(deftest source-metadata-cannot-add-proof-status
  (let [src (implication-proof)]
    (is (= :metadata-not-allowed
           (rejects! (with-meta src {:proved true}))))
    (is (= :metadata-not-allowed
           (rejects! (assoc src :steps
                            (with-meta (:steps src) {:proved true})))))))

(deftest forged-kernel-acceptance-cannot-validate-wrong-step-conclusion
  (let [original (implication-proof)
        accepted (kernel/check-derivation! original)
        forged (assoc-in original [:steps 2 :conclusion] p)]
    (with-redefs [kernel/check-derivation! (fn [_] accepted)]
      (is (= :source-rule-conclusion-mismatch
             (reason #(corr/audit-derivation! forged)))))))

(deftest forged-kernel-premise-report-is-detected
  (let [src (implication-proof)
        accepted (kernel/check-derivation! src)]
    (with-redefs [kernel/check-derivation!
                  (fn [_] (assoc accepted :open-premises #{:premise/fabricated}))]
      (is (= :kernel-premise-correspondence-mismatch
             (reason #(corr/audit-derivation! src)))))))

(deftest forged-independent-rule-conclusion-is-detected
  (let [src (implication-proof)
        original obligations/instantiate!]
    (with-redefs [obligations/instantiate!
                  (fn [x] (assoc (original x) :conclusion [:false]))]
      (is (= :source-rule-conclusion-mismatch
             (reason #(corr/audit-derivation! src)))))))

(deftest forged-independent-audit-report-is-detected
  (let [src (implication-proof)
        original audit/audit-instance!]
    (with-redefs [audit/audit-instance!
                  (fn [x] (assoc (original x) :conclusion [:false]))]
      (is (= :oracle-source-mismatch
             (reason #(corr/audit-derivation! src)))))))

(deftest source-fingerprint-changes-with-premise-source
  (let [a (implication-proof)
        b (-> a
              (assoc-in [:premises :premise/imp] [:implies p r])
              (assoc-in [:steps 0 :conclusion] [:implies p r])
              (assoc-in [:steps 2 :conclusion] r)
              (assoc :goal r))]
    (is (not= (:kernel-source-fingerprint (report! a))
              (:kernel-source-fingerprint (report! b))))
    (is (= #{:premise/imp :premise/p} (:kernel-open-premises (report! b))))))

(deftest supplied-checker-reports-cannot-replace-derivation-source
  (let [src (implication-proof)
        report (report! src)]
    (is (= :wrong-keys
           (rejects! (assoc src :correspondence-report report))))
    (is (= :wrong-keys
           (rejects! (assoc src :kernel-report (kernel/check-derivation! src)))))
    (is (= :wrong-keys
           (rejects! (assoc src :proof-status :verified))))))

(deftest source-relative-audits-are-not-certificates
  (doseq [src [(implication-proof) (conjunction-proof) (all-rules-proof)]]
    (let [r (report! src)]
      (is (= :gesso.assurance.proof.propositional-kernel-correspondence/source-audit
             (:type r)))
      (is (= :not-issued (:certificate r)))
      (is (= :not-admitted (:admission r)))
      (is (= :none (:authority r)))
      (is (= :not-established (:universal-rule-soundness r)))
      (is (= :not-established (:universal-implementation-correspondence r)))
      (is (= :not-established (:source-normalization-soundness r)))
      (is (= :not-interpreted (:typed-atomic-semantics r)))
      (is (= :not-established (:physical-runtime-correspondence r))))))

(deftest source-audit-is-deterministic
  (let [src (all-rules-proof)]
    (is (= (report! src) (report! src)))))

(deftest source-step-budget-never-becomes-a-pass
  ;; Every truth step feeds the final conjunction, so kernel graph reachability
  ;; accepts this source. The correspondence checker intentionally stops at 48.
  (let [truth-steps (mapv (fn [i]
                            (step (keyword "step" (str "truth-" i))
                                  :truth-introduction [:true]))
                          (range 49))
        ids (mapv :id truth-steps)
        goal (into [:and] (repeat 49 [:true]))
        src (source {} {} (conj truth-steps
                                 (step :step/aggregate :conjunction-introduction
                                       goal :from ids)) goal)
        r (report! src)]
    (is (= 50 (:source-step-count r)))
    (is (= :not-checked (:status r)))
    (is (= :step-budget (:reason r)))
    (is (zero? (:checked-step-count r)))
    (is (= [] (:step-reports r)))
    (is (= :not-checked (:source-rule-correspondence r)))
    (is (= :not-issued (:certificate r)))))
