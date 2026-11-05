(ns gesso.assurance.proof.kernel-independent-soundness-test
  "v1424: bounded, independently implemented Boolean mutation gate for the
   CLOSED v1231 propositional derivation kernel.

   The oracle below is deliberately local to this TEST namespace. It does not
   call the production proposition evaluator, the v1318/v1331 reference
   interpreters, the v1335 correspondence audit, or the v1339 induction audit.
   Instead, it interprets source formulas and derives each step's inherited
   open premises directly from the untrusted source's ordered references.

   This is a finite falsification gate, NOT a formal proof of kernel
   meta-soundness or any typed-atom/XTDB/runtime correspondence. Even passing
   every bounded valuation cannot discharge a source premise, issue a
   certificate, or authorize a transaction."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.set :as set]
            [gesso.assurance.proof.kernel :as kernel]))

(def ^:private subject
  {:kind :operation :id :example/advance
   :interface {:operation :example/advance}})

(def ^:private a
  [:atom {:readers #{:result/a}
          :expression [:> [:read :result/a] [:literal 0]]}])
(def ^:private b
  [:atom {:readers #{:result/b}
          :expression [:> [:read :result/b] [:literal 0]]}])
(def ^:private c
  [:atom {:readers #{:result/c}
          :expression [:> [:read :result/c] [:literal 0]]}])
(def ^:private variable-types
  {:result/a {:type :integer}
   :result/b {:type :integer}
   :result/c {:type :integer}})

(defn- premise-step [id premise-id conclusion]
  {:id id :rule :premise :premise premise-id :conclusion conclusion})
(defn- inferred-step [id rule conclusion & attrs]
  (merge {:id id :rule rule :conclusion conclusion}
         (apply hash-map attrs)))

(defn- source [premises steps goal]
  (let [declarations (filter #(and (map? %) (contains? % :readers))
                             (tree-seq coll? seq [premises steps goal]))
        readers (reduce set/union #{} (map :readers declarations))]
    {:subject subject :variables (select-keys variable-types readers)
     :premises premises :steps steps :goal goal}))

;; Independent reference semantics: opaque atomic declarations denote distinct
;; Booleans. This is intentionally not an alias of any production evaluator.
(defn- leaves [formula]
  (case (first formula)
    :atom #{(second formula)}
    (:true :false) #{}
    (reduce set/union #{} (map leaves (rest formula)))))

(defn- meaning [formula valuation]
  (case (first formula)
    :true true
    :false false
    :atom (get valuation (second formula))
    :not (not (meaning (second formula) valuation))
    :and (every? #(meaning % valuation) (rest formula))
    :or (boolean (some #(meaning % valuation) (rest formula)))
    :implies (or (not (meaning (second formula) valuation))
                 (boolean (meaning (nth formula 2) valuation)))
    (throw (ex-info "Fixture used an unknown connective"
                    {:formula formula}))))

(defn- valuations [formulas]
  (let [atoms (vec (sort-by pr-str (reduce set/union #{} (map leaves formulas))))]
    (when (> (count atoms) 5)
      (throw (ex-info "Bounded oracle exceeded its own atom cap"
                      {:atom-count (count atoms)})))
    (mapv (fn [mask]
            (zipmap atoms (mapv #(boolean (bit-test mask %))
                               (range (count atoms)))))
          (range (bit-shift-left 1 (count atoms))))))

(defn- inherited-premises [step earlier]
  (case (:rule step)
    :premise #{(:premise step)}
    (:truth-introduction :implication-reflexivity) #{}
    (reduce set/union #{} (map #(get earlier % #{}) (:from step)))))

(defn- independently-audit
  "A fresh direct truth-table evaluation of EACH source step, conditional on
   exactly its inherited ORIGINAL named premises, as well as the final goal.
   Input is a small, known-well-formed TEST fixture; no caller reports are
   accepted. A counterexample is a valuation, not a proof certificate."
  [src]
  (let [all-formulas (concat (vals (:premises src))
                             (map :conclusion (:steps src))
                             [(:goal src)])
        assignments (valuations all-formulas)
        steps (reduce
               (fn [{:keys [earlier records]} [index step]]
                 (let [inherited (inherited-premises step earlier)
                       assumptions (mapv (:premises src) inherited)
                       counterexamples
                       (filterv (fn [valuation]
                                  (and (every? #(meaning % valuation) assumptions)
                                       (not (meaning (:conclusion step) valuation))))
                                assignments)
                       satisfying-count
                       (count (filter #(every? (fn [premise]
                                                 (meaning premise %)) assumptions)
                                      assignments))
                       record {:index index :id (:id step)
                               :inherited inherited
                               :counterexamples counterexamples
                               :satisfying-count satisfying-count}]
                   {:earlier (assoc earlier (:id step) inherited)
                    :records (conj records record)}))
               {:earlier {} :records []}
               (map-indexed vector (:steps src)))
        goal-counterexamples
        (filterv (fn [valuation]
                   (and (every? #(meaning % valuation)
                                (vals (:premises src)))
                        (not (meaning (:goal src) valuation))))
                 assignments)]
    {:valuation-count (count assignments)
     :premise-satisfying-valuations
     (count (filter (fn [valuation]
                      (every? #(meaning % valuation) (vals (:premises src))))
                    assignments))
     :steps (:records steps)
     :goal-counterexamples goal-counterexamples}))

(defn- accepted! [src]
  (kernel/check-derivation! src))
(defn- rejection [src]
  (try (accepted! src) nil
       (catch clojure.lang.ExceptionInfo error (ex-data error))))

(defn- no-authority! [report]
  (is (= kernel/report-type (:type report)))
  (is (= :none (:authority report)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-performed (:assumption-discharge report)))
  (is (not (contains? report :transaction-authority)))
  (is (not (contains? report :valid?))))

(defn- assert-sound-for-bounded-fixture! [src]
  (let [checked (accepted! src)
        independent (independently-audit src)
        all-premise-ids (set (keys (:premises src)))]
    (no-authority! checked)
    (is (= all-premise-ids (:open-premises checked)))
    (is (empty? (:goal-counterexamples independent)))
    (doseq [{:keys [index id counterexamples]} (:steps independent)]
      (is (empty? counterexamples)
          (str "Unsound accepted step " id " at index " index ": "
               (pr-str (first counterexamples)))))
    independent))

(defn- all-seven-rule-proof [p q r]
  (let [both [:and p q]
        disj [:or r [:false]]
        reflex [:implies p p]
        combined [:and disj [:true] reflex]]
    (source {:premise/p p
             :premise/q q
             :premise/conditional [:implies both r]}
            [(premise-step :step/p :premise/p p)
             (premise-step :step/q :premise/q q)
             (inferred-step :step/both :conjunction-introduction both
                            :from [:step/p :step/q])
             (premise-step :step/conditional :premise/conditional
                           [:implies both r])
             (inferred-step :step/r :implication-elimination r
                            :from [:step/conditional :step/both])
             (inferred-step :step/disj :disjunction-introduction disj
                            :from [:step/r] :index 0)
             (inferred-step :step/truth :truth-introduction [:true])
             (inferred-step :step/reflex :implication-reflexivity reflex)
             (inferred-step :step/combined :conjunction-introduction combined
                            :from [:step/disj :step/truth :step/reflex])
             (inferred-step :step/result :conjunction-elimination disj
                            :from [:step/combined] :index 0)]
            disj)))

(deftest independent-valuation-gate-covers-all-seven-production-rules
  (let [checked (for [p [a b c]
                      q [a b c]
                      r [a b c]]
                  (let [src (all-seven-rule-proof p q r)
                        verdict (assert-sound-for-bounded-fixture! src)]
                    (is (= (set (keys (:premises src)))
                           (:inherited (last (:steps verdict)))))
                    verdict))]
    (is (= 27 (count checked)))
    (is (= #{2 4 8} (set (map :valuation-count checked))))
    (is (every? #(pos? (:premise-satisfying-valuations %)) checked))
    (is (every? #(= 10 (count (:steps %))) checked))))

(deftest nested-negation-and-implication-have-independent-boolean-meaning
  (doseq [triple [[[:not a] [:not b] [:implies a c]]
                  [[:and a b] [:or b c] [:not [:false]]]
                  [[:or a b] [:not [:implies a b]] [:or a c]]
                  [[:or a b] [:not c] [:and a [:true]]]]]
    (let [verdict (assert-sound-for-bounded-fixture!
                   (apply all-seven-rule-proof triple))]
      (is (empty? (:goal-counterexamples verdict)))
      (is (pos? (:premise-satisfying-valuations verdict))))))

(deftest contradiction-premise-is-open-even-with-no-satisfying-valuation
  (let [src (source {:premise/false [:false]}
                    [(premise-step :step/false :premise/false [:false])]
                    [:false])
        independently (assert-sound-for-bounded-fixture! src)]
    (is (= 1 (:valuation-count independently)))
    (is (zero? (:premise-satisfying-valuations independently)))
    (is (= 0 (get-in independently [:steps 0 :satisfying-count])))
    (is (= #{:premise/false}
           (:inherited (first (:steps independently)))))))

(deftest independent-oracle-rejects-each-semantically-unsound-rule-mutation
  (let [mutants
        [{:label :fake-truth
          :src (source {} [(inferred-step :step/fake :truth-introduction [:false])]
                       [:false])
          :bad 0}
         {:label :fake-reflexivity
          :src (source {} [(inferred-step :step/fake :implication-reflexivity
                                          [:implies a b])]
                       [:implies a b])
          :bad 0}
         {:label :fake-premise
          :src (source {:premise/a a}
                       [(premise-step :step/fake :premise/a b)] b)
          :bad 0}
         {:label :fake-conjunction-introduction
          :src (source {:premise/a a :premise/b b}
                       [(premise-step :step/a :premise/a a)
                        (premise-step :step/b :premise/b b)
                        (inferred-step :step/fake :conjunction-introduction
                                       [:and a c] :from [:step/a :step/b])]
                       [:and a c])
          :bad 2}
         {:label :fake-conjunction-elimination
          :src (source {:premise/both [:and a b]}
                       [(premise-step :step/both :premise/both [:and a b])
                        (inferred-step :step/fake :conjunction-elimination c
                                       :from [:step/both] :index 0)] c)
          :bad 1}
         {:label :fake-implication-elimination
          :src (source {:premise/conditional [:implies a b] :premise/a a}
                       [(premise-step :step/conditional :premise/conditional
                                      [:implies a b])
                        (premise-step :step/a :premise/a a)
                        (inferred-step :step/fake :implication-elimination c
                                       :from [:step/conditional :step/a])] c)
          :bad 2}
         {:label :fake-disjunction-introduction
          :src (source {:premise/a a}
                       [(premise-step :step/a :premise/a a)
                        (inferred-step :step/fake :disjunction-introduction
                                       [:or b c] :from [:step/a] :index 0)]
                       [:or b c])
          :bad 1}]]
    (doseq [{:keys [label src bad]} mutants]
      (testing (name label)
        (let [reference (independently-audit src)
              error (rejection src)]
          (is (seq (get-in reference [:steps bad :counterexamples]))
              (str "Mutation was not semantically falsifiable: " label))
          (is (seq (:goal-counterexamples reference))
              (str "Unsound final goal escaped independent falsification: " label))
          (is (= :gesso.assurance.proof.kernel/invalid-derivation
                 (:error/type error))
              (str "Kernel accepted an unsound rule mutation: " label)))))))

(deftest wrong-final-conclusion-cannot-be-hidden-by-changing-the-goal
  (doseq [p [a b c]
          q [a b c]
          r [a b c]]
    (let [good (all-seven-rule-proof p q r)
          bad (-> good
                  (assoc-in [:steps 9 :conclusion] [:false])
                  (assoc :goal [:false]))
          independently (independently-audit bad)]
      (is (seq (:goal-counterexamples independently)))
      (is (seq (get-in independently [:steps 9 :counterexamples])))
      (is (= :gesso.assurance.proof.kernel/invalid-derivation
             (:error/type (rejection bad)))))))

(deftest valid-sources-and-negative-sources-keep-reproducible-results
  (let [good (all-seven-rule-proof a b c)
        bad (assoc-in good [:steps 9 :conclusion] [:false])
        original-good (independently-audit good)
        original-bad (independently-audit bad)]
    (is (= original-good (independently-audit good)))
    (is (= original-bad (independently-audit bad)))
    (is (empty? (:goal-counterexamples original-good)))
    (is (seq (get-in original-bad [:steps 9 :counterexamples])))
    (is (not= original-good original-bad))
    (is (nil? (rejection good)))
    (is (= :gesso.assurance.proof.kernel/invalid-derivation
           (:error/type (rejection bad))))))
