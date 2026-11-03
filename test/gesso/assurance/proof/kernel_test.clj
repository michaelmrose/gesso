(ns gesso.assurance.proof.kernel-test
  "v1232: permanent adversarial regressions for the deliberately small v1231
   propositional derivation checker. All inputs are original source data.
   Checked derivations are descriptive, copyable, and never certificates.
   In particular, premises remain OPEN and cannot turn false or opaque
   assumptions into unconditional theorems."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.walk]
            [gesso.assurance.proof.kernel :as kernel]))

(def ^:private subj
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
(def ^:private vars
  {:result/score {:type :integer}
   :request/priority {:type :integer}})

(defn- src [variables goal premises steps]
  {:subject subj :variables variables :premises premises :steps steps :goal goal})
(defn- s [id rule conclusion & kvs]
  (merge {:id id :rule rule :conclusion conclusion} (apply hash-map kvs)))
(defn- premise [id premise-id conclusion]
  (s id :premise conclusion :premise premise-id))
(defn- imp-proof []
  (src vars q
       {:premise/imp [:implies p q] :premise/p p}
       [(premise :step/imp :premise/imp [:implies p q])
        (premise :step/p :premise/p p)
        (s :step/q :implication-elimination q
           :from [:step/imp :step/p])]))
(defn- and-proof []
  (src vars p
       {:premise/p p :premise/q q}
       [(premise :step/p :premise/p p)
        (premise :step/q :premise/q q)
        (s :step/pq :conjunction-introduction [:and p q]
           :from [:step/p :step/q])
        (s :step/select :conjunction-elimination p
           :from [:step/pq] :index 0)]))
(defn- true-proof []
  (src {} [:true] {} [(s :step/true :truth-introduction [:true])]))
(defn- error-data [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))
(defn- rejects! [declaration]
  (let [data (error-data #(kernel/check-derivation! declaration))]
    (is (map? data) (str "Expected rejection: " (pr-str declaration)))
    (is (qualified-keyword? (:error/type data)) (pr-str data))
    (is (some? (:at data)) (pr-str data))
    data))
(defn- kernel-rejects! [declaration reason]
  (let [data (rejects! declaration)]
    (is (= :gesso.assurance.proof.kernel/invalid-derivation
           (:error/type data)) (pr-str data))
    (is (= reason (:reason data)) (pr-str data))
    data))
(defn- evidence-is-not-authority! [report]
  (is (= :gesso.assurance.proof.kernel/unadmitted-checked-derivation
         (:type report)))
  (is (= 1 (:version report)))
  (is (= :propositional-derivation-checked (:checking report)))
  (is (= :not-performed (:assumption-discharge report)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :not-established (:runtime-enforcement report)))
  (is (= :none (:authority report)))
  (is (not (contains? report :valid?)))
  (is (not (contains? report :transaction-authority)))
  (is (re-matches #"sha256:[0-9a-f]{64}" (:fingerprint report)))
  report)

(deftest conditional-modus-ponens-preserves-all-open-premises
  (let [source (imp-proof)
        report (evidence-is-not-authority! (kernel/check-derivation! source))]
    (is (= #{:premise/imp :premise/p} (:open-premises report)))
    (is (= :conditional-on-open-premises (:conclusion-status report)))
    (is (= 3 (:rule-count report)))
    (is (= report (kernel/check-derivation! source)))
    (is (= (:fingerprint report)
           (:fingerprint (kernel/check-derivation! source))))
    (is (not= (:fingerprint report) (:subject-fingerprint report)))
    (is (not= (:fingerprint report) (:goal-fingerprint report)))))

(deftest unconditional-truth-and-reflexivity-are-narrow-theorems
  (doseq [source [(true-proof)
                  (src {:result/score (:result/score vars)}
                       [:implies p p] {}
                       [(s :step/reflex :implication-reflexivity [:implies p p])])]]
    (let [report (evidence-is-not-authority! (kernel/check-derivation! source))]
      (is (= #{} (:open-premises report)))
      (is (= :checked-without-premises (:conclusion-status report)))))
  (kernel-rejects!
   (src {} [:false] {}
        [(s :step/no :truth-introduction [:false])])
   :invalid-truth-introduction)
  (kernel-rejects!
   (src vars [:implies p q] {}
        [(s :step/no :implication-reflexivity [:implies p q])])
   :invalid-implication-reflexivity))

(deftest conjunction-introduction-and-elimination-check-exact-order
  (let [source (and-proof)
        report (evidence-is-not-authority! (kernel/check-derivation! source))]
    (is (= #{:premise/p :premise/q} (:open-premises report)))
    (is (= 4 (:rule-count report)))
    (kernel-rejects! (assoc-in source [:steps 2 :conclusion] [:and q p])
                     :invalid-conjunction-introduction)
    (kernel-rejects! (assoc-in source [:steps 2 :from] [:step/p :step/p])
                     :invalid-conjunction-introduction)
    (kernel-rejects! (assoc-in source [:steps 3 :index] 1)
                     :invalid-conjunction-elimination)
    (kernel-rejects! (assoc-in source [:steps 3 :index] -1)
                     :invalid-index)
    (kernel-rejects! (assoc-in source [:steps 3 :index] 2)
                     :invalid-index)
    (kernel-rejects! (assoc-in source [:steps 3 :index] 0.0)
                     :invalid-index)))

(deftest conjunction-elimination-requires-real-conjunction
  (let [source (src {:result/score (:result/score vars)} p
                    {:premise/p p}
                    [(premise :step/p :premise/p p)
                     (s :step/select :conjunction-elimination p
                        :from [:step/p] :index 0)])]
    (kernel-rejects! source :expected-conjunction)))

(deftest implication-elimination-requires-ordered-exact-premises
  (let [source (imp-proof)]
    (kernel-rejects! (assoc-in source [:steps 2 :from] [:step/p :step/imp])
                     :invalid-implication-elimination)
    (kernel-rejects! (assoc-in source [:steps 2 :conclusion] p)
                     :invalid-implication-elimination)
    (kernel-rejects! (assoc-in source [:steps 2 :from] [:step/p :step/p])
                     :invalid-implication-elimination)
    (kernel-rejects! (assoc-in source [:steps 2 :from] [:step/imp])
                     :invalid-sources)
    (kernel-rejects! (assoc-in source [:steps 2 :from]
                                [:step/imp :step/p :step/imp])
                     :invalid-sources)))

(deftest disjunction-introduction-proves-only-source-at-stated-index
  (let [source (src {:result/score (:result/score vars)}
                    [:or [:false] p [:true]]
                    {:premise/p p}
                    [(premise :step/p :premise/p p)
                     (s :step/or :disjunction-introduction
                        [:or [:false] p [:true]]
                        :from [:step/p] :index 1)])]
    (is (= #{:premise/p}
           (:open-premises (kernel/check-derivation! source))))
    (kernel-rejects! (assoc-in source [:steps 1 :index] 0)
                     :invalid-disjunction-introduction)
    (kernel-rejects! (assoc-in source [:steps 1 :index] 3)
                     :invalid-index)
    (kernel-rejects! (assoc-in source [:steps 1 :conclusion]
                              [:and [:false] p [:true]])
                     :expected-disjunction)))

(deftest premise-is-not-a-fact-even-when-it-is-false
  (let [source (src {} [:false] {:premise/false [:false]}
                    [(premise :step/false :premise/false [:false])])
        report (evidence-is-not-authority! (kernel/check-derivation! source))]
    (is (= #{:premise/false} (:open-premises report)))
    (is (= :conditional-on-open-premises (:conclusion-status report)))
    (kernel-rejects!
     (assoc source :premises {}) :unknown-premise)
    (kernel-rejects!
     (assoc source :steps [(s :step/fraud :truth-introduction [:false])])
     :invalid-truth-introduction)))

(deftest missing-and-swapped-premises-fail-closed
  (let [source (imp-proof)]
    (kernel-rejects! (update source :premises dissoc :premise/p)
                     :unknown-premise)
    (kernel-rejects! (assoc-in source [:steps 0 :premise] :premise/absent)
                     :unknown-premise)
    (kernel-rejects! (assoc-in source [:steps 0 :conclusion] p)
                     :wrong-premise-conclusion)
    (kernel-rejects! (assoc-in source [:steps 1 :conclusion] q)
                     :wrong-premise-conclusion)
    (kernel-rejects! (assoc source :premises {:premise/imp [:implies p q]
                                            :premise/p p
                                            :premise/unused [:true]})
                     :unused-premises)))

(deftest forward-unknown-and-self-references-are-never-proofs
  (let [source (imp-proof)]
    (kernel-rejects! (assoc-in source [:steps 2 :from] [:step/imp :step/absent])
                     :unknown-or-forward-step)
    (kernel-rejects! (assoc-in source [:steps 2 :from] [:step/imp :step/q])
                     :unknown-or-forward-step)
    (kernel-rejects! (assoc-in source [:steps 2 :from] [:step/imp :step/after])
                     :unknown-or-forward-step)
    (kernel-rejects!
     (assoc source :steps
            [(s :step/try :implication-elimination q
                :from [:step/imp :step/p])
             (nth (:steps source) 0)
             (nth (:steps source) 1)])
     :unknown-or-forward-step)
    (kernel-rejects! (assoc-in source [:steps 2 :id] :step/p)
                     :duplicate-step-id)))

(deftest unused-steps-cannot-be-used-to-launder-false-or-hidden-evidence
  (let [source (imp-proof)]
    (kernel-rejects!
     (assoc source :steps (conj (:steps source)
                                (s :step/extra :truth-introduction [:true])))
     :wrong-final-conclusion)
    (kernel-rejects!
     (assoc source :steps
            (into [(s :step/extra :truth-introduction [:true])]
                  (:steps source)))
     :unused-proof-steps)
    (kernel-rejects! (assoc source :goal p)
                     :wrong-final-conclusion)))

(deftest source-and-step-closed-world-keys
  (let [source (imp-proof)]
    (doseq [key [:valid? :proved? :certificate :admission :proof-report
                 :checked? :runtime-enforcement :kernel-handle]]
      (testing (name key)
        (kernel-rejects! (assoc source key true) :wrong-keys)
        (kernel-rejects! (assoc-in source [:steps 2 key] true) :wrong-keys)))
    (kernel-rejects! (dissoc source :subject) :wrong-keys)
    (kernel-rejects! (dissoc source :goal) :wrong-keys)
    (kernel-rejects! (assoc-in source [:steps 2 :rule] :derive-anything)
                     :unknown-rule)
    (kernel-rejects! (assoc-in source [:steps 2 :id] :unqualified)
                     :invalid-id)
    (kernel-rejects! (assoc-in source [:steps 0 :premise] :unqualified)
                     :invalid-id)))

(deftest evidence-report-cannot-be-replayed-as-source
  (let [source (imp-proof)
        report (evidence-is-not-authority! (kernel/check-derivation! source))]
    (kernel-rejects! report :wrong-keys)
    (kernel-rejects! (merge source report) :wrong-keys)
    (kernel-rejects! (assoc source :fingerprint (:fingerprint report))
                     :wrong-keys)
    (kernel-rejects! (assoc source :certificate report) :wrong-keys)
    (rejects! (assoc-in source [:steps 2 :conclusion]
                              {:validated true :formula q}))))

(deftest subject-binding-and-fingerprints-do-not-confuse-identity-with-proof
  (let [source (imp-proof)
        result (kernel/check-derivation! source)
        other (assoc-in source [:subject :id] :example/different)
        other-result (kernel/check-derivation! other)]
    (is (not= (:subject-fingerprint result) (:subject-fingerprint other-result)))
    (is (not= (:fingerprint result) (:fingerprint other-result)))
    (is (= (:open-premises result) (:open-premises other-result)))
    (is (= :not-admitted (:admission other-result)))
    (is (not= (:fingerprint result)
              (:fingerprint (kernel/check-derivation!
                             (assoc-in source [:subject :interface :effects]
                                       [[:set :result/score [:input :other]]])))))
    (rejects! (assoc-in source [:subject :fingerprint] (:subject-fingerprint result)))
    (rejects! (assoc-in source [:subject :certificate] :alleged))))

(deftest variable-context-typing-is-checked-from-source
  (let [source (imp-proof)]
    (rejects! (assoc-in source [:variables :result/score] {:type :string}))
    (rejects! (assoc-in source [:variables :request/priority]
                        {:type :enum :name :example/priority :values #{:low :high}}))
    (kernel-rejects! (update source :variables dissoc :request/priority)
                     :undeclared-variable)
    (kernel-rejects! (assoc-in source [:variables :unused/field] {:type :integer})
                     :unused-variables)
    (rejects! (assoc-in source [:steps 2 :conclusion]
                        [:atom {:readers #{:request/priority}
                                :expression [:> [:read :request/priority]
                                               [:literal :wrong]]}]))
    (rejects! (assoc-in source [:premises :premise/p 1 :expression]
                        [:> [:read :result/score] [:literal "bad"]]))))

(deftest optional-and-nullable-reads-stay-locally-guarded
  (let [optional-var {:type :integer :optional? true}
        presence [:atom {:readers #{:result/score}
                         :expression [:present [:read :result/score]]}]
        unsafe (src {:result/score optional-var}
                    [:and presence p]
                    {:premise/presence presence :premise/p p}
                    [(premise :step/presence :premise/presence presence)
                     (premise :step/p :premise/p p)
                     (s :step/both :conjunction-introduction [:and presence p]
                        :from [:step/presence :step/p])])
        safe [:atom {:readers #{:result/score}
                     :expression [:and [:present [:read :result/score]]
                                  [:> [:read :result/score] [:literal 7]]]}]]
    (rejects! unsafe)
    (is (= :conditional-on-open-premises
           (:conclusion-status
            (kernel/check-derivation!
             (src {:result/score optional-var} safe
                  {:premise/safe safe}
                  [(premise :step/safe :premise/safe safe)])))))))

(deftest cannot-invent-arithmetic-axioms-or-atomic-truth
  (let [greater [:atom {:readers #{:result/score}
                        :expression [:> [:read :result/score] [:literal 7]]}]
        greater-equal [:atom {:readers #{:result/score}
                              :expression [:>= [:read :result/score] [:literal 7]]}]
        source (src {:result/score (:result/score vars)}
                    [:implies greater greater-equal] {}
                    [(s :step/fraud :implication-reflexivity
                        [:implies greater greater-equal])])]
    (kernel-rejects! source :invalid-implication-reflexivity)
    (kernel-rejects!
     (assoc source :goal greater :steps [(s :step/fraud :truth-introduction greater)])
     :invalid-truth-introduction)
    (is (= :conditional-on-open-premises
           (:conclusion-status
            (kernel/check-derivation!
             (src {:result/score (:result/score vars)} greater-equal
                  {:premise/claim [:implies greater greater-equal]
                   :premise/p greater}
                  [(premise :step/claim :premise/claim
                            [:implies greater greater-equal])
                   (premise :step/p :premise/p greater)
                   (s :step/derived :implication-elimination greater-equal
                      :from [:step/claim :step/p])])))))))

(deftest normalized-equivalent-integer-literals-replay-consistently
  (let [source (imp-proof)
        update-lit (fn [tree f]
                     (clojure.walk/postwalk
                      (fn [v] (if (and (vector? v) (= :literal (first v))
                                       (integer? (second v)))
                                [:literal (f (second v))]
                                v)) tree))
        updated (update-lit source bigint)]
    (is (= (:fingerprint (kernel/check-derivation! source))
           (:fingerprint (kernel/check-derivation! updated))))
    (is (= (:goal-fingerprint (kernel/check-derivation! source))
           (:goal-fingerprint (kernel/check-derivation! updated))))))

(deftest deterministic-mutation-matrix-cannot-turn-false-inference-true
  (let [base (src {:result/score (:result/score vars)}
                  [:implies p p] {}
                  [(s :step/refl :implication-reflexivity [:implies p p])])]
    (doseq [n (range -60 90)]
      (let [different [:atom {:readers #{:result/score}
                              :expression [:> [:read :result/score]
                                             [:literal n]]}]
            forged (assoc base :goal [:implies p different]
                          :steps [(s :step/fraud :implication-reflexivity
                                    [:implies p different])])]
        (testing (str "threshold=" n)
          (if (= n 7)
            (is (= :checked-without-premises
                   (:conclusion-status (kernel/check-derivation! forged))))
            (kernel-rejects! forged :invalid-implication-reflexivity)))))))

(deftest graph-reachability-checks-all-declared-evidence
  (let [source (and-proof)]
    (kernel-rejects!
     (assoc source :steps (vec (concat [(s :step/stray :truth-introduction [:true])]
                                       (:steps source))))
     :unused-proof-steps)
    (kernel-rejects!
     (assoc source :premises (assoc (:premises source)
                                   :premise/ghost [:false]))
     :unused-premises)
    (is (= #{:premise/p :premise/q}
           (:open-premises (kernel/check-derivation! source))))))

(deftest resource-and-data-boundaries-are-enforced
  (let [source (true-proof)]
    (kernel-rejects! (assoc source :steps []) :invalid-steps)
    (kernel-rejects! (assoc source :steps
                           (vec (repeat 257 (s :step/true :truth-introduction [:true]))))
                     :invalid-steps)
    (kernel-rejects! (assoc source :premises
                           (into {} (for [n (range 129)]
                                      [(keyword "premise" (str n)) [:true]])))
                     :premise-limit)
    (kernel-rejects! (assoc source :variables
                           (into {} (for [n (range 257)]
                                      [(keyword "example" (str n)) {:type :integer}])))
                     :variable-limit)
    (kernel-rejects! (assoc-in source [:steps 0 :from] [:step/true])
                     :wrong-keys)
    (kernel-rejects! (assoc source :steps '({:id :step/true :rule :truth-introduction
                                            :conclusion [:true]}))
                     :invalid-steps)
    ;; Semantic subject identity explicitly ignores source metadata (v1211).
    (is (= (:fingerprint (kernel/check-derivation! source))
           (:fingerprint (kernel/check-derivation!
                          (assoc source :subject (with-meta subj {:trust true}))))))
    (rejects! (assoc source :goal (with-meta [:true] {:proved true})))
    (rejects! (assoc source :steps [(with-meta (first (:steps source)) {:proved true})]))))
