(ns gesso.assurance.proof.typed-atom-mutation-test
  "v1324: permanent adversarial semantic mutation tests for v1320/v1322.

   Mutate the ORIGINAL production interpreter's individual operator calls,
   rather than fabricating a checker report or indiscriminately flipping the
   final Boolean. The independent typed-atom oracle must detect *activated*
   semantic faults over explicitly declared binding values. A fault that is
   masked by a short-circuit or absent from the sampled expression corpus
   MUST NOT be counted as killed. Neither mutation testing nor a finite
   clean run proves semantic soundness or permits runtime authority."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.typed-atom-oracle :as independent]
            [gesso.assurance.proof.typed-atom-fuzz :as fuzz]
            [gesso.assurance.rule-eval :as production]))

(def ^:private bool {:type :boolean})
(def ^:private integer-type {:type :integer})
(def ^:private optional-bool
  {:type :boolean :optional? true :nullable? true})
(def ^:private optional-integer
  {:type :integer :optional? true})
(def ^:private status-type
  {:type :enum :name :request/status
   :values #{:request/open :request/claimed :request/done}})
(def ^:private score :request/score)
(def ^:private active :request/active)
(def ^:private status :request/status)
(def ^:private rscore [:read :request/score])
(def ^:private ractive [:read :request/active])
(def ^:private rstatus [:read :request/status])
(def ^:private missing independent/absent)

(defn- source
  ([expr] (source expr {} {}))
  ([expr types domains]
   {:rule {:readers (set (keys types)) :expression expr}
    :reader-types types :binding-domains domains :max-cases 512}))

(defn- configuration
  ([types domains seed]
   {:reader-types types :binding-domains domains :seed seed
    :max-expressions 256 :max-cases 512 :max-total-cases 32768})
  ([types domains seed trials]
   (assoc (configuration types domains seed) :max-expressions trials)))

(def ^:private interpreter-var
  (or (ns-resolve 'gesso.assurance.rule-eval 'interpret!)
      (throw (ex-info "Cannot instrument production expression interpreter."
                      {:reason :missing-production-interpreter}))))

(defn- mutate-interpreter!
  "Run thunk with a targeted production interpreter mutation. Return
   the audit outcome and the precise number of targeted operator calls.
   Rebinding is lexically restored even when a check fails or throws."
  [target replacement thunk]
  (let [original @interpreter-var
        activations (atom [])
        replacement-fn
        (fn [expression bindings at]
          (let [original-value (original expression bindings at)]
            (if (= target (first expression))
              (do (swap! activations conj {:expression expression
                                           :bindings bindings :at at})
                  (replacement expression bindings at original-value))
              original-value)))
        report (with-redefs-fn {interpreter-var replacement-fn} thunk)]
    {:report report :activations @activations}))

(defn- flip-boolean [_ _ _ value] (not value))

(defn- assert-diagnostic! [report]
  (is (= independent/report-type (:type report)) (pr-str report))
  (is (= independent/oracle-version (:version report)))
  (is (= :not-proved (:proof-status report)))
  (is (= :not-admitted (:admission report)))
  (is (= :none (:authority report)))
  (is (= :none (:transaction-authority report)))
  (is (= :not-established (:independent-checker-soundness report)))
  (is (= :not-established (:atomic-semantics-universality report)))
  (is (not (contains? report :certificate)))
  (is (not (contains? report :transaction-handle)))
  report)

(defn- assert-killed! [source target replacement]
  (let [original (independent/audit-typed-atom! source)
        {changed :report activations :activations}
        (mutate-interpreter! target replacement
                             #(independent/audit-typed-atom! source))
        restored (independent/audit-typed-atom! source)]
    (is (= :bounded-agreement (:status original)) (pr-str original))
    (is (= original restored) (str "Mutation leaked into " target))
    (is (seq activations) (str "Operator not exercised: " target))
    (assert-diagnostic! changed)
    (is (= :divergence (:status changed)) (pr-str [target changed]))
    (is (#{:different-truth-value
           :production-evaluator-rejected
           :invalid-production-report}
         (:reason changed)) (pr-str changed))
    (is (map? (:counterexample changed)))
    (is (<= 1 (:cases-checked changed) (:cases-planned changed)))
    changed))

(defn- expression-contains? [expr wanted]
  (and (vector? expr)
       (or (= wanted (first expr))
           (boolean (some #(expression-contains? % wanted)
                          (rest expr))))))

(defn- assert-fuzz-diagnostic! [report]
  (is (= fuzz/report-type (:type report)))
  (is (= :none (:authority report)))
  (is (= :not-proved (:proof-status report)))
  (is (= :not-admitted (:admission report)))
  (is (= :not-established (:atomic-semantics-universality report)))
  (is (= :not-established (:expression-language-exhaustiveness report)))
  (is (= :not-established (:independent-checker-soundness report)))
  report)

(def ^:private operator-cases
  [{:op :literal :expr [:literal true] :types {} :domains {}}
   {:op :read :expr ractive :types {active bool}
    :domains {active [false true]}}
   {:op :present :expr [:present ractive] :types {active optional-bool}
    :domains {active [missing nil false true]}}
   {:op :not :expr [:not ractive] :types {active bool}
    :domains {active [false true]}}
   {:op :and :expr [:and ractive [:literal true]] :types {active bool}
    :domains {active [false true]}}
   {:op :or :expr [:or ractive [:literal false]] :types {active bool}
    :domains {active [false true]}}
   {:op :implies :expr [:implies ractive [:literal false]]
    :types {active bool} :domains {active [false true]}}
   {:op := :expr [:= rscore [:literal 0]]
    :types {score integer-type} :domains {score [-1 0 1]}}
   {:op :not= :expr [:not= rscore [:literal 0]]
    :types {score integer-type} :domains {score [-1 0 1]}}
   {:op :< :expr [:< rscore [:literal 0]]
    :types {score integer-type} :domains {score [-1 0 1]}}
   {:op :<= :expr [:<= rscore [:literal 0]]
    :types {score integer-type} :domains {score [-1 0 1]}}
   {:op :> :expr [:> rscore [:literal 0]]
    :types {score integer-type} :domains {score [-1 0 1]}}
   {:op :>= :expr [:>= rscore [:literal 0]]
    :types {score integer-type} :domains {score [-1 0 1]}}
   {:op :in :expr [:in rstatus [:literal #{:request/open :request/done}]]
    :types {status status-type}
    :domains {status [:request/open :request/claimed :request/done]}}])

(deftest semantic-operator-mutation-matrix-kills-every-activated-family
  (is (= 14 (count operator-cases)))
  (is (= 14 (count (set (map :op operator-cases)))))
  (doseq [{:keys [op expr types domains]} operator-cases]
    (testing (str "mutated expression operator " op)
      (let [result (assert-killed! (source expr types domains)
                                   op flip-boolean)]
        (is (= :different-truth-value (:reason result)))
        (is (instance? Boolean
                       (get-in result [:counterexample :independent-value])))
        (is (instance? Boolean
                       (get-in result [:counterexample :production-value])))))))

(deftest mutation-is-local-to-the-targeted-operator
  (let [s (source [:< rscore [:literal 0]]
                  {score integer-type} {score [-1 0 1]})
        normal (independent/audit-typed-atom! s)
        {:keys [report activations]}
        (mutate-interpreter! :> flip-boolean
                             #(independent/audit-typed-atom! s))]
    (is (= :bounded-agreement (:status normal)))
    (is (= normal report))
    (is (empty? activations))
    (is (= normal (independent/audit-typed-atom! s)))))

(deftest short-circuited-and-branch-does-not-count-as-a-killed-mutation
  (let [s (source [:and [:present rscore]
                        [:> rscore [:literal 0]]]
                  {score optional-integer} {score [missing]})
        {:keys [report activations]}
        (mutate-interpreter! :> flip-boolean
                             #(independent/audit-typed-atom! s))]
    (is (= :bounded-agreement (:status report)) (pr-str report))
    (is (= 1 (:cases-checked report)))
    (is (empty? activations))
    (is (= :divergence
           (:status (:report
                     (mutate-interpreter! :and flip-boolean
                                          #(independent/audit-typed-atom! s))))))))

(deftest short-circuited-or-branch-does-not-count-as-a-killed-mutation
  (let [s (source [:or [:not [:present rscore]]
                       [:> rscore [:literal 0]]]
                  {score optional-integer} {score [missing]})
        {:keys [report activations]}
        (mutate-interpreter! :> flip-boolean
                             #(independent/audit-typed-atom! s))]
    (is (= :bounded-agreement (:status report)) (pr-str report))
    (is (empty? activations))
    (is (= :divergence
           (:status (:report
                     (mutate-interpreter! :or flip-boolean
                                          #(independent/audit-typed-atom! s))))))))

(deftest false-antecedent-masks-a-consequent-mutation
  (let [s (source [:implies [:not [:present rscore]]
                            [:literal false]]
                  {score optional-integer} {score [0]})
        {:keys [report activations]}
        (mutate-interpreter! :literal flip-boolean
                             #(independent/audit-typed-atom! s))]
    (is (= :bounded-agreement (:status report)))
    (is (empty? activations))
    (is (= :divergence
           (:status (:report
                     (mutate-interpreter! :implies flip-boolean
                                          #(independent/audit-typed-atom! s))))))))

(defn- eager-boolean [target expression bindings at original-value]
  ;; Execute each later node after the actual short-circuited result. This
  ;; models a production implementation that incorrectly evaluates a guarded
  ;; :read; the original interpreter supplies the real typed error.
  (when (and (#{:and :or :implies} target)
             (= target (first expression)))
    (let [trailing (last expression)
          original @interpreter-var]
      ;; Calling the Var under the mutation requires the non-target child
      ;; interpreter; the extra guarded read must not become a normal result.
      (original trailing bindings (conj at (dec (count expression))))))
  original-value)

(deftest eager-and-breaks-optional-read-safety
  (let [s (source [:and [:present rscore]
                        [:> rscore [:literal 0]]]
                  {score optional-integer} {score [missing 1]})]
    (is (= :bounded-agreement (:status (independent/audit-typed-atom! s))))
    (let [result (assert-killed!
                  s :and (partial eager-boolean :and))]
      (is (= :production-evaluator-rejected (:reason result)))
      (is (= :absent-reader-evaluated
             (get-in result [:counterexample :production-error]))))))

(deftest eager-or-breaks-optional-read-safety
  (let [s (source [:or [:not [:present rscore]]
                       [:> rscore [:literal 0]]]
                  {score optional-integer} {score [missing 1]})]
    (let [result (assert-killed!
                  s :or (partial eager-boolean :or))]
      (is (= :production-evaluator-rejected (:reason result)))
      (is (= :absent-reader-evaluated
             (get-in result [:counterexample :production-error]))))))

(deftest eager-implication-breaks-optional-read-safety
  (let [s (source [:implies [:present rscore]
                            [:> rscore [:literal 0]]]
                  {score optional-integer} {score [missing 1]})]
    (let [result (assert-killed!
                  s :implies (partial eager-boolean :implies))]
      (is (= :production-evaluator-rejected (:reason result)))
      (is (= :absent-reader-evaluated
             (get-in result [:counterexample :production-error]))))))

(deftest optional-presence-must-not-be-implemented-as-non-nil
  (let [s (source [:present ractive]
                  {active optional-bool}
                  {active [missing nil false true]})
        changed (assert-killed!
                 s :present
                 (fn [_ bindings _ _] (some? (get bindings active))))]
    (is (= :different-truth-value (:reason changed)))
    (is (= {active nil} (get-in changed [:counterexample :bindings])))
    (is (= true (get-in changed [:counterexample :independent-value])))
    (is (= false (get-in changed [:counterexample :production-value])))))

(deftest optional-presence-must-not-be-implemented-as-truthiness
  (let [s (source [:present ractive]
                  {active optional-bool}
                  {active [missing false nil true]})
        changed (assert-killed!
                 s :present
                 (fn [_ bindings _ _] (boolean (get bindings active))))]
    (is (= {active false} (get-in changed [:counterexample :bindings])))
    (is (= true (get-in changed [:counterexample :independent-value])))
    (is (= false (get-in changed [:counterexample :production-value])))))

(deftest inclusive-integer-bounds-must-not-be-exclusive
  (doseq [op [:<= :>=]
          value [0 1 -1 9223372036854775808N
                 -9223372036854775809N]]
    (testing (str op " at the equality boundary " value)
      (let [s (source [op rscore [:literal value]]
                      {score integer-type} {score [value]})
            changed (assert-killed!
                     s op (fn [_ _ _ original-value]
                            (if original-value false original-value)))]
        (is (= true (get-in changed [:counterexample :independent-value])))
        (is (= false (get-in changed [:counterexample :production-value])))))))

(deftest operator-mutation-cannot-claim-universal-coverage
  (let [s (source [:literal true])
        original (independent/audit-typed-atom! s)
        {r :report activations :activations}
        (mutate-interpreter! :in flip-boolean
                             #(independent/audit-typed-atom! s))]
    (is (= original r))
    (is (= :bounded-agreement (:status r)))
    (is (empty? activations))
    (is (= :not-established (:atomic-semantics-universality r)))
    (is (= :none (:authority r)))))

(deftest mutations-can-be-detected-through-deterministic-generated-corpora
  (doseq [[target types domains seed]
          [[:> {score integer-type} {score [-5 0 7]} 22]
           [:in {status status-type}
            {status [:request/open :request/claimed :request/done]} 37]
           [:present {active optional-bool}
            {active [missing nil false true]} 11]
           [:not {active bool} {active [false true]} 41]
           [:and {active bool} {active [false true]} 79]
           [:or {active bool} {active [false true]} 61]
           [:implies {active bool} {active [false true]} 55]]]
    (testing (str "generated corpus finds activated " target)
      (let [c (configuration types domains seed)
            baseline (fuzz/audit-generated-atoms! c)
            {r :report activations :activations}
            (mutate-interpreter! target flip-boolean
                                 #(fuzz/audit-generated-atoms! c))]
        (is (= :bounded-agreement (:status baseline)) (pr-str baseline))
        (assert-fuzz-diagnostic! r)
        (is (= :divergence (:status r)) (pr-str [target r]))
        (is (seq activations))
        (is (= :different-truth-value (:reason r)))
        (is (= :divergence (get-in r [:shrunk :comparison :status])))
        (is (= :different-truth-value
               (get-in r [:shrunk :comparison :reason])))
        (is (= 1 (get-in r [:shrunk :comparison :cases-checked])))
        (is (expression-contains?
             (get-in r [:shrunk :source :rule :expression]) target))
        (is (= baseline (fuzz/audit-generated-atoms! c)))))))

(deftest generated-mandatory-presence-probe-kills-absence-mutation
  (let [c (configuration {active optional-bool}
                         {active [missing nil false true]} 2 1)
        {r :report activations :activations}
        (mutate-interpreter!
         :present
         (fn [_ bindings _ _] (some? (get bindings active)))
         #(fuzz/audit-generated-atoms! c))]
    (assert-fuzz-diagnostic! r)
    (is (= :divergence (:status r)))
    (is (= 1 (:expressions-checked r)))
    (is (seq activations))
    (is (= :different-truth-value (:reason r)))
    (is (= :none (:authority (get-in r [:shrunk :comparison]))))))

(deftest generated-corpus-is-limited-by-expressions-actually-selected
  (let [c (configuration {score integer-type}
                         {score [-1 0 1]} 0 1)
        {r :report activations :activations}
        (mutate-interpreter! :> flip-boolean
                             #(fuzz/audit-generated-atoms! c))]
    ;; A one-expression corpus contains the mandatory presence probe; it
    ;; cannot exercise the ordering mutant and must not count as a kill.
    (assert-fuzz-diagnostic! r)
    (is (= :bounded-agreement (:status r)))
    (is (= 1 (:expressions-checked r)))
    (is (empty? activations))
    (is (= :sampled-from-restricted-grammar (:expression-coverage r)))))


(deftest strict-ordering-mutants-turn-equality-into-the-wrong-answer
  ;; :< wrongly implemented as :<=, and :> wrongly implemented as :>=.
  (doseq [op [:< :>]
          x [-9223372036854775809N -1 0 1 9223372036854775808N]]
    (let [s (source [op rscore [:literal x]]
                    {score integer-type} {score [x]})
          r (assert-killed! s op (fn [_ _ _ _] true))]
      (is (= {score x} (get-in r [:counterexample :bindings])))
      (is (= false (get-in r [:counterexample :independent-value])))
      (is (= true (get-in r [:counterexample :production-value]))))))

(deftest floating-point-coercion-mutant-loses-integer-precision
  (let [limit 9007199254740992N
        larger 9007199254740993N
        s (source [:> rscore [:literal limit]]
                  {score integer-type} {score [larger]})
        changed (assert-killed!
                 s :> (fn [_ bindings _ _]
                        (> (double (get bindings score)) (double limit))))]
    (is (= {score larger} (get-in changed [:counterexample :bindings])))
    (is (= true (get-in changed [:counterexample :independent-value])))
    (is (= false (get-in changed [:counterexample :production-value])))))

(deftest an-activated-mutant-can-survive-a-missing-boundary-value
  ;; Coverage of an operator is NOT coverage of all its semantic values.
  (let [defect (fn [_ bindings _ value]
                 (if (= 9 (get bindings score)) (not value) value))
        sampled (source [:> rscore [:literal 0]]
                        {score integer-type} {score [-1 0 1]})
        expanded (assoc-in sampled [:binding-domains score] [-1 0 1 9])
        {sampled-report :report sampled-calls :activations}
        (mutate-interpreter! :> defect
                             #(independent/audit-typed-atom! sampled))
        {expanded-report :report expanded-calls :activations}
        (mutate-interpreter! :> defect
                             #(independent/audit-typed-atom! expanded))]
    (is (= :bounded-agreement (:status sampled-report)))
    (is (= 3 (count sampled-calls)))
    (is (= 3 (:cases-checked sampled-report)))
    (is (= :divergence (:status expanded-report)))
    (is (= {score 9} (get-in expanded-report [:counterexample :bindings])))
    (is (= 4 (count expanded-calls)))
    (is (= :none (:authority expanded-report)))
    (is (= :bounded-agreement
           (:status (independent/audit-typed-atom! expanded))))))

(deftest combined-operation-counterexample-retains-original-binding
  (let [s (source [:and [:> rscore [:literal 0]] ractive]
                  {score integer-type active bool}
                  {active [false true] score [-1 1]})
        changed (assert-killed! s :> flip-boolean)
        binding (get-in changed [:counterexample :bindings])]
    (is (contains? binding score))
    (is (contains? binding active))
    (is (#{-1 1} (get binding score)))
    (is (= true (get binding active)))
    (is (= :different-truth-value (:reason changed)))))

(deftest a-short-circuited-operator-is-not-counted-as-activated
  (let [s (source [:and [:literal false] [:> rscore [:literal 0]]]
                  {score integer-type} {score [1 2]})
        {r :report activations :activations}
        (mutate-interpreter! :> flip-boolean
                             #(independent/audit-typed-atom! s))]
    (is (= :bounded-agreement (:status r)))
    (is (empty? activations))
    (is (= 2 (:cases-checked r)))
    (is (= :none (:authority r)))))

(deftest bounded-generated-budget-does-not-accidentally-kill-a-mutant
  (let [c (configuration {score integer-type} {score [-1 0 1]} 44 80)
        c (assoc c :max-cases 3 :max-total-cases 3)
        {r :report activations :activations}
        (mutate-interpreter! :> flip-boolean
                             #(fuzz/audit-generated-atoms! c))]
    (is (= :budget-exceeded (:status r)))
    (is (= :aggregate-enumeration-limit (:reason r)))
    (is (= 1 (:expressions-checked r)))
    (is (empty? activations))
    (assert-fuzz-diagnostic! r)))

(deftest production-evaluator-exception-is-a-concrete-divergence
  (let [s (source [:= rscore [:literal 0]]
                  {score integer-type} {score [0 1]})]
    (let [changed (assert-killed!
                   s := (fn [& _]
                           (throw (ex-info "injected equality failure"
                                           {:reason :injected-equality}))))]
      (is (= :production-evaluator-rejected (:reason changed)))
      (is (= :injected-equality
             (get-in changed [:counterexample :production-error]))))))

(deftest forged-production-proof-report-is-not-accepted
  (let [s (source [:literal true])
        actual production/evaluate-rule!]
    (with-redefs [production/evaluate-rule!
                  (fn [rule types bindings]
                    (assoc (actual rule types bindings) :proof-status :proved))]
      (let [r (assert-diagnostic! (independent/audit-typed-atom! s))]
        (is (= :divergence (:status r)))
        (is (= :invalid-production-report (:reason r)))
        (is (= :none (:authority r)))))))

(deftest malformed-production-result-is-not-accepted-as-semantic-agreement
  (let [s (source [:literal true])]
    (with-redefs [production/evaluate-rule!
                  (fn [& _] {:value true :authority :transaction})]
      (let [r (assert-diagnostic! (independent/audit-typed-atom! s))]
        (is (= :divergence (:status r)))
        (is (= :invalid-production-report (:reason r)))))))

(deftest test-mutants-are-restored-after-exception
  (let [s (source [:not ractive] {active bool} {active [true false]})
        baseline (independent/audit-typed-atom! s)]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"intentional test interruption"
         (mutate-interpreter!
          :not
          (fn [& _] (throw (ex-info "intentional test interruption" {})))
          #(production/evaluate-rule! (:rule s) (:reader-types s)
                                      {active true}))))
    (is (= baseline (independent/audit-typed-atom! s)))
    (is (= :bounded-agreement (:status baseline)))))

(deftest disabled-mutation-never-produces-certification
  (let [s (source [:literal true])
        r (assert-diagnostic! (independent/audit-typed-atom! s))]
    (is (= :bounded-agreement (:status r)))
    (is (not (contains? r :proof-certificate)))
    (is (not (contains? r :tx-id)))
    (is (= :none (:authority r)))))
