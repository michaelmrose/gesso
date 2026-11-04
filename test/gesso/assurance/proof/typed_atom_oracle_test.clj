(ns gesso.assurance.proof.typed-atom-oracle-test
  "v1321: permanent adversarial tests for the independent bounded typed-atom
   semantics audit (v1320). Every fixture passes ORIGINAL rule/type/domain
   declarations, not a checked proof report or a certificate.

   In particular, success over representative integers or text is never a
   universal theorem. Even an exhaustive declared Boolean/enum domain checks
   an executable implementation on finitely many values, not its soundness or
   the provenance of production snapshots. Reports remain copyable diagnostics
   with no admission or transaction authority."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.typed-atom-oracle :as oracle]
            [gesso.assurance.rule-eval :as production]
            [gesso.assurance.rule-type :as production-types]
            [gesso.assurance.rule :as production-rule]))

(def ^:private bool {:type :boolean})
(def ^:private int-type {:type :integer})
(def ^:private status-type
  {:type :enum :name :request/status :values #{:open :claimed :done}})
(def ^:private optional-score
  {:type :integer :optional? true :nullable? true})

(defn- source
  ([expression] (source expression {} {}))
  ([expression types domains]
   {:rule {:readers (set (keys types)) :expression expression}
    :reader-types types
    :binding-domains domains
    :max-cases 8192}))

(defn- audit [s] (oracle/audit-typed-atom! s))

(defn- unauthoritative! [r]
  (is (= oracle/report-type (:type r)) (pr-str r))
  (is (= oracle/oracle-version (:version r)))
  (is (= :not-proved (:proof-status r)))
  (is (= :not-admitted (:admission r)))
  (is (= :none (:authority r)))
  (is (not (contains? r :certificate)))
  (is (not (contains? r :handle)))
  r)

(defn- agreement! [s expected-cases expected-true coverage]
  (let [r (unauthoritative! (audit s))]
    (is (= :bounded-agreement (:status r)) (pr-str r))
    (is (= :independent-typing-and-finite-evaluation (:checking r)))
    (is (= :supplied-binding-domains-only (:scope r)))
    (is (= expected-cases (:cases-planned r)))
    (is (= expected-cases (:cases-checked r)))
    (is (= expected-true (:true-cases r)))
    (is (= (- expected-cases expected-true) (:false-cases r)))
    (is (= coverage (:domain-coverage r)))
    (is (= :not-established (:atomic-semantics-universality r)))
    (is (= :not-established (:independent-checker-soundness r)))
    (is (= :not-established (:jvm-cljs-equivalence r)))
    (is (= :not-established (:trusted-snapshot-provenance r)))
    (is (= :not-established (:runtime-enforcement r)))
    (is (= :none (:transaction-authority r)))
    r))

(defn- failure! [s status reason at]
  (let [r (unauthoritative! (audit s))]
    (is (= status (:status r)) (pr-str r))
    (is (= reason (:reason r)) (pr-str r))
    (when at (is (= at (:at r)) (pr-str r)))
    r))

(deftest constant-boolean-truth-tables-have-one-empty-binding
  (doseq [[expression expected]
          [[[:literal true] 1]
           [[:literal false] 0]
           [[:not [:literal false]] 1]
           [[:not [:not [:literal true]]] 1]
           [[:and [:literal true] [:literal false]] 0]
           [[:or [:literal false] [:literal true]] 1]
           [[:implies [:literal false] [:literal false]] 1]
           [[:implies [:literal true] [:literal false]] 0]
           [[:= [:literal 0N] [:literal 0]] 1]
           [[:not= [:literal :request/open] [:literal :request/done]] 1]
           [[:in [:literal :request/open]
             [:literal #{:request/open :request/done}]] 1]
           [[:>= [:literal 9223372036854775808N]
             [:literal 9223372036854775807N]] 1]]]
    (testing (pr-str expression)
      (let [r (agreement! (source expression) 1 expected
                          :complete-declared-finite-values)]
        (is (= #{} (:reader-ids r)))))))

(deftest all-boolean-bindings-and-nary-connectives
  (let [types {:request/p bool :request/q bool :request/r bool}
        domains {:request/r [true false]
                 :request/q [true false]
                 :request/p [true false]}
        a [:read :request/p]
        b [:read :request/q]
        c [:read :request/r]]
    (agreement! (source [:and a b c] types domains) 8 1
                :complete-declared-finite-values)
    (agreement! (source [:or a b c] types domains) 8 7
                :complete-declared-finite-values)
    (agreement! (source [:implies [:and a b] c] types domains) 8 7
                :complete-declared-finite-values)
    (agreement! (source [:= [:and a b] [:not [:or [:not a] [:not b]]]]
                        {:request/p bool :request/q bool}
                        {:request/p [false true] :request/q [false true]})
                4 4 :complete-declared-finite-values)))

(deftest complete-enum-boolean-product-with-exact-case-counts
  (let [types {:request/status status-type :request/approved bool}
        domains {:request/approved [false true]
                 :request/status [:done :claimed :open]}
        e [:and [:read :request/approved]
           [:in [:read :request/status] [:literal #{:open :claimed}]]]
        r (agreement! (source e types domains) 6 2
                      :complete-declared-finite-values)]
    (is (= #{:request/status :request/approved} (:reader-ids r)))
    (is (= r (audit (source e types domains))))))

(deftest enum-equality-preserves-nominal-identity
  (agreement!
   (source [:= [:read :request/status] [:literal :open]]
           {:request/status status-type}
           {:request/status [:open :claimed :done]})
   3 1 :complete-declared-finite-values)
  (failure!
   (source [:= [:read :request/a] [:read :request/b]]
           {:request/a {:type :enum :name :request/a :values #{:open :done}}
            :request/b {:type :enum :name :request/b :values #{:open :done}}}
           {:request/a [:open :done] :request/b [:open :done]})
   :unsupported-language :incompatible-equality [:rule :expression])
  (failure!
   (source [:= [:read :request/status] [:literal :invalid]]
           {:request/status status-type}
           {:request/status [:open :claimed :done]})
   :unsupported-language :enum-literal-outside-domain [:rule :expression]))

(deftest optional-presence-is-distinct-from-truth-value
  (let [r [:read :request/flag]
        t {:request/flag {:type :boolean :optional? true :nullable? true}}
        d {:request/flag [oracle/absent nil false true]}]
    (agreement! (source [:present r] t d) 4 3
                :complete-declared-finite-values)
    (agreement! (source [:and [:present r]
                          [:not= r [:literal nil]] r] t d)
                4 1 :complete-declared-finite-values)
    (agreement! (source [:or [:not [:present r]]
                         [:= r [:literal nil]] r] t d)
                4 3 :complete-declared-finite-values)))

(deftest optional-integer-never-reads-absent-value-under-and
  (let [r [:read :request/score]
        t {:request/score optional-score}
        d {:request/score [oracle/absent nil -1 7 8]}
        e [:and [:present r]
           [:not= r [:literal nil]]
           [:>= r [:literal 7]]]]
    (agreement! (source e t d) 5 2 :representative-values)
    (agreement! (source [:not e] t d) 5 3 :representative-values)))

(deftest optional-integer-never-reads-absent-value-under-or
  (let [r [:read :request/score]
        t {:request/score optional-score}
        d {:request/score [oracle/absent nil 0 7 8]}
        e [:or [:not [:present r]]
           [:= r [:literal nil]]
           [:> r [:literal 7]]]]
    (agreement! (source e t d) 5 3 :representative-values)))

(deftest implication-guards-optional-and-nullable-reads
  (let [r [:read :request/score]
        t {:request/score optional-score}
        d {:request/score [oracle/absent nil 5 7 9]}
        e [:implies [:and [:present r] [:not= r [:literal nil]]]
           [:>= r [:literal 7]]]]
    (agreement! (source e t d) 5 4 :representative-values)))

(deftest short-circuit-refinement-is-statically-required-even-if-sample-safe
  (let [r [:read :request/score]
        t {:request/score optional-score}
        d {:request/score [7 9]}]
    (failure! (source [:> r [:literal 1]] t d)
              :unsupported-language :unguarded-optional-read
              [:rule :expression 1])
    (failure! (source [:and [:present r] [:> r [:literal 1]]] t d)
              :unsupported-language :noninteger-order
              [:rule :expression 2])
    (failure! (source [:or [:not [:present r]] [:> r [:literal 1]]] t d)
              :unsupported-language :noninteger-order
              [:rule :expression 2])))

(deftest nullable-boolean-requires-a-nonnull-guard
  (let [r [:read :request/active]
        t {:request/active {:type :boolean :nullable? true}}
        d {:request/active [nil false true]}]
    (failure! (source r t d) :unsupported-language
              :nonboolean-expression [:rule :expression])
    (agreement! (source [:and [:not= r [:literal nil]] r] t d)
                3 1 :complete-declared-finite-values)
    (agreement! (source [:or [:= r [:literal nil]] r] t d)
                3 2 :complete-declared-finite-values)))

(deftest optional-enum-presence-and-membership
  (let [r [:read :request/status]
        t {:request/status (assoc status-type :optional? true)}
        d {:request/status [oracle/absent :open :claimed :done]}
        e [:or [:not [:present r]]
           [:in r [:literal #{:open :claimed}]]]]
    (agreement! (source e t d) 4 3
                :complete-declared-finite-values)))

(deftest integer-ordering-uses-arbitrary-precision-samples
  (let [t {:request/version int-type}
        d {:request/version [-999999999999999999999999999999N
                             -1 0 1 9223372036854775808N]}
        r [:read :request/version]]
    (doseq [[op expected]
            [[:< 2] [:<= 3] [:> 2] [:>= 3]]]
      (agreement! (source [op r [:literal 0N]] t d) 5 expected
                  :representative-values))))

(deftest set-membership-is-typed-and-independent
  (let [t {:request/version int-type}
        d {:request/version [-2 -1 0 1 2]}
        r [:read :request/version]]
    (agreement! (source [:in r [:literal #{-1 0 1}]] t d)
                5 3 :representative-values)
    (failure! (source [:in r [:literal #{:open :done}]] t d)
              :unsupported-language :incompatible-membership
              [:rule :expression])
    (failure! (source [:in r [:literal #{nil 0}]] t d)
              :unsupported-language :heterogeneous-literal-set
              [:rule :expression 2 1])))

(deftest nonboolean-root-never-counts-as-success
  (doseq [expression [[:literal 7] [:literal nil]
                      [:literal "text"] [:literal :open]
                      [:literal #{:open :done}]]]
    (failure! (source expression)
              :unsupported-language :nonboolean-expression
              [:rule :expression])))

(deftest all-reader-declarations-must-be-used-and-qualified
  (failure!
   (source [:literal true] {:request/unused bool}
           {:request/unused [false true]})
   :invalid-source :unused-reader [:rule :readers])
  (failure!
   (assoc-in (source [:read :request/active]
                     {:request/active bool} {:request/active [false true]})
             [:rule :readers] #{:not-qualified})
   :invalid-source :invalid-readers [:rule :readers])
  (failure!
   (source [:present [:read :request/unlisted]] {} {})
   :invalid-source :undeclared-reader [:rule :expression 1]))

(deftest malformed-expression-arity-and-operators-fail-closed
  (doseq [expression [[:eval [:literal true]]
                      [:and [:literal true]]
                      [:or [:literal false]]
                      [:not [:literal true] [:literal false]]
                      [:= [:literal 1]]
                      [:present [:literal false]]
                      [:present :request/active]
                      [:literal true :extra]
                      [:read :request/unknown]
                      '(:literal true)]]
    (let [result (unauthoritative! (audit (source expression)))]
      (is (= :invalid-source (:status result)) (pr-str [expression result])))))

(deftest bad-boolean-and-equality-types-are-not-executable-proofs
  (doseq [[expression reason]
          [[[:not [:literal 7]] :nonboolean-expression]
           [[:and [:literal true] [:literal 1]] :nonboolean-expression]
           [[:implies [:literal 7] [:literal true]] :nonboolean-expression]
                      [[:< [:literal :open] [:literal :done]] :noninteger-order]
           [[:= [:literal true] [:literal 1]] :incompatible-equality]]]
    (let [r (unauthoritative! (audit (source expression)))]
      (is (= :unsupported-language (:status r)) (pr-str [expression r]))
      (is (= reason (:reason r)) (pr-str [expression r])))))

(deftest malformed-type-descriptors-are-diagnostic-failures
  (let [base (source [:present [:read :request/value]]
                     {:request/value (assoc bool :optional? true)}
                     {:request/value [oracle/absent true false]})]
    (doseq [[desc status reason]
            [[{:type :dragon} :unsupported-language :unknown-type]
             [{:type :boolean :optional? :yes} :invalid-source
              :nonboolean-descriptor-flag]
             [{:type :boolean :nullable? 1} :invalid-source
              :nonboolean-descriptor-flag]
             [{:type :boolean :name :request/wrong} :invalid-source
              :unexpected-enum-keys]
             [{:type :enum :name :unqualified :values #{:open}}
              :invalid-source :invalid-enum-name]
             [{:type :enum :name :request/status :values #{}}
              :invalid-source :invalid-enum-domain]
             [{:type :enum :name :request/status :values #{7}}
              :invalid-source :invalid-enum-member]
             [{:type :boolean :extra :value} :invalid-source
              :unknown-descriptor-keys]]]
      (let [r (unauthoritative! (audit (assoc-in base
                                                 [:reader-types :request/value]
                                                 desc)))]
        (is (= status (:status r)) (pr-str [desc r]))
        (is (= reason (:reason r)) (pr-str [desc r]))))))

(deftest invalid-domains-never-fabricate-finite-completeness
  (let [r [:read :request/active]
        t {:request/active (assoc bool :optional? true)}
        base (source [:present r] t {:request/active [oracle/absent false true]})]
    (doseq [[domain reason]
            [[[false false] :duplicate-domain-entry]
             [[nil false] :ill-typed-domain-entry]
             [[1 true] :ill-typed-domain-entry]
             [[] :invalid-domain]
             [nil :invalid-domain]]]
      (let [result (audit (assoc-in base [:binding-domains :request/active]
                                    domain))]
        (is (= :invalid-source (:status result)) (pr-str result))
        (is (= reason (:reason result)) (pr-str result))))
    (let [no-absence (audit (assoc-in base [:binding-domains :request/active]
                                    [false true]))]
      (is (= :bounded-agreement (:status no-absence)))
      (is (= :representative-values (:domain-coverage no-absence))))))

(deftest absence-is-only-legal-for-optional-descriptors
  (failure!
   (source [:present [:read :request/active]]
           {:request/active bool}
           {:request/active [oracle/absent false true]})
   :invalid-source :absent-required-field
   [:binding-domains :request/active]))

(deftest domain-shape-and-budget-fail-before-evaluation
  (let [base (source [:literal true] {} {})]
    (failure! (assoc base :max-cases 0)
              :invalid-source :invalid-budget [:max-cases])
    (failure! (assoc base :max-cases 8193)
              :invalid-source :invalid-budget [:max-cases])
    (failure! (assoc base :max-cases :many)
              :invalid-source :invalid-budget [:max-cases])
    (failure! (assoc base :binding-domains {:request/extra [true]})
              :invalid-source :wrong-keys [:binding-domains]))
  (let [t (into {} (for [i (range 4)]
                     [(keyword "request" (str "n" i)) int-type]))
        d (into {} (for [k (keys t)] [k (vec (range 24))]))
        reads (mapv (fn [k] [:read k]) (sort-by str (keys t)))
        e (into [:and] (map (fn [r] [:> r [:literal -1]]) reads))
        base (source e t d)]
    (failure! base :budget-exceeded :enumeration-limit [:max-cases])
    (let [r (failure! (assoc base :max-cases 1)
                      :budget-exceeded :enumeration-limit [:max-cases])]
      (is (not (contains? r :cases-checked))))))

(deftest excessive-declarations-and-expression-depth-fail-closed
  (let [expr (nth (iterate (fn [e] [:not e]) [:literal true]) 33)
        r (audit (source expr))]
    (is (= :unsupported-language (:status r)) (pr-str r))
    (is (= :expression-too-deep (:reason r))))
  (let [types (into {} (for [i (range 13)]
                         [(keyword "request" (str "f" i)) bool]))
        domains (into {} (map (fn [id] [id [true false]]) (keys types)))
        r (audit (source [:literal true] types domains))]
    (is (= :invalid-source (:status r)))
    (is (= :invalid-readers (:reason r)))))

(deftest forged-source-and-nested-metadata-are-rejected
  (let [base (source [:literal true])]
    (doseq [mutation [(assoc base :certificate :fake)
                     (assoc base :valid? true)
                     (assoc base :authority :transaction)
                     (assoc base :rule {:readers #{} :expression [:literal true]
                                        :proof-status :proved})
                     (with-meta base {:trusted true})
                     (assoc base :rule (with-meta (:rule base) {:trusted true}))
                     (assoc-in base [:rule :expression]
                               (with-meta [:literal true] {:proof true}))
                     (assoc base :reader-types (sorted-map))
                     (assoc base :binding-domains (sorted-map))
                     (assoc-in base [:rule :expression]
                               [:literal (fn [] true)])]]
      (let [r (unauthoritative! (audit mutation))]
        (is (not= :bounded-agreement (:status r)) (pr-str r)))))
  (let [base (source [:in [:literal :open] [:literal #{:open :done}]])]
    (failure! (assoc-in base [:rule :expression 2 1]
                        (with-meta #{:open :done} {:trusted true}))
              :invalid-source :metadata-not-allowed [:rule :expression 2 1])))

(deftest no-inference-from-sparse-domain-to-universal-coverage
  (let [r (agreement!
           (source [:> [:read :request/score] [:literal 7]]
                   {:request/score int-type}
                   {:request/score [8]})
           1 1 :representative-values)]
    (is (= :not-established (:atomic-semantics-universality r))))
  (let [r (agreement!
           (source [:present [:read :request/flag]]
                   {:request/flag bool}
                   {:request/flag [true]})
           1 1 :representative-values)]
    (is (= 1 (:cases-checked r)))))

(deftest deterministic-enumeration-order-selects-first-counterexample
  (let [base (source [:> [:read :request/score] [:literal 7]]
                     {:request/score int-type}
                     {:request/score [6 7 8]})
        original production/evaluate-rule!]
    (with-redefs [production/evaluate-rule!
                  (fn [rule types binding]
                    (assoc (original rule types binding) :value true))]
      (let [r (unauthoritative! (audit base))]
        (is (= :divergence (:status r)))
        (is (= :different-truth-value (:reason r)))
        (is (= {:request/score 6}
               (get-in r [:counterexample :bindings])))
        (is (false? (get-in r [:counterexample :independent-value])))
        (is (true? (get-in r [:counterexample :production-value])))
        (is (= 1 (:cases-checked r)))
        (is (= r (audit base)))))))

(deftest missing-production-evaluator-becomes-a-counterexample-not-a-pass
  (let [base (source [:literal true])]
    (with-redefs [production/evaluate-rule!
                  (fn [& _] (throw (ex-info "deliberately disabled"
                                           {:reason :no-evaluator})))]
      (let [r (unauthoritative! (audit base))]
        (is (= :divergence (:status r)))
        (is (= :production-evaluator-rejected (:reason r)))
        (is (= :no-evaluator
               (get-in r [:counterexample :production-error])))
        (is (= true (get-in r [:counterexample :independent-value])))))))

(deftest invalid-production-result-never-inherits-a-proof-claim
  (let [base (source [:literal true])]
    (doseq [response [{:value true}
                      {:type production/evaluated-type :value true
                       :proof-status :proved :admission :not-admitted}
                      {:type production/evaluated-type :value true
                       :proof-status :not-proved :admission :admitted}
                      {:type production/evaluated-type :value true
                       :proof-status :not-proved :admission :not-admitted
                       :authority :transaction}]]
      (with-redefs [production/evaluate-rule! (fn [& _] response)]
        (let [r (unauthoritative! (audit base))]
          (is (= :divergence (:status r)))
          (is (= :invalid-production-report (:reason r)))
          (is (nil? (get-in r [:counterexample :production-value]))))))))

(deftest independent-static-analysis-does-not-call-production-type-checker
  (let [base (source [:and [:present [:read :request/score]]
                           [:not= [:read :request/score] [:literal nil]]
                           [:> [:read :request/score] [:literal 7]]]
                     {:request/score optional-score}
                     {:request/score [oracle/absent nil 8]})]
    (with-redefs [production-rule/check-rule!
                  (fn [& _] (throw (ex-info "syntax stub" {})))
                  production-types/check-rule-type!
                  (fn [& _] (throw (ex-info "type stub" {})))
                  production/evaluate-rule!
                  (fn [& _] (throw (ex-info "evaluation stub" {})))]
      (let [r (unauthoritative! (audit base))]
        (is (= :divergence (:status r)))
        (is (= :production-evaluator-rejected (:reason r)))
        (is (= false (get-in r [:counterexample :independent-value])))
        (is (= {} (get-in r [:counterexample :bindings]))
            (pr-str r))))))

(deftest successful-diagnostics-remain-copyable-non-admissible-data
  (let [r (agreement! (source [:literal true]) 1 1
                      :complete-declared-finite-values)
        modified (assoc r :authority :transaction
                        :proof-status :proved
                        :admission :admitted)]
    (is (= :bounded-agreement (:status modified)))
    (is (not= r modified))
    (is (= :none (:authority r)))
    (is (not= :admitted (:admission r)))
    (is (not (contains? r :authentication)))))

(deftest strings-and-keywords-are-representative-domains-only
  (agreement!
   (source [:= [:read :request/label] [:literal "gesso"]]
           {:request/label {:type :string}}
           {:request/label ["" "gesso" "Gesso"]})
   3 1 :representative-values)
  (agreement!
   (source [:not= [:read :request/tag] [:literal :request/absent]]
           {:request/tag {:type :keyword}}
           {:request/tag [:request/absent :request/active]})
   2 1 :representative-values))

(deftest nullable-optional-enum-requires-both-presence-and-nonnull
  (let [r [:read :request/status]
        t {:request/status (assoc status-type :optional? true :nullable? true)}
        d {:request/status [oracle/absent nil :open :claimed :done]}
        e [:or [:not [:present r]]
           [:and [:not= r [:literal nil]]
            [:in r [:literal #{:open :claimed}]]]]]
    (agreement! (source e t d) 5 3
                :complete-declared-finite-values)))

(deftest metadata-on-the-domain-container-is-rejected
  (let [base (source [:present [:read :request/flag]]
                     {:request/flag {:type :boolean :optional? true}}
                     {:request/flag [oracle/absent false true]})]
    (failure!
     (assoc-in base [:binding-domains :request/flag]
               (with-meta [oracle/absent false true] {:approved true}))
     :invalid-source :metadata-not-allowed
     [:binding-domains :request/flag])
    (failure!
     (assoc-in base [:binding-domains :request/flag]
               [(with-meta oracle/absent {:approved true}) false true])
     :invalid-source :metadata-not-allowed
     [:binding-domains :request/flag])))
