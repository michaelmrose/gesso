(ns gesso.assurance.proof.kernel-typed-atom-semantic-correspondence-test
  "v1428: bounded *independent* semantic tests for the typed atoms carried by
   normalized proof propositions. A small test-local interpreter evaluates
   original and canonical atom expressions over closed finite typed states,
   and is compared with the production rule interpreter and the independent
   v1344 branch-sensitive interpreter.

   Covers nominal enum identity, optional/nullable presence, short-circuit
   evaluation and static rejection of unsafe dead branches. This does NOT
   prove arbitrary typed-atom equivalence, implementation soundness,
   JVM/CLJS portability, or certificate/transaction authority."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.proposition :as proposition]
            [gesso.assurance.proof.propositional-normalization-audit :as audit]
            [gesso.assurance.proof.typed-presence-semantics :as independent]
            [gesso.assurance.proof.typed-value-semantics :as values]
            [gesso.assurance.rule-eval :as production]))

(def ^:private subject
  {:kind :operation :id :example/typed-atom-correspondence
   :interface {:operation :example/typed-atom-correspondence}})
(def ^:private status :request/status)
(def ^:private score :request/score)
(def ^:private flag :request/flag)
(def ^:private status-types
  {status {:type :enum :name :request/status
           :values #{:request/new :request/claimed :request/done}
           :optional? true}})
(def ^:private other-nominal-types
  {status {:type :enum :name :organization/status
           :values #{:request/new :request/claimed :request/done}
           :optional? true}})
(def ^:private narrower-types
  {status {:type :enum :name :request/status
           :values #{:request/new :request/claimed} :optional? true}})
(def ^:private nullable-types
  {score {:type :integer :optional? true :nullable? true}})
(def ^:private optional-boolean-types
  {flag {:type :boolean :optional? true :nullable? true}})
(def ^:private pstatus [:present [:read status]])
(def ^:private pscore [:present [:read score]])
(def ^:private pflag [:present [:read flag]])
(def ^:private nonnil-score [:not= [:read score] [:literal nil]])
(def ^:private nonnil-flag [:not= [:read flag] [:literal nil]])
(def ^:private claimed [:= [:read status] [:literal :request/claimed]])
(def ^:private positive [:> [:read score] [:literal 7]])
(def ^:private claimed-or-done
  [:in [:read status] [:literal #{:request/claimed :request/done}]])

(defn- declaration [expression types]
  {:subject subject :variables types
   :proposition [:atom {:readers (set (keys types))
                        :expression expression}]})

(defn- failure [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

;; Test-only denotation. It never calls rule-eval, typed-presence-semantics,
;; rule-type, typed-atom-oracle, or a production formula interpretation.
;; The visitor also permits exact checks of executed short-circuit paths.
(defn- local-value [expression state]
  (let [visited (atom [])]
    (letfn [(walk [expr path]
              (swap! visited conj path)
              (let [[op a b] expr]
                (case op
                  :literal a
                  :read (if (contains? state a)
                          (get state a)
                          (throw (ex-info "Executed absent reader"
                                          {:reader a :path path})))
                  :present (contains? state (second a))
                  :not (not (walk a (conj path 1)))
                  :and (loop [i 1]
                         (cond (= i (count expr)) true
                               (walk (nth expr i) (conj path i)) (recur (inc i))
                               :else false))
                  :or (loop [i 1]
                        (cond (= i (count expr)) false
                              (walk (nth expr i) (conj path i)) true
                              :else (recur (inc i))))
                  :implies (if (walk a (conj path 1))
                             (walk b (conj path 2)) true)
                  := (= (walk a (conj path 1)) (walk b (conj path 2)))
                  :not= (not= (walk a (conj path 1)) (walk b (conj path 2)))
                  :> (> (walk a (conj path 1)) (walk b (conj path 2)))
                  :>= (>= (walk a (conj path 1)) (walk b (conj path 2)))
                  :< (< (walk a (conj path 1)) (walk b (conj path 2)))
                  :<= (<= (walk a (conj path 1)) (walk b (conj path 2)))
                  :in (contains? (walk b (conj path 2))
                                 (walk a (conj path 1)))
                  (throw (ex-info "Outside v1428 test language"
                                  {:operator op})))))]
      {:value (walk expression [:expression])
       :visited @visited})))

(defn- correspondence! [expression types states]
  (let [source (declaration expression types)
        normalized (proposition/describe-proposition! source)
        canonical (get-in normalized [:semantics :proposition 1])
        audited (audit/audit-proposition! source)
        canonical-expression (:expression canonical)]
    (is (= :preserved-for-all-opaque-valuations (:status audited)))
    (is (= :not-established (:typed-atomic-denotational-equivalence audited)))
    (is (= :not-issued (:certificate audited)))
    (is (= :none (:authority audited)))
    (is (= (set (keys types)) (:readers canonical)))
    (doseq [state states]
      (let [local (local-value expression state)
            normalized-local (local-value canonical-expression state)
            checked (independent/evaluate-expression! expression types state)
            actual (production/evaluate-rule!
                    {:readers (set (keys types)) :expression expression}
                    types state)
            context (pr-str [expression types state])]
        (is (= (:value local) (:value normalized-local)) context)
        (is (= (:value local) (:value checked)) context)
        (is (= (:value local) (:value actual)) context)
        (is (= (:visited local) (:evaluated-paths checked)) context)
        (is (instance? Boolean (:value actual)) context)
        (is (= :not-proved (:proof-status checked)))
        (is (= :not-admitted (:admission checked)))
        (is (= :none (:authority checked)))))))

(deftest v1428-nominal-enum-membership-and-equality-across-finite-domain
  (let [states [{} {status :request/new} {status :request/claimed}
                {status :request/done}]
        expressions [[:and pstatus claimed]
                     [:and pstatus claimed-or-done]
                     [:or [:not pstatus] claimed]
                     [:implies pstatus claimed]
                     [:and pstatus [:not claimed]]
                     [:or [:not pstatus] [:not claimed-or-done]]]]
    (doseq [expr expressions]
      (correspondence! expr status-types states))))

(deftest v1428-optional-nullable-integer-and-nonnil-short-circuit
  (let [expr [:and pscore nonnil-score positive]
        states [{} {score nil} {score -1} {score 7} {score 8}
                {score 999999999999999999999999999N}]]
    (correspondence! expr nullable-types states)
    (is (= [[:expression] [:expression 1]]
           (:visited (local-value expr {}))))
    (is (not-any? #{[:expression 3]}
                  (:visited (local-value expr {score nil}))))
    (is (some #{[:expression 3]}
              (:visited (local-value expr {score 8}))))))

(deftest v1428-presence-is-not-nonnull-and-present-false-is-not-absence
  (let [states [{} {flag nil} {flag false} {flag true}]
        expressions [[:and pflag nonnil-flag [:read flag]]
                     [:or [:not pflag] [:and nonnil-flag [:read flag]]]
                     [:implies [:and pflag nonnil-flag] [:read flag]]]]
    (doseq [expr expressions]
      (correspondence! expr optional-boolean-types states))
    (is (= false (:value (local-value pflag {}))))
    (is (= true (:value (local-value pflag {flag nil}))))
    (is (= true (:value (local-value pflag {flag false}))))
    (is (= :present (:presence
                    (values/observe-field! optional-boolean-types
                                           {flag false} flag))))))

(deftest v1428-nominal-enum-identity-is-not-merely-keyword-set-equality
  (let [expr [:and pstatus claimed]
        a (proposition/describe-proposition! (declaration expr status-types))
        b (proposition/describe-proposition! (declaration expr other-nominal-types))
        c (proposition/describe-proposition! (declaration expr narrower-types))]
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (not= (:fingerprint a) (:fingerprint c)))
    (is (not= (get-in a [:semantics :proposition])
              (get-in b [:semantics :proposition])))
    (is (not= (get-in a [:semantics :proposition])
              (get-in c [:semantics :proposition])))
    (is (= :preserved-for-all-opaque-valuations
           (:status (audit/audit-proposition! (declaration expr status-types)))))))

(deftest v1428-enum-domain-violations-and-invalid-present-values-reject
  (doseq [state [{status :request/other} {status nil} {status false}
                 {status "request/new"} {status 7}]]
    (is (some? (failure #(independent/evaluate-expression!
                          [:and pstatus claimed] status-types state))))
    (is (some? (failure #(production/evaluate-rule!
                          {:readers #{status} :expression [:and pstatus claimed]}
                          status-types state)))))
  (is (some? (failure #(proposition/describe-proposition!
                        (declaration [:and pstatus
                                      [:= [:read status] [:literal :request/other]]]
                                     status-types)))))
  (is (some? (failure #(proposition/describe-proposition!
                        (declaration [:and pstatus
                                      [:in [:read status]
                                       [:literal #{:request/claimed :request/other}]]]
                                     status-types))))))

(deftest v1428-optional-absent-and-nullable-present-are-distinct
  (is (= :absent (:presence
                  (values/observe-field! nullable-types {} score))))
  (is (= :present (:presence
                   (values/observe-field! nullable-types {score nil} score))))
  (is (= :nil (:kind
               (values/observe-field! nullable-types {score nil} score))))
  (is (some? (failure #(values/observe-state!
                        {score {:type :integer :optional? true}}
                        {score nil}))))
  (is (some? (failure #(values/observe-state!
                        {score {:type :integer :nullable? true}}
                        {})))))

(deftest v1428-static-typing-rejects-unsafe-dead-branches
  (let [unsafe [[:and [:literal false] positive]
                [:or [:literal true] positive]
                [:implies [:literal false] positive]
                [:and positive pscore]
                [:and pscore positive]]]
    (doseq [expr unsafe]
      (let [ind (failure #(independent/evaluate-expression!
                           expr nullable-types {}))
            prod (failure #(production/evaluate-rule!
                            {:readers #{score} :expression expr}
                            nullable-types {}))]
        (is (map? ind) (pr-str expr))
        (is (map? prod) (pr-str expr))
        (is (some? (:reason ind)) (pr-str ind))
        (is (some? (:reason prod)) (pr-str prod))))))

(deftest v1428-static-typing-rejects-invalid-nominal-dead-branches
  (let [expr [:or [:literal true]
              [:and pstatus [:= [:read status] [:literal :request/other]]]]]
    (is (some? (failure #(independent/evaluate-expression!
                          expr status-types {}))))
    (is (some? (failure #(proposition/describe-proposition!
                          (declaration expr status-types)))))))

(deftest v1428-proposition-connectives-retain-distinct-atomic-boundaries
  (let [a [:atom {:readers #{status}
                  :expression [:and pstatus claimed]}]
        b [:atom {:readers #{flag}
                  :expression [:and pflag nonnil-flag [:read flag]]}]
        source {:subject subject
                :variables (merge status-types optional-boolean-types)
                :proposition [:implies a [:or b [:not a]]]}
        normalized (proposition/describe-proposition! source)
        audited (audit/audit-proposition! source)]
    (is (= :preserved-for-all-opaque-valuations (:status audited)))
    (is (= 3 (:atomic-occurrences audited)))
    (is (= 2 (:distinct-source-atoms audited)))
    (is (= :implies (get-in normalized [:semantics :proposition 0])))
    (is (= :none (:authority audited)))
    (is (= :not-established (:typed-atomic-denotational-equivalence audited)))))

(deftest v1428-canonical-typed-atom-mutations-fail-source-relative-audit
  (let [source (declaration [:and pstatus claimed] status-types)
        original proposition/describe-proposition!
        changes [[#(assoc-in % [:semantics :proposition 1 :reader-types status :name]
                             :organization/status)
                  :atomic-canonicalization-mismatch]
                 [#(assoc-in % [:semantics :proposition 1 :reader-types status :values]
                             #{:request/claimed})
                  :atomic-canonicalization-mismatch]
                 [#(assoc-in % [:semantics :proposition 1 :reader-types status :optional?]
                             false)
                  :atomic-canonicalization-mismatch]
                 [#(assoc-in % [:semantics :proposition 1 :expression 2 2 1]
                             :request/new)
                  :atomic-canonicalization-mismatch]]]
    (doseq [[mutate expected] changes]
      (let [got (failure #(with-redefs [proposition/describe-proposition!
                                       (fn [s] (mutate (original s)))]
                             (audit/audit-proposition! source)))]
        (is (= expected (:reason got)) (pr-str got))))))

(deftest v1428-no-privilege-or-certificate-from-finite-correspondence
  (let [expr [:and pscore nonnil-score positive]
        checked (independent/evaluate-expression!
                 expr nullable-types {score 8})
        normalized (proposition/describe-proposition!
                    (declaration expr nullable-types))
        audited (audit/audit-proposition!
                 (declaration expr nullable-types))]
    (doseq [report [checked normalized audited]]
      (is (not (contains? report :transaction-handle)))
      (is (not (contains? report :runtime-authorization)))
      (is (not= :issued (:certificate report)))
      (is (not= :admitted (:admission report)))
      (is (not= :authorized (:authority report))))))
