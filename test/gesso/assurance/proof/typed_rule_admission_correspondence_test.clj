(ns gesso.assurance.proof.typed-rule-admission-correspondence-test
  "v1347: permanent adversarial tests for the source-relative S1 admission
   audit introduced in v1346. Exercise both actual production admission and
   the independent v1344 branch-sensitive interpreter, while deliberately
   simulating compromised reports and contradictory admissions.

   All outcomes are diagnostics. Three synthesized states are witnesses, not
   an exhaustive proof, certificate, authenticated XTDB snapshot, or evidence
   of JVM/CLJS equivalence. The independent 64-reader limit must never be
   silently promoted into an admission guarantee."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.proof.typed-rule-admission-correspondence :as audit]
            [gesso.assurance.proof.typed-presence-semantics :as independent]
            [gesso.assurance.rule-type :as production]))

(def ^:private score :request/score)
(def ^:private flag :request/flag)
(def ^:private status :request/status)
(def ^:private integer {:type :integer})
(def ^:private optional-integer {:type :integer :optional? true})
(def ^:private nullable-integer {:type :integer :nullable? true})
(def ^:private optional-nullable-integer
  {:type :integer :optional? true :nullable? true})
(def ^:private optional-boolean {:type :boolean :optional? true})
(def ^:private optional-nullable-boolean
  {:type :boolean :optional? true :nullable? true})
(def ^:private workflow-enum
  {:type :enum :name :workflow/status
   :values #{:workflow/open :workflow/done}})

(def ^:private score-present [:present [:read score]])
(def ^:private score-nonnil [:not= [:read score] [:literal nil]])
(def ^:private positive-score [:> [:read score] [:literal 0]])
(def ^:private guarded-score
  [:and score-present score-nonnil positive-score])

(defn- source
  [expression reader-types]
  {:readers (set (keys reader-types)) :expression expression})

(defn- run-audit
  [expression reader-types]
  (audit/audit-rule! (source expression reader-types) reader-types))

(defn- error-data
  [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo exception
         (ex-data exception))))

(defn- expected-witness-values
  [report values]
  (is (= [:all-present :optional-absent :nullable-nil]
         (mapv :witness (:witnesses report))))
  (is (= [:evaluated :evaluated :evaluated]
         (mapv :outcome (:witnesses report))))
  (is (= values (mapv :value (:witnesses report))))
  (is (every? #(instance? Boolean (:value %)) (:witnesses report))))

(defn- assert-source-bound-diagnostic
  [report declaration reader-types]
  (is (= audit/audit-type (:type report)))
  (is (= audit/audit-version (:version report)))
  (is (= declaration (:source-declaration report)))
  (is (= reader-types (:source-reader-types report)))
  (is (= :production-admission-vs-independent-typed-semantics
         (:checking report)))
  (is (= :three-synthetic-typed-states (:scope report)))
  (is (= :not-proved (:proof-status report)))
  (is (= :not-admitted (:admission report)))
  (is (= :not-established (:runtime-enforcement report)))
  (is (= :not-established (:snapshot-provenance report)))
  (is (= :not-established (:jvm-cljs-equivalence report)))
  (is (= :none (:authority report)))
  (is (not (contains? report :certificate)))
  report)

(defn- assert-supported!
  [expression reader-types expected-values]
  (let [declaration (source expression reader-types)
        r (audit/audit-rule! declaration reader-types)]
    (assert-source-bound-diagnostic r declaration reader-types)
    (is (= :witness-supported (:status r)))
    (is (= :accepted (:production r)))
    (is (= :evaluated (:independent r)))
    (expected-witness-values r expected-values)
    r))

(defn- assert-production-rejected!
  [declaration reader-types]
  (let [r (audit/audit-rule! declaration reader-types)]
    (assert-source-bound-diagnostic r declaration reader-types)
    (is (= :production-rejected (:status r)))
    (is (= :not-executed (:independent r)))
    (is (keyword? (:production-reason r)))
    (is (not (contains? r :witnesses)))
    r))

(defn- reader-catalog
  [n]
  (let [ids (mapv #(keyword "probe" (str "reader-" %)) (range n))
        clauses (mapv (fn [id] [:present [:read id]]) ids)]
    {:readers (into {} (map (fn [id] [id optional-boolean])) ids)
     ;; Keep each expression below the independent per-node arity limit,
     ;; including the 257-reader adversarial case.
     :expression (if (> n 255)
                   [:and (into [:and] (take 129 clauses))
                    (into [:and] (drop 129 clauses))]
                   (into [:and] clauses))}))

(deftest direct-integer-admission-is-bounded-diagnostic
  (let [r (assert-supported! positive-score {score integer}
                             [false false false])]
    (is (= [{score 0N} {score 0N} {score 0N}]
           (mapv :state (:witnesses r))))))

(deftest literal-only-formulas-have-an-empty-reader-footprint
  (let [r (assert-supported! [:= [:literal 14] [:literal 14]] {}
                             [true true true])]
    (is (= [{} {} {}] (mapv :state (:witnesses r))))))

(deftest literal-inequality-and-negation-remain-boolean
  (assert-supported! [:not [:= [:literal false] [:literal true]]]
                     {} [true true true])
  (assert-supported! [:not= [:literal -8N] [:literal 8N]]
                     {} [true true true]))

(deftest guarded-optional-nullable-reads-vary-by-synthetic-state
  (let [r (assert-supported! guarded-score {score optional-nullable-integer}
                             [false false false])]
    (is (= [{score 0N} {} {score nil}]
           (mapv :state (:witnesses r))))))

(deftest optional-false-is-present-not-missing
  (let [r (assert-supported!
           [:present [:read flag]] {flag optional-boolean}
           [true false true])]
    (is (= [{flag false} {} {flag false}]
           (mapv :state (:witnesses r))))))

(deftest nullable-false-versus-nil-are-distinct
  (let [r (assert-supported!
           [:= [:read flag] [:literal nil]]
           {flag {:type :boolean :nullable? true}}
           [false false true])]
    (is (= [{flag false} {flag false} {flag nil}]
           (mapv :state (:witnesses r))))))

(deftest optional-nullable-presence-has-three-distinct-paths
  (let [r (assert-supported!
           [:present [:read flag]] {flag optional-nullable-boolean}
           [true false true])]
    (is (= [{flag false} {} {flag nil}]
           (mapv :state (:witnesses r))))))

(deftest boolean-optional-read-needs-guard
  (assert-supported!
   [:and [:present [:read flag]] [:read flag]]
   {flag optional-boolean} [false false false]))

(deftest nested-or-requires-false-branch-refinement
  (let [expression [:or [:not score-present]
                    [:and score-nonnil [:>= [:read score] [:literal 0]]]]]
    (assert-supported! expression {score optional-nullable-integer}
                       [true true false])))

(deftest implication-antecedent-refines-optional-nullable-read
  (let [expression [:implies [:and score-present score-nonnil]
                    [:>= [:read score] [:literal 0]]]]
    (assert-supported! expression {score optional-nullable-integer}
                       [true true true])))

(deftest nominal-enum-membership-preserves-declared-source
  (let [expr [:in [:read status] [:literal #{:workflow/done}]]
        r (assert-supported! expr {status workflow-enum} [true true true])]
    (is (= [{status :workflow/done}
            {status :workflow/done}
            {status :workflow/done}]
           (mapv :state (:witnesses r))))))

(deftest source-changes-are-reflected-in-the-report
  (let [d {score integer}
        r1 (run-audit [:> [:read score] [:literal 0]] d)
        r2 (run-audit [:>= [:read score] [:literal 0]] d)]
    (is (not= (:source-declaration r1) (:source-declaration r2)))
    (is (= :witness-supported (:status r1) (:status r2)))
    (is (= [false false false] (mapv :value (:witnesses r1))))
    (is (= [true true true] (mapv :value (:witnesses r2))))))

(deftest an-unguarded-optional-reader-is-rejected-by-production
  (assert-production-rejected! (source positive-score {score optional-integer})
                               {score optional-integer}))

(deftest nullable-ordering-without-nonnil-guard-is-rejected
  (assert-production-rejected! (source positive-score {score nullable-integer})
                               {score nullable-integer}))

(deftest reversed-presence-and-nonnil-guard-is-rejected
  (assert-production-rejected!
   (source [:and positive-score score-nonnil score-present]
           {score optional-nullable-integer})
   {score optional-nullable-integer}))

(deftest enum-value-outside-nominal-domain-is-rejected
  (assert-production-rejected!
   (source [:= [:read status] [:literal :other/open]]
           {status workflow-enum})
   {status workflow-enum}))

(deftest invalid-expression-operator-is-rejected
  (assert-production-rejected!
   (source [:xor [:literal true] [:literal false]] {}) {}))

(deftest reader-type-footprint-differences-are-not-silently-accepted
  (assert-production-rejected!
   (source positive-score {score integer})
   {score integer flag {:type :boolean}})
  (assert-production-rejected!
   (source positive-score {score integer flag {:type :boolean}})
   {score integer flag {:type :boolean}})
  (assert-production-rejected!
   {:readers #{} :expression positive-score} {score integer}))

(deftest malformed-declarations-and-descriptors-are-rejected
  (doseq [[decl types]
          [[{:readers #{score} :expression positive-score :certificate :forged}
            {score integer}]
           [(source positive-score {score integer})
            {score {:type :integer :optional? :sometimes}}]
           [(source positive-score {score integer})
            {score {:type :integer :unknown true}}]
           [(source positive-score {score integer})
            {score {:type :unknown}}]
           [(source positive-score {score integer})
            {score {:type :integer :nullable? :not-a-boolean}}]]]
    (assert-production-rejected! decl types)))

(deftest untrusted-metadata-in-source-is-rejected
  (assert-production-rejected!
   (source (with-meta positive-score {:admitted true}) {score integer})
   {score integer})
  (assert-production-rejected!
   (source positive-score {score integer})
   {score (with-meta {:type :integer} {:allowed true})}))

(deftest executable-payloads-are-not-source-language
  (assert-production-rejected!
   (source [:= [:literal (fn [] true)] [:literal true]] {}) {}))

(deftest a-production-rejection-never-runs-independent-evaluation
  (with-redefs [independent/evaluate-expression!
                (fn [& _] (throw (ex-info "must not run" {:unexpected true})))]
    (let [r (run-audit positive-score {score optional-integer})]
      (is (= :production-rejected (:status r)))
      (is (= :not-executed (:independent r))))))

(deftest independent-admission-disagreement-is-not-a-success
  ;; Deliberately compromised production type checker: a source rejected by
  ;; the actual implementation is presented as accepted. The independent
  ;; interpreter must still refuse the unsafe optional read.
  (let [types {score optional-integer}]
    (with-redefs [production/check-rule-type!
                  (fn [declaration reader-types]
                    {:expression (:expression declaration)
                     :readers (:readers declaration)
                     :reader-types reader-types
                     :admission :not-admitted
                     :authority :none})]
      (let [r (run-audit positive-score types)]
        (is (= :disagreement (:status r)))
        (is (= :accepted (:production r)))
        (is (= :rejected (:independent r)))
        (is (= :independent-rejected (:reason r)))
        (is (= [:rejected :rejected :rejected]
               (mapv :outcome (:witnesses r))))
        (is (every? #(= :unguarded-optional-reader (:reason %))
                    (:witnesses r)))
        (is (= :not-proved (:proof-status r)))
        (is (= :none (:authority r)))))))

(deftest independent-refusal-for-admitted-source-becomes-disagreement
  (with-redefs [independent/evaluate-expression!
                (fn [& _]
                  (throw (ex-info "simulated independent refusal"
                                  {:typed-presence-semantics/rejection true
                                   :reason :simulated-independent-refusal
                                   :at [:expression]})))]
    (let [r (run-audit positive-score {score integer})]
      (is (= :disagreement (:status r)))
      (is (= :independent-rejected (:reason r)))
      (is (every? #(= :simulated-independent-refusal (:reason %))
                  (:witnesses r))))))

(deftest independent-value-domain-refusal-also-becomes-disagreement
  (with-redefs [independent/evaluate-expression!
                (fn [& _]
                  (throw (ex-info "simulated value domain refusal"
                                  {:typed-value-semantics/rejection true
                                   :reason :simulated-domain-refusal
                                   :at [:state]})))]
    (let [r (run-audit positive-score {score integer})]
      (is (= :disagreement (:status r)))
      (is (= [:rejected :rejected :rejected]
             (mapv :outcome (:witnesses r)))))))

(deftest exact-independent-reader-capacity-is-supported
  (let [{:keys [readers expression]} (reader-catalog 64)
        r (run-audit expression readers)]
    (is (= :witness-supported (:status r)))
    (is (= 64 (count (:readers (source expression readers)))))
    (is (= [true false true] (mapv :value (:witnesses r))))
    (is (= 64 (count (:state (first (:witnesses r))))))
    (is (= 0 (count (:state (second (:witnesses r))))))))

(deftest over-independent-reader-capacity-is-explicitly-not-checked
  (let [{:keys [readers expression]} (reader-catalog 65)]
    (with-redefs [independent/evaluate-expression!
                  (fn [& _] (throw (ex-info "over budget must not execute"
                                          {:unexpected true})))]
      (let [r (run-audit expression readers)]
        (assert-source-bound-diagnostic r (source expression readers) readers)
        (is (= :not-checked (:status r)))
        (is (= :independent-reader-limit (:reason r)))
        (is (= 64 (:maximum r)))
        (is (= 65 (:actual r)))
        (is (= :accepted (:production r)))
        (is (= :not-executed (:independent r)))
        (is (not (contains? r :witnesses)))))))

(deftest over-production-reader-capacity-remains-production-rejection
  (let [{:keys [readers expression]} (reader-catalog 257)
        r (run-audit expression readers)]
    (is (= :production-rejected (:status r)))
    (is (= :reader-limit (:production-reason r)))
    (is (= :not-executed (:independent r)))))

(deftest unexpected-production-exception-is-not-mislabeled-rejection
  (with-redefs [production/check-rule-type!
                (fn [& _] (throw (ex-info "unexpected production failure"
                                          {:error/type :unrelated/failure
                                           :reason :infrastructure-failed})))]
    (is (= :infrastructure-failed
           (:reason (error-data #(run-audit positive-score {score integer})))))))

(deftest unexpected-independent-exception-is-not-mislabeled-disagreement
  (with-redefs [independent/evaluate-expression!
                (fn [& _] (throw (ex-info "unexpected independent failure"
                                          {:reason :evaluator-broken})))]
    (is (= :evaluator-broken
           (:reason (error-data #(run-audit positive-score {score integer})))))))

(deftest forged-production-source-is-rejected
  (let [actual production/check-rule-type!]
    (doseq [corrupt [(fn [r] (assoc r :expression [:literal true]))
                     (fn [r] (assoc r :readers #{}))
                     (fn [r] (assoc r :reader-types {}))
                     (fn [r] (assoc r :authority :committed))
                     (fn [r] (assoc r :admission :admitted))]]
      (with-redefs [production/check-rule-type!
                    (fn [decl types] (corrupt (actual decl types)))]
        (is (= :gesso.assurance.proof.typed-rule-admission-correspondence/production-source-mismatch
               (:error/type
                (error-data #(run-audit positive-score {score integer})))))))))

(deftest forged-independent-semantic-report-is-rejected
  (let [actual independent/evaluate-expression!]
    (doseq [corrupt [(fn [r] (assoc r :type :forged/type))
                     (fn [r] (assoc r :version -9))
                     (fn [r] (assoc r :expression [:literal false]))
                     (fn [r] (assoc r :source-readers #{}))
                     (fn [r] (assoc r :value :not-boolean))
                     (fn [r] (assoc r :proof-status :proved))
                     (fn [r] (assoc r :admission :admitted))
                     (fn [r] (assoc r :authority :committed))]]
      (with-redefs [independent/evaluate-expression!
                    (fn [expr types state]
                      (corrupt (actual expr types state)))]
        (is (= :gesso.assurance.proof.typed-rule-admission-correspondence/independent-source-mismatch
               (:error/type
                (error-data #(run-audit positive-score {score integer})))))))))

(deftest copied-audit-reports-have-no-execution-authority
  (let [r (run-audit guarded-score {score optional-nullable-integer})
        copy (edn/read-string (pr-str r))]
    (is (= r copy))
    (is (= :not-proved (:proof-status copy)))
    (is (= :not-admitted (:admission copy)))
    (is (= :none (:authority copy)))
    (is (not (contains? copy :committed-transaction)))
    (is (not (contains? copy :certificate)))))
