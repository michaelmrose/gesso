(ns gesso.assurance.proof.kernel-normalization-semantic-preservation-test
  "v1427: independent bounded semantic regression at the closed proposition
   normalization boundary. Interpret a deliberately small subset of source and
   canonical typed expressions with a TEST-LOCAL evaluator, distinguish
   nearly identical formulas by concrete witness states, and inject faults
   into the normalizer's returned data to ensure that the source-relative
   auditor rejects changed operators, operands, types, and identity.

   Finite examples do not prove typed-atom equivalence over all states,
   universal normalizer correctness, kernel metatheory, or certification."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.proposition :as proposition]
            [gesso.assurance.proof.propositional-normalization-audit :as audit]))

(def ^:private subject
  {:kind :operation :id :example/normalization-witness
   :interface {:operation :example/normalization-witness}})

(def ^:private integer-type {:type :integer})
(def ^:private optional-type {:type :integer :optional? true :nullable? true})
(def ^:private score :result/score)
(def ^:private other :result/other)
(def ^:private optional :result/optional)

(defn- numeric-atom [op reader threshold]
  [:atom {:readers #{reader}
          :expression [op [:read reader] [:literal threshold]]}])

(def ^:private gt7 (numeric-atom :> score 7))
(def ^:private ge7 (numeric-atom :>= score 7))
(def ^:private ge8 (numeric-atom :>= score 8))
(def ^:private eq7 (numeric-atom := score 7))
(def ^:private ne7 (numeric-atom :not= score 7))
(def ^:private other-gt7 (numeric-atom :> other 7))
(def ^:private present
  [:atom {:readers #{optional}
          :expression [:present [:read optional]]}])
(def ^:private absent
  [:atom {:readers #{optional}
          :expression [:not [:present [:read optional]]]}])

(def ^:private types
  {score integer-type other integer-type optional optional-type})

(defn- readers-in [form]
  (case (first form)
    :atom (:readers (second form))
    (:true :false) #{}
    (reduce into #{} (map readers-in (rest form)))))

(defn- declaration [form]
  {:subject subject
   :variables (select-keys types (readers-in form))
   :proposition form})

;; Intentionally independent of rule-eval, propositional-semantics, and the
;; production normalization audit. The fixtures use only these finite ops.
(defn- expression-value [expr bindings]
  (case (first expr)
    :literal (second expr)
    :read (get bindings (second expr))
    :present (contains? bindings (second (second expr)))
    :not (not (expression-value (second expr) bindings))
    :> (> (expression-value (second expr) bindings)
          (expression-value (nth expr 2) bindings))
    :>= (>= (expression-value (second expr) bindings)
            (expression-value (nth expr 2) bindings))
    := (= (expression-value (second expr) bindings)
          (expression-value (nth expr 2) bindings))
    :not= (not= (expression-value (second expr) bindings)
              (expression-value (nth expr 2) bindings))
    (throw (ex-info "Unsupported test expression" {:operator (first expr)}))))

(defn- formula-value [form bindings]
  (case (first form)
    :true true
    :false false
    :atom (boolean (expression-value (:expression (second form)) bindings))
    :not (not (formula-value (second form) bindings))
    :and (every? #(formula-value % bindings) (rest form))
    :or (boolean (some #(formula-value % bindings) (rest form)))
    :implies (or (not (formula-value (second form) bindings))
                 (formula-value (nth form 2) bindings))
    (throw (ex-info "Unsupported test formula" {:operator (first form)}))))

(defn- normalization [form]
  (proposition/describe-proposition! (declaration form)))

(defn- assert-bounded-preservation! [form bindings]
  (let [bindings (select-keys bindings (readers-in form))
        src (declaration form)
        produced (normalization form)
        canonical (get-in produced [:semantics :proposition])
        report (audit/audit-proposition! src)]
    (is (= (formula-value form bindings)
           (formula-value canonical bindings)))
    (is (= :preserved-for-all-opaque-valuations (:status report)))
    (is (= :not-established (:typed-atomic-denotational-equivalence report)))
    (is (= :not-established (:universal-normalizer-implementation-soundness report)))
    (is (= :not-issued (:certificate report)))
    (is (= :not-admitted (:admission report)))
    (is (= :none (:authority report)))))

(defn- injected-reason [form mutator]
  (let [orig proposition/describe-proposition!]
    (try
      (with-redefs [proposition/describe-proposition!
                    (fn [source]
                      (mutator (orig source)))]
        (audit/audit-proposition! (declaration form)))
      nil
      (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))

(deftest v1427-source-and-normalized-typed-truth-on-finite-states
  (let [numerical-forms [gt7 ge7 ge8 eq7 ne7 other-gt7
                         [:and gt7 ge7] [:or gt7 ge7]
                         [:implies gt7 ge7] [:implies ge7 gt7]
                         [:not gt7]
                         [:and [:or gt7 other-gt7] [:not eq7]]]
        states (for [a [6 7 8 9] b [6 7 8 9]]
                 {score a other b})]
    (doseq [form numerical-forms
            state states]
      (assert-bounded-preservation! form state))
    (doseq [form [present absent [:implies present [:not absent]]]
            state [{} {optional nil} {optional 0} {optional 7}]]
      (assert-bounded-preservation! form state))))

(deftest v1427-similar-source-shape-can-have-different-meaning
  (doseq [[left right state expected-left expected-right]
          [[gt7 ge7 {score 7} false true]
           [ge7 ge8 {score 7} true false]
           [eq7 ne7 {score 7} true false]
           [gt7 other-gt7 {score 8 other 6} true false]
           [[:and gt7 ge7] [:or gt7 ge7] {score 7} false true]
           [[:implies gt7 ge7] [:implies ge7 gt7] {score 7} true false]
           [[:not gt7] gt7 {score 7} true false]
           [present absent {} false true]]]
    (is (= expected-left (formula-value left state)))
    (is (= expected-right (formula-value right state)))
    (is (not= (get-in (normalization left) [:semantics :proposition])
              (get-in (normalization right) [:semantics :proposition])))
    (is (not= (:fingerprint (normalization left))
              (:fingerprint (normalization right))))))

(deftest v1427-allowed-integer-presentation-canonicalization
  (let [left (normalization gt7)
        right (normalization (numeric-atom :> score 7N))]
    (is (= (get-in left [:semantics :proposition])
           (get-in right [:semantics :proposition])))
    (is (= (:fingerprint left) (:fingerprint right)))
    (doseq [n [6 7 8 9]]
      (is (= (formula-value gt7 {score n})
             (formula-value (numeric-atom :> score 7N) {score n}))))))

(deftest v1427-default-false-type-flags-are-canonicalized
  (let [src (declaration gt7)
        explicit (assoc-in src [:variables score]
                           {:type :integer :optional? false :nullable? false})
        left (proposition/describe-proposition! src)
        right (proposition/describe-proposition! explicit)]
    (is (= (:fingerprint left) (:fingerprint right)))
    (is (= (get-in left [:semantics :proposition])
           (get-in right [:semantics :proposition])))
    (is (= :preserved-for-all-opaque-valuations
           (:status (audit/audit-proposition! explicit))))))

(deftest v1427-normalized-operator-mutations-are-rejected
  (doseq [[form fault expected]
          [[gt7 #(assoc-in % [:semantics :proposition 1 :expression 0] :>=)
            :atomic-canonicalization-mismatch]
           [ge7 #(assoc-in % [:semantics :proposition 1 :expression 0] :>)
            :atomic-canonicalization-mismatch]
           [gt7 #(assoc-in % [:semantics :proposition 1 :expression 2 1] 8N)
            :atomic-canonicalization-mismatch]
           [gt7 #(assoc-in % [:semantics :proposition 1 :expression 1 1] other)
            :atomic-canonicalization-mismatch]
           [[:and gt7 ge7] #(assoc-in % [:semantics :proposition 0] :or)
            :connective-structure-mismatch]
           [[:implies gt7 ge7] #(update-in % [:semantics :proposition]
                                            (fn [p] [:implies (nth p 2) (nth p 1)]))
            :atomic-canonicalization-mismatch]
           [[:and gt7 ge7] #(update-in % [:semantics :proposition] pop)
            :connective-structure-mismatch]]]
    (is (= expected (injected-reason form fault)))))

(deftest v1427-canonical-atom-identity-mutations-are-rejected
  (doseq [[fault expected]
          [[#(assoc-in % [:semantics :proposition 1 :reader-types score :optional?] true)
            :atomic-canonicalization-mismatch]
           [#(assoc-in % [:semantics :proposition 1 :reader-types score :type] :string)
            :atomic-canonicalization-mismatch]
           [#(assoc-in % [:semantics :proposition 1 :readers] #{other})
            :atomic-canonicalization-mismatch]
           [#(assoc-in % [:semantics :proposition 1 :evaluator-version] 999)
            :atomic-canonicalization-mismatch]
           [#(assoc-in % [:semantics :proposition 1 :normalization-version] 999)
            :atomic-canonicalization-mismatch]
           [#(update-in % [:semantics :proposition 1] dissoc :syntax-version)
            :atomic-canonicalization-mismatch]
           [#(assoc-in % [:semantics :proposition 1 :extra-authority] true)
            :atomic-canonicalization-mismatch]]]
    (is (= expected (injected-reason gt7 fault)))))

(deftest v1427-repeated-atoms-retain-one-opaque-identity
  (let [form [:or gt7 [:not gt7] gt7]
        report (audit/audit-proposition! (declaration form))]
    (is (= 3 (:atomic-occurrences report)))
    (is (= 1 (:distinct-source-atoms report)))
    (is (= 2 (:valuations-checked report)))
    (is (= :preserved-for-all-opaque-valuations (:status report)))))

(deftest v1427-syntactically-different-truth-equivalences-stay-distinct
  ;; Conservative identity is not a quotient by Boolean equivalence.
  (let [p gt7
        pairs [[p [:not [:not p]]]
               [[:and p [:true]] p]
               [[:or p [:false]] p]]]
    (doseq [[left right] pairs]
      (is (not= (:fingerprint (normalization left))
                (:fingerprint (normalization right))))
      (doseq [n [6 7 8]]
        (is (= (formula-value left {score n})
               (formula-value right {score n})))))))
