(ns gesso.assurance.proof.kernel-typed-atom-normalization-mutation-test
  "v1431: adversarial source-relative normalization checks for *nested* typed
   atoms, including logically dead proposition branches and repeated atoms.

   Every mutation is applied to a real normalized result while the auditor
   independently reconstructs the original source's canonical structure.
   This is distinct from v1430's mutations of the production value evaluator.
   A bounded pass is NOT universal normalizer soundness, atom denotational
   equivalence, proof authority, or permission to execute a transaction."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.proposition :as proposition]
            [gesso.assurance.proof.propositional-normalization-audit :as audit]
            [gesso.assurance.proof.typed-presence-semantics :as independent]
            [gesso.assurance.rule-eval :as production]))

(def ^:private status :request/status)
(def ^:private score :request/score)
(def ^:private flag :request/flag)
(def ^:private status-type
  {:type :enum :name :request/status
   :values #{:request/new :request/claimed :request/done}
   :optional? true})
(def ^:private score-type {:type :integer :optional? true :nullable? true})
(def ^:private flag-type {:type :boolean :optional? true :nullable? true})
(def ^:private types {status status-type score score-type flag flag-type})
(def ^:private subject
  {:kind :operation :id :example/normalization-adversarial
   :interface {:operation :example/normalization-adversarial}})

(def ^:private status-expression
  [:and [:present [:read status]]
   [:= [:read status] [:literal :request/claimed]]])
(def ^:private score-expression
  [:and [:present [:read score]]
   [:not= [:read score] [:literal nil]]
   [:> [:read score] [:literal 7]]])
(def ^:private flag-expression
  [:and [:present [:read flag]]
   [:not= [:read flag] [:literal nil]]
   [:read flag]])

(defn- atom-form [reader expression]
  [:atom {:readers #{reader} :expression expression}])

(def ^:private status-atom (atom-form status status-expression))
(def ^:private score-atom (atom-form score score-expression))
(def ^:private flag-atom (atom-form flag flag-expression))

;; :or [:true] and :implies [:false] have semantically dead children. The
;; original typed atoms are nevertheless independently validated and audited.
;; The status atom occurs twice, but must have one consistent canonical ID.
(def ^:private formula
  [:and
   [:or [:true] status-atom]
   [:implies [:false] score-atom]
   [:and flag-atom status-atom]])

(defn- source [form declared-types]
  {:subject subject :variables declared-types :proposition form})

(def ^:private declaration (source formula types))
(def ^:private dead-status-path [:semantics :proposition 1 2 1])
(def ^:private dead-score-path [:semantics :proposition 2 2 1])
(def ^:private live-flag-path [:semantics :proposition 3 1 1])
(def ^:private repeated-status-path [:semantics :proposition 3 2 1])

(defn- thrown-data [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- audit-mutant [mutation]
  (let [original proposition/describe-proposition!]
    (with-redefs [proposition/describe-proposition!
                  (fn [s] (mutation (original s)))]
      (audit/audit-proposition! declaration))))

(defn- mutant-error [mutation]
  (thrown-data #(audit-mutant mutation)))

(defn- mutate-atom [atom-path f]
  (fn [result] (update-in result atom-path f)))

(defn- forbidden-approval! [result]
  (is (= :not-established (:typed-atomic-denotational-equivalence result)))
  (is (= :not-established (:universal-normalizer-implementation-soundness result)))
  (is (= :not-established (:kernel-derivation-soundness result)))
  (is (= :not-issued (:certificate result)))
  (is (= :not-admitted (:admission result)))
  (is (= :none (:authority result))))

(deftest v1431-full-nested-declaration-is-audited
  (let [normalized (proposition/describe-proposition! declaration)
        result (audit/audit-proposition! declaration)
        canonical (get-in normalized [:semantics :proposition])]
    (is (= :preserved-for-all-opaque-valuations (:status result)))
    (is (= 4 (:atomic-occurrences result)))
    (is (= 3 (:distinct-source-atoms result)))
    (is (= 8 (:valuations-checked result)))
    (is (= (get-in normalized dead-status-path)
           (get-in normalized repeated-status-path)))
    (is (= :and (first canonical)))
    (forbidden-approval! result)))

(deftest v1431-mutations-in-logically-unreachable-proposition-branches
  ;; The cases are data, so a mutation cannot accidentally reuse a checked
  ;; production result as its own oracle.
  (doseq [[description change expected]
          [["dead enum equality -> inequality"
            (mutate-atom dead-status-path
                         #(assoc-in % [:expression 2 0] :not=))
            :atomic-canonicalization-mismatch]
           ["dead enum nominal identity"
            (mutate-atom dead-status-path
                         #(assoc-in % [:reader-types status :name]
                                    :organization/status))
            :atomic-canonicalization-mismatch]
           ["dead enum allowed domain widened"
            (mutate-atom dead-status-path
                         #(update-in % [:reader-types status :values]
                                     conj :request/canceled))
            :atomic-canonicalization-mismatch]
           ["dead optional reader becomes required"
            (mutate-atom dead-status-path
                         #(assoc-in % [:reader-types status :optional?] false))
            :atomic-canonicalization-mismatch]
           ["dead nullable integer becomes nonnullable"
            (mutate-atom dead-score-path
                         #(assoc-in % [:reader-types score :nullable?] false))
            :atomic-canonicalization-mismatch]
           ["dead integer comparison threshold changes"
            (mutate-atom dead-score-path
                         #(assoc-in % [:expression 3 2 1] 8N))
            :atomic-canonicalization-mismatch]
           ["dead integer comparison guard is removed"
            (mutate-atom dead-score-path
                         #(update % :expression
                                  (fn [e] (into [:and] (drop 2 e)))))
            :atomic-canonicalization-mismatch]
           ["dead branch swapped from or to and"
            #(assoc-in % [:semantics :proposition 1 0] :and)
            :connective-structure-mismatch]
           ["dead implication swapped to disjunction"
            #(assoc-in % [:semantics :proposition 2 0] :or)
            :connective-structure-mismatch]
           ["dead atom removed from an or branch"
            #(update-in % [:semantics :proposition 1] pop)
            :connective-structure-mismatch]]]
    (testing description
      (is (= expected (:reason (mutant-error change))) description))))

(deftest v1431-repeated-atoms-must-retain-exact-identity
  (let [baseline (proposition/describe-proposition! declaration)]
    (is (= (get-in baseline dead-status-path)
           (get-in baseline repeated-status-path)))
    (doseq [[description change]
            [["second occurrence has a different expression"
              (mutate-atom repeated-status-path
                           #(assoc-in % [:expression 2 2 1] :request/done))]
             ["second occurrence has a different nominal name"
              (mutate-atom repeated-status-path
                           #(assoc-in % [:reader-types status :name]
                                      :organization/status))]
             ["second occurrence has an extra reader"
              (mutate-atom repeated-status-path
                           #(assoc % :readers #{status score}))]
             ["second occurrence has an altered evaluator version"
              (mutate-atom repeated-status-path
                           #(update % :evaluator-version inc))]
             ["second occurrence silently changes optionality"
              (mutate-atom repeated-status-path
                           #(assoc-in % [:reader-types status :optional?] false))]]]
      (testing description
        (is (= :atomic-canonicalization-mismatch
               (:reason (mutant-error change))) description)))))

(deftest v1431-nested-metadata-and-shape-smuggling-are-rejected
  (let [baseline (proposition/describe-proposition! declaration)
        original-atom (get-in baseline dead-status-path)
        meta-tag {:forged/evidence :trusted}]
    (doseq [[description change]
            [["metadata on dead canonical atom map"
              (mutate-atom dead-status-path #(with-meta % meta-tag))]
             ["metadata on dead expression vector"
              (mutate-atom dead-status-path
                           #(update % :expression with-meta meta-tag))]
             ["metadata on nested type map"
              (mutate-atom dead-status-path
                           #(update-in % [:reader-types status]
                                       with-meta meta-tag))]
             ["metadata on nested enum domain set"
              (mutate-atom dead-status-path
                           #(update-in % [:reader-types status :values]
                                       with-meta meta-tag))]]]
      (testing description
        ;; Clojure value equality ignores collection metadata. The audit must
        ;; reject this independently of its expected-atom equality check.
        (is (= original-atom
               (get-in (change baseline) dead-status-path)))
        (is (= :metadata-not-allowed
               (:reason (mutant-error change))) description)))
    (doseq [[description change]
            [["extra canonical atom field"
              (mutate-atom dead-status-path #(assoc % :certificate :forged))]
             ["missing canonical version field"
              (mutate-atom dead-status-path #(dissoc % :syntax-version))]
             ["changed live boolean reader footprint"
              (mutate-atom live-flag-path #(assoc % :readers #{score}))]]]
      (testing description
        (is (= :atomic-canonicalization-mismatch
               (:reason (mutant-error change))) description)))))

(deftest v1431-unsafe-dead-source-atoms-remain-invalid
  (let [unsafe-score (atom-form score [:> [:read score] [:literal 7]])
        unsafe-flag (atom-form flag [:read flag])
        bad-forms [[:or [:true] unsafe-score]
                   [:implies [:false] unsafe-score]
                   [:and [:false] unsafe-flag]
                   [:or [:true] (atom-form score
                                           [:mystery [:read score]])]]]
    (doseq [form bad-forms]
      (testing (pr-str form)
        (let [src (source form
                          (if (#{:or :implies} (first form))
                            {score score-type} {flag flag-type}))]
          (is (some? (thrown-data #(proposition/describe-proposition! src))))
          (is (some? (thrown-data #(audit/audit-proposition! src)))))))))

(deftest v1431-legitimate-presentation-canonicalization-remains-admitted-as-description-only
  (let [number-atom (atom-form score
                               [:and [:present [:read score]]
                                [:not= [:read score] [:literal nil]]
                                [:>= [:read score] [:literal 7]]])
        bigint-atom (atom-form score
                               [:and [:present [:read score]]
                                [:not= [:read score] [:literal nil]]
                                [:>= [:read score] [:literal 7N]]])
        a (source number-atom {score score-type})
        b (source bigint-atom {score score-type})
        left (proposition/describe-proposition! a)
        right (proposition/describe-proposition! b)]
    (is (= (:fingerprint left) (:fingerprint right)))
    (is (= (get-in left [:semantics :proposition])
           (get-in right [:semantics :proposition])))
    (doseq [s [{} {score nil} {score 6} {score 7} {score 8}]]
      (let [v1 (production/evaluate-rule! (second number-atom)
                                          {score score-type} s)
            v2 (independent/evaluate-expression!
                (:expression (second bigint-atom)) {score score-type} s)]
        (is (= (:value v1) (:value v2)))))
    (doseq [src [a b]]
      (let [result (audit/audit-proposition! src)]
        (is (= :preserved-for-all-opaque-valuations (:status result)))
        (forbidden-approval! result)))))
