(ns gesso.assurance.rule-normalize-test
  "v1226: permanent adversarial regression coverage for canonical, typed rule
   semantics introduced in v1225. The normalized fingerprint is deliberately
   conservative: only explicitly defined representation equivalences collapse.
   A fingerprint is NOT a proof, evaluated observation, checker admission, or
   deployed-operation authorization."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.rule :as rule]
            [gesso.assurance.rule-type :as rule-type]
            [gesso.assurance.rule-eval :as rule-eval]
            [gesso.assurance.rule-normalize :as normalize]
            [gesso.assurance.subject :as subject]))

(def ^:private score :result/score)
(def ^:private other-score :result/other-score)
(def ^:private status :request/status)
(def ^:private enabled :request/enabled)
(def ^:private integer-type {:type :integer})
(def ^:private boolean-type {:type :boolean})
(def ^:private status-type
  {:type :enum :name :humanhelp/request-status
   :values #{:new :active :done}})

(defn- decl
  [expression types]
  {:readers (set (keys types)) :expression expression})

(defn- normalized
  [expression types]
  (normalize/normalize-rule! (decl expression types) types))

(defn- fingerprint
  [expression types]
  (:fingerprint (normalized expression types)))

(defn- failure
  [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- rejects!
  [thunk error-type reason]
  (let [data (failure thunk)]
    (is (map? data) (str "Expected typed rejection: " error-type "/" reason))
    (is (= error-type (:error/type data)) (pr-str data))
    (is (= reason (:reason data)) (pr-str data))
    data))

(defn- reject-syntax!
  [declaration types reason]
  (rejects! #(normalize/normalize-rule! declaration types)
            :gesso.assurance.rule/invalid-rule reason))

(defn- reject-types!
  [declaration types reason]
  (rejects! #(normalize/normalize-rule! declaration types)
            :gesso.assurance.rule-type/invalid-rule-type reason))

(deftest normalized-report-is-structural-and-unadmitted
  (let [expr [:> [:read score] [:literal 7]]
        report (normalized expr {score integer-type})
        semantics (:semantics report)]
    (is (= normalize/normalized-type (:type report)))
    (is (= normalize/normalization-version (:version report)))
    (is (= subject/canonical-format (:canonical-format report)))
    (is (re-matches #"sha256:[0-9a-f]{64}" (:fingerprint report)))
    (is (= :syntax-and-types (:checking report)))
    (is (= :conservative-interpretation (:normalization report)))
    (is (= :not-performed (:evaluation report)))
    (is (= :not-proved (:proof-status report)))
    (is (= :not-admitted (:admission report)))
    (is (= :not-established (:runtime-enforcement report)))
    (is (= :none (:authority report)))
    (is (= normalize/normalization-version (:normalization-version semantics)))
    (is (= rule/rule-version (:syntax-version semantics)))
    (is (= rule-type/type-version (:type-version semantics)))
    (is (= rule-eval/evaluator-version (:evaluator-version semantics)))
    (is (= #{score} (:readers semantics)))
    (is (= expr (:expression semantics)))
    (is (= {:type :integer :optional? false :nullable? false}
           (get-in semantics [:reader-types score])))))

(deftest deterministic-repetition-and-input-preservation
  (let [expr [:and
              [:in [:read status] [:literal #{:active :new}]]
              [:> [:read score] [:literal 7]]]
        types {status status-type score integer-type}
        declaration (decl expr types)
        before-decl (pr-str declaration)
        before-types (pr-str types)
        samples (repeatedly 30 #(normalize/normalize-rule! declaration types))]
    (is (= 1 (count (set (map :fingerprint samples)))))
    (is (= 1 (count (set (map :semantics samples)))))
    (is (= before-decl (pr-str declaration)))
    (is (= before-types (pr-str types)))
    (is (every? #(= :not-admitted (:admission %)) samples))))

(deftest alternative-map-construction-and-readers-set-order
  (let [x [:and [:> [:read score] [:literal 7]]
               [:= [:read status] [:literal :active]]]
        first-types (array-map score integer-type status status-type)
        second-types (hash-map status status-type score integer-type)
        first-decl {:readers (into #{} [score status]) :expression x}
        second-decl (hash-map :expression x :readers (into #{} [status score]))]
    (is (= (:fingerprint (normalize/normalize-rule! first-decl first-types))
           (:fingerprint (normalize/normalize-rule! second-decl second-types))))
    (is (normalize/same-normalized-rule? first-decl first-types
                                        second-decl second-types))))

(deftest reordered-enum-domains-and-literal-sets-are-identical
  (let [t1 {status status-type}
        t2 {status (assoc status-type :values (into #{} [:done :new :active]))}
        a [:in [:read status] [:literal (into #{} [:new :active])]]
        b [:in [:read status] [:literal (into #{} [:active :new])]]]
    (is (= (fingerprint a t1) (fingerprint b t2)))
    (is (normalize/same-normalized-rule? (decl a t1) t1 (decl b t2) t2))
    (is (= (get-in (normalized a t1) [:semantics :reader-types status :values])
           #{:new :active :done}))))

(deftest integer-representation-is-mathematical-and-exact
  (let [types {score integer-type}
        read [:read score]
        lit (fn [v] [:> read [:literal v]])
        inputs [7 7N (long 7) (java.math.BigInteger. "7")]
        giant (bigint "100000000000000000000000000000000000000")]
    (is (= 1 (count (set (map #(fingerprint (lit %) types) inputs)))))
    (is (= [:> read [:literal 7N]]
           (get-in (normalized (lit 7) types) [:semantics :expression])))
    (is (= (fingerprint (lit giant) types)
           (fingerprint (lit (.toBigInteger giant)) types)))
    (is (not= (fingerprint (lit giant) types)
              (fingerprint (lit (inc giant)) types)))
    (is (= (fingerprint [:in read [:literal #{7 9 11}]] types)
           (fingerprint [:in read [:literal #{11N 7N 9N}]] types)))))

(deftest default-false-type-flags-normalize-identically
  (let [expr [:> [:read score] [:literal 7]]
        a {score {:type :integer}}
        b {score {:type :integer :optional? false :nullable? false}}
        c {score {:nullable? false :type :integer :optional? false}}]
    (is (= (fingerprint expr a) (fingerprint expr b)))
    (is (= (fingerprint expr b) (fingerprint expr c)))
    (is (= (get-in (normalized expr a) [:semantics :reader-types score])
           (get-in (normalized expr c) [:semantics :reader-types score])))))

(deftest optional-flag-is-identity-bearing-even-when-expression-is-unchanged
  (let [expr [:present [:read score]]
        required {score integer-type}
        optional {score {:type :integer :optional? true}}]
    (is (not= (fingerprint expr required) (fingerprint expr optional)))
    (is (false? (normalize/same-normalized-rule? (decl expr required) required
                                                 (decl expr optional) optional)))))

(deftest nullable-flag-is-identity-bearing-even-when-expression-is-unchanged
  (let [expr [:present [:read score]]
        nonnull {score integer-type}
        nullable {score {:type :integer :nullable? true}}]
    (is (not= (fingerprint expr nonnull) (fingerprint expr nullable)))))

(deftest nominal-enum-name-is-identity-bearing
  (let [expr [:= [:read status] [:literal :active]]
        old {status status-type}
        renamed {status (assoc status-type :name :other/request-status)}]
    (is (not= (fingerprint expr old) (fingerprint expr renamed)))))

(deftest nominal-enum-domain-is-identity-bearing
  (let [expr [:= [:read status] [:literal :active]]
        old {status status-type}
        grown {status (update status-type :values conj :paused)}
        shrunk {status (update status-type :values disj :new)}]
    (is (not= (fingerprint expr old) (fingerprint expr grown)))
    (is (not= (fingerprint expr old) (fingerprint expr shrunk)))
    (is (not= (fingerprint expr grown) (fingerprint expr shrunk)))))

(deftest field-type-is-identity-bearing
  (let [expr [:= [:read score] [:read score]]
        old {score integer-type}
        new {score {:type :string}}]
    (is (not= (fingerprint expr old) (fingerprint expr new)))))

(deftest reader-renaming-is-identity-bearing
  (let [one [:> [:read score] [:literal 7]]
        two [:> [:read other-score] [:literal 7]]]
    (is (not= (fingerprint one {score integer-type})
              (fingerprint two {other-score integer-type})))))

(deftest reordered-conjunction-and-disjunction-are-distinct
  (let [types {score integer-type enabled boolean-type}
        left [:> [:read score] [:literal 7]]
        right [:= [:read enabled] [:literal true]]]
    (doseq [operator [:and :or]]
      (let [forward [operator left right]
            reversed [operator right left]]
        (is (not= (fingerprint forward types) (fingerprint reversed types)))
        (is (false? (normalize/same-normalized-rule? (decl forward types) types
                                                     (decl reversed types) types)))))))

(deftest implication-operand-order-is-preserved
  (let [types {score integer-type enabled boolean-type}
        a [:> [:read score] [:literal 7]]
        b [:= [:read enabled] [:literal true]]]
    (is (not= (fingerprint [:implies a b] types)
              (fingerprint [:implies b a] types)))))

(deftest comparison-and-boolean-operator-mutations-invalidate-identity
  (let [types {score integer-type}
        r [:read score]
        expressions [[:> r [:literal 7]]
                     [:>= r [:literal 7]]
                     [:< r [:literal 7]]
                     [:<= r [:literal 7]]
                     [:= r [:literal 7]]
                     [:not= r [:literal 7]]
                     [:not [:> r [:literal 7]]]]]
    (is (= (count expressions)
           (count (set (map #(fingerprint % types) expressions)))))))

(deftest meaning-changing-literal-mutations-invalidate-identity
  (let [types {score integer-type}
        expressions (mapv (fn [v] [:> [:read score] [:literal v]])
                          (range -12 61))]
    (is (= (count expressions)
           (count (set (map #(fingerprint % types) expressions)))))
    (doseq [n (range -12 61 4)]
      (is (= (fingerprint [:> [:read score] [:literal n]] types)
             (fingerprint [:> [:read score] [:literal (bigint n)]] types))))))

(deftest mutation-family-never-collapses-except-allowed-integer-representation
  (let [types {score integer-type}
        r [:read score]
        base [:> r [:literal 7]]
        mutants [[:> r [:literal 8]]
                 [:>= r [:literal 7]]
                 [:not [:> r [:literal 7]]]
                 [:= r [:literal 7]]
                 [:< r [:literal 7]]]
        origin (fingerprint base types)]
    (doseq [mutant mutants]
      (testing (pr-str mutant)
        (is (not= origin (fingerprint mutant types)))))
    (is (= origin (fingerprint [:> r [:literal 7N]] types)))))

(deftest logically-equivalent-but-different-expressions-remain-distinct
  (let [types {score integer-type}
        p [:> [:read score] [:literal 7]]
        double-negation [:not [:not p]]
        equivalent-by-algebra [:>= [:read score] [:literal 8]]]
    (is (not= (fingerprint p types) (fingerprint double-negation types)))
    (is (not= (fingerprint p types) (fingerprint equivalent-by-algebra types)))
    (is (false? (normalize/same-normalized-rule? (decl p types) types
                                                 (decl double-negation types) types)))))

(deftest semantically-irrelevant-descriptor-key-order-is-ignored
  (let [expr [:present [:read status]]
        a {status (array-map :type :enum :name :humanhelp/request-status
                             :values #{:new :active :done}
                             :optional? false :nullable? false)}
        b {status (hash-map :nullable? false :values #{:done :new :active}
                            :name :humanhelp/request-status :optional? false
                            :type :enum)}]
    (is (= (fingerprint expr a) (fingerprint expr b)))))

(deftest rule-version-and-semantic-dependencies-are-present-in-identity
  (let [r (normalized [:literal true] {})
        semantics (:semantics r)
        identity (subject/describe-subject
                  {:kind :rule :id :gesso.assurance/rule-semantics-v1
                   :interface semantics})
        mutated (update semantics :evaluator-version inc)
        changed (subject/describe-subject
                 {:kind :rule :id :gesso.assurance/rule-semantics-v1
                  :interface mutated})]
    (is (= (:fingerprint r) (:fingerprint identity)))
    (is (not= (:fingerprint identity) (:fingerprint changed)))
    (is (= 1 (:normalization-version semantics)))
    (is (= 1 (:syntax-version semantics)))
    (is (= 1 (:type-version semantics)))
    (is (= 1 (:evaluator-version semantics)))))

(deftest source-mutation-invalidates-dependent-identities
  (let [base [:and [:> [:read score] [:literal 7]]
               [:in [:read status] [:literal #{:new :active}]]]
        t {score integer-type status status-type}
        old (normalized base t)
        altered (normalized [:and [:> [:read score] [:literal 8]]
                             [:in [:read status] [:literal #{:new :active}]]] t)]
    (is (not= (:fingerprint old) (:fingerprint altered)))
    (is (= :not-admitted (:admission old)))
    (is (= :not-admitted (:admission altered)))
    (is (= :not-proved (:proof-status altered)))
    (is (not (contains? altered :certificate)))))

(deftest identity-input-revalidated-and-forged-reports-rejected
  (let [d (decl [:> [:read score] [:literal 7]] {score integer-type})
        types {score integer-type}
        report (normalize/normalize-rule! d types)
        injected-fields [[:fingerprint (:fingerprint report)]
                         [:admission :admitted]
                         [:proof-status :proved]
                         [:certificate {:valid? true}]
                         [:valid? true]
                         [:checking :proof-kernel]]]
    (doseq [[key value] injected-fields]
      (testing (str "injected " key)
        (reject-syntax! (assoc d key value) types :wrong-declaration-keys)))
    (is (= (:fingerprint report) (:fingerprint (normalize/normalize-rule! d types))))
    (is (not (contains? report :certificate)))))

(deftest forged-reader-type-report-rejected
  (let [d (decl [:> [:read score] [:literal 7]] {score integer-type})
        types {score integer-type}]
    (doseq [forged [(assoc types score {:type :integer :proof-status :proved})
                    (assoc types score {:type :integer :admission :admitted})
                    (assoc types score {:type :integer :valid? true})]]
      (reject-types! d forged :unknown-descriptor-keys))))

(deftest untrusted-check-reports-cannot-be-normalization-inputs
  (let [d (decl [:> [:read score] [:literal 7]] {score integer-type})
        types {score integer-type}
        checked (rule-type/check-rule-type! d types)
        report (normalize/normalize-rule! d types)]
    (reject-syntax! checked types :wrong-declaration-keys)
    (reject-syntax! report types :wrong-declaration-keys)
    (reject-types! d checked :reader-type-footprint-mismatch)
    (reject-types! d report :reader-type-footprint-mismatch)))

(deftest false-type-declarations-cannot-be-normalized-to-authority
  (let [d (decl [:> [:read score] [:literal 7]] {score integer-type})]
    (reject-types! d {score {:type :string}} :expected-nonnullable-integers)
    (reject-types! d {score {:type :enum :name :example/status
                             :values #{:new :done}}} :expected-nonnullable-integers)))

(deftest incompatible-or-malformed-expressions-rejected-before-identity
  (let [d (decl [:> [:read score] [:literal 7]] {score integer-type})
        types {score integer-type}]
    (reject-syntax! (assoc d :expression [:unknown [:read score]]) types :unknown-operator)
    (reject-syntax! (assoc d :expression [:> [:read other-score] [:literal 7]])
                    types :undeclared-reader)
    (reject-types! (assoc d :expression [:> [:read score] [:literal "7"]])
                   types :expected-nonnullable-integers)))

(deftest normalization-does-not-evaluate-or-prove
  (let [expr [:> [:read score] [:literal 7]]
        types {score integer-type}
        normalized-report (normalized expr types)
        observed (rule-eval/evaluate-rule! (decl expr types) types {score 3})]
    (is (false? (:value observed)))
    (is (= :not-proved (:proof-status normalized-report)))
    (is (= :not-performed (:evaluation normalized-report)))
    (is (= :none (:authority normalized-report)))
    (is (not (contains? normalized-report :value)))
    (is (not (contains? normalized-report :bindings)))))

(deftest unsupported-executable-values-and-metadata-rejected
  (let [types {score integer-type}
        d (decl [:> [:read score] [:literal 7]] types)]
    (reject-syntax! (assoc d :expression [:> [:read score] [:literal (fn [] 7)]])
                    types :invalid-literal)
    (reject-syntax! (with-meta d {:proof true}) types :metadata-not-allowed)
    (reject-syntax! (assoc d :expression (with-meta (:expression d) {:auth true}))
                    types :metadata-not-allowed)
    (reject-types! d (assoc types score (with-meta integer-type {:proof true}))
                   :metadata-not-allowed)))

(deftest evaluation-agreement-for-intended-representation-equivalences
  (let [types {score integer-type}
        expr-a [:> [:read score] [:literal 7]]
        expr-b [:> [:read score] [:literal 7N]]
        a (decl expr-a types)
        b (decl expr-b types)]
    (is (normalize/same-normalized-rule? a types b types))
    (doseq [n [-100 -1 0 7 8 9 120N (bigint "9223372036854775808")]]
      (let [v1 (:value (rule-eval/evaluate-rule! a types {score n}))
            v2 (:value (rule-eval/evaluate-rule! b types {score n}))]
        (is (= v1 v2) (str "Representation difference at score " n))))))

(deftest conditional-short-circuit-order-preserved
  (let [types {score {:type :integer :optional? true :nullable? true}}
        r [:read score]
        expr [:and [:present r] [:not= r [:literal nil]] [:> r [:literal 7]]]
        report (normalized expr types)]
    (is (= expr (:expression (:semantics report))))
    (is (= :not-admitted (:admission report)))
    (reject-types! (decl [:and [:> r [:literal 7]] [:present r]
                                 [:not= r [:literal nil]]] types)
                   types :unguarded-optional-reader)))

(deftest normalized-fingerprint-is-not-a-certificate
  (let [r (normalized [:literal true] {})]
    (is (= :not-proved (:proof-status r)))
    (is (= :not-admitted (:admission r)))
    (is (= :not-established (:runtime-enforcement r)))
    (is (= :none (:authority r)))
    (is (not (contains? r :proof)))
    (is (not (contains? r :proof-receipt)))
    (is (not (contains? r :validated?)))
    (is (not (contains? r :handle)))))

(deftest deterministic-threshold-mutation-matrix
  (let [types {score integer-type status status-type}]
    (doseq [threshold (range -63 129)]
      (let [build (fn [n statuses]
                    [:and [:> [:read score] [:literal n]]
                     [:in [:read status] [:literal statuses]]])
            original (build threshold #{:new :active})
            integer-representation (build (bigint threshold)
                                          (into #{} [:active :new]))
            changed-threshold (build (inc threshold) #{:new :active})
            changed-statuses (build threshold #{:new :done})
            a (fingerprint original types)
            b (fingerprint integer-representation types)
            c (fingerprint changed-threshold types)
            d (fingerprint changed-statuses types)]
        (is (= a b) (str "Integer/set normalization at " threshold))
        (is (not= a c) (str "Threshold mutation at " threshold))
        (is (not= a d) (str "Domain mutation at " threshold))
        (is (not= c d) (str "Distinct mutations at " threshold))))))

(deftest deterministic-boolean-mutation-matrix
  (let [types {score integer-type enabled boolean-type}]
    (doseq [threshold (range -20 60)]
      (let [score? [:> [:read score] [:literal threshold]]
            enabled? [:= [:read enabled] [:literal true]]
            values (mapv (fn [expression] (fingerprint expression types))
                         [[:and score? enabled?]
                          [:and enabled? score?]
                          [:or score? enabled?]
                          [:or enabled? score?]
                          [:implies score? enabled?]
                          [:implies enabled? score?]])]
        (is (= (count values) (count (set values)))
            (str "Operator and argument order identities at " threshold))
        (is (= (first values)
               (fingerprint [:and score? enabled?] types)))))))

(deftest deterministic-nominal-enum-mutation-matrix
  (let [expr [:= [:read status] [:read status]]]
    (doseq [index (range 1 60)]
      (let [name-a (keyword "model.enum" (str "status-" index))
            name-b (keyword "model.enum" (str "status-" (inc index)))
            a {status {:type :enum :name name-a :values #{:new :done}}}
            same {status {:values (into #{} [:done :new]) :name name-a
                          :type :enum :optional? false :nullable? false}}
            changed-name {status {:type :enum :name name-b :values #{:new :done}}}
            changed-domain {status {:type :enum :name name-a :values #{:new :active}}}]
        (is (= (fingerprint expr a) (fingerprint expr same)))
        (is (not= (fingerprint expr a) (fingerprint expr changed-name)))
        (is (not= (fingerprint expr a) (fingerprint expr changed-domain)))))))

(deftest malformed-and-colliding-collections-fail-closed
  (let [d (decl [:in [:read score] [:literal #{7 9}]] {score integer-type})
        types {score integer-type}]
    (reject-syntax! (assoc d :expression
                           [:in [:read score] [:literal (sorted-set 7 9)]])
                    types :unsupported-collection)
    (reject-syntax! (assoc d :expression
                           [:in [:read score] [:literal (into #{} (range 257))]])
                    types :invalid-literal-set)
    (reject-types! d (assoc types score {:type :integer :optional? :false})
                   :invalid-type-flag)))
