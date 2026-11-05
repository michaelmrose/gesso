(ns gesso.assurance.proof.jvm-cljs-semantics-test
  "Permanent v1326 two-host regression contract for the v1325 portable probe.

   These tests run unchanged under Clojure/JVM and ClojureScript/Node. The
   shared-corpus-results vector must ALSO be compared between separate JVM
   and Node processes: a successful run on either host alone cannot establish
   cross-host agreement. The JVM-only production-evaluator tests are an
   additional check, not a claim that the authoritative evaluator runs in JS.

   v1349 extends that JVM/Node corpus with v1348 typed-portability witnesses.
   This suite is finite diagnostic evidence, never an assurance credential."
  (:require [clojure.string :as str]
            #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [gesso.assurance.proof.jvm-cljs-semantics :as probe]
            #?(:clj [gesso.assurance.rule-eval :as production])
            #?(:clj [gesso.assurance.proof.typed-portability-correspondence
                     :as typed-portability])))

(def safe-max 9007199254740991)
(def int-samples [-9007199254740991 -9007199254740990 -101 -8 -1
                  0 1 7 8 101 9007199254740990 9007199254740991])
(def bool-samples [false true])
(def int-ops [:< :<= :> :>= := :not=])
(def bool-ops [:and :or :implies := :not=])

(defn source
  ([expression] (source expression {} {}))
  ([expression types bindings]
   {:rule {:readers (set (keys types)) :expression expression}
    :reader-types types :bindings bindings}))

(defn captured
  "Cross-runtime stable diagnostic projection, including exact failure path."
  [src]
  (try
    (let [report (probe/probe! src)]
      [:ok (:value report)])
    (catch #?(:clj clojure.lang.ExceptionInfo
              :cljs cljs.core.ExceptionInfo) ex
      (let [data (ex-data ex)]
        (if (:jvm-cljs-semantics/failure data)
          [:failure (:status data) (:reason data) (:at data)]
          (throw ex))))))

(defn successful! [expression types bindings expected]
  (let [src (source expression types bindings)
        r (probe/probe! src)]
    (is (= [:ok expected] (captured src)))
    (is (= probe/report-type (:type r)))
    (is (= probe/probe-version (:version r)))
    (is (= :portable-single-host-result (:status r)))
    (is (= :restricted-pure-pretyped-boolean-expression (:scope r)))
    (is (= :exact-js-safe-integers-only (:integer-scope r)))
    (is (= :not-established (:snapshot-provenance r)))
    (is (= :not-established-by-single-host (:jvm-cljs-correspondence r)))
    (is (= :not-established (:independent-checker-soundness r)))
    (is (= :none (:authority r)))
    (is (= :none (:transaction-authority r)))
    r))

(defn rejected! [src status reason at]
  (is (= [:failure status reason at] (captured src))))

(defn expected-bool [op a b]
  (case op
    :and (and a b)
    :or (or a b)
    :implies (or (not a) b)
    := (= a b)
    :not= (not= a b)))

(defn expected-int [op a b]
  (case op
    :< (< a b)
    :<= (<= a b)
    :> (> a b)
    :>= (>= a b)
    := (= a b)
    :not= (not= a b)))

(def flag-type {:request/flag {:type :boolean :optional? true :nullable? true}})
(def flag-read [:read :request/flag])
(def flag-presence [:present flag-read])
(def flag-absence [:not flag-presence])
(def enum-type {:request/status {:type :enum :name :request/status
                                 :values #{:request/open :request/done}}})

(defn boolean-cases []
  (vec
   (concat
    (for [a bool-samples]
      [(str "not/" a) (source [:not [:literal a]])])
    (for [op bool-ops, a bool-samples, b bool-samples]
      [(str "boolean/" (name op) "/" a "/" b)
       (source [op [:literal a] [:literal b]])])
    (for [a bool-samples, b bool-samples]
      [(str "short-circuit/" a "/" b)
       (source [:or [:and [:literal a] [:literal b]]
                [:not [:literal b]]])]))))

(defn integer-cases []
  (vec
   (for [op int-ops, a int-samples, b int-samples]
     [(str "integer/" (name op) "/" a "/" b)
      (source [op [:literal a] [:literal b]])])))

(defn optional-cases []
  (vec
   (for [[label bindings] [["absent" {}] ["nil" {:request/flag nil}]
                            ["false" {:request/flag false}]
                            ["true" {:request/flag true}]]
         [expr-label expr] [["present" flag-presence]
                            ["absent" flag-absence]
                            ["guarded-read" [:or flag-absence
                                             [:= flag-read [:literal true]]]]
                            ["nullable-equality" [:implies flag-presence
                                                  [:= flag-read [:literal nil]]]]
                            ["direct-read" flag-read]]]
     [(str "optional/" label "/" expr-label)
      (source expr flag-type bindings)])))

(defn enum-cases []
  (vec
   (concat
    (for [status [:request/open :request/done]]
      [(str "enum/member/" (name status))
       (source [:in [:read :request/status]
                [:literal #{:request/open :request/done}]]
               enum-type {:request/status status})])
    (for [status [:request/open :request/done]]
      [(str "enum/equality/" (name status))
       (source [:= [:read :request/status] [:literal :request/open]]
               enum-type {:request/status status})])
    [["enum/out-of-domain"
      (source [:literal true] enum-type {:request/status :request/unknown})]
     ["enum/nullable"
      (source [:present [:read :request/status]]
              (assoc-in enum-type [:request/status :nullable?] true)
              {:request/status nil})]
     ["string/equality" (source [:= [:literal "hello"] [:literal "hello"]])]
     ["keyword/equality" (source [:= [:literal :request/a] [:literal :request/b]])]
     ["set/membership-positive"
      (source [:in [:literal 7] [:literal #{7 8 9}]])]
     ["set/membership-negative"
      (source [:in [:literal 10] [:literal #{7 8 9}]])]])))

(defn rejection-cases []
  [["unsafe-positive" (source [:>= [:literal 9007199254740992]
                                [:literal 9007199254740991]])]
   ["unsafe-negative" (source [:< [:literal -9007199254740992]
                                [:literal 0]])]
   ["unsafe-binding" (source [:literal true]
                             {:request/score {:type :integer}}
                             {:request/score 9007199254740992})]
   ["float" (source [:literal 1.5])]
   ["empty-membership-domain" (source [:in [:literal 1] [:literal #{}]])]
   ["mixed-set" (source [:in [:literal 1] [:literal #{1 :request/a}]])]
   ["missing-required" (source [:literal true]
                               {:request/score {:type :integer}} {})]
   ["nonnullable-nil" (source [:literal true]
                               {:request/flag {:type :boolean}}
                               {:request/flag nil})]
   ["wrong-binding-kind" (source [:literal true]
                                 {:request/flag {:type :boolean}}
                                 {:request/flag 1})]
   ["absent-read" (source flag-read flag-type {})]
   ["bad-boolean" (source [:and [:literal 1] [:literal true]])]
   ["bad-comparison" (source [:> [:literal true] [:literal 1]])]
   ["invalid-presence" (source [:present [:literal true]])]
   ["unknown-op" (source [:interpret [:literal true]])]
   ["bad-arity" (source [:not [:literal true] [:literal false]])]
   ["extra-source-key" (assoc (source [:literal true]) :authority :proved)]
   ["metadata" (assoc (source [:literal true]) :bindings
                       (with-meta {} {:forge :yes}))]
   ["incorrect-enum-name" (source [:literal true]
                                   {:request/status {:type :enum :name :status
                                                     :values #{:request/open}}}
                                   {:request/status :request/open})]
   ["unexpected-reader" (assoc-in (source [:literal true])
                                  [:bindings :request/other] true)]
   ["overdepth" (source (nth (iterate (fn [x] [:not x]) [:literal true]) 34))]
   ["overbudget" (source (into [:and] (repeat 513 [:literal true])))]])

(defn typed-portability-cases
  "v1349: pretyped portable witnesses selected from the v1348 S1 bridge.
   All of these become part of the existing JVM/Node byte-for-byte corpus;
   a separate JVM-only test checks the source against the independent S1
   evaluator. Neither the portable probe nor Node trusts a copied report."
  []
  (let [integer-type {:request/score {:type :integer}}
        integer-expr [:>= [:read :request/score] [:literal 0]]
        nullable-type {:request/value {:type :integer
                                       :optional? true :nullable? true}}
        value-read [:read :request/value]
        guarded [:and [:present value-read]
                 [:not= value-read [:literal nil]]
                 [:>= value-read [:literal 7]]]
        enum-expr [:in [:read :request/status]
                   [:literal #{:request/open :request/done}]]]
    (vec
     (concat
      (for [v [(- safe-max) (inc (- safe-max)) -7 0 7
               (dec safe-max) safe-max]]
        [(str "v1349/safe-integer/" v)
         (source integer-expr integer-type {:request/score v})])
      (for [[id expr] [["minimum" [:<= [:literal (- safe-max)]
                                          [:literal safe-max]]]
                       ["maximum" [:>= [:literal safe-max]
                                          [:literal (- safe-max)]]]
                       ["membership" [:in [:literal safe-max]
                                           [:literal #{0 7 9007199254740991}]]]]]
        [(str "v1349/safe-literal/" id) (source expr)])
      (for [[id v] [["positive" 9007199254740992]
                    ["negative" -9007199254740992]]]
        [(str "v1349/nonportable-binding/" id)
         (source integer-expr integer-type {:request/score v})])
      [["v1349/nonportable-unreachable"
        (source [:or [:literal true]
                 [:> [:literal 9007199254740992] [:literal 0]]])]
       ["v1349/nonportable-set-member"
        (source [:in [:literal 1]
                 [:literal #{1 9007199254740992}]])]]
      (for [[id bindings] [["absent" {}]
                            ["nil" {:request/value nil}]
                            ["false" {:request/value false}]
                            ["below" {:request/value 6}]
                            ["equal" {:request/value 7}]
                            ["above" {:request/value 8}]]]
        [(str "v1349/optional-nullable/" id)
         (source guarded nullable-type bindings)])
      (for [state [:request/open :request/done]]
        [(str "v1349/enum/" (name state))
         (source enum-expr enum-type {:request/status state})])
      (for [v [-1 0 7 8 9]]
        [(str "v1349/membership/" v)
         (source [:in [:literal v] [:literal #{0 7 8}]])])
      [["v1349/unsafe-optional-read"
        (source [:>= [:read :request/value] [:literal 7]]
                nullable-type {:request/value 8})]
       ["v1349/missing-required"
        (source integer-expr integer-type {})]
       ["v1349/unsupported-precision-from-literal"
        (source [:< [:literal (- 9007199254740992)] [:literal 0]])]]))))

(defn shared-cases []
  (vec (concat (boolean-cases) (integer-cases) (optional-cases)
               (enum-cases) (rejection-cases)
               (typed-portability-cases))))

(defn shared-corpus-results
  "Value-only artifact to print and compare byte-for-byte across two hosts.
   The returned data is not an authenticated observation or proof report."
  []
  (mapv (fn [[id src]] [id (captured src)]) (shared-cases)))

(deftest complete-boolean-truth-tables
  (doseq [a bool-samples, b bool-samples, op bool-ops]
    (is (= [:ok (expected-bool op a b)]
           (captured (source [op [:literal a] [:literal b]])))
        (str op " " a " " b)))
  (doseq [a bool-samples]
    (is (= [:ok (not a)]
           (captured (source [:not [:literal a]]))))))

(deftest exact-javascript-integer-ordering-matrix
  (doseq [a int-samples, b int-samples, op int-ops]
    (is (= [:ok (expected-int op a b)]
           (captured (source [op [:literal a] [:literal b]])))
        (str op " " a " " b)))
  (is (= 12 (count int-samples)))
  (is (= safe-max (last int-samples))))

(deftest integer-overflow-is-explicit-not-rounded
  (rejected! (source [:> [:literal 9007199254740992] [:literal 0]])
             :unsupported-portable-semantics :unsafe-integer-precision
             [:rule :expression 1 1])
  (rejected! (source [:<= [:literal -9007199254740992] [:literal 0]])
             :unsupported-portable-semantics :unsafe-integer-precision
             [:rule :expression 1 1])
  (rejected! (source [:literal true]
                     {:request/score {:type :integer}}
                     {:request/score 9007199254740992})
             :unsupported-portable-semantics :unsafe-integer-precision
             [:bindings :request/score]))

(deftest optional-absence-and-nil-are-distinct
  (doseq [[bindings present?]
          [[{} false] [{:request/flag nil} true]
           [{:request/flag false} true] [{:request/flag true} true]]]
    (is (= [:ok present?]
           (captured (source flag-presence flag-type bindings))))
    (is (= [:ok (not present?)]
           (captured (source flag-absence flag-type bindings))))
    (is (= [:ok true]
           (captured (source [:or flag-absence [:literal true]]
                             flag-type bindings))))
    (is (= [:ok (if present? (= (get bindings :request/flag) nil) true)]
           (captured (source [:implies flag-presence
                              [:= flag-read [:literal nil]]]
                             flag-type bindings))))))

(deftest guarded-branches-are-not-evaluated
  (is (= [:ok false]
         (captured (source [:and [:literal false] flag-read]
                           flag-type {}))))
  (is (= [:ok true]
         (captured (source [:or [:literal true] flag-read]
                           flag-type {}))))
  (is (= [:ok true]
         (captured (source [:implies [:literal false] flag-read]
                           flag-type {}))))
  (rejected! (source flag-read flag-type {})
             :invalid-source :absent-reader-evaluated [:rule :expression])
  (rejected! (source [:and [:literal true] flag-read] flag-type {})
             :invalid-source :absent-reader-evaluated [:rule :expression 2])
  (rejected! (source [:or [:literal false] flag-read] flag-type {})
             :invalid-source :absent-reader-evaluated [:rule :expression 2]))

(deftest presence-does-not-follow-truthiness
  (is (= [:ok true]
         (captured (source flag-presence flag-type {:request/flag false}))))
  (is (= [:ok true]
         (captured (source flag-presence flag-type {:request/flag nil}))))
  (is (= [:ok false]
         (captured (source flag-presence flag-type {})))))

(deftest nominal-enum-and-membership-behavior
  (doseq [state [:request/open :request/done]]
    (is (= [:ok true]
           (captured (source [:in [:read :request/status]
                              [:literal #{:request/open :request/done}]]
                             enum-type {:request/status state}))))
    (is (= [:ok (= state :request/open)]
           (captured (source [:= [:read :request/status]
                              [:literal :request/open]]
                             enum-type {:request/status state})))))
  (rejected! (source [:literal true] enum-type
                     {:request/status :request/other})
             :invalid-source :enum-binding-outside-domain
             [:bindings :request/status]))

(deftest strict-boolean-results
  (rejected! (source [:and [:literal true] [:literal 1]])
             :invalid-source :nonboolean-operand [:rule :expression 2])
  (rejected! (source [:or [:literal false] [:literal nil]])
             :invalid-source :nonboolean-operand [:rule :expression 2])
  (rejected! (source [:not [:literal 1]])
             :invalid-source :nonboolean-operand [:rule :expression 1])
  (rejected! (source [:literal 1])
             :invalid-source :nonboolean-operand [:rule :expression]))

(deftest malformed-source-and-report-injection
  (let [plain (source [:literal true])]
    (rejected! (assoc plain :proof-status :proved)
               :invalid-source :wrong-keys [])
    (rejected! (assoc plain :jvm-cljs-correspondence :established)
               :invalid-source :wrong-keys [])
    (rejected! (with-meta plain {:trusted true})
               :invalid-source :metadata-not-allowed [])
    (rejected! (assoc plain :bindings (with-meta {} {:trusted true}))
               :invalid-source :metadata-not-allowed [:bindings])
    (rejected! (assoc-in plain [:rule :expression]
                         (with-meta [:literal true] {:trusted true}))
               :invalid-source :metadata-not-allowed [:rule :expression])
    (rejected! (assoc-in plain [:rule :readers] #{:unqualified})
               :invalid-source :unqualified-reader [:rule :readers])
    (rejected! (assoc-in plain [:rule :readers] #{:request/new})
               :invalid-source :wrong-keys [:reader-types])))

(deftest malformed-expression-and-resource-bounds
  (rejected! (source [:execute [:literal true]])
             :unsupported-portable-semantics
             :unsupported-expression-operator [:rule :expression])
  (rejected! (source [:present [:literal true]])
             :invalid-source :invalid-presence-operand [:rule :expression])
  (rejected! (source [:not [:literal true] [:literal false]])
             :invalid-source :wrong-expression-arity [:rule :expression])
  (rejected! (source [:in [:literal 1] [:literal #{}]])
             :unsupported-portable-semantics :unsupported-literal-set
             [:rule :expression 2 1])
  (rejected! (source (nth (iterate (fn [x] [:not x]) [:literal true]) 34))
             :unsupported-portable-semantics :expression-depth-limit
             (vec (concat [:rule :expression] (repeat 33 1))))
  (is (= :expression-node-limit
         (nth (captured (source (into [:and]
                                     (repeat 513 [:literal true])))) 2))))

(deftest long-boolean-chains-preserve-lazy-guarding
  ;; A chunked lazy sequence must not force a later invalid optional read.
  (let [unread flag-read
        long-and (into [:and [:literal false]]
                       (repeat 120 unread))
        long-or (into [:or [:literal true]]
                      (repeat 120 unread))]
    (is (= [:ok false] (captured (source long-and flag-type {}))))
    (is (= [:ok true] (captured (source long-or flag-type {}))))
    (is (= [:ok true]
           (captured (source [:implies [:literal false]
                              [:and unread [:literal true]]]
                             flag-type {}))))))

(deftest short-circuiting-does-not-hide-malformed-structure
  ;; Syntax and declaration checks precede runtime guard evaluation.
  (rejected! (source [:and [:literal false] [:execute [:literal true]]])
             :unsupported-portable-semantics :unsupported-expression-operator
             [:rule :expression 2])
  (rejected! (source [:or [:literal true] [:present [:literal true]]])
             :invalid-source :invalid-presence-operand
             [:rule :expression 2])
  (rejected! (source [:implies [:literal false] [:read :request/unknown]])
             :invalid-source :undeclared-reader [:rule :expression 2 1]))

(deftest portable-integer-boundaries-are-inclusive
  (is (= [:ok true]
         (captured (source [:= [:literal safe-max]
                            [:literal 9007199254740991]]))))
  (is (= [:ok true]
         (captured (source [:= [:literal (- safe-max)]
                            [:literal -9007199254740991]]))))
  (is (= [:ok true]
         (captured (source [:< [:literal (- safe-max)]
                            [:literal safe-max]]))))
  (is (= [:ok true]
         (captured (source [:>= [:literal safe-max]
                            [:literal (- safe-max)]])))))

(deftest portable-probe-avoids-jvm-only-integer-semantics
  ;; The JVM can represent 2^53+1 exactly; a JS Number cannot. Reject both.
  (let [large #?(:clj 9007199254740993N :cljs 9007199254740992)]
    (is (= :unsafe-integer-precision
           (nth (captured (source [:> [:literal large]
                                    [:literal 9007199254740991]])) 2)))
    #?(:clj
       (is (= true
              (:value (production/evaluate-rule!
                       {:readers #{} :expression
                        [:> [:literal large] [:literal 9007199254740991]]}
                       {} {})))))))

(deftest mixed-literal-and-unsupported-value-rejection
  (rejected! (source [:in [:literal 1] [:literal #{1 :request/open}]])
             :unsupported-portable-semantics :mixed-literal-set
             [:rule :expression 2 1])
  (rejected! (source [:literal 0.5])
             :unsupported-portable-semantics :nonportable-value
             [:rule :expression 1])
  (rejected! (source [:literal (fn [] :not-data)])
             :unsupported-portable-semantics :nonportable-value
             [:rule :expression 1])
  (rejected! (source [:literal (apply str (repeat 2050 "x"))])
             :unsupported-portable-semantics :oversize-value
             [:rule :expression 1]))

(deftest reader-type-descriptors-are-not-trusted
  (let [base (source [:literal true]
                     {:request/flag {:type :boolean}}
                     {:request/flag true})]
    (rejected! (assoc-in base [:reader-types :request/flag :optional?]
                         :present)
               :invalid-source :nonboolean-descriptor-flag
               [:reader-types :request/flag :optional?])
    (rejected! (assoc-in base [:reader-types :request/flag :nullable?]
                         :nullable)
               :invalid-source :nonboolean-descriptor-flag
               [:reader-types :request/flag :nullable?])
    (rejected! (assoc-in base [:reader-types :request/flag :proof] true)
               :invalid-source :unknown-descriptor-keys
               [:reader-types :request/flag])
    (rejected! (assoc-in base [:reader-types :request/flag :type] :float)
               :unsupported-portable-semantics :unsupported-reader-type
               [:reader-types :request/flag])))

(deftest binding-shape-must-match-original-declaration
  (rejected! (source [:literal true] {:request/flag {:type :boolean}} {})
             :invalid-source :missing-required-binding
             [:bindings :request/flag])
  (rejected! (assoc-in (source [:literal true]) [:bindings :request/other]
                       true)
             :invalid-source :undeclared-bindings [:bindings])
  (rejected! (source [:literal true]
                     {:request/flag {:type :boolean :nullable? false}}
                     {:request/flag nil})
             :invalid-source :nil-for-nonnullable-binding
             [:bindings :request/flag]))

(deftest optional-read-and-present-nil-do-not-conflate
  (let [types {:request/score {:type :integer :optional? true
                               :nullable? true}}
        read-score [:read :request/score]
        is-present [:present read-score]]
    (is (= [:ok false] (captured (source is-present types {}))))
    (is (= [:ok true] (captured (source is-present types
                                           {:request/score nil}))))
    (is (= [:ok true]
           (captured (source [:= read-score [:literal nil]]
                             types {:request/score nil}))))
    (rejected! (source [:= read-score [:literal nil]] types {})
               :invalid-source :absent-reader-evaluated
               [:rule :expression 1])))

(deftest malformed-literal-domains-and-membership
  (rejected! (source [:in [:literal true] [:literal true]])
             :invalid-source :nonset-membership-domain
             [:rule :expression])
  (is (= [:ok true]
         (captured (source [:in [:literal nil] [:literal #{nil}]]))))
  (is (= [:ok false]
         (captured (source [:in [:literal nil] [:literal #{:request/a}]]))))
  (is (= [:ok false]
         (captured (source [:not= [:literal nil] [:literal nil]])))))

(deftest all-shared-case-identifiers-are-unique
  (let [names (map first (shared-cases))]
    (is (= (count names) (count (set names))))
    (is (every? string? names))
    (is (> (count names) 900))))

(deftest ordinary-portable-reports-grant-no-assurance
  (successful! [:implies [:literal true] [:literal true]] {} {} true)
  (successful! [:implies [:literal true] [:literal false]] {} {} false)
  (successful! [:in [:literal :request/open]
                [:literal #{:request/open :request/done}]] {} {} true))

(deftest new-corpus-is-deterministic-and-nontrivial
  (let [a (shared-corpus-results)
        b (shared-corpus-results)]
    (is (= a b))
    (is (= (count (shared-cases)) (count a)))
    (is (> (count a) 900))
    (is (= (count a) (count (distinct (map first a)))))
    (is (every? #(#{:ok :failure} (first (second %))) a))
    (is (some #(= :unsafe-integer-precision
                  (get-in % [1 2])) a))
    (is (some #(= :absent-reader-evaluated
                  (get-in % [1 2])) a))))

#?(:clj
   (deftest portable-successes-crosscheck-authoritative-jvm-evaluator
     (doseq [src [(source [:literal true])
                  (source [:implies [:literal false] [:literal true]])
                  (source [:> [:read :request/score] [:literal 7]]
                          {:request/score {:type :integer}}
                          {:request/score 8})
                  (source [:present [:read :request/flag]]
                          flag-type {})
                  (source [:or flag-absence
                           [:= flag-read [:literal true]]]
                          flag-type {:request/flag false})
                  (source [:in [:read :request/status]
                           [:literal #{:request/open :request/done}]]
                          enum-type {:request/status :request/done})]]
       (is (= (:value (probe/probe! src))
              (:value (production/evaluate-rule!
                       (:rule src) (:reader-types src) (:bindings src))))))))

(deftest typed-portability-shared-witnesses-are-distinct
  (let [cases (typed-portability-cases)
        ids (mapv first cases)
        by-id (into {} cases)]
    (is (>= (count cases) 25))
    (is (= (count ids) (count (set ids))))
    (is (every? #(str/starts-with? % "v1349/") ids))
    (doseq [id ["v1349/safe-integer/9007199254740991"
                "v1349/safe-integer/-9007199254740991"
                "v1349/optional-nullable/absent"
                "v1349/optional-nullable/nil"
                "v1349/optional-nullable/equal"
                "v1349/enum/done"]]
      (is (= :ok (first (captured (get by-id id)))) id))
    (doseq [id ["v1349/nonportable-binding/positive"
                "v1349/nonportable-binding/negative"
                "v1349/nonportable-unreachable"
                "v1349/nonportable-set-member"
                "v1349/unsupported-precision-from-literal"]]
      (is (= :unsafe-integer-precision
             (nth (captured (get by-id id)) 2)) id))
    (is (= [:ok false]
           (captured (get by-id "v1349/optional-nullable/absent"))))
    (is (= [:ok false]
           (captured (get by-id "v1349/optional-nullable/nil"))))
    (is (= [:ok true]
           (captured (get by-id "v1349/optional-nullable/equal"))))
    (is (= [:failure :invalid-source :incorrect-binding-type
            [:bindings :request/value]]
           (captured (get by-id "v1349/optional-nullable/false"))))
    (is (= :missing-required-binding
           (nth (captured (get by-id "v1349/missing-required")) 2)))))

#?(:clj
   (deftest typed-portability-witnesses-match-independent-s1-on-jvm
     (doseq [[id src] (typed-portability-cases)]
       (let [r (typed-portability/audit-witness! src)
             observed (captured src)]
         (is (= src (:source r)) id)
         (is (= :not-established (:cross-host-correspondence r)) id)
         (is (= :not-proved (:proof-status r)) id)
         (is (= :not-admitted (:admission r)) id)
         (is (= :none (:authority r)) id)
         (is (not (contains? r :certificate)) id)
         (case (:status r)
           :witness-agreement
           (is (= [:ok (:independent-value r)] observed) id)

           :nonportable
           (is (= :unsafe-integer-precision (nth observed 2 nil)) id)

           :invalid-witness
           (is (= :failure (first observed)) id)

           :production-rejected
           (is (contains? #{:ok :failure} (first observed)) id)

           (is false (str "Unexpected v1348 audit status for " id
                          ": " (:status r))))))))
