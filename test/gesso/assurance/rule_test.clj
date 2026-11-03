(ns gesso.assurance.rule-test
  "v1220: permanent syntax/adversarial regressions for v1219 closed rule data.

   This tests the exact current grammar, reading-footprint closure, resource
   budgets, and rejection of executable payloads. A successful check is ONLY
   syntactic. It is not operand type checking, evaluation, proof admission,
   invariant preservation, trusted code isolation, or XTDB correspondence."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.rule :as rule]))

(def ^:private request-status :request/status)
(def ^:private request-owner :request/owner)
(def ^:private result-score :result/score)

(defn- declaration
  ([expression] {:readers #{} :expression expression})
  ([readers expression] {:readers readers :expression expression}))

(defn- rejection
  [thunk]
  (try
    (thunk)
    nil
    (catch clojure.lang.ExceptionInfo e
      (ex-data e))))

(defn- rejects!
  ([candidate reason]
   (rejects! candidate reason nil))
  ([candidate reason at]
   (let [data (rejection #(rule/check-rule! candidate))]
     (is (map? data) (str "Expected typed rejection " reason " of " (pr-str candidate)))
     (is (= :gesso.assurance.rule/invalid-rule (:error/type data)))
     (is (= reason (:reason data)))
     (when at
       (is (= at (:at data)))))))

(defn- accepted!
  [candidate]
  (let [result (rule/check-rule! candidate)]
    (is (= rule/rule-type (:type result)))
    (is (= rule/rule-version (:version result)))
    (is (= (:expression candidate) (:expression result)))
    (is (= (:readers candidate) (:readers result)))
    (is (pos-int? (:nodes result)))
    (is (= :syntax-only (:checking result)))
    (is (= :not-performed (:typing result)))
    (is (= :not-performed (:evaluation result)))
    (is (= :not-proved (:proof-status result)))
    (is (= :not-admitted (:admission result)))
    (is (= :none (:authority result)))
    (is (= #{:type :version :expression :readers :nodes :checking
             :typing :evaluation :proof-status :admission :authority}
           (set (keys result))))
    result))

(deftest minimal-closed-rule-and-report-remain-nonauthoritative
  (let [candidate (declaration [:literal true])
        report (accepted! candidate)]
    (is (= 1 (:nodes report)))
    (is (= report (rule/check-rule! candidate)))
    (is (not (contains? report :certificate)))
    (is (not (contains? report :valid?)))
    (is (not (contains? report :proved?)))
    (is (not (contains? report :discharged?)))
    (is (not (identical? candidate report)))))

(deftest every-supported-operator-can-be-validated
  (let [cases [[:literal nil] [:literal false] [:literal 9N]
               [:read request-status]
               [:present [:read request-status]]
               [:not [:literal false]]
               [:and [:literal true] [:literal true]]
               [:and [:literal true] [:literal false] [:literal true]]
               [:or [:literal false] [:literal true]]
               [:= [:literal 1] [:literal 1]]
               [:not= [:literal 1] [:literal 2]]
               [:< [:literal 1] [:literal 2]]
               [:<= [:literal 1] [:literal 2]]
               [:> [:literal 2] [:literal 1]]
               [:>= [:literal 2] [:literal 1]]
               [:in [:literal :active] [:literal #{:active :done}]]
               [:implies [:literal true] [:literal false]]]]
    (doseq [expression cases]
      (testing (pr-str expression)
        (let [readers (if (re-find #":read " (pr-str expression))
                        #{request-status}
                        #{})]
          (is (= :syntax-only
                 (:checking (rule/check-rule! (declaration readers expression))))))))))

(deftest compound-realistic-rule-has-closed-declared-read-set
  (let [candidate (declaration
                   #{request-status result-score request-owner}
                   [:and
                    [:in [:read request-status]
                     [:literal #{:new :active :done}]]
                    [:> [:read result-score] [:literal 7]]
                    [:implies [:present [:read request-owner]]
                     [:not= [:read request-owner] [:literal nil]]]])
        result (accepted! candidate)]
    (is (= 16 (:nodes result)))
    (is (= (:readers candidate) (:readers result)))))

(deftest repeated-reads-use-a-set-not-an-incremental-read-count
  (let [candidate (declaration #{result-score}
                               [:and [:= [:read result-score] [:literal 7]]
                                [:> [:read result-score] [:literal 0]]])]
    (is (= #{result-score} (:readers (accepted! candidate))))
    (is (= 7 (:nodes (rule/check-rule! candidate))))))

(deftest literals-accept-only-closed-scalar-and-finite-set-values
  (doseq [value [nil true false 0 -3 3N (bigint 3)
                 "" "hello\nworld" :state :state/active
                 #{:new :active} #{1 2 3} #{nil true false 2N "ok" :ok}]]
    (testing (str "Allowed literal " (pr-str value))
      (is (= :syntax-only
             (:checking (rule/check-rule! (declaration [:literal value])))))))
  (doseq [value [1.0 1/2 0.1M \x 'plus (Object.)
                 {:status :active} [:mutable] '(1 2)
                 (byte-array 1) (fn [] :unexpected)]]
    (testing (str "Refused literal of type " (type value))
      (rejects! (declaration [:literal value]) :invalid-literal
                [:expression 1]))))

(deftest literal-set-rejects-nonplain-unbounded-and-opaque-values
  (doseq [[value reason] [[#{} :invalid-literal-set]
                          [(set (range 257)) :invalid-literal-set]
                          [(sorted-set :x :y) :unsupported-collection]
                          [(java.util.HashSet. [:x :y]) :invalid-literal]
                          [#{[:not-a-scalar]} :invalid-literal]
                          [#{1.0 :active} :invalid-literal]
                          [#{'symbol} :invalid-literal]]]
    (testing (pr-str value)
      (rejects! (declaration [:literal value]) reason)))
  (is (= :syntax-only
         (:checking (rule/check-rule!
                     (declaration [:literal (set (range 256))]))))))

(deftest literals-enforce-text-and-mathematical-integer-length-limits
  (doseq [value [(apply str (repeat 2049 "x"))
                 (keyword (apply str (repeat 2049 "k")))
                 (bigint (apply str (repeat 2049 "9")))]]
    (rejects! (declaration [:literal value]) :literal-size-limit
              [:expression 1]))
  (is (= :syntax-only
         (:checking (rule/check-rule!
                     (declaration [:literal (apply str (repeat 2048 "x"))])))))
  (is (= :syntax-only
         (:checking (rule/check-rule!
                     (declaration [:literal (bigint (apply str (repeat 2048 "9")))]))))))

(deftest exact-declaration-shape-and-nonexecutable-map-required
  (doseq [candidate [nil 1 :rule [] #{} '(1 2)
                     (sorted-map :readers #{} :expression [:literal true])]]
    (rejects! candidate :expected-declaration-map []))
  (doseq [candidate [{:readers #{}}
                     {:expression [:literal true]}
                     {:readers #{} :expression [:literal true] :valid? true}
                     {:readers #{} :expression [:literal true] :proof-status :proved}
                     {:readers #{} :expression [:literal true] :certificate {:ok true}}
                     {:readers #{} :expression [:literal true] :evaluate! (fn [] true)}]]
    (rejects! candidate :wrong-declaration-keys []))
  (rejects! (with-meta (declaration [:literal true]) {:proof true})
            :metadata-not-allowed []))

(deftest root-operator-must-be-a-known-keyword
  (doseq [[expression reason] [[[] :invalid-expression-arity]
                               [['= [:literal 1] [:literal 1]] :invalid-operator]
                               [["and" [:literal true] [:literal true]] :invalid-operator]
                               [[nil [:literal true]] :invalid-operator]
                               [[:execute [:literal true]] :unknown-operator]
                               [[:eval [:literal "(+ 1 2)"]] :unknown-operator]
                               [[:invoke :request/status] :unknown-operator]
                               [[:foo/function :x] :unknown-operator]
                               [[:literal true :extra] :invalid-arity]]]
    (rejects! (declaration expression) reason [:expression])))

(deftest every-operator-enforces-its-arity
  (doseq [op [:literal :read :present :not]]
    (testing (str "Unary operator " op)
      (rejects! (declaration [op]) :invalid-arity [:expression])
      (rejects! (declaration [op [:literal true] [:literal true]])
                :invalid-arity [:expression])))
  (doseq [op [:and :or]]
    (testing (str "N-ary operator " op)
      (rejects! (declaration [op]) :invalid-arity [:expression])
      (rejects! (declaration [op [:literal true]])
                :invalid-arity [:expression])))
  (doseq [op [:= :not= :< :<= :> :>= :in :implies]]
    (testing (str "Binary operator " op)
      (rejects! (declaration [op]) :invalid-arity [:expression])
      (rejects! (declaration [op [:literal 1]]) :invalid-arity [:expression])
      (rejects! (declaration [op [:literal 1] [:literal 2] [:literal 3]])
                :invalid-arity [:expression]))))

(deftest only-explicitly-declared-qualified-readers-are-allowed
  (let [expression [:read request-status]]
    (rejects! (declaration #{} expression)
              :undeclared-reader [:expression])
    (rejects! (declaration #{result-score} expression)
              :undeclared-reader [:expression])
    (rejects! (declaration #{request-status result-score} expression)
              :unused-reader-declaration [:readers])
    (is (= #{result-score}
           (:unused (rejection #(rule/check-rule!
                                  (declaration #{request-status result-score}
                                               expression))))))
    (accepted! (declaration #{request-status} expression)))
  (doseq [bad [:status "request/status" 'request/status 7 nil false
               [:request :status]]]
    (rejects! (declaration #{request-status} [:read bad])
              :invalid-reader-id [:expression]))
  (doseq [bad [nil [] [:literal true] '( :request/status)]]
    (rejects! (declaration bad [:literal true])
              :invalid-reader-declarations [:readers]))
  (rejects! (declaration #{request-status} [:literal true])
            :unused-reader-declaration [:readers])
  (doseq [bad [#{:unqualified} #{"request/status"}]]
    (rejects! (declaration bad [:literal true])
              :invalid-reader-id [:readers])))

(deftest extra-reader-declarations-are-not-allowed
  (rejects! (declaration #{request-owner} [:literal true])
            :unused-reader-declaration [:readers])
  (rejects! (declaration #{request-owner result-score}
                         [:present [:read request-owner]])
            :unused-reader-declaration [:readers])
  (rejects! (declaration #{:unqualified} [:literal true])
            :invalid-reader-id [:readers])
  (rejects! (declaration #{"status"} [:literal true])
            :invalid-reader-id [:readers])
  (rejects! (declaration (sorted-set request-status)
                         [:read request-status])
            :invalid-reader-declarations [:readers]))

(deftest present-requires-a-direct-read-and-has-presence-not-truthiness-syntax
  (accepted! (declaration #{request-owner} [:present [:read request-owner]]))
  (doseq [expression [[:present [:literal nil]]
                      [:present [:= [:literal 1] [:literal 1]]]
                      [:present [:not [:literal false]]]
                      [:present :request/owner]
                      [:present [:unknown [:read request-owner]]]]]
    (rejects! (declaration #{request-owner} expression)
              :present-requires-reader [:expression]))
  (rejects! (declaration #{request-owner} [:present [:read request-owner 1]])
            :invalid-arity [:expression 1]))

(deftest expressions-must-be-vectors-at-every-position
  (doseq [expression [nil true "true" '( :literal true)
                      {:literal true} #{:literal true} (Object.)]]
    (rejects! (declaration expression) :expected-expression-vector [:expression]))
  (doseq [child [true nil '( :literal true) {:literal 1}
                 (fn [] true) [:literal [:invalid-value]]]]
    (let [reason (if (and (vector? child) (= :literal (first child)))
                   :invalid-literal
                   :expected-expression-vector)]
      (rejects! (declaration [:not child]) reason)))
  (accepted! (declaration (subvec [:ignored :literal true] 1))))

(deftest nested-metadata-does-not-become-a-hidden-execution-channel
  (rejects! (declaration (with-meta [:literal true] {:proved? true}))
            :metadata-not-allowed [:expression])
  (rejects! (declaration [:not (with-meta [:literal true] {:auth :admin})])
            :metadata-not-allowed [:expression 1])
  (rejects! (declaration [:literal (with-meta #{1 2} {:eval! (fn [] true)})])
            :metadata-not-allowed [:expression 1])
  (rejects! (declaration
             (with-meta #{request-status} {:proof true})
             [:read request-status])
            :metadata-not-allowed [:readers]))

(deftest expression-arity-limit-is-closed-at-256-elements
  (let [minimum [:and [:literal true] [:literal true]]
        largest (into [:and] (repeat 255 [:literal true]))
        excessive (into [:and] (repeat 256 [:literal true]))]
    (accepted! (declaration minimum))
    (is (= 256 (count largest)))
    (is (= 256 (:nodes (rule/check-rule! (declaration largest)))))
    (rejects! (declaration excessive) :invalid-expression-arity [:expression])))

(deftest depth-boundary-is-enforced-not-just-recursion-safe
  (let [wrap-not (fn [depth] (nth (iterate (fn [e] [:not e])
                                         [:literal true]) depth))]
    (is (= :syntax-only
           (:checking (rule/check-rule! (declaration (wrap-not 64))))))
    (rejects! (declaration (wrap-not 65)) :depth-limit)))

(deftest node-budget-is-global-across-the-expression-tree
  (let [branch (into [:and] (repeat 43 [:literal true]))
        within (into [:and] (repeat 180 branch))
        exceeds (into [:and] (repeat 200 branch))]
    (is (= 7921 (:nodes (rule/check-rule! (declaration within)))))
    (rejects! (declaration exceeds) :node-limit)))

(deftest reader-declarations-have-a-global-size-limit
  (let [ids (mapv (comp keyword (partial str "field")) (range 257))
        namespaced (mapv (fn [id] (keyword "record" (name id))) ids)
        huge (set namespaced)]
    (rejects! (declaration huge [:literal true]) :reader-limit [:readers])
    (is (= 257 (count huge)))))

(deftest type-invalid-but-syntactically-valid-expressions-remain-unproved
  ;; Intentionally MUST be accepted at v1219. Type checking is v1221 and
  ;; evaluation is v1223; rejecting now would conflate these obligations.
  (doseq [expression [[:< [:literal :active] [:literal 7]]
                      [:and [:literal 13] [:literal 0]]
                      [:not [:literal nil]]
                      [:in [:literal 5] [:literal :not-a-set]]
                      [:= [:literal 1] [:literal "1"]]]]
    (let [result (rule/check-rule! (declaration expression))]
      (is (= :not-performed (:typing result)))
      (is (= :not-performed (:evaluation result)))
      (is (= :not-proved (:proof-status result)))
      (is (= :none (:authority result))))))

(deftest reports-cannot-be-submitted-as-declarations-or-self-certify
  (let [declaration (declaration [:literal true])
        report (rule/check-rule! declaration)]
    (rejects! report :wrong-declaration-keys [])
    (doseq [mutation [{:valid? true}
                      {:admission :admitted}
                      {:proof-status :proved}
                      {:checking :kernel-checked}
                      {:certificate {:valid? true}}
                      {:authority :proof}]]
      (rejects! (merge declaration mutation) :wrong-declaration-keys []))
    (is (= report (rule/check-rule! declaration)))))
