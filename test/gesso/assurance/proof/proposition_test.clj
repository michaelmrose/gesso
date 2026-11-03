(ns gesso.assurance.proof.proposition-test
  "v1230: adversarial regressions for closed, typed, subject-bound mathematical
   proposition descriptions. Syntax, typing, and structural identities never
   amount to proof, assumption discharge, or authoritative runtime admission."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.proposition :as proposition]))

(def ^:private subject
  {:kind :operation
   :id :example/advance
   :interface {:operation :example/advance
               :guards [[:> [:field :result/score] 7]]
               :effects [[:set :result/score [:input :amount]]]}})
(def ^:private score :result/score)
(def ^:private status :result/status)
(def ^:private int-type {:type :integer})
(def ^:private status-type {:type :enum :name :example/status
                            :values #{:new :active :done}})

(defn- atom-rule [readers expression]
  [:atom {:readers readers :expression expression}])
(defn- gt [n]
  (atom-rule #{score} [:> [:read score] [:literal n]]))
(defn- gte [n]
  (atom-rule #{score} [:>= [:read score] [:literal n]]))
(def ^:private base
  {:subject subject
   :variables {score int-type}
   :proposition [:implies (gt 7) (gte 7)]})
(defn- check! [declaration]
  (proposition/describe-proposition! declaration))
(defn- error-data [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))
(defn- rejects! [declaration]
  (let [error (error-data #(check! declaration))]
    (is (map? error) (str "Expected rejection: " (pr-str declaration)))
    (is (qualified-keyword? (:error/type error)) (pr-str error))
    error))
(defn- direct-rejects! [declaration reason]
  (let [error (rejects! declaration)]
    (is (= :gesso.assurance.proof.proposition/invalid-proposition
           (:error/type error)) (pr-str error))
    (is (= reason (:reason error)) (pr-str error))
    (is (vector? (:at error)) (pr-str error))
    error))
(defn- unadmitted! [report]
  (is (= :gesso.assurance.proof.proposition/unadmitted-proposition (:type report)))
  (is (= 1 (:version report)))
  (is (= :syntax-and-types (:checking report)))
  (is (= :not-performed (:evaluation report)))
  (is (= :not-proved (:proof-status report)))
  (is (= :not-performed (:assumption-discharge report)))
  (is (= :not-admitted (:admission report)))
  (is (= :not-established (:runtime-enforcement report)))
  (is (= :none (:authority report)))
  (is (= #{:type :version :fingerprint :subject-fingerprint :semantics
           :variables :nodes :atoms :atomic-rule-nodes :checking
           :evaluation :proof-status :assumption-discharge :admission
           :runtime-enforcement :authority}
         (set (keys report))))
  (doseq [k [:valid? :proved? :certificate :kernel-checked
             :admission-handle :discharged? :transaction-authority]]
    (is (not (contains? report k))))
  report)

(deftest typed-proposition-is-deterministic-subject-bound-and-unadmitted
  (let [result (unadmitted! (check! base))]
    (is (= result (check! base)))
    (is (= #{score} (:variables result)))
    (is (= 2 (:atoms result)))
    (is (= 3 (:nodes result)))
    (is (= :implies (get-in result [:semantics :proposition 0])))
    (is (= (:subject-fingerprint result)
           (get-in result [:semantics :subject-fingerprint])))
    (is (= :operation (get-in result [:semantics :subject-kind])))
    (is (= :example/advance (get-in result [:semantics :subject-id])))
    (is (re-matches #"sha256:[0-9a-f]{64}" (:fingerprint result)))
    (is (not= (:fingerprint result) (:subject-fingerprint result)))))

(deftest closed-boolean-connectives-have-descriptions-not-proofs
  (let [yes (atom-rule #{} [:literal true])]
    (doseq [p [[:true] [:false] yes [:not [:true]]
               [:and [:true] [:false]]
               [:and [:true] yes [:true]]
               [:or [:false] [:true]]
               [:or [:false] yes [:false]]
               [:implies [:false] [:true]]
               [:not [:implies yes [:not [:false]]]]]]
      (testing (pr-str p)
        (let [result (check! {:subject subject :variables {} :proposition p})]
          (is (= :not-proved (:proof-status result)))
          (is (= :not-admitted (:admission result)))
          (is (= (first p) (first (get-in result [:semantics :proposition])))))))))

(deftest exact-variable-footprint-and-independent-atom-typing
  (let [p [:and (gt 7)
           (atom-rule #{status} [:in [:read status] [:literal #{:new :done}]])]
        decl (assoc base :variables {score int-type status status-type}
                         :proposition p)]
    (is (= 2 (:atoms (check! decl))))
    (is (= #{score status} (:variables (check! decl))))
    (direct-rejects! (assoc decl :variables {score int-type}) :unbound-variable)
    (direct-rejects! (assoc decl :variables {score int-type status status-type
                                           :unused/field int-type}) :unused-variable)
    (rejects! (assoc-in decl [:variables status] int-type))
    (rejects! (assoc-in decl [:proposition 2 1 :readers] #{score}))
    (rejects! (assoc-in decl [:proposition 1 1 :readers] #{}))))

(deftest ill-typed-ordering-equality-and-membership-are-rejected
  (doseq [expr [[:> [:read score] [:literal :active]]
                [:= [:read score] [:literal "7"]]
                [:> [:literal "8"] [:literal "7"]]
                [:< [:literal nil] [:literal 5]]
                [:in [:read score] [:literal #{:new :done}]]]]
    (testing (pr-str expr)
      (rejects! (assoc base :proposition (atom-rule #{score} expr)))))
  (rejects! (assoc base :variables {score status-type}
                        :proposition (gt 7)))
  (rejects! (assoc base :variables {score status-type}
                        :proposition (atom-rule #{score}
                               [:= [:read score] [:literal 7]]))))

(deftest unrelated-nominal-enums-never-share-type-authority
  (let [l :result/status r :request/status
        atom (atom-rule #{l r} [:= [:read l] [:read r]])
        decl (assoc base :variables {l status-type r status-type}
                         :proposition atom)]
    (is (= 1 (:atoms (check! decl))))
    (rejects! (assoc-in decl [:variables r :name] :another/status))
    (rejects! (assoc-in decl [:variables r :values]
                        #{:new :done :cancelled}))))

(deftest optional-guard-in-one-atom-does-not-refine-another
  (let [presence (atom-rule #{score} [:present [:read score]])
        safe (atom-rule #{score}
                        [:and [:present [:read score]]
                         [:> [:read score] [:literal 7]]])
        decl (assoc base :variables {score {:type :integer :optional? true}})]
    (rejects! (assoc decl :proposition [:and presence (gt 7)]))
    (rejects! (assoc decl :proposition [:implies presence (gt 7)]))
    (is (= 1 (:atoms (check! (assoc decl :proposition safe)))))
    (is (= 2 (:atoms (check! (assoc decl :proposition [:and safe presence])))))))

(deftest nullable-guard-in-one-atom-does-not-refine-another
  (let [nonnull (atom-rule #{score}
                           [:not= [:read score] [:literal nil]])
        safe (atom-rule #{score}
                        [:and [:not= [:read score] [:literal nil]]
                         [:> [:read score] [:literal 7]]])
        decl (assoc base :variables {score {:type :integer :nullable? true}})]
    (is (= 1 (:atoms (check! (assoc decl :proposition safe)))))
    (rejects! (assoc decl :proposition (gt 7)))
    (rejects! (assoc decl :proposition [:and nonnull (gt 7)]))
    (rejects! (assoc decl :proposition [:implies nonnull (gt 7)]))))

(deftest optional-nullable-read-needs-both-internal-guards
  (let [decl (assoc base :variables {score {:type :integer
                                            :optional? true :nullable? true}})
        safe (atom-rule #{score}
                        [:and [:present [:read score]]
                         [:not= [:read score] [:literal nil]]
                         [:> [:read score] [:literal 7]]])]
    (is (= 1 (:atoms (check! (assoc decl :proposition safe)))))
    (rejects! (assoc decl :proposition (atom-rule #{score}
                       [:and [:present [:read score]]
                        [:> [:read score] [:literal 7]]])))
    (rejects! (assoc decl :proposition (atom-rule #{score}
                       [:and [:not= [:read score] [:literal nil]]
                        [:> [:read score] [:literal 7]]])))))

(deftest invalid-source-keys-cannot-inject-checker-authority
  (doseq [k [:fingerprint :valid? :proved? :certificate :handle
             :checking :admission :runtime-enforcement :assumption-discharge]
          v [true {:proof :checked} "sha256:fake"]]
    (direct-rejects! (assoc base k v) :wrong-declaration-keys))
  (doseq [k [:subject :variables :proposition]]
    (direct-rejects! (dissoc base k) :wrong-declaration-keys))
  (direct-rejects! (assoc base :variables #{score}) :invalid-variable-map))

(deftest copied-typed-description-cannot-be-resubmitted-as-proof
  (let [desc (check! base)]
    (rejects! desc)
    (rejects! (assoc desc :proof-status :proved :admission :admitted
                          :authority :commit))
    (rejects! (assoc base :proposition
                     [:atom (assoc (second (gt 7)) :valid? true)]))))

(deftest malformed-connectives-have-explicit-errors
  (doseq [[p reason]
          [[[] :invalid-proposition-arity]
           [[:mystery] :unknown-connective]
           [[:true 1] :invalid-arity]
           [[:false [:true]] :invalid-arity]
           [[:atom] :invalid-arity]
           [[:atom {} {}] :invalid-arity]
           [[:not] :invalid-arity]
           [[:not [:true] [:false]] :invalid-arity]
           [[:and] :invalid-arity]
           [[:and [:true]] :invalid-arity]
           [[:or [:true]] :invalid-arity]
           [[:implies [:true]] :invalid-arity]
           [[:implies [:true] [:false] [:true]] :invalid-arity]
           [[:and [:true] nil] :expected-proposition-vector]
           [[:not :true] :expected-proposition-vector]
           [[:or [:true] '(:false)] :expected-proposition-vector]]]
    (testing (pr-str p)
      (direct-rejects! {:subject subject :variables {} :proposition p}
                       reason))))

(deftest nested-unknown-connectives-and-bad-atomic-rules-rejected
  (direct-rejects! {:subject subject :variables {}
                    :proposition [:and [:true] [:invented [:false]]]}
                   :unknown-connective)
  (doseq [expr [[:invoke :unsafe/function] [:literal 3] [:literal nil]]]
    (rejects! {:subject subject :variables {}
               :proposition (atom-rule #{} expr)})))

(deftest changing-semantic-subject-invalidates-both-identities
  (let [original (check! base)
        mutants [(assoc-in base [:subject :id] :example/other-operation)
                 (assoc-in base [:subject :interface :operation] :example/other)
                 (assoc-in base [:subject :interface :guards 0 2] 8)
                 (update-in base [:subject :interface :effects] conj
                            [:set :result/status :done])]]
    (doseq [m mutants]
      (let [changed (check! m)]
        (is (not= (:subject-fingerprint original)
                  (:subject-fingerprint changed)))
        (is (not= (:fingerprint original) (:fingerprint changed)))
        (unadmitted! changed)))))

(deftest changes-to-connective-constants-and-operand-order-invalidate
  (let [original (:fingerprint (check! base))
        forms [[:implies (gt 7) (gte 8)]
               [:implies (gt 8) (gte 7)]
               [:implies (gte 7) (gt 7)]
               [:and (gt 7) (gte 7)]
               [:or (gt 7) (gte 7)]
               [:not (gt 7)]
               [:implies (gt 7) [:not [:not (gte 7)]]]]]
    (doseq [p forms]
      (is (not= original (:fingerprint (check! (assoc base :proposition p)))))))
  ;; Making a read optional must not recycle the prior fingerprint: it renders
  ;; the unguarded rule ill typed and thus invalid outright.
  (rejects! (assoc-in base [:variables score :optional?] true)))

(deftest normalization-matches-only-explicitly-supported-equivalences
  (let [fingerprint (:fingerprint (check! base))
        bigint-form (-> base
                        (assoc-in [:proposition 1 1 :expression 2 1] 7N)
                        (assoc-in [:proposition 2 1 :expression 2 1] 7N))
        explicit-false (assoc-in base [:variables score]
                                 {:type :integer :optional? false :nullable? false})]
    (is (= fingerprint (:fingerprint (check! bigint-form))))
    (is (= fingerprint (:fingerprint (check! explicit-false))))))

(deftest enum-domain-names-and-values-are-identity-bearing
  (let [decl {:subject subject
              :variables {status status-type}
              :proposition (atom-rule #{status}
                                [:in [:read status] [:literal #{:new :done}]])}
        fingerprint (:fingerprint (check! decl))]
    (is (= fingerprint (:fingerprint
                        (check! (assoc-in decl [:variables status :values]
                                          (into #{} [:done :active :new]))))))
    (is (not= fingerprint (:fingerprint
                           (check! (assoc-in decl [:variables status :name]
                                             :another/status)))))
    (is (not= fingerprint (:fingerprint
                           (check! (assoc-in decl [:variables status :values]
                                             #{:new :active :done :paused})))))))

(deftest malformed-variable-descriptors-and-subjects-rejected
  (direct-rejects! (assoc base :variables (java.util.HashMap. (:variables base)))
                   :invalid-variable-map)
  (direct-rejects! (assoc base :variables {'score int-type})
                   :invalid-variable-id)
  (rejects! (assoc base :variables {score {:type :integer :optional? :maybe}}))
  (rejects! (assoc-in base [:subject :kind] :unknown-kind))
  (rejects! (assoc-in base [:subject :id] :unqualified))
  (rejects! (assoc-in base [:subject :interface :callback] (fn [] true))))

(deftest hidden-metadata-and-executable-values-rejected
  (direct-rejects! (with-meta base {:checked? true}) :metadata-not-allowed)
  (direct-rejects! (assoc base :variables
                          (with-meta (:variables base) {:proved? true}))
                   :metadata-not-allowed)
  (direct-rejects! (assoc base :proposition
                          (with-meta (:proposition base) {:proved? true}))
                   :metadata-not-allowed)
  (let [executed (atom 0)
        callback (fn [] (swap! executed inc))]
    (rejects! (assoc base :proposition (atom-rule #{} [:literal callback])))
    (is (zero? @executed)))
  (rejects! (assoc base :proposition '(:true))))

(deftest manifestly-false-formulas-are-still-not-proved-or-evaluated
  (doseq [p [[:false] [:not [:true]] [:implies [:true] [:false]]
             [:and [:true] [:false]] (atom-rule #{} [:literal false])]]
    (let [r (check! {:subject subject :variables {} :proposition p})]
      (is (= :not-proved (:proof-status r)))
      (is (= :not-performed (:evaluation r)))
      (is (= :not-admitted (:admission r)))
      (is (not (contains? r :truth))))))

(deftest mutation-matrix-never-reuses-original-identity
  (let [fingerprint (:fingerprint (check! base))]
    (doseq [n (range -50 101) :when (not= n 7)]
      (is (not= fingerprint
                (:fingerprint
                 (check! (assoc-in base [:proposition 1 1 :expression 2 1]
                                   n))))
          (str "changed threshold " n)))))

(deftest proposition-resource-limits-are-enforced
  (let [deep (nth (iterate (fn [p] [:not p]) [:true]) 66)]
    (direct-rejects! {:subject subject :variables {} :proposition deep}
                     :depth-limit))
  (direct-rejects! {:subject subject :variables {}
                    :proposition (into [:and] (repeat 128 [:true]))}
                   :invalid-proposition-arity)
  (let [atom (atom-rule #{} [:literal true])
        children (repeat 127 atom)
        too-many (into [:and] (repeat 5 (into [:and] children)))]
    (direct-rejects! {:subject subject :variables {}
                      :proposition too-many} :atom-limit)))

(deftest number-of-variables-is-bounded
  (let [variables (into {} (map (fn [i] [(keyword "input" (str "field" i))
                                       int-type])) (range 257))]
    (direct-rejects! {:subject subject :variables variables
                      :proposition [:true]} :variable-limit)))
