(ns gesso.model.relational-invariant-test
  "Permanent regressions for finite, source-interpreted referential rules.

   The fixtures describe an illustrative request/membership relationship,
   not HumanHelp production policy. These tests establish interpretation of
   explicitly declared relations and their interaction with the bounded
   transaction and operation-family checkers. They do not certify omitted
   business rules, arbitrary callbacks, real XTDB enforcement, or complete
   operation and argument domains."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.model.command :as command]
            [gesso.model.invariant-check :as invariant]
            [gesso.model.operation-family :as family]
            [gesso.model.relational-invariant :as relational]
            [gesso.model.tx-model :as reference]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(def ^:private request-target [:request :request-1])
(def ^:private membership-target [:membership :membership-1])

(def ^:private request
  {:xt/id :request-1 :revision 0 :created-at :t0 :updated-at :t0
   :member-id nil :status :open})

(def ^:private membership
  {:xt/id :membership-1 :revision 0 :created-at :t0 :updated-at :t0
   :active? true :organization-id :org-1})

(def ^:private initial-state
  {request-target request
   membership-target membership})

(def ^:private active-membership-rule
  {:id :request/active-membership
   :source-kind :request
   :source-where {}
   :reference-attr :member-id
   :allow-unset? true
   :target-kind :membership
   :target-where {:active? true}})

(def ^:private strict-membership-rule
  (assoc active-membership-rule :allow-unset? false))

(defn- with-owner [state owner]
  (assoc-in state [request-target :member-id] owner))

(defn- inactive-membership [state]
  (assoc-in state [membership-target :active?] false))

(defn- update-document [before changes]
  (-> before
      (merge changes)
      (update :revision inc)
      (assoc :updated-at :t1)))

(defn- update-command [kind action before changes]
  (command/update-command kind action before
                          (update-document before changes) version))

(defn- plan
  ([commands] (plan commands []))
  ([commands guards]
   {:commands (vec commands) :guards (vec guards) :emit false}))

(def ^:private claim
  (plan [(update-command :request :claim request {:member-id :membership-1})]
        [(command/guard :membership membership version)]))

(def ^:private unsafe-revocation
  (plan [(update-command :membership :revoke membership {:active? false})]))

(def ^:private release-and-revoke
  (plan [(update-command :request :release request {:member-id nil})
         (update-command :membership :revoke membership {:active? false})]))

(defn- error-of [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e e)))

(defn- error-kind [f]
  (some-> (error-of f) ex-data :error/kind))

(defn- inspect
  ([state] (relational/inspect state active-membership-rule))
  ([state rule] (relational/inspect state rule)))

(defn- reasons [report]
  (mapv :reason (:violations report)))

(defn- claim-family []
  {:id :request/claim
   :principals [:helper/a]
   :arguments [{:request-id :request-1}]
   :enabled? (fn [state _ _]
               (and (nil? (get-in state [request-target :member-id]))
                    (true? (get-in state [membership-target :active?]))))
   :prepare (fn [state _ _]
              (plan [(update-command :request :claim
                                     (get state request-target)
                                     {:member-id :membership-1})]
                    [(command/guard :membership
                                    (get state membership-target) version)]))})

(defn- revoke-family [release?]
  {:id :membership/revoke
   :principals [:manager/a]
   :arguments [{:membership-id :membership-1}]
   :enabled? (fn [state _ _]
               (true? (get-in state [membership-target :active?])))
   :prepare (fn [state _ _]
              (plan (cond-> [(update-command :membership :revoke
                                             (get state membership-target)
                                             {:active? false})]
                      release?
                      (conj (update-command :request :release
                                            (get state request-target)
                                            {:member-id nil})))))})

(deftest assurance-scope-separates-owned-rules-from-trusted-completeness-test
  (let [{:keys [rule-engine checked assumed not-established]}
        relational/assurance-scope]
    (is (= :gesso.model.relational-invariant/finite-reference-v1 rule-engine))
    (doseq [property [:source-owned-reference-rule-interpretation
                     :explicit-source-selection
                     :presence-of-reference-attributes
                     :target-document-existence
                     :required-target-attribute-equalities
                     :complete-declared-rule-evaluation-over-modeled-documents]]
      (is (contains? checked property) (str property)))
    (doseq [premise [:complete-finite-modeled-document-state
                     :truthful-business-meaning-of-declared-relations
                     :serial-atomic-commit-reference-semantics
                     :pure-total-truthful-modeled-operation-builders]]
      (is (contains? assumed premise) (str premise)))
    (doseq [limit [:completeness-of-business-rules
                   :completeness-of-documents-or-argument-domains
                   :authorization-of-real-principals
                   :correctness-of-real-application-callbacks
                   :real-xtdb-constraint-enforcement
                   :unbounded-invariant-preservation
                   :verified-projection-or-browser-semantics
                   :application-certification]]
      (is (contains? not-established limit) (str limit)))))

(deftest optional-unset-reference-is-valid-test
  (let [report (inspect initial-state)]
    (is (:holds? report))
    (is (empty? (:violations report)))
    (is (= 1 (:inspected-source-count report)))
    (is (= :request/active-membership (:id report)))
    (is (= relational/assurance-scope (:scope report)))))

(deftest required-unset-reference-violates-test
  (let [report (inspect initial-state strict-membership-rule)]
    (is (false? (:holds? report)))
    (is (= [:unset-reference] (reasons report)))
    (is (= request-target (get-in report [:violations 0 :source-target])))))

(deftest present-active-target-satisfies-relation-test
  (let [report (inspect (with-owner initial-state :membership-1))]
    (is (:holds? report))
    (is (= 1 (:inspected-source-count report)))
    (is (empty? (:violations report)))))

(deftest absent-target-is-distinct-from-invalid-target-test
  (let [missing (inspect (with-owner initial-state :unregistered))
        inactive (inspect (inactive-membership
                           (with-owner initial-state :membership-1)))]
    (is (= [:missing-target] (reasons missing)))
    (is (= [:membership :unregistered]
           (get-in missing [:violations 0 :target])))
    (is (= [:target-requirements-not-satisfied] (reasons inactive)))
    (is (= membership-target (get-in inactive [:violations 0 :target])))))

(deftest absent-required-target-attribute-is-not-treated-as-nil-test
  (let [rule (assoc active-membership-rule :target-where {:active? nil})
        missing (inspect (-> initial-state
                             (with-owner :membership-1)
                             (update membership-target dissoc :active?)) rule)
        present-nil (inspect (-> initial-state
                                (with-owner :membership-1)
                                (assoc-in [membership-target :active?] nil)) rule)]
    (is (= [:target-requirements-not-satisfied] (reasons missing)))
    (is (:holds? present-nil))))

(deftest target-conditions-require-all-attributes-test
  (let [rule (assoc active-membership-rule
                    :target-where {:active? true :organization-id :org-1})
        state (with-owner initial-state :membership-1)]
    (is (:holds? (inspect state rule)))
    (is (= [:target-requirements-not-satisfied]
           (reasons (inspect (assoc-in state [membership-target :organization-id]
                                       :other-org) rule))))
    (is (= [:target-requirements-not-satisfied]
           (reasons (inspect (update state membership-target
                                     dissoc :organization-id) rule))))))

(deftest incorrect-target-kind-does-not-alias-same-id-test
  (let [state (with-owner initial-state :membership-1)
        rule (assoc active-membership-rule :target-kind :organization)]
    (is (= [:missing-target] (reasons (inspect state rule))))
    (is (= [:organization :membership-1]
           (get-in (inspect state rule) [:violations 0 :target])))))

(deftest missing-reference-attribute-never-counts-as-optional-unset-test
  (let [state (update initial-state request-target dissoc :member-id)
        report (inspect state)]
    (is (= [:missing-reference-attribute] (reasons report)))
    (is (false? (:holds? report)))
    (is (= 1 (:inspected-source-count report)))))

(deftest invalid-reference-identity-is-not-coerced-test
  (doseq [invalid [(Object.) {:id :membership-1} [:membership-1]
                   3.5]]
    (let [report (inspect (with-owner initial-state invalid))]
      (is (= [:invalid-reference-identity] (reasons report)) (pr-str (type invalid)))
      (is (false? (:holds? report)) (pr-str (type invalid))))))

(deftest source-selection-checks-present-attributes-before-filtering-test
  (let [rule (assoc active-membership-rule :source-where {:status :claimed})
        missing (inspect (update initial-state request-target dissoc :status) rule)
        not-selected (inspect initial-state rule)
        selected (inspect (-> initial-state
                              (assoc-in [request-target :status] :claimed)
                              (with-owner :membership-1)) rule)]
    (is (= [:missing-source-selector-attribute] (reasons missing)))
    (is (= :status (get-in missing [:violations 0 :attribute])))
    (is (= 0 (:inspected-source-count missing)))
    (is (:holds? not-selected))
    (is (= 0 (:inspected-source-count not-selected)))
    (is (:holds? selected))
    (is (= 1 (:inspected-source-count selected)))))

(deftest explicitly-nil-selector-differs-from-missing-selector-test
  (let [rule (assoc active-membership-rule :source-where {:status nil})
        matching (assoc-in initial-state [request-target :status] nil)
        missing (update initial-state request-target dissoc :status)]
    (is (:holds? (inspect matching rule)))
    (is (= 1 (:inspected-source-count (inspect matching rule))))
    (is (= [:missing-source-selector-attribute]
           (reasons (inspect missing rule))))))

(deftest nonmatching-source-does-not-have-its-reference-checked-test
  (let [rule (assoc active-membership-rule :source-where {:status :claimed})
        state (-> initial-state
                  (assoc-in [request-target :member-id] :nonexistent)
                  (assoc-in [request-target :status] :open))]
    (is (:holds? (inspect state rule)))
    (is (= 0 (:inspected-source-count (inspect state rule))))))

(deftest missing-source-table-is-vacuously-true-not-evidence-of-coverage-test
  (let [report (inspect (dissoc initial-state request-target))]
    (is (:holds? report))
    (is (zero? (:inspected-source-count report)))
    (is (empty? (:violations report)))))

(deftest all-matching-sources-are-checked-with-deterministic-diagnostics-test
  (let [state (assoc initial-state
                     [:request :request-3]
                     {:xt/id :request-3 :member-id :missing-3 :status :open}
                     [:request :request-2]
                     {:xt/id :request-2 :member-id :missing-2 :status :open})
        report (inspect state)]
    (is (false? (:holds? report)))
    (is (= 3 (:inspected-source-count report)))
    (is (= [[:request :request-2] [:request :request-3]]
           (mapv :source-target (:violations report))))
    (is (= [:missing-target :missing-target] (reasons report)))
    (is (= (:violations report)
           (:violations (inspect (into (sorted-map) state)))))))

(deftest inspect-rejects-invalid-current-document-state-test
  (doseq [state [nil [] {[:request :request-1] nil}
                 {[:request :request-1] (assoc request :xt/id :incorrect)}
                 {[:request] request}]]
    (is (= :gesso.model.tx-model/invalid-state
           (some-> (error-of #(inspect state)) ex-data :error/type))
        (pr-str state))))

(deftest compile-rules-preserves-declared-identity-and-interpreted-behavior-test
  (let [rules (relational/compile-rules
               [active-membership-rule
                (assoc strict-membership-rule :id :request/membership-required)])
        valid (with-owner initial-state :membership-1)
        invalid (inactive-membership valid)]
    (is (= [:request/active-membership :request/membership-required]
           (mapv :id rules)))
    (is (every? #(= #{:id :holds?} (set (keys %))) rules))
    (is (every? (comp fn? :holds?) rules))
    (is (every? #((:holds? %) valid) rules))
    (is (every? #(false? ((:holds? %) invalid)) rules))
    (is (false? ((:holds? (second rules)) initial-state)))
    (is ((:holds? (first rules)) initial-state))))

(deftest invalid-declaration-collections-fail-before-compiling-test
  (doseq [invalid [nil [] {} () [active-membership-rule active-membership-rule]
                   [(assoc active-membership-rule :id :request/active-membership)
                    active-membership-rule]]]
    (is (= (if (and (vector? invalid) (= 2 (count invalid)))
             :duplicate-declaration
             :invalid-declarations)
           (error-kind #(relational/compile-rules invalid)))
        (pr-str invalid))))

(deftest declaration-shapes-and-identities-are-strict-test
  (let [mutations [(dissoc active-membership-rule :reference-attr)
                   (assoc active-membership-rule :extra true)
                   (assoc active-membership-rule :id :unqualified)
                   (assoc active-membership-rule :id "rule")
                   (assoc active-membership-rule :source-kind "request")
                   (assoc active-membership-rule :source-where nil)
                   (assoc active-membership-rule :reference-attr "member-id")
                   (assoc active-membership-rule :allow-unset? nil)
                   (assoc active-membership-rule :target-kind "membership")
                   (assoc active-membership-rule :target-where {})
                   (assoc active-membership-rule :target-where nil)]]
    (doseq [bad mutations]
      (is (= :invalid-declaration
             (error-kind #(relational/compile-rules [bad])))
          (pr-str bad)))))

(deftest selector-and-target-constraints-refuse-nonportable-values-test
  (doseq [[key value] [[:source-where {"status" :open}]
                       [:source-where {:status (Object.)}]
                       [:source-where {:status 1.5}]
                       [:source-where {:status [:open]}]
                       [:target-where {"active?" true}]
                       [:target-where {:active? (fn [] true)}]
                       [:target-where {:active? {:is true}}]
                       [:target-where {:active? 1.5}]]]
    (is (= :invalid-declaration
           (error-kind #(relational/compile-rules
                          [(assoc active-membership-rule key value)])))
        (pr-str [key value]))))

(deftest prepared-check-detects-cross-operation-revocation-test
  (let [result (relational/check-prepared initial-state
                                          [claim unsafe-revocation]
                                          [active-membership-rule])
        witness (first (:violations result))]
    (is (= :counterexample (:status result)))
    (is (= :request/active-membership (:rule-id witness)))
    (is (= :after-plan (:at witness)))
    (is (= [0 1] (:order witness)))
    (is (= [:committed :committed] (mapv :status (:trace witness))))
    (is (= relational/assurance-scope (:relational-scope result)))
    (is (= [active-membership-rule] (:relational-declarations result)))
    (is (= invariant/assurance-scope (:scope result)))))

(deftest prepared-check-accepts-atomic-release-and-revocation-test
  (let [result (relational/check-prepared initial-state
                                          [claim release-and-revoke]
                                          [active-membership-rule])]
    (is (= :holds-in-explored-model (:status result)))
    (is (= [] (:violations result)))
    (is (= 2 (get-in result [:exploration :orderings])))))

(deftest prepared-check-detects-initial-violation-test
  (let [result (relational/check-prepared (inactive-membership
                                           (with-owner initial-state :membership-1))
                                          [claim] [active-membership-rule])]
    (is (= :counterexample (:status result)))
    (is (= :initial-state (-> result :violations first :at)))
    (is (= :request/active-membership (-> result :violations first :rule-id)))))

(deftest prepared-check-fails-closed-on-unsupported-plan-test
  (let [unsupported {:commands (:commands claim)
                     :guards (:guards claim)
                     :assertions [{:assert true}]
                     :emit false}]
    (is (= :gesso.model.tx-model/unsupported-plan
           (some-> (error-of #(relational/check-prepared
                                initial-state [unsupported]
                                [active-membership-rule]))
                   ex-data :error/type)))
    (is (= :gesso.model.tx/invalid-assertions
           (some-> (error-of #(relational/check-prepared
                                initial-state
                                [(assoc unsupported :assertions ["SELECT 1"])]
                                [active-membership-rule]))
                   ex-data :error/type)))))

(deftest family-check-finds-shortest-claim-then-revoke-trace-test
  (let [result (relational/check-families initial-state
                                          [(claim-family) (revoke-family false)]
                                          [active-membership-rule]
                                          {:max-depth 2 :max-transitions 25})
        witness (first (:violations result))]
    (is (= :counterexample (:status result)))
    (is (= 2 (:depth witness)))
    (is (= [:request/claim :membership/revoke]
           (mapv :family-id (:family-trace witness))))
    (is (= [:helper/a :manager/a]
           (mapv :principal (:family-trace witness))))
    (is (= [{:request-id :request-1} {:membership-id :membership-1}]
           (mapv :arguments (:family-trace witness))))
    (is (= relational/assurance-scope (:relational-scope result)))
    (is (= family/family-assurance-scope (:family-scope result)))
    (is (= invariant/reachable-assurance-scope (:scope result)))))

(deftest family-check-accepts-atomic-release-and-revoke-test
  (let [result (relational/check-families initial-state
                                          [(claim-family) (revoke-family true)]
                                          [active-membership-rule]
                                          {:max-depth 2 :max-transitions 25})]
    (is (= :holds-in-explored-model (:status result)))
    (is (empty? (:violations result)))
    (is (= 2 (get-in result [:family-expansion :instance-count])))))

(deftest family-check-enforces-explicit-search-budget-test
  (is (= :exploration-budget-exhausted
         (error-kind #(relational/check-families initial-state
                                                 [(claim-family) (revoke-family false)]
                                                 [active-membership-rule]
                                                 {:max-depth 3
                                                  :max-transitions 1}))))
  (is (= :invalid-reachability-bound
         (error-kind #(relational/check-families initial-state
                                                 [(claim-family)]
                                                 [active-membership-rule]
                                                 {:max-depth 7
                                                  :max-transitions 20})))))

(deftest weak-or-omitted-declarations-cannot-prove-missing-business-policy-test
  (let [weak-rule (assoc active-membership-rule
                         :target-where {:organization-id :org-1})
        bad-state (inactive-membership
                   (with-owner initial-state :membership-1))]
    (is (:holds? (inspect bad-state weak-rule)))
    (is (= :holds-in-explored-model
           (:status (relational/check-prepared initial-state [claim]
                                                [weak-rule]))))
    (is (= :counterexample
           (:status (relational/check-prepared initial-state
                                                [claim unsafe-revocation]
                                                [active-membership-rule]))))))

(deftest interpretation-does-not-mutate-input-state-or-declaration-test
  (let [state (with-owner initial-state :membership-1)
        before-state state
        declarations [active-membership-rule]
        before-declarations declarations]
    (inspect state)
    (relational/compile-rules declarations)
    (relational/check-prepared initial-state [claim] declarations)
    (is (= before-state state))
    (is (= before-declarations declarations))
    (is (= initial-state {request-target request membership-target membership}))))
