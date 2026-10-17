(ns gesso.model.tx-refinement-prestate-test
  "Permanent adversarial regressions for the LOCAL XTDB prewrite obligation,
   including v1020 SQL-predicate and bound-value correspondence.

   These tests establish the order and structural correspondence of Gesso's
   admitted transaction lowering with its bounded reference-model checks.
   They intentionally mutate the lowering or reference evidence to verify
   that contradictory constructions are rejected. They do NOT prove XTDB's
   internal read snapshot, SQL predicate correctness, physical execution,
   durable replay, or universal concrete-to-abstract refinement."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.model.command :as command]
            [gesso.model.tx :as tx]
            [gesso.model.tx-model :as model]
            [gesso.model.tx-refinement-prestate :as prestate]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(def ^:private request
  {:xt/id "secret-request-prewrite-1"
   :revision 0
   :created-at "t0"
   :updated-at "t0"
   :owner "unclaimed"})

(def ^:private membership
  {:xt/id "secret-membership-prewrite-1"
   :revision 0
   :created-at "t0"
   :updated-at "t0"
   :active true})

(def ^:private organization
  {:xt/id "secret-organization-prewrite-1"
   :revision 0
   :created-at "t0"
   :updated-at "t0"
   :policy "allow"})

(defn- advance [document changes]
  (-> document
      (merge changes)
      (update :revision inc)
      (assoc :updated-at "t1")))

(defn- update-command [type operation before changes]
  (command/update-command type operation before (advance before changes) version))

(defn- create-command [type id]
  (command/create
   type {:xt/id id :revision 0 :created-at "t0" :updated-at "t0"}
   version))

(def ^:private claim
  (update-command :requests :claim request {:owner "helper-1"}))

(def ^:private revoke
  (update-command :memberships :revoke membership {:active false}))

(def ^:private membership-guard
  (command/guard :memberships membership version))

(def ^:private organization-guard
  (command/guard :organizations organization version))

(def ^:private request-guard
  (command/guard :requests request version))

(defn- plan [commands & {:keys [guards assertions]}]
  {:commands (vec commands)
   :guards (vec guards)
   :assertions (vec assertions)
   :emit false})

(def ^:private create-plan
  (plan [(create-command :requests "secret-new-request-prewrite-2")]))

(def ^:private guarded-plan
  (plan [claim] :guards [membership-guard organization-guard]))

(def ^:private field-count
  (tx/assert-document-field-count :requests (:xt/id request) 5))

(def ^:private rich-plan
  (plan [claim (create-command :audits "secret-audit-prewrite-3")]
        :assertions [field-count]
        :guards [membership-guard organization-guard]))

(defn- error-type [thunk]
  (try (thunk)
       nil
       (catch clojure.lang.ExceptionInfo error
         (:error/type (ex-data error)))))

(defn- false-checks [obligation]
  (->> (:checks obligation)
       (keep (fn [[check result]] (when (false? result) check)))
       set))

(deftest scope-explicitly-separates-construction-from-backend-premises-test
  (let [scope prestate/contract-scope
        obligation (prestate/derive-obligation create-plan)]
    (is (= :gesso.model.tx/prewrite-serial-refinement-v1 (:obligation scope)))
    (is (= scope (:contract-scope obligation)))
    (is (= (:external-premises scope) (:backend-premises obligation)))
    (is (= (:external-premises scope) (:undischarged-premises obligation)))
    (is (contains? (:checked scope) :assertions-precede-all-writes))
    (is (contains? (:checked scope) :precondition-order-matches-finite-reference))
    (doseq [premise [:xtdb-dml-is-serialized-in-transaction-log-order
                     :xtdb-asserts-evaluate-against-current-transaction-state
                     :xtdb-assert-failure-rolls-back-all-transaction-writes
                     :xtdb-sql-predicates-refine-finite-guard-semantics
                     :concrete-transaction-internal-prestate-relates-to-abstract-prestate
                     :relevant-external-writers-and-policy-dependencies-are-closed]]
      (is (contains? (:undischarged-premises obligation) premise)))
    (doseq [unproved [:internal-prestate-from-pinned-external-observations
                      :production-execution-or-ledger-attestation
                      :application-authorization-or-business-rule-proof
                      :universal-physical-to-abstract-step-refinement]]
      (is (contains? (:not-established scope) unproved)))
    (is (false? (:certifies-physical-execution? obligation)))
    (is (false? (:proves-internal-prestate? obligation)))
    (is (false? (:proves-general-refinement? obligation)))))

(deftest create-has-one-assertion-followed-by-one-write-test
  (let [obligation (prestate/derive-obligation create-plan)]
    (is (:local-structure-valid? obligation))
    (is (every? true? (vals (:checks obligation))))
    (is (= 1 (:assertion-count obligation)))
    (is (= 1 (:write-count obligation)))
    (is (= 1 (:first-write-index obligation)))
    (is (= [{:assertion-index 0 :kind :document-absent :table :requests}]
           (:preconditions obligation)))))

(deftest guarded-update-preserves-ordered-dependency-targets-test
  (let [obligation (prestate/derive-obligation guarded-plan)]
    (is (:local-structure-valid? obligation))
    (is (= 3 (:assertion-count obligation)))
    (is (= 1 (:write-count obligation)))
    (is (= [:dependency-version :dependency-version :command-version]
           (mapv :kind (:preconditions obligation))))
    (is (= [:memberships :organizations :requests]
           (mapv :table (:preconditions obligation))))
    (is (= [0 1 2] (mapv :assertion-index (:preconditions obligation))))
    (is (= 3 (:first-write-index obligation)))))

(deftest multiwrite-with-explicit-assertion-puts-all-checks-before-all-writes-test
  (let [obligation (prestate/derive-obligation rich-plan)]
    (is (:local-structure-valid? obligation))
    (is (= 5 (:assertion-count obligation)))
    (is (= 2 (:write-count obligation)))
    (is (= 5 (:first-write-index obligation)))
    (is (= [0 1 2 3 4] (mapv :assertion-index (:preconditions obligation))))
    (is (= [:document-field-count
            :dependency-version :dependency-version
            :command-version :document-absent]
           (mapv :kind (:preconditions obligation))))
    (is (= [:requests :memberships :organizations :requests :audits]
           (mapv :table (:preconditions obligation))))
    (is (= 7 (count (:tx-ops (tx/prepare {} rich-plan)))))))

(deftest duplicate-and-redundant-guards-are-normalized-before-lowering-test
  (let [duplicate (plan [claim]
                        :guards [membership-guard membership-guard
                                 request-guard organization-guard])
        obligation (prestate/derive-obligation duplicate)]
    (is (:local-structure-valid? obligation))
    (is (= 3 (:assertion-count obligation)))
    (is (= [:memberships :organizations :requests]
           (mapv :table (:preconditions obligation))))
    (is (= [:dependency-version :dependency-version :command-version]
           (mapv :kind (:preconditions obligation))))))

(deftest publication-and-domain-changes-are-not-mistaken-for-proved-effects-test
  (let [with-effects (assoc create-plan
                            :emit :sync
                            :changes [{:topic :requests :id "secret-notification"}])
        obligation (prestate/derive-obligation with-effects)]
    (is (:local-structure-valid? obligation))
    (is (= 1 (:assertion-count obligation)))
    (is (contains? (get-in obligation [:contract-scope :not-established])
                   :browser-or-choreo-refinement))
    (is (false? (:certifies-physical-execution? obligation)))))

(deftest contract-output-does-not-disclose-identities-guard-values-or-sql-test
  (let [rendered (pr-str (prestate/derive-obligation rich-plan))]
    (is (not (re-find #"secret-" rendered)))
    (is (not (re-find #"(?i)ASSERT\\s+" rendered)))
    (is (not (re-find #"helper-1" rendered)))
    (is (not (re-find #"unclaimed" rendered)))))

(deftest unsupported-sql-and-transaction-features-fail-before-a-valid-obligation-test
  (doseq [[label supplement]
          [[:other-sql {:assertions [{:assert {:select [true]}}]}]
           [:entry {:entry {:custom true}}]
           [:entry-fn {:entry-fn identity}]
           [:tx-options {:tx-options {}}]]]
    (testing (name label)
      (is (= ::model/unsupported-plan
             (error-type #(prestate/derive-obligation
                           (merge create-plan supplement)))))))
  (is (= ::tx/unknown-plan-keys
         (error-type #(prestate/derive-obligation
                       (assoc create-plan :arbitrary-rule true)))))
  (is (= ::tx/missing-commands
         (error-type #(prestate/derive-obligation
                       (assoc create-plan :commands []))))))

(deftest contradictory-or-duplicate-command-targets-fail-admission-test
  (is (= ::tx/duplicate-command-targets
         (error-type #(prestate/derive-obligation
                       (plan [claim (update-command :requests :cancel
                                                   request {:owner "none"})])))))
  (is (= ::tx/guard-conflicts-with-create
         (error-type #(prestate/derive-obligation
                       (plan [(create-command :requests (:xt/id request))]
                             :guards [request-guard])))))
  (is (= ::tx/conflicting-guards
         (error-type #(prestate/derive-obligation
                       (plan [claim] :guards
                             [membership-guard
                              (command/guard :memberships
                                             (advance membership {:active false})
                                             version)]))))))

(deftest moved-assertion-after-write-fails-local-structure-test
  (let [original tx/transaction-ops]
    (with-redefs [tx/transaction-ops
                  (fn [a-plan]
                    (let [ops (original a-plan)]
                      (conj (subvec ops 1) (first ops))))]
      (let [obligation (prestate/derive-obligation create-plan)]
        (is (false? (:local-structure-valid? obligation)))
        (is (contains? (false-checks obligation) :canonical-operation-sequence))
        (is (contains? (false-checks obligation) :prewrite-assertion-prefix))
        (is (contains? (false-checks obligation) :write-only-suffix))))))

(deftest swapping-guard-and-command-assertions-fails-reference-order-test
  (let [original tx/transaction-ops]
    (with-redefs [tx/transaction-ops
                  (fn [a-plan]
                    (let [ops (original a-plan)]
                      (assoc ops 0 (nth ops 1) 1 (first ops))))]
      (let [obligation (prestate/derive-obligation guarded-plan)]
        (is (false? (:local-structure-valid? obligation)))
        (is (contains? (false-checks obligation) :canonical-operation-sequence))
        (is (contains? (false-checks obligation) :prewrite-assertion-prefix))))))

(deftest omitted-assertion-fails-local-structure-test
  (let [original tx/transaction-ops]
    (with-redefs [tx/transaction-ops
                  (fn [a-plan]
                    (subvec (original a-plan) 1))]
      (let [obligation (prestate/derive-obligation guarded-plan)]
        (is (false? (:local-structure-valid? obligation)))
        (is (contains? (false-checks obligation) :canonical-operation-sequence))
        (is (contains? (false-checks obligation) :prewrite-assertion-prefix))))))

(deftest duplicated-write-fails-count-and-suffix-test
  (let [original tx/transaction-ops]
    (with-redefs [tx/transaction-ops
                  (fn [a-plan]
                    (let [ops (original a-plan)]
                      (conj ops (peek ops))))]
      (let [obligation (prestate/derive-obligation create-plan)]
        (is (false? (:local-structure-valid? obligation)))
        (is (contains? (false-checks obligation) :canonical-operation-sequence))
        (is (contains? (false-checks obligation) :write-only-suffix))
        (is (contains? (false-checks obligation) :formatted-operation-count))))))

(deftest formatted-prefix-must-contain-only-recognized-assert-sql-test
  (let [original tx/prepare]
    (with-redefs [tx/prepare
                  (fn [ctx a-plan]
                    (update (original ctx a-plan) :tx-ops
                            #(assoc % 0 ["SELECT TRUE"])))]
      (let [obligation (prestate/derive-obligation create-plan)]
        (is (false? (:local-structure-valid? obligation)))
        (is (contains? (false-checks obligation) :formatted-assertion-prefix))
        (is (true? (get-in obligation [:checks :canonical-operation-sequence])))))))

(deftest formatted-suffix-must-contain-only-canonical-puts-test
  (let [original tx/prepare]
    (with-redefs [tx/prepare
                  (fn [ctx a-plan]
                    (update (original ctx a-plan) :tx-ops
                            #(assoc % 1 [:put-docs :wrong-table {:xt/id "fake"}])))]
      (let [obligation (prestate/derive-obligation create-plan)]
        (is (false? (:local-structure-valid? obligation)))
        (is (contains? (false-checks obligation) :formatted-put-only-suffix))
        (is (true? (get-in obligation [:checks :prewrite-assertion-prefix])))))))

(deftest formatted-transaction-with-extra-operation-fails-count-test
  (let [original tx/prepare]
    (with-redefs [tx/prepare
                  (fn [ctx a-plan]
                    (update (original ctx a-plan) :tx-ops
                            conj [:put-docs :audits {:xt/id "extra"}]))]
      (let [obligation (prestate/derive-obligation create-plan)]
        (is (false? (:local-structure-valid? obligation)))
        (is (contains? (false-checks obligation) :formatted-operation-count))
        (is (contains? (false-checks obligation) :formatted-put-only-suffix))))))

(deftest mismatched-reference-guard-target-fails-local-structure-test
  (let [original model/step]
    (with-redefs [model/step
                  (fn [state a-plan]
                    (update-in (original state a-plan) [:checks 0 :target]
                               (constantly [:memberships "wrong-target"])))]
      (let [obligation (prestate/derive-obligation guarded-plan)]
        (is (false? (:local-structure-valid? obligation)))
        (is (contains? (false-checks obligation) :dependency-guard-targets))
        (is (true? (get-in obligation [:checks :canonical-operation-sequence])))))))

(deftest mismatched-reference-command-target-fails-local-structure-test
  (let [original model/step]
    (with-redefs [model/step
                  (fn [state a-plan]
                    (update-in (original state a-plan) [:checks 0 :target]
                               (constantly [:requests "wrong-target"])))]
      (let [obligation (prestate/derive-obligation create-plan)]
        (is (false? (:local-structure-valid? obligation)))
        (is (contains? (false-checks obligation) :command-targets))
        (is (true? (get-in obligation [:checks :formatted-assertion-prefix])))))))

(deftest mismatched-reference-kind-fails-local-structure-test
  (let [original model/step]
    (with-redefs [model/step
                  (fn [state a-plan]
                    (update-in (original state a-plan) [:checks 0 :kind]
                               (constantly :wrong-precondition)))]
      (let [obligation (prestate/derive-obligation rich-plan)]
        (is (false? (:local-structure-valid? obligation)))
        (is (contains? (false-checks obligation) :precondition-kinds-and-order))
        (is (= 5 (:assertion-count obligation)))))))

(deftest missing-reference-check-fails-count-and-order-test
  (let [original model/step]
    (with-redefs [model/step
                  (fn [state a-plan]
                    (update (original state a-plan) :checks pop))]
      (let [obligation (prestate/derive-obligation guarded-plan)]
        (is (false? (:local-structure-valid? obligation)))
        (is (contains? (false-checks obligation) :precondition-count))
        (is (contains? (false-checks obligation) :precondition-kinds-and-order))))))


;; ---------------------------------------------------------------------------
;; v1021: the finite-model check vocabulary is the independent reference for
;; BOTH raw HoneySQL predicates and the prepared XTDB SQL/binding vectors.
;; These are local construction checks, not a proof of backend SQL semantics.
;; ---------------------------------------------------------------------------

(defn- reference-checks-valid?
  [obligation]
  (and (true? (get-in obligation
                       [:checks :reference-preconditions-match-unformatted-assertions]))
       (true? (get-in obligation
                       [:checks :formatted-assertion-sql-and-bindings-match-reference]))))

(deftest reference-sql-contract-scope-remains-explicitly-local-test
  (let [scope prestate/contract-scope
        obligation (prestate/derive-obligation rich-plan)]
    (is (contains? (:checked scope)
                   :reference-precondition-to-honeysql-expression-correspondence))
    (is (contains? (:checked scope)
                   :formatted-assertion-sql-and-bound-values-match-reference))
    (is (contains? (:external-premises scope)
                   :xtdb-sql-predicates-refine-finite-guard-semantics))
    (is (contains? (:not-established scope)
                   :backend-premises-proved-from-xtdb-source))
    (is (reference-checks-valid? obligation))
    (is (false? (:certifies-physical-execution? obligation)))
    (is (false? (:proves-general-refinement? obligation)))))

(deftest reference-sql-accepts-the-entire-admitted-check-vocabulary-test
  (doseq [[label a-plan expected-kinds]
          [[:create create-plan [:document-absent]]
           [:guard guarded-plan
            [:dependency-version :dependency-version :command-version]]
           [:field-count rich-plan
            [:document-field-count :dependency-version :dependency-version
             :command-version :document-absent]]]]
    (testing (name label)
      (let [obligation (prestate/derive-obligation a-plan)]
        (is (:local-structure-valid? obligation))
        (is (reference-checks-valid? obligation))
        (is (= expected-kinds (mapv :kind (:preconditions obligation))))))))

(deftest altered-formatted-assertion-parameter-fails-even-with-valid-assert-prefix-test
  (let [original tx/prepare]
    (with-redefs [tx/prepare
                  (fn [ctx a-plan]
                    (update-in (original ctx a-plan) [:tx-ops 0 1]
                               (constantly "secret-forged-bound-id")))]
      (let [result (prestate/derive-obligation create-plan)]
        (is (false? (:local-structure-valid? result)))
        (is (false? (get-in result
                            [:checks :formatted-assertion-sql-and-bindings-match-reference])))
        (is (true? (get-in result [:checks :formatted-assertion-prefix])))
        (is (true? (get-in result
                           [:checks :reference-preconditions-match-unformatted-assertions])))
        (is (not (re-find #"secret-forged" (pr-str result))))))))

(deftest altered-formatted-assertion-sql-fails-despite-unchanged-bindings-test
  (let [original tx/prepare]
    (with-redefs [tx/prepare
                  (fn [ctx a-plan]
                    (update-in (original ctx a-plan) [:tx-ops 0 0]
                               #(str % " /* changed-assert-predicate */")))]
      (let [result (prestate/derive-obligation create-plan)]
        (is (false? (:local-structure-valid? result)))
        (is (false? (get-in result
                            [:checks :formatted-assertion-sql-and-bindings-match-reference])))
        (is (true? (get-in result [:checks :formatted-assertion-prefix])))
        (is (true? (get-in result
                           [:checks :reference-preconditions-match-unformatted-assertions])))
        (is (not (re-find #"changed-assert" (pr-str result))))))))

(deftest reordered-formatted-guard-sql-fails-independent-reference-test
  (let [original tx/prepare]
    (with-redefs [tx/prepare
                  (fn [ctx a-plan]
                    (update (original ctx a-plan) :tx-ops
                            #(assoc % 0 (nth % 1) 1 (first %))))]
      (let [result (prestate/derive-obligation guarded-plan)]
        (is (false? (:local-structure-valid? result)))
        (is (false? (get-in result
                            [:checks :formatted-assertion-sql-and-bindings-match-reference])))
        (is (true? (get-in result [:checks :formatted-assertion-prefix])))
        (is (true? (get-in result [:checks :canonical-operation-sequence])))))))

(deftest dropped-formatted-assertion-binding-is-rejected-test
  (let [original tx/prepare]
    (with-redefs [tx/prepare
                  (fn [ctx a-plan]
                    (update-in (original ctx a-plan) [:tx-ops 0] pop))]
      (let [result (prestate/derive-obligation create-plan)]
        (is (false? (:local-structure-valid? result)))
        (is (false? (get-in result
                            [:checks :formatted-assertion-sql-and-bindings-match-reference])))
        (is (true? (get-in result [:checks :formatted-assertion-prefix])))))))

(deftest changed-unformatted-document-absence-predicate-fails-reference-test
  (let [original tx/command-precondition]
    (with-redefs [tx/command-precondition
                  (fn [c]
                    (update-in (original c) [:assert 2 :where]
                               (constantly [:= :xt/id "secret-wrong-document"])))]
      (let [result (prestate/derive-obligation create-plan)]
        (is (false? (:local-structure-valid? result)))
        (is (false? (get-in result
                            [:checks :reference-preconditions-match-unformatted-assertions])))
        (is (false? (get-in result
                            [:checks :formatted-assertion-sql-and-bindings-match-reference])))
        (is (not (re-find #"secret-wrong" (pr-str result))))))))

(deftest changed-unformatted-version-predicate-fails-reference-test
  (let [original tx/guard-assertion]
    (with-redefs [tx/guard-assertion
                  (fn [g]
                    (update-in (original g) [:assert 2 :where]
                               (constantly [:= :xt/id "secret-other-guard"])))]
      (let [result (prestate/derive-obligation guarded-plan)]
        (is (false? (:local-structure-valid? result)))
        (is (false? (get-in result
                            [:checks :reference-preconditions-match-unformatted-assertions])))
        (is (false? (get-in result
                            [:checks :formatted-assertion-sql-and-bindings-match-reference])))
        (is (not (re-find #"secret-other" (pr-str result))))))))

(deftest changed-finite-reference-field-count-fails-both-checks-test
  (let [original model/step]
    (with-redefs [model/step
                  (fn [state a-plan]
                    (update-in (original state a-plan) [:checks 0 :expected-count]
                               (constantly 4)))]
      (let [result (prestate/derive-obligation rich-plan)]
        (is (false? (:local-structure-valid? result)))
        (is (false? (get-in result
                            [:checks :reference-preconditions-match-unformatted-assertions])))
        (is (false? (get-in result
                            [:checks :formatted-assertion-sql-and-bindings-match-reference])))))))

(deftest malformed-finite-reference-field-count-fails-closed-test
  (let [original model/step]
    (with-redefs [model/step
                  (fn [state a-plan]
                    (update-in (original state a-plan) [:checks 0 :expected-count]
                               (constantly -1)))]
      (let [result (prestate/derive-obligation rich-plan)]
        (is (false? (:local-structure-valid? result)))
        (is (false? (get-in result
                            [:checks :reference-preconditions-match-unformatted-assertions])))
        (is (false? (get-in result
                            [:checks :formatted-assertion-sql-and-bindings-match-reference])))))))

(deftest qualified-tables-and-nested-values-are-bound-as-data-test
  (let [version-with-payload (assoc version :compare-keys
                                    [:revision :updated-at :payload])
        before {:xt/id "secret-qualified-id"
                :revision 3 :created-at "t0" :updated-at "t0"
                :payload {:nested [:domain/role {:value "secret-nested-value"}]}}
        after (advance before {:status :claimed})
        a-plan (plan [(command/update-command
                       :domain/request :claim before after version-with-payload)])
        prepared (first (:tx-ops (tx/prepare {} a-plan)))
        result (prestate/derive-obligation a-plan)]
    (is (:local-structure-valid? result))
    (is (reference-checks-valid? result))
    (is (= [:domain/request] (mapv :table (:preconditions result))))
    (is (some #(= (:payload before) %) (rest prepared)))
    (is (not (re-find #"secret-qualified-id|secret-nested-value"
                       (pr-str result))))))

(deftest qualified-nested-bound-parameter-mutation-fails-reference-test
  (let [version-with-payload (assoc version :compare-keys
                                    [:revision :updated-at :payload])
        before {:xt/id "secret-qualified-parameter"
                :revision 3 :created-at "t0" :updated-at "t0"
                :payload {:nested [:domain/role {:value "secret-unchanged"}]}}
        after (advance before {:status :claimed})
        a-plan (plan [(command/update-command
                       :domain/request :claim before after version-with-payload)])
        original tx/prepare]
    (with-redefs [tx/prepare
                  (fn [ctx p]
                    (update-in (original ctx p) [:tx-ops 0]
                               (fn [sql-vec]
                                 (mapv (fn [param]
                                         (if (= param (:payload before))
                                           {:nested [:domain/role {:value "secret-mutated"}]}
                                           param))
                                       sql-vec))))]
      (let [result (prestate/derive-obligation a-plan)]
        (is (false? (:local-structure-valid? result)))
        (is (false? (get-in result
                            [:checks :formatted-assertion-sql-and-bindings-match-reference])))
        (is (true? (get-in result
                           [:checks :reference-preconditions-match-unformatted-assertions])))
        (is (not (re-find #"secret-unchanged|secret-mutated" (pr-str result))))))))
