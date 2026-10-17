(ns gesso.model.tx-refinement-prestate-test
  "Permanent adversarial regressions for the LOCAL XTDB prewrite obligation.

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
