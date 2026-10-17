(ns gesso.model.tx-refinement-premises-test
  "Permanent adversarial checks for the v1029 XTDB refinement premise ledger.

   A valid local Gesso derivation is deliberately NOT a backend proof.
   These tests keep every XTDB, state-correspondence, and application-closure
   premise open, exercise catalog drift and contradictory evidence, and check
   the structure of the resulting obligation graph. No XTDB execution or
   caller-authored formal specification is involved."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gesso.model.command :as command]
            [gesso.model.tx-model :as model]
            [gesso.model.tx-refinement-prestate :as prestate]
            [gesso.model.tx-refinement-premises :as premises]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(def ^:private request
  {:xt/id "private-request-refinement-1030"
   :revision 1
   :created-at "t0"
   :updated-at "t1"
   :owner "private-owner-A"})

(def ^:private membership
  {:xt/id "private-membership-refinement-1030"
   :revision 3
   :created-at "t0"
   :updated-at "t1"
   :active true})

(def ^:private organization
  {:xt/id "private-organization-refinement-1030"
   :revision 2
   :created-at "t0"
   :updated-at "t1"
   :policy "private-policy"})

(def ^:private after-request
  (assoc request :revision 2 :updated-at "t2" :owner "private-owner-B"))

(def ^:private create-plan
  {:commands [(command/create
               :requests
               {:xt/id "private-new-request-refinement-1030"
                :revision 0 :created-at "t0" :updated-at "t0"}
               version)]
   :guards [] :assertions [] :emit false})

(def ^:private guarded-plan
  {:commands [(command/update-command
               :requests :claim request after-request version)]
   :guards [(command/guard :memberships membership version)
            (command/guard :organizations organization version)]
   :assertions [] :emit false})

(def ^:private guarded-prestate
  {[:requests (:xt/id request)] request
   [:memberships (:xt/id membership)] membership
   [:organizations (:xt/id organization)] organization})

(def ^:private expected-premises
  #{:xtdb-dml-is-serialized-in-transaction-log-order
    :xtdb-asserts-evaluate-against-current-transaction-state
    :xtdb-assert-failure-rolls-back-all-transaction-writes
    :xtdb-put-docs-atomically-realizes-command-after-documents
    :xtdb-sql-predicates-refine-finite-guard-semantics
    :transaction-valid-time-and-value-representation-is-admitted
    :concrete-transaction-internal-prestate-relates-to-abstract-prestate
    :relevant-external-writers-and-policy-dependencies-are-closed
    :supplied-abstract-prestate-is-complete-and-genuine})

(def ^:private expected-local-checks
  #{:local-prewrite-lowering-correspondence
    :reference-predicate-truth-recheck
    :all-preconditions-or-atomic-rejection
    :first-failed-precondition-order
    :complete-reference-write-or-no-write})

(defn- ledger
  ([] (premises/derive-obligation-ledger {} create-plan))
  ([state plan] (premises/derive-obligation-ledger state plan)))

(defn- drift-type [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e
         (:error/type (ex-data e)))))

(defn- alter-upstream [rewrite]
  (let [original prestate/derive-conditional-step]
    (with-redefs [prestate/derive-conditional-step
                  (fn [state plan] (rewrite (original state plan)))]
      (ledger))))

(defn- premise-index [report]
  (into {} (map (juxt :id identity)) (:open-premises report)))

(deftest ledger-scope-makes-no-certification-claim-test
  (is (= :gesso.model.tx/serial-refinement-premise-ledger-v1
         (:obligation premises/obligation-scope)))
  (is (= :gesso.model.tx/conditional-serial-step-refinement-v1
         (:local-source premises/obligation-scope)))
  (is (= #{:premise-catalog-equals-source-contract
           :local-check-catalog-equals-source-contract
           :recomputed-local-step-obligation
           :no-external-premise-discharged}
         (:checked premises/obligation-scope)))
  (is (every? (:not-established premises/obligation-scope)
              #{:physical-execution-attestation
                :internal-xtdb-read-snapshot-refinement
                :xtdb-backend-semantics-proved
                :external-writer-closure-proved
                :general-xtdb-refinement
                :application-certification})))

(deftest successful-local-step-leaves-all-backend-premises-open-test
  (let [report (ledger)]
    (is (= :open-external-premises (:status report)))
    (is (= :checked (get-in report [:local-argument :status])))
    (is (= :not-established (get-in report [:refinement-goal :status])))
    (is (= expected-premises (set (map :id (:open-premises report)))))
    (is (= expected-local-checks
           (set (map :id (get-in report [:local-argument :checks])))))
    (is (every? #(= :checked (:status %))
                (get-in report [:local-argument :checks])))
    (is (every? #(= :undischarged (:status %)) (:open-premises report)))
    (is (= #{} (:external-premises-discharged report)))
    (is (false? (:physical-observations-prove-backend-semantics? report)))
    (is (false? (:general-refinement-proved? report)))
    (is (false? (:certifies-physical-execution? report)))))

(deftest legitimate-abstract-rejection-is-not-a-broken-local-proof-test
  (let [state {[:requests "private-new-request-refinement-1030"]
               {:xt/id "private-new-request-refinement-1030"
                :revision 0 :created-at "t0" :updated-at "t0"}}
        report (ledger state create-plan)]
    (is (= :rejected (:abstract-status
                      (prestate/derive-conditional-step state create-plan))))
    (is (= :open-external-premises (:status report)))
    (is (= :checked (get-in report [:local-argument :status])))
    (is (= :not-established (get-in report [:refinement-goal :status])))))

(deftest guarded-transition-keeps-policy-and-read-state-premises-open-test
  (let [report (ledger guarded-prestate guarded-plan)
        by-id (premise-index report)]
    (is (= :open-external-premises (:status report)))
    (is (= :checked (get-in report [:local-argument :status])))
    (is (= :application-admission
           (get-in by-id [:relevant-external-writers-and-policy-dependencies-are-closed :owner])))
    (is (= :concrete-abstract-correspondence
           (get-in by-id [:concrete-transaction-internal-prestate-relates-to-abstract-prestate :owner])))
    (is (= :undischarged
           (get-in by-id [:supplied-abstract-prestate-is-complete-and-genuine :status])))))

(deftest stale-authorization-guard-does-not-turn-into-certification-test
  (let [state (assoc-in guarded-prestate
                        [[:memberships (:xt/id membership)] :revision] 99)
        report (ledger state guarded-plan)]
    (is (= :rejected (:abstract-status
                      (prestate/derive-conditional-step state guarded-plan))))
    (is (= :open-external-premises (:status report)))
    (is (= :checked (get-in report [:local-argument :status])))
    (is (= :not-established (get-in report [:refinement-goal :status])))))

(deftest every-premise-has-independent-owner-and-discharge-requirements-test
  (let [report (ledger)
        by-id (premise-index report)]
    (is (= expected-premises (set (keys by-id))))
    (is (= {:xtdb-backend-semantics 6
            :concrete-abstract-correspondence 2
            :application-admission 1}
           (frequencies (map :owner (:open-premises report)))))
    (doseq [{:keys [id owner discharge-route needed status depends-on]}
            (:open-premises report)]
      (is (contains? expected-premises id))
      (is (keyword? owner))
      (is (keyword? discharge-route))
      (is (keyword? needed))
      (is (= :undischarged status))
      (is (= [] depends-on)))))

(deftest proof-graph-names-exactly-all-required-prerequisites-test
  (let [report (ledger)
        local (get report :local-argument)
        goal (:refinement-goal report)
        local-ids (mapv :id (:checks local))
        premise-ids (mapv :id (:open-premises report))]
    (is (= :valid-local-argument (:id local)))
    (is (= (vec (sort-by name expected-local-checks)) local-ids))
    (is (= local-ids (:depends-on local)))
    (is (= :physical-serial-step-refines-finite-model (:id goal)))
    (is (= :not-established (:status goal)))
    (is (= (into [:valid-local-argument] premise-ids)
           (:depends-on goal)))
    (is (= (vec (sort-by name expected-premises)) premise-ids))
    (is (= (count (:depends-on goal))
           (count (set (:depends-on goal)))))
    (is (= premises/obligation-scope (:scope report)))))

(deftest ledger-output-does-not-leak-sensitive-model-values-test
  (let [report (ledger guarded-prestate guarded-plan)
        printed (pr-str report)]
    (doseq [secret ["private-request-refinement-1030"
                    "private-membership-refinement-1030"
                    "private-organization-refinement-1030"
                    "private-owner-A"
                    "private-owner-B"
                    "private-policy"]]
      (is (not (.contains printed secret))))
    (is (not (contains? report :abstract-state)))
    (is (not (contains? report :sql-parameters)))
    (is (not (contains? report :physical-receipt)))))

(deftest broken-local-transition-suppresses-all-proof-claims-test
  (let [original model/step]
    (with-redefs [model/step
                  (fn [state plan]
                    (assoc (original state plan) :status :rejected))]
      (let [report (ledger)]
        (is (= :invalid-local-argument (:status report)))
        (is (= :failed (get-in report [:local-argument :status])))
        (is (some #(= :failed (:status %))
                  (get-in report [:local-argument :checks])))
        (is (= :not-established (get-in report [:refinement-goal :status])))
        (is (= expected-premises (set (map :id (:open-premises report)))))
        (is (every? #(= :undischarged (:status %)) (:open-premises report)))
        (is (false? (:general-refinement-proved? report)))))))

(deftest missing-or-added-upstream-premises-fail-closed-test
  (doseq [rewrite [(fn [r] (update r :undischarged-premises disj
                                    :xtdb-assert-failure-rolls-back-all-transaction-writes))
                   (fn [r] (update r :undischarged-premises conj :invented-backend-premise))
                   (fn [r] (assoc r :undischarged-premises []))
                   (fn [r] (update-in r [:scope :undischarged-premises]
                                      disj :supplied-abstract-prestate-is-complete-and-genuine))
                   (fn [r] (update-in r [:scope :undischarged-premises]
                                      conj :unclassified-model-dependency))
                   (fn [r] (assoc-in r [:scope :undischarged-premises] nil))]]
    (is (= ::premises/source-contract-drift
           (drift-type #(alter-upstream rewrite))))))

(deftest missing-extra-or-malformed-local-checks-fail-closed-test
  (doseq [rewrite [(fn [r] (update r :checks dissoc
                                    :reference-predicate-truth-recheck))
                   (fn [r] (assoc-in r [:checks :invented-proof-check] true))
                   (fn [r] (assoc-in r [:scope :checked] #{}))
                   (fn [r] (assoc-in r [:scope :checked] nil))
                   (fn [r] (assoc-in r [:scope :checked] #{"not-a-keyword"}))]]
    (is (= ::premises/source-contract-drift
           (drift-type #(alter-upstream rewrite))))))

(deftest changed-conditional-source-identity-fails-closed-test
  (is (= ::premises/source-contract-drift
         (drift-type
          #(alter-upstream
            (fn [r] (assoc r :obligation :different-formal-subject)))))))

(deftest contradictory-validity-flags-fail-closed-test
  (doseq [rewrite [(fn [r] (assoc r :local-argument-valid? false))
                   (fn [r] (assoc r :conditional-argument-admissible? false))
                   (fn [r] (assoc-in r [:physical-refinement :status]
                                     :invalid-local-argument))
                   (fn [r] (assoc-in r [:physical-refinement :status]
                                     :certified))
                   (fn [r] (dissoc r :conditional-argument-admissible?))]]
    (is (= ::premises/source-contract-drift
           (drift-type #(alter-upstream rewrite))))))

(deftest malformed-individual-check-cannot-be-upgraded-to-a-proof-test
  (doseq [value [false nil :checked 1]]
    (let [original prestate/derive-conditional-step]
      (with-redefs [prestate/derive-conditional-step
                    (fn [state plan]
                      (assoc-in (original state plan)
                                [:checks :reference-predicate-truth-recheck]
                                value))]
        (let [outcome (try {:report (ledger)}
                           (catch clojure.lang.ExceptionInfo e
                             {:error (:error/type (ex-data e))}))]
          (is (or (= ::premises/source-contract-drift (:error outcome))
                  (and (= :invalid-local-argument
                          (get-in outcome [:report :status]))
                       (= :not-established
                          (get-in outcome [:report :refinement-goal :status]))))))))))

(deftest catalog-drift-diagnostics-do-not-include-document-values-test
  (let [original prestate/derive-conditional-step]
    (with-redefs [prestate/derive-conditional-step
                  (fn [state plan]
                    (update (original state plan)
                            :undischarged-premises conj :unknown-proof-premise))]
      (let [error (try (ledger guarded-prestate guarded-plan)
                       nil
                       (catch clojure.lang.ExceptionInfo e e))]
        (is (instance? clojure.lang.ExceptionInfo error))
        (is (= ::premises/source-contract-drift
               (:error/type (ex-data error))))
        (is (= #{:unknown-proof-premise}
               (get-in (ex-data error) [:error/details :unknown])))
        (doseq [secret ["private-request-refinement-1030"
                        "private-owner-A" "private-policy"]]
          (is (not (.contains (pr-str (ex-data error)) secret))))))))
