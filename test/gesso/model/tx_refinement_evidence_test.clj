(ns gesso.model.tx-refinement-evidence-test
  "Adversarial tests for v1031's candidate-evidence review boundary.

   These tests distinguish well-formed *proposals* from authenticated proof.
   They intentionally do not fabricate a verifier, discharge XTDB premises,
   or bind generic catalog evidence to a particular application transaction."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.model.command :as command]
            [gesso.model.tx-model :as model]
            [gesso.model.tx-refinement-evidence :as evidence]
            [gesso.model.tx-refinement-premises :as premises]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(def ^:private private-id "private-request-evidence-v1032")
(def ^:private create-doc
  {:xt/id private-id :revision 0 :created-at "t0" :updated-at "t0"})
(def ^:private create-plan
  {:commands [(command/create :requests create-doc version)]
   :guards [] :assertions [] :emit false})

(def ^:private subject :gesso.model.tx/serial-refinement-premise-ledger-v1)
(def ^:private sha-a (apply str (repeat 64 "a")))
(def ^:private sha-b (apply str (repeat 64 "b")))
(def ^:private default-premise :xtdb-dml-is-serialized-in-transaction-log-order)

(def ^:private expected-routes
  {:xtdb-dml-is-serialized-in-transaction-log-order
   {:owner :xtdb-backend-semantics
    :alternatives [#{:versioned-backend-contract} #{:independent-formal-proof}]}
   :xtdb-asserts-evaluate-against-current-transaction-state
   {:owner :xtdb-backend-semantics
    :alternatives [#{:versioned-backend-contract} #{:independent-formal-proof}]}
   :xtdb-assert-failure-rolls-back-all-transaction-writes
   {:owner :xtdb-backend-semantics
    :alternatives [#{:versioned-backend-contract} #{:independent-formal-proof}]}
   :xtdb-put-docs-atomically-realizes-command-after-documents
   {:owner :xtdb-backend-semantics
    :alternatives [#{:versioned-backend-contract} #{:independent-formal-proof}]}
   :xtdb-sql-predicates-refine-finite-guard-semantics
   {:owner :xtdb-backend-semantics
    :alternatives [#{:independent-predicate-proof}]}
   :transaction-valid-time-and-value-representation-is-admitted
   {:owner :xtdb-backend-semantics
    :alternatives [#{:versioned-backend-contract :representation-refinement-proof}]}
   :concrete-transaction-internal-prestate-relates-to-abstract-prestate
   {:owner :concrete-abstract-correspondence
    :alternatives [#{:internal-read-state-proof}]}
   :relevant-external-writers-and-policy-dependencies-are-closed
   {:owner :application-admission
    :alternatives [#{:application-dependency-closure-proof}]}
   :supplied-abstract-prestate-is-complete-and-genuine
   {:owner :concrete-abstract-correspondence
    :alternatives [#{:abstract-prestate-provenance-proof}]}})

(defn- proposal
  ([premise kind]
   (proposal premise kind sha-a))
  ([premise kind digest]
   {:premise-id premise
    :evidence-kind kind
    :subject subject
    :artifact-sha256 digest
    :claimed-issuer "private-issuer-1032"
    :artifact-version "private-version-1032"}))

(def ^:private ordinary
  (proposal default-premise :versioned-backend-contract))

(defn- review
  ([proposals] (evidence/review-evidence {} create-plan proposals))
  ([state plan proposals] (evidence/review-evidence state plan proposals)))

(defn- entry [report id]
  (some #(when (= id (:id %)) %) (:requirements report)))

(defn- reason [p]
  (get-in (review [p]) [:proposals 0 :reason]))

(defn- exception-type [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e
         (:error/type (ex-data e)))))

(defn- no-proof-claims! [report]
  (is (= #{} (:external-premises-discharged report)))
  (is (false? (:evidence-authenticated? report)))
  (is (false? (:any-proof-checked? report)))
  (is (false? (:backend-contract-admitted? report)))
  (is (false? (:general-refinement-proved? report)))
  (is (false? (:certifies-physical-execution? report)))
  (is (false? (:plan-specific-subject-bound? report)))
  (is (= #{:undischarged} (set (map :status (:requirements report)))))
  (doseq [p (:proposals report)]
    (is (false? (:authenticated? p)))
    (is (false? (:proof-checked? p)))
    (is (false? (:discharges-premise? p)))))

(deftest review-scope-explicitly-excludes-proof-and-application-binding-test
  (is (= :gesso.model.tx/refinement-evidence-review-v1
         (:obligation evidence/evidence-scope)))
  (is (= subject (:premise-source evidence/evidence-scope)))
  (is (every? (:checked evidence/evidence-scope)
              #{:open-premise-catalog-and-dependency-graph
                :independent-evidence-route-classification
                :proposal-shape-and-subject-binding
                :duplicate-and-unknown-proposal-rejection
                :no-caller-supplied-discharge}))
  (is (every? (:not-established evidence/evidence-scope)
              #{:artifact-existence :artifact-content-authenticity
                :issuer-authentication :independent-proof-validation
                :application-plan-specific-subject-binding
                :backend-contract-admission :physical-execution-attestation
                :transaction-internal-read-state-refinement
                :any-external-premise-discharged :general-xtdb-refinement
                :application-certification})))

(deftest empty-review-keeps-all-nine-premises-undischarged-test
  (let [r (review [])]
    (is (= :awaiting-independent-evidence (:status r)))
    (is (= subject (:subject r)))
    (is (= :checked (:local-argument-status r)))
    (is (= [] (:proposals r)))
    (is (zero? (:reviewable-proposals r)))
    (is (= (set (keys expected-routes)) (set (map :id (:requirements r)))))
    (is (every? (comp false? :review-package-complete?) (:requirements r)))
    (no-proof-claims! r)))

(deftest exact-nine-premise-routes-and-owners-test
  (let [r (review [])]
    (is (= 9 (count (:requirements r))))
    (is (= {:xtdb-backend-semantics 6
            :concrete-abstract-correspondence 2
            :application-admission 1}
           (frequencies (map :owner (:requirements r)))))
    (doseq [[id {:keys [owner alternatives]}] expected-routes]
      (let [requirement (entry r id)]
        (is (= owner (:owner requirement)))
        (is (= alternatives (:candidate-alternatives requirement)))
        (is (keyword? (:needed requirement)))
        (is (keyword? (:discharge-route requirement)))
        (is (= :undischarged (:status requirement)))))))

(deftest reviewable-contract-proposal-is-not-authenticated-proof-test
  (let [r (review [ordinary])]
    (is (= :independent-review-required (:status r)))
    (is (= 1 (:reviewable-proposals r)))
    (is (= :review-required (get-in r [:proposals 0 :status])))
    (is (= :requires-independent-verification
           (get-in r [:proposals 0 :reason])))
    (is (true? (:review-package-complete? (entry r default-premise))))
    (is (false? (:review-package-complete?
                 (entry r :xtdb-sql-predicates-refine-finite-guard-semantics))))
    (no-proof-claims! r)))

(deftest backend-contract-or-independent-proof-are-alternatives-test
  (doseq [kind [:versioned-backend-contract :independent-formal-proof]]
    (let [r (review [(proposal default-premise kind)])]
      (is (= :independent-review-required (:status r)))
      (is (= :requires-independent-verification
             (get-in r [:proposals 0 :reason])))
      (is (true? (:review-package-complete? (entry r default-premise))))
      (no-proof-claims! r))))

(deftest predicate-equivalence-does-not-accept-backend-contract-or-test-log-test
  (let [id :xtdb-sql-predicates-refine-finite-guard-semantics]
    (is (= :requires-independent-verification
           (reason (proposal id :independent-predicate-proof))))
    (doseq [kind [:versioned-backend-contract :independent-formal-proof
                  :test-run :xtdb-observation :successful-model-check]]
      (is (= :incompatible-evidence-kind (reason (proposal id kind)))))))

(deftest representation-requires-both-kinds-not-either-alone-test
  (let [id :transaction-valid-time-and-value-representation-is-admitted
        contract (proposal id :versioned-backend-contract)
        proof (proposal id :representation-refinement-proof sha-b)
        a (review [contract])
        b (review [proof])
        together (review [contract proof])]
    (is (false? (:review-package-complete? (entry a id))))
    (is (false? (:review-package-complete? (entry b id))))
    (is (true? (:review-package-complete? (entry together id))))
    (is (= 2 (:reviewable-proposals together)))
    (no-proof-claims! together)))

(deftest every-premise-accepts-only-its-declared-evidence-route-test
  (doseq [[id {:keys [alternatives]}] expected-routes
          :let [kinds (first alternatives)]
          kind kinds]
    (let [r (review [(proposal id kind)])]
      (is (= :independent-review-required (:status r)))
      (is (= :requires-independent-verification
             (get-in r [:proposals 0 :reason])))))
  (doseq [id (keys expected-routes)]
    (is (= :incompatible-evidence-kind
           (reason (proposal id :caller-says-proved))))))

(deftest complete-candidate-catalog-is-still-only-a-review-package-test
  (let [all (vec (mapcat (fn [[id {:keys [alternatives]}]]
                           (map (fn [kind] (proposal id kind))
                                (first alternatives)))
                         (sort-by (comp name key) expected-routes)))
        r (review all)]
    (is (= 10 (count all)))
    (is (= 10 (:reviewable-proposals r)))
    (is (= :independent-review-required (:status r)))
    (is (every? :review-package-complete? (:requirements r)))
    (no-proof-claims! r)))

(deftest any-rejected-proposal-invalidates-the-batch-not-the-good-ones-test
  (let [r (review [ordinary (assoc ordinary :evidence-kind :test-run)])]
    (is (= :invalid-evidence-proposals (:status r)))
    (is (= 1 (:reviewable-proposals r)))
    (is (= [:review-required :rejected]
           (mapv :status (:proposals r))))
    (is (= :incompatible-evidence-kind (get-in r [:proposals 1 :reason])))
    (is (true? (:review-package-complete? (entry r default-premise))))
    (no-proof-claims! r)))

(deftest caller-forged-proof-and-trust-flags-rejected-test
  (doseq [k [:verified? :discharged? :certified? :trusted?
             :proof-checked? :authenticated? :physical-proof
             :status :backend-contract-admitted? :plan-specific-subject-bound?]]
    (is (= :malformed-proposal (reason (assoc ordinary k true)))))
  (is (= :malformed-proposal
         (reason (assoc ordinary :artifact-sha256 sha-b :verified? true)))))

(deftest exact-proposal-shape-and-bounded-fields-required-test
  (doseq [p [(dissoc ordinary :premise-id)
             (dissoc ordinary :evidence-kind)
             (dissoc ordinary :subject)
             (dissoc ordinary :artifact-sha256)
             (dissoc ordinary :claimed-issuer)
             (dissoc ordinary :artifact-version)
             (assoc ordinary :premise-id "xtdb")
             (assoc ordinary :evidence-kind "proof")
             (assoc ordinary :subject "subject")
             (assoc ordinary :artifact-sha256 nil)
             (assoc ordinary :artifact-sha256 "not-a-digest")
             (assoc ordinary :artifact-sha256 (.toUpperCase sha-a))
             (assoc ordinary :artifact-sha256 (apply str (repeat 63 "a")))
             (assoc ordinary :artifact-sha256 (apply str (repeat 65 "a")))
             (assoc ordinary :claimed-issuer " ")
             (assoc ordinary :claimed-issuer nil)
             (assoc ordinary :artifact-version "\t")
             (assoc ordinary :artifact-version 10)
             (assoc ordinary :claimed-issuer (apply str (repeat 257 "x")))
             (assoc ordinary :artifact-version (apply str (repeat 257 "x")))
             nil [] {:arbitrary :map}]]
    (is (= :malformed-proposal (reason p)))))

(deftest wrong-subject-cannot-be-repaired-by-a-plausible-proof-label-test
  (doseq [other [:some-application :some-transaction :gesso.model.tx/other-catalog]]
    (let [r (review [(assoc ordinary :subject other)])]
      (is (= :invalid-evidence-proposals (:status r)))
      (is (= :wrong-formal-subject (get-in r [:proposals 0 :reason])))
      (is (false? (:review-package-complete? (entry r default-premise))))
      (no-proof-claims! r))))

(deftest unknown-premise-is-rejected-and-not-reflected-in-diagnostics-test
  (let [secret :private-secret-authorization-dependency
        r (review [(assoc ordinary :premise-id secret)])]
    (is (= :invalid-evidence-proposals (:status r)))
    (is (= :unknown-premise (get-in r [:proposals 0 :reason])))
    (is (nil? (get-in r [:proposals 0 :premise-id])))
    (is (not (.contains (pr-str r) (name secret))))
    (no-proof-claims! r)))

(deftest duplicate-same-premise-kind-and-hash-is-rejected-test
  (let [changed-issuer (assoc ordinary :claimed-issuer "second-claimed-issuer")
        r (review [ordinary changed-issuer])]
    (is (= :invalid-evidence-proposals (:status r)))
    (is (= :review-required (get-in r [:proposals 0 :status])))
    (is (= :duplicate-proposal (get-in r [:proposals 1 :reason])))
    (is (= 1 (:reviewable-proposals r)))
    (no-proof-claims! r)))

(deftest rejected-proposal-does-not-poison-later-valid-one-test
  (let [invalid (assoc ordinary :subject :wrong-subject)
        r (review [invalid ordinary])]
    (is (= :wrong-formal-subject (get-in r [:proposals 0 :reason])))
    (is (= :requires-independent-verification
           (get-in r [:proposals 1 :reason])))
    (is (= 1 (:reviewable-proposals r)))
    (is (= :invalid-evidence-proposals (:status r)))
    (no-proof-claims! r)))

(deftest distinct-alleged-artifacts-remain-unverified-even-if-both-reviewable-test
  (let [r (review [ordinary (assoc ordinary :artifact-sha256 sha-b)])]
    (is (= :independent-review-required (:status r)))
    (is (= 2 (:reviewable-proposals r)))
    (is (true? (:review-package-complete? (entry r default-premise))))
    (no-proof-claims! r)))

(deftest outer-input-must-be-a-bounded-vector-test
  (doseq [bad [nil {} '() (list ordinary) (seq [ordinary])
               (repeat 65 ordinary) (vec (repeat 65 ordinary))]]
    (is (= ::evidence/invalid-proposals
           (exception-type #(review bad)))))
  (let [proposals (mapv (fn [i]
                          (assoc ordinary :artifact-sha256 (format "%064x" (inc i))))
                        (range 64))
        r (review proposals)]
    (is (= 64 (:reviewable-proposals r)))
    (is (= 64 (count (:proposals r))))
    (is (= :independent-review-required (:status r)))
    (no-proof-claims! r)))

(deftest legitimately-rejected-domain-operation-does-not-invalidate-local-proof-test
  (let [state {[:requests private-id] create-doc}
        r (review state create-plan [ordinary])]
    (is (= :independent-review-required (:status r)))
    (is (= :checked (:local-argument-status r)))
    (is (= 1 (:reviewable-proposals r)))
    (no-proof-claims! r)))

(deftest contradictory-local-step-must-not-produce-an-applicable-proof-test
  (let [original model/step]
    (with-redefs [model/step (fn [state plan]
                              (assoc (original state plan) :status :rejected))]
      (let [r (review [ordinary])]
        (is (= :invalid-local-argument (:status r)))
        (is (= :failed (:local-argument-status r)))
        (is (= 1 (:reviewable-proposals r)))
        (no-proof-claims! r)))))

(deftest upstream-proof-flags-and-premise-statuses-fail-closed-test
  (let [base (premises/derive-obligation-ledger {} create-plan)]
    (doseq [corrupt [(fn [r] (assoc r :external-premises-discharged #{default-premise}))
                     (fn [r] (assoc r :general-refinement-proved? true))
                     (fn [r] (assoc r :certifies-physical-execution? true))
                     (fn [r] (assoc r :physical-observations-prove-backend-semantics? true))
                     (fn [r] (assoc-in r [:refinement-goal :status] :proved))
                     (fn [r] (assoc-in r [:open-premises 0 :status] :discharged))
                     (fn [r] (update r :open-premises pop))
                     (fn [r] (update-in r [:open-premises 0 :id]
                                        (constantly :invented-premise)))
                     (fn [r] (assoc-in r [:open-premises 0 :discharge-route] :fake-proof-route))
                     (fn [r] (assoc-in r [:open-premises 0 :owner] :caller))
                     (fn [r] (update-in r [:refinement-goal :depends-on] pop))
                     (fn [r] (assoc-in r [:local-argument :status] :failed))
                     (fn [r] (assoc r :status :certified))]]
      (is (= ::evidence/source-contract-drift
             (with-redefs [premises/derive-obligation-ledger
                           (fn [_ _] (corrupt base))]
               (exception-type #(review [ordinary]))))))))

(deftest upstream-same-count-premise-substitution-is-not-admitted-test
  (let [base (premises/derive-obligation-ledger {} create-plan)
        replaced (assoc-in base [:open-premises 0 :id] :plausible-other-premise)]
    (is (= 9 (count (:open-premises replaced))))
    (is (= ::evidence/source-contract-drift
           (with-redefs [premises/derive-obligation-ledger (fn [_ _] replaced)]
             (exception-type #(review [])))))))

(deftest review-and-errors-never-disclose-private-values-test
  (let [with-secrets (assoc ordinary :claimed-issuer "private-issuer-leak-1032"
                                   :artifact-version "private-artifact-version-1032")
        reviewed (review [with-secrets])
        invalid (review [(assoc with-secrets :artifact-sha256 "secret-digest-1032")])
        printed (pr-str [reviewed invalid])]
    (doseq [secret [private-id "private-issuer-leak-1032"
                    "private-artifact-version-1032" "secret-digest-1032"
                    sha-a]]
      (is (not (.contains printed secret))))
    (is (= :malformed-proposal (get-in invalid [:proposals 0 :reason])))
    (is (every? #(= #{:index :premise-id :status :reason
                      :authenticated? :proof-checked? :discharges-premise?}
                    (set (keys %)))
                (:proposals reviewed)))
    (let [base (premises/derive-obligation-ledger {} create-plan)
          err (with-redefs [premises/derive-obligation-ledger
                            (fn [_ _] (assoc base :general-refinement-proved? true))]
                (try (review [with-secrets]) nil
                     (catch clojure.lang.ExceptionInfo e e)))]
      (is (= ::evidence/source-contract-drift (:error/type (ex-data err))))
      (is (not (.contains (pr-str (ex-data err)) "private-issuer-leak-1032"))))))
