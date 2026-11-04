(ns gesso.assurance.application-induction-checkpoint-test
  "v1297: acceptance checkpoint for the SOURCE-RELATIVE application induction
   constructed by v1278-v1296. This checks a genuine admitted abstract Claim
   operation, a v1290-issued executor, a v1253-issued Ring handler, an actual
   closed application assembly, and a host-observed entry-point inventory.

   The four-operation abstract branching lifecycle is covered separately by
   gesso.assurance.proof.induction-acceptance-test (v1287). This checkpoint
   does not conflate that abstract example with the one-operation physically
   assembled Claim fixture. Both are useful, but neither proves all XTDB
   writers, deployment adoption, runtime authorization or physical refinement.

   No successful proof, route, or plan checker is mocked. Adversarial Var
   substitution is used only to show that fabricated reports fail closed."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.application-induction :as app]
            [gesso.assurance.inductive-report :as inductive]
            [gesso.assurance.proof.induction :as induction]
            [gesso.live.application-preflight.authoritative-coverage :as coverage]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.model.certified-plan :as certified-plan]
            [gesso.model.certified-plan-test]
            [gesso.model.inductive-plan :as plan]
            [gesso.model.inductive-plan-test]))

(def ^:private operation :example/claim)
(def ^:private other-operation :example/second-operation)
(def ^:private direct-writer :example/direct-xtdb-writer)
(def ^:private application-error
  :gesso.assurance.application-induction/invalid-application-induction)
(def ^:private coverage-error
  :gesso.live.application-preflight.authoritative-coverage/invalid-authoritative-coverage)
(def ^:private necessarily-open
  #{:host-authoritative-entry-point-inventory-is-complete
    :non-route-xtdb-writers-are-covered
    :deployed-router-and-executors-adopt-checked-snapshot
    :post-check-runtime-bindings-remain-stable
    :physical-prestate-and-initialization-correspondence
    :xtdb-commit-atomicity-and-authorization
    :abstract-to-physical-transition-refinement
    :independent-induction-checker-soundness})

(def ^:dynamic *source* nil)
(def ^:dynamic *context* nil)

(defn- fixture-helper [namespace symbol]
  (or (some-> (ns-resolve namespace symbol) deref)
      (throw (ex-info "Missing source-owned test fixture"
                      {:namespace namespace :symbol symbol}))))

(defn- build-real-subject []
  (let [environment ((fixture-helper 'gesso.model.certified-plan-test
                                     'environment))
        certified-source (:source environment)
        catalog ((fixture-helper 'gesso.model.inductive-plan-test
                                 'catalog-for)
                 (:construction certified-source))
        evidence ((fixture-helper 'gesso.model.inductive-plan-test
                                  'proof-package) catalog)
        plan-source {:inductive-source evidence
                     :certified-plan-source certified-source}
        executor (plan/build-inductive-plan-operation! plan-source)
        handler (certified/issue-certified-handler!
                 {:executor executor
                  :current-source certified-source
                  :request->input! (fn [_] {:mode :reconcile :binding {}})
                  :result->response! (fn [_ result]
                                       {:status 200 :body result})})]
    {:catalog catalog
     :evidence evidence
     :certified-source certified-source
     :plan-source plan-source
     :executor executor
     :handler handler}))

(defn- route-entry [kind handler]
  {:kind kind :handler handler})

(defn- observed-source [entries]
  (assoc-in *source*
            [:authoritative-coverage-source :enumerate-entry-points!]
            (fn [] entries)))

(use-fixtures
  :once
  (fn [run]
    (let [{:keys [evidence certified-source plan-source executor handler]
           :as subject} (build-real-subject)]
      (fixture/with-closed-application
        {:operations {operation
                      (fixture/trusted-operation
                       operation {:published-change-topics #{}})}}
        (fn [{:keys [assembly]}]
          (let [contract-source
                {:inductive-source evidence
                 :application-assembly assembly
                 :semantic-route-handlers {operation handler}
                 :certified-operations
                 {operation {:executor executor
                             :current-source certified-source}}}
                coverage-source
                {:inductive-contract-source contract-source
                 :enumerate-entry-points!
                 (fn [] {operation (route-entry :semantic-route handler)})}
                source
                {:authoritative-coverage-source coverage-source
                 :inductive-plans
                 {operation {:executor executor
                             :current-source plan-source}}}]
            (binding [*source* source
                      *context* (assoc subject :assembly assembly)]
              (run))))))))

(defn- failure [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo ex
      (ex-data ex))))

(defn- reject-type! [f expected-type]
  (let [data (failure f)]
    (is (map? data) (str "Expected typed rejection: " expected-type))
    (is (= expected-type (:error/type data)) (pr-str data))
    (is (keyword? (:reason data)) (pr-str data))
    data))

(defn- reject-app! [reason f]
  (let [data (reject-type! f application-error)]
    (is (= reason (:reason data)) (pr-str data))
    data))

(defn- checked
  ([] (app/check-application-induction! *source*))
  ([source] (app/check-application-induction! source)))

(defn- assert-scope-limits! [r]
  (is (= app/report-type (:type r)))
  (is (= app/report-version (:version r)))
  (is (= :fresh-induction-route-coverage-and-issued-plans-rechecked
         (:checking r)))
  (is (= :current-supplied-application-and-host-observation-only
         (:scope r)))
  (is (= :source-bound-conditionally-inductive (:abstract-invariant r)))
  (is (= :exact-issued-executor-and-abstract-construction
         (:plan-correspondence r)))
  (is (= :all-original-obligations-remain-open
         (:original-obligation-status r)))
  (is (set/subset? necessarily-open (:open-obligations r)))
  (is (seq (:open-premises r)))
  (is (seq (get (:original-open-obligation-ids-by-operation r)
                operation)))
  (doseq [field [:physical-write-path-completeness
                 :initialization-correspondence
                 :abstract-to-physical-transition-refinement
                 :deployed-router-adoption
                 :independent-checker-soundness
                 :production-certification]]
    (is (= :not-established (get r field)) (str field)))
  (is (= :none (:transaction-authority r)))
  (is (= :none (:authority r)))
  (is (not (contains? r :proved?)))
  (is (not (contains? r :valid?)))
  r)

(deftest checkpoint-abstract-invariant-is-conditionally-inductive
  (let [result (induction/check-induction! (:catalog *context*))
        abstract (inductive/build-inductive-report! (:evidence *context*))]
    (is (= #{operation} (:declared-operation-ids result)))
    (is (true? (:all-declared-invariants-conditionally-inductive?
                result)))
    (is (empty? (:unsupported-inductive-invariant-ids result)))
    (is (= :source-bound-conditional-abstract-safety (:verdict abstract)))
    (is (= :source-bound-conditional-induction
           (get-in abstract [:classification :checked-abstract :status])))
    (is (= :exact-source-relative-closure-checked
           (get-in abstract [:classification :dependency-closure :status])))
    (is (= :all-original-obligations-remain-open
           (get-in abstract [:classification :original-obligations :status])))
    (is (= :not-established
           (get-in abstract [:classification :open
                             :implementation-correspondence])))))

(deftest checkpoint-source-bound-application-join-is-current
  (let [r (assert-scope-limits! (checked))
        issued (:executor *context*)
        row (get-in r [:checked-inductive-plans operation])]
    (is (= #{operation} (:abstract-operation-ids r)
           (:issued-inductive-plan-ids r) (:semantic-route-ids r)))
    (is (= #{operation} (:observed-entry-point-ids r)))
    (is (= #{} (:observed-unclassified-entry-point-ids r)))
    (is (= :exact-for-supplied-host-snapshot
           (:observational-coverage r)))
    (is (= :exact-guarded-transition-construction (:source-join row)))
    (is (= :original-v1251-pre-submit-checker (:plan-gate row)))
    (is (identical? issued
                    (get-in *source* [:inductive-plans operation :executor])))
    (is (plan/inductive-plan-operation? issued))
    (is (not (contains? r :executor)))
    (is (not (contains? r :handler)))
    (doseq [field [:catalog-fingerprint :induction-fingerprint
                   :certificate-fingerprint :dependency-fingerprint]]
      (is (re-matches #"sha256:[a-f0-9]{64}" (get r field))
          (str "missing source fingerprint: " field)))))

(deftest checkpoint-copied-report-remains-non-authoritative
  (let [r (checked)]
    (is (= r (app/require-current-application-induction! (into {} r)
                                                       *source*)))
    (is (false? (plan/inductive-plan-operation? r)))
    (is (false? (certified-plan/checked-plan-operation? r)))
    (assert-scope-limits! r)))

(deftest checkpoint-missing-or-extra-declared-plan-is-rejected
  (doseq [[label table missing unexpected]
          [[:missing {} #{operation} #{}]
           [:extra (assoc (:inductive-plans *source*) other-operation
                          (get-in *source* [:inductive-plans operation]))
            #{} #{other-operation}]]]
    (testing (name label)
      (let [data (reject-app!
                  :incomplete-or-extra-inductive-plans
                  #(checked (assoc *source* :inductive-plans table)))]
        (is (= missing (:missing data)))
        (is (= unexpected (:unexpected data)))))))

(deftest checkpoint-equivalent-but-distinct-issued-executor-is-rejected
  (let [alternative (plan/build-inductive-plan-operation!
                     (:plan-source *context*))]
    (is (plan/inductive-plan-operation? alternative))
    (is (not (identical? (:executor *context*) alternative)))
    (reject-app!
     :issued-handler-plan-identity-mismatch
     #(checked (assoc-in *source* [:inductive-plans operation :executor]
                         alternative)))))

(deftest checkpoint-ordinary-certified-executor-cannot-replace-bound-plan
  (let [ordinary (certified-plan/build-certified-plan-operation!
                  (:certified-source *context*))]
    (is (certified-plan/checked-plan-operation? ordinary))
    (is (false? (plan/inductive-plan-operation? ordinary)))
    (reject-app!
     :issued-handler-plan-identity-mismatch
     #(checked (assoc-in *source* [:inductive-plans operation :executor]
                         ordinary)))))

(deftest checkpoint-current-proof-handle-identity-is-required
  (doseq [handle [:certificate-handle :dependency-handle]]
    (testing (name handle)
      (reject-app!
       :mismatched-induction-source
       #(checked (assoc-in *source*
                           [:inductive-plans operation :current-source
                            :inductive-source handle] nil))))))

(deftest checkpoint-same-id-with-altered-abstract-transition-is-rejected
  (let [path [:authoritative-coverage-source :inductive-contract-source
              :inductive-source :catalog :operations 0 :construction
              :guard :expression]
        changed (assoc-in *source* path [:literal true])]
    (is (not= (get-in *source* path) (get-in changed path)))
    (is (= #{operation} (set (keys (:inductive-plans changed)))))
    (let [data (failure #(checked changed))]
      (is (map? data))
      (is (qualified-keyword? (:error/type data)) (pr-str data))
      (is (keyword? (:reason data)) (pr-str data)))))

(deftest checkpoint-missing-route-is-rejected
  (let [data (reject-type! #(checked (observed-source {}))
                           coverage-error)]
    (is (= :missing-observed-semantic-routes (:reason data)))
    (is (= #{operation} (:missing-route-ids data)))))

(deftest checkpoint-handler-swap-is-rejected
  (let [replacement (fn [_] {:status 200})
        data (reject-type!
              #(checked (observed-source
                         {operation (route-entry :semantic-route replacement)}))
              coverage-error)]
    (is (= :substituted-semantic-route-handler (:reason data)))))

(deftest checkpoint-reclassified-handler-is-rejected
  (let [data (reject-type!
              #(checked (observed-source
                         {operation (route-entry :unclassified
                                                 (:handler *context*))}))
              coverage-error)]
    (is (= :misclassified-semantic-route (:reason data)))))

(deftest checkpoint-extra-observed-writer-is-open-not-certified
  (let [extra (fn [_] {:status 200})
        source (observed-source
                {operation (route-entry :semantic-route (:handler *context*))
                 direct-writer (route-entry :unclassified extra)})
        r (assert-scope-limits! (checked source))]
    (is (= #{operation direct-writer} (:observed-entry-point-ids r)))
    (is (= #{direct-writer} (:observed-unclassified-entry-point-ids r)))
    (is (= :observed-uncovered (:unclassified-entry-point-status r)))
    (is (contains? (:open-obligations r)
                   :observed-entry-points-outside-inductive-contract))
    (is (= #{operation} (:issued-inductive-plan-ids r)))))

(deftest checkpoint-clean-host-inventory-does-not-prove-no-hidden-writers
  (let [r (checked)]
    (is (= :none-observed (:unclassified-entry-point-status r)))
    (is (= :not-established (:physical-write-path-completeness r)))
    (is (contains? (:open-obligations r)
                   :host-authoritative-entry-point-inventory-is-complete))
    (is (contains? (:open-obligations r)
                   :non-route-xtdb-writers-are-covered))
    (is (= :not-established (:deployed-router-adoption r)))))

(deftest checkpoint-host-snapshot-change-invalidates-prior-report
  (let [inventory (atom {operation
                         (route-entry :semantic-route (:handler *context*))})
        source (assoc-in *source*
                         [:authoritative-coverage-source :enumerate-entry-points!]
                         (fn [] @inventory))
        prior (checked source)]
    (is (= :none-observed (:unclassified-entry-point-status prior)))
    (swap! inventory assoc direct-writer
           (route-entry :unclassified (fn [_] nil)))
    (let [now (checked source)]
      (is (= :observed-uncovered (:unclassified-entry-point-status now)))
      (is (not= prior now))
      (reject-app!
       :stale-or-forged-application-induction-report
       #(app/require-current-application-induction! prior source)))))

(deftest checkpoint-copying-or-editing-report-cannot-fabricate-authority
  (let [r (checked)]
    (doseq [[label mutant]
            [[:claimed-proof (assoc r :proved? true)]
             [:hidden-obligation (update r :open-obligations disj
                                         :abstract-to-physical-transition-refinement)]
             [:claimed-production (assoc r :production-certification :proved)]
             [:missing-plan (update r :checked-inductive-plans dissoc operation)]
             [:metadata (with-meta r {:certified true})]]]
      (testing (name label)
        ;; Clojure equality intentionally ignores metadata. An annotated
        ;; copy is equal as data, but must still fail the source check.
        (if (= label :metadata)
          (is (= {:certified true} (meta mutant)))
          (is (not= r mutant)))
        (reject-app!
         :stale-or-forged-application-induction-report
         #(app/require-current-application-induction! mutant *source*))))))

(deftest checkpoint-fabricated-upstream-coverage-cannot-admit-invalid-source
  (with-redefs [coverage/check-authoritative-coverage!
                (fn [_] {:type coverage/report-type
                         :checking :fresh-source-derived-contract-and-host-enumeration
                         :valid? true :authority :granted})]
    (reject-app! :substituted-upstream-checker #(checked))))

(deftest checkpoint-extra-real-assembled-operation-is-not-inherited-by-proof
  ;; An additional real assembly entry point cannot be covered by keeping the
  ;; old single-operation proof and route sources. In particular this check
  ;; does not classify the new operation as an abstract theorem of the old
  ;; one-operation catalog.
  (fixture/with-closed-application
    {:operations
     {operation (fixture/trusted-operation
                 operation {:published-change-topics #{}})
      other-operation (fixture/trusted-operation
                       other-operation {:published-change-topics #{}})}}
    (fn [{:keys [assembly]}]
      (let [mutated (assoc-in *source*
                              [:authoritative-coverage-source
                               :inductive-contract-source
                               :application-assembly]
                              assembly)
            data (failure #(checked mutated))]
        (is (map? data))
        (is (qualified-keyword? (:error/type data)) (pr-str data))
        (is (keyword? (:reason data)) (pr-str data))))))
