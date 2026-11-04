(ns gesso.assurance.application-induction-test
  "v1295: permanent adversarial assurance tests for the source-derived
   whole-application induction join introduced by v1294.

   This namespace uses the actual v1251 certified-plan source, v1290 issued
   induction-bound executor, v1253 issued Ring handler, closed Gesso application
   assembly, v1280 conditional induction certificate, v1282 exact dependency
   closure, and v1292 host-observed route inventory. No positive proof or
   binding checker is stubbed. Repetition of inexpensive malformed-map tests
   targets the genuine v1294 shape validator rather than re-running the entire
   proof pipeline dozens of times.

   The report concerns only a supplied current snapshot. An apparently complete
   host inventory is not proof that all XTDB writers were enumerated, nor is a
   checked candidate evidence of physical transaction refinement, authorization,
   or deployment adoption. Its data cannot confer execution authority."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.application-induction :as app]
            [gesso.live.application-preflight.authoritative-coverage :as coverage]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.model.certified-plan :as certified-plan]
            [gesso.model.certified-plan-test]
            [gesso.model.inductive-plan :as induction-plan]
            [gesso.model.inductive-plan-test]))

(def ^:private operation :example/claim)
(def ^:private outside-operation :example/direct-xtdb-writer)
(def ^:private other-operation :example/extra-operation)
(def ^:private error-type
  :gesso.assurance.application-induction/invalid-application-induction)
(def ^:dynamic *base* nil)
(def ^:dynamic *context* nil)

(defn- test-helper [test-ns name]
  (or (some-> (ns-resolve test-ns name) deref)
      (throw (ex-info "Required source-owning test fixture is missing"
                      {:test-namespace test-ns :name name}))))

(defn- make-context []
  (let [original ((test-helper 'gesso.model.certified-plan-test 'environment))
        plan-source (:source original)
        construction (:construction plan-source)
        catalog ((test-helper 'gesso.model.inductive-plan-test 'catalog-for)
                 construction)
        proof ((test-helper 'gesso.model.inductive-plan-test 'proof-package)
               catalog)
        induction-source {:inductive-source proof
                          :certified-plan-source plan-source}
        executor (induction-plan/build-inductive-plan-operation!
                  induction-source)
        handler (certified/issue-certified-handler!
                 {:executor executor
                  :current-source plan-source
                  :request->input! (fn [_] {:mode :reconcile :binding {}})
                  :result->response! (fn [_ result] {:status 200 :body result})})]
    {:proof proof
     :plan-source plan-source
     :induction-source induction-source
     :executor executor
     :handler handler}))

(use-fixtures
  :once
  (fn [run]
    (let [{:keys [proof plan-source induction-source executor handler]
           :as context} (make-context)]
      (fixture/with-closed-application
        {:operations {operation
                      (fixture/trusted-operation
                       operation {:published-change-topics #{}})}}
        (fn [{:keys [assembly]}]
          (let [contract-source
                {:inductive-source proof
                 :application-assembly assembly
                 :semantic-route-handlers {operation handler}
                 :certified-operations
                 {operation {:executor executor :current-source plan-source}}}
                entry-point-source
                {:inductive-contract-source contract-source
                 :enumerate-entry-points!
                 (fn [] {operation {:kind :semantic-route :handler handler}})}
                base {:authoritative-coverage-source entry-point-source
                      :inductive-plans
                      {operation {:executor executor
                                  :current-source induction-source}}}]
            (binding [*context* (assoc context :assembly assembly)
                      *base* base]
              (run))))))))

(defn- failure-data [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- reject! [reason f]
  (let [data (failure-data f)]
    (is (map? data) (str "Missing typed rejection: " reason))
    (is (= error-type (:error/type data)) (pr-str data))
    (is (= reason (:reason data)) (pr-str data))
    data))

(defn- check
  ([] (app/check-application-induction! *base*))
  ([source] (app/check-application-induction! source)))

(defn- plan-binding [source]
  (get-in source [:inductive-plans operation]))

(defn- with-plan [source operation-id binding]
  (assoc source :inductive-plans {operation-id binding}))

(defn- with-observed [entries]
  (assoc-in *base* [:authoritative-coverage-source :enumerate-entry-points!]
            (fn [] entries)))

(defn- entry [kind handler]
  {:kind kind :handler handler})

(def ^:private permanent-open
  #{:host-authoritative-entry-point-inventory-is-complete
    :non-route-xtdb-writers-are-covered
    :deployed-router-and-executors-adopt-checked-snapshot
    :post-check-runtime-bindings-remain-stable
    :physical-prestate-and-initialization-correspondence
    :xtdb-commit-atomicity-and-authorization
    :abstract-to-physical-transition-refinement
    :independent-induction-checker-soundness})

(defn- assert-conditional-only! [result]
  (is (= app/report-type (:type result)))
  (is (= app/report-version (:version result)))
  (is (= :fresh-induction-route-coverage-and-issued-plans-rechecked
         (:checking result)))
  (is (= :current-supplied-application-and-host-observation-only
         (:scope result)))
  (is (= #{operation} (:abstract-operation-ids result)))
  (is (= #{operation} (:issued-inductive-plan-ids result)))
  (is (= #{operation} (:semantic-route-ids result)))
  (is (= :exact-issued-executor-and-abstract-construction
         (:plan-correspondence result)))
  (is (= :source-bound-conditionally-inductive
         (:abstract-invariant result)))
  (is (= :all-original-obligations-remain-open
         (:original-obligation-status result)))
  (is (set/subset? permanent-open (:open-obligations result)))
  (is (seq (:open-premises result)))
  (is (seq (:open-obligations result)))
  (is (= #{operation}
         (set (keys (:original-open-obligation-ids-by-operation result)))))
  (is (seq (get (:original-open-obligation-ids-by-operation result)
                operation)))
  (doseq [field [:physical-write-path-completeness
                 :initialization-correspondence
                 :abstract-to-physical-transition-refinement
                 :deployed-router-adoption
                 :independent-checker-soundness
                 :production-certification]]
    (is (= :not-established (get result field)) (str field)))
  (is (= :none (:transaction-authority result)))
  (is (= :none (:authority result)))
  (is (not (contains? result :proved?)))
  (is (not (contains? result :valid?)))
  result)

(deftest actual-application-plan-and-handler-join-is-source-relative
  (let [r (assert-conditional-only! (check))
        row (get-in r [:checked-inductive-plans operation])]
    (is (= #{operation} (set (keys (:checked-inductive-plans r)))))
    (is (= operation (:operation row)))
    (is (= :exact-guarded-transition-construction (:source-join row)))
    (is (= :original-v1251-pre-submit-checker (:plan-gate row)))
    (is (= :one-registered-plan-of-declared-abstract-machine (:scope row)))
    (is (seq (:open-obligations row)))
    (is (seq (:open-premises row)))
    (doseq [field [:catalog-fingerprint :induction-fingerprint
                   :certificate-fingerprint :dependency-fingerprint]]
      (is (re-matches #"sha256:[0-9a-f]{64}" (get r field))
          (str "missing source fingerprint " field)))))

(deftest copied-current-report-is-explanation-not-authority
  (let [r (check)]
    (is (= r (app/require-current-application-induction! r *base*)))
    (is (= r (app/require-current-application-induction!
              (into {} r) (into {} *base*))))
    (assert-conditional-only! r)))

(deftest stale-or-fabricated-report-is-rejected
  (let [r (check)]
    (doseq [[label modified]
            [[:hidden-obligation (update r :open-obligations disj
                                         :xtdb-commit-atomicity-and-authorization)]
             [:fake-authority (assoc r :authority :granted)]
             [:false-completeness (assoc r :physical-write-path-completeness
                                         :proved)]
             [:forged-status (assoc r :valid? true)]
             [:altered-version (assoc r :version 999)]
             [:removed-plan (update r :checked-inductive-plans dissoc operation)]
             [:metadata (with-meta r {:admitted true})]]]
      (testing (name label)
        (reject! :stale-or-forged-application-induction-report
                 #(app/require-current-application-induction!
                   modified *base*))))))

(deftest physical-writer-completeness-remains-open-when-inventory-is-clean
  (let [r (check)]
    (is (= #{operation} (:observed-entry-point-ids r)))
    (is (= #{} (:observed-unclassified-entry-point-ids r)))
    (is (= :none-observed (:unclassified-entry-point-status r)))
    (is (= :not-established (:physical-write-path-completeness r)))
    (is (contains? (:open-obligations r)
                   :host-authoritative-entry-point-inventory-is-complete))
    (is (contains? (:open-obligations r)
                   :non-route-xtdb-writers-are-covered))))

(deftest host-observed-extra-writer-is-uncovered-not-certified
  (let [other-handler (fn [_] {:status 200})
        source (with-observed
                 {operation (entry :semantic-route (:handler *context*))
                  outside-operation (entry :unclassified other-handler)})
        r (check source)]
    (is (= #{outside-operation}
           (:observed-unclassified-entry-point-ids r)))
    (is (= #{operation outside-operation} (:observed-entry-point-ids r)))
    (is (= :observed-uncovered (:unclassified-entry-point-status r)))
    (is (= #{operation} (:issued-inductive-plan-ids r)))
    (assert-conditional-only! r)))

(deftest later-host-snapshot-must-not-inherit-prior-report
  (let [observed (atom {operation
                        (entry :semantic-route (:handler *context*))})
        source (assoc-in *base*
                         [:authoritative-coverage-source :enumerate-entry-points!]
                         (fn [] @observed))
        prior (check source)]
    (swap! observed assoc outside-operation
           (entry :unclassified (fn [_] {:status 200})))
    (is (= :none-observed (:unclassified-entry-point-status prior)))
    (is (= :observed-uncovered
           (:unclassified-entry-point-status (check source))))
    (reject! :stale-or-forged-application-induction-report
             #(app/require-current-application-induction! prior source))))

(deftest missing-or-additional-inductive-plan-id-fails-closed
  (doseq [[label table missing unexpected]
          [[:missing {} #{operation} #{}]
           [:additional (assoc (:inductive-plans *base*)
                               other-operation (plan-binding *base*))
            #{} #{other-operation}]]]
    (testing (name label)
      (let [data (reject! :incomplete-or-extra-inductive-plans
                          #(check (assoc *base* :inductive-plans table)))]
        (is (= missing (:missing data)))
        (is (= unexpected (:unexpected data)))))))

(deftest unissued-executor-cannot-join-checked-handler
  (let [ordinary (certified-plan/build-certified-plan-operation!
                  (:plan-source *context*))
        replacement (assoc-in *base* [:inductive-plans operation :executor]
                              ordinary)]
    (is (certified-plan/checked-plan-operation? ordinary))
    (is (false? (induction-plan/inductive-plan-operation? ordinary)))
    (reject! :issued-handler-plan-identity-mismatch
             #(check replacement))))

(deftest separately-issued-valid-plan-cannot-impersonate-bound-plan
  (let [other (induction-plan/build-inductive-plan-operation!
               (:induction-source *context*))]
    (is (induction-plan/inductive-plan-operation? other))
    (is (not (identical? other (:executor *context*))))
    (reject! :issued-handler-plan-identity-mismatch
             #(check (assoc-in *base* [:inductive-plans operation :executor]
                               other)))))

(deftest copied-description-cannot-replace-issued-executor
  (let [description (induction-plan/describe-inductive-plan
                     (:executor *context*))]
    (is (map? description))
    (reject! :issued-handler-plan-identity-mismatch
             #(check (assoc-in *base* [:inductive-plans operation :executor]
                               description)))))

(deftest unchanged-plan-id-does-not-excuse-different-source
  (let [source (assoc-in *base*
                         [:inductive-plans operation :current-source
                          :certified-plan-source :field-map]
                         {:wrong/source :request/status})]
    (reject! :issued-handler-plan-identity-mismatch #(check source))))

(deftest altered-induction-source-handle-is-not-equal-to-live-proof
  (doseq [field [:certificate-handle :dependency-handle]]
    (testing (str field)
      (reject! :mismatched-induction-source
               #(check (assoc-in
                        *base*
                        [:inductive-plans operation :current-source
                         :inductive-source field]
                        nil))))))

(deftest changed-abstract-transition-fails-before-plan-join
  (let [source (assoc-in
                *base*
                [:authoritative-coverage-source :inductive-contract-source
                 :inductive-source :catalog :operations 0 :construction
                 :guard :expression]
                [:literal true])]
    (is (some? (failure-data #(check source))))))

(deftest missing-or-swapped-observed-route-is-rejected
  (let [missing (with-observed {})
        swapped (with-observed
                 {operation (entry :semantic-route (fn [_] {:status 200}))})]
    (is (= :missing-observed-semantic-routes
           (:reason (failure-data #(check missing)))))
    (is (= :substituted-semantic-route-handler
           (:reason (failure-data #(check swapped)))))))

(deftest wrong-route-kind-is-rejected
  (is (= :misclassified-semantic-route
         (:reason
          (failure-data
           #(check (with-observed
                     {operation (entry :unclassified
                                       (:handler *context*))})))))))

(deftest fabricated-sibling-checker-cannot-supply-successful-coverage
  (with-redefs [coverage/check-authoritative-coverage!
                (fn [_] {:type coverage/report-type
                         :authority :granted
                         :physical-write-path-completeness :proved})]
    (reject! :substituted-upstream-checker #(check))))

(deftest fabricated-plan-freshness-checker-cannot-supply-valid-executor
  (with-redefs [induction-plan/require-current-inductive-plan!
                (fn [& _] {:type induction-plan/report-type
                           :authority :granted})]
    (reject! :substituted-upstream-checker #(check))))

(deftest top-level-source-shape-rejects-untrusted-claims
  (doseq [source [nil [] (sorted-map) (with-meta *base* {:proof true})
                  (dissoc *base* :inductive-plans)
                  (assoc *base* :proved? true)
                  (assoc *base* :transaction-authority :granted)]]
    (let [data (failure-data #(check source))]
      (is (= error-type (:error/type data)) (pr-str data))
      (is (contains? #{:expected-unannotated-plain-map
                       :wrong-source-keys} (:reason data)) (pr-str data)))))

(deftest plan-table-shape-rejects-untrusted-claims
  (doseq [table [nil [] (sorted-map)
                 (with-meta (:inductive-plans *base*) {:admitted true})]]
    (reject! :invalid-inductive-plan-table
             #(check (assoc *base* :inductive-plans table)))))

(deftest binding-and-proof-source-shapes-are-exact
  (let [binding (plan-binding *base*)
        current (:current-source binding)
        source (:inductive-source current)]
    (doseq [wrong [(dissoc binding :executor)
                   (assoc binding :valid? true)
                   (with-meta binding {:authoritative true})]]
      (let [data (failure-data #(check (with-plan *base* operation wrong)))]
        (is (some? data) (pr-str wrong))))
    (doseq [wrong [(dissoc current :inductive-source)
                   (assoc current :trusted? true)
                   (with-meta current {:proved true})]]
      (let [data (failure-data
                  #(check (assoc-in *base*
                                    [:inductive-plans operation :current-source]
                                    wrong)))]
        (is (some? data) (pr-str wrong))))
    (doseq [wrong [(dissoc source :catalog)
                   (assoc source :authorization-proved true)
                   (with-meta source {:certified true})]]
      (let [data (failure-data
                  #(check (assoc-in *base*
                                    [:inductive-plans operation :current-source
                                     :inductive-source] wrong)))]
        (is (some? data) (pr-str wrong))))))

(deftest certified-route-plan-identity-is-not-derived-from-operation-key-alone
  (let [r (check)
        row (get-in r [:checked-inductive-plans operation])]
    (is (= #{operation} (:issued-inductive-plan-ids r)))
    (is (= operation (:operation row)))
    (is (= (:transition-fingerprint row)
           (:transition-fingerprint
            (induction-plan/describe-inductive-plan
             (:executor *context*)))))
    (is (not (contains? row :executor)))
    (is (not (contains? r :executor)))))
