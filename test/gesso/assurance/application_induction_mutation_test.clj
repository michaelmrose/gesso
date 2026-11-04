(ns gesso.assurance.application-induction-mutation-test
  "v1296: systematic source-owned mutations of the v1294 application induction
   join. A REAL v1251 construction, v1290 issued executor, v1253 issued Ring
   handler, closed Gesso assembly, live v1280 certificate, v1282 exact
   dependency handle, and v1292 host-observed inventory form the baseline.

   Mutations independently attack catalog/plan/route/observation identity,
   source freshness, declaration completeness, and explanatory report fields.
   Failures are checked through the actual source-derived interfaces. Cheap
   malformed-map cases exercise the genuine v1294 shape gate directly, rather
   than repeatedly rebuilding induction proofs. Valid host observations are
   allowed but MUST remain explicitly outside production certification.

   Scope: a supplied in-process snapshot, NOT all real XTDB writers, a live
   router installation, physical transition refinement, or checker soundness."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.application-induction :as app]
            [gesso.live.application-preflight.authoritative-coverage :as coverage]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.model.certified-plan :as certified-plan]
            [gesso.model.certified-plan-test]
            [gesso.model.inductive-plan :as inductive-plan]
            [gesso.model.inductive-plan-test]))

(def ^:private op :example/claim)
(def ^:private extra :example/other-writer)
(def ^:private other :example/other-operation)
(def ^:private app-error
  :gesso.assurance.application-induction/invalid-application-induction)
(def ^:private always-open
  #{:host-authoritative-entry-point-inventory-is-complete
    :non-route-xtdb-writers-are-covered
    :deployed-router-and-executors-adopt-checked-snapshot
    :post-check-runtime-bindings-remain-stable
    :physical-prestate-and-initialization-correspondence
    :xtdb-commit-atomicity-and-authorization
    :abstract-to-physical-transition-refinement
    :independent-induction-checker-soundness})

(def ^:dynamic *base* nil)
(def ^:dynamic *context* nil)

(defn- helper [ns-sym fn-sym]
  (or (some-> (ns-resolve ns-sym fn-sym) deref)
      (throw (ex-info "Missing original source-owning test fixture"
                      {:namespace ns-sym :function fn-sym}))))

(defn- build-real-context []
  (let [environment ((helper 'gesso.model.certified-plan-test 'environment))
        plan-source (:source environment)
        catalog ((helper 'gesso.model.inductive-plan-test 'catalog-for)
                 (:construction plan-source))
        proof ((helper 'gesso.model.inductive-plan-test 'proof-package) catalog)
        plan-source+proof {:inductive-source proof
                           :certified-plan-source plan-source}
        executor (inductive-plan/build-inductive-plan-operation!
                  plan-source+proof)
        handler (certified/issue-certified-handler!
                 {:executor executor
                  :current-source plan-source
                  :request->input! (fn [_] {:mode :reconcile :binding {}})
                  :result->response! (fn [_ result]
                                       {:status 200 :body result})})]
    {:proof proof
     :plan-source plan-source
     :plan-source+proof plan-source+proof
     :executor executor
     :handler handler}))

(use-fixtures
  :once
  (fn [run]
    (let [{:keys [proof plan-source plan-source+proof executor handler]
           :as ctx} (build-real-context)]
      (fixture/with-closed-application
        {:operations {op (fixture/trusted-operation
                          op {:published-change-topics #{}})}}
        (fn [{:keys [assembly]}]
          (let [contract {:inductive-source proof
                          :application-assembly assembly
                          :semantic-route-handlers {op handler}
                          :certified-operations
                          {op {:executor executor :current-source plan-source}}}
                coverage-source {:inductive-contract-source contract
                                 :enumerate-entry-points!
                                 (fn [] {op {:kind :semantic-route
                                             :handler handler}})}
                source {:authoritative-coverage-source coverage-source
                        :inductive-plans
                        {op {:executor executor
                             :current-source plan-source+proof}}}]
            (binding [*base* source
                      *context* (assoc ctx :assembly assembly)]
              (run))))))))

(defn- failure [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo ex
      (ex-data ex))))

(defn- check! [source]
  (app/check-application-induction! source))

(defn- error-of [source]
  (failure #(check! source)))

(defn- reject-any! [source]
  (let [data (error-of source)]
    (is (map? data) "Mutation was accepted or failed without a typed ex-info")
    (is (qualified-keyword? (:error/type data)) (pr-str data))
    (is (keyword? (:reason data)) (pr-str data))
    data))

(defn- reject-app! [source expected]
  (let [data (reject-any! source)]
    (is (= app-error (:error/type data)) (pr-str data))
    (is (= expected (:reason data)) (pr-str data))
    data))

(defn- observed [entries]
  (assoc-in *base*
            [:authoritative-coverage-source :enumerate-entry-points!]
            (fn [] entries)))

(defn- entry [kind handler]
  {:kind kind :handler handler})

(defn- base-entry []
  (entry :semantic-route (:handler *context*)))

(defn- plan-binding []
  (get-in *base* [:inductive-plans op]))

(defn- assert-no-physical-authority! [r]
  (is (= app/report-type (:type r)))
  (is (= app/report-version (:version r)))
  (is (= :source-bound-conditionally-inductive (:abstract-invariant r)))
  (is (= :exact-issued-executor-and-abstract-construction
         (:plan-correspondence r)))
  (is (= #{op} (:abstract-operation-ids r)))
  (is (= #{op} (:issued-inductive-plan-ids r)))
  (is (= #{op} (:semantic-route-ids r)))
  (is (= :all-original-obligations-remain-open
         (:original-obligation-status r)))
  (is (set/subset? always-open (:open-obligations r)))
  (is (seq (:open-premises r)))
  (is (seq (get (:original-open-obligation-ids-by-operation r) op)))
  (doseq [k [:physical-write-path-completeness
             :initialization-correspondence
             :abstract-to-physical-transition-refinement
             :deployed-router-adoption
             :independent-checker-soundness
             :production-certification]]
    (is (= :not-established (get r k)) (str k)))
  (is (= :none (:transaction-authority r)))
  (is (= :none (:authority r)))
  (is (not (contains? r :proved?)))
  (is (not (contains? r :valid?)))
  r)

(deftest unmutated-source-is-a-real-positive-control
  (let [r (assert-no-physical-authority! (check! *base*))]
    (is (= #{op} (set (keys (:checked-inductive-plans r)))))
    (is (= :exact-for-supplied-host-snapshot
           (:observational-coverage r)))
    (is (= :none-observed (:unclassified-entry-point-status r)))
    (is (= r (app/require-current-application-induction! r *base*)))))

(deftest source-map-copy-does-not-change-live-evidence-identities
  (let [copy (into {} *base*)
        copy-plans (update copy :inductive-plans #(into {} %))
        baseline (check! *base*)]
    (is (= baseline (check! copy-plans)))
    (is (identical? (:executor *context*)
                    (get-in copy-plans [:inductive-plans op :executor])))
    (assert-no-physical-authority! baseline)))

(deftest mutation-matrix-rejects-incomplete-and-overcomplete-operation-tables
  (doseq [[label updated missing unexpected]
          [[:omitted {} #{op} #{}]
           [:added (assoc (:inductive-plans *base*) other (plan-binding))
            #{} #{other}]
           [:rekeyed {other (plan-binding)} #{op} #{other}]]]
    (testing (name label)
      (let [data (reject-app!
                  (assoc *base* :inductive-plans updated)
                  :incomplete-or-extra-inductive-plans)]
        (is (= missing (:missing data)))
        (is (= unexpected (:unexpected data)))))))

(deftest mutation-matrix-rejects-cheap-source-shape-injections
  ;; All of these fail BEFORE expensive original-proof reconstruction.
  (doseq [[label bad]
          [[:absent-coverage (dissoc *base* :authoritative-coverage-source)]
           [:absent-plan-table (dissoc *base* :inductive-plans)]
           [:injected-proof-bit (assoc *base* :proved? true)]
           [:injected-valid-bit (assoc *base* :valid? true)]
           [:annotated (with-meta *base* {:certified true})]
           [:non-plain (sorted-map :authoritative-coverage-source
                                   (:authoritative-coverage-source *base*)
                                   :inductive-plans (:inductive-plans *base*))]
           [:nil nil]
           [:vector []]]]
    (testing (name label)
      (let [data (error-of bad)]
        (is (= app-error (:error/type data)) (pr-str data))
        (is (contains? #{:wrong-source-keys
                         :expected-unannotated-plain-map}
                       (:reason data)) (pr-str data))))))

(deftest mutation-matrix-rejects-non-plain-or-annotated-plan-tables
  (doseq [[label table]
          [[:nil nil]
           [:vector []]
           [:metadata (with-meta (:inductive-plans *base*)
                        {:admitted true})]
           [:sorted (into (sorted-map) (:inductive-plans *base*))]]]
    (testing (name label)
      (reject-app! (assoc *base* :inductive-plans table)
                   :invalid-inductive-plan-table))))

(deftest mutation-matrix-rejects-substituted-executor-objects
  (let [original (:executor *context*)
        second (inductive-plan/build-inductive-plan-operation!
                (:plan-source+proof *context*))
        ordinary (certified-plan/build-certified-plan-operation!
                  (:plan-source *context*))
        copied-meta (with-meta (fn [& _] :forged) (meta original))
        fake-report (inductive-plan/describe-inductive-plan original)]
    (is (inductive-plan/inductive-plan-operation? original))
    (is (inductive-plan/inductive-plan-operation? second))
    (is (not (identical? second original)))
    (is (certified-plan/checked-plan-operation? ordinary))
    (doseq [[label candidate]
            [[:new-valid-executor second]
             [:old-certified-executor ordinary]
             [:unregistered-fn (fn [& _] :forged)]
             [:copied-function-metadata copied-meta]
             [:descriptive-map fake-report]]]
      (testing (name label)
        (reject-app!
          (assoc-in *base* [:inductive-plans op :executor] candidate)
          :issued-handler-plan-identity-mismatch)))))

(deftest mutation-matrix-rejects-identity-shared-but-misbound-plan-sources
  (let [source-path [:inductive-plans op :current-source
                     :certified-plan-source]]
    (doseq [[label mutate]
            [[:wrong-field-map #(assoc-in % [:field-map] {:wrong :field})]
             [:missing-plan-callback #(dissoc % :current-document!)]
             [:altered-command #(assoc-in % [:command-operation] other)]]]
      (testing (name label)
        (let [original (get-in *base* source-path)
              altered (assoc-in *base* source-path (mutate original))]
          (reject-app! altered :issued-handler-plan-identity-mismatch))))))

(deftest mutation-matrix-rejects-mismatched-proof-handle-identities
  (let [source-path [:inductive-plans op :current-source :inductive-source]]
    (doseq [handle [:certificate-handle :dependency-handle]]
      (testing (name handle)
        (reject-app!
          (assoc-in *base* (conj source-path handle) nil)
          :mismatched-induction-source)))))

(deftest changing-both-plan-and-contract-proof-sources-does-not-reuse-certificate
  ;; A mutually consistent forged *description* is still not a new issued
  ;; certificate, dependency handle, and plan from the altered source.
  (let [path [:catalog :id]
        mutate #(assoc-in % path :example/mutated-induction-catalog)
        cpath [:authoritative-coverage-source :inductive-contract-source
               :inductive-source]
        ppath [:inductive-plans op :current-source :inductive-source]
        mutation (-> *base*
                     (update-in cpath mutate)
                     (update-in ppath mutate))]
    (is (= (get-in mutation (into cpath path))
           (get-in mutation (into ppath path))))
    (is (not= (get-in *base* (into cpath path))
              (get-in mutation (into cpath path))))
    (reject-any! mutation)))

(deftest changed-guard-with-original-identifiers-is-not-an-admitted-transition
  (let [path [:authoritative-coverage-source :inductive-contract-source
              :inductive-source :catalog :operations 0 :construction
              :guard :expression]
        before (get-in *base* path)
        mutated (assoc-in *base* path [:literal true])]
    (is (not= before (get-in mutated path)))
    (is (= #{op} (set (keys (:inductive-plans mutated)))))
    (reject-any! mutated)))

(deftest changed-initial-condition-and-invariant-reject-old-evidence
  (let [path [:authoritative-coverage-source :inductive-contract-source
              :inductive-source :catalog]]
    (doseq [[label mutate]
            [[:initial #(assoc-in % [:initial-condition :expression]
                                  [:literal true])]
             [:invariant #(assoc-in % [:invariants :example/valid-status
                                       :expression] [:literal true])]]]
      (testing (name label)
        (let [old (get-in *base* path)
              changed (assoc-in *base* path (mutate old))]
          (is (not= old (get-in changed path)))
          (reject-any! changed))))))

(deftest observational-mutation-missing-semantic-route-is-rejected
  (let [result (error-of (observed {}))]
    (is (= :gesso.live.application-preflight.authoritative-coverage/invalid-authoritative-coverage
           (:error/type result)))
    (is (= :missing-observed-semantic-routes (:reason result)))
    (is (= #{op} (:missing-route-ids result)))))

(deftest observational-mutation-swapped-or-relabeled-route-is-rejected
  (doseq [[label entries reason]
          [[:handler-swap {op (entry :semantic-route (fn [_] {:status 200}))}
            :substituted-semantic-route-handler]
           [:route-misclassified {op (entry :unclassified (:handler *context*))}
            :misclassified-semantic-route]
           [:unauthorized-semantic-route
            {op (base-entry)
             other (entry :semantic-route (fn [_] {:status 200}))}
            :undeclared-semantic-route]]]
    (testing (name label)
      (let [result (error-of (observed entries))]
        (is (= :gesso.live.application-preflight.authoritative-coverage/invalid-authoritative-coverage
               (:error/type result)) (pr-str result))
        (is (= reason (:reason result)) (pr-str result))))))

(deftest observational-extra-writer-is-visible-but-not-certified
  (let [r (assert-no-physical-authority!
           (check! (observed {op (base-entry)
                              extra (entry :unclassified
                                           (fn [_] {:status 200}))})))]
    (is (= #{op extra} (:observed-entry-point-ids r)))
    (is (= #{extra} (:observed-unclassified-entry-point-ids r)))
    (is (= :observed-uncovered (:unclassified-entry-point-status r)))
    (is (contains? (:open-obligations r)
                   :observed-entry-points-outside-inductive-contract))))

(deftest misleading-host-inventory-never-proves-real-writer-completeness
  (let [r (assert-no-physical-authority!
           (check! (observed {op (base-entry)})))]
    (is (= #{} (:observed-unclassified-entry-point-ids r)))
    (is (= :none-observed (:unclassified-entry-point-status r)))
    (is (contains? (:open-obligations r)
                   :host-authoritative-entry-point-inventory-is-complete))
    (is (contains? (:open-obligations r)
                   :non-route-xtdb-writers-are-covered))))

(deftest earlier-report-becomes-stale-when-the-host-snapshot-changes
  (let [live (atom {op (base-entry)})
        source (assoc-in *base*
                         [:authoritative-coverage-source :enumerate-entry-points!]
                         (fn [] @live))
        first-report (check! source)]
    (is (= :none-observed (:unclassified-entry-point-status first-report)))
    (swap! live assoc extra (entry :unclassified (fn [_] nil)))
    (let [now (assert-no-physical-authority! (check! source))]
      (is (= #{extra} (:observed-unclassified-entry-point-ids now)))
      (is (= :observed-uncovered (:unclassified-entry-point-status now)))
      (is (not= first-report now))
      (let [failure (failure #(app/require-current-application-induction!
                              first-report source))]
        (is (= app-error (:error/type failure)))
        (is (= :stale-or-forged-application-induction-report
               (:reason failure)))))))

(deftest mutated-copy-of-report-cannot-be-used-as-fresh-evidence
  (let [r (check! *base*)]
    (doseq [[label change]
            [[:fake-authority #(assoc % :authority :granted)]
             [:insert-provability #(assoc % :proved? true)]
             [:conceal-open-writer #(assoc % :physical-write-path-completeness :proved)]
             [:discharge-assumption #(update % :open-obligations disj
                                             :abstract-to-physical-transition-refinement)]
             [:forge-plan-identity #(assoc-in % [:checked-inductive-plans op
                                               :source-join] :different)]
             [:erase-certificate-fingerprint #(dissoc % :certificate-fingerprint)]
             [:remove-operation #(update % :issued-inductive-plan-ids disj op)]]]
      (testing (name label)
        (let [altered (change r)
              result (failure #(app/require-current-application-induction!
                                altered *base*))]
          (is (not= r altered))
          (is (= app-error (:error/type result)) (pr-str result))
          (is (= :stale-or-forged-application-induction-report
                 (:reason result)) (pr-str result)))))))

(deftest copyable-report-does-not-become-an-execution-token
  (let [r (assert-no-physical-authority! (check! *base*))]
    (is (= r (app/require-current-application-induction! (into {} r) *base*)))
    (is (false? (inductive-plan/inductive-plan-operation? r)))
    (is (false? (certified-plan/checked-plan-operation? r)))
    (is (not (contains? r :executor)))
    (is (not (contains? r :handler)))))

(deftest substituted-upstream-checkers-cannot-approve-a-mutant
  (let [fake (fn [_] {:type coverage/report-type
                      :checking :fresh-source-derived-contract-and-host-enumeration
                      :valid? true :authority :granted})]
    (with-redefs [coverage/check-authoritative-coverage! fake]
      (reject-app! *base* :substituted-upstream-checker))
    (with-redefs [inductive-plan/require-current-inductive-plan! fake]
      (reject-app! *base* :substituted-upstream-checker))))
