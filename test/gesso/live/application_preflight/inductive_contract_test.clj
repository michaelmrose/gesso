(ns gesso.live.application-preflight.inductive-contract-test
  "v1289: adversarial permanent tests for the v1288 source-bound application
   contract. The real ApplicationAssembly, issued Ring handler, certified-plan
   executor, inductive certificate and exact dependency handle are used; no
   production checker is stubbed in a successful path. Any temporary Var
   substitution is deliberately adversarial and must fail closed.

   The result authenticates a supplied CURRENT route snapshot, not router
   adoption, physical write-path completeness, transaction refinement or
   checker soundness. The source-derived report is not an execution token."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.inductive-report :as inductive]
            [gesso.assurance.proof.certificate :as certificate]
            [gesso.assurance.proof.induction :as induction]
            [gesso.assurance.proof.induction-dependency :as closure]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.live.application-preflight.inductive-contract :as contract]
            [gesso.model.certified-plan :as plan]
            [gesso.model.certified-plan-test]))

(def ^:private operation :example/claim)
(def ^:private other-operation :example/cancel)
(def ^:private state-type
  {:type :enum :name :example/status :values #{:open :claimed}})
(def ^:private state-invariant
  {:readers #{:example/status}
   :expression [:in [:read :example/status]
                [:literal #{:open :claimed}]]})
(def ^:private catalog
  {:id :example/claims
   :declared-operation-ids #{operation}
   :initial-condition state-invariant
   :invariants {:example/valid-status state-invariant}
   :operations
   [{:id operation
     :construction
     {:subject {:kind :operation :id operation
                :interface {:operation operation}}
      :state-types {:example/status state-type}
      :input-types {}
      :guard {:readers #{:example/status}
              :expression [:= [:read :example/status] [:literal :open]]}
      :reads #{:example/status}
      :writes #{:example/status}
      :replacements {:example/status [:literal :claimed]}
      :frame #{}}}]})

(def ^:private contract-error
  :gesso.live.application-preflight.inductive-contract/invalid-inductive-contract)
(def ^:private certified-error
  :gesso.live.application-preflight.certified-contract/invalid-certified-contract)
(def ^:dynamic *base* nil)
(def ^:dynamic *fixture-state* nil)

(defn- proof-package []
  (let [graph (closure/build-induction-dependency-graph! catalog)
        checked (induction/check-induction! catalog)
        dependency-handle (closure/check-induction-dependencies! catalog graph)
        cert (certificate/admit-certificate!
              {:proof-kind :conditional-induction
               :proof-source catalog
               :graph-handle (dependency/check-graph! graph)
               :current-graph graph
               :expected-open-premises
               #{:gesso.assurance.proof.induction/matching-declared-abstract-machine}
               :expected-open-obligations
               (set/union
                #{:production-transition-correspondence
                  :current-runtime-authorization
                  :atomic-transaction-enforcement
                  :checker-meta-soundness}
                (disj (:open-meta-obligations checked)
                      :certificate-admission))})]
    {:catalog catalog
     :certificate-handle cert
     :dependency-handle dependency-handle
     :current-graph graph}))

(defn- certified-environment []
  ;; Reuse the original *test-only* v1252 source fixture. Every executor,
  ;; handler and certificate is still checked by the actual production APIs.
  (let [fixture-fn (some-> (ns-resolve 'gesso.model.certified-plan-test
                                    'environment)
                           deref)
        _ (when-not fixture-fn
            (throw (ex-info "Missing v1252 certified-plan fixture" {})))
        source (:source (fixture-fn))
        executor (plan/build-certified-plan-operation! source)
        handler (certified/issue-certified-handler!
                 {:executor executor
                  :current-source source
                  :request->input! (fn [_] {:mode :reconcile :binding {}})
                  :result->response! (fn [_ r] {:status 200 :body r})})]
    {:executor executor :source source :handler handler}))

(use-fixtures
  :once
  (fn [run]
    (let [proof (proof-package)
          {:keys [source executor handler] :as issued} (certified-environment)]
      (fixture/with-closed-application
        {:operations {operation
                      (fixture/trusted-operation
                       operation {:published-change-topics #{}})}}
        (fn [{:keys [assembly] :as app}]
          (binding [*base* {:inductive-source proof
                            :application-assembly assembly
                            :semantic-route-handlers {operation handler}
                            :certified-operations
                            {operation {:executor executor
                                        :current-source source}}}
                    *fixture-state* {:proof proof :issued issued :app app}]
            (run)))))))

(defn- failure-data [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- reject! [kind reason thunk]
  (let [data (failure-data thunk)]
    (is (map? data) (str "Expected typed failure " reason))
    (is (= kind (:error/type data)) (pr-str data))
    (is (= reason (:reason data)) (pr-str data))
    data))

(defn- reject-contract! [reason thunk]
  (reject! contract-error reason thunk))

(defn- check [] (contract/check-inductive-contract! *base*))

(defn- report-is-limited! [r]
  (is (= contract/report-type (:type r)))
  (is (= contract/contract-version (:version r)))
  (is (= :current-abstract-source-and-issued-route-snapshot-rechecked
         (:checking r)))
  (is (= #{operation} (:abstract-operation-ids r)))
  (is (= (:abstract-operation-ids r) (:assembled-operation-ids r)
         (:certified-operation-ids r)))
  (is (= #{operation} (:semantic-route-ids r)))
  (is (= :exact-operation-identity-and-issued-handler-to-plan (:binding r)))
  (is (= {operation #{operation}} (:operation-route-ids r)))
  (is (= :current-supplied-application-route-snapshot-only (:scope r)))
  (is (= :all-original-obligations-remain-open
         (:original-obligation-status r)))
  (is (= #{operation}
         (set (keys (:original-open-obligation-ids-by-operation r)))))
  (is (seq (get (:original-open-obligation-ids-by-operation r) operation)))
  (doseq [field [:abstract-to-physical-transition :physical-writer-completeness
                 :deployed-router-adoption :initialization-correspondence
                 :proof-kernel-independent-soundness :production-certification]]
    (is (= :not-established (get r field)) (str field)))
  (doseq [obligation [:abstract-to-authoritative-transition-correspondence
                      :abstract-initial-state-to-physical-initializer-correspondence
                      :physical-writer-and-entry-point-completeness
                      :actual-router-adopts-supplied-handler-snapshot
                      :post-check-router-and-handler-stability
                      :induction-checker-independent-soundness]]
    (is (contains? (:open-obligations r) obligation) (str obligation)))
  (is (seq (:open-premises r)))
  (is (= :none (:transaction-authority r)))
  (is (= :none (:authority r)))
  (is (not (contains? r :valid?)))
  r)

(deftest valid-real-assembly-stays-conditional
  (report-is-limited! (check)))

(deftest current-copyable-report-must-be-rechecked
  (let [r (check)]
    (is (= r (contract/require-current-inductive-contract! r *base*)))
    (is (= r (contract/require-current-inductive-contract!
              (into {} r) (into {} *base*))))))

(deftest top-level-source-map-must-be-exact
  (doseq [source [(dissoc *base* :inductive-source)
                  (assoc *base* :unrecognized true)
                  (with-meta *base* {:claimed-proof true})
                  (sorted-map) [] nil]]
    (let [data (failure-data #(contract/check-inductive-contract! source))]
      (is (= contract-error (:error/type data)) (pr-str data))
      (is (contains? #{:wrong-source-keys
                       :expected-unannotated-source-map} (:reason data))))))

(deftest inductive-source-map-must-be-exact
  (doseq [source [(dissoc (:inductive-source *base*) :catalog)
                  (assoc (:inductive-source *base*) :proof-accepted? true)
                  (with-meta (:inductive-source *base*) {:certified true})
                  [] nil]]
    (let [data (failure-data
                #(contract/check-inductive-contract!
                  (assoc *base* :inductive-source source)))]
      (is (= contract-error (:error/type data)) (pr-str data))
      (is (contains? #{:wrong-source-keys
                       :expected-unannotated-source-map} (:reason data))))))

(deftest source-only-induction-is-not-an-application-contract
  (doseq [field [:certificate-handle :dependency-handle :current-graph]]
    (reject-contract!
     :source-bound-induction-required
     #(contract/check-inductive-contract!
       (assoc-in *base* [:inductive-source field] nil)))))

(deftest copy-of-an-inductive-description-is-not-a-live-handle
  (let [proof (:inductive-source *base*)
        description (inductive/build-inductive-report! proof)]
    (is (= :source-bound-conditional-abstract-safety (:verdict description)))
    (doseq [field [:certificate-handle :dependency-handle]]
      (let [data (failure-data
                  #(contract/check-inductive-contract!
                    (assoc-in *base* [:inductive-source field] description)))]
        (is (map? data) (pr-str field))
        (is (not= :source-bound-conditional-abstract-safety (:verdict data)))))))

(deftest altered-abstract-operation-id-set-is-rejected
  (reject! :gesso.assurance.proof.operation-set/invalid-operation-set
           :operation-id-coverage-mismatch
           #(contract/check-inductive-contract!
             (assoc-in *base* [:inductive-source :catalog
                               :declared-operation-ids]
                       #{other-operation}))))

(deftest incomplete-abstract-catalog-cannot-be-promoted
  (reject! :gesso.assurance.proof.operation-set/invalid-operation-set
           :invalid-operation-sequence
           #(contract/check-inductive-contract!
             (update-in *base* [:inductive-source :catalog :operations]
                        empty))))

(deftest changed-operation-body-invalidates-live-certificate
  (let [changed (assoc-in *base*
                          [:inductive-source :catalog :operations 0
                           :construction :guard :expression]
                          [:= [:read :example/status] [:literal :claimed]])
        data (failure-data #(contract/check-inductive-contract! changed))]
    (is (map? data))
    (is (not= :source-bound-conditional-abstract-safety (:verdict data)))))

(deftest changed-initial-condition-invalidates-live-certificate
  (let [changed (assoc-in *base*
                          [:inductive-source :catalog :initial-condition]
                          {:readers #{} :expression [:literal true]})]
    (is (map? (failure-data
               #(contract/check-inductive-contract! changed))))))

(deftest changed-invariant-set-invalidates-live-certificate
  (let [changed (assoc-in *base*
                          [:inductive-source :catalog :invariants
                           :example/new-invariant]
                          {:readers #{} :expression [:literal true]})]
    (is (map? (failure-data
               #(contract/check-inductive-contract! changed))))))

(deftest altered-dependency-graph-invalidates-evidence
  (let [changed (assoc-in *base*
                          [:inductive-source :current-graph :roots]
                          #{:example/forged-root})]
    (is (map? (failure-data
               #(contract/check-inductive-contract! changed))))))

(deftest forged-catalog-report-is-not-a-proof-source
  (let [fake (assoc (check) :catalog-id :example/claims)]
    (is (map? (failure-data
               #(contract/check-inductive-contract!
                 (assoc-in *base* [:inductive-source :catalog] fake)))))))

(deftest complete-proof-does-not-cover-an-extra-assembled-operation
  (let [foreign (:assembly (:app *fixture-state*))
        extra (fixture/trusted-operation
               other-operation {:published-change-topics #{}})]
    (is (some? foreign))
    ;; Real second assembly, not a fabricated operation summary.
    (fixture/with-closed-application
      {:operations {operation (fixture/trusted-operation
                              operation {:published-change-topics #{}})
                    other-operation extra}}
      (fn [{:keys [assembly]}]
        (let [data (reject-contract!
                    :abstract-assembly-operation-set-mismatch
                    #(contract/check-inductive-contract!
                      (assoc *base* :application-assembly assembly)))]
          (is (= #{} (:missing-from-assembly data)))
          (is (= #{other-operation} (:outside-abstract-catalog data))))))))

(deftest missing-certified-operation-is-rejected
  (let [data (reject-contract!
              :abstract-certified-operation-set-mismatch
              #(contract/check-inductive-contract!
                (assoc *base* :certified-operations {})))]
    (is (= #{operation} (:uncertified data)))
    (is (= #{} (:outside-abstract-catalog data)))))

(deftest extra-certified-operation-is-rejected
  (let [data (reject-contract!
              :abstract-certified-operation-set-mismatch
              #(contract/check-inductive-contract!
                (assoc-in *base* [:certified-operations other-operation]
                          (get-in *base* [:certified-operations operation]))))]
    (is (= #{other-operation} (:outside-abstract-catalog data)))
    (is (= #{} (:uncertified data)))))

(deftest swapped-certified-operation-key-is-rejected
  (let [entry (get-in *base* [:certified-operations operation])
        data (reject-contract!
              :abstract-certified-operation-set-mismatch
              #(contract/check-inductive-contract!
                (assoc *base* :certified-operations
                       {other-operation entry})))]
    (is (= #{operation} (:uncertified data)))
    (is (= #{other-operation} (:outside-abstract-catalog data)))))

(deftest certified-operation-table-must-be-plain
  (doseq [value [nil [] (sorted-map)
                 (with-meta (:certified-operations *base*) {:allowed true})]]
    (reject-contract!
     :invalid-certified-operation-table
     #(contract/check-inductive-contract!
       (assoc *base* :certified-operations value)))))

(deftest omitted-route-is-rejected-by-original-route-checker
  (reject! certified-error :semantic-route-table-mismatch
           #(contract/check-inductive-contract!
             (assoc *base* :semantic-route-handlers {}))))

(deftest extra-route-is-rejected-by-original-route-checker
  (reject! certified-error :semantic-route-table-mismatch
           #(contract/check-inductive-contract!
             (assoc-in *base* [:semantic-route-handlers :route/undocumented]
                       (get-in *base* [:semantic-route-handlers operation])))))

(deftest raw-handler-cannot-impersonate-an-issued-handler
  (reject! certified-error :unissued-route-handler
           #(contract/check-inductive-contract!
             (assoc-in *base* [:semantic-route-handlers operation]
                       (fn [_] {:status 200})))))

(deftest copied-handler-metadata-does-not-grant-issuance
  (let [original (get-in *base* [:semantic-route-handlers operation])
        fake (with-meta (fn [_] {:status 200}) (meta original))]
    (is (not (certified/certified-handler? fake)))
    (reject! certified-error :unissued-route-handler
             #(contract/check-inductive-contract!
               (assoc-in *base* [:semantic-route-handlers operation] fake)))))

(deftest second-issued-handler-with-different-executor-is-rejected
  (let [{:keys [source]} (:issued *fixture-state*)
        second-executor (plan/build-certified-plan-operation! source)
        other-handler (certified/issue-certified-handler!
                       {:executor second-executor
                        :current-source source
                        :request->input! (fn [_] {:mode :reconcile :binding {}})
                        :result->response! (fn [_ result] {:status 200 :body result})})]
    (is (certified/certified-handler? other-handler))
    (reject! certified-error :swapped-route-executor
             #(contract/check-inductive-contract!
               (assoc-in *base* [:semantic-route-handlers operation]
                         other-handler)))))

(deftest raw-executor-cannot-appear-in-certified-table
  (reject! certified-error :unregistered-certified-plan-executor
           #(contract/check-inductive-contract!
             (assoc-in *base* [:certified-operations operation :executor]
                       (fn [& _] nil)))))

(deftest issued-handler-with-swapped-executor-is-rejected
  (let [{:keys [source]} (:issued *fixture-state*)
        second-executor (plan/build-certified-plan-operation! source)]
    (reject! certified-error :swapped-route-executor
             #(contract/check-inductive-contract!
               (assoc-in *base* [:certified-operations operation :executor]
                         second-executor)))))

(deftest stale-plan-source-invalidates-handler-binding
  (reject! :gesso.model.certified-plan/invalid-certified-plan
           :stale-plan-source
           #(contract/check-inductive-contract!
             (assoc-in *base* [:certified-operations operation :current-source]
                       (assoc (get-in *base* [:certified-operations
                                             operation :current-source])
                              :test/modified true)))))

(deftest no-application-assembly-cannot-be-certified
  (reject-contract! :stale-or-invalid-application-assembly
                    #(contract/check-inductive-contract!
                      (assoc *base* :application-assembly {}))))

(deftest forged-inductive-checker-report-is-rejected
  (let [fabricated {:type inductive/report-type
                    :version inductive/report-version
                    :verdict :source-bound-conditional-abstract-safety
                    :authority :none}]
    (reject-contract!
     :substituted-sibling-checker-report
     #(with-redefs [inductive/build-inductive-report!
                    (fn [_] fabricated)]
        (check)))))

(deftest forged-certified-route-checker-report-is-rejected
  (let [fabricated {:type certified/report-type
                    :version certified/adapter-version
                    :certified-operations {operation {:route-ids #{operation}}}
                    :matched-certified-route-ids #{operation}
                    :authority :none}]
    (reject-contract!
     :substituted-sibling-checker-report
     #(with-redefs [certified/check-certified-contracts!
                    (fn [_] fabricated)]
        (check)))))

(deftest forged-report-cannot-pass-current-report-check
  (let [r (check)]
    (doseq [tampered [(assoc r :authority :production)
                      (assoc r :transaction-authority :permitted)
                      (assoc r :production-certification :established)
                      (assoc r :abstract-to-physical-transition :proved)
                      (assoc r :physical-writer-completeness :proved)
                      (assoc r :original-open-obligation-ids-by-operation {})
                      (update r :open-obligations disj
                              :physical-writer-and-entry-point-completeness)
                      (assoc r :claim :valid)
                      (with-meta r {:blessed true})]]
      (reject-contract!
       :stale-or-forged-inductive-contract
       #(contract/require-current-inductive-contract! tampered *base*)))))

(deftest forged-past-report-does-not-revalidate-against-changed-source
  (let [r (check)
        changed (assoc-in *base*
                          [:inductive-source :catalog :invariants
                           :example/another]
                          {:readers #{} :expression [:literal true]})]
    (is (map? (failure-data
               #(contract/require-current-inductive-contract! r changed))))))

(deftest abstract-certification-never-implies-production-authority
  (let [report (report-is-limited! (check))]
    (is (not (contains? report :runtime-authorization)))
    (is (set/subset?
         #{:physical-writer-and-entry-point-completeness
           :abstract-to-authoritative-transition-correspondence}
         (:open-obligations report)))
    (is (not= :established (:production-certification report)))))
