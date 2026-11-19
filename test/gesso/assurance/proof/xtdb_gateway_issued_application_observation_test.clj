(ns gesso.assurance.proof.xtdb-gateway-issued-application-observation-test
  "v1867: Real-XTDB original-executor candidate collection, one-use
   observation semantics and explicit non-certification boundaries."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.admission :as admission]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.proof.certificate :as certificate]
            [gesso.assurance.proof.checked-authorization-assembly :as checked-assembly]
            [gesso.assurance.proof.xtdb-gateway-issued-application-observation
             :as application-observation]
            [gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test]
            [gesso.live.application-preflight.fixture :as app-fixture]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.model.certified-plan :as plan]
            [gesso.model.command-resolution :as resolution]))

(def ^:private parent-ns
  'gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test)
(def ^:private model-ns 'gesso.model.resolved-operation-test)
(defn- original [ns-name sym]
  (var-get (or (ns-resolve ns-name sym)
               (throw (ex-info "Missing original test fixture"
                               {:namespace ns-name :symbol sym})))))
(defn- data [] ((original parent-ns 'data)))
(defn- environment [d opts] ((original parent-ns 'environment) d opts))
(defn- original-issued-source [d e opts]
  ((original parent-ns 'original-issued-source) d e opts))
(defn- original-catalog [] ((original parent-ns 'original-catalog)))
(defn- physical-map [] ((original parent-ns 'physical-map)))
(defn- db [] ((original parent-ns 'db)))
(defn- seed! [& xs] (apply (original parent-ns 'seed!) xs))
(defn- read-request [id] ((original parent-ns 'read-request) id))
(defn- read-command [id] ((original parent-ns 'read-command) id))
(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))

(defn- parent-fixture [run]
  ((original model-ns 'with-real-xtdb)
   (fn []
     (app-fixture/with-closed-application
      {:operations {:request/claim
                    (app-fixture/trusted-operation
                     :request/claim {:published-change-topics #{}})}}
      (fn [{:keys [assembly]}]
        (with-bindings {(ns-resolve parent-ns '*assembly*) assembly}
          (run)))))))
(use-fixtures :once parent-fixture)

(defn- v1867-alias-case-env []
  ;; The original certified plan expressly permits this operation-ID alias.
  ;; Rebuild its current certificate and exact registered executor as in v1861.
  (let [d (data)
        e (environment d nil)
        base (original-issued-source d e nil)
        operation :request/claim
        construction (assoc-in (:construction base)
                               [:subject :interface :operation] operation)
        subject (:subject construction)
        transition (-> construction (dissoc :replacements :frame)
                       (assoc :assignments (:replacements construction)))
        proof {:transition transition
               :steps [{:id :step/guard :rule :guard :conclusion [:guard]}]
               :goal [:guard]}
        graph {:roots #{operation}
               :nodes [{:id operation :subject subject
                        :subject-handle (admission/admit-subject! subject)
                        :depends-on []}]}
        certificate (certificate/admit-certificate!
                     {:proof-kind :guarded-transition
                      :proof-source proof
                      :graph-handle (dependency/check-graph! graph)
                      :current-graph graph
                      :expected-open-premises
                      #{:gesso.assurance.proof.derivation/abstract-relation-match}
                      :expected-open-obligations
                      #{:production-transition-correspondence
                        :current-runtime-authorization
                        :atomic-transaction-enforcement
                        :checker-meta-soundness}})
        source (-> base
                   (assoc :construction construction)
                   (assoc-in [:certified-source :certificate] certificate)
                   (assoc-in [:certified-source :proof-source] proof)
                   (assoc-in [:certified-source :current-graph] graph))
        executor (plan/build-certified-plan-operation! source)
        handler (certified/issue-certified-handler!
                 {:executor executor :current-source source
                  :request->input! (fn [_] {:binding (:binding d) :mode :submit})
                  :result->response! (fn [_ result] {:status 200 :body result})})
        contract {:application-assembly (var-get (ns-resolve parent-ns '*assembly*))
                  :semantic-route-handlers {operation handler}
                  :certified-operations {operation {:executor executor
                                                   :current-source source}}}
        catalog-source {:catalog (assoc-in (original-catalog)
                                           [:operations 0 :construction] construction)
                        :physical-field-map (physical-map)}
        ]
    {:data d :environment e :contract contract :catalog-source catalog-source}))

(deftest v1867-collected-original-executor-candidate-with-physical-read
  ;; The collector receives an operation+canonical invocation, NEVER a
  ;; caller-submitted observation. This fixture runs its real original executor
  ;; and pinned read and aborts before the canonical Gesso writer.
  (let [{:keys [data environment] :as env} (v1867-alias-case-env)
        operation :request/claim
        observer (application-observation/issue-current-issued-application-observer!
                  (:contract env) (:catalog-source env))
        initial ((:summary! observer))]
    (is (= :wrong-keys
           (reason #(application-observation/issue-current-issued-application-observer!
                     (:contract env)
                     {:catalog {:operations (get-in env [:catalog-source :catalog :operations])}
                      :physical-field-map {}})))
        "Route-only input must not substitute for the complete typed catalog")
    (is (= application-observation/report-type (:type initial)))
    (is (= application-observation/report-version (:version initial)))
    (is (= #{operation} (:certified-operation-ids initial)))
    (is (= #{operation} (set (keys (:per-operation-observations initial)))))
    (is (= :not-observed (get-in initial [:per-operation-observations operation])))
    (is (= #{} (:observed-operation-ids initial)))
    (is (= 0 (:observation-count initial)))
    (is (false? (:all-certified-operations-individually-observed? initial)))
    (with-redefs [checked-assembly/check-declared-route-coverage!
                  (fn [& _] {:type checked-assembly/coverage-report-type})]
      (is (= :substituted-original-application-observation-checker
             (reason #((:summary! observer)))))
      (is (zero? @(:constructs environment))))
    (is (= :operation-not-originally-certified
           (reason #((:observe! observer) :request/cancel
                     {:biff.xtdb/node (db)}
                     {:binding (:binding data) :mode :submit}))))
    (is (zero? @(:constructs environment)))
    (seed! [:requests (:request data)])
    (let [result ((:observe! observer) operation
                  {:biff.xtdb/node (db)}
                  {:binding (:binding data) :mode :submit})
          selected (get-in result [:per-operation-observations operation])]
      (is (= #{operation} (:observed-operation-ids result)))
      (is (= 1 (:observation-count result)))
      (is (= :individually-pinned-prestate-matched (:physical-before selected)))
      (is (= :original-and-independent-source-relative
             (:typed-transition selected)))
      (is (= (get-in result [:certified-operation-proof-origins operation])
             (:proof-origin selected)))
      (is (true? (:all-certified-operations-individually-observed? result)))
      (is (= (:original-open-proof-evidence initial)
             (:original-open-proof-evidence result)))
      (is (= :not-established (:joint-snapshot-basis result)
             (:physical-application-correspondence result)
             (:deployed-handler-adoption result)
             (:original-kernel-proof-discharge result)
             (:physical-refinement-proof-discharge result)))
      (is (= :none (:transaction-authority result)
             (:retry-authority result) (:authority result)))
      (is (= :not-issued (:certificate result)))
      (is (= :not-admitted (:admission result)))
      (is (not-any? #(contains? result %)
                    [:candidate :plan :binding :ctx :node :snapshot-token
                     :executor :observe-once! :submit! :current-document]))
      (is (= 1 @(:constructs environment)))
      (is (= (:request data) (read-request (:xt/id (:request data)))))
      (is (nil? (read-command
                 (resolution/record-id (:binding data)))))
      (is (= :issued-application-observation-already-attempted
             (reason #((:observe! observer) operation
                       {:biff.xtdb/node (db)}
                       {:binding (:binding data) :mode :submit}))))
      (is (= result ((:summary! observer)))))
    ;; Failed observation attempts are consumed and NEVER counted as evidence.
    (let [failed (application-observation/issue-current-issued-application-observer!
                  (:contract env) (:catalog-source env))]
      (is (= :invalid-original-executor-alias-invocation
             (reason #((:observe! failed) operation {:biff.xtdb/node (db)}
                       {:binding (:binding data) :mode :reconcile}))))
      (let [summary ((:summary! failed))]
        (is (= #{} (:observed-operation-ids summary)))
        (is (= #{operation} (:attempted-without-qualified-evidence summary)))
        (is (= :not-observed
               (get-in summary [:per-operation-observations operation])))
        (is (= :issued-application-observation-already-attempted
               (reason #((:observe! failed) operation {:biff.xtdb/node (db)}
                         {:binding (:binding data) :mode :submit}))))))))


(defn- second-original-issued-alias [operation d]
  ;; An independently issued and admitted source, not a copied executor or
  ;; a caller-generated success report. Both operations use the same closed
  ;; Claim-shaped abstract state model but address distinct physical rows.
  (let [environment (environment d nil)
        base (original-issued-source d environment nil)
        construction (-> (:construction base)
                         (assoc-in [:subject :id] operation)
                         (assoc-in [:subject :interface :operation] operation))
        subject (:subject construction)
        transition (-> construction (dissoc :replacements :frame)
                       (assoc :assignments (:replacements construction)))
        proof {:transition transition
               :steps [{:id :step/guard :rule :guard :conclusion [:guard]}]
               :goal [:guard]}
        graph {:roots #{operation}
               :nodes [{:id operation :subject subject
                        :subject-handle (admission/admit-subject! subject)
                        :depends-on []}]}
        certificate (certificate/admit-certificate!
                     {:proof-kind :guarded-transition
                      :proof-source proof
                      :graph-handle (dependency/check-graph! graph)
                      :current-graph graph
                      :expected-open-premises
                      #{:gesso.assurance.proof.derivation/abstract-relation-match}
                      :expected-open-obligations
                      #{:production-transition-correspondence
                        :current-runtime-authorization
                        :atomic-transaction-enforcement
                        :checker-meta-soundness}})
        source (-> base
                   (assoc :construction construction)
                   (assoc-in [:certified-source :operation] operation)
                   (assoc-in [:certified-source :resolved-configuration :operation]
                             operation)
                   (assoc-in [:certified-source :certificate] certificate)
                   (assoc-in [:certified-source :proof-source] proof)
                   (assoc-in [:certified-source :current-graph] graph))
        executor (plan/build-certified-plan-operation! source)
        binding (assoc (:binding d) :operation operation)
        handler (certified/issue-certified-handler!
                 {:executor executor :current-source source
                  :request->input! (fn [_] {:binding binding :mode :submit})
                  :result->response! (fn [_ result] {:status 200 :body result})})]
    {:source source :executor executor :handler handler
     :data (assoc d :binding binding) :environment environment}))

(deftest v1868-two-independent-executor-observations-never-become-certification
  (let [primary :request/claim
        secondary :request/claim-secondary
        audit :request/audit
        primary-route :route/claim-primary
        secondary-route :route/claim-secondary
        audit-route :route/audit
        routes {primary-route {:operation primary :path "/request/claim-primary"}
                secondary-route {:operation secondary :path "/request/claim-secondary"}
                audit-route {:operation audit :path "/request/audit"}}
        operations (into {}
                         (for [op [primary secondary audit]]
                           [op (app-fixture/trusted-operation
                                op {:published-change-topics #{}})]))]
    (app-fixture/with-surfaced-application
      {:operations operations
       :route-capabilities (app-fixture/custom-route-capabilities routes)
       :rendered-surfaces {:proof [:div "safe"]}}
      (fn [{:keys [assembly report]}]
        (is (application/valid? report)
            (pr-str (:errors report)))
        (when assembly
          (let [first-source (v1867-alias-case-env)
                first-row (:data first-source)
                second-source (second-original-issued-alias secondary (data))
                second-row (:data second-source)
                contract {:application-assembly assembly
                          :semantic-route-handlers
                          {primary-route
                           (get-in first-source [:contract :semantic-route-handlers
                                                 primary])
                           secondary-route (:handler second-source)
                           audit-route (fn [_] {:status 202 :body "uncertified"})}
                          :certified-operations
                          {primary (get-in first-source
                                           [:contract :certified-operations primary])
                           secondary {:executor (:executor second-source)
                                      :current-source (:source second-source)}}}
                catalog-source
                {:catalog (-> (original-catalog)
                              (assoc :declared-operation-ids #{primary secondary}
                                     :operations
                                     [{:id primary
                                       :construction (get-in first-source
                                                             [:contract :certified-operations
                                                              primary :current-source :construction])}
                                      {:id secondary
                                       :construction (get-in second-source
                                                             [:source :construction])}]))
                 :physical-field-map (physical-map)}
                observer (application-observation/issue-current-issued-application-observer!
                          contract catalog-source)
                summary! (:summary! observer)
                observe! (:observe! observer)
                before (summary!)
                ctx {:biff.xtdb/node (db)}
                invoke-primary {:binding (:binding first-row) :mode :submit}
                invoke-secondary {:binding (:binding second-row) :mode :submit}]
            (is (not= (get-in first-row [:request :xt/id])
                      (get-in second-row [:request :xt/id])))
            (is (not= (:executor (get-in first-source
                                          [:contract :certified-operations primary]))
                      (:executor second-source)))
            (is (= #{primary secondary} (:certified-operation-ids before)))
            (is (= #{audit} (:uncertified-operation-ids before)))
            (is (= #{audit-route} (:uncertified-route-ids before)))
            (is (= {:request/claim :not-observed
                    :request/claim-secondary :not-observed}
                   (:per-operation-observations before)))
            (is (= :operation-not-originally-certified
                   (reason #(observe! audit ctx invoke-primary))))
            (seed! [:requests (:request first-row)]
                   [:requests (:request second-row)])
            (let [first-result (observe! primary ctx invoke-primary)]
              (is (= #{primary} (:observed-operation-ids first-result)))
              (is (= #{secondary}
                     (set (for [[operation status]
                                (:per-operation-observations first-result)
                                :when (= status :not-observed)]
                            operation))))
              (is (= :individually-pinned-prestate-matched
                     (get-in first-result
                             [:per-operation-observations primary :physical-before])))
              (is (false? (:all-certified-operations-individually-observed?
                           first-result)))
              (is (= :not-established (:joint-snapshot-basis first-result)))
              (is (= :issued-application-observation-already-attempted
                     (reason #(observe! primary ctx invoke-primary)))))
            ;; Between independent pinned observations, a new external XTDB
            ;; write invalidates the FIRST observation's current-row claim.
            ;; This intentionally rules out interpreting the final ledger as
            ;; one atomic shared snapshot, even though both entries succeed.
            (seed! [:requests (assoc (:request first-row) :owner "later-writer")])
            (is (= "later-writer"
                   (:owner (read-request (get-in first-row [:request :xt/id])))))
            (let [after (observe! secondary ctx invoke-secondary)]
              (is (= #{primary secondary} (:observed-operation-ids after)))
              (is (= 2 (:observation-count after)))
              (is (true? (:all-certified-operations-individually-observed? after)))
              (is (= #{} (:attempted-without-qualified-evidence after)))
              (is (= :individually-pinned-prestate-matched
                     (get-in after
                             [:per-operation-observations secondary :physical-before])))
              (is (= (get-in before [:certified-operation-proof-origins primary])
                     (get-in after [:per-operation-observations primary :proof-origin])))
              (is (= (get-in before [:certified-operation-proof-origins secondary])
                     (get-in after [:per-operation-observations secondary :proof-origin])))
              (is (= (:original-open-proof-evidence before)
                     (:original-open-proof-evidence after)))
              (is (seq (get-in after [:original-open-proof-evidence :open-obligations])))
              (is (= #{audit} (:uncertified-operation-ids after)))
              (is (= #{audit-route} (:uncertified-route-ids after)))
              (is (not (contains? (:per-operation-observations after) audit)))
              (is (= :not-established
                     (:joint-snapshot-basis after)
                     (:physical-application-correspondence after)
                     (:deployed-handler-adoption after)
                     (:original-kernel-proof-discharge after)
                     (:physical-refinement-proof-discharge after)))
              (is (= :not-issued (:certificate after)))
              (is (= :not-admitted (:admission after)))
              (is (= :none (:authority after)
                     (:retry-authority after) (:transaction-authority after)))
              (is (= 1 @(:constructs (:environment first-source))))
              (is (= 1 @(:constructs (:environment second-source))))
              (is (= "later-writer"
                     (:owner (read-request (get-in first-row [:request :xt/id])))))
              (is (= (:request second-row)
                     (read-request (get-in second-row [:request :xt/id]))))
              (is (nil? (read-command
                         (resolution/record-id (:binding first-row)))))
              (is (nil? (read-command
                         (resolution/record-id (:binding second-row)))))
              (is (= :issued-application-observation-already-attempted
                     (reason #(observe! secondary ctx invoke-secondary))))
              (is (= after (summary!))))))))))
