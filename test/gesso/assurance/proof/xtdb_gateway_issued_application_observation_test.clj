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

