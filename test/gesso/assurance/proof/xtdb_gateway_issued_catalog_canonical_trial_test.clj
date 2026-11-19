(ns gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test
  "v1776: Bind an actual issued certified plan to a checked assembled route
   and original catalog, then submit through THAT executor to real XTDB."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.admission :as admission]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.proof.certificate :as certificate]
            [gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial :as bound]
            [gesso.assurance.proof.checked-authorization-assembly :as checked-assembly]
            [gesso.assurance.proof.xtdb-gateway-catalog-bound-atomic-canonical-trial-test]
            [gesso.assurance.proof.xtdb-gateway-full-atomic-typed-preparation-test]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.fixture :as app-fixture]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.model.certified-plan :as plan]
            [gesso.model.resolved-operation-test]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]))

(def ^:private catalog-ns
  'gesso.assurance.proof.xtdb-gateway-catalog-bound-atomic-canonical-trial-test)
(def ^:private typed-ns
  'gesso.assurance.proof.xtdb-gateway-full-atomic-typed-preparation-test)
(def ^:private model-ns 'gesso.model.resolved-operation-test)
(defn- test-helper [ns-name sym]
  (var-get (or (ns-resolve ns-name sym)
               (throw (ex-info "Missing test helper" {:namespace ns-name :symbol sym})))))
(defn- original-catalog [] ((test-helper catalog-ns 'original-catalog)))
(defn- physical-map [] (test-helper typed-ns 'mapping))
(defn- data [] ((test-helper model-ns 'fixture-data)))
(defn- environment [d opts] ((test-helper model-ns 'real-environment) d opts))
(defn- db [] ((test-helper model-ns 'database)))
(defn- seed! [& xs] (apply (test-helper model-ns 'seed!) xs))
(defn- read-request [id] ((test-helper model-ns 'read-request) id))
(defn- read-command [id] ((test-helper model-ns 'read-command) id))
(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo ex
         (:reason (ex-data ex)))))

(def ^:dynamic *assembly* nil)
(use-fixtures :once
  (fn [run]
    ((test-helper model-ns 'with-real-xtdb)
     (fn []
       (app-fixture/with-closed-application
        {:operations {:request/claim
                      (app-fixture/trusted-operation
                       :request/claim {:published-change-topics #{}})}}
        (fn [{:keys [assembly]}]
          (binding [*assembly* assembly] (run))))))))

(def ^:private certified-proof
  (delay
    (let [construction (get-in (original-catalog) [:operations 0 :construction])
          operation :request/claim
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
          cert (certificate/admit-certificate!
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
                   :checker-meta-soundness}})]
      {:proof proof :graph graph :certificate cert})))

(defn- original-issued-source [data environment opts]
  (let [{:keys [proof graph certificate]} @certified-proof
        request (:request data)
        base {:operation :request/claim
              :read-record! (fn [_ row-id] (read-command row-id))
              :may-disclose? (fn [_ _ _] true)
              :construct! (fn [_ _]
                            (swap! (:constructs environment) inc)
                            {:now "t1"
                             :plan ((test-helper model-ns 'domain-plan)
                                    request (when (:guard-membership? opts)
                                              (:membership data)))
                             :outcome {:status :claimed :owner "helper-a"}})}]
    {:certified-source {:operation :request/claim
                        :certificate certificate
                        :proof-source proof
                        :current-graph graph
                        :resolved-configuration base}
     :construction (get-in (original-catalog) [:operations 0 :construction])
     :field-map (physical-map)
     :command-operation :claim
     :current-document! (fn [_ _] request)
     :transition-inputs! (fn [_ _] {:input/owner "helper-a"})
     :target! (fn [_ _] [:requests (:xt/id request)])}))

(defn- case-env
  ([] (case-env (data) nil))
  ([d opts]
   (let [e (environment d opts)
         source (original-issued-source d e opts)
         executor (plan/build-certified-plan-operation! source)
         handler (certified/issue-certified-handler!
                  {:executor executor :current-source source
                   :request->input! (fn [_] {:binding (:binding d) :mode :submit})
                   :result->response! (fn [_ result] {:status 200 :body result})})
         contract {:application-assembly *assembly*
                   :semantic-route-handlers {:request/claim handler}
                   :certified-operations
                   {:request/claim {:executor executor :current-source source}}}]
     {:data d :environment e :source source :executor executor
      :handler handler :contract contract
      :catalog-source {:catalog (original-catalog)
                       :physical-field-map (physical-map)}})))

(defn- issued [env]
  (bound/issue-issued-catalog-canonical-trial!
   (:contract env) (:catalog-source env)
   :request/claim {:input/owner "helper-a"}))
(defn- invoke! [issued env mode]
  ((:observe-once! issued)
   {:biff.xtdb/node (db)}
   {:binding (get-in env [:data :binding]) :mode mode}))

(deftest issued-executor-assembly-and-catalog-produce-real-atomic-xtdb-write
  (let [{:keys [data environment] :as env} (case-env)
        {:keys [request binding]} data
        trial (issued env)]
    (seed! [:requests request])
    (is (= :not-attempted (:status ((:summary! trial)))))
    (let [r (invoke! trial env :submit)]
      (is (= bound/report-type (:type r)))
      (is (= :bounded-issued-executor-catalog-atomic-acknowledgment
             (:status r) (:status ((:summary! trial)))))
      (is (= :exact-closed-operation-coverage (:catalog-to-checked-assembly r)))
      (is (= :exact-original-construction-and-field-map
             (:catalog-to-issued-executor r)))
      (is (= :current-issued-handlers-in-assembly (:certified-routes r)))
      (is (= :one-original-plan-executor-returned
             (:issued-executor-return r)))
      (is (= :one-source-relative-model-transaction-acknowledged
             (:canonical-physical-execution r)))
      (is (= :not-established (:deployed-biff-handler-adoption r)))
      (is (= :not-established (:original-business-policy-authenticity r)))
      (is (= :not-established (:typed-complete-cas-writer-adoption r)))
      (is (= :not-issued (:certificate r)))
      (is (= :none (:authority r) (:retry-authority r)))
      (is (= ((test-helper model-ns 'next-version) request {:owner "helper-a"})
             (read-request (:xt/id request))))
      (is (= (resolution/command-record binding {:status :claimed :owner "helper-a"} "t1")
             (read-command (resolution/record-id binding))))
      (is (= 1 @(:constructs environment)))
      (is (not-any? #(contains? r %)
                    [:contract :catalog :source :executor :handler
                     :transaction :plan :tx-ops :execution :request :node]))
      (is (= :issued-executor-trial-already-attempted
             (reason #(invoke! trial env :submit)))))))

(deftest issued-physical-trial-requires-original-full-catalog-closure
  ;; The physical binder cannot attest whole-catalog construction/handler
  ;; correspondence by inspecting only its selected operation. In particular,
  ;; substituting the catalog-wide checked closure must be rejected before
  ;; a one-use physical trial is issued, even if the selected source is real.
  (let [env (case-env)]
    (with-redefs [checked-assembly/check-declared-route-coverage!
                  (constantly {:type checked-assembly/coverage-report-type
                               :certificate :not-issued :authority :none})]
      (is (= :substituted-original-assembly-or-issuer
             (reason #(issued env)))))))

(deftest issued-physical-report-retains-original-open-catalog-obligations
  ;; The source-relative full catalog check has unresolved proof premises.
  ;; Even after a bounded physical XTDB acknowledgment the issued trial must
  ;; not erase, synthesize or claim to discharge those outstanding premises.
  (let [{:keys [data] :as env} (case-env)
        catalog-report
        (checked-assembly/check-declared-route-coverage!
         {:contract-source (:contract env)
          :catalog-source (:catalog-source env)})
        expected (select-keys catalog-report
                              [:open-assumptions :open-premises :open-obligations
                               :certified-operation-open-evidence])
        trial (issued env)
        initial ((:summary! trial))]
    (is (seq (:open-obligations catalog-report)))
    (is (seq (:certified-operation-open-evidence catalog-report)))
    (is (= expected (:catalog-open-evidence initial)))
    (is (= :original-source-rechecked
           (:catalog-proof-evidence-scope initial)))
    (seed! [:requests (:request data)])
    (let [observed (invoke! trial env :submit)
          summary ((:summary! trial))]
      (is (= :bounded-issued-executor-catalog-atomic-acknowledgment
             (:status observed)))
      (is (= expected (:catalog-open-evidence observed)
             (:catalog-open-evidence summary)))
      (is (= :original-source-rechecked
             (:catalog-proof-evidence-scope observed)
             (:catalog-proof-evidence-scope summary)))
      (is (= :not-issued (:certificate observed)))
      (is (= :none (:transaction-authority observed))))))

(deftest unrelated-checked-catalog-cannot-impersonate-issued-executor
  (let [env (case-env)
        changed (assoc-in env [:catalog-source :catalog :operations 0
                               :construction :guard :expression]
                          [:= [:read :doc/owner] [:literal "someone-else"]])]
    (is (= :catalog-executor-or-assembled-operation-diverged
           (reason #(issued changed))))))

(deftest different-physical-projection-is-rejected-before-submission
  (let [env (case-env)
        changed (assoc-in env [:catalog-source :physical-field-map
                               :doc/created] :other-created)]
    (is (= :catalog-executor-or-assembled-operation-diverged
           (reason #(issued changed))))))

(deftest substituted-unissued-router-handler-is-not-a-valid-contract
  (let [env (case-env)
        changed (assoc-in env [:contract :semantic-route-handlers :request/claim]
                          (fn [_] {:status 200}))]
    (is (= :invalid-current-certified-operation-table
           (reason #(issued changed))))))

(deftest catalog-source-cannot-claim-extra-unassembled-operation
  (let [env (case-env)
        changed (-> env
                    (assoc-in [:catalog-source :catalog :declared-operation-ids]
                              #{:request/claim :request/cancel}))]
    (is (#{:invalid-original-catalog
           :catalog-executor-or-assembled-operation-diverged}
         (reason #(issued changed))))))

(deftest wrong-operation-invocation-consumes-trial-before-a-write
  (let [{:keys [data] :as env} (case-env)
        issued (issued env)
        request (get data :request)]
    (seed! [:requests request])
    (is (= :invalid-bound-executor-invocation
           (reason #((:observe-once! issued)
                     {:biff.xtdb/node (db)}
                     {:binding (assoc (:binding data) :operation :request/cancel)
                      :mode :submit}))))
    (is (= request (read-request (:xt/id request))))
    (is (= :issued-executor-trial-already-attempted
           (reason #(invoke! issued env :submit))))))

(deftest stale-dependency-guard-keeps-domain-and-ledger-unchanged
  (let [d (data)
        {:keys [request membership binding]} d
        env (case-env d {:guard-membership? true})
        issued (issued env)]
    (seed! [:requests request] [:memberships membership])
    (xt/execute-tx (db)
                   (:tx-ops (tx/prepare {}
                                        {:commands [((test-helper model-ns 'revoke-command)
                                                     membership)]
                                         :emit false})))
    (let [r (invoke! issued env :submit)]
      (is (= :issued-executor-canonical-submission-rejected (:status r)))
      (is (= request (read-request (:xt/id request))))
      (is (nil? (read-command (resolution/record-id binding))))
      (is (= :none (:retry-authority r))))))

(deftest lost-acknowledgment-remains-uncertain-without-retry-permission
  (let [{:keys [data] :as env} (case-env)
        {:keys [request binding]} data
        issued (issued env)
        open-evidence (:catalog-open-evidence ((:summary! issued)))
        original tx/transact!
        calls (atom 0)]
    (seed! [:requests request])
    (let [r (with-redefs [tx/transact!
                         (fn [ctx plan]
                           (swap! calls inc)
                           (original ctx plan)
                           (throw (ex-info "ack lost" {})))]
              (invoke! issued env :submit))]
      (is (= :not-qualified-or-uncertain (:status r)))
      (is (= open-evidence (:catalog-open-evidence r)))
      (is (= :original-source-rechecked (:catalog-proof-evidence-scope r)))
      (is (= 1 @calls))
      (is (some? (read-command (resolution/record-id binding))))
      (is (= :none (:retry-authority r))))))

(deftest replace-catalog-checker-consumes-permit-without-a-write
  (let [{:keys [data] :as env} (case-env)
        request (:request data)
        issued (issued env)]
    (seed! [:requests request])
    (with-redefs [gesso.assurance.proof.operation-set/check-operation-set!
                  (fn [& _] {:fingerprint :forged})]
      (is (= :substituted-original-assembly-or-issuer
             (reason #(invoke! issued env :submit)))))
    (is (= request (read-request (:xt/id request))))
    (is (= :issued-executor-trial-already-attempted
           (reason #(invoke! issued env :submit))))))

(deftest immutable-xt-id-is-a-framed-declared-field-not-an-extra-write
  (let [d (data)
        request (:request d)
        construction (get-in (original-catalog) [:operations 0 :construction])
        field-map (physical-map)
        input {:construction construction
               :field-map field-map
               :command-operation :claim
               :target [:requests (:xt/id request)]
               :current-document request
               :inputs {:input/owner "helper-a"}
               :plan {:commands [((test-helper model-ns 'claim-command) request)]
                      :emit false}}
        checked (plan/check-candidate! input)]
    (is (= :pre-submit-candidate-checked (:checking checked)))
    (is (= :request/claim (:operation checked)))
    (is (= :claim (:command-operation checked)))
    (is (= :mutable-or-optional-identity-projection
           (reason #(plan/check-candidate!
                     (-> input
                         (update-in [:construction :frame] disj :doc/id)
                         (update-in [:construction :writes] conj :doc/id)
                         (assoc-in [:construction :replacements :doc/id]
                                   [:literal "other-id"]))))))
    (is (= :mutable-or-optional-identity-projection
           (reason #(plan/check-candidate!
                     (assoc-in input [:construction :state-types :doc/id :optional?]
                               true)))))
    (is (= :non-injective-or-invalid-field-map
           (reason #(plan/check-candidate!
                     (assoc-in input [:field-map :doc/created] :xt/id)))))
    (is (= :document-target-mismatch
           (reason #(plan/check-candidate!
                     (assoc input :target [:requests "wrong-target"])))))))
