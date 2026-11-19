(ns gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test
  "v1776: Bind an actual issued certified plan to a checked assembled route
   and original catalog, then submit through THAT executor to real XTDB."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.admission :as admission]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.proof.certificate :as certificate]
            [gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial :as bound]
            [gesso.assurance.proof.xtdb-gateway-typed-transition-seam :as typed-seam]
            [gesso.assurance.proof.checked-authorization-assembly :as checked-assembly]
            [gesso.assurance.proof.operation-set :as operation-set]
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

(deftest v1854-selected-operation-identity-persists-through-physical-ack
  ;; The original binding rechecks all three fingerprints already. Preserve
  ;; their exact origin in public execution evidence without issuing a proof
  ;; certificate or pretending the observed physical receipt discharges it.
  (let [{:keys [data] :as env} (case-env)
        operation :request/claim
        catalog-report (operation-set/check-operation-set!
                        (get-in env [:catalog-source :catalog]))
        contract-report (certified/check-certified-contracts! (:contract env))
        op-row (get-in contract-report [:certified-operations operation])
        expected {:catalog-fingerprint (:fingerprint catalog-report)
                  :transition-fingerprint (:transition-fingerprint op-row)
                  :proof-fingerprint (:proof-fingerprint op-row)
                  :route-ids (:route-ids op-row)}
        trial (issued env)
        before ((:summary! trial))]
    (is (every? some? (vals expected)))
    (is (= expected (:selected-operation-source-identity before)))
    (is (= :original-source-rechecked
           (:selected-operation-identity-scope before)))
    (seed! [:requests (:request data)])
    (let [observed (invoke! trial env :submit)
          after ((:summary! trial))]
      (is (= :bounded-issued-executor-catalog-atomic-acknowledgment
             (:status observed)))
      (is (= expected (:selected-operation-source-identity observed)
             (:selected-operation-source-identity after)))
      (is (= :original-source-rechecked
             (:selected-operation-identity-scope observed)
             (:selected-operation-identity-scope after)))
      (is (= :not-issued (:certificate observed)))
      (is (= :none (:authority observed)
             (:transaction-authority observed))))))

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

(deftest selected-physical-trial-allows-explicitly-uncertified-application-route
  ;; The v1253 contract allows a certified Claim operation alongside an
  ;; unrelated uncertified Cancel route. The original selected catalog must
  ;; remain complete for Claim, while the full source route table contains
  ;; BOTH and the unselected route never acquires certified authority.
  (app-fixture/with-closed-application
   {:operations {:request/claim
                 (app-fixture/trusted-operation
                  :request/claim {:published-change-topics #{}})
                 :request/cancel
                 (app-fixture/trusted-operation
                  :request/cancel {:published-change-topics #{}})}}
   (fn [{:keys [assembly]}]
     (let [summary (application/operation-summary
                    (:operation-acquisition-assembly assembly))
           extra-routes (into #{}
                              (map :route-id)
                              (get-in summary [:request/cancel :routes]))
           extra-handler (fn [_] {:status 202 :body "uncertified"})
           env (binding [*assembly* assembly] (case-env))
           full (update-in env [:contract :semantic-route-handlers]
                           merge (zipmap extra-routes (repeat extra-handler)))
           contract-report (certified/check-certified-contracts!
                            (:contract full))
           selection-report
           (checked-assembly/check-declared-route-coverage!
            {:contract-source (:contract full)
             :catalog-source (:catalog-source full)})
           expected-route-scope
           (select-keys selection-report
                        [:catalog-operations :matched-certified-route-ids
                         :uncertified-application-operations
                         :uncertified-application-route-ids])]
       (is (seq extra-routes))
       (is (= #{:request/cancel} (:uncertified-operations contract-report)))
       (is (= extra-routes (:uncertified-application-route-ids
                            selection-report)))
       (is (= #{:request/claim} (:catalog-operations selection-report)))
       (is (= #{:request/claim}
              (set (keys (:certified-operation-open-evidence selection-report)))))
       ;; Even a route which is explicitly UNCERTIFIED is part of the closed
       ;; current application route table. Its disappearance cannot be hidden
       ;; by the correct selected Claim source and executor.
       (is (= :invalid-current-certified-operation-table
              (reason #(issued
                        (update-in full [:contract :semantic-route-handlers]
                                   dissoc (first extra-routes))))))
       (is (= :invalid-current-certified-operation-table
              (reason #(issued
                        (assoc-in full [:contract :semantic-route-handlers
                                        :route/invented]
                                  (fn [_] {:status 200}))))))
       (let [trial (issued full)
             initial ((:summary! trial))]
         (is (= expected-route-scope (:catalog-route-scope initial)))
         (is (= :original-source-rechecked
                (:catalog-route-scope-evidence initial)))
         (seed! [:requests (get-in full [:data :request])])
         (let [result (invoke! trial full :submit)
               final ((:summary! trial))]
           (is (= :bounded-issued-executor-catalog-atomic-acknowledgment
                  (:status result)))
           (is (= expected-route-scope (:catalog-route-scope result)
                  (:catalog-route-scope final)))
           (is (= #{:request/cancel}
                  (get-in result [:catalog-route-scope
                                  :uncertified-application-operations])))
           (is (= extra-routes
                  (get-in result [:catalog-route-scope
                                  :uncertified-application-route-ids])))
           (is (= :original-source-rechecked
                  (:catalog-route-scope-evidence result)
                  (:catalog-route-scope-evidence final)))
           (is (= :none (:transaction-authority result)))
           (is (= :not-issued (:certificate result)))
           (is (= :not-established (:deployed-biff-handler-adoption result)))))))))

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

(deftest v1861-pinned-executor-before-is-an-independent-physical-read
  ;; The original constructor returns a self-consistent candidate even when
  ;; the target is absent or its physical prestate has changed. A real pinned
  ;; same-node XTDB read must reject those cases, without Gesso submission.
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
        contract {:application-assembly *assembly*
                  :semantic-route-handlers {operation handler}
                  :certified-operations {operation {:executor executor
                                                   :current-source source}}}
        catalog-source {:catalog (assoc-in (original-catalog)
                                           [:operations 0 :construction] construction)
                        :physical-field-map (physical-map)}
        ctx {:biff.xtdb/node (db)}
        invocation {:binding (:binding d) :mode :submit}
        premises (test-helper typed-ns 'premises)
        check! (fn [context mode]
                 (bound/check-current-issued-executor-typed-command-alias!
                  contract catalog-source operation context invocation premises mode))]
    (is (= :missing-issued-executor-external-before-node
           (reason #(check! {} :pinned-external-before))))
    (is (zero? @(:constructs e))
        "Missing node must fail BEFORE application callbacks")
    (is (= :unsupported-issued-executor-before-observation-mode
           (reason #(check! ctx :invented-mode))))
    (is (zero? @(:constructs e)))
    (is (= :issued-executor-external-before-missing-or-ambiguous
           (reason #(check! ctx :pinned-external-before)))
        "An internally checked candidate is NOT proof the physical row exists")
    ;; The proof inventory is derived from the original closed catalog's
    ;; COMPLETE original declaration, not invented by the diagnostic.
    (let [derived (typed-seam/check-source-bound-typed-read-premises!
                   (:catalog catalog-source) operation)]
      (is (= typed-seam/source-bound-read-premises-type (:type derived)))
      (is (= typed-seam/source-bound-read-premises-version (:version derived)))
      (is (= operation (:operation-id derived)
             (get-in (:declaration derived) [:subject :id])))
      (is (= (:invariants (:catalog catalog-source))
             (get-in derived [:proof-context :invariants])))
      (is (seq (get-in derived [:proof-context :read-premises])))
      (is (every? #(= operation (:operation-id %))
                  (get-in derived [:proof-context :read-premises])))
      (is (= :none (:authority derived)))
      (is (= :not-issued (:certificate derived)))
      (is (= :not-admitted (:admission derived)))
      (is (not-any? #(contains? derived %)
                    [:prepare! :submit! :executor :node])))
    (is (= :issued-executor-external-before-missing-or-ambiguous
           (reason #(bound/check-current-issued-executor-typed-command-alias!
                     contract catalog-source operation ctx invocation nil
                     :pinned-source-derived-premises)))
        "A derived read-invariant inventory is not a substitute for XTDB")
    (seed! [:requests (:request d)])
    ;; v1862: Only the original declared operation/canonical catalog can
    ;; select the invariant/read-premise set; no caller map is accepted.
    (let [before @(:constructs e)]
      (is (= :caller-supplied-executor-premises-not-accepted
             (reason #(bound/check-current-issued-executor-typed-command-alias!
                       contract catalog-source operation ctx invocation
                       premises :pinned-source-derived-premises))))
      (is (= before @(:constructs e))
          "Rejected caller premises must not invoke the executor"))
    (let [source-bound
          (bound/check-current-issued-executor-typed-command-alias!
           contract catalog-source operation ctx invocation nil
           :pinned-source-derived-premises)]
      (is (= bound/issued-executor-candidate-alias-report-type
             (:type source-bound)))
      (is (= :original-checked-catalog-derived
             (:typed-read-premise-provenance source-bound)))
      (is (= :original-issued-operation-id
             (:typed-read-premise-subject source-bound)))
      (is (= :pinned-same-node-document-match
             (:external-before source-bound)))
      (is (= :original-and-independent-source-relative
             (:typed-abstract-alias-correspondence source-bound)))
      (let [original (checked-assembly/check-declared-route-coverage!
                      {:contract-source contract
                       :catalog-source catalog-source})
            expected (select-keys original
                                  [:open-assumptions :open-premises
                                   :open-obligations
                                   :certified-operation-open-evidence])
            selected (get-in expected
                             [:certified-operation-open-evidence operation])
            alias (bound/check-current-issued-command-alias!
                   contract catalog-source operation)]
        (is (seq (:open-obligations expected))
            "A matching XTDB row must not discharge source obligations")
        (is (= expected (:catalog-open-evidence source-bound)))
        (is (= selected (:selected-operation-open-evidence source-bound)))
        (is (= (get-in alias [:source-identity])
               (:selected-operation-source-identity source-bound)))
        (is (= :original-source-rechecked
               (:catalog-proof-evidence-scope source-bound)))
        (is (= :retained-open-not-discharged
               (:proof-obligations-disposition source-bound)))
        (is (= :not-established
               (:original-kernel-proof-discharge source-bound)
               (:physical-refinement-proof-discharge source-bound)))
        (is (= (set (keys (:certified-operation-open-evidence expected)))
               (get-in original [:catalog-operations]))
            "Do not collapse full catalog proof evidence to selected row"))
      (is (= :disabled (:canonical-physical-alias-execution source-bound)))
      (is (= :none (:authority source-bound)
             (:transaction-authority source-bound)))
      (is (not-any? #(contains? source-bound %)
                    [:proof-context :invariants :read-premises
                     :snapshot-token :candidate :plan :current-document
                     :node :target :sql :executor :submit! :prepare!])))
    (let [before @(:constructs e)]
      (with-redefs [typed-seam/check-source-bound-typed-read-premises!
                    (fn [& _] {:type typed-seam/source-bound-read-premises-type})]
        (is (= :substituted-original-assembly-or-issuer
               (reason #(bound/check-current-issued-executor-typed-command-alias!
                         contract catalog-source operation ctx invocation nil
                         :pinned-source-derived-premises)))))
      (is (= before @(:constructs e))
          "Substituted premise derivation must be rejected before callbacks"))
    (let [result (check! ctx :pinned-external-before)]
      (is (= bound/issued-executor-candidate-alias-report-type (:type result)))
      (is (= :pinned-same-node-document-match (:external-before result)))
      (is (= :caller-context-node-unattested
             (:external-basis-authenticity result)))
      (is (= :retained-open-not-discharged
             (:proof-obligations-disposition result)))
      (is (= :not-established
             (:original-kernel-proof-discharge result)
             (:physical-refinement-proof-discharge result)))
      (is (= :not-established (:transaction-time-prestate result)
             (:prestate-aba-exclusion result)))
      (is (= :blocked-before-canonical-writer (:physical-submission result)))
      (is (= :none (:authority result) (:transaction-authority result)))
      (is (not-any? #(contains? result %)
                    [:snapshot-token :candidate :plan :current-document
                     :node :target :sql :executor :submit! :prepare!])))
    (seed! [:requests (assoc (:request d) :owner "other-writer")])
    (is (= :issued-executor-external-before-mismatch
           (reason #(bound/check-current-issued-executor-typed-command-alias!
                     contract catalog-source operation ctx invocation nil
                     :pinned-source-derived-premises)))
        "Derived premises cannot overcome a conflicting physical prestate")
    (is (= :issued-executor-external-before-mismatch
           (reason #(check! ctx :pinned-external-before)))
        "A changed authoritative document must defeat the captured prestate")
    (is (nil? (read-command (resolution/record-id (:binding d)))))
    (is (= "other-writer" (:owner (read-request (:xt/id (:request d))))))))

(deftest v1857-source-bound-alias-check-requires-a-genuine-alias
  (let [env (case-env)]
    ;; A normal command/interface agreement is not an ID alias. The new
    ;; read-only entry point must not silently label ordinary issuance as one.
    (is (= :original-issued-command-alias-not-established
           (reason #(bound/check-current-issued-command-alias!
                     (:contract env) (:catalog-source env) :request/claim))))
    (is (= :invalid-current-assembled-catalog-source
           (reason #(bound/check-current-issued-command-alias!
                     (:contract env) (:catalog-source env) :claim))))))

(deftest original-plan-operation-id-alias-is-not-physical-typed-correspondence
  ;; The original certified plan legitimately accepts an interface operation
  ;; specified by the certified operation ID, not only by the domain command.
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
        contract {:application-assembly *assembly*
                  :semantic-route-handlers {operation handler}
                  :certified-operations {operation {:executor executor
                                                   :current-source source}}}
        catalog-source {:catalog (assoc-in (original-catalog)
                                           [:operations 0 :construction] construction)
                        :physical-field-map (physical-map)}
        env {:data d :environment e :source source :executor executor
             :handler handler :contract contract :catalog-source catalog-source}]
    (is (= :claim (:command-operation (plan/describe-plan-operation executor))))
    (is (= operation (get-in construction [:subject :interface :operation])))
    ;; Unlike v1856's caller-declared alias, this report is derived from the
    ;; ORIGINAL currently issued plan and complete assembled catalog, before
    ;; any executor invocation. It is still only copyable source evidence.
    (let [alias (bound/check-current-issued-command-alias!
                 contract catalog-source operation)]
      (is (= bound/issued-alias-report-type (:type alias)))
      (is (= bound/issued-alias-report-version (:version alias)))
      (is (= operation (:operation-id alias)))
      (is (= {:operation-id operation
              :interface-operation operation
              :domain-command-operation :claim}
             (:command-binding alias)))
      (is (= :current-original-issued-plan-and-catalog
             (:command-binding-provenance alias)))
      (is (= :original-checked-application-assembly
             (:complete-catalog-scope alias)))
      (is (every? some? (vals (:source-identity alias))))
      (is (= :not-established (:typed-abstract-alias-correspondence alias)))
      (is (= :disabled (:canonical-physical-alias-execution alias)))
      (is (= :none (:transaction-authority alias) (:authority alias)))
      (is (= :not-issued (:certificate alias)))
      (is (= :not-admitted (:admission alias)))
      (is (not-any? #(contains? alias %)
                    [:executor :submit! :observe-once! :prepare!])))
    ;; The new v1858 check derives the declaration, field projection and
    ;; alias from the ORIGINAL issued source, not a caller-supplied binding.
    (let [candidate {:plan ((test-helper model-ns 'domain-plan) (:request d))}
          inputs {:input/owner "helper-a"}
          proof (test-helper typed-ns 'premises)
          check! (fn [domain ctx]
                   (bound/check-current-issued-typed-command-alias!
                    contract catalog-source operation domain inputs ctx))
          checked (check! candidate proof)]
      (is (= bound/issued-typed-alias-report-type (:type checked)))
      (is (= bound/issued-typed-alias-report-version (:version checked)))
      (is (= operation (:operation-id checked)))
      (is (= {:operation-id operation
              :interface-operation operation
              :domain-command-operation :claim}
             (:command-binding checked)))
      (is (= :original-and-independent-source-relative
             (:typed-abstract-alias-correspondence checked)))
      (is (= :original-issued-plan-construction
             (:typed-declaration-source checked)))
      (is (= :original-issued-plan-field-map
             (:typed-field-map-source checked)))
      (is (= :entire-original-checked-catalog
             (:typed-invariant-inventory checked)))
      (is (= :source-relative-values-screened
             (:typed-read-premises checked)))
      (is (= :caller-supplied-not-authenticated
             (:candidate-domain-source checked)))
      (is (= :disabled (:physical-submission checked)
             (:typed-screening-physical-submission checked)
             (:canonical-physical-alias-execution checked)))
      (is (= :none (:transaction-authority checked) (:authority checked)))
      (is (= :not-issued (:certificate checked)))
      (is (= :not-admitted (:admission checked)))
      (is (not-any? #(contains? checked %)
                    [:executor :submit! :observe-once! :prepare!
                     :plan :domain-source :declaration :field-map]))
      (is (= :issued-typed-read-invariants-not-original
             (reason #(check! candidate
                              (assoc-in proof [:invariants :doc/owner-not-blank
                                               :expression]
                                        [:not= [:read :doc/owner]
                                         [:literal "different-policy"]])))))
      (is (= :invalid-source-relative-command-binding
             (reason #(check!
                       (assoc-in candidate [:plan :commands 0 :model/operation]
                                 :cancel)
                       proof))))
      (is (= :original-and-independent-abstract-relation-not-established
             (reason #(check!
                       (assoc-in candidate [:plan :commands 0 :model/after :owner]
                                 "unrelated-owner")
                       proof))))
      (with-redefs [typed-seam/check-source-relative-typed-domain-with-command-binding!
                    (fn [& _] {:type typed-seam/source-check-type})]
        (is (= :substituted-original-typed-alias-screen
               (reason #(check! candidate proof)))))
      (is (zero? @(:constructs e))
          "Source-relative checks must not execute the issued plan"))
    ;; v1859 does not accept a caller-supplied candidate. Instead, obtain
    ;; it from the identical constructor registered in the issued plan and
    ;; screen it before any Gesso physical submit. Application callbacks
    ;; are trusted but NOT proven free from side effects.
    (let [proof (test-helper typed-ns 'premises)
          invocation {:binding (:binding d) :mode :submit}
          ctx {:biff.xtdb/node (db)}
          check! (fn [inv premises]
                   (bound/check-current-issued-constructor-typed-command-alias!
                    contract catalog-source operation ctx inv premises))
          before @(:constructs e)]
      (is (= :invalid-registered-constructor-alias-invocation
             (reason #(check! (assoc invocation :mode :reconcile) proof))))
      (is (= :invalid-registered-constructor-alias-invocation
             (reason #(check! (assoc-in invocation
                                          [:binding :principal] :forged)
                              proof))))
      (is (= :issued-typed-read-invariants-not-original
             (reason #(check! invocation (assoc proof :invariants {})))))
      (is (= before @(:constructs e))
          "Invalid calls must fail before executing application callbacks")
      ;; A substituted constructor in a copy of the certified operation table
      ;; is not the registered source, even when the operation ID is correct.
      (is (= :invalid-current-certified-operation-table
             (reason #(bound/check-current-issued-constructor-typed-command-alias!
                       (assoc-in contract
                                 [:certified-operations operation :current-source
                                  :certified-source :resolved-configuration :construct!]
                                 (fn [& _] (throw (ex-info "substituted" {}))))
                       catalog-source operation ctx invocation proof))))
      (is (= before @(:constructs e)))
      (let [checked (check! invocation proof)]
        (is (= bound/issued-constructed-alias-report-type (:type checked)))
        (is (= bound/issued-constructed-alias-report-version (:version checked)))
        (is (= operation (:operation-id checked)))
        (is (= {:operation-id operation
                :interface-operation operation
                :domain-command-operation :claim}
               (:command-binding checked)))
        (is (= :original-and-independent-source-relative
               (:typed-abstract-alias-correspondence checked)))
        (is (= :same-registered-source-constructor-evaluated
               (:candidate-domain-source checked)))
        (is (= :original-registered-input-acquisition-evaluated
               (:candidate-input-source checked)))
        (is (= :original-certified-plan-pre-submit-checked
               (:candidate-prestate-check checked)))
        (is (= :not-invoked (:registered-executor-invocation checked)))
        (is (= :not-established (:callback-purity checked)))
        (is (= :not-excluded (:callback-physical-side-effects checked)))
        (is (= :not-invoked-by-this-check (:physical-submission checked)))
        (is (= :disabled (:typed-screening-physical-submission checked)))
        (is (= :none (:transaction-authority checked) (:authority checked)))
        (is (= :not-issued (:certificate checked)))
        (is (= :not-admitted (:admission checked)))
        (is (not-any? #(contains? checked %)
                      [:executor :submit! :observe-once! :prepare!
                       :plan :domain-source :declaration :field-map
                       :inputs :current-document :target])))
      (is (= (inc before) @(:constructs e))
          "Exact registered constructor must run once for the candidate")
      (is (nil? (read-command
                 (resolution/record-id (:binding d))))
          "Known-safe original constructor fixture must not write XTDB"))
    ;; v1860 observes the exact executor's original guarded candidate gate.
    ;; It installs an independent canonical-writer backstop, so the known-safe
    ;; fixture's second constructor call cannot submit a command or domain tx.
    (let [ctx {:biff.xtdb/node (db)}
          invocation {:binding (:binding d) :mode :submit}
          proof (test-helper typed-ns 'premises)
          check! (fn [inv premises]
                   (bound/check-current-issued-executor-typed-command-alias!
                    contract catalog-source operation ctx inv premises))
          before @(:constructs e)
          original-request (read-request (:xt/id (:request d)))]
      (is (= :invalid-original-executor-alias-invocation
             (reason #(check! (assoc invocation :mode :reconcile) proof))))
      (is (= :issued-typed-read-invariants-not-original
             (reason #(check! invocation (assoc proof :invariants {})))))
      (is (= before @(:constructs e)))
      (let [checked (check! invocation proof)]
        (is (= bound/issued-executor-candidate-alias-report-type
               (:type checked)))
        (is (= bound/issued-executor-candidate-alias-report-version
               (:version checked)))
        (is (= operation (:operation-id checked)))
        (is (= {:operation-id operation
                :interface-operation operation
                :domain-command-operation :claim}
               (:command-binding checked)))
        (is (= :original-executor-guarded-construct
               (:candidate-domain-source checked)))
        (is (= :original-executor-acquisition
               (:candidate-input-source checked)))
        (is (= :original-certified-plan-pre-submit-checked
               (:candidate-prestate-check checked)))
        (is (= :original-and-independent-source-relative
               (:typed-abstract-alias-correspondence checked)))
        (is (= :aborted-after-checked-candidate
               (:registered-executor-invocation checked)))
        (is (= :blocked-before-canonical-writer (:physical-submission checked)))
        (is (= :disabled (:canonical-physical-alias-execution checked)))
        (is (= :not-established (:callback-purity checked)))
        (is (= :not-excluded (:callback-physical-side-effects checked)))
        (is (= :none (:transaction-authority checked) (:authority checked)))
        (is (= :not-issued (:certificate checked)))
        (is (= :not-admitted (:admission checked)))
        (is (not-any? #(contains? checked %)
                      [:executor :submit! :observe-once! :prepare!
                       :plan :candidate :inputs :declaration :field-map])))
      (is (= (inc before) @(:constructs e)))
      (is (= original-request (read-request (:xt/id (:request d)))))
      (is (nil? (read-command (resolution/record-id (:binding d))))))
    ;; A changed copy of the source cannot impersonate the registered plan.
    (is (= :invalid-current-certified-operation-table
           (reason #(bound/check-current-issued-command-alias!
                     (assoc-in contract
                               [:certified-operations operation
                                :current-source :command-operation]
                               :cancel)
                     catalog-source operation))))
    ;; Only the explicitly opted-in registered constructor evaluation above
    ;; may run the callback; the physical alias trial must remain rejected.
    (is (= 2 @(:constructs e)))
    (is (= #{operation} (set (keys (:certified-operations
                                   (certified/check-certified-contracts! contract))))))
    (seed! [:requests (:request d)])
    ;; The original certified plan is valid, but the LOWER typed physical
    ;; relation insists the actual command operation equal the interface op.
    ;; Until an original source-bound alias relation reaches that interpreter,
    ;; reject the physical trial before one-use issuance or any model write.
    (is (= :typed-physical-command-interface-alias-not-established
           (reason #(issued env))))
    (is (= (:request d) (read-request (:xt/id (:request d)))))
    (is (nil? (read-command
               (resolution/record-id (:binding d)))))))
