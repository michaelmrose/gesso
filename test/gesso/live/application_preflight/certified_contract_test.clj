(ns gesso.live.application-preflight.certified-contract-test
  "v1838: adversarial source-relative checks for issued Ring bindings.

   The ordinary operation-acquisition ApplicationAssembly is the authority for
   semantic routes; the checked handler registry is an exact, in-process link
   to v1251 issued executors. Reports, copied metadata, and caller-supplied
   route maps cannot certify deployment or grant transaction authority.
   Catalog coverage must invoke the original per-route checker, not a
   substituted public Var that simply reports success. It must also reject
   substituted application-recognition and operation-summary entry points.

   These tests distinguish real ApplicationAssembly closure (no redefs) from
   a deliberately stubbed operation summary used solely to isolate malformed
   route/binding inputs. We never replace the certified-plan proof checks."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.checked-authorization-assembly :as authorization]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.certified-contract :as binding]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.model.certified-plan :as plan]
            [gesso.model.certified-operation :as certified-operation]
            [gesso.model.certified-plan-test]))

(def ^:private error-type
  :gesso.live.application-preflight.certified-contract/invalid-certified-contract)
(def ^:private plan-error-type :gesso.model.certified-plan/invalid-certified-plan)
(def ^:private claim :example/claim)
(def ^:private cancel :example/cancel)
(def ^:private route-1 :route/claim)
(def ^:private route-2 :route/claim-alternate)
(def ^:private route-3 :route/cancel)

(def ^:private summary
  {claim {:routes [{:route-id route-1 :method :post :path "/claim"}
                   {:route-id route-2 :method :post :path "/claim/alternate"}]}
   cancel {:routes [{:route-id route-3 :method :post :path "/cancel"}]}})

;; Reuse only the existing test-only proof fixture, not any private production
;; API. The original certificate is real and freshly checked by production
;; v1251/v1253 machinery on each registration and assembly check.
(defn- previous-test-helper [symbol]
  (or (some-> (ns-resolve 'gesso.model.certified-plan-test symbol) deref)
      (throw (ex-info "Missing v1252 test fixture" {:symbol symbol}))))

(defn- plan-source []
  (:source ((previous-test-helper 'environment))))

(defn- reconcile-input []
  ((previous-test-helper 'invocation) :reconcile))

(defn- environment []
  (let [source (plan-source)
        executor (plan/build-certified-plan-operation! source)
        calls (atom [])
        request->input!
        (fn [ctx]
          (swap! calls conj [:request ctx])
          (reconcile-input))
        result->response!
        (fn [ctx result]
          (swap! calls conj [:response ctx result])
          {:status 200 :body result})
        issue-source {:executor executor :current-source source
                      :request->input! request->input!
                      :result->response! result->response!}
        handler (binding/issue-certified-handler! issue-source)
        unrelated (fn [_] {:status 204})
        base {:application-assembly {:operation-acquisition-assembly :placeholder}
              :semantic-route-handlers
              {route-1 handler route-2 handler route-3 unrelated}
              :certified-operations
              {claim {:executor executor :current-source source}}}]
    {:source source :executor executor :handler handler :unrelated unrelated
     :issue-source issue-source :base base :calls calls}))

(defn- with-summary [operation-summary f]
  (with-redefs [application/application-assembly? (constantly true)
                application/operation-summary (fn [_] operation-summary)]
    (f)))

(defn- check [base]
  (binding/check-certified-contracts! base))

(defn- failure [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- reject! [type reason f]
  (let [data (failure f)]
    (is (map? data) (str "Expected typed rejection " reason))
    (is (= type (:error/type data)) (pr-str data))
    (is (= reason (:reason data)) (pr-str data))
    (when (= type error-type)
      (is (vector? (:at data)) (pr-str data)))
    data))

(defn- reject-binding! [reason f]
  (reject! error-type reason f))

(defn- reject-plan! [reason f]
  (reject! plan-error-type reason f))

(defn- report-remains-conditional! [report]
  (is (= binding/report-type (:type report)))
  (is (= binding/adapter-version (:version report)))
  (is (= :current-source-relative-semantic-route-bindings
         (:checking report)))
  (is (= :not-established (:deployment-adoption report)))
  (is (= :not-established (:physical-operation-correspondence report)))
  (is (= :not-proved (:invariant-preservation report)))
  (is (= :none (:transaction-authority report)))
  (is (= :none (:certificate report)))
  (is (contains? (:open-assumptions report)
                 :actual-deployment-uses-this-checked-route-table))
  (is (contains? (:open-obligations report)
                 :no-later-router-or-handler-replacement))
  (is (contains? (:open-obligations report)
                 :atomic-xtdb-state-guard-and-commit-enforcement))
  (is (contains? (:open-obligations report)
                 :application-invariant-preservation))
  (is (not (contains? report :valid?)))
  report)

(deftest issued-handler-actually-delegates-through-certified-executor
  (let [{:keys [handler executor source calls]} (environment)
        ctx {:principal :example/helper :request/id 12}
        response (handler ctx)]
    (is (binding/certified-handler? handler))
    (is (plan/checked-plan-operation? executor))
    (plan/require-current-plan-operation! executor source)
    (is (= 200 (:status response)))
    (is (map? (:body response)))
    (is (= 2 (count @calls)))
    (is (= [:request ctx] (first @calls)))
    (is (= :response (first (second @calls))))
    (is (= ctx (second (second @calls))))
    (is (= (:body response) (nth (second @calls) 2)))))

(deftest one-operation-may-have-two-declared-issued-routes
  (let [{:keys [base handler]} (environment)]
    (with-summary summary
      (fn []
        (let [report (report-remains-conditional! (check base))
              row (get-in report [:certified-operations claim])]
          (is (= #{route-1 route-2 route-3} (:route-ids report)))
          (is (= #{route-1 route-2} (:matched-certified-route-ids report)))
          (is (= #{cancel} (:uncertified-operations report)))
          (is (= #{route-1 route-2} (:route-ids row)))
          (is (= :issued-handler-to-current-plan-executor (:binding row)))
          (is (string? (:proof-fingerprint row)))
          (is (string? (:transition-fingerprint row)))
          (is (= handler (get-in base [:semantic-route-handlers route-2]))))))))

(deftest uncertified-operation-may-use-ordinary-handler
  (let [{:keys [base]} (environment)]
    (with-summary summary
      (fn []
        (is (= #{cancel} (:uncertified-operations (check base))))))))

(deftest genuine-application-assembly-is-rechecked-without-stubbing
  (let [{:keys [executor source handler]} (environment)
        operations {claim (fixture/trusted-operation claim
                                                     {:published-change-topics #{}})}]
    (fixture/with-closed-application
      {:operations operations}
      (fn [{:keys [assembly]}]
        (let [operation-summary
              (application/operation-summary
               (:operation-acquisition-assembly assembly))
              routes (get-in operation-summary [claim :routes])]
          (is (application/application-assembly? assembly))
          (is (= #{claim} (set (keys operation-summary))))
          (is (= #{claim} (set (map :route-id routes))))
          (let [report (report-remains-conditional!
                        (check {:application-assembly assembly
                                :semantic-route-handlers {claim handler}
                                :certified-operations
                                {claim {:executor executor :current-source source}}}))]
            (is (= #{claim} (:matched-certified-route-ids report)))
            (is (= #{} (:uncertified-operations report)))))))))

(deftest missing-route-id-is-rejected-with-exact-missing-set
  (let [{:keys [base]} (environment)]
    (with-summary summary
      (fn []
        (let [data (reject-binding!
                    :semantic-route-table-mismatch
                    #(check (update base :semantic-route-handlers dissoc route-2)))]
          (is (= #{route-2} (:missing data)))
          (is (= #{} (:unexpected data))))))))

(deftest extra-route-id-is-rejected-with-exact-unexpected-set
  (let [{:keys [base]} (environment)]
    (with-summary summary
      (fn []
        (let [data (reject-binding!
                    :semantic-route-table-mismatch
                    #(check (assoc-in base [:semantic-route-handlers :route/hidden]
                                      (fn [_] nil))))]
          (is (= #{} (:missing data)))
          (is (= #{:route/hidden} (:unexpected data))))))))

(deftest replacement-raw-handler-is-rejected
  (let [{:keys [base]} (environment)]
    (with-summary summary
      (fn []
        (reject-binding! :unissued-route-handler
                         #(check (assoc-in base [:semantic-route-handlers route-1]
                                           (fn [_] {:status 200}))))))))

(deftest copied-metadata-does-not-issue-a-handler
  (let [{:keys [base handler]} (environment)
        imitation (with-meta (fn [_] {:status 200}) (meta handler))]
    (is (not (binding/certified-handler? imitation)))
    (with-summary summary
      (fn []
        (reject-binding! :unissued-route-handler
                         #(check (assoc-in base [:semantic-route-handlers route-2]
                                           imitation)))))))

(deftest even-a-valid-second-issued-handler-cannot-substitute-for-the-first
  (let [{:keys [base executor source]} (environment)
        other-executor (plan/build-certified-plan-operation! source)
        other-handler (binding/issue-certified-handler!
                       {:executor other-executor :current-source source
                        :request->input! (fn [_] (reconcile-input))
                        :result->response! (fn [_ r] r)})]
    (is (binding/certified-handler? other-handler))
    (is (not (identical? executor other-executor)))
    (with-summary summary
      (fn []
        (reject-binding! :swapped-route-executor
                         #(check (assoc-in base [:semantic-route-handlers route-1]
                                           other-handler)))))))

(deftest raw-executor-in-certification-table-is-rejected
  (let [{:keys [base source]} (environment)]
    (with-summary summary
      (fn []
        (reject-binding! :unregistered-certified-plan-executor
                         #(check (assoc-in base [:certified-operations claim :executor]
                                           (fn [& _] nil))))))))

(deftest second-valid-executor-cannot-be-spliced-into-binding
  (let [{:keys [base source]} (environment)
        second (plan/build-certified-plan-operation! source)]
    (with-summary summary
      (fn []
        (reject-binding! :swapped-route-executor
                         #(check (assoc-in base [:certified-operations claim :executor]
                                           second)))))))

(deftest changed-acquisition-callback-invalidates-source
  (let [{:keys [base source]} (environment)
        changed (assoc source :current-document! (fn [_ _] {:changed true}))]
    (with-summary summary
      (fn []
        (reject-plan! :stale-plan-source
                      #(check (assoc-in base
                                        [:certified-operations claim :current-source]
                                        changed)))))))

(deftest changed-proofs-are-not-current-certificates
  (let [{:keys [base source]} (environment)
        changed (assoc-in source [:certified-source :proof-source :steps 0 :id]
                          :step/different)]
    (with-summary summary
      (fn []
        (reject-plan! :stale-plan-source
                      #(check (assoc-in base
                                        [:certified-operations claim :current-source]
                                        changed)))))))

(deftest missing-and-invented-certification-declarations-fail
  (let [{:keys [base]} (environment)]
    (with-summary summary
      (fn []
        (reject-binding! :invalid-certified-operation-table
                         #(check (assoc base :certified-operations {})))
        (let [data (reject-binding!
                    :unknown-certified-operation
                    #(check (assoc-in base [:certified-operations :example/forged]
                                      {:executor :fake :current-source {}})))]
          (is (= #{:example/forged} (:unknown data))))))))

(deftest certified-operation-must-have-a-semantic-route
  (let [{:keys [base]} (environment)
        no-claim-route (assoc-in summary [claim :routes] [])]
    (with-summary no-claim-route
      (fn []
        (reject-binding! :missing-certified-operation-route
                         #(check (assoc base :semantic-route-handlers
                                        {route-3 (fn [_] nil)})))))))

(deftest unrelated-route-free-operation-is-explicitly-uncertified
  (let [{:keys [base]} (environment)
        no-cancel-route (assoc-in summary [cancel :routes] [])]
    (with-summary no-cancel-route
      (fn []
        (let [report (check (update base :semantic-route-handlers dissoc route-3))]
          (is (= #{cancel} (:uncertified-operations report)))
          (is (= #{route-1 route-2} (:route-ids report))))))))

(deftest fabricated-application-assembly-is-rejected
  (let [{:keys [base]} (environment)]
    (reject-binding! :invalid-or-stale-application-assembly
                     #(check base))
    (reject-binding! :invalid-or-stale-application-assembly
                     #(check (assoc base :application-assembly
                                    {:type :gesso.live.application-preflight/type
                                     :valid? true})))))

(deftest copied-report-cannot-be-used-as-a-route-credential
  (let [{:keys [base]} (environment)
        report (with-summary summary (fn [] (check base)))]
    ;; Real ApplicationAssembly validation must not be stubbed when testing a
    ;; forged assembly. Reports are observations, never assembly handles.
    (reject-binding! :invalid-or-stale-application-assembly
                     #(check (assoc base :application-assembly
                                    (assoc report :valid? true))))
    (with-summary summary
      (fn []
        (reject-binding! :invalid-route-handler-table
                         #(check (assoc base :semantic-route-handlers
                                        {route-1 report route-2 report
                                         route-3 report})))))))

(deftest issuing-handlers-is-a-closed-source-contract
  (let [{:keys [issue-source executor source]} (environment)]
    (reject-binding! :wrong-keys
                     #(binding/issue-certified-handler!
                       (assoc issue-source :certified? true)))
    (reject-binding! :wrong-keys
                     #(binding/issue-certified-handler!
                       (dissoc issue-source :current-source)))
    (reject-binding! :metadata-not-allowed
                     #(binding/issue-certified-handler!
                       (with-meta issue-source {:valid? true})))
    (reject-binding! :invalid-handler-callbacks
                     #(binding/issue-certified-handler!
                       (assoc issue-source :request->input! nil)))
    (reject-binding! :invalid-handler-callbacks
                     #(binding/issue-certified-handler!
                       (assoc issue-source :result->response! :unchecked)))
    (reject-binding! :unregistered-certified-plan-executor
                     #(binding/issue-certified-handler!
                       (assoc issue-source :executor (fn [& _] nil))))
    (is (binding/certified-handler?
         (binding/issue-certified-handler! issue-source)))
    (is (plan/checked-plan-operation? executor))
    (is (map? source))))

(deftest certified-operation-table-has-no-copyable-authority
  (let [{:keys [base executor source]} (environment)]
    (with-summary summary
      (fn []
        (reject-binding! :invalid-certified-operation-table
                         #(check (assoc base :certified-operations
                                        (with-meta (:certified-operations base)
                                          {:proof true}))))
        (reject-binding! :expected-plain-map
                         #(check (assoc-in base [:certified-operations claim]
                                           [:executor executor :current-source source])))
        (reject-binding! :wrong-keys
                         #(check (assoc-in base [:certified-operations claim]
                                           {:executor executor :current-source source
                                            :certificate {:valid? true}})))))))

(deftest semantic-route-table-must-be-closed-and-callable
  (let [{:keys [base]} (environment)]
    (with-summary summary
      (fn []
        (doseq [bad [nil [] {route-1 nil} {route-1 {:fn :fake}}
                     (with-meta (:semantic-route-handlers base) {:valid? true})]]
          (reject-binding! :invalid-route-handler-table
                           #(check (assoc base :semantic-route-handlers bad))))))))

(deftest options-must-be-exact-plain-data
  (let [{:keys [base]} (environment)]
    (reject-binding! :expected-plain-map #(check []))
    (reject-binding! :wrong-keys #(check (assoc base :valid? true)))
    (reject-binding! :wrong-keys #(check (dissoc base :application-assembly)))
    (reject-binding! :metadata-not-allowed
                     #(check (with-meta base {:authority true})))))

(deftest malformed-operation-route-summaries-are-rejected
  (let [{:keys [base]} (environment)]
    (doseq [[bad-summary reason]
            [[nil :invalid-application-operation-summary]
             [{"wrong" {:routes []}} :invalid-application-operation]
             [{claim {:routes nil}} :invalid-declared-operation-routes]
             [{claim {:routes [{:route-id "wrong" :method :post :path "/claim"}]}}
              :invalid-route-coordinate]
             [{claim {:routes [{:route-id route-1 :method :post :path ""}]}}
              :invalid-route-coordinate]
             [{claim {:routes [{:route-id route-1 :method :post :path "/one"}
                               {:route-id route-1 :method :post :path "/two"}]}}
              :duplicate-route-id]]]
      (with-summary bad-summary
        (fn []
          (reject-binding! reason #(check base)))))))

(deftest source-relative-report-is-not-deployment-adoption
  (let [{:keys [base]} (environment)
        live-router (atom (:semantic-route-handlers base))]
    (with-summary summary
      (fn []
        (let [snapshot @live-router
              report (report-remains-conditional!
                      (check (assoc base :semantic-route-handlers snapshot)))]
          ;; An independently changed router is not observable from an old
          ;; snapshot. This is a deliberately DOCUMENTED trusted boundary,
          ;; not a claim that Gesso has checked the deployed router.
          (swap! live-router assoc route-1 (fn [_] {:status 500}))
          (is (not (identical? (get snapshot route-1)
                               (get @live-router route-1))))
          (is (= :not-established (:deployment-adoption report)))
          (is (contains? (:open-assumptions report)
                         :installed-route-table-is-current-and-complete)))))))


(deftest selected-issued-binding-is-generic-and-exact
  (let [{:keys [source executor handler issue-source]} (environment)
        second (plan/build-certified-plan-operation! source)
        second-handler (binding/issue-certified-handler!
                        (assoc issue-source :executor second))
        imitation (with-meta (fn [_] nil) {:issued true})]
    (is (binding/issued-handler-binding? handler executor claim source))
    (is (not (binding/issued-handler-binding? handler executor cancel source)))
    (is (not (binding/issued-handler-binding? handler second claim source)))
    (is (not (binding/issued-handler-binding? second-handler executor claim source)))
    (is (binding/issued-handler-binding? second-handler second claim source))
    (is (not (binding/issued-handler-binding? handler executor claim
                                              (assoc source :unbound true))))
    (is (not (binding/issued-handler-binding? imitation executor claim source)))))

(deftest reusable-authorization-assembly-check-selects-exact-operation-and-route
  (let [{:keys [source executor handler unrelated]} (environment)
        operations {claim (fixture/trusted-operation claim
                                                     {:published-change-topics #{}})
                    cancel (fixture/trusted-operation cancel
                                                      {:published-change-topics #{}})}]
    (fixture/with-closed-application
      {:operations operations}
      (fn [{:keys [assembly]}]
        (let [contract {:application-assembly assembly
                        :semantic-route-handlers {claim handler cancel unrelated}
                        :certified-operations
                        {claim {:executor executor :current-source source}}}
              catalog-source {:catalog {:operations [{:id claim
                                                      :construction (:construction source)}]}
                              :physical-field-map {}}
              selection {:contract-source contract
                         :catalog-source catalog-source
                         :operation claim :route-id claim}
              check authorization/check-selected-binding!
              accepted (check selection)
              second-executor (plan/build-certified-plan-operation! source)
              second-handler (binding/issue-certified-handler!
                              {:executor second-executor :current-source source
                               :request->input! (fn [_] (reconcile-input))
                               :result->response! (fn [_ r] r)})]
          (is (= authorization/report-type (:type accepted)))
          (is (= :selected-original-construction-to-exact-issued-handler
                 (:checking accepted)))
          (is (= :not-established (:application-assembly-closure accepted)))
          (is (= :not-established (:installed-handler-adoption accepted)))
          (is (= :not-established (:authorization-policy-provenance accepted)))
          (is (= :none (:transaction-authority accepted)))
          (is (= :not-issued (:certificate accepted)))
          ;; This selected binding coexists with another declared, un-certified
          ;; operation. The local relation does not claim anything about it.
          (is (= #{claim cancel}
                 (set (keys (:semantic-route-handlers contract)))))
          (doseq [bad [(assoc selection :operation cancel)
                       (assoc selection :route-id cancel)
                       (assoc-in selection [:contract-source :semantic-route-handlers claim]
                                 unrelated)
                       (assoc-in selection [:contract-source :semantic-route-handlers claim]
                                 second-handler)
                       (assoc-in selection [:catalog-source :catalog :operations 0
                                             :construction :subject :id] cancel)
                       (assoc-in selection [:catalog-source :catalog :operations]
                                 [{:id claim :construction (:construction source)}
                                  {:id claim :construction (:construction source)}])]]
            (let [ex (try (check bad) nil
                          (catch clojure.lang.ExceptionInfo e (ex-data e)))]
              (is (= :authorization-not-bound-to-original-assembly
                     (:reason ex)) (pr-str bad)))))))))


(defn- with-genuine-catalog-selection [f]
  ;; No stubs of the ApplicationAssembly recognizer or operation summary:
  ;; both routes for Claim and the independent ordinary Cancel route come
  ;; from a current, physically recorded application-preflight fixture.
  (let [{:keys [source executor handler unrelated issue-source]} (environment)
        operations {claim (fixture/trusted-operation claim
                                                     {:published-change-topics #{}})
                    cancel (fixture/trusted-operation cancel
                                                      {:published-change-topics #{}})}
        routes {route-1 {:operation claim :path "/claim"}
                route-2 {:operation claim :path "/claim/alternate"}
                route-3 {:operation cancel :path "/cancel"}}]
    (fixture/with-surfaced-application
      {:operations operations
       :route-capabilities (fixture/custom-route-capabilities routes)
       :rendered-surfaces {:proof [:div "safe"]}}
      (fn [{:keys [assembly report]}]
        (is (application/valid? report) (pr-str (:errors report)))
        (is (some? assembly))
        (when assembly
          (let [base {:application-assembly assembly
                      :semantic-route-handlers
                      {route-1 handler route-2 handler route-3 unrelated}
                      :certified-operations
                      {claim {:executor executor :current-source source}}}
                catalog-source
                {:catalog {:operations [{:id claim
                                        :construction (:construction source)}]}
                 :physical-field-map {}}]
            (f {:base base :source source :issue-source issue-source
                :unrelated unrelated :catalog-source catalog-source
                :input {:contract-source base
                        :catalog-source catalog-source}})))))))

(deftest catalog-wide-coverage-checks-every-issued-route
  (with-genuine-catalog-selection
    (fn [{:keys [input]}]
      (let [report (authorization/check-declared-route-coverage! input)]
        (is (= authorization/coverage-report-type (:type report)))
        (is (= :all-declared-catalog-operations-and-issued-routes
               (:checking report)))
        (is (= #{claim} (:catalog-operations report)))
        (is (= #{route-1 route-2} (:matched-certified-route-ids report)))
        (is (= #{cancel} (:uncertified-application-operations report)))
        (is (= :selected-not-all-possible-application-policy
               (:catalog-scope report)))
        (is (= :source-relative-checked (:current-assembly-closure report)))
        (is (= :not-established (:installed-handler-adoption report)))
        (is (= :not-established (:authorization-policy-provenance report)))
        (is (= :none (:transaction-authority report)))
        (is (= :not-issued (:certificate report)))))))

(deftest catalog-wide-coverage-rejects-missing-replaced-or-undeclared-bindings
  (with-genuine-catalog-selection
    (fn [{:keys [input source unrelated issue-source catalog-source]}]
      (let [second-executor (plan/build-certified-plan-operation! source)
            second-handler (binding/issue-certified-handler!
                            (assoc issue-source :executor second-executor))
            bad-catalog (fn [entries]
                          (assoc-in input [:catalog-source :catalog :operations]
                                    entries))
            entry {:id claim :construction (:construction source)}
            mutations [(assoc-in input [:contract-source :semantic-route-handlers route-2]
                                 unrelated)
                       (assoc-in input [:contract-source :semantic-route-handlers route-2]
                                 second-handler)
                       (update-in input [:contract-source :semantic-route-handlers]
                                  dissoc route-2)
                       (assoc-in input [:contract-source :certified-operations] {})
                       (bad-catalog [])
                       (bad-catalog [entry entry])
                       (bad-catalog [{:id cancel :construction (:construction source)}])
                       (bad-catalog [(assoc-in entry [:construction :subject :id]
                                               cancel)])
                       (assoc input :certified? true)
                       (assoc input :catalog-source
                              (with-meta catalog-source {:proof :forged}))]]
        (doseq [bad mutations]
          (let [data (failure #(authorization/check-declared-route-coverage!
                               bad))]
            (is (= :authorization-not-bound-to-original-assembly
                   (:reason data)) (pr-str bad))))))))

(deftest coverage-pins-original-contract-checker
  (with-genuine-catalog-selection
    (fn [{:keys [input]}]
      (with-redefs [binding/check-certified-contracts!
                    (fn [_] {:type binding/report-type
                             :version binding/adapter-version
                             :certified-operations {claim {:route-ids #{route-1 route-2}}}
                             :matched-certified-route-ids #{route-1 route-2}})]
        (is (= :authorization-not-bound-to-original-assembly
               (:reason (failure #(authorization/check-declared-route-coverage!
                                  input)))))))))

(deftest catalog-coverage-refuses-substituted-selected-binding-checker
  (with-genuine-catalog-selection
    (fn [{:keys [input]}]
      (with-redefs [authorization/check-selected-binding!
                    (fn [_] {:checking :selected-original-construction-to-exact-issued-handler})]
        (is (= :authorization-not-bound-to-original-assembly
               (:reason (failure #(authorization/check-declared-route-coverage!
                                  input)))))
        (is (= :authorization-not-bound-to-original-assembly
               (:reason (failure #(authorization/check-declared-route-coverage!
                                  (assoc-in input
                                            [:catalog-source :catalog :operations 0
                                             :construction :subject :id]
                                            cancel))))))))))

(deftest catalog-coverage-pins-original-application-verifiers
  (with-genuine-catalog-selection
    (fn [{:keys [input]}]
      (is (= authorization/coverage-report-type
             (:type (authorization/check-declared-route-coverage! input))))
      (with-redefs [application/application-assembly? (constantly true)]
        (is (= :authorization-not-bound-to-original-assembly
               (:reason (failure #(authorization/check-declared-route-coverage!
                                  input))))))
      (with-redefs [application/operation-summary (fn [_] summary)]
        (is (= :authorization-not-bound-to-original-assembly
               (:reason (failure #(authorization/check-declared-route-coverage!
                                  input)))))))))

(deftest catalog-coverage-pins-original-plan-proof-verifier-roots
  ;; The ORIGINAL certified-contract checker calls these mutable public Vars
  ;; while rechecking each v1251 executor. Even replacing a current-plan proof
  ;; check with a permissive callback must never yield catalog-wide coverage.
  ;; Set up the genuine ApplicationAssembly BEFORE substituting a proof root.
  (with-genuine-catalog-selection
    (fn [{:keys [input]}]
      (is (= authorization/coverage-report-type
             (:type (authorization/check-declared-route-coverage! input))))
      (doseq [[label v replacement]
              [[:registered-plan #'plan/checked-plan-operation? (constantly true)]
               [:current-plan #'plan/require-current-plan-operation! (constantly nil)]
               [:plan-description #'plan/describe-plan-operation nil]
               [:operation-description #'certified-operation/describe-operation nil]]]
        (let [original (var-get v)
              substitute (or replacement (fn [& args] (apply original args)))]
          (with-redefs-fn {v substitute}
            (fn []
              (is (= :authorization-not-bound-to-original-assembly
                     (:reason (failure
                               #(authorization/check-declared-route-coverage!
                                 input))))
                  (pr-str label)))))))))

(deftest fabricated-assembly-and-invented-routes-cannot-certify-catalog
  (let [{:keys [base source]} (environment)
        input {:contract-source base
               :catalog-source
               {:catalog {:operations [{:id claim
                                        :construction (:construction source)}]}
                :physical-field-map {}}}]
    ;; This exact fixture previously succeeded because both public application
    ;; verification functions were replaced. Its catalog must never qualify.
    (with-summary summary
      (fn []
        (is (= :authorization-not-bound-to-original-assembly
               (:reason (failure #(authorization/check-declared-route-coverage!
                                  input)))))))))

(deftest catalog-wide-coverage-uses-genuine-application-assembly
  (let [{:keys [source executor handler]} (environment)
        operations {claim (fixture/trusted-operation claim
                                                     {:published-change-topics #{}})}]
    (fixture/with-closed-application
      {:operations operations}
      (fn [{:keys [assembly]}]
        (let [contract {:application-assembly assembly
                        :semantic-route-handlers {claim handler}
                        :certified-operations
                        {claim {:executor executor :current-source source}}}
              catalog-source {:catalog {:operations [{:id claim
                                                      :construction (:construction source)}]}
                              :physical-field-map {}}
              report (authorization/check-declared-route-coverage!
                      {:contract-source contract :catalog-source catalog-source})]
          (is (= #{claim} (:catalog-operations report)))
          (is (= #{claim} (:matched-certified-route-ids report)))
          (is (= #{} (:uncertified-application-operations report)))
          (is (= :not-established (:installed-handler-adoption report))))))))
