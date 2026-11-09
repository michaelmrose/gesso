(ns gesso.assurance.proof.xtdb-gateway-route-plan-bound-target-test
  "v1585: permanent adversarial correspondence tests for v1584's exact,
   Gesso-issued literal target callback installed in the current certified
   plan. These checks use a real v1251 certified-plan executor and the checked
   application assembly; no fabricated certificates or transaction receipts.

   The issued literal targets are still application premises. These tests do
   NOT prove business target selection, authorization, live Biff installation,
   dynamic route parameters, universal refinement, or XTDB commit causality."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-route-target-provenance :as provenance]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.model.certified-plan :as plan]
            [gesso.model.certified-plan-test])
  (:import (java.math BigInteger)))

(def ^:private operation :example/claim)
(def ^:private route operation)
(def ^:private path "/operations/example/claim")
(def ^:private private-id "v1585-private-document")
(def ^:private private-canary "v1585-private-should-not-appear")
(def ^:private request {:request-method :post :uri path})
(def ^:private expected
  {:operation-id operation :command-operation :claim
   :target [:requests private-id]})
(def ^:private route-row
  {:method :post :uri path :target [:requests private-id]})
(def ^:dynamic *verified* nil)

(defn- original [symbol]
  (let [v (ns-resolve 'gesso.model.certified-plan-test symbol)]
    (when-not v
      (throw (ex-info "Missing certified plan test helper"
                      {:reason :missing-certified-plan-helper})))
    (var-get v)))

(defn- certified-case []
  (let [target! (provenance/issue-literal-route-plan-target! {route route-row})
        source (assoc (:source ((original 'environment))) :target! target!)
        executor (plan/build-certified-plan-operation! source)
        handler (certified/issue-certified-handler!
                 {:executor executor
                  :current-source source
                  :request->input! (fn [_] ((original 'invocation) :reconcile))
                  :result->response! (fn [_ result]
                                       {:status 200 :body result})})]
    {:target! target!
     :source source
     :executor executor
     :handler handler}))

(defn- certified-case-fixture [run]
  (let [case (certified-case)]
    (fixture/with-closed-application
     {:operations {operation (fixture/trusted-operation
                              operation {:published-change-topics #{}})}}
     (fn [{:keys [assembly]}]
       (binding [*verified* (assoc case :contract
                                  {:application-assembly assembly
                                   :semantic-route-handlers
                                   {operation (:handler case)}
                                   :certified-operations
                                   {operation {:executor (:executor case)
                                               :current-source (:source case)}}})]
         (run))))))

(use-fixtures :once certified-case-fixture)

(defn- issue! []
  (provenance/issue-certified-plan-bound-literal-route-target-resolver!
   (:contract *verified*) {operation (:target! *verified*)}))

(defn- independently-certified-with-target-table [table]
  ;; Make a genuinely fresh certified executor and certified handler with
  ;; the alternative original :target! callback. This isolates the route
  ;; coverage/selector check from earlier executor-source substitution guards.
  (let [target! (provenance/issue-literal-route-plan-target! table)
        source (assoc (:source *verified*) :target! target!)
        executor (plan/build-certified-plan-operation! source)
        handler (certified/issue-certified-handler!
                 {:executor executor :current-source source
                  :request->input! (fn [_] ((original 'invocation) :reconcile))
                  :result->response! (fn [_ result] {:status 200 :body result})})
        contract (-> (:contract *verified*)
                     (assoc-in [:certified-operations operation]
                               {:executor executor :current-source source})
                     (assoc-in [:semantic-route-handlers operation] handler))]
    {:target! target! :contract contract}))

(defn- captured [f]
  (try {:accepted (f)}
       (catch clojure.lang.ExceptionInfo e
         {:message (ex-message e) :data (ex-data e)})))

(defn- reject! [reason f]
  (let [{:keys [data message] :as observation} (captured f)]
    (is (not (contains? observation :accepted))
        "A rejected call must not return a resolver or target")
    (is (= true (:xtdb-gateway-route-target-provenance/rejection data)))
    (is (= reason (:reason data)) (pr-str observation))
    (is (= #{:xtdb-gateway-route-target-provenance/rejection :reason}
           (set (keys data))) "Errors must not disclose sources or targets")
    (is (= "Invalid source-linked XTDB route target provenance." message))
    (is (not (str/includes? (str message " " (pr-str data)) private-id)))
    (is (not (str/includes? (str message " " (pr-str data)) private-canary)))))

(deftest issued-plan-callback-is-the-current-plan-target
  (let [{:keys [target! contract source]} *verified*
        resolver (:expected-target! (issue!))]
    (is (identical? target! (:target! source)))
    (is (identical? target!
                    (get-in contract [:certified-operations operation
                                      :current-source :target!])))
    (is (= [:requests private-id] (target! request {:ignored true})))
    (is (= expected (resolver request)))
    (is (= expected (resolver (assoc request
                                      :params {:id private-canary}
                                      :path-params {:id private-canary}
                                      :candidate-command {:target [:requests private-canary]}))))
    (is (= expected (resolver (assoc request :query-params {:target private-canary}))))
    (is (= expected (resolver (assoc request :body {:target private-canary}))))))

(deftest independently-issued-identical-callback-is-not-plan-identity
  (let [other (provenance/issue-literal-route-plan-target! {route route-row})]
    (is (= [:requests private-id] (other request nil)))
    (is (not (identical? other (:target! *verified*))))
    (reject! :certified-plan-target-callback-identity-mismatch
             #(provenance/issue-certified-plan-bound-literal-route-target-resolver!
               (:contract *verified*) {operation other}))
    (reject! :unissued-certified-plan-target-callback
             #(provenance/issue-certified-plan-bound-literal-route-target-resolver!
               (:contract *verified*)
               {operation (fn [_ _] [:requests private-id])}))
    (reject! :unissued-certified-plan-target-callback
             #(provenance/issue-certified-plan-bound-literal-route-target-resolver!
               (:contract *verified*)
               {operation (fn [ctx binding]
                            ((:target! *verified*) ctx binding))}))))

(deftest plan-callback-coverage-and-selector-must-match-certified-route
  (let [contract (:contract *verified*)
        source (:source *verified*)
        base {route route-row}]
    (doseq [bad [nil [] {} {operation nil} {:example/unrelated (:target! *verified*)}
                 {operation (:target! *verified*)
                  :example/unrelated (:target! *verified*)}]]
      (reject! :invalid-or-incomplete-issued-plan-targets
               #(provenance/issue-certified-plan-bound-literal-route-target-resolver!
                 contract bad)))
    ;; A malformed or empty declaration cannot even issue a plan callback.
    (reject! :invalid-or-ambiguous-literal-plan-target-declarations
             #(provenance/issue-literal-route-plan-target!
               (dissoc base route)))
    (reject! :invalid-or-ambiguous-literal-plan-target-declarations
             #(provenance/issue-literal-route-plan-target!
               (assoc base :route/other route-row)))
    ;; A callback with a different route-ID or selector is issued, but cannot
    ;; become an accepted substitute for the installed certified-plan source.
    (doseq [entries [{:route/other route-row}
                     {route (assoc route-row :uri "/else")}]]
      (let [target! (provenance/issue-literal-route-plan-target! entries)
            changed-contract (assoc-in contract
                                       [:certified-operations operation :current-source]
                                       (assoc source :target! target!))]
        (is (not (identical? target! (:target! source))))
        (is (not (contains? (captured
                            #(provenance/issue-certified-plan-bound-literal-route-target-resolver!
                              changed-contract {operation target!}))
                            :accepted)))))
    ;; Issued callback for a different route is not the plan's callback.
    (doseq [[entries reason] [[{route (assoc route-row :uri "/other")}
                             :certified-plan-target-callback-identity-mismatch]
                            [{:route/other route-row}
                             :certified-plan-target-callback-identity-mismatch]]]
      (let [other (provenance/issue-literal-route-plan-target! entries)]
        (reject! reason #(provenance/issue-certified-plan-bound-literal-route-target-resolver!
                          contract {operation other}))))))

(deftest freshly-certified-plans-still-require-exact-target-route-coverage
  (doseq [[table reason]
          [[{:route/other route-row} :issued-plan-target-route-coverage-mismatch]
           [{route (assoc route-row :uri "/a-different-route")}
            :issued-plan-target-selector-mismatch]
           [{route (assoc route-row :method :put)}
            :issued-plan-target-selector-mismatch]]]
    (testing (str "newly certified plan with alternative target table: " reason)
      (let [{:keys [target! contract]}
            (independently-certified-with-target-table table)]
        ;; This is not an old executor with a swapped callback: source, handler
        ;; and executor were created together, and are checked independently.
        (is (identical? target!
                        (get-in contract [:certified-operations operation
                                          :current-source :target!])))
        (reject! reason
                 #(provenance/issue-certified-plan-bound-literal-route-target-resolver!
                   contract {operation target!}))))))

(deftest issued-plan-target-is-fail-closed-on-unmatched-requests
  (let [target! (:target! *verified*)
        resolver (:expected-target! (issue!))]
    (doseq [bad [nil [] {:request-method :get :uri path}
                 {:request-method :post :uri "/else"}
                 {:request-method :post :uri (str path "/")}
                 {:request-method "post" :uri path}
                 (with-meta request {:injected true})]]
      (reject! :unmatched-issued-literal-plan-target
               #(target! bad nil)))
    (doseq [bad [nil [] {:request-method :get :uri path}
                 {:request-method :post :uri "/else"}
                 {:request-method :post :uri (str path "/")}
                 (with-meta request {:injected true})]]
      (reject! :unmatched-certified-literal-request
               #(resolver bad)))))

(deftest malformed-plan-target-tables-are-rejected-before-issuance
  (doseq [bad [nil [] {} {route (dissoc route-row :uri)}
               {route (assoc route-row :method "post")}
               {route (assoc route-row :uri "/items/:id")}
               {route (assoc route-row :target [:requests nil])}
               {route (assoc route-row :target [:requests (BigInteger. "4")])}
               {route (assoc route-row :target [:requests ""])}
               {route (assoc route-row :target [:private/requests "id"])}
               {route (assoc route-row :unexpected true)}
               {route route-row :route/other route-row}
               {route route-row :route/other (assoc route-row :target [:requests "another"])}
               (with-meta {route route-row} {:authorized true})]]
    (reject! :invalid-or-ambiguous-literal-plan-target-declarations
             #(provenance/issue-literal-route-plan-target! bad))))

(deftest resolver-revalidation-does-not-upgrade-assurance
  (let [{:keys [contract]} *verified*
        issuance (issue!)
        resolver (:expected-target! issuance)
        report (provenance/require-current-certified-literal-route-target-resolver!
                resolver contract)]
    (is (= provenance/report-type (:type report)))
    (is (= provenance/report-version (:version report)))
    (is (= :conditional-certified-literal-route-static-target-resolver (:status report)))
    (is (= 1 (:certified-route-count report)))
    (is (= :closed-issued-certified-plan-target-callback (:target-identity report)))
    (is (= :exact-issued-source-callback-checked
           (:certified-plan-target-callback-identity report)))
    (is (false? (:candidate-command-used-to-derive-target? report)))
    (is (false? (:request-document-parameters-used? report)))
    (is (= :application-premise-not-proved (:static-declaration-authenticity report)))
    (is (= :not-established (:physical-target-authorization report)))
    (is (= :not-established (:abstract-operation-to-command-refinement report)))
    (is (= :not-established (:physical-transaction-causality report)))
    (is (= :not-admitted (:admission report)))
    (is (= :not-issued (:certificate report)))
    (is (= :none (:transaction-authority report)))
    (is (contains? (:open-obligations report)
                   :startup-target-declarations-are-independently-correct-and-authorized))
    (is (contains? (:open-obligations report)
                   :external-writer-coverage-not-established))
    (is (not (str/includes? (pr-str report) private-id)))
    (is (= expected (resolver request)))))

(deftest issued-resolver-does-not-accept-a-copied-or-wrapped-source
  (let [contract (:contract *verified*)
        resolver (:expected-target! (issue!))
        tampered (assoc-in contract [:certified-operations operation
                                     :current-source :target!]
                           (fn [_ _] [:requests private-id]))]
    (reject! :unissued-target-resolver
             #(provenance/require-current-certified-literal-route-target-resolver!
               (fn [req] (resolver req)) contract))
    (reject! :changed-target-resolver-source
             #(provenance/require-current-certified-literal-route-target-resolver!
               resolver (assoc contract :canary private-canary)))
    ;; Actual reissue is required; a source map containing a substitute
    ;; callback must not be promoted to one checked against the old executor.
    (is (not (contains? (captured
                        #(provenance/issue-certified-plan-bound-literal-route-target-resolver!
                          tampered {operation (:target! *verified*)})) :accepted)))
    (is (= expected (resolver request)))))
