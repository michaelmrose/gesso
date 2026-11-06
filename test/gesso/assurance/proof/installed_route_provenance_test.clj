(ns gesso.assurance.proof.installed-route-provenance-test
  "v1477: Permanent adversarial tests for the v1476 literal Ring dispatcher.

   Positive paths use a real closed ApplicationAssembly, a v1251-issued plan
   executor, and a v1253-issued Ring handler. No production checker is stubbed
   to make an installation succeed. This proves only synchronous, process-local
   identity and observed invocation, NOT Biff deployment, XTDB refinement,
   authorization, route-table completeness outside this owned dispatcher, or
   other writer coverage."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.installed-route-provenance :as installed]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.model.certified-plan :as plan]
            [gesso.model.certified-plan-test]))

(def ^:private operation :example/claim)
(def ^:dynamic *case* nil)

(defn- plan-fixture [name]
  (let [v (ns-resolve 'gesso.model.certified-plan-test name)]
    (when-not v
      (throw (ex-info "Missing v1252 test-only fixture" {:symbol name})))
    (var-get v)))

(defn- ring-request
  ([] (ring-request 1))
  ([id] {:request-method :post
         :uri "/operations/example/claim"
         :request-id id}))

(defn- rejection [thunk]
  (try
    (thunk)
    :not-rejected
    (catch clojure.lang.ExceptionInfo error
      (let [data (ex-data error)]
        (if (:installed-route-provenance/rejection data)
          (:reason data)
          [:unexpected-error data])))))

(defn- rejects! [reason thunk]
  (is (= reason (rejection thunk)) (str "Expected checked-route rejection " reason)))

(use-fixtures
  :once
  (fn [run]
    (let [source (:source ((plan-fixture 'environment)))
          executor (plan/build-certified-plan-operation! source)
          calls (atom [])
          handler (certified/issue-certified-handler!
                   {:executor executor
                    :current-source source
                    :request->input!
                    (fn [request]
                      (swap! calls conj [:input request])
                      ((plan-fixture 'invocation) :reconcile))
                    :result->response!
                    (fn [request result]
                      (swap! calls conj [:response request result])
                      {:status 200 :body result})})]
      (fixture/with-closed-application
        {:operations {operation (fixture/trusted-operation
                                 operation {:published-change-topics #{}})}}
        (fn [{:keys [assembly]}]
          (let [routes (get-in (application/operation-summary
                                (:operation-acquisition-assembly assembly))
                               [operation :routes])
                contract-source
                {:application-assembly assembly
                 :semantic-route-handlers {operation handler}
                 :certified-operations
                 {operation {:executor executor :current-source source}}}
                router (installed/install-checked-ring-router! contract-source)]
            (is (= [{:route-id operation :method :post
                     :path "/operations/example/claim"
                     :required-transport :htmx}]
                   routes))
            (binding [*case* {:source contract-source :router router
                              :handler handler :calls calls
                              :executor executor :plan-source source}]
              (run))))))))

(defn- description []
  (installed/describe-checked-ring-router (:router *case*) (:source *case*)))

(deftest real-assembly-router-is-recognized-but-never-deployment-proof
  (let [{:keys [router source]} *case*
        report (description)]
    (is (installed/checked-ring-router? router))
    (is (not (installed/checked-ring-router? (fn [_] {:status 200}))))
    (is (= installed/report-type (:type report)))
    (is (= installed/report-version (:version report)))
    (is (= #{operation} (:route-ids report)))
    (is (= #{operation} (:certified-route-ids report)))
    (is (= 1 (:route-count report)))
    (is (= :registered-in-current-jvm (:router-callable report)))
    (is (= :not-established (:host-adoption report)))
    (is (= :not-established (:physical-correspondence report)))
    (is (= :not-issued (:certification report)))
    (is (= :none (:authority report)))
    (is (contains? (:open-obligations report)
                   :no-independent-router-or-raw-writer-bypass))
    (is (contains? (:open-obligations report)
                   :checked-ring-dispatcher-not-attested-as-deployed-biff-handler))
    (is (not (contains? report :valid?)))
    (is (= report (installed/describe-checked-ring-router router source)))))

(deftest one-matching-ring-request-invokes-the-issued-handler-once
  (let [{:keys [router source calls]} *case*
        request (ring-request :unique)
        start (count @calls)
        {:keys [witness response report]}
        (installed/observe-checked-ring-route! router source request)]
    (is (= 200 (:status response)))
    (is (= 2 (- (count @calls) start)) "Conversion and response run once")
    (is (= [:input request] (nth @calls start)))
    (is (= :response (first (nth @calls (inc start)))))
    (is (identical? request (second (nth @calls (inc start)))))
    (is (= (:body response) (nth (nth @calls (inc start)) 2)))
    (is (= operation (:observed-route-id report)))
    (is (= :one-actual-in-process-invocation (:dispatch report)))
    (is (= :none (:authority report)))
    (is (= :not-established (:host-adoption report)))
    (is (= (description)
           (installed/require-current-route-dispatch!
            witness router source request)))
    (is (= 2 (- (count @calls) start))
        "Witness revalidation must not call the handler again")))

(deftest unmatched-and-malformed-requests-never-create-route-witness
  (let [{:keys [router source calls]} *case*
        before (count @calls)]
    (doseq [request [(assoc (ring-request) :request-method :get)
                     (assoc (ring-request) :uri "/not-registered")
                     (assoc (ring-request) :uri "/operations/example/claim/")]]
      (is (= 404 (:status (router request))))
      (rejects! :unmatched-or-invalid-ring-request
                #(installed/observe-checked-ring-route! router source request)))
    (doseq [request [nil [] {} {:request-method :post :uri nil}]]
      (rejects! :invalid-ring-request #(router request)))
    (is (= before (count @calls)) "No unmatched request executes an operation")))

(deftest unregistered-router-and-copied-source-cannot-authenticate-a-witness
  (let [{:keys [router source calls]} *case*
        request (ring-request :identity)
        other-router (installed/install-checked-ring-router! source)
        {:keys [witness]} (installed/observe-checked-ring-route!
                            router source request)
        before (count @calls)]
    (rejects! :unregistered-ring-router
              #(installed/describe-checked-ring-router
                (fn [_] {:status 200}) source))
    (rejects! :changed-ring-router-source-object
              #(installed/describe-checked-ring-router router (into {} source)))
    (rejects! :changed-ring-router-source-object
              #(installed/require-current-route-dispatch!
                witness router (into {} source) request))
    (rejects! :stale-or-forged-route-dispatch-witness
              #(installed/require-current-route-dispatch!
                witness other-router source request))
    (rejects! :stale-or-forged-route-dispatch-witness
              #(installed/require-current-route-dispatch!
                witness router source (into {} request)))
    (rejects! :stale-or-forged-route-dispatch-witness
              #(installed/require-current-route-dispatch!
                (Object.) router source request))
    (is (= before (count @calls)) "Attempted forgeries cannot re-execute")))

(deftest report-and-stolen-function-metadata-are-not-route-credentials
  (let [{:keys [router source]} *case*
        request (ring-request :metadata)
        report (description)
        fake (with-meta (fn [_] {:status 200}) (meta router))]
    (is (= report (into {} report)))
    (is (not (installed/checked-ring-router? fake)))
    (rejects! :unregistered-ring-router
              #(installed/observe-checked-ring-route! fake source request))
    (rejects! :stale-or-forged-route-dispatch-witness
              #(installed/require-current-route-dispatch!
                report router source request))))

(deftest stale-or-substituted-contract-sources-are-fail-closed
  (let [{:keys [router source handler]} *case*
        raw (fn [_] {:status 200})]
    (doseq [altered [(assoc source :semantic-route-handlers {operation raw})
                     (assoc source :certified-operations {})
                     (assoc source :extra :claimed-certified)
                     (dissoc source :certified-operations)
                     (with-meta source {:valid? true})]]
      (rejects! :changed-ring-router-source-object
                #(installed/describe-checked-ring-router router altered)))
    (rejects! :invalid-contract-source
              #(installed/install-checked-ring-router!
                (assoc source :extra :claimed-certified)))
    (rejects! :invalid-contract-source
              #(installed/install-checked-ring-router!
                (with-meta source {:valid? true})))
    (is (identical? handler (get-in source [:semantic-route-handlers operation])))
    (is (= 1 (:route-count (description))))))

(deftest unissued-and-missing-installed-handlers-fail-original-contract
  (let [{:keys [source]} *case*
        variations
        [[:unissued-route-handler
          (assoc-in source [:semantic-route-handlers operation]
                    (fn [_] {:status 200}))]
         [:semantic-route-table-mismatch
          (assoc source :semantic-route-handlers {})]]]
    (doseq [[reason candidate] variations]
      (let [data (try
                   (installed/install-checked-ring-router! candidate)
                   nil
                   (catch clojure.lang.ExceptionInfo error
                     (ex-data error)))]
        (is (= :gesso.live.application-preflight.certified-contract/invalid-certified-contract
               (:error/type data)) (pr-str data))
        (is (= reason (:reason data)) (pr-str data))))))

(deftest substituted-source-checkers-never-produce-a-route-handle
  (let [{:keys [router source]} *case*]
    (with-redefs [certified/check-certified-contracts! (fn [_] {:route-ids #{operation}})]
      (rejects! :substituted-application-or-route-checker
                #(installed/install-checked-ring-router! source))
      (rejects! :substituted-application-or-route-checker
                #(installed/describe-checked-ring-router router source)))
    (with-redefs [application/operation-summary (fn [_] {operation {:routes []}})]
      (rejects! :substituted-application-or-route-checker
                #(installed/install-checked-ring-router! source)))
    (is (= 1 (:route-count (description))))))

(deftest throwing-issued-handler-does-not-mint-observation
  (let [{:keys [source plan-source executor router calls]} *case*
        throwing-handler (certified/issue-certified-handler!
                          {:executor executor :current-source plan-source
                           :request->input! (fn [_]
                                              (throw (ex-info "test handler failure"
                                                              {:failure :forced})))
                           :result->response! (fn [_ _] {:status 200})})
        altered-source (assoc-in source [:semantic-route-handlers operation]
                                 throwing-handler)
        throwing-router (installed/install-checked-ring-router! altered-source)
        request (ring-request :throwing)
        before (count @calls)]
    (is (= :forced
           (try
             (installed/observe-checked-ring-route!
              throwing-router altered-source request)
             :did-not-throw
             (catch clojure.lang.ExceptionInfo e
               (:failure (ex-data e))))))
    (is (= before (count @calls)))
    (is (= 1 (:route-count (installed/describe-checked-ring-router
                            throwing-router altered-source))))
    (is (= 1 (:route-count (installed/describe-checked-ring-router
                            router source))))))

(deftest nested-observation-is-rejected-without-duplicating-the-handler
  (let [{:keys [source plan-source executor calls]} *case*
        router-slot (atom nil)
        nested-source-slot (atom nil)
        input-calls (atom 0)
        nested-handler (certified/issue-certified-handler!
                        {:executor executor :current-source plan-source
                         :request->input!
                         (fn [request]
                           (swap! input-calls inc)
                           (installed/observe-checked-ring-route!
                            @router-slot @nested-source-slot request))
                         :result->response! (fn [_ _] {:status 200})})
        nested-source (assoc-in source [:semantic-route-handlers operation]
                                nested-handler)
        nested-router (installed/install-checked-ring-router! nested-source)
        request (ring-request :nested)
        before (count @calls)]
    (reset! router-slot nested-router)
    (reset! nested-source-slot nested-source)
    (rejects! :nested-route-observation
              #(installed/observe-checked-ring-route!
                nested-router nested-source request))
    (is (= 1 @input-calls))
    (is (= before (count @calls)) "A nested observation never reaches another handler")
    (is (= 1 (:route-count (description))))))
