(ns gesso.assurance.proof.parameterized-route-provenance-test
  "v1488: Permanent adversarial coverage for v1487 checked Ring path templates.

   Fixtures construct genuine application-preflight assemblies, v1251-issued
   executors, and v1253-issued handlers. All checks concern this exact owned
   synchronous Ring router. No result proves host URI normalization, actual
   deployment, authorization, dynamic-route physical correlation, or XTDB
   execution refinement. The test-only issued executor never writes to XTDB."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.installed-route-provenance :as installed]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.model.certified-plan :as plan]
            [gesso.model.certified-plan-test]))

(def ^:private operation :example/claim)
(def ^:dynamic *case* nil)

(defn- plan-fixture [symbol]
  (let [v (ns-resolve 'gesso.model.certified-plan-test symbol)]
    (when-not v
      (throw (ex-info "Missing certified-plan test fixture" {:symbol symbol})))
    (var-get v)))

(defn- with-checked-source [routes f]
  (let [plan-source (:source ((plan-fixture 'environment)))
        executor (plan/build-certified-plan-operation! plan-source)
        calls (atom [])
        handler (certified/issue-certified-handler!
                 {:executor executor
                  :current-source plan-source
                  :request->input!
                  (fn [request]
                    (swap! calls conj request)
                    ((plan-fixture 'invocation) :reconcile))
                  :result->response!
                  (fn [_request result] {:status 200 :body result})})
        capabilities (fixture/custom-route-capabilities
                      (into {}
                            (for [[route-id method path] routes]
                              [route-id {:operation operation
                                         :method method :path path}])))
        operations {operation (fixture/trusted-operation
                               operation {:published-change-topics #{}})}]
    (fixture/with-surfaced-application
      {:operations operations :route-capabilities capabilities
       :rendered-surfaces nil}
      (fn [{:keys [assembly report]}]
        (is (some? assembly)
            (str "Test application did not assemble: " (:errors report)))
        (when assembly
          (f {:source {:application-assembly assembly
                       :semantic-route-handlers
                       (into {} (for [[route-id] routes]
                                  [route-id handler]))
                       :certified-operations
                       {operation {:executor executor
                                   :current-source plan-source}}}
              :handler handler :calls calls}))))))

(defn- reason [thunk]
  (try
    (thunk)
    :not-rejected
    (catch clojure.lang.ExceptionInfo e
      (if (:installed-route-provenance/rejection (ex-data e))
        (:reason (ex-data e))
        [:unexpected-error (ex-data e)]))))

(defn- rejects! [expected thunk]
  (is (= expected (reason thunk))
      (str "Expected checked-ring rejection " expected)))

(defn- request [uri]
  {:request-method :post :uri uri :request-id :v1488})

(use-fixtures
  :once
  (fn [run]
    (with-checked-source
      [[:example/claim :post "/requests/:request-id/claim"]]
      (fn [test-case]
        (binding [*case* (assoc test-case
                                :router (installed/install-checked-ring-router!
                                         (:source test-case)))]
          (run))))))

(deftest bound-parameter-is-derived-only-from-raw-matched-uri
  (let [{:keys [router source calls]} *case*
        original (request "/requests/r-42/claim")
        before (count @calls)
        {:keys [response witness report]}
        (installed/observe-checked-ring-route! router source original)]
    (is (= 200 (:status response)))
    (is (= 1 (- (count @calls) before)))
    (is (= {:request-id "r-42"}
           (:path-params (last @calls))))
    (is (= "/requests/r-42/claim" (:uri (last @calls))))
    (is (not (contains? original :path-params))
        "The caller's Ring request is never modified in place")
    (is (not (identical? original (last @calls)))
        "A dynamic route supplies a separately bound request map")
    (is (= operation (:observed-route-id report)))
    (is (= :one-actual-in-process-invocation (:dispatch report)))
    (is (= :none (:authority report)))
    (is (= :not-established (:physical-correspondence report)))
    (is (= :not-issued (:certification report)))
    (is (= (installed/describe-checked-ring-router router source)
           (installed/require-current-route-dispatch!
            witness router source original)))
    (is (= 1 (- (count @calls) before))
        "Witness revalidation must never execute the handler again")))

(deftest all-supported-unreserved-parameter-characters-match
  (let [{:keys [router calls]} *case*
        values ["R42" "user_name" "a-b" "a.b" "a~b" "a_b-c.9~x"]
        before (count @calls)]
    (doseq [value values]
      (is (= 200 (:status (router (request
                                  (str "/requests/" value "/claim")))))
          value)
      (is (= {:request-id value} (:path-params (last @calls))) value))
    (is (= (count values) (- (count @calls) before)))))

(deftest unmatched-unsafe-and-encoded-parameters-never-dispatch
  (let [{:keys [router source calls]} *case*
        before (count @calls)
        uris ["/requests//claim"
              "/requests/./claim"
              "/requests/../claim"
              "/requests/r%2F42/claim"
              "/requests/r%20x/claim"
              "/requests/r+42/claim"
              "/requests/r:42/claim"
              "/requests/r?42/claim"
              "/requests/r#42/claim"
              "/requests/é/claim"
              "/requests/r/extra/claim"
              "/requests/r/claim/"
              "/requests/r/claim?mode=1"
              "/Requests/r/claim"
              "/requests/:request-id/claim"]]
    (doseq [uri uris]
      (let [req (request uri)]
        (is (= 404 (:status (router req))) uri)
        (rejects! :unmatched-or-invalid-ring-request
                  #(installed/observe-checked-ring-route! router source req))))
    (is (= before (count @calls))
        "Invalid or unsupported raw URI segments never execute an operation")))

(deftest unsupported-ring-request-shapes-fail-closed
  (let [{:keys [router source calls]} *case*
        before (count @calls)]
    (doseq [req [nil [] {} {:uri "/requests/r/claim"}
                 {:request-method "post" :uri "/requests/r/claim"}
                 {:request-method :post :uri nil}
                 {:request-method :post :uri ["/requests/r/claim"]}]]
      (rejects! :invalid-ring-request #(router req))
      (rejects! :unmatched-or-invalid-ring-request
                #(installed/observe-checked-ring-route! router source req)))
    (is (= before (count @calls)))))

(deftest forged-preexisting-parameters-are-not-accepted
  (let [{:keys [router source calls]} *case*
        req (request "/requests/r/claim")
        before (count @calls)]
    (doseq [extra [{:path-params {:request-id "other"}}
                   {:path-params {:request-id "r"}}
                   {:path-params {}}
                   {:path-params nil}
                   {:route-params {:request-id "other"}}
                   {:route-params {}}
                   {:route-params nil}
                   {:path-params {:request-id "r"}
                    :route-params {:request-id "r"}}]]
      (let [altered (merge req extra)]
        (rejects! :preexisting-dynamic-route-parameters
                  #(router altered))
        (rejects! :preexisting-dynamic-route-parameters
                  #(installed/observe-checked-ring-route!
                    router source altered))))
    (is (= before (count @calls)))))

(deftest wrong-method-does-not-match-parameterized-route
  (let [{:keys [router source calls]} *case*
        before (count @calls)
        req (assoc (request "/requests/r/claim") :request-method :get)]
    (is (= 404 (:status (router req))))
    (rejects! :unmatched-or-invalid-ring-request
              #(installed/observe-checked-ring-route! router source req))
    (is (= before (count @calls)))))

(deftest dynamic-witness-is-bound-to-exact-request-object
  (let [{:keys [router source calls]} *case*
        req (request "/requests/42/claim")
        {:keys [witness]} (installed/observe-checked-ring-route!
                            router source req)
        before (count @calls)]
    (is (= (installed/describe-checked-ring-router router source)
           (installed/require-current-route-dispatch! witness router source req)))
    (rejects! :stale-or-forged-route-dispatch-witness
              #(installed/require-current-route-dispatch!
                witness router source (into {} req)))
    (rejects! :stale-or-forged-route-dispatch-witness
              #(installed/require-current-route-dispatch!
                witness router source (request "/requests/43/claim")))
    (rejects! :stale-or-forged-route-dispatch-witness
              #(installed/require-current-route-dispatch!
                (Object.) router source req))
    (rejects! :stale-or-forged-route-dispatch-witness
              #(installed/require-current-route-dispatch!
                (installed/describe-checked-ring-router router source)
                router source req))
    (is (= before (count @calls))
        "Failed witness checks must not invoke any handler")))

(deftest witness-is-bound-to-router-and-source-object-identity
  (let [{:keys [router source calls]} *case*
        req (request "/requests/42/claim")
        {:keys [witness]} (installed/observe-checked-ring-route!
                            router source req)
        other-router (installed/install-checked-ring-router! source)
        before (count @calls)]
    (rejects! :stale-or-forged-route-dispatch-witness
              #(installed/require-current-route-dispatch!
                witness other-router source req))
    (rejects! :changed-ring-router-source-object
              #(installed/require-current-route-dispatch!
                witness router (into {} source) req))
    (rejects! :changed-ring-router-source-object
              #(installed/describe-checked-ring-router
                router (assoc source :untrusted true)))
    (is (= before (count @calls)))))

(deftest source-modification-and-handler-swap-are-rejected
  (let [{:keys [router source calls]} *case*
        request (request "/requests/44/claim")
        before (count @calls)]
    (doseq [candidate [(assoc source :extra :forged)
                       (assoc source :semantic-route-handlers
                              {operation (fn [_] {:status 200})})
                       (assoc source :certified-operations {})
                       (with-meta source {:certified true})]]
      (rejects! :changed-ring-router-source-object
                #(installed/describe-checked-ring-router router candidate))
      (rejects! :changed-ring-router-source-object
                #(installed/observe-checked-ring-route!
                  router candidate request)))
    (is (= before (count @calls)))))

(deftest dynamic-route-report-does-not-upgrade-its-assurance
  (let [{:keys [router source]} *case*
        report (installed/describe-checked-ring-router router source)]
    (is (= installed/report-type (:type report)))
    (is (= installed/report-version (:version report)))
    (is (= 1 (:route-count report)))
    (is (= #{operation} (:route-ids report)))
    (is (installed/checked-ring-router? router))
    (is (= :registered-in-current-jvm (:router-callable report)))
    (is (= :not-established (:host-adoption report)))
    (is (= :not-established (:physical-correspondence report)))
    (is (= :not-issued (:certification report)))
    (is (= :none (:authority report)))
    (is (contains? (:open-obligations report)
                   :dynamic-route-physical-correlation-not-established))
    (is (contains? (:open-obligations report)
                   :route-template-matcher-excludes-wildcards-and-encoded-parameters))
    (is (contains? (:open-obligations report)
                   :host-uri-normalization-and-rewriting-not-authenticated))))

(deftest duplicate-or-invalid-template-parameters-reject-installation
  (doseq [path ["/requests/:id/:id" "/:x/:x" "/requests/:9id/claim"
                "/requests/:id/:id/claim" "/requests/:x:foo/claim"
                "/requests/:id?view=1" "/requests/:id*"
                "/requests/:id/{x}" "/requests/:id//claim"
                "/requests/:id/" "/requests/:id/é"
                "/requests/:id/+claim" "/requests/:id/./claim"
                "/requests/:id/../claim" "/requests/:id/:/claim"]]
    (with-checked-source
      [[operation :post path]]
      (fn [{:keys [source]}]
        (rejects! :unsupported-parameterized-or-invalid-route
                  #(installed/install-checked-ring-router! source))))))

(deftest overlapping-parameter-and-literal-templates-reject-installation
  (doseq [pair [["/requests/:id/claim" "/requests/current/claim"]
                ["/requests/:id/claim" "/requests/:other/claim"]
                ["/teams/:team/requests" "/teams/me/requests"]
                ["/:first/fixed" "/literal/:second"]
                ["/one/:a/two/:b" "/one/fixed/two/:c"]]]
    (with-checked-source
      (mapv vector [:example/claim :example/other] [:post :post] pair)
      (fn [{:keys [source]}]
        (rejects! :ambiguous-ring-route-templates
                  #(installed/install-checked-ring-router! source))))))

(deftest same-template-on-distinct-methods-is-not-ambiguous
  (with-checked-source
    [[:example/claim :post "/requests/:id/claim"]
     [:example/other :get "/requests/:id/claim"]]
    (fn [{:keys [source calls]}]
      (let [router (installed/install-checked-ring-router! source)]
        (is (= 200 (:status (router {:request-method :post
                                    :uri "/requests/one/claim"}))))
        (is (= 200 (:status (router {:request-method :get
                                    :uri "/requests/two/claim"}))))
        (is (= ["one" "two"]
               (mapv #(get-in % [:path-params :id]) @calls)))
        (is (= 2 (:route-count
                  (installed/describe-checked-ring-router router source))))))))

(deftest literal-route-outside-parameter-alphabet-does-not-overlap
  (with-checked-source
    [[:example/claim :post "/requests/:id/claim"]
     [:example/encoded :post "/requests/a+b/claim"]]
    (fn [{:keys [source calls]}]
      (let [router (installed/install-checked-ring-router! source)]
        (is (= 200 (:status (router (request "/requests/a+b/claim")))))
        (is (not (contains? (first @calls) :path-params))
            "Literal route does not invent dynamic bindings")
        (is (= 200 (:status (router (request "/requests/a-b/claim")))))
        (is (= {:id "a-b"} (:path-params (last @calls))))
        (is (= 2 (count @calls)))))))

(deftest disjoint-parameterized-templates-can-coexist
  (with-checked-source
    [[:example/claim :post "/requests/:id/claim"]
     [:example/other :post "/requests/:id/cancel"]]
    (fn [{:keys [source calls]}]
      (let [router (installed/install-checked-ring-router! source)]
        (is (= 200 (:status (router (request "/requests/99/claim")))))
        (is (= 200 (:status (router (request "/requests/88/cancel")))))
        (is (= ["99" "88"]
               (mapv #(get-in % [:path-params :id]) @calls)))
        (is (= 2 (:route-count
                  (installed/describe-checked-ring-router router source))))))))

(deftest multi-parameter-route-binds-all-names-separately
  (with-checked-source
    [[:example/claim :post "/teams/:team-id/requests/:request-id/claim"]]
    (fn [{:keys [source calls]}]
      (let [router (installed/install-checked-ring-router! source)
            req (request "/teams/team-5/requests/request-9/claim")
            {:keys [witness]} (installed/observe-checked-ring-route!
                                router source req)]
        (is (= {:team-id "team-5" :request-id "request-9"}
               (:path-params (last @calls))))
        (is (= (installed/describe-checked-ring-router router source)
               (installed/require-current-route-dispatch!
                witness router source req)))))))
