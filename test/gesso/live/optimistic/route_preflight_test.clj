(ns gesso.live.optimistic.route-preflight-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.preflight :as choreo-preflight]
   [gesso.live.browser.preflight :as browser-preflight]
   [gesso.live.optimistic.capability :as capability]
   [gesso.live.optimistic.choreo :as optimistic-choreo]
   [gesso.live.optimistic.preflight :as operation-preflight]
   [gesso.live.optimistic.route-preflight :as route-preflight]
   [gesso.live.optimistic.server :as server]))

;; =============================================================================
;; Fixtures / helpers
;; =============================================================================

(defn- error-data
  [f]
  (try
    (f)
    nil
    (catch Throwable error
      (ex-data error))))

(defn- error-kind
  [f]
  (:error/kind (error-data f)))

(defn- error-kinds
  [report]
  (set (map :kind (:errors report))))

(defn- issue-of-kind
  [report kind]
  (first (filter #(= kind (:kind %)) (:errors report))))

(defn- trusted-operation
  [operation]
  (server/operation
   {:name (keyword (namespace operation)
                   (str (name operation) "-optimistic"))
    :operation operation
    :browser-role :browser
    :authority-role :authority
    :execute!
    (fn [_]
      {:resolution :rejected
       :reason :test-only})}))

(def claim-operation
  (trusted-operation :request/claim))

(def cancel-operation
  (trusted-operation :request/cancel))

(def claim-plan
  (optimistic-choreo/command-plan
   {:name (:name claim-operation)
    :operation (:operation claim-operation)
    :browser-role (:browser-role claim-operation)
    :authority-role (:authority-role claim-operation)}
   :browser))

(def cancel-plan
  (optimistic-choreo/command-plan
   {:name (:name cancel-operation)
    :operation (:operation cancel-operation)
    :browser-role (:browser-role cancel-operation)
    :authority-role (:authority-role cancel-operation)}
   :browser))

(defn- browser-assembly
  ([plans]
   (browser-assembly plans {}))
  ([plans overrides]
   (let [plan-registry
         (choreo-preflight/require-plan-registry!
          {:name :example/route-plans
           :plans plans
           :required-keys (set (keys plans))
           :single-role? true
           :expected-role :browser})]
     (browser-preflight/require-browser-assembly!
      (merge
       {:name :example/route-browser
        :plan-registry plan-registry
        :browser-role :browser
        :required-plan-keys (set (keys plans))
        :optimistic? true
        :optimistic-htmx? true}
       overrides)))))

(defn- operation-assembly
  ([]
   (operation-assembly {}))
  ([overrides]
   (let [manifest
         (or (:browser-assembly overrides)
             (browser-assembly {:request/claim claim-plan}))

         capabilities
         (or (:operation-capabilities overrides)
             (capability/operation-capabilities
              (get-in manifest [:plan-registry :plans])))

         server-operations
         (or (:server-operations overrides)
             {:request/claim claim-operation})]
     (operation-preflight/require-operation-assembly!
      {:name :example/request-operations
       :browser-assembly manifest
       :operation-capabilities capabilities
       :server-operations server-operations}))))

(def claim-route
  (route-preflight/route-capability
   {:operation :request/claim
    :method :post
    :path "/requests/:request-id/claim"
    :transports #{:htmx}}))

(defn- claim-route-registry
  ([]
   {:request/claim claim-route})
  ([route]
   {:request/claim route}))

(defn- route-options
  ([]
   (route-options {}))
  ([overrides]
   (merge
    {:name :example/request-routes
     :operation-assembly (operation-assembly)
     :route-capabilities (claim-route-registry)}
    overrides)))

;; =============================================================================
;; Route capability declarations
;; =============================================================================

(deftest route-capability-constructor-is-closed-and-canonical
  (is (route-preflight/route-capability? claim-route))
  (is (= :request/claim (:operation claim-route)))
  (is (= :post (:method claim-route)))
  (is (= "/requests/:request-id/claim" (:path claim-route)))
  (is (= #{:htmx} (:transports claim-route)))
  (is (= :missing-route-capability-key
         (error-kind
          #(route-preflight/route-capability
            {:operation :request/claim
             :method :post
             :path "/claim"}))))
  (is (= :unknown-route-capability-key
         (error-kind
          #(route-preflight/route-capability
            {:operation :request/claim
             :method :post
             :path "/claim"
             :transports #{:htmx}
             :handler 'example/claim})))))
  (is (= :invalid-route-operation
         (error-kind
          #(route-preflight/route-capability
            {:operation "claim"
             :method :post
             :path "/claim"
             :transports #{:htmx}}))))
  (is (= :invalid-route-method
         (error-kind
          #(route-preflight/route-capability
            {:operation :request/claim
             :method "POST"
             :path "/claim"
             :transports #{:htmx}}))))
  (is (= :invalid-route-path
         (error-kind
          #(route-preflight/route-capability
            {:operation :request/claim
             :method :post
             :path "   "
             :transports #{:htmx}}))))
  (is (= :invalid-route-transports
         (error-kind
          #(route-preflight/route-capability
            {:operation :request/claim
             :method :post
             :path "/claim"
             :transports #{:none}}))))

(deftest route-capability-registry-is-closed-but-may-be-an-application-superset
  (let [registry
        (route-preflight/route-capabilities
         {:request/claim
          {:operation :request/claim
           :method :post
           :path "/claim"
           :transports #{:htmx}}

          :health/check
          {:operation :health/check
           :method :get
           :path "/health"
           :transports #{:custom}}})]
    (is (route-preflight/route-capabilities? registry))
    (is (= #{:request/claim :health/check}
           (set (keys registry))))
    (is (= :empty-route-capabilities
           (error-kind #(route-preflight/route-capabilities {}))))
    (is (= :invalid-route-id
           (error-kind
            #(route-preflight/route-capabilities
              {"claim"
               {:operation :request/claim
                :method :post
                :path "/claim"
                :transports #{:htmx}}}))))))

;; =============================================================================
;; Successful route closure
;; =============================================================================

(deftest successful-route-preflight-closes-derived-transport-to-trusted-route
  (let [report
        (route-preflight/check-route-assembly (route-options))

        assembly
        (route-preflight/require-route-assembly! (route-options))

        realization
        (get-in assembly [:routes :request/claim])]
    (testing "report exposes the physical route facts and derived requirements"
      (is (route-preflight/report? report))
      (is (route-preflight/valid? report))
      (is (= [] (:errors report)))
      (is (= [] (:warnings report)))
      (is (= :example/request-routes
             (get-in report [:analysis :name])))
      (is (= :example/request-operations
             (get-in report [:analysis :operation-assembly-name])))
      (is (= #{:request/claim}
             (get-in report [:analysis :required-operations])))
      (is (= {:request/claim :htmx}
             (get-in report [:analysis :required-transports])))
      (is (= #{:request/claim}
             (get-in report [:analysis :declared-route-ids]))))

    (testing "successful product records the selected trusted physical realization"
      (is (route-preflight/route-assembly? assembly))
      (is (= :example/request-routes (:name assembly)))
      (is (= :request/claim (:route-id realization)))
      (is (= :request/claim (:operation realization)))
      (is (= :post (:method realization)))
      (is (= "/requests/:request-id/claim" (:path realization)))
      (is (= #{:htmx} (:accepted-transports realization)))
      (is (= :htmx (:required-transport realization))))

    (testing "operation lookup returns only closed realizations"
      (is (= [realization]
             (route-preflight/routes-for-operation
              assembly
              :request/claim)))
      (is (= []
             (route-preflight/routes-for-operation
              assembly
              :request/cancel))))))

(deftest route-registry-may-be-a-superset-and-only-relevant-routes-enter-the-assembly
  (let [routes
        (route-preflight/route-capabilities
         {:request/claim
          {:operation :request/claim
           :method :post
           :path "/claim"
           :transports #{:htmx}}

          :admin/rebuild
          {:operation :admin/rebuild
           :method :post
           :path "/admin/rebuild"
           :transports #{:custom}}})

        assembly
        (route-preflight/require-route-assembly!
         (route-options {:route-capabilities routes}))]
    (is (route-preflight/route-assembly? assembly))
    (is (= #{:request/claim}
           (set (keys (:routes assembly)))))
    (is (= #{:request/claim}
           (:operations (route-preflight/explain assembly))))))

(deftest multiple-compatible-physical-routes-may-realize-one-operation
  (let [routes
        (route-preflight/route-capabilities
         {:request/claim-primary
          {:operation :request/claim
           :method :post
           :path "/requests/:request-id/claim"
           :transports #{:htmx}}

          :request/claim-alias
          {:operation :request/claim
           :method :post
           :path "/legacy/requests/:request-id/claim"
           :transports #{:htmx :custom}}})

        assembly
        (route-preflight/require-route-assembly!
         (route-options {:route-capabilities routes}))]
    (is (route-preflight/route-assembly? assembly))
    (is (= #{:request/claim-primary :request/claim-alias}
           (set (keys (:routes assembly)))))
    (is (= 2
           (count
            (route-preflight/routes-for-operation
             assembly
             :request/claim))))))

(deftest custom-command-transport-is-derived-from-operation-assembly
  (let [manifest
        (browser-assembly
         {:request/claim claim-plan}
         {:optimistic? true
          :optimistic-htmx? false
          :optimistic-command-transport :custom})

        operation-assembly'
        (operation-assembly {:browser-assembly manifest})

        route
        (route-preflight/route-capability
         {:operation :request/claim
          :method :post
          :path "/claim"
          :transports #{:custom}})

        assembly
        (route-preflight/require-route-assembly!
         (route-options
          {:operation-assembly operation-assembly'
           :route-capabilities {:request/claim route}}))]
    (is (= :custom
           (get-in assembly
                   [:routes :request/claim :required-transport])))
    (is (= #{:custom}
           (get-in assembly
                   [:routes :request/claim :accepted-transports])))))

;; =============================================================================
;; Closure failures
;; =============================================================================

(deftest missing-route-is-distinguished-from-an-incompatible-route
  (let [missing-report
        (route-preflight/check-route-assembly
         (route-options
          {:route-capabilities
           (route-preflight/route-capabilities
            {:request/cancel
             {:operation :request/cancel
              :method :post
              :path "/cancel"
              :transports #{:htmx}}})}))

        missing-issue
        (issue-of-kind missing-report :missing-trusted-route)

        incompatible-report
        (route-preflight/check-route-assembly
         (route-options
          {:route-capabilities
           (claim-route-registry
            (route-preflight/route-capability
             {:operation :request/claim
              :method :post
              :path "/claim"
              :transports #{:custom}}))}))

        incompatible-issue
        (issue-of-kind
         incompatible-report
         :incompatible-route-transport)]
    (testing "no route for the semantic operation"
      (is (= #{:missing-trusted-route}
             (error-kinds missing-report)))
      (is (= :request/claim (:operation missing-issue)))
      (is (= :htmx (:required-transport missing-issue)))
      (is (= #{:request/cancel}
             (:available-route-operations missing-issue))))

    (testing "a route exists but does not accept the derived transport"
      (is (= #{:incompatible-route-transport}
             (error-kinds incompatible-report)))
      (is (= :request/claim (:operation incompatible-issue)))
      (is (= :htmx (:required-transport incompatible-issue)))
      (is (= #{:custom}
             (get-in incompatible-issue
                     [:route-capabilities :request/claim :transports]))))))

(deftest duplicate-physical-method-path-endpoints-are-rejected
  (let [routes
        (route-preflight/route-capabilities
         {:request/claim-a
          {:operation :request/claim
           :method :post
           :path "/request/action"
           :transports #{:htmx}}

          :request/claim-b
          {:operation :request/claim
           :method :post
           :path "/request/action"
           :transports #{:htmx}}})

        report
        (route-preflight/check-route-assembly
         (route-options {:route-capabilities routes}))

        issue
        (issue-of-kind report :duplicate-route-endpoint)]
    (is (contains? (error-kinds report) :duplicate-route-endpoint))
    (is (= [[:post "/request/action"]]
           (mapv :endpoint (:duplicates issue))))
    (is (= #{:request/claim-a :request/claim-b}
           (get-in issue [:duplicates 0 :route-ids])))))

(deftest malformed-upstream-operation-assembly-is-rejected-before-route-closure
  (let [good (operation-assembly)
        tampered
        (assoc-in good
                  [:route-requirements :request/claim :transport]
                  :custom)
        report
        (route-preflight/check-route-assembly
         (route-options {:operation-assembly tampered}))]
    (is (false? (operation-preflight/operation-assembly? tampered)))
    (is (= #{:invalid-operation-assembly}
           (error-kinds report)))))

(deftest malformed-route-capability-registry-is-rejected
  (let [report
        (route-preflight/check-route-assembly
         (route-options
          {:route-capabilities
           {:request/claim
            {:operation :request/claim
             :method :post
             :path "/claim"
             :transports #{:htmx}}}}))]
    (is (= #{:invalid-route-capabilities}
           (error-kinds report)))))

(deftest option-validation-fails-before-route-preflight
  (is (= :invalid-shape
         (error-kind
          #(route-preflight/check-route-assembly [:not :a :map]))))
  (is (= :unknown-option-keys
         (error-kind
          #(route-preflight/check-route-assembly
            (assoc (route-options) :handler 'example/handler)))))
  (is (= :missing-option-key
         (error-kind
          #(route-preflight/check-route-assembly
            (dissoc (route-options) :route-capabilities)))))
  (is (= :invalid-name
         (error-kind
          #(route-preflight/check-route-assembly
            (assoc (route-options) :name "request-routes"))))))

(deftest require-route-assembly-preserves-structured-report-on-failure
  (let [data
        (error-data
         #(route-preflight/require-route-assembly!
           (route-options
            {:route-capabilities
             (route-preflight/route-capabilities
              {:request/cancel
               {:operation :request/cancel
                :method :post
                :path "/cancel"
                :transports #{:htmx}}})})))
        report (:preflight data)]
    (is (= :gesso.live.optimistic.route-preflight/error
           (:error/type data)))
    (is (= :route-assembly-preflight-failed
           (:error/kind data)))
    (is (route-preflight/report? report))
    (is (false? (route-preflight/valid? report)))
    (is (= #{:missing-trusted-route}
           (error-kinds report)))))

;; =============================================================================
;; Closed product integrity / honest physical boundary
;; =============================================================================

(deftest route-assembly-recognition-rejects-derived-operation-and-transport-tampering
  (let [assembly
        (route-preflight/require-route-assembly! (route-options))

        relabeled
        (assoc-in assembly
                  [:routes :request/claim :operation]
                  :request/cancel)

        transport-tampered
        (assoc-in assembly
                  [:routes :request/claim :required-transport]
                  :custom)

        accepted-transport-tampered
        (assoc-in assembly
                  [:routes :request/claim :accepted-transports]
                  #{:custom})]
    (is (route-preflight/route-assembly? assembly))
    (is (false? (route-preflight/route-assembly? relabeled)))
    (is (false? (route-preflight/route-assembly? transport-tampered)))
    (is (false? (route-preflight/route-assembly? accepted-transport-tampered)))))

(deftest route-assembly-recognition-rejects-route-id-shape-and-duplicate-endpoint-tampering
  (let [assembly
        (route-preflight/require-route-assembly! (route-options))
        realization
        (get-in assembly [:routes :request/claim])

        wrong-route-id
        (assoc assembly
               :routes
               {:request/claim-copy realization})

        duplicate
        (assoc assembly
               :routes
               {:request/claim realization
                :request/claim-copy
                (assoc realization
                       :route-id :request/claim-copy)})]
    (is (false? (route-preflight/route-assembly? wrong-route-id)))
    (is (false? (route-preflight/route-assembly? duplicate)))))

(deftest physical-method-and-path-remain-explicit-trusted-assembly-facts
  (let [assembly
        (route-preflight/require-route-assembly! (route-options))

        changed-physical-coordinate
        (-> assembly
            (assoc-in [:routes :request/claim :method] :put)
            (assoc-in [:routes :request/claim :path]
                      "/different/application-owned/path"))]
    (testing "route assembly self-recognition checks closure, not arbitrary handler truth"
      (is (route-preflight/route-assembly? changed-physical-coordinate)))

    (testing "the explanation therefore exposes the declared physical facts for inspection"
      (is (= :put
             (get-in (route-preflight/explain changed-physical-coordinate)
                     [:routes :request/claim :method])))
      (is (= "/different/application-owned/path"
             (get-in (route-preflight/explain changed-physical-coordinate)
                     [:routes :request/claim :path]))))))

(deftest route-assembly-recognition-rejects-upstream-operation-assembly-tampering
  (let [assembly
        (route-preflight/require-route-assembly! (route-options))
        tampered
        (assoc-in assembly
                  [:operation-assembly
                   :route-requirements
                   :request/claim
                   :transport]
                  :custom)]
    (is (false? (route-preflight/route-assembly? tampered)))
    (is (= :invalid-route-assembly
           (error-kind
            #(route-preflight/routes-for-operation
              tampered
              :request/claim))))))

(deftest report-recognition-is-closed-and-validity-must-agree-with-errors
  (let [report
        (route-preflight/check-route-assembly (route-options))]
    (is (route-preflight/report? report))
    (is (route-preflight/valid? report))
    (is (false?
         (route-preflight/report?
          (assoc report :valid? false))))
    (is (false?
         (route-preflight/report?
          (assoc report :unexpected true))))))

(deftest explain-is-stable-and-rejects-unrecognized-values
  (let [report
        (route-preflight/check-route-assembly (route-options))
        assembly
        (route-preflight/require-route-assembly! (route-options))]
    (is (= route-preflight/report-type
           (:type (route-preflight/explain report))))
    (is (= route-preflight/route-assembly-type
           (:type (route-preflight/explain assembly))))
    (is (= #{:request/claim}
           (:operations (route-preflight/explain assembly))))
    (is (= :not-explainable
           (error-kind #(route-preflight/explain {:nope true}))))))
