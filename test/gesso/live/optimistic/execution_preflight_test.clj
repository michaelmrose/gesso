(ns gesso.live.optimistic.execution-preflight-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.identity :as identity]
   [gesso.choreo.preflight :as choreo-preflight]
   [gesso.live.browser.preflight :as browser-preflight]
   [gesso.live.optimistic.capability :as capability]
   [gesso.live.optimistic.choreo :as optimistic-choreo]
   [gesso.live.optimistic.execution-preflight :as execution-preflight]
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

(def trusted-principal
  (identity/principal "helper-1"))

(defn- trusted-operation
  ([operation]
   (trusted-operation operation {}))
  ([operation overrides]
   (server/operation
    (merge
     {:name (keyword (namespace operation)
                     (str (name operation) "-optimistic"))
      :operation operation
      :browser-role :browser
      :authority-role :authority
      :execute!
      (fn [_]
        {:resolution :rejected
         :reason :test-only})}
     overrides))))

(def claim-operation
  (trusted-operation :request/claim))

(def cancel-operation
  (trusted-operation :request/cancel))

(def confirmed-claim-contract
  (server/settlement-contract
   {:confirmed
    {:outcomes #{:request/claimed}
     :commit/status :committed
     :progression :authoritative-basis}}))

(def confirmed-cancel-contract
  (server/settlement-contract
   {:confirmed
    {:outcomes #{:request/cancelled}
     :commit/status :committed
     :progression :authoritative-basis}}))

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
          {:name :example/execution-plans
           :plans plans
           :required-keys (set (keys plans))
           :single-role? true
           :expected-role :browser})]
     (browser-preflight/require-browser-assembly!
      (merge
       {:name :example/execution-browser
        :plan-registry plan-registry
        :browser-role :browser
        :required-plan-keys (set (keys plans))
        :optimistic? true
        :optimistic-htmx? true}
       overrides)))))

(defn- operation-assembly
  ([server-operations]
   (operation-assembly server-operations {}))
  ([server-operations overrides]
   (let [plans
         (or (:plans overrides)
             {:request/claim claim-plan})
         manifest
         (or (:browser-assembly overrides)
             (browser-assembly plans))
         capabilities
         (or (:operation-capabilities overrides)
             (capability/operation-capabilities
              (get-in manifest [:plan-registry :plans])))]
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

(def cancel-route
  (route-preflight/route-capability
   {:operation :request/cancel
    :method :post
    :path "/requests/:request-id/cancel"
    :transports #{:htmx}}))

(defn- route-assembly
  ([server-operations]
   (route-assembly server-operations {}))
  ([server-operations overrides]
   (let [operation-assembly'
         (or (:operation-assembly overrides)
             (operation-assembly server-operations))
         routes
         (or (:route-capabilities overrides)
             {:request/claim claim-route})]
     (route-preflight/require-route-assembly!
      {:name :example/request-routes
       :operation-assembly operation-assembly'
       :route-capabilities routes}))))

(defn- prepared-server
  ([operations]
   (prepared-server operations {}))
  ([operations overrides]
   (server/server
    (merge
     {:principal-fn (fn [_] trusted-principal)
      :operations operations}
     overrides))))

(defn- execution-options
  ([route-assembly' prepared-server']
   (execution-options route-assembly' prepared-server' {}))
  ([route-assembly' prepared-server' overrides]
   (merge
    {:name :example/request-execution
     :route-assembly route-assembly'
     :server prepared-server'}
    overrides)))

(defn- base-fixture
  ([]
   (base-fixture {}))
  ([{:keys [operation-entry server-operations server-options]
     :or {operation-entry claim-operation}}]
   (let [upstream-operations
         (or server-operations
             {:request/claim operation-entry})
         routes
         (route-assembly upstream-operations)
         concrete-server
         (prepared-server upstream-operations server-options)]
     {:route-assembly routes
      :server concrete-server
      :options (execution-options routes concrete-server)})))

;; =============================================================================
;; Successful execution closure
;; =============================================================================

(deftest successful-execution-preflight-closes-route-to-the-concrete-server-boundary
  (let [{:keys [route-assembly server options]} (base-fixture)
        report (execution-preflight/check-execution-assembly options)
        assembly (execution-preflight/require-execution-assembly! options)
        summary (execution-preflight/execution-capabilities assembly)]
    (testing "report exposes the checked execution boundary"
      (is (execution-preflight/report? report))
      (is (execution-preflight/valid? report))
      (is (= [] (:errors report)))
      (is (= [] (:warnings report)))
      (is (= :example/request-execution
             (get-in report [:analysis :name])))
      (is (= :example/request-routes
             (get-in report [:analysis :route-assembly-name])))
      (is (= #{:request/claim}
             (get-in report [:analysis :required-operations])))
      (is (= #{:request/claim}
             (get-in report [:analysis :registered-server-operations])))
      (is (= #{server/authenticated-principal-capability}
             (get-in report [:analysis :supplied-capabilities])))
      (is (= {:request/claim
              #{server/authenticated-principal-capability}}
             (get-in report
                     [:analysis :required-capabilities-by-operation]))))

    (testing "successful assembly retains the exact already-prepared objects"
      (is (execution-preflight/execution-assembly? assembly))
      (is (= :example/request-execution (:name assembly)))
      (is (identical? route-assembly (:route-assembly assembly)))
      (is (identical? server (:server assembly))))

    (testing "capability summary is derived from the concrete prepared server"
      (is (= {:supplied #{server/authenticated-principal-capability}
              :required-by-operation
              {:request/claim
               #{server/authenticated-principal-capability}}}
             summary))))

(deftest application-specific-execution-capabilities-flow-through-the-assembly
  (let [operation
        (trusted-operation
         :request/claim
         {:required-capabilities #{:transaction :clock}})
        routes
        (route-assembly {:request/claim operation})
        concrete-server
        (prepared-server
         {:request/claim operation}
         {:supplied-capabilities #{:transaction :clock :random-seed}})
        assembly
        (execution-preflight/require-execution-assembly!
         (execution-options routes concrete-server))]
    (is (= #{:authenticated-principal
             :transaction
             :clock
             :random-seed}
           (get-in assembly [:execution-capabilities :supplied])))
    (is (= #{:authenticated-principal
             :transaction
             :clock}
           (get-in assembly
                   [:execution-capabilities
                    :required-by-operation
                    :request/claim])))))

(deftest concrete-server-may-be-a-superset-of-route-exposed-operations
  (let [upstream-operations
        {:request/claim claim-operation}
        routes
        (route-assembly upstream-operations)
        concrete-server
        (prepared-server
         {:request/claim claim-operation
          :request/cancel cancel-operation})
        assembly
        (execution-preflight/require-execution-assembly!
         (execution-options routes concrete-server))]
    (is (execution-preflight/execution-assembly? assembly))
    (is (= #{:request/claim :request/cancel}
           (set (keys (get-in assembly [:server :operations])))))
    (is (= #{:request/claim}
           (set (keys
                 (get-in assembly
                         [:execution-capabilities
                          :required-by-operation])))))
        "The derived execution summary covers route-exposed operations, not unrelated server-only operations.")))


(deftest settlement-contracts-are-derived-from-the-prepared-server-boundary
  (let [operation
        (trusted-operation
         :request/claim
         {:settlement-contract confirmed-claim-contract})
        routes
        (route-assembly {:request/claim operation})
        concrete-server
        (prepared-server {:request/claim operation})
        options
        (execution-options routes concrete-server)
        report
        (execution-preflight/check-execution-assembly options)
        assembly
        (execution-preflight/require-execution-assembly! options)
        expected
        {:request/claim confirmed-claim-contract}]
    (is (= expected
           (get-in report [:analysis :settlement-contracts-by-operation])))
    (is (= expected (:settlement-contracts assembly)))
    (is (= expected
           (execution-preflight/settlement-contracts assembly)))
    (is (= expected
           (:settlement-contracts
            (execution-preflight/explain assembly))))
    (is (= 2
           (:gesso.live.optimistic.execution-preflight/version assembly)))
    (is (= 2
           (:gesso.live.optimistic.execution-preflight/version report)))))

(deftest route-exposed-unconstrained-operations-are-explicitly-nil
  (let [claim
        (trusted-operation
         :request/claim
         {:settlement-contract confirmed-claim-contract})
        operations
        {:request/claim claim
         :request/cancel cancel-operation}
        op-assembly
        (operation-assembly
         operations
         {:plans {:request/claim claim-plan
                  :request/cancel cancel-plan}})
        routes
        (route-assembly
         operations
         {:operation-assembly op-assembly
          :route-capabilities
          {:request/claim claim-route
           :request/cancel cancel-route}})
        concrete-server
        (prepared-server operations)
        assembly
        (execution-preflight/require-execution-assembly!
         (execution-options routes concrete-server))
        contracts
        (execution-preflight/settlement-contracts assembly)]
    (is (= #{:request/claim :request/cancel}
           (set (keys contracts))))
    (is (= confirmed-claim-contract
           (:request/claim contracts)))
    (is (contains? contracts :request/cancel))
    (is (nil? (:request/cancel contracts))
        "Nil means this known route-exposed operation intentionally has no declared settlement constraint yet.")))

(deftest server-only-settlement-contracts-do-not-leak-into-the-route-exposed-summary
  (let [claim
        (trusted-operation
         :request/claim
         {:settlement-contract confirmed-claim-contract})
        cancel
        (trusted-operation
         :request/cancel
         {:settlement-contract confirmed-cancel-contract})
        routes
        (route-assembly {:request/claim claim})
        concrete-server
        (prepared-server
         {:request/claim claim
          :request/cancel cancel})
        assembly
        (execution-preflight/require-execution-assembly!
         (execution-options routes concrete-server))
        contracts
        (execution-preflight/settlement-contracts assembly)]
    (is (= {:request/claim confirmed-claim-contract}
           contracts))
    (is (not (contains? contracts :request/cancel))
        "Server-registry supersets remain valid, but unrelated server-only operations are outside this application slice.")))

(deftest capability-closure-does-not-imply-domain-authorization
  (let [operation
        (trusted-operation
         :request/claim
         {:required-capabilities #{:transaction}
          :execute! (fn [_]
                      {:resolution :rejected
                       :reason :not-authorized})})
        routes
        (route-assembly {:request/claim operation})
        concrete-server
        (prepared-server
         {:request/claim operation}
         {:supplied-capabilities #{:transaction}})
        assembly
        (execution-preflight/require-execution-assembly!
         (execution-options routes concrete-server))]
    (is (execution-preflight/execution-assembly? assembly))
    (is (= #{:authenticated-principal :transaction}
           (get-in assembly [:execution-capabilities :supplied])))
    (is (= :not-authorized
           (:reason
            ((get-in assembly [:server :operations :request/claim :execute!])
             {:principal trusted-principal}))))))

;; =============================================================================
;; Boundary failures
;; =============================================================================

(deftest missing-concrete-server-operation-is-rejected-with-closed-diagnostics
  (let [routes
        (route-assembly {:request/claim claim-operation})
        concrete-server
        (prepared-server {:request/cancel cancel-operation})
        report
        (execution-preflight/check-execution-assembly
         (execution-options routes concrete-server))
        issue
        (issue-of-kind report :missing-server-boundary-operation)]
    (is (= #{:missing-server-boundary-operation}
           (error-kinds report)))
    (is (= :request/claim (:operation issue)))
    (is (= #{:request/cancel}
           (:registered-server-operations issue)))))

(deftest substituted-server-operation-identity-is-rejected
  (let [routes
        (route-assembly {:request/claim claim-operation})
        substituted
        (trusted-operation
         :request/claim
         {:name :request/claim-alternate-optimistic})
        concrete-server
        (prepared-server {:request/claim substituted})
        report
        (execution-preflight/check-execution-assembly
         (execution-options routes concrete-server))
        issue
        (issue-of-kind
         report
         :server-boundary-operation-correspondence-mismatch)]
    (is (= #{:server-boundary-operation-correspondence-mismatch}
           (error-kinds report)))
    (is (= :request/claim (:operation issue)))
    (is (= :request/claim-optimistic
           (get-in issue [:expected :choreography-name])))
    (is (= :request/claim-alternate-optimistic
           (get-in issue [:actual :choreography-name])))
    (is (= (get-in issue [:expected :authority-plan-digest])
           (get-in issue [:actual :authority-plan-digest])))
    (is (not= (get-in issue [:expected :choreography-name])
              (get-in issue [:actual :choreography-name]))
        "Static choreography identity remains part of correspondence even when the projected executable bytes happen to be identical.")))

(deftest canonical-but-forged-authority-plan-is-rejected
  (let [routes
        (route-assembly {:request/claim claim-operation})
        forged-operation
        (assoc claim-operation
               :authority-plan
               (:authority-plan cancel-operation))
        concrete-server
        (prepared-server {:request/claim forged-operation})
        report
        (execution-preflight/check-execution-assembly
         (execution-options routes concrete-server))]
    (is (server/operation? forged-operation)
        "The lower server predicate intentionally recognizes canonical plan shape, so execution preflight must retain exact upstream correspondence.")
    (is (= #{:server-boundary-operation-correspondence-mismatch}
           (error-kinds report)))))

(deftest execution-preflight-rejects-an-invalid-capability-open-server-boundary
  (let [operation
        (trusted-operation
         :request/claim
         {:required-capabilities #{:transaction}})
        routes
        (route-assembly {:request/claim operation})
        valid-server
        (prepared-server
         {:request/claim operation}
         {:supplied-capabilities #{:transaction}})
        tampered-server
        (assoc valid-server
               :supplied-capabilities
               #{:authenticated-principal})
        report
        (execution-preflight/check-execution-assembly
         (execution-options routes tampered-server))]
    (is (false? (server/server? tampered-server)))
    (is (= #{:invalid-server-boundary}
           (error-kinds report)))))

(deftest malformed-route-assembly-is-rejected-before-server-correspondence
  (let [{:keys [route-assembly server]} (base-fixture)
        tampered-route-assembly
        (assoc-in route-assembly
                  [:operation-assembly
                   :route-requirements
                   :request/claim
                   :transport]
                  :custom)
        report
        (execution-preflight/check-execution-assembly
         (execution-options tampered-route-assembly server))]
    (is (false? (route-preflight/route-assembly? tampered-route-assembly)))
    (is (= #{:invalid-route-assembly}
           (error-kinds report)))))

(deftest option-validation-fails-before-execution-preflight
  (let [{:keys [options]} (base-fixture)]
    (is (= :invalid-shape
           (error-kind
            #(execution-preflight/check-execution-assembly [:not :a :map]))))
    (is (= :unknown-option-keys
           (error-kind
            #(execution-preflight/check-execution-assembly
              (assoc options :handler 'example/handler)))))
    (is (= :missing-option-key
           (error-kind
            #(execution-preflight/check-execution-assembly
              (dissoc options :server)))))
    (is (= :invalid-name
           (error-kind
            #(execution-preflight/check-execution-assembly
              (assoc options :name "request-execution")))))))

(deftest require-execution-assembly-preserves-structured-report-on-failure
  (let [routes
        (route-assembly {:request/claim claim-operation})
        concrete-server
        (prepared-server {:request/cancel cancel-operation})
        data
        (error-data
         #(execution-preflight/require-execution-assembly!
           (execution-options routes concrete-server)))
        report (:preflight data)]
    (is (= :gesso.live.optimistic.execution-preflight/error
           (:error/type data)))
    (is (= :execution-assembly-preflight-failed
           (:error/kind data)))
    (is (execution-preflight/report? report))
    (is (false? (execution-preflight/valid? report)))
    (is (= #{:missing-server-boundary-operation}
           (error-kinds report)))))

;; =============================================================================
;; Closed product integrity
;; =============================================================================

(deftest execution-assembly-recognition-rejects-prepared-server-operation-tampering
  (let [{:keys [options]} (base-fixture)
        assembly
        (execution-preflight/require-execution-assembly! options)
        missing-operation
        (update-in assembly [:server :operations] dissoc :request/claim)
        substituted-name
        (assoc-in assembly
                  [:server :operations :request/claim :name]
                  :request/claim-alternate-optimistic)
        substituted-authority-plan
        (assoc-in assembly
                  [:server :operations :request/claim :authority-plan]
                  (:authority-plan cancel-operation))]
    (is (execution-preflight/execution-assembly? assembly))
    (is (false? (execution-preflight/execution-assembly? missing-operation)))
    (is (false? (execution-preflight/execution-assembly? substituted-name)))
    (is (false? (execution-preflight/execution-assembly? substituted-authority-plan)))))

(deftest execution-assembly-recognition-rejects-capability-summary-tampering
  (let [{:keys [options]} (base-fixture)
        assembly
        (execution-preflight/require-execution-assembly! options)
        supplied-tampered
        (assoc-in assembly
                  [:execution-capabilities :supplied]
                  #{:authenticated-principal :invented})
        requirements-tampered
        (assoc-in assembly
                  [:execution-capabilities
                   :required-by-operation
                   :request/claim]
                  #{:authenticated-principal :transaction})
        extra-operation
        (assoc-in assembly
                  [:execution-capabilities
                   :required-by-operation
                   :request/cancel]
                  #{:authenticated-principal})]
    (is (false? (execution-preflight/execution-assembly? supplied-tampered)))
    (is (false? (execution-preflight/execution-assembly? requirements-tampered)))
    (is (false? (execution-preflight/execution-assembly? extra-operation)))))


(deftest execution-assembly-recognition-rejects-settlement-contract-summary-tampering
  (let [operation
        (trusted-operation
         :request/claim
         {:settlement-contract confirmed-claim-contract})
        routes
        (route-assembly {:request/claim operation})
        concrete-server
        (prepared-server {:request/claim operation})
        assembly
        (execution-preflight/require-execution-assembly!
         (execution-options routes concrete-server))
        changed-outcome
        (assoc-in assembly
                  [:settlement-contracts
                   :request/claim
                   :confirmed
                   :outcomes]
                  #{:request/cancelled})
        erased-contract
        (assoc-in assembly
                  [:settlement-contracts :request/claim]
                  nil)
        extra-operation
        (assoc-in assembly
                  [:settlement-contracts :request/cancel]
                  nil)]
    (is (execution-preflight/execution-assembly? assembly))
    (is (false? (execution-preflight/execution-assembly? changed-outcome)))
    (is (false? (execution-preflight/execution-assembly? erased-contract)))
    (is (false? (execution-preflight/execution-assembly? extra-operation)))
    (is (= :invalid-execution-assembly
           (error-kind
            #(execution-preflight/settlement-contracts changed-outcome))))))

(deftest execution-assembly-recognition-rejects-upstream-route-tampering
  (let [{:keys [options]} (base-fixture)
        assembly
        (execution-preflight/require-execution-assembly! options)
        tampered
        (assoc-in assembly
                  [:route-assembly
                   :operation-assembly
                   :route-requirements
                   :request/claim
                   :transport]
                  :custom)]
    (is (false? (execution-preflight/execution-assembly? tampered)))
    (is (= :invalid-execution-assembly
           (error-kind
            #(execution-preflight/execution-capabilities tampered))))))

(deftest execution-assembly-recognition-is-closed-over-shape-version-and-name
  (let [{:keys [options]} (base-fixture)
        assembly
        (execution-preflight/require-execution-assembly! options)]
    (is (= 2 execution-preflight/preflight-version))
    (is (= 2
           (:gesso.live.optimistic.execution-preflight/version assembly)))
    (is (contains? assembly :settlement-contracts))
    (is (false?
         (execution-preflight/execution-assembly?
          (dissoc assembly :settlement-contracts))))
    (is (false?
         (execution-preflight/execution-assembly?
          (assoc assembly :unexpected true))))
    (is (false?
         (execution-preflight/execution-assembly?
          (assoc assembly
                 :gesso.live.optimistic.execution-preflight/version
                 (inc execution-preflight/preflight-version)))))
    (is (false?
         (execution-preflight/execution-assembly?
          (assoc assembly :name "request-execution"))))))

(deftest execution-capabilities-accessor-requires-a-current-closed-assembly
  (let [{:keys [options]} (base-fixture)
        assembly
        (execution-preflight/require-execution-assembly! options)]
    (is (= (:execution-capabilities assembly)
           (execution-preflight/execution-capabilities assembly)))
    (is (= :invalid-execution-assembly
           (error-kind
            #(execution-preflight/execution-capabilities
              (assoc assembly
                     :execution-capabilities
                     {:supplied #{}
                      :required-by-operation {}})))))))

(deftest report-recognition-is-closed-and-validity-must-agree-with-errors
  (let [{:keys [options]} (base-fixture)
        report
        (execution-preflight/check-execution-assembly options)]
    (is (execution-preflight/report? report))
    (is (execution-preflight/valid? report))
    (is (false?
         (execution-preflight/report?
          (assoc report :valid? false))))
    (is (false?
         (execution-preflight/report?
          (assoc report :unexpected true))))))

(deftest explain-is-stable-and-rejects-unrecognized-values
  (let [{:keys [options]} (base-fixture)
        report
        (execution-preflight/check-execution-assembly options)
        assembly
        (execution-preflight/require-execution-assembly! options)
        explanation
        (execution-preflight/explain assembly)]
    (is (= execution-preflight/report-type
           (:type (execution-preflight/explain report))))
    (is (= execution-preflight/execution-assembly-type
           (:type explanation)))
    (is (= :example/request-execution (:name explanation)))
    (is (= :example/request-routes (:route-assembly-name explanation)))
    (is (= #{:request/claim} (:operations explanation)))
    (is (= (:execution-capabilities assembly)
           (:execution-capabilities explanation)))
    (is (= {:request/claim nil}
           (:settlement-contracts assembly)))
    (is (= (:settlement-contracts assembly)
           (:settlement-contracts explanation)))
    (is (= :unrecognized-value
           (error-kind
            #(execution-preflight/explain {:nope true}))))))
