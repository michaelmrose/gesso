(ns gesso.live.optimistic.preflight-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.artifact :as choreo-artifact]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.preflight :as choreo-preflight]
   [gesso.choreo.project :as project]
   [gesso.live.browser.preflight :as browser-preflight]
   [gesso.live.optimistic.capability :as capability]
   [gesso.live.optimistic.choreo :as optimistic-choreo]
   [gesso.live.optimistic.preflight :as preflight]
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

(defn- operation-name
  [operation]
  (keyword
   (namespace operation)
   (str (name operation) "-optimistic")))

(defn- operation-options
  ([operation]
   (operation-options operation {}))
  ([operation overrides]
   (merge
    {:name (operation-name operation)
     :operation operation
     :browser-role :browser
     :authority-role :authority}
    overrides)))

(defn- trusted-operation
  ([operation]
   (trusted-operation operation {}))
  ([operation overrides]
   (server/operation
    (assoc
     (operation-options operation overrides)
     :execute!
     (fn [_]
       {:resolution :rejected
        :reason :test-only})))))

(defn- expected-browser-plan
  [trusted-operation]
  (optimistic-choreo/command-plan
   {:name (:name trusted-operation)
    :operation (:operation trusted-operation)
    :browser-role (:browser-role trusted-operation)
    :authority-role (:authority-role trusted-operation)}
   (:browser-role trusted-operation)))

(defn- unrelated-browser-plan
  []
  (project/project
   (choreo/->choreography
    {:name :example/unrelated-browser-plan
     :initial :prepare
     :states
     {:prepare
      (choreo/local :browser :example/prepare :done)

      :done
      (choreo/return :done)}})
   :browser))

(defn- browser-assembly
  ([plans]
   (browser-assembly plans {}))
  ([plans overrides]
   (let [browser-role
         (or (:browser-role overrides) :browser)

         plan-registry
         (choreo-preflight/require-plan-registry!
          {:name :example/optimistic-operation-plans
           :plans plans
           :required-keys (set (keys plans))
           :single-role? true
           :expected-role browser-role})]
     (browser-preflight/require-browser-assembly!
      (merge
       {:name :example/optimistic-operation-browser
        :plan-registry plan-registry
        :browser-role browser-role
        :required-plan-keys (set (keys plans))
        :optimistic? true
        :optimistic-htmx? true}
       (dissoc overrides :browser-role))))))

(defn- plain-browser-assembly
  [plans]
  (let [plan-registry
        (choreo-preflight/require-plan-registry!
         {:plans plans
          :required-keys (set (keys plans))
          :single-role? true
          :expected-role :browser})]
    (browser-preflight/require-browser-assembly!
     {:name :example/plain-browser
      :plan-registry plan-registry
      :browser-role :browser
      :required-plan-keys (set (keys plans))})))

(def claim-operation
  (trusted-operation :request/claim))

(def cancel-operation
  (trusted-operation :request/cancel))

(def claim-plan
  (expected-browser-plan claim-operation))

(def cancel-plan
  (expected-browser-plan cancel-operation))

(def claim-browser-assembly
  (browser-assembly {:request/claim claim-plan}))

(def claim-capabilities
  (capability/operation-capabilities
   {:request/claim claim-plan}))

(defn- claim-options
  ([]
   (claim-options {}))
  ([overrides]
   (merge
    {:name :example/request-operations
     :browser-assembly claim-browser-assembly
     :operation-capabilities claim-capabilities
     :server-operations {:request/claim claim-operation}}
    overrides)))

;; =============================================================================
;; Successful closure
;; =============================================================================

(deftest successful-operation-preflight-closes-browser-and-trusted-server-realization
  (let [report
        (preflight/check-operation-assembly (claim-options))

        assembly
        (preflight/require-operation-assembly! (claim-options))

        realization
        (get-in assembly [:operations :request/claim])

        route-requirement
        (get-in assembly [:route-requirements :request/claim])]
    (testing "the report exposes the exact checked application slice"
      (is (preflight/report? report))
      (is (preflight/valid? report))
      (is (= [] (:errors report)))
      (is (= [] (:warnings report)))
      (is (= {:name :example/request-operations
              :browser-assembly-name :example/optimistic-operation-browser
              :browser-role :browser
              :command-transport :htmx
              :browser-operations #{:request/claim}
              :registered-server-operations #{:request/claim}}
             (:analysis report))))

    (testing "the successful product records exact browser and authority correspondence"
      (is (preflight/operation-assembly? assembly))
      (is (= :example/request-operations (:name assembly)))
      (is (= claim-browser-assembly (:browser-assembly assembly)))
      (is (= #{:request/claim} (set (keys (:operations assembly)))))
      (is (= :request/claim (:operation realization)))
      (is (= :request/claim (:plan-key realization)))
      (is (= (:name claim-operation) (:choreography-name realization)))
      (is (= :browser (:browser-role realization)))
      (is (= :authority (:authority-role realization)))
      (is (= (get-in claim-browser-assembly
                     [:plan-registry :digests :request/claim])
             (:browser-plan-digest realization)))
      (is (= (choreo-artifact/executable-digest
              (:authority-plan claim-operation))
             (:authority-plan-digest realization)))
      (is (= :htmx (:command-transport realization))))

    (testing "route transport is derived as a downstream obligation, not caller data"
      (is (preflight/route-requirement? route-requirement))
      (is (= :request/claim (:operation route-requirement)))
      (is (= :htmx (:transport route-requirement))))))

(deftest server-registry-may-be-a-trusted-superset-of-browser-exposed-operations
  (let [assembly
        (preflight/require-operation-assembly!
         (claim-options
          {:server-operations
           {:request/claim claim-operation
            :request/cancel cancel-operation}}))]
    (is (preflight/operation-assembly? assembly))
    (is (= #{:request/claim}
           (set (keys (:operations assembly)))))
    (is (= #{:request/claim}
           (set (keys (:route-requirements assembly)))))
    (is (= #{:request/claim}
           (:operations (preflight/explain assembly))))))

(deftest explicit-semantic-operation-to-plan-key-mapping-is-supported
  (let [plan-key :browser/request-claim
        manifest (browser-assembly {plan-key claim-plan})
        capabilities
        {:request/claim
         (capability/operation-capability
          {:operation :request/claim
           :plan-key plan-key})}
        assembly
        (preflight/require-operation-assembly!
         {:name :example/explicit-plan-key
          :browser-assembly manifest
          :operation-capabilities capabilities
          :server-operations {:request/claim claim-operation}})]
    (is (preflight/operation-assembly? assembly))
    (is (= plan-key
           (get-in assembly
                   [:operations :request/claim :plan-key])))
    (is (= :request/claim
           (get-in assembly
                   [:route-requirements :request/claim :operation])))))

(deftest route-requirement-transport-is-derived-from-browser-assembly
  (let [manifest
        (browser-assembly
         {:request/claim claim-plan}
         {:optimistic? true
          :optimistic-htmx? false
          :optimistic-command-transport :custom})

        assembly
        (preflight/require-operation-assembly!
         (claim-options {:browser-assembly manifest}))]
    (is (= :custom
           (get-in assembly
                   [:operations :request/claim :command-transport])))
    (is (= :custom
           (get-in assembly
                   [:route-requirements :request/claim :transport])))
    (is (preflight/route-requirement?
         (get-in assembly
                 [:route-requirements :request/claim])))))

;; =============================================================================
;; Browser/server correspondence failures
;; =============================================================================

(deftest browser-exposed-operation-must-have-a-trusted-server-operation
  (let [report
        (preflight/check-operation-assembly
         (claim-options
          {:server-operations
           {:request/cancel cancel-operation}}))
        issue
        (issue-of-kind report :missing-trusted-server-operation)]
    (is (false? (preflight/valid? report)))
    (is (= #{:missing-trusted-server-operation}
           (error-kinds report)))
    (is (= :request/claim (:operation issue)))
    (is (= #{:request/cancel}
           (:registered-server-operations issue)))))

(deftest trusted-server-authority-plan-must-match-its-own-static-choreography-identity
  (let [forged-operation
        (assoc claim-operation
               :authority-plan (:authority-plan cancel-operation))

        report
        (preflight/check-operation-assembly
         (claim-options
          {:server-operations
           {:request/claim forged-operation}}))

        issue
        (issue-of-kind
         report
         :server-authority-plan-correspondence-mismatch)]
    (testing "the lower server predicate alone cannot establish this correspondence"
      (is (server/operation? forged-operation)))

    (testing "operation preflight rejects the forged canonical authority plan"
      (is (false? (preflight/valid? report)))
      (is (= #{:server-authority-plan-correspondence-mismatch}
             (error-kinds report)))
      (is (= :request/claim (:operation issue)))
      (is (= (:name claim-operation)
             (:choreography-name issue)))
      (is (= :authority (:authority-role issue))))))

(deftest browser-plan-must-match-the-projection-implied-by-the-trusted-server-operation
  (let [unrelated-plan (unrelated-browser-plan)
        manifest (browser-assembly {:request/claim unrelated-plan})
        capabilities
        (capability/operation-capabilities
         {:request/claim unrelated-plan})
        report
        (preflight/check-operation-assembly
         {:browser-assembly manifest
          :operation-capabilities capabilities
          :server-operations {:request/claim claim-operation}})
        issue
        (issue-of-kind
         report
         :browser-server-plan-correspondence-mismatch)]
    (is (false? (preflight/valid? report)))
    (is (= #{:browser-server-plan-correspondence-mismatch}
           (error-kinds report)))
    (is (= :request/claim (:operation issue)))
    (is (= :request/claim (:plan-key issue)))
    (is (= (:name claim-operation)
           (:choreography-name issue)))
    (is (= :browser (:browser-role issue)))))

(deftest trusted-server-browser-role-must-match-the-browser-assembly-role
  (let [other-browser-operation
        (trusted-operation
         :request/claim
         {:browser-role :other-browser})
        report
        (preflight/check-operation-assembly
         (claim-options
          {:server-operations
           {:request/claim other-browser-operation}}))
        issue
        (issue-of-kind
         report
         :server-operation-browser-role-mismatch)]
    (is (false? (preflight/valid? report)))
    (is (= #{:server-operation-browser-role-mismatch}
           (error-kinds report)))
    (is (= :request/claim (:operation issue)))
    (is (= :browser (:assembly-browser-role issue)))
    (is (= :other-browser
           (:server-operation-browser-role issue)))))

;; =============================================================================
;; Invalid assembly inputs
;; =============================================================================

(deftest malformed-browser-capability-and-server-registries-fail-closed
  (testing "tampered browser manifest is rejected before correspondence work"
    (let [report
          (preflight/check-operation-assembly
           (claim-options
            {:browser-assembly
             (assoc claim-browser-assembly :forged true)}))]
      (is (= #{:invalid-browser-assembly}
             (error-kinds report)))))

  (testing "capability registry must be non-empty and canonical"
    (let [report
          (preflight/check-operation-assembly
           (claim-options {:operation-capabilities {}}))]
      (is (= #{:invalid-operation-capabilities}
             (error-kinds report)))))

  (testing "trusted server registry must be non-empty and key-aligned"
    (let [report
          (preflight/check-operation-assembly
           (claim-options
            {:server-operations
             {:request/claim cancel-operation}}))]
      (is (= #{:invalid-server-operations}
             (error-kinds report))))))

(deftest a-browser-assembly-without-optimism-is-not-an-optimistic-operation-assembly
  (let [manifest
        (plain-browser-assembly {:request/claim claim-plan})
        report
        (preflight/check-operation-assembly
         (claim-options {:browser-assembly manifest}))]
    (is (browser-preflight/assembly-manifest? manifest))
    (is (= :none (:optimistic-command-transport manifest)))
    (is (= #{:browser-assembly-without-optimism
             :browser-assembly-without-command-transport}
           (error-kinds report)))))

(deftest option-validation-fails-before-preflight
  (is (= :invalid-shape
         (error-kind
          #(preflight/check-operation-assembly [:not :a :map]))))
  (is (= :unknown-option-keys
         (error-kind
          #(preflight/check-operation-assembly
            (assoc (claim-options) :route "/request/claim")))))
  (is (= :missing-option-key
         (error-kind
          #(preflight/check-operation-assembly
            (dissoc (claim-options) :server-operations)))))
  (is (= :invalid-name
         (error-kind
          #(preflight/check-operation-assembly
            (assoc (claim-options) :name "request-operations"))))))

(deftest require-operation-assembly-preserves-the-structured-report-on-failure
  (let [data
        (error-data
         #(preflight/require-operation-assembly!
           (claim-options
            {:server-operations
             {:request/cancel cancel-operation}})))
        report (:preflight data)]
    (is (= :gesso.live.optimistic.preflight/error
           (:error/type data)))
    (is (= :operation-assembly-preflight-failed
           (:error/kind data)))
    (is (preflight/report? report))
    (is (false? (preflight/valid? report)))
    (is (= #{:missing-trusted-server-operation}
           (error-kinds report)))))

;; =============================================================================
;; Closed product / tamper resistance
;; =============================================================================

(deftest operation-assembly-recognition-rejects-semantic-relabeling
  (let [assembly
        (preflight/require-operation-assembly! (claim-options))
        realization
        (get-in assembly [:operations :request/claim])
        requirement
        (get-in assembly [:route-requirements :request/claim])
        relabeled
        (assoc assembly
               :operations
               {:request/cancel
                (assoc realization
                       :operation :request/cancel)}
               :route-requirements
               {:request/cancel
                (assoc requirement
                       :operation :request/cancel)})]
    (is (preflight/operation-assembly? assembly))
    (is (false? (preflight/operation-assembly? relabeled)))
    (is (= (:browser-plan-digest realization)
           (get-in relabeled
                   [:operations :request/cancel :browser-plan-digest])))
    (is (= (:authority-plan-digest realization)
           (get-in relabeled
                   [:operations :request/cancel :authority-plan-digest])))))

(deftest operation-assembly-recognition-rechecks-digests-roles-routes-and-closed-shape
  (let [assembly
        (preflight/require-operation-assembly! (claim-options))]
    (doseq [[label tampered]
            [[:browser-digest
              (assoc-in assembly
                        [:operations :request/claim :browser-plan-digest]
                        "forged")]

             [:authority-digest
              (assoc-in assembly
                        [:operations :request/claim :authority-plan-digest]
                        "forged")]

             [:browser-role
              (assoc-in assembly
                        [:operations :request/claim :browser-role]
                        :other-browser)]

             [:colliding-authority-role
              (assoc-in assembly
                        [:operations :request/claim :authority-role]
                        :browser)]

             [:route-transport
              (assoc-in assembly
                        [:route-requirements :request/claim :transport]
                        :custom)]

             [:route-operation
              (assoc-in assembly
                        [:route-requirements :request/claim :operation]
                        :request/cancel)]

             [:extra-assembly-field
              (assoc assembly :trusted true)]

             [:missing-operation-field
              (update-in assembly
                         [:operations :request/claim]
                         dissoc
                         :authority-plan-digest)]]]
      (testing (name label)
        (is (false? (preflight/operation-assembly? tampered)))))))

(deftest route-requirement-recognition-is-closed-and-current
  (let [assembly
        (preflight/require-operation-assembly! (claim-options))
        requirement
        (get-in assembly [:route-requirements :request/claim])]
    (is (preflight/route-requirement? requirement))
    (doseq [tampered
            [(assoc requirement :transport :none)
             (assoc requirement :operation "request/claim")
             (assoc requirement
                    :gesso.live.optimistic.preflight/version
                    (inc preflight/preflight-version))
             (assoc requirement :trusted true)
             (dissoc requirement :transport)]]
      (is (false? (preflight/route-requirement? tampered))))))

(deftest report-recognition-is-closed-and-validity-must-agree-with-errors
  (let [report
        (preflight/check-operation-assembly (claim-options))]
    (is (preflight/report? report))
    (is (preflight/valid? report))
    (is (false?
         (preflight/report?
          (assoc report :valid? false))))
    (is (false?
         (preflight/report?
          (assoc report
                 :errors
                 [{:kind :forged}]))))
    (is (false?
         (preflight/report?
          (assoc report :trusted true))))
    (is (false?
         (preflight/report?
          (assoc report
                 :gesso.live.optimistic.preflight/version
                 (inc preflight/preflight-version)))))))

;; =============================================================================
;; Explanation surface
;; =============================================================================

(deftest explain-distinguishes-successful-assembly-from-failed-report
  (let [assembly
        (preflight/require-operation-assembly! (claim-options))

        failed-report
        (preflight/check-operation-assembly
         (claim-options
          {:server-operations
           {:request/cancel cancel-operation}}))]
    (is (= {:type preflight/operation-assembly-type
            :version preflight/preflight-version
            :name :example/request-operations
            :browser-assembly-name :example/optimistic-operation-browser
            :browser-role :browser
            :command-transport :htmx
            :operations #{:request/claim}
            :route-requirements
            (:route-requirements assembly)}
           (preflight/explain assembly)))

    (is (= {:type preflight/report-type
            :version preflight/preflight-version
            :valid? false
            :error-kinds #{:missing-trusted-server-operation}
            :analysis (:analysis failed-report)}
           (preflight/explain failed-report)))

    (is (= :not-explainable
           (error-kind #(preflight/explain {:not :preflight}))))))
