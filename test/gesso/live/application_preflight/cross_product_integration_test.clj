(ns gesso.live.application-preflight.cross-product-integration-test
  "Integration tests for the original v642 cross-product substitution
   campaign, migrated from application_preflight_test.clj (v764).
   The tests validate rejection of incompatible composition while permitting
   independently valid and semantically compatible product substitutions.
   The originals remain in application_preflight_test.clj until v766."
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.live.acquisition-preflight :as acquisition]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.fixture
    :refer [allow? application-options child-path error-kinds execution-assembly
            generic-query generic-render html-response record-artifact!
            standard-operation-acquisition trusted-operation with-temp-dir]]
   [gesso.live.browser.preflight :as browser-preflight]
   [gesso.live.model :as model]
   [gesso.live.operation-acquisition-preflight :as operation-acquisition]
   [gesso.live.optimistic.execution-preflight :as execution-preflight]
   [gesso.live.optimistic.preflight :as operation-preflight]
   [gesso.live.optimistic.route-preflight :as route-preflight]
   [gesso.live.optimistic.server :as server]))

;; =============================================================================
;; v642 whole-application cross-product substitution campaign
;; =============================================================================

(defn- variant-operations
  []
  {:request/claim
   (trusted-operation
    :request/claim
    {:name :fixture-b/request-claim
     :authority-role :authority-b
     :published-change-topics #{:request}})

   :request/reassign
   (trusted-operation
    :request/reassign
    {:name :fixture-b/request-reassign
     :authority-role :authority-b
     :published-change-topics
     #{:request :request-assignment :audit :external/audit}})

   :request/cancel
   (trusted-operation
    :request/cancel
    {:name :fixture-b/request-cancel
     :authority-role :authority-b
     :published-change-topics #{}})})

(defn- compiled-live-b
  []
  (model/compile-live-app
   {:response html-response
    :scopes
    {:request-toolbar
     {:topic :fixture-b/request-toolbar
      :id-key :request/location-id
      :authorized? allow?}

     :request-list
     {:topic :fixture-b/request-list
      :id-key :request/location-id
      :authorized? allow?}

     :audit-scope
     {:topic :fixture-b/audit
      :id-key :request/location-id
      :authorized? allow?}}

    :graph
    {:request
     [{:scope :request-toolbar
       :id-key :request/location-id}
      {:scope :request-list
       :id-key :request/location-id}]

     :request-assignment
     [{:scope :request-list
       :id-key :request/location-id}]

     :audit
     [{:scope :audit-scope
       :id-key :request/location-id}]}

    :fragments
    {:request-toolbar
     {:scope :request-toolbar
      :id-fn (fn [id] (str "request-toolbar-b-" id))
      :query generic-query
      :render generic-render
      :swap :outerHTML}

     :request-list
     {:scope :request-list
      :id-fn (fn [id] (str "request-list-b-" id))
      :query generic-query
      :render generic-render
      :swap :outerHTML}}}))

(defn- acquisition-assembly-b
  []
  (let [live-app (compiled-live-b)]
    (acquisition/require-acquisition-assembly!
     {:name :fixture-b/live-acquisition
      :live-app live-app
      :realizations
      {:request-toolbar
       (acquisition/require-acquisition-realization!
        {:name :fixture-b/request-toolbar-acquisition
         :live-app live-app
         :fragment :request-toolbar
         :fragment-route
         (acquisition/fragment-route
          {:fragment :request-toolbar
           :path "/b/fragments/request-toolbar"})
         :stream-route
         (acquisition/stream-route
          {:fragment :request-toolbar
           :path "/b/streams/request-toolbar"})})
       :request-list
       (acquisition/require-acquisition-realization!
        {:name :fixture-b/request-list-acquisition
         :live-app live-app
         :fragment :request-list
         :fragment-route
         (acquisition/fragment-route
          {:fragment :request-list
           :path "/b/fragments/request-list"})
         :stream-route
         (acquisition/stream-route
          {:fragment :request-list
           :path "/b/streams/request-list"})})}})))

(defn- operation-acquisition-b
  []
  (operation-acquisition/require-operation-acquisition-assembly!
   {:name :fixture-b/application-operation-acquisition
    :execution-assembly (execution-assembly (variant-operations))
    :acquisition-assembly (acquisition-assembly-b)}))

(deftest cross-product-campaign-establishes-two-independently-valid-distinct-products
  (let [a @standard-operation-acquisition
        b (operation-acquisition-b)]
    (is (operation-acquisition/operation-acquisition-assembly? a))
    (is (operation-acquisition/operation-acquisition-assembly? b))
    (is (not= (get-in a [:execution-assembly :route-assembly :operation-assembly :browser-assembly])
              (get-in b [:execution-assembly :route-assembly :operation-assembly :browser-assembly])))
    (is (not= (:acquisition-assembly a) (:acquisition-assembly b)))
    (is (= (execution-preflight/published-change-topics (:execution-assembly a))
           (execution-preflight/published-change-topics (:execution-assembly b))))))

(deftest cross-product-campaign-rejects-browser-assembly-from-another-valid-operation-product
  (let [a @standard-operation-acquisition
        b (operation-acquisition-b)
        browser-b
        (get-in b [:execution-assembly :route-assembly :operation-assembly :browser-assembly])
        spliced
        (assoc-in
         a
         [:execution-assembly :route-assembly :operation-assembly :browser-assembly]
         browser-b)]
    (is (browser-preflight/assembly-manifest? browser-b))
    (is (not (operation-preflight/operation-assembly?
              (get-in spliced [:execution-assembly :route-assembly :operation-assembly]))))
    (is (not (operation-acquisition/operation-acquisition-assembly? spliced)))))

(deftest cross-product-campaign-rejects-route-assembly-from-another-valid-execution-product
  (let [a @standard-operation-acquisition
        b (operation-acquisition-b)
        route-b (get-in b [:execution-assembly :route-assembly])
        spliced (assoc-in a [:execution-assembly :route-assembly] route-b)]
    (is (route-preflight/route-assembly? route-b))
    (is (not (execution-preflight/execution-assembly?
              (:execution-assembly spliced))))
    (is (not (operation-acquisition/operation-acquisition-assembly? spliced)))))

(deftest cross-product-campaign-rejects-prepared-server-from-another-valid-execution-product
  (let [a @standard-operation-acquisition
        b (operation-acquisition-b)
        server-b (get-in b [:execution-assembly :server])
        spliced (assoc-in a [:execution-assembly :server] server-b)]
    (is (server/server? server-b))
    (is (not (execution-preflight/execution-assembly?
              (:execution-assembly spliced))))
    (is (not (operation-acquisition/operation-acquisition-assembly? spliced)))))

(deftest cross-product-campaign-rejects-fragment-realization-from-another-compiled-live-application
  (let [a @standard-operation-acquisition
        b (operation-acquisition-b)
        realization-b
        (get-in b [:acquisition-assembly :realizations :request-list])
        spliced
        (assoc-in
         a
         [:acquisition-assembly :realizations :request-list]
         realization-b)]
    (is (acquisition/acquisition-realization? realization-b))
    (is (not (acquisition/acquisition-assembly?
              (:acquisition-assembly spliced))))
    (is (not (operation-acquisition/operation-acquisition-assembly? spliced)))))

(deftest cross-product-campaign-rejects-valid-artifact-from-a-different-browser-product
  (with-temp-dir
    (fn [dir]
      (let [a @standard-operation-acquisition
            b (operation-acquisition-b)
            artifact-a (child-path dir "application-a.js")]
        (record-artifact! a artifact-a)
        (let [report
              (application/check-application-assembly
               (application-options b artifact-a))]
          (is (application/report? report))
          (is (not (application/valid? report)))
          (is (contains? (error-kinds report)
                         :browser-artifact-verification-failed)))))))

(deftest cross-product-campaign-allows-semantically-compatible-execution-live-recomposition
  (let [a @standard-operation-acquisition
        b (operation-acquisition-b)
        recomposed
        (operation-acquisition/require-operation-acquisition-assembly!
         {:name :fixture/recomposed-b-execution-a-live
          :execution-assembly (:execution-assembly b)
          :acquisition-assembly (:acquisition-assembly a)})]
    (is (operation-acquisition/operation-acquisition-assembly? recomposed))
    (is (= (execution-preflight/published-change-topics (:execution-assembly b))
           (execution-preflight/published-change-topics (:execution-assembly recomposed))))
    (is (= #{:request-toolbar :request-list}
           (get (operation-acquisition/affected-fragments recomposed)
                :request/claim)))
    (is (= (:acquisition-assembly a)
           (:acquisition-assembly recomposed)))))

(deftest cross-product-campaign-allows-semantically-compatible-live-substitution-with-different-physical-routes
  (let [a @standard-operation-acquisition
        b (operation-acquisition-b)
        recomposed
        (operation-acquisition/require-operation-acquisition-assembly!
         {:name :fixture/recomposed-a-execution-b-live
          :execution-assembly (:execution-assembly a)
          :acquisition-assembly (:acquisition-assembly b)})]
    (is (operation-acquisition/operation-acquisition-assembly? recomposed))
    (is (= #{:request-toolbar :request-list}
           (get (operation-acquisition/affected-fragments recomposed)
                :request/claim)))
    (is (= "/b/fragments/request-list"
           (get-in recomposed
                   [:acquisition-assembly
                    :realizations
                    :request-list
                    :fragment-route
                    :path])))))
