(ns gesso.live.acquisition-preflight-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.acquisition-preflight :as acquisition]
   [gesso.live.model :as model]))

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

(defn- allow?
  [_ctx _id]
  true)

(defn- html-response
  [node]
  {:status 200
   :headers {"content-type" "text/html; charset=utf-8"}
   :body node})

(defn- toolbar-query
  [_ctx location-id]
  {:fragment/id "request-toolbar"
   :location/id location-id
   :pending-open-count 0
   :stale? false})

(defn- toolbar-render
  [{:keys [fragment/id pending-open-count stale?]}]
  [:section {:id id
             :data-stale stale?}
   [:span pending-open-count]])

(defn- request-list-query
  [_ctx location-id]
  {:fragment/id "request-list"
   :location/id location-id
   :requests []})

(defn- request-list-render
  [{:keys [fragment/id requests]}]
  [:section {:id id}
   [:span (count requests)]])

(defn- compiled-live
  []
  (model/compile-live-app
   {:response html-response

    :scopes
    {:request-toolbar
     {:topic :humanhelp/request-toolbar
      :id-key :request/location-id
      :authorized? allow?}

     :request-list
     {:topic :humanhelp/request-list
      :id-key :request/location-id
      :authorized? allow?}}

    :graph
    {:request
     [{:scope :request-toolbar
       :id-key :request/location-id}
      {:scope :request-list
       :id-key :request/location-id}]}

    :fragments
    {:request-toolbar
     {:scope :request-toolbar
      :id-fn (constantly "request-toolbar")
      :query toolbar-query
      :render toolbar-render
      :swap :outerHTML}

     :request-list
     {:scope :request-list
      :id-fn (constantly "request-list")
      :query request-list-query
      :render request-list-render
      :swap :outerHTML}}}))

(def humanhelp-base-path
  "/app")

(def humanhelp-route-specs
  "Fixture deliberately shaped like net.humanhelp.example.routes/route-specs.

   Physical route facts remain application-owned. Acquisition preflight should
   be able to consume those facts without asking the application to repeat the
   HTTP method or route string in a second route registry."
  [{:id :humanhelp/request-toolbar-fragment
    :method :get
    :route "/fragments/request-toolbar"}
   {:id :humanhelp/request-list-fragment
    :method :get
    :route "/fragments/requests"}
   {:id :humanhelp/request-toolbar-stream
    :method :get
    :route "/streams/request-toolbar"}
   {:id :humanhelp/request-list-stream
    :method :get
    :route "/streams/requests"}])

(def humanhelp-route-spec-by-id
  (into {}
        (map (juxt :id identity))
        humanhelp-route-specs))

(defn- humanhelp-route-spec
  [route-id]
  (get humanhelp-route-spec-by-id route-id))

(defn- humanhelp-path
  [route-spec]
  (str humanhelp-base-path (:route route-spec)))

(defn- fragment-capability-from-route-spec
  [fragment route-spec]
  (is (= :get (:method route-spec))
      "The existing application route descriptor owns the physical method fact.")
  (acquisition/fragment-route
   {:fragment fragment
    :path (humanhelp-path route-spec)}))

(defn- stream-capability-from-route-spec
  [fragment route-spec]
  (is (= :get (:method route-spec))
      "The existing application route descriptor owns the physical method fact.")
  (acquisition/stream-route
   {:fragment fragment
    :path (humanhelp-path route-spec)}))

(defn- request-list-routes
  []
  {:fragment-route
   (fragment-capability-from-route-spec
    :request-list
    (humanhelp-route-spec :humanhelp/request-list-fragment))

   :stream-route
   (stream-capability-from-route-spec
    :request-list
    (humanhelp-route-spec :humanhelp/request-list-stream))})

(defn- acquisition-options
  ([]
   (acquisition-options {}))
  ([overrides]
   (merge
    {:name :humanhelp/request-list-acquisition
     :live-app (compiled-live)
     :fragment :request-list}
    (request-list-routes)
    overrides)))


(defn- generic-query
  [_ctx id]
  {:id id})

(defn- generic-render
  [{:keys [id]}]
  [:section {:id (str id)}])

(defn- never-target?
  [_ctx _change]
  false)

(defn- fragment-id
  [prefix]
  (fn [id]
    (str prefix "-" id)))

(defn- compile-shared-scope-live
  ([]
   (compile-shared-scope-live :humanhelp/shared))
  ([shared-topic]
   (model/compile-live-app
    {:response html-response

     :scopes
     {:shared
      {:topic shared-topic
       :id-key :request/location-id
       :authorized? allow?}

      :static
      {:topic :humanhelp/static
       :id-key :request/location-id
       :authorized? allow?}}

     :graph
     {:request
      [{:scope :shared
        :id-key :request/location-id
        :optional? true
        :when never-target?}]}

     :fragments
     {:shared-summary
      {:scope :shared
       :id-fn (fragment-id "shared-summary")
       :query generic-query
       :render generic-render
       :swap :outerHTML}

      :shared-detail
      {:scope :shared
       :id-fn (fragment-id "shared-detail")
       :query generic-query
       :render generic-render
       :swap :outerHTML}

      :static
      {:scope :static
       :id-fn (fragment-id "static")
       :query generic-query
       :render generic-render
       :swap :outerHTML}}})))

(defn- generic-realization
  [live-app fragment]
  (acquisition/require-acquisition-realization!
   {:name (keyword "fixture" (name fragment))
    :live-app live-app
    :fragment fragment
    :fragment-route
    (acquisition/fragment-route
     {:fragment fragment
      :path (str "/fragments/" (name fragment))})
    :stream-route
    (acquisition/stream-route
     {:fragment fragment
      :path (str "/streams/" (name fragment))})}))

(defn- assembly-error-kinds
  [report]
  (set (map :kind (:errors report))))

(defn- assembly-warning-kinds
  [report]
  (set (map :kind (:warnings report))))

(defn- assembly-issue-of-kind
  [report kind]
  (first (filter #(= kind (:kind %)) (:errors report))))

;; =============================================================================
;; Canonical route capabilities
;; =============================================================================

(deftest canonical-route-helpers-produce-closed-fragment-bound-capabilities
  (let [{:keys [fragment-route stream-route]}
        (request-list-routes)]
    (testing "physical method/boundary are derived from the selected route role"
      (is (acquisition/route-capability? fragment-route))
      (is (acquisition/route-capability? stream-route))
      (is (= {:kind :fragment
              :fragment :request-list
              :method :get
              :path "/app/fragments/requests"
              :boundary acquisition/progression-safe-fragment-boundary}
             (select-keys fragment-route
                          [:kind :fragment :method :path :boundary])))
      (is (= {:kind :stream
              :fragment :request-list
              :method :get
              :path "/app/streams/requests"
              :boundary acquisition/managed-stream-boundary}
             (select-keys stream-route
                          [:kind :fragment :method :path :boundary]))))

    (testing "route capability itself remains closed"
      (is (= #{:gesso.live.acquisition-preflight/type
               :gesso.live.acquisition-preflight/version
               :kind
               :fragment
               :method
               :path
               :boundary}
             (set (keys fragment-route)))))))

(deftest humanhelp-shaped-route-facts-feed-preflight-without-a-second-physical-route-registry
  (let [fragment-spec
        (humanhelp-route-spec :humanhelp/request-list-fragment)

        stream-spec
        (humanhelp-route-spec :humanhelp/request-list-stream)

        {:keys [fragment-route stream-route]}
        (request-list-routes)]
    (testing "the preflight capabilities derive path from the already-owned route descriptors"
      (is (= (humanhelp-path fragment-spec)
             (:path fragment-route)))
      (is (= (humanhelp-path stream-spec)
             (:path stream-route)))
      (is (= (:method fragment-spec)
             (:method fragment-route)))
      (is (= (:method stream-spec)
             (:method stream-route))))

    (testing "the only additional relation is the semantic Live fragment identity"
      (is (= :request-list (:fragment fragment-route)))
      (is (= :request-list (:fragment stream-route))))))

(deftest route-capability-constructor-preserves-malformed-physical-facts-for-diagnostics
  (let [route
        (acquisition/route-capability
         {:kind :fragment
          :fragment :request-list
          :method :post
          :path "/app/fragments/requests"
          :boundary :example/arbitrary-handler})]
    (is (acquisition/route-capability? route))
    (is (= :post (:method route)))
    (is (= :example/arbitrary-handler (:boundary route)))))

;; =============================================================================
;; Successful authoritative acquisition
;; =============================================================================

(deftest successful-acquisition-closes-the-managed-progression-bound-reread-path
  (let [options
        (acquisition-options)

        report
        (acquisition/check-acquisition options)

        realization
        (acquisition/require-acquisition-realization! options)]
    (testing "report is closed and successful"
      (is (acquisition/report? report))
      (is (acquisition/valid? report))
      (is (= [] (:errors report)))
      (is (= [] (:warnings report)))
      (is (= :request-list
             (get-in report [:analysis :fragment])))
      (is (= :request-list
             (get-in report [:analysis :scope])))
      (is (= #{:request-toolbar :request-list}
             (get-in report [:analysis :known-fragments]))))

    (testing "successful realization retains exact semantic/physical evidence"
      (is (acquisition/acquisition-realization? realization))
      (is (= :humanhelp/request-list-acquisition
             (:name realization)))
      (is (= :request-list (:fragment realization)))
      (is (= :request-list (:scope realization)))
      (is (= (:fragment-route options)
             (:fragment-route realization)))
      (is (= (:stream-route options)
             (:stream-route realization))))

    (testing "the guarantee describes the current managed path without claiming automatic adoption"
      (is (= {:profile acquisition/managed-acquisition-profile
              :initial :stream-open-reread
              :invalidation :advisory-sse
              :refresh-owner :browser-adapter
              :reread :progression-bound
              :render :compiled-live-fragment}
             (:acquisition realization)))
      (is (= :preflight-closed-relative-to-trusted-routes
             (:guarantee (acquisition/explain realization)))))))

;; =============================================================================
;; Missing and incompatible acquisition edges
;; =============================================================================

(deftest missing-acquisition-is-rejected-before-browser-interaction
  (let [report
        (acquisition/check-acquisition
         (acquisition-options
          {:fragment-route nil
           :stream-route nil}))]
    (is (acquisition/report? report))
    (is (false? (acquisition/valid? report)))
    (is (= #{:missing-authoritative-acquisition
             :missing-fragment-route
             :missing-stream-route}
           (error-kinds report)))
    (is (= #{:fragment-route :stream-route}
           (:required
            (issue-of-kind report :missing-authoritative-acquisition))))))

(deftest each-physical-route-is-an-independent-required-edge
  (let [without-fragment
        (acquisition/check-acquisition
         (acquisition-options {:fragment-route nil}))

        without-stream
        (acquisition/check-acquisition
         (acquisition-options {:stream-route nil}))]
    (is (= #{:missing-fragment-route}
           (error-kinds without-fragment)))
    (is (= #{:missing-stream-route}
           (error-kinds without-stream)))))

(deftest unknown-semantic-fragment-fails-with-the-closed-available-set
  (let [report
        (acquisition/check-acquisition
         (acquisition-options {:fragment :request-lsit}))

        issue
        (issue-of-kind report :unknown-fragment)]
    (is (= #{:unknown-fragment}
           (error-kinds report)))
    (is (= :request-lsit (:fragment issue)))
    (is (= #{:request-toolbar :request-list}
           (:known-fragments issue)))))

(deftest route-semantic-relabeling-is-rejected-even-when-the-physical-route-is-valid
  (let [toolbar-fragment-route
        (fragment-capability-from-route-spec
         :request-toolbar
         (humanhelp-route-spec :humanhelp/request-toolbar-fragment))

        report
        (acquisition/check-acquisition
         (acquisition-options {:fragment-route toolbar-fragment-route}))

        issue
        (issue-of-kind report :route-fragment-mismatch)]
    (is (acquisition/route-capability? toolbar-fragment-route))
    (is (= #{:route-fragment-mismatch}
           (error-kinds report)))
    (is (= :request-list (:expected issue)))
    (is (= :request-toolbar (:actual issue)))
    (is (= "/app/fragments/request-toolbar"
           (:path issue)))))

(deftest route-role-method-and-boundary-mismatches-remain-distinct
  (let [wrong-role
        (acquisition/route-capability
         {:kind :stream
          :fragment :request-list
          :method :get
          :path "/app/fragments/requests"
          :boundary acquisition/progression-safe-fragment-boundary})

        wrong-method
        (acquisition/route-capability
         {:kind :fragment
          :fragment :request-list
          :method :post
          :path "/app/fragments/requests"
          :boundary acquisition/progression-safe-fragment-boundary})

        arbitrary-handler
        (acquisition/route-capability
         {:kind :fragment
          :fragment :request-list
          :method :get
          :path "/app/fragments/requests"
          :boundary :example/arbitrary-get})]
    (is (= #{:route-kind-mismatch}
           (error-kinds
            (acquisition/check-acquisition
             (acquisition-options {:fragment-route wrong-role})))))

    (is (= #{:fragment-route-not-get}
           (error-kinds
            (acquisition/check-acquisition
             (acquisition-options {:fragment-route wrong-method})))))

    (is (= #{:fragment-reread-not-progression-safe}
           (error-kinds
            (acquisition/check-acquisition
             (acquisition-options {:fragment-route arbitrary-handler})))))))

(deftest stream-must-use-the-managed-model-backed-live-boundary
  (let [arbitrary-stream
        (acquisition/route-capability
         {:kind :stream
          :fragment :request-list
          :method :get
          :path "/app/streams/requests"
          :boundary :example/arbitrary-sse})

        report
        (acquisition/check-acquisition
         (acquisition-options {:stream-route arbitrary-stream}))]
    (is (= #{:stream-route-not-managed-live}
           (error-kinds report)))
    (is (= acquisition/managed-stream-boundary
           (:expected
            (issue-of-kind report :stream-route-not-managed-live))))))

;; =============================================================================
;; Closed report / realization recognition
;; =============================================================================

(deftest forged-positive-report-is-not-recognized
  (let [report
        (acquisition/check-acquisition
         (acquisition-options {:fragment-route nil}))

        forged
        (assoc report
               :valid? true
               :errors [])]
    (is (acquisition/report? report))
    (is (false? (acquisition/valid? report)))
    (is (false? (acquisition/report? forged)))
    (is (false? (acquisition/valid? forged)))))

(deftest relabeling-report-analysis-is-not-recognized
  (let [report
        (acquisition/check-acquisition (acquisition-options))

        forged
        (assoc-in report [:analysis :fragment] :request-toolbar)]
    (is (acquisition/valid? report))
    (is (false? (acquisition/report? forged)))
    (is (false? (acquisition/valid? forged)))))

(deftest successful-realization-cannot-be-relabelled-or-route-swapped
  (let [realization
        (acquisition/require-acquisition-realization!
         (acquisition-options))

        relabelled
        (assoc realization :fragment :request-toolbar)

        toolbar-route
        (fragment-capability-from-route-spec
         :request-toolbar
         (humanhelp-route-spec :humanhelp/request-toolbar-fragment))

        route-swapped
        (assoc realization :fragment-route toolbar-route)]
    (is (acquisition/acquisition-realization? realization))
    (is (false? (acquisition/acquisition-realization? relabelled)))
    (is (false? (acquisition/acquisition-realization? route-swapped)))))

(deftest require-acquisition-realization-preserves-the-complete-failure-report
  (let [data
        (error-data
         #(acquisition/require-acquisition-realization!
           (acquisition-options {:stream-route nil})))

        report
        (:preflight data)]
    (is (= :gesso.live.acquisition-preflight/error
           (:error/type data)))
    (is (= :authoritative-acquisition-preflight-failed
           (:error/kind data)))
    (is (acquisition/report? report))
    (is (= #{:missing-stream-route}
           (error-kinds report)))))

;; =============================================================================
;; Definition-time input hygiene
;; =============================================================================

(deftest canonical-route-helpers-reject-duplicated-or-incomplete-physical-description
  (testing "normal helper owns method and boundary so callers cannot restate them"
    (is (= :unknown-route-key
           (error-kind
            #(acquisition/fragment-route
              {:fragment :request-list
               :path "/app/fragments/requests"
               :method :get}))))
    (is (= :unknown-route-key
           (error-kind
            #(acquisition/stream-route
              {:fragment :request-list
               :path "/app/streams/requests"
               :boundary acquisition/managed-stream-boundary})))))

  (testing "normal helper requires exactly semantic fragment plus physical path"
    (is (= :missing-route-key
           (error-kind
            #(acquisition/fragment-route
              {:fragment :request-list}))))
    (is (= :missing-route-key
           (error-kind
            #(acquisition/stream-route
              {:path "/app/streams/requests"}))))))

(deftest malformed-compiled-live-input-is-never-treated-as-authoritative-model-evidence
  (let [compiled
        (compiled-live)

        tampered
        (assoc-in compiled
                  [:fragments :request-list :scope]
                  :missing-scope)

        report
        (acquisition/check-acquisition
         (acquisition-options {:live-app tampered}))]
    (is (= #{:invalid-live-app}
           (error-kinds report)))
    (is (false? (acquisition/valid? report)))))

;; =============================================================================
;; Graph-derived acquisition obligations
;; =============================================================================

(deftest graph-obligations-require-every-fragment-projecting-an-invalidated-scope
  (let [live-app     (compile-shared-scope-live)
        obligations  (acquisition/acquisition-obligations live-app)]
    (is (= #{:shared-summary :shared-detail}
           (set (keys obligations))))
    (testing "multiple fragments projecting the same invalidated scope are independent obligations"
      (doseq [fragment [:shared-summary :shared-detail]]
        (let [obligation (get obligations fragment)]
          (is (acquisition/acquisition-obligation? obligation))
          (is (= fragment (:fragment obligation)))
          (is (= :shared (:scope obligation)))
          (is (= :humanhelp/shared (:scope-topic obligation)))
          (is (= :request/location-id (:scope-id-key obligation)))
          (is (= #{:request} (:change-topics obligation)))
          (is (= acquisition/managed-acquisition-profile
                 (:acquisition-profile obligation))))))
    (testing "declared fragments whose scopes are not graph-reachable are not silently promoted"
      (is (nil? (get obligations :static))))))

(deftest conditional-and-optional-targets-still-create-conservative-may-invalidate-obligations
  (let [live-app    (compile-shared-scope-live)
        graph-target (first (get-in live-app [:graph :request]))
        obligations (acquisition/acquisition-obligations live-app)]
    (is (true? (:optional? graph-target)))
    (is (= never-target? (:when graph-target)))
    (testing "preflight does not execute arbitrary application predicates to erase a possible edge"
      (is (= #{:shared-summary :shared-detail}
             (set (keys obligations))))
      (is (= #{:request}
             (get-in obligations [:shared-summary :change-topics]))))
    (testing "the obligation means acquisition must exist if the target becomes active, not that every change must invalidate it"
      (is (= acquisition/managed-acquisition-profile
             (get-in obligations [:shared-detail :acquisition-profile]))))))

(deftest missing-graph-derived-realizations-fail-with-one-local-obligation-per-fragment
  (let [live-app (compile-shared-scope-live)
        report   (acquisition/check-acquisition-assembly
                  {:name :fixture/shared
                   :live-app live-app
                   :realizations {}})
        errors   (:errors report)]
    (is (acquisition/acquisition-assembly-report? report))
    (is (false? (acquisition/acquisition-assembly-valid? report)))
    (is (= #{:missing-acquisition-realization}
           (assembly-error-kinds report)))
    (is (= 2 (count errors)))
    (is (= #{:shared-summary :shared-detail}
           (get-in report [:analysis :required-fragments])))
    (is (= #{}
           (get-in report [:analysis :supplied-fragments])))
    (doseq [error errors]
      (is (contains? #{:shared-summary :shared-detail} (:fragment error)))
      (is (= #{:shared-summary :shared-detail}
             (:required-fragments error)))
      (is (= #{} (:supplied-fragments error)))
      (is (acquisition/acquisition-obligation? (:obligation error))))))

(deftest successful-acquisition-assembly-is-derived-from-the-live-graph-not-caller-requirements
  (let [live-app (compile-shared-scope-live)
        summary  (generic-realization live-app :shared-summary)
        detail   (generic-realization live-app :shared-detail)
        options  {:name :fixture/shared
                  :live-app live-app
                  :realizations {:shared-summary summary
                                 :shared-detail detail}}
        report   (acquisition/check-acquisition-assembly options)
        assembly (acquisition/require-acquisition-assembly! options)
        explanation (acquisition/explain assembly)]
    (is (acquisition/acquisition-assembly-valid? report))
    (is (empty? (:errors report)))
    (is (empty? (:warnings report)))
    (is (= #{:shared-summary :shared-detail}
           (get-in report [:analysis :required-fragments])))
    (is (= #{:shared-summary :shared-detail}
           (get-in report [:analysis :supplied-fragments])))
    (is (acquisition/acquisition-assembly? assembly))
    (is (= (acquisition/acquisition-obligations live-app)
           (:obligations assembly)))
    (is (= #{:shared-summary :shared-detail}
           (:required-fragments assembly)))
    (is (= acquisition/managed-acquisition-profile
           (:acquisition-profile assembly)))
    (is (= :invalidation-acquisition-preflight-closed-relative-to-trusted-routes
           (:guarantee explanation)))
    (is (= (:obligations assembly) (:obligations explanation)))
    (is (= (:required-fragments assembly) (:required-fragments explanation)))))

(deftest valid-realization-outside-the-invalidation-graph-is-retained-as-a-warning-not-a-fake-obligation
  (let [live-app (compile-shared-scope-live)
        summary  (generic-realization live-app :shared-summary)
        detail   (generic-realization live-app :shared-detail)
        static   (generic-realization live-app :static)
        options  {:name :fixture/shared-with-static
                  :live-app live-app
                  :realizations {:shared-summary summary
                                 :shared-detail detail
                                 :static static}}
        report   (acquisition/check-acquisition-assembly options)
        assembly (acquisition/require-acquisition-assembly! options)
        warning  (first (:warnings report))]
    (is (acquisition/acquisition-assembly-valid? report))
    (is (= #{:unrequired-acquisition-realization}
           (assembly-warning-kinds report)))
    (is (= 1 (count (:warnings report))))
    (is (= :static (:fragment warning)))
    (is (= #{:shared-summary :shared-detail}
           (:required-fragments warning)))
    (is (= #{:shared-summary :shared-detail}
           (:required-fragments assembly)))
    (is (= #{:shared-summary :shared-detail :static}
           (set (keys (:realizations assembly)))))
    (is (nil? (get (:obligations assembly) :static)))
    (is (acquisition/acquisition-assembly? assembly))))

(deftest invalid-required-realization-is-not-mistaken-for-a-missing-registry-key
  (let [live-app (compile-shared-scope-live)
        summary  (generic-realization live-app :shared-summary)
        detail   (generic-realization live-app :shared-detail)
        tampered (assoc detail :scope :static)
        report   (acquisition/check-acquisition-assembly
                  {:name :fixture/tampered
                   :live-app live-app
                   :realizations {:shared-summary summary
                                  :shared-detail tampered}})]
    (is (= #{:invalid-acquisition-realization}
           (assembly-error-kinds report)))
    (is (= #{:shared-summary :shared-detail}
           (get-in report [:analysis :supplied-fragments])))
    (is (false? (acquisition/acquisition-assembly-valid? report)))))

(deftest acquisition-realization-cannot-be-reused-across-compiled-live-apps
  (let [live-app-a (compile-shared-scope-live :humanhelp/shared-a)
        live-app-b (compile-shared-scope-live :humanhelp/shared-b)
        summary-a  (generic-realization live-app-a :shared-summary)
        detail-b   (generic-realization live-app-b :shared-detail)
        report     (acquisition/check-acquisition-assembly
                    {:name :fixture/cross-app
                     :live-app live-app-b
                     :realizations {:shared-summary summary-a
                                    :shared-detail detail-b}})
        mismatch   (assembly-issue-of-kind report :acquisition-live-app-mismatch)]
    (is (acquisition/acquisition-realization? summary-a))
    (is (= #{:acquisition-live-app-mismatch}
           (assembly-error-kinds report)))
    (is (= :shared-summary (:fragment mismatch)))
    (is (= live-app-b (:expected-live-app mismatch)))
    (is (= live-app-a (:actual-live-app mismatch)))))

(deftest realization-registry-key-cannot-semantically-relabel-another-fragment
  (let [live-app (compile-shared-scope-live)
        summary  (generic-realization live-app :shared-summary)
        detail   (generic-realization live-app :shared-detail)
        report   (acquisition/check-acquisition-assembly
                  {:name :fixture/relabelled
                   :live-app live-app
                   :realizations {:shared-summary detail
                                  :shared-detail summary}})]
    (is (= #{:acquisition-fragment-key-mismatch}
           (assembly-error-kinds report)))
    (is (= 2 (count (:errors report))))
    (is (false? (acquisition/acquisition-assembly-valid? report)))))

(deftest unknown-realization-fragments-are-rejected-with-the-compiled-available-set
  (let [live-app (compile-shared-scope-live)
        summary  (generic-realization live-app :shared-summary)
        detail   (generic-realization live-app :shared-detail)
        report   (acquisition/check-acquisition-assembly
                  {:name :fixture/unknown
                   :live-app live-app
                   :realizations {:shared-summary summary
                                  :shared-detail detail
                                  :missing summary}})
        unknown  (assembly-issue-of-kind report :unknown-acquisition-fragment)]
    (is (= #{:unknown-acquisition-fragment
             :acquisition-fragment-key-mismatch}
           (assembly-error-kinds report)))
    (is (= :missing (:fragment unknown)))
    (is (= #{:shared-summary :shared-detail :static}
           (:known-fragments unknown)))))

;; =============================================================================
;; Closed assembly/report recognition
;; =============================================================================

(deftest forged-positive-acquisition-assembly-report-is-not-recognized
  (let [live-app (compile-shared-scope-live)
        report   (acquisition/check-acquisition-assembly
                  {:name :fixture/forged
                   :live-app live-app
                   :realizations {}})
        forged   (assoc report :valid? true :errors [])]
    (is (acquisition/acquisition-assembly-report? report))
    (is (false? (acquisition/acquisition-assembly-valid? report)))
    (is (false? (acquisition/acquisition-assembly-report? forged)))
    (is (false? (acquisition/acquisition-assembly-valid? forged)))))

(deftest acquisition-assembly-report-obligations-cannot-be-relabelled-after-derivation
  (let [live-app (compile-shared-scope-live)
        summary  (generic-realization live-app :shared-summary)
        detail   (generic-realization live-app :shared-detail)
        report   (acquisition/check-acquisition-assembly
                  {:name :fixture/report
                   :live-app live-app
                   :realizations {:shared-summary summary
                                  :shared-detail detail}})
        forged   (assoc-in report
                           [:analysis :obligations :shared-summary :change-topics]
                           #{:other-change})]
    (is (acquisition/acquisition-assembly-valid? report))
    (is (false? (acquisition/acquisition-assembly-report? forged)))
    (is (false? (acquisition/acquisition-assembly-valid? forged)))))

(deftest closed-acquisition-assembly-rejects-obligation-required-set-profile-and-realization-tampering
  (let [live-app (compile-shared-scope-live)
        summary  (generic-realization live-app :shared-summary)
        detail   (generic-realization live-app :shared-detail)
        assembly (acquisition/require-acquisition-assembly!
                  {:name :fixture/closed
                   :live-app live-app
                   :realizations {:shared-summary summary
                                  :shared-detail detail}})
        obligation-tampered
        (assoc-in assembly
                  [:obligations :shared-summary :change-topics]
                  #{:other-change})
        required-tampered
        (assoc assembly :required-fragments #{:shared-summary})
        profile-tampered
        (assoc assembly :acquisition-profile :example/other-profile)
        realization-swapped
        (assoc-in assembly [:realizations :shared-summary] detail)]
    (is (acquisition/acquisition-assembly? assembly))
    (is (false? (acquisition/acquisition-assembly? obligation-tampered)))
    (is (false? (acquisition/acquisition-assembly? required-tampered)))
    (is (false? (acquisition/acquisition-assembly? profile-tampered)))
    (is (false? (acquisition/acquisition-assembly? realization-swapped)))))

(deftest require-acquisition-assembly-preserves-the-complete-failure-report
  (let [live-app (compile-shared-scope-live)
        summary  (generic-realization live-app :shared-summary)
        data     (error-data
                  #(acquisition/require-acquisition-assembly!
                    {:name :fixture/incomplete
                     :live-app live-app
                     :realizations {:shared-summary summary}}))
        report   (:preflight data)]
    (is (= :gesso.live.acquisition-preflight/error
           (:error/type data)))
    (is (= :authoritative-acquisition-assembly-failed
           (:error/kind data)))
    (is (acquisition/acquisition-assembly-report? report))
    (is (= #{:missing-acquisition-realization}
           (assembly-error-kinds report)))
    (is (= :shared-detail
           (:fragment
            (assembly-issue-of-kind report :missing-acquisition-realization))))))

(deftest assembly-definition-time-input-hygiene-is-closed-and-early
  (let [live-app (compile-shared-scope-live)]
    (is (= :unknown-assembly-option
           (error-kind
            #(acquisition/check-acquisition-assembly
              {:live-app live-app
               :realizations {}
               :required-fragments #{:shared-summary}}))))
    (is (= :invalid-acquisition-realizations
           (error-kind
            #(acquisition/check-acquisition-assembly
              {:live-app live-app
               :realizations [:shared-summary]}))))
    (is (= :invalid-name
           (error-kind
            #(acquisition/check-acquisition-assembly
              {:name "not-a-keyword"
               :live-app live-app
               :realizations {}}))))))

(deftest acquisition-obligation-derivation-rejects-tampered-compiled-live-metadata
  (let [live-app (compile-shared-scope-live)
        tampered (assoc-in live-app [:fragments :shared-summary :scope] :missing-scope)]
    (is (= :invalid-live-app
           (error-kind #(acquisition/acquisition-obligations tampered))))))
