(ns gesso.live.application-preflight.fixture
  "Shared test-only fixtures for complete application-preflight assemblies.

   This namespace is not part of Gesso's application-facing API.  It builds
   actual upstream Choreo, operation, acquisition, execution, and browser
   products for integration tests; a local temporary browser artifact and
   receipt are deliberately minted for each application test.

   Fixture helpers are extracted unchanged from the original whole-application
   tests.  They do not certify the assembled application independently of the
   production preflight boundary."
  (:require
   [gesso.choreo.identity :as identity]
   [gesso.choreo.preflight :as choreo-preflight]
   [gesso.live.acquisition-preflight :as acquisition]
   [gesso.live.application-preflight :as application]
   [gesso.live.browser.build :as browser-build]
   [gesso.live.browser.preflight :as browser-preflight]
   [gesso.live.model :as model]
   [gesso.live.operation-acquisition-preflight :as operation-acquisition]
   [gesso.live.ui :as ui]
   [gesso.live.optimistic.capability :as capability]
   [gesso.live.optimistic.choreo :as optimistic-choreo]
   [gesso.live.optimistic.execution-preflight :as execution-preflight]
   [gesso.live.optimistic.preflight :as operation-preflight]
   [gesso.live.optimistic.route-preflight :as route-preflight]
   [gesso.live.optimistic.server :as server])
  (:import
   (java.nio.file Files)))

;; Test-only fixture construction.  Public bindings are deliberately shared
;; across future independent integration-test namespaces; private helpers
;; remain local to this namespace.

(defn- exception-info-data
  [error]
  (loop [current error]
    (cond
      (nil? current)
      nil

      (instance? clojure.lang.ExceptionInfo current)
      (ex-data current)

      :else
      (recur (.getCause ^Throwable current)))))

(defn error-data
  [f]
  (try
    (f)
    nil
    (catch Throwable error
      (exception-info-data error))))

(defn error-kind
  [f]
  (:error/kind (error-data f)))

(defn error-kinds
  [report]
  (set (map :kind (:errors report))))

(defn warning-kinds
  [report]
  (set (map :kind (:warnings report))))

(defn- delete-tree!
  [root]
  (doseq [file (reverse (file-seq root))]
    (Files/deleteIfExists (.toPath file))))

(defn with-temp-dir
  [f]
  (let [path
        (Files/createTempDirectory
         "gesso-application-preflight-test-"
         (make-array java.nio.file.attribute.FileAttribute 0))
        dir (.toFile path)]
    (try
      (f dir)
      (finally
        (delete-tree! dir)))))

(defn child-path
  [dir child]
  (str (.resolve (.toPath dir) child)))

(defn- allow?
  [_ctx _id]
  true)

(defn html-response
  [node]
  {:status 200
   :headers {"content-type" "text/html; charset=utf-8"}
   :body node})

(defn generic-query
  [_ctx id]
  {:fragment/id (str id)})

(defn generic-render
  [{:keys [fragment/id]}]
  [:section {:id id}])

(defn- compiled-live
  []
  (model/compile-live-app
   {:response html-response
    :scopes
    {:request-toolbar
     {:topic :fixture/request-toolbar
      :id-key :request/location-id
      :authorized? allow?}

     :request-list
     {:topic :fixture/request-list
      :id-key :request/location-id
      :authorized? allow?}

     :audit-scope
     {:topic :fixture/audit
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
      :id-fn (fn [id] (str "request-toolbar-" id))
      :query generic-query
      :render generic-render
      :swap :outerHTML}

     :request-list
     {:scope :request-list
      :id-fn (fn [id] (str "request-list-" id))
      :query generic-query
      :render generic-render
      :swap :outerHTML}}}))

(defn- realization
  [live-app fragment]
  (acquisition/require-acquisition-realization!
   {:name (keyword "fixture" (str (name fragment) "-acquisition"))
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

(defn acquisition-assembly
  [live-app]
  (acquisition/require-acquisition-assembly!
   {:name :fixture/live-acquisition
    :live-app live-app
    :realizations
    {:request-toolbar (realization live-app :request-toolbar)
     :request-list (realization live-app :request-list)}}))

(def trusted-principal
  (identity/principal "helper-1"))

(defn trusted-operation
  [operation overrides]
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
    overrides)))

(defn- standard-operations
  []
  {:request/claim
   (trusted-operation
    :request/claim
    {:published-change-topics #{:request}})

   :request/reassign
   (trusted-operation
    :request/reassign
    {:published-change-topics
     #{:request :request-assignment :audit :external/audit}})

   :request/cancel
   (trusted-operation
    :request/cancel
    {:published-change-topics #{}})})

(defn- browser-plan
  [prepared-operation]
  (optimistic-choreo/command-plan
   {:name (:name prepared-operation)
    :operation (:operation prepared-operation)
    :browser-role (:browser-role prepared-operation)
    :authority-role (:authority-role prepared-operation)}
   :browser))

(defn- browser-assembly
  [operations]
  (let [plans
        (into
         (sorted-map)
         (map (fn [[operation entry]]
                [operation (browser-plan entry)]))
         operations)
        plan-registry
        (choreo-preflight/require-plan-registry!
         {:name :fixture/application-preflight-plans
          :plans plans
          :required-keys (set (keys plans))
          :single-role? true
          :expected-role :browser})]
    (browser-preflight/require-browser-assembly!
     {:name :fixture/application-preflight-browser
      :plan-registry plan-registry
      :browser-role :browser
      :required-plan-keys (set (keys plans))
      :optimistic? true
      :optimistic-htmx? true})))

(defn operation-assembly
  [operations]
  (let [browser (browser-assembly operations)
        capabilities
        (capability/operation-capabilities
         (get-in browser [:plan-registry :plans]))]
    (operation-preflight/require-operation-assembly!
     {:name :fixture/application-preflight-operations
      :browser-assembly browser
      :operation-capabilities capabilities
      :server-operations operations})))

(defn route-capabilities
  [operations]
  (into
   (sorted-map)
   (map
    (fn [operation]
      [operation
       (route-preflight/route-capability
        {:operation operation
         :method :post
         :path (str "/operations/" (namespace operation) "/" (name operation))
         :transports #{:htmx}})]))
   (keys operations)))

(defn execution-assembly
  [operations]
  (let [operation-assembly' (operation-assembly operations)
        route-assembly'
        (route-preflight/require-route-assembly!
         {:name :fixture/application-preflight-routes
          :operation-assembly operation-assembly'
          :route-capabilities (route-capabilities operations)})
        prepared-server
        (server/server
         {:principal-fn (fn [_] trusted-principal)
          :operations operations})]
    (execution-preflight/require-execution-assembly!
     {:name :fixture/application-preflight-execution
      :route-assembly route-assembly'
      :server prepared-server})))

(defn operation-acquisition-assembly
  ([]
   (operation-acquisition-assembly (standard-operations)))
  ([operations]
   (let [live-app (compiled-live)
         execution (execution-assembly operations)
         acquisition' (acquisition-assembly live-app)]
     (operation-acquisition/require-operation-acquisition-assembly!
      {:name :fixture/application-operation-acquisition
       :execution-assembly execution
       :acquisition-assembly acquisition'}))))

(defonce standard-operation-acquisition
  (delay
    (operation-acquisition-assembly)))

(defn- nested-browser-assembly
  [operation-acquisition-assembly]
  (get-in operation-acquisition-assembly
          [:execution-assembly
           :route-assembly
           :operation-assembly
           :browser-assembly]))

(defn record-artifact!
  ([operation-acquisition-assembly artifact-path]
   (record-artifact! operation-acquisition-assembly artifact-path nil))
  ([operation-acquisition-assembly artifact-path receipt-path]
   (spit artifact-path
         "console.log('application preflight fixture');\n"
         :encoding "UTF-8")
   (browser-build/record-generated-artifact!
    (nested-browser-assembly operation-acquisition-assembly)
    artifact-path
    (cond-> {}
      receipt-path
      (assoc :receipt-path receipt-path)))))

(defn application-options
  ([operation-acquisition-assembly artifact-path]
   (application-options operation-acquisition-assembly artifact-path nil))
  ([operation-acquisition-assembly artifact-path receipt-path]
   (cond->
    {:name :fixture/application
     :operation-acquisition-assembly operation-acquisition-assembly
     :browser-artifact-path artifact-path}
     receipt-path
     (assoc :browser-receipt-path receipt-path))))

(defn with-closed-application
  ([f]
   (with-closed-application {} f))
  ([{:keys [operations custom-receipt?]
     :or {custom-receipt? false}}
    f]
   (with-temp-dir
    (fn [dir]
      (let [operation-acquisition'
            (if operations
              (operation-acquisition-assembly operations)
              @standard-operation-acquisition)
            artifact-path (child-path dir "gesso-live.js")
            receipt-path (when custom-receipt?
                           (child-path dir "metadata/browser.edn"))]
        (record-artifact! operation-acquisition' artifact-path receipt-path)
        (let [options
              (application-options
               operation-acquisition'
               artifact-path
               receipt-path)
              report (application/check-application-assembly options)
              assembly (application/require-application-assembly! options)]
          (f {:dir dir
              :operation-acquisition-assembly operation-acquisition'
              :artifact-path artifact-path
              :receipt-path receipt-path
              :options options
              :report report
              :assembly assembly})))))))

;; Physical Hiccup request fixtures and custom-route application assembly.

(def rendered-basis
  {:tx-id 42
   :system-time "2026-09-08T00:00:00Z"})

(defn render-context
  [operations]
  (let [plans
        (into
         (sorted-map)
         (map (fn [[operation entry]]
                [operation (browser-plan entry)]))
         operations)]
    (ui/with-optimistic-operation-capabilities
     {:anti-forgery-token "application-preflight-token"}
     (capability/operation-capabilities plans))))

(defn rendered-operation-button
  [ctx operation path]
  (ui/post-button
   ctx
   {:to path
    :label (name operation)
    :choreo/op operation
    :optimistic-binding
    {:arguments {:fixture/op operation}
     :observed-basis rendered-basis}}))

(defn custom-route-capabilities
  [route-specs]
  (into
   (sorted-map)
   (map
    (fn [[route-id {:keys [operation method path transports]
                    :or {method :post
                         transports #{:htmx}}}]]
      [route-id
       (route-preflight/route-capability
        {:operation operation
         :method method
         :path path
         :transports transports})]))
   route-specs))

(defn- execution-assembly-with-routes
  [operations route-capabilities']
  (let [operation-assembly' (operation-assembly operations)
        route-assembly'
        (route-preflight/require-route-assembly!
         {:name :fixture/application-preflight-custom-routes
          :operation-assembly operation-assembly'
          :route-capabilities route-capabilities'})
        prepared-server
        (server/server
         {:principal-fn (fn [_] trusted-principal)
          :operations operations})]
    (execution-preflight/require-execution-assembly!
     {:name :fixture/application-preflight-custom-execution
      :route-assembly route-assembly'
      :server prepared-server})))

(defn- operation-acquisition-assembly-with-routes
  [operations route-capabilities']
  (let [live-app (compiled-live)
        execution (execution-assembly-with-routes operations route-capabilities')
        acquisition' (acquisition-assembly live-app)]
    (operation-acquisition/require-operation-acquisition-assembly!
     {:name :fixture/application-operation-acquisition-custom-routes
      :execution-assembly execution
      :acquisition-assembly acquisition'})))

(defn with-surfaced-application
  [config-or-surfaces f]
  (let [{:keys [operations route-capabilities rendered-surfaces custom-receipt?]
         :or {custom-receipt? false}}
        (if (and (map? config-or-surfaces)
                 (contains? config-or-surfaces :rendered-surfaces))
          config-or-surfaces
          {:operations (standard-operations)
           :route-capabilities (route-capabilities (standard-operations))
           :rendered-surfaces config-or-surfaces})]
    (with-temp-dir
     (fn [dir]
       (let [operation-acquisition'
             (operation-acquisition-assembly-with-routes
              operations
              route-capabilities)
             artifact-path (child-path dir "gesso-live.js")
             receipt-path (when custom-receipt?
                            (child-path dir "metadata/browser.edn"))]
         (record-artifact! operation-acquisition' artifact-path receipt-path)
         (let [options
               (cond->
                (application-options
                 operation-acquisition'
                 artifact-path
                 receipt-path)
                 (some? rendered-surfaces)
                 (assoc :rendered-surfaces rendered-surfaces))
               report (application/check-application-assembly options)]
           (f {:dir dir
               :operation-acquisition-assembly operation-acquisition'
               :artifact-path artifact-path
               :receipt-path receipt-path
               :options options
               :report report
               :assembly
               (when (application/valid? report)
                 (application/require-application-assembly! options))})))))))
