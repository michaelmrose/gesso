(ns gesso.live.application-preflight-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [com.biffweb.core :as biff]
   [gesso.core :as g]
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

;; =============================================================================
;; Helpers
;; =============================================================================

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

(defn- error-data
  [f]
  (try
    (f)
    nil
    (catch Throwable error
      (exception-info-data error))))

(defn- error-kind
  [f]
  (:error/kind (error-data f)))

(defn- error-kinds
  [report]
  (set (map :kind (:errors report))))

(defn- warning-kinds
  [report]
  (set (map :kind (:warnings report))))

(defn- delete-tree!
  [root]
  (doseq [file (reverse (file-seq root))]
    (Files/deleteIfExists (.toPath file))))

(defn- with-temp-dir
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

(defn- child-path
  [dir child]
  (str (.resolve (.toPath dir) child)))

(defn- allow?
  [_ctx _id]
  true)

(defn- html-response
  [node]
  {:status 200
   :headers {"content-type" "text/html; charset=utf-8"}
   :body node})

(defn- generic-query
  [_ctx id]
  {:fragment/id (str id)})

(defn- generic-render
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

(defn- acquisition-assembly
  [live-app]
  (acquisition/require-acquisition-assembly!
   {:name :fixture/live-acquisition
    :live-app live-app
    :realizations
    {:request-toolbar (realization live-app :request-toolbar)
     :request-list (realization live-app :request-list)}}))

(def trusted-principal
  (identity/principal "helper-1"))

(defn- trusted-operation
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

(defn- operation-assembly
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

(defn- route-capabilities
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

(defn- execution-assembly
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

(defn- operation-acquisition-assembly
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

(defonce ^:private standard-operation-acquisition
  (delay
    (operation-acquisition-assembly)))

(defn- nested-browser-assembly
  [operation-acquisition-assembly]
  (get-in operation-acquisition-assembly
          [:execution-assembly
           :route-assembly
           :operation-assembly
           :browser-assembly]))

(defn- record-artifact!
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

(defn- application-options
  ([operation-acquisition-assembly artifact-path]
   (application-options operation-acquisition-assembly artifact-path nil))
  ([operation-acquisition-assembly artifact-path receipt-path]
   (cond->
    {:name :fixture/application
     :operation-acquisition-assembly operation-acquisition-assembly
     :browser-artifact-path artifact-path}
     receipt-path
     (assoc :browser-receipt-path receipt-path))))

(defn- with-closed-application
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

;; =============================================================================
;; Canonical whole-application backbone closure
;; =============================================================================

(deftest successful-application-assembly-closes-runtime-backbone-but-keeps-affordance-obligation-open
  (with-closed-application
    (fn [{:keys [operation-acquisition-assembly report assembly]}]
      (let [explanation (application/explain assembly)
            obligations (application/open-obligations assembly)]
        (testing "the report and emitted physical product are current"
          (is (application/report? report))
          (is (application/valid? report))
          (is (application/application-assembly? assembly))
          (is (= [] (:errors report))))

        (testing "the whole-application affordance edge remains explicitly open"
          (is (= 1 (count obligations)))
          (is (= :static-affordance-closure-not-yet-modeled
                 (:kind (first obligations))))
          (is (= :rendered-affordance->semantic-operation
                 (:edge (first obligations))))
          (is (= :open (:status (first obligations))))
          (is (= obligations
                 (get-in report [:analysis :open-obligations])))
          (is (= obligations (:open-obligations explanation)))
          (is (= #{:static-affordance-closure-not-yet-modeled
                   :published-change-topics-not-consumed-by-live-app}
                 (warning-kinds report))))

        (testing "the guarantee is deliberately narrower than whole-application closure"
          (is (= :application-runtime-backbone-preflight-closed
                 (:guarantee explanation)))
          (is (not= :whole-application-preflight-closed
                    (:guarantee explanation))))

        (testing "the application assembly stores no duplicated derived operation registry"
          (is (= operation-acquisition-assembly
                 (:operation-acquisition-assembly assembly)))
          (is (= #{:gesso.live.application-preflight/type
                   :gesso.live.application-preflight/version
                   :name
                   :operation-acquisition-assembly
                   :browser-artifact-path
                   :browser-receipt-path}
                 (set (keys assembly))))
          (is (not (contains? assembly :operations)))
          (is (not (contains? assembly :open-obligations)))
          (is (not (contains? assembly :trusted-assumptions))))))))

(deftest operation-explanation-reaches-physical-command-and-authoritative-acquisition-routes
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [claim (application/explain-operation assembly :request/claim)
            reassign (application/explain-operation assembly :request/reassign)
            cancel (application/explain-operation assembly :request/cancel)]
        (testing "claim closes browser/server/publication/acquisition chain"
          (is (= :request/claim (:operation claim)))
          (is (= :application-runtime-backbone-preflight-closed
                 (:guarantee claim)))
          (is (= :htmx (:command-transport claim)))
          (is (= [{:route-id :request/claim
                   :method :post
                   :path "/operations/request/claim"
                   :required-transport :htmx}]
                 (:routes claim)))
          (is (= #{:request} (:published-change-topics claim)))
          (is (= #{:request-toolbar :request-list}
                 (:affected-scopes claim)))
          (is (= #{:request-toolbar :request-list}
                 (:affected-fragments claim)))
          (is (= #{:request-toolbar :request-list}
                 (set (keys (:authoritative-acquisitions claim)))))
          (is (= {:method :get
                  :path "/fragments/request-list"
                  :boundary :gesso.live.core/render-fragment-response}
                 (get-in claim
                         [:authoritative-acquisitions
                          :request-list
                          :fragment-route])))
          (is (= {:method :get
                  :path "/streams/request-list"
                  :boundary :gesso.live.transport.sse/start-fragment-stream!}
                 (get-in claim
                         [:authoritative-acquisitions
                          :request-list
                          :stream-route]))))

        (testing "semantic scope impact remains richer than managed fragment impact"
          (is (= #{:request-toolbar :request-list :audit-scope}
                 (:affected-scopes reassign)))
          (is (= #{:request-toolbar :request-list}
                 (:affected-fragments reassign))))

        (testing "explicit no-publication operation has no authoritative acquisition"
          (is (= #{} (:published-change-topics cancel)))
          (is (= #{} (:affected-scopes cancel)))
          (is (= #{} (:affected-fragments cancel)))
          (is (= {} (:authoritative-acquisitions cancel))))))))

;; =============================================================================
;; Physical currentness
;; =============================================================================

(deftest changing-generated-artifact-invalidates-report-assembly-and-inspection
  (with-closed-application
    (fn [{:keys [artifact-path report assembly]}]
      (is (application/report? report))
      (is (application/application-assembly? assembly))

      (spit artifact-path
            "console.log('tampered after application assembly');\n"
            :encoding "UTF-8")

      (is (not (application/report? report)))
      (is (not (application/valid? report)))
      (is (not (application/application-assembly? assembly)))
      (is (= :invalid-application-input
             (error-kind #(application/open-obligations assembly))))
      (is (= :invalid-application-assembly
             (error-kind #(application/explain-operation
                           assembly
                           :request/claim))))
      (is (= :unrecognized-value
             (error-kind #(application/explain assembly)))))))

(deftest changing-default-receipt-invalidates-currentness-without-changing-artifact
  (with-closed-application
    (fn [{:keys [artifact-path report assembly]}]
      (let [receipt-path (browser-build/receipt-path artifact-path)]
        (spit receipt-path "{:tampered true}\n" :encoding "UTF-8")
        (is (not (application/report? report)))
        (is (not (application/application-assembly? assembly)))
        (is (= :browser-artifact-verification-failed
               (first (error-kinds
                       (application/check-application-assembly
                        {:operation-acquisition-assembly
                         (:operation-acquisition-assembly assembly)
                         :browser-artifact-path artifact-path})))))))))

(deftest custom-receipt-path-is-a-first-class-currentness-boundary
  (with-closed-application
    {:custom-receipt? true}
    (fn [{:keys [artifact-path receipt-path report assembly]}]
      (let [explanation (application/explain assembly)
            default-path (browser-build/receipt-path artifact-path)]
        (is (application/report? report))
        (is (application/application-assembly? assembly))
        (is (= receipt-path (:browser-receipt-path assembly)))
        (is (= receipt-path
               (get-in explanation [:browser-artifact :receipt-path])))
        (is (.isFile (java.io.File. receipt-path)))
        (is (not (.exists (java.io.File. default-path))))

        (spit receipt-path "{:tampered :custom-receipt}\n" :encoding "UTF-8")
        (is (not (application/report? report)))
        (is (not (application/application-assembly? assembly)))))))

(deftest missing-physical-artifact-fails-with-derived-semantic-analysis-still-visible
  (with-temp-dir
    (fn [dir]
      (let [operation-acquisition' @standard-operation-acquisition
            artifact-path (child-path dir "missing.js")
            report
            (application/check-application-assembly
             (application-options operation-acquisition' artifact-path))]
        (is (application/report? report))
        (is (not (application/valid? report)))
        (is (= #{:browser-artifact-verification-failed}
               (error-kinds report)))
        (is (= #{:request/claim :request/reassign :request/cancel}
               (set (keys (get-in report [:analysis :operations])))))
        (is (= :application-backbone-preflight-failed
               (error-kind
                #(application/require-application-assembly!
                  (application-options
                   operation-acquisition'
                   artifact-path)))))))))

;; =============================================================================
;; Closed report and assembly tamper resistance
;; =============================================================================

(deftest derived-analysis-tampering-invalidates-recognized-report
  (with-closed-application
    (fn [{:keys [report]}]
      (doseq [tampered
              [(assoc-in report
                         [:analysis :operations :request/claim :affected-fragments]
                         #{:request-list})
               (assoc-in report
                         [:analysis :operations :request/claim :published-change-topics]
                         #{:audit})
               (assoc-in report
                         [:analysis :open-obligations]
                         [])
               (assoc-in report
                         [:analysis :trusted-assumptions]
                         #{:invented-assumption})
               (assoc report :warnings [])]]
        (is (not (application/report? tampered)))
        (is (not (application/valid? tampered)))))))

(deftest failed-report-cannot-be-forged-positive-by-clearing-errors
  (with-temp-dir
    (fn [dir]
      (let [operation-acquisition' @standard-operation-acquisition
            artifact-path (child-path dir "missing.js")
            report
            (application/check-application-assembly
             (application-options operation-acquisition' artifact-path))
            forged (assoc report :valid? true :errors [])]
        (is (application/report? report))
        (is (not (application/valid? report)))
        (is (not (application/report? forged)))
        (is (not (application/valid? forged)))))))

(deftest closed-application-assembly-rejects-duplicated-derived-fields
  (with-closed-application
    (fn [{:keys [assembly]}]
      (doseq [tampered
              [(assoc assembly
                      :operations
                      (:operations (application/explain assembly)))
               (assoc assembly
                      :open-obligations
                      (application/open-obligations assembly))
               (assoc assembly
                      :trusted-assumptions
                      (:trusted-assumptions (application/explain assembly)))]]
        (is (not (application/application-assembly? tampered)))
        (is (= :invalid-application-input
               (error-kind #(application/open-obligations tampered))))))))

(deftest nested-operation-acquisition-tampering-invalidates-report-and-assembly
  (with-closed-application
    (fn [{:keys [report assembly]}]
      (let [tampered-operation-acquisition
            (assoc (:operation-acquisition-assembly assembly)
                   :name
                   "not-a-keyword")
            tampered-assembly
            (assoc assembly
                   :operation-acquisition-assembly
                   tampered-operation-acquisition)
            tampered-report
            (assoc-in report
                      [:analysis :operation-acquisition-assembly]
                      tampered-operation-acquisition)]
        (is (not (operation-acquisition/operation-acquisition-assembly?
                  tampered-operation-acquisition)))
        (is (not (application/application-assembly? tampered-assembly)))
        (is (not (application/report? tampered-report)))
        (is (= :invalid-application-input
               (error-kind
                #(application/open-obligations tampered-assembly))))))))

(deftest nested-execution-or-acquisition-tampering-propagates-through-unified-assembly
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [operation-acquisition'
            (:operation-acquisition-assembly assembly)
            bad-execution
            (assoc (:execution-assembly operation-acquisition')
                   :name
                   "not-a-keyword")
            bad-acquisition
            (assoc (:acquisition-assembly operation-acquisition')
                   :name
                   "not-a-keyword")]
        (doseq [bad-operation-acquisition
                [(assoc operation-acquisition'
                        :execution-assembly bad-execution)
                 (assoc operation-acquisition'
                        :acquisition-assembly bad-acquisition)]]
          (is (not (operation-acquisition/operation-acquisition-assembly?
                    bad-operation-acquisition)))
          (is (= #{:invalid-operation-acquisition-assembly}
                 (error-kinds
                  (application/check-application-assembly
                   {:operation-acquisition-assembly bad-operation-acquisition
                    :browser-artifact-path (:browser-artifact-path assembly)})))))))))

;; =============================================================================
;; Explanation, obligations, warnings, and option boundaries
;; =============================================================================

(deftest open-obligations-is-identical-before-and-after-physical-application-assembly
  (with-closed-application
    (fn [{:keys [operation-acquisition-assembly assembly]}]
      (is (= (application/open-obligations operation-acquisition-assembly)
             (application/open-obligations assembly)))
      (is (= :static-affordance-closure-not-yet-modeled
             (:kind
              (first
               (application/open-obligations
                operation-acquisition-assembly))))))))

(deftest trusted-assumptions-remain-separate-from-open-obligations-and-guarantee
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [explanation (application/explain assembly)
            assumptions (:trusted-assumptions explanation)
            obligations (:open-obligations explanation)]
        (is (set? assumptions))
        (is (contains? assumptions
                       :application-publication-declarations-match-model-publication))
        (is (contains? assumptions
                       :trusted-command-route-declarations-match-installed-handlers))
        (is (contains? assumptions
                       :trusted-acquisition-route-declarations-match-installed-handlers))
        (is (= :application-runtime-backbone-preflight-closed
               (:guarantee explanation)))
        (is (= :open (:status (first obligations))))
        (is (not (contains? assumptions
                            :static-affordance-closure-not-yet-modeled)))))))

(deftest unconsumed-publication-warning-is-lifted-without-becoming-an-open-acquisition-error
  (with-closed-application
    (fn [{:keys [report assembly]}]
      (let [warning
            (first
             (filter
              #(= :published-change-topics-not-consumed-by-live-app
                  (:kind %))
              (:warnings report)))]
        (is (application/valid? report))
        (is (= #{:published-change-topics-not-consumed-by-live-app
                 :static-affordance-closure-not-yet-modeled}
               (warning-kinds report)))
        (is (= {:request/reassign #{:external/audit}}
               (:topics-by-operation warning)))
        (is (= :application-runtime-backbone-preflight-closed
               (:guarantee (application/explain assembly))))))))

(deftest unknown-operation-explanation-fails-with-closed-available-set
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [data
            (error-data
             #(application/explain-operation assembly :request/unknown))]
        (is (= :unknown-operation (:error/kind data)))
        (is (= :request/unknown (:operation data)))
        (is (= #{:request/claim :request/reassign :request/cancel}
               (:available-operations data)))))))

(deftest malformed-options-fail-before-physical-verification
  (let [operation-acquisition' @standard-operation-acquisition]
    (is (= :invalid-shape
           (error-kind #(application/check-application-assembly nil))))
    (is (= :missing-option
           (error-kind
            #(application/check-application-assembly
              {:browser-artifact-path "/tmp/unused.js"}))))
    (is (= :missing-option
           (error-kind
            #(application/check-application-assembly
              {:operation-acquisition-assembly operation-acquisition'}))))
    (is (= :unknown-option-keys
           (error-kind
            #(application/check-application-assembly
              {:operation-acquisition-assembly operation-acquisition'
               :browser-artifact-path "/tmp/unused.js"
               :surprise true}))))
    (is (= :invalid-name
           (error-kind
            #(application/check-application-assembly
              {:name "not-a-keyword"
               :operation-acquisition-assembly operation-acquisition'
               :browser-artifact-path "/tmp/unused.js"}))))
    (is (= :invalid-browser-artifact-path
           (error-kind
            #(application/check-application-assembly
              {:operation-acquisition-assembly operation-acquisition'
               :browser-artifact-path "  "}))))
    (is (= :invalid-browser-receipt-path
           (error-kind
            #(application/check-application-assembly
              {:operation-acquisition-assembly operation-acquisition'
               :browser-artifact-path "/tmp/unused.js"
               :browser-receipt-path "  "}))))))


;; =============================================================================
;; v627 rendered-surface -> affordance -> trusted-route closure
;; =============================================================================

(def rendered-basis
  {:tx-id 42
   :system-time "2026-09-08T00:00:00Z"})

(defn- render-context
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

(defn- rendered-operation-button
  [ctx operation path]
  (ui/post-button
   ctx
   {:to path
    :label (name operation)
    :choreo/op operation
    :optimistic-binding
    {:arguments {:fixture/op operation}
     :observed-basis rendered-basis}}))

(defn- custom-route-capabilities
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

(defn- with-surfaced-application
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

(deftest anonymous-post-to-parameterized-semantic-route-is-rejected-before-application-assembly
  (let [operations
        {:request/claim
         (get (standard-operations) :request/claim)}
        routes
        (custom-route-capabilities
         {:humanhelp/claim
          {:operation :request/claim
           :path "/app/requests/:request-id/claim"
           :transports #{:htmx}}})
        concrete-path
        "/app/requests/01a0702a-302d-7604-a880-7b421af742a0/claim"
        surfaces
        {:request-board
         [:button {:hx-post concrete-path} "Claim"]}]
    (with-surfaced-application
      {:operations operations
       :route-capabilities routes
       :rendered-surfaces surfaces}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (not (application/valid? report)))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (let [error (first (:errors report))]
          (is (= :request-board (:surface error)))
          (is (= [] (:render-path error)))
          (is (= :post (:method error)))
          (is (= concrete-path (:path error)))
          (is (= #{:request/claim} (:candidate-operations error)))
          (is (= [{:operation :request/claim
                   :route-id :humanhelp/claim
                   :method :post
                   :route-template "/app/requests/:request-id/claim"
                   :required-transport :htmx}]
                 (:matching-routes error))))))))

(deftest dynamic-pre-browser-boundary-rejects-silent-semantic-route-downgrade-before-rendering
  (let [rendered
        [:main
         [:button {:hx-post "/operations/request/claim"} "Claim"]]
        calls (atom [])]
    (with-closed-application
      (fn [{:keys [assembly]}]
        (let [report
              (application/check-rendered-surface
               assembly
               :request-board
               rendered)
              data
              (error-data
               #(application/checked-rendered-response!
                 assembly
                 :request-board
                 (fn [node]
                   (swap! calls conj node)
                   {:status 200 :body node})
                 rendered))]
          (is (application/rendered-surface-report? report))
          (is (not (application/rendered-surface-valid? report)))
          (is (= #{:rendered-semantic-route-without-choreo-operation}
                 (error-kinds report)))
          (is (= :rendered-surface-preflight-failed (:error/kind data)))
          (is (= #{:rendered-semantic-route-without-choreo-operation}
                 (error-kinds (:preflight data))))
          (is (empty? @calls)))))))

(deftest semantic-route-identity-guard-does-not-ban-ordinary-htmx-or-canonical-choreo-affordances
  (let [operations (standard-operations)
        ctx (render-context operations)
        ordinary
        [:main
         [:button {:hx-post "/ordinary/save"} "Save"]]
        canonical
        [:main
         (rendered-operation-button
          ctx
          :request/claim
          "/operations/request/claim")]]
    (with-closed-application
      (fn [{:keys [assembly]}]
        (let [ordinary-report
              (application/check-rendered-surface
               assembly
               :ordinary-form
               ordinary)
              canonical-report
              (application/check-rendered-surface
               assembly
               :request-board
               canonical)]
          (is (application/rendered-surface-valid? ordinary-report))
          (is (empty? (:errors ordinary-report)))
          (is (application/rendered-surface-valid? canonical-report))
          (is (empty? (:errors canonical-report)))
          (is (= :request/claim
                 (get-in canonical-report
                         [:analysis :affordances 0 :operation]))))))))

(deftest anonymous-post-does-not-collide-with-semantic-route-of-a-different-http-method
  (let [operations
        {:request/claim
         (get (standard-operations) :request/claim)}
        routes
        (custom-route-capabilities
         {:request/claim-put
          {:operation :request/claim
           :method :put
           :path "/requests/:request-id/claim"
           :transports #{:htmx}}})]
    (with-surfaced-application
      {:operations operations
       :route-capabilities routes
       :rendered-surfaces
       {:request-board
        [:button {:hx-post "/requests/request-1/claim"} "Ordinary POST"]}}
      (fn [{:keys [report assembly]}]
        (is (application/valid? report))
        (is (application/application-assembly? assembly))
        (is (empty? (:errors report)))))))

;; =============================================================================
;; v717 adversarial regression: explicit non-Choreo semantic route invocations
;; =============================================================================
;; These checks exercise the public assembly and response boundaries, rather
;; than asserting only the private Hiccup scanner's behavior. They cover the
;; explicit request coordinates enumerated by v716. Implicit native form actions
;; and JavaScript request construction remain separate unclosed cases; neither
;; is certified by these tests. Submitter overrides are covered below (v719).

(deftest anonymous-htmx-methods-cannot-invoke-matching-semantic-routes
  (let [methods [:get :post :put :patch :delete]
        path "/requests/request-42/claim?from=board#pending"
        operations
        {:request/claim (get (standard-operations) :request/claim)}
        routes
        (custom-route-capabilities
         (into {}
               (map
                (fn [method]
                  [(keyword "fixture" (str "claim-" (name method)))
                   {:operation :request/claim
                    :method method
                    :path "/requests/:request-id/claim"
                    :transports #{:htmx}}])
                methods)))
        rendered
        (into [:main]
              (map
               (fn [method]
                 [:button {(keyword (str "hx-" (name method))) path}
                  (name method)])
               methods))]
    (with-surfaced-application
      {:operations operations
       :route-capabilities routes
       :rendered-surfaces {:request-board rendered}}
      (fn [{:keys [report assembly]}]
        (is (application/report? report))
        (is (nil? assembly))
        (is (not (application/valid? report)))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (is (= (count methods) (count (:errors report))))
        (is (= (mapv
                (fn [index method]
                  {:surface :request-board
                   :render-path [index]
                   :method method
                   :path path
                   :request-source (keyword (str "hx-" (name method)))
                   :candidate-operations #{:request/claim}})
                (range 1 (inc (count methods)))
                methods)
               (mapv
                #(select-keys % [:surface :render-path :method :path
                                 :request-source :candidate-operations])
                (:errors report))))
        (is (= (set methods)
               (set (map (comp :method first :matching-routes)
                         (:errors report)))))))))

(deftest anonymous-native-post-forms-are-checked-with-nested-hiccup-tags
  (let [claim "/operations/request/claim"
        queried (str claim "?from=board#confirm")
        rendered
        [:main
         [:form#claim.panel {:method "PoSt" :action claim}
          [:button {:type "submit"} "Claim"]]
         [:section
          ["form" {:method :POST :action queried}
           [:input {:type "submit" :value "Claim again"}]]]
         [:form {:method :post :action "/unrelated/save"}
          [:button {:type "submit"} "Save"]]
         [:form {:method :get :action claim}
          [:button {:type "submit"} "Read"]]
         [:form {:method "POST"}
          [:button {:type "submit"} "Implicit action"]]]]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (application/report? report))
        (is (nil? assembly))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (is (= [{:surface :request-board
                 :render-path [1]
                 :method :post
                 :path claim
                 :request-source :native-form}
                {:surface :request-board
                 :render-path [2 1]
                 :method :post
                 :path queried
                 :request-source :native-form}]
               (mapv
                #(select-keys % [:surface :render-path :method :path
                                 :request-source])
                (:errors report))))
        (is (every? #(= #{:request/claim} (:candidate-operations %))
                    (:errors report)))))))

(deftest anonymous-physical-method-is-matched-not-just-the-semantic-url
  (let [operations
        {:request/claim (get (standard-operations) :request/claim)}
        routes
        (custom-route-capabilities
         {:fixture/claim-put
          {:operation :request/claim
           :method :put
           :path "/requests/:request-id/claim"
           :transports #{:htmx}}})
        rendered
        [:main
         [:button {:hx-post "/requests/42/claim"} "Nonmatching POST"]
         [:form {:method "POST" :action "/requests/42/claim"}
          [:button {:type "submit"} "Nonmatching form"]]
         [:button {:hx-get "/requests/42/claim"} "Nonmatching GET"]
         [:button {:hx-put "/requests/42/claim"} "Matching PUT"]]]
    (with-surfaced-application
      {:operations operations
       :route-capabilities routes
       :rendered-surfaces {:request-board rendered}}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (is (= 1 (count (:errors report))))
        (is (= {:method :put
                :request-source :hx-put
                :render-path [4]
                :candidate-operations #{:request/claim}}
               (select-keys (first (:errors report))
                            [:method :request-source :render-path
                             :candidate-operations])))))))

(deftest native-post-and-anonymous-htmx-collisions-block-renderer
  (with-closed-application
    (fn [{:keys [assembly]}]
      (doseq [[case-name rendered expected-source]
              [[:native-post
                [:form {:method "POST" :action "/operations/request/claim"}
                 [:button {:type "submit"} "Claim"]]
                :native-form]
               [:anonymous-htmx
                [:button {:hx-post "/operations/request/claim"} "Claim"]
                :hx-post]]]
        (testing (name case-name)
          (let [calls (atom 0)
                report (application/check-rendered-surface
                        assembly :request-board rendered)
                data (error-data
                      #(application/checked-rendered-response!
                        assembly :request-board
                        (fn [_]
                          (swap! calls inc)
                          {:status 200})
                        rendered))]
            (is (application/rendered-surface-report? report))
            (is (not (application/rendered-surface-valid? report)))
            (is (= #{:rendered-semantic-route-without-choreo-operation}
                   (error-kinds report)))
            (is (= expected-source (:request-source (first (:errors report)))))
            (is (= :rendered-surface-preflight-failed (:error/kind data)))
            (is (= #{:rendered-semantic-route-without-choreo-operation}
                   (error-kinds (:preflight data))))
            (is (zero? @calls))))))))

(deftest ordinary-physical-requests-and-canonical-choreo-remain-admitted
  (let [ctx (render-context (standard-operations))
        canonical (rendered-operation-button
                   ctx :request/claim "/operations/request/claim")
        rendered
        [:main
         canonical
         [:button {:hx-get "/ordinary/list"} "List"]
         [:button {:hx-post "/ordinary/save"} "Save"]
         [:button {:hx-put "/ordinary/replace"} "Replace"]
         [:button {:hx-patch "/ordinary/edit"} "Edit"]
         [:button {:hx-delete "/ordinary/remove"} "Remove"]
         [:form {:method :post :action "/ordinary/form"}
          [:button {:type "submit"} "Submit"]]
         [:form {:method "GET" :action "/operations/request/claim"}
          [:button {:type "submit"} "Nonmatching native GET"]]]]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (application/valid? report))
        (is (application/application-assembly? assembly))
        (is (empty? (:errors report)))
        (is (= :closed-relative-to-supplied-rendered-surfaces
               (get-in report [:analysis :affordance-closure :status])))
        (is (= [:request/claim]
               (mapv :operation (get-in report [:analysis :affordances]))))))))

;; =============================================================================
;; v719 adversarial regression: HTML submitter overrides (v718 implementation)
;; =============================================================================
;; These test actual application assembly and the checked response boundary. The
;; source derives the effective method/action from the owning HTML form and the
;; submitter's formaction/formmethod. These tests deliberately do not claim that
;; implicit document URLs, JavaScript submissions, or unsupplied DOM are checked.

(deftest anonymous-inline-submitters-cannot-override-to-semantic-post-route
  (let [claim "/operations/request/claim"
        queried (str claim "?from=inline#after")
        rendered
        [:main
         [:form {:method "POST" :action "/ordinary/save"}
          [:button {:formaction claim} "Default submit button"]]
         [:form {:method :get :action "/ordinary/view"}
          [:input {:type "submit" :formmethod "PoSt" :formaction queried}]]
         [:form {:method "POST" :action "/ordinary/save"}
          [:input {:type "image" :formaction claim}]]
         [:form {:method "GET" :action claim}
          [:button {:formmethod :post} "Inherited action"]]]]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (application/report? report))
        (is (nil? assembly))
        (is (not (application/valid? report)))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (is (= [{:method :post
                 :path claim
                 :render-path [1 2]
                 :form-render-path [1]
                 :submitter-overrides #{:formaction}}
                {:method :post
                 :path queried
                 :render-path [2 2]
                 :form-render-path [2]
                 :submitter-overrides #{:formaction :formmethod}}
                {:method :post
                 :path claim
                 :render-path [3 2]
                 :form-render-path [3]
                 :submitter-overrides #{:formaction}}
                {:method :post
                 :path claim
                 :render-path [4 2]
                 :form-render-path [4]
                 :submitter-overrides #{:formmethod}}]
               (mapv #(select-keys % [:method :path :render-path
                                       :form-render-path :submitter-overrides])
                     (:errors report))))
        (is (every? #(= :native-submitter (:request-source %))
                    (:errors report)))
        (is (every? #(= #{:request/claim} (:candidate-operations %))
                    (:errors report)))))))

(deftest detached-submitters-resolve-explicit-form-owner-not-dom-parent
  (let [claim "/operations/request/claim"
        rendered
        [:main
         [:form#claim-form {:method "GET" :action claim}]
         [:form {:id "ordinary-form" :method "POST" :action "/ordinary/save"}]
         [:form {:id "other-parent" :method "GET" :action "/ordinary/parent"}
          [:button {:form "claim-form" :formmethod "POST"}
           "Use external owner"]]
         [:input {:type "submit" :form "ordinary-form"
                  :formaction claim :value "Detached submit"}]]]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (is (= [{:request-source :native-submitter
                 :method :post
                 :path claim
                 :render-path [3 2]
                 :form-render-path [1]
                 :submitter-overrides #{:formmethod}}
                {:request-source :native-submitter
                 :method :post
                 :path claim
                 :render-path [4]
                 :form-render-path [2]
                 :submitter-overrides #{:formaction}}]
               (mapv #(select-keys % [:request-source :method :path
                                       :render-path :form-render-path
                                       :submitter-overrides])
                     (:errors report))))))))

(deftest submitter-check-is-method-sensitive-for-parameterized-semantic-routes
  (let [operations
        {:request/claim (get (standard-operations) :request/claim)}
        routes
        (custom-route-capabilities
         {:fixture/claim-get
          {:operation :request/claim
           :method :get
           :path "/requests/:request-id/claim"
           :transports #{:htmx}}})
        concrete "/requests/request-42/claim?from=submitter#section"
        rendered
        [:main
         [:form {:method :post :action "/ordinary/save"}
          [:button {:formaction concrete :formmethod "GET"} "Matching GET"]]
         [:form {:method :post :action "/ordinary/save"}
          [:button {:formaction concrete} "Nonmatching POST"]]
         [:form {:method :get :action "/ordinary/read"}
          [:button {:formaction concrete :formmethod "dialog"} "Dialog"]]]]
    (with-surfaced-application
      {:operations operations
       :route-capabilities routes
       :rendered-surfaces {:request-board rendered}}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (is (= 1 (count (:errors report))))
        (is (= {:request-source :native-submitter
                :method :get
                :path concrete
                :render-path [1 2]
                :form-render-path [1]
                :submitter-overrides #{:formaction :formmethod}
                :candidate-operations #{:request/claim}}
               (select-keys (first (:errors report))
                            [:request-source :method :path :render-path
                             :form-render-path :submitter-overrides
                             :candidate-operations])))))))

(deftest non-submitting-controls-and-unowned-submitters-remain-admitted
  (let [claim "/operations/request/claim"
        rendered
        [:main
         [:form {:method :post :action "/ordinary/save"}
          [:button {:type "button" :formaction claim} "Ordinary button"]
          [:button {:type :reset :formaction claim} "Reset"]
          [:input {:type "button" :formaction claim}]
          [:input {:type "text" :formaction claim}]
          [:button {:type "submit" :formaction "/ordinary/submit"} "Submit"]]
         [:form {:method :get :action "/ordinary/lookup"}
          [:button {:type "submit" :formaction claim} "GET only"]]
         [:form {:method :dialog :action claim}
          [:button {:formmethod :dialog} "Dialog only"]]
         [:button {:type :submit :form "missing" :formaction claim}
          "Unknown form owner"]
         [:input {:type "submit" :formaction claim}]
         [:button {:hx-post "/ordinary/htmx"} "Ordinary HTMX"]]]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (application/valid? report))
        (is (application/application-assembly? assembly))
        (is (empty? (:errors report)))))))

(deftest submitter-override-blocks-dynamic-renderer-before-html-delivery
  (with-closed-application
    (fn [{:keys [assembly]}]
      (doseq [[scenario rendered expected-overrides]
              [[:action-override
                [:form {:method :post :action "/ordinary/save"}
                 [:button {:formaction "/operations/request/claim"} "Claim"]]
                #{:formaction}]
               [:method-override
                [:form {:method :get :action "/operations/request/claim"}
                 [:button {:formmethod :post} "Claim"]]
                #{:formmethod}]
               [:detached-override
                [:main
                 [:form#claim {:method :get
                               :action "/operations/request/claim"}]
                 [:input {:type "submit" :form "claim" :formmethod :post}]]
                #{:formmethod}]]]
        (testing (name scenario)
          (let [calls (atom 0)
                report (application/check-rendered-surface
                        assembly :request-board rendered)
                data (error-data
                      #(application/checked-rendered-response!
                        assembly :request-board
                        (fn [_]
                          (swap! calls inc)
                          {:status 200})
                        rendered))]
            (is (application/rendered-surface-report? report))
            (is (not (application/rendered-surface-valid? report)))
            (is (= #{:rendered-semantic-route-without-choreo-operation}
                   (error-kinds report)))
            (is (= :native-submitter
                   (:request-source (first (:errors report)))))
            (is (= expected-overrides
                   (:submitter-overrides (first (:errors report)))))
            (is (= :rendered-surface-preflight-failed (:error/kind data)))
            (is (= #{:rendered-semantic-route-without-choreo-operation}
                   (error-kinds (:preflight data))))
            (is (zero? @calls))))))))

(deftest canonical-choreo-and-ordinary-submitters-coexist-on-valid-surface
  (let [ctx (render-context (standard-operations))
        canonical (rendered-operation-button
                   ctx :request/claim "/operations/request/claim")
        rendered
        [:main
         canonical
         [:form {:id "ordinary" :method :post :action "/ordinary/save"}
          [:button {:formaction "/ordinary/alternate"} "Alternate save"]
          [:input {:type "submit" :formmethod :get
                   :formaction "/operations/request/claim"}]]
         [:button {:form "ordinary" :formaction "/ordinary/detached"}
          "Detached save"]]]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (application/valid? report))
        (is (application/application-assembly? assembly))
        (is (empty? (:errors report)))
        (is (= [:request/claim]
               (mapv :operation (get-in report [:analysis :affordances]))))))))

;; =============================================================================
;; v721 adversarial regression: Hiccup forms without attrs and HTML button types
;; =============================================================================
;; v720 closes two scanner omissions: a form ID supplied through Hiccup #id
;; shorthand must be usable as a detached submitter's owner even if the form
;; has no attrs map; and invalid button type values have HTML's Submit default.
;; These checks use application assembly and the checked rendered boundary.
;; They do not claim to inspect implicit document actions or unsupplied DOM.

(deftest unadorned-hiccup-form-ids-are-resolved-for-detached-submitters
  (let [claim "/operations/request/claim"
        rendered
        [:main
         [:form#first-owner]
         [:button {:form "first-owner" :formmethod "POST"
                   :formaction claim}
          "First"]
         ["form#second-owner" [:p "Unadorned string tag"]]
         [:section
          [:input {:type "submit" :form "second-owner"
                   :formmethod :post :formaction claim}]]
         [:form#third-owner
          [:button {:formmethod "post" :formaction claim} "Inline"]]]]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (application/report? report))
        (is (nil? assembly))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (is (= [{:request-source :native-submitter
                 :method :post
                 :path claim
                 :render-path [2]
                 :form-render-path [1]
                 :submitter-overrides #{:formaction :formmethod}}
                {:request-source :native-submitter
                 :method :post
                 :path claim
                 :render-path [4 1]
                 :form-render-path [3]
                 :submitter-overrides #{:formaction :formmethod}}
                {:request-source :native-submitter
                 :method :post
                 :path claim
                 :render-path [5 1]
                 :form-render-path [5]
                 :submitter-overrides #{:formaction :formmethod}}]
               (mapv #(select-keys % [:request-source :method :path
                                       :render-path :form-render-path
                                       :submitter-overrides])
                     (:errors report))))
        (is (every? #(= #{:request/claim} (:candidate-operations %))
                    (:errors report)))))))

(deftest invalid-button-types-default-to-submission-on-semantic-post-routes
  (let [claim "/operations/request/claim"
        cases [["" :empty]
               ["unknown" :unknown]
               ["SUBMITT" :misspelled]
               [:other :keyword]
               [42 :non-text]]
        rendered
        (into [:main
               [:form {:method :post :action "/ordinary/save"}
                [:button {:type :reset :formaction claim} "Reset"]
                [:button {:type :button :formaction claim} "Non-submit"]]]
              (map (fn [[button-type label]]
                     [:form {:method :post :action "/ordinary/save"}
                      [:button {:type button-type :formaction claim}
                       (name label)]])
                   cases))]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (not (application/valid? report)))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (is (= (count cases) (count (:errors report))))
        (is (= (mapv (fn [index]
                       {:request-source :native-submitter
                        :method :post
                        :path claim
                        :render-path [(+ index 2) 2]
                        :form-render-path [(+ index 2)]
                        :submitter-overrides #{:formaction}})
                     (range (count cases)))
               (mapv #(select-keys % [:request-source :method :path
                                       :render-path :form-render-path
                                       :submitter-overrides])
                     (:errors report))))))))

(deftest default-get-and-nonsubmit-controls-do-not-create-false-collisions
  (let [claim "/operations/request/claim"
        rendered
        [:main
         [:form#default-get
          [:button {:formaction claim} "GET does not match semantic POST"]]
         [:button {:form "default-get" :formaction claim}
          "Detached GET does not match"]
         [:form#ordinary
          [:button {:formmethod "post" :formaction "/ordinary/save"}
           "Ordinary POST"]]
         [:form {:method :post :action "/ordinary/save"}
          [:button {:type :reset :formaction claim} "Reset"]
          [:button {:type "BuTtOn" :formaction claim} "Button"]
          [:input {:type :text :formaction claim}]]
         [:button {:form "unknown" :type "invalid" :formaction claim}
          "No form owner"]]
        canonical
        (rendered-operation-button
         (render-context (standard-operations))
         :request/claim claim)]
    (with-surfaced-application
      {:request-board [:section canonical rendered]}
      (fn [{:keys [report assembly]}]
        (is (application/report? report))
        (is (application/valid? report))
        (is (application/application-assembly? assembly))
        (is (empty? (:errors report)))
        (is (= [:request/claim]
               (mapv :operation (get-in report [:analysis :affordances]))))))))

(deftest hiccup-owner-and-invalid-type-reject-before-renderer-executes
  (with-closed-application
    (fn [{:keys [assembly]}]
      (doseq [[scenario rendered expected-path expected-form-path]
              [[:unadorned-owner
                [:main
                 [:form#owner]
                 [:button {:form "owner" :formmethod "POST"
                           :formaction "/operations/request/claim"} "Claim"]]
                [2] [1]]
               [:invalid-button-type
                [:main
                 [:form {:method :post :action "/ordinary/save"}
                  [:button {:type "unrecognized"
                            :formaction "/operations/request/claim"} "Claim"]]]
                [1 2] [1]]]]
        (testing (name scenario)
          (let [render-calls (atom 0)
                report (application/check-rendered-surface
                        assembly :request-board rendered)
                data (error-data
                      #(application/checked-rendered-response!
                        assembly :request-board
                        (fn [_]
                          (swap! render-calls inc)
                          {:status 200 :body "must-not-render"})
                        rendered))
                error (first (:errors report))]
            (is (application/rendered-surface-report? report))
            (is (not (application/rendered-surface-valid? report)))
            (is (= #{:rendered-semantic-route-without-choreo-operation}
                   (error-kinds report)))
            (is (= :native-submitter (:request-source error)))
            (is (= expected-path (:render-path error)))
            (is (= expected-form-path (:form-render-path error)))
            (is (= :rendered-surface-preflight-failed (:error/kind data)))
            (is (= #{:rendered-semantic-route-without-choreo-operation}
                   (error-kinds (:preflight data))))
            (is (zero? @render-calls))))))))

;; =============================================================================
;; v723 adversarial regression: string-keyed request attrs and duplicate attrs
;; =============================================================================
;; Rum accepts both string and keyword HTML attribute keys. v722 closes the
;; semantic-route collision bypass created when the scanner previously ignored
;; string-keyed request attributes. Both spellings are checked; duplicates
;; fail closed even when their values agree. These tests exercise the public
;; application assembly and dynamic pre-browser response boundaries, without
;; claiming completeness for JavaScript-generated or unsupplied DOM.

(deftest string-keyed-htmx-requests-cannot-bypass-semantic-routes
  (let [methods [:get :post :put :patch :delete]
        path "/requests/request-42/claim?from=string#pending"
        operations {:request/claim (get (standard-operations) :request/claim)}
        routes (custom-route-capabilities
                (into {}
                      (map (fn [method]
                             [(keyword "fixture" (str "string-claim-" (name method)))
                              {:operation :request/claim
                               :method method
                               :path "/requests/:request-id/claim"
                               :transports #{:htmx}}])
                           methods)))
        rendered (into [:main]
                       (map (fn [method]
                              [:button {(str "hx-" (name method)) path}
                               (name method)])
                            methods))]
    (with-surfaced-application
      {:operations operations
       :route-capabilities routes
       :rendered-surfaces {:request-board rendered}}
      (fn [{:keys [report assembly]}]
        (is (application/report? report))
        (is (nil? assembly))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (is (= (mapv (fn [index method]
                       {:surface :request-board
                        :render-path [index]
                        :method method
                        :path path
                        :request-source (keyword (str "hx-" (name method)))
                        :candidate-operations #{:request/claim}})
                     (range 1 (inc (count methods))) methods)
               (mapv #(select-keys % [:surface :render-path :method :path
                                       :request-source :candidate-operations])
                     (:errors report))))))))

(deftest string-and-mixed-keyed-native-form-submissions-are-checked
  (let [claim "/operations/request/claim"
        rendered
        [:main
         [:form {"method" "POST" "action" claim}
          [:button {"type" "submit"} "Native"]]
         [:form {"id" "detached-owner" :method :post
                 "action" "/ordinary/save"}]
         [:button {"form" "detached-owner" "formaction" claim
                   "type" "submit"} "Detached"]
         [:form {:method :get "action" "/ordinary/list"}
          [:input {"type" "submit" "formmethod" "POST"
                   "formaction" claim}]]
         [:form {"method" "GET" :action claim}
          [:button "Nonmatching GET"]]]]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (is (= [{:request-source :native-form
                 :method :post :path claim :render-path [1]}
                {:request-source :native-submitter
                 :method :post :path claim :render-path [3]
                 :form-render-path [2]
                 :submitter-overrides #{:formaction}}
                {:request-source :native-submitter
                 :method :post :path claim :render-path [4 2]
                 :form-render-path [4]
                 :submitter-overrides #{:formaction :formmethod}}]
               (mapv #(select-keys % [:request-source :method :path
                                       :render-path :form-render-path
                                       :submitter-overrides])
                     (:errors report))))))))

(deftest duplicate-keyword-string-html-attributes-are-always-ambiguous
  (let [attributes ["hx-get" "hx-post" "hx-put" "hx-patch" "hx-delete"
                    "id" "method" "action" "form" "formaction"
                    "formmethod" "type"]
        rendered
        (into [:main]
              (mapcat (fn [attribute]
                        [[:div {(keyword attribute) "/ordinary/no-op"
                                attribute "/ordinary/no-op"}]
                         [:div {(keyword attribute) "/ordinary/first"
                                attribute "/ordinary/second"}]])
                      attributes))]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (application/report? report))
        (is (nil? assembly))
        (is (= #{:rendered-ambiguous-html-attribute}
               (error-kinds report)))
        (is (= (* 2 (count attributes)) (count (:errors report))))
        (is (= (mapv (fn [index attribute]
                       {:surface :request-board
                        :render-path [(inc index)]
                        :attribute (keyword attribute)})
                     (range (* 2 (count attributes)))
                     (mapcat #(repeat 2 %) attributes))
               (mapv #(select-keys % [:surface :render-path :attribute])
                     (:errors report))))))))

(deftest duplicate-html-attributes-on-canonical-choreo-are-rejected
  (let [ctx (render-context (standard-operations))
        canonical (rendered-operation-button
                   ctx :request/claim "/operations/request/claim")
        button-index (first (keep-indexed
                             (fn [index node]
                               (when (and (vector? node)
                                          (= :button (first node))) index))
                             canonical))
        ambiguous (update-in canonical [button-index 1]
                             assoc "hx-post" "/operations/request/claim")]
    (is (some? button-index))
    (with-surfaced-application
      {:request-board [:main ambiguous]}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (not (application/valid? report)))
        (is (contains? (error-kinds report)
                       :rendered-ambiguous-html-attribute))
        (is (some (fn [error]
                    (and (= :rendered-ambiguous-html-attribute (:kind error))
                         (= :request-board (:surface error))
                         (= :hx-post (:attribute error))
                         (= [1 button-index] (:render-path error))))
                  (:errors report)))))))

(deftest string-keyed-requests-and-duplicate-attrs-fail-before-rendering
  (with-closed-application
    (fn [{:keys [assembly]}]
      (doseq [[scenario rendered expected-kind]
              [[:string-htmx
                [:button {"hx-post" "/operations/request/claim"} "Claim"]
                :rendered-semantic-route-without-choreo-operation]
               [:string-native-post
                [:form {"method" "POST" "action" "/operations/request/claim"}
                 [:button "Claim"]]
                :rendered-semantic-route-without-choreo-operation]
               [:duplicate-ordinary-attrs
                [:div {:id "unique" "id" "unique"}]
                :rendered-ambiguous-html-attribute]]]
        (testing (name scenario)
          (let [calls (atom 0)
                report (application/check-rendered-surface
                        assembly :request-board rendered)
                failure (error-data
                         #(application/checked-rendered-response!
                           assembly :request-board
                           (fn [_]
                             (swap! calls inc)
                             {:status 200})
                           rendered))]
            (is (application/rendered-surface-report? report))
            (is (not (application/rendered-surface-valid? report)))
            (is (contains? (error-kinds report) expected-kind))
            (is (= :rendered-surface-preflight-failed (:error/kind failure)))
            (is (contains? (error-kinds (:preflight failure)) expected-kind))
            (is (zero? @calls))))))))

(deftest string-keyed-ordinary-requests-coexist-with-canonical-choreo
  (let [claim "/operations/request/claim"
        canonical (rendered-operation-button
                   (render-context (standard-operations)) :request/claim claim)
        rendered
        [:main canonical
         [:button {"hx-get" "/ordinary/list"} "List"]
         [:button {"hx-post" "/ordinary/save"} "Save"]
         [:button {"hx-put" "/ordinary/replace"} "Replace"]
         [:button {"hx-patch" "/ordinary/edit"} "Edit"]
         [:button {"hx-delete" "/ordinary/delete"} "Delete"]
         [:form {"id" "form-1" "method" "POST"
                 "action" "/ordinary/save"}
          [:button {"type" "submit"} "Submit"]]
         [:button {"form" "form-1" "formaction" "/ordinary/other"}
          "Detached save"]
         [:form {"method" "GET" "action" claim}
          [:button "Nonmatching GET"]]]]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (application/report? report))
        (is (application/valid? report))
        (is (application/application-assembly? assembly))
        (is (empty? (:errors report)))
        (is (= [:request/claim]
               (mapv :operation (get-in report [:analysis :affordances]))))))))

;; =============================================================================
;; v725 adversarial regression: canonical physical request correspondence
;; =============================================================================
;; A validated :choreo/op only admits the corresponding physical hx-post on the
;; marked button. It does not whitelist other request attributes on that node.
;; These scenarios check the public static assembly and dynamic pre-render
;; boundary, including method/path evidence and healthy ordinary requests.

(defn- add-request-attrs-to-canonical-button
  "Mutate Hiccup test data without rebuilding (or inventing) Choreo metadata."
  [canonical attrs]
  (let [button-index
        (first
         (keep-indexed
          (fn [index node]
            (when (and (vector? node)
                       (= :button (first node)))
              index))
          canonical))]
    (assert (some? button-index) "Expected a canonical Gesso POST button")
    (update-in canonical [button-index 1] merge attrs)))

(deftest canonical-mark-does-not-exempt-additional-htmx-methods
  (let [canonical (rendered-operation-button
                   (render-context (standard-operations))
                   :request/claim "/operations/request/claim")
        extra-requests
        {:hx-get "/ordinary/read"
         :hx-put "/ordinary/replace"
         :hx-patch "/ordinary/update"
         :hx-delete "/ordinary/remove"}
        rendered [:main (add-request-attrs-to-canonical-button
                         canonical extra-requests)]]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (application/report? report))
        (is (nil? assembly))
        (is (= #{:rendered-choreo-physical-request-mismatch}
               (error-kinds report)))
        (let [errors (:errors report)]
          (is (= 4 (count errors)))
          (is (= [[:get :hx-get "/ordinary/read"]
                  [:put :hx-put "/ordinary/replace"]
                  [:patch :hx-patch "/ordinary/update"]
                  [:delete :hx-delete "/ordinary/remove"]]
                 (mapv (juxt :method :request-source :path) errors)))
          (is (every? #(= :request-board (:surface %)) errors))
          (is (= 1 (count (set (map :render-path errors)))))
          (is (every? #(= [{:operation :request/claim
                            :method :post
                            :path "/operations/request/claim"}]
                          (mapv (fn [a]
                                  (select-keys a [:operation :method :path]))
                                (:canonical-affordances %)))
                      errors))
          (is (every? (comp empty? :matching-routes) errors)))))))

(deftest canonical-extra-semantic-route-carries-matching-route-evidence
  (let [operations (standard-operations)
        routes (custom-route-capabilities
                {:request/claim
                 {:operation :request/claim
                  :method :post :path "/operations/request/claim"}
                 :request/cancel-read
                 {:operation :request/cancel
                  :method :get :path "/operations/request/cancel"}
                 :request/reassign
                 {:operation :request/reassign
                  :method :post :path "/operations/request/reassign"}})
        canonical (rendered-operation-button
                   (render-context operations)
                   :request/claim "/operations/request/claim")
        offending (add-request-attrs-to-canonical-button
                   canonical {:hx-get "/operations/request/cancel?from=claim"})]
    (with-surfaced-application
      {:operations operations
       :route-capabilities routes
       :rendered-surfaces {:request-board [:main offending]}}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (= #{:rendered-choreo-physical-request-mismatch}
               (error-kinds report)))
        (let [error (first (:errors report))]
          (is (= :hx-get (:request-source error)))
          (is (= :get (:method error)))
          (is (= "/operations/request/cancel?from=claim" (:path error)))
          (is (= [{:operation :request/cancel
                   :route-id :request/cancel-read
                   :method :get
                   :route-template "/operations/request/cancel"
                   :required-transport :htmx}]
                 (:matching-routes error)))
          (is (= :request/claim
                 (:operation (first (:canonical-affordances error)))))))))

(deftest canonical-physical-request-mismatches-are-occurrence-local
  (let [ctx (render-context (standard-operations))
        canonical (rendered-operation-button
                   ctx :request/claim "/operations/request/claim")
        bad-a (add-request-attrs-to-canonical-button
               canonical {:hx-get "/ordinary/first"})
        bad-b (add-request-attrs-to-canonical-button
               canonical {"hx-delete" "/ordinary/second"})]
    (with-surfaced-application
      {:alpha [:main canonical bad-a]
       :beta [:section bad-b canonical]}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (= #{:rendered-choreo-physical-request-mismatch}
               (error-kinds report)))
        (is (= [[:alpha [2 3] :hx-get "/ordinary/first"]
                [:beta [1 3] :hx-delete "/ordinary/second"]]
               (mapv (juxt :surface :render-path :request-source :path)
                     (:errors report))))
        (is (= 4 (count (get-in report [:analysis :affordances]))))
        (is (= #{:request/claim}
               (set (map :operation (get-in report [:analysis :affordances]))))))))))

(deftest canonical-extra-request-fails-before-html-rendering
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [ctx (render-context (standard-operations))
            canonical (rendered-operation-button
                       ctx :request/claim "/operations/request/claim")
            invalid [:main (add-request-attrs-to-canonical-button
                            canonical {:hx-delete "/ordinary/delete"})]
            valid [:main canonical [:button {:hx-get "/ordinary/list"} "List"]]
            calls (atom 0)
            invalid-report (application/check-rendered-surface
                            assembly :request-board invalid)
            failure (error-data
                     #(application/checked-rendered-response!
                       assembly :request-board
                       (fn [_]
                         (swap! calls inc)
                         {:status 200})
                       invalid))
            valid-report (application/check-rendered-surface
                          assembly :request-board valid)
            response (application/checked-rendered-response!
                      assembly :request-board
                      (fn [_]
                        (swap! calls inc)
                        {:status 200 :body "accepted"})
                      valid)]
        (is (application/rendered-surface-report? invalid-report))
        (is (= #{:rendered-choreo-physical-request-mismatch}
               (error-kinds invalid-report)))
        (is (= :rendered-surface-preflight-failed (:error/kind failure)))
        (is (= #{:rendered-choreo-physical-request-mismatch}
               (error-kinds (:preflight failure))))
        (is (application/rendered-surface-valid? valid-report))
        (is (= {:status 200 :body "accepted"} response))
        (is (= 1 @calls))))))

(deftest canonical-mark-does-not-hide-anonymous-sibling-semantic-route
  (let [ctx (render-context (standard-operations))
        canonical (rendered-operation-button
                   ctx :request/claim "/operations/request/claim")
        rendered [:main
                  canonical
                  [:button {:hx-post "/operations/request/claim"}
                   "Anonymous claim"]
                  [:button {:hx-get "/ordinary/list"} "Ordinary GET"]]]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (is (= [{:surface :request-board
                 :render-path [2]
                 :method :post
                 :path "/operations/request/claim"
                 :request-source :hx-post
                 :candidate-operations #{:request/claim}}]
               (mapv #(select-keys % [:surface :render-path :method :path
                                       :request-source :candidate-operations])
                     (:errors report))))))))

(deftest malformed-framework-affordance-metadata-remains-fail-closed
  (let [forged
        (with-meta
          [:button {:hx-post "/operations/request/claim"} "Claim"]
          {ui/choreo-affordance-metadata-key :request/claim})]
    (with-surfaced-application
      {:request-board forged}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (not (application/valid? report)))
        ;; v661 already refuses malformed framework metadata rather than letting
        ;; the node disappear into ordinary HTMX.  A later hardening revision may
        ;; additionally retain the semantic-route collision diagnostic alongside
        ;; this scanner-local cause; this regression deliberately forbids only
        ;; accidental acceptance.
        (is (contains? (error-kinds report)
                       :rendered-affordance-scan-failed))
        (let [error
              (first
               (filter #(= :rendered-affordance-scan-failed (:kind %))
                       (:errors report)))]
          (is (= :gesso.live.ui/affordance-error (:cause-type error)))
          (is (= :invalid-rendered-affordance-encoding (:cause-kind error))))))))

(deftest supplied-rendered-surfaces-close-every-discovered-affordance-relative-to-the-snapshot
  (let [operations (standard-operations)
        ctx (render-context operations)
        surfaces
        {:request-board
         [:main
          (rendered-operation-button ctx :request/claim "/operations/request/claim")
          (rendered-operation-button ctx :request/cancel "/operations/request/cancel")]
         :request-toolbar
         [:nav
          (rendered-operation-button ctx :request/reassign "/operations/request/reassign")]}]
    (with-surfaced-application
      surfaces
      (fn [{:keys [report assembly]}]
        (let [explanation (application/explain assembly)
              obligation (first (application/open-obligations assembly))]
          (is (application/valid? report))
          (is (application/application-assembly? assembly))
          (is (= :closed-relative-to-supplied-rendered-surfaces
                 (get-in report [:analysis :affordance-closure :status])))
          (is (= application/rendered-affordance-guarantee
                 (get-in report [:analysis :affordance-closure :guarantee])))
          (is (= 2 (get-in report [:analysis :affordance-closure :surface-count])))
          (is (= 3 (get-in report [:analysis :affordance-closure :affordance-count])))
          (is (= #{:request-board :request-toolbar}
                 (:rendered-surface-names explanation)))
          (is (= :rendered-surface-enumeration-completeness-not-yet-modeled
                 (:kind obligation)))
          (is (= :application->rendered-surfaces (:edge obligation)))
          (is (= :open (:status obligation)))
          (is (= #{:rendered-surface-enumeration-completeness-not-yet-modeled
                   :published-change-topics-not-consumed-by-live-app}
                 (warning-kinds report)))
          (is (not= :whole-application-preflight-closed
                    (:guarantee explanation))))))))

(deftest repeated-operation-affordances-across-surfaces-remain-distinct-occurrences
  (let [operations (standard-operations)
        ctx (render-context operations)
        surfaces
        {:alpha
         [:section
          (rendered-operation-button ctx :request/claim "/operations/request/claim")]
         :beta
         [:section
          (rendered-operation-button ctx :request/claim "/operations/request/claim")
          (rendered-operation-button ctx :request/claim "/operations/request/claim")]}]
    (with-surfaced-application
      surfaces
      (fn [{:keys [assembly]}]
        (let [claim (application/explain-operation assembly :request/claim)
              affordances (:rendered-affordances claim)]
          (is (= 3 (count affordances)))
          (is (= [:alpha :beta :beta] (mapv :surface affordances)))
          (is (= 3 (count (set (map (juxt :surface :render-path) affordances)))))
          (is (every? #(= :request/claim (:operation %)) affordances))
          (is (every? #(= :request/claim (:route-id %)) affordances)))))))

(deftest parameterized-route-template-matches-concrete-rendered-path-and-ignores-query-fragment
  (let [operations (standard-operations)
        ctx (render-context operations)
        routes
        (custom-route-capabilities
         {:request/claim
          {:operation :request/claim
           :path "/requests/:request-id/claim"}
          :request/reassign
          {:operation :request/reassign
           :path "/operations/request/reassign"}
          :request/cancel
          {:operation :request/cancel
           :path "/operations/request/cancel"}})
        surfaces
        {:board
         [:main
          (rendered-operation-button
           ctx
           :request/claim
           "/requests/request-1/claim?from=board#card")]}]
    (with-surfaced-application
      {:operations operations
       :route-capabilities routes
       :rendered-surfaces surfaces}
      (fn [{:keys [report assembly]}]
        (is (application/valid? report))
        (let [affordance
              (first (:rendered-affordances
                      (application/explain-operation assembly :request/claim)))]
          (is (= "/requests/request-1/claim?from=board#card" (:path affordance)))
          (is (= "/requests/:request-id/claim" (:route-template affordance)))
          (is (= :request/claim (:route-id affordance)))
          (is (= :htmx (:required-transport affordance))))))))

(deftest rendered-affordance-wrong-concrete-path-fails-with-local-route-evidence
  (let [operations (standard-operations)
        ctx (render-context operations)
        routes
        (custom-route-capabilities
         {:request/claim
          {:operation :request/claim
           :path "/requests/:request-id/claim"}
          :request/reassign
          {:operation :request/reassign
           :path "/operations/request/reassign"}
          :request/cancel
          {:operation :request/cancel
           :path "/operations/request/cancel"}})
        surfaces
        {:board
         [:main
          (rendered-operation-button
           ctx
           :request/claim
           "/requests/request-1/cancel")]}]
    (with-surfaced-application
      {:operations operations
       :route-capabilities routes
       :rendered-surfaces surfaces}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (not (application/valid? report)))
        (is (= #{:rendered-affordance-path-mismatch}
               (error-kinds report)))
        (let [error (first (:errors report))]
          (is (= :board (:surface error)))
          (is (= :request/claim (:operation error)))
          (is (= :post (:method error)))
          (is (= "/requests/request-1/cancel" (:path error)))
          (is (= ["/requests/:request-id/claim"] (:route-templates error))))))))

(deftest rendered-affordance-method-mismatch-is-distinct-from-path-mismatch
  (let [operations (standard-operations)
        ctx (render-context operations)
        routes
        (custom-route-capabilities
         {:request/claim
          {:operation :request/claim
           :method :put
           :path "/requests/:request-id/claim"}
          :request/reassign
          {:operation :request/reassign
           :path "/operations/request/reassign"}
          :request/cancel
          {:operation :request/cancel
           :path "/operations/request/cancel"}})
        surfaces
        {:board
         [:main
          (rendered-operation-button
           ctx
           :request/claim
           "/requests/request-1/claim")]}]
    (with-surfaced-application
      {:operations operations
       :route-capabilities routes
       :rendered-surfaces surfaces}
      (fn [{:keys [report]}]
        (is (not (application/valid? report)))
        (is (= #{:rendered-affordance-method-mismatch}
               (error-kinds report)))
        (let [error (first (:errors report))]
          (is (= :post (:method error)))
          (is (= :put (get-in error [:declared-routes 0 :method]))))))))

(deftest rendered-affordance-for-operation-outside-assembled-slice-fails-with-available-operations
  (let [operations (standard-operations)
        extra-operation
        (trusted-operation :request/archive {:published-change-topics #{:request}})
        render-operations (assoc operations :request/archive extra-operation)
        ctx (render-context render-operations)
        surfaces
        {:board
         [:main
          (rendered-operation-button
           ctx
           :request/archive
           "/operations/request/archive")]}]
    (with-surfaced-application
      surfaces
      (fn [{:keys [report]}]
        (is (not (application/valid? report)))
        (is (= #{:rendered-affordance-unknown-operation}
               (error-kinds report)))
        (let [error (first (:errors report))]
          (is (= :request/archive (:operation error)))
          (is (= #{:request/claim :request/reassign :request/cancel}
                 (:available-operations error))))))))

(deftest ambiguous-trusted-route-templates-fail-at-rendered-affordance-join
  (let [operations (standard-operations)
        ctx (render-context operations)
        routes
        (custom-route-capabilities
         {:request/claim-by-request-id
          {:operation :request/claim
           :path "/requests/:request-id/claim"}
          :request/claim-by-id
          {:operation :request/claim
           :path "/requests/:id/claim"}
          :request/reassign
          {:operation :request/reassign
           :path "/operations/request/reassign"}
          :request/cancel
          {:operation :request/cancel
           :path "/operations/request/cancel"}})
        surfaces
        {:board
         [:main
          (rendered-operation-button
           ctx
           :request/claim
           "/requests/request-1/claim")]}]
    (with-surfaced-application
      {:operations operations
       :route-capabilities routes
       :rendered-surfaces surfaces}
      (fn [{:keys [report]}]
        (is (not (application/valid? report)))
        (is (= #{:ambiguous-rendered-affordance-route}
               (error-kinds report)))
        (let [error (first (:errors report))]
          (is (= 2 (count (:matching-routes error))))
          (is (= #{:request/claim-by-request-id :request/claim-by-id}
                 (set (map :route-id (:matching-routes error))))))))))

(deftest one-malformed-surface-does-not-hide-valid-affordances-from-other-surfaces
  (let [operations (standard-operations)
        ctx (render-context operations)
        valid-button
        (rendered-operation-button ctx :request/claim "/operations/request/claim")
        bad-button
        (rendered-operation-button ctx :request/cancel "/wrong/cancel")
        surfaces {:good [:main valid-button]
                  :bad [:main bad-button]}]
    (with-surfaced-application
      surfaces
      (fn [{:keys [report]}]
        (is (not (application/valid? report)))
        (is (= #{:rendered-affordance-path-mismatch}
               (error-kinds report)))
        (is (= :bad (:surface (first (:errors report)))))
        (let [affordances (get-in report [:analysis :affordances])
              good (some #(when (= :good (:surface %)) %) affordances)
              bad (some #(when (= :bad (:surface %)) %) affordances)]
          (is (= :request/claim (:operation good)))
          (is (= :request/claim (:route-id good)))
          (is (= :request/cancel (:operation bad)))
          (is (nil? (:route-id bad))))))))

(deftest malformed-canonical-affordance-in-one-surface-fails-scan-with-surface-local-cause
  (let [operations (standard-operations)
        ctx (render-context operations)
        good (rendered-operation-button ctx :request/claim "/operations/request/claim")
        canonical (rendered-operation-button ctx :request/cancel "/operations/request/cancel")
        button-index
        (first
         (keep-indexed
          (fn [index node]
            (when (and (vector? node) (= :button (first node))) index))
          canonical))
        malformed
        (update canonical button-index
                (fn [node]
                  (with-meta node
                    (assoc (meta node)
                           ui/choreo-affordance-metadata-key
                           :request/claim))))
        surfaces {:good [:main good]
                  :bad [:main malformed]}]
    (with-surfaced-application
      surfaces
      (fn [{:keys [report]}]
        (is (not (application/valid? report)))
        (is (= #{:rendered-affordance-scan-failed}
               (error-kinds report)))
        (let [error (first (:errors report))]
          (is (= :bad (:surface error)))
          (is (= :gesso.live.ui/affordance-error (:cause-type error)))
          (is (keyword? (:cause-kind error))))))))

(deftest rendered-surface-report-and-assembly-tampering-fail-rederivation
  (let [operations (standard-operations)
        ctx (render-context operations)
        surfaces
        {:board
         [:main
          (rendered-operation-button ctx :request/claim "/operations/request/claim")]}]
    (with-surfaced-application
      surfaces
      (fn [{:keys [report assembly]}]
        (let [forged-affordance-report
              (assoc-in report
                        [:analysis :affordances 0 :route-template]
                        "/forged")
              forged-closure-report
              (assoc-in report
                        [:analysis :affordance-closure :status]
                        :whole-application-closed)
              tampered-surface
              {:board
               [:main
                (rendered-operation-button
                 ctx
                 :request/claim
                 "/wrong/claim")]}
              tampered-assembly
              (assoc assembly :rendered-surfaces tampered-surface)]
          (is (not (application/report? forged-affordance-report)))
          (is (not (application/report? forged-closure-report)))
          (is (not (application/application-assembly? tampered-assembly)))
          (is (= :invalid-application-input
                 (error-kind #(application/open-obligations tampered-assembly)))))))))

(deftest surfaced-assembly-stores-render-snapshot-not-derived-affordance-registry
  (let [operations (standard-operations)
        ctx (render-context operations)
        surfaces
        {:board
         [:main
          (rendered-operation-button ctx :request/claim "/operations/request/claim")]}]
    (with-surfaced-application
      surfaces
      (fn [{:keys [assembly]}]
        (is (= surfaces (:rendered-surfaces assembly)))
        (is (= #{:gesso.live.application-preflight/type
                 :gesso.live.application-preflight/version
                 :name
                 :operation-acquisition-assembly
                 :browser-artifact-path
                 :browser-receipt-path
                 :rendered-surfaces}
               (set (keys assembly))))
        (is (not (contains? assembly :affordances)))
        (is (not (contains? assembly :affordance-closure)))
        (is (not (contains? assembly :operations)))
        (is (not (application/application-assembly?
                  (assoc assembly :affordances []))))))))

(deftest rendered-surfaces-option-must-be-non-empty-keyed-snapshot
  (with-temp-dir
    (fn [dir]
      (let [operation-acquisition' @standard-operation-acquisition
            artifact-path (child-path dir "gesso-live.js")]
        (record-artifact! operation-acquisition' artifact-path)
        (doseq [rendered-surfaces
                [{}
                 {"not-keyword" [:main]}
                 {:board nil}]]
          (is (= :invalid-rendered-surfaces
                 (error-kind
                  #(application/check-application-assembly
                    (assoc
                     (application-options operation-acquisition' artifact-path)
                     :rendered-surfaces rendered-surfaces))))))))))

(deftest custom-receipt-currentness-remains-fail-closed-with-rendered-surfaces
  (let [operations (standard-operations)
        ctx (render-context operations)
        surfaces
        {:board
         [:main
          (rendered-operation-button ctx :request/claim "/operations/request/claim")]}]
    (with-surfaced-application
      {:operations operations
       :route-capabilities (route-capabilities operations)
       :rendered-surfaces surfaces
       :custom-receipt? true}
      (fn [{:keys [receipt-path report assembly]}]
        (is (application/valid? report))
        (is (application/application-assembly? assembly))
        (spit receipt-path "{:tampered :surface-receipt}\n" :encoding "UTF-8")
        (is (not (application/report? report)))
        (is (not (application/application-assembly? assembly)))))))

;; =============================================================================
;; Dynamic pre-browser rendered-surface boundary
;; =============================================================================

(deftest dynamic-rendered-surface-report-closes-one-actual-surface
  (let [operations (standard-operations)
        ctx (render-context operations)
        rendered
        [:main
         (rendered-operation-button
          ctx
          :request/claim
          "/operations/request/claim")]]
    (with-closed-application
      (fn [{:keys [assembly]}]
        (let [report
              (application/check-rendered-surface
               assembly
               :request-board
               rendered)]
          (is (application/rendered-surface-report? report))
          (is (application/rendered-surface-valid? report))
          (is (= :rendered-surface-pre-browser-preflight-closed
                 (get-in report [:analysis :guarantee])))
          (is (= :request-board
                 (get-in report [:analysis :surface])))
          (is (= rendered
                 (get-in report [:analysis :rendered])))
          (let [affordance (first (get-in report [:analysis :affordances]))]
            (is (= :gesso.live.ui/rendered-choreo-affordance
                   (:gesso.live.ui/type affordance)))
            (is (= 1 (:gesso.live.ui/version affordance)))
            (is (= :request-board (:surface affordance)))
            (is (= :post-button (:kind affordance)))
            (is (= :request/claim (:operation affordance)))
            (is (= :request/claim (:plan-key affordance)))
            (is (= :post (:method affordance)))
            (is (= "/operations/request/claim" (:path affordance)))
            (is (= [1 3] (:render-path affordance)))
            (is (= :request/claim (:route-id affordance)))
            (is (= "/operations/request/claim" (:route-template affordance)))
            (is (= :htmx (:required-transport affordance)))))))))

(deftest checked-rendered-response-validates-before-calling-renderer-exactly-once
  (let [operations (standard-operations)
        ctx (render-context operations)
        rendered
        [:main
         (rendered-operation-button
          ctx
          :request/claim
          "/operations/request/claim")]
        calls (atom [])]
    (with-closed-application
      (fn [{:keys [assembly]}]
        (let [response
              (application/checked-rendered-response!
               assembly
               :request-board
               (fn [node]
                 (swap! calls conj node)
                 {:status 200 :body node})
               rendered)]
          (is (= [rendered] @calls))
          (is (= {:status 200 :body rendered} response)))))))

(deftest checked-rendered-response-never-calls-renderer-on-affordance-failure
  (let [operations (standard-operations)
        ctx (render-context operations)
        rendered
        [:main
         (rendered-operation-button
          ctx
          :request/claim
          "/wrong/claim")]
        calls (atom 0)]
    (with-closed-application
      (fn [{:keys [assembly]}]
        (let [data
              (error-data
               #(application/checked-rendered-response!
                 assembly
                 :request-board
                 (fn [_]
                   (swap! calls inc)
                   {:status 200})
                 rendered))]
          (is (= :rendered-surface-preflight-failed (:error/kind data)))
          (is (= 0 @calls))
          (is (= #{:rendered-affordance-path-mismatch}
                 (error-kinds (:preflight data)))))))))

(deftest invalid-or-malformed-render-boundary-input-never-reaches-renderer
  (let [operations (standard-operations)
        ctx (render-context operations)
        rendered
        [:main
         (rendered-operation-button
          ctx
          :request/claim
          "/operations/request/claim")]]
    (with-closed-application
      (fn [{:keys [assembly]}]
        (doseq [[surface node expected]
                [["request-board" rendered :invalid-rendered-surface-name]
                 [nil rendered :invalid-rendered-surface-name]
                 [:request-board nil :invalid-rendered-surface]]]
          (let [calls (atom 0)]
            (is (= expected
                   (error-kind
                    #(application/checked-rendered-response!
                      assembly
                      surface
                      (fn [_]
                        (swap! calls inc)
                        {:status 200})
                      node))))
            (is (= 0 @calls))))))))

(deftest non-callable-response-renderer-fails-before-browser-delivery
  (let [operations (standard-operations)
        ctx (render-context operations)
        rendered
        [:main
         (rendered-operation-button
          ctx
          :request/claim
          "/operations/request/claim")]]
    (with-closed-application
      (fn [{:keys [assembly]}]
        (is (= :invalid-response-renderer
               (error-kind
                #(application/checked-rendered-response!
                  assembly
                  :request-board
                  42
                  rendered))))))))

(deftest dynamic-surface-need-not-have-appeared-in-static-render-snapshot
  (let [operations (standard-operations)
        ctx (render-context operations)
        snapshot
        {:board
         [:main
          (rendered-operation-button
           ctx
           :request/claim
           "/operations/request/claim")]}
        later-render
        [:aside
         (rendered-operation-button
          ctx
          :request/reassign
          "/operations/request/reassign")]]
    (with-surfaced-application
      snapshot
      (fn [{:keys [assembly]}]
        (let [report
              (application/check-rendered-surface
               assembly
               :assignment-panel
               later-render)]
          (is (application/rendered-surface-valid? report))
          (is (= :assignment-panel
                 (get-in report [:analysis :surface])))
          (is (= :request/reassign
                 (get-in report [:analysis :affordances 0 :operation])))
          (is (= :request/reassign
                 (get-in report [:analysis :affordances 0 :route-id]))))))))

(deftest unsurfaced-application-can-enforce-an-actual-dynamic-render
  (let [operations (standard-operations)
        ctx (render-context operations)
        rendered
        [:main
         (rendered-operation-button
          ctx
          :request/cancel
          "/operations/request/cancel")]]
    (with-closed-application
      (fn [{:keys [assembly]}]
        (is (= #{:static-affordance-closure-not-yet-modeled}
               (set (map :kind (application/open-obligations assembly)))))
        (let [report
              (application/require-rendered-surface!
               assembly
               :request-board
               rendered)]
          (is (application/rendered-surface-valid? report))
          (is (= :request/cancel
                 (get-in report [:analysis :affordances 0 :operation])))
          (is (= #{}
                 (get-in report
                         [:analysis :affordances 0 :publication-topics]
                         #{}))))))))

(deftest rendered-surface-report-recognizer-rejects-derived-and-input-tampering
  (let [operations (standard-operations)
        ctx (render-context operations)
        rendered
        [:main
         (rendered-operation-button
          ctx
          :request/claim
          "/operations/request/claim")]]
    (with-closed-application
      (fn [{:keys [assembly]}]
        (let [report
              (application/require-rendered-surface!
               assembly
               :request-board
               rendered)]
          (is (application/rendered-surface-report? report))
          (doseq [forged
                  [(assoc-in report
                             [:analysis :guarantee]
                             :whole-application-preflight-closed)
                   (assoc-in report
                             [:analysis :affordances 0 :route-template]
                             "/forged")
                   (assoc-in report
                             [:analysis :surface]
                             :forged-surface)
                   (assoc-in report
                             [:analysis :rendered]
                             [:main "forged"])
                   (assoc report :valid? false)
                   (assoc report :extra :forged)]]
            (is (not (application/rendered-surface-report? forged)))))))))

(deftest stale-browser-artifact-invalidates-rendered-surface-report-and-blocks-response
  (let [operations (standard-operations)
        ctx (render-context operations)
        rendered
        [:main
         (rendered-operation-button
          ctx
          :request/claim
          "/operations/request/claim")]]
    (with-closed-application
      (fn [{:keys [assembly artifact-path]}]
        (let [report
              (application/require-rendered-surface!
               assembly
               :request-board
               rendered)
              calls (atom 0)]
          (is (application/rendered-surface-report? report))
          (spit artifact-path "console.log('stale');\n" :encoding "UTF-8")
          (is (not (application/rendered-surface-report? report)))
          (is (= :rendered-surface-preflight-failed
                 (error-kind
                  #(application/checked-rendered-response!
                    assembly
                    :request-board
                    (fn [_]
                      (swap! calls inc)
                      {:status 200})
                    rendered))))
          (is (= 0 @calls)))))))

(deftest stale-custom-receipt-invalidates-rendered-surface-report
  (let [operations (standard-operations)
        ctx (render-context operations)
        rendered
        [:main
         (rendered-operation-button
          ctx
          :request/claim
          "/operations/request/claim")]]
    (with-closed-application
      {:custom-receipt? true}
      (fn [{:keys [assembly receipt-path]}]
        (let [report
              (application/require-rendered-surface!
               assembly
               :request-board
               rendered)]
          (is (application/rendered-surface-report? report))
          (spit receipt-path "{:tampered :dynamic-receipt}\n" :encoding "UTF-8")
          (is (not (application/rendered-surface-report? report)))
          (let [failed
                (application/check-rendered-surface
                 assembly
                 :request-board
                 rendered)]
            (is (not (application/rendered-surface-valid? failed)))
            (is (contains? (error-kinds failed)
                           :invalid-application-assembly))))))))

(deftest dynamic-surface-validation-does-not-erase-producer-coverage-obligation
  (let [operations (standard-operations)
        ctx (render-context operations)
        rendered
        [:main
         (rendered-operation-button
          ctx
          :request/claim
          "/operations/request/claim")]]
    (with-surfaced-application
      {:board rendered}
      (fn [{:keys [assembly]}]
        (is (application/rendered-surface-valid?
             (application/check-rendered-surface
              assembly
              :board
              rendered)))
        (is (= #{:rendered-surface-enumeration-completeness-not-yet-modeled}
               (set (map :kind (application/open-obligations assembly)))))
        (is (= :application-runtime-backbone-preflight-closed
               (:guarantee (application/explain assembly))))))))


;; =============================================================================
;; v704 released Biff 2 lifecycle module boundary
;; =============================================================================

(def module-test-modules
  [application/application-handler-module])

(defn lifecycle-var-handler
  [_request]
  (g/html-response [:main [:p "var-handler"]]))

(defn- valid-lifecycle-handler
  [ctx]
  (fn [_request]
    (g/html-response
     [:main
      (rendered-operation-button
       ctx
       :request/claim
       "/operations/request/claim")])))

(defn- invalid-lifecycle-handler
  [ctx]
  (fn [_request]
    (g/html-response
     [:main
      (rendered-operation-button
       ctx
       :request/claim
       "/operations/request/not-claim")])))

(defn- application-handler-start
  []
  (:biff.core/start application/application-handler-module))

(defn- application-handler-system
  [assembly handler & kvs]
  (merge
   {application/application-assembly-system-key assembly
    :biff.ring/handler handler}
   (apply hash-map kvs)))

(deftest application-handler-module-has-stable-native-biff-identity
  (is (= application/application-handler-module-id
         (:biff.core/id application/application-handler-module)))
  (is (qualified-keyword? application/application-handler-module-id))
  (is (fn? (:biff.core/start application/application-handler-module)))
  (is (nil? (:biff.core/stop application/application-handler-module))))

(deftest application-handler-module-integrates-through-real-biff-start
  (let [operations (standard-operations)
        ctx (render-context operations)]
    (with-surfaced-application
      {:request-board
       [:main
        (rendered-operation-button
         ctx :request/claim "/operations/request/claim")]}
      (fn [{:keys [assembly]}]
        (let [original-handler (valid-lifecycle-handler ctx)
              observed-handler (atom nil)
              observed-assembly-key? (atom nil)
              observer-id :fixture/observe-gesso-handler
              observer-module
              {:biff.core/id observer-id
               :biff.core/start
               (fn [system]
                 (reset! observed-handler (:biff.ring/handler system))
                 (reset! observed-assembly-key?
                         (contains?
                          system
                          application/application-assembly-system-key))
                 (assoc system :fixture/observer-saw-handler? true))}
              initial-system
              (application-handler-system
               assembly
               original-handler
               :fixture/preserved {:sentinel 42})]
          (with-redefs [module-test-modules
                        [application/application-handler-module
                         observer-module]]
            (let [started
                  (biff/start
                   initial-system
                   #'module-test-modules
                   [application/application-handler-module-id
                    observer-id])]
              (is (= {:sentinel 42} (:fixture/preserved started)))
              (is (true? (:fixture/observer-saw-handler? started)))
              (is (false? @observed-assembly-key?))
              (is (not (contains?
                        started
                        application/application-assembly-system-key)))
              (is (fn? @observed-handler))
              (is (not (identical? original-handler @observed-handler)))
              (is (identical? @observed-handler (:biff.ring/handler started)))
              (is (= 200
                     (:status
                      ((:biff.ring/handler started) {:uri "/"})))))))))))

(deftest application-handler-module-preserves-system-except-handler-and-temporary-assembly
  (let [operations (standard-operations)
        ctx (render-context operations)]
    (with-surfaced-application
      {:request-board
       [:main
        (rendered-operation-button
         ctx :request/claim "/operations/request/claim")]}
      (fn [{:keys [assembly]}]
        (let [handler (valid-lifecycle-handler ctx)
              system
              (application-handler-system
               assembly
               handler
               :fixture/a 1
               :fixture/b {:nested true})
              updated ((application-handler-start) system)]
          (is (= 1 (:fixture/a updated)))
          (is (= {:nested true} (:fixture/b updated)))
          (is (= (-> system
                     (dissoc :biff.ring/handler)
                     (dissoc application/application-assembly-system-key))
                 (dissoc updated :biff.ring/handler)))
          (is (not (contains?
                    updated
                    application/application-assembly-system-key)))
          (is (fn? (:biff.ring/handler updated)))
          (is (not (identical? handler (:biff.ring/handler updated)))))))))

(deftest application-handler-module-accepts-var-handler
  (let [operations (standard-operations)
        ctx (render-context operations)]
    (with-surfaced-application
      {:request-board
       [:main
        (rendered-operation-button
         ctx :request/claim "/operations/request/claim")]}
      (fn [{:keys [assembly]}]
        (let [updated
              ((application-handler-start)
               (application-handler-system
                assembly
                #'lifecycle-var-handler))
              response ((:biff.ring/handler updated) {:uri "/"})]
          (is (fn? (:biff.ring/handler updated)))
          (is (= 200 (:status response)))
          (is (string? (:body response))))))))

(deftest application-handler-module-rejects-invalid-system-assembly-and-handler
  (let [operations (standard-operations)
        ctx (render-context operations)]
    (with-surfaced-application
      {:request-board
       [:main
        (rendered-operation-button
         ctx :request/claim "/operations/request/claim")]}
      (fn [{:keys [assembly]}]
        (let [start (application-handler-start)]
          (is (= :invalid-application-handler-system
                 (error-kind #(start nil))))
          (is (= :invalid-application-handler-assembly
                 (error-kind
                  #(start {:biff.ring/handler
                           (valid-lifecycle-handler ctx)}))))
          (is (= :invalid-application-handler-assembly
                 (error-kind
                  #(start
                    {application/application-assembly-system-key
                     {:gesso.live.application-preflight/type
                      :gesso.live.application-preflight/application-assembly}
                     :biff.ring/handler
                     (valid-lifecycle-handler ctx)}))))
          (is (= :missing-biff-ring-handler
                 (error-kind
                  #(start
                    {application/application-assembly-system-key assembly
                     :fixture/value 1}))))
          (doseq [handler [nil 42 :callable-keyword {}]]
            (is (= :invalid-biff-ring-handler
                   (error-kind
                    #(start
                      (application-handler-system
                       assembly
                       handler)))))))))))

(deftest stale-assembly-before-module-installation-fails
  (let [operations (standard-operations)
        ctx (render-context operations)]
    (with-surfaced-application
      {:request-board
       [:main
        (rendered-operation-button
         ctx :request/claim "/operations/request/claim")]}
      (fn [{:keys [assembly artifact-path]}]
        (spit artifact-path "// stale before module installation\n")
        (is (= :invalid-application-handler-assembly
               (error-kind
                #((application-handler-start)
                  (application-handler-system
                   assembly
                   (valid-lifecycle-handler ctx))))))))))

(deftest stale-artifact-after-module-installation-fails-on-next-html-response
  (let [operations (standard-operations)
        ctx (render-context operations)]
    (with-surfaced-application
      {:request-board
       [:main
        (rendered-operation-button
         ctx :request/claim "/operations/request/claim")]}
      (fn [{:keys [assembly artifact-path]}]
        (let [updated
              ((application-handler-start)
               (application-handler-system
                assembly
                (valid-lifecycle-handler ctx)))
              handler (:biff.ring/handler updated)]
          (is (= 200 (:status (handler {:uri "/before"}))))
          (spit artifact-path "// stale after handler installation\n")
          (is (= :rendered-surface-preflight-failed
                 (error-kind #(handler {:uri "/after"})))))))))

(deftest application-handler-module-blocks-route-incoherent-affordance-before-html-delivery
  (let [operations (standard-operations)
        ctx (render-context operations)]
    (with-surfaced-application
      {:request-board
       [:main
        (rendered-operation-button
         ctx :request/claim "/operations/request/claim")]}
      (fn [{:keys [assembly]}]
        (let [handler
              (:biff.ring/handler
               ((application-handler-start)
                (application-handler-system
                 assembly
                 (invalid-lifecycle-handler ctx))))
              data (error-data #(handler {:uri "/"}))]
          (is (= :rendered-surface-preflight-failed (:error/kind data)))
          (is (contains?
               (set (map :kind (:errors (:preflight data))))
               :rendered-affordance-path-mismatch)))))))

(deftest application-handler-module-blocks-silent-semantic-route-downgrade-before-html-delivery
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [handler
            (:biff.ring/handler
             ((application-handler-start)
              (application-handler-system
               assembly
               (fn [_request]
                 (g/html-response
                  [:main
                   [:button
                    {:hx-post "/operations/request/claim"}
                    "Claim"]])))))
            data (error-data #(handler {:uri "/app"}))]
        (is (= :rendered-surface-preflight-failed (:error/kind data)))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds (:preflight data))))))))

(deftest later-biff-module-observes-already-wrapped-handler
  (let [operations (standard-operations)
        ctx (render-context operations)]
    (with-surfaced-application
      {:request-board
       [:main
        (rendered-operation-button
         ctx :request/claim "/operations/request/claim")]}
      (fn [{:keys [assembly]}]
        (let [original (valid-lifecycle-handler ctx)
              seen (atom nil)
              observer-id :fixture/observe-wrapped-handler
              observer-module
              {:biff.core/id observer-id
               :biff.core/start
               (fn [system]
                 (reset! seen (:biff.ring/handler system))
                 system)}]
          (with-redefs [module-test-modules
                        [application/application-handler-module
                         observer-module]]
            (let [started
                  (biff/start
                   (application-handler-system assembly original)
                   #'module-test-modules
                   [application/application-handler-module-id
                    observer-id])]
              (is (fn? @seen))
              (is (identical? @seen (:biff.ring/handler started)))
              (is (not (identical? original @seen))))))))))

(deftest duplicate-application-handler-modules-are-rejected-instead-of-composed
  (with-closed-application
    (fn [{:keys [assembly]}]
      (with-redefs [module-test-modules
                    [application/application-handler-module
                     application/application-handler-module]]
        (is (= :missing-or-duplicate-application-handler-module
               (error-kind
                #(application/start-biff-application!
                  assembly
                  {:biff.ring/handler lifecycle-var-handler}
                  #'module-test-modules
                  [application/application-handler-module-id]))))))))

;; =============================================================================
;; v704 released Biff 2 canonical Gesso/Biff startup boundary
;; =============================================================================

(defn canonical-start-module-handler
  [_request]
  (g/html-response [:main [:p "module-handler"]]))

(def canonical-start-modules
  [application/application-handler-module
   {:biff.core/init
    (fn [_modules-var]
      {:biff.ring/handler canonical-start-module-handler})}])

(deftest canonical-biff-start-runs-gesso-before-later-modules
  (let [operations (standard-operations)
        ctx (render-context operations)]
    (with-closed-application
      (fn [{:keys [assembly]}]
        (let [original (valid-lifecycle-handler ctx)
              events (atom [])
              validation-calls (atom 0)
              original-require application/require-rendered-surface!
              observer-id :fixture/observe-canonical-handler
              later-id :fixture/later-module
              observer-module
              {:biff.core/id observer-id
               :biff.core/start
               (fn [system]
                 (swap! events conj :observer)
                 (is (fn? (:biff.ring/handler system)))
                 (is (not (identical? original (:biff.ring/handler system))))
                 (is (not (contains?
                           system
                           application/application-assembly-system-key)))
                 system)}
              later-module
              {:biff.core/id later-id
               :biff.core/start
               (fn [system]
                 (swap! events conj :later)
                 system)}]
          (with-redefs [module-test-modules
                        [application/application-handler-module
                         observer-module
                         later-module]]
            (let [started
                  (application/start-biff-application!
                   assembly
                   {:biff.ring/handler original
                    :fixture/preserved 42}
                   #'module-test-modules
                   [application/application-handler-module-id
                    observer-id
                    later-id])]
              (is (= [:observer :later] @events))
              (is (= 42 (:fixture/preserved started)))
              (with-redefs [application/require-rendered-surface!
                            (fn [app surface rendered]
                              (swap! validation-calls inc)
                              (original-require app surface rendered))]
                (is (= 200
                       (:status
                        ((:biff.ring/handler started) {:uri "/"})))))
              (is (= 1 @validation-calls)))))))))

(deftest canonical-biff-start-short-arity-uses-module-init-handler
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [started
            (application/start-biff-application!
             assembly
             #'canonical-start-modules
             [application/application-handler-module-id])
            response ((:biff.ring/handler started) {:uri "/module"})]
        (is (fn? (:biff.ring/handler started)))
        (is (= 200 (:status response)))
        (is (re-find #"module-handler" (:body response)))))))

(deftest canonical-biff-start-initial-system-handler-overrides-module-init-handler
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [initial-handler
            (fn [_request]
              (g/html-response [:main [:p "initial-handler"]]))
            started
            (application/start-biff-application!
             assembly
             {:biff.ring/handler initial-handler
              :fixture/source :initial-system}
             #'canonical-start-modules
             [application/application-handler-module-id])
            response ((:biff.ring/handler started) {:uri "/initial"})]
        (is (= :initial-system (:fixture/source started)))
        (is (= 200 (:status response)))
        (is (re-find #"initial-handler" (:body response)))
        (is (not (re-find #"module-handler" (:body response))))))))

(deftest canonical-biff-start-requires-handler-before-later-server-module
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [server-ran? (atom false)
            server-id :fixture/install-handler-too-late
            server-module
            {:biff.core/id server-id
             :biff.core/start
             (fn [system]
               (reset! server-ran? true)
               (assoc system
                      :biff.ring/handler
                      canonical-start-module-handler))}]
        (with-redefs [module-test-modules
                      [application/application-handler-module
                       server-module]]
          (is (= :missing-biff-ring-handler
                 (error-kind
                  #(application/start-biff-application!
                    assembly
                    {}
                    #'module-test-modules
                    [application/application-handler-module-id
                     server-id]))))
          (is (false? @server-ran?)))))))

(deftest canonical-biff-start-validates-local-startup-inputs
  (with-closed-application
    (fn [{:keys [assembly]}]
      (is (= :invalid-biff-application-initial-system
             (error-kind
              #(application/start-biff-application!
                assembly
                42
                #'module-test-modules
                [application/application-handler-module-id]))))
      (is (= :invalid-biff-application-modules-var
             (error-kind
              #(application/start-biff-application!
                assembly
                {}
                []
                [application/application-handler-module-id]))))
      (is (= :invalid-biff-application-start-order
             (error-kind
              #(application/start-biff-application!
                assembly
                {}
                #'module-test-modules
                42))))))
  (is (= :invalid-biff-application-start-assembly
         (error-kind
          #(application/start-biff-application!
            nil
            {:biff.ring/handler canonical-start-module-handler}
            #'module-test-modules
            [application/application-handler-module-id])))))

(deftest canonical-biff-start-requires-exactly-one-canonical-gesso-module
  (with-closed-application
    (fn [{:keys [assembly]}]
      (with-redefs [module-test-modules []]
        (is (= :missing-or-duplicate-application-handler-module
               (error-kind
                #(application/start-biff-application!
                  assembly
                  {:biff.ring/handler canonical-start-module-handler}
                  #'module-test-modules
                  [application/application-handler-module-id])))))
      (with-redefs [module-test-modules
                    [application/application-handler-module
                     application/application-handler-module]]
        (is (= :missing-or-duplicate-application-handler-module
               (error-kind
                #(application/start-biff-application!
                  assembly
                  {:biff.ring/handler canonical-start-module-handler}
                  #'module-test-modules
                  [application/application-handler-module-id])))))
      (with-redefs [module-test-modules
                    [{:biff.core/id application/application-handler-module-id
                      :biff.core/start identity}]]
        (is (= :invalid-application-handler-module
               (error-kind
                #(application/start-biff-application!
                  assembly
                  {:biff.ring/handler canonical-start-module-handler}
                  #'module-test-modules
                  [application/application-handler-module-id]))))))))

(deftest canonical-biff-start-requires-gesso-module-first-in-start-order
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [other-id :fixture/other-lifecycle-module
            other-module
            {:biff.core/id other-id
             :biff.core/start identity}]
        (with-redefs [module-test-modules
                      [application/application-handler-module
                       other-module]]
          (is (= :application-handler-module-not-first
                 (error-kind
                  #(application/start-biff-application!
                    assembly
                    {:biff.ring/handler canonical-start-module-handler}
                    #'module-test-modules
                    [other-id application/application-handler-module-id])))))))))

(deftest canonical-biff-start-rechecks-assembly-currentness-before-start
  (with-closed-application
    (fn [{:keys [assembly artifact-path]}]
      (spit artifact-path "// stale before canonical Biff start\n")
      (is (= :invalid-biff-application-start-assembly
             (error-kind
              #(application/start-biff-application!
                assembly
                {:biff.ring/handler canonical-start-module-handler}
                #'module-test-modules
                [application/application-handler-module-id])))))))

(deftest canonical-biff-start-installed-handler-retains-per-response-currentness
  (let [operations (standard-operations)
        ctx (render-context operations)]
    (with-closed-application
      (fn [{:keys [assembly artifact-path]}]
        (let [started
              (application/start-biff-application!
               assembly
               {:biff.ring/handler (valid-lifecycle-handler ctx)}
               #'module-test-modules
               [application/application-handler-module-id])
              handler (:biff.ring/handler started)]
          (is (= 200 (:status (handler {:uri "/before"}))))
          (spit artifact-path "// stale after canonical Biff start\n")
          (is (= :rendered-surface-preflight-failed
                 (error-kind #(handler {:uri "/after"})))))))))

(deftest canonical-biff-start-accepts-sequential-start-order-and-preserves-order
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [events (atom [])
            first-id :fixture/first-lifecycle-module
            second-id :fixture/second-lifecycle-module
            first-module
            {:biff.core/id first-id
             :biff.core/start
             (fn [system]
               (swap! events conj :first)
               (assoc system :fixture/first true))}
            second-module
            {:biff.core/id second-id
             :biff.core/start
             (fn [system]
               (swap! events conj :second)
               (assoc system :fixture/second true))}]
        (with-redefs [module-test-modules
                      [application/application-handler-module
                       first-module
                       second-module]]
          (let [started
                (application/start-biff-application!
                 assembly
                 {:biff.ring/handler canonical-start-module-handler
                  :fixture/original :kept}
                 #'module-test-modules
                 (list application/application-handler-module-id
                       first-id
                       second-id))]
            (is (= [:first :second] @events))
            (is (= :kept (:fixture/original started)))
            (is (true? (:fixture/first started)))
            (is (true? (:fixture/second started)))))))))

(deftest later-biff-module-deliberately-replacing-handler-remains-explicit-escape-hatch
  (let [operations (standard-operations)
        ctx (render-context operations)]
    (with-closed-application
      (fn [{:keys [assembly]}]
        (let [original (valid-lifecycle-handler ctx)
              replacement (invalid-lifecycle-handler ctx)
              saw-checked-handler? (atom false)
              replacement-id :fixture/replace-checked-handler
              replacement-module
              {:biff.core/id replacement-id
               :biff.core/start
               (fn [system]
                 (reset!
                  saw-checked-handler?
                  (and (fn? (:biff.ring/handler system))
                       (not (identical?
                             original
                             (:biff.ring/handler system)))))
                 (assoc system :biff.ring/handler replacement))}]
          (with-redefs [module-test-modules
                        [application/application-handler-module
                         replacement-module]]
            (let [started
                  (application/start-biff-application!
                   assembly
                   {:biff.ring/handler original}
                   #'module-test-modules
                   [application/application-handler-module-id
                    replacement-id])]
              (is (true? @saw-checked-handler?))
              (is (identical? replacement (:biff.ring/handler started)))
              ;; Deliberate replacement occurs after the canonical Gesso module,
              ;; so this response is intentionally outside the canonical guarantee.
              (is (= 200
                     (:status
                      ((:biff.ring/handler started)
                       {:uri "/explicit-escape"})))))))))))

;; =============================================================================
;; v640 consolidated application explanation contract
;; =============================================================================

(defn- escape-hatches-by-kind
  [explanation]
  (into {}
        (map (juxt :kind identity))
        (:explicit-escape-hatches explanation)))

(deftest consolidated-explanation-separates-closure-enforcement-obligations-assumptions-and-escapes
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [explanation (application/explain assembly)
            enforcement (:canonical-html-enforcement explanation)
            obligations (:open-obligations explanation)
            assumptions (:trusted-assumptions explanation)
            escapes (:explicit-escape-hatches explanation)
            escape-kinds (set (map :kind escapes))]
        (is (= :application-runtime-backbone-preflight-closed
               (:guarantee explanation)))
        (is (= :available-on-canonical-gesso-biff-startup
               (:status enforcement)))
        (is (= :gesso.live.application-preflight/start-biff-application!
               (:startup-boundary enforcement)))
        (is (= :biff.ring/handler (:biff-handler-key enforcement)))
        (is (= :gesso.live.application-preflight/wrap-application-handler
               (:handler-boundary enforcement)))
        (is (= :gesso.http/html-response (:response-boundary enforcement)))
        (is (= :rendered-surface-pre-browser-preflight-closed
               (:per-render-guarantee enforcement)))
        (is (= :not-carried-by-application-assembly
               (:installation-proof enforcement)))
        (is (= #{:direct-biff-start-bypass
                 :later-handler-replacement
                 :server-ignores-biff-ring-handler
                 :pre-serialized-html-ring-response}
               escape-kinds))
        (is (= #{:static-affordance-closure-not-yet-modeled}
               (set (map :kind obligations))))
        (is (set? assumptions))
        (is (not-any? assumptions escape-kinds))
        (is (not-any? (set (map :kind obligations)) escape-kinds))
        (is (not (contains? assembly :canonical-html-enforcement)))
        (is (not (contains? assembly :explicit-escape-hatches)))
        (is (not (contains? assembly :deployment-proof)))))))

(deftest historical-obligation-identifiers-remain-stable-but-messages-describe-current-deployment-boundary
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [obligation (first (application/open-obligations assembly))]
        (is (= :static-affordance-closure-not-yet-modeled (:kind obligation)))
        (is (= :rendered-affordance->semantic-operation (:edge obligation)))
        (is (= :open (:status obligation)))
        (is (re-find #"Canonical Gesso/Biff startup"
                     (:message obligation)))
        (is (re-find #"ApplicationAssembly alone"
                     (:message obligation)))
        (is (re-find #"deployment"
                     (:message obligation))))))
  (let [operations (standard-operations)
        ctx (render-context operations)
        surfaces
        {:board
         [:main
          (rendered-operation-button
           ctx
           :request/claim
           "/operations/request/claim")]}]
    (with-surfaced-application
      surfaces
      (fn [{:keys [assembly]}]
        (let [obligation (first (application/open-obligations assembly))]
          (is (= :rendered-surface-enumeration-completeness-not-yet-modeled
                 (:kind obligation)))
          (is (= :application->rendered-surfaces (:edge obligation)))
          (is (= :open (:status obligation)))
          (is (re-find #"Canonical Gesso/Biff startup"
                       (:message obligation)))
          (is (re-find #"ApplicationAssembly alone"
                       (:message obligation)))
          (is (re-find #"raw-response/handler-replacement escape hatches"
                       (:message obligation))))))))

(deftest application-and-operation-explanations-share-one-canonical-enforcement-contract
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [application-explanation (application/explain assembly)
            claim (application/explain-operation assembly :request/claim)
            enforcement (:canonical-html-enforcement application-explanation)]
        (is (= enforcement (:canonical-html-enforcement claim)))
        (is (= :application-runtime-backbone-preflight-closed
               (:guarantee claim)))
        (is (= :request/claim (:operation claim)))
        (is (= :not-carried-by-application-assembly
               (get-in claim [:canonical-html-enforcement :installation-proof])))
        (is (not (contains? claim :deployment-proof)))
        (is (not (contains? claim :explicit-escape-hatches)))))))

(deftest explanation-of-recognized-report-carries-enforcement-and-escape-boundaries-without-promoting-them-into-analysis
  (with-closed-application
    (fn [{:keys [report assembly]}]
      (let [from-report (application/explain report)
            from-assembly (application/explain assembly)]
        (is (= (:canonical-html-enforcement from-assembly)
               (:canonical-html-enforcement from-report)))
        (is (= (:explicit-escape-hatches from-assembly)
               (:explicit-escape-hatches from-report)))
        (is (= (:open-obligations from-assembly)
               (get-in from-report [:analysis :open-obligations])))
        (is (= (:trusted-assumptions from-assembly)
               (get-in from-report [:analysis :trusted-assumptions])))
        (is (not (contains? (:analysis from-report)
                            :canonical-html-enforcement)))
        (is (not (contains? (:analysis from-report)
                            :explicit-escape-hatches)))
        (is (not (contains? (:analysis from-report)
                            :deployment-proof)))))))

(deftest application-report-recognition-rejects-obligation-and-assumption-tampering-after-consolidation
  (with-closed-application
    (fn [{:keys [report]}]
      (let [forged-obligation
            (assoc-in report
                      [:analysis :open-obligations 0 :status]
                      :closed)
            forged-message
            (assoc-in report
                      [:analysis :open-obligations 0 :message]
                      "deployment proven")
            forged-assumptions
            (assoc-in report
                      [:analysis :trusted-assumptions]
                      #{})]
        (is (application/report? report))
        (is (not (application/report? forged-obligation)))
        (is (not (application/report? forged-message)))
        (is (not (application/report? forged-assumptions)))
        (is (= :unrecognized-value
               (error-kind #(application/explain forged-obligation))))
        (is (= :unrecognized-value
               (error-kind #(application/explain forged-message))))
        (is (= :unrecognized-value
               (error-kind #(application/explain forged-assumptions))))))))

(deftest canonical-startup-does-not-mutate-application-assembly-into-a-deployment-attestation
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [before (application/explain assembly)
            started
            (application/start-biff-application!
             assembly
             {:biff.ring/handler canonical-start-module-handler
              :fixture/started true}
             #'module-test-modules
             [application/application-handler-module-id])
            after (application/explain assembly)]
        (is (true? (:fixture/started started)))
        (is (fn? (:biff.ring/handler started)))
        (is (= before after))
        (is (= :not-carried-by-application-assembly
               (get-in after
                       [:canonical-html-enforcement
                        :installation-proof])))
        (is (= #{:direct-biff-start-bypass
                 :later-handler-replacement
                 :server-ignores-biff-ring-handler
                 :pre-serialized-html-ring-response}
               (set (map :kind (:explicit-escape-hatches after)))))
        (is (= :open
               (:status (first (:open-obligations after)))))
        (is (not (contains? assembly :deployment-proof)))))))

(deftest consolidated-escape-hatches-have-distinct-explicit-boundaries
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [by-kind (escape-hatches-by-kind (application/explain assembly))]
        (is (= :application-startup
               (get-in by-kind [:direct-biff-start-bypass :boundary])))
        (is (= :biff-module-start-order
               (get-in by-kind [:later-handler-replacement :boundary])))
        (is (= :server-integration
               (get-in by-kind [:server-ignores-biff-ring-handler :boundary])))
        (is (= :html-serialization
               (get-in by-kind [:pre-serialized-html-ring-response :boundary])))
        (is (every? string? (map :description (vals by-kind))))
        (is (every? seq (map :description (vals by-kind))))))))

;; =============================================================================
;; v641 whole-application contradiction campaign
;; =============================================================================

(defn- contradiction-report
  [operation-acquisition-assembly artifact-path]
  (application/check-application-assembly
   (application-options operation-acquisition-assembly artifact-path)))

(defn- assert-structural-contradiction-rejected!
  [report]
  (is (application/report? report))
  (is (not (application/valid? report)))
  (is (contains? (error-kinds report)
                 :invalid-operation-acquisition-assembly)))

(deftest whole-application-campaign-rejects-browser-plan-correspondence-corruption
  (with-closed-application
    (fn [{:keys [operation-acquisition-assembly artifact-path]}]
      (let [tampered
            (assoc-in
             operation-acquisition-assembly
             [:execution-assembly
              :route-assembly
              :operation-assembly
              :operations
              :request/claim
              :browser-plan-digest]
             "forged-browser-plan-digest")
            report (contradiction-report tampered artifact-path)]
        (assert-structural-contradiction-rejected! report)
        (is (= :application-backbone-preflight-failed
               (error-kind
                #(application/require-application-assembly!
                  (application-options tampered artifact-path)))))))))

(deftest whole-application-campaign-rejects-execution-capability-summary-corruption
  (with-closed-application
    (fn [{:keys [operation-acquisition-assembly artifact-path]}]
      (let [tampered
            (update-in
             operation-acquisition-assembly
             [:execution-assembly
              :execution-capabilities
              :request/claim]
             conj
             :forged-capability)
            report (contradiction-report tampered artifact-path)]
        (assert-structural-contradiction-rejected! report)
        (is (= {}
               (get-in report [:analysis :operations])))))))

(deftest whole-application-campaign-rejects-settlement-contract-corruption
  (with-closed-application
    (fn [{:keys [operation-acquisition-assembly artifact-path]}]
      (let [tampered
            (assoc-in
             operation-acquisition-assembly
             [:execution-assembly
              :settlement-contracts
              :request/claim]
             {:forged :settlement-contract})
            report (contradiction-report tampered artifact-path)]
        (assert-structural-contradiction-rejected! report)
        (is (= {}
               (get-in report [:analysis :operations])))))))

(deftest whole-application-campaign-treats-command-route-path-as-trusted-until-a-rendered-affordance-disagrees
  (with-closed-application
    (fn [{:keys [operation-acquisition-assembly artifact-path]}]
      (let [new-path "/operations/request/claim-v2"
            changed
            (assoc-in
             operation-acquisition-assembly
             [:execution-assembly
              :route-assembly
              :routes
              :request/claim
              :path]
             new-path)
            report (contradiction-report changed artifact-path)
            assembly
            (application/require-application-assembly!
             (application-options changed artifact-path))
            ctx (render-context (standard-operations))
            old-surface
            [:main
             (rendered-operation-button
              ctx :request/claim "/operations/request/claim")]
            new-surface
            [:main
             (rendered-operation-button ctx :request/claim new-path)]]
        (testing "a well-formed route declaration is an application-owned physical fact"
          (is (application/valid? report))
          (is (= new-path
                 (get-in (application/explain-operation assembly :request/claim)
                         [:routes 0 :path]))))
        (testing "the independent rendered affordance makes an old physical URL contradictory"
          (is (= #{:rendered-affordance-path-mismatch}
                 (error-kinds
                  (application/check-rendered-surface
                   assembly :old-request-board old-surface))))
          (is (true?
               (:valid?
                (application/check-rendered-surface
                 assembly :new-request-board new-surface)))))))))

(deftest whole-application-campaign-keeps-acquisition-url-as-an-explicit-trusted-physical-boundary
  (with-closed-application
    (fn [{:keys [operation-acquisition-assembly artifact-path]}]
      (let [new-path "/fragments/request-list-v2"
            changed
            (assoc-in
             operation-acquisition-assembly
             [:acquisition-assembly
              :realizations
              :request-list
              :fragment-route
              :path]
             new-path)
            report (contradiction-report changed artifact-path)
            assembly
            (application/require-application-assembly!
             (application-options changed artifact-path))
            claim (application/explain-operation assembly :request/claim)
            explanation (application/explain assembly)]
        (is (application/valid? report))
        (is (= new-path
               (get-in claim
                       [:authoritative-acquisitions
                        :request-list
                        :fragment-route
                        :path])))
        (is (contains? (:trusted-assumptions explanation)
                       :trusted-acquisition-route-declarations-match-installed-handlers))))))

(deftest whole-application-campaign-rejects-rendered-affordance-physical-route-contradiction-before-browser-delivery
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [ctx (render-context (standard-operations))
            surface
            [:main
             (rendered-operation-button
              ctx
              :request/claim
              "/operations/request/not-claim")]
            rendered? (atom false)
            error
            (error-data
             #(application/checked-rendered-response!
               assembly
               :contradictory-request-board
               (fn [_]
                 (reset! rendered? true)
                 {:status 200})
               surface))]
        (is (= :rendered-surface-preflight-failed
               (:error/kind error)))
        (is (= #{:rendered-affordance-path-mismatch}
               (set (map :kind (get-in error [:preflight :errors])))))
        (is (false? @rendered?))))))

(deftest whole-application-campaign-stale-artifact-destroys-previously-credible-closure
  (with-closed-application
    (fn [{:keys [artifact-path report assembly]}]
      (is (application/valid? report))
      (is (application/application-assembly? assembly))
      (spit artifact-path
            "console.log('v641 contradictory artifact');\n"
            :encoding "UTF-8")
      (is (not (application/report? report)))
      (is (not (application/application-assembly? assembly)))
      (is (= :invalid-application-handler-assembly
             (error-kind
              #(application/wrap-application-handler
                assembly
                (fn [_] {:status 200}))))))))

(deftest whole-application-campaign-does-not-misclassify-a-new-valid-trusted-publication-declaration-as-forgery
  (let [operations
        (assoc
         (standard-operations)
         :request/claim
         (trusted-operation
          :request/claim
          {:published-change-topics #{:audit}}))
        operation-acquisition'
        (operation-acquisition-assembly operations)]
    (with-temp-dir
     (fn [dir]
       (let [artifact-path (child-path dir "trusted-publication.js")]
         (record-artifact! operation-acquisition' artifact-path)
         (let [report
               (application/check-application-assembly
                (application-options operation-acquisition' artifact-path))
               assembly
               (application/require-application-assembly!
                (application-options operation-acquisition' artifact-path))
               claim (application/explain-operation assembly :request/claim)]
           (is (application/valid? report))
           (is (= #{:audit} (:published-change-topics claim)))
           (is (= #{:audit-scope} (:affected-scopes claim)))
           (is (= #{} (:affected-fragments claim)))
           (is (contains?
                (:trusted-assumptions (application/explain assembly))
                :application-publication-declarations-match-model-publication))))))))

(deftest whole-application-campaign-keeps-pre-serialized-raw-ring-html-an-explicit-escape-hatch
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [raw-response
            {:status 200
             :headers {"content-type" "text/html; charset=utf-8"}
             :body "<button data-forged='true'>raw html</button>"}
            wrapped
            (application/wrap-application-handler
             assembly
             (fn [_] raw-response))
            response (wrapped {:uri "/raw"})
            explanation (application/explain assembly)]
        (is (= raw-response response))
        (is (contains?
             (set (map :kind (:explicit-escape-hatches explanation)))
             :pre-serialized-html-ring-response))
        (is (= :not-carried-by-application-assembly
               (get-in explanation
                       [:canonical-html-enforcement :installation-proof])))))))

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
