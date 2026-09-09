(ns gesso.live.application-preflight-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.identity :as identity]
   [gesso.choreo.preflight :as choreo-preflight]
   [gesso.live.acquisition-preflight :as acquisition]
   [gesso.live.application-preflight :as application]
   [gesso.live.browser.build :as browser-build]
   [gesso.live.browser.preflight :as browser-preflight]
   [gesso.live.model :as model]
   [gesso.live.operation-acquisition-preflight :as operation-acquisition]
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
