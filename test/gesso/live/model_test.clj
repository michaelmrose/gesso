(ns gesso.live.model-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.core :as live]
   [gesso.live.model :as model]))

;; -----------------------------------------------------------------------------
;; Test helpers
;; -----------------------------------------------------------------------------

(defn allow
  [_ctx _id]
  true)

(defn deny
  [_ctx _id]
  false)

(defn html-response
  [node]
  {:status 200
   :headers {"content-type" "text/html; charset=utf-8"}
   :body node})

(defn store-queue-query
  [ctx store-id]
  {:ctx-marker (:ctx-marker ctx)
   :fragment/id (str "store-queue-" store-id)
   :store/id store-id
   :open-tasks [{:task/id "task-1"
                 :task/title "Find paint"
                 :task/status :requested}]})

(defn store-queue-render
  [{:keys [fragment/id open-tasks]}]
  [:section {:id id}
   [:h2 "Store queue"]
   [:ul
    (for [task open-tasks]
      [:li (:task/title task)])]])

(defn helper-query
  [_ctx helper-id]
  {:fragment/id (str "helper-panel-" helper-id)
   :helper/id helper-id
   :active-task {:task/id "task-2"
                 :task/title "Carry lumber"
                 :task/status :assigned}})

(defn helper-render
  [{:keys [fragment/id active-task]}]
  [:section {:id id}
   [:h2 "Helper panel"]
   [:p (:task/title active-task)]])

(defn var-backed-render
  [{:keys [fragment/id]}]
  [:section {:id id}
   "Var backed render"])

(def base-app
  {:response html-response

   :scopes
   {:store-queue
    {:topic :humanhelp/store-queue
     :id-key :store/id
     :authorized? allow}

    :helper-panel
    {:topic :humanhelp/helper
     :id-key :helper/id
     :label "Helper panel"
     :authorized? allow}

    :customer-status
    {:topic :humanhelp/customer
     :id-key :customer/id
     :authorized? allow}

    :public-feed
    {:topic :humanhelp/public-feed
     :id-key :feed/id
     :public? true}}

   :graph
   {:task/assigned
    [:store-queue
     [:helper-panel :helper/id]
     {:scope :customer-status
      :id-key :customer/id}]

    :task/assigned-duplicate
    [:store-queue
     {:scope :store-queue
      :id-key :store/id}]

    :public/changed
    [:public-feed]

    :optional/missing
    [{:scope :store-queue
      :id-key :store/id
      :optional? true}]

    :conditional/change
    [{:scope :store-queue
      :id-key :store/id
      :when (fn [_ctx change]
              (:notify? change))}]}

   :fragments
   {:store-queue
    {:scope :store-queue
     :query store-queue-query
     :render store-queue-render
     :consistency {:read :after-triggering-write}
     :request-policy {:in-flight :one
                      :queued :last
                      :stale-window-ms 750}}

    :helper-panel
    {:scope :helper-panel
     :id-fn (fn [helper-id]
              (str "helper-panel-" helper-id))
     :query helper-query
     :render helper-render}

    :var-backed
    {:scope :store-queue
     :query #'store-queue-query
     :render #'var-backed-render}}})

(defn thrown-data
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e
      (ex-data e))))

(defn compile-errors
  [app]
  (:errors
   (thrown-data
    #(model/compile-live-app app))))

(defn has-error?
  ([errors kind]
   (boolean
    (some #(= kind (:kind %)) errors)))
  ([errors kind path]
   (boolean
    (some #(and (= kind (:kind %))
                (= path (:path %)))
          errors))))

(defn compiled-app
  []
  (model/compile-live-app base-app))

(defn scope-keys
  [scopes]
  (set (map model/scope-key scopes)))

;; -----------------------------------------------------------------------------
;; Compilation and normalization
;; -----------------------------------------------------------------------------

(deftest compile-live-app-normalizes-and-compiles
  (let [compiled (compiled-app)]
    (is (:compiled? compiled))

    (testing "scope defaults"
      (is (= :store-queue
             (get-in compiled [:scopes :store-queue :name])))
      (is (= "store queue"
             (get-in compiled [:scopes :store-queue :label])))
      (is (= "Helper panel"
             (get-in compiled [:scopes :helper-panel :label]))))

    (testing "graph shorthand targets are normalized"
      (is (= [{:scope :store-queue
               :id-key :store/id}
              {:scope :helper-panel
               :id-key :helper/id}
              {:scope :customer-status
               :id-key :customer/id}]
             (get-in compiled [:graph :task/assigned]))))

    (testing "fragment defaults"
      (is (= :outerHTML
             (get-in compiled [:fragments :store-queue :swap])))
      (is (= :default-safe-live-fragment
             (get-in compiled [:fragments :helper-panel :request-policy])))
      (is (fn? (get-in compiled [:fragments :store-queue :id-fn]))))

    (testing "compiled rules exist"
      (is (= #{:task/assigned
               :task/assigned-duplicate
               :public/changed
               :optional/missing
               :conditional/change}
             (set (map :when-topic (model/live-rules compiled))))))))

(deftest explain-live-app-returns-non-runtime-summary
  (let [compiled (compiled-app)
        explained (model/explain-live-app compiled)]
    (is (:compiled? explained))
    (is (= :humanhelp/store-queue
           (get-in explained [:scopes :store-queue :topic])))
    (is (= :store-queue
           (get-in explained [:fragments :store-queue :scope])))
    (is (= #{:task/assigned
             :task/assigned-duplicate
             :public/changed
             :optional/missing
             :conditional/change}
           (set (:events explained))))
    (is (= {:read :after-triggering-write}
           (get-in explained [:fragments :store-queue :consistency])))
    (is (= {:in-flight :one
            :queued :last
            :stale-window-ms 750}
           (get-in explained [:fragments :store-queue :request-policy])))))

;; -----------------------------------------------------------------------------
;; Malli structural validation
;; -----------------------------------------------------------------------------

(deftest structural-validation-requires-top-level-keys
  (let [errors (compile-errors {})]
    (is (has-error? errors :malli/schema))
    (is (= :gesso.live/validation-failed
           (:error/type
            (thrown-data #(model/compile-live-app {})))))))

(deftest structural-validation-requires-scope-auth-unless-public
  (testing "non-public scope without auth fails"
    (let [bad-app (update-in base-app
                             [:scopes :store-queue]
                             dissoc
                             :authorized?)
          errors (compile-errors bad-app)]
      (is (has-error? errors :malli/schema))))

  (testing "public scope without auth passes"
    (let [app (assoc-in base-app
                        [:scopes :store-queue]
                        {:topic :humanhelp/store-queue
                         :id-key :store/id
                         :public? true})]
      (is (:compiled? (model/compile-live-app app))))))

(deftest structural-validation-requires-callable-query-render-and-response
  (testing "bad fragment query fails"
    (let [bad-app (assoc-in base-app
                            [:fragments :store-queue :query]
                            :not-a-function)
          errors (compile-errors bad-app)]
      (is (has-error? errors :malli/schema))))

  (testing "bad fragment render fails"
    (let [bad-app (assoc-in base-app
                            [:fragments :store-queue :render]
                            :not-a-function)
          errors (compile-errors bad-app)]
      (is (has-error? errors :malli/schema))))

  (testing "bad root response fails"
    (let [bad-app (assoc base-app :response :not-a-function)
          errors (compile-errors bad-app)]
      (is (has-error? errors :malli/schema)))))

(deftest structural-validation-rejects-malformed-graph-targets
  (let [bad-app (assoc-in base-app
                          [:graph :task/assigned]
                          ["not a valid target"])
        errors (compile-errors bad-app)]
    (is (has-error? errors :malli/schema))))

;; -----------------------------------------------------------------------------
;; Semantic validation
;; -----------------------------------------------------------------------------

(deftest semantic-validation-catches-unknown-graph-scope-clearly
  (let [bad-app (assoc-in base-app
                          [:graph :task/assigned]
                          [{:scope :store-qeue}])
        errors (compile-errors bad-app)]
    (is (has-error? errors
                    :unknown-scope
                    [:graph :task/assigned 0 :scope]))
    ;; The important regression: unknown scope should not be masked primarily as
    ;; a missing :id-key problem.
    (is (not (has-error? errors
                         :missing-id-key
                         [:graph :task/assigned 0 :id-key])))))

(deftest semantic-validation-catches-unknown-fragment-scope
  (let [bad-app (assoc-in base-app
                          [:fragments :store-queue :scope]
                          :store-qeue)
        errors (compile-errors bad-app)]
    (is (has-error? errors
                    :unknown-scope
                    [:fragments :store-queue :scope]))))

(deftest semantic-validation-catches-duplicate-topics
  (let [bad-app (assoc-in base-app
                          [:scopes :store-queue-copy]
                          {:topic :humanhelp/store-queue
                           :id-key :store/id
                           :authorized? allow})
        errors (compile-errors bad-app)]
    (is (has-error? errors :duplicate-topic [:scopes]))))

(deftest semantic-validation-catches-known-target-that-cannot-inherit-id-key
  (let [bad-app (-> base-app
                    (assoc-in [:scopes :broken-scope]
                              {:topic :humanhelp/broken
                               ;; structurally invalid scope, but this test
                               ;; also verifies the semantic graph checker.
                               :authorized? allow})
                    (assoc-in [:graph :broken/event]
                              [{:scope :broken-scope}]))
        errors (compile-errors bad-app)]
    (is (has-error? errors :malli/schema))
    (is (has-error? errors
                    :missing-id-key
                    [:graph :broken/event 0 :id-key]))))

;; -----------------------------------------------------------------------------
;; Scope construction and graph expansion
;; -----------------------------------------------------------------------------

(deftest scope-builds-runtime-compatible-map
  (let [compiled (compiled-app)
        scope (model/scope compiled :store-queue "store-42")]
    (is (= {:topic :humanhelp/store-queue
            :id "store-42"}
           (select-keys scope [:topic :id])))
    (is (= :store-queue
           (:gesso.live/scope scope)))
    (is (= [:humanhelp/store-queue "store-42"]
           (model/scope-key scope)))))

(deftest unknown-scope-throws
  (let [compiled (compiled-app)
        data (thrown-data #(model/scope compiled :missing "x"))]
    (is (= :missing (:scope data)))
    (is (some? (:known-scopes data)))))

(deftest expand-change-produces-expected-runtime-scopes
  (let [compiled (compiled-app)
        scopes (model/expand-change
                compiled
                {:topic :task/assigned
                 :store/id "store-42"
                 :helper/id "helper-7"
                 :customer/id "customer-9"
                 :task/id "task-1"})]
    (is (= #{[:humanhelp/store-queue "store-42"]
             [:humanhelp/helper "helper-7"]
             [:humanhelp/customer "customer-9"]}
           (scope-keys scopes)))))

(deftest expand-change-dedupes-scopes
  (let [compiled (compiled-app)
        scopes (model/expand-change
                compiled
                {:topic :task/assigned-duplicate
                 :store/id "store-42"})]
    (is (= [{:topic :humanhelp/store-queue
             :id "store-42"}]
           (mapv #(select-keys % [:topic :id]) scopes)))))

(deftest expand-changes-dedupes-across-many-changes
  (let [compiled (compiled-app)
        scopes (model/expand-changes
                compiled
                [{:topic :task/assigned-duplicate
                  :store/id "store-42"}
                 {:topic :task/assigned-duplicate
                  :store/id "store-42"}])]
    (is (= [{:topic :humanhelp/store-queue
             :id "store-42"}]
           (mapv #(select-keys % [:topic :id]) scopes)))))

(deftest compiled-live-rules-expand-like-current-rules
  (let [compiled (compiled-app)
        rule (first
              (filter #(= :task/assigned (:when-topic %))
                      (model/live-rules compiled)))
        expanded ((:expand rule)
                  {:ctx-marker :ctx}
                  {:topic :task/assigned
                   :store/id "store-42"
                   :helper/id "helper-7"
                   :customer/id "customer-9"})]
    (is (= #{[:humanhelp/store-queue "store-42"]
             [:humanhelp/helper "helper-7"]
             [:humanhelp/customer "customer-9"]}
           (scope-keys expanded)))))

(deftest expand-change-throws-for-unknown-event-topic
  (let [compiled (compiled-app)
        data (thrown-data
              #(model/expand-change compiled {:topic :not/in-graph}))]
    (is (= :not/in-graph (:topic data)))
    (is (some? (:known-events data)))))

(deftest expand-change-throws-for-missing-required-id-key
  (let [compiled (compiled-app)
        data (thrown-data
              #(model/expand-change
                compiled
                {:topic :task/assigned
                 :helper/id "helper-7"
                 :customer/id "customer-9"}))]
    (is (= :store/id (:id-key data)))))

(deftest optional-target-may-be-missing
  (let [compiled (compiled-app)]
    (is (= []
           (model/expand-change
            compiled
            {:topic :optional/missing})))))

(deftest conditional-targets-can-skip-or-emit
  (let [compiled (compiled-app)]
    (is (= []
           (model/expand-change
            compiled
            {:topic :conditional/change
             :store/id "store-42"
             :notify? false})))
    (is (= #{[:humanhelp/store-queue "store-42"]}
           (scope-keys
            (model/expand-change
             compiled
             {:topic :conditional/change
              :store/id "store-42"
              :notify? true}))))))

;; -----------------------------------------------------------------------------
;; Authorization
;; -----------------------------------------------------------------------------

(deftest authorization-allows-public-scopes
  (let [compiled (compiled-app)]
    (is (model/authorized-for-scope?
         compiled
         {}
         :public-feed
         "global"))
    (is (model/require-scope-authorized!
         compiled
         {}
         :public-feed
         "global"))))

(deftest authorization-calls-scope-predicate
  (let [compiled (model/compile-live-app
                  (assoc-in base-app
                            [:scopes :store-queue :authorized?]
                            deny))]
    (is (false?
         (model/authorized-for-scope?
          compiled
          {}
          :store-queue
          "store-42")))
    (is (= :gesso.live/not-authorized
           (:error/type
            (thrown-data
             #(model/require-scope-authorized!
               compiled
               {}
               :store-queue
               "store-42")))))))

;; -----------------------------------------------------------------------------
;; Fragment descriptors, query, render, response
;; -----------------------------------------------------------------------------

(deftest fragment-descriptor-and-dom-id-work
  (let [compiled (compiled-app)]
    (is (= :store-queue
           (:scope (model/fragment-descriptor compiled :store-queue))))
    (is (= "store-queue-store-42"
           (model/fragment-dom-id compiled :store-queue "store-42")))
    (is (= "helper-panel-helper-7"
           (model/fragment-dom-id compiled :helper-panel "helper-7")))))

(deftest unknown-fragment-throws
  (let [compiled (compiled-app)
        data (thrown-data #(model/fragment-descriptor compiled :missing))]
    (is (= :missing (:fragment data)))
    (is (some? (:known-fragments data)))))

(deftest query-fragment-returns-data-map
  (let [compiled (compiled-app)
        ctx {:ctx-marker :ctx}
        data (model/query-fragment compiled ctx :store-queue "store-42")]
    (is (= :ctx (:ctx-marker data)))
    (is (= "store-42" (:store/id data)))
    (is (= "store-queue-store-42" (:fragment/id data)))))

(deftest render-fragment-node-passes-only-data-to-render
  (let [compiled (compiled-app)
        ctx {:ctx-marker :ctx}
        node (model/render-fragment-node compiled ctx :store-queue "store-42")]
    (is (= :section (first node)))
    (is (= "store-queue-store-42"
           (get-in node [1 :id])))))

(deftest var-backed-query-and-render-work
  (let [compiled (compiled-app)
        node (model/render-fragment-node compiled {} :var-backed "store-42")]
    (is (= :section (first node)))
    (is (= "store-queue-store-42"
           (get-in node [1 :id])))))

(deftest render-fragment-response-uses-compiled-response
  (let [compiled (compiled-app)
        response (model/render-fragment-response
                  compiled
                  {}
                  :store-queue
                  "store-42")]
    (is (= 200 (:status response)))
    (is (= :section (first (:body response))))))

(deftest render-fragment-response-allows-explicit-response-override
  (let [compiled (model/compile-live-app (dissoc base-app :response))
        response (model/render-fragment-response
                  compiled
                  {}
                  :store-queue
                  "store-42"
                  {:response (fn [node]
                               {:status 299
                                :body [:wrapped node]})})]
    (is (= 299 (:status response)))
    (is (= :wrapped (first (:body response))))))

(deftest render-fragment-response-requires-response-renderer
  (let [compiled (model/compile-live-app (dissoc base-app :response))
        data (thrown-data
              #(model/render-fragment-response
                compiled
                {}
                :store-queue
                "store-42"))]
    (is (= :gesso.live.model/missing-response-renderer
           (:error/type data)))))

(deftest render-fragment-response-can-skip-auth-when-route-already-checked
  (let [compiled (model/compile-live-app
                  (assoc-in base-app
                            [:scopes :store-queue :authorized?]
                            deny))
        response (model/render-fragment-response
                  compiled
                  {}
                  :store-queue
                  "store-42"
                  {:authorize? false})]
    (is (= 200 (:status response)))))

;; -----------------------------------------------------------------------------
;; Delegation to existing gesso.live UI/runtime helpers
;; -----------------------------------------------------------------------------

(deftest fragment-runtime-descriptor-only-passes-current-ui-keys
  (let [compiled (compiled-app)]
    (with-redefs [live/->fragment identity]
      (let [runtime-fragment
            (model/fragment->runtime-fragment
             compiled
             :store-queue
             "store-42"
             {:fragment-url "/app/stores/store-42/queue/fragment"
              :stream-url "/app/stores/store-42/queue/stream"})]
        (is (= "store-queue-store-42"
               (:id runtime-fragment)))
        (is (= "/app/stores/store-42/queue/fragment"
               (:src runtime-fragment)))
        (is (= "/app/stores/store-42/queue/stream"
               (:stream-url runtime-fragment)))
        (is (= {:topic :humanhelp/store-queue
                :id "store-42"}
               (select-keys (:subscription runtime-fragment) [:topic :id])))
        (is (= :outerHTML
               (:swap runtime-fragment)))

        ;; These remain model metadata only until gesso.live.ui supports them.
        (is (not (contains? runtime-fragment :request-policy)))
        (is (not (contains? runtime-fragment :consistency)))))))

(deftest fragment-runtime-descriptor-requires-explicit-urls
  (let [compiled (compiled-app)]
    (is (some?
         (thrown-data
          #(model/fragment->runtime-fragment
            compiled
            :store-queue
            "store-42"
            {:stream-url "/stream"}))))
    (is (some?
         (thrown-data
          #(model/fragment->runtime-fragment
            compiled
            :store-queue
            "store-42"
            {:fragment-url "/fragment"}))))))

(deftest fragment-panel-delegates-to-current-live-ui
  (let [compiled (compiled-app)]
    (with-redefs [live/->fragment identity
                  live/fragment-panel (fn [fragment]
                                        [:panel fragment])]
      (let [panel (model/fragment-panel
                   compiled
                   :store-queue
                   "store-42"
                   {:fragment-url "/fragment"
                    :stream-url "/stream"})]
        (is (= :panel (first panel)))
        (is (= "/fragment"
               (get-in panel [1 :src])))
        (is (= "/stream"
               (get-in panel [1 :stream-url])))))))

(deftest start-fragment-stream-checks-auth-and-delegates-to-live
  (let [compiled (compiled-app)]
    (with-redefs [live/start-sse!
                  (fn [system subscription opts]
                    {:response {:status 200
                                :body {:system system
                                       :subscription subscription
                                       :opts opts}}})]
      (let [response (model/start-fragment-stream
                      compiled
                      {}
                      ::system
                      :store-queue
                      "store-42"
                      {:flow-options {:relieve? true}})]
        (is (= 200 (:status response)))
        (is (= ::system
               (get-in response [:body :system])))
        (is (= {:topic :humanhelp/store-queue
                :id "store-42"}
               (select-keys
                (get-in response [:body :subscription])
                [:topic :id])))
        (is (= {:flow-options {:relieve? true}}
               (get-in response [:body :opts])))))))

(deftest start-fragment-stream-denies-unauthorized-scope
  (let [compiled (model/compile-live-app
                  (assoc-in base-app
                            [:scopes :store-queue :authorized?]
                            deny))]
    (with-redefs [live/start-sse!
                  (fn [& _]
                    (throw
                     (ex-info "should not be called" {})))]
      (is (= :gesso.live/not-authorized
             (:error/type
              (thrown-data
               #(model/start-fragment-stream
                 compiled
                 {}
                 ::system
                 :store-queue
                 "store-42"))))))))

;; -----------------------------------------------------------------------------
;; Current-runtime integration shape
;; -----------------------------------------------------------------------------

(deftest model-live-rules-are-suitable-for-core-system-options
  (let [compiled (compiled-app)
        rules (model/live-rules compiled)]
    (is (sequential? rules))
    (is (every? #(contains? % :when-topic) rules))
    (is (every? #(fn? (:expand %)) rules))))

(deftest write-path-should-use-live-rules-with-current-core
  (let [compiled (compiled-app)]
    ;; This is not calling live/transact-and-notify!, because this namespace
    ;; should not need XTDB or a real system in its unit tests.
    ;;
    ;; The assertion is about shape: app handlers should pass these rules to
    ;; current core helpers rather than asking model.clj to own dispatch.
    (is (= (model/live-rules compiled)
           (:live-rules compiled)))))
