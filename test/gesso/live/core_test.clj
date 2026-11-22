(ns gesso.live.core-test
  (:require
   [clojure.edn :as edn]
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.project :as project]
   [gesso.live.consistency.xtdb :as xtdb-live]
   [gesso.live.core :as live]
   [gesso.live.dispatch :as dispatch]
   [gesso.live.fragment :as fragment]
   [gesso.live.htmx :as htmx]
   [gesso.live.optimistic.capability :as optimistic.capability]
   [gesso.live.optimistic.server :as optimistic.server]
   [gesso.live.progression :as progression]
   [gesso.live.progression.http :as progression.http]
   [gesso.live.source :as source]
   [gesso.live.synced :as synced]
   [gesso.live.transport.sse :as sse]
   [manifold.stream :as s]))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(def timeout-ms 1000)

(def request-change
  {:topic :request
   :id "req-1"
   :change/kind :updated})

(def other-request-change
  {:topic :request
   :id "req-2"
   :change/kind :updated})

(def sample-tx
  [[:put-docs :requests {:xt/id "req-1"
                         :status :open}]])

(def sample-consistency
  {:tx-id 42
   :system-time :system-time-42
   :snapshot-time :system-time-42})

(def sample-request-progression
  (progression/requirement :basis/request))

(def sample-commit-progression
  (progression/requirement :basis/commit))

(def sample-other-progression
  (progression/requirement :basis/other))

(defn start-task
  [task]
  (let [p (promise)
        cancel (task #(deliver p {:status :success :value %})
                     #(deliver p {:status :failure :error %}))]
    {:promise p
     :cancel cancel}))

(defn task-result
  [runner]
  (let [result (deref (:promise runner) timeout-ms ::timeout)]
    (when (= ::timeout result)
      ((:cancel runner)))
    result))

(defn run-task
  [task]
  (task-result (start-task task)))

(defn request-rules
  []
  [{:when-topic :request
    :expand (fn [_ctx change]
              [(select-keys change [:topic :id :change/kind])])}])

(defn xtdb-var
  [sym]
  (or (ns-resolve 'gesso.live.consistency.xtdb sym)
      (throw
       (ex-info "Missing test seam var in gesso.live.consistency.xtdb."
                {:sym sym}))))

(defn with-xtdb-stub
  [sym replacement thunk]
  (with-redefs-fn
    {(xtdb-var sym) replacement}
    thunk))

;; -----------------------------------------------------------------------------
;; System lifecycle / existing facade smoke tests
;; -----------------------------------------------------------------------------

(deftest create-builds-live-system-test
  (let [system (live/create {:rules (request-rules)
                             :source-options {:id :core-test/source}
                             :dispatch-options {:threads 1
                                                :queue-size 8
                                                :on-overflow :throw}
                             :fragment-options {:ttl-ms 1000}})]
    (try
      (is (map? system))
      (is (source/stats (:source system)))
      (is (= false (live/closed? system)))
      (is (= 1 (count (:rules system))))
      (is (= {:source true
              :dispatcher true
              :fragment-manager true}
             (:owned system)))
      (finally
        (live/close! system)))))

(deftest close-is-idempotent-test
  (let [system (live/create)]
    (is (= system (live/close! system)))
    (is (= system (live/close! system)))
    (is (live/closed? system))))

(deftest render-task-uses-system-fragment-manager-test
  (let [system (live/create {:fragment-options {:ttl-ms 1000}})
        key (live/strict-fragment-key
             {:fragment :request-panel
              :scope [:request "req-1"]
              :user-key [:user "u-1"]
              :params {:tab :summary}
              :progression sample-request-progression})
        render-count (atom 0)
        render-fn (fn []
                    (str "<section>render-" (swap! render-count inc) "</section>"))]
    (try
      (is (= {:status :success
              :value "<section>render-1</section>"}
             (run-task
              (live/render-task system key render-fn))))

      (is (= {:status :success
              :value "<section>render-1</section>"}
             (run-task
              (live/render-task system key render-fn))))

      (is (= 1 @render-count))
      (is (= 1 (get-in (live/stats system) [:fragment :cache-count])))
      (finally
        (live/close! system)))))

(deftest fragment-key-facade-delegates-to-fragment-namespace-test
  (is (= (fragment/fragment-key :request-panel)
         (live/fragment-key :request-panel)))

  (is (= (fragment/fragment-key :request-panel {:scope [:request "req-1"]})
         (live/fragment-key :request-panel {:scope [:request "req-1"]})))

  (is (= (fragment/strict-fragment-key
          {:fragment :request-panel
           :scope [:request "req-1"]
           :user-key [:user "u-1"]})
         (live/strict-fragment-key
          {:fragment :request-panel
           :scope [:request "req-1"]
           :user-key [:user "u-1"]}))))

;; -----------------------------------------------------------------------------
;; XTDB read / transaction facade
;; -----------------------------------------------------------------------------

(deftest q-facade-uses-consistent-ctx-aware-read-path-test
  (let [seen (atom nil)
        ctx {:xtdb/read-connectable :read-node
             :gesso.live/consistency {:snapshot-time :snapshot-1}}
        query ["SELECT * FROM requests WHERE _id = ?" "req-1"]]
    (with-xtdb-stub
      'q-consistent-from
      (fn
        ([ctx' query']
         (reset! seen [ctx' query'])
         [{:status :fresh}])
        ([ctx' query' opts]
         (reset! seen [ctx' query' opts])
         [{:status :fresh :opts opts}]))
      (fn []
        (is (= [{:status :fresh}]
               (live/q ctx query)))
        (is (= [ctx query]
               @seen))

        (is (= [{:status :fresh :opts {:key-fn :kebab-case-keyword}}]
               (live/q ctx query {:key-fn :kebab-case-keyword})))
        (is (= [ctx query {:key-fn :kebab-case-keyword}]
               @seen))))))

(deftest raw-xtdb-write-facades-are-not-public-core-api-test
  (let [publics (ns-publics 'gesso.live.core)]
    (testing "core keeps the authoritative Live mutation boundary public"
      (is (contains? publics 'transact-and-notify!))
      (is (contains? publics 'live-set!))
      (is (contains? publics 'live-swap!)))

    (testing "raw XTDB mutation helpers stay in the XTDB adapter instead of the Live core facade"
      (doseq [sym '[execute-tx!
                    submit-tx!
                    put-docs-op
                    delete-docs-op]]
        (is (not (contains? publics sym))
            (str "raw XTDB write helper must not be public through gesso.live.core: " sym))))

    (testing "the progression-aware read facade remains intentionally public"
      (is (contains? publics 'q)))))

;; -----------------------------------------------------------------------------
;; Consistency ctx helpers
;; -----------------------------------------------------------------------------

(deftest consistency-reads-explicit-consistency-from-ctx-test
  (is (= {:snapshot-time :snapshot-1
          :tx-id 42}
         (live/consistency
          {:gesso.live/consistency {:snapshot-time :snapshot-1
                                    :tx-id 42
                                    :ignored true}})))

  (is (= {:await-token "await-1"}
         (live/consistency
          {:xtdb/consistency {:await-token "await-1"}})))

  (is (= {}
         (live/consistency
          {:biff/node :shared-node}))))

(deftest with-consistency-assocs-normalized-consistency-test
  (is (= {:app/name :demo
          :gesso.live/consistency {:snapshot-time :snapshot-1
                                   :tx-id 42}}
         (live/with-consistency
          {:app/name :demo}
          {:snapshot-time :snapshot-1
           :tx-id 42
           :ignored true}))))

(deftest transaction-consistency-attachment-is-not-public-core-api-test
  (let [publics (ns-publics 'gesso.live.core)]
    (testing "explicit read-context consistency helpers remain public"
      (is (contains? publics 'consistency))
      (is (contains? publics 'with-consistency)))

    (testing "transaction-derived change consistency is attached only inside the mutation boundary"
      (is (contains? publics 'transact-and-notify!))
      (is (not (contains? publics 'attach-consistency))
          "change consistency attachment must remain an internal transaction step"))))

;; -----------------------------------------------------------------------------
;; Progression ctx/change helpers
;; -----------------------------------------------------------------------------

(deftest progression-reads-normalized-authoritative-requirement-from-ctx-test
  (testing "canonical request/read-context key"
    (is (= sample-request-progression
           (live/progression
            {:gesso.live/progression sample-request-progression}))))

  (testing "narrow request-local convenience key remains readable"
    (is (= sample-request-progression
           (live/progression
            {:progression sample-request-progression}))))

  (testing "absence remains absence"
    (is (nil?
         (live/progression
          {:biff/node :shared-node})))))

(deftest with-progression-owns-canonical-context-key-test
  (is (= {:app/name :demo
          :gesso.live/progression sample-request-progression}
         (live/with-progression
          {:app/name :demo}
          sample-request-progression)))

  (testing "nil removes only the canonical progression key"
    (is (= {:app/name :demo
            :progression sample-other-progression}
           (live/with-progression
            {:app/name :demo
             :gesso.live/progression sample-request-progression
             :progression sample-other-progression}
            nil)))))

(deftest authoritative-progression-attachment-is-not-public-core-api-test
  (let [publics (ns-publics 'gesso.live.core)]
    (testing "transaction-established progression remains part of the public mutation boundary"
      (is (contains? publics 'transact-and-notify!)))

    (testing "callers cannot manufacture authoritative change progression through the core facade"
      (is (not (contains? publics 'attach-progression))
          "authoritative progression attachment must remain an internal transaction-evidence step"))))

;; -----------------------------------------------------------------------------
;; HTTP progression request boundary
;; -----------------------------------------------------------------------------

(defn progression-header
  [requirement-value]
  {progression.http/request-header-name
   (progression.http/encode-request-progression requirement-value)})

(defn progression-fragment-app
  [seen]
  (live/compile-live-app
   {:response (fn [node]
                {:status 200
                 :headers {"content-type" "text/html; charset=utf-8"}
                 :body node})
    :scopes
    {:request
     {:topic :request
      :id-key :request/id
      :authorized?
      (fn [ctx id]
        (swap! seen assoc
               :authorize/id id
               :authorize/progression (live/progression ctx))
        true)}}
    :graph {}
    :fragments
    {:request-panel
     {:scope :request
      :query
      (fn [ctx id]
        (swap! seen assoc
               :query/id id
               :query/progression (live/progression ctx))
        {:fragment/id (str "request-panel-" id)
         :request/id id
         :progression (live/progression ctx)})
      :render
      (fn [{:keys [fragment/id progression]}]
        [:section {:id id
                   :data-progression (pr-str progression)}
         "Request"])}}}))

(deftest bind-request-progression-decodes-ring-and-biff-contexts-test
  (testing "raw Ring request fields bind into canonical progression"
    (let [ctx {:app/name :demo
               :headers (progression-header sample-request-progression)}
          bound (live/bind-request-progression ctx)]
      (is (= sample-request-progression
             (live/progression bound)))
      (is (= :demo (:app/name bound)))
      (is (= (:headers ctx) (:headers bound)))))

  (testing "nested Biff-style :request is decoded while the outer ctx is preserved"
    (let [ctx {:app/name :demo
               :request {:request-method :get
                         :headers (progression-header sample-request-progression)}}
          bound (live/bind-request-progression ctx)]
      (is (= sample-request-progression
             (live/progression bound)))
      (is (= (:request ctx) (:request bound)))
      (is (= :demo (:app/name bound)))))

  (testing "absence is a true no-op"
    (let [ctx {:app/name :demo
               :headers {"accept" "text/html"}}]
      (is (identical? ctx
                      (live/bind-request-progression ctx))))))

(deftest bind-request-progression-composes-with-trusted-server-requirement-test
  (let [ctx {:gesso.live/progression sample-other-progression
             :headers (progression-header sample-request-progression)}
        bound (live/bind-request-progression ctx)]
    (is (= (progression/compose
            sample-other-progression
            sample-request-progression)
           (live/progression bound)))
    (is (= (:headers ctx) (:headers bound)))))

(deftest bind-request-progression-fails-closed-on-malformed-header-test
  (let [error
        (try
          (live/bind-request-progression
           {:headers {progression.http/request-header-name "%not-valid"}})
          nil
          (catch clojure.lang.ExceptionInfo e
            e))]
    (is (some? error))
    (is (= :gesso.live.progression.http/error
           (:error/type (ex-data error))))
    (is (= :invalid-header-encoding
           (:error/kind (ex-data error))))))

(deftest standard-fragment-entry-points-bind-request-progression-test
  (let [seen (atom {})
        compiled (progression-fragment-app seen)
        ctx {:request {:headers (progression-header
                                 sample-request-progression)}}]
    (testing "query-fragment binds before the model query"
      (reset! seen {})
      (let [data (live/query-fragment compiled ctx :request-panel "req-1")]
        (is (= sample-request-progression (:progression data)))
        (is (= "req-1" (:query/id @seen)))
        (is (= sample-request-progression
               (:query/progression @seen)))))

    (testing "render-fragment-node binds before query/render"
      (reset! seen {})
      (let [node (live/render-fragment-node
                  compiled ctx :request-panel "req-2")]
        (is (= :section (first node)))
        (is (= "req-2" (:query/id @seen)))
        (is (= sample-request-progression
               (:query/progression @seen)))))

    (testing "render-fragment-response binds before authorization and query"
      (reset! seen {})
      (let [response (live/render-fragment-response
                      compiled ctx :request-panel "req-3")]
        (is (= 200 (:status response)))
        (is (= "req-3" (:authorize/id @seen)))
        (is (= sample-request-progression
               (:authorize/progression @seen)))
        (is (= "req-3" (:query/id @seen)))
        (is (= sample-request-progression
               (:query/progression @seen)))))))

(deftest render-fragment-response-rejects-malformed-progression-before-model-work-test
  (let [seen (atom {})
        compiled (progression-fragment-app seen)
        error
        (try
          (live/render-fragment-response
           compiled
           {:headers {progression.http/request-header-name "%not-valid"}}
           :request-panel
           "req-bad")
          nil
          (catch clojure.lang.ExceptionInfo e
            e))]
    (is (some? error))
    (is (= :gesso.live.progression.http/error
           (:error/type (ex-data error))))
    (is (= :invalid-header-encoding
           (:error/kind (ex-data error))))
    (is (= {} @seen)
        "Malformed browser progression must fail before authorization/query/render.")))

;; -----------------------------------------------------------------------------
;; HTMX facade
;; -----------------------------------------------------------------------------

(deftest htmx-facades-reexport-app-facing-helpers-test
  (is (identical? htmx/fragment-root-attrs
                  live/fragment-root-attrs))

  (is (identical? htmx/fragment-target-attrs
                  live/fragment-target-attrs))

  (is (identical? htmx/post-form-attrs
                  live/post-form-attrs)))

;; -----------------------------------------------------------------------------
;; Optimistic protocol-v3 application capability facade
;; -----------------------------------------------------------------------------

(defn- core-one-role-browser-plan
  [operation]
  (project/project
   (choreo/->choreography
    {:name :example/core-operation-binding
     :initial :run
     :states
     {:run
      (choreo/local :browser operation :done)

      :done
      (choreo/return :done)}})
   :browser))

(def core-browser-plans
  {:request/claim
   (core-one-role-browser-plan :request/claim)

   :request/cancel
   (core-one-role-browser-plan :request/cancel)})

(def core-operation-policy
  {:request/claim
   {:rollback-eligible? true
    :timeout-ms 5000
    :replace-owner? false
    :replace-execution? true}})

(def core-optimistic-binding
  {:arguments {:request-id "request-1"}
   :observed-basis {:tx-id 42
                    :system-time "2026-08-26T17:00:00Z"}
   :scope [:request "request-1"]
   :fact-versions {:request/status 9}
   :target-id "request-request-1"})

(defn- core-element-children
  [hiccup]
  (let [xs (rest hiccup)]
    (if (map? (first xs))
      (rest xs)
      xs)))

(defn- core-direct-child-by-tag
  [hiccup tag]
  (some #(when (and (vector? %)
                    (= tag (first %)))
           %)
        (core-element-children hiccup)))

(defn- core-decoded-optimistic-action
  [hiccup]
  (some-> hiccup
          (core-direct-child-by-tag :button)
          second
          :data-gesso-live-optimistic
          edn/read-string))

(defn- core-error-data
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e
      (ex-data e))))

(deftest optimistic-operation-capability-derivation-is-public-through-core-test
  (testing "core re-exports the canonical browser-plan -> operation-capability derivation"
    (is (identical? optimistic.capability/operation-capabilities
                    live/optimistic-operation-capabilities)))

  (let [derived
        (live/optimistic-operation-capabilities
         core-browser-plans
         core-operation-policy)]
    (is (= #{:request/claim :request/cancel}
           (set (keys derived))))
    (is (= :request/claim
           (get-in derived [:request/claim :operation])))
    (is (= :request/claim
           (get-in derived [:request/claim :plan-key])))
    (is (= true
           (get-in derived [:request/claim :rollback-eligible?])))
    (is (= 5000
           (get-in derived [:request/claim :timeout-ms])))
    (is (= false
           (get-in derived [:request/claim :replace-owner?])))
    (is (= true
           (get-in derived [:request/claim :replace-execution?])))
    (is (= :request/cancel
           (get-in derived [:request/cancel :operation])))
    (is (= :request/cancel
           (get-in derived [:request/cancel :plan-key])))))

(deftest with-optimistic-browser-plans-is-the-one-step-core-composition-path-test
  (let [ctx {:anti-forgery-token "anti-forgery-token"
             :app/value 7}
        derived
        (live/optimistic-operation-capabilities
         core-browser-plans
         core-operation-policy)
        explicit
        (live/with-optimistic-operation-capabilities
         ctx
         derived)
        one-step
        (live/with-optimistic-browser-plans
         ctx
         core-browser-plans
         core-operation-policy)]
    (testing "one-step composition is exactly derivation followed by installation"
      (is (= explicit one-step))
      (is (= 7 (:app/value one-step))))

    (testing "the two-arity form is the same path with no optional browser policy"
      (is (= (live/with-optimistic-operation-capabilities
              ctx
              (live/optimistic-operation-capabilities core-browser-plans))
             (live/with-optimistic-browser-plans
              ctx
              core-browser-plans))))))

(deftest core-composed-context-renders-semantic-choreo-operation-test
  (let [ctx {:anti-forgery-token "anti-forgery-token"}
        derived
        (live/optimistic-operation-capabilities
         core-browser-plans
         core-operation-policy)
        semantic
        (live/post-button
         (live/with-optimistic-browser-plans
          ctx
          core-browser-plans
          core-operation-policy)
         {:to "/request/claim"
          :target "request-request-1"
          :swap "outerHTML"
          :label "Claim"
          :choreo/op :request/claim
          :optimistic-binding core-optimistic-binding})
        explicit
        (live/post-button
         ctx
         {:to "/request/claim"
          :target "request-request-1"
          :swap "outerHTML"
          :label "Claim"
          :optimistic (get derived :request/claim)
          :optimistic-binding core-optimistic-binding})
        action
        (core-decoded-optimistic-action semantic)]
    (testing "the semantic operation path renders exactly like the explicit derived capability"
      (is (= explicit semantic)))

    (testing "the emitted inert action keeps semantic identity and derived browser plan identity aligned"
      (is (= :request/claim (:operation action)))
      (is (= :request/claim (:plan-key action)))
      (is (= (:arguments core-optimistic-binding)
             (:arguments action)))
      (is (= (:observed-basis core-optimistic-binding)
             (:observed-basis action)))
      (is (= 5000 (:timeout-ms action)))
      (doseq [authority-key [:principal
                             :authority
                             :authorities
                             :command-id
                             :execution-id
                             :settlement]]
        (is (not (contains? action authority-key)))))))

(deftest core-operation-composition-propagates-closed-lower-layer-diagnostics-test
  (testing "malformed browser-plan registries fail during derivation"
    (let [data
          (core-error-data
           #(live/with-optimistic-browser-plans
             {}
             {}))]
      (is (= :gesso.live.optimistic.capability/error
             (:error/type data)))
      (is (= :empty-browser-plans
             (:error/kind data)))))

  (testing "policy cannot name an operation absent from the canonical browser-plan set"
    (let [data
          (core-error-data
           #(live/with-optimistic-browser-plans
             {}
             core-browser-plans
             {:request/unclaim
              {:timeout-ms 5000}}))]
      (is (= :unknown-policy-operations
             (:error/kind data)))
      (is (= #{:request/unclaim}
             (:unknown-operations data)))
      (is (= #{:request/claim :request/cancel}
             (:available-operations data)))))

  (testing "policy cannot reintroduce an independently authored browser plan key"
    (let [data
          (core-error-data
           #(live/with-optimistic-browser-plans
             {}
             core-browser-plans
             {:request/claim
              {:plan-key :request/cancel}}))]
      (is (= :unknown-fields
             (:error/kind data)))
      (is (= #{:plan-key}
             (:unknown data)))))

  (testing "the lower-level precomputed-registry facade validates before installing"
    (let [data
          (core-error-data
           #(live/with-optimistic-operation-capabilities
             {}
             {:request/claim
              {:operation :request/claim
               :plan-key :request/claim}}))]
      (is (= :invalid-operation-capabilities
             (:error/kind data))))))

(deftest optimistic-capability-facades-test
  (testing "core exposes the portable application capability constructors"
    (is (identical? optimistic.capability/operation-capability
                    live/optimistic-capability))
    (is (identical? optimistic.capability/operation-capability?
                    live/optimistic-capability?))
    (is (identical? optimistic.capability/bind
                    live/bind-optimistic-capability))))

(deftest optimistic-capability-facade-remains-separate-from-trusted-server-registry-test
  (let [capability
        (live/optimistic-capability
         {:operation :request/claim
          :plan-key :request/claim
          :rollback-eligible? true
          :timeout-ms 5000})
        binding
        {:arguments {:request-id "request-1"}
         :observed-basis {:tx-id 42
                          :system-time "2026-08-26T17:00:00Z"}
         :scope [:request "request-1"]
         :target-id "request-request-1"}
        action
        (live/bind-optimistic-capability capability binding)]
    (testing "the app-facing facade preserves the capability owner's exact values"
      (is (true? (live/optimistic-capability? capability)))
      (is (= (optimistic.capability/operation-capability
              {:operation :request/claim
               :plan-key :request/claim
               :rollback-eligible? true
               :timeout-ms 5000})
             capability))
      (is (= (optimistic.capability/bind capability binding)
             action)))

    (testing "capability and bound action are not trusted server operation entries"
      (is (false? (optimistic.server/operation? capability)))
      (is (false? (live/optimistic-operation? capability)))
      (is (false? (optimistic.server/operation? action)))
      (is (false? (live/optimistic-operation? action))))

    (testing "binding emits inert semantic data rather than authority or correlation"
      (is (= :request/claim (:operation action)))
      (is (= {:request-id "request-1"} (:arguments action)))
      (is (= (:observed-basis binding) (:observed-basis action)))
      (is (= (:scope binding) (:scope action)))
      (is (= (:target-id binding) (:target-id action)))
      (is (= :request/claim (:plan-key action)))
      (is (true? (:rollback-eligible? action)))
      (is (= 5000 (:timeout-ms action)))
      (doseq [key [:principal
                   :authority
                   :authorities
                   :role
                   :roles
                   :command-id
                   :execution-id
                   :settlement
                   :resolution
                   :authoritative]]
        (is (not (contains? action key))
            (str "bound application capability must remain inert: " key))))))

;; -----------------------------------------------------------------------------
;; Optimistic protocol-v3 trusted-server facade
;; -----------------------------------------------------------------------------

(deftest optimistic-registry-facades-test
  (testing "core exposes the trusted protocol-v3 registry constructors"
    (is (identical? optimistic.server/operation
                    live/optimistic-operation))
    (is (identical? optimistic.server/operation?
                    live/optimistic-operation?))
    (is (identical? optimistic.server/server
                    live/optimistic-server))
    (is (identical? optimistic.server/server?
                    live/optimistic-server?))))

(deftest optimistic-command-boundary-facades-test
  (testing "core exposes the trusted command decode/normalization boundary"
    (is (identical? optimistic.server/decode-command
                    live/decode-optimistic-command))
    (is (identical? optimistic.server/normalize-command
                    live/normalize-optimistic-command))
    (is (identical? optimistic.server/begin-command
                    live/begin-optimistic-command))
    (is (identical? optimistic.server/command-boundary?
                    live/optimistic-command-boundary?))
    (is (identical? optimistic.server/operation-context
                    live/optimistic-operation-context))))

(deftest optimistic-settlement-send-facades-test
  (testing "core exposes the trusted settlement/send lifecycle"
    (is (identical? optimistic.server/settlement-from-result
                    live/optimistic-settlement-from-result))
    (is (identical? optimistic.server/prepare-settlement-send
                    live/prepare-optimistic-settlement-send))
    (is (identical? optimistic.server/prepared-send?
                    live/optimistic-prepared-send?))
    (is (identical? optimistic.server/complete-settlement-send
                    live/complete-optimistic-send))
    (is (identical? optimistic.server/completed-send?
                    live/optimistic-completed-send?))
    (is (identical? optimistic.server/run-command
                    live/run-optimistic-command))
    (is (identical? optimistic.server/run-wire-command
                    live/run-optimistic-wire-command))))

(deftest obsolete-protocol-v2-core-facades-are-gone-test
  (testing "removed protocol-v2 descriptor/rendering/settlement APIs stay removed"
    (doseq [sym '[->optimistic
                  optimistic?
                  canonical
                  canonical-attrs
                  request-execution-id
                  ->settlement
                  settlement?
                  settlement-for-request
                  settlement-marker
                  with-settlement
                  optimistic-response-hiccup
                  optimistic-post-button]]
      (is (nil?
           (ns-resolve 'gesso.live.core sym))
          (str "obsolete protocol-v2 facade must remain absent: " sym)))))

(deftest final-live-core-facade-authority-boundaries-test
  (let [publics (ns-publics 'gesso.live.core)]
    (testing "current authoritative read/write and protocol-v3 boundaries remain public"
      (doseq [sym '[q
                    progression
                    with-progression
                    bind-request-progression
                    transact-and-notify!
                    live-set!
                    live-swap!
                    optimistic-capability
                    bind-optimistic-capability
                    optimistic-server
                    run-optimistic-command
                    run-optimistic-wire-command]]
        (is (contains? publics sym)
            (str "current Live facade boundary must remain public: " sym))))

    (testing "retired mutation and transaction-derived metadata escape hatches stay out of the facade"
      (doseq [sym '[execute-tx!
                    submit-tx!
                    put-docs-op
                    delete-docs-op
                    attach-consistency
                    attach-progression]]
        (is (not (contains? publics sym))
            (str "internal/low-level Live authority helper must not be public: " sym))))

    (testing "retired fragment consistency-token vocabulary is not reintroduced through core"
      (doseq [sym '[consistency-fragment-dimension
                    with-consistency-dimension
                    with-consistency-dimension-from]]
        (is (not (contains? publics sym))
            (str "fragment freshness belongs to canonical progression, not retired consistency dimensions: " sym))))))

;; -----------------------------------------------------------------------------
;; transact-and-notify!
;; -----------------------------------------------------------------------------

(deftest transact-and-notify-invalid-primary-changes-cannot-commit-test
  ;; Invalid app-supplied changes are knowable before executing XTDB. A bad
  ;; change in the middle of a batch must not commit the write and then cause
  ;; an avoidable post-commit notification or preparation failure.
  (doseq [invalid-change [{:id "req-1"}
                          {:topic "request" :id "req-1"}
                          {:topic :request :id nil}
                          17
                          nil]
          emit-mode [:sync :async false]]
    (let [writes (atom 0)
          emissions (atom 0)]
      (with-redefs [live/emit-expanded! (fn [& _] (swap! emissions inc)
                                        {:status :emitted})
                    live/submit-expanded! (fn [& _] (swap! emissions inc)
                                          {:status :submitted})]
        (with-xtdb-stub
          'execute-tx-from!
          (fn [& _]
            (swap! writes inc)
            {:tx-result {:tx-id 42}
             :consistency sample-consistency
             :progression sample-commit-progression})
          (fn []
            (let [error (try
                          (live/transact-and-notify!
                           {:options {}}
                           {:xtdb/connectable :write-node}
                           {:tx-ops sample-tx
                            :changes [request-change invalid-change other-request-change]
                            :emit emit-mode})
                          nil
                          (catch Throwable e e))]
              (is (instance? clojure.lang.ExceptionInfo error)
                  (str "Malformed change must fail: " (pr-str invalid-change)))
              (is (= :gesso.live/primary-change
                     (:schema-key (ex-data error)))
                  "Reject the offending primary change at the admission boundary")
              (is (= 0 @writes)
                  "Malformed change must be rejected before authoritative commit")
              (is (= 0 @emissions)
                  "No notifications may precede full batch admission"))))))))

(deftest transact-and-notify-rejects-invalid-context-progression-before-commit-test
  ;; A malformed request progression is available before XTDB execution.
  ;; Reject it before the authoritative write rather than discovering it in
  ;; post-commit context composition, even for notification-free mutations.
  (doseq [context-data [{:gesso.live/progression 42}
                        {:progression "bad-progression"}
                        {:gesso.live/progression
                         {:gesso.live.progression/type
                          :gesso.live.progression/requirement
                          :gesso.live.progression/version 1
                          :bases []}}]
          emit-mode [false :sync :async]]
    (let [ctx (merge {:xtdb/connectable :node} context-data)
          writes (atom 0)
          notification-count (atom 0)]
      (with-redefs [live/emit-expanded!
                    (fn [& _] (swap! notification-count inc) {:status :emitted})
                    live/submit-expanded!
                    (fn [& _] (swap! notification-count inc) {:status :submitted})]
        (with-xtdb-stub
          'execute-tx-from!
          (fn [& _]
            (swap! writes inc)
            {:tx-result {:tx-id 42}
             :consistency sample-consistency
             :progression sample-commit-progression})
          (fn []
            (let [error (try
                          (live/transact-and-notify!
                           {:options {}}
                           ctx
                           {:tx-ops sample-tx
                            :change request-change
                            :emit emit-mode})
                          nil
                          (catch Throwable error error))]
              (is (= :gesso.live.progression/error
                     (:error/type (ex-data error)))
                  (str "Malformed request progression must fail at admission: "
                       (pr-str context-data)))
              (is (zero? @writes)
                  "An invalid request context must never reach XTDB execution")
              (is (zero? @notification-count)
                  "Rejected requests cannot publish invalidations"))))))))

(deftest transact-and-notify-rejects-lossy-async-dispatch-before-commit-test
  ;; Live invalidations carry the authoritative XTDB progression requirement.
  ;; :drop can silently discard them; :coalesce replaces a pending job, losing
  ;; earlier requirements (including cross-database progression). A successful
  ;; transaction must not be admitted into either lossy queue policy.
  (doseq [policy [:drop :coalesce]
          changes [[request-change]
                   [request-change other-request-change]]]
    (let [writes (atom 0)
          submissions (atom 0)
          system (live/create {:dispatch-options {:threads 1
                                                  :queue-size 2
                                                  :on-overflow policy}})]
      (try
        (with-redefs [live/submit-expanded!
                      (fn [& _]
                        (swap! submissions inc)
                        {:status :submitted})]
          (with-xtdb-stub
            'execute-tx-from!
            (fn [& _]
              (swap! writes inc)
              {:tx-result {:tx-id 42}
               :consistency sample-consistency
               :progression sample-commit-progression})
            (fn []
              (let [error (try
                            (live/transact-and-notify!
                             system
                             {:xtdb/connectable :write-node}
                             {:tx-ops sample-tx
                              :changes changes
                              :emit :async
                              :entry {:coalesce-key [:request "req-1"]}})
                            nil
                            (catch clojure.lang.ExceptionInfo e e))]
                (is (instance? clojure.lang.ExceptionInfo error))
                (is (= :gesso.live.core/lossy-async-dispatch-policy
                       (:error/type (ex-data error))))
                (is (= policy (:on-overflow (ex-data error))))
                (is (zero? @writes)
                    "A lossy queue must fail before a physical mutation")
                (is (zero? @submissions)
                    "No published work may escape the rejected batch")))))
        (finally (live/close! system))))))

(deftest transact-and-notify-allows-nonlossy-and-explicit-no-delivery-modes-test
  ;; Deliberately notification-free writes and synchronous delivery must not
  ;; depend on the configured asynchronous queue. Async :throw/:block are
  ;; nonlossy policies and retain existing semantics.
  (doseq [[policy emit-mode] [[:drop false] [:coalesce :sync]
                             [:throw :async] [:block :async]]]
    (let [writes (atom 0)
          delivered (atom 0)
          system (live/create {:dispatch-options {:threads 1
                                                  :queue-size 2
                                                  :on-overflow policy}})]
      (try
        (with-redefs [live/submit-expanded! (fn [& _]
                                               (swap! delivered inc)
                                               {:status :submitted})
                      live/emit-expanded! (fn [& _]
                                             (swap! delivered inc)
                                             {:status :emitted})]
          (with-xtdb-stub
            'execute-tx-from!
            (fn [& _]
              (swap! writes inc)
              {:tx-result {:tx-id 42}
               :consistency sample-consistency
               :progression sample-commit-progression})
            (fn []
              (let [result (live/transact-and-notify!
                            system {:xtdb/connectable :write-node}
                            {:tx-ops sample-tx :change request-change
                             :emit emit-mode})]
                (is (= 1 @writes))
                (is (= emit-mode (:emit result)))
                (is (= (if (= false emit-mode) 0 1) @delivered))
                (is (= sample-commit-progression
                       (get-in result [:changes 0 :progression])))))))
        (finally (live/close! system))))))

(deftest transact-and-notify-requires-tx-ops-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"requires :tx-ops"
       (live/transact-and-notify!
        {:options {}}
        {:xtdb/connectable :node}
        {:change request-change}))))

(deftest transact-and-notify-rejects-unsupported-emit-mode-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Unsupported gesso.live transact-and-notify! emit mode"
       (live/transact-and-notify!
        {:options {}}
        {:xtdb/connectable :node}
        {:tx-ops sample-tx
         :change request-change
         :emit :banana}))))

(deftest transact-and-notify-binds-transaction-progression-to-context-and-changes-test
  (let [system {:options {}}
        ctx {:xtdb/connectable :node
             :gesso.live/progression sample-request-progression}
        expected-context-progression
        (progression/compose
         sample-request-progression
         sample-commit-progression)]
    (with-xtdb-stub
      'execute-tx-from!
      (fn [ctx' tx-ops opts]
        (is (= ctx ctx'))
        (is (= sample-tx tx-ops))
        (is (nil? opts))
        {:tx-result {:tx-id 42}
         :consistency sample-consistency
         :progression sample-commit-progression})
      (fn []
        (let [result
              (live/transact-and-notify!
               system
               ctx
               {:tx-ops sample-tx
                :change request-change
                :emit false})
              change' (first (:changes result))]
          (is (= sample-commit-progression
                 (:progression result)))
          (is (= expected-context-progression
                 (live/progression (:ctx result))))
          (is (= sample-commit-progression
                 (:progression change')))
          (is (= sample-consistency
                 (:gesso.live/consistency change')))
          (is (= false (:emit result)))
          (is (= [] (:emit-results result))))))))

(deftest transact-and-notify-change-progression-is-transaction-owned-test
  (let [system {:options {}}
        ctx {:xtdb/connectable :node}]
    (testing "caller may repeat exactly the transaction-established requirement"
      (with-xtdb-stub
        'execute-tx-from!
        (fn [_ctx _tx-ops _opts]
          {:tx-result {:tx-id 42}
           :consistency sample-consistency
           :progression sample-commit-progression})
        (fn []
          (let [change (assoc request-change
                              :progression sample-commit-progression)
                result
                (live/transact-and-notify!
                 system
                 ctx
                 {:tx-ops sample-tx
                  :change change
                  :emit false})]
            (is (= sample-commit-progression
                   (:progression (first (:changes result)))))))))

    (testing "caller cannot replace transaction-established progression"
      (with-xtdb-stub
        'execute-tx-from!
        (fn [_ctx _tx-ops _opts]
          {:tx-result {:tx-id 42}
           :consistency sample-consistency
           :progression sample-commit-progression})
        (fn []
          (let [error
                (try
                  (live/transact-and-notify!
                   system
                   ctx
                   {:tx-ops sample-tx
                    :change (assoc request-change
                                   :progression sample-other-progression)
                    :emit false})
                  nil
                  (catch clojure.lang.ExceptionInfo e
                    e))]
            (is (some? error))
            (is (= sample-other-progression
                   (:existing-progression (ex-data error))))
            (is (= sample-commit-progression
                   (:authoritative-progression (ex-data error))))))))))

(deftest transact-and-notify-rejects-fabricated-change-progression-test
  (let [system {:options {}}
        ctx {:xtdb/connectable :node}]
    (with-xtdb-stub
      'execute-tx-from!
      (fn [_ctx _tx-ops _opts]
        {:tx-result {:tx-id 42}
         :consistency sample-consistency})
      (fn []
        (let [error
              (try
                (live/transact-and-notify!
                 system
                 ctx
                 {:tx-ops sample-tx
                  :change (assoc request-change
                                 :progression sample-other-progression)
                  :emit false})
                nil
                (catch clojure.lang.ExceptionInfo e
                  e))]
          (is (some? error))
          (is (= sample-other-progression
                 (:progression (ex-data error)))))))))

(deftest transact-and-notify-context-progression-never-substitutes-for-commit-evidence-test
  (let [system {:options {}}
        ctx {:xtdb/connectable :node
             :gesso.live/progression sample-request-progression}]
    (with-xtdb-stub
      'execute-tx-from!
      (fn [_ctx _tx-ops _opts]
        {:tx-result {:tx-id 42}
         :consistency sample-consistency})
      (fn []
        (let [result
              (live/transact-and-notify!
               system
               ctx
               {:tx-ops sample-tx
                :change request-change
                :emit false})]
          (is (= sample-request-progression
                 (live/progression (:ctx result))))
          (is (not (contains? (first (:changes result))
                              :progression)))
          (is (not (contains? result :progression))))))))

(deftest transact-and-notify-emit-false-executes-tx-and-returns-metadata-test
  (let [seen-tx (atom nil)
        system {:options {}}
        ctx {:xtdb/connectable :node}]
    (with-redefs [live/submit-expanded!
                  (fn [& _]
                    (throw
                     (ex-info "submit-expanded! should not be called"
                              {})))

                  live/emit-expanded!
                  (fn [& _]
                    (throw
                     (ex-info "emit-expanded! should not be called"
                              {})))]
      (with-xtdb-stub
        'execute-tx-from!
        (fn [ctx' tx-ops opts]
          (reset! seen-tx [ctx' tx-ops opts])
          {:tx-result {:tx-id 42
                       :system-time :system-time-42}
           :consistency sample-consistency})
        (fn []
          (is (= {:tx-result {:tx-id 42
                              :system-time :system-time-42}
                  :consistency sample-consistency
                  :ctx (assoc ctx :gesso.live/consistency sample-consistency)
                  :changes [(assoc request-change
                                    :gesso.live/consistency
                                    sample-consistency)]
                  :emit false
                  :emit-results []}
                 (live/transact-and-notify!
                  system
                  ctx
                  {:tx-ops sample-tx
                   :change request-change
                   :tx-options {:database :xtdb}
                   :emit false})))

          (is (= [ctx sample-tx {:database :xtdb}]
                 @seen-tx)))))))

(deftest transact-and-notify-sync-emits-attached-changes-on-caller-thread-test
  (let [seen-tx (atom nil)
        seen-emits (atom [])
        system {:options {}}
        ctx {:xtdb/connectable :node}]
    (with-redefs [live/emit-expanded!
                  (fn [system' ctx' change']
                    (swap! seen-emits conj [system' ctx' change'])
                    {:status :emitted
                     :change-id (:id change')})]
      (with-xtdb-stub
        'execute-tx-from!
        (fn [ctx' tx-ops opts]
          (reset! seen-tx [ctx' tx-ops opts])
          {:tx-result {:tx-id 42}
           :consistency sample-consistency})
        (fn []
          (let [result (live/transact-and-notify!
                        system
                        ctx
                        {:tx-ops sample-tx
                         :changes [request-change other-request-change]
                         :emit :sync})
                ctx' (:ctx result)
                changes' (:changes result)]
            (is (= [ctx sample-tx nil]
                   @seen-tx))

            (is (= sample-consistency
                   (:gesso.live/consistency ctx')))

            (is (= [(assoc request-change
                            :gesso.live/consistency
                            sample-consistency)
                    (assoc other-request-change
                            :gesso.live/consistency
                            sample-consistency)]
                   changes'))

            (is (= [[system ctx' (first changes')]
                    [system ctx' (second changes')]]
                   @seen-emits))

            (is (= [{:status :emitted
                     :change-id "req-1"}
                    {:status :emitted
                     :change-id "req-2"}]
                   (:emit-results result)))

            (is (= :sync (:emit result)))))))))

(deftest transact-and-notify-async-submits-attached-changes-test
  (let [seen-tx (atom nil)
        seen-submits (atom [])
        system {:options {}}
        ctx {:xtdb/connectable :node}
        entry {:coalesce-key [:request "req-1"]}]
    (with-redefs [live/submit-expanded!
                  (fn [system' ctx' change' entry']
                    (swap! seen-submits conj [system' ctx' change' entry'])
                    {:status :submitted
                     :job-id (:id change')})]
      (with-xtdb-stub
        'execute-tx-from!
        (fn [ctx' tx-ops opts]
          (reset! seen-tx [ctx' tx-ops opts])
          {:tx-result {:tx-id 42}
           :consistency sample-consistency})
        (fn []
          (let [result (live/transact-and-notify!
                        system
                        ctx
                        {:tx-ops sample-tx
                         :change request-change
                         :entry entry})
                ctx' (:ctx result)
                change' (first (:changes result))]
            (is (= [ctx sample-tx nil]
                   @seen-tx))

            (is (= (assoc ctx :gesso.live/consistency sample-consistency)
                   ctx'))

            (is (= (assoc request-change
                          :gesso.live/consistency
                          sample-consistency)
                   change'))

            (is (= [[system ctx' change' entry]]
                   @seen-submits))

            (is (= [{:status :submitted
                     :job-id "req-1"}]
                   (:emit-results result)))

            (is (= :async (:emit result)))))))))

(deftest transact-and-notify-rejects-invalid-async-dispatch-config-before-write-test
  ;; :entry and :entry-fn describe async dispatch, and their shape is fully
  ;; knowable before an XTDB commit. Previously malformed values failed only
  ;; during post-commit job construction (or were silently ignored when false).
  (doseq [invalid-options [{:entry 42}
                           {:entry "not-a-map"}
                           {:entry false}
                           {:entry-fn 42}
                           {:entry-fn "not-a-function"}
                           {:entry-fn false}]
          changes [[request-change]
                   [request-change other-request-change]]]
    (let [writes (atom 0)
          submitted (atom 0)]
      (with-redefs [live/submit-expanded! (fn [& _]
                                           (swap! submitted inc)
                                           {:status :submitted})]
        (with-xtdb-stub
          'execute-tx-from!
          (fn [& _]
            (swap! writes inc)
            {:tx-result {:tx-id 42}
             :consistency sample-consistency})
          (fn []
            (let [error (try
                          (live/transact-and-notify!
                           {:options {}}
                           {:xtdb/connectable :write-node}
                           (merge {:tx-ops sample-tx
                                   :changes changes
                                   :emit :async}
                                  invalid-options))
                          nil
                          (catch Throwable e e))]
              (is (instance? clojure.lang.ExceptionInfo error)
                  (str "Invalid async options must be rejected: "
                       (pr-str invalid-options)))
              (is (= :gesso.live.core/invalid-async-dispatch-options
                     (:error/type (ex-data error)))
                  "Reject at the explicit async admission boundary")
              (is (zero? @writes)
                  "Invalid async job metadata cannot commit a mutation")
              (is (zero? @submitted)
                  "No partial notification should be scheduled"))))))))

(deftest malformed-async-entry-with-real-dispatcher-does-not-commit-test
  ;; This uses the real submit-expanded!/dispatch entry construction path:
  ;; on v1992, assoc on the scalar :entry threw after execute-tx! succeeded.
  ;; Rejecting the bad job metadata before XTDB is the real protection.
  (let [writes (atom 0)
        system (live/create {:dispatch-options {:threads 1 :queue-size 4}})]
    (try
      (with-xtdb-stub
        'execute-tx-from!
        (fn [& _]
          (swap! writes inc)
          {:tx-result {:tx-id 42}
           :consistency sample-consistency})
        (fn []
          (let [error (try
                        (live/transact-and-notify!
                         system {:xtdb/connectable :write-node}
                         {:tx-ops sample-tx
                          :change request-change
                          :entry 42
                          :emit :async})
                        nil
                        (catch Throwable e e))]
            (is (= :gesso.live.core/invalid-async-dispatch-options
                   (:error/type (ex-data error))))
            (is (= :entry (:option (ex-data error))))
            (is (zero? @writes)
                "A real invalid dispatcher entry cannot commit first")
            (is (zero? (:queue-size (dispatch/stats (:dispatcher system))))))))
      (finally
        (live/close! system)))))

(deftest transact-and-notify-ignores-dispatch-metadata-when-emit-disabled-test
  ;; Dispatch metadata is irrelevant when notification is disabled, so avoid
  ;; a new restriction on deliberately transaction-only writes.
  (let [writes (atom 0)]
    (with-xtdb-stub
      'execute-tx-from!
      (fn [& _]
        (swap! writes inc)
        {:tx-result {:tx-id 42}
         :consistency sample-consistency})
      (fn []
        (let [result (live/transact-and-notify!
                      {:options {}}
                      {:xtdb/connectable :write-node}
                      {:tx-ops sample-tx
                       :change request-change
                       :emit false
                       :entry 42
                       :entry-fn false})]
          (is (= 1 @writes))
          (is (= false (:emit result)))
          (is (= [] (:emit-results result))))))))

(deftest transact-and-notify-async-supports-entry-fn-test
  (let [seen-submits (atom [])
        system {:options {}}
        ctx {:xtdb/connectable :node}]
    (with-redefs [live/submit-expanded!
                  (fn [system' ctx' change' entry']
                    (swap! seen-submits conj [system' ctx' change' entry'])
                    {:status :submitted
                     :entry entry'})]
      (with-xtdb-stub
        'execute-tx-from!
        (fn [_ctx _tx-ops _opts]
          {:tx-result {:tx-id 42}
           :consistency sample-consistency})
        (fn []
          (let [result (live/transact-and-notify!
                        system
                        ctx
                        {:tx-ops sample-tx
                         :change request-change
                         :entry-fn (fn [change]
                                     {:coalesce-key [:request (:id change)]
                                      :seen-consistency (:gesso.live/consistency change)})})
                entry' (get-in result [:emit-results 0 :entry])]
            (is (= {:coalesce-key [:request "req-1"]
                    :seen-consistency sample-consistency}
                   entry'))

            (is (= [[system
                     (:ctx result)
                     (first (:changes result))
                     {:coalesce-key [:request "req-1"]
                      :seen-consistency sample-consistency}]]
                   @seen-submits))))))))

(deftest transact-and-notify-defaults-to-async-emit-test
  (let [seen-submits (atom [])
        system {:options {}}
        ctx {:xtdb/connectable :node}]
    (with-redefs [live/submit-expanded!
                  (fn [& args]
                    (swap! seen-submits conj args)
                    {:status :submitted})]
      (with-xtdb-stub
        'execute-tx-from!
        (fn [_ctx _tx-ops _opts]
          {:tx-result {:tx-id 42}
           :consistency sample-consistency})
        (fn []
          (let [result (live/transact-and-notify!
                        system
                        ctx
                        {:tx-ops sample-tx
                         :change request-change})]
            (is (= :async (:emit result)))
            (is (= 1 (count @seen-submits)))))))))

(deftest transact-and-notify-with-no-changes-only-executes-transaction-test
  (let [system {:options {}}
        ctx {:xtdb/connectable :node}]
    (with-redefs [live/submit-expanded!
                  (fn [& _]
                    (throw
                     (ex-info "submit-expanded! should not be called"
                              {})))

                  live/emit-expanded!
                  (fn [& _]
                    (throw
                     (ex-info "emit-expanded! should not be called"
                              {})))]
      (with-xtdb-stub
        'execute-tx-from!
        (fn [_ctx _tx-ops _opts]
          {:tx-result {:tx-id 42}
           :consistency sample-consistency})
        (fn []
          (is (= {:tx-result {:tx-id 42}
                  :consistency sample-consistency
                  :ctx (assoc ctx :gesso.live/consistency sample-consistency)
                  :changes []
                  :emit :async
                  :emit-results []}
                 (live/transact-and-notify!
                  system
                  ctx
                  {:tx-ops sample-tx}))))))))

(deftest transact-and-notify-precommit-failure-preserves-original-exception-test
  (let [cause (ex-info "XTDB failed before commit."
                       {:phase :execute-tx})
        submitted? (atom false)
        system {:options {}}
        ctx {:xtdb/connectable :node}]
    (with-redefs [live/submit-expanded!
                  (fn [& _]
                    (reset! submitted? true)
                    {:status :submitted})]
      (with-xtdb-stub
        'execute-tx-from!
        (fn [_ctx _tx-ops _opts]
          (throw cause))
        (fn []
          (let [error
                (try
                  (live/transact-and-notify!
                   system
                   ctx
                   {:tx-ops sample-tx
                    :change request-change})
                  nil
                  (catch Throwable t
                    t))]
            (is (identical? cause error)
                "pre-commit XTDB failure must escape unchanged")
            (is (false? @submitted?)
                "no post-commit delivery may begin when the transaction failed")
            (is (false? (live/post-commit-delivery-failure? error))
                "pre-commit failures must never be classified as committed")))))))

(deftest transact-and-notify-sync-delivery-failure-is-classified-after-commit-test
  (let [cause (ex-info "second synchronous invalidation failed"
                       {:delivery :sync})
        seen-emits (atom [])
        system {:options {}}
        ctx {:xtdb/connectable :node}
        expected-ctx (-> ctx
                         (assoc :gesso.live/consistency sample-consistency)
                         (assoc :gesso.live/progression sample-commit-progression))
        expected-changes
        [(assoc request-change
                :gesso.live/consistency sample-consistency
                :progression sample-commit-progression)
         (assoc other-request-change
                :gesso.live/consistency sample-consistency
                :progression sample-commit-progression)]]
    (with-redefs [live/emit-expanded!
                  (fn [system' ctx' change']
                    (swap! seen-emits conj [system' ctx' change'])
                    (if (= "req-2" (:id change'))
                      (throw cause)
                      {:status :emitted
                       :change-id (:id change')}))]
      (with-xtdb-stub
        'execute-tx-from!
        (fn [_ctx _tx-ops _opts]
          {:tx-result {:tx-id 42}
           :consistency sample-consistency
           :progression sample-commit-progression})
        (fn []
          (let [error
                (try
                  (live/transact-and-notify!
                   system
                   ctx
                   {:tx-ops sample-tx
                    :changes [request-change other-request-change]
                    :emit :sync})
                  nil
                  (catch Throwable t
                    t))
                data (ex-data error)]
            (is (live/post-commit-delivery-failure? error))
            (is (identical? cause (.getCause ^Throwable error)))
            (is (= live/post-commit-delivery-failure-type
                   (:error/type data)))
            (is (= :post-commit-delivery
                   (:failure/stage data)))
            (is (= :committed
                   (:commit/status data)))
            (is (= {:tx-id 42}
                   (:tx-result data)))
            (is (= sample-consistency
                   (:consistency data)))
            (is (= sample-commit-progression
                   (:progression data)))
            (is (= expected-ctx
                   (:ctx data)))
            (is (= expected-changes
                   (:changes data)))
            (is (= :sync
                   (:emit data)))
            (is (= 1
                   (:delivery/index data)))
            (is (= (second expected-changes)
                   (:delivery/change data)))
            (is (= [{:status :emitted
                     :change-id "req-1"}]
                   (:delivery/completed-results data))
                "completed delivery results must survive a later failure")
            (is (= [[system expected-ctx (first expected-changes)]
                    [system expected-ctx (second expected-changes)]]
                   @seen-emits)
                "delivery stops exactly at the failing change")))))))

(deftest transact-and-notify-async-submission-failure-is-classified-after-commit-test
  (let [cause (ex-info "second dispatcher submission failed"
                       {:delivery :async})
        seen-submits (atom [])
        system {:options {}}
        ctx {:xtdb/connectable :node}
        entry {:priority :normal}]
    (with-redefs [live/submit-expanded!
                  (fn [system' ctx' change' entry']
                    (swap! seen-submits conj [system' ctx' change' entry'])
                    (if (= "req-2" (:id change'))
                      (throw cause)
                      {:status :submitted
                       :job-id (:id change')}))]
      (with-xtdb-stub
        'execute-tx-from!
        (fn [_ctx _tx-ops _opts]
          {:tx-result {:tx-id 42}
           :consistency sample-consistency})
        (fn []
          (let [error
                (try
                  (live/transact-and-notify!
                   system
                   ctx
                   {:tx-ops sample-tx
                    :changes [request-change other-request-change]
                    :entry entry})
                  nil
                  (catch Throwable t
                    t))
                data (ex-data error)]
            (is (live/post-commit-delivery-failure? error))
            (is (identical? cause (.getCause ^Throwable error)))
            (is (= :committed (:commit/status data)))
            (is (= :post-commit-delivery (:failure/stage data)))
            (is (= :async (:emit data)))
            (is (= 1 (:delivery/index data)))
            (is (= "req-2" (get-in data [:delivery/change :id])))
            (is (= [{:status :submitted
                     :job-id "req-1"}]
                   (:delivery/completed-results data)))
            (is (= 2 (count @seen-submits)))
            (is (= [entry entry]
                   (mapv #(nth % 3) @seen-submits)))))))))

(deftest transact-and-notify-post-commit-debug-failure-is-observational-test
  (let [debug-calls (atom [])
        system {:options
                {:debug-fn
                 (fn [event]
                   (swap! debug-calls conj event)
                   (throw
                    (ex-info "debug sink failed"
                             {:debug true})))}}
        ctx {:xtdb/connectable :node}]
    (with-redefs [live/submit-expanded!
                  (fn [_system _ctx change _entry]
                    {:status :submitted
                     :change-id (:id change)})]
      (with-xtdb-stub
        'execute-tx-from!
        (fn [_ctx _tx-ops _opts]
          {:tx-result {:tx-id 42}
           :consistency sample-consistency
           :progression sample-commit-progression})
        (fn []
          (let [result
                (live/transact-and-notify!
                 system
                 ctx
                 {:tx-ops sample-tx
                  :change request-change})]
            (is (= {:tx-id 42}
                   (:tx-result result)))
            (is (= :async
                   (:emit result)))
            (is (= [{:status :submitted
                     :change-id "req-1"}]
                   (:emit-results result)))
            (is (= 1 (count @debug-calls))
                "the post-commit debug hook still runs")
            (is (= :gesso.live.core/transact-and-notify
                   (:event (first @debug-calls)))
                "debug failure is swallowed only after the event is offered")))))))

;; -----------------------------------------------------------------------------
;; Atomic synced swap contract
;; -----------------------------------------------------------------------------

(def atomic-swap-counter
  (live/->synced
   {:table :atomic_swap_counters
    :id "counter-1"
    :col :counter/value
    :topic :atomic-swap-counter
    :default 0}))

(deftest live-swap-rejects-invalid-context-progression-before-guarded-write-test
  ;; A successful guarded write must not be followed by a preventable request
  ;; progression error. This is also necessary with :emit false.
  (let [writes (atom 0)
        reads (atom 0)
        transforms (atom 0)
        ctx {:xtdb/connectable :write-node
             :gesso.live/progression :invalid-progression}]
    (with-redefs [synced/live-read (fn [& _] (swap! reads inc) 1)
                  synced/guarded-tx-ops (fn [& _] [[:guard]])]
      (with-xtdb-stub
        'execute-tx-from!
        (fn [& _]
          (swap! writes inc)
          {:tx-result {:tx-id 42}
           :consistency sample-consistency
           :progression sample-commit-progression})
        (fn []
          (let [error (try
                        (live/live-swap!
                         ctx atomic-swap-counter
                         (fn [value] (swap! transforms inc) (inc value))
                         {:system {:options {}} :emit false})
                        nil
                        (catch Throwable error error))]
            (is (= :gesso.live.progression/error
                   (:error/type (ex-data error))))
            (is (zero? @writes)
                "Invalid context progression must not commit guarded mutation")
            (is (= 1 @reads))
            (is (= 1 @transforms))))))))

(deftest live-swap-retries-only-after-xtdb-assert-conflict-test
  (let [read-count (atom 0)
        f-inputs (atom [])
        guarded-calls (atom [])
        tx-calls (atom [])
        assert-error (ex-info "guard lost"
                              {:xtdb.error/code :xtdb/assert-failed})
        wrapped-assert-error (ex-info "transaction failed"
                                      {:phase :execute}
                                      assert-error)
        system {:options {}}
        ctx {:xtdb/connectable :write-node}]
    (with-redefs [synced/live-read
                  (fn [read-ctx synced-value read-options]
                    (is (= {:xtdb/read-connectable :write-node}
                           read-ctx))
                    (is (identical? atomic-swap-counter synced-value))
                    (is (= {} read-options))
                    (case (swap! read-count inc)
                      1 0
                      2 1
                      (throw
                       (ex-info "unexpected extra live-swap read"
                                {:read-count @read-count}))))

                  synced/guarded-tx-ops
                  (fn [synced-value expected new-value]
                    (swap! guarded-calls conj
                           [synced-value expected new-value])
                    [[:guard expected new-value]])

                  live/transact-and-notify!
                  (fn [system' ctx' options]
                    (swap! tx-calls conj [system' ctx' options])
                    (if (= 1 (count @tx-calls))
                      (throw wrapped-assert-error)
                      {:tx-result {:tx-id 2}
                       :consistency {:tx-id 2}}))]
      (let [result
            (live/live-swap!
             ctx
             atomic-swap-counter
             (fn [value]
               (swap! f-inputs conj value)
               (inc value))
             {:system system})]
        (is (= 2 (:value result)))
        (is (= [0 1] @f-inputs)
            "f is reapplied to the freshly reread value after contention")
        (is (= 2 @read-count))
        (is (= [[atomic-swap-counter 0 1]
                [atomic-swap-counter 1 2]]
               @guarded-calls))
        (is (= 2 (count @tx-calls)))
        (is (= [[[:guard 0 1]]
                [[:guard 1 2]]]
               (mapv #(get-in % [2 :tx-ops]) @tx-calls)))
        (is (= [{:topic :atomic-swap-counter
                 :id "counter-1"
                 :change/kind :updated
                 :old-value 0
                 :new-value 1}
                {:topic :atomic-swap-counter
                 :id "counter-1"
                 :change/kind :updated
                 :old-value 1
                 :new-value 2}]
               (mapv #(get-in % [2 :change]) @tx-calls))
            "each attempt describes the value pair it actually tried to commit")))))

(deftest live-swap-does-not-retry-ordinary-transaction-failure-test
  (let [cause (ex-info "database unavailable"
                       {:xtdb.error/code :xtdb/transient-failure})
        f-calls (atom 0)
        tx-calls (atom 0)
        system {:options {}}
        ctx {:xtdb/connectable :write-node}]
    (with-redefs [synced/live-read
                  (fn [_ctx _synced _read-options]
                    7)

                  synced/guarded-tx-ops
                  (fn [_synced expected new-value]
                    [[:guard expected new-value]])

                  live/transact-and-notify!
                  (fn [& _]
                    (swap! tx-calls inc)
                    (throw cause))]
      (let [error
            (try
              (live/live-swap!
               ctx
               atomic-swap-counter
               (fn [value]
                 (swap! f-calls inc)
                 (inc value))
               {:system system})
              nil
              (catch Throwable t
                t))]
        (is (identical? cause error))
        (is (= 1 @f-calls)
            "non-ASSERT transaction failure must not cause f to run again")
        (is (= 1 @tx-calls)
            "non-ASSERT transaction failure must escape immediately")))))

(deftest live-swap-invalid-explicit-change-cannot-commit-test
  ;; The same admission rule applies to the guarded mutation facade. A
  ;; malformed explicit :change cannot be returned as successful metadata,
  ;; even with :emit false, after a real guarded transaction was submitted.
  (let [reads (atom 0)
        writes (atom 0)
        transforms (atom 0)]
    (with-redefs [synced/live-read (fn [& _] (swap! reads inc) 1)
                  synced/guarded-tx-ops (fn [& _] [[:guard]])]
      (with-xtdb-stub
        'execute-tx-from!
        (fn [& _]
          (swap! writes inc)
          {:tx-result {:tx-id 42}
           :consistency sample-consistency
           :progression sample-commit-progression})
        (fn []
          (let [error (try
                        (live/live-swap! {:xtdb/connectable :write-node}
                                         atomic-swap-counter
                                         (fn [n] (swap! transforms inc) (inc n))
                                         {:system {:options {}}
                                          :change {:topic "not-a-keyword" :id "req-1"}
                                          :emit false})
                        nil
                        (catch Throwable e e))]
            (is (= :gesso.live/primary-change
                   (:schema-key (ex-data error))))
            (is (= 0 @writes) "A rejected swap must not commit")
            (is (= 1 @reads))
            (is (= 1 @transforms))))))))

(deftest live-swap-never-retries-classified-post-commit-delivery-failure-test
  (let [delivery-error
        (ex-info
         "invalidation delivery failed after commit"
         {:error/type live/post-commit-delivery-failure-type
          :failure/stage :post-commit-delivery
          :commit/status :committed})
        f-calls (atom 0)
        tx-calls (atom 0)
        system {:options {}}
        ctx {:xtdb/connectable :write-node}]
    (with-redefs [synced/live-read
                  (fn [_ctx _synced _read-options]
                    10)

                  synced/guarded-tx-ops
                  (fn [_synced expected new-value]
                    [[:guard expected new-value]])

                  live/transact-and-notify!
                  (fn [& _]
                    (swap! tx-calls inc)
                    (throw delivery-error))]
      (let [error
            (try
              (live/live-swap!
               ctx
               atomic-swap-counter
               (fn [value]
                 (swap! f-calls inc)
                 (inc value))
               {:system system})
              nil
              (catch Throwable t
                t))]
        (is (identical? delivery-error error))
        (is (live/post-commit-delivery-failure? error))
        (is (= 1 @f-calls)
            "a committed mutation must never be reapplied after delivery failure")
        (is (= 1 @tx-calls)
            "post-commit failure is terminal for the mutation attempt")))))

(deftest transact-and-notify-classifies-committed-preparation-failure-test
  ;; The authoritative transaction has already returned when Gesso notices
  ;; incompatible, caller-supplied progression on the primary change.
  ;; Reporting an unclassified error would conceal the committed mutation.
  (let [writes (atom 0)
        system {:options {}}
        ctx {:xtdb/connectable :node}]
    (with-xtdb-stub
      'execute-tx-from!
      (fn [_ctx _tx-ops _opts]
        (swap! writes inc)
        {:tx-result {:tx-id 42}
         :consistency sample-consistency
         :progression sample-commit-progression})
      (fn []
        (let [error (try
                      (live/transact-and-notify!
                       system ctx
                       {:tx-ops sample-tx
                        :change (assoc request-change
                                       :progression sample-other-progression)
                        :emit false})
                      nil
                      (catch Throwable e e))]
          (is (= ::live/post-commit-preparation-failure
                 (:error/type (ex-data error)))
              "Committed preparation failures must be explicitly typed")
          (is (= :committed (:commit/status (ex-data error))))
          (is (= :post-commit-preparation (:failure/stage (ex-data error))))
          (is (= {:tx-id 42} (:tx-result (ex-data error))))
          (is (= sample-other-progression
                 (:existing-progression (ex-data error)))
              "Preserve useful original validation diagnostics")
          (is (instance? Throwable (.getCause ^Throwable error)))
          (is (= 1 @writes)
              "Preparation failure cannot retroactively undo an XTDB write"))))))

(deftest live-swap-does-not-retry-assert-shaped-preparation-failure-test
  ;; The original AssertionError classifier must not see an XTDB-shaped
  ;; exception raised by *post-commit metadata preparation* and retry a
  ;; previously committed guarded write.
  (let [assert-error (ex-info "metadata construction failed"
                              {:xtdb.error/code :xtdb/assert-failed})
        writes (atom 0)
        reads (atom 0)
        transforms (atom 0)]
    (with-redefs [synced/live-read
                  (fn [& _]
                    (when (> (swap! reads inc) 1)
                      (throw (ex-info "unsafe post-commit retry" {})))
                    1)
                  synced/guarded-tx-ops
                  (fn [& _] [[:guard]])
                  progression/compose
                  (fn [& _] (throw assert-error))]
      (with-xtdb-stub
        'execute-tx-from!
        (fn [_ctx _tx-ops _opts]
          (swap! writes inc)
          {:tx-result {:tx-id 42}
           :consistency sample-consistency
           :progression sample-commit-progression})
        (fn []
          (let [error (try
                        (live/live-swap!
                         {:xtdb/connectable :write-node}
                         atomic-swap-counter
                         (fn [n] (swap! transforms inc) (inc n))
                         {:system {:options {}} :emit false})
                        nil
                        (catch Throwable e e))]
            (is (= ::live/post-commit-preparation-failure
                   (:error/type (ex-data error)))
                "A post-commit error must never be mistaken for an ASSERT loss")
            (is (identical? assert-error (.getCause ^Throwable error)))
            (is (= :committed (:commit/status (ex-data error))))
            (is (= 1 @writes)
                "Never resubmit a write after metadata processing fails")
            (is (= 1 @reads)
                "No post-commit retry of the authoritative read")
            (is (= 1 @transforms)
                "Never reapply a swap transform after commit")))))))

(deftest live-swap-missing-execute-time-does-not-report-success-or-retry-test
  ;; execute-tx returned an identified transaction without a usable system
  ;; time. Unlike submit-tx's accepted-only receipt, this cannot be treated
  ;; as a successful read-after-write commit: its required snapshot is absent.
  ;; The write has already returned and must not be retried.
  (let [reads (atom 0)
        writes (atom 0)
        transforms (atom 0)
        returned {:tx-id 308}]
    (with-redefs-fn
      {#'synced/live-read (fn [& _] (swap! reads inc) 1)
       #'synced/guarded-tx-ops (fn [& _] [[:guard]])
       (xtdb-var '*execute-tx*) (fn [& _] (swap! writes inc) returned)}
      (fn []
        (let [outcome (try
                        (live/live-swap!
                         {:xtdb/connectable :write-node}
                         atomic-swap-counter
                         (fn [n] (swap! transforms inc) (inc n))
                         {:system {:options {}} :emit false})
                        (catch Throwable error error))]
          (is (xtdb-live/post-execute-result-failure? outcome)
              "An identified but incomplete execute result must not appear successful")
          (is (= :committed (:commit/status (ex-data outcome)))
              "The identified returned write must not be blindly retried")
          (is (identical? returned (:tx-result (ex-data outcome))))
          (is (= 1 @writes))
          (is (= 1 @reads))
          (is (= 1 @transforms)))))))

(deftest live-swap-cannot-claim-commit-from-missing-xtdb-result-test
  ;; The XTDB API returned without a usable transaction identity. A guarded
  ;; mutation cannot report success/committed, nor repeat the write in an
  ;; attempt to resolve an outcome that remains unknown.
  (let [reads (atom 0)
        writes (atom 0)
        transforms (atom 0)]
    (with-redefs-fn
      {#'synced/live-read (fn [& _] (swap! reads inc) 1)
       #'synced/guarded-tx-ops (fn [& _] [[:guard]])
       (xtdb-var '*execute-tx*) (fn [& _] (swap! writes inc) nil)}
      (fn []
        (let [error (try
                      (live/live-swap!
                       {:xtdb/connectable :write-node}
                       atomic-swap-counter
                       (fn [n] (swap! transforms inc) (inc n))
                       {:system {:options {}} :emit false})
                      nil
                      (catch Throwable e e))]
          (is (xtdb-live/post-execute-result-failure? error))
          (is (= :unknown (:commit/status (ex-data error))))
          (is (= :post-execute-result (:failure/stage (ex-data error))))
          (is (= 1 @writes) "Unknown result must not trigger a blind retry")
          (is (= 1 @reads))
          (is (= 1 @transforms)))))))

(deftest live-swap-does-not-retry-xtdb-result-derivation-assert-test
  ;; Exercise the real XTDB execute-tx adapter through the public guarded
  ;; swap. Its underlying API returns a committed key before derivation fails
  ;; with an ASSERT-shaped error. Retrying that write duplicates the mutation.
  (let [reads (atom 0)
        commits (atom 0)
        transforms (atom 0)
        tx-result {:tx-id 42 :system-time (java.time.Instant/parse "2026-01-01T00:00:00Z")}
        cause (ex-info "XTDB-shaped metadata failure"
                       {:xtdb.error/code :xtdb/assert-failed})]
    (with-redefs-fn
      {#'synced/live-read
       (fn [& _]
         (when (> (swap! reads inc) 1)
           (throw (ex-info "committed swap retried" {:unsafe true})))
         1)
       #'synced/guarded-tx-ops
       (fn [& _] [[:guard]])
       (xtdb-var '*execute-tx*)
       (fn [& _] (swap! commits inc) tx-result)
       (xtdb-var 'tx-result-progression)
       (fn [& _] (throw cause))}
      (fn []
        (let [error (try
                      (live/live-swap!
                       {:xtdb/connectable :write-node}
                       atomic-swap-counter
                       (fn [n] (swap! transforms inc) (inc n))
                       {:system {:options {}} :emit false})
                      nil
                      (catch Throwable e e))]
          (is (if-let [predicate (ns-resolve 'gesso.live.consistency.xtdb
                                            'post-execute-result-failure?)]
                (predicate error)
                false)
              "An adapter metadata failure cannot become a retryable ASSERT")
          (is (identical? cause (.getCause ^Throwable error)))
          (is (= :committed (:commit/status (ex-data error))))
          (is (= 1 @commits) "An already-completed XTDB call cannot repeat")
          (is (= 1 @reads))
          (is (= 1 @transforms)))))))

(deftest live-swap-never-retries-assert-shaped-post-commit-error-test
  ;; An ASSERT-shaped failure in an app delivery callback does not mean the
  ;; authoritative XTDB write failed. transact-and-notify! has already
  ;; committed and classifies the subsequent notification error explicitly.
  ;; Retrying would reapply an already committed mutation.
  (let [assert-cause (ex-info "application delivery asserts" 
                              {:xtdb.error/code :xtdb/assert-failed})
        tx-count (atom 0)
        read-count (atom 0)
        apply-count (atom 0)
        ctx {:xtdb/connectable :write-node}
        system {:options {}}]
    (with-redefs [synced/live-read
                  (fn [_ctx _synced _read-options]
                    (let [count' (swap! read-count inc)]
                      (when (> count' 1)
                        (throw (ex-info "committed swap retried" {:count count'})))
                      10))
                  synced/guarded-tx-ops
                  (fn [_synced expected new-value]
                    [[:guard expected new-value]])]
      (with-xtdb-stub
        'execute-tx-from!
        (fn [_ctx _tx-ops _opts]
          (swap! tx-count inc)
          {:tx-result {:tx-id 42}
           :consistency sample-consistency
           :progression sample-commit-progression})
        (fn []
          (let [error (try
                        (live/live-swap!
                         ctx atomic-swap-counter
                         (fn [value]
                           (swap! apply-count inc)
                           (inc value))
                         {:system system
                          :emit :async
                          :entry-fn (fn [_change]
                                      (throw assert-cause))})
                        nil
                        (catch Throwable t t))]
            (is (live/post-commit-delivery-failure? error)
                "The original classified post-commit error must escape")
            (is (identical? assert-cause (some-> error .getCause))
                "The real delivery callback error must remain the cause")
            (is (= :committed (:commit/status (ex-data error))))
            (is (= 1 @tx-count)
                "One committed transaction must never be submitted again")
            (is (= 1 @read-count)
                "The mutation must not reread after notification failure")
            (is (= 1 @apply-count)
                "The swap function must not be reapplied after commit")))))))

(deftest live-swap-does-not-retry-wrapped-assert-shaped-post-commit-error-test
  ;; Outer adapters may wrap a classified delivery failure. Walking the
  ;; exception cause chain must stop when it reaches the commit boundary,
  ;; rather than misinterpreting the nested, unrelated XTDB code.
  (let [assert-cause (ex-info "delivery assert" {:xtdb.error/code :xtdb/assert-failed})
        committed-error (ex-info "already committed"
                                 {:error/type live/post-commit-delivery-failure-type
                                  :failure/stage :post-commit-delivery
                                  :commit/status :committed}
                                 assert-cause)
        outer (ex-info "transport wrapper" {:kind :wrapper} committed-error)
        reads (atom 0)
        writes (atom 0)]
    (with-redefs [synced/live-read (fn [& _] (swap! reads inc) 1)
                  synced/guarded-tx-ops (fn [& _] [[:guard]])
                  live/transact-and-notify!
                  (fn [& _]
                    (when (> (swap! writes inc) 1)
                      (throw (ex-info "retried committed transaction" {})))
                    (throw outer))]
      (is (identical? outer
                      (try
                        (live/live-swap! {:xtdb/connectable :write-node}
                                         atomic-swap-counter inc
                                         {:system {:options {}}})
                        nil
                        (catch Throwable t t))))
      (is (= 1 @writes))
      (is (= 1 @reads)))))

(deftest live-swap-reads-current-write-database-not-request-snapshot-test
  (let [reads (atom [])
        guarded-calls (atom [])
        tx-calls (atom [])
        system {:options {}}
        ctx {:xtdb/connectable :write-node
             :gesso.live/consistency {:snapshot-time :request-snapshot
                                      :snapshot-token "request-token"}
             :gesso.live/progression
             (progression/requirement :request-basis)}]
    (with-redefs [synced/live-read
                  (fn [read-ctx synced-value read-options]
                    (swap! reads conj [read-ctx synced-value read-options])
                    4)

                  synced/guarded-tx-ops
                  (fn [synced-value expected new-value]
                    (swap! guarded-calls conj
                           [synced-value expected new-value])
                    [[:guard expected new-value]])

                  live/transact-and-notify!
                  (fn [system' ctx' options]
                    (swap! tx-calls conj [system' ctx' options])
                    {:tx-result {:tx-id 5}
                     :consistency {:tx-id 5}})]
      (let [result
            (live/live-swap!
             ctx
             atomic-swap-counter
             inc
             {:system system
              :read-options {:snapshot-time :stale-snapshot
                             :snapshot-token "stale-token"
                             :database :caller-read-db
                             :key-fn :snake-case-keyword}
              :tx-options {:database :authoritative-write-db}})]
        (is (= 5 (:value result)))
        (is (= [[{:xtdb/read-connectable :write-node}
                 atomic-swap-counter
                 {:database :authoritative-write-db
                  :key-fn :snake-case-keyword}]]
               @reads)
            "historical snapshot coordinates are stripped and the write database owns the read")
        (is (= [[atomic-swap-counter 4 5]]
               @guarded-calls))
        (is (= 1 (count @tx-calls)))
        (is (= {:database :authoritative-write-db}
               (get-in @tx-calls [0 2 :tx-options])))
        (is (= [[:guard 4 5]]
               (get-in @tx-calls [0 2 :tx-ops])))))))


;; -----------------------------------------------------------------------------
;; Optional core diagnostics must not control owned resources or commit delivery
;; -----------------------------------------------------------------------------

(deftest core-assertion-debug-during-create-retains-usable-owner-test
  ;; The ownership handoff precedes :created tracing. If tracing throws an
  ;; Error, callers must still receive the system and be able to close it.
  ;; Capture raw allocations so a failing baseline cannot leak test workers.
  (let [allocated-source (atom nil)
        allocated-dispatcher (atom nil)
        source-create (var-get #'source/create)
        dispatch-create (var-get #'dispatch/create)
        observed (atom [])]
    (try
      (with-redefs [source/create (fn [options]
                                   (let [resource (source-create options)]
                                     (reset! allocated-source resource)
                                     resource))
                    dispatch/create (fn [options]
                                      (let [resource (dispatch-create options)]
                                        (reset! allocated-dispatcher resource)
                                        resource))]
        (let [result (try
                       (live/create
                        {:dispatch-options {:threads 1 :queue-size 2}
                         :debug-fn (fn [{:keys [event]}]
                                     (swap! observed conj event)
                                     (throw (AssertionError. "creation trace failed")))})
                       (catch Throwable error error))]
          (is (map? result)
              "The debug hook must not make a fully created system disappear")
          (when (map? result)
            (is (false? (live/closed? result)))
            (live/close! result))))
      (is (= :gesso.live.core/created (first @observed)))
      (is (some? @allocated-source))
      (is (some? @allocated-dispatcher))
      (is (source/closed? @allocated-source)
          "Returned systems must retain shutdown ownership of the source")
      (is (dispatch/closed? @allocated-dispatcher)
          "Returned systems must retain shutdown ownership of the dispatcher")
      (finally
        (when @allocated-dispatcher (dispatch/close! @allocated-dispatcher))
        (when @allocated-source (source/close! @allocated-source))))))

(deftest interrupted-core-creation-debug-restores-caller-interrupt-test
  ;; The core creation diagnostic is observational. Its interruption cannot
  ;; abandon a constructed system or silently erase JVM cancellation state.
  (Thread/interrupted)
  (let [seen (atom [])
        outcome (try
                  (live/create
                   {:dispatch-options {:threads 1 :queue-size 2}
                    :debug-fn (fn [{:keys [event]}]
                                (when (= :gesso.live.core/created event)
                                  (swap! seen conj event)
                                  (throw (InterruptedException.
                                          "create diagnostic interrupted"))))})
                  (catch Throwable error error))
        interrupted? (.isInterrupted (Thread/currentThread))]
    (Thread/interrupted)
    (try
      (is (map? outcome) "The diagnostic cannot abort system creation")
      (is (= [:gesso.live.core/created] @seen))
      (is interrupted? "A swallowed create diagnostic must restore interrupt")
      (when (map? outcome)
        (is (false? (live/closed? outcome)))
        (live/close! outcome)
        (is (source/closed? (:source outcome)))
        (is (dispatch/closed? (:dispatcher outcome))))
      (finally
        (Thread/interrupted)))))

(deftest interrupted-core-closing-diagnostic-completes-shutdown-test
  (let [system (live/create {:dispatch-options {:threads 1 :queue-size 2}})
        seen (atom [])
        noisy (assoc-in system [:options :debug-fn]
                        (fn [{:keys [event]}]
                          (swap! seen conj event)
                          (when (= :gesso.live.core/closing event)
                            (throw (InterruptedException.
                                    "closing diagnostic interrupted")))))]
    (try
      (Thread/interrupted)
      (let [result (try (live/close! noisy)
                        (catch Throwable error error))
            interrupted? (.isInterrupted (Thread/currentThread))]
        (Thread/interrupted)
        (is (identical? noisy result))
        (is interrupted? "Shutdown cannot erase interrupted diagnostic")
        (is (live/closed? noisy))
        (is (dispatch/closed? (:dispatcher system)))
        (is (source/closed? (:source system)))
        (is (= [:gesso.live.core/closing :gesso.live.core/closed] @seen)))
      (finally
        (Thread/interrupted)
        (dispatch/close! (:dispatcher system))
        (source/close! (:source system))))))

(deftest interrupted-core-owned-cleanup-continues-and-restores-flag-test
  ;; A best-effort owned dispatcher close can be interrupted; core must still
  ;; close the source and retain the signal through terminal shutdown.
  (let [calls (atom [])
        interrupted (InterruptedException. "dispatcher cleanup interrupted")
        system {:source ::owned-source
                :dispatcher ::owned-dispatcher
                :owned {:source true :dispatcher true}
                :options {}
                :closed? (atom false)}]
    (try
      (Thread/interrupted)
      (let [outcome (with-redefs [dispatch/close! (fn [_]
                                                   (swap! calls conj :dispatcher)
                                                   (throw interrupted))
                                 source/close! (fn [_]
                                                 (swap! calls conj
                                                        [:source (.isInterrupted
                                                                  (Thread/currentThread))]))]
                      (live/close! system))
            interrupted? (.isInterrupted (Thread/currentThread))]
        (Thread/interrupted)
        (is (identical? system outcome))
        (is (= [:dispatcher [:source true]] @calls)
            "Source release must execute after an interrupted dispatcher close")
        (is interrupted? "The best-effort cleanup must retain interruption")
        (is (live/closed? system)))
      (finally
        (Thread/interrupted)))))

(deftest core-assertion-debug-during-close-still-closes-owned-resources-test
  (let [system (live/create {:dispatch-options {:threads 1 :queue-size 2}})
        seen (atom [])
        noisy (assoc-in system [:options :debug-fn]
                        (fn [{:keys [event]}]
                          (swap! seen conj event)
                          (throw (AssertionError. "shutdown trace failed"))))]
    (try
      (let [result (try (live/close! noisy) (catch Throwable error error))]
        (is (identical? noisy result)
            "A failed diagnostic callback must not throw from close!"))
      (is (live/closed? noisy))
      (is (source/closed? (:source noisy)))
      (is (dispatch/closed? (:dispatcher noisy)))
      (is (= [:gesso.live.core/closing :gesso.live.core/closed] @seen))
      (finally
        (dispatch/close! (:dispatcher system))
        (source/close! (:source system))))))

(deftest core-assertion-debug-after-commit-does-not-report-delivery-failure-test
  ;; Delivery is post-commit; a broken observer must not be classified as a
  ;; delivery failure or stop an otherwise valid source invalidation.
  (let [base (live/create {:rules (request-rules)
                           :dispatch-options {:threads 1 :queue-size 4}})
        seen (atom [])
        noisy (assoc-in base [:options :debug-fn]
                        (fn [{:keys [event]}]
                          (swap! seen conj event)
                          (throw (AssertionError. "commit tracing failed"))))
        tap (source/subscribe (:source base) {:topic :request :id "req-1"})]
    (try
      (with-xtdb-stub
        'execute-tx-from!
        (fn [_ctx _ops _opts]
          {:tx-result {:tx-id 42}
           :consistency sample-consistency
           :progression sample-commit-progression})
        (fn []
          (let [outcome (try
                          (live/transact-and-notify!
                           noisy {:xtdb/connectable :write-node}
                           {:tx-ops sample-tx :change request-change :emit :sync})
                          (catch Throwable error error))]
            (is (map? outcome)
                "Tracing cannot turn a committed write into delivery failure")
            (when (map? outcome)
              (is (= {:tx-id 42} (:tx-result outcome)))
              (is (= :emitted-many
                     (get-in outcome [:emit-results 0 :status]))))
            (is (= "req-1" (:id (deref (s/take! tap) 300 ::timeout)))
                "Tracing cannot prevent source delivery")
            (is (some #{:gesso.live.core/emit-expanded} @seen)))))
      (finally
        (live/close! base)))))

(deftest core-assertion-in-post-commit-result-hook-is-observational-test
  (let [seen (atom [])
        system {:options
                {:debug-fn (fn [{:keys [event]}]
                             (swap! seen conj event)
                             (throw (AssertionError. "result trace failed")))}}]
    (with-xtdb-stub
      'execute-tx-from!
      (fn [_ctx _ops _opts]
        {:tx-result {:tx-id 42}
         :consistency sample-consistency
         :progression sample-commit-progression})
      (fn []
        (let [result (try
                       (live/transact-and-notify!
                        system {:xtdb/connectable :write-node}
                        {:tx-ops sample-tx :emit false})
                       (catch Throwable error error))]
          (is (map? result)
              "Committed transactions cannot fail in result debug callbacks")
          (when (map? result)
            (is (= {:tx-id 42} (:tx-result result)))
            (is (false? (:emit result))))
          (is (= [:gesso.live.core/transact-and-notify] @seen)))))))

(deftest core-debug-failure-does-not-abort-system-creation-test
  (let [seen (atom [])
        system (live/create
                {:rules (request-rules)
                 :dispatch-options {:threads 1 :queue-size 2}
                 :debug-fn (fn [entry]
                             (swap! seen conj (:event entry))
                             (throw (ex-info "diagnostics unavailable" {})))})]
    (try
      (is (= [:gesso.live.core/created] @seen))
      (is (not (live/closed? system)))
      (is (not (source/closed? (:source system))))
      (is (not (dispatch/closed? (:dispatcher system))))
      (finally
        (live/close! system)))))

(deftest core-debug-failure-does-not-skip-owned-shutdown-test
  (let [system (live/create {:dispatch-options {:threads 1 :queue-size 2}})
        seen (atom [])
        noisy (assoc-in system [:options :debug-fn]
                        (fn [entry]
                          (swap! seen conj (:event entry))
                          (throw (ex-info "diagnostics unavailable" {}))))]
    (try
      (is (identical? noisy (live/close! noisy)))
      (is (live/closed? noisy))
      (is (source/closed? (:source noisy)))
      (is (dispatch/closed? (:dispatcher noisy)))
      (is (= [:gesso.live.core/closing :gesso.live.core/closed] @seen))
      (finally
        ;; Also clean up resources when testing the unfixed implementation.
        (dispatch/close! (:dispatcher system))
        (source/close! (:source system))))))

(deftest core-debug-failure-after-xtdb-commit-does-not-interrupt-live-delivery-test
  (let [base (live/create {:rules (request-rules)
                           :dispatch-options {:threads 1 :queue-size 4}})
        seen (atom [])
        noisy (assoc-in base [:options :debug-fn]
                        (fn [entry]
                          (swap! seen conj (:event entry))
                          (throw (ex-info "diagnostics unavailable" {}))))
        tap (source/subscribe (:source base) {:topic :request :id "req-1"})]
    (try
      (with-xtdb-stub
        'execute-tx-from!
        (fn [_ctx _ops _options]
          {:tx-result {:tx-id 42}
           :consistency sample-consistency
           :progression sample-commit-progression})
        (fn []
          (let [result (live/transact-and-notify!
                        noisy {:xtdb/connectable :write-node}
                        {:tx-ops sample-tx :change request-change :emit :sync})]
            (is (= {:tx-id 42} (:tx-result result)))
            (is (= :sync (:emit result)))
            (is (= :emitted-many (get-in result [:emit-results 0 :status])))
            (is (= "req-1" (:id (deref (s/take! tap) 1000 ::timeout))))
            (is (some #{:gesso.live.core/emit-expanded} @seen)))))
      (finally
        (live/close! base)))))

(deftest core-rejected-sse-options-do-not-leak-source-subscription-test
  ;; Flow construction eagerly registers an indexed consumer. Transport
  ;; validation must precede that registration or malformed SSE options leave
  ;; an unconsumed subscription in an otherwise healthy source.
  (let [system (live/create)]
    (try
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #":keepalive-ms must be a positive integer"
           (live/start-sse! system {:topic :request :id "req-1"}
                            {:sse-options {:keepalive-ms -1}})))
      (is (zero? (:subscription-count (source/stats (:source system))))
          "a rejected SSE request must not register an indexed subscription")
      (is (zero? (:tap-count (source/stats (:source system)))))
      (is (not (source/closed? (:source system)))
          "rejecting one request must not close the shared Live source")
      (finally
        (live/close! system)))))

(deftest core-rejected-sse-headers-do-not-leak-source-subscription-test
  ;; Ring response creation merges headers after the transport has started.
  ;; Invalid header container types must be rejected before an eager indexed
  ;; Live subscription is registered; otherwise start-sse! loses its handle.
  (let [system (live/create)]
    (try
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #":headers must be a map"
           (live/start-sse! system {:topic :request :id "req-1"}
                            {:sse-options {:headers :invalid}})))
      (is (zero? (:subscription-count (source/stats (:source system))))
          "invalid response headers must not leak an indexed subscription")
      (is (zero? (:tap-count (source/stats (:source system)))))
      (is (not (source/closed? (:source system))))
      (finally
        (live/close! system)))))

(deftest core-rejects-unrepresentable-keepalive-before-source-subscription-test
  ;; A positive Clojure BigInt can exceed the signed long consumed by
  ;; Missionary's timer. Startup then fails after an eager source subscription
  ;; is opened, but before the event consumer can close that subscription.
  (let [system (live/create)
        overflow-ms (inc (bigint Long/MAX_VALUE))]
    (try
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #":keepalive-ms must be a positive integer"
           (live/start-sse! system {:topic :request :id "req-1"}
                            {:sse-options {:keepalive-ms overflow-ms}})))
      (is (zero? (:subscription-count (source/stats (:source system))))
          "reject an unrepresentable timer before opening the source tap")
      (is (zero? (:tap-count (source/stats (:source system)))))
      (is (not (source/closed? (:source system))))
      (finally
        (live/close! system)))))

(deftest core-sse-synchronous-startup-failure-releases-unstarted-flow-test
  ;; The indexed source subscription is opened before SSE starts. If
  ;; keepalive startup fails synchronously, the event flow is never collected.
  ;; A closed SSE response must nevertheless retire that subscription.
  (let [system (live/create)
        boom (ex-info "timer initialization failed" {:phase :start})]
    (try
      (with-redefs-fn
        {#'gesso.live.transport.sse/keepalive-task*
         (fn [_out _opts] (throw boom))}
        (fn []
          (let [started (live/start-sse! system {:topic :request :id "req-1"}
                                         {:sse-options {:keepalive-ms 1000}})]
            (is (sse/closed? started))
            (is (s/closed? (:stream started)))
            (is (zero? (:subscription-count
                        (source/stats (:source system))))
                "terminal startup must release its never-collected source subscription")
            (is (zero? (:tap-count (source/stats (:source system))))))))
      (finally
        (live/close! system)))))

(deftest core-sse-throwing-transport-releases-eager-flow-test
  ;; Even if an unexpected transport error escapes before an SSE handle can
  ;; be returned, the public startup boundary must not strand source interest.
  (let [system (live/create)
        boom (ex-info "unexpected transport exception" {:phase :start})]
    (try
      (with-redefs [sse/start! (fn [_ _] (throw boom))]
        (is (identical? boom
                        (try
                          (live/start-sse! system {:topic :request :id "req-1"})
                          (catch Exception e e))))
        (is (zero? (:subscription-count
                    (source/stats (:source system))))
            "a thrown startup error must retire the eager source subscription"))
      (finally
        (live/close! system)))))

(deftest core-sse-release-when-closed-before-observer-test
  ;; An event producer can be cancelled before it starts collecting the
  ;; Missionary flow. Terminal SSE shutdown must still release the eager
  ;; indexed registration or broadcast tap even without an observer.
  (doseq [[path flow-options stat-key]
          [[:indexed nil :subscription-count]
           [:broadcast {:debug-fn (fn [_] nil)} :tap-count]]]
    (let [system (live/create)]
      (try
        (with-redefs-fn
          {#'gesso.live.transport.sse/write-events-task
           (fn [_out _flow _opts]
             (fn [_success _failure]
               ;; Deliberately do not start collecting the source flow.
               (fn [] nil)))}
          (fn []
            (let [started (live/start-sse! system {:topic :request :id "req-1"}
                                           {:flow-options flow-options})]
              (is (= 1 (stat-key (source/stats (:source system))))
                  (str path " eager registration exists before close"))
              (sse/cancel! started)
              (is (sse/closed? started))
              (is (zero? (stat-key (source/stats (:source system))))
                  (str path " must release registration on terminal close")))))
        (finally
          (live/close! system))))))
