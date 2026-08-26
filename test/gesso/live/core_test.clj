(ns gesso.live.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.consistency.xtdb :as xtdb-live]
   [gesso.live.core :as live]
   [gesso.live.fragment :as fragment]
   [gesso.live.htmx :as htmx]
   [gesso.live.optimistic.server :as optimistic.server]
   [gesso.live.progression :as progression]
   [gesso.live.progression.http :as progression.http]
   [gesso.live.source :as source]))

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
              :consistency-token "tx-1"})
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

(deftest execute-tx-facade-delegates-to-xtdb-helper-test
  (let [seen (atom nil)
        ctx {:xtdb/connectable :node}]
    (with-xtdb-stub
      'execute-tx-from!
      (fn
        ([ctx' tx-ops]
         (reset! seen [ctx' tx-ops])
         {:tx-result {:tx-id 1}
          :consistency {:tx-id 1}})
        ([ctx' tx-ops opts]
         (reset! seen [ctx' tx-ops opts])
         {:tx-result {:tx-id 2}
          :consistency {:tx-id 2}}))
      (fn []
        (is (= {:tx-result {:tx-id 1}
                :consistency {:tx-id 1}}
               (live/execute-tx! ctx sample-tx)))
        (is (= [ctx sample-tx]
               @seen))

        (is (= {:tx-result {:tx-id 2}
                :consistency {:tx-id 2}}
               (live/execute-tx! ctx sample-tx {:database :xtdb})))
        (is (= [ctx sample-tx {:database :xtdb}]
               @seen))))))

(deftest submit-tx-facade-delegates-to-xtdb-helper-test
  (let [seen (atom nil)
        ctx {:xtdb/connectable :node}]
    (with-xtdb-stub
      'submit-tx-from!
      (fn
        ([ctx' tx-ops]
         (reset! seen [ctx' tx-ops])
         {:tx-result {:tx-id 1}
          :consistency {:tx-id 1}})
        ([ctx' tx-ops opts]
         (reset! seen [ctx' tx-ops opts])
         {:tx-result {:tx-id 2}
          :consistency {:tx-id 2}}))
      (fn []
        (is (= {:tx-result {:tx-id 1}
                :consistency {:tx-id 1}}
               (live/submit-tx! ctx sample-tx)))
        (is (= [ctx sample-tx]
               @seen))

        (is (= {:tx-result {:tx-id 2}
                :consistency {:tx-id 2}}
               (live/submit-tx! ctx sample-tx {:database :xtdb})))
        (is (= [ctx sample-tx {:database :xtdb}]
               @seen))))))

(deftest tx-op-facades-delegate-to-xtdb-helper-test
  (is (= (xtdb-live/put-docs-op :requests {:xt/id "req-1"})
         (live/put-docs-op :requests {:xt/id "req-1"})))

  (is (= (xtdb-live/delete-docs-op :requests "req-1")
         (live/delete-docs-op :requests "req-1"))))

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

(deftest attach-consistency-assocs-normalized-consistency-onto-change-test
  (is (= (assoc request-change
                :gesso.live/consistency
                {:snapshot-time :snapshot-1
                 :tx-id 42})
         (live/attach-consistency
          request-change
          {:snapshot-time :snapshot-1
           :tx-id 42
           :ignored true}))))

(deftest attach-consistency-leaves-change-alone-when-consistency-is-empty-test
  (is (= request-change
         (live/attach-consistency request-change nil)))

  (is (= request-change
         (live/attach-consistency request-change {:ignored true}))))

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

(deftest attach-progression-preserves-or-attaches-exact-authority-test
  (is (= (assoc request-change
                :progression sample-commit-progression)
         (live/attach-progression
          request-change
          sample-commit-progression)))

  (testing "an exact repeated requirement is idempotent"
    (let [change (assoc request-change
                        :progression sample-commit-progression)]
      (is (identical? change
                      (live/attach-progression
                       change
                       sample-commit-progression)))))

  (testing "nil cannot erase an existing requirement"
    (let [change (assoc request-change
                        :progression sample-commit-progression)]
      (is (identical? change
                      (live/attach-progression change nil))))))

(deftest attach-progression-rejects-conflicting-authority-test
  (let [error
        (try
          (live/attach-progression
           (assoc request-change
                  :progression sample-other-progression)
           sample-commit-progression)
          nil
          (catch clojure.lang.ExceptionInfo e
            e))]
    (is (some? error))
    (is (= sample-other-progression
           (:existing-progression (ex-data error))))
    (is (= sample-commit-progression
           (:authoritative-progression (ex-data error))))))

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

;; -----------------------------------------------------------------------------
;; transact-and-notify!
;; -----------------------------------------------------------------------------

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

