(ns gesso.live.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.consistency.xtdb :as xtdb-live]
   [gesso.live.core :as live]
   [gesso.live.fragment :as fragment]
   [gesso.live.htmx :as htmx]
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
