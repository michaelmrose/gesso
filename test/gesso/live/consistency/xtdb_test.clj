(ns gesso.live.consistency.xtdb-test
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.live.consistency.xtdb :as xtdb-live]
   [missionary.core :as m]
   [xtdb.api :as xt])
  (:import
   [xtdb.api DataSource]))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(def timeout-ms 1000)

(defn fake-datasource
  ([] (fake-datasource nil))
  ([initial-token]
   (let [token (atom initial-token)]
     (reify DataSource
       (getAwaitToken [_]
         @token)
       (setAwaitToken [_ token']
         (reset! token token'))
       (createConnectionBuilder [_]
         nil)))))

(defn start-task
  [task]
  (let [p (promise)
        cancel (task #(deliver p {:status :success :value %})
                     #(deliver p {:status :failure :error %}))]
    {:promise p
     :cancel cancel}))

(defn cancel-runner!
  [runner]
  ((:cancel runner)))

(defn task-result
  [runner]
  (let [result (deref (:promise runner) timeout-ms ::timeout)]
    (when (= ::timeout result)
      (cancel-runner! runner))
    result))

(defn run-task
  [task]
  (task-result (start-task task)))

(def sample-query
  "SELECT * FROM requests WHERE _id = ?")

(def sample-query-args
  [sample-query "req-1"])

(def sample-tx
  [[:put-docs :requests {:xt/id "req-1"
                         :status :open}]])

;; -----------------------------------------------------------------------------
;; Option validation
;; -----------------------------------------------------------------------------

(deftest prepare-options-accepts-defaults-test
  (is (= {:debug-fn nil
          :await-token-source nil}
         (xtdb-live/prepare-options! nil))))

(deftest prepare-options-keeps-only-adapter-options-test
  (let [debug-fn (fn [_])
        source (fake-datasource)]
    (is (= {:debug-fn debug-fn
            :await-token-source source}
           (xtdb-live/prepare-options!
            {:debug-fn debug-fn
             :await-token-source source
             :database :xtdb
             :metadata {:ignored? true}})))))

(deftest prepare-options-rejects-invalid-debug-fn-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":debug-fn must be a function"
       (xtdb-live/prepare-options! {:debug-fn :not-a-function}))))

;; -----------------------------------------------------------------------------
;; Context/connectable helpers
;; -----------------------------------------------------------------------------

(deftest connectable-from-prefers-raw-request-connectable-order-test
  (let [node (fake-datasource)
        conn :request-conn
        ctx {:xtdb/node node
             :xtdb/conn conn}]
    (is (= conn (xtdb-live/connectable-from ctx)))))

(deftest read-connectable-from-prefers-node-over-request-connection-test
  (let [node (fake-datasource)
        conn :request-conn
        ctx {:xtdb/node node
             :xtdb/conn conn}]
    (is (identical? node (xtdb-live/read-connectable-from ctx)))))

(deftest biff-context-read-connectable-prefers-biff-node-over-biff-conn-test
  (let [node (fake-datasource)
        conn :biff-conn
        ctx {:biff/node node
             :biff/conn conn}]
    (is (= conn (xtdb-live/connectable-from ctx)))
    (is (identical? node (xtdb-live/read-connectable-from ctx)))))

(deftest await-token-source-from-ignores-request-connection-test
  (let [node (fake-datasource)
        conn :biff-conn
        ctx {:biff/node node
             :biff/conn conn}]
    (is (identical? node (xtdb-live/await-token-source-from ctx)))))

(deftest require-connectable-rejects-nil-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Missing XTDB2 connectable"
       (xtdb-live/require-connectable! nil))))

;; -----------------------------------------------------------------------------
;; Await token helpers
;; -----------------------------------------------------------------------------

(deftest await-token-helpers-work-with-xtdb-datasource-test
  (let [source (fake-datasource)]
    (is (xtdb-live/can-use-await-token? source))
    (is (nil? (xtdb-live/current-await-token source)))
    (is (identical? source
                    (xtdb-live/apply-await-token! source "tok-1")))
    (is (= "tok-1" (xtdb-live/current-await-token source)))
    (is (= true (xtdb-live/try-apply-await-token! source "tok-2")))
    (is (= "tok-2" (xtdb-live/current-await-token source)))))

(deftest await-token-helpers-ignore-nil-token-test
  (let [source (fake-datasource)]
    (is (identical? source
                    (xtdb-live/apply-await-token! source nil)))
    (is (= false (xtdb-live/try-apply-await-token! source nil)))))

(deftest strict-apply-await-token-rejects-non-datasource-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Cannot apply XTDB2 await token"
       (xtdb-live/apply-await-token! :not-a-datasource "tok"))))

(deftest try-apply-await-token-noops-for-non-datasource-test
  (is (= false
         (xtdb-live/try-apply-await-token! :not-a-datasource "tok"))))

(deftest current-consistency-reads-await-token-test
  (let [source (fake-datasource "tok-1")]
    (is (= {:await-token "tok-1"}
           (xtdb-live/current-consistency source)))))

;; -----------------------------------------------------------------------------
;; Consistency maps/query opts
;; -----------------------------------------------------------------------------

(deftest normalize-consistency-keeps-only-known-fields-test
  (is (= {:await-token "await"
          :snapshot-token "snap"
          :snapshot-time :snapshot-time
          :current-time :current-time
          :default-tz :tz
          :tx-id 12
          :system-time :system-time}
         (xtdb-live/normalize-consistency
          {:await-token "await"
           :snapshot-token "snap"
           :snapshot-time :snapshot-time
           :current-time :current-time
           :default-tz :tz
           :tx-id 12
           :system-time :system-time
           :extra :ignored}))))

(deftest consistency-from-prefers-explicit-context-consistency-test
  (let [source (fake-datasource "await-from-source")
        ctx {:xtdb/node source
             :gesso.live/consistency {:await-token "explicit"
                                      :extra :ignored}}]
    (is (= {:await-token "explicit"}
           (xtdb-live/consistency-from ctx)))))

(deftest consistency-from-falls-back-to-await-token-source-test
  (let [source (fake-datasource "await-from-source")
        ctx {:xtdb/node source}]
    (is (= {:await-token "await-from-source"}
           (xtdb-live/consistency-from ctx)))))

(deftest query-opts-filters-query-options-test
  (is (= {:await-token "await"
          :snapshot-token "snap"
          :key-fn :kebab-case-keyword
          :database :xtdb}
         (xtdb-live/query-opts
          {:await-token "await"
           :snapshot-token "snap"
           :key-fn :kebab-case-keyword
           :database :xtdb
           :metadata :not-query-opt}))))

(deftest consistent-query-opts-merges-consistency-and-explicit-options-test
  (is (= {:await-token "explicit-await"
          :snapshot-token "snap"
          :database :xtdb}
         (xtdb-live/consistent-query-opts
          {:await-token "consistency-await"
           :snapshot-token "snap"}
          {:await-token "explicit-await"
           :database :xtdb}))))

(deftest tx-opts-filters-transaction-options-test
  (is (= {:database :xtdb
          :metadata {:request-id "r1"}
          :authn {:user "u" :password "p"}}
         (xtdb-live/tx-opts
          {:database :xtdb
           :metadata {:request-id "r1"}
           :authn {:user "u" :password "p"}
           :debug-fn (fn [_])
           :await-token-source (fake-datasource)}))))

(deftest consistency-fragment-dimension-test
  (is (nil? (xtdb-live/consistency-fragment-dimension nil)))
  (is (= [:xtdb2/read-consistency {:await-token "tok"}]
         (xtdb-live/consistency-fragment-dimension
          {:await-token "tok"
           :extra :ignored}))))

(deftest with-consistency-dimension-test
  (is (= {:scope [:request "req-1"]
          :consistency-token [:xtdb2/read-consistency {:await-token "tok"}]}
         (xtdb-live/with-consistency-dimension
          {:scope [:request "req-1"]}
          {:await-token "tok"})))
  (is (= {:scope [:request "req-1"]}
         (xtdb-live/with-consistency-dimension
          {:scope [:request "req-1"]}
          nil))))

(deftest with-consistency-dimension-from-test
  (let [source (fake-datasource "tok")
        ctx {:xtdb/node source}]
    (is (= {:scope [:request "req-1"]
            :consistency-token [:xtdb2/read-consistency {:await-token "tok"}]}
           (xtdb-live/with-consistency-dimension-from
            {:scope [:request "req-1"]}
            ctx)))))

;; -----------------------------------------------------------------------------
;; Query wrappers
;; -----------------------------------------------------------------------------

(deftest q-calls-xtdb-q-with-filtered-opts-test
  (let [seen (atom nil)]
    (with-redefs [xt/q (fn [& args]
                         (reset! seen args)
                         [:rows])]
      (is (= [:rows]
             (xtdb-live/q :conn sample-query-args
                          {:await-token "tok"
                           :metadata :ignored})))
      (is (= [:conn sample-query-args {:await-token "tok"}]
             @seen)))))

(deftest q-from-uses-raw-connectable-from-test
  (let [node (fake-datasource)
        ctx {:xtdb/node node
             :xtdb/conn :request-conn}
        seen (atom nil)]
    (with-redefs [xt/q (fn [& args]
                         (reset! seen args)
                         [:rows])]
      (is (= [:rows] (xtdb-live/q-from ctx sample-query-args)))
      (is (= [:request-conn sample-query-args]
             @seen)))))

(deftest q-consistent-uses-consistency-query-opts-test
  (let [seen (atom nil)]
    (with-redefs [xt/q (fn [& args]
                         (reset! seen args)
                         [:rows])]
      (is (= [:rows]
             (xtdb-live/q-consistent
              :conn
              sample-query-args
              {:await-token "tok"}
              {:database :xtdb})))
      (is (= [:conn sample-query-args {:await-token "tok"
                                       :database :xtdb}]
             @seen)))))

(deftest q-consistent-from-prefers-read-connectable-and-context-consistency-test
  (let [node (fake-datasource "await-from-node")
        ctx {:xtdb/node node
             :xtdb/conn :request-conn}
        seen (atom nil)]
    (with-redefs [xt/q (fn [& args]
                         (reset! seen args)
                         [:rows])]
      (is (= [:rows]
             (xtdb-live/q-consistent-from ctx sample-query-args)))
      (is (= [node sample-query-args {:await-token "await-from-node"}]
             @seen)))))

(deftest q-task-runs-q-test
  (with-redefs [xt/q (fn [& _args] [:rows])]
    (is (= {:status :success
            :value [:rows]}
           (run-task
            (xtdb-live/q-task :conn sample-query-args))))))

(deftest q-consistent-task-from-runs-consistent-query-test
  (let [node (fake-datasource "tok")
        ctx {:xtdb/node node}]
    (with-redefs [xt/q (fn [& args] args)]
      (is (= {:status :success
              :value [node sample-query-args {:await-token "tok"}]}
             (run-task
              (xtdb-live/q-consistent-task-from ctx sample-query-args)))))))

(deftest plan-q-calls-xtdb-plan-q-test
  (let [seen (atom nil)]
    (with-redefs [xt/plan-q (fn [& args]
                              (reset! seen args)
                              :reducible)]
      (is (= :reducible
             (xtdb-live/plan-q :conn sample-query-args
                               {:snapshot-token "snap"
                                :debug-fn :ignored})))
      (is (= [:conn sample-query-args {:snapshot-token "snap"}]
             @seen)))))

(deftest plan-q-consistent-from-prefers-read-connectable-and-context-consistency-test
  (let [node (fake-datasource "tok")
        ctx {:biff/node node
             :biff/conn :request-conn}
        seen (atom nil)]
    (with-redefs [xt/plan-q (fn [& args]
                              (reset! seen args)
                              :reducible)]
      (is (= :reducible
             (xtdb-live/plan-q-consistent-from ctx sample-query-args)))
      (is (= [node sample-query-args {:await-token "tok"}]
             @seen)))))

;; -----------------------------------------------------------------------------
;; Tx result/transaction wrappers
;; -----------------------------------------------------------------------------

(deftest tx-result-consistency-keeps-tx-id-and-system-time-test
  (is (= {:tx-id 7
          :system-time :system-time}
         (xtdb-live/tx-result-consistency
          {:tx-id 7
           :system-time :system-time
           :extra :ignored}))))

(deftest tx-consistency-includes-await-token-from-source-test
  (let [source (fake-datasource "await-token")]
    (is (= {:tx-id 7
            :await-token "await-token"}
           (xtdb-live/tx-consistency {:tx-id 7} source)))))

(deftest submit-tx-calls-xtdb-submit-tx-and-filters-opts-test
  (let [source (fake-datasource)
        seen (atom nil)]
    (with-redefs [xt/submit-tx (fn [connectable tx-ops opts]
                                 (.setAwaitToken ^DataSource connectable "await-after-submit")
                                 (reset! seen [connectable tx-ops opts])
                                 {:tx-id 1})]
      (is (= {:tx-result {:tx-id 1}
              :consistency {:tx-id 1
                            :await-token "await-after-submit"}}
             (xtdb-live/submit-tx!
              source
              sample-tx
              {:database :xtdb
               :metadata {:request-id "r1"}
               :debug-fn (fn [_])
               :await-token-source :ignored-for-datasource})))
      (is (= [source sample-tx {:database :xtdb
                                :metadata {:request-id "r1"}}]
             @seen)))))

(deftest submit-tx-can-use-explicit-await-token-source-test
  (let [conn :jdbc-conn
        source (fake-datasource "await-from-source")
        seen (atom nil)]
    (with-redefs [xt/submit-tx (fn [connectable tx-ops opts]
                                 (reset! seen [connectable tx-ops opts])
                                 {:tx-id 1})]
      (is (= {:tx-result {:tx-id 1}
              :consistency {:tx-id 1
                            :await-token "await-from-source"}}
             (xtdb-live/submit-tx!
              conn
              sample-tx
              {:await-token-source source})))
      (is (= [conn sample-tx {}]
             @seen)))))

(deftest submit-tx-from-uses-context-connectable-and-await-token-source-test
  (let [node (fake-datasource "await-from-node")
        ctx {:biff/node node
             :biff/conn :request-conn}
        seen (atom nil)]
    (with-redefs [xt/submit-tx (fn [connectable tx-ops opts]
                                 (reset! seen [connectable tx-ops opts])
                                 {:tx-id 1})]
      (is (= {:tx-result {:tx-id 1}
              :consistency {:tx-id 1
                            :await-token "await-from-node"}}
             (xtdb-live/submit-tx-from! ctx sample-tx)))
      ;; Write path still uses connectable-from, which prefers request conn.
      (is (= [:request-conn sample-tx {}]
             @seen)))))

(deftest execute-tx-calls-xtdb-execute-tx-test
  (let [source (fake-datasource)
        seen (atom nil)]
    (with-redefs [xt/execute-tx (fn [connectable tx-ops opts]
                                  (.setAwaitToken ^DataSource connectable "await-after-exec")
                                  (reset! seen [connectable tx-ops opts])
                                  {:tx-id 2
                                   :system-time :system-time})]
      (is (= {:tx-result {:tx-id 2
                          :system-time :system-time}
              :consistency {:tx-id 2
                            :system-time :system-time
                            :await-token "await-after-exec"}}
             (xtdb-live/execute-tx!
              source
              sample-tx
              {:database :xtdb
               :authn {:user "u" :password "p"}})))
      (is (= [source sample-tx {:database :xtdb
                                :authn {:user "u" :password "p"}}]
             @seen)))))

(deftest execute-tx-from-uses-context-await-token-source-test
  (let [node (fake-datasource "await-from-node")
        ctx {:xtdb/node node
             :xtdb/conn :request-conn}
        seen (atom nil)]
    (with-redefs [xt/execute-tx (fn [connectable tx-ops opts]
                                  (reset! seen [connectable tx-ops opts])
                                  {:tx-id 2})]
      (is (= {:tx-result {:tx-id 2}
              :consistency {:tx-id 2
                            :await-token "await-from-node"}}
             (xtdb-live/execute-tx-from! ctx sample-tx)))
      (is (= [:request-conn sample-tx {}]
             @seen)))))

(deftest submit-and-execute-tx-tasks-run-on-missionary-test
  (with-redefs [xt/submit-tx (fn [& _args] {:tx-id 1})
                xt/execute-tx (fn [& _args] {:tx-id 2})]
    (is (= {:status :success
            :value {:tx-result {:tx-id 1}
                    :consistency {:tx-id 1}}}
           (run-task
            (xtdb-live/submit-tx-task :conn sample-tx))))
    (is (= {:status :success
            :value {:tx-result {:tx-id 2}
                    :consistency {:tx-id 2}}}
           (run-task
            (xtdb-live/execute-tx-task :conn sample-tx))))))

;; -----------------------------------------------------------------------------
;; Tx op helpers
;; -----------------------------------------------------------------------------

(deftest put-docs-op-builds-put-docs-operation-test
  (is (= [:put-docs :requests {:xt/id "req-1"}]
         (xtdb-live/put-docs-op :requests {:xt/id "req-1"}))))

(deftest put-docs-op-rejects-missing-table-or-docs-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Missing XTDB2 table"
       (xtdb-live/put-docs-op nil {:xt/id "req-1"})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Missing XTDB2 docs"
       (xtdb-live/put-docs-op :requests))))

(deftest delete-docs-op-builds-delete-docs-operation-test
  (is (= [:delete-docs :requests "req-1" "req-2"]
         (xtdb-live/delete-docs-op :requests "req-1" "req-2"))))

(deftest delete-docs-op-rejects-missing-table-or-doc-ids-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Missing XTDB2 table"
       (xtdb-live/delete-docs-op nil "req-1")))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Missing XTDB2 doc ids"
       (xtdb-live/delete-docs-op :requests))))

(deftest put-doc-calls-execute-tx-with-put-doc-op-test
  (let [seen (atom nil)]
    (with-redefs [xt/execute-tx (fn [connectable tx-ops opts]
                                  (reset! seen [connectable tx-ops opts])
                                  {:tx-id 3})]
      (is (= {:tx-result {:tx-id 3}
              :consistency {:tx-id 3}}
             (xtdb-live/put-doc! :conn
                                  :requests
                                  {:xt/id "req-1"}
                                  {:database :xtdb})))
      (is (= [:conn
              [[:put-docs :requests {:xt/id "req-1"}]]
              {:database :xtdb}]
             @seen)))))

(deftest put-doc-from-calls-execute-tx-from-with-context-test
  (let [node (fake-datasource "await")
        ctx {:biff/node node
             :biff/conn :conn}
        seen (atom nil)]
    (with-redefs [xt/execute-tx (fn [connectable tx-ops opts]
                                  (reset! seen [connectable tx-ops opts])
                                  {:tx-id 4})]
      (is (= {:tx-result {:tx-id 4}
              :consistency {:tx-id 4
                            :await-token "await"}}
             (xtdb-live/put-doc-from! ctx
                                       :requests
                                       {:xt/id "req-1"})))
      (is (= [:conn
              [[:put-docs :requests {:xt/id "req-1"}]]
              {}]
             @seen)))))
