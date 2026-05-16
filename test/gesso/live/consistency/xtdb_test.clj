(ns gesso.live.consistency.xtdb-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.consistency.xtdb :as xtdb-live])
  (:import
   [java.time Instant]
   [xtdb.api TransactionKey]))

(def sample-query
  '(from :users [{:xt/id id} name]))

(def sample-tx
  [[:put-docs :users {:xt/id "u1" :name "Ada"}]])

(defn tx-key
  [tx-id system-time]
  (reify TransactionKey
    (getTxId [_] tx-id)
    (getSystemTime [_] system-time)))

(defn run-task
  [task]
  (let [p (promise)
        cancel (task #(deliver p {:status :success :value %})
                     #(deliver p {:status :failure :error %}))
        result (deref p 2000 ::timeout)]
    (when (= ::timeout result)
      (cancel))
    result))

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
;; Context helpers
;; -----------------------------------------------------------------------------

(deftest connectable-from-prefers-general-write-connectables-test
  (is (= :explicit
         (xtdb-live/connectable-from
          {:xtdb/connectable :explicit
           :xtdb/conn :conn
           :xtdb/node :node
           :biff/conn :biff-conn
           :biff/node :biff-node})))

  (is (= :conn
         (xtdb-live/connectable-from
          {:xtdb/conn :conn
           :xtdb/node :node
           :biff/conn :biff-conn
           :biff/node :biff-node})))

  (is (= :raw
         (xtdb-live/connectable-from :raw))))

(deftest read-connectable-from-prefers-read-and-request-scoped-connectables-test
  (is (= :read
         (xtdb-live/read-connectable-from
          {:xtdb/read-connectable :read
           :xtdb/conn :conn
           :biff/conn :biff-conn
           :xtdb/connectable :connectable
           :xtdb/node :node
           :biff/node :biff-node})))

  (is (= :conn
         (xtdb-live/read-connectable-from
          {:xtdb/conn :conn
           :biff/conn :biff-conn
           :xtdb/connectable :connectable
           :xtdb/node :node
           :biff/node :biff-node})))

  (is (= :biff-conn
         (xtdb-live/read-connectable-from
          {:biff/conn :biff-conn
           :xtdb/connectable :connectable
           :xtdb/node :node
           :biff/node :biff-node}))))

(deftest require-connectable-throws-on-missing-connectable-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Missing XTDB2 connectable"
       (xtdb-live/require-connectable! nil))))

;; -----------------------------------------------------------------------------
;; Options and consistency maps
;; -----------------------------------------------------------------------------

(deftest prepare-options-keeps-only-adapter-options-test
  (is (= {:debug-fn nil}
         (xtdb-live/prepare-options!
          {:database :xtdb
           :await-token "tok"
           :debug-fn nil}))))

(deftest prepare-options-rejects-non-function-debug-fn-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"must be a function"
       (xtdb-live/prepare-options! {:debug-fn :nope}))))

(deftest normalize-consistency-keeps-only-supported-keys-test
  (let [t (Instant/parse "2026-01-01T00:00:00Z")]
    (is (= {:await-token "await-1"
            :snapshot-token "snap-1"
            :snapshot-time t
            :current-time t
            :default-tz "UTC"
            :tx-id 42
            :system-time t}
           (xtdb-live/normalize-consistency
            {:await-token "await-1"
             :snapshot-token "snap-1"
             :snapshot-time t
             :current-time t
             :default-tz "UTC"
             :tx-id 42
             :system-time t
             :ignored true})))))

(deftest consistency-from-uses-only-explicit-consistency-test
  (is (= {:await-token "tok-1"}
         (xtdb-live/consistency-from
          {:xtdb/node :shared-node
           :biff/node :shared-biff-node
           :consistency {:await-token "tok-1"}}))))

(deftest consistency-from-does-not-infer-from-node-state-test
  (is (= {}
         (xtdb-live/consistency-from
          {:xtdb/node :shared-node
           :biff/node :shared-biff-node}))))

(deftest consistency-from-precedence-test
  (is (= {:snapshot-token "gesso"}
         (xtdb-live/consistency-from
          {:gesso.live/consistency {:snapshot-token "gesso"}
           :gesso.live.xtdb/consistency {:snapshot-token "gesso-xtdb"}
           :xtdb/consistency {:snapshot-token "xtdb"}
           :consistency {:snapshot-token "plain"}})))

  (is (= {:snapshot-token "gesso-xtdb"}
         (xtdb-live/consistency-from
          {:gesso.live.xtdb/consistency {:snapshot-token "gesso-xtdb"}
           :xtdb/consistency {:snapshot-token "xtdb"}
           :consistency {:snapshot-token "plain"}}))))

(deftest query-consistency-drops-tx-metadata-test
  (let [t (Instant/parse "2026-01-01T00:00:00Z")]
    (is (= {:snapshot-time t
            :await-token "tok"}
           (xtdb-live/query-consistency
            {:tx-id 42
             :system-time t
             :snapshot-time t
             :await-token "tok"})))))

(deftest query-opts-keeps-only-query-options-test
  (is (= {:database :xtdb
          :key-fn :kebab-case-keyword
          :snapshot-token "snap"}
         (xtdb-live/query-opts
          {:database :xtdb
           :key-fn :kebab-case-keyword
           :snapshot-token "snap"
           :metadata {:ignored true}
           :debug-fn println}))))

(deftest consistent-query-opts-merges-consistency-and-explicit-options-test
  (let [t1 (Instant/parse "2026-01-01T00:00:00Z")
        t2 (Instant/parse "2026-01-02T00:00:00Z")]
    (is (= {:snapshot-time t2
            :await-token "tok"
            :key-fn :kebab-case-keyword}
           (xtdb-live/consistent-query-opts
            {:snapshot-time t1
             :await-token "tok"}
            {:snapshot-time t2
             :key-fn :kebab-case-keyword})))))

(deftest tx-opts-keeps-only-transaction-options-test
  (let [t (Instant/parse "2026-01-01T00:00:00Z")]
    (is (= {:database :xtdb
            :system-time t
            :metadata {:request-id "r1"}
            :authn {:user "u" :password "p"}}
           (xtdb-live/tx-opts
            {:database :xtdb
             :system-time t
             :metadata {:request-id "r1"}
             :authn {:user "u" :password "p"}
             :await-token "ignored"
             :debug-fn println})))))

;; -----------------------------------------------------------------------------
;; Fragment consistency dimensions
;; -----------------------------------------------------------------------------

(deftest consistency-fragment-dimension-test
  (is (nil? (xtdb-live/consistency-fragment-dimension nil)))

  (is (= [:xtdb2/read-consistency {:snapshot-token "snap"}]
         (xtdb-live/consistency-fragment-dimension
          {:snapshot-token "snap"}))))

(deftest with-consistency-dimension-test
  (is (= {:entity [:user "u1"]
          :consistency-token [:xtdb2/read-consistency {:snapshot-token "snap"}]}
         (xtdb-live/with-consistency-dimension
          {:entity [:user "u1"]}
          {:snapshot-token "snap"})))

  (is (= {:entity [:user "u1"]}
         (xtdb-live/with-consistency-dimension
          {:entity [:user "u1"]}
          nil))))

(deftest with-consistency-dimension-from-test
  (is (= {:entity [:user "u1"]
          :consistency-token [:xtdb2/read-consistency {:await-token "tok"}]}
         (xtdb-live/with-consistency-dimension-from
          {:entity [:user "u1"]}
          {:consistency {:await-token "tok"}}))))

;; -----------------------------------------------------------------------------
;; Query wrappers
;; -----------------------------------------------------------------------------

(deftest q-calls-xtdb-q-with-filtered-options-test
  (let [calls (atom [])]
    (with-xtdb-stub
      '*q*
      (fn [connectable query opts]
        (swap! calls conj [connectable query opts])
        [{:ok true}])
      (fn []
        (is (= [{:ok true}]
               (xtdb-live/q :conn sample-query
                            {:database :xtdb
                             :key-fn :kebab-case-keyword
                             :metadata {:ignored true}})))
        (is (= [[:conn sample-query {:database :xtdb
                                     :key-fn :kebab-case-keyword}]]
               @calls))))))

(deftest q-consistent-applies-consistency-as-query-opts-test
  (let [calls (atom [])
        t (Instant/parse "2026-01-01T00:00:00Z")]
    (with-xtdb-stub
      '*q*
      (fn [connectable query opts]
        (swap! calls conj [connectable query opts])
        [{:ok true}])
      (fn []
        (is (= [{:ok true}]
               (xtdb-live/q-consistent
                :conn
                sample-query
                {:snapshot-time t
                 :tx-id 42}
                {:key-fn :kebab-case-keyword})))
        (is (= [[:conn sample-query {:snapshot-time t
                                     :key-fn :kebab-case-keyword}]]
               @calls))))))

(deftest q-consistent-from-uses-read-connectable-and-explicit-consistency-test
  (let [calls (atom [])
        ctx {:xtdb/read-connectable :read
             :xtdb/conn :conn
             :consistency {:snapshot-token "snap"}}]
    (with-xtdb-stub
      '*q*
      (fn [connectable query opts]
        (swap! calls conj [connectable query opts])
        [{:ok true}])
      (fn []
        (is (= [{:ok true}]
               (xtdb-live/q-consistent-from ctx sample-query)))
        (is (= [[:read sample-query {:snapshot-token "snap"}]]
               @calls))))))

(deftest plan-q-consistent-applies-consistency-as-query-opts-test
  (let [calls (atom [])
        planned ::planned]
    (with-xtdb-stub
      '*plan-q*
      (fn [connectable query opts]
        (swap! calls conj [connectable query opts])
        planned)
      (fn []
        (is (= planned
               (xtdb-live/plan-q-consistent
                :conn
                sample-query
                {:await-token "tok"}
                {:database :xtdb})))
        (is (= [[:conn sample-query {:await-token "tok"
                                     :database :xtdb}]]
               @calls))))))

;; -----------------------------------------------------------------------------
;; Transaction result consistency
;; -----------------------------------------------------------------------------

(deftest tx-result-consistency-from-submit-tx-map-test
  (is (= {:tx-id 100}
         (xtdb-live/tx-result-consistency
          {:tx-id 100}))))

(deftest tx-result-consistency-from-transaction-key-test
  (let [t (Instant/parse "2026-01-01T00:00:00Z")]
    (is (= {:tx-id 101
            :system-time t
            :snapshot-time t}
           (xtdb-live/tx-result-consistency
            (tx-key 101 t))))))

(deftest tx-consistency-is-single-arity-test
  (is (= {:tx-id 100}
         (xtdb-live/tx-consistency {:tx-id 100}))))

;; -----------------------------------------------------------------------------
;; Transaction wrappers
;; -----------------------------------------------------------------------------

(deftest submit-tx-calls-xtdb-submit-tx-and-filters-opts-test
  (let [calls (atom [])
        debug-events (atom [])]
    (with-xtdb-stub
      '*submit-tx*
      (fn [connectable tx-ops opts]
        (swap! calls conj [connectable tx-ops opts])
        {:tx-id 1})
      (fn []
        (is (= {:tx-result {:tx-id 1}
                :consistency {:tx-id 1}}
               (xtdb-live/submit-tx!
                :conn
                sample-tx
                {:database :xtdb
                 :metadata {:request-id "r1"}
                 :debug-fn #(swap! debug-events conj %)
                 :await-token "ignored"})))
        (is (= [[:conn sample-tx {:database :xtdb
                                  :metadata {:request-id "r1"}}]]
               @calls))
        (is (= [:gesso.live.xtdb/submit-tx-started
                :gesso.live.xtdb/submit-tx-succeeded]
               (mapv :event @debug-events)))))))

(deftest submit-tx-from-uses-connectable-from-test
  (let [calls (atom [])]
    (with-xtdb-stub
      '*submit-tx*
      (fn [connectable tx-ops opts]
        (swap! calls conj [connectable tx-ops opts])
        {:tx-id 1})
      (fn []
        (is (= {:tx-result {:tx-id 1}
                :consistency {:tx-id 1}}
               (xtdb-live/submit-tx-from!
                {:xtdb/connectable :connectable
                 :xtdb/conn :conn}
                sample-tx
                {:database :xtdb})))
        (is (= [[:connectable sample-tx {:database :xtdb}]]
               @calls))))))

(deftest execute-tx-calls-xtdb-execute-tx-and-derives-snapshot-time-test
  (let [calls (atom [])
        t (Instant/parse "2026-01-01T00:00:00Z")
        result-tx-key (tx-key 2 t)
        debug-events (atom [])]
    (with-xtdb-stub
      '*execute-tx*
      (fn [connectable tx-ops opts]
        (swap! calls conj [connectable tx-ops opts])
        result-tx-key)
      (fn []
        (is (= {:tx-result result-tx-key
                :consistency {:tx-id 2
                              :system-time t
                              :snapshot-time t}}
               (xtdb-live/execute-tx!
                :conn
                sample-tx
                {:database :xtdb
                 :metadata {:request-id "r2"}
                 :debug-fn #(swap! debug-events conj %)
                 :await-token "ignored"})))
        (is (= [[:conn sample-tx {:database :xtdb
                                  :metadata {:request-id "r2"}}]]
               @calls))
        (is (= [:gesso.live.xtdb/execute-tx-started
                :gesso.live.xtdb/execute-tx-succeeded]
               (mapv :event @debug-events)))))))

(deftest execute-tx-from-uses-connectable-from-test
  (let [calls (atom [])
        t (Instant/parse "2026-01-01T00:00:00Z")
        result-tx-key (tx-key 3 t)]
    (with-xtdb-stub
      '*execute-tx*
      (fn [connectable tx-ops opts]
        (swap! calls conj [connectable tx-ops opts])
        result-tx-key)
      (fn []
        (is (= {:tx-result result-tx-key
                :consistency {:tx-id 3
                              :system-time t
                              :snapshot-time t}}
               (xtdb-live/execute-tx-from!
                {:xtdb/connectable :connectable
                 :xtdb/conn :conn}
                sample-tx
                {:database :xtdb})))
        (is (= [[:connectable sample-tx {:database :xtdb}]]
               @calls))))))

(deftest submit-tx-propagates-failure-test
  (let [boom (ex-info "submit boom" {})]
    (with-xtdb-stub
      '*submit-tx*
      (fn [_ _ _]
        (throw boom))
      (fn []
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"submit boom"
             (xtdb-live/submit-tx! :conn sample-tx)))))))

(deftest execute-tx-propagates-failure-test
  (let [boom (ex-info "execute boom" {})]
    (with-xtdb-stub
      '*execute-tx*
      (fn [_ _ _]
        (throw boom))
      (fn []
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"execute boom"
             (xtdb-live/execute-tx! :conn sample-tx)))))))

;; -----------------------------------------------------------------------------
;; Task wrappers
;; -----------------------------------------------------------------------------

(deftest submit-tx-task-runs-submit-tx-test
  (with-xtdb-stub
    '*submit-tx*
    (fn [_ _ _]
      {:tx-id 9})
    (fn []
      (is (= {:status :success
              :value {:tx-result {:tx-id 9}
                      :consistency {:tx-id 9}}}
             (run-task
              (xtdb-live/submit-tx-task :conn sample-tx)))))))

(deftest execute-tx-task-runs-execute-tx-test
  (let [t (Instant/parse "2026-01-01T00:00:00Z")
        result-tx-key (tx-key 10 t)]
    (with-xtdb-stub
      '*execute-tx*
      (fn [_ _ _]
        result-tx-key)
      (fn []
        (is (= {:status :success
                :value {:tx-result result-tx-key
                        :consistency {:tx-id 10
                                      :system-time t
                                      :snapshot-time t}}}
               (run-task
                (xtdb-live/execute-tx-task :conn sample-tx))))))))

;; -----------------------------------------------------------------------------
;; Tx-op helpers
;; -----------------------------------------------------------------------------

(deftest put-docs-op-test
  (is (= [:put-docs :users {:xt/id "u1"} {:xt/id "u2"}]
         (xtdb-live/put-docs-op
          :users
          {:xt/id "u1"}
          {:xt/id "u2"})))

  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Missing XTDB2 table"
       (xtdb-live/put-docs-op nil {:xt/id "u1"})))

  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Missing XTDB2 docs"
       (xtdb-live/put-docs-op :users))))

(deftest delete-docs-op-test
  (is (= [:delete-docs :users "u1" "u2"]
         (xtdb-live/delete-docs-op :users "u1" "u2")))

  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Missing XTDB2 table"
       (xtdb-live/delete-docs-op nil "u1")))

  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Missing XTDB2 doc ids"
       (xtdb-live/delete-docs-op :users))))

(deftest put-doc-calls-execute-tx-test
  (let [calls (atom [])
        t (Instant/parse "2026-01-01T00:00:00Z")
        result-tx-key (tx-key 11 t)]
    (with-xtdb-stub
      '*execute-tx*
      (fn [connectable tx-ops opts]
        (swap! calls conj [connectable tx-ops opts])
        result-tx-key)
      (fn []
        (is (= {:tx-result result-tx-key
                :consistency {:tx-id 11
                              :system-time t
                              :snapshot-time t}}
               (xtdb-live/put-doc!
                :conn
                :users
                {:xt/id "u1"}
                {:database :xtdb})))
        (is (= [[:conn [[:put-docs :users {:xt/id "u1"}]] {:database :xtdb}]]
               @calls))))))

(deftest put-doc-from-calls-execute-tx-from-test
  (let [calls (atom [])
        t (Instant/parse "2026-01-01T00:00:00Z")
        result-tx-key (tx-key 12 t)]
    (with-xtdb-stub
      '*execute-tx*
      (fn [connectable tx-ops opts]
        (swap! calls conj [connectable tx-ops opts])
        result-tx-key)
      (fn []
        (is (= {:tx-result result-tx-key
                :consistency {:tx-id 12
                              :system-time t
                              :snapshot-time t}}
               (xtdb-live/put-doc-from!
                {:xtdb/connectable :connectable}
                :users
                {:xt/id "u1"}
                {:database :xtdb})))
        (is (= [[:connectable [[:put-docs :users {:xt/id "u1"}]] {:database :xtdb}]]
               @calls))))))
