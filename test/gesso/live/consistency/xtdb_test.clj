(ns gesso.live.consistency.xtdb-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.consistency.xtdb :as xtdb-live]
   [gesso.live.progression :as progression])
  (:import
   [java.lang.reflect InvocationHandler Proxy]
   [java.time Instant ZonedDateTime]
   [xtdb.api DataSource TransactionKey]))

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
;; Optional transport consistency token
;; -----------------------------------------------------------------------------

(deftest consistency-token-test
  (let [token-var (ns-resolve 'gesso.live.consistency.xtdb 'consistency-token)]
    (is (var? token-var)
        "XTDB consistency may still be encoded as an opaque optional transport token.")
    (is (or (nil? token-var)
            (fn? @token-var))
        "The resolved consistency-token Var must contain an ordinary function.")

    (when token-var
      (let [token-fn @token-var]
        (is (nil? (token-fn nil)))
        (is (nil? (token-fn {:ignored true})))
        (is (= [:xtdb2/read-consistency {:snapshot-token "snap"}]
               (token-fn {:snapshot-token "snap"})))
        (is (= [:xtdb2/read-consistency
                {:snapshot-time (Instant/parse "2026-01-01T00:00:00Z")
                 :tx-id 42}]
               (token-fn {:snapshot-time (Instant/parse "2026-01-01T00:00:00Z")
                          :tx-id 42
                          :ignored true})))))))

(deftest fragment-consistency-dimension-helpers-are-retired-test
  (doseq [sym '[consistency-fragment-dimension
                with-consistency-dimension
                with-consistency-dimension-from]]
    (is (nil? (ns-resolve 'gesso.live.consistency.xtdb sym))
        (str sym
             " must stay retired; fragment freshness is represented by canonical progression, not read-consistency metadata."))))

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

(deftest q-consistent-from-enforces-progression-after-caller-options-test
  (let [calls (atom [])
        t (Instant/parse "2026-01-03T00:00:00Z")
        required-basis (xtdb-live/basis :analytics 80 t)
        ctx {:xtdb/read-connectable :read
             :consistency {:snapshot-token "ordinary-token"
                           :await-token "await"}
             :gesso.live/progression (progression/requirement required-basis)}]
    (with-xtdb-stub
      '*q*
      (fn [connectable query opts]
        (swap! calls conj [connectable query opts])
        [{:ok true}])
      (fn []
        (is (= [{:ok true}]
               (xtdb-live/q-consistent-from
                ctx
                sample-query
                {:snapshot-token "caller-token"
                 :snapshot-time (Instant/parse "2025-01-01T00:00:00Z")
                 :database :analytics
                 :key-fn :kebab-case-keyword})))
        (is (= [[:read
                 sample-query
                 {:await-token "await"
                  :database :analytics
                  :key-fn :kebab-case-keyword
                  :snapshot-token (xtdb-live/basis-snapshot-token required-basis)}]]
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

(deftest portable-xtdb-basis-is-closed-and-lossless-test
  (let [t (Instant/parse "2026-01-01T00:00:00.123456789Z")
        large-tx-id 9007199254740993
        basis (xtdb-live/basis :analytics large-tx-id t)]
    (is (xtdb-live/xtdb-basis? basis))
    (is (= :gesso.live.consistency.xtdb/basis
           (:gesso.live.consistency.xtdb/type basis)))
    (is (= 1 (:gesso.live.consistency.xtdb/version basis)))
    (is (= "analytics" (:database basis)))
    ;; Decimal text must survive a CLJS/browser carrier without IEEE-754 loss.
    (is (= "9007199254740993" (:tx-id basis)))
    (is (string? (:snapshot-token basis)))
    (is (= large-tx-id (xtdb-live/basis-tx-id basis)))
    (is (= "analytics" (xtdb-live/basis-database basis)))
    (is (= (:snapshot-token basis)
           (xtdb-live/basis-snapshot-token basis)))
    (is (= t (xtdb-live/basis-system-time basis)))
    (is (not (xtdb-live/xtdb-basis? (assoc basis :extra true))))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"closed portable XTDB"
         (xtdb-live/require-xtdb-basis! (assoc basis :extra true))))))

(deftest consistency-to-basis-requires-complete-authoritative-basis-test
  (let [t (Instant/parse "2026-01-01T00:00:00Z")]
    (is (nil? (xtdb-live/consistency->basis {:tx-id 7})))
    (is (nil? (xtdb-live/consistency->basis {:system-time t})))
    (is (= (xtdb-live/basis :xtdb 7 t)
           (xtdb-live/consistency->basis
            {:tx-id 7 :system-time t}
            :xtdb)))
    (is (= (xtdb-live/basis :analytics 8 t)
           (xtdb-live/consistency->basis
            {:tx-id 8 :snapshot-time t}
            :analytics)))))

(deftest tx-result-progression-needs-complete-public-result-test
  (let [t (Instant/parse "2026-01-01T00:00:00Z")
        result (tx-key 9 t)
        expected-basis (xtdb-live/basis :xtdb 9 t)]
    (is (= expected-basis
           (xtdb-live/tx-result-basis result :xtdb)))
    (is (= (progression/requirement expected-basis)
           (xtdb-live/tx-result-progression result :xtdb)))
    ;; submit-tx's public tx-id-only result is not enough to construct the
    ;; snapshot token required for an authoritative reread.
    (is (nil? (xtdb-live/tx-result-basis {:tx-id 9} :xtdb)))
    (is (nil? (xtdb-live/tx-result-progression {:tx-id 9} :xtdb)))))

(deftest xtdb-basis-comparison-is-storage-specific-and-database-scoped-test
  (let [t1 (Instant/parse "2026-01-01T00:00:00Z")
        t2 (Instant/parse "2026-01-02T00:00:00Z")
        b1 (xtdb-live/basis :xtdb 10 t1)
        b2 (xtdb-live/basis :xtdb 11 t2)
        conflicting (xtdb-live/basis :xtdb 10 t2)
        other-db (xtdb-live/basis :analytics 11 t2)]
    (is (neg? (xtdb-live/compare-bases b1 b2)))
    (is (pos? (xtdb-live/compare-bases b2 b1)))
    (is (zero? (xtdb-live/compare-bases b1 b1)))
    (is (xtdb-live/basis-advances? b1 b2))
    (is (not (xtdb-live/basis-advances? b2 b1)))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"different databases"
         (xtdb-live/compare-bases b1 other-db)))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Conflicting XTDB progression bases"
         (xtdb-live/compare-bases b1 conflicting)))))

(deftest progression-witness-is-explicit-and-never-backward-test
  (let [t1 (Instant/parse "2026-01-01T00:00:00Z")
        t2 (Instant/parse "2026-01-02T00:00:00Z")
        b1 (xtdb-live/basis 20 t1)
        b2 (xtdb-live/basis 21 t2)]
    (is (= (progression/advance b1 b2)
           (xtdb-live/progression-witness b1 b2)))
    (is (nil? (xtdb-live/progression-witness b1 b1)))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"cannot witness a backward"
         (xtdb-live/progression-witness b2 b1)))))

(deftest strongest-required-basis-is-order-independent-test
  (let [t1 (Instant/parse "2026-01-01T00:00:00Z")
        t2 (Instant/parse "2026-01-02T00:00:00Z")
        t3 (Instant/parse "2026-01-03T00:00:00Z")
        b1 (xtdb-live/basis 30 t1)
        b2 (xtdb-live/basis 31 t2)
        b3 (xtdb-live/basis 32 t3)
        forward (progression/requirement-from-bases [b1 b2 b3])
        reverse (progression/requirement-from-bases [b3 b2 b1])]
    (is (= b3 (xtdb-live/strongest-required-basis forward)))
    (is (= b3 (xtdb-live/strongest-required-basis reverse)))
    (is (= (xtdb-live/progression-consistency forward)
           (xtdb-live/progression-consistency reverse)))
    (is (= (xtdb-live/progression-query-opts forward)
           (xtdb-live/progression-query-opts reverse)))))

(deftest strongest-required-basis-fails-closed-for-incomparable-requirements-test
  (let [t (Instant/parse "2026-01-01T00:00:00Z")
        default-basis (xtdb-live/basis :xtdb 40 t)
        other-db-basis (xtdb-live/basis :analytics 41 t)]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"different databases"
         (xtdb-live/strongest-required-basis
          (progression/requirement-from-bases
           [default-basis other-db-basis]))))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"closed portable XTDB"
         (xtdb-live/strongest-required-basis
          (progression/requirement {:opaque/basis 1}))))))

(deftest progression-query-opts-carry-exact-required-snapshot-test
  (let [t (Instant/parse "2026-01-01T00:00:00Z")
        default-basis (xtdb-live/basis :xtdb 50 t)
        analytics-basis (xtdb-live/basis :analytics 51 t)]
    (is (= {:snapshot-token (xtdb-live/basis-snapshot-token default-basis)}
           (xtdb-live/progression-query-opts
            (progression/requirement default-basis))))
    (is (= {:snapshot-token (xtdb-live/basis-snapshot-token analytics-basis)
            :database "analytics"}
           (xtdb-live/progression-query-opts
            (progression/requirement analytics-basis))))))

(deftest read-query-opts-cannot-be-weakened-by-caller-test
  (let [old-time (Instant/parse "2025-12-01T00:00:00Z")
        caller-time (Instant/parse "2025-12-15T00:00:00Z")
        required-time (Instant/parse "2026-01-01T00:00:00Z")
        required-basis (xtdb-live/basis :analytics 60 required-time)
        requirement (progression/requirement required-basis)]
    (is (= {:await-token "await"
            :key-fn :kebab-case-keyword
            :database :analytics
            :snapshot-token (xtdb-live/basis-snapshot-token required-basis)}
           (xtdb-live/read-query-opts
            {:snapshot-time old-time
             :snapshot-token "old-token"
             :await-token "await"}
            requirement
            {:snapshot-time caller-time
             :snapshot-token "caller-token"
             :database :analytics
             :key-fn :kebab-case-keyword})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"database conflicts with authoritative progression"
         (xtdb-live/read-query-opts
          {}
          requirement
          {:database :xtdb})))))

(deftest progression-from-prefers-canonical-context-key-test
  (let [t1 (Instant/parse "2026-01-01T00:00:00Z")
        t2 (Instant/parse "2026-01-02T00:00:00Z")
        canonical (progression/requirement (xtdb-live/basis 70 t1))
        convenience (progression/requirement (xtdb-live/basis 71 t2))]
    (is (= canonical
           (xtdb-live/progression-from
            {:gesso.live/progression canonical
             :progression convenience})))
    (is (= convenience
           (xtdb-live/progression-from
            {:progression convenience})))
    (is (nil? (xtdb-live/progression-from :not-a-context)))))


;; -----------------------------------------------------------------------------
;; Authoritative request frontier
;; -----------------------------------------------------------------------------

(defn data-source-stub
  []
  (Proxy/newProxyInstance
   (.getClassLoader DataSource)
   (into-array Class [DataSource])
   (reify InvocationHandler
     (invoke [_ _ _ _]
       nil))))

(deftest node-from-accepts-only-authoritative-node-sources-test
  (let [raw-data-source (data-source-stub)]
    (is (= :xtdb-node
           (xtdb-live/node-from
            {:xtdb/node :xtdb-node
             :xtdb/conn :request-conn})))
    (is (= :biff-xtdb-node
           (xtdb-live/node-from
            {:biff.xtdb/node :biff-xtdb-node
             :biff/conn :request-conn})))
    (is (= :biff-node
           (xtdb-live/node-from
            {:biff/node :biff-node})))
    (is (nil?
         (xtdb-live/node-from
          {:xtdb/conn :request-conn
           :biff/conn :biff-request-conn
           :xtdb/connectable :shared-connectable})))
    (is (nil? (xtdb-live/node-from raw-data-source)))))

(deftest latest-completed-basis-uses-one-status-observation-test
  (let [calls (atom [])
        zdt (ZonedDateTime/parse "2026-09-10T18:30:00Z")
        instant (.toInstant zdt)
        expected (xtdb-live/basis :analytics 101 instant)]
    (with-xtdb-stub
      '*status*
      (fn [node]
        (swap! calls conj node)
        {:latest-completed-txs
         {"analytics" [{:tx-id 101
                        :system-time zdt}]
          "xtdb" [{:tx-id 999
                    :system-time (Instant/parse "2026-09-10T19:00:00Z")}]}})
      (fn []
        (is (= expected
               (xtdb-live/latest-completed-basis
                {:xtdb/node :authority-node}
                :analytics)))
        (is (= [:authority-node] @calls))
        (is (= instant
               (xtdb-live/basis-system-time expected)))))))

(deftest latest-completed-basis-accepts-instant-status-coordinate-test
  (let [instant (Instant/parse "2026-09-10T18:31:00.123456789Z")]
    (with-xtdb-stub
      '*status*
      (fn [_]
        {:latest-completed-txs
         {"xtdb" [{:tx-id 102
                    :system-time instant}]}})
      (fn []
        (is (= (xtdb-live/basis :xtdb 102 instant)
               (xtdb-live/latest-completed-basis
                {:biff.xtdb/node :authority-node})))))))

(deftest latest-completed-basis-returns-nil-when-no-transaction-completed-test
  (let [calls (atom 0)]
    (with-xtdb-stub
      '*status*
      (fn [_]
        (swap! calls inc)
        {:latest-completed-txs {"xtdb" []}})
      (fn []
        (is (nil?
             (xtdb-live/latest-completed-basis
              {:biff/node :authority-node})))
        (is (= 1 @calls))))))

(deftest latest-completed-basis-rejects-incomplete-authoritative-coordinates-test
  (doseq [completed [{:tx-id nil
                      :system-time (Instant/parse "2026-09-10T18:32:00Z")}
                     {:tx-id 103
                      :system-time nil}
                     {:tx-id nil
                      :system-time nil}]]
    (with-xtdb-stub
      '*status*
      (fn [_]
        {:latest-completed-txs {"xtdb" [completed]}})
      (fn []
        (let [error
              (try
                (xtdb-live/latest-completed-basis
                 {:xtdb/node :authority-node})
                nil
                (catch clojure.lang.ExceptionInfo e
                  e))]
          (is (some? error))
          (when error
            (is (re-find #"incomplete authoritative coordinates"
                         (.getMessage error)))
            (is (= "xtdb" (:database (ex-data error))))
            (is (= completed (:latest-completed (ex-data error))))))))))

(deftest latest-completed-basis-rejects-unsupported-system-time-test
  (with-xtdb-stub
    '*status*
    (fn [_]
      {:latest-completed-txs
       {"xtdb" [{:tx-id 104
                  :system-time "2026-09-10T18:33:00Z"}]}})
    (fn []
      (let [error
            (try
              (xtdb-live/latest-completed-basis
               {:xtdb/node :authority-node})
              nil
              (catch clojure.lang.ExceptionInfo e
                e))]
        (is (some? error))
        (is (re-find #"unsupported system-time type"
                     (.getMessage error)))
        (is (= "java.lang.String"
               (:system-time-class (ex-data error))))))))

(deftest bind-request-frontier-binds-matching-biff-and-gesso-frontier-test
  (let [calls (atom 0)
        instant (Instant/parse "2026-09-10T18:34:00Z")
        expected-basis (xtdb-live/basis :xtdb 105 instant)
        expected-token (xtdb-live/basis-snapshot-token expected-basis)]
    (with-xtdb-stub
      '*status*
      (fn [_]
        (swap! calls inc)
        {:latest-completed-txs
         {"xtdb" [{:tx-id 105
                    :system-time instant}]}})
      (fn []
        (let [bound
              (xtdb-live/bind-request-frontier
               {:xtdb/node :authority-node
                :request/id :request-1})

              run-body
              ((:biff.core/wrap-db-snapshot bound)
               (fn [ctx]
                 {:request-id (:request/id ctx)
                  :snapshot-token (:biff.xtdb/snapshot-token ctx)}))]
          (is (= 1 @calls))
          (is (= :request-1 (:request/id bound)))
          (is (= expected-token
                 (:biff.xtdb/snapshot-token bound)))
          (is (= #{expected-basis}
                 (progression/required-bases
                  (:gesso.live/progression bound))))
          (is (= {:request-id :request-1
                  :snapshot-token expected-token}
                 (run-body {:request/id :request-1}))))))))

(deftest bind-request-frontier-preserves-existing-wrapper-and-independent-requirement-test
  (let [instant (Instant/parse "2026-09-10T18:35:00Z")
        analytics-time (Instant/parse "2026-09-10T17:00:00Z")
        frontier (xtdb-live/basis :xtdb 106 instant)
        analytics-basis (xtdb-live/basis :analytics 77 analytics-time)
        existing-requirement (progression/requirement analytics-basis)
        wrapper-calls (atom 0)
        existing-wrapper
        (fn [body]
          (swap! wrapper-calls inc)
          (fn [ctx]
            (body (assoc ctx :existing-wrapper-ran? true))))]
    (with-xtdb-stub
      '*status*
      (fn [_]
        {:latest-completed-txs
         {"xtdb" [{:tx-id 106
                    :system-time instant}]}})
      (fn []
        (let [bound
              (xtdb-live/bind-request-frontier
               {:xtdb/node :authority-node
                :gesso.live/progression existing-requirement
                :biff.core/wrap-db-snapshot existing-wrapper})

              run-body
              ((:biff.core/wrap-db-snapshot bound)
               (fn [ctx]
                 (select-keys
                  ctx
                  [:existing-wrapper-ran?
                   :biff.xtdb/snapshot-token])))]
          (is (= #{analytics-basis frontier}
                 (progression/required-bases
                  (:gesso.live/progression bound))))
          (is (= 1 @wrapper-calls))
          (is (= {:existing-wrapper-ran? true
                  :biff.xtdb/snapshot-token
                  (xtdb-live/basis-snapshot-token frontier)}
                 (run-body {}))))))))

(deftest bind-request-frontier-rejects-current-frontier-behind-existing-requirement-test
  (let [current-time (Instant/parse "2026-09-10T18:36:00Z")
        required-time (Instant/parse "2026-09-10T18:37:00Z")
        required-basis (xtdb-live/basis :xtdb 108 required-time)
        existing-requirement (progression/requirement required-basis)]
    (with-xtdb-stub
      '*status*
      (fn [_]
        {:latest-completed-txs
         {"xtdb" [{:tx-id 107
                    :system-time current-time}]}})
      (fn []
        (let [error
              (try
                (xtdb-live/bind-request-frontier
                 {:xtdb/node :authority-node
                  :gesso.live/progression existing-requirement})
                nil
                (catch clojure.lang.ExceptionInfo e
                  e))]
          (is (some? error))
          (is (re-find #"behind an existing authoritative progression requirement"
                       (.getMessage error)))
          (is (= required-basis (:required (ex-data error))))
          (is (= (xtdb-live/basis :xtdb 107 current-time)
                 (:frontier (ex-data error)))))))))

(deftest bind-request-frontier-does-not-fabricate-initial-basis-test
  (let [ctx {:xtdb/node :authority-node
             :request/id :request-2}
        calls (atom 0)]
    (with-xtdb-stub
      '*status*
      (fn [_]
        (swap! calls inc)
        {:latest-completed-txs {"xtdb" []}})
      (fn []
        (let [bound (xtdb-live/bind-request-frontier ctx)]
          (is (= 1 @calls))
          (is (identical? ctx bound))
          (is (nil? (:biff.xtdb/snapshot-token bound)))
          (is (nil? (:gesso.live/progression bound)))
          (is (nil? (:biff.core/wrap-db-snapshot bound))))))))

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
                              :snapshot-time t}
                :progression (progression/requirement
                              (xtdb-live/basis :xtdb 2 t))}
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
                              :snapshot-time t}
                :progression (progression/requirement
                              (xtdb-live/basis :xtdb 3 t))}
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
                                      :snapshot-time t}
                        :progression (progression/requirement
                                      (xtdb-live/basis :xtdb 10 t))}}
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
                              :snapshot-time t}
                :progression (progression/requirement
                              (xtdb-live/basis :xtdb 11 t))}
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
                              :snapshot-time t}
                :progression (progression/requirement
                              (xtdb-live/basis :xtdb 12 t))}
               (xtdb-live/put-doc-from!
                {:xtdb/connectable :connectable}
                :users
                {:xt/id "u1"}
                {:database :xtdb})))
        (is (= [[:connectable [[:put-docs :users {:xt/id "u1"}]] {:database :xtdb}]]
               @calls))))))
