(ns gesso.live.consistency.xtdb-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.consistency.xtdb :as xtdb-live]
   [gesso.live.progression :as progression]
   [xtdb.basis :as xt.basis])
  (:import
   [java.lang.reflect InvocationHandler Proxy]
   [java.time Instant ZonedDateTime]
   [java.util Base64]
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
           :biff.xtdb/connection-pool :biff-pool
           :biff.xtdb/node :biff-node})))

  (is (= :conn
         (xtdb-live/connectable-from
          {:xtdb/conn :conn
           :xtdb/node :node
           :biff.xtdb/connection-pool :biff-pool
           :biff.xtdb/node :biff-node})))

  (is (= :biff-pool
         (xtdb-live/connectable-from
          {:biff.xtdb/connection-pool :biff-pool
           :biff.xtdb/node :biff-node})))

  (is (= :raw
         (xtdb-live/connectable-from :raw))))

(deftest read-connectable-from-prefers-read-and-request-scoped-connectables-test
  (is (= :read
         (xtdb-live/read-connectable-from
          {:xtdb/read-connectable :read
           :xtdb/conn :conn
           :xtdb/connectable :connectable
           :biff.xtdb/connection-pool :biff-pool
           :xtdb/node :node
           :biff.xtdb/node :biff-node})))

  (is (= :conn
         (xtdb-live/read-connectable-from
          {:xtdb/conn :conn
           :xtdb/connectable :connectable
           :biff.xtdb/connection-pool :biff-pool
           :xtdb/node :node
           :biff.xtdb/node :biff-node})))

  (is (= :biff-pool
         (xtdb-live/read-connectable-from
          {:biff.xtdb/connection-pool :biff-pool
           :xtdb/node :node
           :biff.xtdb/node :biff-node}))))

(deftest require-connectable-throws-on-missing-connectable-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Missing XTDB2 connectable"
       (xtdb-live/require-connectable! nil))))

;; -----------------------------------------------------------------------------
;; Options and consistency maps
;; -----------------------------------------------------------------------------

(deftest malformed-explicit-xtdb-options-fail-before-io-test
  ;; false and [] used to fall through select-keys as empty maps. For a write,
  ;; silently dropping a caller-supplied database or transaction option can
  ;; change the authoritative operation that is actually submitted.
  (let [calls (atom 0)
        unexpected-api (fn [& _]
                         (swap! calls inc)
                         :unexpected)
        ctx {:xtdb/connectable :connection
             :xtdb/read-connectable :connection}]
    (with-redefs-fn
      {(xtdb-var '*q*) unexpected-api
       (xtdb-var '*plan-q*) unexpected-api
       (xtdb-var '*submit-tx*) unexpected-api
       (xtdb-var '*execute-tx*) unexpected-api}
      (fn []
        (doseq [invalid [false []]]
          (testing (str "Malformed XTDB options: " (pr-str invalid))
            (doseq [invoke [#(xtdb-live/query-opts invalid)
                            #(xtdb-live/tx-opts invalid)
                            #(xtdb-live/prepare-options! invalid)
                            #(xtdb-live/q-from ctx sample-query invalid)
                            #(xtdb-live/q-consistent-from ctx sample-query invalid)
                            #(xtdb-live/plan-q-consistent-from ctx sample-query invalid)
                            #(xtdb-live/submit-tx-from! ctx sample-tx invalid)
                            #(xtdb-live/execute-tx-from! ctx sample-tx invalid)]]
              (is (thrown-with-msg?
                   clojure.lang.ExceptionInfo
                   #"XTDB options must be a map or nil"
                   (invoke)))
              (is (zero? @calls)
                  "Invalid options must be rejected before any XTDB API call"))))))
    ;; nil is the intentionally absent option value; explicit maps still work.
    (is (= {} (xtdb-live/query-opts nil)))
    (is (= {} (xtdb-live/tx-opts nil)))
    (is (= {:debug-fn nil} (xtdb-live/prepare-options! nil)))
    (is (= {:database "xtdb"}
           (xtdb-live/query-opts {:database "xtdb"})))
    (is (= {:database "xtdb"}
           (xtdb-live/tx-opts {:database "xtdb"})))))

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

(deftest malformed-explicit-read-consistency-fails-before-xtdb-io-test
  ;; An explicitly supplied malformed read constraint is not absence. In
  ;; particular, false must not select a weaker legacy value and an empty
  ;; vector must not silently normalize into an unconstrained query.
  (doseq [invalid [false []]]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"XTDB consistency must be a map"
         (xtdb-live/normalize-consistency invalid))))
  (let [legacy {:snapshot-token "legacy"}
        contexts [{:xtdb/read-connectable :read
                   :gesso.live/consistency false
                   :gesso.live.xtdb/consistency legacy}
                  {:xtdb/read-connectable :read
                   :gesso.live.xtdb/consistency false
                   :xtdb/consistency legacy}
                  {:xtdb/read-connectable :read
                   :xtdb/consistency false
                   :consistency legacy}
                  {:xtdb/read-connectable :read
                   :consistency false}
                  {:xtdb/read-connectable :read
                   :gesso.live/consistency []
                   :xtdb/consistency legacy}]
        external-calls (atom 0)
        unexpected-api (fn [& _]
                         (swap! external-calls inc)
                         :unexpected)]
    (with-redefs-fn
      {(xtdb-var '*q*) unexpected-api
       (xtdb-var '*plan-q*) unexpected-api}
      (fn []
        (doseq [ctx contexts]
          (is (thrown-with-msg?
               clojure.lang.ExceptionInfo
               #"XTDB consistency must be a map"
               (xtdb-live/consistency-from ctx)))
          (doseq [invoke [#(xtdb-live/q-consistent-from ctx sample-query)
                          #(xtdb-live/plan-q-consistent-from ctx sample-query)]]
            (is (thrown-with-msg?
                 clojure.lang.ExceptionInfo
                 #"XTDB consistency must be a map"
                 (invoke)))
            (is (zero? @external-calls)
                "Malformed read consistency must not reach XTDB I/O")))))
    ;; nil is the only absent value; preserve fallback and optional defaults.
    (is (= legacy
           (xtdb-live/consistency-from
            {:gesso.live/consistency nil :xtdb/consistency legacy})))
    (is (= {} (xtdb-live/normalize-consistency nil)))
    (is (= {} (xtdb-live/consistency-from {:xtdb/node :node})))))

(deftest consistency-from-uses-only-explicit-consistency-test
  (is (= {:await-token "tok-1"}
         (xtdb-live/consistency-from
          {:xtdb/node :shared-node
           :biff.xtdb/node :shared-biff-node
           :consistency {:await-token "tok-1"}}))))

(deftest consistency-from-does-not-infer-from-node-state-test
  (is (= {}
         (xtdb-live/consistency-from
          {:xtdb/node :shared-node
           :biff.xtdb/node :shared-biff-node}))))

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

(deftest query-opts-rejects-ambiguous-database-identity-test
  ;; Database selection on reads must observe the same identity contract as
  ;; writes and portable progression bases. A namespace-qualified keyword
  ;; cannot silently alias the ordinary database named by its local part.
  (doseq [database [:tenant-a/shared :tenant-b/shared "" "   " 42]]
    (let [outcome (try
                    (xtdb-live/query-opts {:database database})
                    nil
                    (catch clojure.lang.ExceptionInfo e e))]
      (is (instance? clojure.lang.ExceptionInfo outcome)
          (str "Invalid read database must be rejected: " (pr-str database)))
      (when outcome
        (is (= database (:database (ex-data outcome)))))))
  (is (= {:database :shared}
         (xtdb-live/query-opts {:database :shared})))
  (is (= {:database "tenant-a/shared" :key-fn :kebab-case-keyword}
         (xtdb-live/query-opts {:database "tenant-a/shared"
                                :key-fn :kebab-case-keyword})))
  (is (= {} (xtdb-live/query-opts {:database nil}))))

(deftest invalid-read-database-cannot-reach-xtdb-query-api-test
  ;; Every read facade applies query-opts even when no progression is given.
  ;; An invalid database name must be caught before either public XTDB API
  ;; executes, not merely during authoritative progression comparison.
  (doseq [database [:tenant-a/shared :tenant-b/shared "" " " 42]
          api [:q :q-consistent :q-consistent-from
               :plan-q :plan-q-consistent :plan-q-consistent-from]]
    (let [calls (atom 0)
          opts {:database database}
          read-ctx {:xtdb/read-connectable :conn}
          read-op (fn [& _] (swap! calls inc) :read-result)
          run-read (case api
                     :q #(xtdb-live/q :conn sample-query opts)
                     :q-consistent #(xtdb-live/q-consistent :conn sample-query nil opts)
                     :q-consistent-from #(xtdb-live/q-consistent-from read-ctx sample-query opts)
                     :plan-q #(xtdb-live/plan-q :conn sample-query opts)
                     :plan-q-consistent #(xtdb-live/plan-q-consistent :conn sample-query nil opts)
                     :plan-q-consistent-from #(xtdb-live/plan-q-consistent-from read-ctx sample-query opts))]
      (with-redefs-fn
        {(xtdb-var '*q*) read-op
         (xtdb-var '*plan-q*) read-op}
        (fn []
          (let [outcome (try (run-read) nil
                             (catch clojure.lang.ExceptionInfo e e))]
            (is (instance? clojure.lang.ExceptionInfo outcome)
                (str "Invalid database must fail before " api ": " (pr-str database)))
            (when outcome
              (is (= database (:database (ex-data outcome)))))
            (is (zero? @calls)
                "An invalid database identity must never reach an XTDB read API"))))))
  ;; Valid names retain their exact representation through both query APIs.
  (let [calls (atom [])
        read-op (fn [_ _ opts] (swap! calls conj opts) :read-result)]
    (with-redefs-fn
      {(xtdb-var '*q*) read-op (xtdb-var '*plan-q*) read-op}
      (fn []
        (is (= :read-result
               (xtdb-live/q :conn sample-query {:database :shared})))
        (is (= :read-result
               (xtdb-live/plan-q-consistent :conn sample-query nil
                                            {:database "tenant-a/shared"})))
        (is (= [{:database :shared} {:database "tenant-a/shared"}]
               @calls))))))

(deftest tx-opts-rejects-ambiguous-database-identity-before-use-test
  ;; A transaction database is an authority identity, not a free-form hint.
  ;; A qualified keyword would lose its namespace when XTDB progression is
  ;; constructed after execute-tx, whereas submit-tx's ID-only receipt would
  ;; never validate the database at all. Both should reject it at admission.
  (doseq [database [:tenant-a/shared :tenant-b/shared "" "   " 42]]
    (let [error (try (xtdb-live/tx-opts {:database database})
                     nil
                     (catch clojure.lang.ExceptionInfo e e))]
      (is (instance? clojure.lang.ExceptionInfo error)
          (str "Reject unidentifiable database: " (pr-str database)))
      (when error
        (is (= database (:database (ex-data error)))))))
  ;; An unqualified keyword and a literal string with a slash remain distinct
  ;; and retain their original XTDB API representation.
  (is (= {:database :shared} (xtdb-live/tx-opts {:database :shared})))
  (is (= {:database "tenant-a/shared"}
         (xtdb-live/tx-opts {:database "tenant-a/shared"})))
  (is (= {} (xtdb-live/tx-opts {:database nil}))))

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

(deftest portable-xtdb-basis-rejects-blank-database-identities-test
  ;; Browser-carried bases must satisfy the same database naming invariant as
  ;; server-constructed bases. The XTDB snapshot-token encoder itself allows
  ;; an empty database string, and older admission accidentally trusted it.
  (let [t (Instant/parse "2026-01-01T00:00:00Z")
        valid (xtdb-live/basis :xtdb 19 t)]
    (doseq [database ["" " " "\t" " \n " "\r\n"]]
      (let [forged (assoc valid
                          :database database
                          :snapshot-token
                          (xt.basis/->time-basis-str {database [t]}))]
        (is (false? (xtdb-live/xtdb-basis? forged))
            (str "Reject empty/blank browser-carried database: " (pr-str database)))
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"closed portable XTDB"
             (xtdb-live/require-xtdb-basis! forged)))
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"closed portable XTDB"
             (xtdb-live/compare-bases forged forged)))
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"closed portable XTDB"
             (xtdb-live/strongest-required-basis
              (progression/requirement forged))))
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"non-blank string"
             (xtdb-live/basis database 19 t)))))
    (is (xtdb-live/xtdb-basis? valid))
    (is (= "xtdb" (xtdb-live/basis-database valid)))
    (is (xtdb-live/xtdb-basis? (xtdb-live/basis "tenant/a" 19 t)))))

(deftest blank-keyword-database-identities-rejected-at-admission-test
  ;; Clojure permits (keyword "") and whitespace-named keywords. Their
  ;; unqualified names must obey exactly the same nonblank contract as string
  ;; database names. Otherwise basis constructs malformed portable evidence,
  ;; and the read/write options admit databases closed-basis validation rejects.
  (let [t (Instant/parse "2026-01-01T00:00:00Z")]
    (doseq [value ["" " " "  " "\t" "\n "]
            :let [db (keyword value)]]
      (is (= value (name db)) "Control: keyword preserves the blank name")
      (doseq [[op f] [[:basis #(xtdb-live/basis db 17 t)]
                      [:query #(xtdb-live/query-opts {:database db})]
                      [:write #(xtdb-live/tx-opts {:database db})]]]
        (let [error (try (f) nil (catch clojure.lang.ExceptionInfo e e))]
          (is (instance? clojure.lang.ExceptionInfo error)
              (str "Reject blank keyword from " op ": " (pr-str db)))
          (when error
            (is (= db (:database (ex-data error))))))))
    ;; Ordinary keywords and explicit nonblank strings remain unchanged.
    (is (xtdb-live/xtdb-basis? (xtdb-live/basis :shared 17 t)))
    (is (= {:database :shared} (xtdb-live/query-opts {:database :shared})))
    (is (= {:database "tenant-a/shared"}
           (xtdb-live/tx-opts {:database "tenant-a/shared"})))))

(deftest blank-keyword-database-cannot-reach-xtdb-adapter-apis-test
  ;; A malformed database identifier must be rejected before status, reads,
  ;; writes or submissions. On the old path it was passed to XTDB unexamined;
  ;; execute-tx could even complete before progression projection failed.
  (let [t (Instant/parse "2026-01-01T00:00:00Z")]
    (doseq [db [(keyword "") (keyword "  ")]
            api [:q :plan-q :submit-tx :execute-tx :frontier]]
      (let [calls (atom 0)
            run (case api
                  :q #(xtdb-live/q :conn sample-query {:database db})
                  :plan-q #(xtdb-live/plan-q :conn sample-query {:database db})
                  :submit-tx #(xtdb-live/submit-tx! :conn sample-tx {:database db})
                  :execute-tx #(xtdb-live/execute-tx! :conn sample-tx {:database db})
                  :frontier #(xtdb-live/latest-completed-basis
                              {:xtdb/node :node} db))
            observed (fn [& _]
                       (swap! calls inc)
                       (if (= api :execute-tx)
                         (tx-key 45 t)
                         (if (= api :submit-tx)
                           {:tx-id 45}
                           {:latest-completed-txs {"xtdb" []}})))]
        (with-redefs-fn
          {(xtdb-var '*q*) observed
           (xtdb-var '*plan-q*) observed
           (xtdb-var '*submit-tx*) observed
           (xtdb-var '*execute-tx*) observed
           (xtdb-var '*status*) observed}
          (fn []
            (let [error (try (run) nil (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo error)
                  (str "Blank keyword must fail before " api))
              (when error
                (is (= db (:database (ex-data error)))))
              (is (zero? @calls)
                  (str "Invalid database passed through to XTDB " api)))))))))

(deftest database-identity-rejects-qualified-keyword-aliases-test
  ;; Qualified keywords are normal Clojure data, but `name` drops their
  ;; namespace. The two tenant-scoped identifiers would otherwise construct
  ;; indistinguishable authoritative XTDB bases for database "shared".
  (let [time (Instant/parse "2026-01-01T00:00:00Z")
        plain (xtdb-live/basis :shared 17 time)]
    (doseq [database [:tenant-a/shared :tenant-b/shared]]
      (let [caught (try (xtdb-live/basis database 17 time)
                        nil
                        (catch clojure.lang.ExceptionInfo error error))]
        (is (instance? clojure.lang.ExceptionInfo caught)
            "Qualified database keyword must not alias its local name")
        (when caught
          (is (= database (:database (ex-data caught))))
          (is (re-find #"unqualified keyword or exact string"
                       (.getMessage caught))))))
    (is (= "shared" (xtdb-live/basis-database plain)))
    (is (= plain (xtdb-live/basis "shared" 17 time)))
    ;; Exact string identity remains available for any XTDB database name.
    (is (= "tenant-a/shared"
           (xtdb-live/basis-database
            (xtdb-live/basis "tenant-a/shared" 17 time))))))

(deftest query-progression-rejects-qualified-database-identity-test
  ;; Caller query options may never pretend that a requirement for :shared
  ;; matches :tenant-a/shared by silently dropping the qualifier.
  (let [required (xtdb-live/basis :shared 18
                                   (Instant/parse "2026-01-01T00:00:00Z"))
        requirement (progression/requirement required)]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"unqualified keyword or exact string"
         (xtdb-live/read-query-opts nil requirement
                                    {:database :tenant-a/shared})))
    (is (= :shared
           (:database
            (xtdb-live/read-query-opts nil requirement
                                       {:database :shared}))))))

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

(defn- snapshot-token-with-unknown-field
  "Append an ignorable protobuf varint field to an XTDB snapshot token.

   The XTDB decoder accepts unknown protobuf fields, so this creates a
   different string that decodes to exactly the same database/time basis."
  [snapshot-token field-number]
  (let [raw (.decode (Base64/getDecoder) snapshot-token)
        extended (byte-array (+ (alength raw) 2))]
    (System/arraycopy raw 0 extended 0 (alength raw))
    (aset-byte extended (alength raw) (byte (* 8 field-number)))
    (aset-byte extended (inc (alength raw)) (byte 1))
    (.encodeToString (Base64/getEncoder) extended)))

(deftest portable-xtdb-basis-rejects-noncanonical-snapshot-tokens-test
  ;; An XTDB protobuf parser can ignore extra fields. A token with an extra
  ;; field therefore represents the same authoritative time as the canonical
  ;; token but has different opaque characters. Gesso must reject, rather than
  ;; admit two distinct closed bases and later report conflicting evidence.
  (let [time (Instant/parse "2026-01-01T00:00:00.123456789Z")
        canonical (xtdb-live/basis :analytics 9 time)
        original-token (:snapshot-token canonical)
        later (xtdb-live/basis :analytics 10 (.plusSeconds time 1))]
    (doseq [unknown-field [3 4 15]]
      (let [alternate-token (snapshot-token-with-unknown-field
                             original-token unknown-field)
            alternate (assoc canonical :snapshot-token alternate-token)]
        (is (not= original-token alternate-token))
        (is (= (xt.basis/<-time-basis-str original-token)
               (xt.basis/<-time-basis-str alternate-token))
            "The alternate protobuf spelling must denote the same time basis")
        (is (false? (xtdb-live/xtdb-basis? alternate))
            "Only canonical snapshot-token strings may become closed evidence")
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"closed portable XTDB"
             (xtdb-live/require-xtdb-basis! alternate)))
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"closed portable XTDB"
             (xtdb-live/compare-bases canonical alternate))
            "Comparison must reject malformed evidence before reporting conflict")
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"closed portable XTDB"
             (xtdb-live/compare-bases alternate later))
            "Ordering must not trust alternate authoritative encodings")))
    (is (xtdb-live/xtdb-basis? canonical))
    (is (= time (xtdb-live/basis-system-time canonical)))
    (is (= original-token
           (xt.basis/->time-basis-str
            (xt.basis/<-time-basis-str original-token)))
        "The native XTDB encoder remains canonical and round-trippable")))

(deftest portable-xtdb-basis-rejects-noncanonical-decimal-identities-test
  ;; Browser-carried bases are compared as closed immutable data, not merely
  ;; parsed longs. Accepting "09" as the same id as "9" introduces a second
  ;; representation of one authoritative coordinate. Previously such a basis
  ;; passed admission and could even be ordered against newer valid bases;
  ;; comparison with its canonical twin then falsely reported a conflict.
  (let [t (Instant/parse "2026-01-01T00:00:00Z")
        canonical (xtdb-live/basis :xtdb 9 t)
        zero (xtdb-live/basis :xtdb 0 t)
        later (xtdb-live/basis :xtdb 10 t)]
    (doseq [[original wire] [[canonical "09"]
                             [canonical "0009"]
                             [canonical "+9"]
                             [canonical "+09"]
                             [zero "00"]
                             [zero "-0"]]]
      (let [forged (assoc original :tx-id wire)]
        (is (false? (xtdb-live/xtdb-basis? forged))
            (str "Noncanonical tx-id must not be admitted: " (pr-str wire)))
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"closed portable XTDB"
             (xtdb-live/require-xtdb-basis! forged))
            (str "Noncanonical tx-id must fail closed basis admission: "
                 (pr-str wire)))
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"closed portable XTDB"
             (xtdb-live/compare-bases forged later))
            "Ordering must reject invalid evidence rather than trust its id")))
    (is (xtdb-live/xtdb-basis? canonical))
    (is (xtdb-live/xtdb-basis? zero))
    (is (= "9" (:tx-id canonical)))
    (is (= "0" (:tx-id zero)))
    (is (= 9 (xtdb-live/basis-tx-id canonical)))
    (is (= 0 (xtdb-live/basis-tx-id zero)))))

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


(deftest progression-from-rejects-false-canonical-requirement-test
  ;; Only nil denotes an absent progression requirement. A false canonical
  ;; value must not be replaced by the legacy convenience key or silently
  ;; become no requirement: either path could weaken authoritative reads.
  (let [required-basis (xtdb-live/basis 72
                                        (Instant/parse "2026-11-20T00:00:00Z"))
        legacy (progression/requirement required-basis)
        invalid-ctx {:gesso.live/progression false
                     :progression legacy
                     :xtdb/read-connectable :request-conn
                     :xtdb/node :authority-node}]
    (doseq [ctx [(dissoc invalid-ctx :progression)
                 invalid-ctx]]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Expected a normalized non-empty Gesso Live progression requirement"
           (xtdb-live/progression-from ctx))))
    ;; An explicitly nil canonical requirement retains the existing migration
    ;; behavior: a well-formed legacy convenience requirement is used.
    (is (= legacy
           (xtdb-live/progression-from
            {:gesso.live/progression nil
             :progression legacy})))
    (let [external-calls (atom 0)
          unexpected-api (fn [& _]
                           (swap! external-calls inc)
                           :unexpected)]
      (with-redefs-fn
        {(xtdb-var '*q*) unexpected-api
         (xtdb-var '*plan-q*) unexpected-api
         (xtdb-var '*status*) unexpected-api}
        (fn []
          (doseq [invoke [#(xtdb-live/q-consistent-from invalid-ctx sample-query)
                          #(xtdb-live/plan-q-consistent-from invalid-ctx sample-query)
                          #(xtdb-live/bind-request-frontier invalid-ctx)]]
            (is (thrown-with-msg?
                 clojure.lang.ExceptionInfo
                 #"Expected a normalized non-empty Gesso Live progression requirement"
                 (invoke)))
            (is (zero? @external-calls)
                "Malformed canonical progression must fail before XTDB I/O")))))))

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
             :biff.xtdb/connection-pool :request-pool})))
    (is (nil?
         (xtdb-live/node-from
          {:xtdb/conn :request-conn
           :xtdb/connectable :shared-connectable
           :biff.xtdb/connection-pool :biff-request-pool})))
    (is (nil?
         (xtdb-live/node-from
          {:biff/node :preview-era-node
           :biff/conn :preview-era-conn})))
    (is (nil? (xtdb-live/node-from raw-data-source)))))

(deftest latest-completed-basis-cannot-alias-qualified-database-test
  ;; The status boundary must reject ambiguous database identities before
  ;; querying an unrelated XTDB database with the same local name.
  (let [observations (atom 0)]
    (with-xtdb-stub
      '*status*
      (fn [_]
        (swap! observations inc)
        {:latest-completed-txs
         {"shared" [{:tx-id 19
                     :system-time (Instant/parse "2026-01-01T00:00:00Z")}]}})
      (fn []
        (let [error (try
                      (xtdb-live/latest-completed-basis
                       {:xtdb/node :authority-node} :tenant-a/shared)
                      nil
                      (catch clojure.lang.ExceptionInfo error error))]
          (is (instance? clojure.lang.ExceptionInfo error))
          (when error
            (is (= :tenant-a/shared (:database (ex-data error)))))
          (is (zero? @observations)
              "Do not inspect a different database's authoritative status"))))))

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

(deftest latest-completed-basis-rejects-unobservable-status-shapes-test
  ;; A successful XTDB status observation is not interchangeable with an
  ;; absent or malformed status payload. In particular, nil for a selected
  ;; database is NOT the same as its documented empty completed-tx list.
  ;; Otherwise a missing status observation can silently disable Biff's
  ;; authoritative read frontier.
  (doseq [status [nil
                  {}
                  {:latest-completed-txs nil}
                  {:latest-completed-txs []}
                  {:latest-completed-txs {"xtdb" nil}}
                  {:latest-completed-txs {"xtdb" [nil]}}]]
    (let [status-calls (atom 0)]
      (with-xtdb-stub
        '*status*
        (fn [_] (swap! status-calls inc) status)
        (fn []
          (let [outcome (try
                          (xtdb-live/latest-completed-basis
                           {:xtdb/node :authority-node})
                          nil
                          (catch clojure.lang.ExceptionInfo e e))]
            (is (instance? clojure.lang.ExceptionInfo outcome)
                (str "Malformed status must fail closed: " (pr-str status)))
            (when outcome
              (is (= "xtdb" (:database (ex-data outcome))))
              (is (= status (:status (ex-data outcome)))))
            (is (= 1 @status-calls))))))))

(deftest latest-completed-basis-rejects-malformed-status-database-keys-test
  ;; XTDB status uses exact nonblank string database identities. If an
  ;; unexpected keyword or blank key contains completed transactions, ordinary
  ;; string lookup appears empty and could leave Biff reads unpinned. This is
  ;; malformed STATUS, not evidence of a genuinely empty selected database.
  (let [t (Instant/parse "2026-09-10T18:30:00Z")
        completed [{:tx-id 104 :system-time t}]
        ctx {:xtdb/node :authority-node}]
    (doseq [by-database [{:xtdb completed}
                         {" " completed}
                         {nil completed}
                         {"xtdb" [] :analytics completed}
                         {"xtdb" [] "  " completed}]
            :let [status {:latest-completed-txs by-database}]]
      (with-xtdb-stub
        '*status*
        (fn [_] status)
        (fn []
          (doseq [invoke [#(xtdb-live/latest-completed-basis ctx)
                          #(xtdb-live/bind-request-frontier ctx)]]
            (let [outcome (try (invoke)
                               nil
                               (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo outcome)
                  (str "Malformed database keys must fail closed: "
                       (pr-str by-database)))
              (when (instance? clojure.lang.ExceptionInfo outcome)
                (is (= "xtdb" (:database (ex-data outcome))))
                (is (= status (:status (ex-data outcome))))))))))
    ;; A valid map with no completed transaction for the selected database
    ;; remains a legitimate empty-database startup, even if another database
    ;; has a completed transaction.
    (with-xtdb-stub
      '*status*
      (fn [_] {:latest-completed-txs {"analytics" completed}})
      (fn []
        (is (nil? (xtdb-live/latest-completed-basis ctx)))
        (is (identical? ctx (xtdb-live/bind-request-frontier ctx)))))))

(deftest bind-request-frontier-never-mistakes-missing-status-for-empty-database-test
  ;; The invalid observation is dangerous even with no preexisting browser
  ;; minimum: an unchanged context permits ordinary Biff reads without a
  ;; server-established snapshot. A truly empty per-database vector is okay.
  (let [ctx {:xtdb/node :authority-node :request/id :fresh-request}
        status-calls (atom 0)]
    (with-xtdb-stub
      '*status*
      (fn [_] (swap! status-calls inc) nil)
      (fn []
        (let [outcome (try (xtdb-live/bind-request-frontier ctx)
                           nil
                           (catch clojure.lang.ExceptionInfo e e))]
          (is (instance? clojure.lang.ExceptionInfo outcome))
          (when outcome
            (is (= "xtdb" (:database (ex-data outcome)))))
          (is (= 1 @status-calls)))))
    (with-xtdb-stub
      '*status*
      (fn [_] {:latest-completed-txs {"xtdb" []}})
      (fn []
        (is (identical? ctx (xtdb-live/bind-request-frontier ctx))
            "Legitimate empty-database startup must remain supported")))))

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
              {:biff.xtdb/node :authority-node})))
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

(deftest bind-request-frontier-rejects-malformed-claimed-xtdb-evidence-test
  ;; Generic progression intentionally treats bases as opaque. XTDB is the
  ;; authority-specific admission boundary and must reject a malformed basis
  ;; tagged with its own type, rather than ignore it as "unrelated" evidence.
  ;; Previously both branches silently skipped all of these forged bases.
  (let [now (Instant/parse "2026-09-10T18:44:00Z")
        valid (xtdb-live/basis :xtdb 22 now)
        bad-bases [(assoc valid :database "")
                   (assoc valid :tx-id "022")
                   (assoc valid :gesso.live.consistency.xtdb/version 99)
                   (assoc valid :snapshot-token "garbage")
                   (dissoc valid :snapshot-token)]]
    (doseq [completed? [true false]
            bad bad-bases]
      (let [status-calls (atom 0)
            ctx {:xtdb/node :authority-node
                 :gesso.live/progression (progression/requirement bad)}]
        (is (false? (xtdb-live/xtdb-basis? bad)))
        (with-xtdb-stub
          '*status*
          (fn [_]
            (swap! status-calls inc)
            {:latest-completed-txs
             {"xtdb" (if completed?
                       [{:tx-id 23 :system-time (.plusSeconds now 1)}]
                       [])}})
          (fn []
            (let [error (try
                          (xtdb-live/bind-request-frontier ctx)
                          nil
                          (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo error)
                  "An XTDB-typed malformed requirement must fail closed")
              (when error
                (is (re-find #"closed portable XTDB"
                             (.getMessage error)))
                (is (= bad (:basis (ex-data error)))))
              (is (zero? @status-calls)
                  "Reject malformed claimed evidence before status observation"))))))))

(deftest bind-request-frontier-preserves-foreign-opaque-basis-test
  ;; This adapter must not reject or reinterpret genuinely non-XTDB opaque
  ;; requirements, even if no XTDB transaction has completed yet.
  (let [foreign {:authority :external-service :epoch "external-v4"}
        ctx {:xtdb/node :authority-node
             :gesso.live/progression (progression/requirement foreign)}
        now (Instant/parse "2026-09-10T18:44:01Z")]
    (doseq [completed? [true false]]
      (with-xtdb-stub
        '*status*
        (fn [_]
          {:latest-completed-txs
           {"xtdb" (if completed?
                     [{:tx-id 23 :system-time now}]
                     [])}})
        (fn []
          (let [bound (xtdb-live/bind-request-frontier ctx)]
            (if completed?
              (is (= #{foreign (xtdb-live/basis :xtdb 23 now)}
                     (progression/required-bases
                      (:gesso.live/progression bound))))
              (is (identical? ctx bound)))))))))

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

(deftest bind-request-frontier-rejects-unobserved-required-transaction-test
  ;; If this XTDB node has no completed transaction, it cannot bind a Biff
  ;; snapshot satisfying an existing browser requirement for that database.
  ;; Returning ctx unchanged would allow ordinary Biff reads to proceed
  ;; without any snapshot token despite the established minimum frontier.
  (let [required (xtdb-live/basis :xtdb 209
                                  (Instant/parse "2026-09-10T18:38:00Z"))
        calls (atom 0)
        ctx {:xtdb/node :authority-node
             :gesso.live/progression (progression/requirement required)}]
    (with-xtdb-stub
      '*status*
      (fn [_]
        (swap! calls inc)
        {:latest-completed-txs {"xtdb" []}})
      (fn []
        (let [result (try (xtdb-live/bind-request-frontier ctx)
                          (catch clojure.lang.ExceptionInfo e e))]
          (is (instance? clojure.lang.ExceptionInfo result)
              "A node with no completed transaction cannot satisfy a known requirement")
          (when (instance? clojure.lang.ExceptionInfo result)
            (is (re-find #"cannot satisfy.*existing.*progression"
                         (.getMessage result)))
            (is (= "xtdb" (:database (ex-data result))))
            (is (= required (:required (ex-data result)))))
          (is (= 1 @calls)))))))

(deftest bind-request-frontier-rejects-unobserved-selected-database-test
  ;; A request-local legacy :progression and a nondefault requested database
  ;; carry exactly the same freshness requirement. No completed transaction
  ;; for that selected database cannot be mistaken for no constraint.
  (let [required (xtdb-live/basis :analytics 17
                                  (Instant/parse "2026-09-10T18:38:01Z"))
        ctx {:xtdb/node :authority-node
             :progression (progression/requirement required)}]
    (with-xtdb-stub
      '*status*
      (fn [_] {:latest-completed-txs {"analytics" []
                                         "xtdb" [{:tx-id 900
                                                  :system-time
                                                  (Instant/parse "2026-09-10T18:40:00Z")}]}})
      (fn []
        (let [result (try (xtdb-live/bind-request-frontier ctx :analytics)
                          (catch clojure.lang.ExceptionInfo e e))]
          (is (instance? clojure.lang.ExceptionInfo result))
          (when (instance? clojure.lang.ExceptionInfo result)
            (is (= "analytics" (:database (ex-data result))))
            (is (= required (:required (ex-data result))))))))))

(deftest bind-request-frontier-empty-selected-database-preserves-other-requirements-test
  ;; A frontier for a different database is not evidence that the selected
  ;; database contains completed transactions. Preserve it without making up
  ;; an initial selected-database basis or rejecting unrelated constraints.
  (let [required (xtdb-live/basis :analytics 17
                                  (Instant/parse "2026-09-10T18:38:02Z"))
        ctx {:xtdb/node :authority-node
             :request/id :untouched
             :gesso.live/progression (progression/requirement required)}]
    (with-xtdb-stub
      '*status*
      (fn [_] {:latest-completed-txs {"xtdb" []}})
      (fn []
        (is (identical? ctx (xtdb-live/bind-request-frontier ctx)))
        (is (= (progression/requirement required)
               (:gesso.live/progression ctx)))))))

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

(deftest invalid-database-identity-cannot-reach-xtdb-write-test
  ;; Rejection must precede BOTH public XTDB APIs. Previously execute-tx!
  ;; failed only after a successful returned transaction key, and submit-tx!
  ;; could report success because its ID-only receipt has no progression basis.
  (let [system-time (Instant/parse "2026-01-01T00:00:00Z")]
    (doseq [database [:tenant-a/shared :tenant-b/shared "" "  " 42]
            api [:execute :submit]]
      (let [writes (atom 0)
            seam (if (= :execute api) '*execute-tx* '*submit-tx*)
            replacement (fn [& _]
                          (swap! writes inc)
                          (if (= :execute api)
                            (tx-key 312 system-time)
                            {:tx-id 312}))]
        (with-xtdb-stub
          seam replacement
          (fn []
            (let [outcome (try
                            ((if (= :execute api)
                               xtdb-live/execute-tx!
                               xtdb-live/submit-tx!)
                             :conn sample-tx {:database database})
                            nil
                            (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo outcome)
                  (str "Reject invalid " api " database: " (pr-str database)))
              (when outcome
                (is (= database (:database (ex-data outcome)))
                    "Preserve the exact invalid database identity")
                (is (nil? (:commit/status (ex-data outcome)))
                    "A preflight failure is not a post-commit failure")
                (is (nil? (:submission/status (ex-data outcome)))
                    "A preflight failure is not an accepted submission"))
              (is (zero? @writes)
                  "An invalid database name must never reach XTDB")))))))
  ;; Valid keyword and explicit slash-containing string names still reach XTDB
  ;; and produce a correctly attributed authoritative basis on execute.
  (let [system-time (Instant/parse "2026-01-01T00:00:00Z")
        calls (atom [])]
    (with-xtdb-stub
      '*execute-tx*
      (fn [_ _ opts]
        (swap! calls conj opts)
        (tx-key 313 system-time))
      (fn []
        (let [simple (xtdb-live/execute-tx!
                      :conn sample-tx {:database :shared})
              literal (xtdb-live/execute-tx!
                       :conn sample-tx {:database "tenant-a/shared"})]
          (is (= [{:database :shared} {:database "tenant-a/shared"}]
                 @calls))
          (is (= "shared"
                 (xtdb-live/basis-database
                  (first (progression/required-bases
                          (:progression simple))))))
          (is (= "tenant-a/shared"
                 (xtdb-live/basis-database
                  (first (progression/required-bases
                          (:progression literal)))))))))))

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

(deftest execute-tx-requires-complete-committed-time-basis-test
  ;; execute-tx is a completed operation and the adapter promises a complete
  ;; read-after-write basis. A tx-id-only result or non-Instant system time
  ;; must not become a successful response without any :progression.
  ;; Unlike submit-tx, execute-tx must expose a usable system-time.
  (let [bad-results [{:tx-id 301}
                     {:tx-id 302 :system-time nil}
                     (tx-key 303 nil)
                     {:tx-id 304 :system-time "wrong type"}]]
    (doseq [tx-result bad-results]
      (let [calls (atom 0)
            events (atom [])]
        (with-xtdb-stub
          '*execute-tx*
          (fn [& _] (swap! calls inc) tx-result)
          (fn []
            (let [outcome (try
                            (xtdb-live/execute-tx!
                             :conn sample-tx
                             {:debug-fn #(swap! events conj (:event %))})
                            (catch Throwable e e))]
              (is (xtdb-live/post-execute-result-failure? outcome)
                  "A completed operation without a time basis cannot report success")
              (is (= :committed (:commit/status (ex-data outcome)))
                  "A returned identified execution must never be retried")
              (is (= :post-execute-result (:failure/stage (ex-data outcome))))
              (is (identical? tx-result (:tx-result (ex-data outcome))))
              (is (and (instance? Throwable outcome)
                       (some? (.getCause ^Throwable outcome)))
                  "The missing result coordinate must remain diagnosable")
              (is (= [:gesso.live.xtdb/execute-tx-started
                      :gesso.live.xtdb/execute-tx-result-failed]
                     @events)
                  "No false successful-execution event")
              (is (= 1 @calls)))))))
    ;; A tx-id-only public submit-tx receipt remains legitimate.
    (with-xtdb-stub
      '*submit-tx*
      (fn [& _] {:tx-id 305})
      (fn []
        (is (= {:tx-result {:tx-id 305} :consistency {:tx-id 305}}
               (xtdb-live/submit-tx! :conn sample-tx)))))))

(deftest execute-tx-invalid-return-never-proves-commit-test
  ;; Returning from the API is not by itself a reliable success receipt when
  ;; the result lacks an identifiable transaction. Avoid claiming a committed
  ;; write or publishing a success diagnostic from malformed adapter results.
  (doseq [receipt [nil {} {:tx-id nil} {:tx-id "7"} {:tx-id -1}
                   {:tx-id 2.5} {:tx-id 9223372036854775808N}]]
    (let [calls (atom 0)
          events (atom [])]
      (with-xtdb-stub
        '*execute-tx*
        (fn [& _] (swap! calls inc) receipt)
        (fn []
          (let [error (try
                        (xtdb-live/execute-tx!
                         :conn sample-tx
                         {:debug-fn #(swap! events conj (:event %))})
                        nil
                        (catch Throwable e e))]
            (is (xtdb-live/post-execute-result-failure? error)
                (str "Malformed execute receipt must not report success: "
                     (pr-str receipt)))
            (is (= :unknown (:commit/status (ex-data error)))
                "Cannot assert a committed write without a usable tx-id")
            (is (= :post-execute-result (:failure/stage (ex-data error))))
            (is (= receipt (:tx-result (ex-data error))))
            (is (= [:gesso.live.xtdb/execute-tx-started
                    :gesso.live.xtdb/execute-tx-result-failed]
                   @events)
                "Malformed receipt is a result failure, not a successful commit")
            (is (= 1 @calls))))))))

(deftest submit-tx-invalid-return-never-proves-submission-test
  ;; A syntactically unusable receipt does not establish even that an async
  ;; submission was accepted. Its outcome is unknown, not submitted/committed.
  (doseq [receipt [nil {} {:tx-id nil} {:tx-id "8"} {:tx-id -1}
                   {:tx-id 2.5} {:tx-id 9223372036854775808N}]]
    (let [calls (atom 0)
          events (atom [])]
      (with-xtdb-stub
        '*submit-tx*
        (fn [& _] (swap! calls inc) receipt)
        (fn []
          (let [error (try
                        (xtdb-live/submit-tx!
                         :conn sample-tx
                         {:debug-fn #(swap! events conj (:event %))})
                        nil
                        (catch Throwable e e))]
            (is (xtdb-live/post-submit-result-failure? error)
                (str "Malformed submit receipt must not report success: "
                     (pr-str receipt)))
            (is (= :unknown (:submission/status (ex-data error)))
                "Cannot confirm acceptance without a usable tx-id")
            (is (nil? (:commit/status (ex-data error)))
                "Submission outcome is not commit evidence")
            (is (= :post-submit-result (:failure/stage (ex-data error))))
            (is (= receipt (:tx-result (ex-data error))))
            (is (= [:gesso.live.xtdb/submit-tx-started
                    :gesso.live.xtdb/submit-tx-result-failed]
                   @events))
            (is (= 1 @calls))))))))

(deftest submit-tx-result-failure-is-classified-as-submitted-test
  ;; A returned submit-tx receipt proves acceptance, not execution/commit.
  ;; A later failure deriving Gesso metadata must not claim that the API
  ;; rejected the submission or that an authoritative commit was confirmed.
  (let [receipt {:tx-id 72}
        cause (ex-info "projection failed"
                       {:xtdb.error/code :xtdb/assert-failed})
        calls (atom 0)
        events (atom [])]
    (with-redefs-fn
      {(xtdb-var '*submit-tx*)
       (fn [& _] (swap! calls inc) receipt)
       (xtdb-var 'tx-result-progression)
       (fn [& _] (throw cause))}
      (fn []
        (let [captured
              (xtdb-live/capture-tx-api-calls!
               #(xtdb-live/submit-tx!
                 :conn sample-tx
                 {:debug-fn (fn [data] (swap! events conj (:event data)))}))
              error (:error captured)
              classified? (if-let [predicate
                                   (ns-resolve 'gesso.live.consistency.xtdb
                                               'post-submit-result-failure?)]
                            (predicate error)
                            false)]
          (is classified?
              "A returned submit receipt must be distinguished from rejection")
          (is (= :submitted (:submission/status (ex-data error))))
          (is (= :post-submit-result (:failure/stage (ex-data error))))
          (is (nil? (:commit/status (ex-data error)))
              "Submission is not proof of a committed XTDB transaction")
          (is (identical? receipt (:tx-result (ex-data error))))
          (is (identical? cause (.getCause ^Throwable error)))
          (is (= 1 @calls) "A receipt must never be silently discarded")
          (is (= [:invoked :returned] (mapv :phase (:events captured)))
              "Capture must prove that the raw XTDB API returned normally")
          (is (= [:gesso.live.xtdb/submit-tx-started
                  :gesso.live.xtdb/submit-tx-result-failed]
                 @events)
              "A metadata failure must not be reported as XTDB rejection"))))))

(deftest submit-tx-consistency-derivation-failure-is-submitted-test
  ;; Consistency extraction, not just optional progression, happens after the
  ;; XTDB API has returned and must preserve the same submission boundary.
  (let [receipt {:tx-id 73}
        cause (ex-info "consistency extraction failed" {:reason :bad-result})
        calls (atom 0)]
    (with-redefs-fn
      {(xtdb-var '*submit-tx*) (fn [& _] (swap! calls inc) receipt)
       (xtdb-var 'tx-consistency) (fn [& _] (throw cause))}
      (fn []
        (let [error (try
                      (xtdb-live/submit-tx! :conn sample-tx)
                      (catch Throwable e e))]
          (is (= :submitted (:submission/status (ex-data error))))
          (is (= :post-submit-result (:failure/stage (ex-data error))))
          (is (nil? (:commit/status (ex-data error))))
          (is (identical? receipt (:tx-result (ex-data error))))
          (is (identical? cause (.getCause ^Throwable error)))
          (is (= 1 @calls)))))))

(deftest actual-submit-tx-error-remains-a-genuine-submission-error-test
  ;; A genuine failure raised by the API itself still propagates unchanged;
  ;; a missing receipt cannot be claimed accepted by the adapter.
  (let [real-error (ex-info "XTDB submit failed" {:reason :storage})
        calls (atom 0)
        events (atom [])]
    (with-xtdb-stub
      '*submit-tx*
      (fn [& _] (swap! calls inc) (throw real-error))
      (fn []
        (let [caught (try
                       (xtdb-live/submit-tx!
                        :conn sample-tx
                        {:debug-fn (fn [event]
                                     (swap! events conj (:event event)))})
                       (catch Throwable e e))]
          (is (identical? real-error caught))
          (is (nil? (:submission/status (ex-data caught))))
          (is (= 1 @calls))
          (is (= [:gesso.live.xtdb/submit-tx-started
                  :gesso.live.xtdb/submit-tx-failed] @events)))))))

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

(deftest execute-tx-classifies-failed-result-derivation-after-commit-test
  ;; The bound XTDB API has returned a transaction key, so a failure while
  ;; converting it must not look like the underlying transaction aborted.
  (let [commits (atom 0)
        events (atom [])
        tx-result (tx-key 52 (Instant/parse "2026-01-01T00:00:00Z"))
        cause (ex-info "result projection failed"
                       {:xtdb.error/code :xtdb/assert-failed})]
    (with-redefs-fn
      {(xtdb-var '*execute-tx*)
       (fn [_connectable _ops _opts]
         (swap! commits inc)
         tx-result)
       (xtdb-var 'tx-result-progression)
       (fn [& _] (throw cause))}
      (fn []
        (let [error (try
                      (xtdb-live/execute-tx! :conn sample-tx
                                             {:database :xtdb
                                              :debug-fn #(swap! events conj (:event %))})
                      nil
                      (catch Throwable e e))]
          (is (if-let [predicate (ns-resolve 'gesso.live.consistency.xtdb
                                            'post-execute-result-failure?)]
                (predicate error)
                false)
              "Post-result derivation errors must be classified as committed")
          (is (= :committed (:commit/status (ex-data error))))
          (is (= :post-execute-result (:failure/stage (ex-data error))))
          (is (identical? tx-result (:tx-result (ex-data error))))
          (is (identical? cause (.getCause ^Throwable error)))
          (is (= 1 @commits) "The authoritative call must occur exactly once")
          (is (= [:gesso.live.xtdb/execute-tx-started
                  :gesso.live.xtdb/execute-tx-result-failed]
                 @events)))))))

(deftest execute-tx-actual-xtdb-failure-is-not-classified-as-committed-test
  (let [error (ex-info "real ASSERT failure"
                       {:xtdb.error/code :xtdb/assert-failed})]
    (with-xtdb-stub
      '*execute-tx*
      (fn [& _] (throw error))
      (fn []
        (let [caught (try (xtdb-live/execute-tx! :conn sample-tx)
                          (catch Throwable e e))]
          (is (identical? error caught))
          (is (false? (if-let [predicate (ns-resolve 'gesso.live.consistency.xtdb
                                            'post-execute-result-failure?)]
                        (predicate caught)
                        false))))))))

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

;; -----------------------------------------------------------------------------
;; XTDB diagnostics must not change authoritative write outcomes
;; -----------------------------------------------------------------------------

(deftest assertion-in-execute-success-trace-preserves-completed-result-test
  ;; A successful execute-tx has already completed authoritatively when the
  ;; successful-result debug event is observed. Failing instrumentation cannot
  ;; turn that completed mutation into an apparent execution failure.
  (let [calls (atom 0)
        observed (atom [])
        t (Instant/parse "2026-01-01T00:00:00Z")
        committed (tx-key 220 t)
        diagnostic (AssertionError. "post-execution trace failed")]
    (with-xtdb-stub
      '*execute-tx*
      (fn [& _] (swap! calls inc) committed)
      (fn []
        (let [outcome (try
                        (xtdb-live/execute-tx!
                         :conn sample-tx
                         {:debug-fn (fn [{:keys [event]}]
                                      (swap! observed conj event)
                                      (when (= event :gesso.live.xtdb/execute-tx-succeeded)
                                        (throw diagnostic)))})
                        (catch Throwable error error))]
          (is (map? outcome) "A completed write must return normally")
          (is (and (map? outcome)
                   (identical? committed (:tx-result outcome))))
          (is (= 1 @calls))
          (is (= [:gesso.live.xtdb/execute-tx-started
                  :gesso.live.xtdb/execute-tx-succeeded]
                 @observed)
              "No spurious execute-tx-failed event after successful execution"))))))

(deftest assertion-in-execute-error-trace-preserves-original-error-test
  ;; Reporting an XTDB API error is observational. An AssertionError in its
  ;; diagnostic hook must not replace the authoritative API error.
  (let [original (ex-info "XTDB really failed" {:xtdb.error/code :xtdb/assert-failed})
        diagnostic (AssertionError. "failure trace failed")
        observed (atom [])
        calls (atom 0)]
    (with-xtdb-stub
      '*execute-tx*
      (fn [& _] (swap! calls inc) (throw original))
      (fn []
        (let [caught (try
                       (xtdb-live/execute-tx!
                        :conn sample-tx
                        {:debug-fn (fn [{:keys [event]}]
                                     (swap! observed conj event)
                                     (when (= event :gesso.live.xtdb/execute-tx-failed)
                                       (throw diagnostic)))})
                       nil
                       (catch Throwable e e))]
          (is (identical? original caught))
          (is (= 1 @calls))
          (is (= [:gesso.live.xtdb/execute-tx-started
                  :gesso.live.xtdb/execute-tx-failed]
                 @observed)))))))

(deftest assertion-in-submit-success-trace-preserves-submission-result-test
  ;; submit-tx is not a confirmed commit, but its returned acceptance result
  ;; must not be replaced by a debug hook's failure either.
  (let [submitted {:tx-id 221}
        calls (atom 0)
        diagnostic (AssertionError. "submit trace failed")]
    (with-xtdb-stub
      '*submit-tx*
      (fn [& _] (swap! calls inc) submitted)
      (fn []
        (let [outcome (try
                        (xtdb-live/submit-tx!
                         :conn sample-tx
                         {:debug-fn (fn [{:keys [event]}]
                                      (when (= event :gesso.live.xtdb/submit-tx-succeeded)
                                        (throw diagnostic)))})
                        (catch Throwable e e))]
          (is (= {:tx-result submitted :consistency {:tx-id 221}} outcome))
          (is (= 1 @calls)))))))

(deftest assertion-in-submit-failure-trace-preserves-original-error-test
  (let [original (ex-info "submit really failed" {:reason :storage})
        diagnostic (AssertionError. "submit failure trace failed")]
    (with-xtdb-stub
      '*submit-tx*
      (fn [& _] (throw original))
      (fn []
        (is (identical? original
                        (try
                          (xtdb-live/submit-tx!
                           :conn sample-tx
                           {:debug-fn (fn [{:keys [event]}]
                                        (when (= event :gesso.live.xtdb/submit-tx-failed)
                                          (throw diagnostic)))})
                          (catch Throwable e e))))))))

(deftest assertion-in-execute-start-trace-cannot-suppress-write-test
  ;; A tracing problem before executing must not suppress the real operation.
  (let [calls (atom 0)
        t (Instant/parse "2026-01-01T00:00:00Z")
        committed (tx-key 222 t)]
    (with-xtdb-stub
      '*execute-tx*
      (fn [& _] (swap! calls inc) committed)
      (fn []
        (let [outcome (try
                        (xtdb-live/execute-tx!
                         :conn sample-tx
                         {:debug-fn (fn [{:keys [event]}]
                                      (when (= event :gesso.live.xtdb/execute-tx-started)
                                        (throw (AssertionError. "startup trace failed"))))})
                        (catch Throwable e e))]
          (is (and (map? outcome)
                   (identical? committed (:tx-result outcome))))
          (is (= 1 @calls)))))))

(deftest submit-tx-debug-failure-after-success-preserves-commit-result-test
  (let [calls (atom 0)
        observed (atom [])
        committed {:tx-id 211}]
    (with-xtdb-stub
      '*submit-tx*
      (fn [_ _ _]
        (swap! calls inc)
        committed)
      (fn []
        (let [result (xtdb-live/submit-tx!
                      :conn sample-tx
                      {:debug-fn (fn [entry]
                                   (swap! observed conj (:event entry))
                                   (when (= :gesso.live.xtdb/submit-tx-succeeded
                                            (:event entry))
                                     (throw (ex-info "diagnostic failed" {}))))})]
          (is (= {:tx-result committed
                  :consistency {:tx-id 211}}
                 result))
          (is (= 1 @calls)
              "a committed write must not be retried or misreported")
          (is (= [:gesso.live.xtdb/submit-tx-started
                  :gesso.live.xtdb/submit-tx-succeeded]
                 @observed)))))))

(deftest execute-tx-debug-failure-before-start-does-not-prevent-write-test
  (let [calls (atom 0)
        t (Instant/parse "2026-01-01T00:00:00Z")
        committed (tx-key 212 t)]
    (with-xtdb-stub
      '*execute-tx*
      (fn [_ _ _]
        (swap! calls inc)
        committed)
      (fn []
        (let [result (xtdb-live/execute-tx!
                      :conn sample-tx
                      {:debug-fn (fn [entry]
                                   (when (= :gesso.live.xtdb/execute-tx-started
                                            (:event entry))
                                     (throw (ex-info "diagnostic failed" {}))))})]
          (is (= committed (:tx-result result)))
          (is (= {:tx-id 212 :system-time t :snapshot-time t}
                 (:consistency result)))
          (is (= 1 @calls)
              "a debug exception must not suppress authoritative execution"))))))

(deftest execute-tx-debug-failure-does-not-mask-original-xtdb-error-test
  (let [cause (ex-info "actual XTDB execution failed" {:reason :storage})
        diagnostic (ex-info "diagnostic failed" {})
        calls (atom 0)
        observations (atom [])]
    (with-xtdb-stub
      '*execute-tx*
      (fn [_ _ _]
        (swap! calls inc)
        (throw cause))
      (fn []
        (let [caught (try
                       (xtdb-live/execute-tx!
                        :conn sample-tx
                        {:debug-fn (fn [entry]
                                     (swap! observations conj (:event entry))
                                     (when (= :gesso.live.xtdb/execute-tx-failed
                                              (:event entry))
                                       (throw diagnostic)))})
                       nil
                       (catch Throwable e e))]
          (is (identical? cause caught)
              "the original XTDB exception must survive failed tracing")
          (is (= 1 @calls))
          (is (= [:gesso.live.xtdb/execute-tx-started
                  :gesso.live.xtdb/execute-tx-failed]
                 @observations)))))))
