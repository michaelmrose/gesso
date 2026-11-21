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

(deftest explicit-false-connectable-slots-cannot-fall-back-test
  ;; These are ordered authority-bearing connection selections. An explicit
  ;; false is invalid, not permission to silently use a weaker node/pool.
  ;; nil remains the deliberately absent optional slot.
  (let [calls (atom [])
        unexpected (fn [& args]
                     (swap! calls conj args)
                     :unexpected)
        write-ctx {:xtdb/connectable false
                   :xtdb/conn :fallback-connection
                   :xtdb/node :fallback-node}
        read-ctx {:xtdb/read-connectable false
                  :xtdb/conn :fallback-connection
                  :xtdb/node :fallback-node}
        status-ctx {:xtdb/node false
                    :biff.xtdb/node :fallback-node}]
    (with-redefs-fn
      {(xtdb-var '*q*) unexpected
       (xtdb-var '*plan-q*) unexpected
       (xtdb-var '*submit-tx*) unexpected
       (xtdb-var '*execute-tx*) unexpected
       (xtdb-var '*status*) unexpected}
      (fn []
        (is (false? (xtdb-live/connectable-from write-ctx)))
        (is (false? (xtdb-live/read-connectable-from read-ctx)))
        (is (false? (xtdb-live/node-from status-ctx)))
        (doseq [invoke [#(xtdb-live/q-from write-ctx sample-query)
                        #(xtdb-live/plan-q-from write-ctx sample-query)
                        #(xtdb-live/q-consistent-from read-ctx sample-query)
                        #(xtdb-live/plan-q-consistent-from read-ctx sample-query)
                        #(xtdb-live/submit-tx-from! write-ctx sample-tx)
                        #(xtdb-live/execute-tx-from! write-ctx sample-tx)
                        #(xtdb-live/latest-completed-basis status-ctx)
                        #(xtdb-live/bind-request-frontier status-ctx)]]
          (is (thrown? clojure.lang.ExceptionInfo (invoke)))
          (is (empty? @calls)
              "Invalid higher-priority connectable must fail before XTDB I/O")))))
  ;; Explicit nil continues to permit the existing fallback semantics.
  (is (= :fallback-connection
         (xtdb-live/connectable-from
          {:xtdb/connectable nil :xtdb/conn :fallback-connection})))
  (is (= :fallback-connection
         (xtdb-live/read-connectable-from
          {:xtdb/read-connectable nil :xtdb/conn :fallback-connection})))
  (is (= :fallback-node
         (xtdb-live/node-from
          {:xtdb/node nil :biff.xtdb/node :fallback-node}))))

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

(deftest consistency-context-aliases-preserve-all-read-obligations-test
  ;; Request layers can supply the same read contract through distinct facade
  ;; keys. Treat them as independently admitted constraints, not as an
  ;; override chain that silently discards the lower-priority key's values.
  (let [t (Instant/parse "2026-11-20T17:00:00Z")
        ctx {:xtdb/node :node
             :xtdb/read-connectable :read
             :gesso.live/consistency {:snapshot-time t}
             :xtdb/consistency {:await-token "write-2046"}}
        seen (atom [])
        query (fn [_ _ opts] (swap! seen conj opts) :queried)]
    (is (= {:snapshot-time t :await-token "write-2046"}
           (xtdb-live/consistency-from ctx)))
    (with-redefs-fn {(xtdb-var '*q*) query
                     (xtdb-var '*plan-q*) query}
      (fn []
        (is (= :queried (xtdb-live/q-consistent-from ctx sample-query)))
        (is (= :queried (xtdb-live/plan-q-consistent-from ctx sample-query)))))
    (is (= ["write-2046" "write-2046"]
           (mapv :await-token @seen))
        "Both physical read paths must carry the otherwise discarded wait")
    (is (= [t t] (mapv :snapshot-time @seen)))))

(deftest consistency-context-aliases-reject-contradictions-before-io-test
  (let [t (Instant/parse "2026-11-20T17:00:00Z")
        later (.plusSeconds t 60)
        variants [{:gesso.live/consistency {:snapshot-time t}
                   :xtdb/consistency {:snapshot-time later}}
                  {:gesso.live/consistency {:await-token "a"}
                   :consistency {:await-token "b"}}
                  {:gesso.live.xtdb/consistency {:default-tz "UTC"}
                   :xtdb/consistency {:default-tz "Asia/Tokyo"}}
                  {:gesso.live/consistency {:snapshot-time t}
                   :xtdb/consistency false}]
        invoked (atom [])
        io (fn [& args] (swap! invoked conj args) :unexpected)]
    (with-redefs-fn {(xtdb-var '*q*) io
                     (xtdb-var '*plan-q*) io
                     (xtdb-var '*status*) io}
      (fn []
        (doseq [aliases variants]
          (let [ctx (merge {:xtdb/node :authority-node
                            :xtdb/read-connectable :read} aliases)
                before (count @invoked)]
            (doseq [invoke [#(xtdb-live/consistency-from ctx)
                            #(xtdb-live/q-consistent-from ctx sample-query)
                            #(xtdb-live/plan-q-consistent-from ctx sample-query)
                            #(xtdb-live/bind-request-frontier ctx)]]
              (is (thrown? clojure.lang.ExceptionInfo (invoke))
                  (str "Contradictory consistency aliases must fail: "
                       (pr-str aliases)))
              (is (= before (count @invoked))
                  "Contradictory consistency must fail before XTDB I/O"))))))
    ;; Repeated identical coordinates are safe; nil is optional absence.
    (is (= {:await-token "a"}
           (xtdb-live/consistency-from
            {:gesso.live/consistency {:await-token "a"}
             :xtdb/consistency {:await-token "a"}})))
    (is (= {:await-token "a"}
           (xtdb-live/consistency-from
            {:gesso.live/consistency nil
             :xtdb/consistency {:await-token "a"}})))))

(deftest consistency-from-rejects-conflicting-context-aliases-test
  ;; Independent aliases may combine disjoint snapshot bases, but two
  ;; distinct times for the same database cannot become one admitted read.
  (let [t (Instant/parse "2026-11-20T18:00:00Z")
        later (.plusSeconds t 60)
        first-token (xt.basis/->time-basis-str {"analytics" [t]})
        second-token (xt.basis/->time-basis-str {"analytics" [later]})]
    (doseq [ctx [{:gesso.live/consistency {:snapshot-token first-token}
                  :gesso.live.xtdb/consistency
                  {:snapshot-token second-token}}
                 {:xtdb/consistency {:snapshot-token first-token}
                  :consistency {:snapshot-token second-token}}]]
      (let [failure (try (xtdb-live/consistency-from ctx)
                         nil
                         (catch clojure.lang.ExceptionInfo e e))]
        (is (instance? clojure.lang.ExceptionInfo failure))
        (when failure
          (is (= :conflicting-xtdb-consistency-aliases
                 (:reason (ex-data failure)))))))
    (doseq [invalid [false "not-a-snapshot-token" 42]]
      (let [failure (try
                      (xtdb-live/consistency-from
                       {:gesso.live/consistency {:snapshot-token first-token}
                        :xtdb/consistency {:snapshot-token invalid}})
                      nil
                      (catch clojure.lang.ExceptionInfo e e))]
        (is (instance? clojure.lang.ExceptionInfo failure))
        (when failure
          (is (= :invalid-snapshot-token (:reason (ex-data failure))))))))
  (is (= {:await-token "wait" :default-tz "UTC"}
         (xtdb-live/consistency-from
          {:gesso.live.xtdb/consistency {:await-token "wait"}
           :xtdb/consistency {:default-tz "UTC"}
           :consistency {:await-token "wait"}}))))

(deftest consistency-alias-snapshot-tokens-compose-independent-bases-test
  ;; Each alias is a separate source of admitted query read obligations. Two
  ;; canonical tokens for disjoint databases must retain BOTH bases instead
  ;; of being rejected solely because their opaque strings differ.
  (let [t (Instant/parse "2026-11-20T18:00:00Z")
        later (.plusSeconds t 60)
        analytics (xt.basis/->time-basis-str {"analytics" [t]})
        reports (xt.basis/->time-basis-str {"reports" [later]})
        all-bases {"analytics" [t] "reports" [later]}
        ctx {:xtdb/node :authority-node
             :xtdb/read-connectable :read
             :gesso.live/consistency {:snapshot-token analytics}
             :xtdb/consistency {:snapshot-token reports}}
        observed (atom [])
        query (fn [_ _ opts] (swap! observed conj opts) :queried)]
    (is (= all-bases
           (-> ctx xtdb-live/consistency-from :snapshot-token
               xt.basis/<-time-basis-str)))
    (with-redefs-fn {(xtdb-var '*q*) query
                     (xtdb-var '*plan-q*) query}
      (fn []
        (is (= :queried (xtdb-live/q-consistent-from ctx sample-query)))
        (is (= :queried (xtdb-live/plan-q-consistent-from ctx sample-query)))))
    (is (= [all-bases all-bases]
           (mapv (comp xt.basis/<-time-basis-str :snapshot-token) @observed)))
    ;; The selected XTDB database has no completed transaction; independent
    ;; foreign database observations are still legitimate request evidence.
    (with-xtdb-stub '*status*
      (fn [_] {:latest-completed-txs {"xtdb" []}})
      (fn []
        (let [bound (xtdb-live/bind-request-frontier ctx)]
          (is (= all-bases
                 (-> bound xtdb-live/consistency-from :snapshot-token
                     xt.basis/<-time-basis-str)))
          (is (nil? (:biff.xtdb/snapshot-token bound))))))))

(deftest consistency-alias-snapshot-tokens-preserve-overlap-and-reject-conflicts-test
  (let [t (Instant/parse "2026-11-20T18:00:00Z")
        later (.plusSeconds t 60)
        analytics (xt.basis/->time-basis-str {"analytics" [t]})
        both (xt.basis/->time-basis-str
              {"analytics" [t] "reports" [later]})
        conflicting (xt.basis/->time-basis-str {"analytics" [later]})
        expected {"analytics" [t] "reports" [later]}
        compatible {:gesso.live/consistency {:snapshot-token analytics}
                    :xtdb/consistency {:snapshot-token both}}
        incompatible {:gesso.live/consistency {:snapshot-token analytics}
                      :xtdb/consistency {:snapshot-token conflicting}}
        calls (atom 0)]
    (is (= expected
           (-> compatible xtdb-live/consistency-from :snapshot-token
               xt.basis/<-time-basis-str))
        "Overlapping matching coordinates must not discard an extra base")
    (with-redefs-fn {(xtdb-var '*q*)
                     (fn [& _] (swap! calls inc) :unexpected)}
      (fn []
        (doseq [invoke [#(xtdb-live/consistency-from incompatible)
                        #(xtdb-live/q-consistent-from
                          (assoc incompatible :xtdb/read-connectable :read)
                          sample-query)]]
          (let [failure (try (invoke)
                             nil
                             (catch clojure.lang.ExceptionInfo e e))]
            (is (instance? clojure.lang.ExceptionInfo failure))
            (when failure
              (is (= :conflicting-xtdb-consistency-aliases
                     (:reason (ex-data failure)))))))))
    (is (zero? @calls)
        "Contradictory independent read snapshots must fail before XTDB")))

(deftest deferred-biff-composes-compatible-query-snapshot-tokens-test
  ;; A Biff wrapper may receive a new request after frontier admission.
  ;; Preserve independent query snapshot bases across that boundary instead
  ;; of rejecting two different serialized tokens on string inequality.
  (let [t (Instant/parse "2026-11-20T18:00:00Z")
        later (.plusSeconds t 60)
        analytics (xt.basis/->time-basis-str {"analytics" [t]})
        reports (xt.basis/->time-basis-str {"reports" [later]})
        different-analytics (xt.basis/->time-basis-str
                             {"analytics" [later]})
        expected {"analytics" [t] "reports" [later]}
        called (atom 0)
        bound-ctx {:xtdb/node :authority-node
                   :gesso.live/consistency {:snapshot-token analytics}}]
    (doseq [populated? [true false]]
      (with-xtdb-stub '*status*
        (fn [_] {:latest-completed-txs
                 {"xtdb" (if populated?
                           [{:tx-id 2052 :system-time t}]
                           [])}})
        (fn []
          (let [bound (xtdb-live/bind-request-frontier bound-ctx)
                handler ((:biff.core/wrap-db-snapshot bound)
                         (fn [ctx]
                           (swap! called inc)
                           (:snapshot-token
                            (xtdb-live/consistency-from ctx))))]
            (is (= expected
                   (-> (handler {:xtdb/consistency
                                 {:snapshot-token reports}})
                       xt.basis/<-time-basis-str)))
            (let [prior @called
                  failure (try
                            (handler {:xtdb/consistency
                                      {:snapshot-token different-analytics}})
                            nil
                            (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo failure))
              (when failure
                (is (= :conflicting-runtime-xtdb-consistency
                       (:reason (ex-data failure)))))
              (is (= prior @called)
                  "Conflicting runtime snapshot must fail before Biff body"))))))
    (is (= 2 @called))))

(deftest consistency-aliases-must-not-fabricate-tx-metadata-pairs-test
  ;; A tx-id and its system-time are one transaction observation. Combining
  ;; independently supplied fragments can manufacture a complete basis that
  ;; neither source ever actually established. The same applies to the
  ;; snapshot-time fallback used by consistency->basis.
  (let [t (Instant/parse "2026-11-20T18:00:00Z")
        split [{:gesso.live/consistency {:tx-id 2047}
                :xtdb/consistency {:system-time t}}
               {:gesso.live/consistency {:tx-id 2047}
                :xtdb/consistency {:snapshot-time t}}
               {:gesso.live/consistency {:system-time t}
                :xtdb/consistency {:tx-id 2047}}
               {:gesso.live/consistency {:snapshot-time t}
                :xtdb/consistency {:tx-id 2047}}]
        calls (atom [])
        unexpected (fn [& args] (swap! calls conj args) :unexpected)]
    (with-redefs-fn {(xtdb-var '*q*) unexpected
                     (xtdb-var '*plan-q*) unexpected
                     (xtdb-var '*status*) unexpected}
      (fn []
        (doseq [aliases split
                :let [ctx (merge {:xtdb/node :authority-node
                                  :xtdb/read-connectable :read} aliases)]]
          (doseq [invoke [#(xtdb-live/consistency-from ctx)
                          #(xtdb-live/q-consistent-from ctx sample-query)
                          #(xtdb-live/plan-q-consistent-from ctx sample-query)
                          #(xtdb-live/bind-request-frontier ctx)]]
            (let [failure (try (invoke)
                               nil
                               (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo failure)
                  (str "Split transaction metadata should not be promoted: "
                       (pr-str aliases)))
              (when failure
                (is (= :split-xtdb-transaction-metadata
                       (:reason (ex-data failure)))))))
          (is (empty? @calls)
              "Incomplete independent receipts must fail before XTDB I/O"))))
    ;; A complete same-source receipt can safely survive composition with
    ;; other independent query obligations, including an equal snapshot time.
    (is (= (xtdb-live/basis :xtdb 2047 t)
           (-> {:gesso.live/consistency {:tx-id 2047 :system-time t}
                :xtdb/consistency {:snapshot-time t :await-token "wait"}}
               xtdb-live/consistency-from
               (xtdb-live/consistency->basis :xtdb))))
    (is (= {:tx-id 2047 :snapshot-time t :default-tz "UTC"}
           (xtdb-live/consistency-from
            {:gesso.live/consistency {:tx-id 2047 :snapshot-time t}
             :consistency {:default-tz "UTC"}})))))

(deftest deferred-biff-cannot-assemble-transaction-receipt-from-two-requests-test
  ;; Biff may pass a new Ring context to the handler. Admission-time and
  ;; deferred metadata are distinct sources; their merge must not manufacture
  ;; a complete tx basis, whether the database is populated or empty.
  (let [t (Instant/parse "2026-11-20T18:00:00Z")
        splits [[{:tx-id 2047} {:system-time t}]
                [{:tx-id 2047} {:snapshot-time t}]
                [{:system-time t} {:tx-id 2047}]
                [{:snapshot-time t} {:tx-id 2047}]]
        calls (atom 0)
        body (fn [_] (swap! calls inc) :unexpected)]
    (doseq [populated? [true false]]
      (with-xtdb-stub '*status*
        (fn [_] {:latest-completed-txs
                 {"xtdb" (if populated?
                           [{:tx-id 2047 :system-time t}]
                           [])}})
        (fn []
          (doseq [[admitted deferred] splits]
            (let [bound (xtdb-live/bind-request-frontier
                         {:xtdb/node :authority-node
                          :gesso.live/consistency admitted})
                  handler ((:biff.core/wrap-db-snapshot bound) body)
                  failure (try (handler {:xtdb/consistency deferred})
                               nil
                               (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo failure)
                  "Deferred wrapper cannot synthesize tx-id/time pairing")
              (when failure
                (is (= :split-xtdb-transaction-metadata
                       (:reason (ex-data failure))))))
            (is (zero? @calls)
                "Rejected pairing must not invoke the Biff handler")))))))

(deftest deferred-biff-preserves-complete-admitted-transaction-metadata-test
  ;; One complete admission-time pair remains usable after an independent
  ;; deferred read constraint is added; the safety check is not a ban on
  ;; composing query consistency or keeping matching metadata.
  (let [t (Instant/parse "2026-11-20T18:00:00Z")
        expected {:tx-id 2047 :system-time t
                  :snapshot-time t :await-token "fresh-wait"}]
    (doseq [populated? [true false]]
      (with-xtdb-stub '*status*
        (fn [_] {:latest-completed-txs
                 {"xtdb" (if populated?
                           [{:tx-id 2047 :system-time t}]
                           [])}})
        (fn []
          (let [bound (xtdb-live/bind-request-frontier
                       {:xtdb/node :authority-node
                        :gesso.live/consistency
                        {:tx-id 2047 :system-time t}})
                handler ((:biff.core/wrap-db-snapshot bound)
                         (fn [ctx] (:gesso.live/consistency ctx)))]
            (is (= expected
                   (handler {:xtdb/consistency
                             {:snapshot-time t :await-token "fresh-wait"}})))))))))

(deftest consistency-context-rejects-inconsistent-transaction-times-test
  ;; A completed tx receipt has one system time. An independently admitted
  ;; snapshot-time cannot silently change that receipt merely because it
  ;; arrives through a different context alias (or the same alias).
  (let [committed (Instant/parse "2026-11-20T18:00:00Z")
        different (.minusSeconds committed 1)
        cases [{:gesso.live/consistency {:tx-id 2048 :system-time committed}
                :xtdb/consistency {:snapshot-time different}}
               {:gesso.live/consistency {:tx-id 2048 :snapshot-time committed}
                :consistency {:system-time different}}
               {:gesso.live/consistency {:tx-id 2048
                                         :system-time committed
                                         :snapshot-time different}}]
        calls (atom [])
        unexpected (fn [& args] (swap! calls conj args) :unexpected)]
    (with-redefs-fn {(xtdb-var '*q*) unexpected
                     (xtdb-var '*plan-q*) unexpected
                     (xtdb-var '*status*) unexpected}
      (fn []
        (doseq [aliases cases
                :let [ctx (merge {:xtdb/node :authority-node
                                  :xtdb/read-connectable :read} aliases)]]
          (doseq [invoke [#(xtdb-live/consistency-from ctx)
                          #(xtdb-live/q-consistent-from ctx sample-query)
                          #(xtdb-live/plan-q-consistent-from ctx sample-query)
                          #(xtdb-live/bind-request-frontier ctx)]]
            (let [failure (try (invoke)
                               nil
                               (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo failure)
                  (str "Contradictory transaction times must fail: "
                       (pr-str aliases)))
              (when failure
                (is (= :contradictory-xtdb-transaction-times
                       (:reason (ex-data failure)))))))
          (is (empty? @calls)
              "Rejected transaction-time metadata must not reach XTDB"))))
    ;; A complete transaction accompanied by an equal independent snapshot
    ;; time is allowed and must retain its metadata and await requirement.
    (is (= {:tx-id 2048 :system-time committed
            :snapshot-time committed :await-token "write-token"}
           (xtdb-live/consistency-from
            {:gesso.live/consistency {:tx-id 2048
                                      :system-time committed}
             :xtdb/consistency {:snapshot-time committed
                                :await-token "write-token"}})))))

(deftest deferred-biff-rejects-contradictory-transaction-times-test
  ;; Biff may introduce new consistency in the actual handler context.
  ;; A complete admitted pair is not authority to overwrite its time from a
  ;; different context; check both the populated and empty node paths.
  (let [committed (Instant/parse "2026-11-20T18:00:00Z")
        different (.minusSeconds committed 1)
        calls (atom 0)]
    (doseq [populated? [true false]]
      (with-xtdb-stub '*status*
        (fn [_] {:latest-completed-txs
                 {"xtdb" (if populated?
                           [{:tx-id 2048 :system-time committed}]
                           [])}})
        (fn []
          (let [bound (xtdb-live/bind-request-frontier
                       {:xtdb/node :authority-node
                        :gesso.live/consistency {:tx-id 2048
                                                 :system-time committed}})
                wrapped ((:biff.core/wrap-db-snapshot bound)
                         (fn [_] (swap! calls inc) :unexpected))
                failure (try (wrapped {:xtdb/consistency
                                       {:snapshot-time different}})
                             nil
                             (catch clojure.lang.ExceptionInfo e e))]
            (is (instance? clojure.lang.ExceptionInfo failure))
            (when failure
              (is (= :contradictory-xtdb-transaction-times
                     (:reason (ex-data failure)))))
            (is (zero? @calls)
                "A conflicting deferred time must not reach the handler")))))))

(deftest complete-tx-receipts-pin-unprogressed-consistent-reads-test
  ;; A complete transaction receipt is a minimum read obligation even when
  ;; the caller has not installed a separate generic progression carrier.
  ;; The XTDB query path must not silently ignore its system-time or allow an
  ;; explicit earlier snapshot-time to override it.
  (let [t (Instant/parse "2026-11-20T21:00:00Z")
        older (.minusSeconds t 60)
        later (.plusSeconds t 60)
        receipt {:tx-id 2057 :system-time t}
        with-as-of {:tx-id 2057 :system-time t :snapshot-time t}
        ctx {:xtdb/read-connectable :read
             :gesso.live/consistency receipt}
        calls (atom [])
        observed (fn [& args]
                   (swap! calls conj args)
                   :queried)]
    (is (= t (:snapshot-time
              (xtdb-live/consistent-query-opts receipt))))
    (is (= t (:snapshot-time
              (xtdb-live/read-query-opts receipt nil))))
    (with-redefs-fn {(xtdb-var '*q*) observed
                     (xtdb-var '*plan-q*) observed}
      (fn []
        (is (= :queried (xtdb-live/q-consistent :read sample-query receipt)))
        (is (= :queried (xtdb-live/plan-q-consistent :read sample-query receipt)))
        (is (= :queried (xtdb-live/q-consistent-from ctx sample-query)))
        (is (= :queried (xtdb-live/plan-q-consistent-from ctx sample-query)))
        (is (= [t t t t] (mapv (comp :snapshot-time last) @calls))
            "All four physical query paths must retain the complete receipt")
        (let [before (count @calls)]
          (doseq [consistency [receipt with-as-of]
                  invoke [#(xtdb-live/consistent-query-opts consistency
                                                             {:snapshot-time older})
                          #(xtdb-live/read-query-opts consistency nil
                                                      {:snapshot-time older})
                          #(xtdb-live/q-consistent :read sample-query consistency
                                                   {:snapshot-time older})
                          #(xtdb-live/q-consistent-from
                            (assoc ctx :gesso.live/consistency consistency)
                            sample-query {:snapshot-time older})]]
            (let [failure (try (invoke) nil
                               (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo failure)
                  "An earlier caller snapshot cannot satisfy a complete receipt")
              (when failure
                (is (= :query-snapshot-behind-transaction-receipt
                       (:reason (ex-data failure)))))))
          (is (= before (count @calls))
              "No rejected query may reach XTDB"))))
    ;; A canonical selected-db snapshot token also has to cover the receipt;
    ;; a foreign-db token cannot replace the selected database's minimum.
    (let [old-selected (xt.basis/->time-basis-str {"xtdb" [older]})
          matching (xt.basis/->time-basis-str {"xtdb" [t]})
          foreign (xt.basis/->time-basis-str {"analytics" [older]})]
      (doseq [opts [{:snapshot-token old-selected}
                    {:snapshot-token "malformed-token"}]]
        (let [failure (try (xtdb-live/consistent-query-opts receipt opts)
                           nil
                           (catch clojure.lang.ExceptionInfo e e))]
          (is (instance? clojure.lang.ExceptionInfo failure))
          (when failure
            (is (contains? #{:invalid-snapshot-token
                             :query-snapshot-behind-transaction-receipt}
                           (:reason (ex-data failure)))))))
      (is (= matching (:snapshot-token
                       (xtdb-live/consistent-query-opts
                        receipt {:snapshot-token matching}))))
      (is (nil? (:snapshot-time
                 (xtdb-live/consistent-query-opts
                  receipt {:snapshot-token matching}))))
      (is (= t (:snapshot-time
                (xtdb-live/consistent-query-opts
                 receipt {:snapshot-token foreign}))))
      (is (= foreign (:snapshot-token
                      (xtdb-live/consistent-query-opts
                       receipt {:snapshot-token foreign})))))
    ;; A later caller snapshot still includes the complete transaction.
    (is (= later (:snapshot-time
                  (xtdb-live/consistent-query-opts receipt
                                                   {:snapshot-time later}))))
    ;; Incomplete metadata is not a receipt and retains ordinary precedence.
    (is (= older (:snapshot-time
                  (xtdb-live/read-query-opts {:tx-id 2057} nil
                                             {:snapshot-time older}))))))

(deftest complete-receipts-compose-caller-query-snapshot-tokens-test
  ;; With a complete tx/time receipt, query consistency describes an admitted
  ;; minimum. Caller opts must not silently discard an independent database's
  ;; snapshot token while preserving only the transaction's selected minimum.
  ;; No generic progression is required to expose this loss of read evidence.
  (let [t (Instant/parse "2026-11-20T21:00:00Z")
        earlier (.minusSeconds t 60)
        older (.minusSeconds t 120)
        analytics (xt.basis/->time-basis-str {"analytics" [earlier]})
        reports (xt.basis/->time-basis-str {"reports" [older]})
        duplicate (xt.basis/->time-basis-str {"analytics" [earlier]
                                                   "reports" [older]})
        conflict (xt.basis/->time-basis-str {"analytics" [older]})
        expected {"analytics" [earlier] "reports" [older]}
        consistency {:tx-id 2058 :system-time t :snapshot-token analytics}
        ctx {:xtdb/read-connectable :read
             :gesso.live/consistency consistency}
        seen (atom [])
        query (fn [& args] (swap! seen conj args) :queried)]
    ;; Both option constructors must retain disjoint or equal-overlap bases.
    (doseq [caller [reports duplicate]]
      (is (= expected
             (-> (xtdb-live/consistent-query-opts
                  consistency {:snapshot-token caller})
                 :snapshot-token xt.basis/<-time-basis-str)))
      (is (= expected
             (-> (xtdb-live/read-query-opts
                  consistency nil {:snapshot-token caller})
                 :snapshot-token xt.basis/<-time-basis-str))))
    (with-redefs-fn {(xtdb-var '*q*) query
                     (xtdb-var '*plan-q*) query}
      (fn []
        (is (= :queried (xtdb-live/q-consistent
                         :read sample-query consistency
                         {:snapshot-token reports})))
        (is (= :queried (xtdb-live/plan-q-consistent
                         :read sample-query consistency
                         {:snapshot-token reports})))
        (is (= :queried (xtdb-live/q-consistent-from
                         ctx sample-query {:snapshot-token reports})))
        (is (= :queried (xtdb-live/plan-q-consistent-from
                         ctx sample-query {:snapshot-token reports})))
        (is (= [expected expected expected expected]
               (mapv (comp xt.basis/<-time-basis-str
                           :snapshot-token last) @seen)))
        (let [before (count @seen)]
          ;; Conflicting overlap and malformed admitted tokens must fail
          ;; before either physical XTDB query entry point is called.
          (doseq [[bad-consistency caller reason]
                  [[consistency conflict :conflicting-receipt-query-snapshot-bases]
                   [(assoc consistency :snapshot-token "malformed-token")
                    reports :invalid-snapshot-token]]
                  invoke [#(xtdb-live/consistent-query-opts
                            bad-consistency {:snapshot-token caller})
                          #(xtdb-live/read-query-opts
                            bad-consistency nil {:snapshot-token caller})
                          #(xtdb-live/q-consistent
                            :read sample-query bad-consistency
                            {:snapshot-token caller})
                          #(xtdb-live/plan-q-consistent-from
                            (assoc ctx :gesso.live/consistency bad-consistency)
                            sample-query {:snapshot-token caller})]]
            (let [failure (try (invoke)
                               nil
                               (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo failure))
              (when failure
                (is (= reason (:reason (ex-data failure)))))))
          (is (= before (count @seen))
              "Rejected query snapshot overrides cannot reach XTDB"))))
    ;; No complete transaction receipt means normal caller option precedence
    ;; is still available for this convenience-only consistency API.
    (is (= reports
           (:snapshot-token
            (xtdb-live/consistent-query-opts
             {:snapshot-token analytics} {:snapshot-token reports}))))
    (is (= reports
           (:snapshot-token
            (xtdb-live/read-query-opts
             {:snapshot-token analytics} nil {:snapshot-token reports}))))))

(deftest complete-receipts-preserve-admitted-query-await-tokens-test
  ;; A complete transaction receipt is an admitted read minimum even without
  ;; a separate generic progression carrier. An independent opaque await
  ;; token can express an additional write visibility requirement: the
  ;; caller's different token has no known stronger/weaker ordering and
  ;; cannot silently replace it in either XTDB read path.
  (let [time (Instant/parse "2026-11-20T21:00:00Z")
        consistency {:tx-id 2059 :system-time time
                     :await-token "admitted-wait"}
        ctx {:xtdb/read-connectable :read
             :gesso.live/consistency consistency}
        alternate {:await-token "caller-wait"}
        calls (atom [])
        query (fn [& args]
                (swap! calls conj args)
                :queried)]
    (doseq [builder [#(xtdb-live/consistent-query-opts consistency alternate)
                     #(xtdb-live/read-query-opts consistency nil alternate)]]
      (let [failure (try (builder)
                         nil
                         (catch clojure.lang.ExceptionInfo e e))]
        (is (instance? clojure.lang.ExceptionInfo failure)
            "Different opaque wait tokens cannot satisfy each other")
        (when failure
          (is (= :conflicting-receipt-query-await-tokens
                 (:reason (ex-data failure)))))))
    (with-redefs-fn {(xtdb-var '*q*) query
                     (xtdb-var '*plan-q*) query}
      (fn []
        (doseq [invoke [#(xtdb-live/q-consistent
                          :read sample-query consistency alternate)
                        #(xtdb-live/plan-q-consistent
                          :read sample-query consistency alternate)
                        #(xtdb-live/q-consistent-from
                          ctx sample-query alternate)
                        #(xtdb-live/plan-q-consistent-from
                          ctx sample-query alternate)]]
          (let [failure (try (invoke)
                             nil
                             (catch clojure.lang.ExceptionInfo e e))]
            (is (instance? clojure.lang.ExceptionInfo failure)
                "Conflicting waits must fail before physical query I/O")
            (when failure
              (is (= :conflicting-receipt-query-await-tokens
                     (:reason (ex-data failure)))))))
        (is (empty? @calls)
            "Neither physical query path may see a discarded admitted wait")
        ;; Exact duplicate tokens and independent other option fields remain
        ;; valid, preserving the original wait in the resulting XTDB opts.
        (doseq [builder [#(xtdb-live/consistent-query-opts
                            consistency {:await-token "admitted-wait"
                                         :key-fn :kebab-case-keyword})
                         #(xtdb-live/read-query-opts
                            consistency nil {:await-token "admitted-wait"
                                             :key-fn :kebab-case-keyword})]]
          (is (= {:await-token "admitted-wait"
                  :snapshot-time time
                  :key-fn :kebab-case-keyword}
                 (select-keys (builder)
                              [:await-token :snapshot-time :key-fn]))))
        (is (= :queried
               (xtdb-live/q-consistent
                :read sample-query consistency
                {:await-token "admitted-wait"})))
        (is (= "admitted-wait" (:await-token (last (last @calls)))))))
    ;; Without a complete receipt, the ordinary caller override remains
    ;; intentionally supported by both option constructors.
    (is (= "caller-wait"
           (:await-token
            (xtdb-live/consistent-query-opts
             {:await-token "admitted-wait"} alternate))))
    (is (= "caller-wait"
           (:await-token
            (xtdb-live/read-query-opts
             {:tx-id 2059 :await-token "admitted-wait"} nil alternate))))))

(deftest complete-receipts-preserve-admitted-temporal-query-context-test
  ;; Temporal query options affect the meaning of an admitted read. As with
  ;; authoritative progression, a complete tx/time receipt cannot let caller
  ;; opts silently replace the originally admitted current-time or timezone.
  ;; This does not impose new precedence on convenience-only consistency.
  (let [tx-time (Instant/parse "2026-11-20T18:00:00Z")
        current-time (Instant/parse "2026-11-20T20:00:00Z")
        later-time (.plusSeconds current-time 60)
        base {:tx-id 2060 :system-time tx-time}
        conflicts [[{:current-time current-time}
                    {:current-time later-time} :current-time]
                   [{:default-tz "UTC"}
                    {:default-tz "Asia/Tokyo"} :default-tz]]
        invoked (atom [])
        query (fn [& args]
                (swap! invoked conj args)
                :unexpected)]
    (with-redefs-fn {(xtdb-var '*q*) query
                     (xtdb-var '*plan-q*) query}
      (fn []
        (doseq [[admitted caller field] conflicts
                :let [consistency (merge base admitted)
                      ctx {:xtdb/read-connectable :read
                           :gesso.live/consistency consistency}]
                invoke [#(xtdb-live/consistent-query-opts consistency caller)
                        #(xtdb-live/read-query-opts consistency nil caller)
                        #(xtdb-live/q-consistent :read sample-query
                                                 consistency caller)
                        #(xtdb-live/plan-q-consistent :read sample-query
                                                      consistency caller)
                        #(xtdb-live/q-consistent-from ctx sample-query caller)
                        #(xtdb-live/plan-q-consistent-from ctx sample-query caller)]]
          (let [failure (try (invoke)
                             nil
                             (catch clojure.lang.ExceptionInfo e e))]
            (is (instance? clojure.lang.ExceptionInfo failure)
                (str "Admitted temporal context overridden: " field))
            (when (instance? clojure.lang.ExceptionInfo failure)
              (is (= :conflicting-receipt-temporal-read-context
                     (:reason (ex-data failure))))
              (is (= field (:field (ex-data failure)))))))
        (is (empty? @invoked)
            "Rejected temporal context conflicts must fail before XTDB I/O")
        ;; Identical or independent temporal fields are compatible, and the
        ;; complete receipt still establishes its transaction-time minimum.
        (let [same (xtdb-live/consistent-query-opts
                    (merge base {:current-time current-time})
                    {:current-time current-time :default-tz "UTC"})
              separate (xtdb-live/read-query-opts
                        (merge base {:current-time current-time}) nil
                        {:default-tz "Asia/Tokyo"})]
          (is (= current-time (:current-time same)))
          (is (= "UTC" (:default-tz same)))
          (is (= tx-time (:snapshot-time same)))
          (is (= current-time (:current-time separate)))
          (is (= "Asia/Tokyo" (:default-tz separate))))))
    ;; Existing caller override behavior remains documented for no complete
    ;; receipt, even when one partial metadata field is present.
    (is (= later-time
           (:current-time
            (xtdb-live/consistent-query-opts
             {:tx-id 2060 :current-time current-time}
             {:current-time later-time}))))
    (is (= "Asia/Tokyo"
           (:default-tz
            (xtdb-live/read-query-opts
             {:system-time tx-time :default-tz "UTC"} nil
             {:default-tz "Asia/Tokyo"}))))))

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
             :consistency {:snapshot-token (xtdb-live/basis-snapshot-token
                                            (xtdb-live/basis :analytics 1
                                            (Instant/parse "2025-01-01T00:00:00Z")))
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
                {:snapshot-token (xtdb-live/basis-snapshot-token
                                  (xtdb-live/basis :analytics 2
                                  (Instant/parse "2025-01-01T00:00:00Z")))
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

(deftest portable-xtdb-basis-rejects-out-of-range-transaction-ids-test
  ;; A JVM transaction id is a signed 64-bit nonnegative integer. Basis
  ;; construction must reject larger arbitrary-precision numbers as typed
  ;; validation failures, not leak a Java long-cast exception or construct
  ;; browser evidence that the closed basis decoder cannot accept.
  (let [time (Instant/parse "2026-11-20T23:00:00Z")
        in-range [0 1 9007199254740993N (bigint Long/MAX_VALUE)]
        outside [(inc (bigint Long/MAX_VALUE))
                 (+ (bigint Long/MAX_VALUE) 123456789N)]
        expected (str Long/MAX_VALUE)]
    (doseq [tx-id in-range]
      (let [encoded (xtdb-live/basis :xtdb tx-id time)]
        (is (xtdb-live/xtdb-basis? encoded))
        (is (= (str tx-id) (:tx-id encoded)))
        (is (= (long tx-id) (xtdb-live/basis-tx-id encoded)))))
    (is (= expected (:tx-id (xtdb-live/basis :xtdb Long/MAX_VALUE time))))
    (doseq [tx-id outside]
      (doseq [produce [#(xtdb-live/basis :xtdb tx-id time)
                       #(xtdb-live/consistency->basis
                         {:tx-id tx-id :system-time time} :xtdb)
                       #(xtdb-live/tx-result-basis
                         {:tx-id tx-id :system-time time} :xtdb)]]
        (let [failure (try (produce) nil (catch Throwable t t))]
          (is (instance? clojure.lang.ExceptionInfo failure)
              (str "Overflow must be rejected by Gesso, not JVM long cast: " tx-id))
          (when (instance? clojure.lang.ExceptionInfo failure)
            (is (= :invalid-xtdb-transaction-id-range
                   (:reason (ex-data failure))))))))))

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

(deftest consistency-to-basis-rejects-disagreeing-time-coordinates-test
  ;; An authoritative basis is a paired tx-id/system-time observation.
  ;; If both metadata fields are present but disagree, choosing system-time
  ;; with `or` silently discards a stronger snapshot-time requirement.
  (let [committed (Instant/parse "2026-11-20T18:00:00Z")
        future (.plusSeconds committed 60)
        past (.minusSeconds committed 60)
        expected (xtdb-live/basis :analytics 2022 committed)]
    (doseq [snapshot-time [future past]
            :let [metadata {:tx-id 2022
                            :system-time committed
                            :snapshot-time snapshot-time}
                  failure (try (xtdb-live/consistency->basis metadata :analytics)
                               nil
                               (catch clojure.lang.ExceptionInfo e e))]]
      (is (instance? clojure.lang.ExceptionInfo failure)
          (str "Conflicting authoritative times must be rejected: "
               (pr-str snapshot-time)))
      (when (instance? clojure.lang.ExceptionInfo failure)
        (is (= committed (:system-time (ex-data failure))))
        (is (= snapshot-time (:snapshot-time (ex-data failure))))))
    ;; Valid complete and partially supplied metadata still construct the
    ;; same basis, including the public execute-tx normalized form.
    (doseq [metadata [{:tx-id 2022 :system-time committed}
                      {:tx-id 2022 :snapshot-time committed}
                      {:tx-id 2022
                       :system-time committed
                       :snapshot-time committed}]]
      (is (= expected
             (xtdb-live/consistency->basis metadata :analytics))))))

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

(deftest basis-advances-predicate-rejects-regressing-snapshot-test
  ;; This public storage-specific predicate must agree with trusted witness
  ;; issuance: a greater tx-id is not sufficient when system-time regresses.
  ;; A false result prevents callers from mistaking the transaction-id
  ;; comparator for proof that an earlier required snapshot is satisfied.
  (let [time (Instant/parse "2026-11-20T18:10:00Z")
        earlier (.minusSeconds time 60)
        later (.plusSeconds time 60)
        from (xtdb-live/basis :xtdb 70 time)
        backward-time (xtdb-live/basis :xtdb 71 earlier)
        same-time (xtdb-live/basis :xtdb 71 time)
        forward-time (xtdb-live/basis :xtdb 71 later)]
    (is (neg? (xtdb-live/compare-bases from backward-time))
        "Transaction id alone still orders the two records")
    (is (false? (xtdb-live/basis-advances? from backward-time))
        "Regression cannot be reported as an authoritative advance")
    (is (xtdb-live/basis-advances? from same-time))
    (is (xtdb-live/basis-advances? from forward-time))
    (is (false? (xtdb-live/basis-advances? from from)))
    (is (false? (xtdb-live/basis-advances? forward-time from)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (xtdb-live/progression-witness from backward-time)))
    (is (progression/advances?
         from forward-time
         (xtdb-live/progression-witness from forward-time)))))

(deftest progression-witness-must-not-assert-a-regressing-snapshot-test
  ;; A newer transaction id alone does not establish that the next basis
  ;; satisfies an already installed later system-time. The generic progression
  ;; layer deliberately trusts this storage-specific witness, so its issuer
  ;; must reject a contradictory pair before it can authorize installation.
  (let [later (Instant/parse "2026-11-20T18:10:00Z")
        earlier (.minusSeconds later 60)
        old-id-later-time (xtdb-live/basis :xtdb 40 later)
        new-id-earlier-time (xtdb-live/basis :xtdb 41 earlier)
        new-id-equal-time (xtdb-live/basis :xtdb 41 later)]
    (is (neg? (xtdb-live/compare-bases
               old-id-later-time new-id-earlier-time))
        "The transaction-order comparator still reports the newer id")
    (let [outcome (try
                    (xtdb-live/progression-witness
                     old-id-later-time new-id-earlier-time)
                    nil
                    (catch clojure.lang.ExceptionInfo e e))]
      (is (instance? clojure.lang.ExceptionInfo outcome)
          "Trusted witness must not certify a snapshot-time regression")
      (when (instance? clojure.lang.ExceptionInfo outcome)
        (is (= :regressing-progression-snapshot-time
               (:reason (ex-data outcome))))
        (is (= old-id-later-time (:from (ex-data outcome))))
        (is (= new-id-earlier-time (:to (ex-data outcome))))))
    (is (not (progression/install-allowed?
              old-id-later-time new-id-earlier-time nil))
        "Generic installation requires a trusted witness")
    (let [witness (xtdb-live/progression-witness
                   old-id-later-time new-id-equal-time)]
      (is (progression/advances?
           old-id-later-time new-id-equal-time witness))
      (is (progression/install-allowed?
           old-id-later-time new-id-equal-time witness)))))

(deftest stronger-tx-id-must-not-erase-later-required-snapshot-test
  ;; Transaction id determines ordering, but a basis with a higher id and
  ;; earlier timestamp cannot establish the later snapshot also required.
  ;; Requirement reduction must reject rather than discard that constraint,
  ;; regardless of input order or whether a query reaches an XTDB call.
  (let [later (Instant/parse "2026-11-20T18:10:00Z")
        earlier (Instant/parse "2026-11-20T18:00:00Z")
        old-id-new-time (xtdb-live/basis :xtdb 40 later)
        new-id-old-time (xtdb-live/basis :xtdb 41 earlier)
        ctx {:xtdb/read-connectable :read
             :gesso.live/progression
             (progression/requirement-from-bases
              [old-id-new-time new-id-old-time])}
        calls (atom 0)]
    (doseq [candidates [[old-id-new-time new-id-old-time]
                        [new-id-old-time old-id-new-time]]]
      (let [req (progression/requirement-from-bases candidates)]
        (is (thrown? clojure.lang.ExceptionInfo
                     (xtdb-live/strongest-required-basis req))
            "No single basis may silently discard a later required time")
        (is (thrown? clojure.lang.ExceptionInfo
                     (xtdb-live/read-query-opts nil req)))))
    (with-xtdb-stub '*q* (fn [& _] (swap! calls inc) :unexpected)
      (fn []
        (is (thrown? clojure.lang.ExceptionInfo
                     (xtdb-live/q-consistent-from ctx sample-query)))
        (is (zero? @calls)
            "Unsupported requirement must fail before any XTDB read")))))

(deftest request-frontier-must-not-satisfy-future-snapshot-with-higher-tx-id-test
  ;; Even if status observes a higher transaction id, an earlier system time
  ;; cannot establish a preexisting request requirement for a later snapshot.
  (let [later (Instant/parse "2026-11-20T18:10:00Z")
        earlier (Instant/parse "2026-11-20T18:00:00Z")
        required (xtdb-live/basis :xtdb 40 later)
        ctx {:xtdb/node :authority-node
             :gesso.live/progression (progression/requirement required)}]
    (with-xtdb-stub '*status*
      (fn [_] {:latest-completed-txs
               {"xtdb" [{:tx-id 41 :system-time earlier}]}})
      (fn []
        (is (thrown? clojure.lang.ExceptionInfo
                     (xtdb-live/bind-request-frontier ctx))
            "An earlier XTDB status time must not downgrade a browser minimum")))))

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
             :snapshot-token (xtdb-live/basis-snapshot-token
                              (xtdb-live/basis :analytics 1 old-time))
             :await-token "await"}
            requirement
            {:snapshot-time caller-time
             :snapshot-token (xtdb-live/basis-snapshot-token
                              (xtdb-live/basis :analytics 2 caller-time))
             :database :analytics
             :key-fn :kebab-case-keyword})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"database conflicts with authoritative progression"
         (xtdb-live/read-query-opts
          {}
          requirement
          {:database :xtdb})))))

(deftest progression-read-rejects-conflicting-independent-await-tokens-test
  ;; :await-token is an explicit read-consistency constraint. Once a
  ;; progression is authoritative, caller options cannot replace a different
  ;; consistency await-token: without trusted ordering, neither token proves
  ;; satisfaction of the other. Ordinary non-progression precedence is intact.
  (let [frontier (Instant/parse "2026-11-20T18:00:00Z")
        required (progression/requirement
                  (xtdb-live/basis :analytics 2026 frontier))
        consistency {:await-token "consistency-await"}
        caller {:await-token "caller-await"}
        ctx {:xtdb/read-connectable :read
             :gesso.live/progression required}
        calls (atom [])
        xtdb-call (fn [& args]
                    (swap! calls conj args)
                    :unexpected)]
    (let [failure (try (xtdb-live/read-query-opts consistency required caller)
                       nil
                       (catch clojure.lang.ExceptionInfo e e))]
      (is (instance? clojure.lang.ExceptionInfo failure)
          "Conflicting read wait requirements must not be silently merged")
      (when (instance? clojure.lang.ExceptionInfo failure)
        (is (= :conflicting-await-tokens (:reason (ex-data failure))))
        (is (= "consistency-await" (:consistency-await-token (ex-data failure))))
        (is (= "caller-await" (:caller-await-token (ex-data failure))))))
    (with-redefs-fn {(xtdb-var '*q*) xtdb-call
                     (xtdb-var '*plan-q*) xtdb-call}
      (fn []
        (doseq [invoke [#(xtdb-live/q-consistent-from
                           (assoc ctx :gesso.live/consistency consistency)
                           sample-query caller)
                        #(xtdb-live/plan-q-consistent-from
                           (assoc ctx :gesso.live/consistency consistency)
                           sample-query caller)]]
          (is (thrown? clojure.lang.ExceptionInfo (invoke)))
          (is (empty? @calls)
              "Conflicting await tokens must fail before query execution"))))
    ;; Identical or independently absent await constraints remain supported.
    (is (= "consistency-await"
           (:await-token (xtdb-live/read-query-opts
                          consistency required
                          {:await-token "consistency-await"}))))
    (is (= "caller-await"
           (:await-token (xtdb-live/read-query-opts
                          {} required caller))))
    (is (= "consistency-await"
           (:await-token (xtdb-live/read-query-opts
                          consistency required {}))))
    (is (= "caller-await"
           (:await-token (xtdb-live/read-query-opts
                          consistency nil caller))))))

(deftest progression-read-rejects-conflicting-temporal-context-test
  ;; Under an authoritative progression requirement, a query's current-time
  ;; and default-tz are part of the admitted temporal read context. Different
  ;; caller values have no reliable stronger/weaker ordering and cannot
  ;; silently replace already admitted values in the actual XTDB query.
  (let [frontier (Instant/parse "2026-11-20T18:00:00Z")
        current (Instant/parse "2026-11-20T17:00:00Z")
        later (.plusSeconds current 60)
        required (progression/requirement
                  (xtdb-live/basis :xtdb 2044 frontier))
        conflicts [[{:current-time current} {:current-time later}
                    :current-time]
                   [{:default-tz "UTC"} {:default-tz "Asia/Tokyo"}
                    :default-tz]]
        calls (atom [])
        actual-query (fn [& args] (swap! calls conj args) :unexpected)]
    (doseq [[admitted caller field] conflicts]
      (let [failure (try (xtdb-live/read-query-opts admitted required caller)
                         nil
                         (catch clojure.lang.ExceptionInfo e e))]
        (is (instance? clojure.lang.ExceptionInfo failure)
            (str "Conflicting temporal read context should fail: " field))
        (when failure
          (is (= :conflicting-temporal-read-context
                 (:reason (ex-data failure))))
          (is (= field (:field (ex-data failure))))))
      (with-redefs-fn {(xtdb-var '*q*) actual-query
                       (xtdb-var '*plan-q*) actual-query}
        (fn []
          (let [ctx {:xtdb/read-connectable :read
                     :gesso.live/progression required
                     :gesso.live/consistency admitted}]
            (doseq [invoke [#(xtdb-live/q-consistent-from ctx sample-query caller)
                            #(xtdb-live/plan-q-consistent-from ctx sample-query caller)]]
              (is (thrown? clojure.lang.ExceptionInfo (invoke)))
              (is (empty? @calls)
                  "Conflicting temporal context must fail before XTDB I/O"))))))
    ;; Equal values and independent option fields are not conflicts.
    (is (= current
           (:current-time (xtdb-live/read-query-opts
                           {:current-time current} required
                           {:current-time current}))))
    (is (= {:current-time current :default-tz "UTC"}
           (select-keys (xtdb-live/read-query-opts
                         {:current-time current} required
                         {:default-tz "UTC"})
                        [:current-time :default-tz])))
    ;; Ordinary calls without progression retain documented opts precedence.
    (is (= later
           (:current-time (xtdb-live/read-query-opts
                           {:current-time current} nil
                           {:current-time later}))))
    (is (= "Asia/Tokyo"
           (:default-tz (xtdb-live/read-query-opts
                         {:default-tz "UTC"} nil
                         {:default-tz "Asia/Tokyo"}))))))

(deftest progression-read-opts-preserve-both-consistency-and-caller-constraints-test
  ;; A plain query allows caller opts to override convenience consistency.
  ;; Under authoritative progression, that merge must not discard an
  ;; independently supplied time or an unrelated database's snapshot basis.
  (let [frontier (Instant/parse "2026-11-20T18:00:00Z")
        earlier (.minusSeconds frontier 60)
        later (.plusSeconds frontier 60)
        archive (.minusSeconds frontier 120)
        reports (.minusSeconds frontier 180)
        required (progression/requirement
                  (xtdb-live/basis :analytics 2025 frontier))
        token (fn [m] (xt.basis/->time-basis-str m))
        consistency-token (token {"analytics" [earlier]
                                  "archive" [archive]})
        caller-token (token {"analytics" [earlier]
                             "reports" [reports]})
        expected {"analytics" [frontier]
                  "archive" [archive]
                  "reports" [reports]}
        ctx {:xtdb/read-connectable :read
             :gesso.live/progression required}]
    (let [result (xtdb-live/read-query-opts
                  {:snapshot-token consistency-token}
                  required
                  {:snapshot-token caller-token})]
      (is (= expected (xt.basis/<-time-basis-str (:snapshot-token result)))
          "Both distinct databases must retain their independent snapshots"))
    ;; The consistency source may be stronger than the explicit query option.
    ;; Checking only the result of merge silently downgrades it.
    (doseq [[consistency opts expected-reason]
            [[{:snapshot-time later} {:snapshot-time earlier}
              :explicit-snapshot-ahead-of-progression]
             [{:snapshot-token (token {"analytics" [later]})}
              {:snapshot-token caller-token}
              :explicit-snapshot-token-ahead-of-progression]
             [{:snapshot-token "malformed-consistency-token"}
              {:snapshot-token caller-token}
              :invalid-snapshot-token]
             [{:snapshot-token consistency-token}
              {:snapshot-token (token {"archive" [later]})}
              :conflicting-independent-snapshot-bases]]]
      (let [failure (try (xtdb-live/read-query-opts consistency required opts)
                         nil
                         (catch clojure.lang.ExceptionInfo e e))]
        (is (instance? clojure.lang.ExceptionInfo failure)
            (str "Discarded consistency must fail closed: "
                 (pr-str [consistency opts])))
        (when (instance? clojure.lang.ExceptionInfo failure)
          (is (= expected-reason (:reason (ex-data failure)))))))
    ;; The request wrappers must reject conflicts before consulting XTDB.
    (let [calls (atom [])
          io (fn [& args] (swap! calls conj args) :unexpected)]
      (with-redefs-fn {(xtdb-var '*q*) io
                       (xtdb-var '*plan-q*) io}
        (fn []
          (doseq [invoke [#(xtdb-live/q-consistent-from
                             (assoc ctx :gesso.live/consistency
                                    {:snapshot-time later})
                             sample-query {:snapshot-time earlier})
                          #(xtdb-live/plan-q-consistent-from
                             (assoc ctx :gesso.live/consistency
                                    {:snapshot-token "malformed-consistency-token"})
                             sample-query {:snapshot-token caller-token})]]
            (is (thrown? clojure.lang.ExceptionInfo (invoke)))
            (is (empty? @calls) "Conflicting query constraints must block XTDB I/O"))
          (let [result (xtdb-live/q-consistent-from
                        (assoc ctx :gesso.live/consistency
                               {:snapshot-token consistency-token})
                        sample-query {:snapshot-token caller-token})]
            (is (= :unexpected result))
            (is (= expected
                   (xt.basis/<-time-basis-str
                    (:snapshot-token (nth (first @calls) 2)))))))))
    ;; Without an authoritative progression, the existing documented
    ;; ordinary query-option precedence remains unchanged.
    (is (= earlier
           (:snapshot-time (xtdb-live/read-query-opts
                            {:snapshot-time later} nil
                            {:snapshot-time earlier}))))))

(deftest read-progression-preserves-explicit-multi-db-snapshot-token-test
  ;; The caller's explicit XTDB snapshot-token is independently meaningful.
  ;; Installing the progression token must neither discard foreign database
  ;; bases nor overwrite a stronger selected-database time without rejection.
  (let [frontier (Instant/parse "2026-11-20T18:00:00Z")
        earlier (.minusSeconds frontier 60)
        later (.plusSeconds frontier 60)
        archive (.minusSeconds frontier 120)
        required-basis (xtdb-live/basis :analytics 2024 frontier)
        required (progression/requirement required-basis)
        context {:xtdb/read-connectable :read :gesso.live/progression required}
        opts-token (fn [times] (xt.basis/->time-basis-str
                                (cond-> {"archive" [archive]}
                                  times (assoc "analytics" times))))
        expected (xt.basis/->time-basis-str
                  {"archive" [archive] "analytics" [frontier]})]
    (doseq [token [(opts-token [earlier])
                   (opts-token nil)
                   (opts-token [earlier frontier])]]
      (let [actual (:snapshot-token
                    (xtdb-live/read-query-opts
                     {:snapshot-token token} required))]
        (is (= expected actual)
            "Composed query token must preserve the independent archive basis")
        (is (= {"archive" [archive] "analytics" [frontier]}
               (xt.basis/<-time-basis-str actual)))))
    (doseq [token [(opts-token [later])
                   (opts-token [earlier later])]]
      (let [outcome (try (xtdb-live/read-query-opts
                          {:snapshot-token token} required)
                         nil
                         (catch clojure.lang.ExceptionInfo e e))]
        (is (instance? clojure.lang.ExceptionInfo outcome)
            "Later explicit token cannot be replaced by an older progression")
        (when (instance? clojure.lang.ExceptionInfo outcome)
          (is (= :explicit-snapshot-token-ahead-of-progression
                 (:reason (ex-data outcome)))))))
    (doseq [invalid-token [false "not-a-valid-xtdb-token"]]
      (let [outcome (try (xtdb-live/read-query-opts
                          {:snapshot-token invalid-token} required)
                         nil
                         (catch clojure.lang.ExceptionInfo e e))]
        (is (instance? clojure.lang.ExceptionInfo outcome)
            "A malformed explicit snapshot token must not disappear")
        (when (instance? clojure.lang.ExceptionInfo outcome)
          (is (= :invalid-snapshot-token (:reason (ex-data outcome)))))))
    (let [calls (atom [])
          attempted (fn [& args]
                      (swap! calls conj args)
                      :unexpected)]
      (with-redefs-fn {(xtdb-var '*q*) attempted
                       (xtdb-var '*plan-q*) attempted}
        (fn []
          (doseq [bad [(opts-token [later])
                       "not-a-valid-xtdb-token"]]
            (doseq [invoke [#(xtdb-live/q-consistent-from
                               context sample-query {:snapshot-token bad})
                            #(xtdb-live/plan-q-consistent-from
                               context sample-query {:snapshot-token bad})]]
              (is (thrown? clojure.lang.ExceptionInfo (invoke)))
              (is (empty? @calls)
                  "An invalid explicit token must fail before XTDB I/O")))
          (let [result (xtdb-live/q-consistent-from
                        context sample-query {:snapshot-token (opts-token nil)})]
            (is (= :unexpected result))
            (is (= expected (:snapshot-token (nth (first @calls) 2))))))))))

(deftest read-progression-rejects-later-explicit-snapshot-time-before-io-test
  ;; A progression is a minimum authoritative read frontier. Installing its
  ;; snapshot token must not erase an explicit later XTDB snapshot-time: that
  ;; would silently downgrade a caller's already selected read constraint.
  (let [frontier-time (Instant/parse "2026-11-20T18:00:00Z")
        later (.plusSeconds frontier-time 120)
        older (.minusSeconds frontier-time 120)
        requirement (progression/requirement
                     (xtdb-live/basis :analytics 2023 frontier-time))
        context {:xtdb/read-connectable :request-connection
                 :gesso.live/progression requirement}
        actual-io (atom 0)
        unexpected (fn [& _] (swap! actual-io inc) :unexpected)]
    (doseq [[consistency opts] [[{:snapshot-time later} nil]
                                [nil {:snapshot-time later}]
                                [{:snapshot-time older}
                                 {:snapshot-time later}]]]
      (let [err (try (xtdb-live/read-query-opts consistency requirement opts)
                     nil
                     (catch clojure.lang.ExceptionInfo e e))]
        (is (instance? clojure.lang.ExceptionInfo err))
        (when (instance? clojure.lang.ExceptionInfo err)
          (is (= :explicit-snapshot-ahead-of-progression
                 (:reason (ex-data err))))
          (is (= later (:snapshot-time (ex-data err)))))))
    ;; XTDB also supplies java.time.ZonedDateTime at physical boundaries.
    ;; Compare the actual instant, not timezone-sensitive text.
    (let [zoned-later (ZonedDateTime/ofInstant later
                                            (java.time.ZoneId/of "America/Los_Angeles"))
          err (try (xtdb-live/read-query-opts
                    {:snapshot-time zoned-later} requirement)
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
      (is (instance? clojure.lang.ExceptionInfo err))
      (when (instance? clojure.lang.ExceptionInfo err)
        (is (= :explicit-snapshot-ahead-of-progression
               (:reason (ex-data err))))))
    (let [err (try (xtdb-live/read-query-opts
                    {:snapshot-time false} requirement)
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
      (is (instance? clojure.lang.ExceptionInfo err))
      (when (instance? clojure.lang.ExceptionInfo err)
        (is (= :invalid-explicit-snapshot-time
               (:reason (ex-data err))))))
    (with-redefs-fn
      {(xtdb-var '*q*) unexpected
       (xtdb-var '*plan-q*) unexpected}
      (fn []
        (doseq [invoke [#(xtdb-live/q-consistent-from
                           (assoc context :gesso.live/consistency
                                  {:snapshot-time later}) sample-query)
                        #(xtdb-live/q-consistent-from
                           context sample-query {:snapshot-time later})
                        #(xtdb-live/plan-q-consistent-from
                           context sample-query {:snapshot-time later})]]
          (let [err (try (invoke) nil
                         (catch clojure.lang.ExceptionInfo e e))]
            (is (instance? clojure.lang.ExceptionInfo err))
            (when (instance? clojure.lang.ExceptionInfo err)
              (is (= :explicit-snapshot-ahead-of-progression
                     (:reason (ex-data err))))))
          (is (zero? @actual-io)
              "A stronger explicit snapshot must never be downgraded and queried"))))
    ;; An older, equal, or absent time may still be advanced to the
    ;; authoritative frontier by the existing progression query translation.
    (doseq [requested [nil older frontier-time]]
      (is (= (xtdb-live/basis-snapshot-token
              (xtdb-live/basis :analytics 2023 frontier-time))
             (:snapshot-token
              (xtdb-live/read-query-opts
               (when requested {:snapshot-time requested})
               requirement)))))))

(deftest progression-from-composes-canonical-and-convenience-context-test
  (let [t1 (Instant/parse "2026-01-01T00:00:00Z")
        t2 (Instant/parse "2026-01-02T00:00:00Z")
        canonical (progression/requirement (xtdb-live/basis 70 t1))
        convenience (progression/requirement (xtdb-live/basis 71 t2))]
    (is (= (progression/compose canonical convenience)
           (xtdb-live/progression-from
            {:gesso.live/progression canonical
             :progression convenience})))
    (is (= convenience
           (xtdb-live/progression-from
            {:progression convenience})))
    (is (nil? (xtdb-live/progression-from :not-a-context)))))


(deftest progression-from-preserves-both-context-requirements-test
  ;; Both keys are valid separately, and either can be introduced by a
  ;; different request wrapper. Dropping the legacy minimum when the
  ;; canonical minimum is present can permit a stale read or Biff binding.
  (let [earlier (Instant/parse "2026-11-20T18:00:00Z")
        later (.plusSeconds earlier 60)
        canonical (progression/requirement
                   (xtdb-live/basis :xtdb 2044 earlier))
        legacy (progression/requirement
                (xtdb-live/basis :xtdb 2045 later))
        ctx {:xtdb/node :authority-node
             :xtdb/read-connectable :read
             :gesso.live/progression canonical
             :progression legacy}
        expected (progression/compose canonical legacy)
        latest-token (xtdb-live/basis-snapshot-token
                      (xtdb-live/basis :xtdb 2045 later))
        observed (atom [])
        record-call (fn [_ _ opts]
                      (swap! observed conj opts)
                      :queried)]
    (is (= expected (xtdb-live/progression-from ctx))
        "Independent context requirements must never be discarded")
    (with-redefs-fn {(xtdb-var '*q*) record-call
                     (xtdb-var '*plan-q*) record-call}
      (fn []
        (is (= :queried (xtdb-live/q-consistent-from ctx sample-query)))
        (is (= :queried (xtdb-live/plan-q-consistent-from ctx sample-query)))))
    (is (= [latest-token latest-token]
           (mapv :snapshot-token @observed))
        "Both physical query entry points must honor the stronger minimum")
    (with-xtdb-stub '*status*
      (fn [_] {:latest-completed-txs
               {"xtdb" [{:tx-id 2044 :system-time earlier}]}})
      (fn []
        (is (thrown? clojure.lang.ExceptionInfo
                     (xtdb-live/bind-request-frontier ctx))
            "An observed older frontier cannot satisfy the discarded requirement")))
    ;; An invalid secondary requirement must not be masked by a valid first.
    (let [bad (assoc ctx :progression false)
          before (count @observed)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (xtdb-live/progression-from bad)))
      (with-redefs-fn {(xtdb-var '*q*) record-call}
        (fn []
          (is (thrown? clojure.lang.ExceptionInfo
                       (xtdb-live/q-consistent-from bad sample-query))
              "Malformed secondary requirement must fail before XTDB I/O")))
      (is (= before (count @observed))
          "Malformed deferred constraint may not reach XTDB I/O"))
    (is (= canonical
           (xtdb-live/progression-from
            {:gesso.live/progression canonical :progression nil})))
    (is (= legacy
           (xtdb-live/progression-from {:progression legacy})))
    (is (= canonical
           (xtdb-live/progression-from
            {:gesso.live/progression canonical :progression canonical})))))

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

(deftest conflicting-admitted-xtdb-node-identities-fail-before-status-test
  ;; The status frontier belongs to a particular XTDB node. A binding context
  ;; with distinct primary and Biff node objects must not advertise a
  ;; trustworthy snapshot from the first while exposing the second to Biff.
  ;; Reject the ambiguity at node selection, before any status observation.
  (let [primary (Object.)
        biff-node (Object.)
        status-calls (atom 0)
        conflicting {:xtdb/node primary :biff.xtdb/node biff-node}
        observed (Instant/parse "2026-11-20T23:55:00Z")]
    (with-xtdb-stub '*status*
      (fn [_]
        (swap! status-calls inc)
        {:latest-completed-txs
         {"xtdb" [{:tx-id 2040 :system-time observed}]}})
      (fn []
        (doseq [operation [#(xtdb-live/node-from conflicting)
                           #(xtdb-live/latest-completed-basis conflicting)
                           #(xtdb-live/bind-request-frontier conflicting)]]
          (let [failure (try (operation)
                             nil
                             (catch clojure.lang.ExceptionInfo e e))]
            (is (instance? clojure.lang.ExceptionInfo failure)
                "Two different explicit XTDB nodes must not share an admitted frontier")
            (when (instance? clojure.lang.ExceptionInfo failure)
              (is (= :conflicting-admitted-xtdb-nodes
                     (:reason (ex-data failure)))))
            (is (zero? @status-calls)
                "Conflicting node identities must fail before any XTDB status call")))
        ;; Both names may legitimately refer to the exact same node.
        (is (identical? primary
                        (xtdb-live/node-from
                         {:xtdb/node primary :biff.xtdb/node primary})))
        (is (identical? biff-node
                        (xtdb-live/node-from
                         {:xtdb/node nil :biff.xtdb/node biff-node})))
        (is (identical? primary
                        (xtdb-live/node-from {:xtdb/node primary})))))))

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

(deftest latest-completed-basis-rejects-malformed-status-tail-test
  ;; Only the first completed entry is selected as the node-observed basis,
  ;; but the remainder is still part of that same trusted status response.
  ;; Malformed trailing entries must not be ignored while claiming that the
  ;; observation establishes an authoritative request frontier.
  (let [observed (Instant/parse "2026-11-20T23:58:00Z")
        completed {:tx-id 2041 :system-time observed}
        malformed [nil false {}
                   {:tx-id 2040 :system-time nil}
                   {:tx-id nil :system-time observed}
                   {:tx-id "2040" :system-time observed}
                   {:tx-id 2040 :system-time "not-an-instant"}]
        status-calls (atom 0)
        ctx {:xtdb/node :authority-node}]
    (doseq [trailing malformed]
      (let [status {:latest-completed-txs {"xtdb" [completed trailing]}}]
        (with-xtdb-stub '*status*
          (fn [_] (swap! status-calls inc) status)
          (fn []
            (doseq [invoke [#(xtdb-live/latest-completed-basis ctx)
                            #(xtdb-live/bind-request-frontier ctx)]]
              (let [failure (try (invoke)
                                 nil
                                 (catch clojure.lang.ExceptionInfo e e))]
                (is (instance? clojure.lang.ExceptionInfo failure)
                    (str "Malformed completed transaction tail must fail: "
                         (pr-str trailing)))
                (when failure
                  (is (= :malformed-completed-transaction-tail
                         (:reason (ex-data failure)))))))))))
    (is (= (* 2 (count malformed)) @status-calls))
    ;; Multiple individually valid entries are not prohibited by this check.
    (with-xtdb-stub '*status*
      (fn [_] {:latest-completed-txs
               {"xtdb" [completed
                        {:tx-id 2040 :system-time (.minusSeconds observed 1)}]}})
      (fn []
        (is (= (xtdb-live/basis :xtdb 2041 observed)
               (xtdb-live/latest-completed-basis ctx)))))))

(deftest latest-completed-basis-rejects-contradictory-status-frontier-test
  ;; The first completed key is advertised as the latest transaction for the
  ;; selected database. Other well-formed keys in that same observation must
  ;; not contradict it by claiming a newer tx id, later snapshot time, or a
  ;; different time for the exact same tx id. Otherwise Biff can be pinned to
  ;; a frontier already contradicted by the status response itself.
  (let [observed (Instant/parse "2026-11-20T23:58:00Z")
        latest {:tx-id 2041 :system-time observed}
        contradictory [{:tx-id 2042 :system-time observed}
                       {:tx-id 2040 :system-time (.plusSeconds observed 1)}
                       {:tx-id 2041 :system-time (.minusSeconds observed 1)}
                       {:tx-id 2041 :system-time (.plusSeconds observed 1)}]
        ctx {:xtdb/node :authority-node}
        calls (atom 0)]
    (doseq [trailing contradictory]
      (with-xtdb-stub '*status*
        (fn [_]
          (swap! calls inc)
          {:latest-completed-txs {"xtdb" [latest trailing]}})
        (fn []
          (doseq [invoke [#(xtdb-live/latest-completed-basis ctx)
                          #(xtdb-live/bind-request-frontier ctx)]]
            (let [failure (try (invoke)
                               nil
                               (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo failure)
                  (str "Contradictory completed evidence must fail: "
                       (pr-str trailing)))
              (when failure
                (is (= :contradictory-completed-transaction-frontier
                       (:reason (ex-data failure))))))))))
    (is (= (* 2 (count contradictory)) @calls))
    ;; Earlier keys and a repeated identical key do not weaken the observed
    ;; frontier. The check should not reject otherwise valid status evidence.
    (with-xtdb-stub '*status*
      (fn [_]
        {:latest-completed-txs
         {"xtdb" [latest
                  {:tx-id 2040 :system-time (.minusSeconds observed 1)}
                  latest]}})
      (fn []
        (is (= (xtdb-live/basis :xtdb 2041 observed)
               (xtdb-live/latest-completed-basis ctx)))))))

(deftest latest-completed-basis-rejects-contradictory-historical-keys-test
  ;; v2042 compares each historical key with the selected latest frontier.
  ;; That alone cannot establish that two earlier keys agree with each other:
  ;; a repeated tx-id with two times, or a higher older tx-id with a smaller
  ;; time, is not one coherent selected-database status observation.
  (let [observed (Instant/parse "2026-11-20T23:58:00Z")
        latest {:tx-id 2043 :system-time observed}
        tx41 {:tx-id 2041 :system-time (.minusSeconds observed 4)}
        tx42 {:tx-id 2042 :system-time (.minusSeconds observed 2)}
        conflicting-duplicate {:tx-id 2041
                               :system-time (.minusSeconds observed 3)}
        inverted-tx42 {:tx-id 2042
                       :system-time (.minusSeconds observed 6)}
        contradictory-tails [[tx41 conflicting-duplicate]
                             [conflicting-duplicate tx41]
                             [tx41 inverted-tx42]
                             [inverted-tx42 tx41]]
        ctx {:xtdb/node :authority-node}
        calls (atom 0)]
    (doseq [tail contradictory-tails]
      (with-xtdb-stub '*status*
        (fn [_]
          (swap! calls inc)
          {:latest-completed-txs {"xtdb" (into [latest] tail)}})
        (fn []
          (doseq [invoke [#(xtdb-live/latest-completed-basis ctx)
                          #(xtdb-live/bind-request-frontier ctx)]]
            (let [failure (try (invoke)
                               nil
                               (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo failure)
                  (str "Conflicting historical evidence must fail: "
                       (pr-str tail)))
              (when failure
                (is (= :contradictory-completed-transaction-history
                       (:reason (ex-data failure))))))))))
    (is (= (* 2 (count contradictory-tails)) @calls))
    ;; Historical keys need not arrive sorted, and an exact duplicate is
    ;; valid. Neither should be mistaken for contradictory evidence.
    (with-xtdb-stub '*status*
      (fn [_]
        {:latest-completed-txs
         {"xtdb" [latest tx42 tx41 tx42]}})
      (fn []
        (is (= (xtdb-live/basis :xtdb 2043 observed)
               (xtdb-live/latest-completed-basis ctx)))
        (is (= (xtdb-live/basis :xtdb 2043 observed)
               (-> (xtdb-live/bind-request-frontier ctx)
                   :gesso.live/progression
                   progression/required-bases
                   first)))))))

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
        (let [bound (xtdb-live/bind-request-frontier ctx)]
          (is (ifn? (:biff.core/wrap-db-snapshot bound)))
          (is (nil? (:biff.xtdb/snapshot-token bound)))
          (is (nil? (:gesso.live/progression bound))))))))

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
        (let [bound (xtdb-live/bind-request-frontier ctx)]
          (is (ifn? (:biff.core/wrap-db-snapshot bound))
              "Even empty startup guards fresh requests")
          (is (nil? (:biff.xtdb/snapshot-token bound))))))))

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
              (do
                (is (ifn? (:biff.core/wrap-db-snapshot bound)))
                (is (= #{foreign}
                       (progression/required-bases
                        (:gesso.live/progression bound))))))))))))

(deftest bind-request-frontier-rejects-database-without-snapshot-time-test
  ;; The XTDB basis codec accepts and roundtrips a database entry with zero
  ;; snapshot coordinates, but that is not a pinned database snapshot.
  ;; Neither populated nor fresh selected databases should carry it onward.
  (let [t (Instant/parse "2026-11-20T18:00:00Z")
        statuses [{:latest-completed-txs
                   {"xtdb" [{:tx-id 2020 :system-time t}]}}
                  {:latest-completed-txs {"xtdb" []}}]
        ctx {:xtdb/node :authority-node :request/id :missing-foreign-time}]
    (doseq [db ["analytics" "archive"]
            :let [token (xt.basis/->time-basis-str
                         {db [] "reports" [t]})]]
      (is (= {db [] "reports" [t]}
             (xt.basis/<-time-basis-str token)))
      (is (= token
             (xt.basis/->time-basis-str
              (xt.basis/<-time-basis-str token)))
          "XTDB's canonical codec does not enforce a nonempty time basis")
      (doseq [status statuses]
        (with-xtdb-stub
          '*status*
          (fn [_] status)
          (fn []
            (let [outcome (try
                            (xtdb-live/bind-request-frontier
                             (assoc ctx :biff.xtdb/snapshot-token token))
                            (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo outcome)
                  "Empty database time-basis entry must fail closed")
              (when (instance? clojure.lang.ExceptionInfo outcome)
                (is (= :invalid-snapshot-token (:reason (ex-data outcome))))
                (is (= token (:snapshot-token (ex-data outcome))))))))))
    ;; Valid nonempty vector entries are retained, including XTDB's more
    ;; general multiple-coordinate snapshot representation.
    (let [t2 (Instant/parse "2026-11-19T18:00:00Z")
          token (xt.basis/->time-basis-str {"analytics" [t t2]})]
      (with-xtdb-stub
        '*status*
        (fn [_] (first statuses))
        (fn []
          (let [bound (xtdb-live/bind-request-frontier
                       (assoc ctx :biff.xtdb/snapshot-token token))]
            (is (= [t t2]
                   (get (xt.basis/<-time-basis-str
                         (:biff.xtdb/snapshot-token bound)) "analytics")))))))))

(deftest bind-request-frontier-rejects-blank-database-ids-in-biff-token-test
  ;; XTDB's protobuf time-basis codec can canonically roundtrip blank
  ;; database names. Canonical encoding alone cannot admit an authoritative
  ;; Biff snapshot: every database identity must satisfy Gesso's nonblank
  ;; database-name contract, including preserved foreign databases.
  (let [t (Instant/parse "2026-11-20T18:00:00Z")
        active {:latest-completed-txs
                {"xtdb" [{:tx-id 2019 :system-time t}]}}
        empty-db {:latest-completed-txs {"xtdb" []}}
        ctx {:xtdb/node :authority-node :request/id :invalid-foreign-db}]
    (doseq [invalid-database ["" " "]
            :let [token (xt.basis/->time-basis-str
                         {invalid-database [t]
                          "analytics" [t]})]]
      (is (= token
             (xt.basis/->time-basis-str
              (xt.basis/<-time-basis-str token)))
          "Malformed database identity is still a canonical XTDB token")
      (doseq [status [active empty-db]]
        (with-xtdb-stub
          '*status*
          (fn [_] status)
          (fn []
            (let [outcome (try
                            (xtdb-live/bind-request-frontier
                             (assoc ctx :biff.xtdb/snapshot-token token))
                            (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo outcome)
                  "Blank foreign database IDs must not survive Biff frontier binding")
              (when (instance? clojure.lang.ExceptionInfo outcome)
                (is (= :invalid-snapshot-token (:reason (ex-data outcome))))
                (is (= token (:snapshot-token (ex-data outcome))))))))))))

(deftest biff-and-query-consistency-reject-conflicting-foreign-snapshots-test
  ;; Biff and Gesso-aware XTDB queries may both carry a basis for the same
  ;; independent database. Composing Biff with Biff or query with query is
  ;; insufficient: their cross-path intersection must agree as well.
  (let [now (Instant/parse "2026-11-20T21:00:00Z")
        later (.plusSeconds now 60)
        biff-token (xt.basis/->time-basis-str {"analytics" [now]})
        conflicting (xt.basis/->time-basis-str {"analytics" [later]})
        matching (xt.basis/->time-basis-str {"analytics" [now]})
        disjoint (xt.basis/->time-basis-str {"reports" [later]})
        calls (atom 0)
        base {:xtdb/node :authority-node
              :biff.xtdb/snapshot-token biff-token}
        body (fn [request]
               (swap! calls inc)
               (xtdb-live/consistency-from request))]
    (doseq [populated? [true false]]
      (with-xtdb-stub '*status*
        (fn [_] {:latest-completed-txs
                 {"xtdb" (if populated?
                           [{:tx-id 2051 :system-time now}]
                           [])}})
        (fn []
          (let [failure (try
                          (xtdb-live/bind-request-frontier
                           (assoc base :xtdb/consistency
                                  {:snapshot-token conflicting}))
                          nil
                          (catch clojure.lang.ExceptionInfo e e))]
            (is (instance? clojure.lang.ExceptionInfo failure)
                "Conflicting admitted Biff and query snapshots must fail")
            (when failure
              (is (= :conflicting-biff-query-snapshot-bases
                     (:reason (ex-data failure))))))
          (let [bound (xtdb-live/bind-request-frontier base)
                handler ((:biff.core/wrap-db-snapshot bound) body)
                before @calls
                failure (try
                          (handler {:gesso.live/consistency
                                    {:snapshot-token conflicting}})
                          nil
                          (catch clojure.lang.ExceptionInfo e e))]
            (is (instance? clojure.lang.ExceptionInfo failure)
                "Deferred conflicting query snapshot must fail")
            (when failure
              (is (= :conflicting-biff-query-snapshot-bases
                     (:reason (ex-data failure)))))
            (is (= before @calls)
                "A conflicting foreign snapshot must not reach the Biff body")
            ;; Equal foreign observations and separate database observations
            ;; remain independent, valid constraints on the same request.
            (is (= matching
                   (:snapshot-token
                    (handler {:xtdb/consistency
                              {:snapshot-token matching}}))))
            (is (= disjoint
                   (:snapshot-token
                    (handler {:xtdb/consistency
                              {:snapshot-token disjoint}}))))
            (is (= (+ 2 before) @calls))))))))

(deftest empty-frontier-validates-complete-tx-receipt-shapes-test
  ;; Empty selected databases cannot compare a proposed transaction receipt
  ;; with an observed frontier. That is not license to accept a structurally
  ;; impossible receipt: it must still be a portable signed-long tx id paired
  ;; with a java.time.Instant. A valid but unobserved startup receipt remains
  ;; permitted under the current empty-frontier contract.
  (let [t (Instant/parse "2026-11-20T18:00:00Z")
        bad [{:tx-id (inc (bigint Long/MAX_VALUE)) :system-time t}
             {:tx-id -1 :system-time t}
             {:tx-id "2056" :system-time t}
             {:tx-id 2056 :system-time "not-an-instant"}
             {:tx-id 2056 :snapshot-time :not-an-instant}]
        valid {:tx-id 2056 :system-time t}
        calls (atom 0)]
    (with-xtdb-stub '*status*
      (fn [_] {:latest-completed-txs {"xtdb" []}})
      (fn []
        (let [bound (xtdb-live/bind-request-frontier
                     {:xtdb/node :authority-node})
              wrapped ((:biff.core/wrap-db-snapshot bound)
                       (fn [_] (swap! calls inc) :executed))]
          (doseq [receipt bad]
            (let [admitted (try
                             (xtdb-live/bind-request-frontier
                              {:xtdb/node :authority-node
                               :gesso.live/consistency receipt})
                             nil
                             (catch clojure.lang.ExceptionInfo e e))
                  deferred (try
                             (wrapped {:xtdb/consistency receipt})
                             nil
                             (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo admitted)
                  (str "Invalid empty-startup tx receipt admitted: "
                       (pr-str receipt)))
              (is (instance? clojure.lang.ExceptionInfo deferred)
                  (str "Deferred invalid receipt entered Biff: "
                       (pr-str receipt)))))
          (is (zero? @calls)
              "Invalid complete receipts must fail before any handler call")
          ;; No completed frontier exists, so these structurally sound or
          ;; partial claims retain the existing empty-startup behavior.
          (is (ifn? (:biff.core/wrap-db-snapshot
                     (xtdb-live/bind-request-frontier
                      {:xtdb/node :authority-node
                       :gesso.live/consistency valid}))))
          (is (= :executed (wrapped {:gesso.live/consistency valid})))
          (is (= :executed
                 (wrapped {:xtdb/consistency {:tx-id 2056}})))
          (is (= 2 @calls)))))))

(deftest biff-frontier-rejects-unobserved-complete-tx-metadata-test
  ;; A complete tx/time pair is a request's claimed transaction minimum.
  ;; Without an observed node frontier at least as recent in BOTH coordinates,
  ;; fixing Biff reads to the observed snapshot silently drops that claim.
  ;; Query snapshot-time validation alone does not cover :system-time metadata.
  (let [observed (Instant/parse "2026-11-20T18:00:00Z")
        older (.minusSeconds observed 60)
        newer (.plusSeconds observed 60)
        invalid [{:tx-id 2055 :system-time older}
                 {:tx-id 2054 :system-time older}
                 {:tx-id 2053 :system-time newer}
                 {:tx-id 2054 :system-time newer}
                 {:tx-id 2055 :snapshot-time older}]
        valid [{:tx-id 2053 :system-time older}
               {:tx-id 2054 :system-time observed}
               {:tx-id 2053 :snapshot-time older}
               {:tx-id 2055}]
        called (atom 0)
        status {:latest-completed-txs
                {"xtdb" [{:tx-id 2054 :system-time observed}]}}]
    (with-xtdb-stub '*status*
      (fn [_] status)
      (fn []
        (doseq [receipt invalid]
          (let [failure (try
                          (xtdb-live/bind-request-frontier
                           {:xtdb/node :authority-node
                            :xtdb/consistency receipt})
                          nil
                          (catch clojure.lang.ExceptionInfo e e))]
            (is (instance? clojure.lang.ExceptionInfo failure)
                (str "Unobserved complete transaction must fail at admission: "
                     (pr-str receipt)))
            (when failure
              (is (= :unobserved-transaction-metadata-frontier
                     (:reason (ex-data failure)))))))
        (let [bound (xtdb-live/bind-request-frontier
                     {:xtdb/node :authority-node})
              handler ((:biff.core/wrap-db-snapshot bound)
                       (fn [_] (swap! called inc) :unexpected))]
          (doseq [receipt invalid]
            (let [failure (try
                            (handler {:gesso.live/consistency receipt})
                            nil
                            (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo failure)
                  (str "Unobserved deferred receipt must not enter Biff: "
                       (pr-str receipt)))
              (when failure
                (is (= :unobserved-transaction-metadata-frontier
                       (:reason (ex-data failure)))))))
          (is (zero? @called))
          (doseq [receipt valid]
            (is (ifn? (:biff.core/wrap-db-snapshot
                       (xtdb-live/bind-request-frontier
                        {:xtdb/node :authority-node
                         :gesso.live/consistency receipt}))))
            (is (= :unexpected
                   (handler {:xtdb/consistency receipt}))))
          (is (= (count valid) @called)))))
    ;; An empty selected database has no complete observed frontier. Keep
    ;; existing startup treatment of supplied metadata unchanged here.
    (with-xtdb-stub '*status*
      (fn [_] {:latest-completed-txs {"xtdb" []}})
      (fn []
        (is (ifn? (:biff.core/wrap-db-snapshot
                   (xtdb-live/bind-request-frontier
                    {:xtdb/node :authority-node
                     :xtdb/consistency
                     {:tx-id 2055 :system-time newer}}))))))))

(deftest biff-frontier-rejects-snapshot-token-behind-admitted-transaction-test
  ;; Transaction metadata is not itself authenticated. Nevertheless, if a
  ;; request claims a complete tx/time receipt it cannot simultaneously pin
  ;; the selected database to an earlier token and call that same request's
  ;; read obligations satisfied. A foreign database is not that transaction.
  (let [committed (Instant/parse "2026-11-20T18:00:00Z")
        old (.minusSeconds committed 30)
        observed (.plusSeconds committed 60)
        old-selected (xt.basis/->time-basis-str {"xtdb" [old]})
        same-selected (xt.basis/->time-basis-str {"xtdb" [committed]})
        old-foreign (xt.basis/->time-basis-str {"analytics" [old]})
        completed [{:tx-id 2053 :system-time observed}]
        calls (atom 0)]
    (with-xtdb-stub '*status*
      (fn [_] {:latest-completed-txs {"xtdb" completed}})
      (fn []
        (doseq [receipt [{:tx-id 2052 :system-time committed}
                         {:tx-id 2052 :snapshot-time committed}]]
          (let [full (assoc receipt :snapshot-token old-selected)
                failure (try
                          (xtdb-live/bind-request-frontier
                           {:xtdb/node :authority-node
                            :gesso.live/consistency full})
                          nil
                          (catch clojure.lang.ExceptionInfo e e))]
            (is (instance? clojure.lang.ExceptionInfo failure)
                "An admitted stale selected token cannot satisfy the tx")
            (when failure
              (is (= :snapshot-token-behind-transaction-metadata
                     (:reason (ex-data failure))))))
          ;; Deferred Ring contexts may add a token after the admission-time
          ;; receipt was preserved, and must be checked before the handler.
          (let [bound (xtdb-live/bind-request-frontier
                       {:xtdb/node :authority-node
                        :gesso.live/consistency receipt})
                handler ((:biff.core/wrap-db-snapshot bound)
                         (fn [ctx]
                           (swap! calls inc)
                           (xtdb-live/consistency-from ctx)))
                failure (try
                          (handler {:xtdb/consistency
                                    {:snapshot-token old-selected}})
                          nil
                          (catch clojure.lang.ExceptionInfo e e))]
            (is (instance? clojure.lang.ExceptionInfo failure)
                "Deferred stale selected token must fail before Biff body")
            (when failure
              (is (= :snapshot-token-behind-transaction-metadata
                     (:reason (ex-data failure))))))
          (is (zero? @calls)))
      ;; A token at the receipt time is compatible; an independent foreign
      ;; database's earlier time does not conflict with this transaction.
      (doseq [token [same-selected old-foreign]]
        (is (ifn? (:biff.core/wrap-db-snapshot
                   (xtdb-live/bind-request-frontier
                    {:xtdb/node :authority-node
                     :gesso.live/consistency
                     {:tx-id 2052 :system-time committed
                      :snapshot-token token}})))))
      ;; A tx-id without a system/snapshot time is only partial metadata;
      ;; it does not establish the transaction minimum used by this check.
      (is (ifn? (:biff.core/wrap-db-snapshot
                 (xtdb-live/bind-request-frontier
                  {:xtdb/node :authority-node
                   :gesso.live/consistency
                   {:tx-id 2052 :snapshot-token old-selected}}))))))))

(deftest bind-request-frontier-rejects-unsatisfied-admitted-query-consistency-test
  ;; Admitted consistency is an independent read obligation. Merely fixing
  ;; Biff's snapshot to the observed status is not sufficient if an admitted
  ;; snapshot-time or token demands a newer selected-database snapshot.
  ;; Otherwise ordinary Biff reads can proceed at a known insufficient time.
  (let [observed (Instant/parse "2026-11-20T18:00:00Z")
        future (.plusSeconds observed 60)
        earlier (.minusSeconds observed 60)
        status {:latest-completed-txs
                {"xtdb" [{:tx-id 2049 :system-time observed}]}}
        token (fn [t]
                (xt.basis/->time-basis-str {"xtdb" [t]}))
        forbidden [[{:snapshot-time future}
                    :explicit-snapshot-ahead-of-progression]
                   [{:snapshot-token (token future)}
                    :explicit-snapshot-token-ahead-of-progression]
                   [{:snapshot-token "malformed-token"}
                    :invalid-snapshot-token]]
        base {:xtdb/node :authority-node}]
    (with-xtdb-stub '*status*
      (fn [_] status)
      (fn []
        (doseq [[consistency reason] forbidden]
          (let [outcome (try
                          (xtdb-live/bind-request-frontier
                           (assoc base :gesso.live/consistency consistency))
                          nil
                          (catch clojure.lang.ExceptionInfo e e))]
            (is (instance? clojure.lang.ExceptionInfo outcome)
                (str "Admitted snapshot demand must not be forgotten: "
                     (pr-str consistency)))
            (when outcome
              (is (= reason (:reason (ex-data outcome)))))))
        ;; Earlier snapshots are satisfied by the server-observed frontier.
        (let [bound (xtdb-live/bind-request-frontier
                     (assoc base :xtdb/consistency
                            {:snapshot-time earlier}))]
          (is (= (xtdb-live/basis-snapshot-token
                  (xtdb-live/basis 2049 observed))
                 (:biff.xtdb/snapshot-token bound))))))))

(deftest deferred-biff-rejects-unsatisfied-query-consistency-test
  ;; A fresh Ring request or an outer Biff wrapper can add consistency after
  ;; frontier admission. A newer selected-database read requirement cannot be
  ;; silently retained while the installed Biff snapshot stays older.
  (let [observed (Instant/parse "2026-11-20T18:00:00Z")
        future (.plusSeconds observed 60)
        status {:latest-completed-txs
                {"xtdb" [{:tx-id 2049 :system-time observed}]}}
        token (xt.basis/->time-basis-str {"xtdb" [future]})
        candidates [[{:snapshot-time future}
                     :explicit-snapshot-ahead-of-progression]
                    [{:snapshot-token token}
                     :explicit-snapshot-token-ahead-of-progression]
                    [{:snapshot-token "malformed-token"}
                     :invalid-snapshot-token]]
        calls (atom 0)]
    (with-xtdb-stub '*status*
      (fn [_] status)
      (fn []
        (let [bound (xtdb-live/bind-request-frontier
                     {:xtdb/node :authority-node})
              wrapped ((:biff.core/wrap-db-snapshot bound)
                       (fn [_] (swap! calls inc) :unexpected))]
          (doseq [[consistency reason] candidates]
            (let [failure (try
                            (wrapped {:gesso.live/consistency consistency})
                            nil
                            (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo failure)
                  (str "Deferred request may not weaken Biff frontier: "
                       (pr-str consistency)))
              (when failure
                (is (= reason (:reason (ex-data failure)))))))
          (is (zero? @calls)
              "An unsatisfied read constraint must fail before the handler"))))))

(deftest bind-request-frontier-rejects-selected-snapshot-ahead-of-observed-frontier-test
  ;; The selected database's existing Biff token may have established a
  ;; later snapshot than this node currently reports. Replacing it with the
  ;; older observed time would silently weaken the request's read constraint.
  ;; Foreign database coordinates must remain independent.
  (let [observed (Instant/parse "2026-11-20T18:00:00Z")
        earlier (.minusSeconds observed 60)
        later (.plusSeconds observed 60)
        foreign (.minusSeconds observed 120)
        status {:latest-completed-txs
                {"xtdb" [{:tx-id 2021 :system-time observed}]}}
        ctx {:xtdb/node :authority-node :request/id :ahead-biff-snapshot}
        token-for (fn [times]
                    (xt.basis/->time-basis-str
                     {"xtdb" times "analytics" [foreign]}))]
    (with-xtdb-stub
      '*status*
      (fn [_] status)
      (fn []
        (doseq [times [[later] [earlier later]]]
          (let [token (token-for times)
                outcome (try
                          (xtdb-live/bind-request-frontier
                           (assoc ctx :biff.xtdb/snapshot-token token))
                          (catch clojure.lang.ExceptionInfo e e))]
            (is (instance? clojure.lang.ExceptionInfo outcome)
                "Never downgrade a selected-database snapshot ahead of status")
            (when (instance? clojure.lang.ExceptionInfo outcome)
              (is (= :selected-snapshot-ahead-of-frontier
                     (:reason (ex-data outcome))))
              (is (= token (:snapshot-token (ex-data outcome)))))))
        (doseq [times [[earlier] [earlier observed]]]
          (let [bound (xtdb-live/bind-request-frontier
                       (assoc ctx :biff.xtdb/snapshot-token
                              (token-for times)))]
            (is (= {"xtdb" [observed] "analytics" [foreign]}
                   (xt.basis/<-time-basis-str
                    (:biff.xtdb/snapshot-token bound))))))))))

(deftest bind-request-frontier-preserves-independent-biff-database-snapshots-test
  ;; A Biff snapshot token can represent more than one XTDB database. Binding
  ;; the current database's frontier must replace that database's coordinate
  ;; without erasing independently established time bases for other databases.
  (let [current (Instant/parse "2026-11-20T18:00:00Z")
        old (Instant/parse "2026-11-19T18:00:00Z")
        analytics (Instant/parse "2026-11-20T17:00:00Z")
        reports (Instant/parse "2026-11-20T16:00:00Z")
        foreign {"analytics" [analytics]
                 "reports" [reports]}
        foreign-token (xt.basis/->time-basis-str foreign)
        with-selected (xt.basis/->time-basis-str
                       (assoc foreign "xtdb" [old]))
        expected (xt.basis/->time-basis-str
                  (assoc foreign "xtdb" [current]))
        status {:latest-completed-txs
                {"xtdb" [{:tx-id 202 :system-time current}]
                 "analytics" [{:tx-id 89 :system-time analytics}]}}
        base {:xtdb/node :authority-node :request/id :multiple-databases}]
    (with-xtdb-stub
      '*status*
      (fn [_] status)
      (fn []
        (doseq [existing-token [foreign-token with-selected]]
          (let [bound (xtdb-live/bind-request-frontier
                       (assoc base :biff.xtdb/snapshot-token existing-token))
                wrapped ((:biff.core/wrap-db-snapshot bound)
                         (fn [ctx] (:biff.xtdb/snapshot-token ctx)))]
            (is (= expected (:biff.xtdb/snapshot-token bound)))
            (is (= expected (wrapped {}))
                "Biff's request wrapper must preserve the composed time basis")
            (is (= (assoc foreign "xtdb" [current])
                   (xt.basis/<-time-basis-str
                    (:biff.xtdb/snapshot-token bound))))))
        ;; Bind a non-default selected database without losing xtdb's basis.
        (let [default-token (xt.basis/->time-basis-str {"xtdb" [old]})
              bound (xtdb-live/bind-request-frontier
                     (assoc base :biff.xtdb/snapshot-token default-token)
                     :analytics)]
          (is (= {"xtdb" [old] "analytics" [analytics]}
                 (xt.basis/<-time-basis-str
                  (:biff.xtdb/snapshot-token bound)))))
        ;; Invalid existing tokens are not silently dropped just because the
        ;; selected database has a valid current frontier.
        (doseq [token [false "not-a-valid-token"]]
          (let [result (try
                         (xtdb-live/bind-request-frontier
                          (assoc base :biff.xtdb/snapshot-token token))
                         nil
                         (catch clojure.lang.ExceptionInfo e e))]
            (is (instance? clojure.lang.ExceptionInfo result))
            (when (instance? clojure.lang.ExceptionInfo result)
              (is (= token (:snapshot-token (ex-data result)))))))))))

(deftest biff-snapshot-wrappers-reject-invokable-data-test
  ;; Clojure collections and keywords implement IFn so `ifn?` alone does not
  ;; establish that a configured Biff middleware wrapper is an actual fn.
  ;; Invoking a map or keyword with the handler as its key simply returns nil,
  ;; silently bypassing the request and all of its read obligations.
  (let [time (Instant/parse "2026-11-20T22:00:00Z")
        invalid-wrappers [{} #{:handler} [:handler] :handler]
        invalid-bodies [{} #{:request} [:request] :request]
        body-calls (atom 0)]
    (doseq [populated? [true false]]
      (with-xtdb-stub '*status*
        (fn [_] {:latest-completed-txs
                 {"xtdb" (if populated?
                           [{:tx-id 2061 :system-time time}]
                           [])}})
        (fn []
          (doseq [wrapper invalid-wrappers]
            (let [failure (try
                            (xtdb-live/bind-request-frontier
                             {:xtdb/node :authority-node
                              :biff.core/wrap-db-snapshot wrapper})
                            nil
                            (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo failure)
                  (str "Invokable data cannot be a Biff wrapper: "
                       (pr-str wrapper)))
              (when (instance? clojure.lang.ExceptionInfo failure)
                (is (= wrapper (:wrapper (ex-data failure)))))))
          (let [bound (xtdb-live/bind-request-frontier
                       {:xtdb/node :authority-node})
                wrap (:biff.core/wrap-db-snapshot bound)]
            (doseq [body invalid-bodies]
              (let [failure (try (wrap body)
                                 nil
                                 (catch clojure.lang.ExceptionInfo e e))]
                (is (instance? clojure.lang.ExceptionInfo failure)
                    (str "Invokable data cannot be a Biff handler: "
                         (pr-str body)))
                (when (instance? clojure.lang.ExceptionInfo failure)
                  (is (= body (:body (ex-data failure)))))))
            (is (ifn? (wrap (fn [_] (swap! body-calls inc) :ok)))
                "Ordinary Biff middleware functions must remain supported")))))
    (is (zero? @body-calls)
        "Valid wrapped handlers have not yet been invoked")))

(deftest populated-frontier-rejects-noncallable-outer-biff-wrapper-test
  ;; A populated XTDB database is not permission to erase a configured Biff
  ;; wrapper. In particular, false is present but non-callable: the former
  ;; truthiness guard treated it as nil and silently skipped the wrapper.
  ;; Reject such a configuration at request-frontier binding, not after the
  ;; handler has been installed or invoked.
  (let [observed (Instant/parse "2026-11-20T22:00:00Z")
        ctx {:xtdb/node :authority-node}
        body-calls (atom 0)
        body (fn [request] (swap! body-calls inc) request)]
    (with-xtdb-stub '*status*
      (fn [_] {:latest-completed-txs
               {"xtdb" [{:tx-id 2034 :system-time observed}]}})
      (fn []
        (doseq [invalid [false 17 "not-a-wrapper"]]
          (let [error (try
                        (xtdb-live/bind-request-frontier
                         (assoc ctx :biff.core/wrap-db-snapshot invalid))
                        nil
                        (catch clojure.lang.ExceptionInfo e e))]
            (is (instance? clojure.lang.ExceptionInfo error)
                (str "Configured non-callable Biff wrapper must not vanish: "
                     (pr-str invalid)))
            (when (instance? clojure.lang.ExceptionInfo error)
              (is (= invalid (:wrapper (ex-data error)))))))
        (let [bound (xtdb-live/bind-request-frontier ctx)
              handler ((:biff.core/wrap-db-snapshot bound) body)]
          (is (string? (:biff.xtdb/snapshot-token (handler {})))
              "Absent wrapper still uses Gesso's fixed frontier")
          (is (= 1 @body-calls)))
        (let [outer-calls (atom 0)
              outer (fn [handler]
                      (swap! outer-calls inc)
                      (fn [request] (handler request)))
              bound (xtdb-live/bind-request-frontier
                     (assoc ctx :biff.core/wrap-db-snapshot outer))
              handler ((:biff.core/wrap-db-snapshot bound) body)]
          (is (map? (handler {})))
          (is (= 1 @outer-calls))
          (is (= 2 @body-calls)))))))

(deftest populated-frontier-guards-deferred-biff-wrapper-snapshots-test
  ;; A previous Biff wrapper can inject its own time-basis token after Gesso
  ;; binds an authoritative selected-database frontier. A fixed wrapper must
  ;; preserve independent coordinates and reject stronger selected-database
  ;; or contradictory foreign constraints, rather than erase them at runtime.
  (let [observed (Instant/parse "2026-11-20T20:00:00Z")
        earlier (.minusSeconds observed 60)
        later (.plusSeconds observed 60)
        archive (.minusSeconds observed 120)
        reports (.minusSeconds observed 180)
        bound-foreign {"archive" [archive]}
        initial-token (xt.basis/->time-basis-str bound-foreign)
        token (fn [bases] (xt.basis/->time-basis-str bases))
        expected (assoc bound-foreign "xtdb" [observed] "reports" [reports])
        base {:xtdb/node :authority-node :request/id :wrapped-frontier
              :biff.xtdb/snapshot-token initial-token}
        injected-wrapper (fn [injected]
                           (fn [body]
                             (fn [request]
                               (body (assoc request :biff.xtdb/snapshot-token injected)))))
        body-calls (atom 0)
        body (fn [request]
               (swap! body-calls inc)
               (:biff.xtdb/snapshot-token request))
        bind (fn [wrapper]
               (xtdb-live/bind-request-frontier
                (cond-> base wrapper
                  (assoc :biff.core/wrap-db-snapshot wrapper))))]
    (with-xtdb-stub '*status*
      (fn [_] {:latest-completed-txs {"xtdb" [{:tx-id 2031
                                              :system-time observed}]}})
      (fn []
        ;; A deferred wrapper's independent database must survive composition.
        (let [bound (bind (injected-wrapper (token {"reports" [reports]})))
              wrapped ((:biff.core/wrap-db-snapshot bound) body)]
          (is (= expected (xt.basis/<-time-basis-str (wrapped {}))))
          (is (= {"archive" [archive] "xtdb" [observed]}
                 (xt.basis/<-time-basis-str
                  (:biff.xtdb/snapshot-token bound)))))
        ;; Earlier selected-db snapshots may advance to the observed basis.
        (let [wrapped ((:biff.core/wrap-db-snapshot
                        (bind (injected-wrapper (token {"xtdb" [earlier]})))) body)]
          (is (= {"archive" [archive] "xtdb" [observed]}
                 (xt.basis/<-time-basis-str (wrapped {})))))
        (doseq [[injected expected-reason]
                [[(token {"xtdb" [later]})
                  :selected-snapshot-ahead-of-frontier]
                 [(token {"archive" [later]})
                  :conflicting-independent-snapshot-bases]
                 ["not-a-protobuf-token" :invalid-snapshot-token]
                 [false :invalid-snapshot-token]]]
          (let [wrapped ((:biff.core/wrap-db-snapshot
                          (bind (injected-wrapper injected))) body)
                before @body-calls
                outcome (try (wrapped {}) nil
                             (catch clojure.lang.ExceptionInfo e e))]
            (is (instance? clojure.lang.ExceptionInfo outcome)
                "Deferred unsafe Biff token must not be silently overwritten")
            (when (instance? clojure.lang.ExceptionInfo outcome)
              (is (= expected-reason (:reason (ex-data outcome)))))
            (is (= before @body-calls)
                "Unsafe tokens must be rejected before invoking the body")))
        ;; Even with no outer Biff wrapper, a request-supplied token must not
        ;; be lost when the already-bound wrapper is applied to that request.
        (let [wrapped ((:biff.core/wrap-db-snapshot (bind nil)) body)
              before @body-calls
              outcome (try (wrapped {:biff.xtdb/snapshot-token
                                    (token {"xtdb" [later]})})
                           nil
                           (catch clojure.lang.ExceptionInfo e e))]
          (is (instance? clojure.lang.ExceptionInfo outcome))
          (is (= :selected-snapshot-ahead-of-frontier
                 (:reason (ex-data outcome))))
          (is (= before @body-calls)))))))

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
        (let [bound (xtdb-live/bind-request-frontier ctx)]
          (is (ifn? (:biff.core/wrap-db-snapshot bound)))
          (is (= (progression/requirement required)
                 (:gesso.live/progression bound))))))))

(deftest empty-frontier-rejects-admitted-selected-query-snapshot-tokens-test
  ;; A fixed snapshot for a selected database with no completed transaction
  ;; cannot be admitted merely by putting it in query consistency instead of
  ;; the Biff snapshot key (which already rejects the identical claim).
  (let [t (Instant/parse "2026-11-20T19:00:00Z")
        selected (xtdb-live/basis :xtdb 2050 t)
        foreign (xtdb-live/basis :analytics 13 t)
        selected-token (xtdb-live/basis-snapshot-token selected)
        foreign-token (xtdb-live/basis-snapshot-token foreign)
        combined-token (xt.basis/->time-basis-str {"xtdb" [t]
                                                   "analytics" [t]})
        cases [[selected-token :unobserved-selected-database]
               [combined-token :unobserved-selected-database]
               ["not-a-token" :invalid-snapshot-token]
               [false :invalid-snapshot-token]]
        status-calls (atom 0)]
    (with-xtdb-stub '*status*
      (fn [_]
        (swap! status-calls inc)
        {:latest-completed-txs {"xtdb" []}})
      (fn []
        (doseq [[token expected-reason] cases]
          (let [failure (try
                          (xtdb-live/bind-request-frontier
                           {:xtdb/node :authority-node
                            :gesso.live/consistency {:snapshot-token token}})
                          nil
                          (catch clojure.lang.ExceptionInfo e e))]
            (is (instance? clojure.lang.ExceptionInfo failure)
                (str "Empty node cannot establish admitted selected snapshot "
                     (pr-str token)))
            (when failure
              (is (= expected-reason (:reason (ex-data failure)))))))
        ;; Foreign snapshots and waits remain possible; an as-of time is
        ;; not a fixed Biff snapshot receipt and retains its prior handling.
        (let [admitted {:snapshot-token foreign-token
                        :await-token "eventual-tx"}
              bound (xtdb-live/bind-request-frontier
                     {:xtdb/node :authority-node
                      :xtdb/consistency admitted})]
          (is (= admitted (xtdb-live/consistency-from bound)))
          (is (nil? (:biff.xtdb/snapshot-token bound)))
          (is (ifn? (:biff.core/wrap-db-snapshot bound))))
        (is (ifn? (:biff.core/wrap-db-snapshot
                   (xtdb-live/bind-request-frontier
                    {:xtdb/node :authority-node
                     :gesso.live/consistency {:snapshot-time t}}))))
        (is (= (+ 2 (count cases)) @status-calls))))))

(deftest empty-frontier-rejects-deferred-selected-query-snapshot-tokens-test
  ;; A Biff wrapper may see a fresh Ring context, so it must reject the same
  ;; selected-db token before running a handler even if it was not present
  ;; when the authoritative empty frontier was observed.
  (let [t (Instant/parse "2026-11-20T19:00:00Z")
        selected-token (xtdb-live/basis-snapshot-token
                        (xtdb-live/basis :xtdb 2050 t))
        foreign-token (xtdb-live/basis-snapshot-token
                       (xtdb-live/basis :analytics 13 t))
        cases [[selected-token :unobserved-selected-database]
               ["not-a-token" :invalid-snapshot-token]
               [false :invalid-snapshot-token]]
        calls (atom 0)]
    (with-xtdb-stub '*status*
      (fn [_] {:latest-completed-txs {"xtdb" []}})
      (fn []
        (let [bound (xtdb-live/bind-request-frontier
                     {:xtdb/node :authority-node
                      :gesso.live/consistency {:await-token "eventual-tx"}})
              handler ((:biff.core/wrap-db-snapshot bound)
                       (fn [ctx]
                         (swap! calls inc)
                         (xtdb-live/consistency-from ctx)))]
          (doseq [[token expected-reason] cases]
            (let [failure (try
                            (handler {:xtdb/consistency
                                      {:snapshot-token token}})
                            nil
                            (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo failure)
                  (str "Deferred selected-db token must fail: " (pr-str token)))
              (when failure
                (is (= expected-reason (:reason (ex-data failure))))))
            (is (zero? @calls) "Failure must precede the Biff body"))
          (is (= {:snapshot-token foreign-token
                  :await-token "eventual-tx"}
                 (handler {:xtdb/consistency
                           {:snapshot-token foreign-token}})))
          (is (= 1 @calls)))))))

(deftest empty-frontier-rejects-unverified-preexisting-biff-snapshot-test
  ;; No completed XTDB transaction means Gesso cannot establish a current
  ;; authoritative Biff snapshot. Returning ctx unchanged must not retain a
  ;; previously supplied snapshot token and allow ordinary Biff reads to act
  ;; on an unverified (possibly stale or foreign) time basis.
  (let [t (Instant/parse "2026-09-10T18:38:03Z")
        stale-token (xtdb-live/basis-snapshot-token
                     (xtdb-live/basis :xtdb 33 t))
        foreign-token (xtdb-live/basis-snapshot-token
                       (xtdb-live/basis :analytics 11 t))
        status {:latest-completed-txs {"xtdb" []}}
        observations (atom 0)
        base {:xtdb/node :authority-node :request/id :fresh}]
    (with-xtdb-stub
      '*status*
      (fn [_] (swap! observations inc) status)
      (fn []
        (doseq [token [stale-token "not-a-valid-token" false]]
          (let [ctx (assoc base :biff.xtdb/snapshot-token token)
                result (try (xtdb-live/bind-request-frontier ctx)
                            nil
                            (catch clojure.lang.ExceptionInfo e e))]
            (is (instance? clojure.lang.ExceptionInfo result)
                (str "Unobserved Biff snapshot must fail closed: " (pr-str token)))
            (when (instance? clojure.lang.ExceptionInfo result)
              (is (re-find #"preexisting Biff snapshot token" (.getMessage result)))
              (is (= "xtdb" (:database (ex-data result))))
              (is (= token (:snapshot-token (ex-data result)))))))
        ;; No token (including explicit nil) remains a legitimate startup.
        ;; Both now receive a guard against later fabricated runtime state.
        (is (ifn? (:biff.core/wrap-db-snapshot
                   (xtdb-live/bind-request-frontier base))))
        (let [without-token (assoc base :biff.xtdb/snapshot-token nil)
              bound (xtdb-live/bind-request-frontier without-token)]
          (is (ifn? (:biff.core/wrap-db-snapshot bound)))
          (is (nil? (:biff.xtdb/snapshot-token bound))))
        ;; A valid token for an independent database must not be rejected or
        ;; stripped simply because the selected default database is empty.
        (let [foreign (assoc base :biff.xtdb/snapshot-token foreign-token)
              bound (xtdb-live/bind-request-frontier foreign)]
          (is (= foreign-token (:biff.xtdb/snapshot-token bound)))
          (is (ifn? (:biff.core/wrap-db-snapshot bound))))
        (is (= 6 @observations))))))

(deftest empty-frontier-no-outer-wrapper-preserves-admitted-obligations-test
  ;; A Biff body can run on a fresh Ring request even when no outer Biff
  ;; wrapper was configured at binding. The existing empty-database path
  ;; returned ctx unchanged, allowing a pre-admitted foreign snapshot and
  ;; progression to vanish before the body, or a fabricated selected-db
  ;; snapshot/requirement to arrive without a guard.
  (let [t (Instant/parse "2026-11-20T23:00:00Z")
        archive-time (.minusSeconds t 60)
        selected (xtdb-live/basis :xtdb 2035 t)
        foreign (xtdb-live/basis :analytics 9 archive-time)
        foreign-token (xtdb-live/basis-snapshot-token foreign)
        selected-token (xtdb-live/basis-snapshot-token selected)
        foreign-requirement (progression/requirement foreign)
        selected-requirement (progression/requirement selected)
        calls (atom 0)
        body (fn [request]
               (swap! calls inc)
               {:snapshot (:biff.xtdb/snapshot-token request)
                :requirement (xtdb-live/progression-from request)})
        ctx {:xtdb/node :authority-node
             :biff.xtdb/snapshot-token foreign-token
             :gesso.live/progression foreign-requirement}]
    (with-xtdb-stub '*status*
      (fn [_] {:latest-completed-txs {"xtdb" []}})
      (fn []
        (let [bound (xtdb-live/bind-request-frontier ctx)
              outer (:biff.core/wrap-db-snapshot bound)
              ;; The fallback models how Biff runs a handler when Gesso did
              ;; not install a wrapper. It exposes the v2034 bug directly.
              handler ((or outer identity) body)
              result (handler {:request/id :fresh})]
          (is (ifn? outer)
              "Admitted obligations require a request-execution guard")
          (is (= foreign-token (:snapshot result))
              "An admitted foreign Biff snapshot must survive the fresh request")
          (is (= #{foreign}
                 (progression/required-bases (:requirement result)))
              "Admitted independent progression must reach the handler")
          (doseq [[request expected-reason]
                  [[{:biff.xtdb/snapshot-token selected-token}
                    :unobserved-selected-database]
                   [{:biff.xtdb/snapshot-token false}
                    :invalid-snapshot-token]
                   [{:gesso.live/progression selected-requirement} nil]
                   [{:gesso.live/progression false} nil]]]
            (let [before @calls
                  error (try (handler request)
                             nil
                             (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo error)
                  (str "Unsafe runtime constraint should fail closed: "
                       (pr-str request)))
              (when (and expected-reason
                         (instance? clojure.lang.ExceptionInfo error))
                (is (= expected-reason (:reason (ex-data error)))))
              (is (= before @calls)
                  "Rejected runtime constraints must never reach the body"))))
        ;; Either admitted source independently warrants the guard; do not
        ;; require both fields to be present to preserve either one.
        (doseq [partial [{:gesso.live/progression foreign-requirement}
                         {:biff.xtdb/snapshot-token foreign-token}]]
          (let [bound (xtdb-live/bind-request-frontier
                       (merge {:xtdb/node :authority-node} partial))]
            (is (ifn? (:biff.core/wrap-db-snapshot bound))
                "A single foreign constraint also requires a wrapper")))))))

(deftest deferred-biff-wrappers-preserve-authoritative-progression-test
  ;; The Biff handler may receive a new request map, not the context returned
  ;; by bind-request-frontier. The server-established progression must reach
  ;; that request, and later wrapper-supplied requirements cannot replace it.
  (let [time (Instant/parse "2026-11-20T21:00:00Z")
        archive-time (.minusSeconds time 60)
        selected (xtdb-live/basis :xtdb 2033 time)
        foreign (xtdb-live/basis :analytics 7 archive-time)
        foreign-requirement (progression/requirement foreign)
        stronger (xtdb-live/basis :xtdb 2034 (.plusSeconds time 60))
        stronger-requirement (progression/requirement stronger)
        other {:authority :unrelated-service :version "v1"}
        other-requirement (progression/requirement other)
        body-calls (atom 0)
        body (fn [request]
               (swap! body-calls inc)
               (xtdb-live/progression-from request))
        existing-wrapper (fn [handler]
                           (fn [request] (handler request)))
        base {:xtdb/node :authority-node
              :gesso.live/progression foreign-requirement
              :biff.core/wrap-db-snapshot existing-wrapper}]
    (doseq [completed? [true false]]
      (with-xtdb-stub '*status*
        (fn [_] {:latest-completed-txs
                 {"xtdb" (if completed?
                           [{:tx-id 2033 :system-time time}]
                           [])}})
        (fn []
          (let [bound (xtdb-live/bind-request-frontier base)
                wrapper (:biff.core/wrap-db-snapshot bound)
                handler (wrapper body)
                expected (if completed? #{selected foreign} #{foreign})]
            (is (= expected
                   (progression/required-bases (handler {:request/id :new-map})))
                "The handler must receive the admitted progression from the binding context")
            (is (= (conj expected other)
                   (progression/required-bases
                    (handler {:gesso.live/progression other-requirement})))
                "Independent runtime progression must compose with admitted requirements")
            (let [before @body-calls
                  error (try (handler {:gesso.live/progression stronger-requirement})
                             nil
                             (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo error)
                  "An unsatisfied runtime XTDB requirement cannot reach the body")
              (is (= before @body-calls)
                  "Reject unsupported runtime progression before handler execution"))
            (let [before @body-calls
                  error (try (handler {:gesso.live/progression false})
                             nil
                             (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo error)
                  "A malformed deferred progression cannot be discarded")
              (is (= before @body-calls)))))))))

(deftest empty-frontier-preserves-bound-and-deferred-foreign-snapshots-test
  ;; No selected-database completion means no basis for "xtdb". But a Biff
  ;; snapshot for an independent database already admitted at binding time is
  ;; a read constraint, not an optional hint: an outer wrapper must not erase
  ;; it when installing another token only during request execution.
  (let [time (Instant/parse "2026-11-20T20:45:00Z")
        older (.minusSeconds time 60)
        archive-time (.minusSeconds time 120)
        bound-token (xt.basis/->time-basis-str {"analytics" [time]})
        reports-token (xt.basis/->time-basis-str {"reports" [older]})
        same-token (xt.basis/->time-basis-str {"analytics" [time]})
        conflicting-token (xt.basis/->time-basis-str {"analytics" [archive-time]})
        selected-token (xt.basis/->time-basis-str {"xtdb" [time]})
        expected {"analytics" [time] "reports" [older]}
        body-calls (atom 0)
        body (fn [request]
               (swap! body-calls inc)
               (:biff.xtdb/snapshot-token request))
        outer (fn [token]
                (fn [handler]
                  (fn [request]
                    (handler (if (nil? token)
                               request
                               (assoc request :biff.xtdb/snapshot-token token))))))
        bind (fn [token]
               (xtdb-live/bind-request-frontier
                {:xtdb/node :authority-node
                 :biff.xtdb/snapshot-token bound-token
                 :biff.core/wrap-db-snapshot (outer token)}))]
    (with-xtdb-stub '*status*
      (fn [_] {:latest-completed-txs {"xtdb" []}})
      (fn []
        ;; A wrapper with no new token must retain the already-bound archive.
        (let [handler ((:biff.core/wrap-db-snapshot (bind nil)) body)]
          (is (= {"analytics" [time]}
                 (xt.basis/<-time-basis-str (handler {})))))
        ;; Distinct foreign snapshots should be composed, not selected by
        ;; whichever wrapper happens to execute last.
        (let [handler ((:biff.core/wrap-db-snapshot (bind reports-token)) body)]
          (is (= expected (xt.basis/<-time-basis-str (handler {})))))
        (let [handler ((:biff.core/wrap-db-snapshot (bind same-token)) body)]
          (is (= {"analytics" [time]}
                 (xt.basis/<-time-basis-str (handler {})))))
        ;; No basis exists to reconcile contradictory foreign snapshots, and
        ;; a selected-database snapshot remains fabricated at this boundary.
        (doseq [[token reason] [[conflicting-token :conflicting-independent-snapshot-bases]
                                [selected-token :unobserved-selected-database]
                                ["not-a-valid-token" :invalid-snapshot-token]]]
          (let [before @body-calls
                handler ((:biff.core/wrap-db-snapshot (bind token)) body)
                result (try (handler {}) nil
                            (catch clojure.lang.ExceptionInfo ex ex))]
            (is (instance? clojure.lang.ExceptionInfo result))
            (when (instance? clojure.lang.ExceptionInfo result)
              (is (= reason (:reason (ex-data result)))))
            (is (= before @body-calls)
                "Unsafe deferred snapshot must fail before request execution")))))))

(deftest empty-frontier-guards-existing-biff-wrapper-snapshots-test
  ;; Empty-database admission must continue to hold after a pre-existing Biff
  ;; wrapper runs. Checking only the token already in the binding context
  ;; misses a selected-database or malformed token installed by that wrapper.
  (let [time (Instant/parse "2026-11-20T20:30:00Z")
        selected-token (xtdb-live/basis-snapshot-token
                        (xtdb-live/basis :xtdb 1 time))
        foreign-token (xtdb-live/basis-snapshot-token
                       (xtdb-live/basis :analytics 1 time))
        called (atom 0)
        base {:xtdb/node :authority-node :request/id :new-database}
        wrap (fn [token]
               (fn [body]
                 (fn [request]
                   (swap! called inc)
                   (body (if (nil? token)
                           request
                           (assoc request :biff.xtdb/snapshot-token token))))))]
    (with-xtdb-stub '*status*
      (fn [_] {:latest-completed-txs {"xtdb" []}})
      (fn []
        (doseq [invalid [selected-token false "invalid-protobuf"]]
          (let [ctx (assoc base :biff.core/wrap-db-snapshot (wrap invalid))
                bound (xtdb-live/bind-request-frontier ctx)
                handler ((:biff.core/wrap-db-snapshot bound)
                         (fn [_] :unsafe-read))
                result (try (handler {:request/id :new-database})
                            nil
                            (catch clojure.lang.ExceptionInfo e e))]
            (is (instance? clojure.lang.ExceptionInfo result)
                (str "Biff wrapper must not inject an unobserved snapshot: "
                     (pr-str invalid)))
            (when (instance? clojure.lang.ExceptionInfo result)
              (is (= "xtdb" (:database (ex-data result))))
              (is (= invalid (:snapshot-token (ex-data result)))))))
        ;; Legitimate empty startup and independent database snapshots remain
        ;; usable; the existing Biff wrapper must still execute exactly once.
        (doseq [token [nil foreign-token]]
          (let [ctx (assoc base :biff.core/wrap-db-snapshot (wrap token))
                bound (xtdb-live/bind-request-frontier ctx)
                handler ((:biff.core/wrap-db-snapshot bound)
                         (fn [request]
                           (:biff.xtdb/snapshot-token request)))]
            (is (= token (handler {:request/id :new-database})))))
        (is (= 5 @called))))))

(deftest deferred-biff-requests-preserve-admitted-read-consistency-test
  ;; Unlike progression, explicit read consistency can require an XTDB wait.
  ;; A fresh Ring request must not discard the admitted :await-token, even
  ;; though the Biff snapshot and progression are already preserved.
  (let [observed (Instant/parse "2026-11-20T23:50:00Z")
        query-calls (atom [])
        body-calls (atom 0)
        body (fn [request]
               (swap! body-calls inc)
               (xtdb-live/q-consistent-from request sample-query))]
    (doseq [completed? [true false]]
      (with-redefs-fn
        {(xtdb-var '*status*)
         (fn [_]
           {:latest-completed-txs
            {"xtdb" (if completed?
                      [{:tx-id 2039 :system-time observed}]
                      [])}})
         (xtdb-var '*q*)
         (fn [connectable _query opts]
           (swap! query-calls conj [connectable opts])
           :queried)}
        (fn []
          (let [bound (xtdb-live/bind-request-frontier
                       {:xtdb/node :authority-node
                        :gesso.live/consistency
                        {:await-token "admitted-await"}})
                handler ((:biff.core/wrap-db-snapshot bound) body)]
            (is (= :queried (handler {:request/id :fresh})))
            (is (= "admitted-await" (:await-token (second (last @query-calls))))
                "A fresh request must still wait for the admitted transaction")
            (is (= :queried
                   (handler {:xtdb/consistency {:default-tz "UTC"}})))
            (is (= {:await-token "admitted-await" :default-tz "UTC"}
                   (select-keys (second (last @query-calls))
                                [:await-token :default-tz]))
                "Independent runtime consistency fields should compose")
            (is (= :queried
                   (handler {:gesso.live/consistency
                             {:await-token "admitted-await"}})))
            (is (= "admitted-await" (:await-token (second (last @query-calls)))))
            (doseq [request [{:gesso.live/consistency
                              {:await-token "different-await"}}
                             {:consistency {:await-token "different-await"}}
                             {:gesso.live/consistency false}]]
              (let [before @body-calls
                    prior-q (count @query-calls)
                    failure (try (handler request)
                                 nil
                                 (catch clojure.lang.ExceptionInfo e e))]
                (is (instance? clojure.lang.ExceptionInfo failure)
                    "A conflicting or malformed deferred wait must fail closed")
                (is (= before @body-calls)
                    "Conflicting consistency must fail before the handler")
                (is (= prior-q (count @query-calls))
                    "Conflicting consistency must fail before XTDB I/O")))))))))

(deftest deferred-biff-requests-retain-admitted-xtdb-node-for-reads-test
  ;; A freshly allocated Ring request may carry neither an XTDB node nor an
  ;; explicit read connectable. The frontier wrapper must make the admitted
  ;; node available to the handler so its snapshot/progression are usable,
  ;; without overriding a deliberately request-scoped read connection.
  (let [authority (Object.)
        observed (Instant/parse "2026-11-20T23:45:00Z")
        calls (atom [])
        body (fn [request]
               (xtdb-live/q-consistent-from request sample-query))]
    (doseq [completed? [true false]]
      (with-redefs-fn
        {(xtdb-var '*status*)
         (fn [node]
           (is (identical? authority node))
           {:latest-completed-txs
            {"xtdb" (if completed?
                      [{:tx-id 2038 :system-time observed}]
                      [])}})
         (xtdb-var '*q*)
         (fn [connectable _query opts]
           (swap! calls conj [connectable opts])
           :queried)}
        (fn []
          (let [bound (xtdb-live/bind-request-frontier
                       {:xtdb/node authority})
                wrapper (:biff.core/wrap-db-snapshot bound)
                handler (wrapper body)]
            (is (= :queried (handler {:request/id :fresh}))
                "A fresh handler request must still query the admitted node")
            (is (identical? authority (first (last @calls)))
                "The actual query must use the node that supplied status")
            (is (= :queried
                   (handler {:xtdb/read-connectable :request-read}))
                "An explicitly supplied read connection stays usable")
            (is (= :request-read (first (last @calls)))
                "The admitted fallback must not override request read preference")
            (is (= :queried (handler {:biff.xtdb/node authority}))
                "The matching legacy node key remains usable")
            (is (identical? authority (first (last @calls))))))))))

(deftest deferred-biff-requests-must-not-switch-authoritative-xtdb-node-test
  ;; The observed transaction and snapshot come from exactly one node. A
  ;; deferred outer Biff wrapper may send a different :xtdb/node or
  ;; :biff.xtdb/node to the handler; it must not inherit the first node's
  ;; authoritative frontier and then query a second node under that basis.
  (let [authority (Object.)
        other-node (Object.)
        t (Instant/parse "2026-11-20T23:40:00Z")
        body-calls (atom 0)
        query-calls (atom [])
        body (fn [request]
               (swap! body-calls inc)
               (xtdb-live/q-consistent-from request sample-query))]
    (doseq [completed? [true false]]
      (with-redefs-fn {(xtdb-var '*status*)
                       (fn [node]
                         (is (identical? authority node))
                         {:latest-completed-txs
                          {"xtdb" (if completed?
                                    [{:tx-id 2037 :system-time t}]
                                    [])}})
                       (xtdb-var '*q*)
                       (fn [connectable _query opts]
                         (swap! query-calls conj [connectable opts])
                         :queried)}
        (fn []
          (let [bound (xtdb-live/bind-request-frontier
                       {:xtdb/node authority})
                handler ((:biff.core/wrap-db-snapshot bound) body)]
            (is (= :queried (handler {:xtdb/node authority}))
                "The bound node remains an allowed request authority")
            (is (identical? authority (first (last @query-calls))))
            (doseq [key [:xtdb/node :biff.xtdb/node]]
              (let [before @body-calls
                    calls-before (count @query-calls)
                    result (try (handler {key other-node})
                                nil
                                (catch clojure.lang.ExceptionInfo e e))]
                (is (instance? clojure.lang.ExceptionInfo result)
                    "A different XTDB node cannot inherit the admitted frontier")
                (when (instance? clojure.lang.ExceptionInfo result)
                  (is (= :conflicting-runtime-xtdb-node
                         (:reason (ex-data result)))))
                (is (= before @body-calls)
                    "Conflicting node must fail before the handler")
                (is (= calls-before (count @query-calls))
                    "Conflicting node must fail before XTDB I/O")))))))))

(deftest empty-frontier-without-admitted-obligations-guards-later-request-test
  ;; The empty XTDB node cannot prove even its first completed transaction.
  ;; A request without an outer wrapper or admitted constraints nevertheless
  ;; needs a guard: a subsequent fresh Ring request can bring a selected-db
  ;; snapshot token or progression that was not present at binding time.
  (let [t (Instant/parse "2026-11-20T23:30:00Z")
        selected (xtdb-live/basis :xtdb 2036 t)
        foreign (xtdb-live/basis :analytics 7 t)
        selected-token (xtdb-live/basis-snapshot-token selected)
        foreign-token (xtdb-live/basis-snapshot-token foreign)
        selected-requirement (progression/requirement selected)
        body-calls (atom 0)
        body (fn [request]
               (swap! body-calls inc)
               request)
        ctx {:xtdb/node :authority-node :request/id :initial-empty}]
    (with-xtdb-stub '*status*
      (fn [_] {:latest-completed-txs {"xtdb" []}})
      (fn []
        (let [bound (xtdb-live/bind-request-frontier ctx)
              wrapper (:biff.core/wrap-db-snapshot bound)
              handler ((or wrapper identity) body)]
          (is (ifn? wrapper)
              "Empty startup must guard even when no obligation was admitted")
          (is (nil? (:biff.xtdb/snapshot-token bound))
              "No completed transaction means no manufactured snapshot")
          (is (nil? (:gesso.live/progression bound))
              "No completed transaction means no manufactured progression")
          (is (= {:request/id :ordinary :xtdb/node :authority-node}
                 (handler {:request/id :ordinary}))
              "An ordinary fresh empty request receives the admitted XTDB node")
          (is (= foreign-token
                 (:biff.xtdb/snapshot-token
                  (handler {:biff.xtdb/snapshot-token foreign-token})))
              "A real independent database token remains usable")
          (doseq [[request expected-reason]
                  [[{:biff.xtdb/snapshot-token selected-token}
                    :unobserved-selected-database]
                   [{:biff.xtdb/snapshot-token false}
                    :invalid-snapshot-token]
                   [{:gesso.live/progression selected-requirement} nil]
                   [{:gesso.live/progression false} nil]]]
            (let [before @body-calls
                  outcome (try (handler request)
                               nil
                               (catch clojure.lang.ExceptionInfo e e))]
              (is (instance? clojure.lang.ExceptionInfo outcome)
                  "A fabricated or malformed read obligation must fail closed")
              (when (and expected-reason
                         (instance? clojure.lang.ExceptionInfo outcome))
                (is (= expected-reason (:reason (ex-data outcome)))))
              (is (= before @body-calls)
                  "Unsafe runtime obligations must not reach the request body"))))))))

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
          (is (ifn? (:biff.core/wrap-db-snapshot bound))
              "Empty startup guards request execution without creating a basis")
          (is (nil? (:biff.xtdb/snapshot-token bound)))
          (is (nil? (:gesso.live/progression bound)))
          (is (= {:request/id :fresh :xtdb/node :authority-node}
                 (((:biff.core/wrap-db-snapshot bound)
                    (fn [request] request)) {:request/id :fresh}))))))))

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
