(ns gesso.assurance.xtdb-source-log-pilot
  "Opt-in probe of the installed XTDB 2.2.0-beta1 source log. Internal
   provider interfaces, not a production history classifier or receipt.
   Run with clojure -M:test -m gesso.assurance.xtdb-source-log-pilot."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests]]
            [gesso.model.mapped-frame :as frame]
            [gesso.model.certified-plan :as plan]
            [gesso.model.mapped-projection-test]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.time Duration)
           (org.apache.arrow.memory BufferAllocator)
           (xtdb TaggedValue)
           (xtdb.api.log Log Log$Record SourceMessage$Tx)
           (xtdb.api.query IKeyFn$KeyFn)
           (xtdb.arrow Relation VectorReader)
           (xtdb.database Database)
           (xtdb.util MsgIdUtil)))

(defn- reject! [reason]
  (throw (ex-info "Unsupported source log observation." {:reason reason})))

(defn- one-batch! [^BufferAllocator al ^bytes data f]
  (with-open [loader (Relation/streamLoader al data)
              rel (Relation. al (.getSchema loader))]
    (when-not (.loadNextPage loader rel) (reject! :missing-arrow-batch))
    (let [result (f rel)]
      (when (.loadNextPage loader rel) (reject! :multiple-arrow-batches))
      result)))

(defn- decode-value! [v]
  (cond
    (or (string? v) (keyword? v) (boolean? v) (integer? v) (uuid? v)) v
    (instance? java.util.List v) (mapv decode-value! v)
    :else (reject! :unsupported-sql-argument)))

(defn- sql-arguments! [al bytes]
  (if (nil? bytes)
    []
    (do
      (when-not (bytes? bytes) (reject! :unsupported-argument-bytes))
      (one-batch! al bytes
                  (fn [^Relation rel]
                    (let [columns (mapv (fn [i] (str "?_" i)) (range (count (.getVectors rel))))]
                      (when-not (and (= 1 (.getRowCount rel))
                                     (= (set columns) (set (map #(.getName ^VectorReader %) (.getVectors rel)))))
                        (reject! :unsupported-argument-relation))
                      (mapv (fn [column]
                              (decode-value! (.getObject (.vectorFor rel column) 0
                                                         IKeyFn$KeyFn/KEBAB_CASE_KEYWORD))) columns)))))))

(defn observe-source-message!
  "Read one retained source message at an exact epoch-qualified tx id.
   Recover SQL text and single-row typed parameters; record non-SQL legs
   only. This does NOT attest commit, writer identity or execution."
  [node tx-id]
  (let [^Database database (db/primary-db node)
        ^Log log (.getSourceLog database)]
    (if-not (and (integer? tx-id) (<= 0 tx-id) (< tx-id Long/MAX_VALUE)
                 (= (.getEpoch log) (MsgIdUtil/msgIdToEpoch tx-id)
                    (MsgIdUtil/msgIdToEpoch (inc tx-id))))
      {:status :unavailable :reason :unsupported-log-identity}
      (let [records (vec (iterator-seq (.iterator (.readRecords log tx-id (inc tx-id)))))]
        (cond
          (empty? records) {:status :unavailable :reason :source-message-not-retained}
          (not= 1 (count records)) (reject! :ambiguous-source-message)
          :else
          (let [^Log$Record record (first records)
                message (.getMessage record)]
            (when-not (= tx-id (.getMsgId record)) (reject! :source-message-identity-mismatch))
            (if-not (instance? SourceMessage$Tx message)
              {:status :unavailable :reason :not-a-transaction-message}
              (let [al (.getAllocator database)
                    ops (one-batch! al (.getTxOps ^SourceMessage$Tx message)
                                    (fn [^Relation rel]
                                      (when-not (= ["tx-ops"] (mapv #(.getName ^VectorReader %) (.getVectors rel)))
                                        (reject! :unexpected-transaction-relation))
                                      (mapv (fn [row]
                                              (let [^TaggedValue op (:tx-ops row)]
                                                (when-not (instance? TaggedValue op) (reject! :unexpected-operation))
                                                (if (= :sql (.getTag op))
                                                  (let [{:keys [query args]} (.getValue op)]
                                                    (when-not (string? query) (reject! :invalid-sql))
                                                    {:kind :sql :query query :args (sql-arguments! al args)})
                                                  {:kind (.getTag op)})))
                                            (.toMaps rel IKeyFn$KeyFn/KEBAB_CASE_KEYWORD))))]
                {:status :source-message-observed :tx-id tx-id :operations ops
                 :scope :retained-primary-source-log-sql-and-operation-kinds
                 :authority :none :certificate :not-issued}))))))))

(defn- expected-log-sql! [[sql & args]]
  ;; Only the generated mapped prefix fragment: all question marks are
  ;; bind markers, never quoted literals or identifier characters. This
  ;; is not a normalizer for arbitrary SQL.
  (let [n (atom 0)
        query (str/replace sql #"\?" (fn [_] (str "$" (swap! n inc))))]
    (when-not (= @n (count args)) (reject! :unsupported-placeholder-shape))
    {:kind :sql :query query :args (vec args)}))

(deftest retained-source-log-distinguishes-guarded-unguarded-and-aborted-transactions
  (let [lookup #(deref (ns-resolve 'gesso.model.mapped-projection-test %))
        original (lookup 'source) targets (lookup 'targets) docs (lookup 'rows)
        uuid (java.util.UUID/fromString "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")
        journal [((lookup 'record) "old" "p2" {:values [1 :open "open" true uuid]})]
        prefix (frame/formatted-prestate-assertions! (plan/check-target-mapping! original targets) docs journal)
        expected (mapv expected-log-sql! prefix)
        marker [:put-docs :guard_probe {:xt/id "probe" :written true}]]
    (with-open [node (xtn/start-node {})]
      (xt/execute-tx node [[:put-docs :requests (first (:requests docs))]
                           [:put-docs :gesso_commands (first journal)]])
      (let [guarded (xt/execute-tx node (conj prefix marker))
            unguarded (xt/execute-tx node [marker])
            ;; A log record records submission even when the provider aborts.
            aborted (xt/submit-tx node [["ASSERT FALSE"] marker])
            ^Database database (db/primary-db node)
            _ (.awaitTxBlocking database (:tx-id aborted) (Duration/ofSeconds 10))
            g (observe-source-message! node (:tx-id guarded))
            u (observe-source-message! node (:tx-id unguarded))
            a (observe-source-message! node (:tx-id aborted))
            committed? (fn [id] (:committed (first (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" id]))))]
        (doseq [[tx observed] [[guarded g] [unguarded u] [aborted a]]]
          (is (= :source-message-observed (:status observed)))
          (is (= (:tx-id tx) (:tx-id observed)))
          (is (= :none (:authority observed)))
          (is (= :not-issued (:certificate observed))))
        (is (= (conj expected {:kind :put-docs}) (:operations g)))
        (is (= [{:kind :put-docs}] (:operations u)))
        (is (= [{:kind :sql :query "ASSERT FALSE" :args []} {:kind :put-docs}] (:operations a)))
        (is (true? (committed? (:tx-id guarded))))
        (is (true? (committed? (:tx-id unguarded))))
        (is (false? (committed? (:tx-id aborted))))
        (is (= :unavailable (:status (observe-source-message! node (+ 1000 (:tx-id aborted))))))
        (is (= :unsupported-log-identity
               (:reason (observe-source-message! node (MsgIdUtil/offsetToMsgId 1 0)))))))))

(defn -main [& _]
  (let [{:keys [fail error]} (run-tests 'gesso.assurance.xtdb-source-log-pilot)]
    (shutdown-agents)
    (System/exit (if (zero? (+ fail error)) 0 1))))
