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
  (:import (java.time Duration Instant ZoneId ZonedDateTime)
           (java.util Arrays HexFormat)
           (org.apache.arrow.memory BufferAllocator)
           (xtdb TaggedValue)
           (xtdb.api.log Log Log$Record SourceMessage$Tx)
           (xtdb.api.query IKeyFn$KeyFn)
           (xtdb.arrow Relation VectorReader)
           (xtdb.database Database)
           (xtdb.util Iid MsgIdUtil)))

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

(defn- sql-argument-rows! [al bytes]
  (if (nil? bytes)
    [[]]
    (do
      (when-not (bytes? bytes) (reject! :unsupported-argument-bytes))
      (one-batch! al bytes
                  (fn [^Relation rel]
                    (let [columns (mapv (fn [i] (str "?_" i)) (range (count (.getVectors rel))))
                          n (.getRowCount rel)]
                      (when-not (and (pos? n)
                                     (= (set columns) (set (map #(.getName ^VectorReader %) (.getVectors rel)))))
                        (reject! :unsupported-argument-relation))
                      ;; PGWire coalesces adjacent equal statements, including
                      ;; parameterless statements. Preserve every row in order.
                      (mapv (fn [i]
                              (mapv (fn [column]
                                      (decode-value! (.getObject (.vectorFor rel column) i
                                                                 IKeyFn$KeyFn/KEBAB_CASE_KEYWORD))) columns))
                            (range n))))))))

(defn- document-value! [v]
  (cond
    (or (map? v) (instance? java.util.Map v))
    (into {} (map (fn [[k child]]
                    (when-not (keyword? k) (reject! :unsupported-document-key))
                    [k (document-value! child)])) v)
    (instance? java.util.List v) (mapv document-value! v)
    (instance? ZonedDateTime v) v
    (instance? Instant v) v
    :else (decode-value! v)))

(defn- iid-hex [id]
  (.formatHex (HexFormat/of) (Iid/getAsIid id)))

(defn- put-documents! [payload]
  ;; The logged table is a tagged union leg, not the caller's table alias.
  ;; Current API writes valid-time options into individual document rows;
  ;; retain outer bounds too if present instead of dropping them.
  (let [{:keys [iids documents]} payload]
    (when-not (and (every? #{:iids :documents :xt/valid-from :xt/valid-to} (keys payload))
                   (instance? TaggedValue documents) (instance? java.util.List iids))
      (reject! :unsupported-put-docs-payload))
    (let [table (.getTag ^TaggedValue documents)
          raw (.getValue ^TaggedValue documents)]
      (when-not (and (qualified-keyword? table) (instance? java.util.List raw)
                     (seq raw) (= (count raw) (count iids)))
        (reject! :unsupported-document-relation))
      (let [docs (mapv document-value! raw)]
        (doseq [[doc iid] (map vector docs iids)]
          (when-not (and (map? doc) (contains? doc :xt/id)
                         (bytes? iid) (= 16 (alength ^bytes iid))
                         (Arrays/equals ^bytes iid ^bytes (Iid/getAsIid (:xt/id doc))))
            (reject! :document-iid-mismatch)))
        {:kind :put-docs :table table :documents docs
         :iids (mapv #(.formatHex (HexFormat/of) ^bytes %) iids)
         :bounds (document-value! (select-keys payload [:xt/valid-from :xt/valid-to]))}))))

(defn observe-source-message!
  "Read one retained source message at an exact epoch-qualified tx id.
   Recover SQL text, ordered typed argument rows, and supported decoded
   document puts. Reject unsupported legs. This does NOT attest commit,
   writer identity, execution, or byte-identical caller input."
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
                    groups (one-batch! al (.getTxOps ^SourceMessage$Tx message)
                                    (fn [^Relation rel]
                                      (when-not (= ["tx-ops"] (mapv #(.getName ^VectorReader %) (.getVectors rel)))
                                        (reject! :unexpected-transaction-relation))
                                      (mapv (fn [row]
                                              (let [^TaggedValue op (:tx-ops row)]
                                                (when-not (instance? TaggedValue op) (reject! :unexpected-operation))
                                                (case (.getTag op)
                                                  :sql (let [{:keys [query args] :as payload} (.getValue op)]
                                                         (when-not (and (string? query)
                                                                        (every? #{:query :args} (keys payload)))
                                                           (reject! :invalid-sql))
                                                         (mapv (fn [row] {:kind :sql :query query :args row})
                                                               (sql-argument-rows! al args)))
                                                  :put-docs [(put-documents! (.getValue op))]
                                                  (reject! :unsupported-operation-kind))))
                                            (.toMaps rel IKeyFn$KeyFn/KEBAB_CASE_KEYWORD))))]
                {:status :source-message-observed :tx-id tx-id :operations (into [] cat groups)
                 :logged-operation-spans (mapv count groups)
                 :scope :retained-primary-source-log-decoded-sql-and-document-puts
                 :authority :none :certificate :not-issued}))))))))

(defn- expected-log-sql! [[sql & args]]
  ;; Only the generated mapped prefix fragment: all question marks are
  ;; bind markers, never quoted literals or identifier characters. This
  ;; is not a normalizer for arbitrary SQL.
  (let [n (atom 0)
        query (str/replace sql #"\?" (fn [_] (str "$" (swap! n inc))))]
    (when-not (= @n (count args)) (reject! :unsupported-placeholder-shape))
    {:kind :sql :query query :args (vec args)}))

(defn- expected-put [table docs]
  {:kind :put-docs :table table :documents docs
   :iids (mapv #(iid-hex (:xt/id %)) docs) :bounds {}})

(defn- expected-claim-operation! [op]
  ;; This is the fixture's restricted transaction language. Do not silently
  ;; erase temporal options, nulls, or unsupported table aliases to force a
  ;; match. General caller-envelope normalization remains unqualified.
  (cond
    (and (vector? op) (string? (first op))
         (re-find #"^ASSERT " (first op)))
    (expected-log-sql! op)

    (and (vector? op) (= :put-docs (first op))
         (simple-keyword? (second op))
         (re-matches #"[a-z][a-z0-9_]*" (name (second op)))
         (< 2 (count op)) (every? map? (drop 2 op)))
    (expected-put (keyword "public" (name (second op)))
                  (mapv document-value! (drop 2 op)))

    :else (reject! :unsupported-claim-operation)))

(defn check-claim-envelope!
  "Compare the complete restricted Claim submission with an exact retained
   log observation and its separate provider outcome. Diagnostic only."
  [node tx-id ops committed?]
  (let [expected (mapv expected-claim-operation! ops)
        observed (observe-source-message! node tx-id)
        outcomes (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" tx-id])]
    (when-not (= :source-message-observed (:status observed))
      (reject! :claim-source-message-unavailable))
    (when-not (= expected (:operations observed))
      (throw (ex-info "Claim source-log envelope differs from submitted operations."
                      {:reason :claim-envelope-mismatch :tx-id tx-id
                       :expected expected :observed (:operations observed)})))
    (when-not (= [{:committed committed?}] outcomes)
      (reject! :claim-transaction-outcome-mismatch))
    {:status :checked-decoded-claim-envelope
     :tx-id tx-id :committed? committed? :operation-count (count expected)
     :authority :none :certificate :not-issued}))

(deftest retained-source-log-distinguishes-guarded-unguarded-and-aborted-transactions
  (let [lookup #(deref (ns-resolve 'gesso.model.mapped-projection-test %))
        original (lookup 'source) targets (lookup 'targets) docs (lookup 'rows)
        uuid (java.util.UUID/fromString "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")
        journal [((lookup 'record) "old" "p2" {:values [1 :open "open" true uuid]})]
        prefix (frame/formatted-prestate-assertions! (plan/check-target-mapping! original targets) docs journal)
        expected (mapv expected-log-sql! prefix)
        marker [:put-docs :guard_probe {:xt/id "probe" :written true}]
        put (expected-put :public/guard_probe [(peek marker)])]
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
        (is (= (conj expected put) (:operations g)))
        ;; The envelope verifier must refuse an unguarded record even
        ;; when its committed marker matches the guarded transaction.
        (let [ops (conj prefix marker)
              reason (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
          (is (= :checked-decoded-claim-envelope
                 (:status (check-claim-envelope! node (:tx-id guarded) ops true))))
          (is (= :claim-envelope-mismatch
                 (reason #(check-claim-envelope! node (:tx-id unguarded) ops true))))
          (is (= :claim-envelope-mismatch
                 (reason #(check-claim-envelope! node (:tx-id guarded)
                            (conj prefix (assoc-in marker [2 :written] false)) true))))
          (is (= :claim-transaction-outcome-mismatch
                 (reason #(check-claim-envelope! node (:tx-id guarded) ops false))))
          (is (= :checked-decoded-claim-envelope
                 (:status (check-claim-envelope! node (:tx-id aborted) [["ASSERT FALSE"] marker] false))))
          (is (= :claim-transaction-outcome-mismatch
                 (reason #(check-claim-envelope! node (:tx-id aborted) [["ASSERT FALSE"] marker] true))))
          (is (= :claim-source-message-unavailable
                 (reason #(check-claim-envelope! node (+ 1000 (:tx-id aborted)) ops true)))))
        (is (= [put] (:operations u)))
        (is (= [{:kind :sql :query "ASSERT FALSE" :args []} put] (:operations a)))
        (is (true? (committed? (:tx-id guarded))))
        (is (true? (committed? (:tx-id unguarded))))
        (is (false? (committed? (:tx-id aborted))))
        (is (= :unavailable (:status (observe-source-message! node (+ 1000 (:tx-id aborted))))))
        (is (= :unsupported-log-identity
               (:reason (observe-source-message! node (MsgIdUtil/offsetToMsgId 1 0)))))))))

(deftest retained-document-puts-preserve-decoded-envelope-and-temporal-bounds
  (let [lookup #(deref (ns-resolve 'gesso.model.mapped-projection-test %))
        uuid (java.util.UUID/fromString "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")
        command ((lookup 'record) "c1" "p1" {:nested {:value :open :empty {} :list [1 "open" uuid]}})
        start (Instant/parse "2020-01-01T00:00:00Z")
        end (Instant/parse "2021-01-01T00:00:00Z")
        doc {:xt/id "a" :some/ns-key {:value :open :empty {}}}
        other (assoc doc :xt/id "b")
        with-bounds #(assoc % :xt/valid-from (.atZone start (ZoneId/of "UTC"))
                              :xt/valid-to (.atZone end (ZoneId/of "UTC")))
        puts [[:put-docs {:into :probe :valid-from start :valid-to end} doc other]
              [:put-docs :gesso_commands command]]
        expected [(expected-put :public/probe (mapv with-bounds [doc other]))
                  (expected-put :public/gesso_commands [command])]]
    (with-open [node (xtn/start-node {})]
      (let [observe (fn [ops] (observe-source-message! node (:tx-id (xt/execute-tx node ops))))
            baseline (observe puts)]
        (is (= :source-message-observed (:status baseline)))
        (is (= expected (:operations baseline)))
        (doseq [changed [(assoc-in puts [0 2 :some/ns-key :value] "open")
                         (assoc-in puts [0 2 :xt/id] "c")
                         (assoc-in puts [0 1 :into] :other_probe)
                         (assoc-in puts [0 1 :valid-to] (Instant/parse "2022-01-01T00:00:00Z"))
                         (assoc puts 0 (assoc (first puts) 2 other 3 doc))
                         (assoc-in puts [1 2 :gesso.command/outcome :result] :rejected)
                         (vec (reverse puts))]]
          (is (not= expected (:operations (observe changed)))))
        ;; Arrow map decoding omits explicit null fields. This observation
        ;; describes the provider's visible decoded rows, not caller bytes.
        (is (= expected (:operations (observe (assoc-in puts [0 2 :nullable] nil)))))
        (is (= :none (:authority baseline)))
        (is (= :not-issued (:certificate baseline)))))))

(deftest retained-source-log-expands-consecutive-sql-invocations-in-order
  (with-open [node (xtn/start-node {})]
    (let [ops [["ASSERT ? = ?" 1 1] ["ASSERT ? = ?" 2 2]
               ["ASSERT TRUE"] ["ASSERT TRUE"]
               ["ASSERT ? = ?" :open :open] ["ASSERT ? = ?" "open" "open"]
               [:put-docs :probe {:xt/id "a" :value 1}]]
          tx-id (:tx-id (xt/execute-tx node ops))
          observation (observe-source-message! node tx-id)
          reason (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
      (is (= [2 2 2 1] (:logged-operation-spans observation)))
      (is (= (mapv expected-claim-operation! ops) (:operations observation)))
      (is (= :checked-decoded-claim-envelope (:status (check-claim-envelope! node tx-id ops true))))
      (is (= :claim-envelope-mismatch
             (reason #(check-claim-envelope! node tx-id (assoc ops 0 (nth ops 1) 1 (first ops)) true))))
      (is (= :claim-envelope-mismatch
             (reason #(check-claim-envelope! node tx-id (vec (rest ops)) true)))))))

(defn -main [& args]
  (let [{:keys [fail error]}
        (if (= ["claim"] (vec args))
          (let [flag (requiring-resolve 'gesso.live.outbox.claim-physical-test/*claim-source-log-observer?*)]
            (with-bindings {flag true}
              (run-tests 'gesso.live.outbox.claim-physical-test)))
          (run-tests 'gesso.assurance.xtdb-source-log-pilot))]
    (shutdown-agents)
    (System/exit (if (zero? (+ fail error)) 0 1))))
