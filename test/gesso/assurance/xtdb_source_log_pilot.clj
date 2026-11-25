(ns gesso.assurance.xtdb-source-log-pilot
  "Opt-in probe of the installed XTDB 2.2.0-beta1 source log. Internal
   provider interfaces, not a production history classifier or receipt.
   Run with clojure -M:test -m gesso.assurance.xtdb-source-log-pilot."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests]]
            [gesso.assurance.xtdb-source-log :as source-log]
            [gesso.assurance.xtdb-envelope :as envelope]
            [gesso.model.mapped-frame :as frame]
            [gesso.model.certified-plan :as plan]
            [gesso.model.mapped-projection-test]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.time Duration Instant ZoneId ZonedDateTime)
           (java.util HexFormat)
           (xtdb.database Database)
           (xtdb.util Iid MsgIdUtil)))

(defn- reject! [reason]
  (throw (ex-info "Unsupported source log observation." {:reason reason})))

(defn- decode-value! [v]
  (cond
    (or (string? v) (keyword? v) (boolean? v) (integer? v) (uuid? v)) v
    (instance? java.util.List v) (mapv decode-value! v)
    :else (reject! :unsupported-sql-argument)))

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

(def observe-source-message! source-log/observe-source-message!)

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
  "Compatibility adapter for the existing opt-in regression callers."
  [node tx-id ops committed?]
  (let [checked (envelope/check-envelope! node tx-id ops committed?)]
    (case (:status checked)
      :matched (-> checked (dissoc :profile) (assoc :status :checked-decoded-claim-envelope))
      :mismatch (reject! (if (= :transaction-outcome-mismatch (:reason checked))
                           :claim-transaction-outcome-mismatch :claim-envelope-mismatch))
      (reject! :claim-source-message-unavailable))))

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

(deftest retained-local-source-log-survives-node-restart
  (let [root (java.nio.file.Files/createTempDirectory
              "gesso-v2574-source-log-" (make-array java.nio.file.attribute.FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        doc {:xt/id "probe" :value :open :nested {:items [1 "open" true]}}
        put [:put-docs :probe doc]
        guarded [["ASSERT ? = ?" 1 1] ["ASSERT ? = ?" 2 2] put]
        unguarded [put]
        aborted [["ASSERT FALSE"] [:put-docs :probe (assoc doc :value :aborted)]]
        check! (fn [node {:keys [tx-id ops committed? observation]}]
                 (is (= observation (observe-source-message! node tx-id)))
                 (is (= {:status :checked-decoded-claim-envelope :tx-id tx-id
                         :committed? committed? :operation-count (count ops)
                         :authority :none :certificate :not-issued}
                        (check-claim-envelope! node tx-id ops committed?))))]
    (try
      (let [originals
            (with-open [node (xtn/start-node config)]
              (let [g (xt/execute-tx node guarded)
                    u (xt/execute-tx node unguarded)
                    a (xt/submit-tx node aborted)
                    ^Database database (db/primary-db node)]
                (.awaitTxBlocking database (:tx-id a) (Duration/ofSeconds 10))
                (let [observations (mapv (fn [[tx ops committed?]]
                                           {:tx-id (:tx-id tx) :ops ops :committed? committed?
                                            :observation (observe-source-message! node (:tx-id tx))})
                                         [[g guarded true] [u unguarded true] [a aborted false]])]
                  (is (= 3 (count (set (map :tx-id observations)))))
                  (doseq [entry observations] (check! node entry))
                  (is (= [doc] (xt/q node "SELECT * FROM probe")))
                  observations)))
            later (assoc doc :value :later)]
        ;; Fresh node, same persistent source log and storage.  No retained
        ;; provider objects from the first node are used in the second.
        (with-open [node (xtn/start-node config)]
          ;; Wait for the reopened index to reach the last original tx
          ;; before preparing a schema-dependent query.
          (.awaitTxBlocking ^Database (db/primary-db node)
                            (apply max (map :tx-id originals)) (Duration/ofSeconds 10))
          (is (= [doc] (xt/q node "SELECT * FROM probe")))
          (doseq [entry originals] (check! node entry))
          (let [tx-id (:tx-id (xt/execute-tx node [[:put-docs :probe later]]))]
            (is (> tx-id (apply max (map :tx-id originals))))
            (is (= :checked-decoded-claim-envelope
                   (:status (check-claim-envelope! node tx-id [[:put-docs :probe later]] true))))
            (is (= [later] (xt/q node "SELECT * FROM probe")))
            ;; Subsequent writes do not substitute the current row for the
            ;; original logged operation, or change an aborted outcome.
            (doseq [entry originals] (check! node entry)))
          (is (= :unavailable (:status (observe-source-message! node (+ 1000000 (apply max (map :tx-id originals)))))))))
      (finally
        (doseq [entry (reverse (file-seq (.toFile root)))]
          (io/delete-file entry true))))))

(deftest source-log-observation-refuses-unsupported-and-bounded-inputs
  (with-open [node (xtn/start-node {})]
    (let [tx-id (:tx-id (xt/execute-tx node [["ASSERT ? = ?" 1 1]
                                            ["ASSERT ? = ?" 2 2]
                                            [:put-docs :probe {:xt/id "a" :nested {:items [1 2]}}]]))
          delete-id (:tx-id (xt/execute-tx node [[:delete-docs :probe "a"]]))
          unsupported-id (:tx-id (xt/execute-tx node [[:put-docs :probe {:xt/id "float" :value 2.0}]]))
          observe #(source-log/observe-source-message! node tx-id %)]
      (is (= :xtdb-2.2.0-beta1/decoded-source-log-v1 (:provider-profile (observe {}))))
      (doseq [[limits reason] [[{:max-message-bytes 1} :message-byte-limit]
                               [{:max-rows 1} :arrow-row-limit]
                               [{:max-values 1} :decoded-value-limit]
                               [{:max-depth 1} :decoded-depth-limit]
                               [{:max-rows 0} :invalid-observation-limits]
                               [{:max-message-bytes Long/MAX_VALUE} :invalid-observation-limits]
                               [{:other 1} :invalid-observation-limits]]]
        (is (= {:status :unsupported :reason reason} (observe limits))))
      ;; The public API lowers delete-docs into a SQL operation. Observe
      ;; that actual log representation, not the caller operation family.
      (is (= [{:kind :sql :query "DELETE FROM \"public\".\"probe\"  WHERE _id = $1" :args ["a"]}]
             (:operations (source-log/observe-source-message! node delete-id))))
      (is (= {:status :unsupported :reason :unsupported-sql-argument}
             (source-log/observe-source-message! node unsupported-id)))
      (is (= {:status :unsupported :reason :embedded-primary-log-required}
             (source-log/observe-source-message! nil tx-id)))
      (is (= {:status :unsupported :reason :unsupported-log-identity}
             (source-log/observe-source-message! node -1)))
      (is (= {:status :unavailable :reason :source-message-not-retained}
             (source-log/observe-source-message! node (+ tx-id 1000000))))
      (is (= :source-message-observed (:status (observe {})))))))

(deftest production-envelope-comparison-contract
  (with-open [node (xtn/start-node {})]
    (let [ops [["ASSERT ? = ?" :open :open] [:put-docs :probe {:xt/id "a" :value "secret"}]]
          id (:tx-id (xt/execute-tx node ops))
          check #(envelope/check-envelope! node id %1 %2)
          result (check ops true)]
      (is (= {:status :matched :profile envelope/profile :authority :none
              :certificate :not-issued :tx-id id :committed? true :operation-count 2} result))
      (is (= :transaction-outcome-mismatch (:reason (check ops false))))
      (is (= :operation-sequence-mismatch (:reason (check (subvec ops 1) true))))
      (is (= :operation-sequence-mismatch (:reason (check (assoc-in ops [1 2 :value] "other") true))))
      (doseq [[bad reason] [[[ ["ASSERT '?' = '?'" ]] :unsupported-assertion-text]
                            [[["ASSERT TRUE -- ?" 1]] :unsupported-assertion-text]
                            [[["ASSERT $1 = $1" 1]] :unsupported-assertion-text]
                            [[["ASSERT ? = ?" 1]] :unsupported-placeholder-shape]
                            [[[:put-docs {:into :probe} {:xt/id "a"}]] :unsupported-put-shape]
                            [[[:put-docs :probe {:xt/id "a" :value nil}]] :unsupported-expected-value]
                            [[[:put-docs :probe {:xt/id "a" :xt/valid-from (Instant/now)}]] :unsupported-document-shape]
                            [[[:delete-docs :probe "a"]] :unsupported-operation]]]
        (is (= {:status :unsupported :reason reason :profile envelope/profile
                :authority :none :certificate :not-issued} (check bad true))))
      (is (= :invalid-expected-outcome (:reason (check ops nil))))
      (is (= :unavailable (:status (envelope/check-envelope! node (+ id 1000000) ops true))))
      ;; A provider record or query result cannot silently change identity,
      ;; profile, or outcome. No raw SQL, parameters, or errors escape.
      (let [observed (source-log/observe-source-message! node id)]
        (doseq [[changed reason] [[(assoc observed :tx-id (inc id)) :source-transaction-identity-mismatch]
                                  [(assoc observed :provider-profile :old) :source-profile-mismatch]]]
          (with-redefs [source-log/observe-source-message! (fn [& _] changed)]
            (is (= reason (:reason (check ops true))))))
        (with-redefs [xt/q (fn [& _] [])]
          (is (= :transaction-outcome-unavailable (:reason (check ops true)))))
        (with-redefs [xt/q (fn [& _] (throw (ex-info "secret" {:raw ops})))]
          (is (= {:status :unavailable :reason :envelope-check-failed :profile envelope/profile
                  :authority :none :certificate :not-issued} (check ops true))))))))

(defn -main [& args]
  (let [{:keys [fail error]}
        (if (= ["claim"] (vec args))
          (let [flag (requiring-resolve 'gesso.live.outbox.claim-physical-test/*claim-source-log-observer?*)]
            (with-bindings {flag true}
              (run-tests 'gesso.live.outbox.claim-physical-test)))
          (run-tests 'gesso.assurance.xtdb-source-log-pilot))]
    (shutdown-agents)
    (System/exit (if (zero? (+ fail error)) 0 1))))
