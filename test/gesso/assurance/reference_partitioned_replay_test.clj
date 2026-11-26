(ns gesso.assurance.reference-partitioned-replay-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-arrow-operations :as arrow]
            [gesso.assurance.reference-provider :as reference]
            [gesso.assurance.reference-replay-input :as input]
            [gesso.assurance.reference-replay-input-test :as fixture]
            [gesso.assurance.reference-source-coverage :as coverage]
            [gesso.assurance.reference-source-payload :as payload]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.assurance.reference-transaction :as current]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.mapped-frame :as frame]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration Instant)))

(deftest strict-retained-partitioned-journal-assertions
  (let [root (Files/createTempDirectory "gesso-retained-partitioned-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        t (Instant/parse "2026-01-01T00:00:00Z")
        at #(.plusSeconds t %)
        binding {:command-id (identity/command-id "c1") :principal (identity/principal "helper-a")
                 :operation :request/claim :arguments {:request-id "r"}}
        items (into [1 :claim/open "open"] (range 3 32))
        row (resolution/command-record binding {:status :claimed :details {:empty {} :items items}} "t1")
        changed (assoc-in row [:gesso.command/outcome :details :items 16] 999)
        mapping {:target-map {:outcome [:journal-slot "helper-a" :request/claim]}}
        prefix (frame/formatted-prestate-assertions! mapping {} [row])
        changed-prefix (frame/formatted-prestate-assertions! mapping {} [changed])
        put (fn [from to doc]
              [:put-docs (cond-> {:into :gesso_commands :valid-from (at from)}
                           to (assoc :valid-to (at to))) doc])
        probe [:put-docs :guard_probe {:xt/id "probe" :written true}]
        txs [[(put 3 7 row)] prefix prefix
             (into [(put 4 6 changed) probe ["ASSERT TRUE"]] prefix)
             (into [(put 8 nil changed)] prefix)
             prefix prefix prefix (conj changed-prefix probe)]
        tables #{:gesso_commands :guard_probe}
        failure (:failed-assertion-index (current/execute {:gesso_commands {(:xt/id changed) changed}} prefix))
        check! (fn [node expected]
                 (let [sample (coverage/observe-local-processed-retained-replay-inputs! node)
                       prediction (temporal/replay (mapv #(dissoc % :tx-id) (:replay-inputs sample)))
                       report (reference/compare-local-processed-retained-replay-execution! node tables)
                       id (:tx-id (nth expected 2)) body (get (#'fixture/read-bytes node) id)
                       arrow! arrow/decode-bounded-operations
                       decoded (-> body payload/decode-payload :message :tx-ops arrow!)
                       ops (:operations decoded)]
                   (is (= :observed (:status sample)) (pr-str (dissoc sample :replay-inputs :payload-fingerprint)))
                   (is (= expected (:replay-inputs sample)))
                   (is (= :predicted (:status prediction)))
                   (is (= [true false true false true true false false true] (mapv :committed? (:history prediction))))
                   (is (= [0 (+ 3 failure) 0 failure]
                          (mapv :failed-assertion-index (filter #(not (:committed? %)) (:history prediction)))))
                   (is (= :matched (:status report)) (pr-str report))
                   (is (= 9 (:transaction-count report)))
                   (is (= :none (:authority report)))
                   (is (= :not-issued (:certificate report)))
                   (is (= :not-established (:provider-refinement report)))
                   (is (= :unsupported (:status (coverage/observe-local-processed-retained-clocked-inputs! node))))
                   (is (= :unsupported (:status (reference/compare-local-processed-retained-replay-execution! node #{:gesso_commands}))))
                   (is (= :decoded (:status decoded)))
                   (is (= (mapv #(subvec % 1) prefix) (mapv :args ops)))
                   (is (every? #(<= (count (:args %)) 14) (subvec ops 2)))
                   (is (some #(nil? (current/operation-tables %)) (subvec prefix 2)))
                   (doseq [mutate [#(subvec % 2)
                                   #(assoc-in % [3 :query] (str/replace-first (get-in % [3 :query]) "$3" "$03"))
                                   #(assoc-in % [3 :query] (str/replace-first (get-in % [3 :query]) "$3" "$2"))
                                   #(update-in % [3 :args] pop)
                                   #(assoc-in % [3 :args 0] "other")
                                   #(update-in % [3 :query] str " -- comment")
                                   #(into (subvec % 0 3) (cons {:kind :sql :query "ASSERT TRUE" :args []} (subvec % 3)))
                                   #(into [{:kind :sql :query "ASSERT FALSE" :args []}] (subvec % 2))]]
                     (with-redefs [arrow/decode-bounded-operations #(update (arrow! %) :operations mutate)]
                       (let [r (input/decode-replay-input id body)]
                         (is (= :unsupported (:status r)) (pr-str r))
                         (is (not (contains? r :transaction))))))))]
    (try
      (is (> (count prefix) 9))
      (is (pos-int? failure))
      (let [expected (with-open [node (xtn/start-node config)]
                       (let [database (db/primary-db node)
                             expected (mapv (fn [n ops]
                                              (let [time (at n)
                                                    id (:tx-id (xt/submit-tx node ops {:system-time time}))]
                                                (.awaitTxBlocking database id (Duration/ofSeconds 15))
                                                {:tx-id id :system-time time :ops ops})) (range 1 10) txs)]
                         (.awaitSourceBlocking database (:tx-id (peek expected)) (Duration/ofSeconds 15))
                         (check! node expected)
                         expected))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (:tx-id (peek expected)) (Duration/ofSeconds 15))
          (check! node expected)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
