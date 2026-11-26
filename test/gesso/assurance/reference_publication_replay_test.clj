(ns gesso.assurance.reference-publication-replay-test
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
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.publication-manifest :as manifest]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration Instant)))

(deftest strict-retained-publication-absence
  (let [root (Files/createTempDirectory "gesso-retained-publication-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}] :storage [:local {:path (str root "/storage")}]}
        t (Instant/parse "2026-01-01T00:00:00Z")
        a #uuid "00000000-0000-4000-8000-000000000001"
        b #uuid "00000000-0000-4000-8000-000000000002"
        prepare (fn [id source]
                  (outbox/prepare! {:intent-ids [id] :source-id source
                                    :operation :request/claim :purpose :request/refresh}
                                   [{:topic :request/changed :id "r"}]))
        pa (prepare a "source-a") pb (prepare b "source-b")
        guard-a (first (:guard-ops pa)) guard-b (first (:guard-ops pb))
        source-guard (fn [source] (second (#'manifest/expected-establishment source "descriptor")))
        source-a (source-guard "source-a") source-b (source-guard "source-b")
        txs [[guard-a source-a]
             [guard-a (:tx-op pa)]
             [guard-a]
             [source-a]
             [guard-b source-b]
             [(:tx-op pb) source-b]
             [guard-b source-b]
             [guard-b (:tx-op pb)]
             [source-b]]
        check! (fn [node expected]
                 (let [sample (coverage/observe-local-processed-retained-replay-inputs! node)
                       prediction (temporal/replay (mapv #(dissoc % :tx-id) expected))
                       report (reference/compare-local-processed-retained-replay-execution! node #{:gesso_live_outbox})
                       id (:tx-id (first expected)) body (get (#'fixture/read-bytes node) id)
                       arrow! arrow/decode-bounded-operations
                       ops (-> body payload/decode-payload :message :tx-ops arrow! :operations)]
                   (is (= :observed (:status sample)) (pr-str (dissoc sample :replay-inputs :payload-fingerprint)))
                   (is (= expected (:replay-inputs sample)))
                   (is (= :predicted (:status prediction)))
                   (is (= [true true false false true false true true false]
                          (mapv :committed? (:history prediction))))
                   (is (= [0 0 1 0] (mapv :failed-assertion-index (filter #(not (:committed? %)) (:history prediction)))))
                   (is (= :matched (:status report)) (pr-str report))
                   (is (= 9 (:transaction-count report)))
                   (is (= :none (:authority report)))
                   (is (= :not-issued (:certificate report)))
                   (is (= :unsupported (:status (coverage/observe-local-processed-retained-clocked-inputs! node))))
                   (is (= :unsupported (:status (reference/compare-local-processed-retained-replay-execution! node #{:probe}))))
                   (is (= [[a] ["source-a"]] (mapv :args ops)))
                   (doseq [[i mutate] [[0 #(update % :query str/replace "$1" "$2")]
                                      [0 #(update % :query str/replace "$1" "$01")]
                                      [0 #(assoc % :args [(str a)])]
                                      [0 #(update % :args conj a)]
                                      [1 #(assoc % :args [a])]
                                      [1 #(assoc % :args [""])]
                                      [1 #(update % :query str/replace "source_id" "source")]
                                      [1 #(update % :query str/replace "ASSERT 0" "ASSERT 1")]
                                      [1 #(update % :query str " -- comment")]]]
                     (with-redefs [arrow/decode-bounded-operations #(update-in (arrow! %) [:operations i] mutate)]
                       (let [r (input/decode-replay-input id body)]
                         (is (= :unsupported (:status r)))
                         (is (not (contains? r :transaction))))))))]
    (try
      (let [expected (with-open [node (xtn/start-node config)]
                       (let [database (db/primary-db node)
                             expected (mapv (fn [i ops]
                                              (let [time (.plusSeconds t i)
                                                    id (:tx-id (xt/submit-tx node ops {:system-time time}))]
                                                (.awaitTxBlocking database id (Duration/ofSeconds 15))
                                                {:tx-id id :system-time time :ops ops})) (range) txs)]
                         (.awaitSourceBlocking database (:tx-id (peek expected)) (Duration/ofSeconds 15))
                         (check! node expected)
                         expected))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (:tx-id (peek expected)) (Duration/ofSeconds 15))
          (check! node expected)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
