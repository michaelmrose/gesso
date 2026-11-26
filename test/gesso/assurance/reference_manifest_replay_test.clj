(ns gesso.assurance.reference-manifest-replay-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-arrow-operations :as arrow]
            [gesso.assurance.reference-provider :as reference]
            [gesso.assurance.reference-replay-input :as input]
            [gesso.assurance.reference-replay-input-test :as fixture]
            [gesso.assurance.reference-source-coverage :as coverage]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.live.consistency.xtdb :as adapter]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.publication-manifest :as manifest]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration Instant)))

(deftest strict-retained-manifest-establishment
  (let [root (Files/createTempDirectory "gesso-retained-manifest-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        t (Instant/parse "2026-01-01T00:00:00Z")
        descriptor "[[[\"request\" \"changed\"] [\"request\" \"id\"] []]]"
        establish (fn [source] (#'manifest/expected-establishment source descriptor))
        a (establish "source-a") b (establish "source-b")
        c (establish "source-c") d (establish "source-d")
        publication (:tx-op (outbox/prepare! {:intent-ids [(java.util.UUID. 0 1)]
                                             :source-id "source-b" :operation :request/claim
                                             :purpose :request/refresh}
                                            [{:topic :request/changed :id "r"}]))
        txs [a (assoc-in a [2 2 :descriptor] "different") [publication] b c
             (into [(peek d)] d) d]
        tables #{:gesso_publication_manifests :gesso_live_outbox}
        submit! (fn [node i ops]
                  (let [time (.plusSeconds t i)
                        id (:tx-id (xt/submit-tx node ops {:system-time time}))]
                    (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 15))
                    {:tx-id id :system-time time :ops ops}))
        check! (fn [node expected]
                 (let [sample (coverage/observe-local-processed-retained-replay-inputs! node)
                       prediction (temporal/replay (mapv #(dissoc % :tx-id) expected))
                       report (reference/compare-local-processed-retained-replay-execution! node tables)
                       id (:tx-id (first expected)) body (get (#'fixture/read-bytes node) id)
                       arrow! arrow/decode-bounded-operations]
                   (is (= :observed (:status sample)) (pr-str (dissoc sample :replay-inputs :payload-fingerprint)))
                   (is (= expected (:replay-inputs sample)))
                   (is (= :predicted (:status prediction)))
                   (is (= [true false true false true false true] (mapv :committed? (:history prediction))))
                   (is (= [0 1 1] (mapv :failed-assertion-index (filter #(not (:committed? %)) (:history prediction)))))
                   (is (= tables (temporal/required-observation-tables (mapv #(dissoc % :tx-id) expected))))
                   (is (= :matched (:status report)) (pr-str report))
                   (is (= 7 (:transaction-count report)))
                   (is (= :none (:authority report)))
                   (is (= :not-issued (:certificate report)))
                   (is (= :not-established (:provider-refinement report)))
                   (is (= :unsupported (:status (coverage/observe-local-processed-retained-clocked-inputs! node))))
                   (doseq [scope [#{:gesso_publication_manifests} #{:gesso_live_outbox}]]
                     (is (= :unsupported (:status (reference/compare-local-processed-retained-replay-execution! node scope)))))
                   (doseq [[i mutate] [[0 #(update % :query str/replace "$1" "$2")]
                                      [0 #(update % :query str/replace "$1" "$01")]
                                      [0 #(assoc % :args [(java.util.UUID. 0 1)])]
                                      [0 #(assoc % :args [""])]
                                      [0 #(update % :args conj "source-a")]
                                      [0 #(update % :query str/replace "ASSERT 0" "ASSERT 1")]
                                      [0 #(update % :query str " -- comment")]
                                      [2 #(assoc-in % [:documents 0 :descriptor] (apply str (repeat 4097 "x")))]]]
                     (with-redefs [arrow/decode-bounded-operations #(update-in (arrow! %) [:operations i] mutate)]
                       (let [r (input/decode-replay-input id body)]
                         (is (= :unsupported (:status r)))
                         (is (not (contains? r :transaction))))))))]
    (try
      (let [expected (with-open [node (xtn/start-node config)]
                       ;; Exercise the real startup writer. Only submission is
                       ;; wrapped to assign a deterministic transaction clock.
                       (let [first-tx (atom nil)]
                         (with-redefs [adapter/execute-tx! (fn [n ops] (reset! first-tx (submit! n 0 ops)))]
                           (is (nil? (manifest/require-compatible! node "source-a" descriptor))))
                         (is (= a (:ops @first-tx)))
                         (let [expected (into [@first-tx] (map-indexed #(submit! node (inc %1) %2) (rest txs)))]
                           (.awaitSourceBlocking (db/primary-db node) (:tx-id (peek expected)) (Duration/ofSeconds 15))
                           (check! node expected)
                           expected)))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (:tx-id (peek expected)) (Duration/ofSeconds 15))
          (check! node expected)
          (is (nil? (manifest/require-compatible! node "source-a" descriptor)))))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
