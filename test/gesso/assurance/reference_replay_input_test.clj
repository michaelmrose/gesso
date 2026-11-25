(ns gesso.assurance.reference-replay-input-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-replay-input :as input]
            [gesso.assurance.reference-source-coverage :as coverage]
            [gesso.assurance.reference-source-payload :as payload]
            [gesso.assurance.reference-temporal :as temporal]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.io RandomAccessFile)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration Instant ZoneId)))

(defn- read-bytes [node]
  (let [log (.getSourceLog (db/primary-db node))
        path (#'coverage/local-log-path log)]
    (with-open [file (RandomAccessFile. (.toFile path) "r")]
      (let [frames (:frames (#'coverage/local-frames file))]
        (zipmap (map first frames) (#'coverage/read-local-payloads! file frames))))))

(defn- check-inputs! [node expected rejected]
  (let [bytes (read-bytes node)
        decode! #(input/decode-replay-input % (get bytes %))
        reports (mapv #(decode! (:tx-id %)) expected)]
    (is (= expected (mapv :transaction reports)) (pr-str reports))
    (is (every? #(= :decoded (:status %)) reports))
    (is (every? #(= :none (:authority %)) reports))
    (is (every? #(= :not-issued (:certificate %)) reports))
    (is (= [true false true]
           (mapv :committed? (:history (temporal/replay (mapv #(dissoc (:transaction %) :tx-id) reports))))))
    (doseq [id rejected]
      (let [r (decode! id)]
        (is (= :unsupported (:status r)) (pr-str r))
        (is (not (contains? r :transaction)))))
    (let [id (:tx-id (first expected)) decode-payload payload/decode-payload]
      (doseq [[k v] [[:system-time nil] [:default-tz (ZoneId/of "Asia/Tokyo")] [:user "other"]
                     [:user-metadata (byte-array [1])]]]
        (with-redefs [payload/decode-payload #(assoc-in (decode-payload %) [:message k] v)]
          (let [r (decode! id)]
            (is (= :unsupported-replay-envelope-context (:reason r)))
            (is (not (contains? r :transaction)))))))
    (is (= :invalid-replay-transaction-id (:reason (input/decode-replay-input -1 nil))))
    (is (= :unsupported-replay-envelope (:reason (input/decode-replay-input 0 (byte-array [1])))))))

(deftest replay-input-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-replay-input-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}] :storage [:local {:path (str root "/storage")}]}
        t (Instant/parse "2026-01-01T00:00:00Z")
        txs [[[:put-docs :probe {:xt/id "a" :value 1}]]
             [[:put-docs :probe {:xt/id "a" :value 2}] ["ASSERT FALSE"]]
             [["ASSERT ? = ?" 3 3] ["ASSERT TRUE"]]]]
    (try
      (let [[expected rejected]
            (with-open [node (xtn/start-node config)]
              (let [database (db/primary-db node)
                    submit! (fn [i ops] (:tx-id (xt/submit-tx node ops {:system-time (.plusSeconds t i)})))
                    expected (mapv (fn [i ops] {:tx-id (submit! i ops) :system-time (.plusSeconds t i) :ops ops})
                                   (range) txs)
                    rejected [(submit! 3 [["ASSERT ? IS NOT NULL" 1]])
                              (submit! 4 [[:put-docs {:into :probe :valid-from t} {:xt/id "b" :value 1}]])
                              (:tx-id (xt/submit-tx node [["ASSERT TRUE"]]))]]
                (.awaitSourceBlocking database (peek rejected) (Duration/ofSeconds 15))
                (check-inputs! node expected rejected)
                [expected rejected]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (peek rejected) (Duration/ofSeconds 15))
          (check-inputs! node expected rejected)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
