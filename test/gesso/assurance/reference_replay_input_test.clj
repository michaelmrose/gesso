(ns gesso.assurance.reference-replay-input-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-replay-input :as input]
            [gesso.assurance.reference-arrow-operations :as arrow]
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
                              (submit! 4 [[:put-docs :probe {:xt/id "b" :xt/valid-from t}
                                                   {:xt/id "c"}]])
                              (:tx-id (xt/submit-tx node [["ASSERT TRUE"]]))]]
                (.awaitSourceBlocking database (peek rejected) (Duration/ofSeconds 15))
                (check-inputs! node expected rejected)
                [expected rejected]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (peek rejected) (Duration/ofSeconds 15))
          (check-inputs! node expected rejected)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(defn- no-replay-sample! [r]
  (is (not-any? #(contains? r %) [:replay-inputs :payload-fingerprint :operation-correspondence
                                 :operation-message-ids :payload-correspondence]))
  (is (= :none (:authority r)))
  (is (= :not-issued (:certificate r))))

(defn- check-replay-history! [node expected]
  (let [observe! #(coverage/observe-local-processed-retained-replay-inputs! node)
        decode! input/decode-replay-input
        r (observe!)]
    (is (= :observed (:status r)) (pr-str (dissoc r :replay-inputs :payload-fingerprint)))
    (is (= coverage/processed-retained-replay-inputs-profile (:processed-retained-replay-inputs-profile r)))
    (is (= input/profile (:replay-input-profile r)))
    (is (= expected (:replay-inputs r)))
    (is (= (mapv :tx-id expected) (:transaction-ids r) (:operation-message-ids r)))
    (is (= (if (seq expected) :matched :not-applicable) (:operation-correspondence r)))
    (is (= :matched (:payload-correspondence r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= (coverage/observe-local-processed-retained-history-bound! node)
           (dissoc r :processed-retained-replay-inputs-profile :replay-input-profile :replay-inputs)))
    (when (seq expected)
      (let [last-id (:tx-id (peek expected))
            first-time (:system-time (first expected))]
        (doseq [change [(fn [r] (assoc r :profile :wrong))
                        (fn [r] (update-in r [:transaction :tx-id] inc))
                        (fn [r] (dissoc r :transaction))]]
          (with-redefs [input/decode-replay-input
                        (fn [id bytes] (cond-> (decode! id bytes) (= id last-id) change))]
            (let [r (observe!)]
              (is (= :unsupported (:status r)))
              (is (= :retained-replay-input-not-qualified (:reason r)))
              (is (= last-id (:message-id r)))
              (no-replay-sample! r))))
        (with-redefs [input/decode-replay-input
                      (fn [id bytes] (cond-> (decode! id bytes)
                                       (= id last-id) (assoc-in [:transaction :system-time] first-time)))]
          (let [r (observe!)]
            (is (= :unsupported (:status r)))
            (is (= :retained-replay-system-time-order (:reason r)))
            (no-replay-sample! r)))
        (with-redefs-fn {#'coverage/max-replay-transactions 2}
          (fn [] (let [r (observe!)]
                   (is (= :retained-replay-transaction-limit (:reason r)))
                   (no-replay-sample! r))))
        (let [read! @#'coverage/read-local-payloads!
              seen (atom 0)]
          (with-redefs-fn {#'coverage/read-local-payloads!
                          (fn [file frames]
                            (let [sample (read! file frames)]
                              (if (= 2 (swap! seen inc)) (pop sample) sample)))}
            (fn [] (let [r (observe!)]
                     (is (= :source-payload-changed-during-observation (:reason r)))
                     (no-replay-sample! r)))))
        (let [frontier {:submitted (:last-message-id r) :processed -1 :ingestion-failed? false}]
          (with-redefs-fn {#'coverage/read-local-frontier! (constantly frontier)}
            (fn [] (let [r (observe!)]
                     (is (= :source-processing-incomplete (:reason r)))
                     (no-replay-sample! r)))))
        (is (= [true false true]
               (mapv :committed? (:history (temporal/replay (mapv #(dissoc % :tx-id) (:replay-inputs r)))))))))
    (is (= r (observe!)))))

(deftest complete-retained-replay-input-history
  (let [root (Files/createTempDirectory "gesso-replay-history-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}] :storage [:local {:path (str root "/storage")}]}
        t (Instant/parse "2026-01-01T00:00:00Z")
        txs [[[:put-docs :probe {:xt/id "a" :value 1}]]
             [[:put-docs :probe {:xt/id "a" :value 2}] ["ASSERT FALSE"]]
             [["ASSERT ? = ?" 3 3] ["ASSERT TRUE"]]]]
    (try
      (let [expected (with-open [node (xtn/start-node config)]
                       (let [database (db/primary-db node)
                             log (.getSourceLog database)
                             flush-id (.getMsgId (.appendMessageBlocking log (xtdb.api.log.SourceMessage$FlushBlock. Long/MAX_VALUE)))
                             _ (.awaitSourceBlocking database flush-id (Duration/ofSeconds 15))
                             _ (check-replay-history! node [])
                             expected (mapv (fn [i ops]
                                              (let [time (.plusSeconds t i)]
                                                {:tx-id (:tx-id (xt/submit-tx node ops {:system-time time}))
                                                 :system-time time :ops ops}))
                                            (range) txs)]
                         (.awaitSourceBlocking database (:tx-id (peek expected)) (Duration/ofSeconds 15))
                         (check-replay-history! node expected)
                         expected))]
        (with-open [node (xtn/start-node config)]
          (let [database (db/primary-db node)]
            (.awaitSourceBlocking database (:tx-id (peek expected)) (Duration/ofSeconds 15))
            (check-replay-history! node expected)
            (let [bad-id (:tx-id (xt/submit-tx node [["ASSERT ? IS NOT NULL" 1]] {:system-time (.plusSeconds t 3)}))]
              (.awaitSourceBlocking database bad-id (Duration/ofSeconds 15))
              (let [r (coverage/observe-local-processed-retained-replay-inputs! node)]
                (is (= :unsupported (:status r)))
                (is (= bad-id (:message-id r)))
                (is (= :retained-replay-input-not-qualified (:reason r)))
                (no-replay-sample! r))
              (is (= :observed (:status (coverage/observe-local-processed-retained-history! node))))))))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(defn- check-valid-time-inputs! [node expected]
  (let [sample (coverage/observe-local-processed-retained-replay-inputs! node)
        bytes (read-bytes node)
        id (:tx-id (first expected))
        body (get bytes id)
        decode! #(input/decode-replay-input id body)
        arrow! arrow/decode-bounded-operations
        prediction (temporal/replay (mapv #(dissoc % :tx-id) expected))]
    (is (= :observed (:status sample)) (pr-str (dissoc sample :replay-inputs :payload-fingerprint)))
    (is (= expected (:replay-inputs sample)))
    (is (= :matched (:operation-correspondence sample)))
    (is (= :predicted (:status prediction)))
    (is (= [true true true false] (mapv :committed? (:history prediction))))
    (doseq [tx expected]
      (let [r (input/decode-replay-input (:tx-id tx) (get bytes (:tx-id tx)))]
        (is (= :decoded (:status r)))
        (is (= input/profile (:profile r)))
        (is (= tx (:transaction r)))))
    ;; Retained facts with different bounds are not silently coalesced.
    (doseq [alter [(fn [r] (update-in r [:operations 0 :documents 1] dissoc :xt/valid-from))
                   (fn [r] (assoc-in r [:operations 0 :documents 0 :xt/valid-from] nil))
                   (fn [r] (assoc-in r [:operations 0 :bounds] {:xt/valid-from Instant/EPOCH}))
                   (fn [r] (update-in r [:operations 0 :documents]
                                     #(mapv (fn [d] (assoc d :xt/valid-to (:xt/valid-from d))) %)))
                   (fn [r] (update-in r [:operations 0 :documents]
                                     #(mapv (fn [d] (update d :xt/valid-from
                                                          (fn [v] (.withZoneSameInstant v (ZoneId/of "Asia/Tokyo"))))) %)))]]
      (with-redefs [arrow/decode-bounded-operations (comp alter arrow!)]
        (let [r (decode!)]
          (is (= :unsupported (:status r)))
          (is (= :unsupported-replay-operation (:reason r)))
          (is (not (contains? r :transaction))))))))

(deftest uniform-valid-time-replay-from-retained-history
  (let [root (Files/createTempDirectory "gesso-replay-valid-time-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        t (Instant/parse "2026-01-01T00:00:00Z")
        from (Instant/parse "1969-12-31T23:59:59.999999Z")
        to (.plusSeconds t 100)
        txs [[[:put-docs {:into :probe :valid-from from :valid-to to}
               {:xt/id "a" :value 1} {:xt/id "b" :value 1}]]
             [[:put-docs {:into :probe :valid-from t} {:xt/id "a" :value 2}]]
             [[:put-docs {:into :probe :valid-to to} {:xt/id "b" :value 3}]]
             [[:put-docs {:into :probe :valid-from from :valid-to to} {:xt/id "a" :value 4}]
              ["ASSERT FALSE"]]]]
    (try
      (let [expected (with-open [node (xtn/start-node config)]
                       (let [database (db/primary-db node)
                             expected (mapv (fn [i ops]
                                              (let [time (.plusSeconds t i)]
                                                {:tx-id (:tx-id (xt/submit-tx node ops {:system-time time}))
                                                 :system-time time :ops ops}))
                                            (range) txs)]
                         (.awaitSourceBlocking database (:tx-id (peek expected)) (Duration/ofSeconds 15))
                         (check-valid-time-inputs! node expected)
                         expected))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (:tx-id (peek expected)) (Duration/ofSeconds 15))
          (check-valid-time-inputs! node expected)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
