(ns gesso.assurance.reference-retained-acquisition-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-retained-acquisition :as acquisition]
            [gesso.assurance.reference-source-coverage :as coverage]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import [gesso.assurance.reference_retained_acquisition RetainedAcquisition]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.time Duration Instant]))

(deftest actual-retained-sample-is-locally-issued
  (let [root (Files/createTempDirectory "gesso-retained-acquisition-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        time (Instant/parse "2026-01-01T00:00:00Z")]
    (try
      (let [{:keys [handle sample tx-id]}
            (with-open [node (xtn/start-node config)]
              (let [ops [[:put-docs :probe {:xt/id "a" :value 1}]]
                    tx-id (:tx-id (xt/submit-tx node ops {:system-time time}))
                    _ (.awaitSourceBlocking (db/primary-db node) tx-id (Duration/ofSeconds 30))
                    r (acquisition/capture! node)
                    handle (:acquisition r)
                    sample (acquisition/read-acquisition! node handle)]
                (is (= :issued (:status r)))
                (is (= acquisition/profile (:profile r)))
                (is (= :none (:authority r)))
                (is (= :not-issued (:certificate r)))
                (is (= :not-established (:observation-storage-identity r)
                       (:authorized-historical-lineage r)))
                (is (not (contains? r :sample)))
                (is (= :observed (:status sample)))
                (is (= [tx-id] (:transaction-ids sample)))
                (is (= [{:tx-id tx-id :system-time time :ops ops}] (:replay-inputs sample)))
                (is (= sample (coverage/observe-local-processed-retained-replay-inputs! node)))
                (is (identical? sample (acquisition/read-acquisition! node handle)))
                (is (nil? (acquisition/read-acquisition! node (RetainedAcquisition.))))
                (is (nil? (acquisition/read-acquisition! node sample)))
                (is (nil? (acquisition/read-acquisition! node r)))
                (is (nil? (acquisition/read-acquisition! (Object.) handle)))
                (with-redefs [coverage/observe-local-processed-retained-replay-inputs! (constantly sample)]
                  (let [rejected (acquisition/capture! node)]
                    (is (= :unsupported (:status rejected)))
                    (is (= :retained-reader-replaced (:reason rejected)))
                    (is (not (contains? rejected :acquisition))))
                  (is (identical? sample (acquisition/read-acquisition! node handle))))
                {:handle handle :sample sample :tx-id tx-id}))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) tx-id (Duration/ofSeconds 30))
          (is (= sample (coverage/observe-local-processed-retained-replay-inputs! node)))
          (is (nil? (acquisition/read-acquisition! node handle)))
          (let [new-handle (:acquisition (acquisition/capture! node))]
            (is (some? new-handle))
            (is (not (identical? handle new-handle)))
            (is (= sample (acquisition/read-acquisition! node new-handle))))))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(deftest unsupported-source-does-not-issue
  (with-open [node (xtn/start-node {})]
    (let [r (acquisition/capture! node)]
      (is (= :unsupported (:status r)))
      (is (= :unsupported-source-log (:reason r)))
      (is (not (contains? r :acquisition)))
      (is (= :none (:authority r)))
      (is (= :not-issued (:certificate r))))))
