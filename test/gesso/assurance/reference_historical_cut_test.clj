(ns gesso.assurance.reference-historical-cut-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-historical-cut :as cut]
            [gesso.assurance.reference-retained-acquisition :as acquisition]
            [gesso.assurance.reference-storage-identity :as storage]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.time Duration Instant]))

(deftest derive-and-compare-actual-historical-cuts
  (let [root (Files/createTempDirectory "gesso-historical-cut-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        time (Instant/parse "2026-01-01T00:00:00Z")]
    (try
      (with-open [node (xtn/start-node config)]
        (let [capture! #(:acquisition (acquisition/capture! node))
              submit! (fn [id t]
                        (let [tx-id (:tx-id (xt/submit-tx node [[:put-docs :probe {:xt/id id :value 1}]] {:system-time t}))]
                          (.awaitSourceBlocking (db/primary-db node) tx-id (Duration/ofSeconds 30))
                          tx-id))
              empty-handle (capture!)
              empty-cut (:historical-cut (cut/derive-cut! node empty-handle))
              tx-id (submit! "a" time)
              handle (capture!)
              sample (acquisition/read-acquisition! node handle)
              r (cut/derive-cut! node handle)
              required (:historical-cut r)
              compare! #(cut/compare-exact-cut! %1 node %2)]
          (is (storage/historical-cut? empty-cut))
          (is (= [] (:record-offsets empty-cut) (:transaction-ids empty-cut)))
          (is (= -1 (:processed-message-id empty-cut)))
          (is (= :matched (:status (compare! empty-cut empty-handle))))
          (is (= :derived (:status r)))
          (is (= (select-keys sample (keys required)) required))
          (is (storage/historical-cut? required))
          (is (= [tx-id] (:transaction-ids required)))
          (is (= tx-id (:processed-message-id required)))
          (is (= :exact-content (:cut-correspondence (compare! required handle))))
          (is (= :invalid-required-cut (:reason (compare! nil handle))))
          (is (= :unissued-retained-acquisition (:reason (compare! required sample))))
          (is (= :unissued-retained-acquisition (:reason (cut/derive-cut! (Object.) handle))))
          (with-redefs [acquisition/read-acquisition! (constantly sample)]
            (is (= :acquisition-reader-replaced (:reason (cut/derive-cut! node handle)))))
          (let [changed (assoc required :payload-fingerprint (apply str (repeat 64 "0")))]
            (is (= :required-cut-content-mismatch (:reason (compare! changed handle)))))
          (is (= :required-cut-coordinates-not-preserved (:reason (compare! required empty-handle))))
          (submit! "b" (.plusSeconds time 1))
          (let [later-handle (capture!)
                later (:historical-cut (cut/derive-cut! node later-handle))
                extension (compare! required later-handle)
                prefix! #(cut/compare-prefix-cut! %1 node %2)]
            (is (= :matched (:status (compare! later later-handle))))
            (is (= :prefix-content (:cut-correspondence (prefix! required later-handle))))
            (is (= :prefix-content (:cut-correspondence (prefix! empty-cut later-handle))))
            (is (= :exact-content (:cut-correspondence (prefix! later later-handle))))
            (is (= :required-cut-coordinates-not-preserved (:reason (prefix! later handle))))
            (is (= :required-cut-coordinates-not-preserved
                   (:reason (prefix! (assoc required :transaction-ids []) later-handle))))
            (is (= :required-cut-content-mismatch
                   (:reason (prefix! (assoc required :payload-fingerprint (apply str (repeat 64 "0"))) later-handle))))
            (is (= :unissued-retained-acquisition (:reason (prefix! required sample))))
            (is (= :invalid-required-cut (:reason (prefix! nil later-handle))))
            (is (nil? (acquisition/read-prefix-fingerprint! (Object.) later-handle 1)))
            (is (nil? (acquisition/read-prefix-fingerprint! node later-handle 1024)))
            (is (not (contains? (acquisition/read-acquisition! node later-handle) :prefix-fingerprints)))
            (with-redefs [acquisition/read-prefix-fingerprint! (constantly (:payload-fingerprint required))]
              (is (= :prefix-reader-replaced (:reason (prefix! required later-handle)))))
            (is (= :unsupported (:status extension)))
            (is (= :qualified-prefix-digest-required (:reason extension)))
            (is (= :required-cut-coordinates-not-preserved (:reason (compare! later handle))))
            (is (= :matched (:status (compare! required handle))))
            (doseq [report [r extension (compare! required handle) (compare! required sample) (prefix! required later-handle)]]
              (is (= :none (:authority report)))
              (is (= :not-issued (:certificate report)))
              (is (= :not-established (:observation-storage-identity report)
                     (:authorized-historical-lineage report))))
            (doseq [report [extension (compare! required handle) (compare! required sample) (prefix! required later-handle)]]
              (is (not (contains? report :historical-cut)))
              (is (not (contains? report :payload-fingerprint)))))))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
