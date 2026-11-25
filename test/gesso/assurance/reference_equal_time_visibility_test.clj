(ns gesso.assurance.reference-equal-time-visibility-test
  "Provider characterization only; not an independent replay qualification."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-temporal :as temporal]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration Instant)))

(def ^:private t (Instant/parse "2025-01-01T00:00:00Z"))
(defn- put [id v] [:put-docs :probe {:xt/id id :value v}])
(defn- guard [id v]
  ["ASSERT EXISTS (SELECT 1 FROM \"probe\" WHERE _id = ? AND \"value\" = ?)" id v])
(defn- count-guard [id n]
  ["ASSERT ? = (SELECT COUNT(*) FROM \"probe\" WHERE _id = ?)" n id])

(defn- attempt! [node time ops committed?]
  (let [id (:tx-id (xt/submit-tx node ops {:system-time time}))]
    (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 15))
    (let [rows (xt/q node ["SELECT committed, error FROM xt.txs FOR VALID_TIME ALL FOR SYSTEM_TIME ALL WHERE _id = ?" id])]
      (is (= 1 (count rows)))
      (is (= committed? (:committed (first rows))) (pr-str ops))
      (if committed?
        (is (nil? (:error (first rows))))
        (is (= :xtdb/assert-failed (:xtdb.error/code (ex-data (:error (first rows))))))))
    id))

(defn- values-at [node time]
  (into {} (map (juxt :xt/id :value))
        (xt/q node ["SELECT _id, value FROM probe FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ?" time time])))

(defn- seed! [node]
  (attempt! node (.minusSeconds t 1) [(put "old" 1)] true)
  (attempt! node t [(put "same" 1)] true))

(defn- characterize! [node]
  (is (= {"old" 1 "same" 1} (values-at node t)))
  ;; Same-time prior commits and fresh tentative rows are visible.
  (attempt! node t [(guard "same" 1)] true)
  (attempt! node t [(put "new" 2) (guard "new" 2)] true)
  (attempt! node t [(put "old" 2) (guard "old" 2)] true)
  (is (= {"old" 2 "same" 1 "new" 2} (values-at node t)))
  ;; An overlapping tentative replacement of an already committed row
  ;; at exactly the same system time does not win the assertion read.
  (attempt! node t [(put "same" 2) (guard "same" 2)] false)
  (is (= 1 (get (values-at node t) "same")))
  (attempt! node t [(put "same" 2) (guard "same" 1)] true)
  (is (= 2 (get (values-at node t) "same")))
  (attempt! node t [(put "same" 3) (count-guard "same" 2)] false)
  (attempt! node t [(put "same" 3) (count-guard "same" 1)] true)
  (is (= 3 (get (values-at node t) "same")))
  (attempt! node t [(guard "same" 3)] true)
  ;; Later system time restores the tested read-your-write behavior.
  (attempt! node (.plusSeconds t 1) [(put "same" 4) (guard "same" 4)] true))

(deftest equal-time-assertion-visibility-characterization
  (let [root (Files/createTempDirectory "gesso-equal-time-visibility-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}]
    ;; Keep the observed behavior out of the independent replay contract.
    (is (= :unsupported-equal-time-assertion
           (:reason (temporal/replay-equal-time [{:system-time t :ops [(put "same" 1)]}
                                                 {:system-time t :ops [(put "same" 2) (guard "same" 2)]}]))))
    (try
      (with-open [node (xtn/start-node {})]
        (seed! node)
        (characterize! node))
      (let [id (with-open [node (xtn/start-node config)] (seed! node))
            end (with-open [node (xtn/start-node config)]
                  (.awaitSourceBlocking (db/primary-db node) id (Duration/ofSeconds 15))
                  (characterize! node))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) end (Duration/ofSeconds 15))
          (is (= {"old" 2 "same" 3 "new" 2} (values-at node t)))
          (is (= {"old" 2 "same" 4 "new" 2} (values-at node (.plusSeconds t 1))))))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
