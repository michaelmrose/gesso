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

(def ^:private interval-cases
  ;; Prior interval, tentative interval, value visible to the assertion
  ;; at transaction time. Bounds are seconds relative to t, half-open.
  [[[-10 10] [-5 5] 1]
   [[-5 5] [-10 10] 1]
   [[-10 0] [0 5] 2]
   [[0 10] [-5 0] 1]
   [[1 10] [-5 5] 2]
   [[-5 5] [1 10] 1]
   [[-10 -5] [1 10] nil]
   [[0 10] [0 5] 1]
   [[-5 5] [0 10] 1]
   [[0 10] [-5 5] 1]])

(defn- interval-id [i] (str "interval-" i))

(defn- bounded-put [id v [from to]]
  [:put-docs {:into :probe :valid-from (.plusSeconds t from) :valid-to (.plusSeconds t to)}
   {:xt/id id :value v}])

(defn- interval-guard [id v]
  (if (nil? v) (count-guard id 0) (guard id v)))

(defn- covers? [[from to] point] (and (<= from point) (< point to)))

(defn- expected-interval-values [point committed?]
  (into {} (keep-indexed
            (fn [i [prior replacement _]]
              (when-let [v (cond
                             (and committed? (covers? replacement point)) 2
                             (covers? prior point) 1)]
                [(interval-id i) v])) interval-cases)))

(defn- check-interval-snapshots! [node committed?]
  (doseq [point [-11 -10 -6 -5 -1 0 1 4 5 9 10 11]]
    (let [valid-time (.plusSeconds t point)
          rows (xt/q node ["SELECT _id, value FROM probe FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ?" valid-time t])
          expected (expected-interval-values point committed?)]
      (is (= (count expected) (count rows)) (str "valid point " point))
      (is (= expected (into {} (map (juxt :xt/id :value)) rows))
          (str "valid point " point " committed replacements? " committed?)))))

(defn- seed-intervals! [node]
  (last (mapv (fn [i [prior]]
                (attempt! node t [(bounded-put (interval-id i) 1 prior)] true))
              (range) interval-cases)))

(defn- characterize-intervals! [node]
  (check-interval-snapshots! node false)
  ;; Every opposite guard must abort, preserving the complete seeded
  ;; interval state; successful guards must commit the actual new rows.
  (doseq [[i [_ replacement visible]] (map-indexed vector interval-cases)]
    (let [id (interval-id i)
          opposite (if (= visible 2) 1 2)]
      (attempt! node t [(bounded-put id 2 replacement) (guard id opposite)] false)))
  (check-interval-snapshots! node false)
  (let [end (last (mapv (fn [i [_ replacement visible]]
                          (let [id (interval-id i)]
                            (attempt! node t [(bounded-put id 2 replacement)
                                              (interval-guard id visible)] true)))
                        (range) interval-cases))]
    (check-interval-snapshots! node true)
    end))

(deftest equal-time-assertion-visibility-at-valid-time-boundaries
  (let [root (Files/createTempDirectory "gesso-equal-time-intervals-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}]
    (try
      (with-open [node (xtn/start-node {})]
        (seed-intervals! node)
        (characterize-intervals! node))
      (let [id (with-open [node (xtn/start-node config)] (seed-intervals! node))
            end (with-open [node (xtn/start-node config)]
                  (.awaitSourceBlocking (db/primary-db node) id (Duration/ofSeconds 15))
                  (characterize-intervals! node))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) end (Duration/ofSeconds 15))
          (check-interval-snapshots! node true)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
