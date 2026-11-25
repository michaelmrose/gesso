(ns gesso.assurance.reference-temporal-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.assurance.reference-temporal-fixtures
             :refer [day sys put a b c d provider-at rectangle provider-rectangles coalesce-valid]]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.time Duration Instant)))

(def ^:private transactions
  (mapv (fn [n ops] {:system-time (sys n) :ops ops}) (range 1 8)
        [[(put :temporalprobe 1 9 a)]
         [(put :temporalprobe 3 6 b) (put :temporalother 2 8 a)]
         [(put :temporalprobe 0 4 c) (put :temporalprobe 5 7 c d)]
         [(put :temporalprobe 0 10 b) ["ASSERT FALSE"] (put :temporalother 0 10 b)]
         [(put :temporalprobe 8 10 c)]
         [(put :temporalprobe 4 5 a) (put :temporalprobe 4 5 b)]
         [["ASSERT TRUE"]]]))

(deftest independent-bitemporal-overlay
  (let [r (temporal/replay transactions)
        at #(get-in (temporal/visible-at r (sys %1) (day %2)) [:temporalprobe "a"])]
    (is (= :predicted (:status r)))
    (is (= [true true true false true true true] (mapv :committed? (:history r))))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (nil? (at 0 1)))
    (is (nil? (at 1 0)))
    (is (= a (at 1 1)))
    (is (= a (at 1 8)))
    (is (nil? (at 1 9)))
    (is (= a (at 2 2)))
    (is (= b (at 2 3)))
    (is (= b (at 2 5)))
    (is (= a (at 2 6)))
    (is (= c (at 3 0)))
    (is (= b (at 3 4)))
    (is (= c (at 3 5)))
    (is (= a (at 3 7)))
    (is (= (get-in r [:history 2 :segments]) (get-in r [:history 3 :segments])))
    (is (= c (at 5 9)))
    (is (nil? (at 5 10)))
    (is (= b (at 6 4)))
    (is (= a (get-in (temporal/visible-at r (sys 7) (day 5)) [:temporalother "a"])))
    (is (= d (get-in (temporal/visible-at r (sys 7) (day 5)) [:temporalprobe "b"])))
    (is (= b (at 2 4)))))

(deftest rejects-unsupported-temporal-input
  (doseq [txs [nil [] [(assoc (first transactions) :system-time nil)]
               [(first transactions) (first transactions)]
               [(second transactions) (first transactions)]
               [{:system-time (sys 1) :ops [(put :temporalprobe 4 4 a)]}]
               [{:system-time (sys 1) :ops [(put :temporalprobe 5 4 a)]}]
               [{:system-time (sys 1) :ops [(assoc-in (put :temporalprobe 1 2 a) [1 :valid-from] (.plusNanos (day 1) 1))]}]
               [{:system-time (sys 1) :ops [["ASSERT FALSE"] [:put-docs {:into :temporalprobe :valid-from nil} a]]}]
               [{:system-time (sys 1) :ops [(put :temporalprobe 1 2 a b)]}]
               [{:system-time (sys 1) :ops [(put :temporalprobe 1 2 (assoc a :value nil))]}]]]
    (let [r (temporal/replay txs)]
      (is (= :unsupported (:status r)))
      (is (not (contains? r :history))))))

(deftest independently-predicted-bitemporal-points-match-xtdb
  (let [r (temporal/replay transactions)]
    (is (= :predicted (:status r)))
    (with-open [node (xtn/start-node {})]
      (doseq [[tx expected] (map vector transactions (:history r))]
        (let [id (:tx-id (xt/submit-tx node (:ops tx) {:system-time (:system-time tx)}))]
          (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 10))
          (is (= [{:committed (:committed? expected)}]
                 (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" id])))))
      ;; Compare old as well as new system snapshots after all writes.
      (doseq [s (range 0 8) v (range -1 12)]
        (is (= (temporal/visible-at r (sys s) (day v))
               (provider-at node (sys s) (day v))) (str "system " s " valid " v))))))

(def ^:private open-transactions
  (mapv (fn [n ops] {:system-time (sys n) :ops ops}) (range 1 10)
        [[[:put-docs {:into :temporalprobe :valid-from (sys 4) :valid-to (sys 7)} a]]
         [[:put-docs :temporalprobe b]]
         [[:put-docs {:into :temporalprobe :valid-from (sys 5) :valid-to (sys 6)} c]]
         [[:put-docs {:into :temporalprobe :valid-to (sys 5)} a]]
         [[:put-docs {:into :temporalprobe :valid-from (sys 0)} c]]
         [[:put-docs :temporalprobe a] ["ASSERT FALSE"]]
         [[:put-docs {:into :temporalprobe :valid-from (sys 8) :valid-to (sys 10)} a]]
         [[:put-docs :temporalprobe b]]
         [[:put-docs {:into :temporalother} d]]]))

(deftest independent-default-and-unbounded-validity
  (let [r (temporal/replay open-transactions)
        at #(get-in (temporal/visible-at r (sys %1) (sys %2)) [:temporalprobe "a"])]
    (is (= :predicted (:status r)))
    (is (= [true true true true true false true true true] (mapv :committed? (:history r))))
    (is (nil? (at 1 3)))
    (is (= a (at 1 4)))
    (is (nil? (at 1 7)))
    (is (nil? (at 2 1)))
    (is (= b (at 2 2)))
    (is (= b (at 2 5)))
    (is (= b (at 2 100)))
    (is (= b (at 3 4)))
    (is (= c (at 3 5)))
    (is (= b (at 3 6)))
    (is (= b (at 4 3)))
    (is (= a (at 4 4)))
    (is (= c (at 4 5)))
    (is (= c (at 5 0)))
    (is (= c (at 5 100)))
    (is (= c (at 6 6)))
    (is (= c (at 7 7)))
    (is (= a (at 7 8)))
    (is (= c (at 7 10)))
    (is (= b (at 8 8)))
    (is (= b (at 8 100)))
    (is (= a (at 7 9)))
    (is (nil? (get-in (temporal/visible-at r (sys 9) (sys 8)) [:temporalother "b"])))
    (is (= d (get-in (temporal/visible-at r (sys 9) (sys 9)) [:temporalother "b"])))))

(deftest rejects-invalid-default-and-unbounded-puts
  (doseq [opts [{:into :temporalprobe :valid-from nil}
                {:into :temporalprobe :valid-to nil}
                {:into :temporalprobe :valid-to (sys 1)}
                {:into :temporalprobe :valid-to (sys 0)}
                {:into :temporalprobe :valid-from "2025-01-01"}
                {:into :temporalprobe :extra true}
                {:valid-from (sys 1)}
                :invalid/table]]
    (let [r (temporal/replay [{:system-time (sys 1)
                              :ops [["ASSERT FALSE"] [:put-docs opts a]]}])]
      (is (= :unsupported (:status r)) (pr-str opts))
      (is (not (contains? r :history))))))

(deftest independently-predicted-unbounded-points-match-xtdb
  (let [r (temporal/replay open-transactions)]
    (is (= :predicted (:status r)))
    (with-open [node (xtn/start-node {})]
      (doseq [[tx expected] (map vector open-transactions (:history r))]
        (let [id (:tx-id (xt/submit-tx node (:ops tx) {:system-time (:system-time tx)}))]
          (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 10))
          (is (= [{:committed (:committed? expected)}]
                 (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" id])))))
      (doseq [s (range 0 10) v (concat (range -1 12) [100])]
        (is (= (temporal/visible-at r (sys s) (sys v))
               (provider-at node (sys s) (sys v))) (str "system " s " valid " v))))))

(def ^:private provenance-transactions
  (mapv (fn [n ops] {:system-time (sys n) :ops ops}) (range 1 6)
        [[[:put-docs {:into :temporalprobe :valid-from (day 1)} a]]
         [(put :temporalprobe 3 5 a)]
         [(put :temporalprobe 2 4 b) (put :temporalprobe 3 6 c)]
         [(put :temporalprobe 0 10 d) ["ASSERT FALSE"]]
         [["ASSERT TRUE"]]]))

(deftest independent-history-rectangle-provenance
  (let [r (temporal/replay provenance-transactions)
        rect #(rectangle :temporalprobe %1 (day %2) (when %3 (day %3))
                         (sys %4) (when %5 (sys %5)))]
    (is (= :predicted (:status r)))
    ;; Same-value replacement creates a new version. The two puts in S3
    ;; overlap, but the intermediate value never obtains a system interval.
    (is (= (frequencies [(rect a 3 5 1 2)
                         (rect a 2 3 1 3) (rect a 5 6 1 3)
                         (rect a 3 4 2 3) (rect a 4 5 2 3)
                         (rect a 1 2 1 nil) (rect b 2 3 3 nil)
                         (rect c 3 6 3 nil) (rect a 6 nil 1 nil)])
           (frequencies (:rectangles r))))
    (is (= [true true true false true] (mapv :committed? (:history r))))
    (is (not (contains? (temporal/replay []) :rectangles)))))

(deftest independently-predicted-history-rectangles-match-xtdb
  (doseq [txs [transactions open-transactions provenance-transactions]]
    (let [r (temporal/replay txs)]
      (is (= :predicted (:status r)))
      (with-open [node (xtn/start-node {})]
        (doseq [[tx expected] (map vector txs (:history r))]
          (let [id (:tx-id (xt/submit-tx node (:ops tx) {:system-time (:system-time tx)}))]
            (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 10))
            (is (= [{:committed (:committed? expected)}]
                   (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" id])))))
        (let [actual (mapcat #(provider-rectangles node %) [:temporalprobe :temporalother])]
          (is (= (frequencies (coalesce-valid (:rectangles r)))
                 (frequencies (coalesce-valid actual)))))))))

(defn- value-guard [value]
  ["ASSERT EXISTS (SELECT 1 FROM \"temporalprobe\" WHERE _id = ? AND \"value\" = ?)" "a" value])

(defn- row-count [n]
  ["ASSERT ? = (SELECT COUNT(*) FROM \"temporalprobe\" WHERE _id = ?)" n "a"])

(def ^:private guarded-transactions
  (mapv (fn [n ops] {:system-time (sys n) :ops ops}) (range 1 10)
        [[[:put-docs {:into :temporalprobe :valid-from (sys 3) :valid-to (sys 6)} a]
          (row-count 0)]
         [(row-count 0) (put :temporalprobe 0 4 b) (row-count 0)]
         [(value-guard 1) [:put-docs :temporalprobe b] (value-guard 2)]
         [[:put-docs {:into :temporalprobe :valid-from (sys 5)} c]
          (value-guard 2)]
         [(value-guard 3) [:put-docs :temporalprobe a] (value-guard 3)]
         [(value-guard 3) [:put-docs {:into :temporalprobe :valid-from (sys 4)
                                     :valid-to (sys 6)} a] (value-guard 3)]
         [[:put-docs :temporalprobe a] (value-guard 1) ["ASSERT ? = ?" 1 2]
          [:put-docs :temporalother d]]
         [(value-guard 3) [:put-docs :temporalother d] (row-count 1)]
         [[:put-docs :temporalprobe b] (value-guard 2) (row-count 1)]]))

(deftest independent-transaction-time-assertions
  (let [r (temporal/replay guarded-transactions)
        at #(get-in (temporal/visible-at r (sys %1) (sys %2)) [:temporalprobe "a"])]
    (is (= :predicted (:status r)))
    (is (= [true true true true false true false true true]
           (mapv :committed? (:history r))))
    (is (= [nil nil nil nil 2 nil 2 nil nil]
           (mapv :failed-assertion-index (:history r))))
    (is (= a (at 1 3)))
    (is (nil? (at 2 2)))
    (is (= b (at 3 4)))
    (is (= c (at 4 5)))
    (is (= c (at 5 5)))
    (is (= a (at 6 5)))
    (is (= c (at 6 6)))
    (is (= c (at 7 7)))
    (is (= b (at 9 9)))
    (is (= (get-in r [:history 3 :segments]) (get-in r [:history 4 :segments])))
    (is (= (get-in r [:history 5 :segments]) (get-in r [:history 6 :segments])))))

(deftest rejects-unsupported-temporal-assertions
  (doseq [ops [[[:put-docs :temporalprobe a] (value-guard "1")]
               [["ASSERT FALSE"] ["ASSERT arbitrary"]]
               [["ASSERT FALSE"] (row-count nil)]
               [["ASSERT EXISTS (SELECT 1 FROM temporalprobe FOR VALID_TIME ALL)"]]]]
    (let [r (temporal/replay [{:system-time (sys 1) :ops ops}])]
      (is (= :unsupported (:status r)))
      (is (not (contains? r :history)))
      (is (not (contains? r :rectangles))))))

(deftest independently-predicted-temporal-assertions-match-xtdb
  (let [r (temporal/replay guarded-transactions)]
    (is (= :predicted (:status r)))
    (with-open [node (xtn/start-node {})]
      (doseq [[tx expected] (map vector guarded-transactions (:history r))]
        (let [id (:tx-id (xt/submit-tx node (:ops tx) {:system-time (:system-time tx)}))]
          (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 10))
          (is (= [{:committed (:committed? expected)}]
                 (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" id])))))
      (doseq [s (range 0 10) v (range 0 11)]
        (is (= (temporal/visible-at r (sys s) (sys v))
               (provider-at node (sys s) (sys v))) (str "system " s " valid " v)))
      (let [actual (mapcat #(provider-rectangles node %) [:temporalprobe :temporalother])]
        (is (= (frequencies (coalesce-valid (:rectangles r)))
               (frequencies (coalesce-valid actual))))))))

(def ^:private equal-time-transactions
  [{:system-time (sys 1) :ops [(put :temporalprobe 0 9 a)]}
   {:system-time (sys 2) :ops [(put :temporalprobe 2 7 b) [:put-docs :temporalprobe a]]}
   {:system-time (sys 2) :ops [(put :temporalprobe 4 8 c) [:put-docs :temporalprobe b] ["ASSERT ? = ?" 2 2]]}
   {:system-time (sys 2) :ops [(put :temporalprobe 0 9 c) [:put-docs :temporalprobe c] ["ASSERT FALSE"]]}
   {:system-time (sys 2) :ops [["ASSERT TRUE"] (put :temporalother 1 3 d)]}
   {:system-time (sys 3) :ops [(put :temporalprobe 3 5 a)]}])

(defn- check-equal-time-history! [node r ids]
  (is (= [true true true false true true]
         (mapv :committed? (:history r))
         (mapv (fn [id] (:committed (first (xt/q node ["SELECT committed FROM xt.txs FOR VALID_TIME ALL FOR SYSTEM_TIME ALL WHERE _id = ?" id])))) ids)))
  (doseq [s (range 0 4) v (concat (map day (range -1 11)) (map sys (range 1 5)))]
    (is (= (temporal/visible-at r (sys s) v) (provider-at node (sys s) v))
        (str "system " s " valid " v)))
  (let [actual (mapcat #(provider-rectangles node %) [:temporalprobe :temporalother])]
    (is (= (frequencies (coalesce-valid (:rectangles r)))
           (frequencies (coalesce-valid actual))))))

(deftest independently-predicted-equal-system-time-history
  (let [txs equal-time-transactions
        r (temporal/replay-equal-time txs)
        root (java.nio.file.Files/createTempDirectory "gesso-equal-system-time-" (make-array java.nio.file.attribute.FileAttribute 0))
        config {:log [:local {:path (str root "/log")}] :storage [:local {:path (str root "/storage")}]}
        at #(get-in (temporal/visible-at r (sys %1) (day %2)) [:temporalprobe "a"])]
    (is (= :predicted (:status r)))
    (is (= temporal/equal-time-profile (:profile r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :unsupported (:status (temporal/replay txs))))
    (doseq [guard [(value-guard 2) (row-count 1) ["ASSERT CURRENT_TIMESTAMP < ?" (sys 4)]]]
      (let [report (temporal/replay-equal-time (update-in txs [2 :ops] conj guard))]
        (is (= :unsupported (:status report)))
        (is (= :unsupported-equal-time-assertion (:reason report)))
        (is (not-any? #(contains? report %) [:history :rectangles]))))
    (is (= :unsupported (:status (temporal/replay-equal-time [(nth txs 1) (first txs)]))))
    (is (= (dissoc (temporal/replay transactions) :profile)
           (dissoc (temporal/replay-equal-time transactions) :profile)))
    (is (= a (at 1 5)))
    (is (= b (at 2 3)))
    (is (= c (at 2 5)))
    (is (= a (at 2 8)))
    (is (= a (at 3 4)))
    (is (= 2 (:failed-assertion-index (nth (:history r) 3))))
    (is (= (get-in r [:history 2 :segments]) (get-in r [:history 3 :segments])))
    (is (every? #(or (nil? (:system-to %)) (.isBefore ^Instant (:system-from %) ^Instant (:system-to %))) (:rectangles r)))
    (try
      (let [ids (with-open [node (xtn/start-node config)]
                  (let [database (db/primary-db node)
                        ids (mapv (fn [tx]
                                    (let [id (:tx-id (xt/submit-tx node (:ops tx) {:system-time (:system-time tx)}))]
                                      (.awaitTxBlocking database id (Duration/ofSeconds 15)) id)) txs)]
                    (check-equal-time-history! node r ids)
                    ids))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (peek ids) (Duration/ofSeconds 15))
          (check-equal-time-history! node r ids)))
      (finally (doseq [file (reverse (file-seq (.toFile root)))] (io/delete-file file true))))))
