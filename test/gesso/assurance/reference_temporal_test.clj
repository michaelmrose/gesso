(ns gesso.assurance.reference-temporal-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.assurance.reference-transaction :as current]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.read-frontier :as frontier]
            [gesso.live.consistency.xtdb :as live.xtdb]
            [gesso.live.outbox.publication-manifest :as manifest]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.time Duration Instant)))

(defn- day [n] (.plusSeconds (Instant/parse "2020-01-01T00:00:00Z") (* 86400 n)))
(defn- sys [n] (.plusSeconds (Instant/parse "2025-01-01T00:00:00Z") (* 86400 n)))
(defn- put [table a b & docs]
  (into [:put-docs {:into table :valid-from (day a) :valid-to (day b)}] docs))
(def ^:private a {:xt/id "a" :value 1 :old true})
(def ^:private b {:xt/id "a" :value 2})
(def ^:private c {:xt/id "a" :value 3})
(def ^:private d {:xt/id "b" :value 4})
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

(defn- provider-at [node system-time valid-time]
  (into {} (keep (fn [table]
                   (let [rows (xt/q node [(str "SELECT * FROM " (name table)
                                               " FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ?")
                                         valid-time system-time])]
                     (is (= (count rows) (count (set (map :xt/id rows)))))
                     (when (seq rows) [table (into {} (map (juxt :xt/id identity)) rows)]))))
        [:temporalprobe :temporalother]))

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

(defn- rectangle [table doc vf vt sf st]
  {:table table :document doc :valid-from vf :valid-to vt
   :system-from sf :system-to st})

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

(defn- provider-rectangles [node table]
  (mapv (fn [row]
          (let [instant #(when-let [v (get row %)] (.toInstant ^java.time.ZonedDateTime v))]
            (rectangle table (dissoc row :xt/valid-from :xt/valid-to
                                     :xt/system-from :xt/system-to)
                       (instant :xt/valid-from) (instant :xt/valid-to)
                       (instant :xt/system-from) (instant :xt/system-to))))
        (xt/q node [(str "SELECT *, _valid_from, _valid_to, _system_from, _system_to FROM "
                         (name table) " FOR VALID_TIME ALL FOR SYSTEM_TIME ALL")])))

(defn- coalesce-valid [rectangles]
  ;; Compare equivalent valid-axis partitions, preserving system origins,
  ;; ends, row contents and multiplicity of overlapping rectangles.
  (mapcat (fn [[_ rs]]
            (reduce (fn [acc r]
                      (if (and (seq acc) (= (:valid-to (peek acc)) (:valid-from r)))
                        (conj (pop acc) (assoc (peek acc) :valid-to (:valid-to r)))
                        (conj acc r)))
                    [] (sort-by :valid-from rs)))
          (group-by #(dissoc % :valid-from :valid-to) rectangles)))

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

(defn- deadline-guards [deadline]
  [["ASSERT CURRENT_TIMESTAMP < ?" deadline]
   ["ASSERT CURRENT_TIMESTAMP + INTERVAL '5' MINUTE >= ?" deadline]])

(def ^:private clock-cases
  ;; Deadline offsets in microseconds relative to the supplied tx clock.
  [[-1 false 1] [0 false 1] [1 true nil]
   [299999999 true nil] [300000000 true nil] [300000001 false 2]
   [600000000 false 2]])

(def ^:private clock-transactions
  (mapv (fn [n [micros _ _]]
          (let [time (sys n)
                deadline (.plusNanos time (* 1000 micros))]
            {:system-time time
             :ops (into [[:put-docs :temporalprobe {:xt/id "a" :value n}]]
                        (concat (deadline-guards deadline)
                                [[:put-docs :temporalother d]]))}))
        (range 1 8) clock-cases))

(deftest independent-worker-deadline-guards
  (let [r (temporal/replay clock-transactions)]
    (is (= :predicted (:status r)))
    (is (= (mapv second clock-cases) (mapv :committed? (:history r))))
    (is (= (mapv #(nth % 2) clock-cases)
           (mapv :failed-assertion-index (:history r))))
    (is (= {:xt/id "a" :value 5}
           (get-in (temporal/visible-at r (sys 7) (sys 7)) [:temporalprobe "a"])))
    (is (= (get-in r [:history 4 :segments]) (get-in r [:history 6 :segments])))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))))

(deftest rejects-unsupported-worker-clock-guards
  (doseq [op [["ASSERT CURRENT_TIMESTAMP < ?"]
              ["ASSERT CURRENT_TIMESTAMP < ?" nil]
              ["ASSERT CURRENT_TIMESTAMP < ?" "2025-01-01"]
              ["ASSERT CURRENT_TIMESTAMP < ?" (.plusNanos (sys 1) 1)]
              ["ASSERT CURRENT_TIMESTAMP < ?" Instant/MAX]
              ["ASSERT CURRENT_TIMESTAMP < ?" (sys 2) (sys 3)]
              ["ASSERT CURRENT_TIMESTAMP <= ?" (sys 2)]
              ["ASSERT CURRENT_TIMESTAMP + INTERVAL '6' MINUTE >= ?" (sys 2)]]]
    (let [r (temporal/replay [{:system-time (sys 1) :ops [["ASSERT FALSE"] op]}])]
      (is (= :unsupported (:status r)) (pr-str op))
      (is (not (contains? r :history)))
      (is (not (contains? r :rectangles))))))

(deftest independently-predicted-worker-clock-guards-match-xtdb
  (let [r (temporal/replay clock-transactions)]
    (is (= :predicted (:status r)))
    (with-open [node (xtn/start-node {})]
      (doseq [[tx expected] (map vector clock-transactions (:history r))]
        (let [id (:tx-id (xt/submit-tx node (:ops tx) {:system-time (:system-time tx)}))]
          (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 10))
          (is (= [{:committed (:committed? expected)}]
                 (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" id])))))
      (doseq [s (range 0 8) v (range 0 9)]
        (is (= (temporal/visible-at r (sys s) (sys v))
               (provider-at node (sys s) (sys v))) (str "system " s " valid " v)))
      (let [actual (mapcat #(provider-rectangles node %) [:temporalprobe :temporalother])]
        (is (= (frequencies (coalesce-valid (:rectangles r)))
               (frequencies (coalesce-valid actual))))))))

(def ^:private intent-id #uuid "00000000-0000-0000-0000-000000000001")
(def ^:private other-intent-id #uuid "00000000-0000-0000-0000-000000000002")
(defn- intent-put [value]
  [:put-docs :gesso_live_outbox {:xt/id intent-id :value value}])
(defn- version-guard [id version]
  ["ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ?)" id version])
(defn- version-at [n]
  (.atZone (sys n) (java.time.ZoneId/of "America/Los_Angeles")))

(def ^:private version-transactions
  (mapv (fn [n ops] {:system-time (sys n) :ops ops}) (range 1 11)
        [[(version-guard intent-id (sys 1))]
         [(intent-put 1)]
         [(version-guard intent-id (version-at 2)) (intent-put 1)]
         [(intent-put 2) (version-guard intent-id (sys 3))]
         [(version-guard intent-id (sys 3)) (intent-put 2) (version-guard intent-id (sys 5))]
         [(version-guard other-intent-id (sys 5))]
         [(version-guard intent-id (sys 5))
          [:put-docs {:into :gesso_live_outbox :valid-from (sys 8) :valid-to (sys 9)}
           {:xt/id intent-id :value 3}]
          (version-guard intent-id (sys 5))]
         [(version-guard intent-id (sys 7))]
         [(version-guard intent-id (sys 5)) (intent-put 4) ["ASSERT FALSE"]]
         [(version-guard intent-id (sys 5)) (intent-put 5)]]))

(deftest independent-worker-version-guards
  (let [r (temporal/replay version-transactions)]
    (is (= :predicted (:status r)))
    (is (= [false true true false true false true true false true]
           (mapv :committed? (:history r))))
    (is (= [0 nil nil 1 nil 0 nil nil 2 nil]
           (mapv :failed-assertion-index (:history r))))
    (is (= {:xt/id intent-id :value 5}
           (get-in (temporal/visible-at r (sys 10) (sys 10)) [:gesso_live_outbox intent-id])))
    (is (= {:xt/id intent-id :value 3}
           (get-in (temporal/visible-at r (sys 10) (sys 8)) [:gesso_live_outbox intent-id])))
    (is (= (get-in r [:history 7 :segments]) (get-in r [:history 8 :segments])))))

(deftest rejects-unsupported-worker-version-guards
  (doseq [op [(version-guard "id" (sys 1))
              (version-guard intent-id nil)
              (version-guard intent-id "2025-01-01")
              (version-guard intent-id (.plusNanos (sys 1) 1))
              (pop (version-guard intent-id (sys 1)))
              (conj (version-guard intent-id (sys 1)) true)
              (assoc (version-guard intent-id (sys 1)) 0 "ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from >= ?)")]]
    (let [r (temporal/replay [{:system-time (sys 1) :ops [["ASSERT FALSE"] op]}])]
      (is (= :unsupported (:status r)))
      (is (not (contains? r :history)))))
  (let [r (temporal/replay [{:system-time (sys 1)
                            :ops [[:put-docs :gesso_live_outbox {:xt/id "a" :value 1}]
                                  (version-guard intent-id (sys 1))]}])]
    (is (= :unsupported (:status r)))
    (is (not (contains? r :history)))))

(deftest independently-predicted-worker-versions-match-xtdb
  (let [r (temporal/replay version-transactions)]
    (is (= :predicted (:status r)))
    (with-open [node (xtn/start-node {})]
      (doseq [[tx expected] (map vector version-transactions (:history r))]
        (let [id (:tx-id (xt/submit-tx node (:ops tx) {:system-time (:system-time tx)}))]
          (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 10))
          (is (= [{:committed (:committed? expected)}]
                 (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" id])))))
      (doseq [s (range 0 11) v (range 0 12)]
        (let [rows (xt/q node ["SELECT * FROM gesso_live_outbox FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ?"
                               (sys v) (sys s)])]
          (is (= (get (temporal/visible-at r (sys s) (sys v)) :gesso_live_outbox {})
                 (into {} (map (juxt :xt/id identity)) rows)))))
      (is (= (frequencies (coalesce-valid (:rectangles r)))
             (frequencies (coalesce-valid (provider-rectangles node :gesso_live_outbox))))))))

(defn- lease-guard [kind id version deadline]
  [(str "ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ? AND CURRENT_TIMESTAMP "
        ({:expired ">=" :active "<"} kind) " ?)") id version deadline])

(defn- micro-offset [n offset]
  (.plusNanos (sys n) (* 1000 offset)))

(def ^:private lease-transactions
  (mapv (fn [n ops] {:system-time (sys n) :ops ops}) (range 1 15)
        [[(intent-put 1)]
         [(lease-guard :active intent-id (version-at 1) (micro-offset 2 1)) (intent-put 2)]
         [(lease-guard :expired intent-id (sys 2) (micro-offset 3 1)) (intent-put 3)]
         (into [(lease-guard :expired intent-id (sys 2) (version-at 4))]
               (concat (deadline-guards (.plusSeconds (sys 4) 300)) [(intent-put 3)]))
         [(lease-guard :active intent-id (sys 4) (sys 5)) (intent-put 4)]
         [(lease-guard :expired intent-id (sys 2) (micro-offset 6 -1))]
         [(lease-guard :active intent-id (sys 4) (micro-offset 7 1)) (intent-put 4)]
         [(lease-guard :expired intent-id (sys 7) (micro-offset 8 -1)) (intent-put 5)]
         [(lease-guard :active intent-id (sys 7) (micro-offset 9 1))]
         [(lease-guard :expired other-intent-id (sys 8) (sys 10))]
         [(intent-put 6) (lease-guard :active intent-id (sys 11) (micro-offset 11 1)) ["ASSERT FALSE"]]
         [(intent-put 7) (lease-guard :expired intent-id (sys 12) (sys 12)) (intent-put 8)]
         [(lease-guard :active intent-id (sys 12) (micro-offset 13 -1))]
         [(lease-guard :active intent-id (sys 12) (version-at 15))]]))

(deftest independent-worker-lease-fencing
  (let [r (temporal/replay lease-transactions)]
    (is (= :predicted (:status r)))
    (is (= [true true false true false false true true false false false true false true]
           (mapv :committed? (:history r))))
    (is (= [nil nil 0 nil 0 0 nil nil 0 0 2 nil 0 nil]
           (mapv :failed-assertion-index (:history r))))
    (is (= {:xt/id intent-id :value 5}
           (get-in (temporal/visible-at r (sys 11) (sys 11)) [:gesso_live_outbox intent-id])))
    (is (= {:xt/id intent-id :value 8}
           (get-in (temporal/visible-at r (sys 14) (sys 14)) [:gesso_live_outbox intent-id])))
    (is (= (get-in r [:history 7 :segments]) (get-in r [:history 10 :segments])))
    (is (= :none (:authority r)))))

(deftest rejects-unsupported-worker-lease-guards
  (let [guard (lease-guard :active intent-id (sys 1) (sys 2))]
    (doseq [op [(pop guard) (conj guard true) (assoc guard 1 "id")
                (assoc guard 2 nil) (assoc guard 3 nil) (assoc guard 3 "2025-01-01")
                (assoc guard 3 (.plusNanos (sys 2) 1)) (assoc guard 3 Instant/MAX)]]
      (let [r (temporal/replay [{:system-time (sys 1) :ops [["ASSERT FALSE"] op]}])]
        (is (= :unsupported (:status r)) (pr-str op))
        (is (not (contains? r :history)))
        (is (not (contains? r :rectangles)))))))

(deftest independently-predicted-worker-lease-fences-match-xtdb
  (let [r (temporal/replay lease-transactions)]
    (is (= :predicted (:status r)))
    (with-open [node (xtn/start-node {})]
      (doseq [[tx expected] (map vector lease-transactions (:history r))]
        (let [id (:tx-id (xt/submit-tx node (:ops tx) {:system-time (:system-time tx)}))]
          (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 10))
          (is (= [{:committed (:committed? expected)}]
                 (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" id])))))
      (doseq [s (range 0 15) v (range 0 16)]
        (let [rows (xt/q node ["SELECT * FROM gesso_live_outbox FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ?"
                               (sys v) (sys s)])]
          (is (= (count rows) (count (set (map :xt/id rows)))))
          (is (= (get (temporal/visible-at r (sys s) (sys v)) :gesso_live_outbox {})
                 (into {} (map (juxt :xt/id identity)) rows)))))
      (is (= (frequencies (coalesce-valid (:rectangles r)))
             (frequencies (coalesce-valid (provider-rectangles node :gesso_live_outbox))))))))

(def ^:private pending-intent
  (first (:intents (outbox/prepare!
                    {:intent-ids [intent-id] :source-id "reference-source"
                     :operation :request/claim :purpose :gesso.live/refresh}
                    [{:topic :request :id "request-1" :change/kind :updated}]))))
(defn- worker-time [seconds] (.plusSeconds (sys 1) seconds))
(defn- utc-time [instant] (.atZone ^Instant instant (java.time.ZoneId/of "UTC")))
(def ^:private first-deadline (.plusNanos (worker-time 180) 1000))
(def ^:private next-deadline (worker-time 360))
(def ^:private acquire-claim
  {:intent-id intent-id :lease-id other-intent-id :worker-id "worker-a"
   :now (worker-time 1) :lease-until first-deadline})
(def ^:private acquire-operations
  (#'acquisition/acquire-ops (assoc pending-intent :xt/system-from (utc-time (worker-time 0)))
                           acquire-claim))
(def ^:private first-lease
  (assoc (nth (last acquire-operations) 2) :gesso.live.outbox/lease-until (utc-time first-deadline)))
(def ^:private next-lease
  (assoc first-lease :gesso.live.outbox/lease-id intent-id
         :gesso.live.outbox/lease-owner "worker-b"
         :gesso.live.outbox/lease-until next-deadline))
(def ^:private decoded-next-lease
  (assoc next-lease :gesso.live.outbox/lease-until (utc-time next-deadline)))
(def ^:private terminal-intent
  (assoc decoded-next-lease :gesso.live.outbox/status :dispatch-accepted
         :gesso.live.outbox/dispatch-receipt-id "receipt-1"
         :gesso.live.outbox/dispatch-adapter :test/adapter))
(def ^:private timestamp-transactions
  (mapv (fn [seconds ops] {:system-time (worker-time seconds) :ops ops})
        [0 1 2 3 181 182 183]
        [[[:put-docs outbox/intent-table pending-intent]]
         acquire-operations
         [(lease-guard :active intent-id (worker-time 0) first-deadline)
          [:put-docs outbox/intent-table next-lease]]
         [(lease-guard :active intent-id (worker-time 1) (utc-time first-deadline))
          [:put-docs outbox/intent-table next-lease] ["ASSERT FALSE"]]
         (into [(lease-guard :expired intent-id (worker-time 1) (utc-time first-deadline))]
               (concat (deadline-guards next-deadline) [[:put-docs outbox/intent-table next-lease]]))
         [(lease-guard :active intent-id (worker-time 181) (utc-time next-deadline))
          [:put-docs outbox/intent-table terminal-intent]]
         [[:put-docs outbox/intent-table pending-intent] ["ASSERT FALSE"]]]))

(deftest independent-timestamp-valued-outbox-storage
  (let [r (temporal/replay timestamp-transactions)
        at #(get-in (temporal/visible-at r (worker-time %1) (worker-time %2))
                    [outbox/intent-table intent-id])]
    (is (= :predicted (:status r)))
    (is (= [true true false false true true false] (mapv :committed? (:history r))))
    (is (= pending-intent (at 0 0)))
    (is (= first-lease (at 1 1) (at 3 3) (at 183 180)))
    (is (= decoded-next-lease (at 181 181)))
    (is (= terminal-intent (at 183 183)))
    (is (= first-deadline (.toInstant (:gesso.live.outbox/lease-until (at 1 1)))))
    (is (= (get-in r [:history 5 :segments]) (get-in r [:history 6 :segments]))))
  (let [r (current/execute {} [[:put-docs outbox/intent-table next-lease]])]
    (is (= decoded-next-lease (get-in r [:state outbox/intent-table intent-id])))
    (is (= (:state r) (:state (current/execute (:state r) [["ASSERT TRUE"]]))))))

(deftest rejects-unsupported-outbox-timestamps
  (doseq [value [nil "2025-01-01" (.plusNanos (worker-time 1) 1) Instant/MAX
                 (.atZone (worker-time 1) (java.time.ZoneId/of "America/Los_Angeles"))]]
    (let [op [:put-docs outbox/intent-table (assoc first-lease :gesso.live.outbox/lease-until value)]
          r (temporal/replay [{:system-time (worker-time 0) :ops [["ASSERT FALSE"] op]}])]
      (is (= :unsupported (:status r)) (pr-str value))
      (is (not (contains? r :history)))))
  (doseq [op [[:put-docs :temporalprobe (assoc a :value first-deadline)]
              [:put-docs :temporalprobe (assoc a :gesso.live.outbox/lease-until first-deadline)]]]
    (is (= :unsupported (:status (current/execute {} [op]))))))

(deftest independently-predicted-outbox-timestamps-match-xtdb
  (let [r (temporal/replay timestamp-transactions)]
    (is (= :predicted (:status r)))
    (with-open [node (xtn/start-node {})]
      (doseq [[tx expected] (map vector timestamp-transactions (:history r))]
        (let [id (:tx-id (xt/submit-tx node (:ops tx) {:system-time (:system-time tx)}))]
          (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 10))
          (is (= [{:committed (:committed? expected)}]
                 (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" id])))))
      (doseq [s [0 1 2 3 181 182 183] v [0 1 2 3 180 181 182 183 360]]
        (let [rows (xt/q node ["SELECT * FROM gesso_live_outbox FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ?"
                               (worker-time v) (worker-time s)])]
          (is (= (count rows) (count (set (map :xt/id rows)))))
          (is (= (get (temporal/visible-at r (worker-time s) (worker-time v)) outbox/intent-table {})
                 (into {} (map (juxt :xt/id identity)) rows)))))
      (is (= (frequencies (coalesce-valid (:rectangles r)))
             (frequencies (coalesce-valid (provider-rectangles node outbox/intent-table))))))))

(def ^:private third-intent-id #uuid "00000000-0000-0000-0000-000000000003")
(defn- publication [ids]
  (outbox/prepare! {:intent-ids ids :source-id "reference-source"
                    :operation :request/claim :purpose :gesso.live/refresh}
                   (mapv (fn [id] {:topic :request :id (str id) :change/kind :updated}) ids)))
(defn- publication-ops [ids domain-doc]
  (let [{:keys [guard-ops tx-op]} (publication ids)]
    (into guard-ops [[:put-docs :temporalprobe domain-doc] tx-op])))
(def ^:private publication-transactions
  (mapv (fn [n ops] {:system-time (sys n) :ops ops}) (range 1 9)
        [[[:put-docs {:into outbox/intent-table :valid-from (sys 4) :valid-to (sys 6)}
           (first (:intents (publication [intent-id])))]]
         (publication-ops [other-intent-id] a)
         (publication-ops [other-intent-id] b)
         (publication-ops [third-intent-id intent-id] b)
         (into (publication-ops [third-intent-id] b) [["ASSERT FALSE"]])
         (publication-ops [intent-id third-intent-id] b)
         (publication-ops [third-intent-id] c)
         (into [[:put-docs :temporalprobe c]] (:guard-ops (publication [intent-id])))]))

(deftest independent-publication-uuid-absence
  (let [r (temporal/replay publication-transactions)]
    (is (= :predicted (:status r)))
    (is (= [true true false false false true false false] (mapv :committed? (:history r))))
    (is (= [nil nil 0 1 3 nil 0 1] (mapv :failed-assertion-index (:history r))))
    (is (= a (get-in (temporal/visible-at r (sys 5) (sys 5)) [:temporalprobe "a"])))
    (is (= b (get-in (temporal/visible-at r (sys 8) (sys 8)) [:temporalprobe "a"])))
    (is (= #{intent-id other-intent-id third-intent-id}
           (set (keys (get (temporal/visible-at r (sys 8) (sys 8)) outbox/intent-table))))))
  (let [{:keys [guard-ops tx-op]} (publication [intent-id])
        guard (first guard-ops)
        r (current/execute {} [tx-op guard])]
    (is (= false (:committed? r)))
    (is (= {} (:state r)))
    (is (= 1 (:failed-assertion-index r)))
    (doseq [op [(pop guard) (conj guard true) (assoc guard 1 nil)
                (assoc guard 1 (str intent-id))
                (assoc guard 0 "ASSERT 0 = (SELECT COUNT(*) FROM gesso_other WHERE _id = ?)")]]
      (let [r (current/execute {} [["ASSERT FALSE"] op])]
        (is (= :unsupported (:status r)))
        (is (not (contains? r :state)))
        (is (not (contains? r :committed?)))))
    (let [r (current/execute {} [[:put-docs outbox/intent-table {:xt/id "legacy" :value 1}] guard])]
      (is (= :unsupported (:status r)))
      (is (not (contains? r :state))))))

(deftest independently-predicted-publication-absence-matches-xtdb
  (let [r (temporal/replay publication-transactions)]
    (is (= :predicted (:status r)))
    (with-open [node (xtn/start-node {})]
      (doseq [[tx expected] (map vector publication-transactions (:history r))]
        (let [id (:tx-id (xt/submit-tx node (:ops tx) {:system-time (:system-time tx)}))]
          (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 10))
          (is (= [{:committed (:committed? expected)}]
                 (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" id])))))
      (doseq [s (range 0 9) v (range 0 10) table [outbox/intent-table :temporalprobe]]
        (let [rows (xt/q node [(str "SELECT * FROM " (name table) " FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ?")
                               (sys v) (sys s)])]
          (is (= (count rows) (count (set (map :xt/id rows)))))
          (is (= (get (temporal/visible-at r (sys s) (sys v)) table {})
                 (into {} (map (juxt :xt/id identity)) rows)))))
      (is (= (frequencies (coalesce-valid (:rectangles r)))
             (frequencies (coalesce-valid (mapcat #(provider-rectangles node %) [outbox/intent-table :temporalprobe]))))))))

(defn- establish [source]
  (#'manifest/expected-establishment source "reference-descriptor"))
(defn- source-intent [id source]
  (assoc (first (:intents (publication [id]))) :gesso.live.outbox/source-id source))
(def ^:private manifest-transactions
  (mapv (fn [n ops] {:system-time (sys n) :ops ops}) (range 1 11)
        [[[:put-docs {:into outbox/intent-table :valid-from (sys 3) :valid-to (sys 6)}
           (source-intent intent-id "scheduled")]
          [:put-docs outbox/intent-table (source-intent other-intent-id "existing")]]
         (establish "fresh")
         (establish "scheduled")
         (establish "existing")
         (establish "fresh")
         (establish "scheduled")
         (into [[:put-docs outbox/intent-table (source-intent third-intent-id "tentative")]]
               (establish "tentative"))
         (into (establish "aborted") [["ASSERT FALSE"]])
         (establish "aborted")
         (into [[:put-docs outbox/intent-table {:xt/id third-intent-id :value 1}]]
               (establish "missing-field"))]))

(deftest independent-publication-manifest-establishment
  (let [r (temporal/replay manifest-transactions)]
    (is (= :predicted (:status r)))
    (is (= [true true false false false true false false true true]
           (mapv :committed? (:history r))))
    (is (= [nil nil 1 1 0 nil 2 3 nil nil]
           (mapv :failed-assertion-index (:history r))))
    (is (= #{"fresh" "scheduled" "aborted" "missing-field"}
           (set (keys (get (temporal/visible-at r (sys 10) (sys 10))
                          :gesso_publication_manifests)))))
    (is (= (get-in r [:history 5 :segments]) (get-in r [:history 7 :segments]))))
  (let [guard (second (establish "existing"))]
    (doseq [op [(pop guard) (conj guard true) (assoc guard 1 nil)
                (assoc guard 1 :existing) (assoc guard 1 "")
                (assoc guard 0 "ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE gesso$live$outbox$source_id = ?)")]]
      (let [r (current/execute {} [["ASSERT FALSE"] op])]
        (is (= :unsupported (:status r)))
        (is (not (contains? r :state)))))
    (doseq [v [:existing 1 true intent-id ["existing"]]]
      (let [r (current/execute {} [[:put-docs outbox/intent-table
                                    (source-intent intent-id v)] guard])]
        (is (= :unsupported (:status r)))
        (is (not (contains? r :state)))))))

(deftest independently-predicted-manifest-establishment-matches-xtdb
  (let [r (temporal/replay manifest-transactions)
        tables [outbox/intent-table :gesso_publication_manifests]]
    (is (= :predicted (:status r)))
    (with-open [node (xtn/start-node {})]
      (doseq [[tx expected] (map vector manifest-transactions (:history r))]
        (let [id (:tx-id (xt/submit-tx node (:ops tx) {:system-time (:system-time tx)}))]
          (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 10))
          (is (= [{:committed (:committed? expected)}]
                 (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" id])))))
      (doseq [s (range 0 11) v (range 0 12) table tables]
        (let [rows (xt/q node [(str "SELECT * FROM " (name table) " FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ?")
                               (sys v) (sys s)])]
          (is (= (count rows) (count (set (map :xt/id rows)))))
          (is (= (get (temporal/visible-at r (sys s) (sys v)) table {})
                 (into {} (map (juxt :xt/id identity)) rows)))))
      (is (= (frequencies (coalesce-valid (:rectangles r)))
             (frequencies (coalesce-valid (mapcat #(provider-rectangles node %) tables))))))))

(defn- observed-at [r s v id]
  (temporal/observe-outbox-at r (worker-time s) (worker-time v) id))

(deftest independent-observed-lease-operands
  (let [r (temporal/replay timestamp-transactions)
        old (lease-guard :active intent-id (utc-time (worker-time 1)) (utc-time first-deadline))
        new (lease-guard :expired intent-id (utc-time (worker-time 181)) (utc-time next-deadline))
        check #(temporal/lease-fence-correspondence r (worker-time %1) (worker-time %2) %3)]
    (is (= :predicted (:status (observed-at r 0 0 intent-id))))
    (is (= pending-intent (dissoc (:row (observed-at r 0 0 intent-id)) :xt/system-from)))
    (is (= (assoc first-lease :xt/system-from (utc-time (worker-time 1)))
           (:row (observed-at r 183 180 intent-id))))
    (is (= (assoc decoded-next-lease :xt/system-from (utc-time (worker-time 181)))
           (:row (observed-at r 181 181 intent-id))))
    (is (= :predicted (:status (observed-at r 1 1 other-intent-id))))
    (is (nil? (:row (observed-at r 1 1 other-intent-id))))
    (is (= :unsupported (:status (observed-at r 184 184 intent-id))))
    (is (= :unsupported (:status (observed-at r 1 1 "id"))))
    (is (= :matched (:status (check 1 1 old)) (:status (check 183 180 old))))
    (is (= :matched (:status (check 181 181 new))))
    (is (= :row-version (:reason (check 181 181 old))))
    (is (= :lease-deadline (:reason (check 1 1 (assoc old 3 (utc-time next-deadline))))))
    (is (= :not-leased (:reason (check 0 0 old)) (:reason (check 183 183 new))))
    (is (= :none (:authority (check 1 1 old))))
    (doseq [guard [(pop old) (assoc old 1 "id") (assoc old 2 (worker-time 1))
                   (assoc old 3 nil) (version-guard intent-id (worker-time 1))]]
      (is (= :unsupported (:status (check 1 1 guard)))))))

(deftest independently-predicted-worker-observations-match-xtdb
  (let [r (temporal/replay timestamp-transactions)
        fence {:intent-id intent-id :lease-id other-intent-id :worker-id "worker-a"}]
    (with-open [node (xtn/start-node {})]
      (doseq [[tx expected] (map vector timestamp-transactions (:history r))]
        (let [id (:tx-id (xt/submit-tx node (:ops tx) {:system-time (:system-time tx)}))]
          (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 10))
          (is (= [{:committed (:committed? expected)}]
                 (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" id])))))
      (let [read-at (fn [s v id]
                      (xt/q node ["SELECT *, _system_from FROM gesso_live_outbox FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ? WHERE _id = ?"
                                  (worker-time v) (worker-time s) id]))]
        (doseq [s [0 1 3 181 182 183] v [0 1 180 181 182 183] id [intent-id other-intent-id]]
          (let [predicted (observed-at r s v id)]
            (is (= :predicted (:status predicted)))
            (is (= (if-let [row (:row predicted)] [row] []) (read-at s v id)))))
        ;; Real pinned provider read, not evidence of frontier acquisition.
        (with-redefs [frontier/q-from! (fn [_ [_ id]] (read-at 3 3 id))]
          (let [guard (first (acquisition/active-lease-guard-ops node fence))]
            (is (= :matched (:status (temporal/lease-fence-correspondence
                                      r (worker-time 3) (worker-time 3) guard))))))
        ;; Capture reclamation operations, without claiming a real commit.
        (let [captured (atom nil)
              claim {:intent-id intent-id :lease-id intent-id :worker-id "worker-b"
                     :now (worker-time 181) :lease-until next-deadline}]
          (with-redefs [frontier/q-from! (fn [_ [_ id]] (read-at 3 3 id))
                        live.xtdb/execute-tx! (fn [_ ops] (reset! captured ops) {})]
            (acquisition/reclaim-expired! node claim))
          (is (= 4 (count @captured)))
          (is (= :matched (:status (temporal/lease-fence-correspondence
                                    r (worker-time 3) (worker-time 3) (first @captured)))))
          (is (= next-lease (nth (last @captured) 2))))))))

(defn- pending-claim [id now deadline]
  {:intent-id id :lease-id other-intent-id :worker-id "worker-a"
   :source-id "reference-source" :now (worker-time now) :lease-until (worker-time deadline)})

(def ^:private acquisition-seed
  {:system-time (worker-time 0)
   :ops [[:put-docs outbox/intent-table pending-intent
          (source-intent other-intent-id "reference-source")]]})

(def ^:private acquisition-cases
  ;; [observation time, transaction time, claim, expected outcome, reason]
  [[0 1 (pending-claim third-intent-id 0 180) :unavailable :missing]
   [0 1 (assoc (pending-claim intent-id 0 180) :source-id "wrong") :unavailable :source-mismatch]
   [0 1 (pending-claim intent-id 0 180) :acquired nil]
   [1 2 (pending-claim intent-id 1 180) :unavailable :not-pending]
   [0 2 (pending-claim intent-id 1 180) :contended nil]
   [2 3 (pending-claim other-intent-id 2 3) :contended nil]
   [3 4 (pending-claim other-intent-id 5 305) :contended nil]
   [4 5 (pending-claim other-intent-id 5 305) :acquired nil]])

(deftest independently-composed-pending-acquisition
  (let [prefix [acquisition-seed]
        claim (pending-claim intent-id 0 180)
        predict #(temporal/predict-pending-acquisition prefix (worker-time 0) (worker-time 0)
                                                       (worker-time 1) %)]
    (doseq [invalid [(assoc claim :worker-id "") (assoc claim :source-id nil)
                     (assoc claim :now nil) (assoc claim :lease-until (worker-time 0))
                     (assoc claim :lease-until (worker-time 301)) (assoc claim :extra true)]]
      (is (= :unsupported (:status (predict invalid))))
      (is (not (contains? (predict invalid) :ops))))
    (let [r (predict claim)]
      (is (= :predicted (:status r)))
      (is (= :acquired (:outcome r)))
      (is (= :none (:authority r)))
      (is (= :not-issued (:certificate r)))
      (is (= 4 (count (:ops r))))
      (is (= :leased (get-in (temporal/visible-at (:replay r) (worker-time 1) (worker-time 1))
                             [outbox/intent-table intent-id :gesso.live.outbox/status]))))))

(deftest composed-pending-acquisition-matches-worker-and-xtdb
  (with-open [node (xtn/start-node {})]
    (letfn [(submit! [time ops]
              (let [id (:tx-id (xt/submit-tx node ops {:system-time time}))]
                (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 10))
                (:committed (first (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" id])))))]
      (is (true? (submit! (:system-time acquisition-seed) (:ops acquisition-seed))))
      (let [execute! live.xtdb/execute-tx!
            prefix
            (reduce (fn [prefix [observed tx-time claim outcome reason]]
                      (let [s (worker-time observed) time (worker-time tx-time)
                            predicted (temporal/predict-pending-acquisition prefix s s time claim)
                            captured (atom [])
                            actual (with-redefs [frontier/q-from! (fn [_ [_ id]]
                                                                  (xt/q node ["SELECT *, _system_from FROM gesso_live_outbox FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ? WHERE _id = ?" s s id]))
                                                 live.xtdb/execute-tx! (fn [connectable ops]
                                                                         (swap! captured conj ops)
                                                                         (execute! connectable ops {:system-time time}))]
                                     (acquisition/acquire-pending! node claim))]
                        (is (= :predicted (:status predicted)))
                        (is (= outcome (:outcome predicted) (:status actual)))
                        (is (= reason (:reason predicted) (:reason actual)))
                        (is (= (if (seq (:ops predicted)) [(:ops predicted)] []) @captured))
                        (is (= (frequencies (coalesce-valid (:rectangles (:replay predicted))))
                               (frequencies (coalesce-valid (provider-rectangles node outbox/intent-table)))))
                        (if (seq (:ops predicted))
                          (conj prefix {:system-time time :ops (:ops predicted)})
                          prefix)))
                    [acquisition-seed] acquisition-cases)]
        (is (= 6 (count prefix)))))))
