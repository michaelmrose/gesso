(ns gesso.assurance.reference-worker-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.assurance.reference-temporal-fixtures
             :refer [day sys put a b c d provider-at rectangle provider-rectangles coalesce-valid]]
            [gesso.assurance.reference-transaction :as current]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.acknowledgment :as acknowledgment]
            [gesso.live.outbox.read-frontier :as frontier]
            [gesso.live.consistency.xtdb :as live.xtdb]
            [gesso.live.outbox.publication-manifest :as manifest]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.time Duration Instant)))

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

(def ^:private reclamation-seed
  [acquisition-seed {:system-time (worker-time 1) :ops acquire-operations}])
(defn- reclaim-claim [id now deadline]
  (assoc (pending-claim id now deadline) :lease-id intent-id :worker-id "worker-b"))
(def ^:private reclamation-cases
  [[1 2 (reclaim-claim third-intent-id 1 180) :unavailable :missing]
   [1 2 (reclaim-claim other-intent-id 1 180) :unavailable :not-leased]
   [1 2 (assoc (reclaim-claim intent-id 1 180) :source-id "wrong") :unavailable :source-mismatch]
   [1 2 (assoc (reclaim-claim intent-id 1 180) :lease-id other-intent-id) :rejected :reused-lease-id]
   [1 180 (reclaim-claim intent-id 180 360) :contended nil]
   [180 181 (reclaim-claim intent-id 180 181) :contended nil]
   [181 182 (reclaim-claim intent-id 183 483) :contended nil]
   [182 183 (reclaim-claim intent-id 183 483) :reclaimed nil]
   [1 184 (reclaim-claim intent-id 184 484) :contended nil]
   [183 185 (assoc (reclaim-claim intent-id 185 485) :lease-id third-intent-id) :contended nil]
   [185 483 (assoc (reclaim-claim intent-id 483 600) :lease-id third-intent-id) :reclaimed nil]])

(deftest independently-composed-expired-reclamation
  (let [claim (reclaim-claim intent-id 180 360)
        predict #(temporal/predict-expired-reclamation reclamation-seed (worker-time 1) (worker-time 1) %1 %2)
        exact (predict first-deadline claim)]
    (is (= :predicted (:status exact)))
    (is (= :reclaimed (:outcome exact)))
    (is (= :none (:authority exact)))
    (is (= :not-issued (:certificate exact)))
    (is (= (utc-time first-deadline) (get-in exact [:ops 0 3])))
    (is (= :contended (:outcome (predict (.minusNanos first-deadline 1000) claim))))
    (is (= :reused-lease-id (:reason (predict first-deadline (assoc claim :lease-id other-intent-id)))))
    (is (= :unsupported (:status (predict first-deadline (assoc claim :lease-id nil)))))
    (let [malformed (conj reclamation-seed {:system-time (worker-time 2)
                                           :ops [[:put-docs outbox/intent-table
                                                  (dissoc first-lease :gesso.live.outbox/lease-id)]]})
          r (temporal/predict-expired-reclamation malformed (worker-time 2) (worker-time 2)
                                                (worker-time 181) claim)]
      (is (= :unsupported (:status r)))
      (is (not (contains? r :ops))))))

(deftest composed-expired-reclamation-matches-worker-and-xtdb
  (with-open [node (xtn/start-node {})]
    (doseq [tx reclamation-seed]
      (let [id (:tx-id (xt/submit-tx node (:ops tx) {:system-time (:system-time tx)}))]
        (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 10))
        (is (= [{:committed true}] (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" id])))))
    (let [execute! live.xtdb/execute-tx!
          prefix
          (reduce (fn [prefix [observed tx-time claim outcome reason]]
                    (let [s (worker-time observed) time (worker-time tx-time)
                          predicted (temporal/predict-expired-reclamation prefix s s time claim)
                          captured (atom [])
                          actual (with-redefs [frontier/q-from! (fn [_ [_ id]]
                                                                (xt/q node ["SELECT *, _system_from FROM gesso_live_outbox FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ? WHERE _id = ?" s s id]))
                                               live.xtdb/execute-tx! (fn [connectable ops]
                                                                       (swap! captured conj ops)
                                                                       (execute! connectable ops {:system-time time}))]
                                   (try (acquisition/reclaim-expired! node claim)
                                        (catch clojure.lang.ExceptionInfo e
                                          (if (= :gesso.live.outbox/invalid-acquisition (:error/type (ex-data e)))
                                            {:status :rejected :reason (:reason (ex-data e))}
                                            (throw e)))))]
                      (is (= :predicted (:status predicted)))
                      (is (= outcome (:outcome predicted) (:status actual)))
                      (is (= reason (:reason predicted) (:reason actual)))
                      (is (= (if (seq (:ops predicted)) [(:ops predicted)] []) @captured))
                      (is (= (frequencies (coalesce-valid (:rectangles (:replay predicted))))
                             (frequencies (coalesce-valid (provider-rectangles node outbox/intent-table)))))
                      (if (seq (:ops predicted))
                        (conj prefix {:system-time time :ops (:ops predicted)}) prefix)))
                  reclamation-seed reclamation-cases)]
      (is (= 9 (count prefix))))))

(def ^:private owner-fence {:intent-id intent-id :lease-id other-intent-id :worker-id "worker-a"})
(def ^:private fence-cases
  ;; [observed seconds, tx instant, fence, writes, outcome, reason]
  (let [marker (fn [v] [[:put-docs :temporalprobe {:xt/id "marker" :value v}]])]
    [[1 (worker-time 2) (assoc owner-fence :intent-id third-intent-id) (marker 1) :rejected :missing-lease]
     [1 (worker-time 2) (assoc owner-fence :intent-id other-intent-id) (marker 1) :rejected :not-leased]
     [1 (worker-time 2) (assoc owner-fence :lease-id intent-id) (marker 1) :rejected :stale-lease]
     [1 (worker-time 2) (assoc owner-fence :worker-id "worker-b") (marker 1) :rejected :stale-lease]
     [1 (worker-time 2) owner-fence (conj (marker 2) [:put-docs outbox/intent-table first-lease]) :committed nil]
     [1 (worker-time 3) owner-fence (marker 3) :contended nil]
     [2 (worker-time 4) owner-fence (marker 4) :committed nil]
     [4 (.minusNanos first-deadline 1000) owner-fence (marker 5) :committed nil]
     [4 first-deadline owner-fence (marker 6) :contended nil]
     [4 (worker-time 181) owner-fence (marker 7) :contended nil]]))

(deftest independently-composed-active-fence
  (let [prior (temporal/replay reclamation-seed)
        predict #(temporal/predict-active-fence prior (worker-time 1) (worker-time 1) %)]
    (is (= :guarded (:outcome (predict owner-fence))))
    (is (= :none (:authority (predict owner-fence))))
    (doseq [f [(dissoc owner-fence :lease-id) (assoc owner-fence :worker-id "")
               (assoc owner-fence :extra true) (assoc owner-fence :intent-id "id")]]
      (is (= :unsupported (:status (predict f)))))
    (let [r (temporal/predict-active-fenced-transaction reclamation-seed
              (worker-time 1) (worker-time 1) (worker-time 2) owner-fence
              [["ASSERT TRUE"]])]
      (is (= :unsupported (:status r)))
      (is (not (contains? r :ops))))))

(deftest composed-active-fencing-matches-worker-and-xtdb
  (with-open [node (xtn/start-node {})]
    (doseq [tx reclamation-seed]
      (let [id (:tx-id (xt/submit-tx node (:ops tx) {:system-time (:system-time tx)}))]
        (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 10))
        (is (= [{:committed true}] (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" id])))))
    (let [prefix
          (reduce (fn [prefix [observed time fence writes outcome reason]]
                    (let [s (worker-time observed)
                          predicted (temporal/predict-active-fenced-transaction prefix s s time fence writes)
                          built (try
                                  {:ops (with-redefs [frontier/q-from! (fn [_ [_ id]]
                                            (xt/q node ["SELECT *, _system_from FROM gesso_live_outbox FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ? WHERE _id = ?" s s id]))]
                                          (acquisition/active-lease-guard-ops node fence))}
                                  (catch clojure.lang.ExceptionInfo e
                                    (if (= :gesso.live.outbox/invalid-acquisition (:error/type (ex-data e)))
                                      {:reason (:reason (ex-data e))} (throw e))))
                          ops (if (:ops built) (into (:ops built) writes) [])
                          actual (if (empty? ops) :rejected
                                   (try (live.xtdb/execute-tx! node ops {:system-time time}) :committed
                                        (catch Throwable e
                                          (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data e)))
                                            :contended (throw e)))))]
                      (is (= :predicted (:status predicted)))
                      (is (= outcome (:outcome predicted) actual))
                      (is (= reason (:reason predicted) (:reason built)))
                      (is (= (:ops predicted) ops))
                      (is (= (frequencies (coalesce-valid (:rectangles (:replay predicted))))
                             (frequencies (coalesce-valid (mapcat #(provider-rectangles node %)
                                                                  [outbox/intent-table :temporalprobe])))))
                      (if (seq (:ops predicted))
                        (conj prefix {:system-time time :ops (:ops predicted)}) prefix)))
                  reclamation-seed fence-cases)]
      (is (= 8 (count prefix))))))

(def ^:private expiry-intent-id #uuid "00000000-0000-0000-0000-000000000004")
(def ^:private ack-submission
  (assoc owner-fence :receipt {:receipt-id third-intent-id :adapter :test/adapter :result :accepted}))
(def ^:private acknowledgment-seed
  (conj reclamation-seed {:system-time (worker-time 2)
                         :ops [[:put-docs outbox/intent-table first-lease
                                (assoc first-lease :xt/id expiry-intent-id)]]}))
(def ^:private acknowledgment-cases
  [[2 (worker-time 3) (assoc ack-submission :intent-id third-intent-id) :unavailable :missing]
   [2 (worker-time 3) (assoc ack-submission :intent-id other-intent-id) :unavailable :not-leased]
   [2 (worker-time 3) (assoc ack-submission :lease-id intent-id) :rejected :stale-lease]
   [2 (worker-time 3) (assoc ack-submission :worker-id "worker-b") :rejected :stale-lease]
   [1 (worker-time 3) ack-submission :contended nil]
   [3 (worker-time 4) ack-submission :acknowledged nil]
   [4 (worker-time 5) ack-submission :unavailable :not-leased]
   [2 (worker-time 5) ack-submission :contended nil]
   [5 first-deadline (assoc ack-submission :intent-id expiry-intent-id) :contended nil]
   [5 (worker-time 181) (assoc ack-submission :intent-id expiry-intent-id) :contended nil]])

(deftest independently-composed-acknowledgment
  (let [predict #(temporal/predict-acknowledgment acknowledgment-seed
                   (worker-time 2) (worker-time 2) %1 %2)
        r (predict (.minusNanos first-deadline 1000) ack-submission)
        updated (nth (last (:ops r)) 2)]
    (is (= :acknowledged (:outcome r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= (assoc first-lease :gesso.live.outbox/status :dispatch-accepted
                  :gesso.live.outbox/dispatch-receipt-id third-intent-id
                  :gesso.live.outbox/dispatch-adapter :test/adapter) updated))
    (is (= :contended (:outcome (predict first-deadline ack-submission))))
    (doseq [submission [(dissoc ack-submission :receipt)
                        (assoc ack-submission :extra true)
                        (assoc ack-submission :worker-id "")
                        (assoc-in ack-submission [:receipt :receipt-id] "receipt")
                        (assoc-in ack-submission [:receipt :adapter] :adapter)
                        (assoc-in ack-submission [:receipt :result] :failed)
                        (assoc-in ack-submission [:receipt :extra] true)]]
      (let [invalid (predict (worker-time 3) submission)]
        (is (= :unsupported (:status invalid)))
        (is (not (contains? invalid :ops)))
        (is (not (contains? invalid :replay)))))))

(deftest composed-acknowledgment-matches-worker-and-xtdb
  (with-open [node (xtn/start-node {})]
    (doseq [tx acknowledgment-seed]
      (let [id (:tx-id (xt/submit-tx node (:ops tx) {:system-time (:system-time tx)}))]
        (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 10))
        (is (= [{:committed true}] (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" id])))))
    (let [execute! live.xtdb/execute-tx!
          prefix
          (reduce (fn [prefix [observed time submission outcome reason]]
                    (let [s (worker-time observed)
                          predicted (temporal/predict-acknowledgment prefix s s time submission)
                          captured (atom [])
                          actual (with-redefs [frontier/q-from! (fn [_ [_ id]]
                                                                (xt/q node ["SELECT *, _system_from FROM gesso_live_outbox FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ? WHERE _id = ?" s s id]))
                                               live.xtdb/execute-tx! (fn [connectable ops]
                                                                       (swap! captured conj ops)
                                                                       (execute! connectable ops {:system-time time}))]
                                   (try (acknowledgment/acknowledge-dispatch! node submission)
                                        (catch clojure.lang.ExceptionInfo e
                                          (if (= :gesso.live.outbox/invalid-acknowledgment (:error/type (ex-data e)))
                                            {:status :rejected :reason (:reason (ex-data e))}
                                            (throw e)))))]
                      (is (= :predicted (:status predicted)))
                      (is (= outcome (:outcome predicted) (:status actual)))
                      (is (= reason (:reason predicted) (:reason actual)))
                      (is (= (if (seq (:ops predicted)) [(:ops predicted)] []) @captured))
                      (when (= :acknowledged outcome)
                        (is (= (select-keys submission [:intent-id :lease-id])
                               (select-keys actual [:intent-id :lease-id])))
                        (is (= (get-in submission [:receipt :receipt-id]) (:receipt-id actual))))
                      (is (= (frequencies (coalesce-valid (:rectangles (:replay predicted))))
                             (frequencies (coalesce-valid (provider-rectangles node outbox/intent-table)))))
                      (if (seq (:ops predicted))
                        (conj prefix {:system-time time :ops (:ops predicted)}) prefix)))
                  acknowledgment-seed acknowledgment-cases)]
      (is (= 8 (count prefix))))))
