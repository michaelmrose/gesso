(ns gesso.assurance.reference-transaction-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-transaction :as reference]
            [gesso.assurance.xtdb-envelope :as envelope]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.time Duration)))

(def ^:private a {:xt/id "a" :value 1 :old true})
(def ^:private b {:xt/id "b" :value 2})
(def ^:private replacement {:xt/id "a" :value 3})
(def ^:private seed {:refprobe {"a" a} :refother {"a" b}})
;; Use matching keys for the canonical state, including same id across tables.
(def ^:private initial (assoc-in seed [:refother "a"] (assoc b :xt/id "a")))
(def ^:private put-a [:put-docs :refprobe replacement])
(def ^:private put-b [:put-docs :refprobe b])
(def ^:private put-other [:put-docs :refother {:xt/id "a" :value 9}])

(deftest independent-current-state-execution
  (let [run #(reference/execute initial %)
        committed (run [put-a ["ASSERT TRUE"] put-b put-other])
        aborted (run [put-a put-other ["ASSERT FALSE"] put-b])]
    (is (= :predicted (:status committed)))
    (is (true? (:committed? committed)))
    (is (= {:refprobe {"a" replacement "b" b}
            :refother {"a" {:xt/id "a" :value 9}}} (:state committed)))
    (is (= replacement (get-in (:state committed) [:refprobe "a"])))
    (is (= :predicted (:status aborted)))
    (is (false? (:committed? aborted)))
    (is (= 2 (:failed-assertion-index aborted)))
    (is (= initial (:state aborted)))
    (is (= initial (:state (run [["ASSERT TRUE"]]))))
    (is (= 0 (:failed-assertion-index (run [["ASSERT FALSE"] put-a]))))
    (is (= a (get-in (:state (run [put-a [:put-docs :refprobe a]])) [:refprobe "a"])))
    (is (= :none (:authority committed)))
    (is (= :not-issued (:certificate committed)))))

(def ^:private uuid-a #uuid "00000000-0000-0000-0000-000000000001")
(def ^:private uuid-b #uuid "00000000-0000-0000-0000-000000000002")

(def ^:private equality-cases
  [[0 0 true] [Long/MIN_VALUE Long/MAX_VALUE false]
   [Long/MAX_VALUE Long/MAX_VALUE true] [-1 1 false]
   [true true true] [false false true] [true false false]
   ["" "" true] ["open" "Open" false] ["café" "café" true]
   ["café" "cafe\u0301" false] ["a" "a " false]
   [:open :open true] [:open :closed false]
   [:claim/status :claim/status true] [:claim/status :other/status false]
   [uuid-a uuid-a true] [uuid-a uuid-b false]])

(deftest independent-parameterized-equality
  (doseq [[left right expected] equality-cases]
    (let [assertion ["ASSERT ? = ?" left right]
          predicted (reference/execute initial [put-a assertion put-other])]
      (is (= :predicted (:status predicted)) (pr-str assertion))
      (is (= expected (:committed? predicted)) (pr-str assertion))
      (is (= (if expected (-> initial
                              (assoc-in [:refprobe "a"] replacement)
                              (assoc-in [:refother "a"] {:xt/id "a" :value 9}))
                 initial) (:state predicted)))
      (is (= (when-not expected 1) (:failed-assertion-index predicted)))
      (is (= :none (:authority predicted)))
      (is (= :not-issued (:certificate predicted))))))

(defn- read-guard [table id field expected]
  [(str "ASSERT EXISTS (SELECT 1 FROM \"" (name table)
        "\" WHERE _id = ? AND \"" (name field) "\" = ?)") id expected])

(def ^:private read-cases
  ;; Each case establishes its own relevant starting value.
  (let [reset-a [:put-docs :refprobe a]
        g #(read-guard :refprobe "a" :value %)]
    [[true [reset-a (g 1) put-a (g 3) put-other]]
     [false [reset-a put-a (g 1) put-other]]
     [true [reset-a put-a (g 3) reset-a (g 1)]]
     [false [reset-a put-a (g 3) reset-a (g 3)]]
     [true [put-b (read-guard :refprobe "b" :value 2) put-other]]
     [false [put-a (read-guard :refprobe "absent" :value 3) put-other]]
     [false [reset-a put-a (read-guard :refprobe "a" :old true)]]
     [true [reset-a (read-guard :refprobe "a" :old true) put-a]]
     [true [put-other put-a (read-guard :refother "a" :value 9) (g 3)]]
     [false [put-other put-a (read-guard :refother "a" :value 3)]]
     [true [[:put-docs :refprobe {:xt/id "a" :value "café"}] (g "café")]]
     [false [[:put-docs :refprobe {:xt/id "a" :value "café"}] (g "cafe\u0301")]]
     [true [[:put-docs :refprobe {:xt/id "a" :value false}] (g false)]]
     [false [[:put-docs :refprobe {:xt/id "a" :value false}] (g true)]]]))

(deftest independent-transaction-internal-reads
  (doseq [[expected ops] read-cases]
    (let [r (reference/execute initial ops)]
      (is (= :predicted (:status r)) (pr-str ops))
      (is (= expected (:committed? r)) (pr-str ops))
      (if expected
        (is (not (contains? r :failed-assertion-index)))
        (do (is (= initial (:state r)))
            (is (string? (first (nth ops (:failed-assertion-index r)))))))
      (is (= :none (:authority r)))
      (is (= :not-issued (:certificate r)))))
  (let [r (reference/execute initial [put-a (read-guard :refprobe "a" :value 3)])]
    (is (= replacement (get-in (:state r) [:refprobe "a"]))))
  (let [r (reference/execute initial [put-a (read-guard :refprobe "a" :value 1)])]
    (is (= 1 (:failed-assertion-index r)))))

(deftest unsupported-reference-input-does-not-predict
  (doseq [ops [[] [(read-guard :refprobe "a" :value nil)]
               [(read-guard :refprobe "a" :value "1")]
               [(read-guard :refprobe "a" :value :open)]
               [(read-guard :xt_txs "a" :value 1)]
               [(read-guard :refprobe "a" :bad_field 1)]
               [(read-guard :refprobe "" :value 1)]
               [(pop (read-guard :refprobe "a" :value 1))]
               [["ASSERT FALSE"] (read-guard :refprobe "a" :value nil)]
               [["ASSERT ? = ?" 1]] [["ASSERT ? = ?" 1 1 1]]
               [["ASSERT ? = ?" 1 "1"]] [["ASSERT ? = ?" :open "open"]]
               [["ASSERT ? = ?" nil nil]] [["ASSERT ? = ?" 1.0 1.0]]
               [["ASSERT ? = ?" 1N 1N]] [["ASSERT ? = ?" [1] [1]]]
               [["ASSERT ? = ?" (apply str (repeat 4097 "a")) ""]]
               [["ASSERT ? = ?" (str (char 0xd800)) "?"]]
               [["ASSERT FALSE"] ["ASSERT ? = ?" 1 nil]]
               [["ASSERT ? = ?" 1 2] ["ASSERT ? = ?" [] []]]
               [["ASSERT FALSE"] ["ASSERT arbitrary"]]
               [[:delete-docs :refprobe "a"]]
               [[:put-docs {:into :refprobe :valid-from :now} a]]
               [[:put-docs :refprobe (assoc a :xt/valid-to 1)]]
               [[:put-docs :refprobe (assoc a :value nil)]]
               [[:put-docs :refprobe (assoc a :value {:nested 1})]]
               [[:put-docs :refprobe (assoc a :xt/id 1)]]
               [[:put-docs :refprobe a replacement]]
               [[:put-docs :refprobe]] [[:put-docs :xt_txs a]]
               [[:put-docs :refprobe (assoc a :alias_key 1)]]]]
    (let [r (reference/execute initial ops)]
      (is (= :unsupported (:status r)) (pr-str ops))
      (is (not (contains? r :committed?)))
      (is (not (contains? r :state)))))
  (doseq [state [seed {:refprobe {}} {:refprobe {"a" (assoc a :value nil)}} nil]]
    (is (= :unsupported-reference-prestate
           (:reason (reference/execute state [["ASSERT TRUE"]]))))))

(defn- provider-state [node]
  (into {} (keep (fn [table]
                   (let [rows (xt/q node (str "SELECT * FROM " (name table)))]
                     (when (seq rows)
                       (let [indexed (into {} (map (juxt :xt/id identity)) rows)]
                         (is (= (count rows) (count indexed)) "No duplicate visible document IDs")
                         [table indexed])))))
        [:refprobe :refother]))

(deftest reference-predictions-match-selected-real-xtdb-executions
  (with-open [node (xtn/start-node {})]
    (let [ops [[:put-docs :refprobe a] [:put-docs :refother (assoc b :xt/id "a")]]
          start (reference/execute {} ops)
          _ (xt/execute-tx node ops)]
      (is (= initial (:state start) (provider-state node)))
      ;; Each reference prediction uses its OWN previous predicted state,
      ;; never a readback from the provider. Compare complete row maps.
      (loop [state (:state start)
             cases (into [[["ASSERT TRUE"]]
                    [put-a put-b put-other]
                    [[:put-docs :refprobe a] put-a]
                    [["ASSERT FALSE"] put-a]
                    [[:put-docs :refprobe {:xt/id "c" :value 4}] put-other ["ASSERT FALSE"]]
                    [[:put-docs :refprobe a b] ["ASSERT TRUE"]]
                    [put-a ["ASSERT FALSE"] [:put-docs :refother {:xt/id "z" :value 7}]]
                    [put-other ["ASSERT TRUE"] put-a]]
                         (concat
                          (map-indexed
                           (fn [i [left right _]]
                             ;; Distinct writes around the assertion expose
                             ;; wrong commits and incomplete rollback.
                             [[:put-docs :refprobe {:xt/id "a" :value (+ 100 i)}]
                              ["ASSERT ? = ?" left right]
                              [:put-docs :refother {:xt/id "a" :value (+ 200 i)}]])
                           equality-cases)
                          (map second read-cases)))]
        (when-let [ops (first cases)]
          (let [predicted (reference/execute state ops)
                id (:tx-id (xt/submit-tx node ops))
                _ (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 10))
                outcomes (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" id])]
            (is (= :predicted (:status predicted)))
            (is (= [{:committed (:committed? predicted)}] outcomes) (pr-str ops))
            (is (= (:state predicted) (provider-state node)) (pr-str ops))
            (is (= :matched (:status (envelope/check-envelope! node id ops (:committed? predicted)))))
            (recur (:state predicted) (next cases))))))))
