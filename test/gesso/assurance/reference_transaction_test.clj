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

(deftest unsupported-reference-input-does-not-predict
  (doseq [ops [[] [["ASSERT ? = ?" 1 1]] [["ASSERT FALSE"] ["ASSERT arbitrary"]]
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
             cases [[["ASSERT TRUE"]]
                    [put-a put-b put-other]
                    [[:put-docs :refprobe a] put-a]
                    [["ASSERT FALSE"] put-a]
                    [[:put-docs :refprobe {:xt/id "c" :value 4}] put-other ["ASSERT FALSE"]]
                    [[:put-docs :refprobe a b] ["ASSERT TRUE"]]
                    [put-a ["ASSERT FALSE"] [:put-docs :refother {:xt/id "z" :value 7}]]
                    [put-other ["ASSERT TRUE"] put-a]]]
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
