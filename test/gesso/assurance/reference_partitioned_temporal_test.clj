(ns gesso.assurance.reference-partitioned-temporal-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-transaction :as current]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.assurance.reference-temporal-fixtures :refer [sys provider-rectangles coalesce-valid]]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.mapped-frame :as frame]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.time Duration)))

(deftest partitioned-journal-at-tentative-temporal-basis
  (let [binding {:command-id (identity/command-id "c1")
                 :principal (identity/principal "helper-a")
                 :operation :request/claim :arguments {:request-id "r"}}
        items (into [1 :claim/open "open"] (range 3 32))
        row (resolution/command-record binding {:status :claimed :details {:empty {} :items items}} "t1")
        changed (assoc-in row [:gesso.command/outcome :details :items 16] 999)
        mapping {:target-map {:outcome [:journal-slot "helper-a" :request/claim]}}
        prefix (frame/formatted-prestate-assertions! mapping {} [row])
        changed-prefix (frame/formatted-prestate-assertions! mapping {} [changed])
        put (fn [from to doc]
              [:put-docs (cond-> {:into :gesso_commands :valid-from (sys from)}
                           to (assoc :valid-to (sys to))) doc])
        probe [:put-docs :guard_probe {:xt/id "probe" :written true}]
        guard ["ASSERT CURRENT_TIMESTAMP < ?" (sys 10)]
        transactions (mapv (fn [n ops] {:system-time (sys n) :ops ops}) (range 1 10)
                           [[(put 3 7 row)] prefix prefix
                            (into [(put 4 6 changed) probe guard ["ASSERT TRUE"]] prefix)
                            (into [(put 8 nil changed)] prefix)
                            (into [guard ["ASSERT TRUE"]] prefix)
                            prefix prefix (conj changed-prefix probe)])
        prediction (temporal/replay transactions)
        expected-failure (:failed-assertion-index
                          (current/execute {:gesso_commands {(:xt/id changed) changed}} prefix))
        scope #{:gesso_commands :guard_probe}]
    (is (> (count prefix) 9))
    (is (= #{:gesso_commands} (current/operation-sequence-tables prefix)))
    (is (= scope (temporal/required-observation-tables transactions)))
    (is (= :predicted (:status prediction)) (pr-str (dissoc prediction :history :rectangles)))
    (is (= [true false true false true true false false true] (mapv :committed? (:history prediction))))
    (is (pos-int? expected-failure))
    (is (= [0 (+ 4 expected-failure) 0 expected-failure]
           (mapv :failed-assertion-index (filter #(not (:committed? %)) (:history prediction)))))
    (is (= :none (:authority prediction)))
    (is (= :not-issued (:certificate prediction)))
    (is (= :unsupported (:status (temporal/replay-equal-time transactions))))
    (is (= row (get-in (temporal/visible-at prediction (sys 4) (sys 4)) [:gesso_commands (:xt/id row)])))
    (is (= row (get-in (temporal/visible-at prediction (sys 9) (sys 6)) [:gesso_commands (:xt/id row)])))
    (is (nil? (get-in (temporal/visible-at prediction (sys 9) (sys 7)) [:gesso_commands (:xt/id row)])))
    (is (= changed (get-in (temporal/visible-at prediction (sys 9) (sys 8)) [:gesso_commands (:xt/id row)])))
    (is (nil? (get-in (temporal/visible-at prediction (sys 8) (sys 8)) [:guard_probe "probe"])))
    (is (= {:xt/id "probe" :written true} (get-in (temporal/visible-at prediction (sys 9) (sys 9)) [:guard_probe "probe"])))
    (doseq [bad [(subvec prefix 2)
                 (assoc prefix 3 (pop (nth prefix 3)))
                 (into (subvec prefix 0 3) (cons probe (subvec prefix 3)))
                 (into (subvec prefix 0 3) (cons guard (subvec prefix 3)))]]
      (let [txs [(first transactions) {:system-time (sys 4) :ops (into [["ASSERT FALSE"]] bad)}]
            r (temporal/replay txs)]
        (is (nil? (temporal/required-observation-tables txs)))
        (is (= :unsupported (:status r)) (pr-str r))
        (is (not (contains? r :history)))))
    (with-open [node (xtn/start-node {})]
      (doseq [[tx expected] (map vector transactions (:history prediction))]
        (let [id (:tx-id (xt/submit-tx node (:ops tx) {:system-time (:system-time tx)}))]
          (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 15))
          (is (= [{:committed (:committed? expected)}]
                 (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" id])) (pr-str (:system-time tx)))))
      (is (= (frequencies (coalesce-valid (:rectangles prediction)))
             (frequencies (coalesce-valid (mapcat #(provider-rectangles node %) scope)))))
      (doseq [s [2 3 4 5 9] v [2 3 4 6 7 8 9] table scope]
        (let [rows (xt/q node [(str "SELECT * FROM " (name table)
                                   " FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ?") (sys v) (sys s)])]
          (is (= (get (temporal/visible-at prediction (sys s) (sys v)) table {})
                 (into {} (map (juxt :xt/id identity)) rows)) (str s "/" v " " table)))))))
