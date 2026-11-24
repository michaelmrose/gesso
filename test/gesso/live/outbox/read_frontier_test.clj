(ns gesso.live.outbox.read-frontier-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.outbox.read-frontier :as frontier]
            [xtdb.api :as xt]
            [xtdb.basis :as basis]
            [xtdb.node :as xtn]))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest projection-waits-before-observing-and-pins-the-actual-read
  (let [node :selected
        ctx {:xtdb/connectable node :xtdb/read-connectable :other}
        token (basis/->tx-basis-str {"xtdb" [42]})
        opts {:await-token token}
        query ["SELECT status FROM requests"]
        calls (atom [])]
    (with-redefs [xt/status (fn [n]
                             (swap! calls conj [:status n])
                             {:latest-submitted-msg-ids {"xtdb" [42]}})
                  xtdb/q-from (fn [n q o]
                                (swap! calls conj [:query n q o])
                                (if (= q ["SELECT 1 AS ready"])
                                  [{:ready 1}]
                                  [{:status :claimed}]))
                  xtdb/latest-completed-basis (fn [n]
                                               (swap! calls conj [:basis n])
                                               :completed)
                  xtdb/basis-snapshot-token (constantly "snapshot")]
      (is (= {:basis :completed :rows [{:status :claimed}]}
             (frontier/q-with-basis! ctx query)))
      (is (= [[:status node]
              [:query node ["SELECT 1 AS ready"] opts]
              [:basis node]
              [:query node query (assoc opts :snapshot-token "snapshot")]]
             @calls))
      (reset! calls [])
      (with-redefs [xtdb/latest-completed-basis (constantly nil)]
        (is (= :missing-completed-basis (reason #(frontier/q-with-basis! ctx query))))
        (is (= [[:status node] [:query node ["SELECT 1 AS ready"] opts]] @calls)))
      (with-redefs [xtdb/q-from (constantly [])]
        (is (= :invalid-readiness-observation (reason #(frontier/q-with-basis! ctx query)))))
      (let [interrupted (InterruptedException. "projection interrupted")]
        (try
          (with-redefs [xtdb/latest-completed-basis (fn [_] (throw interrupted))]
            (is (= [interrupted true]
                   (try (frontier/q-with-basis! ctx query)
                        nil (catch Exception e [e (.isInterrupted (Thread/currentThread))])))))
          (finally (Thread/interrupted)))))))

(deftest physical-projection-is-pinned-despite-a-later-commit
  (with-open [node (xtn/start-node {})]
    (xt/submit-tx node [[:put-docs :requests {:xt/id "r1" :status :claimed}]])
    (let [observe xtdb/latest-completed-basis
          seen (atom nil)
          result (with-redefs [xtdb/latest-completed-basis
                               (fn [n]
                                 (let [b (observe n nil)]
                                   (reset! seen b)
                                   ;; Simulate another command after basis acquisition.
                                   (xt/execute-tx node [[:put-docs :requests {:xt/id "r1" :status :done}]])
                                   b))]
                   (frontier/q-with-basis! {:xtdb/connectable node}
                                           ["SELECT status FROM requests WHERE _id = ?" "r1"]))]
      (is (xtdb/xtdb-basis? (:basis result)))
      (is (= @seen (:basis result)))
      (is (= [{:status :claimed}] (:rows result)))
      (is (= [{:status :done}]
             (:rows (frontier/q-with-basis! node ["SELECT status FROM requests WHERE _id = ?" "r1"]))))
      (is (pos? (xtdb/compare-bases (observe node) (:basis result)))))))
