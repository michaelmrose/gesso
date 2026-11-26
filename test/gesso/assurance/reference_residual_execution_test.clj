(ns gesso.assurance.reference-residual-execution-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-transaction :as current]
            [gesso.assurance.reference-temporal :as temporal]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import (java.time Instant)))

(def ^:private row
  {:xt/id "r1" :amount 2 :revision 0 :updated-at "t0" :enabled true
   :status :example/open :owner #uuid "00000000-0000-0000-0000-000000000001"
   :request/location-id "l1"})
(def ^:private state {:gesso_requests {"r1" row}})
(def ^:private guard
  ["ASSERT ? = (SELECT COUNT(*) FROM gesso_requests WHERE (_id = ?) AND (revision = ?) AND (amount = ?) AND (updated_at = ?) AND (enabled = ?) AND (status = ?) AND (owner = ?) AND (\"request$location_id\" = ?))"
   1 "r1" 0 2 "t0" true :example/open (:owner row) "l1"])
(def ^:private probe [:put-docs :guard_probe {:xt/id "probe" :written true}])
(defn- put [r] [:put-docs :gesso_requests r])
(defn- attempt! [node ops]
  (try (xt/execute-tx node ops) :committed
       (catch Exception e
         (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data e)))
           :assert-failed (throw e)))))

(deftest ordinary-residual-equality-physical-correspondence
  (is (= #{:gesso_requests} (current/operation-tables guard)))
  (is (= :predicted (:status (current/execute state [guard]))))
  (is (true? (:committed? (current/execute state [guard]))))
  (with-open [node (xtn/start-node {})]
    (is (= :committed (attempt! node [(put row)])))
    (is (= :committed (attempt! node [guard])))
    (doseq [changed [(assoc row :revision 1) (assoc row :amount 3)
                     (assoc row :updated-at "T0") (assoc row :enabled false)
                     (assoc row :status :example/closed)
                     (assoc row :owner #uuid "00000000-0000-0000-0000-000000000002")
                     (assoc row :request/location-id "l2") (dissoc row :request/location-id)]]
      (let [ops [(put changed) probe guard] result (current/execute state ops)]
        (is (= :predicted (:status result)) (pr-str result))
        (is (false? (:committed? result)))
        (is (= 2 (:failed-assertion-index result)))
        (is (= state (:state result)))
        (is (= :assert-failed (attempt! node ops)))
        (is (= [row] (xt/q node "SELECT * FROM gesso_requests")))
        (is (empty? (xt/q node "SELECT * FROM guard_probe")))))
    (is (= :assert-failed (attempt! node [(assoc guard 2 "missing")])))
    (is (= 0 (:failed-assertion-index (current/execute state [(assoc guard 2 "missing")]))))
    (is (= :committed (attempt! node [guard probe]))))
  ;; Unknown cross-domain coercion is not a predicted abort, even when
  ;; another field would fail. Structural checks apply before any abort.
  (doseq [changed [(assoc row :amount "2") (assoc row :status "example/open")
                   (assoc row :owner (str (:owner row))) (assoc row :amount [2])
                   (assoc row :revision 9 :enabled "true")]]
    (let [r (current/execute state [(put changed) guard])]
      (is (= :unsupported (:status r)))
      (is (not (contains? r :committed?)))))
  (doseq [bad [(assoc guard 1 1N) (pop guard) (assoc guard 2 2)
               (assoc guard 0 "ASSERT ? = (SELECT COUNT(*) FROM gesso_requests WHERE (_id = ?) AND (select = ?))")
               ["ASSERT ? = (SELECT COUNT(*) FROM gesso_requests WHERE (_id = ?) AND (amount = ?) AND (amount = ?))" 1 "r1" 2 2]
               (update guard 0 str " -- comment")]]
    (is (nil? (current/operation-tables bad)))
    (let [r (current/execute state [["ASSERT FALSE"] bad])]
      (is (= :unsupported (:status r)))
      (is (not (contains? r :state)))))
  (let [t (Instant/parse "2026-01-01T00:00:00Z")
        txs [{:system-time t :ops [(put row)]}
             {:system-time (.plusSeconds t 1) :ops [(put (assoc row :amount 3)) probe guard]}
             {:system-time (.plusSeconds t 2) :ops [guard probe]}]
        r (temporal/replay txs)]
    (is (= #{:gesso_requests :guard_probe} (temporal/required-observation-tables txs)))
    (is (= :predicted (:status r)))
    (is (= [true false true] (mapv :committed? (:history r))))
    (is (= 2 (get-in r [:history 1 :failed-assertion-index])))
    (is (= :unsupported (:status (temporal/replay-equal-time txs))))))
