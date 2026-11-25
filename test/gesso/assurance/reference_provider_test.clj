(ns gesso.assurance.reference-provider-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-provider :as reference]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.assurance.reference-temporal-fixtures :refer [sys provider-rectangles]]
            [gesso.assurance.xtdb-envelope :as envelope]
            [gesso.assurance.reference-lifecycle :as lifecycle]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.time Duration ZonedDateTime)))

(defn- id [n] (java.util.UUID. 0 n))
(defn- time-at [n] (.plusSeconds (sys 1) n))
(def ^:private steps
  [{:kind :publish :system-time (time-at 0)
    :spec {:source-id "provider-reference" :intent-ids [(id 1)]
           :operation :request/claim :purpose :gesso.live/refresh}
    :changes [{:topic :request :id "a"}]
    :writes [[:put-docs :providerprobe {:xt/id "a" :value 1}]]}
   {:kind :acquire :system-time (time-at 1) :observed-system (time-at 0) :observed-valid (time-at 0)
    :claim {:intent-id (id 1) :lease-id (id 2) :worker-id "w" :source-id "provider-reference"
            :now (time-at 0) :lease-until (time-at 10)}}
   {:kind :acknowledge :system-time (time-at 10) :observed-system (time-at 1) :observed-valid (time-at 10)
    :submission {:intent-id (id 1) :lease-id (id 2) :worker-id "w"
                 :receipt {:receipt-id (id 3) :adapter :test/adapter :result :accepted}}}])

(def ^:private tables #{:providerprobe :gesso_live_outbox})

(defn- transactions []
  (let [p (lifecycle/predict-lifecycle steps)]
    (is (= :predicted (:status p)))
    (into (:transactions p)
          [{:system-time (time-at 11) :ops [["ASSERT TRUE"]]}
           {:system-time (time-at 12) :ops [[:put-docs :providerprobe {:xt/id "a" :value 2}]
                                          ["ASSERT FALSE"]]}])))

(defn- observe! [node txs]
  (let [predicted (temporal/replay txs)
        ids (mapv (fn [{:keys [system-time ops]}]
                    (let [id (:tx-id (xt/submit-tx node ops {:system-time system-time}))]
                      (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 10))
                      id)) txs)
        ledger (mapv (fn [r] {:tx-id (:xt/id r) :committed? (:committed r)
                             :system-time (.toInstant ^ZonedDateTime (:system-time r))})
                     (xt/q node ["SELECT _id, committed, system_time FROM xt.txs ORDER BY _id"]))
        envelopes (mapv (fn [i tx entry]
                          ((if (#{1 2} i) envelope/check-worker-attempt-envelope! envelope/check-envelope!)
                           node (nth ids i) (:ops tx) (:committed? entry)))
                        (range) txs (:history predicted))]
    {:ledger ledger :envelopes envelopes
     :rectangles (vec (mapcat #(provider-rectangles node %) tables))}))

(deftest selected-provider-execution-comparison
  (let [txs (transactions)]
    (with-open [node (xtn/start-node {})]
      (let [observed (observe! node txs)
            report (reference/compare-execution txs tables observed)]
        (is (= [:matched :matched :matched :matched :matched] (mapv :status (:envelopes observed))) (pr-str (:envelopes observed)))
        (is (= [true true false true false] (mapv :committed? (:ledger observed))))
        (is (= :matched (:status report)) (pr-str report))
        (is (= {:ledger true :retained-envelopes true :selected-history true} (:checks report)))
        (is (= :not-established (:provider-refinement report)))
        (is (= :none (:authority report)))
        (is (= :not-issued (:certificate report)))
        (let [ops (:ops (nth txs 1)) id (get-in observed [:ledger 1 :tx-id])
              path [(dec (count ops)) 2 :gesso.live.outbox/lease-until]]
          (is (= :mismatch (:status (envelope/check-worker-attempt-envelope!
                                     node id (update-in ops path #(.plusSeconds % 1)) true))))
          (is (= :mismatch (:status (envelope/check-worker-attempt-envelope!
                                     node id (update-in ops path #(.atZone % (java.time.ZoneId/of "Europe/Paris"))) true))))
          (is (= :mismatch (:status (envelope/check-worker-attempt-envelope! node id ops false)))))
        (doseq [[changed expected] [[(assoc-in observed [:ledger 2 :committed?] true) :mismatch]
                                    [(update-in observed [:ledger 0 :system-time] #(.plusSeconds % 1)) :mismatch]
                                    [(assoc-in observed [:envelopes 0 :tx-id] -1) :mismatch]
                                    [(assoc-in observed [:envelopes 0 :status] :unavailable) :unavailable]
                                    [(assoc-in observed [:envelopes 0 :status] :unsupported) :unsupported]
                                    [(assoc-in observed [:envelopes 0 :status] :mismatch) :mismatch]
                                    [(update observed :rectangles pop) :mismatch]
                                    [(update observed :rectangles conj (first (:rectangles observed))) :mismatch]
                                    [(update observed :ledger pop) :unsupported]
                                    [(assoc-in observed [:envelopes 0 :profile] :foreign) :unsupported]]]
          (is (= expected (:status (reference/compare-execution txs tables changed)))))
        (is (= :unsupported (:status (reference/compare-execution txs #{:providerprobe} observed))))
        (is (= :unsupported (:status (reference/compare-execution [] tables observed))))))))
