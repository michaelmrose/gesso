(ns gesso.assurance.reference-lifecycle-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-lifecycle :as lifecycle]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.assurance.reference-temporal-fixtures :refer [sys provider-rectangles coalesce-valid]]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.acknowledgment :as acknowledgment]
            [gesso.live.outbox.read-frontier :as frontier]
            [gesso.live.consistency.xtdb :as live.xtdb]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import (java.time Instant)))

(defn- id [n] (java.util.UUID. 0 n))
(defn- time-at [n] (.plusSeconds ^Instant (sys 1) n))
(defn- publish [n ids value]
  {:kind :publish :system-time (time-at n)
   :spec {:intent-ids ids :source-id "lifecycle" :operation :request/claim
          :purpose :gesso.live/refresh :command-id "command-1"}
   :changes (mapv (fn [i] {:topic :request :id (str i) :change/kind :updated}) ids)
   :writes [[:put-docs :lifecycleprobe {:xt/id "domain" :value value}]]})
(defn- acquire [kind n observed intent lease owner deadline]
  {:kind kind :system-time (time-at n) :observed-system (time-at observed)
   :observed-valid (time-at observed)
   :claim {:intent-id (id intent) :lease-id (id lease) :worker-id owner
           :source-id "lifecycle" :now (time-at n) :lease-until (time-at deadline)}})
(defn- ack [n observed intent lease owner receipt]
  {:kind :acknowledge :system-time (time-at n) :observed-system (time-at observed)
   :observed-valid (time-at observed)
   :submission {:intent-id (id intent) :lease-id (id lease) :worker-id owner
                :receipt {:receipt-id (id receipt) :adapter :test/adapter :result :accepted}}})

(def ^:private steps
  [(publish 0 [(id 1) (id 2)] 1)
   (publish 1 [(id 3) (id 2)] 2)
   (acquire :acquire 2 1 1 11 "a" 10)
   (acquire :acquire 3 1 1 12 "b" 20)
   (acquire :reclaim 4 2 1 12 "b" 20)
   (acquire :reclaim 10 4 1 12 "b" 20)
   (ack 11 2 1 11 "a" 21)
   (ack 12 10 1 12 "b" 22)
   (ack 13 12 1 12 "b" 22)
   (acquire :acquire 14 12 2 13 "c" 30)
   (ack 15 14 2 13 "c" 23)])
(def ^:private outcomes [:published :contended :acquired :contended :contended
                         :reclaimed :contended :acknowledged :unavailable :acquired :acknowledged])

(deftest closed-reference-publication-lifecycle
  (let [r (lifecycle/predict-lifecycle steps)
        visible #(temporal/visible-at (:replay r) (time-at 15) (time-at %))
        intents (get (visible 15) :gesso_live_outbox)]
    (is (= :predicted (:status r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= outcomes (mapv :outcome (:trace r))))
    (is (= 10 (count (:transactions r))))
    (is (= [true false true false false true false true true true]
           (mapv :committed? (get-in r [:replay :history]))))
    (is (= #{(id 1) (id 2)} (set (keys intents))))
    (is (= [:dispatch-accepted :dispatch-accepted] (mapv #(get-in intents [(id %) :gesso.live.outbox/status]) [1 2])))
    (is (= [(id 22) (id 23)] (mapv #(get-in intents [(id %) :gesso.live.outbox/dispatch-receipt-id]) [1 2])))
    (is (= (id 11) (get-in (visible 9) [:gesso_live_outbox (id 1) :gesso.live.outbox/lease-id])))
    (is (= (id 12) (get-in (visible 10) [:gesso_live_outbox (id 1) :gesso.live.outbox/lease-id])))
    (is (= 1 (get-in (visible 15) [:lifecycleprobe "domain" :value])))
    (is (= [] (get-in r [:trace 8 :ops])))
    (doseq [invalid [[] (subvec steps 1) (assoc-in steps [0 :spec :intent-ids] [(id 1) (id 1)])
                     (assoc-in steps [0 :writes] [[:put-docs :gesso_live_outbox {:xt/id (id 3)}]])
                     (assoc-in steps [0 :changes 0 :extra] true)
                     (assoc-in steps [1 :system-time] (time-at 0))
                     (assoc-in steps [10 :submission :receipt :result] :failed)]]
      (let [p (lifecycle/predict-lifecycle invalid)]
        (is (= :unsupported (:status p)))
        (is (not-any? #(contains? p %) [:trace :transactions :replay]))))))

(defn- execute-publication! [node {:keys [spec changes writes system-time]} captured]
  (let [{:keys [guard-ops tx-op]} (outbox/prepare! spec changes)
        ops (into (into guard-ops writes) [tx-op])]
    (swap! captured conj ops)
    (try (live.xtdb/execute-tx! node ops {:system-time system-time}) {:status :published}
         (catch Throwable e
           (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data e)))
             {:status :contended} (throw e))))))

(deftest independent-lifecycle-matches-production-and-xtdb
  (let [prediction (lifecycle/predict-lifecycle steps)]
    (is (= :predicted (:status prediction)))
    (with-open [node (xtn/start-node {})]
      (let [execute! live.xtdb/execute-tx!]
        (doseq [[{:keys [kind system-time observed-system observed-valid claim submission] :as step} predicted]
                (map vector steps (:trace prediction))]
          (let [captured (atom [])
                actual (if (= :publish kind)
                         (execute-publication! node step captured)
                         (with-redefs [frontier/q-from! (fn [_ [_ id]]
                                                          (xt/q node ["SELECT *, _system_from FROM gesso_live_outbox FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ? WHERE _id = ?"
                                                                      observed-valid observed-system id]))
                                       live.xtdb/execute-tx! (fn [connectable ops]
                                                               (swap! captured conj ops)
                                                               (execute! connectable ops {:system-time system-time}))]
                           (case kind
                             :acquire (acquisition/acquire-pending! node claim)
                             :reclaim (acquisition/reclaim-expired! node claim)
                             :acknowledge (acknowledgment/acknowledge-dispatch! node submission))))]
            (is (= (:outcome predicted) (:status actual)))
            (is (= (:reason predicted) (:reason actual)))
            (is (= (if (seq (:ops predicted)) [(:ops predicted)] []) @captured)))))
      (is (= (frequencies (coalesce-valid (get-in prediction [:replay :rectangles])))
             (frequencies (coalesce-valid (mapcat #(provider-rectangles node %) [:gesso_live_outbox :lifecycleprobe])))))
      (doseq [s (range 0 16) v (range 0 16) table [:gesso_live_outbox :lifecycleprobe]]
        (let [rows (xt/q node [(str "SELECT * FROM " (name table) " FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ?")
                               (time-at v) (time-at s)])]
          (is (= (count rows) (count (set (map :xt/id rows)))))
          (is (= (get (temporal/visible-at (:replay prediction) (time-at s) (time-at v)) table {})
                 (into {} (map (juxt :xt/id identity)) rows))))))))
