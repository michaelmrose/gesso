(ns gesso.live.outbox.reclaim-send-race-test
  "Physical contention at the external-send/XTDB-lease boundary.

   An old worker can pass its physical lease check, pause before source
   emission, and lose its lease to another worker. Reclamation and terminal
   acknowledgment are fenced by XTDB; the external Live source is not.
   Witness the permitted duplicate notifications without allowing the
   stale worker to overwrite the new owner's acknowledgment."
  (:require [clojure.test :refer [deftest is]]
            [gesso.live.core :as live]
            [gesso.live.dispatch :as dispatch]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.delivery :as delivery]
            [gesso.live.outbox.acknowledgment :as acknowledgment]
            [gesso.live.source :as source]
            [manifold.stream :as stream]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.time Instant]
           [java.util.concurrent CountDownLatch TimeUnit]))

(def ^:private source-id "gesso/send-reclaim-race")
(def ^:private target "reclaimed-request")

(defn- system! []
  (live/create {:rules [{:when-topic :request
                         :expand (fn [_ change]
                                   [(select-keys change [:topic :id :change/kind])])}]
                :source-options {:coalesce-window-ms 0}
                :dispatch-options {:threads 1 :queue-size 8 :on-overflow :throw}}))

(defn- seed! [node id]
  (let [prepared (outbox/prepare!
                  {:intent-ids [id] :source-id source-id
                   :operation :request/claim :purpose :gesso.live/refresh}
                  [{:topic :request :id target :change/kind :updated}])]
    (xt/execute-tx node (into (vec (:guard-ops prepared))
                              [[:put-docs :requests {:xt/id target
                                                      :status :claimed}]
                               (:tx-op prepared)]))))

(defn- lease-at! [f node id worker millis]
  (let [now (Instant/now)]
    (f node {:intent-id id :worker-id worker :source-id source-id
             :lease-id (random-uuid) :now now
             :lease-until (.plusMillis now millis)})))

(defn- current! [node id]
  (first (xt/q node ["SELECT * FROM gesso_live_outbox WHERE _id = ?" id])))

(defn- reclaim-after-expiry! [node id]
  ;; The real provider clock, not the test JVM's timer, decides when the
  ;; original lease is expired and a new owner may commit a reclaim.
  (loop [attempts 0]
    (when (>= attempts 120)
      (throw (ex-info "XTDB lease did not become reclaimable"
                      {:intent-id id :attempts attempts})))
    (let [result (lease-at! acquisition/reclaim-expired!
                            node id "new-owner" 30000)]
      (if (= :reclaimed (:status result))
        result
        (do (is (= :contended (:status result)))
            (Thread/sleep 75)
            (recur (inc attempts)))))))

(defn- after-release! [pending]
  (let [result (deref pending 8000 ::timeout)]
    (when (= ::timeout result)
      (throw (ex-info "Paused original delivery did not complete" {})))
    result))

(deftest reclaimed-terminal-owner-blocks-late-original-ack-test
  (with-open [node (xtn/start-node {})]
    (let [system (system!)
          id (random-uuid)
          pause (CountDownLatch. 1)
          release (CountDownLatch. 1)
          real-run dispatch/run-sync!]
      (try
        (seed! node id)
        (let [tap (source/subscribe (:source system)
                                    {:topic :request :id target})
              old (lease-at! acquisition/acquire-pending!
                             node id "old-owner" 1200)]
          (is (= :acquired (:status old)))
          (try
            (with-redefs [dispatch/run-sync!
                          (fn [job]
                            (when (= "paused-old-send" (.getName (Thread/currentThread)))
                              (.countDown pause)
                              (when-not (.await release 10 TimeUnit/SECONDS)
                                (throw (ex-info "Timed out holding old send" {}))))
                            (real-run job))]
              (let [old-result (promise)
                    thread (Thread.
                            (fn []
                              (deliver old-result
                                       (try {:result (delivery/dispatch-acquired!
                                                     node system {} old source-id)}
                                            (catch Throwable e {:error e}))))
                            "paused-old-send")]
                (.start thread)
                (try
                  (is (.await pause 5 TimeUnit/SECONDS)
                      "Old worker must pass pre-send physical lease check")
                  (let [new-owner (reclaim-after-expiry! node id)
                        new-result (delivery/dispatch-acquired!
                                    node system {} new-owner source-id)
                        first-notice (deref (stream/take! tap) 3000 ::timeout)]
                    (is (= :dispatch-accepted (:status new-result)))
                    (is (= :acknowledged (get-in new-result [:acknowledgment :status])))
                    (is (= (:progression new-owner) (:progression first-notice)))
                    (is (= :dispatch-accepted (:gesso.live.outbox/status (current! node id))))
                    (.countDown release)
                    (let [late (after-release! old-result)
                          second-notice (deref (stream/take! tap) 3000 ::timeout)
                          physical (current! node id)]
                      (is (nil? (:error late)))
                      (is (= :dispatched-unacknowledged
                             (get-in late [:result :status])))
                      (is (= :unavailable
                             (get-in late [:result :acknowledgment :status])))
                      (is (= :not-leased
                             (get-in late [:result :acknowledgment :reason])))
                      (is (= (:progression old) (:progression second-notice)))
                      (is (not= (:progression first-notice)
                                (:progression second-notice)))
                      (is (= :dispatch-accepted (:gesso.live.outbox/status physical)))
                      (is (= (:lease-id new-owner)
                             (:gesso.live.outbox/lease-id physical)))
                      (is (= "new-owner" (:gesso.live.outbox/lease-owner physical)))
                      (is (= (:receipt-id new-result)
                             (:gesso.live.outbox/dispatch-receipt-id physical)))
                      (is (not= (:receipt-id new-result)
                                (get-in late [:result :receipt-id])))
                      ;; Exactly one durable terminal acknowledgment, but two
                      ;; legitimate source sends. Neither is browser delivery.
                      (is (= :gesso.live/synchronous-source
                             (:gesso.live.outbox/dispatch-adapter physical)))))
                  (finally
                    (.countDown release)
                    (.join thread 3000)
                    (is (not (.isAlive thread)))))))
            (finally (stream/close! tap))))
        (finally (live/close! system))))))

(deftest reclaimed-but-unacknowledged-lease-rejects-late-original-ack-test
  ;; The competing worker has reclaimed the lease but has NOT sent its
  ;; notice or recorded an acknowledgment. The old worker's delayed source
  ;; emission may still occur, yet its acknowledgment must fail with an
  ;; explicit post-dispatch unknown result, never overwriting the new lease.
  (with-open [node (xtn/start-node {})]
    (let [system (system!)
          id (random-uuid)
          pause (CountDownLatch. 1)
          release (CountDownLatch. 1)
          real-run dispatch/run-sync!]
      (try
        (seed! node id)
        (let [tap (source/subscribe (:source system)
                                    {:topic :request :id target})
              old (lease-at! acquisition/acquire-pending!
                             node id "old-owner" 1200)]
          (is (= :acquired (:status old)))
          (try
            (with-redefs [dispatch/run-sync!
                          (fn [job]
                            (when (= "paused-old-send" (.getName (Thread/currentThread)))
                              (.countDown pause)
                              (when-not (.await release 10 TimeUnit/SECONDS)
                                (throw (ex-info "Timed out holding old send" {}))))
                            (real-run job))]
              (let [old-result (promise)
                    thread (Thread.
                            (fn []
                              (deliver old-result
                                       (try {:result (delivery/dispatch-acquired!
                                                     node system {} old source-id)}
                                            (catch Throwable e {:error e}))))
                            "paused-old-send")]
                (.start thread)
                (try
                  (is (.await pause 5 TimeUnit/SECONDS))
                  (let [new-owner (reclaim-after-expiry! node id)]
                    (is (= :leased (:gesso.live.outbox/status (current! node id))))
                    (is (= "new-owner" (:gesso.live.outbox/lease-owner
                                          (current! node id))))
                    (.countDown release)
                    (let [late (after-release! old-result)
                          first-event (deref (stream/take! tap) 3000 ::timeout)
                          physical-after-old (current! node id)
                          error (:error late)]
                      (is (instance? clojure.lang.ExceptionInfo error))
                      (is (= :gesso.live.outbox/post-dispatch-ack-unknown
                             (:error/type (ex-data error))))
                      (is (= (:intent-id old) (:intent-id (ex-data error))))
                      (is (= (:lease-id old) (:lease-id (ex-data error))))
                      (is (= :accepted (:dispatch/status (ex-data error))))
                      (is (= :unknown (:acknowledgment/status (ex-data error))))
                      (is (= :gesso.live.outbox/invalid-acknowledgment
                             (:error/type (ex-data (.getCause error)))))
                      (is (= :stale-lease
                             (:reason (ex-data (.getCause error)))))
                      (is (= (:progression old) (:progression first-event)))
                      (is (= :leased (:gesso.live.outbox/status physical-after-old)))
                      (is (= (:lease-id new-owner)
                             (:gesso.live.outbox/lease-id physical-after-old)))
                      (is (nil? (:gesso.live.outbox/dispatch-receipt-id
                                 physical-after-old)))
                      ;; Only the real current owner may subsequently send
                      ;; and acknowledge; the older send remains advisory.
                      (let [new-result (delivery/dispatch-acquired!
                                        node system {} new-owner source-id)
                            second-event (deref (stream/take! tap) 3000 ::timeout)
                            final-row (current! node id)]
                        (is (= :dispatch-accepted (:status new-result)))
                        (is (= :acknowledged
                               (get-in new-result [:acknowledgment :status])))
                        (is (= (:progression new-owner)
                               (:progression second-event)))
                        (is (not= (:progression first-event)
                                  (:progression second-event)))
                        (is (= :dispatch-accepted
                               (:gesso.live.outbox/status final-row)))
                        (is (= (:lease-id new-owner)
                               (:gesso.live.outbox/lease-id final-row)))
                        (is (= (:receipt-id new-result)
                               (:gesso.live.outbox/dispatch-receipt-id
                                final-row))))))
                  (finally
                    (.countDown release)
                    (.join thread 3000)
                    (is (not (.isAlive thread)))))))
            (finally (stream/close! tap))))
        (finally (live/close! system))))))
