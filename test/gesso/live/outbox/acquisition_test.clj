(ns gesso.live.outbox.acquisition-test
  "v2090-v2092: physical pending-intent acquisition and provider-time guards."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.live.outbox :as outbox]
            [gesso.live.consistency.xtdb :as live.xtdb]
            [gesso.live.outbox.acquisition :as acquisition]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.time Instant]))

(defn- claim
  [intent-id worker]
  (let [now (Instant/ofEpochMilli (System/currentTimeMillis))]
    {:intent-id intent-id :worker-id worker :lease-id (random-uuid)
     :now now :lease-until (.plusSeconds now 60)}))

(defn- pending! [node id]
  (let [plan (outbox/prepare!
              {:intent-ids [id] :source-id "example/source"
               :operation :request/claim :purpose :gesso.live/refresh}
              [{:topic :request :id "request-1" :change/kind :updated}])]
    (xt/execute-tx node (conj (:guard-ops plan) (:tx-op plan)))))

(defn- read-intent [node id]
  (first (xt/q node ["SELECT *, _system_from FROM gesso_live_outbox WHERE _id = ?" id])))

(deftest validation-rejects-untrusted-and-malformed-lease-input-test
  (let [id (random-uuid)
        valid (claim id "worker-a")]
    (doseq [[mutation reason]
            [[(constantly nil) :invalid-claim]
             [#(assoc % :intent-id "not-uuid") :invalid-intent-id]
             [#(assoc % :worker-id " ") :invalid-worker-id]
             [#(assoc % :lease-id "x") :invalid-lease-id]
             [#(assoc % :now "not-an-instant") :invalid-clock]
             [#(assoc % :lease-until (:now %)) :invalid-lease-duration]
             [#(assoc % :lease-until (.plusSeconds ^Instant (:now %) 301))
              :invalid-lease-duration]
             [#(assoc % :extra :unsafe) :unknown-fields]]]
      (let [e (try (acquisition/acquire-pending! :unconnected (mutation valid))
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (= :gesso.live.outbox/invalid-acquisition
               (:error/type (ex-data e))))
        (is (= reason (:reason (ex-data e))))))))

(deftest physical-claim-stays-durable-and-preserves-original-intent-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          c (claim id "worker-a")]
      (pending! node id)
      (let [before (read-intent node id)
            result (acquisition/acquire-pending! node c)
            after (read-intent node id)]
        (is (= :acquired (:status result)))
        (is (some? (get-in result [:consistency :tx-id])))
        (is (= :leased (:gesso.live.outbox/status after)))
        (is (= (:lease-id c) (:gesso.live.outbox/lease-id after)))
        (is (= (:worker-id c) (:gesso.live.outbox/lease-owner after)))
        (is (= (:lease-until c) (.toInstant ^java.time.ZonedDateTime
                                           (:gesso.live.outbox/lease-until after))))
        (is (= (dissoc before :xt/system-from :gesso.live.outbox/status)
               (dissoc after :xt/system-from :gesso.live.outbox/status
                       :gesso.live.outbox/lease-id :gesso.live.outbox/lease-owner
                       :gesso.live.outbox/lease-until)))
        (is (not= (:xt/system-from before) (:xt/system-from after)))
        (is (= {:status :unavailable :reason :not-pending :intent-id id}
               (acquisition/acquire-pending! node (claim id "worker-b"))))))))

(deftest physical-two-claimants-do-not-both-acquire-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          a (claim id "worker-a")
          b (claim id "worker-b")]
      (pending! node id)
      (let [gate (promise)
            fa (future @gate (acquisition/acquire-pending! node a))
            fb (future @gate (acquisition/acquire-pending! node b))
            _ (deliver gate true)
            results [@fa @fb]
            successes (filter #(= :acquired (:status %)) results)
            after (read-intent node id)]
        (is (= 1 (count successes)) (pr-str results))
        (is (= 1 (count (filter #(#{:contended :unavailable} (:status %)) results))))
        (is (= (:lease-id (first successes))
               (:gesso.live.outbox/lease-id after)))
        (is (= :leased (:gesso.live.outbox/status after)))))))

(deftest physical-missing-and-bogus-intent-rejected-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)]
      ;; Prepare the table without inserting a current row matching id.
      (pending! node (random-uuid))
      (is (= {:status :unavailable :reason :missing :intent-id id}
             (acquisition/acquire-pending! node (claim id "worker-a"))))
      (xt/execute-tx node [[:put-docs :gesso_live_outbox
                            {:xt/id id :gesso.live.outbox/type :unexpected
                             :gesso.live.outbox/status :pending}]])
      (let [e (try (acquisition/acquire-pending! node (claim id "worker-a"))
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (= :invalid-physical-intent (:reason (ex-data e))))))))

(deftest physical-initial-acquire-rejects-host-clock-skew-test
  ;; Both claims have a valid host-relative 60s duration. But one already
  ;; expired in provider time and the other lasts far beyond five minutes.
  ;; The XTDB transaction must reject either one without modifying the row.
  (with-open [node (xtn/start-node {})]
    (doseq [[label offset] [[:already-expired -180]
                            [:excessive-future 900]]]
      (let [id (random-uuid)
            _ (pending! node id)
            before (read-intent node id)
            proposed-now (.plusSeconds ^Instant (:now (claim id "probe")) offset)
            proposed {:intent-id id :worker-id "skewed-worker"
                      :lease-id (random-uuid)
                      :now proposed-now
                      :lease-until (.plusSeconds ^Instant proposed-now 60)}
            result (acquisition/acquire-pending! node proposed)]
        (is (= :contended (:status result)) (name label))
        (is (= before (read-intent node id)) (name label))
        (is (= :pending (:gesso.live.outbox/status (read-intent node id)))
            (name label))))))

(deftest source-scoped-physical-claim-rejects-foreign-row-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          _ (pending! node id)
          before (read-intent node id)
          claim (assoc (claim id "worker-a") :source-id "foreign/source")]
      (is (= {:status :unavailable :reason :source-mismatch :intent-id id}
             (acquisition/acquire-pending! node claim)))
      (is (= before (read-intent node id)))
      (is (= :acquired
             (:status (acquisition/acquire-pending!
                       node (assoc claim :source-id "example/source"))))))))

(deftest physical-source-change-after-read-is-stopped-by-cas-test
  ;; Even if the row matches our source when acquisition reads it, an
  ;; independent writer may reassign it before the claim's physical commit.
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          _ (pending! node id)
          original-execute xt/execute-tx
          ;; Rebind the wrapper called by the acquisition implementation.
          attempted (atom 0)
          execute live.xtdb/execute-tx!
          result (with-redefs [live.xtdb/execute-tx!
                              (fn [n ops & rest-args]
                                (when (= 1 (swap! attempted inc))
                                  (let [row (read-intent n id)]
                                  (original-execute n
                                    [[:put-docs outbox/intent-table
                                      (assoc (dissoc row :xt/system-from)
                                             :gesso.live.outbox/source-id
                                             "foreign/source")]])))
                                (apply execute n ops rest-args))]
                   (acquisition/acquire-pending!
                     node (assoc (claim id "worker-a")
                                 :source-id "example/source")))]
      (is (pos? @attempted))
      (is (= :contended (:status result)))
      (is (= "foreign/source"
             (:gesso.live.outbox/source-id (read-intent node id))))
      (is (= :pending (:gesso.live.outbox/status (read-intent node id)))))))

(deftest interrupted-lease-transaction-preserves-thread-signal-test
  ;; The acquisition and reclaim boundaries may receive InterruptedException
  ;; from an interruptible physical XTDB call. That exception's identity is
  ;; authoritative; a failed transaction is neither a safe :contended result
  ;; nor permission to publish an invalidation. Preserve cancellation too.
  (with-open [node (xtn/start-node {})]
    (doseq [phase [:acquire :reclaim]]
      (let [id (random-uuid)
            _ (pending! node id)
            ;; Recovery exercises the preexisting :leased row path. A future
            ;; deadline is sufficient here: the injected exception precedes
            ;; the physical expiration guard's evaluation.
            _ (when (= phase :reclaim)
                (is (= :acquired
                       (:status (acquisition/acquire-pending!
                                 node (claim id "initial-worker"))))))
            before (read-intent node id)
            request (claim id "new-worker")
            cause (InterruptedException. (str (name phase) " interrupted"))
            called (atom 0)]
        (try
          (Thread/interrupted)
          (let [caught (with-redefs [live.xtdb/execute-tx!
                                    (fn [& _]
                                      (swap! called inc)
                                      (throw cause))]
                         (try
                           (case phase
                             :acquire (acquisition/acquire-pending! node request)
                             :reclaim (acquisition/reclaim-expired! node request))
                           (catch Throwable error error)))
                interrupted? (.isInterrupted (Thread/currentThread))]
            (Thread/interrupted)
            (is (identical? cause caught)
                (str phase " must propagate the original XTDB interruption"))
            (is interrupted?
                (str phase " must retain JVM cancellation on the caller"))
            (is (= 1 @called) (str phase " must attempt one physical write"))
            (is (= before (read-intent node id))
                (str phase " may not invent a successful lease transition")))
          (finally
            (Thread/interrupted)))))))
