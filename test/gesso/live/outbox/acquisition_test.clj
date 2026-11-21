(ns gesso.live.outbox.acquisition-test
  "v2090: physical pending-intent acquisition and stale-claim regressions."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acquisition :as acquisition]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.time Instant]))

(defn- inst [offset-seconds]
  (.plusSeconds (Instant/parse "2026-11-21T00:00:00Z") offset-seconds))

(defn- claim
  [intent-id worker]
  {:intent-id intent-id :worker-id worker :lease-id (random-uuid)
   :now (inst 0) :lease-until (inst 60)})

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
             [#(assoc % :lease-until (inst 0)) :invalid-lease-duration]
             [#(assoc % :lease-until (inst 301)) :invalid-lease-duration]
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
