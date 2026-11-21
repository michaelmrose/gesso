(ns gesso.live.outbox.recovery-test
  "v2091: bounded physical recovery of expired outbox leases, with
   single-transaction fencing of subsequent state transitions. No external
   SSE delivery, crash-restart or exactly-once claim is implied."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acquisition :as acquisition]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.time Instant ZonedDateTime]))

(defn- lease-claim
  ([id worker] (lease-claim id worker (Instant/now)))
  ([id worker now]
   {:intent-id id
    :lease-id (random-uuid)
    :worker-id worker
    :now now
    :lease-until (.plusSeconds ^Instant now 60)}))

(defn- seeded-lease!
  "Construct a physical previously leased row with an arbitrary historical
   deadline. We don't wait for leases to expire in tests."
  [node intent-id lease-until]
  (let [plan (outbox/prepare!
              {:intent-ids [intent-id] :source-id "gesso/test-recovery"
               :operation :request/claim :purpose :gesso.live/refresh}
              [{:topic :request :id "request-1" :change/kind :updated}])
        old {:lease-id (random-uuid)
             :worker-id "old-worker"}
        row (assoc (first (:intents plan))
                   :gesso.live.outbox/status :leased
                   :gesso.live.outbox/lease-id (:lease-id old)
                   :gesso.live.outbox/lease-owner (:worker-id old)
                   :gesso.live.outbox/lease-until lease-until)]
    (xt/execute-tx node (conj (:guard-ops plan)
                              [:put-docs outbox/intent-table row]))
    old))

(defn- read-intent [node id]
  (first (xt/q node
               ["SELECT *, _system_from FROM gesso_live_outbox WHERE _id = ?" id])))

(defn- conflict? [f]
  (try
    (f)
    false
    (catch Throwable t
      (= :xtdb/assert-failed (:xtdb.error/code (ex-data t))))))

(defn- rejection-reason [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e
      (when (= :gesso.live.outbox/invalid-acquisition
               (:error/type (ex-data e)))
        (:reason (ex-data e))))))

(deftest expired-lease-reclaimed-with-new-token-and-original-intent-preserved-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          old (seeded-lease! node id (.minusSeconds (Instant/now) 120))
          before (read-intent node id)
          claim (lease-claim id "worker-b")
          result (acquisition/reclaim-expired! node claim)
          after (read-intent node id)]
      (is (= :reclaimed (:status result)))
      (is (some? (get-in result [:consistency :tx-id])))
      (is (= (:lease-id claim) (:lease-id result)))
      (is (not= (:lease-id old) (:lease-id result)))
      (is (= :leased (:gesso.live.outbox/status after)))
      (is (= (:lease-id claim) (:gesso.live.outbox/lease-id after)))
      (is (= (:worker-id claim) (:gesso.live.outbox/lease-owner after)))
      ;; XTDB stores timestamp precision in microseconds, not nanos.
      (is (= (.toEpochMilli ^Instant (:lease-until claim))
             (.toEpochMilli (.toInstant ^ZonedDateTime
                                           (:gesso.live.outbox/lease-until after)))))
      (is (not= (:xt/system-from before) (:xt/system-from after)))
      (is (= (dissoc before :xt/system-from :gesso.live.outbox/lease-id
                     :gesso.live.outbox/lease-owner :gesso.live.outbox/lease-until)
             (dissoc after :xt/system-from :gesso.live.outbox/lease-id
                     :gesso.live.outbox/lease-owner :gesso.live.outbox/lease-until)))
      (is (= :stale-lease
             (rejection-reason
               #(acquisition/active-lease-guard-ops
                  node {:intent-id id :worker-id (:worker-id old)
                        :lease-id (:lease-id old)})))))))

(deftest unexpired-lease-cannot-be-reclaimed-early-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          old (seeded-lease! node id (.plusSeconds (Instant/now) 120))
          before (read-intent node id)
          result (acquisition/reclaim-expired! node (lease-claim id "worker-b"))]
      (is (= :contended (:status result)))
      (is (= before (read-intent node id)))
      (is (= (:lease-id old)
             (:gesso.live.outbox/lease-id (read-intent node id)))))))

(deftest concurrent-reclaimants-observe-single-winning-lease-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          _ (seeded-lease! node id (.minusSeconds (Instant/now) 120))
          a (lease-claim id "a")
          b (lease-claim id "b")
          gate (promise)
          fa (future @gate (acquisition/reclaim-expired! node a))
          fb (future @gate (acquisition/reclaim-expired! node b))]
      (deliver gate true)
      (let [results [@fa @fb]
            winning (first (filter #(= :reclaimed (:status %)) results))
            after (read-intent node id)]
        (is (= 1 (count (filter #(= :reclaimed (:status %)) results)))
            (pr-str results))
        (is (= 1 (count (filter #(= :contended (:status %)) results))))
        (is (= (:lease-id winning) (:gesso.live.outbox/lease-id after)))
        (is (= (:worker-id winning) (:gesso.live.outbox/lease-owner after)))))))

(deftest physically-reject-invalid-provider-time-new-deadlines-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          _ (seeded-lease! node id (.minusSeconds (Instant/now) 120))
          original (read-intent node id)
          past-now (.minusSeconds (Instant/now) 180)
          far-future-now (.plusSeconds (Instant/now) 1000)]
      ;; Both claim declarations have a valid 60s host duration. They must
      ;; nevertheless be refused by provider-time bounds at XTDB submission.
      (is (= :contended (:status
                         (acquisition/reclaim-expired!
                          node (lease-claim id "past" past-now)))))
      (is (= :contended (:status
                         (acquisition/reclaim-expired!
                          node (lease-claim id "future" far-future-now)))))
      (is (= original (read-intent node id))))))

(deftest active-lease-guard-fences-stale-physical-writes-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          old (seeded-lease! node id (.plusSeconds (Instant/now) 120))
          old-fence {:intent-id id :worker-id (:worker-id old)
                     :lease-id (:lease-id old)}
          ops (acquisition/active-lease-guard-ops node old-fence)
          original (read-intent node id)
          replaced (-> original
                       (dissoc :xt/system-from)
                       (assoc :gesso.live.outbox/lease-id (random-uuid)
                              :gesso.live.outbox/lease-owner "replacement-worker"))]
      ;; Simulate an independent physical replacement of the active lease.
      ;; Even a guard that was valid when constructed must fail at its
      ;; transaction after the row's physical _system_from changes.
      (xt/execute-tx node [[:put-docs outbox/intent-table replaced]])
      (let [after (read-intent node id)]
        (is (not= (:xt/system-from original) (:xt/system-from after)))
        (is (conflict?
              #(xt/execute-tx node
                              (conj (vec ops)
                                    [:put-docs outbox/intent-table
                                     (assoc replaced :gesso.live.outbox/status :delivered)]))))
        (is (= after (read-intent node id)))
        (is (= :stale-lease (rejection-reason
                             #(acquisition/active-lease-guard-ops
                               node old-fence))))
        (is (= 1 (count (acquisition/active-lease-guard-ops
                         node {:intent-id id
                               :worker-id "replacement-worker"
                               :lease-id (:gesso.live.outbox/lease-id after)}))))))))

(deftest expired-lease-cannot-use-active-guard-even-without-reclaim-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          old (seeded-lease! node id (.minusSeconds (Instant/now) 120))
          before (read-intent node id)
          guards (acquisition/active-lease-guard-ops
                   node {:intent-id id
                         :lease-id (:lease-id old)
                         :worker-id (:worker-id old)})]
      (is (conflict?
            #(xt/execute-tx node
                            (conj (vec guards)
                                  [:put-docs outbox/intent-table
                                   (assoc (dissoc before :xt/system-from)
                                          :gesso.live.outbox/status :delivered)]))))
      (is (= before (read-intent node id))))))

(deftest malformed-state-and-missing-intent-rejected-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          other-id (random-uuid)
          _ (seeded-lease! node id (.minusSeconds (Instant/now) 120))
          original (read-intent node id)
          claim (lease-claim id "worker-b")]
      (is (= :reused-lease-id
             (rejection-reason #(acquisition/reclaim-expired!
                                 node (assoc claim :lease-id
                                             (:gesso.live.outbox/lease-id original))))))
      (is (= :unavailable
             (:status (acquisition/reclaim-expired!
                        node (assoc claim :intent-id other-id)))))
      (is (= :invalid-worker-id
             (rejection-reason #(acquisition/active-lease-guard-ops
                                 node {:intent-id id
                                       :worker-id ""
                                       :lease-id (:lease-id claim)}))))
      (is (= original (read-intent node id)))
      (xt/execute-tx node [[:put-docs outbox/intent-table
                            (dissoc (dissoc original :xt/system-from)
                                    :gesso.live.outbox/lease-id)]])
      (is (= :invalid-physical-lease
             (rejection-reason #(acquisition/reclaim-expired! node claim)))))))

(deftest current-lease-guard-permits-one-physical-state-transition-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          current (seeded-lease! node id (.plusSeconds (Instant/now) 120))
          before (read-intent node id)
          guards (acquisition/active-lease-guard-ops
                   node {:intent-id id
                         :worker-id (:worker-id current)
                         :lease-id (:lease-id current)})
          next-row (assoc (dissoc before :xt/system-from)
                          :gesso.live.outbox/audit-marker "fenced-write")]
      (is (some? (xt/execute-tx node (conj (vec guards)
                                         [:put-docs outbox/intent-table next-row]))))
      (is (= "fenced-write"
             (:gesso.live.outbox/audit-marker (read-intent node id))))
      ;; Same fence cannot be reused after even an otherwise valid update.
      (is (conflict? #(xt/execute-tx node (conj (vec guards)
                                                 [:put-docs outbox/intent-table
                                                  next-row])))))))
