(ns gesso.live.outbox.acknowledgment-test
  "v2095: guarded XTDB acceptance recording; no actual SSE delivery."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.live.consistency.xtdb :as live.xtdb]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.acknowledgment :as acknowledgment]
            [gesso.live.outbox.discovery :as discovery]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.time Instant]))

(defn- submit [intent-id lease-id worker-id]
  {:intent-id intent-id
   :lease-id lease-id
   :worker-id worker-id
   :receipt {:receipt-id (random-uuid)
             :adapter :gesso.live/dispatch
             :result :accepted}})

(defn- seed! [node intent-id]
  (let [plan (outbox/prepare!
              {:intent-ids [intent-id]
               :source-id "gesso/test-acknowledgment"
               :operation :request/claim
               :purpose :gesso.live/refresh}
              [{:topic :request :id "claim-1" :change/kind :updated}])]
    (xt/execute-tx node (conj (:guard-ops plan) (:tx-op plan)))))

(defn- pending->leased! [node intent-id worker]
  (let [now (Instant/now)
        lease-id (random-uuid)
        result (acquisition/acquire-pending!
                node {:intent-id intent-id :worker-id worker
                      :lease-id lease-id :now now
                      :lease-until (.plusSeconds now 60)})]
    (is (= :acquired (:status result)))
    lease-id))

(defn- row [node intent-id]
  (first (xt/q node
               ["SELECT *, _system_from FROM gesso_live_outbox WHERE _id = ?"
                intent-id])))

(defn- reason [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo ex
      (when (= :gesso.live.outbox/invalid-acknowledgment
               (:error/type (ex-data ex)))
        (:reason (ex-data ex))))))

(deftest closed-receipt-and-lease-input-validation-test
  (let [id (random-uuid)
        lease-id (random-uuid)
        ok (submit id lease-id "w")]
    (doseq [[candidate expected]
            [[nil :invalid-submission]
             [(assoc ok :intent-id "bad") :invalid-intent-id]
             [(assoc ok :lease-id "bad") :invalid-lease-id]
             [(assoc ok :worker-id " ") :invalid-worker-id]
             [(assoc ok :unknown :unsafe) :unknown-fields]
             [(assoc ok :receipt nil) :invalid-receipt]
             [(assoc-in ok [:receipt :receipt-id] "bad") :invalid-receipt-id]
             [(assoc-in ok [:receipt :adapter] :unqualified) :invalid-adapter]
             [(assoc-in ok [:receipt :result] :failed) :not-accepted]
             [(assoc-in ok [:receipt :secret] "must-not-persist")
              :unknown-receipt-fields]]]
      ;; Invalid request never even attempts to read an XTDB node.
      (is (= expected
             (reason #(acknowledgment/acknowledge-dispatch!
                       :unconnected candidate)))
          (pr-str candidate)))))

(deftest physical-terminal-ack-preserves-intent-and-receipt-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          _ (seed! node id)
          lease-id (pending->leased! node id "worker-a")
          receipt (submit id lease-id "worker-a")
          before (row node id)
          result (acknowledgment/acknowledge-dispatch! node receipt)
          after (row node id)]
      (is (= :acknowledged (:status result)))
      (is (some? (get-in result [:consistency :tx-id])))
      (is (= (:receipt-id (:receipt receipt)) (:receipt-id result)))
      (is (= :dispatch-accepted (:gesso.live.outbox/status after)))
      (is (= (:receipt-id (:receipt receipt))
             (:gesso.live.outbox/dispatch-receipt-id after)))
      (is (= :gesso.live/dispatch
             (:gesso.live.outbox/dispatch-adapter after)))
      (is (= lease-id (:gesso.live.outbox/lease-id after)))
      (is (= "worker-a" (:gesso.live.outbox/lease-owner after)))
      (is (not= (:xt/system-from before) (:xt/system-from after)))
      (is (= (dissoc before :xt/system-from :gesso.live.outbox/status)
             (dissoc after :xt/system-from :gesso.live.outbox/status
                     :gesso.live.outbox/dispatch-receipt-id
                     :gesso.live.outbox/dispatch-adapter)))
      ;; Terminal acceptance is not eligible for work recovery.
      (is (empty? (:candidates (discovery/discover-eligible! node {:source-id "gesso/test-acknowledgment"}))))
      (is (= :unavailable (:status (acknowledgment/acknowledge-dispatch!
                                  node receipt))))
      (is (= after (row node id))))))

(deftest pending-missing-and-stale-lease-cannot-acknowledge-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          absent (random-uuid)
          _ (seed! node id)
          claim (submit id (random-uuid) "worker-a")]
      (is (= {:status :unavailable :reason :missing :intent-id absent}
             (acknowledgment/acknowledge-dispatch!
              node (assoc claim :intent-id absent))))
      (is (= :not-leased (:reason (acknowledgment/acknowledge-dispatch!
                                  node claim))))
      (let [lease-id (pending->leased! node id "worker-a")
            before (row node id)]
        (is (= :stale-lease
               (reason #(acknowledgment/acknowledge-dispatch!
                         node claim))))
        (is (= :stale-lease
               (reason #(acknowledgment/acknowledge-dispatch!
                         node (submit id lease-id "worker-b")))))
        (is (= before (row node id)))))))

(deftest expired-lease-rejected-by-xtdb-transaction-clock-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          _ (seed! node id)
          lease-id (random-uuid)
          expired (assoc (dissoc (row node id) :xt/system-from)
                         :gesso.live.outbox/status :leased
                         :gesso.live.outbox/lease-id lease-id
                         :gesso.live.outbox/lease-owner "old"
                         :gesso.live.outbox/lease-until
                         (.minusSeconds (Instant/now) 120))
          _ (xt/execute-tx node [[:put-docs outbox/intent-table expired]])
          before (row node id)
          result (acknowledgment/acknowledge-dispatch!
                  node (submit id lease-id "old"))]
      (is (= :contended (:status result)))
      (is (= before (row node id)))
      (is (= :leased (:gesso.live.outbox/status (row node id)))))))

(deftest physical-stale-worker-cannot-acknowledge-reclaimed-lease-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          _ (seed! node id)
          old-id (random-uuid)
          expired (assoc (dissoc (row node id) :xt/system-from)
                         :gesso.live.outbox/status :leased
                         :gesso.live.outbox/lease-id old-id
                         :gesso.live.outbox/lease-owner "old"
                         :gesso.live.outbox/lease-until
                         (.minusSeconds (Instant/now) 120))
          _ (xt/execute-tx node [[:put-docs outbox/intent-table expired]])
          now (Instant/now)
          reclaimed (acquisition/reclaim-expired!
                     node {:intent-id id :worker-id "new"
                           :lease-id (random-uuid)
                           :now now :lease-until (.plusSeconds now 60)})
          before (row node id)]
      (is (= :reclaimed (:status reclaimed)))
      (is (= :stale-lease
             (reason #(acknowledgment/acknowledge-dispatch!
                       node (submit id old-id "old")))))
      (is (= before (row node id)))
      (is (= :acknowledged
             (:status (acknowledgment/acknowledge-dispatch!
                       node (submit id (:lease-id reclaimed) "new"))))))))

(deftest physical-race-between-read-and-terminal-write-is-fenced-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          _ (seed! node id)
          lease-id (pending->leased! node id "winner")
          receipt (submit id lease-id "winner")
          original-execute live.xtdb/execute-tx!
          injected? (atom false)
          outcome
          (with-redefs [live.xtdb/execute-tx!
                        (fn [n & args]
                          ;; Between acknowledgment's read and its guarded
                          ;; write, another physical writer changes the row.
                          (when (compare-and-set! injected? false true)
                            (xt/execute-tx n
                                           [[:put-docs outbox/intent-table
                                             (assoc (dissoc (row n id)
                                                            :xt/system-from)
                                                    :gesso.live.outbox/lease-id
                                                    (random-uuid)
                                                    :gesso.live.outbox/lease-owner
                                                    "new-winner")]]))
                          (apply original-execute n args))]
            (acknowledgment/acknowledge-dispatch! node receipt))]
      (is @injected?)
      (is (= :contended (:status outcome)))
      (is (= :leased (:gesso.live.outbox/status (row node id))))
      (is (= "new-winner" (:gesso.live.outbox/lease-owner (row node id)))))))

(deftest concurrent-acknowledgers-only-one-can-terminalize-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          _ (seed! node id)
          lease-id (pending->leased! node id "worker")
          a (submit id lease-id "worker")
          b (submit id lease-id "worker")
          gate (promise)
          fa (future @gate (acknowledgment/acknowledge-dispatch! node a))
          fb (future @gate (acknowledgment/acknowledge-dispatch! node b))]
      (deliver gate true)
      (let [results [@fa @fb]
            winners (filter #(= :acknowledged (:status %)) results)
            loser (filter #(#{:contended :unavailable} (:status %)) results)]
        (is (= 1 (count winners)) (pr-str results))
        (is (= 1 (count loser)) (pr-str results))
        (is (= :dispatch-accepted (:gesso.live.outbox/status (row node id))))
        (is (= (:receipt-id (first winners))
               (:gesso.live.outbox/dispatch-receipt-id (row node id))))))))

(deftest interrupted-physical-acknowledgment-retains-uncertainty-and-thread-signal-test
  ;; A trusted adapter has already reported acceptance when this guarded
  ;; physical XTDB transaction begins. If it throws InterruptedException,
  ;; there is no definitive commit result: the adapter must propagate the
  ;; original error, not return :contended or fabricate :acknowledged.
  ;; JVM cancellation must remain visible even if the physical operation
  ;; cleared the caller's interrupted bit before throwing.
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          _ (seed! node id)
          lease-id (pending->leased! node id "worker")
          submission (submit id lease-id "worker")
          before (row node id)
          cause (InterruptedException. "guarded acknowledgment interrupted")
          attempts (atom 0)]
      (try
        (Thread/interrupted)
        (let [caught (with-redefs [live.xtdb/execute-tx!
                                   (fn [& _]
                                     (swap! attempts inc)
                                     (throw cause))]
                       (try
                         (acknowledgment/acknowledge-dispatch! node submission)
                         (catch Throwable error error)))
              interrupted? (.isInterrupted (Thread/currentThread))]
          (Thread/interrupted)
          (is (identical? cause caught)
              "Unknown physical XTDB outcome must retain the original error")
          (is interrupted?
              "Guarded acknowledgment interruption must retain JVM cancellation")
          (is (= 1 @attempts) "Only one transaction attempt is permitted")
          (is (= before (row node id))
              "An interrupted transaction may not fabricate terminal state")
          (is (= :leased (:gesso.live.outbox/status (row node id))))
          (is (nil? (:gesso.live.outbox/dispatch-receipt-id (row node id)))))
        (finally
          (Thread/interrupted))))))

(deftest unexpected-provider-errors-are-not-called-safe-rejections-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          _ (seed! node id)
          lease-id (pending->leased! node id "worker")
          before (row node id)]
      (with-redefs [live.xtdb/execute-tx!
                    (fn [& _] (throw (ex-info "unknown-result" {:phase :commit})))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unknown-result"
                              (acknowledgment/acknowledge-dispatch!
                               node (submit id lease-id "worker")))))
      (is (= before (row node id))))))
