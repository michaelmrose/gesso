(ns gesso.live.outbox.maintenance-policy-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.acknowledgment :as ack]
            [gesso.live.outbox.maintenance-policy :as policy]
            [gesso.live.outbox.publication-manifest :as manifest]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.time Instant]))

(deftest exact-maintenance-policy-preserves-physical-payload
  (with-open [node (xtn/start-node {})]
    (let [scope {:source-id "s" :worker-id "w" :descriptor "profile"}
          pending-id (random-uuid)
          expired-id (random-uuid)
          now (Instant/now)
          pending {:xt/id pending-id :gesso.live.outbox/type outbox/intent-type
                   :gesso.live.outbox/version outbox/intent-version
                   :gesso.live.outbox/status :pending :gesso.live.outbox/source-id "s"
                   :gesso.live.outbox/operation :example/update
                   :gesso.live.outbox/purpose :gesso.live/refresh
                   :gesso.live.outbox/topic :example :gesso.live.outbox/subject-id "r1"
                   :gesso.live.outbox/command-id "c1" :extra "preserve"}
          expired (assoc pending :xt/id expired-id :gesso.live.outbox/status :leased
                         :gesso.live.outbox/lease-id (random-uuid)
                         :gesso.live.outbox/lease-owner "previous"
                         :gesso.live.outbox/lease-until (.minusSeconds now 60))
          seen (atom [])
          check #(policy/admit node scope %)
          release (xtdb/register-submission-policy!
                   node (fn [{:keys [tx-ops]}]
                          (when-let [kind (check tx-ops)]
                            (let [i (dec (count tx-ops))]
                              (is (nil? (check (into [["ASSERT TRUE"]] tx-ops))))
                              (is (nil? (check (assoc-in tx-ops [i 2 :extra] "changed"))))
                              (is (nil? (check (assoc-in tx-ops [0 1] "wrong"))))
                              (if (= :manifest (:kind kind))
                                (is (nil? (check (assoc-in tx-ops [i 2 :descriptor] "other"))))
                                (do
                                  (is (nil? (check (update-in tx-ops [i 2] dissoc :extra))))
                                  (is (nil? (check (assoc-in tx-ops [i 2 :gesso.live.outbox/subject-id] "r2"))))
                                  (is (nil? (check (assoc-in tx-ops [i 2 :gesso.live.outbox/lease-owner] "other"))))
                                  (is (nil? (check (assoc-in tx-ops [i 2 :gesso.live.outbox/source-id] "other"))))))
                            (swap! seen conj [kind tx-ops]) true))))]
      (try
        (manifest/require-compatible! node "s" "profile")
        (xt/execute-tx node [[:put-docs :gesso_live_outbox pending expired]])
        (let [claim (fn [id] {:intent-id id :source-id "s" :worker-id "w"
                              :lease-id (random-uuid) :now (Instant/now)
                              :lease-until (.plusSeconds (Instant/now) 60)})
              first-claim (claim pending-id)
              second-claim (claim expired-id)]
          (is (= :acquired (:status (acquisition/acquire-pending! node first-claim))))
          (is (= :reclaimed (:status (acquisition/reclaim-expired! node second-claim))))
          (doseq [c [first-claim second-claim]]
            (is (= :acknowledged (:status (ack/acknowledge-dispatch!
                                          node (assoc (select-keys c [:intent-id :worker-id :lease-id])
                                                      :receipt {:receipt-id (random-uuid)
                                                                :adapter :example/adapter :result :accepted}))))))
          (is (= [:manifest :worker :worker :worker :worker] (mapv (comp :kind first) @seen)))
          (is (every? #(nil? (check (second %))) (rest @seen)))
          (is (nil? (check [42])))
          (is (nil? (check [[:put-docs :requests {:xt/id "r1"}]])))
          (is (= #{"preserve"} (set (map :extra (xt/q node ["SELECT * FROM gesso_live_outbox"])))))
          (is (= #{:dispatch-accepted} (set (map :gesso.live.outbox/status
                                                (xt/q node ["SELECT * FROM gesso_live_outbox"]))))))
        (finally (release))))))

(deftest historical-worker-relation-checks-clock-and-complete-payload
  (let [time (Instant/parse "2026-01-01T00:00:00Z")
        at #(java.time.ZonedDateTime/ofInstant (.plusSeconds time %) java.time.ZoneOffset/UTC)
        pending {:xt/id (random-uuid) :xt/system-from (at -100)
                 :gesso.live.outbox/type outbox/intent-type
                 :gesso.live.outbox/version outbox/intent-version
                 :gesso.live.outbox/status :pending :gesso.live.outbox/source-id "s"
                 :gesso.live.outbox/command-id "c1" :extra "preserve"}
        leased (assoc pending :xt/system-from (at 0) :gesso.live.outbox/status :leased
                      :gesso.live.outbox/lease-id (random-uuid)
                      :gesso.live.outbox/lease-owner "w" :gesso.live.outbox/lease-until (at 60))
        expired (assoc leased :xt/system-from (at -100) :gesso.live.outbox/lease-until (at -1)
                       :gesso.live.outbox/lease-id (random-uuid))
        active (assoc expired :gesso.live.outbox/lease-until (at 1))
        acknowledged (assoc active :xt/system-from (at 0) :gesso.live.outbox/status :dispatch-accepted
                            :gesso.live.outbox/dispatch-receipt-id (random-uuid)
                            :gesso.live.outbox/dispatch-adapter :example/adapter)
        observe #(policy/observe-worker-transition "s" time %1 %2)]
    (is (= :lease-acquisition (observe pending leased)))
    (is (= :expired-lease-reclamation (observe expired leased)))
    (is (= :dispatch-acknowledgment (observe active acknowledged)))
    (doseq [bad [(assoc leased :extra "changed") (dissoc leased :extra)
                 (assoc leased :xt/system-from (at 1))
                 (assoc leased :gesso.live.outbox/source-id "other")
                 (assoc leased :gesso.live.outbox/lease-owner "")
                 (assoc leased :gesso.live.outbox/lease-until (at 0))
                 (assoc leased :gesso.live.outbox/lease-until (at 301))]]
      (is (nil? (observe pending bad))))
    (is (nil? (observe active leased)))
    (is (nil? (observe expired (assoc leased :gesso.live.outbox/lease-id (:gesso.live.outbox/lease-id expired)))))
    (is (nil? (observe expired (assoc acknowledged :gesso.live.outbox/lease-until (at -1)))))
    (is (nil? (observe active (assoc acknowledged :gesso.live.outbox/lease-owner "other"))))
    (is (nil? (observe active (assoc acknowledged :gesso.live.outbox/dispatch-receipt-id "fake"))))
    (is (nil? (policy/observe-worker-transition "other" time pending leased)))))
