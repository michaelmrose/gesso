(ns gesso.live.outbox.maintenance-policy-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.assurance.xtdb-envelope :as envelope]
            [gesso.assurance.xtdb-source-log :as source-log]
            [gesso.live.outbox.history-coverage :as history]
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
              second-claim (claim expired-id)
              read-row (fn [id] (first (xt/q node ["SELECT *, _system_from FROM gesso_live_outbox WHERE _id = ?" id])))
              verify! (fn [id kind execute!]
                        (let [before (read-row id)
                              result (execute!)
                              after (read-row id)
                              time (get-in result [:consistency :system-time])
                              tx-id (get-in result [:consistency :tx-id])
                              expected (policy/historical-worker-envelope "s" time before after)]
                          (is (= kind (:maintenance expected)))
                          (is (= :matched (:status (xtdb/check-worker-envelope! (xtdb/adapter-connectable! node) tx-id (:ops expected)))))
                          result))]
          (is (= :acquired (:status (verify! pending-id :lease-acquisition #(acquisition/acquire-pending! node first-claim)))))
          (is (= :reclaimed (:status (verify! expired-id :expired-lease-reclamation #(acquisition/reclaim-expired! node second-claim)))))
          (doseq [c [first-claim second-claim]]
            (is (= :acknowledged (:status (verify! (:intent-id c) :dispatch-acknowledgment
                                          #(ack/acknowledge-dispatch!
                                            node (assoc (select-keys c [:intent-id :worker-id :lease-id])
                                                        :receipt {:receipt-id (random-uuid)
                                                                  :adapter :example/adapter :result :accepted})))))))
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

(deftest historical-worker-envelope-checks-retained-operations
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          pending {:xt/id id :gesso.live.outbox/type outbox/intent-type
                   :gesso.live.outbox/version outbox/intent-version
                   :gesso.live.outbox/status :pending :gesso.live.outbox/source-id "s"
                   :gesso.live.outbox/command-id "c1" :extra "preserve"}
          read-row #(first (xt/q node ["SELECT *, _system_from FROM gesso_live_outbox WHERE _id = ?" id]))
          classify (fn [before after tx-id]
                     (let [time (.toInstant ^java.time.ZonedDateTime (:xt/system-from after))
                           expected (policy/historical-worker-envelope "s" time before after)]
                       (is (some? expected))
                       (#'history/classify-worker! node {:tx-id tx-id} (assoc expected :intent-id id))))]
      (doseq [[mode expected] [[:valid :verified-infrastructure-transition]
                               [:unguarded :unclassified]
                               [:extra-write :unclassified]
                               [:extra-assertion :unclassified]
                               [:changed-argument :unclassified]
                               [:duplicate-put :unclassified]]]
        (xt/execute-tx node [[:put-docs :gesso_live_outbox pending]])
        (let [before (read-row)
              deadline (.plusSeconds (Instant/now) 60)
              leased (assoc pending :gesso.live.outbox/status :leased
                            :gesso.live.outbox/lease-owner "w" :gesso.live.outbox/lease-id (random-uuid)
                            :gesso.live.outbox/lease-until deadline)
              guards [["ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ?)" id (:xt/system-from before)]
                      ["ASSERT CURRENT_TIMESTAMP < ?" deadline]
                      ["ASSERT CURRENT_TIMESTAMP + INTERVAL '5' MINUTE >= ?" deadline]]
              put [:put-docs :gesso_live_outbox leased]
              ops (case mode
                    :valid (conj guards put)
                    :unguarded [put]
                    :extra-write (conj guards put [:put-docs :unselected {:xt/id "hidden" :value 1}])
                    :extra-assertion (into [["ASSERT TRUE"]] (conj guards put))
                    :changed-argument (conj (assoc-in guards [1 1] (.plusSeconds deadline 1)) put)
                    :duplicate-put (conj guards put put))
              tx-id (:tx-id (xt/execute-tx node ops))
              after (read-row)
              report (classify before after tx-id)]
          (is (= expected (:classification report)) (name mode))
          (is (= (when (= :valid mode) :matched) (:worker-envelope report)))
          (is (= (when-not (= :valid mode) :historical-worker-envelope-mismatch) (:reason report)))
          (is (not (contains? report :ops)))
          (when (= :valid mode)
            (let [expected-ops (:ops (policy/historical-worker-envelope "s" (.toInstant ^java.time.ZonedDateTime (:xt/system-from after)) before after))]
              (is (= :unsupported (:status (envelope/check-envelope! node tx-id expected-ops true))))
              (with-redefs [source-log/observe-source-message! (fn [& _] {:status :unavailable :reason :source-message-not-retained})]
                (is (= :historical-worker-envelope-unavailable (:reason (classify before after tx-id))))))))))))
