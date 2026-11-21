(ns gesso.live.outbox.worker-test
  "v2094: bounded physical discovery-to-CAS worker sweep tests. No delivery."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.discovery :as discovery]
            [gesso.live.outbox.worker :as worker]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.time Instant]))

(defn- seed!
  [node entries]
  (let [ids (mapv :id entries)
        plan (outbox/prepare!
              {:intent-ids ids :source-id "gesso/test-worker"
               :operation :request/claim :purpose :gesso.live/refresh}
              (mapv (fn [id] {:topic :request :id (str id)}) ids))
        docs (mapv
              (fn [{:keys [status expires]} intent]
                (cond-> (assoc intent :gesso.live.outbox/status status)
                  (= status :leased)
                  (assoc :gesso.live.outbox/lease-id (random-uuid)
                         :gesso.live.outbox/lease-owner "former-worker"
                         :gesso.live.outbox/lease-until expires)))
              entries (:intents plan))]
    (xt/execute-tx node (conj (:guard-ops plan)
                              (into [:put-docs outbox/intent-table] docs)))
    ids))

(defn- row [node id]
  (first (xt/q node ["SELECT * FROM gesso_live_outbox WHERE _id = ?" id])))

(defn- rejected? [f reason]
  (let [e (try (f) nil (catch clojure.lang.ExceptionInfo e e))]
    (and e
         (= :gesso.live.outbox/invalid-worker-request
            (:error/type (ex-data e)))
         (= reason (:reason (ex-data e))))))

(deftest worker-options-rejected-before-physical-query-test
  (doseq [[opts reason]
          [[nil :invalid-options]
           [{} :invalid-worker-id]
           [{:worker-id "   "} :invalid-worker-id]
           [{:worker-id "worker" :lease-millis 0} :invalid-lease-millis]
           [{:worker-id "worker" :lease-millis 300001} :invalid-lease-millis]
           [{:worker-id "worker" :page-size 0} :invalid-page-size]
           [{:worker-id "worker" :page-size 129} :invalid-page-size]
           [{:worker-id "worker" :max-pages 0} :invalid-max-pages]
           [{:worker-id "worker" :max-pages 17} :invalid-max-pages]
           [{:worker-id "worker" :after nil} :invalid-cursor]
           [{:worker-id "worker" :unexpected :value} :unknown-fields]]]
    (is (rejected? #(worker/acquire-sweep! :unconnected opts) reason)
        (pr-str opts))))

(deftest physical-worker-acquires-pending-and-reclaims-expired-test
  (with-open [node (xtn/start-node {})]
    (let [now (Instant/now)
          [pending expired active delivered]
          (seed! node [{:id (random-uuid) :status :pending}
                       {:id (random-uuid) :status :leased
                        :expires (.minusSeconds now 120)}
                       {:id (random-uuid) :status :leased
                        :expires (.plusSeconds now 120)}
                       {:id (random-uuid) :status :delivered}])
          before-active (row node active)
          result (worker/acquire-sweep! node {:worker-id "process-a"
                                               :page-size 1 :max-pages 4})
          statuses (set (map :status (:results result)))]
      (is (= 2 (:attempted result)))
      (is (= #{:acquired :reclaimed} statuses))
      (is (= 1 (:acquired result)))
      (is (= 1 (:reclaimed result)))
      (is (= 2 (:pages-scanned result)))
      (is (= :observed-end (:halt-reason result)))
      (is (nil? (:next-after result)))
      (is (= :leased (:gesso.live.outbox/status (row node pending))))
      (is (= :leased (:gesso.live.outbox/status (row node expired))))
      (is (= before-active (row node active)))
      (is (= :delivered (:gesso.live.outbox/status (row node delivered))))
      (doseq [{:keys [status intent-id lease-id worker-id]} (:results result)]
        (is (#{:acquired :reclaimed} status))
        (is (uuid? lease-id))
        (is (= "process-a" worker-id))
        (is (= lease-id (:gesso.live.outbox/lease-id (row node intent-id)))))
      ;; The second sweep cannot rediscover still-active leases.
      (is (= :observed-end
             (:halt-reason (worker/acquire-sweep! node
                                                  {:worker-id "process-b"}))))
      (is (zero? (:attempted (worker/acquire-sweep! node
                                                      {:worker-id "process-b"})))))))

(deftest physical-page-budget-exposes-resumable-keyset-cursor-test
  (with-open [node (xtn/start-node {})]
    (let [ids (seed! node (mapv (fn [_] {:id (random-uuid) :status :pending})
                                (range 5)))
          options {:worker-id "worker-a" :page-size 2 :max-pages 1}
          a (worker/acquire-sweep! node options)
          b (worker/acquire-sweep! node (assoc options :after (:next-after a)))
          c (worker/acquire-sweep! node (assoc options :after (:next-after b)))
          found (mapcat :results [a b c])]
      (is (= [:page-budget :page-budget :observed-end]
             (mapv :halt-reason [a b c])))
      (is (= [2 2 1] (mapv :attempted [a b c])))
      (is (uuid? (:next-after a)))
      (is (uuid? (:next-after b)))
      (is (nil? (:next-after c)))
      (is (= (set ids) (set (map :intent-id found))))
      (is (= 5 (count (distinct (map :intent-id found)))))
      (is (every? #(= :acquired (:status %)) found))
      (is (zero? (:attempted (worker/acquire-sweep! node options)))))))

(deftest stale-discovery-does-not-grant-a-second-worker-lease-test
  (with-open [node (xtn/start-node {})]
    (let [[id] (seed! node [{:id (random-uuid) :status :pending}])
          original-discover discovery/discover-eligible!
          hijacked? (atom false)
          result
          (with-redefs [discovery/discover-eligible!
                        (fn [n opts]
                          (let [page (original-discover n opts)]
                            (when (compare-and-set! hijacked? false true)
                              (let [now (Instant/now)]
                                (is (= :acquired
                                       (:status (acquisition/acquire-pending!
                                                 n {:intent-id id :worker-id "winner"
                                                    :lease-id (random-uuid)
                                                    :now now
                                                    :lease-until (.plusSeconds now 60)}))))))
                            page))]
            (worker/acquire-sweep! node {:worker-id "loser"}))]
      (is (= 1 (:attempted result)))
      (is (= 1 (:unavailable result)))
      (is (zero? (:acquired result)))
      (is (= :leased (:gesso.live.outbox/status (row node id))))
      (is (= "winner" (:gesso.live.outbox/lease-owner (row node id)))))))

(deftest unexpected-acquisition-errors-are-not-silently-converted-test
  (with-open [node (xtn/start-node {})]
    (let [[id] (seed! node [{:id (random-uuid) :status :pending}])]
      (with-redefs [acquisition/acquire-pending!
                    (fn [& _] (throw (ex-info "indeterminate" {:phase :submit})))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"indeterminate"
                              (worker/acquire-sweep! node {:worker-id "worker"}))))
      (is (= :pending (:gesso.live.outbox/status (row node id)))))))

(deftest physical-empty-worker-sweep-is-bounded-and-read-only-test
  (with-open [node (xtn/start-node {})]
    (is (= {:pages-scanned 1 :attempted 0 :acquired 0 :reclaimed 0
            :contended 0 :unavailable 0 :halt-reason :observed-end
            :next-after nil :results [] :worker-id "worker"}
           (worker/acquire-sweep! node {:worker-id "worker"})))))
