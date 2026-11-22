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
           [{:worker-id "w" :source-id nil} :invalid-source-id]
           [{:worker-id "w" :source-id " "} :invalid-source-id]
           [{:worker-id "   "} :invalid-worker-id]
           [{:worker-id "worker" :lease-millis 0} :invalid-lease-millis]
           [{:worker-id "worker" :lease-millis 300001} :invalid-lease-millis]
           [{:worker-id "worker" :page-size 0} :invalid-page-size]
           [{:worker-id "worker" :page-size 129} :invalid-page-size]
           [{:worker-id "worker" :max-pages 0} :invalid-max-pages]
           [{:worker-id "worker" :max-pages 17} :invalid-max-pages]
           [{:worker-id "worker" :after nil} :invalid-cursor]
           [{:worker-id "worker" :unexpected :value} :unknown-fields]]]
    (is (rejected? #(worker/acquire-sweep! :unconnected (if (map? opts) (cond-> opts (not (contains? opts :source-id)) (assoc :source-id "gesso/test-worker")) opts)) reason)
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
          result (worker/acquire-sweep! node {:source-id "gesso/test-worker" :worker-id "process-a"
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
                                                  {:source-id "gesso/test-worker" :worker-id "process-b"}))))
      (is (zero? (:attempted (worker/acquire-sweep! node
                                                      {:source-id "gesso/test-worker" :worker-id "process-b"})))))))

(deftest physical-page-budget-exposes-resumable-keyset-cursor-test
  (with-open [node (xtn/start-node {})]
    (let [ids (seed! node (mapv (fn [_] {:id (random-uuid) :status :pending})
                                (range 5)))
          options {:source-id "gesso/test-worker" :worker-id "worker-a" :page-size 2 :max-pages 1}
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
            (worker/acquire-sweep! node {:source-id "gesso/test-worker" :worker-id "loser"}))]
      (is (= 1 (:attempted result)))
      (is (= 1 (:unavailable result)))
      (is (zero? (:acquired result)))
      (is (= :leased (:gesso.live.outbox/status (row node id))))
      (is (= "winner" (:gesso.live.outbox/lease-owner (row node id)))))))

(deftest malformed-discovery-pages-never-signal-observed-end-test
  ;; Discovery is advisory, but its page shape is a mandatory framework
  ;; contract. A truncated/corrupt response must not certify an empty queue.
  (let [id (random-uuid)
        valid {:intent-id id :action :acquire-pending}
        cases [nil
               {}
               {:candidates nil :has-more? false :next-after nil}
               {:candidates [] :has-more? nil :next-after nil}
               {:candidates [] :has-more? :unknown :next-after nil}
               {:candidates [] :has-more? true :next-after id}
               {:candidates [] :has-more? false :next-after id}
               {:candidates [valid] :has-more? true :next-after nil}
               {:candidates [valid] :has-more? true :next-after (random-uuid)}
               {:candidates [valid] :has-more? true :next-after id}]]
    (doseq [page cases]
      (let [calls (atom 0)
            opts {:source-id "gesso/test-worker" :worker-id "worker"
                  :page-size 2 :max-pages 1}]
        (with-redefs [discovery/discover-eligible! (fn [_ _] page)
                      acquisition/acquire-pending!
                      (fn [& _] (swap! calls inc) {:status :acquired})]
          (is (rejected? #(worker/acquire-sweep! :no-xtdb opts)
                         :invalid-discovery-page)
              (str "Malformed discovery must be rejected: " (pr-str page)))
          (is (zero? @calls)
              "Malformed discovery must not commit any acquisition"))))))

(deftest invalid-discovery-candidates-rejected-before-first-lease-test
  ;; Whole-page validation must precede the first physical CAS. Otherwise a
  ;; malformed later candidate can strand an earlier lease as partial work.
  (let [a (random-uuid)
        b (random-uuid)
        good {:intent-id a :action :acquire-pending}
        invalids [[good {:intent-id b :action :unrecognized}]
                  [good {:intent-id :not-uuid :action :acquire-pending}]
                  [good {:intent-id a :action :reclaim-expired}]
                  [good nil]]]
    (doseq [candidates invalids]
      (let [calls (atom [])
            page {:candidates candidates :has-more? false :next-after nil}]
        (with-redefs [discovery/discover-eligible! (fn [_ _] page)
                      acquisition/acquire-pending!
                      (fn [_ request]
                        (swap! calls conj (:intent-id request))
                        {:status :acquired})
                      acquisition/reclaim-expired!
                      (fn [_ request]
                        (swap! calls conj (:intent-id request))
                        {:status :reclaimed})]
          (is (rejected?
               #(worker/acquire-sweep! :no-xtdb
                                       {:source-id "gesso/test-worker"
                                        :worker-id "worker"})
               :invalid-discovery-page)
              (str "Invalid candidate must reject the page: " (pr-str candidates)))
          (is (empty? @calls)
              "Invalid later candidate cannot commit an earlier lease"))))))

(deftest malformed-discovery-continuation-cannot-loop-or-commit-test
  ;; A continuation that repeats the input cursor must not be accepted as
  ;; progress; nor can an incomplete has-more page justify further scanning.
  (let [after (random-uuid)
        page {:candidates [{:intent-id after :action :acquire-pending}]
              :has-more? true :next-after after}
        calls (atom 0)]
    (with-redefs [discovery/discover-eligible! (fn [_ _] page)
                  acquisition/acquire-pending!
                  (fn [& _] (swap! calls inc) {:status :acquired})]
      (is (rejected?
           #(worker/acquire-sweep! :no-xtdb
                                   {:source-id "gesso/test-worker"
                                    :worker-id "worker" :after after
                                    :page-size 1 :max-pages 2})
           :invalid-discovery-page))
      (is (zero? @calls)))))


(deftest malformed-discovery-keyset-order-rejects-before-acquisition-test
  ;; :next-after merely differing from :after does not prove progress. An
  ;; advisory provider returning reversed identities or a row <= :after must
  ;; fail before ANY physical lease transition, regardless of :has-more?.
  (let [a (java.util.UUID/fromString "00000000-0000-0000-0000-000000000001")
        b (java.util.UUID/fromString "00000000-0000-0000-0000-000000000002")
        c (java.util.UUID/fromString "00000000-0000-0000-0000-000000000003")
        item (fn [id] {:intent-id id :action :acquire-pending})
        cases [[:reversed-terminal nil [b a] false nil]
               [:reversed-continuation nil [b a] true a]
               [:behind-initial-cursor b [a] false nil]
               [:equal-initial-cursor b [b] false nil]
               [:backward-continuation b [c a] true a]
               [:backward-terminal b [c a] false nil]]]
    (doseq [[label after ids more? cursor] cases]
      (let [calls (atom [])
            page {:candidates (mapv item ids)
                  :has-more? more?
                  :next-after cursor}
            options (cond-> {:source-id "gesso/test-worker"
                             :worker-id "worker" :page-size 2 :max-pages 2}
                      after (assoc :after after))]
        (with-redefs [discovery/discover-eligible! (fn [_ _] page)
                      acquisition/acquire-pending!
                      (fn [_ claim]
                        (swap! calls conj (:intent-id claim))
                        {:status :acquired})]
          (is (rejected? #(worker/acquire-sweep! :no-xtdb options)
                         :invalid-discovery-page)
              (str label " must reject a non-advancing discovery page"))
          (is (empty? @calls)
              (str label " must not acquire a lease before validation")))))))

(deftest canonical-uuid-keyset-order-crosses-java-signed-boundary-test
  ;; XTDB orders UUID SQL keys by unsigned canonical representation. Java's
  ;; UUID.compareTo places 8000... before 7fff...; do NOT use it for this
  ;; validation. Both an entire ascending page and an advancing resume must
  ;; remain legal, even when they cross that signed Java representation.
  (let [low (java.util.UUID/fromString "7fffffff-ffff-ffff-ffff-ffffffffffff")
        high (java.util.UUID/fromString "80000000-0000-0000-0000-000000000000")
        calls (atom [])
        item (fn [id] {:intent-id id :action :acquire-pending})]
    (doseq [[label after ids]
            [[:whole-page nil [low high]]
             [:resumed-page low [high]]]]
      (with-redefs [discovery/discover-eligible!
                    (fn [_ _]
                      {:candidates (mapv item ids)
                       :has-more? false :next-after nil})
                    acquisition/acquire-pending!
                    (fn [_ claim]
                      (swap! calls conj (:intent-id claim))
                      {:status :acquired :intent-id (:intent-id claim)})]
        (let [result (worker/acquire-sweep!
                      :no-xtdb (cond-> {:source-id "gesso/test-worker"
                                       :worker-id "worker" :page-size 2}
                                 after (assoc :after after)))]
          (is (= ids (mapv :intent-id (:results result)))
              (str label " must preserve actual XTDB ordering"))
          (is (= :observed-end (:halt-reason result)))
          (is (= (count ids) (:acquired result))))))
    (is (= [low high high] @calls))))

(deftest physical-xtdb-uuid-keyset-crosses-signed-boundary-test
  ;; Regress against the actual XTDB ORDER BY _id and WHERE _id > ? behavior,
  ;; not merely the worker's simulated provider response.
  (with-open [node (xtn/start-node {})]
    (let [low (java.util.UUID/fromString "7fffffff-ffff-ffff-ffff-ffffffffffff")
          high (java.util.UUID/fromString "80000000-0000-0000-0000-000000000000")
          _ (seed! node [{:id low :status :pending}
                         {:id high :status :pending}])
          opts {:source-id "gesso/test-worker" :worker-id "worker"
                :page-size 1 :max-pages 1}
          first-page (worker/acquire-sweep! node opts)
          next-page (worker/acquire-sweep! node
                                           (assoc opts :after (:next-after first-page)))]
      (is (= [low] (mapv :intent-id (:results first-page))))
      (is (= :page-budget (:halt-reason first-page)))
      (is (= low (:next-after first-page)))
      (is (= [high] (mapv :intent-id (:results next-page))))
      (is (= :observed-end (:halt-reason next-page)))
      (is (= :leased (:gesso.live.outbox/status (row node low))))
      (is (= :leased (:gesso.live.outbox/status (row node high)))))))

(deftest unexpected-acquisition-errors-are-not-silently-converted-test
  (with-open [node (xtn/start-node {})]
    (let [[id] (seed! node [{:id (random-uuid) :status :pending}])]
      (with-redefs [acquisition/acquire-pending!
                    (fn [& _] (throw (ex-info "indeterminate" {:phase :submit})))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"indeterminate"
                              (worker/acquire-sweep! node {:source-id "gesso/test-worker" :worker-id "worker"}))))
      (is (= :pending (:gesso.live.outbox/status (row node id)))))))

(deftest physical-empty-worker-sweep-is-bounded-and-read-only-test
  (with-open [node (xtn/start-node {})]
    (is (= {:pages-scanned 1 :attempted 0 :acquired 0 :reclaimed 0
            :contended 0 :unavailable 0 :halt-reason :observed-end
            :next-after nil :results [] :worker-id "worker"}
           (worker/acquire-sweep! node {:source-id "gesso/test-worker" :worker-id "worker"})))))

(deftest physical-worker-skips-other-source-without-leasing-test
  (with-open [node (xtn/start-node {})]
    (let [owned-id (java.util.UUID/fromString "00000000-0000-0000-0000-000000000002")
          other-id (java.util.UUID/fromString "00000000-0000-0000-0000-000000000001")
          own (seed! node [{:id owned-id :status :pending}])
          other (outbox/prepare!
                 {:intent-ids [other-id] :source-id "another/source"
                  :operation :request/claim :purpose :gesso.live/refresh}
                 [{:topic :request :id "foreign"}])]
      (xt/execute-tx node (conj (vec (:guard-ops other)) (:tx-op other)))
      (let [result (worker/acquire-sweep!
                    node {:worker-id "local" :source-id "gesso/test-worker"
                          :page-size 1 :max-pages 1})]
        (is (= [owned-id] (mapv :intent-id (:results result))))
        (is (= 1 (:acquired result)))
        (is (= :leased (:gesso.live.outbox/status (row node owned-id))))
        (is (= :pending (:gesso.live.outbox/status (row node other-id))))))))

(deftest physical-source-change-after-discovery-cannot-acquire-test
  ;; Advisory discovery sees the original source, then an external writer
  ;; changes the physical row. Worker must skip rather than lease wrong work.
  (with-open [node (xtn/start-node {})]
    (let [[id] (seed! node [{:id (random-uuid) :status :pending}])
          original discovery/discover-eligible!
          changed? (atom false)
          result
          (with-redefs [discovery/discover-eligible!
                        (fn [n opts]
                          (let [page (original n opts)]
                            (when (compare-and-set! changed? false true)
                              (xt/execute-tx n
                                [[:put-docs outbox/intent-table
                                  (assoc (row n id)
                                         :gesso.live.outbox/source-id
                                         "another/source")]]))
                            page))]
            (worker/acquire-sweep!
              node {:worker-id "local" :source-id "gesso/test-worker"}))]
      (is @changed?)
      (is (= 1 (:attempted result)))
      (is (= 1 (:unavailable result)))
      (is (= :source-mismatch (get-in result [:results 0 :reason])))
      (is (= :pending (:gesso.live.outbox/status (row node id))))
      (is (= "another/source" (:gesso.live.outbox/source-id (row node id)))))))
