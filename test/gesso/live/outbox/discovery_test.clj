(ns gesso.live.outbox.discovery-test
  "v2093: physical XTDB discovery of pending and expired-lease intent IDs."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.discovery :as discovery]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.time Instant]))

(defn- seed-intents!
  "Seed current outbox rows with explicit physical status and lease state."
  [node rows]
  (let [ids (mapv :id rows)
        plan (outbox/prepare!
              {:intent-ids ids :source-id "gesso/test-discovery"
               :operation :request/claim :purpose :gesso.live/refresh}
              (mapv (fn [id] {:topic :request :id (str id)}) ids))
        intents
        (mapv (fn [{:keys [status lease-until type version]} row]
                (cond-> (assoc row :gesso.live.outbox/status status)
                  (= :leased status)
                  (assoc :gesso.live.outbox/lease-until lease-until
                         :gesso.live.outbox/lease-id (random-uuid)
                         :gesso.live.outbox/lease-owner "stopped-worker")
                  type (assoc :gesso.live.outbox/type type)
                  version (assoc :gesso.live.outbox/version version)))
              rows (:intents plan))]
    (xt/execute-tx node (conj (:guard-ops plan)
                              (into [:put-docs outbox/intent-table] intents)))
    ids))

(defn- rejected? [f reason]
  (let [e (try (f) nil (catch clojure.lang.ExceptionInfo e e))]
    (and e
         (= :gesso.live.outbox/invalid-discovery
            (:error/type (ex-data e)))
         (= reason (:reason (ex-data e))))))

(deftest discovery-options-are-closed-and-bounded-test
  (doseq [[opts reason] [[nil :invalid-options]
                         [1 :invalid-options]
                         [{:limit 0} :invalid-limit]
                         [{:limit -1} :invalid-limit]
                         [{:limit 129} :invalid-limit]
                         [{:limit 1.1} :invalid-limit]
                         [{:limit "10"} :invalid-limit]
                         [{:after nil} :invalid-cursor]
                         [{:after "wrong"} :invalid-cursor]
                         [{:limit 1 :extra :untrusted} :unknown-fields]]]
    ;; Validation must precede even the attempted physical query.
    (is (rejected? #(discovery/discover-eligible! :unconnected opts) reason)
        (str opts))))

(deftest physical-discovery-returns-only-current-eligible-intents-test
  (with-open [node (xtn/start-node {})]
    (let [now (Instant/now)
          ids (vec (repeatedly 6 random-uuid))
          [pending expired active delivered wrong-type wrong-version] ids
          _ (seed-intents! node
                           [{:id pending :status :pending}
                            {:id expired :status :leased
                             :lease-until (.minusSeconds now 90)}
                            {:id active :status :leased
                             :lease-until (.plusSeconds now 90)}
                            {:id delivered :status :delivered}
                            {:id wrong-type :status :pending
                             :type :not/our-intent}
                            {:id wrong-version :status :pending
                             :version 99}])
          page (discovery/discover-eligible! node)
          found (set (:candidates page))]
      (is (= #{{:intent-id pending :action :acquire-pending}
               {:intent-id expired :action :reclaim-expired}}
             found))
      (is (false? (:has-more? page)))
      (is (nil? (:next-after page)))
      (is (= 2 (count (:candidates page))))
      ;; Discovery has no physical side effects; leases stay as they were.
      (is (= :pending (:gesso.live.outbox/status
                        (first (xt/q node ["SELECT * FROM gesso_live_outbox WHERE _id = ?" pending])))))
      (is (= :leased (:gesso.live.outbox/status
                       (first (xt/q node ["SELECT * FROM gesso_live_outbox WHERE _id = ?" expired]))))))))

(deftest physical-cursor-pages-exhaust-eligible-work-without-duplicates-test
  (with-open [node (xtn/start-node {})]
    (let [ids (vec (repeatedly 7 random-uuid))
          _ (seed-intents! node (mapv (fn [id] {:id id :status :pending}) ids))
          pages (loop [after nil, pages [], remaining 10]
                  (when (zero? remaining)
                    (throw (ex-info "Pagination did not terminate" {:pages pages})))
                  (let [opts (cond-> {:limit 2} after (assoc :after after))
                        page (discovery/discover-eligible! node opts)
                        pages' (conj pages page)]
                    (if (:has-more? page)
                      (recur (:next-after page) pages' (dec remaining))
                      pages')))
          candidates (vec (mapcat :candidates pages))]
      (is (= [2 2 2 1] (mapv (comp count :candidates) pages)))
      (is (= [true true true false] (mapv :has-more? pages)))
      (is (= (set ids) (set (map :intent-id candidates))))
      (is (= (count ids) (count candidates)))
      (is (= (count ids) (count (distinct (map :intent-id candidates)))))
      (is (every? #(= :acquire-pending (:action %)) candidates))
      (is (nil? (:next-after (last pages)))))))

(deftest physical-discovery-finds-newly-recoverable-lease-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          _ (seed-intents! node [{:id id :status :leased
                                   :lease-until (.plusSeconds (Instant/now) 120)}])
          first-page (discovery/discover-eligible! node {:limit 1})
          before (first (xt/q node ["SELECT *, _system_from FROM gesso_live_outbox WHERE _id = ?" id]))]
      (is (= [] (:candidates first-page)))
      ;; A persisted lease can become eligible without an in-memory ID index.
      (xt/execute-tx node [[:put-docs outbox/intent-table
                            (assoc (dissoc before :xt/system-from)
                                   :gesso.live.outbox/lease-until
                                   (.minusSeconds (Instant/now) 60))]])
      (let [second-page (discovery/discover-eligible! node {:limit 1})
            now (Instant/now)
            claim {:intent-id id, :lease-id (random-uuid), :worker-id "fresh-worker",
                   :now now, :lease-until (.plusSeconds now 60)}]
        (is (= [{:intent-id id :action :reclaim-expired}]
               (:candidates second-page)))
        (is (= :reclaimed (:status (acquisition/reclaim-expired! node claim))))
        (is (empty? (:candidates (discovery/discover-eligible! node))))))))

(deftest physical-stale-discovery-does-not-bypass-acquisition-cas-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          _ (seed-intents! node [{:id id :status :pending}])
          read-only (discovery/discover-eligible! node)
          now (Instant/now)
          claim {:intent-id id, :lease-id (random-uuid), :worker-id "worker-a",
                 :now now, :lease-until (.plusSeconds now 60)}]
      (is (= [{:intent-id id :action :acquire-pending}]
             (:candidates read-only)))
      (is (= :acquired (:status (acquisition/acquire-pending! node claim))))
      (is (= :unavailable
             (:status (acquisition/acquire-pending!
                       node (assoc claim :worker-id "worker-b"
                                   :lease-id (random-uuid))))))
      (is (empty? (:candidates (discovery/discover-eligible! node)))))))

(deftest physical-discovery-on-empty-outbox-is-empty-test
  ;; A worker may start before Gesso has ever persisted an outbox document.
  (with-open [node (xtn/start-node {})]
    (is (= {:candidates [] :has-more? false :next-after nil}
           (discovery/discover-eligible! node)))))

(deftest physical-discovery-rejects-malformed-current-identity-test
  ;; Do not quietly hand a downstream worker an ID which the CAS acquisition
  ;; refuses to accept. Treat malformed selected rows as an explicit error.
  (with-open [node (xtn/start-node {})]
    (xt/execute-tx node
                   [[:put-docs outbox/intent-table
                     {:xt/id "not-a-uuid"
                      :gesso.live.outbox/type outbox/intent-type
                      :gesso.live.outbox/version outbox/intent-version
                      :gesso.live.outbox/status :pending}]])
    (is (rejected? #(discovery/discover-eligible! node)
                   :invalid-physical-candidate))))
