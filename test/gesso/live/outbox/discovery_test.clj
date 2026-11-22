(ns gesso.live.outbox.discovery-test
  "v2093: physical XTDB discovery of pending and expired-lease intent IDs."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.discovery :as discovery]
            [gesso.live.outbox.worker :as worker]
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
                         [{:source-id nil} :invalid-source-id]
                         [{:source-id "  "} :invalid-source-id]
                         [{:limit 0} :invalid-limit]
                         [{:limit -1} :invalid-limit]
                         [{:limit 129} :invalid-limit]
                         [{:limit 1.1} :invalid-limit]
                         [{:limit "10"} :invalid-limit]
                         [{:after nil} :invalid-cursor]
                         [{:after "wrong"} :invalid-cursor]
                         [{:limit 1 :extra :untrusted} :unknown-fields]]]
    ;; Validation must precede even the attempted physical query.
    (is (rejected? #(discovery/discover-eligible! :unconnected (if (map? opts) (cond-> opts (not (contains? opts :source-id)) (assoc :source-id "gesso/test-discovery")) opts)) reason)
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
          page (discovery/discover-eligible! node {:source-id "gesso/test-discovery"})
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
                  (let [opts (cond-> {:limit 2 :source-id "gesso/test-discovery"} after (assoc :after after))
                        page (discovery/discover-eligible! node (assoc opts :source-id "gesso/test-discovery"))
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
          first-page (discovery/discover-eligible! node {:source-id "gesso/test-discovery" :limit 1})
          before (first (xt/q node ["SELECT *, _system_from FROM gesso_live_outbox WHERE _id = ?" id]))]
      (is (= [] (:candidates first-page)))
      ;; A persisted lease can become eligible without an in-memory ID index.
      (xt/execute-tx node [[:put-docs outbox/intent-table
                            (assoc (dissoc before :xt/system-from)
                                   :gesso.live.outbox/lease-until
                                   (.minusSeconds (Instant/now) 60))]])
      (let [second-page (discovery/discover-eligible! node {:source-id "gesso/test-discovery" :limit 1})
            now (Instant/now)
            claim {:intent-id id, :lease-id (random-uuid), :worker-id "fresh-worker",
                   :now now, :lease-until (.plusSeconds now 60)}]
        (is (= [{:intent-id id :action :reclaim-expired}]
               (:candidates second-page)))
        (is (= :reclaimed (:status (acquisition/reclaim-expired! node claim))))
        (is (empty? (:candidates (discovery/discover-eligible! node {:source-id "gesso/test-discovery"}))))))))

(deftest physical-stale-discovery-does-not-bypass-acquisition-cas-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          _ (seed-intents! node [{:id id :status :pending}])
          read-only (discovery/discover-eligible! node {:source-id "gesso/test-discovery"})
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
      (is (empty? (:candidates (discovery/discover-eligible! node {:source-id "gesso/test-discovery"})))))))

(deftest physical-discovery-on-empty-outbox-is-empty-test
  ;; A worker may start before Gesso has ever persisted an outbox document.
  (with-open [node (xtn/start-node {})]
    (is (= {:candidates [] :has-more? false :next-after nil}
           (discovery/discover-eligible! node {:source-id "gesso/test-discovery"})))))

(deftest physical-discovery-rejects-malformed-current-identity-test
  ;; Do not quietly hand a downstream worker an ID which the CAS acquisition
  ;; refuses to accept. Treat malformed selected rows as an explicit error.
  (with-open [node (xtn/start-node {})]
    (xt/execute-tx node
                   [[:put-docs outbox/intent-table
                     {:xt/id "not-a-uuid"
                      :gesso.live.outbox/type outbox/intent-type
                      :gesso.live.outbox/version outbox/intent-version
                      :gesso.live.outbox/source-id "gesso/test-discovery"
                      :gesso.live.outbox/status :pending}]])
    (is (rejected? #(discovery/discover-eligible! node {:source-id "gesso/test-discovery"})
                   :invalid-physical-candidate))))

(deftest physical-source-scoped-discovery-filters-before-cursor-and-limit-test
  ;; Interleaved IDs from a second source cannot fill our page or make a
  ;; cursor skip later owned records; both pending and expired are scoped.
  (with-open [node (xtn/start-node {})]
    (let [[a1 b1 a2 b2 a3 b3]
          (mapv #(java.util.UUID/fromString
                   (format "00000000-0000-0000-0000-%012d" %))
                (range 1 7))
          _ (seed-intents! node [{:id a1 :status :pending}
                                 {:id a2 :status :leased
                                  :lease-until (.minusSeconds (Instant/now) 90)}
                                 {:id a3 :status :pending}])
          other (outbox/prepare!
                 {:intent-ids [b1 b2 b3] :source-id "other/installed-source"
                  :operation :request/claim :purpose :gesso.live/refresh}
                 (mapv (fn [id] {:topic :request :id (str id)}) [b1 b2 b3]))
          _ (xt/execute-tx node (conj (vec (:guard-ops other))
                                     (:tx-op other)))
          pages (loop [after nil pages []]
                  (let [opts (cond-> {:source-id "gesso/test-discovery" :limit 1}
                               after (assoc :after after))
                        page (discovery/discover-eligible! node opts)]
                    (if (:has-more? page)
                      (recur (:next-after page) (conj pages page))
                      (conj pages page))))
          found (mapv (comp :intent-id first :candidates) pages)]
      (is (= [a1 a2 a3] found))
      (is (= [true true false] (mapv :has-more? pages)))
      (is (= [:acquire-pending :reclaim-expired :acquire-pending]
             (mapv (comp :action first :candidates) pages)))
      (is (= #{b1 b2 b3}
             (set (map :intent-id
                       (:candidates (discovery/discover-eligible!
                                     node {:source-id "other/installed-source"})))))))))

(deftest interrupted-physical-discovery-query-preserves-unknown-and-thread-signal-test
  ;; A failed XTDB read is an indeterminate discovery, not an empty page.
  ;; Cancellation from an interrupted physical query must remain visible
  ;; after the original exception escapes the discovery boundary.
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          _ (seed-intents! node [{:id id :status :pending}])
          cause (InterruptedException. "outbox discovery query interrupted")
          queries (atom 0)]
      (try
        (Thread/interrupted)
        (let [outcome (with-redefs [xt/q (fn [& _]
                                          (swap! queries inc)
                                          (throw cause))]
                        (try
                          (discovery/discover-eligible!
                           node {:source-id "gesso/test-discovery"})
                          (catch Throwable error error)))
              interrupted? (.isInterrupted (Thread/currentThread))]
          (Thread/interrupted)
          (is (identical? cause outcome)
              "The original query interruption must not become an empty page")
          (is interrupted? "The discovery read must retain JVM cancellation")
          (is (= 1 @queries))
          (is (= [{:intent-id id :action :acquire-pending}]
                 (:candidates
                  (discovery/discover-eligible!
                   node {:source-id "gesso/test-discovery"})))
              "Subsequent physical discovery must still see the durable row"))
        (finally
          (Thread/interrupted))))))

(deftest interrupted-discovery-after-worker-lease-preserves-progress-and-signal-test
  ;; The worker may already have committed one lease before XTDB fails on
  ;; its second page. The failure cannot pretend the whole sweep ended or
  ;; roll back the committed prefix. A fresh scan must find the other row.
  (with-open [node (xtn/start-node {})]
    (let [a (java.util.UUID/fromString "00000000-0000-0000-0000-000000000001")
          b (java.util.UUID/fromString "00000000-0000-0000-0000-000000000002")
          _ (seed-intents! node [{:id a :status :pending}
                                 {:id b :status :pending}])
          cause (InterruptedException. "second discovery page interrupted")
          original-query xt/q
          pages (atom 0)
          opts {:source-id "gesso/test-discovery"
                :worker-id "interrupted-page-worker"
                :page-size 1 :max-pages 2}]
      (try
        (Thread/interrupted)
        (let [outcome (with-redefs
                        [xt/q (fn [& args]
                                (let [query (second args)]
                                  (if (and (= 3 (count args))
                                           (vector? query)
                                           (string? (first query))
                                           (.startsWith ^String (first query)
                                                        "SELECT _id")
                                           (= 2 (swap! pages inc)))
                                    (throw cause)
                                    (apply original-query args))))]
                        (try
                          (worker/acquire-sweep! node opts)
                          (catch Throwable error error)))
              interrupted? (.isInterrupted (Thread/currentThread))]
          (Thread/interrupted)
          (is (instance? clojure.lang.ExceptionInfo outcome)
              "Interrupted second page must throw a reconciliation envelope")
          (is (identical? cause (.getCause ^Throwable outcome))
              "The original physical interruption remains the direct cause")
          (is (= 1 (:completed-count (ex-data outcome)))
              "The first committed acquisition must remain visible")
          (is (= a (:last-completed-cursor (ex-data outcome)))
              "The cursor must name the last verified acquisition")
          (is (= a (get-in (ex-data outcome) [:completed 0 :intent-id])))
          (is (true? (:rescan-required? (ex-data outcome))))
          (is interrupted? "An interrupted paged sweep retains JVM cancellation")
          (is (= 2 @pages))
          (let [current (fn [id]
                          (first (xt/q node
                                       ["SELECT * FROM gesso_live_outbox WHERE _id = ?" id])))]
            (is (= :leased (:gesso.live.outbox/status (current a)))
                "First page acquisition is already committed")
            (is (= :pending (:gesso.live.outbox/status (current b)))
                "Second page must not be falsely acquired"))
          (let [rescan (worker/acquire-sweep! node opts)]
            (is (= 1 (:attempted rescan)))
            (is (= 1 (:acquired rescan)))
            (is (= b (-> rescan :results first :intent-id)))))
        (finally
          (Thread/interrupted))))))


(defn- synthetic-discovery-id [n]
  (java.util.UUID/fromString
   (format "00000000-0000-0000-0000-%012d" (long n))))

(defn- synthetic-discovery-row [id]
  {:xt/id id
   :outbox-status :pending
   :outbox-source-id "gesso/test-discovery"})

(deftest physical-discovery-rejects-out-of-order-and-backward-provider-rows-test
  ;; ORDER BY and WHERE _id > :after must be validated at the physical read
  ;; boundary, independently of the worker's later advisory-page checks.
  (let [[a b c] (mapv synthetic-discovery-id [401 402 403])
        opts {:source-id "gesso/test-discovery" :limit 2}
        cases [[:descending-visible opts [b a] :invalid-physical-order]
               [:descending-lookahead opts [a c b] :invalid-physical-order]
               [:behind-cursor (assoc opts :after b) [a] :invalid-physical-order]
               [:repeated-cursor (assoc opts :after b) [b] :invalid-physical-order]
               [:duplicate-lookahead opts [a b b] :duplicate-current-identities]]]
    (doseq [[label request ids reason] cases]
      (let [queries (atom 0)
            rows (mapv synthetic-discovery-row ids)]
        (with-redefs [xt/q (fn [& _] (swap! queries inc) rows)]
          (is (rejected? #(discovery/discover-eligible! :synthetic request)
                         reason)
              (str label " must not invent a safe discovery cursor"))
          (is (= 1 @queries) "A bad physical page must never be retried"))))))

(deftest physical-discovery-validates-lookahead-and-page-size-test
  ;; Lookahead determines :has-more? and therefore whether it is safe to
  ;; resume. A mismatched source, malformed status, or invalid identity at
  ;; that boundary cannot be dropped merely because it is not returned.
  (let [[a b c] (mapv synthetic-discovery-id [411 412 413])
        valid [(synthetic-discovery-row a) (synthetic-discovery-row b)]
        bad-lookaheads [[:wrong-source (assoc (synthetic-discovery-row c)
                                               :outbox-source-id "wrong/source")]
                        [:bad-status (assoc (synthetic-discovery-row c)
                                            :outbox-status :delivered)]
                        [:invalid-id (assoc (synthetic-discovery-row c)
                                           :xt/id "not-a-uuid")]]
        opts {:source-id "gesso/test-discovery" :limit 2}]
    (doseq [[label extra] bad-lookaheads]
      (with-redefs [xt/q (fn [& _] (conj valid extra))]
        (is (rejected? #(discovery/discover-eligible! :synthetic opts)
                       :invalid-physical-candidate)
            (str label " lookahead must fail before returning a cursor"))))
    (with-redefs [xt/q (fn [& _] (conj valid (synthetic-discovery-row c)
                                          (synthetic-discovery-row
                                           (synthetic-discovery-id 414))))]
      (is (rejected? #(discovery/discover-eligible! :synthetic opts)
                     :oversized-physical-page)
          "XTDB must never return more than the requested limit + 1"))))

(deftest physical-discovery-accepts-sql-uuid-order-across-sign-boundary-test
  ;; Canonical unsigned XTDB UUID ordering differs from Java UUID.compareTo.
  ;; The adapter must accept both a full page and a continuation crossing it.
  (let [a (java.util.UUID/fromString "7fffffff-ffff-ffff-ffff-ffffffffffff")
        b (java.util.UUID/fromString "80000000-0000-0000-0000-000000000000")
        opts {:source-id "gesso/test-discovery" :limit 1}
        page (with-redefs [xt/q (fn [& _] [(synthetic-discovery-row a)
                                           (synthetic-discovery-row b)])]
               (discovery/discover-eligible! :synthetic opts))
        resumed (with-redefs [xt/q (fn [& _] [(synthetic-discovery-row b)])]
                  (discovery/discover-eligible! :synthetic
                                               (assoc opts :after a)))]
    (is (= [{:intent-id a :action :acquire-pending}] (:candidates page)))
    (is (true? (:has-more? page)))
    (is (= a (:next-after page)))
    (is (= [{:intent-id b :action :acquire-pending}]
           (:candidates resumed)))
    (is (false? (:has-more? resumed)))
    (is (nil? (:next-after resumed)))))
