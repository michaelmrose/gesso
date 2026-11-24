(ns gesso.live.outbox.recovery-audit-test
  "Physical cold-host debt inventory independent of in-memory cycle history."
  (:require [clojure.test :refer [deftest is]]
            [gesso.live.outbox :as outbox]
            [gesso.live.consistency.xtdb :as live.xtdb]
            [gesso.live.outbox.read-frontier :as frontier]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.acknowledgment :as acknowledgment]
            [gesso.live.outbox.recovery-audit :as audit]
            [gesso.live.outbox.discovery :as discovery]
            [xtdb.api :as xt]
            [xtdb.basis :as basis]
            [xtdb.node :as xtn])
  (:import [java.time Instant]))

(def source-id "gesso/cold-host-audit")
(def other-source "gesso/another-source")

(defn- id [n]
  (java.util.UUID/fromString (format "00000000-0000-0000-0000-%012d" n)))

(defn- seed! [node intent source]
  (let [p (outbox/prepare!
           {:intent-ids [intent] :source-id source
            :operation :request/claim :purpose :gesso.live/refresh}
           [{:topic :request :id (str "request-" intent)}])]
    (xt/execute-tx node (conj (vec (:guard-ops p)) (:tx-op p)))))

(defn- acquire! [node intent worker millis]
  (let [now (Instant/now)
        result (acquisition/acquire-pending!
                node {:intent-id intent :source-id source-id
                      :worker-id worker :lease-id (random-uuid)
                      :now now :lease-until (.plusMillis now millis)})]
    (is (= :acquired (:status result)))
    result))

(defn- reason [node opts]
  (try (audit/inspect-source-page! node opts) nil
       (catch clojure.lang.ExceptionInfo error
         (when (= :gesso.live.outbox/invalid-recovery-audit
                  (:error/type (ex-data error)))
           (:reason (ex-data error))))))

(deftest cold-host-audit-covers-all-durable-states-without-replay-authority-test
  (with-open [node (xtn/start-node {})]
    (let [[p a e t foreign] (map id [1 2 3 4 5])]
      (doseq [intent [p a e t]] (seed! node intent source-id))
      (seed! node foreign other-source)
      (let [active (acquire! node a "active-worker" 60000)
            expired (acquire! node e "crashed-worker" 800)
            terminal (acquire! node t "terminal-worker" 60000)
            receipt (random-uuid)]
        (is (= :acknowledged
               (:status (acknowledgment/acknowledge-dispatch!
                         node {:intent-id t :lease-id (:lease-id terminal)
                               :worker-id "terminal-worker"
                               :receipt {:receipt-id receipt
                                         :adapter :gesso.live/synchronous-source
                                         :result :accepted}}))))
        (Thread/sleep 1200)
        (let [page1 (audit/inspect-source-page!
                     node {:source-id source-id :limit 2})
              page2 (audit/inspect-source-page!
                     node {:source-id source-id :limit 2
                           :after (:next-after page1)})
              entries (into (:entries page1) (:entries page2))]
          (is (= [p a e t] (mapv :intent-id entries)))
          (is (= [:pending-unclaimed :active-lease-at-read
                  :expired-lease-at-read :acknowledgment-recorded]
                 (mapv :observation entries)))
          (is (= [nil false true nil]
                 (mapv :lease-expired-at-read? entries)))
          (is (= 2 (count (:entries page1))))
          (is (= a (:next-after page1)))
          (is (:has-more? page1))
          (is (false? (:has-more? page2)))
          (is (nil? (:next-after page2)))
          (is (:rescan-required? page1))
          (is (= source-id (:source-id page2)))
          (is (= (:lease-id active) (:lease-id (second entries))))
          (is (= "crashed-worker" (:lease-owner (nth entries 2))))
          (is (= receipt (:receipt-id (last entries))))
          (is (every? :observed-system-from entries))
          (is (every? #(not (contains? % :may-replay?)) entries))
          (is (= [p e]
                 (mapv :intent-id
                       (:candidates (discovery/discover-eligible!
                                     node {:source-id source-id}))))))))))

(deftest cold-host-audit-remains-advisory-after-guarded-reclaim-test
  (with-open [node (xtn/start-node {})]
    (let [intent (id 60)]
      (seed! node intent source-id)
      (let [old (acquire! node intent "lost-host" 650)]
        (Thread/sleep 1000)
        (let [prior (-> (audit/inspect-source-page!
                         node {:source-id source-id}) :entries first)
              now (Instant/now)
              next-lease (random-uuid)
              replacement (acquisition/reclaim-expired!
                           node {:intent-id intent :source-id source-id
                                 :worker-id "new-host" :lease-id next-lease
                                 :now now :lease-until (.plusSeconds now 30)})]
          (is (= :expired-lease-at-read (:observation prior)))
          (is (= :reclaimed (:status replacement)))
          (is (not= (:lease-id old) next-lease))
          (let [after (-> (audit/inspect-source-page!
                           node {:source-id source-id}) :entries first)]
            (is (= :active-lease-at-read (:observation after)))
            (is (= "new-host" (:lease-owner after)))
            (is (= next-lease (:lease-id after)))
            (is (= (:intent-id prior) (:intent-id after)))))))))

(deftest cold-host-audit-rejects-invalid-requests-before-physical-reads-test
  (let [base {:source-id source-id}]
    (doseq [[opts expected]
            [[nil :invalid-options]
             [{} :invalid-source-id]
             [(assoc base :source-id " ") :invalid-source-id]
             [(assoc base :limit 0) :invalid-limit]
             [(assoc base :limit 129) :invalid-limit]
             [(assoc base :after nil) :invalid-cursor]
             [(assoc base :after "bad") :invalid-cursor]
             [(assoc base :extra true) :unknown-fields]]]
      (is (= expected (reason :no-node opts))))))

(deftest cold-host-audit-rejects-malformed-provider-pages-including-lookahead-test
  (with-open [node (xtn/start-node {})]
    (let [first-id (id 30)
          second-id (id 31)]
      (seed! node first-id source-id)
      (seed! node second-id source-id)
      (let [valid (xt/q node ["SELECT *, _system_from FROM gesso_live_outbox WHERE _id = ?" first-id])
            other (xt/q node ["SELECT *, _system_from FROM gesso_live_outbox WHERE _id = ?" second-id])
            p {:source-id source-id :limit 1}]
        (with-redefs [live.xtdb/q-from (fn [_ _ _] (into valid [(assoc (first other)
                                                   :gesso.live.outbox/source-id other-source)]))]
          (is (= :invalid-physical-intent (reason node p))))
        (with-redefs [live.xtdb/q-from (fn [_ _ _] (into valid valid))]
          (is (= :invalid-physical-order (reason node p))))
        (with-redefs [live.xtdb/q-from (fn [_ _ _] nil)]
          (is (= :invalid-physical-page (reason node p))))
        (with-redefs [live.xtdb/q-from (fn [_ _ _] (vec (repeat 3 (first valid))))]
          (is (= :oversized-physical-page (reason node p))))
        (let [physical (first valid)]
          (xt/execute-tx node [[:put-docs outbox/intent-table
                                (assoc (dissoc physical :xt/system-from) :gesso.live.outbox/status :leased)]])
          (is (= :invalid-physical-status
                 (reason node {:source-id source-id}))))))))

(deftest audit-awaits-a-fresh-selected-node-frontier-for-each-page-test
  (let [node :selected-node
        cursor (id 80)
        calls (atom [])
        frontier-id (atom 40)
        opts {:source-id source-id :limit 1}]
    (with-redefs [xt/status (fn [n]
                             (swap! calls conj [:status n])
                             {:latest-submitted-msg-ids {"xtdb" [(swap! frontier-id inc)]}})
                  live.xtdb/q-from (fn [n query actual]
                                     (swap! calls conj [:query n query actual])
                                     [])]
      (is (empty? (:entries (audit/inspect-source-page! node opts))))
      (is (empty? (:entries (audit/inspect-source-page! node (assoc opts :after cursor)))))
      (let [[s1 q1 s2 q2] @calls]
        (is (= [[:status node] [:status node]] [s1 s2]))
        (is (= [[:query node] [:query node]] (mapv #(subvec % 0 2) [q1 q2])))
        (is (= [{:await-token (basis/->tx-basis-str {"xtdb" [41]})}
                {:await-token (basis/->tx-basis-str {"xtdb" [42]})}]
               (mapv last [q1 q2])))
        (is (= cursor (last (nth q2 2))))))
    (with-redefs [xt/status (constantly {:latest-submitted-msg-ids {}})
                  live.xtdb/q-from (fn [& _] (throw (AssertionError. "Invalid frontier queried")))]
      (is (= ::frontier/invalid-frontier
             (try (audit/inspect-source-page! node opts)
                  nil (catch clojure.lang.ExceptionInfo e (:error/type (ex-data e)))))))
    (let [cause (InterruptedException. "audit interrupted")]
      (try
        (with-redefs [xt/status (constantly {:latest-submitted-msg-ids {"xtdb" [43]}})
                      live.xtdb/q-from (fn [& _] (throw cause))]
          (is (= [cause true]
                 (try (audit/inspect-source-page! node opts)
                      nil (catch Exception e [e (.isInterrupted (Thread/currentThread))])))))
        (finally (Thread/interrupted))))))

(deftest audit-sees-a-submitted-intent-without-a-separate-client-await-test
  (with-open [node (xtn/start-node {})]
    (let [intent (id 90)
          plan (outbox/prepare!
                {:intent-ids [intent] :source-id source-id
                 :operation :request/claim :purpose :gesso.live/refresh}
                [{:topic :request :id "submitted-request"}])]
      (xt/submit-tx node (conj (vec (:guard-ops plan)) (:tx-op plan)))
      (let [page (audit/inspect-source-page! node {:source-id source-id})]
        (is (= [intent] (mapv :intent-id (:entries page))))
        (is (= [:pending-unclaimed] (mapv :observation (:entries page))))
        (is (true? (:rescan-required? page)))
        (is (not (contains? (first (:entries page)) :may-replay?)))))))
