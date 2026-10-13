(ns gesso.model.tx-model-xtdb-concurrency-test
  "Bounded correspondence tests with *overlapping* real XTDB submissions.

   Each scenario prepares canonical Gesso model transaction plans from the
   SAME initial document snapshot, then releases their real XTDB execute-tx
   calls from separate JVM workers at a common gate. After all calls settle,
   their per-plan commit/rejection results and final persisted documents must
   match at least one serial path enumerated by gesso.model.tx-model.

   This tests concrete executions against a finite serial-reference oracle;
   it does not assume a particular winning thread or try to infer an execution
   order from wall-clock timing. Only :xtdb/assert-failed is an admitted
   rejection; other errors and timeouts fail the tests.

   LIMITS: finite schedules actually exercised, no universal isolation or
   linearizability proof, no cross-node concurrency, no durable command replay,
   no domain-authorization completeness, and no arbitrary SQL support.
   In particular, all Claim guards here are test assumptions, NOT a verified
   HumanHelp policy. No certification claim follows from these tests."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.model.command :as command]
   [gesso.model.tx :as tx]
   [gesso.model.tx-model :as model]
   [xtdb.api :as xt]
   [xtdb.node :as xtn])
  (:import
   [java.util.concurrent CountDownLatch TimeUnit]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(defonce ^:private !node (atom nil))

(defn- node []
  (or @!node
      (throw (ex-info "Real XTDB fixture was not started." {}))))

(defn- with-real-xtdb [f]
  (with-open [database (xtn/start-node {})]
    (reset! !node database)
    (try
      (f)
      (finally (reset! !node nil)))))

(use-fixtures :once with-real-xtdb)

(defn- request-document [id]
  {:xt/id id
   :revision 0
   :created-at "t0"
   :updated-at "t0"
   :owner "unclaimed"})

(defn- membership [id]
  {:xt/id id
   :revision 0
   :created-at "t0"
   :updated-at "t0"
   :active true})

(defn- updated [before changes]
  (-> before
      (merge changes)
      (update :revision inc)
      (assoc :updated-at "t1")))

(defn- claim [before owner]
  (command/update-command
   :requests :claim before (updated before {:owner owner}) version))

(defn- revoke [before]
  (command/update-command
   :memberships :revoke before (updated before {:active false}) version))

(defn- plan [commands & guards]
  {:commands (vec commands)
   :guards (vec guards)
   :emit false})

(defn- seed! [documents]
  (when (seq documents)
    (xt/execute-tx
     (node)
     (mapv (fn [[entity-type document]]
             [:put-docs entity-type document])
           documents))))

(defn- fixture []
  (let [suffix (str (random-uuid))
        request (request-document (str "tx-race-request-" suffix))
        membership (membership (str "tx-race-member-" suffix))
        state {[:requests (:xt/id request)] request
               [:memberships (:xt/id membership)] membership}]
    (seed! [[:requests request] [:memberships membership]])
    {:request request
     :membership membership
     :state state}))

(defn- select-document [table id]
  ;; The same narrowly admitted document projections used by v787.
  (let [sql (case table
              :requests
              "SELECT _id, revision, created_at, updated_at, owner FROM requests WHERE _id = ?"
              :memberships
              "SELECT _id, revision, created_at, updated_at, active FROM memberships WHERE _id = ?")
        rows (xt/q (node) [sql id])]
    (when (> (count rows) 1)
      (throw (ex-info "Multiple current documents for one ID."
                      {:target [table id] :rows rows})))
    (first rows)))

(defn- persisted-state [targets]
  (into {}
        (keep (fn [[table id :as target]]
                (when-let [document (select-document table id)]
                  [target document])))
        targets))

(defn- execute-and-classify [operations]
  (try
    {:status :committed
     :tx-key (xt/execute-tx (node) operations)}
    (catch Throwable error
      (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data error)))
        {:status :rejected
         :error/code :xtdb/assert-failed}
        (throw error)))))

(defn- execute-overlapping!
  "Gate worker entry before releasing prepared ops to XTDB. These calls may
   still serialize internally. A worker timeout is a hard failure, never a
   model-matching rejection."
  [plans]
  (let [operations (mapv (fn [a-plan]
                           ;; Actual production lowering; do not synthesize SQL.
                           (:tx-ops (tx/prepare {} a-plan)))
                         plans)
        ready (CountDownLatch. (count plans))
        start (CountDownLatch. 1)
        workers (mapv (fn [ops]
                        (future
                          (.countDown ready)
                          (when-not (.await start 30 TimeUnit/SECONDS)
                            (throw (ex-info "XTDB race start gate timed out." {})))
                          (execute-and-classify ops)))
                      operations)]
    (try
      (when-not (.await ready 30 TimeUnit/SECONDS)
        (throw (ex-info "XTDB race workers did not reach start gate."
                        {:expected (count plans)
                         :remaining (.getCount ready)})))
      (.countDown start)
      (let [results (mapv #(deref % 60000 ::timeout) workers)]
        (when (some #{::timeout} results)
          (throw (ex-info "Concurrent XTDB transaction timed out."
                          {:results results})))
        results)
      (finally
        (.countDown start)
        (doseq [worker workers]
          (when-not (realized? worker)
            (future-cancel worker)))))))

(defn- path-outcome [path plan-count]
  {:statuses (reduce (fn [statuses {:keys [plan-index status]}]
                       (assoc statuses plan-index status))
                     (vec (repeat plan-count nil))
                     (:trace path))
   :state (:final-state path)})

(defn- compare-race!
  "Reject the run unless BOTH the per-plan results AND persisted state agree
   with one complete serial-reference path. The final-state read also includes
   absent create targets, to detect partially applied writes."
  [initial-state plans targets]
  (let [exploration (model/explore-orders initial-state plans)
        possible (set (map #(path-outcome % (count plans))
                           (:paths exploration)))
        results (execute-overlapping! plans)
        actual {:statuses (mapv :status results)
                :state (persisted-state targets)}]
    (is (contains? possible actual)
        (str "Concurrent execution is not represented by any admitted serial "
             "path: " (pr-str {:actual actual :possible possible})))
    (is (every? (fn [{:keys [status error/code]}]
                  (or (= :committed status)
                      (and (= :rejected status)
                           (= :xtdb/assert-failed code))))
                results)
        "Only actual successful transactions or typed ASSERT rejections count.")
    {:actual actual
     :results results
     :possible possible}))

(deftest competing-guarded-claims-have-only-one-winner-test
  (dotimes [round 6]
    (testing (str "concurrent same-revision Claims, round " round)
      (let [{:keys [request membership state]} (fixture)
            guard (command/guard :memberships membership version)
            plans [(plan [(claim request "helper-a")] guard)
                   (plan [(claim request "helper-b")] guard)]
            {:keys [actual]} (compare-race! state plans (keys state))
            statuses (:statuses actual)
            owner (get-in actual [:state [:requests (:xt/id request)] :owner])]
        (is (= #{:committed :rejected} (set statuses)))
        (is (= 1 (count (filter #{:committed} statuses))))
        (is (= (if (= :committed (first statuses)) "helper-a" "helper-b")
               owner))
        (is (= 1 (get-in actual [:state [:requests (:xt/id request)] :revision])))))))

(deftest claim-races-membership-revocation-test
  (dotimes [round 6]
    (testing (str "Claim versus membership revocation, round " round)
      (let [{:keys [request membership state]} (fixture)
            plans [(plan [(claim request "helper-a")]
                         (command/guard :memberships membership version))
                   (plan [(revoke membership)])]
            {:keys [actual]} (compare-race! state plans (keys state))
            claim-status (first (:statuses actual))
            final-state (:state actual)]
        (is (= :committed (second (:statuses actual)))
            "The membership revocation is independent of the request update.")
        (is (false? (get-in final-state
                            [[:memberships (:xt/id membership)] :active])))
        (is (= (if (= :committed claim-status) "helper-a" "unclaimed")
               (get-in final-state [[:requests (:xt/id request)] :owner])))))))

(deftest disjoint-claims-can-both-commit-test
  (dotimes [round 4]
    (testing (str "distinct request IDs, round " round)
      (let [{:keys [request membership state]} (fixture)
            other (request-document (str "tx-race-other-" (random-uuid)))
            state (assoc state [:requests (:xt/id other)] other)
            _ (seed! [[:requests other]])
            guard (command/guard :memberships membership version)
            plans [(plan [(claim request "helper-a")] guard)
                   (plan [(claim other "helper-b")] guard)]
            {:keys [actual]} (compare-race! state plans (keys state))]
        (is (= [:committed :committed] (:statuses actual)))
        (is (= "helper-a"
               (get-in actual [:state [:requests (:xt/id request)] :owner])))
        (is (= "helper-b"
               (get-in actual [:state [:requests (:xt/id other)] :owner])))))))

(deftest competing-creations-admit-one-insert-test
  (dotimes [round 4]
    (testing (str "same absent document, round " round)
      (let [created-a (request-document (str "tx-race-create-" (random-uuid)))
            created-b (assoc created-a :owner "other-helper")
            target [:requests (:xt/id created-a)]
            plans [(plan [(command/create :requests created-a version)])
                   (plan [(command/create :requests created-b version)])]
            {:keys [actual]} (compare-race! {} plans [target])]
        (is (= #{:committed :rejected} (set (:statuses actual))))
        (is (= (if (= :committed (first (:statuses actual)))
                 created-a created-b)
               (get-in actual [:state target])))))))

(deftest rejected-multi-write-remains-atomic-under-contention-test
  (dotimes [round 5]
    (testing (str "multi-write versus competing Claim, round " round)
      (let [{:keys [request membership state]} (fixture)
            created (request-document (str "tx-race-created-" (random-uuid)))
            new-target [:requests (:xt/id created)]
            guard (command/guard :memberships membership version)
            plans [(plan [(claim request "helper-a")
                          (command/create :requests created version)]
                         guard)
                   (plan [(claim request "helper-b")] guard)]
            {:keys [actual]} (compare-race! state plans
                                            (conj (vec (keys state)) new-target))
            multi-committed? (= :committed (first (:statuses actual)))
            final-state (:state actual)]
        (is (= #{:committed :rejected} (set (:statuses actual))))
        (is (= multi-committed?
               (contains? final-state new-target))
            "The extra create must commit iff its whole multi-write wins.")
        (is (= (if multi-committed? created nil)
               (get final-state new-target)))
        (is (= (if multi-committed? "helper-a" "helper-b")
               (get-in final-state [[:requests (:xt/id request)] :owner])))))))

(deftest three-way-contention-matches-some-serial-order-test
  (dotimes [round 4]
    (testing (str "two Claims and one revocation, round " round)
      (let [{:keys [request membership state]} (fixture)
            guard (command/guard :memberships membership version)
            plans [(plan [(claim request "helper-a")] guard)
                   (plan [(claim request "helper-b")] guard)
                   (plan [(revoke membership)])]
            {:keys [actual]} (compare-race! state plans (keys state))
            statuses (:statuses actual)
            final-state (:state actual)]
        (is (= :committed (nth statuses 2)))
        (is (<= (count (filter #{:committed} (subvec statuses 0 2))) 1))
        (is (false? (get-in final-state
                            [[:memberships (:xt/id membership)] :active])))))))
