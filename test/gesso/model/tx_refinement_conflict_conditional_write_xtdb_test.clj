(ns gesso.model.tx-refinement-conflict-conditional-write-xtdb-test
  "Permanent, bounded XTDB 2.2.0-beta1 conflict experiments from v1404.

   Each race releases separate JVM workers through a start gate and checks the
   resulting authoritative documents and committed rows in xt.txs. Canonical
   guarded writes are lowered by Gesso; deliberate blind puts bypass those
   guards to demonstrate that XTDB does not reject all same-document writes.

   This is *empirical* single-node evidence only. A public transaction ID and
   later ledger row do not authenticate the submitted payload, attribute an
   attempt to its commit, establish an internal read basis, or prove universal
   serializability. No proof obligation or admission authority is discharged."
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

(defn- with-node [f]
  (with-open [db (xtn/start-node {})]
    (reset! !node db)
    (try (f)
         (finally (reset! !node nil)))))

(use-fixtures :once with-node)

(defn- node []
  (or @!node (throw (ex-info "Embedded XTDB node was not started" {}))))

(defn- unique-id [kind]
  (str "v1405-" kind "-" (random-uuid)))

(defn- request-doc [id owner]
  {:xt/id id :revision 0 :created-at "t0" :updated-at "t0" :owner owner})

(defn- member-doc [id]
  {:xt/id id :revision 0 :created-at "t0" :updated-at "t0" :active true})

(defn- advance [before changes]
  (-> before (merge changes) (update :revision inc) (assoc :updated-at "t1")))

(defn- claim [before owner]
  (command/update-command :requests :claim before
                          (advance before {:owner owner}) version))

(defn- revoke [before]
  (command/update-command :memberships :revoke before
                          (advance before {:active false}) version))

(defn- canonical-ops [commands guards]
  (:tx-ops (tx/prepare {} {:commands (vec commands)
                           :guards (vec guards)
                           :emit false})))

(defn- seed! [& typed-documents]
  (xt/execute-tx (node)
                 (mapv (fn [[table doc]] [:put-docs table doc])
                       typed-documents)))

(defn- query-one [sql id]
  (let [rows (xt/q (node) [sql id])]
    (is (<= (count rows) 1) (str "Multiple visible records for " id))
    (first rows)))

(defn- request-state [id]
  (query-one
   "SELECT _id, revision, created_at, updated_at, owner FROM requests WHERE _id = ?"
   id))

(defn- member-state [id]
  (query-one
   "SELECT _id, revision, created_at, updated_at, active FROM memberships WHERE _id = ?"
   id))

(defn- execute-result! [ops]
  (try
    (let [key (xt/execute-tx (node) ops)]
      {:status :committed
       :tx-id (.getTxId ^xtdb.api.TransactionKey key)})
    (catch Throwable error
      (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data error)))
        {:status :rejected :error/code :xtdb/assert-failed}
        (throw error)))))

(defn- overlapping! [operations]
  (let [ready (CountDownLatch. (count operations))
        release (CountDownLatch. 1)
        workers (mapv (fn [ops]
                        (future
                          (.countDown ready)
                          (when-not (.await release 30 TimeUnit/SECONDS)
                            (throw (ex-info "XTDB race release timed out" {})))
                          (execute-result! ops)))
                      operations)]
    (try
      (when-not (.await ready 30 TimeUnit/SECONDS)
        (throw (ex-info "XTDB workers did not reach the release gate"
                        {:pending (.getCount ready)})))
      (.countDown release)
      (let [results (mapv #(deref % 60000 ::timeout) workers)]
        (when (some #{::timeout} results)
          (throw (ex-info "XTDB race did not settle" {:results results})))
        results)
      (finally
        (.countDown release)
        (doseq [worker workers]
          (when-not (realized? worker)
            (future-cancel worker)))))))

(defn- check-ledger! [results]
  (let [committed (filterv #(= :committed (:status %)) results)
        tx-ids (mapv :tx-id committed)]
    (is (= (count tx-ids) (count (distinct tx-ids)))
        "Each observed successful execute-tx should return a distinct ID")
    (doseq [tx-id tx-ids]
      (is (and (integer? tx-id) (<= 0 tx-id))
          "A returned transaction ID must be a nonnegative integer")
      (let [rows (xt/q (node)
                       ["SELECT _id, committed FROM xt.txs WHERE _id = ?" tx-id])]
        (is (= 1 (count rows))
            (str "Expected exactly one xt.txs row for returned ID " tx-id))
        (is (= {:xt/id tx-id :committed true} (first rows))
            "The returned execute-tx ID must have a committed ledger row")))
    (doseq [{:keys [status error/code tx-id]} results]
      (is (or (= :committed status)
              (and (= :rejected status) (= :xtdb/assert-failed code)))
          "Only a returned key or the specific ASSERT rejection is admitted")
      (when (= :rejected status)
        (is (nil? tx-id)
            "A rejected call does not invent a committed transaction ID")))))

(defn- serial-state [targets]
  (into {}
        (keep (fn [[table id :as target]]
                (when-let [document (case table
                                      :requests (request-state id)
                                      :memberships (member-state id))]
                  [target document])))
        targets))

(defn- assert-some-serial-path! [initial plans targets results]
  (let [possible (set
                  (map (fn [{:keys [trace final-state]}]
                         {:statuses (reduce (fn [statuses {:keys [plan-index status]}]
                                              (assoc statuses plan-index status))
                                            (vec (repeat (count plans) nil))
                                            trace)
                          :state final-state})
                       (:paths (model/explore-orders initial plans))))
        actual {:statuses (mapv :status results)
                :state (serial-state targets)}]
    (is (contains? possible actual)
        (str "No admitted serial model path explains the bounded observation: "
             (pr-str {:actual actual :possible possible})))
    actual))

(deftest repeated-same-version-guarded-claims-have-one-winner-test
  (dotimes [round 6]
    (testing (str "version-guarded conflict, round " round)
      (let [id (unique-id "guarded")
            before (request-doc id "unclaimed")
            _ (seed! [:requests before])
            plans [{:commands [(claim before "helper-a")]
                    :emit false}
                   {:commands [(claim before "helper-b")]
                    :emit false}]
            operations (mapv (fn [{:keys [commands]}]
                               (canonical-ops commands [])) plans)
            _ (is (every? #(= 2 (count %)) operations)
                  "Canonical update has an ASSERT followed by a put")
            results (overlapping! operations)
            statuses (mapv :status results)
            actual (assert-some-serial-path!
                    {[:requests id] before} plans [[:requests id]] results)]
        (check-ledger! results)
        (is (= #{:committed :rejected} (set statuses)))
        (is (= 1 (count (filter #{:committed} statuses))))
        (is (= (if (= :committed (first statuses)) "helper-a" "helper-b")
               (get-in actual [:state [:requests id] :owner])))
        (is (= 1 (get-in actual [:state [:requests id] :revision])))))))

(deftest repeated-blind-same-target-writes-both-commit-test
  (dotimes [round 6]
    (testing (str "unguarded same-target puts, round " round)
      (let [id (unique-id "blind")
            original (request-doc id "unclaimed")
            a (assoc original :owner "helper-a")
            b (assoc original :owner "helper-b")
            _ (seed! [:requests original])
            results (overlapping! [[[:put-docs :requests a]]
                                   [[:put-docs :requests b]]])
            actual (request-state id)]
        (check-ledger! results)
        (is (= [:committed :committed] (mapv :status results))
            "XTDB need not reject either write when neither has an ASSERT")
        (is (contains? #{a b} actual)
            "Final whole document must be one submitted value, not a hybrid")
        (is (= 0 (:revision actual))
            "Blind writes do not enforce or automatically increment revision")))))

(deftest multi-document-contention-is-all-or-none-test
  (dotimes [round 5]
    (testing (str "two-target guarded conflict, round " round)
      (let [id (unique-id "multi-existing")
            created-id (unique-id "multi-created")
            member-id (unique-id "multi-member")
            request (request-doc id "unclaimed")
            member (member-doc member-id)
            created (request-doc created-id "additional")
            initial {[:requests id] request
                     [:memberships member-id] member}
            _ (seed! [:requests request] [:memberships member])
            guard (command/guard :memberships member version)
            plans [{:commands [(claim request "helper-a")
                               (command/create :requests created version)]
                    :guards [guard] :emit false}
                   {:commands [(claim request "helper-b")]
                    :guards [guard] :emit false}]
            operations (mapv (fn [p]
                               (canonical-ops (:commands p) (:guards p))) plans)
            _ (is (= [5 3] (mapv count operations))
                  "Multi-target candidate has 3 ASSERTs and 2 puts; single-target has 2 ASSERTs and 1 put")
            results (overlapping! operations)
            actual (assert-some-serial-path!
                    initial plans [[:requests id]
                                   [:memberships member-id]
                                   [:requests created-id]] results)
            extra (get-in actual [:state [:requests created-id]])
            multi-committed? (= :committed (:status (first results)))]
        (check-ledger! results)
        (is (= #{:committed :rejected} (set (map :status results))))
        (is (= multi-committed? (some? extra))
            "The new document is visible iff the entire multi-write committed")
        (is (= (when multi-committed? created) extra))
        (is (= (if multi-committed? "helper-a" "helper-b")
               (get-in actual [:state [:requests id] :owner])))
        (is (= member (get-in actual [:state [:memberships member-id]])))))))

(deftest guard-vs-membership-revocation-obeys-a-serial-reference-test
  (dotimes [round 5]
    (testing (str "claim guard versus revocation, round " round)
      (let [id (unique-id "revoke-request")
            member-id (unique-id "revoke-member")
            request (request-doc id "unclaimed")
            member (member-doc member-id)
            initial {[:requests id] request [:memberships member-id] member}
            _ (seed! [:requests request] [:memberships member])
            plans [{:commands [(claim request "helper-a")]
                    :guards [(command/guard :memberships member version)]
                    :emit false}
                   {:commands [(revoke member)] :emit false}]
            results (overlapping!
                     (mapv (fn [p]
                             (canonical-ops (:commands p) (:guards p))) plans))
            actual (assert-some-serial-path!
                    initial plans [[:requests id] [:memberships member-id]] results)]
        (check-ledger! results)
        (is (= :committed (:status (second results)))
            "The revocation itself always has a current version guard")
        (is (false? (get-in actual [:state [:memberships member-id] :active])))
        (is (= (if (= :committed (:status (first results)))
                 "helper-a" "unclaimed")
               (get-in actual [:state [:requests id] :owner])))))))
