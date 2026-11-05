(ns gesso.model.tx-refinement-concurrent-serial-correspondence-test
  "Permanent bounded single-node contention and serial-correspondence tests.

   These tests use the v1406 controlled release-wave fixture with an embedded
   XTDB node and fresh documents for every schedule. They check canonical
   Gesso guarded Claims, Claim versus membership revocation, and an atomic
   multi-document conditional transaction. Completed-wave barriers constrain
   admissible serial histories; concurrently released calls may serialize in
   either order.

   The returned transaction keys and later xt.txs rows are corroborating
   observations, not authenticated payload receipts or causal witnesses.
   Neither bounded testing nor a matching serial path discharges physical
   refinement, complete writer mediation, or isolation proof obligations."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.model.command :as command]
   [gesso.model.tx-model :as model]
   [gesso.model.tx-refinement-concurrent-replay-fixture :as replay]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(def ^:private schedules
  [[[0 1]] [[0] [1]] [[1] [0]]])

(defonce ^:private !node (atom nil))

(defn- with-node [run]
  (with-open [db (xtn/start-node {})]
    (reset! !node db)
    (try (run)
         (finally (reset! !node nil)))))

(use-fixtures :once with-node)

(defn- node []
  (or @!node (throw (ex-info "Embedded XTDB node has not started" {}))))

(defn- document-id [label]
  (str "v1407-" label "-" (random-uuid)))

(defn- request-document [id owner]
  {:xt/id id :revision 0 :created-at "t0" :updated-at "t0"
   :owner owner})

(defn- member-document [id]
  {:xt/id id :revision 0 :created-at "t0" :updated-at "t0"
   :active true})

(defn- advance [before changes]
  (-> before (merge changes) (update :revision inc)
      (assoc :updated-at "t1")))

(defn- claim [before owner]
  (command/update-command :requests :claim before
                          (advance before {:owner owner}) version))

(defn- revoke [before]
  (command/update-command :memberships :revoke before
                          (advance before {:active false}) version))

(defn- seed! [typed-documents]
  (xt/execute-tx (node)
                 (mapv (fn [[table document]]
                         [:put-docs table document])
                       typed-documents)))

(defn- read-document [table id]
  (let [sql (case table
              :requests
              "SELECT _id, revision, created_at, updated_at, owner FROM requests WHERE _id = ?"
              :memberships
              "SELECT _id, revision, created_at, updated_at, active FROM memberships WHERE _id = ?")
        rows (xt/q (node) [sql id])]
    (when-not (<= (count rows) 1)
      (throw (ex-info "Nonunique physical readback" {:table table :id id})))
    (first rows)))

(defn- read-targets [targets]
  (into {}
        (keep (fn [[table id :as target]]
                (when-let [document (read-document table id)]
                  [target document])))
        targets))

(defn- scenario [kind _run-index _waves]
  (let [request-id (document-id (name kind))
        member-id (document-id "membership")
        created-id (document-id "created")
        request (request-document request-id "unclaimed")
        member (member-document member-id)
        created (request-document created-id "additional")
        guarded (command/guard :memberships member version)
        [seeded plans targets]
        (case kind
          :competing-claims
          [[[:requests request]]
           [{:commands [(claim request "helper-a")] :emit false}
            {:commands [(claim request "helper-b")] :emit false}]
           [[:requests request-id]]]

          :claim-and-revocation
          [[[:requests request] [:memberships member]]
           [{:commands [(claim request "helper-a")]
             :guards [guarded] :emit false}
            {:commands [(revoke member)] :emit false}]
           [[:requests request-id] [:memberships member-id]]]

          :atomic-multi-document
          [[[:requests request] [:memberships member]]
           [{:commands [(claim request "helper-a")
                        (command/create :requests created version)]
             :guards [guarded] :emit false}
            {:commands [(claim request "helper-b")]
             :guards [guarded] :emit false}]
           [[:requests request-id]
            [:memberships member-id]
            [:requests created-id]]])
        initial (into {}
                      (map (fn [[table document]]
                             [[table (:xt/id document)] document]))
                      seeded)]
    (seed! seeded)
    {:initial-state initial
     :plans plans
     :targets targets
     :submit! (replay/xtdb-submitter (node))
     :readback! read-targets}))

(defn- status-vector [report]
  (mapv :status (:observed-per-plan report)))

(defn- committed-tx-ids [report]
  (mapv (fn [{:keys [tx-key]}]
          (is (instance? xtdb.api.TransactionKey tx-key))
          (.getTxId ^xtdb.api.TransactionKey tx-key))
        (filterv #(= :committed (:status %))
                 (:observed-per-plan report))))

(defn- assert-ledger! [report]
  (let [tx-ids (committed-tx-ids report)]
    (is (= (count tx-ids) (count (distinct tx-ids)))
        "Distinct returned commit keys must not repeat")
    (doseq [tx-id tx-ids]
      (is (and (integer? tx-id) (<= 0 tx-id)))
      (is (= [{:xt/id tx-id :committed true}]
             (xt/q (node)
                   ["SELECT _id, committed FROM xt.txs WHERE _id = ?" tx-id]))
          "Returned committed transaction keys have a visible committed ledger row"))
    (doseq [result (:observed-per-plan report)
            :when (= :rejected (:status result))]
      (is (= #{:status :error/code} (set (keys result))))
      (is (= :xtdb/assert-failed (:error/code result))))))

(defn- assert-report! [report waves]
  (is (= waves (:release-waves report)))
  (is (:matching-serial-path? report))
  (is (pos? (:eligible-serial-outcomes report)))
  (is (= 2 (:plan-count report)))
  (is (= 2 (count (:observed-per-plan report))))
  (is (= #{:committed :rejected} (set (status-vector report)))
      "A successful guarded contender excludes its stale competitor")
  (is (= :bounded-single-node-reference-comparison (:scope report)))
  (is (= :caller-supplied (:readback-origin report)))
  (is (= :not-established (:actual-xtdb-order report)))
  (is (= :not-established (:transaction-causality report)))
  (is (= :not-established (:complete-write-coverage report)))
  (is (= :diagnostic-only (:proof-status report)))
  (is (= :not-issued (:certificate report)))
  (is (= :none (:authority report)))
  (is (= report (replay/require-correspondence! report)))
  (assert-ledger! report))

(deftest competing-guarded-claims-follow-release-wave-compatible-serial-paths
  (let [reports (replay/replay-matrix!
                 (partial scenario :competing-claims) schedules {})]
    (is (= 3 (count reports)))
    (doseq [[waves report] (map vector schedules reports)]
      (testing (str "competing Claim schedule " waves)
        (assert-report! report waves)
        (is (= (if (= :committed (first (status-vector report)))
                 "helper-a" "helper-b")
               (get-in report [:observed-state
                               (first (keys (:observed-state report))) :owner])))
        (is (= 1 (-> report :observed-state vals first :revision)))
        (when (= [[0] [1]] waves)
          (is (= [:committed :rejected] (status-vector report))))
        (when (= [[1] [0]] waves)
          (is (= [:rejected :committed] (status-vector report))))))))

(deftest claim-and-membership-revocation-have-guarded-serial-explanations
  (let [reports (replay/replay-matrix!
                 (partial scenario :claim-and-revocation) schedules {})]
    (is (= 3 (count reports)))
    (doseq [[waves report] (map vector schedules reports)]
      (testing (str "Claim versus revocation schedule " waves)
        (is (:matching-serial-path? report))
        (is (= :committed (second (status-vector report)))
            "The membership version itself is not concurrently changed by Claim")
        (is (contains? #{[:committed :committed]
                         [:rejected :committed]}
                       (status-vector report)))
        (is (= false (->> (:observed-state report)
                          (filter (fn [[target _]] (= :memberships (first target))))
                          first val :active)))
        (is (= (if (= :committed (first (status-vector report)))
                 "helper-a" "unclaimed")
               (->> (:observed-state report)
                    (filter (fn [[target _]] (= :requests (first target))))
                    first val :owner)))
        (when (= [[0] [1]] waves)
          (is (= [:committed :committed] (status-vector report))))
        (when (= [[1] [0]] waves)
          (is (= [:rejected :committed] (status-vector report))))
        (is (= :diagnostic-only (:proof-status report)))
        (is (= :none (:authority report)))
        (is (= report (replay/require-correspondence! report)))
        (assert-ledger! report)))))

(deftest multi-document-conditional-transaction-is-all-or-none
  (let [reports (replay/replay-matrix!
                 (partial scenario :atomic-multi-document) schedules {})]
    (is (= 3 (count reports)))
    (doseq [[waves report] (map vector schedules reports)]
      (testing (str "atomic multi-document schedule " waves)
        (assert-report! report waves)
        (let [docs (:observed-state report)
              created (first (for [[target doc] docs
                                   :when (and (= :requests (first target))
                                              (= "additional" (:owner doc)))]
                               doc))
              winner-is-multi? (= :committed (first (status-vector report)))]
          (is (= winner-is-multi? (some? created))
              "New document must be present iff its multi-write transaction committed")
          (is (= (if winner-is-multi? "helper-a" "helper-b")
                 (:owner (first (for [[target doc] docs
                                        :when (and (= :requests (first target))
                                                   (not= "additional" (:owner doc)))]
                                    doc)))))
          (is (= 1 (count (filter (fn [[target _]]
                                    (= :memberships (first target))) docs))))
          (is (true? (:active (first (for [[target doc] docs
                                              :when (= :memberships (first target))]
                                          doc))))))
        (when (= [[0] [1]] waves)
          (is (= [:committed :rejected] (status-vector report))))
        (when (= [[1] [0]] waves)
          (is (= [:rejected :committed] (status-vector report))))))))

(defn- in-memory-scenario [id]
  (let [before (request-document id "unclaimed")
        plans [{:commands [(claim before "helper-a")] :emit false}
               {:commands [(claim before "helper-b")] :emit false}]
        state (atom {[:requests id] before})]
    {:initial-state @state
     :plans plans
     :targets [[:requests id]]
     :submit! (fn [index _ops]
                (locking state
                  (let [{:keys [status after]} (model/step @state (nth plans index))]
                    (reset! state after)
                    (if (= :committed status)
                      {:status :committed :tx-key {:unverified-id -1}}
                      {:status :rejected :error/code :xtdb/assert-failed}))))
     :readback! (fn [_] @state)}))

(defn- invalid-reason [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo error (:reason (ex-data error)))))

(deftest matching-synthetic-serial-observations-are-not-authenticated
  (let [report (replay/replay! (in-memory-scenario "v1407-synthetic") [[0] [1]])]
    (is (:matching-serial-path? report))
    (is (= :diagnostic-only (:proof-status report)))
    (is (= :not-established (:transaction-causality report)))
    (is (= :not-established (:actual-xtdb-order report)))
    (is (= :none (:authority report)))
    (is (= :not-issued (:certificate report)))))

(deftest globally-possible-but-wave-incompatible-observations-fail
  (let [s (in-memory-scenario "v1407-forged-order")
        backwards (:after (model/step (:initial-state s) (second (:plans s))))
        report (replay/replay!
                (assoc s
                       :submit! (fn [index _]
                                  (if (zero? index)
                                    {:status :rejected :error/code :xtdb/assert-failed}
                                    {:status :committed :tx-key {:invented true}}))
                       :readback! (fn [_] backwards))
                [[0] [1]])]
    (is (false? (:matching-serial-path? report)))
    (is (= :no-admitted-serial-path
           (invalid-reason #(replay/require-correspondence! report))))
    (is (= :none (:authority report)))))

(deftest incomplete-target-coverage-rejects-a-scenario-before-submission
  (let [s (in-memory-scenario "v1407-missing-target")
        submissions (atom 0)
        bad (assoc s :targets []
                   :submit! (fn [& _] (swap! submissions inc) nil))]
    (is (= :incomplete-targets
           (invalid-reason #(replay/replay! bad [[0 1]]))))
    (is (zero? @submissions))))
