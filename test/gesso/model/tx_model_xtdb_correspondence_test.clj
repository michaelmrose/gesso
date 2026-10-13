(ns gesso.model.tx-model-xtdb-correspondence-test
  "Bounded, real-XTDB correspondence tests for gesso.model.tx-model.

   Exercises the *production* gesso.model.tx/prepare lowering through a live
   embedded XTDB node. Each serialized execution is compared with the pure
   reference machine's prediction of commit/rejection and the persisted
   document state. A rejection must be XTDB's typed :xtdb/assert-failed
   conflict; unrelated execution failures are rethrown, never counted as
   evidence of correspondence.

   These tests cover a deliberately small admitted subset: version-guarded
   :put-docs on :requests and :memberships, including document absence for
   creates. Their serialization is deliberate. They do not establish XTDB's
   isolation under truly concurrent submissions, business authorization,
   durable application-command replay, general SQL correspondence, or
   correctness of domain-specific guards. In particular, the missing-guard
   test demonstrates a policy dependency that this generic layer cannot
   infer. This namespace is NOT a certification or formal proof artifact."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.model.command :as command]
   [gesso.model.tx :as tx]
   [gesso.model.tx-model :as model]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]))

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

(defn- prepared-ops [a-plan]
  ;; Gesso's actual XTDB/HoneySQL lowering: no test-side SQL formatter.
  (:tx-ops (tx/prepare {} a-plan)))

(defn- fixture []
  (let [suffix (str (random-uuid))
        req (request-document (str "tx-model-request-" suffix))
        mem (membership (str "tx-model-member-" suffix))
        state {[:requests (:xt/id req)] req
               [:memberships (:xt/id mem)] mem}]
    ;; Seed through real XTDB; each scenario gets disjoint unique IDs.
    (xt/execute-tx (node) [[:put-docs :requests req]
                           [:put-docs :memberships mem]])
    {:request req
     :membership mem
     :state state}))

(defn- select-document [table id]
  ;; Fixed SQL projections for the admitted test document shapes. Query rows
  ;; normalize _id to :xt/id in the XTDB 2 public Clojure API.
  (let [sql (case table
              :requests
              "SELECT _id, revision, created_at, updated_at, owner FROM requests WHERE _id = ?"
              :memberships
              "SELECT _id, revision, created_at, updated_at, active FROM memberships WHERE _id = ?")
        rows (xt/q (node) [sql id])]
    (when-not (<= (count rows) 1)
      (throw (ex-info "More than one document for the requested ID."
                      {:table table :id id :rows rows})))
    (first rows)))

(defn- persisted-state [targets]
  (into {}
        (keep (fn [[table id :as target]]
                (when-let [document (select-document table id)]
                  [target document])))
        targets))

(defn- execute-and-classify [ops]
  (try
    {:status :committed
     :tx-key (xt/execute-tx (node) ops)}
    (catch Throwable error
      (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data error)))
        {:status :rejected
         :error/code :xtdb/assert-failed}
        (throw error)))))

(defn- compare-step!
  "Compare the abstract serial step to actual XTDB execution, then read the
   durable documents rather than trusting the in-memory expected state.
   `targets` includes absent write targets, to detect partial commits."
  [before a-plan targets]
  (let [prediction (model/step before a-plan)
        execution (execute-and-classify (prepared-ops a-plan))
        actual (persisted-state targets)]
    (is (= (:status prediction) (:status execution))
        (str "Reference/XTDB disagreement: " (pr-str execution)))
    (is (= (:after prediction) actual)
        (str "Persisted documents disagree with reference step: "
             (pr-str {:expected (:after prediction) :actual actual})))
    (when (= :rejected (:status prediction))
      (is (= :xtdb/assert-failed (:error/code execution))
          "Only a recognized XTDB ASSERT conflict counts as rejection."))
    {:prediction prediction
     :execution execution
     :actual actual}))

(deftest successful-guarded-claim-and-stale-retry-correspond-test
  (let [{:keys [request membership state]} (fixture)
        action (plan [(claim request "helper-a")]
                     (command/guard :memberships membership version))
        targets (keys state)
        first-step (compare-step! state action targets)
        second-step (compare-step! (:actual first-step) action targets)]
    (is (= :committed (get-in first-step [:prediction :status])))
    (is (= "helper-a"
           (get-in first-step [:actual [:requests (:xt/id request)] :owner])))
    (is (= :rejected (get-in second-step [:prediction :status])))
    (is (= (:actual first-step) (:actual second-step))
        "Repeating a prepared update must not apply it twice.")))

(deftest competing-prepared-claims-correspond-in-both-serial-orders-test
  (doseq [order [[0 1] [1 0]]]
    (let [{:keys [request membership state]} (fixture)
          actions [(plan [(claim request "helper-a")]
                         (command/guard :memberships membership version))
                   (plan [(claim request "helper-b")]
                         (command/guard :memberships membership version))]
          reference-path (first (filter #(= order (:order %))
                                        (:paths (model/explore-orders state actions))))
          actual-trace (loop [current state
                              remaining order
                              trace []]
                         (if-let [index (first remaining)]
                           (let [result (compare-step! current (nth actions index)
                                                       (keys state))]
                             (recur (:actual result)
                                    (rest remaining)
                                    (conj trace (get-in result [:execution :status]))))
                           trace))]
      (is (= (:final-state reference-path) (persisted-state (keys state))))
      (is (= (mapv :status (:trace reference-path)) actual-trace))
      (is (= [:committed :rejected] actual-trace))
      (is (= (if (= 0 (first order)) "helper-a" "helper-b")
             (:owner (select-document :requests (:xt/id request))))))))

(deftest stale-membership-guard-rejects-claim-test
  (let [{:keys [request membership state]} (fixture)
        claim-plan (plan [(claim request "helper-a")]
                         (command/guard :memberships membership version))
        revoke-plan (plan [(revoke membership)])
        after-revoke (:actual (compare-step! state revoke-plan (keys state)))
        result (compare-step! after-revoke claim-plan (keys state))]
    (is (= false
           (get-in after-revoke [[:memberships (:xt/id membership)] :active])))
    (is (= :rejected (get-in result [:execution :status])))
    (is (= "unclaimed"
           (get-in result [:actual [:requests (:xt/id request)] :owner])))))

(deftest rejected-multi-write-is-atomic-in-real-xtdb-test
  (let [{:keys [request membership state]} (fixture)
        created (request-document (str "tx-model-created-" (random-uuid)))
        action (plan [(claim request "helper-a")
                      (command/create :requests created version)]
                     (command/guard :memberships membership version))
        revoke-plan (plan [(revoke membership)])
        all-targets (conj (vec (keys state)) [:requests (:xt/id created)])
        after-revoke (:actual (compare-step! state revoke-plan (keys state)))
        result (compare-step! after-revoke action all-targets)]
    (is (= :rejected (get-in result [:execution :status])))
    (is (= "unclaimed" (:owner (select-document :requests (:xt/id request)))))
    (is (nil? (select-document :requests (:xt/id created)))
        "A failing guard must suppress every put-docs operation.")))

(deftest successful-multi-write-matches-reference-test
  (let [{:keys [request membership state]} (fixture)
        created (request-document (str "tx-model-created-" (random-uuid)))
        action (plan [(claim request "helper-a")
                      (command/create :requests created version)]
                     (command/guard :memberships membership version))
        result (compare-step! state action
                              (conj (vec (keys state)) [:requests (:xt/id created)]))]
    (is (= :committed (get-in result [:execution :status])))
    (is (= created (select-document :requests (:xt/id created))))
    (is (= "helper-a" (:owner (select-document :requests (:xt/id request)))))))

(deftest duplicate-create-is-rejected-without-overwrite-test
  (let [created (request-document (str "tx-model-create-" (random-uuid)))
        target [:requests (:xt/id created)]
        action (plan [(command/create :requests created version)])
        first-step (compare-step! {} action [target])
        retry (compare-step! (:actual first-step) action [target])]
    (is (= :committed (get-in first-step [:execution :status])))
    (is (= :rejected (get-in retry [:execution :status])))
    (is (= created (select-document :requests (:xt/id created))))))

(deftest omitting-revoked-membership-guard-permits-an-unsafe-domain-outcome-test
  (let [{:keys [request membership state]} (fixture)
        guarded (plan [(claim request "helper-a")]
                      (command/guard :memberships membership version))
        unguarded (plan [(claim request "helper-a")])
        revoke-plan (plan [(revoke membership)])
        after-revoke (:actual (compare-step! state revoke-plan (keys state)))
        guarded-prediction (model/step after-revoke guarded)
        unguarded-result (compare-step! after-revoke unguarded (keys state))]
    (is (= :rejected (:status guarded-prediction)))
    (is (= :committed (get-in unguarded-result [:execution :status])))
    (is (= false
           (get-in unguarded-result [:actual [:memberships (:xt/id membership)] :active])))
    (is (= "helper-a"
           (get-in unguarded-result [:actual [:requests (:xt/id request)] :owner])))
    (testing "This is a counterexample to an *undeclared policy*, not a pass for authorization."
      (is (= :committed (:status (model/step after-revoke unguarded)))))))
