(ns gesso.model.command-resolution-concurrency-test
  "Bounded, real-XTDB concurrency regressions for the v789 atomic command
   incorporation construction.

   Independent workers submit already prepared gesso.model.tx plans through
   the ACTUAL XTDB transaction API. The accepted results must match an entire
   serial path of gesso.model.tx-model, including both transaction outcomes
   and the persisted domain documents AND command-result records. Compatible
   replays and incompatible reuse are checked only AFTER reading the stored
   command record and applying an explicit trusted disclosure predicate.

   A rejected duplicate submission is NOT itself a replay: only subsequent
   authoritative reconciliation can return the original recorded outcome.
   Likewise, a rejected business transition with no command record remains
   unresolved, not a durable business rejection or a safe retry instruction.

   Finite, single-node evidence only. The test's assumed Claim guards do not
   certify HumanHelp authorization, arbitrary domain invariants, command
   record immutability against untrusted writers, recovery liveness, or
   universal XTDB linearizability. No timing observation is used to infer
   which transaction linearized first."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.choreo.identity :as identity]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]
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

(def ^:private allowed (fn [_ _] true))
(def ^:private denied (fn [_ _] false))
(def ^:private commit-time "t1")
(defonce ^:private !node (atom nil))

(defn- node []
  (or @!node
      (throw (ex-info "Embedded XTDB fixture has not started." {}))))

(defn- with-real-xtdb [f]
  (with-open [database (xtn/start-node {})]
    (reset! !node database)
    (try (f)
         (finally (reset! !node nil)))))

(use-fixtures :once with-real-xtdb)

(defn- unique-id [prefix]
  (str prefix "-" (random-uuid)))

(defn- request-document [id]
  {:xt/id id :revision 0 :created-at "t0" :updated-at "t0"
   :owner "unclaimed"})

(defn- membership-document [id]
  {:xt/id id :revision 0 :created-at "t0" :updated-at "t0"
   :active true})

(defn- updated [before changes]
  (-> before
      (merge changes)
      (update :revision inc)
      (assoc :updated-at commit-time)))

(defn- claim-command [request owner]
  (command/update-command :requests :claim request
                          (updated request {:owner owner}) version))

(defn- revoke-command [membership]
  (command/update-command :memberships :revoke membership
                          (updated membership {:active false}) version))

(defn- command-binding [command-id request-id owner]
  {:command-id (identity/command-id command-id)
   :principal (identity/principal owner)
   :operation :request/claim
   :arguments {:request-id request-id}})

(defn- incorporation
  ([request owner command-id]
   (incorporation request owner command-id nil []))
  ([request owner command-id membership extra-commands]
   (let [supplied-binding (command-binding command-id (:xt/id request) owner)]
     (assoc
      (resolution/incorporation-plan
       {:binding supplied-binding
        :outcome {:status :claimed :owner owner}
        :now commit-time
        :plan {:commands (into [(claim-command request owner)] extra-commands)
               :guards (if membership
                         [(command/guard :memberships membership version)]
                         [])
               :emit false}})
      :binding supplied-binding))))

(defn- seed! [documents]
  (when (seq documents)
    (xt/execute-tx
     (node)
     (mapv (fn [[table document]] [:put-docs table document]) documents))))

(defn- scenario []
  (let [request (request-document (unique-id "resolution-race-request"))
        membership (membership-document (unique-id "resolution-race-member"))
        initial-state {[:requests (:xt/id request)] request
                       [:memberships (:xt/id membership)] membership}]
    (seed! [[:requests request] [:memberships membership]])
    {:request request
     :membership membership
     :state initial-state}))

(defn- select-document [table id]
  (let [sql (case table
              :requests
              "SELECT _id, revision, created_at, updated_at, owner FROM requests WHERE _id = ?"
              :memberships
              "SELECT _id, revision, created_at, updated_at, active FROM memberships WHERE _id = ?"
              :gesso_commands
              "SELECT * FROM gesso_commands WHERE _id = ?")
        rows (xt/q (node) [sql id])]
    (when (> (count rows) 1)
      (throw (ex-info "More than one authoritative document has this identity."
                      {:target [table id] :rows rows})))
    (first rows)))

(defn- persisted-state [targets]
  (into {}
        (keep (fn [[table id :as target]]
                (when-let [document (select-document table id)]
                  [target document])))
        targets))

(defn- stored-record [incorporated]
  (select-document resolution/command-entity-type
                   (resolution/record-id (:binding incorporated))))

(defn- execute-and-classify [operations]
  (try
    {:status :committed
     :tx-key (xt/execute-tx (node) operations)}
    (catch Throwable error
      (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data error)))
        {:status :rejected
         :error/code :xtdb/assert-failed}
        (throw error)))))

(defn- execute-overlapping! [plans]
  ;; Prepare the ACTUAL transaction lowering before releasing the workers.
  (let [operations (mapv (fn [plan] (:tx-ops (tx/prepare {} plan))) plans)
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
        (throw (ex-info "Concurrent XTDB workers did not become ready."
                        {:expected (count plans)
                         :remaining (.getCount ready)})))
      (.countDown start)
      (let [results (mapv #(deref % 60000 ::timeout) workers)]
        (when (some #{::timeout} results)
          (throw (ex-info "XTDB concurrent transaction timed out."
                          {:results results})))
        results)
      (finally
        (.countDown start)
        (doseq [worker workers]
          (when-not (realized? worker)
            (future-cancel worker)))))))

(defn- reference-outcome [path plan-count]
  {:statuses (reduce (fn [statuses {:keys [plan-index status]}]
                       (assoc statuses plan-index status))
                     (vec (repeat plan-count nil))
                     (:trace path))
   :state (:final-state path)})

(defn- compare-race!
  "Compare BOTH each transaction's result and the complete selected persisted
   state with an allowed serial execution of the SAME prepared plans. All
   command IDs, including records expected to remain absent, must be targets."
  [initial-state plans targets]
  (let [expected (set (map #(reference-outcome % (count plans))
                           (:paths (model/explore-orders initial-state plans))))
        results (execute-overlapping! plans)
        actual {:statuses (mapv :status results)
                :state (persisted-state targets)}]
    (is (every? (fn [{:keys [status error/code]}]
                  (or (= :committed status)
                      (and (= :rejected status)
                           (= :xtdb/assert-failed code))))
                results)
        "Rejection must be a typed XTDB assertion conflict, not an arbitrary error.")
    (is (contains? expected actual)
        (str "Concurrent command incorporation does not match a complete "
             "serial-reference outcome: " (pr-str {:actual actual
                                                    :expected expected})))
    actual))

(defn- targets-for [state incorporated]
  (into (vec (keys state))
        (map (fn [entry]
               [resolution/command-entity-type
                (resolution/record-id (:binding entry))]))
        incorporated))

(deftest compatible-duplicate-deliveries-incorporate-once-and-reconcile-test
  (dotimes [round 4]
    (testing (str "duplicate compatible command, round " round)
      (let [{:keys [request membership state]} (scenario)
            command-id (unique-id "resolution-duplicate")
            first-attempt (incorporation request "helper-a" command-id membership [])
            second-attempt (incorporation request "helper-a" command-id membership [])
            targets (targets-for state [first-attempt second-attempt])
            actual (compare-race! state
                                  [(:plan first-attempt) (:plan second-attempt)]
                                  targets)
            record (stored-record first-attempt)]
        (is (= #{:committed :rejected} (set (:statuses actual))))
        (is (= (:record first-attempt) record))
        (is (= (updated request {:owner "helper-a"})
               (select-document :requests (:xt/id request))))
        (is (= {:status :replay
                :outcome {:status :claimed :owner "helper-a"}
                :scope resolution/assurance-scope}
               (resolution/reconcile (:binding second-attempt) record allowed)))
        (is (= :unavailable
               (:status (resolution/reconcile (:binding second-attempt)
                                              record denied))))))))

(deftest incompatible-reuse-of-one-command-id-cannot-mutate-disjoint-targets-test
  (dotimes [round 4]
    (testing (str "same command ID / conflicting binding, round " round)
      (let [request-a (request-document (unique-id "resolution-disjoint-a"))
            request-b (request-document (unique-id "resolution-disjoint-b"))
            state {[:requests (:xt/id request-a)] request-a
                   [:requests (:xt/id request-b)] request-b}
            command-id (unique-id "resolution-conflict")
            first-attempt (incorporation request-a "helper-a" command-id)
            second-attempt (incorporation request-b "helper-b" command-id)
            _ (seed! [[:requests request-a] [:requests request-b]])
            actual (compare-race! state
                                  [(:plan first-attempt) (:plan second-attempt)]
                                  (targets-for state [first-attempt second-attempt]))
            winner (if (= :committed (first (:statuses actual)))
                     first-attempt second-attempt)
            loser (if (= :committed (first (:statuses actual)))
                    second-attempt first-attempt)
            record (stored-record winner)]
        (is (= #{:committed :rejected} (set (:statuses actual))))
        (is (= (:record winner) record))
        (is (= (updated (if (identical? winner first-attempt)
                          request-a request-b)
                        {:owner (if (identical? winner first-attempt)
                                  "helper-a" "helper-b")})
               (select-document :requests
                                (get-in winner [:binding :arguments :request-id]))))
        (is (= (if (identical? loser first-attempt) request-a request-b)
               (select-document :requests
                                (get-in loser [:binding :arguments :request-id]))))
        (is (= :incompatible
               (:status (resolution/reconcile (:binding loser) record allowed))))
        (is (= :unavailable
               (:status (resolution/reconcile (:binding loser) record denied))))))))

(deftest competing-distinct-commands-on-one-request-leave-loser-unresolved-test
  (dotimes [round 4]
    (testing (str "different command IDs / same request, round " round)
      (let [{:keys [request membership state]} (scenario)
            first-attempt (incorporation request "helper-a"
                                         (unique-id "resolution-first") membership [])
            second-attempt (incorporation request "helper-b"
                                          (unique-id "resolution-second") membership [])
            actual (compare-race! state
                                  [(:plan first-attempt) (:plan second-attempt)]
                                  (targets-for state [first-attempt second-attempt]))
            winner (if (= :committed (first (:statuses actual)))
                     first-attempt second-attempt)
            loser (if (= :committed (first (:statuses actual)))
                    second-attempt first-attempt)]
        (is (= #{:committed :rejected} (set (:statuses actual))))
        (is (= (:record winner) (stored-record winner)))
        (is (nil? (stored-record loser))
            "Failed domain transition must NOT create a durable command outcome.")
        (is (= :unresolved
               (:status (resolution/reconcile (:binding loser)
                                              (stored-record loser) allowed))))
        (is (= :replay
               (:status (resolution/reconcile (:binding winner)
                                              (stored-record winner) allowed))))))))

(deftest command-incorporation-racing-membership-revocation-is-atomic-test
  (dotimes [round 4]
    (testing (str "claim/revoke race, round " round)
      (let [{:keys [request membership state]} (scenario)
            attempt (incorporation request "helper-a"
                                   (unique-id "resolution-revoke") membership [])
            revoke-plan {:commands [(revoke-command membership)] :emit false}
            actual (compare-race! state [(:plan attempt) revoke-plan]
                                  (targets-for state [attempt]))
            accepted? (= :committed (first (:statuses actual)))
            record (stored-record attempt)]
        (is (= :committed (second (:statuses actual))))
        (is (false? (:active (select-document :memberships (:xt/id membership)))))
        (is (= accepted? (some? record))
            "Record exists exactly when its guarded domain transition committed.")
        (is (= (if accepted? "helper-a" "unclaimed")
               (:owner (select-document :requests (:xt/id request)))))
        (is (= (if accepted? :replay :unresolved)
               (:status (resolution/reconcile (:binding attempt) record allowed))))))))

(deftest disjoint-commands-with-disjoint-ids-both-incorporate-test
  (dotimes [round 3]
    (testing (str "two independent command incorporations, round " round)
      (let [request-a (request-document (unique-id "resolution-independent-a"))
            request-b (request-document (unique-id "resolution-independent-b"))
            state {[:requests (:xt/id request-a)] request-a
                   [:requests (:xt/id request-b)] request-b}
            _ (seed! [[:requests request-a] [:requests request-b]])
            first-attempt (incorporation request-a "helper-a"
                                         (unique-id "resolution-independent-command-a"))
            second-attempt (incorporation request-b "helper-b"
                                          (unique-id "resolution-independent-command-b"))
            actual (compare-race! state
                                  [(:plan first-attempt) (:plan second-attempt)]
                                  (targets-for state [first-attempt second-attempt]))]
        (is (= [:committed :committed] (:statuses actual)))
        (is (= (:record first-attempt) (stored-record first-attempt)))
        (is (= (:record second-attempt) (stored-record second-attempt)))))))

(deftest contested-multi-document-incorporation-has-no-orphan-writes-test
  (dotimes [round 4]
    (testing (str "atomic extra create / command record, round " round)
      (let [{:keys [request membership state]} (scenario)
            created (request-document (unique-id "resolution-side-effect"))
            first-attempt (incorporation request "helper-a"
                                         (unique-id "resolution-multi")
                                         membership
                                         [(command/create :requests created version)])
            second-attempt (incorporation request "helper-b"
                                          (unique-id "resolution-contender") membership [])
            targets (conj (targets-for state [first-attempt second-attempt])
                          [:requests (:xt/id created)])
            actual (compare-race! state
                                  [(:plan first-attempt) (:plan second-attempt)]
                                  targets)
            first-won? (= :committed (first (:statuses actual)))]
        (is (= #{:committed :rejected} (set (:statuses actual))))
        (is (= first-won? (some? (select-document :requests (:xt/id created)))))
        (is (= first-won? (some? (stored-record first-attempt))))
        (is (= (not first-won?) (some? (stored-record second-attempt))))
        (is (= (if first-won? "helper-a" "helper-b")
               (:owner (select-document :requests (:xt/id request)))))))))
