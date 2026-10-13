(ns gesso.live.optimistic.resolved-operation-concurrency-test
  "Bounded *concurrent* cross-layer correspondence for Live -> resolved operation
   -> Gesso transaction -> real embedded XTDB.

   Each submitted command enters the same trusted domain-construction gate
   before any worker is allowed to commit. This means every attempt has first
   seen an absent command record and the corresponding initial domain snapshot.
   Workers then contend using the actual Gesso Live protocol-v3 operation path.

   Compare BOTH each individual Live settlement/error and the persisted domain
   and command-record write set with an admissible serial run of the exact
   constructed transaction plans in gesso.model.tx-model. A :confirmed result
   corresponds to an incorporated plan; a :already-incorporated settlement
   follows a lost duplicate incorporation attempt and a durable replay read;
   :resolution-not-settleable means an attempted plan was not incorporated.

   Scope: these tests establish evidence for the tested finite races with
   application-owned pure Claim guards and projectors. The snapshot policy,
   authenticated principal, disclosure decision and authoritative basis are
   trusted fixture inputs, NOT proven HumanHelp business rules. Model paths
   are serial outcomes, not a universal proof of XTDB isolation/linearizability
   or delivery liveness. Unrecognized exceptions and timeouts fail closed."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.choreo.identity :as identity]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.optimistic.resolved-operation :as adapter]
   [gesso.live.optimistic.server :as server]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.resolved-operation :as model-resolved]
   [gesso.model.tx-model :as reference]
   [xtdb.api :as xt]
   [xtdb.node :as xtn])
  (:import
   [java.util.concurrent CountDownLatch TimeUnit]))

(def ^:private operation-id :request/claim)
(def ^:private now "2026-10-12T10:00:00Z")
(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})
(def ^:private outcome {:status :claimed})
(defonce ^:private active-database (atom nil))

(defn- with-database [f]
  (with-open [database (xtn/start-node {})]
    (reset! active-database database)
    (try (f)
         (finally (reset! active-database nil)))))

(use-fixtures :once with-database)

(defn- database []
  (or @active-database
      (throw (ex-info "Real embedded XTDB test fixture not running." {}))))

(defn- row [table id]
  (let [rows (xt/q (database)
                   [(case table
                      :requests "SELECT * FROM requests WHERE _id = ?"
                      :memberships "SELECT * FROM memberships WHERE _id = ?"
                      :gesso_commands "SELECT * FROM gesso_commands WHERE _id = ?")
                    id])]
    (when-not (<= (count rows) 1)
      (throw (ex-info "Multiple current documents at target." {:table table :id id})))
    (first rows)))

(defn- request [id]
  {:xt/id id :revision 0 :created-at "t0" :updated-at "t0"
   :owner "unclaimed"})

(defn- membership [id]
  {:xt/id id :revision 0 :created-at "t0" :updated-at "t0"
   :active true})

(defn- change [before attrs]
  (-> before (merge attrs) (update :revision inc) (assoc :updated-at now)))

(defn- claim-plan [req member owner]
  {:commands [(command/update-command
               :requests :claim req (change req {:owner owner}) version)]
   :guards [(command/guard :memberships member version)]
   :emit false})

(defn- fixture []
  (let [suffix (str (random-uuid))
        req (request (str "v802-request-" suffix))
        member (membership (str "v802-member-" suffix))]
    (xt/execute-tx (database) [[:put-docs :requests req]
                                [:put-docs :memberships member]])
    {:request req
     :membership member
     :initial {[:requests (:xt/id req)] req
               [:memberships (:xt/id member)] member}}))

(defn- browser-command [request-id command-id execution-id]
  (protocol/command
   {:command-id command-id
    :execution-id execution-id
    :operation operation-id
    :arguments {:request-id request-id}
    :observed-basis {:tx-id 1 :system-time "2026-10-12T09:00:00Z"}
    :scope [:request request-id]}))

(defn- fresh-command [request-id]
  (browser-command request-id
                   (identity/command-id (str (random-uuid)))
                   (identity/execution-id (str (random-uuid)))))

(defn- binding-for [principal cmd]
  {:command-id (:command-id cmd)
   :principal principal
   :operation operation-id
   :arguments (:arguments cmd)})

(defn- planned [binding req member]
  (:plan (resolution/incorporation-plan
          {:binding binding
           :now now
           :plan (claim-plan req member
                             (identity/raw-value (:principal binding)))
           :outcome outcome})))

(defn- persisted-state [initial bindings]
  (into {}
        (keep (fn [[table id :as target]]
                (when-let [doc (row table id)] [target doc])))
        (concat (keys initial)
                (map (fn [binding]
                       [:gesso_commands (resolution/record-id binding)])
                     bindings))))

(defn- server-for [req member calls construction-barrier]
  (let [op (model-resolved/build-operation
            {:operation operation-id
             :read-record! (fn [_ctx id] (row :gesso_commands id))
             :may-disclose? (fn [_ctx _binding _record] true)
             :construct! (fn [_ctx binding]
                           (swap! calls update :construct inc)
                           ;; Gate INSIDE the trusted plan constructor: both
                           ;; have already read an absent authoritative record.
                           (construction-barrier)
                           {:now now
                            :plan (claim-plan req member
                                              (identity/raw-value (:principal binding)))
                            :outcome outcome})})
        executor (adapter/build-executor
                  {:operation operation-id
                   :resolved-operation op
                   :submission-mode! (fn [_context] :submit)
                   :project! (fn [_context result]
                               (swap! calls update :project inc)
                               (let [stored (row :requests (:xt/id req))]
                                 {:resolution (if (= :replay (:status result))
                                                :already-incorporated :confirmed)
                                  :authoritative
                                  (protocol/authoritative
                                   {:presence :present
                                    :basis {:tx-id 2 :system-time now}
                                    :projection {:request/id (:xt/id stored)
                                                 :request/owner (:owner stored)}})
                                  :outcome :request/claimed}))})]
    (server/server
     {:principal-fn (fn [ctx] (:authenticated-principal ctx))
      :operations
      {operation-id
       (server/operation
        {:name :request/claim-optimistic
         :operation operation-id
         :execute! executor
         :settlement-contract
         {:confirmed {:outcomes #{:request/claimed} :commit/status :committed}
          :reconciled {:outcomes #{:request/claimed} :commit/status :committed}
          :already-incorporated {:outcomes #{:request/claimed} :commit/status :committed}}})}})))

(defn- invoke [app principal cmd]
  (try
    (let [prepared (server/run-command
                    app {:authenticated-principal principal
                         :biff.xtdb/node (database)} cmd)]
      (when-not (server/prepared-send? prepared)
        (throw (ex-info "Live returned a non-prepared result." {:prepared prepared})))
      (let [settlement (:settlement prepared)]
        (when-not (and (= (:command-id cmd) (:command-id settlement))
                       (= (:execution-id cmd) (:execution-id settlement))
                       (= :request/claimed (:outcome settlement)))
          (throw (ex-info "Live prepared a mismatched settlement."
                          {:command cmd :settlement settlement})))
        {:status (case (:resolution settlement)
                   :confirmed :committed
                   :already-incorporated :replayed
                   (throw (ex-info "Unexpected successful Live resolution."
                                   {:settlement settlement})))
         :settlement settlement}))
    (catch clojure.lang.ExceptionInfo e
      (if (= :resolution-not-settleable (:error/kind (ex-data e)))
        {:status :unresolved}
        (throw e)))))

(defn- gated-workers! [attempts]
  (let [n (count attempts)
        ready (CountDownLatch. n)
        release (CountDownLatch. 1)
        gate (fn []
               (.countDown ready)
               (when-not (.await release 45 TimeUnit/SECONDS)
                 (throw (ex-info "Timed out waiting for all Live constructors."
                                 {:workers n}))))
        calls (atom {:construct 0 :project 0})
        workers (mapv (fn [{:keys [request membership principal command]}]
                        (let [app (server-for request membership calls gate)]
                          (future (invoke app principal command))))
                      attempts)]
    (try
      (when-not (.await ready 45 TimeUnit/SECONDS)
        (throw (ex-info "Live requests did not overlap in domain construction."
                        {:workers n :remaining (.getCount ready)})))
      (.countDown release)
      (let [results (mapv #(deref % 70000 ::timeout) workers)]
        (when (some #{::timeout} results)
          (throw (ex-info "Concurrent Live operation timed out." {:results results})))
        {:results results :calls @calls})
      (finally
        (.countDown release)
        (doseq [worker workers]
          (when-not (realized? worker) (future-cancel worker)))))))

(defn- model-outcome [path n]
  {:statuses (reduce (fn [statuses {:keys [plan-index status]}]
                       (assoc statuses plan-index status))
                     (vec (repeat n nil))
                     (:trace path))
   :state (:final-state path)})

(defn- assert-race! [initial attempts]
  (let [bindings (mapv (fn [{:keys [principal command]}]
                         (binding-for principal command)) attempts)
        plans (mapv (fn [{:keys [request membership]} binding]
                      (planned binding request membership))
                    attempts bindings)
        expected (set (map #(model-outcome % (count plans))
                           (:paths (reference/explore-orders initial plans))))
        {:keys [results calls]} (gated-workers! attempts)
        actual {:statuses (mapv (fn [r]
                                  (case (:status r)
                                    :committed :committed
                                    :replayed :rejected
                                    :unresolved :rejected)) results)
                :state (persisted-state initial bindings)}]
    (is (= (count attempts) (:construct calls))
        "Every worker must have prepared its own candidate before release.")
    (is (contains? expected actual)
        (str "Concurrent Live execution not in the reference serial outcomes: "
             (pr-str {:actual actual :expected expected})))
    (is (= (count (filter #(contains? #{:committed :replayed} (:status %)) results))
           (:project calls))
        "Only authoritative incorporation/replay may produce projections.")
    (doseq [[attempt result] (map vector attempts results)]
      (let [binding (binding-for (:principal attempt) (:command attempt))
            record (row :gesso_commands (resolution/record-id binding))]
        (case (:status result)
          :committed
          (do
            (is (= :confirmed (get-in result [:settlement :resolution])))
            (is (= binding (:gesso.command/binding record)))
            (is (= outcome (:gesso.command/outcome record))))

          :replayed
          (do
            (is (= :already-incorporated
                   (get-in result [:settlement :resolution])))
            (is (= binding (:gesso.command/binding record)))
            (is (= outcome (:gesso.command/outcome record))))

          :unresolved
          (is (or (nil? record)
                  (not= binding (:gesso.command/binding record)))
              "A losing command may not have a matching incorporated record."))
        (when (contains? #{:committed :replayed} (:status result))
          (is (= (get-in result [:settlement :authoritative :projection :request/owner])
                 (get-in actual [:state [:requests (:xt/id (:request attempt))] :owner]))
              "Successful settlement must reflect the final stored owner."))))
    {:results results :calls calls :state (:state actual)}))

(defn- attempt [fixture principal command]
  {:request (:request fixture)
   :membership (:membership fixture)
   :principal principal
   :command command})

(deftest concurrent-distinct-claim-commands-have-one-authoritative-winner-test
  (dotimes [round 3]
    (testing (str "two different claim command IDs, round " round)
      (let [{:keys [request initial] :as f} (fixture)
            a (identity/principal "helper-v802-a")
            b (identity/principal "helper-v802-b")
            result (assert-race! initial
                                 [(attempt f a (fresh-command (:xt/id request)))
                                  (attempt f b (fresh-command (:xt/id request)))])]
        (is (= #{:committed :unresolved}
               (set (map :status (:results result)))))
        (is (= 1 (count (filter #(= :gesso_commands (first %))
                                (keys (:state result))))))
        (is (contains? #{"helper-v802-a" "helper-v802-b"}
                       (get-in result [:state [:requests (:xt/id request)] :owner])))))))

(deftest concurrent-duplicate-command-reconciles-without-double-commit-test
  (dotimes [round 3]
    (testing (str "same binding, two execution IDs, round " round)
      (let [{:keys [request initial] :as f} (fixture)
            principal (identity/principal "helper-v802-a")
            original (fresh-command (:xt/id request))
            duplicate (browser-command (:xt/id request)
                                       (:command-id original)
                                       (identity/execution-id (str (random-uuid))))
            result (assert-race! initial [(attempt f principal original)
                                          (attempt f principal duplicate)])]
        (is (= #{:committed :replayed}
               (set (map :status (:results result)))))
        (is (= 1 (count (filter #(= :gesso_commands (first %))
                                (keys (:state result))))))
        (is (= "helper-v802-a"
               (get-in result [:state [:requests (:xt/id request)] :owner])))))))

(deftest concurrent-incompatible-command-id-reuse-cannot-claim-two-requests-test
  (dotimes [round 2]
    (testing (str "same command ID, different bindings and targets, round " round)
      (let [f1 (fixture)
            f2 (fixture)
            initial (merge (:initial f1) (:initial f2))
            principal-a (identity/principal "helper-v802-a")
            principal-b (identity/principal "helper-v802-b")
            original (fresh-command (get-in f1 [:request :xt/id]))
            reused (browser-command (get-in f2 [:request :xt/id])
                                    (:command-id original)
                                    (identity/execution-id (str (random-uuid))))
            result (assert-race! initial [(attempt f1 principal-a original)
                                          (attempt f2 principal-b reused)])]
        (is (= #{:committed :unresolved} (set (map :status (:results result)))))
        (is (= 1 (count (filter #(= :gesso_commands (first %))
                                (keys (:state result))))))
        (is (= 1 (count (filter (fn [f]
                                 (not= "unclaimed"
                                       (get-in result [:state [:requests
                                                               (get-in f [:request :xt/id])]
                                                              :owner])))
                               [f1 f2]))))))))

(deftest independent-concurrent-claims-can-both-incorporate-test
  (dotimes [round 2]
    (testing (str "independent claim transactions, round " round)
      (let [f1 (fixture)
            f2 (fixture)
            initial (merge (:initial f1) (:initial f2))
            a (identity/principal "helper-v802-a")
            b (identity/principal "helper-v802-b")
            result (assert-race! initial
                                 [(attempt f1 a (fresh-command (get-in f1 [:request :xt/id])))
                                  (attempt f2 b (fresh-command (get-in f2 [:request :xt/id])))])]
        (is (= [:committed :committed] (mapv :status (:results result))))
        (is (= 2 (count (filter #(= :gesso_commands (first %))
                                (keys (:state result))))))))))
