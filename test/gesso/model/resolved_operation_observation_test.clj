(ns gesso.model.resolved-operation-observation-test
  "Permanent v810 regressions for v809's optional execution-observation hook.

   Observation is diagnostic, not a proof certificate. These tests distinguish
   what was passed to tx/transact!, what was reported as a physical outcome,
   and what the authoritative command record establishes. Simulated commits
   test hook control flow; embedded-XTDB cases independently inspect storage.

   Event sinks are neither authenticated nor durable; missing events, opaque
   domain predicates, and independent XTDB reads remain trust boundaries."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.choreo.identity :as identity]
   [gesso.live.core :as live]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.resolved-operation :as resolved]
   [gesso.model.tx :as tx]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})
(def ^:private outcome {:status :claimed :owner "helper-a"})
(def ^:private now "t1")

(defn- id [] (str (random-uuid)))
(defn- request []
  {:xt/id (id) :revision 0 :created-at "t0" :updated-at "t0"
   :owner "unclaimed"})
(defn- membership []
  {:xt/id (id) :revision 0 :created-at "t0" :updated-at "t0"
   :active true})
(defn- claimed [before]
  (assoc before :revision (inc (:revision before))
         :updated-at now :owner "helper-a"))
(defn- binding-for [request-id]
  {:command-id (identity/command-id (id))
   :principal (identity/principal "helper-a")
   :operation :request/claim
   :arguments {:request-id request-id}})
(defn- plan-for [before guard]
  {:commands [(command/update-command :requests :claim before
                                      (claimed before) version)]
   :guards (if guard [(command/guard :memberships guard version)] [])
   :emit false})
(defn- invoke [environment mode observer]
  ((:operation environment)
   (cond-> (:ctx environment)
     (some? observer) (assoc resolved/observation-context-key observer))
   {:binding (:binding environment) :mode mode}))
(defn- events [captured] (fn [event] (swap! captured conj event)))
(defn- reported [captured]
  (mapv :phase @captured))
(defn- terminal [captured]
  (last @captured))
(defn- error-kind [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e
                 (:error/kind (ex-data e)))))

(defn- fake-environment
  ([] (fake-environment nil))
  ([guard]
   (let [before (request)
         binding (binding-for (:xt/id before))
         record (atom nil)
         allowed? (atom true)
         counts (atom {:read 0 :disclose 0 :construct 0})
         op (resolved/build-operation
             {:operation :request/claim
              :read-record! (fn [_ _]
                              (swap! counts update :read inc)
                              @record)
              :may-disclose? (fn [_ _ _]
                               (swap! counts update :disclose inc)
                               @allowed?)
              :construct! (fn [_ _]
                            (swap! counts update :construct inc)
                            {:now now :plan (plan-for before guard)
                             :outcome outcome})})]
     {:operation op :binding binding :before before :guard guard
      :record record :allowed? allowed? :counts counts :ctx {}})))

(defn- paired-events! [captured environment]
  (let [[prepared physical :as pair] @captured]
    (is (= 2 (count pair)))
    (is (= [:prepared :physical-outcome] (reported captured)))
    (is (uuid? (:attempt-id prepared)))
    (is (= (:attempt-id prepared) (:attempt-id physical)))
    (is (= (:binding environment) (:binding prepared) (:binding physical)))
    (is (= :request/claim (:operation prepared) (:operation physical)))
    (is (= (:record prepared) (:record physical)))
    (is (= (:plan prepared) (:plan physical)))
    (is (= (resolution/command-record (:binding environment) outcome now)
           (:record prepared)))
    (is (= 2 (count (get-in prepared [:plan :commands]))))
    (is (= :gesso_commands
           (-> prepared :plan :commands last :model/entity-type)))
    (is (= resolved/execution-observation-scope
           (:scope prepared) (:scope physical)))
    (is (nil? (:physical/status prepared)))
    pair))

(deftest assurance-scope-does-not-promise-proof-test
  (let [scope resolved/execution-observation-scope]
    (is (= :gesso.model.resolved-operation/transaction-attempt-v1
           (:capture scope)))
    (is (contains? (:observed scope) :explicit-xtdb-assertion-rejection))
    (is (contains? (:not-established scope) :observer-event-authentication))
    (is (contains? (:not-established scope) :observer-event-durability))
    (is (contains? (:not-established scope) :policy-guard-completeness))
    (is (contains? (:not-established scope) :universal-linearizability))))

(deftest successful-ack-emits-correlated-exact-incorporation-plan-test
  (let [e (fake-environment)
        captured (atom [])
        handed-to-transaction (atom nil)
        acknowledged {:commit/status :committed :tx-result {:tx-id 17}}]
    (with-redefs [tx/transact! (fn [_ctx plan]
                                 (reset! handed-to-transaction plan)
                                 acknowledged)]
      (is (= :incorporated (:status (invoke e :submit (events captured)))))
      (paired-events! captured e)
      (is (= @handed-to-transaction (:plan (first @captured))))
      (is (= :committed (:physical/status (terminal captured))))
      (is (= :gesso-model-transaction-ack
             (:physical/source (terminal captured))))
      (is (= {:tx-id 17} (:physical/receipt (terminal captured))))
      (is (= 1 (:construct @(:counts e)))))))

(deftest no-observer-keeps-existing-execution-contract-test
  (let [e (fake-environment)
        submissions (atom 0)]
    (with-redefs [tx/transact! (fn [_ctx _plan]
                                 (swap! submissions inc)
                                 {:commit/status :committed})]
      (is (= :incorporated (:status (invoke e :submit nil))))
      (is (= 1 @submissions))
      (is (= 1 (:construct @(:counts e)))))))

(deftest explicit-xtdb-assertion-rejection-remains-unresolved-test
  (let [e (fake-environment)
        captured (atom [])]
    (with-redefs [tx/transact! (fn [& _]
                                 (throw (ex-info "guard failed"
                                                 {:xtdb.error/code :xtdb/assert-failed}))) ]
      (let [result (invoke e :submit (events captured))]
        (is (= :unresolved (:status result)))
        (is (not (contains? result :outcome)))
        (is (not (contains? result :retry?)))))
    (paired-events! captured e)
    (is (= :rejected (:physical/status (terminal captured))))
    (is (= :xtdb-explicit-assertion-failure
           (:physical/source (terminal captured))))
    (is (not (contains? (terminal captured) :physical/receipt)))
    (is (= 2 (:read @(:counts e))))))

(deftest commit-ack-lost-is-indeterminate-even-when-reconciliation-finds-record-test
  (let [e (fake-environment)
        captured (atom [])
        submissions (atom 0)]
    (with-redefs [tx/transact! (fn [& _]
                                 (swap! submissions inc)
                                 (reset! (:record e)
                                         (resolution/command-record (:binding e)
                                                                    outcome now))
                                 (throw (ex-info "network lost acknowledgment" {})))]
      (let [result (invoke e :submit (events captured))]
        (is (= :replay (:status result)))
        (is (= outcome (:outcome result)))))
    (paired-events! captured e)
    (is (= :indeterminate (:physical/status (terminal captured))))
    (is (= :unclassified-transaction-exception
           (:physical/source (terminal captured))))
    (is (not (contains? (terminal captured) :physical/receipt)))
    (is (= 1 @submissions))
    (is (= 2 (:read @(:counts e))))))

(deftest unrecognized-return-is-not-a-commit-or-rejection-test
  (let [e (fake-environment)
        captured (atom [])]
    (with-redefs [tx/transact! (fn [& _] {:commit/status :unknown :tx-result {:tx-id 9}})]
      (is (= :unresolved (:status (invoke e :submit (events captured))))))
    (paired-events! captured e)
    (is (= :indeterminate (:physical/status (terminal captured))))
    (is (= :unrecognized-transaction-response
           (:physical/source (terminal captured))))
    (is (not (contains? (terminal captured) :physical/receipt)))))

(deftest forged-post-commit-exception-is-not-accepted-test
  (let [e (fake-environment)
        captured (atom [])]
    (with-redefs [tx/transact! (fn [& _]
                                 (throw (ex-info "fake post-commit classification"
                                                 {:commit/status :committed
                                                  :failure/stage :post-commit-delivery
                                                  :tx-result {:tx-id 99}})))]
      (is (= :unresolved (:status (invoke e :submit (events captured))))))
    (paired-events! captured e)
    (is (= :indeterminate (:physical/status (terminal captured))))
    (is (not (contains? (terminal captured) :physical/receipt)))))

(deftest classified-post-commit-delivery-failure-is-observed-as-committed-test
  (let [e (fake-environment)
        captured (atom [])
        receipt {:tx-id 43}]
    (with-redefs [tx/transact! (fn [& _]
                                 (throw (ex-info "post-commit delivery failed"
                                                 {:error/type live/post-commit-delivery-failure-type
                                                  :commit/status :committed
                                                  :failure/stage :post-commit-delivery
                                                  :tx-result receipt})))]
      (is (= :unresolved (:status (invoke e :submit (events captured))))))
    (paired-events! captured e)
    (is (= :committed (:physical/status (terminal captured))))
    (is (= :gesso-live-classified-post-commit-failure
           (:physical/source (terminal captured))))
    (is (= receipt (:physical/receipt (terminal captured))))
    ;; A positive physical classification does not manufacture an application
    ;; replay result when the separately read authoritative row is absent.
    (is (= 2 (:read @(:counts e))))))

(deftest classified-post-commit-failure-with-record-reconciles-without-resubmit-test
  (let [e (fake-environment)
        captured (atom [])
        submissions (atom 0)]
    (with-redefs [tx/transact! (fn [& _]
                                 (swap! submissions inc)
                                 (reset! (:record e)
                                         (resolution/command-record (:binding e) outcome now))
                                 (throw (ex-info "post-commit delivery failed"
                                                 {:error/type live/post-commit-delivery-failure-type
                                                  :commit/status :committed
                                                  :failure/stage :post-commit-delivery}))) ]
      (is (= :replay (:status (invoke e :submit (events captured))))))
    (is (= :committed (:physical/status (terminal captured))))
    (is (= 1 @submissions))
    (is (= 1 (:construct @(:counts e))))))

(deftest observer-failure-before-and-after-commit-cannot-change-result-test
  (doseq [throw-on-phase [:prepared :physical-outcome]]
    (let [e (fake-environment)
          called (atom [])
          submitted (atom 0)]
      (with-redefs [tx/transact! (fn [& _]
                                   (swap! submitted inc)
                                   {:commit/status :committed})]
        (is (= :incorporated
               (:status (invoke e :submit (fn [event]
                                             (swap! called conj (:phase event))
                                             (when (= throw-on-phase (:phase event))
                                               (throw (ex-info "observer failed" {})))))))))
      (is (= [:prepared :physical-outcome] @called))
      (is (= 1 @submitted)))))

(deftest observer-errors-cannot-turn-rejection-into-success-test
  (let [e (fake-environment)
        calls (atom 0)]
    (with-redefs [tx/transact! (fn [& _]
                                 (throw (ex-info "stale request"
                                                 {:xtdb.error/code :xtdb/assert-failed}))) ]
      (is (= :unresolved
             (:status (invoke e :submit (fn [_]
                                          (swap! calls inc)
                                          (throw (Exception. "sink failed"))))))))
    (is (= 2 @calls))))

(deftest observation-attempt-ids-are-fresh-per-submission-test
  (let [e (fake-environment)
        captured (atom [])]
    (with-redefs [tx/transact! (fn [& _]
                                 (throw (ex-info "stale dependency"
                                                 {:xtdb.error/code :xtdb/assert-failed}))) ]
      (is (= :unresolved (:status (invoke e :submit (events captured)))))
      (is (= :unresolved (:status (invoke e :submit (events captured))))))
    (let [[first-prepared first-final second-prepared second-final] @captured]
      (is (= [:prepared :physical-outcome :prepared :physical-outcome]
             (reported captured)))
      (is (= (:attempt-id first-prepared) (:attempt-id first-final)))
      (is (= (:attempt-id second-prepared) (:attempt-id second-final)))
      (is (not= (:attempt-id first-prepared) (:attempt-id second-prepared)))
      (is (= (:binding first-prepared) (:binding second-prepared))))))

(deftest replay-and-reconcile-emit-no-transaction-events-test
  (let [e (fake-environment)
        captured (atom [])]
    (reset! (:record e) (resolution/command-record (:binding e) outcome now))
    (with-redefs [tx/transact! (fn [& _] (throw (ex-info "unexpected write" {})))]
      (is (= :replay (:status (invoke e :submit (events captured)))))
      (is (= :replay (:status (invoke e :reconcile (events captured)))))
      (is (empty? @captured))
      (is (zero? (:construct @(:counts e)))))))

(deftest incompatible-and-undisclosable-records-emit-no-events-test
  (let [e (fake-environment)
        captured (atom [])
        other (assoc (:binding e) :arguments {:request-id (id)})]
    (reset! (:record e) (resolution/command-record (:binding e) outcome now))
    (with-redefs [tx/transact! (fn [& _] (throw (ex-info "unexpected write" {})))]
      (is (= :incompatible
             (:status ((:operation e)
                       {resolved/observation-context-key (events captured)}
                       {:binding other :mode :submit}))))
      (reset! (:allowed? e) false)
      (is (= :unavailable (:status (invoke e :submit (events captured))))))
    (is (empty? @captured))
    (is (zero? (:construct @(:counts e))))))

(deftest reconciliation-without-record-never-validates-observer-or-submits-test
  (let [e (fake-environment)]
    (with-redefs [tx/transact! (fn [& _] (throw (ex-info "unexpected write" {})))]
      (is (= :unresolved
             (:status ((:operation e)
                       {resolved/observation-context-key :not-a-function}
                       {:binding (:binding e) :mode :reconcile})))))
    (is (zero? (:construct @(:counts e))))))

(deftest malformed-observer-rejected-before-construction-test
  (let [e (fake-environment)
        submitted (atom 0)]
    (with-redefs [tx/transact! (fn [& _] (swap! submitted inc))]
      (doseq [invalid [nil false :bogus [] {:fn :bogus}]]
        (is (= :invalid-observer
               (error-kind #((:operation e)
                             {resolved/observation-context-key invalid}
                             {:binding (:binding e) :mode :submit})))))
      (is (= 0 @submitted))
      (is (= 0 (:construct @(:counts e)))))))

(deftest planning-errors-produce-no-observation-event-test
  (let [e (fake-environment)
        captured (atom [])
        invalid (resolved/build-operation
                 {:operation :request/claim
                  :read-record! (fn [& _] nil)
                  :may-disclose? (fn [& _] true)
                  :construct! (fn [& _] {:now now})})]
    (is (= :invalid-construction
           (error-kind #(invalid {resolved/observation-context-key
                                  (events captured)}
                                 {:binding (:binding e) :mode :submit}))))
    (is (empty? @captured))))

(deftest authoritative-first-read-failure-produces-no-observation-event-test
  (let [e (fake-environment)
        captured (atom [])
        failing (resolved/build-operation
                 {:operation :request/claim
                  :read-record! (fn [& _] (throw (ex-info "read failed" {})))
                  :may-disclose? (fn [& _] true)
                  :construct! (fn [& _] (throw (Exception. "must not construct")))})]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"read failed"
                          (failing {resolved/observation-context-key
                                    (events captured)}
                                   {:binding (:binding e) :mode :submit})))
    (is (empty? @captured))))

;; Embedded XTDB exercises the real Gesso model transaction path. The node is
;; shared only within this namespace, with unique row IDs per test.
(defonce ^:private !node (atom nil))
(defn- database [] (or @!node (throw (ex-info "No test XTDB node" {}))))
(defn- xtdb-fixture [f]
  (with-open [node (xtn/start-node {})]
    (reset! !node node)
    (try (f) (finally (reset! !node nil)))))
(use-fixtures :once xtdb-fixture)

(defn- read-row [table row-id]
  (let [rows (xt/q (database)
                   [(str "SELECT * FROM " (name table) " WHERE _id = ?") row-id])]
    (when (> (count rows) 1)
      (throw (ex-info "Duplicate XTDB rows" {:table table :id row-id})))
    (first rows)))
(defn- seed! [& rows]
  (xt/execute-tx (database)
                 (mapv (fn [[table doc]] [:put-docs table doc]) rows)))
(defn- real-environment
  ([] (real-environment nil))
  ([guard]
   (let [e (fake-environment guard)
         constructs (atom 0)
         op (resolved/build-operation
             {:operation :request/claim
              :read-record! (fn [_ row-id] (read-row :gesso_commands row-id))
              :may-disclose? (fn [& _] true)
              :construct! (fn [& _]
                            (swap! constructs inc)
                            {:now now :outcome outcome
                             :plan (plan-for (:before e) guard)})})]
     (assoc e :operation op :ctx {:biff.xtdb/node (database)}
            :real-constructs constructs))))

(deftest real-xtdb-acknowledged-commit-events-match-persisted-state-test
  (let [e (real-environment)
        captured (atom [])
        row-id (resolution/record-id (:binding e))]
    (seed! [:requests (:before e)])
    (let [result (invoke e :submit (events captured))]
      (is (= :incorporated (:status result)))
      (is (= :committed (get-in result [:transaction :commit/status])))
      (paired-events! captured e)
      (is (= :committed (:physical/status (terminal captured))))
      (is (= :gesso-model-transaction-ack
             (:physical/source (terminal captured))))
      (is (= (get-in result [:transaction :tx-result])
             (:physical/receipt (terminal captured))))
      (is (= (claimed (:before e))
             (read-row :requests (-> e :before :xt/id))))
      (is (= (resolution/command-record (:binding e) outcome now)
             (read-row :gesso_commands row-id))))
    (let [prior-events (count @captured)]
      (is (= :replay (:status (invoke e :submit (events captured)))))
      (is (= :replay (:status (invoke e :reconcile (events captured)))))
      (is (= prior-events (count @captured)))
      (is (= 1 @(:real-constructs e))))))

(deftest real-xtdb-stale-membership-guard-emits-definitive-rejection-test
  (let [guard (membership)
        e (real-environment guard)
        captured (atom [])]
    (seed! [:requests (:before e)] [:memberships guard])
    (xt/execute-tx (database) (:tx-ops (tx/prepare {}
                    {:commands [(command/update-command :memberships :revoke guard
                                                        (assoc guard :revision 1
                                                               :updated-at "t1" :active false)
                                                        version)] :emit false})))
    (let [result (invoke e :submit (events captured))]
      (is (= :unresolved (:status result)))
      (is (not (contains? result :outcome))))
    (paired-events! captured e)
    (is (= :rejected (:physical/status (terminal captured))))
    (is (= :xtdb-explicit-assertion-failure
           (:physical/source (terminal captured))))
    (is (= (:before e) (read-row :requests (-> e :before :xt/id))))
    (is (nil? (read-row :gesso_commands
                        (resolution/record-id (:binding e)))))))

(deftest real-xtdb-lost-acknowledgment-physically-commits-but-event-stays-unknown-test
  (let [e (real-environment)
        captured (atom [])
        original-tx! tx/transact!
        submitted (atom 0)]
    (seed! [:requests (:before e)])
    (with-redefs [tx/transact! (fn [ctx plan]
                                 (swap! submitted inc)
                                 (original-tx! ctx plan)
                                 (throw (ex-info "lost-ack" {})))]
      (let [result (invoke e :submit (events captured))]
        (is (= :replay (:status result)))
        (is (= outcome (:outcome result)))))
    (paired-events! captured e)
    (is (= :indeterminate (:physical/status (terminal captured))))
    (is (= 1 @submitted))
    (is (= 1 @(:real-constructs e)))
    (is (= (claimed (:before e))
           (read-row :requests (-> e :before :xt/id))))
    (is (= (resolution/command-record (:binding e) outcome now)
           (read-row :gesso_commands
                     (resolution/record-id (:binding e)))))))

(deftest real-xtdb-observer-throws-after-commit-but-record-persists-test
  (let [e (real-environment)
        phases (atom [])]
    (seed! [:requests (:before e)])
    (is (= :incorporated
           (:status (invoke e :submit
                            (fn [event]
                              (swap! phases conj (:phase event))
                              (throw (ex-info "sink unavailable" {})))))))
    (is (= [:prepared :physical-outcome] @phases))
    (is (= (claimed (:before e))
           (read-row :requests (-> e :before :xt/id))))
    (is (= (resolution/command-record (:binding e) outcome now)
           (read-row :gesso_commands
                     (resolution/record-id (:binding e)))))))
