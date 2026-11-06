(ns gesso.assurance.example.physical-single-node-guard-correspondence-test
  "v1510: Permanent adversarial regression tests for the v1509 bounded
   source-derived guard to prepared XTDB-plan correspondence boundary.

   These tests use actual Gesso typed declarations, tx/prepare lowering, and
   embedded XTDB. The checker is fixture-specific and pre-submission only:
   matching SQL assertions are NOT a certificate of executed guards,
   authenticated principals, transaction-internal prestate, writer closure,
   or universal refinement."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.example.single-node-proving :as pilot]
            [gesso.assurance.proof.typed-presence-semantics :as typed]
            [gesso.model.command :as command]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]))

(def ^:private catalog-id :slot/reservation-lifecycle)
(def ^:private mismatch :source-guard-and-concrete-plan-diverge)
(def ^:private source-mismatch :unbound-or-substituted-guard-source)

(defn- rejection [f]
  (try
    (f)
    :unexpected-success
    (catch clojure.lang.ExceptionInfo e
      (if (:physical-single-node-proving/rejection (ex-data e))
        (:reason (ex-data e))
        (throw e)))))

(defn- prepared [app op]
  (let [before ((:read! app))]
    {:before before
     :plan (#'physical/transition-plan before op)
     :check! (fn [plan]
               (#'physical/checked-guard-plan!
                (:catalog app) (:biff-context app) before op plan))}))

(defn- limited! [report op]
  (is (= :gesso.assurance.example.physical-single-node-proving/guard-plan-correspondence
         (:type report)))
  (is (= 1 (:version report)))
  (is (= catalog-id (:catalog-id report)))
  (is (= op (:operation-id report)))
  (is (true? (:source-guard-and-prepared-assertions-agree? report)))
  (is (= 2 (:prewrite-assertion-count report)))
  (is (= 1 (:observed-plan-write-count report)))
  (is (= :original-typed-guard-and-actual-lowered-gesso-plan (:checking report)))
  (is (= :not-established (:executed-plan-binding report)))
  (is (= :not-authenticated (:xtdb-internal-prestate report)))
  (is (= :not-established (:principal-authenticity report)))
  (is (= :not-established (:complete-writer-coverage report)))
  (is (= :not-established (:physical-refinement report)))
  (is (= :not-proved (:proof-status report)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :none (:authority report)))
  (doseq [forbidden [:slot-id :node :physical-before :physical-after
                     :physical-row :tx-ops :assertions :commands :prepared-plan
                     :transaction-key :principal :user-id :raw-sql]]
    (is (not (contains? report forbidden)) (str "No sensitive detail " forbidden))))

(defn- unchanged! [app original]
  (is (= original ((:read! app))) "No candidate preflight may mutate XTDB"))

(deftest reserve-preflight-checks-actual-lowered-assertions-without-writing
  (physical/with-proving-application!
    (fn [app]
      (let [before ((:read! app))
            {:keys [plan check!]} (prepared app :slot/reserve)
            ops (:tx-ops (tx/prepare (:biff-context app) plan))]
        (is (= 3 (count ops)))
        (is (every? #(and (vector? %) (string? (first %))
                          (re-find #"^ASSERT" (first %))) (take 2 ops)))
        (is (= :put-docs (first (last ops))))
        (limited! (check! plan) :slot/reserve)
        (limited! ((:guard-preflight! app) :slot/reserve) :slot/reserve)
        (unchanged! app before)))))

(deftest release-preflight-checks-its-opposite-guard
  (physical/with-proving-application!
    (fn [app]
      (is (= :operation-precondition-not-met
             (rejection #((:guard-preflight! app) :slot/release))))
      (is (= :committed (:commit/status ((:execute! app) :slot/reserve))))
      (let [before ((:read! app))
            {:keys [plan check!]} (prepared app :slot/release)]
        (limited! (check! plan) :slot/release)
        (limited! ((:guard-preflight! app) :slot/release) :slot/release)
        (unchanged! app before)
        (is (= :committed (:commit/status ((:execute! app) :slot/release))))
        (is (= :available (:slot/status ((:abstract! app)))))
        (is (= 2 (:revision ((:read! app)))))))))

(deftest weakened-or-duplicated-business-assertions-are-rejected
  (physical/with-proving-application!
    (fn [app]
      (let [{:keys [plan check!]} (prepared app :slot/reserve)
            original (first (:assertions plan))
            variants [(assoc plan :assertions [])
                      (assoc plan :assertions [original original])
                      (assoc plan :assertions [(tx/assert-one physical/table
                                                    [:= :status "available"])])
                      (assoc plan :assertions [(tx/assert-one physical/table
                                                    [:and [:= :xt/id (:xt/id ((:read! app)))]
                                                     [:= :status "available"]
                                                     [:= :claims 0]])])]]
        (doseq [candidate variants]
          (is (= mismatch (rejection #(check! candidate)))))
        (unchanged! app ((:read! app)))))))

(deftest changed-guard-parameters-and-targets-are-rejected
  (physical/with-proving-application!
    (fn [app]
      (let [before ((:read! app))
            {:keys [plan check!]} (prepared app :slot/reserve)
            variants [(assoc plan :assertions
                             [(tx/assert-one physical/table
                                [:and [:= :xt/id (:xt/id before)]
                                 [:= :status "reserved"] [:= :claims 0]
                                 [:= :audit-tag (:audit-tag before)]])])
                      (assoc plan :assertions
                             [(tx/assert-one physical/table
                                [:and [:= :xt/id "other-slot"]
                                 [:= :status "available"] [:= :claims 0]
                                 [:= :audit-tag (:audit-tag before)]])])
                      (assoc plan :assertions
                             [(tx/assert-one physical/table
                                [:and [:= :xt/id (:xt/id before)]
                                 [:= :status "available"] [:= :claims 1]
                                 [:= :audit-tag (:audit-tag before)]])])
                      (assoc plan :assertions
                             [(tx/assert-one physical/table
                                [:and [:= :xt/id (:xt/id before)]
                                 [:= :status "available"] [:= :claims 0]
                                 [:= :audit-tag "changed"]])])]]
        (doseq [candidate variants]
          (is (= mismatch (rejection #(check! candidate)))))
        (unchanged! app before)))))

(deftest changed-write-values-and-framed-fields-are-rejected
  (physical/with-proving-application!
    (fn [app]
      (let [before ((:read! app))
            {:keys [plan check!]} (prepared app :slot/reserve)
            variants [(assoc-in plan [:commands 0 :model/after :status] "available")
                      (assoc-in plan [:commands 0 :model/after :claims] 0)
                      (assoc-in plan [:commands 0 :model/after :audit-tag] "changed")
                      (assoc-in plan [:commands 0 :model/after :revision] 7)
                      (assoc-in plan [:commands 0 :model/after :updated-at] "changed")]]
        (doseq [candidate variants]
          (is (= mismatch (rejection #(check! candidate)))))
        (is (= :gesso.model.command/invalid-command
               (try (check! (assoc-in plan [:commands 0 :model/after :xt/id] "foreign"))
                    nil
                    (catch clojure.lang.ExceptionInfo e
                      (:error/type (ex-data e))))))
        (unchanged! app before)))))

(deftest changed-original-prestate-and-command-identity-are-rejected
  (physical/with-proving-application!
    (fn [app]
      (let [before ((:read! app))
            {:keys [plan check!]} (prepared app :slot/reserve)
            variants [(assoc-in plan [:commands 0 :model/before :revision] 21)
                      (assoc-in plan [:commands 0 :model/before :updated-at] "stale")
                      (assoc-in plan [:commands 0 :model/operation] :slot/release)]]
        (doseq [candidate variants]
          (is (= mismatch (rejection #(check! candidate)))))
        (is (= :gesso.model.command/invalid-command
               (try (check! (assoc-in plan [:commands 0 :model/id] "foreign"))
                    nil
                    (catch clojure.lang.ExceptionInfo e
                      (:error/type (ex-data e))))))
        (is (= :gesso.model.tx/missing-changes
               (try (check! (assoc plan :emit :async))
                    nil
                    (catch clojure.lang.ExceptionInfo e
                      (:error/type (ex-data e))))))
        (is (= :gesso.model.tx/unknown-plan-keys
               (try (check! (assoc plan :unexpected-flag true))
                    nil
                    (catch clojure.lang.ExceptionInfo e
                      (:error/type (ex-data e))))))
        (unchanged! app before)))))

(deftest mismatched-operation-and-prestate-cannot-pass-the-guard-check
  (physical/with-proving-application!
    (fn [app]
      (let [before ((:read! app))
            reserve-plan (#'physical/transition-plan before :slot/reserve)]
        (is (= mismatch
               (rejection #(#'physical/checked-guard-plan!
                            (:catalog app) (:biff-context app) before
                            :slot/release reserve-plan))))
        (is (= mismatch
               (rejection #(#'physical/checked-guard-plan!
                            (:catalog app) (:biff-context app)
                            (update before :revision inc)
                            :slot/reserve reserve-plan))))
        (unchanged! app before)))))

(deftest changed-source-guard-declaration-fails-before-any-transaction
  (physical/with-proving-application!
    (fn [app]
      (let [before ((:read! app))
            plan (#'physical/transition-plan before :slot/reserve)
            mutate (fn [catalog]
                     (update-in catalog [:operations 0 :construction :guard :expression]
                                (constantly [:= [:read :slot/status]
                                             [:literal :reserved]])))
            changed (mutate (:catalog app))]
        (is (= source-mismatch
               (rejection #(#'physical/checked-guard-plan!
                            changed (:biff-context app) before :slot/reserve plan))))
        (unchanged! app before)))))

(deftest substituted-catalog-constructor-fails-closed
  (physical/with-proving-application!
    (fn [app]
      (let [before ((:read! app))]
        (with-redefs [pilot/reservation-catalog (fn [] (:catalog app))]
          (is (= source-mismatch
                 (rejection #((:execute! app) :slot/reserve)))))
        (unchanged! app before)))))

(deftest substituted-transaction-lowerer-fails-closed
  (physical/with-proving-application!
    (fn [app]
      (let [before ((:read! app))]
        (with-redefs [tx/prepare (fn [& _] {:tx-ops []})]
          (is (= source-mismatch
                 (rejection #((:execute! app) :slot/reserve)))))
        (unchanged! app before)))))

(deftest substituted-typed-guard-evaluator-fails-closed
  (physical/with-proving-application!
    (fn [app]
      (let [before ((:read! app))]
        (with-redefs [typed/evaluate-expression! (fn [& _] {:value true})]
          (is (= source-mismatch
                 (rejection #((:execute! app) :slot/reserve)))))
        (unchanged! app before)))))

(deftest substituted-plan-builder-cannot-submit-a-weakened-plan
  (physical/with-proving-application!
    (fn [app]
      (let [before ((:read! app))
            original-builder (var-get #'physical/transition-plan)
            attempted-submissions (atom 0)]
        (with-redefs-fn
          {#'physical/transition-plan
           (fn [row op]
             (assoc (original-builder row op) :assertions []))
           #'tx/transact!
           (fn [& _] (swap! attempted-submissions inc)
             (throw (ex-info "Unexpected transaction submission" {})))}
          (fn []
            (is (= mismatch
                   (rejection #((:execute! app) :slot/reserve))))))
        (is (zero? @attempted-submissions))
        (unchanged! app before)))))

(deftest unknown-and-inapplicable-operations-do-not-submit
  (physical/with-proving-application!
    (fn [app]
      (let [before ((:read! app))
            submissions (atom 0)]
        (with-redefs [tx/transact! (fn [& _] (swap! submissions inc))]
          (is (= :unknown-operation
                 (rejection #((:execute! app) :other/operation))))
          (is (= :operation-precondition-not-met
                 (rejection #((:execute! app) :slot/release)))))
        (is (zero? @submissions))
        (unchanged! app before)))))

(deftest a-valid-preflight-is-not-an-authenticated-execution-witness
  (physical/with-proving-application!
    (fn [app]
      (let [before ((:read! app))
            report ((:guard-preflight! app) :slot/reserve)
            calls (atom 0)]
        (limited! report :slot/reserve)
        (with-redefs [tx/transact!
                      (fn [& _]
                        (swap! calls inc)
                        {:commit/status :committed :commit/transaction-key :forged})]
          (is (= :committed
                 (:commit/status ((:execute! app) :slot/reserve)))))
        (is (= 1 @calls))
        (unchanged! app before)
        (is (= :not-established (:executed-plan-binding report)))
        (is (= :not-issued (:certificate report)))))))

(deftest read-only-preflight-is-not-authentication-or-authorization
  (physical/with-proving-application!
    (fn [app]
      (let [before ((:read! app))
            result ((:guard-preflight! app) :slot/reserve)]
        (is (= :not-established (:principal-authenticity result)))
        (is (= :not-established (:complete-writer-coverage result)))
        (is (= :not-established (:physical-refinement result)))
        (is (= :not-authenticated (:xtdb-internal-prestate result)))
        (unchanged! app before)))))

(deftest two-actor-preparation-still-binds-the-same-guarded-plan
  (physical/with-two-actor-reservation!
    (fn [{:keys [prepared-tx-ops before-revision read! report]}]
      (is (zero? before-revision))
      (is (= (get prepared-tx-ops :actor/one)
             (get prepared-tx-ops :actor/two)))
      (is (= 3 (count (get prepared-tx-ops :actor/one))))
      (is (= :not-established (:physical-refinement report)))
      (is (zero? (:revision (read!)))))))

(deftest a-foreign-state-change-invalidates-the-original-guard
  (physical/with-proving-application!
    (fn [app]
      (let [before ((:read! app))
            plan (#'physical/transition-plan before :slot/reserve)]
        (is (true? (:source-guard-and-prepared-assertions-agree?
                    (#'physical/checked-guard-plan!
                     (:catalog app) (:biff-context app) before
                     :slot/reserve plan))))
        (xt/execute-tx (:node app)
                       [[:put-docs physical/table (assoc before :audit-tag "modified")]])
        (is (thrown? xtdb.error.Conflict
                     (tx/transact! (:biff-context app) plan)))
        (is (= "modified" (:audit-tag ((:read! app)))))
        (is (zero? (:revision ((:read! app))))))))
)
