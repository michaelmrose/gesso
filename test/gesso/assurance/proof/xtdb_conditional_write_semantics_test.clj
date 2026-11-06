(ns gesso.assurance.proof.xtdb-conditional-write-semantics-test
  "v1512: Permanent adversarial correspondence checks for v1511's restricted
   XTDB ASSERT/ASSERT/PUT semantics and the real Gesso reservation fixture.

   In particular, an unguarded persisted field can be overwritten by a
   successful stale put-docs replacement. The suite independently observes
   that outcome in embedded XTDB, tests conflicting conditional submissions,
   and demonstrates that guarding the extra field prevents that overwrite.
   These are bounded physical experiments. Neither the model nor matching
   executions authenticate XTDB's internal transaction snapshot, identify
   all external writers, establish principal ownership, or prove refinement."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.proof.xtdb-conditional-write-semantics :as semantics]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(def ^:private reserve :slot/reserve)
(def ^:private release :slot/release)
(def ^:private guarded-fields [:status :claims :audit-tag])
(def ^:private cas-fields [:revision :updated-at])

(defn- plan-and-spec
  ([app operation]
   (plan-and-spec app operation ((:read! app))))
  ([app operation before]
   (let [plan (#'physical/transition-plan before operation)
         after (get-in plan [:commands 0 :model/after])]
     {:plan plan
      :spec {:table physical/table
             :before before
             :after after
             :guard-fields guarded-fields
             :cas-fields cas-fields
             :tx-ops (:tx-ops (tx/prepare (:biff-context app) plan))}})))

(defn- rejected-model-reason [f]
  (try
    (f)
    :not-rejected
    (catch clojure.lang.ExceptionInfo e
      (if (:xtdb-conditional-write-semantics/rejection (ex-data e))
        (:reason (ex-data e))
        (throw e)))))

(defn- tx-outcome [f]
  (try
    (let [result (f)]
      (if (= :committed (:commit/status result))
        :committed
        [:unexpected-result (:commit/status result)]))
    (catch Throwable e
      (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data e)))
        :assert-failed
        (throw e)))))

(defn- put-foreign! [app row]
  ;; Bypass Gesso deliberately. This is an *uncertified* external writer.
  (xt/execute-tx (:node app) [[:put-docs physical/table row]]))

(defn- limited-model! [report]
  (is (= semantics/report-type (:type report)))
  (is (true? (:canonical-shape-matches? report)))
  (is (true? (:all-assertions-precede-write? report)))
  (is (= :not-authenticated (:physical-prestate report)))
  (is (= :not-established (:executed-plan-binding report)))
  (is (= :not-established (:external-writer-coverage report)))
  (is (= :not-established (:physical-frame-preservation report)))
  (is (= :not-established (:universal-refinement report)))
  (is (= :not-proved (:proof-status report)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :none (:authority report)))
  (doseq [sensitive [:before :after :tx-ops :slot-id :xt/id :tx-key
                     :statement-parameters :current-row :sql]]
    (is (not (contains? report sensitive)) (str "Redacted: " sensitive))))

(deftest canonical-reservation-and-release-are-both-partially-guarded
  (physical/with-proving-application!
   (fn [app]
     (let [{s :spec} (plan-and-spec app reserve)
           before (:before s)
           report (semantics/inspect-plan! s)
           predicted (semantics/model-step! s before)]
       (limited-model! report)
       (is (= #{:created-at} (:unguarded-persisted-fields report)))
       (is (false? (:complete-declared-row-compared? report)))
       (is (= :would-commit (:modeled-outcome predicted)))
       (is (= (:after s) (:next-document predicted)))
       (is (= :not-observed (:physical-outcome predicted)))
       (is (= before ((:read! app))) "Inspection never submits a transaction")
       (is (= :committed (tx-outcome #((:execute! app) reserve))))
       (let [{r :spec} (plan-and-spec app release)]
         (limited-model! (semantics/inspect-plan! r))
         (is (= #{:created-at}
                (:unguarded-persisted-fields (semantics/inspect-plan! r))))
         (is (= :would-commit
                (:modeled-outcome (semantics/model-step! r (:before r)))))
         (is (= :committed (tx-outcome #((:execute! app) release))))
         (is (= 2 (:revision ((:read! app))))))))))

(deftest noncanonical-sql-parameters-write-and-order-fail-closed
  (physical/with-proving-application!
   (fn [app]
     (let [{s :spec} (plan-and-spec app reserve)
           ops (:tx-ops s)
           variants [(assoc-in ops [0 1] 0)
                     (assoc-in ops [0 2] "wrong-slot")
                     (assoc-in ops [0 3] "reserved")
                     (assoc-in ops [1 3] 99)
                     (assoc-in ops [2 2 :claims] 4)
                     (assoc-in ops [2 2 :created-at] "silently-altered")
                     (vec (reverse ops))
                     (assoc-in ops [0 0] "ASSERT TRUE")]
           wrong-arity [(conj ops [:delete-docs physical/table "other"])
                        (subvec ops 1)]]
       (doseq [candidate variants]
         (is (= :noncanonical-or-reordered-conditional-write
                (rejected-model-reason
                 #(semantics/inspect-plan! (assoc s :tx-ops candidate))))))
       (doseq [candidate wrong-arity]
         (is (= :invalid-restricted-plan-input
                (rejected-model-reason
                 #(semantics/inspect-plan! (assoc s :tx-ops candidate))))))
       (is (= (:before s) ((:read! app))))))))

(deftest malformed-or-overclaimed-model-inputs-are-rejected
  (physical/with-proving-application!
   (fn [app]
     (let [{s :spec} (plan-and-spec app reserve)
           inputs [(assoc s :unauthorized :certified)
                   (assoc s :table :bad/table)
                   (assoc s :guard-fields [:status :status])
                   (assoc s :guard-fields [])
                   (assoc s :cas-fields [:revision :status])
                   (assoc-in s [:before :claims] {:bad :nested})
                   (assoc-in s [:after :xt/id] "other")
                   (assoc-in s [:after :created-at] nil)
                   (update s :after dissoc :created-at)
                   (assoc s :tx-ops nil)]]
       (doseq [candidate inputs]
         (is (= :invalid-restricted-plan-input
                (rejected-model-reason
                 #(semantics/inspect-plan! candidate)))))
       (is (= (:before s) ((:read! app))))))))

(deftest model-fails-on-business-and-cas-guard-divergence
  (physical/with-proving-application!
   (fn [app]
     (let [{s :spec} (plan-and-spec app reserve)
           before (:before s)]
       (doseq [changed [(assoc before :status "reserved")
                        (assoc before :claims 1)
                        (assoc before :audit-tag "foreign")
                        (update before :revision inc)
                        (assoc before :updated-at "foreign")
                        nil]]
         (let [modeled (semantics/model-step! s changed)]
           (is (= :assertion-would-fail (:modeled-outcome modeled)))
           (is (= changed (:next-document modeled)))
           (is (= #{} (:overwritten-unguarded-fields modeled)))
           (is (= :not-observed (:physical-outcome modeled)))))
       (is (= before ((:read! app))))))))

(deftest missing-and-invalid-current-state-never-produce-a-physical-success
  (physical/with-proving-application!
   (fn [app]
     (let [{s :spec} (plan-and-spec app reserve)]
       (doseq [current [{:xt/id "only-id"}
                        (assoc (:before s) :created-at [:unsupported])
                        (with-meta (:before s) {:forged true})]]
         (is (= :unsupported-current-document
                (rejected-model-reason #(semantics/model-step! s current)))))
       (is (= :assertion-would-fail
              (:modeled-outcome (semantics/model-step! s nil))))
       (is (= 0 (:revision ((:read! app)))))))))

(deftest unguarded-new-field-and-unguarded-modification-are-modeled-as-overwrites
  (physical/with-proving-application!
   (fn [app]
     (let [{s :spec} (plan-and-spec app reserve)
           before (:before s)
           changed (assoc before :created-at "other-created" :foreign-note "new")
           modeled (semantics/model-step! s changed)]
       (is (= :would-commit (:modeled-outcome modeled)))
       (is (= #{:created-at :foreign-note}
              (:overwritten-unguarded-fields modeled)))
       (is (= (:after s) (:next-document modeled)))
       (is (not (contains? (:next-document modeled) :foreign-note)))
       (is (= :not-proved (:proof-status modeled)))
       (is (= :not-issued (:certificate modeled)))
       (is (= before ((:read! app))))))))

(deftest actual-xtdb-accepts-unchanged-prestate-and-matches-bounded-model
  (physical/with-proving-application!
   (fn [app]
     (let [{:keys [plan spec]} (plan-and-spec app reserve)
           modeled (semantics/model-step! spec (:before spec))]
       (is (= :would-commit (:modeled-outcome modeled)))
       (is (= :committed
              (tx-outcome #(tx/transact! (:biff-context app) plan))))
       (is (= (:next-document modeled) ((:read! app))))
       (is (= :reserved (:slot/status ((:abstract! app)))))
       (is (= :not-observed (:physical-outcome modeled))
           "A pure model must not claim it executed a physical transaction")))))

(deftest actual-xtdb-rejects-intervening-business-guard-change
  (physical/with-proving-application!
   (fn [app]
     (let [{:keys [plan spec]} (plan-and-spec app reserve)
           changed (assoc (:before spec) :audit-tag "foreign")]
       (is (= :assertion-would-fail
              (:modeled-outcome (semantics/model-step! spec changed))))
       (put-foreign! app changed)
       (is (= :assert-failed
              (tx-outcome #(tx/transact! (:biff-context app) plan))))
       (is (= changed ((:read! app)))
           "The failed prewrite assertion must not replace the foreign state")
       (is (= 0 (:revision ((:read! app)))))))))

(deftest actual-xtdb-rejects-intervening-version-change
  (physical/with-proving-application!
   (fn [app]
     (let [{:keys [plan spec]} (plan-and-spec app reserve)
           changed (update (:before spec) :revision inc)]
       (is (= :assertion-would-fail
              (:modeled-outcome (semantics/model-step! spec changed))))
       (put-foreign! app changed)
       (is (= :assert-failed
              (tx-outcome #(tx/transact! (:biff-context app) plan))))
       (is (= changed ((:read! app))))))))

(deftest real-unguarded-created-at-change-is-lost-despite-successful-asserts
  (physical/with-proving-application!
   (fn [app]
     (let [{:keys [plan spec]} (plan-and-spec app reserve)
           original (:before spec)
           changed (assoc original :created-at "external-created-at")
           modeled (semantics/model-step! spec changed)]
       (is (= :would-commit (:modeled-outcome modeled)))
       (is (= #{:created-at} (:overwritten-unguarded-fields modeled)))
       (put-foreign! app changed)
       (is (= "external-created-at" (:created-at ((:read! app)))))
       (is (= :committed
              (tx-outcome #(tx/transact! (:biff-context app) plan))))
       (is (= (:after spec) ((:read! app))))
       (is (= (:created-at original) (:created-at ((:read! app)))))
       (is (not= (:created-at changed) (:created-at ((:read! app)))))
       (is (= :reserved (:slot/status ((:abstract! app)))))
       (is (= :not-established
              (:physical-frame-preservation (semantics/inspect-plan! spec))))))))

(deftest real-new-unguarded-field-disappears-on-full-document-replacement
  (physical/with-proving-application!
   (fn [app]
     (let [{:keys [plan spec]} (plan-and-spec app reserve)
           changed (assoc (:before spec) :external-flag "foreign")
           modeled (semantics/model-step! spec changed)]
       (is (= #{:external-flag} (:overwritten-unguarded-fields modeled)))
       (put-foreign! app changed)
       (is (= "foreign" (:external-flag ((:read! app)))))
       (is (= :committed
              (tx-outcome #(tx/transact! (:biff-context app) plan))))
       (is (not (contains? ((:read! app)) :external-flag)))
       (is (= (:after spec) ((:read! app))))))))

(deftest guarding-created-at-rejects-the-same-foreign-change
  (physical/with-proving-application!
   (fn [app]
     (let [{:keys [plan spec]} (plan-and-spec app reserve)
           before (:before spec)
           old-assert (first (:assertions plan))
           ;; This is an experimental STRONGER assertion, not a change to
           ;; the production source-derived guard.
           guard (tx/assert-one
                  physical/table
                  [:and [:= :xt/id (:xt/id before)]
                   [:= :status (:status before)]
                   [:= :claims (:claims before)]
                   [:= :audit-tag (:audit-tag before)]
                   [:= :created-at (:created-at before)]])
           stronger-plan (assoc plan :assertions [guard])
           stronger-spec (-> spec
                            (assoc :guard-fields
                                   (conj guarded-fields :created-at))
                            (assoc :tx-ops
                                   (:tx-ops (tx/prepare (:biff-context app)
                                                        stronger-plan))))
           report (semantics/inspect-plan! stronger-spec)
           changed (assoc before :created-at "external-created-at")]
       (is (not= old-assert guard))
       (limited-model! report)
       (is (true? (:complete-declared-row-compared? report)))
       (is (empty? (:unguarded-persisted-fields report)))
       (is (= :assertion-would-fail
              (:modeled-outcome (semantics/model-step! stronger-spec changed))))
       (put-foreign! app changed)
       (is (= :assert-failed
              (tx-outcome #(tx/transact! (:biff-context app) stronger-plan))))
       (is (= changed ((:read! app))))))))

(deftest external-writer-to-another-document-is-not-covered-by-model
  (physical/with-proving-application!
   (fn [app]
     (let [{:keys [plan spec]} (plan-and-spec app reserve)
           foreign-id (str "v1512-foreign-" (random-uuid))
           foreign (physical/initial-document foreign-id)]
       (put-foreign! app foreign)
       (is (= foreign
              (first (xt/q (:node app)
                           ["SELECT * FROM gesso_v1494_reservation_slots WHERE _id = ?"
                            foreign-id]))))
       (is (= :would-commit
              (:modeled-outcome (semantics/model-step! spec (:before spec)))))
       (is (= :committed
              (tx-outcome #(tx/transact! (:biff-context app) plan))))
       (is (= foreign
              (first (xt/q (:node app)
                           ["SELECT * FROM gesso_v1494_reservation_slots WHERE _id = ?"
                            foreign-id]))))
       (is (= :not-established
              (:external-writer-coverage (semantics/inspect-plan! spec))))))))

(deftest two-concurrent-identical-guarded-plans-cannot-both-commit
  (physical/with-proving-application!
   (fn [app]
     (let [{:keys [plan spec]} (plan-and-spec app reserve)
           gate (CountDownLatch. 1)
           start (fn [] (future
                         (when-not (.await gate 15 TimeUnit/SECONDS)
                           (throw (ex-info "Race gate timed out." {})))
                         (tx-outcome #(tx/transact! (:biff-context app) plan))))
           worker-a (start)
           worker-b (start)]
       (try
         (.countDown gate)
         (let [a (deref worker-a 45000 ::timed-out)
               b (deref worker-b 45000 ::timed-out)]
           (is (not= ::timed-out a))
           (is (not= ::timed-out b))
           (is (= #{:committed :assert-failed} (set [a b])))
           (is (= 1 (:revision ((:read! app)))))
           (is (= :reserved (:slot/status ((:abstract! app)))))
           (is (= (:after spec) ((:read! app)))))
         (finally
           (.countDown gate)
           (when (= ::timed-out (deref worker-a 10 ::timed-out))
             (future-cancel worker-a))
           (when (= ::timed-out (deref worker-b 10 ::timed-out))
             (future-cancel worker-b))))))))

(deftest model-matches-rejected-prepared-plan-after-first-commit
  (physical/with-proving-application!
   (fn [app]
     (let [{:keys [plan spec]} (plan-and-spec app reserve)]
       (is (= :committed
              (tx-outcome #(tx/transact! (:biff-context app) plan))))
       (let [current ((:read! app))
             modeled (semantics/model-step! spec current)]
         (is (= :assertion-would-fail (:modeled-outcome modeled)))
         (is (= current (:next-document modeled)))
         (is (= :assert-failed
                (tx-outcome #(tx/transact! (:biff-context app) plan))))
         (is (= current ((:read! app))))
         (is (= :not-authenticated (:physical-prestate modeled))))))))
