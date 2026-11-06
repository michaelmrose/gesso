(ns gesso.assurance.example.physical-single-node-concurrency-test
  "v1502: permanent, adversarial checks of v1501's TWO-ACTOR physical
   reservation fixture against a real embedded XTDB2 node.

   Both Gesso model transactions are prepared from the same observed state.
   The assertions test guarded rejection, one-shot submission, concurrent
   contention, persisted at-most-one-claim, and preservation of the audit
   field. These tests do NOT establish authenticated actor ownership,
   application-wide writer closure, commit provenance, a certified executor,
   or universal serializability/physical refinement."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.example.single-node-proving :as pilot]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(def ^:private actor-one :actor/one)
(def ^:private actor-two :actor/two)
(def ^:private actors #{actor-one actor-two})

(defn- reason [f]
  (try (f) :not-rejected
       (catch clojure.lang.ExceptionInfo e
         (if (:physical-single-node-proving/rejection (ex-data e))
           (:reason (ex-data e))
           (throw e)))))

(defn- outcome [f]
  (try
    (let [result (f)]
      (if (= :committed (:commit/status result))
        :committed
        [:unexpected-result (:commit/status result)]))
    (catch Throwable e
      (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data e)))
        :assert-failed
        (throw e)))))

(defn- assert-available! [read! abstract!]
  (let [row (read!)
        abstract (abstract!)]
    (is (= "available" (:status row)))
    (is (zero? (:claims row)))
    (is (zero? (:revision row)))
    (is (= :available (:slot/status abstract)))
    (is (zero? (:slot/active-claims abstract)))
    (is (= "preserved" (:audit-tag row)))))

(defn- assert-one-claim! [read! abstract!]
  (let [row (read!)
        abstract (abstract!)]
    (is (= "reserved" (:status row)))
    (is (= 1 (:claims row)))
    (is (= 1 (:revision row)))
    (is (= :reserved (:slot/status abstract)))
    (is (= 1 (:slot/active-claims abstract)))
    (is (= :preserved (:slot/audit-tag abstract)))
    (is (= "preserved" (:audit-tag row)))
    (is (= "v1494-created" (:created-at row)))
    (is (= "v1494-revision-1" (:updated-at row)))
    ;; The labels are deliberately not persisted owner/principal identities.
    (is (not (contains? row :owner)))
    (is (not (contains? row :actor)))))

(defn- assert-unproven! [report]
  (is (= :gesso.assurance.example.physical-single-node-proving/two-actor-scenario
         (:type report)))
  (is (= 1 (:version report)))
  (is (= :slot/reserve (:operation-id report)))
  (is (= (:id (pilot/reservation-catalog)) (:catalog-id report)))
  (is (= actors (:actor-ids report)))
  (is (= :diagnostic-label-only (:actor-identity report)))
  (is (true? (:prepared-from-one-observed-state? report)))
  (is (true? (:one-attempt-per-actor? report)))
  (is (= :not-established (:complete-writer-coverage report)))
  (is (= :not-established (:authorization report)))
  (is (= :not-authenticated (:transaction-internal-prestate report)))
  (is (= :not-established (:physical-refinement report)))
  (is (= :not-proved (:proof-status report)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :none (:authority report)))
  (is (not (contains? report :slot-id))))

(defn- race-once! [iteration first-actor second-actor]
  (physical/with-two-actor-reservation!
   (str "v1502-race-" iteration "-" (random-uuid))
   (fn [{:keys [submit! attempt-state! read! abstract! report]}]
     (let [ready (CountDownLatch. 2)
           go (CountDownLatch. 1)
           worker (fn [actor]
                    (future
                      (.countDown ready)
                      (when-not (.await go 15 TimeUnit/SECONDS)
                        (throw (ex-info "Worker never received start signal."
                                        {:actor actor :iteration iteration})))
                      (outcome #(submit! actor))))
           a (worker first-actor)
           b (worker second-actor)]
       (try
         (is (.await ready 15 TimeUnit/SECONDS)
             "Both workers must be ready before concurrent submission")
         (.countDown go)
         (let [results [(deref a 40000 ::timed-out)
                        (deref b 40000 ::timed-out)]]
           (is (= #{:committed :assert-failed} (set results))
               (str "one commit and one assertion failure: " (pr-str results)))
           (is (= 2 (count results)))
           (is (= {actor-one :attempted actor-two :attempted}
                  (attempt-state!)))
           (assert-one-claim! read! abstract!)
           (assert-unproven! report))
         (finally
           (.countDown go)
           ;; Never close XTDB while submitted workers remain live.
           (let [ra (deref a 40000 ::timed-out)
                 rb (deref b 40000 ::timed-out)]
             (when (or (= ::timed-out ra) (= ::timed-out rb))
               (future-cancel a)
               (future-cancel b)
               (throw (ex-info "Concurrent XTDB submissions did not finish."
                               {:iteration iteration}))))))))))

(deftest both-actor-plans-share-the-initial-observation
  (physical/with-two-actor-reservation!
   (fn [{:keys [actor-ids prepared-tx-ops before-revision before-abstract
                attempt-state! read! abstract! report]}]
     (is (= actors actor-ids))
     (is (zero? before-revision))
     (is (= {:slot/status :available
             :slot/active-claims 0
             :slot/audit-tag :preserved} before-abstract))
     (is (= actors (set (keys prepared-tx-ops))))
     (is (= (get prepared-tx-ops actor-one)
            (get prepared-tx-ops actor-two)))
     (is (seq (get prepared-tx-ops actor-one)))
     (is (= {actor-one :prepared actor-two :prepared} (attempt-state!)))
     (assert-available! read! abstract!)
     (assert-unproven! report))))

(deftest prepared-plan-contains-assertions-before-physical-writes
  (physical/with-two-actor-reservation!
   (fn [{:keys [prepared-tx-ops read!]}]
     (let [ops (get prepared-tx-ops actor-one)
           verbs (mapv first ops)
           assert-indices (keep-indexed (fn [i op] (when (and (vector? op)
                                                   (string? (first op))
                                                   (re-find #"(?i)^ASSERT\b" (first op)))
                                          i)) ops)
           write-indices (keep-indexed (fn [i op] (when (#{:put-docs :delete-docs} (first op)) i)) ops)]
       (is (seq assert-indices))
       (is (seq write-indices))
       (is (< (apply max assert-indices) (apply min write-indices)))
       (is (= verbs (mapv first (get prepared-tx-ops actor-two))))
       (is (= 0 (:revision (read!)))
           "Inspecting prepared operations must not submit a transaction")))))

(deftest first-actor-wins-second-rejected
  (physical/with-two-actor-reservation!
   (fn [{:keys [submit! read! abstract! attempt-state!]}]
     (is (= :committed (outcome #(submit! actor-one))))
     (is (= :assert-failed (outcome #(submit! actor-two))))
     (assert-one-claim! read! abstract!)
     (is (= {actor-one :attempted actor-two :attempted} (attempt-state!))))))

(deftest second-actor-wins-first-rejected
  (physical/with-two-actor-reservation!
   (fn [{:keys [submit! read! abstract! attempt-state!]}]
     (is (= :committed (outcome #(submit! actor-two))))
     (is (= :assert-failed (outcome #(submit! actor-one))))
     (assert-one-claim! read! abstract!)
     (is (= {actor-one :attempted actor-two :attempted} (attempt-state!))))))

(deftest one-submission-leaves-the-other-actor-prepared
  (physical/with-two-actor-reservation!
   (fn [{:keys [submit! attempt-state! read! abstract!]}]
     (is (= :committed (outcome #(submit! actor-one))))
     (is (= {actor-one :attempted actor-two :prepared} (attempt-state!)))
     (assert-one-claim! read! abstract!)
     (is (= :assert-failed (outcome #(submit! actor-two))))
     (is (= {actor-one :attempted actor-two :attempted} (attempt-state!))))))

(deftest invalid-actor-cannot-consume-attempt
  (physical/with-two-actor-reservation!
   (fn [{:keys [submit! attempt-state! read! abstract!]}]
     (doseq [actor [nil :actor/three "actor/one" :slot/reserve]]
       (is (= :unknown-competing-actor (reason #(submit! actor)))))
     (is (= {actor-one :prepared actor-two :prepared} (attempt-state!)))
     (assert-available! read! abstract!))))

(deftest one-actor-cannot-submit-twice-even-after-commit
  (physical/with-two-actor-reservation!
   (fn [{:keys [submit! attempt-state! read! abstract!]}]
     (is (= :committed (outcome #(submit! actor-one))))
     (dotimes [_ 3]
       (is (= :competing-actor-already-attempted
              (reason #(submit! actor-one)))))
     (is (= {actor-one :attempted actor-two :prepared} (attempt-state!)))
     (assert-one-claim! read! abstract!))))

(deftest failed-guard-consumes-attempt-and-cannot-be-retried
  (physical/with-two-actor-reservation!
   (fn [{:keys [submit! attempt-state! read! abstract!]}]
     (is (= :committed (outcome #(submit! actor-two))))
     (is (= :assert-failed (outcome #(submit! actor-one))))
     (dotimes [_ 3]
       (is (= :competing-actor-already-attempted
              (reason #(submit! actor-one)))))
     (is (= {actor-one :attempted actor-two :attempted} (attempt-state!)))
     (assert-one-claim! read! abstract!))))

(deftest failed-submission-does-not-trigger-second-attempt
  (physical/with-two-actor-reservation!
   (fn [{:keys [submit! attempt-state! read! abstract!]}]
     (let [calls (atom 0)
           original @#'tx/transact!]
       (with-redefs [tx/transact!
                    (fn [& args]
                      (swap! calls inc)
                      (throw (ex-info "Simulated submission failure."
                                      {:v1502/test-failure true
                                       :args-count (count args)})))]
         (is (thrown? clojure.lang.ExceptionInfo
                      (submit! actor-one)))
         (is (= :competing-actor-already-attempted
                (reason #(submit! actor-one))))
         (is (= 1 @calls)))
       (is (= 1 @calls))
       (is (identical? original @#'tx/transact!))
       (is (= {actor-one :attempted actor-two :prepared} (attempt-state!)))
       (assert-available! read! abstract!)))))

(deftest actual-transaction-call-count-stays-two
  (physical/with-two-actor-reservation!
   (fn [{:keys [submit! read! abstract!]}]
     (let [calls (atom 0)
           real-tx @#'tx/transact!]
       (with-redefs [tx/transact!
                    (fn [& args]
                      (swap! calls inc)
                      (apply real-tx args))]
         (is (= :committed (outcome #(submit! actor-one))))
         (is (= :assert-failed (outcome #(submit! actor-two))))
         (is (= :competing-actor-already-attempted
                (reason #(submit! actor-one))))
         (is (= :competing-actor-already-attempted
                (reason #(submit! actor-two)))))
       (is (= 2 @calls))
       (assert-one-claim! read! abstract!)))))

(deftest source-plan-description-cannot-change-submitted-plan
  (physical/with-two-actor-reservation!
   (fn [{:keys [submit! prepared-tx-ops read! abstract!]}]
     ;; Copying/replacing externally returned data cannot change the
     ;; closed-over plans that the fixture actually submits.
     (let [changed (assoc prepared-tx-ops actor-one [[:delete-docs physical/table "fake"]])]
       (is (not= changed prepared-tx-ops))
       (is (= :committed (outcome #(submit! actor-one))))
       (assert-one-claim! read! abstract!)))))

(deftest foreign-write-before-submission-invalidates-both-guarded-plans
  (physical/with-two-actor-reservation!
   (fn [{:keys [submit! read! attempt-state! abstract!]}]
     ;; Simulate a raw writer *outside* either planned Gesso command, just
     ;; before the first actor's transaction reaches the XTDB adapter.
     (let [calls (atom 0)
           real-tx @#'tx/transact!]
       (with-redefs [tx/transact!
                    (fn [ctx plan]
                      (when (= 1 (swap! calls inc))
                        (xt/execute-tx (:biff.xtdb/node ctx)
                                       [[:put-docs physical/table
                                         (assoc (read!) :audit-tag "changed")]]))
                      (real-tx ctx plan))]
         (is (= :assert-failed (outcome #(submit! actor-one))))
         (is (= :assert-failed (outcome #(submit! actor-two)))))
       (is (= 2 @calls))
       (is (= {actor-one :attempted actor-two :attempted} (attempt-state!)))
       (is (zero? (:revision (read!))))
       (is (= "changed" (:audit-tag (read!))))
       (is (= :available (:slot/status (abstract!))))
       (is (zero? (:slot/active-claims (abstract!))))))))

(deftest simultaneous-different-actors-one-commit-one-conflict
  (doseq [n (range 4)]
    (race-once! n actor-one actor-two)))

(deftest simultaneous-actors-opposite-thread-creation-order
  (doseq [n (range 2)]
    (race-once! (+ n 100) actor-two actor-one)))

(deftest simultaneous-duplicate-submission-of-one-actor-is-at-most-once
  (physical/with-two-actor-reservation!
   (fn [{:keys [submit! attempt-state! read! abstract!]}]
     (let [start (CountDownLatch. 1)
           worker (fn [] (future
                           (.await start 10 TimeUnit/SECONDS)
                           (try (outcome #(submit! actor-one))
                                (catch clojure.lang.ExceptionInfo e
                                  (if (:physical-single-node-proving/rejection (ex-data e))
                                    (:reason (ex-data e))
                                    (throw e))))))
           a (worker)
           b (worker)]
       (try
         (.countDown start)
         (let [results [(deref a 40000 ::timed-out) (deref b 40000 ::timed-out)]]
           (is (= #{:committed :competing-actor-already-attempted}
                  (set results)))
           (is (= {actor-one :attempted actor-two :prepared} (attempt-state!)))
           (assert-one-claim! read! abstract!))
         (finally (.countDown start)
                  (let [ra (deref a 40000 ::timed-out)
                        rb (deref b 40000 ::timed-out)]
                    (when (or (= ::timed-out ra) (= ::timed-out rb))
                      (future-cancel a)
                      (future-cancel b)
                      (throw (ex-info "Duplicate submissions hung."
                                      {}))))))))))

(deftest explicit-slot-id-is-used-but-not-evidence-of-authorized-principal
  (physical/with-two-actor-reservation!
   "v1502-explicit-slot"
   (fn [{:keys [submit! read! report]}]
     (is (= "v1502-explicit-slot" (:xt/id (read!))))
     (is (= :diagnostic-label-only (:actor-identity report)))
     (is (= :not-established (:authorization report)))
     (is (= :committed (outcome #(submit! actor-two))))
     (is (= "v1502-explicit-slot" (:xt/id (read!)))))))

(deftest invalid-callbacks-and-slot-ids-rejected-before-startup
  (is (= :invalid-scenario-callback
         (reason #(physical/with-two-actor-reservation! nil))))
  (is (= :unsupported-slot-id
         (reason #(physical/with-two-actor-reservation! "bad/id" identity))))
  (is (= :unsupported-slot-id
         (reason #(physical/with-two-actor-reservation! "" identity)))))
