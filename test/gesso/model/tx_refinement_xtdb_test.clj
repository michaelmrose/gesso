(ns gesso.model.tx-refinement-xtdb-test
  "Permanent controlled-XTDB regressions for observe-and-compare!.

   Exercises the actual collector: it independently reads the declared target
   domain, constructs and submits canonical tx/prepare operations, classifies
   the resulting XTDB outcome, and compares poststate to the bounded serial
   reference model. Tests intentionally distinguish invalid input rejected
   *before* any physical I/O from real assertion rejection after submission.

   A successful report is diagnostic evidence for this controlled execution,
   not an authenticated XTDB receipt or a general refinement theorem. The
   fixture assumes exclusive control of its embedded node and target IDs."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.model.command :as command]
   [gesso.model.tx :as tx]
   [gesso.model.tx-refinement-xtdb :as collector]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(defonce ^:private !node (atom nil))

(defn- node []
  (or @!node
      (throw (ex-info "The embedded XTDB fixture is not running." {}))))

(defn- with-real-xtdb [f]
  (with-open [db (xtn/start-node {})]
    (reset! !node db)
    (try (f)
         (finally (reset! !node nil)))))

(use-fixtures :once with-real-xtdb)

(defn- request [prefix]
  {:xt/id (str prefix "-" (random-uuid))
   :revision 0
   :created-at "t0"
   :updated-at "t0"
   :owner "unclaimed"})

(defn- membership [prefix]
  {:xt/id (str prefix "-" (random-uuid))
   :revision 0
   :created-at "t0"
   :updated-at "t0"
   :active true})

(defn- advance [document changes]
  (-> document
      (merge changes)
      (update :revision inc)
      (assoc :updated-at "t1")))

(defn- plan [commands & guards]
  {:commands (vec commands)
   :guards (vec guards)
   :emit false})

(defn- claim [document owner]
  (command/update-command
   :requests :claim document (advance document {:owner owner}) version))

(defn- suspend [document]
  (command/update-command
   :memberships :suspend document (advance document {:active false}) version))

(defn- seed! [table document]
  (xt/execute-tx (node) [[:put-docs table document]])
  document)

(defn- physical-document [table id]
  (let [table-name (case table
                     :requests "requests"
                     :memberships "memberships")
        rows (xt/q (node) [(str "SELECT * FROM " table-name " WHERE _id = ?") id])]
    (is (<= (count rows) 1) "A target must identify at most one XTDB row.")
    (first rows)))

(defn- collect! [a-plan targets]
  (collector/observe-and-compare! {:node (node)
                                   :plan a-plan
                                   :targets targets}))

(defn- error-type [thunk]
  (try
    (thunk)
    nil
    (catch clojure.lang.ExceptionInfo e
      (:error/type (ex-data e)))))

(defn- check-report! [report status]
  (is (= status (get-in report [:execution :status])))
  (is (true? (get-in report [:comparison :corresponds?]))
      (pr-str (get-in report [:comparison :checks])))
  (is (every? true? (vals (get-in report [:comparison :checks]))))
  (is (= :direct-controlled-xtdb-observation (:evidence-kind report)))
  (is (false? (:certifies-physical-execution? report)))
  (is (false? (get-in report [:comparison :certifies-physical-execution?])))
  (is (not (contains? report :recorded-tx-ops)))
  (is (not (contains? report :observed-before)))
  (is (not (contains? report :observed-after)))
  (is (not (contains? report :reference-before)))
  (is (not (contains? report :plan)))
  report)

(deftest collector-scope-never-promotes-observation-to-certification-test
  (let [{:keys [collector collected assumed not-established] :as scope}
        collector/collection-scope]
    (is (= :gesso.model.tx/direct-xtdb-step-observation-v1 collector))
    (is (= 4 (count (select-keys scope
                                 [:collector :collected :assumed :not-established]))))
    (doseq [claim [:reference-prestate-from-physical-reads
                   :complete-explicit-target-reads
                   :operations-from-production-tx-prepare
                   :same-operations-passed-to-xtdb-execute-tx
                   :definitive-xtdb-outcome-classification
                   :physical-poststate-reads
                   :recorded-step-comparison]]
      (is (contains? collected claim)))
    (doseq [assumption [:exclusive-control-of-tracked-documents-during-collection
                        :all-semantic-read-and-write-dependencies-identified
                        :consistent-series-of-separate-xtdb-reads
                        :supplied-node-is-the-intended-controlled-database]]
      (is (contains? assumed assumption)))
    (doseq [unproved [:authenticated-physical-submission-receipt
                      :production-live-execution-correspondence
                      :one-pinned-snapshot-across-all-target-reads
                      :physical-read-completeness-outside-declared-targets
                      :absence-of-external-writers
                      :general-xtdb-step-refinement
                      :safe-unknown-outcome-reconciliation
                      :authorization-and-business-invariants
                      :durable-command-incorporation-and-replay
                      :browser-or-choreography-correspondence]]
      (is (contains? not-established unproved))
      (is (not (contains? collected unproved))))))

(deftest physical-claim-and-stale-retry-correspond-test
  (let [req (seed! :requests (request "collected-claim"))
        member (seed! :memberships (membership "collected-member"))
        rt [:requests (:xt/id req)]
        mt [:memberships (:xt/id member)]
        action (plan [(claim req "helper-a")]
                     (command/guard :memberships member version))
        before (physical-document :requests (:xt/id req))
        accepted (check-report! (collect! action [rt mt]) :committed)
        after (physical-document :requests (:xt/id req))
        retry (check-report! (collect! action [rt mt]) :rejected)]
    (is (= "unclaimed" (:owner before)))
    (is (= "helper-a" (:owner after)))
    (is (= 1 (:revision after)))
    (is (= :xtdb/assert-failed (get-in retry [:execution :error/code])))
    (is (= :command-version
           (get-in retry [:comparison :failed-precondition-kind])))
    (is (= after (physical-document :requests (:xt/id req))))
    (is (= 2 (:collected-target-count accepted)))
    (is (pos? (:submitted-operation-count accepted)))
    (is (= (:submitted-operation-count accepted)
           (:submitted-operation-count retry)))
    (is (true? (get-in accepted [:comparison :checks :recorded-submission])))
    (is (true? (get-in retry [:comparison :checks :recorded-submission])))))

(deftest stale-membership-dependency-rejects-otherwise-current-claim-test
  (let [req (seed! :requests (request "collected-auth"))
        member (seed! :memberships (membership "collected-auth-member"))
        rt [:requests (:xt/id req)]
        mt [:memberships (:xt/id member)]
        action (plan [(claim req "helper-b")]
                     (command/guard :memberships member version))]
    (xt/execute-tx (node) [[:put-docs :memberships (advance member {:active false})]])
    (let [report (check-report! (collect! action [rt mt]) :rejected)]
      (is (= :xtdb/assert-failed (get-in report [:execution :error/code])))
      (is (= :dependency-version
             (get-in report [:comparison :failed-precondition-kind])))
      (is (= req (physical-document :requests (:xt/id req))))
      (is (false? (:active (physical-document :memberships (:xt/id member))))))))

(deftest rejected-multi-document-transaction-is-atomic-test
  (let [req (seed! :requests (request "collected-atomic"))
        member (seed! :memberships (membership "collected-atomic-member"))
        rt [:requests (:xt/id req)]
        mt [:memberships (:xt/id member)]
        action (plan [(claim req "helper-c") (suspend member)])]
    ;; Make the second write stale after the plan was constructed.
    (xt/execute-tx (node) [[:put-docs :memberships (advance member {:active false})]])
    (let [before-member (physical-document :memberships (:xt/id member))
          report (check-report! (collect! action [rt mt]) :rejected)]
      (is (= :xtdb/assert-failed (get-in report [:execution :error/code])))
      (is (= req (physical-document :requests (:xt/id req))))
      (is (= before-member (physical-document :memberships (:xt/id member))))
      (is (= 2 (:collected-target-count report))))))

(deftest create-then-duplicate-create-correspond-test
  (let [req (request "collected-create")
        rt [:requests (:xt/id req)]
        action (plan [(command/create :requests req version)])]
    (is (nil? (physical-document :requests (:xt/id req))))
    (let [created (check-report! (collect! action [rt]) :committed)
          duplicate (check-report! (collect! action [rt]) :rejected)]
      (is (= 1 (:collected-target-count created)))
      (is (= req (physical-document :requests (:xt/id req))))
      (is (= :document-absent
             (get-in duplicate [:comparison :failed-precondition-kind])))
      (is (= :xtdb/assert-failed (get-in duplicate [:execution :error/code]))))))

(deftest nil-fields-are-projected-at-the-physical-boundary-test
  (let [req (assoc (request "collected-nil") :optional nil)
        rt [:requests (:xt/id req)]
        action (plan [(command/create :requests req version)])
        report (check-report! (collect! action [rt]) :committed)
        physical (physical-document :requests (:xt/id req))]
    (is (true? (get-in report [:comparison :checks :poststate])))
    (is (not (contains? physical :optional)))
    (is (= (dissoc req :optional) physical))))

(deftest field-count-assertion-checks-xtdb-persisted-fields-test
  (let [req (seed! :requests (assoc (request "collected-count") :nullable nil))
        rt [:requests (:xt/id req)]
        updated (claim req "helper-d")
        base (plan [updated])
        count-assert (tx/assert-document-field-count :requests (:xt/id req) 5)
        allowed (check-report!
                 (collect! (assoc base :assertions [count-assert]) [rt])
                 :committed)
        stale (check-report!
               (collect! (assoc base :assertions [count-assert]) [rt])
               :rejected)]
    (is (= 1 (:revision (physical-document :requests (:xt/id req)))))
    (is (= "helper-d" (:owner (physical-document :requests (:xt/id req)))))
    (is (true? (get-in allowed [:comparison :checks :recorded-submission])))
    (is (= :command-version
           (get-in stale [:comparison :failed-precondition-kind])))))

(deftest supplemental-observation-targets-are-preserved-test
  (let [req (seed! :requests (request "collected-extra"))
        spectator (seed! :memberships (membership "collected-extra-spectator"))
        rt [:requests (:xt/id req)]
        st [:memberships (:xt/id spectator)]
        action (plan [(claim req "helper-e")])
        report (check-report! (collect! action [rt st]) :committed)]
    (is (= 2 (:collected-target-count report)))
    (is (= spectator (physical-document :memberships (:xt/id spectator))))
    (is (= "helper-e" (:owner (physical-document :requests (:xt/id req)))))))

(deftest missing-read-and-write-targets-fail-before-physical-io-test
  (let [req (request "collected-missing")
        member (membership "collected-missing-member")
        rt [:requests (:xt/id req)]
        mt [:memberships (:xt/id member)]
        action (plan [(claim req "helper-f")]
                     (command/guard :memberships member version))
        calls (atom [])]
    (with-redefs [xt/q (fn [& _] (swap! calls conj :read) nil)
                  xt/execute-tx (fn [& _] (swap! calls conj :submit) nil)]
      (is (= ::collector/untracked-dependencies
             (error-type #(collect! action [rt]))))
      (is (= ::collector/untracked-dependencies
             (error-type #(collect! action [mt]))))
      (is (= [] @calls)))))

(deftest unsupported-table-and-plan-fail-before-any-physical-io-test
  (let [req (request "collected-unsafe")
        rt [:requests (:xt/id req)]
        action (plan [(command/create :requests req version)])
        calls (atom [])]
    (with-redefs [xt/q (fn [& _] (swap! calls conj :read) nil)
                  xt/execute-tx (fn [& _] (swap! calls conj :submit) nil)]
      (testing "Extra tracked targets with SQL-unsafe table identifiers"
        (is (= ::collector/unsupported-table
               (error-type #(collect! action [rt [:bad-name/unsafe-table "x"]])))))
      (testing "Unknown SQL assertion is never assumed true"
        (is (= :gesso.model.tx-model/unsupported-plan
               (error-type #(collect! (assoc action :assertions [{:assert [:select 1]}])
                                     [rt])))))
      (is (= [] @calls)))))

(deftest input-shape-and-target-integrity-fail-before-physical-io-test
  (let [req (request "collected-input")
        rt [:requests (:xt/id req)]
        base {:node (node)
              :plan (plan [(command/create :requests req version)])
              :targets [rt]}
        calls (atom [])]
    (with-redefs [xt/q (fn [& _] (swap! calls conj :read) nil)
                  xt/execute-tx (fn [& _] (swap! calls conj :submit) nil)]
      (doseq [invalid [(dissoc base :targets)
                       (assoc base :execution {:status :committed})
                       (assoc base :observed-before {})
                       (assoc base :recorded-tx-ops [])
                       (assoc base :node nil)]]
        (is (some? (error-type #(collector/observe-and-compare! invalid)))))
      (is (= ::collector/invalid-input
             (error-type #(collector/observe-and-compare!
                           (assoc base :execution {:status :committed})))))
      (is (= ::collector/missing-node
             (error-type #(collector/observe-and-compare!
                           (assoc base :node nil)))))
      (doseq [targets [[] [rt rt] [[:requests nil]] [:requests]]]
        (is (= :gesso.model.tx-refinement/invalid-targets
               (error-type #(collector/observe-and-compare!
                             (assoc base :targets targets))))))
      (is (= [] @calls)))))

(deftest unknown-execution-error-is-propagated-without-retry-test
  (let [req (seed! :requests (request "collected-unknown"))
        rt [:requests (:xt/id req)]
        original (physical-document :requests (:xt/id req))
        submission-count (atom 0)
        failure (ex-info "The execution outcome is unknown"
                         {:error/type ::indeterminate-execution})
        action (plan [(claim req "helper-g")])]
    (is (identical?
         failure
         (try
           (with-redefs [xt/execute-tx (fn [& _]
                                         (swap! submission-count inc)
                                         (throw failure))]
             (collect! action [rt]))
           nil
           (catch clojure.lang.ExceptionInfo error
             error))))
    (is (= 1 @submission-count))
    (is (= original (physical-document :requests (:xt/id req))))))
