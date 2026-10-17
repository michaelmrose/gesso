(ns gesso.model.tx-refinement-test
  "Permanent bounded correspondence regressions for gesso.model.tx-refinement.

   The physical cases use production tx/prepare, real embedded XTDB2 writes,
   and independent SELECT * observations before and after each transaction.
   Deliberately incomplete or falsified evidence is tested separately so it
   cannot be confused with a witnessed physical disagreement. Passing these
   finite cases is NOT a universal concrete-to-abstract refinement proof."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.model.command :as command]
   [gesso.model.tx :as tx]
   [gesso.model.tx-model :as model]
   [gesso.model.tx-refinement :as refinement]
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
  (with-open [database (xtn/start-node {})]
    (reset! !node database)
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

(defn- claim [document who]
  (command/update-command
   :requests :claim document (advance document {:owner who}) version))

(defn- revoke [document]
  (command/update-command
   :memberships :revoke document (advance document {:active false}) version))

(defn- seed! [table document]
  (xt/execute-tx (node) [[:put-docs table document]])
  document)

(defn- physical-document [table id]
  ;; Deliberately read SELECT *, not a fixed list of expected fields.
  (let [query (case table
                :requests "SELECT * FROM requests WHERE _id = ?"
                :memberships "SELECT * FROM memberships WHERE _id = ?")
        rows (xt/q (node) [query id])]
    (when-not (<= (count rows) 1)
      (throw (ex-info "The target does not identify a single physical document."
                      {:table table :id id :rows rows})))
    (first rows)))

(defn- observe! [targets]
  ;; The *presence* of a nil value proves the absent target was queried.
  ;; A missing target is never used to represent an absent document.
  (into {}
        (map (fn [[table id :as target]]
               [target (physical-document table id)]))
        targets))

(defn- execute! [a-plan]
  (try
    (xt/execute-tx (node) (:tx-ops (tx/prepare {} a-plan)))
    {:status :committed}
    (catch Throwable error
      (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data error)))
        {:status :rejected :error/code :xtdb/assert-failed}
        (throw error)))))

(defn- input [reference-before a-plan targets before execution after]
  {:reference-before reference-before
   :plan a-plan
   :targets targets
   :observed-before before
   :execution execution
   :observed-after after})

(defn- run-physical-step! [reference-before a-plan targets]
  (let [before (observe! targets)
        execution (execute! a-plan)
        after (observe! targets)
        arguments (input reference-before a-plan targets before execution after)
        comparison (refinement/compare-step arguments)]
    (is (:corresponds? comparison)
        (str "Real XTDB transition differs from the bounded reference: "
             (pr-str comparison)))
    (is (every? true? (vals (:checks comparison)))
        (pr-str (:checks comparison)))
    {:arguments arguments
     :comparison comparison
     :execution execution
     :physical-before before
     :physical-after after
     :reference-after (:after (model/step reference-before a-plan))}))

(defn- error-type [thunk]
  (try (thunk)
       nil
       (catch clojure.lang.ExceptionInfo error
         (:error/type (ex-data error)))))

(deftest comparison-scope-is-explicit-and-bounded-test
  (let [scope refinement/comparison-scope]
    (is (= :gesso.model.tx/physical-observation-v1 (:relation scope)))
    (is (contains? (:checked scope) :explicit-target-coverage))
    (is (contains? (:checked scope) :reference-outcome-status))
    (is (contains? (:assumed scope) :independent-before-and-after-physical-reads))
    (is (contains? (:assumed scope) :serialized-atomic-commit))
    (doseq [unproved [:general-xtdb-step-refinement
                     :writer-and-dependency-closure
                     :database-isolation-and-temporal-history
                     :authorization-and-business-invariants
                     :durable-command-incorporation-and-replay
                     :browser-or-choreography-correspondence]]
      (is (contains? (:not-established scope) unproved)))))

(deftest state-relation-requires-explicit-absent-document-test
  (let [a [:requests "never-created"]
        b [:requests "also-absent"]]
    (is (= {:relates? true :mismatched-targets [] :tracked-target-count 2}
           (refinement/state-relation [a b] {} {a nil b nil})))
    (is (= ::refinement/incomplete-observation
           (error-type #(refinement/state-relation [a b] {} {a nil}))))
    (is (= ::refinement/incomplete-observation
           (error-type #(refinement/state-relation [a] {} {a nil b nil}))))
    (is (= ::refinement/invalid-targets
           (error-type #(refinement/state-relation [a a] {} {a nil}))))
    (is (= ::refinement/invalid-targets
           (error-type #(refinement/state-relation [] {} {}))))))

(deftest state-relation-exposes-nil-projection-and-mismatched-values-test
  (let [doc {:xt/id "reference-1" :owner "unclaimed" :optional nil
             :nested {:unknown nil}}
        target [:requests "reference-1"]
        projected (dissoc doc :optional)]
    (is (= {target projected}
           (refinement/project-model-state {target doc})))
    (is (:relates? (refinement/state-relation
                    [target] {target doc} {target projected})))
    (is (= [target]
           (:mismatched-targets
            (refinement/state-relation
             [target] {target doc}
             {target (assoc-in projected [:nested :unknown] "not-nil")}))))
    (is (= ::refinement/invalid-physical-document
           (error-type #(refinement/state-relation
                         [target] {target doc} {target doc}))))
    (is (= ::refinement/invalid-physical-document
           (error-type #(refinement/state-relation
                         [target] {target doc}
                         {target (assoc projected :xt/id "wrong-id")}))))
    (is (= ::refinement/untracked-model-state
           (error-type #(refinement/state-relation
                         [[:requests "different"]] {target doc}
                         {[:requests "different"] nil}))))))

(deftest physical-claim-and-stale-retry-have-corresponding-transitions-test
  (let [req (seed! :requests (assoc (request "refinement-claim")
                                    :optional-policy nil))
        member (seed! :memberships (membership "refinement-member"))
        req-target [:requests (:xt/id req)]
        member-target [:memberships (:xt/id member)]
        targets [req-target member-target]
        initial {req-target req member-target member}
        action (plan [(claim req "helper-a")]
                     (command/guard :memberships member version))
        accepted (run-physical-step! initial action targets)
        retry (run-physical-step! (:reference-after accepted) action targets)]
    (testing "Successful commit projects away an explicit top-level nil"
      (is (not (contains? (get-in accepted [:physical-before req-target])
                          :optional-policy)))
      (is (= :committed (get-in accepted [:execution :status])))
      (is (= :committed (get-in accepted [:comparison :expected-status])))
      (is (= "helper-a" (get-in accepted [:physical-after req-target :owner])))
      (is (= 1 (get-in accepted [:physical-after req-target :revision]))))
    (testing "Stale retry is a recognized assertion rejection and stutters"
      (is (= :rejected (get-in retry [:execution :status])))
      (is (= :xtdb/assert-failed (get-in retry [:execution :error/code])))
      (is (= :command-version
             (get-in retry [:comparison :failed-precondition-kind])))
      (is (= (:physical-before retry) (:physical-after retry)))
      (is (= (:physical-after accepted) (:physical-after retry))))))

(deftest physical-multi-write-and-absent-create-target-correspond-test
  (let [req (seed! :requests (request "refinement-update"))
        created (request "refinement-created")
        req-target [:requests (:xt/id req)]
        new-target [:requests (:xt/id created)]
        targets [req-target new-target]
        action (plan [(claim req "helper-b")
                      (command/create :requests created version)])
        result (run-physical-step! {req-target req} action targets)]
    (is (contains? (:physical-before result) new-target))
    (is (nil? (get-in result [:physical-before new-target])))
    (is (= :committed (get-in result [:execution :status])))
    (is (= created (get-in result [:physical-after new-target])))
    (is (= "helper-b" (get-in result [:physical-after req-target :owner])))))

(deftest physical-failing-membership-guard-prevents-all-writes-test
  (let [req (seed! :requests (request "refinement-denied"))
        member (seed! :memberships (membership "refinement-revoked"))
        created (request "refinement-no-create")
        req-target [:requests (:xt/id req)]
        member-target [:memberships (:xt/id member)]
        created-target [:requests (:xt/id created)]
        initial {req-target req member-target member}
        revoked (run-physical-step! initial (plan [(revoke member)])
                                    [req-target member-target])
        action (plan [(claim req "helper-c")
                      (command/create :requests created version)]
                     (command/guard :memberships member version))
        rejected (run-physical-step! (:reference-after revoked) action
                                     [req-target member-target created-target])]
    (is (= :rejected (get-in rejected [:execution :status])))
    (is (= :dependency-version
           (get-in rejected [:comparison :failed-precondition-kind])))
    (is (= (:physical-before rejected) (:physical-after rejected)))
    (is (nil? (get-in rejected [:physical-after created-target])))
    (is (= "unclaimed" (get-in rejected [:physical-after req-target :owner])))))

(deftest physical-duplicate-create-has-explicit-absent-and-present-reads-test
  (let [created (request "refinement-duplicate")
        target [:requests (:xt/id created)]
        action (plan [(command/create :requests created version)])
        accepted (run-physical-step! {} action [target])
        rejected (run-physical-step! (:reference-after accepted) action [target])]
    (is (nil? (get-in accepted [:physical-before target])))
    (is (= created (get-in accepted [:physical-after target])))
    (is (= :rejected (get-in rejected [:execution :status])))
    (is (= :document-absent
           (get-in rejected [:comparison :failed-precondition-kind])))
    (is (= (:physical-before rejected) (:physical-after rejected)))))

(deftest physical-field-count-at-nil-boundary-corresponds-test
  (let [req (seed! :requests (assoc (request "refinement-count")
                                    :optional-policy nil))
        created (request "refinement-count-new")
        req-target [:requests (:xt/id req)]
        created-target [:requests (:xt/id created)]
        targets [req-target created-target]
        actual (get (observe! targets) req-target)
        a-plan {:commands [(command/create :requests created version)]
                :guards [(command/guard :requests req version)]
                :assertions [(tx/assert-document-field-count
                              :requests (:xt/id req) (count actual))]
                :emit false}
        result (run-physical-step! {req-target req} a-plan targets)]
    (is (= (dec (count req)) (count actual)))
    (is (= :committed (get-in result [:execution :status])))
    (is (= created (get-in result [:physical-after created-target])))))

(deftest physical-observations-expose-deliberate-poststate-disagreement-test
  (let [req (seed! :requests (request "refinement-suspect-after"))
        target [:requests (:xt/id req)]
        action (plan [(claim req "helper-d")])
        result (run-physical-step! {target req} action [target])
        original (:arguments result)
        falsified (assoc-in original [:observed-after target :owner] "impossible")
        mismatch (refinement/compare-step falsified)]
    (is (false? (:corresponds? mismatch)))
    (is (= {:prestate true
            :outcome-status true
            :recognized-conflict true
            :poststate false}
           (:checks mismatch)))
    (is (= [target] (:poststate-mismatches mismatch)))
    (is (empty? (:prestate-mismatches mismatch)))))

(deftest physical-observations-expose-deliberate-prestate-disagreement-test
  (let [req (seed! :requests (request "refinement-suspect-before"))
        target [:requests (:xt/id req)]
        action (plan [(claim req "helper-e")])
        result (run-physical-step! {target req} action [target])
        original (:arguments result)
        mismatch (refinement/compare-step
                  (assoc-in original [:observed-before target :owner]
                            "unauthorized-value"))]
    (is (false? (:corresponds? mismatch)))
    (is (= {:prestate false
            :outcome-status true
            :recognized-conflict true
            :poststate true}
           (:checks mismatch)))
    (is (= [target] (:prestate-mismatches mismatch)))
    (is (empty? (:poststate-mismatches mismatch)))))

(deftest real-execution-record-cannot-be-misclassified-as-valid-evidence-test
  (let [req (seed! :requests (request "refinement-status"))
        target [:requests (:xt/id req)]
        action (plan [(claim req "helper-f")])
        result (run-physical-step! {target req} action [target])
        original (:arguments result)
        bogus-rejected (refinement/compare-step
                        (assoc original :execution
                               {:status :rejected
                                :error/code :xtdb/assert-failed}))
        bogus-error (refinement/compare-step
                     (assoc original :execution
                            {:status :committed :error/code :xtdb/assert-failed}))]
    (is (false? (:corresponds? bogus-rejected)))
    (is (false? (get-in bogus-rejected [:checks :outcome-status])))
    (is (false? (:corresponds? bogus-error)))
    (is (false? (get-in bogus-error [:checks :recognized-conflict])))
    (is (= ::refinement/invalid-execution
           (error-type #(refinement/compare-step
                         (assoc original :execution {:status :unknown})))))))

(deftest comparison-requires-all-read-dependencies-and-write-targets-test
  (let [req (seed! :requests (request "refinement-footprint"))
        member (seed! :memberships (membership "refinement-footprint-member"))
        created (request "refinement-footprint-created")
        req-target [:requests (:xt/id req)]
        member-target [:memberships (:xt/id member)]
        created-target [:requests (:xt/id created)]
        targets [req-target member-target created-target]
        a-plan (plan [(claim req "helper-g")
                      (command/create :requests created version)]
                     (command/guard :memberships member version))
        initial {req-target req member-target member}
        before (observe! targets)
        arguments (input initial a-plan targets before {:status :committed} before)]
    (is (= ::refinement/untracked-dependency
           (error-type #(refinement/compare-step
                         (-> arguments
                             (assoc :targets [req-target member-target])
                             (update :observed-before dissoc created-target)
                             (update :observed-after dissoc created-target))))))
    (is (= ::refinement/untracked-dependency
           (error-type #(refinement/compare-step
                         (-> arguments
                             (assoc :targets [req-target created-target])
                             (update :observed-before dissoc member-target)
                             (update :observed-after dissoc member-target))))))
    (is (= ::refinement/incomplete-observation
           (error-type #(refinement/compare-step
                         (update arguments :observed-after dissoc created-target)))))
    (is (= ::refinement/invalid-input
           (error-type #(refinement/compare-step
                         (dissoc arguments :observed-before)))))))

(deftest comparison-refuses-unrecognized-rejection-codes-test
  (let [req (seed! :requests (request "refinement-error-code"))
        target [:requests (:xt/id req)]
        action (plan [(claim req "helper-h")])
        accepted (run-physical-step! {target req} action [target])
        rejected (run-physical-step! (:reference-after accepted) action [target])
        classified (refinement/compare-step
                    (assoc (:arguments rejected) :execution
                           {:status :rejected :error/code :unexpected-error}))]
    (is (= :rejected (get-in rejected [:execution :status])))
    (is (false? (:corresponds? classified)))
    (is (false? (get-in classified [:checks :recognized-conflict])))
    (is (true? (get-in classified [:checks :outcome-status])))))
