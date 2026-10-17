(ns gesso.model.tx-refinement-sql-xtdb-test
  "Physical XTDB truth-table witnesses for the admitted finite precondition SQL.

   Each case acquires its physical before-state independently with SELECT *,
   evaluates the finite model over that observed state, executes the actual
   production tx/prepare operations once, and compares the typed physical
   outcome and the entire tracked poststate. The cases vary independently the
   truth of document-absence, command/dependency versions, and canonical
   top-level field-count predicates. They test actual XTDB interpretation,
   beyond HoneySQL structural equality checked by tx-refinement-prestate.

   This is a bounded integration regression on one controlled embedded node.
   It does NOT prove general SQL semantics, transaction-internal snapshot
   correspondence, absence of external writers, or universal refinement."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.model.command :as command]
            [gesso.model.tx :as tx]
            [gesso.model.tx-model :as model]
            [gesso.model.tx-refinement-prestate :as prestate]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(defonce ^:private !db (atom nil))

(defn- database []
  (or @!db (throw (ex-info "XTDB fixture is not running." {}))))

(defn- with-database [f]
  (with-open [db (xtn/start-node {})]
    (reset! !db db)
    (try (f)
         (finally (reset! !db nil)))))

(use-fixtures :once with-database)

(defn- document [label]
  {:xt/id (str "v1022-" label "-" (random-uuid))
   :revision 0
   :created-at "t0"
   :updated-at "t0"
   :owner "unclaimed"})

(defn- after-update [before change]
  (-> before
      (merge change)
      (update :revision inc)
      (assoc :updated-at "t1")))

(defn- creation [table doc]
  (command/create table doc version))

(defn- update-action [table before change]
  (command/update-command table :update before (after-update before change) version))

(defn- plan
  ([cmds] (plan cmds [] []))
  ([cmds guards assertions]
   {:commands (vec cmds)
    :guards (vec guards)
    :assertions (vec assertions)
    :emit false}))

(defn- select-document [[table id]]
  ;; Fixed, test-owned table vocabulary: never interpolate a caller string.
  (let [sql (case table
              :requests "SELECT * FROM requests WHERE _id = ?"
              :memberships "SELECT * FROM memberships WHERE _id = ?"
              :public/receipts "SELECT * FROM public.receipts WHERE _id = ?"
              (throw (ex-info "Unrecognized test table." {:table table})))
        rows (xt/q (database) [sql id])]
    (when-not (<= (count rows) 1)
      (throw (ex-info "Duplicate physical document observations." {})))
    (first rows)))

(defn- observed-state [targets]
  (into {}
        (keep (fn [target]
                (when-let [doc (select-document target)]
                  [target doc])))
        targets))

(defn- seed! [table doc]
  (xt/execute-tx (database) [[:put-docs table doc]])
  doc)

(defn- physical-outcome! [prepared]
  (try
    (xt/execute-tx (database) (:tx-ops prepared))
    {:status :committed}
    (catch Throwable error
      (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data error)))
        {:status :rejected :error/code :xtdb/assert-failed}
        (throw error)))))

(defn- witness!
  "All targets must be supplied, including absent write targets. The
   expected classification is independent of the reference prediction."
  [{:keys [label action targets expected-status expected-failed-kind]}]
  (let [targets (vec targets)
        before (observed-state targets)
        obligation (prestate/derive-obligation action)
        prediction (model/step before action)
        execution (physical-outcome! (tx/prepare {} action))
        actual (observed-state targets)]
    (testing label
      (is (:local-structure-valid? obligation))
      (is (false? (:certifies-physical-execution? obligation)))
      (is (= expected-status (:status prediction))
          "Finite precondition interpretation differs from the truth table.")
      (is (= expected-status (:status execution))
          "Physical ASSERT interpretation differs from the truth table.")
      (is (= (:after prediction) actual)
          "Physical complete tracked poststate differs from finite transition.")
      (when expected-failed-kind
        (is (= expected-failed-kind
               (get-in prediction [:failed-precondition :kind]))))
      (when (= :rejected expected-status)
        (is (= :xtdb/assert-failed (:error/code execution)))
        (is (= before actual) "Rejected SQL ASSERT must not write.")))
    {:before before :prediction prediction :execution execution :actual actual}))

(defn- output-target [table output]
  [table (:xt/id output)])

(deftest absence-assertion-truth-table-test
  (testing "No target row: canonical create ASSERT accepts"
    (let [new-doc (document "absent")]
      (witness! {:label "absent create"
                 :action (plan [(creation :requests new-doc)])
                 :targets [(output-target :requests new-doc)]
                 :expected-status :committed})))
  (testing "An existing target row: canonical create ASSERT rejects"
    (let [existing (seed! :requests (document "exists"))
          new-doc (assoc existing :owner "wrong-overwrite")]
      (witness! {:label "duplicate create"
                 :action (plan [(creation :requests new-doc)])
                 :targets [(output-target :requests existing)]
                 :expected-status :rejected
                 :expected-failed-kind :document-absent}))))

(deftest schema-qualified-absence-assertion-test
  (let [table :public/receipts
        new-doc (document "qualified")
        target (output-target table new-doc)]
    (witness! {:label "qualified absence accepts"
               :action (plan [(creation table new-doc)])
               :targets [target]
               :expected-status :committed})
    (witness! {:label "qualified absence rejects"
               :action (plan [(creation table (assoc new-doc :owner "duplicate"))])
               :targets [target]
               :expected-status :rejected
               :expected-failed-kind :document-absent})))

(deftest command-version-assertion-truth-table-test
  (testing "Exactly observed revision and timestamp admit a write"
    (let [original (seed! :requests (document "command-match"))]
      (witness! {:label "command version matches"
                 :action (plan [(update-action :requests original {:owner "helper"})])
                 :targets [(output-target :requests original)]
                 :expected-status :committed})))
  (testing "A stale command is rejected with its complete write set unchanged"
    (let [original (document "command-stale")
          _ (seed! :requests (after-update original {:owner "first"}))]
      (witness! {:label "stale command version"
                 :action (plan [(update-action :requests original {:owner "second"})])
                 :targets [(output-target :requests original)]
                 :expected-status :rejected
                 :expected-failed-kind :command-version}))))

(deftest dependency-guard-assertion-truth-table-test
  (doseq [[label physical-change expected-status]
          [["matching membership" {} :committed]
           ["changed revision" {:revision 1} :rejected]
           ["changed timestamp" {:updated-at "t1"} :rejected]
           ["uncompared field" {:owner "another"} :committed]]]
    (let [acquired (document label)
          physical (merge acquired physical-change)
          _ (seed! :memberships physical)
          out (document (str label "-out"))]
      (witness! {:label label
                 :action (plan [(creation :requests out)]
                               [(command/guard :memberships acquired version)]
                               [])
                 :targets [(output-target :memberships acquired)
                           (output-target :requests out)]
                 :expected-status expected-status
                 :expected-failed-kind (when (= :rejected expected-status)
                                         :dependency-version)})))
  (let [acquired (document "missing-dependency")
        out (document "missing-dependency-out")]
    (witness! {:label "missing guarded document"
               :action (plan [(creation :requests out)]
                             [(command/guard :memberships acquired version)]
                             [])
               :targets [(output-target :memberships acquired)
                         (output-target :requests out)]
               :expected-status :rejected
               :expected-failed-kind :dependency-version})))

(deftest nested-lifted-value-assertion-truth-table-test
  (let [acquired (assoc (document "nested-value")
                        :policy {:roles [:helper :dispatcher]
                                 :rules {:claimed? false :priority [1 2]}})
        compare-version (assoc version :compare-keys [:revision :updated-at :policy])]
    (doseq [[label observed expected-status]
            [["equal nested structure" acquired :committed]
             ["changed nested structure"
              (assoc-in acquired [:policy :rules :priority] [1 3]) :rejected]]]
      (seed! :memberships observed)
      (let [out (document (str "nested-" label))]
        (witness! {:label label
                   :action (plan [(creation :requests out)]
                                 [(command/guard :memberships acquired compare-version)]
                                 [])
                   :targets [(output-target :memberships acquired)
                             (output-target :requests out)]
                   :expected-status expected-status
                   :expected-failed-kind (when (= :rejected expected-status)
                                           :dependency-version)})))))

(deftest field-count-assertion-truth-table-test
  (doseq [[label change expected-status]
          [["same top-level fields" identity :committed]
           ["new field" #(assoc % :extra true) :rejected]
           ["removed field" #(dissoc % :owner) :rejected]
           ["nil top-level field" #(assoc % :optional nil) :committed]
           ["changed existing value" #(assoc % :owner "different") :committed]]]
    (let [observed (document (str "count-" label))
          physical (change observed)
          _ (seed! :requests physical)
          out (document (str "count-out-" label))
          action (plan [(creation :requests out)]
                       []
                       [(tx/assert-document-field-count
                         :requests (:xt/id observed) (count observed))])]
      (witness! {:label label
                 :action action
                 :targets [(output-target :requests observed)
                           (output-target :requests out)]
                 :expected-status expected-status
                 :expected-failed-kind (when (= :rejected expected-status)
                                         :document-field-count)})))
  (let [absent (document "count-absent")
        out (document "count-missing-out")]
    (witness! {:label "missing row has no matching field count"
               :action (plan [(creation :requests out)] []
                             [(tx/assert-document-field-count
                               :requests (:xt/id absent) (count absent))])
               :targets [(output-target :requests absent)
                         (output-target :requests out)]
               :expected-status :rejected
               :expected-failed-kind :document-field-count})))

(deftest multiple-preconditions-are-checked-before-any-write-test
  (let [observed (document "atomic-dependency")
        physically-stale (assoc observed :updated-at "t1")
        _ (seed! :memberships physically-stale)
        out1 (document "multi-out-1")
        out2 (document "multi-out-2")
        action (plan [(creation :requests out1)
                      (creation :requests out2)]
                     [(command/guard :memberships observed version)]
                     [])]
    (witness! {:label "stale guard prevents both writes"
               :action action
               :targets [(output-target :memberships observed)
                         (output-target :requests out1)
                         (output-target :requests out2)]
               :expected-status :rejected
               :expected-failed-kind :dependency-version})))
