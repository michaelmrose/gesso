(ns gesso.assurance.proof.xtdb-guarded-prestate-acquisition-test
  "v1485: Permanent adversarial checks of the v1484 conditional external
   snapshot-to-prewrite-ASSERT plan constructor, against embedded XTDB.

   Observe real statement-order rejection and atomic rollback, rather than
   treating an external snapshot token as a DML internal basis. Each test
   exercises the public acquisition function, not a hand-built imitation.
   Acquisition must not submit DML. A diagnostic is never an issued executor,
   receipt, certificate, or authorization for retry or settlement.

   All scenarios are bounded to supported top-level scalar document fields
   and the registered probe tables. They do NOT authenticate XTDB's hidden
   transaction prestate, all application read dependencies, valid-time
   semantics, complete writer mediation, or universal physical refinement."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.xtdb-guarded-prestate-acquisition :as acquire]
            [gesso.assurance.proof.xtdb-internal-prestate-capability :as cap]
            [gesso.model.command :as command]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(def ^:private table :gesso_v1485_guarded_probe)
(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(defonce ^:private !node (atom nil))

(defn- with-node [f]
  (with-open [database (xtn/start-node {})]
    (reset! !node database)
    (try (f)
         (finally (reset! !node nil)))))

(use-fixtures :once with-node)

(defn- node []
  (or @!node (throw (ex-info "Missing v1485 embedded XTDB" {}))))

(defn- id! [label]
  (str "v1485-" label "-" (random-uuid)))

(defn- document [id revision owner]
  {:xt/id id :revision revision :owner owner
   :created-at "t0" :updated-at (str "t" revision)})

(defn- next-document [before owner]
  (-> before
      (update :revision inc)
      (assoc :owner owner)
      (assoc :updated-at (str "t" (inc (:revision before))))))

(defn- update-plan
  ([before] (update-plan table before "claimed"))
  ([entity before owner]
   {:commands [(command/update-command entity :claim before
                                       (next-document before owner) version)]
    :emit false}))

(defn- source
  ([before] (source table before))
  ([entity before]
   {:plan (update-plan entity before "claimed")
    :targets [[entity (:xt/id before)]]}))

(defn- put! [doc]
  (xt/execute-tx (node) [[:put-docs table doc]]))

(defn- current [entity id]
  (let [sql (case entity
              :gesso_v1485_guarded_probe
              "SELECT * FROM gesso_v1485_guarded_probe WHERE _id = ?"
              :gesso_v1485_dependencies
              "SELECT * FROM gesso_v1485_dependencies WHERE _id = ?"
              :v1485/subjects
              "SELECT * FROM v1485.subjects WHERE _id = ?")]
    (first (xt/q (node) [sql id]))))

(defn- execution-outcome [ops]
  (try
    (xt/execute-tx (node) ops)
    :committed
    (catch Throwable e
      (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data e)))
        :assert-failed
        (throw e)))))

(defn- reason [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e
      (if (:xtdb-guarded-prestate/rejection (ex-data e))
        (:reason (ex-data e))
        (throw e)))))

(defn- acquire! [source]
  (acquire/acquire-guarded-prestate! (node) source))

(defn- assert-fail-and-preserved! [report id expected]
  (is (= :assert-failed (execution-outcome (:guarded-tx-ops report))))
  (is (= expected (current table id))
      "A rejected guarded plan must not commit its later mutation"))

(deftest unmodified-supported-prestate-allows-exact-guarded-plan
  (let [id (id! "unchanged")
        before (document id 1 "open")]
    (put! before)
    (let [r (acquire! (source before))]
      (is (= acquire/report-type (:type r)))
      (is (= 1 (:observed-target-count r)))
      (is (= 1 (:observed-present-count r)))
      (is (= 2 (:inserted-assertion-count r)))
      (is (= :committed (execution-outcome (:guarded-tx-ops r))))
      (is (= (next-document before "claimed") (current table id)))
      (is (= :not-authenticated (:transaction-internal-prestate r)))
      (is (= :conditional-unexecuted-plan-diagnostic (:proof-status r)))
      (is (= :not-issued (:certificate r)))
      (is (= :none (:authority r))))))

(deftest acquisition-makes-no-write-and-never-reexecutes-a-plan
  (let [id (id! "read-only")
        before (document id 1 "open")
        actual-execute xt/execute-tx
        writes (atom 0)]
    (put! before)
    (with-redefs [xt/execute-tx (fn [& args]
                                   (swap! writes inc)
                                   (apply actual-execute args))]
      (let [report (acquire! (source before))]
        (is (zero? @writes) "External read and plan construction do not submit DML")
        (is (= :committed (execution-outcome (:guarded-tx-ops report))))
        ;; XTDB 2.2.0-beta1's execute-tx delegates through its other arity;
        ;; the wrapper may see both calls for ONE explicit submission.
        (is (pos? @writes))
        (is (= :assert-failed (execution-outcome (:guarded-tx-ops report))))))
    (is (= (next-document before "claimed") (current table id)))))

(deftest changed-nonversion-field-rejects-even-when-revision-is-unchanged
  (let [id (id! "owner") before (document id 1 "open")]
    (put! before)
    (let [r (acquire! (source before))
          changed (assoc before :owner "intervened")]
      (put! changed)
      (assert-fail-and-preserved! r id changed))))

(deftest introduced-top-level-field-invalidates-guarded-plan
  (let [id (id! "added") before (document id 1 "open")]
    (put! before)
    (let [r (acquire! (source before))
          changed (assoc before :rogue-field "added-after-read")]
      (put! changed)
      (assert-fail-and-preserved! r id changed))))

(deftest removed-top-level-field-invalidates-guarded-plan
  (let [id (id! "removed") before (document id 1 "open")]
    (put! before)
    (let [r (acquire! (source before))
          changed (dissoc before :owner)]
      (put! changed)
      (assert-fail-and-preserved! r id changed))))

(deftest deleting-an-observed-document-invalidates-guarded-plan
  (let [id (id! "deleted") before (document id 1 "open")]
    (put! before)
    (let [r (acquire! (source before))]
      (xt/execute-tx (node) [[:delete-docs table id]])
      (is (nil? (current table id)))
      (is (= :assert-failed (execution-outcome (:guarded-tx-ops r))))
      (is (nil? (current table id))))))

(deftest absent-create-target-may-be-guarded-and-committed
  (let [id (id! "new")
        doc (document id 0 "new")
        plan {:commands [(command/create table doc version)] :emit false}
        r (acquire! {:plan plan :targets [[table id]]})]
    (is (nil? (current table id)))
    (is (= 0 (:observed-present-count r)))
    (is (= 1 (:inserted-assertion-count r)))
    (is (= :committed (execution-outcome (:guarded-tx-ops r))))
    (is (= doc (current table id)))))

(deftest creating-previously-absent-target-causes-atomic-rejection
  (let [id (id! "absent-raced")
        expected (document id 0 "new")
        plan {:commands [(command/create table expected version)] :emit false}
        r (acquire! {:plan plan :targets [[table id]]})
        intervened (document id 1 "another-writer")]
    (put! intervened)
    (assert-fail-and-preserved! r id intervened)))

(deftest extra-observed-absent-dependency-must-remain-absent
  (let [id (id! "main")
        absent-id (id! "absence-dependency")
        before (document id 1 "open")]
    (put! before)
    (let [r (acquire! {:plan (update-plan before)
                       :targets [[table id] [table absent-id]]})
          intervened (document absent-id 1 "external")]
      (is (= 1 (:observed-present-count r)))
      (is (= 3 (:inserted-assertion-count r)))
      (put! intervened)
      (is (= :assert-failed (execution-outcome (:guarded-tx-ops r))))
      (is (= before (current table id)))
      (is (= intervened (current table absent-id))))))

(deftest an-undeclared-dependency-is-not-protected-or-certified
  ;; This intentionally demonstrates a LIMIT of bounded target enumeration,
  ;; not an acceptable complete application certification.
  (let [id (id! "enumerated")
        outside-id (id! "unlisted-dependency")
        before (document id 1 "open")
        outside (document outside-id 1 "active")]
    (xt/execute-tx (node) [[:put-docs table before]
                           [:put-docs :gesso_v1485_dependencies outside]])
    (let [r (acquire! (source before))
          changed (assoc outside :owner "revoked-without-observation")]
      (xt/execute-tx (node) [[:put-docs :gesso_v1485_dependencies changed]])
      (is (= :committed (execution-outcome (:guarded-tx-ops r)))
          "An undeclared read dependency is outside these bounded assertions")
      (is (= changed (current :gesso_v1485_dependencies outside-id)))
      (is (= :not-established (:complete-physical-prestate r)))
      (is (= :not-established (:protected-domain-and-other-writers r)))
      (is (= :not-issued (:certificate r))))))

(deftest two-target-prestate-guards-fail-atomically-if-second-changes
  (let [id (id! "written")
        dependency-id (id! "dep")
        before (document id 1 "open")
        dependency (document dependency-id 1 "active")]
    (xt/execute-tx (node) [[:put-docs table before]
                           [:put-docs :gesso_v1485_dependencies dependency]])
    (let [r (acquire! {:plan (update-plan before)
                       :targets [[table id] [:gesso_v1485_dependencies dependency-id]]})
          changed (assoc dependency :owner "revoked")]
      (xt/execute-tx (node) [[:put-docs :gesso_v1485_dependencies changed]])
      (is (= 4 (:inserted-assertion-count r)))
      (is (= :assert-failed (execution-outcome (:guarded-tx-ops r))))
      (is (= before (current table id)))
      (is (= changed (current :gesso_v1485_dependencies dependency-id))))))

(deftest guards-remain-before-existing-assertions-and-every-command-write
  (let [id (id! "order")
        before (document id 1 "open")
        plan (assoc (update-plan before)
                    :assertions [{:assert [:= 1 1]}])]
    (put! before)
    (let [r (acquire! {:plan plan :targets [[table id]]})
          ops (:guarded-tx-ops r)
          inserted (:inserted-assertion-count r)]
      (is (= 2 inserted))
      (is (= 3 (count (get-in r [:guarded-plan :assertions]))))
      (is (every? #(and (vector? %) (string? (first %))
                        (re-find #"(?i)^ASSERT\b" (first %)))
                  (take inserted ops)))
      (is (every? #(and (vector? %) (string? (first %))
                        (re-find #"(?i)^ASSERT\b" (first %)))
                  (take 3 ops)))
      (is (= :put-docs (first (last ops))))
      (is (:unknown-assertion-dependencies? r))
      (is (= :committed (execution-outcome ops))))))

(deftest duplicated-targets-and-incomplete-target-set-reject
  (let [id (id! "missing")
        before (document id 1 "open")
        p (update-plan before)]
    (put! before)
    (doseq [[targets expected]
            [[[] :invalid-or-repeated-targets]
             [[[table id] [table id]] :invalid-or-repeated-targets]
             [[[:gesso_v1485_dependencies id]] :unobserved-command-or-guard-target]
             [[[table (id! "wrong")]] :unobserved-command-or-guard-target]
             [[[:broken.table id]] :unsupported-table-identifier]
             [[[table nil]] :unsupported-target]]]
      (is (= expected (reason #(acquire! {:plan p :targets targets})))
          (pr-str targets)))))

(deftest declared-guard-target-is-required-even-when-not-written
  (let [id (id! "written-guard")
        dep-id (id! "guarded")
        before (document id 1 "open")
        dep (document dep-id 1 "active")
        guarded-plan (assoc (update-plan before)
                            :guards [(command/guard :gesso_v1485_dependencies
                                                    dep version)])]
    (put! before)
    (xt/execute-tx (node) [[:put-docs :gesso_v1485_dependencies dep]])
    (is (= :unobserved-command-or-guard-target
           (reason #(acquire! {:plan guarded-plan
                               :targets [[table id]]}))))
    (let [r (acquire! {:plan guarded-plan
                       :targets [[table id] [:gesso_v1485_dependencies dep-id]]})]
      (is (= 2 (:observed-target-count r)))
      (is (= 4 (:inserted-assertion-count r)))
      (is (= :committed (execution-outcome (:guarded-tx-ops r)))))))

(deftest source-and-target-shapes-are-rejected-before-reading
  (let [id (id! "shapes")
        before (document id 1 "open")
        s (source before)]
    (doseq [[source expected]
            [[nil :invalid-source-shape]
             [(assoc s :extra :unexpected) :invalid-source-shape]
             [(with-meta s {:untrusted true}) :invalid-source-shape]
             [(assoc s :targets nil) :invalid-or-repeated-targets]
             [(assoc s :targets '([:gesso_v1485_guarded_probe 3]))
              :invalid-or-repeated-targets]
             [(assoc s :targets (vec (repeat 33 [table id])))
              :invalid-or-repeated-targets]
             [(assoc s :targets (mapv (fn [i] [table (str id "-" i)])
                                      (range 33)))
              :invalid-or-repeated-targets]
             [(assoc s :plan [:not-a-plan]) :invalid-canonical-plan]]]
      (is (= expected (reason #(acquire! source))) (pr-str expected)))))

(deftest unsupported-nested-document-data-does-not-produce-a-guarded-plan
  (let [id (id! "nested")
        before (document id 1 "open")]
    (put! (assoc before :owner {:nested "not-a-supported-scalar"}))
    (is (= :unsupported-snapshot-row
           (reason #(acquire! (source before)))))))

(deftest unsupported-array-document-data-does-not-produce-a-guarded-plan
  (let [id (id! "array")
        before (document id 1 "open")]
    (put! (assoc before :owner ["one" "two"]))
    (is (= :unsupported-snapshot-row
           (reason #(acquire! (source before)))))))

(deftest table-schema-and-parameterized-id-are-preserved
  (let [id (str "v1485-' OR 1=1 --" (random-uuid))
        before (document id 1 "open")
        entity :v1485/subjects]
    (xt/execute-tx (node) [[:put-docs entity before]])
    (let [r (acquire! (source entity before))]
      (is (= [[entity id]] (mapv :target (:observed-rows r))))
      (is (= :committed (execution-outcome (:guarded-tx-ops r))))
      (is (= (next-document before "claimed") (current entity id))))))

(deftest external-snapshot-report-remains-conditional-and-redaction-safe
  (let [id (id! "secret-should-not-leak")
        before (document id 1 "hidden")]
    (put! before)
    (let [r (acquire! (source before))
          summary (acquire/redacted-summary r)
          printed (pr-str summary)]
      (is (= :one-external-read-and-unexecuted-guarded-plan (:scope summary)))
      (is (= :caller-supplied-unattested (:external-read-node-provenance summary)))
      (is (= :not-established (:guarded-ops-execution-binding summary)))
      (is (= :not-authenticated (:transaction-internal-prestate summary)))
      (is (= :not-established (:complete-physical-prestate summary)))
      (is (= :not-established (:protected-domain-and-other-writers summary)))
      (is (= :not-issued (:certificate summary)))
      (is (= :not-admitted (:admission summary)))
      (is (= :none (:transaction-authority summary)))
      (is (= :none (:retry-authority summary)))
      (is (= :none (:settlement-authority summary)))
      (is (not (contains? summary :external-read-basis)))
      (is (not (contains? summary :observed-rows)))
      (is (not (contains? summary :original-plan)))
      (is (not (contains? summary :guarded-tx-ops)))
      (is (not (.contains printed id)))
      (is (not (.contains printed "snapshot-token"))))))

(deftest malformed-summary-authority-or-proof-claims-are-rejected
  (let [id (id! "summary")
        before (document id 1 "open")]
    (put! before)
    (let [r (acquire! (source before))]
      (doseq [altered [(assoc r :type :other)
                       (assoc r :version 42)
                       (assoc r :certificate :issued)
                       (assoc r :proof-status :proved)
                       (assoc r :authority :submit)]]
        (is (= :invalid-diagnostic-summary
               (reason #(acquire/redacted-summary altered))))))))

(deftest substituted-xtdb-read-and-capability-checkers-reject
  (let [id (id! "substitution")
        before (document id 1 "open")]
    (put! before)
    (is (= :substituted-checker-or-acquisition-seam
           (reason #(with-redefs [xt/q (fn [& _] [])]
                      (acquire! (source before))))))
    (is (= :substituted-checker-or-acquisition-seam
           (reason #(with-redefs [cap/capability-report!
                                   (fn [] {:compatibility :pinned-api-and-gesso-option-surface})]
                      (acquire! (source before))))))))

(deftest unknown-original-assertion-dependencies-are-never-certified
  (let [id (id! "unknown")
        before (document id 1 "open")
        plan (assoc (update-plan before) :assertions [{:assert [:= 1 1]}])]
    (put! before)
    (let [r (acquire! {:plan plan :targets [[table id]]})]
      (is (:unknown-assertion-dependencies? r))
      (is (= :not-established (:complete-physical-prestate r)))
      (is (= :not-issued (:certificate r)))
      (is (= :none (:authority r))))))

(deftest two-guarded-claims-from-the-same-external-basis-cannot-both-commit
  (let [id (id! "concurrent")
        before (document id 1 "open")]
    (put! before)
    (let [r-a (acquire! {:plan (update-plan table before "helper-A")
                         :targets [[table id]]})
          r-b (acquire! {:plan (update-plan table before "helper-B")
                         :targets [[table id]]})
          ready (CountDownLatch. 2)
          go (CountDownLatch. 1)
          run (fn [r]
                (future
                  (.countDown ready)
                  (when-not (.await go 30 TimeUnit/SECONDS)
                    (throw (ex-info "Timed out awaiting concurrent transaction release" {})))
                  (execution-outcome (:guarded-tx-ops r))))
          a (run r-a)
          b (run r-b)]
      (try
        (is (.await ready 30 TimeUnit/SECONDS))
        (.countDown go)
        (let [results [(deref a 30000 ::timed-out)
                       (deref b 30000 ::timed-out)]
              final-doc (current table id)]
          (is (= #{:committed :assert-failed} (set results)))
          (is (= 2 (:revision final-doc)))
          (is (contains? #{"helper-A" "helper-B"} (:owner final-doc))))
        (finally
          (.countDown go))))))
