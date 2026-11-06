(ns gesso.assurance.proof.xtdb-internal-prestate-experiment-test
  "v1482: Bounded XTDB 2.2.0-beta1 transaction-internal read experiments.

   A DML transaction's ASSERTs observe statement-order state, not a caller's
   previously pinned read-only snapshot. These experiments use real embedded
   XTDB and deliberately put ASSERTs both before and after writes. They also
   test rollback of writes preceding a failed assertion and the canonical
   Gesso assertion-before-write lowering.

   Interpretation is deliberately narrow: this suite demonstrates observable
   behavior for the concrete statements tested. It does NOT expose an internal
   transaction snapshot token, prove which physical basis was selected,
   authenticate authorization/guards for arbitrary Clojure, establish complete
   writer coverage, or discharge universal physical refinement."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.model.command :as command]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(defonce ^:private !node (atom nil))

(defn- with-embedded-node [f]
  (with-open [database (xtn/start-node {})]
    (reset! !node database)
    (try
      (f)
      (finally
        (reset! !node nil)))))

(use-fixtures :once with-embedded-node)

(defn- node []
  (or @!node (throw (ex-info "v1482 embedded XTDB not started" {}))))

(defn- unique-id [label]
  (str "gesso-v1482-" label "-" (random-uuid)))

(defn- doc [id revision owner]
  {:xt/id id :revision revision :owner owner
   :created-at "t0" :updated-at (str "t" revision)})

(defn- put! [table document]
  (xt/execute-tx (node) [[:put-docs table document]]))

(defn- current [table id]
  (let [query (case table
                :gesso_v1482_prestate_probe
                "SELECT _id, revision, owner, created_at, updated_at FROM gesso_v1482_prestate_probe WHERE _id = ?"
                :gesso_v1482_prestate_memberships
                "SELECT _id, revision, owner, created_at, updated_at FROM gesso_v1482_prestate_memberships WHERE _id = ?")
        rows (xt/q (node) [query id])]
    (is (<= (count rows) 1) "A probe ID has at most one current document")
    (first rows)))

(defn- assert-revision [table id revision]
  (let [table-name (case table
                     :gesso_v1482_prestate_probe "gesso_v1482_prestate_probe"
                     :gesso_v1482_prestate_memberships "gesso_v1482_prestate_memberships")]
    [(str "ASSERT 1 = (SELECT COUNT(*) FROM " table-name
          " WHERE _id = ? AND revision = ?)")
     id revision]))

(defn- execute-probe! [ops]
  (try
    {:status :committed :tx-key (xt/execute-tx (node) ops)}
    (catch Throwable e
      (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data e)))
        {:status :assert-failed :error/code :xtdb/assert-failed}
        (throw e)))))

(defn- assert-committed! [result]
  (is (= :committed (:status result)) (pr-str result))
  (is (some? (:tx-key result)) (pr-str result)))

(defn- assert-rejected! [result]
  (is (= :assert-failed (:status result)) (pr-str result))
  (is (= :xtdb/assert-failed (:error/code result)) (pr-str result)))

(deftest later-assert-sees-an-earlier-put-in-same-dml-transaction
  (let [id (unique-id "read-own-put")
        before (doc id 1 "initial")
        after (doc id 2 "claimed")]
    (put! :gesso_v1482_prestate_probe before)
    (assert-committed!
     (execute-probe! [[:put-docs :gesso_v1482_prestate_probe after]
            (assert-revision :gesso_v1482_prestate_probe id 2)]))
    (is (= after (current :gesso_v1482_prestate_probe id)))
    (is (not= before (current :gesso_v1482_prestate_probe id)))))

(deftest asserts-before-and-after-write-observe-different-statement-states
  (let [id (unique-id "order")
        before (doc id 1 "open")
        after (doc id 2 "taken")]
    (put! :gesso_v1482_prestate_probe before)
    (assert-committed!
     (execute-probe! [(assert-revision :gesso_v1482_prestate_probe id 1)
            [:put-docs :gesso_v1482_prestate_probe after]
            (assert-revision :gesso_v1482_prestate_probe id 2)]))
    (is (= after (current :gesso_v1482_prestate_probe id)))))

(deftest failing-post-write-assert-rolls-back-earlier-writes
  (let [id-a (unique-id "rollback-a")
        id-b (unique-id "rollback-b")
        before-a (doc id-a 1 "open")
        before-b (doc id-b 1 "open")
        after-a (doc id-a 2 "helper-a")
        after-b (doc id-b 2 "helper-b")]
    (xt/execute-tx (node)
                   [[:put-docs :gesso_v1482_prestate_probe before-a]
                    [:put-docs :gesso_v1482_prestate_probe before-b]])
    (assert-rejected!
     (execute-probe! [[:put-docs :gesso_v1482_prestate_probe after-a]
            [:put-docs :gesso_v1482_prestate_probe after-b]
            ;; A failing last statement must undo BOTH preceding puts.
            (assert-revision :gesso_v1482_prestate_probe id-a 1)]))
    (is (= before-a (current :gesso_v1482_prestate_probe id-a)))
    (is (= before-b (current :gesso_v1482_prestate_probe id-b)))))

(deftest cross-table-assert-sees-prior-membership-revocation
  (let [membership-id (unique-id "member")
        request-id (unique-id "request")
        membership (doc membership-id 1 "active")
        revoked (doc membership-id 2 "revoked")
        request (doc request-id 1 "unclaimed")
        claimed (doc request-id 2 "helper")]
    (xt/execute-tx (node)
                   [[:put-docs :gesso_v1482_prestate_memberships membership]
                    [:put-docs :gesso_v1482_prestate_probe request]])
    (assert-rejected!
     (execute-probe! [[:put-docs :gesso_v1482_prestate_memberships revoked]
            ;; A stale authorization read does NOT survive an earlier DML
            ;; statement in this same transaction.
            (assert-revision :gesso_v1482_prestate_memberships membership-id 1)
            [:put-docs :gesso_v1482_prestate_probe claimed]]))
    (is (= membership
           (current :gesso_v1482_prestate_memberships membership-id)))
    (is (= request (current :gesso_v1482_prestate_probe request-id)))))

(deftest canonical-gesso-lowering-checks-old-version-before-any-write
  (let [id (unique-id "canonical")
        before (doc id 1 "open")
        after (doc id 2 "helper")
        version {:revision-key :revision
                 :created-at-key :created-at
                 :updated-at-key :updated-at}
        operation (command/update-command :gesso_v1482_prestate_probe
                                          :claim before after version)
        ops (:tx-ops (tx/prepare {} {:commands [operation] :emit false}))]
    (is (= 2 (count ops)))
    (is (and (vector? (first ops))
             (string? (ffirst ops))
             (re-find #"(?i)^ASSERT\b" (ffirst ops))))
    (is (= :put-docs (first (last ops))))
    (put! :gesso_v1482_prestate_probe before)
    (assert-committed! (execute-probe! ops))
    (is (= after (current :gesso_v1482_prestate_probe id)))
    ;; This is a fresh second DML transaction, no cached external basis.
    (assert-rejected! (execute-probe! ops))
    (is (= after (current :gesso_v1482_prestate_probe id)))))

(deftest public-tx-key-and-read-only-snapshot-are-not-internal-prestate-evidence
  (let [id (unique-id "external-not-internal")
        before (doc id 1 "before")
        after (doc id 2 "after")]
    (put! :gesso_v1482_prestate_probe before)
    (let [token (:snapshot-token (first (xt/q (node) "SHOW SNAPSHOT_TOKEN")))
          basis {:snapshot-token token :current-time (java.time.Instant/now)}]
      (is (and (string? token) (seq token)))
      (is (= before (first (xt/q (node)
                                 ["SELECT _id, revision, owner, created_at, updated_at FROM gesso_v1482_prestate_probe WHERE _id = ?" id]
                                 basis))))
      (let [result (execute-probe! [[:put-docs :gesso_v1482_prestate_probe after]
                          (assert-revision :gesso_v1482_prestate_probe id 2)])]
        (assert-committed! result)
        (is (= after (current :gesso_v1482_prestate_probe id)))
        (is (= before (first (xt/q (node)
                                   ["SELECT _id, revision, owner, created_at, updated_at FROM gesso_v1482_prestate_probe WHERE _id = ?" id]
                                   basis))))
        (is (not (contains? (:tx-key result) :internal-prestate))
            "An execute-tx receipt is not an internal snapshot attestation")))))
