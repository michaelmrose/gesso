(ns gesso.model.tx-refinement-internal-basis-xtdb-test
  "Permanent bounded XTDB 2.2.0-beta1 regressions for v1386's distinction
   between an EXTERNAL read-only snapshot and a transaction's INTERNAL read
   state. These exercises use real embedded XTDB transactions; no production
   code is patched or replaced.

   Scope: fresh in-memory node, unique target IDs, current valid-time documents,
   SQL ASSERT-before-put plans, and observable committed/rejected results.
   A pinned external query is NOT an authenticated DML prestate; a supplied
   :snapshot-token DML option is NOT a supported basis selector. None of these
   tests proves general serializability, full writer coverage, authorization,
   internal transaction readset, or physical-execution refinement."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.model.command :as command]
   [gesso.live.consistency.xtdb :as xtdb-live]
   [gesso.model.tx :as tx]
   [xtdb.api :as xt]
   [xtdb.node :as xtn])
  (:import [java.time Instant]))

(defonce ^:private !node (atom nil))

(defn- with-embedded-xtdb [f]
  (with-open [db (xtn/start-node {})]
    (reset! !node db)
    (try (f)
         (finally (reset! !node nil)))))

(use-fixtures :once with-embedded-xtdb)

(defn- node []
  (or @!node (throw (ex-info "XTDB fixture is not running" {}))))

(defn- unique-id [label]
  (str "gesso-v1387-" label "-" (random-uuid)))

(defn- put! [table document]
  (xt/execute-tx (node) [[:put-docs table document]]))

(defn- read-revision
  ([id] (read-revision id {}))
  ([id opts]
   (first (xt/q (node)
                ["SELECT revision FROM gesso_internal_basis_probe WHERE _id = ?" id]
                opts))))

(defn- read-request [id]
  (first (xt/q (node)
               ["SELECT revision, owner, updated_at FROM requests WHERE _id = ?" id])))

(defn- read-membership [id]
  (first (xt/q (node)
               ["SELECT revision, active, updated_at FROM memberships WHERE _id = ?" id])))

(defn- external-basis []
  (let [token (:snapshot-token (first (xt/q (node) "SHOW SNAPSHOT_TOKEN")))
        clock (Instant/now)]
    (is (and (string? token) (seq token))
        "Embedded XTDB must expose a usable external read-only token")
    {:snapshot-token token :current-time clock}))

(defn- outcome [thunk]
  (try
    {:status :committed :tx-key (thunk)}
    (catch Throwable ex
      {:status :rejected
       :error/code (:xtdb.error/code (ex-data ex))
       :message (.getMessage ex)})))

(defn- assert-code! [result code]
  (is (= :rejected (:status result)) (pr-str result))
  (is (= code (:error/code result)) (pr-str result)))

(deftest external-snapshot-retains-the-original-read-and-clock-test
  (let [id (unique-id "pinned-read")]
    (put! :gesso_internal_basis_probe {:xt/id id :revision 1})
    (let [basis (external-basis)]
      (is (= {:revision 1} (read-revision id basis)))
      (put! :gesso_internal_basis_probe {:xt/id id :revision 2})
      (is (= {:revision 1} (read-revision id basis))
          "An external pinned read may see the older committed document")
      (is (= {:revision 1} (read-revision id basis))
          "The original token AND clock remain repeatable")
      (is (= {:revision 2} (read-revision id))
          "An unpinned read observes the intervening commit")
      (is (not= (read-revision id basis) (read-revision id))))))

(deftest before-and-after-read-bases-must-remain-phase-local-test
  (let [id (unique-id "phase-basis")]
    (put! :gesso_internal_basis_probe {:xt/id id :revision 1})
    (let [before-basis (external-basis)]
      (put! :gesso_internal_basis_probe {:xt/id id :revision 2})
      (let [after-basis (external-basis)]
        (is (not= (:snapshot-token before-basis)
                  (:snapshot-token after-basis))
            "After-phase pinned read must use a different committed horizon")
        (is (= {:revision 1} (read-revision id before-basis)))
        (is (= {:revision 2} (read-revision id after-basis)))
        (is (= {:revision 1} (read-revision id before-basis))
            "Reading after-state must not rewrite a prior phase's basis")))))

(deftest stale-read-token-cannot-force-a-stale-dml-assert-test
  (let [id (unique-id "stale-assert")]
    (put! :gesso_internal_basis_probe {:xt/id id :revision 1})
    (let [basis (external-basis)
          ;; A different thread completes an independent writer before DML.
          writer (future
                   (put! :gesso_internal_basis_probe {:xt/id id :revision 2}))]
      (is (not= ::timeout (deref writer 30000 ::timeout))
          "Independent writer must settle before the stale DML assertion")
      (is (= {:revision 1} (read-revision id basis)))
      (is (= {:revision 2} (read-revision id)))
      (let [result (outcome
                    #(xt/execute-tx
                      (node)
                      [["ASSERT 1 = (SELECT COUNT(*) FROM gesso_internal_basis_probe WHERE _id = ? AND revision = 1)" id]
                       [:put-docs :gesso_internal_basis_probe {:xt/id id :revision 3}]]
                      {:snapshot-token (:snapshot-token basis)}))]
        (assert-code! result :xtdb/assert-failed)
        (is (= {:revision 2} (read-revision id))
            "Rejected ASSERT must not apply the subsequent put")
        (is (= {:revision 1} (read-revision id basis))
            "Rejected DML must not alter the earlier external read")))))

(deftest matching-current-assert-is-not-forced-backward-by-stale-token-test
  (let [id (unique-id "fresh-assert")]
    (put! :gesso_internal_basis_probe {:xt/id id :revision 1})
    (let [basis (external-basis)]
      (put! :gesso_internal_basis_probe {:xt/id id :revision 2})
      (let [result (outcome
                    #(xt/execute-tx
                      (node)
                      [["ASSERT 1 = (SELECT COUNT(*) FROM gesso_internal_basis_probe WHERE _id = ? AND revision = 2)" id]
                       [:put-docs :gesso_internal_basis_probe {:xt/id id :revision 3}]]
                      {:snapshot-token (:snapshot-token basis)}))]
        (is (= :committed (:status result)) (pr-str result))
        (is (some? (:tx-key result)))
        (is (= {:revision 3} (read-revision id)))
        (is (= {:revision 1} (read-revision id basis)))
        (is (not (contains? (:tx-key result) :transaction-internal-prestate))
            "Returned tx key must not be promoted into a basis attestation")))))

(deftest dml-cannot-query-its-own-snapshot-token-test
  (let [id (unique-id "dml-show")]
    (put! :gesso_internal_basis_probe {:xt/id id :revision 1})
    (let [result (outcome
                  #(xt/execute-tx
                    (node)
                    [["SHOW SNAPSHOT_TOKEN"]
                     [:put-docs :gesso_internal_basis_probe {:xt/id id :revision 2}]]))]
      (assert-code! result :xtdb/queries-in-read-write-tx)
      (is (= {:revision 1} (read-revision id))
          "A rejected DML query cannot be used to acquire transaction basis"))))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(deftest canonical-gesso-guard-reads-current-not-prior-external-basis-test
  (let [id (unique-id "canonical")
        mid (unique-id "membership")
        request {:xt/id id :revision 0 :created-at "t0"
                 :updated-at "t0" :owner "unclaimed"}
        member {:xt/id mid :revision 0 :created-at "t0"
                :updated-at "t0" :active true}]
    (xt/execute-tx (node) [[:put-docs :requests request]
                           [:put-docs :memberships member]])
    (let [basis (external-basis)
          ;; The application read was performed while membership was active.
          ;; A separate writer now invalidates that source-derived guard.
          revoked (assoc member :revision 1 :active false :updated-at "t1")
          _ (put! :memberships revoked)
          updated (assoc request :revision 1 :owner "helper"
                         :updated-at "t1")
          operation (command/update-command :requests :claim request updated version)
          guard (command/guard :memberships member version)
          prepared (tx/prepare {} {:commands [operation]
                                   :guards [guard]
                                   :emit false})
          ops (:tx-ops prepared)]
      (is (seq ops))
      (is (every? #(and (vector? %) (= :put-docs (first %)))
                  (take-last 1 ops)))
      (is (every? (fn [op]
                    (and (vector? op)
                         (string? (first op))
                         (re-find #"(?i)^ASSERT\b" (first op))))
                  (butlast ops))
          "Canonical Gesso lowering places all SQL ASSERTs before writes")
      (is (= {:revision 0 :active true :updated-at "t0"}
             (first (xt/q (node)
                          ["SELECT revision, active, updated_at FROM memberships WHERE _id = ?" mid]
                          basis))))
      (is (= {:revision 1 :active false :updated-at "t1"}
             (read-membership mid)))
      (let [result (outcome #(xt/execute-tx (node) ops))]
        (assert-code! result :xtdb/assert-failed)
        (is (= {:revision 0 :owner "unclaimed" :updated-at "t0"}
               (read-request id))
            "The stale guard rejects without writing the request")
        (is (= {:revision 1 :active false :updated-at "t1"}
               (read-membership mid)))))))

(deftest read-only-query-basis-is-stripped-from-gesso-dml-options-test
  (let [id (unique-id "write-option")
        request {:xt/id id :revision 0 :created-at "t0"
                 :updated-at "t0" :owner "unclaimed"}
        updated (assoc request :revision 1 :updated-at "t1" :owner "helper")
        operation (command/update-command :requests :claim request updated version)
        options {:snapshot-token "untrusted-old-token"
                 :current-time (Instant/now)
                 :metadata {:test "v1387"}
                 :database "xtdb"}
        prepared (tx/prepare {} {:commands [operation]
                                 :emit false
                                 :tx-options options})
        admitted (xtdb-live/tx-opts (get-in prepared [:plan :tx-options]))]
    (is (= options (get-in prepared [:plan :tx-options]))
        "Plan preparation preserves options but does not attest their meaning")
    (is (= {:metadata {:test "v1387"} :database "xtdb"} admitted)
        "The Gesso XTDB adapter forwards only its allowlisted DML options")
    (is (not (contains? admitted :snapshot-token)))
    (is (not (contains? admitted :current-time)))
    (is (nil? (read-request id))
        "Source-side option inspection is not a physical transaction")))
