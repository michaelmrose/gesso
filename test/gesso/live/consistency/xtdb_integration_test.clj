(ns gesso.live.consistency.xtdb-integration-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.live.consistency.xtdb :as xtdb-live]
   [xtdb.api :as xt]
   [xtdb.node :as xtn])
  (:import
   [java.time Instant]
   [xtdb.api TransactionKey]))

;; -----------------------------------------------------------------------------
;; Real XTDB fixture
;; -----------------------------------------------------------------------------

(defonce ^:private !node
  (atom nil))

(defn- start-test-node!
  []
  ;; Empty config uses XTDB's in-memory/default test-friendly configuration.
  (xtn/start-node {}))

(defn- node
  []
  (or @!node
      (throw
       (ex-info "XTDB test node is not started."
                {}))))

(defn- with-xtdb-node
  [f]
  (let [n (start-test-node!)]
    (reset! !node n)
    (try
      (f)
      (finally
        (reset! !node nil)
        (.close n)))))

(use-fixtures :once with-xtdb-node)

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defn- unique-id
  [prefix]
  (str prefix "-" (System/nanoTime) "-" (random-uuid)))

(defn- tx-key->map
  [tx-key]
  {:tx-id (.getTxId ^TransactionKey tx-key)
   :system-time (.getSystemTime ^TransactionKey tx-key)})

(defn- only-row
  [rows]
  (is (= 1 (count rows))
      (str "Expected exactly one row, got: " (pr-str rows)))
  (first rows))

(defn- row-id
  "Return XTDB's normalized document id column from a query row.

   Real XTDB returns SELECT _id as :xt/id here, not :_id."
  [row]
  (:xt/id row))

(defn- select-user-sql
  [id]
  ["SELECT _id, name FROM users WHERE _id = ?" id])

(defn- put-user-op
  [id name]
  [[:put-docs :users {:xt/id id
                      :name name}]])

(defn- q-user
  ([id]
   (xt/q (node) (select-user-sql id)))
  ([id opts]
   (xt/q (node) (select-user-sql id) opts)))

(defn- q-user-consistent
  [id consistency]
  (xtdb-live/q-consistent
   (node)
   (select-user-sql id)
   consistency))

;; -----------------------------------------------------------------------------
;; Baseline XTDB behavior
;; -----------------------------------------------------------------------------

(deftest real-xtdb-node-starts-and-queries-test
  (testing "The fixture starts a real XTDB node and basic SQL query works."
    (is (vector? (xt/q (node) "SELECT 1 AS one")))
    (is (= [{:one 1}]
           (xt/q (node) "SELECT 1 AS one")))))

(deftest real-execute-tx-returns-transaction-key-test
  (testing "XTDB execute-tx returns a TransactionKey with tx-id and system-time."
    (let [id     (unique-id "execute-return")
          result (xt/execute-tx (node) (put-user-op id "Ada"))]
      (is (instance? TransactionKey result))
      (is (integer? (.getTxId ^TransactionKey result)))
      (is (instance? Instant (.getSystemTime ^TransactionKey result))))))

(deftest real-submit-tx-returns-map-with-tx-id-test
  (testing "XTDB submit-tx public result is a map containing :tx-id."
    (let [id     (unique-id "submit-return")
          result (xt/submit-tx (node) (put-user-op id "Grace"))]
      (is (map? result))
      (is (contains? result :tx-id))
      (is (integer? (:tx-id result))))))

;; -----------------------------------------------------------------------------
;; Adapter transaction wrappers against real XTDB
;; -----------------------------------------------------------------------------

(deftest execute-tx-wrapper-derives-consistency-from-real-transaction-key-test
  (testing "execute-tx! wraps real XTDB execute-tx and derives :snapshot-time."
    (let [id          (unique-id "execute-wrapper")
          result      (xtdb-live/execute-tx! (node) (put-user-op id "Ada"))
          tx-result   (:tx-result result)
          consistency (:consistency result)]
      (is (instance? TransactionKey tx-result))
      (is (= (.getTxId ^TransactionKey tx-result)
             (:tx-id consistency)))
      (is (= (.getSystemTime ^TransactionKey tx-result)
             (:system-time consistency)))
      (is (= (:system-time consistency)
             (:snapshot-time consistency)))
      (is (instance? Instant (:snapshot-time consistency))))))

(deftest submit-tx-wrapper-preserves-real-submit-tx-metadata-test
  (testing "submit-tx! wraps real XTDB submit-tx and returns only public metadata."
    (let [id          (unique-id "submit-wrapper")
          result      (xtdb-live/submit-tx! (node) (put-user-op id "Grace"))
          tx-result   (:tx-result result)
          consistency (:consistency result)]
      (is (map? tx-result))
      (is (contains? tx-result :tx-id))
      (is (= (:tx-id tx-result)
             (:tx-id consistency)))
      (is (not (contains? consistency :snapshot-time)))
      (is (not (contains? consistency :system-time))))))

;; -----------------------------------------------------------------------------
;; Read-after-write behavior using real XTDB
;; -----------------------------------------------------------------------------

(deftest execute-tx-consistency-can-be-used-by-q-consistent-test
  (testing "A doc written with execute-tx! is visible through q-consistent using returned consistency."
    (let [id          (unique-id "execute-visible")
          name        "Visible Ada"
          result      (xtdb-live/execute-tx! (node) (put-user-op id name))
          consistency (:consistency result)
          row         (only-row (q-user-consistent id consistency))]
      (is (= id (row-id row)))
      (is (= name (:name row))))))

(deftest put-doc-wrapper-consistency-can-be-used-by-q-consistent-test
  (testing "put-doc! uses execute-tx! and its returned consistency can read the written doc."
    (let [id          (unique-id "put-doc-visible")
          name        "Put Doc Ada"
          result      (xtdb-live/put-doc! (node)
                                          :users
                                          {:xt/id id
                                           :name name})
          consistency (:consistency result)
          row         (only-row (q-user-consistent id consistency))]
      (is (= id (row-id row)))
      (is (= name (:name row))))))

(deftest q-consistent-from-uses-explicit-context-consistency-against-real-xtdb-test
  (testing "q-consistent-from reads explicit consistency from ctx and uses read-connectable-from."
    (let [id          (unique-id "ctx-visible")
          name        "Context Ada"
          result      (xtdb-live/execute-tx! (node) (put-user-op id name))
          consistency (:consistency result)
          ctx         {:xtdb/read-connectable (node)
                       :xtdb/consistency consistency}
          row         (only-row
                       (xtdb-live/q-consistent-from
                        ctx
                        (select-user-sql id)))]
      (is (= id (row-id row)))
      (is (= name (:name row))))))

;; -----------------------------------------------------------------------------
;; Query option behavior against real XTDB
;; -----------------------------------------------------------------------------

(deftest q-consistent-accepts-snapshot-time-from-execute-tx-test
  (testing "The :snapshot-time derived from execute-tx is accepted by real XTDB q."
    (let [id          (unique-id "snapshot-time")
          name        "Snapshot Ada"
          result      (xtdb-live/execute-tx! (node) (put-user-op id name))
          snapshot-t  (get-in result [:consistency :snapshot-time])
          row         (only-row
                       (xtdb-live/q-consistent
                        (node)
                        (select-user-sql id)
                        {:snapshot-time snapshot-t}))]
      (is (instance? Instant snapshot-t))
      (is (= id (row-id row)))
      (is (= name (:name row))))))

(deftest q-consistent-allows-explicit-opts-to-override-consistency-test
  (testing "This proves our merge contract against real XTDB, not just a mock."
    (let [id          (unique-id "override")
          name        "Override Ada"
          result      (xtdb-live/execute-tx! (node) (put-user-op id name))
          snapshot-t  (get-in result [:consistency :snapshot-time])
          row         (only-row
                       (xtdb-live/q-consistent
                        (node)
                        (select-user-sql id)
                        {:snapshot-time Instant/EPOCH}
                        {:snapshot-time snapshot-t}))]
      (is (= id (row-id row)))
      (is (= name (:name row))))))

(deftest plan-q-consistent-works-against-real-xtdb-test
  (testing "plan-q-consistent returns a reducible that sees the written row."
    (let [id          (unique-id "plan-q")
          name        "Plan Ada"
          result      (xtdb-live/execute-tx! (node) (put-user-op id name))
          consistency (:consistency result)
          rows        (into []
                            (xtdb-live/plan-q-consistent
                             (node)
                             (select-user-sql id)
                             consistency))
          row         (only-row rows)]
      (is (= id (row-id row)))
      (is (= name (:name row))))))

;; -----------------------------------------------------------------------------
;; Tx-op helpers against real XTDB
;; -----------------------------------------------------------------------------

(deftest put-docs-op-executes-against-real-xtdb-test
  (testing "Our generated :put-docs op is accepted by real XTDB."
    (let [id          (unique-id "put-docs-op")
          name        "Generated Put Ada"
          op          (xtdb-live/put-docs-op :users {:xt/id id :name name})
          result      (xtdb-live/execute-tx! (node) [op])
          row         (only-row (q-user-consistent id (:consistency result)))]
      (is (= id (row-id row)))
      (is (= name (:name row))))))

(deftest delete-docs-op-executes-against-real-xtdb-test
  (testing "Our generated :delete-docs op is accepted by real XTDB."
    (let [id            (unique-id "delete-docs-op")
          name          "Delete Ada"
          put-result    (xtdb-live/execute-tx! (node) (put-user-op id name))
          before-delete (only-row (q-user-consistent id (:consistency put-result)))
          delete-op     (xtdb-live/delete-docs-op :users id)
          del-result    (xtdb-live/execute-tx! (node) [delete-op])
          after-delete  (q-user-consistent id (:consistency del-result))]
      (is (= id (row-id before-delete)))
      (is (= [] after-delete)))))

;; -----------------------------------------------------------------------------
;; Observed submit-tx behavior
;; -----------------------------------------------------------------------------

(deftest submit-tx-does-not-return-query-usable-consistency-by-itself-test
  (testing "submit-tx! returns :tx-id metadata, not :snapshot-time/system-time."
    (let [id          (unique-id "submit-no-snapshot")
          result      (xtdb-live/submit-tx! (node) (put-user-op id "Async Ada"))
          consistency (:consistency result)]
      (is (contains? consistency :tx-id))
      (is (not (contains? consistency :snapshot-time)))
      (is (not (contains? consistency :system-time))))))

(deftest real-xtdb-submit-tx-may-still-be-visible-through-node-await-token-test
  (testing "This records XTDB DataSource behavior, not our adapter contract."
    (let [id     (unique-id "submit-await-token")
          name   "Async Visible Ada"
          result (xtdb-live/submit-tx! (node) (put-user-op id name))]
      ;; XTDB's own DataSource implementation may make this visible because
      ;; submit-tx mutates the DataSource await token internally and q uses it
      ;; as the default await-token. This test records that observed behavior.
      (is (contains? (:consistency result) :tx-id))
      (is (= [{:xt/id id :name name}]
             (q-user id))))))
