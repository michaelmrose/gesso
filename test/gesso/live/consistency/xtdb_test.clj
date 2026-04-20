(ns gesso.live.consistency.xtdb-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.live.bus :as bus]
   [gesso.live.consistency.xtdb :as live.xtdb]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]))

(def ^:dynamic *node*)
(def ^:dynamic *conn*)

(def trivial-matcher
  {:subscription->entries (fn [_subscription] [])
   :changed->entries (fn [_ctx _changed] [])})

(defn with-node-and-conn
  [f]
  (with-open [node (xtn/start-node)]
    (binding [*node* node
              *conn* node]
      (f))))

(use-fixtures :each with-node-and-conn)

(defn ctx
  ([] (ctx {}))
  ([m]
   (merge
    {:biff/node *node*
     :biff/conn *conn*
     :gesso.live/bus (bus/memory-bus trivial-matcher)}
    m)))

(defn request-query
  [id]
  (str "SELECT _id, status FROM requests WHERE _id = '" id "'"))

(defn request-rows-raw
  [id]
  (live.xtdb/q-raw *conn* (request-query id)))

(defn request-rows
  ([id]
   (live.xtdb/q (ctx) (request-query id)))
  ([ctx-map id]
   (live.xtdb/q ctx-map (request-query id))))

(defn request-doc
  [id status]
  {:xt/id id
   :status status})

(defn put-request-tx
  [id status]
  [[:put-docs :requests
    (request-doc id status)]])

(deftest connectable-from-ctx-test
  (testing "prefers :biff/conn"
    (is (= *conn*
           (live.xtdb/connectable-from-ctx (ctx)))))

  (testing "falls back to :biff/node"
    (is (= *node*
           (live.xtdb/connectable-from-ctx {:biff/node *node*})))))

(deftest node-from-ctx-test
  (is (= *node*
         (live.xtdb/node-from-ctx (ctx)))))

(deftest await-token-source-from-ctx-test
  (is (= *node*
         (live.xtdb/await-token-source-from-ctx (ctx)))))

(deftest current-await-token-test
  (is (= (.getAwaitToken *conn*)
         (live.xtdb/current-await-token *conn*))))

(deftest extract-consistency-token-test
  (is (= (.getAwaitToken *conn*)
         (live.xtdb/extract-consistency-token *conn*))))

(deftest apply-await-token-test
  (let [token "fake-token"]
    (live.xtdb/apply-await-token! *conn* token)
    (is (= token
           (.getAwaitToken *conn*)))))

(deftest submit-tx-raw-test
  (let [tx (put-request-tx "req-raw-1" "open")
        tx-result (live.xtdb/submit-tx-raw! *conn* tx)]
    (is (map? tx-result))
    (is (contains? tx-result :tx-id))
    (is (some? (live.xtdb/current-await-token *conn*)))))

(deftest q-raw-roundtrip-test
  (xt/submit-tx *conn* (put-request-tx "req-qraw-1" "open"))
  (let [rows (request-rows-raw "req-qraw-1")]
    (is (sequential? rows))
    (is (= [{:xt/id "req-qraw-1"
             :status "open"}]
           (vec rows)))))

(deftest q-roundtrip-test
  (xt/submit-tx *conn* (put-request-tx "req-q-1" "open"))
  (let [result (request-rows "req-q-1")]
    (is (sequential? result))
    (is (= [{:xt/id "req-q-1"
             :status "open"}]
           (vec result)))))

(deftest submit-tx-test
  (let [changed {:entity/type :request
                 :entity/id "req-1"
                 :change/kind :updated}
        tx (put-request-tx "req-1" "open")
        result (live.xtdb/submit-tx!
                (ctx)
                {:tx tx
                 :changed changed
                 :data {:reason :request-updated}})
        token (:consistency-token result)
        rows (request-rows
              (ctx {:headers {"x-gesso-live-consistency-token" token}})
              "req-1")]
    (is (map? (:tx-result result)))
    (is (contains? (:tx-result result) :tx-id))
    (is (= (.getAwaitToken *conn*)
           token))
    (is (= {:event "live-update"
            :changed changed
            :consistency-token token
            :data {:reason :request-updated}}
           (:published result)))
    (is (= [{:xt/id "req-1"
             :status "open"}]
           (vec rows)))))

(deftest q-applies-request-token-test
  (xt/submit-tx *conn* (put-request-tx "req-token-1" "open"))
  (let [token (.getAwaitToken *conn*)]
    (live.xtdb/q
     (ctx {:headers {"x-gesso-live-consistency-token" token}})
     (request-query "req-token-1"))
    (is (= token
           (.getAwaitToken *conn*)))))

(deftest submit-tx-custom-event-test
  (let [result (live.xtdb/submit-tx!
                (ctx)
                {:tx (put-request-tx "req-2" "open")
                 :changed {:entity/type :request
                           :entity/id "req-2"
                           :change/kind :updated}
                 :event "custom-event"})]
    (is (= "custom-event"
           (get-in result [:published :event])))))

(deftest submit-and-await-tx!-test
  (let [id "req-await-1"
        changed {:entity/type :request
                 :entity/id id
                 :change/kind :updated}
        result (live.xtdb/submit-and-await-tx!
                (ctx)
                {:tx (put-request-tx id "open")
                 :changed changed
                 :data {:reason :visible-write}})]
    (is (some? (:tx-result result)))
    (is (some? (:tx-id (:tx-result result))))
    (is (= {:event "live-update"
            :changed changed
            :consistency-token nil
            :data {:reason :visible-write}}
           (:published result)))
    (is (= [{:xt/id id
             :status "open"}]
           (vec (request-rows-raw id))))))

(deftest submit-and-await-tx!-custom-event-test
  (let [id "req-await-2"
        result (live.xtdb/submit-and-await-tx!
                (ctx)
                {:tx (put-request-tx id "open")
                 :changed {:entity/type :request
                           :entity/id id
                           :change/kind :updated}
                 :event "request-changed"})]
    (is (= "request-changed"
           (get-in result [:published :event])))))

(deftest put-and-publish!-test
  (let [id "req-put-1"
        changed {:entity/type :request
                 :entity/id id
                 :change/kind :updated}
        result (live.xtdb/put-and-publish!
                (ctx)
                {:table :requests
                 :doc (request-doc id "open")
                 :changed changed
                 :data {:reason :put-and-publish}})]
    (is (some? (:tx-result result)))
    (is (some? (:tx-id (:tx-result result))))
    (is (= {:event "live-update"
            :changed changed
            :consistency-token nil
            :data {:reason :put-and-publish}}
           (:published result)))
    (is (= [{:xt/id id
             :status "open"}]
           (vec (request-rows-raw id))))))

(deftest put-and-publish!-missing-table-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Missing required XTDB table"
       (live.xtdb/put-and-publish!
        (ctx)
        {:doc (request-doc "req-put-missing-table" "open")
         :changed {:entity/type :request
                   :entity/id "req-put-missing-table"
                   :change/kind :updated}}))))

(deftest put-and-publish!-missing-doc-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Missing required XTDB document"
       (live.xtdb/put-and-publish!
        (ctx)
        {:table :requests
         :changed {:entity/type :request
                   :entity/id "req-put-missing-doc"
                   :change/kind :updated}}))))

(deftest with-consistency-test
  (let [token "forwarded-token"
        result (live.xtdb/with-consistency
                (ctx {:headers {"x-gesso-live-consistency-token" token}})
                identity)]
    (is (= {:consistency-token token}
           result))))
