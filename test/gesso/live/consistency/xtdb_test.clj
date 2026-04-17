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

(deftest connectable-from-ctx-test
  (testing "prefers :biff/conn"
    (is (= *conn*
           (live.xtdb/connectable-from-ctx (ctx)))))

  (testing "falls back to :biff/node"
    (is (= *node*
           (live.xtdb/connectable-from-ctx {:biff/node *node*})))))

(deftest current-await-token-test
  (is (= (.getAwaitToken *conn*)
         (live.xtdb/current-await-token *conn*))))

(deftest apply-await-token-test
  (let [token "fake-token"]
    (live.xtdb/apply-await-token! *conn* token)
    (is (= token
           (.getAwaitToken *conn*)))))

(deftest submit-tx-raw-test
  (let [tx [[:put-docs :requests
             {:xt/id "req-raw-1"
              :status "open"}]]
        tx-result (live.xtdb/submit-tx-raw! *conn* tx)]
    (is (map? tx-result))
    (is (contains? tx-result :tx-id))
    (is (some? (live.xtdb/current-await-token *conn*)))))

(deftest q-roundtrip-test
  (xt/submit-tx *conn*
                [[:put-docs :requests
                  {:xt/id "req-q-1"
                   :status "open"}]])

  (let [query "SELECT _id, status FROM requests WHERE _id = 'req-q-1'"
        result (live.xtdb/q (ctx) query)]
    (is (sequential? result))
    (is (= [{:xt/id "req-q-1"
             :status "open"}]
           (vec result)))))

(deftest submit-tx-test
  (let [changed {:entity/type :request
                 :entity/id "req-1"
                 :change/kind :updated}
        tx [[:put-docs :requests
             {:xt/id "req-1"
              :status "open"}]]
        result (live.xtdb/submit-tx!
                (ctx)
                {:tx tx
                 :changed changed
                 :data {:reason :request-updated}})
        token (:consistency-token result)
        query "SELECT _id, status FROM requests WHERE _id = 'req-1'"
        rows (live.xtdb/q (ctx {:headers {"x-gesso-live-consistency-token" token}})
                          query)]
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
  ;; first create a real token by writing through the XTDB connectable
  (xt/submit-tx *conn*
                [[:put-docs :requests
                  {:xt/id "req-token-1"
                   :status "open"}]])
  (let [token (.getAwaitToken *conn*)
        query "SELECT _id, status FROM requests WHERE _id = 'req-token-1'"]
    ;; now pass that real token through the request header
    (live.xtdb/q (ctx {:headers {"x-gesso-live-consistency-token" token}})
                 query)
    ;; and assert it was applied to the connectable
    (is (= token
           (.getAwaitToken *conn*)))))

(deftest submit-tx-custom-event-test
  (let [result (live.xtdb/submit-tx!
                (ctx)
                {:tx [[:put-docs :requests
                       {:xt/id "req-2"
                        :status "open"}]]
                 :changed {:entity/type :request
                           :entity/id "req-2"
                           :change/kind :updated}
                 :event "custom-event"})]
    (is (= "custom-event"
           (get-in result [:published :event])))))

(deftest with-consistency-test
  (let [token "forwarded-token"
        result (live.xtdb/with-consistency
                (ctx {:headers {"x-gesso-live-consistency-token" token}})
                identity)]
    (is (= {:consistency-token token}
           result))))
