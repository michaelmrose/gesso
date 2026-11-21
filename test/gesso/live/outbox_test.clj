(ns gesso.live.outbox-test
  "v2088 tests for transactional publication intent construction.

   These tests establish input validation and same-submission XTDB insertion,
   not post-commit replay, publisher fairness, source authenticity, fencing,
   browser receipt, or at-least-once delivery."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.consistency.xtdb :as xtdb-live]
   [gesso.live.core :as live]
   [gesso.live.outbox :as outbox]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]))

(defn- specification [intent-ids]
  {:intent-ids intent-ids
   :source-id "gesso/example/claim-v1"
   :operation :request/claim
   :purpose :gesso.live/refresh
   :command-id "command-1"})

(defn- change [id]
  {:topic :request
   :id id
   :change/kind :updated
   :secret {:do-not-persist "sensitive"}})

(defn- rejected? [f reason]
  (let [error (try (f) nil (catch clojure.lang.ExceptionInfo e e))]
    (and error
         (= :gesso.live.outbox/invalid-intent
            (:error/type (ex-data error)))
         (= reason (:reason (ex-data error))))))

(deftest prepare-intent-minimal-serializable-projection-test
  (let [ids [(random-uuid) (random-uuid)]
        changes [(change "a") (change "b")]
        plan (outbox/prepare! (specification ids) changes)
        intents (:intents plan)]
    (is (= 2 (count intents)))
    (is (= (into [:put-docs :gesso_live_outbox] intents)
           (:tx-op plan)))
    (is (= ids (mapv :xt/id intents)))
    (is (every? #(= :gesso.live.outbox/publication-intent
                    (:gesso.live.outbox/type %)) intents))
    (is (every? #(= :pending (:gesso.live.outbox/status %)) intents))
    (is (= ["a" "b"] (mapv :gesso.live.outbox/subject-id intents)))
    (is (every? #(= 1 (:gesso.live.outbox/version %)) intents))
    (is (every? #(= :request/claim (:gesso.live.outbox/operation %)) intents))
    (is (every? #(= "command-1" (:gesso.live.outbox/command-id %)) intents))
    (is (every? #(not (contains? % :secret)) intents))
    (is (every? #(not (contains? % :progression)) intents))
    (is (every? #(not (contains? % :gesso.live.outbox/progression)) intents))
    (is (every? #(not-any? (fn [v] (and (map? v) (contains? v :do-not-persist)))
                              (vals %)) intents))))

(deftest prepare-intent-rejects-unsafe-declarations-test
  (let [id (random-uuid)
        base (specification [id])
        changes [(change "a")]]
    (doseq [[spec expected]
            [[nil :invalid-spec]
             [(assoc base :source-id "  ") :invalid-source]
             [(assoc base :operation :claim) :invalid-operation]
             [(assoc base :purpose :refresh) :invalid-purpose]
             [(assoc base :command-id {:untrusted true}) :invalid-command-id]
             [(assoc base :unexpected :silent) :unknown-fields]
             [(assoc base :intent-ids []) :invalid-intent-identities]
             [(assoc base :intent-ids ["not-a-uuid"]) :invalid-intent-identities]
             [(assoc base :intent-ids [id id]) :invalid-intent-identities]
             [(assoc base :command-id id) :invalid-intent-identities]]]
      (is (rejected? #(outbox/prepare! spec changes) expected)
          (str "reject " (pr-str spec))))
    (is (rejected? #(outbox/prepare! base []) :missing-changes))
    (is (rejected? #(outbox/prepare! base [(dissoc (change "a") :id)])
                   :invalid-change))
    (is (rejected? #(outbox/prepare! base [(assoc (change "a") :topic "raw")])
                   :invalid-change))
    (is (rejected? #(outbox/prepare! base [(assoc (change "a") :id {:secret 1})])
                   :invalid-change))))

(deftest transact-and-notify-submits-domain-and-intent-together-test
  (let [intent-id (random-uuid)
        original [[:put-docs :requests {:xt/id "a" :status :open}]]
        seen (atom [])
        spec (specification [intent-id])]
    (with-redefs [xtdb-live/execute-tx-from!
                  (fn [_ctx ops _opts]
                    (swap! seen conj ops)
                    {:tx-result {:tx-id 42}
                     :consistency {:tx-id 42}})]
      (let [result (live/transact-and-notify!
                    {:options {}}
                    {:xtdb/connectable :stub}
                    {:tx-ops original
                     :change (change "a")
                     :emit false
                     :outbox spec})]
        (is (= 1 (count @seen)))
        (is (= original (subvec (first @seen) 0 1)))
        (is (= [:put-docs :gesso_live_outbox]
               (subvec (last (first @seen)) 0 2)))
        (is (= [intent-id] (mapv :xt/id (:outbox/intents result))))
        (is (= (:outbox/intents result)
               (subvec (last (first @seen)) 2)))
        (is (= [] (:emit-results result)))))))

(deftest invalid-intent-aborts-before-physical-write-test
  (let [writes (atom 0)
        spec (assoc (specification [(random-uuid)]) :operation :unqualified)]
    (with-redefs [xtdb-live/execute-tx-from!
                  (fn [& _] (swap! writes inc) (throw (ex-info "wrote" {})))]
      (is (rejected? #(live/transact-and-notify!
                       {:options {}}
                       {:xtdb/connectable :stub}
                       {:tx-ops [[:put-docs :requests {:xt/id "a"}]]
                        :change (change "a")
                        :emit false
                        :outbox spec})
                     :invalid-operation))
      (is (zero? @writes))
      (is (rejected? #(live/transact-and-notify!
                       {:options {}}
                       {:xtdb/connectable :stub}
                       {:tx-ops [[:put-docs :requests {:xt/id "a"}]]
                        :emit false
                        :outbox (specification [(random-uuid)])})
                     :missing-changes))
      (is (zero? @writes)))))

(deftest physical-xtdb-commits-domain-and-intent-together-test
  ;; A controlled embedded node shows a single actual XTDB write carrying
  ;; both documents. It does not establish replay or absence of competing IDs.
  (with-open [node (xtn/start-node {})]
    (let [domain-id (str "outbox-domain-" (random-uuid))
          intent-id (random-uuid)
          result (live/transact-and-notify!
                  {:options {}}
                  {:xtdb/connectable node}
                  {:tx-ops [[:put-docs :requests {:xt/id domain-id :status :open}]]
                   :change (change domain-id)
                   :emit false
                   :outbox (specification [intent-id])})
          domain-rows (xt/q node ["SELECT _id FROM requests WHERE _id = ?" domain-id])
          outbox-rows (xt/q node ["SELECT _id FROM gesso_live_outbox WHERE _id = ?" intent-id])]
      (is (some? (get-in result [:consistency :tx-id])))
      (is (= [domain-id] (mapv :xt/id domain-rows)))
      (is (= [intent-id] (mapv :xt/id outbox-rows)))
      (is (= intent-id (get-in result [:outbox/intents 0 :xt/id]))))))
