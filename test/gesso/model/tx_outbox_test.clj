(ns gesso.model.tx-outbox-test
  "v2343: Physical atomic publication intent through ordinary model plans."
  (:require [clojure.test :refer [deftest is]]
            [gesso.live.consistency.xtdb :as xtdb-live]
            [gesso.model.command :as command]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(def ^:private version
  {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at})

(defn- plan [id]
  {:commands [(command/create :model_outbox_widget
                              {:xt/id id :revision 0 :created-at "t0" :updated-at "t0"}
                              version)]
   :changes [{:topic :widget :id id :change/kind :created}]
   :emit false})

(defn- declaration [id]
  {:intent-ids [id]
   :source-id "gesso/model-outbox-test"
   :operation :widget/create
   :purpose :gesso.live/refresh})

(defn- rejected [f]
  (try (f) nil (catch Exception e (or (ex-data e) (throw e)))))

(deftest model-outbox-declaration-is-explicit-and-validated-before-submit
  (let [p (plan "invalid-outbox")
        d (declaration (random-uuid))
        attempts (atom 0)]
    (is (= ::tx/unknown-plan-keys
           (:error/type (rejected #(tx/prepare {} (assoc p :outbox d))))))
    (is (not (contains? (:plan (tx/prepare {} p)) :outbox)))
    (with-redefs [xtdb-live/execute-tx-from!
                  (fn [& _] (swap! attempts inc) (throw (ex-info "unexpected write" {})))]
      (doseq [[candidate decl reason] [[p nil :invalid-spec]
                                      [p (assoc d :operation :unqualified) :invalid-operation]
                                      [(dissoc p :changes) d :missing-changes]]]
        (let [error (rejected #(tx/transact-with-outbox! {} candidate decl))]
          (is (= :gesso.live.outbox/invalid-intent (:error/type error)))
          (is (= reason (:reason error)))))
      (is (zero? @attempts)))))

(defn- row-ids [node table]
  (mapv :xt/id (xt/q node [(str "SELECT _id FROM " table)])))

(deftest physical-model-command-and-pending-intent-commit-or-rollback-together
  (with-open [node (xtn/start-node {})]
    (let [ctx {:xtdb/connectable node}
          first-id "model-outbox-first"
          second-id "model-outbox-second"
          intent-id (random-uuid)
          fresh-id (random-uuid)
          spec (declaration intent-id)
          result (tx/transact-with-outbox! ctx (plan first-id) spec)]
      (is (= :committed (:commit/status result)))
      (is (= [] (:emit-results result)))
      (is (= [intent-id] (mapv :xt/id (:outbox/intents result))))
      (is (= :pending (get-in result [:outbox/intents 0 :gesso.live.outbox/status])))
      (is (= [first-id] (row-ids node "model_outbox_widget")))
      (is (= [intent-id] (row-ids node "gesso_live_outbox")))
      ;; Reusing a durable intent identity must roll back a DIFFERENT domain
      ;; create. A conflicting domain create must roll back a NEW intent.
      (is (= :xtdb/assert-failed
             (:xtdb.error/code (rejected #(tx/transact-with-outbox! ctx (plan second-id) spec)))))
      (is (= :xtdb/assert-failed
             (:xtdb.error/code (rejected #(tx/transact-with-outbox! ctx (plan first-id)
                                                                   (declaration fresh-id))))))
      (is (= [first-id] (row-ids node "model_outbox_widget")))
      (is (= [intent-id] (row-ids node "gesso_live_outbox")))
      ;; The old transaction path still creates ordinary domain documents
      ;; without silently recording publication intents.
      (is (= :committed (:commit/status (tx/transact! ctx (plan second-id)))))
      (is (= #{first-id second-id} (set (row-ids node "model_outbox_widget"))))
      (is (= [intent-id] (row-ids node "gesso_live_outbox"))))))
