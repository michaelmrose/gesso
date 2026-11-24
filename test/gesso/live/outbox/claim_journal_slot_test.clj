(ns gesso.live.outbox.claim-journal-slot-test
  "Physical coverage for the optional principal/operation journal slot."
  (:require [clojure.test :refer [deftest is]]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.outbox.model-publication :as publication]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- binding-for [id helper operation]
  {:command-id (identity/command-id id) :principal (identity/principal helper)
   :operation operation :arguments {:request-id "r1"}})

(defn- attempt! [node binding guard?]
  (let [record (resolution/command-record binding {:outcome :request/not-claimable} "t1")
        ops (cond-> [] guard? (conj (resolution/principal-operation-slot-assertion binding))
              true (conj ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_commands WHERE _id = ?)" (:xt/id record)]
                         [:put-docs :gesso_commands record]))]
    (try (xtdb/execute-tx! node ops) :committed
         (catch Exception e
           (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data e)))
             :xtdb/assert-failed (throw e))))))

(deftest distinct-command-ids-contend-for-one-durable-principal-operation-slot
  (with-open [node (xtn/start-node {})]
    (let [a (binding-for "a" "helper-1" :request/claim)
          b (binding-for "b" "helper-1" :request/claim)
          gate (CountDownLatch. 2)
          contender (fn [binding] (future (.countDown gate)
                                          (when-not (.await gate 10 TimeUnit/SECONDS)
                                            (throw (ex-info "Slot contention timed out" {})))
                                          (attempt! node binding true)))
          one (contender a) two (contender b)
          results [(deref one 20000 :timeout) (deref two 20000 :timeout)]
          rows (fn [] (xt/q node ["SELECT * FROM gesso_commands"]))]
      (is (= {:committed 1 :xtdb/assert-failed 1} (frequencies results)))
      (is (= 1 (count (rows))))
      (let [winner (if (= :committed (first results)) a b)
            loser (if (= winner a) b a)]
        (is (= winner (:gesso.command/binding (first (rows)))))
        (is (= :xtdb/assert-failed (attempt! node loser true)))
        (is (= 1 (count (rows))))
        (is (= :committed (attempt! node (binding-for "c" "helper-2" :request/claim) true)))
        (is (= :committed (attempt! node (binding-for "d" "helper-1" :request/other) true)))
        (is (= 3 (count (rows))))
        ;; Negative control: command-ID absence ALONE does not protect the slot.
        (is (= :committed (attempt! node loser false)))
        (is (= 4 (count (rows)))))))
  (is (= :unsupported-principal-slot
         (try (resolution/principal-operation-slot-assertion (binding-for "e" :helper/one :request/claim))
              nil (catch clojure.lang.ExceptionInfo e (:error/kind (ex-data e)))))))

(deftest principal-slot-requires-explicit-contracted-opt-in
  (let [config {:operation :request/claim :submission-mode! (constantly :submit)
                :construct! identity :may-disclose? identity :project! identity}
        reason (fn [cfg] (try (publication/build-executor cfg) nil
                             (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (is (fn? (publication/build-executor config)))
    (doseq [value [true false nil :truthy]]
      (is (= :invalid-principal-slot-contract
             (reason (assoc config :single-command-per-principal? value)))))))
