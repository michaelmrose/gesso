(ns gesso.assurance.reference-writer-temporal-coverage-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-writer-temporal-coverage :as coverage]
            [gesso.assurance.reference-temporal :as temporal])
  (:import (java.time Instant)))

(defn- at [seconds] (.plusSeconds (Instant/parse "2026-01-01T00:00:00Z") seconds))
(defn- tx [id time & ops] {:tx-id id :system-time (at time) :ops (vec ops)})
(defn- fixture []
  [[(tx 1 0 [:put-docs :manifest {:xt/id "m"}])
    (tx 3 1 [:put-docs :requests {:xt/id "r" :status :open}])
    (tx 7 5 [:put-docs :requests {:xt/id "r" :status :claimed}])
    (tx 9 8 [:put-docs :gesso_live_outbox {:xt/id "w"}])
    (tx 11 10 ["ASSERT FALSE"])]
   {:transaction-ids [1 3 7 9 11]}
   [{:tx-id 7 :class :committed-mapped} {:tx-id 9 :class :committed-worker}
    {:tx-id 11 :class :aborted-attempt}]
   #{:requests}])

(defn- check [args]
  (try (apply coverage/check-selection! args)
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest exact-diagonal-intervals
  (let [args (fixture) result (check args)]
    (is (= :conditional-selected-diagonal-coverage (:status result)))
    (is (= [[3 7] [7 9] [9 11]] (mapv (juxt :from-tx-id :to-tx-id) (:intervals result))))
    (is (= [:source-bound-committed-transition :maintenance-stutter :aborted-stutter]
           (mapv :schema (:intervals result))))
    (is (= :not-established (:universal-refinement result) (:replay-coverage result)))
    (is (= :not-performed (:proof-discharge result)))
    (is (= :temporal-selection-mismatch (check (update args 2 pop))))
    (is (= :temporal-selection-mismatch (check (assoc-in args [1 :transaction-ids] [1 3 7 9 12]))))
    (is (= :temporal-replay-or-commitment-mismatch (check (assoc-in args [2 2 :class] :committed-mapped))))
    (is (= :temporal-replay-or-commitment-mismatch (check (assoc-in args [0 3 :system-time] (at 5)))))
    (is (= :unknown-temporal-step-class (check (assoc-in args [2 0 :class] :foreign))))
    (with-redefs [temporal/replay (constantly {})]
      (is (= :temporal-replay-boundary-replaced (check args))))))

(deftest reject-hidden-valid-time-boundaries
  (let [args (fixture) put (fn [table from to]
                            [:put-docs {:into table :valid-from (at from) :valid-to (at to)}
                             {:xt/id "r" :status :temporary}])]
    ;; A transient change can disappear before the next transaction point.
    (doseq [[from to] [[2 3] [2 5] [5 6]]]
      (is (= :intervening-protected-valid-time-boundary
             (check (update-in args [0 1 :ops] conj (put :requests from to))))))
    (is (= :conditional-selected-diagonal-coverage
           (:status (check (update-in args [0 1 :ops] conj (put :unprotected 2 3))))))))
