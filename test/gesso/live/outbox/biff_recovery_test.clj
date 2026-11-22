(ns gesso.live.outbox.biff-recovery-test
  "Biff provider teardown must own explicitly restarted worker descendants."
  (:require [clojure.test :refer [deftest is]]
            [com.biffweb.core :as biff]
            [gesso.live.core :as live]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.outbox.cycle :as cycle]
            [gesso.live.outbox.delivery :as delivery]
            [gesso.live.outbox.acknowledgment :as acknowledgment]
            [gesso.live.outbox.lifecycle :as lifecycle]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(def ^:private source-id "gesso/biff-managed-test")
(def ^:dynamic *modules* [])

(defn- config [extras]
  (merge {managed/source-id-key source-id
          managed/options-key {:worker-id "biff-worker" :poll-ms 20}
          managed/context-key {}}
         extras))

(defn- live-system []
  (live/create {:rules [{:when-topic :request
                         :expand (fn [_ change]
                                   [(select-keys change [:topic :id])])}]
                :source-options {:coalesce-window-ms 0}
                :dispatch-options {:threads 1 :queue-size 8 :on-overflow :throw}}))

(defn- poll-until [f]
  (loop [n 0]
    (cond (f) true
          (>= n 250) false
          :else (do (Thread/sleep 20) (recur (inc n))))))

(defn- invalid-reason [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e
         (when (= :gesso.live.outbox/invalid-biff-lifecycle
                  (:error/type (ex-data e)))
           (:reason (ex-data e))))))

(defn- lifecycle-reason [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e
         (when (= :gesso.live.outbox/invalid-lifecycle
                  (:error/type (ex-data e)))
           (:reason (ex-data e))))))

(deftest biff-teardown-retires-and-drains-physical-recovery-successor-test
  ;; One worker physically acknowledges its intent and loses the reply.
  ;; The original Biff system owns that failed worker. After explicit recovery
  ;; it MUST also own its successor: a timeout cannot close the provider;
  ;; a repeated native stop drains it; no restart can race past retirement.
  (with-open [node (xtn/start-node {})]
    (let [intent (random-uuid)
          receipt (random-uuid)
          plan (outbox/prepare!
                {:intent-ids [intent] :source-id source-id
                 :operation :request/claim :purpose :gesso.live/refresh}
                [{:topic :request :id "biff-restart"}])
          _ (xt/execute-tx node (conj (vec (:guard-ops plan)) (:tx-op plan)))
          live (live-system)
          events (atom [])
          entered (CountDownLatch. 1)
          release (CountDownLatch. 1)
          real-cycle cycle/process-cycle!
          provider {:biff.core/id :fixture/owned-providers
                    :biff.core/start
                    (fn [system]
                      (assoc system :xtdb/connectable node
                             :gesso.live/system live))
                    :biff.core/stop
                    (fn [_]
                      (swap! events conj :providers-closed)
                      (live/close! live))}]
      (try
        (with-redefs [delivery/dispatch-acquired!
                      (fn [_ _ _ acquired _]
                        (is (= :acknowledged
                               (:status (acknowledgment/acknowledge-dispatch!
                                         node {:intent-id (:intent-id acquired)
                                               :lease-id (:lease-id acquired)
                                               :worker-id (:worker-id acquired)
                                               :receipt {:receipt-id receipt
                                                         :adapter :gesso.live/synchronous-source
                                                         :result :accepted}}))))
                        (throw (ex-info "ack committed but adapter reply lost"
                                        {:error/type
                                         :gesso.live.outbox/post-dispatch-ack-unknown
                                         :intent-id (:intent-id acquired)
                                         :lease-id (:lease-id acquired)
                                         :receipt-id receipt
                                         :dispatch/status :accepted
                                         :acknowledgment/status :unknown})))
                      cycle/process-cycle!
                      (fn [node system ctx source opts]
                        (if (= "biff-successor" (:worker-id opts))
                          (do
                            (.countDown entered)
                            (.await release)
                            (real-cycle node system ctx source opts))
                          (real-cycle node system ctx source opts)))]
          (binding [*modules* [provider managed/module]]
            (let [started (biff/start
                           (config {managed/stop-timeout-key 20})
                           #'*modules* [:fixture/owned-providers managed/module-id])
                  old (get started managed/handle-key)]
              (try
                (is (poll-until #(= :failed (:status (lifecycle/status old)))))
                (let [{child :handle reconciliation :reconciliation}
                      (lifecycle/restart-recorded!
                       old {:worker-id "biff-successor" :poll-ms 20})]
                  (is (= :matching-acknowledgment-recorded
                         (:status reconciliation)))
                  (is (.await entered 3000 TimeUnit/MILLISECONDS))
                  (is (= :drain-timed-out
                         (invalid-reason #(biff/stop started))))
                  (is (empty? @events)
                      "Biff must not close the provider while successor runs")
                  (is (= :retired-lifecycle
                         (lifecycle-reason
                          #(lifecycle/restart-recorded!
                            old {:worker-id "late-restart"}))))
                  (is (= :retired-lifecycle
                         (lifecycle-reason
                          #(lifecycle/restart-with-redelivery!
                            child {:worker-id "late-descendant"}))))
                  (.countDown release)
                  (is (poll-until #(pos? (:cycles (lifecycle/status child)))))
                  (biff/stop started)
                  (is (= [:providers-closed] @events))
                  (is (= :stopped (:status (lifecycle/status child))))
                  (is (= :failed (:status (lifecycle/status old))))
                  (is (live/closed? live)))
                (finally
                  (.countDown release))))))
        (finally (live/close! live))))))
