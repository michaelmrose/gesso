(ns gesso.live.outbox.biff-recovery-test
  "Biff provider teardown must own explicitly restarted worker descendants."
  (:require [clojure.test :refer [deftest is]]
            [com.biffweb.core :as biff]
            [gesso.live.core :as live]
            [gesso.live.source :as source]
            [manifold.stream :as stream]
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

(deftest managed-recovery-entrypoint-is-bound-to-the-original-host-test
  ;; The actual Biff system owns the provider/worker relationship. A forged
  ;; system map cannot replace its XTDB, Live system, source or rules context
  ;; and then induce a restart. The original Biff stop owns the new worker.
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          receipt-id (random-uuid)
          plan (outbox/prepare!
                {:intent-ids [id] :source-id source-id
                 :operation :request/claim :purpose :gesso.live/refresh}
                [{:topic :request :id "managed-biff-recovery"}])
          _ (xt/execute-tx node (conj (vec (:guard-ops plan)) (:tx-op plan)))
          live (live-system)
          forged-live (live-system)
          closed (atom false)
          provider {:biff.core/id :fixture/managed-provider
                    :biff.core/start
                    (fn [system]
                      (assoc system :xtdb/connectable node
                             :gesso.live/system live))
                    :biff.core/stop
                    (fn [_]
                      (reset! closed true)
                      (live/close! live))}]
      (try
        (with-redefs [delivery/dispatch-acquired!
                      (fn [_ _ _ acquired _]
                        (is (= :acknowledged
                               (:status
                                (acknowledgment/acknowledge-dispatch!
                                 node {:intent-id (:intent-id acquired)
                                       :lease-id (:lease-id acquired)
                                       :worker-id (:worker-id acquired)
                                       :receipt {:receipt-id receipt-id
                                                 :adapter :gesso.live/synchronous-source
                                                 :result :accepted}}))))
                        (throw (ex-info "ack reply lost"
                                        {:error/type
                                         :gesso.live.outbox/post-dispatch-ack-unknown
                                         :intent-id (:intent-id acquired)
                                         :lease-id (:lease-id acquired)
                                         :receipt-id receipt-id
                                         :dispatch/status :accepted
                                         :acknowledgment/status :unknown})))]
          (binding [*modules* [provider managed/module]]
            (let [started (biff/start
                           (config {}) #'*modules*
                           [:fixture/managed-provider managed/module-id])
                  original (get started managed/handle-key)]
              (try
                (is (poll-until #(= :failed (:status (lifecycle/status original)))))
                (is (= :invalid-recovery-mode
                       (invalid-reason #(managed/recover-module!
                                         started :unspecified
                                         {:worker-id "bad-mode"}))))
                (is (= :missing-managed-handle
                       (invalid-reason #(managed/recover-module!
                                         (dissoc started managed/handle-key)
                                         :physically-recorded
                                         {:worker-id "missing-handle"}))))
                (is (= :recovery-provider-mismatch
                       (invalid-reason #(managed/recover-module!
                                         (assoc started :xtdb/connectable :foreign)
                                         :physically-recorded
                                         {:worker-id "wrong-node"}))))
                (is (= :recovery-provider-mismatch
                       (invalid-reason #(managed/recover-module!
                                         (assoc started :gesso.live/system
                                                forged-live)
                                         :physically-recorded
                                         {:worker-id "wrong-live"}))))
                (is (= :recovery-source-mismatch
                       (invalid-reason #(managed/recover-module!
                                         (assoc started managed/source-id-key
                                                "foreign/source")
                                         :physically-recorded
                                         {:worker-id "wrong-source"}))))
                (is (= :recovery-context-mismatch
                       (invalid-reason #(managed/recover-module!
                                         (assoc started managed/context-key
                                                {:unexpected :context})
                                         :physically-recorded
                                         {:worker-id "wrong-context"}))))
                (let [{:keys [handle recovery-mode reconciliation]}
                      (managed/recover-module!
                       started :physically-recorded
                       {:worker-id "qualified-biff-successor" :poll-ms 20})]
                  (is (= :physically-recorded recovery-mode))
                  (is (= :matching-acknowledgment-recorded
                         (:status reconciliation)))
                  (is (identical? original (get started managed/handle-key)))
                  (is (poll-until #(pos? (:cycles (lifecycle/status handle)))))
                  (is (zero? (:total-dispatch-accepted
                              (lifecycle/status handle))))
                  (is (false? @closed))
                  (is (= :already-restarted
                         (lifecycle-reason
                          #(managed/recover-module!
                            started :at-least-once-invalidation
                            {:worker-id "not-a-second-successor"}))))
                  (biff/stop started)
                  (is @closed)
                  (is (= :stopped (:status (lifecycle/status handle)))))
                (finally
                  (when-not @closed
                    (biff/stop started)))))))
        (finally
          (live/close! live)
          (live/close! forged-live))))))

(deftest managed-biff-redelivery-after-unknown-ack-remains-at-least-once-test
  ;; A real Biff-managed worker emits the physical advisory invalidation but
  ;; fails to acknowledge. Its explicit successor must wait for the guarded
  ;; lease reclaim, then send the same notification again without claiming
  ;; that either copy reached a browser.
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          target-id "biff-redelivery"
          plan (outbox/prepare!
                {:intent-ids [id] :source-id source-id
                 :operation :request/claim :purpose :gesso.live/refresh}
                [{:topic :request :id target-id}])
          _ (xt/execute-tx node (conj (vec (:guard-ops plan)) (:tx-op plan)))
          live (live-system)
          subscriber (source/subscribe (:source live)
                                       {:topic :request :id target-id})
          provider {:biff.core/id :fixture/replay-provider
                    :biff.core/start
                    (fn [system]
                      (assoc system :xtdb/connectable node
                             :gesso.live/system live))
                    :biff.core/stop (fn [_] (live/close! live))}]
      (try
        (binding [*modules* [provider managed/module]]
          (let [started
                (with-redefs [acknowledgment/acknowledge-dispatch!
                              (fn [_ _]
                                (throw (ex-info "acknowledgment outcome unknown"
                                                {:error/type :test/unknown-ack})))]
                  (let [started
                        (biff/start
                         (config {managed/options-key
                                  {:worker-id "biff-worker"
                                   :poll-ms 20 :lease-millis 1500}})
                         #'*modules* [:fixture/replay-provider managed/module-id])
                        old (get started managed/handle-key)]
                    (is (poll-until #(= :failed (:status (lifecycle/status old)))))
                    started))
                old (get started managed/handle-key)]
            (try
              (is (= target-id (:id (deref (stream/take! subscriber)
                                            4000 ::timeout))))
              (is (= :leased (:gesso.live.outbox/status
                              (first (xt/q node ["SELECT * FROM gesso_live_outbox WHERE _id = ?" id])))))
              (let [{:keys [handle recovery-mode reconciliation]}
                    (managed/recover-module!
                     started :at-least-once-invalidation
                     {:worker-id "managed-redelivery-successor"
                      :poll-ms 20 :lease-millis 1500})]
                (is (= :at-least-once-invalidation recovery-mode))
                (is (= :same-lease-active (:status reconciliation)))
                (is (= target-id (:id (deref (stream/take! subscriber)
                                               8000 ::timeout))))
                (is (poll-until #(= 1 (:total-dispatch-accepted
                                       (lifecycle/status handle)))))
                (is (= 0 (:total-dispatch-accepted
                          (lifecycle/status old))))
                (is (= :dispatch-accepted
                       (:gesso.live.outbox/status
                        (first (xt/q node ["SELECT * FROM gesso_live_outbox WHERE _id = ?" id])))))
                (biff/stop started)
                (is (= :stopped (:status (lifecycle/status handle)))))
              (finally (biff/stop started)))))
        (finally
          (stream/close! subscriber)
          (live/close! live))))))
