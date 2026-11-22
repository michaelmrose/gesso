(ns gesso.live.outbox.lifecycle-test
  "v2098: opt-in single-thread lifecycle, graceful drain, failure halt and
   physical XTDB-to-Live emission from an automatically scheduled cycle."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.live.core :as live]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.cycle :as cycle]
            [gesso.live.outbox.delivery :as delivery]
            [gesso.live.outbox.acknowledgment :as acknowledgment]
            [gesso.live.outbox.lifecycle :as lifecycle]
            [gesso.live.source :as source]
            [manifold.stream :as s]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.util UUID]
           [java.util.concurrent CountDownLatch TimeUnit]))

(def ^:private source-id "gesso/test-lifecycle")
(defn- system []
  (live/create {:rules [{:when-topic :request
                         :expand (fn [_ctx change]
                                   [(select-keys change [:topic :id :change/kind])])}]
                :source-options {:coalesce-window-ms 0}
                :dispatch-options {:threads 1 :queue-size 8 :on-overflow :throw}}))
(defn- intent-id [i]
  (UUID/fromString (format "00000000-0000-0000-0000-%012d" i)))
(defn- seed! [node id]
  (let [plan (outbox/prepare!
              {:intent-ids [id] :source-id source-id
               :operation :request/claim :purpose :gesso.live/refresh}
              [{:topic :request :id (str "request-" id)
                :change/kind :updated}])]
    (xt/execute-tx node (into (vec (:guard-ops plan))
                              [[:put-docs :requests
                                {:xt/id (str "request-" id) :status :claimed}]
                               (:tx-op plan)]))))
(defn- row [node id]
  (first (xt/q node ["SELECT * FROM gesso_live_outbox WHERE _id = ?" id])))
(defn- eventually [pred]
  (loop [n 0]
    (cond (pred) true
          (>= n 250) false
          :else (do (Thread/sleep 20) (recur (inc n))))))
(defn- summary [worker-id halt after]
  (let [results (if (= :page-budget halt)
                  [{:intent-id after :acquisition-status :contended
                    :reason :lease-held}]
                  [])]
    {:status :cycle-completed :worker-id worker-id
     :processed (count results) :dispatch-accepted 0
     :acquired 0 :reclaimed 0 :contended (count results) :unavailable 0
     :halt-reason halt :next-after after :results results}))
(defn- invalid-reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e
                 (when (= :gesso.live.outbox/invalid-lifecycle
                          (:error/type (ex-data e)))
                   (:reason (ex-data e))))))

(deftest invalid-lifecycle-options-fail-before-thread-start-test
  (let [sys {:source :fake}]
    (doseq [[opts reason]
            [[nil :invalid-options]
             [{} :invalid-worker-id]
             [{:worker-id " "} :invalid-worker-id]
             [{:worker-id "w" :poll-ms 0} :invalid-poll-ms]
             [{:worker-id "w" :poll-ms 60001} :invalid-poll-ms]
             [{:worker-id "w" :max-items 0} :invalid-max-items]
             [{:worker-id "w" :max-items 129} :invalid-max-items]
             [{:worker-id "w" :lease-millis 0} :invalid-lease-millis]
             [{:worker-id "w" :unknown true} :unknown-options]]]
      (is (= reason (invalid-reason #(lifecycle/start! :unused sys {} source-id opts)))))
    (is (= :invalid-system (invalid-reason #(lifecycle/start!
                                            :unused {} {} source-id {:worker-id "w"}))))
    (is (= :invalid-context (invalid-reason #(lifecycle/start!
                                             :unused sys nil source-id {:worker-id "w"}))))
    (is (= :invalid-source-id (invalid-reason #(lifecycle/start!
                                               :unused sys {} " " {:worker-id "w"}))))))

(deftest physical-start-schedules-real-xtdb-and-live-cycle-test
  (with-open [node (xtn/start-node {})]
    (let [sys (system)
          id (intent-id 801)
          out (atom nil)]
      (try
        (seed! node id)
        (let [stream (source/subscribe (:source sys)
                                        {:topic :request :id (str "request-" id)})
              handle (lifecycle/start! node sys {} source-id
                                       {:worker-id "host-a" :poll-ms 30 :max-items 4})]
          (try
            (is (eventually #(= :dispatch-accepted
                                (:gesso.live.outbox/status (row node id)))))
            (is (eventually #(>= (:total-dispatch-accepted
                                  (lifecycle/status handle)) 1)))
            (let [event (deref (s/take! stream) 1500 ::timeout)
                  snapshot (lifecycle/status handle)]
              (is (= (str "request-" id) (:id event)))
              (is (some? (:progression event)))
              (is (>= (:cycles snapshot) 1))
              (is (= 1 (:total-dispatch-accepted snapshot)))
              (is (nil? (:failure snapshot))))
            (finally
              (reset! out (lifecycle/stop! handle 10000))
              (s/close! stream)))
          (is (= :drained (:outcome @out)))
          (is (= :stopped (get-in @out [:snapshot :status])))
          (is (false? (get-in @out [:snapshot :in-cycle?]))))
        (finally (live/close! sys))))))

(deftest interrupted-stop-wait-preserves-caller-signal-and-retryable-drain-test
  ;; stop! has already requested cooperative shutdown when its wait begins.
  ;; If that wait is interrupted, it must rethrow the exact interruption while
  ;; retaining JVM cancellation. A later stop! on the same handle must still
  ;; drain the original, non-interrupted physical worker before host teardown.
  (let [entered (CountDownLatch. 1)
        release (CountDownLatch. 1)
        calls (atom 0)
        worker-interrupted? (atom false)]
    (with-redefs [cycle/process-cycle!
                  (fn [_ _ _ _ opts]
                    (swap! calls inc)
                    (.countDown entered)
                    (try
                      (.await release)
                      (catch InterruptedException error
                        (reset! worker-interrupted? true)
                        (throw error)))
                    (summary (:worker-id opts) :observed-end nil))]
      (let [handle (lifecycle/start! :unused {:source :fake} {} source-id
                                     {:worker-id "interrupted-stop" :poll-ms 10})]
        (try
          (is (.await entered 3000 TimeUnit/MILLISECONDS))
          (Thread/interrupted)
          (.interrupt (Thread/currentThread))
          (let [caught (try
                         (lifecycle/stop! handle 3000)
                         (catch Throwable error error))
                interrupted? (.isInterrupted (Thread/currentThread))]
            (Thread/interrupted)
            (is (instance? InterruptedException caught)
                "An interrupted await must not claim a drained worker")
            (is interrupted?
                "An interrupted await must preserve caller cancellation")
            (is (true? (:stop-requested? (lifecycle/status handle))))
            (is (true? (:in-cycle? (lifecycle/status handle)))))
          (.countDown release)
          (let [drained (lifecycle/stop! handle 3000)]
            (is (= :drained (:outcome drained)))
            (is (= :stopped (get-in drained [:snapshot :status])))
            (is (= 1 @calls))
            (is (false? @worker-interrupted?)))
          (finally
            (Thread/interrupted)
            (.countDown release)
            (lifecycle/stop! handle 3000)))))))

(deftest stop-drains-blocked-cycle-without-interruption-or-new-cycles-test
  (let [entered (CountDownLatch. 1)
        release (CountDownLatch. 1)
        calls (atom 0)
        interrupted (atom false)
        sys {:source :fake}]
    (with-redefs [cycle/process-cycle!
                  (fn [_ _ _ _ opts]
                    (swap! calls inc)
                    (.countDown entered)
                    (try
                      (.await release)
                      (catch InterruptedException _
                        (reset! interrupted true)))
                    (summary (:worker-id opts) :observed-end nil))]
      (let [handle (lifecycle/start! :unused sys {} source-id
                                     {:worker-id "blocked" :poll-ms 10})]
        (try
          (is (.await entered 3000 TimeUnit/MILLISECONDS))
          (let [pending (lifecycle/stop! handle 0)]
            (is (= :timed-out (:outcome pending)))
            (is (true? (get-in pending [:snapshot :stop-requested?])))
            (is (true? (get-in pending [:snapshot :in-cycle?]))))
          (is (false? @interrupted))
          (finally (.countDown release)))
        (let [drained (lifecycle/stop! handle 3000)]
          (is (= :drained (:outcome drained)))
          (is (= :stopped (get-in drained [:snapshot :status])))
          (is (= 1 @calls))
          (is (false? @interrupted))
          (is (= :drained (:outcome (lifecycle/stop! handle 0)))))))))

(deftest fatal-cycle-halts-scheduling-and-preserves-reconciliation-report-test
  (let [calls (atom 0)
        lease-id (random-uuid)
        intent-id (random-uuid)
        sys {:source :fake}]
    (with-redefs [cycle/process-cycle!
                  (fn [& _]
                    (swap! calls inc)
                    (throw (ex-info "uncertain source send"
                                    {:error/type :gesso.live.outbox/cycle-interrupted
                                     :phase :delivery
                                     :completed-count 1
                                     :completed [{:intent-id (random-uuid)
                                                  :status :dispatch-accepted}]
                                     :in-flight {:intent-id intent-id
                                                 :lease-id lease-id}
                                     :source-acceptance :reported
                                     :rescan-required? true})))]
      (let [handle (lifecycle/start! :unused sys {} source-id
                                     {:worker-id "fail-once" :poll-ms 10})]
        (try
          (is (eventually #(= :failed (:status (lifecycle/status handle)))))
          (Thread/sleep 70)
          (let [report (lifecycle/status handle)]
            (is (= 1 @calls))
            (is (= 0 (:cycles report)))
            (is (= :gesso.live.outbox/cycle-interrupted
                   (get-in report [:failure :error/type])))
            (is (= :delivery (get-in report [:failure :phase])))
            (is (= 1 (get-in report [:failure :completed-count])))
            (is (= intent-id (get-in report [:failure :in-flight :intent-id])))
            (is (= lease-id (get-in report [:failure :in-flight :lease-id])))
            (is (= :reported (get-in report [:failure :source-acceptance])))
            (is (true? (get-in report [:failure :rescan-required?]))))
          (finally
            (is (= :failed (:outcome (lifecycle/stop! handle 2000))))))))))

(deftest poll-continuation-uses-page-cursor-and-resets-after-end-test
  (let [seen (atom [])
        entered (CountDownLatch. 3)
        cursor (random-uuid)
        sys {:source :fake}]
    (with-redefs [cycle/process-cycle!
                  (fn [_ _ _ _ opts]
                    (let [n (count (swap! seen conj opts))]
                      (.countDown entered)
                      (if (= 1 n)
                        (summary (:worker-id opts) :page-budget cursor)
                        (summary (:worker-id opts) :observed-end nil))))]
      (let [handle (lifecycle/start! :unused sys {} source-id
                                     {:worker-id "cursor-test" :poll-ms 10})]
        (try
          (is (.await entered 3000 TimeUnit/MILLISECONDS))
          (let [[a b c] (take 3 @seen)]
            (is (not (contains? a :after)))
            (is (= cursor (:after b)))
            (is (not (contains? c :after))))
          (finally
            (is (= :drained (:outcome (lifecycle/stop! handle 3000))))))))))

(deftest invalid-cycle-result-fails-closed-instead-of-spinning-test
  (let [calls (atom 0)]
    (with-redefs [cycle/process-cycle!
                  (fn [& _]
                    (swap! calls inc)
                    {:status :cycle-completed :processed 1
                     :dispatch-accepted 1 :halt-reason :page-budget
                     :next-after nil})]
      (let [handle (lifecycle/start! :unused {:source :fake} {} source-id
                                     {:worker-id "malformed" :poll-ms 10})]
        (try
          (is (eventually #(= :failed (:status (lifecycle/status handle)))))
          (is (= 1 @calls))
          (is (= :gesso.live.outbox/invalid-lifecycle
                 (get-in (lifecycle/status handle) [:failure :error/type])))
          (finally (lifecycle/stop! handle 2000)))))))

(deftest stop-input-and-handle-validated-test
  (is (= :invalid-handle (invalid-reason #(lifecycle/status nil))))
  (is (= :invalid-handle (invalid-reason #(lifecycle/stop! nil))))
  (with-redefs [cycle/process-cycle!
                (fn [_ _ _ _ opts]
                  (summary (:worker-id opts) :observed-end nil))]
    (let [h (lifecycle/start! :unused {:source :fake} {} source-id
                              {:worker-id "stop-options" :poll-ms 1000})]
      (try
        (is (= :invalid-stop-timeout
               (invalid-reason #(lifecycle/stop! h -1))))
        (is (= :invalid-stop-timeout
               (invalid-reason #(lifecycle/stop! h 120001))))
        (finally (lifecycle/stop! h 3000))))))


(deftest physical-worker-finds-new-work-after-empty-cycle-test
  (with-open [node (xtn/start-node {})]
    (let [sys (system) id (intent-id 802)
          handle (lifecycle/start! node sys {} source-id
                                   {:worker-id "late-arrival" :poll-ms 20})]
      (try
        (is (eventually #(>= (:cycles (lifecycle/status handle)) 1)))
        (seed! node id)
        (is (eventually #(= :dispatch-accepted
                            (:gesso.live.outbox/status (row node id)))))
        (is (eventually #(= 1 (:total-dispatch-accepted
                                 (lifecycle/status handle)))))
        (finally
          (is (= :drained (:outcome (lifecycle/stop! handle 10000))))
          (live/close! sys))))))

(deftest source-scoped-lifecycle-does-not-lease-foreign-work-test
  (with-open [node (xtn/start-node {})]
    (let [sys (system) id (intent-id 803)]
      (try
        (seed! node id)
        (let [handle (lifecycle/start! node sys {} "another/source"
                                       {:worker-id "wrong-source" :poll-ms 20})]
          (try
            (is (eventually #(>= (:cycles (lifecycle/status handle)) 1)))
            (let [report (lifecycle/status handle)]
              (is (nil? (:failure report)))
              (is (= :pending (:gesso.live.outbox/status (row node id))))
              (is (zero? (:total-processed report)))
              (is (zero? (:total-dispatch-accepted report))))
            (finally
              (is (= :drained (:outcome (lifecycle/stop! handle 10000)))))))
        (finally (live/close! sys))))))

(deftest inconsistent-cycle-completions-cannot-poison-scheduler-test
  ;; A claimed :cycle-completed result is not authority to update durable
  ;; progress counters or advance the scheduler's keyset cursor. Fail closed
  ;; on inconsistent summaries, without scheduling another cycle.
  (let [id (intent-id 991)
        other (intent-id 992)
        entry {:intent-id id :acquisition-status :contended
               :reason :lease-held}
        valid {:status :cycle-completed :worker-id "checked-worker"
               :processed 1 :acquired 0 :reclaimed 0 :contended 1
               :unavailable 0 :dispatch-accepted 0
               :results [entry] :halt-reason :page-budget :next-after id}
        cases [[:fabricated-count (assoc valid :processed 2)]
               [:invented-dispatch (assoc valid :dispatch-accepted 1)]
               [:invented-acquisition (assoc valid :acquired 1)]
               [:omitted-count (dissoc valid :contended)]
               [:wrong-worker (assoc valid :worker-id "another-worker")]
               [:wrong-cursor (assoc valid :next-after other)]
               [:missing-results (dissoc valid :results)]
               [:wrong-entry-status (assoc-in valid [:results 0 :status]
                                              :dispatch-accepted)]
               [:missing-intent (assoc-in valid [:results 0 :intent-id] nil)]
               [:duplicate-entry (-> valid
                                     (assoc :results [entry entry]
                                            :processed 2 :contended 2))]]]
    (doseq [[label bogus] cases]
      (let [entered (CountDownLatch. 1)
            calls (atom 0)]
        (with-redefs [cycle/process-cycle!
                      (fn [& _]
                        (swap! calls inc)
                        (.countDown entered)
                        bogus)]
          (let [handle (lifecycle/start! :unused {:source :fake} {}
                                         source-id
                                         {:worker-id "checked-worker"
                                          :poll-ms 3000 :max-items 2})]
            (try
              (is (.await entered 1500 TimeUnit/MILLISECONDS)
                  (str label " must invoke cycle once"))
              (is (eventually
                   #(#{:failed :idle :stopped} (:status (lifecycle/status handle))))
                  (str label " cycle must reach a terminal observation"))
              (let [snapshot (lifecycle/status handle)]
                (is (= :failed (:status snapshot))
                    (str label " must fail closed, never report a successful cycle"))
                (is (= :gesso.live.outbox/invalid-lifecycle
                       (get-in snapshot [:failure :error/type]))
                    (str label " must preserve the typed contract violation"))
                (is (zero? (:cycles snapshot))
                    (str label " must not increment cycle progress"))
                (is (zero? (:total-dispatch-accepted snapshot))
                    (str label " must not fabricate accepted delivery")))
              (is (= 1 @calls)
                  (str label " must never schedule a second cycle"))
              (finally
                (lifecycle/stop! handle 3000)))))))))

(deftest valid-cycle-completion-accounts-for-the-actual-results-test
  ;; Valid real-style outcomes remain schedulable, including mixed accepted
  ;; delivery and contention. The scheduler must account only for outcomes
  ;; described by the actual results vector.
  (let [id (intent-id 993)
        finished (CountDownLatch. 1)
        calls (atom 0)
        valid {:status :cycle-completed :worker-id "checked-worker"
               :processed 1 :acquired 1 :reclaimed 0 :contended 0
               :unavailable 0 :dispatch-accepted 1
               :results [{:intent-id id :acquisition-status :acquired
                          :status :dispatch-accepted :worker-id "checked-worker"
                          :lease-id (random-uuid)
                          :receipt-id (random-uuid)
                          :progression {:basis :valid}}]
               :halt-reason :observed-end :next-after nil}]
    (with-redefs [cycle/process-cycle!
                  (fn [& _]
                    (swap! calls inc)
                    (.countDown finished)
                    valid)]
      (let [handle (lifecycle/start! :unused {:source :fake} {} source-id
                                     {:worker-id "checked-worker"
                                      :poll-ms 3000 :max-items 2})]
        (try
          (is (.await finished 1500 TimeUnit/MILLISECONDS))
          (is (eventually #(= 1 (:total-dispatch-accepted
                                 (lifecycle/status handle)))))
          (let [snapshot (lifecycle/status handle)]
            (is (= 1 (:cycles snapshot)))
            (is (= 1 (:total-processed snapshot)))
            (is (= 1 (:total-dispatch-accepted snapshot)))
            (is (= valid (:last-cycle snapshot)))
            (is (= :idle (:status snapshot))))
          (is (= 1 @calls))
          (finally
            (is (= :drained (:outcome (lifecycle/stop! handle 3000))))))))))

(deftest failure-inspection-refuses-healthy-and-non-cycle-lifecycles-test
  (is (= :invalid-handle
         (invalid-reason #(lifecycle/inspect-failure! nil))))
  (with-redefs [cycle/process-cycle!
                (fn [_ _ _ _ opts]
                  (summary (:worker-id opts) :observed-end nil))]
    (let [handle (lifecycle/start! :unused {:source :fake} {} source-id
                                   {:worker-id "healthy-inspection"
                                    :poll-ms 3000})]
      (try
        (is (eventually #(= :idle (:status (lifecycle/status handle)))))
        (is (= :not-failed-lifecycle
               (invalid-reason #(lifecycle/inspect-failure! handle))))
        (finally (lifecycle/stop! handle 3000)))))
  (with-redefs [cycle/process-cycle!
                (fn [& _]
                  (throw (ex-info "unrelated failure"
                                  {:error/type :gesso.live.outbox/invalid-cycle})))]
    (let [handle (lifecycle/start! :unused {:source :fake} {} source-id
                                   {:worker-id "generic-failure"
                                    :poll-ms 3000})]
      (try
        (is (eventually #(= :failed (:status (lifecycle/status handle)))))
        (is (= :failed (:outcome (lifecycle/stop! handle 3000))))
        (is (= :not-cycle-interruption
               (invalid-reason #(lifecycle/inspect-failure! handle))))
        (finally (lifecycle/stop! handle 3000))))))

(deftest failed-managed-cycle-inspects-its-bound-physical-acknowledgment-test
  ;; A real asynchronous managed lifecycle physically leases and acknowledges
  ;; one publication. The adapter loses its post-ack result. The lifecycle
  ;; halts, preserving that *unverified* receipt; inspection uses the exact
  ;; XTDB node/source owned by the failed lifecycle, not host-supplied IDs.
  (with-open [node (xtn/start-node {})]
    (let [id (intent-id 844)
          receipt-id (random-uuid)
          calls (atom 0)
          _ (seed! node id)]
      (with-redefs [delivery/dispatch-acquired!
                    (fn [_ _ _ acquired _]
                      (swap! calls inc)
                      (let [ack (acknowledgment/acknowledge-dispatch!
                                 node {:intent-id (:intent-id acquired)
                                       :lease-id (:lease-id acquired)
                                       :worker-id (:worker-id acquired)
                                       :receipt {:receipt-id receipt-id
                                                 :adapter :gesso.live/synchronous-source
                                                 :result :accepted}})]
                        (is (= :acknowledged (:status ack))))
                      (throw (ex-info "ack reply unknown after physical commit"
                                      {:error/type
                                       :gesso.live.outbox/post-dispatch-ack-unknown
                                       :intent-id (:intent-id acquired)
                                       :lease-id (:lease-id acquired)
                                       :receipt-id receipt-id
                                       :dispatch/status :accepted
                                       :acknowledgment/status :unknown})))]
        (let [handle (lifecycle/start! node {:source :installed} {} source-id
                                       {:worker-id "managed-recovery"
                                        :poll-ms 10 :max-items 1})]
          (try
            (is (eventually #(= :failed (:status (lifecycle/status handle)))))
            (is (= :failed (:outcome (lifecycle/stop! handle 3000))))
            (let [before (lifecycle/status handle)
                  before-row (row node id)
                  result (lifecycle/inspect-failure! handle)]
              (is (= :gesso.live.outbox/cycle-interrupted
                     (get-in before [:failure :error/type])))
              (is (= :unverified-post-dispatch-receipt
                     (get-in before [:failure :in-flight :receipt-evidence])))
              (is (= receipt-id
                     (get-in before [:failure :in-flight :receipt-id])))
              (is (= :matching-acknowledgment-recorded (:status result)))
              (is (= id (:intent-id result)))
              (is (= :dispatch-accepted (:observed-status result)))
              (is (true? (:rescan-required? result)))
              (is (not (contains? result :may-replay?)))
              (is (= before (lifecycle/status handle)))
              (is (= before-row (row node id)))
              (is (= 0 (:total-dispatch-accepted before)))
              (is (= 1 @calls))
              (is (= result (lifecycle/inspect-failure! handle))))
            (finally (lifecycle/stop! handle 3000))))))))

(deftest recorded-ack-restart-requires-complete-physical-qualification-test
  (with-open [node (xtn/start-node {})]
    (let [id (intent-id 860)
          _ (seed! node id)
          entered (CountDownLatch. 1)
          receipt (random-uuid)
          prior (with-redefs
                  [delivery/dispatch-acquired!
                   (fn [_ _ _ acquired _]
                     (is (= :acknowledged
                            (:status (acknowledgment/acknowledge-dispatch!
                                      node {:intent-id (:intent-id acquired)
                                            :lease-id (:lease-id acquired)
                                            :worker-id (:worker-id acquired)
                                            :receipt {:receipt-id receipt
                                                      :adapter :gesso.live/synchronous-source
                                                      :result :accepted}}))))
                     (.countDown entered)
                     (throw (ex-info "lost acknowledgment response"
                                     {:error/type
                                      :gesso.live.outbox/post-dispatch-ack-unknown
                                      :intent-id (:intent-id acquired)
                                      :lease-id (:lease-id acquired)
                                      :receipt-id receipt
                                      :dispatch/status :accepted
                                      :acknowledgment/status :unknown})))]
                  (let [h (lifecycle/start! node {:source :installed} {} source-id
                                            {:worker-id "old-worker" :poll-ms 20})]
                    (is (.await entered 5 TimeUnit/SECONDS))
                    (is (eventually #(= :failed (:status (lifecycle/status h)))))
                    (is (= :failed (:outcome (lifecycle/stop! h 3000))))
                    h))
          before (lifecycle/status prior)
          before-row (row node id)]
      (is (= :reused-worker-id
             (invalid-reason #(lifecycle/restart-recorded!
                               prior {:worker-id "old-worker"}))))
      (is (= :invalid-worker-id
             (invalid-reason #(lifecycle/restart-recorded!
                               prior {:worker-id " "}))))
      (let [{:keys [handle reconciliation]}
            (lifecycle/restart-recorded! prior
                                         {:worker-id "new-worker"
                                          :poll-ms 20 :max-items 1})]
        (try
          (is (= :matching-acknowledgment-recorded (:status reconciliation)))
          (is (= [] (:completed-observations reconciliation)))
          (is (eventually #(pos? (:cycles (lifecycle/status handle)))))
          (is (= "new-worker" (:worker-id (lifecycle/status handle))))
          (is (= 0 (:total-dispatch-accepted (lifecycle/status handle))))
          (is (= :already-restarted
                 (invalid-reason #(lifecycle/restart-recorded!
                                   prior {:worker-id "third-worker"}))))
          (is (= before (lifecycle/status prior)))
          (is (= before-row (row node id)))
          (finally (is (= :drained (:outcome (lifecycle/stop! handle 3000))))))))))

(deftest recorded-restart-refuses-pending-uncertain-physical-work-test
  (with-open [node (xtn/start-node {})]
    (let [id (intent-id 861)
          _ (seed! node id)
          old (with-redefs [delivery/dispatch-acquired!
                            (fn [_ _ _ _ _]
                              (throw (ex-info "send unknown"
                                              {:error/type :unknown/send})))]
                (let [handle (lifecycle/start! node {:source :installed} {} source-id
                                               {:worker-id "uncertain-worker"
                                                :poll-ms 20})]
                  (is (eventually #(= :failed (:status (lifecycle/status handle)))))
                  (is (= :failed (:outcome (lifecycle/stop! handle 3000))))
                  handle))]
      (is (= :same-lease-active (:status (lifecycle/inspect-failure! old))))
      (is (= :recovery-not-qualified
             (invalid-reason #(lifecycle/restart-recorded!
                               old {:worker-id "new-worker"}))))
      (is (= :failed (:status (lifecycle/status old)))))))

(deftest restart-refuses-generic-and-nonfailed-lifecycles-test
  (with-redefs [cycle/process-cycle!
                (fn [_ _ _ _ opts]
                  (summary (:worker-id opts) :observed-end nil))]
    (let [h (lifecycle/start! :unused {:source :fake} {} source-id
                              {:worker-id "healthy-worker" :poll-ms 3000})]
      (try
        (is (eventually #(pos? (:cycles (lifecycle/status h)))))
        (is (= :not-failed-lifecycle
               (invalid-reason #(lifecycle/restart-recorded!
                                 h {:worker-id "next-worker"}))))
        (finally (lifecycle/stop! h 3000)))))
  (with-redefs [cycle/process-cycle!
                (fn [& _]
                  (throw (ex-info "generic failure" {:error/type :generic/failure})))]
    (let [h (lifecycle/start! :unused {:source :fake} {} source-id
                              {:worker-id "failed-generic" :poll-ms 3000})]
      (try
        (is (eventually #(= :failed (:status (lifecycle/status h)))))
        (is (= :failed (:outcome (lifecycle/stop! h 3000))))
        (is (= :not-cycle-interruption
               (invalid-reason #(lifecycle/restart-recorded!
                                 h {:worker-id "next-worker"}))))
        (finally (lifecycle/stop! h 3000))))))


(deftest restarted-worker-actually-emits-new-live-intents-test
  ;; The failed worker physically acknowledged its old intent but lost the
  ;; response. Explicit recovery must preserve its real Live source and
  ;; acquire/deliver *new* work after a full rescan, not merely create a thread.
  (with-open [node (xtn/start-node {})]
    (let [sys (system)
          first-id (intent-id 870)
          next-id (intent-id 871)
          receipt-id (random-uuid)]
      (try
        (seed! node first-id)
        (let [old (with-redefs
                    [delivery/dispatch-acquired!
                     (fn [_ _ _ acquired _]
                       (is (= :acknowledged
                              (:status (acknowledgment/acknowledge-dispatch!
                                        node {:intent-id (:intent-id acquired)
                                              :lease-id (:lease-id acquired)
                                              :worker-id (:worker-id acquired)
                                              :receipt {:receipt-id receipt-id
                                                        :adapter :gesso.live/synchronous-source
                                                        :result :accepted}}))))
                       (throw (ex-info "reply lost" {:error/type
                                                      :gesso.live.outbox/post-dispatch-ack-unknown
                                                      :intent-id (:intent-id acquired)
                                                      :lease-id (:lease-id acquired)
                                                      :receipt-id receipt-id
                                                      :dispatch/status :accepted
                                                      :acknowledgment/status :unknown})))]
                    (let [h (lifecycle/start! node sys {} source-id
                                              {:worker-id "failed-first" :poll-ms 20})]
                      (is (eventually #(= :failed (:status (lifecycle/status h)))))
                      (is (= :failed (:outcome (lifecycle/stop! h 3000))))
                      h))
              stream (source/subscribe (:source sys)
                                       {:topic :request
                                        :id (str "request-" next-id)})
              {:keys [handle]} (lifecycle/restart-recorded!
                                old {:worker-id "live-successor"
                                     :poll-ms 20 :max-items 2})]
          (try
            (seed! node next-id)
            (is (eventually #(= :dispatch-accepted
                                (:gesso.live.outbox/status (row node next-id)))))
            (is (= (str "request-" next-id)
                   (:id (deref (s/take! stream) 3000 ::timeout))))
            (is (= :dispatch-accepted
                   (:gesso.live.outbox/status (row node first-id))))
            (is (eventually #(pos? (:total-dispatch-accepted
                                   (lifecycle/status handle)))))
            (is (= 1 (:total-dispatch-accepted (lifecycle/status handle))))
            (is (= 0 (:total-dispatch-accepted (lifecycle/status old))))
            (finally
              (is (= :drained (:outcome (lifecycle/stop! handle 5000))))
              (s/close! stream))))
        (finally (live/close! sys))))))

(deftest restart-rejects-divergent-completed-prefix-and-remains-retryable-test
  (with-open [node (xtn/start-node {})]
    (let [first-id (intent-id 862)
          second-id (intent-id 863)
          _ (doseq [id [first-id second-id]] (seed! node id))
          attempts (atom 0)
          receipt-1 (random-uuid)
          receipt-2 (random-uuid)
          old (with-redefs
                [delivery/dispatch-acquired!
                 (fn [_ _ _ acquired _]
                   (let [n (swap! attempts inc)
                         receipt (if (= n 1) receipt-1 receipt-2)
                         ack (acknowledgment/acknowledge-dispatch!
                              node {:intent-id (:intent-id acquired)
                                    :lease-id (:lease-id acquired)
                                    :worker-id (:worker-id acquired)
                                    :receipt {:receipt-id receipt
                                              :adapter :gesso.live/synchronous-source
                                              :result :accepted}})]
                     (is (= :acknowledged (:status ack)))
                     (if (= n 1)
                       {:status :dispatch-accepted
                        :intent-id (:intent-id acquired)
                        :lease-id (:lease-id acquired)
                        :receipt-id receipt
                        :progression (:progression acquired)
                        :acknowledgment {:status :acknowledged
                                         :intent-id (:intent-id acquired)
                                         :lease-id (:lease-id acquired)
                                         :receipt-id receipt}
                        :source-result {:status :emitted-many :count 1
                                        :results [{:status :emitted}]}}
                       (throw
                        (ex-info "second ack committed, reply uncertain"
                                 {:error/type
                                  :gesso.live.outbox/post-dispatch-ack-unknown
                                  :intent-id (:intent-id acquired)
                                  :lease-id (:lease-id acquired)
                                  :receipt-id receipt
                                  :dispatch/status :accepted
                                  :acknowledgment/status :unknown})))))]
                (let [h (lifecycle/start! node {:source :installed} {} source-id
                                          {:worker-id "old-prefix" :poll-ms 20
                                           :max-items 2})]
                  (is (eventually #(= :failed (:status (lifecycle/status h)))))
                  (is (= :failed (:outcome (lifecycle/stop! h 3000))))
                  h))
          first-ack (row node first-id)]
      (is (= 2 @attempts))
      (is (= 1 (get-in (lifecycle/status old) [:failure :completed-count])))
      (is (= first-id
             (get-in (lifecycle/status old) [:failure :completed 0 :intent-id])))
      ;; Contradict the old completed prefix without modifying the uncertain
      ;; second acknowledgment: checking only the in-flight row would restart.
      (xt/execute-tx node [[:put-docs outbox/intent-table
                            (assoc first-ack :gesso.live.outbox/dispatch-receipt-id
                                   (random-uuid))]])
      (is (= :recovery-not-qualified
             (invalid-reason #(lifecycle/restart-recorded!
                               old {:worker-id "new-prefix" :poll-ms 20}))))
      ;; Even explicit duplicate-tolerant recovery cannot bypass a
      ;; contradictory *completed* prefix.
      (is (= :recovery-not-qualified
             (invalid-reason #(lifecycle/restart-with-redelivery!
                               old {:worker-id "new-redelivery" :poll-ms 20}))))
      (is (= :different-acknowledgment-recorded
             (get-in (lifecycle/inspect-failure! old)
                     [:completed-observations 0 :status])))
      ;; The rejected read did not reserve a restart: physical correction can
      ;; be independently requalified by the original source-owned provider.
      (xt/execute-tx node [[:put-docs outbox/intent-table first-ack]])
      (let [{:keys [handle reconciliation]}
            (lifecycle/restart-recorded! old
                                         {:worker-id "new-prefix" :poll-ms 20})]
        (try
          (is (= :matching-acknowledgment-recorded (:status reconciliation)))
          (is (= :matching-acknowledgment-recorded
                 (get-in reconciliation [:completed-observations 0 :status])))
          (is (eventually #(pos? (:cycles (lifecycle/status handle)))))
          (is (= 0 (:total-dispatch-accepted (lifecycle/status handle))))
          (is (= 2 @attempts))
          (finally
            (is (= :drained (:outcome (lifecycle/stop! handle 3000))))))))))

(deftest duplicate-invalidation-recovery-after-lost-ack-test
  ;; A real Live source accepted the FIRST invalidation, but its physical XTDB
  ;; acknowledgment threw. The failed worker must stop; deliberate opt-in
  ;; recovery waits for provider-time lease expiry and sends an ADVISORY
  ;; duplicate before acknowledging the intent through its new guarded lease.
  (with-open [node (xtn/start-node {})]
    (let [sys (system)
          id (intent-id 880)
          sub (source/subscribe (:source sys)
                                 {:topic :request :id (str "request-" id)})]
      (try
        (seed! node id)
        (let [old (with-redefs
                    [acknowledgment/acknowledge-dispatch!
                     (fn [_ _]
                       (throw (ex-info "lost acknowledgment reply"
                                       {:error/type :test/ack-unknown})))]
                    (let [h (lifecycle/start! node sys {} source-id
                                              {:worker-id "failed-emitter"
                                               :poll-ms 30
                                               :lease-millis 2500})]
                      (is (eventually #(= :failed (:status (lifecycle/status h)))))
                      (is (= :failed (:outcome (lifecycle/stop! h 5000))))
                      h))
              first-event (deref (s/take! sub) 3000 ::timeout)
              prior (lifecycle/status old)]
          (is (= (str "request-" id) (:id first-event)))
          (is (= :leased (:gesso.live.outbox/status (row node id))))
          (is (= :gesso.live.outbox/cycle-interrupted
                 (get-in prior [:failure :error/type])))
          (is (= :delivery (get-in prior [:failure :phase])))
          (is (= :same-lease-active
                 (:status (lifecycle/inspect-failure! old))))
          (is (= :recovery-not-qualified
                 (invalid-reason #(lifecycle/restart-recorded!
                                   old {:worker-id "not-recorded"}))))
          (let [{:keys [handle reconciliation recovery-mode]}
                (lifecycle/restart-with-redelivery!
                 old {:worker-id "explicit-re-emitter" :poll-ms 30
                      :lease-millis 2500 :max-items 2})]
            (try
              (is (= :at-least-once-invalidation recovery-mode))
              (is (= :same-lease-active (:status reconciliation)))
              (is (= [] (:completed-observations reconciliation)))
              (is (= :already-restarted
                     (invalid-reason #(lifecycle/restart-recorded!
                                       old {:worker-id "third-worker"}))))
              (is (= :already-restarted
                     (invalid-reason #(lifecycle/restart-with-redelivery!
                                       old {:worker-id "fourth-worker"}))))
              (let [second-event (deref (s/take! sub) 8000 ::timeout)]
                (is (= (str "request-" id) (:id second-event)))
                (is (some? (:progression second-event))))
              (is (eventually #(= :dispatch-accepted
                                  (:gesso.live.outbox/status (row node id)))))
              (is (eventually #(= 1 (:total-dispatch-accepted
                                     (lifecycle/status handle)))))
              (is (= 0 (:total-dispatch-accepted prior)))
              (is (= :failed (:status (lifecycle/status old))))
              (finally
                (is (= :drained (:outcome (lifecycle/stop! handle 5000))))))))
        (finally
          (s/close! sub)
          (live/close! sys))))))

(deftest redelivery-restart-refuses-physically-recorded-ack-test
  ;; A completed physical acknowledgment must use restart-recorded!, never
  ;; the duplicate-send path. This check is independent of receipt reporting.
  (with-open [node (xtn/start-node {})]
    (let [id (intent-id 881)
          _ (seed! node id)
          receipt (random-uuid)
          old (with-redefs
                [delivery/dispatch-acquired!
                 (fn [_ _ _ acquired _]
                   (is (= :acknowledged
                          (:status (acknowledgment/acknowledge-dispatch!
                                    node {:intent-id (:intent-id acquired)
                                          :lease-id (:lease-id acquired)
                                          :worker-id (:worker-id acquired)
                                          :receipt {:receipt-id receipt
                                                    :adapter :gesso.live/synchronous-source
                                                    :result :accepted}}))))
                   (throw (ex-info "reply lost" {:error/type
                                                 :gesso.live.outbox/post-dispatch-ack-unknown
                                                 :intent-id (:intent-id acquired)
                                                 :lease-id (:lease-id acquired)
                                                 :receipt-id receipt
                                                 :dispatch/status :accepted
                                                 :acknowledgment/status :unknown})))]
                (let [h (lifecycle/start! node {:source :installed} {} source-id
                                          {:worker-id "failed-ack" :poll-ms 20})]
                  (is (eventually #(= :failed (:status (lifecycle/status h)))))
                  (is (= :failed (:outcome (lifecycle/stop! h 5000))))
                  h))]
      (is (= :matching-acknowledgment-recorded
             (:status (lifecycle/inspect-failure! old))))
      (is (= :recovery-not-qualified
             (invalid-reason #(lifecycle/restart-with-redelivery!
                               old {:worker-id "not-a-replay"}))))
      (is (= :dispatch-accepted (:gesso.live.outbox/status (row node id)))))))

(deftest foreign-or-missing-completed-owner-cannot-advance-lifecycle-test
  ;; A normal (non-failed) cycle completion is another trust boundary:
  ;; reported acknowledgments cannot advance the scheduler's counters or
  ;; cursor if their owner differs from its installed worker. Unlike a
  ;; failure report, no later reconciliation is attempted before accounting.
  (let [id (intent-id 994)
        good {:status :cycle-completed :worker-id "checked-worker"
              :processed 1 :acquired 1 :reclaimed 0 :contended 0
              :unavailable 0 :dispatch-accepted 1
              :results [{:intent-id id :acquisition-status :acquired
                         :status :dispatch-accepted
                         :worker-id "checked-worker"
                         :lease-id (random-uuid) :receipt-id (random-uuid)
                         :progression {:basis :mock-ack}}]
              :halt-reason :page-budget :next-after id}
        cases [[:foreign-worker (assoc-in good [:results 0 :worker-id]
                                          "foreign-worker")]
               [:missing-worker (update good :results
                                        #(mapv (fn [r] (dissoc r :worker-id)) %))]
               [:blank-worker (assoc-in good [:results 0 :worker-id] " ")]
               [:missing-lease (update good :results
                                       #(mapv (fn [r] (dissoc r :lease-id)) %))]]]
    (doseq [[label report] cases]
      (let [entered (CountDownLatch. 1)
            calls (atom 0)]
        (with-redefs [cycle/process-cycle!
                      (fn [& _]
                        (swap! calls inc)
                        (.countDown entered)
                        report)]
          (let [handle (lifecycle/start! :unused {:source :fake} {} source-id
                                         {:worker-id "checked-worker"
                                          :poll-ms 3000 :max-items 2})]
            (try
              (is (.await entered 1500 TimeUnit/MILLISECONDS) (str label))
              (is (eventually #(= :failed (:status (lifecycle/status handle))))
                  (str label " must halt accounting"))
              (let [state (lifecycle/status handle)]
                (is (= :gesso.live.outbox/invalid-lifecycle
                       (get-in state [:failure :error/type])) (str label))
                (is (zero? (:cycles state)) (str label))
                (is (zero? (:total-processed state)) (str label))
                (is (zero? (:total-dispatch-accepted state)) (str label))
                (is (nil? (:last-cycle state)) (str label)))
              (is (= 1 @calls) (str label " cannot run another cycle"))
              (finally (lifecycle/stop! handle 3000)))))))))
