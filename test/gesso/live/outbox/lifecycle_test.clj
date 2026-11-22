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
                          :status :dispatch-accepted :lease-id (random-uuid)
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
