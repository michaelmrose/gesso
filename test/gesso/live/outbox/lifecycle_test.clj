(ns gesso.live.outbox.lifecycle-test
  "v2098: opt-in single-thread lifecycle, graceful drain, failure halt and
   physical XTDB-to-Live emission from an automatically scheduled cycle."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.live.core :as live]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.cycle :as cycle]
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
(defn- summary [halt after]
  {:status :cycle-completed :processed 0 :dispatch-accepted 0
   :halt-reason halt :next-after after :results []})
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

(deftest stop-drains-blocked-cycle-without-interruption-or-new-cycles-test
  (let [entered (CountDownLatch. 1)
        release (CountDownLatch. 1)
        calls (atom 0)
        interrupted (atom false)
        sys {:source :fake}]
    (with-redefs [cycle/process-cycle!
                  (fn [& _]
                    (swap! calls inc)
                    (.countDown entered)
                    (try
                      (.await release)
                      (catch InterruptedException _
                        (reset! interrupted true)))
                    (summary :observed-end nil))]
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
                        (summary :page-budget cursor)
                        (summary :observed-end nil))))]
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
  (with-redefs [cycle/process-cycle! (fn [& _] (summary :observed-end nil))]
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

(deftest mismatched-persisted-source-fails-and-retains-lease-for-reconciliation-test
  (with-open [node (xtn/start-node {})]
    (let [sys (system) id (intent-id 803)]
      (try
        (seed! node id)
        (let [handle (lifecycle/start! node sys {} "another/source"
                                       {:worker-id "wrong-source" :poll-ms 20})]
          (try
            (is (eventually #(= :failed (:status (lifecycle/status handle)))))
            (let [report (lifecycle/status handle)]
              (is (= :delivery (get-in report [:failure :phase])))
              (is (true? (get-in report [:failure :rescan-required?])))
              (is (= id (get-in report [:failure :in-flight :intent-id])))
              (is (= :leased (:gesso.live.outbox/status (row node id))))
              (is (zero? (:total-dispatch-accepted report))))
            (finally
              (is (= :failed (:outcome (lifecycle/stop! handle 10000)))))))
        (finally (live/close! sys))))))
