(ns gesso.live.outbox.http-aleph-live-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.live.outbox.http-aleph-live :as owned]
            [gesso.live.outbox.http-netty-terminal :as terminal]
            [gesso.live.outbox.http-listener :as listener]
            [manifold.stream :as stream])
  (:import [io.netty.channel.embedded EmbeddedChannel]
           [java.util.concurrent Executor Executors TimeUnit]))

(defn- eventually [pred]
  (loop [n 0]
    (cond (pred) true (>= n 200) false
          :else (do (Thread/sleep 10) (recur (inc n))))))

(defn- session [body close! closed?]
  {:stream body :response {:status 200 :headers {} :body body}
   :cancel! close! :source-closed? closed?})

(defn- failure-reason [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))

(deftest actual-channel-close-cancels-source-backed-session-test
  (let [ch (EmbeddedChannel.)
        executor (Executors/newSingleThreadExecutor)
        body (stream/stream 1)
        source-closed (atom false)
        invocations (atom 0)
        owner (listener/prepare!
               {:dispatch-async!
                (fn [_ finished!]
                  (let [bridge (terminal/prepare! finished!)]
                    (owned/attach!
                     {:channel ch :bridge bridge :executor executor
                      :session (session body
                                        (fn []
                                          (swap! invocations inc)
                                          (stream/close! body)
                                          (reset! source-closed true))
                                        (fn [] @source-closed))})))
                :on-rejected! (constantly :refused)
                :stop-accept! (constantly {:outcome :stopped})})]
    (try
      (let [result ((:handle! owner) :exchange)]
        (is (zero? @invocations) "Registration cannot cancel an active client")
        (is (= 1 ((:active-count owner))))
        (is (= {:outcome :timed-out} ((:stop! owner) 0)))
        (.close ch)
        (.runPendingTasks ch)
        (is (eventually #(= 1 @invocations)))
        (is (eventually #(zero? ((:active-count owner)))))
        (is (true? @source-closed))
        (is (stream/closed? body))
        (is (= {:outcome :drained} ((:stop! owner) 0)))
        (is (true? (:released? ((:status result))))))
      (finally
        (.close ch)
        (.shutdownNow executor)
        (.finishAndReleaseAll ch)))))

(deftest failed-source-release-is-retained-until-explicit-retry-test
  (let [ch (EmbeddedChannel.)
        executor (Executors/newSingleThreadExecutor)
        body (stream/stream 1)
        closed? (atom false)
        calls (atom 0)
        guard (listener/prepare!
               {:dispatch-async!
                (fn [_ finished!]
                  (let [bridge (terminal/prepare! finished!)]
                    (owned/attach!
                     {:channel ch :bridge bridge :executor executor
                      :session (session body
                                        (fn []
                                          (if (= 1 (swap! calls inc))
                                            (throw (ex-info "physical release failed" {}))
                                            (do (stream/close! body)
                                                (reset! closed? true))))
                                        #(deref closed?))})))
                :on-rejected! (constantly :no)
                :stop-accept! (constantly {:outcome :stopped})})]
    (try
      (let [release ((:handle! guard) :exchange)]
        (.close ch)
        (.runPendingTasks ch)
        (is (eventually #(= 1 @calls)))
        (is (some? (:last-error ((:status release)))))
        (is (= 1 ((:active-count guard))))
        (is (= {:outcome :timed-out} ((:stop! guard) 0)))
        (is (= :released ((:retry! release))))
        (is (= 2 @calls))
        (is (nil? (:last-error ((:status release)))))
        (is (zero? ((:active-count guard))))
        (is (= {:outcome :drained} ((:stop! guard) 0)))
        (is (= :released ((:retry! release))))
        (is (= 2 @calls) "Released session must not cancel twice"))
      (finally
        (.close ch) (.shutdownNow executor) (.finishAndReleaseAll ch)))))

(deftest source-release-must-not-be-inferred-from-stream-close-test
  (let [ch (EmbeddedChannel.)
        executor (Executors/newSingleThreadExecutor)
        body (stream/stream 1)
        calls (atom 0)
        source-closed? (atom false)
        bridge (terminal/prepare! #(swap! calls inc))
        session (session body (fn [] (stream/close! body))
                         (fn [] @source-closed?))]
    (try
      (let [release (owned/attach! {:channel ch :bridge bridge
                                    :executor executor :session session})]
        (.close ch)
        (.runPendingTasks ch)
        (is (eventually #(some? (:last-error ((:status release))))))
        (is (= :source-not-released
               (failure-reason #((:retry! release)))))
        (is (stream/closed? body))
        (is (zero? @calls))
        (reset! source-closed? true)
        (is (= :released ((:retry! release))))
        (is (= 1 @calls)))
      (finally
        (.close ch) (.shutdownNow executor) (.finishAndReleaseAll ch)))))

(deftest disconnect-plan-and-pre-disconnect-retry-are-rejected-test
  (let [ch (EmbeddedChannel.)
        executor (Executors/newSingleThreadExecutor)
        body (stream/stream 1)
        bridge (terminal/prepare! (fn [] nil))
        session (session body #(stream/close! body) (constantly true))]
    (try
      (is (= :invalid-disconnect-plan
             (failure-reason #(owned/attach!
                               {:channel ch :session session :bridge {}
                                :executor executor}))))
      (let [release (owned/attach! {:channel ch :session session
                                    :bridge bridge :executor executor})]
        (is (= :channel-still-active
               (failure-reason #((:retry! release)))))
        (is (zero? (:release-attempts ((:status release))))))
      (finally
        (.close ch) (.shutdownNow executor) (.finishAndReleaseAll ch)))))

(deftest executor-rejection-remains-explicit-retry-debt-test
  (let [ch (EmbeddedChannel.)
        body (stream/stream 1)
        calls (atom 0)
        finished (atom 0)
        bridge (terminal/prepare! #(swap! finished inc))
        rejected (reify Executor
                   (execute [_ _]
                     (throw (java.util.concurrent.RejectedExecutionException.
                             "saturated cleanup executor"))))
        release (owned/attach!
                 {:channel ch :bridge bridge :executor rejected
                  :session (session body
                                    (fn [] (swap! calls inc) (stream/close! body))
                                    #(stream/closed? body))})]
    (try
      (.close ch)
      (.runPendingTasks ch)
      (is (eventually #(some? (:last-error ((:status release))))))
      (is (zero? @calls) "Netty event loop must not perform source cleanup")
      (is (zero? @finished))
      (is (= :released ((:retry! release))))
      (is (= 1 @calls))
      (is (= 1 @finished))
      (is (nil? (:last-error ((:status release)))))
      (finally (.finishAndReleaseAll ch)))))

(deftest a-pending-source-cancellation-cannot-release-listener-early-test
  (let [ch (EmbeddedChannel.)
        pool (Executors/newSingleThreadExecutor)
        body (stream/stream 1)
        entered (promise)
        release-cancel (promise)
        completed (atom 0)
        bridge (terminal/prepare! #(swap! completed inc))
        owned (owned/attach!
               {:channel ch :bridge bridge :executor pool
                :session (session body
                                  (fn []
                                    (deliver entered true)
                                    @release-cancel
                                    (stream/close! body))
                                  #(stream/closed? body))})]
    (try
      (.close ch)
      (.runPendingTasks ch)
      (is (= true (deref entered 2000 false)))
      (is (not (:released? ((:status owned)))))
      (is (= 0 @completed))
      (is (false? (stream/closed? body)))
      (deliver release-cancel true)
      (is (eventually #(:released? ((:status owned)))))
      (is (= 1 @completed))
      (finally
        (deliver release-cancel true)
        (.shutdownNow pool)
        (.finishAndReleaseAll ch)))))

(deftest recovered-route-reconciles-without-repeating-cancellation-test
  ;; Source closure can precede return from a route-owned physical callback.
  ;; Reconciliation is evidence-only, including after route close! succeeds.
  (let [ch (EmbeddedChannel.)
        pool (Executors/newSingleThreadExecutor)
        body (stream/stream 1)
        source-closed? (atom false)
        route-released? (atom false)
        cancel-calls (atom 0)
        terminal-count (atom 0)
        bridge (terminal/prepare! #(swap! terminal-count inc))
        session (assoc (session body
                                (fn [] (swap! cancel-calls inc))
                                #(deref source-closed?))
                       :route-released? #(deref route-released?))
        hook (owned/attach! {:channel ch :executor pool
                             :session session :bridge bridge})]
    (try
      (is (= :waiting-channel ((:reconcile! hook))))
      (.close ch)
      (.runPendingTasks ch)
      ;; The close handler may have called cancel once, but not completed it.
      (is (eventually #(pos? @cancel-calls)))
      (is (= :waiting-response ((:reconcile! hook))))
      (stream/close! body)
      (is (= :waiting-source ((:reconcile! hook))))
      (reset! source-closed? true)
      (is (= :waiting-route ((:reconcile! hook))))
      (is (zero? @terminal-count))
      (let [before @cancel-calls]
        (reset! route-released? true)
        (is (= :released ((:reconcile! hook))))
        (is (= :released ((:reconcile! hook))))
        (is (= before @cancel-calls)
            "Evidence-only reconciliation must not call cancel again"))
      (is (= 1 @terminal-count))
      (is (true? (:released? ((:status hook)))))
      (finally
        (.close ch) (.shutdownNow pool) (.finishAndReleaseAll ch)))))

(deftest reconciler-rotates-bounded-passes-and-preserves-failed-evidence-test
  (let [registry (owned/prepare-reconciler! {:max-pending 3 :max-per-step 2})
        ready (atom false)
        attempted (atom [])
        failed (ex-info "unavailable terminal callback" {:test :v2257})
        first-hook {:status (constantly {})
                    :reconcile! (fn [] (swap! attempted conj :first)
                                  (if @ready :released :waiting-route))}
        second-hook {:status (constantly {})
                     :reconcile! (fn [] (swap! attempted conj :second)
                                   (throw failed))}
        third-hook {:status (constantly {})
                    :reconcile! (fn [] (swap! attempted conj :third)
                                  :released)}]
    ((:register! registry) first-hook)
    ((:register! registry) second-hook)
    ((:register! registry) third-hook)
    (is (= :reconciler-capacity
           (failure-reason #((:register! registry) first-hook))))
    (let [r ((:after-step! registry) {:outcome :failed})]
      (is (= 2 (:attempted r)))
      (is (<= 2 (:remaining r) 3))
      (is (<= 0 (:reconciled r) 1)))
    (let [r ((:after-step! registry) {:outcome :attempted})]
      (is (= 2 (:attempted r)))
      (is (<= 0 (:reconciled r) 1))
      (is (<= (:remaining r) 2)))
    (is (pos? (:errors ((:stats registry))))
        "A bad hook must not prevent a later hook from completing")
    (reset! ready true)
    (dotimes [_ 5] ((:after-step! registry) {:outcome :idle}))
    (is (= 1 ((:pending-count registry))))
    (is (pos? (:reconciled ((:stats registry)))))
    (is (identical? failed (:last-error ((:stats registry)))))))

(deftest reconciler-refuses-invalid-plans-and-hooks-test
  (is (= :invalid-reconciler-options
         (failure-reason #(owned/prepare-reconciler! {:extra 1}))))
  (doseq [opts [{:max-pending 0} {:max-pending 4097}
                {:max-per-step 0} {:max-per-step 65}]]
    (is (= :invalid-reconciler-limit
           (failure-reason #(owned/prepare-reconciler! opts)))))
  (let [registry (owned/prepare-reconciler! {:max-pending 1})]
    (is (= :invalid-reconciliation-hook
           (failure-reason #((:register! registry) {}))))
    (is (instance? java.util.UUID
                   ((:register! registry)
                    {:status (constantly {})
                     :reconcile! (constantly :waiting-channel)})))
    (is (= :reconciler-capacity
           (failure-reason
            #((:register! registry) {:status (constantly {})
                                     :reconcile! (constantly :released)}))))
    (is (= {:attempted 1 :reconciled 0 :remaining 1}
           ((:after-step! registry) {:outcome :idle})))))

(deftest reconciler-refuses-untrusted-success-outcome-test
  (let [registry (owned/prepare-reconciler!)
        bad (owned/prepare-reconciler!)]
    ((:register! registry) {:status (constantly {})
                            :reconcile! (constantly :unknown)})
    (is (= {:attempted 1 :reconciled 0 :remaining 1}
           ((:after-step! registry) {:outcome :attempted})))
    (is (= :invalid-reconciliation-result
           (:reason (ex-data (:last-error ((:stats registry)))))))
    (is (= 1 ((:pending-count registry))))))
