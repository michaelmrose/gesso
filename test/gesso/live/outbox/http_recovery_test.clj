(ns gesso.live.outbox.http-recovery-test
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.live.outbox.http-recovery :as recovery]))

(defn- fake-route [pending calls closed? failure?]
  {:closed? (fn [] @closed?)
   :disconnected-count (fn [] @pending)
   :retry-disconnected!
   (fn [max-attempts]
     (swap! calls conj max-attempts)
     (when @failure?
       (throw @failure?))
     (let [n (min @pending max-attempts)]
       (swap! pending - n)
       {:attempted n :recovered n}))})

(deftest host-driven-retry-is-idle-paced-and-bounded-test
  (let [now (atom 10000)
        pending (atom 0)
        calls (atom [])
        closed? (atom false)
        failure? (atom nil)
        route (fake-route pending calls closed? failure?)
        driver (recovery/prepare!
                route {:min-interval-ms 1000 :max-attempts 2
                       :now-ms #(deref now)})
        step! (:step! driver)]
    (is (= {:outcome :idle :pending 0} (step!)))
    (is (empty? @calls) "Empty route must never call physical cleanup")
    (reset! pending 5)
    (is (= {:outcome :attempted :attempted 2 :recovered 2 :remaining 3}
           (step!)))
    (is (= [2] @calls))
    (is (= {:outcome :deferred :pending 3 :retry-after-ms 1000}
           (step!)))
    (swap! now + 999)
    (is (= {:outcome :deferred :pending 3 :retry-after-ms 1}
           (step!)))
    (swap! now inc)
    (is (= {:outcome :attempted :attempted 2 :recovered 2 :remaining 1}
           (step!)))
    (swap! now + 1000)
    (is (= {:outcome :attempted :attempted 1 :recovered 1 :remaining 0}
           (step!)))
    (is (= {:outcome :idle :pending 0} (step!)))
    (is (= [2 2 2] @calls))
    (is (= {:last-attempt-ms 12000 :attempt-count 3 :failure-count 0
            :consecutive-failures 0}
           ((:stats driver))))
    (reset! closed? true)
    (reset! pending 4)
    (is (= {:outcome :route-closed} (step!)))
    (is (= [2 2 2] @calls)
        "A closed route is handled by ordered host shutdown, not live retry")))

(deftest failed-physical-retry-is-not-replayed-before-due-test
  (let [now (atom 100)
        pending (atom 2)
        calls (atom [])
        closed? (atom false)
        error (ex-info "source still unreleased" {:reason :physical})
        fail (atom error)
        controller (recovery/prepare!
                    (fake-route pending calls closed? fail)
                    {:min-interval-ms 100 :max-attempts 1
                     :now-ms #(deref now)})
        step! (:step! controller)]
    (is (identical? error (try (step!) nil (catch Throwable e e))))
    (is (= 2 @pending) "Failure cannot claim the source was released")
    (is (= {:outcome :deferred :pending 2 :retry-after-ms 100}
           (step!)))
    (is (= [1] @calls))
    (reset! fail nil)
    (swap! now + 100)
    (is (= {:outcome :attempted :attempted 1 :recovered 1 :remaining 1}
           (step!)))
    (is (= {:last-attempt-ms 200 :attempt-count 2 :failure-count 1
            :consecutive-failures 0}
           ((:stats controller))))
    (is (= [1 1] @calls))))

(deftest invalid-recovery-config-and-time-do-not-touch-source-test
  (let [pending (atom 1)
        calls (atom [])
        closed (atom false)
        failure (atom nil)
        route (fake-route pending calls closed failure)
        reason (fn [f] (try (f) nil
                            (catch clojure.lang.ExceptionInfo e
                              (:reason (ex-data e)))))]
    (doseq [[config expected]
            [[{:min-interval-ms 0} :invalid-min-interval-ms]
             [{:min-interval-ms 120001} :invalid-min-interval-ms]
             [{:max-failure-interval-ms 0} :invalid-max-failure-interval-ms]
             [{:max-failure-interval-ms 120001} :invalid-max-failure-interval-ms]
             [{:max-failure-interval-ms nil} :invalid-max-failure-interval-ms]
             [{:min-interval-ms 100 :max-failure-interval-ms 99}
              :invalid-max-failure-interval-ms]
             [{:max-attempts 0} :invalid-max-attempts]
             [{:max-attempts 1025} :invalid-max-attempts]
             [{:now-ms 4} :invalid-clock]
             [{:typo true} :invalid-options]]]
      (is (= expected (reason #(recovery/prepare! route config)))))
    (is (= :invalid-route (reason #(recovery/prepare! {} {}))))
    (let [clock (atom 5)
          driver (recovery/prepare! route {:now-ms #(deref clock)})]
      (is (= :attempted (:outcome ((:step! driver)))))
      (reset! pending 1)
      (reset! clock 4)
      (is (= :nonmonotonic-clock (reason #((:step! driver)))))
      (is (= [16] @calls))
      (reset! clock :bad)
      (is (= :invalid-clock-result (reason #((:step! driver)))))
      (is (= [16] @calls)))))

(deftest concurrent-host-ticks-cannot-double-attempt-test
  (let [now (atom 1000)
        entered (promise)
        release (promise)
        calls (atom 0)
        pending (atom 1)
        route {:closed? (constantly false)
               :disconnected-count (fn [] @pending)
               :retry-disconnected!
               (fn [budget]
                 (is (= 1 budget))
                 (swap! calls inc)
                 (deliver entered true)
                 @release
                 (reset! pending 0)
                 {:attempted 1 :recovered 1})}
        driver (recovery/prepare! route {:min-interval-ms 1000
                                          :max-attempts 1
                                          :now-ms #(deref now)})
        a (future ((:step! driver)))]
    (try
      (is (= true (deref entered 2000 ::timeout)))
      (let [b (future ((:step! driver)))]
        (deliver release true)
        (is (= :attempted (:outcome (deref a 2000 ::timeout))))
        (is (= {:outcome :idle :pending 0} (deref b 2000 ::timeout)))
        (is (= 1 @calls)))
      (finally
        (deliver release true)
        (future-cancel a)))))

(defn- await-condition
  [pred]
  (loop [n 0]
    (cond
      (pred) true
      (>= n 300) false
      :else (do (Thread/sleep 10) (recur (inc n))))))

(deftest scheduled-recovery-starts-is-paced-and-stops-test
  (let [pending (atom 3)
        calls (atom [])
        route {:closed? (constantly false)
               :disconnected-count (fn [] @pending)
               :retry-disconnected!
               (fn [budget]
                 (swap! calls conj (System/nanoTime))
                 (is (= 1 budget))
                 (swap! pending dec)
                 {:attempted 1 :recovered 1})}
        owner (recovery/start-scheduled!
               route {:poll-ms 10 :min-interval-ms 75 :max-attempts 1})]
    (try
      (is (await-condition #(zero? @pending))
          "Explicit scheduler must recover each outstanding unit")
      (is (= 3 (count @calls)))
      (is (every? #(>= % 50000000)
                  (map - (rest @calls) @calls))
          "Subsequent physical retries must be paced in monotonic time")
      (is (= 3 (get-in ((:stats owner)) [:controller :attempt-count])))
      (is (= :running ((:status owner))))
      (is (= {:outcome :drained} ((:stop! owner) 2000)))
      (is (= :drained ((:status owner))))
      (is (true? ((:drained? owner))))
      (let [before (count @calls)]
        (Thread/sleep 70)
        (is (= before (count @calls))))
      (is (= {:outcome :drained} ((:stop! owner) 0)))
      (finally ((:stop! owner) 2000)))))

(deftest scheduled-recovery-errors-remain-visible-and-continue-test
  (let [pending (atom 1)
        attempts (atom 0)
        original (ex-info "source cleanup failed" {:test :v2239})
        route {:closed? (constantly false)
               :disconnected-count (fn [] @pending)
               :retry-disconnected!
               (fn [_]
                 (if (= 1 (swap! attempts inc))
                   (throw original)
                   (do (reset! pending 0)
                       {:attempted 1 :recovered 1})))}
        owner (recovery/start-scheduled!
               route {:poll-ms 10 :min-interval-ms 60 :max-attempts 1})]
    (try
      (is (await-condition #(zero? @pending)))
      (is (= 2 @attempts))
      (is (= 1 (:failure-count ((:stats owner)))))
      (is (identical? original (:last-error ((:stats owner)))))
      (is (= 1 (get-in ((:stats owner)) [:controller :failure-count])))
      (is (= :drained (:outcome ((:stop! owner) 2000))))
      (finally ((:stop! owner) 2000)))))

(deftest host-shutdown-fences-running-physical-cleanup-test
  ;; No provider-stop callback may run while an in-flight recovery tick is
  ;; still inside a physical source-release operation.
  (let [pending (atom 1)
        entered (promise)
        release (promise)
        closed? (atom false)
        calls (atom [])
        route {:closed? (fn [] @closed?)
               :disconnected-count (fn [] @pending)
               :retry-disconnected!
               (fn [_]
                 (deliver entered true)
                 @release
                 (reset! pending 0)
                 {:attempted 1 :recovered 1})
               :close! (fn []
                         (swap! calls conj :route-close)
                         (reset! closed? true)
                         :closed)
               :await-drained! (fn [_]
                                 (swap! calls conj :route-drain)
                                 {:outcome :drained})}
        worker (recovery/start-scheduled!
                route {:poll-ms 10 :min-interval-ms 50})
        shutdown (recovery/prepare-owned-shutdown
                  worker {:route route
                          :stop-listener! (fn []
                                            (swap! calls conj :listener-stop)
                                            {:outcome :drained})
                          :stop-host! (fn []
                                        (swap! calls conj :host-stop)
                                        {:outcome :stopped})
                          :drain-timeout-ms 0})]
    (try
      (is (= true (deref entered 2000 ::timeout)))
      (is (= {:outcome :recovery-timed-out} ((:stop! shutdown))))
      (is (= :stopping ((:status worker))))
      (is (empty? @calls)
          "No listener, route or provider can be stopped before worker drain")
      (is (= :ready (get ((:status shutdown)) :shutdown)))
      (deliver release true)
      (is (await-condition #((:drained? worker))))
      (is (= :stopped (:outcome ((:stop! shutdown)))))
      (is (= [:route-close :listener-stop :route-drain :host-stop] @calls))
      (is (= :stopped (get ((:status shutdown)) :shutdown)))
      (is (= :stopped (:outcome ((:stop! shutdown)))))
      (is (= [:route-close :listener-stop :route-drain :host-stop] @calls)
          "A repeated confirmed stop cannot replay provider teardown")
      (finally
        (deliver release true)
        ((:stop! worker) 2000)))))

(deftest scheduled-recovery-validates-before-starting-test
  (let [route {:closed? (constantly false)
               :disconnected-count (constantly 0)
               :retry-disconnected! (fn [_] {:attempted 0 :recovered 0})}
        reason (fn [f] (try (f) nil
                            (catch clojure.lang.ExceptionInfo e
                              (:reason (ex-data e)))))]
    (doseq [opts [{:poll-ms 0} {:poll-ms 9} {:poll-ms 120001}
                  {:poll-ms nil} {:poll-ms 1.5} {:unknown true}]]
      (is (= (if (contains? opts :poll-ms)
               :invalid-poll-ms :invalid-scheduled-options)
             (reason #(recovery/start-scheduled! route opts)))))
    (let [worker (recovery/start-scheduled!
                  route {:poll-ms 1000})]
      (try
        (is (= :invalid-stop-timeout-ms
               (reason #((:stop! worker) -1))))
        (is (= :running ((:status worker))))
        (finally (is (= :drained (:outcome ((:stop! worker) 2000)))))))))

(deftest repeated-source-release-failures-backoff-and-recover-test
  ;; A persistently failing physical release must not be retried at the
  ;; minimum cadence forever. After the first failure preserve the original
  ;; interval; double it for later failures, cap it, then reset on success.
  (let [now (atom 1000)
        pending (atom 2)
        calls (atom [])
        fails-left (atom 4)
        failure (ex-info "persistent physical source release" {:test :v2241})
        route {:closed? (constantly false)
               :disconnected-count (fn [] @pending)
               :retry-disconnected!
               (fn [max-attempts]
                 (is (= 1 max-attempts))
                 (swap! calls conj @now)
                 (if (pos? @fails-left)
                   (do (swap! fails-left dec)
                       (throw failure))
                   (do (swap! pending dec)
                       {:attempted 1 :recovered 1})))}
        driver (recovery/prepare!
                route {:min-interval-ms 100
                       :max-failure-interval-ms 400
                       :max-attempts 1
                       :now-ms #(deref now)})
        step! (:step! driver)
        blocked! (fn [remaining]
                   (is (= {:outcome :deferred :pending @pending
                           :retry-after-ms remaining}
                          (step!))))
        fail! (fn []
                (is (identical? failure
                                (try (step!) nil (catch Throwable e e)))))]
    (fail!)                         ; 1000, first failure
    (is (= 1 (:consecutive-failures ((:stats driver)))))
    (blocked! 100)                 ; first failure: no extra latency
    (swap! now + 99)
    (blocked! 1)
    (swap! now inc)
    (fail!)                         ; 1100, second failure
    (is (= 2 (:consecutive-failures ((:stats driver)))))
    (blocked! 200)
    (swap! now + 200)
    (fail!)                         ; 1300, third failure
    (blocked! 400)
    (swap! now + 400)
    (fail!)                         ; 1700, fourth failure
    (blocked! 400)                  ; capped, not exponential forever
    (swap! now + 400)
    (is (= {:outcome :attempted :attempted 1 :recovered 1 :remaining 1}
           (step!)))                ; 2100 succeeds
    (is (= 0 (:consecutive-failures ((:stats driver)))))
    (blocked! 100)                  ; success resets to normal cadence
    (swap! now + 100)
    (is (= {:outcome :attempted :attempted 1 :recovered 1 :remaining 0}
           (step!)))
    (is (= {:last-attempt-ms 2200 :attempt-count 6
            :failure-count 4 :consecutive-failures 0}
           ((:stats driver))))
    (is (= [1000 1100 1300 1700 2100 2200] @calls))
    (is (zero? @pending))))

(deftest scheduled-physical-failure-observability-and-backoff-test
  ;; The hosted scheduler must distinguish an active failure streak from an
  ;; earlier historical error; a later successful release clears the streak.
  (let [pending (atom 1)
        calls (atom 0)
        failing? (atom true)
        error (ex-info "unreleased source" {:test :v2241-scheduled})
        route {:closed? (constantly false)
               :disconnected-count (fn [] @pending)
               :retry-disconnected!
               (fn [_]
                 (swap! calls inc)
                 (if @failing?
                   (throw error)
                   (do (reset! pending 0)
                       {:attempted 1 :recovered 1})))}
        owner (recovery/start-scheduled!
               route {:poll-ms 10 :min-interval-ms 80
                      :max-failure-interval-ms 320 :max-attempts 1})]
    (try
      (is (await-condition #(pos? (:failure-count ((:stats owner))))))
      (let [status ((:stats owner))]
        (is (= :failed (get-in status [:last-physical-outcome :outcome])))
        (is (identical? error (:last-error status)))
        (is (pos? (get-in status [:controller :consecutive-failures]))))
      (reset! failing? false)
      (is (await-condition #(zero? @pending)))
      (is (zero? (get-in ((:stats owner))
                         [:controller :consecutive-failures])))
      (is (= :attempted
             (get-in ((:stats owner)) [:last-physical-outcome :outcome])))
      (is (identical? error (:last-error ((:stats owner))))
          "Historical error stays visible without claiming current failure")
      (is (pos? @calls))
      (is (= :drained (:outcome ((:stop! owner) 2000))))
      (finally ((:stop! owner) 2000)))))

(deftest idle-observation-clears-stale-backoff-test
  ;; Another physical owner can release outstanding resources between ticks.
  ;; Once the route reports no cleanup debt, a new unrelated orphan should
  ;; not inherit a long failure streak from an already-resolved obligation.
  (let [now (atom 0)
        pending (atom 1)
        failure? (atom true)
        calls (atom [])
        error (ex-info "prior orphan cleanup failed" {:test :v2241-idle})
        route {:closed? (constantly false)
               :disconnected-count (fn [] @pending)
               :retry-disconnected!
               (fn [max-attempts]
                 (swap! calls conj @now)
                 (if @failure?
                   (throw error)
                   (do (swap! pending dec)
                       {:attempted 1 :recovered 1})))}
        controller (recovery/prepare!
                    route {:min-interval-ms 100
                           :max-failure-interval-ms 400
                           :max-attempts 1 :now-ms #(deref now)})
        step! (:step! controller)]
    (is (identical? error (try (step!) nil (catch Throwable e e))))
    (swap! now + 100)
    (is (identical? error (try (step!) nil (catch Throwable e e))))
    (is (= 2 (:consecutive-failures ((:stats controller)))))
    ;; An independent owner completes the old physical cleanup.
    (reset! pending 0)
    (is (= {:outcome :idle :pending 0} (step!)))
    (is (zero? (:consecutive-failures ((:stats controller)))))
    (reset! pending 1)
    (reset! failure? false)
    (swap! now + 100)
    (is (= {:outcome :attempted :attempted 1 :recovered 1 :remaining 0}
           (step!)))
    (is (= [0 100 200] @calls))
    (is (zero? @pending))))
