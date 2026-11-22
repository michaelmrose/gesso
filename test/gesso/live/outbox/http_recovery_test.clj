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
    (is (= {:last-attempt-ms 12000 :attempt-count 3 :failure-count 0}
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
    (is (= {:last-attempt-ms 200 :attempt-count 2 :failure-count 1}
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
