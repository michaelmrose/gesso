(ns gesso.live.outbox.history-scheduler-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.live.outbox.history-scheduler :as scheduler]))

(defn- await-state [status pred]
  (let [deadline (+ (System/nanoTime) 5000000000)]
    (loop []
      (let [s (status)]
        (cond (pred s) s
              (< (System/nanoTime) deadline) (do (Thread/sleep 5) (recur))
              :else s)))))

(deftest schedule-options-are-explicit-and-bounded
  (let [opts {:catch-up-ms 10 :idle-ms 50 :failure-ms 100 :max-failure-ms 200}]
    (is (scheduler/valid-options? opts))
    (doseq [bad [nil {} (assoc opts :extra 1) (assoc opts :catch-up-ms 0)
                 (assoc opts :idle-ms 3600001) (assoc opts :catch-up-ms 51)
                 (assoc opts :failure-ms 201) (assoc opts :failure-ms 1.5)]]
      (is (not (scheduler/valid-options? bad))))))

(deftest scheduler-paces-retries-and-recovers
  (let [attempts (atom 0)
        s (scheduler/start! {:catch-up-ms 100 :idle-ms 500 :failure-ms 200 :max-failure-ms 300}
                            (fn []
                              (case (swap! attempts inc)
                                1 (throw (ex-info "failure" {}))
                                2 (throw (ex-info "failure" {}))
                                3 (throw (ex-info "failure" {}))
                                4 (throw (ex-info "busy" {:reason :history-controller-busy}))
                                5 {:caught-up? false}
                                {:caught-up? true}))
                            (constantly true) {:caught-up? false})
        status (:status s)
        waiting (fn [outcome count]
                  (await-state status #(and (= :waiting (:phase %))
                                            (= outcome (:last-outcome %))
                                            (= count (:consecutive-failures %)))))]
    (try
      (is (= 200 (:next-delay-ms (waiting :failed 1))))
      (is (= 300 (:next-delay-ms (waiting :failed 2))))
      (is (= 300 (:next-delay-ms (waiting :failed 3))))
      (is (= 500 (:next-delay-ms (waiting :busy 3))))
      (is (= 100 (:next-delay-ms (waiting :success 0))))
      (is (= 500 (:next-delay-ms (await-state status #(and (= :waiting (:phase %))
                                                           (= :success (:last-outcome %))
                                                           (= 500 (:next-delay-ms %)))))))
      (finally (is (true? ((:stop! s) 5000000000)))))
    (is (= :stopped (:phase (status))))
    (is (nil? (:next-delay-ms (status))))))

(deftest scheduler-shutdown-drains-without-interrupting
  (let [entered (promise) proceed (promise) finished (promise) calls (atom 0)
        s (scheduler/start! {:catch-up-ms 1 :idle-ms 1 :failure-ms 1 :max-failure-ms 1}
                            (fn [] (swap! calls inc) (deliver entered true)
                              @proceed (deliver finished (not (.isInterrupted (Thread/currentThread))))
                              {:caught-up? false})
                            (constantly true) {:caught-up? false})]
    (try
      (is (= true (deref entered 5000 :timeout)))
      (is (false? ((:stop! s) 0)))
      (finally (deliver proceed true)))
    (is (= true (deref finished 5000 :timeout)))
    (is (true? ((:stop! s) 5000000000)))
    (is (= 1 @calls))
    (is (= :stopped (:phase ((:status s)))))))
