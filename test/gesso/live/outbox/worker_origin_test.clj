(ns gesso.live.outbox.worker-origin-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.live.outbox.cycle :as cycle]
            [gesso.live.outbox.lifecycle :as lifecycle]
            [gesso.live.outbox.worker :as worker])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- empty-page []
  {:results [] :attempted 0 :halt-reason :observed-end :next-after nil})

(deftest scheduled-cycle-origin-is-thread-and-lifecycle-local
  (let [node (Object.)
        system {:source :fixture}
        source-id "fixture/source"
        worker-id "fixture-worker"
        opts {:worker-id worker-id :poll-ms 10}
        observations (atom [])
        latch (atom (CountDownLatch. 1))
        read-origin #(lifecycle/current-worker-origin node source-id worker-id)]
    (with-redefs [worker/acquire-sweep!
                  (fn [n _]
                    (swap! observations conj
                           {:node-matches? (identical? node n)
                            :origin (read-origin)
                            :wrong-node (lifecycle/current-worker-origin (Object.) source-id worker-id)
                            :wrong-source (lifecycle/current-worker-origin node "other" worker-id)
                            :wrong-worker (lifecycle/current-worker-origin node source-id "other")
                            :other-thread @(future (read-origin))})
                    (.countDown ^CountDownLatch @latch)
                    (empty-page))]
      ;; A directly invoked cycle on the same node is not a scheduled cycle.
      (cycle/process-cycle! node system {} source-id {:worker-id worker-id})
      (is (= [nil] (mapv :origin @observations)))
      (reset! observations [])
      (reset! latch (CountDownLatch. 2))
      (let [handle (lifecycle/start! node system {} source-id opts)]
        (try
          (is (.await ^CountDownLatch @latch 3000 TimeUnit/MILLISECONDS))
          (is (nil? (read-origin)))
          (finally (is (= :drained (:outcome (lifecycle/stop! handle 3000)))))))
      (let [samples @observations
            origins (mapv :origin samples)
            first-origin (first origins)]
        (is (<= 2 (count origins)))
        (is (every? :node-matches? samples))
        (is (every? #(every? nil? (map % [:wrong-node :wrong-source :wrong-worker :other-thread])) samples))
        (is (uuid? (:lifecycle-ref first-origin)))
        (is (= (vec (range 1 (inc (count origins)))) (mapv :cycle-number origins)))
        (is (every? #(= (dissoc first-origin :cycle-number) (dissoc % :cycle-number)) origins))
        (is (= {:source-id source-id :worker-id worker-id
                :origin :scheduled-local-cycle :authentication :not-established}
               (dissoc first-origin :lifecycle-ref :cycle-number)))
        (reset! observations [])
        (reset! latch (CountDownLatch. 1))
        (let [handle (lifecycle/start! node system {} source-id opts)]
          (try
            (is (.await ^CountDownLatch @latch 3000 TimeUnit/MILLISECONDS))
            (finally (is (= :drained (:outcome (lifecycle/stop! handle 3000)))))))
        (is (not= (:lifecycle-ref first-origin) (get-in @observations [0 :origin :lifecycle-ref])))))))

(deftest substituted-cycle-does-not-confer-origin
  (let [node (Object.)
        source-id "fixture/source"
        worker-id "fixture-worker"
        seen (promise)]
    (with-redefs [cycle/process-cycle! (fn [& _]
                                       (deliver seen (lifecycle/current-worker-origin node source-id worker-id))
                                       (throw (ex-info "substituted cycle" {})))]
      (let [handle (lifecycle/start! node {:source :fixture} {} source-id {:worker-id worker-id})]
        (try
          (is (nil? (deref seen 3000 ::timeout)))
          (finally (is (= :failed (:outcome (lifecycle/stop! handle 3000))))))))))

(deftest exceptional-exit-clears-origin-before-failure-reporting
  (let [node (Object.)
        source-id "fixture/source"
        worker-id "fixture-worker"
        read-origin #(lifecycle/current-worker-origin node source-id worker-id)
        seen (promise)
        cleanup (promise)
        failure-report #'lifecycle/failure-report
        original @failure-report]
    (with-redefs-fn {#'worker/acquire-sweep! (fn [& _]
                                            (deliver seen (read-origin))
                                            (throw (ex-info "acquisition failed" {})))
                      failure-report (fn [e] (deliver cleanup (read-origin)) (original e))}
      (fn []
        (let [handle (lifecycle/start! node {:source :fixture} {} source-id {:worker-id worker-id})]
          (try
            (is (= :scheduled-local-cycle (:origin (deref seen 3000 ::timeout))))
            (is (nil? (deref cleanup 3000 ::timeout)))
            (finally (is (= :failed (:outcome (lifecycle/stop! handle 3000)))))))))))
