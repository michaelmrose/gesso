(ns gesso.live.fragment-test
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.live.fragment :as fragment]
   [missionary.core :as m]))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(def timeout-ms 1000)

(def fragment-base
  :store-queue)

(def fragment-key
  [:fragment :store-queue "store-1"])

(def other-fragment-key
  [:fragment :store-queue "store-2"])

(defn start-task
  [task]
  (let [p (promise)
        cancel (task #(deliver p {:status :success :value %})
                     #(deliver p {:status :failure :error %}))]
    {:promise p
     :cancel cancel}))

(defn cancel-runner!
  [runner]
  ((:cancel runner)))

(defn task-result
  [runner]
  (let [result (deref (:promise runner) timeout-ms ::timeout)]
    (when (= ::timeout result)
      (cancel-runner! runner))
    result))

(defn run-task
  [task]
  (task-result (start-task task)))

(defn eventually
  [pred]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (cond
        (pred)
        true

        (< deadline (System/currentTimeMillis))
        false

        :else
        (do
          (Thread/sleep 10)
          (recur))))))

(defn event-names
  [events]
  (set (map :event events)))

(defn controlled-success-task
  [started release value render-count cancelled]
  (fn [success failure]
    (swap! render-count inc)
    (deliver started :started)
    (let [cancelled? (atom false)
          worker (future
                   (try
                     @release
                     (when-not @cancelled?
                       (success value))
                     (catch Throwable e
                       (when-not @cancelled?
                         (failure e)))))]
      (fn cancel []
        (reset! cancelled? true)
        (deliver cancelled :cancelled)
        (future-cancel worker)))))

(defn controlled-failure-task
  [started release error render-count cancelled]
  (fn [_success failure]
    (swap! render-count inc)
    (deliver started :started)
    (let [cancelled? (atom false)
          worker (future
                   (try
                     @release
                     (when-not @cancelled?
                       (failure error))
                     (catch Throwable e
                       (when-not @cancelled?
                         (failure e)))))]
      (fn cancel []
        (reset! cancelled? true)
        (deliver cancelled :cancelled)
        (future-cancel worker)))))

;; -----------------------------------------------------------------------------
;; Option validation
;; -----------------------------------------------------------------------------

(deftest prepare-options-accepts-defaults-test
  (let [options (fragment/prepare-options! nil)]
    (is (= nil (:ttl-ms options)))
    (is (= true (:cache? options)))
    (is (= true (:singleflight? options)))
    (is (= :blk (:executor options)))
    (is (fn? (:clock options)))))

(deftest prepare-options-accepts-custom-options-test
  (let [clock (fn [] 42)
        debug-fn (fn [_])
        options (fragment/prepare-options!
                 {:ttl-ms 100
                  :cache? false
                  :max-cache-entries 10
                  :singleflight? false
                  :executor :cpu
                  :clock clock
                  :debug-fn debug-fn})]
    (is (= 100 (:ttl-ms options)))
    (is (= false (:cache? options)))
    (is (= 10 (:max-cache-entries options)))
    (is (= false (:singleflight? options)))
    (is (= :cpu (:executor options)))
    (is (identical? clock (:clock options)))
    (is (identical? debug-fn (:debug-fn options)))))

(deftest prepare-options-rejects-invalid-ttl-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":ttl-ms must be a non-negative integer"
       (fragment/prepare-options! {:ttl-ms -1})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":ttl-ms must be a non-negative integer"
       (fragment/prepare-options! {:ttl-ms "soon"}))))

(deftest prepare-options-rejects-invalid-max-cache-entries-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":max-cache-entries must be a positive integer"
       (fragment/prepare-options! {:max-cache-entries 0})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":max-cache-entries must be a positive integer"
       (fragment/prepare-options! {:max-cache-entries -1})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":max-cache-entries must be a positive integer"
       (fragment/prepare-options! {:max-cache-entries "many"}))))

(deftest prepare-options-rejects-invalid-booleans-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":cache\? must be a boolean"
       (fragment/prepare-options! {:cache? :yes})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":singleflight\? must be a boolean"
       (fragment/prepare-options! {:singleflight? :yes}))))

(deftest prepare-options-rejects-invalid-functions-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":clock must be a function"
       (fragment/prepare-options! {:clock :not-a-function})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":debug-fn must be a function"
       (fragment/prepare-options! {:debug-fn :not-a-function}))))

(deftest prepare-options-rejects-invalid-executor-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":executor must be :blk, :cpu, or a java.util.concurrent.Executor"
       (fragment/prepare-options! {:executor :magic}))))

;; -----------------------------------------------------------------------------
;; Key helpers
;; -----------------------------------------------------------------------------

(deftest fragment-key-builds-stable-key-test
  (is (= [:gesso.live.fragment :store-queue]
         (fragment/fragment-key :store-queue))))

(deftest fragment-key-includes-known-dimensions-in-stable-order-test
  (is (= [:gesso.live.fragment :store-queue
          :scope [:store "store-1"]
          :user-key [:user "u-1"]
          :variant :compact
          :params {:page 1}
          :locale :en
          :theme :dark
          :consistency-token "tx-1"]
         (fragment/fragment-key
          :store-queue
          {:theme :dark
           :params {:page 1}
           :scope [:store "store-1"]
           :unknown :ignored
           :consistency-token "tx-1"
           :variant :compact
           :user-key [:user "u-1"]
           :locale :en}))))

(deftest strict-fragment-key-requires-fragment-scope-and-user-key-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"missing required dimension"
       (fragment/strict-fragment-key {:scope [:store "store-1"]
                                      :user-key [:user "u-1"]})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"missing required dimension"
       (fragment/strict-fragment-key {:fragment :store-queue
                                      :user-key [:user "u-1"]})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"missing required dimension"
       (fragment/strict-fragment-key {:fragment :store-queue
                                      :scope [:store "store-1"]}))))

(deftest strict-fragment-key-rejects-nil-required-dimensions-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"dimension must not be nil"
       (fragment/strict-fragment-key {:fragment :store-queue
                                      :scope nil
                                      :user-key [:user "u-1"]}))))

(deftest strict-fragment-key-builds-key-test
  (is (= [:gesso.live.fragment :store-queue
          :scope [:store "store-1"]
          :user-key [:user "u-1"]
          :params {:page 1}
          :consistency-token "tx-1"]
         (fragment/strict-fragment-key
          {:fragment :store-queue
           :scope [:store "store-1"]
           :user-key [:user "u-1"]
           :params {:page 1}
           :consistency-token "tx-1"}))))

;; -----------------------------------------------------------------------------
;; Manager stats and validation
;; -----------------------------------------------------------------------------

(deftest create-builds-empty-manager-test
  (let [manager (fragment/create)]
    (is (= {:cache-count 0
            :inflight-count 0}
           (fragment/stats manager)))))

(deftest protect-task-rejects-nil-key-test
  (let [manager (fragment/create)]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"key must not be nil"
         (fragment/protect-task manager nil (m/sleep 1 :ok))))))

(deftest protect-task-rejects-non-task-test
  (let [manager (fragment/create)]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"task must be a Missionary task function"
         (fragment/protect-task manager fragment-key :not-a-task)))))

(deftest render-task-rejects-non-function-test
  (let [manager (fragment/create)]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"render-fn must be a function"
         (fragment/render-task manager fragment-key :not-a-function)))))

;; -----------------------------------------------------------------------------
;; Render task basics
;; -----------------------------------------------------------------------------

(deftest render-task-renders-value-test
  (let [manager (fragment/create)
        result (run-task
                (fragment/render-task manager fragment-key
                                      (fn [] "<div>ok</div>")))]
    (is (= {:status :success
            :value "<div>ok</div>"}
           result))))

(deftest render-task-propagates-render-errors-test
  (let [manager (fragment/create)
        result (run-task
                (fragment/render-task manager fragment-key
                                      (fn []
                                        (throw
                                         (RuntimeException. "render boom")))))]
    (is (= :failure (:status result)))
    (is (= "render boom"
           (ex-message (:error result))))))

(deftest call-task-supports-blk-and-cpu-test
  (is (= {:status :success
          :value :blk}
         (run-task (fragment/call-task :blk (fn [] :blk)))))
  (is (= {:status :success
          :value :cpu}
         (run-task (fragment/call-task :cpu (fn [] :cpu))))))

;; -----------------------------------------------------------------------------
;; TTL cache
;; -----------------------------------------------------------------------------

(deftest render-task-caches-within-ttl-test
  (let [clock (atom 0)
        render-count (atom 0)
        manager (fragment/create {:ttl-ms 1000
                                  :clock #(deref clock)})
        render-fn (fn []
                    (swap! render-count inc)
                    :html)]
    (is (= {:status :success :value :html}
           (run-task (fragment/render-task manager fragment-key render-fn))))
    (is (= {:status :success :value :html}
           (run-task (fragment/render-task manager fragment-key render-fn))))
    (is (= 1 @render-count))
    (is (= {:cache-count 1
            :inflight-count 0}
           (fragment/stats manager)))))

(deftest render-task-rerenders-after-ttl-expiration-test
  (let [clock (atom 0)
        render-count (atom 0)
        manager (fragment/create {:ttl-ms 10
                                  :clock #(deref clock)})
        render-fn (fn []
                    (swap! render-count inc)
                    :html)]
    (is (= :html (:value (run-task
                          (fragment/render-task manager fragment-key render-fn)))))
    (swap! clock + 11)
    (is (= :html (:value (run-task
                          (fragment/render-task manager fragment-key render-fn)))))
    (is (= 2 @render-count))))

(deftest cache-can-be-disabled-test
  (let [render-count (atom 0)
        manager (fragment/create {:ttl-ms 1000
                                  :cache? false})
        render-fn (fn []
                    (swap! render-count inc)
                    :html)]
    (is (= :html (:value (run-task
                          (fragment/render-task manager fragment-key render-fn)))))
    (is (= :html (:value (run-task
                          (fragment/render-task manager fragment-key render-fn)))))
    (is (= 2 @render-count))
    (is (= 0 (:cache-count (fragment/stats manager))))))

(deftest ttl-zero-disables-cache-test
  (let [render-count (atom 0)
        manager (fragment/create {:ttl-ms 0})
        render-fn (fn []
                    (swap! render-count inc)
                    :html)]
    (is (= :html (:value (run-task
                          (fragment/render-task manager fragment-key render-fn)))))
    (is (= :html (:value (run-task
                          (fragment/render-task manager fragment-key render-fn)))))
    (is (= 2 @render-count))
    (is (= 0 (:cache-count (fragment/stats manager))))))

(deftest clear-cache-removes-all-cached-values-test
  (let [render-count (atom 0)
        manager (fragment/create {:ttl-ms 1000})
        render-fn (fn []
                    (swap! render-count inc)
                    :html)]
    (is (= :html (:value (run-task
                          (fragment/render-task manager fragment-key render-fn)))))
    (is (= 1 (:cache-count (fragment/stats manager))))
    (fragment/clear-cache! manager)
    (is (= 0 (:cache-count (fragment/stats manager))))
    (is (= :html (:value (run-task
                          (fragment/render-task manager fragment-key render-fn)))))
    (is (= 2 @render-count))))

(deftest clear-key-removes-one-cached-value-test
  (let [render-count (atom 0)
        manager (fragment/create {:ttl-ms 1000})
        render-fn (fn []
                    (swap! render-count inc)
                    :html)]
    (is (= :html (:value (run-task
                          (fragment/render-task manager fragment-key render-fn)))))
    (is (= :html (:value (run-task
                          (fragment/render-task manager other-fragment-key render-fn)))))
    (is (= 2 (:cache-count (fragment/stats manager))))

    (fragment/clear-key! manager fragment-key)

    (is (= 1 (:cache-count (fragment/stats manager))))
    (is (= :html (:value (run-task
                          (fragment/render-task manager fragment-key render-fn)))))
    (is (= 3 @render-count))))

(deftest purge-expired-removes-expired-values-test
  (let [clock (atom 0)
        manager (fragment/create {:ttl-ms 10
                                  :clock #(deref clock)})]
    (is (= :html (:value (run-task
                          (fragment/render-task manager fragment-key
                                                (fn [] :html))))))
    (is (= 1 (:cache-count (fragment/stats manager))))
    (swap! clock + 11)
    (is (= 1 (fragment/purge-expired! manager)))
    (is (= 0 (:cache-count (fragment/stats manager))))))

(deftest max-cache-entries-evicts-oldest-values-test
  (let [clock (atom 0)
        render-count (atom 0)
        manager (fragment/create {:ttl-ms 1000
                                  :max-cache-entries 2
                                  :clock #(deref clock)})
        render-fn (fn []
                    (swap! render-count inc)
                    :html)
        k1 [:fragment 1]
        k2 [:fragment 2]
        k3 [:fragment 3]]
    (is (= :html (:value (run-task
                          (fragment/render-task manager k1 render-fn)))))
    (swap! clock inc)
    (is (= :html (:value (run-task
                          (fragment/render-task manager k2 render-fn)))))
    (swap! clock inc)
    (is (= :html (:value (run-task
                          (fragment/render-task manager k3 render-fn)))))
    (is (= 2 (:cache-count (fragment/stats manager))))
    (is (= 3 @render-count))

    ;; k1 was oldest and should have been evicted.
    (is (= :html (:value (run-task
                          (fragment/render-task manager k1 render-fn)))))
    (is (= 4 @render-count))))

;; -----------------------------------------------------------------------------
;; Singleflight
;; -----------------------------------------------------------------------------

(deftest singleflight-shares-one-successful-render-test
  (let [manager (fragment/create)
        started (promise)
        release (promise)
        cancelled (promise)
        render-count (atom 0)
        task (controlled-success-task started release :html render-count cancelled)
        runner-1 (start-task (fragment/protect-task manager fragment-key task))
        _ (is (= :started (deref started timeout-ms ::timeout)))
        runner-2 (start-task (fragment/protect-task manager fragment-key task))]
    (is (= 1 @render-count))
    (is (= 1 (:inflight-count (fragment/stats manager))))

    (deliver release :go)

    (is (= {:status :success :value :html}
           (task-result runner-1)))
    (is (= {:status :success :value :html}
           (task-result runner-2)))
    (is (= 1 @render-count))
    (is (eventually #(= 0 (:inflight-count (fragment/stats manager)))))))

(deftest singleflight-disabled-runs-concurrent-renders-test
  (let [manager (fragment/create {:singleflight? false})
        release (promise)
        render-count (atom 0)
        task (controlled-success-task (promise) release :html render-count (promise))
        runner-1 (start-task (fragment/protect-task manager fragment-key task))
        runner-2 (start-task (fragment/protect-task manager fragment-key task))]
    (is (eventually #(= 2 @render-count)))
    (deliver release :go)
    (is (= {:status :success :value :html}
           (task-result runner-1)))
    (is (= {:status :success :value :html}
           (task-result runner-2)))
    (is (= 0 (:inflight-count (fragment/stats manager))))))

(deftest singleflight-shares-failure-test
  (let [manager (fragment/create)
        started (promise)
        release (promise)
        cancelled (promise)
        render-count (atom 0)
        error (RuntimeException. "render failed")
        task (controlled-failure-task started release error render-count cancelled)
        runner-1 (start-task (fragment/protect-task manager fragment-key task))
        _ (is (= :started (deref started timeout-ms ::timeout)))
        runner-2 (start-task (fragment/protect-task manager fragment-key task))]
    (is (= 1 @render-count))
    (deliver release :go)

    (let [result-1 (task-result runner-1)
          result-2 (task-result runner-2)]
      (is (= :failure (:status result-1)))
      (is (= :failure (:status result-2)))
      (is (= "render failed" (ex-message (:error result-1))))
      (is (= "render failed" (ex-message (:error result-2)))))

    (is (= 1 @render-count))
    (is (eventually #(= 0 (:inflight-count (fragment/stats manager)))))))

(deftest clear-key-cancels-inflight-render-test
  (let [manager (fragment/create)
        started (promise)
        release (promise)
        cancelled (promise)
        render-count (atom 0)
        task (controlled-success-task started release :html render-count cancelled)
        runner (start-task (fragment/protect-task manager fragment-key task))]
    (is (= :started (deref started timeout-ms ::timeout)))
    (is (= 1 (:inflight-count (fragment/stats manager))))

    (fragment/clear-key! manager fragment-key)

    (is (= :cancelled (deref cancelled timeout-ms ::timeout)))
    (is (= 0 (:inflight-count (fragment/stats manager))))
    (cancel-runner! runner)))

;; -----------------------------------------------------------------------------
;; Debug events
;; -----------------------------------------------------------------------------

(deftest debug-events-cover-cache-and-render-path-test
  (let [events (atom [])
        manager (fragment/create {:ttl-ms 1000
                                  :debug-fn #(swap! events conj %)})
        render-count (atom 0)
        render-fn (fn []
                    (swap! render-count inc)
                    :html)]
    (is (= :html (:value (run-task
                          (fragment/render-task manager fragment-key render-fn)))))
    (is (= :html (:value (run-task
                          (fragment/render-task manager fragment-key render-fn)))))
    (is (= 1 @render-count))

    (let [names (event-names @events)]
      (is (contains? names :gesso.live.fragment/cache-miss))
      (is (contains? names :gesso.live.fragment/singleflight-owner))
      (is (contains? names :gesso.live.fragment/render-started))
      (is (contains? names :gesso.live.fragment/cache-stored))
      (is (contains? names :gesso.live.fragment/render-succeeded))
      (is (contains? names :gesso.live.fragment/singleflight-released))
      (is (contains? names :gesso.live.fragment/cache-hit)))))

(deftest debug-events-cover-singleflight-join-path-test
  (let [events (atom [])
        manager (fragment/create {:debug-fn #(swap! events conj %)})
        started (promise)
        release (promise)
        cancelled (promise)
        render-count (atom 0)
        task (controlled-success-task started release :html render-count cancelled)
        runner-1 (start-task (fragment/protect-task manager fragment-key task))
        _ (is (= :started (deref started timeout-ms ::timeout)))
        runner-2 (start-task (fragment/protect-task manager fragment-key task))]
    (deliver release :go)

    (is (= {:status :success :value :html}
           (task-result runner-1)))
    (is (= {:status :success :value :html}
           (task-result runner-2)))

    (let [names (event-names @events)]
      (is (contains? names :gesso.live.fragment/singleflight-owner))
      (is (contains? names :gesso.live.fragment/singleflight-joined))
      (is (contains? names :gesso.live.fragment/singleflight-waiting))
      (is (contains? names :gesso.live.fragment/singleflight-reused))
      (is (contains? names :gesso.live.fragment/singleflight-released)))))

(deftest debug-events-cover-render-failure-test
  (let [events (atom [])
        manager (fragment/create {:debug-fn #(swap! events conj %)})
        result (run-task
                (fragment/render-task manager fragment-key
                                      (fn []
                                        (throw
                                         (RuntimeException. "boom")))))]
    (is (= :failure (:status result)))
    (is (contains? (event-names @events)
                   :gesso.live.fragment/render-failed))))

(deftest debug-events-cover-cache-expiration-test
  (let [clock (atom 0)
        events (atom [])
        manager (fragment/create {:ttl-ms 10
                                  :clock #(deref clock)
                                  :debug-fn #(swap! events conj %)})
        render-fn (fn [] :html)]
    (is (= :html (:value (run-task
                          (fragment/render-task manager fragment-key render-fn)))))
    (swap! clock + 11)
    (is (= :html (:value (run-task
                          (fragment/render-task manager fragment-key render-fn)))))
    (is (contains? (event-names @events)
                   :gesso.live.fragment/cache-expired))))
