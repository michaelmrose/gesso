(ns gesso.live.fragment-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gesso.live.fragment :as fragment]
   [gesso.live.model :as model]
   [gesso.live.progression :as progression]
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

(defn hiccup-node?
  [x]
  (and (vector? x)
       (seq x)
       (or (keyword? (first x))
           (symbol? (first x))
           (string? (first x)))))

(defn attrs
  [node]
  (if (and (vector? node)
           (map? (second node)))
    (second node)
    {}))

(defn children
  [node]
  (cond
    (and (vector? node)
         (map? (second node)))
    (nnext node)

    (vector? node)
    (next node)

    (sequential? node)
    node

    :else
    nil))

(defn hiccup-nodes
  [root]
  (filter
   hiccup-node?
   (tree-seq
    (fn [x]
      (or (hiccup-node? x)
          (and (sequential? x)
               (not (string? x)))))
    children
    root)))

(defn find-by-id
  [root id]
  (some
   (fn [node]
     (when (= id (:id (attrs node)))
       node))
   (hiccup-nodes root)))

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

(defn allow-all?
  [_ctx _id]
  true)

(defn compiled-live-model
  []
  (model/compile-live-app
   {:response identity

    :scopes
    {:store
     {:topic :demo/store
      :id-key :store/id
      :label "Store"
      :authorized? allow-all?}}

    :graph
    {:demo/store-updated
     [:store]}

    :fragments
    {:store-panel
     {:scope :store
      :id-fn (fn [id]
               (str "store-panel-" id))
      :query (fn [_ctx id]
               {:store/id id})
      :render (fn [data]
                [:div data])
      :swap :outerHTML}}}))

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
  (let [requirement (progression/requirement-from-bases
                     [:basis/store-1 :basis/index-7])]
    (is (= [:gesso.live.fragment :store-queue
            :scope [:store "store-1"]
            :user-key [:user "u-1"]
            :variant :compact
            :params {:page 1}
            :locale :en
            :theme :dark
            :progression requirement]
           (fragment/fragment-key
            :store-queue
            {:theme :dark
             :params {:page 1}
             :scope [:store "store-1"]
             :unknown :ignored
             :progression requirement
             :variant :compact
             :user-key [:user "u-1"]
             :locale :en})))))

(deftest fragment-key-normalizes-orderless-progression-test
  (let [forward (progression/requirement-from-bases
                 [:basis/store-1 :basis/index-7])
        reverse (progression/requirement-from-bases
                 [:basis/index-7 :basis/store-1])]
    (is (= forward reverse))
    (is (= (fragment/fragment-key :store-queue {:progression forward})
           (fragment/fragment-key :store-queue {:progression reverse})))))

(deftest fragment-key-separates-distinct-progression-requirements-test
  (let [before (progression/requirement :basis/before)
        after (progression/requirement :basis/after)]
    (is (not= before after))
    (is (not= (fragment/fragment-key :store-queue {:progression before})
              (fragment/fragment-key :store-queue {:progression after})))))

(deftest fragment-key-omits-nil-progression-test
  (is (= (fragment/fragment-key :store-queue)
         (fragment/fragment-key :store-queue {:progression nil}))))

(deftest fragment-key-rejects-malformed-progression-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"normalized non-empty Gesso Live progression requirement"
       (fragment/fragment-key
        :store-queue
        {:progression {:basis :not-a-requirement}}))))

(deftest fragment-key-rejects-retired-consistency-token-test
  (let [error
        (try
          (fragment/fragment-key
           :store-queue
           {:consistency-token "tx-1"})
          nil
          (catch clojure.lang.ExceptionInfo e
            e))]
    (is (some? error))
    (is (re-find #"no longer accept :consistency-token"
                 (ex-message error)))
    (is (= :consistency-token
           (:unsupported-dimension (ex-data error))))))

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
  (let [requirement (progression/requirement :basis/store-1)]
    (is (= [:gesso.live.fragment :store-queue
            :scope [:store "store-1"]
            :user-key [:user "u-1"]
            :params {:page 1}
            :progression requirement]
           (fragment/strict-fragment-key
            {:fragment :store-queue
             :scope [:store "store-1"]
             :user-key [:user "u-1"]
             :params {:page 1}
             :progression requirement})))))

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

    ;; The flight completion cell becomes available before its releasing
    ;; callback emits this debug event. Await the event rather than assuming
    ;; task completion and debug publication are one atomic operation.
    (is (eventually
         #(contains? (event-names @events)
                     :gesso.live.fragment/singleflight-released)))

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
    ;; Wait until the second request has actually joined the first flight.
    ;; Starting the task alone does not guarantee the scheduler has run it.
    (is (eventually
         #(contains? (event-names @events)
                     :gesso.live.fragment/singleflight-joined)))
    (deliver release :go)

    (is (= {:status :success :value :html}
           (task-result runner-1)))
    (is (= {:status :success :value :html}
           (task-result runner-2)))
    (is (eventually
         #(contains? (event-names @events)
                     :gesso.live.fragment/singleflight-released)))

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

;; -----------------------------------------------------------------------------
;; Model-backed fragment adapters
;; -----------------------------------------------------------------------------

(deftest fragment-runtime-fragment-defaults-from-model-test
  (let [compiled (compiled-live-model)
        runtime (fragment/fragment->runtime-fragment
                 compiled
                 :store-panel
                 "store-1"
                 {:fragment-url "/stores/store-1/fragment"
                  :stream-url "/stores/store-1/stream"})]
    (is (= "store-panel-store-1" (:id runtime)))
    (is (= "/stores/store-1/fragment" (:src runtime)))
    (is (= "/stores/store-1/stream" (:stream-url runtime)))
    (is (= :outerHTML (:swap runtime)))
    (is (= {:topic :demo/store
            :id "store-1"
            :gesso.live/scope :store
            :gesso.live/scope-label "Store"}
           (:subscription runtime)))))

(deftest fragment-runtime-fragment-requires-fragment-url-test
  (let [compiled (compiled-live-model)]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"fragment-url is required"
         (fragment/fragment->runtime-fragment
          compiled
          :store-panel
          "store-1"
          {:stream-url "/stores/store-1/stream"})))))

(deftest fragment-runtime-fragment-requires-stream-url-test
  (let [compiled (compiled-live-model)]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"stream-url is required"
         (fragment/fragment->runtime-fragment
          compiled
          :store-panel
          "store-1"
          {:fragment-url "/stores/store-1/fragment"})))))

(deftest fragment-runtime-fragment-passes-managed-ui-options-test
  (let [compiled (compiled-live-model)
        continuity-config {:preserve {:inputs true}}
        runtime (fragment/fragment->runtime-fragment
                 compiled
                 :store-panel
                 "store-1"
                 {:fragment-url "/stores/store-1/fragment"
                  :stream-url "/stores/store-1/stream"
                  :swap :innerHTML
                  :attrs {:data-fragment "store-panel"}
                  :root-attrs {:data-root "root"
                               :hx-ext "path-deps"}
                  :target-attrs {:data-target "target"
                                 :hx-include "#store-board-state"
                                 :hx-indicator "#spinner"}
                  :event :store-updated
                  :client-continuity continuity-config})]
    (is (= "store-panel-store-1" (:id runtime)))
    (is (= "/stores/store-1/fragment" (:src runtime)))
    (is (= "/stores/store-1/stream" (:stream-url runtime)))
    (is (= :innerHTML (:swap runtime)))
    (is (= {:data-fragment "store-panel"}
           (:attrs runtime)))
    (is (= {:data-root "root"
            :hx-ext "path-deps"}
           (:root-attrs runtime)))
    (is (= {:data-target "target"
            :hx-include "#store-board-state"
            :hx-indicator "#spinner"}
           (:target-attrs runtime)))
    (is (= :store-updated (:event runtime)))
    (is (= continuity-config (:client-continuity runtime)))
    (is (not (contains? runtime :trigger)))
    (is (not (contains? runtime :jitter-ms)))
    (is (not (contains? runtime :jitter-delay-ms)))))

(deftest fragment-runtime-fragment-rejects-unmanaged-refresh-options-test
  (let [compiled (compiled-live-model)
        base-opts {:fragment-url "/stores/store-1/fragment"
                   :stream-url "/stores/store-1/stream"}]
    (doseq [[option value] [[:trigger "load"]
                            [:jitter-ms 250]
                            [:jitter-delay-ms 25]]]
      (let [error (try
                    (fragment/fragment->runtime-fragment
                     compiled
                     :store-panel
                     "store-1"
                     (assoc base-opts option value))
                    nil
                    (catch clojure.lang.ExceptionInfo e
                      e))]
        (is (some? error))
        (is (re-find #"no longer accept direct HTMX refresh trigger/jitter options"
                     (ex-message error)))
        (is (= {option value}
               (:unsupported-options (ex-data error))))))))

(deftest model-fragment-panel-renders-managed-refresh-markup-test
  (let [compiled (compiled-live-model)
        panel (fragment/model-fragment-panel
               compiled
               :store-panel
               "store-1"
               {:fragment-url "/stores/store-1/fragment"
                :stream-url "/stores/store-1/stream"

                ;; The stable root owns request-affecting attrs that do not
                ;; create an alternate request path.
                :root-attrs {:data-root "root"
                             :hx-ext "path-deps"
                             :hx-include "#store-board-state"
                             :hx-indicator "#spinner"}

                ;; The target remains a replaceable presentation node.
                :target-attrs {:data-target "target"}

                :event :store-updated})
        root-attrs (attrs panel)
        target (find-by-id panel "store-panel-store-1")
        target-attrs (attrs target)
        invalidation-node
        (some
         (fn [node]
           (when (= "store-panel-store-1"
                    (:data-gesso-live-invalidation (attrs node)))
             node))
         (hiccup-nodes panel))
        invalidation-attrs (attrs invalidation-node)]
    (is (= :div (first panel)))
    (is (= "store-panel-store-1"
           (:data-gesso-live-fragment root-attrs)))
    (is (= "/stores/store-1/stream"
           (:sse-connect root-attrs)))
    (is (str/includes? (:hx-ext root-attrs) "sse"))
    (is (str/includes? (:hx-ext root-attrs) "path-deps"))
    (is (= "root" (:data-root root-attrs)))

    ;; The stable root is the only HTMX request owner, and the adapter-authorized
    ;; custom event is the only trigger for that GET.
    (is (= "/stores/store-1/fragment" (:hx-get root-attrs)))
    (is (= "gesso:live-refresh" (:hx-trigger root-attrs)))
    (is (= "#store-panel-store-1" (:hx-target root-attrs)))
    (is (= "outerHTML" (:hx-swap root-attrs)))
    (is (= "#store-board-state" (:hx-include root-attrs)))
    (is (= "#spinner" (:hx-indicator root-attrs)))
    (is (not (str/includes? (:hx-trigger root-attrs) "sse:")))

    ;; The stable SSE listener subscribes to the named event but cannot itself
    ;; issue a request or swap advisory payload bytes into application markup.
    (is (some? invalidation-node))
    (is (= "store-updated" (:sse-swap invalidation-attrs)))
    (is (= "none" (:hx-swap invalidation-attrs)))
    (is (= "true" (:aria-hidden invalidation-attrs)))
    (is (nil? (:hx-get invalidation-attrs)))
    (is (nil? (:hx-trigger invalidation-attrs)))

    (is (some? target))
    (is (= :div (first target)))
    (is (= "store-panel-store-1" (:id target-attrs)))
    (is (= "target" (:data-target target-attrs)))
    (is (nil? (:hx-get target-attrs)))
    (is (nil? (:hx-trigger target-attrs)))
    (is (nil? (:sse-swap target-attrs)))))

;; -----------------------------------------------------------------------------
;; Diagnostic callback failures must not change fragment delivery
;; -----------------------------------------------------------------------------

(deftest throwing-debug-hook-does-not-break-cache-hit-or-render-test
  (let [attempts (atom [])
        renders (atom 0)
        manager (fragment/create
                 {:ttl-ms 1000
                  :clock (constantly 10)
                  :debug-fn (fn [event]
                              (swap! attempts conj (:event event))
                              (throw (ex-info "diagnostic unavailable" {})))})
        render-fn (fn []
                    (swap! renders inc)
                    :html)]
    (is (= {:status :success :value :html}
           (run-task (fragment/render-task manager fragment-key render-fn))))
    (is (= {:status :success :value :html}
           (run-task (fragment/render-task manager fragment-key render-fn))))
    (is (= 1 @renders)
        "a diagnostic exception must not disable useful TTL caching")
    (is (= 1 (:cache-count (fragment/stats manager))))
    (is (some #{:gesso.live.fragment/cache-miss} @attempts))
    (is (some #{:gesso.live.fragment/cache-hit} @attempts))))

(deftest throwing-debug-hook-does-not-strand-singleflight-owner-test
  (let [manager (fragment/create
                 {:debug-fn (fn [event]
                              (when (= :gesso.live.fragment/singleflight-owner
                                       (:event event))
                                (throw (ex-info "owner trace unavailable" {}))))})]
    (try
      (is (= {:status :success :value :html}
             (run-task (fragment/render-task manager fragment-key
                                             (fn [] :html)))))
      (is (eventually #(zero? (:inflight-count (fragment/stats manager))))
          "the owner must start and release its flight despite tracing errors")
      (finally
        (fragment/clear-key! manager fragment-key)))))

(deftest throwing-debug-hook-does-not-replace-render-failure-test
  (let [cause (ex-info "original render failed" {:operation :render})
        attempts (atom 0)
        manager (fragment/create
                 {:singleflight? false
                  :debug-fn (fn [event]
                              (when (= :gesso.live.fragment/render-failed
                                       (:event event))
                                (swap! attempts inc)
                                (throw (ex-info "trace failed" {}))))})
        result (run-task (fragment/render-task manager fragment-key
                                               (fn [] (throw cause))))]
    (is (= :failure (:status result)))
    (is (identical? cause (:error result))
        "the real renderer exception must not be replaced by debug failure")
    (is (= 1 @attempts))))

(deftest clear-key-does-not-unregister-replacement-flight-after-cancel-test
  ;; A canceller may release the previous flight synchronously. Force a new
  ;; owner to register before clear-key! resumes after cancelling the old one.
  ;; Clearing by key alone would delete that *new* owner's registration,
  ;; allowing another request to start a duplicate render of the same key.
  (let [manager (fragment/create)
        first-started (promise)
        first-release (promise)
        first-cancelled (promise)
        first-count (atom 0)
        first-task (controlled-success-task first-started first-release
                                            :obsolete first-count first-cancelled)
        first-runner (start-task (fragment/protect-task manager fragment-key first-task))
        second-started (promise)
        second-release (promise)
        second-cancelled (promise)
        second-count (atom 0)
        second-task (controlled-success-task second-started second-release
                                             :fresh second-count second-cancelled)
        replacement (atom nil)
        original-cancel @#'gesso.live.fragment/cancel-flight!]
    (try
      (is (= :started (deref first-started timeout-ms ::timeout)))
      (is (= 1 (:inflight-count (fragment/stats manager))))
      (with-redefs-fn
        {#'gesso.live.fragment/cancel-flight!
         (fn [flight]
           (original-cancel flight)
           ;; Reproduce an old-flight completion removing its own identity
           ;; before clear-key! has performed its final map update.
           (swap! (:inflight manager)
                  (fn [flights]
                    (if (identical? flight (get flights fragment-key))
                      (dissoc flights fragment-key)
                      flights)))
           (reset! replacement
                   (start-task (fragment/protect-task manager fragment-key second-task)))
           (is (= :started (deref second-started timeout-ms ::timeout))))}
        #(fragment/clear-key! manager fragment-key))
      (is (= 1 (:inflight-count (fragment/stats manager)))
          "clear-key must not remove a newer flight installed during cancellation")
      (let [joining (start-task (fragment/protect-task manager fragment-key second-task))]
        (is (eventually #(= 1 @second-count))
            "a joining request must share the new owner's render")
        (deliver second-release :go)
        (is (= {:status :success :value :fresh} (task-result @replacement)))
        (is (= {:status :success :value :fresh} (task-result joining)))
        (is (= 1 @second-count)
            "a replacement flight must still coalesce subsequent requests"))
      (finally
        (deliver first-release :go)
        (deliver second-release :go)
        (cancel-runner! first-runner)
        (when-let [runner @replacement]
          (cancel-runner! runner))
        (fragment/clear-key! manager fragment-key)))))

(deftest clear-key-cancels-flight-when-canceller-published-after-invalidation-test
  ;; The owner registers an in-flight key before start-flight! has installed
  ;; the task's canceller. Invalidating that key must remember cancellation
  ;; and apply it when the task startup publishes its cancellation handle.
  (let [manager (fragment/create)
        acquired (promise)
        continue-start (promise)
        cancel-count (atom 0)
        render-starts (atom 0)
        original-start @#'gesso.live.fragment/start-flight!
        render (fn [_success _failure]
                 (swap! render-starts inc)
                 (fn [] (swap! cancel-count inc)))]
    (with-redefs-fn
      {#'gesso.live.fragment/start-flight!
       (fn [m k task options flight]
         (deliver acquired flight)
         @continue-start
         (original-start m k task options flight))}
      (fn []
        (let [runner-future
              (future (start-task (fragment/protect-task manager fragment-key render)))]
          (try
            (let [flight (deref acquired timeout-ms ::timeout)]
              (is (map? flight) "owner should have acquired a flight")
              (when (map? flight)
                (is (nil? @(:cancel-ref flight)) "canceller has not yet been registered")
                (is (= 1 (:inflight-count (fragment/stats manager))))
                (fragment/clear-key! manager fragment-key)
                (is (= 0 (:inflight-count (fragment/stats manager))))
                (deliver continue-start true)
                (is (eventually #(= 1 @render-starts)))
                (is (eventually #(= 1 @cancel-count))
                    "late-published cancellation handle must be called")
                (is (= 1 @cancel-count)
                    "the same task cancellation must happen exactly once")
                (@#'gesso.live.fragment/cancel-flight! flight)
                (is (= 1 @cancel-count)
                    "a second cancellation request must not invoke the handle twice")))
            (finally
              (deliver continue-start true)
              (when-let [runner (deref runner-future timeout-ms nil)]
                (cancel-runner! runner))
              (fragment/clear-key! manager fragment-key))))))))

(deftest expired-cache-lookup-preserves-concurrent-fresh-store-test
  ;; Expiration checks run outside the cache atom's update. While an expired
  ;; lookup is paused after reading the old entry, another successful render
  ;; can replace it with a fresh entry. The old lookup must not evict the
  ;; replacement just because it occupies the same key.
  (let [now (atom 0)
        manager (fragment/create {:ttl-ms 10 :clock #(deref now)})
        cache (:cache manager)
        old-entry {:value :old :stored-at 0 :expires-at 10 :ttl-ms 10}
        observed (promise)
        proceed (promise)
        render-count (atom 0)
        original-valid? @#'gesso.live.fragment/cache-entry-valid?]
    (swap! cache assoc fragment-key old-entry)
    (reset! now 11)
    (with-redefs-fn
      {#'gesso.live.fragment/cache-entry-valid?
       (fn [t entry]
         (when (identical? old-entry entry)
           (deliver observed :expired-read)
           @proceed)
         (original-valid? t entry))}
      (fn []
        (let [lookup (future
                       (@#'gesso.live.fragment/cache-lookup
                        manager fragment-key (:options manager)))]
          (try
            (is (= :expired-read (deref observed timeout-ms ::timeout))
                "expired lookup must have read its old entry")
            ;; This is the same cache-store path used by a concurrent render.
            (@#'gesso.live.fragment/cache-store!
             manager fragment-key :fresh (:options manager))
            (is (= :fresh (get-in @cache [fragment-key :value])))
            (deliver proceed true)
            (is (= fragment/miss (deref lookup timeout-ms ::timeout)))
            (is (= :fresh (get-in @cache [fragment-key :value]))
                "expired lookup must preserve newer cached fragment")
            (is (= {:status :success :value :fresh}
                   (run-task (fragment/render-task
                              manager fragment-key
                              (fn [] (swap! render-count inc) :unneeded)))))
            (is (zero? @render-count)
                "subsequent request should reuse newly rendered fragment")
            (finally
              (deliver proceed true)
              (deref lookup timeout-ms ::timeout))))))))

(deftest cached-fragment-value-equal-to-public-miss-is-not-re-rendered-test
  ;; protect-task/render-task accept arbitrary values, including the public
  ;; sentinel formerly used to signal a failed cache lookup. A completed
  ;; render containing that value must be cached just like any other value.
  (let [renders (atom 0)
        manager (fragment/create {:ttl-ms 1000 :clock (constantly 0)})
        render #(do (swap! renders inc) fragment/miss)]
    (is (= {:status :success :value fragment/miss}
           (run-task (fragment/render-task manager fragment-key render))))
    (is (= 1 (:cache-count (fragment/stats manager))))
    (is (= {:status :success :value fragment/miss}
           (run-task (fragment/render-task manager fragment-key render))))
    (is (= 1 @renders)
        "a legitimate cached value must not be mistaken for a cache miss")))

(deftest per-render-shorter-ttl-must-not-reuse-older-cache-entry-test
  ;; A render-task call can override the manager's cache TTL. A value older
  ;; than the calling render's chosen TTL must not be reused merely because
  ;; it was originally stored with a longer TTL. Conversely, a later caller
  ;; with a longer TTL must not resurrect an entry whose stored TTL expired.
  (let [now (atom 0)
        renders (atom 0)
        manager (fragment/create {:ttl-ms 100 :clock #(deref now)})
        render #(swap! renders inc)
        run (fn [options]
              (run-task (fragment/render-task manager fragment-key render options)))]
    (is (= {:status :success :value 1} (run nil)))
    (reset! now 20)
    (is (= {:status :success :value 2} (run {:ttl-ms 10}))
        "requesting a 10-ms TTL cannot reuse a 20-ms-old entry")
    (reset! now 25)
    (is (= {:status :success :value 2} (run {:ttl-ms 10}))
        "the new entry should be reusable within the narrower TTL")
    (reset! now 35)
    (is (= {:status :success :value 3} (run {:ttl-ms 10}))
        "the 10-ms entry expires at the requested boundary")
    (reset! now 50)
    (is (= {:status :success :value 4} (run {:ttl-ms 100}))
        "a longer request TTL cannot resurrect a previously expired entry")
    (is (= 4 @renders))))

(deftest max-cache-entries-evicts-earliest-write-with-tied-clock-test
  ;; Fixed millisecond clocks are normal for bursty concurrent fragment writes.
  ;; Eviction must never discard the freshly stored value merely because
  ;; several entries have the same :stored-at and hash-map order is arbitrary.
  (let [manager (fragment/create {:ttl-ms 1000
                                  :max-cache-entries 8
                                  :clock (constantly 0)})
        cache (:cache manager)
        options (:options manager)]
    (doseq [key (range 30)]
      (@#'fragment/cache-store! manager key [:rendered key] options)
      (is (contains? @cache key)
          (str "the newly rendered fragment " key " must survive its own store")))
    (is (= 8 (:cache-count (fragment/stats manager))))
    (is (= (set (range 22 30)) (set (keys @cache)))
        "bounded cache must retain the latest eight writes at a tied clock")))

(deftest bounded-cache-evicts-by-publication-order-when-writes-overlap-test
  ;; The clock is monotonic, but a render can read its time and be delayed
  ;; during cache publication while another render publishes first. Ordering
  ;; eviction by timestamp then :write-order would discard the newest
  ;; *published* fragment, even though a publication order is available.
  (let [tick (atom 1000)
        manager (fragment/create {:ttl-ms 10000
                                  :max-cache-entries 1
                                  :clock #(swap! tick inc)})
        cache (:cache manager)
        options (:options manager)
        first-paused (promise)
        resume-first (promise)
        original-prune @#'gesso.live.fragment/prune-cache]
    (with-redefs-fn
      {#'gesso.live.fragment/prune-cache
       (fn [entries now cap]
         (when (and (contains? entries :earlier-clock)
                    (not (contains? entries :later-clock)))
           (deliver first-paused true)
           @resume-first)
         (original-prune entries now cap))}
      (fn []
        (let [early (future
                      (@#'gesso.live.fragment/cache-store!
                       manager :earlier-clock :later-publication options))]
          (try
            (is (= true (deref first-paused timeout-ms ::timeout))
                "the first write must be paused after capturing its clock")
            (@#'gesso.live.fragment/cache-store!
             manager :later-clock :earlier-publication options)
            (is (= #{:later-clock} (set (keys @cache))))
            (deliver resume-first true)
            (is (= :later-publication (deref early timeout-ms ::timeout)))
            (is (= #{:earlier-clock} (set (keys @cache)))
                "last published value must survive a bounded cache eviction")
            (is (= :later-publication (get-in @cache [:earlier-clock :value])))
            (finally
              (deliver resume-first true)
              (deref early timeout-ms ::timeout))))))))

(deftest clear-key-during-completion-must-not-cache-stale-flight-test
  ;; Invalidation wins while a completed render is paused immediately before
  ;; publishing to cache. An obsolete flight cannot repopulate that cache.
  (let [manager (fragment/create {:ttl-ms 1000 :clock (constantly 0)})
        reached-store (promise)
        resume-store (promise)
        completed-store (promise)
        original-store @#'gesso.live.fragment/cache-store!]
    (with-redefs-fn
      {#'gesso.live.fragment/cache-store!
       (fn [& args]
         (deliver reached-store true)
         @resume-store
         (try
           (apply original-store args)
           (finally (deliver completed-store true))))}
      (fn []
        (let [runner-future (future (start-task
                                    (fragment/render-task manager fragment-key
                                                          (fn [] :stale))))
              invalidation (atom nil)]
          (try
            (is (= true (deref reached-store timeout-ms ::timeout)))
            (let [flight (get @(:inflight manager) fragment-key)]
              (is (some? flight))
              (reset! invalidation (future (fragment/clear-key! manager fragment-key)))
              (is (eventually #(true? @(:cancel-requested? flight)))
                  "invalidation records cancellation before the store unblocks"))
            (deliver resume-store true)
            (is (= true (deref completed-store timeout-ms ::timeout))
                "obsolete render should attempt its delayed publication")
            (is (not= ::timeout (deref @invalidation timeout-ms ::timeout)))
            (is (nil? (get @(:cache manager) fragment-key))
                "invalidated flight must not restore its stale cache entry")
            (finally
              (deliver resume-store true)
              (when-let [invalidation-future @invalidation]
                (deref invalidation-future timeout-ms ::timeout))
              (when-let [runner (deref runner-future timeout-ms nil)]
                (cancel-runner! runner)))))))))

(deftest stricter-request-ttl-does-not-evict-still-valid-cached-entry-test
  ;; A request with a shorter freshness bound needs a rerender, but a failed
  ;; rerender must not evict an entry that is still fresh for other callers.
  (let [now (atom 0)
        manager (fragment/create {:ttl-ms 100 :clock #(deref now)})
        broad-renders (atom 0)
        original-render (fn [] (swap! broad-renders inc) :original)
        strict-error (ex-info "strict renderer failed" {})
        strict-renders (atom 0)
        strict-render (fn []
                        (swap! strict-renders inc)
                        (throw strict-error))]
    (is (= {:status :success :value :original}
           (run-task (fragment/render-task manager fragment-key original-render))))
    (reset! now 20)
    (let [result (run-task (fragment/render-task manager fragment-key
                                                 strict-render {:ttl-ms 10}))]
      (is (= :failure (:status result))
          "stricter TTL must reject older markup and execute its renderer")
      (is (identical? strict-error (:error result))))
    (is (= 1 @strict-renders))
    (is (= :original (get-in @(:cache manager) [fragment-key :value]))
        "a stricter request must not discard an entry still valid for other callers")
    (is (= {:status :success :value :original}
           (run-task (fragment/render-task manager fragment-key original-render)))
        "the less restrictive caller should still reuse its cached result")
    (is (= 1 @broad-renders))
    (reset! now 100)
    (is (= {:status :success :value :original}
           (run-task (fragment/render-task manager fragment-key original-render))))
    (is (= 2 @broad-renders)
        "the original entry must still expire under its own stored TTL")))

(deftest clear-cache-does-not-repopulate-from-inflight-render-test
  ;; A render can finish its work before an explicit cache clear but pause
  ;; immediately before publishing. A cache clear must not be undone by a
  ;; late publication, regardless of whether singleflight is enabled.
  (doseq [singleflight? [true false]]
    (let [manager (fragment/create {:ttl-ms 1000
                                    :clock (constantly 0)
                                    :singleflight? singleflight?})
          reached-store (promise)
          resume-store (promise)
          original-store @#'gesso.live.fragment/cache-store!]
      (with-redefs-fn
        {#'gesso.live.fragment/cache-store!
         (fn [& args]
           (deliver reached-store true)
           @resume-store
           (apply original-store args))}
        (fn []
          (let [runner-future
                (future (start-task (fragment/render-task
                                    manager fragment-key (fn [] :obsolete))))]
            (try
              (is (= true (deref reached-store timeout-ms ::timeout))
                  (str "render reached publication: " singleflight?))
              (fragment/clear-cache! manager)
              (is (= {} @(:cache manager)))
              (deliver resume-store true)
              (is (not= ::timeout (deref runner-future timeout-ms ::timeout)))
              (is (eventually #(zero? (:inflight-count (fragment/stats manager)))))
              (is (= {} @(:cache manager))
                  (str "an old render must not resurrect cached HTML: " singleflight?))
              (is (= {:status :success :value :current}
                     (run-task (fragment/render-task manager fragment-key
                                                     (fn [] :current))))
                  "a new render must work after clear-cache!")
              (finally
                (deliver resume-store true)
                (when-let [runner (deref runner-future timeout-ms nil)]
                  (cancel-runner! runner))))))))))
