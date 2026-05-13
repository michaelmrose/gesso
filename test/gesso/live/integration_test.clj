(ns gesso.live.integration-test
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.live.consistency.xtdb :as xtdb-live]
   [xtdb.api :as xt]
   [gesso.live.dispatch :as dispatch]
   [gesso.live.flow :as flow]
   [gesso.live.fragment :as fragment]
   [gesso.live.invalidation :as invalidation]
   [gesso.live.source :as source]
   [gesso.live.core :as live]
   [manifold.stream :as s]
   [gesso.live.transport.sse :as sse]
   [missionary.core :as m])
  (:import
   [xtdb.api DataSource])
  )

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------


(def timeout-ms 1000)

(defn cancel-runner!
  [runner]
  ((:cancel runner)))

(defn task-result
  [runner]
  (let [result (deref (:promise runner) timeout-ms ::timeout)]
    (when (= ::timeout result)
      (cancel-runner! runner))
    result))

(defn start-task
  [task]
  (let [p (promise)
        cancel (task #(deliver p {:status :success :value %})
                     #(deliver p {:status :failure :error %}))]
    {:promise p
     :cancel cancel}))

(defn run-task
  [task]
  (task-result (start-task task)))

(defn controlled-fragment-task
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

(defn request-fragment-key
  [token]
  (fragment/strict-fragment-key
   {:fragment :request-panel
    :scope [:request "req-1"]
    :user-key [:user "u-1"]
    :params {:tab :summary}
    :consistency-token token}))


(def sample-xtdb-tx
  [[:put-docs :requests {:xt/id "req-1"
                         :status :open}]])

(defn fake-xtdb-datasource
  ([] (fake-xtdb-datasource nil))
  ([initial-token]
   (let [token (atom initial-token)]
     (reify DataSource
       (getAwaitToken [_]
         @token)
       (setAwaitToken [_ token']
         (reset! token token'))
       (createConnectionBuilder [_]
         nil)))))

(defn request-panel-key-from-consistency
  [consistency]
  (live/strict-fragment-key
   (xtdb-live/with-consistency-dimension
     {:fragment :request-panel
      :scope [:request "req-1"]
      :user-key [:user "u-1"]
      :params {:tab :summary}}
     consistency)))

(defn request-panel-key-from-ctx
  [ctx]
  (live/strict-fragment-key
   (xtdb-live/with-consistency-dimension-from
     {:fragment :request-panel
      :scope [:request "req-1"]
      :user-key [:user "u-1"]
      :params {:tab :summary}}
     ctx)))


(def ctx
  {:app/name :integration-test})

(def request-change
  {:topic :request
   :id "req-1"
   :change/kind :updated})

(def other-request-change
  {:topic :request
   :id "req-2"
   :change/kind :updated})

(def bad-change
  {:topic :global-announcement
   :change/kind :updated})

(def request-sub
  {:topic :request
   :id "req-1"})

(def other-request-sub
  {:topic :request
   :id "req-2"})

(def store-sub
  {:topic :store-queue
   :id "store-1"})

(def manager-sub
  {:topic :manager-dashboard
   :id "store-1"})

(def store-invalidation
  {:topic :store-queue
   :id "store-1"
   :change/kind :updated})

(def manager-invalidation
  {:topic :manager-dashboard
   :id "store-1"
   :change/kind :updated})

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

(defn take-value
  [stream]
  (deref (s/take! stream) timeout-ms ::timeout))

(defn close-source
  [src]
  (source/close! src))

(defn close-dispatcher
  [dispatcher]
  (dispatch/close! dispatcher))







(defn collect-runner
  [f]
  (start-task (flow/collect-task f)))

(defn live-event
  [invalidation]
  {:event "live-update"
   :invalidation invalidation})

(defn request-rules
  []
  (invalidation/compile-rules
   [{:when-topic :request
     :expand (fn [_ctx change]
               [{:topic :request
                 :id (:id change)
                 :change/kind (:change/kind change)}
                store-invalidation
                manager-invalidation])}]))

(defn emit-expanded!
  [src rules change]
  (let [expanded (invalidation/expand rules ctx change)]
    (source/emit-many! src expanded)))

(defn event-names
  [events]
  (set (map :event events)))

;; -----------------------------------------------------------------------------
;; invalidation.clj + source.clj
;; -----------------------------------------------------------------------------

(deftest expanded-invalidations-emit-through-source-test
  (let [src (source/create {:id :integration/source})
        tap (source/changes src)
        rules (request-rules)]
    (try
      (let [result (emit-expanded! src rules request-change)]
        (is (= :emitted-many (:status result)))
        (is (= 3 (:count result)))
        (is (= request-change (take-value tap)))
        (is (= store-invalidation (take-value tap)))
        (is (= manager-invalidation (take-value tap))))
      (finally
        (close-source src)))))

(deftest unmatched-valid-primary-change-can-emit-through-source-test
  (let [src (source/create)
        tap (source/changes src)]
    (try
      (let [expanded (invalidation/expand [] ctx request-change)
            result (source/emit-many! src expanded)]
        (is (= [request-change] expanded))
        (is (= :emitted-many (:status result)))
        (is (= request-change (take-value tap))))
      (finally
        (close-source src)))))

(deftest unmatched-invalid-primary-change-does-not-reach-source-test
  (let [src (source/create)
        tap (source/changes src)]
    (try
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"cannot be kept as an invalidation"
           (emit-expanded! src [] bad-change)))
      (is (= ::timeout (take-value tap)))
      (is (= 0 (:emitted-count (source/stats src))))
      (finally
        (close-source src)))))

;; -----------------------------------------------------------------------------
;; dispatch.clj + invalidation.clj + source.clj
;; -----------------------------------------------------------------------------

(deftest async-dispatch-can-expand-and-emit-through-source-test
  (let [src (source/create)
        tap (source/changes src)
        rules (request-rules)
        done (promise)
        dispatcher (dispatch/create {:threads 1
                                     :queue-size 8
                                     :on-overflow :throw
                                     :on-error (fn [error entry]
                                                 (deliver done {:error error
                                                                :entry entry}))})]
    (try
      (dispatch/submit!
       dispatcher
       {:run (fn []
               (emit-expanded! src rules request-change)
               (deliver done :ok))})

      (is (= :ok (deref done timeout-ms ::timeout)))
      (is (= request-change (take-value tap)))
      (is (= store-invalidation (take-value tap)))
      (is (= manager-invalidation (take-value tap)))
      (finally
        (close-dispatcher dispatcher)
        (close-source src)))))

(deftest async-dispatch-records-expansion-failure-test
  (let [src (source/create)
        done (promise)
        dispatcher (dispatch/create {:threads 1
                                     :queue-size 8
                                     :on-overflow :throw
                                     :on-error (fn [error entry]
                                                 (deliver done {:error error
                                                                :entry entry}))})]
    (try
      (dispatch/submit!
       dispatcher
       {:run (fn []
               (emit-expanded! src [] bad-change))})

      (let [result (deref done timeout-ms ::timeout)]
        (is (not= ::timeout result))
        (is (some? (:error result)))
        (is (some? (:entry result)))
        (is (= 0 (:emitted-count (source/stats src)))))
      (finally
        (close-dispatcher dispatcher)
        (close-source src)))))

;; -----------------------------------------------------------------------------
;; source.clj + flow.clj
;; -----------------------------------------------------------------------------

(deftest source-flow-emits-only-matching-live-events-test
  (let [src (source/create)
        f (flow/flow-for-source src {:subscription request-sub
                                     :relieve? false})
        runner (collect-runner f)]
    (try
      (source/emit! src store-invalidation)
      (source/emit! src request-change)
      (source/close! src)
      (is (= {:status :success
              :value [(live-event request-change)]}
             (task-result runner)))
      (finally
        (close-source src)))))

(deftest source-flow-completes-empty-when-no-events-match-test
  (let [src (source/create)
        f (flow/flow-for-source src {:subscription other-request-sub
                                     :relieve? false})
        runner (collect-runner f)]
    (try
      (source/emit! src store-invalidation)
      (source/emit! src manager-invalidation)
      (source/close! src)
      (is (= {:status :success
              :value []}
             (task-result runner)))
      (finally
        (close-source src)))))

(deftest source-flow-supports-debug-tracing-test
  (let [events (atom [])
        src (source/create)
        f (flow/flow-for-source src {:subscription request-sub
                                     :relieve? false
                                     :debug-fn #(swap! events conj %)})
        runner (collect-runner f)]
    (try
      (source/emit! src store-invalidation)
      (source/emit! src request-change)
      (source/close! src)

      (is (= {:status :success
              :value [(live-event request-change)]}
             (task-result runner)))

      (let [names (event-names @events)]
        (is (contains? names :gesso.live.flow/source-tap-opened))
        (is (contains? names :gesso.live.flow/invalidation-received))
        (is (contains? names :gesso.live.flow/invalidation-ignored))
        (is (contains? names :gesso.live.flow/invalidation-matched))
        (is (eventually #(contains? (event-names @events)
                                    :gesso.live.flow/source-tap-closed))))
      (finally
        (close-source src)))))

(deftest source-flow-early-downstream-termination-closes-source-tap-test
  (let [src (source/create)
        f (m/eduction
           (take 1)
           (flow/flow-for-source src {:subscription request-sub
                                      :relieve? false}))
        runner (collect-runner f)]
    (try
      (source/emit! src request-change)

      (is (= {:status :success
              :value [(live-event request-change)]}
             (task-result runner)))
      (is (eventually #(= 0 (:tap-count (source/stats src)))))
      (finally
        (close-source src)))))

;; -----------------------------------------------------------------------------
;; invalidation.clj + source.clj + flow.clj
;; -----------------------------------------------------------------------------

(deftest invalidation-source-flow-pipeline-emits-request-event-test
  (let [src (source/create)
        f (flow/flow-for-source src {:subscription request-sub
                                     :relieve? false})
        runner (collect-runner f)
        rules (request-rules)]
    (try
      (emit-expanded! src rules request-change)
      (source/close! src)

      (is (= {:status :success
              :value [(live-event request-change)]}
             (task-result runner)))
      (finally
        (close-source src)))))

(deftest invalidation-source-flow-pipeline-emits-derived-store-event-test
  (let [src (source/create)
        f (flow/flow-for-source src {:subscription store-sub
                                     :relieve? false})
        runner (collect-runner f)
        rules (request-rules)]
    (try
      (emit-expanded! src rules request-change)
      (source/close! src)

      (is (= {:status :success
              :value [(live-event store-invalidation)]}
             (task-result runner)))
      (finally
        (close-source src)))))

(deftest invalidation-source-flow-pipeline-emits-derived-manager-event-test
  (let [src (source/create)
        f (flow/flow-for-source src {:subscription manager-sub
                                     :relieve? false})
        runner (collect-runner f)
        rules (request-rules)]
    (try
      (emit-expanded! src rules request-change)
      (source/close! src)

      (is (= {:status :success
              :value [(live-event manager-invalidation)]}
             (task-result runner)))
      (finally
        (close-source src)))))

(deftest invalidation-source-flow-pipeline-ignores-unrelated-change-test
  (let [src (source/create)
        f (flow/flow-for-source src {:subscription request-sub
                                     :relieve? false})
        runner (collect-runner f)]
    (try
      (source/emit! src other-request-change)
      (source/close! src)

      (is (= {:status :success
              :value []}
             (task-result runner)))
      (finally
        (close-source src)))))

;; -----------------------------------------------------------------------------
;; dispatch.clj + invalidation.clj + source.clj + flow.clj
;; -----------------------------------------------------------------------------

(deftest dispatch-invalidation-source-flow-pipeline-test
  (let [src (source/create)
        f (flow/flow-for-source src {:subscription store-sub
                                     :relieve? false})
        runner (collect-runner f)
        rules (request-rules)
        done (promise)
        dispatcher (dispatch/create {:threads 1
                                     :queue-size 8
                                     :on-overflow :throw
                                     :on-error (fn [error entry]
                                                 (deliver done {:error error
                                                                :entry entry}))})]
    (try
      (dispatch/submit!
       dispatcher
       {:run (fn []
               (emit-expanded! src rules request-change)
               (deliver done :ok))})

      (is (= :ok (deref done timeout-ms ::timeout)))

      (source/close! src)
      (is (= {:status :success
              :value [(live-event store-invalidation)]}
             (task-result runner)))
      (finally
        (close-dispatcher dispatcher)
        (close-source src)))))

(deftest dispatch-invalidation-source-flow-pipeline-preserves-multiple-subscribers-test
  (let [src (source/create)
        request-flow (flow/flow-for-source src {:subscription request-sub
                                                :relieve? false})
        store-flow (flow/flow-for-source src {:subscription store-sub
                                              :relieve? false})
        manager-flow (flow/flow-for-source src {:subscription manager-sub
                                                :relieve? false})
        request-runner (collect-runner request-flow)
        store-runner (collect-runner store-flow)
        manager-runner (collect-runner manager-flow)
        rules (request-rules)]
    (try
      (emit-expanded! src rules request-change)
      (source/close! src)

      (is (= {:status :success
              :value [(live-event request-change)]}
             (task-result request-runner)))
      (is (= {:status :success
              :value [(live-event store-invalidation)]}
             (task-result store-runner)))
      (is (= {:status :success
              :value [(live-event manager-invalidation)]}
             (task-result manager-runner)))
      (finally
        (close-source src)))))

(deftest dispatch-expansion-failure-does-not-emit-flow-event-test
  (let [src (source/create)
        f (flow/flow-for-source src {:subscription store-sub
                                     :relieve? false})
        runner (collect-runner f)
        done (promise)
        dispatcher (dispatch/create {:threads 1
                                     :queue-size 8
                                     :on-overflow :throw
                                     :on-error (fn [error entry]
                                                 (deliver done {:error error
                                                                :entry entry}))})]
    (try
      (dispatch/submit!
       dispatcher
       {:run (fn []
               (emit-expanded! src [] bad-change))})

      (let [result (deref done timeout-ms ::timeout)]
        (is (not= ::timeout result))
        (is (some? (:error result)))
        (is (= 0 (:emitted-count (source/stats src)))))

      (source/close! src)
      (is (= {:status :success
              :value []}
             (task-result runner)))
      (finally
        (close-dispatcher dispatcher)
        (close-source src)))))

;; -----------------------------------------------------------------------------
;; flow.clj + transport/sse.clj
;; -----------------------------------------------------------------------------

(deftest source-flow-sse-emits-frame-test
  (let [src (source/create)
        f (flow/flow-for-source src {:subscription request-sub
                                     :relieve? false})
        started (sse/start! f {:encoder (constantly "encoded")})]
    (try
      (source/emit! src request-change)

      (is (= "event: live-update\ndata: encoded\n\n"
             (take-value (:stream started))))

      (source/close! src)
      (is (eventually #(sse/closed? started)))
      (finally
        (sse/cancel! started)
        (close-source src)))))

(deftest source-flow-sse-ignores-nonmatching-event-test
  (let [src (source/create)
        f (flow/flow-for-source src {:subscription request-sub
                                     :relieve? false})
        started (sse/start! f {:encoder (constantly "encoded")})]
    (try
      (source/emit! src store-invalidation)
      (source/close! src)

      (is (eventually #(sse/closed? started)))
      (is (nil? (take-value (:stream started))))
      (finally
        (sse/cancel! started)
        (close-source src)))))

(deftest sse-cancel-closes-flow-source-tap-test
  (let [src (source/create)
        f (flow/flow-for-source src {:subscription request-sub
                                     :relieve? false})
        started (sse/start! f)]
    (try
      (is (= 1 (:tap-count (source/stats src))))

      (sse/cancel! started)

      (is (eventually #(sse/closed? started)))
      (is (eventually #(= 0 (:tap-count (source/stats src)))))
      (finally
        (sse/cancel! started)
        (close-source src)))))

(deftest flow-failure-reaches-sse-error-handler-test
  (let [src (source/create)
        seen-error (promise)
        f (flow/flow-for-source
           src
           {:subscription request-sub
            :relieve? false
            :data (fn [_invalidation]
                    (throw (RuntimeException. "render payload boom")))})
        started (sse/start! f {:on-error (fn [error _entry]
                                           (deliver seen-error error))})]
    (try
      (source/emit! src request-change)

      (let [error (deref seen-error timeout-ms ::timeout)]
        (is (not= ::timeout error))
        (is (= "gesso.live flow failed to build live event."
               (ex-message error)))
        (is (= "render payload boom"
               (ex-message (ex-cause error)))))

      (is (eventually #(sse/closed? started)))
      (finally
        (sse/cancel! started)
        (close-source src)))))

;; -----------------------------------------------------------------------------
;; invalidation.clj + source.clj + flow.clj + transport/sse.clj
;; -----------------------------------------------------------------------------

(deftest invalidation-source-flow-sse-pipeline-emits-derived-store-frame-test
  (let [src (source/create)
        seen-payload (atom nil)
        f (flow/flow-for-source src {:subscription store-sub
                                     :relieve? false})
        started (sse/start!
                 f
                 {:encoder (fn [payload]
                             (reset! seen-payload payload)
                             "encoded-store")})
        rules (request-rules)]
    (try
      (emit-expanded! src rules request-change)

      (is (= "event: live-update\ndata: encoded-store\n\n"
             (take-value (:stream started))))
      (is (= {:invalidation store-invalidation}
             @seen-payload))

      (source/close! src)
      (is (eventually #(sse/closed? started)))
      (finally
        (sse/cancel! started)
        (close-source src)))))

(deftest invalidation-source-flow-sse-preserves-multiple-subscribers-test
  (let [src (source/create)
        request-flow (flow/flow-for-source src {:subscription request-sub
                                                :relieve? false})
        store-flow (flow/flow-for-source src {:subscription store-sub
                                              :relieve? false})
        manager-flow (flow/flow-for-source src {:subscription manager-sub
                                                :relieve? false})
        request-started (sse/start! request-flow {:encoder (constantly "request")})
        store-started (sse/start! store-flow {:encoder (constantly "store")})
        manager-started (sse/start! manager-flow {:encoder (constantly "manager")})
        rules (request-rules)]
    (try
      (emit-expanded! src rules request-change)

      (is (= "event: live-update\ndata: request\n\n"
             (take-value (:stream request-started))))
      (is (= "event: live-update\ndata: store\n\n"
             (take-value (:stream store-started))))
      (is (= "event: live-update\ndata: manager\n\n"
             (take-value (:stream manager-started))))

      (source/close! src)
      (is (eventually #(sse/closed? request-started)))
      (is (eventually #(sse/closed? store-started)))
      (is (eventually #(sse/closed? manager-started)))
      (finally
        (sse/cancel! request-started)
        (sse/cancel! store-started)
        (sse/cancel! manager-started)
        (close-source src)))))

;; -----------------------------------------------------------------------------
;; dispatch.clj + invalidation.clj + source.clj + flow.clj + transport/sse.clj
;; -----------------------------------------------------------------------------

(deftest dispatch-invalidation-source-flow-sse-pipeline-test
  (let [src (source/create)
        f (flow/flow-for-source src {:subscription store-sub
                                     :relieve? false})
        started (sse/start! f {:encoder (constantly "encoded-store")})
        rules (request-rules)
        done (promise)
        dispatcher (dispatch/create {:threads 1
                                     :queue-size 8
                                     :on-overflow :throw
                                     :on-error (fn [error entry]
                                                 (deliver done {:error error
                                                                :entry entry}))})]
    (try
      (dispatch/submit!
       dispatcher
       {:run (fn []
               (emit-expanded! src rules request-change)
               (deliver done :ok))})

      (is (= :ok (deref done timeout-ms ::timeout)))
      (is (= "event: live-update\ndata: encoded-store\n\n"
             (take-value (:stream started))))

      (source/close! src)
      (is (eventually #(sse/closed? started)))
      (finally
        (sse/cancel! started)
        (close-dispatcher dispatcher)
        (close-source src)))))

(deftest dispatch-expansion-failure-produces-no-sse-frame-test
  (let [src (source/create)
        f (flow/flow-for-source src {:subscription store-sub
                                     :relieve? false})
        started (sse/start! f {:encoder (constantly "encoded")})
        done (promise)
        dispatcher (dispatch/create {:threads 1
                                     :queue-size 8
                                     :on-overflow :throw
                                     :on-error (fn [error entry]
                                                 (deliver done {:error error
                                                                :entry entry}))})]
    (try
      (dispatch/submit!
       dispatcher
       {:run (fn []
               (emit-expanded! src [] bad-change))})

      (let [result (deref done timeout-ms ::timeout)]
        (is (not= ::timeout result))
        (is (some? (:error result))))

      (source/close! src)
      (is (eventually #(sse/closed? started)))
      (is (nil? (take-value (:stream started))))
      (finally
        (sse/cancel! started)
        (close-dispatcher dispatcher)
        (close-source src)))))

;; -----------------------------------------------------------------------------
;; live wakeup + fragment render protection
;; -----------------------------------------------------------------------------

(deftest source-flow-sse-wakeup-can-trigger-protected-fragment-render-test
  (let [src (source/create)
        live-flow (flow/flow-for-source src {:subscription request-sub
                                             :relieve? false})
        started-sse (sse/start! live-flow {:encoder (constantly "wake")})
        manager (fragment/create {:ttl-ms 1000})
        render-count (atom 0)
        key (request-fragment-key "tx-1")]
    (try
      ;; Live layer wakes the client.
      (source/emit! src request-change)
      (is (= "event: live-update\ndata: wake\n\n"
             (take-value (:stream started-sse))))

      ;; Fragment endpoint render is protected separately.
      (is (= {:status :success
              :value "<section>request</section>"}
             (run-task
              (fragment/render-task
               manager
               key
               (fn []
                 (swap! render-count inc)
                 "<section>request</section>")))))

      ;; Same key within TTL reuses cached render.
      (is (= {:status :success
              :value "<section>request</section>"}
             (run-task
              (fragment/render-task
               manager
               key
               (fn []
                 (swap! render-count inc)
                 "<section>request</section>")))))

      (is (= 1 @render-count))
      (is (= {:cache-count 1
              :inflight-count 0}
             (fragment/stats manager)))

      (source/close! src)
      (is (eventually #(sse/closed? started-sse)))
      (finally
        (sse/cancel! started-sse)
        (close-source src)))))

(deftest fragment-singleflight-collapses-concurrent-live-refresh-renders-test
  (let [manager (fragment/create {:ttl-ms 1000})
        key (request-fragment-key "tx-1")
        started (promise)
        release (promise)
        cancelled (promise)
        render-count (atom 0)
        task (controlled-fragment-task
              started
              release
              "<section>request</section>"
              render-count
              cancelled)
        runner-1 (start-task (fragment/protect-task manager key task))
        _ (is (= :started (deref started timeout-ms ::timeout)))
        runners (doall
                 (cons runner-1
                       (for [_ (range 9)]
                         (start-task
                          (fragment/protect-task manager key task)))))]
    (is (= 1 @render-count))
    (is (= 1 (:inflight-count (fragment/stats manager))))

    (deliver release :go)

    (doseq [runner runners]
      (is (= {:status :success
              :value "<section>request</section>"}
             (task-result runner))))

    (is (= 1 @render-count))
    (is (eventually #(= 0 (:inflight-count (fragment/stats manager)))))
    (is (= 1 (:cache-count (fragment/stats manager))))))

(deftest fragment-short-ttl-cache-collapses-staggered-refreshes-test
  (let [clock (atom 0)
        manager (fragment/create {:ttl-ms 100
                                  :clock #(deref clock)})
        key (request-fragment-key "tx-1")
        render-count (atom 0)
        render-fn (fn []
                    (str "<section>render-" (swap! render-count inc) "</section>"))]
    ;; First refresh renders.
    (is (= {:status :success
            :value "<section>render-1</section>"}
           (run-task
            (fragment/render-task manager key render-fn))))

    ;; Staggered refreshes inside TTL reuse cached HTML.
    (swap! clock + 10)
    (is (= {:status :success
            :value "<section>render-1</section>"}
           (run-task
            (fragment/render-task manager key render-fn))))

    (swap! clock + 20)
    (is (= {:status :success
            :value "<section>render-1</section>"}
           (run-task
            (fragment/render-task manager key render-fn))))

    (is (= 1 @render-count))

    ;; After TTL, render runs again.
    (swap! clock + 100)
    (is (= {:status :success
            :value "<section>render-2</section>"}
           (run-task
            (fragment/render-task manager key render-fn))))

    (is (= 2 @render-count))))

(deftest fragment-consistency-token-partitions-cache-keys-test
  (let [manager (fragment/create {:ttl-ms 1000})
        key-tx-1 (request-fragment-key "tx-1")
        key-tx-2 (request-fragment-key "tx-2")
        render-count (atom 0)
        render-fn (fn []
                    (str "<section>render-" (swap! render-count inc) "</section>"))]
    (is (not= key-tx-1 key-tx-2))

    ;; First consistency point renders and caches.
    (is (= {:status :success
            :value "<section>render-1</section>"}
           (run-task
            (fragment/render-task manager key-tx-1 render-fn))))

    ;; Same consistency point reuses cache.
    (is (= {:status :success
            :value "<section>render-1</section>"}
           (run-task
            (fragment/render-task manager key-tx-1 render-fn))))

    ;; New consistency point does not reuse old token's cached HTML.
    (is (= {:status :success
            :value "<section>render-2</section>"}
           (run-task
            (fragment/render-task manager key-tx-2 render-fn))))

    ;; Same new consistency point now reuses its own cache.
    (is (= {:status :success
            :value "<section>render-2</section>"}
           (run-task
            (fragment/render-task manager key-tx-2 render-fn))))

    (is (= 2 @render-count))
    (is (= 2 (:cache-count (fragment/stats manager))))))

(deftest dispatch-live-wakeup-plus-fragment-render-protection-test
  (let [src (source/create)
        live-flow (flow/flow-for-source src {:subscription store-sub
                                             :relieve? false})
        started-sse (sse/start! live-flow {:encoder (constantly "store-wake")})
        manager (fragment/create {:ttl-ms 1000})
        key (fragment/strict-fragment-key
             {:fragment :store-queue
              :scope [:store "store-1"]
              :user-key [:manager "mgr-1"]
              :params {:page 1}
              :consistency-token "tx-1"})
        render-count (atom 0)
        rules (request-rules)
        done (promise)
        dispatcher (dispatch/create {:threads 1
                                     :queue-size 8
                                     :on-overflow :throw
                                     :on-error (fn [error entry]
                                                 (deliver done {:error error
                                                                :entry entry}))})]
    (try
      ;; App write path emits derived store invalidation through dispatcher.
      (dispatch/submit!
       dispatcher
       {:run (fn []
               (emit-expanded! src rules request-change)
               (deliver done :ok))})

      (is (= :ok (deref done timeout-ms ::timeout)))

      ;; SSE wakes the matching store subscriber.
      (is (= "event: live-update\ndata: store-wake\n\n"
             (take-value (:stream started-sse))))

      ;; The resulting fragment endpoint render is cached/protected.
      (is (= {:status :success
              :value "<section>store queue</section>"}
             (run-task
              (fragment/render-task
               manager
               key
               (fn []
                 (swap! render-count inc)
                 "<section>store queue</section>")))))

      (is (= {:status :success
              :value "<section>store queue</section>"}
             (run-task
              (fragment/render-task
               manager
               key
               (fn []
                 (swap! render-count inc)
                 "<section>store queue</section>")))))

      (is (= 1 @render-count))

      (source/close! src)
      (is (eventually #(sse/closed? started-sse)))
      (finally
        (sse/cancel! started-sse)
        (close-dispatcher dispatcher)
        (close-source src)))))

;; -----------------------------------------------------------------------------
;; core.clj vertical pipeline
;; -----------------------------------------------------------------------------

(deftest core-vertical-submit-expanded-to-sse-frame-test
  (let [system (live/create {:rules (request-rules)
                             :dispatch-options {:threads 1
                                                :queue-size 8
                                                :on-overflow :throw}})
        started (live/start-sse!
                 system
                 store-sub
                 {:flow-options {:relieve? false}
                  :sse-options {:encoder (constantly "store-wake")}})]
    (try
      (live/submit-expanded! system ctx request-change)

      (is (= "event: live-update\ndata: store-wake\n\n"
             (take-value (:stream started))))

      (source/close! (:source system))
      (is (eventually #(sse/closed? started)))
      (finally
        (live/cancel-sse! started)
        (live/close! system)))))

(deftest core-vertical-submit-expanded-to-sse-and-fragment-render-test
  (let [system (live/create {:rules (request-rules)
                             :dispatch-options {:threads 1
                                                :queue-size 8
                                                :on-overflow :throw}
                             :fragment-options {:ttl-ms 1000}})
        started (live/start-sse!
                 system
                 store-sub
                 {:flow-options {:relieve? false}
                  :sse-options {:encoder (constantly "store-wake")}})
        key (live/strict-fragment-key
             {:fragment :store-queue
              :scope [:store "store-1"]
              :user-key [:manager "mgr-1"]
              :params {:page 1}
              :consistency-token "tx-1"})
        render-count (atom 0)]
    (try
      ;; App write submits the primary change.
      (live/submit-expanded! system ctx request-change)

      ;; Derived store invalidation wakes the matching subscriber over SSE.
      (is (= "event: live-update\ndata: store-wake\n\n"
             (take-value (:stream started))))

      ;; The fragment endpoint render is protected by the system fragment manager.
      (is (= {:status :success
              :value "<section>store queue</section>"}
             (run-task
              (live/render-task
               system
               key
               (fn []
                 (swap! render-count inc)
                 "<section>store queue</section>")))))

      ;; Same render key inside TTL reuses cached output.
      (is (= {:status :success
              :value "<section>store queue</section>"}
             (run-task
              (live/render-task
               system
               key
               (fn []
                 (swap! render-count inc)
                 "<section>store queue</section>")))))

      (is (= 1 @render-count))

      (source/close! (:source system))
      (is (eventually #(sse/closed? started)))
      (finally
        (live/cancel-sse! started)
        (live/close! system)))))

(deftest core-vertical-multiple-sse-subscribers-get-derived-events-test
  (let [system (live/create {:rules (request-rules)
                             :dispatch-options {:threads 1
                                                :queue-size 8
                                                :on-overflow :throw}})
        request-started (live/start-sse!
                         system
                         request-sub
                         {:flow-options {:relieve? false}
                          :sse-options {:encoder (constantly "request")}})
        store-started (live/start-sse!
                       system
                       store-sub
                       {:flow-options {:relieve? false}
                        :sse-options {:encoder (constantly "store")}})
        manager-started (live/start-sse!
                         system
                         manager-sub
                         {:flow-options {:relieve? false}
                          :sse-options {:encoder (constantly "manager")}})]
    (try
      (live/submit-expanded! system ctx request-change)

      (is (= "event: live-update\ndata: request\n\n"
             (take-value (:stream request-started))))
      (is (= "event: live-update\ndata: store\n\n"
             (take-value (:stream store-started))))
      (is (= "event: live-update\ndata: manager\n\n"
             (take-value (:stream manager-started))))

      (source/close! (:source system))
      (is (eventually #(sse/closed? request-started)))
      (is (eventually #(sse/closed? store-started)))
      (is (eventually #(sse/closed? manager-started)))
      (finally
        (live/cancel-sse! request-started)
        (live/cancel-sse! store-started)
        (live/cancel-sse! manager-started)
        (live/close! system)))))

(deftest core-vertical-sse-cancel-closes-upstream-tap-test
  (let [system (live/create {:rules (request-rules)})
        started (live/start-sse! system store-sub)]
    (try
      (is (= 1 (:tap-count (source/stats (:source system)))))

      (live/cancel-sse! started)

      (is (eventually #(sse/closed? started)))
      (is (eventually #(= 0 (:tap-count (source/stats (:source system))))))
      (finally
        (live/cancel-sse! started)
        (live/close! system)))))

(deftest core-vertical-fragment-consistency-token-partitions-cache-test
  (let [system (live/create {:fragment-options {:ttl-ms 1000}})
        key-tx-1 (live/strict-fragment-key
                  {:fragment :request-panel
                   :scope [:request "req-1"]
                   :user-key [:user "u-1"]
                   :params {:tab :summary}
                   :consistency-token "tx-1"})
        key-tx-2 (live/strict-fragment-key
                  {:fragment :request-panel
                   :scope [:request "req-1"]
                   :user-key [:user "u-1"]
                   :params {:tab :summary}
                   :consistency-token "tx-2"})
        render-count (atom 0)
        render-fn (fn []
                    (str "<section>render-" (swap! render-count inc) "</section>"))]
    (try
      (is (= "<section>render-1</section>"
             (:value (run-task
                      (live/render-task system key-tx-1 render-fn)))))
      (is (= "<section>render-1</section>"
             (:value (run-task
                      (live/render-task system key-tx-1 render-fn)))))

      (is (= "<section>render-2</section>"
             (:value (run-task
                      (live/render-task system key-tx-2 render-fn)))))
      (is (= "<section>render-2</section>"
             (:value (run-task
                      (live/render-task system key-tx-2 render-fn)))))

      (is (= 2 @render-count))
      (is (= 2 (get-in (live/stats system) [:fragment :cache-count])))
      (finally
        (live/close! system)))))
;; -----------------------------------------------------------------------------
;; XTDB2 consistency propagation
;; -----------------------------------------------------------------------------

(deftest xtdb-consistent-read-prefers-node-over-stale-request-connection-test
  (let [node (fake-xtdb-datasource "await-from-node")
        ctx {:biff/conn :stale-request-conn
             :biff/node node}
        seen (atom nil)]
    (with-redefs [xt/q (fn [& args]
                         (reset! seen args)
                         [{:status :fresh}])]
      (is (= [{:status :fresh}]
             (xtdb-live/q-consistent-from
              ctx
              ["SELECT * FROM requests WHERE _id = ?" "req-1"])))

      ;; This is the critical stale-ctx regression check:
      ;; q-consistent-from should read via the node/DataSource, not :biff/conn.
      (is (= [node
              ["SELECT * FROM requests WHERE _id = ?" "req-1"]
              {:await-token "await-from-node"}]
             @seen)))))

(deftest xtdb-context-consistency-partitions-fragment-cache-test
  (let [node (fake-xtdb-datasource "await-1")
        ctx {:biff/conn :stale-request-conn
             :biff/node node}
        system (live/create {:fragment-options {:ttl-ms 1000}})
        render-count (atom 0)
        render-fn (fn []
                    (str "<section>render-" (swap! render-count inc) "</section>"))]
    (try
      (let [key-1 (request-panel-key-from-ctx ctx)]
        (is (= "<section>render-1</section>"
               (:value
                (run-task
                 (live/render-task system key-1 render-fn)))))

        ;; Same await token produces same fragment key and reuses cache.
        (is (= key-1 (request-panel-key-from-ctx ctx)))
        (is (= "<section>render-1</section>"
               (:value
                (run-task
                 (live/render-task system key-1 render-fn))))))

      ;; New XTDB2 await token produces a different fragment cache key.
      (.setAwaitToken ^DataSource node "await-2")

      (let [key-2 (request-panel-key-from-ctx ctx)]
        (is (not= (request-panel-key-from-ctx
                   {:biff/node (fake-xtdb-datasource "await-1")})
                  key-2))

        (is (= "<section>render-2</section>"
               (:value
                (run-task
                 (live/render-task system key-2 render-fn)))))

        (is (= "<section>render-2</section>"
               (:value
                (run-task
                 (live/render-task system key-2 render-fn))))))

      (is (= 2 @render-count))
      (is (= 2 (get-in (live/stats system) [:fragment :cache-count])))
      (finally
        (live/close! system)))))

(deftest xtdb-write-consistency-feeds-live-wakeup-and-fragment-key-test
  (let [node (fake-xtdb-datasource)
        ctx {:biff/conn :stale-request-conn
             :biff/node node}
        system (live/create {:rules (request-rules)
                             :dispatch-options {:threads 1
                                                :queue-size 8
                                                :on-overflow :throw}
                             :fragment-options {:ttl-ms 1000}})
        started (live/start-sse!
                 system
                 request-sub
                 {:flow-options {:relieve? false}
                  :sse-options {:encoder (constantly "wake")}})
        seen-tx (atom nil)
        render-count (atom 0)]
    (try
      (with-redefs [xt/execute-tx
                    (fn [connectable tx-ops opts]
                      ;; execute-tx-from! should write through connectable-from,
                      ;; which may be the request connection, but capture
                      ;; consistency from the await-token source.
                      (reset! seen-tx [connectable tx-ops opts])
                      (.setAwaitToken ^DataSource node "await-after-write")
                      {:tx-id 42
                       :system-time :system-time-42})]
        (let [{:keys [consistency] :as tx-result}
              (xtdb-live/execute-tx-from! ctx sample-xtdb-tx)

              key
              (request-panel-key-from-consistency consistency)]

          (is (= [:stale-request-conn sample-xtdb-tx {}]
                 @seen-tx))

          (is (= {:tx-result {:tx-id 42
                              :system-time :system-time-42}
                  :consistency {:tx-id 42
                                :system-time :system-time-42
                                :await-token "await-after-write"}}
                 tx-result))

          ;; The app still explicitly emits the primary change.
          ;; XTDB helper does not publish anything by itself.
          (live/submit-expanded! system ctx request-change)

          ;; The live pipeline wakes the browser/client.
          (is (= "event: live-update\ndata: wake\n\n"
                 (take-value (:stream started))))

          ;; The fragment render key includes the XTDB2 consistency dimension,
          ;; so the post-write render does not share stale cached HTML.
          (is (= "<section>request 1</section>"
                 (:value
                  (run-task
                   (live/render-task
                    system
                    key
                    (fn []
                      (str "<section>request "
                           (swap! render-count inc)
                           "</section>")))))))

          (is (= "<section>request 1</section>"
                 (:value
                  (run-task
                   (live/render-task
                    system
                    key
                    (fn []
                      (str "<section>request "
                           (swap! render-count inc)
                           "</section>")))))))

          (is (= 1 @render-count))))

      (source/close! (:source system))
      (is (eventually #(sse/closed? started)))
      (finally
        (live/cancel-sse! started)
        (live/close! system)))))

(deftest xtdb-consistency-token-can-be-attached-to-live-event-when-caller-supplies-it-test
  (let [system (live/create)
        seen-payload (atom nil)
        consistency {:await-token "await-from-write"
                     :tx-id 42}
        consistency-token (xtdb-live/consistency-fragment-dimension consistency)
        started (live/start-sse!
                 system
                 request-sub
                 {:flow-options {:relieve? false
                                 :consistency-token consistency-token}
                  :sse-options {:encoder (fn [payload]
                                           (reset! seen-payload payload)
                                           "encoded")}})]
    (try
      (live/emit! system request-change)

      (is (= "event: live-update\ndata: encoded\n\n"
             (take-value (:stream started))))

      (is (= {:invalidation request-change
              :consistency-token [:xtdb2/read-consistency
                                  {:await-token "await-from-write"
                                   :tx-id 42}]}
             @seen-payload))

      (source/close! (:source system))
      (is (eventually #(sse/closed? started)))
      (finally
        (live/cancel-sse! started)
        (live/close! system)))))
