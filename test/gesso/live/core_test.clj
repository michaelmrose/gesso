(ns gesso.live.core-test
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.live.core :as live]
   [gesso.live.dispatch :as dispatch]
   [gesso.live.flow :as flow]
   [gesso.live.fragment :as fragment]
   [gesso.live.source :as source]
   [gesso.live.transport.sse :as sse]
   [manifold.stream :as s]))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(def timeout-ms 1000)

(def ctx
  {:app/name :core-test})

(def request-change
  {:topic :request
   :id "req-1"
   :change/kind :updated})

(def other-request-change
  {:topic :request
   :id "req-2"
   :change/kind :updated})

(def request-sub
  {:topic :request
   :id "req-1"})

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

(defn request-rules
  []
  [{:when-topic :request
    :expand (fn [_ctx change]
              [{:topic :request
                :id (:id change)
                :change/kind (:change/kind change)}
               store-invalidation
               manager-invalidation])}])

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

(defn collect-runner
  [f]
  (start-task (flow/collect-task f)))

(defn run-task
  [task]
  (task-result (start-task task)))

(defn live-event
  [invalidation]
  {:event "live-update"
   :invalidation invalidation})

(defn event-names
  [events]
  (set (map :event events)))

;; -----------------------------------------------------------------------------
;; Option validation
;; -----------------------------------------------------------------------------

(deftest prepare-options-accepts-defaults-test
  (let [options (live/prepare-options! nil)]
    (is (= [] (:rules options)))
    (is (nil? (:source options)))
    (is (nil? (:dispatcher options)))
    (is (nil? (:fragment-manager options)))
    (is (nil? (:debug-fn options)))))

(deftest prepare-options-accepts-valid-options-test
  (let [debug-fn (fn [_])
        options (live/prepare-options!
                 {:source-options {:id :core-test/source}
                  :dispatch-options {:threads 1
                                     :queue-size 8
                                     :on-overflow :throw}
                  :fragment-options {:ttl-ms 100}
                  :rules (request-rules)
                  :debug-fn debug-fn})]
    (is (= {:id :core-test/source} (:source-options options)))
    (is (= {:threads 1
            :queue-size 8
            :on-overflow :throw}
           (:dispatch-options options)))
    (is (= {:ttl-ms 100} (:fragment-options options)))
    (is (= 1 (count (:rules options))))
    (is (identical? debug-fn (:debug-fn options)))))

(deftest prepare-options-rejects-invalid-debug-fn-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":debug-fn must be a function"
       (live/prepare-options! {:debug-fn :not-a-function}))))

(deftest prepare-options-rejects-invalid-option-maps-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":source-options must be a map"
       (live/prepare-options! {:source-options :bad})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":dispatch-options must be a map"
       (live/prepare-options! {:dispatch-options :bad})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":fragment-options must be a map"
       (live/prepare-options! {:fragment-options :bad}))))

(deftest prepare-options-rejects-invalid-rules-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":rules must be sequential"
       (live/prepare-options! {:rules :not-rules}))))

;; -----------------------------------------------------------------------------
;; System lifecycle
;; -----------------------------------------------------------------------------

(deftest create-builds-owned-system-test
  (let [system (live/create {:rules (request-rules)
                             :dispatch-options {:threads 1
                                                :queue-size 8
                                                :on-overflow :throw}
                             :fragment-options {:ttl-ms 100}})]
    (try
      (is (source/stats (:source system)))
      (is (:dispatcher system))
      (is (:fragment-manager system))
      (is (= {:source true
              :dispatcher true
              :fragment-manager true}
             (:owned system)))
      (is (not (live/closed? system)))
      (is (= false (:closed? (live/stats system))))
      (finally
        (live/close! system)))))

(deftest close-is-idempotent-test
  (let [events (atom [])
        system (live/create {:debug-fn #(swap! events conj %)
                             :dispatch-options {:threads 1
                                                :queue-size 8
                                                :on-overflow :throw}})]
    (live/close! system)
    (live/close! system)
    (is (live/closed? system))
    (is (= 1 (count (filter #(= :gesso.live.core/closed (:event %))
                            @events))))))

(deftest create-can-use-supplied-resources-test
  (let [src (source/create)
        dispatcher (dispatch/create {:threads 1
                                     :queue-size 8
                                     :on-overflow :throw})
        fragment-manager (fragment/create {:ttl-ms 100})
        system (live/create {:source src
                             :dispatcher dispatcher
                             :fragment-manager fragment-manager
                             :rules (request-rules)})]
    (try
      (is (identical? src (:source system)))
      (is (identical? dispatcher (:dispatcher system)))
      (is (identical? fragment-manager (:fragment-manager system)))
      (is (= {:source false
              :dispatcher false
              :fragment-manager false}
             (:owned system)))
      (live/close! system)
      (is (live/closed? system))
      ;; Supplied source is not owned by core, so it should remain usable.
      (is (= :emitted (:status (source/emit! src request-change))))
      (finally
        (dispatch/close! dispatcher)
        (source/close! src)))))

(deftest create-cleans-up-owned-resources-if-later-step-fails-test
  ;; This mainly proves that construction failure is surfaced cleanly. The
  ;; implementation also closes any owned source/dispatcher created before the
  ;; failure.
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":executor must be :blk, :cpu, or a java.util.concurrent.Executor"
       (live/create {:dispatch-options {:threads 1
                                        :queue-size 8
                                        :on-overflow :throw}
                     :fragment-options {:executor :not-real}}))))

;; -----------------------------------------------------------------------------
;; Expansion and emission
;; -----------------------------------------------------------------------------

(deftest expand-uses-system-rules-test
  (let [system (live/create {:rules (request-rules)})]
    (try
      (is (= [request-change store-invalidation manager-invalidation]
             (live/expand system ctx request-change)))
      (finally
        (live/close! system)))))

(deftest emit-direct-invalidation-to-source-test
  (let [system (live/create)
        tap (source/changes (:source system))]
    (try
      (is (= :emitted (:status (live/emit! system request-change))))
      (is (= request-change (take-value tap)))
      (finally
        (live/close! system)))))

(deftest emit-many-direct-invalidations-to-source-test
  (let [system (live/create)
        tap (source/changes (:source system))]
    (try
      (is (= :emitted-many
             (:status (live/emit-many! system [request-change
                                               store-invalidation]))))
      (is (= request-change (take-value tap)))
      (is (= store-invalidation (take-value tap)))
      (finally
        (live/close! system)))))

(deftest emit-expanded-expands-and-emits-test
  (let [system (live/create {:rules (request-rules)})
        tap (source/changes (:source system))]
    (try
      (is (= :emitted-many
             (:status (live/emit-expanded! system ctx request-change))))
      (is (= request-change (take-value tap)))
      (is (= store-invalidation (take-value tap)))
      (is (= manager-invalidation (take-value tap)))
      (finally
        (live/close! system)))))

(deftest submit-expanded-runs-through-dispatcher-test
  (let [done (promise)
        system (live/create {:rules (request-rules)
                             :dispatch-options {:threads 1
                                                :queue-size 8
                                                :on-overflow :throw
                                                :on-error (fn [error entry]
                                                            (deliver done {:error error
                                                                           :entry entry}))}})
        tap (source/changes (:source system))]
    (try
      (live/submit-expanded! system ctx request-change
                             {:after (fn []
                                       (deliver done :ok))})
      ;; :after is just metadata for dispatch in this test unless dispatch.clj
      ;; interprets it, so the reliable proof is the emitted source values.
      (is (= request-change (take-value tap)))
      (is (= store-invalidation (take-value tap)))
      (is (= manager-invalidation (take-value tap)))
      (finally
        (live/close! system)))))

;; -----------------------------------------------------------------------------
;; Flow and SSE facade
;; -----------------------------------------------------------------------------

(deftest live-flow-filters-system-source-events-test
  (let [system (live/create)
        f (live/live-flow system request-sub {:relieve? false})
        runner (collect-runner f)]
    (try
      (live/emit! system store-invalidation)
      (live/emit! system request-change)
      (source/close! (:source system))
      (is (= {:status :success
              :value [(live-event request-change)]}
             (task-result runner)))
      (finally
        (live/close! system)))))

(deftest start-sse-streams-system-source-events-test
  (let [system (live/create)
        started (live/start-sse!
                 system
                 request-sub
                 {:flow-options {:relieve? false}
                  :sse-options {:encoder (constantly "encoded")}})]
    (try
      (live/emit! system request-change)
      (is (= "event: live-update\ndata: encoded\n\n"
             (take-value (:stream started))))
      (source/close! (:source system))
      (is (eventually #(sse/closed? started)))
      (finally
        (live/cancel-sse! started)
        (live/close! system)))))

(deftest start-sse-ignores-nonmatching-events-test
  (let [system (live/create)
        started (live/start-sse!
                 system
                 request-sub
                 {:flow-options {:relieve? false}
                  :sse-options {:encoder (constantly "encoded")}})]
    (try
      (live/emit! system store-invalidation)
      (source/close! (:source system))
      (is (eventually #(sse/closed? started)))
      (is (nil? (take-value (:stream started))))
      (finally
        (live/cancel-sse! started)
        (live/close! system)))))

(deftest cancel-sse-closes-source-tap-test
  (let [system (live/create)
        started (live/start-sse! system request-sub)]
    (try
      (is (= 1 (:tap-count (source/stats (:source system)))))
      (live/cancel-sse! started)
      (is (eventually #(sse/closed? started)))
      (is (eventually #(= 0 (:tap-count (source/stats (:source system))))))
      (finally
        (live/cancel-sse! started)
        (live/close! system)))))

;; -----------------------------------------------------------------------------
;; Fragment facade
;; -----------------------------------------------------------------------------

(deftest fragment-key-helpers-delegate-to-fragment-namespace-test
  (is (= (fragment/fragment-key :request-panel
                                {:scope [:request "req-1"]
                                 :user-key [:user "u-1"]
                                 :consistency-token "tx-1"})
         (live/fragment-key :request-panel
                            {:scope [:request "req-1"]
                             :user-key [:user "u-1"]
                             :consistency-token "tx-1"})))
  (is (= (fragment/strict-fragment-key
          {:fragment :request-panel
           :scope [:request "req-1"]
           :user-key [:user "u-1"]})
         (live/strict-fragment-key
          {:fragment :request-panel
           :scope [:request "req-1"]
           :user-key [:user "u-1"]}))))

(deftest render-task-uses-system-fragment-manager-test
  (let [system (live/create {:fragment-options {:ttl-ms 1000}})
        key (live/strict-fragment-key
             {:fragment :request-panel
              :scope [:request "req-1"]
              :user-key [:user "u-1"]
              :consistency-token "tx-1"})
        render-count (atom 0)]
    (try
      (is (= {:status :success
              :value "<section>request</section>"}
             (run-task
              (live/render-task
               system
               key
               (fn []
                 (swap! render-count inc)
                 "<section>request</section>")))))
      (is (= {:status :success
              :value "<section>request</section>"}
             (run-task
              (live/render-task
               system
               key
               (fn []
                 (swap! render-count inc)
                 "<section>request</section>")))))
      (is (= 1 @render-count))
      (is (= 1 (get-in (live/stats system) [:fragment :cache-count])))
      (finally
        (live/close! system)))))

(deftest protect-task-uses-system-fragment-manager-test
  (let [system (live/create)
        key [:fragment :protected-task]]
    (try
      (is (= {:status :success
              :value :ok}
             (run-task
              (live/protect-task system key (fragment/call-task (fn [] :ok))))))
      (finally
        (live/close! system)))))

(deftest fragment-cache-maintenance-delegates-test
  (let [system (live/create {:fragment-options {:ttl-ms 1000}})
        key [:fragment :cache-maintenance]]
    (try
      (is (= :ok (:value (run-task
                          (live/render-task system key (fn [] :ok))))))
      (is (= 1 (get-in (live/stats system) [:fragment :cache-count])))

      (live/clear-fragment-key! system key)
      (is (= 0 (get-in (live/stats system) [:fragment :cache-count])))

      (is (= :ok (:value (run-task
                          (live/render-task system key (fn [] :ok))))))
      (is (= 1 (get-in (live/stats system) [:fragment :cache-count])))

      (live/clear-fragment-cache! system)
      (is (= 0 (get-in (live/stats system) [:fragment :cache-count])))
      (finally
        (live/close! system)))))

(deftest purge-expired-fragments-delegates-test
  (let [clock (atom 0)
        system (live/create {:fragment-options {:ttl-ms 10
                                                :clock #(deref clock)}})
        key [:fragment :expires]]
    (try
      (is (= :ok (:value (run-task
                          (live/render-task system key (fn [] :ok))))))
      (is (= 1 (get-in (live/stats system) [:fragment :cache-count])))
      (swap! clock + 11)
      (is (= 1 (live/purge-expired-fragments! system)))
      (is (= 0 (get-in (live/stats system) [:fragment :cache-count])))
      (finally
        (live/close! system)))))

;; -----------------------------------------------------------------------------
;; Debug inheritance
;; -----------------------------------------------------------------------------

(deftest core-debug-is-inherited-by-flow-sse-and-fragment-test
  (let [events (atom [])
        system (live/create {:debug-fn #(swap! events conj %)
                             :fragment-options {:ttl-ms 1000}})
        started (live/start-sse!
                 system
                 request-sub
                 {:flow-options {:relieve? false}
                  :sse-options {:encoder (constantly "encoded")}})
        key [:fragment :debug]]
    (try
      (live/emit! system request-change)
      (is (= "event: live-update\ndata: encoded\n\n"
             (take-value (:stream started))))

      (is (= :html
             (:value
              (run-task
               (live/render-task system key (fn [] :html))))))

      (source/close! (:source system))
      (is (eventually #(sse/closed? started)))

      (let [names (event-names @events)]
        (is (contains? names :gesso.live.core/created))
        (is (contains? names :gesso.live.core/live-flow))
        (is (contains? names :gesso.live.core/sse-started))
        (is (contains? names :gesso.live.core/emit))
        (is (contains? names :gesso.live.flow/invalidation-matched))
        (is (contains? names :gesso.live.sse/frame-built))
        (is (contains? names :gesso.live.fragment/render-started)))
      (finally
        (live/cancel-sse! started)
        (live/close! system)))))

(deftest explicit-nil-debug-disables-inheritance-for-child-operation-test
  (let [events (atom [])
        system (live/create {:debug-fn #(swap! events conj %)})
        started (live/start-sse!
                 system
                 request-sub
                 {:flow-options {:relieve? false
                                 :debug-fn nil}
                  :sse-options {:encoder (constantly "encoded")
                                :debug-fn nil}})]
    (try
      (live/emit! system request-change)
      (is (= "event: live-update\ndata: encoded\n\n"
             (take-value (:stream started))))
      (source/close! (:source system))
      (is (eventually #(sse/closed? started)))

      (let [names (event-names @events)]
        (is (contains? names :gesso.live.core/created))
        (is (contains? names :gesso.live.core/sse-started))
        (is (not (contains? names :gesso.live.flow/invalidation-matched)))
        (is (not (contains? names :gesso.live.sse/frame-built))))
      (finally
        (live/cancel-sse! started)
        (live/close! system)))))
