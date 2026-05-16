(ns gesso.live.flow-test
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.live.flow :as flow]
   [gesso.live.source :as source]
   [manifold.deferred :as d]
   [manifold.stream :as s]
   [missionary.core :as m]))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(def timeout-ms 1000)

(def request-sub
  {:topic :request
   :id "req-1"})

(def other-request-sub
  {:topic :request
   :id "req-2"})

(def store-sub
  {:topic :store-queue
   :id "store-1"})

(def request-invalidation
  {:topic :request
   :id "req-1"
   :change/kind :updated})

(def other-request-invalidation
  {:topic :request
   :id "req-2"
   :change/kind :updated})

(def store-invalidation
  {:topic :store-queue
   :id "store-1"
   :change/kind :updated})

(def invalid-invalidation
  {:topic :request})

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

(defn put-value
  [stream value]
  (deref (s/put! stream value) timeout-ms ::timeout))

(defn sleep-briefly
  []
  (Thread/sleep 25))

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

(defn event-count
  [events event-name]
  (count (filter #(= event-name (:event %)) events)))

(defn first-event
  [events event-name]
  (first (filter #(= event-name (:event %)) events)))

(defn complete!
  [deferred value]
  (d/success! deferred value))

(defn fail!
  [deferred error]
  (d/error! deferred error))

;; -----------------------------------------------------------------------------
;; Pure helper behavior
;; -----------------------------------------------------------------------------

(deftest default-interested-matches-topic-and-id-test
  (is (flow/default-interested? request-sub request-invalidation))
  (is (not (flow/default-interested? request-sub other-request-invalidation)))
  (is (not (flow/default-interested? request-sub store-invalidation)))
  (is (not (flow/default-interested? other-request-sub request-invalidation))))

(deftest invalidation-key-selects-topic-and-id-test
  (is (= {:topic :request
          :id "req-1"}
         (flow/invalidation-key request-invalidation))))

(deftest invalidation-event-builds-default-live-event-test
  (is (= {:event "live-update"
          :invalidation request-invalidation}
         (flow/invalidation-event request-invalidation))))

(deftest invalidation-event-supports-custom-event-test
  (let [event (flow/invalidation-event request-invalidation
                                       {:event :client-oob})]
    (is (= "client-oob" (:event event)))
    (is (= request-invalidation (:invalidation event)))))

(deftest invalidation-event-supports-static-data-test
  (let [event (flow/invalidation-event request-invalidation
                                       {:data {:source :test}})]
    (is (= {:source :test} (:data event)))))

(deftest invalidation-event-supports-data-function-test
  (let [event (flow/invalidation-event
               request-invalidation
               {:data (fn [invalidation]
                        {:key (flow/invalidation-key invalidation)})})]
    (is (= {:key {:topic :request :id "req-1"}}
           (:data event)))))

(deftest invalidation-event-supports-consistency-token-test
  (let [event (flow/invalidation-event request-invalidation
                                       {:consistency-token "tx-1"})]
    (is (= "tx-1" (:consistency-token event)))))

;; -----------------------------------------------------------------------------
;; Option preparation
;; -----------------------------------------------------------------------------

(deftest prepare-options-accepts-minimal-valid-options-test
  (let [options (flow/prepare-options! {:subscription request-sub})]
    (is (= request-sub (:subscription options)))
    (is (= flow/default-event (:event options)))
    (is (= true (:relieve? options)))
    (is (fn? (:interested? options)))))

(deftest prepare-options-accepts-full-valid-options-test
  (let [debug-fn (fn [_])
        on-close (fn [])
        interested? (fn [_ _] true)
        options (flow/prepare-options!
                 {:subscription request-sub
                  :interested? interested?
                  :event :client-oob
                  :data {:source :test}
                  :relieve? false
                  :debug-fn debug-fn
                  :on-close on-close})]
    (is (= request-sub (:subscription options)))
    (is (identical? interested? (:interested? options)))
    (is (= :client-oob (:event options)))
    (is (= {:source :test} (:data options)))
    (is (= false (:relieve? options)))
    (is (identical? debug-fn (:debug-fn options)))
    (is (identical? on-close (:on-close options)))))

(deftest prepare-options-rejects-invalid-subscription-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Invalid gesso.live value"
       (flow/prepare-options! {:subscription {:topic :request}}))))

(deftest prepare-options-rejects-invalid-interested-predicate-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Invalid gesso.live value"
       (flow/prepare-options! {:subscription request-sub
                               :interested? :not-a-function}))))

(deftest prepare-options-rejects-invalid-debug-fn-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":debug-fn must be a function"
       (flow/prepare-options! {:subscription request-sub
                               :debug-fn :not-a-function}))))

(deftest prepare-options-rejects-invalid-on-close-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":on-close must be a function"
       (flow/prepare-options! {:subscription request-sub
                               :on-close :not-a-function}))))

(deftest prepare-options-rejects-invalid-relieve-flag-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":relieve\? must be a boolean"
       (flow/prepare-options! {:subscription request-sub
                               :relieve? :sometimes}))))

;; -----------------------------------------------------------------------------
;; stream-flow lifecycle
;; -----------------------------------------------------------------------------

(deftest stream-flow-emits-stream-values-test
  (let [stream (s/stream)
        f (flow/stream-flow stream)
        runner (collect-runner f)]
    (is (= true (put-value stream :a)))
    (is (= true (put-value stream :b)))
    (s/close! stream)
    (is (= {:status :success
            :value [:a :b]}
           (task-result runner)))))

(deftest stream-flow-completes-empty-when-stream-closes-without-values-test
  (let [stream (s/stream)
        f (flow/stream-flow stream)
        runner (collect-runner f)]
    (s/close! stream)
    (is (= {:status :success
            :value []}
           (task-result runner)))))

(deftest stream-flow-calls-on-close-on-normal-stream-close-test
  (let [stream (s/stream)
        close-count (atom 0)
        f (flow/stream-flow stream {:on-close #(swap! close-count inc)})
        runner (collect-runner f)]
    (is (= true (put-value stream :a)))
    (s/close! stream)
    (is (= {:status :success
            :value [:a]}
           (task-result runner)))
    (is (eventually #(= 1 @close-count)))))

(deftest stream-flow-closes-stream-on-early-downstream-termination-test
  (let [stream (s/stream)
        close-count (atom 0)
        f (m/eduction
           (take 1)
           (flow/stream-flow stream {:on-close #(swap! close-count inc)}))
        runner (collect-runner f)]
    (is (= true (put-value stream :a)))
    (is (= {:status :success
            :value [:a]}
           (task-result runner)))
    (is (eventually #(s/closed? stream)))
    (is (eventually #(= 1 @close-count)))))

(deftest stream-flow-cancel-closes-stream-and-calls-on-close-test
  (let [stream (s/stream)
        close-count (atom 0)
        f (flow/stream-flow stream {:on-close #(swap! close-count inc)})
        runner (collect-runner f)]
    (cancel-runner! runner)
    (is (eventually #(s/closed? stream)))
    (is (eventually #(= 1 @close-count)))))

(deftest stream-flow-emits-debug-events-when-enabled-test
  (let [events (atom [])
        stream (s/stream)
        f (flow/stream-flow stream {:debug-fn #(swap! events conj %)})
        runner (collect-runner f)]
    (is (= true (put-value stream :a)))
    (s/close! stream)
    (is (= {:status :success
            :value [:a]}
           (task-result runner)))
    (is (contains? (event-names @events)
                   :gesso.live.flow/stream-value))
    (is (contains? (event-names @events)
                   :gesso.live.flow/stream-closed))
    (is (eventually
         #(contains? (event-names @events)
                     :gesso.live.flow/stream-flow-closed)))))

;; -----------------------------------------------------------------------------
;; stream-flow bridge assumptions for the m/ap implementation
;; -----------------------------------------------------------------------------

(deftest stream-flow-does-not-use-s-consume-test
  (with-redefs [s/consume (fn [& _]
                            (throw
                             (ex-info "s/consume must not be used by stream-flow."
                                      {})))]
    (let [stream (s/stream)
          f (flow/stream-flow stream)
          runner (collect-runner f)]
      (is (= true (put-value stream :a)))
      (s/close! stream)
      (is (= {:status :success
              :value [:a]}
             (task-result runner))))))

(deftest stream-flow-debug-return-values-are-not-emitted-test
  (let [events (atom [])
        stream (s/stream)
        f (flow/stream-flow
           stream
           {:debug-fn (fn [event]
                        (swap! events conj event)
                        {:debug-return (:event event)})})
        runner (collect-runner f)]
    (is (= true (put-value stream :a)))
    (s/close! stream)
    (let [result (task-result runner)]
      (is (= :success (:status result)))
      (is (= [:a] (:value result)))
      (is (not-any? nil? (:value result)))
      (is (not-any? #(and (map? %)
                          (contains? % :debug-return))
                    (:value result))))
    (is (contains? (event-names @events)
                   :gesso.live.flow/stream-value))))


(deftest stream-flow-installs-one-take-at-a-time-test
  (let [stream (s/stream)
        take-count (atom 0)
        takes (atom [])]
    (with-redefs [s/take! (fn [_stream _closed-sentinel]
                            (swap! take-count inc)
                            (let [p (d/deferred)]
                              (swap! takes conj p)
                              p))]
      (let [f (flow/stream-flow stream)
            runner (collect-runner f)]
        (when-not (eventually #(= 1 @take-count))
          (cancel-runner! runner))
        (is (= 1 @take-count)
            "stream-flow should install its first s/take! when collection starts.")

        (when (= 1 @take-count)
          (sleep-briefly)
          (is (= 1 @take-count)
              "stream-flow should not install a second take before the first take has completed.")

          (complete! (nth @takes 0) :a)
          (is (eventually #(= 2 @take-count)))

          (complete! (nth @takes 1) :b)
          (is (eventually #(= 3 @take-count)))

          (complete! (nth @takes 2) flow/closed-sentinel)

          (is (= {:status :success
                  :value [:a :b]}
                 (task-result runner))))))))

(deftest stream-flow-early-downstream-termination-does-not-install-next-take-test
  (let [stream (s/stream)
        close-count (atom 0)
        take-count (atom 0)
        first-take (d/deferred)]
    (with-redefs [s/take! (fn [_stream _closed-sentinel]
                            (swap! take-count inc)
                            first-take)]
      (let [f (m/eduction
               (take 1)
               (flow/stream-flow
                stream
                {:on-close #(swap! close-count inc)}))
            runner (collect-runner f)]
        (is (eventually #(= 1 @take-count)))

        (complete! first-take :a)

        (is (= {:status :success
                :value [:a]}
               (task-result runner)))
        (is (eventually #(s/closed? stream)))
        (is (eventually #(= 1 @close-count)))
        (sleep-briefly)
        (is (= 1 @take-count)
            "After downstream take 1 terminates the flow, stream-flow must not install another s/take!.")))))

(deftest stream-flow-cancel-while-take-pending-closes-stream-once-test
  (let [stream (s/stream)
        events (atom [])
        close-count (atom 0)
        take-count (atom 0)
        pending-take (d/deferred)]
    (with-redefs [s/take! (fn [_stream _closed-sentinel]
                            (swap! take-count inc)
                            pending-take)]
      (let [f (flow/stream-flow
               stream
               {:debug-fn #(swap! events conj %)
                :on-close #(swap! close-count inc)})
            runner (collect-runner f)]
        (is (eventually #(= 1 @take-count)))

        (cancel-runner! runner)

        (is (eventually #(s/closed? stream)))
        (is (eventually #(= 1 @close-count)))
        (is (eventually
             #(contains? (event-names @events)
                         :gesso.live.flow/stream-flow-closed)))

        (complete! pending-take :late)
        (sleep-briefly)
        (is (not (contains? (event-names @events)
                            :gesso.live.flow/stream-value)))))))

(deftest stream-flow-stream-error-is-wrapped-debugged-and-closes-once-test
  (let [stream (s/stream)
        events (atom [])
        close-count (atom 0)
        take-count (atom 0)
        pending-take (d/deferred)]
    (with-redefs [s/take! (fn [_stream _closed-sentinel]
                            (swap! take-count inc)
                            pending-take)]
      (let [f (flow/stream-flow
               stream
               {:debug-fn #(swap! events conj %)
                :on-close #(swap! close-count inc)})
            runner (collect-runner f)
            cause (RuntimeException. "stream boom")]
        (is (eventually #(= 1 @take-count)))

        (fail! pending-take cause)

        (let [result (task-result runner)
              error (:error result)]
          (is (= :failure (:status result)))
          (is (= "gesso.live stream failed."
                 (ex-message error)))
          (is (identical? cause (ex-cause error))))

        (is (eventually #(s/closed? stream)))
        (is (eventually #(= 1 @close-count)))
        (is (contains? (event-names @events)
                       :gesso.live.flow/stream-error))
        (is (contains? (event-names @events)
                       :gesso.live.flow/stream-flow-closed))
        (is (identical? cause
                        (:error (first-event @events
                                             :gesso.live.flow/stream-error))))))))

;; -----------------------------------------------------------------------------
;; flow-for-stream behavior
;; -----------------------------------------------------------------------------

(deftest flow-for-stream-emits-matching-live-events-test
  (let [stream (s/stream)
        f (flow/flow-for-stream stream {:subscription request-sub
                                        :relieve? false})
        runner (collect-runner f)]
    (is (= true (put-value stream other-request-invalidation)))
    (is (= true (put-value stream request-invalidation)))
    (s/close! stream)
    (is (= {:status :success
            :value [{:event "live-update"
                     :invalidation request-invalidation}]}
           (task-result runner)))))

(deftest flow-for-stream-completes-empty-when-no-invalidations-match-test
  (let [stream (s/stream)
        f (flow/flow-for-stream stream {:subscription request-sub
                                        :relieve? false})
        runner (collect-runner f)]
    (is (= true (put-value stream store-invalidation)))
    (is (= true (put-value stream other-request-invalidation)))
    (s/close! stream)
    (is (= {:status :success
            :value []}
           (task-result runner)))))

(deftest flow-for-stream-supports-custom-interested-predicate-test
  (let [stream (s/stream)
        f (flow/flow-for-stream
           stream
           {:subscription store-sub
            :relieve? false
            :interested? (fn [_sub invalidation]
                           (= :store-queue (:topic invalidation)))})
        runner (collect-runner f)]
    (is (= true (put-value stream request-invalidation)))
    (is (= true (put-value stream store-invalidation)))
    (s/close! stream)
    (is (= {:status :success
            :value [{:event "live-update"
                     :invalidation store-invalidation}]}
           (task-result runner)))))

(deftest flow-for-stream-supports-custom-event-options-test
  (let [stream (s/stream)
        f (flow/flow-for-stream
           stream
           {:subscription request-sub
            :relieve? false
            :event :client-oob
            :data {:source :test}
            :consistency-token "tx-1"})
        runner (collect-runner f)]
    (is (= true (put-value stream request-invalidation)))
    (s/close! stream)
    (is (= {:status :success
            :value [{:event "client-oob"
                     :invalidation request-invalidation
                     :data {:source :test}
                     :consistency-token "tx-1"}]}
           (task-result runner)))))

(deftest flow-for-stream-supports-data-function-test
  (let [stream (s/stream)
        f (flow/flow-for-stream
           stream
           {:subscription request-sub
            :relieve? false
            :data (fn [invalidation]
                    {:key (flow/invalidation-key invalidation)})})
        runner (collect-runner f)]
    (is (= true (put-value stream request-invalidation)))
    (s/close! stream)
    (is (= {:status :success
            :value [{:event "live-update"
                     :invalidation request-invalidation
                     :data {:key {:topic :request :id "req-1"}}}]}
           (task-result runner)))))

(deftest flow-for-stream-calls-on-close-test
  (let [stream (s/stream)
        close-count (atom 0)
        f (flow/flow-for-stream stream {:subscription request-sub
                                        :relieve? false
                                        :on-close #(swap! close-count inc)})
        runner (collect-runner f)]
    (is (= true (put-value stream request-invalidation)))
    (s/close! stream)
    (is (= :success (:status (task-result runner))))
    (is (eventually #(= 1 @close-count)))))

(deftest flow-for-stream-validates-options-before-consuming-stream-test
  (let [stream (s/stream)]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid gesso.live value"
         (flow/flow-for-stream stream {:subscription {:topic :request}})))
    (is (not (s/closed? stream)))))

;; -----------------------------------------------------------------------------
;; Debug behavior
;; -----------------------------------------------------------------------------

(deftest flow-for-stream-emits-debug-events-for-match-and-ignore-test
  (let [events (atom [])
        stream (s/stream)
        f (flow/flow-for-stream
           stream
           {:subscription request-sub
            :relieve? false
            :debug-fn #(swap! events conj %)})
        runner (collect-runner f)]
    (is (= true (put-value stream store-invalidation)))
    (is (= true (put-value stream request-invalidation)))
    (s/close! stream)

    (is (= :success (:status (task-result runner))))

    (let [names (event-names @events)]
      (is (contains? names :gesso.live.flow/flow-for-stream-created))
      (is (contains? names :gesso.live.flow/invalidation-received))
      (is (contains? names :gesso.live.flow/invalidation-ignored))
      (is (contains? names :gesso.live.flow/invalidation-matched))
      (is (contains? names :gesso.live.flow/stream-value)))

    (is (eventually
         #(contains? (event-names @events)
                     :gesso.live.flow/stream-flow-closed)))

    (is (= 2
           (event-count @events :gesso.live.flow/invalidation-received)))
    (is (= 1
           (event-count @events :gesso.live.flow/invalidation-ignored)))
    (is (= 1
           (event-count @events :gesso.live.flow/invalidation-matched)))))

;; -----------------------------------------------------------------------------
;; Failure behavior
;; -----------------------------------------------------------------------------

(deftest interested-predicate-failure-preserves-cause-test
  (let [stream (s/stream)
        f (flow/flow-for-stream
           stream
           {:subscription request-sub
            :relieve? false
            :interested? (fn [_sub _invalidation]
                           (throw (RuntimeException. "predicate boom")))})
        runner (collect-runner f)]
    (is (= true (put-value stream request-invalidation)))
    (s/close! stream)
    (let [result (task-result runner)
          error (:error result)]
      (is (= :failure (:status result)))
      (is (= "gesso.live flow interested? predicate failed."
             (ex-message error)))
      (is (= "predicate boom"
             (ex-message (ex-cause error)))))))

(deftest interested-predicate-failure-emits-debug-event-test
  (let [events (atom [])
        stream (s/stream)
        f (flow/flow-for-stream
           stream
           {:subscription request-sub
            :relieve? false
            :debug-fn #(swap! events conj %)
            :interested? (fn [_sub _invalidation]
                           (throw (RuntimeException. "predicate boom")))})
        runner (collect-runner f)]
    (is (= true (put-value stream request-invalidation)))
    (s/close! stream)
    (is (= :failure (:status (task-result runner))))
    (is (contains? (event-names @events)
                   :gesso.live.flow/interested-predicate-failed))))

(deftest event-build-failure-preserves-cause-test
  (let [stream (s/stream)
        f (flow/flow-for-stream
           stream
           {:subscription request-sub
            :relieve? false
            :data (fn [_invalidation]
                    (throw (RuntimeException. "data boom")))})
        runner (collect-runner f)]
    (is (= true (put-value stream request-invalidation)))
    (s/close! stream)
    (let [result (task-result runner)
          error (:error result)]
      (is (= :failure (:status result)))
      (is (= "gesso.live flow failed to build live event."
             (ex-message error)))
      (is (= "data boom"
             (ex-message (ex-cause error)))))))

(deftest event-build-failure-emits-debug-event-test
  (let [events (atom [])
        stream (s/stream)
        f (flow/flow-for-stream
           stream
           {:subscription request-sub
            :relieve? false
            :debug-fn #(swap! events conj %)
            :data (fn [_invalidation]
                    (throw (RuntimeException. "data boom")))})
        runner (collect-runner f)]
    (is (= true (put-value stream request-invalidation)))
    (s/close! stream)
    (is (= :failure (:status (task-result runner))))
    (is (contains? (event-names @events)
                   :gesso.live.flow/event-build-failed))))

(deftest invalid-live-event-failure-is-wrapped-test
  (let [stream (s/stream)
        f (flow/flow-for-stream
           stream
           {:subscription request-sub
            :relieve? false
            :interested? (fn [_sub _invalidation]
                           true)})
        runner (collect-runner f)]
    (is (= true (put-value stream invalid-invalidation)))
    (s/close! stream)
    (let [result (task-result runner)
          error (:error result)]
      (is (= :failure (:status result)))
      (is (= "gesso.live flow failed to build live event."
             (ex-message error)))
      (is (some? (ex-cause error))))))

;; -----------------------------------------------------------------------------
;; flow-for-source behavior
;; -----------------------------------------------------------------------------

(deftest flow-for-source-receives-source-emissions-test
  (let [src (source/create)
        f (flow/flow-for-source src {:subscription request-sub
                                     :relieve? false})
        runner (collect-runner f)]
    (try
      (source/emit! src request-invalidation)
      (source/close! src)
      (is (= {:status :success
              :value [{:event "live-update"
                       :invalidation request-invalidation}]}
             (task-result runner)))
      (finally
        (source/close! src)))))

(deftest flow-for-source-filters-nonmatching-source-emissions-test
  (let [src (source/create)
        f (flow/flow-for-source src {:subscription request-sub
                                     :relieve? false})
        runner (collect-runner f)]
    (try
      (source/emit! src store-invalidation)
      (source/emit! src request-invalidation)
      (source/close! src)
      (is (= {:status :success
              :value [{:event "live-update"
                       :invalidation request-invalidation}]}
             (task-result runner)))
      (finally
        (source/close! src)))))

(deftest flow-for-source-completes-empty-when-source-closes-test
  (let [src (source/create)
        f (flow/flow-for-source src {:subscription request-sub
                                     :relieve? false})
        runner (collect-runner f)]
    (try
      (source/close! src)
      (is (= {:status :success
              :value []}
             (task-result runner)))
      (finally
        (source/close! src)))))

(deftest flow-for-source-validates-before-opening-tap-test
  (let [src (source/create)]
    (try
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid gesso.live value"
           (flow/flow-for-source src {:subscription {:topic :request}})))
      (is (= 0 (:tap-count (source/stats src))))
      (finally
        (source/close! src)))))

(deftest flow-for-source-calls-existing-on-close-test
  (let [src (source/create)
        close-count (atom 0)
        f (flow/flow-for-source src {:subscription request-sub
                                     :relieve? false
                                     :on-close #(swap! close-count inc)})
        runner (collect-runner f)]
    (try
      (source/emit! src request-invalidation)
      (source/close! src)
      (is (= :success (:status (task-result runner))))
      (is (eventually #(= 1 @close-count)))
      (finally
        (source/close! src)))))

(deftest flow-for-source-closes-source-tap-after-early-termination-test
  (let [src (source/create)
        f (m/eduction
           (take 1)
           (flow/flow-for-source src {:subscription request-sub
                                      :relieve? false}))
        runner (collect-runner f)]
    (try
      (source/emit! src request-invalidation)
      (is (= {:status :success
              :value [{:event "live-update"
                       :invalidation request-invalidation}]}
             (task-result runner)))
      (is (eventually #(= 0 (:tap-count (source/stats src)))))
      (finally
        (source/close! src)))))

(deftest flow-for-source-emits-source-tap-debug-events-test
  (let [events (atom [])
        src (source/create)
        f (flow/flow-for-source src {:subscription request-sub
                                     :relieve? false
                                     :debug-fn #(swap! events conj %)})
        runner (collect-runner f)]
    (try
      (source/emit! src request-invalidation)
      (source/close! src)

      (is (= :success (:status (task-result runner))))

      (is (contains? (event-names @events)
                     :gesso.live.flow/source-tap-opened))

      (is (eventually
           #(contains? (event-names @events)
                       :gesso.live.flow/source-tap-closed)))

      (finally
        (source/close! src)))))

;; -----------------------------------------------------------------------------
;; Relief smoke behavior
;; -----------------------------------------------------------------------------

(deftest flow-for-stream-with-relieve-enabled-still-emits-live-events-test
  (let [stream (s/stream)
        f (flow/flow-for-stream stream {:subscription request-sub})
        runner (collect-runner f)]
    (is (= true (put-value stream request-invalidation)))
    (s/close! stream)
    (is (= {:status :success
            :value [{:event "live-update"
                     :invalidation request-invalidation}]}
           (task-result runner)))))
