(ns gesso.live.flow-test
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.live.flow :as flow]
   [gesso.live.progression :as progression]
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


(deftest stream-flow-releases-stream-before-blocked-close-hook-test
  ;; Flow cancellation is a terminal action. An application on-close hook
  ;; may block waiting for another component; that must not keep the source
  ;; subscription open or retain its stream during the callback.
  (let [src (source/create)
        stream (source/subscribe src request-sub)
        hook-entered (promise)
        release-hook (promise)
        close-count (atom 0)
        runner (collect-runner
                (flow/stream-flow
                 stream
                 {:on-close (fn []
                              (swap! close-count inc)
                              (deliver hook-entered :entered)
                              @release-hook)}))
        cancelled (future (cancel-runner! runner))]
    (try
      (is (= :entered (deref hook-entered 2000 ::timeout))
          "cancellation should reach the application hook")
      (is (s/closed? stream)
          "physical stream must close before application on-close")
      (is (= 0 (:subscription-count (source/stats src)))
          "source registration must be retired before application on-close")
      (finally
        (deliver release-hook :released)
        (deref cancelled 2000 ::timeout)
        (source/close! src)))
    (is (= 1 @close-count)
        "the close hook remains at-most-once")))

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
        takes (atom [])
        terminal-token (atom nil)]
    (with-redefs [s/take! (fn [_stream stream-closed-sentinel]
                            (reset! terminal-token stream-closed-sentinel)
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

          (complete! (nth @takes 2) @terminal-token)

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
;; Authoritative progression and relief
;; -----------------------------------------------------------------------------

(def basis-1
  {:test/basis :basis-1})

(def basis-2
  {:test/basis :basis-2})

(def basis-3
  {:test/basis :basis-3})

(def progression-1
  (progression/requirement basis-1))

(def progression-2
  (progression/requirement basis-2))

(def progression-3
  (progression/requirement basis-3))

(def progressing-request-invalidation
  (assoc request-invalidation :progression progression-1))

(defn live-event
  ([event-name invalidation]
   {:event event-name
    :invalidation invalidation})
  ([event-name invalidation requirement]
   {:event event-name
    :invalidation (assoc invalidation :progression requirement)
    :progression requirement}))

(deftest invalidation-event-carries-progression-at-top-level-test
  (let [event (flow/invalidation-event progressing-request-invalidation)]
    (is (= progression-1 (:progression event)))
    (is (= progression-1
           (get-in event [:invalidation :progression])))
    (is (= progressing-request-invalidation
           (:invalidation event)))))

(deftest coalesce-live-events-composes-distinct-progression-test
  (let [pending (assoc (live-event "pending"
                                   (assoc request-invalidation :change/kind :pending)
                                   progression-1)
                       :data {:value :pending})
        incoming (assoc (live-event "incoming"
                                    (assoc request-invalidation :change/kind :incoming)
                                    progression-2)
                        :data {:value :incoming})
        expected-progression (progression/compose progression-1 progression-2)
        result (flow/coalesce-live-events pending incoming)]
    (is (= "incoming" (:event result)))
    (is (= {:value :incoming} (:data result)))
    (is (= :incoming
           (get-in result [:invalidation :change/kind])))
    (is (= expected-progression (:progression result)))
    (is (= expected-progression
           (get-in result [:invalidation :progression])))))

(deftest coalesce-live-events-cannot-erase-pending-progression-test
  (let [pending (live-event "pending" request-invalidation progression-1)
        incoming {:event "incoming"
                  :invalidation (assoc request-invalidation
                                       :change/kind :incoming)
                  :data {:value :incoming}}
        result (flow/coalesce-live-events pending incoming)]
    (is (= "incoming" (:event result)))
    (is (= {:value :incoming} (:data result)))
    (is (= progression-1 (:progression result)))
    (is (= progression-1
           (get-in result [:invalidation :progression])))))

(deftest coalesce-live-events-preserves-incoming-progression-test
  (let [pending {:event "pending"
                 :invalidation request-invalidation}
        incoming (live-event "incoming" request-invalidation progression-2)
        result (flow/coalesce-live-events pending incoming)]
    (is (= progression-2 (:progression result)))
    (is (= progression-2
           (get-in result [:invalidation :progression])))))

(deftest coalesce-live-events-is-idempotent-for-repeated-requirement-test
  (let [pending (live-event "pending" request-invalidation progression-1)
        incoming (live-event "incoming" request-invalidation progression-1)
        result (flow/coalesce-live-events pending incoming)]
    (is (= progression-1 (:progression result)))
    (is (= progression-1
           (get-in result [:invalidation :progression])))))

(deftest coalesce-live-events-is-associative-test
  (let [a (live-event "a" request-invalidation progression-1)
        b (live-event "b" request-invalidation progression-2)
        c (assoc (live-event "c" request-invalidation progression-3)
                 :data {:winner :c})
        left (flow/coalesce-live-events
              (flow/coalesce-live-events a b)
              c)
        right (flow/coalesce-live-events
               a
               (flow/coalesce-live-events b c))]
    (is (= left right))
    (is (= "c" (:event left)))
    (is (= {:winner :c} (:data left)))
    (is (= (progression/compose progression-1
                                progression-2
                                progression-3)
           (:progression left)))))

(deftest coalesce-live-events-rejects-disagreeing-progression-test
  (let [bad-event {:event "bad"
                   :progression progression-1
                   :invalidation (assoc request-invalidation
                                        :progression progression-2)}]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"progression disagrees with its invalidation"
         (flow/coalesce-live-events bad-event
                                    {:event "incoming"
                                     :invalidation request-invalidation})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"progression disagrees with its invalidation"
         (flow/coalesce-live-events
          {:event "pending"
           :invalidation request-invalidation}
          bad-event)))))

(deftest relieve-uses-progression-safe-semigroup-test
  (let [first-event (live-event "first" request-invalidation progression-1)
        second-event (live-event "second" request-invalidation progression-2)
        latest-event {:event "latest"
                      :invalidation (assoc request-invalidation
                                           :change/kind :latest)
                      :data {:winner :latest}}
        expected-progression (progression/compose progression-1 progression-2)
        runner (collect-runner
                (flow/relieve
                 (m/seed [first-event second-event latest-event])))
        result (task-result runner)]
    (is (= :success (:status result)))
    (is (= 1 (count (:value result))))
    (let [event (first (:value result))]
      (is (= "latest" (:event event)))
      (is (= {:winner :latest} (:data event)))
      (is (= :latest
             (get-in event [:invalidation :change/kind])))
      (is (= expected-progression (:progression event)))
      (is (= expected-progression
             (get-in event [:invalidation :progression]))))))

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

(deftest stream-flow-prebuffered-closed-stream-drains-without-reentrant-emission-test
  ;; Manifold may complete take! synchronously when a value is already
  ;; buffered. A second synchronous completion must not reenter Missionary's
  ;; emit! while it is still processing the preceding value.
  (let [stream (s/stream 128)
        values (vec (range 64))]
    (try
      (doseq [value values]
        (is (= true (put-value stream value))))
      (s/close! stream)
      (let [started (future (collect-runner (flow/stream-flow stream)))
            runner (deref started timeout-ms ::timeout)]
        (try
          (is (not= ::timeout runner)
              "a buffered stream must start collecting without hanging")
          (when (map? runner)
            (is (= {:status :success :value values}
                   (task-result runner))
                "the bridge must drain every queued value before close"))
          (finally
            (future-cancel started))))
      (finally
        (s/close! stream)))))

(deftest stream-flow-synchronous-first-take-failure-closes-with-error-test
  ;; take! may throw before returning a deferred. The bridge must terminate
  ;; with a wrapped failure instead of silently abandoning its first pull.
  (let [stream (s/stream)
        cause (ex-info "first take failed" {:stage :initial})
        close-count (atom 0)
        f (flow/stream-flow
           stream
           {:take! (fn [_ _] (throw cause))
            :on-close #(swap! close-count inc)})
        runner (collect-runner f)
        result (task-result runner)]
    (is (= :failure (:status result)))
    (is (= "gesso.live stream failed." (some-> result :error ex-message)))
    (is (identical? cause (some-> result :error ex-cause)))
    (is (eventually #(s/closed? stream)))
    (is (eventually #(= 1 @close-count)))))

(deftest stream-flow-synchronous-later-take-failure-closes-with-error-test
  ;; After emitting a buffered value, a synchronous take! failure must
  ;; propagate through the same finite-flow failure path as a failed deferred.
  (let [stream (s/stream 4)
        cause (ex-info "later take failed" {:stage :subsequent})
        calls (atom 0)
        seen (atom [])
        f (flow/stream-flow
           stream
           {:take! (fn [s closed]
                     (if (= 1 (swap! calls inc))
                       (s/take! s closed)
                       (throw cause)))
            :debug-fn (fn [event]
                        (when (= :gesso.live.flow/stream-value (:event event))
                          (swap! seen conj (:value event))))})]
    (try
      (is (true? (put-value stream :first)))
      (let [runner (collect-runner f)
            result (task-result runner)]
        (is (= :failure (:status result)))
        (is (identical? cause (some-> result :error ex-cause)))
        (is (= [:first] @seen))
        (is (= 2 @calls))
        (is (eventually #(s/closed? stream))))
      (finally
        (s/close! stream)))))

(deftest stream-flow-treats-former-close-keyword-as-ordinary-data-test
  ;; A stream is allowed to carry the namespaced keyword that previously
  ;; served as a structural close marker. Data must never terminate the flow.
  (let [stream (s/stream 4)
        values [:gesso.live.flow/closed :after-close-keyword]]
    (try
      (doseq [value values]
        (is (true? (put-value stream value))))
      (s/close! stream)
      (is (= {:status :success :value values}
             (task-result (collect-runner (flow/stream-flow stream)))))
      (finally
        (s/close! stream)))))

(deftest stream-flow-treats-former-error-key-as-ordinary-data-test
  ;; A valid event map may contain the namespaced key formerly used for
  ;; an internal failure envelope. It is caller data, not an exception.
  (let [stream (s/stream 4)
        values [{:topic :request :id "req-1"
                 :gesso.live.flow/stream-error :caller-metadata}
                {:topic :request :id "req-2"}]]
    (try
      (doseq [value values]
        (is (true? (put-value stream value))))
      (s/close! stream)
      (is (= {:status :success :value values}
             (task-result (collect-runner (flow/stream-flow stream)))))
      (finally
        (s/close! stream)))))

(deftest stream-flow-debug-callback-failure-does-not-stop-delivery-test
  ;; A faulty optional diagnostic hook must not strand the bridge's serialized
  ;; completion pump or suppress ordinary values and terminal close.
  (let [stream (s/stream 4)
        values [:first :second]
        trace-attempted (promise)]
    (try
      (doseq [value values]
        (is (true? (put-value stream value))))
      (s/close! stream)
      (let [runner (collect-runner
                    (flow/stream-flow
                     stream
                     {:debug-fn (fn [event]
                                  (when (= :gesso.live.flow/stream-value
                                           (:event event))
                                    (deliver trace-attempted true)
                                    (throw (ex-info "diagnostic hook failed" {}))))}))]
        (is (= true (deref trace-attempted timeout-ms ::timeout)))
        (is (= {:status :success :value values}
               (task-result runner))
            "the diagnostic callback must not affect event delivery"))
      (finally
        (s/close! stream)))))

(deftest flow-for-source-debug-callback-failure-does-not-prevent-subscription-test
  (let [src (source/create)]
    (try
      (let [f (flow/flow-for-source
               src
               {:subscription request-sub
                :relieve? false
                :debug-fn (fn [_] (throw (ex-info "trace unavailable" {})))})
            runner (collect-runner f)]
        (source/emit! src request-invalidation)
        (source/close! src)
        (is (= {:status :success
                :value [(flow/invalidation-event request-invalidation)]}
               (task-result runner))
            "trace errors must not prevent flow creation, delivery, or completion"))
      (finally
        (source/close! src)))))

(deftest stream-flow-public-legacy-sentinel-is-ordinary-stream-data-test
  ;; The v1920 identity token was a public var; callers could legitimately
  ;; place that same object in a Manifold stream. It must not end the flow.
  (let [stream (s/stream 4)
        values [flow/closed-sentinel :after-legacy-marker]]
    (try
      (doseq [value values]
        (is (true? (put-value stream value))))
      (s/close! stream)
      (let [result (task-result (collect-runner (flow/stream-flow stream)))]
        (is (= :success (:status result)))
        (is (= values (:value result))
            "a publicly accessible value is not a structural close signal"))
      (finally
        (s/close! stream)))))

(deftest stream-flow-distinguishes-terminal-tokens-between-flows-test
  ;; The close marker supplied to each take! adapter must be owned by that
  ;; flow. A marker from another flow is valid data here, not termination.
  (let [first-token (promise)
        first-stream (s/stream 4)
        second-stream (s/stream 4)]
    (try
      (let [first (collect-runner
                   (flow/stream-flow
                    first-stream
                    {:take! (fn [stream token]
                              (deliver first-token token)
                              (s/take! stream token))}))
            token (deref first-token timeout-ms ::timeout)]
        (is (not= ::timeout token))
        (is (true? (put-value first-stream :done)))
        (s/close! first-stream)
        (is (= {:status :success :value [:done]}
               (task-result first)))
        (is (true? (put-value second-stream token)))
        (is (true? (put-value second-stream :after-foreign-token)))
        (s/close! second-stream)
        (let [second (task-result
                      (collect-runner (flow/stream-flow second-stream)))]
          (is (= :success (:status second)))
          (is (= [token :after-foreign-token] (:value second)))))
      (finally
        (s/close! first-stream)
        (s/close! second-stream)))))
