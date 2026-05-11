(ns gesso.live.flow-test
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.live.flow :as flow]
   [gesso.live.source :as source]
   [manifold.stream :as s]))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(def timeout-ms 1000)

(def request-sub
  {:topic :request
   :id "req-1"})

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

(defn start-task
  [task]
  (let [p (promise)
        cancel (task #(deliver p {:status :success :value %})
                     #(deliver p {:status :failure :error %}))]
    {:promise p
     :cancel cancel}))

(defn task-result
  [runner]
  (let [result (deref (:promise runner) timeout-ms ::timeout)]
    (when (= ::timeout result)
      ((:cancel runner)))
    result))

(defn put-value
  [stream value]
  (deref (s/put! stream value) timeout-ms ::timeout))

(defn collect-runner
  [f]
  (start-task (flow/collect-task f)))

;; -----------------------------------------------------------------------------
;; Pure helpers
;; -----------------------------------------------------------------------------

(deftest default-interested-matches-topic-and-id-test
  (is (flow/default-interested? request-sub request-invalidation))
  (is (not (flow/default-interested? request-sub other-request-invalidation)))
  (is (not (flow/default-interested? request-sub store-invalidation))))

(deftest invalidation-key-selects-topic-and-id-test
  (is (= {:topic :request :id "req-1"}
         (flow/invalidation-key request-invalidation))))

(deftest invalidation-event-builds-default-live-event-test
  (is (= {:event "live-update"
          :invalidation request-invalidation}
         (flow/invalidation-event request-invalidation))))

(deftest invalidation-event-supports-custom-event-data-and-token-test
  (let [event (flow/invalidation-event
               request-invalidation
               {:event :client-oob
                :data {:reason :test}
                :consistency-token "tx-1"})]
    (is (= "client-oob" (:event event)))
    (is (= request-invalidation (:invalidation event)))
    (is (= {:reason :test} (:data event)))
    (is (= "tx-1" (:consistency-token event)))))

(deftest invalidation-event-supports-data-function-test
  (let [event (flow/invalidation-event
               request-invalidation
               {:data (fn [invalidation]
                        {:key (flow/invalidation-key invalidation)})})]
    (is (= {:key {:topic :request :id "req-1"}}
           (:data event)))))

;; -----------------------------------------------------------------------------
;; Option validation
;; -----------------------------------------------------------------------------

(deftest prepare-options-accepts-valid-options-test
  (let [options (flow/prepare-options!
                 {:subscription request-sub
                  :event :live-update
                  :relieve? false})]
    (is (= request-sub (:subscription options)))
    (is (= :live-update (:event options)))
    (is (= false (:relieve? options)))))

(deftest prepare-options-rejects-invalid-subscription-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Invalid gesso.live value"
       (flow/prepare-options!
        {:subscription {:topic :request}}))))

(deftest prepare-options-rejects-invalid-debug-fn-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":debug-fn must be a function"
       (flow/prepare-options!
        {:subscription request-sub
         :debug-fn :not-a-function}))))

(deftest prepare-options-rejects-invalid-on-close-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":on-close must be a function"
       (flow/prepare-options!
        {:subscription request-sub
         :on-close :not-a-function}))))

(deftest prepare-options-rejects-invalid-relieve-flag-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":relieve\? must be a boolean"
       (flow/prepare-options!
        {:subscription request-sub
         :relieve? :sometimes}))))

;; -----------------------------------------------------------------------------
;; stream-flow
;; -----------------------------------------------------------------------------

(deftest stream-flow-emits-stream-values-test
  (let [stream (s/stream)
        f (flow/stream-flow stream)
        runner (collect-runner f)]
    (is (= true (put-value stream :a)))
    (is (= true (put-value stream :b)))
    (s/close! stream)
    (is (= {:status :success :value [:a :b]}
           (task-result runner)))))

(deftest stream-flow-calls-on-close-test
  (let [stream (s/stream)
        closed (promise)
        f (flow/stream-flow stream {:on-close #(deliver closed :closed)})
        runner (collect-runner f)]
    (is (= true (put-value stream :a)))
    (s/close! stream)
    (is (= {:status :success :value [:a]}
           (task-result runner)))
    (is (= :closed (deref closed timeout-ms ::timeout)))))

;; -----------------------------------------------------------------------------
;; flow-for-stream
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

(deftest flow-for-stream-emits-debug-events-when-enabled-test
  (let [events (atom [])
        stream (s/stream)
        f (flow/flow-for-stream
           stream
           {:subscription request-sub
            :relieve? false
            :debug-fn #(swap! events conj (:event %))})
        runner (collect-runner f)]
    (is (= true (put-value stream request-invalidation)))
    (s/close! stream)
    (is (= :success (:status (task-result runner))))
    (is (some #{:gesso.live.flow/flow-for-stream-created} @events))
    (is (some #{:gesso.live.flow/invalidation-received} @events))
    (is (some #{:gesso.live.flow/invalidation-matched} @events))
    (is (some #{:gesso.live.flow/stream-flow-closed} @events))))

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

;; -----------------------------------------------------------------------------
;; flow-for-source
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
        closed (promise)
        f (flow/flow-for-source src {:subscription request-sub
                                     :relieve? false
                                     :on-close #(deliver closed :closed)})
        runner (collect-runner f)]
    (try
      (source/emit! src request-invalidation)
      (source/close! src)
      (is (= :success (:status (task-result runner))))
      (is (= :closed (deref closed timeout-ms ::timeout)))
      (finally
        (source/close! src)))))
