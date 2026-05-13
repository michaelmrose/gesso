(ns gesso.live.transport.sse-test
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.live.transport.sse :as sse]
   [manifold.stream :as s]
   [missionary.core :as m]))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(def timeout-ms 1000)

(def request-invalidation
  {:topic :request
   :id "req-1"
   :change/kind :updated})

(def live-event
  {:event "live-update"
   :invalidation request-invalidation})

(def client-oob-event
  {:event :client-oob
   :invalidation request-invalidation
   :data {:source :test}
   :consistency-token "tx-1"})

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

(defn never-flow
  []
  (m/ap
    (m/? m/never)))

;; -----------------------------------------------------------------------------
;; Option validation
;; -----------------------------------------------------------------------------

(deftest prepare-options-accepts-defaults-test
  (let [options (sse/prepare-options! nil)]
    (is (= 200 (:status options)))
    (is (= pr-str (:encoder options)))
    (is (= true (:close-on-complete? options)))
    (is (nil? (:keepalive-ms options)))))

(deftest prepare-options-rejects-invalid-functions-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":encoder must be a function"
       (sse/prepare-options! {:encoder :not-a-function})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":debug-fn must be a function"
       (sse/prepare-options! {:debug-fn :not-a-function})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":on-error must be a function"
       (sse/prepare-options! {:on-error :not-a-function})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":on-close must be a function"
       (sse/prepare-options! {:on-close :not-a-function}))))

(deftest prepare-options-rejects-invalid-booleans-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":close-on-complete\? must be a boolean"
       (sse/prepare-options! {:close-on-complete? :sometimes}))))

(deftest prepare-options-rejects-invalid-keepalive-ms-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":keepalive-ms must be a positive integer"
       (sse/prepare-options! {:keepalive-ms 0})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":keepalive-ms must be a positive integer"
       (sse/prepare-options! {:keepalive-ms -1})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":keepalive-ms must be a positive integer"
       (sse/prepare-options! {:keepalive-ms "soon"}))))

;; -----------------------------------------------------------------------------
;; Frame formatting
;; -----------------------------------------------------------------------------

(deftest comment-frame-formats-comment-test
  (is (= ": ping\n\n"
         (sse/comment-frame "ping"))))

(deftest comment-frame-splits-multiline-comments-test
  (is (= ": one\n: two\n\n"
         (sse/comment-frame "one\ntwo"))))

(deftest event-frame-formats-basic-event-test
  (is (= "event: live-update\ndata: payload\n\n"
         (sse/event-frame {:event "live-update"
                           :data "payload"}))))

(deftest event-frame-formats-id-retry-and-data-test
  (is (= "id: 1\nevent: live-update\nretry: 5000\ndata: payload\n\n"
         (sse/event-frame {:id "1"
                           :event "live-update"
                           :retry 5000
                           :data "payload"}))))

(deftest event-frame-splits-multiline-data-test
  (is (= "event: live-update\ndata: one\ndata: two\n\n"
         (sse/event-frame {:event "live-update"
                           :data "one\ntwo"}))))

(deftest event-frame-rejects-newlines-in-field-values-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"SSE field value must not contain a newline"
       (sse/event-frame {:event "bad\nevent"
                         :data "payload"})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"SSE field value must not contain a newline"
       (sse/event-frame {:id "bad\nid"
                         :data "payload"})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"SSE field value must not contain a newline"
       (sse/event-frame {:retry "bad\nretry"
                         :data "payload"}))))

(deftest live-event-payload-removes-event-field-test
  (is (= {:invalidation request-invalidation}
         (sse/live-event-payload live-event))))

(deftest live-event-frame-uses-custom-encoder-test
  (let [seen (atom nil)
        frame (sse/live-event-frame
               client-oob-event
               {:encoder (fn [payload]
                           (reset! seen payload)
                           "encoded")})]
    (is (= "event: client-oob\ndata: encoded\n\n"
           frame))
    (is (= {:invalidation request-invalidation
            :data {:source :test}
            :consistency-token "tx-1"}
           @seen))))

(deftest live-event-frame-wraps-invalid-live-event-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Invalid gesso.live value"
       (sse/live-event-frame {:event "live-update"}))))

;; -----------------------------------------------------------------------------
;; Flow/frame transformation
;; -----------------------------------------------------------------------------

(deftest frames-flow-converts-live-events-to-frames-test
  (let [frames (sse/frames-flow
                (m/seed [live-event])
                {:encoder (constantly "encoded")})
        runner (start-task (m/reduce conj [] frames))]
    (is (= {:status :success
            :value ["event: live-update\ndata: encoded\n\n"]}
           (task-result runner)))))

(deftest frames-flow-wraps-frame-build-failure-test
  (let [frames (sse/frames-flow
                (m/seed [{:event "live-update"}])
                nil)
        runner (start-task (m/reduce conj [] frames))
        result (task-result runner)
        error (:error result)]
    (is (= :failure (:status result)))
    (is (= "gesso.live SSE failed to build frame."
           (ex-message error)))
    (is (some? (ex-cause error)))))

(deftest frames-flow-emits-debug-events-test
  (let [events (atom [])
        frames (sse/frames-flow
                (m/seed [live-event])
                {:encoder (constantly "encoded")
                 :debug-fn #(swap! events conj %)})
        runner (start-task (m/reduce conj [] frames))]
    (is (= :success (:status (task-result runner))))
    (is (contains? (event-names @events)
                   :gesso.live.sse/frame-built))))

;; -----------------------------------------------------------------------------
;; Writing frames/events
;; -----------------------------------------------------------------------------

(deftest put-frame-task-writes-frame-test
  (let [out (s/stream)
        runner (start-task (sse/put-frame-task out "frame\n\n" nil))]
    (is (= "frame\n\n" (take-value out)))
    (is (= {:status :success
            :value true}
           (task-result runner)))))

(deftest put-frame-task-fails-when-stream-rejects-frame-test
  (let [out (s/stream)]
    (s/close! out)
    (let [runner (start-task (sse/put-frame-task out "frame\n\n" nil))
          result (task-result runner)]
      (is (= :failure (:status result)))
      (is (= "gesso.live SSE output stream rejected frame."
             (ex-message (:error result)))))))

(deftest write-frames-task-drains-frames-to-stream-test
  (let [out (s/stream)
        frames (m/seed ["a\n\n" "b\n\n"])
        runner (start-task (sse/write-frames-task out frames nil))]
    (is (= "a\n\n" (take-value out)))
    (is (= "b\n\n" (take-value out)))
    (is (= {:status :success
            :value nil}
           (task-result runner)))))

(deftest write-events-task-drains-live-events-to-stream-test
  (let [out (s/stream)
        events (m/seed [live-event])
        runner (start-task
                (sse/write-events-task
                 out
                 events
                 {:encoder (constantly "encoded")}))]
    (is (= "event: live-update\ndata: encoded\n\n"
           (take-value out)))
    (is (= {:status :success
            :value nil}
           (task-result runner)))))

;; -----------------------------------------------------------------------------
;; Response/start lifecycle
;; -----------------------------------------------------------------------------

(deftest response-builds-sse-response-test
  (let [stream (s/stream)
        response (sse/response stream {:status 201
                                       :headers {"x-test" "yes"}})]
    (is (= 201 (:status response)))
    (is (= stream (:body response)))
    (is (= "text/event-stream; charset=utf-8"
           (get-in response [:headers "content-type"])))
    (is (= "no-cache, no-transform"
           (get-in response [:headers "cache-control"])))
    (is (= "no"
           (get-in response [:headers "x-accel-buffering"])))
    (is (= "yes"
           (get-in response [:headers "x-test"])))))

(deftest start-drains-events-into-response-stream-test
  (let [started (sse/start!
                 (m/seed [live-event])
                 {:encoder (constantly "encoded")})]
    (try
      (is (= "event: live-update\ndata: encoded\n\n"
             (take-value (:stream started))))
      (is (eventually #(sse/closed? started)))
      (is (s/closed? (:stream started)))
      (finally
        (sse/cancel! started)))))

(deftest start-returns-response-map-test
  (let [started (sse/start! (never-flow))]
    (try
      (is (= 200 (get-in started [:response :status])))
      (is (= (:stream started)
             (get-in started [:response :body])))
      (is (= "text/event-stream; charset=utf-8"
             (get-in started [:response :headers "content-type"])))
      (finally
        (sse/cancel! started)))))

(deftest cancel-closes-stream-and-calls-on-close-once-test
  (let [close-count (atom 0)
        started (sse/start! (never-flow)
                            {:on-close (fn [_entry]
                                         (swap! close-count inc))})]
    (sse/cancel! started)
    (sse/cancel! started)
    (is (eventually #(sse/closed? started)))
    (is (eventually #(s/closed? (:stream started))))
    (is (eventually #(= 1 @close-count)))))

(deftest external-stream-close-cancels-upstream-flow-test
  (let [cleanup (promise)
        events (m/observe
                (fn [_emit!]
                  (fn []
                    (deliver cleanup :cleaned-up))))
        started (sse/start! events)]
    (s/close! (:stream started))
    (is (= :cleaned-up (deref cleanup timeout-ms ::timeout)))
    (is (eventually #(sse/closed? started)))))

(deftest flow-failure-calls-on-error-and-closes-test
  (let [seen-error (promise)
        started (sse/start!
                 (m/ap
                   (throw (RuntimeException. "flow boom")))
                 {:on-error (fn [error _entry]
                              (deliver seen-error error))})]
    (is (= "flow boom"
           (ex-message (deref seen-error timeout-ms ::timeout))))
    (is (eventually #(sse/closed? started)))
    (is (s/closed? (:stream started)))))

(deftest on-close-runs-on-flow-completion-test
  (let [closed (promise)
        started (sse/start!
                 (m/seed [live-event])
                 {:encoder (constantly "encoded")
                  :on-close #(deliver closed %)})]
    (try
      (is (= "event: live-update\ndata: encoded\n\n"
             (take-value (:stream started))))
      (let [entry (deref closed timeout-ms ::timeout)]
        (is (map? entry))
        (is (= :completed (:reason entry))))
      (finally
        (sse/cancel! started)))))

(deftest close-on-complete-false-leaves-stream-open-test
  (let [started (sse/start!
                 (m/seed [live-event])
                 {:encoder (constantly "encoded")
                  :close-on-complete? false})]
    (try
      (is (= "event: live-update\ndata: encoded\n\n"
             (take-value (:stream started))))
      (Thread/sleep 50)
      (is (not (sse/closed? started)))
      (is (not (s/closed? (:stream started))))
      (finally
        (sse/cancel! started)))))

;; -----------------------------------------------------------------------------
;; Keepalive
;; -----------------------------------------------------------------------------

(deftest keepalive-sends-comment-frame-test
  (let [started (sse/start!
                 (never-flow)
                 {:keepalive-ms 25
                  :keepalive-comment "ping"})]
    (try
      (is (= ": ping\n\n"
             (take-value (:stream started))))
      (finally
        (sse/cancel! started)))))

(deftest keepalive-emits-debug-event-test
  (let [events (atom [])
        started (sse/start!
                 (never-flow)
                 {:keepalive-ms 25
                  :debug-fn #(swap! events conj %)})]
    (try
      (is (= ": ping\n\n"
             (take-value (:stream started))))
      (is (eventually #(contains? (event-names @events)
                                  :gesso.live.sse/keepalive-sent)))
      (finally
        (sse/cancel! started)))))

;; -----------------------------------------------------------------------------
;; Debug lifecycle
;; -----------------------------------------------------------------------------

(deftest start-emits-debug-lifecycle-events-test
  (let [events (atom [])
        started (sse/start!
                 (m/seed [live-event])
                 {:encoder (constantly "encoded")
                  :debug-fn #(swap! events conj %)})]
    (try
      (is (= "event: live-update\ndata: encoded\n\n"
             (take-value (:stream started))))
      (is (eventually #(sse/closed? started)))
      (let [names (event-names @events)]
        (is (contains? names :gesso.live.sse/started))
        (is (contains? names :gesso.live.sse/frame-built))
        (is (contains? names :gesso.live.sse/frame-write-attempted))
        (is (contains? names :gesso.live.sse/frame-write-accepted))
        (is (contains? names :gesso.live.sse/flow-completed))
        (is (contains? names :gesso.live.sse/closed)))
      (finally
        (sse/cancel! started)))))
