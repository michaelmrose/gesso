(ns gesso.live.transport.sse-test
  (:require
   [clojure.edn :as edn]
   [clojure.test :refer [deftest is testing]]
   [gesso.live.progression :as progression]
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

(def progression-basis
  {:authority :xtdb
   :database "xtdb"
   :tx-id "42"
   :snapshot-token "opaque-snapshot-token"})

(def progression-requirement
  (progression/requirement progression-basis))

(def progression-live-event
  {:event "live-update"
   :progression progression-requirement
   :invalidation (assoc request-invalidation
                        :progression progression-requirement)})

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

(deftest live-event-payload-encodes-progression-at-transport-boundary-test
  (let [wire (progression/requirement->wire progression-requirement)
        payload (sse/live-event-payload progression-live-event)]
    (is (= wire (:progression payload)))
    (is (= wire (get-in payload [:invalidation :progression])))
    (is (= request-invalidation
           (dissoc (:invalidation payload) :progression)))
    (is (not (progression/requirement? (:progression payload))))
    (is (= progression-requirement
           (progression/wire->requirement (:progression payload))))
    (is (= progression-requirement
           (progression/wire->requirement
            (get-in payload [:invalidation :progression]))))))

(deftest live-event-payload-does-not-mutate-internal-live-event-test
  (let [before progression-live-event]
    (sse/live-event-payload progression-live-event)
    (is (= before progression-live-event))
    (is (progression/requirement? (:progression progression-live-event)))
    (is (progression/requirement?
         (get-in progression-live-event [:invalidation :progression])))))

(deftest live-event-frame-custom-encoder-receives-versioned-progression-wire-test
  (let [seen (atom nil)
        wire (progression/requirement->wire progression-requirement)
        frame (sse/live-event-frame
               progression-live-event
               {:encoder (fn [payload]
                           (reset! seen payload)
                           "encoded")})]
    (is (= "event: live-update\ndata: encoded\n\n" frame))
    (is (= wire (:progression @seen)))
    (is (= wire (get-in @seen [:invalidation :progression])))
    (is (= progression-requirement
           (progression/wire->requirement (:progression @seen))))))

(deftest default-edn-frame-carries-round-trippable-progression-wire-test
  (let [frame (sse/live-event-frame progression-live-event)
        data-line (->> (clojure.string/split-lines frame)
                       (filter #(clojure.string/starts-with? % "data: "))
                       first)
        payload (edn/read-string (subs data-line (count "data: ")))]
    (is (= "event: live-update"
           (first (clojure.string/split-lines frame))))
    (is (= progression-requirement
           (progression/wire->requirement (:progression payload))))
    (is (= progression-requirement
           (progression/wire->requirement
            (get-in payload [:invalidation :progression]))))))

(deftest live-event-frame-rejects-wire-progression-as-internal-state-test
  (let [wire (progression/requirement->wire progression-requirement)]
    (testing "top-level wire form cannot bypass the internal normalized schema"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid gesso.live value"
           (sse/live-event-frame
            (assoc progression-live-event :progression wire)))))
    (testing "nested wire form cannot bypass the internal normalized schema"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid gesso.live value"
           (sse/live-event-frame
            (assoc-in progression-live-event
                      [:invalidation :progression]
                      wire)))))))

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

;; -----------------------------------------------------------------------------
;; Diagnostic callback isolation
;; -----------------------------------------------------------------------------

(deftest throwing-debug-hook-does-not-turn-valid-frame-into-error-test
  (let [debug-attempts (atom 0)
        frames (sse/frames-flow
                (m/seed [live-event])
                {:encoder (constantly "encoded")
                 :debug-fn (fn [_]
                             (swap! debug-attempts inc)
                             (throw (ex-info "trace failed" {})))})
        runner (start-task (m/reduce conj [] frames))]
    (is (= {:status :success
            :value ["event: live-update\ndata: encoded\n\n"]}
           (task-result runner))
        "a faulty debug callback must not change valid event framing")
    (is (pos? @debug-attempts))))

(deftest throwing-debug-hook-does-not-prevent-frame-write-test
  (let [out (s/stream)
        runner (start-task
                (sse/put-frame-task
                 out "frame\n\n"
                 {:debug-fn (fn [_]
                              (throw (ex-info "trace failed" {})))}))]
    (try
      (is (= "frame\n\n" (take-value out))
          "diagnostics must not suppress accepted frames")
      (is (= {:status :success :value true}
             (task-result runner)))
      (finally
        (s/close! out)))))

(deftest throwing-debug-hook-does-not-interrupt-sse-lifecycle-test
  (let [attempts (atom #{})
        started (sse/start!
                 (m/seed [live-event])
                 {:encoder (constantly "encoded")
                  :debug-fn (fn [entry]
                              (swap! attempts conj (:event entry))
                              (throw (ex-info "trace failed" {})))})]
    (try
      (is (= "event: live-update\ndata: encoded\n\n"
             (take-value (:stream started))))
      (is (eventually #(sse/closed? started)))
      (is (s/closed? (:stream started)))
      (is (contains? @attempts :gesso.live.sse/started))
      (is (contains? @attempts :gesso.live.sse/closed))
      (finally
        (sse/cancel! started)))))

(deftest throwing-debug-hook-does-not-hide-real-frame-error-test
  (let [cause (ex-info "real encoder failed" {:kind :encoder})
        seen-error (promise)
        started (sse/start!
                 (m/seed [live-event])
                 {:encoder (fn [_] (throw cause))
                  :on-error (fn [error _entry] (deliver seen-error error))
                  :debug-fn (fn [_] (throw (ex-info "trace failed" {})))})]
    (try
      (let [error (deref seen-error timeout-ms ::timeout)]
        (is (instance? clojure.lang.ExceptionInfo error))
        (is (= "gesso.live SSE failed to build frame."
               (when (instance? clojure.lang.ExceptionInfo error)
                 (ex-message error))))
        (is (identical? cause
                        (when (instance? clojure.lang.ExceptionInfo error)
                          (ex-cause error))))
        (is (eventually #(sse/closed? started))))
      (finally
        (sse/cancel! started)))))

;; -----------------------------------------------------------------------------
;; Canceller handoff and terminal cleanup
;; -----------------------------------------------------------------------------

(deftest startup-stream-close-cancels-task-even-before-handle-installation-test
  ;; Stream closure can occur inside Missionary task registration, before the
  ;; returned canceller is stored in the transport. That task still belongs
  ;; to the closed connection and must be cancelled exactly once.
  (let [cancellations (atom 0)]
    (with-redefs-fn
      {#'gesso.live.transport.sse/write-events-task
       (fn [out _events _options]
         (fn [_success _failure]
           (s/close! out)
           (fn [] (swap! cancellations inc))))}
      (fn []
        (let [started (sse/start! (m/seed []))]
          (try
            (is (sse/closed? started))
            (is (eventually #(= 1 @cancellations))
                "a task registered during close must receive cancellation")
            (sse/cancel! started)
            (is (= 1 @cancellations)
                "cancellation races must not call a task canceller twice")
            (finally
              (sse/cancel! started))))))))

(deftest event-write-failure-cancels-existing-keepalive-task-test
  ;; The error callback closes the SSE stream itself; on-closed therefore
  ;; observes :closed? true and cannot be relied on to cancel the keepalive.
  (let [keepalive-cancellations (atom 0)
        write-failure (ex-info "fixture write failure" {:source :write})
        errors (atom [])]
    (with-redefs-fn
      {#'gesso.live.transport.sse/keepalive-task*
       (fn [_out _options]
         (fn [_success _failure]
           (fn [] (swap! keepalive-cancellations inc))))
       #'gesso.live.transport.sse/write-events-task
       (fn [_out _events _options]
         (fn [_success failure]
           (failure write-failure)
           (fn [] nil)))}
      (fn []
        (let [started (sse/start! (m/seed [])
                                  {:keepalive-ms 1000
                                   :on-error (fn [error _entry]
                                               (swap! errors conj error))})]
          (try
            (is (sse/closed? started))
            (is (s/closed? (:stream started)))
            (is (= [write-failure] @errors)
                "the underlying transport error must remain visible")
            (is (eventually #(= 1 @keepalive-cancellations))
                "terminal event failure must cancel the keepalive immediately")
            (finally
              (sse/cancel! started))))))))


(deftest sse-cancellation-failure-still-stops-keepalive-test
  ;; A throwing upstream cancellation callback must not skip cancellation of
  ;; a separate keepalive task or make an already closed connection throw.
  (let [cancelled (atom [])
        debug-events (atom [])]
    (with-redefs-fn
      {#'gesso.live.transport.sse/write-events-task
       (fn [_out _events _options]
         (fn [_success _failure]
           (fn []
             (swap! cancelled conj :events)
             (throw (ex-info "upstream canceller failed" {:source :events})))))
       #'gesso.live.transport.sse/keepalive-task*
       (fn [_out _options]
         (fn [_success _failure]
           (fn [] (swap! cancelled conj :keepalive))))}
      (fn []
        (let [started (sse/start! (m/seed [])
                                  {:keepalive-ms 1000
                                   :debug-fn #(swap! debug-events conj %)})]
          (is (nil? (try (sse/cancel! started) nil
                         (catch Exception e e)))
              "cancellation is cleanup, not a new SSE failure")
          (is (sse/closed? started))
          (is (= [:events :keepalive] @cancelled)
              "both resources must be cancelled independently")
          (is (= [:events]
                 (mapv :task (filter #(= :gesso.live.sse/physical-cancel-failed
                                             (:event %)) @debug-events))))
          (sse/cancel! started)
          (is (= [:events :keepalive] @cancelled)
              "repeated close must not recancel resources"))))))

(deftest sse-late-cancellation-failure-does-not-abort-start-test
  ;; A stream may close while a producer's physical cancellation handle is
  ;; being returned. Its failure must not escape start! after terminal close.
  (let [cancelled (atom 0)
        debug-events (atom [])]
    (with-redefs-fn
      {#'gesso.live.transport.sse/write-events-task
       (fn [out _events _options]
         (fn [_success _failure]
           (s/close! out)
           (fn []
             (swap! cancelled inc)
             (throw (ex-info "late upstream canceller failed" {})))))}
      (fn []
        (let [started (try (sse/start! (m/seed [])
                                       {:debug-fn #(swap! debug-events conj %)})
                           (catch Exception e e))]
          (is (map? started) "start! must return its already-closed handle")
          (when (map? started)
            (is (sse/closed? started))
            (is (s/closed? (:stream started)))
            (sse/cancel! started))
          (is (= 1 @cancelled) "late physical canceller runs at most once")
          (is (some #(= :gesso.live.sse/physical-cancel-failed (:event %))
                    @debug-events)))))))

(deftest terminal-keepalive-start-skips-event-source-start-test
  ;; A keepalive may fail synchronously during task installation. Terminal
  ;; shutdown already owns the stream before the event source is started;
  ;; starting it afterward could create an unnecessary upstream subscription.
  (let [error (ex-info "keepalive failed during start" {:kind :keepalive})
        errors (atom [])
        event-starts (atom 0)
        keepalive-cancels (atom 0)]
    (with-redefs-fn
      {#'gesso.live.transport.sse/keepalive-task*
       (fn [_out _options]
         (fn [_success failure]
           (failure error)
           (fn [] (swap! keepalive-cancels inc))))
       #'gesso.live.transport.sse/write-events-task
       (fn [_out _events _options]
         (fn [_success _failure]
           (swap! event-starts inc)
           (fn [] nil)))}
      (fn []
        (let [started (sse/start! (never-flow)
                                  {:keepalive-ms 1000
                                   :on-error (fn [e _entry] (swap! errors conj e))})]
          (is (map? started))
          (is (sse/closed? started))
          (is (s/closed? (:stream started)))
          (is (= [error] @errors))
          (is (= 1 @keepalive-cancels)
              "late installed keepalive canceller must be retired")
          (is (zero? @event-starts)
              "an already-closed SSE stream must not start an upstream source")
          (sse/cancel! started)
          (is (= 1 @keepalive-cancels)))))))

(deftest terminal-stream-closed-during-keepalive-start-skips-event-source-test
  ;; The downstream may disconnect synchronously during keepalive task
  ;; registration; no event producer should be launched on that closed stream.
  (let [event-starts (atom 0)
        keepalive-cancels (atom 0)]
    (with-redefs-fn
      {#'gesso.live.transport.sse/keepalive-task*
       (fn [out _options]
         (fn [_success _failure]
           (s/close! out)
           (fn [] (swap! keepalive-cancels inc))))
       #'gesso.live.transport.sse/write-events-task
       (fn [_out _events _options]
         (fn [_success _failure]
           (swap! event-starts inc)
           (fn [] nil)))}
      (fn []
        (let [started (sse/start! (never-flow) {:keepalive-ms 1000})]
          (is (map? started))
          (is (sse/closed? started))
          (is (s/closed? (:stream started)))
          (is (= 1 @keepalive-cancels))
          (is (zero? @event-starts))
          (sse/cancel! started)
          (is (= 1 @keepalive-cancels)))))))
