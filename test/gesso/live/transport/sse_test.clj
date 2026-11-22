(ns gesso.live.transport.sse-test
  (:require
   [clojure.edn :as edn]
   [clojure.test :refer [deftest is testing]]
   [gesso.live.progression :as progression]
   [gesso.live.model :as model]
   [gesso.live.source :as source]
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

(deftest prepare-options-rejects-explicit-nil-encoder-test
  ;; Unlike the optional tracing/close callbacks, :encoder is required.
  ;; An explicit nil must not override the default encoder and pass admission.
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":encoder must be a function"
       (sse/prepare-options! {:encoder nil})))
  (is (= pr-str (:encoder (sse/prepare-options! {})))))

(deftest nil-encoder-does-not-register-source-interest-test
  ;; A bad encoder must fail before source-backed transport starts an eager
  ;; subscription; otherwise an idle stream can remain open until an event.
  (let [src (source/create)]
    (try
      (let [result (try
                     (sse/start-source!
                      src
                      {:subscription {:topic :request :id "req-1"}}
                      {:encoder nil})
                     (catch clojure.lang.ExceptionInfo e e))]
        (try
          (is (instance? clojure.lang.ExceptionInfo result))
          (is (and (instance? clojure.lang.ExceptionInfo result)
                   (re-find #":encoder must be a function"
                            (ex-message result))))
          (is (zero? (:subscription-count (source/stats src)))
              "Invalid SSE encoder must not leave a source subscription")
          (finally
            (when (map? result)
              (sse/cancel! result)))))
      (finally
        (source/close! src)))))

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

(deftest event-frame-rejects-browser-ignored-id-test
  ;; EventSource ignores an id: field containing U+0000, silently losing the
  ;; reconnection cursor. Fail at framing rather than emit a misleading id.
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"SSE id field must not contain NUL"
       (sse/event-frame {:id (str "seq-" (char 0) "42")
                         :data "change"})))
  ;; Empty ids are meaningful: they explicitly clear Last-Event-ID.
  (is (= "id: \ndata: change\n\n"
         (sse/event-frame {:id "" :data "change"}))))

(deftest event-frame-rejects-browser-ignored-retry-test
  ;; The HTML event stream parser accepts retry: only when every character
  ;; is an ASCII digit. Reject values that the browser would silently ignore.
  (doseq [retry [-1 "+1" "25ms" "1.5" "" "1 0" "١٢"]]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"SSE retry field must contain only ASCII digits"
         (sse/event-frame {:retry retry :data "change"}))
        (str "Invalid EventSource retry: " (pr-str retry))))
  (is (= "retry: 0\ndata: change\n\n"
         (sse/event-frame {:retry 0 :data "change"})))
  (is (= "retry: 0015\ndata: change\n\n"
         (sse/event-frame {:retry "0015" :data "change"}))))

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

(deftest event-frame-preserves-false-data-test
  ;; `false` is meaningful application data. `or` must not turn it into an
  ;; empty SSE payload when callers use this low-level framing helper.
  (is (= "data: false\n\n"
         (sse/event-frame {:data false}))))

(deftest live-event-frame-rejects-nonstring-encoder-results-test
  ;; An encoder returning nil/false silently sent an empty frame; returning a
  ;; map or number silently passed an unconfigured wire representation.
  ;; Requiring a string makes payload encoding an explicit transport boundary.
  (doseq [encoded [nil false 42 {:payload :not-encoded}]]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"SSE encoder must return a string"
         (sse/live-event-frame
          live-event
          {:encoder (constantly encoded)}))
        (str "SSE encoder must reject " (pr-str encoded)))))

(deftest nonstring-encoder-result-closes-sse-stream-test
  (let [seen-error (promise)
        started (sse/start!
                 (m/seed [live-event])
                 {:encoder (constantly nil)
                  :on-error (fn [error _] (deliver seen-error error))})]
    (try
      (let [error (deref seen-error timeout-ms ::timeout)]
        (is (instance? clojure.lang.ExceptionInfo error))
        (is (= "gesso.live SSE failed to build frame."
               (when (instance? clojure.lang.ExceptionInfo error)
                 (ex-message error))))
        (is (re-find #"SSE encoder must return a string"
                     (or (some-> error ex-cause ex-message) "")))
        (is (eventually #(sse/closed? started)))
        (is (s/closed? (:stream started))))
      (finally
        (sse/cancel! started)))))

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

(deftest source-backed-sse-cancellation-confirms-interest-release-test
  (let [src (source/create)
        started (sse/start-source! src
                                   {:subscription {:topic :request
                                                   :id "v2156-source"}} nil)]
    (try
      (is (fn? (:source-closed? started)))
      (is (fn? (:source-on-closed! started)))
      (let [closed-calls (atom 0)]
        ((:source-on-closed! started) #(swap! closed-calls inc))
        (is (false? ((:source-closed? started))))
        (is (= 1 (:subscription-count (source/stats src))))
        (sse/cancel! started)
        (is (= 1 @closed-calls)))
      (sse/cancel! started)
      (is (s/closed? (:stream started)))
      (is (true? ((:source-closed? started))))
      (is (zero? (:subscription-count (source/stats src))))
      (sse/cancel! started)
      (is (true? ((:source-closed? started))))
      (finally
        (sse/cancel! started)
        (source/close! src)))))

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

(deftest post-close-event-failure-does-not-report-a-new-error-test
  ;; close-once! publishes its terminal state before invoking :on-close.
  ;; A producer failure inside that callback must not be reported as a new
  ;; stream failure merely because cancel-running! has not yet set its flag.
  (let [event-failure (atom nil)
        errors (atom [])
        completed (atom [])
        late-error (ex-info "event failed after terminal close" {})]
    (with-redefs-fn
      {#'gesso.live.transport.sse/write-events-task
       (fn [_out _events _options]
         (fn [_success failure]
           (reset! event-failure failure)
           (fn [] nil)))}
      (fn []
        (let [started (sse/start! (never-flow)
                                  {:on-close (fn [entry]
                                               (swap! completed conj entry)
                                               (@event-failure late-error))
                                   :on-error (fn [error _entry]
                                               (swap! errors conj error))})]
          (sse/cancel! started)
          (is (sse/closed? started))
          (is (s/closed? (:stream started)))
          (is (= 1 (count @completed)))
          (is (= :cancelled (:reason (first @completed))))
          (is (empty? @errors)
              "an event failure after terminal close cannot become on-error"))))))

(deftest post-close-keepalive-failure-does-not-report-a-new-error-test
  ;; Keepalive and event error paths must agree on the terminal boundary.
  ;; If a connection closes synchronously during :on-close, a failing
  ;; keepalive still cannot change the committed shutdown classification.
  (let [keepalive-failure (atom nil)
        errors (atom [])
        completed (atom [])
        late-error (ex-info "keepalive failed after terminal close" {})]
    (with-redefs-fn
      {#'gesso.live.transport.sse/keepalive-task*
       (fn [_out _options]
         (fn [_success failure]
           (reset! keepalive-failure failure)
           (fn [] nil)))}
      (fn []
        (let [started (sse/start! (never-flow)
                                  {:keepalive-ms 1000
                                   :on-close (fn [entry]
                                               (swap! completed conj entry)
                                               (@keepalive-failure late-error))
                                   :on-error (fn [error _entry]
                                               (swap! errors conj error))})]
          (sse/cancel! started)
          (is (sse/closed? started))
          (is (s/closed? (:stream started)))
          (is (= 1 (count @completed)))
          (is (= :cancelled (:reason (first @completed))))
          (is (empty? @errors)
              "a keepalive failure after terminal close cannot become on-error"))))))

(deftest throwing-synchronous-event-start-closes-and-retires-keepalive-test
  ;; Task construction can throw before it returns a physical cancel handle.
  ;; An already-installed keepalive must not be orphaned by that exception.
  (let [boom (ex-info "event startup failed" {:phase :start})
        cancellations (atom 0)
        failures (atom [])
        closes (atom [])]
    (with-redefs-fn
      {#'gesso.live.transport.sse/keepalive-task*
       (fn [_out _options]
         (fn [_success _failure]
           (fn [] (swap! cancellations inc))))
       #'gesso.live.transport.sse/write-events-task
       (fn [_out _events _options]
         (throw boom))}
      (fn []
        (let [result (try
                       (sse/start! (never-flow)
                                   {:keepalive-ms 1000
                                    :on-error (fn [e _] (swap! failures conj e))
                                    :on-close (fn [entry] (swap! closes conj entry))})
                       (catch Exception e e))]
          (is (map? result) "start! should return its terminal response handle")
          (when (map? result)
            (is (sse/closed? result))
            (is (s/closed? (:stream result)))
            (sse/cancel! result))
          (is (= 1 @cancellations) "keepalive must be retired exactly once")
          (is (= [boom] @failures))
          (is (= [:startup-error] (mapv :reason @closes))))))))

(deftest throwing-synchronous-keepalive-start-does-not-start-events-test
  ;; A synchronous keepalive constructor failure must also fail closed,
  ;; before the event producer is started or exposed to a dead stream.
  (let [boom (ex-info "keepalive startup failed" {:phase :start})
        event-starts (atom 0)
        failures (atom [])
        closes (atom [])]
    (with-redefs-fn
      {#'gesso.live.transport.sse/keepalive-task*
       (fn [_out _options] (throw boom))
       #'gesso.live.transport.sse/write-events-task
       (fn [_out _events _options]
         (fn [_success _failure]
           (swap! event-starts inc)
           (fn [] nil)))}
      (fn []
        (let [result (try
                       (sse/start! (never-flow)
                                   {:keepalive-ms 1000
                                    :on-error (fn [e _] (swap! failures conj e))
                                    :on-close (fn [entry] (swap! closes conj entry))})
                       (catch Exception e e))]
          (is (map? result) "start! should return its terminal response handle")
          (when (map? result)
            (is (sse/closed? result))
            (is (s/closed? (:stream result)))
            (sse/cancel! result))
          (is (zero? @event-starts))
          (is (= [boom] @failures))
          (is (= [:startup-error] (mapv :reason @closes))))))))

(deftest overlapping-producer-failures-report-only-first-terminal-error-test
  ;; The first producer failure enters on-error; that application callback
  ;; synchronously delivers another producer failure before returning. The
  ;; selected terminal error must be reserved before either external hook.
  (let [event-failure (atom nil)
        keepalive-failure (atom nil)
        first-error (ex-info "event failed first" {})
        other-error (ex-info "keepalive failed while reporting event" {})
        errors (atom [])
        closes (atom [])]
    (with-redefs-fn
      {#'gesso.live.transport.sse/keepalive-task*
       (fn [_out _options]
         (fn [_success failure]
           (reset! keepalive-failure failure)
           (fn [] nil)))
       #'gesso.live.transport.sse/write-events-task
       (fn [_out _events _options]
         (fn [_success failure]
           (reset! event-failure failure)
           (fn [] nil)))}
      (fn []
        (let [started (sse/start! (never-flow)
                                  {:keepalive-ms 1000
                                   :on-error (fn [error _entry]
                                               (swap! errors conj error)
                                               (when (identical? first-error error)
                                                 (@keepalive-failure other-error)))
                                   :on-close (fn [entry] (swap! closes conj entry))})]
          (@event-failure first-error)
          (is (= [first-error] @errors)
              "the losing producer may not emit a second on-error")
          (is (= [:error] (mapv :reason @closes))
              "a reentrant loser cannot replace the first terminal reason")
          (is (identical? first-error (:error (first @closes))))
          (is (sse/closed? started))
          (is (s/closed? (:stream started)))
          (sse/cancel! started))))))

(deftest error-hook-disconnect-preserves-original-failure-reason-test
  ;; on-error is application code and may close the stream immediately.
  ;; It must not turn a transport error into a misleading disconnect outcome.
  (let [event-failure (atom nil)
        started-ref (atom nil)
        boom (ex-info "event producer failed" {})
        errors (atom [])
        closes (atom [])]
    (with-redefs-fn
      {#'gesso.live.transport.sse/write-events-task
       (fn [_out _events _options]
         (fn [_success failure]
           (reset! event-failure failure)
           (fn [] nil)))}
      (fn []
        (let [started (sse/start! (never-flow)
                                  {:on-error (fn [error _entry]
                                               (swap! errors conj error)
                                               (s/close! (:stream @started-ref)))
                                   :on-close (fn [entry] (swap! closes conj entry))})]
          (reset! started-ref started)
          (@event-failure boom)
          (is (= [boom] @errors))
          (is (= [:error] (mapv :reason @closes))
              "a callback-induced disconnect cannot steal an earlier error")
          (is (identical? boom (:error (first @closes))))
          (is (sse/closed? started))
          (is (s/closed? (:stream started)))
          (sse/cancel! started))))))

(deftest blocked-error-hook-cannot-delay-terminal-stream-cleanup-test
  ;; A user callback can block indefinitely. Once an event failure claims the
  ;; terminal result, the stream and all already-installed producers must be
  ;; closed before invoking the observational on-error hook.
  (let [boom (ex-info "event producer failed" {})
        event-failure (atom nil)
        entered (promise)
        release (promise)
        cancellations (atom [])
        closes (atom [])]
    (with-redefs-fn
      {#'gesso.live.transport.sse/write-events-task
       (fn [_out _events _options]
         (fn [_success failure]
           (reset! event-failure failure)
           (fn [] (swap! cancellations conj :events))))
       #'gesso.live.transport.sse/keepalive-task*
       (fn [_out _options]
         (fn [_success _failure]
           (fn [] (swap! cancellations conj :keepalive))))}
      (fn []
        (let [started (sse/start! (never-flow)
                                  {:keepalive-ms 1000
                                   :on-error (fn [_ _]
                                               (deliver entered true)
                                               @release)
                                   :on-close #(swap! closes conj %)})
              failure-delivery (future (@event-failure boom))]
          (try
            (is (= true (deref entered timeout-ms ::timeout)))
            (is (sse/closed? started))
            (is (s/closed? (:stream started))
                "stream must close before the blocking error hook completes")
            (is (= #{:events :keepalive} (set @cancellations))
                "both producer cancellers must run before the error hook")
            (finally
              (deliver release true)
              (is (not= ::timeout (deref failure-delivery timeout-ms ::timeout)))
              (sse/cancel! started)))
          (is (= [:error] (mapv :reason @closes)))
          (is (= [:events :keepalive] @cancellations)))))))

(deftest blocked-close-hook-cannot-delay-terminal-stream-cleanup-test
  ;; Even a normal user-requested cancellation must release physical stream
  ;; resources before calling an application-supplied on-close hook.
  (let [entered (promise)
        release (promise)
        cancellations (atom [])
        closes (atom [])]
    (with-redefs-fn
      {#'gesso.live.transport.sse/write-events-task
       (fn [_out _events _options]
         (fn [_success _failure]
           (fn [] (swap! cancellations conj :events))))
       #'gesso.live.transport.sse/keepalive-task*
       (fn [_out _options]
         (fn [_success _failure]
           (fn [] (swap! cancellations conj :keepalive))))}
      (fn []
        (let [started (sse/start! (never-flow)
                                  {:keepalive-ms 1000
                                   :on-close (fn [entry]
                                               (swap! closes conj entry)
                                               (deliver entered true)
                                               @release)})
              cancel-delivery (future (sse/cancel! started))]
          (try
            (is (= true (deref entered timeout-ms ::timeout)))
            (is (sse/closed? started))
            (is (s/closed? (:stream started))
                "stream must close before the blocking close hook completes")
            (is (= #{:events :keepalive} (set @cancellations))
                "both producer cancellers must run before the close hook")
            (finally
              (deliver release true)
              (is (not= ::timeout (deref cancel-delivery timeout-ms ::timeout)))
              (sse/cancel! started)))
          (is (= [:cancelled] (mapv :reason @closes)))
          (is (= [:events :keepalive] @cancellations)))))))

(deftest fragment-rejected-sse-options-do-not-leak-source-subscription-test
  ;; Model-backed fragments use the same eager source registration. Keep the
  ;; authorization gate intact, then reject transport options before opening
  ;; a source subscription. The model stubs isolate this startup ordering.
  (let [src (source/create)
        system {:source src :options {}}
        authorized (atom 0)]
    (try
      (with-redefs [model/fragment-scope-name (fn [_ _] :request)
                    model/fragment-scope-instance
                    (fn [_ _ _] {:topic :request :id "req-1"})
                    model/require-scope-authorized!
                    (fn [_ _ _ _] (swap! authorized inc))]
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #":keepalive-ms must be a positive integer"
             (sse/start-fragment-stream!
              system {} {} :request-panel "req-1"
              {:sse-options {:keepalive-ms -1}})))
        (is (= 1 @authorized)
            "transport validation must not bypass model authorization")
        (is (zero? (:subscription-count (source/stats src)))
            "rejected model-backed SSE startup must not retain a subscription")
        (is (zero? (:tap-count (source/stats src))))
        (is (not (source/closed? src))))
      (finally
        (source/close! src)))))

(deftest fragment-rejected-sse-headers-do-not-leak-source-subscription-test
  ;; Model-backed startup has the same late response construction boundary.
  ;; Keep authorization first, then reject malformed header containers before
  ;; allocating the authorized source subscription.
  (let [src (source/create)
        system {:source src :options {}}
        authorized (atom 0)]
    (try
      (with-redefs [model/fragment-scope-name (fn [_ _] :request)
                    model/fragment-scope-instance
                    (fn [_ _ _] {:topic :request :id "req-1"})
                    model/require-scope-authorized!
                    (fn [_ _ _ _] (swap! authorized inc))]
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #":headers must be a map"
             (sse/start-fragment-stream!
              system {} {} :request-panel "req-1"
              {:sse-options {:headers :invalid}})))
        (is (= 1 @authorized))
        (is (zero? (:subscription-count (source/stats src)))
            "invalid model SSE headers must not retain a subscription")
        (is (zero? (:tap-count (source/stats src)))))
      (finally
        (source/close! src)))))

(deftest prepare-options-rejects-invalid-http-status-test
  ;; Malformed status values are not valid Ring responses. A source-backed
  ;; stream must reject them before registering a Live interest.
  (doseq [status [nil false "200" 0 99 600 -1]]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":status must be an HTTP status integer"
         (sse/prepare-options! {:status status}))
        (str "Invalid SSE response status " (pr-str status))))
  (is (= 200 (:status (sse/prepare-options! nil))))
  (is (= 204 (:status (sse/prepare-options! {:status 204})))))

(deftest invalid-sse-http-status-does-not-register-source-interest-test
  (let [src (source/create)]
    (try
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #":status must be an HTTP status integer"
           (sse/start-source!
            src
            {:subscription {:topic :request :id "req-1"}}
            {:status nil})))
      (is (= 0 (:tap-count (source/stats src)))
          "A rejected Ring response must not leave any live source tap")
      (finally
        (source/close! src)))))

(deftest prepare-options-rejects-malformed-header-fields-test
  ;; Ring response headers must be representable as HTTP field names and
  ;; string values. Reject malformed values before a source is opened.
  (doseq [headers [{nil "value"}
                   {:keyword "value"}
                   {"bad name" "value"}
                   {"x-test" nil}
                   {"x-test" 42}
                   {"x-test" "good\r\nInjected: yes"}
                   {"x-test" (str "bad" (char 0) "value")}
                   {"set-cookie" ["a=1" nil]}
                   {"set-cookie" ["a=1" "b=2\nno"]}]]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"SSE HTTP header"
         (sse/prepare-options! {:headers headers}))
        (str "Invalid SSE response headers: " (pr-str headers))))
  (is (= {"set-cookie" ["a=1" "b=2"]
          "x-test" "ok"}
         (:headers (sse/prepare-options!
                    {:headers {"set-cookie" ["a=1" "b=2"]
                               "x-test" "ok"}})))))

(deftest invalid-sse-header-fields-do-not-register-source-interest-test
  (let [src (source/create)]
    (try
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"SSE HTTP header"
           (sse/start-source!
            src
            {:subscription {:topic :request :id "req-1"}}
            {:headers {"x-trace" "bad\r\nInjected: true"}})))
      (is (zero? (:subscription-count (source/stats src)))
          "Rejected SSE response headers cannot register a source interest")
      (finally
        (source/close! src)))))

(deftest sse-content-type-cannot-be-overridden-test
  ;; EventSource requires text/event-stream. Even a differently-cased header
  ;; name is an HTTP Content-Type override, not an independent custom header.
  (doseq [headers [{"content-type" "application/json"}
                   {"Content-Type" "text/plain"}
                   {"CONTENT-TYPE" sse/content-type}]]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"SSE Content-Type header is reserved"
         (sse/prepare-options! {:headers headers}))
        (str "SSE MIME type must not be overridden: " (pr-str headers)))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"SSE Content-Type header is reserved"
         (sse/response (s/stream) {:headers headers})))))

(deftest reserved-sse-content-type-rejected-before-source-registration-test
  (let [src (source/create)]
    (try
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"SSE Content-Type header is reserved"
           (sse/start-source!
            src
            {:subscription {:topic :request :id "req-1"}}
            {:headers {"Content-Type" "text/plain"}})))
      (is (zero? (:subscription-count (source/stats src)))
          "Invalid SSE MIME type must not allocate an interested consumer")
      (finally
        (source/close! src)))))

(deftest prepare-options-rejects-invalid-headers-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":headers must be a map"
       (sse/prepare-options! {:headers :invalid})))
  (is (nil? (:headers (sse/prepare-options! nil))))
  (is (= {"x-test" "ok"}
         (:headers (sse/prepare-options! {:headers {"x-test" "ok"}})))))

(deftest fragment-rejects-unrepresentable-keepalive-before-source-subscription-test
  ;; The model-backed entry point must reject timer values that cannot reach
  ;; the timer without accidentally retaining an authorized source interest.
  (let [src (source/create)
        system {:source src :options {}}
        authorized (atom 0)
        overflow-ms (inc (bigint Long/MAX_VALUE))]
    (try
      (with-redefs [model/fragment-scope-name (fn [_ _] :request)
                    model/fragment-scope-instance
                    (fn [_ _ _] {:topic :request :id "req-1"})
                    model/require-scope-authorized!
                    (fn [_ _ _ _] (swap! authorized inc))]
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #":keepalive-ms must be a positive integer"
             (sse/start-fragment-stream!
              system {} {} :request-panel "req-1"
              {:sse-options {:keepalive-ms overflow-ms}})))
        (is (= 1 @authorized))
        (is (zero? (:subscription-count (source/stats src)))
            "a rejected timer cannot retain a model-authorized interest")
        (is (zero? (:tap-count (source/stats src)))))
      (finally
        (source/close! src)))))

(deftest prepare-options-requires-representable-keepalive-duration-test
  ;; The API accepts Clojure integral types but the runtime scheduler has
  ;; signed 64-bit millisecond range. Retain both the upper bound and the
  ;; existing positive-integer lower-bound contract.
  (is (= Long/MAX_VALUE
         (:keepalive-ms (sse/prepare-options! {:keepalive-ms Long/MAX_VALUE}))))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":keepalive-ms must be a positive integer"
       (sse/prepare-options! {:keepalive-ms (inc (bigint Long/MAX_VALUE))})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":keepalive-ms must be a positive integer"
       (sse/prepare-options! {:keepalive-ms -1}))))

(deftest fragment-sse-synchronous-startup-failure-releases-unstarted-flow-test
  ;; A checked/authorized fragment has an eager indexed source subscription,
  ;; even when the keepalive constructor fails before consuming that flow.
  (let [src (source/create)
        system {:source src :options {}}
        authorized (atom 0)
        boom (ex-info "timer initialization failed" {:phase :start})]
    (try
      (with-redefs-fn
        {#'gesso.live.transport.sse/keepalive-task*
         (fn [_out _opts] (throw boom))
         #'gesso.live.model/fragment-scope-name (fn [_ _] :request)
         #'gesso.live.model/fragment-scope-instance
         (fn [_ _ _] {:topic :request :id "req-1"})
         #'gesso.live.model/require-scope-authorized!
         (fn [_ _ _ _] (swap! authorized inc))}
        (fn []
          (let [started (sse/start-fragment-stream!
                         system {} {} :request-panel "req-1"
                         {:sse-options {:keepalive-ms 1000}})]
            (is (= 1 @authorized))
            (is (sse/closed? started))
            (is (s/closed? (:stream started)))
            (is (zero? (:subscription-count (source/stats src)))
                "fragment SSE startup must retire its never-collected subscription")
            (is (zero? (:tap-count (source/stats src)))))))
      (finally
        (source/close! src)))))

(deftest fragment-sse-release-when-closed-before-observer-test
  ;; The model-backed SSE entry point must retain ownership until shutdown,
  ;; including when an SSE producer never observes its eager indexed flow.
  (let [src (source/create)
        system {:source src :options {}}
        authorized (atom 0)]
    (try
      (with-redefs-fn
        {#'gesso.live.transport.sse/write-events-task
         (fn [_out _flow _opts]
           (fn [_success _failure]
             (fn [] nil)))
         #'gesso.live.model/fragment-scope-name (fn [_ _] :request)
         #'gesso.live.model/fragment-scope-instance
         (fn [_ _ _] {:topic :request :id "req-1"})
         #'gesso.live.model/require-scope-authorized!
         (fn [_ _ _ _] (swap! authorized inc))}
        (fn []
          (let [started (sse/start-fragment-stream!
                         system {} {} :request-panel "req-1")]
            (is (= 1 @authorized))
            (is (= 1 (:subscription-count (source/stats src))))
            ;; Simulate downstream disconnect instead of explicit cancel!.
            (s/close! (:stream started))
            (is (sse/closed? started))
            (is (zero? (:subscription-count (source/stats src)))
                "downstream close must release unobserved authorized interest"))))
      (finally
        (source/close! src)))))
