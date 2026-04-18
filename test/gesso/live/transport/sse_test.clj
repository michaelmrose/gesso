(ns gesso.live.transport.sse-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gesso.live.bus :as bus]
   [gesso.live.transport.sse :as sse])
  (:import
   [java.io ByteArrayOutputStream BufferedWriter OutputStreamWriter PipedInputStream PipedOutputStream]
   [java.nio.charset StandardCharsets]
   [java.util.concurrent LinkedBlockingQueue TimeUnit]))

(defn read-until
  [body pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (with-open [rdr (clojure.java.io/reader body)]
      (let [buf (char-array 1024)
            acc (StringBuilder.)]
        (loop []
          (if (> (System/currentTimeMillis) deadline)
            (str acc)
            (let [n (.read rdr buf 0 (alength buf))]
              (cond
                (neg? n) (str acc)

                :else
                (let [chunk (String. buf 0 n)]
                  (.append acc chunk)
                  (let [s (str acc)]
                    (if (pred s)
                      s
                      (recur))))))))))))

;; Helper to wrap the new body-fn so tests can read it like an InputStream
(defn stream-from-body-fn
  [body-fn]
  (let [in (PipedInputStream. 65536)
        out (PipedOutputStream. in)]
    (future
      (try
        (body-fn out)
        (catch Throwable _ nil)
        (finally (.close out))))
    in))

(def test-matcher
  {:subscription->entries
   (fn [subscription]
     (case (:kind subscription)
       :demo-counter
       [[:demo-counter (:id subscription)]]
       []))

   :changed->entries
   (fn [_ctx changed]
     (case (:entity/type changed)
       :demo-counter
       [[:demo-counter (:entity/id changed)]]
       []))})

(defn mk-bus []
  (bus/memory-bus test-matcher))

(defn mk-event
  ([] (mk-event {}))
  ([m]
   (merge
    {:event "live-update"
     :changed {:entity/type :demo-counter
               :entity/id "global-shared-counter"
               :change/kind :updated}
     :consistency-token nil
     :data {:reason :increment}}
    m)))

(defn mk-subscription []
  {:kind :demo-counter
   :id "global-shared-counter"})

(defn mk-subscription-fn
  ([] (mk-subscription-fn (mk-subscription)))
  ([subscription]
   (fn [_ctx] subscription)))

(defn queue-poll
  [^LinkedBlockingQueue q timeout-ms]
  (.poll q timeout-ms TimeUnit/MILLISECONDS))

(defn write-stream!
  [frames]
  (let [out (ByteArrayOutputStream.)
        writer (BufferedWriter.
                (OutputStreamWriter. out StandardCharsets/UTF_8))]
    (doseq [frame frames]
      (.write writer frame))
    (.flush writer)
    (.toString out "UTF-8")))

(deftest constants-test
  (is (= "/gesso/live/stream" sse/default-path))
  (is (= "live-update" sse/default-event))
  (is (pos? sse/default-keepalive-ms)))

(deftest event-name-test
  (testing "uses explicit event"
    (is (= "custom-event"
           (sse/event-name {:event "custom-event"}))))
  (testing "defaults when missing"
    (is (= "live-update"
           (sse/event-name {:changed {:entity/type :demo-counter
                                      :entity/id "x"}})))))

(deftest event-payload-test
  (is (= {:changed {:entity/type :demo-counter
                    :entity/id "x"}
          :consistency-token [:tx 9]
          :data {:reason :updated}}
         (sse/event-payload
          {:event "live-update"
           :changed {:entity/type :demo-counter
                     :entity/id "x"}
           :consistency-token [:tx 9]
           :data {:reason :updated}}))))

(deftest encode-payload-test
  (is (= (pr-str {:x 1})
         (sse/encode-payload {:x 1}))))

(deftest event->sse-test
  (let [frame (sse/event->sse (mk-event))]
    (is (str/includes? frame "event: live-update\n"))
    (is (str/includes? frame "data: "))
    (is (str/ends-with? frame "\n\n"))))

(deftest keepalive-frame-test
  (is (= ": keepalive\n\n"
         (sse/keepalive-frame))))

(deftest stream-url-test
  (is (= "/gesso/live/stream?subscription=shared-counter"
         (sse/stream-url "shared-counter"))))

(deftest build-subscriber-test
  (let [queue (LinkedBlockingQueue.)
        sub (sse/build-subscriber
             {:subscription (mk-subscription)
              :queue queue
              :meta {:transport :sse}})
        event (mk-event)]
    (testing "shape"
      (is (string? (:subscriber/id sub)))
      (is (= (mk-subscription) (:subscription sub)))
      (is (fn? (:send! sub)))
      (is (= {:transport :sse} (:meta sub))))
    (testing "send enqueues SSE frame"
      ((:send! sub) event)
      (let [frame (queue-poll queue 100)]
        (is (string? frame))
        (is (str/includes? frame "event: live-update"))
        (is (str/includes? frame "global-shared-counter"))))))

(deftest handler-validation-test
  (testing "missing bus"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Missing :gesso.live/bus"
         (sse/handler
          {:ctx {}
           :subscription-fn (mk-subscription-fn)}))))
  (testing "missing subscription"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Missing or invalid live subscription"
         (sse/handler
          {:ctx {:gesso.live/bus (mk-bus)}
           :subscription-fn (mk-subscription-fn nil)})))))

(deftest handler-response-shape-test
  (let [resp (sse/handler
              {:ctx {:gesso.live/bus (mk-bus)}
               :subscription-fn (mk-subscription-fn)})]
    (is (= 200 (:status resp)))
    (is (= "text/event-stream; charset=utf-8"
           (get-in resp [:headers "content-type"])))
    (is (= "no-cache, no-transform"
           (get-in resp [:headers "cache-control"])))
    (is (= "keep-alive"
           (get-in resp [:headers "connection"])))
    (is (some? (:body resp)))))

(deftest response-subscribes-while-open-test
  (let [live-bus (mk-bus)
        queue (LinkedBlockingQueue.)
        subscriber (sse/build-subscriber
                    {:subscription (mk-subscription)
                     :queue queue
                     :meta {:transport :sse}})
        resp (sse/response
              {:live-bus live-bus
               :subscriber subscriber
               :queue queue
               :keepalive-ms 25})
        body (stream-from-body-fn (:body resp))]
    (try
      (Thread/sleep 50)
      (is (contains? (bus/subscribers-snapshot live-bus)
                     (:subscriber/id subscriber)))
      (finally
        (.close body)))))

(deftest queue-send-fn-test
  (let [queue (LinkedBlockingQueue.)
        send! ((fn [q]
                 (fn [event]
                   (.offer q (sse/event->sse event))))
               queue)]
    (send! (mk-event))
    (let [frame (queue-poll queue 100)]
      (is (string? frame))
      (is (str/includes? frame "event: live-update")))))

(deftest matching-event-reaches-subscriber-queue-test
  (let [live-bus (mk-bus)
        queue (LinkedBlockingQueue.)
        subscriber (sse/build-subscriber
                    {:subscription (mk-subscription)
                     :queue queue
                     :meta {:transport :sse}})]
    (bus/subscribe! live-bus subscriber)
    (bus/publish! live-bus (mk-event))
    (let [frame (queue-poll queue 100)]
      (is (string? frame))
      (is (str/includes? frame "event: live-update"))
      (is (str/includes? frame "global-shared-counter")))))

(deftest non-matching-event-does-not-reach-subscriber-queue-test
  (let [live-bus (mk-bus)
        queue (LinkedBlockingQueue.)
        subscriber (sse/build-subscriber
                    {:subscription (mk-subscription)
                     :queue queue
                     :meta {:transport :sse}})]
    (bus/subscribe! live-bus subscriber)
    (bus/publish! live-bus
                  (mk-event {:changed {:entity/type :other
                                       :entity/id "nope"
                                       :change/kind :updated}}))
    (is (nil? (queue-poll queue 100)))))

(deftest multiple-published-events-reach-queue-in-order-test
  (let [live-bus (mk-bus)
        queue (LinkedBlockingQueue.)
        subscriber (sse/build-subscriber
                    {:subscription (mk-subscription)
                     :queue queue
                     :meta {:transport :sse}})
        first-event (mk-event {:data {:reason :first}})
        second-event (mk-event {:data {:reason :second}})]
    (bus/subscribe! live-bus subscriber)
    (bus/publish! live-bus first-event)
    (bus/publish! live-bus second-event)
    (let [frame-1 (queue-poll queue 100)
          frame-2 (queue-poll queue 100)]
      (is (string? frame-1))
      (is (string? frame-2))
      (is (str/includes? frame-1 ":first"))
      (is (str/includes? frame-2 ":second")))))

(deftest write-frame-smoke-test
  (let [text (write-stream! [(sse/keepalive-frame)
                             (sse/event->sse (mk-event))])]
    (is (str/includes? text ": keepalive"))
    (is (str/includes? text "event: live-update"))
    (is (str/includes? text "global-shared-counter"))))

(defn body-reader-future
  [body]
  (future
    (with-open [rdr (clojure.java.io/reader body)]
      (let [buf (char-array 4096)
            n (.read rdr buf)]
        (when (pos? n)
          (String. buf 0 n))))))

(defn await-future
  [f timeout-ms]
  (deref f timeout-ms ::timeout))

(deftest response-body-stays-open-long-enough-to-emit-keepalive-test
  (let [live-bus (mk-bus)
        resp (sse/handler
              {:ctx {:gesso.live/bus live-bus}
               :subscription-fn (mk-subscription-fn)
               :keepalive-ms 50})
        body (stream-from-body-fn (:body resp))
        read-fut (body-reader-future body)]
    (try
      (let [text (await-future read-fut 1000)]
        (is (not= ::timeout text))
        (is (string? text))
        (is (str/includes? text ": keepalive")))
      (finally
        (.close body)))))

(deftest response-body-can-observe-published-event-while-open-test
  (let [live-bus (mk-bus)
        resp (sse/handler
              {:ctx {:gesso.live/bus live-bus}
               :subscription-fn (mk-subscription-fn)
               :keepalive-ms 1000})
        body (stream-from-body-fn (:body resp))]
    (try
      (Thread/sleep 100)
      (bus/publish! live-bus (mk-event {:data {:reason :transport-check}}))
      (let [text (read-until body
                             #(str/includes? % "event: live-update")
                             1500)]
        (is (str/includes? text "event: live-update"))
        (is (str/includes? text ":transport-check")))
      (finally
        (.close body)))))

(deftest response-close-removes-subscriber-test
  (let [live-bus (mk-bus)
        resp (sse/handler
              {:ctx {:gesso.live/bus live-bus}
               :subscription-fn (mk-subscription-fn)
               :keepalive-ms 50})
        body (stream-from-body-fn (:body resp))]
    (try
      (Thread/sleep 100)
      (is (pos? (count (bus/subscribers-snapshot live-bus))))
      (finally
        (.close body)))
    (Thread/sleep 100)
    (is (= 0 (count (bus/subscribers-snapshot live-bus))))))

(deftest repeated-open-close-does-not-leak-subscribers-test
  (let [live-bus (mk-bus)]
    (dotimes [_ 5]
      (let [resp (sse/handler
                  {:ctx {:gesso.live/bus live-bus}
                   :subscription-fn (mk-subscription-fn)
                   :keepalive-ms 25})
            body (stream-from-body-fn (:body resp))]
        (Thread/sleep 50)
        (.close body)
        (Thread/sleep 50)))
    (is (= 0 (count (bus/subscribers-snapshot live-bus))))))
