(ns gesso.live.outbox.http-aleph-pipeline-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.live.outbox.http-aleph-pipeline :as capture]
            [gesso.live.outbox.http-netty-terminal :as terminal]
            [gesso.live.outbox.http-listener :as listener])
  (:import [io.netty.channel ChannelDuplexHandler ChannelHandlerContext]
           [io.netty.channel.embedded EmbeddedChannel]
           [io.netty.buffer Unpooled]
           [io.netty.handler.codec.http DefaultHttpContent DefaultLastHttpContent]
           [io.netty.handler.codec.http2 DefaultHttp2DataFrame
            DefaultHttp2HeadersFrame DefaultHttp2Headers]))

(defn- error-reason [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- channel [kind capture]
  (let [ch (EmbeddedChannel.)
        pipeline (.pipeline ch)
        handler (if (= kind :http1) "request-handler" "handler")]
    (.addLast pipeline handler (proxy [ChannelDuplexHandler] []))
    ((if (= kind :http1)
       (:http1-pipeline-transform capture)
       (:http2-stream-pipeline-transform capture)) pipeline)
    (is (not (nil? (.context pipeline (if (= kind :http1)
                                       "gesso-final-http1"
                                       "gesso-final-http2")))))
    ch))

(defn- write-as-aleph! [ch kind msg]
  ;; Aleph 0.9.11 writes from request-handler/handler context, so only
  ;; outbound handlers preceding that context can observe its writes.
  (.writeAndFlush (.context (.pipeline ch)
                            (if (= kind :http1) "request-handler" "handler"))
                  msg))

(deftest http1-final-write-is-associated-with-registered-response-test
  (let [capture (capture/prepare!)
        ch (channel :http1 capture)
        calls (atom [])
        bridge (terminal/prepare! #(swap! calls conj :terminal))]
    (try
      (is (= :registered ((:register-response! capture) ch bridge)))
      (is (.isSuccess (write-as-aleph! ch :http1
                                     (DefaultHttpContent. Unpooled/EMPTY_BUFFER))))
      ((:producer-ceased! bridge))
      (is (= :unregistered (:write-outcome ((:status bridge)))))
      (is (empty? @calls))
      (is (.isSuccess (write-as-aleph! ch :http1 (DefaultLastHttpContent.))))
      (.runPendingTasks ch)
      (is (= :succeeded (:write-outcome ((:status bridge)))))
      (is (= [:terminal] @calls))
      (is (= 0 (:pending ((:stats capture)))))
      (is (= 1 (:matched ((:stats capture)))))
      (finally (.finishAndReleaseAll ch)))))

(deftest http1-pipelined-requests-are-matched-in-response-order-test
  (let [capture (capture/prepare! {:max-pending-per-channel 2})
        ch (channel :http1 capture)
        called (atom [])
        a (terminal/prepare! #(swap! called conj :a))
        b (terminal/prepare! #(swap! called conj :b))]
    (try
      ((:register-response! capture) ch a)
      ((:register-response! capture) ch b)
      (is (= :pending-channel-capacity
             (error-reason #((:register-response! capture)
                             ch (terminal/prepare! (fn [] nil))))))
      ((:producer-ceased! a))
      ((:producer-ceased! b))
      (is (.isSuccess (write-as-aleph! ch :http1 (DefaultLastHttpContent.))))
      (.runPendingTasks ch)
      (is (= [:a] @called))
      (is (= 1 (:pending ((:stats capture)))))
      (is (.isSuccess (write-as-aleph! ch :http1 (DefaultLastHttpContent.))))
      (.runPendingTasks ch)
      (is (= [:a :b] @called))
      (is (= 0 (:pending ((:stats capture)))))
      (finally (.finishAndReleaseAll ch)))))

(deftest http2-stream-end-frames-are-independent-test
  (let [capture (capture/prepare!)
        a (channel :http2 capture)
        b (channel :http2 capture)
        finished (atom [])
        ba (terminal/prepare! #(swap! finished conj :a))
        bb (terminal/prepare! #(swap! finished conj :b))]
    (try
      ((:register-response! capture) a ba)
      ((:register-response! capture) b bb)
      (is (.isSuccess (write-as-aleph! a :http2
                                     (DefaultHttp2DataFrame.
                                      Unpooled/EMPTY_BUFFER false))))
      ((:producer-ceased! ba))
      ((:producer-ceased! bb))
      (is (= [] @finished))
      (is (.isSuccess (write-as-aleph! b :http2
                                     (DefaultHttp2HeadersFrame.
                                      (DefaultHttp2Headers.) true))))
      (.runPendingTasks b)
      (is (= [:b] @finished))
      (is (.isSuccess (write-as-aleph! a :http2
                                     (DefaultHttp2DataFrame.
                                      Unpooled/EMPTY_BUFFER true))))
      (.runPendingTasks a)
      (is (= [:b :a] @finished))
      (is (= 2 (:matched ((:stats capture)))))
      (finally (.finishAndReleaseAll a)
               (.finishAndReleaseAll b)))))

(deftest channel-inactive-retains-unknown-listener-obligations-test
  (let [capture (capture/prepare!)
        ch (channel :http1 capture)
        called (atom 0)
        bridge (terminal/prepare! #(swap! called inc))]
    ((:register-response! capture) ch bridge)
    ((:producer-ceased! bridge))
    (is (= 1 (:pending ((:stats capture)))))
    (.finishAndReleaseAll ch)
    (is (= 0 (:pending ((:stats capture)))))
    (is (= 1 (:orphaned ((:stats capture)))))
    (is (= 0 @called))
    (is (= :unregistered (:write-outcome ((:status bridge)))))))

(deftest unmatched-aleph-response-is-passed-through-without-forged-evidence-test
  (let [capture (capture/prepare!)
        ch (channel :http1 capture)]
    (try
      (is (.isSuccess (write-as-aleph! ch :http1 (DefaultLastHttpContent.))))
      (.runPendingTasks ch)
      (is (= 1 (:unmatched ((:stats capture)))))
      (is (= 0 (:matched ((:stats capture)))))
      (finally (.finishAndReleaseAll ch)))))

(deftest pipeline-capture-validates-scopes-and-configuration-test
  (is (= :invalid-options
         (error-reason #(capture/prepare! {:unexpected true}))))
  (is (= :invalid-pending-limit
         (error-reason #(capture/prepare! {:max-pending-per-channel 0}))))
  (let [capture (capture/prepare!)
        ch (EmbeddedChannel.)]
    (try
      (is (= :missing-aleph-handler
             (error-reason #((:http1-pipeline-transform capture)
                             (.pipeline ch)))))
      (is (= :invalid-channel
             (error-reason #((:register-response! capture)
                             nil (terminal/prepare! (fn [] nil))))))
      (is (= :invalid-terminal-bridge
             (error-reason #((:register-response! capture) ch {}))))
      (finally (.finishAndReleaseAll ch)))))

(deftest aleph-style-pipeline-completion-gates-managed-listener-drain-test
  ;; Full ownership join: a Ring-style dispatch returns at once, the outbound
  ;; final-response promise subsequently completes, and only the separate
  ;; producer-quiescence signal permits provider-safe listener drain.
  (let [capture (capture/prepare!)
        ch (channel :http1 capture)
        bridge (atom nil)
        owner (listener/prepare!
               {:dispatch-async!
                (fn [channel done!]
                  (let [terminal (terminal/prepare! done!)]
                    (reset! bridge terminal)
                    ((:register-response! capture) channel terminal)
                    :ring-returned))
                :on-rejected! (constantly :refused)
                :stop-accept! (constantly {:outcome :stopped})})]
    (try
      (is (= :ring-returned ((:handle! owner) ch)))
      (is (= 1 ((:active-count owner))))
      (is (.isSuccess (write-as-aleph! ch :http1
                                     (DefaultLastHttpContent.))))
      (.runPendingTasks ch)
      (is (= :succeeded (:write-outcome ((:status @bridge)))))
      (is (= {:outcome :timed-out} ((:stop! owner) 0))
          "Even a final Netty write cannot prove producer quiescence")
      (is (false? ((:drained? owner))))
      ((:producer-ceased! @bridge))
      (is (= {:outcome :drained} ((:stop! owner) 0)))
      (is (zero? ((:active-count owner))))
      (finally (.finishAndReleaseAll ch)))))
