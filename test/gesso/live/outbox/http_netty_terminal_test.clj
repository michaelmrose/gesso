(ns gesso.live.outbox.http-netty-terminal-test
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.live.outbox.http-listener :as listener]
            [gesso.live.outbox.http-netty-terminal :as netty-terminal])
  (:import [io.netty.buffer Unpooled]
           [io.netty.handler.codec.http DefaultHttpContent DefaultLastHttpContent]
           [io.netty.handler.codec.http2 DefaultHttp2DataFrame]
           [io.netty.channel.embedded EmbeddedChannel]
           [java.util.concurrent CountDownLatch]))

(defn- reason [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest successful-netty-write-with-live-producer-must-not-drain-test
  (let [channel (EmbeddedChannel.)
        terminal-count (atom 0)
        done (netty-terminal/prepare! #(swap! terminal-count inc))
        promise (.newPromise channel)]
    (try
      (is (= :watching ((:watch-final-write! done) promise)))
      (.setSuccess promise)
      (.runPendingTasks channel)
      (is (= :succeeded (:write-outcome ((:status done)))))
      (is (zero? @terminal-count)
          "Netty write completion cannot release a live source/continuation")
      (is (false? (:producer-ceased? ((:status done)))))
      ((:producer-ceased! done))
      (is (= 1 @terminal-count))
      (is (true? (:terminal? ((:status done)))))
      ((:producer-ceased! done))
      ((:writer-aborted! done))
      (is (= 1 @terminal-count))
      (finally (.finishAndReleaseAll channel)))))

(deftest producer-close-before-final-write-must-not-drain-test
  (let [channel (EmbeddedChannel.)
        called (atom 0)
        done (netty-terminal/prepare! #(swap! called inc))
        promise (.newPromise channel)]
    (try
      ((:producer-ceased! done))
      (is (zero? @called))
      ((:watch-final-write! done) promise)
      (is (= :pending (:write-outcome ((:status done)))))
      (is (zero? @called))
      (.setSuccess promise)
      (.runPendingTasks channel)
      (is (= :succeeded (:write-outcome ((:status done)))))
      (is (= 1 @called))
      (finally (.finishAndReleaseAll channel)))))

(deftest failed-netty-final-write-retains-outcome-until-producer-ceases-test
  (let [channel (EmbeddedChannel.)
        count (atom 0)
        done (netty-terminal/prepare! #(swap! count inc))
        failed (ex-info "netty outbound write failed" {:test :v2248})
        promise (.newPromise channel)]
    (try
      ((:watch-final-write! done) promise)
      (.setFailure promise failed)
      (.runPendingTasks channel)
      (is (zero? @count))
      (is (= :failed (:write-outcome ((:status done)))))
      (is (identical? failed (:cause ((:status done)))))
      ((:producer-ceased! done))
      (is (= 1 @count))
      (is (= :failed (:write-outcome ((:status done)))))
      (finally (.finishAndReleaseAll channel)))))

(deftest aborted-response-also-requires-producer-quiescence-test
  (let [channel (EmbeddedChannel.)
        promise (.newPromise channel)
        count (atom 0)
        done (netty-terminal/prepare! #(swap! count inc))]
    (is (= :unregistered (:write-outcome ((:status done)))))
    ((:writer-aborted! done))
    (is (= :aborted (:write-outcome ((:status done)))))
    (is (zero? @count))
    (is (= :final-write-after-abort
           (reason #((:watch-final-write! done) promise))))
    ((:producer-ceased! done))
    (is (= 1 @count))
    ((:writer-aborted! done))
    (is (= 1 @count))
    (.finishAndReleaseAll channel)))

(deftest writer-bridge-fences-real-listener-shutdown-test
  ;; Exercises the actual listener guard at its asynchronous boundary using
  ;; a genuine Netty ChannelFuture: both observations are required before
  ;; an otherwise idle host could be safely retired.
  (let [channel (EmbeddedChannel.)
        write-promise (.newPromise channel)
        evidence (atom nil)
        providers (atom 0)
        guard (listener/prepare!
               {:dispatch-async!
                (fn [_ terminal!]
                  (let [bridge (netty-terminal/prepare! terminal!)]
                    (reset! evidence bridge)
                    ((:watch-final-write! bridge) write-promise)
                    :handler-returned))
                :max-in-flight 1
                :on-rejected! (constantly :no-capacity)
                :stop-accept! (constantly {:outcome :stopped})})]
    (try
      (is (= :handler-returned ((:handle! guard) :exchange)))
      (is (= 1 ((:active-count guard))))
      (is (= :no-capacity ((:handle! guard) :extra)))
      (.setSuccess write-promise)
      (.runPendingTasks channel)
      (is (= :succeeded (:write-outcome ((:status @evidence)))))
      (is (= {:outcome :timed-out} ((:stop! guard) 0)))
      (is (false? ((:drained? guard))))
      (is (= 1 ((:active-count guard))))
      ((:producer-ceased! @evidence))
      (is (= {:outcome :drained} ((:stop! guard) 0)))
      (is (zero? ((:active-count guard))))
      (is (zero? ((:uncertain-count guard))))
      (finally (.finishAndReleaseAll channel)))))

(deftest concurrent-evidence-still-sends-one-terminal-notification-test
  (let [channel (EmbeddedChannel.)
        calls (atom 0)
        done (netty-terminal/prepare! #(swap! calls inc))
        promise (.newPromise channel)
        start (CountDownLatch. 1)]
    (try
      ((:watch-final-write! done) promise)
      (let [a (future (.await start) ((:producer-ceased! done)))
            b (future (.await start) (.setSuccess promise) (.runPendingTasks channel))]
        (.countDown start)
        (is (not= :timeout (deref a 3000 :timeout)))
        (is (not= :timeout (deref b 3000 :timeout)))
        (is (= 1 @calls)))
      (finally (.finishAndReleaseAll channel)))))

(deftest evidence-must-not-be-forged-by-invalid-or-duplicate-watches-test
  (let [channel (EmbeddedChannel.)
        count (atom 0)
        evidence (netty-terminal/prepare! #(swap! count inc))
        promise (.newPromise channel)]
    (try
      (is (= :invalid-terminal-callback
             (reason #(netty-terminal/prepare! nil))))
      (is (= :invalid-final-write-future
             (reason #((:watch-final-write! evidence) nil))))
      (is (= :watching ((:watch-final-write! evidence) promise)))
      (is (= :duplicate-final-write-future
             (reason #((:watch-final-write! evidence) promise))))
      ((:producer-ceased! evidence))
      (is (zero? @count))
      (.setSuccess promise)
      (.runPendingTasks channel)
      (is (= 1 @count))
      (finally (.finishAndReleaseAll channel)))))

(deftest actual-http1-end-response-write-future-needs-producer-quiescence-test
  ;; The intermediate HttpContent write may succeed, but cannot constitute
  ;; the final response disposition. Only the LastHttpContent write future
  ;; and completed provider-using continuations satisfy the lifecycle gate.
  (let [channel (EmbeddedChannel.)
        calls (atom 0)
        terminal (netty-terminal/prepare! #(swap! calls inc))]
    (try
      (let [intermediate (.writeAndFlush channel (DefaultHttpContent. Unpooled/EMPTY_BUFFER))]
        (is (.isSuccess intermediate))
        ((:producer-ceased! terminal))
        (is (zero? @calls))
        (let [last (.writeAndFlush channel (DefaultLastHttpContent.))]
          (is (.isSuccess last))
          ((:watch-final-write! terminal) last)
          (.runPendingTasks channel)
          (is (= :succeeded (:write-outcome ((:status terminal)))))
          (is (= 1 @calls))))
      (finally (.finishAndReleaseAll channel)))))

(deftest actual-http2-endstream-frame-future-needs-producer-quiescence-test
  ;; HTTP/2 uses per-stream endStream disposition rather than HTTP/1's
  ;; LastHttpContent. The Netty completion contract is the same, without
  ;; treating a successful intermediate DATA frame as response completion.
  (let [channel (EmbeddedChannel.)
        calls (atom 0)
        terminal (netty-terminal/prepare! #(swap! calls inc))]
    (try
      (let [intermediate (DefaultHttp2DataFrame. Unpooled/EMPTY_BUFFER false)
            final-frame (DefaultHttp2DataFrame. Unpooled/EMPTY_BUFFER true)
            before (.writeAndFlush channel intermediate)]
        (is (false? (.isEndStream intermediate)))
        (is (.isSuccess before))
        (is (zero? @calls))
        (let [last (.writeAndFlush channel final-frame)]
          (is (.isEndStream final-frame))
          (is (.isSuccess last))
          ((:watch-final-write! terminal) last)
          (.runPendingTasks channel)
          (is (= :succeeded (:write-outcome ((:status terminal)))))
          (is (zero? @calls)
              "Netty final-frame acceptance alone isn't provider-safe drain")
          ((:producer-ceased! terminal))
          (is (= 1 @calls))))
      (finally (.finishAndReleaseAll channel)))))
