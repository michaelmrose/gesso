(ns gesso.assurance.example.physical-single-node-aleph-writes-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.example.physical-single-node-aleph-writes :as writes]
            [manifold.stream :as stream])
  (:import [io.netty.channel ChannelDuplexHandler]
           [io.netty.channel.embedded EmbeddedChannel]
           [io.netty.buffer Unpooled]
           [io.netty.handler.codec.http DefaultHttpContent DefaultLastHttpContent]))

(deftest final-write-observation-keeps-finite-and-stream-evidence-separate
  (let [body (stream/stream)
        finite {:status 200 :headers {} :body "finite"}
        streaming {:status 200 :headers {} :body body}
        observer (writes/prepare! #(if (= "/sse" (:uri %)) streaming finite))
        ch (EmbeddedChannel.)
        p (.pipeline ch)]
    (try
      (.addLast p "request-handler" (proxy [ChannelDuplexHandler] []))
      ((:http1-pipeline-transform observer) p)
      (let [dispatch (fn [uri] ((:handler observer) {:aleph/channel ch :uri uri}))
            write! (fn [msg] (.writeAndFlush (.context p "request-handler") msg))
            stats (:stats observer)]
        (is (identical? finite (dispatch "/finite")))
        (is (identical? streaming (dispatch "/sse")))
        (is (= 1 (:finite-registered (stats))))
        (is (= 1 (:stream-registered (stats))))
        (is (= 2 (get-in (stats) [:capture :pending])))
        (is (zero? (:finite-terminal (stats))))
        (is (.isSuccess (write! (DefaultHttpContent. Unpooled/EMPTY_BUFFER))))
        (is (zero? (:finite-terminal (stats))))
        (is (.isSuccess (write! (DefaultLastHttpContent.))))
        (.runPendingTasks ch)
        (is (= 1 (:finite-terminal (stats))))
        (is (= 1 (:finite-succeeded (stats))))
        (is (zero? (:stream-final-succeeded (stats))))
        (is (.isSuccess (write! (DefaultLastHttpContent.))))
        (.runPendingTasks ch)
        (is (= 1 (:stream-final-succeeded (stats))))
        (is (= 1 (:finite-terminal (stats))))
        (is (not (stream/closed? body)))
        (is (= 2 (get-in (stats) [:capture :matched])))
        (is (zero? (get-in (stats) [:capture :pending])))
        (is (zero? (:unobserved (stats)))))
      (finally (stream/close! body) (.finishAndReleaseAll ch)))))

(deftest unobserved-response-does-not-change-original-result
  (let [response {:status 200 :headers {} :body "committed"}
        failure (ex-info "original failure" {})
        observer (writes/prepare! (fn [request]
                                    (if (= "/error" (:uri request))
                                      (throw failure) response)))
        handler (:handler observer)]
    (is (identical? response (handler {:uri "/finite"})))
    (is (= 1 (:unobserved ((:stats observer)))))
    (is (zero? (:finite-terminal ((:stats observer)))))
    (is (identical? failure (try (handler {:uri "/error"})
                                 (catch Exception error error))))
    (is (= 2 (:unobserved ((:stats observer)))))))

(deftest channel-retirement-drains-admitted-dispatch-and-closes-idle-channels
  (let [entered (promise)
        release (promise)
        calls (atom 0)
        observer (writes/prepare! (fn [_] (swap! calls inc)
                                    (deliver entered true) @release
                                    {:status 200 :headers {} :body "finite"}))
        ch (EmbeddedChannel.)
        idle (EmbeddedChannel.)
        late (EmbeddedChannel.)
        install! (fn [^EmbeddedChannel c]
                   (.addLast (.pipeline c) "request-handler" (proxy [ChannelDuplexHandler] []))
                   ((:http1-pipeline-transform observer) (.pipeline c)))
        status (:channel-status observer)]
    (try
      (install! ch)
      (install! idle)
      (is (= {:accepting? true :active 0 :owned-channels 2} (status)))
      (let [request (future ((:handler observer) {:aleph/channel ch}))
            retire (future (deref entered 3000 false) ((:retire-channels! observer)))]
        (try
          (is (= true (deref entered 3000 false)))
          (is (loop [n 0]
                (cond (not (:accepting? (status))) true
                      (= 100 n) false
                      :else (do (Thread/sleep 10) (recur (inc n))))))
          (is (= {:accepting? false :active 1 :owned-channels 2} (status)))
          (is (= 503 (:status ((:handler observer) {:aleph/channel ch}))))
          (is (= 1 @calls))
          (install! late)
          (is (.isDone (.closeFuture late)))
          (is (not (.isActive late)))
          (is (= ::blocked (deref retire 100 ::blocked)))
          (is (.isActive ch))
          (is (.isActive idle))
          (finally (deliver release true)))
        (is (= 200 (:status (deref request 3000 nil))))
        (is (not= ::timeout (deref retire 3000 ::timeout)))
        (is (.isDone (.closeFuture ch)))
        (is (.isDone (.closeFuture idle)))
        (is (= {:accepting? false :active 0 :owned-channels 0} (status)))
        ;; Closing a channel is not a successful final write.
        (is (zero? (:finite-succeeded ((:stats observer)))))
        (is (= 1 (get-in ((:stats observer)) [:capture :orphaned]))))
      (finally
        (deliver release true)
        (doseq [^EmbeddedChannel c [ch idle late]] (.finishAndReleaseAll c))))))
