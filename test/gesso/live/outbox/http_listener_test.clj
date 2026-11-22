(ns gesso.live.outbox.http-listener-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.live.outbox.http :as http]
            [gesso.live.outbox.http-listener :as listener])
  (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.net InetSocketAddress URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent Executors TimeUnit]))

(defn- reason [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- wait-until [pred]
  (loop [n 0]
    (cond (pred) true
          (>= n 200) false
          :else (do (Thread/sleep 10) (recur (inc n))))))

(deftest listener-requires-physical-acknowledgement-and-tracks-writing-test
  (let [entered (promise)
        release (promise)
        stops (atom 0)
        owner (listener/prepare!
               {:dispatch! (fn [_] (deliver entered true) @release :written)
                :on-rejected! (constantly :refused)
                :stop-accept! (fn [] (swap! stops inc) {:outcome :stopped})})
        request (future ((:handle! owner) :request))]
    (try
      (is (= true (deref entered 2000 ::timeout)))
      (is (= 1 ((:active-count owner))))
      (is (= {:outcome :timed-out} ((:stop! owner) 0)))
      (is (= :stopped ((:status owner))))
      (is (false? ((:drained? owner))))
      (is (= :refused ((:handle! owner) :late)))
      (is (= 1 ((:active-count owner))))
      (is (= {:outcome :timed-out} ((:stop! owner) 0)))
      (is (= 1 @stops) "Repeated stop must not stop socket twice")
      (deliver release true)
      (is (= :written (deref request 2000 ::timeout)))
      (is (= {:outcome :drained} ((:stop! owner) 0)))
      (is (zero? ((:active-count owner))))
      (is (true? ((:drained? owner))))
      (finally (deliver release true) (future-cancel request)))))

(deftest unconfirmed-physical-listener-stop-never-claims-drain-test
  (doseq [stop-result [nil {:outcome :timed-out} {:outcome :drained}]]
    (let [calls (atom 0)
          owner (listener/prepare!
                 {:dispatch! (constantly nil)
                  :on-rejected! (constantly nil)
                  :stop-accept! (fn [] (swap! calls inc) stop-result)})]
      (is (= :unconfirmed-physical-listener-stop
             (reason #((:stop! owner) 0))))
      (is (= :stop-unknown ((:status owner))))
      (is (false? ((:drained? owner))))
      (is (= :physical-listener-stop-unknown
             (reason #((:stop! owner) 0))))
      (is (= 1 @calls))))
  (is (= :invalid-listener-plan (reason #(listener/prepare! {}))))
  (let [owner (listener/prepare!
               {:dispatch! (constantly nil)
                :on-rejected! (constantly nil)
                :stop-accept! (constantly {:outcome :stopped})})]
    (is (= :invalid-listener-drain-timeout (reason #((:stop! owner) -1))))
    (is (= :open ((:status owner))))
    (is (= {:outcome :drained} ((:stop! owner) 0)))))

(deftest real-http-listener-blocked-ordinary-writer-fences-provider-stop-test
  ;; The guard sits OUTSIDE Ring at the actual HttpServer dispatch boundary.
  ;; A response writer that is still active must block provider teardown,
  ;; even though the managed SSE route itself has no active subscriptions.
  (let [entered (promise)
        release (promise)
        stopped-providers (atom 0)
        stopped-sockets (atom 0)
        server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
        executor (Executors/newCachedThreadPool)
        owner (listener/prepare!
               {:dispatch! (fn [^HttpExchange exchange]
                             (deliver entered true)
                             @release
                             (try
                               (let [bytes (.getBytes "done" StandardCharsets/UTF_8)]
                                 (.sendResponseHeaders exchange 200 (alength bytes))
                                 (with-open [out (.getResponseBody exchange)]
                                   (.write out bytes)))
                               (catch java.io.IOException _ nil)
                               (finally (.close exchange))))
                :on-rejected! (fn [^HttpExchange exchange] (.close exchange))
                :stop-accept! (fn []
                                (.stop server 0)
                                (swap! stopped-sockets inc)
                                {:outcome :stopped})})
        fake-route {:close! (constantly :closed)
                    :await-drained! (constantly {:outcome :drained})}
        shutdown (http/prepare-managed-http-shutdown
                  {:route fake-route
                   :stop-listener! (fn [] ((:stop! owner) 0))
                   :stop-host! (fn [] (swap! stopped-providers inc)
                                 {:outcome :stopped})
                   :drain-timeout-ms 0})]
    (.setExecutor server executor)
    (.createContext server "/" (reify HttpHandler
                                  (handle [_ exchange]
                                    ((:handle! owner) exchange))))
    (.start server)
    (let [url (URI/create
               (str "http://127.0.0.1:" (.getPort (.getAddress server)) "/held"))
          client (HttpClient/newHttpClient)
          request (.build (HttpRequest/newBuilder url))
          response (.sendAsync client request (HttpResponse$BodyHandlers/ofString))]
      (try
        (is (= true (deref entered 3000 ::timeout)))
        (is (= 1 ((:active-count owner))))
        (is (= {:outcome :listener-timed-out} ((:stop! shutdown))))
        (is (= 1 @stopped-sockets))
        (is (= 0 @stopped-providers)
            "A real blocked ordinary HTTP writer prevents host provider stop")
        (is (false? ((:drained? owner))))
        (is (= :ready ((:status shutdown))))
        (deliver release true)
        (is (wait-until #((:drained? owner))))
        (is (= :stopped (:outcome ((:stop! shutdown)))))
        (is (= 1 @stopped-providers))
        (is (= 1 @stopped-sockets)
            "Socket shutdown must not repeat across drain retries")
        (is (= :stopped (:outcome ((:stop! shutdown)))))
        (is (= 1 @stopped-providers)
            "Provider stop is exactly once after physical handler drain")
        (finally
          (deliver release true)
          (.cancel response true)
          (.stop server 0)
          (.shutdownNow executor))))))

(deftest failed-physical-writer-still-retires-its-listener-obligation-test
  ;; A failed response write must never be interpreted as a successful
  ;; delivery, but the writer has ceased using the provider and its own
  ;; listener obligation must retire without leaking the drain count.
  (let [started (promise)
        release (promise)
        failure (ex-info "socket write failed" {:test :v2245})
        owner (listener/prepare!
               {:dispatch! (fn [_] (deliver started true) @release
                             (throw failure))
                :on-rejected! (constantly :closed)
                :stop-accept! (constantly {:outcome :stopped})})
        worker (future (try ((:handle! owner) :exchange)
                            (catch Throwable e e)))]
    (try
      (is (= true (deref started 2000 ::timeout)))
      (is (= 1 ((:active-count owner))))
      (is (= {:outcome :timed-out} ((:stop! owner) 0)))
      (deliver release true)
      (is (identical? failure (deref worker 2000 ::timeout)))
      (is (wait-until #((:drained? owner))))
      (is (zero? ((:active-count owner))))
      (is (= {:outcome :drained} ((:stop! owner) 0)))
      (finally (deliver release true) (future-cancel worker)))))
