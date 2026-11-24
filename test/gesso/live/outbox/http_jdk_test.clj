(ns gesso.live.outbox.http-jdk-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.live.outbox.http :as http]
            [gesso.live.outbox.http-jdk :as jdk]
            [manifold.stream :as stream])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.nio.charset StandardCharsets]))

(defn- eventual [pred]
  (loop [tries 0]
    (cond (pred) true
          (>= tries 150) false
          :else (do (Thread/sleep 20) (recur (inc tries))))))

(defn- request [base path]
  (.build (HttpRequest/newBuilder (URI/create (str base path)))))

(defn- cause [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest physical-jdk-dispatch-serves-real-http-and-drains-test
  (let [observed (atom [])
        adapter (jdk/start!
                 (fn [req]
                   (swap! observed conj
                          (select-keys req [:uri :query-string :request-method
                                            :server-port]))
                   {:status 200 :headers {"content-type" "text/plain"}
                    :body "actual bytes"})
                 {})
        client (HttpClient/newHttpClient)]
    (try
      (let [response (.send client (request (:base-url adapter) "/hello?a=1")
                            (HttpResponse$BodyHandlers/ofString))]
        (is (= 200 (.statusCode response)))
        (is (= "actual bytes" (.body response)))
        (is (= "text/plain" (first (.allValues (.headers response) "content-type"))))
        (is (= [{:uri "/hello" :query-string "a=1" :request-method :get
                 :server-port (:port adapter)}] @observed)))
      (is (= 0 ((:active-count adapter))))
      (is (= {:outcome :drained} ((:stop! adapter) 1000)))
      (is ((:drained? adapter)))
      (is (= :stopped ((:status adapter))))
      (is (= {:outcome :drained} ((:stop! adapter) 0)))
      (finally ((:stop! adapter) 1000)))))

(deftest physical-jdk-handler-write-fences-provider-shutdown-test
  (let [entered (promise)
        release (promise)
        providers (atom 0)
        route-close (atom 0)
        adapter (jdk/start!
                 (fn [_]
                   (deliver entered true)
                   @release
                   {:status 200 :headers {} :body "late"})
                 {:drain-timeout-ms 0})
        client (HttpClient/newHttpClient)
        response (.sendAsync client (request (:base-url adapter) "/held")
                             (HttpResponse$BodyHandlers/ofString))
        route {:close! (fn [] (swap! route-close inc) :closed)
               :await-drained! (fn [_] {:outcome :drained})}
        shutdown (http/prepare-managed-http-shutdown
                  {:route route :stop-listener! (:stop-listener! adapter)
                   :stop-host! (fn [] (swap! providers inc) {:outcome :stopped})
                   :drain-timeout-ms 0})]
    (try
      (is (= true (deref entered 3000 ::timeout)))
      (is (= 1 ((:active-count adapter))))
      (is (= {:outcome :listener-timed-out} ((:stop! shutdown))))
      (is (= 1 @route-close))
      (is (zero? @providers) "Physical request still owns host resources")
      (is (false? ((:drained? adapter))))
      (deliver release true)
      (is (eventual #((:drained? adapter))))
      (is (= :stopped (:outcome ((:stop! shutdown)))))
      (is (= 1 @providers))
      (is (= :stopped (:outcome ((:stop! shutdown)))))
      (is (= 1 @providers))
      (finally
        (deliver release true)
        (.cancel response true)
        ((:stop! adapter) 2000)))))

(deftest physical-jdk-stream-remains-active-until-response-source-closes-test
  (let [stream-body (stream/stream 4)
        adapter (jdk/start!
                 (fn [_] {:status 200
                          :headers {"content-type" "text/event-stream"}
                          :body stream-body})
                 {})
        client (HttpClient/newHttpClient)]
    (try
      (let [response (.send client (request (:base-url adapter) "/sse")
                            (HttpResponse$BodyHandlers/ofInputStream))]
        (with-open [input (.body response)]
          (is (= 200 (.statusCode response)))
          (is (= "text/event-stream"
                 (first (.allValues (.headers response) "content-type"))))
          (is (eventual #(= 1 ((:active-count adapter)))))
          (is (true? (deref (stream/put! stream-body "event: live-update\ndata: one\n\n")
                            2000 false)))
          (let [buf (byte-array 30)
                n (.read input buf)]
            (is (pos? n))
            (is (= "event: live-update\ndata: one\n\n"
                   (String. buf 0 n StandardCharsets/UTF_8))))
          (stream/close! stream-body)))
      (is (eventual #(and (zero? ((:active-count adapter)))
                          (stream/closed? stream-body)))
          "Server-side writer must finish before capacity is released")
      (is (= {:outcome :drained} ((:stop! adapter) 1000)))
      (finally
        (stream/close! stream-body)
        ((:stop! adapter) 2000)))))

(deftest physical-jdk-validates-before-opening-listener-test
  (doseq [[options expected] [[{:port -1} :invalid-port]
                              [{:port 65536} :invalid-port]
                              [{:threads 0} :invalid-threads]
                              [{:threads 65} :invalid-threads]
                              [{:drain-timeout-ms -1} :invalid-drain-timeout]
                              [{:host ""} :invalid-host]
                              [{:extra true} :invalid-options]]]
    (is (= expected (cause #(jdk/start! (constantly {:status 200 :headers {} :body "ok"})
                                         options)))))
  (is (= :invalid-handler (cause #(jdk/start! :not-a-handler {})))))

(deftest real-gesso-host-sse-through-owned-jdk-listener-test
  ;; Wire the same genuine Gesso/Biff/XTDB host used by the Live HTTP
  ;; acceptance tests into a first-party *physical* listener. Unlike a mock
  ;; :stop-listener! callback, listener drainage now observes actual server
  ;; response-writing threads, including an authorized streaming SSE body.
  (let [ops ((requiring-resolve
              'gesso.live.application-preflight.fixture/standard-operations))
        render ((requiring-resolve
                 'gesso.live.application-preflight.fixture/render-context) ops)
        button ((requiring-resolve
                 'gesso.live.application-preflight.fixture/rendered-operation-button)
                render :request/claim "/operations/request/claim")
        fixture (requiring-resolve
                 'gesso.live.application-preflight.fixture/with-surfaced-application)]
    (fixture
     {:request-board [:main button]}
     (fn [{:keys [assembly]}]
       (let [host-start (requiring-resolve 'gesso.live.outbox.host/start!)
             host-stop (requiring-resolve 'gesso.live.outbox.host/stop!)
             source-emit (requiring-resolve 'gesso.live.source/emit!)
             live-stats (requiring-resolve 'gesso.live.source/stats)
             config {:xtdb-config {}
                     :source-id "gesso/v2246-physical-jdk"
                     :rules-context {}
                     :worker-options {:worker-id "v2246-jdk-worker" :poll-ms 25}
                     :live-options {:rules [{:when-topic :request
                                            :expand (fn [_ change]
                                                      [(select-keys change
                                                                    [:topic :id :change/kind])])}]
                                    :source-options {:coalesce-window-ms 0}
                                    :dispatch-options {:threads 1
                                                       :queue-size 8
                                                       :on-overflow :throw}}}
             app (host-start assembly
                             (fn [_] {:status 200 :headers {} :body "normal"})
                             config)
             scope {:topic :request :id "v2246-item"}
             route (http/wrap-authorized-sse
                    app {:path "/live/events"
                         :max-connections 2
                         :authorize-subscription (constantly scope)})
             server (jdk/start! (:handler route) {:drain-timeout-ms 1000})
             closed-host (atom false)
             shutdown (http/prepare-managed-http-shutdown
                       {:route route
                        :stop-listener! (:stop-listener! server)
                        :stop-host! (fn []
                                      (host-stop app)
                                      (reset! closed-host true)
                                      {:outcome :stopped})
                        :drain-timeout-ms 1000})
             client (HttpClient/newHttpClient)]
         (try
           (let [response (.send client
                                 (request (:base-url server) "/live/events")
                                 (HttpResponse$BodyHandlers/ofInputStream))]
             (with-open [input (.body response)]
               (is (= 200 (.statusCode response)))
               (is (eventual #(= 1 ((:active-count server)))))
               (is (eventual #(= 1 ((:active-count route)))))
               (let [src (:source (:gesso.live/system app))]
                 (source-emit src {:topic :request :id "v2246-item"
                                   :change/kind :updated})
                 (let [reader (java.io.BufferedReader.
                               (java.io.InputStreamReader. input StandardCharsets/UTF_8))
                       lines (future (loop [acc []]
                                       (if (some #(= "event: live-update" %) acc)
                                         acc
                                         (if-let [line (.readLine reader)]
                                           (recur (conj acc line)) acc))))]
                   (is (some #(= "event: live-update" %)
                             (deref lines 5000 [])))
                   (future-cancel lines)))
               (is (pos? (:subscription-count
                          (live-stats (:source (:gesso.live/system app)))))))
               ;; Provider retirement runs *after* the route closes every
               ;; physical SSE producer and the real writer thread drains.
               (is (= :stopped (:outcome ((:stop! shutdown)))))
               (is @closed-host)
               (is ((:drained? server)))
               (is (zero? ((:active-count server))))
               (is (= :stopped (:outcome ((:stop! shutdown))))))
           (finally
             (when-not @closed-host
               (try ((:stop! shutdown)) (catch Throwable _ nil)))
             ((:stop! server) 1000))))))))

(deftest physical-jdk-sse-writer-after-ring-return-fences-shutdown-test
  ;; Ring has ALREADY returned the streaming response, yet the real adapter
  ;; still owns the open HTTP response writer. A route close that cannot
  ;; cancel its producer must still wait for the listener-owned cancellation
  ;; and physical writer retirement before shutting down host providers.
  (let [producer (stream/stream 4)
        ring-returned (promise)
        provider-stops (atom [])
        adapter (jdk/start!
                 (fn [_]
                   (deliver ring-returned true)
                   {:status 200 :headers {"content-type" "text/event-stream"}
                    :body producer})
                 {:drain-timeout-ms 0})
        route {:close! (constantly :closed)
               :await-drained! (constantly {:outcome :drained})}
        shutdown (http/prepare-managed-http-shutdown
                  {:route route :stop-listener! (:stop-listener! adapter)
                   :stop-host! (fn []
                                 (swap! provider-stops conj
                                        {:closed? (stream/closed? producer)
                                         :drained? ((:drained? adapter))})
                                 {:outcome :stopped})
                   :drain-timeout-ms 0})
        client (HttpClient/newHttpClient)]
    (try
      (let [response (.send client (request (:base-url adapter) "/events")
                            (HttpResponse$BodyHandlers/ofInputStream))]
        (with-open [body (.body response)]
          (is (= 200 (.statusCode response)))
          (is (= true (deref ring-returned 2000 ::timeout)))
          (is (= 1 ((:active-count adapter))))
          ;; A zero-timeout attempt may drain or time out, depending on
          ;; whether the writer has already observed cancellation. Neither
          ;; outcome permits stopping providers while the writer is active.
          (let [result ((:stop! shutdown))]
            (is (contains? #{:listener-timed-out :stopped} (:outcome result)))
            (when (= :listener-timed-out (:outcome result))
              (is (empty? @provider-stops))))
          (is (eventual #((:drained? adapter))))
          (is (stream/closed? producer))
          (is (= :stopped (:outcome ((:stop! shutdown)))))
          (is (= [{:closed? true :drained? true}] @provider-stops))
          (is (= :stopped (:outcome ((:stop! shutdown)))))
          (is (= 1 (count @provider-stops)))))
      (finally
        (stream/close! producer)
        ((:stop! adapter) 1000)))))

(deftest physical-jdk-refuses-lossy-request-normalization-before-dispatch
  (let [seen (atom [])
        adapter (jdk/start! (fn [req]
                              (swap! seen conj req)
                              {:status 200 :headers {} :body "ok"}) {})
        client (HttpClient/newHttpClient)
        send! (fn [path headers]
                (let [builder (HttpRequest/newBuilder (URI/create (str (:base-url adapter) path)))]
                  (doseq [[k v] headers] (.header builder k v))
                  (.send client (.build builder) (HttpResponse$BodyHandlers/ofString))))]
    (try
      (doseq [path ["/operations/%72equest/claim"
                    "/operations%2Frequest/claim"
                    "/operations/request/%252e%252e/claim"
                    "/operations/request/../claim"
                    "/operations//request/claim"]]
        (is (= 400 (.statusCode (send! path []))) path))
      ;; HttpClient combines repeated Cookie fields before transmission.
      ;; Write actual distinct fields to the loopback socket instead.
      (doseq [header ["authorization" "cookie" "content-type"
                      "gesso-live-progression"]]
        (with-open [socket (java.net.Socket. "127.0.0.1" (:port adapter))]
          (.setSoTimeout socket 5000)
          (let [wire (str "GET /normal HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n"
                          header ": one\r\n" (.toUpperCase header java.util.Locale/ROOT)
                          ": two\r\n\r\n")
                output (.getOutputStream socket)]
            (.write output (.getBytes wire StandardCharsets/US_ASCII))
            (.flush output)
            (with-open [reader (java.io.BufferedReader.
                                (java.io.InputStreamReader. (.getInputStream socket)
                                                            StandardCharsets/US_ASCII))]
              (is (re-find #"^HTTP/1\.1 400(?: |$)" (.readLine reader)) header)))))
      (is (empty? @seen))
      (is (= 200 (.statusCode (send! "/normal?q=a%2Fb" [["accept" "text/html, text/plain"]]))))
      (is (= 1 (count @seen)))
      (is (= "/normal" (:uri (first @seen))))
      (is (= "q=a%2Fb" (:query-string (first @seen))))
      (is (= "text/html, text/plain" (get-in (first @seen) [:headers "accept"])))
      (finally ((:stop! adapter) 2000) (.close client)))))

(deftest physical-jdk-does-not-alias-case-sensitive-methods
  (let [seen (atom [])
        adapter (jdk/start! (fn [req]
                              (swap! seen conj (:request-method req))
                              {:status 200 :headers {} :body "ok"}) {})
        client (HttpClient/newHttpClient)
        send! (fn [method]
                (.send client
                       (-> (HttpRequest/newBuilder (URI/create (str (:base-url adapter) "/command")))
                           (.method method (java.net.http.HttpRequest$BodyPublishers/noBody))
                           (.build))
                       (HttpResponse$BodyHandlers/ofString)))]
    (try
      (doseq [method ["post" "PoSt" "get" "GeT" "pUt"]]
        (is (= 400 (.statusCode (send! method))) method))
      (is (empty? @seen))
      (doseq [method ["POST" "GET" "PUT" "PATCH"]]
        (is (= 200 (.statusCode (send! method))) method))
      (is (= [:post :get :put :patch] @seen))
      (finally ((:stop! adapter) 2000) (.close client)))))

(deftest physical-jdk-releases-stream-on-response-setup-failure
  (doseq [[status headers] [[999 {}]
                            [200 nil]
                            [200 {"bad\nheader" "value"}]]]
    (let [producer (stream/stream 1)
          entered (promise)
          adapter (jdk/start! (fn [_]
                               (deliver entered true)
                               {:status status :headers headers :body producer}) {})]
      (try
        (with-open [socket (java.net.Socket. "127.0.0.1" (:port adapter))]
          (let [out (.getOutputStream socket)]
            (.write out (.getBytes "GET /events HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
                                   StandardCharsets/US_ASCII))
            (.flush out))
          (is (= true (deref entered 3000 ::timeout)))
          (is (eventual #(zero? ((:active-count adapter)))))
          (is (stream/closed? producer) [status headers])
          (is (= {:outcome :drained} ((:stop! adapter) 1000))))
        (finally
          (stream/close! producer)
          ((:stop! adapter) 2000))))))

(deftest physical-jdk-stop-cancels-quiet-owned-response-test
  (let [producer (stream/stream 4)
        adapter (jdk/start!
                 (fn [_] {:status 200
                          :headers {"content-type" "text/event-stream"}
                          :body producer}) {})
        client (HttpClient/newHttpClient)]
    (try
      (let [response (.send client (request (:base-url adapter) "/quiet-events")
                            (HttpResponse$BodyHandlers/ofInputStream))]
        (with-open [input (.body response)]
          (is (= 200 (.statusCode response)))
          (is (= 1 ((:active-count adapter))))
          (is (not (stream/closed? producer)))
          ;; No producer frame or external close is needed to cancel an
          ;; adapter-owned writer after listener stop.
          (is (= {:outcome :drained} ((:stop! adapter) 2000)))
          (is (stream/closed? producer))
          (is (zero? ((:active-count adapter))))
          (is ((:drained? adapter)))
          (is (= {:outcome :drained} ((:stop! adapter) 0)))))
      (finally
        (stream/close! producer)
        ((:stop! adapter) 2000)
        (.close client)))))
