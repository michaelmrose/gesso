(ns gesso.live.browser.fixture-server-test
  "Tests for the deterministic HTTP/SSE fixture used by real-browser Live tests.

   These tests establish the fixture's own control contract so later Chromium
   failures can be attributed to Gesso/browser behavior rather than an
   unreliable test peer. They intentionally exercise real loopback HTTP and SSE
   connections; they do not test the Gesso browser runtime itself."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.browser.fixture-server :as fixture])
  (:import
   (java.io BufferedReader IOException InputStreamReader)
   (java.net HttpURLConnection URI URL)
   (java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
                  HttpResponse$BodyHandlers)
   (java.time Duration)
   (java.nio.charset StandardCharsets)
   (java.util.concurrent ExecutionException TimeUnit)))

(def ^:private network-timeout-ms
  5000)

(def ^:private network-timeout
  (Duration/ofMillis network-timeout-ms))

(def ^:dynamic *client*
  nil)

(defn- new-http-client
  []
  (-> (HttpClient/newBuilder)
      (.connectTimeout network-timeout)
      (.build)))

(defn- with-fixture
  [f]
  (let [server (fixture/start!)]
    (try
      ;; Keep connection pools scoped to one test. A process-wide HttpClient can
      ;; retain idle loopback connections after a fixture stops; rapid ephemeral
      ;; port reuse can then make an otherwise deterministic fixture test depend
      ;; on stale pooled transport state.
      (binding [*client* (new-http-client)]
        (f server))
      (finally
        (fixture/stop! server)))))

(defn- http-request
  ([url]
   (http-request url {}))
  ([url {:keys [method body headers]
         :or {method :get
              headers {}}}]
   (let [builder (doto (HttpRequest/newBuilder (URI/create url))
                   (.timeout network-timeout))
         publisher (if (nil? body)
                     (HttpRequest$BodyPublishers/noBody)
                     (HttpRequest$BodyPublishers/ofString
                      (str body)
                      StandardCharsets/UTF_8))]
     (doseq [[header-name header-value] headers]
       (.header builder (str header-name) (str header-value)))
     (.method builder (.toUpperCase (name method)) publisher)
     (.build builder))))

(defn- send-string!
  ([url]
   (send-string! url {}))
  ([url request-options]
   (let [response
         (.send ^HttpClient *client*
                (http-request url request-options)
                (HttpResponse$BodyHandlers/ofString StandardCharsets/UTF_8))]
     {:status (.statusCode response)
      :body (.body response)})))

(defn- send-string-async!
  [url request-options]
  (.sendAsync ^HttpClient *client*
              (http-request url request-options)
              (HttpResponse$BodyHandlers/ofString StandardCharsets/UTF_8)))

(defn- await-response!
  [future]
  (let [response (.get future 5 TimeUnit/SECONDS)]
    {:status (.statusCode response)
     :body (.body response)}))

(defn- exception-data
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo error
      (ex-data error))))

(defn- open-sse!
  [url]
  ;; HttpURLConnection exposes a socket read timeout, which is exactly what the
  ;; fixture's SSE contract test needs. Java HttpClient returns an InputStream
  ;; for a streaming response but offers no per-read timeout; a missing frame
  ;; could therefore leave BufferedReader.readLine blocked forever.
  (let [^HttpURLConnection connection (.openConnection (URL. url))]
    (.setConnectTimeout connection network-timeout-ms)
    (.setReadTimeout connection network-timeout-ms)
    (.setRequestMethod connection "GET")
    (.setRequestProperty connection "Accept" "text/event-stream")
    (.connect connection)
    {:status (.getResponseCode connection)
     :connection connection
     :reader (BufferedReader.
              (InputStreamReader.
               (.getInputStream connection)
               StandardCharsets/UTF_8))}))

(defn- close-sse-client!
  [{:keys [^HttpURLConnection connection
           ^BufferedReader reader]}]
  (when reader
    (try
      (.close reader)
      (catch IOException _ nil)))
  (when connection
    (.disconnect connection))
  nil)

(defn- read-sse-lines
  [^BufferedReader reader line-count]
  ;; The connection backing reader has network-timeout-ms as SO_TIMEOUT. A
  ;; missing or truncated frame therefore becomes a bounded test error instead
  ;; of hanging the entire JVM test process.
  (mapv (fn [_] (.readLine reader))
        (range line-count)))

(deftest scripted-responses-are-consumed-in-order-test
  (with-fixture
    (fn [server]
      (fixture/script!
       server
       :get
       "/scripted"
       [(fixture/respond
         (fixture/response 201 {"x-fixture" "first"} "one"))
        (fixture/respond
         (fixture/response 202 "two"))])

      (fixture/enqueue!
       server
       "GET"
       "/scripted"
       (fixture/respond
        (fixture/response 203 "three")))

      (testing "scripted actions are consumed in exact queue order"
        (is (= {:status 201 :body "one"}
               (send-string! (fixture/url server "/scripted"))))
        (is (= {:status 202 :body "two"}
               (send-string! (fixture/url server "/scripted"))))
        (is (= {:status 203 :body "three"}
               (send-string! (fixture/url server "/scripted")))))

      (testing "an exhausted script falls back to the fixture default"
        (is (= {:status 404 :body "fixture route not scripted"}
               (send-string! (fixture/url server "/scripted")))))

      (testing "every ordinary request is recorded"
        (is (= [:respond :respond :respond :respond]
               (mapv :script-kind (fixture/requests server))))))))

(deftest request-records-preserve-browser-visible-input-test
  (with-fixture
    (fn [server]
      (fixture/script!
       server
       :post
       "/record"
       (fixture/respond (fixture/response 204)))

      (is (= 204
             (:status
              (send-string!
               (fixture/url server "/record?visible=raw%20query")
               {:method :post
                :body "alpha=1&beta=two"
                :headers {"X-Gesso-Test" "present"}}))))

      (let [request
            (fixture/await-request!
             server
             #(= "/record" (:path %)))]
        (is (= :post (:method request)))
        (is (= "/record" (:path request)))
        (is (= "visible=raw%20query" (:raw-query request)))
        (is (= "alpha=1&beta=two" (:body request)))
        (is (= ["present"]
               (get (:headers request) "x-gesso-test")))
        (is (= :respond (:script-kind request)))
        (is (pos-int? (:request-id request)))
        (is (= request
               (fixture/request-by-id server (:request-id request))))))))

(deftest held-request-releases-only-when-commanded-test
  (with-fixture
    (fn [server]
      (fixture/script!
       server
       :post
       "/held"
       [(fixture/hold (fixture/response 200 "default-release"))
        (fixture/hold (fixture/response 200 "unused-default"))])

      (let [first-future
            (send-string-async!
             (fixture/url server "/held")
             {:method :post
              :body "first"})

            first-request
            (fixture/await-pending!
             server
             #(= "first" (:body %)))]

        (testing "the held request remains physically unresolved"
          (is (false? (.isDone first-future)))
          (is (= #{(:request-id first-request)}
                 (fixture/pending-request-ids server))))

        (testing "release without override uses the hold action response"
          (is (true? (fixture/release! server (:request-id first-request))))
          (is (= {:status 200 :body "default-release"}
                 (await-response! first-future)))))

      (let [second-future
            (send-string-async!
             (fixture/url server "/held")
             {:method :post
              :body "second"})

            second-request
            (fixture/await-pending!
             server
             #(= "second" (:body %)))]

        (testing "release may override the response captured by hold"
          (fixture/release!
           server
           (:request-id second-request)
           (fixture/response
            409
            {"content-type" "text/plain; charset=utf-8"}
            "override"))
          (is (= {:status 409 :body "override"}
                 (await-response! second-future))))))))

(deftest held-request-may-be-closed-without-response-test
  (with-fixture
    (fn [server]
      (fixture/script!
       server
       :post
       "/held-close"
       (fixture/hold (fixture/response 200 "must-not-arrive")))

      (let [response-future
            (send-string-async! (fixture/url server "/held-close") {:method :post})

            request
            (fixture/await-pending! server)]
        (fixture/release! server (:request-id request) :close)

        (testing "the client observes transport failure rather than an HTTP response"
          (is (instance?
               IOException
               (try
                 (.get response-future 5 TimeUnit/SECONDS)
                 nil
                 (catch ExecutionException error
                   (.getCause error))))))))))

(deftest immediate-close-action-drops-the-exchange-test
  (with-fixture
    (fn [server]
      (fixture/script! server :post "/close" (fixture/close))
      (let [response-future
            (send-string-async! (fixture/url server "/close") {:method :post})]
        (is (instance?
             IOException
             (try
               (.get response-future 5 TimeUnit/SECONDS)
               nil
               (catch ExecutionException error
                 (.getCause error)))))
        (is (= :close
               (:script-kind
                (fixture/await-request!
                 server
                 #(= "/close" (:path %))))))))))

(deftest deterministic-awaits-fail-explicitly-on-timeout-test
  (with-fixture
    (fn [server]
      (testing "request waits identify their timeout as a fixture failure"
        (is (= :await-timeout
               (:error/kind
                (exception-data
                 #(fixture/await-request!
                   server
                   (constantly true)
                   10)))))

      (testing "pending-request waits use the same explicit timeout contract"
        (is (= :await-timeout
               (:error/kind
                (exception-data
                 #(fixture/await-pending!
                   server
                   (constantly true)
                   10))))))))))

(deftest sse-connections-are-real-addressable-streams-test
  (with-fixture
    (fn [server]
      (let [first-client (open-sse! (fixture/sse-url server "browser one"))
            first-status (:status first-client)
            first-reader (:reader first-client)
            second-client (open-sse! (fixture/sse-url server "browser one"))
            second-status (:status second-client)
            second-reader (:reader second-client)]
        (try
          (is (= 200 first-status))
          (is (= 200 second-status))

          (let [connection
                (fixture/await-sse-client! server "browser one")]
            (is (= "browser one" (:client-id connection)))
            (is (string? (:connection-id connection))))

          (testing "one emitted event is broadcast to every connection for the client"
            (is (= 2
                   (fixture/emit-sse!
                    server
                    "browser one"
                    {:id "evt-7"
                     :event "live-update"
                     :retry 1250
                     :data "line one\nline two"})))

            (let [expected
                  ["id: evt-7"
                   "event: live-update"
                   "retry: 1250"
                   "data: line one"
                   "data: line two"
                   ""]]
              (is (= expected (read-sse-lines first-reader 6)))
              (is (= expected (read-sse-lines second-reader 6)))))

          (testing "forced connection loss is targeted by logical client id"
            (is (= 2 (fixture/close-sse! server "browser one")))
            (is (empty? (fixture/sse-connections server))))

          (finally
            (close-sse-client! first-client)
            (close-sse-client! second-client)))))))

(deftest sse-client-ids-round-trip-through-the-url-test
  (with-fixture
    (fn [server]
      (let [client-id "browser/a + b"
            sse-client (open-sse! (fixture/sse-url server client-id))]
        (try
          (is (= client-id
                 (:client-id
                  (fixture/await-sse-client! server client-id))))
          (finally
            (fixture/close-sse! server client-id)
            (close-sse-client! sse-client)))))))

(deftest scripts-can-be-cleared-deliberately-test
  (with-fixture
    (fn [server]
      (fixture/script!
       server
       :get
       "/clear"
       (fixture/respond (fixture/response 200 "scripted")))
      (fixture/clear-script! server :get "/clear")
      (is (= {:status 404 :body "fixture route not scripted"}
             (send-string! (fixture/url server "/clear")))))))

(deftest stop-is-idempotent-test
  (let [server (fixture/start!)]
    (is (true? (fixture/stop! server)))
    (is (true? (fixture/stop! server)))))
