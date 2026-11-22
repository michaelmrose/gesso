(ns gesso.live.outbox.http-test
  "v2108: real loopback HTTP/SSE bytes from an actual canonical Gesso host.

   The JDK HttpServer in this test is a minimal Ring-to-HTTP bridge (Aleph is
   not part of Gesso's core test classpath). Network receipt is observed via
   java.net.http.HttpClient; no real browser/HTMX refresh is claimed."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gesso.core :as g]
            [gesso.live.application-preflight.fixture
             :refer [with-surfaced-application standard-operations render-context
                     rendered-operation-button]]
            [gesso.live.core :as live]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.host :as host]
            [gesso.live.outbox.http :as http]
            [gesso.live.outbox.lifecycle :as lifecycle]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.progression :as progression]
            [gesso.live.source :as source]
            [manifold.stream :as stream]
            [xtdb.api :as xt])
  (:import [com.sun.net.httpserver HttpServer HttpHandler HttpExchange]
           [java.io BufferedReader InputStreamReader]
           [java.net InetSocketAddress URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpClient$Version HttpResponse$BodyHandlers]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent Executors TimeUnit]))

(def ^:private scope {:topic :request :id "http-client-request"})
(def ^:private source-id "gesso/v2108-http-client")

(defn- config []
  {:xtdb-config {}
   :source-id source-id
   :rules-context {}
   :worker-options {:worker-id "v2108-http-worker" :poll-ms 20}
   :live-options {:rules [{:when-topic :request
                           :expand (fn [_ change]
                                     [(select-keys change [:topic :id :change/kind])])}]
                  :source-options {:coalesce-window-ms 0}
                  :dispatch-options {:threads 1 :queue-size 8 :on-overflow :throw}}})

(defn- setup-host [f]
  (let [ops (standard-operations)
        render (render-context ops)]
    (with-surfaced-application
      {:request-board
       [:main (rendered-operation-button render :request/claim
                                         "/operations/request/claim")]}
      (fn [{:keys [assembly]}]
        (let [handler (fn [_]
                        (g/html-response
                         [:main (rendered-operation-button
                                 render :request/claim
                                 "/operations/request/claim")]))
              started (host/start! assembly handler (config))]
          (try (f started)
               (finally (host/stop! started))))))))

(defn- eventually [pred]
  (loop [n 0]
    (cond (pred) true
          (>= n 250) false
          :else (do (Thread/sleep 20) (recur (inc n))))))

(defn- exception-reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- reply! [^HttpExchange exchange response]
  (let [headers (.getResponseHeaders exchange)
        body (:body response)
        sse? (stream/stream? body)
        status (:status response)]
    (doseq [[k v] (:headers response)]
      (.add headers (str k) (str v)))
    (if sse?
      (do
        (.sendResponseHeaders exchange status 0)
        (try
          (with-open [out (.getResponseBody exchange)]
            (loop [pending (stream/take! body)]
              ;; IMPORTANT: deref timeout must never abandon an outstanding
              ;; Manifold take! -- an orphaned take silently eats the frame.
              (let [next (deref pending 500 ::timeout)]
                (cond
                  (= ::timeout next)
                  (when-not (stream/closed? body) (recur pending))

                  (nil? next) nil

                  :else (do
                          (.write out (.getBytes (str next) StandardCharsets/UTF_8))
                          (.flush out)
                          (recur (stream/take! body)))))))
          (finally (stream/close! body))))
      (let [bytes (.getBytes (str body) StandardCharsets/UTF_8)]
        (.sendResponseHeaders exchange status (alength bytes))
        (with-open [out (.getResponseBody exchange)]
          (.write out bytes))))))

(defn- start-http! [ring-handler]
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
        executor (Executors/newCachedThreadPool)
        dispatch (reify HttpHandler
                   (handle [_ exchange]
                     (try
                       (let [headers (into {}
                                           (map (fn [[k v]]
                                                  [(str/lower-case k) (first v)]))
                                           (.entrySet (.getRequestHeaders exchange)))
                             request {:uri (.getPath (.getRequestURI exchange))
                                      :request-method (keyword (str/lower-case
                                                               (.getRequestMethod exchange)))
                                      :headers headers}]
                         (reply! exchange (ring-handler request)))
                       (catch Throwable _
                         ;; Error paths must not leave a leaked connection.
                         (try (.close exchange) (catch Throwable _ nil))))))]
    (.setExecutor server executor)
    (.createContext server "/" dispatch)
    (.start server)
    {:base (str "http://127.0.0.1:" (.getPort (.getAddress server)))
     :close! (fn [] (.stop server 0)
               (.shutdownNow executor))}))

(defn- request [^HttpClient client base path method token]
  (let [builder (HttpRequest/newBuilder (URI/create (str base path)))
        builder (if token (.header builder "authorization" token) builder)
        req (.build (.method builder method
                             (HttpRequest$BodyPublishers/noBody)))]
    (.send client req (HttpResponse$BodyHandlers/ofInputStream))))

(defn- frame-lines [^BufferedReader reader]
  (let [runner (future
                 (loop [acc []]
                   (let [line (.readLine reader)]
                     (cond
                       (nil? line) acc
                       (empty? line) (if (seq acc) acc (recur []))
                       :else (recur (conj acc line))))))
        result (deref runner 10000 ::timeout)]
    (when (= ::timeout result) (future-cancel runner))
    result))

(deftest invalid-route-configuration-fails-before-subscription-test
  (setup-host
   (fn [started]
     (let [base {:path "/live/events"
                 :authorize-subscription (fn [_] scope)}]
       (is (= :invalid-path (exception-reason
                            #(http/wrap-authorized-sse started
                                                       (assoc base :path "/")))))
       (is (= :missing-authorization-callback
              (exception-reason #(http/wrap-authorized-sse
                                  started (assoc base :authorize-subscription nil)))))
       (is (= :unknown-options
              (exception-reason #(http/wrap-authorized-sse
                                  started (assoc base :external-source :forged)))))
       (is (= :invalid-authorized-subscription
              (let [route (http/wrap-authorized-sse
                           started (assoc base :authorize-subscription
                                          (fn [_] {:topic :request :id nil})))]
                (try (exception-reason #((:handler route)
                                         {:uri "/live/events" :request-method :get}))
                     (finally ((:close! route)))))))
       (is (zero? (:subscription-count (source/stats (:source (:gesso.live/system started))))))))))

(deftest managed-route-rejects-non-200-sse-before-authorization-test
  (setup-host
   (fn [started]
     (let [source (:source (:gesso.live/system started))
           authorized (atom 0)
           base {:path "/live/events"
                 :authorize-subscription (fn [_]
                                           (swap! authorized inc)
                                           scope)}]
       ;; The generic SSE transport permits valid HTTP statuses other than
       ;; 200. The managed Ring route must not create a subscription that a
       ;; browser's EventSource will reject at the response boundary.
       (doseq [status [101 201 204 302 403 503]]
         (is (= :invalid-sse-status
                (exception-reason
                 #(http/wrap-authorized-sse
                   started (assoc base :sse-options {:status status}))))
             (str "Reject non-200 EventSource status " status))
         (is (zero? @authorized)
             "Route preflight must not invoke the authorization callback")
         (is (zero? (:subscription-count (source/stats source)))
             "Route preflight must not install a Live interest"))
       ;; An explicit 200 is fine and continues to use the existing source.
       (let [route (http/wrap-authorized-sse
                    started (assoc base :sse-options {:status 200}))]
         (try
           (let [response ((:handler route)
                           {:uri "/live/events" :request-method :get})]
             (is (= 200 (:status response)))
             (is (= 1 @authorized))
             (is (= 1 (:subscription-count (source/stats source)))))
           (finally
             ((:close! route))
             (is (eventually #(zero? (:subscription-count
                                     (source/stats source))))))))))))

(deftest managed-route-rejects-fixed-content-length-before-admission-test
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           calls (atom 0)
           base {:path "/live/events"
                 :authorize-subscription (fn [_]
                                           (swap! calls inc)
                                           scope)}]
       ;; EventSource consumes a live, indefinite body. Advertised fixed
       ;; lengths, including zero, can truncate it at the HTTP boundary.
       ;; Header field names are case-insensitive even in Ring maps.
       (doseq [header ["content-length" "Content-Length" "CONTENT-LENGTH"]]
         (let [result (try
                        (http/wrap-authorized-sse
                         started (assoc base :sse-options
                                        {:headers {header "0"}}))
                        (catch clojure.lang.ExceptionInfo e e))]
           (try
             (is (= :invalid-sse-content-length
                    (when (instance? clojure.lang.ExceptionInfo result)
                      (:reason (ex-data result))))
                 (str "Reject a fixed length before exposing SSE route: " header))
             (is (zero? @calls)
                 "Invalid header must not invoke application authorization")
             (is (zero? (:subscription-count (source/stats src)))
                 "Invalid header must not allocate a Live subscription")
             (finally
               (when (map? result) ((:close! result)))))))
       ;; Other safe custom headers must still be configurable.
       (let [route (http/wrap-authorized-sse
                    started (assoc base :sse-options
                                   {:headers {"x-gesso-test" "allowed"}}))]
         (try
           (let [response ((:handler route)
                           {:uri "/live/events" :request-method :get})]
             (is (= 200 (:status response)))
             (is (= "allowed" (get-in response [:headers "x-gesso-test"])))
             (is (= 1 @calls))
             (is (= 1 (:subscription-count (source/stats src))))
             (stream/close! (:body response))
             (is (eventually #(zero? (:subscription-count
                                     (source/stats src))))))
           (finally ((:close! route)))))))))

(deftest managed-route-rejects-server-owned-sse-headers-test
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           calls (atom 0)
           base {:path "/live/events"
                 :authorize-subscription (fn [_]
                                           (swap! calls inc)
                                           scope)}]
       ;; Header names are case-insensitive over HTTP even if Ring preserves
       ;; distinct string keys. No application override may weaken the SSE
       ;; caching, buffering or framing contract.
       (doseq [[header value]
               [["cache-control" "public, max-age=86400"]
                ["Cache-Control" "public, max-age=86400"]
                ["x-accel-buffering" "yes"]
                ["X-Accel-Buffering" "yes"]
                ["transfer-encoding" "chunked"]
                ["Transfer-Encoding" "chunked"]
                ["connection" "close"]
                ["CONNECTION" "close"]
                ["keep-alive" "timeout=1"]
                ["proxy-connection" "close"]
                ["te" "trailers"]
                ["trailer" "x-trace"]
                ["upgrade" "websocket"]]]
         (let [result (try
                        (http/wrap-authorized-sse
                         started (assoc base :sse-options
                                        {:headers {header value}}))
                        (catch clojure.lang.ExceptionInfo e e))]
           (try
             (is (= :invalid-sse-reserved-header
                    (when (instance? clojure.lang.ExceptionInfo result)
                      (:reason (ex-data result))))
                 (str "Reject server-owned response header " header))
             (is (zero? @calls)
                 "Invalid response framing must not invoke authorization")
             (is (zero? (:subscription-count (source/stats src)))
                 "Invalid response framing must not create an interest")
             (finally
               (when (map? result) ((:close! result)))))))
       ;; An ordinary custom header is allowed; framework-owned defaults
       ;; must still be present and active session cleanup must work.
       (let [route (http/wrap-authorized-sse
                    started (assoc base :sse-options
                                   {:headers {"x-gesso-trace" "safe"}}))]
         (try
           (let [response ((:handler route)
                           {:uri "/live/events" :request-method :get})]
             (is (= 200 (:status response)))
             (is (= "safe" (get-in response [:headers "x-gesso-trace"])))
             (is (= "no-cache, no-transform"
                    (get-in response [:headers "cache-control"])))
             (is (= "no" (get-in response [:headers "x-accel-buffering"])))
             (is (= 1 @calls))
             (is (= 1 (:subscription-count (source/stats src))))
             (stream/close! (:body response))
             (is (eventually #(zero? (:subscription-count
                                     (source/stats src))))))
           (finally ((:close! route)))))))))

(deftest managed-route-rejects-false-sse-representation-metadata-test
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           calls (atom 0)
           base {:path "/live/events"
                 :authorize-subscription (fn [_]
                                           (swap! calls inc)
                                           scope)}]
       ;; This transport writes plain SSE text. An application cannot claim
       ;; it encoded those bytes as gzip/br, nor advertise a partial range on
       ;; a successful unbounded EventSource stream. HTTP fields ignore case.
       (doseq [[header value] [["content-encoding" "gzip"]
                               ["Content-Encoding" "br"]
                               ["CONTENT-ENCODING" "identity"]
                               ["content-range" "bytes 0-9/10"]
                               ["Content-Range" "bytes 0-9/*"]]]
         (let [result (try
                        (http/wrap-authorized-sse
                         started (assoc base :sse-options
                                        {:headers {header value}}))
                        (catch clojure.lang.ExceptionInfo e e))]
           (try
             (is (= :invalid-sse-reserved-header
                    (when (instance? clojure.lang.ExceptionInfo result)
                      (:reason (ex-data result))))
                 (str "Reject false SSE body metadata: " header))
             (is (zero? @calls)
                 "Misleading response metadata must not invoke authorization")
             (is (zero? (:subscription-count (source/stats src)))
                 "Misleading response metadata must not create a Live interest")
             (finally
               (when (map? result) ((:close! result)))))))
       ;; Vary is legitimate application-controlled response metadata, while
       ;; neither content encoding nor a content range may be synthesized.
       (let [route (http/wrap-authorized-sse
                    started (assoc base :sse-options
                                   {:headers {"vary" "Origin"
                                              "x-gesso-trace" "allowed"}}))]
         (try
           (let [response ((:handler route)
                           {:uri "/live/events" :request-method :get})]
             (is (= 200 (:status response)))
             (is (= "Origin" (get-in response [:headers "vary"])))
             (is (= "allowed" (get-in response [:headers "x-gesso-trace"])))
             (is (not (contains? (:headers response) "content-encoding")))
             (is (not (contains? (:headers response) "content-range")))
             (is (false? (stream/closed? (:body response))))
             (is (= 1 @calls))
             (is (= 1 (:subscription-count (source/stats src))))
             (stream/close! (:body response))
             (is (eventually #(zero? (:subscription-count
                                     (source/stats src))))))
           (finally ((:close! route)))))))))

(deftest managed-route-rejects-unknown-sse-options-before-admission-test
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           calls (atom 0)
           base {:path "/live/events"
                 :authorize-subscription (fn [_]
                                           (swap! calls inc)
                                           scope)}]
       ;; Generic SSE options are merged permissively. At the managed HTTP
       ;; boundary, misspelled operational settings must not be silently
       ;; ignored, especially keepalive and lifecycle callbacks.
       (doseq [[opts missing-key]
               [[{:keepalive-mis 1000} :keepalive-mis]
                [{:on-erro (fn [_] nil)} :on-erro]
                [{:header {"x-gesso-test" "present"}} :header]
                [{:status 200 :keepalive-ms 1000 :unknown-setting true}
                 :unknown-setting]]]
         (let [result (try
                        (http/wrap-authorized-sse
                         started (assoc base :sse-options opts))
                        (catch clojure.lang.ExceptionInfo e e))]
           (try
             (is (= :unknown-sse-options
                    (when (instance? clojure.lang.ExceptionInfo result)
                      (:reason (ex-data result))))
                 (str "Unknown SSE setting must fail closed: " missing-key))
             (is (= #{missing-key}
                    (when (instance? clojure.lang.ExceptionInfo result)
                      (:keys (ex-data result)))))
             (is (zero? @calls)
                 "Bad SSE preflight may not authorize a request")
             (is (zero? (:subscription-count (source/stats src)))
                 "Bad SSE preflight may not allocate a Live interest")
             (finally
               (when (map? result) ((:close! result)))))))
       ;; A non-map must also fail with a managed, typed error rather than
       ;; a generic exception thrown by option merging.
       (doseq [opts [false [[:keepalive-ms 2000]] "keepalive-ms"]]
         (is (= :invalid-sse-options
                (exception-reason #(http/wrap-authorized-sse
                                    started (assoc base :sse-options opts)))))
         (is (zero? @calls))
         (is (zero? (:subscription-count (source/stats src)))))
       ;; Every advertised transport option remains accepted (using defaults
       ;; for options not under test), and a real stream still cleans up.
       (let [route (http/wrap-authorized-sse
                    started (assoc base :sse-options
                                   {:status 200
                                    :keepalive-ms 30000
                                    :headers {"x-gesso-trace" "yes"}
                                    :on-error (fn [_] nil)}))]
         (try
           (let [response ((:handler route)
                           {:uri "/live/events" :request-method :get})]
             (is (= 200 (:status response)))
             (is (= "yes" (get-in response [:headers "x-gesso-trace"])))
             (is (false? (stream/closed? (:body response))))
             (is (= 1 @calls))
             (is (= 1 (:subscription-count (source/stats src))))
             (stream/close! (:body response))
             (is (eventually #(zero? (:subscription-count
                                     (source/stats src))))))
           (finally ((:close! route)))))))))

(deftest managed-route-rejects-duplicate-case-insensitive-headers-test
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           calls (atom 0)
           base {:path "/live/events"
                 :authorize-subscription (fn [_] (swap! calls inc) scope)}]
       ;; Ring maps can carry both spellings, but HTTP interprets each pair
       ;; as one field. Reject even if their values agree: duplicate field
       ;; ownership can be ambiguous after serialization/proxy handling.
       (doseq [[fields canonical]
               [[{"Vary" "Origin" "vary" "Accept-Encoding"} "vary"]
                [{"X-Gesso-Trace" "a" "x-gesso-trace" "a"} "x-gesso-trace"]
                [{"Set-Cookie" "a=1" "set-cookie" "b=2"} "set-cookie"]]]
         (let [result (try
                        (http/wrap-authorized-sse
                         started (assoc base :sse-options {:headers fields}))
                        (catch clojure.lang.ExceptionInfo e e))]
           (try
             (is (= :duplicate-sse-header
                    (when (instance? clojure.lang.ExceptionInfo result)
                      (:reason (ex-data result))))
                 (str "Reject duplicate case-insensitive HTTP field " canonical))
             (is (= canonical
                    (when (instance? clojure.lang.ExceptionInfo result)
                      (:header (ex-data result)))))
             (is (zero? @calls)
                 "Duplicate headers cannot reach authorization")
             (is (zero? (:subscription-count (source/stats src)))
                 "Duplicate headers cannot allocate a source interest")
             (finally (when (map? result) ((:close! result)))))))
       ;; Legitimate multiple values under one HTTP field remain supported.
       ;; One active subscription still cleans up after its body closes.
       (let [route (http/wrap-authorized-sse
                    started (assoc base :sse-options
                                   {:headers {"vary" ["Origin" "Accept-Encoding"]
                                              "x-gesso-trace" "allowed"}}))]
         (try
           (let [response ((:handler route)
                           {:uri "/live/events" :request-method :get})]
             (is (= 200 (:status response)))
             (is (= ["Origin" "Accept-Encoding"]
                    (get-in response [:headers "vary"])))
             (is (= "allowed" (get-in response [:headers "x-gesso-trace"])))
             (is (= 1 @calls))
             (is (= 1 (:subscription-count (source/stats src))))
             (is (false? (stream/closed? (:body response))))
             (stream/close! (:body response))
             (is (eventually #(zero? (:subscription-count
                                     (source/stats src))))))
           (finally ((:close! route)))))))))

(deftest managed-route-rejects-ambiguous-url-path-test
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           authorized (atom 0)
           base {:path "/live/events"
                 :authorize-subscription
                 (fn [_] (swap! authorized inc) scope)}]
       ;; A proxy may collapse adjacent slashes or remove dot segments before
       ;; Ring sees :uri. The configured route must be unambiguous at both
       ;; boundaries; detecting this only on requests is too late.
       (doseq [path ["//live/events" "/live//events" "/live/events//"
                     "/live/./events" "/live/../events"
                     "/live/events/." "/live/events/.."]]
         (let [result (try
                        (http/wrap-authorized-sse
                         started (assoc base :path path))
                        (catch clojure.lang.ExceptionInfo e e))]
           (try
             (is (= :invalid-path
                    (when (instance? clojure.lang.ExceptionInfo result)
                      (:reason (ex-data result))))
                 (str "Ambiguous SSE route path must fail: " path))
             (is (= path
                    (when (instance? clojure.lang.ExceptionInfo result)
                      (:path (ex-data result))))
                 "Typed rejection must identify the configured path")
             (is (zero? @authorized)
                 "Invalid route must not invoke authorization")
             (is (zero? (:subscription-count (source/stats src)))
                 "Invalid route must not allocate Live interests")
             (finally
               (when (map? result) ((:close! result)))))))
       ;; Ordinary namespaced/versioned segments and an intentional trailing
       ;; slash are legitimate. None of these requires normalization.
       (doseq [path ["/live/events" "/live/v1.2/events"
                     "/.well-known/live" "/live/events/"]]
         (let [route (http/wrap-authorized-sse
                      started (assoc base :path path))]
           (try
             (let [response ((:handler route)
                             {:uri path :request-method :get})]
               (is (= 200 (:status response)))
               (is (false? (stream/closed? (:body response))))
               (stream/close! (:body response))
               (is (eventually #(zero? (:subscription-count
                                       (source/stats src))))))
             (finally ((:close! route))))))
       (is (= 4 @authorized))
       (is (zero? (:subscription-count (source/stats src))))))))

(deftest managed-route-limits-active-connections-before-source-admission-test
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           calls (atom 0)
           request {:uri "/live/events" :request-method :get}
           base {:path "/live/events"
                 :authorize-subscription (fn [_] (swap! calls inc) scope)}]
       ;; Misconfigured limits must fail before invoking authorization or
       ;; allocating a Live interest, rather than silently disabling the cap.
       (doseq [limit [nil 0 -1 1.5 (inc (bigint Long/MAX_VALUE))]]
         (let [result (try
                        (http/wrap-authorized-sse
                         started (assoc base :max-connections limit))
                        (catch clojure.lang.ExceptionInfo e e))]
           (try
             (is (= :invalid-max-connections
                    (when (instance? clojure.lang.ExceptionInfo result)
                      (:reason (ex-data result)))))
             (is (zero? @calls))
             (is (zero? (:subscription-count (source/stats src))))
             (finally (when (map? result) ((:close! result)))))))
       ;; The resource limit is enforced inside the atomic source-admission
       ;; fence: another authorized GET is refused without opening a stream.
       (let [route (http/wrap-authorized-sse
                    started (assoc base :max-connections 1))]
         (try
           (let [first-response ((:handler route) request)]
             (is (= 200 (:status first-response)))
             (is (= 1 ((:active-count route))))
             (is (= 1 (:subscription-count (source/stats src))))
             (is (= {:status 503 :headers {}
                     :body "Live connection capacity reached"}
                    ((:handler route) request)))
             (is (= 2 @calls)
                 "Capacity is checked only after authorization")
             (is (= 1 ((:active-count route))))
             (is (= 1 (:subscription-count (source/stats src)))
                 "Over-capacity admission must not allocate an interest")
             ;; Reclaiming the slot permits a subsequent authorized GET.
             (stream/close! (:body first-response))
             (is (eventually #(zero? ((:active-count route)))))
             (is (eventually #(zero? (:subscription-count (source/stats src)))))
             (let [second-response ((:handler route) request)]
               (is (= 200 (:status second-response)))
               (is (= 1 ((:active-count route))))
               (is (= 1 (:subscription-count (source/stats src))))
               (stream/close! (:body second-response))
               (is (eventually #(zero? ((:active-count route)))))
               (is (eventually #(zero? (:subscription-count
                                       (source/stats src)))))))
           (finally ((:close! route)))))
       (is (= 3 @calls))
       (is (zero? (:subscription-count (source/stats src))))))))

(deftest concurrent-managed-route-admission-respects-capacity-test
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           entered (atom 0)
           release (promise)
           request {:uri "/live/events" :request-method :get}
           route (http/wrap-authorized-sse
                  started {:path "/live/events" :max-connections 1
                           :authorize-subscription
                           (fn [_]
                             (swap! entered inc)
                             @release
                             scope)})
           first-request (future ((:handler route) request))
           second-request (future ((:handler route) request))]
       (try
         (is (eventually #(= 2 @entered))
             "Both callbacks should begin outside the admission lock")
         (deliver release true)
         (let [a (deref first-request 10000 ::timeout)
               b (deref second-request 10000 ::timeout)
               responses [a b]
               accepted (first (filter #(= 200 (:status %)) responses))]
           (is (= #{200 503} (set (map :status responses)))
               "Exactly one racing request should acquire the route capacity")
           (is (= 1 ((:active-count route))))
           (is (= 1 (:subscription-count (source/stats src))))
           (is (= {:status 503 :headers {}
                   :body "Live connection capacity reached"}
                  (first (filter #(= 503 (:status %)) responses))))
           (when accepted (stream/close! (:body accepted)))
           (is (eventually #(zero? ((:active-count route)))))
           (is (eventually #(zero? (:subscription-count (source/stats src))))))
         (finally
           (deliver release true)
           (future-cancel first-request)
           (future-cancel second-request)
           ((:close! route))))))))

(deftest managed-route-bounds-pending-authorizations-test
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           calls (atom 0)
           entered (promise)
           release (promise)
           request {:uri "/live/events" :request-method :get}
           base {:path "/live/events"
                 :authorize-subscription
                 (fn [_]
                   (when (= 1 (swap! calls inc))
                     (deliver entered true)
                     @release)
                   scope)}]
       ;; Invalid limits are rejected before authorization or subscription.
       (doseq [limit [nil 0 -5 1.2 (inc (bigint Long/MAX_VALUE))]]
         (is (= :invalid-max-pending-authorizations
                (exception-reason
                 #(http/wrap-authorized-sse
                   started (assoc base :max-pending-authorizations limit)))))
         (is (zero? @calls))
         (is (zero? (:subscription-count (source/stats src)))))
       (let [route (http/wrap-authorized-sse
                    started (assoc base :max-pending-authorizations 1))
             pending (future ((:handler route) request))]
         (try
           (is (= true (deref entered 5000 ::timeout)))
           (is (= 1 ((:pending-authorization-count route))))
           (is (= 0 ((:active-count route))))
           (is (zero? (:subscription-count (source/stats src))))
           (is (= {:status 503 :headers {}
                   :body "Live authorization capacity reached"}
                  ((:handler route) request))
               "A second GET cannot invoke an unbounded callback")
           (is (= 1 @calls))
           (deliver release true)
           (let [response (deref pending 10000 ::timeout)]
             (is (= 200 (:status response)))
             (is (= 0 ((:pending-authorization-count route))))
             (is (= 1 ((:active-count route))))
             (is (= 1 (:subscription-count (source/stats src))))
             (stream/close! (:body response)))
           (is (eventually #(zero? ((:active-count route)))))
           (is (eventually #(zero? (:subscription-count (source/stats src)))))
           ;; Release on normal completion allows another admission.
           (let [response ((:handler route) request)]
             (is (= 200 (:status response)))
             (stream/close! (:body response))
             (is (eventually #(zero? ((:active-count route))))))
           (is (= 2 @calls))
           (finally
             (deliver release true)
             (future-cancel pending)
             ((:close! route)))))))))

(deftest managed-route-releases-pending-authorization-on-failure-and-shutdown-test
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           request {:uri "/live/events" :request-method :get}
           calls (atom 0)
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :max-pending-authorizations 1
                           :authorize-subscription
                           (fn [_]
                             (swap! calls inc)
                             (if (= 1 @calls)
                               (throw (ex-info "auth failed" {:cause :expected}))
                               scope))})]
       (try
         (is (= :expected
                (try
                  ((:handler route) request)
                  nil
                  (catch clojure.lang.ExceptionInfo e (:cause (ex-data e))))))
         (is (zero? ((:pending-authorization-count route))))
         (let [response ((:handler route) request)]
           (is (= 200 (:status response)))
           (is (= 1 (:subscription-count (source/stats src))))
           (stream/close! (:body response)))
         (is (eventually #(zero? (:subscription-count (source/stats src)))))
         (is (= 2 @calls))
         (finally ((:close! route)))))
     ;; Close while authorization is blocked; its reservation must be
     ;; released, but the delayed callback must not re-open the route.
     (let [entered (promise)
           release (promise)
           request {:uri "/live/events" :request-method :get}
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :max-pending-authorizations 1
                           :authorize-subscription
                           (fn [_] (deliver entered true) @release scope)})
           pending (future ((:handler route) request))]
       (try
         (is (= true (deref entered 5000 ::timeout)))
         (is (= 1 ((:pending-authorization-count route))))
         ((:close! route))
         (deliver release true)
         (is (= {:status 503 :headers {} :body "Live shutting down"}
                (deref pending 5000 ::timeout)))
         (is (zero? ((:pending-authorization-count route))))
         (is (zero? ((:active-count route))))
         (is (zero? (:subscription-count
                      (source/stats (:source (:gesso.live/system started))))))
         (finally
           (deliver release true)
           (future-cancel pending)
           ((:close! route))))))))

(deftest managed-route-bounds-get-requests-blocked-on-sse-admission-test
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           request {:uri "/live/events" :request-method :get}
           called (atom 0)
           entered (promise)
           release (promise)
           original-start live/start-sse!
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :max-connections 2
                           :max-pending-authorizations 1
                           :authorize-subscription
                           (fn [_] (swap! called inc) scope)})
           first-request (atom nil)
           second-request (atom nil)]
       (try
         ;; SSE start holds the route's admission monitor. A callback that
         ;; already returned must keep its pending slot until session handoff,
         ;; and overloaded GETs must not wait on the occupied monitor.
         (with-redefs [live/start-sse!
                       (fn [& args]
                         (deliver entered true)
                         @release
                         (apply original-start args))]
           (reset! first-request (future ((:handler route) request)))
           (is (= true (deref entered 5000 ::timeout)))
           (is (= 1 @called))
           (is (= 1 ((:pending-authorization-count route)))
               "A request awaiting SSE startup retains its reservation")
           (is (= 0 ((:active-count route))))
           (reset! second-request (future ((:handler route) request)))
           (is (= {:status 503 :headers {}
                   :body "Live authorization capacity reached"}
                  (deref @second-request 2000 ::timeout))
               "Over-capacity GET must fail without waiting for SSE startup")
           (is (= 1 @called) "The excess GET must not invoke authorization")
           (is (= 1 ((:pending-authorization-count route))))
           (is (zero? (:subscription-count (source/stats src))))
           (deliver release true)
           (let [response (deref @first-request 10000 ::timeout)]
             (is (= 200 (:status response)))
             (is (zero? ((:pending-authorization-count route))))
             (is (= 1 ((:active-count route))))
             (stream/close! (:body response)))
           ;; A broken implementation might admit a second stream after
           ;; the blocked startup finishes; retire it before teardown.
           (let [response (deref @second-request 10000 ::timeout)]
             (when (= 200 (:status response))
               (stream/close! (:body response))))
           (is (eventually #(zero? ((:active-count route)))))
           (is (eventually #(zero? (:subscription-count (source/stats src))))))
         (finally
           (deliver release true)
           (when @first-request (future-cancel @first-request))
           (when @second-request (future-cancel @second-request))
           ((:close! route))))))))

(deftest managed-route-releases-pending-slot-after-sse-start-exception-test
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           request {:uri "/live/events" :request-method :get}
           expected (ex-info "synthetic SSE failure" {:phase :sse-start})
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :max-pending-authorizations 1
                           :authorize-subscription (fn [_] scope)})]
       (try
         (with-redefs [live/start-sse! (fn [& _] (throw expected))]
           (is (identical? expected
                           (try ((:handler route) request)
                                nil
                                (catch clojure.lang.ExceptionInfo e e))))
           (is (zero? ((:pending-authorization-count route)))
               "SSE startup failure must release the pending slot")
           (is (zero? ((:active-count route))))
           (is (zero? (:subscription-count (source/stats src)))))
         (let [response ((:handler route) request)]
           (is (= 200 (:status response))
               "The released slot permits a subsequent successful GET")
           (stream/close! (:body response)))
         (is (eventually #(zero? ((:active-count route)))))
         (is (eventually #(zero? (:subscription-count (source/stats src)))))
         (finally ((:close! route))))))))

(deftest managed-route-default-keepalive-and-explicit-overrides-test
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           seen (atom [])
           original live/start-sse!
           request {:uri "/live/events" :request-method :get}
           base {:path "/live/events"
                 :authorize-subscription (constantly scope)}]
       (with-redefs [live/start-sse!
                     (fn [system subscription opts]
                       (swap! seen conj (get-in opts [:sse-options :keepalive-ms]))
                       (original system subscription opts))]
         ;; The managed default is distinct from the generic SSE transport
         ;; default (nil). A caller may choose a different interval or
         ;; explicitly opt out when another HTTP disconnect probe exists.
         (doseq [[options expected]
                 [[base 15000]
                  [(assoc base :sse-options {:keepalive-ms 55}) 55]
                  [(assoc base :sse-options {:keepalive-ms nil}) nil]]]
           (let [route (http/wrap-authorized-sse started options)]
             (try
               (let [response ((:handler route) request)]
                 (is (= 200 (:status response)))
                 (is (= expected (last @seen)))
                 (is (= 1 ((:active-count route))))
                 (stream/close! (:body response)))
               (finally ((:close! route))))))
         (is (= [15000 55 nil] @seen))
         (is (eventually #(zero? (:subscription-count (source/stats src))))))))))

(deftest real-http-client-disconnect-releases-sse-capacity-test
  ;; This tests a physical client abort, not route-wide :close! or a direct
  ;; Manifold close. The loopback adapter must observe an abandoned response
  ;; body, close the producer, release its Live interest, and make the
  ;; per-route slot available to the next client.
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           ;; Scale only the managed default interval down for this physical
           ;; test. The route gets no :sse-options override: its own default
           ;; must supply the probe that reveals the abandoned HTTP writer.
           route (with-redefs-fn
                   {#'gesso.live.outbox.http/default-managed-keepalive-ms 30}
                   #(http/wrap-authorized-sse
                     started {:path "/live/events"
                              :max-connections 1
                              :authorize-subscription (constantly scope)}))
           server (start-http! (:handler route))
           client (-> (HttpClient/newBuilder)
                      (.version HttpClient$Version/HTTP_1_1)
                      (.build))]
       (try
         (let [first-reply (request client (:base server)
                                    "/live/events" "GET" "Bearer allowed")]
           (is (= 200 (.statusCode first-reply)))
           (is (eventually #(= 1 ((:active-count route)))))
           (is (eventually #(= 1 (:subscription-count (source/stats src)))))
           (with-open [body (.body first-reply)
                       reader (BufferedReader. (InputStreamReader.
                                                body StandardCharsets/UTF_8))]
             ;; Observing a frame on the wire establishes that the adapter
             ;; has begun pumping the stream before the client disconnects.
             (is (= [": ping"] (frame-lines reader)))))
         ;; The route remains open; capacity must recover on its own.
         (is (false? ((:closed? route))))
         (is (eventually #(zero? ((:active-count route))))
             "Closing the network response must release route capacity")
         (is (eventually #(zero? (:subscription-count (source/stats src))))
             "Closing the network response must release the Live interest")
         (let [second-reply (request client (:base server)
                                     "/live/events" "GET" "Bearer allowed")]
           (with-open [body (.body second-reply)]
             (is (= 200 (.statusCode second-reply))
                 "A disconnected client's freed slot must be reusable")
             (is (eventually #(= 1 ((:active-count route)))))
             (is (eventually #(= 1 (:subscription-count (source/stats src)))))))
         (finally
           ((:close! route))
           ((:close! server))
           (.close ^HttpClient client)
           (is (eventually #(zero? ((:active-count route)))))
           (is (eventually #(zero? (:subscription-count (source/stats src)))))))))))

(deftest real-http-sse-active-capacity-returns-503-without-eviction-test
  ;; A Ring-level status alone does not establish that a real HTTP adapter
  ;; sends the overload status while keeping the first EventSource alive.
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           authorized (atom 0)
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :max-connections 1
                           :authorize-subscription
                           (fn [req]
                             (when (= "Bearer allowed"
                                      (get-in req [:headers "authorization"]))
                               (swap! authorized inc)
                               scope))})
           server (start-http! (:handler route))
           client (-> (HttpClient/newBuilder)
                      (.version HttpClient$Version/HTTP_1_1)
                      (.build))]
       (try
         (let [first-reply (request client (:base server)
                                    "/live/events" "GET" "Bearer allowed")]
           (with-open [first-body (.body first-reply)]
             (is (= 200 (.statusCode first-reply)))
             (is (eventually #(= 1 ((:active-count route)))))
             (is (eventually #(= 1 (:subscription-count (source/stats src)))))
             ;; Over capacity, a second authorized GET must be visibly
             ;; rejected on the wire without replacing the first stream.
             (let [overload (request client (:base server)
                                     "/live/events" "GET" "Bearer allowed")]
               (with-open [body (.body overload)]
                 (is (= 503 (.statusCode overload)))
                 (is (= "Live connection capacity reached"
                        (String. (.readAllBytes body) StandardCharsets/UTF_8)))))
             (is (= 2 @authorized))
             (is (= 1 ((:active-count route))))
             (is (= 1 (:subscription-count (source/stats src))))
             (is (zero? ((:pending-authorization-count route))))
             ;; Never disclose capacity before checking requester authority.
             (let [denied (request client (:base server)
                                   "/live/events" "GET" nil)]
               (with-open [body (.body denied)]
                 (is (= 403 (.statusCode denied)))
                 (is (= "Forbidden"
                        (String. (.readAllBytes body) StandardCharsets/UTF_8)))))
             (is (= 1 ((:active-count route))))
             (is (= 1 (:subscription-count (source/stats src))))
             ((:close! route))
             (is (eventually #(zero? ((:active-count route)))))
             (is (eventually #(zero? (:subscription-count (source/stats src)))))))
         (finally
           ((:close! route))
           ((:close! server))
           (.close ^HttpClient client)))))))

(deftest real-http-sse-pending-authorization-capacity-returns-503-test
  ;; Exercise admission pressure through actual HTTP threads while the
  ;; application's first authorization callback is blocked. A second GET
  ;; must return overload without entering the callback or source.
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           entered (promise)
           release (promise)
           authorized (atom 0)
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :max-pending-authorizations 1
                           :authorize-subscription
                           (fn [_]
                             (swap! authorized inc)
                             (deliver entered true)
                             @release
                             scope)})
           server (start-http! (:handler route))
           client (-> (HttpClient/newBuilder)
                      (.version HttpClient$Version/HTTP_1_1)
                      (.build))
           pending (future (request client (:base server)
                                    "/live/events" "GET" "Bearer allowed"))]
       (try
         (is (= true (deref entered 5000 ::timeout)))
         (is (= 1 ((:pending-authorization-count route))))
         (is (zero? (:subscription-count (source/stats src))))
         (let [overload (request client (:base server)
                                 "/live/events" "GET" "Bearer allowed")]
           (with-open [body (.body overload)]
             (is (= 503 (.statusCode overload)))
             (is (= "Live authorization capacity reached"
                    (String. (.readAllBytes body) StandardCharsets/UTF_8)))))
         (is (= 1 @authorized) "Overload must not run authorization")
         (is (= 1 ((:pending-authorization-count route))))
         (is (zero? (:subscription-count (source/stats src))))
         (deliver release true)
         (let [first-reply (deref pending 10000 ::timeout)]
           (is (not= ::timeout first-reply))
           (when (not= ::timeout first-reply)
             (with-open [body (.body first-reply)]
               (is (= 200 (.statusCode first-reply)))
               (is (eventually #(= 1 (:subscription-count (source/stats src)))))
               ((:close! route))
               (is (eventually #(zero? (:subscription-count (source/stats src))))))))
         (is (zero? ((:pending-authorization-count route))))
         (finally
           (deliver release true)
           (future-cancel pending)
           ((:close! route))
           ((:close! server))
           (.close ^HttpClient client)))))))

(deftest concrete-host-serves-scope-authorized-progression-over-real-http-test
  (setup-host
   (fn [started]
     (let [sys (:gesso.live/system started)
           node (:xtdb/connectable started)
           work (get started managed/handle-key)
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :authorize-subscription
                           (fn [req]
                             (when (= "Bearer allowed" (get-in req [:headers "authorization"]))
                               scope))})
           server (start-http! (:handler route))
           client (-> (HttpClient/newBuilder)
                      (.version HttpClient$Version/HTTP_1_1)
                      (.build))]
       (try
         ;; An untrusted request cannot establish a Live interest.
         (let [deny (request client (:base server) "/live/events" "GET" nil)]
           (is (= 403 (.statusCode deny)))
           (.close (.body deny)))
         (let [wrong (request client (:base server) "/live/events" "POST" "Bearer allowed")]
           (is (= 405 (.statusCode wrong)))
           (.close (.body wrong)))
         (let [page (request client (:base server) "/" "GET" nil)]
           (is (= 200 (.statusCode page)))
           (.close (.body page)))
         (is (zero? (:subscription-count (source/stats (:source sys)))))
         (let [reply (request client (:base server) "/live/events" "GET" "Bearer allowed")]
           (is (= 200 (.statusCode reply)))
           (is (str/starts-with? (first (.allValues (.headers reply) "content-type"))
                                 "text/event-stream"))
           (is (eventually #(= 1 ((:active-count route)))))
           (is (eventually #(= 1 (:subscription-count
                                   (source/stats (:source sys))))))
           (let [intent-id (random-uuid)
                 plan (outbox/prepare!
                       {:intent-ids [intent-id] :source-id source-id
                        :operation :request/claim :purpose :gesso.live/refresh}
                       [{:topic :request :id (:id scope) :change/kind :updated}])]
             (xt/execute-tx node (into (vec (:guard-ops plan))
                                       [[:put-docs :requests
                                         {:xt/id (:id scope) :status :claimed}]
                                        (:tx-op plan)]))
             (is (eventually
                  #(= :dispatch-accepted
                      (:gesso.live.outbox/status
                       (first (xt/q node ["SELECT * FROM gesso_live_outbox WHERE _id = ?"
                                          intent-id]))))))
             (is (eventually #(= 1 (:total-dispatch-accepted
                                    (lifecycle/status work)))))
             (with-open [body (.body reply)]
               (let [reader (BufferedReader. (InputStreamReader. body StandardCharsets/UTF_8))
                     lines (frame-lines reader)
                     payload-line (first (filter #(str/starts-with? % "data: ") lines))
                     event-line (first (filter #(str/starts-with? % "event: ") lines))
                     payload (when payload-line (edn/read-string (subs payload-line 6)))]
                 (is (not= ::timeout lines))
                 (is (= "event: live-update" event-line))
                 (is (= (:id scope) (get-in payload [:invalidation :id])))
                 (is (= :request (get-in payload [:invalidation :topic])))
                 (is (some? (progression/wire->requirement (:progression payload))))))))
         ;; Do not leave HTTP streams live when the host's providers close.
         ((:close! route))
         (is (eventually #(zero? (:subscription-count
                                  (source/stats (:source sys))))))
         (is (true? ((:closed? route))))
         (finally
           ((:close! route))
           ((:close! server))
           (.close ^HttpClient client)))))))

(deftest stalled-sse-startup-reserves-active-connection-capacity-test
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           entered (promise)
           release (promise)
           started-count (atom 0)
           authorized (atom 0)
           request {:uri "/live/events" :request-method :get}
           route (http/wrap-authorized-sse
                  started {:path "/live/events" :max-connections 1
                           :max-pending-authorizations 2
                           :authorize-subscription
                           (fn [_] (swap! authorized inc) scope)})
           original-start live/start-sse!
           first-request (atom nil)]
       (try
         (with-redefs [live/start-sse!
                       (fn [& args]
                         (swap! started-count inc)
                         (deliver entered true)
                         @release
                         (apply original-start args))]
           (reset! first-request (future ((:handler route) request)))
           (is (= true (deref entered 5000 ::timeout)))
           (is (= 1 ((:pending-authorization-count route))))
           (is (zero? ((:active-count route)))
               "A reserved starting slot is not yet an active stream")
           ;; A second authorized GET may reach the admission lock but must
           ;; not start a second session or wait for the first startup.
           (let [second-request (future ((:handler route) request))]
             (try
               (is (= {:status 503 :headers {}
                       :body "Live connection capacity reached"}
                      (deref second-request 1500 ::timeout)))
               (finally (future-cancel second-request))))
           (is (= 2 @authorized))
           (is (= 1 @started-count))
           (is (zero? (:subscription-count (source/stats src))))
           (deliver release true)
           (let [response (deref @first-request 10000 ::timeout)]
             (is (= 200 (:status response)))
             (is (= 1 ((:active-count route))))
             (is (= 1 (:subscription-count (source/stats src))))
             (stream/close! (:body response)))
           (is (eventually #(zero? ((:active-count route)))))
           (is (eventually #(zero? (:subscription-count (source/stats src)))))
           (is (zero? ((:pending-authorization-count route)))))
         (finally
           (deliver release true)
           (when @first-request (future-cancel @first-request))
           ((:close! route))))))))

(deftest managed-http-shutdown-coordinator-serializes-success-test
  ;; The v2148 one-shot helper requires external caller serialization.
  ;; This coordinator must enforce at-most-once host stop itself, including
  ;; while several callers overlap in the listener cleanup step.
  (let [calls (atom [])
        listener-entered (promise)
        listener-release (promise)
        route {:close! #(swap! calls conj :route)
               :await-drained! (fn [_] (swap! calls conj :drain)
                                  {:outcome :drained})}
        coordinator
        (http/prepare-managed-http-shutdown
         {:route route :drain-timeout-ms 50
          :stop-listener! (fn []
                            (swap! calls conj :listener)
                            (deliver listener-entered true)
                            @listener-release
                            {:outcome :drained})
          :stop-host! (fn [] (swap! calls conj :host)
                        {:outcome :stopped :host-result :once})})
        first-call (future ((:stop! coordinator)))
        others (atom [])]
    (try
      (is (= true (deref listener-entered 2000 ::timeout)))
      (is (= :running ((:status coordinator))))
      (reset! others (vec (repeatedly 4 #(future ((:stop! coordinator))))))
      (deliver listener-release true)
      (is (= {:outcome :stopped :host-result :once}
             (deref first-call 5000 ::timeout)))
      (doseq [other @others]
        (is (= {:outcome :stopped :host-result :once}
               (deref other 5000 ::timeout))))
      (is (= :stopped ((:status coordinator))))
      (is (= {:outcome :stopped :host-result :once} ((:stop! coordinator))))
      (is (= [:route :listener :drain :host] @calls)
          "Concurrent and repeated shutdown must not repeat side effects")
      (finally
        (deliver listener-release true)
        (future-cancel first-call)
        (doseq [other @others] (future-cancel other))))))

(deftest managed-http-shutdown-coordinator-retries-before-provider-stop-test
  (let [calls (atom [])
        drain-attempts (atom 0)
        route {:close! #(swap! calls conj :route)
               :await-drained!
               (fn [_]
                 (swap! calls conj :drain)
                 (if (= 1 (swap! drain-attempts inc))
                   {:outcome :timed-out}
                   {:outcome :drained}))}
        coordinator
        (http/prepare-managed-http-shutdown
         {:route route :drain-timeout-ms 20
          :stop-listener! (fn [] (swap! calls conj :listener)
                            {:outcome :drained})
          :stop-host! (fn [] (swap! calls conj :host)
                        {:outcome :stopped :host-result :ok})})]
    (is (= :ready ((:status coordinator))))
    (is (= {:outcome :timed-out} ((:stop! coordinator))))
    (is (= :ready ((:status coordinator)))
        "A drain timeout may be retried once outstanding work finishes")
    (is (= [:route :listener :drain] @calls))
    (is (= {:outcome :stopped :host-result :ok} ((:stop! coordinator))))
    (is (= :stopped ((:status coordinator))))
    (is (= [:route :listener :drain :route :listener :drain :host] @calls))
    (is (= {:outcome :stopped :host-result :ok} ((:stop! coordinator))))
    (is (= 1 (count (filter #{:host} @calls)))))
  ;; Invalid plans must fail before creating a coordinator, without effects.
  (let [called (atom 0)
        plan {:route {:close! #(swap! called inc)
                      :await-drained! (fn [_] {:outcome :drained})}
              :stop-listener! (fn [] (swap! called inc)
                                {:outcome :drained})
              :stop-host! #(swap! called inc)
              :drain-timeout-ms 200001}]
    (is (= :invalid-drain-timeout-ms
           (exception-reason #(http/prepare-managed-http-shutdown plan))))
    (is (zero? @called))))

(deftest managed-http-host-stop-requires-confirmation-test
  ;; The external host stop callback may return normally after timing out or
  ;; after swallowing an error. Such results MUST NOT certify teardown.
  (doseq [unconfirmed [nil :ok false {:outcome :timed-out}
                       {:outcome :unknown} {:host-result :ok}]]
    (let [calls (atom [])
          coordinator
          (http/prepare-managed-http-shutdown
           {:route {:close! #(swap! calls conj :route)
                    :await-drained! (fn [_] (swap! calls conj :drain)
                                      {:outcome :drained})}
            :drain-timeout-ms 25
            :stop-listener! (fn [] (swap! calls conj :listener)
                              {:outcome :drained})
            :stop-host! (fn [] (swap! calls conj :host) unconfirmed)})]
      (is (= :unconfirmed-host-stop-outcome
             (exception-reason #((:stop! coordinator))))
          (str "A normal callback return is not confirmation: "
               (pr-str unconfirmed)))
      (is (= :host-stop-unknown ((:status coordinator)))
          "The provider could already have been partially stopped")
      (is (= :host-shutdown-outcome-unknown
             (exception-reason #((:stop! coordinator))))
          "Ambiguous teardown must not be replayed")
      (is (= [:route :listener :drain :host] @calls))))
  ;; The single-attempt primitive must reject the same unsafe outcome.
  (let [calls (atom [])
        plan {:route {:close! #(swap! calls conj :route)
                      :await-drained! (constantly {:outcome :drained})}
              :drain-timeout-ms 25
              :stop-listener! (constantly {:outcome :drained})
              :stop-host! (fn [] (swap! calls conj :host) nil)}]
    (is (= :unconfirmed-host-stop-outcome
           (exception-reason #(http/stop-managed-http-host! plan))))
    (is (= [:route :host] @calls))))

(deftest managed-http-shutdown-rejections-redact-callback-results-test
  ;; External callbacks may return app-private diagnostics or tokens. An
  ;; invalid drain/stop acknowledgment must not put that payload into
  ;; exception data, which callers may log or send to error reporters.
  (doseq [[boundary invalid-outcome expected-reason]
          [[:listener {:outcome "private-listener-outcome"
                       :secret "listener-token"}
            :invalid-listener-drain-outcome]
           [:drain {:outcome "private-drain-outcome"
                    :secret "drain-token"}
            :invalid-drain-outcome]
           [:host {:outcome "private-host-outcome"
                   :secret "host-token"}
            :unconfirmed-host-stop-outcome]]]
    (let [events (atom [])
          callback-result (fn [kind]
                            (if (= boundary kind)
                              invalid-outcome
                              {:outcome (if (= kind :host) :stopped :drained)}))
          coordinator
          (http/prepare-managed-http-shutdown
           {:route {:close! #(swap! events conj :route)
                    :await-drained! (fn [_]
                                      (swap! events conj :drain)
                                      (callback-result :drain))}
            :drain-timeout-ms 50
            :stop-listener! (fn []
                              (swap! events conj :listener)
                              (callback-result :listener))
            :stop-host! (fn []
                          (swap! events conj :host)
                          (callback-result :host))})
          thrown (try ((:stop! coordinator))
                      (catch clojure.lang.ExceptionInfo e e))
          public-data {:error/type :gesso.live.outbox/invalid-http-route
                       :reason expected-reason}]
      (is (instance? clojure.lang.ExceptionInfo thrown)
          (str boundary " must reject the invalid callback outcome"))
      (when (instance? clojure.lang.ExceptionInfo thrown)
        (is (= public-data (ex-data thrown))
            (str boundary " must expose only the stable typed rejection"))
        (is (not (.contains (pr-str (ex-data thrown)) "private-")))
        (is (not (.contains (pr-str (ex-data thrown)) "-token"))))
      (is (= (case boundary
               :listener [:route :listener]
               :drain [:route :listener :drain]
               :host [:route :listener :drain :host])
             @events)
          (str boundary " must not enter later teardown phases"))
      (is (= (if (= boundary :host)
               :host-stop-unknown :ready)
             ((:status coordinator)))
          "Only attempted host teardown must poison automatic retries")
      (when (= boundary :host)
        (is (= :host-shutdown-outcome-unknown
               (exception-reason #((:stop! coordinator)))))
        (is (= [:route :listener :drain :host] @events)
            "A failed confirmation must not replay host stop")))))

(deftest managed-http-shutdown-coordinator-fences-unknown-host-outcome-test
  (let [calls (atom [])
        failed-stop (ex-info "Host stop may have happened" {:test :v2149})
        route {:close! #(swap! calls conj :route)
               :await-drained! (fn [_] (swap! calls conj :drain)
                                  {:outcome :drained})}
        coordinator
        (http/prepare-managed-http-shutdown
         {:route route :drain-timeout-ms 50
          :stop-listener! (fn [] (swap! calls conj :listener)
                            {:outcome :drained})
          :stop-host! (fn [] (swap! calls conj :host)
                        (throw failed-stop))})]
    (is (identical? failed-stop
                    (try ((:stop! coordinator))
                         (catch Throwable error error))))
    (is (= :host-stop-unknown ((:status coordinator))))
    (is (= :host-shutdown-outcome-unknown
           (exception-reason #((:stop! coordinator)))))
    (is (= [:route :listener :drain :host] @calls)
        "Do not replay host teardown when the initial effect is ambiguous")))

(deftest real-http-ordinary-handler-must-drain-before-host-stop-test
  ;; A closed SSE route does not establish quiescence for unrelated Ring
  ;; requests. Exercise a real loopback HTTP handler that still owns host
  ;; resources when the coordinated shutdown first attempts listener drain.
  (setup-host
   (fn [started]
     (let [entered (promise)
           release (promise)
           in-flight (atom 0)
           events (atom [])
           listener-stopped? (atom false)
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :authorize-subscription (constantly scope)})
           ring-handler
           (fn [req]
             (if (= "/slow" (:uri req))
               (do
                 (swap! in-flight inc)
                 (deliver entered true)
                 (try
                   @release
                   {:status 200 :headers {} :body "ordinary response"}
                   (finally (swap! in-flight dec))))
               ((:handler route) req)))
           server (start-http! ring-handler)
           client (-> (HttpClient/newBuilder)
                      (.version HttpClient$Version/HTTP_1_1)
                      (.build))
           coordinator
           (http/prepare-managed-http-shutdown
            {:route route
             :drain-timeout-ms 2000
             :stop-listener!
             (fn []
               ;; This is an explicit, test-owned listener drain contract:
               ;; the callback may not certify drain while an HTTP handler
               ;; is still executing against the Gesso host.
               (swap! events conj [:listener @in-flight])
               (if (pos? @in-flight)
                 {:outcome :timed-out}
                 (do
                   (when (compare-and-set! listener-stopped? false true)
                     ((:close! server)))
                   {:outcome :drained})))
             :stop-host!
             (fn []
               (swap! events conj [:host @in-flight])
               {:outcome :stopped :host-result :approved})})
           pending (future (request client (:base server)
                                    "/slow" "GET" nil))]
       (try
         (is (= true (deref entered 5000 ::timeout))
             "A real network request must enter the ordinary Ring handler")
         (is (= 1 @in-flight))
         (is (= {:outcome :listener-timed-out}
                ((:stop! coordinator)))
             "SSE drain alone cannot certify an unrelated HTTP handler")
         (is (= :ready ((:status coordinator))))
         (is (= [[:listener 1]] @events)
             "No provider stop can occur while ordinary HTTP work remains")
         (is (false? (live/closed? (:gesso.live/system started))))
         (is (true? ((:drained? route)))
             "The SSE route can be fully drained while HTTP is still busy")
         (deliver release true)
         (let [response (deref pending 10000 ::timeout)]
           (is (not= ::timeout response))
           (when (not= ::timeout response)
             (with-open [body (.body response)]
               (is (= 200 (.statusCode response)))
               (is (= "ordinary response"
                      (String. (.readAllBytes body) StandardCharsets/UTF_8))))))
         (is (eventually #(zero? @in-flight)))
         (is (= {:outcome :stopped :host-result :approved}
                ((:stop! coordinator))))
         (is (= :stopped ((:status coordinator))))
         (is (= [[:listener 1] [:listener 0] [:host 0]] @events))
         (is (= {:outcome :stopped :host-result :approved}
                ((:stop! coordinator))))
         (is (= 1 (count (filter #(= :host (first %)) @events)))
             "A second call cannot repeat the already confirmed stop")
         (finally
           (deliver release true)
           (future-cancel pending)
           ((:close! route))
           (when-not @listener-stopped?
             ((:close! server)))
           (.close ^HttpClient client)))))))

(deftest managed-http-owner-shutdown-orders-listener-drain-and-host-test
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           events (atom [])
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :authorize-subscription (constantly scope)})
           server (start-http! (:handler route))
           client (-> (HttpClient/newBuilder)
                      (.version HttpClient$Version/HTTP_1_1)
                      (.build))
           response (request client (:base server)
                             "/live/events" "GET" "Bearer allowed")]
       (try
         (is (= 200 (.statusCode response)))
         (is (eventually #(= 1 ((:active-count route)))))
         (let [result (http/stop-managed-http-host!
                       {:route route :drain-timeout-ms 2000
                        :stop-listener!
                        (fn []
                          (swap! events conj [:listener ((:closed? route))])
                          ((:close! server))
                          ;; This loopback test has only the SSE request. A
                          ;; production listener must also drain its ordinary
                          ;; HTTP handlers before returning this outcome.
                          {:outcome :drained})
                        :stop-host!
                        (fn []
                          (swap! events conj
                                 [:host ((:drained? route))
                                  (:subscription-count (source/stats src))])
                          {:outcome :stopped
                           :host-result :host-stop-approved})})]
           (is (= {:outcome :stopped :host-result :host-stop-approved}
                  result))
           (is (= [[:listener true] [:host true 0]] @events))
           (is (true? ((:drained? route)))))
         (finally
           (.close (.body response))
           ((:close! route))
           ((:close! server))
           (.close ^HttpClient client)))))))

(deftest managed-http-owner-shutdown-timeout-preserves-providers-test
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           entered (promise)
           release (promise)
           events (atom [])
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :authorize-subscription
                           (fn [_] (deliver entered true) @release scope)})
           pending (future ((:handler route)
                            {:uri "/live/events" :request-method :get}))
           stop-listener! #(do (swap! events conj :listener)
                                {:outcome :drained})
           stop-host! #(do (swap! events conj :host)
                            {:outcome :stopped
                             :host-result :host-stop-approved})
           plan {:route route :drain-timeout-ms 20
                 :stop-listener! stop-listener!
                 :stop-host! stop-host!}]
       (try
         (is (= true (deref entered 5000 ::timeout)))
         (is (= {:outcome :timed-out}
                (http/stop-managed-http-host! plan)))
         (is (= [:listener] @events))
         (is (false? (live/closed? (:gesso.live/system started)))
             "Timed-out drain must retain the Live provider for cleanup")
         (is (zero? (:subscription-count (source/stats src))))
         (deliver release true)
         (is (= {:status 503 :headers {} :body "Live shutting down"}
                (deref pending 5000 ::timeout)))
         (is (= {:outcome :stopped :host-result :host-stop-approved}
                (http/stop-managed-http-host! plan)))
         (is (= [:listener :listener :host] @events))
         (finally
           (deliver release true)
           (future-cancel pending)
           ((:close! route))))))))

(deftest managed-http-owner-shutdown-rejects-unsafe-cleanup-test
  (let [events (atom [])
        route {:close! #(swap! events conj :route)
               :await-drained! (fn [_] {:outcome :drained})}
        base {:route route :drain-timeout-ms 10
              :stop-listener! #(do (swap! events conj :listener)
                                   {:outcome :drained})
              :stop-host! #(swap! events conj :host)}]
    (doseq [bad [(dissoc base :stop-listener!)
                 (assoc base :stop-host! nil)
                 (assoc base :unexpected true)]]
      (is (= :invalid-http-shutdown-plan
             (exception-reason #(http/stop-managed-http-host! bad)))))
    (doseq [timeout [-1 nil 1.5 120001]]
      (is (= :invalid-drain-timeout-ms
             (exception-reason #(http/stop-managed-http-host!
                                 (assoc base :drain-timeout-ms timeout))))))
    (is (empty? @events) "Invalid plans must not initiate shutdown")
    (let [failure (ex-info "route failed" {})
          listener-failure (ex-info "listener failed" {})
          failing (assoc base
                         :route {:close! #(throw failure)
                                 :await-drained! (:await-drained! route)}
                         :stop-listener!
                         (fn [] (swap! events conj :listener)
                           (throw listener-failure)))
          thrown (try (http/stop-managed-http-host! failing)
                      (catch Throwable e e))]
      (is (identical? failure thrown))
      (is (= [listener-failure]
             (vec (.getSuppressed ^Throwable thrown))))
      (is (= [:listener] @events))
      (is (not-any? #{:host} @events)))
    (reset! events [])
    (is (= {:outcome :listener-timed-out}
           (http/stop-managed-http-host!
            (assoc base :stop-listener! (fn []
                                          (swap! events conj :listener)
                                          {:outcome :timed-out})))))
    (is (= [:route :listener] @events))
    (is (not-any? #{:host} @events))
    (reset! events [])
    (is (= :invalid-listener-drain-outcome
           (exception-reason
            #(http/stop-managed-http-host!
              (assoc base :stop-listener! (constantly nil))))))
    (is (= [:route] @events))
    (is (not-any? #{:host} @events))))

(deftest managed-route-await-drain-is-bounded-and-observes-inflight-test
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           request {:uri "/live/events" :request-method :get}
           entered (promise)
           release (promise)
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :authorize-subscription
                           (fn [_] (deliver entered true) @release scope)})
           pending (atom nil)
           waiter (atom nil)]
       (try
         (is (= {:outcome :timed-out} ((:await-drained! route) 0))
             "An open route may be idle, but has not completed shutdown")
         (doseq [invalid [nil -1 1.5 120001 "20" (inc (bigint Long/MAX_VALUE))]]
           (is (= :invalid-drain-timeout-ms
                  (exception-reason #((:await-drained! route) invalid)))))
         (reset! pending (future ((:handler route) request)))
         (is (= true (deref entered 5000 ::timeout)))
         (is (= :closed ((:close! route))))
         (is (false? ((:drained? route))))
         (is (= {:outcome :timed-out} ((:await-drained! route) 0)))
         (is (= {:outcome :timed-out} ((:await-drained! route) 10))
             "A bounded drain must not claim progress during stalled auth")
         (reset! waiter (future ((:await-drained! route) 5000)))
         (is (not (realized? @waiter))
             "A drain waiter must remain pending until authorization exits")
         (deliver release true)
         (is (= {:status 503 :headers {} :body "Live shutting down"}
                (deref @pending 5000 ::timeout)))
         (is (= {:outcome :drained} (deref @waiter 5000 ::timeout))
             "A state transition should wake a blocked drain waiter")
         (is (true? ((:drained? route))))
         (is (= {:outcome :drained} ((:await-drained! route) 0))
             "Drain completion remains observable to future waiters")
         (is (zero? (:subscription-count (source/stats src))))
         (finally
           (deliver release true)
           (when @pending (future-cancel @pending))
           (when @waiter (future-cancel @waiter))
           ((:close! route))))))))

(deftest late-sse-rollback-retains-failed-cancellation-after-disconnect-test
  ;; close! may finish while a previously admitted SSE startup is blocked.
  ;; If that late stream closes and rollback cancellation fails, the route
  ;; must not report drain after its startup reservation is released.
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           entered (promise)
           release (promise)
           original-start live/start-sse!
           original-cancel live/cancel-sse!
           fail (ex-info "Late SSE cancellation failed" {:test :v2154})
           calls (atom 0)
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :authorize-subscription (constantly scope)})
           request {:uri "/live/events" :request-method :get}
           pending (atom nil)]
       (try
         (with-redefs [live/start-sse!
                       (fn [& args]
                         (deliver entered true)
                         @release
                         (apply original-start args))
                       live/cancel-sse!
                       (fn [session]
                         (swap! calls inc)
                         (stream/close! (:stream session))
                         (throw fail))]
           (reset! pending (future
                             (try ((:handler route) request)
                                  (catch Throwable e e))))
           (is (= true (deref entered 5000 ::timeout)))
           (is (= :closed ((:close! route))))
           (is (false? ((:drained? route)))
               "A late starter still owns an outstanding reservation")
           (deliver release true)
           (is (identical? fail (deref @pending 10000 ::timeout)))
           (is (pos? @calls) "Late rollback attempted cancellation")
           (is (zero? ((:active-count route)))
               "The disconnect hook removed the active entry")
           (is (false? ((:drained? route)))
               "Failed late rollback must remain a retry obligation")
           (is (= {:outcome :timed-out} ((:await-drained! route) 0))))
         ;; The original cancellation function is now restored. The next
         ;; route close must still find the failed, disconnected session.
         (is (= :closed ((:close! route))))
         (is (true? ((:drained? route))))
         (is (= {:outcome :drained} ((:await-drained! route) 0)))
         (is (eventually #(zero? (:subscription-count (source/stats src)))))
         (finally
           (deliver release true)
           (when @pending (future-cancel @pending))
           ((:close! route))))))))

(deftest route-retries-cancellation-failed-after-disconnect-hook-test
  ;; Closing a stream invokes its on-closed hook and removes the active
  ;; registry entry. If cancellation THEN fails, the route must retain a
  ;; separate retry obligation and cannot certify provider-safe drain.
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :authorize-subscription (constantly scope)})
           response ((:handler route)
                     {:uri "/live/events" :request-method :get})
           failure (ex-info "Cancellation failed after stream closed"
                            {:test :v2154})
           attempts (atom 0)
           original-cancel live/cancel-sse!]
       (try
         (is (= 200 (:status response)))
         (is (= 1 ((:active-count route))))
         (with-redefs [live/cancel-sse!
                       (fn [session]
                         (if (= 1 (swap! attempts inc))
                           (do
                             ;; The close hook removes :connections first.
                             (stream/close! (:stream session))
                             (throw failure))
                           (original-cancel session)))]
           (is (identical? failure
                           (try ((:close! route))
                                (catch Throwable error error))))
           (is (true? ((:closed? route))))
           (is (zero? ((:active-count route)))
               "The disconnect hook removed the active connection")
           (is (false? ((:drained? route)))
               "Failed cancellation remains an outstanding cleanup obligation")
           (is (= {:outcome :timed-out} ((:await-drained! route) 0)))
           (is (= :closed ((:close! route)))
               "Retry must reattempt cancellation even after disconnect")
           (is (= 2 @attempts)
               "A lost cancellation cannot silently become a successful retry")
           (is (true? ((:drained? route))))
           (is (= {:outcome :drained} ((:await-drained! route) 0))))
         (is (eventually #(zero? (:subscription-count (source/stats src)))))
         (finally ((:close! route))))))))

(deftest route-drain-waits-for-close-cancellation-completion-test
  ;; A stream may close and synchronously unregister itself before the
  ;; caller's cancellation operation returns. The route must not claim
  ;; quiescence or wake drain waiters until close! finishes its own pass.
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :authorize-subscription (constantly scope)})
           response ((:handler route)
                     {:uri "/live/events" :request-method :get})
           entered (promise)
           release (promise)
           original-cancel live/cancel-sse!
           closing (atom nil)]
       (try
         (is (= 200 (:status response)))
         (is (= 1 ((:active-count route))))
         (with-redefs [live/cancel-sse!
                       (fn [session]
                         ;; Simulate an adapter's close notification before
                         ;; the cancellation operation itself is finished.
                         (stream/close! (:stream session))
                         (deliver entered true)
                         @release
                         (original-cancel session))]
           (reset! closing (future ((:close! route))))
           (is (= true (deref entered 5000 ::timeout)))
           (is (eventually #(zero? ((:active-count route))))
               "The disconnect hook has already removed the last session")
           (is (true? ((:closed? route))))
           (is (false? ((:drained? route)))
               "Registry emptiness must not certify unfinished cancellation")
           (is (= {:outcome :timed-out} ((:await-drained! route) 20))
               "Drain wait must remain pending during cancellation")
           (is (not (realized? @closing)))
           (deliver release true)
           (is (= :closed (deref @closing 5000 ::timeout)))
           (is (true? ((:drained? route))))
           (is (= {:outcome :drained} ((:await-drained! route) 0)))
           (is (eventually #(zero? (:subscription-count
                                   (source/stats src))))))
         (finally
           (deliver release true)
           (when @closing (future-cancel @closing))
           ((:close! route))))))))

(deftest managed-route-await-drain-requires-last-active-session-test
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :authorize-subscription (constantly scope)})
           response ((:handler route) {:uri "/live/events"
                                       :request-method :get})]
       (try
         (is (= 200 (:status response)))
         (is (= 1 ((:active-count route))))
         (is (= {:outcome :timed-out} ((:await-drained! route) 0)))
         (is (= :closed ((:close! route))))
         (is (= {:outcome :drained} ((:await-drained! route) 2000)))
         (is (true? ((:drained? route))))
         (is (eventually #(zero? (:subscription-count (source/stats src)))))
         (finally ((:close! route))))))))

(deftest managed-route-drain-requires-closed-and-no-inflight-work-test
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           request {:uri "/live/events" :request-method :get}
           entered (promise)
           release (promise)
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :authorize-subscription
                           (fn [_]
                             (deliver entered true)
                             @release
                             scope)})
           pending (atom nil)]
       (try
         (is (false? ((:drained? route)))
             "An idle but open route is not shut down")
         (reset! pending (future ((:handler route) request)))
         (is (= true (deref entered 5000 ::timeout)))
         (is (= 1 ((:pending-authorization-count route))))
         (is (false? ((:drained? route))))
         (is (= :closed ((:close! route)))
             "Closing does not have to wait for application authorization")
         (is (true? ((:closed? route))))
         (is (false? ((:drained? route)))
             "Closed must not mean a blocked authorization has finished")
         (is (zero? ((:active-count route))))
         (is (zero? (:subscription-count (source/stats src))))
         (deliver release true)
         (is (= {:status 503 :headers {} :body "Live shutting down"}
                (deref @pending 5000 ::timeout)))
         (is (zero? ((:pending-authorization-count route))))
         (is (true? ((:drained? route)))
             "The last pending request releases the route drain barrier")
         (is (= :closed ((:close! route)))
             "A drained route remains idempotently closed")
         (finally
           (deliver release true)
           (when @pending (future-cancel @pending))
           ((:close! route))))))))

(deftest shutdown-fences-get-during-stalled-sse-startup-test
  ;; The SSE startup path holds the admission monitor. If it stalls, close!
  ;; must publish its closure before waiting for that monitor, preventing
  ;; further authorization and rejecting the in-flight startup at handoff.
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           request {:uri "/live/events" :request-method :get}
           entered (promise)
           release (promise)
           authorized (atom 0)
           original-start live/start-sse!
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :authorize-subscription
                           (fn [_] (swap! authorized inc) scope)})
           startup (atom nil)
           closing (atom nil)]
       (try
         (with-redefs [live/start-sse!
                       (fn [& args]
                         (deliver entered true)
                         @release
                         (apply original-start args))]
           (reset! startup (future ((:handler route) request)))
           (is (= true (deref entered 5000 ::timeout))
               "A permitted request must enter SSE startup under the lock")
           (reset! closing (future ((:close! route))))
           (is (eventually #((:closed? route)))
               "Shutdown must fence new GETs even when startup holds the lock")
           ;; Even the negative control must not hang the test runner when
           ;; the old implementation waits on the admission monitor.
           (let [next-request (future ((:handler route) request))]
             (try
               (is (= {:status 503 :headers {} :body "Live shutting down"}
                      (deref next-request 1000 ::timeout))
                   "An overloaded or closing route cannot keep authorizing")
               (finally (future-cancel next-request))))
           (is (= 1 @authorized))
           (is (= 1 ((:pending-authorization-count route)))
               "Stalled startup owns its reservation until it exits")
           (is (= :closed (deref @closing 1500 ::timeout))
               "Route shutdown must not wait for blocked SSE startup")
           (is (false? ((:drained? route)))
               "A completed close! cannot claim that blocked startup drained")
           (deliver release true)
           (is (= {:status 503 :headers {} :body "Live shutting down"}
                  (deref @startup 10000 ::timeout))
               "A stream started after close begins must never return 200")
           (is (= :closed (deref @closing 10000 ::timeout)))
           (is (zero? ((:pending-authorization-count route))))
           (is (zero? ((:active-count route))))
           (is (true? ((:drained? route)))
               "Late canceled startup must eventually complete route drain")
           (is (eventually #(zero? (:subscription-count (source/stats src))))))
         (finally
           (deliver release true)
           (when @startup (future-cancel @startup))
           (when @closing (future-cancel @closing))
           ((:close! route))))))))

(deftest closed-route-skips-authorization-and-inflight-admission-test
  (setup-host
   (fn [started]
     (let [source (:source (:gesso.live/system started))
           calls (atom 0)
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :authorize-subscription
                           (fn [_]
                             (swap! calls inc)
                             (throw (ex-info "Cannot authorize after shutdown"
                                             {:unexpected true})))})
           request {:uri "/live/events" :request-method :get}]
       (try
         ((:close! route))
         ;; The closed response is unconditional: no application callback,
         ;; even one that throws or would return nil, may preempt HTTP 503.
         (is (= {:status 503 :headers {} :body "Live shutting down"}
                ((:handler route) request)))
         (is (zero? @calls))
         (is (zero? ((:active-count route))))
         (is (zero? (:subscription-count (source/stats source))))
         (finally ((:close! route)))))
     ;; An authorization already in progress can finish after shutdown, but
     ;; the locked post-authorization admission check must still reject it.
     (let [source (:source (:gesso.live/system started))
           entered (promise)
           release (promise)
           calls (atom 0)
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :authorize-subscription
                           (fn [_]
                             (swap! calls inc)
                             (deliver entered true)
                             @release
                             scope)})
           request {:uri "/live/events" :request-method :get}
           pending (future ((:handler route) request))]
       (try
         (is (= true (deref entered 5000 ::timeout)))
         ((:close! route))
         (deliver release true)
         (is (= 503 (:status (deref pending 5000 ::timeout))))
         (is (= 1 @calls))
         (is (zero? ((:active-count route))))
         (is (zero? (:subscription-count (source/stats source))))
         (finally
           (deliver release true)
           (future-cancel pending)
           ((:close! route))))))))

(deftest shutdown-preempts-late-authorization-results-test
  (setup-host
   (fn [started]
     (let [source (:source (:gesso.live/system started))
           request {:uri "/live/events" :request-method :get}]
       ;; Both outcomes are legitimate callback results until the route
       ;; closes. In-flight callbacks cannot reopen a closed route, nor may
       ;; their late denial/invalid scope replace the shutdown response.
       (doseq [[label outcome] [[:denied nil]
                                [:invalid {:topic :request :id nil}]]]
         (let [entered (promise)
               release (promise)
               calls (atom 0)
               route (http/wrap-authorized-sse
                      started {:path "/live/events"
                               :authorize-subscription
                               (fn [_]
                                 (swap! calls inc)
                                 (deliver entered true)
                                 @release
                                 outcome)})
               pending (future ((:handler route) request))]
           (try
             (is (= true (deref entered 5000 ::timeout))
                 (str label " callback must enter before shutdown"))
             ((:close! route))
             (deliver release true)
             (is (= {:status 503 :headers {} :body "Live shutting down"}
                    (deref pending 5000 ::timeout))
                 (str label " must not override the shutdown response"))
             (is (= 1 @calls))
             (is (true? ((:closed? route))))
             (is (zero? ((:active-count route))))
             (is (zero? (:subscription-count (source/stats source))))
             (finally
               (deliver release true)
               (future-cancel pending)
               ((:close! route))))))
       ;; The same callback results retain their original semantics while
       ;; admission is open: denied requests are 403, malformed scopes fail
       ;; with the typed rejection, and neither acquires a source interest.
       (let [denied (http/wrap-authorized-sse
                     started {:path "/live/events"
                              :authorize-subscription (constantly nil)})
             malformed (http/wrap-authorized-sse
                        started {:path "/live/events"
                                 :authorize-subscription
                                 (constantly {:topic :request :id nil})})]
         (try
           (is (= 403 (:status ((:handler denied) request))))
           (is (= :invalid-authorized-subscription
                  (exception-reason #((:handler malformed) request))))
           (is (zero? (:subscription-count (source/stats source))))
           (finally
             ((:close! denied))
             ((:close! malformed)))))))))

(deftest shutdown-preempts-late-authorization-exception-test
  (setup-host
   (fn [started]
     (let [source (:source (:gesso.live/system started))
           request {:uri "/live/events" :request-method :get}
           failure (ex-info "Application provider failed during teardown"
                            {:failure :provider-stopped})
           entered (promise)
           release (promise)
           calls (atom 0)
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :authorize-subscription
                           (fn [_]
                             (swap! calls inc)
                             (deliver entered true)
                             @release
                             (throw failure))})
           pending (future ((:handler route) request))]
       (try
         (is (= true (deref entered 5000 ::timeout))
             "The callback must begin while the route is still open")
         ((:close! route))
         (deliver release true)
         (is (= {:status 503 :headers {} :body "Live shutting down"}
                (deref pending 5000 ::timeout))
             "A late ordinary callback exception cannot replace shutdown")
         (is (= 1 @calls))
         (is (true? ((:closed? route))))
         (is (zero? ((:active-count route))))
         (is (zero? (:subscription-count (source/stats source))))
         (finally
           (deliver release true)
           (future-cancel pending)
           ((:close! route))))
       ;; The very same exception must escape unchanged while the route is
       ;; open, rather than silently becoming a denial or success response.
       (let [open-route (http/wrap-authorized-sse
                         started {:path "/live/events"
                                  :authorize-subscription (fn [_] (throw failure))})]
         (try
           (is (identical? failure
                           (try ((:handler open-route) request)
                                (catch Exception e e))))
           (is (zero? ((:active-count open-route))))
           (is (zero? (:subscription-count (source/stats source))))
           (finally ((:close! open-route)))))))))

(deftest closed-live-provider-rejects-sse-before-and-after-authorization-test
  (setup-host
   (fn [started]
     (let [system (:gesso.live/system started)
           src (:source system)
           request {:uri "/live/events" :request-method :get}
           entered (promise)
           release (promise)
           calls (atom 0)
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :authorize-subscription
                           (fn [_]
                             (swap! calls inc)
                             (deliver entered true)
                             @release
                             scope)})
           pending (future ((:handler route) request))]
       (try
         ;; Although the HTTP route was created while Live was active,
         ;; its in-flight callback must not admit a new subscription after
         ;; the owning provider is closed independently of route shutdown.
         (is (= true (deref entered 5000 ::timeout)))
         (live/close! system)
         (is (true? (live/closed? system)))
         (deliver release true)
         (is (= {:status 503 :headers {} :body "Live shutting down"}
                (deref pending 5000 ::timeout)))
         (is (= 1 @calls))
         ;; A later GET must not even consult the callback (whose providers
         ;; could have disappeared). The route itself was not closed yet.
         (is (false? ((:closed? route))))
         (is (= {:status 503 :headers {} :body "Live shutting down"}
                ((:handler route) request)))
         (is (= 1 @calls))
         (is (zero? ((:active-count route))))
         (is (zero? (:subscription-count (source/stats src))))
         (finally
           (deliver release true)
           (future-cancel pending)
           ((:close! route))))))))

(deftest route-shutdown-retries-failed-cancellation-test
  (setup-host
   (fn [started]
     (let [source (:source (:gesso.live/system started))
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :authorize-subscription (fn [_] scope)})
           request {:uri "/live/events" :request-method :get}
           failure (ex-info "Injected session cancellation failure"
                            {:test :v2128})
           cancellation-attempts (atom 0)
           original-cancel live/cancel-sse!]
       (try
         (let [a ((:handler route) request)
               b ((:handler route) request)]
           (is (= 200 (:status a)))
           (is (= 200 (:status b)))
           (is (= 2 ((:active-count route))))
           (is (= 2 (:subscription-count (source/stats source))))
           ;; The first cancellation fails *before* closing its stream.
           ;; The remaining connection must still be canceled. A later
           ;; close! must find and retry the failed connection.
           (let [caught (try
                          (with-redefs [live/cancel-sse!
                                        (fn [session]
                                          (if (= 1 (swap! cancellation-attempts inc))
                                            (throw failure)
                                            (original-cancel session)))]
                            ((:close! route)))
                          (catch Throwable e e))]
             (is (identical? failure caught))
             (is (= 2 @cancellation-attempts)))
           (is (true? ((:closed? route))))
           (is (= 1 ((:active-count route)))
               "Failed cancellation must stay registered for retry")
           (is (eventually #(= 1 (:subscription-count (source/stats source)))))
           (is (= 503 (:status ((:handler route) request)))
               "Even partly failed shutdown must reject new SSE GETs")
           (is (= :closed ((:close! route)))
               "Retry must successfully cancel the retained session")
           (is (eventually #(zero? ((:active-count route)))))
           (is (eventually #(zero? (:subscription-count (source/stats source)))))
           (is (stream/closed? (:body a)))
           (is (stream/closed? (:body b)))
           (is (= :closed ((:close! route)))
               "Completed shutdown remains idempotent"))
         (finally ((:close! route))))))))

(deftest failed-disconnect-hook-registration-rolls-back-sse-test
  (setup-host
   (fn [started]
     (let [src (:source (:gesso.live/system started))
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :authorize-subscription (constantly scope)})
           request {:uri "/live/events" :request-method :get}
           original-start live/start-sse!
           original-cancel live/cancel-sse!
           acquired (atom nil)
           ;; Preserve the real session's cancellation handle but replace only
           ;; its exposed stream with one that cannot accept on-closed hooks.
           ;; This fails exactly after Live has allocated a real subscription.
           start-with-broken-hook
           (fn [& args]
             (let [session (apply original-start args)]
               (reset! acquired session)
               (assoc session :stream (Object.))))]
       (try
         (let [error (try
                       (with-redefs [live/start-sse! start-with-broken-hook]
                         ((:handler route) request))
                       (catch Throwable e e))]
           (is (instance? Throwable error)
               "Disconnect hook registration must fail")
           (is (some? @acquired) "A real SSE session was already started")
           (is (eventually #(zero? (:subscription-count (source/stats src))))
               "Failed hook registration must release its Live interest")
           (is (eventually #(zero? ((:active-count route))))
               "Failed admission must leave no registered connection")
           (is (stream/closed? (:stream @acquired))))
         ;; A cancellation failure while rolling back must not erase the
         ;; session. Retain it for a later explicit route close! retry and
         ;; report the failed cancellation as suppressed on the hook error.
         (let [cancel-failure (ex-info "Injected rollback cancel failure"
                                       {:test :v2129})
               error (try
                       (with-redefs [live/start-sse! start-with-broken-hook
                                     live/cancel-sse! (fn [_] (throw cancel-failure))]
                         ((:handler route) request))
                       (catch Throwable e e))]
           (is (instance? Throwable error))
           (is (some #(identical? cancel-failure %)
                     (.getSuppressed ^Throwable error))
               "Preserve the rollback failure on the original hook error")
           (is (= 1 ((:active-count route)))
               "Failed cancellation remains registered for retry")
           (is (= 1 (:subscription-count (source/stats src))))
           (is (false? ((:closed? route)))
               "Failed admission does not forcibly close the entire route")
           (is (= :closed ((:close! route))))
           (is (eventually #(zero? ((:active-count route)))))
           (is (eventually #(zero? (:subscription-count (source/stats src)))))
           (is (stream/closed? (:stream @acquired))))
         (finally ((:close! route))))))))

(deftest provider-closes-during-sse-startup-returns-503-test
  ;; This is a physical Live provider rather than a mock source. Replacing
  ;; only the startup seam allows exact scheduling of the provider close
  ;; between the route's pre-admission check and SSE startup/return.
  (doseq [close-point [:before-start :after-start]]
    (setup-host
     (fn [started]
       (let [sys (:gesso.live/system started)
             src (:source sys)
             request {:uri "/live/events" :request-method :get}
             original-start live/start-sse!
             authorized (atom 0)
             route (http/wrap-authorized-sse
                    started {:path "/live/events"
                             :authorize-subscription
                             (fn [_] (swap! authorized inc) scope)})]
         (try
           (let [response
                 (try
                   (with-redefs
                   [live/start-sse!
                    (fn [system subscription options]
                      (case close-point
                        :before-start (do
                                        (live/close! system)
                                        (original-start system subscription options))
                        :after-start (let [session
                                           (original-start system subscription options)]
                                       (live/close! system)
                                       session)))]
                     ((:handler route) request))
                   (catch Exception e e))]
             (is (= {:status 503 :headers {} :body "Live shutting down"}
                    response)
                 (str close-point " must not produce an unusable SSE response")))
           (is (true? (live/closed? sys)))
           (is (= 1 @authorized))
           (is (eventually #(zero? ((:active-count route))))
               "A closed provider cannot leave an admitted HTTP session")
           (is (eventually #(zero? (:subscription-count (source/stats src))))
               "The real source must retain no Live subscription")
           (finally ((:close! route))))))))
  ;; Do not hide a genuine start failure when the provider remains live.
  (setup-host
   (fn [started]
     (let [sys (:gesso.live/system started)
           src (:source sys)
           failure (ex-info "Injected SSE startup failure" {:test :v2130})
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :authorize-subscription (constantly scope)})]
       (try
         (is (identical? failure
                         (with-redefs [live/start-sse!
                                       (fn [& _] (throw failure))]
                           (try ((:handler route)
                                 {:uri "/live/events" :request-method :get})
                                (catch Exception e e)))))
         (is (false? (live/closed? sys)))
         (is (zero? ((:active-count route))))
         (is (zero? (:subscription-count (source/stats src))))
         (finally ((:close! route))))))))

(deftest already-terminal-sse-stream-rejected-before-http-handoff-test
  (setup-host
   (fn [started]
     (let [sys (:gesso.live/system started)
           src (:source sys)
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :authorize-subscription (constantly scope)})
           request {:uri "/live/events" :request-method :get}
           original-start live/start-sse!]
       (try
         ;; A real SSE session can terminate synchronously without the Live
         ;; provider closing, e.g. an early transport cancellation/failure.
         ;; Ring must not advertise its already-closed body as HTTP 200.
         (doseq [termination [:cancel-session :close-output]]
           (let [started-session (atom nil)
                 response (with-redefs
                            [live/start-sse!
                             (fn [system subscription options]
                               (let [session
                                     (original-start system subscription options)]
                                 (reset! started-session session)
                                 (case termination
                                   :cancel-session (live/cancel-sse! session)
                                   :close-output (stream/close! (:stream session)))
                                 session))]
                            ((:handler route) request))]
             (is (= {:status 503 :headers {} :body "Live stream unavailable"}
                    response)
                 (str termination " must not return HTTP 200 with closed body"))
             (is (some? @started-session))
             (is (stream/closed? (:stream @started-session)))
             (is (false? (live/closed? sys)))
             (is (eventually #(zero? ((:active-count route)))))
             (is (eventually #(zero? (:subscription-count (source/stats src)))))))
         ;; Live is still healthy: a subsequent ordinary SSE admission must
         ;; return an active 200 stream, not poison the entire route.
         (let [response ((:handler route) request)]
           (is (= 200 (:status response)))
           (is (false? (stream/closed? (:body response))))
           (is (= 1 ((:active-count route))))
           (is (= 1 (:subscription-count (source/stats src))))
           (stream/close! (:body response))
           (is (eventually #(zero? ((:active-count route)))))
           (is (eventually #(zero? (:subscription-count (source/stats src))))))
         (finally ((:close! route))))))))

(deftest route-shutdown-cancels-active-sessions-and-rejects-new-get-test
  (setup-host
   (fn [started]
     (let [source (:source (:gesso.live/system started))
           route (http/wrap-authorized-sse
                  started {:path "/live/events"
                           :authorize-subscription (fn [_] scope)})]
       (try
         (let [response ((:handler route) {:uri "/live/events"
                                            :request-method :get})]
           (is (= 200 (:status response)))
           (is (= 1 ((:active-count route))))
           (is (= 1 (:subscription-count (source/stats source))))
           (is (false? (stream/closed? (:body response))))
           ((:close! route))
           (is (true? ((:closed? route))))
           (is (stream/closed? (:body response)))
           (is (eventually #(zero? ((:active-count route)))))
           (is (eventually #(zero? (:subscription-count
                                   (source/stats source)))))
           (is (= 503 (:status ((:handler route)
                               {:uri "/live/events" :request-method :get}))))
           (is (zero? (:subscription-count (source/stats source))))
           (is (= :closed ((:close! route)))))
         (finally ((:close! route))))))))
