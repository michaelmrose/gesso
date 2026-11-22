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
