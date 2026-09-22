(ns gesso.live.browser.fixture-server
  "Deterministic HTTP/SSE fixture for real-browser Gesso Live tests.

   The fixture is deliberately test-only and independent of Aleph/Biff. Its job
   is not to emulate the production web server; it is to give browser tests an
   explicit controller for otherwise timing-sensitive network orderings.

   Ordinary requests are driven by per-method/per-path scripts. A script action
   may respond immediately, hold the request until the test releases it, or
   close the exchange without an HTTP response. Every request is recorded before
   its scripted action runs.

   SSE connections are real HTTP event-stream responses. Tests can wait for a
   client to connect, emit named events, and force connection loss/reconnect.

   No sleeps participate in ordering. Tests synchronize on recorded requests,
   held requests, and SSE connection state."
  (:require
   [clojure.string :as str])
  (:import
   (com.sun.net.httpserver Headers HttpExchange HttpHandler HttpServer)
   (java.io IOException OutputStream)
   (java.net InetSocketAddress URI)
   (java.nio.charset StandardCharsets)
   (java.util Base64 UUID)
   (java.util.concurrent CompletableFuture Executors ThreadFactory TimeUnit)
   (java.util.concurrent.atomic AtomicLong)))

(def fixture-type
  :gesso.live.browser.fixture-server/fixture)

(def fixture-version
  1)

(def default-host
  "127.0.0.1")

(def default-await-ms
  5000)

(def default-response
  {:status 404
   :headers {"content-type" "text/plain; charset=utf-8"}
   :body "fixture route not scripted"})

(def ^:private stop-sentinel
  ::stop)

;; =============================================================================
;; Errors / validation
;; =============================================================================

(defn- fixture-error
  [kind message data]
  (ex-info
   message
   (merge
    {:error/type :gesso.live.browser.fixture-server/error
     :error/kind kind}
    data)))

(defn fixture?
  [value]
  (and
   (map? value)
   (= fixture-type (:gesso.live.browser.fixture-server/type value))
   (= fixture-version (:gesso.live.browser.fixture-server/version value))
   (instance? HttpServer (:server value))))

(defn- require-fixture!
  [fixture]
  (when-not (fixture? fixture)
    (throw
     (fixture-error
      :invalid-fixture
      "Expected a running Gesso browser fixture server."
      {:fixture fixture})))
  fixture)

(defn- require-nonblank-string!
  [label value]
  (when-not (and (string? value)
                 (not (str/blank? value)))
    (throw
     (fixture-error
      :invalid-string
      (str label " must be a nonblank string.")
      {:label label
       :value value})))
  value)

(defn- normalize-method
  [method]
  (let [method
        (cond
          (keyword? method) method
          (string? method) (keyword (str/lower-case method))
          :else nil)]
    (when-not (keyword? method)
      (throw
       (fixture-error
        :invalid-method
        "HTTP method must be a keyword or string."
        {:method method})))
    method))

(defn- normalize-path
  [path]
  (let [path (require-nonblank-string! "Path" path)]
    (when-not (str/starts-with? path "/")
      (throw
       (fixture-error
        :invalid-path
        "Fixture HTTP paths must begin with '/'."
        {:path path})))
    path))

(defn- require-timeout!
  [timeout-ms]
  (when-not (and (integer? timeout-ms)
                 (not (neg? timeout-ms)))
    (throw
     (fixture-error
      :invalid-timeout
      "Timeout must be a non-negative integer number of milliseconds."
      {:timeout-ms timeout-ms})))
  timeout-ms)

;; =============================================================================
;; Response / script representation
;; =============================================================================

(defn- status-forbids-response-body?
  [status]
  (or (<= 100 status 199)
      (contains? #{204 205 304} status)))

(defn- nonempty-body?
  [body]
  (cond
    (nil? body) false
    (string? body) (not (empty? body))
    (= (Class/forName "[B") (class body)) (pos? (alength ^bytes body))
    :else false))

(defn response
  "Construct one finite fixture HTTP response.

   body may be nil, a string, or a byte array. Header names and values are
   converted to strings when written."
  ([status]
   (response status nil nil))
  ([status body]
   (response status nil body))
  ([status headers body]
   (when-not (and (integer? status)
                  (<= 100 status 599))
     (throw
      (fixture-error
       :invalid-status
       "HTTP response status must be an integer from 100 through 599."
       {:status status})))
   (when-not (or (nil? headers) (map? headers))
     (throw
      (fixture-error
       :invalid-headers
       "HTTP response headers must be a map or nil."
       {:headers headers})))
   (when-not (or (nil? body)
                 (string? body)
                 (= (Class/forName "[B") (class body)))
     (throw
      (fixture-error
       :invalid-body
       "HTTP response body must be nil, a string, or a byte array."
       {:body-type (some-> body class str)})))
   (when (and (status-forbids-response-body? status)
              (nonempty-body? body))
     (throw
      (fixture-error
       :body-forbidden-for-status
       "HTTP response status does not permit a response body."
       {:status status})))
   {:status status
    :headers (or headers {})
    :body body}))

(defn respond
  "Script action: respond immediately with response-map."
  [response-map]
  {:kind :respond
   :response response-map})

(defn hold
  "Script action: record and hold the request until release! is called.

   response-map is the response used when release! does not supply an override."
  [response-map]
  {:kind :hold
   :response response-map})

(defn close
  "Script action: close the HTTP exchange without sending a response."
  []
  {:kind :close})

(defn- normalize-response!
  [value]
  (when-not (and (map? value)
                 (contains? value :status))
    (throw
     (fixture-error
      :invalid-response
      "Fixture response must be produced by response or have the same shape."
      {:response value})))
  ;; Reconstruct through response so hand-written maps receive the same checks.
  (response (:status value)
            (:headers value)
            (:body value)))

(defn- normalize-action!
  [action]
  (when-not (map? action)
    (throw
     (fixture-error
      :invalid-script-action
      "Fixture script action must be a map."
      {:action action})))
  (case (:kind action)
    :respond
    {:kind :respond
     :response (normalize-response! (:response action))}

    :hold
    {:kind :hold
     :response (normalize-response! (:response action))}

    :close
    {:kind :close}

    (throw
     (fixture-error
      :invalid-script-kind
      "Unsupported fixture script action kind."
      {:action action
       :supported #{:respond :hold :close}}))))

(defn- action-seq!
  [actions]
  (let [actions
        (cond
          (and (map? actions) (contains? actions :kind)) [actions]
          (sequential? actions) (vec actions)
          :else nil)]
    (when-not (seq actions)
      (throw
       (fixture-error
        :invalid-script
        "Fixture script must contain at least one action."
        {:actions actions})))
    (mapv normalize-action! actions)))

;; =============================================================================
;; Request observation / deterministic waits
;; =============================================================================

(defn requests
  "Return the immutable public request records observed so far."
  [fixture]
  @(:requests (require-fixture! fixture)))

(defn request-by-id
  [fixture request-id]
  (some #(when (= request-id (:request-id %)) %) (requests fixture)))

(defn pending-request-ids
  [fixture]
  (set (keys @(:pending (require-fixture! fixture)))))

(defn- signal!
  [fixture]
  (locking (:monitor fixture)
    (.notifyAll ^Object (:monitor fixture)))
  true)

(defn- await-value!
  [fixture timeout-ms description f]
  (let [fixture (require-fixture! fixture)
        timeout-ms (require-timeout! timeout-ms)
        deadline (+ (System/nanoTime)
                    (* 1000000 timeout-ms))]
    (locking (:monitor fixture)
      (loop []
        (if-let [value (f)]
          value
          (let [remaining-ns (- deadline (System/nanoTime))]
            (when-not (pos? remaining-ns)
              (throw
               (fixture-error
                :await-timeout
                (str "Timed out waiting for " description ".")
                {:timeout-ms timeout-ms
                 :description description})))
            (let [remaining-ms (max 1 (long (Math/ceil (/ remaining-ns 1000000.0))))]
              (.wait ^Object (:monitor fixture) remaining-ms)
              (recur))))))))

(defn await-request!
  "Wait until an observed request satisfies predicate and return that record.

   This is non-consuming: multiple assertions may wait for the same request."
  ([fixture predicate]
   (await-request! fixture predicate default-await-ms))
  ([fixture predicate timeout-ms]
   (when-not (ifn? predicate)
     (throw
      (fixture-error
       :invalid-predicate
       "Request predicate must be callable."
       {:predicate predicate})))
   (await-value!
    fixture timeout-ms "matching HTTP request"
    #(some (fn [request]
             (when (predicate request) request))
           (requests fixture)))))

(defn await-pending!
  "Wait until one held request is pending and return its public request record.

   With predicate, waits for a pending request whose public record satisfies it."
  ([fixture]
   (await-pending! fixture (constantly true) default-await-ms))
  ([fixture predicate]
   (await-pending! fixture predicate default-await-ms))
  ([fixture predicate timeout-ms]
   (await-value!
    fixture timeout-ms "held HTTP request"
    #(let [pending @(:pending (require-fixture! fixture))]
       (some
        (fn [[request-id _control]]
          (let [request (request-by-id fixture request-id)]
            (when (and request (predicate request)) request)))
        pending)))))

;; =============================================================================
;; Request scripting / release
;; =============================================================================

(defn script!
  "Replace the queued actions for one [method path].

   Requests consume actions in order. Once the queue is empty the fixture's
   default response is used."
  [fixture method path actions]
  (let [fixture (require-fixture! fixture)
        route [(normalize-method method) (normalize-path path)]
        actions (action-seq! actions)]
    (swap! (:scripts fixture) assoc route actions)
    route))

(defn enqueue!
  "Append actions to the existing script for one [method path]."
  [fixture method path actions]
  (let [fixture (require-fixture! fixture)
        route [(normalize-method method) (normalize-path path)]
        actions (action-seq! actions)]
    (swap! (:scripts fixture) update route (fnil into []) actions)
    route))

(defn clear-script!
  [fixture method path]
  (let [fixture (require-fixture! fixture)
        route [(normalize-method method) (normalize-path path)]]
    (swap! (:scripts fixture) dissoc route)
    route))

(defn release!
  "Release one held request.

   response-map overrides the response captured by its hold action.

   Passing :close closes the exchange before sending response headers. A browser
   may transparently retry an idempotent GET in that case, so :close is useful for
   testing pre-response connection loss but is not a deterministic way to force an
   XHR/EventSource-visible transport error.

   Passing :truncate commits the held response's status, headers, and full
   Content-Length, writes only a strict prefix of its non-empty body, and then
   closes the exchange. Once response headers have been observed the browser
   cannot safely replay the request as though no response existed, making this the
   preferred deterministic fault for tests that require HTMX sendError."
  ([fixture request-id]
   (release! fixture request-id nil))
  ([fixture request-id response-map]
   (let [fixture (require-fixture! fixture)
         control (get @(:pending fixture) request-id)]
     (when-not control
       (throw
        (fixture-error
         :unknown-pending-request
         "No held fixture request has that request id."
         {:request-id request-id
          :pending-request-ids (pending-request-ids fixture)})))
     (let [value
           (cond
             (= :close response-map)
             :close

             (= :truncate response-map)
             {:fixture/release-kind :truncate
              :response (:response control)}

             (nil? response-map)
             (:response control)

             :else
             (normalize-response! response-map))]
       (.complete ^CompletableFuture (:release control) value)
       true))))

;; =============================================================================
;; HTTP request / response internals
;; =============================================================================

(defn- header-map
  [^Headers headers]
  (into {}
        (map (fn [[name values]]
               [(str/lower-case (str name)) (vec values)]))
        headers))

(defn- read-body
  [^HttpExchange exchange]
  (with-open [input (.getRequestBody exchange)]
    (slurp input :encoding "UTF-8")))

(defn- request-record*
  [fixture ^HttpExchange exchange action]
  (let [^URI uri (.getRequestURI exchange)
        request-id (.incrementAndGet ^AtomicLong (:next-request-id fixture))]
    {:request-id request-id
     :method (keyword (str/lower-case (.getRequestMethod exchange)))
     :path (.getPath uri)
     :raw-query (.getRawQuery uri)
     :headers (header-map (.getRequestHeaders exchange))
     :body (read-body exchange)
     :script-kind (:kind action)
     :remote-address (str (.getRemoteAddress exchange))}))

(defn- body-bytes
  [body]
  (cond
    (nil? body) (byte-array 0)
    (string? body) (.getBytes ^String body StandardCharsets/UTF_8)
    :else body))

(defn- add-response-headers!
  [^HttpExchange exchange headers]
  (doseq [[name value] headers]
    (let [values (if (sequential? value) value [value])]
      (doseq [v values]
        (.add (.getResponseHeaders exchange) (str name) (str v)))))
  true)

(defn- write-truncated-response!
  [^HttpExchange exchange response-map]
  (let [{:keys [status headers body]} (normalize-response! response-map)
        bytes (body-bytes body)
        length (alength bytes)]
    (when (status-forbids-response-body? status)
      (throw
       (fixture-error
        :truncate-body-forbidden
        "Cannot truncate a response status that forbids a response body."
        {:status status})))
    (when (< length 2)
      (throw
       (fixture-error
        :truncate-body-too-short
        "Truncated fixture responses require at least two response-body bytes."
        {:status status
         :body-length length})))
    (add-response-headers! exchange headers)
    ;; Declare the complete body length, then deliberately send only a strict
    ;; prefix. The client has now observed an HTTP response and cannot safely
    ;; replay an idempotent request as a pre-response connection retry.
    (.sendResponseHeaders exchange status length)
    (let [prefix-length (max 1 (quot length 2))
          output (.getResponseBody exchange)]
      (try
        (.write output bytes 0 prefix-length)
        (.flush output)
        (finally
          ;; Closing the fixed-length response stream itself may complain that
          ;; fewer bytes than declared were written. Closing the exchange is the
          ;; transport fault we actually want and reliably tears down the socket.
          (.close exchange))))
    true))

(defn- write-response!
  [^HttpExchange exchange response-map]
  (let [{:keys [status headers body]} (normalize-response! response-map)
        bytes (body-bytes body)]
    (add-response-headers! exchange headers)
    (if (status-forbids-response-body? status)
      (do
        ;; HttpServer warns and rewrites 204/304-style responses when given a
        ;; zero content length. -1 is its explicit no-response-body mode.
        (.sendResponseHeaders exchange status -1)
        (.close exchange))
      (do
        (.sendResponseHeaders exchange status (alength bytes))
        (if (pos? (alength bytes))
          (with-open [output (.getResponseBody exchange)]
            (.write output bytes))
          (.close exchange))))
    true))

(defn- pop-action!
  [fixture method path]
  (let [route [method path]
        scripts (:scripts fixture)]
    (locking scripts
      (let [queue (get @scripts route)]
        (if (seq queue)
          (let [action (first queue)
                remainder (subvec (vec queue) 1)]
            (if (seq remainder)
              (swap! scripts assoc route remainder)
              (swap! scripts dissoc route))
            action)
          {:kind :respond
           :response (:default-response fixture)})))))

(defn- handle-scripted-request!
  [fixture ^HttpExchange exchange]
  (let [^URI uri (.getRequestURI exchange)
        method (keyword (str/lower-case (.getRequestMethod exchange)))
        path (.getPath uri)
        action (pop-action! fixture method path)
        request (request-record* fixture exchange action)
        request-id (:request-id request)]
    (swap! (:requests fixture) conj request)
    (signal! fixture)
    (case (:kind action)
      :respond
      (write-response! exchange (:response action))

      :close
      (.close exchange)

      :hold
      (let [release (CompletableFuture.)
            control {:release release
                     :response (:response action)}]
        (swap! (:pending fixture) assoc request-id control)
        (signal! fixture)
        (try
          (let [released (.get release)]
            (cond
              (= stop-sentinel released)
              (.close exchange)

              (= :close released)
              (.close exchange)

              (= :truncate (:fixture/release-kind released))
              (write-truncated-response!
               exchange
               (:response released))

              :else
              (write-response! exchange released)))
          (finally
            (swap! (:pending fixture) dissoc request-id)
            (signal! fixture)))))))

;; =============================================================================
;; SSE
;; =============================================================================

(def ^:private sse-path-prefix
  "/__gesso_fixture/sse/")

(defn- encode-sse-client-id
  [client-id]
  (let [client-id
        (require-nonblank-string! "SSE client id" (str client-id))]
    (.encodeToString
     (.withoutPadding (Base64/getUrlEncoder))
     (.getBytes ^String client-id StandardCharsets/UTF_8))))

(defn- decode-sse-client-id
  [encoded]
  (String.
   (.decode (Base64/getUrlDecoder) encoded)
   StandardCharsets/UTF_8))

(defn sse-path
  [client-id]
  (str sse-path-prefix
       (encode-sse-client-id client-id)))

(defn- sse-client-id-from-path
  [path]
  (when (str/starts-with? path sse-path-prefix)
    (let [encoded (subs path (count sse-path-prefix))]
      (when-not (str/blank? encoded)
        (try
          (decode-sse-client-id encoded)
          (catch IllegalArgumentException _
            nil))))))

(defn sse-connections
  "Return public metadata for currently open SSE connections."
  [fixture]
  (->> @(:sse-connections (require-fixture! fixture))
       vals
       (mapv #(select-keys % [:connection-id :client-id]))))

(defn await-sse-client!
  ([fixture client-id]
   (await-sse-client! fixture client-id default-await-ms))
  ([fixture client-id timeout-ms]
   (let [client-id (str client-id)]
     (await-value!
      fixture timeout-ms (str "SSE client " (pr-str client-id))
      #(some (fn [connection]
               (when (= client-id (:client-id connection)) connection))
             (sse-connections fixture))))))

(defn- sse-frame
  [{:keys [event data id retry]}]
  (str
   (when (some? id) (str "id: " id "\n"))
   (when (some? event) (str "event: " event "\n"))
   (when (some? retry) (str "retry: " retry "\n"))
   (->> (str/split-lines (str (or data "")))
        (map #(str "data: " % "\n"))
        (apply str))
   "\n"))

(defn- emit-sse-to-connections!
  [fixture connections event]
  (let [bytes (.getBytes ^String (sse-frame event) StandardCharsets/UTF_8)]
    (reduce
     (fn [sent {:keys [connection-id output close-latch]}]
       (try
         (locking output
           (.write ^OutputStream output bytes)
           (.flush ^OutputStream output))
         (inc sent)
         (catch IOException _
           (swap! (:sse-connections fixture) dissoc connection-id)
           (.countDown ^java.util.concurrent.CountDownLatch close-latch)
           sent)))
     0
     connections)))

(defn emit-sse-connection!
  "Emit one SSE frame to exactly one currently open connection.

   connection-id must be one of the ids returned by sse-connections or
   await-sse-client!. Returns 1 when the frame was written and 0 when that
   connection is no longer open or the write fails. Failed connections are
   closed and removed. This is a deterministic test-control primitive for
   multi-client scenarios; client-id broadcast semantics remain owned by
   emit-sse!."
  [fixture connection-id event]
  (let [fixture (require-fixture! fixture)
        connection-id (str connection-id)
        connection (get @(:sse-connections fixture) connection-id)]
    (if connection
      (emit-sse-to-connections! fixture [connection] event)
      0)))

(defn emit-sse!
  "Emit one SSE frame to every currently open connection for client-id.

   Returns the number of connections successfully written. Failed connections
   are closed and removed."
  [fixture client-id event]
  (let [fixture (require-fixture! fixture)
        client-id (str client-id)
        connections
        (filter #(= client-id (:client-id %))
                (vals @(:sse-connections fixture)))]
    (emit-sse-to-connections! fixture connections event)))

(defn close-sse!
  "Force-close all current SSE connections for client-id.

   Returns the number of connections closed."
  [fixture client-id]
  (let [fixture (require-fixture! fixture)
        client-id (str client-id)
        connections
        (filter #(= client-id (:client-id %))
                (vals @(:sse-connections fixture)))]
    (doseq [{:keys [connection-id output close-latch]} connections]
      (swap! (:sse-connections fixture) dissoc connection-id)
      (try
        (.close ^OutputStream output)
        (catch IOException _ nil))
      (.countDown ^java.util.concurrent.CountDownLatch close-latch))
    (signal! fixture)
    (count connections)))

(defn- handle-sse!
  [fixture ^HttpExchange exchange client-id]
  (let [connection-id (str (UUID/randomUUID))
        close-latch (java.util.concurrent.CountDownLatch. 1)]
    (add-response-headers!
     exchange
     {"content-type" "text/event-stream; charset=utf-8"
      "cache-control" "no-cache"
      "connection" "keep-alive"})
    ;; Length zero selects streaming/chunked transfer for HttpServer.
    (.sendResponseHeaders exchange 200 0)
    (let [output (.getResponseBody exchange)
          _ (.flush ^OutputStream output)
          connection {:connection-id connection-id
                      :client-id client-id
                      :output output
                      :close-latch close-latch}]
      (swap! (:sse-connections fixture) assoc connection-id connection)
      (signal! fixture)
      (try
        ;; Keep ownership of HttpExchange on this fixture worker until the test
        ;; explicitly closes it or the fixture is stopped.
        (.await close-latch)
        (finally
          (swap! (:sse-connections fixture) dissoc connection-id)
          (try
            (.close ^OutputStream output)
            (catch IOException _ nil))
          (.close exchange)
          (signal! fixture))))))

;; =============================================================================
;; Lifecycle / URLs
;; =============================================================================

(defn- daemon-thread-factory
  []
  (let [counter (AtomicLong.)]
    (reify ThreadFactory
      (newThread [_ runnable]
        (doto (Thread. runnable
                       (str "gesso-browser-fixture-"
                            (.incrementAndGet counter)))
          (.setDaemon true))))))

(defn start!
  "Start a deterministic fixture server.

   Options:

     :host              bind host, default 127.0.0.1
     :port              bind port, default 0 (ephemeral)
     :default-response  finite response used for unscripted ordinary routes

   The returned fixture is the controller used by script!, await-request!,
   release!, emit-sse!, close-sse!, url, and stop!."
  ([]
   (start! {}))
  ([options]
   (when-not (map? options)
     (throw
      (fixture-error
       :invalid-options
       "Fixture server options must be a map."
       {:options options})))
   (let [unknown (seq (remove #{:host :port :default-response} (keys options)))]
     (when unknown
       (throw
        (fixture-error
         :unknown-options
         "Fixture server options contain unsupported keys."
         {:unknown-keys (set unknown)}))))
   (let [host (or (:host options) default-host)
         _ (require-nonblank-string! "Host" host)
         port (get options :port 0)
         _ (when-not (and (integer? port) (<= 0 port 65535))
             (throw
              (fixture-error
               :invalid-port
               "Fixture server port must be an integer from 0 through 65535."
               {:port port})))
         default-response (normalize-response!
                           (or (:default-response options) default-response))
         server (HttpServer/create (InetSocketAddress. host port) 0)
         executor (Executors/newCachedThreadPool (daemon-thread-factory))
         fixture
         {:gesso.live.browser.fixture-server/type fixture-type
          :gesso.live.browser.fixture-server/version fixture-version
          :server server
          :executor executor
          :host host
          :default-response default-response
          :monitor (Object.)
          :scripts (atom {})
          :requests (atom [])
          :pending (atom {})
          :sse-connections (atom {})
          :next-request-id (AtomicLong.)}
         handler
         (reify HttpHandler
           (handle [_ exchange]
             (try
               (let [path (.getPath (.getRequestURI ^HttpExchange exchange))]
                 (if-let [client-id (sse-client-id-from-path path)]
                   (handle-sse! fixture exchange client-id)
                   (handle-scripted-request! fixture exchange)))
               (catch Throwable error
                 ;; A fixture bug should become an observable HTTP failure, not a
                 ;; silently abandoned connection whenever headers are still writable.
                 (try
                   (write-response!
                    exchange
                    (response
                     500
                     {"content-type" "text/plain; charset=utf-8"}
                     (str "fixture server error: " (.getMessage error))))
                   (catch Throwable _
                     (try (.close ^HttpExchange exchange)
                          (catch Throwable _ nil))))))))]
     (.setExecutor server executor)
     (.createContext server "/" handler)
     (.start server)
     (let [actual-port (.getPort (.getAddress server))]
       (assoc fixture
              :port actual-port
              :base-url (str "http://" host ":" actual-port))))))

(defn base-url
  [fixture]
  (:base-url (require-fixture! fixture)))

(defn url
  [fixture path]
  (str (base-url fixture) (normalize-path path)))

(defn sse-url
  [fixture client-id]
  (url fixture (sse-path client-id)))

(defn stop!
  "Stop the fixture and release all blocked HTTP/SSE workers.

   Safe to call more than once on a fixture value returned by start!."
  [fixture]
  (let [fixture (require-fixture! fixture)]
    (doseq [[_request-id {:keys [release]}] @(:pending fixture)]
      (.complete ^CompletableFuture release stop-sentinel))
    (doseq [client-id (distinct (map :client-id (sse-connections fixture)))]
      (close-sse! fixture client-id))
    (.stop ^HttpServer (:server fixture) 0)
    (.shutdownNow ^java.util.concurrent.ExecutorService (:executor fixture))
    (.awaitTermination ^java.util.concurrent.ExecutorService (:executor fixture)
                       1000
                       TimeUnit/MILLISECONDS)
    (signal! fixture)
    true))
