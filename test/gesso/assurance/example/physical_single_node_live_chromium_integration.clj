(ns gesso.assurance.example.physical-single-node-live-chromium-integration
  "v1506: Real Chromium/HTMX/SSE integration against embedded XTDB.

   This is a dedicated real-browser gate, deliberately NOT an ordinary *_test
   namespace. Call -main after building the production browser artifact.
   Its loopback HTTP bridge delegates to v1505's *actual* :live-handler and
   forwards its Manifold SSE body over a real socket. Playwright controls only
   the browser; it does not mock HTTP, EventSource, HTMX, or database state.

   The bridge is an isolated test transport, not Biff host-adoption evidence.
   The v1495/v1505 fixture is still uncertified: no principal/CSRF checking,
   authenticated commit receipt, complete writer inventory, or universal
   refinement is established by successful browser execution."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests testing]]
            [gesso.assurance.example.physical-single-node-live :as proving]
            [gesso.live.browser.chromium :as chromium]
            [manifold.stream :as stream])
  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.io BufferedReader IOException InputStreamReader)
           (java.net HttpURLConnection InetSocketAddress URL)
           (java.nio.charset StandardCharsets)
           (java.util.concurrent Executors ThreadFactory TimeUnit)
           (com.microsoft.playwright Page)))

(def ^:private reservation-uri "/__gesso_proving/slot/reserve")
(def ^:private release-uri "/__gesso_proving/slot/release")
(def ^:private network-timeout-ms 12000)

(defn- reject! [reason data]
  (throw (ex-info "Physical single-node Chromium integration failed."
                  (assoc data :gesso.v1506/error reason))))

(defn- signal! [bridge]
  (locking (:monitor bridge)
    (.notifyAll ^Object (:monitor bridge))))

(defn- await-observation!
  [bridge predicate description]
  (let [deadline (+ (System/nanoTime) (* 1000000 network-timeout-ms))]
    (locking (:monitor bridge)
      (loop []
        (if-let [observation (predicate)]
          observation
          (let [remaining (- deadline (System/nanoTime))]
            (when-not (pos? remaining)
              (reject! :observation-timeout
                       {:description description
                        :requests @(:requests bridge)
                        :active-streams @(:active-streams bridge)
                        :bridge-errors @(:errors bridge)}))
            (.wait ^Object (:monitor bridge)
                   (max 1 (long (Math/ceil (/ remaining 1000000.0)))))
            (recur)))))))

(defn- header! [^HttpExchange exchange name value]
  (.set (.getResponseHeaders exchange) (str name) (str value)))

(defn- status-body! [^HttpExchange exchange {:keys [status headers body]}]
  (doseq [[name value] headers]
    ;; HTTP adapter's response headers are scalar strings, not collections.
    (header! exchange name value))
  (let [payload (.getBytes ^String (str (or body "")) StandardCharsets/UTF_8)]
    (if (#{204 304} status)
      ;; The JDK HTTP server uses -1, not zero, for a bodyless response.
      (.sendResponseHeaders exchange (int status) -1)
      (.sendResponseHeaders exchange (int status) (alength payload)))
    (with-open [out (.getResponseBody exchange)]
      (when (pos? (alength payload))
        (.write out payload)))))

(defn- sse-body! [bridge ^HttpExchange exchange response]
  (doseq [[name value] (:headers response)]
    (header! exchange name value))
  (.sendResponseHeaders exchange (int (:status response)) 0)
  (swap! (:active-streams bridge) inc)
  (signal! bridge)
  (try
    (with-open [out (.getResponseBody exchange)]
      ;; Flush a real SSE comment so EventSource's onopen can complete before
      ;; any mutation. Comments are not application invalidations.
      (.write out (.getBytes ": connected\n\n" StandardCharsets/UTF_8))
      (.flush out)
      (loop []
        (let [frame (deref (stream/take! (:body response) ::closed)
                           network-timeout-ms ::timeout)]
          (cond
            (= frame ::closed) nil
            (= frame ::timeout) (recur)
            :else
            (do
              (.write out (.getBytes ^String (str frame) StandardCharsets/UTF_8))
              (.flush out)
              (recur))))))
    (catch IOException _
      ;; A browser-initiated EventSource close is a normal socket teardown.
      nil)
    (catch InterruptedException _
      (.interrupt (Thread/currentThread)))
    (finally
      (swap! (:active-streams bridge) dec)
      (signal! bridge))))

(defn- handle! [bridge handler ^HttpExchange exchange]
  (let [method (keyword (str/lower-case (.getRequestMethod exchange)))
        uri (.getPath (.getRequestURI exchange))
        request {:request-method method :uri uri}]
    (try
      (with-open [input (.getRequestBody exchange)]
        ;; Consume any browser form payload; this fixture takes no form fields.
        (slurp input :encoding "UTF-8"))
      (let [response
            (try
              (if (and (= method :get) (= uri "/favicon.ico"))
                ;; Browser chrome may request a favicon implicitly. Keep that
                ;; non-application resource from producing spurious 404 logs.
                {:status 204 :headers {} :body ""}
                (handler request))
              (catch clojure.lang.ExceptionInfo error
                ;; Invalid operation preconditions and indeterminate outcomes
                ;; must not become browser-visible HTTP success.
                {:status 409
                 :headers {"content-type" "text/plain; charset=utf-8"}
                 :body "Rejected proving operation"
                 :test/reason (:reason (ex-data error))}))]
        (swap! (:requests bridge) conj
               {:method method :uri uri :status (:status response)
                :rejection (:test/reason response)})
        (signal! bridge)
        (if (= uri proving/events-uri)
          (sse-body! bridge exchange response)
          (status-body! exchange response)))
      (catch Throwable error
        (swap! (:errors bridge) conj
               {:method method :uri uri :error (.getMessage error)})
        (signal! bridge)
        (try
          (status-body!
           exchange {:status 500
                     :headers {"content-type" "text/plain"}
                     :body "Local proving transport error"})
          (catch Throwable _ nil)))
      (finally
        (.close exchange)))))

(defn- start-bridge! [handler]
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
        worker-number (atom 0)
        factory (reify ThreadFactory
                  (newThread [_ task]
                    (doto (Thread. ^Runnable task
                                   (str "gesso-v1506-http-" (swap! worker-number inc)))
                      (.setDaemon true))))
        executor (Executors/newCachedThreadPool factory)
        bridge {:server server
                :executor executor
                :monitor (Object.)
                :requests (atom [])
                :errors (atom [])
                :active-streams (atom 0)}]
    (try
      (.createContext server "/"
                      (reify HttpHandler
                        (handle [_ exchange]
                          (handle! bridge handler exchange))))
      (.setExecutor server executor)
      (.start server)
      (assoc bridge :base-url
             (str "http://127.0.0.1:" (.getPort (.getAddress server))))
      (catch Throwable error
        (.stop server 0)
        (.shutdownNow executor)
        (throw error)))))

(defn- stop-bridge! [{:keys [^HttpServer server executor]}]
  (.stop server 0)
  (.shutdownNow executor)
  (.awaitTermination executor 3 TimeUnit/SECONDS)
  nil)

(defn- url [bridge uri]
  (str (:base-url bridge) uri))

(defn- count-requests [bridge method uri]
  (count (filter #(and (= method (:method %)) (= uri (:uri %)))
                 @(:requests bridge))))

(defn- with-real-application-browser [f]
  (proving/with-live-proving-application!
   (fn [app]
     (let [bridge (start-bridge! (:live-handler app))
           harness* (atom nil)
           contexts* (atom [])]
       (try
         (let [harness (chromium/start!
                        {:args ["--no-sandbox" "--disable-dev-shm-usage"]})]
           (reset! harness* harness)
           (f {:app app :bridge bridge :harness harness
               :new-page!
               (fn []
                 (let [context (chromium/new-context! harness)
                       _ (swap! contexts* conj context)]
                   {:context context :page (chromium/new-page! context)}))}))
         (finally
           ;; Always stop the HTTP server, even when browser cleanup fails.
           ;; EventSource closes before the managed Live/XTDB fixture exits.
           (try
             (doseq [context (reverse @contexts*)]
               (chromium/close-context! context))
             (finally
               (try
                 (when-let [harness @harness*]
                   (chromium/stop! harness))
                 (finally
                   (stop-bridge! bridge)))))))))))

(defn- state-from-dom [^Page page]
  (chromium/evaluate
   page
   "() => { const f = document.querySelector('[data-gesso-proving-live-target]');
           if (!f) return null;
           return {status: f.getAttribute('data-slot-status'),
                   revision: Number(f.getAttribute('data-slot-revision'))}; }"))

(defn- wait-state! [^Page page status revision]
  (chromium/wait-for-js!
   page
   (str "() => { const f = document.querySelector('[data-gesso-proving-live-target]');"
        " return !!f && f.getAttribute('data-slot-status') === '" status "'"
        " && f.getAttribute('data-slot-revision') === '" revision "'; }")))

(defn- open-page! [bridge new-page!]
  (let [{:keys [context page] :as item} (new-page!)]
    (chromium/navigate! page (url bridge proving/panel-uri))
    (chromium/wait-for-js!
     page "() => !!window.htmx && !!window.gessoLive &&
                 !!document.querySelector('[data-gesso-proving-live-target]')")
    (await-observation! bridge #(when (pos? @(:active-streams bridge)) true)
                        "real EventSource connection")
    item))

(defn- loopback-response!
  [bridge method uri]
  (let [^HttpURLConnection connection
        (.openConnection (URL. (url bridge uri)))]
    (try
      (.setConnectTimeout connection network-timeout-ms)
      (.setReadTimeout connection network-timeout-ms)
      (.setRequestMethod connection (.toUpperCase (name method)))
      (let [status (.getResponseCode connection)
            body-stream (if (< status 400)
                          (.getInputStream connection)
                          (.getErrorStream connection))]
        {:status status
         :body (if body-stream
                 (with-open [input body-stream]
                   (slurp input :encoding "UTF-8"))
                 "")})
      (finally
        (.disconnect connection)))))

(deftest loopback-http-and-sse-transport-smoke-test
  ;; This is intentionally browser-independent, so the real HTTP bridge and
  ;; actual embedded XTDB behavior remain testable even where a managed
  ;; Chromium installation forbids all URL navigations.
  (proving/with-live-proving-application!
   (fn [app]
     (let [bridge (start-bridge! (:live-handler app))
           ^HttpURLConnection sse (.openConnection
                                    (URL. (url bridge proving/events-uri)))]
       (try
         (is (= 200 (:status (loopback-response!
                              bridge :get proving/panel-uri))))
         (is (= 200 (:status (loopback-response!
                              bridge :get proving/fragment-uri))))
         (is (= 200 (:status (loopback-response!
                              bridge :get proving/htmx-asset-uri))))
         (.setConnectTimeout sse network-timeout-ms)
         (.setReadTimeout sse network-timeout-ms)
         (.setRequestProperty sse "Accept" "text/event-stream")
         (is (= 200 (.getResponseCode sse)))
         (with-open [reader (BufferedReader.
                            (InputStreamReader. (.getInputStream sse)
                                                StandardCharsets/UTF_8))]
           (is (= ": connected" (.readLine reader)))
           (is (= "" (.readLine reader)))
           (is (= 200 (:status (loopback-response!
                                bridge :post reservation-uri))))
           (is (= 1 (:revision ((:read! app)))))
           (let [frame-lines
                 (loop [lines []]
                   (when (> (count lines) 12)
                     (reject! :oversized-sse-frame {:lines lines}))
                   (let [line (.readLine reader)]
                     (when (nil? line)
                       (reject! :truncated-sse-frame {:lines lines}))
                     (if (empty? line)
                       lines
                       (recur (conj lines line)))))]
             (is (some #(= "event: live-update" %) frame-lines)
                 (pr-str frame-lines)))
           (is (str/includes?
                (:body (loopback-response! bridge :get proving/fragment-uri))
                "data-slot-status=\"reserved\""))
           (is (= 409 (:status (loopback-response!
                                bridge :post reservation-uri))))
           (is (= 1 (:revision ((:read! app)))))
           (is (= 200 (:status (loopback-response!
                                bridge :post release-uri))))
           (is (= 2 (:revision ((:read! app))))))
         (is (empty? @(:errors bridge)) (pr-str @(:errors bridge)))
         (finally
           (.disconnect sse)
           (stop-bridge! bridge)))))))

(deftest chromium-htmx-actions-and-authoritative-sse-refresh-test
  (with-real-application-browser
    (fn [{:keys [app bridge new-page!]}]
      (let [{:keys [context page]} (open-page! bridge new-page!)]
        (testing "real browser loads local assets and initial XTDB state"
          (wait-state! page "available" 0)
          (is (= {"status" "available" "revision" 0} (state-from-dom page)))
          (is (zero? (:revision ((:read! app)))))
          (is (= 1 (count-requests bridge :get proving/panel-uri)))
          (is (pos? (count-requests bridge :get proving/events-uri))))
        (testing "HTMX POST reserves once, real SSE causes authoritative GET"
          (.click (.locator ^Page page "button[hx-post='/__gesso_proving/slot/reserve']"))
          (wait-state! page "reserved" 1)
          (is (= 1 (count-requests bridge :post reservation-uri)))
          (is (pos? (count-requests bridge :get proving/fragment-uri)))
          (is (= 1 (:revision ((:read! app))))))
        (testing "second HTMX POST releases and the fragment remains operational"
          (.click (.locator ^Page page "button[hx-post='/__gesso_proving/slot/release']"))
          (wait-state! page "available" 2)
          (is (= 1 (count-requests bridge :post release-uri)))
          (is (= 2 (:revision ((:read! app))))))
        (is (empty? @(:errors bridge)) (pr-str @(:errors bridge)))
        (is (true? (chromium/assert-clean! context)))))))

(deftest chromium-rejected-actions-remain-nonauthoritative-test
  (with-real-application-browser
    (fn [{:keys [app bridge new-page!]}]
      (let [{:keys [context page]} (open-page! bridge new-page!)]
        (wait-state! page "available" 0)
        (testing "browser-originated invalid release is rejected by the real model"
          (is (= 409
                 (chromium/evaluate
                  page
                  "async () => (await fetch('/__gesso_proving/slot/release',
                         {method: 'POST'})).status")))
          (is (= 0 (:revision ((:read! app)))))
          (is (= {"status" "available" "revision" 0} (state-from-dom page)))
          (is (zero? (count-requests bridge :get proving/fragment-uri)))
          (is (= :operation-precondition-not-met
                 (:rejection (last (filter #(= release-uri (:uri %))
                                           @(:requests bridge)))))))
        (is (empty? @(:errors bridge)) (pr-str @(:errors bridge)))
        ;; A browser may log a failed-resource console entry for the deliberate
        ;; HTTP 409 even when fetch itself resolved normally. Do not suppress
        ;; unrelated errors or mistake a 409 for a transport failure.
        (let [errors (chromium/browser-errors context)]
          (is (every?
               (fn [entry]
                 (and (= :console-error (:kind entry))
                      (str/includes? (:text entry) "409")
                      (or (str/includes? (str (get-in entry [:location :url]))
                                         release-uri)
                          (str/includes? (:text entry) release-uri))))
               errors)
              (pr-str errors)))
        (is (empty? (:request-failures (chromium/diagnostics context))))))))

(deftest chromium-two-contexts-share-authoritative-state-test
  (with-real-application-browser
    (fn [{:keys [app bridge new-page!]}]
      (let [{context-a :context page-a :page} (open-page! bridge new-page!)
            {context-b :context page-b :page} (open-page! bridge new-page!)]
        (wait-state! page-a "available" 0)
        (wait-state! page-b "available" 0)
        (await-observation! bridge #(when (= 2 @(:active-streams bridge)) true)
                            "two real EventSource connections")
        (testing "one actor posts, both browsers adopt server-read state"
          (.click (.locator ^Page page-a "button[hx-post='/__gesso_proving/slot/reserve']"))
          (wait-state! page-a "reserved" 1)
          (wait-state! page-b "reserved" 1)
          (is (= 1 (count-requests bridge :post reservation-uri)))
          (is (= 1 (:revision ((:read! app))))))
        (testing "second actor posts, both browsers recover to current state"
          (.click (.locator ^Page page-b "button[hx-post='/__gesso_proving/slot/release']"))
          (wait-state! page-a "available" 2)
          (wait-state! page-b "available" 2)
          (is (= 1 (count-requests bridge :post release-uri)))
          (is (= 2 (:revision ((:read! app))))))
        (is (empty? @(:errors bridge)) (pr-str @(:errors bridge)))
        (is (true? (chromium/assert-clean! context-a)))
        (is (true? (chromium/assert-clean! context-b)))))))

(defn -main [& _]
  (let [{:keys [fail error] :as result}
        (run-tests 'gesso.assurance.example.physical-single-node-live-chromium-integration)]
    (when (pos? (+ fail error))
      (reject! :chromium-integration-failed {:summary result}))
    result))
