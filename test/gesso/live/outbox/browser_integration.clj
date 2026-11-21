(ns gesso.live.outbox.browser-integration
  "v2109: opt-in Chromium browser proof over the concrete Gesso/Biff host.

   Physical path: XTDB atomic domain/outbox transaction -> managed worker ->
   authorized Live SSE -> JDK HTTP loopback -> Chromium EventSource -> pinned
   htmx-ext-sse and production Gesso browser runtime -> HTMX GET -> XTDB ->
   visible fragment DOM. Chromium/Playwright is the observer, not a network
   or protocol mock. The JDK Ring bridge is a test server, not Aleph.

   This scenario deliberately remains outside the ordinary JVM suite; invoke
   -main in the Chromium test profile. No authentication implementation,
   process restart, or durable browser persistence is claimed here."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test :refer [deftest is testing run-tests]]
            [gesso.core :as g]
            [gesso.live.application-preflight.fixture
             :refer [with-surfaced-application standard-operations render-context
                     rendered-operation-button]]
            [gesso.live.browser.chromium :as chromium]
            [gesso.live.core :as live]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.outbox.host :as host]
            [gesso.live.outbox.http :as http]
            [gesso.live.consistency.xtdb :as live.xtdb]
            [gesso.live.progression.http :as progression.http]
            [gesso.live.source :as source]
            [gesso.live.ui :as ui]
            [manifold.stream :as stream]
            [rum.core :as rum]
            [xtdb.api :as xt])
  (:import [com.sun.net.httpserver HttpServer HttpHandler HttpExchange]
           [java.net InetSocketAddress URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent Executors]))

(def ^:private subject "v2109-browser-request")
(def ^:private panel-id "v2109-browser-panel")
(def ^:private source-id "gesso/v2109-browser")
(def ^:private scope {:topic :request :id subject})
(def ^:private js-headers {"content-type" "text/javascript; charset=utf-8"
                           "cache-control" "no-store"})
(def ^:private html-headers {"content-type" "text/html; charset=utf-8"
                             "cache-control" "no-store"})
(def ^:private htmx-assets
  ["META-INF/resources/webjars/htmx.org/2.0.7/dist/htmx.min.js"
   "META-INF/resources/webjars/htmx.org/2.0.7/dist/htmx.js"])
(def ^:private sse-assets
  ["META-INF/resources/webjars/htmx-ext-sse/2.2.4/dist/sse.min.js"
   "META-INF/resources/webjars/htmx-ext-sse/2.2.4/dist/sse.js"
   "META-INF/resources/webjars/htmx-ext-sse/2.2.4/sse.min.js"
   "META-INF/resources/webjars/htmx-ext-sse/2.2.4/sse.js"])

(defn- asset! [candidates]
  (if-let [url (some io/resource candidates)]
    (slurp url)
    (throw (ex-info "Required pinned JS asset not found on test classpath"
                    {:candidates candidates}))))

(defn- runtime! []
  (let [file (io/file "resources/public/js/gesso-live.js")]
    (when-not (.isFile file)
      (throw (ex-info "Advanced compiled browser runtime missing; build it first"
                      {:file (.getPath file)})))
    (slurp file)))

(defn- config []
  {:xtdb-config {}
   :source-id source-id
   :worker-options {:worker-id "v2109-chromium" :poll-ms 20}
   :rules-context {}
   :live-options {:rules [{:when-topic :request
                           :expand (fn [_ change]
                                     [(select-keys change [:topic :id :change/kind])])}]
                  :source-options {:coalesce-window-ms 0}
                  :dispatch-options {:threads 1 :queue-size 8 :on-overflow :throw}}})

(defn- stable-q
  ;; XTDB 2.2.0-beta1 occasionally invalidates SELECT * plans when an outbox
  ;; row gains lease/ack fields. Retry ONLY that typed transient conflict.
  ;; When a Ring request is supplied, bind the (untrusted) HTTP progression
  ;; as a request-local minimum and perform XTDB's authority-aware read.
  ([node query] (stable-q node query nil))
  ([node query request]
   (let [ctx (when request
               (progression.http/bind-request-progression
                (assoc request :xtdb/connectable node)))]
     (loop [remaining 8]
       (let [attempt (try {:rows (if ctx
                                   (live.xtdb/q-consistent-from ctx query)
                                   (xt/q node query))}
                          (catch Throwable error {:error error}))]
         (if-let [error (:error attempt)]
           (if (and (pos? remaining)
                    (= :prepared-query-out-of-date
                       (:xtdb.error/code (ex-data error))))
             (do (Thread/sleep 30) (recur (dec remaining)))
             (throw error))
           (:rows attempt)))))))

(defn- fragment-markup
  ([node] (fragment-markup node {}))
  ([node request]
   (let [row (first (stable-q node
                              ["SELECT * FROM requests WHERE _id = ?" subject]
                              request))
         state (or (:status row) :pending)]
     (str "<div id=\"" panel-id "\" data-authoritative-status=\""
          (name state) "\">" (name state) "</div>"))))

(defn- fragment-response
  "Fixture's minimum-read boundary: invalid browser header fails BEFORE any
   authoritative query, while valid requirements reach q-consistent-from.
   This is test-only Ring code, NOT an application authentication policy."
  [node request]
  (try
    {:status 200 :headers html-headers :body (fragment-markup node request)}
    (catch clojure.lang.ExceptionInfo error
      (if (= :gesso.live.progression.http/error
             (:error/type (ex-data error)))
        {:status 400 :headers html-headers :body "Invalid progression"}
        (throw error)))))

(def ^:private observer-js
  (str "window.__v2109={messages:0,opens:0,refreshes:0,headers:[],errors:[]};\n"
       "document.addEventListener('htmx:sseBeforeMessage',function(e){"
       "if(e.detail&&e.detail.elt&&e.detail.elt.hasAttribute('data-gesso-live-invalidation'))"
       "{window.__v2109.messages++;}});\n"
       "document.addEventListener('htmx:sseOpen',function(){window.__v2109.opens++;});\n"
       "document.addEventListener('htmx:beforeRequest',function(e){"
       "if(e.detail&&e.detail.elt&&e.detail.elt.hasAttribute('data-gesso-live-fragment'))"
       "{window.__v2109.refreshes++;}});\n"
       "document.addEventListener('htmx:sendError',function(){window.__v2109.errors.push('send');});\n"
       "document.addEventListener('htmx:responseError',function(){window.__v2109.errors.push('response');});\n"
       "gesso.live.browser.runtime.init_BANG_();\n"
       "window.__v2109.runtime=!!window.gessoLive;\n"))

(defn- page-html [render]
  (str "<!doctype html><html><head><meta charset=\"utf-8\">"
       "<link rel=\"icon\" href=\"data:,\">"
       "<script src=\"/assets/htmx.js\"></script>"
       "<script src=\"/assets/sse.js\"></script>"
       "<script src=\"/assets/gesso-live.js\"></script>"
       "</head><body>"
       (rum/render-static-markup
        (ui/fragment-panel {:id panel-id :src "/fragment"
                            :stream-url "/live/events"
                            :event "live-update" :swap "outerHTML"}))
       (rum/render-static-markup
        (rendered-operation-button render :request/claim "/operations/request/claim"))
       "<script>" observer-js "</script></body></html>"))

(defn- http-write! [^HttpExchange exchange response]
  (let [body (:body response)
        status (:status response)
        live-stream? (stream/stream? body)]
    (doseq [[k v] (:headers response)]
      (.set (.getResponseHeaders exchange) (str k) (str v)))
    (if live-stream?
      (do
        (.sendResponseHeaders exchange status 0)
        (try
          (with-open [out (.getResponseBody exchange)]
            (loop [pending (stream/take! body)]
              ;; Never abandon an unresolved take! on timeout: the orphan
              ;; would consume the NEXT SSE frame unseen by the HTTP client.
              (let [value (deref pending 500 ::timeout)]
                (cond (= ::timeout value)
                      (when-not (stream/closed? body) (recur pending))
                      (nil? value) nil
                      :else (do (.write out (.getBytes (str value) StandardCharsets/UTF_8))
                                (.flush out)
                                (recur (stream/take! body)))))))
          (finally (stream/close! body))))
      (let [bytes (.getBytes (str (or body "")) StandardCharsets/UTF_8)]
        (.sendResponseHeaders exchange status (alength bytes))
        (with-open [out (.getResponseBody exchange)]
          (.write out bytes))))))

(defn- start-loopback! [ring-handler]
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
        executor (Executors/newCachedThreadPool)]
    (.setExecutor server executor)
    (.createContext server "/"
                    (reify HttpHandler
                      (handle [_ exchange]
                        (try
                          (let [request {:uri (.getPath (.getRequestURI exchange))
                                         :request-method (keyword
                                                          (str/lower-case (.getRequestMethod exchange)))
                                         :headers (into {}
                                                        (map (fn [[k v]]
                                                               [(str/lower-case k) (first v)]))
                                                        (.entrySet (.getRequestHeaders exchange)))}]
                            (http-write! exchange (ring-handler request)))
                          (catch Throwable error
                            (binding [*out* *err*]
                              (println "v2109 browser HTTP bridge failure:"
                                       (.getMessage error)))
                            (try (.close exchange) (catch Throwable _ nil)))))))
    (.start server)
    {:url (str "http://127.0.0.1:" (.getPort (.getAddress server)))
     :close! (fn [] (.stop server 0)
               (.shutdownNow executor))}))

(defn- eventually [predicate]
  (loop [n 0]
    (cond (predicate) true
          (>= n 350) false
          :else (do (Thread/sleep 20) (recur (inc n))))))

(deftest concrete-host-eventsource-htmx-converges-to-authoritative-xtdb-test
  (let [ops (standard-operations)
        render (render-context ops)]
    (with-surfaced-application
      {:request-board [:main (rendered-operation-button
                              render :request/claim "/operations/request/claim")]}
      (fn [{:keys [assembly]}]
        (let [ring (fn [_] (g/html-response
                            [:main (rendered-operation-button
                                    render :request/claim "/operations/request/claim")]))
              started (host/start! assembly ring (config))
              node (:xtdb/connectable started)
              sys (:gesso.live/system started)
              requests (atom [])
              route (http/wrap-authorized-sse
                     started {:path "/live/events"
                              :authorize-subscription
                              (fn [request]
                                ;; Test-only session admission, NOT production auth.
                                (when (str/includes?
                                       (or (get-in request [:headers "cookie"]) "")
                                       "v2109-test-session=1")
                                  scope))})
              html (page-html render)
              javascript {"/assets/htmx.js" (asset! htmx-assets)
                          "/assets/sse.js" (asset! sse-assets)
                          "/assets/gesso-live.js" (runtime!)}
              serve (fn [request]
                      (cond
                        (= "/live/events" (:uri request))
                        ((:handler route) request)
                        (contains? javascript (:uri request))
                        {:status 200 :headers js-headers
                         :body (get javascript (:uri request))}
                        (= "/" (:uri request))
                        {:status 200 :headers (assoc html-headers
                                                    "set-cookie" "v2109-test-session=1; Path=/; SameSite=Lax")
                         :body html}
                        (= "/fragment" (:uri request))
                        (do (swap! requests conj request)
                            (fragment-response node request))
                        :else ((:handler route) request)))
              server (start-loopback! serve)
              harness* (atom nil)
              context* (atom nil)]
          (try
            (xt/execute-tx node [[:put-docs :requests
                                  {:xt/id subject :status :pending}]])
            (let [harness (chromium/start! {:args ["--no-sandbox"
                                                   "--disable-dev-shm-usage"]})
                  _ (reset! harness* harness)
                  context (chromium/new-context! harness)
                  _ (reset! context* context)
                  page (chromium/new-page! context)]
              (chromium/navigate! page (str (:url server) "/"))
              (chromium/wait-for-js! page
                                     "() => window.__v2109 && window.__v2109.runtime === true")
              (chromium/wait-for-js! page
                                     "() => window.__v2109.opens >= 1")
              ;; Wait for an initial advisory refresh to complete, so the
              ;; succeeding refresh is attributable to the persisted write.
              (chromium/wait-for-js! page
                                     "() => document.getElementById('v2109-browser-panel') && document.getElementById('v2109-browser-panel').textContent === 'pending'")
              (is (eventually #(pos? (count @requests))))
              (let [baseline (long (chromium/evaluate
                                    page "() => window.__v2109.refreshes"))
                    intent (random-uuid)
                    plan (outbox/prepare!
                          {:intent-ids [intent] :source-id source-id
                           :operation :request/claim :purpose :gesso.live/refresh}
                          [{:topic :request :id subject :change/kind :updated}])]
                (xt/execute-tx node (into (vec (:guard-ops plan))
                                          [[:put-docs :requests
                                            {:xt/id subject :status :claimed}]
                                           (:tx-op plan)]))
                (is (eventually
                     #(= :dispatch-accepted
                         (:gesso.live.outbox/status
                          (first (stable-q node
                                       ["SELECT * FROM gesso_live_outbox WHERE _id = ?" intent]))))))
                (chromium/wait-for-js! page
                                       "() => window.__v2109.messages >= 1")
                (chromium/wait-for-js! page
                                       "() => document.getElementById('v2109-browser-panel') && document.getElementById('v2109-browser-panel').getAttribute('data-authoritative-status') === 'claimed'")
                (is (> (long (chromium/evaluate page
                                                "() => window.__v2109.refreshes"))
                       baseline))
                (is (= "claimed"
                       (chromium/evaluate page
                                          "() => document.getElementById('v2109-browser-panel').textContent")))
                (is (= :claimed (:status (first
                                         (stable-q node ["SELECT * FROM requests WHERE _id = ?" subject])))))
                (is (some (fn [req]
                            (some-> (get-in req [:headers
                                                 progression.http/request-header-name])
                                    progression.http/decode-request-progression))
                          @requests)
                    "The browser's refresh must carry an authoritative progression requirement")
                (is (= [] (chromium/evaluate page "() => window.__v2109.errors")))
                (is (true? (chromium/assert-clean! context)))))
            (finally
              (when-let [ctx @context*] (chromium/close-context! ctx))
              (when-let [br @harness*] (chromium/stop! br))
              ((:close! route))
              ((:close! server))
              (host/stop! started))))))))

(defn- get-body!
  ([^HttpClient client base path] (get-body! client base path {}))
  ([^HttpClient client base path headers]
   (let [builder (.GET (HttpRequest/newBuilder (URI/create (str base path))))
         builder (reduce-kv (fn [b name value] (.header b name value))
                            builder headers)
         response (.send client (.build builder)
                         (HttpResponse$BodyHandlers/ofString))]
     {:status (.statusCode response)
      :body (.body response)})))

(deftest canonical-browser-page-and-authoritative-fragment-over-loopback-test
  ;; Independent JVM/HTTP gate: does NOT claim a browser received the page.
  ;; It makes asset availability and actual authoritative reads measurable
  ;; even when a managed Chromium policy disallows navigation to localhost.
  (let [ops (standard-operations)
        render (render-context ops)]
    (with-surfaced-application
      {:request-board [:main (rendered-operation-button
                              render :request/claim "/operations/request/claim")]}
      (fn [{:keys [assembly]}]
        (let [handler (fn [_] (g/html-response
                              [:main (rendered-operation-button
                                      render :request/claim
                                      "/operations/request/claim")]))
              started (host/start! assembly handler (config))
              node (:xtdb/connectable started)
              page (page-html render)
              js {"/assets/htmx.js" (asset! htmx-assets)
                  "/assets/sse.js" (asset! sse-assets)
                  "/assets/gesso-live.js" (runtime!)}
              server (start-loopback!
                      (fn [{:keys [uri]}]
                        (cond (= uri "/")
                              {:status 200 :headers html-headers :body page}
                              (= uri "/fragment")
                              (fragment-response node {:uri uri :headers {}})
                              (contains? js uri)
                              {:status 200 :headers js-headers
                               :body (get js uri)}
                              :else {:status 404 :headers {} :body "Not found"})))
              client (HttpClient/newHttpClient)]
          (try
            (xt/execute-tx node [[:put-docs :requests
                                  {:xt/id subject :status :pending}]])
            (let [resp (get-body! client (:url server) "/")]
              (is (= 200 (:status resp)))
              (is (str/includes? (:body resp) "data-gesso-live-fragment"))
              (is (str/includes? (:body resp) "sse-connect"))
              (is (str/includes? (:body resp) "/live/events"))
              (is (str/includes? (:body resp) "gesso.live.browser.runtime.init_BANG_")))
            (doseq [path (keys js)]
              (let [resp (get-body! client (:url server) path)]
                (is (= 200 (:status resp)) path)
                (is (> (count (:body resp)) 500) path)))
            (is (str/includes?
                 (:body (get-body! client (:url server) "/fragment"))
                 "data-authoritative-status=\"pending\""))
            (let [intent (random-uuid)
                  plan (outbox/prepare!
                        {:intent-ids [intent] :source-id source-id
                         :operation :request/claim :purpose :gesso.live/refresh}
                        [{:topic :request :id subject :change/kind :updated}])]
              (xt/execute-tx node (into (vec (:guard-ops plan))
                                        [[:put-docs :requests
                                          {:xt/id subject :status :claimed}]
                                         (:tx-op plan)]))
              (is (str/includes?
                   (:body (get-body! client (:url server) "/fragment"))
                   "data-authoritative-status=\"claimed\""))
              (is (eventually
                   #(= :dispatch-accepted
                       (:gesso.live.outbox/status
                        (first (stable-q node
                                     ["SELECT * FROM gesso_live_outbox WHERE _id = ?"
                                      intent]))))))
              (is (= :claimed
                     (:status (first (stable-q node
                                           ["SELECT * FROM requests WHERE _id = ?"
                                            subject]))))))
            (finally
              ((:close! server))
              (host/stop! started))))))))


(deftest actual-http-fragment-read-honors-source-progression-test
  ;; This JVM/HTTP gate cannot replace real EventSource/HTMX, but it proves
  ;; the server never ignores the header emitted by the browser adapter.
  ;; The required basis comes from a REAL outbox source event, not an invented
  ;; token; the physical request uses the same header spelling as HTMX.
  (let [ops (standard-operations)
        render (render-context ops)]
    (with-surfaced-application
      {:request-board [:main (rendered-operation-button
                              render :request/claim "/operations/request/claim")]}
      (fn [{:keys [assembly]}]
        (let [handler (fn [_] (g/html-response
                              [:main (rendered-operation-button
                                      render :request/claim
                                      "/operations/request/claim")]))
              started (host/start! assembly handler (config))
              node (:xtdb/connectable started)
              tap (source/subscribe (:source (:gesso.live/system started))
                                    {:topic :request :id subject})
              seen-requests (atom [])
              server (start-loopback!
                      (fn [request]
                        (if (= "/fragment" (:uri request))
                          (do (swap! seen-requests conj request)
                              (fragment-response node request))
                          {:status 404 :headers {} :body "Not found"})))
              client (HttpClient/newHttpClient)]
          (try
            (xt/execute-tx node [[:put-docs :requests
                                  {:xt/id subject :status :pending}]])
            (is (= 200 (:status (get-body! client (:url server) "/fragment"))))
            ;; Even a malformed URI/EDN header must be rejected before query.
            (let [bad (get-body! client (:url server) "/fragment"
                                 {progression.http/request-header-name "%not-valid"})]
              (is (= 400 (:status bad)))
              (is (= "Invalid progression" (:body bad))))
            (let [id (random-uuid)
                  plan (outbox/prepare!
                        {:intent-ids [id] :source-id source-id
                         :operation :request/claim :purpose :gesso.live/refresh}
                        [{:topic :request :id subject :change/kind :updated}])]
              (xt/execute-tx node (into (vec (:guard-ops plan))
                                        [[:put-docs :requests
                                          {:xt/id subject :status :claimed}]
                                         (:tx-op plan)]))
              (is (eventually
                   #(= :dispatch-accepted
                       (:gesso.live.outbox/status
                        (first (stable-q node
                                 ["SELECT * FROM gesso_live_outbox WHERE _id = ?" id]))))))
              (let [event (deref (stream/take! tap) 5000 ::timeout)
                    requirement (:progression event)
                    header (progression.http/encode-request-progression requirement)
                    reply (get-body! client (:url server) "/fragment"
                                     {progression.http/request-header-name header})]
                (is (= subject (:id event)))
                (is (some? requirement))
                (is (string? header))
                (is (= requirement
                       (progression.http/decode-request-progression header)))
                (is (= 200 (:status reply)))
                (is (str/includes? (:body reply)
                                   "data-authoritative-status=\"claimed\""))
                (is (= header
                       (get-in (last @seen-requests)
                               [:headers progression.http/request-header-name])))))
            (finally
              (stream/close! tap)
              ((:close! server))
              (host/stop! started))))))))

(defn -main [& args]
  ;; The readiness selection can run where managed Chromium policies deny
  ;; loopback URLs. The default browser gate never silently downgrades to it.
  (let [mode (vec args)
        _ (when-not (#{[] ["--readiness"]} mode)
            (throw (ex-info "Unknown browser integration test mode"
                            {:args mode})))
        result (if (= mode ["--readiness"])
                 (binding [test/*report-counters*
                           (ref test/*initial-report-counters*)]
                   (test/test-vars
                    [#'canonical-browser-page-and-authoritative-fragment-over-loopback-test
                     #'actual-http-fragment-read-honors-source-progression-test])
                   @test/*report-counters*)
                 (run-tests 'gesso.live.outbox.browser-integration))]
    (println "V2109 RESULTS" result)
    (shutdown-agents)
    (System/exit (if (and (zero? (:fail result)) (zero? (:error result))) 0 1))))
