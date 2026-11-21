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
            [gesso.live.progression :as progression]
            [gesso.live.source :as source]
            [gesso.live.ui :as ui]
            [manifold.stream :as stream]
            [rum.core :as rum]
            [xtdb.api :as xt])
  (:import [com.sun.net.httpserver HttpServer HttpHandler HttpExchange]
           [java.net InetSocketAddress URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.io BufferedReader InputStreamReader]
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
              ;; The real HTTP response Manifold stream is the server-owned
              ;; test fault-control point. Closing it forces a native
              ;; EventSource disconnect without mocking the browser APIs.
              sse-responses (atom [])
              correlated-source (source/subscribe (:source sys) scope)
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
                        (let [response ((:handler route) request)]
                          (when (stream/stream? (:body response))
                            (swap! sse-responses conj (:body response)))
                          response)
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
              ;; Capture actual node identities before any outerHTML refresh.
              ;; Mere marker presence after a refresh would also pass if HTMX
              ;; replaced the SSE-owning root and silently lost its listener.
              (is (true?
                   (chromium/evaluate
                    page
                    "() => { window.__v2109.root = document.querySelector('[data-gesso-live-fragment]'); window.__v2109.listener = document.querySelector('[data-gesso-live-invalidation]'); return !!window.__v2109.root && !!window.__v2109.listener; }")))
              (let [baseline (long (chromium/evaluate
                                    page "() => window.__v2109.refreshes"))
                    baseline-requests (count @requests)
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
                (let [source-event (deref (stream/take! correlated-source)
                                          5000 ::no-source-event)]
                  (is (= subject (:id source-event)))
                  (is (some? (:progression source-event)))
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
                  ;; Only requests issued AFTER this write's baseline count,
                  ;; carrying the EXACT requirement accepted from this source
                  ;; event, establish correspondence. An unrelated header or
                  ;; the initial advisory refresh must not satisfy the test.
                  (is (some (fn [req]
                              (when-let [header (get-in
                                                 req [:headers
                                                      progression.http/request-header-name])]
                                (= (:progression source-event)
                                   (progression.http/decode-request-progression
                                    header))))
                            (drop baseline-requests @requests))
                      "The post-invalidation browser GET must carry the exact source progression")
                  ;; The first HTMX outerHTML swap must NOT remove the
                  ;; stable SSE connection or the fragment refresh owner.
                  ;; A second independent write must generate a second
                  ;; source-correlated refresh on the SAME root/connection.
                  (is (true?
                       (chromium/evaluate
                        page
                        "() => window.__v2109.root === document.querySelector('[data-gesso-live-fragment]') && window.__v2109.listener === document.querySelector('[data-gesso-live-invalidation]')")))
                  (let [second-baseline (count @requests)
                        second-refreshes (long (chromium/evaluate
                                                page "() => window.__v2109.refreshes"))
                        second-intent (random-uuid)
                        second-plan (outbox/prepare!
                                     {:intent-ids [second-intent] :source-id source-id
                                      :operation :request/done
                                      :purpose :gesso.live/refresh}
                                     [{:topic :request :id subject
                                       :change/kind :updated}])]
                    (xt/execute-tx node
                                   (into (vec (:guard-ops second-plan))
                                         [[:put-docs :requests
                                           {:xt/id subject :status :done}]
                                          (:tx-op second-plan)]))
                    (is (eventually
                         #(= :dispatch-accepted
                             (:gesso.live.outbox/status
                              (first (stable-q node
                                               ["SELECT * FROM gesso_live_outbox WHERE _id = ?"
                                                second-intent]))))))
                    (let [second-event (deref (stream/take! correlated-source)
                                              5000 ::no-second-source-event)]
                      (is (= subject (:id second-event)))
                      (is (some? (:progression second-event)))
                      (is (not= (:progression source-event)
                                (:progression second-event)))
                      (chromium/wait-for-js!
                       page "() => window.__v2109.messages >= 2")
                      (chromium/wait-for-js!
                       page
                       "() => document.getElementById('v2109-browser-panel') && document.getElementById('v2109-browser-panel').getAttribute('data-authoritative-status') === 'done'")
                      (is (> (long (chromium/evaluate
                                    page "() => window.__v2109.refreshes"))
                             second-refreshes))
                      (is (some (fn [req]
                                  (when-let [h (get-in req
                                                        [:headers
                                                         progression.http/request-header-name])]
                                    (= (:progression second-event)
                                       (progression.http/decode-request-progression h))))
                                (drop second-baseline @requests))
                          "Second browser refresh must carry second source progression")
                      (is (true?
                           (chromium/evaluate
                            page
                            "() => window.__v2109.root === document.querySelector('[data-gesso-live-fragment]') && window.__v2109.listener === document.querySelector('[data-gesso-live-invalidation]')"))))
                  ;; v2116: after proving two canonical refreshes over one
                  ;; connection, intentionally retire only the *first* SSE
                  ;; response. EventSource must reconnect to a newly admitted
                  ;; Live subscription without remounting the stable root.
                  (let [disconnect-request-baseline (count @requests)
                        original (first @sse-responses)]
                    (is (= 1 (count @sse-responses)))
                    (is (= 1 ((:active-count route))))
                    (is (some? original))
                    (stream/close! original)
                    (is (stream/closed? original))
                    (chromium/wait-for-js! page
                                           "() => window.__v2109.opens >= 2")
                    (is (eventually #(>= (count @sse-responses) 2)))
                    (is (eventually #(= 1 ((:active-count route)))))
                    (is (not (identical? original (second @sse-responses))))
                    (is (true?
                         (chromium/evaluate
                          page
                          "() => window.__v2109.root === document.querySelector('[data-gesso-live-fragment]') && window.__v2109.listener === document.querySelector('[data-gesso-live-invalidation]')")))
                    ;; An SSE open conveys no authoritative progression.
                    ;; Its post-reconnect advisory GET must not invent one.
                    (is (eventually #(> (count @requests)
                                        disconnect-request-baseline)))
                    (is (some #(nil? (get-in %
                                             [:headers progression.http/request-header-name]))
                              (drop disconnect-request-baseline @requests))
                        "The reconnect advisory refresh must not invent authority")
                    ;; The deliberate disconnect may produce one HTMX-sourced
                    ;; EventSource.onerror console diagnostic. Classify just
                    ;; that expected error; keep unrelated browser failures
                    ;; fatal rather than globally suppressing diagnostics.
                    (let [errors (chromium/browser-errors context)
                          diagnostic (chromium/diagnostics context)]
                      (is (<= (count errors) 1))
                      (is (every? (fn [{:keys [kind type location]}]
                                    (and (= :console-error kind)
                                         (= "error" type)
                                         (string? location)
                                         (str/includes? location "/assets/htmx.js")))
                                  errors))
                      (is (empty? (:request-failures diagnostic)))
                      (chromium/clear-diagnostics! context))
                    ;; A third independent physical transaction must now
                    ;; traverse the NEW EventSource and return EXACTLY its
                    ;; authoritative XTDB progression in the fragment GET.
                    (let [baseline-third (count @requests)
                          refreshes-third (long (chromium/evaluate
                                                 page "() => window.__v2109.refreshes"))
                          third-id (random-uuid)
                          third (outbox/prepare!
                                 {:intent-ids [third-id]
                                  :source-id source-id
                                  :operation :request/reopen
                                  :purpose :gesso.live/refresh}
                                 [{:topic :request :id subject
                                   :change/kind :updated}])]
                      (xt/execute-tx node
                                     (into (vec (:guard-ops third))
                                           [[:put-docs :requests
                                             {:xt/id subject :status :reopened}]
                                            (:tx-op third)]))
                      (is (eventually
                           #(= :dispatch-accepted
                               (:gesso.live.outbox/status
                                (first (stable-q node
                                     ["SELECT * FROM gesso_live_outbox WHERE _id = ?"
                                      third-id]))))))
                      (let [third-event (deref (stream/take! correlated-source)
                                               5000 ::no-third-source-event)]
                        (is (= subject (:id third-event)))
                        (is (some? (:progression third-event)))
                        (is (not= (:progression source-event)
                                  (:progression third-event)))
                        (chromium/wait-for-js! page
                                               "() => window.__v2109.messages >= 3")
                        (chromium/wait-for-js!
                         page
                         "() => document.getElementById('v2109-browser-panel') && document.getElementById('v2109-browser-panel').getAttribute('data-authoritative-status') === 'reopened'")
                        (is (> (long (chromium/evaluate
                                      page "() => window.__v2109.refreshes"))
                               refreshes-third))
                        (is (some (fn [req]
                                    (when-let [header (get-in
                                                       req [:headers
                                                            progression.http/request-header-name])]
                                      (= (:progression third-event)
                                         (progression.http/decode-request-progression
                                          header))))
                                  (drop baseline-third @requests))
                            "After reconnect the browser must GET with the third exact progression")
                        (is (true?
                             (chromium/evaluate
                              page
                              "() => window.__v2109.root === document.querySelector('[data-gesso-live-fragment]') && window.__v2109.listener === document.querySelector('[data-gesso-live-invalidation]')")))
                        (is (= 1 ((:active-count route)))))))
                  )
                (is (= [] (chromium/evaluate page "() => window.__v2109.errors")))
                (is (true? (chromium/assert-clean! context))))))
            (finally
              (when-let [ctx @context*] (chromium/close-context! ctx))
              (when-let [br @harness*] (chromium/stop! br))
              (stream/close! correlated-source)
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

(defn- sse-frame-from-http!
  "Read one complete physical SSE frame without abandoning a pending stream
   read. Caller retains and closes the HTTP InputStream after observation."
  [input]
  (let [reader (BufferedReader.
                (InputStreamReader. input StandardCharsets/UTF_8))
        task (future
               (loop [lines []]
                 (let [line (.readLine reader)]
                   (cond
                     (nil? line) lines
                     (empty? line) (if (seq lines) lines (recur []))
                     :else (recur (conj lines line))))))
        observed (deref task 10000 ::sse-timeout)]
    (when (= ::sse-timeout observed)
      (future-cancel task))
    observed))

(deftest physical-sse-frame-progresses-exact-authoritative-fragment-http-test
  ;; This is an HTTP-only contract, not a browser-observed convergence proof.
  ;; Unlike a direct source tap it verifies the SAME progression wire made it
  ;; through Ring SSE and came back on the real client's HTTP fragment GET.
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
              seen-requests (atom [])
              route (http/wrap-authorized-sse
                     started {:path "/live/events"
                              :authorize-subscription
                              (fn [req]
                                (when (= "Bearer accepted"
                                         (get-in req [:headers "authorization"]))
                                  scope))})
              server (start-loopback!
                      (fn [request]
                        (cond
                          (= "/live/events" (:uri request))
                          ((:handler route) request)
                          (= "/fragment" (:uri request))
                          (do (swap! seen-requests conj request)
                              (fragment-response node request))
                          :else {:status 404 :headers {} :body "Not found"})))
              client (HttpClient/newHttpClient)]
          (try
            (xt/execute-tx node [[:put-docs :requests
                                  {:xt/id subject :status :pending}]])
            (let [req (.build (.header
                               (.GET (HttpRequest/newBuilder
                                      (URI/create (str (:url server) "/live/events"))))
                               "authorization" "Bearer accepted"))
                  response-future (future
                                    (.send client req
                                           (HttpResponse$BodyHandlers/ofInputStream)))]
              ;; HttpServer/JDK streaming responses can defer client headers
              ;; until the first body bytes. Establish server-side interest
              ;; and publish before awaiting the HTTP response on the client.
              (is (eventually #(= 1 ((:active-count route)))))
              (let [id (random-uuid)
                      plan (outbox/prepare!
                            {:intent-ids [id] :source-id source-id
                             :operation :request/claim
                             :purpose :gesso.live/refresh}
                            [{:topic :request :id subject
                              :change/kind :updated}])]
                  (xt/execute-tx node
                                 (into (vec (:guard-ops plan))
                                       [[:put-docs :requests
                                         {:xt/id subject :status :claimed}]
                                        (:tx-op plan)]))
                  (let [response (deref response-future 10000 ::response-timeout)
                        _ (is (not= ::response-timeout response))
                        _ (when (not= ::response-timeout response)
                            (is (= 200 (.statusCode response))))
                        lines (when (not= ::response-timeout response)
                                (with-open [body (.body response)]
                                  (sse-frame-from-http! body)))
                        event (first (filter #(str/starts-with? % "event: ") lines))
                        data (first (filter #(str/starts-with? % "data: ") lines))
                        payload (when data (clojure.edn/read-string (subs data 6)))
                        requirement (some-> payload :progression
                                             progression/wire->requirement)
                        header (when requirement
                                 (progression.http/encode-request-progression
                                  requirement))]
                    (is (not= ::sse-timeout lines))
                    (is (= "event: live-update" event))
                    (is (= subject (get-in payload [:invalidation :id])))
                    (is (= :request (get-in payload [:invalidation :topic])))
                    (is (some? requirement))
                    (is (= requirement
                           (progression/wire->requirement
                            (get-in payload [:invalidation :progression]))))
                    (is (string? header))
                    (when header
                      (let [fragment (get-body!
                                      client (:url server) "/fragment"
                                      {progression.http/request-header-name
                                       header})]
                        (is (= 200 (:status fragment)))
                        (is (str/includes?
                             (:body fragment)
                             "data-authoritative-status=\"claimed\""))
                        (is (= header
                               (get-in (last @seen-requests)
                                       [:headers
                                        progression.http/request-header-name]))))))))
            ;; JDK HttpServer may not observe a peer-side InputStream close
            ;; until another write. Explicit adapter shutdown must always
            ;; retire the interest, regardless of disconnect detection.
            (is (= :closed ((:close! route))))
            (is (eventually #(zero? ((:active-count route)))))
            (finally
              ((:close! route))
              ((:close! server))
              (.shutdownNow ^HttpClient client)
              (host/stop! started))))))))


(defn- next-http-sse-frame! [^BufferedReader reader]
  ;; Maintain the SAME reader over successive SSE events. Do not abandon a
  ;; pending read on timeout, and never treat an empty keepalive as a frame.
  (let [task (future
               (loop [lines []]
                 (let [line (.readLine reader)]
                   (cond
                     (nil? line) lines
                     (empty? line) (if (seq lines) lines (recur []))
                     :else (recur (conj lines line))))))
        value (deref task 10000 ::sse-timeout)]
    (when (= ::sse-timeout value)
      (future-cancel task))
    value))

(deftest two-source-events-refresh-one-http-sse-connection-test
  ;; A single HTTP/SSE connection must remain usable after the first
  ;; progression-bearing refresh. This HTTP-only gate establishes server
  ;; framing and repeated authoritative reads; it does NOT establish that
  ;; an EventSource and HTMX survived a browser's outerHTML swap.
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
              seen-requests (atom [])
              route (http/wrap-authorized-sse
                     started {:path "/live/events"
                              :authorize-subscription (fn [request]
                                                        (when (= "Bearer allowed"
                                                                 (get-in request
                                                                         [:headers "authorization"]))
                                                          scope))})
              server (start-loopback!
                      (fn [request]
                        (cond
                          (= "/live/events" (:uri request))
                          ((:handler route) request)
                          (= "/fragment" (:uri request))
                          (do (swap! seen-requests conj request)
                              (fragment-response node request))
                          :else {:status 404 :headers {} :body "Not found"})))
              client (HttpClient/newHttpClient)
              body* (atom nil)
              reader* (atom nil)
              read-future* (atom nil)]
          (try
            (xt/execute-tx node [[:put-docs :requests
                                  {:xt/id subject :status :pending}]])
            (let [req (-> (HttpRequest/newBuilder
                           (URI/create (str (:url server) "/live/events")))
                          (.header "authorization" "Bearer allowed")
                          .GET .build)
                  incoming (future (.send client req
                                          (HttpResponse$BodyHandlers/ofInputStream)))]
              (reset! read-future* incoming)
              ;; The bridge may defer SSE HTTP headers until first event.
              (is (eventually #(= 1 ((:active-count route)))))
              (let [requirements (atom [])]
                (doseq [status [:claimed :done]]
                  (let [id (random-uuid)
                        plan (outbox/prepare!
                              {:intent-ids [id] :source-id source-id
                               :operation :request/claim
                               :purpose :gesso.live/refresh}
                              [{:topic :request :id subject
                                :change/kind :updated}])]
                    (xt/execute-tx node
                                   (into (vec (:guard-ops plan))
                                         [[:put-docs :requests
                                           {:xt/id subject :status status}]
                                          (:tx-op plan)]))
                    (let [response (deref incoming 10000 ::response-timeout)]
                      (is (not= ::response-timeout response))
                      (when (and (not= ::response-timeout response)
                                 (nil? @body*))
                        (reset! body* (.body response)))
                      (when (not= ::response-timeout response)
                        (is (= 200 (.statusCode response))))
                      (when (and @body* (nil? @reader*))
                        (reset! reader* (BufferedReader.
                                         (InputStreamReader. @body* StandardCharsets/UTF_8))))
                      (let [reader @reader*]
                        (when reader
                          (let [frame (next-http-sse-frame! reader)
                                event (first (filter #(str/starts-with? % "event: ") frame))
                                data (first (filter #(str/starts-with? % "data: ") frame))
                                payload (when data (clojure.edn/read-string (subs data 6)))
                                requirement (some-> payload :progression
                                                     progression/wire->requirement)
                                header (when requirement
                                         (progression.http/encode-request-progression
                                          requirement))]
                            (is (not= ::sse-timeout frame))
                            (is (= "event: live-update" event))
                            (is (= subject (get-in payload [:invalidation :id])))
                            (is (some? requirement))
                            (when requirement (swap! requirements conj requirement))
                            (when header
                              (let [fragment (get-body! client (:url server) "/fragment"
                                                        {progression.http/request-header-name header})]
                                (is (= 200 (:status fragment)))
                                (is (str/includes? (:body fragment)
                                                   (str "data-authoritative-status=\""
                                                        (name status) "\"")))
                                (is (= header
                                       (get-in (last @seen-requests)
                                               [:headers progression.http/request-header-name])))))))))
                    (is (eventually
                         #(= :dispatch-accepted
                             (:gesso.live.outbox/status
                              (first (stable-q node
                                               ["SELECT * FROM gesso_live_outbox WHERE _id = ?" id]))))))))
                (is (= 2 (count @requirements)))
                (is (apply not= @requirements))
                (is (= 1 ((:active-count route))))
                (is (= 2 (count @seen-requests)))))
            (finally
              ((:close! route))
              (when-let [reader @reader*] (.close reader))
              (when-let [body @body*] (.close body))
              (when-let [pending @read-future*] (future-cancel pending))
              ((:close! server))
              (.shutdownNow ^HttpClient client)
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
                     #'actual-http-fragment-read-honors-source-progression-test
                     #'physical-sse-frame-progresses-exact-authoritative-fragment-http-test
                     #'two-source-events-refresh-one-http-sse-connection-test])
                   @test/*report-counters*)
                 (run-tests 'gesso.live.outbox.browser-integration))]
    (println "V2112 RESULTS" result)
    (shutdown-agents)
    (System/exit (if (and (zero? (:fail result)) (zero? (:error result))) 0 1))))
