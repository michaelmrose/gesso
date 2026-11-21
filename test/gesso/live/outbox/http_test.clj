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
