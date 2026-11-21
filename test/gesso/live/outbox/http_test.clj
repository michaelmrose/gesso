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
