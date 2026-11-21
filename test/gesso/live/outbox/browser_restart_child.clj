(ns gesso.live.outbox.browser-restart-child
  "v2121 physical child-JVM server records per-tab HTMX read attribution.

   Owns canonical Gesso/Biff app, local persistent XTDB, Live source, worker,
   and an actual loopback HTTP/SSE listener. The controlling test runs in a
   DIFFERENT JVM and keeps its browser alive across an ungraceful OS kill of
   this process. Control URLs are test-only and bound to 127.0.0.1; they are
   not production application endpoints or authentication mechanisms."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [gesso.core :as g]
            [gesso.live.application-preflight.fixture
             :refer [with-surfaced-application standard-operations render-context
                     rendered-operation-button]]
            [gesso.live.core :as live]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.outbox.host :as host]
            [gesso.live.outbox.http :as http]
            [gesso.live.outbox.browser-integration :as fixture]
            [gesso.live.progression.http :as progression.http]
            [gesso.live.source :as source]
            [manifold.stream :as stream]
            [xtdb.api :as xt])
  (:import [com.sun.net.httpserver HttpServer HttpHandler HttpExchange]
           [java.net InetSocketAddress]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent Executors TimeUnit]))

(def ^:private subject "v2109-browser-request")
(def ^:private scope {:topic :request :id subject})
(def ^:private source-id "gesso/v2117-restart")
(def ^:private html-headers {"content-type" "text/html; charset=utf-8"})
(def ^:private pid (.pid (java.lang.ProcessHandle/current)))

(defn- config [root phase]
  {:xtdb-config {:log [:local {:path (str root "/log")}]
                 :storage [:local {:path (str root "/storage")}]}
   :source-id source-id
   :worker-options {:worker-id (str "v2117-" phase) :poll-ms 25}
   :rules-context {}
   :live-options {:rules [{:when-topic :request
                           :expand (fn [_ change]
                                     [(select-keys change [:topic :id :change/kind])])}]
                  :source-options {:coalesce-window-ms 0}
                  :dispatch-options {:threads 1 :queue-size 8
                                     :on-overflow :throw}}})

(defn- query! [node statement]
  (loop [attempts 8]
    (let [result (try {:value (xt/q node statement)}
                      (catch Throwable e {:failure e}))]
      (if-let [e (:failure result)]
        (if (and (pos? attempts)
                 (= :prepared-query-out-of-date
                    (:xtdb.error/code (ex-data e))))
          (do (Thread/sleep 30) (recur (dec attempts)))
          (throw e))
        (:value result)))))

(defn- domain-state [node]
  (:status (first (query! node ["SELECT * FROM requests WHERE _id = ?" subject]))))

(defn- wait-for-accepted! [node id]
  (loop [n 0]
    (let [status (:gesso.live.outbox/status
                  (first (query! node
                                 ["SELECT * FROM gesso_live_outbox WHERE _id = ?" id])))]
      (cond (= status :dispatch-accepted) true
            (>= n 400) (throw (ex-info "Outbox never dispatched" {:id id :status status}))
            :else (do (Thread/sleep 25) (recur (inc n)))))))

(defn- mutate! [node tap status]
  (let [intent (random-uuid)
        plan (outbox/prepare!
              {:intent-ids [intent] :source-id source-id
               :operation (case status :claimed :request/claim
                                :done :request/done)
               :purpose :gesso.live/refresh}
              [{:topic :request :id subject :change/kind :updated}])]
    (xt/execute-tx node
                   (into (vec (:guard-ops plan))
                         [[:put-docs :requests
                           {:xt/id subject :status status}]
                          (:tx-op plan)]))
    (let [event (deref (stream/take! tap) 10000 ::timeout)]
      (when-not (and (= subject (:id event))
                     (some? (:progression event)))
        (throw (ex-info "Source invalidation not witnessed" {:event event})))
      (wait-for-accepted! node intent)
      {:status status
       :intent-id intent
       :expected-header (progression.http/encode-request-progression
                         (:progression event))
       :pid pid})))

(defn- write-response! [^HttpExchange exchange response]
  (let [status (:status response)
        body (:body response)
        headers (.getResponseHeaders exchange)]
    (doseq [[key value] (:headers response)]
      (.set headers (str key) (str value)))
    (if (stream/stream? body)
      (do
        (.sendResponseHeaders exchange status 0)
        (try
          (with-open [out (.getResponseBody exchange)]
            (loop [pending (stream/take! body)]
              ;; Preserve the outstanding take on timeout. Discarding the
              ;; deferred consumes later SSE frames invisibly.
              (let [item (deref pending 500 ::timeout)]
                (cond (= ::timeout item)
                      (when-not (stream/closed? body) (recur pending))
                      (nil? item) nil
                      :else (do
                              (.write out (.getBytes (str item) StandardCharsets/UTF_8))
                              (.flush out)
                              (recur (stream/take! body)))))))
          (finally (stream/close! body))))
      (let [bytes (.getBytes (str (or body "")) StandardCharsets/UTF_8)]
        (.sendResponseHeaders exchange status (alength bytes))
        (with-open [out (.getResponseBody exchange)]
          (.write out bytes))))))

(defn- start-http! [port ring-handler]
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" port) 0)
        pool (Executors/newCachedThreadPool)]
    (.setExecutor server pool)
    (.createContext
     server "/"
     (reify HttpHandler
       (handle [_ exchange]
         (try
           (let [request {:uri (.getPath (.getRequestURI exchange))
                          :request-method (keyword
                                           (str/lower-case (.getRequestMethod exchange)))
                          :headers (into {}
                                         (map (fn [[key values]]
                                                [(str/lower-case key) (first values)]))
                                         (.entrySet (.getRequestHeaders exchange)))}]
             (write-response! exchange (ring-handler request)))
           (catch Throwable ex
             (binding [*out* *err*]
               (println "v2117 child HTTP failure" (.getMessage ex))
               (.printStackTrace ex))
             (try (.close exchange) (catch Throwable _ nil)))))))
    (.start server)
    {:stop! (fn [] (.stop server 0) (.shutdownNow pool))}))

(defn- serve! [phase root port ready-file]
  (let [ops (standard-operations)
        ctx (render-context ops)
        quit (promise)]
    (with-surfaced-application
      {:request-board [:main (rendered-operation-button
                              ctx :request/claim "/operations/request/claim")]}
      (fn [{:keys [assembly]}]
        (let [handler (fn [_]
                        (g/html-response
                         [:main (rendered-operation-button
                                 ctx :request/claim "/operations/request/claim")]))
              started (host/start! assembly handler (config root phase))
              node (:xtdb/connectable started)
              system (:gesso.live/system started)
              tap (source/subscribe (:source system) scope)
              history (atom [])
              connections (atom 0)
              route (http/wrap-authorized-sse
                     started {:path "/live/events"
                              :authorize-subscription
                              (fn [req]
                                (when (str/includes?
                                       (or (get-in req [:headers "cookie"]) "")
                                       "v2109-test-session=1")
                                  scope))})]
          (try
            (when (= phase "initial")
              (xt/execute-tx node
                             [[:put-docs :requests
                               {:xt/id subject :status :pending}]]))
            (let [assets (fixture/restart-fixture-assets)
                  page (fixture/restart-fixture-page ctx)
                  server (start-http!
                          port
                          (fn [request]
                            (let [uri (:uri request)]
                              (cond
                                (= uri "/")
                                {:status 200
                                 :headers (assoc html-headers
                                                 "set-cookie" "v2109-test-session=1; Path=/; SameSite=Lax")
                                 :body page}

                                (contains? assets uri)
                                {:status 200 :headers {"content-type" "text/javascript"}
                                 :body (get assets uri)}

                                (= uri "/live/events")
                                (let [response ((:handler route) request)]
                                  (when (stream/stream? (:body response))
                                    (swap! connections inc))
                                  response)

                                (= uri "/fragment")
                                (let [response (fixture/restart-fixture-fragment
                                                node request)]
                                  (swap! history conj
                                         {:status (domain-state node)
                                          :header (get-in request
                                                          [:headers progression.http/request-header-name])
                                          ;; Test-only provenance marker, never an
                                          ;; application identity or auth credential.
                                          :tab (get-in request
                                                       [:headers "x-gesso-test-tab"])})
                                  response)

                                (= uri "/control/status")
                                {:status 200 :body (pr-str
                                                     {:pid pid :phase phase
                                                      :domain (domain-state node)
                                                      :fragments @history
                                                      :connections @connections
                                                      :active ((:active-count route))})}

                                (= uri "/control/claim")
                                {:status 200 :body (pr-str (mutate! node tap :claimed))}

                                (= uri "/control/done")
                                {:status 200 :body (pr-str (mutate! node tap :done))}

                                (= uri "/control/stop")
                                (do (deliver quit :stop)
                                    {:status 200 :body "stopping"})

                                :else ((:handler route) request)))))]
              (try
                ;; Readiness is emitted only after the real HTTP port is bound
                ;; and the canonical host has initialized all providers.
                (spit ready-file (pr-str {:pid pid :phase phase :port port}))
                (when-not (= :stop (deref quit 150000 ::timeout))
                  (throw (ex-info "Timed out waiting for controller" {:phase phase})))
                (finally ((:stop! server)))))
            (finally
              ((:close! route))
              (stream/close! tap)
              (host/stop! started))))))))

(defn -main [phase root port ready-file]
  (try
    (when-not (#{"initial" "reopened"} phase)
      (throw (ex-info "Unknown restart phase" {:phase phase})))
    (serve! phase root (Integer/parseInt port) ready-file)
    (shutdown-agents)
    (System/exit 0)
    (catch Throwable e
      (binding [*out* *err*] (.printStackTrace e))
      (shutdown-agents)
      (System/exit 2))))
