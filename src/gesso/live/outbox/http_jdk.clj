(ns gesso.live.outbox.http-jdk
  "Opt-in physical JDK HttpServer adapter for a single-node Gesso Ring route.

   This is a concrete, loopback-first, blocking reference HTTP adapter, NOT
   Aleph and NOT the automatically selected Biff listener. The adapter's
   owned listener guard spans handler invocation AND the entire physical
   response write. It is suitable for local integration tests and explicit
   small deployments; a thread is occupied for each long-lived SSE response.

   Stop this listener through the managed HTTP coordinator, BEFORE stopping
   the Gesso host. Socket stop closes live connections; no TCP/browser receipt
   is inferred from completed server-side writes."
  (:require [clojure.string :as str]
            [gesso.live.outbox.http-listener :as listener]
            [manifold.stream :as stream])
  (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.net InetSocketAddress]
           [java.nio.charset StandardCharsets]
           [java.util Locale]
           [java.util.concurrent Executors ThreadFactory]))

(defn- reject! [reason]
  (throw (ex-info "Invalid physical JDK HTTP listener."
                  {:error/type :gesso.live.outbox/invalid-http-jdk
                   :reason reason})))

(defn- request-map [^HttpExchange exchange]
  (let [uri (.getRequestURI exchange)
        path (.getRawPath uri)
        method (.getRequestMethod exchange)
        fields (.entrySet (.getRequestHeaders exchange))]
    ;; This selected reference transport does not admit alternative path
    ;; spellings or multi-valued header fields. Never discard information
    ;; before route selection, authorization or progression validation.
    (when (and (string? path) (str/starts-with? path "/")
               ;; HTTP methods are case-sensitive. Ring uses lowercase
               ;; keywords, so admit only uppercase ASCII method tokens.
               (re-matches #"[A-Z][A-Z0-9-]*" method)
               (not (.isAbsolute uri)) (nil? (.getRawAuthority uri))
               (nil? (.getRawFragment uri))
               (= path (.getPath uri) (.getRawPath (.normalize uri)))
               (every? (fn [[_ values]] (= 1 (count values))) fields))
      {:uri path
       :query-string (.getRawQuery uri)
       :request-method (keyword (.toLowerCase ^String method Locale/ROOT))
       :headers (into {}
                      (map (fn [[name values]]
                             [(.toLowerCase ^String name Locale/ROOT) (first values)])) fields)
       :body (.getRequestBody exchange)
       :scheme :http
       :server-port (.getPort (.getLocalAddress exchange))
       :remote-addr (some-> exchange .getRemoteAddress .getAddress .getHostAddress)})))

(def ^:private ambiguous-request-response
  {:status 400 :headers {"content-type" "text/plain; charset=utf-8"
                         "cache-control" "no-store"}
   :body "Unsupported or ambiguous HTTP request."})

(defn- write-stream! [body out]
  ;; Keep the original take! during timeout polling; starting another take!
  ;; would orphan the first and consume the frame invisibly.
  (loop [pending (stream/take! body)]
    (let [frame (deref pending 500 ::pending)]
      (cond
        (= ::pending frame) (when-not (stream/closed? body)
                              (recur pending))
        (nil? frame) nil
        :else (do
                (let [bytes (if (bytes? frame)
                              frame
                              (.getBytes (str frame) StandardCharsets/UTF_8))]
                  (.write out ^bytes bytes)
                  (.flush out))
                (recur (stream/take! body)))))))

(defn- write-response! [^HttpExchange exchange response]
  (when-not (and (map? response)
                 (integer? (:status response))
                 (<= 100 (:status response) 599)
                 (map? (:headers response)))
    (reject! :invalid-ring-response))
  (let [body (:body response)
        streaming? (stream/stream? body)
        headers (.getResponseHeaders exchange)
        status (:status response)]
    (doseq [[k v] (:headers response)]
      (.set headers (name k) (str v)))
    (try
      (cond
        (nil? body) (.sendResponseHeaders exchange status -1)
        streaming?
        (do
          (.sendResponseHeaders exchange status 0)
          (with-open [out (.getResponseBody exchange)]
            (write-stream! body out)))
        :else
        (let [bytes (cond
                      (string? body) (.getBytes ^String body StandardCharsets/UTF_8)
                      (bytes? body) body
                      :else nil)]
          (when-not bytes
            (reject! :unsupported-ring-body))
          ;; JDK HttpServer uses -1 for no-body; explicit positive length for
          ;; nonempty finite responses. A zero-length body has no writer.
          (if (zero? (alength ^bytes bytes))
            (.sendResponseHeaders exchange status -1)
            (do (.sendResponseHeaders exchange status (alength ^bytes bytes))
                (with-open [out (.getResponseBody exchange)]
                  (.write out ^bytes bytes))))))
      (finally
        (when streaming? (stream/close! body))))))

(defn start!
  "Start the explicitly selected blocking HTTP adapter around `ring-handler`.

   Options: :host (default 127.0.0.1), :port (0..65535, default 0),
   :threads (1..64, default 8), :drain-timeout-ms (0..120000, default 5000).
   Requests must use uppercase ASCII method names (letters, digits or
   hyphens, beginning with a letter), an unescaped normalized origin-form path and one
   value per physical header name (comma-separated field values remain
   intact). Invalid requests get 400 before Ring dispatch. Raw query
   strings are unchanged. This is a deliberately narrow reference profile,
   not a general-purpose URI normalization or header combination policy.

   All callbacks are synchronous from the caller's perspective. Return
   :base-url, :port, :stop! (optional timeout), :stop-listener! (zero-arg
   shutdown-plan callback), :drained?, :active-count, :status. No Biff or
   XTDB module is automatically started or stopped. The owning host must
   call the managed shutdown coordinator to retire route, listener and
   providers in that order."
  [ring-handler options]
  (when-not (fn? ring-handler) (reject! :invalid-handler))
  (when-not (and (map? options)
                 (every? #{:host :port :threads :drain-timeout-ms} (keys options)))
    (reject! :invalid-options))
  (let [host (get options :host "127.0.0.1")
        port (get options :port 0)
        threads (get options :threads 8)
        timeout (get options :drain-timeout-ms 5000)]
    (when-not (and (string? host) (not (str/blank? host)))
      (reject! :invalid-host))
    (when-not (and (integer? port) (<= 0 port 65535))
      (reject! :invalid-port))
    (when-not (and (integer? threads) (<= 1 threads 64))
      (reject! :invalid-threads))
    (when-not (and (integer? timeout) (<= 0 timeout 120000))
      (reject! :invalid-drain-timeout))
    (let [^HttpServer server (HttpServer/create (InetSocketAddress. ^String host (int port)) 0)
          factory (reify ThreadFactory
                    (newThread [_ runnable]
                      (doto (Thread. ^Runnable runnable "gesso-jdk-http")
                        (.setDaemon true))))
          executor (Executors/newFixedThreadPool (int threads) factory)
          owner (listener/prepare!
                 {:dispatch! (fn [^HttpExchange exchange]
                               (try
                                 (write-response! exchange
                                                  (if-let [request (request-map exchange)]
                                                    (ring-handler request)
                                                    ambiguous-request-response))
                                 (finally (.close exchange))))
                  :on-rejected! (fn [^HttpExchange exchange]
                                  ;; This response is handled without host
                                  ;; providers. Socket stop may have closed
                                  ;; the exchange already; just retire it.
                                  (.close exchange))
                  :stop-accept! (fn []
                                  (.stop server 0)
                                  (.shutdown executor)
                                  {:outcome :stopped})})]
      (try
        (.setExecutor server executor)
        (.createContext server "/" (reify HttpHandler
                                     (handle [_ exchange]
                                       ((:handle! owner) exchange))))
        (.start server)
        (let [port (.getPort (.getAddress server))]
          {:base-url (str "http://" host ":" port)
           :port port
           :stop! (fn
                    ([] ((:stop! owner) timeout))
                    ([ms] ((:stop! owner) ms)))
           :stop-listener! (fn [] ((:stop! owner) timeout))
           :drained? (:drained? owner)
           :active-count (:active-count owner)
           :status (:status owner)})
        (catch Throwable error
          (try (.stop server 0) (catch Throwable _ nil))
          (.shutdownNow executor)
          (throw error))))))
