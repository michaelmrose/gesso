(ns gesso.live.outbox.browser-restart-test
  "v2121: Correlate independent tabs with physical server-side fragment reads.

   The replacement host binds the SAME port, reopens the SAME local XTDB
   log/storage, and provides a newly owned Live source/worker. The browser
   reconnects both EventSource streams and delivers progression-correlated
   HTTP/HTMX refreshes to each unchanged page. The per-tab configRequest
   observer checks outgoing headers separately and associates each header
   with an actual successful HTTP response and authoritative HTML. The child
   server independently logs the test-only tab marker and exact received
   progression for every fragment read. This covers process death,
   not an entire machine, disk, or network partition."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.live.browser.chromium :as chromium]
            [gesso.live.progression.http :as progression.http])
  (:import [java.net InetSocketAddress ServerSocket URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.nio.file Files Path]
           [java.time Duration]
           [java.util.concurrent TimeUnit]))

(def ^:private panel "v2109-browser-panel")

(defn- capture-dom-js [tab-id]
  (str "() => { window.__v2121={"
       "tab:" (pr-str tab-id) ","
       "root:document.querySelector('[data-gesso-live-fragment]'),"
       "listener:document.querySelector('[data-gesso-live-invalidation]'),"
       "href:location.href,headers:[],responses:[]};"
       "document.addEventListener('htmx:configRequest',function(event){"
       "const detail=event.detail;"
       "if(detail && detail.elt && "
       "detail.elt.hasAttribute('data-gesso-live-fragment')){"
       "detail.headers['X-Gesso-Test-Tab']=window.__v2121.tab;"
       "window.__v2121.headers.push(detail.headers && detail.headers["
       (pr-str progression.http/request-header-name)
       "]);}});"
       "document.addEventListener('htmx:afterRequest',function(event){"
       "const detail=event.detail;"
       "if(detail && detail.elt && "
       "detail.elt.hasAttribute('data-gesso-live-fragment')){"
       "window.__v2121.responses.push({"
       "header:detail.requestConfig && detail.requestConfig.headers && "
       "detail.requestConfig.headers["
       (pr-str progression.http/request-header-name)
       "],tab:detail.requestConfig && detail.requestConfig.headers && "
       "detail.requestConfig.headers['X-Gesso-Test-Tab'],"
       "status:detail.xhr && detail.xhr.status,"
       "successful:detail.successful === true,"
       "body:detail.xhr && detail.xhr.responseText"
       "});}});"
       "return !!window.__v2121.root && !!window.__v2121.listener; }"))

(def ^:private stable-dom-js
  "() => window.__v2121.root === document.querySelector('[data-gesso-live-fragment]') && window.__v2121.listener === document.querySelector('[data-gesso-live-invalidation]') && window.__v2121.href === location.href")

(defn- wait-for-page-header! [page expected from-index]
  (chromium/wait-for-js!
   page (str "() => window.__v2121.headers.slice(" from-index
             ").includes(" (pr-str expected) ")")))

(defn- wait-for-page-response! [page expected status from-index]
  (chromium/wait-for-js!
   page (str "() => window.__v2121.responses.slice(" from-index
             ").some(response => response.header === " (pr-str expected)
             " && response.tab === window.__v2121.tab"
             " && response.status === 200 && response.successful"
             " && typeof response.body === 'string'"
             " && response.body.includes("
             (pr-str (str "data-authoritative-status=\"" (name status) "\""))
             "))")))

(defn- wait-for-status! [page status]
  (chromium/wait-for-js!
   page (str "() => document.getElementById('" panel
             "')?.getAttribute('data-authoritative-status') === '"
             (name status) "'")))

(defn- independent-tab-fragment-reads? [fragments status progression tab-ids]
  (every? (fn [tab]
            (some (fn [{observed :status header :header received :tab}]
                    (and (= status observed)
                         (= progression header)
                         (= tab received)))
                  fragments))
          tab-ids))

(defn- eventual! [f timeout-ms]
  (let [deadline (+ (System/nanoTime) (* 1000000 timeout-ms))]
    (loop []
      (if-let [value (f)]
        value
        (when (< (System/nanoTime) deadline)
          (Thread/sleep 100)
          (recur))))))

(defn- random-loopback-port []
  (with-open [socket (ServerSocket.)]
    (.bind socket (InetSocketAddress. "127.0.0.1" 0))
    (.getLocalPort socket)))

(defn- http! [^HttpClient client base path]
  (let [req (-> (HttpRequest/newBuilder (URI/create (str base path)))
                (.timeout (Duration/ofSeconds 20))
                .GET .build)
        resp (.send client req (HttpResponse$BodyHandlers/ofString))]
    (when-not (= 200 (.statusCode resp))
      (throw (ex-info "Child HTTP request failed"
                      {:path path :status (.statusCode resp)
                       :body (.body resp)})))
    (if (= path "/control/stop")
      {:status :stopping}
      (edn/read-string (.body resp)))))

(defn- start-child! [^Path root port phase]
  (let [java (str (System/getProperty "java.home") "/bin/java")
        ready (.resolve root (str phase ".ready.edn"))
        log (.resolve root (str phase ".log"))
        cmd [java "-Xmx1536m"
             "--add-opens=java.base/java.nio=ALL-UNNAMED"
             "--enable-native-access=ALL-UNNAMED"
             "-Dio.netty.tryReflectionSetAccessible=true"
             "-Dio.netty.noUnsafe=false"
             "-cp" (System/getProperty "java.class.path")
             "clojure.main" "-m"
             "gesso.live.outbox.browser-restart-child"
             phase (str root) (str port) (str ready)]
        pb (ProcessBuilder. (into-array String cmd))]
    (.redirectErrorStream pb true)
    (.redirectOutput pb (.toFile log))
    (let [proc (.start pb)
          running? (eventual!
                    #(or (and (Files/exists ready
                                             (make-array java.nio.file.LinkOption 0))
                              (.isAlive proc))
                         (when-not (.isAlive proc)
                           (throw (ex-info "Child JVM terminated before HTTP readiness"
                                           {:phase phase
                                            :exit (.exitValue proc)
                                            :log (slurp (.toFile log))}))))
                    105000)]
      (when-not running?
        (.destroyForcibly proc)
        (throw (ex-info "Child JVM startup timeout" {:phase phase
                                                      :log (slurp (.toFile log))})))
      {:process proc :log log :phase phase})))

(defn- stop-child! [child]
  (when-let [^Process p (:process child)]
    (when (.isAlive p)
      (.destroyForcibly p)
      (.waitFor p 15 TimeUnit/SECONDS))))

(defn- delete-root! [^Path root]
  (doseq [f (reverse (file-seq (.toFile root)))]
    (io/delete-file f true)))

(deftest browser-survives-physical-host-jvm-kill-and-reopens-persistent-xtdb-test
  (let [root (Files/createTempDirectory
              "gesso-v2121-attributed-two-tab-restart-"
              (make-array java.nio.file.attribute.FileAttribute 0))
        port (random-loopback-port)
        base (str "http://127.0.0.1:" port)
        client (HttpClient/newHttpClient)
        initial* (atom nil)
        reopened* (atom nil)
        harness* (atom nil)
        context* (atom nil)]
    (try
      (let [first-child (start-child! root port "initial")
            _ (reset! initial* first-child)
            first-status (http! client base "/control/status")
            harness (chromium/start! {:args ["--no-sandbox"
                                             "--disable-dev-shm-usage"]})
            _ (reset! harness* harness)
            ctx (chromium/new-context! harness)
            _ (reset! context* ctx)
            ;; Separate physical tabs share the same authorized browser session,
            ;; but each owns its own EventSource, HTMX root, and pending state.
            tabs [(chromium/new-page! ctx) (chromium/new-page! ctx)]
            tab-ids (mapv (fn [_] (str (random-uuid))) tabs)]
        (is (= :pending (:domain first-status)))
        (is (= 2 (count (set tab-ids))))
        (doseq [[page tab-id] (map vector tabs tab-ids)]
          (chromium/navigate! page (str base "/"))
          (chromium/wait-for-js!
           page "() => window.__v2109 && window.__v2109.runtime === true && window.__v2109.opens >= 1")
          (wait-for-status! page :pending)
          ;; Keep the precise original nodes and URL in each page. A reload
          ;; or silently remounted root in either tab invalidates the evidence.
          (is (true? (chromium/evaluate page (capture-dom-js tab-id)))))
        (is (true?
             (eventual!
              #(= 2 (:active (http! client base "/control/status")))
              10000))
            "Both independent EventSource subscriptions must be active")
        (let [claim (http! client base "/control/claim")]
          (is (= :claimed (:status claim)))
          (is (string? (:expected-header claim)))
          (doseq [page tabs]
            (wait-for-status! page :claimed)
            (wait-for-page-header! page (:expected-header claim) 0)
            (wait-for-page-response! page (:expected-header claim) :claimed 0))
          (is (true?
               (eventual!
                #(independent-tab-fragment-reads?
                  (:fragments (http! client base "/control/status"))
                  :claimed (:expected-header claim) tab-ids)
                10000))
              "Both distinct tab IDs must be received with exact claim progression")
          (let [old-pid (:pid first-status)
                old-opens (mapv #(long (chromium/evaluate
                                        % "() => window.__v2109.opens")) tabs)]
            ;; Kill the ENTIRE child JVM. No orderly Biff/Live/XTDB close.
            ;; Both pages stay open in Chromium throughout the interruption.
            (.destroyForcibly ^Process (:process first-child))
            (is (.waitFor ^Process (:process first-child)
                          20 TimeUnit/SECONDS))
            (is (not= 0 (.exitValue ^Process (:process first-child))))
            (let [second-child (start-child! root port "reopened")
                  _ (reset! reopened* second-child)
                  recovered (http! client base "/control/status")]
              (is (not= old-pid (:pid recovered)))
              (is (= :claimed (:domain recovered))
                  "The replacement JVM must reopen the persisted XTDB change")
              (doseq [[page opens] (map vector tabs old-opens)]
                (chromium/wait-for-js!
                 page (str "() => window.__v2109.opens > " opens))
                (is (true? (chromium/evaluate page stable-dom-js))))
              (is (true?
                   (eventual!
                    #(= 2 (:active (http! client base "/control/status")))
                    10000))
                  "Both EventSource sessions must reconnect to the new JVM")
              ;; Clear only diagnostics caused by the intentionally lost host,
              ;; after BOTH independent EventSource connections are reopened.
              (chromium/clear-diagnostics! ctx)
              (let [baselines (mapv #(long (chromium/evaluate
                                           % "() => window.__v2109.refreshes")) tabs)
                    header-baselines (mapv #(long (chromium/evaluate
                                                  % "() => window.__v2121.headers.length")) tabs)
                    response-baselines (mapv #(long (chromium/evaluate
                                                    % "() => window.__v2121.responses.length")) tabs)
                    done (http! client base "/control/done")]
                (is (= :done (:status done)))
                (is (string? (:expected-header done)))
                (is (not= (:expected-header claim) (:expected-header done))
                    "The post-restart write must advance the source requirement")
                (doseq [[page baseline header-baseline response-baseline]
                        (map vector tabs baselines header-baselines
                             response-baselines)]
                  (wait-for-status! page :done)
                  (wait-for-page-header! page (:expected-header done)
                                         header-baseline)
                  (wait-for-page-response! page (:expected-header done)
                                           :done response-baseline)
                  (is (> (long (chromium/evaluate
                                page "() => window.__v2109.refreshes"))
                         baseline))
                  (is (true? (chromium/evaluate page stable-dom-js))))
                (is (true?
                     (eventual!
                      #(independent-tab-fragment-reads?
                        (:fragments (http! client base "/control/status"))
                        :done (:expected-header done) tab-ids)
                      10000))
                    "Both distinct tab IDs must be received with new source progression")
                (is (= :done (:domain (http! client base "/control/status"))))
                (is (true? (chromium/assert-clean! ctx))))))))
      (finally
        (when-let [ctx @context*] (chromium/close-context! ctx))
        (when-let [harness @harness*] (chromium/stop! harness))
        (when @reopened*
          (try (http! client base "/control/stop")
               (catch Throwable _ nil))
          (let [^Process proc (:process @reopened*)]
            (is (.waitFor proc 30 TimeUnit/SECONDS)
                "Replacement host must shut down normally")
            (when-not (.isAlive proc)
              (is (zero? (.exitValue proc))
                  (str "Replacement host shutdown log: "
                       (slurp (.toFile (:log @reopened*))))))))
        (stop-child! @initial*)
        (stop-child! @reopened*)
        (.shutdownNow ^HttpClient client)
        (delete-root! root)))))
