(ns gesso.live.outbox.browser-restart-test
  "v2118: Two independent Chromium tabs survive physical host JVM death.

   The replacement host binds the SAME port, reopens the SAME local XTDB
   log/storage, and provides a newly owned Live source/worker. The browser
   reconnects both EventSource streams and delivers progression-correlated
   HTTP/HTMX refreshes to each unchanged page. This covers process death,
   not an entire machine, disk, or network partition."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.live.browser.chromium :as chromium])
  (:import [java.net InetSocketAddress ServerSocket URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.nio.file Files Path]
           [java.time Duration]
           [java.util.concurrent TimeUnit]))

(def ^:private panel "v2109-browser-panel")

(def ^:private capture-dom-js
  "() => { window.__v2118={root:document.querySelector('[data-gesso-live-fragment]'),listener:document.querySelector('[data-gesso-live-invalidation]'),href:location.href}; return !!window.__v2118.root && !!window.__v2118.listener; }")

(def ^:private stable-dom-js
  "() => window.__v2118.root === document.querySelector('[data-gesso-live-fragment]') && window.__v2118.listener === document.querySelector('[data-gesso-live-invalidation]') && window.__v2118.href === location.href")

(defn- wait-for-status! [page status]
  (chromium/wait-for-js!
   page (str "() => document.getElementById('" panel
             "')?.getAttribute('data-authoritative-status') === '"
             (name status) "'")))

(defn- at-least-two-fragment-reads? [fragments status progression]
  (<= 2 (count (filter (fn [{observed :status header :header}]
                         (and (= status observed) (= progression header)))
                       fragments))))

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
              "gesso-v2118-two-tab-process-restart-"
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
            tabs [(chromium/new-page! ctx) (chromium/new-page! ctx)]]
        (is (= :pending (:domain first-status)))
        (doseq [page tabs]
          (chromium/navigate! page (str base "/"))
          (chromium/wait-for-js!
           page "() => window.__v2109 && window.__v2109.runtime === true && window.__v2109.opens >= 1")
          (wait-for-status! page :pending)
          ;; Keep the precise original nodes and URL in each page. A reload
          ;; or silently remounted root in either tab invalidates the evidence.
          (is (true? (chromium/evaluate page capture-dom-js))))
        (is (true?
             (eventual!
              #(= 2 (:active (http! client base "/control/status")))
              10000))
            "Both independent EventSource subscriptions must be active")
        (let [claim (http! client base "/control/claim")]
          (is (= :claimed (:status claim)))
          (is (string? (:expected-header claim)))
          (doseq [page tabs] (wait-for-status! page :claimed))
          (is (true?
               (eventual!
                #(at-least-two-fragment-reads?
                  (:fragments (http! client base "/control/status"))
                  :claimed (:expected-header claim))
                10000))
              "Both tabs must issue an exact-progression HTMX claim refresh")
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
                    done (http! client base "/control/done")]
                (is (= :done (:status done)))
                (is (string? (:expected-header done)))
                (doseq [[page baseline] (map vector tabs baselines)]
                  (wait-for-status! page :done)
                  (is (> (long (chromium/evaluate
                                page "() => window.__v2109.refreshes"))
                         baseline))
                  (is (true? (chromium/evaluate page stable-dom-js))))
                (is (true?
                     (eventual!
                      #(at-least-two-fragment-reads?
                        (:fragments (http! client base "/control/status"))
                        :done (:expected-header done))
                      10000))
                    "Both tabs must request the exact new source progression")
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
