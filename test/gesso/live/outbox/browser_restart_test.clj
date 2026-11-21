(ns gesso.live.outbox.browser-restart-test
  "v2117: Chromium remains alive while the entire concrete host JVM dies.

   The replacement host binds the SAME port, reopens the SAME local XTDB
   log/storage, and provides a newly owned Live source/worker. The browser
   receives an EventSource reconnect and a progression-correlated HTTP/HTMX
   refresh without page navigation. This covers physical process death,
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
              "gesso-v2117-browser-process-restart-"
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
            page (chromium/new-page! ctx)]
        (is (= :pending (:domain first-status)))
        (chromium/navigate! page (str base "/"))
        (chromium/wait-for-js!
         page "() => window.__v2109 && window.__v2109.runtime === true && window.__v2109.opens >= 1")
        (chromium/wait-for-js!
         page "() => document.getElementById('v2109-browser-panel')?.getAttribute('data-authoritative-status') === 'pending'")
        ;; Capture original DOM node identities and browser state before the
        ;; host JVM is killed. A reload/recreated page cannot satisfy this.
        (is (true? (chromium/evaluate
                    page
                    "() => { window.__v2117={root:document.querySelector('[data-gesso-live-fragment]'),listener:document.querySelector('[data-gesso-live-invalidation]'),href:location.href}; return !!window.__v2117.root && !!window.__v2117.listener; }")))
        (let [claim (http! client base "/control/claim")]
          (is (= :claimed (:status claim)))
          (is (string? (:expected-header claim)))
          (chromium/wait-for-js!
           page "() => document.getElementById('v2109-browser-panel')?.getAttribute('data-authoritative-status') === 'claimed'")
          (is (true?
               (eventual!
                #(some (fn [{:keys [status header]}]
                         (and (= :claimed status)
                              (= (:expected-header claim) header)))
                       (:fragments (http! client base "/control/status")))
                10000)))
          (let [old-pid (:pid first-status)
                old-opens (long (chromium/evaluate page
                                                  "() => window.__v2109.opens"))]
            ;; Kill the ENTIRE child JVM. No Biff stop, Live close, normal
            ;; XTDB close or HTTP shutdown runs. Chromium remains untouched.
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
              (chromium/wait-for-js!
               page (str "() => window.__v2109.opens > " old-opens))
              (is (true?
                   (chromium/evaluate
                    page
                    "() => window.__v2117.root === document.querySelector('[data-gesso-live-fragment]') && window.__v2117.listener === document.querySelector('[data-gesso-live-invalidation]') && window.__v2117.href === location.href")))
              (is (eventual! #(pos? (:connections (http! client base "/control/status")))
                             10000))
              ;; Diagnostics caused by the intentional forced disconnect
              ;; are separately observable; clear only after the re-open.
              (chromium/clear-diagnostics! ctx)
              (let [baseline (long (chromium/evaluate
                                    page "() => window.__v2109.refreshes"))
                    done (http! client base "/control/done")]
                (is (= :done (:status done)))
                (is (string? (:expected-header done)))
                (chromium/wait-for-js!
                 page "() => document.getElementById('v2109-browser-panel')?.getAttribute('data-authoritative-status') === 'done'")
                (is (> (long (chromium/evaluate
                              page "() => window.__v2109.refreshes"))
                       baseline))
                (is (true?
                     (eventual!
                      #(some (fn [{:keys [status header]}]
                               (and (= :done status)
                                    (= (:expected-header done) header)))
                             (:fragments (http! client base "/control/status")))
                      10000)))
                (is (= :done (:domain (http! client base "/control/status"))))
                (is (true?
                     (chromium/evaluate
                      page
                      "() => window.__v2117.root === document.querySelector('[data-gesso-live-fragment]') && window.__v2117.listener === document.querySelector('[data-gesso-live-invalidation]') && window.__v2117.href === location.href")))
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
