(ns gesso.assurance.release.fenced-biff-http-probe-shutdown-test
  "v2302: Permanent live-HTTP qualification of private-probe admission,
   synchronous probe drain, and fail-closed retries at the selected
   Biff traffic fence. Controlled installed-source callbacks are explicit
   test doubles, not independent source or release authority.
   v2293 permanently checks that private probes can call summary! while
   physical stop drains their read leases without a monitor-order deadlock.
   v2295 qualifies original synchronous startup probes through summary!
   and rejects cross-thread stop during either successful or failed startup.
   v2298 captures the ordinary Ring request / queued diagnostic summary /
   shutdown deadlock with deterministic lock-order coordination.
   v2300 qualifies cleanup-retry callbacks that consult the outer lifecycle
   summary from another thread while the one-use retry is in progress.
   v2302 rejects protected handler and guard self-stop before consuming
   the one-use external shutdown, including nested Ring dispatch.

   After stop begins, even a valid assurance marker cannot bypass
   the closed fence on a surviving physical HTTP listener."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [gesso.assurance.release.fenced-biff-http :as outer]
            [gesso.assurance.release.installed-source :as installed]
            [gesso.assurance.proof.checked-biff-http-application-configuration-fixture :as fixture]
            [gesso.live.application-preflight :as app])
  (:import [com.sun.net.httpserver HttpServer HttpHandler HttpExchange]
           [java.net InetSocketAddress URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.time Duration]
           [java.util.concurrent CountDownLatch TimeUnit]))

(def ^:private header-probes
  [{:path "/anything" :header "x-gesso-local-liveness-probe" :value "forged"}
   {:path "/anything" :header "x-gesso-operation-route-probe" :value "forged"}
   {:path "/__gesso_assurance_http_consumption/abcdef12-1234-1234-1234-abcdef123456"
    :header "x-gesso-consumption-probe" :value "forged"}])

(defn- request! [port path header value]
  (let [uri (URI/create (str "http://127.0.0.1:" port path))
        build (-> (HttpRequest/newBuilder uri)
                  (.timeout (Duration/ofSeconds 3))
                  (.GET))
        _ (when header (.header build header value))]
    (.statusCode (.send (HttpClient/newHttpClient) (.build build)
                        (HttpResponse$BodyHandlers/ofString)))))

(defn- host [retry? & [blocking]]
  (let [opened (atom [])
        post-stop? (atom false)
        listener (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
        port (.getPort (.getAddress listener))
        captured-wire (atom nil)
        seen (atom [])
        cleanup-retried? (atom false)
        retry-while-closed (atom [])
        stop-while-closing (atom [])
        stopped? (atom false)
        raw {:biff.core/id :fake/installed-adapter
             :biff.core/start (fn [system]
                                (reset! captured-wire (get system app/biff-ring-handler-key))
                                (.start listener)
                                (swap! opened conj :started)
                                system)
             :biff.core/stop (fn [_] nil)}
        original (fn [request]
                   (swap! seen conj request)
                   (when-let [on-original (:on-original blocking)]
                     (on-original request))
                   (when (and blocking
                              (= "slow" (get-in request [:headers "x-gesso-local-liveness-probe"])))
                     (.countDown ^CountDownLatch (:entered blocking))
                     (.await ^CountDownLatch (:release blocking) 5 TimeUnit/SECONDS))
                   {:status 200 :headers {"content-type" "text/plain"} :body "original"})
        _ (.createContext listener "/"
                          (reify HttpHandler
                            (^void handle [_ ^HttpExchange exchange]
                              (try
                                (let [headers (into {} (map (fn [entry]
                                                              [(str/lower-case (key entry))
                                                               (first (val entry))])
                                                            (.entrySet (.getRequestHeaders exchange))))
                                      req {:uri (.getPath (.getRequestURI exchange))
                                           :request-method (keyword (str/lower-case (.getRequestMethod exchange)))
                                           :headers headers}
                                      handler @captured-wire
                                      response (if handler (handler req)
                                                   {:status 503 :body "no handler"})
                                      bytes (.getBytes ^String (str (:body response)) "UTF-8")]
                                  (.sendResponseHeaders exchange (:status response) (count bytes))
                                  (with-open [out (.getResponseBody exchange)]
                                    (.write out bytes)))
                                (catch Throwable error
                                  (.printStackTrace error)
                                  (try (.sendResponseHeaders exchange 500 -1)
                                       (catch Throwable _ nil)))
                                (finally (.close exchange))))))
        config (cond-> {:contract-source {}
                        :adapter-module raw
                        :base-url! (constantly (str "http://127.0.0.1:" port))
                        :listener-instance! (constantly listener)
                        :typed-trial {}}
                 retry? (assoc :idempotent-stop-retry? true
                               :listener-shutdown-confirmed? (constantly true)))
        issuer (fn [_ configured]
                 {:start-once! (fn [_] ((:biff.core/start (:adapter-module configured))
                                        {app/biff-ring-handler-key original}))
                  :stop! (fn []
                           (reset! post-stop? true)
                           (reset! stop-while-closing
                                   (mapv (fn [{:keys [path header value]}]
                                           (request! port path header value))
                                         header-probes))
                           {:status :cleanup-unconfirmed :authority :none})
                  :summary! (fn []
                              {:status (if @cleanup-retried? :stopped :cleanup-unconfirmed)
                               :cleanup-retry-policy (if retry? :explicit-host-idempotence :one-shot-only)
                               :cleanup-retry-attempted? @cleanup-retried?
                               :selected-source-fingerprint "same-original-source"
                               :release-authority :none})
                  :assess-current-installation! (fn []
                                                   {:type installed/local-assessment-type
                                                    :status :selected-local-source-and-installation-checks-only
                                                    :release-diagnostic
                                                    {:decisions {:candidate {:status :checks-passed-only}
                                                                 :installation {:status :checks-passed-only}
                                                                 :activation {:status :blocked}}}
                                                    :release-authority :none
                                                    :installation-eligibility :not-issued
                                                    :activation-eligibility :not-issued})
                  :retry-cleanup! (fn []
                                    (reset! retry-while-closed
                                            (mapv (fn [{:keys [path header value]}]
                                                    (request! port path header value))
                                                  header-probes))
                                    (reset! cleanup-retried? true)
                                    {:status :stopped :authority :none})})]
    {:listener listener :port port :issuer issuer :config config :wire captured-wire
     :seen seen :opened opened :stop-while-closing stop-while-closing
     :retry-while-closed retry-while-closed}))

(defn- stop-server! [h]
  (try (.stop ^HttpServer (:listener h) 0) (catch Throwable _ nil)))

(deftest probe-exception-ends-on-first-stop-even-with-live-http
  (let [h (host false)]
    (try
      (with-redefs [installed/issue-selected-installed-catalog! (:issuer h)]
        (let [w (outer/issue-fenced-checked-biff-http! (constantly {}) (:config h) (constantly true))]
          (is (= :one-original-biff-listener-host-locally-fenced
                 (:status ((:start-once! w) {}))))
          (is (= 200 (request! (:port h) "/anything" nil nil)))
          (is (= 200 (request! (:port h) "/anything" "x-gesso-local-liveness-probe" "legit")))
          (is (= :cleanup-unconfirmed (:status ((:stop! w)))))
          (is (= [503 503 503] @(:stop-while-closing h))
              "Probe markers cannot pass during the original physical stop callback")
          (is (= [503 503 503]
                 (mapv (fn [{:keys [path header value]}]
                         (request! (:port h) path header value))
                       header-probes))
              "Surviving listener must deny every marked probe after failed stop")
          (is (= 503 (request! (:port h) "/anything" nil nil)))
          (is (= 2 (count @(:seen h)))
              "Original handler sees only pre-stop normal + marked requests")))
      (finally (stop-server! h)))))

(deftest probe-exception-is-not-restored-by-recovery
  (let [h (host true)]
    (try
      (with-redefs [installed/issue-selected-installed-catalog! (:issuer h)]
        (let [w (outer/issue-fenced-checked-biff-http! (constantly {}) (:config h) (constantly true))]
          (is (= :one-original-biff-listener-host-locally-fenced
                 (:status ((:start-once! w) {}))))
          (is (= :cleanup-unconfirmed (:status ((:stop! w)))))
          (is (= [503 503 503] @(:stop-while-closing h)))
          (is (= :stopped (:status ((:retry-cleanup! w)))))
          (is (= [503 503 503] @(:retry-while-closed h))
              "Retry cannot reopen the private bypass while running callback")
          (is (= [503 503 503]
                 (mapv (fn [{:keys [path header value]}]
                         (request! (:port h) path header value)) header-probes)))
          (is (empty? @(:seen h)))))
      (finally (stop-server! h)))))

(deftest private-probe-already-admitted-must-drain-before-stop-callback
  (let [blocking {:entered (CountDownLatch. 1) :release (CountDownLatch. 1)}
        h (host false blocking)
        dispatched (atom nil)
        stopped (atom nil)]
    (try
      (with-redefs [installed/issue-selected-installed-catalog! (:issuer h)]
        (let [w (outer/issue-fenced-checked-biff-http! (constantly {}) (:config h) (constantly true))]
          ((:start-once! w) {})
          (reset! dispatched (future (@(:wire h) {:uri "/other"
                                                  :request-method :get
                                                  :headers {"x-gesso-local-liveness-probe" "slow"}})))
          (is (.await ^CountDownLatch (:entered blocking) 3 TimeUnit/SECONDS)
              "The original private probe owns its read lease")
          (let [start-stop (CountDownLatch. 1)]
            (reset! stopped (future (.countDown start-stop) ((:stop! w))))
            (is (.await start-stop 3 TimeUnit/SECONDS))
            (Thread/sleep 70)
            (is (not (realized? @stopped))
                "The physical callback must not be entered while a private probe is active")
            (is (empty? @(:stop-while-closing h)))
            (.countDown ^CountDownLatch (:release blocking))
            (is (= 200 (:status (deref @dispatched 3000 :timeout))))
            (is (= :cleanup-unconfirmed (:status (deref @stopped 3000 :timeout))))
            (is (= [503 503 503] @(:stop-while-closing h)))
            (is (= 503 (request! (:port h) "/anything"
                                 "x-gesso-local-liveness-probe" "after-stop"))))))
      (finally
        (.countDown ^CountDownLatch (:release blocking))
        (when @dispatched (deref @dispatched 3000 nil))
        (when @stopped (deref @stopped 3000 nil))
        (stop-server! h)))))

(deftest probe-cannot-promote-its-read-lease-into-exclusive-stop
  (let [issued (atom nil)
        observed (atom nil)
        hooks {:on-original (fn [request]
                              (when (= "attempt-stop" (get-in request
                                                     [:headers "x-gesso-local-liveness-probe"]))
                                (reset! observed
                                        (try ((:stop! @issued)) :unexpected-success
                                             (catch clojure.lang.ExceptionInfo e
                                               (:reason (ex-data e)))))))}
        h (host false hooks)]
    (try
      (with-redefs [installed/issue-selected-installed-catalog! (:issuer h)]
        (let [w (outer/issue-fenced-checked-biff-http! (constantly {}) (:config h) (constantly true))]
          (reset! issued w)
          ((:start-once! w) {})
          (is (= 200 (request! (:port h) "/anything"
                               "x-gesso-local-liveness-probe" "attempt-stop")))
          (is (= :fenced-biff-stop-during-private-probe @observed))
          (is (= :running (:status ((:summary! w)))))
          (is (= 200 (request! (:port h) "/anything" nil nil)))
          (is (= :cleanup-unconfirmed (:status ((:stop! w)))))
          (is (= [503 503 503] @(:stop-while-closing h)))))
      (finally (stop-server! h)))))

(deftest original-biff-xtdb-listener-never-reopens-assurance-probes
  ;; This is a REAL original adapter HTTP listener and XTDB-backed fixture.
  ;; The installed-source control callbacks remain deliberate test doubles:
  ;; original configured source/route qualification is outside this test.
  (fixture/with-real-application-fixture
   (fn []
     (let [s (fixture/scenario :honest)
           raw (:raw s)
           stop-original (:biff.core/stop raw)
           system (atom nil)
           last-stop-probes (atom nil)
           retry-probes (atom nil)
           retry? (atom false)
           selected-original (fn [_] {:status 200 :body "biff original handler"})
           config (assoc (fixture/original-source s true)
                         :listener-instance!
                         (fn [_] (:server @(:server-state s)))
                         :idempotent-stop-retry? true
                         :listener-shutdown-confirmed?
                         (fn [original _]
                           (and (identical? original (:server @(:server-state s)))
                                (.isShutdown (:executor @(:server-state s))))))
           port! #(get @(:server-state s) :port)
           markers! #(mapv (fn [{:keys [path header value]}]
                             (request! (port!) path header value))
                           header-probes)
           fake-issuer
           (fn [_ source]
             {:start-once!
              (fn [initial]
                (let [out ((:biff.core/start (:adapter-module source))
                           (assoc initial app/biff-ring-handler-key
                                  selected-original))]
                  (reset! system out)
                  out))
              :stop!
              (fn []
                (reset! last-stop-probes (markers!))
                {:status :cleanup-unconfirmed :authority :none})
              :retry-cleanup!
              (fn []
                (reset! retry-probes (markers!))
                (stop-original @system)
                (reset! retry? true)
                {:status :stopped :authority :none})
              :summary!
              (fn [] {:status (if @retry? :stopped :cleanup-unconfirmed)
                      :cleanup-retry-policy :explicit-host-idempotence
                      :cleanup-retry-attempted? @retry?
                      :selected-source-fingerprint "one-captured-xtdb-fixture"
                      :release-authority :none})
              :assess-current-installation!
              (fn []
                {:type installed/local-assessment-type
                 :status :selected-local-source-and-installation-checks-only
                 :release-diagnostic
                 {:decisions {:candidate {:status :checks-passed-only}
                              :installation {:status :checks-passed-only}
                              :activation {:status :blocked}}}
                 :release-authority :none
                 :installation-eligibility :not-issued
                 :activation-eligibility :not-issued})})]
       (try
         (with-redefs [installed/issue-selected-installed-catalog! fake-issuer]
           (let [w (outer/issue-fenced-checked-biff-http!
                    (constantly {}) config (constantly true))]
             (is (= :one-original-biff-listener-host-locally-fenced
                    (:status ((:start-once! w) {}))))
             (is (= 200 (request! (port!) "/probe" nil nil)))
             (is (= 200 (request! (port!) "/probe"
                                  "x-gesso-local-liveness-probe" "valid")))
             (is (= :cleanup-unconfirmed (:status ((:stop! w)))))
             (is (= [503 503 503] @last-stop-probes)
                 "The original Biff socket refuses private headers during stop")
             (is (= [503 503 503] (markers!))
                 "A surviving Biff socket cannot bypass its closed fence")
             (is (= 503 (request! (port!) "/probe" nil nil)))
             (is (= :stopped (:status ((:retry-cleanup! w)))))
             (is (= [503 503 503] @retry-probes)
                 "Retry must not reopen the private path")
             (is (= :closed (get-in ((:summary! w)) [:traffic-fence :phase])))
             (is (= :none (:release-authority ((:summary! w)))))
             (is (.isShutdown (:executor @(:server-state s))))))
         (finally
           (try (stop-original (or @system {})) (catch Throwable _ nil))))))))

(deftest private-probe-summary-must-not-deadlock-with-stop-drain
  ;; A private probe may call summary! while it owns its original-handler
  ;; read lease. Stop must publish :stopping without retaining the monitor
  ;; while it waits for that lease. The old ordering (monitor -> write lock)
  ;; deadlocked against the probe's (read lock -> monitor) ordering.
  (let [entered (CountDownLatch. 1)
        release (CountDownLatch. 1)
        probe-summary (promise)
        issued (atom nil)
        hooks {:on-original
               (fn [request]
                 (when (= "summary-race"
                          (get-in request [:headers "x-gesso-local-liveness-probe"]))
                   (.countDown entered)
                   (when-not (.await release 8 TimeUnit/SECONDS)
                     (throw (ex-info "Private probe was never released" {})))
                   (deliver probe-summary (:status ((:summary! @issued))))))}
        h (host false hooks)
        pending-request (atom nil)
        stop-thread (atom nil)]
    (try
      (with-redefs [installed/issue-selected-installed-catalog! (:issuer h)]
        (let [w (outer/issue-fenced-checked-biff-http!
                 (constantly {}) (:config h) (constantly true))
              stop-result (promise)]
          (reset! issued w)
          (is (= :one-original-biff-listener-host-locally-fenced
                 (:status ((:start-once! w) {}))))
          (reset! pending-request
                  (future (request! (:port h) "/anything"
                                    "x-gesso-local-liveness-probe" "summary-race")))
          (is (.await entered 3 TimeUnit/SECONDS)
              "Private probe must enter the original handler before stop")
          (let [t (Thread. (fn []
                             (deliver stop-result
                                      (try ((:stop! w))
                                           (catch Throwable error error))))
                           "gesso-v2293-probe-drain-stop")]
            (.setDaemon t true)
            (reset! stop-thread t)
            (.start t)
            (is (loop [deadline (+ (System/nanoTime) 3000000000)]
                  (cond
                    (and (= Thread$State/WAITING (.getState t))
                         (some #(= "java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock"
                                   (.getClassName ^StackTraceElement %))
                               (.getStackTrace t)))
                    true
                    (< (System/nanoTime) deadline)
                    (do (Thread/sleep 5) (recur deadline))
                    :else false))
                "Stop must be blocked draining the admitted private probe")
            (is (= :stopping
                   (deref (future (:status ((:summary! w)))) 2000 ::timeout))
                "Lifecycle summary must not wait on stop's monitor")
            (is (= :fenced-biff-stop-in-progress
                   (deref (future
                            (try ((:stop! w)) :unexpected-success
                                 (catch clojure.lang.ExceptionInfo error
                                   (:reason (ex-data error)))))
                          2000 ::timeout))
                "Concurrent stops must be rejected while draining")
            (.countDown release)
            (is (= 200 (deref @pending-request 3000 ::timeout))
                "The original HTTP probe can finish after shutdown starts")
            (is (= :stopping (deref probe-summary 3000 ::timeout))
                "The admitted probe must obtain summary under its read lease")
            (is (= :cleanup-unconfirmed
                   (:status (deref stop-result 3000 ::timeout)))
                "Managed stop resumes after the read lease is released")
            (is (= [503 503 503] @(:stop-while-closing h)))
            (is (= :closed (get-in ((:summary! w)) [:traffic-fence :phase])))
            (is (= 503 (request! (:port h) "/anything"
                                 "x-gesso-local-liveness-probe" "after-stop")))
            (is (= 1 (count @(:seen h)))
                "Only the admitted private probe reached the original handler"))))
      (finally
        (.countDown release)
        (when @pending-request (deref @pending-request 3000 nil))
        (when @stop-thread (.join ^Thread @stop-thread 3000))
        (stop-server! h)))))

(deftest original-startup-probe-can-read-summary-without-lifecycle-deadlock
  ;; The original selected adapter may synchronously probe its own local
  ;; listener before start-once! returns. That probe owns the private read
  ;; lease and calls summary!, requiring the lifecycle monitor. Startup
  ;; must not retain monitor over the adapter callback (v2294).
  ;;
  ;; Cover both an admitted successful installation and a raw adapter that
  ;; fails AFTER the probe: the original startup thread, not a concurrent
  ;; external stop, must close the fence on that failure.
  (doseq [fail-after-probe? [false true]]
    (testing (if fail-after-probe?
               "A probe completes before failure, which retires admission"
               "A probe completes before a successful original startup")
      (let [issued (atom nil)
            summary-observed (promise)
            probe-stop-rejection (promise)
            external-stop-rejection (promise)
            h (host false
                    {:on-original
                     (fn [request]
                       (when (= "starting-summary"
                                (get-in request [:headers "x-gesso-local-liveness-probe"]))
                         ;; Bound the nested lifecycle read so a
                         ;; regression reports a failure instead of leaving
                         ;; this HTTP fixture permanently deadlocked.
                         (deliver summary-observed
                                  (deref (future (:status ((:summary! @issued))))
                                         900 ::summary-timeout))
                         (deliver probe-stop-rejection
                                  (try ((:stop! @issued))
                                       :unexpected-stop-success
                                       (catch clojure.lang.ExceptionInfo error
                                         (:reason (ex-data error)))))))})
            original-start (get-in h [:config :adapter-module :biff.core/start])
            config (assoc-in
                    (:config h) [:adapter-module :biff.core/start]
                    (fn [system]
                      (let [started (original-start system)]
                        ;; The raw adapter has a live socket but the checked
                        ;; installation is not yet admitted. A different
                        ;; thread cannot stop that partially installed host.
                        (deliver external-stop-rejection
                                 (deref
                                  (future
                                    (try ((:stop! @issued))
                                         :unexpected-stop-success
                                         (catch clojure.lang.ExceptionInfo error
                                           (:reason (ex-data error)))))
                                  2000 ::stop-timeout))
                        (when-not (= 200
                                     (request! (:port h) "/starting-probe"
                                               "x-gesso-local-liveness-probe"
                                               "starting-summary"))
                          (throw (ex-info "Selected startup probe did not complete" {})))
                        (when fail-after-probe?
                          (throw (ex-info "Injected failure after startup probe"
                                          {:injected true})))
                        started)))]
        (try
          (with-redefs [installed/issue-selected-installed-catalog! (:issuer h)]
            (let [w (outer/issue-fenced-checked-biff-http!
                     (constantly {}) config (constantly true))
                  _ (reset! issued w)
                  result (try {:value ((:start-once! w) {})}
                              (catch Throwable error {:error error}))]
              (is (= :fenced-biff-stop-in-progress
                     (deref external-stop-rejection 2000 ::missing))
                  "Concurrent external stop is refused while original startup runs")
              (is (= :starting (deref summary-observed 2000 ::missing))
                  "Original listener can query summary! during :starting")
              (is (= :fenced-biff-stop-during-private-probe
                     (deref probe-stop-rejection 2000 ::missing))
                  "Read lease cannot be upgraded into a synchronous stop")
              (is (= 1 (count @(:seen h)))
                  "Only the one admitted original probe reached the handler")
              (is (= :fenced-biff-start-not-once-only
                     (try ((:start-once! w) {})
                          :unexpected-second-start
                          (catch clojure.lang.ExceptionInfo error
                            (:reason (ex-data error)))))
                  "Original source startup remains one-use, even on failure")
              (if fail-after-probe?
                (do
                  (is (= "Injected failure after startup probe"
                         (some-> result :error .getMessage)))
                  (is (= :cleanup-unconfirmed (:status ((:summary! w)))))
                  (is (= :closed
                         (get-in ((:summary! w)) [:traffic-fence :phase])))
                  (is (= [503 503 503] @(:stop-while-closing h))
                      "Failed startup closes all private paths before cleanup")
                  (is (= 503 (request! (:port h) "/anything" nil nil)))
                  (is (= 503 (request! (:port h) "/anything"
                                       "x-gesso-local-liveness-probe" "after-failure"))))
                (do
                  (is (= :one-original-biff-listener-host-locally-fenced
                         (get-in result [:value :status])))
                  (is (= :running (:status ((:summary! w)))))
                  (is (= 200 (request! (:port h) "/anything" nil nil)))
                  (is (= :cleanup-unconfirmed (:status ((:stop! w)))))
                  (is (= [503 503 503] @(:stop-while-closing h)))
                  (is (= :closed
                         (get-in ((:summary! w)) [:traffic-fence :phase])))))
              (is (= :none (:release-authority ((:summary! w)))))))
          (finally (stop-server! h)))))))


(deftest ordinary-ring-request-summary-must-not-deadlock-with-stop-drain
  ;; This is a NORMAL protected Ring request, not an original assurance
  ;; probe. It owns the traffic fence's read lease while its handler runs.
  ;; v2296 removed stop's outer monitor hold, but v2297 also needed to
  ;; move the fence snapshot OUTSIDE summary!'s outer monitor hold.
  ;; A third-party summary can otherwise hold monitor while waiting
  ;; behind a fair Ring fence writer, preventing the admitted handler
  ;; from obtaining monitor and releasing its own read lease.
  (let [entered (CountDownLatch. 1)
        release (CountDownLatch. 1)
        observed-summary (promise)
        issued (atom nil)
        h (host false
                {:on-original
                 (fn [request]
                   (when (= "/ordinary-summary-drain" (:uri request))
                     (.countDown entered)
                     (when-not (.await release 8 TimeUnit/SECONDS)
                       (throw (ex-info "Ordinary request was not released" {})))
                     ;; Must execute on the admitted HTTP handler's thread,
                     ;; not a detached future that bypasses its read lease.
                     (deliver observed-summary
                              (:status ((:summary! @issued))))))})
        pending-request (atom nil)
        stop-thread (atom nil)]
    (try
      (with-redefs [installed/issue-selected-installed-catalog! (:issuer h)]
        (let [w (outer/issue-fenced-checked-biff-http!
                 (constantly {}) (:config h) (constantly true))
              stop-result (promise)]
          (reset! issued w)
          (is (= :one-original-biff-listener-host-locally-fenced
                 (:status ((:start-once! w) {}))))
          (reset! pending-request
                  (future (request! (:port h) "/ordinary-summary-drain" nil nil)))
          (is (.await entered 3 TimeUnit/SECONDS)
              "Ordinary HTTP request is admitted before shutdown starts")
          (let [t (Thread. (fn []
                             (deliver stop-result
                                      (try ((:stop! w))
                                           (catch Throwable error error))))
                           "gesso-v2298-ordinary-ring-drain-stop")]
            (.setDaemon t true)
            (reset! stop-thread t)
            (.start t)
            (is (loop [deadline (+ (System/nanoTime) 3000000000)]
                  (cond
                    (and (= Thread$State/WAITING (.getState t))
                         (some #(= "java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock"
                                   (.getClassName ^StackTraceElement %))
                               (.getStackTrace t)))
                    true
                    (< (System/nanoTime) deadline)
                    (do (Thread/sleep 5) (recur deadline))
                    :else false))
                "Shutdown must wait for the admitted ordinary request's read lease")
            ;; A summary from another thread may legitimately queue behind
            ;; the fair fence writer; it must not hold monitor while queued.
            (let [summary-thread (promise)
                  outside-summary (future
                                    (deliver summary-thread
                                             (Thread/currentThread))
                                    ((:summary! w)))
                  ^Thread snapshot-worker (deref summary-thread 2000 nil)]
              (is (instance? Thread snapshot-worker))
              (is (loop [deadline (+ (System/nanoTime) 3000000000)]
                    (cond
                      (and snapshot-worker
                           (= Thread$State/WAITING (.getState snapshot-worker))
                           (some #(= "java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock"
                                     (.getClassName ^StackTraceElement %))
                                 (.getStackTrace snapshot-worker)))
                      true
                      (< (System/nanoTime) deadline)
                      (do (Thread/sleep 5) (recur deadline))
                      :else false))
                  "Third-party summary must be queued behind the fair fence writer before release")
              (is (= :fenced-biff-stop-in-progress
                     (deref (future
                              (try ((:stop! w)) :unexpected-second-stop
                                   (catch clojure.lang.ExceptionInfo error
                                     (:reason (ex-data error)))))
                            2000 ::timeout))
                  "A competing stop cannot begin while the original stop drains")
              (is (not (realized? stop-result))
                  "Physical stop must not run before the ordinary request finishes")
              (.countDown release)
              (is (= 200 (deref @pending-request 4000 ::timeout))
                  "Ordinary HTTP handler returns after it queries summary!")
              (is (= :stopping (deref observed-summary 2000 ::timeout))
                  "The admitted handler reads the lifecycle state without deadlocking")
              (is (= :cleanup-unconfirmed
                     (:status (deref stop-result 4000 ::timeout)))
                  "Managed cleanup proceeds after the ordinary Ring request drains")
              (is (contains? #{:stopping :cleanup-unconfirmed}
                             (:status (deref outside-summary 4000 ::timeout)))
                  "A queued diagnostic snapshot finishes without blocking cleanup")
              (is (= :closed
                     (get-in (deref outside-summary 4000 ::timeout)
                             [:traffic-fence :phase]))))
            (is (= [503 503 503] @(:stop-while-closing h))
                "Original managed stop observes closed private and normal admission")
            (is (= :closed
                   (get-in ((:summary! w)) [:traffic-fence :phase])))
            (is (= :cleanup-unconfirmed (:status ((:summary! w)))))
            (is (= 503 (request! (:port h) "/ordinary-summary-drain" nil nil))
                "The surviving physical listener cannot admit another ordinary request")
            (is (= 503 (request! (:port h) "/anything"
                                 "x-gesso-local-liveness-probe" "after-stop"))
                "Original private markers cannot bypass terminal traffic fencing")
            (is (= 1 (count @(:seen h)))
                "Only the original ordinary request reached the business handler")
            (is (= :none (:release-authority ((:summary! w))))))))
      (finally
        (.countDown release)
        (when @pending-request (deref @pending-request 4000 nil))
        (when @stop-thread (.join ^Thread @stop-thread 4000))
        (stop-server! h)))))


(deftest managed-retry-callback-can-observe-lifecycle-summary
  ;; The managed installed-source retry executes while the outer service
  ;; holds its exclusive :retrying-cleanup lifecycle claim. Its physical
  ;; callback may synchronously obtain a summary from a DIFFERENT thread.
  ;; Before v2299, the outer service held monitor over the callback, so
  ;; the callback's nested summary could not finish until the callback
  ;; itself had returned. This test uses a bounded wait to expose that
  ;; inversion without hanging the JVM on regression.
  (let [entered (CountDownLatch. 1)
        release (CountDownLatch. 1)
        callback-summary (promise)
        issued (atom nil)
        h (host true)
        retry-worker (atom nil)
        selected-issuer
        (fn [catalog configured]
          (let [original ((:issuer h) catalog configured)
                original-retry (:retry-cleanup! original)]
            (assoc original :retry-cleanup!
                   (fn []
                     (.countDown entered)
                     (deliver callback-summary
                              (deref (future ((:summary! @issued)))
                                     2500 ::summary-timeout))
                     (when-not (.await release 8 TimeUnit/SECONDS)
                       (throw (ex-info "Managed retry callback was not released" {})))
                     (original-retry)))))]
    (try
      (with-redefs [installed/issue-selected-installed-catalog! selected-issuer]
        (let [w (outer/issue-fenced-checked-biff-http!
                 (constantly {}) (:config h) (constantly true))]
          (reset! issued w)
          (is (= :one-original-biff-listener-host-locally-fenced
                 (:status ((:start-once! w) {}))))
          (is (= :cleanup-unconfirmed (:status ((:stop! w)))))
          (is (= :original-managed-stop-not-confirmed
                 (get-in ((:summary! w)) [:stop-result :reason]))
              "Selected physical stop remains unconfirmed before retry")
          (is (= :closed (get-in ((:summary! w)) [:traffic-fence :phase])))
          (reset! retry-worker (future ((:retry-cleanup! w))))
          (is (.await entered 3 TimeUnit/SECONDS)
              "Selected original managed-retry callback must be entered")
          (let [snapshot (deref callback-summary 4000 ::missing)]
            (is (map? snapshot)
                "Original managed-retry callback must obtain the outer summary")
            (is (= :retrying-cleanup (:status snapshot)))
            (is (true? (:cleanup-retry-attempted? snapshot)))
            (is (= :closed (get-in snapshot [:traffic-fence :phase])))
            (is (= :none (:release-authority snapshot))))
          (is (= :fenced-biff-cleanup-retry-already-attempted
                 (deref (future
                          (try ((:retry-cleanup! w)) :unexpected-second-retry
                               (catch clojure.lang.ExceptionInfo error
                                 (:reason (ex-data error)))))
                        2500 ::retry-timeout))
              "One-shot retry must reject a competing retry during callback")
          (is (= :fenced-biff-stop-in-progress
                 (deref (future
                          (try ((:stop! w)) :unexpected-stop
                               (catch clojure.lang.ExceptionInfo error
                                 (:reason (ex-data error)))))
                        2500 ::stop-timeout))
              "Stop must remain excluded while original recovery runs")
          (is (not (realized? @retry-worker)))
          (is (= 503 (request! (:port h) "/anything" nil nil)))
          (is (= 503 (request! (:port h) "/anything"
                               "x-gesso-local-liveness-probe" "forged")))
          (.countDown release)
          (is (= :stopped (:status (deref @retry-worker 4000 ::retry-timeout))))
          (is (= [503 503 503] @(:retry-while-closed h))
              "Physical callback must not reopen original or private traffic")
          (is (= :stopped (:status ((:summary! w)))))
          (is (= :closed (get-in ((:summary! w)) [:traffic-fence :phase])))
          (is (= :none (:release-authority ((:summary! w)))))
          (is (= :fenced-biff-cleanup-retry-already-attempted
                 (try ((:retry-cleanup! w)) :unexpected-late-retry
                      (catch clojure.lang.ExceptionInfo error
                        (:reason (ex-data error))))))))
      (finally
        (.countDown release)
        (when @retry-worker (deref @retry-worker 4000 nil))
        (stop-server! h)))))

(defn- protected-stop-rejection [action]
  (try
    (action)
    :unexpected-stop-success
    (catch clojure.lang.ExceptionInfo error
      (:reason (ex-data error)))))

(deftest ordinary-and-nested-protected-handlers-cannot-consume-stop
  ;; v2301: A normal Ring handler cannot upgrade its own fence read lease to
  ;; exclusive shutdown. Even though the lower fence rejects that upgrade,
  ;; the outer issuer must refuse it BEFORE consuming the one-shot lifecycle.
  ;; Exercise a direct handler and a same-thread nested wire dispatch, where
  ;; the inner request must restore the parent's protected-thread context.
  (let [issued (atom nil)
        host-ref (atom nil)
        observed (atom [])
        hooks {:on-original
               (fn [request]
                 (case (:uri request)
                   "/try-self-stop"
                   (swap! observed conj
                          [:direct (protected-stop-rejection
                                    #((:stop! @issued)))])
                   "/outer-request"
                   (do
                     (let [response (@(:wire @host-ref)
                                     {:uri "/nested-request"
                                      :request-method :get
                                      :headers {}})]
                       (swap! observed conj [:inner-http (:status response)]))
                     (swap! observed conj
                            [:outer (protected-stop-rejection
                                     #((:stop! @issued)))]))
                   "/nested-request"
                   (swap! observed conj
                          [:inner (protected-stop-rejection
                                   #((:stop! @issued)))])
                   nil))}
        h (host false hooks)]
    (reset! host-ref h)
    (try
      (with-redefs [installed/issue-selected-installed-catalog! (:issuer h)]
        (let [w (outer/issue-fenced-checked-biff-http!
                 (constantly {}) (:config h) (constantly true))]
          (reset! issued w)
          (is (= :one-original-biff-listener-host-locally-fenced
                 (:status ((:start-once! w) {}))))
          (is (= 200 (request! (:port h) "/try-self-stop" nil nil)))
          (is (= 200 (request! (:port h) "/outer-request" nil nil)))
          (is (= [[:direct :fenced-biff-stop-during-protected-request]
                  [:inner :fenced-biff-stop-during-protected-request]
                  [:inner-http 200]
                  [:outer :fenced-biff-stop-during-protected-request]]
                 @observed))
          (is (= :running (:status ((:summary! w)))))
          (is (= 200 (request! (:port h) "/after-self-stop" nil nil))
              "Rejected in-request stops must not interrupt later traffic")
          (is (= :none (:release-authority ((:summary! w)))))
          (is (= :cleanup-unconfirmed (:status ((:stop! w))))
              "A subsequent external stop still has the original authority")
          (is (= :closed (get-in ((:summary! w)) [:traffic-fence :phase])))
          (is (= [503 503 503] @(:stop-while-closing h)))
          (is (= 503 (request! (:port h) "/after-shutdown" nil nil)))
          (is (= :fenced-biff-start-not-once-only
                 (protected-stop-rejection #((:start-once! w) {}))))))
      (finally (stop-server! h)))))

(deftest ordinary-protected-guard-cannot-consume-stop
  ;; The guard executes under the SAME lexical wire-dispatch context as the
  ;; business handler. A stop from this guard must not poison the lifecycle
  ;; even when the guard would otherwise allow the request through.
  (let [issued (atom nil)
        guard-observations (atom [])
        h (host false)]
    (try
      (with-redefs [installed/issue-selected-installed-catalog! (:issuer h)]
        (let [w (outer/issue-fenced-checked-biff-http!
                 (constantly {}) (:config h)
                 (fn [request]
                   (when (= "/guard-stop" (:uri request))
                     (swap! guard-observations conj
                            (protected-stop-rejection #((:stop! @issued)))))
                   true))]
          (reset! issued w)
          (is (= :one-original-biff-listener-host-locally-fenced
                 (:status ((:start-once! w) {}))))
          (is (= 200 (request! (:port h) "/guard-stop" nil nil)))
          (is (= [:fenced-biff-stop-during-protected-request]
                 @guard-observations))
          (is (= :running (:status ((:summary! w)))))
          (is (= 200 (request! (:port h) "/guard-allowed-later" nil nil)))
          (is (= :cleanup-unconfirmed (:status ((:stop! w)))))
          (is (= :closed (get-in ((:summary! w)) [:traffic-fence :phase])))
          (is (= 503 (request! (:port h) "/guard-after-stop" nil nil)))
          (is (= :none (:release-authority ((:summary! w)))))))
      (finally (stop-server! h)))))
