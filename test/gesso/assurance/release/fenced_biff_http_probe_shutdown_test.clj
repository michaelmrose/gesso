(ns gesso.assurance.release.fenced-biff-http-probe-shutdown-test
  "v2290: Permanent live-HTTP qualification of private-probe admission,
   synchronous probe drain, and fail-closed retries at the selected
   Biff traffic fence. Controlled installed-source callbacks are explicit
   test doubles, not independent source or release authority.

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
