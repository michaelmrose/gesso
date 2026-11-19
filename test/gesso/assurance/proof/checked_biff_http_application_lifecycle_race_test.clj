(ns gesso.assurance.proof.checked-biff-http-application-lifecycle-race-test
  "v1814: Real JDK listener replacement, inspection, managed stop and startup
   publication races for one issued Biff HTTP application. Identity claims
   remain local and conditional on trusted listener instrumentation."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.string :as str]
            [gesso.assurance.proof.checked-biff-http-application-configuration :as config]
            [gesso.assurance.proof.checked-biff-http-application-configuration-fixture
             :refer [model scenario original-source reason cleanup!
                     with-real-application-fixture]])
  (:import [com.sun.net.httpserver HttpHandler HttpServer]
           [java.net BindException InetSocketAddress]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent Executors]))

(use-fixtures :once with-real-application-fixture)

;; v1807: A second, genuinely bound JDK HttpServer replaces the first on the
;; SAME loopback port. Reusing its checked Ring wrapper is intentional: a
;; fresh private HTTP nonce alone cannot identify the physical server object.
(defn- bind-replacement-on-port! [port checked-ring-handler]
  (let [address (InetSocketAddress. "127.0.0.1" (int port))
        server (loop [attempt 0]
                 (let [outcome (try
                                 {:server (HttpServer/create address 0)}
                                 (catch BindException ex {:error ex}))]
                   (if-let [bound (:server outcome)]
                     bound
                     (if (< attempt 9)
                       (do (Thread/sleep 30) (recur (inc attempt)))
                       (throw (:error outcome))))))
        executor (Executors/newSingleThreadExecutor)
        hits (atom 0)]
    (try
      (.createContext server "/"
        (reify HttpHandler
          (handle [_ exchange]
            (swap! hits inc)
            (try
              (let [method (keyword (str/lower-case (.getRequestMethod exchange)))
                    uri (.getRequestURI exchange)
                    headers (into {}
                                  (for [[k v] (.entrySet (.getRequestHeaders exchange))]
                                    [(str/lower-case k) (first v)]))
                    response (checked-ring-handler
                              {:request-method method
                               :uri (.getPath uri)
                               :headers headers
                               :biff.xtdb/node ((model 'database))})
                    bytes (.getBytes (str (:body response))
                                     StandardCharsets/UTF_8)]
                (.sendResponseHeaders exchange
                                      (int (or (:status response) 500))
                                      (alength bytes))
                (with-open [out (.getResponseBody exchange)]
                  (.write out bytes)))
              (catch Throwable _
                (try (.sendResponseHeaders exchange 500 (long -1))
                     (catch Throwable _ nil)))
              (finally (.close exchange))))))
      (.setExecutor server executor)
      (.start server)
      {:server server :executor executor
       :port (.getPort (.getAddress server)) :hits hits}
      (catch Throwable failure
        (try (.stop server 0) (catch Throwable _ nil))
        (.shutdownNow executor)
        (throw failure)))))

(defn- replace-actual-listener-at-same-port! [scenario system]
  (let [original @(:server-state scenario)
        {:keys [server port]} original]
    (when-not (and (some? server) (integer? port)
                   (fn? (:biff.ring/handler system)))
      (throw (ex-info "Original listener fixture missing"
                      {:reason :missing-original-http-listener})))
    ;; Intentionally bypass managed stop; its historical adapter report
    ;; remains :verified-selected-adapter-start until managed shutdown.
    ((get-in scenario [:raw :biff.core/stop]) system)
    (let [replacement
          (bind-replacement-on-port! port (:biff.ring/handler system))]
      (reset! (:server-state scenario) replacement)
      {:old original :new replacement})))

(deftest real-same-port-server-replacement-invalidates-trusted-generation
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (assoc (original-source s true)
                       :listener-instance!
                       (fn [_] (:server @(:server-state s)))))]
    (try
      (let [{:keys [system instance-generation]} ((:start-once! issued) {})
            inspect! (fn [] ((:inspect-generation! issued)
                             instance-generation system))
            old-server (:server @(:server-state s))
            old-port (:port @(:server-state s))]
        (is (= :matches-original-host-reported-object
               (:physical-listener-instance (inspect!))))
        (let [{:keys [new]} (replace-actual-listener-at-same-port! s system)]
          (is (= old-port (:port new)))
          (is (not (identical? old-server (:server new))))
          (is (not (.isShutdown (:executor new))))
          (is (= :verified-configured-start
                 (:status ((:summary! issued)))))
          (is (= :configured-physical-listener-not-current
                 (reason inspect!)))
          (is (= :configured-physical-listener-not-current
                 (reason #((:inspect-running! issued) system))))
          (is (zero? @(:hits new))
              "Exact-object mismatch rejected before any liveness request"))
        (is (= :stopped (:status ((:stop! issued)))))
        (is (= :configured-instance-generation-not-current
               (reason inspect!))))
      (finally (cleanup! s issued)))))

(deftest real-same-port-replacement-exposes-uninstrumented-identity-limit
  ;; Without :listener-instance!, the second physical server can answer the
  ;; original wrapper's fresh one-use HTTP challenge on the original port.
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (original-source s true))]
    (try
      (let [{:keys [system instance-generation]} ((:start-once! issued) {})
            inspect! (fn [] ((:inspect-generation! issued)
                             instance-generation system))
            old-server (:server @(:server-state s))
            old-port (:port @(:server-state s))]
        (is (= :observed-at-current-inspection
               (:fresh-local-http-response (inspect!))))
        (let [{:keys [new]} (replace-actual-listener-at-same-port! s system)
              before @(:hits new)
              observed (inspect!)]
          (is (= old-port (:port new)))
          (is (not (identical? old-server (:server new))))
          (is (< before @(:hits new))
              "A real fresh challenge reached the replacement HttpServer")
          (is (= :observed-at-current-inspection
                 (:fresh-local-http-response observed)))
          (is (= :matches-startup-verified-loopback-port
                 (:original-local-listener-endpoint observed)))
          (is (= :not-established (:physical-listener-instance observed)))
          (is (= :not-established (:physical-instance-provenance observed))))
        (is (= :stopped (:status ((:stop! issued)))))
        (is (= :configured-instance-generation-not-current
               (reason inspect!))))
      (finally (cleanup! s issued)))))

;; v1812: Share deterministic pauses across four listener/stop races while
;; retaining the original checked HTTP challenge after the release signal.
(defn- paused-generation-inspection [s trusted-listener-identity?]
  (let [source (original-source s true)
        original-locator (:base-url! source)
        inspection-in-locator (promise)
        release-inspection (promise)
        inspection-armed? (atom false)
        instrumented (cond-> source
                       trusted-listener-identity?
                       (assoc :listener-instance!
                              (fn [_] (:server @(:server-state s)))))
        source-with-pause (assoc instrumented :base-url!
                                 (fn [system]
                                   (when @inspection-armed?
                                     (deliver inspection-in-locator true)
                                     (when-not (deref release-inspection 15000 false)
                                       (throw (ex-info "Inspection fixture timed out" {}))))
                                   (original-locator system)))]
    {:issued (config/issue-configured-checked-http-application! source-with-pause)
     :inspection-in-locator inspection-in-locator
     :release-inspection release-inspection
     :inspection-armed? inspection-armed?}))

(deftest same-port-swap-during-inspection-rejects-even-after-fresh-http-response
  ;; The first source-trusted listener identity check happens BEFORE the
  ;; liveness locator is called. Suspend there, replace the actual HttpServer
  ;; with another one on the SAME port and same final Ring handler, and let
  ;; the second physical server answer the fresh private HTTP challenge.
  ;; The final listener identity check must reject that apparent success.
  ;; This is an in-process inspection TOCTOU test, not a claim about remote
  ;; deployments or a continuously authenticated HTTP connection.
  (let [s (scenario :honest)
        {:keys [issued inspection-in-locator release-inspection inspection-armed?]}
        (paused-generation-inspection s true)]
    (try
      (let [{:keys [system instance-generation]}
            ((:start-once! issued) {})
            inspect! (fn [] ((:inspect-generation! issued)
                             instance-generation system))
            original-server (:server @(:server-state s))
            original-port (:port @(:server-state s))]
        ;; Establish the ordinary case before arming the deterministic race.
        (is (= :matches-original-host-reported-object
               (:physical-listener-instance (inspect!))))
        (reset! inspection-armed? true)
        (let [inspecting (future (reason inspect!))]
          (try
            (is (true? (deref inspection-in-locator 10000 false)))
            (is (= :still-inspecting
                   (deref inspecting 200 :still-inspecting)))
            (let [{:keys [new]} (replace-actual-listener-at-same-port!
                                 s system)]
              (is (= original-port (:port new)))
              (is (not (identical? original-server (:server new))))
              (is (= :verified-configured-start
                     (:status ((:summary! issued)))))
              (deliver release-inspection true)
              (is (= :configured-physical-listener-not-current
                     (deref inspecting 15000 :timeout)))
              (is (pos? @(:hits new))
                  "Fresh HTTP challenge reached the physically new server")
              (is (= :verified-configured-start
                     (:status ((:summary! issued))))))
            (finally
              (deliver release-inspection true)
              (deref inspecting 15000 :timeout))))
        (is (= :stopped (:status ((:stop! issued)))))
        (is (= :configured-instance-generation-not-current
               (reason inspect!))))
      (finally
        (deliver release-inspection true)
        (cleanup! s issued)))))

(deftest managed-stop-waits-for-fresh-generation-inspection-to-complete
  ;; Complement the v1801 stop-first race: a real fresh HTTP inspection that
  ;; already owns the managed lifecycle monitor must finish BEFORE a managed
  ;; stop can begin cleanup. The successful point-in-time inspection must not
  ;; become a lease or allow its generation to be reused after that stop.
  (let [s (scenario :honest)
        {:keys [issued inspection-in-locator release-inspection inspection-armed?]}
        (paused-generation-inspection s true)]
    (try
      (let [{:keys [system instance-generation]}
            ((:start-once! issued) {})
            inspect! (fn [] ((:inspect-generation! issued)
                             instance-generation system))]
        (is (= :current-at-inspection (:status (inspect!))))
        (reset! inspection-armed? true)
        (let [inspecting (future (inspect!))]
          (try
            (is (true? (deref inspection-in-locator 10000 false)))
            (let [stop-entered (promise)
                  stopping (future
                             (deliver stop-entered true)
                             ((:stop! issued)))]
              (try
                (is (true? (deref stop-entered 5000 false)))
                ;; The inspector is blocked in the URL locator while holding
                ;; the monitor. Managed stop must wait, not close the listener.
                (is (= :inspection-still-pending
                       (deref inspecting 200 :inspection-still-pending)))
                (is (= :stop-still-pending
                       (deref stopping 200 :stop-still-pending)))
                (is (= :verified-configured-start
                       (:status ((:summary! issued)))))
                (is (not (.isShutdown (:executor @(:server-state s)))))
                (deliver release-inspection true)
                (let [observed (deref inspecting 15000 :inspection-timed-out)
                      stopped (deref stopping 15000 :stop-timed-out)]
                  (is (= :current-at-inspection (:status observed)))
                  (is (= :observed-at-current-inspection
                         (:fresh-local-http-response observed)))
                  (is (= :matches-original-host-reported-object
                         (:physical-listener-instance observed)))
                  (is (= :stopped (:status stopped))))
                (is (= :configured-instance-generation-not-current
                       (reason inspect!)))
                (finally
                  (deliver release-inspection true)
                  (deref stopping 15000 :stop-timed-out))))
            (finally
              (deliver release-inspection true)
              (deref inspecting 15000 :inspection-timed-out)))))
      (finally
        (deliver release-inspection true)
        (cleanup! s issued)))))

(deftest failed-inflight-inspection-releases-managed-stop
  ;; Complement v1809's successful inspection: a REAL listener disappears
  ;; out of band while the current-generation inspection owns the managed
  ;; lifecycle monitor. The pending managed stop must not pass the inspection,
  ;; and it must still finish after the failed fresh HTTP challenge. Never
  ;; turn the historical successful-start report into present-tense liveness.
  (let [s (scenario :honest)
        {:keys [issued inspection-in-locator release-inspection inspection-armed?]}
        (paused-generation-inspection s true)]
    (try
      (let [{:keys [system instance-generation]} ((:start-once! issued) {})
            inspect! (fn [] ((:inspect-generation! issued)
                             instance-generation system))]
        (is (= :current-at-inspection (:status (inspect!))))
        (reset! inspection-armed? true)
        (let [inspecting (future (reason inspect!))]
          (try
            (is (true? (deref inspection-in-locator 10000 false)))
            (let [stop-entered (promise)
                  stopping (future
                             (deliver stop-entered true)
                             ((:stop! issued)))]
              (try
                (is (true? (deref stop-entered 5000 false)))
                (is (= :inspection-pending
                       (deref inspecting 200 :inspection-pending)))
                (is (= :stop-pending
                       (deref stopping 200 :stop-pending)))
                ;; This physical stop deliberately bypasses managed lifecycle
                ;; synchronization; the managed status remains historical.
                ((get-in s [:raw :biff.core/stop]) system)
                (is (.isShutdown (:executor @(:server-state s))))
                (is (= :verified-configured-start
                       (:status ((:summary! issued)))))
                (deliver release-inspection true)
                (is (= :configured-instance-http-liveness-not-observed
                       (deref inspecting 15000 :inspection-timed-out)))
                (is (= :stopped
                       (:status (deref stopping 15000 :stop-timed-out))))
                (is (= :configured-instance-generation-not-current
                       (reason inspect!)))
                (finally
                  (deliver release-inspection true)
                  (deref stopping 15000 :stop-timed-out))))
            (finally
              (deliver release-inspection true)
              (deref inspecting 15000 :inspection-timed-out)))))
      (finally
        (deliver release-inspection true)
        (cleanup! s issued)))))

(deftest uninstrumented-inflight-listener-replacement-remains-unproven
  ;; Complement v1808: without a trusted :listener-instance! callback,
  ;; the same-port replacement is NOT identified by the original probe's
  ;; fresh nonce. A successful HTTP response must retain explicitly unknown
  ;; physical identity, even though an independent actor replaced the real
  ;; JDK HttpServer while this inspection was waiting for its URL locator.
  (let [s (scenario :honest)
        {:keys [issued inspection-in-locator release-inspection inspection-armed?]}
        (paused-generation-inspection s false)]
    (try
      (let [{:keys [system instance-generation]} ((:start-once! issued) {})
            inspect! (fn [] ((:inspect-generation! issued)
                             instance-generation system))
            original-server (:server @(:server-state s))
            original-port (:port @(:server-state s))]
        (is (= :not-established (:physical-listener-instance (inspect!))))
        (reset! inspection-armed? true)
        (let [inspecting (future (inspect!))]
          (try
            (is (true? (deref inspection-in-locator 10000 false)))
            (is (= :inspection-pending
                   (deref inspecting 200 :inspection-pending)))
            (let [{:keys [new]} (replace-actual-listener-at-same-port! s system)
                  before @(:hits new)]
              (is (= original-port (:port new)))
              (is (not (identical? original-server (:server new))))
              (is (= :verified-configured-start
                     (:status ((:summary! issued)))))
              (deliver release-inspection true)
              (let [observed (deref inspecting 15000 :inspection-timed-out)]
                (is (= :current-at-inspection (:status observed)))
                (is (< before @(:hits new))
                    "A replacement physical server answered the fresh challenge")
                (is (= :observed-at-current-inspection
                       (:fresh-local-http-response observed)))
                (is (= :matches-startup-verified-loopback-port
                       (:original-local-listener-endpoint observed)))
                (is (= :not-established (:physical-listener-instance observed)))
                (is (= :not-established (:physical-instance-provenance observed)))
                (is (= :not-established (:future-local-listener-availability observed)))
                (is (= :not-issued (:certificate observed)))
                (is (= :not-admitted (:admission observed)))
                (is (= :none (:authority observed)))))
            (finally
              (deliver release-inspection true)
              (deref inspecting 15000 :inspection-timed-out))))
        (is (= :stopped (:status ((:stop! issued)))))
        (is (= :configured-instance-generation-not-current
               (reason inspect!))))
      (finally
        (deliver release-inspection true)
        (cleanup! s issued)))))

(deftest startup-publication-cannot-race-managed-stop-on-unset-system
  ;; Suspend the ORIGINAL successful :starting -> :verified transition just
  ;; AFTER the real Clojure CAS succeeds. In the former implementation, the
  ;; selected routes and @started were still nil, allowing concurrent stop to
  ;; claim cleanup without stopping the actual Biff listener. The publication
  ;; monitor must keep that stop waiting until every started handle is saved.
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (original-source s true))
        original-cas clojure.core/compare-and-set!
        published (promise)
        release-publish (promise)]
    (try
      (with-redefs [clojure.core/compare-and-set!
                    (fn [atm before after]
                      (let [changed? (original-cas atm before after)]
                        (when (and changed?
                                   (= :starting before)
                                   (= :verified-configured-start after))
                          (deliver published true)
                          (when-not (deref release-publish 15000 false)
                            (throw (ex-info "Startup publication stalled" {}))))
                        changed?))]
        (let [starting (future ((:start-once! issued) {}))]
          (try
            (is (true? (deref published 15000 false)))
            (let [stop-entered (promise)
                  stopping (future
                             (deliver stop-entered true)
                             ((:stop! issued)))]
              (try
                (is (true? (deref stop-entered 5000 false)))
                (is (= :stop-pending (deref stopping 250 :stop-pending))
                    "Managed stop must not consume half-published startup")
                (deliver release-publish true)
                (let [started-result (deref starting 15000 :startup-timed-out)
                      stop-result (deref stopping 15000 :stop-timed-out)]
                  (is (= :verified-configured-start
                         (get-in started-result [:report :status])))
                  (is (some? (:system started-result)))
                  (is (= :stopped (:status stop-result)))
                  (is (.isShutdown (:executor @(:server-state s))))
                  (is (= :configured-instance-generation-not-current
                         (reason #((:inspect-generation! issued)
                                   (:instance-generation started-result)
                                   (:system started-result))))))
                (finally
                  (deliver release-publish true)
                  (deref stopping 15000 :stop-timed-out))))
            (finally
              (deliver release-publish true)
              (deref starting 15000 :startup-timed-out)))))
      (finally
        (deliver release-publish true)
        (cleanup! s issued)))))
