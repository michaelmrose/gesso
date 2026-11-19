(ns gesso.assurance.proof.checked-biff-http-application-lifecycle-race-test
  "v1822: Real JDK listener replacement, inspection, managed stop and startup
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

(deftest concurrent-start-cannot-issue-second-physical-adapter
  ;; A previous test rejected a second start only AFTER the first finished.
  ;; Block the first on its original post-listener loopback URL locator,
  ;; then launch a competing start while the original adapter is still
  ;; :starting. Only the first caller may consume this issued one-shot start.
  ;; The competitor must not create another listener or advance the state.
  (let [s (scenario :honest)
        source (original-source s true)
        original-locator (:base-url! source)
        entered (promise)
        release-first (promise)
        issued (config/issue-configured-checked-http-application!
                (assoc source :base-url!
                       (fn [system]
                         (deliver entered true)
                         (when-not (deref release-first 15000 false)
                           (throw (ex-info "First startup challenge timed out" {})))
                         (original-locator system))))
        first-start (future ((:start-once! issued) {}))]
    (try
      (is (true? (deref entered 15000 false)))
      (let [original-listener (:server @(:server-state s))
            second-start (future
                           (reason #((:start-once! issued) {})))]
        (is (some? original-listener)
            "Original Biff listener is already physically started")
        (is (= :starting (:status ((:summary! issued)))))
        (is (= :configured-start-already-attempted
               (deref second-start 10000 :second-start-timed-out)))
        (is (identical? original-listener
                        (:server @(:server-state s)))
            "Competing caller cannot replace the first listener")
        (is (= :starting (:status ((:summary! issued)))))
        (deliver release-first true)
        (let [{:keys [system instance-generation report] :as result}
              (deref first-start 15000 :first-start-timed-out)]
          (is (map? result))
          (is (= :verified-configured-start (:status report)))
          (is (= :current-at-inspection
                 (:status ((:inspect-generation! issued)
                           instance-generation system))))
          (is (identical? original-listener
                          (:server @(:server-state s)))))
        (is (= :stopped (:status ((:stop! issued)))))
        (is (.isShutdown (:executor @(:server-state s)))))
      (finally
        (deliver release-first true)
        (deref first-start 15000 :first-start-timed-out)
        (cleanup! s issued)))))

(deftest stop-between-one-shot-claim-and-start-gate-prevents-physical-start
  ;; The first start claims its one-shot AtomicBoolean before changing the
  ;; issued state to :starting. Pause at that original state transition and
  ;; let the managed stop close the unstarted issuance. The first caller must
  ;; fail rather than resurrect a stopped application or bind an HTTP socket.
  ;; The consumed one-shot also forbids attempting startup a second time.
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (original-source s true))
        original-cas clojure.core/compare-and-set!
        start-at-gate (promise)
        release-start (promise)]
    (try
      (with-redefs [clojure.core/compare-and-set!
                    (fn [atm before after]
                      (when (and (= :issued before) (= :starting after))
                        (deliver start-at-gate true)
                        (when-not (deref release-start 15000 false)
                          (throw (ex-info "Start gate fixture timed out" {}))))
                      (original-cas atm before after))]
        (let [starting (future (reason #((:start-once! issued) {})))]
          (try
            (is (true? (deref start-at-gate 15000 false)))
            (is (= :issued (:status ((:summary! issued)))))
            (is (nil? @(:server-state s)))
            (is (= :stopped-without-successful-start
                   (:status ((:stop! issued)))))
            (is (= :stopped-without-successful-start
                   (:status ((:summary! issued)))))
            (is (nil? @(:server-state s))
                "Stop won before physical Biff startup")
            (deliver release-start true)
            (is (= :configured-start-stopped-before-use
                   (deref starting 15000 :start-timed-out)))
            (is (= :configured-start-already-attempted
                   (reason #((:start-once! issued) {}))))
            (is (= :stopped-without-successful-start
                   (:status ((:stop! issued)))))
            (is (nil? @(:server-state s))
                "Rejected original/second callers did not open a listener")
            (finally
              (deliver release-start true)
              (deref starting 15000 :start-timed-out)))))
      (finally
        (deliver release-start true)
        (cleanup! s issued)))))

(deftest managed-stop-rejects-during-in-flight-physical-start
  ;; Complement v1816's stop-before-state-transition race. Once the one-shot
  ;; startup has advanced to :starting and the real listener is open, a
  ;; competing managed stop must reject, NOT stop a partly initialized Biff
  ;; application or mark the issuance stopped. After the original HTTP
  ;; challenge finishes, its exact issued generation can be inspected and
  ;; then stopped normally. This is local lifecycle ordering, not a guarantee
  ;; about independent processes or out-of-band listener shutdown.
  (let [s (scenario :honest)
        source (original-source s true)
        original-locator (:base-url! source)
        challenge-entered (promise)
        release-challenge (promise)
        issued (config/issue-configured-checked-http-application!
                (assoc source :base-url!
                       (fn [system]
                         (deliver challenge-entered true)
                         (when-not (deref release-challenge 15000 false)
                           (throw (ex-info "Startup challenge fixture timed out" {})))
                         (original-locator system))))
        starting (future ((:start-once! issued) {}))]
    (try
      (is (true? (deref challenge-entered 15000 false)))
      (let [{:keys [server executor port]} @(:server-state s)
            stopping (future (reason #((:stop! issued))))]
        (is (some? server) "Original physical listener has started")
        (is (pos-int? port))
        (is (not (.isShutdown executor)))
        (is (= :starting (:status ((:summary! issued)))))
        (is (= :configured-start-in-progress
               (deref stopping 10000 :stop-timed-out))
            "Managed stop must reject during incomplete startup")
        (is (identical? server (:server @(:server-state s))))
        (is (not (.isShutdown executor))
            "Rejected managed stop must not close the listener")
        (is (= :starting (:status ((:summary! issued)))))
        (deliver release-challenge true)
        (let [{:keys [system instance-generation report] :as result}
              (deref starting 15000 :start-timed-out)]
          (is (map? result))
          (is (= :verified-configured-start (:status report)))
          (is (= :current-at-inspection
                 (:status ((:inspect-generation! issued)
                           instance-generation system))))
          (is (identical? server (:server @(:server-state s))))
          (is (= :stopped (:status ((:stop! issued)))))
          (is (.isShutdown executor))
          (is (= :configured-instance-generation-not-current
                 (reason #((:inspect-generation! issued)
                           instance-generation system)))))
        (is (= :configured-start-already-attempted
               (reason #((:start-once! issued) {})))))
      (finally
        (deliver release-challenge true)
        (deref starting 15000 :start-timed-out)
        (cleanup! s issued)))))

(deftest rejected-stop-does-not-hide-failed-start-cleanup
  ;; The successful-start race in v1817 is not sufficient to establish the
  ;; failure path. A managed stop must reject a :starting application, while
  ;; the original guarded adapter must still clean its PHYSICAL listener if
  ;; that startup's local HTTP locator subsequently fails. Neither a prior
  ;; stop rejection nor observed physical cleanup permits a successful-start
  ;; report, retry, generation inspection, or release admission.
  (let [s (scenario :honest)
        entered (promise)
        release-start (promise)
        issued (config/issue-configured-checked-http-application!
                (assoc (original-source s true)
                       :base-url!
                       (fn [_]
                         (deliver entered true)
                         (when-not (deref release-start 15000 false)
                           (throw (ex-info "Failed startup fixture timed out"
                                           {:reason :fixture-timeout})))
                         (throw (ex-info "Original startup URL locator failed"
                                         {:reason :fixture-start-locator-failed})))) )
        starting (future (reason #((:start-once! issued) {})))]
    (try
      (is (true? (deref entered 15000 false)))
      (let [{:keys [server executor]} @(:server-state s)]
        (is (some? server) "Original physical listener opened before failure")
        (is (not (.isShutdown executor)))
        (is (= :starting (:status ((:summary! issued)))))
        (is (= :configured-start-in-progress
               (reason #((:stop! issued)))))
        (is (not (.isShutdown executor))
            "Rejected managed stop did not prematurely close the adapter")
        (deliver release-start true)
        (is (= :fixture-start-locator-failed
               (deref starting 15000 :start-timed-out)))
        (is (.isShutdown executor)
            "Original guarded adapter cleans the failed physical listener")
        (is (= :start-failed-physical-state-unknown
               (:status ((:summary! issued)))))
        (is (= :start-failed-physical-state-unknown
               (:status ((:stop! issued)))))
        (is (= :configured-start-already-attempted
               (reason #((:start-once! issued) {}))))
        (is (= :configured-instance-not-current
               (reason #((:inspect-running! issued) {}))))
        (is (= :configured-instance-generation-not-current
               (reason #((:inspect-generation! issued) nil {}))))
        (is (= :start-failed-physical-state-unknown
               (:status ((:summary! issued))))))
      (finally
        (deliver release-start true)
        (deref starting 15000 :start-timed-out)
        (cleanup! s issued)))))

(deftest concurrent-managed-stops-run-physical-cleanup-once
  ;; Two independent callers race a real managed stop while its original
  ;; selected adapter stop callback is suspended. Only one thread may enter
  ;; the physical cleanup path; the second waits on the issued lifecycle
  ;; monitor and subsequently observes the same completed terminal state.
  ;; A running inspection/generation must not survive that terminal state.
  (let [s (scenario :honest)
        original-stop (get-in s [:raw :biff.core/stop])
        physical-stop-calls (atom 0)
        physical-stop-entered (promise)
        release-physical-stop (promise)
        adapter (assoc (:raw s) :biff.core/stop
                       (fn [system]
                         (swap! physical-stop-calls inc)
                         (deliver physical-stop-entered true)
                         (when-not (deref release-physical-stop 15000 false)
                           (throw (ex-info "Physical stop fixture timed out" {})))
                         (original-stop system)))
        issued (config/issue-configured-checked-http-application!
                (assoc (original-source s true) :adapter-module adapter))]
    (try
      (let [{:keys [system instance-generation]} ((:start-once! issued) {})
            executor (:executor @(:server-state s))
            first-stop (future ((:stop! issued)))]
        (try
          (is (true? (deref physical-stop-entered 15000 false)))
          (is (= :stopping (:status ((:summary! issued)))))
          (is (= 1 @physical-stop-calls))
          (is (not (.isShutdown executor)))
          (let [second-entered (promise)
                second-stop (future
                              (deliver second-entered true)
                              ((:stop! issued)))]
            (try
              (is (true? (deref second-entered 5000 false)))
              (is (= :second-stop-pending
                     (deref second-stop 250 :second-stop-pending))
                  "Second managed stop must wait for physical cleanup")
              (is (= 1 @physical-stop-calls)
                  "Second caller cannot enter the physical stop callback")
              (deliver release-physical-stop true)
              (is (= :stopped
                     (:status (deref first-stop 15000 :first-stop-timed-out))))
              (is (= :stopped
                     (:status (deref second-stop 15000 :second-stop-timed-out))))
              (is (= 1 @physical-stop-calls)
                  "Exactly one physical adapter cleanup is attempted")
              (is (.isShutdown executor))
              (is (= :configured-instance-generation-not-current
                     (reason #((:inspect-generation! issued)
                               instance-generation system))))
              (is (= :configured-start-already-attempted
                     (reason #((:start-once! issued) {}))))
              (finally
                (deliver release-physical-stop true)
                (deref second-stop 15000 :second-stop-timed-out))))
          (finally
            (deliver release-physical-stop true)
            (deref first-stop 15000 :first-stop-timed-out))))
      (finally
        (deliver release-physical-stop true)
        (cleanup! s issued)))))

(deftest concurrent-stops-preserve-uncertain-physical-cleanup
  ;; A successful shutdown race is not enough: the original raw adapter may
  ;; close its real listener but THROW while reporting completion. One
  ;; concurrent managed caller must wait for the first, inherit its uncertain
  ;; outcome, and never retry an ambiguous physical stop. The old generation
  ;; remains invalid even though the process-local executor is shut down.
  (let [s (scenario :honest)
        original-stop (get-in s [:raw :biff.core/stop])
        physical-stop-calls (atom 0)
        cleanup-attempted (promise)
        release-cleanup (promise)
        adapter (assoc (:raw s) :biff.core/stop
                       (fn [system]
                         (swap! physical-stop-calls inc)
                         (original-stop system)
                         (deliver cleanup-attempted true)
                         (when-not (deref release-cleanup 15000 false)
                           (throw (ex-info "Stop fixture timed out" {})))
                         (throw (ex-info "Original stop result uncertain" {}))))
        issued (config/issue-configured-checked-http-application!
                (assoc (original-source s true) :adapter-module adapter))]
    (try
      (let [{:keys [system instance-generation]} ((:start-once! issued) {})
            executor (:executor @(:server-state s))
            first-stop (future ((:stop! issued)))]
        (try
          (is (true? (deref cleanup-attempted 15000 false)))
          (is (= :stopping (:status ((:summary! issued)))))
          (is (.isShutdown executor)
              "Physical listener closed before stop reports an error")
          (is (= 1 @physical-stop-calls))
          (let [second-entered (promise)
                second-stop (future
                              (deliver second-entered true)
                              ((:stop! issued)))]
            (try
              (is (true? (deref second-entered 5000 false)))
              (is (= :second-stop-pending
                     (deref second-stop 250 :second-stop-pending)))
              (is (= 1 @physical-stop-calls))
              (deliver release-cleanup true)
              (is (= :stop-cleanup-not-established
                     (:status (deref first-stop 15000 :first-stop-timed-out))))
              (is (= :stop-cleanup-not-established
                     (:status (deref second-stop 15000 :second-stop-timed-out))))
              (is (= 1 @physical-stop-calls)
                  "No second physical stop attempt after ambiguous cleanup")
              (is (= :stop-cleanup-not-established
                     (:status ((:summary! issued)))))
              (is (.isShutdown executor))
              (is (= :configured-instance-generation-not-current
                     (reason #((:inspect-generation! issued)
                               instance-generation system))))
              (is (= :configured-instance-not-current
                     (reason #((:inspect-running! issued) system))))
              (is (= :configured-start-already-attempted
                     (reason #((:start-once! issued) {}))))
              (finally
                (deliver release-cleanup true)
                (deref second-stop 15000 :second-stop-timed-out))))
          (finally
            (deliver release-cleanup true)
            (deref first-stop 15000 :first-stop-timed-out))))
      (finally
        (deliver release-cleanup true)
        (cleanup! s issued)))))

(deftest inspections-wait-for-failed-managed-stop-and-cannot-requalify-instance
  ;; The original lifecycle monitor must serialize both inspection entry
  ;; points behind an already in-flight physical stop. When the selected
  ;; adapter closed its HTTP listener but subsequently reports uncertain
  ;; cleanup, neither an old generation nor an old system handle may be
  ;; requalified after the monitor is released. Fresh HTTP probing must not
  ;; be attempted for the no-longer-managed instance.
  (let [s (scenario :honest)
        original-stop (get-in s [:raw :biff.core/stop])
        cleanup-in-progress (promise)
        release-cleanup (promise)
        stop-calls (atom 0)
        adapter (assoc (:raw s) :biff.core/stop
                       (fn [system]
                         (swap! stop-calls inc)
                         (original-stop system)
                         (deliver cleanup-in-progress true)
                         (when-not (deref release-cleanup 15000 false)
                           (throw (ex-info "Inspection race fixture timed out" {})))
                         (throw (ex-info "Stop reported uncertain completion" {}))))
        issued (config/issue-configured-checked-http-application!
                (assoc (original-source s true) :adapter-module adapter))]
    (try
      (let [{:keys [system instance-generation]} ((:start-once! issued) {})
            executor (:executor @(:server-state s))
            stopping (future ((:stop! issued)))]
        (try
          (is (true? (deref cleanup-in-progress 15000 false)))
          (is (= :stopping (:status ((:summary! issued)))))
          (is (.isShutdown executor)
              "Physical listener has closed while managed stop is pending")
          (let [inspect-generation-entered (promise)
                inspect-running-entered (promise)
                inspecting-generation (future
                                        (deliver inspect-generation-entered true)
                                        (reason #((:inspect-generation! issued)
                                                  instance-generation system)))
                inspecting-running (future
                                     (deliver inspect-running-entered true)
                                     (reason #((:inspect-running! issued) system)))]
            (try
              (is (true? (deref inspect-generation-entered 5000 false)))
              (is (true? (deref inspect-running-entered 5000 false)))
              (is (= :inspection-pending
                     (deref inspecting-generation 250 :inspection-pending))
                  "Generation inspection waits behind managed cleanup")
              (is (= :inspection-pending
                     (deref inspecting-running 250 :inspection-pending))
                  "Current-system inspection also waits behind cleanup")
              (is (= 1 @stop-calls))
              (deliver release-cleanup true)
              (is (= :stop-cleanup-not-established
                     (:status (deref stopping 15000 :stop-timed-out))))
              (is (= :configured-instance-generation-not-current
                     (deref inspecting-generation 15000 :inspection-timed-out)))
              (is (= :configured-instance-not-current
                     (deref inspecting-running 15000 :inspection-timed-out)))
              (is (= 1 @stop-calls)
                  "A failed stop and rejected inspections never retry cleanup")
              (is (= :stop-cleanup-not-established
                     (:status ((:summary! issued)))))
              (is (.isShutdown executor))
              (finally
                (deliver release-cleanup true)
                (deref inspecting-generation 15000 :inspection-timed-out)
                (deref inspecting-running 15000 :inspection-timed-out))))
          (finally
            (deliver release-cleanup true)
            (deref stopping 15000 :stop-timed-out))))
      (finally
        (deliver release-cleanup true)
        (cleanup! s issued)))))

(deftest provisional-biff-system-cannot-pass-running-inspection-before-publication
  ;; A host-owned URL locator may temporarily see a real Biff system after
  ;; the physical listener has started but before the configured startup
  ;; has qualified and published its original issued generation. Merely
  ;; possessing that provisional system must not grant :inspect-running!
  ;; or :inspect-generation! authority, nor issue an extra HTTP challenge.
  ;; After real startup completes, the exact returned system and generation
  ;; can be inspected; a separately created nominal generation still fails.
  (let [s (scenario :honest)
        source (original-source s true)
        original-locator (:base-url! source)
        provisional-system (promise)
        release-startup (promise)
        locator-calls (atom 0)
        issued (config/issue-configured-checked-http-application!
                (assoc source :base-url!
                       (fn [system]
                         (swap! locator-calls inc)
                         (deliver provisional-system system)
                         (when-not (deref release-startup 15000 false)
                           (throw (ex-info "Provisional inspection fixture timed out" {})))
                         (original-locator system))))
        started (future ((:start-once! issued) {}))]
    (try
      (let [candidate (deref provisional-system 15000 :startup-not-at-locator)
            physical (:server @(:server-state s))
            count-before @locator-calls
            synthetic-generation (config/->StartedInstanceGeneration)]
        (is (map? candidate) "URL locator has an actual provisional Biff system")
        (is (fn? (:biff.ring/handler candidate))
            "Biff has already installed its selected final Ring handler")
        (is (some? physical) "Actual physical HTTP listener has started")
        (is (= :starting (:status ((:summary! issued)))))
        (is (= :not-established
               (:real-http-consumption ((:summary! issued)))))
        (is (= :configured-instance-not-current
               (reason #((:inspect-running! issued) candidate))))
        (is (= :configured-instance-generation-not-current
               (reason #((:inspect-generation! issued)
                         synthetic-generation candidate))))
        (is (= count-before @locator-calls)
            "Rejected provisional inspections did not request fresh HTTP")
        (is (identical? physical (:server @(:server-state s))))
        (deliver release-startup true)
        (let [{:keys [system instance-generation report] :as result}
              (deref started 15000 :startup-timed-out)]
          (is (map? result))
          (is (= :verified-configured-start (:status report)))
          (is (= :current-at-inspection
                 (:status ((:inspect-running! issued) system))))
          (is (= :current-at-inspection
                 (:status ((:inspect-generation! issued)
                           instance-generation system))))
          (is (not (identical? synthetic-generation instance-generation)))
          (is (= :configured-instance-generation-not-current
                 (reason #((:inspect-generation! issued)
                           synthetic-generation system))))
          (is (= :stopped (:status ((:stop! issued)))))
          (is (.isShutdown (:executor @(:server-state s))))))
      (finally
        (deliver release-startup true)
        (deref started 15000 :startup-timed-out)
        (cleanup! s issued)))))
