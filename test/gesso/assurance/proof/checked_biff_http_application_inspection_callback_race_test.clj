(ns gesso.assurance.proof.checked-biff-http-application-inspection-callback-race-test
  "v2287: Real Biff HTTP listener lifecycle and callback races, including
   failure-preserving stop, bounded original managed cleanup retry, and
   physically verified traffic-fence exclusion. Original source-trusted
   listener callbacks are still explicit premises, not production release
   authority or independent deployment certification."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.checked-biff-http-application-configuration :as config]
            [gesso.assurance.release.fenced-biff-http :as f]
            [gesso.assurance.release.installed-source :as installed]
            [gesso.assurance.proof.checked-biff-http-consumption-probe :as probe]
            [gesso.assurance.proof.checked-biff-http-application-configuration-fixture
             :as fixture
             :refer [scenario original-source reason cleanup!
                     with-real-application-fixture]])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]
           [java.time Duration]))

(use-fixtures :once with-real-application-fixture)

(deftest failed-inspection-releases-waiting-fresh-inspection
  ;; One original current-generation inspection can fail because its
  ;; application-owned HTTP locator fails. A second original current-system
  ;; inspection, already waiting on the managed lifecycle monitor, must not
  ;; inherit that failed observation or pretend it is a fresh HTTP proof.
  ;; The running application is unchanged; the second check must independently
  ;; contact the original real local listener once the monitor is released.
  (let [s (scenario :honest)
        source (original-source s true)
        original-locator (:base-url! source)
        first-at-locator (promise)
        release-first (promise)
        inspect-armed? (atom false)
        locator-calls (atom 0)
        issued (config/issue-configured-checked-http-application!
                (assoc source
                       :listener-instance!
                       (fn [_] (:server @(:server-state s)))
                       :base-url!
                       (fn [system]
                         (when @inspect-armed?
                           (when (= 1 (swap! locator-calls inc))
                             (deliver first-at-locator true)
                             (when-not (deref release-first 15000 false)
                               (throw (ex-info "Locator fixture timed out" {})))
                             (throw (ex-info "First inspection locator failed" {}))))
                         (original-locator system))))]
    (try
      (let [{:keys [system instance-generation]} ((:start-once! issued) {})
            listener (:server @(:server-state s))]
        (is (some? listener))
        (reset! inspect-armed? true)
        (let [first (future (reason #((:inspect-generation! issued)
                                       instance-generation system)))]
          (try
            (is (true? (deref first-at-locator 10000 false)))
            (is (= 1 @locator-calls))
            (is (= :first-inspection-pending
                   (deref first 200 :first-inspection-pending)))
            (let [second-entered (promise)
                  second (future
                           (deliver second-entered true)
                           ((:inspect-running! issued) system))]
              (try
                (is (true? (deref second-entered 5000 false)))
                (is (= :second-inspection-pending
                       (deref second 250 :second-inspection-pending)))
                (is (= 1 @locator-calls)
                    "Waiting inspection cannot bypass the lifecycle monitor")
                (deliver release-first true)
                (is (= :configured-instance-http-liveness-not-observed
                       (deref first 15000 :first-inspection-timed-out)))
                (let [observed (deref second 15000 :second-inspection-timed-out)]
                  (is (= :current-at-inspection (:status observed)))
                  (is (= :observed-at-current-inspection
                         (:fresh-local-http-response observed)))
                  (is (= :matches-original-host-reported-object
                         (:physical-listener-instance observed)))
                  (is (= :not-issued (:certificate observed)))
                  (is (= :not-admitted (:admission observed)))
                  (is (= :none (:authority observed))))
                (is (= 2 @locator-calls)
                    "Second inspection performed a new HTTP challenge")
                (is (= :verified-configured-start
                       (:status ((:summary! issued)))))
                (is (identical? listener (:server @(:server-state s))))
                (finally
                  (deliver release-first true)
                  (deref second 15000 :second-inspection-timed-out))))
            (finally
              (deliver release-first true)
              (deref first 15000 :first-inspection-timed-out))))
        (is (= :current-at-inspection
               (:status ((:inspect-generation! issued)
                         instance-generation system))))
        (is (= 3 @locator-calls)
            "The issued generation remains eligible for a fresh reinspection")
        (is (= :stopped (:status ((:stop! issued)))))
        (is (.isShutdown (:executor @(:server-state s))))
        (is (= :configured-instance-generation-not-current
               (reason #((:inspect-generation! issued)
                         instance-generation system)))))
      (finally
        (deliver release-first true)
        (cleanup! s issued)))))

(deftest post-challenge-listener-identity-callback-failure-rejects-inspection
  ;; A fresh HTTP challenge only establishes that the original issued
  ;; wrapper responded at the captured local endpoint. Even after that
  ;; response succeeds, failure of the trusted physical-instance callback
  ;; at the SECOND identity check must not be promoted to a current-instance
  ;; observation. A later healthy callback may qualify a NEW inspection,
  ;; but cannot retroactively qualify the rejected one or issue authority.
  (let [s (scenario :honest)
        source (original-source s true)
        original-locator (:base-url! source)
        reject-postcheck? (atom false)
        locator-called? (atom false)
        identity-reads (atom 0)
        locator-reads (atom 0)
        issued (config/issue-configured-checked-http-application!
                (assoc source
                       :listener-instance!
                       (fn [_]
                         (swap! identity-reads inc)
                         (when (and @reject-postcheck? @locator-called?)
                           (throw (ex-info "Trusted listener lookup failed"
                                           {:reason :source-callback-failed})))
                         (:server @(:server-state s)))
                       :base-url!
                       (fn [system]
                         (swap! locator-reads inc)
                         (when @reject-postcheck?
                           (reset! locator-called? true))
                         (original-locator system))))]
    (try
      (let [{:keys [system instance-generation]} ((:start-once! issued) {})
            inspect! (fn [] ((:inspect-generation! issued)
                             instance-generation system))
            server (:server @(:server-state s))
            before-reads @identity-reads
            before-locator-reads @locator-reads]
        (is (some? server))
        (is (pos? before-reads)
            "Startup captured actual source-reported listener identity")
        (reset! reject-postcheck? true)
        (is (= :configured-physical-listener-not-current (reason inspect!)))
        (is (true? @locator-called?)
            "First physical identity check passed before the URL lookup")
        (is (= (inc before-locator-reads) @locator-reads)
            "First inspection requested its own original HTTP challenge")
        (is (= (+ before-reads 2) @identity-reads)
            "Source callback was read both before and after the challenge")
        (is (= :verified-configured-start
               (:status ((:summary! issued))))
            "Historical startup status is not a fresh liveness claim")
        (reset! reject-postcheck? false)
        (reset! locator-called? false)
        (let [observed (inspect!)]
          (is (= :current-at-inspection (:status observed)))
          (is (= :observed-at-current-inspection
                 (:fresh-local-http-response observed)))
          (is (= :matches-original-host-reported-object
                 (:physical-listener-instance observed)))
          (is (= :trusted-host-callback-conditional
                 (:physical-instance-provenance observed)))
          (is (= :not-issued (:certificate observed)))
          (is (= :not-admitted (:admission observed)))
          (is (= :none (:authority observed))))
        (is (= (+ before-locator-reads 2) @locator-reads)
            "Recovered inspection requested another independent challenge")
        (is (identical? server (:server @(:server-state s))))
        (is (= :stopped (:status ((:stop! issued)))))
        (is (= :configured-instance-generation-not-current (reason inspect!))))
      (finally
        (cleanup! s issued)))))

(deftest reentrant-managed-stop-in-listener-callback-cannot-return-current-report
  ;; The monitor is reentrant: a host callback may stop the real listener
  ;; after HTTP succeeds. Its stale object must not yield a current report.
  (let [s (scenario :honest)
        source (original-source s true)
        original-url (:base-url! source)
        issued-holder (atom nil)
        stop-during-postcheck? (atom false)
        passed-locator? (atom false)
        source-identity-reads (atom 0)
        nested-stops (atom 0)
        issued (config/issue-configured-checked-http-application!
                (assoc source
                       :listener-instance!
                       (fn [_]
                         (swap! source-identity-reads inc)
                         (when (and @stop-during-postcheck? @passed-locator?)
                           (swap! nested-stops inc)
                           (reset! stop-during-postcheck? false)
                           (let [stopped ((:stop! @issued-holder))]
                             (when-not (= :stopped (:status stopped))
                               (throw (ex-info "Managed stop was not completed"
                                               {:result stopped})))))
                         (:server @(:server-state s)))
                       :base-url!
                       (fn [system]
                         (when @stop-during-postcheck?
                           (reset! passed-locator? true))
                         (original-url system))))]
    (reset! issued-holder issued)
    (try
      (let [{:keys [system instance-generation]} ((:start-once! issued) {})
            inspect! (fn [] ((:inspect-generation! issued)
                             instance-generation system))
            original-server (:server @(:server-state s))
            reads-before @source-identity-reads]
        (is (some? original-server))
        (is (pos? reads-before) "Startup captured trusted listener identity")
        (reset! stop-during-postcheck? true)
        (is (= :configured-instance-not-current (reason inspect!))
            "Reentrant managed stop invalidates inspection after challenge")
        (is (true? @passed-locator?)
            "Source locator executed between the two identity reads")
        (is (= (+ reads-before 2) @source-identity-reads)
            "Both physical listener identity checks occurred")
        (is (= 1 @nested-stops)
            "The post-challenge callback stopped the original application")
        (is (= :stopped (:status ((:summary! issued)))))
        (is (.isShutdown (:executor @(:server-state s)))
            "The real HTTP listener closed in the reentrant stop")
        (is (identical? original-server (:server @(:server-state s)))
            "The source still returns the same, now stopped, server object")
        (is (= :configured-instance-generation-not-current
               (reason inspect!)))
        (is (= :configured-instance-not-current
               (reason #((:inspect-running! issued) system))))
        (is (= :configured-start-already-attempted
               (reason #((:start-once! issued) {}))))
        (is (= 1 @nested-stops)
            "Rejected subsequent checks must not run source callbacks"))
      (finally
        (cleanup! s issued)))))

(deftest reentrant-url-locator-cannot-recursively-inspect-running-instance
  ;; The source-owned URL locator runs during the outer generation inspection.
  ;; It may attempt to inspect that very application again on the same JVM
  ;; thread, where Java's lifecycle monitor is reentrant. The nested attempt
  ;; must reject before issuing its own HTTP challenge. The original outer
  ;; inspection can still succeed after the callback recovers normally.
  (let [s (scenario :honest)
        source (original-source s true)
        original-url (:base-url! source)
        issued-holder (atom nil)
        nested-on-next-inspection? (atom false)
        locator-calls (atom 0)
        nested-reasons (atom [])
        issued (config/issue-configured-checked-http-application!
                (assoc source :base-url!
                       (fn [system]
                         (swap! locator-calls inc)
                         (when (compare-and-set! nested-on-next-inspection?
                                                 true false)
                           (swap! nested-reasons conj
                                  (reason #((:inspect-running! @issued-holder)
                                            system))))
                         (original-url system))))]
    (reset! issued-holder issued)
    (try
      (let [{:keys [system instance-generation]} ((:start-once! issued) {})
            starting-urls @locator-calls
            inspect! (fn [] ((:inspect-generation! issued)
                             instance-generation system))]
        (is (pos? starting-urls))
        (reset! nested-on-next-inspection? true)
        (let [observation (inspect!)]
          (is (= :current-at-inspection (:status observation)))
          (is (= :observed-at-current-inspection
                 (:fresh-local-http-response observation)))
          (is (= :not-issued (:certificate observation)))
          (is (= :not-admitted (:admission observation)))
          (is (= :none (:authority observation))))
        (is (= [:configured-instance-inspection-reentrant] @nested-reasons)
            "Callback cannot recursively create a second inspection")
        (is (= (inc starting-urls) @locator-calls)
            "Nested inspection did not reach the original URL locator")
        (is (= :current-at-inspection (:status (inspect!)))
            "A later independent inspection is not blocked")
        (is (= (+ starting-urls 2) @locator-calls))
        (is (= :stopped (:status ((:stop! issued)))))
        (is (.isShutdown (:executor @(:server-state s)))))
      (finally
        (cleanup! s issued)))))

(deftest reentrant-listener-callback-cannot-recursively-inspect-generation
  ;; The optional source-trusted listener callback is also invoked under the
  ;; same monitor and must not initiate a second generation inspection while
  ;; the outer current-system inspection is unfinished. Object identity is
  ;; still checked before and after the one fresh HTTP challenge.
  (let [s (scenario :honest)
        source (original-source s true)
        issued-holder (atom nil)
        nested-on-next-inspection? (atom false)
        listener-reads (atom 0)
        nested-reasons (atom [])
        issued (config/issue-configured-checked-http-application!
                (assoc source :listener-instance!
                       (fn [system]
                         (swap! listener-reads inc)
                         (when (compare-and-set! nested-on-next-inspection?
                                                 true false)
                           (swap! nested-reasons conj
                                  (reason #((:inspect-generation! @issued-holder)
                                            (:instance-generation @issued-holder)
                                            system))))
                         (:server @(:server-state s)))))]
    (reset! issued-holder issued)
    (try
      (let [{:keys [system instance-generation]} ((:start-once! issued) {})
            startup-reads @listener-reads
            observed-generation (atom instance-generation)]
        ;; Publish only the exact issued token into a test-local holder. Never
        ;; derive or invent a generation from an HTTP status report.
        (swap! issued-holder assoc :instance-generation @observed-generation)
        (is (pos? startup-reads))
        (reset! nested-on-next-inspection? true)
        (let [observation ((:inspect-running! issued) system)]
          (is (= :current-at-inspection (:status observation)))
          (is (= :matches-original-host-reported-object
                 (:physical-listener-instance observation)))
          (is (= :trusted-host-callback-conditional
                 (:physical-instance-provenance observation))))
        (is (= [:configured-instance-inspection-reentrant] @nested-reasons)
            "Trusted listener callback cannot recursively inspect generation")
        (is (= (+ startup-reads 2) @listener-reads)
            "Only the outer inspection performs the two identity checks")
        (is (= :current-at-inspection
               (:status ((:inspect-generation! issued)
                         instance-generation system)))
            "In-progress guard is released after the outer inspection")
        (is (= :stopped (:status ((:stop! issued)))))
        (is (.isShutdown (:executor @(:server-state s)))))
      (finally
        (cleanup! s issued)))))

(deftest post-challenge-boundary-var-substitution-rejects-current-instance
  ;; An app-owned callback invoked at the final listener identity check can
  ;; replace an original proof function Var AFTER the entry-time pinned! check
  ;; and the real HTTP challenge. Even though that original physical listener
  ;; still responds, the current-instance report must reject the changed
  ;; source boundary rather than silently accept success from a stale pin.
  ;; The original root is restored before any further inspection or cleanup.
  (let [s (scenario :honest)
        source (original-source s true)
        original-url (:base-url! source)
        original-live-root (var-get #'probe/require-fresh-local-http-liveness!)
        mutate-postcheck? (atom false)
        locator-completed? (atom false)
        substitutions (atom 0)
        locator-calls (atom 0)
        issued (config/issue-configured-checked-http-application!
                (assoc source
                       :listener-instance!
                       (fn [_]
                         (when (and @mutate-postcheck? @locator-completed?)
                           (reset! mutate-postcheck? false)
                           (swap! substitutions inc)
                           (alter-var-root
                            #'probe/require-fresh-local-http-liveness!
                            (constantly (fn [& _]
                                          (throw (ex-info "Substituted original liveness"
                                                          {:reason :fixture-substitution}))))))
                         (:server @(:server-state s)))
                       :base-url!
                       (fn [system]
                         (swap! locator-calls inc)
                         (when @mutate-postcheck?
                           (reset! locator-completed? true))
                         (original-url system))))]
    (try
      (let [{:keys [system instance-generation]}
            ((:start-once! issued) {})
            inspect! (fn [] ((:inspect-generation! issued)
                             instance-generation system))
            initial-locator-count @locator-calls
            original-server (:server @(:server-state s))]
        (is (some? original-server))
        (reset! mutate-postcheck? true)
        (is (= :substituted-original-configured-start-boundary
               (reason inspect!))
            "A post-challenge original Var swap must invalidate inspection")
        (is (true? @locator-completed?)
            "Actual source-owned HTTP target lookup occurred before the swap")
        (is (= 1 @substitutions)
            "The final listener callback performed exactly one substitution")
        (is (= (inc initial-locator-count) @locator-calls)
            "The failed inspection issued its original HTTP challenge")
        (is (not (identical? original-live-root
                             (var-get #'probe/require-fresh-local-http-liveness!))))
        (is (identical? original-server (:server @(:server-state s))))
        (alter-var-root #'probe/require-fresh-local-http-liveness!
                        (constantly original-live-root))
        (is (= :verified-configured-start
               (:status ((:summary! issued))))
            "Historical startup status is independent of failed inspection")
        (let [observed (inspect!)]
          (is (= :current-at-inspection (:status observed)))
          (is (= :observed-at-current-inspection
                 (:fresh-local-http-response observed)))
          (is (= :not-issued (:certificate observed)))
          (is (= :not-admitted (:admission observed)))
          (is (= :none (:authority observed))))
        (is (= (+ initial-locator-count 2) @locator-calls)
            "Recovered inspection must conduct its own fresh HTTP check")
        (is (= :stopped (:status ((:stop! issued)))))
        (is (.isShutdown (:executor @(:server-state s)))))
      (finally
        (alter-var-root #'probe/require-fresh-local-http-liveness!
                        (constantly original-live-root))
        (cleanup! s issued)))))

;; v2287: Physical cleanup and traffic-fence recovery qualification is kept
;; here so the exact-head Typed Clojure real Biff/XTDB CI gate runs it.
(defn- http-status [s method path]
  (let [port (:port @(:server-state s))
        req (-> (HttpRequest/newBuilder
                 (URI/create (str "http://127.0.0.1:" port path)))
                (.timeout (Duration/ofSeconds 5))
                (.method method (HttpRequest$BodyPublishers/noBody))
                (.build))]
    (.statusCode (.send (HttpClient/newHttpClient) req
                        (HttpResponse$BodyHandlers/ofString)))))

(defn mock-reason [g]
  (try (g) :no-rejection
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
(def base
  {:contract-source {} :adapter-module {:biff.core/id :v2286/raw
                                        :biff.core/start identity
                                        :biff.core/stop (fn [_] true)}
   :typed-trial {} :base-url! (fn [_] "http://127.0.0.1:1")
   :listener-instance! (fn [_] (Object.))})
(defn make-wrapper [options]
  (let [commands (atom [])
        state (atom :issued)
        ret (atom nil)
        fake {:stop! (fn []
                       (swap! commands conj :stop)
                       (reset! state :cleanup-unconfirmed)
                       {:status :cleanup-unconfirmed :authority :none})
              :retry-cleanup! (fn []
                                (swap! commands conj :retry)
                                (when-let [before (:before options)]
                                  (before @ret))
                                (case (:outcome options :success)
                                  :throws (throw (ex-info "stop failed" {}))
                                  :forged {:status :stopped :authority :none}
                                  :bad-authority (do (reset! state :stopped)
                                                     {:status :stopped :authority :unexpected})
                                  :bad-status (do (reset! state :bad)
                                                  {:status :stopped :authority :none})
                                  :success (do (reset! state :stopped)
                                               {:status :stopped :authority :none})))
              :summary! (fn [] {:status @state :authority :none
                                 :release-authority :none
                                 :cleanup-retry-policy
                                 (if (:opt-in? options) :explicit-host-idempotence :one-shot-only)
                                 :cleanup-retry-attempted? (boolean (= :stopped @state))
                                 :selected-source-fingerprint "source-v2286"})}
        config (cond-> base
                 (:opt-in? options) (assoc :idempotent-stop-retry? true
                                           :listener-shutdown-confirmed? (fn [_ _] true)))
        wrapper (with-redefs [installed/issue-selected-installed-catalog!
                              (fn [_ candidate]
                                (is (= config (assoc candidate :adapter-module (:adapter-module config))))
                                fake)]
                  (f/issue-fenced-checked-biff-http!
                   (constantly {}) config (constantly true)))]
    (reset! ret wrapper)
    {:wrapper wrapper :calls commands :state state}))
(deftest valid-opt-in-and-invalid-declarations
  (let [issued (make-wrapper {:opt-in? true})]
    (is (= :explicit-host-idempotence (:cleanup-retry-policy ((:summary! (:wrapper issued)))))))
  (doseq [extra [{:idempotent-stop-retry? true}
                 {:idempotent-stop-retry? false :listener-shutdown-confirmed? (fn [_ _] true)}
                 {:idempotent-stop-retry? true :listener-shutdown-confirmed? false}
                 {:listener-shutdown-confirmed? (fn [_ _] true)}]]
    (is (= :invalid-original-fenced-biff-input
           (mock-reason #(f/issue-fenced-checked-biff-http!
                     (constantly {}) (merge base extra) (constantly true)))))))
(deftest explicit-success-preserves-closed-fence
  (let [{:keys [wrapper calls]} (make-wrapper {:opt-in? true})]
    (is (= :fenced-biff-managed-cleanup-not-retryable (mock-reason #((:retry-cleanup! wrapper)))))
    (is (= :cleanup-unconfirmed (:status ((:stop! wrapper)))))
    (is (= :closed (get-in ((:summary! wrapper)) [:traffic-fence :phase])))
    (is (= :original-managed-stop-not-confirmed (get-in ((:summary! wrapper)) [:stop-result :reason])))
    (is (= [:stop] @calls))
    (is (= :stopped (:status ((:retry-cleanup! wrapper)))))
    (is (= :stopped (:status ((:summary! wrapper)))))
    (is (= :closed (get-in ((:summary! wrapper)) [:traffic-fence :phase])))
    (is (= :none (:release-authority ((:summary! wrapper)))))
    (is (true? (:cleanup-retry-attempted? ((:summary! wrapper)))))
    (is (= [:stop :retry] @calls))
    (is (= :fenced-biff-cleanup-retry-already-attempted (mock-reason #((:retry-cleanup! wrapper)))))
    (is (= :stopped (:status ((:stop! wrapper)))))
    (is (= [:stop :retry] @calls))
    (is (= :fenced-biff-start-not-once-only (mock-reason #((:start-once! wrapper) {}))))))
(deftest failure-and-forgeries-cannot-report-stopped
  (doseq [outcome [:throws :forged :bad-authority :bad-status]]
    (let [{:keys [wrapper calls]} (make-wrapper {:opt-in? true :outcome outcome})]
      (is (= :cleanup-unconfirmed (:status ((:stop! wrapper)))))
      (is (= :cleanup-unconfirmed (:status ((:retry-cleanup! wrapper)))) (str outcome))
      (is (= :cleanup-unconfirmed (:status ((:summary! wrapper)))) (str outcome))
      (is (= :closed (get-in ((:summary! wrapper)) [:traffic-fence :phase])) (str outcome))
      (is (= [:stop :retry] @calls))
      (is (= :fenced-biff-cleanup-retry-already-attempted (mock-reason #((:retry-cleanup! wrapper)))))
      (is (= :fenced-biff-start-not-once-only (mock-reason #((:start-once! wrapper) {})))))))
(deftest no-default-retry-and-no-downgrade
  (let [{:keys [wrapper calls]} (make-wrapper {:opt-in? false})]
    (is (= :one-shot-only (:cleanup-retry-policy ((:summary! wrapper)))))
    (is (= :cleanup-unconfirmed (:status ((:stop! wrapper)))))
    (is (= :fenced-biff-cleanup-retry-not-enabled (mock-reason #((:retry-cleanup! wrapper)))))
    (is (= [:stop] @calls))))
(deftest reentrant-retry-and-stop-rejected
  (let [observed (atom [])
        {:keys [wrapper calls]} (make-wrapper
                                 {:opt-in? true
                                  :before (fn [issuer]
                                            (swap! observed conj
                                                   (mock-reason (fn [] ((:stop! issuer))))
                                                   (mock-reason (fn [] ((:retry-cleanup! issuer))))))})]
    (is (= :cleanup-unconfirmed (:status ((:stop! wrapper)))))
    (is (= :stopped (:status ((:retry-cleanup! wrapper)))))
    (is (= [:fenced-biff-stop-in-progress
            :fenced-biff-cleanup-retry-already-attempted] @observed))
    (is (= [:stop :retry] @calls))))

(defn stopped? [s]
  (boolean (and @(:server-state s)
                (.isShutdown ^java.util.concurrent.ExecutorService
                             (:executor @(:server-state s))))))
(defn physical-reason [g]
  (try (g) :no-error (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
(defn- physical-setup [observer]
  (let [s (fixture/scenario :honest)
        attempts (atom 0)
        physical (:biff.core/stop (:raw s))
        raw (assoc (:raw s) :biff.core/stop
                   (fn [sys]
                     (if (= 1 (swap! attempts inc))
                       (throw (ex-info "First physical stop fails" {}))
                       (physical sys))))
        source (assoc (fixture/original-source s true)
                      :adapter-module raw
                      :listener-instance! (fn [_] (:server @(:server-state s)))
                      :idempotent-stop-retry? true
                      :listener-shutdown-confirmed?
                      (fn [instance _]
                        (and (identical? instance (:server @(:server-state s)))
                             (stopped? s)
                             (observer s))))
        wrapper (f/issue-fenced-checked-biff-http!
                 (constantly (get-in s [:env :catalog-source :catalog]))
                 source (constantly true))]
    {:s s :attempts attempts :wrapper wrapper :physical physical}))
(defn- physical-clean [{:keys [s wrapper physical]}]
  (try ((:stop! wrapper)) (catch Throwable _ nil))
  (try (physical {}) (catch Throwable _ nil)))

(deftest real-biff-http-listener-retry-recovery-and-observer-refusal
  (do
    (testing "The first real Biff stop fails: the socket remains live but the closed fence denies protected traffic"
       (let [{:keys [s attempts wrapper] :as ctx} (physical-setup (constantly true))]
         (try
           (is (= :one-original-biff-listener-host-locally-fenced
                  (:status ((:start-once! wrapper) {}))))
           (is (not (stopped? s)))
           (is (= :active (get-in ((:summary! wrapper)) [:traffic-fence :phase])))
           (is (= 404 (http-status s "GET" "/not-a-route")))
           (let [failed ((:stop! wrapper))]
             (is (= :cleanup-unconfirmed (:status failed)))
             (is (= :original-managed-stop-not-confirmed (:reason failed))))
           (is (= 1 @attempts))
           (is (not (stopped? s)))
           (is (= :closed (get-in ((:summary! wrapper)) [:traffic-fence :phase])))
           (is (= 503 (http-status s "GET" "/not-a-route"))
               "Physical listener still responds, but closed fence refuses business")
           (is (= :fenced-biff-start-not-once-only
                  (physical-reason #((:start-once! wrapper) {}))))
           (is (= :stopped (:status ((:retry-cleanup! wrapper)))))
           (is (= 2 @attempts))
           (is (stopped? s))
           (is (= :stopped (:status ((:summary! wrapper)))))
           (is (= :closed (get-in ((:summary! wrapper)) [:traffic-fence :phase])))
           (is (= :none (:release-authority ((:summary! wrapper)))))
           (is (= :not-issued (:completion-claim ((:summary! wrapper)))))
           (is (= :fenced-biff-cleanup-retry-already-attempted
                  (physical-reason #((:retry-cleanup! wrapper)))))
           (is (= :stopped (:status ((:stop! wrapper)))))
           (is (= 2 @attempts))
           (finally (physical-clean ctx)))))
     (testing "A false independent listener observer retains uncertainty after physical stop"
       (let [{:keys [s attempts wrapper] :as ctx} (physical-setup (constantly false))]
         (try
           (is (= :one-original-biff-listener-host-locally-fenced
                  (:status ((:start-once! wrapper) {}))))
           (is (= :cleanup-unconfirmed (:status ((:stop! wrapper)))))
           (is (= 1 @attempts))
           (is (= :closed (get-in ((:summary! wrapper)) [:traffic-fence :phase])))
           (is (= 503 (http-status s "GET" "/not-a-route")))
           (is (= :cleanup-unconfirmed (:status ((:retry-cleanup! wrapper)))))
           (is (= 2 @attempts))
           (is (stopped? s))
           (is (= :cleanup-unconfirmed (:status ((:summary! wrapper)))))
           (is (= :closed (get-in ((:summary! wrapper)) [:traffic-fence :phase])))
           (is (= :none (:release-authority ((:summary! wrapper)))))
           (is (= :not-issued (:completion-claim ((:summary! wrapper)))))
           (is (= :fenced-biff-cleanup-retry-already-attempted
                  (physical-reason #((:retry-cleanup! wrapper)))))
           (is (= :fenced-biff-start-not-once-only
                  (physical-reason #((:start-once! wrapper) {}))))
           (finally (physical-clean ctx)))))))
