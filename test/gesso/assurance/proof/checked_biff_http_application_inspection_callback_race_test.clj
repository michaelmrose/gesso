(ns gesso.assurance.proof.checked-biff-http-application-inspection-callback-race-test
  "v1828: Callback-driven inspection races against the original Biff HTTP
   listener: failed URL lookup, second identity check, reentrant managed stop.
   These are in-process observations, never deployment certification."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.checked-biff-http-application-configuration :as config]
            [gesso.assurance.proof.checked-biff-http-application-configuration-fixture
             :refer [scenario original-source reason cleanup!
                     with-real-application-fixture]]))

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
