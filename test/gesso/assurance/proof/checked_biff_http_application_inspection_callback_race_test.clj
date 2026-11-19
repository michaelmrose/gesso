(ns gesso.assurance.proof.checked-biff-http-application-inspection-callback-race-test
  "v1827: Callback-driven inspection races against the original Biff HTTP
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
