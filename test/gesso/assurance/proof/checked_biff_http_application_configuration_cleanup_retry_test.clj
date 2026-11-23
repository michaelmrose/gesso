(ns gesso.assurance.proof.checked-biff-http-application-configuration-cleanup-retry-test
  "v2313: Permanent real-Biff/XTDB cleanup-retry regressions for the original
   configured issuer. Successful adapter stop callbacks alone are never
   treated as confirmed original-listener shutdown. The selected original
   host listener observer may request lifecycle inspection from another JVM
   thread during :retrying-cleanup without deadlocking physical recovery."
  (:import [java.util.concurrent CountDownLatch TimeUnit])
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.checked-biff-http-application-configuration
             :as config]
            [gesso.assurance.proof.checked-biff-http-application-configuration-fixture
             :refer [scenario original-source reason cleanup!
                     with-real-application-fixture]]))

(use-fixtures :once with-real-application-fixture)

(defn- listener [s]
  (:server @(:server-state s)))

(defn- stopped? [s]
  (boolean (and @(:server-state s)
                (.isShutdown ^java.util.concurrent.ExecutorService
                             (:executor @(:server-state s))))))

(defn- retry-source
  [s adapter observer]
  (assoc (original-source s true)
         :adapter-module adapter
         :idempotent-stop-retry? true
         :listener-instance! (fn [_] (listener s))
         :listener-shutdown-confirmed? observer))

(defn- fail-first-stop
  [s calls]
  (let [physical-stop (get-in s [:raw :biff.core/stop])]
    (assoc (:raw s)
           :biff.core/stop
           (fn [system]
             (if (= 1 (swap! calls inc))
               (throw (ex-info "Injected first physical stop failure" {}))
               (physical-stop system))))))

(deftest retry-opt-in-requires-a-complete-explicit-source-contract
  (let [s (scenario :honest)
        source (original-source s true)
        issue! config/issue-configured-checked-http-application!]
    (doseq [invalid [(assoc source :idempotent-stop-retry? true)
                     (assoc source :idempotent-stop-retry? false)
                     (assoc source :idempotent-stop-retry? nil)
                     (assoc source :listener-shutdown-confirmed? (fn [_ _] true))
                     (assoc source :idempotent-stop-retry? true
                                   :listener-instance! (fn [_] (listener s)))
                     (assoc source :idempotent-stop-retry? true
                                   :listener-instance! :not-a-callback
                                   :listener-shutdown-confirmed? (fn [_ _] true))
                     (assoc source :idempotent-stop-retry? true
                                   :listener-instance! (fn [_] (listener s))
                                   :listener-shutdown-confirmed? nil)
                     (with-meta
                       (retry-source s (:raw s) (fn [_ _] true))
                       {:forged :retry-policy})]]
      (is (= :invalid-application-owned-http-configuration
             (reason #(issue! invalid)))
          "No incomplete or metadata-bearing policy may issue a retry"))
    (is (nil? @(:server-state s))
        "Invalid retry declarations must not start an HTTP listener")))

(deftest default-configured-issuer-never-replays-failed-stop
  (let [s (scenario :honest)
        attempts (atom 0)
        adapter (fail-first-stop s attempts)
        issued (config/issue-configured-checked-http-application!
                (assoc (original-source s true) :adapter-module adapter))]
    (try
      (let [{:keys [system]} ((:start-once! issued) {})]
        (is (some? system))
        (is (= :stop-cleanup-not-established
               (:status ((:stop! issued)))))
        (is (= 1 @attempts))
        (is (not (stopped? s)) "Failed first stop left the real listener alive")
        (is (= :configured-cleanup-retry-not-enabled
               (reason (:retry-cleanup! issued))))
        (is (= :stop-cleanup-not-established
               (:status ((:stop! issued)))))
        (is (= 1 @attempts)
            "Default one-shot behavior is preserved across repeated stop"))
      (finally (cleanup! s issued)))))

(deftest explicit-retry-verifies-the-original-real-listener-after-stop
  (let [s (scenario :honest)
        attempts (atom 0)
        observations (atom [])
        issued (config/issue-configured-checked-http-application!
                (retry-source
                 s (fail-first-stop s attempts)
                 (fn [captured system]
                   (swap! observations conj {:listener captured :system system
                                             :shutdown? (stopped? s)})
                   (and (identical? captured (listener s))
                        (stopped? s)))))]
    (try
      (let [{:keys [system instance-generation]}
            ((:start-once! issued) {})
            original (listener s)]
        (is (some? original))
        (is (= :current-at-inspection
               (:status ((:inspect-generation! issued)
                         instance-generation system))))
        (is (= :stop-cleanup-not-established
               (:status ((:stop! issued)))))
        (is (= 1 @attempts))
        (is (not (stopped? s)))
        (is (empty? @observations))
        (is (= :stopped (:status ((:retry-cleanup! issued)))))
        (is (= 2 @attempts))
        (is (stopped? s))
        (is (= 1 (count @observations)))
        (is (identical? original (:listener (first @observations))))
        (is (identical? system (:system (first @observations))))
        (is (true? (:shutdown? (first @observations))))
        (is (= :stopped (:status ((:summary! issued)))))
        (is (= :stopped (:status ((:stop! issued)))))
        (is (= 2 @attempts)
            "Successful cleanup cannot replay the original stop")
        (is (= :configured-cleanup-retry-already-attempted
               (reason (:retry-cleanup! issued))))
        (is (= :configured-instance-generation-not-current
               (reason #((:inspect-generation! issued)
                         instance-generation system))))
        (is (= :configured-start-already-attempted
               (reason #((:start-once! issued) {})))))
      (finally (cleanup! s issued)))))

(deftest second-original-stop-failure-exhausts-configured-retry
  (let [s (scenario :honest)
        calls (atom 0)
        observations (atom 0)
        adapter (assoc (:raw s)
                       :biff.core/stop
                       (fn [_]
                         (swap! calls inc)
                         (throw (ex-info "Both stop attempts fail" {}))))
        issued (config/issue-configured-checked-http-application!
                (retry-source s adapter (fn [_ _]
                                          (swap! observations inc)
                                          true)))]
    (try
      ((:start-once! issued) {})
      (is (= :stop-cleanup-not-established
             (:status ((:stop! issued)))))
      (is (= :stop-cleanup-not-established
             (:status ((:retry-cleanup! issued)))))
      (is (= 2 @calls))
      (is (not (stopped? s)))
      (is (zero? @observations)
          "A failed lower stop must not consult the shutdown observer")
      (is (= :configured-cleanup-retry-already-attempted
             (reason (:retry-cleanup! issued))))
      (is (= :stop-cleanup-not-established
             (:status ((:stop! issued)))))
      (is (= 2 @calls) "Repeated failures must not get a third stop")
      (finally (cleanup! s issued)))))

(deftest returning-stop-callback-with-live-listener-cannot-confirm-cleanup
  (let [s (scenario :honest)
        calls (atom 0)
        observations (atom 0)
        adapter (assoc (:raw s) :biff.core/stop
                       (fn [_]
                         (if (= 1 (swap! calls inc))
                           (throw (ex-info "First stop fails" {}))
                           :returned-without-physical-shutdown)))
        issued (config/issue-configured-checked-http-application!
                (retry-source s adapter
                              (fn [captured _]
                                (swap! observations inc)
                                (and (identical? captured (listener s))
                                     (stopped? s)))))]
    (try
      ((:start-once! issued) {})
      (is (= :stop-cleanup-not-established
             (:status ((:stop! issued)))))
      (is (= :stop-cleanup-not-established
             (:status ((:retry-cleanup! issued)))))
      (is (= 2 @calls))
      (is (= 1 @observations))
      (is (not (stopped? s)))
      (is (= :stop-cleanup-not-established
             (:status ((:summary! issued)))))
      (is (= :configured-cleanup-retry-already-attempted
             (reason (:retry-cleanup! issued))))
      (finally (cleanup! s issued)))))

(deftest physically-closed-listener-with-negative-observer-remains-unconfirmed
  (let [s (scenario :honest)
        attempts (atom 0)
        observations (atom [])
        issued (config/issue-configured-checked-http-application!
                (retry-source s (fail-first-stop s attempts)
                              (fn [captured system]
                                (swap! observations conj [captured system])
                                false)))]
    (try
      (let [{:keys [system]} ((:start-once! issued) {})
            original (listener s)]
        (is (= :stop-cleanup-not-established
               (:status ((:stop! issued)))))
        (is (= :stop-cleanup-not-established
               (:status ((:retry-cleanup! issued)))))
        (is (= 2 @attempts))
        (is (stopped? s))
        (is (= 1 (count @observations)))
        (is (identical? original (ffirst @observations)))
        (is (identical? system (second (first @observations))))
        (is (= :stop-cleanup-not-established
               (:status ((:summary! issued)))))
        (is (= :configured-cleanup-retry-already-attempted
               (reason (:retry-cleanup! issued)))))
      (finally (cleanup! s issued)))))

(deftest throwing-listener-observer-cannot-produce-clean-shutdown
  (let [s (scenario :honest)
        attempts (atom 0)
        observer-calls (atom 0)
        issued (config/issue-configured-checked-http-application!
                (retry-source s (fail-first-stop s attempts)
                              (fn [_ _]
                                (swap! observer-calls inc)
                                (throw (ex-info "Unconfirmed observer" {})))))]
    (try
      ((:start-once! issued) {})
      (is (= :stop-cleanup-not-established
             (:status ((:stop! issued)))))
      (is (= :stop-cleanup-not-established
             (:status ((:retry-cleanup! issued)))))
      (is (= 2 @attempts))
      (is (stopped? s))
      (is (= 1 @observer-calls))
      (is (= :configured-cleanup-retry-already-attempted
             (reason (:retry-cleanup! issued))))
      (finally (cleanup! s issued)))))

(deftest callback-reentrancy-cannot-bypass-one-retry-capability
  (let [s (scenario :honest)
        attempts (atom 0)
        issued-ref (atom nil)
        reentrant-rejection (atom nil)
        issued (config/issue-configured-checked-http-application!
                (retry-source s (fail-first-stop s attempts)
                              (fn [_ _]
                                (reset! reentrant-rejection
                                        (reason (:retry-cleanup! @issued-ref)))
                                (stopped? s))))]
    (reset! issued-ref issued)
    (try
      ((:start-once! issued) {})
      (is (= :stop-cleanup-not-established
             (:status ((:stop! issued)))))
      (is (= :stopped (:status ((:retry-cleanup! issued)))))
      (is (= :configured-cleanup-retry-already-attempted
             @reentrant-rejection))
      (is (= 2 @attempts))
      (is (stopped? s))
      (is (= :configured-cleanup-retry-already-attempted
             (reason (:retry-cleanup! issued))))
      (finally (cleanup! s issued)))))


(deftest original-listener-retry-observer-may-inspect-concurrently
  ;; v2312: The trusted original-listener shutdown observer executes AFTER
  ;; guarded and probe stop results, while the configured issuer holds its
  ;; exclusive :retrying-cleanup claim. The observer can synchronously wait
  ;; for another JVM thread to attempt a generation inspection. Holding the
  ;; lifecycle monitor through this callback used to deadlock that thread.
  ;; A real Biff/XTDB executor, original physical stop and captured listener
  ;; identity are required; this test does not substitute an installed issuer.
  (let [s (scenario :honest)
        attempts (atom 0)
        issued-ref (atom nil)
        started (atom nil)
        entered (CountDownLatch. 1)
        release (CountDownLatch. 1)
        observation (promise)
        observer-calls (atom 0)
        issued (config/issue-configured-checked-http-application!
                (retry-source
                 s (fail-first-stop s attempts)
                 (fn [captured running]
                   (swap! observer-calls inc)
                   (.countDown entered)
                   (deliver observation
                            (deref (future
                                     (let [[generation system] @started]
                                       (reason
                                        #((:inspect-generation! @issued-ref)
                                          generation system))))
                                   1800 ::inspection-timeout))
                   (when-not (.await release 8 TimeUnit/SECONDS)
                     (throw (ex-info "Original host observer not released" {})))
                   (and (identical? captured (listener s))
                        (identical? running (second @started))
                        (stopped? s)))))
        recovery (atom nil)]
    (reset! issued-ref issued)
    (try
      (let [{:keys [system instance-generation]}
            ((:start-once! issued) {})]
        (reset! started [instance-generation system])
        (is (= :verified-configured-start (:status ((:summary! issued)))))
        (is (= :stop-cleanup-not-established (:status ((:stop! issued)))))
        (is (= 1 @attempts))
        (is (false? (stopped? s))
            "The original physical listener remains after failed first stop")
        (reset! recovery (future ((:retry-cleanup! issued))))
        (is (.await entered 4000 TimeUnit/MILLISECONDS)
            "The original host shutdown observer must be reached")
        (is (= :configured-instance-generation-not-current
               (deref observation 3500 ::observation-timeout))
            "Host observer must obtain a prompt cross-thread inspection rejection")
        (is (= :configured-retry-in-progress
               (deref (future (reason #((:stop! issued))))
                      1900 ::competing-stop-timeout))
            "A competing stop must not enter while original recovery is active")
        (is (= :configured-cleanup-retry-already-attempted
               (deref (future (reason #((:retry-cleanup! issued))))
                      1900 ::competing-retry-timeout))
            "The physical retry is consumed before original callbacks run")
        (is (= :retrying-cleanup (:status ((:summary! issued)))))
        (is (not (realized? @recovery))
            "Recovery must not finish before the original observer returns")
        (.countDown release)
        (is (= :stopped (:status (deref @recovery 6000 ::recovery-timeout))))
        (is (= 2 @attempts)
            "Exactly the first failed stop and one original retry may run")
        (is (= 1 @observer-calls))
        (is (stopped? s) "The actual Biff server executor must terminate")
        (is (= :stopped (:status ((:summary! issued)))))
        (is (= :stopped (:status ((:stop! issued)))))
        (is (= :configured-cleanup-retry-already-attempted
               (reason (:retry-cleanup! issued))))
        (is (= :configured-instance-generation-not-current
               (reason #((:inspect-generation! issued)
                         instance-generation system))))
        (is (= 2 @attempts)
            "A repeated stop or retry cannot re-invoke original cleanup"))
      (finally
        (.countDown release)
        (when @recovery (deref @recovery 6000 nil))
        (cleanup! s issued)))))
