(ns gesso.assurance.release.installed-source-test
  "v2311: Original Claim catalog through an actual, locally started Biff/XTDB
   application, with permanent checks of the diagnostic release phase boundary.

   This suite uses the existing original Biff HTTP fixture, its actual loopback
   listener and the real source-derived operation checker. It does NOT grant
   installation eligibility, activation, release or execution permission.
   The host's listener-instance callback and catalog completeness remain
   explicit assumptions; this is one LOCAL in-process observation.

   v2304 preserves the original physical-stop callback/summary lock-order
   regression on a real local Biff/XTDB listener, including concurrent-stop
   exclusion and one-shot cleanup without additional release authority.

   v2306 permanently qualifies real original Biff physical cleanup retry
   callbacks that inspect this selected-source lifecycle on another JVM
   thread, preserving one-use retry and no-authority semantics.

   v2309 permanently qualifies provisional :inspecting and :assessing
   lifecycle claims on real original Biff/XTDB listeners. Cross-thread
   callback summaries must remain responsive, concurrent operations must
   reject promptly, and a failed assessment must physically retire the
   listener without ever issuing release authority.

   v2311 permanently checks that source preflight during an opted-in
   physical cleanup retry can call summary! from a different JVM thread,
   while duplicate retry and stop requests are refused. Rejected original
   catalog validation must leave the one-use physical retry unconsumed
   and permit recovery after the catalog is restored."
  (:import [java.util.concurrent CountDownLatch ExecutorService TimeUnit])
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.release.installed-source :as installed]
            [gesso.assurance.proof.checked-biff-http-application-configuration-fixture
             :as fixture]))

(use-fixtures :once fixture/with-real-application-fixture)

(defn- scenario [] (fixture/scenario :honest))

(defn- selected [s & [{:keys [catalog! listener-instance!] :as overrides}]]
  (let [catalog (get-in s [:env :catalog-source :catalog])
        catalog! (or catalog! (constantly catalog))
        listener-instance! (or listener-instance!
                               (fn [_] (:server @(:server-state s))))
        config (assoc (fixture/original-source s true)
                      :listener-instance! listener-instance!)]
    (installed/issue-selected-installed-catalog! catalog! config)))

(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- clean! [s instance]
  (when instance (try ((:stop! instance)) (catch Throwable _ nil)))
  (when-let [raw-stop (get-in s [:raw :biff.core/stop])]
    (try (raw-stop {}) (catch Throwable _ nil))))

(defn- no-authority! [report]
  (doseq [[field expected]
          {:release-authority :none
           :runtime-effect-permission :none
           :candidate-admission :not-issued
           :installation-eligibility :not-issued
           :activation-eligibility :not-issued
           :completion-claim :not-issued}]
    (is (= expected (get report field)) (str "No authority: " field))))

(defn- diagnostic-phases! [diagnostic]
  (is (= :checks-passed-only
         (get-in diagnostic [:decisions :candidate :status])))
  (is (= :checks-passed-only
         (get-in diagnostic [:decisions :installation :status])))
  (is (= :blocked
         (get-in diagnostic [:decisions :activation :status])))
  (is (= :not-examined
         (get-in diagnostic [:decisions :completion :status])))
  (is (= :matched (get-in diagnostic [:initial-inventory :status])))
  (is (= :matched (get-in diagnostic [:final-inventory :status])))
  (is (= {:candidate :matched :installation :matched} (:final-subjects diagnostic)))
  (is (= :blocked (:status diagnostic)))
  (is (true? (:conditional? diagnostic)))
  (no-authority! diagnostic))

(deftest a-running-original-biff-instance-qualifies-only-local-installation-checks
  (let [s (scenario)
        instance (selected s)]
    (try
      (is (= :issued (:status ((:summary! instance)))))
      (is (= :selected-source-and-current-local-http-observed-only
             (:status ((:start-once! instance) {}))))
      (let [report ((:assess-current-installation! instance))
            diagnostic (:release-diagnostic report)
            running (:current-local-instance report)]
        (is (= installed/local-assessment-type (:type report)))
        (is (= installed/local-assessment-version (:version report)))
        (is (= :selected-local-source-and-installation-checks-only
               (:status report)))
        (is (= :fresh-original-http-challenge-and-identity-observed
               (:current-local-listener running)))
        (is (seq (:observed-http-routes report)))
        (is (set? (:accepted-premises report)))
        (is (contains? (:accepted-premises report)
                       :gesso.release.premise/source-obligations-open))
        (is (contains? (:accepted-premises report)
                       :gesso.release.premise/trusted-host-listener-callback))
        (is (= :not-established (:original-application-source-complete report)))
        (is (= :not-issued (:original-release-installation-eligibility report)))
        (is (true? (:conditional? report)))
        (no-authority! report)
        (diagnostic-phases! diagnostic)
        (doseq [phase [:candidate :installation]]
          (is (= #{:passed}
                 (set (map :status
                           (vals (get-in diagnostic [:decisions phase :rows])))))))
        (is (= #{:gesso.release.local/wrong-generation-negative
                 :gesso.release.local/wrong-system-negative
                 :gesso.release.local/loopback-positive}
               (set (keys (get-in diagnostic [:decisions :installation :rows])))))
        (is (= :positive
               (get-in diagnostic [:decisions :installation :rows
                                   :gesso.release.local/loopback-positive :kind])))
        (doseq [negative-id
                [:gesso.release.local/wrong-generation-negative
                 :gesso.release.local/wrong-system-negative]]
          (is (= :negative
                 (get-in diagnostic [:decisions :installation :rows
                                     negative-id :kind]))))
        (is (not-any? #(contains? report %)
                      [:system :handler :adapter :server :port :generation
                       :listener-handle :host-handle :original-config])))
      (is (= :running (:status ((:summary! instance)))))
      (finally (clean! s instance)))))

(deftest repeated-qualification-reruns-physical-listener-observation
  (let [s (scenario)
        callbacks (atom 0)
        actual! (fn [_] (swap! callbacks inc)
                  (:server @(:server-state s)))
        instance (selected s {:listener-instance! actual!})]
    (try
      ((:start-once! instance) {})
      (let [n @callbacks
            first-report ((:assess-current-installation! instance))
            after-first @callbacks
            second-report ((:assess-current-installation! instance))]
        (is (pos? n))
        (is (> after-first n))
        (is (> @callbacks after-first))
        (is (= :selected-local-source-and-installation-checks-only
               (:status first-report) (:status second-report)))
        (is (= (:observed-http-routes first-report)
               (:observed-http-routes second-report)))
        (no-authority! second-report)
        (is (true? (:conditional? second-report))))
      (finally (clean! s instance)))))

(deftest managed-stop-revokes-observation-and-never-reopens-one-shot-start
  (let [s (scenario)
        instance (selected s)]
    (try
      ((:start-once! instance) {})
      (is (= :selected-local-source-and-installation-checks-only
             (:status ((:assess-current-installation! instance)))))
      (is (= :stopped (:status ((:stop! instance)))))
      (is (= :stopped (:status ((:summary! instance)))))
      (is (= :requires-fresh-inspection
             (:current-installation-evidence ((:summary! instance)))))
      (is (= :no-current-issued-running-instance
             (reason #((:assess-current-installation! instance)))))
      (is (= :no-current-issued-running-instance
             (reason #((:inspect-current! instance)))))
      (is (= :selected-original-http-start-not-once-only
             (reason #((:start-once! instance) {}))))
      (is (= :none (:release-authority ((:summary! instance)))))
      (finally (clean! s instance)))))

(deftest listener-substitution-revokes-previous-observation
  (let [s (scenario)
        substitute (atom nil)
        instance (selected s {:listener-instance!
                              (fn [_] (or @substitute
                                          (:server @(:server-state s))))})]
    (try
      ((:start-once! instance) {})
      (is (= :selected-local-source-and-installation-checks-only
             (:status ((:assess-current-installation! instance)))))
      (reset! substitute (Object.))
      (is (= :selected-local-installation-phase-not-qualified
             (reason #((:assess-current-installation! instance)))))
      (reset! substitute nil)
      (is (not= :running (:status ((:summary! instance)))))
      (is (= :no-current-issued-running-instance
             (reason #((:assess-current-installation! instance)))))
      (is (= :none (:release-authority ((:summary! instance)))))
      (finally (clean! s instance)))))

(deftest original-catalog-drift-fails-closed-and-stops-the-selected-instance
  (let [s (scenario)
        current (atom (get-in s [:env :catalog-source :catalog]))
        instance (selected s {:catalog! (fn [] @current)})]
    (try
      ((:start-once! instance) {})
      (is (= :selected-local-source-and-installation-checks-only
             (:status ((:assess-current-installation! instance)))))
      (swap! current update :invariants dissoc
             (first (keys (:invariants @current))))
      (is (= :selected-local-installation-phase-not-qualified
             (reason #((:assess-current-installation! instance)))))
      (is (not= :running (:status ((:summary! instance)))))
      (is (= :no-current-issued-running-instance
             (reason #((:assess-current-installation! instance)))))
      (finally (clean! s instance)))))

(deftest invalid-startup-is-one-use-and-does-not-grant-authority
  (let [s (scenario)
        current (atom (get-in s [:env :catalog-source :catalog]))
        instance (selected s {:catalog! (fn [] @current)})]
    (try
      (swap! current update :invariants dissoc
             (first (keys (:invariants @current))))
      (is (= :invariant-count
             (reason #((:start-once! instance) {}))))
      (reset! current (get-in s [:env :catalog-source :catalog]))
      (is (= :selected-original-http-start-not-once-only
             (reason #((:start-once! instance) {}))))
      (is (not= :running (:status ((:summary! instance)))))
      (is (= :none (:release-authority ((:summary! instance)))))
      (finally (clean! s instance)))))

(deftest construction-requires-a-truthful-host-listener-identity-source
  (let [s (scenario)
        opts (fixture/original-source s true)
        catalog (get-in s [:env :catalog-source :catalog])]
    (try
      (is (= :missing-selected-original-source-or-listener
             (reason #(installed/issue-selected-installed-catalog!
                       (constantly catalog) opts))))
      (is (= :missing-selected-original-source-or-listener
             (reason #(installed/issue-selected-installed-catalog!
                       nil (assoc opts :listener-instance! (fn [_] :fake))))))
      (is (= :missing-selected-original-source-or-listener
             (reason #(installed/issue-selected-installed-catalog!
                       (constantly catalog)
                       (assoc opts :listener-instance! :fake)))))
      (is (nil? @(:server-state s)))
      (finally (clean! s nil)))))

(deftest physical-stop-callback-may-read-selected-lifecycle-without-deadlock
  ;; A host-managed Biff stop callback can synchronously ask another JVM
  ;; thread to read this selected issuer's lifecycle summary. The selected
  ;; source must claim :stopping under its monitor and release the monitor
  ;; BEFORE invoking the original physical stop. Prior to v2303, this
  ;; inversion timed out the callback's cross-thread summary and a competing
  ;; stop, even though the original listener could eventually be stopped.
  (let [s (scenario)
        original (fixture/original-source s true)
        original-stop (get-in original [:adapter-module :biff.core/stop])
        original-calls (atom 0)
        instance-ref (atom nil)
        callback-entered (promise)
        callback-release (promise)
        callback-summary (promise)
        config (-> original
                   (assoc :listener-instance!
                          (fn [_] (:server @(:server-state s))))
                   (assoc-in [:adapter-module :biff.core/stop]
                             (fn [system]
                               (swap! original-calls inc)
                               (deliver callback-entered true)
                               ;; The observer MUST run on another thread,
                               ;; reproducing the physical callback's real
                               ;; monitor inversion, with a bounded wait.
                               (deliver callback-summary
                                        (deref (future ((:summary! @instance-ref)))
                                               2300 ::summary-timeout))
                               (deref callback-release 5000 ::callback-timeout)
                               (original-stop system))))
        catalog (get-in s [:env :catalog-source :catalog])
        instance (installed/issue-selected-installed-catalog!
                  (constantly catalog) config)]
    (reset! instance-ref instance)
    (try
      (is (= :selected-source-and-current-local-http-observed-only
             (:status ((:start-once! instance) {}))))
      (let [stop-future (future ((:stop! instance)))]
        (try
          (is (= true (deref callback-entered 3000 ::callback-not-entered))
              "The original configured physical-stop callback must execute")
          (is (= 1 @original-calls))
          (let [snapshot (deref callback-summary 4000 ::callback-no-snapshot)]
            (is (map? snapshot)
                "The original callback can obtain a lifecycle summary")
            (is (= :stopping (:status snapshot)))
            (is (= :requires-fresh-inspection
                   (:current-installation-evidence snapshot)))
            (is (= :not-issued (:installation-eligibility snapshot)))
            (is (= :none (:release-authority snapshot))))
          (is (= :local-physical-stop-in-progress
                 (deref (future (reason #((:stop! instance))))
                        1500 ::competing-stop-timeout))
              "An in-flight original physical stop excludes a second stop")
          (is (= :stopping
                 (deref (future (:status ((:summary! instance))))
                        1500 ::summary-timeout))
              "An external summary stays responsive during cleanup")
          (is (= :no-current-issued-running-instance
                 (deref (future
                          (reason #((:assess-current-installation! instance))))
                        1500 ::assessment-timeout))
              "No installation requalification is permitted during cleanup")
          (is (not (realized? stop-future))
              "Physical cleanup cannot finalize while its callback is held")
          (finally (deliver callback-release true)))
        (is (= :stopped (:status (deref stop-future 5000 ::stop-timeout))))
        (is (= :stopped (:status ((:summary! instance)))))
        (is (= :none (:release-authority ((:summary! instance)))))
        (is (= :no-current-issued-running-instance
               (reason #((:inspect-current! instance)))))
        (is (= :selected-original-http-start-not-once-only
               (reason #((:start-once! instance) {}))))
        (is (= :stopped (:status ((:stop! instance))))
            "A confirmed physical stop remains idempotent")
        (is (= 1 @original-calls)
            "A repeated stop must not call physical shutdown again"))
      (finally
        (deliver callback-release true)
        (clean! s instance)))))

(deftest original-managed-retry-callback-can-observe-selected-source-lifecycle
  ;; An original Biff physical stop first fails while the listener remains
  ;; alive. The sole opted-in host-idempotent retry enters the SAME original
  ;; Biff adapter, and requests this selected-source wrapper's summary on a
  ;; distinct JVM thread. Before v2305 the wrapper retained monitor over
  ;; the callback and the nested observation could not complete.
  ;;
  ;; This is a real original Biff/XTDB listener and original configured
  ;; source/issuer, not a replacement installed-source callback. Time-bounded
  ;; observations expose deadlocks while cleanup always releases the latch.
  (let [s (scenario)
        physical-stop (get-in s [:raw :biff.core/stop])
        physical-calls (atom 0)
        instance-ref (atom nil)
        entered (CountDownLatch. 1)
        release (CountDownLatch. 1)
        observed-summary (promise)
        configured-stop
        (fn [system]
          (if (= 1 (swap! physical-calls inc))
            (throw (ex-info "Injected first original physical stop failure"
                            {:first-stop-failure true}))
            (do
              (.countDown entered)
              (deliver observed-summary
                       (deref (future ((:summary! @instance-ref)))
                              1800 ::summary-timeout))
              (when-not (.await release 8 TimeUnit/SECONDS)
                (throw (ex-info "Original physical retry was not released" {})))
              (physical-stop system))))
        config (assoc (fixture/original-source s true)
                      :adapter-module
                      (assoc (:raw s) :biff.core/stop configured-stop)
                      :listener-instance!
                      (fn [_] (:server @(:server-state s)))
                      :listener-shutdown-confirmed?
                      (fn [listener _]
                        (and (identical? listener (:server @(:server-state s)))
                             (.isShutdown ^ExecutorService
                                          (:executor @(:server-state s)))))
                      :idempotent-stop-retry? true)
        catalog (get-in s [:env :catalog-source :catalog])
        instance (installed/issue-selected-installed-catalog!
                  (constantly catalog) config)
        retry-worker (atom nil)]
    (reset! instance-ref instance)
    (try
      (is (= :selected-source-and-current-local-http-observed-only
             (:status ((:start-once! instance) {}))))
      (is (= :cleanup-unconfirmed (:status ((:stop! instance)))))
      (is (= 1 @physical-calls)
          "The first original Biff stop fails without stopping the listener")
      (is (= :cleanup-unconfirmed (:status ((:summary! instance)))))
      (is (= :explicit-host-idempotence
             (:cleanup-retry-policy ((:summary! instance)))))
      (reset! retry-worker (future ((:retry-cleanup! instance))))
      (is (.await entered 3000 TimeUnit/MILLISECONDS)
          "The original configured physical retry callback must execute")
      (let [snapshot (deref observed-summary 3500 ::not-observed)]
        (is (map? snapshot)
            "The original physical retry must obtain a cross-thread summary")
        (is (= :retrying-cleanup (:status snapshot)))
        (is (true? (:cleanup-retry-attempted? snapshot)))
        (is (= :explicit-host-idempotence (:cleanup-retry-policy snapshot)))
        (is (= :requires-fresh-inspection
               (:current-installation-evidence snapshot)))
        (is (= :not-issued (:installation-eligibility snapshot)))
        (is (= :none (:release-authority snapshot))))
      (is (= :local-cleanup-retry-already-attempted
             (deref (future (reason #((:retry-cleanup! instance))))
                    1900 ::duplicate-retry-timeout))
          "Competing retry is rejected while the physical callback runs")
      (is (= :local-cleanup-retry-in-progress
             (deref (future (reason #((:stop! instance))))
                    1900 ::competing-stop-timeout))
          "Competing physical stop cannot replace the pending retry")
      (is (= :no-current-issued-running-instance
             (deref (future (reason #((:inspect-current! instance))))
                    1900 ::inspection-timeout))
          "No stale running-instance evidence can be reused during recovery")
      (is (not (realized? @retry-worker))
          "Recovery must wait for the real original callback to finish")
      (.countDown release)
      (is (= :stopped (:status (deref @retry-worker 6000 ::retry-timeout))))
      (is (= 2 @physical-calls)
          "Exactly one failed stop and one original physical retry ran")
      (is (= :stopped (:status ((:summary! instance)))))
      (is (true? (.isShutdown ^ExecutorService
                            (:executor @(:server-state s))))
          "Original physical retry must terminate the real server executor")
      (is (= :none (:release-authority ((:summary! instance)))))
      (is (= :local-cleanup-retry-already-attempted
             (reason #((:retry-cleanup! instance))))
          "The source never grants a second retry after success")
      (is (= :no-current-issued-running-instance
             (reason #((:inspect-current! instance)))))
      (is (= :stopped (:status ((:stop! instance))))
          "Confirmed terminal stop remains idempotent")
      (is (= 2 @physical-calls)
          "Idempotent stop cannot call the physical adapter again")
      (finally
        (.countDown release)
        (when @retry-worker (deref @retry-worker 6000 nil))
        (clean! s instance))))
)

(deftest original-live-inspection-summary-and-exclusive-claim
  (let [s (scenario)
        instance-ref (atom nil)
        intercept? (atom false)
        entered (CountDownLatch. 1)
        release (CountDownLatch. 1)
        observed (promise)
        inspected (atom nil)
        config (assoc (fixture/original-source s true)
                      :listener-instance!
                      (fn [_]
                        (when (compare-and-set! intercept? true false)
                          (.countDown entered)
                          (deliver observed
                                   (deref (future ((:summary! @instance-ref)))
                                          2500 ::summary-timeout))
                          (.await release 8 TimeUnit/SECONDS))
                        (:server @(:server-state s))))
        catalog (get-in s [:env :catalog-source :catalog])
        instance (installed/issue-selected-installed-catalog!
                  (constantly catalog) config)]
    (reset! instance-ref instance)
    (try
      (is (= :selected-source-and-current-local-http-observed-only
             (:status ((:start-once! instance) {}))))
      (reset! intercept? true)
      (reset! inspected (future (try ((:inspect-current! instance))
                                     (catch Throwable error error))))
      (is (.await entered 4 TimeUnit/SECONDS))
      ;; The original host callback synchronously awaits a summary on a
      ;; separate JVM thread. This fails against the pre-v2308 monitor
      ;; ordering and is not merely an in-memory lifecycle simulation.
      (let [snapshot (deref observed 3500 ::missing)]
        (is (map? snapshot) "Listener callback must read lifecycle without timing out")
        (is (= :inspecting (:status snapshot)))
        (is (= :none (:release-authority snapshot))))
      (is (= :local-inspection-in-progress
             (deref (future (reason #((:stop! instance))))
                    1800 ::stop-timeout)))
      (is (= :no-current-issued-running-instance
             (deref (future (reason #((:assess-current-installation! instance))))
                    1800 ::assessment-timeout)))
      (is (not (realized? @inspected)))
      (.countDown release)
      (is (= :fresh-original-http-challenge-and-identity-observed
             (:current-local-listener (deref @inspected 5000 ::inspect-timeout))))
      (is (= :running (:status ((:summary! instance)))))
      (is (= :stopped (:status ((:stop! instance)))))
      (is (= :none (:release-authority ((:summary! instance)))))
      (finally
        (.countDown release)
        (when @inspected (deref @inspected 5000 nil))
        (clean! s instance)))))

(deftest failed-original-assessment-retires-real-listener-without-monitor
  (let [s (scenario)
        instance-ref (atom nil)
        intercept? (atom false)
        entered (CountDownLatch. 1)
        release (CountDownLatch. 1)
        observation (promise)
        stop-observation (promise)
        physical-stop (get-in s [:raw :biff.core/stop])
        original-config (fixture/original-source s true)
        config (-> original-config
                   (assoc :listener-instance!
                          (fn [_]
                            (when (compare-and-set! intercept? true false)
                              (.countDown entered)
                              (deliver observation
                                       (deref (future ((:summary! @instance-ref)))
                                              2500 ::summary-timeout))
                              (.await release 8 TimeUnit/SECONDS)
                              (throw (ex-info "Injected failure in current listener check" {})))
                            (:server @(:server-state s))))
                   (assoc-in [:adapter-module :biff.core/stop]
                             (fn [system]
                               (deliver stop-observation
                                        (deref (future ((:summary! @instance-ref)))
                                               2500 ::stop-summary-timeout))
                               (physical-stop system))))
        catalog (get-in s [:env :catalog-source :catalog])
        instance (installed/issue-selected-installed-catalog!
                  (constantly catalog) config)
        result (atom nil)]
    (reset! instance-ref instance)
    (try
      (is (= :selected-source-and-current-local-http-observed-only
             (:status ((:start-once! instance) {}))))
      (reset! intercept? true)
      (reset! result (future (try ((:assess-current-installation! instance))
                                  :unexpected-assessment-success
                                  (catch Throwable error error))))
      (is (.await entered 7 TimeUnit/SECONDS))
      (let [snapshot (deref observation 4000 ::missing)]
        (is (map? snapshot))
        (is (= :assessing (:status snapshot)))
        (is (= :none (:release-authority snapshot))))
      (is (= :local-assessment-in-progress
             (deref (future (reason #((:stop! instance))))
                    1800 ::stop-timeout)))
      (is (= :no-current-issued-running-instance
             (deref (future (reason #((:inspect-current! instance))))
                    1800 ::inspect-timeout)))
      (is (not (realized? @result)))
      (.countDown release)
      (is (instance? Throwable (deref @result 7000 ::assessment-timeout)))
      ;; Failure-triggered physical cleanup must not monopolize the
      ;; lifecycle monitor during original configured Biff stop callbacks.
      (let [snapshot (deref stop-observation 4500 ::missing)]
        (is (map? snapshot) "Original managed stop callback must see lifecycle")
        (is (= :stopping (:status snapshot)))
        (is (= :none (:release-authority snapshot))))
      (is (= :stopped (:status ((:summary! instance)))))
      (is (= :no-current-issued-running-instance
             (reason #((:inspect-current! instance)))))
      (is (= :selected-original-http-start-not-once-only
             (reason #((:start-once! instance) {}))))
      (is (true? (.isShutdown ^ExecutorService
                            (:executor @(:server-state s)))))
      (is (= :none (:release-authority ((:summary! instance)))))
      (finally
        (.countDown release)
        (when @result (deref @result 5000 nil))
        (clean! s instance)))))

(deftest original-catalog-retry-preflight-allows-cross-thread-lifecycle-summary
  ;; The source-relative retry preflight invokes the ORIGINAL frozen
  ;; catalog callback while :validating-retry is exclusively claimed.
  ;; That callback may need a lifecycle summary from a different JVM
  ;; thread. A blocked callback must not deadlock or consume the retry.
  (let [s (scenario)
        catalog (get-in s [:env :catalog-source :catalog])
        real-stop (get-in s [:raw :biff.core/stop])
        physical-calls (atom 0)
        instance-ref (atom nil)
        armed? (atom false)
        entered (CountDownLatch. 1)
        release (CountDownLatch. 1)
        catalog-observation (promise)
        catalog! (fn []
                   (when (compare-and-set! armed? true false)
                     (.countDown entered)
                     (deliver catalog-observation
                              (deref (future ((:summary! @instance-ref)))
                                     1800 ::summary-timeout))
                     (when-not (.await release 8 TimeUnit/SECONDS)
                       (throw (ex-info "Catalog validation was never released" {}))))
                   catalog)
        config (assoc (fixture/original-source s true)
                      :adapter-module
                      (assoc (:raw s) :biff.core/stop
                             (fn [system]
                               (if (= 1 (swap! physical-calls inc))
                                 (throw (ex-info "First stop failed" {}))
                                 (real-stop system))))
                      :listener-instance!
                      (fn [_] (:server @(:server-state s)))
                      :listener-shutdown-confirmed?
                      (fn [listener _]
                        (and (identical? listener (:server @(:server-state s)))
                             (.isShutdown ^ExecutorService (:executor @(:server-state s)))))
                      :idempotent-stop-retry? true)
        instance (installed/issue-selected-installed-catalog! catalog! config)
        retry-job (atom nil)]
    (reset! instance-ref instance)
    (try
      (is (= :selected-source-and-current-local-http-observed-only
             (:status ((:start-once! instance) {}))))
      (is (= :cleanup-unconfirmed (:status ((:stop! instance)))))
      (is (= 1 @physical-calls))
      (reset! armed? true)
      (reset! retry-job (future (try ((:retry-cleanup! instance))
                                     (catch Throwable ex ex))))
      (is (.await entered 4000 TimeUnit/MILLISECONDS)
          "Physical-retry validation must call the original selected catalog")
      (let [snapshot (deref catalog-observation 3500 ::missing)]
        (is (map? snapshot) "Catalog callback must read lifecycle without deadlocking")
        (is (= :validating-retry (:status snapshot)))
        (is (= false (:cleanup-retry-attempted? snapshot))
            "No physical callback has been attempted while validating")
        (is (= :none (:release-authority snapshot))))
      (is (= :local-cleanup-retry-in-progress
             (deref (future (reason #((:stop! instance)))) 1700 ::stop-timeout)))
      (is (= :local-cleanup-retry-in-progress
             (deref (future (reason #((:retry-cleanup! instance))))
                    1700 ::retry-timeout)))
      (is (not (realized? @retry-job)))
      (.countDown release)
      (is (= :stopped (:status (deref @retry-job 9000 ::retry-timeout))))
      (is (= 2 @physical-calls))
      (is (= :stopped (:status ((:summary! instance)))))
      (is (= true (.isShutdown ^ExecutorService
                              (:executor @(:server-state s)))))
      (is (= :none (:release-authority ((:summary! instance)))))
      (finally
        (.countDown release)
        (when @retry-job (deref @retry-job 9000 nil))
        (clean! s instance)))))

(deftest rejected-catalog-retry-preflight-preserves-original-physical-capability
  ;; No original physical retry occurs if source revalidation rejects.
  ;; After the original catalog is restored, one retry remains available
  ;; to stop the real Biff/XTDB listener and executor exactly once.
  (let [s (scenario)
        original (get-in s [:env :catalog-source :catalog])
        current (atom original)
        physical-stop (get-in s [:raw :biff.core/stop])
        physical-calls (atom 0)
        config (assoc (fixture/original-source s true)
                      :adapter-module
                      (assoc (:raw s) :biff.core/stop
                             (fn [system]
                               (if (= 1 (swap! physical-calls inc))
                                 (throw (ex-info "Expected original first stop failure" {}))
                                 (physical-stop system))))
                      :listener-instance!
                      (fn [_] (:server @(:server-state s)))
                      :listener-shutdown-confirmed?
                      (fn [listener _]
                        (and (identical? listener (:server @(:server-state s)))
                             (.isShutdown ^ExecutorService (:executor @(:server-state s)))))
                      :idempotent-stop-retry? true)
        instance (installed/issue-selected-installed-catalog!
                  (fn [] @current) config)]
    (try
      (is (= :selected-source-and-current-local-http-observed-only
             (:status ((:start-once! instance) {}))))
      (is (= :cleanup-unconfirmed (:status ((:stop! instance)))))
      (is (= 1 @physical-calls))
      (swap! current update :invariants dissoc
             (first (keys (:invariants @current))))
      (is (= :invariant-count (reason #((:retry-cleanup! instance)))))
      (is (= :cleanup-unconfirmed (:status ((:summary! instance)))))
      (is (false? (:cleanup-retry-attempted? ((:summary! instance)))))
      (is (= 1 @physical-calls))
      (is (= :none (:release-authority ((:summary! instance)))))
      (reset! current original)
      (is (= :stopped (:status ((:retry-cleanup! instance)))))
      (is (= 2 @physical-calls))
      (is (.isShutdown ^ExecutorService (:executor @(:server-state s))))
      (is (= :local-cleanup-retry-already-attempted
             (reason #((:retry-cleanup! instance)))))
      (is (= :none (:release-authority ((:summary! instance)))))
      (finally
        (try ((:stop! instance)) (catch Throwable _ nil))
        (try (physical-stop {}) (catch Throwable _ nil))))))
