(ns gesso.assurance.release.installed-source-test
  "v2304: Original Claim catalog through an actual, locally started Biff/XTDB
   application, with permanent checks of the diagnostic release phase boundary.

   This suite uses the existing original Biff HTTP fixture, its actual loopback
   listener and the real source-derived operation checker. It does NOT grant
   installation eligibility, activation, release or execution permission.
   The host's listener-instance callback and catalog completeness remain
   explicit assumptions; this is one LOCAL in-process observation.

   v2304 preserves the original physical-stop callback/summary lock-order
   regression on a real local Biff/XTDB listener, including concurrent-stop
   exclusion and one-shot cleanup without additional release authority."
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
