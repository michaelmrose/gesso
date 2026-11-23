(ns gesso.assurance.release.installed-source-test
  "v2266: Original Claim catalog through an actual, locally started Biff/XTDB
   application, with permanent checks of the diagnostic release phase boundary.

   This suite uses the existing original Biff HTTP fixture, its actual loopback
   listener and the real source-derived operation checker. It does NOT grant
   installation eligibility, activation, release or execution permission.
   The host's listener-instance callback and catalog completeness remain
   explicit assumptions; this is one LOCAL in-process observation."
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
