(ns gesso.assurance.operational-report-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.operational-report :as report]
            [gesso.live.consistency.xtdb :as xtdb])
  (:import [java.time Instant]))

(def ^:private time (Instant/parse "2026-01-01T00:00:00Z"))
(def ^:private before (xtdb/basis 5 time))
(def ^:private after (xtdb/basis 12 (.plusMillis time 2500)))
(def ^:private sample
  {:phase :open :advance-in-progress? true :last-success-at (str time)
   :source {:catalog-fingerprint "catalog" :induction-fingerprint "induction"}
   :report {:status :partial-interval :through-basis before :target-basis after
            :caught-up? false :counts {:verified-mapped-transition 2}
            :open-obligations #{:unrestricted-physical-writer-closure}
            :initialization {:system-time time :catalog-fingerprint "catalog" :state {:secret 1}}
            :steps [{:command-id "private-command" :witness :private}]
            :checkpoint :private}
   :key :private :cursor :private})

(deftest operational-report-is-portable-and-explicitly-relative-to-last-sample
  (let [r (report/describe-history sample)]
    (is (= :partial-interval (:status r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (false? (:full-success? r)))
    (is (= :not-observed (get-in r [:coverage :current-frontier])))
    (is (= "7" (get-in r [:coverage :tx-id-distance])))
    (is (= 2500 (get-in r [:coverage :system-time-distance-ms])))
    (is (= "5" (get-in r [:coverage :through :tx-id])))
    (is (= "12" (get-in r [:coverage :sampled-target :tx-id])))
    (is (= {:verified-mapped-transition 2} (get-in r [:coverage :classification-counts])))
    (is (contains? (:open-obligations r) :unrestricted-physical-writer-closure))
    (is (contains? (:open-obligations r) :sampled-history-coverage-incomplete))
    (is (true? (get-in r [:controller :advance-in-progress?])))
    (is (= :owner-driven (:schedule r)))
    (is (= r (edn/read-string (pr-str r))))
    (is (not (re-find #"private|secret" (pr-str r))))))

(deftest failures-and-lifecycle-do-not-erase-historical-findings
  (let [step {:tx-id 6 :system-time time :committed? true :classification :unclassified
              :reason :unexplained-observed-row-change :command-id "private"}
        checked (-> sample (assoc :advance-in-progress? false)
                    (update :report assoc :status :unclassified-steps :through-basis after
                            :caught-up? true :counts {:unclassified 1} :first-unclassified step))
        failed (assoc checked :last-error :history-advance-failed :last-failure-at (str time))
        stopped (assoc checked :phase :closed)
        scheduler (assoc checked :schedule {:phase :stopped :last-outcome :fatal :consecutive-failures 0})]
    (is (= :not-selected (:status (report/describe-history nil))))
    (is (= :unclassified-steps (:status (report/describe-history checked))))
    (is (= :audit-failed (:status (report/describe-history failed))))
    (is (= :inactive (:status (report/describe-history stopped))))
    (is (= :scheduler-stopped (:status (report/describe-history scheduler))))
    (doseq [snapshot [checked failed stopped scheduler]
            :let [r (report/describe-history snapshot)]]
      (is (= "0" (get-in r [:coverage :tx-id-distance])))
      (is (= :unclassified-steps (get-in r [:coverage :status])))
      (is (= "6" (get-in r [:coverage :first-unclassified :tx-id])))
      (is (not (contains? (get-in r [:coverage :first-unclassified]) :command-id)))
      (is (contains? (:open-obligations r) :unclassified-historical-steps)))))
