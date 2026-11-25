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

(deftest combined-report-retains-both-sides-without-granting-authority
  (let [abstract {:identity (:source sample) :fingerprint "report"
                  :verdict :source-bound-conditional-abstract-safety
                  :classification {:open {:assumptions #{:checker-meta-soundness}}
                                   :certificate {:open-premises #{:abstract-premise}}
                                   :original-obligations {:by-operation {:example/op #{:original-premise}}}}}
        joined (report/describe-application abstract sample)
        mismatch (report/describe-application abstract (assoc-in sample [:source :induction-fingerprint] "other"))]
    (is (= :matching-source-descriptions (:source-join joined)))
    (is (= :source-mismatch (:source-join mismatch)))
    (is (= :static-only (:source-join (report/describe-application abstract nil))))
    (is (= :history-only (:source-join (report/describe-application nil sample))))
    (is (= :not-selected (:source-join (report/describe-application nil nil))))
    (is (= :source-mismatch (:source-join (report/describe-application {} {:phase :open}))))
    (doseq [r [joined mismatch]]
      (is (= :none (:authority r)))
      (is (= :not-issued (:certificate r)))
      (is (false? (:full-success? r)))
      (is (= (:coverage (report/describe-history sample)) (get-in r [:history :coverage])))
      (is (every? (:open-obligations r) [:checker-meta-soundness :abstract-premise
                                         :original-premise :sampled-history-coverage-incomplete
                                         :complete-physical-induction-correspondence]))
      (is (= r (edn/read-string (pr-str r)))))
    (is (contains? (:open-obligations mismatch) :static-history-source-join-open))))

(deftest physical-prefix-argument-requires-clean-current-profile
  (let [abstract {:identity (:source sample)
                  :verdict :source-bound-conditional-abstract-safety
                  :checking :original-sources-and-both-live-handles-rechecked
                  :all-declared-invariants-conditionally-inductive? true
                  :declared-invariant-ids #{:example/nonnegative}}
        snapshot (update sample :report assoc
                         :profile :selected-mapped-ledger-chunk-v8
                         :initialization {:system-time time :catalog-fingerprint "catalog"
                                          :case-fingerprint "case" :initialization-fingerprint "initial"
                                          :physical-correspondence :checked-supplied-projection})
        joined (report/describe-application abstract snapshot)
        arg (:physical-induction joined)]
    (is (= :conditional-observed-prefix-argument (:status arg)))
    (is (= :v8-prefix-checked (get-in arg [:premises :visible-valid-time-continuity])))
    (is (= :v8-mapped-steps-checked (get-in arg [:premises :recorded-envelope-correspondence])))
    (is (= :v8-mapped-steps-checked (get-in arg [:premises :intended-guard-prefix-correspondence])))
    (is (= :v8-mapped-steps-checked (get-in arg [:premises :intended-mapped-write-correspondence])))
    (is (= :v8-mapped-steps-checked (get-in arg [:premises :transition-evidence-write-correspondence])))
    (is (= :not-established (get-in arg [:premises :historical-writer-provenance])))
    (is (contains? (:open-obligations joined) :historical-intended-envelope-provenance))
    (is (contains? (:open-obligations joined) :historical-mapped-writer-authentication))
    (is (= #{:example/nonnegative} (:invariant-ids arg)))
    (is (= (get-in joined [:history :coverage :through]) (:through arg)))
    (is (= :none (:authority arg)))
    (is (= :not-issued (:certificate arg)))
    (is (every? (:open-obligations joined) (:open-obligations arg)))
    (is (= joined (edn/read-string (pr-str joined))))
    (doseq [[a s] [[(dissoc abstract :checking) snapshot]
                   [(assoc abstract :all-declared-invariants-conditionally-inductive? false) snapshot]
                   [abstract (assoc-in snapshot [:source :induction-fingerprint] "different")]
                   [abstract (assoc-in snapshot [:report :profile] :selected-mapped-ledger-chunk-v3)]
                   [abstract (assoc-in snapshot [:report :profile] :selected-mapped-ledger-chunk-v4)]
                   [abstract (assoc-in snapshot [:report :profile] :selected-mapped-ledger-chunk-v5)]
                   [abstract (assoc-in snapshot [:report :profile] :selected-mapped-ledger-chunk-v6)]
                   [abstract (assoc-in snapshot [:report :profile] :selected-mapped-ledger-chunk-v7)]
                   [abstract (assoc-in snapshot [:report :initialization :catalog-fingerprint] "different")]
                   [abstract (assoc-in snapshot [:report :initialization :system-time] (.plusSeconds time 1))]
                   [abstract (assoc-in snapshot [:report :counts] {:unknown 1})]
                   [abstract (assoc-in snapshot [:report :counts] {:unclassified 1})]
                   [abstract (assoc-in snapshot [:report :counts] {:aborted-stutter -1})]
                   [abstract (assoc snapshot :last-error :history-advance-failed)]
                   [abstract (assoc snapshot :phase :closed)]
                   [abstract nil]]]
      (let [r (:physical-induction (report/describe-application a s))]
        (is (= :not-established (:status r)))
        (is (= #{} (:invariant-ids r)))
        (is (nil? (:through r)))))))
