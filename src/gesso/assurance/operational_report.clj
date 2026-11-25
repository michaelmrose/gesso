(ns gesso.assurance.operational-report
  "Portable, nonauthoritative operational assurance observations. Format the
   host-owned history controller's last persisted report, not a live database
   query or a replacement for source-bound abstract assurance reports."
  (:require [gesso.live.consistency.xtdb :as xtdb])
  (:import [java.time Duration]))

(def ^:private permanent-obligations
  #{:history-coverage-beyond-sampled-frontier
    :complete-application-refinement})

(defn- basis-summary [basis]
  {:database (xtdb/basis-database basis)
   :tx-id (:tx-id basis)
   :system-time (str (xtdb/basis-system-time basis))})

(defn- coverage [report]
  (let [through (:through-basis report)
        target (:target-basis report)
        checked-time (xtdb/basis-system-time through)
        target-time (xtdb/basis-system-time target)]
    {:status (:status report)
     :through (basis-summary through)
     :sampled-target (basis-summary target)
     ;; IDs are log coordinates, not a count of pending transactions.
     :tx-id-distance (str (- (xtdb/basis-tx-id target) (xtdb/basis-tx-id through)))
     :system-time-distance-ms (.toMillis (Duration/between checked-time target-time))
     :caught-up-to-sampled-target? (:caught-up? report)
     :current-frontier :not-observed
     :classification-counts (:counts report)
     :first-unclassified (when-let [step (:first-unclassified report)]
                           (-> (select-keys step [:committed? :classification :reason])
                               (assoc :tx-id (str (:tx-id step)) :system-time (str (:system-time step)))))}))

(defn describe-history
  "Build an EDN-portable diagnostic summary of one controller observation.
   The owner-facing host API obtains that observation from the installed
   controller; a supplied map is merely descriptive data, never evidence of
   a live installation. No keys, cursors, callbacks, witness values or command
   identifiers are exported. Lag is relative to the last persisted sample."
  [snapshot]
  (let [{:keys [phase report last-error schedule source advance-in-progress?
                last-success-at last-failure-at]} snapshot
        base {:type :gesso.assurance/operational-history-report
              :version 1 :authority :none :certificate :not-issued
              :full-success? false :scope :last-persisted-history-observation}]
    (if-not snapshot
      (assoc base :status :not-selected :coverage nil
             :open-obligations (conj permanent-obligations :history-audit-not-selected))
      (let [unclassified? (pos? (get-in report [:counts :unclassified] 0))
            partial? (and report (not (:caught-up? report)))
            retired? (not= :open phase)
            scheduler-stopped? (and (= :open phase) (= :stopped (:phase schedule)))]
        (assoc base
               :status (cond retired? :inactive
                             last-error :audit-failed
                             scheduler-stopped? :scheduler-stopped
                             (nil? report) :not-observed
                             unclassified? :unclassified-steps
                             partial? :partial-interval
                             :else :observed-interval-checked)
               :source source
               :initialization (when-let [initial (:initialization report)]
                                 (-> (select-keys initial [:catalog-fingerprint :case-fingerprint
                                                          :initialization-fingerprint :physical-correspondence])
                                     (assoc :system-time (str (:system-time initial)))))
               :coverage (when report (coverage report))
               :controller {:phase phase :advance-in-progress? (boolean advance-in-progress?)
                            :last-error last-error :last-success-at last-success-at
                            :last-failure-at last-failure-at}
               :schedule (if schedule
                           (select-keys schedule [:phase :last-outcome :consecutive-failures
                                                  :failure-delay-ms :next-delay-ms])
                           :owner-driven)
               :open-obligations (cond-> (into permanent-obligations (:open-obligations report))
                                   (nil? report) (conj :history-coverage-not-observed)
                                   partial? (conj :sampled-history-coverage-incomplete)
                                   unclassified? (conj :unclassified-historical-steps)
                                   last-error (conj :history-audit-recovery)
                                   retired? (conj :history-controller-inactive)
                                   scheduler-stopped? (conj :history-scheduler-recovery)))))))

(defn- physical-prefix-argument [abstract snapshot history matching?]
  (let [r (:report snapshot)
        initial (:initialization r)
        counts (:counts r)
        invariants (:declared-invariant-ids abstract)
        abstract? (and (= :source-bound-conditional-abstract-safety (:verdict abstract))
                       (= :original-sources-and-both-live-handles-rechecked (:checking abstract))
                       (true? (:all-declared-invariants-conditionally-inductive? abstract))
                       (set? invariants) (seq invariants))
        initial? (and matching?
                      (= (:catalog-fingerprint initial) (get-in abstract [:identity :catalog-fingerprint]))
                      (= :checked-supplied-projection (:physical-correspondence initial))
                      (every? #(and (string? %) (seq %))
                              (map initial [:case-fingerprint :initialization-fingerprint]))
                      (instance? java.time.Instant (:system-time initial))
                      (:through-basis r)
                      (not (pos? (compare (:system-time initial)
                                         (xtdb/basis-system-time (:through-basis r))))))
        clean? (and (= :selected-mapped-ledger-chunk-v4 (:profile r))
                    (#{:observed-interval-checked :partial-interval} (:status history))
                    (= (:status r) (:status history))
                    (= (:caught-up? r) (= :observed-interval-checked (:status r)))
                    (map? counts) (nil? (:first-unclassified r))
                    (every? (fn [[k n]]
                              (and (#{:verified-mapped-transition :verified-infrastructure-transition
                                      :observed-mapped-stutter :aborted-stutter} k)
                                   (integer? n) (pos? n))) counts))
        eligible? (and matching? abstract? initial? clean?)]
    {:status (if eligible? :conditional-observed-prefix-argument :not-established)
     :scope :selected-mapped-history-through-persisted-basis
     :authority :none :certificate :not-issued
     :premises {:source-identity (if matching? :matched :open)
                :abstract-induction (if abstract? :conditionally-checked :open)
                :physical-initialization (if initial? :observed-projection-checked :open)
                :historical-step-coverage (if clean? :clean-v4-prefix :open)
                :visible-valid-time-continuity (if clean? :v4-prefix-checked :open)
                ;; Reconstructed guard identity is not evidence of execution.
                :historical-writer-provenance :not-established}
     :invariant-ids (if eligible? invariants #{})
     :through (when eligible? (get-in history [:coverage :through]))
     :open-obligations #{:historical-retention-and-checkpoint-integrity
                         :unrestricted-physical-writer-closure
                         :historical-mapped-writer-authentication
                         :transaction-internal-read-basis-refinement
                         :valid-time-continuity-between-steps
                         :history-coverage-beyond-sampled-frontier
                         :complete-physical-induction-correspondence}}))

(defn describe-application
  "Join source-relative abstract and operational descriptions by exact
   catalog and induction fingerprints. A matching description is not a
   proof of current installation, writer coverage or physical induction.
   The host supplies a rechecked abstract report; this pure formatter
   cannot authenticate arbitrary supplied report maps."
  [abstract snapshot]
  (let [history (describe-history snapshot)
        identity-keys [:catalog-fingerprint :induction-fingerprint]
        static-source (select-keys (:identity abstract) identity-keys)
        history-source (select-keys (:source history) identity-keys)
        matching? (and abstract snapshot
                       (every? #(and (string? %) (not (empty? %)))
                               (map static-source identity-keys))
                       (= static-source history-source))
        assumptions (get-in abstract [:classification :open :assumptions])
        premises (get-in abstract [:classification :certificate :open-premises])
        original (get-in abstract [:classification :original-obligations :by-operation])
        argument (physical-prefix-argument abstract snapshot history matching?)]
    {:type :gesso.assurance/operational-application-report
     :version 1 :authority :none :certificate :not-issued :full-success? false
     :scope :conditional-abstract-and-last-persisted-history-descriptions
     :source-join (cond matching? :matching-source-descriptions
                        (and abstract snapshot) :source-mismatch
                        abstract :static-only
                        snapshot :history-only
                        :else :not-selected)
     :abstract (when abstract
                 (assoc (select-keys abstract [:identity :fingerprint :checking :verdict
                                               :declared-operation-ids :declared-invariant-ids
                                               :all-declared-invariants-conditionally-inductive?])
                        :open-assumptions assumptions :open-premises premises
                        :original-open-obligations original))
     :history history
     :physical-induction argument
     :open-obligations (cond-> (into (into (into (into (:open-obligations history)
                                                      (:open-obligations argument)) assumptions)
                                            premises) (mapcat val original))
                         (not abstract) (conj :abstract-induction-not-selected)
                         (not matching?) (conj :static-history-source-join-open)
                         true (conj :complete-physical-induction-correspondence))}))
