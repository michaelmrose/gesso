(ns gesso.assurance.provider-refinement
  "Explicit open provider premises for the selected v19 historical argument.
   This is a diagnostic decomposition, not a provider refinement checker."
  (:require [gesso.assurance.xtdb-envelope :as envelope]))

(def ^:private common-assumptions
  #{:source-log-decoder-and-retention-refinement
    :provider-ledger-identity-and-outcome-refinement
    :provider-pinned-snapshot-refinement
    :valid-time-continuity-between-steps})

(def ^:private write-assumptions
  #{:provider-ordered-atomic-write-refinement
    :provider-document-identity-and-valid-time-refinement
    :provider-assertion-semantics-refinement
    :transaction-internal-read-basis-refinement})

(def ^:private cases
  {:initialization {:premises [:physical-initialization :initialization-envelope-correspondence]
                    :assumptions write-assumptions}
   :manifest {:premises [:manifest-establishment-correspondence]
              :assumptions write-assumptions}
   :mapped-transition {:classification :verified-mapped-transition
                       :premises [:recorded-envelope-correspondence :intended-guard-prefix-correspondence
                                  :intended-mapped-write-correspondence :mapped-write-table-closure
                                  :transition-evidence-write-correspondence :publication-outbox-correspondence
                                  :publication-expectation-correspondence :residual-assertion-correspondence]
                       :assumptions write-assumptions}
   :worker-transition {:classification :verified-infrastructure-transition
                       :premises [:worker-envelope-correspondence]
                       :assumptions (conj write-assumptions :provider-transaction-clock-refinement)}
   :committed-stutter {:classification :observed-mapped-stutter
                       :premises [:committed-stutter-correspondence]
                       :assumptions #{:provider-assertion-semantics-refinement
                                      :provider-assertion-read-only-refinement}}
   :aborted-stutter {:classification :aborted-stutter
                     :premises [:aborted-stutter-correspondence]
                     :assumptions #{:provider-abort-atomicity-refinement}}})

(defn describe
  "Describe provider dependencies of an already assembled physical prefix
   argument. Counts are meaningful only for an eligible observed prefix.
   All semantic assumptions remain open, including unexercised cases."
  [argument counts]
  (let [eligible? (= :conditional-observed-prefix-argument (:status argument))
        premises (:premises argument)
        details (into {}
                      (for [[id {:keys [classification assumptions] required :premises}] cases
                            :let [observations (select-keys premises required)
                                  n (when eligible?
                                      (cond classification (get counts classification 0)
                                            (= id :manifest) (if (= :not-selected (:manifest-establishment-correspondence premises)) 0 1)
                                            :else 1))]]
                        [id {:status (cond
                                       (not eligible?) :not-established
                                       (zero? n) :not-observed
                                       (some #(or (nil? %) (= :open %)) (map premises required)) :incomplete-expectation
                                       :else :conditional-observation)
                             :observed-count n
                             :correspondence observations
                             :open-assumptions (into common-assumptions assumptions)}]))]
    {:status :not-established
     :profile :selected-xtdb-provider-refinement-v1
     :envelope-profile envelope/profile
     :scope :selected-history-through-persisted-basis
     :authority :none :certificate :not-issued
     :observed-prefix? eligible?
     :prefix-correspondence (select-keys premises [:historical-step-coverage :visible-valid-time-continuity])
     :through (when eligible? (:through argument))
     :cases details
     :open-assumptions (into #{} (mapcat :open-assumptions) (vals details))}))
