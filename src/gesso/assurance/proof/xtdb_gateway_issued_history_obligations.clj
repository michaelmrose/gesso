(ns gesso.assurance.proof.xtdb-gateway-issued-history-obligations
  "v1876: Source-relative, purely declarative OPEN conditions for claiming
   complete selected-row XTDB history from bounded pinned observations.

   These are NEW diagnostic proof obligations. They do NOT retroactively
   belong to the original certified proof kernel's checked open-obligation
   ledger and do not discharge anything in that ledger. A copied report is
   neither a certificate nor evidence of the conditions being satisfied."
  (:require [clojure.set :as set]))

(def report-type
  :gesso.assurance.proof.xtdb-gateway-issued-history-obligations/diagnostic-open)
(def report-version 1)

(def required-conditions
  #{:history-assurance/selected-row-version-retention-through-interval
    :history-assurance/selected-row-erasure-exclusion-through-interval
    :history-assurance/complete-system-valid-time-interval-visibility
    :history-assurance/authenticated-database-target-and-snapshot-provenance})

(def ^:private origin-keys
  #{:proof-fingerprint :proof-source-fingerprint :proof-subject-fingerprint
    :dependency-graph-fingerprint :transition-fingerprint :route-ids})

(defn- reject! [reason]
  (throw (ex-info "Original source-bound history obligations not established."
                  {:reason reason})))

(defn derive-open-history-conditions!
  "Require a complete, original-current checked certified-operation scope.
   Return immutable, explicitly undischarged conditions for *each* certified
   operation. Uncertified routes receive none. Do not accept read results or
   caller-supplied proofs; a bounded snapshot query cannot discharge these.

   This function only checks report shape and association; the caller must
   have obtained and rechecked the original source with original checkers."
  [{:keys [certified-operation-ids certified-operation-proof-origins
           certified-route-ids uncertified-operation-ids uncertified-route-ids]
    :as original-scope}]
  (let [origins certified-operation-proof-origins
        all-routes (vec (mapcat :route-ids (vals origins)))]
    (when-not (and (map? original-scope) (map? origins) (seq origins)
                   (set? certified-operation-ids)
                   (set? certified-route-ids)
                   (set? uncertified-operation-ids)
                   (set? uncertified-route-ids)
                   (= certified-operation-ids (set (keys origins)))
                   (every? qualified-keyword? (keys origins))
                   (every? (fn [origin]
                             (and (map? origin)
                                  (= origin-keys (set (keys origin)))
                                  (every? some? (vals origin))
                                  (set? (:route-ids origin))
                                  (seq (:route-ids origin))))
                           (vals origins))
                   (= (count all-routes) (count (set all-routes)))
                   (= certified-route-ids (set all-routes))
                   (empty? (set/intersection certified-operation-ids
                                             uncertified-operation-ids))
                   (empty? (set/intersection certified-route-ids
                                             uncertified-route-ids)))
      (reject! :incomplete-source-bound-history-obligation-scope))
    {:type report-type
     :version report-version
     :provenance :new-diagnostic-conditions-not-original-kernel-obligations
     :proof-claim :selected-row-history-continuity
     :required-conditions required-conditions
     :certified-operation-open-conditions
     (into (sorted-map)
           (for [[operation origin] origins]
             [operation {:original-certified-proof-origin origin
                         :required-conditions required-conditions
                         :discharged-conditions #{}
                         :disposition :open-unproven}]))
     :uncertified-operation-ids uncertified-operation-ids
     :uncertified-route-ids uncertified-route-ids
     :uncertified-history-proof-conditions :none
     :bounded-inventory-discharge :not-established
     :row-boundary-discharge :not-established
     :database-basis-discharge :not-established
     :history-retention :not-established
     :erasure-exclusion :not-established
     :full-interval-coverage :not-established
     :snapshot-source-authenticity :not-established
     :historical-continuity :not-established
     :discharged-conditions #{}
     :original-kernel-proof-discharge :not-established
     :certificate :not-issued
     :authority :none}))
