(ns gesso.assurance.reference-mapped-observed-recovery
  "Compose local issued submission observation and retained recovery correspondence."
  (:require [gesso.assurance.reference-mapped-evidence :as evidence]
            [gesso.assurance.reference-mapped-submission :as submission]))

(def profile :selected-issued-observed-mapped-recovery-v1)

(defn compare-selected!
  "Check one locally issued submission observation and the issued recovery
   comparison against exactly the same selected steps. Do not expose a
   positive subcomparison when either side fails. Historical observation
   does not authenticate the provider, prove storage identity across reopen,
   establish an aborted receipt identity, or cover other writers."
  [node tables admission source outcome-declarations steps handle]
  (let [base {:observed-recovery-profile profile :authority :none :certificate :not-issued
              :provider-refinement :not-established :source-provenance :not-established
              :outcome-provenance :not-established :submission-provenance :not-established
              :aborted-receipt-identity :not-established :observation-storage-identity :not-established}
        fail #(assoc base :status %1 :reason %2)
        required #{:original :targets :docs :journal :expectation :prefix-count
                   :spec :context :source-id :declarations :residual-declarations}]
    (if-not (and (vector? steps) (nil? (meta steps)) (= 3 (count steps))
                 (every? #(and (map? %) (nil? (meta %)) (= required (set (keys %)))
                               (map? (:expectation %)) (map? (:spec %))) steps))
      (fail :unsupported :invalid-observed-recovery-source)
      (let [observed (submission/compare-observed! admission steps handle)]
        (cond
          (not= :matched (:status observed)) (fail (:status observed) (:reason observed))
          (not (and (= submission/observed-profile (:observed-profile observed))
                    (= :matched-local-issued-policy-window (:observation-origin observed))
                    (= :matched-observed-checked-candidates (:framed-handoffs observed))
                    (= :matched-selected-retained-operations (:adapter-payloads observed))
                    (= :matched-selected-commit-identities (:returned-transaction-keys observed))
                    (= :observed-throw-without-transaction-key (:aborted-call observed))))
          (fail :unsupported :invalid-observed-recovery-submission-comparison)
          :else
          (let [retained (evidence/compare-local-processed-retained-issued-executor-recovery!
                          node tables admission source outcome-declarations steps)]
            (cond
              (not= :matched (:status retained)) (fail (:status retained) (:reason retained))
              (not (and (= evidence/issued-executor-recovery-profile (:issued-executor-recovery-profile retained))
                        (= :matched-issued-declarations (:issued-admission-binding retained))
                        (= :matched-issued-closed-construction (:closed-decoder-binding retained))
                        (= (mapv #(get-in % [:expectation :tx-id]) steps) (:selected-tx-ids retained))
                        (= [:committed :aborted :committed] (:selected-commitments retained))
                        (= :matched-retained-journal-trajectory (:recovery-linkage retained))))
              (fail :unsupported :invalid-observed-recovery-retained-comparison)
              :else (merge retained
                           (select-keys observed [:observed-profile :observation-origin :framed-handoffs
                                                 :adapter-payloads :returned-transaction-keys :aborted-call])
                           base {:status :matched :submission-recovery-linkage :matched-local-observed-retained-steps}))))))))
