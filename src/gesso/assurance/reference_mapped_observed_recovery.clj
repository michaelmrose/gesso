(ns gesso.assurance.reference-mapped-observed-recovery
  "Compose local issued submission observation and retained recovery correspondence."
  (:require [gesso.assurance.reference-mapped-evidence :as evidence]
            [gesso.assurance.reference-mapped-submission :as submission]
            [gesso.assurance.reference-provider :as provider]
            [gesso.live.outbox.initialization :as initialization]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.model.inductive-plan :as induction]))

(def profile :selected-issued-observed-mapped-recovery-v1)

(def interleaved-profile :selected-issued-observed-mapped-worker-interleaved-recovery-v1)

(defn- compare-selected*
  [node tables admission source outcome-declarations steps handle interleaved?]
  (let [base {:observed-recovery-profile (if interleaved? interleaved-profile profile) :authority :none :certificate :not-issued
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
          (let [retained ((if interleaved?
                           evidence/compare-local-processed-retained-issued-executor-worker-interleaved-recovery!
                           evidence/compare-local-processed-retained-issued-executor-recovery!)
                          node tables admission source outcome-declarations steps)]
            (cond
              (not= :matched (:status retained)) (fail (:status retained) (:reason retained))
              (not (and (= (if interleaved? evidence/issued-executor-interleaved-recovery-profile
                                             evidence/issued-executor-recovery-profile)
                           (:issued-executor-recovery-profile retained))
                        (= :matched-issued-declarations (:issued-admission-binding retained))
                        (= :matched-issued-closed-construction (:closed-decoder-binding retained))
                        (= (mapv #(get-in % [:expectation :tx-id]) steps) (:selected-tx-ids retained))
                        (= [:committed :aborted :committed] (:selected-commitments retained))
                        (= :matched-retained-journal-trajectory (:recovery-linkage retained))
                        (or (not interleaved?)
                            (and (= provider/mapped-interleaved-recovery-profile (:mapped-recovery-profile retained))
                                 (vector? (:intervening-maintenance retained))
                                 (integer? (:replayed-transaction-count retained))))))
              (fail :unsupported :invalid-observed-recovery-retained-comparison)
              :else (merge retained
                           (select-keys observed [:observed-profile :observation-origin :framed-handoffs
                                                 :adapter-payloads :returned-transaction-keys :aborted-call])
                           base {:status :matched :submission-recovery-linkage :matched-local-observed-retained-steps}
                           (when interleaved? {:worker-observation-provenance :not-established})))))))))

(defn compare-selected!
  "Check one locally issued submission observation and the issued recovery
   comparison against exactly the same selected steps. Do not expose a
   positive subcomparison when either side fails. Historical observation
   does not authenticate the provider, prove storage identity across reopen,
   establish an aborted receipt identity, or cover other writers."
  [node tables admission source outcome-declarations steps handle]
  (compare-selected* node tables admission source outcome-declarations steps handle false))

(defn compare-worker-interleaved!
  "Match the three observed mapped submissions against the full retained
   worker-interleaved history. Worker envelopes are checked from history,
   not attributed to the synchronous submission observation."
  [node tables admission source outcome-declarations steps handle]
  (compare-selected* node tables admission source outcome-declarations steps handle true))

(def activated-profile :selected-activated-observed-mapped-recovery-v1)

(def activated-interleaved-profile :selected-activated-observed-mapped-worker-interleaved-recovery-v1)

(defn- compare-activated*
  [node tables admission source outcome-declarations steps handle interleaved?]
  (let [base {:activated-recovery-profile (if interleaved? activated-interleaved-profile activated-profile) :authority :none :certificate :not-issued
              :provider-refinement :not-established :observation-storage-identity :not-established
              :submission-provenance :not-established :aborted-receipt-identity :not-established}
        fail #(assoc base :status %1 :reason %2)]
    (try
      (let [{:keys [activation-root] :as observed} (publication/read-framed-submission-observation! admission handle)
            {:keys [observation root]} activation-root
            {:keys [source-id manifest-tx-id initialization-tx-id system-time]} observation
            {:keys [initializer descriptor]} root]
        (cond
          (nil? observed) (fail :unsupported :unissued-framed-submission-observation)
          (not (and (= :matched (:manifest-envelope observation) (:initialization-envelope observation))
                    (= :checked-supplied-projection (:physical-correspondence observation))
                    (string? source-id) (string? descriptor) (vector? initializer)
                    (integer? manifest-tx-id) (integer? initialization-tx-id)
                    (< manifest-tx-id initialization-tx-id)
                    (instance? java.time.Instant system-time)))
          (fail :unsupported :missing-selected-activation-root)
          (not (and (vector? steps) (= 3 (count steps)) (every? #(= source-id (:source-id %)) steps)))
          (fail :mismatch :selected-activation-source-mismatch)
          (not (and (set? tables) (every? tables [:gesso_initializations :gesso_publication_manifests])))
          (fail :unsupported :incomplete-activation-root-scope)
          :else
          (let [checked (induction/check-mapped-initialization! admission initializer)
                fingerprints [:catalog-fingerprint :case-fingerprint :initialization-fingerprint]
                manifest {:tx-id manifest-tx-id
                          :ops [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_publication_manifests WHERE _id = ?)" source-id]
                                ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE gesso$live$outbox$source_id = ?)" source-id]
                                [:put-docs :gesso_publication_manifests {:xt/id source-id :profile-version 1 :descriptor descriptor}]]}
                init {:tx-id initialization-tx-id :system-time system-time
                      :ops (initialization/transaction source-id initializer (:tables checked) checked)}]
            (if-not (= (select-keys checked fingerprints) (select-keys observation fingerprints))
              (fail :mismatch :selected-activation-initialization-mismatch)
              (let [r (provider/compare-with-retained-root!
                       node manifest init (get-in steps [0 :expectation :tx-id])
                       #(compare-selected* node tables admission source outcome-declarations steps handle interleaved?))]
                (if (and (= :matched (:status r)) (= (if interleaved? interleaved-profile profile) (:observed-recovery-profile r))
                         (= :matched-local-observed-retained-steps (:submission-recovery-linkage r)))
                  (merge r base {:status :matched :activation-root-binding :matched-local-activated-retained-root
                                 :initialization-tx-id initialization-tx-id :manifest-tx-id manifest-tx-id})
                  (fail (if (= :matched (:status r)) :unsupported (:status r))
                        (or (:reason r) :invalid-activated-recovery-comparison))))))))
      (catch clojure.lang.ExceptionInfo e
        (fail :unsupported (or (:reason (ex-data e)) :invalid-activated-recovery-source))))))

(defn compare-activated!
  "Require an observation captured under the same locally recorded activated
   writer generation, and its exact manifest and initialization as the first
   two retained transactions immediately preceding the selected recovery.
   Reconstruct the seed envelope from the issued admission. Historical root
   matching is not durable storage authentication or full host refinement."
  [node tables admission source outcome-declarations steps handle]
  (compare-activated* node tables admission source outcome-declarations steps handle false))

(defn compare-activated-worker-interleaved!
  "Bind the activated initialization root and the three locally observed
   mapped submissions to one complete worker-interleaved retained sample.
   The root must still immediately precede the first selected submission."
  [node tables admission source outcome-declarations steps handle]
  (compare-activated* node tables admission source outcome-declarations steps handle true))

(def local-acquisition-profile :selected-local-origin-retained-recovery-v1)

(def ^:private original-origin-matcher publication/framed-submission-origin-matches?)
(def ^:private original-observation-reader publication/read-framed-submission-observation!)
(def ^:private original-acquisition-comparison provider/compare-with-issued-retained-acquisition!)
(def ^:private original-activated-comparison compare-activated*)

(defn- local-boundaries-current? []
  (and (identical? original-origin-matcher publication/framed-submission-origin-matches?)
       (identical? original-observation-reader publication/read-framed-submission-observation!)
       (identical? original-acquisition-comparison provider/compare-with-issued-retained-acquisition!)
       (identical? original-activated-comparison compare-activated*)))

(defn compare-local-activated!
  "Require the exact issued original observation and retained acquisition
   to originate at the supplied node. Compare the activated recovery against
   the entire issued sample while independently reobserving its contents.
   Preserve the historical content-only APIs for reopened stores. Local
   matching does not authenticate writer provenance or establish continuity."
  [node tables admission source outcome-declarations steps observation acquisition interleaved?]
  (let [base {:local-acquisition-profile local-acquisition-profile
              :authority :none :certificate :not-issued
              :observation-storage-identity :not-established
              :authorized-historical-lineage :not-established}
        fail #(assoc base :status %1 :reason %2)]
    (cond
      (not (boolean? interleaved?)) (fail :unsupported :invalid-local-recovery-mode)
      (not (local-boundaries-current?))
      (fail :unsupported :local-recovery-boundary-replaced)
      (not (original-origin-matcher admission observation node))
      (fail :unsupported :submission-origin-mismatch)
      :else
      (let [r (original-acquisition-comparison
               node acquisition
               #(original-activated-comparison node tables admission source outcome-declarations steps observation interleaved?))]
        (cond
          (not (local-boundaries-current?)) (fail :unsupported :local-recovery-boundary-replaced)
          (not= :matched (:status r)) (fail (:status r) (:reason r))
          (not (original-origin-matcher admission observation node))
          (fail :unsupported :submission-origin-mismatch)
          (not (and (= (if interleaved? activated-interleaved-profile activated-profile) (:activated-recovery-profile r))
                    (= :matched-local-activated-retained-root (:activation-root-binding r))
                    (= :matched-local-observed-retained-steps (:submission-recovery-linkage r))))
          (fail :unsupported :invalid-local-recovery-comparison)
          :else (merge r base {:local-acquisition-binding :matched-issued-origin-and-retained-sample}))))))
