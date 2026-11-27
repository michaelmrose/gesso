(ns gesso.assurance.reference-mapped-worker-submission
  "Check the exact installed worker capture against selected retained recovery."
  (:require [gesso.assurance.reference-mapped-observed-recovery :as recovery]
            [gesso.assurance.reference-provider :as provider]
            [gesso.assurance.reference-mapped-worker-envelope :as envelope]
            [gesso.live.outbox.host :as host]))

(def profile :selected-issued-worker-submission-correspondence-v2)

(def ^:private original-host-matcher host/worker-outcome-capture-matches?)
(def ^:private original-recovery-comparison recovery/compare-local-activated!)
(def ^:private original-acquisition-bracket provider/compare-with-issued-retained-acquisition!)
(def ^:private original-envelope-check envelope/compare-issued-selection!)
(def ^:private original-envelope-boundaries envelope/boundaries-current?)

(defn- boundaries-current? []
  (and (identical? original-host-matcher host/worker-outcome-capture-matches?)
       (identical? original-recovery-comparison recovery/compare-local-activated!)
       (identical? original-acquisition-bracket provider/compare-with-issued-retained-acquisition!)
       (identical? original-envelope-check envelope/compare-issued-selection!)
       (identical? original-envelope-boundaries envelope/boundaries-current?)
       (original-envelope-boundaries)))

(defn- report [status reason]
  (cond-> {:profile profile :status status :authority :none :certificate :not-issued
           :worker-authentication :not-established :dispatch-authentication :not-established
           :provider-refinement :not-established :complete-writer-coverage :not-established}
    reason (assoc :reason reason)))

(defn compare-issued!
  "Independently recheck the selected local activated recovery under its
   issued retained acquisition, then match ALL intervening maintenance
   calls in the exact host-issued capture, in order. Check each returned
   transaction key (ID and system time), complete decoded operation sequence
   and retained committed outcome. The new lease deadline is compared at
   the selected provider's microsecond precision; no other field is relaxed.
   Reject copied captures, extra/missing
   calls and mismatched payloads. The entire sample is freshly bracketed;
   neither a caller-supplied comparison nor a supplied sample can qualify.
   This is local correspondence only, not authentication or proof discharge.
   The returned origin summaries are sensitive internal evidence."
  [system tables admission source outcome-declarations steps observation retained-acquisition capture]
  (let [node (:xtdb/connectable system)]
    (cond
      (not (boundaries-current?)) (report :unsupported :worker-comparison-boundary-replaced)
      (not (original-host-matcher system capture)) (report :unsupported :unissued-worker-capture)
      :else
      (let [result (original-acquisition-bracket
                    node retained-acquisition
                    (fn []
                      (let [r (original-recovery-comparison node tables admission source outcome-declarations
                                                            steps observation retained-acquisition true)]
                        (cond
                          (not= :matched (:status r)) (report (:status r) (:reason r))
                          (not (and (= recovery/activated-interleaved-profile (:activated-recovery-profile r))
                                    (= :matched-issued-origin-and-retained-sample (:local-acquisition-binding r))
                                    (= :matched-local-observed-retained-steps (:submission-recovery-linkage r))))
                          (report :unsupported :invalid-worker-recovery-comparison)
                          :else (let [checked (original-envelope-check system retained-acquisition (:source-id (first steps))
                                                                (:intervening-maintenance r) capture)]
                                  (if (= :matched (:status checked))
                                    (assoc checked :selected-tx-ids (:selected-tx-ids r))
                                    checked))))))]
        (cond
          (not (boundaries-current?)) (report :unsupported :worker-comparison-boundary-replaced)
          (not (original-host-matcher system capture)) (report :unsupported :unissued-worker-capture)
          (not= :matched (:status result)) (report (:status result) (:reason result))
          :else result)))))
