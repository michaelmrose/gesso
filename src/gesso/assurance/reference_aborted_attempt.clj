(ns gesso.assurance.reference-aborted-attempt
  "Selected competing-abort diagnostic for the pinned XTDB provider.
   Supplied observations establish neither issuance nor window completeness."
  (:import [java.time Instant]
           [java.time.temporal ChronoUnit]))

(def profile :xtdb-2.2.0-beta1/selected-aborted-attempt-association-v1)

(defn- coordinates? [key]
  (and (map? key) (= #{:tx-id :system-time} (set (keys key)))
       (integer? (:tx-id key)) (not (neg? (:tx-id key)))
       (instance? Instant (:system-time key))))

(defn compare-candidates
  "Do not resolve a keyless competing history by ordering. Match a supplied
   exception key observation to retained row ID, microsecond row time and
   raw retained error key. Diagnostic only, not an authenticated receipt."
  [capture call-id candidates]
  (let [base {:profile profile :authority :none :certificate :not-issued
              :aborted-receipt-identity :not-established
              :submission-provenance :not-established
              :candidate-window-completeness :not-established}
        report #(assoc base :status %1 :reason %2)
        [started finished :as events] (filterv #(= call-id (:call-id %)) (:events capture))
        anomaly (get-in capture [:xtdb-anomalies call-id])
        key (:transaction-key anomaly)]
    (cond
      (not (and (uuid? call-id) (= 2 (count events))
                (= :invoked (:phase started)) (= :threw (:phase finished))
                (= :execute-tx (:api started) (:api finished))
                (vector? (:tx-ops started)) (seq (:tx-ops started))
                (empty? (:tx-options started))
                (not (contains? finished :tx-result))))
      (report :unsupported :invalid-aborted-call)

      (not (and (vector? candidates) (every? #(and (map? %) (integer? (:tx-id %))
                                                (not (neg? (:tx-id %)))
                                                (instance? Instant (:system-time %))
                                                (vector? (:ops %)) (boolean? (:committed? %))) candidates)
                (= (count candidates) (count (set (map :tx-id candidates))))))
      (report :unsupported :invalid-aborted-candidate-window)

      :else
      (let [plausible (filterv #(and (false? (:committed? %)) (= (:tx-ops started) (:ops %))) candidates)]
        (cond
          (and (nil? key) (contains? (:transaction-key-fields anomaly) :tx-key))
          (report :unsupported :unusable-exception-key)

          (nil? key)
          (assoc (report (if (> (count plausible) 1) :ambiguous :unresolved)
                         (if (> (count plausible) 1) :competing-aborted-attempts :no-exception-key))
                 :candidate-count (count plausible))

          (not (and (coordinates? key) (= #{:tx-key} (:transaction-key-fields anomaly))
                    (= "xtdb.error.Conflict" (:exception-class finished))
                    (= :xtdb/assert-failed (:code anomaly))
                    (= :cognitect.anomalies/conflict (:category anomaly))
                    (integer? (:failed-operation-index anomaly))
                    (<= 0 (:failed-operation-index anomaly))
                    (< (:failed-operation-index anomaly) (count (:tx-ops started)))))
          (report :unsupported :invalid-exception-key-observation)

          :else
          (let [row (first (filter #(= (:tx-id key) (:tx-id %)) plausible))]
            (if (and row (= (:system-time row) (.truncatedTo ^Instant (:system-time key) ChronoUnit/MICROS))
                     (= key (:error-key row))
                     (= (:code anomaly) (:error-code row))
                     (= (:category anomaly) (:error-category row))
                     (= (:failed-operation-index anomaly) (:failed-operation-index row)))
              (assoc base :status :matched :association :supplied-exception-key-retained-row
                     :candidate-count (count plausible) :selected-tx-id (:tx-id row))
              (report :mismatch :exception-key-retained-row-mismatch))))))))
