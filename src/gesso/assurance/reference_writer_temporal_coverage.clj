(ns gesso.assurance.reference-writer-temporal-coverage
  "Conditional finite diagonal-time coverage of a selected retained history."
  (:require [gesso.assurance.reference-temporal :as temporal])
  (:import (java.time Instant)))

(def profile :conditional-selected-writer-temporal-coverage-v1)
(def ^:private original-replay temporal/replay)

(defn- reject! [reason]
  (throw (ex-info "Unqualified selected temporal coverage" {:reason reason})))

(defn- between? [^Instant from ^Instant to ^Instant point]
  (and point (.isBefore from point) (not (.isBefore to point))))

(defn check-selection!
  "Internal pure check over the exact retained inputs and classified suffix.
   A prior system snapshot must have no protected valid-time boundary in
   (prior transaction time, next transaction time]. Thus its protected row
   image is constant along the intervening diagonal, including the next
   prestate. This conservatively rejects even harmless boundary changes.
   No assertion about arbitrary historical queries, future times, provider
   truth, read-only observation exhaustiveness or supplied report authority."
  [inputs cut partition protected]
  (when-not (identical? original-replay temporal/replay)
    (reject! :temporal-replay-boundary-replaced))
  (when-not (and (vector? inputs) (<= 2 (count inputs))
                 (= (:transaction-ids cut) (mapv :tx-id inputs))
                 (every? integer? (:transaction-ids cut))
                 (apply < (:transaction-ids cut))
                 (= (subvec (:transaction-ids cut) 2) (mapv :tx-id partition))
                 (set? protected) (seq protected)
                 (every? #(instance? Instant (:system-time %)) inputs))
    (reject! :temporal-selection-mismatch))
  (let [replay (original-replay (mapv #(dissoc % :tx-id) inputs))
        history (:history replay)]
    (when-not (and (= :predicted (:status replay))
                   (= (mapv :system-time inputs) (mapv :system-time history))
                   (= (into [true true] (map #(not= :aborted-attempt (:class %))) partition)
                      (mapv :committed? history)))
      (reject! :temporal-replay-or-commitment-mismatch))
    (let [intervals
          (mapv (fn [i step]
                  (let [prior (nth inputs (dec i)) tx (nth inputs i)
                        from (:system-time prior) to (:system-time tx)
                        protected-segments (select-keys (:segments (nth history (dec i))) protected)
                        schema (case (:class step)
                                 :committed-mapped :source-bound-committed-transition
                                 :aborted-attempt :aborted-stutter
                                 :committed-worker :maintenance-stutter
                                 (reject! :unknown-temporal-step-class))]
                    (when (some #(between? from to %)
                                (for [rows (vals protected-segments), segments (vals rows)
                                      [a b] segments, boundary [a b]] boundary))
                      (reject! :intervening-protected-valid-time-boundary))
                    {:from-tx-id (:tx-id prior) :to-tx-id (:tx-id tx)
                     :from from :to to :schema schema
                     :intervening-protected-boundaries :none}))
                (range 2 (count inputs)) partition)]
      (when-not (identical? original-replay temporal/replay)
        (reject! :temporal-replay-boundary-replaced))
      {:profile profile :status :conditional-selected-diagonal-coverage
       :scope :initialization-through-last-selected-transaction
       :intervals intervals :protected-tables protected
       :replay-coverage :not-established :universal-refinement :not-established
       :proof-discharge :not-performed :authority :none :certificate :not-issued
       :remaining-conditions #{:faithful-retained-temporal-replay-and-provider-images
                               :complete-retained-transaction-and-clock-history
                               :faithful-classified-transition-schema-correspondence
                               :complete-read-only-and-temporal-observation-coverage}})))
