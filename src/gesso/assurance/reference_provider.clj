(ns gesso.assurance.reference-provider
  "Bounded comparison of independently expected execution and supplied observations."
  (:require [gesso.assurance.reference-temporal :as temporal]
            [gesso.assurance.xtdb-envelope :as envelope])
  (:import (java.time Instant)))

(def profile :selected-provider-execution-comparison-v1)
(def ^:private base {:profile profile :authority :none :certificate :not-issued
                     :provider-refinement :not-established})
(def ^:private unsupported (assoc base :status :unsupported))

(defn- canonical-rectangles [rectangles]
  ;; Adjacent valid-axis partitions may differ without changing history.
  ;; Do not erase overlapping multiplicity or merge system-time origins.
  (frequencies
   (mapcat (fn [[_ rs]]
             (reduce (fn [acc r]
                       (if (and (seq acc) (= (:valid-to (peek acc)) (:valid-from r)))
                         (conj (pop acc) (assoc (peek acc) :valid-to (:valid-to r)))
                         (conj acc r)))
                     [] (sort-by :valid-from rs)))
           (group-by #(dissoc % :valid-from :valid-to) rectangles))))

(defn- rectangle? [r tables]
  (and (map? r) (= #{:table :document :valid-from :valid-to :system-from :system-to} (set (keys r)))
       (contains? tables (:table r)) (map? (:document r)) (contains? (:document r) :xt/id)
       (every? (fn [[a b]] (and (instance? Instant a)
                                (or (nil? b) (and (instance? Instant b) (.isBefore ^Instant a ^Instant b)))))
               [[(:valid-from r) (:valid-to r)] [(:system-from r) (:system-to r)]])))

(defn compare-execution
  "Compare a supplied complete bounded transaction sequence with observed
   ledger rows, per-transaction envelope checks and all-time rectangles for
   the explicitly selected tables. Observation acquisition/completeness and
   correct envelope-check invocation remain premises. No provider data is
   used to predict the outcome. This is not a live audit or a certificate."
  [transactions tables {:keys [ledger envelopes rectangles] :as observed}]
  (let [prediction (temporal/replay transactions)
        history (:history prediction)]
    (if-not (and (= :predicted (:status prediction))
                 (set? tables) (seq tables) (every? simple-keyword? tables)
                 (every? tables (map :table (:rectangles prediction)))
                 (every? tables (for [tx transactions op (:ops tx) :when (= :put-docs (first op))]
                                  (if (map? (second op)) (:into (second op)) (second op))))
                 (map? observed) (= #{:ledger :envelopes :rectangles} (set (keys observed)))
                 (vector? ledger) (<= (count ledger) 256)
                 (every? #(and (map? %) (= #{:tx-id :system-time :committed?} (set (keys %)))
                               (integer? (:tx-id %)) (<= 0 (:tx-id %))
                               (instance? Instant (:system-time %)) (boolean? (:committed? %))) ledger)
                 (every? (fn [[a b]] (< (:tx-id a) (:tx-id b))) (partition 2 1 ledger))
                 (vector? envelopes) (= (count ledger) (count envelopes))
                 (every? #(and (map? %) (= envelope/profile (:profile %))
                               (#{:matched :mismatch :unsupported :unavailable} (:status %))) envelopes)
                 (vector? rectangles) (<= (count rectangles) 65536)
                 (every? #(rectangle? % tables) rectangles))
      unsupported
      (let [ledger-match? (= (mapv #(select-keys % [:system-time :committed?]) history)
                             (mapv #(dissoc % :tx-id) ledger))
            envelopes-match? (every? (fn [[row e]]
                                       (and (= :matched (:status e)) (= (:tx-id row) (:tx-id e))
                                            (= (:committed? row) (:committed? e))))
                                     (map vector ledger envelopes))
            history-match? (= (canonical-rectangles (:rectangles prediction))
                              (canonical-rectangles rectangles))
            checks {:ledger ledger-match? :retained-envelopes envelopes-match? :selected-history history-match?}
            status (cond (or (not ledger-match?) (not history-match?) (some #(= :mismatch (:status %)) envelopes)) :mismatch
                         (some #(= :unsupported (:status %)) envelopes) :unsupported
                         (some #(= :unavailable (:status %)) envelopes) :unavailable
                         envelopes-match? :matched
                         :else :mismatch)]
        (assoc base :status status :checks checks :transaction-count (count history))))))
