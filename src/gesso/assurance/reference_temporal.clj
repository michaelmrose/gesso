(ns gesso.assurance.reference-temporal
  "Independent finite bitemporal replay of explicit bounded document puts.
   System times are supplied as inputs, never inferred from provider results."
  (:require [gesso.assurance.reference-transaction :as current])
  (:import (java.time Instant)))

(def profile :selected-bitemporal-transaction-v1)

(defn- instant? [x]
  (and (instance? Instant x) (zero? (mod (.getNano ^Instant x) 1000))))

(defn- before? [^Instant a ^Instant b] (.isBefore a b))

(defn- reject! [reason]
  (throw (ex-info "Unsupported temporal reference input" {::unsupported true :reason reason})))

(defn- check-op! [op]
  (when-not (and (vector? op) (nil? (meta op)))
    (reject! :unsupported-temporal-operation))
  (when-not (or (= op ["ASSERT TRUE"]) (= op ["ASSERT FALSE"]))
    (let [[kind opts & docs] op
          {:keys [into valid-from valid-to]} opts]
      (when-not (and (= :put-docs kind) (map? opts) (nil? (meta opts))
                     (= #{:into :valid-from :valid-to} (set (keys opts)))
                     (instant? valid-from) (instant? valid-to)
                     (before? valid-from valid-to)
                     (= :predicted (:status (current/execute {} [(clojure.core/into [:put-docs into] docs)]))))
        (reject! :unsupported-temporal-put))))
  op)

(defn- overlay [segments from to doc]
  ;; Each segment is [inclusive-from exclusive-to complete-document].
  (->> segments
       (mapcat (fn [[a b old :as segment]]
                 (if (or (not (before? a to)) (not (before? from b)))
                   [segment]
                   (cond-> []
                     (before? a from) (conj [a from old])
                     (before? to b) (conj [to b old])))))
       (cons [from to doc])
       (sort-by first) vec))

(defn- apply-put [state [_ opts & docs]]
  (reduce (fn [s doc]
            (update-in s [(:into opts) (:xt/id doc)] overlay
                       (:valid-from opts) (:valid-to opts) doc))
          state docs))

(defn replay
  "Replay from empty state. Each transaction is {:system-time Instant :ops
   vector}. All system times strictly increase; explicit valid intervals
   are finite, half-open and microsecond-aligned. Only bounded puts and
   literal assertions are supported. Abort retains the previous snapshot.
   No provider observations, implicit clock or authority are admitted."
  [transactions]
  (let [report {:profile profile :authority :none :certificate :not-issued}]
    (try
      (when-not (and (vector? transactions) (nil? (meta transactions))
                     (<= 1 (count transactions) 256))
        (reject! :unsupported-temporal-sequence))
      (doseq [[i {:keys [system-time ops] :as tx}] (map-indexed vector transactions)]
        (when-not (and (map? tx) (nil? (meta tx))
                       (= #{:system-time :ops} (set (keys tx)))
                       (instant? system-time)
                       (or (zero? i) (before? (:system-time (nth transactions (dec i))) system-time))
                       (vector? ops) (nil? (meta ops)) (<= 1 (count ops) 256))
          (reject! :unsupported-temporal-transaction))
        (doseq [op ops] (check-op! op)))
      (assoc report :status :predicted
             :history (reduce (fn [history {:keys [system-time ops]}]
                                (let [prior (or (:segments (peek history)) {})
                                      aborted? (some #{["ASSERT FALSE"]} ops)
                                      next-state (if aborted? prior
                                                     (reduce (fn [s op]
                                                               (if (= :put-docs (first op)) (apply-put s op) s))
                                                             prior ops))]
                                  (conj history {:system-time system-time
                                                 :committed? (not aborted?) :segments next-state})))
                              [] transactions))
      (catch clojure.lang.ExceptionInfo e
        (if (::unsupported (ex-data e))
          (assoc report :status :unsupported :reason (:reason (ex-data e)))
          (throw e))))))

(defn visible-at
  "Project complete visible document maps at a system and valid instant.
   Query only a successful replay; no interval metadata is projected."
  [report system-time valid-time]
  (when (and (= profile (:profile report)) (= :predicted (:status report))
             (instant? system-time) (instant? valid-time))
    (let [segments (:segments (last (take-while #(not (before? system-time (:system-time %)))
                                               (:history report))))]
      (into {} (keep (fn [[table rows]]
                       (let [visible (into {} (keep (fn [[id intervals]]
                                                     (when-let [[_ _ doc]
                                                                (first (filter (fn [[a b _]]
                                                                                 (and (not (before? valid-time a))
                                                                                      (before? valid-time b))) intervals))]
                                                       [id doc]))) rows)]
                         (when (seq visible) [table visible])))) segments))))
