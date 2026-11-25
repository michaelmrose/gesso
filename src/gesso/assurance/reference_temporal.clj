(ns gesso.assurance.reference-temporal
  "Independent finite bitemporal replay of selected document puts.
   System times are supplied as inputs, never inferred from provider results."
  (:require [gesso.assurance.reference-transaction :as current])
  (:import (java.time Instant ZonedDateTime)))

(def profile :selected-bitemporal-transaction-v6)

(defn- instant? [x]
  (and (instance? Instant x) (zero? (mod (.getNano ^Instant x) 1000))))

(defn- before? [^Instant a ^Instant b] (.isBefore a b))

(defn- reject! [reason]
  (throw (ex-info "Unsupported temporal reference input" {::unsupported true :reason reason})))

(defn- before-end? [instant end]
  ;; Internal nil represents an unbounded exclusive end, not a SQL null.
  (or (nil? end) (before? instant end)))

;; Exact transaction-clock guards emitted by the outbox lease worker.
(def ^:private clock-guards
  {"ASSERT CURRENT_TIMESTAMP < ?" :future
   "ASSERT CURRENT_TIMESTAMP + INTERVAL '5' MINUTE >= ?" :within-five-minutes})

(defn- clock-instant? [x]
  ;; Conservative portable timestamp domain; exclude precision loss and
  ;; extreme JVM instants before doing arithmetic or comparing deadlines.
  (and (instant? x)
       (not (before? x (Instant/parse "0001-01-01T00:00:00Z")))
       (before? x (Instant/parse "9999-01-01T00:00:00Z"))))

(defn- check-clock-guard! [system-time op]
  (when-not (and (= 2 (count op)) (clock-instant? system-time)
                 (clock-instant? (second op)))
    (reject! :unsupported-temporal-clock-guard)))

(defn- clock-matches? [system-time [sql deadline]]
  (case (get clock-guards sql)
    :future (before? system-time deadline)
    :within-five-minutes (not (before? (.plusSeconds ^Instant system-time 300) deadline))))

(def ^:private version-guard
  "ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ?)")

(defn- version-instant [x]
  (cond
    (instance? Instant x) x
    (instance? ZonedDateTime x) (.toInstant ^ZonedDateTime x)))

(defn- check-version-guard! [system-time op]
  (when-not (and (= 3 (count op)) (uuid? (second op))
                 (clock-instant? system-time)
                 (clock-instant? (version-instant (nth op 2))))
    (reject! :unsupported-temporal-version-guard)))

(defn- version-matches! [working system-time [_ id version]]
  (let [rows (get-in working [:segments :gesso_live_outbox])
        visible (fn [intervals]
                  (first (filter (fn [[a b _ _]]
                                   (and (not (before? system-time a))
                                        (before-end? system-time b))) intervals)))]
    (when (some (fn [[k intervals]] (and (not (uuid? k)) (visible intervals))) rows)
      (reject! :unsupported-temporal-version-id-domain))
    (= (version-instant version) (nth (visible (get rows id)) 3 nil))))

(defn- normalize-op! [system-time op]
  (when-not (and (vector? op) (nil? (meta op)))
    (reject! :unsupported-temporal-operation))
  (if (string? (first op))
    (do (cond
          (contains? clock-guards (first op)) (check-clock-guard! system-time op)
          (= version-guard (first op)) (check-version-guard! system-time op)
          :else (when-not (= :predicted (:status (current/execute {} [op])))
                  (reject! :unsupported-temporal-assertion)))
        op)
    (let [[kind raw-opts & docs] op
          opts (if (keyword? raw-opts) {:into raw-opts} raw-opts)]
      (when-not (and (= :put-docs kind) (map? opts) (nil? (meta opts))
                     (contains? opts :into)
                     (every? #{:into :valid-from :valid-to} (keys opts)))
        (reject! :unsupported-temporal-put))
      (let [table (:into opts)
            from (get opts :valid-from system-time)
            to (:valid-to opts)]
        (when-not (and (instant? from)
                       (or (not (contains? opts :valid-to)) (instant? to))
                       (before-end? from to)
                       (= :predicted (:status (current/execute {} [(into [:put-docs table] docs)]))))
          (reject! :unsupported-temporal-put))
        (into [:put-docs {:into table :valid-from from :valid-to to}] docs)))))

(defn- rectangle [table [from to doc origin] end]
  {:table table :document doc :valid-from from :valid-to to
   :system-from origin :system-to end})

(defn- earlier-end [a b]
  (cond (nil? a) b (nil? b) a (before? a b) a :else b))

(defn- overlay [segments table from to doc system-time]
  ;; Preserve issuing system time on unaffected pieces. Close only the
  ;; intersection that is overwritten, and never emit a zero-system-time
  ;; rectangle for an earlier put in the same transaction.
  (let [{:keys [active closed]}
        (reduce (fn [acc [a b old origin :as segment]]
                  (if (or (not (before-end? a to)) (not (before-end? from b)))
                    (update acc :active conj segment)
                    (cond-> acc
                      (before? a from) (update :active conj [a from old origin])
                      (and to (before-end? to b)) (update :active conj [to b old origin])
                      (before? origin system-time)
                      (update :closed conj
                              (rectangle table [(if (before? a from) from a)
                                                (earlier-end b to) old origin]
                                         system-time)))))
                {:active [] :closed []} segments)]
    {:active (vec (sort-by first (conj active [from to doc system-time])))
     :closed closed}))

(defn- apply-put [state system-time [_ opts & docs]]
  (reduce (fn [s doc]
            (let [table (:into opts)
                  path [:segments table (:xt/id doc)]
                  {:keys [active closed]} (overlay (get-in s path) table
                                                   (:valid-from opts) (:valid-to opts)
                                                   doc system-time)]
              (-> s (assoc-in path active) (update :closed into closed))))
          state docs))

(defn- active-rectangles [segments]
  (vec (for [[table rows] segments [_ intervals] rows segment intervals]
         (rectangle table segment nil))))

(defn- project-visible [segments valid-time]
  (into {} (keep (fn [[table rows]]
                   (let [visible (into {} (keep (fn [[id intervals]]
                                                 (when-let [[_ _ doc]
                                                            (first (filter (fn [[a b _]]
                                                                             (and (not (before? valid-time a))
                                                                                  (before-end? valid-time b))) intervals))]
                                                   [id doc]))) rows)]
                     (when (seq visible) [table visible])))) segments))

(defn- execute-transaction! [prior system-time ops]
  ;; Assertions without explicit temporal clauses read tentative documents
  ;; at the supplied transaction time, not all active intervals.
  (loop [working prior i 0]
    (if (= i (count ops))
      {:state working :committed? true}
      (let [op (nth ops i)]
        (if (= :put-docs (first op))
          (recur (apply-put working system-time op) (inc i))
          (let [matches? (cond
                           (contains? clock-guards (first op)) (clock-matches? system-time op)
                           (= version-guard (first op)) (version-matches! working system-time op)
                           :else (let [result (current/execute (project-visible (:segments working) system-time) [op])]
                             (when-not (= :predicted (:status result))
                               (reject! :unsupported-temporal-assertion-domain))
                             (:committed? result)))]
            (if matches?
              (recur working (inc i))
              {:state prior :committed? false :failed-assertion-index i})))))))

(defn replay
  "Replay from empty state. Each transaction is {:system-time Instant :ops
   vector}. All system times strictly increase. Valid intervals are
   half-open and microsecond-aligned, with omitted starts at the supplied
   system time and omitted ends unbounded. Abort retains the prior snapshot.
   Selected assertions read tentative documents at the transaction system
   time (no current-time override), including selected worker clock and row-version guards.
   Validate all operation shapes before
   replay. Rectangles retain the issuing system time and exclusive supersession
   time. No provider observations, implicit clock or authority are admitted."
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
          (reject! :unsupported-temporal-transaction)))
      (let [normalized (mapv (fn [{:keys [system-time ops] :as tx}]
                               (assoc tx :ops (mapv #(normalize-op! system-time %) ops)))
                             transactions)]
        (let [{:keys [history segments closed]}
              (reduce (fn [{:keys [history] :as prior} {:keys [system-time ops]}]
                        (let [{:keys [state committed? failed-assertion-index]}
                              (execute-transaction! prior system-time ops)]
                          (assoc state :history
                                 (conj history (cond-> {:system-time system-time
                                                        :committed? committed?
                                                        :segments (:segments state)}
                                                 (not committed?)
                                                 (assoc :failed-assertion-index failed-assertion-index))))))
                      {:history [] :segments {} :closed []} normalized)]
          (assoc report :status :predicted :history history
                 :rectangles (into closed (active-rectangles segments)))))
      (catch clojure.lang.ExceptionInfo e
        (if (::unsupported (ex-data e))
          (assoc report :status :unsupported :reason (:reason (ex-data e)))
          (throw e))))))

(defn visible-at
  "Project complete visible document maps at a system and valid instant."
  [report system-time valid-time]
  (when (and (= profile (:profile report)) (= :predicted (:status report))
             (instant? system-time) (instant? valid-time))
    (project-visible (:segments (last (take-while #(not (before? system-time (:system-time %)))
                                                (:history report))))
                     valid-time)))
