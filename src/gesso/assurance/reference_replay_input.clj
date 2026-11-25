(ns gesso.assurance.reference-replay-input
  "Derive selected temporal replay inputs from independent retained bytes."
  (:require [gesso.assurance.reference-arrow-operations :as arrow]
            [gesso.assurance.reference-source-payload :as payload]
            [gesso.assurance.reference-temporal :as temporal])
  (:import (java.time Instant ZoneId ZonedDateTime)))

(def profile :selected-retained-replay-input-v4)
(def ^:private base {:profile profile :authority :none :certificate :not-issued})

(defn- project-put [table documents]
  ;; Preserve physical group identity even when row intervals differ.
  (let [bound-keys [:xt/valid-from :xt/valid-to]
        row-bounds (mapv #(select-keys % bound-keys) documents)
        bounds (first row-bounds)]
    (when (every? #(and (instance? ZonedDateTime %)
                        (= (ZoneId/of "UTC") (.getZone ^ZonedDateTime %)))
                  (mapcat vals row-bounds))
      (let [table (keyword (name table))]
        (if (apply = row-bounds)
          (let [opts (reduce-kv (fn [opts k v]
                                  (assoc opts (keyword (name k)) (.toInstant ^ZonedDateTime v)))
                                {:into table} bounds)]
            (into [:put-docs (if (empty? bounds) table opts)]
                  (map #(apply dissoc % bound-keys) documents)))
          (into [:put-docs {:into table :row-valid-time? true}]
                (map (fn [doc]
                       (reduce (fn [d k] (if (contains? d k)
                                           (update d k #(.toInstant ^ZonedDateTime %)) d))
                               doc bound-keys)) documents)))))))

(defn- project-operation [{:keys [kind query args table documents bounds] :as op}]
  (case kind
    :sql (when (and (= #{:kind :query :args} (set (keys op))) (vector? args))
           (cond
             (and (#{"ASSERT TRUE" "ASSERT FALSE"} query) (empty? args)) [query]
             (and (= "ASSERT $1 = $2" query) (= 2 (count args))) (into ["ASSERT ? = ?"] args)))
    :put-docs (when (and (= #{:kind :table :documents :iids :bounds} (set (keys op)))
                         (keyword? table) (= "public" (namespace table)) (= {} bounds)
                         (vector? documents) (seq documents)
                         (every? map? documents))
                (project-put table documents))
    nil))

(defn- frame-instant [micros]
  (when (and (integer? micros) (<= Long/MIN_VALUE micros Long/MAX_VALUE))
    (Instant/ofEpochSecond (Math/floorDiv (long micros) 1000000)
                           (* 1000 (Math/floorMod (long micros) 1000000)))))

(defn- implicit-system-time [micros prior]
  ;; The pinned Indexer uses max(log timestamp, latest completed time +
  ;; one microsecond) for implicit times, including after aborted txs.
  (when-let [stamp (frame-instant micros)]
    (cond
      (nil? prior) stamp
      (not (and (instance? Instant prior)
                (zero? (mod (.getNano ^Instant prior) 1000))
                (not (.isBefore ^Instant prior (frame-instant Long/MIN_VALUE)))
                (.isBefore ^Instant prior (frame-instant Long/MAX_VALUE)))) nil
      (.isAfter stamp ^Instant prior) stamp
      :else (.plusNanos ^Instant prior 1000))))

(def clock-profile :xtdb-2.2.0-beta1/selected-retained-clock-decision-v1)

(defn- clock-instant? [x]
  (and (instance? Instant x)
       (zero? (mod (.getNano ^Instant x) 1000))
       (not (.isBefore ^Instant x (frame-instant Long/MIN_VALUE)))
       (not (.isAfter ^Instant x (frame-instant Long/MAX_VALUE)))))

(defn resolve-retained-clock
  "Predict the pinned indexer's clock decision from independently acquired
   frame microseconds, requested envelope time (nil for implicit) and the
   preceding completed transaction time (nil at genesis). Equal explicit
   times are admitted; older requests are rejected before operations run
   and receive the default clock time. Admission here is clock-only, not a
   prediction of commit. No provider results or authenticated provenance."
  [frame-micros requested prior]
  (let [report {:profile clock-profile :authority :none :certificate :not-issued}
        fail #(assoc report :status :unsupported :reason %)]
    (cond
      (nil? (frame-instant frame-micros)) (fail :invalid-clock-frame)
      (not (or (nil? requested) (clock-instant? requested))) (fail :invalid-requested-system-time)
      (not (or (nil? prior) (clock-instant? prior))) (fail :invalid-prior-system-time)
      :else
      (let [older? (and requested prior (.isBefore ^Instant requested ^Instant prior))
            default? (or (nil? requested) older?)
            resolved (if default? (implicit-system-time frame-micros prior) requested)]
        (if-not resolved
          (fail :retained-clock-overflow)
          (assoc report :status :resolved :system-time resolved
                 :decision (if older? :reject-before-execution :execute)
                 :time-source (cond (not default?) :explicit
                                    (= resolved (frame-instant frame-micros)) :frame
                                    :else :smoothed)
                 :reason (if older? :invalid-system-time :clock-admitted)))))))

(defn decode-replay-input
  "Project one independently decoded transaction envelope to the selected
   temporal input domain. Explicit time takes precedence. The four-argument
   form may derive implicit time from the supplied framed microseconds and
   preceding completed transaction time; the two-argument form cannot.
   Require UTC default zone,
   absent or default xtdb user, no user metadata, ordinary puts, row-level valid-time bounds and exact
   literal/equality assertions.
   The physical coordinate is supplied, not authenticated. No provider
   observations or outcomes are consulted. Internal sensitive input only."
  ([tx-id bytes] (decode-replay-input tx-id bytes nil nil))
  ([tx-id bytes frame-micros prior-time]
  (let [fail #(assoc base :status :unsupported :reason %)]
    (if-not (and (integer? tx-id) (<= 0 tx-id (dec Long/MAX_VALUE)))
      (fail :invalid-replay-transaction-id)
      (let [r (payload/decode-payload bytes)
            {:keys [kind system-time default-tz tx-ops] :as message} (:message r)
            system-time (if (nil? system-time) (implicit-system-time frame-micros prior-time) system-time)]
        (cond
          (not= :decoded (:status r)) (fail :unsupported-replay-envelope)
          (not (and (= :transaction kind) (instance? Instant system-time)
                    (#{(ZoneId/of "Z") (ZoneId/of "UTC") (ZoneId/of "Etc/UTC")} default-tz)
                    (or (not (contains? message :user)) (= "xtdb" (:user message)))
                    (not (contains? message :user-metadata))))
          (fail :unsupported-replay-envelope-context)
          :else
          (let [decoded (arrow/decode-bounded-operations tx-ops)
                ops (:operations decoded)]
            (if-not (and (= :decoded (:status decoded))
                         (= arrow/bounded-operation-profile (:profile decoded))
                         (vector? ops) (<= 1 (count ops) 256)
                         (= (if (some #(= :put-docs (:kind %)) ops) :matched :not-applicable)
                            (:iid-correspondence decoded)))
              (fail :unsupported-replay-operation-bytes)
              (let [projected (mapv project-operation ops)
                    tx {:system-time system-time :ops projected}]
                (if (and (every? some? projected)
                         (= :predicted (:status (temporal/replay [tx]))))
                  (assoc base :status :decoded :transaction (assoc tx :tx-id tx-id))
                  (fail :unsupported-replay-operation)))))))))))
