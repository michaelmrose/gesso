(ns gesso.assurance.reference-replay-input
  "Derive selected temporal replay inputs from independent retained bytes."
  (:require [gesso.assurance.reference-arrow-operations :as arrow]
            [gesso.assurance.reference-source-payload :as payload]
            [gesso.assurance.reference-temporal :as temporal])
  (:import (java.time Instant ZoneId ZonedDateTime)))

(def profile :selected-retained-replay-input-v2)
(def ^:private base {:profile profile :authority :none :certificate :not-issued})

(defn- project-put [table documents]
  ;; XTDB writes valid-time options into each row. Keep the physical
  ;; group intact; heterogeneous row bounds are a separate obligation.
  (let [bound-keys [:xt/valid-from :xt/valid-to]
        row-bounds (mapv #(select-keys % bound-keys) documents)
        bounds (first row-bounds)]
    (when (and (apply = row-bounds)
               (every? #(and (instance? ZonedDateTime %)
                             (= (ZoneId/of "UTC") (.getZone ^ZonedDateTime %)))
                       (vals bounds)))
      (let [table (keyword (name table))
            opts (reduce-kv (fn [opts k v]
                              (assoc opts (keyword (name k)) (.toInstant ^ZonedDateTime v)))
                            {:into table} bounds)]
        (into [:put-docs (if (empty? bounds) table opts)]
              (map #(apply dissoc % bound-keys) documents))))))

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

(defn decode-replay-input
  "Project one independently decoded transaction envelope to the selected
   temporal input domain. Require explicit system time, UTC default zone,
   absent or default xtdb user, no user metadata, ordinary puts, uniform row-level valid-time bounds and exact
   literal/equality assertions.
   The physical coordinate is supplied, not authenticated. No provider
   observations or outcomes are consulted. Internal sensitive input only."
  [tx-id bytes]
  (let [fail #(assoc base :status :unsupported :reason %)]
    (if-not (and (integer? tx-id) (<= 0 tx-id (dec Long/MAX_VALUE)))
      (fail :invalid-replay-transaction-id)
      (let [r (payload/decode-payload bytes)
            {:keys [kind system-time default-tz tx-ops] :as message} (:message r)]
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
                  (fail :unsupported-replay-operation))))))))))
