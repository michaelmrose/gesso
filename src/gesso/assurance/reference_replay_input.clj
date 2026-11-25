(ns gesso.assurance.reference-replay-input
  "Derive selected temporal replay inputs from independent retained bytes."
  (:require [clojure.string :as str]
            [gesso.assurance.reference-arrow-operations :as arrow]
            [gesso.assurance.reference-clock :as clock]
            [gesso.assurance.reference-source-payload :as payload]
            [gesso.assurance.reference-temporal :as temporal])
  (:import (java.time Instant ZoneId ZonedDateTime)))

(def profile :selected-retained-replay-input-v5)
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

(defn- project-state-assertion [query args]
  ;; Exact writer forms only: no general numbered-parameter rewrite.
  (when (and (string? query)
             (or (and (= 2 (count args))
                      (re-matches #"ASSERT EXISTS \(SELECT 1 FROM \"[a-z][a-z0-9_]*\" WHERE _id = \$1 AND \"[a-z][a-z0-9]*\" = \$2\)" query))
                 (and (= 1 (count args))
                      (re-matches #"ASSERT \$1 = \(SELECT COUNT\(\*\) FROM \"[a-z][a-z0-9_]*\"\)" query))
                 (and (= 2 (count args))
                      (re-matches #"ASSERT \$1 = \(SELECT COUNT\(\*\) FROM \"[a-z][a-z0-9_]*\" WHERE _id = \$2\)" query))
                 (and (empty? args)
                      (re-matches #"ASSERT (?:0|[1-9][0-9]{0,4}) = \(SELECT COUNT\(\*\) FROM \"[a-z][a-z0-9_]*\"\)" query))
                 (and (= 1 (count args))
                      (re-matches #"ASSERT (?:0|[1-9][0-9]{0,4}) = \(SELECT COUNT\(\*\) FROM \"[a-z][a-z0-9_]*\" WHERE _id = \$1\)" query))))
    (into [(str/replace query #"\$[12]" "?")] args)))

(defn- project-operation [clocked? {:keys [kind query args table documents bounds] :as op}]
  (case kind
    :sql (when (and (= #{:kind :query :args} (set (keys op))) (vector? args))
           (cond
             (and (#{"ASSERT TRUE" "ASSERT FALSE"} query) (empty? args)) [query]
             (and (= "ASSERT $1 = $2" query) (= 2 (count args))) (into ["ASSERT ? = ?"] args)
             (not clocked?) (project-state-assertion query args)))
    :put-docs (when (and (= #{:kind :table :documents :iids :bounds} (set (keys op)))
                         (keyword? table) (= "public" (namespace table)) (= {} bounds)
                         (vector? documents) (seq documents)
                         (every? map? documents))
                (project-put table documents))
    nil))

(def clock-profile clock/clock-profile)
(def clocked-input-profile clock/clocked-input-profile)
(def resolve-retained-clock clock/resolve-retained-clock)

(defn- decode-input* [tx-id bytes frame-micros prior-time clocked?]
  (let [report (if clocked? (assoc base :profile clocked-input-profile) base)
        fail #(assoc report :status :unsupported :reason %)]
    (if-not (and (integer? tx-id) (<= 0 tx-id (dec Long/MAX_VALUE)))
      (fail :invalid-replay-transaction-id)
      (let [r (payload/decode-payload bytes)
            {:keys [kind system-time default-tz tx-ops] :as message} (:message r)]
        (cond
          (not= :decoded (:status r)) (fail :unsupported-replay-envelope)
          (not (and (= :transaction kind)
                    (#{(ZoneId/of "Z") (ZoneId/of "UTC") (ZoneId/of "Etc/UTC")} default-tz)
                    (or (not (contains? message :user)) (= "xtdb" (:user message)))
                    (not (contains? message :user-metadata))))
          (fail :unsupported-replay-envelope-context)
          :else
          (let [clock (when clocked? (resolve-retained-clock frame-micros system-time prior-time))
                resolved (if clocked? (:system-time clock)
                             (if (nil? system-time) (clock/implicit-system-time frame-micros prior-time) system-time))]
            (cond
              (and clocked? (not= :resolved (:status clock))) (fail (:reason clock))
              (not (instance? Instant resolved)) (fail :unsupported-replay-envelope-context)
              :else
              (let [decoded (arrow/decode-bounded-operations tx-ops)
                    ops (:operations decoded)]
                (if-not (and (= :decoded (:status decoded))
                             (= arrow/bounded-operation-profile (:profile decoded))
                             (vector? ops) (<= 1 (count ops) 256)
                             (= (if (some #(= :put-docs (:kind %)) ops) :matched :not-applicable)
                                (:iid-correspondence decoded)))
                  (fail :unsupported-replay-operation-bytes)
                  (let [projected (mapv #(project-operation clocked? %) ops)
                        tx {:system-time resolved :ops projected}]
                    (if (and (every? some? projected)
                             (= :predicted (:status (temporal/replay [tx]))))
                      (cond-> (assoc report :status :decoded :transaction (assoc tx :tx-id tx-id))
                        clocked? (assoc :clock clock :requested-system-time system-time
                                        :frame-micros frame-micros))
                      (fail :unsupported-replay-operation))))))))))))

(defn decode-replay-input
  "Project selected retained operations with explicit time precedence.
   Includes exact quoted-table row-read and row-count assertions; the
   history must still qualify for strictly increasing temporal replay.
   The four-argument form resolves implicit times from the frame and prior
   time; the two-argument form requires explicit time. Clock rejection
   and equal-time history qualification are outside this legacy profile."
  ([tx-id bytes] (decode-replay-input tx-id bytes nil nil))
  ([tx-id bytes frame-micros prior-time]
   (decode-input* tx-id bytes frame-micros prior-time false)))

(defn decode-clocked-replay-input
  "Project selected attempted operations and independently resolve their
   clock decision from framed microseconds and the preceding completed time.
   Keep the requested time distinct from the assigned time. A clock-rejected
   attempt retains its actual operations; callers must not execute them.
   Even rejected attempts must pass the selected operation grammar at the
   assigned time. Internal sensitive sample only, not authenticated source
   identity, history qualification or an execution prediction."
  [tx-id bytes frame-micros prior-time]
  (decode-input* tx-id bytes frame-micros prior-time true))
