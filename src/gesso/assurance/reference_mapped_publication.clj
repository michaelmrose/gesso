(ns gesso.assurance.reference-mapped-publication
  "Source-relative publication derivation, not an installed admission or
   a claim that publication was committed or delivered."
  (:require [clojure.string :as str]
            [gesso.assurance.reference-mapped-writes :as writes]
            [gesso.assurance.reference-transaction :as current]
            [gesso.choreo.identity :as identity]
            [gesso.model.certified-plan :as plan]
            [gesso.model.command-resolution :as resolution])
  (:import (java.nio ByteBuffer)
           (java.security MessageDigest)
           (java.util UUID)))

(def profile :selected-mapped-publication-v2)

(defn- reject! [reason]
  (throw (ex-info "Unsupported mapped publication." {::unsupported true :reason reason})))

(defn- exact-map? [m ks]
  (and (map? m) (nil? (meta m)) (= ks (set (keys m)))))

(defn- bounded-id? [v]
  (and (string? v) (not (str/blank? v)) (<= (count v) 1024)))

(defn- intent-id [source operation principal command-id slot]
  ;; Reimplement the frozen UTF-16 code-unit framing independently of the
  ;; writer. This selected profile has string command/principal coordinates.
  (let [digest (MessageDigest/getInstance "SHA-256")
        fields ["gesso.live.outbox/command-intent-v1" "string" source
                "keyword" "qualified" (namespace operation) (name operation)
                "string" principal "string" command-id "integer" (str slot)]]
    (doseq [^String field fields]
      (let [buf (ByteBuffer/allocate (+ 4 (* 2 (.length field))))]
        (.putInt buf (.length field))
        (dotimes [i (.length field)] (.putChar buf (.charAt field i)))
        (.update digest (.array buf))))
    (let [bytes (.digest digest)]
      (aset-byte bytes 6 (unchecked-byte (bit-or 128 (bit-and 15 (aget bytes 6)))))
      (aset-byte bytes 8 (unchecked-byte (bit-or 128 (bit-and 63 (aget bytes 8)))))
      (let [buf (ByteBuffer/wrap bytes)] (UUID. (.getLong buf) (.getLong buf))))))

(defn- subject! [row field]
  (let [v (get row field)]
    (when-not (or (bounded-id? v) (keyword? v) (instance? Long v) (uuid? v))
      (reject! :unsupported-publication-subject))
    v))

(defn derive-publication!
  "Derive ordered intents and their canonical guard/put fragment from the
   original mapped operation, complete prepared rows, checked forward
   replacements and supplied declaration. A missing declaration is not a
   declared empty publication. Bound to string command/principal IDs and
   at most 128 contributions/128 distinct changes in this selected profile.
   Source ID and declaration provenance remain supplied premises."
  [original targets docs journal spec source-id declarations]
  (let [binding (resolution/require-binding (:binding spec))
        operation (:operation binding)
        alternative (get-in original [:subject :id])
        principal (identity/raw-value (:principal binding))
        command-id (resolution/record-id binding)
        _ (when-not (and (qualified-keyword? alternative)
                         (qualified-keyword? operation)
                         (every? bounded-id? [source-id principal command-id])
                         (map? declarations) (nil? (meta declarations))
                         (contains? declarations alternative))
            (reject! :invalid-publication-source))
        declaration (get declarations alternative)
        puts (writes/derive-puts! original targets docs journal spec)
        mapping (plan/check-target-mapping! original targets)
        addresses (set (filter #(= :document (first %)) (:write-targets mapping)))]
    (if (nil? declaration)
      {:intents [] :guard-ops [] :put-ops []}
      (let [{:keys [purpose contributions]} declaration]
        (when-not (and (exact-map? declaration #{:purpose :contributions})
                       (qualified-keyword? purpose) (vector? contributions)
                       (nil? (meta contributions)) (<= 1 (count contributions) 128)
                       (every? #(and (exact-map? % #{:target :topic :field})
                                     (contains? addresses (:target %))
                                     (keyword? (:topic %)) (keyword? (:field %))) contributions)
                       (= addresses (set (map :target contributions)))
                       (= (count contributions) (count (set (map (juxt :target :topic) contributions))))
                       (every? #(apply = (map :field %)) (vals (group-by :topic contributions))))
          (reject! :invalid-publication-declaration))
        (let [before (into {} (for [[table rows] docs, row rows]
                                [[:document table (:xt/id row)] row]))
              after (into {} (for [[_ table row] puts :when (not= table :gesso_commands)]
                               [[:document table (:xt/id row)] row]))
              changes (vec (distinct
                            (mapcat (fn [{:keys [target topic field]}]
                                      (let [prior (get before target)
                                            next (get after target)
                                            new (subject! next field)]
                                        (map #(vector topic %)
                                             (if prior [(subject! prior field) new] [new]))))
                                    contributions)))
              _ (when-not (<= 1 (count changes) 128) (reject! :unsupported-publication-count))
              intents (mapv (fn [slot [topic id]]
                              {:xt/id (intent-id source-id operation principal command-id slot)
                               :gesso.live.outbox/type :gesso.live.outbox/publication-intent
                               :gesso.live.outbox/version 1
                               :gesso.live.outbox/status :pending
                               :gesso.live.outbox/source-id source-id
                               :gesso.live.outbox/operation operation
                               :gesso.live.outbox/purpose purpose
                               :gesso.live.outbox/topic topic
                               :gesso.live.outbox/subject-id id
                               :gesso.live.outbox/command-id command-id})
                            (range) changes)
              guards (mapv #(vector "ASSERT 0 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ?)" (:xt/id %)) intents)
              put-ops [(into [:put-docs :gesso_live_outbox] intents)]]
          (when-not (= #{:gesso_live_outbox} (current/operation-sequence-tables (into guards put-ops)))
            (reject! :unsupported-publication-operations))
          {:intents intents :guard-ops guards :put-ops put-ops})))))

(defn check-publication!
  "Compare the exact canonical publication fragment, not a whole physical
   transaction. Guard placement relative to other writes and historical
   identity absence require separate retained-envelope composition."
  [original targets docs journal spec source-id declarations fragment]
  (let [base {:profile profile :authority :none :certificate :not-issued
              :provider-refinement :not-established}]
    (try
      (let [{:keys [guard-ops put-ops]} (derive-publication! original targets docs journal spec source-id declarations)]
        (if (and (exact-map? fragment #{:guard-ops :put-ops})
                 (every? #(and (vector? %) (nil? (meta %))) (vals fragment))
                 (or (and (empty? (:guard-ops fragment)) (empty? (:put-ops fragment)))
                     (= #{:gesso_live_outbox}
                        (current/operation-sequence-tables (into (:guard-ops fragment) (:put-ops fragment)))))
                 (= {:guard-ops guard-ops :put-ops put-ops} fragment))
          (assoc base :status :matched :scope :source-relative-publication-fragment)
          (assoc base :status :mismatch :reason :publication-fragment-mismatch)))
      (catch clojure.lang.ExceptionInfo _
        (assoc base :status :unsupported :reason :invalid-publication-source)))))
