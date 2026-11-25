(ns gesso.assurance.reference-lifecycle
  "Selected publication and worker traces, predicted without provider reads."
  (:require [clojure.string :as str]
            [gesso.assurance.reference-temporal :as temporal])
  (:import (java.time Instant)))

(def profile :selected-publication-lifecycle-v1)
(def ^:private base-report {:profile profile :authority :none :certificate :not-issued})
(def ^:private unsupported (assoc base-report :status :unsupported))

(defn- plain-map? [x] (and (map? x) (nil? (meta x))))
(defn- plain-vector? [x] (and (vector? x) (nil? (meta x))))
(defn- text-id? [x] (and (string? x) (not (str/blank? x))))
(defn- time? [x] (and (instance? Instant x) (zero? (mod (.getNano ^Instant x) 1000))))

(defn- publication-ops [spec changes writes]
  ;; Explicit UUIDs and string subject/command IDs. Domain puts are supplied,
  ;; not derived or authorized here. Exclude framework tables from those puts.
  (let [{:keys [intent-ids source-id operation purpose command-id]} spec]
    (when (and (plain-map? spec)
               (every? #{:intent-ids :source-id :operation :purpose :command-id} (keys spec))
               (text-id? source-id) (qualified-keyword? operation) (qualified-keyword? purpose)
               (or (not (contains? spec :command-id)) (text-id? command-id))
               (plain-vector? intent-ids) (<= 1 (count intent-ids) 64)
               (every? uuid? intent-ids) (= (count intent-ids) (count (distinct intent-ids)))
               (plain-vector? changes) (= (count intent-ids) (count changes))
               (every? #(and (plain-map? %) (every? #{:topic :id :change/kind} (keys %))
                             (keyword? (:topic %)) (text-id? (:id %))
                             (or (not (contains? % :change/kind)) (keyword? (:change/kind %)))) changes)
               (plain-vector? writes) (<= 1 (count writes) 64)
               (every? #(and (plain-vector? %) (= :put-docs (first %))
                             (simple-keyword? (second %))
                             (not (str/starts-with? (name (second %)) "gesso"))) writes))
      (let [docs (mapv (fn [id change]
                         (cond-> {:xt/id id
                                  :gesso.live.outbox/type :gesso.live.outbox/publication-intent
                                  :gesso.live.outbox/version 1
                                  :gesso.live.outbox/status :pending
                                  :gesso.live.outbox/source-id source-id
                                  :gesso.live.outbox/operation operation
                                  :gesso.live.outbox/purpose purpose
                                  :gesso.live.outbox/topic (:topic change)
                                  :gesso.live.outbox/subject-id (:id change)}
                           (:change/kind change) (assoc :gesso.live.outbox/change-kind (:change/kind change))
                           command-id (assoc :gesso.live.outbox/command-id command-id)))
                       intent-ids changes)
            guards (mapv (fn [id] ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ?)" id]) intent-ids)]
        (into (into guards writes) [(into [:put-docs :gesso_live_outbox] docs)])))))

(defn- predict-step [prefix {:keys [kind system-time observed-system observed-valid spec changes writes claim submission] :as step}]
  (case kind
    :publish (when (= #{:kind :system-time :spec :changes :writes} (set (keys step)))
               (when-let [ops (publication-ops spec changes writes)]
                 (let [r (temporal/replay (conj prefix {:system-time system-time :ops ops}))]
                   (when (= :predicted (:status r))
                     {:status :predicted :ops ops :replay r
                      :outcome (if (:committed? (peek (:history r))) :published :contended)}))))
    (:acquire :reclaim) (when (= #{:kind :system-time :observed-system :observed-valid :claim} (set (keys step)))
                          ((if (= :acquire kind) temporal/predict-pending-acquisition
                               temporal/predict-expired-reclamation)
                           prefix observed-system observed-valid system-time claim))
    :acknowledge (when (= #{:kind :system-time :observed-system :observed-valid :submission} (set (keys step)))
                   (temporal/predict-acknowledgment prefix observed-system observed-valid system-time submission))
    nil))

(defn predict-lifecycle
  "Replay selected publication and worker calls from empty state. Explicit
   strictly increasing call times and pinned observations are inputs.
   Only predicted submissions extend the transaction prefix; unavailable
   or rejected calls do not invent ledger entries. Unsupported steps discard
   the entire prediction. No authentication, delivery, manifest admission
   or arbitrary writer closure is established."
  [steps]
  (if-not (and (plain-vector? steps) (<= 1 (count steps) 128)
               (= :publish (:kind (first steps)))
               (every? #(and (plain-map? %) (time? (:system-time %))) steps)
               (every? (fn [[a b]] (.isBefore ^Instant (:system-time a) ^Instant (:system-time b)))
                       (partition 2 1 steps)))
    unsupported
    (loop [remaining (seq steps) prefix [] trace [] r nil]
      (if-let [step (first remaining)]
        (let [p (predict-step prefix step)]
          (if (= :predicted (:status p))
            (recur (next remaining)
                   (cond-> prefix (seq (:ops p)) (conj {:system-time (:system-time step) :ops (:ops p)}))
                   (conj trace (select-keys p [:outcome :reason :ops])) (:replay p))
            unsupported))
        (assoc base-report :status :predicted :trace trace :transactions prefix :replay r)))))
