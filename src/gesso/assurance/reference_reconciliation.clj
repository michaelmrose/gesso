(ns gesso.assurance.reference-reconciliation
  "Selected read-only reconciliation of completed and in-flight worker reports."
  (:require [clojure.string :as str]
            [gesso.assurance.reference-temporal :as temporal])
  (:import (java.time ZonedDateTime)))

(def profile :selected-worker-reconciliation-v2)
(def ^:private base-report {:profile profile :authority :none :certificate :not-issued})
(def ^:private unsupported (assoc base-report :status :unsupported))
(defn- text? [x] (and (string? x) (not (str/blank? x))))
(defn- claim? [claim]
  (and (map? claim) (nil? (meta claim))
       (every? #{:intent-id :lease-id :worker-id :receipt-id :receipt-evidence} (keys claim))
       (uuid? (:intent-id claim)) (uuid? (:lease-id claim)) (text? (:worker-id claim))
       (or (and (not (contains? claim :receipt-id)) (not (contains? claim :receipt-evidence)))
           (and (uuid? (:receipt-id claim))
                (= :unverified-post-dispatch-receipt (:receipt-evidence claim))))))
(defn- row? [row id source]
  (and (= id (:xt/id row)) (= source (:gesso.live.outbox/source-id row))
       (= :gesso.live.outbox/publication-intent (:gesso.live.outbox/type row))
       (= 1 (:gesso.live.outbox/version row))
       (qualified-keyword? (:gesso.live.outbox/operation row))
       (qualified-keyword? (:gesso.live.outbox/purpose row))
       (instance? ZonedDateTime (:xt/system-from row))
       (case (:gesso.live.outbox/status row)
         :pending true
         (:leased :dispatch-accepted)
         (and (uuid? (:gesso.live.outbox/lease-id row))
              (text? (:gesso.live.outbox/lease-owner row))
              (instance? ZonedDateTime (:gesso.live.outbox/lease-until row))
              (or (= :leased (:gesso.live.outbox/status row))
                  (and (uuid? (:gesso.live.outbox/dispatch-receipt-id row))
                       (qualified-keyword? (:gesso.live.outbox/dispatch-adapter row)))))
         false)))
(defn- disposition [row claim]
  (let [same? (and (= (:lease-id claim) (:gesso.live.outbox/lease-id row))
                   (= (:worker-id claim) (:gesso.live.outbox/lease-owner row)))]
    (case (:gesso.live.outbox/status row)
      :pending :pending-after-interruption
      ;; This production label identifies the observed lease, NOT whether
      ;; its deadline is still in the future at observation or execution.
      :leased (if same? :same-lease-active :lease-replaced)
      :dispatch-accepted (cond
                           (not same?) :other-lease-recorded
                           (nil? (:receipt-id claim)) :terminal-without-receipt-correlation
                           (= (:receipt-id claim) (:gesso.live.outbox/dispatch-receipt-id row))
                           :matching-acknowledgment-recorded
                           :else :other-receipt-recorded))))

(defn- completed-entry? [entry]
  (and (map? entry) (nil? (meta entry)) (uuid? (:intent-id entry))
       (case (:acquisition-status entry)
         (:acquired :reclaimed)
         (and (= :dispatch-accepted (:status entry))
              (text? (:worker-id entry)) (uuid? (:lease-id entry))
              (uuid? (:receipt-id entry)) (some? (:progression entry)))
         (:contended :unavailable)
         (and (nil? (:status entry))
              (not-any? #(contains? entry %) [:lease-id :receipt-id]))
         false)))

(defn- completed-prefix? [failure]
  (if (contains? failure :completed)
    (let [entries (:completed failure)]
      (and (vector? entries) (nil? (meta entries)) (<= (count entries) 128)
           (= (count entries) (:completed-count failure))
           (every? completed-entry? entries)
           (every? (fn [[a b]] (neg? (compare (str (:intent-id a)) (str (:intent-id b)))))
                   (partition 2 1 entries))
           (or (empty? entries) (= (:intent-id (peek entries)) (:last-completed-cursor failure)))))
    (not-any? #(contains? failure %) [:completed-count :last-completed-cursor])))

(defn- inspection-plan [source failure]
  (when (and (text? source) (map? failure) (nil? (meta failure))
             (every? #{:error/type :rescan-required? :in-flight :completed
                       :completed-count :last-completed-cursor} (keys failure))
             (= :gesso.live.outbox/cycle-interrupted (:error/type failure))
             (true? (:rescan-required? failure))
             (or (nil? (:in-flight failure)) (claim? (:in-flight failure)))
             (completed-prefix? failure))
    (conj (mapv (fn [entry] {:kind :completed :entry entry :read? (some? (:status entry))})
                (:completed failure))
          {:kind :in-flight :entry (:in-flight failure) :read? (some? (:in-flight failure))})))

(defn- completed-disposition [row entry]
  (cond
    (not= :dispatch-accepted (:gesso.live.outbox/status row)) :not-terminal
    (and (= (:worker-id entry) (:gesso.live.outbox/lease-owner row))
         (= (:lease-id entry) (:gesso.live.outbox/lease-id row))
         (= (:receipt-id entry) (:gesso.live.outbox/dispatch-receipt-id row)))
    :matching-acknowledgment-recorded
    :else :different-acknowledgment-recorded))

(defn- inspect-planned [prior source {:keys [kind entry read?]} point]
  (if-not read?
    (if (= :completed kind)
      {:intent-id (:intent-id entry) :status :no-dispatch-claim}
      {:status :unidentified})
    (let [[s v] point
          id (:intent-id entry)
          {:keys [status row]} (temporal/observe-outbox-at prior s v id)]
      (cond
        (not= :predicted status) nil
        (nil? row) {:status :missing :intent-id id}
        (not (row? row id source)) nil
        :else {:status ((if (= :completed kind) completed-disposition disposition) row entry)
               :intent-id id :observed-status (:gesso.live.outbox/status row)
               :observed-system-from (:xt/system-from row)}))))

(defn- predict-plan [prior source failure plan points]
  (if-not (and plan (vector? points) (nil? (meta points))
               (= (count points) (count (filter :read? plan)))
               (every? #(and (vector? %) (nil? (meta %)) (= 2 (count %))) points))
    unsupported
    (loop [remaining (seq plan) reads (seq points) observations []]
      (if-let [item (first remaining)]
        (if-let [observation (inspect-planned prior source item (first reads))]
          (recur (next remaining) (if (:read? item) (next reads) reads)
                 (conj observations observation))
          unsupported)
        (assoc base-report :status :predicted :observation
               (cond-> (assoc (peek observations) :rescan-required? true)
                 (contains? failure :completed) (assoc :completed-observations (pop observations))))))))

(defn predict-inspection-with-reads
  "Predict the selected reconciliation response with an explicit ordered
   vector of [system-time valid-time] points, one per actual row read.
   Validate the entire reported prefix first. No-dispatch entries consume
   no point. The supplied snapshots need not be identical; no atomic read,
   frontier freshness, noncommit conclusion or restart authority follows."
  [prior source failure points]
  (predict-plan prior source failure (inspection-plan source failure) points))

(defn predict-inspection
  "Convenience for selected reconciliation at one supplied pinned point.
   This does not establish atomicity of production reconciliation reads."
  [prior system-time valid-time source failure]
  (let [plan (inspection-plan source failure)]
    (predict-plan prior source failure plan
                  (vec (repeat (count (filter :read? plan)) [system-time valid-time])))))
