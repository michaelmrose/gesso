(ns gesso.assurance.reference-reconciliation
  "Selected read-only reconciliation of an interrupted in-flight worker call."
  (:require [clojure.string :as str]
            [gesso.assurance.reference-temporal :as temporal])
  (:import (java.time ZonedDateTime)))

(def profile :selected-in-flight-reconciliation-v1)
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

(defn predict-inspection
  "Predict only the selected in-flight reconciliation response at supplied
   pinned system/valid points. Physical replay is an input hypothesis, not
   knowledge available to a worker whose reply was lost. No completed
   prefix, live frontier, noncommit conclusion or restart authority."
  [prior system-time valid-time source failure]
  (let [claim (:in-flight failure)
        predicted #(assoc base-report :status :predicted :observation
                          (assoc % :rescan-required? true))]
    (if-not (and (text? source) (map? failure) (nil? (meta failure))
                 (every? #{:error/type :rescan-required? :in-flight} (keys failure))
                 (= :gesso.live.outbox/cycle-interrupted (:error/type failure))
                 (true? (:rescan-required? failure))
                 (or (nil? claim) (claim? claim)))
      unsupported
      (if (nil? claim)
        (predicted {:status :unidentified})
        (let [{:keys [status row]} (temporal/observe-outbox-at prior system-time valid-time (:intent-id claim))]
          (cond
            (not= :predicted status) unsupported
            (nil? row) (predicted {:status :missing :intent-id (:intent-id claim)})
            (not (row? row (:intent-id claim) source)) unsupported
            :else (predicted {:status (disposition row claim)
                              :intent-id (:intent-id claim)
                              :observed-status (:gesso.live.outbox/status row)
                              :observed-system-from (:xt/system-from row)})))))))
