(ns gesso.assurance.reference-discovery
  "Selected fresh outbox discovery at a supplied completed snapshot and clock."
  (:require [clojure.string :as str]
            [gesso.assurance.reference-temporal :as temporal])
  (:import (java.time Instant ZonedDateTime)))

(def profile :selected-outbox-discovery-v1)
(def ^:private base-report {:profile profile :authority :none :certificate :not-issued})
(def ^:private unsupported (assoc base-report :status :unsupported))

(defn- clock? [x]
  (and (instance? Instant x) (zero? (mod (.getNano ^Instant x) 1000))
       (not (.isBefore ^Instant x (Instant/parse "0001-01-01T00:00:00Z")))
       (.isBefore ^Instant x (Instant/parse "9999-01-01T00:00:00Z"))))

(defn- text? [x] (and (string? x) (not (str/blank? x))))

(defn- row? [[id row]]
  (and (uuid? id) (= id (:xt/id row))
       (keyword? (:gesso.live.outbox/type row))
       (integer? (:gesso.live.outbox/version row))
       (text? (:gesso.live.outbox/source-id row))
       (keyword? (:gesso.live.outbox/status row))
       (or (not= :leased (:gesso.live.outbox/status row))
           (and (instance? ZonedDateTime (:gesso.live.outbox/lease-until row))
                (clock? (.toInstant ^ZonedDateTime (:gesso.live.outbox/lease-until row)))))))

(defn- eligible-action [row source clock]
  (when (and (= :gesso.live.outbox/publication-intent (:gesso.live.outbox/type row))
             (= 1 (:gesso.live.outbox/version row))
             (= source (:gesso.live.outbox/source-id row)))
    (case (:gesso.live.outbox/status row)
      :pending :acquire-pending
      :leased (when-not (.isBefore ^Instant clock (.toInstant ^ZonedDateTime (:gesso.live.outbox/lease-until row)))
                :reclaim-expired)
      nil)))

(defn predict-page
  "Predict one advisory page at the latest supplied completed system point.
   Query clock also selects valid time. No live frontier, stable multipage
   snapshot, restart permission, ownership or delivery is established."
  [prior clock opts]
  (let [limit (get opts :limit 64)
        system-time (:system-time (peek (:history prior)))]
    (if-not (and (map? opts) (nil? (meta opts))
                 (every? #{:source-id :limit :after} (keys opts))
                 (text? (:source-id opts)) (integer? limit) (<= 1 limit 128)
                 (or (not (contains? opts :after)) (uuid? (:after opts)))
                 (= temporal/profile (:profile prior)) (= :predicted (:status prior))
                 (clock? system-time) (clock? clock))
      unsupported
      (let [rows (get (temporal/visible-at prior system-time clock) :gesso_live_outbox {})]
        (if-not (every? row? rows)
          unsupported
          (let [ordered (sort-by (comp str key) rows)
                observed (into [] (comp (filter (fn [[id _]]
                                                 (or (not (:after opts))
                                                     (pos? (compare (str id) (str (:after opts)))))))
                                        (keep (fn [[id row]]
                                                (when-let [action (eligible-action row (:source-id opts) clock)]
                                                  {:intent-id id :action action})))
                                        (take (inc limit))) ordered)
                more? (> (count observed) limit)
                page (vec (take limit observed))]
            (assoc base-report :status :predicted
                   :page {:candidates page :has-more? more?
                          :next-after (when more? (:intent-id (peek page)))})))))))
