(ns gesso.assurance.proof.xtdb-gateway-issued-row-history
  "v1873: Read-only, parameterized, pinned XTDB system-time history
   diagnostic for ONE previously checked target and two reported bases.

   Observed boundaries are affirmative row-specific evidence within the
   pinned snapshot. No returned boundaries is NOT proof that a row never
   changed: erasure, availability, or visibility may prevent such inference.
   This module neither authenticates the node nor provides CAS, proof
   discharge, replay authority or transaction-time enforcement.

   v1874: When the bounded boundary query returns nothing, independently
   check whether ANY selected-row historical version remains visible at the
   same pinned snapshot. Never equate no history returned with no change:
   ERASE, retention and incomplete visibility remain possible."
  (:require [gesso.assurance.proof.xtdb-gateway-issued-snapshot-identity
             :as snapshot]
            [xtdb.api :as xt])
  (:import (java.time Instant ZonedDateTime)
           (java.util UUID)))

(def ^:private original-query xt/q)

(defn- reject! [reason]
  (throw (ex-info "Pinned selected-row system history not established."
                  {:reason reason})))

(defn- pinned! []
  (when-not (identical? original-query (var-get #'xt/q))
    (reject! :substituted-original-row-history-query)))

(defn observe-pinned-row-system-boundaries!
  "Return ONLY a redacted classification. Query one target's history under
   the exact token and clock-time used by the later document read, looking
   for system-version starts OR ends inside (original-basis, later-basis].

   The bounded query is a presence test, not a complete history export.
   No boundaries returned is intentionally inconclusive about intervening
   changes, especially in the presence of erasure or history limitations.
   If no history record is visible at all, report that separately: this does
   NOT assert that an ERASE occurred or a row never existed."
  [node target snapshot-token clock-time original-point later-point]
  (pinned!)
  (let [[table id :as locator] target
        relationship (snapshot/reported-basis-relation
                      original-point later-point)]
    (when-not (and (some? node)
                   (vector? locator) (= 2 (count locator))
                   (keyword? table) (nil? (namespace table))
                   (re-matches #"[a-z][a-z0-9_]*" (name table))
                   (or (and (string? id) (seq id))
                       (instance? UUID id))
                   (string? snapshot-token) (seq snapshot-token)
                   (instance? Instant clock-time))
      (reject! :invalid-pinned-selected-row-history-request))
    (case relationship
      :same-reported-database-basis :same-reported-basis-no-interval
      :later-reported-database-basis
      (let [from (:system-time original-point)
            to (:system-time later-point)
            sql (str "SELECT _system_from, _system_to FROM " (name table)
                     " FOR VALID_TIME ALL FOR SYSTEM_TIME ALL"
                     " WHERE _id = ? AND"
                     " ((_system_from > ? AND _system_from <= ?)"
                     " OR (_system_to > ? AND _system_to <= ?)) LIMIT 2")
            rows (try
                   (original-query node [sql id from to from to]
                                   {:snapshot-token snapshot-token
                                    :current-time clock-time})
                   (catch Throwable _
                     (reject! :pinned-selected-row-history-query-unavailable)))]
        (when-not (and (sequential? rows) (<= 0 (count rows) 2)
                       (every? (fn [row]
                                 (and (map? row)
                                      (contains? row :xt/system-from)
                                      (instance? ZonedDateTime (:xt/system-from row))
                                      (or (not (contains? row :xt/system-to))
                                          (instance? ZonedDateTime
                                                     (:xt/system-to row)))))
                               rows))
          (reject! :invalid-pinned-selected-row-history-result))
        (pinned!)
        (if (seq rows)
          :visible-selected-row-system-boundary-observed
          (let [history-sql (str "SELECT _system_from FROM " (name table)
                                 " FOR VALID_TIME ALL FOR SYSTEM_TIME ALL"
                                 " WHERE _id = ? LIMIT 1")
                visible (try
                          (original-query node [history-sql id]
                                          {:snapshot-token snapshot-token
                                           :current-time clock-time})
                          (catch Throwable _
                            (reject! :pinned-selected-row-history-visibility-unavailable)))]
            (when-not (and (sequential? visible) (<= 0 (count visible) 1)
                           (every? (fn [row]
                                     (and (map? row)
                                          (instance? ZonedDateTime
                                                     (:xt/system-from row))))
                                   visible))
              (reject! :invalid-pinned-selected-row-history-visibility))
            (pinned!)
            (if (seq visible)
              :no-visible-selected-row-system-boundary-returned
              :no-visible-selected-row-system-history-returned))))
      ;; Earlier, cross-database and partially decoded bases cannot be
      ;; interpreted as an ordered historical interval for this row.
      :not-comparable)))
