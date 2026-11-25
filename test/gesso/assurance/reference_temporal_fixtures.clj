(ns gesso.assurance.reference-temporal-fixtures
  "Shared deterministic test data and provider observation helpers."
  (:require [clojure.test :refer [is]]
            [xtdb.api :as xt])
  (:import (java.time Instant)))

(defn day [n] (.plusSeconds (Instant/parse "2020-01-01T00:00:00Z") (* 86400 n)))
(defn sys [n] (.plusSeconds (Instant/parse "2025-01-01T00:00:00Z") (* 86400 n)))
(defn put [table a b & docs]
  (into [:put-docs {:into table :valid-from (day a) :valid-to (day b)}] docs))
(def a {:xt/id "a" :value 1 :old true})
(def b {:xt/id "a" :value 2})
(def c {:xt/id "a" :value 3})
(def d {:xt/id "b" :value 4})

(defn provider-at [node system-time valid-time]
  (into {} (keep (fn [table]
                   (let [rows (xt/q node [(str "SELECT * FROM " (name table)
                                               " FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ?")
                                         valid-time system-time])]
                     (is (= (count rows) (count (set (map :xt/id rows)))))
                     (when (seq rows) [table (into {} (map (juxt :xt/id identity)) rows)]))))
        [:temporalprobe :temporalother]))

(defn rectangle [table doc vf vt sf st]
  {:table table :document doc :valid-from vf :valid-to vt
   :system-from sf :system-to st})

(defn provider-rectangles [node table]
  (mapv (fn [row]
          (let [instant #(when-let [v (get row %)] (.toInstant ^java.time.ZonedDateTime v))]
            (rectangle table (dissoc row :xt/valid-from :xt/valid-to
                                     :xt/system-from :xt/system-to)
                       (instant :xt/valid-from) (instant :xt/valid-to)
                       (instant :xt/system-from) (instant :xt/system-to))))
        (xt/q node [(str "SELECT *, _valid_from, _valid_to, _system_from, _system_to FROM "
                         (name table) " FOR VALID_TIME ALL FOR SYSTEM_TIME ALL")])))

(defn coalesce-valid [rectangles]
  ;; Compare equivalent valid-axis partitions, preserving system origins,
  ;; ends, row contents and multiplicity of overlapping rectangles.
  (mapcat (fn [[_ rs]]
            (reduce (fn [acc r]
                      (if (and (seq acc) (= (:valid-to (peek acc)) (:valid-from r)))
                        (conj (pop acc) (assoc (peek acc) :valid-to (:valid-to r)))
                        (conj acc r)))
                    [] (sort-by :valid-from rs)))
          (group-by #(dissoc % :valid-from :valid-to) rectangles)))
