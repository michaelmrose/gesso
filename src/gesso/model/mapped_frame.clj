(ns gesso.model.mapped-frame
  "Check complete supplied document replacements, not just their projections.
   Completeness and freshness of acquired rows remain trusted."
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [xtdb.util :as xt-util]))

(defn- reject! [reason]
  (throw (ex-info "Invalid mapped document frame." {:error/type ::invalid-frame :reason reason})))

(defn- row-key [[_ _ _ field]]
  (keyword (str/replace (name field) "_" "-")))

(defn- target-fields [coordinates target]
  (into #{} (comp (filter #(= target (into [:document] (subvec % 1 3))))
                  (map row-key))
        (remove #(= :journal-slot (first %)) (vals coordinates))))

(defn- safe-keys! [ks]
  (when-not (every? keyword? ks) (reject! :invalid-document-key))
  (let [normalized (map xt-util/kw->normal-form-kw ks)]
    (when-not (= (count ks) (count (set normalized))) (reject! :aliased-document-key))
    (when (some #(and (str/starts-with? (name %) "xt$")
                     (not= % (xt-util/kw->normal-form-kw :xt/id))) normalized)
      (reject! :reserved-document-key))))

(defn check-effects!
  "Require a complete per-write-target map of exact supplemental values.
   Supplemental fields may not overlap any mapped field or document identity."
  [mapping effects]
  (let [targets (set (filter #(= :document (first %)) (:write-targets mapping)))]
    (when-not (and (map? effects) (= targets (set (keys effects))))
      (reject! :incomplete-document-effects))
    (doseq [[target extras] effects]
      (when-not (map? extras) (reject! :invalid-document-effects))
      (safe-keys! (keys extras))
      (let [mapped (conj (target-fields (:target-map mapping) target) :xt/id)]
        (when (seq (set/intersection (set (map xt-util/kw->normal-form-kw mapped))
                                    (set (map xt-util/kw->normal-form-kw (keys extras)))))
          (reject! :overlapping-document-effects))))
    effects))

(defn check-replacements!
  "After the mapped candidate has been checked, enforce exact supplemental
   values and preserve every other supplied field (including key presence).
   A newly created document must have exactly its mapped fields, identity,
   and declared supplemental fields. Journal rows are checked separately."
  [mapping effects document-rows puts]
  (check-effects! mapping effects)
  (doseq [[_ table & rows] puts :when (not= table :gesso_commands)
          row rows]
    (safe-keys! (keys row))
    (let [target [:document table (:xt/id row)]
          extras (get effects target)
          prior (first (filter #(= (:xt/id row) (:xt/id %)) (get document-rows table)))
          written (target-fields (:write-coordinates mapping) target)
          mutable (into written (keys extras))]
      (when-not (and (contains? effects target)
                     (= extras (select-keys row (keys extras))))
        (reject! :supplemental-document-value-mismatch))
      (if prior
        (do (safe-keys! (keys prior))
            (when-not (= (apply dissoc prior mutable) (apply dissoc row mutable))
              (reject! :unmapped-document-frame-mismatch)))
        (when-not (= (set (keys row))
                     (into (conj (target-fields (:target-map mapping) target) :xt/id) (keys extras)))
          (reject! :created-document-field-mismatch)))))
  true)
