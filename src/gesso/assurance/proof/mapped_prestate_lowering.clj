(ns gesso.assurance.proof.mapped-prestate-lowering
  "Source-relative syntactic correspondence, independent of mapped-frame.
   Builds exact SQL vectors independently of model.tx and HoneySQL.
   Shares XTDB identifier normalization; no provider semantics theorem."
  (:require [clojure.string :as str]
            [gesso.assurance.proof.mapped-prestate-semantics :as semantics]
            [xtdb.util :as xt-util])
  (:import (xtdb.api.query IKeyFn$KeyFn)))

(defn- reject! [reason]
  (throw (ex-info "Invalid mapped prestate lowering."
                  {:error/type ::invalid-lowering :reason reason})))

(defn- scalar? [v]
  (or (string? v) (boolean? v) (keyword? v) (int? v)))

(defn- columns! [row pattern]
  (when-not (every? keyword? (keys row)) (reject! :unsupported-field))
  (let [columns (mapv (fn [[k v]] [k (name (xt-util/kw->normal-form-kw k)) v])
                      (sort-by (comp str key) row))]
    (when-not (and (= (count columns) (count (set (map second columns))))
                   (every? (fn [[k col _]]
                             (and (re-matches pattern col)
                                  (or (not (str/starts-with? col "xt$")) (= :xt/id k))))
                           columns))
      (reject! :unsupported-field))
    (when-not (every? (fn [[k col _]]
                        (= k (.denormalize IKeyFn$KeyFn/KEBAB_CASE_KEYWORD col))) columns)
      (reject! :noncanonical-decoded-field))
    columns))

(defn- table-sql! [table]
  (when-not (and (simple-keyword? table) (re-matches #"[a-z][a-z0-9_]*" (name table)))
    (reject! :unsupported-table))
  (name table))

(defn- field-count-guard [table id n]
  (when-not (and (integer? n) (<= 1 n 8192))
    (reject! :unsupported-field-count))
  [(str "ASSERT ? = (SELECT LENGTH(NEST_ONE (SELECT * FROM "
        (table-sql! table) " WHERE _id = ?)))") n id])

(defn- decoded-document-guard [table id fields]
  (let [pairs (into [["_id" id]]
                    (map (fn [[_ column value]]
                           [(if (str/includes? column "$") (str "\"" column "\"") column) value]))
                    fields)
        clauses (mapcat (fn [[column _]]
                          [(str "(" column " = ?)")
                           (str "(STR(" column ") = STR(?))")
                           (str "(STR(ARRAY[" column "]) = STR(ARRAY[?]))")]) pairs)]
    (into [(str "ASSERT ? = (SELECT COUNT(*) FROM " (table-sql! table)
                " WHERE " (str/join " AND " clauses) ")") 1]
          (mapcat (fn [[_ value]] [value value value])) pairs)))

(defn- document-guards [[_ table id] [kind row]]
  (case kind
    :absent [[(str "ASSERT ? = (SELECT COUNT(*) FROM " (table-sql! table)
                   " WHERE _id = ?)") 0 id]]
    :exact-row
    (let [fields (columns! (dissoc row :xt/id) #"[a-z][a-z0-9_$]*")]
      (when-not (every? (comp scalar? last) fields) (reject! :unsupported-value))
      [(decoded-document-guard table id fields)
       (field-count-guard table id (count row))])
    (reject! :unsupported-predicate)))

(defn- journal-leaf? [v]
  (or (scalar? v) (uuid? v)))

(defn- path-sql [path]
  (reduce (fn [parent column] (str "(" parent ")." column))
          (map #(str "\"" % "\"") path)))

(defn- scalar-constraints [p v]
  [[(str p " = ?") v]
   [(str "STR(" p ") = STR(?)") v]
   [(str "STR(ARRAY[" p "]) = STR(ARRAY[?])") v]])

(defn- journal-constraints [row]
  ;; A nonempty map requires visible cardinality and all child paths.
  (letfn [(walk [path v]
            (let [p (path-sql path)]
              (cond
                (and (map? v) (seq v))
                (cons [(str "LENGTH(" p ") = ?") (count v)]
                      (mapcat (fn [[_ col child]] (walk (conj path col) child))
                              (columns! v #"[a-z_][a-z0-9_$]*")))
                (= {} v)
                ;; XTDB 2.2.0-beta1: zero visible length plus struct print
                ;; shape; length alone accepts other empty value types.
                [[(str "LENGTH(" p ") = ?") 0]
                 [(str "STR(" p ") LIKE ?") "{%"]]
                (journal-leaf? v) (scalar-constraints p v)
                (and (vector? v) (every? journal-leaf? v))
                (into [[(str p " = ?") v]]
                      (mapcat (fn [[i child]]
                                (scalar-constraints (str "(" p ")[" (inc i) "]") child)))
                      (map-indexed vector v))
                :else (reject! :unsupported-value))))]
    (mapcat (fn [[_ col v]] (walk [col] v))
            (columns! row #"[a-z_][a-z0-9_$]*"))))

(def ^:private slot-selector
  " FROM gesso_commands WHERE (gesso$command$binding).principal.gesso$choreo$identity$value = ? AND (gesso$command$binding).operation = ?")

(defn- journal-guards [[_ principal operation] [kind row]]
  (let [count-guard (fn [n] [(str "ASSERT " n " = (SELECT COUNT(*)" slot-selector ")") principal operation])]
    (case kind
      :absent [(count-guard 0)]
      :exact-row
      (let [constraints (vec (journal-constraints row))
            n (count constraints)
            ;; Derive boundaries independently from the ordered constraints.
            ranges (if (> n 96)
                     (map (fn [start] [start (min n (+ start 12))]) (range 0 n 12))
                     [[0 n]])]
        (into [(count-guard 1)
               (field-count-guard :gesso_commands (:xt/id row) (count row))]
              (map (fn [[start end]]
                     (let [group (subvec constraints start end)]
                       (into [(str "ASSERT 1 = (SELECT COUNT(*)" slot-selector " AND "
                                   (str/join " AND " (map first group)) ")") principal operation]
                             (map second group)))))
              ranges))
      (reject! :unsupported-predicate))))

(defn check-prefix!
  "Re-derive the decoded-row predicates and ordered SQL groups from original
   source, mapping and prepared rows. Check the supplied exact prefix, not
   an execution transcript. This diagnostic does not establish SQL semantics."
  [original target-map docs journal prefix]
  (let [description (semantics/describe! original target-map docs journal)
        groups (->> (:predicates description)
                    (sort-by (fn [[target _]] [(if (= :document (first target)) 0 1) (pr-str target)]))
                    (mapv (fn [[target predicate]]
                            {:target target :predicate predicate
                             :assertions ((if (= :document (first target)) document-guards journal-guards)
                                          target predicate)})))
        expected (into [] (mapcat :assertions) groups)]
    (when-not (and (vector? prefix) (every? vector? prefix) (= expected prefix))
      (reject! :noncanonical-lowering-prefix))
    {:type ::mapped-prestate-lowering :version 5
     :mapping-fingerprint (:mapping-fingerprint description)
     :scope :source-relative-syntactic-prefix :status :checked-exact-prefix
     :groups groups :assertion-count (count expected)
     :formatter-correspondence :independent-exact-sql-vectors
     :sql-refinement :not-established :authority :none :certificate :not-issued
     :open-obligations #{:identifier-normalization-refinement
                         :universal-production-formatter-correspondence
                         :sql-value-and-struct-semantics :faithful-row-acquisition
                         :transaction-internal-read-basis :decoder-congruence}}))
