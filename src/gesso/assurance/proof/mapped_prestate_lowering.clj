(ns gesso.assurance.proof.mapped-prestate-lowering
  "Source-relative syntactic correspondence, independent of mapped-frame.
   Shares the low-level transaction formatter and identifier normalization."
  (:require [clojure.string :as str]
            [gesso.assurance.proof.mapped-prestate-semantics :as semantics]
            [gesso.model.tx :as tx]
            [xtdb.util :as xt-util]))

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
    columns))

(defn- document-guards [[_ table id] [kind row]]
  (case kind
    :absent [(tx/format-assertion (tx/assert-document-absent table id))]
    :exact-row
    (let [fields (columns! (dissoc row :xt/id) #"[a-z][a-z0-9_$]*")]
      (when-not (every? (comp scalar? last) fields) (reject! :unsupported-value))
      [(tx/format-assertion
        (if (seq fields)
          (tx/assert-document-current table {:model/id id :model/checks (mapv (fn [[k _ v]] [k v]) fields)})
          (tx/assert-document-exists table id)))
       (tx/format-assertion (tx/assert-document-field-count table id (count row)))])
    (reject! :unsupported-predicate)))

(defn- journal-leaf? [v]
  (or (scalar? v) (uuid? v)))

(defn- path-sql [path]
  (reduce (fn [parent column] (str "(" parent ")." column)) path))

(defn- journal-constraints [row]
  ;; A nonempty map requires visible cardinality and all child paths.
  (letfn [(walk [path v]
            (let [p (path-sql path)]
              (cond
                (and (map? v) (seq v))
                (cons [(str "LENGTH(" p ") = ?") (count v)]
                      (mapcat (fn [[_ col child]] (walk (conj path col) child))
                              (columns! v #"[a-z_][a-z0-9_$]*")))
                (or (= {} v) (journal-leaf? v)
                    (and (vector? v) (every? journal-leaf? v)))
                [[(str p " = ?") v]]
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
      (let [constraints (vec (journal-constraints row))]
        [(count-guard 1)
         (tx/format-assertion (tx/assert-document-field-count :gesso_commands (:xt/id row) (count row)))
         (into [(str "ASSERT 1 = (SELECT COUNT(*)" slot-selector " AND "
                     (str/join " AND " (map first constraints)) ")") principal operation]
               (map second constraints))])
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
    {:type ::mapped-prestate-lowering :version 1
     :mapping-fingerprint (:mapping-fingerprint description)
     :scope :source-relative-syntactic-prefix :status :checked-exact-prefix
     :groups groups :assertion-count (count expected)
     :sql-refinement :not-established :authority :none :certificate :not-issued
     :open-obligations #{:shared-formatter-and-identifier-refinement
                         :sql-value-and-struct-semantics :faithful-row-acquisition
                         :transaction-internal-read-basis :decoder-congruence}}))
