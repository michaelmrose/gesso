(ns gesso.assurance.proof.mapped-prestate-semantics
  "Independent decoded-row semantics for the mapped prestate obligation.
   No dependency on SQL lowering, XTDB, preparation, or the projection
   interpreter. Closed acquired rows and faithful value decoding are premises."
  (:require [gesso.choreo.identity :as identity]
            [gesso.model.certified-plan :as plan]
            [gesso.model.command-resolution :as resolution]))

(defn- reject! [reason]
  (throw (ex-info "Invalid mapped prestate observation."
                  {:error/type ::invalid-prestate :reason reason})))

(defn- targets [mapping]
  (reduce (fn [m [kind a b]]
            (if (= :journal-slot kind)
              (assoc m [kind a b] :optional)
              (assoc m [:document a b]
                     (if (= :document kind) :required :optional))))
          {} (vals (:target-map mapping))))

(defn- journal-target [row]
  (let [binding (:gesso.command/binding (resolution/require-record row))]
    [:journal-slot (identity/raw-value (:principal binding)) (:operation binding)]))

(defn- index-rows! [selection document-rows journal-rows]
  (let [tables (into #{} (keep (fn [[kind table]] (when (= :document kind) table)))
                     (keys selection))]
    (when-not (and (map? document-rows) (= tables (set (keys document-rows)))
                   (every? sequential? (vals document-rows))
                   (sequential? journal-rows)
                   (every? map? (concat (mapcat val document-rows) journal-rows)))
      (reject! :invalid-closed-row-sets))
    (when-not (= (count journal-rows) (count (set (map :xt/id journal-rows))))
      (reject! :duplicate-command-id))
    (reduce (fn [index [target row]]
              (when-not (contains? selection target) (reject! :unexpected-row))
              (when (contains? index target) (reject! :duplicate-row))
              (assoc index target row))
            {} (concat (for [[table rows] document-rows, row rows]
                         [[:document table (:xt/id row)] row])
                       (map (juxt journal-target clojure.core/identity) journal-rows)))))

(defn describe!
  "Derive one exact presence/row predicate per mapped physical target from
   the original construction. Required rows must exist. This semantic
   reference is stronger than projected field equality: unmapped fields
   and the complete decoder input are preserved. It does not decide
   whether a value can be lowered to SQL or validate abstract state types."
  [original target-map document-rows journal-rows]
  (let [mapping (plan/check-target-mapping! original target-map)
        selection (targets mapping)
        index (index-rows! selection document-rows journal-rows)]
    {:type ::mapped-prestate-predicates :version 1
     :mapping-fingerprint (:fingerprint mapping)
     :predicates (into {} (map (fn [[target presence]]
                                [target (if-let [row (get index target)]
                                          [:exact-row row]
                                          (if (= :required presence)
                                            (reject! :missing-required-row)
                                            [:absent]))])) selection)
     :scope :closed-decoded-mapped-rows :authority :none}))

(defn observe!
  "Re-derive the complete predicate set from its original source and
   prepared rows before evaluating it on a second closed row set. Reject
   copied descriptions with changed or omitted predicates. An ill-formed
   observation is not a match. Equality here is Clojure value equality,
   not SQL equality; a deterministic outcome decoder and faithful row
   acquisition remain premises of any projection-preservation argument."
  [original target-map prepared-docs prepared-journal description actual-docs actual-journal]
  (when-not (= description (describe! original target-map prepared-docs prepared-journal))
    (reject! :prestate-description-mismatch))
  (let [predicates (:predicates description)
        index (index-rows! predicates actual-docs actual-journal)
        mismatches (into #{} (keep (fn [[target predicate]]
                                     (let [actual (if-let [row (get index target)]
                                                    [:exact-row row] [:absent])]
                                       (when-not (= predicate actual) target)))) predicates)]
    {:type ::mapped-prestate-observation
     :mapping-fingerprint (:mapping-fingerprint description)
     :matches? (empty? mismatches) :mismatched-targets mismatches
     :scope :supplied-decoded-row-comparison
     :sql-refinement :not-established :authority :none :certificate :not-issued}))
