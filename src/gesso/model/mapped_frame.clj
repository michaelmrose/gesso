(ns gesso.model.mapped-frame
  "Check complete supplied document replacements, not just their projections.
   Completeness and freshness of acquired rows remain trusted."
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [gesso.model.tx :as tx]
            [gesso.model.command-resolution :as resolution]
            [gesso.choreo.identity :as identity]
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


(defn- comparable-field? [value]
  ;; Keep the initial lowering inside the scalar SQL equality fragment. In
  ;; particular, do not treat an explicit nil as proof of column absence, or
  ;; infer recursive equality for arbitrary payloads.
  (or (string? value) (boolean? value) (keyword? value) (int? value)))

(defn prestate-assertions!
  "Derive canonical atomic pre-write assertions for every mapped document
   target (including read-only and framed documents) from a checked mapping
   and its supplied complete rows. An
   existing row requires equality of every non-ID field AND exact field
   count; an absent nullable target requires document absence. Only non-null
   scalar fields and canonical SQL column identities are supported here.
   The caller must have checked projection and complete row acquisition and
   must insert these assertions before any write in the same transaction.
   Journal slots are not covered by this document prefix.
   This is restricted predicate lowering, not general SQL refinement,
   authenticated acquisition, or enforcement at the installed gateway."
  [mapping document-rows]
  (vec
   (mapcat
    (fn [[_ table id]]
      (let [rows (get document-rows table)
            _ (when-not (and (sequential? rows) (every? map? rows))
                (reject! :invalid-frame-row-collection))
            matches (filterv #(= id (:xt/id %)) rows)
            _ (when (> (count matches) 1) (reject! :duplicate-frame-row))
            row (first matches)
            required? (some (fn [[kind t i]]
                              (and (= :document kind) (= table t) (= id i)))
                            (vals (:target-map mapping)))]
        (if row
          (do
            (safe-keys! (keys row))
            (let [checks (mapv (fn [[k v]]
                                 (when-not (and (re-matches #"[a-z][a-z0-9_$]*"
                                                           (name (xt-util/kw->normal-form-kw k)))
                                                (comparable-field? v))
                                   (reject! :unsupported-frame-field))
                                 [k v])
                               (sort-by (comp str key) (dissoc row :xt/id)))]
              [(tx/assert-document-decoded-current table id checks)
               (tx/assert-document-field-count table id (count row))]))
          (do
            (when required? (reject! :missing-required-frame-row))
            [(tx/assert-document-absent table id)]))))
    (sort-by pr-str (into #{} (keep (fn [[kind table id]]
                                      (when (#{:document :nullable-document} kind)
                                        [:document table id])))
                          (vals (:target-map mapping)))))))

(defn require-prestate-assertions!
  "Check that the exact lowered frame assertions occur in a supplied
   pre-write SQL prefix. Other predicates still need their own footprint
   checks. This alone cannot attest that a transaction was submitted."
  [mapping document-rows formatted-guards]
  (let [required (frequencies (map tx/format-assertion
                                  (prestate-assertions! mapping document-rows)))
        supplied (frequencies formatted-guards)]
    (when-not (every? (fn [[guard n]] (<= n (get supplied guard 0))) required)
      (reject! :missing-frame-prestate-assertion))
    true))

(defn- journal-field-sql! [parent k]
  (let [column (name (xt-util/kw->normal-form-kw k))]
    (when-not (re-matches #"[a-z_][a-z0-9_$]*" column)
      (reject! :unsupported-journal-frame-key))
    (if parent (str "(" parent ")." column) column)))

(defn- scalar-tests [path value]
  ;; Ordinary equality coerces some types. Scalar print distinguishes
  ;; keyword/string and float/integer; singleton-array print also
  ;; distinguishes integral decimals from integers in this provider.
  [[(str path " = ?") value]
   [(str "STR(" path ") = STR(?)") value]
   [(str "STR(ARRAY[" path "]) = STR(ARRAY[?])") value]])

(defn- journal-field-tests! [path value]
  ;; XTDB struct equality compares the union schema of field names. Use
  ;; visible field counts and scalar leaves for nonempty maps instead.
  ;; For empty maps in XTDB 2.2.0-beta1, LENGTH counts visible members and
  ;; STR prints structs with a leading brace, even with null schema fields.
  ;; Other LENGTH-supported types of length zero print without that prefix.
  ;; This is a provider-specific fragment, not a portable type predicate.
  (cond
    (map? value)
    (do (safe-keys! (keys value))
        (if (seq value)
          (into [[(str "LENGTH(" path ") = ?") (count value)]]
                (mapcat (fn [[k v]] (journal-field-tests! (journal-field-sql! path k) v)))
                (sort-by (comp str key) value))
          [[(str "LENGTH(" path ") = ?") 0]
           [(str "STR(" path ") LIKE ?") "{%"]]))
    (or (comparable-field? value) (uuid? value))
    (scalar-tests path value)
    (and (vector? value) (every? #(or (comparable-field? %) (uuid? %)) value))
    ;; Keep whole-list equality for shape, but check each element because
    ;; list equality and list printing can both conflate keywords/strings.
    (into [[(str path " = ?") value]]
          (mapcat (fn [[i v]] (scalar-tests (str "(" path ")[" (inc i) "]") v)))
          (map-indexed vector value))
    :else (reject! :unsupported-journal-frame-value)))

(defn- occupied-slot-assertions! [where principal operation row]
  (safe-keys! (keys row))
  (let [checks (mapcat (fn [[k v]] (journal-field-tests! (journal-field-sql! nil k) v))
                       (sort-by (comp str key) row))]
    [(tx/format-assertion (tx/assert-document-field-count :gesso_commands (:xt/id row) (count row)))
     (into [(str "ASSERT 1 = (SELECT COUNT(*)" where " AND "
                 (str/join " AND " (map first checks)) ")") principal operation]
           (map second checks))]))

(defn formatted-prestate-assertions!
  "Extend the mapped document prefix with every mapped journal slot. An
   empty slot requires zero matches by principal/operation; an occupied
   slot requires exactly one match and equality of the complete record,
   including the binding and outcome used by the decoder. Supplied rows
   must already have passed the mapped projection check. Restrict nested
   values to a supported non-null fragment. Structs use visible field counts,
   leaf equality and, for empty structs, a provider-specific shape check.
   This avoids whole-struct schema equality. This
   does not establish independent codec or provider refinement."
  [mapping document-rows journal-rows]
  (let [slots (sort-by pr-str (filter #(= :journal-slot (first %))
                                      (vals (:target-map mapping))))
        slot-set (set slots)
        _ (when-not (and (sequential? journal-rows) (every? map? journal-rows))
            (reject! :invalid-journal-frame-rows))
        index (reduce (fn [m row]
                        (let [binding (:gesso.command/binding (resolution/require-record row))
                              slot [:journal-slot (identity/raw-value (:principal binding))
                                    (:operation binding)]]
                          (when-not (contains? slot-set slot) (reject! :unexpected-journal-frame-row))
                          (when (contains? m slot) (reject! :duplicate-journal-frame-slot))
                          (assoc m slot row))) {} journal-rows)
        where " FROM gesso_commands WHERE (gesso$command$binding).principal.gesso$choreo$identity$value = ? AND (gesso$command$binding).operation = ?"
        count-sql (fn [n] (str "ASSERT " n " = (SELECT COUNT(*)" where ")"))]
    (into (mapv tx/format-assertion (prestate-assertions! mapping document-rows))
          (mapcat (fn [[_ principal operation :as slot]]
                    (if-let [row (get index slot)]
                      (into [[(count-sql 1) principal operation]]
                            (occupied-slot-assertions! where principal operation row))
                      [[(count-sql 0) principal operation]])))
          slots)))

(defn require-formatted-prestate-assertions!
  "Require the full mapped document and journal prestate prefix in the
   supplied pre-write guards. Exact ordering is checked by framed admission."
  [mapping document-rows journal-rows formatted-guards]
  (let [required (frequencies (formatted-prestate-assertions! mapping document-rows journal-rows))
        supplied (frequencies formatted-guards)]
    (when-not (every? (fn [[guard n]] (<= n (get supplied guard 0))) required)
      (reject! :missing-frame-prestate-assertion))
    true))
