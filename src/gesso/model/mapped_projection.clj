(ns gesso.model.mapped-projection
  "Decode a closed supplied collection of rows against an original typed
   construction and its checked multi-record mapping. Acquisition, row-set
   completeness, pinning, and application outcome interpretation are trusted
   inputs, not established here. This is not a physical writer inventory."
  (:require [clojure.string :as str]
            [gesso.assurance.proof.typed-value-semantics :as values]
            [gesso.choreo.identity :as identity]
            [gesso.model.certified-plan :as plan]
            [gesso.model.command-resolution :as resolution]))

(defn- reject! [reason]
  (throw (ex-info "Invalid mapped state projection."
                  {:error/type ::invalid-projection :reason reason})))

(defn- indexed! [rows key-fn expected]
  (when-not (and (sequential? rows) (every? map? rows))
    (reject! :invalid-row-collection))
  (reduce (fn [index row]
            (let [k (key-fn row)]
              (when-not (contains? expected k) (reject! :unexpected-row))
              (when (contains? index k) (reject! :duplicate-row))
              (assoc index k row))) {} rows))

(defn- journal-key! [row]
  (let [binding (:gesso.command/binding (resolution/require-record row))]
    [(identity/raw-value (:principal binding)) (:operation binding)]))

(defn project-state!
  "Recheck the original mapping, then project supplied closed table row sets
   and command rows. Every supplied row must belong to a mapped coordinate;
   extra fields on those rows are not part of the abstract projection.
   Required documents must exist. Absent nullable documents and empty journal
   slots project to nil, but present documents must supply the mapped field.
   Optional fields are unsupported (SQL null/absence cannot be inferred).
   Canonical SQL names use XTDB's default kebab-case result keys. The trusted
   outcome decoder receives a validated command row and must return a keyword
   for an occupied slot. Its application-specific meaning is not proved.
   All rows must be acquired at one basis by the caller. No authority issued."
  [original target-map document-rows journal-rows decode-outcome!]
  (let [mapping (plan/check-target-mapping! original target-map)
        coordinates (:target-map mapping)
        documents (group-by second (remove #(= :journal-slot (first %)) (vals coordinates)))
        slots (set (map #(subvec % 1) (filter #(= :journal-slot (first %)) (vals coordinates))))]
    (when (some :optional? (vals (:state-types original)))
      (reject! :optional-field-unsupported))
    (when-not (and (map? document-rows) (= (set (keys documents)) (set (keys document-rows)))
                   (fn? decode-outcome!))
      (reject! :invalid-projection-input))
    (let [indexed (into {} (map (fn [[table coords]]
                                  [table (indexed! (get document-rows table) :xt/id
                                                   (set (map #(nth % 2) coords)))])) documents)
          journal (indexed! journal-rows journal-key! slots)
          _ (when-not (= (count journal) (count (set (map :xt/id (vals journal)))))
              (reject! :duplicate-command-id))
          state (into {}
                      (map (fn [[field [kind a b c]]]
                             [field
                              (if (= :journal-slot kind)
                                (when-let [row (get journal [a b])]
                                  (let [value (decode-outcome! row)]
                                    (when-not (keyword? value) (reject! :invalid-occupied-slot-outcome))
                                    value))
                                (if-let [row (get-in indexed [a b])]
                                  (let [k (keyword (str/replace (name c) "_" "-"))]
                                    (when-not (contains? row k) (reject! :missing-mapped-field))
                                    (get row k))
                                  (if (= :nullable-document kind) nil
                                      (reject! :missing-required-document))))])) coordinates)]
      (values/observe-state! (:state-types original) state)
      state)))
