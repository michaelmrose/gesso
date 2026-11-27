(ns gesso.assurance.reference-recovery-projection
  "Independent coordinate interpretation over supplied visible rows.
   Shares the canonical record validator, not the mapped poststate decoder.
   No claim that the supplied rows exhaust authoritative provider state."
  (:require [clojure.string :as str]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]))

(def profile :selected-independent-coordinate-projection-v1)
(def ^:private original-record resolution/require-record)
(def ^:private original-raw identity/raw-value)

(defn boundaries-current? []
  (and (identical? original-record resolution/require-record)
       (identical? original-raw identity/raw-value)))

(defn- reject! [reason]
  (throw (ex-info "Unqualified independent coordinate projection" {:reason reason})))

(defn- physical-name? [x]
  (and (simple-keyword? x) (re-matches #"[a-z][a-z0-9_]*" (name x))))

(defn- coordinate! [[kind a b c :as coord]]
  (when-not (and (vector? coord)
                 (case kind
                   (:document :nullable-document)
                   (and (= 4 (count coord)) (physical-name? a) (not= :gesso_commands a)
                        (string? b) (seq b) (physical-name? c))
                   :journal-slot (and (= 3 (count coord)) (string? a) (seq a) (qualified-keyword? b))
                   false))
    (reject! :invalid-independent-coordinate))
  coord)

(defn- journal-records! [visible]
  (let [rows (get visible :gesso_commands {})]
    (when-not (map? rows) (reject! :invalid-independent-journal))
    (mapv (fn [[id raw]]
            (let [record (original-record raw)
                  binding (:gesso.command/binding record)]
              (when-not (= id (:xt/id record)) (reject! :independent-journal-identity-mismatch))
              {:principal (original-raw (:principal binding))
               :operation (:operation binding) :arguments (:arguments binding)
               :outcome (:gesso.command/outcome record)})) rows)))

(defn- slot-value! [records cases principal operation]
  (let [matches (filterv #(and (= principal (:principal %)) (= operation (:operation %))) records)]
    (case (count matches)
      0 nil
      1 (let [{:keys [arguments outcome]} (first matches)
              k [principal operation arguments outcome]]
          (when-not (and (contains? cases k) (keyword? (get cases k)))
            (reject! :independent-unmapped-outcome))
          (get cases k))
      (reject! :independent-ambiguous-slot))))

(defn- document-value! [visible kind table id column]
  (let [rows (get visible table {})
        field (keyword (str/replace (name column) "_" "-"))]
    (when-not (map? rows) (reject! :invalid-independent-document-table))
    (if-not (contains? rows id)
      (if (= :nullable-document kind) nil (reject! :independent-required-document-absent))
      (let [row (get rows id)]
        (when-not (and (map? row) (= id (:xt/id row)) (contains? row field))
          (reject! :independent-mapped-field-missing))
        (get row field)))))

(defn project!
  "Interpret each declared coordinate independently. Only a closed case map
   is allowed; journal matches use full principal/operation/arguments/outcome
   identity and must be unique. The caller supplies the already qualified
   original coordinates and closed decoder cases."
  [coordinates visible cases]
  (when-not (and (boundaries-current?) (map? coordinates) (seq coordinates)
                 (map? visible) (map? cases))
    (reject! :unqualified-independent-projection))
  (let [coords (mapv coordinate! (vals coordinates))
        records (when (some #(= :journal-slot (first %)) coords) (journal-records! visible))]
    (into {} (map (fn [[field [kind a b c]]]
                    [field (if (= :journal-slot kind)
                             (slot-value! records cases a b)
                             (document-value! visible kind a b c))])) coordinates)))
