(ns gesso.assurance.reference-mapped-residual
  "Selected source-relative residual SQL derivation. Not SQL equivalence,
   installed declaration provenance or retained execution evidence."
  (:require [clojure.string :as str]
            [gesso.assurance.proof.mapped-prestate-semantics :as prestate]
            [gesso.choreo.identity :as identity]
            [gesso.model.certified-plan :as plan]
            [gesso.model.command-resolution :as resolution]))

(def profile :selected-mapped-residual-v2)

(defn- reject! [reason]
  (throw (ex-info "Unsupported mapped residual declaration." {::unsupported true :reason reason})))

(defn- exact-map? [m ks]
  (and (map? m) (nil? (meta m)) (= ks (set (keys m)))))

(defn- bounded-vector? [v n]
  (and (vector? v) (nil? (meta v)) (<= (count v) n)))

(defn- scalar? [v]
  (or (instance? Long v) (boolean? v) (uuid? v)
      (and (string? v) (<= (count v) 4096))
      (and (keyword? v) (<= (count (str v)) 4096))))

(defn- field-name! [field qualified?]
  ;; Selected canonical spellings only. Do not rely on the writer's SQL
  ;; formatter or XTDB identifier normalization implementation.
  (when-not (and (keyword? field) (not= :xt/id field)
                 (re-matches #"[a-z][a-z0-9]*(?:-[a-z0-9]+)*" (name field))
                 (or (nil? (namespace field))
                     (and qualified? (not= "xt" (namespace field))
                          (re-matches #"[a-z][a-z0-9]*" (namespace field)))))
    (reject! :unsupported-residual-field))
  (let [local (str/replace (name field) "-" "_")]
    (if-let [ns (namespace field)] (str "\"" ns "$" local "\"") local)))

(defn- mapped-name! [field]
  (when-not (and (simple-keyword? field) (re-matches #"[a-z][a-z0-9]*(?:_[a-z0-9]+)*" (name field)))
    (reject! :unsupported-residual-mapped-field))
  (name field))

(defn- table-name! [table]
  (when-not (and (simple-keyword? table) (re-matches #"(?:gesso_[a-z0-9_]+|requests|request_assignments|memberships)" (name table)))
    (reject! :unsupported-residual-table))
  (name table))

(defn- absent [table id]
  [(str "ASSERT ? = (SELECT COUNT(*) FROM " (table-name! table) " WHERE _id = ?)") 0 id])

(defn- document-check [table id pairs]
  (when-not (and (seq pairs) (= (count pairs) (count (set (map first pairs))))
                 (every? (comp scalar? second) pairs))
    (reject! :invalid-residual-checks))
  (into [(str "ASSERT ? = (SELECT COUNT(*) FROM " (table-name! table)
              " WHERE (_id = ?) AND "
              (str/join " AND " (map #(str "(" (first %) " = ?)") pairs)) ")") 1 id]
        (map second pairs)))

(defn- literal-conditions! [original]
  (let [expr (get-in original [:guard :expression])
        terms (if (= :and (first expr)) (rest expr) [expr])
        pairs (mapv (fn [term]
                      (when-not (and (bounded-vector? term 3) (= 3 (count term))
                                     (every? #(and (bounded-vector? % 2) (= 2 (count %))) (rest term)))
                        (reject! :unsupported-residual-guard))
                      (let [[_ [_ field] [_ value]] term]
                        (when-not (and (= term [:= [:read field] [:literal value]])
                                     (contains? (:state-types original) field)
                                     (not (:optional? (get-in original [:state-types field])))
                                     (or (nil? value) (scalar? value)))
                        (reject! :unsupported-residual-guard))
                      [field value])) terms)]
    (when-not (and (<= 1 (count pairs) 256) (= (count pairs) (count (set (map first pairs)))))
      (reject! :invalid-residual-guard))
    (into {} pairs)))

(defn derive-assertions!
  "Independently derive the ordered residual assertion vector from original
   literal guards, closed prepared rows and a supplied declaration catalog.
   Nil explicitly declares no residual assertions. This profile selects
   Gesso-prefixed tables, canonical scalar fields and string identities."
  [original targets docs journal binding declarations]
  (let [mapping (plan/check-target-mapping! original targets)
        predicates (:predicates (prestate/describe! original targets docs journal))
        binding (resolution/require-binding binding)
        principal (identity/raw-value (:principal binding))
        operation (:operation binding)
        alternative (get-in original [:subject :id])
        command-id (resolution/record-id binding)
        slots (set (filter #(= :journal-slot (first %)) (:write-targets mapping)))]
    (when-not (and (qualified-keyword? alternative) (qualified-keyword? operation)
                   (string? principal) (not (str/blank? principal))
                   (string? command-id) (not (str/blank? command-id))
                   (= #{[:journal-slot principal operation]} slots)
                   (map? declarations) (nil? (meta declarations)) (contains? declarations alternative))
      (reject! :invalid-residual-source))
    (if-let [declaration (get declarations alternative)]
      (let [conditions (literal-conditions! original)
            groups (group-by (fn [[_ coord]] (subvec coord 0 3)) (select-keys targets (keys conditions)))
            required (set (for [[[kind table id] _] groups :when (= :document kind)] [table id]))
            read-required (set (for [[_ [kind table id]] (:read-coordinates mapping) :when (= :document kind)] [table id]))
            {:keys [document-layout subject-checks]} declaration
            value! (fn [table id field]
                     (let [[mode row] (get predicates [:document table id]) v (get row field)]
                       (when-not (and (= :exact-row mode) (scalar? v)) (reject! :invalid-residual-prestate-value)) v))]
        (when-not (and (exact-map? declaration #{:document-layout :subject-checks})
                       (= required read-required) (exact-map? document-layout required)
                       (bounded-vector? subject-checks 256)
                       (= (count subject-checks) (count (set subject-checks))))
          (reject! :invalid-residual-layout))
        (into (vec (mapcat
                    (fn [[[kind table id] entries]]
                      (case kind
                        :document
                        (let [coords (into {} entries) layout (get document-layout [table id])
                              mapped (set (for [[_ [k t i f]] targets :when (and (= :document k) (= t table) (= i id))] (mapped-name! f)))
                              _ (when-not (and (bounded-vector? layout 256)
                                               (every? #(and (bounded-vector? % 2) (= 2 (count %))) layout))
                                  (reject! :invalid-residual-document-layout))
                              fields (mapv second (filter #(= :state (first %)) layout))]
                          (when-not (and (= (count fields) (count coords)) (= (set fields) (set (keys coords))))
                            (reject! :invalid-residual-document-layout))
                          [(document-check table id
                                           (mapv (fn [[k f]]
                                                   (case k
                                                     :state [(mapped-name! (nth (get coords f) 3)) (get conditions f)]
                                                     :prestate (let [col (field-name! f false)]
                                                                 (when (contains? mapped col) (reject! :aliased-residual-field))
                                                                 [col (value! table id f)])
                                                     (reject! :invalid-residual-entry))) layout))])
                        :nullable-document
                        (do (when-not (every? #(nil? (get conditions (first %))) entries) (reject! :invalid-nullable-residual))
                            [(absent table id)])
                        :journal-slot
                        (do (when-not (and (= [table id] [principal operation])
                                           (every? #(nil? (get conditions (first %))) entries))
                              (reject! :invalid-journal-residual))
                            [(absent :gesso_commands command-id)
                             ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_commands WHERE (gesso$command$binding).principal.gesso$choreo$identity$value = ? AND (gesso$command$binding).operation = ?)" principal operation]])
                        (reject! :unsupported-residual-target)))
                    (sort-by (comp pr-str key) groups)))
              (map (fn [entry]
                     (when-not (and (bounded-vector? entry 3) (= 3 (count entry)))
                       (reject! :invalid-residual-subject))
                     (let [[table id field] entry]
                       (when-not (contains? required [table id]) (reject! :invalid-residual-subject))
                       (document-check table id [[(field-name! field true) (value! table id field)]]))))
              subject-checks))
      (if (nil? (get declarations alternative)) [] (reject! :invalid-residual-declaration)))))

(defn check-assertions!
  "Check the complete ordered fragment; no candidate value enters derivation."
  [original targets docs journal binding declarations candidate]
  (let [base {:profile profile :authority :none :certificate :not-issued :provider-refinement :not-established}]
    (try
      (let [expected (derive-assertions! original targets docs journal binding declarations)]
        (if (and (bounded-vector? candidate 1024)
                 (every? #(and (bounded-vector? % 258) (string? (first %)) (every? scalar? (rest %))) candidate)
                 (= expected candidate))
          (assoc base :status :matched :scope :source-relative-residual-fragment)
          (assoc base :status :mismatch :reason :residual-assertion-fragment-mismatch)))
      (catch clojure.lang.ExceptionInfo _
        (assoc base :status :unsupported :reason :invalid-residual-source)))))
