(ns gesso.model.mapped-residual
  "Restricted declaration-relative reconstruction of residual mapped assertions.
   No candidate SQL, callback or observed outbox enters the derivation."
  (:require [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.mapped-guard :as guard]
            [gesso.model.tx :as tx]
            [xtdb.util :as xt-util]))

(defn- reject! [reason]
  (throw (ex-info "Invalid mapped residual assertion derivation."
                  {:reason reason :error/type ::invalid-residual})))

(defn- closed-map? [m ks]
  (and (map? m) (nil? (meta m)) (= ks (set (keys m)))))

(defn- field? [k]
  (and (keyword? k) (not= (xt-util/kw->normal-form-kw k)
                          (xt-util/kw->normal-form-kw :xt/id))
       (re-matches #"[a-z][a-z0-9_$]*" (name (xt-util/kw->normal-form-kw k)))))

(defn- selected-value! [docs [table id] field]
  (let [rows (get docs table)]
    (when-not (and (sequential? rows) (every? map? rows))
      (reject! :invalid-residual-prestate-rows))
    (let [matches (filterv #(= id (:xt/id %)) rows)
          v (get (first matches) field)]
      (when-not (= 1 (count matches)) (reject! :invalid-residual-prestate-rows))
      (when-not (or (string? v) (keyword? v) (integer? v) (boolean? v) (uuid? v))
        (reject! :invalid-residual-prestate-value))
      v)))

(defn- lower! [original mapping declaration binding value!]
  (let [layout (into {} (map (fn [[target entries]]
                               [target (mapv (fn [[kind field :as entry]]
                                               (case kind
                                                 :state entry
                                                 :prestate [:extra field (value! target field)])) entries)]))
                     (:document-layout declaration))]
    (into (guard/lower-assertions! original (:target-map mapping) binding layout)
          (map (fn [[table id field]]
                 (tx/format-assertion
                  (tx/assert-one table [:and [:= :xt/id [:lift id]]
                                       [:= field [:lift (value! [table id] field)]]]))))
          (:subject-checks declaration))))

(defn- slot! [mapping]
  (let [slots (filter #(= :journal-slot (first %)) (:write-targets mapping))]
    (when-not (= 1 (count slots)) (reject! :invalid-residual-command-slot))
    (first slots)))

(defn check-catalog!
  "Require complete alternative coverage and a closed selector grammar.
   Nil explicitly declares no residual assertions. The existing typed guard
   lowerer checks exact required-document layout coverage at issuance."
  [mappings operations declarations]
  (when-not (closed-map? declarations (set (keys mappings)))
    (reject! :incomplete-residual-assertion-catalog))
  (doseq [{:keys [id construction]} operations
          :let [declaration (get declarations id) mapping (get mappings id)]
          :when (some? declaration)]
    (let [required (into #{} (keep (fn [[kind table row-id]]
                                     (when (= :document kind) [table row-id])))
                         (vals (:read-coordinates mapping)))
          {:keys [document-layout subject-checks]} declaration
          [_ principal operation] (slot! mapping)]
      (when-not (and (closed-map? declaration #{:document-layout :subject-checks})
                     (closed-map? document-layout required)
                     (vector? subject-checks) (nil? (meta subject-checks))
                     (<= (count subject-checks) 256)
                     (= (count subject-checks) (count (distinct subject-checks))))
        (reject! :invalid-residual-assertion-declaration))
      (doseq [[_ entries] document-layout]
        (when-not (and (vector? entries) (nil? (meta entries)) (<= (count entries) 256)
                       (= (count entries) (count (distinct entries)))
                       (every? (fn [e] (and (vector? e) (nil? (meta e)) (= 2 (count e))
                                            (case (first e)
                                              :state (keyword? (second e))
                                              :prestate (field? (second e))
                                              false))) entries))
          (reject! :invalid-residual-document-layout)))
      (doseq [[table row-id field :as entry] subject-checks]
        (when-not (and (vector? entry) (nil? (meta entry)) (= 3 (count entry))
                       (contains? required [table row-id]) (field? field))
          (reject! :invalid-residual-subject-selector)))
      ;; Validate the restricted layout without assuming actual prestate values.
      (lower! construction mapping declaration
              {:principal (identity/principal principal) :operation operation
               :command-id (identity/command-id "residual-layout-check") :arguments {}}
              (constantly 0))))
  declarations)

(defn reconstruct!
  "Return the residual SQL vector from an already checked prestate and
   command binding. No proof of acquisition, assertion equivalence or
   completeness of the installed constructor is claimed."
  [original mapping declaration binding docs]
  (let [[_ principal operation] (slot! mapping)]
    (resolution/require-binding binding)
    (when-not (and (= principal (identity/raw-value (:principal binding)))
                   (= operation (:operation binding)))
      (reject! :residual-command-slot-mismatch))
    (if (nil? declaration) []
        (lower! original mapping declaration binding (partial selected-value! docs)))))
