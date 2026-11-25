(ns gesso.model.mapped-guard
  "Restricted lowering of typed literal guard conjunctions to canonical
   pre-write XTDB assertions. Nullable document nil is strengthened to
   document absence; journal nil is strengthened to slot absence. This is
   not an equivalence theorem, general SQL compiler or transaction authority."
  (:require [gesso.choreo.identity :as identity]
            [gesso.model.certified-plan :as plan]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.tx :as tx]
            [xtdb.util :as xt-util]))

(defn- reject! [reason]
  (throw (ex-info "Unsupported mapped guard lowering."
                  {:error/type ::invalid-guard :reason reason})))

(defn- literal-conditions! [original]
  (let [expr (get-in original [:guard :expression])
        terms (if (= :and (first expr)) (rest expr) [expr])
        pairs (mapv (fn [[_ [_ field] [_ value] :as term]]
                      (when-not (and (= term [:= [:read field] [:literal value]])
                                     (contains? (:state-types original) field)
                                     (not (:optional? (get-in original [:state-types field]))))
                        (reject! :unsupported-guard-term))
                      [field value]) terms)]
    (when-not (and (seq pairs) (= (count pairs) (count (set (map first pairs)))))
      (reject! :duplicate-or-empty-guard))
    (into {} pairs)))

(defn- document-checks! [conditions coords all-fields layout]
  (when-not (and (vector? layout) (every? vector? layout))
    (reject! :invalid-document-layout))
  (let [fields (mapv second (filter #(= :state (first %)) layout))]
    (when-not (and (= (set fields) (set (keys coords))) (= (count fields) (count coords)))
      (reject! :incomplete-document-layout)))
  (mapv (fn [[kind field value :as entry]]
          (case kind
            :state (let [v (get conditions field)]
                     (when-not (and (= 2 (count entry)) (some? v))
                       (reject! :unsupported-document-literal))
                     [(nth (get coords field) 3) v])
            :extra (do (when-not (and (= 3 (count entry)) (keyword? field) (nil? (namespace field))
                                      (re-matches #"[a-z][a-z0-9_-]*" (name field)) (some? value)
                                      (not (contains? all-fields (xt-util/kw->normal-form-kw field))))
                         (reject! :invalid-extra-check))
                       [field value])
            (reject! :invalid-document-layout))) layout))

(defn lower-assertions!
  "Return exact formatted assertions for a restricted original guard. The
   ordered document layout names [:state abstract-field] or
   [:extra physical-field value] (trusted additional version checks).
   Every required-document guard field must occur exactly once. Extra
   checks may not alias any mapped field or document identity. Nil on a
   nullable document requires absence; nil on the command's own journal
   slot requires both slot and command-ID absence. Other cases fail closed.
   Does not enforce frame fields, outcome decoding or auxiliary effects."
  [original target-map binding document-layout]
  (plan/check-target-mapping! original target-map)
  (resolution/require-binding binding)
  (let [conditions (literal-conditions! original)
        coordinates (select-keys target-map (keys conditions))
        grouped (group-by (fn [[_ [kind table id]]] [kind table id]) coordinates)
        required (set (for [[[kind table id] _] grouped :when (= :document kind)] [table id]))]
    (when-not (and (map? document-layout) (= required (set (keys document-layout))))
      (reject! :incomplete-document-layout))
    (vec (mapcat
          (fn [[[kind table id] entries]]
            (let [coords (into {} entries)]
              (case kind
                :document
                (let [all-fields (into #{(xt-util/kw->normal-form-kw :xt/id)}
                                       (keep (fn [[k t i f]]
                                               (when (and (= k :document) (= t table) (= i id))
                                                 (xt-util/kw->normal-form-kw f)))) (vals target-map))
                      checks (document-checks! conditions coords all-fields (get document-layout [table id]))]
                  [(tx/format-assertion (tx/assert-document-current table {:model/id id :model/checks checks}))])
                :nullable-document
                (do (when-not (every? #(nil? (get conditions %)) (keys coords))
                      (reject! :unsupported-nullable-document-guard))
                    [(tx/format-assertion (tx/assert-document-absent table id))])
                :journal-slot
                (do (when-not (and (every? #(nil? (get conditions %)) (keys coords))
                                   (= table (identity/raw-value (:principal binding)))
                                   (= id (:operation binding)))
                      (reject! :unsupported-journal-guard))
                    [(tx/format-assertion (tx/assert-document-absent :gesso_commands (resolution/record-id binding)))
                     (resolution/principal-operation-slot-assertion binding)]))))
          (sort-by (comp pr-str key) grouped)))))
