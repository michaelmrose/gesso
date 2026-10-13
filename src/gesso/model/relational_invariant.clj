(ns gesso.model.relational-invariant
  "Finite, source-interpreted referential business invariants for Gesso.

   Many important rules can be stated without an opaque application predicate:
   when a source document has a reference, its referenced document must exist
   and satisfy explicit attribute equalities. This namespace owns that small
   rule's interpretation and makes its entire declaration inspectable.

   A declaration is ordinary Clojure data, for example:

     {:id :request/owner-membership-active
      :source-kind :request
      :source-where {}
      :reference-attr :owner-membership-id
      :allow-unset? true
      :target-kind :membership
      :target-where {:active? true}}

   A source missing a declared selector attribute is invalid, rather than
   silently omitted. Every matching source must contain :reference-attr.
   If the reference value
   is nil it is permitted only when :allow-unset? is true. Otherwise the value
   must identify a document in :target-kind whose attributes satisfy every
   :target-where equality. Missing target attributes do NOT equal nil.

   This is an inspectable, finite *rule constructor*, not a new specification
   language, a database foreign-key constraint, or a general Clojure analyzer.
   It supplies check-prepared and check-families adapters to the existing
   reference-model checkers; it does not mutate XTDB or claim to prove that
   application operations, identities, input domains, or modeled state are
   complete. Arbitrary business predicates still require separate treatment."
  (:require [gesso.model.invariant-check :as invariant]
            [gesso.model.operation-family :as family]
            [gesso.model.tx-model :as reference]))

(def assurance-scope
  {:rule-engine :gesso.model.relational-invariant/finite-reference-v1
   :checked #{:source-owned-reference-rule-interpretation
              :explicit-source-selection
              :presence-of-reference-attributes
              :target-document-existence
              :required-target-attribute-equalities
              :complete-declared-rule-evaluation-over-modeled-documents}
   :assumed #{:complete-finite-modeled-document-state
              :truthful-business-meaning-of-declared-relations
              :serial-atomic-commit-reference-semantics
              :pure-total-truthful-modeled-operation-builders}
   :not-established #{:completeness-of-business-rules
                      :completeness-of-documents-or-argument-domains
                      :authorization-of-real-principals
                      :correctness-of-real-application-callbacks
                      :real-xtdb-constraint-enforcement
                      :unbounded-invariant-preservation
                      :verified-projection-or-browser-semantics
                      :application-certification}})

(def ^:private declaration-keys
  #{:id :source-kind :source-where :reference-attr :allow-unset?
    :target-kind :target-where})

(defn- fail! [kind message details]
  (throw (ex-info message
                  {:error/type ::invalid-relational-invariant
                   :error/kind kind
                   :error/details details})))

(defn- scalar? [value]
  (or (nil? value) (boolean? value) (string? value)
      (keyword? value) (integer? value)
      (instance? java.util.UUID value)))

(defn- predicate-map? [value]
  (and (map? value)
       (every? keyword? (keys value))
       (every? scalar? (vals value))))

(defn- require-declarations! [declarations]
  (when-not (and (vector? declarations) (seq declarations))
    (fail! :invalid-declarations
           "Relational invariants require a nonempty vector of declarations."
           {:declarations declarations}))
  (doseq [[index declaration] (map-indexed vector declarations)]
    (when-not (and (map? declaration)
                   (= declaration-keys (set (keys declaration)))
                   (qualified-keyword? (:id declaration))
                   (keyword? (:source-kind declaration))
                   (predicate-map? (:source-where declaration))
                   (keyword? (:reference-attr declaration))
                   (boolean? (:allow-unset? declaration))
                   (keyword? (:target-kind declaration))
                   (predicate-map? (:target-where declaration))
                   (seq (:target-where declaration)))
      (fail! :invalid-declaration
             "Expected a named relation with source selection, reference, unset policy, and nonempty target requirements."
             {:index index :declaration declaration})))
  (when-not (= (count declarations) (count (set (map :id declarations))))
    (fail! :duplicate-declaration
           "Relational invariant identifiers must be distinct."
           {:ids (mapv :id declarations)}))
  declarations)

(defn- attribute-match? [document constraints]
  (and (map? document)
       (every? (fn [[attribute expected]]
                 (and (contains? document attribute)
                      (= expected (get document attribute))))
               constraints)))

(defn- reference-valid? [value]
  ;; Reference identifiers must be inspectable scalar document identities.
  ;; In particular, arbitrary host objects cannot silently become keys.
  (and (some? value) (scalar? value)))

(defn- source-violation [state declaration source-target source]
  (let [{:keys [reference-attr allow-unset? target-kind target-where]}
        declaration]
    (cond
      (not (contains? source reference-attr))
      {:source-target source-target :reason :missing-reference-attribute}

      (nil? (get source reference-attr))
      (when-not allow-unset?
        {:source-target source-target :reason :unset-reference})

      (not (reference-valid? (get source reference-attr)))
      {:source-target source-target :reason :invalid-reference-identity}

      :else
      (let [target [target-kind (get source reference-attr)]
            document (get state target)]
        (cond
          (nil? document)
          {:source-target source-target
           :reason :missing-target :target target}

          (not (attribute-match? document target-where))
          {:source-target source-target
           :reason :target-requirements-not-satisfied :target target})))))

(defn- violations* [state declaration]
  (->> state
       (keep (fn [[target source]]
               (when (= (:source-kind declaration) (first target))
                 (if-let [missing
                          (some (fn [attribute]
                                  (when-not (contains? source attribute)
                                    attribute))
                                (sort (keys (:source-where declaration))))]
                   {:source-target target
                    :reason :missing-source-selector-attribute
                    :attribute missing}
                   (when (attribute-match? source (:source-where declaration))
                     (source-violation state declaration target source))))))
       (sort-by (comp pr-str :source-target))
       vec))

(defn inspect
  "Inspect an admitted relation against a complete modeled document state.
   Returns the offending source targets and failure reasons; the caller must
   treat this as a potentially sensitive *local* diagnostic. An empty source
   table satisfies the universal rule vacuously and proves no coverage."
  [state declaration]
  (reference/require-state state)
  (require-declarations! [declaration])
  (let [violations (violations* state declaration)]
    {:id (:id declaration)
     :holds? (empty? violations)
     :violations violations
     :inspected-source-count
     (count (filter (fn [[target source]]
                      (and (= (:source-kind declaration) (first target))
                           (attribute-match? source (:source-where declaration))))
                    state))
     :scope assurance-scope}))

(defn compile-rules
  "Construct the precise {:id :holds?} maps accepted by existing invariant
   checkers from declarative, source-interpreted constraints. Do not treat
   copies of these functions, metadata, or arbitrary caller-authored rules as
   authenticated evidence of construction. For local reports retaining the
   declarations, prefer check-prepared or check-families below."
  [declarations]
  (mapv (fn [declaration]
          {:id (:id declaration)
           :holds? (fn [state]
                     (empty? (violations* state declaration)))})
        (require-declarations! declarations)))

(defn check-prepared
  "Check already prepared plans against declared relations using the existing
   finite serial reference model; preserve both the original checker scope
   and the narrower source-owned rule interpretation scope."
  [initial-state plans declarations]
  (let [rules (compile-rules declarations)
        verdict (invariant/check-prepared initial-state plans rules)]
    (assoc verdict
           :relational-declarations declarations
           :relational-scope assurance-scope)))

(defn check-families
  "Check state-dependent finite operation families against source-interpreted
   relations, preserving the existing bounded reachability and family scopes.
   Does not establish that family callbacks match registered executors."
  [initial-state families declarations options]
  (let [rules (compile-rules declarations)
        verdict (family/check-families initial-state families rules options)]
    (assoc verdict
           :relational-declarations declarations
           :relational-scope assurance-scope)))
