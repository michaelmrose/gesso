(ns gesso.model.core
  "Compiles declarative entity descriptors into ordinary Gesso model plumbing.

   Malli owns persisted structure. Domain functions own semantics. This
   namespace derives the repetitive middle:

   - persisted columns and normalization
   - ordinary by-id and equality Graph lookups
   - ordinary document Graph projections and their registry schemas
   - simple single-document Gesso FX operations
   - Biff 2 module assembly

   Complex Graph relationships and complex FX workflows stay application code
   and use the same gesso.model.command / gesso.model.tx primitives.

   Persisted fields opt into generated Graph projection on their Malli map
   entry:

     [:membership/skills
      {:gesso.model/graph true}
      skills-schema]

     [:membership/user
      {:gesso.model/graph :membership/user-id}
      :uuid]

   true means same-name projection; a keyword is an alias; absent/false means
   no generated projection.

   Example descriptor:

     {:entity-type :membership
      :document-schema membership-document-schema
      :identity {:graph-key :membership/id}
      :version membership/version
      :lookups [:membership/user]
      :operations
      {:suspend membership/suspend-command
       :reactivate membership/reactivate-command}
      :live
      {:change-fn membership-change
       :entry-fn change-entry}}

   Function-valued operations are conventional updates. A conventional create
   is explicit:

     :create
     {:kind :create
      :command membership/create-command}

   Generated create commands receive :id and :now. Generated updates receive
   (command current-document input), where the Graph identity is removed and
   :now is added. Anything that does not naturally fit those conventions
   belongs in ordinary hand-written gesso.fx code."
  (:require
   [com.biffweb.core :as biff.core]
   [com.biffweb.xtdb :as biff.xtdb]
   [com.biffweb.fx :as fx]
   [com.biffweb.graph :as graph]
   ;; [gesso.fx :as fx]
   ;; [gesso.graph :as graph]
   [gesso.model.command :as command]
   [gesso.model.schema :as model.schema]
   [gesso.model.tx :as model.tx]))

;; =============================================================================
;; Descriptor vocabulary
;; =============================================================================

(def descriptor-keys
  #{:entity-type :document-schema :identity :version :persistence
    :lookups :operations :live})

(def identity-keys #{:storage-key :graph-key})
(def persistence-keys #{:codec-overrides})
(def live-keys #{:change-fn :entry :entry-fn :emit :tx-options})
(def operation-keys #{:kind :command :result-key})
(def operation-kinds #{:create :update})

(def graph-property
  "Malli map-entry property controlling generated Graph projection."
  :gesso.model/graph)

;; =============================================================================
;; Errors and helpers
;; =============================================================================

(defn- fail!
  ([type message]
   (fail! type message nil))
  ([type message details]
   (throw
    (ex-info
     message
     (cond-> {:error/type type}
       (some? details) (assoc :error/details details))))))

(defn- require-map!
  [value type message]
  (when-not (map? value)
    (fail! type message {:value value}))
  value)

(defn- require-keyword!
  [value type message]
  (when-not (keyword? value)
    (fail! type message {:value value}))
  value)

(defn- require-qualified-keyword!
  [value type message]
  (when-not (qualified-keyword? value)
    (fail! type message {:value value}))
  value)

(defn- check-keys!
  [value allowed type context]
  (let [unknown (seq (remove allowed (keys value)))]
    (when unknown
      (fail! type
             (str context " contains unsupported keys.")
             {:unknown-keys (set unknown)
              :allowed-keys allowed})))
  value)

(defn- deref-if-needed [value]
  (if (instance? clojure.lang.IDeref value) @value value))

(defn- malli-options [ctx]
  (some-> (:biff/malli-opts ctx) deref-if-needed))

(defn- duplicates [values]
  (->> values
       frequencies
       (keep (fn [[value n]] (when (< 1 n) value)))
       set))

(defn- merge-disjoint
  [left right type message]
  (let [overlap (seq (filter #(contains? left %) (keys right)))]
    (when overlap
      (fail! type message {:keys (set overlap)}))
    (merge left right)))

(defn- entity-token [entity-type]
  (if-let [ns-part (namespace entity-type)]
    (str ns-part "." (name entity-type))
    (name entity-type)))

;; =============================================================================
;; Identity and generated names
;; =============================================================================

(defn identity-storage-key [descriptor]
  (get-in descriptor [:identity :storage-key] :xt/id))

(defn identity-graph-key [descriptor]
  (get-in descriptor [:identity :graph-key]))

(defn graph-namespace [descriptor]
  (or (namespace (identity-graph-key descriptor))
      (fail! ::missing-graph-namespace
             "Descriptor Graph identity must be qualified."
             {:identity (:identity descriptor)})))

(defn document-key [descriptor]
  (keyword (graph-namespace descriptor) "doc"))

(defn found-key [descriptor]
  (keyword (graph-namespace descriptor) "found?"))

(defn by-id-resolver-id [descriptor]
  (keyword "gesso.model.generated"
           (str (entity-token (:entity-type descriptor)) "-by-id")))

(defn fields-resolver-id [descriptor]
  (keyword "gesso.model.generated"
           (str (entity-token (:entity-type descriptor)) "-fields")))

(defn lookup-resolver-id [descriptor field]
  (keyword
   "gesso.model.generated"
   (str (entity-token (:entity-type descriptor))
        "-by-"
        (if-let [ns-part (namespace field)]
          (str ns-part "." (name field))
          (name field)))))

(defn operation-id
  "Qualified operation keys are used directly. Unqualified keys inherit the
   entity Graph namespace."
  [descriptor operation-key]
  (if (qualified-keyword? operation-key)
    operation-key
    (keyword (graph-namespace descriptor) (name operation-key))))

;; =============================================================================
;; Document field metadata
;; =============================================================================

(defn- field-map [descriptor]
  (into {}
        (map (juxt :key identity))
        (model.schema/map-entries (:document-schema descriptor))))

(defn- field!
  [descriptor field context]
  (or (get (field-map descriptor) field)
      (fail! ::unknown-document-field
             "Descriptor refers to an undeclared document field."
             {:entity-type (:entity-type descriptor)
              :field field
              :context context})))

(defn graph-projections
  "Returns generated non-identity field projections in schema declaration order."
  [descriptor]
  (let [storage-id (identity-storage-key descriptor)]
    (into
     []
     (keep
      (fn [{:keys [key optional? graph-shape schema-form properties]}]
        (let [setting (get properties graph-property)]
          (when (and setting (not= key storage-id))
            (let [graph-key
                  (cond
                    (true? setting) key
                    (keyword? setting) setting
                    :else
                    (fail! ::invalid-graph-property
                           ":gesso.model/graph must be true, false/nil, or a keyword alias."
                           {:entity-type (:entity-type descriptor)
                            :field key
                            :value setting}))]
              {:storage-key key
               :graph-key graph-key
               :optional? optional?
               :graph-shape graph-shape
               :schema-form schema-form})))))
     (model.schema/map-entries (:document-schema descriptor)))))

;; =============================================================================
;; Operation normalization
;; =============================================================================

(defn operation-config
  "Normalizes one operation. A non-map callable is update shorthand."
  [descriptor operation-key]
  (let [raw (get-in descriptor [:operations operation-key])
        config (if (and (ifn? raw) (not (map? raw)))
                 {:kind :update :command raw}
                 raw)]
    (-> config
        (require-map! ::invalid-operation
                      "Each generated operation must be a function or map.")
        (check-keys! operation-keys
                     ::unknown-operation-keys
                     "Generated operation"))
    (let [kind (get config :kind :update)
          command-fn (:command config)
          result-key (get config :result-key (:entity-type descriptor))]
      (when-not (contains? operation-kinds kind)
        (fail! ::invalid-operation-kind
               "Generated operation :kind must be :create or :update."
               {:operation operation-key :kind kind}))
      (when-not (ifn? command-fn)
        (fail! ::missing-operation-command
               "Generated operation requires a callable :command."
               {:operation operation-key :command command-fn}))
      (require-keyword! result-key
                        ::invalid-operation-result-key
                        "Generated operation :result-key must be a keyword.")
      (assoc config :kind kind :result-key result-key))))

;; =============================================================================
;; Descriptor validation
;; =============================================================================

(defn- validate-identity! [descriptor]
  (let [identity
        (-> (:identity descriptor)
            (require-map! ::invalid-identity "Descriptor :identity must be a map.")
            (check-keys! identity-keys
                         ::unknown-identity-keys
                         "Descriptor :identity"))
        storage-key (identity-storage-key descriptor)
        graph-key (identity-graph-key descriptor)
        info (field! descriptor storage-key :identity)]
    (require-keyword! storage-key
                      ::invalid-storage-identity
                      "Descriptor identity storage key must be a keyword.")
    (require-qualified-keyword! graph-key
                                ::invalid-graph-identity
                                "Descriptor identity Graph key must be qualified.")
    (when-not (= :scalar (:graph-shape info))
      (fail! ::non-scalar-identity
             "Descriptor identity field must be structurally scalar."
             {:field storage-key :graph-shape (:graph-shape info)})))
  descriptor)

(defn- validate-version! [descriptor]
  (let [version (command/require-version (:version descriptor))]
    (doseq [field
            (distinct
             (concat [(:revision-key version)
                      (:created-at-key version)
                      (:updated-at-key version)]
                     (command/compare-keys version)))]
      (field! descriptor field :version)))
  descriptor)

(defn- validate-persistence! [descriptor]
  (let [persistence (or (:persistence descriptor) {})]
    (-> persistence
        (require-map! ::invalid-persistence
                      "Descriptor :persistence must be a map.")
        (check-keys! persistence-keys
                     ::unknown-persistence-keys
                     "Descriptor :persistence"))
    (model.schema/effective-codecs
     (:document-schema descriptor)
     (:codec-overrides persistence)))
  descriptor)

(defn- validate-projections! [descriptor]
  (let [projections (graph-projections descriptor)
        graph-keys (map :graph-key projections)
        duplicate-keys (duplicates graph-keys)
        reserved #{(identity-graph-key descriptor)
                   (document-key descriptor)
                   (found-key descriptor)}]
    (when (seq duplicate-keys)
      (fail! ::duplicate-graph-projections
             "Generated Graph projection aliases must be unique."
             {:graph-keys duplicate-keys}))
    (when-let [collisions (seq (filter reserved graph-keys))]
      (fail! ::reserved-graph-projection
             "A projection collides with a generated identity/document/found attribute."
             {:graph-keys (set collisions)}))
    (doseq [{:keys [storage-key graph-key graph-shape]} projections]
      (require-qualified-keyword!
       graph-key
       ::invalid-projection-graph-key
       "Generated Graph projection keys must be qualified.")
      (when (= :unknown graph-shape)
        (fail! ::ambiguous-graph-projection
               "Generated Graph projection shape is ambiguous."
               {:field storage-key :graph-key graph-key}))))
  descriptor)

(defn- validate-lookups! [descriptor]
  (let [lookups (or (:lookups descriptor) [])]
    (when-not (sequential? lookups)
      (fail! ::invalid-lookups
             "Descriptor :lookups must be sequential."
             {:lookups lookups}))
    (when-not (every? keyword? lookups)
      (fail! ::invalid-lookup-field
             "Descriptor lookup fields must be keywords."
             {:lookups lookups}))
    (when-let [duplicates (seq (duplicates lookups))]
      (fail! ::duplicate-lookups
             "Descriptor lookup fields must be unique."
             {:lookups (set duplicates)}))
    (doseq [field lookups]
      (let [info (field! descriptor field :lookups)]
        (when-not (= :scalar (:graph-shape info))
          (fail! ::non-scalar-lookup
                 "Generated equality lookups require scalar fields."
                 {:field field :graph-shape (:graph-shape info)})))))
  descriptor)

(defn- validate-operations! [descriptor]
  (let [operations (or (:operations descriptor) {})]
    (when-not (map? operations)
      (fail! ::invalid-operations
             "Descriptor :operations must be a map."
             {:operations operations}))
    (doseq [operation-key (keys operations)]
      (require-keyword! operation-key
                        ::invalid-operation-key
                        "Generated operation names must be keywords.")
      (operation-config descriptor operation-key))
    (when-let [duplicate-ids
               (seq
                (duplicates
                 (map #(operation-id descriptor %)
                      (keys operations))))]
      (fail! ::duplicate-operation-ids
             "Generated operation handler ids must be unique."
             {:handler-ids (set duplicate-ids)})))
  descriptor)

(defn- validate-live! [descriptor]
  (let [config (or (:live descriptor) {})]
    (-> config
        (require-map! ::invalid-live-config "Descriptor :live must be a map.")
        (check-keys! live-keys ::unknown-live-keys "Descriptor :live"))
    (when-let [f (:change-fn config)]
      (when-not (ifn? f)
        (fail! ::invalid-change-fn
               "Descriptor :live/:change-fn must be callable."
               {:change-fn f})))
    (when-let [entry (:entry config)]
      (when-not (map? entry)
        (fail! ::invalid-live-entry
               "Descriptor :live/:entry must be a map."
               {:entry entry})))
    (when-let [f (:entry-fn config)]
      (when-not (ifn? f)
        (fail! ::invalid-live-entry-fn
               "Descriptor :live/:entry-fn must be callable."
               {:entry-fn f})))
    (when (and (:entry config) (:entry-fn config))
      (fail! ::ambiguous-live-entry
             "Descriptor :live may use :entry or :entry-fn, not both."
             {:live config}))
    (let [emit (get config :emit :async)]
      (when-not (contains? model.tx/valid-emit-modes emit)
        (fail! ::invalid-live-emit
               "Descriptor :live/:emit must be :async, :sync, or false."
               {:emit emit}))
      (when (and (seq (:operations descriptor))
                 (not= false emit)
                 (nil? (:change-fn config)))
        (fail! ::missing-change-fn
               "Publishing generated operations require :live/:change-fn."
               {:entity-type (:entity-type descriptor)})))
    (when-let [tx-options (:tx-options config)]
      (when-not (map? tx-options)
        (fail! ::invalid-live-tx-options
               "Descriptor :live/:tx-options must be a map."
               {:tx-options tx-options}))))
  descriptor)

(defn validate-descriptor
  "Validates and returns one entity descriptor."
  [descriptor]
  (-> descriptor
      (require-map! ::invalid-descriptor
                    "A Gesso model descriptor must be a map.")
      (check-keys! descriptor-keys
                   ::unknown-descriptor-keys
                   "Gesso model descriptor"))
  (require-keyword! (:entity-type descriptor)
                    ::invalid-entity-type
                    "Descriptor :entity-type must be a keyword.")
  (when-not (:document-schema descriptor)
    (fail! ::missing-document-schema
           "Descriptor requires :document-schema."
           {:entity-type (:entity-type descriptor)}))
  (model.schema/document-map-schema (:document-schema descriptor))
  (-> descriptor
      validate-identity!
      validate-version!
      validate-persistence!
      validate-projections!
      validate-lookups!
      validate-operations!
      validate-live!)
  descriptor)

(defn descriptor? [value]
  (try
    (validate-descriptor value)
    true
    (catch Throwable _ false)))

;; =============================================================================
;; Generated schema registry
;; =============================================================================

(defn- assoc-schema!
  [registry key schema source]
  (if-let [existing (get registry key)]
    (if (= existing schema)
      registry
      (fail! ::schema-collision
             "Generated declarations assign different schemas to one registry key."
             {:key key :existing existing :new schema :source source}))
    (assoc registry key schema)))

(defn generated-schema
  "Derives Biff/Malli registry entries for persisted documents and generated
   Graph attributes."
  [descriptor]
  (validate-descriptor descriptor)
  (let [document-schema (:document-schema descriptor)
        storage-id (identity-storage-key descriptor)
        graph-id (identity-graph-key descriptor)
        base
        (-> {}
            (assoc-schema! (:entity-type descriptor)
                           document-schema
                           :entity)
            (assoc-schema! (document-key descriptor)
                           document-schema
                           :document)
            (assoc-schema! (found-key descriptor)
                           :boolean
                           :found)
            (assoc-schema! graph-id
                           (model.schema/graph-field-schema
                            document-schema storage-id)
                           :identity))
        with-projections
        (reduce
         (fn [registry {:keys [storage-key graph-key]}]
           (assoc-schema!
            registry
            graph-key
            (model.schema/graph-field-schema document-schema storage-key)
            [:projection storage-key]))
         base
         (graph-projections descriptor))]
    (reduce
     (fn [registry lookup-field]
       (assoc-schema!
        registry
        lookup-field
        (model.schema/graph-field-schema document-schema lookup-field)
        [:lookup lookup-field]))
     with-projections
     (:lookups descriptor))))

;; =============================================================================
;; Generated Graph query contracts
;; =============================================================================

(defn document-columns [descriptor]
  (validate-descriptor descriptor)
  (model.schema/field-keys (:document-schema descriptor)))

(defn document-query [_descriptor] [:*])

(defn lookup-query [descriptor]
  (validate-descriptor descriptor)
  [(found-key descriptor)
   {[:? (document-key descriptor)] [:*]}])

(defn- optional-key [optional? key]
  (if optional? [:? key] key))

(defn- projection-query-item
  [{:keys [graph-key optional? graph-shape]}]
  (case graph-shape
    :scalar
    (optional-key optional? graph-key)

    :join
    {(optional-key optional? graph-key) [:*]}

    (fail! ::ambiguous-graph-projection
           "Cannot generate a query item for an ambiguous projection."
           {:graph-key graph-key :graph-shape graph-shape})))

(defn field-query [descriptor]
  (validate-descriptor descriptor)
  (into [(identity-graph-key descriptor)]
        (map projection-query-item)
        (graph-projections descriptor)))

;; =============================================================================
;; Persistence read boundary
;; =============================================================================

(defn- query-context!
  [ctx]
  (if
   (and
    (map?
     ctx)
    (or
     (:biff.xtdb/connection-pool ctx)
     (:biff.xtdb/node ctx)))
    ctx

    (fail!
     ::missing-biff-connection
     "Generated model reads require Biff 2 XTDB context with :biff.xtdb/connection-pool or :biff.xtdb/node."
     {:ctx-keys
      (when
       (map?
        ctx)
       (set
        (keys
         ctx)))})))

(defn- normalize-loaded-document [descriptor ctx document]
  (model.schema/normalize-and-validate
   (:document-schema descriptor)
   document
   {:codec-overrides (get-in descriptor [:persistence :codec-overrides])
    :malli-options (malli-options ctx)}))

(defn load-by-id
  "Loads one current document through Biff's XTDB2 helper."
  [descriptor ctx id]
  (validate-descriptor descriptor)
  (when (some? id)
    (when-let [raw
               (first
                (biff.xtdb/q
                 (query-context! ctx)
                 {:select (document-columns descriptor)
                  :from [(:entity-type descriptor)]
                  :where [:= (identity-storage-key descriptor) id]}))]
      (normalize-loaded-document descriptor ctx raw))))

(defn load-by-lookup
  "Loads at most one current document through a declared scalar equality lookup."
  [descriptor ctx field value]
  (validate-descriptor descriptor)
  (when-not (some #{field} (:lookups descriptor))
    (fail! ::unsupported-lookup
           "The field is not declared as a conventional lookup."
           {:entity-type (:entity-type descriptor)
            :field field
            :lookups (vec (:lookups descriptor))}))
  (if (nil? value)
    nil
    (let [documents
          (mapv
           #(normalize-loaded-document descriptor ctx %)
           (biff.xtdb/q
            (query-context! ctx)
            {:select (document-columns descriptor)
             :from [(:entity-type descriptor)]
             :where [:= field value]}))]
      (case (count documents)
        0 nil
        1 (first documents)
        (fail! ::non-unique-lookup
               "A conventional lookup returned more than one current document."
               {:entity-type (:entity-type descriptor)
                :field field
                :value value
                :result-count (count documents)})))))

;; =============================================================================
;; Generated Graph resolvers
;; =============================================================================

(defn project-document
  "Projects the identity plus schema-annotated Graph fields."
  [descriptor document]
  (validate-descriptor descriptor)
  (when document
    (reduce
     (fn [result {:keys [storage-key graph-key]}]
       (let [value (get document storage-key ::missing)]
         (if (or (= ::missing value) (nil? value))
           result
           (assoc result graph-key value))))
     {(identity-graph-key descriptor)
      (get document (identity-storage-key descriptor))}
     (graph-projections descriptor))))

(defn- lookup-result [descriptor document]
  (if document
    {(found-key descriptor) true
     (document-key descriptor) document}
    {(found-key descriptor) false}))

(defn build-by-id-resolver [descriptor]
  (validate-descriptor descriptor)
  (let [graph-id (identity-graph-key descriptor)]
    (graph/resolver
     {:id (by-id-resolver-id descriptor)
      :input [graph-id]
      :output (lookup-query descriptor)
      :resolve-fn
      (fn [ctx input]
        (lookup-result
         descriptor
         (load-by-id descriptor ctx (get input graph-id))))})))

(defn build-field-resolver [descriptor]
  (validate-descriptor descriptor)
  (let [doc-key (document-key descriptor)]
    (graph/resolver
     {:id (fields-resolver-id descriptor)
      :input [{doc-key [:*]}]
      :output (field-query descriptor)
      :resolve-fn
      (fn [_ctx input]
        (project-document descriptor (get input doc-key)))})))

(defn build-lookup-resolver [descriptor field]
  (validate-descriptor descriptor)
  (when-not (some #{field} (:lookups descriptor))
    (fail! ::unsupported-lookup
           "Cannot build an undeclared conventional lookup."
           {:field field}))
  (graph/resolver
   {:id (lookup-resolver-id descriptor field)
    :input [field]
    :output (lookup-query descriptor)
    :resolve-fn
    (fn [ctx input]
      (lookup-result
       descriptor
       (load-by-lookup descriptor ctx field (get input field))))}))

(defn build-resolvers [descriptor]
  (validate-descriptor descriptor)
  (into [(build-by-id-resolver descriptor)
         (build-field-resolver descriptor)]
        (map #(build-lookup-resolver descriptor %))
        (:lookups descriptor)))

;; =============================================================================
;; Public conventional reads
;; =============================================================================

(defn facts
  "Executes a generated lookup through Gesso Graph.

   lookup is :id or one descriptor-declared persisted lookup field."
  [descriptor ctx lookup value]
  (validate-descriptor descriptor)
  (let [input-key
        (if (= :id lookup)
          (identity-graph-key descriptor)
          lookup)]
    (when-not (or (= :id lookup)
                  (some #{lookup} (:lookups descriptor)))
      (fail! ::unsupported-lookup
             "The requested conventional lookup is not declared."
             {:lookup lookup :entity-type (:entity-type descriptor)}))
    (graph/query ctx
                 {input-key value}
                 (lookup-query descriptor))))

(defn require-document
  "Returns one canonical loaded document or throws when it is absent."
  [descriptor ctx lookup value]
  (let [result (facts descriptor ctx lookup value)]
    (if (true? (get result (found-key descriptor)))
      (or (get result (document-key descriptor))
          (fail! ::incomplete-graph-result
                 "Graph reported a found document without returning it."
                 {:entity-type (:entity-type descriptor)
                  :lookup lookup
                  :value value}))
      (fail! ::document-not-found
             "The requested model document does not exist."
             {:entity-type (:entity-type descriptor)
              :lookup lookup
              :value value}))))

;; =============================================================================
;; Generated transaction plans
;; =============================================================================

(defn- normalize-changes [descriptor model-command]
  (let [change-fn (get-in descriptor [:live :change-fn])
        value (when change-fn (change-fn model-command))]
    (cond
      (nil? value) []
      (map? value) [value]
      (sequential? value)
      (let [value (vec value)]
        (when-not (every? map? value)
          (fail! ::invalid-generated-changes
                 ":live/:change-fn must return nil, a map, or maps."
                 {:changes value}))
        value)
      :else
      (fail! ::invalid-generated-changes
             ":live/:change-fn must return nil, a map, or maps."
             {:changes value}))))

(defn- transaction-plan [descriptor model-command]
  (let [live-config (or (:live descriptor) {})
        plan {:commands [model-command]
              :changes (normalize-changes descriptor model-command)
              :emit (get live-config :emit :async)}]
    (cond-> plan
      (:entry live-config)
      (assoc :entry (:entry live-config))

      (:entry-fn live-config)
      (assoc :entry-fn (:entry-fn live-config))

      (:tx-options live-config)
      (assoc :tx-options (:tx-options live-config)))))

(defn- require-command-for-entity!
  [descriptor operation-key model-command]
  (command/require-command model-command)
  (when-not (= (:entity-type descriptor)
               (:model/entity-type model-command))
    (fail! ::wrong-command-entity
           "Generated operation command targets the wrong entity."
           {:operation operation-key
            :expected (:entity-type descriptor)
            :actual (:model/entity-type model-command)}))
  model-command)

(defn- require-create-command!
  [descriptor operation-key model-command malli-options]
  (require-command-for-entity! descriptor operation-key model-command)
  (when-not (command/create? model-command)
    (fail! ::expected-create-command
           "A generated :create operation must return a create command."
           {:operation operation-key :command model-command}))
  (model.schema/require-valid
   (:document-schema descriptor)
   (command/after model-command)
   malli-options)
  model-command)

(defn- require-update-command!
  [descriptor operation-key current model-command malli-options]
  (require-command-for-entity! descriptor operation-key model-command)
  (when-not (command/update? model-command)
    (fail! ::expected-update-command
           "A generated :update operation must return an update command."
           {:operation operation-key :command model-command}))
  (when-not (= current (command/before model-command))
    (fail! ::command-before-mismatch
           "Update command :model/before must equal the Graph-loaded document."
           {:operation operation-key
            :loaded current
            :command-before (command/before model-command)}))
  (model.schema/require-valid
   (:document-schema descriptor)
   (command/after model-command)
   malli-options)
  model-command)

;; =============================================================================
;; Generated Gesso FX operations
;; =============================================================================

(defn- ensure-input-map! [operation-key input]
  (when-not (map? input)
    (fail! ::invalid-operation-input
           "Generated operation input must be a map."
           {:operation operation-key :input input}))
  input)

(defn- operation-result [result-key model-command transaction]
  {result-key (command/after model-command)
   :transaction transaction})

(defn- build-create-operation
  [descriptor operation-key config]
  (let [machine-id (operation-id descriptor operation-key)
        command-fn (:command config)
        result-key (:result-key config)
        machine
        (fx/machine
         machine-id

         :start
         (fn [{::keys [operation-input]
               :biff.fx/keys [now seed]
               :as ctx}]
           (ensure-input-map! operation-key operation-input)
           (when (contains? operation-input :id)
             (fail! ::reserved-create-input
                    "Generated creates own :id; caller-supplied ids require custom FX."
                    {:operation operation-key}))
           (let [[id _] (fx/uuid7 seed now)
                 model-command
                 (command-fn
                  (assoc operation-input :id id :now now))
                 model-command
                 (require-create-command!
                  descriptor
                  operation-key
                  model-command
                  (malli-options ctx))]
             {::model-command model-command
              ::transaction
              [model.tx/transact-effect
               (transaction-plan descriptor model-command)]
              :biff.fx/next :finish}))

         :finish
         (fn [{::keys [model-command transaction]}]
           {:biff.fx/return
            (operation-result result-key model-command transaction)}))]
    (with-meta
      (fn generated-create [ctx input]
        (machine (assoc ctx ::operation-input input)))
      {:gesso.model/entity-type (:entity-type descriptor)
       :gesso.model/operation operation-key
       :gesso.model/operation-id machine-id
       :gesso.model/kind :create})))

(defn- build-update-operation
  [descriptor operation-key config]
  (let [machine-id (operation-id descriptor operation-key)
        command-fn (:command config)
        result-key (:result-key config)
        graph-id (identity-graph-key descriptor)
        doc-key (document-key descriptor)
        found-attr (found-key descriptor)
        machine
        (fx/machine
         machine-id

         :start
         (fn [{::keys [operation-input]}]
           (ensure-input-map! operation-key operation-input)
           (let [id (get operation-input graph-id)]
             (when (nil? id)
               (fail! ::missing-operation-identity
                      "Generated update input must contain the Graph identity."
                      {:operation operation-key
                       :identity-key graph-id
                       :input operation-input}))
             {::operation-input operation-input
              ::facts
              [:biff.graph.fx/query
               {graph-id id}
               (lookup-query descriptor)]
              :biff.fx/next :command}))

         :command
         (fn [{::keys [operation-input facts]
               :biff.fx/keys [now]
               :as ctx}]
           (when-not (true? (get facts found-attr))
             (fail! ::document-not-found
                    "Generated update could not find the current document."
                    {:operation operation-key
                     :identity-key graph-id
                     :id (get operation-input graph-id)}))
           (let [current
                 (or (get facts doc-key)
                     (fail! ::incomplete-graph-result
                            "Graph reported a found document without returning it."
                            {:operation operation-key
                             :document-key doc-key}))
                 domain-input
                 (assoc (dissoc operation-input graph-id) :now now)
                 model-command
                 (command-fn current domain-input)
                 model-command
                 (require-update-command!
                  descriptor
                  operation-key
                  current
                  model-command
                  (malli-options ctx))]
             {::model-command model-command
              ::transaction
              [model.tx/transact-effect
               (transaction-plan descriptor model-command)]
              :biff.fx/next :finish}))

         :finish
         (fn [{::keys [model-command transaction]}]
           {:biff.fx/return
            (operation-result result-key model-command transaction)}))]
    (with-meta
      (fn generated-update [ctx input]
        (machine (assoc ctx ::operation-input input)))
      {:gesso.model/entity-type (:entity-type descriptor)
       :gesso.model/operation operation-key
       :gesso.model/operation-id machine-id
       :gesso.model/kind :update})))

(defn build-operation [descriptor operation-key]
  (validate-descriptor descriptor)
  (let [config (operation-config descriptor operation-key)]
    (case (:kind config)
      :create (build-create-operation descriptor operation-key config)
      :update (build-update-operation descriptor operation-key config))))

(defn build-operations [descriptor]
  (validate-descriptor descriptor)
  (into {}
        (map (fn [operation-key]
               [operation-key
                (build-operation descriptor operation-key)]))
        (keys (:operations descriptor))))

(defn build-operation-handlers [descriptor]
  (into {}
        (map
         (fn [[operation-key operation]]
           [(operation-id descriptor operation-key)
            operation]))
        (build-operations descriptor)))

;; =============================================================================
;; Descriptor collection validation
;; =============================================================================

(defn- validate-descriptors! [descriptors]
  (when-not (sequential? descriptors)
    (fail! ::invalid-descriptor-collection
           "Model compilation requires sequential descriptors."
           {:descriptors descriptors}))
  (let [descriptors (mapv validate-descriptor descriptors)
        duplicate-entities (duplicates (map :entity-type descriptors))
        resolver-ids
        (mapcat
         (fn [descriptor]
           (concat
            [(by-id-resolver-id descriptor)
             (fields-resolver-id descriptor)]
            (map #(lookup-resolver-id descriptor %)
                 (:lookups descriptor))))
         descriptors)
        duplicate-resolvers (duplicates resolver-ids)
        operation-ids
        (mapcat
         (fn [descriptor]
           (map #(operation-id descriptor %)
                (keys (:operations descriptor))))
         descriptors)
        duplicate-operations (duplicates operation-ids)]
    (when (seq duplicate-entities)
      (fail! ::duplicate-entity-types
             "Each model entity type may be compiled once."
             {:entity-types duplicate-entities}))
    (when (seq duplicate-resolvers)
      (fail! ::duplicate-generated-resolver-ids
             "Generated resolver ids must be unique."
             {:resolver-ids duplicate-resolvers}))
    (when (seq duplicate-operations)
      (fail! ::duplicate-generated-operation-ids
             "Generated operation ids must be unique."
             {:operation-ids duplicate-operations}))
    descriptors))

;; =============================================================================
;; Biff module assembly
;; =============================================================================

(def extension-keys #{:schema :resolvers :fx-handlers})

(defn- combined-generated-schema [descriptors]
  (reduce
   (fn [registry descriptor]
     (reduce-kv
      (fn [registry key schema]
        (assoc-schema! registry key schema (:entity-type descriptor)))
      registry
      (generated-schema descriptor)))
   {}
   descriptors))

(defn- schema-init
  "Returns a Biff 2 module initializer that installs schemas into the global
   biff.core registry.

   The module also retains its :schema value during the staged Gesso migration
   because the current gesso.graph implementation still consumes that key."
  [schema]
  (fn [_modules-var]
    (biff.core/register
     schema)
    {}))

(defn build-module
  "Compiles descriptors plus explicit application escape hatches.

   extensions:
     {:schema      {...}
      :resolvers   [...]
      :fx-handlers {...}}

   Schemas are registered with Biff 2 through :biff.core/init. The :schema
   key remains in the returned module during the staged Gesso migration because
   current gesso.graph still consumes it.

   Install (gesso.model.tx/module) separately once for the application."
  ([descriptors]
   (build-module descriptors nil))
  ([descriptors extensions]
   (let [descriptors (validate-descriptors! descriptors)
         extensions (or extensions {})
         _
         (-> extensions
             (require-map! ::invalid-module-extensions
                           "Model module extensions must be a map.")
             (check-keys! extension-keys
                          ::unknown-module-extension-keys
                          "Model module extensions"))
         generated-schema (combined-generated-schema descriptors)
         custom-schema (or (:schema extensions) {})
         _
         (when-not (map? custom-schema)
           (fail! ::invalid-custom-schema
                  "Model extension :schema must be a map."
                  {:schema custom-schema}))
         complete-schema
         (merge-disjoint
          generated-schema custom-schema
          ::custom-schema-collision
          "Custom schema keys must not replace generated model schemas.")
         generated-resolvers
         (into [] (mapcat build-resolvers) descriptors)
         custom-resolvers (or (:resolvers extensions) [])
         _
         (when-not (sequential? custom-resolvers)
           (fail! ::invalid-custom-resolvers
                  "Model extension :resolvers must be sequential."
                  {:resolvers custom-resolvers}))
         complete-resolvers (into generated-resolvers custom-resolvers)
         duplicate-resolver-ids
         (duplicates (map :biff.graph/id complete-resolvers))
         _
         (when (seq duplicate-resolver-ids)
           (fail! ::duplicate-resolver-ids
                  "Generated and custom resolver ids must be unique."
                  {:resolver-ids duplicate-resolver-ids}))
         generated-handlers
         (into {}
               (mapcat (comp seq build-operation-handlers))
               descriptors)
         custom-handlers (or (:fx-handlers extensions) {})
         _
         (when-not (map? custom-handlers)
           (fail! ::invalid-custom-fx-handlers
                  "Model extension :fx-handlers must be a map."
                  {:fx-handlers custom-handlers}))
         complete-handlers
         (merge-disjoint
          generated-handlers custom-handlers
          ::custom-fx-handler-collision
          "Custom FX handlers must not replace generated model operations.")]
     (cond->
      {:schema
       complete-schema

       :biff.core/init
       (schema-init
        complete-schema)

       :biff.graph/resolvers
       complete-resolvers}
       (seq complete-handlers)
       (assoc :biff.fx/handlers complete-handlers)))))

;; =============================================================================
;; Inspectable compilation
;; =============================================================================

(defn compile-model
  "Compiles one descriptor into ordinary inspectable data/functions."
  [descriptor]
  (validate-descriptor descriptor)
  (let [schema (generated-schema descriptor)
        resolvers (build-resolvers descriptor)
        operations (build-operations descriptor)
        handlers
        (into {}
              (map
               (fn [[operation-key operation]]
                 [(operation-id descriptor operation-key)
                  operation]))
              operations)]
    {:descriptor descriptor
     :schema schema
     :document-columns (document-columns descriptor)
     :document-query (document-query descriptor)
     :lookup-query (lookup-query descriptor)
     :field-query (field-query descriptor)
     :projections (graph-projections descriptor)
     :resolvers resolvers
     :operations operations
     :fx-handlers handlers
     :module
     (cond->
      {:schema
       schema

       :biff.core/init
       (schema-init
        schema)

       :biff.graph/resolvers
       resolvers}
       (seq handlers)
       (assoc :biff.fx/handlers handlers))}))
