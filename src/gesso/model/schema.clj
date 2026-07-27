(ns gesso.model.schema
  "Malli-backed structural metadata and normalization for gesso.model.

   A persisted document schema is the canonical declaration of field shape and
   scalar validation. gesso.model derives mechanical consequences from it
   instead of asking each application model to repeat the same field lists in
   Graph and persistence code.

   A conventional field may attach Gesso metadata directly to its Malli schema:

     (def instant-schema
       [:fn
        {:gesso.model/codec :instant
         :error/message \"must be a java.time.Instant\"}
        timestamp-value?])

   Then every document field using instant-schema automatically inherits the
   persistence decoder. Descriptor-level codec overrides remain available for
   exceptional cases.

   This namespace owns:

   - document-map introspection
   - field structural shape
   - safe Graph scalar/join inference
   - field schema extraction for generated Graph registry entries
   - standard and custom codecs
   - codec derivation from Malli properties
   - persistence normalization
   - generic Malli validation helpers

   It does not query XTDB, construct Graph resolvers, define application
   authorization, or run model transitions."
  (:require
   [clojure.string :as str]
   [malli.core :as m]
   [malli.error :as me])
  (:import
   [java.time Instant OffsetDateTime ZonedDateTime]
   [java.util UUID]))

;; =============================================================================
;; Errors
;; =============================================================================

(defn- fail!
  ([error-type message]
   (fail! error-type message nil))
  ([error-type message details]
   (throw
    (ex-info
     message
     (cond-> {:error/type error-type}
       (some? details)
       (assoc :error/details details))))))

;; =============================================================================
;; Malli access
;; =============================================================================

(defn schema-object
  "Returns schema as a compiled Malli schema.

   malli-options is passed directly to malli.core/schema so application schema
   registries stay application-owned."
  ([schema]
   (schema-object schema nil))
  ([schema malli-options]
   (try
     (if malli-options
       (m/schema schema malli-options)
       (m/schema schema))
     (catch Throwable cause
       (throw
        (ex-info
         "Could not compile Malli schema."
         {:error/type ::invalid-schema
          :error/details {:schema schema}}
         cause))))))

(defn schema-form
  "Returns the canonical Malli form for schema."
  ([schema]
   (schema-form schema nil))
  ([schema malli-options]
   (m/form
    (schema-object
     schema
     malli-options))))

(defn- deref-schema
  [schema malli-options]
  (let [schema
        (schema-object
         schema
         malli-options)]
    (if malli-options
      (m/deref schema malli-options)
      (m/deref schema))))

(defn- schema-properties
  [schema malli-options]
  (or
   (m/properties
    (schema-object
     schema
     malli-options))
   {}))

(def wrapper-types
  "Single-child schema types whose structural meaning is inherited from their
   child."
  #{:maybe
    :schema
    :malli.core/val})

(def sequential-schema-types
  #{:vector
    :sequential})

(def collection-schema-types
  #{:vector
    :sequential
    :set})

;; =============================================================================
;; Structural shape
;; =============================================================================

(declare schema-shape)

(defn- and-shape
  [schema malli-options]
  (let [shapes
        (->> (m/children schema)
             (map
              #(schema-shape
                %
                malli-options))
             (remove
              #{:scalar
                :unknown})
             distinct
             vec)]
    (case
     (count shapes)

      0
      :scalar

      1
      (first shapes)

      :unknown)))

(defn schema-shape
  "Returns one broad runtime structural shape:

     :scalar
     :map
     :collection
     :unknown

   Ambiguous unions intentionally return :unknown rather than guessing."
  ([schema]
   (schema-shape schema nil))
  ([schema malli-options]
   (let [schema
         (deref-schema
          schema
          malli-options)

         type
         (m/type schema)]
     (cond
       (= :map type)
       :map

       (contains?
        collection-schema-types
        type)
       :collection

       (= :and type)
       (and-shape
        schema
        malli-options)

       (contains?
        wrapper-types
        type)
       (if-let [child
                (first
                 (m/children schema))]
         (schema-shape
          child
          malli-options)
         :unknown)

       (contains?
        #{:or
          :orn
          :multi}
        type)
       :unknown

       :else
       :scalar))))

(defn- sequential-element-schema
  [schema malli-options]
  (let [schema
        (deref-schema
         schema
         malli-options)

        type
        (m/type schema)]
    (cond
      (contains?
       sequential-schema-types
       type)
      (first
       (m/children schema))

      (= :and type)
      (some
       #(sequential-element-schema
         %
         malli-options)
       (m/children schema))

      (contains?
       wrapper-types
       type)
      (when-let [child
                 (first
                  (m/children schema))]
        (sequential-element-schema
         child
         malli-options))

      :else
      nil)))

(defn graph-shape
  "Returns the safe default Gesso Graph value shape:

     :scalar
     :join
     :unknown

   Maps are joins. Sequential collections of maps are joins. Collections of
   scalar values remain Graph scalars.

   This describes values already returned by a resolver. It never implies an
   XTDB relationship or an extra database query."
  ([schema]
   (graph-shape schema nil))
  ([schema malli-options]
   (case
    (schema-shape
     schema
     malli-options)

     :map
     :join

     :collection
     (if-let [element-schema
              (sequential-element-schema
               schema
               malli-options)]
       (case
        (schema-shape
         element-schema
         malli-options)

        :map
        :join

        :unknown
        :unknown

        :scalar)
       :scalar)

     :unknown
     :unknown

     :scalar)))

;; =============================================================================
;; Conventional document-map introspection
;; =============================================================================

(defn- find-map-schema
  [schema malli-options]
  (let [schema
        (deref-schema
         schema
         malli-options)

        type
        (m/type schema)]
    (cond
      (= :map type)
      schema

      (= :and type)
      (some
       #(find-map-schema
         %
         malli-options)
       (m/children schema))

      (contains?
       wrapper-types
       type)
      (when-let [child
                 (first
                  (m/children schema))]
        (find-map-schema
         child
         malli-options))

      :else
      nil)))

(defn document-map-schema
  "Returns the structural :map inside a conventional persisted document schema.

   This supports the common form:

     [:and
      [:map ...]
      [:fn domain-document-consistent?]]

   The domain predicate remains part of the full document schema; this function
   only finds the map needed for mechanical field derivation."
  ([document-schema]
   (document-map-schema
    document-schema
    nil))
  ([document-schema malli-options]
   (or
    (find-map-schema
     document-schema
     malli-options)

    (fail!
     ::missing-document-map
     "A conventional model document schema must contain a structural Malli :map."
     {:schema document-schema}))))

(defn- raw-map-entry
  [[key entry-properties child-schema]]
  {:key key
   :entry-properties (or entry-properties {})
   :schema child-schema})

(defn field-info
  "Returns normalized metadata for one document field, or nil when absent.

   Field properties merge Malli child-schema properties over map-entry
   properties. This allows reusable scalar schemas such as instant-schema to
   carry :gesso.model/codec once while still permitting a particular map entry
   to override it."
  ([document-schema key]
   (field-info
    document-schema
    key
    nil))
  ([document-schema key malli-options]
   (some
    (fn [entry]
      (let [{entry-key :key
             entry-properties :entry-properties
             child-schema :schema}
            (raw-map-entry
             entry)]
        (when
         (= key
            entry-key)
          (let [child-properties
                (schema-properties
                 child-schema
                 malli-options)

                properties
                (merge
                 child-properties
                 entry-properties)]
            {:key
             entry-key

             :optional?
             (true?
              (:optional
               entry-properties))

             :entry-properties
             entry-properties

             :schema-properties
             child-properties

             :properties
             properties

             :schema
             child-schema

             :schema-form
             (schema-form
              child-schema
              malli-options)

             :shape
             (schema-shape
              child-schema
              malli-options)

             :graph-shape
             (graph-shape
              child-schema
              malli-options)}))))
    (m/children
     (document-map-schema
      document-schema
      malli-options)))))

(defn map-entries
  "Returns normalized field metadata in declaration order."
  ([document-schema]
   (map-entries
    document-schema
    nil))
  ([document-schema malli-options]
   (mapv
    (fn [entry]
      (let [{:keys [key]}
            (raw-map-entry
             entry)]
        (field-info
         document-schema
         key
         malli-options)))
    (m/children
     (document-map-schema
      document-schema
      malli-options)))))

(defn field-keys
  "Returns persisted document field keys in declaration order."
  ([document-schema]
   (field-keys
    document-schema
    nil))
  ([document-schema malli-options]
   (mapv
    :key
    (map-entries
     document-schema
     malli-options))))

(defn field-schema
  "Returns a field's Malli schema form, or nil when absent."
  ([document-schema key]
   (field-schema
    document-schema
    key
    nil))
  ([document-schema key malli-options]
   (:schema-form
    (field-info
     document-schema
     key
     malli-options))))

(defn optional-field?
  "Returns true only when key is declared optional at the map-entry level."
  ([document-schema key]
   (optional-field?
    document-schema
    key
    nil))
  ([document-schema key malli-options]
   (true?
    (:optional?
     (field-info
      document-schema
      key
      malli-options)))))

;; =============================================================================
;; Graph query derivation
;; =============================================================================

(defn default-graph-query-item
  "Returns the safe Graph query item for one field metadata map.

   Required scalar:
     :example/name

   Optional scalar:
     [:? :example/name]

   Required join:
     {:example/context [:*]}

   Optional join:
     {[:? :example/context] [:*]}

   Returns nil for ambiguous shapes."
  [{:keys [key
           optional?
           graph-shape]}]
  (case
   graph-shape

    :scalar
    (if optional?
      [:? key]
      key)

    :join
    {(if optional?
       [:? key]
       key)
     [:*]}

    :unknown
    nil))

(defn default-graph-query
  "Returns safe structural Graph query items for every unambiguous document
   field.

   This does not choose public fields. gesso.model.core applies descriptor
   exposure and aliases."
  ([document-schema]
   (default-graph-query
    document-schema
    nil))
  ([document-schema malli-options]
   (into
    []
    (keep
     default-graph-query-item)
    (map-entries
     document-schema
     malli-options))))

;; =============================================================================
;; Standard codecs
;; =============================================================================

(defn normalize-instant
  "Normalizes common XTDB/JDBC temporal values to java.time.Instant.

   Unknown values are preserved so the document schema, rather than the codec,
   reports invalid application values."
  [value]
  (cond
    (nil? value)
    nil

    (instance?
     Instant
     value)
    value

    (instance?
     ZonedDateTime
     value)
    (.toInstant
     ^ZonedDateTime value)

    (instance?
     OffsetDateTime
     value)
    (.toInstant
     ^OffsetDateTime value)

    :else
    value))

(defn trim-string
  [value]
  (if
   (string?
    value)
    (str/trim
     value)
    value))

(defn blank-string->nil
  [value]
  (if
   (and
    (string?
     value)
    (str/blank?
     value))
    nil
    value))

(defn normalize-uuid
  "Parses a UUID string and preserves malformed values for later validation."
  [value]
  (cond
    (nil? value)
    nil

    (instance?
     UUID
     value)
    value

    (string? value)
    (try
      (UUID/fromString
       value)
      (catch IllegalArgumentException _
        value))

    :else
    value))

(def standard-codecs
  {:identity
   identity

   :instant
   normalize-instant

   :trim-string
   trim-string

   :blank-string->nil
   blank-string->nil

   :uuid
   normalize-uuid})

(defn codec?
  "Returns true for one supported codec specification.

   A codec may be:

   - a keyword in standard-codecs
   - a callable
   - {:decode fn}
   - a nonempty vector pipeline of codec specifications"
  [value]
  (boolean
   (cond
     (keyword? value)
     (contains?
      standard-codecs
      value)

     (map? value)
     (ifn?
      (:decode value))

     (vector? value)
     (and
      (seq value)
      (every?
       codec?
       value))

     :else
     (ifn?
      value))))

(defn require-codec
  [codec]
  (when-not
   (codec?
    codec)
    (fail!
     ::invalid-codec
     "Codec specification is invalid."
     {:codec codec}))
  codec)

(defn decode
  "Applies one codec specification to value."
  [codec value]
  (require-codec
   codec)

  (cond
    (keyword? codec)
    ((get
      standard-codecs
      codec)
     value)

    (map? codec)
    ((:decode codec)
     value)

    (vector? codec)
    (reduce
     (fn [value codec]
       (decode
        codec
        value))
     value
     codec)

    :else
    (codec
     value)))

;; =============================================================================
;; Codec derivation
;; =============================================================================

(def codec-property
  "Malli property used to attach a persistence decoder to a scalar schema."
  :gesso.model/codec)

(defn field-codec
  "Returns a field's Malli-declared persistence codec, or nil."
  ([document-schema key]
   (field-codec
    document-schema
    key
    nil))
  ([document-schema key malli-options]
   (get-in
    (field-info
     document-schema
     key
     malli-options)
    [:properties
     codec-property])))

(defn derived-codecs
  "Derives field->codec from Malli properties on document fields.

   Every discovered codec is validated immediately."
  ([document-schema]
   (derived-codecs
    document-schema
    nil))
  ([document-schema malli-options]
   (into
    {}
    (keep
     (fn [{:keys [key properties]}]
       (when-let [codec
                  (get
                   properties
                   codec-property)]
         (require-codec
          codec)
         [key
          codec])))
    (map-entries
     document-schema
     malli-options))))

(defn effective-codecs
  "Returns the persistence codec map for a document schema.

   overrides is a field->codec map. Overrides replace Malli-derived codecs for
   the same fields and may add codecs for fields whose Malli schema carries no
   codec metadata.

   Every override field must exist in the document schema."
  ([document-schema overrides]
   (effective-codecs
    document-schema
    overrides
    nil))
  ([document-schema overrides malli-options]
   (let [overrides
         (or
          overrides
          {})

         declared-fields
         (set
          (field-keys
           document-schema
           malli-options))]

     (when-not
      (map?
       overrides)
       (fail!
        ::invalid-codec-overrides
        "Persistence codec overrides must be a map."
        {:overrides overrides}))

     (doseq [[key codec]
             overrides]
       (when-not
        (contains?
         declared-fields
         key)
         (fail!
          ::unknown-codec-field
          "Persistence codec override refers to an undeclared document field."
          {:field key
           :declared-fields declared-fields}))

       (require-codec
        codec))

     (merge
      (derived-codecs
       document-schema
       malli-options)
      overrides))))

(defn decode-fields
  "Applies field codecs only to keys present in value.

   Missing optional fields stay missing."
  [field->codec value]
  (when-not
   (map?
    field->codec)
    (fail!
     ::invalid-codec-map
     "Field codecs must be a map."
     {:codecs field->codec}))

  (doseq [[key codec]
          field->codec]
    (when-not
     (keyword?
      key)
      (fail!
       ::invalid-codec-field
       "Codec field names must be keywords."
       {:field key}))
    (require-codec
     codec))

  (if
   (map?
    value)
    (reduce-kv
     (fn [result key codec]
       (if
        (contains?
         result
         key)
         (update
          result
          key
          #(decode
            codec
            %))
         result))
     value
     field->codec)
    value))

(defn normalize-document
  "Normalizes a persisted document using codecs derived from its Malli schema.

   options:
     :codec-overrides
     :malli-options

   nil is preserved for a missing document.

   The resulting value is not implicitly considered valid; callers should run
   require-valid after normalization."
  ([document-schema document]
   (normalize-document
    document-schema
    document
    nil))
  ([document-schema
    document
    {:keys [codec-overrides
            malli-options]}]
   (cond
     (nil?
      document)
     nil

     (not
      (map?
       document))
     (fail!
      ::invalid-persisted-document
      "A loaded model document must be a map or nil."
      {:document document})

     :else
     (decode-fields
      (effective-codecs
       document-schema
       codec-overrides
       malli-options)
      document))))

;; =============================================================================
;; Generated Graph schema metadata
;; =============================================================================

(defn graph-field-schema
  "Returns the Malli schema form that should validate the Graph representation
   of a document field.

   For same-name projection this is simply the field schema. Aliasing changes
   only the Graph key, not the value schema."
  ([document-schema field]
   (graph-field-schema
    document-schema
    field
    nil))
  ([document-schema field malli-options]
   (or
    (field-schema
     document-schema
     field
     malli-options)

    (fail!
     ::unknown-graph-field
     "Cannot derive a Graph schema for an undeclared document field."
     {:field field}))))

(defn graph-schema-entry
  "Returns [graph-key schema-form] for a persisted field projection.

   storage-key identifies the document field. graph-key may be the same key or
   an explicit alias."
  ([document-schema storage-key graph-key]
   (graph-schema-entry
    document-schema
    storage-key
    graph-key
    nil))
  ([document-schema storage-key graph-key malli-options]
   (when-not
    (keyword?
     graph-key)
     (fail!
      ::invalid-graph-key
      "Generated Graph attribute names must be keywords."
      {:graph-key graph-key}))

   [graph-key
    (graph-field-schema
     document-schema
     storage-key
     malli-options)]))

;; =============================================================================
;; Validation
;; =============================================================================

(defn valid?
  "Returns true when value satisfies schema."
  ([schema value]
   (valid?
    schema
    value
    nil))
  ([schema value malli-options]
   (m/validate
    (schema-object
     schema
     malli-options)
    value)))

(defn explain
  "Returns Malli explain data, or nil when value is valid."
  ([schema value]
   (explain
    schema
    value
    nil))
  ([schema value malli-options]
   (m/explain
    (schema-object
     schema
     malli-options)
    value)))

(defn humanized-errors
  "Returns humanized Malli errors, or nil when valid."
  ([schema value]
   (humanized-errors
    schema
    value
    nil))
  ([schema value malli-options]
   (some->
    (explain
     schema
     value
     malli-options)
    me/humanize)))

(defn require-valid
  "Returns value when it satisfies schema, otherwise throws with Malli errors."
  ([schema value]
   (require-valid
    schema
    value
    nil))
  ([schema value malli-options]
   (let [compiled
         (schema-object
          schema
          malli-options)]
     (if
      (m/validate
       compiled
       value)
       value

       (fail!
        ::validation-failed
        "Value does not satisfy the model schema."
        {:errors
         (me/humanize
          (m/explain
           compiled
           value))

         :value
         value})))))

(defn normalize-and-validate
  "Applies persistence normalization and then validates the canonical domain
   document.

   options are the same as normalize-document."
  ([document-schema document]
   (normalize-and-validate
    document-schema
    document
    nil))
  ([document-schema document options]
   (let [normalized
         (normalize-document
          document-schema
          document
          options)]
     (when
      normalized
       (require-valid
        document-schema
        normalized
        (:malli-options
         options))))))