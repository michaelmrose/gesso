(ns gesso.model.schema-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gesso.model.schema :as model.schema]
   [malli.core :as m])
  (:import
   [java.time Instant OffsetDateTime ZoneOffset ZonedDateTime ZoneId]
   [java.util UUID]))

(def instant-schema
  [:fn
   {:gesso.model/codec :instant
    :error/message "must be an Instant"}
   #(instance? Instant %)])

(def trimmed-schema
  [:string
   {:gesso.model/codec :trim-string}])

(def nested-schema
  [:map
   [:nested/value :string]])

(def document-schema
  [:and
   [:map {:closed true}
    [:xt/id :uuid]
    [:example/name trimmed-schema]
    [:example/nickname
     {:optional true}
     :string]
    [:example/created-at instant-schema]
    [:example/context nested-schema]
    [:example/items
     [:vector nested-schema]]
    [:example/tags
     [:set :keyword]]]
   [:fn map?]])

(def codec-override-schema
  [:map
   [:example/value
    {:gesso.model/codec :identity}
    [:string
     {:gesso.model/codec :trim-string}]]])

(defn error-type
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo ex
      (:error/type
       (ex-data ex)))))

(deftest schema-shape-test
  (testing "ordinary scalar schemas are scalar"
    (doseq [schema
            [:string
             :uuid
             :keyword
             [:int {:min 0}]
             [:fn string?]]]
      (is
       (=
        :scalar
        (model.schema/schema-shape
         schema)))))

  (testing "maps and collections have broad structural shapes"
    (is
     (=
      :map
      (model.schema/schema-shape
       [:map
        [:x :string]])))

    (is
     (=
      :collection
      (model.schema/schema-shape
       [:vector :string])))

    (is
     (=
      :collection
      (model.schema/schema-shape
       [:sequential :string])))

    (is
     (=
      :collection
      (model.schema/schema-shape
       [:set :keyword]))))

  (testing "an :and document schema inherits the structural map shape"
    (is
     (=
      :map
      (model.schema/schema-shape
       document-schema))))

  (testing "ambiguous unions are not guessed"
    (is
     (=
      :unknown
      (model.schema/schema-shape
       [:or
        :string
        [:map
         [:x :string]]])))))

(deftest graph-shape-test
  (testing "maps are Graph joins"
    (is
     (=
      :join
      (model.schema/graph-shape
       nested-schema))))

  (testing "vectors and sequentials of maps are Graph joins"
    (is
     (=
      :join
      (model.schema/graph-shape
       [:vector nested-schema])))

    (is
     (=
      :join
      (model.schema/graph-shape
       [:sequential nested-schema]))))

  (testing "collections of scalar values remain Graph scalar values"
    (is
     (=
      :scalar
      (model.schema/graph-shape
       [:vector :string])))

    (is
     (=
      :scalar
      (model.schema/graph-shape
       [:sequential :uuid])))

    (is
     (=
      :scalar
      (model.schema/graph-shape
       [:set :keyword]))))

  (testing "ambiguous unions remain unknown"
    (is
     (=
      :unknown
      (model.schema/graph-shape
       [:or
        :string
        nested-schema])))))

(deftest document-map-introspection-test
  (testing "the structural map is found through the domain-predicate :and wrapper"
    (is
     (=
      :map
      (-> document-schema
          model.schema/document-map-schema
          m/type))))

  (testing "a schema without a structural map is rejected"
    (is
     (=
      ::model.schema/missing-document-map
      (error-type
       #(model.schema/document-map-schema
         [:and
          :string
          [:fn string?]])))))

  (testing "persisted field order comes directly from Malli"
    (is
     (=
      [:xt/id
       :example/name
       :example/nickname
       :example/created-at
       :example/context
       :example/items
       :example/tags]
      (model.schema/field-keys
       document-schema))))

  (testing "field metadata exposes optionality and structural shape"
    (let [nickname
          (model.schema/field-info
           document-schema
           :example/nickname)

          context
          (model.schema/field-info
           document-schema
           :example/context)

          items
          (model.schema/field-info
           document-schema
           :example/items)]

      (is
       (true?
        (:optional?
         nickname)))

      (is
       (=
        :scalar
        (:graph-shape
         nickname)))

      (is
       (false?
        (:optional?
         context)))

      (is
       (=
        :join
        (:graph-shape
         context)))

      (is
       (=
        :join
        (:graph-shape
         items)))))

  (testing "unknown fields return nil from field-info/field-schema"
    (is
     (nil?
      (model.schema/field-info
       document-schema
       :example/missing)))

    (is
     (nil?
      (model.schema/field-schema
       document-schema
       :example/missing)))))

(deftest field-properties-test
  (testing "reusable scalar schema properties are visible from the map field"
    (is
     (=
      :instant
      (model.schema/field-codec
       document-schema
       :example/created-at))))

  (testing "map-entry properties override reusable child-schema properties"
    (is
     (=
      :identity
      (model.schema/field-codec
       codec-override-schema
       :example/value)))))

(deftest default-graph-query-test
  (testing "scalar and structural values produce conventional Graph query items"
    (is
     (=
      :example/name
      (model.schema/default-graph-query-item
       (model.schema/field-info
        document-schema
        :example/name))))

    (is
     (=
      [:? :example/nickname]
      (model.schema/default-graph-query-item
       (model.schema/field-info
        document-schema
        :example/nickname))))

    (is
     (=
      {:example/context
       [:*]}
      (model.schema/default-graph-query-item
       (model.schema/field-info
        document-schema
        :example/context))))

    (is
     (=
      {:example/items
       [:*]}
      (model.schema/default-graph-query-item
       (model.schema/field-info
        document-schema
        :example/items)))))

  (testing "the full default query follows persisted schema declaration order"
    (is
     (=
      [:xt/id
       :example/name
       [:? :example/nickname]
       :example/created-at
       {:example/context [:*]}
       {:example/items [:*]}
       :example/tags]
      (model.schema/default-graph-query
       document-schema)))))

(deftest instant-normalization-test
  (let [instant
        (Instant/parse
         "2026-07-25T12:34:56Z")

        offset
        (OffsetDateTime/ofInstant
         instant
         ZoneOffset/UTC)

        zoned
        (ZonedDateTime/ofInstant
         instant
         (ZoneId/of
          "America/Los_Angeles"))]

    (testing "Instant values are preserved"
      (is
       (identical?
        instant
        (model.schema/normalize-instant
         instant))))

    (testing "OffsetDateTime and ZonedDateTime normalize to Instant"
      (is
       (=
        instant
        (model.schema/normalize-instant
         offset)))

      (is
       (=
        instant
        (model.schema/normalize-instant
         zoned))))

    (testing "nil and unknown representations are preserved for Malli to judge"
      (is
       (nil?
        (model.schema/normalize-instant
         nil)))

      (is
       (=
        "not-a-time"
        (model.schema/normalize-instant
         "not-a-time"))))))

(deftest standard-codec-test
  (testing "string codecs are deliberately small and composable"
    (is
     (=
      "hello"
      (model.schema/decode
       :trim-string
       "  hello  ")))

    (is
     (nil?
      (model.schema/decode
       [:trim-string
        :blank-string->nil]
       "   "))))

  (testing "custom callable codecs work"
    (is
     (=
      "ABC"
      (model.schema/decode
       str/upper-case
       "abc"))))

  (testing "map codec specifications use :decode"
    (is
     (=
      11
      (model.schema/decode
       {:decode inc}
       10))))

  (testing "UUID codec parses canonical strings and preserves invalid strings"
    (let [uuid
          (UUID/randomUUID)]
      (is
       (=
        uuid
        (model.schema/decode
         :uuid
         (str uuid))))

      (is
       (=
        "not-a-uuid"
        (model.schema/decode
         :uuid
         "not-a-uuid")))))

  (testing "invalid codec specifications fail immediately"
    (doseq [codec
            [:not-a-codec
             {}
             []
             [:trim-string
              :not-a-codec]]]
      (is
       (false?
        (model.schema/codec?
         codec)))

      (is
       (=
        ::model.schema/invalid-codec
        (error-type
         #(model.schema/require-codec
           codec)))))))

(deftest derived-codecs-test
  (testing "codec declarations are derived from Malli field metadata"
    (is
     (=
      {:example/name
       :trim-string

       :example/created-at
       :instant}
      (model.schema/derived-codecs
       document-schema))))

  (testing "descriptor-level overrides replace derived codecs without repeating the rest"
    (is
     (=
      {:example/name
       :identity

       :example/created-at
       :instant

       :example/nickname
       :trim-string}
      (model.schema/effective-codecs
       document-schema
       {:example/name
        :identity

        :example/nickname
        :trim-string}))))

  (testing "overrides cannot silently name fields absent from the schema"
    (is
     (=
      ::model.schema/unknown-codec-field
      (error-type
       #(model.schema/effective-codecs
         document-schema
         {:example/missing
          :identity}))))))

(deftest decode-fields-test
  (testing "only present keys are decoded"
    (is
     (=
      {:a
       "hello"}

      (model.schema/decode-fields
       {:a
        :trim-string

        :b
        :trim-string}
       {:a
        " hello "}))))

  (testing "present nil is still passed through its codec"
    (is
     (=
      {:a nil}
      (model.schema/decode-fields
       {:a
        :identity}
       {:a nil}))))

  (testing "non-map values are preserved; document normalization decides whether that is allowed"
    (is
     (=
      "not-a-map"
      (model.schema/decode-fields
       {:a
        :identity}
       "not-a-map")))))

(deftest normalize-document-test
  (let [uuid
        (UUID/randomUUID)

        instant
        (Instant/parse
         "2026-07-25T12:34:56Z")

        raw
        {:xt/id
         uuid

         :example/name
         "  Widget  "

         :example/created-at
         (OffsetDateTime/ofInstant
          instant
          ZoneOffset/UTC)

         :example/context
         {:nested/value
          "context"}

         :example/items
         []

         :example/tags
         #{:a}}]

    (testing "schema metadata performs persistence normalization"
      (is
       (=
        (assoc
         raw
         :example/name
         "Widget"
         :example/created-at
         instant)

        (model.schema/normalize-document
         document-schema
         raw))))

    (testing "missing optional fields stay missing"
      (is
       (not
        (contains?
         (model.schema/normalize-document
          document-schema
          raw)
         :example/nickname))))

    (testing "nil represents a missing persisted document"
      (is
       (nil?
        (model.schema/normalize-document
         document-schema
         nil))))

    (testing "non-map loaded documents are rejected"
      (is
       (=
        ::model.schema/invalid-persisted-document
        (error-type
         #(model.schema/normalize-document
           document-schema
           [:not
            :a
            :document])))))))

(deftest graph-schema-metadata-test
  (testing "Graph aliases retain the persisted field value schema"
    (is
     (=
      [:example/public-name
       (model.schema/field-schema
        document-schema
        :example/name)]
      (model.schema/graph-schema-entry
       document-schema
       :example/name
       :example/public-name))))

  (testing "unknown persisted fields cannot manufacture Graph registry entries"
    (is
     (=
      ::model.schema/unknown-graph-field
      (error-type
       #(model.schema/graph-field-schema
         document-schema
         :example/missing)))))

  (testing "Graph attribute keys must be keywords"
    (is
     (=
      ::model.schema/invalid-graph-key
      (error-type
       #(model.schema/graph-schema-entry
         document-schema
         :example/name
         "public-name"))))))

(deftest validation-test
  (let [uuid
        (UUID/randomUUID)

        valid-document
        {:xt/id
         uuid

         :example/name
         "Widget"

         :example/created-at
         (Instant/parse
          "2026-07-25T12:34:56Z")

         :example/context
         {:nested/value
          "context"}

         :example/items
         []

         :example/tags
         #{:a}}]

    (testing "valid?, explain, and humanized-errors expose ordinary Malli validation"
      (is
       (true?
        (model.schema/valid?
         document-schema
         valid-document)))

      (is
       (nil?
        (model.schema/explain
         document-schema
         valid-document)))

      (is
       (nil?
        (model.schema/humanized-errors
         document-schema
         valid-document)))

      (is
       (identical?
        valid-document
        (model.schema/require-valid
         document-schema
         valid-document))))

    (testing "invalid values fail through the model validation boundary"
      (let [invalid
            (assoc
             valid-document
             :example/created-at
             "not-an-instant")]

        (is
         (false?
          (model.schema/valid?
           document-schema
           invalid)))

        (is
         (some?
          (model.schema/explain
           document-schema
           invalid)))

        (is
         (map?
          (model.schema/humanized-errors
           document-schema
           invalid)))

        (is
         (=
          ::model.schema/validation-failed
          (error-type
           #(model.schema/require-valid
             document-schema
             invalid))))))))

(deftest normalize-and-validate-test
  (let [uuid
        (UUID/randomUUID)

        instant
        (Instant/parse
         "2026-07-25T12:34:56Z")

        raw
        {:xt/id
         uuid

         :example/name
         " Widget "

         :example/created-at
         (OffsetDateTime/ofInstant
          instant
          ZoneOffset/UTC)

         :example/context
         {:nested/value
          "context"}

         :example/items
         []

         :example/tags
         #{:a}}]

    (testing "the read boundary normalizes first and validates the canonical domain value"
      (is
       (=
        (assoc
         raw
         :example/name
         "Widget"
         :example/created-at
         instant)
        (model.schema/normalize-and-validate
         document-schema
         raw))))

    (testing "missing documents stay missing"
      (is
       (nil?
        (model.schema/normalize-and-validate
         document-schema
         nil))))

    (testing "a value still invalid after decoding is rejected"
      (is
       (=
        ::model.schema/validation-failed
        (error-type
         #(model.schema/normalize-and-validate
           document-schema
           (assoc
            raw
            :example/created-at
            "still-not-an-instant"))))))))
