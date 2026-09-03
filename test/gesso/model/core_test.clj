(ns gesso.model.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [com.biffweb.core :as biff.core]
   [com.biffweb.xtdb :as biff.xtdb]
   [com.biffweb.graph :as graph]
   ;; [gesso.graph :as graph]
   [gesso.live.consistency.xtdb :as live.xtdb]
   [gesso.live.progression :as progression]
   [gesso.model.command :as command]
   [gesso.model.core :as model]
   [gesso.model.schema :as model.schema]
   [gesso.model.tx :as model.tx])
  (:import
   [java.time Instant]
   [java.util UUID]))

;; =============================================================================
;; Test model
;; =============================================================================

(def instant-schema
  [:fn
   {:gesso.model/codec :identity}
   #(instance? Instant %)])

(def widget-version
  {:revision-key :widget/revision
   :created-at-key :widget/created-at
   :updated-at-key :widget/updated-at})

(def widget-document-schema
  [:and
   [:map {:closed true}
    [:xt/id :uuid]

    [:widget/name
     {:gesso.model/graph true}
     [:string
      {:gesso.model/codec :trim-string}]]

    ;; Equality lookup input, deliberately not a generated output projection.
    [:widget/email :string]

    [:widget/owner
     {:gesso.model/graph :widget/owner-id}
     :uuid]

    [:widget/note
     {:optional true
      :gesso.model/graph true}
     :string]

    [:widget/context
     {:gesso.model/graph true}
     [:map
      [:context/label :string]]]

    ;; Persisted but intentionally private from ordinary generated Graph output.
    [:widget/secret :string]

    [:widget/revision
     {:gesso.model/graph true}
     [:int {:min 0}]]

    [:widget/created-at instant-schema]

    [:widget/updated-at
     {:gesso.model/graph true}
     instant-schema]]

   [:fn
    {:error/message "widget must be internally consistent"}
    (fn [document]
      (and
       (string?
        (:widget/name document))
       (pos?
        (count
         (:widget/name document)))))]])

(defn valid-widget
  ([]
   (valid-widget
    (UUID/randomUUID)))
  ([id]
   (let [owner
         (UUID/randomUUID)

         created-at
         (Instant/parse
          "2026-07-25T10:00:00Z")

         updated-at
         (Instant/parse
          "2026-07-25T10:05:00Z")]
     {:xt/id id
      :widget/name "Widget"
      :widget/email "widget@example.com"
      :widget/owner owner
      :widget/context {:context/label "primary"}
      :widget/secret "private"
      :widget/revision 3
      :widget/created-at created-at
      :widget/updated-at updated-at})))

(defn initial-widget
  [{:keys
    [id
     name
     email
     owner
     context
     secret
     now]}]
  {:xt/id id
   :widget/name name
   :widget/email email
   :widget/owner owner
   :widget/context context
   :widget/secret secret
   :widget/revision 0
   :widget/created-at now
   :widget/updated-at now})

(defn create-widget-command
  [input]
  (command/create
   :widget
   (initial-widget input)
   widget-version))

(defn rename-widget-command
  [widget {:keys [name now]}]
  (let [after
        (-> widget
            (assoc
             :widget/name
             name)
            (command/bump-version
             widget-version
             now))]
    (command/update-command
     :widget
     :rename
     widget
     after
     widget-version)))

(defn widget-change
  [model-command]
  {:topic :widget
   :id (:model/id model-command)
   :change/kind
   (if
    (command/create?
     model-command)
    :created
    :updated)
   :widget/operation
   (:model/operation model-command)})

(defn change-entry
  [change]
  {:coalesce-key
   [(:topic change)
    (:id change)]})

(def read-descriptor
  {:entity-type :widget

   :document-schema
   widget-document-schema

   :identity
   {:graph-key :widget/id}

   :version
   widget-version

   :lookups
   [:widget/email]})

(def full-descriptor
  (assoc
   read-descriptor

   :operations
   {:create
    {:kind :create
     :command create-widget-command
     :result-key :widget}

    :rename
    rename-widget-command}

   :live
   {:change-fn widget-change
    :entry-fn change-entry}))

(def gadget-document-schema
  [:map {:closed true}
   [:xt/id :uuid]
   [:gadget/name
    {:gesso.model/graph true}
    :string]
   [:gadget/revision [:int {:min 0}]]
   [:gadget/created-at instant-schema]
   [:gadget/updated-at instant-schema]])

(def gadget-version
  {:revision-key :gadget/revision
   :created-at-key :gadget/created-at
   :updated-at-key :gadget/updated-at})

(def gadget-descriptor
  {:entity-type :gadget
   :document-schema gadget-document-schema
   :identity {:graph-key :gadget/id}
   :version gadget-version})

(defn error-type
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo ex
      (:error/type
       (ex-data ex)))))

(defn resolve-resolver
  [resolver ctx input]
  ((:biff.graph/resolve-fn resolver)
   (assoc
    ctx
    :biff.graph/input
    input)))

;; =============================================================================
;; Naming and identity
;; =============================================================================

(deftest generated-name-test
  (testing "the persisted identity defaults to :xt/id"
    (is
     (=
      :xt/id
      (model/identity-storage-key
       read-descriptor))))

  (testing "the public identity comes from the descriptor"
    (is
     (=
      :widget/id
      (model/identity-graph-key
       read-descriptor)))

    (is
     (=
      "widget"
      (model/graph-namespace
       read-descriptor))))

  (testing "document and found attributes derive from the identity namespace"
    (is
     (=
      :widget/doc
      (model/document-key
       read-descriptor)))

    (is
     (=
      :widget/found?
      (model/found-key
       read-descriptor))))

  (testing "generated resolver ids are stable and inspectable"
    (is
     (=
      :gesso.model.generated/widget-by-id
      (model/by-id-resolver-id
       read-descriptor)))

    (is
     (=
      :gesso.model.generated/widget-fields
      (model/fields-resolver-id
       read-descriptor)))

    (is
     (=
      :gesso.model.generated/widget-by-widget.email
      (model/lookup-resolver-id
       read-descriptor
       :widget/email))))

  (testing "unqualified operations inherit the Graph namespace"
    (is
     (=
      :widget/rename
      (model/operation-id
       full-descriptor
       :rename))))

  (testing "qualified operation ids remain application-owned"
    (is
     (=
      :custom.widget/rename
      (model/operation-id
       full-descriptor
       :custom.widget/rename)))))

;; =============================================================================
;; Projection derivation
;; =============================================================================

(deftest graph-projection-test
  (let [projections
        (model/graph-projections
         read-descriptor)]

    (testing "projection order follows the canonical Malli document"
      (is
       (=
        [:widget/name
         :widget/owner
         :widget/note
         :widget/context
         :widget/revision
         :widget/updated-at]
        (mapv
         :storage-key
         projections))))

    (testing "same-name projection and aliases are both represented"
      (is
       (=
        [:widget/name
         :widget/owner-id
         :widget/note
         :widget/context
         :widget/revision
         :widget/updated-at]
        (mapv
         :graph-key
         projections))))

    (testing "optionality and join shape come from Malli"
      (is
       (=
        {:optional? true
         :graph-shape :scalar}
        (select-keys
         (some
          #(when
            (=
             :widget/note
             (:storage-key %))
            %)
          projections)
         [:optional?
          :graph-shape])))

      (is
       (=
        :join
        (:graph-shape
         (some
          #(when
            (=
             :widget/context
             (:storage-key %))
            %)
          projections)))))

    (testing "unannotated persisted fields are not accidentally exposed"
      (is
       (not
        (some
         #{:widget/email
           :widget/secret
           :widget/created-at}
         (map
          :storage-key
          projections)))))))

;; =============================================================================
;; Descriptor validation
;; =============================================================================

(deftest descriptor-validation-test
  (testing "the representative read and full descriptors are valid"
    (is
     (identical?
      read-descriptor
      (model/validate-descriptor
       read-descriptor)))

    (is
     (model/descriptor?
      read-descriptor))

    (is
     (model/descriptor?
      full-descriptor)))

  (testing "descriptor top-level keys are closed"
    (is
     (=
      ::model/unknown-descriptor-keys
      (error-type
       #(model/validate-descriptor
         (assoc
          read-descriptor
          :graph
          {:fields []}))))))

  (testing "entity type must be a keyword"
    (is
     (=
      ::model/invalid-entity-type
      (error-type
       #(model/validate-descriptor
         (assoc
          read-descriptor
          :entity-type
          "widget"))))))

  (testing "a document schema is required"
    (is
     (=
      ::model/missing-document-schema
      (error-type
       #(model/validate-descriptor
         (dissoc
          read-descriptor
          :document-schema))))))

  (testing "identity requires a qualified Graph key"
    (is
     (=
      ::model/invalid-graph-identity
      (error-type
       #(model/validate-descriptor
         (assoc-in
          read-descriptor
          [:identity
           :graph-key]
          :id))))))

  (testing "identity storage field must exist"
    (is
     (=
      ::model/unknown-document-field
      (error-type
       #(model/validate-descriptor
         (assoc-in
          read-descriptor
          [:identity
           :storage-key]
          :widget/missing))))))

  (testing "version fields must exist in the document schema"
    (is
     (=
      ::model/unknown-document-field
      (error-type
       #(model/validate-descriptor
         (assoc-in
          read-descriptor
          [:version
           :updated-at-key]
          :widget/missing))))))

  (testing "lookup fields must exist and be scalar"
    (is
     (=
      ::model/unknown-document-field
      (error-type
       #(model/validate-descriptor
         (assoc
          read-descriptor
          :lookups
          [:widget/missing])))))

    (is
     (=
      ::model/non-scalar-lookup
      (error-type
       #(model/validate-descriptor
         (assoc
          read-descriptor
          :lookups
          [:widget/context]))))))

  (testing "lookup declarations cannot repeat"
    (is
     (=
      ::model/duplicate-lookups
      (error-type
       #(model/validate-descriptor
         (assoc
          read-descriptor
          :lookups
          [:widget/email
           :widget/email]))))))

  (testing "publishing generated operations require an entity-level semantic change function"
    (is
     (=
      ::model/missing-change-fn
      (error-type
       #(model/validate-descriptor
         (assoc
          read-descriptor
          :operations
          {:rename
           rename-widget-command}))))))

  (testing "silent generated operations do not require semantic changes"
    (is
     (model/descriptor?
      (assoc
       read-descriptor
       :operations
       {:rename
        rename-widget-command}
       :live
       {:emit false}))))

  (testing "operation maps reject unknown configuration keys"
    (is
     (=
      ::model/unknown-operation-keys
      (error-type
       #(model/validate-descriptor
         (assoc-in
          full-descriptor
          [:operations
           :create]
          {:kind :create
           :command create-widget-command
           :magic true})))))))

(deftest graph-projection-validation-test
  (testing "invalid :gesso.model/graph settings are rejected"
    (let [schema
          [:map
           [:xt/id :uuid]
           [:bad/value
            {:gesso.model/graph "bad"}
            :string]
           [:bad/revision [:int {:min 0}]]
           [:bad/created-at instant-schema]
           [:bad/updated-at instant-schema]]

          descriptor
          {:entity-type :bad
           :document-schema schema
           :identity {:graph-key :bad/id}
           :version {:revision-key :bad/revision
                     :created-at-key :bad/created-at
                     :updated-at-key :bad/updated-at}}]
      (is
       (=
        ::model/invalid-graph-property
        (error-type
         #(model/validate-descriptor
           descriptor))))))

  (testing "two persisted fields may not project to the same Graph key"
    (let [schema
          [:map
           [:xt/id :uuid]
           [:bad/a
            {:gesso.model/graph :bad/value}
            :string]
           [:bad/b
            {:gesso.model/graph :bad/value}
            :string]
           [:bad/revision [:int {:min 0}]]
           [:bad/created-at instant-schema]
           [:bad/updated-at instant-schema]]

          descriptor
          {:entity-type :bad
           :document-schema schema
           :identity {:graph-key :bad/id}
           :version {:revision-key :bad/revision
                     :created-at-key :bad/created-at
                     :updated-at-key :bad/updated-at}}]
      (is
       (=
        ::model/duplicate-graph-projections
        (error-type
         #(model/validate-descriptor
           descriptor))))))

  (testing "field projection may not steal the generated identity attribute"
    (let [schema
          [:map
           [:xt/id :uuid]
           [:bad/value
            {:gesso.model/graph :bad/id}
            :string]
           [:bad/revision [:int {:min 0}]]
           [:bad/created-at instant-schema]
           [:bad/updated-at instant-schema]]

          descriptor
          {:entity-type :bad
           :document-schema schema
           :identity {:graph-key :bad/id}
           :version {:revision-key :bad/revision
                     :created-at-key :bad/created-at
                     :updated-at-key :bad/updated-at}}]
      (is
       (=
        ::model/reserved-graph-projection
        (error-type
         #(model/validate-descriptor
           descriptor)))))))

;; =============================================================================
;; Generated schema registry
;; =============================================================================

(deftest generated-schema-test
  (let [schema
        (model/generated-schema
         read-descriptor)]

    (testing "the persisted entity and document Graph value share the canonical document schema"
      (is
       (=
        widget-document-schema
        (:widget schema)))

      (is
       (=
        widget-document-schema
        (:widget/doc schema))))

    (testing "ordinary generated envelope attributes are derived"
      (is
       (=
        :boolean
        (:widget/found?
         schema)))

      (is
       (=
        :uuid
        (:widget/id
         schema))))

    (testing "projected values reuse their persisted Malli field schemas"
      (is
       (=
        (model.schema/field-schema
         widget-document-schema
         :widget/name)
        (:widget/name
         schema)))

      (is
       (=
        :uuid
        (:widget/owner-id
         schema)))

      (is
       (=
        (model.schema/field-schema
         widget-document-schema
         :widget/context)
        (:widget/context
         schema))))

    (testing "lookup inputs are registered even when they are not projected outputs"
      (is
       (=
        :string
        (:widget/email
         schema))))

    (testing "unexposed internal fields do not create unnecessary Graph registry entries"
      (is
       (not
        (contains?
         schema
         :widget/secret)))

      (is
       (not
        (contains?
         schema
         :widget/created-at))))))

;; =============================================================================
;; Query derivation
;; =============================================================================

(deftest generated-query-test
  (testing "persisted XTDB columns come only from the canonical Malli document"
    (is
     (=
      [:xt/id
       :widget/name
       :widget/email
       :widget/owner
       :widget/note
       :widget/context
       :widget/secret
       :widget/revision
       :widget/created-at
       :widget/updated-at]
      (model/document-columns
       read-descriptor))))

  (testing "canonical documents travel through Graph as wildcards"
    (is
     (=
      [:*]
      (model/document-query
       read-descriptor))))

  (testing "all conventional lookups use one found/document envelope"
    (is
     (=
      [:widget/found?
       {[:? :widget/doc]
        [:*]}]
      (model/lookup-query
       read-descriptor))))

  (testing "field query is derived from exposure annotations and Malli shape"
    (is
     (=
      [:widget/id
       :widget/name
       :widget/owner-id
       [:? :widget/note]
       {:widget/context [:*]}
       :widget/revision
       :widget/updated-at]
      (model/field-query
       read-descriptor)))))

;; =============================================================================
;; Persistence reads
;; =============================================================================

(deftest q-read-boundary-test
  (let [query
        {:select [:xt/id]
         :from   [:widget]}]

    (testing "unconstrained two-arity reads delegate to Biff unchanged"
      (let [ctx
            {:biff.xtdb/node :node
             :biff.xtdb/snapshot-token "request-token"
             :opaque/request-value :preserved}

            calls
            (atom [])]

        (with-redefs
         [biff.xtdb/q
          (fn [& args]
            (swap! calls conj args)
            [{:ok true}])]

          (is (= [{:ok true}]
                 (model/q ctx query)))

          (is (= [[ctx query]]
                 @calls)))))

    (testing "unconstrained three-arity reads preserve caller options and Biff snapshot semantics"
      (let [ctx
            {:biff.xtdb/node :node
             :biff.xtdb/snapshot-token "request-token"}

            opts
            {:key-fn :kebab-case-keyword}

            calls
            (atom [])]

        (with-redefs
         [biff.xtdb/q
          (fn [& args]
            (swap! calls conj args)
            [{:ok true}])]

          (is (= [{:ok true}]
                 (model/q ctx query opts)))

          (is (= [[ctx query opts]]
                 @calls)))))

    (testing "authoritative progression overrides a stale request snapshot token"
      (let [required-time
            (Instant/parse
             "2026-09-01T18:00:00Z")

            required-basis
            (live.xtdb/basis
             60
             required-time)

            requirement
            (progression/requirement
             required-basis)

            ctx
            {:biff.xtdb/node :node
             :biff.xtdb/snapshot-token "stale-token"
             :gesso.live/progression requirement}

            calls
            (atom [])]

        (with-redefs
         [biff.xtdb/q
          (fn [& args]
            (swap! calls conj args)
            [{:ok true}])]

          (is (= [{:ok true}]
                 (model/q ctx query)))

          (is (= [[(dissoc ctx :biff.xtdb/snapshot-token)
                   query
                   {:snapshot-token
                    (live.xtdb/basis-snapshot-token
                     required-basis)}]]
                 @calls)))))

    (testing "progression is applied after explicit consistency and caller query options"
      (let [required-time
            (Instant/parse
             "2026-09-01T18:05:00Z")

            required-basis
            (live.xtdb/basis
             :analytics
             61
             required-time)

            requirement
            (progression/requirement
             required-basis)

            caller-time
            (Instant/parse
             "2025-01-01T00:00:00Z")

            ctx
            {:biff.xtdb/node :node
             :biff.xtdb/snapshot-token "request-token"
             :gesso.live/consistency
             {:snapshot-token "consistency-token"
              :await-token "await"}
             :gesso.live/progression requirement}

            opts
            {:snapshot-token "caller-token"
             :snapshot-time caller-time
             :database :analytics
             :key-fn :kebab-case-keyword}

            calls
            (atom [])]

        (with-redefs
         [biff.xtdb/q
          (fn [& args]
            (swap! calls conj args)
            [{:ok true}])]

          (is (= [{:ok true}]
                 (model/q ctx query opts)))

          (is (= [[(dissoc ctx :biff.xtdb/snapshot-token)
                   query
                   {:await-token "await"
                    :database :analytics
                    :key-fn :kebab-case-keyword
                    :snapshot-token
                    (live.xtdb/basis-snapshot-token
                     required-basis)}]]
                 @calls)))))))

(deftest load-by-id-test
  (let [id
        (UUID/randomUUID)

        raw
        (assoc
         (valid-widget id)
         :widget/name
         "  Widget  ")

        calls
        (atom [])]

    (with-redefs
     [biff.xtdb/q
      (fn [connectable query]
        (swap!
         calls
         conj
         [connectable
          query])
        [raw])]

      (testing "load-by-id uses the Biff 2 XTDB context and schema-derived columns"
        (is
         (=
          (assoc
           raw
           :widget/name
           "Widget")
          (model/load-by-id
           read-descriptor
           {:biff.xtdb/node :node}
           id)))

        (is
         (=
          [[{:biff.xtdb/node
             :node}
            {:select
             (model/document-columns
              read-descriptor)

             :from
             [:widget]

             :where
             [:=
              :xt/id
              id]}]]
          @calls)))))

  (testing "no current row returns nil"
    (with-redefs
     [biff.xtdb/q
      (fn [_ _]
        [])]
      (is
       (nil?
        (model/load-by-id
         read-descriptor
         {:biff.xtdb/node :node}
         (UUID/randomUUID))))))

  (testing "nil identity is treated as a missing lookup without querying"
    (with-redefs
     [biff.xtdb/q
      (fn [& _]
        (throw
         (AssertionError.
          "query should not run")))]
      (is
       (nil?
        (model/load-by-id
         read-descriptor
         {}
         nil)))))

  (testing "there is deliberately no Biff 1 connection fallback"
    (is
     (=
      ::model/missing-biff-connection
      (error-type
       #(model/load-by-id
         read-descriptor
         {:biff/conn :old-biff1-connection}
         (UUID/randomUUID)))))))

(deftest load-by-lookup-test
  (let [document
        (valid-widget)]

    (testing "declared equality lookup uses the persisted field directly"
      (let [seen
            (atom nil)]

        (with-redefs
         [biff.xtdb/q
          (fn [connectable query]
            (reset!
             seen
             [connectable query])
            [document])]

          (is
           (=
            document
            (model/load-by-lookup
             read-descriptor
             {:biff.xtdb/node :node}
             :widget/email
             "widget@example.com")))

          (is
           (=
            [{:biff.xtdb/node
              :node}
             {:select
              (model/document-columns
               read-descriptor)

              :from
              [:widget]

              :where
              [:=
               :widget/email
               "widget@example.com"]}]
            @seen)))))

    (testing "nil alternate lookup does not query"
      (with-redefs
       [biff.xtdb/q
        (fn [& _]
          (throw
           (AssertionError.
            "query should not run")))]
        (is
         (nil?
          (model/load-by-lookup
           read-descriptor
           {}
           :widget/email
           nil)))))

    (testing "undeclared fields cannot become ad-hoc query APIs"
      (is
       (=
        ::model/unsupported-lookup
        (error-type
         #(model/load-by-lookup
           read-descriptor
           {:biff.xtdb/node :node}
           :widget/secret
           "private")))))

    (testing "a declared unique-style lookup refuses to choose among duplicates"
      (with-redefs
       [biff.xtdb/q
        (fn [_ _]
          [document
           (assoc
            document
            :xt/id
            (UUID/randomUUID))])]
        (is
         (=
          ::model/non-unique-lookup
          (error-type
           #(model/load-by-lookup
             read-descriptor
             {:biff.xtdb/node :node}
             :widget/email
             "widget@example.com"))))))))

;; =============================================================================
;; Projection behavior
;; =============================================================================

(deftest project-document-test
  (let [document
        (valid-widget)

        projected
        (model/project-document
         read-descriptor
         document)]

    (testing "identity and explicitly exposed values are projected"
      (is
       (=
        (:xt/id document)
        (:widget/id
         projected)))

      (is
       (=
        "Widget"
        (:widget/name
         projected)))

      (is
       (=
        (:widget/owner document)
        (:widget/owner-id
         projected)))

      (is
       (=
        (:widget/context document)
        (:widget/context
         projected))))

    (testing "unexposed persisted values stay private"
      (is
       (not
        (contains?
         projected
         :widget/email)))

      (is
       (not
        (contains?
         projected
         :widget/secret))))

    (testing "missing optional values are omitted instead of becoming nil Graph facts"
      (is
       (not
        (contains?
         projected
         :widget/note)))))

  (testing "present optional values are projected"
    (is
     (=
      "note"
      (:widget/note
       (model/project-document
        read-descriptor
        (assoc
         (valid-widget)
         :widget/note
         "note"))))))

  (testing "nil document remains nil"
    (is
     (nil?
      (model/project-document
       read-descriptor
       nil)))))

;; =============================================================================
;; Generated resolver contracts
;; =============================================================================

(deftest generated-resolver-shape-test
  (let [by-id
        (model/build-by-id-resolver
         read-descriptor)

        fields
        (model/build-field-resolver
         read-descriptor)

        email
        (model/build-lookup-resolver
         read-descriptor
         :widget/email)]

    (testing "resolver ids are the generated stable ids"
      (is
       (=
        (model/by-id-resolver-id
         read-descriptor)
        (:biff.graph/id
         by-id)))

      (is
       (=
        (model/fields-resolver-id
         read-descriptor)
        (:biff.graph/id
         fields)))

      (is
       (=
        (model/lookup-resolver-id
         read-descriptor
         :widget/email)
        (:biff.graph/id
         email))))

    (testing "resolver ASTs reflect the generated query contracts"
      (is
       (=
        (graph/query->ast
         [:widget/id])
        (:biff.graph/input-ast
         by-id)))

      (is
       (=
        (graph/query->ast
         (model/lookup-query
          read-descriptor))
        (:biff.graph/output-ast
         by-id)))

      (is
       (=
        (graph/query->ast
         [{:widget/doc [:*]}])
        (:biff.graph/input-ast
         fields)))

      (is
       (=
        (graph/query->ast
         (model/field-query
          read-descriptor))
        (:biff.graph/output-ast
         fields)))

      (is
       (=
        (graph/query->ast
         [:widget/email])
        (:biff.graph/input-ast
         email))))))

(deftest by-id-resolver-behavior-test
  (let [id
        (UUID/randomUUID)

        document
        (valid-widget id)

        resolver
        (model/build-by-id-resolver
         read-descriptor)]

    (testing "found document is wrapped in the conventional envelope"
      (with-redefs
       [biff.xtdb/q
        (fn [_ _]
          [document])]

        (is
         (=
          {:widget/found?
           true

           :widget/doc
           document}
          (resolve-resolver
           resolver
           {:biff.xtdb/node :node}
           {:widget/id id})))))

    (testing "missing document reports only found? false"
      (with-redefs
       [biff.xtdb/q
        (fn [_ _]
          [])]

        (is
         (=
          {:widget/found?
           false}
          (resolve-resolver
           resolver
           {:biff.xtdb/node :node}
           {:widget/id id})))))))

(deftest field-resolver-behavior-test
  (let [document
        (valid-widget)

        resolver
        (model/build-field-resolver
         read-descriptor)]

    (is
     (=
      (model/project-document
       read-descriptor
       document)
      (resolve-resolver
       resolver
       {}
       {:widget/doc
        document})))))

(deftest equality-resolver-behavior-test
  (let [document
        (valid-widget)

        resolver
        (model/build-lookup-resolver
         read-descriptor
         :widget/email)]

    (with-redefs
     [biff.xtdb/q
      (fn [_ query]
        (is
         (=
          [:=
           :widget/email
           "widget@example.com"]
          (:where query)))
        [document])]

      (is
       (=
        {:widget/found?
         true

         :widget/doc
         document}
        (resolve-resolver
         resolver
         {:biff.xtdb/node :node}
         {:widget/email
          "widget@example.com"}))))))

(deftest resolver-collection-test
  (let [resolvers
        (model/build-resolvers
         read-descriptor)]

    (is
     (=
      [(model/by-id-resolver-id
        read-descriptor)

       (model/fields-resolver-id
        read-descriptor)

       (model/lookup-resolver-id
        read-descriptor
        :widget/email)]
      (mapv
       :biff.graph/id
       resolvers)))))

;; =============================================================================
;; Public conventional reads
;; =============================================================================

(deftest facts-test
  (let [calls
        (atom [])]

    (with-redefs
     [graph/query
      (fn [ctx input query]
        (swap!
         calls
         conj
         [ctx
          input
          query])
        {:widget/found?
         false})]

      (is
       (=
        {:widget/found?
         false}
        (model/facts
         read-descriptor
         {:ctx true}
         :id
         :widget-id)))

      (is
       (=
        {:widget/found?
         false}
        (model/facts
         read-descriptor
         {:ctx true}
         :widget/email
         "widget@example.com")))

      (is
       (=
        [[{:ctx true}
          {:widget/id
           :widget-id}
          (model/lookup-query
           read-descriptor)]

         [{:ctx true}
          {:widget/email
           "widget@example.com"}
          (model/lookup-query
           read-descriptor)]]
        @calls))))

  (testing "public reads cannot invent an undeclared equality lookup"
    (is
     (=
      ::model/unsupported-lookup
      (error-type
       #(model/facts
         read-descriptor
         {}
         :widget/secret
         "private"))))))

(deftest require-document-test
  (let [document
        (valid-widget)]

    (testing "found conventional lookup unwraps the canonical document"
      (with-redefs
       [graph/query
        (fn [& _]
          {:widget/found?
           true

           :widget/doc
           document})]

        (is
         (identical?
          document
          (model/require-document
           read-descriptor
           {}
           :id
           (:xt/id document))))))

    (testing "missing document produces the generic not-found error"
      (with-redefs
       [graph/query
        (fn [& _]
          {:widget/found?
           false})]

        (is
         (=
          ::model/document-not-found
          (error-type
           #(model/require-document
             read-descriptor
             {}
             :id
             (:xt/id document)))))))

    (testing "found? true without a document is treated as an invalid Graph result"
      (with-redefs
       [graph/query
        (fn [& _]
          {:widget/found?
           true})]

        (is
         (=
          ::model/incomplete-graph-result
          (error-type
           #(model/require-document
             read-descriptor
             {}
             :id
             (:xt/id document)))))))))

;; =============================================================================
;; Operation configuration
;; =============================================================================

(deftest operation-config-test
  (testing "callable shorthand means a conventional update"
    (let [config
          (model/operation-config
           full-descriptor
           :rename)]
      (is
       (=
        :update
        (:kind
         config)))

      (is
       (identical?
        rename-widget-command
        (:command
         config)))

      (is
       (=
        :widget
        (:result-key
         config)))))

  (testing "explicit create configuration is retained"
    (let [config
          (model/operation-config
           full-descriptor
           :create)]
      (is
       (=
        :create
        (:kind
         config)))

      (is
       (identical?
        create-widget-command
        (:command
         config)))

      (is
       (=
        :widget
        (:result-key
         config))))))

;; =============================================================================
;; Generated create FX
;; =============================================================================

(deftest generated-create-operation-test
  (let [seen-input
        (atom nil)

        seen-plan
        (atom nil)

        create-command-fn
        (fn [input]
          (reset!
           seen-input
           input)
          (create-widget-command
           input))

        descriptor
        (assoc
         read-descriptor
         :operations
         {:create
          {:kind :create
           :command create-command-fn
           :result-key :widget}}
         :live
         {:change-fn widget-change
          :entry-fn change-entry})

        operation
        (model/build-operation
         descriptor
         :create)

        owner
        (UUID/randomUUID)

        result
        (operation
         {:biff.fx/handlers
          {model.tx/transact-effect
           (fn [_ctx plan]
             (reset!
              seen-plan
              plan)
             {:commit/status
              :committed})}}

         {:name "Created"
          :email "created@example.com"
          :owner owner
          :context {:context/label "new"}
          :secret "private"})]

    (testing "create owns id/time generation and forwards application input"
      (is
       (instance?
        UUID
        (:id
         @seen-input)))

      (is
       (=
        7
        (.version
         ^UUID
         (:id
          @seen-input))))

      (is
       (instance?
        Instant
        (:now
         @seen-input)))

      (is
       (=
        "Created"
        (:name
         @seen-input)))

      (is
       (=
        owner
        (:owner
         @seen-input))))

    (testing "the generated command is the sole transaction mutation"
      (let [model-command
            (first
             (:commands
              @seen-plan))]

        (is
         (command/create?
          model-command))

        (is
         (=
          :widget
          (:model/entity-type
           model-command)))

        (is
         (=
          (:id
           @seen-input)
          (:model/id
           model-command)))

        (is
         (=
          [{:topic :widget
            :id (:model/id model-command)
            :change/kind :created
            :widget/operation :create}]
          (:changes
           @seen-plan)))

        (is
         (identical?
          change-entry
          (:entry-fn
           @seen-plan)))

        (is
         (=
          :async
          (:emit
           @seen-plan)))))

    (testing "operation result returns the new document plus transaction result"
      (is
       (=
        (:model/after
         (first
          (:commands
           @seen-plan)))
        (:widget
         result)))

      (is
       (=
        {:commit/status
         :committed}
        (:transaction
         result))))

    (testing "generated operation metadata remains inspectable"
      (is
       (=
        :widget
        (:gesso.model/entity-type
         (meta operation))))

      (is
       (=
        :create
        (:gesso.model/operation
         (meta operation))))

      (is
       (=
        :widget/create
        (:gesso.model/operation-id
         (meta operation))))

      (is
       (=
        :create
        (:gesso.model/kind
         (meta operation)))))))

;; =============================================================================
;; Generated update FX
;; =============================================================================

(deftest generated-update-operation-test
  (let [id
        (UUID/randomUUID)

        current
        (valid-widget id)

        seen-domain-input
        (atom nil)

        seen-graph
        (atom nil)

        seen-plan
        (atom nil)

        rename-command-fn
        (fn [before input]
          (reset!
           seen-domain-input
           input)
          (rename-widget-command
           before
           input))

        descriptor
        (assoc
         read-descriptor
         :operations
         {:rename
          rename-command-fn}
         :live
         {:change-fn widget-change
          :entry-fn change-entry})

        operation
        (model/build-operation
         descriptor
         :rename)

        result
        (operation
         {:biff.fx/handlers
          {:biff.graph.fx/query
           (fn [_ctx input query]
             (reset!
              seen-graph
              [input query])
             {:widget/found?
              true

              :widget/doc
              current})

           model.tx/transact-effect
           (fn [_ctx plan]
             (reset!
              seen-plan
              plan)
             {:commit/status
              :committed})}}

         {:widget/id id
          :name "Renamed"})]

    (testing "the operation loads through the generated Graph contract"
      (is
       (=
        [{:widget/id id}
         (model/lookup-query descriptor)]
        @seen-graph)))

    (testing "Graph identity is removed from domain input and :now is supplied"
      (is
       (=
        "Renamed"
        (:name
         @seen-domain-input)))

      (is
       (not
        (contains?
         @seen-domain-input
         :widget/id)))

      (is
       (instance?
        Instant
        (:now
         @seen-domain-input))))

    (testing "the domain command remains the transaction representation"
      (let [model-command
            (first
             (:commands
              @seen-plan))

            after
            (command/after
             model-command)]

        (is
         (command/update?
          model-command))

        (is
         (identical?
          current
          (command/before
           model-command)))

        (is
         (=
          :rename
          (:model/operation
           model-command)))

        (is
         (=
          "Renamed"
          (:widget/name
           after)))

        (is
         (=
          4
          (:widget/revision
           after)))

        (is
         (instance?
          Instant
          (:widget/updated-at
           after)))

        (is
         (=
          [{:topic :widget
            :id id
            :change/kind :updated
            :widget/operation :rename}]
          (:changes
           @seen-plan)))))

    (testing "the operation returns the changed document and transaction result"
      (is
       (=
        "Renamed"
        (get-in
         result
         [:widget
          :widget/name])))

      (is
       (=
        {:commit/status
         :committed}
        (:transaction
         result))))

    (testing "generated operation metadata identifies an update"
      (is
       (=
        :update
        (:gesso.model/kind
         (meta operation))))

      (is
       (=
        :widget/rename
        (:gesso.model/operation-id
         (meta operation)))))))

(deftest generated-silent-operation-test
  (let [id
        (UUID/randomUUID)

        current
        (valid-widget id)

        seen-plan
        (atom nil)

        descriptor
        (assoc
         read-descriptor
         :operations
         {:rename
          rename-widget-command}
         :live
         {:emit false})

        operation
        (model/build-operation
         descriptor
         :rename)]

    (operation
     {:biff.fx/handlers
      {:biff.graph.fx/query
       (fn [_ctx _input _query]
         {:widget/found? true
          :widget/doc current})

       model.tx/transact-effect
       (fn [_ctx plan]
         (reset!
          seen-plan
          plan)
         :committed)}}

     {:widget/id id
      :name "Silent"})

    (is
     (=
      false
      (:emit
       @seen-plan)))

    (is
     (empty?
      (:changes
       @seen-plan)))))

;; =============================================================================
;; Operation collections
;; =============================================================================

(deftest generated-operation-collection-test
  (let [operations
        (model/build-operations
         full-descriptor)

        handlers
        (model/build-operation-handlers
         full-descriptor)

        compiled
        (model/compile-model
         full-descriptor)]

    (is
     (=
      #{:create
        :rename}
      (set
       (keys
        operations))))

    (is
     (=
      #{:widget/create
        :widget/rename}
      (set
       (keys
        handlers))))

    (is
     (identical?
      (get-in
       compiled
       [:operations :create])
      (get-in
       compiled
       [:fx-handlers :widget/create])))

    (is
     (identical?
      (get-in
       compiled
       [:operations :rename])
      (get-in
       compiled
       [:fx-handlers :widget/rename])))))

#_(deftest generated-operation-collection-test
  (let [operations
        (model/build-operations
         full-descriptor)

        handlers
        (model/build-operation-handlers
         full-descriptor)]

    (is
     (=
      #{:create
        :rename}
      (set
       (keys
        operations))))

    (is
     (=
      #{:widget/create
        :widget/rename}
      (set
       (keys
        handlers))))

    (is
     (identical?
      (:create
       operations)
      (:widget/create
       handlers)))

    (is
     (identical?
      (:rename
       operations)
      (:widget/rename
       handlers)))))

;; =============================================================================
;; Module assembly
;; =============================================================================

(deftest build-module-test
  (let [custom-schema
        {:widget/derived?
         :boolean}

        custom-resolver
        (graph/resolver
         {:id
          ::custom-resolver

          :input
          [:widget/id]

          :output
          [:widget/derived?]

          :resolve-fn
          (fn [_ctx _input]
            {:widget/derived?
             true})})

        custom-handler
        (fn [_ctx value]
          value)

        module
        (model/build-module
         [full-descriptor
          gadget-descriptor]

         {:schema
          custom-schema

          :resolvers
          [custom-resolver]

          :fx-handlers
          {::custom-operation
           custom-handler}})]

    (testing "generated schemas from several entities and custom derived schemas coexist"
      (is
       (=
        :boolean
        (get-in
         module
         [:schema
          :widget/derived?])))

      (is
       (=
        widget-document-schema
        (get-in
         module
         [:schema
          :widget])))

      (is
       (=
        gadget-document-schema
        (get-in
         module
         [:schema
          :gadget]))))

    (testing "Biff 2 module init registers the complete generated schema"
      (let [registered
            (atom nil)]

        (with-redefs
         [biff.core/register
          (fn [schema]
            (reset!
             registered
             schema))]

          (is
           (=
            {}
            ((:biff.core/init
              module)
             (atom
              [module]))))

          (is
           (=
            (:schema
             module)
            @registered)))))

    (testing "generated and custom resolvers coexist"
      (is
       (contains?
        (set
         (map
          :biff.graph/id
          (:biff.graph/resolvers
           module)))
        ::custom-resolver))

      (is
       (contains?
        (set
         (map
          :biff.graph/id
          (:biff.graph/resolvers
           module)))
        (model/by-id-resolver-id
         full-descriptor)))

      (is
       (contains?
        (set
         (map
          :biff.graph/id
          (:biff.graph/resolvers
           module)))
        (model/by-id-resolver-id
         gadget-descriptor))))

    (testing "generated and custom FX handlers coexist"
      (is
       (contains?
        (:biff.fx/handlers
         module)
        :widget/create))

      (is
       (contains?
        (:biff.fx/handlers
         module)
        :widget/rename))

      (is
       (identical?
        custom-handler
        (get-in
         module
         [:biff.fx/handlers
          ::custom-operation]))))

    (testing "the shared model.tx handler is not secretly installed by core"
      (is
       (not
        (contains?
         (:biff.fx/handlers
          module)
         model.tx/transact-effect))))))

(deftest build-module-collision-test
  (testing "custom schemas may not replace mechanically generated schemas"
    (is
     (=
      ::model/custom-schema-collision
      (error-type
       #(model/build-module
         [read-descriptor]
         {:schema
          {:widget/id
           :string}})))))

  (testing "custom FX may not replace generated operations"
    (is
     (=
      ::model/custom-fx-handler-collision
      (error-type
       #(model/build-module
         [full-descriptor]
         {:fx-handlers
          {:widget/rename
           identity}})))))

  (testing "custom resolver ids may not replace generated resolver ids"
    (let [colliding
          (graph/resolver
           {:id
            (model/by-id-resolver-id
             read-descriptor)

            :output
            [:widget/id]

            :resolve-fn
            (fn [_ctx _input]
              {:widget/id
               (UUID/randomUUID)})})]

      (is
       (=
        ::model/duplicate-resolver-ids
        (error-type
         #(model/build-module
           [read-descriptor]
           {:resolvers
            [colliding]})))))))

(deftest descriptor-collection-collision-test
  (testing "the same entity type cannot be compiled twice"
    (is
     (=
      ::model/duplicate-entity-types
      (error-type
       #(model/build-module
         [read-descriptor
          read-descriptor])))))

  (testing "generated operation ids must be globally unique"
    (let [other-schema
          [:map
           [:xt/id :uuid]
           [:other/revision [:int {:min 0}]]
           [:other/created-at instant-schema]
           [:other/updated-at instant-schema]]

          other
          {:entity-type
           :other

           :document-schema
           other-schema

           :identity
           {:graph-key
            :other/id}

           :version
           {:revision-key
            :other/revision

            :created-at-key
            :other/created-at

            :updated-at-key
            :other/updated-at}

           :operations
           {:widget/rename
            (fn [_current _input]
              (throw
               (AssertionError.
                "not executed")))}

           :live
           {:emit false}}]

      (is
       (=
        ::model/duplicate-generated-operation-ids
        (error-type
         #(model/build-module
           [full-descriptor
            other])))))))

;; =============================================================================
;; Inspectable compilation
;; =============================================================================

(deftest compile-model-test
  (let [compiled
        (model/compile-model
         full-descriptor)]

    (testing "compiled model keeps its canonical descriptor"
      (is
       (identical?
        full-descriptor
        (:descriptor
         compiled))))

    (testing "all mechanical derivations are available as ordinary data"
      (is
       (=
        (model/generated-schema
         full-descriptor)
        (:schema
         compiled)))

      (is
       (=
        (model/document-columns
         full-descriptor)
        (:document-columns
         compiled)))

      (is
       (=
        (model/document-query
         full-descriptor)
        (:document-query
         compiled)))

      (is
       (=
        (model/lookup-query
         full-descriptor)
        (:lookup-query
         compiled)))

      (is
       (=
        (model/field-query
         full-descriptor)
        (:field-query
         compiled)))

      (is
       (=
        (model/graph-projections
         full-descriptor)
        (:projections
         compiled))))

    (testing "resolvers and operations remain directly inspectable"
      (is
       (=
        #{(model/by-id-resolver-id
           full-descriptor)
          (model/fields-resolver-id
           full-descriptor)
          (model/lookup-resolver-id
           full-descriptor
           :widget/email)}
        (set
         (map
          :biff.graph/id
          (:resolvers
           compiled)))))

      (is
       (=
        #{:create
          :rename}
        (set
         (keys
          (:operations
           compiled)))))

      (is
       (=
        #{:widget/create
          :widget/rename}
        (set
         (keys
          (:fx-handlers
           compiled))))))

    (testing "the one-descriptor module is exactly the compiled pieces"
      (is
       (=
        (:schema
         compiled)
        (get-in
         compiled
         [:module
          :schema])))

      (is
       (ifn?
        (get-in
         compiled
         [:module
          :biff.core/init])))

      (is
       (=
        (:resolvers
         compiled)
        (get-in
         compiled
         [:module
          :biff.graph/resolvers])))

      (is
       (=
        (:fx-handlers
         compiled)
        (get-in
         compiled
         [:module
          :biff.fx/handlers]))))))
