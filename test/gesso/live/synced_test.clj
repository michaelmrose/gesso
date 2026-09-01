(ns gesso.live.synced-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gesso.live.consistency.xtdb :as live.xtdb]
   [gesso.live.synced :as synced]))

;; -----------------------------------------------------------------------------
;; Fixtures / helpers
;; -----------------------------------------------------------------------------

(def base-spec
  {:table
   :demo_counters

   :id
   "global-shared-counter"

   :col
   :demo/value

   :topic
   :demo-counter

   :default
   0})

(defn- descriptor
  ([]
   (synced/->synced
    base-spec))
  ([overrides]
   (synced/->synced
    (merge
     base-spec
     overrides))))

(defn- thrown
  [f]
  (try
    (f)
    nil
    (catch Throwable error
      error)))

(defn- thrown-data
  [f]
  (some->
   (thrown f)
   ex-data))

;; -----------------------------------------------------------------------------
;; Descriptor identity
;; -----------------------------------------------------------------------------

(deftest descriptor-shape-test
  (let [value
        (descriptor)]

    (is (= :gesso.live.synced/value
           (:gesso.live.synced/type
            value)))

    (is (= :demo_counters
           (:table
            value)))

    (is (= "global-shared-counter"
           (:id
            value)))

    (is (= :demo/value
           (:col
            value)))

    (is (= :demo-counter
           (:topic
            value)))

    (is (= 0
           (:default
            value)))

    (is (= :updated
           (:change/kind
            value)))

    (is (= [:demo-counter
            "global-shared-counter"]
           (:coalesce-key
            value)))

    (is (= base-spec
           (:spec
            value)))))

(deftest synced-predicate-test
  (let [value
        (descriptor)]

    (is (synced/synced?
         value))

    (is (false?
         (synced/synced?
          nil)))

    (is (false?
         (synced/synced?
          {})))

    (is (false?
         (synced/synced?
          {:gesso.live.synced/type
           :other})))))

(deftest require-synced-test
  (let [value
        (descriptor)]

    (is (identical?
         value
         (synced/require-synced!
          value)))

    (doseq [invalid
            [nil
             {}
             {:gesso.live.synced/type
              :not-synced}]]

      (let [error
            (thrown
             #(synced/require-synced!
               invalid))]

        (is (instance?
             clojure.lang.ExceptionInfo
             error))

        (is (= "Expected a gesso.live synced value descriptor."
               (ex-message
                error)))

        (is (= {:value
                invalid}
               (ex-data
                error)))))))

;; -----------------------------------------------------------------------------
;; Required descriptor fields
;; -----------------------------------------------------------------------------

(deftest descriptor-requires-table-test
  (let [error
        (thrown
         #(synced/->synced
           (dissoc
            base-spec
            :table)))]

    (is (= "gesso.live synced value requires :table."
           (ex-message
            error)))

    (is (= {:table
            nil}
           (ex-data
            error)))))

(deftest descriptor-requires-id-test
  (let [error
        (thrown
         #(synced/->synced
           (dissoc
            base-spec
            :id)))]

    (is (= "gesso.live synced value requires :id."
           (ex-message
            error)))

    (is (= {:id
            nil}
           (ex-data
            error)))))

(deftest descriptor-requires-column-test
  (let [error
        (thrown
         #(synced/->synced
           (dissoc
            base-spec
            :col)))]

    (is (= "gesso.live synced value requires :col."
           (ex-message
            error)))

    (is (= {:col
            nil}
           (ex-data
            error)))))

(deftest descriptor-only-treats-nil-as-missing-id-test
  (doseq [id
          [false
           0
           ""
           :opaque/id
           [:compound
            1]
           {:opaque
            "id"}]]

    (is (= id
           (:id
            (descriptor
             {:id
              id}))))))

;; -----------------------------------------------------------------------------
;; Topic selection
;; -----------------------------------------------------------------------------

(deftest explicit-topic-wins-test
  (let [value
        (synced/->synced
         {:table
          :demo_counters

          :id
          "counter"

          :col
          :demo/value

          :topic
          :preferred-topic

          :entity-type
          :compat-topic})]

    (is (= :preferred-topic
           (:topic
            value)))

    (is (= [:preferred-topic
            "counter"]
           (:coalesce-key
            value)))))

(deftest entity-type-is-topic-compatibility-alias-test
  (let [value
        (synced/->synced
         {:table
          :demo_counters

          :id
          "counter"

          :col
          :demo/value

          :entity-type
          :legacy-topic})]

    (is (= :legacy-topic
           (:topic
            value)))

    (is (= [:legacy-topic
            "counter"]
           (:coalesce-key
            value)))))

(deftest table-is-default-topic-test
  (let [value
        (synced/->synced
         {:table
          :demo_counters

          :id
          "counter"

          :col
          :demo/value})]

    (is (= :demo_counters
           (:topic
            value)))

    (is (= [:demo_counters
            "counter"]
           (:coalesce-key
            value)))))

;; -----------------------------------------------------------------------------
;; Change kind / coalescing configuration
;; -----------------------------------------------------------------------------

(deftest descriptor-change-kind-default-test
  (is (= :updated
         (:change/kind
          (descriptor)))))

(deftest descriptor-explicit-change-kind-test
  (is (= :created
         (:change/kind
          (descriptor
           {:change/kind
            :created})))))

(deftest descriptor-explicit-coalesce-key-test
  (let [key
        [:counter
         "global"]

        value
        (descriptor
         {:coalesce-key
          key})]

    (is (= key
           (:coalesce-key
            value)))

    (is (= {:coalesce-key
            key}
           (synced/entry
            value)))))

(deftest descriptor-nil-coalesce-key-falls-back-to-topic-id-test
  (let [value
        (descriptor
         {:coalesce-key
          nil})]

    (is (= [:demo-counter
            "global-shared-counter"]
           (:coalesce-key
            value)))

    (is (= {:coalesce-key
            [:demo-counter
             "global-shared-counter"]}
           (synced/entry
            value)))))

;; -----------------------------------------------------------------------------
;; Original spec preservation
;; -----------------------------------------------------------------------------

(deftest descriptor-preserves-original-spec-test
  (let [spec
        {:table
         :demo_counters

         :id
         "counter"

         :col
         :demo/value

         :entity-type
         :legacy-counter

         :default
         11

         :change/kind
         :changed

         :coalesce-key
         [:custom
          "counter"]

         :application/metadata
         {:kept
          true}}

        value
        (synced/->synced
         spec)]

    (is (= spec
           (:spec
            value)))

    (is (= {:kept
            true}
           (get-in
            value
            [:spec
             :application/metadata])))))

;; -----------------------------------------------------------------------------
;; SQL table identifier validation
;; -----------------------------------------------------------------------------

(deftest table-identifiers-may-be-keywords-symbols-or-strings-test
  (doseq [table
          [:demo_counters
           'demo_counters
           "demo_counters"
           :app/demo_counters
           'app/demo_counters]]

    (let [value
          (descriptor
           {:table
            table})]

      (is (= table
             (:table
              value)))

      (is (synced/synced?
           value)))))

(deftest invalid-table-identifiers-fail-at-definition-time-test
  (doseq [table
          [""
           "demo-counters"
           "demo counters"
           "demo.counters"
           "demo/counters"
           "1demo"
           :demo-counters
           'demo-counters]]

    (let [error
          (thrown
           #(descriptor
             {:table
              table}))

          data
          (ex-data
           error)]

      (is (instance?
           clojure.lang.ExceptionInfo
           error)
          (str
           "Expected invalid table to fail: "
           (pr-str table)))

      (is (= "gesso.live synced value requires a simple SQL identifier."
             (ex-message
              error)))

      (is (= :table
             (:kind
              data)))

      (is (= table
             (:value
              data)))

      (is (string?
           (:normalized
            data))))))

(deftest namespaced-table-uses-local-name-in-query-test
  (let [value
        (descriptor
         {:table
          :application/demo_counters})]

    (is (= ["SELECT _id, demo$value FROM demo_counters WHERE _id = ?"
            "global-shared-counter"]
           (synced/query
            value)))))

;; -----------------------------------------------------------------------------
;; SQL column identifier validation
;; -----------------------------------------------------------------------------

(deftest columns-may-be-keywords-symbols-or-strings-test
  (doseq [column
          [:value
           :demo/value
           'value
           'demo/value
           "value"
           "demo$value"
           "_value"
           "value_2"]]

    (is (= column
           (:col
            (descriptor
             {:col
              column}))))))

(deftest invalid-column-identifiers-fail-at-definition-time-test
  (doseq [column
          [""
           "demo-value"
           "demo value"
           "demo.value"
           "demo/value"
           "$value"
           "1value"
           :demo-value
           'demo-value]]

    (let [error
          (thrown
           #(descriptor
             {:col
              column}))

          data
          (ex-data
           error)]

      (is (instance?
           clojure.lang.ExceptionInfo
           error)
          (str
           "Expected invalid column to fail: "
           (pr-str column)))

      (is (= "gesso.live synced value requires a simple SQL column identifier."
             (ex-message
              error)))

      (is (= column
             (:attr
              data)))

      (is (string?
           (:normalized
            data))))))

;; -----------------------------------------------------------------------------
;; Query generation
;; -----------------------------------------------------------------------------

(deftest query-namespaced-keyword-column-test
  (is (= ["SELECT _id, demo$value FROM demo_counters WHERE _id = ?"
          "global-shared-counter"]
         (synced/query
          (descriptor)))))

(deftest query-plain-keyword-column-test
  (is (= ["SELECT _id, value FROM demo_counters WHERE _id = ?"
          "global-shared-counter"]
         (synced/query
          (descriptor
           {:col
            :value})))))

(deftest query-namespaced-symbol-column-test
  (is (= ["SELECT _id, demo$value FROM demo_counters WHERE _id = ?"
          "global-shared-counter"]
         (synced/query
          (descriptor
           {:col
            'demo/value})))))

(deftest query-string-column-test
  (is (= ["SELECT _id, demo$value FROM demo_counters WHERE _id = ?"
          "global-shared-counter"]
         (synced/query
          (descriptor
           {:col
            "demo$value"})))))

(deftest query-preserves-opaque-document-id-as-parameter-test
  (let [id
        [:counter
         {:tenant
          7}]]

    (is (= ["SELECT _id, demo$value FROM demo_counters WHERE _id = ?"
            id]
           (synced/query
            (descriptor
             {:id
              id}))))))

(deftest query-requires-synced-descriptor-test
  (is (= {:value
          {:table
           :demo_counters}}
         (thrown-data
          #(synced/query
            {:table
             :demo_counters})))))

(deftest query-revalidates-table-and-column-test
  (let [base
        (descriptor)]

    (is (= :table
           (:kind
            (thrown-data
             #(synced/query
               (assoc
                base
                :table
                "bad-table"))))))

    (is (= "bad-column"
           (:attr
            (thrown-data
             #(synced/query
               (assoc
                base
                :col
                "bad-column"))))))))

;; -----------------------------------------------------------------------------
;; Row extraction
;; -----------------------------------------------------------------------------

(deftest value-from-row-prefers-descriptor-key-test
  (let [value
        (descriptor)]

    (is (= 10
           (synced/value-from-row
            value
            {:demo/value
             10

             :demo$value
             20

             :value
             30})))))

(deftest value-from-row-understands-xtdb-normalized-key-test
  (is (= 42
         (synced/value-from-row
          (descriptor)
          {:demo/value
           42}))))

(deftest value-from-row-understands-sql-column-key-test
  (is (= 42
         (synced/value-from-row
          (descriptor)
          {:demo$value
           42}))))

(deftest value-from-row-understands-unqualified-keyword-fallback-test
  (is (= 42
         (synced/value-from-row
          (descriptor)
          {:value
           42}))))

(deftest value-from-row-string-column-test
  (is (= 42
         (synced/value-from-row
          (descriptor
           {:col
            "demo$value"})
          {:demo$value
           42}))))

(deftest value-from-row-plain-column-test
  (is (= 42
         (synced/value-from-row
          (descriptor
           {:col
            :value})
          {:value
           42}))))

(deftest value-from-row-preserves-nil-when-column-is-present-test
  (is (nil?
       (synced/value-from-row
        (descriptor)
        {:demo/value
         nil}))))

(deftest value-from-row-preserves-false-test
  (is (false?
       (synced/value-from-row
        (descriptor)
        {:demo/value
         false}))))

(deftest value-from-row-preserves-zero-test
  (is (= 0
         (synced/value-from-row
          (descriptor
           {:default
            99})
          {:demo/value
           0}))))

(deftest value-from-row-present-nil-does-not-fall-through-to-alternate-key-test
  (is (nil?
       (synced/value-from-row
        (descriptor
         {:default
          99})
        {:demo/value
         nil

         :demo$value
         42}))))

(deftest value-from-row-missing-column-returns-default-test
  (is (= 7
         (synced/value-from-row
          (descriptor
           {:default
            7})
          {:unrelated
           42}))))

(deftest value-from-row-nil-row-returns-default-test
  (is (= 7
         (synced/value-from-row
          (descriptor
           {:default
            7})
          nil))))

(deftest value-from-row-empty-map-returns-default-test
  (is (= 7
         (synced/value-from-row
          (descriptor
           {:default
            7})
          {}))))

(deftest value-from-row-default-may-be-nil-test
  (is (nil?
       (synced/value-from-row
        (descriptor
         {:default
          nil})
        {}))))

(deftest value-from-row-default-may-be-false-test
  (is (false?
       (synced/value-from-row
        (descriptor
         {:default
          false})
        {}))))

;; -----------------------------------------------------------------------------
;; Consistent reads
;; -----------------------------------------------------------------------------

(deftest live-read-uses-consistency-aware-query-path-test
  (let [value
        (descriptor)

        ctx
        {:biff/conn
         :connection

         :gesso.live/consistency
         {:snapshot-time
          :time}}

        calls
        (atom [])]

    (with-redefs
     [live.xtdb/q-consistent-from
      (fn
        ([actual-ctx query]
         (swap!
          calls
          conj
          [actual-ctx
           query])
         [{:demo/value
           42}])

        ([actual-ctx query opts]
         (swap!
          calls
          conj
          [actual-ctx
           query
           opts])
         [{:demo/value
           99}]))]

      (is (= 42
             (synced/live-read
              ctx
              value)))

      (is (= [[ctx
               ["SELECT _id, demo$value FROM demo_counters WHERE _id = ?"
                "global-shared-counter"]]]
             @calls)))))

(deftest live-read-passes-explicit-query-options-test
  (let [value
        (descriptor)

        ctx
        {:gesso.live/consistency
         {:snapshot-time
          :time}}

        opts
        {:key-fn
         :snake-case-string}

        seen
        (atom nil)]

    (with-redefs
     [live.xtdb/q-consistent-from
      (fn [actual-ctx query actual-opts]
        (reset!
         seen
         [actual-ctx
          query
          actual-opts])

        [{:demo$value
          13}])]

      (is (= 13
             (synced/live-read
              ctx
              value
              opts)))

      (is (= [ctx
              ["SELECT _id, demo$value FROM demo_counters WHERE _id = ?"
               "global-shared-counter"]
              opts]
             @seen)))))

(deftest live-read-explicit-empty-options-use-three-arity-test
  (let [value
        (descriptor)

        arity
        (atom nil)]

    (with-redefs
     [live.xtdb/q-consistent-from
      (fn
        ([_ctx _query]
         (reset!
          arity
          2)

         [])

        ([_ctx _query _opts]
         (reset!
          arity
          3)

         []))]

      (synced/live-read
       {}
       value
       {})

      (is (= 3
             @arity)))))

(deftest live-read-uses-first-row-only-test
  (with-redefs
   [live.xtdb/q-consistent-from
    (fn [_ctx _query]
      [{:demo/value
        1}

       {:demo/value
        2}])]

    (is (= 1
           (synced/live-read
            {}
            (descriptor))))))

(deftest live-read-empty-results-return-default-test
  (with-redefs
   [live.xtdb/q-consistent-from
    (fn [_ctx _query]
      [])]

    (is (= 17
           (synced/live-read
            {}
            (descriptor
             {:default
              17}))))))

(deftest live-read-propagates-consistency-query-errors-test
  (let [cause
        (ex-info
         "XTDB read failed."
         {:error/type
          :test/read-failed})]

    (with-redefs
     [live.xtdb/q-consistent-from
      (fn [& _]
        (throw
         cause))]

      (is (identical?
           cause
           (thrown
            #(synced/live-read
              {}
              (descriptor))))))))

;; -----------------------------------------------------------------------------
;; Documents
;; -----------------------------------------------------------------------------

(deftest doc-test
  (is (= {:xt/id
          "global-shared-counter"

          :demo/value
          42}
         (synced/doc
          (descriptor)
          42))))

(deftest doc-preserves-opaque-document-id-test
  (let [id
        [:counter
         7]]

    (is (= {:xt/id
            id

            :demo/value
            42}
           (synced/doc
            (descriptor
             {:id
              id})
            42)))))

(deftest doc-preserves-nil-value-test
  (is (= {:xt/id
          "global-shared-counter"

          :demo/value
          nil}
         (synced/doc
          (descriptor)
          nil))))

(deftest doc-preserves-false-value-test
  (is (= {:xt/id
          "global-shared-counter"

          :demo/value
          false}
         (synced/doc
          (descriptor)
          false))))

(deftest doc-preserves-structured-value-test
  (let [value
        {:count
         4

         :nested
         [1 2 3]}]

    (is (= {:xt/id
            "global-shared-counter"

            :demo/value
            value}
           (synced/doc
            (descriptor)
            value)))))

(deftest doc-requires-synced-descriptor-test
  (is (= {:value
          {}}
         (thrown-data
          #(synced/doc
            {}
            1)))))

;; -----------------------------------------------------------------------------
;; XTDB transaction ops
;; -----------------------------------------------------------------------------

(deftest tx-ops-test
  (is (= [[:put-docs
           :demo_counters

           {:xt/id
            "global-shared-counter"

            :demo/value
            42}]]
         (synced/tx-ops
          (descriptor)
          42))))

(deftest tx-ops-preserve-configured-table-value-test
  (is (= [[:put-docs
           'demo_counters

           {:xt/id
            "global-shared-counter"

            :demo/value
            42}]]
         (synced/tx-ops
          (descriptor
           {:table
            'demo_counters})
          42))))

(deftest tx-ops-delegates-through-xtdb-helper-test
  (let [seen
        (atom nil)

        sentinel
        [:sentinel-op]]

    (with-redefs
     [live.xtdb/put-docs-op
      (fn [table doc]
        (reset!
         seen
         [table
          doc])

        sentinel)]

      (is (= [sentinel]
             (synced/tx-ops
              (descriptor)
              42)))

      (is (= [:demo_counters
              {:xt/id
               "global-shared-counter"

               :demo/value
               42}]
             @seen)))))

(deftest tx-ops-requires-synced-descriptor-test
  (is (= {:value
          {}}
         (thrown-data
          #(synced/tx-ops
            {}
            1)))))

;; -----------------------------------------------------------------------------
;; Guarded XTDB transaction ops
;; -----------------------------------------------------------------------------

(deftest guarded-tx-ops-assert-exact-non-default-value-test
  (is (= [[:sql
           "ASSERT EXISTS (SELECT 1 FROM demo_counters WHERE _id = ? AND demo$value = ?)"
           ["global-shared-counter"
            7]]

          [:put-docs
           :demo_counters

           {:xt/id
            "global-shared-counter"

            :demo/value
            8}]]
         (synced/guarded-tx-ops
          (descriptor)
          7
          8))))

(deftest guarded-tx-ops-default-guard-matches-logical-absence-null-or-default-test
  (is (= [[:sql
           (str
            "ASSERT ("
            "NOT EXISTS (SELECT 1 FROM demo_counters WHERE _id = ?) "
            "OR EXISTS (SELECT 1 FROM demo_counters "
            "WHERE _id = ? AND (demo$value IS NULL OR demo$value = ?))"
            ")")
           ["global-shared-counter"
            "global-shared-counter"
            0]]

          [:put-docs
           :demo_counters

           {:xt/id
            "global-shared-counter"

            :demo/value
            1}]]
         (synced/guarded-tx-ops
          (descriptor)
          0
          1))))

(deftest guarded-tx-ops-nil-default-uses-logical-default-guard-test
  (is (= [[:sql
           (str
            "ASSERT ("
            "NOT EXISTS (SELECT 1 FROM demo_counters WHERE _id = ?) "
            "OR EXISTS (SELECT 1 FROM demo_counters "
            "WHERE _id = ? AND (demo$value IS NULL OR demo$value = ?))"
            ")")
           ["global-shared-counter"
            "global-shared-counter"
            nil]]

          [:put-docs
           :demo_counters

           {:xt/id
            "global-shared-counter"

            :demo/value
            :first-value}]]
         (synced/guarded-tx-ops
          (descriptor
           {:default
            nil})
          nil
          :first-value))))

(deftest guarded-tx-ops-delegates-write-through-xtdb-helper-test
  (let [seen
        (atom nil)

        sentinel
        [:sentinel-put]]

    (with-redefs
     [live.xtdb/put-docs-op
      (fn [table doc]
        (reset!
         seen
         [table
          doc])

        sentinel)]

      (is (= [[:sql
               "ASSERT EXISTS (SELECT 1 FROM demo_counters WHERE _id = ? AND demo$value = ?)"
               ["global-shared-counter"
                41]]
              sentinel]
             (synced/guarded-tx-ops
              (descriptor)
              41
              42)))

      (is (= [:demo_counters
              {:xt/id
               "global-shared-counter"

               :demo/value
               42}]
             @seen)))))

(deftest guarded-tx-ops-requires-synced-descriptor-test
  (is (= {:value
          {}}
         (thrown-data
          #(synced/guarded-tx-ops
            {}
            0
            1)))))

(deftest guarded-tx-ops-revalidates-mutated-identifiers-test
  (let [base
        (descriptor)]

    (is (= :table
           (:kind
            (thrown-data
             #(synced/guarded-tx-ops
               (assoc
                base
                :table
                "bad-table")
               0
               1)))))

    (is (= "bad-column"
           (:attr
            (thrown-data
             #(synced/guarded-tx-ops
               (assoc
                base
                :col
                "bad-column")
               0
               1)))))))

;; -----------------------------------------------------------------------------
;; Primary live change
;; -----------------------------------------------------------------------------

(deftest change-default-shape-test
  (is (= {:topic
          :demo-counter

          :id
          "global-shared-counter"

          :change/kind
          :updated

          :new-value
          42}
         (synced/change
          (descriptor)
          42))))

(deftest change-uses-descriptor-kind-test
  (is (= :created
         (:change/kind
          (synced/change
           (descriptor
            {:change/kind
             :created})
           42)))))

(deftest change-kind-option-overrides-descriptor-test
  (is (= :reset
         (:change/kind
          (synced/change
           (descriptor
            {:change/kind
             :updated})
           42
           {:change/kind
            :reset})))))

(deftest change-carries-old-and-new-values-test
  (is (= {:topic
          :demo-counter

          :id
          "global-shared-counter"

          :change/kind
          :updated

          :old-value
          41

          :new-value
          42}
         (synced/change
          (descriptor)
          42
          {:old-value
           41

           :new-value
           42}))))

(deftest change-new-value-defaults-to-value-argument-test
  (is (= {:topic
          :demo-counter

          :id
          "global-shared-counter"

          :change/kind
          :updated

          :new-value
          {:count
           42}}
         (synced/change
          (descriptor)
          {:count
           42}))))

(deftest change-explicit-new-value-overrides-value-argument-test
  (is (= "semantic-new"
         (:new-value
          (synced/change
           (descriptor)
           "write-value"
           {:new-value
            "semantic-new"})))))

(deftest change-carries-data-test
  (is (= {:actor-id
          "user-1"

          :reason
          :manual}
         (:data
          (synced/change
           (descriptor)
           42
           {:data
            {:actor-id
             "user-1"

             :reason
             :manual}})))))

(deftest change-compacts-absent-optional-fields-test
  (let [change
        (synced/change
         (descriptor)
         42)]

    (is (not
         (contains?
          change
          :old-value)))

    (is (not
         (contains?
          change
          :data)))))

(deftest change-preserves-false-old-and-new-values-test
  (is (= {:topic
          :demo-counter

          :id
          "global-shared-counter"

          :change/kind
          :updated

          :old-value
          false

          :new-value
          false}
         (synced/change
          (descriptor)
          true
          {:old-value
           false

           :new-value
           false}))))

(deftest change-preserves-zero-old-and-new-values-test
  (is (= {:topic
          :demo-counter

          :id
          "global-shared-counter"

          :change/kind
          :updated

          :old-value
          0

          :new-value
          0}
         (synced/change
          (descriptor)
          1
          {:old-value
           0

           :new-value
           0}))))

(deftest change-nil-value-is-compacted-test
  (let [change
        (synced/change
         (descriptor)
         nil)]

    (is (= {:topic
            :demo-counter

            :id
            "global-shared-counter"

            :change/kind
            :updated}
           change))

    (is (not
         (contains?
          change
          :new-value)))))

(deftest change-explicit-nil-old-value-is-compacted-test
  (let [change
        (synced/change
         (descriptor)
         42
         {:old-value
          nil})]

    (is (= 42
           (:new-value
            change)))

    (is (not
         (contains?
          change
          :old-value)))))

(deftest change-explicit-nil-new-value-is-compacted-test
  (let [change
        (synced/change
         (descriptor)
         42
         {:new-value
          nil})]

    (is (not
         (contains?
          change
          :new-value)))))

(deftest change-nil-data-is-compacted-test
  (let [change
        (synced/change
         (descriptor)
         42
         {:data
          nil})]

    (is (not
         (contains?
          change
          :data)))))

(deftest change-preserves-false-data-test
  (is (= false
         (:data
          (synced/change
           (descriptor)
           42
           {:data
            false})))))

(deftest change-requires-synced-descriptor-test
  (is (= {:value
          {}}
         (thrown-data
          #(synced/change
            {}
            1)))))

;; -----------------------------------------------------------------------------
;; Dispatch entry
;; -----------------------------------------------------------------------------

(deftest entry-default-test
  (is (= {:coalesce-key
          [:demo-counter
           "global-shared-counter"]}
         (synced/entry
          (descriptor)))))

(deftest entry-explicit-coalesce-key-test
  (is (= {:coalesce-key
          [:custom
           7]}
         (synced/entry
          (descriptor
           {:coalesce-key
            [:custom
             7]})))))

(deftest entry-requires-synced-descriptor-test
  (is (= {:value
          {}}
         (thrown-data
          #(synced/entry
            {})))))

;; -----------------------------------------------------------------------------
;; Plain-data boundaries
;; -----------------------------------------------------------------------------

(deftest descriptor-is-plain-data-test
  (let [value
        (descriptor)]

    (is (map?
         value))

    (is (not-any?
         fn?
         (vals
          value)))

    (is (not-any?
         #(instance?
           clojure.lang.IDeref
           %)
         (vals
          value)))))

(deftest query-is-plain-parameterized-data-test
  (let [query
        (synced/query
         (descriptor))]

    (is (vector?
         query))

    (is (string?
         (first
          query)))

    (is (= 2
           (count
            query)))

    (is (not
         (str/includes?
          (first
           query)
          "global-shared-counter"))
        "The document id belongs in the query parameter, not interpolated SQL.")))

;; -----------------------------------------------------------------------------
;; Complete low-level roundtrip
;; -----------------------------------------------------------------------------

(deftest descriptor-read-write-change-entry-roundtrip-test
  (let [counter
        (synced/->synced
         {:table
          :demo_counters

          :id
          "counter-1"

          :col
          :demo/value

          :topic
          :demo-counter

          :default
          0

          :change/kind
          :counter/changed

          :coalesce-key
          [:demo-counter
           "counter-1"]})

        ctx
        {:gesso.live/consistency
         {:snapshot-time
          :snapshot-7}}

        query-seen
        (atom nil)]

    (with-redefs
     [live.xtdb/q-consistent-from
      (fn [actual-ctx query]
        (reset!
         query-seen
         [actual-ctx
          query])

        [{:demo$value
          7}])]

      (let [current
            (synced/live-read
             ctx
             counter)

            next-value
            (inc
             current)

            document
            (synced/doc
             counter
             next-value)

            tx-ops
            (synced/tx-ops
             counter
             next-value)

            change
            (synced/change
             counter
             next-value
             {:old-value
              current

              :data
              {:source
               :button}})

            entry
            (synced/entry
             counter)]

        (is (= 7
               current))

        (is (= [ctx
                ["SELECT _id, demo$value FROM demo_counters WHERE _id = ?"
                 "counter-1"]]
               @query-seen))

        (is (= {:xt/id
                "counter-1"

                :demo/value
                8}
               document))

        (is (= [[:put-docs
                 :demo_counters

                 {:xt/id
                  "counter-1"

                  :demo/value
                  8}]]
               tx-ops))

        (is (= {:topic
                :demo-counter

                :id
                "counter-1"

                :change/kind
                :counter/changed

                :old-value
                7

                :new-value
                8

                :data
                {:source
                 :button}}
               change))

        (is (= {:coalesce-key
                [:demo-counter
                 "counter-1"]}
               entry))))))

;; -----------------------------------------------------------------------------
;; Read path and write metadata stay deliberately separate
;; -----------------------------------------------------------------------------

(deftest live-read-does-not-build-write-or-change-data-test
  (let [tx-calls
        (atom 0)

        read-calls
        (atom 0)]

    (with-redefs
     [live.xtdb/q-consistent-from
      (fn [_ctx _query]
        (swap!
         read-calls
         inc)

        [{:demo/value
          42}])

      live.xtdb/put-docs-op
      (fn [& _]
        (swap!
         tx-calls
         inc)

        :unexpected)]

      (is (= 42
             (synced/live-read
              {}
              (descriptor))))

      (is (= 1
             @read-calls))

      (is (= 0
             @tx-calls)))))

;; -----------------------------------------------------------------------------
;; Descriptor mutation defenses at public query boundary
;; -----------------------------------------------------------------------------

(deftest forged-synced-marker-does-not-bypass-query-sql-validation-test
  (let [forged
        {:gesso.live.synced/type
         :gesso.live.synced/value

         :table
         "bad table"

         :id
         "counter"

         :col
         "also-bad-column"}]

    (let [data
          (thrown-data
           #(synced/query
             forged))]

      (is (= :table
             (:kind
              data)))

      (is (= "bad table"
             (:value
              data))))))

(deftest forged-synced-marker-is-insufficient-for-safe-construction-test
  (let [forged
        {:gesso.live.synced/type
         :gesso.live.synced/value}]

    (testing "type identity alone intentionally makes synced? a cheap predicate"
      (is (synced/synced?
           forged)))

    (testing "operations that need descriptor fields still fail rather than inventing them"
      (is (instance?
           Throwable
           (thrown
            #(synced/query
              forged)))))))

;; -----------------------------------------------------------------------------
;; SQL safety regressions
;; -----------------------------------------------------------------------------

(deftest table-name-is-never-accepted-as-free-form-sql-test
  (doseq [table
          ["demo_counters; DROP TABLE users"
           "demo_counters WHERE true"
           "demo_counters --"
           "\"demo_counters\""]]

    (is (instance?
         clojure.lang.ExceptionInfo
         (thrown
          #(descriptor
            {:table
             table}))))))

(deftest column-name-is-never-accepted-as-free-form-sql-test
  (doseq [column
          ["value, secret"
           "value FROM users"
           "value; DROP TABLE users"
           "\"value\""
           "value::text"]]

    (is (instance?
         clojure.lang.ExceptionInfo
         (thrown
          #(descriptor
            {:col
             column}))))))

;; -----------------------------------------------------------------------------
;; Values and metadata are not application-interpreted
;; -----------------------------------------------------------------------------

(deftest synced-value-is-opaque-to-helper-layer-test
  (let [opaque
        {:application/state
         :whatever

         :nested
         [{:x
           1}

          {:x
           2}]}

        counter
        (descriptor)]

    (is (= opaque
           (:demo/value
            (synced/doc
             counter
             opaque))))

    (is (= opaque
           (:new-value
            (synced/change
             counter
             opaque))))))

(deftest topic-and-coalesce-key-remain-application-data-test
  (let [topic
        [:not
         :necessarily
         :a-keyword]

        coalesce-key
        {:opaque
         [:dispatch
          1]}

        value
        (descriptor
         {:topic
          topic

          :coalesce-key
          coalesce-key})]

    (is (= topic
           (:topic
            value)))

    (is (= coalesce-key
           (:coalesce-key
            value)))

    (is (= {:coalesce-key
            coalesce-key}
           (synced/entry
            value)))))

;; -----------------------------------------------------------------------------
;; Defaults are read semantics, not write semantics
;; -----------------------------------------------------------------------------

(deftest default-value-is-used-only-when-read-value-is-missing-test
  (let [value
        (descriptor
         {:default
          99})]

    (is (= 99
           (synced/value-from-row
            value
            nil)))

    (is (= {:xt/id
            "global-shared-counter"

            :demo/value
            nil}
           (synced/doc
            value
            nil)))

    (is (not
         (contains?
          (synced/change
           value
           nil)
          :new-value)))))

;; -----------------------------------------------------------------------------
;; Query options are delegated, not interpreted by synced
;; -----------------------------------------------------------------------------

(deftest live-read-delegates-arbitrary-query-option-map-test
  (let [opts
        {:key-fn
         :snake-case-string

         :database
         "db"

         :application/extra
         "left-for-consistency-adapter"}

        seen
        (atom nil)]

    (with-redefs
     [live.xtdb/q-consistent-from
      (fn [_ctx _query actual-opts]
        (reset!
         seen
         actual-opts)

        [])]

      (synced/live-read
       {}
       (descriptor)
       opts)

      (is (identical?
           opts
           @seen)))))

;; -----------------------------------------------------------------------------
;; Errors from lower layers stay transparent
;; -----------------------------------------------------------------------------

(deftest tx-op-helper-errors-are-not-rewrapped-test
  (let [cause
        (ex-info
         "put-docs failed"
         {:error/type
          :test/put-docs-failed})]

    (with-redefs
     [live.xtdb/put-docs-op
      (fn [& _]
        (throw
         cause))]

      (is (identical?
           cause
           (thrown
            #(synced/tx-ops
              (descriptor)
              42)))))))

;; -----------------------------------------------------------------------------
;; Repeated construction is deterministic
;; -----------------------------------------------------------------------------

(deftest descriptor-construction-is-deterministic-test
  (is (= (descriptor)
         (descriptor))))

(deftest query-construction-is-deterministic-test
  (let [value
        (descriptor)]

    (is (= (synced/query
            value)
           (synced/query
            value)))))

(deftest change-construction-is-deterministic-test
  (let [value
        (descriptor)]

    (is (= (synced/change
            value
            42
            {:old-value
             41

             :data
             {:reason
              :test}})
           (synced/change
            value
            42
            {:old-value
             41

             :data
             {:reason
              :test}})))))
