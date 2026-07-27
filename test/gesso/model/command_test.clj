(ns gesso.model.command-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.model.command :as command]))

(def version
  {:revision-key :widget/revision
   :created-at-key :widget/created-at
   :updated-at-key :widget/updated-at})

(def custom-version
  {:revision-key :widget/revision
   :created-at-key :widget/created-at
   :updated-at-key :widget/updated-at
   :compare-keys
   [:widget/revision
    :widget/state-token]})

(def before
  {:xt/id :widget-1
   :widget/name "Before"
   :widget/revision 4
   :widget/created-at :t0
   :widget/updated-at :t1
   :widget/state-token "state-4"})

(def after
  {:xt/id :widget-1
   :widget/name "After"
   :widget/revision 5
   :widget/created-at :t0
   :widget/updated-at :t2
   :widget/state-token "state-5"})

(def initial
  {:xt/id :widget-2
   :widget/name "New"
   :widget/revision 0
   :widget/created-at :t0
   :widget/updated-at :t0
   :widget/state-token "state-0"})

(defn error-type
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo ex
      (:error/type
       (ex-data ex)))))

(deftest version-metadata-test
  (testing "the conventional version shape is accepted"
    (is
     (command/valid-version?
      version))

    (is
     (=
      version
      (command/require-version
       version))))

  (testing "compare keys default to revision plus updated-at"
    (is
     (=
      [:widget/revision
       :widget/updated-at]
      (command/compare-keys
       version))))

  (testing "explicit compare keys are preserved in declaration order"
    (is
     (=
      [:widget/revision
       :widget/state-token]
      (command/compare-keys
       custom-version))))

  (testing "all three conventional version fields are required and distinct"
    (is
     (not
      (command/valid-version?
       (dissoc
        version
        :revision-key))))

    (is
     (not
      (command/valid-version?
       (assoc
        version
        :updated-at-key
        :widget/created-at))))

    (is
     (=
      ::command/invalid-version
      (error-type
       #(command/require-version
         (dissoc
          version
          :created-at-key))))))

  (testing "version metadata rejects unknown keys"
    (is
     (not
      (command/valid-version?
       (assoc
        version
        :something-else
        true)))))

  (testing "custom compare keys must be nonempty distinct keywords"
    (doseq [compare-keys
            [[]
             [:widget/revision
              :widget/revision]
             [:widget/revision
              "not-a-keyword"]]]
      (is
       (not
        (command/valid-version?
         (assoc
          version
          :compare-keys
          compare-keys)))))))

(deftest versioned-document-test
  (testing "generic version mechanics do not prescribe id or timestamp types"
    (is
     (command/versioned-document?
      before
      version))

    (is
     (command/versioned-document?
      (assoc
       before
       :xt/id
       42
       :widget/created-at
       "created"
       :widget/updated-at
       "updated")
      version)))

  (testing "revision must be a natural integer"
    (is
     (not
      (command/versioned-document?
       (assoc
        before
        :widget/revision
        -1)
       version)))

    (is
     (not
      (command/versioned-document?
       (assoc
        before
        :widget/revision
        4.0)
       version))))

  (testing "identity and conventional timestamps must be present and non-nil"
    (doseq [document
            [(dissoc
              before
              :xt/id)

             (assoc
              before
              :xt/id
              nil)

             (dissoc
              before
              :widget/created-at)

             (assoc
              before
              :widget/created-at
              nil)

             (dissoc
              before
              :widget/updated-at)

             (assoc
              before
              :widget/updated-at
              nil)]]
      (is
       (not
        (command/versioned-document?
         document
         version)))))

  (testing "explicit compare fields also become required mechanics"
    (is
     (command/versioned-document?
      before
      custom-version))

    (is
     (not
      (command/versioned-document?
       (dissoc
        before
        :widget/state-token)
       custom-version)))

    (is
     (not
      (command/versioned-document?
       (assoc
        before
        :widget/state-token
        nil)
       custom-version))))

  (testing "require-versioned-document returns the document or throws"
    (is
     (identical?
      before
      (command/require-versioned-document
       before
       version)))

    (is
     (=
      ::command/invalid-versioned-document
      (error-type
       #(command/require-versioned-document
         (dissoc
          before
          :widget/revision)
         version))))))

(deftest initial-and-next-version-test
  (testing "initial documents are exactly revision zero"
    (is
     (command/initial-version?
      initial
      version))

    (is
     (not
      (command/initial-version?
       before
       version))))

  (testing "a conventional update keeps identity and creation time and advances one revision"
    (is
     (command/next-version?
      before
      after
      version))

    (is
     (not
      (command/next-version?
       before
       (assoc
        after
        :widget/revision
        6)
       version)))

    (is
     (not
      (command/next-version?
       before
       (assoc
        after
        :xt/id
        :another-widget)
       version)))

    (is
     (not
      (command/next-version?
       before
       (assoc
        after
        :widget/created-at
        :different-created-at)
       version))))

  (testing "updated-at chronology is deliberately a domain/schema concern"
    (is
     (command/next-version?
      before
      (assoc
       after
       :widget/updated-at
       :some-otherwise-invalid-domain-time)
      version))))

(deftest bump-version-test
  (testing "bump-version changes exactly revision and updated-at"
    (is
     (=
      (assoc
       before
       :widget/revision
       5
       :widget/updated-at
       :t2)
      (command/bump-version
       before
       version
       :t2))))

  (testing "custom compare fields are not manufactured or mutated"
    (is
     (=
      "state-4"
      (:widget/state-token
       (command/bump-version
        before
        custom-version
        :t2)))))

  (testing "nil updated-at is rejected"
    (is
     (=
      ::command/invalid-update-value
      (error-type
       #(command/bump-version
         before
         version
         nil))))))

(deftest expected-version-test
  (testing "default expected version carries identity and concrete compare fields"
    (let [expected
          (command/expected-version
           before
           version)]
      (is
       (=
        {:model/id
         :widget-1

         :model/checks
         [[:widget/revision
           4]

          [:widget/updated-at
           :t1]]}
        expected))

      (is
       (command/expected-version?
        expected))

      (is
       (=
        expected
        (command/require-expected-version
         expected)))))

  (testing "custom compare keys flow directly into expected-version"
    (is
     (=
      {:model/id
       :widget-1

       :model/checks
       [[:widget/revision
         4]

        [:widget/state-token
         "state-4"]]}
      (command/expected-version
       before
       custom-version))))

  (testing "expected-version shape is intentionally strict"
    (doseq [value
            [nil
             {}
             {:model/id :widget-1
              :model/checks []}
             {:model/id nil
              :model/checks [[:widget/revision 4]]}
             {:model/id :widget-1
              :model/checks [[:widget/revision nil]]}
             {:model/id :widget-1
              :model/checks [[:widget/revision 4]
                             [:widget/revision 4]]}
             {:model/id :widget-1
              :model/checks [["revision" 4]]}
             {:model/id :widget-1
              :model/checks [[:widget/revision 4]]
              :extra true}]]
      (is
       (not
        (command/expected-version?
         value))))

    (is
     (=
      ::command/invalid-expected-version
      (error-type
       #(command/require-expected-version
         {:model/id
          :widget-1

          :model/checks
          []}))))))

(deftest guard-test
  (let [expected
        (command/expected-version
         before
         version)

        guard
        (command/guard
         :widget
         before
         version)]
    (testing "guards are generic read dependencies"
      (is
       (=
        {:model/entity-type
         :widget

         :model/expected
         expected}
        guard))

      (is
       (command/guard?
        guard))

      (is
       (=
        guard
        (command/require-guard
         guard)))

      (is
       (=
        [:widget
         :widget-1]
        (command/guard-target
         guard))))

    (testing "guard validation is strict"
      (is
       (not
        (command/guard?
         (assoc
          guard
          :authorization?
          true))))

      (is
       (=
        ::command/invalid-guard
        (error-type
         #(command/require-guard
           (dissoc
            guard
            :model/expected))))))

    (testing "entity type must be a keyword"
      (is
       (=
        ::command/invalid-entity-type
        (error-type
         #(command/guard
           "widget"
           before
           version)))))))

(deftest create-command-test
  (let [model-command
        (command/create
         :widget
         initial
         version)]
    (testing "create captures one initial persisted document"
      (is
       (=
        {:model/entity-type
         :widget

         :model/operation
         :create

         :model/id
         :widget-2

         :model/after
         initial}
        model-command))

      (is
       (command/create?
        model-command))

      (is
       (command/command?
        model-command))

      (is
       (not
        (command/update?
         model-command))))

    (testing "create commands expose common command accessors"
      (is
       (=
        [:widget
         :widget-2]
        (command/target
         model-command)))

      (is
       (=
        :create
        (command/operation
         model-command)))

      (is
       (nil?
        (command/before
         model-command)))

      (is
       (identical?
        initial
        (command/after
         model-command)))

      (is
       (identical?
        initial
        (command/command-document
         model-command))))

    (testing "construction rejects noninitial documents"
      (is
       (=
        ::command/invalid-create-command
        (error-type
         #(command/create
           :widget
           before
           version)))))

    (testing "construction rejects invalid entity types"
      (is
       (=
        ::command/invalid-entity-type
        (error-type
         #(command/create
           "widget"
           initial
           version)))))))

(deftest update-command-test
  (let [expected
        (command/expected-version
         before
         version)

        model-command
        (command/update-command
         :widget
         :rename
         before
         after
         version)]

    (testing "update captures operation, before, after, and the prior expected version"
      (is
       (=
        {:model/entity-type
         :widget

         :model/operation
         :rename

         :model/id
         :widget-1

         :model/expected
         expected

         :model/before
         before

         :model/after
         after}
        model-command))

      (is
       (command/update?
        model-command))

      (is
       (command/command?
        model-command))

      (is
       (not
        (command/create?
         model-command))))

    (testing "common command accessors work for updates"
      (is
       (=
        [:widget
         :widget-1]
        (command/target
         model-command)))

      (is
       (=
        :rename
        (command/operation
         model-command)))

      (is
       (identical?
        before
        (command/before
         model-command)))

      (is
       (identical?
        after
        (command/after
         model-command))))

    (testing "create is reserved for create commands"
      (is
       (=
        ::command/invalid-operation
        (error-type
         #(command/update-command
           :widget
           :create
           before
           after
           version)))))

    (testing "operation must be a keyword"
      (is
       (=
        ::command/invalid-operation
        (error-type
         #(command/update-command
           :widget
           "rename"
           before
           after
           version)))))

    (testing "invalid version progression is rejected"
      (is
       (=
        ::command/invalid-update-command
        (error-type
         #(command/update-command
           :widget
           :rename
           before
           (assoc
            after
            :widget/revision
            6)
           version)))))))

(deftest structural-command-validation-test
  (let [create-command
        (command/create
         :widget
         initial
         version)

        update-command
        (command/update-command
         :widget
         :rename
         before
         after
         version)]

    (testing "canonical command maps are exact representations"
      (is
       (not
        (command/create?
         (assoc
          create-command
          :extra
          true))))

      (is
       (not
        (command/update?
         (assoc
          update-command
          :extra
          true)))))

    (testing "ids must agree throughout command representations"
      (is
       (not
        (command/create?
         (assoc
          create-command
          :model/id
          :other))))

      (is
       (not
        (command/update?
         (assoc-in
          update-command
          [:model/expected
           :model/id]
          :other))))

      (is
       (not
        (command/update?
         (assoc
          update-command
          :model/after
          (assoc
           after
           :xt/id
           :other))))))

    (testing "require-command fails on arbitrary lookalikes"
      (is
       (=
        ::command/invalid-command
        (error-type
         #(command/require-command
           {:model/entity-type
            :widget

            :model/operation
            :create})))))))
