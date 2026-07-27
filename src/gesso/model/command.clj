(ns gesso.model.command
  "Pure command and optimistic-version descriptions for gesso.model.

   This namespace is the common representation shared by generated model
   operations and hand-written complex Gesso FX workflows.

   A domain transition still owns semantics. It produces a new valid document.
   gesso.model.command only describes the persistence consequence of that
   transition.

   Create command:

     {:model/entity-type :membership
      :model/operation   :create
      :model/id          membership-id
      :model/after       membership}

   Update command:

     {:model/entity-type :membership
      :model/operation   :suspend
      :model/id          membership-id
      :model/expected    {:model/id membership-id
                          :model/checks
                          [[:membership/revision 3]
                           [:membership/updated-at t0]]}
      :model/before      before
      :model/after       after}

   A version guard has the same expected-version representation but does not
   itself mutate the guarded document:

     {:model/entity-type :location
      :model/expected    {...}}

   Applications may use guards for authorization proofs, hierarchy snapshots,
   uniqueness decisions, or any other read dependency. Gesso deliberately does
   not attach authorization semantics to them.

   Structural field types belong to Malli schemas. This namespace therefore
   does not require UUID ids or java.time.Instant timestamps. It checks only
   the generic version mechanics that a transaction engine needs."
)

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
;; Version metadata
;; =============================================================================

(def version-keys
  #{:revision-key
    :created-at-key
    :updated-at-key
    :compare-keys})

(defn valid-version?
  "Returns true when version describes the conventional revision mechanics.

   Required:
     :revision-key
     :created-at-key
     :updated-at-key

   Optional:
     :compare-keys

   compare-keys defaults to [revision-key updated-at-key]. It controls which
   persisted values participate in optimistic concurrency. The three standard
   version keys must remain distinct even when compare-keys is customized."
  [{:keys [revision-key
           created-at-key
           updated-at-key
           compare-keys]
    :as version}]
  (and
   (map? version)

   (every? version-keys
           (keys version))

   (keyword? revision-key)
   (keyword? created-at-key)
   (keyword? updated-at-key)

   (=
    3
    (count
     (set
      [revision-key
       created-at-key
       updated-at-key])))

   (or
    (nil? compare-keys)

    (and
     (sequential? compare-keys)
     (seq compare-keys)
     (every? keyword? compare-keys)
     (=
      (count compare-keys)
      (count (distinct compare-keys)))))))

(defn require-version
  "Returns version or throws when it does not satisfy valid-version?."
  [version]
  (when-not
   (valid-version? version)
    (fail!
     ::invalid-version
     "Model version metadata is invalid."
     {:version version}))
  version)

(defn compare-keys
  "Returns the optimistic-concurrency fields for version.

   The default is revision plus updated-at."
  [{:keys [revision-key
           updated-at-key
           compare-keys]
    :as version}]
  (require-version version)
  (vec
   (or compare-keys
       [revision-key
        updated-at-key])))

;; =============================================================================
;; Conventional versioned documents
;; =============================================================================

(defn versioned-document?
  "Returns true when document satisfies the generic version mechanics.

   This deliberately does not duplicate Malli field types. In particular, the
   document id and timestamps may use any representation accepted by the
   application's document schema.

   The generic requirements are only:

   - document is a map
   - :xt/id is present and non-nil
   - revision is a natural integer
   - created-at and updated-at fields are present and non-nil
   - every optimistic compare field is present and non-nil"
  [document
   {:keys [revision-key
           created-at-key
           updated-at-key]
    :as version}]
  (and
   (map? document)

   (valid-version? version)

   (contains? document :xt/id)
   (some? (:xt/id document))

   (contains? document revision-key)
   (nat-int? (get document revision-key))

   (contains? document created-at-key)
   (some? (get document created-at-key))

   (contains? document updated-at-key)
   (some? (get document updated-at-key))

   (every?
    (fn [key]
      (and
       (contains? document key)
       (some? (get document key))))
    (compare-keys version))))

(defn require-versioned-document
  "Returns document or throws when generic version mechanics are invalid."
  [document version]
  (when-not
   (versioned-document?
    document
    version)
    (fail!
     ::invalid-versioned-document
     "Document does not satisfy the generic model version contract."
     {:model/id (:xt/id document)
      :version version
      :document document}))
  document)

(defn initial-version?
  "Returns true when document is a valid versioned document at revision zero."
  [document
   {:keys [revision-key]
    :as version}]
  (and
   (versioned-document? document version)
   (zero? (get document revision-key))))

(defn next-version?
  "Returns true when after is the conventional next revision of before.

   This checks only mechanics that are representation-independent:

   - both documents satisfy version metadata
   - :xt/id is unchanged
   - created-at is unchanged
   - revision increases by exactly one

   Domain/schema code remains responsible for timestamp type and chronological
   validity. That avoids duplicating application scalar semantics in Gesso."
  [before
   after
   {:keys [revision-key
           created-at-key]
    :as version}]
  (and
   (versioned-document? before version)
   (versioned-document? after version)

   (=
    (:xt/id before)
    (:xt/id after))

   (=
    (get before created-at-key)
    (get after created-at-key))

   (=
    (inc (get before revision-key))
    (get after revision-key))))

(defn bump-version
  "Returns document with revision incremented and updated-at set to now.

   This is a convenience for pure domain code. It validates the incoming
   document's generic version shape but deliberately does not validate the
   resulting document against an application schema."
  [document
   {:keys [revision-key
           updated-at-key]
    :as version}
   now]
  (require-versioned-document document version)

  (when
   (nil? now)
    (fail!
     ::invalid-update-value
     "A non-nil updated-at value is required."
     {:model/id (:xt/id document)
      :updated-at-key updated-at-key}))

  (-> document
      (update revision-key inc)
      (assoc updated-at-key now)))

;; =============================================================================
;; Expected versions
;; =============================================================================

(def expected-version-keys
  #{:model/id
    :model/checks})

(defn expected-version
  "Returns the compare-and-set snapshot for document.

   Attribute names travel with their values so gesso.model.tx can build an
   assertion without knowing entity-specific version field names."
  [document version]
  (require-versioned-document document version)

  {:model/id
   (:xt/id document)

   :model/checks
   (mapv
    (fn [key]
      [key
       (get document key)])
    (compare-keys version))})

(defn expected-version?
  "Returns true for the canonical expected-version shape."
  [value]
  (and
   (map? value)

   (=
    expected-version-keys
    (set (keys value)))

   (some?
    (:model/id value))

   (vector?
    (:model/checks value))

   (seq
    (:model/checks value))

   (every?
    (fn [check]
      (and
       (vector? check)
       (= 2 (count check))
       (keyword? (first check))
       (some? (second check))))
    (:model/checks value))

   (=
    (count (:model/checks value))
    (count
     (distinct
      (map first
           (:model/checks value)))))))

(defn require-expected-version
  "Returns expected or throws."
  [expected]
  (when-not
   (expected-version? expected)
    (fail!
     ::invalid-expected-version
     "Expected-version metadata is invalid."
     {:expected expected}))
  expected)

;; =============================================================================
;; Version guards
;; =============================================================================

(def guard-keys
  #{:model/entity-type
    :model/expected})

(defn guard
  "Captures one read dependency that must still be current at commit time.

   Gesso does not interpret why the caller depends on the document."
  [entity-type document version]
  (when-not
   (keyword? entity-type)
    (fail!
     ::invalid-entity-type
     "A model entity type must be a keyword."
     {:entity-type entity-type}))

  {:model/entity-type
   entity-type

   :model/expected
   (expected-version
    document
    version)})

(defn guard?
  "Returns true for the canonical generic version-guard shape."
  [value]
  (and
   (map? value)

   (=
    guard-keys
    (set (keys value)))

   (keyword?
    (:model/entity-type value))

   (expected-version?
    (:model/expected value))))

(defn require-guard
  "Returns value or throws when it is not a valid version guard."
  [value]
  (when-not
   (guard? value)
    (fail!
     ::invalid-guard
     "Model version guard is invalid."
     {:guard value}))
  value)

(defn guard-target
  "Returns [entity-type document-id] for one guard."
  [value]
  (let [{:model/keys [entity-type expected]}
        (require-guard value)]
    [entity-type
     (:model/id expected)]))

;; =============================================================================
;; Commands
;; =============================================================================

(def create-command-keys
  #{:model/entity-type
    :model/operation
    :model/id
    :model/after})

(def update-command-keys
  #{:model/entity-type
    :model/operation
    :model/id
    :model/expected
    :model/before
    :model/after})

(defn create
  "Describes creation of one conventional persisted document.

   The document must satisfy version and begin at revision zero. Application
   schema/domain validation should already have established all entity-specific
   invariants."
  [entity-type document version]
  (when-not
   (keyword? entity-type)
    (fail!
     ::invalid-entity-type
     "A model command requires a keyword entity type."
     {:entity-type entity-type}))

  (when-not
   (initial-version?
    document
    version)
    (fail!
     ::invalid-create-command
     "Cannot create a command from an invalid initial versioned document."
     {:entity-type entity-type
      :model/id (:xt/id document)
      :version version}))

  {:model/entity-type
   entity-type

   :model/operation
   :create

   :model/id
   (:xt/id document)

   :model/after
   document})

(defn update-command
  "Describes one version-checked update.

   before and after must form a conventional one-revision progression.
   operation is application-owned and may be any keyword except :create."
  [entity-type
   operation
   before
   after
   version]
  (when-not
   (keyword? entity-type)
    (fail!
     ::invalid-entity-type
     "A model command requires a keyword entity type."
     {:entity-type entity-type}))

  (when-not
   (and
    (keyword? operation)
    (not= :create operation))
    (fail!
     ::invalid-operation
     "An update command requires a non-create keyword operation."
     {:entity-type entity-type
      :operation operation}))

  (when-not
   (next-version?
    before
    after
    version)
    (fail!
     ::invalid-update-command
     "Before and after do not describe one valid model revision."
     {:entity-type entity-type
      :operation operation
      :model/id (:xt/id before)
      :version version}))

  {:model/entity-type
   entity-type

   :model/operation
   operation

   :model/id
   (:xt/id before)

   :model/expected
   (expected-version
    before
    version)

   :model/before
   before

   :model/after
   after})

(defn create?
  "Returns true when command is structurally a canonical create command."
  [command]
  (and
   (map? command)

   (=
    create-command-keys
    (set (keys command)))

   (keyword?
    (:model/entity-type command))

   (=
    :create
    (:model/operation command))

   (some?
    (:model/id command))

   (map?
    (:model/after command))

   (=
    (:model/id command)
    (:xt/id (:model/after command)))))

(defn update?
  "Returns true when command is structurally a canonical update command."
  [command]
  (and
   (map? command)

   (=
    update-command-keys
    (set (keys command)))

   (keyword?
    (:model/entity-type command))

   (keyword?
    (:model/operation command))

   (not=
    :create
    (:model/operation command))

   (some?
    (:model/id command))

   (expected-version?
    (:model/expected command))

   (=
    (:model/id command)
    (get-in command
            [:model/expected
             :model/id]))

   (map?
    (:model/before command))

   (map?
    (:model/after command))

   (=
    (:model/id command)
    (:xt/id (:model/before command))
    (:xt/id (:model/after command)))))

(defn command?
  "Returns true for either canonical command shape."
  [value]
  (or
   (create? value)
   (update? value)))

(defn require-command
  "Returns command or throws when its canonical shape is invalid."
  [command]
  (when-not
   (command? command)
    (fail!
     ::invalid-command
     "Model command is invalid."
     {:command command}))
  command)

(defn target
  "Returns [entity-type document-id] for command."
  [command]
  (let [{:model/keys [entity-type id]}
        (require-command command)]
    [entity-type id]))

(defn operation
  "Returns the application operation keyword for command."
  [command]
  (:model/operation
   (require-command command)))

(defn before
  "Returns the pre-mutation document, or nil for create commands."
  [command]
  (:model/before
   (require-command command)))

(defn after
  "Returns the resulting persisted document."
  [command]
  (:model/after
   (require-command command)))

(def command-document
  "Compatibility-friendly alias for after."
  after)
