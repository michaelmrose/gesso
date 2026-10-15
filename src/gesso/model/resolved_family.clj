(ns gesso.model.resolved-family
  "Construct a durable production operation and an admitted finite family from
   ONE application-owned domain construction callback.

   Ordinary applications traditionally author resolved-operation/:construct!
   (ctx, binding) and separately author operation-family/:prepare (state,
   principal, arguments). That duplication can make a finite model agree with
   itself while disagreeing with the actual production business construction.

   build-pair forwards the EXACT supplied :construct! function to
   resolved-operation/build-operation and invokes that SAME function in the
   family's :prepare closure. It does not translate domain commands, infer
   guards, invent policy, or create a second interpreter.

   The application STILL owns :enabled? and :model-input!: the latter maps a
   selected finite prestate, principal and arguments to a trusted CONTEXT and
   a canonical command BINDING for the existing construction callback. Every
   finite construction is validated against its declared principal/arguments
   and the same durable incorporation constraints as production before its
   domain plan is handed to the existing formal-family checker.

   This pairing removes independently authored preparation BUSINESS LOGIC;
   it does NOT prove that the model-input mapping is faithful, that the
   production context is authoritative, that the callback is pure or total,
   that enablement expresses complete authorization, or that the resulting
   resolved operation is currently registered as the actual Live executor.
   compare-production-input! optionally checks a caller-supplied production
   context and actual server binding against the SAME model-input! function.
   An optional, CLOSED :context-coverage inventory requires every captured
   production context key to be either modeled or explicitly excluded; new
   fields and drift in the modeled key set fail closed, rather than passing
   with only an extra-field count. Exclusions are declarations, NOT evidence
   that construct! cannot read those fields or that authorization ignores them.
   Optional :required-paths checks declared nested modeled and supplied
   production facts for structural presence, without inferring omitted policy.
   Optional :required-guard-paths connects declared context-held canonical
   version guards to the transaction plan in both formal and actual production
   construction. Optional :required-guard-snapshots independently checks each
   declared guard against a named, modeled acquisition snapshot, including
   document identity and every version comparison field. Optional
   :required-model-state-snapshots checks a selected finite prestate's actual
   current document against that declared snapshot before model construction.
   Optional :required-create-absences checks a declared document target is
   absent in the selected finite prestate and that the resulting finite AND
   production transaction plans contain a matching canonical create. Optional
   :required-protected-snapshot-fields requires authored, observed fields of
   declared snapshots to be represented in their canonical version checks,
   on BOTH finite and production construction paths. This links declared
   policy observations to commit-time compare-and-set protection, without
   discovering omitted dependencies or authenticating acquired values. This is
   a guarded absence of ONE document identity, not a proof of query absence,
   uniqueness across arbitrary fields, or actual production read provenance.
   These compare finite-model objects; they do NOT authenticate production
   acquisition. Only explicitly declared dependencies are checked; it does
   not infer missing policy, prove acquisition truth, or authenticate XTDB.
   This is a strict field comparison, not observation authentication: omitted
   production context semantics and the source of the supplied values remain
   explicit assumptions. Neither construction nor a later TLC verdict
   certifies an application.
   The returned functions and data may be sensitive; keep them server-local."
  (:require
   [clojure.set :as set]
   [gesso.choreo.identity :as identity]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.operation-family :as family]
   [gesso.model.resolved-operation :as resolved]
   [gesso.model.tx-model :as reference]
   [gesso.model.tx :as tx]))

(def configuration-keys
  #{:operation :read-record! :may-disclose? :construct!
    :principals :arguments :enabled? :model-input!})

(def context-coverage-keys #{:modeled :excluded})
(def closed-context-coverage-keys #{:modeled :excluded :modeled-values})
(def required-path-coverage-keys
  #{:modeled :excluded :modeled-values :required-paths})
(def guarded-coverage-keys
  #{:modeled :excluded :modeled-values :required-guard-paths})
(def guarded-path-coverage-keys
  #{:modeled :excluded :modeled-values :required-paths :required-guard-paths})
(def guard-snapshot-coverage-key :required-guard-snapshots)
(def model-state-snapshots-coverage-key :required-model-state-snapshots)
(def create-absences-coverage-key :required-create-absences)
(def protected-snapshot-fields-coverage-key :required-protected-snapshot-fields)
(def ^:private maximum-required-paths 256)
(def closed-model-value-mode :closed-data)

;; A deliberately narrow, opt-in semantic-value language. These are immutable
;; scalar values or fully realized persistent collections, not arbitrary
;; Clojure objects that can hide atoms, functions, lazy computations or DB
;; handles. Inspection is bounded so a malicious/deep input fails closed.
;; This is NOT a sandbox for :construct!, nor a proof that excluded production
;; fields or captured variables cannot affect its behavior.
(def ^:private maximum-closed-nodes 8192)
(def ^:private maximum-closed-depth 32)

(def model-input-keys #{:context :binding})

(def assurance-scope
  {:construction :gesso.model.resolved-family/shared-construction-v1
   :checked #{:same-construct-function-used-in-production-and-family
              :closed-production-and-finite-family-configuration
              :canonical-finite-command-binding
              :finite-binding-operation-principal-argument-correspondence
              :complete-durable-incorporation-construction-validation
              :source-derived-finite-domain-plan-admission
              :optional-modeled-context-field-correspondence
              :optional-canonical-production-binding-comparison
              :optional-closed-production-context-key-inventory
              :declared-modeled-context-key-stability
              :optional-recursive-modeled-value-closure
              :optional-declared-nested-semantic-path-presence
              :optional-construction-guard-to-plan-correspondence
              :optional-guard-to-modeled-snapshot-correspondence
              :optional-snapshot-to-selected-finite-prestate-correspondence
              :optional-declared-create-absence-in-selected-finite-prestate
              :optional-declared-create-absence-to-transaction-correspondence
              :optional-declared-snapshot-field-to-commit-guard-correspondence}
   :assumed #{:truthful-and-complete-finite-choice-domains
              :trusted-state-to-production-context-mapping
              :pure-deterministic-total-domain-construction
              :truthful-state-dependent-enablement-and-authorization
              :correct-registered-production-executor
              :trusted-provenance-of-supplied-production-context-and-binding
              :truthful-declaration-of-excluded-context-field-irrelevance
              :truthful-complete-authored-required-semantic-paths
              :truthful-complete-authored-guard-dependency-paths
              :truthful-guard-snapshot-acquisition-and-identity
              :truthful-selection-of-the-finite-prestate
              :complete-and-truthful-declaration-of-create-absence-dependencies
              :truthful-declaration-of-policy-relevant-snapshot-fields}
   :not-established #{:identity-of-model-context-and-production-context
                      :complete-production-principals-and-arguments
                      :completeness-of-authored-policy-and-guards
                      :purity-of-clojure-callbacks
                      :absence-of-ambient-constructor-dependencies
                      :completeness-of-modeled-production-context-fields
                      :irrelevance-of-declared-excluded-production-fields
                      :completeness-of-application-policy-dependency-declaration
                      :commit-time-enforcement-of-declared-semantic-facts
                      :commit-time-enforcement-of-undeclared-semantic-facts
                      :real-xtdb-enforcement-of-declared-guards
                      :independent-authentication-of-production-input-values
                      :truth-of-acquired-document-snapshots
                      :independent-authentication-of-selected-finite-prestate
                      :correspondence-of-finite-prestate-to-production-xtdb
                      :physical-truth-of-declared-create-absences
                      :query-wide-absence-or-uniqueness
                      :completeness-of-declared-create-absence-dependencies
                      :completeness-of-authored-snapshot-compare-fields
                      :completeness-of-declared-guard-snapshot-bindings
                      :equivalence-of-model-command-id-and-live-command-id
                      :current-live-executor-registration
                      :all-reachable-states-and-repreparation
                      :actual-xtdb-execution-refinement
                      :browser-server-correspondence
                      :application-certification}})

(defn- fail! [kind]
  ;; A failed callback may contain a private state, command ID or principal.
  ;; No application values, exceptions or arbitrary callback ex-data are
  ;; attached to the exception crossing this construction boundary.
  (throw (ex-info
          "The shared durable-operation family cannot be admitted."
          {:error/type ::invalid-resolved-family :error/kind kind})))

(defn- immutable-number? [value]
  (let [type (class value)]
    (or (#{java.lang.Byte java.lang.Short java.lang.Integer java.lang.Long
           java.math.BigInteger java.math.BigDecimal clojure.lang.BigInt
           clojure.lang.Ratio} type)
        (and (= java.lang.Double type)
             (Double/isFinite ^double value))
        (and (= java.lang.Float type)
             (Float/isFinite ^float value)))))

(defn- immutable-scalar? [value]
  (or (nil? value)
      (string? value)
      (keyword? value)
      (symbol? value)
      (instance? Boolean value)
      (instance? Character value)
      (instance? java.util.UUID value)
      (instance? java.time.Instant value)
      (and (number? value) (immutable-number? value))))

(defn- known-persistent-container? [value]
  ;; Restrict to known Clojure collection implementations: a custom
  ;; IPersistentMap/Set/Vector may execute application code during seq/lookup.
  (or (instance? clojure.lang.PersistentArrayMap value)
      (instance? clojure.lang.PersistentHashMap value)
      (instance? clojure.lang.PersistentVector value)
      (instance? clojure.lang.APersistentVector$SubVector value)
      (instance? clojure.lang.PersistentHashSet value)
      (instance? clojure.lang.PersistentList value)
      (= "clojure.lang.PersistentList$EmptyList" (.getName (class value)))))

(defn- closed-data? [value]
  (loop [pending (list [value 0]) remaining maximum-closed-nodes]
    (cond
      (empty? pending) true
      (zero? remaining) false
      :else
      (let [[item depth] (first pending)
            pending (rest pending)]
        (cond
          ;; Metadata is observable to ordinary Clojure and can itself
          ;; contain capabilities even when collection entries are plain.
          (and (instance? clojure.lang.IObj item) (some? (meta item)))
          false

          (immutable-scalar? item)
          (recur pending (dec remaining))

          (>= depth maximum-closed-depth)
          false

          (or (record? item)
              ;; Sorted collections can retain an arbitrary user comparator.
              (instance? clojure.lang.Sorted item))
          false

          (known-persistent-container? item)
          (let [child-count (* (if (map? item) 2 1) (count item))]
            (if (>= child-count remaining)
              false
              (let [children (if (map? item)
                               (mapcat (fn [[key val]] [key val]) item)
                               item)]
                (recur (reduce (fn [acc child]
                                 (conj acc [child (inc depth)]))
                               pending children)
                       (dec remaining)))))

          :else false)))))

(defn- require-closed-modeled-values! [coverage modeled-context]
  (when (and (= closed-model-value-mode (:modeled-values coverage))
             (not (closed-data? modeled-context)))
    ;; Deliberately omit the offending value/path from failure metadata: model
    ;; contexts may include private application documents and credentials.
    (fail! :non-data-modeled-context)))

;; A path describes an explicitly declared observation, not a proof that the
;; developer declared every fact affecting authorization. Present nil is a
;; meaningful observation (for example an explicitly absent owner), while a
;; missing key is NOT the same observation. Restrict paths to keys whose
;; identity is independently inspectable. Finite document maps use the usual
;; [entity-type document-id] map keys. Both sides are already :closed-data,
;; so traversal calls no arbitrary Java/Clojure container behavior. Do not
;; include paths/values in errors.
(defn- semantic-document-id? [value]
  (or (keyword? value) (string? value) (integer? value)
      (instance? java.util.UUID value)))

(defn- semantic-path-key? [value]
  (or (semantic-document-id? value)
      (and (vector? value)
           (= 2 (count value))
           (keyword? (first value))
           (semantic-document-id? (second value)))))

(defn- required-path-present? [context path]
  (loop [current context keys-left path]
    (if-let [key (first keys-left)]
      (and (map? current)
           (contains? current key)
           (recur (get current key) (next keys-left)))
      true)))

(defn- require-semantic-paths! [coverage modeled-context kind]
  (when-let [paths (:required-paths coverage)]
    (when-not (every? #(required-path-present? modeled-context %) paths)
      (fail! kind))))

(declare require-modeled-coverage!)

(defn- require-guard-paths! [coverage context]
  ;; The author supplies canonical guard values in modeled context fields.
  ;; This is a deliberate semantic contract, not a discovery of policy facts.
  (mapv (fn [path]
          (when-not (required-path-present? context path)
            (fail! :missing-required-semantic-guard))
          (let [guard (get-in context path)]
            (when-not (command/guard? guard)
              (fail! :invalid-required-semantic-guard))
            guard))
        (:required-guard-paths coverage)))

(defn- require-guard-snapshots! [coverage context]
  ;; Compare each declared guard with the snapshot that the author says was
  ;; acquired. This is a second relationship beyond guard presence in the
  ;; transaction plan. It prevents a forged/stale version, unrelated document
  ;; or wrong entity type from being substituted for a declared observation.
  ;; No XTDB access happens here: the origin and freshness of the supplied
  ;; snapshot are NOT authenticated by comparing two context values.
  (doseq [[guard-path {:keys [document-path entity-type]}]
          (:required-guard-snapshots coverage)]
    (when-not (required-path-present? context document-path)
      (fail! :missing-required-guard-snapshot))
    (let [guard (get-in context guard-path)
          snapshot (get-in context document-path)
          expected (:model/expected guard)
          checks (:model/checks expected)]
      (when-not (and (command/guard? guard)
                     (= entity-type (:model/entity-type guard))
                     (map? snapshot)
                     (contains? snapshot :xt/id)
                     (= (:xt/id snapshot) (:model/id expected))
                     ;; Check every field, including presence; a missing key
                     ;; cannot agree with an expected nil or a fabricated
                     ;; comparison value.
                     (every? (fn [[field value]]
                               (and (contains? snapshot field)
                                    (= value (get snapshot field))))
                             checks))
        (fail! :required-guard-snapshot-mismatch)))))

;; A guard that accurately describes an acquired snapshot can still omit
;; fields on which the decision depends. For an authored set of such fields,
;; demand explicit presence in the canonical compare-and-set version. This
;; checks both finite and production construction, using the same context
;; structure and before any application constructor executes. It does NOT
;; discover which fields the constructor actually reads, nor does a compare
;; field authenticate the snapshot or establish real XTDB transaction behavior.
(defn- require-protected-snapshot-fields! [coverage context]
  (doseq [[guard-path fields] (:required-protected-snapshot-fields coverage)]
    (let [{:keys [document-path]}
          (get (:required-guard-snapshots coverage) guard-path)
          guard (get-in context guard-path)
          snapshot (get-in context document-path)
          compared (set (map first (get-in guard [:model/expected :model/checks])))]
      ;; Snapshot identity and expected compare values are separately checked
      ;; by require-guard-snapshots!, before reaching this check.
      (when-not (and (map? snapshot)
                     (every? #(contains? snapshot %) fields)
                     (set/subset? fields compared))
        (fail! :required-snapshot-field-not-commit-guarded)))))

(defn- require-model-state-snapshots! [coverage state context]
  ;; The model-input! callback is application-owned. Without this optional
  ;; independent check it could fabricate the acquired snapshot while the
  ;; guard and transaction plan faithfully agree with the fabrication.
  ;; The selected PRESTATE, not the callback output, supplies the document
  ;; checked here. Still no claim of XTDB authenticity or complete policy.
  (when (seq (:required-model-state-snapshots coverage))
    ;; Prevent opaque state objects, effectful equality and malformed finite
    ;; state from influencing this check. Do not relay private documents in
    ;; diagnostics from reference/require-state.
    (when-not (and (closed-data? state)
                   (try
                     (reference/require-state state)
                     true
                     (catch Exception _ false)))
      (fail! :invalid-finite-semantic-prestate))
    (doseq [guard-path (:required-model-state-snapshots coverage)]
      (let [{:keys [document-path entity-type]}
            (get (:required-guard-snapshots coverage) guard-path)
            snapshot (get-in context document-path)
            target [entity-type (:xt/id snapshot)]]
        (when-not (contains? state target)
          (fail! :required-snapshot-missing-from-finite-prestate))
        (when-not (= snapshot (get state target))
          (fail! :required-snapshot-finite-prestate-mismatch))))))

;; A negative observation is not representable as an acquired document
;; snapshot. For the narrow case of "document [type id] does not exist",
;; canonical create already has an atomic absent-document precondition in the
;; Gesso reference model. Check that the modeled input declares the intended
;; target ID and the returned plan actually creates THAT target. Query-wide
;; absence, alternate unique keys and actual XTDB semantics remain unproved.
(defn- require-create-absence-targets! [coverage context]
  (mapv (fn [[id-path entity-type]]
          (when-not (required-path-present? context id-path)
            (fail! :missing-required-create-absence-target))
          (let [id (get-in context id-path)]
            (when-not (semantic-document-id? id)
              (fail! :invalid-required-create-absence-target))
            [entity-type id]))
        (:required-create-absences coverage)))

(defn- require-model-create-absences! [coverage state context]
  (when (seq (:required-create-absences coverage))
    ;; The selected prestate is the independent finite reference; do not
    ;; ask the application-owned model-input! callback whether it is absent.
    (when-not (and (closed-data? state)
                   (try (reference/require-state state)
                        true
                        (catch Exception _ false)))
      (fail! :invalid-finite-semantic-prestate))
    (doseq [target (require-create-absence-targets! coverage context)]
      (when (contains? state target)
        (fail! :required-create-target-not-absent-in-finite-prestate)))))

(defn- guarded-plan! [coverage context plan]
  (when (or (seq (:required-guard-paths coverage))
            (seq (:required-create-absences coverage)))
    ;; Admit the same supported transaction subset as the finite reference.
    ;; Raw SQL and opaque entry hooks must not stand in for a declared guard.
    (let [plan (try
                 (reference/admitted-plan plan)
                 (catch Exception _
                   (fail! :unsupported-guarded-domain-plan)))
          guards (require-guard-paths! coverage context)
          effective (try
                      (tx/effective-guards (:commands plan) (:guards plan))
                      (catch Exception _
                        (fail! :invalid-guarded-domain-plan)))
          updates (filter command/update? (:commands plan))]
      (when-not (every? (fn [guard]
                          (or (some #(= guard %) effective)
                              ;; Effective guards omit redundant protection
                              ;; supplied by the corresponding update command.
                              (some (fn [update]
                                      (and (= (command/target update)
                                              (command/guard-target guard))
                                           (= (:model/expected update)
                                              (:model/expected guard))))
                                    updates)))
                        guards)
        (fail! :missing-required-commit-guard))
      (when-not (every? (fn [target]
                          (some (fn [model-command]
                                  (and (command/create? model-command)
                                       (= target (command/target model-command))))
                                (:commands plan)))
                        (require-create-absence-targets! coverage context))
        (fail! :missing-required-absence-protecting-create)))))

(defn- checked-domain-constructor [coverage construct!]
  ;; This SAME wrapper is installed in production and finite family paths.
  ;; It does not infer dependencies, authenticate reads, or make the
  ;; application-owned callback pure. Opted-out applications stay unchanged.
  (if-not (or (seq (:required-guard-paths coverage))
              (seq (:required-create-absences coverage)))
    construct!
    (fn [context binding]
      (when-not (map? context)
        (fail! :invalid-production-context))
      ;; Production ctx may contain excluded capabilities. Only the modeled
      ;; portion has the closed-data requirement.
      (require-modeled-coverage! coverage
                                 (select-keys context (:modeled coverage)))
      (require-closed-modeled-values!
       coverage (select-keys context (:modeled coverage)))
      (require-semantic-paths! coverage context
                               :missing-required-production-semantic-input)
      (require-guard-paths! coverage context)
      (require-guard-snapshots! coverage context)
      (require-protected-snapshot-fields! coverage context)
      (require-create-absence-targets! coverage context)
      (let [built (construct! context binding)]
        (when (and (map? built) (contains? built :plan))
          (guarded-plan! coverage context (:plan built)))
        ;; A malformed construction remains subject to the ordinary
        ;; resolved-operation validation, never silently authorized here.
        built))))

(defn- call-model! [kind f & arguments]
  (try
    (apply f arguments)
    (catch Exception _
      (fail! kind))))

(defn- require-context-coverage! [coverage]
  (when-not (and (map? coverage)
                 (let [declared-keys (apply disj (set (keys coverage))
                                            [guard-snapshot-coverage-key
                                             model-state-snapshots-coverage-key
                                             create-absences-coverage-key
                                             protected-snapshot-fields-coverage-key])]
                   (or (= context-coverage-keys declared-keys)
                       (= closed-context-coverage-keys declared-keys)
                       (= required-path-coverage-keys declared-keys)
                       (= guarded-coverage-keys declared-keys)
                       (= guarded-path-coverage-keys declared-keys)))
                 (set? (:modeled coverage))
                 (seq (:modeled coverage))
                 (every? keyword? (:modeled coverage))
                 (set? (:excluded coverage))
                 (every? keyword? (:excluded coverage))
                 (empty? (set/intersection (:modeled coverage)
                                           (:excluded coverage)))
                 (or (not (contains? coverage :modeled-values))
                     (= closed-model-value-mode (:modeled-values coverage)))
                 (or (not (contains? coverage :required-paths))
                     (and (= closed-model-value-mode (:modeled-values coverage))
                          (set? (:required-paths coverage))
                          (seq (:required-paths coverage))
                          (<= (count (:required-paths coverage)) maximum-required-paths)
                          (every? (fn [path]
                                    (and (vector? path)
                                         (<= 1 (count path) maximum-closed-depth)
                                         (every? semantic-path-key? path)
                                         (contains? (:modeled coverage) (first path))))
                                  (:required-paths coverage))))
                 (or (not (contains? coverage :required-guard-paths))
                     (and (= closed-model-value-mode (:modeled-values coverage))
                          (set? (:required-guard-paths coverage))
                          (seq (:required-guard-paths coverage))
                          (<= (count (:required-guard-paths coverage))
                              maximum-required-paths)
                          (every? (fn [path]
                                    (and (vector? path)
                                         (<= 1 (count path) maximum-closed-depth)
                                         (every? semantic-path-key? path)
                                         (contains? (:modeled coverage)
                                                    (first path))))
                                  (:required-guard-paths coverage))))
                 (or (not (contains? coverage guard-snapshot-coverage-key))
                     (and (= closed-model-value-mode (:modeled-values coverage))
                          (map? (:required-guard-snapshots coverage))
                          (seq (:required-guard-snapshots coverage))
                          (<= (count (:required-guard-snapshots coverage))
                              maximum-required-paths)
                          (every?
                           (fn [[guard-path descriptor]]
                             (and (contains? (:required-guard-paths coverage)
                                             guard-path)
                                  (map? descriptor)
                                  (= #{:document-path :entity-type}
                                     (set (keys descriptor)))
                                  (keyword? (:entity-type descriptor))
                                  (let [path (:document-path descriptor)]
                                    (and (vector? path)
                                         (<= 1 (count path) maximum-closed-depth)
                                         (every? semantic-path-key? path)
                                         (contains? (:modeled coverage)
                                                    (first path))))))
                           (:required-guard-snapshots coverage))))
                 (or (not (contains? coverage model-state-snapshots-coverage-key))
                     (and (= closed-model-value-mode (:modeled-values coverage))
                          (set? (:required-model-state-snapshots coverage))
                          (seq (:required-model-state-snapshots coverage))
                          (<= (count (:required-model-state-snapshots coverage))
                              maximum-required-paths)
                          (every? #(contains? (:required-guard-snapshots coverage) %)
                                  (:required-model-state-snapshots coverage))))
                 (or (not (contains? coverage create-absences-coverage-key))
                     (and (= closed-model-value-mode (:modeled-values coverage))
                          (map? (:required-create-absences coverage))
                          (seq (:required-create-absences coverage))
                          (<= (count (:required-create-absences coverage))
                              maximum-required-paths)
                          (every? (fn [[path entity-type]]
                                    (and (vector? path)
                                         (<= 1 (count path) maximum-closed-depth)
                                         (every? semantic-path-key? path)
                                         (contains? (:modeled coverage) (first path))
                                         (qualified-keyword? entity-type)))
                                  (:required-create-absences coverage))))
                 (or (not (contains? coverage protected-snapshot-fields-coverage-key))
                     (and (= closed-model-value-mode (:modeled-values coverage))
                          (map? (:required-protected-snapshot-fields coverage))
                          (seq (:required-protected-snapshot-fields coverage))
                          (<= (count (:required-protected-snapshot-fields coverage))
                              maximum-required-paths)
                          (every? (fn [[guard-path fields]]
                                    (and (contains? (:required-guard-snapshots coverage)
                                                    guard-path)
                                         (set? fields)
                                         (seq fields)
                                         (<= (count fields) maximum-required-paths)
                                         (every? keyword? fields)))
                                  (:required-protected-snapshot-fields coverage))
                          (<= (reduce + 0 (map count
                                               (vals (:required-protected-snapshot-fields coverage))))
                              maximum-required-paths))))
    (fail! :invalid-context-coverage))
  coverage)

(defn- require-modeled-coverage! [coverage modeled-context]
  ;; Validate at EVERY model-input! invocation, not only at build time.
  ;; The callback may be mutable or state-dependent.
  (when (and coverage
             (not= (:modeled coverage) (set (keys modeled-context))))
    (fail! :modeled-context-key-inventory-drift)))

(defn- matching-principal? [declared bound]
  (and (identity/principal? bound)
       (or (= declared bound)
           (= declared (identity/raw-value bound)))))

(defn- finite-input! [operation model-input! coverage state principal arguments]
  (let [input (call-model! :model-input-exception
                           model-input! state principal arguments)]
    (when-not (and (map? input)
                   (= model-input-keys (set (keys input)))
                   (map? (:context input)))
      (fail! :invalid-model-input))
    (require-modeled-coverage! coverage (:context input))
    (require-closed-modeled-values! coverage (:context input))
    (require-semantic-paths! coverage (:context input)
                             :missing-required-modeled-semantic-input)
    (require-guard-paths! coverage (:context input))
    (require-guard-snapshots! coverage (:context input))
    (require-protected-snapshot-fields! coverage (:context input))
    (require-model-state-snapshots! coverage state (:context input))
    (require-model-create-absences! coverage state (:context input))
    (let [binding (:binding input)]
      (try
        (resolution/require-binding binding)
        (catch Exception _
          (fail! :invalid-model-binding)))
      (when-not (and (= operation (:operation binding))
                     (= arguments (:arguments binding))
                     (matching-principal? principal (:principal binding)))
        (fail! :model-binding-choice-mismatch)))
    input))

(defn- compare-production-input!
  [operation model-input! coverage state principal arguments production-context production-binding]
  ;; This is an explicitly supplied trusted-context witness, NOT an
  ;; authenticated observation. Actual executor provenance must be separately
  ;; established before using any result in application assurance.
  (when-not (map? production-context)
    (fail! :invalid-production-context))
  (try
    (resolution/require-binding production-binding)
    (catch Exception _
      (fail! :invalid-production-binding)))
  (let [{modeled-context :context modeled-binding :binding}
        (finite-input! operation model-input! coverage state principal arguments)]
    (when-not (= (dissoc modeled-binding :command-id)
                 (dissoc production-binding :command-id))
      (fail! :production-binding-differs-from-model-input))
    ;; Comparing an empty context would certify nothing about the context.
    ;; Compare every modeled key, including present-nil versus absent keys;
    ;; never silently select only the keys that happen to agree.
    (when-not (seq modeled-context)
      (fail! :empty-modeled-context-comparison))
    ;; Never compare an admitted closed value against an opaque producer-owned
    ;; value whose equals/hash implementation could itself be effectful.
    ;; Excluded context fields remain outside this opt-in guarantee.
    (when (= closed-model-value-mode (:modeled-values coverage))
      (require-closed-modeled-values!
       coverage (select-keys production-context (:modeled coverage))))
    (require-semantic-paths! coverage production-context
                             :missing-required-production-semantic-input)
    (require-guard-paths! coverage production-context)
    (require-guard-snapshots! coverage production-context)
    (require-protected-snapshot-fields! coverage production-context)
    (require-create-absence-targets! coverage production-context)
    (when-not (every? (fn [[key value]]
                        (and (contains? production-context key)
                             (= value (get production-context key))))
                      modeled-context)
      (fail! :production-context-differs-from-model-input))
    (when (and coverage
               (not= (set (keys production-context))
                     (set/union (:modeled coverage) (:excluded coverage))))
      ;; Refuse both undeclared new fields and absent excluded fields. Merely
      ;; counting unknown keys is NOT enough for a closed inventory claim.
      (fail! :production-context-key-inventory-differs))
    {:status :matched-declared-input-fields
     :context-fields-compared (count modeled-context)
     :context-key-inventory (if coverage
                              :closed-against-authored-exclusions
                              :not-declared)
     :explicitly-excluded-context-fields (if coverage
                                           (count (:excluded coverage)) 0)
     :exclusion-relevance :not-established
     :modeled-value-closure (if (= closed-model-value-mode
                                   (:modeled-values coverage))
                              :checked-closed-data
                              :not-checked)
     :constructor-purity :not-established
     :required-semantic-path-presence
     (if (contains? coverage :required-paths) :checked-declared-paths :not-declared)
     :required-semantic-paths-checked
     (count (:required-paths coverage))
     :required-paths-completeness :not-established
     :declared-semantic-guards-checked (count (:required-guard-paths coverage))
     :declared-guard-snapshots-checked (count (:required-guard-snapshots coverage))
     :declared-protected-snapshot-fields-checked
     (reduce + 0 (map count (vals (:required-protected-snapshot-fields coverage))))
     :protected-snapshot-fields-completeness :not-established
     :guard-snapshot-origin
     (if (seq (:required-guard-snapshots coverage))
       :caller-supplied-not-authenticated
       :not-declared)
     :declared-finite-prestate-snapshots-checked
     (count (:required-model-state-snapshots coverage))
     :declared-create-absences-checked
     (count (:required-create-absences coverage))
     :create-absence-physical-origin :not-authenticated
     :finite-prestate-origin
     (if (seq (:required-model-state-snapshots coverage))
       :caller-supplied-not-authenticated
       :not-declared)
     :semantic-guard-commit-enforcement-in-this-comparison :not-checked
     :semantic-guard-dependency-completeness :not-established
     :unmodeled-production-context-fields
     (count (remove #(contains? modeled-context %) (keys production-context)))
     :command-id-comparison :excluded-independent-execution-identity
     :input-origin :caller-supplied-not-authenticated
     :application-certification :not-established}))

(defn- finite-plan! [operation model-input! coverage construct! state principal arguments]
  (let [{:keys [context binding]}
        (finite-input! operation model-input! coverage state principal arguments)
        built (call-model! :domain-construction-exception
                           construct! context binding)]
    (when-not (and (map? built)
                   (= resolved/construction-keys (set (keys built))))
      (fail! :invalid-domain-construction))
    ;; Production attaches the durable command result record to this
    ;; EXACT domain construction. Enforce its existing structural limits
    ;; and reserved-table prohibition without invoking any transaction.
    (try
      (resolution/incorporation-plan
       {:binding binding :now (:now built) :outcome (:outcome built)
        :plan (:plan built)})
      (catch Exception _
        (fail! :invalid-durable-incorporation)))
    ;; Admit the ORIGINAL application domain plan. Do not silently project
    ;; away guards, commands or unsupported transaction entries.
    (guarded-plan! coverage context (:plan built))
    (try
      (reference/admitted-plan (:plan built))
      (catch Exception _
        (fail! :unsupported-formal-domain-plan)))))

(defn build-pair
  "Build {:resolved-operation ..., :family ..., :scope ...} from one :construct!.

   The returned :resolved-operation is the ordinary callable production
   durable-command operation and must be explicitly supplied to the ordinary
   Live adapter and registered by the application. :family is a standard
   operation-family declaration suitable for formal-family/prepared-competition
   or state-dependent invariant checking. Both use the SAME lexical :construct!
   object, never a separately authored :prepare implementation.

   :model-input! receives [state principal arguments] and returns exactly
   {:context trusted-model-context :binding canonical-command-binding}.
   It is trusted application code and may NOT be assumed to reproduce the
   actual production context. In the finite family, the callback and its
   output are local diagnostics; Gesso invokes no XTDB transaction here.

   :compare-production-input! receives [state principal arguments
   production-context production-binding] and checks that EVERY modeled
   context field is present with the same value in the supplied production
   context, plus semantic binding equality excluding only the command ID.
   Empty modeled contexts fail closed. Unmodeled extra production fields
   are counted, not declared equivalent. It never invokes :construct! or
   authenticates the supplied context; use only at a trusted server boundary.

   Optional :context-coverage {:modeled #{...} :excluded #{...}} enables a
   strict inventory. The :modeled set must be nonempty, and must equal the
   model-input! context keys on EVERY invocation, including formal admission.
   On comparison, the captured production context must contain exactly those
   modeled keys and the explicitly declared excluded keys. An undeclared new
   field or missing declared field fails closed. Excluded values are NOT
   compared; their irrelevance to construct! or authorization is NOT proven.
   Add :modeled-values :closed-data to the context-coverage map to reject
   nested functions, atoms, mutable Java objects, lazy sequences, metadata,
   user comparators, and other opaque context values before :construct! is
   called. Modeled fields in the supplied production context are also checked
   before equality comparison. Only immutable EDN-like
   scalars, UUIDs, Instants and bounded persistent collections are admitted.
   Add :required-paths #{[:record :version] [:eligibility :active?]} to
   :closed-data coverage to require declared nested facts. Segments may be
   keywords, immutable document IDs, or [entity-type document-id] keys for
   finite state maps. Paths must begin with modeled top-level keys and be present in both model
   and supplied production contexts; a present nil is allowed, but a missing
   key is rejected before construction. Paths are bounded; no application
   paths or data are included in errors. This only checks AUTHORED path
   presence; it cannot infer omitted policy dependencies or ensure commit-time
   guards enforce those dependencies. It does not prove callback purity,
   dependency completeness, or excluded context-field irrelevance. Without
   the coverage declaration, legacy conditional comparison is retained.
   Add :required-guard-snapshots
   {[:eligibility :guard] {:document-path [:eligibility :document]
                          :entity-type :fixture/eligibility}}
   to bind an explicitly authored guard to a modeled document. Every expected
   version check must agree with the corresponding acquired document field and
   the guard's entity type must match. The snapshot must be present on both
   construction sides. This never authenticates the snapshot's production
   origin, establishes policy completeness, or proves XTDB behavior.
   Add :required-model-state-snapshots #{[:eligibility :guard]} when that
   guard is already listed in :required-guard-snapshots. On every finite
   model-input! invocation, Gesso compares the authored snapshot to the
   actual current document at [entity-type document-id] in the SELECTED
   finite prestate, using complete document equality (not merely version
   fields). This rejects fabricated model-input snapshots and is also
   checked when comparing supplied production input to the finite model.
   It does not compare the production constructor to XTDB and cannot prove
   that the finite prestate corresponds to any actual production snapshot.
   Add :required-protected-snapshot-fields
   {[:eligibility :guard] #{:eligibility/active?}} when that guard is already
   listed under :required-guard-snapshots. Every authored field must be present
   in that acquired snapshot AND in the guard's canonical expected-version
   checks; the guarded transaction plan must also contain the guard or an
   equivalent update precondition. This is checked before domain construction
   on both finite and production paths. Field completeness and the physical
   provenance of snapshots remain unproved. The declaration is bounded to
   256 field entries overall, and missing or empty declarations fail closed.
   Add :required-create-absences {[:allocation :target-id] :fixture/lease}
   to require that the target ID is absent in each selected finite prestate
   AND a canonical create for precisely [:fixture/lease target-id] appears in
   both the finite and production construction plans. This checks a narrow
   document-identity absence, not a database query's zero-result assertion,
   an arbitrary uniqueness constraint, or actual XTDB enforcement. Production
   context origin and any omitted application dependencies remain trusted.
   Add :required-guard-paths #{[:eligibility :guard]} to :closed-data coverage
   when that nested modeled value is a canonical gesso.model.command/guard.
   Every declared guard must be represented in the returned transaction plan,
   either as a dependency guard or as an equivalent update-command precondition.
   This check runs in BOTH production and finite construction using the SAME
   wrapped callback. It cannot infer omitted dependencies, prove the acquired
   guard matches physical database truth, or show XTDB refinement."
  [configuration]
  (when-not (and (map? configuration)
                 (or (= configuration-keys (set (keys configuration)))
                     (= (conj configuration-keys :context-coverage)
                        (set (keys configuration))))
                 (qualified-keyword? (:operation configuration))
                 (fn? (:enabled? configuration))
                 (fn? (:model-input! configuration))
                 (fn? (:construct! configuration)))
    (fail! :invalid-configuration))
  (let [coverage (when (contains? configuration :context-coverage)
                   (require-context-coverage! (:context-coverage configuration)))
        {:keys [operation principals arguments enabled? model-input! construct!]}
        configuration
        guarded-construct! (checked-domain-constructor coverage construct!)
        production (try
                     (resolved/build-operation
                      (assoc (select-keys configuration resolved/configuration-keys)
                             :construct! guarded-construct!))
                     (catch Exception _ (fail! :invalid-production-operation)))
        operation-family {:id operation
                          :principals principals
                          :arguments arguments
                          :enabled? enabled?
                          :prepare (fn [state principal args]
                                     (finite-plan! operation model-input! coverage
                                                   guarded-construct! state principal args))}]
    ;; Validate the entire finite family NOW, before returning either surface.
    ;; In particular, do not return a production function paired with a
    ;; truncated or duplicate choice declaration.
    (try
      (family/expand-families [operation-family])
      (catch Exception _
        (fail! :invalid-finite-domain)))
    {:kind :gesso.model.resolved-family/shared-construction-v1
     :operation operation
     :resolved-operation production
     :family operation-family
     :compare-production-input!
     (fn [state principal args production-context production-binding]
       (compare-production-input! operation model-input! coverage
                                  state principal args
                                  production-context production-binding))
     :scope assurance-scope
     :application-certification :not-established}))
