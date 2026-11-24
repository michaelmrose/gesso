(ns gesso.live.outbox.model-publication
  "Selected durable subject-to-model correspondence. No arbitrary change
   payload, predicate evaluation at replay, or implicit identity coercion."
  (:require [clojure.string :as str]
            [gesso.live.application-preflight :as application]
            [gesso.live.model :as model]
            [gesso.choreo.identity :as identity]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.publication-manifest :as manifest]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.optimistic.server :as server]
            [gesso.live.optimistic.resolved-operation :as adapter]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.tx :as model-tx]
            [gesso.model.command :as model-command])
  (:import [java.io ByteArrayOutputStream DataOutputStream]
           [java.nio ByteBuffer]
           [java.security MessageDigest]
           [java.util UUID WeakHashMap]
           [java.lang.ref WeakReference]))

(defn- reject! [reason data]
  (throw (ex-info "Durable model publication not admitted."
                  (assoc data :error/type :gesso.live.outbox/invalid-model-publication
                         :reason reason))))

(defn- portable-subject? [id]
  (or (and (string? id) (not (str/blank? id)))
      (keyword? id) (integer? id) (uuid? id)))

(defn- identity-parts [value]
  (cond
    (string? value) ["string" value]
    (keyword? value) ["keyword" (if (namespace value) "qualified" "unqualified")
                      (or (namespace value) "") (name value)]
    (integer? value) ["integer" (str value)]
    (uuid? value) ["uuid" (str value)]
    :else (reject! :unsupported-derived-identity {})))

(defn- derived-intent-id [source operation principal command-id slot]
  ;; Frozen v1 encoding: count each typed component's UTF-16 code units,
  ;; followed by those units in network order. No printer or locale settings
  ;; can affect the identity. SHA-256 truncated into an RFC 9562 UUIDv8.
  (let [buffer (ByteArrayOutputStream.)
        fields (into ["gesso.live.outbox/command-intent-v1"]
                     (mapcat identity-parts)
                     [source operation principal command-id slot])]
    (with-open [out (DataOutputStream. buffer)]
      (doseq [^String field fields]
        (.writeInt out (.length field))
        (.writeChars out field)))
    (let [digest (ByteBuffer/wrap (.digest (MessageDigest/getInstance "SHA-256")
                                          (.toByteArray buffer)))
          high (bit-or (bit-and (.getLong digest) -61441) 32768)
          low (bit-or (bit-and (.getLong digest) 4611686018427387903) Long/MIN_VALUE)]
      (UUID. high low))))

(defn- require-derived-command! [principal command-id]
  (when-not (and (identity/principal? principal)
                 (identity/command-id? command-id)
                 (portable-subject? (identity/raw-value principal))
                 (portable-subject? (identity/raw-value command-id)))
    (reject! :invalid-derived-command-identity {})))

(def ^:private observation-sql
  ;; Fixed projection: worker lease/ack fields may evolve the row schema
  ;; while this read runs. Do not select them, or leak model subjects.
  (str "SELECT _id, gesso$live$outbox$type AS intent_type, "
       "gesso$live$outbox$version AS intent_version, "
       "gesso$live$outbox$source_id AS source_id, "
       "gesso$live$outbox$operation AS operation_id, "
       "gesso$live$outbox$command_id AS command_id, "
       "gesso$live$outbox$status AS intent_status "
       "FROM gesso_live_outbox WHERE _id = ?"))

(defn- observe-intent! [ctx source operation principal command-id]
  (require-derived-command! principal command-id)
  (let [raw-command (identity/raw-value command-id)
        id (derived-intent-id source operation (identity/raw-value principal) raw-command 0)
        rows (try (xtdb/q-from ctx [observation-sql id])
                  (catch InterruptedException e
                    (.interrupt (Thread/currentThread))
                    (throw e)))]
    (when-not (and (vector? rows) (<= (count rows) 1))
      (reject! :invalid-intent-observation {}))
    (if (empty? rows)
      {:outcome :not-observed :intent-id id}
      (let [row (first rows)]
        (when-not (and (map? row)
                       (= {:xt/id id :intent-type outbox/intent-type
                           :intent-version outbox/intent-version :source-id source
                           :operation-id operation :command-id raw-command}
                          (select-keys row [:xt/id :intent-type :intent-version
                                            :source-id :operation-id :command-id]))
                       (#{:pending :leased :dispatch-accepted} (:intent-status row)))
          (reject! :inconsistent-intent-observation {:intent-id id}))
        {:outcome :prior-intent-present :intent-id id
         :publication-status (:intent-status row)}))))

(def preparation-key :gesso.live.outbox.model-publication/prepare!)

(defn observe-command!
  "Read the current slot-zero intent for the installed derived-ID command.
   Requires the same trusted source/operation/principal/command binding as
   preparation. No caller-supplied identity or lookup arguments. A present
   row is evidence of a durable intent under the selected trusted writer
   convention, NOT a reconstructed settlement, domain projection, original
   transaction basis or proof of matching command arguments. Absence is an
   observation, not proof of noncommit or permission to bypass guards.
   Journal-only outcomes intentionally have no intent; use command record
   reconciliation for those. Explicit-ID batches, removed rows and arbitrary writers are outside the
   guarantee. Never retries, mutates, publishes or acknowledges anything."
  [ctx]
  (if-let [observe (::observe (meta (get ctx preparation-key)))]
    (observe ctx)
    (reject! :missing-command-preparation {})))

(defn prepare!
  "Prepare an atomic outbox contribution using the installed command context.
   Supply :purpose alone for derived retry-stable UUIDs, or :intent-ids
   and :purpose for the existing explicit mode. Derived mode assumes one
   nonempty publication batch per source/operation/principal/command; slot
   zero fences retries even when their proposed changes differ. Do not mix
   explicit and derived modes for one command. Changes carry their model
   identity fields. The trusted executor still owns the domain transaction
   and must prefix guard-ops before writes. Strict derived-model profiles
   reject this raw API. Not a sandbox for arbitrary writers."
  [ctx spec changes]
  (if-let [prepare (get ctx preparation-key)]
    (prepare spec changes)
    (reject! :missing-command-preparation {})))

(defn transact!
  "Prepare and execute one command-bound durable publication transaction.
   Put every intent absence guard before the trusted domain transaction
   operations and append the intent write. Return the original XTDB receipt
   (including progression) and prepared :outbox/intents; do not emit or
   acknowledge publication here. Original provider exceptions, including
   committed post-execute-result failures, propagate unchanged.

   Domain ops are trusted, including any domain ASSERTs; this does not
   validate their semantics or prohibit arbitrary raw writes to the outbox.
   Derived mode retains IDs across replacement executions; explicit mode
   requires the caller to retain them. Neither mode reconstructs settlements."
  [ctx spec changes domain-ops]
  (when-not (and (vector? domain-ops) (seq domain-ops))
    (reject! :invalid-domain-ops {}))
  (let [{:keys [intents guard-ops tx-op]} (prepare! ctx spec changes)
        ops (into (into guard-ops domain-ops) [tx-op])
        receipt (xtdb/execute-tx-from! ctx ops)]
    (assoc receipt :outbox/intents intents)))

(defn- require-raw-admission! [ctx]
  (when (::require-derived-model? (meta (get ctx preparation-key)))
    (reject! :canonical-model-plan-required {})))

(defn- command-binding! [ctx]
  (if-let [binding (::binding (meta (get ctx preparation-key)))]
    (binding)
    (reject! :missing-command-preparation {})))

(defn- assertion-op? [op]
  ;; Selected raw XTDB SQL assertion vocabulary, not a general SQL parser.
  ;; Bind values as parameters; do not accept DML or multiple statements.
  (and (vector? op) (string? (first op))
       (re-find #"(?i)^ASSERT\s+" (first op))
       (not (str/includes? (first op) ";"))))

(defn- resolved-plan!
  [ctx spec changes domain-ops outcome now]
  (require-raw-admission! ctx)
  (let [journal-only? (nil? spec)]
    (when-not (or journal-only? (and (map? spec) (= #{:purpose} (set (keys spec)))))
      (reject! :invalid-resolved-intent-spec {}))
    (when-not (and (vector? domain-ops)
                   (if journal-only?
                     (every? assertion-op? domain-ops)
                     (seq domain-ops)))
      (reject! :invalid-domain-ops {}))
    (when journal-only?
      (when-not (= [] changes)
        (reject! :journal-only-requires-no-changes {}))
      (when-not (set? (::topics (meta (get ctx preparation-key))))
        (reject! :undeclared-operation-publication {})))
    (let [record (resolution/command-record (command-binding! ctx) outcome now)
          {:keys [intents guard-ops tx-op]}
          (if journal-only? {:intents [] :guard-ops []} (prepare! ctx spec changes))
          ops (cond-> (conj (into (conj guard-ops
                                        ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_commands WHERE _id = ?)"
                                         (:xt/id record)])
                                  domain-ops)
                            [:put-docs resolution/command-entity-type record])
                tx-op (conj tx-op))]
      {:ops ops :intents intents :record record})))

(defn- derive-model-fragment [fragment subjects entity-topics]
  (when (seq (:changes fragment))
    (reject! :explicit-changes-with-derived-model {}))
  (let [contributions
        (for [{:model/keys [entity-type id before after]} (:commands fragment)
              :let [topics (get entity-topics entity-type)]
              :when (or (seq topics)
                        (reject! :unmapped-model-entity {:entity-type entity-type}))
              topic (sort topics)
              :let [field (get subjects topic)
                    document-field (if (= :id field) :xt/id field)
                    old (when before (get before document-field))
                    new (get after document-field)]]
          (do
            (when-not (and (portable-subject? new)
                           (or (nil? before) (portable-subject? old)))
              (reject! :invalid-model-document-subject {:entity-type entity-type :topic topic :field field}))
            {:changes (mapv #(hash-map :topic topic field %)
                            (distinct (if before [old new] [new])))
             ;; A version guard alone does not establish the correctness of
             ;; a caller's claimed old subject. Check that value in XTDB.
             :assertion (when (and before (not= :xt/id document-field))
                          (model-tx/assert-one entity-type
                                               [:and [:= :xt/id [:lift id]]
                                                [:= document-field [:lift old]]]))}))]
    (-> fragment
        (assoc :changes (vec (distinct (mapcat :changes contributions))))
        (update :assertions into (distinct (keep :assertion contributions))))))

(defn- model-resolved-plan! [ctx spec model-plan outcome now]
  ;; This boundary owns publication, execution options and the command ledger.
  (when-not (and (map? model-plan)
                 (every? (conj model-tx/fragment-keys :emit) (keys model-plan))
                 (or (not (contains? model-plan :emit)) (false? (:emit model-plan))))
    (reject! :invalid-durable-model-plan {}))
  (let [journal-only? (nil? spec)
        fragment (model-tx/fragment (select-keys model-plan model-tx/fragment-keys))]
    (when-not (or journal-only? (and (map? spec) (= #{:purpose} (set (keys spec)))))
      (reject! :invalid-resolved-intent-spec {}))
    (if journal-only?
      (do
        (when (seq (:commands fragment))
          (reject! :journal-only-requires-no-model-commands {}))
        (when (seq (:changes fragment))
          (reject! :journal-only-requires-no-changes {}))
        (when-not (set? (::topics (meta (get ctx preparation-key))))
          (reject! :undeclared-operation-publication {})))
      (when-not (seq (:commands fragment))
        (reject! :missing-domain-model-commands {})))
    ;; Reject direct model commands targeting framework-owned tables before
    ;; adding the framework's own canonical command-record creation.
    (when (some (fn [{:model/keys [entity-type]}]
                  (contains? #{"gesso_commands" "gesso_live_outbox" "gesso_publication_manifests"}
                             (-> entity-type name str/lower-case (str/replace "-" "_"))))
                (:commands fragment))
      (reject! :reserved-durable-model-table {}))
    (let [derive (::derive-model-fragment (meta (get ctx preparation-key)))
          fragment (if (and derive (not journal-only?)) (derive fragment) fragment)
          record (resolution/command-record (command-binding! ctx) outcome now)
          {:keys [plan tx-ops]}
          (model-tx/prepare ctx
                            (-> fragment
                                (assoc :emit false)
                                (update :commands conj
                                        (model-command/create resolution/command-entity-type
                                                              record resolution/record-version))))
          {:keys [intents guard-ops tx-op]}
          (if journal-only? {:intents [] :guard-ops []}
              (if-let [prepare-model (::prepare-model (meta (get ctx preparation-key)))]
                (prepare-model spec (:changes plan))
                (prepare! ctx spec (:changes plan))))]
      {:ops (cond-> (into guard-ops tx-ops) tx-op (conj tx-op))
       :intents intents :record record})))

(defn- execute-resolved-plan! [ctx {:keys [ops intents record]}]
  (assoc (xtdb/execute-tx-from! ctx ops)
         :outbox/intents intents :command/record record))

(defn transact-resolved!
  "Commit the canonical model command-result record, domain operations and
   derived publication intents together. Use the existing database-global
   gesso_commands identity convention (string or UUID command ID). The
   trusted executor supplies its portable business outcome and timestamp,
   not a browser settlement. All absence guards precede all writes.
   Explicit nil spec and [] changes select a journal-only outcome;
   domain-ops must then contain only trusted single-statement SQL ASSERTs
   (or be []). No domain writes or publication intents are constructed.
   Do not mix this profile with other writers of the same command ID.
   Domain operations and outcome truth remain trusted. Unknown commit
   errors propagate; reconcile-command! is a separate read-only operation."
  [ctx spec changes domain-ops outcome now]
  (execute-resolved-plan! ctx (resolved-plan! ctx spec changes domain-ops outcome now)))

(defn transact-model-resolved!
  "Execute a canonical model domain plan with the selected command journal
   and durable publication. Reuse model.tx normalization, Biff validation,
   version guards and assertion compilation. The normalized plan supplies
   the sole change batch; the command record is compiled as a canonical
   model create. Nil spec permits only guards and assertions (no domain
   commands or changes). Inline emit, custom entries and transaction options
   are not admitted. Domain assertions and declared changes remain trusted."
  [ctx spec model-plan outcome now]
  (execute-resolved-plan! ctx (model-resolved-plan! ctx spec model-plan outcome now)))

(defn reconcile-command!
  "Authoritatively read the canonical command record for the current trusted
   binding and use the existing disclosure and exact-binding decision.
   may-disclose? receives binding and record; only literal true permits
   disclosure. An unresolved observation is not permission to resubmit.
   The recorded business outcome is not a current authoritative projection,
   progression basis or transport settlement. The executor must obtain those
   independently and apply its original settlement contract."
  [ctx may-disclose?]
  (when-not (fn? may-disclose?)
    (reject! :missing-command-disclosure {}))
  (let [binding (command-binding! ctx)
        rows (xtdb/q-from ctx ["SELECT * FROM gesso_commands WHERE _id = ?"
                               (resolution/record-id binding)])]
    (when-not (and (vector? rows) (<= (count rows) 1) (every? map? rows))
      (reject! :invalid-command-record-observation {}))
    (resolution/reconcile binding (first rows) may-disclose?)))

(defn reconcile-settlement!
  "Return a validated replay operation result for the original optimistic
   server, not a transport settlement. The invocation must match its bound
   command context. Read and authorize the durable record before calling
   the trusted projector (fn [invocation resolution-result] result).
   Projection must use a fresh authoritative observation. Unresolved,
   unavailable and incompatible results never call the projector or execute
   a mutation. Choose reconciliation using trusted server evidence; this
   helper never decides to submit, and never invents a failure settlement."
  [{:keys [ctx] :as invocation} may-disclose? project!]
  (when-not (= (command-binding! ctx)
               (adapter/require-context (:operation invocation) invocation))
    (reject! :mismatched-command-binding {}))
  (adapter/project-resolution! invocation
                               (reconcile-command! ctx may-disclose?)
                               project!))

(defn- recover-after-execute! [ctx may-disclose? failure]
  ;; An interrupted request must not start another provider call.
  (when (instance? InterruptedException failure)
    (.interrupt (Thread/currentThread)))
  (when (.isInterrupted (Thread/currentThread))
    (throw failure))
  (let [prior (try (reconcile-command! ctx may-disclose?)
                   (catch Exception read-error
                     (when (instance? InterruptedException read-error)
                       (.interrupt (Thread/currentThread)))
                     (when-not (identical? failure read-error)
                       (.addSuppressed failure read-error))
                     (throw failure)))]
    (if (= :replay (:status prior)) prior (throw failure))))

(defn execute-resolved!
  "Compose a trusted initial submission or read-only reconciliation with
   the original settlement validator. Check the durable command before
   construction in either mode. Only an unresolved :submit may invoke the
   trusted, side-effect-free constructor, once, with the invocation. Its
   closed result contains :spec, :changes, :domain-ops, :outcome and :now.
   A journal-only outcome uses nil spec, [] changes and optional ASSERTs.
   Alternatively return :spec, :model-plan, :outcome and :now to compile
   a canonical model plan through the same durable boundary (nil spec
   permits a guards/assertions-only model fragment). With the optional
   entity-to-topic mapping, canonical mutations derive changes and guard
   old subject fields; explicit changes are refused in that mode. Strict
   derived-model profiles require the canonical construction form.
   On an execution exception, perform at most one authoritative recovery
   read, never resubmit. Only an exact disclosed replay can settle that
   failure; otherwise preserve the original exception. Construction,
   validation and projection exceptions are not transaction exceptions.
   Mode remains a trusted server decision, not a browser retry flag.
   Absence in :reconcile never authorizes a new mutation."
  [{:keys [ctx operation] :as invocation}
   {:keys [mode construct! may-disclose? project!] :as options}]
  (when-not (and (map? options)
                 (= #{:mode :construct! :may-disclose? :project!} (set (keys options)))
                 (#{:submit :reconcile} mode)
                 (every? fn? [construct! may-disclose? project!]))
    (reject! :invalid-resolved-execution-options {}))
  (when-not (= (command-binding! ctx) (adapter/require-context operation invocation))
    (reject! :mismatched-command-binding {}))
  (let [prior (reconcile-command! ctx may-disclose?)
        result
        (if (and (= :submit mode) (= :unresolved (:status prior)))
          (let [{:keys [spec changes domain-ops model-plan outcome now] :as construction}
                (construct! invocation)
                plan (case (when (map? construction) (set (keys construction)))
                       #{:spec :changes :domain-ops :outcome :now}
                       (resolved-plan! ctx spec changes domain-ops outcome now)
                       #{:spec :model-plan :outcome :now}
                       (model-resolved-plan! ctx spec model-plan outcome now)
                       (reject! :invalid-resolved-construction {}))]
            (try {:status :incorporated :outcome outcome
                  :transaction (execute-resolved-plan! ctx plan)}
                 (catch Exception failure
                   (recover-after-execute! ctx may-disclose? failure))))
          prior)]
    (adapter/project-resolution! invocation result project!)))

(def ^:private ^WeakHashMap registered-executors (WeakHashMap.))

(defn- executor-operation [executor]
  (locking registered-executors
    (let [{:keys [reference operation]} (.get registered-executors executor)]
      (when (and reference (identical? executor (.get ^WeakReference reference)))
        operation))))

(defn build-executor
  "Construct the ordinary optimistic operation :execute! callback for the
   selected publication profile. Mode selection uses trusted server evidence;
   construction and projection retain their existing trusted contracts.
   The invocation must match its installed binding before mode selection.
   Only the exact returned function is recognized by host admission. This
   is local wiring validation, not certification of business policy."
  [{:keys [operation submission-mode! construct! may-disclose? project!] :as config}]
  (when-not (and (map? config)
                 (= #{:operation :submission-mode! :construct! :may-disclose? :project!}
                    (set (keys config)))
                 (qualified-keyword? operation)
                 (every? fn? [submission-mode! construct! may-disclose? project!]))
    (reject! :invalid-publication-executor-config {}))
  (let [executor (fn [invocation]
                   (when-not (= (command-binding! (:ctx invocation))
                                (adapter/require-context operation invocation))
                     (reject! :mismatched-command-binding {}))
                   (execute-resolved!
                    invocation {:mode (submission-mode! invocation)
                                :construct! construct! :may-disclose? may-disclose?
                                :project! project!}))]
    (locking registered-executors
      (.put registered-executors executor
            {:reference (WeakReference. executor) :operation operation}))
    executor))

(defn require-host-wiring!
  "Check the exact selected executor objects against their registered
   operation and the host's selected publication and operation profiles.
   Strict derived-model hosts require every assembled operation to use the
   recognized executor (including explicitly nonpublishing operations).
   Every declared publication topic must have a derivation mapping. This
   is closed inventory admission, not a sandbox for application callbacks."
  [assembly config]
  (let [operations (get-in assembly [:operation-acquisition-assembly
                                     :execution-assembly :server :operations])
        selected (:assembled-publication config)
        strict? (true? (:require-derived-model? selected))
        entity-topics (:model-entity-topics selected)
        derived-topics (when (and (map? entity-topics) (every? set? (vals entity-topics)))
                         (into #{} cat (vals entity-topics)))]
    (doseq [[operation entry] (sort-by key operations)
            :let [built-for (executor-operation (:execute! entry))]]
      (when (and strict? (nil? built-for))
        (reject! :unrecognized-strict-publication-executor {:operation operation}))
      (when built-for
        (when-not (= operation built-for)
          (reject! :publication-executor-operation-mismatch {:operation operation :built-for built-for}))
        (when-not (and (contains? config :assembled-publication)
                       (contains? config :assembled-operations))
          (reject! :missing-publication-executor-profile {:operation operation}))
        (let [topics (server/operation-published-change-topics entry)
              subjects (:subject-keys selected)]
          (when-not (and (map? subjects) (set? topics) (every? #(contains? subjects %) topics))
            (reject! :unsupported-publication-executor-topics {:operation operation :topics topics}))
          (when (and strict? derived-topics (not (every? derived-topics topics)))
            (reject! :unmapped-publication-executor-topics
                     {:operation operation :topics (into #{} (remove derived-topics) topics)})))))))

(defn- require-operation-entities! [operations entity-topics inventory]
  (when-not (and (map? inventory) (= (set (keys operations)) (set (keys inventory))))
    (reject! :incomplete-operation-model-entities {}))
  (doseq [[operation entities] (sort-by key inventory)]
    (when-not (and (set? entities) (every? #(contains? entity-topics %) entities))
      (reject! :invalid-operation-model-entities {:operation operation}))
    (let [derived (into #{} (mapcat entity-topics) entities)
          declared (server/operation-published-change-topics (get operations operation))]
      (when-not (= derived declared)
        (reject! :operation-model-topic-mismatch
                 {:operation operation :derived derived :declared declared})))))

(defn profile!
  "Compile an explicit topic -> subject field declaration against the exact
   admitted Live graph. Every graph topic must have at least one unconditional
   target; all targets for a topic must use its one declared identity field.
   The durable :id is a trusted writer assertion of that field, not proof
   of the domain write. Reject unsupported graphs before provider startup.
   Unknown topics and inconsistent supplied fields fail closed on expansion;
   the ordinary Live runtime retains the physical lease progression. Return
   both replay rules and a command-bound producer for the same mapping.
   Optional :model-entity-topics maps canonical entity keywords to nonempty
   sets of graph topics. Canonical mutations must then map every command
   and derive subjects from its before/after fields (:id means :xt/id).
   Old subjects are asserted against current storage. Select
   :require-derived-model? true with that mapping to refuse raw preparation,
   raw transactions and raw journal-only plans at this command boundary.
   The canonical host requires all assembled operations to use recognized
   executors and declared topics to have derivation mappings in strict mode.
   Optionally supply :operation-model-entities in strict mode: a closed
   operation -> allowed entity set inventory whose derived topic unions
   exactly match the registered topic declarations. Empty sets admit only
   journal-only outcomes. Canonical guarded journal-only plans remain
   available for every operation. Without the strict flag,
   legacy raw paths remain trusted. This is not a sandbox for raw XTDB access."
  [assembly options]
  (when-not (application/application-assembly? assembly)
    (reject! :invalid-assembly {}))
  (when-not (and (map? options) (#{#{:subject-keys} #{:subject-keys :model-entity-topics}
                     #{:subject-keys :model-entity-topics :require-derived-model?}
                     #{:subject-keys :model-entity-topics :require-derived-model? :operation-model-entities}} (set (keys options)))
                 (map? (:subject-keys options)))
    (reject! :invalid-subject-mapping {}))
  (let [compiled (get-in assembly [:operation-acquisition-assembly
                                   :acquisition-assembly :live-app])
        graph (:graph compiled)
        subjects (:subject-keys options)
        entity-topics (:model-entity-topics options)
        strict? (:require-derived-model? options)
        operations (get-in assembly [:operation-acquisition-assembly
                                     :execution-assembly :server :operations])]
    (when-not (and (seq graph) (= (set (keys graph)) (set (keys subjects))))
      (reject! :incomplete-topic-mapping {:expected (set (keys graph))
                                         :actual (set (keys subjects))}))
    (doseq [[topic targets] graph]
      (let [field (get subjects topic)]
        (when-not (and (keyword? field)
                       (not (#{:topic :event/type :change/topic :change/kind :progression} field))
                       (seq targets)
                       (every? #(and (= field (:id-key %)) (not (contains? % :when))) targets))
          (reject! :unsupported-subject-graph {:topic topic :field field
                                               :target-fields (set (map :id-key targets))}))))
    (when (contains? options :model-entity-topics)
      (when-not (and (map? entity-topics) (seq entity-topics)
                     (every? (fn [[entity topics]]
                               (and (keyword? entity) (set? topics) (seq topics)
                                    (every? #(contains? subjects %) topics))) entity-topics))
        (reject! :invalid-model-entity-topics {})))
    (when (and (contains? options :require-derived-model?) (not (true? strict?)))
      (reject! :invalid-derived-model-requirement {}))
    (when (contains? options :operation-model-entities)
      (require-operation-entities! operations entity-topics (:operation-model-entities options)))
    ;; A catch-all prevents the Live runtime's unmatched :keep default from
    ;; silently publishing a raw outbox topic instead of a model invalidation.
    {:manifest (manifest/descriptor compiled subjects)
     :rules
     [{:when (fn [_ _] true)
      :expand (fn [ctx {:keys [topic id] :as change}]
                (let [field (get subjects topic)]
                  (when-not field
                    (reject! :unknown-change-topic {:topic topic}))
                  (when-not (portable-subject? id)
                    (reject! :invalid-subject {:topic topic}))
                  (when (and (contains? change field) (not= id (get change field)))
                    (reject! :conflicting-subject-field {:topic topic :field field}))
                  (model/expand-change compiled ctx (assoc change field id))))}]
     :bind-command
     (fn [source-id {:keys [operation command-id principal arguments]}]
       (let [entry (get operations operation)
             topics (when entry (server/operation-published-change-topics entry))
             prepare-model (fn [spec changes]
             (when-not (and (map? spec) (#{#{:intent-ids :purpose} #{:purpose}} (set (keys spec))))
               (reject! :invalid-command-intent-spec {}))
             (when-not (and (vector? changes) (seq changes))
               (reject! :missing-model-changes {}))
             (when-not (set? topics)
               (reject! :undeclared-operation-publication {:operation operation}))
             (let [spec (if (contains? spec :intent-ids)
                          spec
                          (do
                            (require-derived-command! principal command-id)
                            (assoc spec :intent-ids
                                   (mapv #(derived-intent-id source-id operation
                                                             (identity/raw-value principal)
                                                             (identity/raw-value command-id) %)
                                         (range (count changes))))))]
               (outbox/prepare!
                (assoc spec :source-id source-id :operation operation
                            :command-id (identity/raw-value command-id))
                (mapv (fn [{:keys [topic] :as change}]
                        (when-not (and (map? change) (contains? topics topic))
                          (reject! :undeclared-operation-topic {:operation operation :topic topic}))
                        (let [field (get subjects topic)
                              id (get change field)]
                          (when-not field
                            (reject! :unknown-change-topic {:topic topic}))
                          (when-not (portable-subject? id)
                            (reject! :invalid-subject {:topic topic :field field}))
                          (when (and (contains? change :id) (not= id (:id change)))
                            (reject! :conflicting-subject-field {:topic topic :field field}))
                          (assoc (select-keys change [:topic :change/kind]) :id id)))
                      changes))))]
         (with-meta
           (if strict?
             (fn [_ _] (reject! :canonical-model-plan-required {}))
             prepare-model)
           {::topics topics
            ::require-derived-model? strict?
            ::prepare-model prepare-model
            ::derive-model-fragment (when entity-topics
                                      #(derive-model-fragment
                                        % subjects
                                        (if (contains? options :operation-model-entities)
                                          (select-keys entity-topics (get-in options [:operation-model-entities operation]))
                                          entity-topics)))
            ::binding (fn []
                        (when-not entry
                          (reject! :undeclared-operation-publication {:operation operation}))
                        (resolution/require-binding
                         {:operation operation :command-id command-id
                          :principal principal :arguments arguments}))
            ::observe (fn [ctx]
                        (when-not entry
                          (reject! :undeclared-operation-publication {:operation operation}))
                        (observe-intent! ctx source-id operation principal command-id))})))}))

(defn rules! [assembly options]
  (:rules (profile! assembly options)))
