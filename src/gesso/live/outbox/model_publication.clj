(ns gesso.live.outbox.model-publication
  "Selected durable subject-to-model correspondence. No arbitrary change
   payload, predicate evaluation at replay, or implicit identity coercion."
  (:require [clojure.string :as str]
            [gesso.assurance.xtdb-envelope :as envelope]
            [gesso.live.application-preflight :as application]
            [gesso.live.model :as model]
            [gesso.choreo.identity :as identity]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.publication-manifest :as manifest]
            [gesso.live.outbox.read-frontier :as frontier]
            [gesso.live.outbox.physical-correspondence :as correspondence]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.optimistic.server :as server]
            [gesso.live.optimistic.resolved-operation :as adapter]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.inductive-plan :as inductive]
            [gesso.model.resolved-operation :as resolved]
            [gesso.model.tx :as model-tx]
            [gesso.model.command :as model-command]
            [xtdb.basis :as basis])
  (:import [java.util WeakHashMap]
           [java.time ZonedDateTime]
           [java.lang.ref WeakReference]))

(defn- reject! [reason data]
  (throw (ex-info "Durable model publication not admitted."
                  (assoc data :error/type :gesso.live.outbox/invalid-model-publication
                         :reason reason))))

(defn- portable-subject? [id]
  (or (and (string? id) (not (str/blank? id)))
      (keyword? id) (integer? id) (uuid? id)))

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
        id (outbox/derived-intent-id source operation (identity/raw-value principal) raw-command 0)
        rows (frontier/q-from! ctx [observation-sql id])]
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
   Await the captured submitted log frontier before observing.
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

(defn- model-resolved-plan!
  ([ctx spec model-plan outcome now]
   (model-resolved-plan! ctx spec model-plan outcome now nil))
  ([ctx spec model-plan outcome now contract]
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
                  (contains? #{"gesso_commands" "gesso_live_outbox" "gesso_publication_manifests" "gesso_initializations" "gesso_transition_evidence"}
                             (-> entity-type name str/lower-case (str/replace "-" "_"))))
                (:commands fragment))
      (reject! :reserved-durable-model-table {}))
    (let [derive (::derive-model-fragment (meta (get ctx preparation-key)))
          _ (resolved/require-plan-outcome-coverage! (:outcome contract) outcome)
          record (resolution/command-record (command-binding! ctx) outcome now)
          incorporated (model-tx/normalize-plan
                        (-> fragment
                            (assoc :emit false)
                            (update :commands conj
                                    (model-command/create resolution/command-entity-type
                                                          record resolution/record-version))))
          ;; Check independently admitted targets against the actual canonical
          ;; domain and ledger plan. Generated subject assertions are added
          ;; only afterwards, and cannot change its command or guard targets.
          _ (when contract
              (resolved/require-document-target-coverage!
               (:tables contract) (:targets contract) {:plan incorporated})
              (resolved/require-plan-command-operation-coverage!
               (:operations contract) {:plan incorporated})
              (resolved/require-plan-predicate-coverage!
               (:predicates contract) {:plan incorporated})
              (resolved/require-plan-post-state-coverage!
               (:post-state contract) {:plan incorporated}))
          derived (if (and derive (not journal-only?)) (derive fragment) fragment)
          {:keys [plan tx-ops]}
          (model-tx/prepare ctx (assoc incorporated
                                      :changes (:changes derived)
                                      :assertions (:assertions derived)))
          {:keys [intents guard-ops tx-op]}
          (if journal-only? {:intents [] :guard-ops []}
              (if-let [prepare-model (::prepare-model (meta (get ctx preparation-key)))]
                (prepare-model spec (:changes plan))
                (prepare! ctx spec (:changes plan))))]
      {:ops (cond-> (into (cond-> guard-ops
                            (:single-command-per-principal? contract)
                            (conj (resolution/principal-operation-slot-assertion (command-binding! ctx))))
                          tx-ops)
              tx-op (conj tx-op))
       :intents intents :record record}))))

(def ^:private ^ThreadLocal framed-submission (ThreadLocal.))

(def ^:private ^ThreadLocal framed-observation (ThreadLocal.))
(deftype FramedSubmissionObservation [])
(def ^:private ^WeakHashMap framed-observations (WeakHashMap.))
(def ^:private original-policy-capture xtdb/capture-policy-tx-api-calls!)
(def ^:private original-origin-matcher xtdb/policy-capture-origin-matches?)

(defn capture-framed-submissions!
  "Observe consumed handoffs during one synchronous installed policy window.
   Issue an opaque local observation only after the window closes intact.
   The raw capture still reports the thunk result on invalidation. Handles
   retain historical evidence, not live submission capability; no provider
   authentication or complete writer coverage is implied. Never log reports."
  [admission connectable thunk]
  (when-not (and (nil? (.get framed-observation))
                 (identical? original-policy-capture xtdb/capture-policy-tx-api-calls!)
                 (xtdb/current-submission-policy-token connectable))
    (reject! :invalid-framed-observation-window {}))
  (let [token (xtdb/current-submission-policy-token connectable)
        root (correspondence/activation-root connectable admission token)
        claims (atom []) valid? (atom true)
        scope {:admission admission :connectable connectable :claims claims :valid? valid?}
        capture (try
                  (.set framed-observation scope)
                  (original-policy-capture connectable thunk)
                  (finally (.remove framed-observation)))
        handle (when (and @valid? (= :matched-local-policy-window (:policy-window capture))
                          (try (identical? token (xtdb/current-submission-policy-token connectable))
                               (catch Exception _ false)))
                 (let [h (FramedSubmissionObservation.)]
                   (locking framed-observations
                     (.put framed-observations h {:admission admission :claims @claims :capture capture
                                                  :activation-root root}))
                   h))]
    {:observation handle :capture capture}))

(defn- framed-observation-entry [admission handle]
  (when (instance? FramedSubmissionObservation handle)
    (locking framed-observations
      (let [entry (.get framed-observations handle)]
        (when (and entry (identical? admission (:admission entry)))
          entry)))))

(defn read-framed-submission-observation!
  "Read an exact locally issued historical observation for its admission.
   Does not re-establish a current policy, and does not expose the node."
  [admission handle]
  (when-let [entry (framed-observation-entry admission handle)]
    (select-keys entry [:claims :capture :activation-root])))

(defn framed-submission-origin-matches?
  "Check the actual capture in the issued admission's observation against a
   supplied connectable. Historical origin only: does not grant submission
   capability, establish current policy, authenticate storage, or admit proof.
   Never accepts a supplied capture in place of the issued observation."
  [admission handle connectable]
  (boolean
   (when (identical? original-origin-matcher xtdb/policy-capture-origin-matches?)
     (when-let [entry (framed-observation-entry admission handle)]
       (original-origin-matcher (:capture entry) connectable)))))

(defn claim-framed-submission!
  "For a trusted installed adapter policy: claim the exact current framed
   admission once, for its original issued registry and connectable. Return
   the checked candidate and identity or nil. The scope is thread-local,
   exists only during synchronous execution, and is cleared on every exit.
   No caller-provided report or independently admitted vector creates a
   scope. This is local handoff, not protection against raw XTDB writers."
  [admission connectable submission]
  (when-let [scope (.get framed-submission)]
    (when (and (identical? admission (:admission scope))
               (xtdb/same-connectable? connectable (:connectable scope))
               (= submission (:submission scope))
               (compare-and-set! (:available? scope) true false))
      (when-let [{observed-admission :admission observed-node :connectable
                  :keys [claims valid?]} (.get framed-observation)]
        (try
          (if (and (identical? admission observed-admission)
                   (xtdb/same-connectable? connectable observed-node))
            (swap! claims conj {:submission submission :checked (:checked scope)})
            (reset! valid? false))
          (catch Exception _ (reset! valid? false))))
      (:checked scope))))

(defn- mapped-rows! [node admission opts]
  (let [tables (into #{} (keep (fn [[kind table]]
                                 (when (#{:document :nullable-document} kind) table)))
                     (vals (:target-map (inductive/describe-mapped-admission admission))))]
    {:docs (into {} (map (fn [table]
                           (when-not (and (simple-keyword? table)
                                          (re-matches #"[a-z][a-z0-9_]*" (name table)))
                             (reject! :invalid-mapped-readback-table {}))
                           [table (xtdb/q-from node [(str "SELECT * FROM " (name table))] opts)]))
                 (sort tables))
     :journal (xtdb/q-from node ["SELECT * FROM gesso_commands"] opts)}))

(defn- ledger-time [row]
  (when (instance? ZonedDateTime (:system-time row))
    (.toInstant ^ZonedDateTime (:system-time row))))

(defn- check-preceding-mapped-state! [node admission evidence opts commit-time tx-id]
  ;; The selected host submits default-time transactions. The two most recent
  ;; ledger rows at the commit basis must identify a unique committed step
  ;; and a strictly earlier predecessor (which may itself have aborted).
  ;; Do not guess a time interval or require adjacent numeric transaction IDs.
  (let [rows (xtdb/q-from node ["SELECT _id, committed, system_time FROM xt.txs ORDER BY _id DESC LIMIT 2"] opts)
        [committed preceding] rows
        preceding-time (ledger-time preceding)]
    (when-not (and (vector? rows) (= 2 (count rows))
                   (integer? (:xt/id committed)) (integer? (:xt/id preceding))
                   (< (:xt/id preceding) (:xt/id committed))
                   (or (nil? tx-id) (= tx-id (:xt/id committed)))
                   (true? (:committed committed)) (boolean? (:committed preceding))
                   (= commit-time (ledger-time committed))
                   preceding-time (neg? (compare preceding-time commit-time)))
      (reject! :ambiguous-mapped-prestate-ledger {}))
    ;; Valid time must remain at the commit time, not the predecessor time.
    (let [prior-opts (assoc opts :snapshot-token
                            (basis/->time-basis-str {xtdb/default-database-name [preceding-time]}))
          {:keys [docs journal]} (mapped-rows! node admission prior-opts)]
      (let [checked (inductive/check-mapped-prestate! admission evidence docs journal)]
        {:system-time preceding-time :tx-id (:xt/id preceding)
         :guard-prefix-correspondence (:guard-prefix-correspondence checked)
         :physical-correspondence :checked-preceding-projection}))))

(defn- check-committed-poststate! [ctx {:keys [admission checked evidence]} receipt]
  (try
    (let [basis (xtdb/tx-result-basis (:tx-result receipt))
          _ (when-not basis (reject! :missing-mapped-commit-basis {}))
          node (xtdb/connectable-from ctx)
          opts (assoc (frontier/query-options! node)
                      :snapshot-token (xtdb/basis-snapshot-token basis)
                      :current-time (xtdb/basis-system-time basis))
          {:keys [docs journal]} (mapped-rows! node admission opts)]
      (when-not (= [evidence] (xtdb/q-from node ["SELECT * FROM gesso_transition_evidence WHERE _id = ?"
                                                (:xt/id evidence)] opts))
        (reject! :mapped-transition-evidence-mismatch {}))
      (inductive/check-mapped-poststate! admission checked docs journal)
      ;; Derive inputs again from the actual durable command and saved prestate;
      ;; the preceding projection check below independently checks that prestate.
      (let [semantic (inductive/check-historical-mapped-poststate!
                      admission (some #(when (= (:xt/id evidence) (:xt/id %)) %) journal) evidence docs journal)
            time (xtdb/basis-system-time basis)
            prestate (check-preceding-mapped-state! node admission evidence opts time (xtdb/basis-tx-id basis))]
        (assoc receipt :mapped-readback {:basis basis :authority :none
                                         :prestate prestate
                                         :correspondence (correspondence/transition-report node admission evidence time prestate semantic)
                                         :physical-correspondence :checked-commit-basis-projection})))
    (catch Exception cause
      (when (instance? InterruptedException cause)
        (.interrupt (Thread/currentThread)))
      ;; The provider has already returned a committed receipt. Never run
      ;; abort recovery or project successful correspondence after this error.
      (throw (ex-info "Committed mapped transaction failed physical poststate verification."
                      {:error/type ::mapped-postcommit-failure
                       :failure/stage :post-commit-correspondence
                       :commit/status :committed
                       :tx-result (:tx-result receipt)} cause)))))

(defn- execute-resolved-plan! [ctx {:keys [ops intents record frame-handoff]}]
  ;; A constructor may finish after its request has been cancelled. Check
  ;; at the last shared boundary before the physical submission, without
  ;; consuming the interrupt flag. This is not atomic with provider entry.
  (when (.isInterrupted (Thread/currentThread))
    (throw (InterruptedException. "Durable model submission interrupted")))
  (let [prior (.get framed-submission)]
    (when prior (reject! :nested-framed-submission {}))
    (let [receipt (try
                    (when frame-handoff
                      (.set framed-submission
                            (assoc frame-handoff :connectable (xtdb/connectable-from ctx)
                                   :available? (atom true))))
                    (assoc (xtdb/execute-tx-from! ctx ops)
                           :outbox/intents intents :command/record record)
                    (finally (.remove framed-submission)))]
      (if frame-handoff
        (check-committed-poststate! ctx frame-handoff receipt)
        receipt))))

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
   binding, awaiting the captured submitted log frontier, and use the
   existing disclosure and exact-binding decision.
   may-disclose? receives binding and record; only literal true permits
   disclosure. An unresolved observation is not permission to resubmit.
   The recorded business outcome is not a current authoritative projection,
   progression basis or transport settlement. The executor must obtain those
   independently and apply its original settlement contract."
  [ctx may-disclose?]
  (when-not (fn? may-disclose?)
    (reject! :missing-command-disclosure {}))
  (let [binding (command-binding! ctx)
        rows (frontier/q-from! ctx ["SELECT * FROM gesso_commands WHERE _id = ?"
                                    (resolution/record-id binding)])]
    (when-not (and (vector? rows) (<= (count rows) 1) (every? map? rows))
      (reject! :invalid-command-record-observation {}))
    (resolution/reconcile binding (first rows) may-disclose?)))

(defn- reconcile-mapped-command! [ctx may-disclose? admission]
  ;; Do not inspect evidence or historical domain state until the ordinary
  ;; disclosure and exact-binding decision permits replay.
  (let [binding (command-binding! ctx)
        id (resolution/record-id binding)
        query ["SELECT *, _system_from FROM gesso_commands WHERE _id = ?" id]
        rows (frontier/q-from! ctx query)
        row (first rows)
        record (some-> row (dissoc :xt/system-from))]
    (when-not (and (vector? rows) (<= (count rows) 1) (every? map? rows))
      (reject! :invalid-command-record-observation {}))
    (let [prior (resolution/reconcile binding record may-disclose?)]
      (if (= :replay (:status prior))
        (let [time (:xt/system-from row)
              _ (when-not (instance? ZonedDateTime time)
                  (reject! :missing-mapped-command-commit-time {}))
              instant (.toInstant ^ZonedDateTime time)
              node (xtdb/connectable-from ctx)
              evidence-query ["SELECT *, _system_from FROM gesso_transition_evidence WHERE _id = ?" id]
              current (frontier/q-from! ctx evidence-query)
              evidence-row (first current)
              _ (when-not (and (vector? current) (= 1 (count current))
                               (= time (:xt/system-from evidence-row)))
                  (reject! :missing-or-displaced-mapped-transition-evidence {}))
              opts (assoc (frontier/query-options! node)
                          :snapshot-token (basis/->time-basis-str {xtdb/default-database-name [instant]})
                          :current-time instant)
              _ (when-not (and (= [row] (xtdb/q-from node query opts))
                               (= current (xtdb/q-from node evidence-query opts)))
                  (reject! :historical-mapped-record-mismatch {}))
              {:keys [docs journal]} (mapped-rows! node admission opts)
              evidence (dissoc evidence-row :xt/system-from)
              semantic (inductive/check-historical-mapped-poststate! admission record evidence docs journal)
              prestate (check-preceding-mapped-state! node admission evidence opts instant nil)]
          (assoc prior :mapped-readback {:system-time instant :authority :none
                                          :prestate prestate
                                          :correspondence (correspondence/transition-report node admission evidence instant prestate semantic)
                                          :physical-correspondence :checked-historical-projection}))
        prior))))

(defn- reconcile-selected! [ctx may-disclose? admission]
  (if admission
    (reconcile-mapped-command! ctx may-disclose? admission)
    (reconcile-command! ctx may-disclose?)))

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

(defn- recover-after-execute! [ctx may-disclose? failure after-abort! admission]
  (when (= ::mapped-postcommit-failure (:error/type (ex-data failure)))
    (throw failure))
  ;; An interrupted request must not start another provider call.
  (when (instance? InterruptedException failure)
    (.interrupt (Thread/currentThread)))
  (when (.isInterrupted (Thread/currentThread))
    (throw failure))
  (let [prior (try (reconcile-selected! ctx may-disclose? admission)
                   (catch Exception read-error
                     (when (instance? InterruptedException read-error)
                       (.interrupt (Thread/currentThread)))
                     (when-not (identical? failure read-error)
                       (.addSuppressed failure read-error))
                     (throw failure)))]
    (cond
      (= :replay (:status prior)) prior
      (and after-abort! (= :unresolved (:status prior))
           (= :xtdb/assert-failed (:xtdb.error/code (ex-data failure)))
           (not (xtdb/post-execute-result-failure? failure))
           (not (.isInterrupted (Thread/currentThread))))
      (after-abort! failure)
      :else (throw failure))))

(defn- frame-resolved-plan! [ctx plan admission]
  (if admission
    (let [original {:api :execute-tx :tx-ops (:ops plan) :tx-options (xtdb/tx-opts nil)}
          actual (admission :frame-submission! original)]
      ;; Consume the exact preparation before physical submission. This is
      ;; intentionally outside transaction recovery: a failed local admission
      ;; is not an XTDB abort or evidence of an uncertain commit.
      (let [checked (admission :admit-framed-publication! original actual
                               (::source-id (meta (get ctx preparation-key))))
            evidence (inductive/mapped-transition-evidence! admission checked)
            ;; The closed footprint excludes this framework-owned record.
            ;; Add it only after admission; bind the final vector to the handoff.
            ops (envelope/bind-transition-envelope!
                 (into [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_transition_evidence WHERE _id = ?)"
                         (:xt/id evidence)]]
                       (conj (:tx-ops actual) [:put-docs :gesso_transition_evidence evidence])))
            evidence (peek (peek ops))
            submission (assoc actual :tx-ops ops)]
        (when-not (= (:xt/id evidence) (:xt/id (:record plan)))
          (reject! :mapped-transition-evidence-command-mismatch {}))
        (assoc plan :ops ops
               :frame-handoff {:admission admission :submission submission
                               :checked checked :evidence evidence})))
    plan))

(defn- guarded-abort-outcome! [invocation may-disclose? construct! failure contract admission]
  (let [ctx (:ctx invocation)
        {:keys [spec model-plan outcome now] :as construction} (construct! invocation)]
    (when (nil? construction) (throw failure))
    (when-not (and (map? construction)
                   (= #{:spec :model-plan :outcome :now} (set (keys construction)))
                   (nil? spec) (map? model-plan)
                   (or (seq (:guards model-plan)) (seq (:assertions model-plan))))
      (reject! :invalid-abort-outcome-construction {}))
    ;; The ordinary journal-only compiler excludes model commands, changes
    ;; and publication. A second abort may reconcile a winning record but
    ;; cannot invoke another constructor.
    (let [plan (frame-resolved-plan!
                ctx (model-resolved-plan! ctx nil model-plan outcome now contract) admission)]
      (try {:status :incorporated :outcome outcome
            :transaction (execute-resolved-plan! ctx plan)}
           (catch Exception second-failure
             (recover-after-execute! ctx may-disclose? second-failure nil admission))))))

(defn- require-target-options! [options]
  (let [table? (contains? options :construction-contract)
        target? (contains? options :target-contract!)
        operation? (contains? options :command-operation-contract!)
        predicate? (contains? options :predicate-contract!)
        post-state? (contains? options :post-state-contract!)
        outcome? (contains? options :outcome-contract!)]
    (when (and (contains? options :single-command-per-principal?)
               (not (and (true? (:single-command-per-principal? options)) target?)))
      (reject! :invalid-principal-slot-contract {}))
    (when-not (= table? target?)
      (reject! :incomplete-document-target-contract {}))
    (when table?
      (resolved/require-construction-contract (:construction-contract options))
      (when-not (fn? (:target-contract! options))
        (reject! :invalid-document-target-callback {})))
    (when operation?
      (when-not target?
        (reject! :command-operation-contract-requires-document-targets {}))
      (when-not (fn? (:command-operation-contract! options))
        (reject! :invalid-command-operation-callback {})))
    (when predicate?
      (when-not target?
        (reject! :predicate-contract-requires-document-targets {}))
      (when-not (fn? (:predicate-contract! options))
        (reject! :invalid-predicate-callback {})))
    (when post-state?
      (when-not target?
        (reject! :post-state-contract-requires-document-targets {}))
      (when-not (fn? (:post-state-contract! options))
        (reject! :invalid-post-state-callback {})))
    (when outcome?
      (when-not (and operation? post-state?)
        (reject! :outcome-contract-requires-operation-and-post-state {}))
      (when-not (fn? (:outcome-contract! options))
        (reject! :invalid-outcome-callback {}))))
  options)

(def ^:private contract-option-keys
  #{:construction-contract :target-contract! :command-operation-contract!
    :predicate-contract! :post-state-contract! :outcome-contract!})

(defn- require-mapped-admission! [options]
  (when (contains? options :mapped-admission)
    (when-not (= contract-option-keys (set (keys (select-keys options contract-option-keys))))
      (reject! :mapped-admission-requires-closed-contract {}))
    ;; Only an exact issued callable is accepted. This is a local admission
    ;; check, not a fresh proof or authentication of acquired rows.
    (inductive/describe-mapped-admission (:mapped-admission options)))
  options)

(defn- require-abort-contract! [options]
  (when (contains? options :abort-contract)
    (let [abort (:abort-contract options)]
      (when-not (and (fn? (:construct-after-abort! options))
                     (= contract-option-keys (set (keys (select-keys options contract-option-keys))))
                     (map? abort) (= contract-option-keys (set (keys abort))))
        (reject! :invalid-abort-contract {}))
      (require-target-options! abort)
      (let [tables (:construction-contract abort)]
        (when-not (and (seq (:required-guard-tables tables))
                       (= #{resolution/command-entity-type}
                          (:required-write-tables tables) (:allowed-write-tables tables)))
          (reject! :abort-contract-requires-guarded-journal-only {})))))
  options)

(defn- admit-contract! [ctx {:keys [construction-contract target-contract!
                                    predicate-contract! command-operation-contract!
                                    post-state-contract! outcome-contract! single-command-per-principal?]}]
  (when target-contract!
    (let [binding (command-binding! ctx)
          targets (resolved/admit-document-targets!
                   construction-contract (target-contract! ctx binding) binding)]
      {:tables construction-contract
       :single-command-per-principal? single-command-per-principal?
       :targets targets
       :predicates (when predicate-contract!
                     (resolved/require-predicate-contract!
                      (predicate-contract! ctx binding) targets))
       :operations (when command-operation-contract!
                     (resolved/require-command-operation-contract!
                      (command-operation-contract! ctx binding) targets binding))
       :post-state (when post-state-contract!
                     (resolved/require-post-state-contract!
                      (post-state-contract! ctx binding) targets))
       :outcome (when outcome-contract!
                  (resolved/require-outcome-contract!
                   (outcome-contract! ctx binding) binding))})))

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
   Optional :construct-after-abort! is a trusted policy callback invoked once
   only after an explicit XTDB assertion abort and unresolved recovery read.
   It returns nil to preserve the failure, or a fresh canonical guarded
   journal-only outcome. It never retries the domain mutation. A second
   failure receives reconciliation only. Mode and this policy are trusted
   server decisions, not browser retry flags.
   Absence in :reconcile never authorizes a new mutation.
   Optional paired :construction-contract and :target-contract! (ctx binding)
   use the existing resolved-operation contracts before construction. The
   same admitted requirements constrain guarded abort outcomes unless a
   separate complete :abort-contract is registered. Optional
   :command-operation-contract! (ctx binding) requires those contracts and
   independently declares the permitted canonical operation triples; names
   alone do not establish predicate or post-state correspondence. Optional
   :predicate-contract! (ctx binding) requires document targets and checks
   exact canonical guard and update conditions, not arbitrary SQL assertions.
   Optional :post-state-contract! (ctx binding) requires document targets
   and checks canonical domain command values, not physical readback.
   Optional :outcome-contract! (ctx binding) additionally requires operation
   and post-state contracts and checks the exact durable command outcome,
   including a guarded abort construction. A separate :abort-contract map
   must contain the full six contract options, require guarded journal-only
   writes, and is admitted only after the physical abort and unresolved
   recovery, before the abort constructor. It cannot authorize domain writes.
   Optional :single-command-per-principal? true requires document-target
   contracts and adds a binding-derived ledger absence assertion to both
   canonical acceptance and abort-refusal transactions. It scopes one
   durable command to a string principal and operation, not command ID or
   arguments, for the lifetime of its current ledger record. Replay is
   unchanged. Other writers must obey the same restriction.
   Optional :mapped-admission is an issued inductive mapped admission. It
   requires the complete canonical contract and frames and consumes the
   selected preparation after ordinary model/publication compilation, before
   either initial or guarded-abort physical submission. Authorized replay
   checks the source-bound durable evidence at the original commit time.
   Local admission failures propagate without transaction recovery.
   The host must still mediate other physical writers; this selection alone
   does not qualify the installed XTDB submission policy.
   Neither ordinary contract verifies projection. Raw SQL
   construction cannot satisfy this selected contract. Replay does not
   invoke the declaration or constructor. Callback policy is still trusted."
  [{:keys [ctx operation] :as invocation}
   {:keys [mode construct! may-disclose? project! construct-after-abort!
           construction-contract target-contract! command-operation-contract! predicate-contract! post-state-contract! outcome-contract! abort-contract single-command-per-principal? mapped-admission] :as options}]
  (when-not (and (map? options)
                 (#{#{:mode :construct! :may-disclose? :project!}
                    #{:mode :construct! :may-disclose? :project! :construct-after-abort!}}
                  (disj (set (keys options)) :construction-contract :target-contract!
                        :command-operation-contract! :predicate-contract! :post-state-contract! :outcome-contract! :abort-contract :single-command-per-principal? :mapped-admission))
                 (or (not (contains? options :construct-after-abort!))
                     (fn? construct-after-abort!))
                 (#{:submit :reconcile} mode)
                 (every? fn? [construct! may-disclose? project!]))
    (reject! :invalid-resolved-execution-options {}))
  (require-target-options! options)
  (require-abort-contract! options)
  (require-mapped-admission! options)
  (when-not (= (command-binding! ctx) (adapter/require-context operation invocation))
    (reject! :mismatched-command-binding {}))
  (let [prior (reconcile-selected! ctx may-disclose? mapped-admission)
        result
        (if (and (= :submit mode) (= :unresolved (:status prior)))
          (let [contract (admit-contract! ctx options)
                {:keys [spec changes domain-ops model-plan outcome now] :as construction}
                (construct! invocation)
                plan (case (when (map? construction) (set (keys construction)))
                       #{:spec :changes :domain-ops :outcome :now}
                       (if contract
                         (reject! :document-target-contract-requires-model-plan {})
                         (resolved-plan! ctx spec changes domain-ops outcome now))
                       #{:spec :model-plan :outcome :now}
                       (model-resolved-plan! ctx spec model-plan outcome now contract)
                       (reject! :invalid-resolved-construction {}))
                plan (frame-resolved-plan! ctx plan mapped-admission)]
            (try {:status :incorporated :outcome outcome
                  :transaction (execute-resolved-plan! ctx plan)}
                 (catch Exception failure
                   (recover-after-execute! ctx may-disclose? failure
                                           (when construct-after-abort!
                                             #(guarded-abort-outcome! invocation may-disclose?
                                                                      construct-after-abort! %
                                                                      (if abort-contract
                                                                        (admit-contract! ctx (assoc abort-contract :single-command-per-principal? single-command-per-principal?))
                                                                        contract)
                                                                      mapped-admission))
                                           mapped-admission))))
          prior)]
    (adapter/project-resolution! invocation result project!)))

(def ^:private ^WeakHashMap registered-executors (WeakHashMap.))

(defn- executor-registration [executor]
  (locking registered-executors
    (let [{:keys [reference] :as registration} (.get registered-executors executor)]
      (when (and reference (identical? executor (.get ^WeakReference reference)))
        registration))))

(defn build-executor
  "Construct the ordinary optimistic operation :execute! callback for the
   selected publication profile. Mode selection uses trusted server evidence;
   construction and projection retain their existing trusted contracts.
   The invocation must match its installed binding before mode selection.
   Only the exact returned function is recognized by host admission. This
   is local wiring validation, not certification of business policy."
  [{:keys [operation submission-mode! construct! may-disclose? project! construct-after-abort!
           construction-contract target-contract! command-operation-contract! predicate-contract! post-state-contract! outcome-contract! abort-contract single-command-per-principal? mapped-admission] :as config}]
  (when-not (and (map? config)
                 (#{#{:operation :submission-mode! :construct! :may-disclose? :project!}
                    #{:operation :submission-mode! :construct! :may-disclose? :project! :construct-after-abort!}}
                  (disj (set (keys config)) :construction-contract :target-contract!
                        :command-operation-contract! :predicate-contract! :post-state-contract! :outcome-contract! :abort-contract :single-command-per-principal? :mapped-admission))
                 (or (not (contains? config :construct-after-abort!))
                     (fn? construct-after-abort!))
                 (qualified-keyword? operation)
                 (every? fn? [submission-mode! construct! may-disclose? project!]))
    (reject! :invalid-publication-executor-config {}))
  (require-target-options! config)
  (require-abort-contract! config)
  (require-mapped-admission! config)
  (let [executor (fn [invocation]
                   (when-not (= (command-binding! (:ctx invocation))
                                (adapter/require-context operation invocation))
                     (reject! :mismatched-command-binding {}))
                   (execute-resolved!
                    invocation (cond-> {:mode (submission-mode! invocation)
                                        :construct! construct! :may-disclose? may-disclose?
                                        :project! project!}
                                 construct-after-abort!
                                 (assoc :construct-after-abort! construct-after-abort!)
                                 target-contract!
                                 (assoc :construction-contract construction-contract
                                        :target-contract! target-contract!)
                                 command-operation-contract!
                                 (assoc :command-operation-contract! command-operation-contract!)
                                 predicate-contract!
                                 (assoc :predicate-contract! predicate-contract!)
                                 post-state-contract!
                                 (assoc :post-state-contract! post-state-contract!)
                                 outcome-contract!
                                 (assoc :outcome-contract! outcome-contract!)
                                 single-command-per-principal?
                                 (assoc :single-command-per-principal? true)
                                 abort-contract
                                 (assoc :abort-contract abort-contract)
                                 mapped-admission
                                 (assoc :mapped-admission mapped-admission))))]
    (locking registered-executors
      (.put registered-executors executor
            {:reference (WeakReference. executor) :operation operation
             :mapped-admission mapped-admission}))
    executor))

(defn- require-declared-publication-wiring! [admission operation topics config]
  (when admission
    (let [{:keys [operation-mappings publication-derivations] :as description}
          (inductive/describe-mapped-admission admission)]
      (when (contains? description :publication-derivations)
        (let [{:keys [subject-keys model-entity-topics require-derived-model? operation-model-entities]}
              (:assembled-publication config)
              alternatives (filter (fn [[_ mapping]]
                                     (some (fn [[kind _ op]] (and (= :journal-slot kind) (= operation op)))
                                           (:write-targets mapping))) operation-mappings)
              contributions (mapcat (fn [[id _]] (:contributions (get publication-derivations id))) alternatives)
              entities (set (map (comp second :target) contributions))]
          (when-not (and (identical? admission (get-in config [:mapped-writer-policy :admission]))
                         (true? require-derived-model?) (seq alternatives)
                         (= entities (get operation-model-entities operation))
                         (= topics (set (map :topic contributions))))
            (reject! :mapped-publication-wiring-mismatch {:operation operation}))
          (doseq [[id _] alternatives
                  :let [entries (:contributions (get publication-derivations id))]
                  [target grouped] (group-by :target entries)]
            (when-not (and (= (get model-entity-topics (second target)) (set (map :topic grouped)))
                           (every? (fn [{:keys [topic field]}]
                                     (let [installed (get subject-keys topic)]
                                       (= field (if (= :id installed) :xt/id installed)))) grouped))
              (reject! :mapped-publication-selector-mismatch {:operation operation}))))))))

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
            :let [registration (executor-registration (:execute! entry))
                  built-for (:operation registration)]]
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
                     {:operation operation :topics (into #{} (remove derived-topics) topics)}))
          (require-declared-publication-wiring! (:mapped-admission registration) operation topics config))))))

(defn- physical-entity-name! [entity]
  ;; Restrict the selected profile to simple lowercase SQL identifiers.
  ;; XTDB normalizes hyphens to underscores; unqualified tables use public.
  ;; Do not guess how exotic keywords, quoting or system schemas behave.
  (let [schema (or (namespace entity) "public")
        table (name entity)
        simple? #(boolean (re-matches #"[a-z][a-z0-9_-]*" %))]
    (when-not (and (simple? schema) (simple? table)
                   (not (#{"xt" "xtdb" "information_schema" "pg_catalog"}
                         (str/replace schema "-" "_"))))
      (reject! :unsupported-model-entity-name {:entity-type entity}))
    [(str/replace schema "-" "_") (str/replace table "-" "_")]))

(defn- require-distinct-physical-entities! [entity-topics]
  (reduce (fn [seen entity]
            (let [physical (physical-entity-name! entity)]
              (when (#{"gesso_commands" "gesso_live_outbox" "gesso_publication_manifests" "gesso_initializations" "gesso_transition_evidence"}
                      (second physical))
                (reject! :reserved-durable-model-table {:entity-type entity}))
              (when-let [previous (get seen physical)]
                (reject! :aliased-model-entities
                         {:entities #{previous entity} :physical-table physical}))
              (assoc seen physical entity)))
          {} (sort (keys entity-topics)))
  nil)

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
   Mapped entity names must be simple lowercase SQL identifiers with
   distinct physical XTDB table identities. Derived command documents must
   have keyword top-level fields with distinct normalized column identities.
   Old subjects are asserted
   against current storage. Select
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
        (reject! :invalid-model-entity-topics {}))
      (require-distinct-physical-entities! entity-topics))
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
                                   (mapv #(outbox/derived-intent-id source-id operation
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
            ::source-id source-id
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
