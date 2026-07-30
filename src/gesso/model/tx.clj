(ns gesso.model.tx
  "Generic atomic transaction mechanics for gesso.model.

   Generated model operations and hand-written complex Gesso FX workflows meet
   at the same transaction representation:

     {:commands   [...]
      :guards     [...]
      :assertions [...]
      :changes    [...]}

   :commands
     Canonical gesso.model.command create/update descriptions. Commands own the
     document mutation and produce generic existence/version preconditions.

   :guards
     Canonical gesso.model.command version guards for documents that influenced
     the decision but are not necessarily mutated. Authorization proofs are one
     application use; Gesso assigns no authorization semantics to guards.

   :assertions
     Explicit HoneySQL XTDB ASSERT forms for invariants that are not reducible
     to ordinary document versions, such as uniqueness or relationship
     cardinality.

   :changes
     Application semantic changes passed to Gesso Live after a successful
     commit.

   This namespace owns transaction-fragment composition, conflict detection,
   ASSERT generation, command translation, Biff 2-compatible transaction
   validation/formatting, and the final Gesso Live commit boundary.

   It deliberately does not own domain transitions, authorization policy,
   Graph reads, or Live invalidation rules."
  (:require
   [clojure.walk :as walk]
   [com.biffweb.core :as biff.core]
   [gesso.live.core :as live]
   [gesso.model.command :as command]
   [honey.sql :as hsql]
   [xtdb.util :as xt.util]))

;; =============================================================================
;; Public transaction contract
;; =============================================================================

(def transact-effect
  "Gesso FX handler key for one atomic model transaction plan."
  ::transact)

(def valid-emit-modes
  #{:async
    :sync
    false})

(def fragment-keys
  #{:commands
    :guards
    :assertions
    :changes})

(def option-keys
  #{:emit
    :entry
    :entry-fn
    :tx-options})

(def plan-keys
  (into
   fragment-keys
   option-keys))

;; =============================================================================
;; Errors and small validation helpers
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

(defn- unknown-keys
  [allowed value]
  (when
   (map?
    value)
    (seq
     (remove
      allowed
      (keys value)))))

(defn- sequential-value!
  [key value]
  (let [value
        (or
         value
         [])]
    (when-not
     (sequential?
      value)
      (fail!
       ::invalid-fragment
       "Transaction fragment collections must be sequential."
       {:key key
        :value value}))
    (vec value)))

(defn- require-entity-type!
  [entity-type]
  (when-not
   (keyword?
    entity-type)
    (fail!
     ::invalid-entity-type
     "A model entity type must be a keyword."
     {:entity-type entity-type}))
  entity-type)

(defn- require-document-id!
  [id]
  (when
   (nil?
    id)
    (fail!
     ::invalid-document-id
     "A model document id must be non-nil."
     {:id id}))
  id)

(defn- assertion-form?
  [value]
  (and
   (map?
    value)
   (contains?
    value
    :assert)))

;; =============================================================================
;; Generic XTDB ASSERT construction
;; =============================================================================

(defn- table-symbol
  [entity-type]
  (symbol
   (name
    (require-entity-type!
     entity-type))))

(defn- count-subquery
  [entity-type where]
  {:select
   [[[:count '*]]]

   :from
   (table-symbol
    entity-type)

   :where
   where})

(defn assert-none
  "Returns an XTDB2 HoneySQL ASSERT requiring zero current matches."
  [entity-type where]
  {:assert
   [:= 0
    (count-subquery
     entity-type
     where)]})

(defn assert-one
  "Returns an XTDB2 HoneySQL ASSERT requiring exactly one current match."
  [entity-type where]
  {:assert
   [:= 1
    (count-subquery
     entity-type
     where)]})

(defn assert-at-most-one
  "Returns an XTDB2 HoneySQL ASSERT requiring at most one current match."
  [entity-type where]
  {:assert
   [:>= 1
    (count-subquery
     entity-type
     where)]})

(defn assert-document-absent
  "Requires that no current document exists for [entity-type id]."
  [entity-type id]
  (require-entity-type!
   entity-type)
  (require-document-id!
   id)

  (assert-none
   entity-type
   [:= :xt/id id]))

(defn assert-document-exists
  "Requires exactly one current document for [entity-type id]."
  [entity-type id]
  (require-entity-type!
   entity-type)
  (require-document-id!
   id)

  (assert-one
   entity-type
   [:= :xt/id id]))

(defn assert-document-current
  "Requires a current document to match one canonical expected-version value.

   expected has the shape produced by gesso.model.command/expected-version:

     {:model/id id
      :model/checks
      [[:entity/revision 4]
       [:entity/updated-at t0]]}

   Gesso does not assume which fields participate in concurrency."
  [entity-type expected]
  (require-entity-type!
   entity-type)

  (let [{:model/keys
         [id
          checks]}
        (command/require-expected-version
         expected)]
    (assert-one
     entity-type
     (into
      [:and
       [:= :xt/id id]]
      (map
       (fn [[key value]]
         [:= key value]))
      checks))))

;; =============================================================================
;; Canonical fragments
;; =============================================================================

(defn fragment
  "Constructs one canonical composable transaction fragment.

   Container validation happens immediately. Commands, guards, assertions, and
   changes are also individually validated here so malformed fragments do not
   travel through several workflow states before failing."
  [value]
  (when-not
   (map?
    value)
    (fail!
     ::invalid-fragment
     "A transaction fragment must be a map."
     {:fragment value}))

  (when-let [unknown
             (unknown-keys
              fragment-keys
              value)]
    (fail!
     ::unknown-fragment-keys
     "A transaction fragment contains unsupported keys."
     {:keys
      (set unknown)

      :allowed-keys
      fragment-keys}))

  (let [commands
        (sequential-value!
         :commands
         (:commands value))

        guards
        (sequential-value!
         :guards
         (:guards value))

        assertions
        (sequential-value!
         :assertions
         (:assertions value))

        changes
        (sequential-value!
         :changes
         (:changes value))]

    (run!
     command/require-command
     commands)

    (run!
     command/require-guard
     guards)

    (when-not
     (every?
      assertion-form?
      assertions)
      (fail!
       ::invalid-assertions
       "Transaction assertions must be HoneySQL :assert maps."
       {:assertions assertions}))

    (when-not
     (every?
      map?
      changes)
      (fail!
       ::invalid-changes
       "Gesso Live semantic changes must be maps."
       {:changes changes}))

    {:commands
     commands

     :guards
     guards

     :assertions
     assertions

     :changes
     changes}))

(def empty-fragment
  "Identity value for transaction-fragment composition."
  (fragment
   {}))

(defn compose
  "Concatenates fragments in argument order.

   Composition itself is intentionally lossless. Duplicate command targets and
   conflicting dependency snapshots are checked when the complete plan is
   normalized, where the framework can see every contributing fragment."
  [& fragments]
  (reduce
   (fn [combined value]
     (let [value
           (fragment
            value)]
       {:commands
        (into
         (:commands combined)
         (:commands value))

        :guards
        (into
         (:guards combined)
         (:guards value))

        :assertions
        (into
         (:assertions combined)
         (:assertions value))

        :changes
        (into
         (:changes combined)
         (:changes value))}))
   empty-fragment
   fragments))

(defn commands-fragment
  "Constructs a fragment containing commands."
  [& commands]
  (fragment
   {:commands commands}))

(defn guards-fragment
  "Constructs a fragment containing generic version guards."
  [& guards]
  (fragment
   {:guards guards}))

(defn assertions-fragment
  "Constructs a fragment containing explicit HoneySQL ASSERT forms."
  [& assertions]
  (fragment
   {:assertions assertions}))

(defn changes-fragment
  "Constructs a fragment containing semantic Gesso Live changes."
  [& changes]
  (fragment
   {:changes changes}))

;; =============================================================================
;; Command and guard targets
;; =============================================================================

(defn command-target
  "Returns [entity-type id] for one canonical command."
  [value]
  (command/target
   value))

(defn guard-target
  "Returns [entity-type id] for one canonical version guard."
  [value]
  (command/guard-target
   value))

(defn- duplicate-targets
  [targets]
  (->> targets
       frequencies
       (keep
        (fn [[target count]]
          (when
           (< 1 count)
            target)))
       set))

(defn- require-distinct-command-targets!
  [commands]
  (let [duplicates
        (duplicate-targets
         (map
          command-target
          commands))]
    (when
     (seq
      duplicates)
      (fail!
       ::duplicate-command-targets
       "One atomic model transaction may mutate each document at most once."
       {:targets duplicates})))
  commands)

;; =============================================================================
;; Guard normalization and dependency conflicts
;; =============================================================================

(defn normalize-guards
  "Validates and canonicalizes version guards.

   Repeated identical guards for one target collapse to the first occurrence.

   Different expected snapshots for the same target are rejected: one atomic
   transaction cannot honestly depend on two versions of the same document."
  [guards]
  (let [guards
        (or
         guards
         [])]

    (when-not
     (sequential?
      guards)
      (fail!
       ::invalid-guards
       "Transaction :guards must be sequential."
       {:guards guards}))

    (let [{:keys
           [order
            by-target]}
          (reduce
           (fn [{:keys
                 [order
                  by-target]
                 :as state}
                value]
             (let [value
                   (command/require-guard
                    value)

                   target
                   (guard-target
                    value)

                   existing
                   (get
                    by-target
                    target)]
               (cond
                 (nil?
                  existing)
                 {:order
                  (conj
                   order
                   target)

                  :by-target
                  (assoc
                   by-target
                   target
                   value)}

                 (=
                  (:model/expected existing)
                  (:model/expected value))
                 state

                 :else
                 (fail!
                  ::conflicting-guards
                  "The same dependency document was supplied at conflicting versions."
                  {:target target
                   :guards
                   [existing
                    value]}))))
           {:order []
            :by-target {}}
           guards)]

      (mapv
       by-target
       order))))

(defn- update-command-expected
  [model-command]
  (when
   (command/update?
    model-command)
    (:model/expected
     model-command)))

(defn- require-command-guard-consistency!
  [commands guards]
  (let [commands-by-target
        (into
         {}
         (map
          (juxt
           command-target
           identity))
         commands)]

    (doseq [guard
            guards
            :let [target
                  (guard-target
                   guard)

                  model-command
                  (get
                   commands-by-target
                   target)]
            :when model-command]

      (cond
        (command/create?
         model-command)
        (fail!
         ::guard-conflicts-with-create
         "A transaction cannot both require an existing version and create the same document."
         {:target target
          :guard guard
          :command model-command})

        (not=
         (:model/expected guard)
         (update-command-expected
          model-command))
        (fail!
         ::guard-conflicts-with-command
         "A mutated document and a dependency guard refer to conflicting versions."
         {:target target
          :guard guard
          :command model-command})))

    true))

(defn effective-guards
  "Returns normalized dependency guards after accounting for mutated documents.

   A guard identical to an update command's own expected version is redundant,
   so it is omitted from the returned vector. Conflicting snapshots fail.

   Guards for unrelated documents remain in first-seen order."
  [commands guards]
  (require-distinct-command-targets!
   commands)

  (let [guards
        (normalize-guards
         guards)

        command-targets
        (set
         (map
          command-target
          commands))]

    (require-command-guard-consistency!
     commands
     guards)

    (into
     []
     (remove
      #(contains?
        command-targets
        (guard-target %)))
     guards)))

;; =============================================================================
;; Translation to XTDB2 transaction operations
;; =============================================================================

(defn command-precondition
  "Returns the generic optimistic ASSERT for one command."
  [model-command]
  (let [{:model/keys
         [entity-type
          id
          expected]}
        (command/require-command
         model-command)]

    (if
     (command/create?
      model-command)
      (assert-document-absent
       entity-type
       id)

      (assert-document-current
       entity-type
       expected))))

(defn guard-assertion
  "Returns the XTDB2 ASSERT for one generic dependency guard."
  [guard]
  (let [{:model/keys
         [entity-type
          expected]}
        (command/require-guard
         guard)]
    (assert-document-current
     entity-type
     expected)))

(defn command->tx-op
  "Returns the Biff/XTDB2 write operation for one canonical command.

   Both create and update commands currently persist with :put-docs. Logical
   deletion remains an application lifecycle concern unless a future model
   requirement demonstrates a need for a generic physical-delete command."
  [model-command]
  (let [{:model/keys
         [entity-type]}
        (command/require-command
         model-command)]
    [:put-docs
     entity-type
     (command/after
      model-command)]))

(defn transaction-ops
  "Compiles one normalized transaction plan to unformatted XTDB2 operations.

   All assertions precede all writes so every optimistic/explicit invariant is
   checked against the transaction's pre-write view.

   Order:
     1. explicit assertions
     2. dependency-guard assertions
     3. command preconditions
     4. command writes"
  [{:keys
    [commands
     guards
     assertions]}]
  (let [commands
        (vec commands)

        guards
        (effective-guards
         commands
         guards)]

    (require-distinct-command-targets!
     commands)

    (into
     []
     cat
     [assertions

      (mapv
       guard-assertion
       guards)

      (mapv
       command-precondition
       commands)

      (mapv
       command->tx-op
       commands)])))

;; =============================================================================
;; Final plan normalization
;; =============================================================================

(defn normalize-plan
  "Validates and canonicalizes one complete transaction plan.

   Defaults:
     :emit :async

   A transaction must contain at least one command.

   Publishing transactions must contain at least one semantic change. Callers
   that intentionally want a database-only transaction must use :emit false."
  [plan]
  (when-not
   (map?
    plan)
    (fail!
     ::invalid-plan
     "A model transaction plan must be a map."
     {:plan plan}))

  (when-let [unknown
             (unknown-keys
              plan-keys
              plan)]
    (fail!
     ::unknown-plan-keys
     "A model transaction plan contains unsupported keys."
     {:keys
      (set unknown)

      :allowed-keys
      plan-keys}))

  (let [fragment
        (fragment
         (select-keys
          plan
          fragment-keys))

        commands
        (:commands fragment)

        guards
        (normalize-guards
         (:guards fragment))

        _
        (require-distinct-command-targets!
         commands)

        _
        (require-command-guard-consistency!
         commands
         guards)

        emit
        (if
         (contains?
          plan
          :emit)
          (:emit plan)
          :async)

        entry
        (:entry plan)

        entry-fn
        (:entry-fn plan)

        tx-options
        (:tx-options plan)]

    (when
     (empty?
      commands)
      (fail!
       ::missing-commands
       "A model transaction must contain at least one command."
       {:plan plan}))

    (when-not
     (contains?
      valid-emit-modes
      emit)
      (fail!
       ::invalid-emit
       "Transaction :emit must be :async, :sync, or false."
       {:emit emit}))

    (when
     (and
      (not=
       false
       emit)

      (empty?
       (:changes fragment)))
      (fail!
       ::missing-changes
       "Publishing model transactions require at least one semantic change."
       {:emit emit}))

    (when
     (and
      (some?
       entry)

      (not
       (map?
        entry)))
      (fail!
       ::invalid-entry
       "Transaction :entry must be a map when supplied."
       {:entry entry}))

    (when
     (and
      (some?
       entry-fn)

      (not
       (fn?
        entry-fn)))
      (fail!
       ::invalid-entry-fn
       "Transaction :entry-fn must be callable when supplied."
       {:entry-fn entry-fn}))

    (when
     (and
      (some?
       entry)
      (some?
       entry-fn))
      (fail!
       ::ambiguous-entry
       "Use either :entry or :entry-fn for a transaction, not both."
       {:entry entry
        :entry-fn entry-fn}))

    (when-not
     (or
      (nil?
       tx-options)
      (map?
       tx-options))
      (fail!
       ::invalid-tx-options
       "Transaction :tx-options must be a map when supplied."
       {:tx-options tx-options}))

    {:commands
     commands

     :guards
     guards

     :assertions
     (:assertions fragment)

     :changes
     (:changes fragment)

     :emit
     emit

     :entry
     entry

     :entry-fn
     entry-fn

     :tx-options
     tx-options}))

;; =============================================================================
;; Biff 2 preparation
;; =============================================================================

(defn- validate-tx!
  "Mirror Biff 2's transaction document validation before Gesso Live executes
   the formatted XTDB operations.

   Biff 2 performs this validation inside com.biffweb.xtdb/execute-tx. Gesso
   Live still owns execution here, so model.tx performs the same public
   biff.core validation step before handing the transaction to Live."
  [tx-ops]
  (doseq [tx-op tx-ops
          :when (vector? tx-op)
          :let [[op _table-or-options & documents] tx-op]
          :when (#{:put-docs :patch-docs} op)]
    (biff.core/validate-with-ex documents))
  tx-ops)

(defn- format-query
  "Format a HoneySQL query/transaction form exactly as Biff 2 does before
   passing it to XTDB.

   Vector XTDB operations already have the representation XTDB expects and are
   returned unchanged."
  [query]
  (if (map? query)
    (hsql/format
     (walk/postwalk
      (fn [value]
        (cond-> value
          (qualified-keyword? value)
          xt.util/kw->normal-form-kw))
      query))
    query))

(defn prepare
  "Normalizes, validates, and formats one transaction plan for Gesso Live.

   Returns:

     {:plan   normalized-plan
      :tx-ops formatted-xtdb-operations}

   Biff 2 moved transaction validation into com.biffweb.xtdb/execute-tx.
   Current Gesso Live remains the execution boundary, so this function mirrors
   Biff 2's validation and HoneySQL normalization without depending on the old
   com.biffweb.experimental namespace or Biff's private impl namespaces."
  [_ctx plan]
  (let [plan
        (normalize-plan
         plan)

        tx-ops
        (transaction-ops
         plan)]

    (validate-tx!
     tx-ops)

    {:plan
     plan

     :tx-ops
     (mapv
      format-query
      tx-ops)}))

;; =============================================================================
;; Gesso Live commit boundary
;; =============================================================================

(defn- live-system-for!
  [ctx emit]
  (when
   (not=
    false
    emit)
    (or
     (:gesso.live/system ctx)
     (:live/system ctx)

     (fail!
      ::missing-live-system
      "Publishing model transactions require the application Gesso Live system."
      {:expected-one-of
       [:gesso.live/system
        :live/system]}))))

(defn- request-biff-listener-poll!
  "Requests an immediate optional Biff 2 XTDB listener poll.

   This is only a latency optimization. A listener-hook failure after a
   successful commit must not make the transaction appear to have failed."
  [ctx]
  (when-some [poll-now
              (:biff.xtdb/poll-now
               ctx)]
    (try
      (poll-now)
      (catch Throwable _
        nil))))

(defn transact!
  "Executes one atomic model plan through Biff validation and Gesso Live.

   Gesso Live owns transaction execution, consistency attachment, change
   expansion, coalescing, dispatch, and publication.

   The consistency-aware :ctx returned by Gesso Live is intentionally
   preserved. Callers that perform a dependent read can therefore carry
   read-your-writes consistency forward rather than reconstructing it."
  [ctx plan]
  (let [{:keys
         [plan
          tx-ops]}
        (prepare
         ctx
         plan)

        {:keys
         [changes
          emit
          entry
          entry-fn
          tx-options]}
        plan

        result
        (live/transact-and-notify!
         (live-system-for!
          ctx
          emit)

         ctx

         {:tx-ops
          tx-ops

          :tx-options
          tx-options

          :changes
          changes

          :emit
          emit

          :entry
          entry

          :entry-fn
          entry-fn})]

    (request-biff-listener-poll!
     ctx)

    (assoc
     result
     :commit/status
     :committed)))

;; =============================================================================
;; Gesso FX/Biff module contribution
;; =============================================================================

(def handlers
  "Gesso FX handlers supplied by (module)."
  {transact-effect
   transact!})

(defn module
  "Returns the Biff module that installs the shared gesso.model transaction
   effect."
  []
  {:biff.fx/handlers
   handlers})
