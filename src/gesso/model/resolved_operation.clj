(ns gesso.model.resolved-operation
  "Experimental application-facing composition of authoritative Gesso model
   operations with durable command resolution.

   Applications supply ordinary, explicit Clojure functions:

     (build-operation
      {:operation :request/claim
       :read-record! (fn [ctx command-row-id] ...) ; AUTHORITATIVE read
       :may-disclose? (fn [ctx binding record] ...) ; trusted permission
       :construct! (fn [ctx binding]                  ; trusted, PURE plan
                     {:now now :plan plan :outcome outcome})
       ;; Optional closed table-coverage contract (see below):
       :construction-contract
       {:required-guard-tables #{:memberships}
        :allowed-guard-tables #{:memberships}
        :required-write-tables #{:requests :gesso_commands}
        :allowed-write-tables #{:requests :gesso_commands}}
       ;; Optional closed document-target declaration (from trusted inputs):
       :target-contract! (fn [ctx binding]
                           {:required-guard-targets #{[:memberships (:membership-id ctx)]}
                            :allowed-guard-targets #{[:memberships (:membership-id ctx)]}
                            :required-write-targets #{[:requests (get-in binding [:arguments :request-id])]}
                            :allowed-write-targets #{[:requests (get-in binding [:arguments :request-id])]}})
       ;; Optional exact command-operation declaration, independent of the
       ;; constructed plan; requires the closed table and target contracts.
       ;; Its triples distinguish :create from update operations such as :claim.
       :command-operation-contract! (fn [ctx binding]
                                      (let [r (get-in binding [:arguments :request-id])]
                                        {:required-command-operations #{[:requests r :claim]}
                                         :allowed-command-operations #{[:requests r :claim]}}))
       ;; Optional post-state document check from trusted invocation data.
       ;; This is a closed declaration of actual domain command :model/after
       ;; field/value tuples; unlike an operation name, it catches bad outputs.
       :post-state-contract! (fn [ctx binding]
                               (let [r (get-in binding [:arguments :request-id])]
                                 {:required-post-state-values #{[:requests r :owner (:helper-id ctx)]}
                                  :allowed-post-state-values #{[:requests r :xt/id r]
                                                               [:requests r :revision (:next-revision ctx)]
                                                               [:requests r :created-at (:created-at ctx)]
                                                               [:requests r :updated-at (:now ctx)]
                                                               [:requests r :owner (:helper-id ctx)]}}))
       ;; Optional outcome declaration, tied to this invocation and checked
       ;; against the command result that will be durably recorded. This
       ;; requires post-state and write-operation contracts; it cannot prove
       ;; that those independently declared business semantics are correct.
       :outcome-contract! (fn [ctx binding]
                            {:expected-outcome {:status :claimed
                                                :request-id (get-in binding [:arguments :request-id])}})
       ;; Optional exact predicate declaration; requires both prior contracts.
       ;; Allowed includes every version/field check that can legitimately
       ;; appear in the final plan for these document identities.
       :predicate-contract! (fn [ctx binding]
                              (let [m (:membership-id ctx)
                                    r (get-in binding [:arguments :request-id])]
                                {:required-guard-checks #{[:memberships m :active true]}
                                 :allowed-guard-checks #{[:memberships m :active true]
                                                         [:memberships m :revision (:membership-revision ctx)]
                                                         [:memberships m :updated-at (:membership-updated-at ctx)]}
                                 :required-update-checks #{[:requests r :revision (:request-revision ctx)]}
                                 :allowed-update-checks #{[:requests r :revision (:request-revision ctx)]
                                                          [:requests r :updated-at (:request-updated-at ctx)]}}))})

   The result is a callable `(fn [ctx {:binding ... :mode ...}])`. Its modes
   are server-owned decisions, NEVER browser-provided authorization:

   * :reconcile: only read a command record and reconcile. An absent record
     remains :unresolved. No domain planning or mutation happens.
   * :submit: reconcile any existing record first. If none is visible, ask the
     trusted application to construct the domain transaction and append a
     command-result creation to the SAME atomic gesso.model.tx plan; then
     commit through gesso.model.tx/transact!. If the commit is ambiguous or
     throws, reread the authoritative record. Never automatically reexecute.

   :submit must be used only from a trusted initial-submission boundary. An
   unknown delivery is not permission to submit again; callers must use
   :reconcile and an explicit higher-level recovery protocol. This layer
   cannot prove that :construct! correctly authenticates/authorizes the
   principal, reads every relevant dependency, expresses sufficient atomic
   guards, avoids effects before commit, or records a truthful outcome.

   A trusted operation registration may opt into a CLOSED TABLE-COVERAGE
   contract. The wrapper checks the final incorporated plan, including the
   automatically added durable command record, BEFORE calling transact!.
   By itself this detects omitted/extra declared guard and write TABLES only.
   When a registration ALSO supplies trusted :target-contract!, Gesso checks
   the final plan against closed required/allowed document targets derived
   from the context and command binding before domain construction begins.
   Optional :command-operation-contract! declares the expected operation
   keyword (including :create versus a named update operation) per required
   domain write target before plan construction. Gesso supplies its own
   command-ledger :create requirement and checks the final plan before send.
   This is another opt-in consistency check, not semantic operation proof.

   Optional :post-state-contract! independently declares required and
   permitted exact final persisted field/value tuples for DOMAIN writes.
   The closed allowed set covers every final field; Gesso's own command
   ledger is excluded from application declarations and checked elsewhere.
   A matching post-state contract still cannot prove that the declared
   values represent the intended business rule.

   Optional :outcome-contract! declares one deterministic, nonempty portable
   command-result outcome from trusted invocation data before :construct!.
   It is only admitted alongside closed command-operation and post-state
   contracts, and is checked before the ledger record can be sent. The
   declaration is still application-owned and does not prove semantic truth.

   A further optional :predicate-contract! independently declares exact
   required and permitted [table document-id field value] checks for both
   version guards and update-command optimistic conditions. It requires
   both other contracts and checks actual canonical :model/checks, not
   arbitrary SQL or the claimed contents of a record. Even this three-layer
   check does not discover missing business rules or prove authorization,
   complete dependencies, constructor purity, or XTDB semantics.

   After a successful transaction commit, Gesso Live publication and any
   read-your-writes context remain owned by gesso.model.tx. This wrapper does
   not turn its returned outcome into an authoritative browser projection or
   promise durable notification delivery. It is NOT a proof certificate and
   does not register an application operation with Choreo/Live preflight."
  (:require
   [clojure.set :as set]
   [gesso.live.core :as live]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.tx :as tx]))

(def configuration-keys
  #{:operation :read-record! :may-disclose? :construct!})

(def construction-contract-key :construction-contract)
(def target-contract-key :target-contract!)
(def predicate-contract-key :predicate-contract!)
(def command-operation-contract-key :command-operation-contract!)
(def post-state-contract-key :post-state-contract!)
(def outcome-contract-key :outcome-contract!)
(def outcome-contract-keys #{:expected-outcome})
(def post-state-contract-keys
  #{:required-post-state-values :allowed-post-state-values})

(def command-operation-contract-keys
  #{:required-command-operations :allowed-command-operations})

(def predicate-contract-keys
  #{:required-guard-checks :allowed-guard-checks
    :required-update-checks :allowed-update-checks})

(def target-contract-keys
  #{:required-guard-targets :allowed-guard-targets
    :required-write-targets :allowed-write-targets})

(def construction-contract-keys
  #{:required-guard-tables :allowed-guard-tables
    :required-write-tables :allowed-write-tables})

(def invocation-keys
  #{:binding :mode})

(def construction-keys
  #{:now :plan :outcome})

(def modes #{:submit :reconcile})

(def assurance-scope
  {:construction :gesso.model.resolved-operation/operation-v1
   :checked #{:closed-operation-configuration
              :binding-operation-match
              :reconcile-before-domain-construction
              :no-write-during-reconcile
              :atomic-command-record-plan-construction
              :no-automatic-resubmission-after-uncertain-commit}
   :assumed #{:trusted-server-owned-invocation-mode
              :authoritative-command-record-read
              :trusted-principal-authentication
              :correct-domain-construction-and-atomic-guards
              :trusted-record-disclosure
              :canonical-model-transaction-commit}
   :not-established #{:humanhelp-authorization
                      :complete-domain-invariants
                      :record-immutability-against-other-writers
                      :typed-business-rejection-on-guard-failure
                      :commit-uncertainty-liveness
                      :durable-notification-debt
                      :whole-application-choreo-preflight
                      :universal-linearizability
                      :independent-authorization-dependency-discovery
                      :custom-assertion-table-dependency-extraction
                      :proof-that-domain-constructor-is-pure}})

(def construction-contract-scope
  {:construction :gesso.model.resolved-operation/closed-table-coverage-v1
   :checked #{:exact-registered-table-declaration-shape
              :incorporation-ledger-included-in-write-coverage
              :required-guard-and-write-table-presence
              :unexpected-guard-and-write-table-rejection
              :opaque-custom-assertions-rejected-before-transaction-send}
   :assumed #{:trusted-registration-of-business-table-requirements
              :constructor-uses-trusted-inputs-and-is-pure
              :sufficient-document-and-predicate-guards}
   :not-established #{:independent-business-rule-discovery
                      :authorization-or-policy-correctness
                      :correct-row-identities-within-declared-tables
                      :all-executor-branches-and-prestates
                      :xtdb-physical-refinement
                      :application-certification}})

(def target-contract-scope
  {:construction :gesso.model.resolved-operation/closed-document-targets-v1
   :checked #{:target-requirements-computed-before-construction
              :required-and-allowed-document-target-coverage
              :independent-command-ledger-target-derived-from-binding
              :target-declarations-consistent-with-closed-table-contract
              :no-transaction-submission-on-target-mismatch}
   :assumed #{:trusted-target-requirements-callback
              :requirements-derived-independently-of-constructed-plan
              :trusted-binding-and-application-context
              :complete-declaration-of-relevant-document-dependencies}
   :not-established #{:independent-discovery-of-authorization-subjects
                      :authorization-policy-correctness
                      :correct-predicate-values-or-document-versions
                      :constructor-purity
                      :all-executor-branches-and-prestates
                      :xtdb-physical-refinement
                      :application-certification}})

(def predicate-contract-scope
  {:construction :gesso.model.resolved-operation/closed-exact-checks-v1
   :checked #{:predicate-requirements-computed-before-construction
              :actual-guard-and-update-check-values-match-declaration
              :required-checks-present-and-undeclared-checks-rejected
              :checks-restricted-to-independently-declared-document-targets
              :required-guard-targets-have-required-checks
              :no-transaction-submission-on-check-mismatch}
   :assumed #{:trusted-predicate-requirements-callback
              :requirements-derived-independently-of-constructed-plan
              :trusted-command-binding-and-application-context
              :expected-values-are-stable-until-construction-check
              :business-rules-adequately-represented-by-expected-checks}
   :not-established #{:independent-discovery-of-business-rule-predicates
                      :authorization-or-policy-correctness
                      :independent-trustworthiness-of-declared-expected-values
                      :arbitrary-honeysql-assertion-equivalence
                      :constructor-purity
                      :all-executor-branches-and-prestates
                      :xtdb-physical-refinement
                      :application-certification}})

(def command-operation-contract-scope
  {:construction :gesso.model.resolved-operation/closed-command-operations-v1
   :checked #{:command-operation-requirements-computed-before-construction
              :required-and-allowed-command-operations-match-final-plan
              :independent-binding-derived-command-ledger-create
              :all-required-domain-write-targets-have-required-operation
              :no-submission-on-command-operation-mismatch}
   :assumed #{:trusted-command-operation-requirements-callback
              :requirements-independent-of-constructed-plan
              :trusted-invocation-context-and-binding
              :complete-declaration-of-domain-write-semantics}
   :not-established #{:correct-post-state-values
                      :authorization-or-policy-correctness
                      :independent-business-rule-discovery
                      :all-executor-branches-and-prestates
                      :xtdb-physical-refinement
                      :application-certification}})

(def post-state-contract-scope
  {:construction :gesso.model.resolved-operation/closed-post-state-values-v1
   :checked #{:post-state-requirements-computed-before-construction
              :required-and-allowed-domain-post-state-values-match-final-plan
              :independent-binding-derived-command-ledger-excluded
              :post-state-declarations-restricted-to-registered-write-targets
              :one-expected-value-per-target-field
              :no-submission-on-post-state-mismatch}
   :assumed #{:trusted-post-state-requirements-callback
              :requirements-independent-of-constructed-plan
              :trusted-invocation-context-and-binding
              :stable-immutable-declared-and-constructed-values
              :complete-declaration-of-domain-post-state-semantics}
   :not-established #{:correct-meaning-of-declared-post-state-values
                      :authorization-or-policy-correctness
                      :independent-business-rule-discovery
                      :all-executor-branches-and-prestates
                      :xtdb-physical-refinement
                      :application-certification}})

(def outcome-contract-scope
  {:construction :gesso.model.resolved-operation/closed-command-outcome-v1
   :checked #{:outcome-requirement-computed-before-construction
              :exact-durable-outcome-equals-independently-declared-outcome
              :requires-closed-write-operation-and-post-state-contracts
              :no-submission-on-outcome-mismatch}
   :assumed #{:trusted-outcome-requirements-callback
              :requirements-independent-of-constructed-plan-and-outcome
              :trusted-invocation-context-and-binding
              :declaration-expresses-correct-business-result}
   :not-established #{:correctness-of-declared-business-result
                      :complete-business-rules-or-authorization
                      :constructor-purity
                      :all-executor-branches-and-prestates
                      :xtdb-physical-refinement
                      :application-certification}})

(defn- fail! [kind message details]
  (throw (ex-info message
                  {:error/type ::invalid-operation
                   :error/kind kind
                   :error/details details})))


;; An *optional*, trusted-context observation hook. This is deliberately not
;; part of the browser command or the operation configuration: the server may
;; install it for controlled diagnostics and disable it in normal operation.
;; No sink, including a sink managed by Gesso, is a transaction oracle.
(def observation-context-key ::observe-transaction!)

(def execution-observation-scope
  {:capture :gesso.model.resolved-operation/transaction-attempt-v1
   :observed #{:submitted-incorporation-plan-at-operation-boundary
               :command-record-and-binding-correlation
               :acknowledged-transaction-result
               :explicit-xtdb-assertion-rejection
               :classified-post-commit-delivery-failure
               :indeterminate-transaction-result}
   :assumed #{:trusted-server-injection-of-observer
              :observer-receives-events-without-loss-or-alteration
              :gesso-model-tx-transact-is-the-real-commit-boundary}
   :not-established #{:observer-event-authentication
                      :physical-transaction-id-for-every-attempt
                      :observer-event-durability
                      :automatic-before-after-xtdb-snapshot
                      :proof-that-tx-was-physically-submitted
                      :complete-application-authorization
                      :policy-guard-completeness
                      :universal-linearizability}})

;; Optional diagnostic input capture at the ORIGINAL application construction
;; call site. Never piggyback on ::observe-transaction!: its prepared/outcome
;; event protocol is strict and cannot accept a third event or extra fields.
;; The supplied sink receives SENSITIVE application context: only install it
;; in trusted, short-lived, server-local verification sessions. Its possession
;; does not establish current executor identity, physical execution, or a
;; portable proof. A missing event is never positive evidence of construction.
(def construction-input-context-key ::observe-construction-input!)

(def construction-input-observation-scope
  {:capture :gesso.model.resolved-operation/construction-input-v1
   :observed #{:context-at-actual-construction-call-site
               :canonical-command-binding-at-construction-call-site
               :only-after-unresolved-initial-submit}
   :assumed #{:trusted-server-injection-of-input-observer
              :observer-receives-and-preserves-sensitive-input-event
              :no-domain-effects-in-observation-hook}
   :not-established #{:independent-authentication-of-captured-context
                      :exact-current-live-executor-identity
                      :complete-model-input-coverage
                      :successful-domain-construction
                      :physical-submission-or-commit
                      :application-authorization-or-certification}})

(defn- construction-input-observer-for! [ctx]
  (when (and (map? ctx) (contains? ctx construction-input-context-key))
    (let [observer (get ctx construction-input-context-key)]
      (when-not (fn? observer)
        (fail! :invalid-construction-input-observer
               "Construction input capture requires a trusted function."
               {:context-key construction-input-context-key}))
      observer)))

(defn- publish-construction-input! [observer ctx binding]
  (when observer
    ;; Capturing may fail, but the diagnostic sink may NEVER suppress an
    ;; authoritative construction/transaction or change its reported outcome.
    ;; A future correlated session must reject missing/failed observations.
    (try
      (observer {:scope construction-input-observation-scope
                 :phase :construction-input
                 :operation (:operation binding)
                 :context ctx
                 :binding binding})
      (catch Throwable _
        nil))))

(defn- observer-for! [ctx]
  (when (and (map? ctx) (contains? ctx observation-context-key))
    (let [observer (get ctx observation-context-key)]
      (when-not (fn? observer)
        (fail! :invalid-observer
               "Transaction observation hook must be a trusted function."
               {:context-key observation-context-key}))
      observer)))

(defn- publish-observation! [observer event]
  ;; The observer must never become part of the authoritative transaction
  ;; boundary. In particular, a callback error *after* commit cannot turn a
  ;; completed mutation into a reported failure. Missing events therefore
  ;; mean NO evidence; consumers must verify complete paired observations.
  (when observer
    (try
      (observer (assoc event :scope execution-observation-scope))
      (catch Throwable _
        nil))))

(defn- physical-result [transaction error]
  (cond
    (and (nil? error) (= :committed (:commit/status transaction)))
    {:physical/status :committed
     :physical/source :gesso-model-transaction-ack
     :physical/receipt (:tx-result transaction)}

    (and error (live/post-commit-delivery-failure? error))
    {:physical/status :committed
     :physical/source :gesso-live-classified-post-commit-failure
     :physical/receipt (:tx-result (ex-data error))}

    (and error (= :xtdb/assert-failed
                  (:xtdb.error/code (ex-data error))))
    {:physical/status :rejected
     :physical/source :xtdb-explicit-assertion-failure}

    :else
    {:physical/status :indeterminate
     :physical/source (if error :unclassified-transaction-exception
                          :unrecognized-transaction-response)}))

(defn- transact-observed! [ctx incorporated binding observer]
  (let [attempt-id (random-uuid)
        base {:attempt-id attempt-id
              :operation (:operation binding)
              :binding binding
              :record (:record incorporated)
              :plan (:plan incorporated)}]
    ;; This event says what THIS wrapper handed to tx/transact!, not what
    ;; XTDB necessarily received. In particular, there is no fabricated
    ;; physical tx-id when a rejection has no receipt.
    (publish-observation! observer (assoc base :phase :prepared))
    (let [executed (try
                     {:transaction (tx/transact! ctx (:plan incorporated))}
                     (catch Exception exception
                       {:error exception}))
          result (physical-result (:transaction executed) (:error executed))]
      (publish-observation! observer
                            (merge base {:phase :physical-outcome} result))
      executed)))

(defn- exact-map! [value expected-keys kind]
  (when-not (and (map? value) (= expected-keys (set (keys value))))
    (fail! kind "Expected an exactly shaped resolved-operation map."
           {:expected expected-keys :received value}))
  value)

(defn- require-construction-contract [contract]
  ;; Only an operation registration made by trusted server code can opt in.
  ;; The sets describe tables, not document IDs or authorization policies.
  ;; An omitted set is never interpreted as an unconstrained wildcard.
  (when-not (and (map? contract)
                 (= construction-contract-keys (set (keys contract)))
                 (every? (fn [k]
                           (let [tables (get contract k)]
                             (and (set? tables)
                                  (every? keyword? tables))))
                         construction-contract-keys)
                 (set/subset? (:required-guard-tables contract)
                              (:allowed-guard-tables contract))
                 (set/subset? (:required-write-tables contract)
                              (:allowed-write-tables contract))
                 (contains? (:required-write-tables contract)
                            resolution/command-entity-type))
    (fail! :invalid-construction-contract
           "A closed construction contract must declare consistent guard and write table sets and require the durable command table."
           {:contract-keys construction-contract-keys}))
  contract)

(defn- require-plan-table-coverage! [contract incorporated]
  (when contract
    (let [plan (:plan incorporated)
          guards (set (map (comp first tx/guard-target) (:guards plan)))
          writes (set (map (comp first tx/command-target) (:commands plan)))
          missing-guards (set/difference (:required-guard-tables contract) guards)
          extra-guards (set/difference guards (:allowed-guard-tables contract))
          missing-writes (set/difference (:required-write-tables contract) writes)
          extra-writes (set/difference writes (:allowed-write-tables contract))]
      ;; Arbitrary HoneySQL assertions can depend on other tables which this
      ;; table-coverage check does not parse. Fail closed rather than claim a
      ;; closed footprint for plans containing opaque predicates.
      (when (or (seq (:assertions plan))
                (seq missing-guards) (seq extra-guards)
                (seq missing-writes) (seq extra-writes))
        (fail! :construction-contract-mismatch
               "The submitted transaction does not satisfy its registered closed table-coverage contract."
               {:opaque-assertions? (boolean (seq (:assertions plan)))
                :missing-guard-tables missing-guards
                :unexpected-guard-tables extra-guards
                :missing-write-tables missing-writes
                :unexpected-write-tables extra-writes}))))
  incorporated)

(defn- target-set? [targets]
  (and (set? targets)
       (every? (fn [target]
                 (and (vector? target)
                      (= 2 (count target))
                      (keyword? (first target))
                      (some? (second target))))
               targets)))

(defn- require-target-contract! [declared table-contract binding]
  ;; The application callback declares DOMAIN targets only. The durable
  ;; command-record identity is Gesso-owned and never caller-selectable.
  (let [ledger-target [resolution/command-entity-type
                       (resolution/record-id binding)]
        required-guard (:required-guard-targets declared)
        allowed-guard (:allowed-guard-targets declared)
        required-write (:required-write-targets declared)
        allowed-write (:allowed-write-targets declared)
        domain-write-tables (disj (:required-write-tables table-contract)
                                 resolution/command-entity-type)]
    (when-not (and (map? declared)
                   (= target-contract-keys (set (keys declared)))
                   (every? target-set? (vals declared))
                   (set/subset? required-guard allowed-guard)
                   (set/subset? required-write allowed-write)
                   (not-any? (fn [[table _]]
                               (= table resolution/command-entity-type))
                             (concat required-guard allowed-guard
                                     required-write allowed-write))
                   (set/subset? (set (map first allowed-guard))
                                (:allowed-guard-tables table-contract))
                   (set/subset? (set (map first allowed-write))
                                (:allowed-write-tables table-contract))
                   (set/subset? (:required-guard-tables table-contract)
                                (set (map first required-guard)))
                   (set/subset? domain-write-tables
                                (set (map first required-write))))
      (fail! :invalid-target-contract
             "Trusted document-target declaration is incomplete or inconsistent with the registered table contract."
             {:declared-shape-valid? false}))
    (-> declared
        (update :required-write-targets conj ledger-target)
        (update :allowed-write-targets conj ledger-target))))

(defn- require-plan-target-coverage! [declared incorporated]
  (when declared
    (let [plan (:plan incorporated)
          guards (set (map tx/guard-target (:guards plan)))
          writes (set (map tx/command-target (:commands plan)))
          missing-guards (set/difference (:required-guard-targets declared) guards)
          unexpected-guards (set/difference guards (:allowed-guard-targets declared))
          missing-writes (set/difference (:required-write-targets declared) writes)
          unexpected-writes (set/difference writes (:allowed-write-targets declared))]
      ;; IDs and target tuples may be sensitive. Do not include them in errors.
      (when (or (seq missing-guards) (seq unexpected-guards)
                (seq missing-writes) (seq unexpected-writes))
        (fail! :construction-target-mismatch
               "The final incorporated transaction does not match registered document-target requirements."
               {:missing-guard-target-count (count missing-guards)
                :unexpected-guard-target-count (count unexpected-guards)
                :missing-write-target-count (count missing-writes)
                :unexpected-write-target-count (count unexpected-writes)}))))
  incorporated)

(defn- check-set? [checks]
  (and (set? checks)
       (every? (fn [check]
                 (and (vector? check)
                      (= 4 (count check))
                      (keyword? (nth check 0))
                      (some? (nth check 1))
                      (keyword? (nth check 2))
                      (some? (nth check 3))))
               checks)))

(defn- check-targets [checks]
  (set (map (fn [[table id _ _]] [table id]) checks)))

(defn- require-predicate-contract! [declared targets]
  ;; This trusted callback is evaluated BEFORE :construct! and has no access
  ;; to the constructed plan. The ledger is a create, not an update version
  ;; check: domain code must never declare ledger predicates.
  (let [required-guards (:required-guard-checks declared)
        allowed-guards (:allowed-guard-checks declared)
        required-updates (:required-update-checks declared)
        allowed-updates (:allowed-update-checks declared)
        required-guard-targets (:required-guard-targets targets)
        allowed-guard-targets (:allowed-guard-targets targets)
        allowed-domain-writes (set (remove (fn [[table _]]
                                             (= table resolution/command-entity-type))
                                           (:allowed-write-targets targets)))]
    (when-not (and (map? declared)
                   (= predicate-contract-keys (set (keys declared)))
                   (every? check-set? (vals declared))
                   (set/subset? required-guards allowed-guards)
                   (set/subset? required-updates allowed-updates)
                   (set/subset? (check-targets required-guards)
                                required-guard-targets)
                   (set/subset? (check-targets allowed-guards)
                                allowed-guard-targets)
                   (set/subset? (check-targets required-updates)
                                allowed-domain-writes)
                   (set/subset? (check-targets allowed-updates)
                                allowed-domain-writes)
                   ;; Every required guarded document must carry at least one
                   ;; independently required value condition. Otherwise the
                   ;; caller could satisfy targets with a useless version check.
                   (set/subset? required-guard-targets
                                (check-targets required-guards)))
      (fail! :invalid-predicate-contract
             "Trusted exact-check requirements are incomplete or inconsistent with declared document targets."
             {:declared-shape-valid? false}))
    declared))

(defn- actual-checks [entries target-fn]
  (into #{}
        (mapcat (fn [entry]
                  (let [[table id] (target-fn entry)]
                    (map (fn [[field value]] [table id field value])
                         (get-in entry [:model/expected :model/checks])))))
        entries))

(defn- require-plan-predicate-coverage! [declared incorporated]
  (when declared
    (let [plan (:plan incorporated)
          guards (actual-checks (:guards plan) tx/guard-target)
          updates (actual-checks (filter command/update? (:commands plan))
                                 tx/command-target)
          missing-guards (set/difference (:required-guard-checks declared) guards)
          extra-guards (set/difference guards (:allowed-guard-checks declared))
          missing-updates (set/difference (:required-update-checks declared) updates)
          extra-updates (set/difference updates (:allowed-update-checks declared))]
      ;; Values, field names, and document identities may all be sensitive.
      ;; Report counts only; never echo registered or observed check tuples.
      (when (or (seq missing-guards) (seq extra-guards)
                (seq missing-updates) (seq extra-updates))
        (fail! :construction-predicate-mismatch
               "The final incorporated transaction does not match declared exact guard and update predicates."
               {:missing-guard-check-count (count missing-guards)
                :unexpected-guard-check-count (count extra-guards)
                :missing-update-check-count (count missing-updates)
                :unexpected-update-check-count (count extra-updates)}))))
  incorporated)

(defn- command-operation-set? [operations]
  (and (set? operations)
       (every? (fn [entry]
                 (and (vector? entry)
                      (= 3 (count entry))
                      (keyword? (nth entry 0))
                      (some? (nth entry 1))
                      (keyword? (nth entry 2))))
               operations)))

(defn- command-operation-targets [operations]
  (set (map (fn [[table id _]] [table id]) operations)))

(defn- require-command-operation-contract! [declared targets binding]
  ;; The trusted callback must not declare its own command-ledger operation.
  ;; Only Gesso computes the ledger target and its mandatory :create.
  (let [required (:required-command-operations declared)
        allowed (:allowed-command-operations declared)
        domain-required (disj (:required-write-targets targets)
                              [resolution/command-entity-type
                               (resolution/record-id binding)])
        domain-allowed (disj (:allowed-write-targets targets)
                             [resolution/command-entity-type
                              (resolution/record-id binding)])
        ledger-operation [resolution/command-entity-type
                          (resolution/record-id binding) :create]]
    (when-not (and (map? declared)
                   (= command-operation-contract-keys (set (keys declared)))
                   (every? command-operation-set? (vals declared))
                   (set/subset? required allowed)
                   (not-any? (fn [[table _ _]]
                               (= resolution/command-entity-type table))
                             allowed)
                   (= domain-required (command-operation-targets required))
                   (set/subset? (command-operation-targets allowed)
                                domain-allowed)
                   (= (count required)
                      (count (command-operation-targets required))))
      (fail! :invalid-command-operation-contract
             "Trusted write-operation declaration must cover exactly the independently required domain targets."
             {:declared-shape-valid? false}))
    {:required-command-operations (conj required ledger-operation)
     :allowed-command-operations (conj allowed ledger-operation)}))

(defn- require-plan-command-operation-coverage! [declared incorporated]
  (when declared
    (let [operations (set (map (fn [entry]
                                 (let [[table id] (tx/command-target entry)]
                                   [table id (command/operation entry)]))
                               (get-in incorporated [:plan :commands])))
          missing (set/difference (:required-command-operations declared)
                                  operations)
          unexpected (set/difference operations
                                     (:allowed-command-operations declared))]
      ;; Do not put command IDs, application document IDs, or operation names
      ;; into caller-visible rejection diagnostics.
      (when (or (seq missing) (seq unexpected))
        (fail! :construction-command-operation-mismatch
               "The final transaction uses a different command operation than the registered requirement."
               {:missing-command-operation-count (count missing)
                :unexpected-command-operation-count (count unexpected)}))))
  incorporated)

(defn- post-state-tuple-set? [values]
  (and (set? values)
       (every? (fn [tuple]
                 (and (vector? tuple)
                      (= 4 (count tuple))
                      (keyword? (nth tuple 0))
                      (some? (nth tuple 1))
                      (keyword? (nth tuple 2))))
               values)))

(defn- unique-post-state-fields? [tuples]
  (= (count tuples)
     (count (set (map (fn [[table id field _]] [table id field]) tuples)))))

(defn- post-state-targets [tuples]
  (set (map (fn [[table id _ _]] [table id]) tuples)))

(defn- require-post-state-contract! [declared targets]
  (let [required (:required-post-state-values declared)
        allowed (:allowed-post-state-values declared)
        required-targets (set (remove (fn [[table _]]
                                        (= table resolution/command-entity-type))
                                      (:required-write-targets targets)))
        allowed-targets (set (remove (fn [[table _]]
                                       (= table resolution/command-entity-type))
                                     (:allowed-write-targets targets)))]
    (when-not (and (map? declared)
                   (= post-state-contract-keys (set (keys declared)))
                   (post-state-tuple-set? required)
                   (post-state-tuple-set? allowed)
                   (set/subset? required allowed)
                   (unique-post-state-fields? required)
                   (unique-post-state-fields? allowed)
                   (= required-targets (post-state-targets required))
                   (set/subset? (post-state-targets allowed) allowed-targets)
                   (not-any? (fn [[table _ _ _]]
                               (= table resolution/command-entity-type)) allowed))
      (fail! :invalid-post-state-contract
             "Post-state declaration must give coherent, independently required values for each domain write target."
             {:declared-shape-valid? false}))
    declared))

(defn- actual-post-state-values [incorporated]
  (into #{}
        (mapcat (fn [entry]
                  (let [[table id] (tx/command-target entry)]
                    (when-not (= table resolution/command-entity-type)
                      (map (fn [[field value]] [table id field value])
                           (command/after entry)))))
                (get-in incorporated [:plan :commands]))))

(defn- require-plan-post-state-coverage! [declared incorporated]
  (when declared
    (let [actual (actual-post-state-values incorporated)
          missing (set/difference (:required-post-state-values declared) actual)
          unexpected (set/difference actual (:allowed-post-state-values declared))]
      (when (or (seq missing) (seq unexpected))
        (fail! :construction-post-state-mismatch
               "The final transaction's domain post-state values do not match the independently registered contract."
               {:missing-post-state-value-count (count missing)
                :unexpected-post-state-value-count (count unexpected)}))))
  incorporated)

(def ^:private stable-outcome-number-classes
  #{Byte Short Integer Long java.math.BigInteger clojure.lang.BigInt
    clojure.lang.Ratio java.math.BigDecimal Float Double})

(defn- stable-outcome-data? [value]
  ;; A command result will be persisted for replay. Do not admit mutable
  ;; Java Number implementations through Number's open inheritance hierarchy.
  ;; In particular AtomicLong can mutate between the declaration comparison
  ;; and durable record creation. Bound recursion and visited nodes as well.
  (let [visited (volatile! 0)]
    (letfn [(stable? [x depth]
              (vswap! visited inc)
              (and (<= depth 64)
                   (<= @visited 50000)
                   (cond
                     (or (nil? x) (boolean? x) (string? x) (keyword? x)
                         (uuid? x)) true
                     (contains? stable-outcome-number-classes (class x))
                     (or (not (#{Float Double} (class x)))
                         (Double/isFinite (double x)))
                     (vector? x) (every? #(stable? % (inc depth)) x)
                     (map? x) (and (every? #(stable? % (inc depth)) (keys x))
                                   (every? #(stable? % (inc depth)) (vals x)))
                     :else false)))]
      (stable? value 0))))

(defn- require-outcome-contract! [declared binding]
  ;; A deterministic outcome contract is meaningful only for operations whose
  ;; admitted result can be computed independently of their construction.
  ;; Do not leak the expected outcome (or its binding) in error diagnostics.
  (let [expected (:expected-outcome declared)]
    (when-not (and (map? declared)
                   (= outcome-contract-keys (set (keys declared)))
                   (map? expected)
                   (seq expected)
                   (stable-outcome-data? expected))
      (fail! :invalid-outcome-contract
             "Outcome contract requires exactly one nonempty expected outcome map."
             {:declared-shape-valid? false}))
    (try
      (resolution/command-record binding expected :contract-validation-time)
      (catch clojure.lang.ExceptionInfo _
        (fail! :invalid-outcome-contract
               "Outcome declaration must be a portable, durable command result."
               {:declared-shape-valid? false})))
    declared))

(defn- require-plan-outcome-coverage! [declared outcome]
  (when (and declared
             (or (not (stable-outcome-data? outcome))
                 (not= (:expected-outcome declared) outcome)))
    (fail! :construction-outcome-mismatch
           "The command result does not match the independently declared invocation outcome."
           {:outcome-match? false}))
  outcome)

(defn- require-configuration [configuration]
  (when-not (and (map? configuration)
                 (contains? #{configuration-keys
                              (conj configuration-keys construction-contract-key)
                              (conj configuration-keys construction-contract-key
                                    target-contract-key)
                              (conj configuration-keys construction-contract-key
                                    target-contract-key predicate-contract-key)
                              (conj configuration-keys construction-contract-key
                                    target-contract-key command-operation-contract-key)
                              (conj configuration-keys construction-contract-key
                                    target-contract-key predicate-contract-key
                                    command-operation-contract-key)
                              (conj configuration-keys construction-contract-key
                                    target-contract-key post-state-contract-key)
                              (conj configuration-keys construction-contract-key
                                    target-contract-key predicate-contract-key
                                    post-state-contract-key)
                              (conj configuration-keys construction-contract-key
                                    target-contract-key command-operation-contract-key
                                    post-state-contract-key)
                              (conj configuration-keys construction-contract-key
                                    target-contract-key predicate-contract-key
                                    command-operation-contract-key
                                    post-state-contract-key)}
                            (disj (set (keys configuration)) outcome-contract-key))
                 (or (not (contains? configuration outcome-contract-key))
                     (and (contains? configuration post-state-contract-key)
                          (contains? configuration command-operation-contract-key))))
    (fail! :invalid-configuration
           "Resolved operation configuration must use the exact admitted key set."
           {:expected configuration-keys
            :optional #{construction-contract-key target-contract-key predicate-contract-key
                        command-operation-contract-key post-state-contract-key
                        outcome-contract-key}}))
  (when (contains? configuration construction-contract-key)
    (require-construction-contract (:construction-contract configuration)))
  (when (and (contains? configuration target-contract-key)
             (not (fn? (:target-contract! configuration))))
    (fail! :invalid-target-contract
           "An enabled document-target contract requires a trusted function."
           {:callback target-contract-key}))
  (when (and (contains? configuration predicate-contract-key)
             (not (fn? (:predicate-contract! configuration))))
    (fail! :invalid-predicate-contract
           "An enabled predicate contract requires a trusted function."
           {:callback predicate-contract-key}))
  (when (and (contains? configuration command-operation-contract-key)
             (not (fn? (:command-operation-contract! configuration))))
    (fail! :invalid-command-operation-contract
           "An enabled command-operation contract requires a trusted function."
           {:callback command-operation-contract-key}))
  (when (and (contains? configuration post-state-contract-key)
             (not (fn? (:post-state-contract! configuration))))
    (fail! :invalid-post-state-contract
           "An enabled post-state contract requires a trusted function."
           {:callback post-state-contract-key}))
  (when (and (contains? configuration outcome-contract-key)
             (not (fn? (:outcome-contract! configuration))))
    (fail! :invalid-outcome-contract
           "An enabled outcome contract requires a trusted function."
           {:callback outcome-contract-key}))
  (let [{:keys [operation read-record! may-disclose? construct!]} configuration]
    (when-not (qualified-keyword? operation)
      (fail! :invalid-operation-id
             "A resolved operation must have a qualified semantic operation keyword."
             {:operation operation}))
    (doseq [[name f] [[:read-record! read-record!]
                      [:may-disclose? may-disclose?]
                      [:construct! construct!]]]
      (when-not (fn? f)
        (fail! :missing-function
               "A resolved operation requires all three trusted callbacks."
               {:callback name}))))
  configuration)

(defn- require-invocation [operation input]
  (exact-map! input invocation-keys :invalid-invocation)
  (let [{:keys [binding mode]} input]
    (resolution/require-binding binding)
    (when-not (= operation (:operation binding))
      (fail! :operation-mismatch
             "Command binding does not identify this admitted operation."
             {:expected operation :binding-operation (:operation binding)}))
    (when-not (contains? modes mode)
      (fail! :invalid-mode
             "Resolution mode must be an explicit, trusted :submit or :reconcile."
             {:mode mode})))
  input)

(defn- disclose [may-disclose? ctx binding]
  (fn [_binding record]
    (true? (may-disclose? ctx binding record))))

(defn- authoritative-resolution [read-record! may-disclose? ctx binding]
  (let [record (read-record! ctx (resolution/record-id binding))]
    (resolution/reconcile binding record
                          (disclose may-disclose? ctx binding))))

(defn- construction! [construct! ctx binding]
  (let [value (construct! ctx binding)]
    (exact-map! value construction-keys :invalid-construction)
    value))

(defn- returned [result]
  ;; The current operation is not itself an assurance certificate. Preserve
  ;; reconciliation's scope rather than replacing it with a stronger claim.
  (assoc result :operation-scope assurance-scope))

(defn- after-commit-error [read-record! may-disclose? ctx binding]
  ;; Reconciliation is the ONLY way to turn an ambiguous commit exception into
  ;; replay. An absent command record is not a definitive business rejection.
  (returned (authoritative-resolution read-record! may-disclose? ctx binding)))

(defn build-operation
  "Build one callable server-side authoritative-operation wrapper.

   Call it with [ctx {:binding typed-command-binding :mode :submit|:reconcile}].
   Both mode selection and the principal identity must originate from trusted
   server machinery, not unvalidated request parameters.

   For :reconcile, return a result from gesso.model.command-resolution/reconcile.
   For :submit, reconcile existing records first. Only if no record exists,
   obtain a trusted pure domain construction and commit its incorporation plan
   through gesso.model.tx/transact!. A positively acknowledged commit returns
   {:status :incorporated :outcome ... :transaction ...}. A thrown commit or a
   non-committed response is resolved using a fresh authoritative record read;
   if no record is visible its status stays :unresolved. No automatic retry.

   The trusted :construct! callback is responsible for correct authorization,
   dependency guards and truthful effects, and must never execute a domain
   mutation or external effect while constructing the plan. The application
   must register the command-result schema/table and use an authoritative,
   read-after-commit-correct :read-record! implementation.

   Optional :construction-contract checks the incorporated plan's explicit
   guard and write table sets before submission. Contract sets are closed;
   :required-* must be present, :allowed-* bounds the actual targets, and
   :gesso_commands must be required as a write. Custom HoneySQL assertions
   fail closed because they may hide undeclared table reads.

   Optional :target-contract! requires :construction-contract, takes trusted
   [ctx binding] before domain construction, and returns exactly four sets
   of required/allowed [table document-id] pairs for DOMAIN targets. Each
   required TABLE must have at least one required document target. The wrapper
   adds the exact command-ledger target derived from binding, never from the
   constructor. Optional :predicate-contract! requires both prior contracts;
   it independently declares required and permitted exact guards and update
   expected-version checks [table id field value]. One required check must
   appear for every required guard target. Optional
   :command-operation-contract! requires the table and target contracts and
   independently declares required and allowed [table id operation] triples
   for every required domain write target (e.g. :claim rather than :create).
   Gesso adds the binding-owned command-ledger :create requirement. These
   checks reject wrong command operations before tx/transact!. Optional
   :post-state-contract! also requires the table and document-target contracts
   and checks the final domain command :model/after field/value tuples against
   independently declared required and allowed post-state values. It excludes
   the Gesso-owned command ledger. Optional :outcome-contract! additionally
   requires :command-operation-contract! and :post-state-contract! and checks
   the independently declared exact durable result against :construct!'s
   outcome before transaction submission. It cannot prove that a declared
   outcome is truthful about the application's intended business semantics.
   None of these checks proves the adequacy
   of application business rules or the authenticity of acquired context."
  [configuration]
  (let [{:keys [operation read-record! may-disclose? construct!
                construction-contract target-contract! predicate-contract!
                command-operation-contract! post-state-contract!
                outcome-contract!]}
        (require-configuration configuration)]
    (with-meta
      (fn resolved-operation [ctx input]
        (let [{:keys [binding mode]} (require-invocation operation input)
              prior (authoritative-resolution read-record! may-disclose?
                                              ctx binding)]
          (cond
            ;; Never plan a domain mutation when a durable record exists,
            ;; even if the supplied binding is incompatible.
            (not= :unresolved (:status prior))
            (returned prior)

            (= :reconcile mode)
            (returned prior)

            :else
            (let [observer (observer-for! ctx)
                  input-observer (construction-input-observer-for! ctx)
                  ;; Establish independent declared identities before domain
                  ;; code builds its plan. Never source expectations from plan.
                  target-contract (when target-contract!
                                    (require-target-contract!
                                     (target-contract! ctx binding)
                                     construction-contract binding))
                  ;; The target requirements include Gesso's binding-derived
                  ;; ledger target, but only DOMAIN update predicates qualify.
                  predicate-contract (when predicate-contract!
                                       (require-predicate-contract!
                                        (predicate-contract! ctx binding)
                                        target-contract))
                  command-operation-contract
                  (when command-operation-contract!
                    (require-command-operation-contract!
                     (command-operation-contract! ctx binding)
                     target-contract binding))
                  post-state-contract
                  (when post-state-contract!
                    (require-post-state-contract!
                     (post-state-contract! ctx binding) target-contract))
                  outcome-contract
                  (when outcome-contract!
                    (require-outcome-contract!
                     (outcome-contract! ctx binding) binding))
                  _ (publish-construction-input! input-observer ctx binding)
                  {:keys [now plan outcome]} (construction! construct! ctx binding)
                  _ (require-plan-outcome-coverage! outcome-contract outcome)
                  incorporated (resolution/incorporation-plan
                                {:binding binding
                                 :now now
                                 :plan plan
                                 :outcome outcome})
                  ;; Evaluate the actual incorporated plan, not just the
                  ;; application's pre-ledger fragment. On rejection, do not
                  ;; send any transaction and do not publish :prepared.
                  _ (require-plan-table-coverage! construction-contract
                                                  incorporated)
                  _ (require-plan-target-coverage! target-contract incorporated)
                  _ (require-plan-predicate-coverage! predicate-contract incorporated)
                  _ (require-plan-command-operation-coverage!
                     command-operation-contract incorporated)
                  _ (require-plan-post-state-coverage!
                     post-state-contract incorporated)
                  {:keys [transaction error]}
                  (transact-observed! ctx incorporated binding observer)]
              (if (and (nil? error)
                       (= :committed (:commit/status transaction)))
                (returned {:status :incorporated
                           :outcome outcome
                           :transaction transaction
                           :scope resolution/assurance-scope})
                ;; A physical assertion rejection, a delivery failure after
                ;; commit, and an indeterminate exception all require the
                ;; same *authoritative* reconciliation before returning an
                ;; application-level resolution. They are NOT interchangeable
                ;; physical results. An authoritative read error propagates.
                (after-commit-error read-record! may-disclose? ctx binding))))))
      (cond-> {:gesso.model/operation-id operation
               :gesso.model/resolution :durable-command-v1
               :gesso.model/assurance-scope assurance-scope}
        construction-contract
        (assoc :gesso.model/construction-contract construction-contract
               :gesso.model/construction-contract-scope
               construction-contract-scope)
        target-contract!
        (assoc :gesso.model/target-contract-scope target-contract-scope)
        predicate-contract!
        (assoc :gesso.model/predicate-contract-scope predicate-contract-scope)
        command-operation-contract!
        (assoc :gesso.model/command-operation-contract-scope
               command-operation-contract-scope)
        post-state-contract!
        (assoc :gesso.model/post-state-contract-scope
               post-state-contract-scope)
        outcome-contract!
        (assoc :gesso.model/outcome-contract-scope
               outcome-contract-scope)))))
