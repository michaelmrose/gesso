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
       ;; Optional, separate CLOSED document-identity contract. Called before
       ;; :construct! with trusted context and canonical command binding:
       :target-contract! (fn [ctx binding]
                           {:required-guard-targets #{[:memberships (:membership-id ctx)]}
                            :allowed-guard-targets #{[:memberships (:membership-id ctx)]}
                            :required-write-targets #{[:requests (get-in binding [:arguments :request-id])]}
                            :allowed-write-targets #{[:requests (get-in binding [:arguments :request-id])]}})
       ;; Optional, separate CLOSED equality-check contract. The trusted
       ;; callback runs before :construct! and declares exact expected values:
       :predicate-contract! (fn [ctx binding]
                              {:required-guard-checks #{[[:memberships (:membership-id ctx)]
                                                         :active true]}
                               :allowed-guard-checks #{[[:memberships (:membership-id ctx)]
                                                        :active true]
                                                       [[:memberships (:membership-id ctx)]
                                                        :revision (:membership-revision ctx)]}
                               :required-update-checks #{}
                               :allowed-update-checks #{}})})

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
   An additional optional :predicate-contract! checks exact declared equality
   values in the final version guards and update-command preconditions for
   those document targets. It cannot independently infer business rules or
   certify that the declarations are authentic, complete, or correct.
   None of these contracts establishes constructor purity or authorization.

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
  {:construction :gesso.model.resolved-operation/closed-expected-checks-v1
   :checked #{:predicate-requirements-computed-before-construction
              :exact-guard-and-update-expected-check-coverage
              :predicate-checks-bound-to-registered-document-targets
              :no-transaction-submission-on-predicate-mismatch}
   :assumed #{:trusted-predicate-requirements-callback
              :requirements-independent-of-constructed-plan
              :correct-expected-values-from-trusted-application-context
              :application-guards-enforce-declared-equality-semantics}
   :not-established #{:independent-authorization-policy-discovery
                      :complete-authorization-or-business-predicates
                      :authenticity-of-declared-inputs
                      :complete-semantic-dependency-closure
                      :arbitrary-sql-predicate-correspondence
                      :constructor-purity
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

(defn- exact-check-set? [checks]
  (and (set? checks)
       (every? (fn [check]
                 (and (vector? check)
                      (= 3 (count check))
                      (target-set? #{(first check)})
                      (keyword? (second check))
                      ;; gesso.model.command/expected-version disallows nil.
                      (some? (nth check 2))))
               checks)))

(defn- require-predicate-contract! [declared targets]
  ;; The declarations must originate independently of the constructed plan.
  ;; Only checks on already declared guard/write targets can be admitted.
  ;; Do not include sensitive expected values or document IDs in diagnostics.
  (let [required-guard (:required-guard-checks declared)
        allowed-guard (:allowed-guard-checks declared)
        required-update (:required-update-checks declared)
        allowed-update (:allowed-update-checks declared)]
    (when-not (and (map? declared)
                   (= predicate-contract-keys (set (keys declared)))
                   (every? exact-check-set? (vals declared))
                   (set/subset? required-guard allowed-guard)
                   (set/subset? required-update allowed-update)
                   (set/subset? (set (map first allowed-guard))
                                (:allowed-guard-targets targets))
                   (set/subset? (set (map first allowed-update))
                                (:allowed-write-targets targets))
                   ;; Update predicates refer to existing domain documents,
                   ;; not the Gesso-created command-result document.
                   (not-any? (fn [[target _ _]]
                               (= (first target) resolution/command-entity-type))
                             (concat required-guard allowed-guard
                                     required-update allowed-update)))
      (fail! :invalid-predicate-contract
             "Trusted equality-check declaration is malformed or inconsistent with its document-target contract."
             {:declared-shape-valid? false})))
  declared)

(defn- expected-check-triples [items target-f]
  (into #{}
        (mapcat (fn [item]
                  (let [target (target-f item)]
                    (map (fn [[attribute value]] [target attribute value])
                         (get-in item [:model/expected :model/checks])))))
        items))

(defn- require-plan-predicate-coverage! [declared incorporated]
  (when declared
    (let [plan (:plan incorporated)
          guard-checks (expected-check-triples (:guards plan) tx/guard-target)
          update-checks (expected-check-triples
                         (remove command/create? (:commands plan))
                         tx/command-target)
          missing-guards (set/difference (:required-guard-checks declared)
                                         guard-checks)
          extra-guards (set/difference guard-checks
                                       (:allowed-guard-checks declared))
          missing-updates (set/difference (:required-update-checks declared)
                                          update-checks)
          extra-updates (set/difference update-checks
                                        (:allowed-update-checks declared))]
      (when (or (seq missing-guards) (seq extra-guards)
                (seq missing-updates) (seq extra-updates))
        (fail! :construction-predicate-mismatch
               "The final incorporated transaction does not match registered equality-check requirements."
               {:missing-guard-check-count (count missing-guards)
                :unexpected-guard-check-count (count extra-guards)
                :missing-update-check-count (count missing-updates)
                :unexpected-update-check-count (count extra-updates)}))))
  incorporated)

(defn- require-configuration [configuration]
  (when-not (and (map? configuration)
                 (contains? #{configuration-keys
                              (conj configuration-keys construction-contract-key)
                              (conj configuration-keys construction-contract-key
                                    target-contract-key)
                              (conj configuration-keys construction-contract-key
                                    target-contract-key predicate-contract-key)}
                            (set (keys configuration))))
    (fail! :invalid-configuration
           "Resolved operation configuration must use the exact admitted key set."
           {:expected configuration-keys
            :optional #{construction-contract-key target-contract-key
                        predicate-contract-key}}))
  (when (contains? configuration construction-contract-key)
    (require-construction-contract (:construction-contract configuration)))
  (when (and (contains? configuration target-contract-key)
             (not (fn? (:target-contract! configuration))))
    (fail! :invalid-target-contract
           "An enabled document-target contract requires a trusted function."
           {:callback target-contract-key}))
  (when (contains? configuration predicate-contract-key)
    (when-not (fn? (:predicate-contract! configuration))
      (fail! :invalid-predicate-contract
             "An enabled equality-check contract requires a trusted function and a document-target contract."
             {:callback predicate-contract-key})))
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
   constructor. Neither layer independently discovers business requirements.

   Optional :predicate-contract! requires :target-contract! and declares four
   required/allowed sets of [target attribute expected-value] triples. It is
   evaluated from trusted [ctx binding] BEFORE :construct!, and compares
   the final version guards and update command preconditions. All predicates
   remain plain, canonical expected-version equalities; arbitrary HoneySQL
   expressions are not admitted. Correct predicates still cannot establish
   that all application authorization requirements were declared."
  [configuration]
  (let [{:keys [operation read-record! may-disclose? construct!
                construction-contract target-contract! predicate-contract!]}
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
                  predicate-contract (when predicate-contract!
                                       (require-predicate-contract!
                                        (predicate-contract! ctx binding)
                                        target-contract))
                  _ (publish-construction-input! input-observer ctx binding)
                  {:keys [now plan outcome]} (construction! construct! ctx binding)
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
        (assoc :gesso.model/predicate-contract-scope predicate-contract-scope)))))
