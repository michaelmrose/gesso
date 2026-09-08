(ns gesso.live.optimistic.server
  "Trusted JVM/server boundary for protocol-v3 optimistic commands.

   This namespace deliberately owns much less than the protocol-v2 server
   implementation did.

   It owns:
   - decoding/validating browser command wire values;
   - deriving the authenticated principal from trusted server context;
   - declaring operation execution prerequisites and server-boundary supplied
     capabilities, with construction-time required-subset-supplied closure;
   - declaring optional per-resolution settlement contracts and checking trusted
     commit/progression evidence before settlement construction;
   - resolving a browser-proposed semantic operation through a trusted registry;
   - starting/resuming the trusted authority projection for that operation;
   - invoking the registered public model-operation adapter;
   - constructing a protocol-v3 settlement whose command/execution identities
     are copied from the validated command rather than accepted from operation
     output;
   - preparing and completing the authority projection's settlement-send
     boundary.

   It deliberately does not own:
   - browser DOM, optimistic templates, target/snapshot ownership, or continuity;
   - application authorization/business policy (the registered public operation
     must re-establish those rules from trusted context and current authority);
   - XTDB transaction construction or commit guards, or proof that an
     application-supplied commit/progression witness is truthful;
   - Live invalidation/publication;
   - HTTP response rendering;
   - conversion of arbitrary exceptions into :failed settlements.

   The last point is important: once an authoritative commit succeeds, a later
   invalidation, notification, settlement-delivery, or rendering failure must not
   be reported as though the mutation failed.  Therefore this layer never catches
   an unknown operation exception and guesses that the semantic resolution was
   :failed.  A trusted operation may explicitly return :resolution :failed only
   when it knows that classification is correct."
  (:require
   [clojure.set :as set]
   [gesso.choreo.identity :as identity]
   [gesso.choreo.machine :as machine]
   [gesso.live.optimistic.choreo :as optimistic-choreo]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.progression :as progression]))

;; =============================================================================
;; Stable public vocabulary
;; =============================================================================

(def protocol-version protocol/version)

(def operation-type
  :gesso.live.optimistic.server/operation)

(def server-type
  :gesso.live.optimistic.server/server)

(def command-boundary-type
  :gesso.live.optimistic.server/command-boundary)

(def prepared-send-type
  :gesso.live.optimistic.server/prepared-send)

(def completed-send-type
  :gesso.live.optimistic.server/completed-send)

(def authenticated-principal-capability
  "Execution-capability identity for the typed principal established by the
   trusted optimistic server before a public operation adapter may run.

   This capability is intrinsic to every prepared optimistic server: the server
   always invokes :principal-fn and rejects an untyped result before execute!.
   It describes the boundary input that is available; it does not mean the
   current principal is authorized for a model operation."
  :authenticated-principal)

(def intrinsic-server-capabilities
  "Execution capabilities supplied by the optimistic server implementation
   itself rather than declared by an application integration."
  #{authenticated-principal-capability})

(def default-operation-required-capabilities
  "Default trusted-operation prerequisites.

   Every optimistic operation runs behind typed principal binding, so the
   ordinary operation constructor records that dependency without asking each
   application to repeat it. Application-declared requirements are added to
   this intrinsic set rather than replacing it."
  #{authenticated-principal-capability})

(defn execution-capabilities?
  "True for a closed set of execution-capability keyword identities."
  [value]
  (and (set? value)
       (every? keyword? value)))

(def authoritative-progression-contract
  "Settlement-contract marker requiring trusted operation progression evidence
   to be satisfied by the settlement's authoritative basis."
  :authoritative-basis)

(def committed-status
  "The one stable successful commit-status currently recognized by settlement
   contracts. Gesso deliberately does not invent semantics for other transaction
   status vocabularies."
  :committed)

(def settlement-contract-rule-keys
  "Closed per-resolution settlement-contract rule vocabulary."
  #{:outcomes
    :commit/status
    :progression})

(def operation-result-keys
  "Closed trusted operation-result vocabulary.

   :resolution is always required. :authoritative is required by protocol v3
   for successful authoritative resolutions. :outcome remains model-specific;
   :reason is optional protocol explanation data.

   :commit/status, :progression, and :progression-advances are trusted execution
   evidence. They are never serialized into the browser settlement. They gain
   application-assembly meaning only when the prepared operation declares a
   matching :settlement-contract."
  #{:resolution
    :authoritative
    :outcome
    :reason
    :commit/status
    :progression
    :progression-advances})

;; =============================================================================
;; Errors / validation
;; =============================================================================

(defn- server-error
  [kind message data]
  (throw
   (ex-info
    message
    (merge
     {:error/type :gesso.live.optimistic.server/error
      :error/kind kind}
     data))))

(defn- require-map!
  [label value]
  (when-not (map? value)
    (server-error
     :invalid-shape
     (str label " must be a map.")
     {:label label
      :value value}))
  value)

(defn- require-keyword!
  [label value]
  (when-not (keyword? value)
    (server-error
     :invalid-keyword
     (str label " must be a keyword.")
     {:label label
      :value value}))
  value)

(defn- require-callable!
  [label value]
  (when-not (fn? value)
    (server-error
     :not-callable
     (str label " must be callable.")
     {:label label
      :value value}))
  value)

(defn- require-execution-capabilities!
  [label value]
  (when-not (execution-capabilities? value)
    (server-error
     :invalid-execution-capabilities
     (str label " must be a set of keyword capability identities.")
     {:label label
      :value value}))
  value)

(defn- require-closed-map!
  [label value required allowed]
  (let [value' (require-map! label value)
        keys' (set (keys value'))
        missing (set/difference required keys')
        unknown (set/difference keys' allowed)]
    (when (seq missing)
      (server-error
       :missing-key
       (str label " is missing required keys.")
       {:label label
        :missing missing
        :required required}))
    (when (seq unknown)
      (server-error
       :unknown-key
       (str label " contains unknown keys.")
       {:label label
        :unknown unknown
        :allowed allowed}))
    value'))

(defn- require-keyword-set!
  [label value]
  (when-not (and (set? value)
                 (seq value)
                 (every? keyword? value))
    (server-error
     :invalid-keyword-set
     (str label " must be a non-empty set of keyword identities.")
     {:label label
      :value value}))
  value)

(defn- settlement-contract-rule
  [resolution rule]
  (let [rule'
        (require-closed-map!
         (str "Optimistic settlement contract rule for " resolution)
         rule
         #{}
         settlement-contract-rule-keys)
        outcomes
        (when (contains? rule' :outcomes)
          (require-keyword-set!
           "Optimistic settlement contract :outcomes"
           (:outcomes rule')))
        commit-status
        (when (contains? rule' :commit/status)
          (require-keyword!
           "Optimistic settlement contract :commit/status"
           (:commit/status rule')))
        _
        (when (and (some? commit-status)
                   (not= committed-status commit-status))
          (server-error
           :invalid-settlement-commit-status-contract
           "Optimistic settlement contract currently recognizes only :committed commit provenance."
           {:resolution resolution
            :commit/status commit-status
            :allowed #{committed-status}}))
        progression-mode
        (when (contains? rule' :progression)
          (:progression rule'))]
    (when (and (some? progression-mode)
               (not= authoritative-progression-contract progression-mode))
      (server-error
       :invalid-settlement-progression-contract
       "Optimistic settlement contract :progression must use the supported authoritative-basis relation."
       {:resolution resolution
        :progression progression-mode
        :allowed #{authoritative-progression-contract}}))
    (cond-> {}
      outcomes
      (assoc :outcomes outcomes)

      commit-status
      (assoc :commit/status commit-status)

      progression-mode
      (assoc :progression progression-mode))))

(defn settlement-contract
  "Construct one closed trusted-operation settlement contract.

   The input is a non-empty map from generic protocol settlement resolution to
   one rule map. Only resolutions present in the contract are permitted for that
   prepared operation.

   Per-resolution rules may contain:

     :outcomes
       Non-empty set of permitted application outcome keywords. When omitted,
       the contract does not constrain the optional outcome.

     :commit/status :committed
       Require trusted evidence that the authoritative model operation crossed a
       successful commit boundary. This evidence is checked server-side and is
       not placed on the wire.

     :progression :authoritative-basis
       Require a normalized Gesso Live progression requirement in the operation
       result and require the settlement's authoritative basis to satisfy it.
       Optional :progression-advances operation-result evidence may supply exact
       trusted advancement witnesses for composed opaque requirements.

   Construction validates the contract shape; it does not prove that a trusted
   adapter tells the truth about its external transaction system."
  [value]
  (let [value' (require-map! "Optimistic settlement contract" value)]
    (when-not (seq value')
      (server-error
       :empty-settlement-contract
       "Optimistic settlement contract must contain at least one resolution rule."
       {:contract value}))
    (into {}
          (map
           (fn [[resolution rule]]
             [(protocol/normalize-settlement-resolution resolution)
              (settlement-contract-rule resolution rule)]))
          value')))

(defn settlement-contract?
  "True for one canonical trusted-operation settlement contract."
  [value]
  (try
    (= value (settlement-contract value))
    (catch Throwable _
      false)))

(defn- require-principal!
  [principal]
  (when-not (identity/principal? principal)
    (server-error
     :invalid-principal
     "Trusted principal resolver must return a typed Choreo principal identity."
     {:principal principal}))
  principal)

;; =============================================================================
;; Trusted operation registry
;; =============================================================================

(def ^:private operation-option-keys
  #{:name
    :operation
    :browser-role
    :authority-role
    :required-capabilities
    :settlement-contract
    :execute!})

(defn- operation-choreo-options
  [{:keys [name operation browser-role authority-role]}]
  {:name name
   :operation operation
   :browser-role browser-role
   :authority-role authority-role})

(defn operation
  "Construct one trusted optimistic-operation registry entry.

   Required:

     :name
       Operation-specific choreography name.

     :operation
       Public semantic model operation, e.g. :request/claim.

     :execute!
       Trusted JVM function invoked only after principal binding, registry
       resolution, command validation, and authority-projection resume.

       It receives one closed-ish trusted context map containing at least:

         :ctx
         :principal
         :command
         :command-id
         :execution-id
         :operation
         :arguments

       plus :observed-basis/:scope/:fact-versions when supplied by the command.

       Browser-supplied observed basis, scope, and arguments remain untrusted
       protocol context. The function must reread/revalidate current authority
       and invoke the public model operation that owns the transition.

       It returns a closed operation-result map containing :resolution and
       optional :authoritative, :outcome, :reason, plus trusted server-only
       execution evidence (:commit/status, :progression, and
       :progression-advances) when required by :settlement-contract. It must not
       return command-id or execution-id; this namespace supplies those identities
       from the validated command.

   :settlement-contract is optional. When present it is a closed per-resolution
   contract built by settlement-contract and checked against every result before
   settlement construction. It lets application authority adapters make stable
   commit/progression expectations inspectable without putting that evidence on
   the browser wire.

   :required-capabilities is an optional closed set of application-specific
   execution prerequisites needed by execute!.
   #{:authenticated-principal} is always added because typed principal binding
   is intrinsic to this trusted server boundary. Requirements such as
   :transaction, :authoritative-basis, :clock, or :random-seed should be declared
   here when the adapter genuinely depends on them.

   Optional :browser-role and :authority-role customize only static choreography
   roles. Neither role nor a capability declaration confers runtime
   authorization."
  [options]
  (let [options'
        (require-closed-map!
         "Optimistic server operation"
         options
         #{:name :operation :execute!}
         operation-option-keys)

        {:keys [name
                operation
                browser-role
                authority-role
                execute!]
         :or {browser-role optimistic-choreo/default-browser-role
              authority-role optimistic-choreo/default-authority-role}}
        options'

        declared-required-capabilities
        (if (contains? options' :required-capabilities)
          (require-execution-capabilities!
           "Optimistic operation :required-capabilities"
           (:required-capabilities options'))
          #{})

        required-capabilities
        (set/union
         default-operation-required-capabilities
         declared-required-capabilities)

        declared-settlement-contract
        (:settlement-contract options')

        settlement-contract'
        (when (some? declared-settlement-contract)
          (settlement-contract declared-settlement-contract))]
    (when (= browser-role authority-role)
      (server-error
       :same-role
       "Optimistic server browser and authority roles must be distinct."
       {:browser-role browser-role
        :authority-role authority-role}))
    (let [entry
          {:gesso.live.optimistic.server/type operation-type
           :name (require-keyword! "Optimistic operation :name" name)
           :operation (require-keyword! "Optimistic operation :operation" operation)
           :browser-role (require-keyword! "Optimistic operation :browser-role" browser-role)
           :authority-role (require-keyword! "Optimistic operation :authority-role" authority-role)
           :required-capabilities required-capabilities
           :settlement-contract settlement-contract'
           :execute! (require-callable! "Optimistic operation :execute!" execute!)}]
      ;; Verification/projection is registry-construction work, not request work.
      ;; Every request for this operation executes the same canonical authority
      ;; ExecutablePlan.
      (assoc entry
             :authority-plan
             (optimistic-choreo/command-plan
              (operation-choreo-options entry)
              (:authority-role entry))))))

(defn operation?
  [value]
  (and (map? value)
       (= operation-type
          (:gesso.live.optimistic.server/type value))
       (keyword? (:name value))
       (keyword? (:operation value))
       (keyword? (:browser-role value))
       (keyword? (:authority-role value))
       (not= (:browser-role value)
             (:authority-role value))
       (execution-capabilities?
        (:required-capabilities value))
       (set/subset? default-operation-required-capabilities
                    (:required-capabilities value))
       (contains? value :settlement-contract)
       (or (nil? (:settlement-contract value))
           (settlement-contract? (:settlement-contract value)))
       (fn? (:execute! value))
       (machine/executable-plan?
        (:authority-plan value))))

(defn- ensure-operation
  [value]
  (if (operation? value)
    value
    (operation value)))

(def ^:private server-option-keys
  #{:principal-fn
    :operations
    :supplied-capabilities})

(defn server
  "Construct a trusted optimistic server adapter.

   :principal-fn is server configuration, never request data. It receives ctx
   and must return a typed Choreo principal identity established from trusted
   authentication/session state.

   :operations is a map from semantic operation keyword to trusted operation
   entry/options. The map key must exactly equal the entry's :operation. A
   browser may propose :operation, but it can only select among entries already
   installed in this trusted registry.

   :supplied-capabilities is an optional application-owned set describing other
   execution prerequisites that this concrete server boundary provides to
   operation adapters. :authenticated-principal is added automatically because
   principal binding is enforced by this namespace. Declaring a capability does
   not establish model authorization and does not prove an arbitrary external
   provider correct; it is assembly data whose required/supplied relation can be
   rejected before requests execute.

   Construction fails when any registered operation requires a capability absent
   from the effective supplied set."
  [options]
  (let [options'
        (require-closed-map!
         "Optimistic server"
         options
         #{:principal-fn :operations}
         server-option-keys)
        principal-fn
        (require-callable!
         "Optimistic server :principal-fn"
         (:principal-fn options'))
        operations-raw
        (require-map!
         "Optimistic server :operations"
         (:operations options'))
        operations'
        (into {}
              (map
               (fn [[registry-key operation-value]]
                 (let [registry-key'
                       (require-keyword!
                        "Optimistic operation registry key"
                        registry-key)
                       operation'
                       (ensure-operation operation-value)]
                   (when-not (= registry-key'
                                (:operation operation'))
                     (server-error
                      :operation-registry-mismatch
                      "Optimistic operation registry key must equal the operation entry's public semantic operation."
                      {:registry-key registry-key'
                       :operation (:operation operation')}))
                   [registry-key' operation'])))
              operations-raw)

        declared-supplied-capabilities
        (if (contains? options' :supplied-capabilities)
          (require-execution-capabilities!
           "Optimistic server :supplied-capabilities"
           (:supplied-capabilities options'))
          #{})

        supplied-capabilities
        (set/union
         intrinsic-server-capabilities
         declared-supplied-capabilities)

        missing-by-operation
        (into {}
              (keep
               (fn [[operation-key operation-entry]]
                 (let [required (:required-capabilities operation-entry)
                       missing (set/difference required supplied-capabilities)]
                   (when (seq missing)
                     [operation-key
                      {:required-capabilities required
                       :missing-capabilities missing}]))))
              operations')]
    (when (seq missing-by-operation)
      (server-error
       :missing-execution-capabilities
       "Optimistic server boundary does not supply every execution capability required by its registered operations."
       {:supplied-capabilities supplied-capabilities
        :missing-by-operation missing-by-operation}))
    {:gesso.live.optimistic.server/type server-type
     :principal-fn principal-fn
     :operations operations'
     :supplied-capabilities supplied-capabilities}))

(defn server?
  [value]
  (and (map? value)
       (= server-type
          (:gesso.live.optimistic.server/type value))
       (fn? (:principal-fn value))
       (execution-capabilities?
        (:supplied-capabilities value))
       (set/subset? intrinsic-server-capabilities
                    (:supplied-capabilities value))
       (map? (:operations value))
       (every?
        (fn [[operation-key operation-entry]]
          (and (= operation-key
                  (:operation operation-entry))
               (operation? operation-entry)
               (set/subset?
                (:required-capabilities operation-entry)
                (:supplied-capabilities value))))
        (:operations value))))

(defn operation-required-capabilities
  "Return the closed execution-capability set required by a prepared trusted
   operation entry."
  [prepared-operation]
  (when-not (operation? prepared-operation)
    (server-error
     :invalid-operation
     "Expected a prepared optimistic server operation entry."
     {:operation prepared-operation}))
  (:required-capabilities prepared-operation))

(defn operation-settlement-contract
  "Return the canonical optional settlement contract for one prepared trusted
   operation entry."
  [prepared-operation]
  (when-not (operation? prepared-operation)
    (server-error
     :invalid-operation
     "Expected a prepared optimistic server operation entry."
     {:operation prepared-operation}))
  (:settlement-contract prepared-operation))

(defn server-supplied-capabilities
  "Return the effective execution-capability set supplied by a prepared trusted
   optimistic server boundary, including intrinsic Gesso capabilities."
  [prepared-server]
  (when-not (server? prepared-server)
    (server-error
     :invalid-server
     "Expected a prepared optimistic server adapter."
     {:server prepared-server}))
  (:supplied-capabilities prepared-server))

(defn- require-server!
  [value]
  (when-not (server? value)
    (server-error
     :invalid-server
     "Expected a prepared optimistic server adapter."
     {:server value}))
  value)

(defn- resolve-operation
  [prepared-server semantic-operation]
  (let [server' (require-server! prepared-server)
        operation-key
        (require-keyword!
         "Optimistic command :operation"
         semantic-operation)]
    (or (get-in server' [:operations operation-key])
        (server-error
         :unknown-operation
         "Optimistic command requested an operation not present in the trusted server registry."
         {:operation operation-key
          :registered-operations
          (set (keys (:operations server')))}))))

;; =============================================================================
;; Command decoding and trusted boundary construction
;; =============================================================================

(defn decode-command
  "Decode and validate one browser wire command.

   Successful decoding establishes shape/correlation identity only. It does not
   authenticate the principal, authorize the operation, prove observed basis,
   or establish any authoritative fact."
  [wire-command]
  (protocol/wire->command wire-command))

(defn normalize-command
  "Validate one already-decoded runtime protocol-v3 command."
  [command]
  (protocol/command
   (dissoc command protocol/protocol-version-key)))

(defn- command-message
  [operation-entry command]
  (machine/message
   (:browser-role operation-entry)
   (:authority-role operation-entry)
   protocol/command-event
   (optimistic-choreo/command-values command)
   {:via :http}))

(defn begin-command
  "Establish the trusted authority projection for one validated runtime command.

   Security-critical order:

     1. normalize the untrusted command shape;
     2. derive principal from trusted server context;
     3. resolve command :operation only through the trusted registry;
     4. require the command operation to match that registry entry;
     5. start the registry-selected authority ExecutablePlan with trusted
        principal plus typed command/execution identity bindings;
     6. resume it with only the declared command message payload;
     7. require that it reaches the expected public authoritative operation.

   Browser data cannot supply principal, role, authority, projected machine
   state, or an arbitrary callable operation."
  [prepared-server ctx command]
  (let [server'
        (require-server! prepared-server)
        command'
        (normalize-command command)
        principal
        (-> ((:principal-fn server') ctx)
            require-principal!)
        operation-entry
        (resolve-operation server'
                           (get command' protocol/operation-key))
        command''
        (optimistic-choreo/require-operation
         (:operation operation-entry)
         command')
        execution0
        (machine/start
         (:authority-plan operation-entry)
         {:identity-bindings
          {:principal principal
           :command-id (get command'' protocol/command-id-key)
           :execution-id (get command'' protocol/execution-id-key)}})
        execution1
        (machine/resume
         execution0
         (command-message operation-entry command''))
        action
        (machine/pending-action execution1)]
    (when-not (machine/waiting-authoritative? execution1)
      (server-error
       :authority-boundary-not-reached
       "Trusted optimistic authority projection did not reach an authoritative operation boundary."
       {:operation (:operation operation-entry)
        :execution (machine/explain execution1)}))
    (when-not (= (:operation operation-entry)
                 (:operation action))
      (server-error
       :unexpected-authoritative-operation
       "Trusted optimistic authority projection reached an unexpected public operation."
       {:expected-operation (:operation operation-entry)
        :actual-operation (:operation action)
        :execution (machine/explain execution1)}))
    {:gesso.live.optimistic.server/type command-boundary-type
     :server server'
     :ctx ctx
     :principal principal
     :operation-entry operation-entry
     :command command''
     :execution execution1}))

(defn command-boundary?
  [value]
  (and (map? value)
       (= command-boundary-type
          (:gesso.live.optimistic.server/type value))
       (machine/waiting-authoritative?
        (:execution value))))

(defn operation-context
  "Return the trusted context passed to a registered public operation adapter.

   This is not a portable Choreo value and may contain host/application ctx.
   Browser-supplied command fields are deliberately nested under :command and
   repeated only as convenience values; :principal is always server-derived."
  [boundary]
  (when-not (command-boundary? boundary)
    (server-error
     :invalid-command-boundary
     "Expected an optimistic command boundary waiting on authority."
     {:boundary boundary}))
  (let [{:keys [ctx principal command operation-entry]}
        boundary]
    (cond->
     {:ctx ctx
      :principal principal
      :command command
      :command-id (get command protocol/command-id-key)
      :execution-id (get command protocol/execution-id-key)
      :operation (:operation operation-entry)
      :arguments (get command protocol/arguments-key)}
      (contains? command protocol/observed-basis-key)
      (assoc :observed-basis
             (get command protocol/observed-basis-key))

      (contains? command protocol/scope-key)
      (assoc :scope
             (get command protocol/scope-key))

      (contains? command protocol/fact-versions-key)
      (assoc :fact-versions
             (get command protocol/fact-versions-key)))))

;; =============================================================================
;; Trusted operation result -> protocol-v3 settlement
;; =============================================================================

(defn- require-progression-advances!
  [value]
  (when-not (vector? value)
    (server-error
     :invalid-progression-advances
     "Optimistic operation :progression-advances evidence must be a vector."
     {:progression-advances value}))
  (mapv progression/require-advance! value))

(defn- normalize-operation-result
  [result]
  (let [result'
        (require-closed-map!
         "Optimistic authoritative operation result"
         result
         #{protocol/resolution-key}
         operation-result-keys)]
    (when (contains? result' :commit/status)
      (require-keyword!
       "Optimistic operation result :commit/status"
       (:commit/status result')))
    (when (contains? result' :progression)
      (progression/require-requirement! (:progression result')))
    (when (contains? result' :progression-advances)
      (require-progression-advances! (:progression-advances result')))
    result'))

(defn- authoritative-observation
  [result]
  (when-some [value (get result protocol/authoritative-key)]
    (protocol/authoritative
     (if (= :authoritative (get value protocol/authority-key))
       (dissoc value protocol/authority-key)
       value))))

(defn- require-settlement-contract!
  [operation-entry result]
  (when-some [contract (:settlement-contract operation-entry)]
    (let [resolution (get result protocol/resolution-key)
          rule (get contract resolution)]
      (when-not (some? rule)
        (server-error
         :settlement-resolution-not-allowed
         "Trusted operation result resolution is not permitted by its settlement contract."
         {:operation (:operation operation-entry)
          :resolution resolution
          :allowed-resolutions (set (keys contract))}))

      (when-some [outcomes (:outcomes rule)]
        (let [outcome (get result protocol/outcome-key)]
          (when-not (contains? outcomes outcome)
            (server-error
             :settlement-outcome-not-allowed
             "Trusted operation result outcome is not permitted by its settlement contract."
             {:operation (:operation operation-entry)
              :resolution resolution
              :outcome outcome
              :allowed-outcomes outcomes}))))

      (when (contains? rule :commit/status)
        (let [expected (:commit/status rule)
              actual (:commit/status result)]
          (when-not (= expected actual)
            (server-error
             :settlement-commit-status-mismatch
             "Trusted operation result does not carry the commit-status required by its settlement contract."
             {:operation (:operation operation-entry)
              :resolution resolution
              :expected expected
              :actual actual}))))

      (when (= authoritative-progression-contract (:progression rule))
        (let [requirement (:progression result)
              advances (or (:progression-advances result) [])
              authoritative (authoritative-observation result)
              basis (get authoritative protocol/basis-key)]
          (when-not (progression/requirement? requirement)
            (server-error
             :missing-settlement-progression
             "Trusted operation result is missing the progression evidence required by its settlement contract."
             {:operation (:operation operation-entry)
              :resolution resolution
              :progression requirement}))
          (when-not (some? authoritative)
            (server-error
             :missing-settlement-progression-authority
             "Settlement progression contract requires an authoritative observation."
             {:operation (:operation operation-entry)
              :resolution resolution}))
          (when-not (progression/satisfied-by? basis requirement advances)
            (server-error
             :settlement-progression-not-satisfied
             "Settlement authoritative basis does not satisfy the trusted operation progression evidence."
             {:operation (:operation operation-entry)
              :resolution resolution
              :authoritative-basis basis
              :progression requirement
              :progression-advances advances}))))))
  result)

(defn settlement-from-result
  "Construct a protocol-v3 settlement from one trusted operation result.

   command-id and execution-id are always copied from the validated command.
   The operation result cannot replace them. Successful resolutions are required
   by protocol v3 to carry a typed authoritative observation.

   This function intentionally performs no generic basis ordering/staleness
   decision. The public model operation owns the semantic interpretation of the
   browser's observed basis and current authoritative facts."
  [boundary result]
  (when-not (command-boundary? boundary)
    (server-error
     :invalid-command-boundary
     "Settlement construction requires an active authoritative command boundary."
     {:boundary boundary}))
  (let [result'
        (->> result
             normalize-operation-result
             (require-settlement-contract! (:operation-entry boundary)))
        command
        (:command boundary)]
    (protocol/settlement
     (cond->
      {protocol/command-id-key
       (get command protocol/command-id-key)

       protocol/execution-id-key
       (get command protocol/execution-id-key)

       protocol/resolution-key
       (get result' protocol/resolution-key)}

      (contains? result' protocol/authoritative-key)
      (assoc protocol/authoritative-key
             (get result' protocol/authoritative-key))

      (contains? result' protocol/outcome-key)
      (assoc protocol/outcome-key
             (get result' protocol/outcome-key))

      (contains? result' protocol/reason-key)
      (assoc protocol/reason-key
             (get result' protocol/reason-key))))))

(defn prepare-settlement-send
  "Complete the trusted authority operation and prepare its settlement send.

   The authority projection is advanced only with the one declared settlement
   semantic value. The returned participant message is validated by the
   projected Choreo send contract. The HTTP adapter may serialize :settlement
   with protocol/settlement->wire and should call complete-settlement-send only
   after accepting/performing the direct settlement send.

   Failure to deliver this prepared settlement does not rewrite the settlement
   or the already-completed authoritative operation as :failed. A later
   authoritative reread may supersede the browser's provisional trajectory."
  [boundary result]
  (when-not (command-boundary? boundary)
    (server-error
     :invalid-command-boundary
     "Settlement preparation requires an active authoritative command boundary."
     {:boundary boundary}))
  (let [settlement
        (settlement-from-result boundary result)
        execution1
        (machine/complete-authoritative
         (:execution boundary)
         {optimistic-choreo/settlement-value-key
          (optimistic-choreo/settlement-value settlement)})
        payload
        (optimistic-choreo/settlement-message-values settlement)
        message
        (machine/pending-message execution1 payload)]
    (when-not (machine/waiting-send? execution1)
      (server-error
       :settlement-send-not-reached
       "Trusted optimistic authority projection did not reach settlement send."
       {:execution (machine/explain execution1)
        :settlement settlement}))
    {:gesso.live.optimistic.server/type prepared-send-type
     :principal (:principal boundary)
     :operation (:operation (:operation-entry boundary))
     :command (:command boundary)
     :settlement settlement
     :settlement-wire (protocol/settlement->wire settlement)
     :message message
     :payload payload
     :execution execution1}))

(defn prepared-send?
  [value]
  (and (map? value)
       (= prepared-send-type
          (:gesso.live.optimistic.server/type value))
       (machine/waiting-send?
        (:execution value))))

(defn complete-settlement-send
  "Record successful direct settlement-send handoff in the authority projection.

   This is transport completion only. It never changes the already-established
   protocol settlement or authoritative model result."
  [prepared]
  (when-not (prepared-send? prepared)
    (server-error
     :invalid-prepared-send
     "Expected a prepared optimistic settlement send."
     {:prepared prepared}))
  (let [{:keys [execution message]}
        (machine/complete-send
         (:execution prepared)
         (:payload prepared))]
    (when-not (machine/completed? execution)
      (server-error
       :authority-execution-not-completed
       "Trusted optimistic authority projection did not terminate after settlement send."
       {:execution (machine/explain execution)}))
    (when-not (= (:message prepared) message)
      (server-error
       :settlement-message-changed
       "Completing optimistic settlement send produced a message different from the prepared transport envelope."
       {:prepared-message (:message prepared)
        :completed-message message}))
    (-> prepared
        (assoc :gesso.live.optimistic.server/type completed-send-type)
        (assoc :execution execution))))

(defn completed-send?
  [value]
  (and (map? value)
       (= completed-send-type
          (:gesso.live.optimistic.server/type value))
       (machine/completed?
        (:execution value))))

;; =============================================================================
;; End-to-end trusted helpers
;; =============================================================================

(defn run-command
  "Execute one already-decoded protocol-v3 command through the trusted registry.

   The registered operation is invoked exactly once with operation-context. Its
   return value must explicitly classify the semantic resolution.

   IMPORTANT: arbitrary operation exceptions are deliberately allowed to escape.
   This function never catches an unknown exception and manufactures a :failed
   settlement, because the exception may represent work that failed *after* an
   authoritative commit. The model/Live layer must distinguish pre-commit
   failure from post-commit delivery failure at its own boundary."
  [prepared-server ctx command]
  (let [boundary
        (begin-command prepared-server ctx command)
        execute!
        (get-in boundary [:operation-entry :execute!])
        result
        (execute! (operation-context boundary))]
    (prepare-settlement-send boundary result)))

(defn run-wire-command
  "Decode one browser wire command and execute it through run-command."
  [prepared-server ctx wire-command]
  (run-command
   prepared-server
   ctx
   (decode-command wire-command)))
