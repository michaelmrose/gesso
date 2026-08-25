(ns gesso.live.optimistic.server
  "Trusted JVM/server boundary for protocol-v3 optimistic commands.

   This namespace deliberately owns much less than the protocol-v2 server
   implementation did.

   It owns:
   - decoding/validating browser command wire values;
   - deriving the authenticated principal from trusted server context;
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
   - XTDB transaction construction or commit guards;
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
   [gesso.live.optimistic.protocol :as protocol]))

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

(def operation-result-keys
  "Closed trusted operation-result vocabulary.

   :resolution is always required. :authoritative is required by protocol v3
   for successful authoritative resolutions. :outcome remains model-specific;
   :reason is optional protocol explanation data."
  #{:resolution
    :authoritative
    :outcome
    :reason})

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
       optional :authoritative, :outcome, and :reason. It must not return
       command-id or execution-id; this namespace supplies those identities from
       the validated command.

   Optional :browser-role and :authority-role customize only static choreography
   roles. They do not confer runtime authorization."
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
        options']
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
    :operations})

(defn server
  "Construct a trusted optimistic server adapter.

   :principal-fn is server configuration, never request data. It receives ctx
   and must return a typed Choreo principal identity established from trusted
   authentication/session state.

   :operations is a map from semantic operation keyword to trusted operation
   entry/options. The map key must exactly equal the entry's :operation. A
   browser may propose :operation, but it can only select among entries already
   installed in this trusted registry."
  [options]
  (let [options'
        (require-closed-map!
         "Optimistic server"
         options
         server-option-keys
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
              operations-raw)]
    {:gesso.live.optimistic.server/type server-type
     :principal-fn principal-fn
     :operations operations'}))

(defn server?
  [value]
  (and (map? value)
       (= server-type
          (:gesso.live.optimistic.server/type value))
       (fn? (:principal-fn value))
       (map? (:operations value))
       (every?
        (fn [[operation-key operation-entry]]
          (and (= operation-key
                  (:operation operation-entry))
               (operation? operation-entry)))
        (:operations value))))

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

(defn- normalize-operation-result
  [result]
  (require-closed-map!
   "Optimistic authoritative operation result"
   result
   #{protocol/resolution-key}
   operation-result-keys))

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
        (normalize-operation-result result)
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
