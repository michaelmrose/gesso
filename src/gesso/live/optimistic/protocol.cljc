(ns gesso.live.optimistic.protocol
  "Portable optimistic protocol-v3 vocabulary shared by Clojure and
   ClojureScript.

   This namespace owns the semantic browser/server wire envelopes for an
   optimistic command.  It deliberately does not own DOM markup, HTMX request
   construction, continuity resources, timers, browser target generations,
   authentication, authorization, model transition policy, or XTDB-specific
   serialization.

   Protocol v3 follows the v4.5 choreography design:

   - command-id identifies one semantic command/intention;
   - execution-id identifies one concrete protocol execution and is distinct
     from command-id;
   - an optimistic projection is explicit provisional knowledge derived from a
     known authoritative basis and command-id;
   - authoritative observations explicitly distinguish presence from absence;
   - settlements are typed resolutions correlated to both command-id and
     execution-id;
   - optional model-specific fact versions remain separate from the
     authoritative database basis;
   - opaque authoritative basis values are never ordered or compared here.

   Shape validation is not authority.  In particular, decoding a well-formed
   command, basis, provisional value, or settlement never authenticates a
   principal, authorizes an operation, establishes provenance, or upgrades
   provisional state into authoritative state."
  (:require
   [clojure.string :as str]
   [gesso.choreo.identity :as identity]
   [gesso.choreo.type :as type]))

;; =============================================================================
;; Protocol identity and message kinds
;; =============================================================================

(def version
  "Optimistic browser/server wire protocol version."
  "3")

(def protocol-version-key :protocol-version)

(def command-event :optimistic/command)
(def provisional-event :optimistic/provisional)
(def settlement-event :optimistic/settlement)
(def authoritative-event :optimistic/authoritative)

;; =============================================================================
;; Public keys and closed envelope contracts
;; =============================================================================

(def command-id-key :command-id)
(def execution-id-key :execution-id)
(def operation-key :operation)
(def arguments-key :arguments)
(def observed-basis-key :observed-basis)
(def scope-key :scope)
(def fact-versions-key :fact-versions)

(def authority-key :authority)
(def projection-key :projection)

(def presence-key :presence)
(def basis-key :basis)

(def resolution-key :resolution)
(def authoritative-key :authoritative)
(def outcome-key :outcome)
(def reason-key :reason)

(def command-required-keys
  #{protocol-version-key
    command-id-key
    execution-id-key
    operation-key
    arguments-key})

(def command-optional-keys
  #{observed-basis-key
    scope-key
    fact-versions-key})

(def provisional-required-keys
  #{protocol-version-key
    authority-key
    command-id-key
    execution-id-key
    observed-basis-key
    projection-key})

(def provisional-optional-keys
  #{scope-key
    fact-versions-key})

(def authoritative-required-keys
  #{authority-key
    presence-key
    basis-key})

(def authoritative-optional-keys
  #{projection-key
    fact-versions-key})

(def settlement-required-keys
  #{protocol-version-key
    command-id-key
    execution-id-key
    resolution-key})

(def settlement-optional-keys
  #{authoritative-key
    outcome-key
    reason-key})

(def command-correlation-keys
  #{command-id-key execution-id-key})

(def settlement-correlation-keys
  #{command-id-key execution-id-key})

;; =============================================================================
;; Semantic vocabulary
;; =============================================================================

(def authoritative-presences
  "Closed authoritative projection presence vocabulary.

   :present means authority establishes that the projection exists.
   :absent means authority establishes a tombstone/absence.  Absence is an
   authoritative observation, not a missing-target error."
  #{:present :absent})

(def settlement-resolutions
  "Generic protocol resolution classes.

   Application/model outcomes may be carried separately in :outcome.  These
   classes describe how an optimistic trajectory relates to trusted authority,
   not the complete business-domain result vocabulary."
  #{:confirmed
    :reconciled
    :rejected
    :already-incorporated
    :failed})

(def authoritative-required-resolutions
  "Settlement resolutions that must carry an authoritative observation.

   Rejection may intentionally omit protected authoritative state, for example
   when authorization fails.  :failed is a trusted operation/protocol failure
   and is not a synonym for post-commit notification or delivery failure."
  #{:confirmed
    :reconciled
    :already-incorporated})

(def provisional-resolution-kinds
  "All generic ways an optimistic trajectory may finish locally.

   :superseded is intentionally not a settlement resolution: it can be learned
   from a later authoritative reread even when the original settlement is lost."
  (conj settlement-resolutions :superseded))

;; =============================================================================
;; Errors and closed-map helpers
;; =============================================================================

(defn- protocol-error
  [kind message data]
  (throw
   (ex-info
    message
    (merge
     {:error/type :gesso.live.optimistic.protocol/error
      :error/kind kind
      :protocol/version version}
     data))))

(defn- require-map!
  [label value]
  (when-not (map? value)
    (protocol-error
     :invalid-shape
     (str label " must be a map.")
     {:label label
      :value value}))
  value)

(defn- require-closed-map!
  [label value required optional]
  (require-map! label value)
  (let [keys' (set (keys value))
        allowed (into required optional)
        missing (set (remove keys' required))
        extra (set (remove allowed keys'))]
    (when (seq missing)
      (protocol-error
       :missing-fields
       (str label " is missing required protocol fields.")
       {:label label
        :missing missing
        :required required
        :value value}))
    (when (seq extra)
      (protocol-error
       :unknown-fields
       (str label " contains unknown protocol fields.")
       {:label label
        :unknown extra
        :allowed allowed
        :value value})))
  value)

(defn- require-version!
  [envelope]
  (let [actual (get envelope protocol-version-key)]
    (when-not (= version actual)
      (protocol-error
       :unsupported-version
       "Unsupported Gesso optimistic protocol version."
       {:expected version
        :actual actual})))
  envelope)

(defn- non-blank-string?
  [value]
  (and (string? value)
       (not (str/blank? value))))

(defn qualified-name
  "Return a stable textual semantic name while preserving keyword namespaces."
  [value]
  (cond
    (keyword? value)
    (if-some [namespace' (namespace value)]
      (str namespace' "/" (name value))
      (name value))

    (symbol? value)
    (str value)

    (nil? value)
    nil

    :else
    (str value)))

(defn normalize-name
  "Normalize one required semantic name to a non-blank string."
  [key value]
  (let [value' (qualified-name value)]
    (when-not (non-blank-string? value')
      (protocol-error
       :invalid-name
       "Gesso optimistic protocol name must not be blank."
       {:key key
        :value value}))
    value'))

(defn normalize-optional-name
  "Normalize one optional semantic name."
  [key value]
  (when (some? value)
    (normalize-name key value)))

;; =============================================================================
;; Typed command/execution identities and their wire forms
;; =============================================================================

(defn require-command-id
  "Validate and return one typed semantic command identity."
  [value]
  (when-not (identity/command-id? value)
    (protocol-error
     :invalid-command-id
     "Optimistic command-id must be a typed Choreo command identity."
     {:value value}))
  value)

(defn require-execution-id
  "Validate and return one typed concrete execution identity."
  [value]
  (when-not (identity/execution-id? value)
    (protocol-error
     :invalid-execution-id
     "Optimistic execution-id must be a typed Choreo execution identity."
     {:value value}))
  value)

(defn command-id->wire
  "Encode one semantic command identity with the canonical Choreo identity wire
   representation."
  [value]
  (identity/encode-wire
   (require-command-id value)))

(defn execution-id->wire
  "Encode one concrete execution identity with the canonical Choreo identity
   wire representation."
  [value]
  (identity/encode-wire
   (require-execution-id value)))

(defn wire->command-id
  "Decode and require one command-id wire value.

   An execution-id encoded with the same raw scalar is rejected rather than
   becoming interchangeable with command identity."
  [wire]
  (require-command-id
   (identity/decode-wire wire)))

(defn wire->execution-id
  "Decode and require one execution-id wire value."
  [wire]
  (require-execution-id
   (identity/decode-wire wire)))

;; =============================================================================
;; Basis, scope, and model-specific fact-version helpers
;; =============================================================================

(defn normalize-basis
  "Require one opaque authoritative basis.

   The basis is deliberately not compared here.  XTDB or another authority
   owns progression/consistency semantics; the browser must not invent ordering
   for opaque basis values."
  [key basis]
  (when (nil? basis)
    (protocol-error
     :missing-basis
     "Optimistic authoritative basis is required."
     {:key key
      :basis basis}))
  basis)

(defn normalize-optional-basis
  "Normalize an optional authoritative basis without inventing comparison
   semantics."
  [key basis]
  (when (some? basis)
    (normalize-basis key basis)))

(defn normalize-scope
  "Normalize an optional application projection scope.

   Protocol v3 carries scope as ordinary portable data and relies on the
   surrounding serializer (for example Transit) to preserve its type.  Scope is
   equality/correlation data only; it never grants authority."
  [scope]
  (when (some? scope)
    (when (and (string? scope)
               (str/blank? scope))
      (protocol-error
       :invalid-scope
       "Gesso optimistic scope must not be blank."
       {:scope scope}))
    scope))

(defn wire-scope
  "Return the protocol-v3 wire scope value.

   Unlike protocol v2, v3 does not stringify/tag scope values.  The enclosing
   portable serializer preserves the value and therefore avoids lossy or
   double-encoded scope identities."
  [scope]
  (normalize-scope scope))

(defn normalize-fact-versions
  "Validate optional model-specific fact/version metadata.

   Fact versions are intentionally separate from :observed-basis/:basis.  They
   are opaque to Gesso and have no generic ordering semantics.  Keys are
   semantic keyword names; non-nil values are model-owned version identities."
  [fact-versions]
  (when (some? fact-versions)
    (when-not (map? fact-versions)
      (protocol-error
       :invalid-fact-versions
       "Optimistic fact-versions must be a map when supplied."
       {:fact-versions fact-versions}))
    (doseq [[fact-key fact-version] fact-versions]
      (when-not (keyword? fact-key)
        (protocol-error
         :invalid-fact-version-key
         "Optimistic fact-version keys must be keywords."
         {:fact-key fact-key
          :fact-version fact-version}))
      (when (nil? fact-version)
        (protocol-error
         :invalid-fact-version
         "Optimistic fact-version values must not be nil."
         {:fact-key fact-key
          :fact-version fact-version})))
    fact-versions))

;; =============================================================================
;; Runtime semantic envelopes
;; =============================================================================

(defn command
  "Construct and validate one protocol-v3 command envelope.

   command-id is the semantic intention and execution-id is one concrete
   protocol attempt.  :observed-basis is optional at the command level because
   commands can exist without optimism; any provisional projection derived from
   the command must carry a basis.

   The operation and arguments are untrusted request data until the trusted
   server authenticates the principal, selects/authorizes the operation, rereads
   authority, and invokes the model's public operation."
  [opts]
  (require-closed-map!
   "Optimistic command"
   opts
   (disj command-required-keys protocol-version-key)
   command-optional-keys)
  (let [{:keys [command-id execution-id operation arguments
                observed-basis scope fact-versions]}
        opts
        semantic-command
        (type/command
         (cond->
          {:command-id (require-command-id command-id)
           :operation operation
           :arguments arguments}
           (some? observed-basis)
           (assoc :observed-basis
                  (normalize-basis :observed-basis observed-basis))))]
    (cond->
     {protocol-version-key version
      command-id-key (:command-id semantic-command)
      execution-id-key (require-execution-id execution-id)
      operation-key (:operation semantic-command)
      arguments-key (:arguments semantic-command)}
      (contains? semantic-command :observed-basis)
      (assoc observed-basis-key (:observed-basis semantic-command))

      (some? scope)
      (assoc scope-key (normalize-scope scope))

      (some? fact-versions)
      (assoc fact-versions-key
             (normalize-fact-versions fact-versions)))))

(defn provisional
  "Construct and validate explicit provisional knowledge for one execution.

   This is never authoritative state.  Every provisional projection requires a
   typed semantic command-id and a known authoritative basis, exactly as v4.5
   requires."
  [opts]
  (require-closed-map!
   "Optimistic provisional value"
   opts
   (disj provisional-required-keys protocol-version-key authority-key)
   provisional-optional-keys)
  (let [{:keys [command-id execution-id observed-basis projection
                scope fact-versions]}
        opts
        provisional-value
        (type/provisional-value
         {:authority :provisional
          :command-id (require-command-id command-id)
          :observed-basis (normalize-basis :observed-basis observed-basis)
          :projection projection})]
    (cond->
     {protocol-version-key version
      authority-key :provisional
      command-id-key (:command-id provisional-value)
      execution-id-key (require-execution-id execution-id)
      observed-basis-key (:observed-basis provisional-value)
      projection-key (:projection provisional-value)}
      (some? scope)
      (assoc scope-key (normalize-scope scope))

      (some? fact-versions)
      (assoc fact-versions-key
             (normalize-fact-versions fact-versions)))))

(defn normalize-presence
  "Validate one authoritative presence/absence marker."
  [presence]
  (when-not (contains? authoritative-presences presence)
    (protocol-error
     :invalid-authoritative-presence
     "Invalid optimistic authoritative presence marker."
     {:presence presence
      :allowed authoritative-presences}))
  presence)

(defn authoritative
  "Construct one authoritative projection observation.

   :present requires an explicit :projection key, whose value may itself be nil
   when nil is meaningful application data.  :absent forbids :projection and is
   an authoritative tombstone rather than an error.

   Constructing this shape does not establish that its basis or projection is
   truthful; only a trusted authority boundary may supply it as authoritative."
  [opts]
  (require-closed-map!
   "Optimistic authoritative observation"
   opts
   #{presence-key basis-key}
   #{projection-key fact-versions-key})
  (let [{:keys [presence basis projection fact-versions]}
        opts
        presence' (normalize-presence presence)
        projection-present? (contains? opts projection-key)]
    (when (and (= :present presence')
               (not projection-present?))
      (protocol-error
       :missing-authoritative-projection
       "Authoritative presence requires an explicit projection."
       {:presence presence'}))
    (when (and (= :absent presence')
               projection-present?)
      (protocol-error
       :projection-on-authoritative-absence
       "Authoritative absence must not carry a projection."
       {:presence presence'
        :projection projection}))
    (cond->
     {authority-key :authoritative
      presence-key presence'
      basis-key (normalize-basis :basis basis)}
      projection-present?
      (assoc projection-key projection)

      (some? fact-versions)
      (assoc fact-versions-key
             (normalize-fact-versions fact-versions)))))

(defn normalize-settlement-resolution
  "Validate one generic settlement resolution class."
  [resolution]
  (when-not (contains? settlement-resolutions resolution)
    (protocol-error
     :invalid-settlement-resolution
     "Invalid Gesso optimistic settlement resolution."
     {:resolution resolution
      :allowed settlement-resolutions}))
  resolution)

(defn settlement
  "Construct one trusted protocol-v3 settlement.

   :confirmed, :reconciled, and :already-incorporated require a typed
   authoritative observation.  :rejected may omit authoritative state when
   returning it would be inappropriate (for example, failed authorization).
   :failed may also omit authority and MUST NOT be used to disguise a mutation
   that already committed but whose later invalidation/settlement delivery
   failed.

   :outcome is an optional model/choreography continuation keyword distinct from
   the generic :resolution class."
  [opts]
  (require-closed-map!
   "Optimistic settlement"
   opts
   (disj settlement-required-keys protocol-version-key)
   settlement-optional-keys)
  (let [{:keys [command-id execution-id resolution outcome reason]}
        opts
        authoritative-value (get opts authoritative-key)
        resolution' (normalize-settlement-resolution resolution)
        authoritative'
        (when (some? authoritative-value)
          (authoritative
           (if (= :authoritative (get authoritative-value authority-key))
             (dissoc authoritative-value authority-key)
             authoritative-value)))]
    (when (and (contains? authoritative-required-resolutions resolution')
               (nil? authoritative'))
      (protocol-error
       :missing-settlement-authority
       "Settlement resolution requires an authoritative observation."
       {:resolution resolution'
        :required-for authoritative-required-resolutions}))
    (when (and (some? outcome)
               (not (keyword? outcome)))
      (protocol-error
       :invalid-settlement-outcome
       "Optimistic settlement outcome must be a keyword when supplied."
       {:outcome outcome}))
    (cond->
     {protocol-version-key version
      command-id-key (require-command-id command-id)
      execution-id-key (require-execution-id execution-id)
      resolution-key resolution'}
      authoritative'
      (assoc authoritative-key authoritative')

      (some? outcome)
      (assoc outcome-key outcome)

      (some? reason)
      (assoc reason-key
             (normalize-name :reason reason)))))

(defn command-provisional-pair
  "Validate that command-envelope and provisional-envelope describe one
   optimistic semantic command/execution and basis.

   This function is intentionally equality-based.  It never invents ordering or
   advancement semantics for authoritative bases."
  [command-envelope provisional-envelope]
  (let [command' (command
                  (dissoc command-envelope protocol-version-key))
        provisional' (provisional
                      (dissoc provisional-envelope
                              protocol-version-key
                              authority-key))]
    (doseq [[key left right]
            [[command-id-key
              (get command' command-id-key)
              (get provisional' command-id-key)]
             [execution-id-key
              (get command' execution-id-key)
              (get provisional' execution-id-key)]]]
      (when-not (= left right)
        (protocol-error
         :correlation-mismatch
         "Optimistic command and provisional value do not correlate."
         {:key key
          :command left
          :provisional right})))
    (when-not (contains? command' observed-basis-key)
      (protocol-error
       :command-missing-observed-basis
       "An optimistic command paired with provisional state must carry observed-basis."
       {:command command'}))
    (when-not (= (get command' observed-basis-key)
                 (get provisional' observed-basis-key))
      (protocol-error
       :basis-mismatch
       "Optimistic command and provisional value must share the same observed basis."
       {:command-basis (get command' observed-basis-key)
        :provisional-basis (get provisional' observed-basis-key)}))
    {:command command'
     :provisional provisional'}))

;; =============================================================================
;; Wire encoding/decoding
;; =============================================================================

(defn- encode-correlated-identities
  [envelope]
  (-> envelope
      (update command-id-key command-id->wire)
      (update execution-id-key execution-id->wire)))

(defn- decode-correlated-identities
  [envelope]
  (-> envelope
      (update command-id-key wire->command-id)
      (update execution-id-key wire->execution-id)))

(defn command->wire
  "Validate and encode a runtime command envelope for transport.

   Only command-id/execution-id receive protocol-specific transformation here.
   The surrounding HTTP/Transit/JSON layer remains responsible for serializing
   the resulting portable data structure."
  [command-envelope]
  (encode-correlated-identities
   (command
    (dissoc command-envelope protocol-version-key))))

(defn wire->command
  "Decode one closed protocol-v3 command wire envelope."
  [wire]
  (require-version!
   (require-closed-map!
    "Optimistic command wire envelope"
    wire
    command-required-keys
    command-optional-keys))
  (let [decoded (decode-correlated-identities wire)]
    (command
     (dissoc decoded protocol-version-key))))

(defn provisional->wire
  "Validate and encode explicit provisional knowledge for transport."
  [provisional-envelope]
  (encode-correlated-identities
   (provisional
    (dissoc provisional-envelope
            protocol-version-key
            authority-key))))

(defn wire->provisional
  "Decode one closed protocol-v3 provisional wire envelope."
  [wire]
  (require-version!
   (require-closed-map!
    "Optimistic provisional wire envelope"
    wire
    provisional-required-keys
    provisional-optional-keys))
  (when-not (= :provisional (get wire authority-key))
    (protocol-error
     :invalid-provisional-authority
     "Optimistic provisional wire envelope must be explicitly provisional."
     {:authority (get wire authority-key)}))
  (let [decoded (decode-correlated-identities wire)]
    (provisional
     (dissoc decoded protocol-version-key authority-key))))

(defn authoritative->wire
  "Validate an authoritative observation for transport.

   No identity transformation is required; the explicit authority/presence/basis
   vocabulary is already portable data."
  [authoritative-observation]
  (authoritative
   (dissoc authoritative-observation authority-key)))

(defn wire->authoritative
  "Decode one authoritative presence/absence observation."
  [wire]
  (require-map! "Optimistic authoritative wire observation" wire)
  (when-not (= :authoritative (get wire authority-key))
    (protocol-error
     :invalid-authoritative-authority
     "Optimistic authoritative wire observation must be explicitly authoritative."
     {:authority (get wire authority-key)}))
  (authoritative
   (dissoc wire authority-key)))

(defn settlement->wire
  "Validate and encode one trusted settlement for transport."
  [settlement-envelope]
  (let [settlement'
        (settlement
         (dissoc settlement-envelope protocol-version-key))]
    (cond->
     (encode-correlated-identities settlement')
      (contains? settlement' authoritative-key)
      (update authoritative-key authoritative->wire))))

(defn wire->settlement
  "Decode one closed protocol-v3 settlement wire envelope."
  [wire]
  (require-version!
   (require-closed-map!
    "Optimistic settlement wire envelope"
    wire
    settlement-required-keys
    settlement-optional-keys))
  (let [decoded
        (cond->
         (decode-correlated-identities wire)
          (contains? wire authoritative-key)
          (update authoritative-key wire->authoritative))]
    (settlement
     (dissoc decoded protocol-version-key))))
