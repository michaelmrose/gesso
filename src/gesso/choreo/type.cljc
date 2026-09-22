(ns gesso.choreo.type
  "Portable Malli-backed type vocabulary for Gesso choreography.

   This namespace supplies the ordinary value-shape layer required by the v4.5
   Choreo design. It deliberately sits below semantic verification and trust:
   satisfying one of these schemas establishes only that a value has the
   promised shape.

   In particular, Malli validation here does not establish that:

   - a browser-supplied principal, actor, authority, basis, or provenance claim
     is trusted;
   - an authenticated principal is authorized for an operation;
   - an authoritative basis advances another basis;
   - communication or observation actually established knowledge;
   - a model transition is legal;
   - a projected execution refines the global choreography.

   Those remain identity-binding, knowledge/provenance, model, progression,
   verifier/proof, and trusted-boundary concerns.

   The vocabulary intentionally reuses existing canonical Choreo data rather
   than introducing parallel representations:

   - roles and runtime identities use gesso.choreo.identity;
   - provenance and role-local fact records use gesso.choreo.knowledge;
   - authoritative basis values remain opaque non-nil values whose ordering is
     supplied by the authority/consistency layer;
   - outcomes remain semantic keywords, matching portable :return states;
   - command and provisional-value envelopes follow the semantic shapes in the
     v4.5 design and are not wire/authentication formats.

   Validators for named schemas are compiled once at namespace load and reused.
   Rich Malli explanation data is produced only on failure or explicit request.

   This namespace contains no HTMX, DOM, browser resources, XTDB representation,
   authentication, authorization, transport, optimism policy, or application
   model behavior."
  (:require
   [gesso.choreo.identity :as identity]
   [gesso.choreo.knowledge :as knowledge]
   [malli.core :as m]))

;; =============================================================================
;; Errors
;; =============================================================================

(defn- type-error
  [kind message data]
  (throw
   (ex-info
    message
    (merge
     {:error/type :gesso.choreo.type/error
      :error/kind kind}
     data))))

;; =============================================================================
;; Primitive semantic vocabulary
;; =============================================================================

(def Role
  "A static participant position in a choreography."
  [:fn
   {:error/message "must be a choreography role keyword"}
   identity/role?])

(def Principal
  "A tagged runtime principal identity.

   Shape does not authenticate the principal."
  [:fn
   {:error/message "must be a Choreo principal identity"}
   identity/principal?])

(def Actor
  "A tagged runtime actor identity.

   Actor identity is distinct from principal, role, authority, and host."
  [:fn
   {:error/message "must be a Choreo actor identity"}
   identity/actor?])

(def Host
  "A tagged physical/runtime host identity.

   Host identity never implies semantic authority."
  [:fn
   {:error/message "must be a Choreo host identity"}
   identity/host?])

(def Authority
  "A tagged runtime authority identity.

   Shape alone does not grant authority."
  [:fn
   {:error/message "must be a Choreo authority identity"}
   identity/authority?])

(def AuthorityName
  "A static semantic authority label used by portable choreography declarations.

   Current authored/projection contracts name logical authorities with keywords;
   concrete runtime authority identity, when needed, uses Authority instead."
  keyword?)

(def CommandId
  "Identity of one semantic command/intention."
  [:fn
   {:error/message "must be a Choreo command identity"}
   identity/command-id?])

(def ExecutionId
  "Identity of one concrete choreography execution attempt."
  [:fn
   {:error/message "must be a Choreo execution identity"}
   identity/execution-id?])

(def built-in-trust-classes
  "Trust labels used directly by the current v4.5 threat model.

   This set is descriptive rather than a closed registry. Applications or later
   Choreo analysis may introduce more precise keyword trust classes without
   changing the portable shape vocabulary."
  #{:trusted
    :untrusted})

(def TrustClass
  "Shape of one portable trust-class label.

   v4.5 requires explicit trust classes but does not freeze a universal enum, so
   the portable vocabulary deliberately remains open to keyword classes. The
   built-in threat model currently uses :trusted and :untrusted."
  keyword?)

(def FactKey
  "A semantic key naming one fact/value in portable role-local knowledge."
  keyword?)

(def Provenance
  "One structurally valid Choreo provenance record.

   This delegates to knowledge/provenance? and therefore validates shape only."
  [:fn
   {:error/message "must be a structurally valid Choreo provenance record"}
   knowledge/provenance?])

(def Basis
  "Opaque authoritative/progression basis value.

   The generic Choreo vocabulary intentionally does not prescribe XTDB or any
   other storage representation. A basis must be present; the authority-specific
   progression layer owns comparison/ordering semantics."
  some?)

(def Outcome
  "A semantic continuation/result label.

   Portable choreography :return states currently use keywords. Application
   result data belongs in separately typed values rather than being smuggled
   into the outcome identity."
  keyword?)

;; =============================================================================
;; Existing canonical fact/provenance structures
;; =============================================================================

(def Fact
  "Shape of one current role-local knowledge fact record.

   This is the structure already stored under gesso.choreo.knowledge/:facts:

     {:value ...
      :provenance [...]}

   The fact key is supplied by the containing knowledge map and is therefore not
   duplicated inside the record. At least one provenance justification is
   required."
  [:map
   {:closed true}
   [:value any?]
   [:provenance
    [:vector
     {:min 1}
     Provenance]]])

(def AuthoritativeBasisProgression
  "Shape-only schema for an authority-supplied basis progression decision.

   Validation does not prove that the declared relation is truthful."
  [:fn
   {:error/message "must be a structurally valid authoritative basis progression"}
   knowledge/authoritative-basis-progression?])

;; =============================================================================
;; Semantic command/provisional vocabulary
;; =============================================================================

(def Command
  "Portable semantic command request.

   command-id identifies the semantic intention and is distinct from a protocol
   execution-id. operation names the trusted server-side semantic operation;
   arguments are untrusted/request data until the authoritative side validates
   and authorizes them. observed-basis is optional protocol context and never
   establishes authority merely because it has this shape.

   This is a semantic envelope, not a browser/server wire encoding and not an
   authorization capability."
  [:map
   {:closed true}
   [:command-id CommandId]
   [:operation keyword?]
   [:arguments map?]
   [:observed-basis
    {:optional true}
    Basis]])

(def ProvisionalValue
  "Explicit provisional knowledge associated with one semantic command/basis.

   This follows the v4.5 conceptual shape:

     {:authority :provisional
      :command-id C42
      :observed-basis B
      :projection P}

   A value conforming to this schema remains provisional. Shape validation can
   never upgrade it into authoritative knowledge."
  [:map
   {:closed true}
   [:authority
    [:= :provisional]]
   [:command-id CommandId]
   [:observed-basis Basis]
   [:projection any?]])

;; =============================================================================
;; Compatibility identity
;; =============================================================================

(def built-in-compatibility-kinds
  "Independent compatibility dimensions named directly by the v4.5 design.

   This set documents the framework dimensions; it is not a closed registry.
   Model/application protocols may need additional explicitly named dimensions.
   None of them are collapsed into one global version integer."
  #{:choreography
    :executable-plan
    :semantic-operation
    :wire-protocol
    :deployment})

(def CompatibilityIdentity
  "One typed compatibility dimension/value.

   kind is an explicit keyword dimension rather than a closed universal enum.
   value remains opaque because different dimensions naturally use different
   identities (integer representation versions, semantic operation versions,
   executable digests, deployment generations, etc.). Comparing compatibility
   identities is a policy decision for the layer that owns that dimension.

   Command identity and execution identity are intentionally absent from the
   built-in dimensions: v4.5 treats them as correlation/semantic identities,
   not compatibility generations."
  [:map
   {:closed true}
   [:kind keyword?]
   [:value some?]])

(def CompatibilitySet
  "A set of distinct compatibility identities, at most one per dimension."
  [:and
   [:set CompatibilityIdentity]
   [:fn
    {:error/message "must contain at most one identity for each compatibility kind"}
    (fn [identities]
      (= (count identities)
         (count (set (map :kind identities)))))]])

;; =============================================================================
;; Registry and compiled validation
;; =============================================================================

(def schemas
  "Named public Malli schemas for the portable Choreo type vocabulary."
  {::role Role
   ::principal Principal
   ::actor Actor
   ::host Host
   ::authority Authority
   ::authority-name AuthorityName
   ::command-id CommandId
   ::execution-id ExecutionId
   ::trust-class TrustClass
   ::fact-key FactKey
   ::provenance Provenance
   ::basis Basis
   ::outcome Outcome
   ::fact Fact
   ::authoritative-basis-progression AuthoritativeBasisProgression
   ::command Command
   ::provisional-value ProvisionalValue
   ::compatibility-identity CompatibilityIdentity
   ::compatibility-set CompatibilitySet})

(def ^:private compiled-schemas
  (into
   {}
   (map
    (fn [[schema-key schema-form]]
      [schema-key
       (m/schema schema-form)]))
   schemas))

(def ^:private compiled-validators
  (into
   {}
   (map
    (fn [[schema-key compiled-schema]]
      [schema-key
       (m/validator compiled-schema)]))
   compiled-schemas))

(defn schema
  "Return the compiled Malli schema for schema-key.

   Only the explicit Choreo type registry is accepted so misspelled semantic
   type names fail closed rather than becoming arbitrary Malli forms."
  [schema-key]
  (or
   (get compiled-schemas schema-key)
   (type-error
    :unknown-schema
    "Unknown Gesso Choreo type schema."
    {:schema-key schema-key
     :known-schema-keys (set (keys schemas))})))

(defn validator
  "Return the precompiled boolean validator for schema-key."
  [schema-key]
  (or
   (get compiled-validators schema-key)
   (do
     (schema schema-key)
     ;; schema always throws for an unknown key; this expression is therefore
     ;; unreachable but keeps the function total for static readers.
     nil)))

(defn valid?
  "Return true when value conforms to the named shape schema.

   This is deliberately only shape validation."
  [schema-key value]
  ((validator schema-key)
   value))

(defn explain-data
  "Return Malli explanation data for value, or nil when valid.

   Explanations are constructed on demand rather than on the successful path."
  [schema-key value]
  (m/explain
   (schema schema-key)
   value))

(defn validate!
  "Return value when it conforms to schema-key; otherwise throw ex-info.

   The exception reports Malli explanation data but does not reinterpret shape
   failure as a semantic/trust/authorization failure."
  [schema-key value]
  (if (valid? schema-key value)
    value
    (type-error
     :invalid-value
     "Value does not satisfy the requested Gesso Choreo type schema."
     {:schema-key schema-key
      :value value
      :explanation
      (explain-data
       schema-key
       value)})))

;; =============================================================================
;; Small constructors for canonical semantic envelopes
;; =============================================================================

(defn command
  "Construct and validate a semantic Command envelope from opts."
  [opts]
  (validate!
   ::command
   opts))

(defn provisional-value
  "Construct and validate an explicit ProvisionalValue envelope from opts."
  [opts]
  (validate!
   ::provisional-value
   opts))

(defn compatibility-identity
  "Construct one typed compatibility identity.

   kind identifies the compatibility dimension; value is the opaque identity or
   version meaningful to that dimension."
  [kind value]
  (validate!
   ::compatibility-identity
   {:kind kind
    :value value}))

(defn compatibility-set
  "Validate and return a set of compatibility identities."
  [identities]
  (validate!
   ::compatibility-set
   identities))
