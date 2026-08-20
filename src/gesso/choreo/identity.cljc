(ns gesso.choreo.identity
  "Small explicit identity vocabulary for Gesso choreography.

   Distributed protocol code becomes dangerous when unrelated identities are
   represented by the same unqualified scalar and therefore become
   interchangeable by accident. Choreo needs to distinguish at least:

     role
       A participant position in the protocol, such as :browser or :server.
       Roles remain keywords in authored choreography and projected plans.

     principal
       An authenticated semantic identity, normally established by a trusted
       server boundary. A principal is not a role and is not a browser claim.

     actor
       The concrete semantic actor on whose behalf an operation is being
       performed. Actor and principal may be related by application policy, but
       they are deliberately not the same identity concept.

     authority
       The semantic authority whose observations/results are trusted as
       authoritative for some domain fact. Authority is not a deployment host.

     host
       A concrete runtime/deployment participant instance: browser context,
       Aleph node, worker process, etc. Host identity must not imply authority.

     command-id
       Identity of one semantic command/intention. Retries or replacement
       executions may retain the same command-id.

     execution-id
       Identity of one concrete choreography execution attempt. It is distinct
       from command-id even when an application initially assigns related raw
       values.

   This namespace intentionally does not define authentication, authorization,
   tenancy, session semantics, host discovery, command durability, or execution
   lifecycle. It only supplies unambiguous portable data identities that later
   layers can bind and reason about.

   All non-role identities are tagged opaque references. Their raw values are
   intentionally application-defined, but nil and already-tagged identity
   references are rejected. Tagging prevents this class of mistake:

     (principal \"42\") != (execution-id \"42\")

   even though both use the same raw string.

   The raw value must be serializable wherever the identity crosses a protocol
   boundary; this namespace cannot prove arbitrary caller values are portable
   EDN."

  (:refer-clojure :exclude [identity]))

;; -----------------------------------------------------------------------------
;; Vocabulary
;; -----------------------------------------------------------------------------

(def identity-type
  :gesso.choreo.identity/ref)

(def identity-kinds
  #{:principal
    :actor
    :authority
    :host
    :command
    :execution})

(defn role?
  "True when value is a valid choreography role identity.

   Roles intentionally remain keywords because they name static protocol
   positions rather than runtime identity references."
  [value]
  (keyword? value))

;; -----------------------------------------------------------------------------
;; Errors
;; -----------------------------------------------------------------------------

(defn- identity-error
  [kind message data]
  (throw
   (ex-info
    message
    (merge
     {:error/type :gesso.choreo.identity/error
      :error/kind kind}
     data))))

(defn require-role
  "Return role when it is a keyword, otherwise throw."
  [role]
  (when-not (role? role)
    (identity-error
     :invalid-role
     "Choreography role must be a keyword."
     {:role role}))
  role)

;; -----------------------------------------------------------------------------
;; Tagged identity references
;; -----------------------------------------------------------------------------

(defn identity?
  "True when value is a structurally valid tagged Choreo identity reference."
  [value]
  (and
   (map? value)
   (= identity-type
      (:gesso.choreo.identity/type value))
   (contains?
    identity-kinds
    (:gesso.choreo.identity/kind value))
   (contains?
    value
    :gesso.choreo.identity/value)
   (some?
    (:gesso.choreo.identity/value value))
   ;; Nested identity refs are almost certainly an accidental re-tagging.
   (not
    (identity?
     (:gesso.choreo.identity/value value)))))

(defn- require-kind
  [kind]
  (when-not
   (contains?
    identity-kinds
    kind)
    (identity-error
     :invalid-kind
     "Unknown Choreo identity kind."
     {:kind kind
      :allowed identity-kinds}))
  kind)

(defn- require-raw-value
  [value]
  (when (nil? value)
    (identity-error
     :invalid-value
     "Choreo identity raw value may not be nil."
     {:value value}))

  (when (identity? value)
    (identity-error
     :nested-identity
     "A Choreo identity reference may not be used as the raw value of another identity reference."
     {:value value}))

  value)

(defn identity
  "Construct a tagged identity reference of kind around opaque raw value.

   kind must be one of identity-kinds. Use the specific constructors below in
   normal code; this generic constructor is primarily useful to decoders and
   generic tooling."
  [kind value]
  {:gesso.choreo.identity/type
   identity-type

   :gesso.choreo.identity/kind
   (require-kind kind)

   :gesso.choreo.identity/value
   (require-raw-value value)})

(defn kind
  "Return the identity kind, or nil when value is not a Choreo identity."
  [value]
  (when (identity? value)
    (:gesso.choreo.identity/kind value)))

(defn raw-value
  "Return the opaque raw identity value, or nil when value is not a Choreo
   identity reference."
  [value]
  (when (identity? value)
    (:gesso.choreo.identity/value value)))

(defn kind?
  "True when value is a Choreo identity of expected-kind."
  [expected-kind value]
  (and
   (contains?
    identity-kinds
    expected-kind)
   (= expected-kind
      (kind value))))

(defn same-kind?
  "True when both values are valid identities of the same identity kind."
  [left right]
  (and
   (identity? left)
   (identity? right)
   (= (kind left)
      (kind right))))

(defn same-raw-value?
  "True when both valid identities wrap equal raw values.

   This diagnostic predicate deliberately ignores kind. It should not be used
   as semantic identity equality; ordinary `=` preserves the important kind
   distinction."
  [left right]
  (and
   (identity? left)
   (identity? right)
   (= (raw-value left)
      (raw-value right))))

;; -----------------------------------------------------------------------------
;; Specific constructors and predicates
;; -----------------------------------------------------------------------------

(defn principal
  "Construct a principal identity reference."
  [value]
  (identity
   :principal
   value))

(defn principal?
  [value]
  (kind?
   :principal
   value))

(defn actor
  "Construct an actor identity reference."
  [value]
  (identity
   :actor
   value))

(defn actor?
  [value]
  (kind?
   :actor
   value))

(defn authority
  "Construct an authority identity reference."
  [value]
  (identity
   :authority
   value))

(defn authority?
  [value]
  (kind?
   :authority
   value))

(defn host
  "Construct a runtime/deployment host identity reference."
  [value]
  (identity
   :host
   value))

(defn host?
  [value]
  (kind?
   :host
   value))

(defn command-id
  "Construct a semantic command identity.

   Command identity may survive retries or replacement executions."
  [value]
  (identity
   :command
   value))

(defn command-id?
  [value]
  (kind?
   :command
   value))

(defn execution-id
  "Construct one concrete choreography execution-attempt identity."
  [value]
  (identity
   :execution
   value))

(defn execution-id?
  [value]
  (kind?
   :execution
   value))

;; -----------------------------------------------------------------------------
;; Explicit binding maps
;; -----------------------------------------------------------------------------

(def binding-keys
  #{:role
    :principal
    :actor
    :authority
    :host
    :command-id
    :execution-id})

(defn- validate-binding
  [key value]
  (case key
    :role
    (require-role value)

    :principal
    (if (principal? value)
      value
      (identity-error
       :invalid-binding
       "Identity binding :principal must contain a principal identity."
       {:binding key
        :value value}))

    :actor
    (if (actor? value)
      value
      (identity-error
       :invalid-binding
       "Identity binding :actor must contain an actor identity."
       {:binding key
        :value value}))

    :authority
    (if (authority? value)
      value
      (identity-error
       :invalid-binding
       "Identity binding :authority must contain an authority identity."
       {:binding key
        :value value}))

    :host
    (if (host? value)
      value
      (identity-error
       :invalid-binding
       "Identity binding :host must contain a host identity."
       {:binding key
        :value value}))

    :command-id
    (if (command-id? value)
      value
      (identity-error
       :invalid-binding
       "Identity binding :command-id must contain a command identity."
       {:binding key
        :value value}))

    :execution-id
    (if (execution-id? value)
      value
      (identity-error
       :invalid-binding
       "Identity binding :execution-id must contain an execution identity."
       {:binding key
        :value value}))))

(defn bindings
  "Construct/validate an explicit identity-binding map.

   Bindings are intentionally sparse: callers include only identities they have
   actually established. No relationship between principal, actor, authority,
   host, command, or execution is inferred.

   Unknown keys are rejected so this vocabulary cannot silently become another
   arbitrary execution-context map."
  [value]
  (when-not (map? value)
    (identity-error
     :invalid-bindings
     "Identity bindings must be a map."
     {:value value}))

  (let [unknown
        (remove
         binding-keys
         (keys value))]

    (when (seq unknown)
      (identity-error
       :unknown-binding
       "Identity bindings contain unsupported keys."
       {:unknown
        (set unknown)
        :allowed
        binding-keys})))

  (into
   {}
   (map
    (fn [[key value]]
      [key
       (validate-binding
        key
        value)]))
   value))

(defn bindings?
  "True when value is a valid explicit identity-binding map."
  [value]
  (try
    (bindings value)
    true
    (catch #?(:clj Throwable
              :cljs :default) _
      false)))

(defn binding
  "Return one explicit binding value, or nil when absent.

   key must be one of binding-keys."
  [identity-bindings key]
  (when-not
   (contains?
    binding-keys
    key)
    (identity-error
     :unknown-binding
     "Unknown identity binding key."
     {:binding key
      :allowed binding-keys}))

  (get
   (bindings identity-bindings)
   key))

(defn explain
  "Return a compact diagnostic representation that preserves identity kinds
   while exposing raw values for development tooling.

   This is diagnostic data only; raw-value equality must not be used as
   semantic identity equality."
  [identity-bindings]
  (let [bindings'
        (bindings identity-bindings)]
    (into
     {}
     (map
      (fn [[key value]]
        [key
         (if (= key :role)
           value
           {:kind
            (kind value)
            :value
            (raw-value value)})]))
     bindings')))
