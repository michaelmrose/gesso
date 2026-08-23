(ns gesso.choreo.knowledge
  "Pure role-local knowledge and provenance for Gesso choreography.

   Choreography semantics has a global abstract value store, and the portable
   machine currently has a role-local execution value store. Neither answers
   the stronger question required by the v4.5 design:

     What does this role know, and why is it entitled to use that fact?

   This namespace supplies that missing model without performing protocol
   execution. A knowledge state belongs to exactly one role. Every known value
   carries one or more explicit provenance records.

   Supported provenance kinds:

     :input
       The value was supplied as an entry fact for this role.

     :communicated
       The value arrived in a declared participant communication.

     :authoritative
       The value was established either by a trusted realization of a public
       authoritative operation or by an explicit authoritative observation /
       reread carrying its logical authority, observation identity, and opaque
       authoritative basis.

     :derived
       The value was derived from already-known facts by a named rule.

     :asserted
       The value was explicitly asserted by a named source. Assertion records
       origin; it does not by itself imply authority or truth.

   Important boundary:

   This namespace does NOT decide authorization, authenticate principals,
   execute public model operations, validate transport envelopes, or infer that
   every field in an open payload is knowledge.

   In particular, establish-communicated accepts an explicit set of declared
   communicated keys and ignores all other payload keys. That makes the
   closed-message rule usable even when :open-payload? permits transport data
   that is not part of the semantic knowledge contract.

   Knowledge updates are immutable and deterministic. Re-establishing the same
   value may add another provenance justification. Generic replacement remains
   available for non-authoritative bookkeeping, but it may not stand in for
   authoritative progression: an authoritative observation that conflicts with
   the current value requires an explicit basis/progression contract in the
   layer that owns such ordering. History contains no wall-clock timestamps.

   Choreography state ids are opaque EDN values. Provenance therefore preserves
   a supplied :state exactly as given instead of incorrectly constraining state
   identity to keywords."

  (:require
   [clojure.set :as set]))

;; -----------------------------------------------------------------------------
;; Identity
;; -----------------------------------------------------------------------------

(def knowledge-type
  :gesso.choreo.knowledge/state)

(def provenance-kinds
  #{:input
    :communicated
    :authoritative
    :derived
    :asserted})

;; -----------------------------------------------------------------------------
;; Errors
;; -----------------------------------------------------------------------------

(defn- knowledge-error
  [kind message data]
  (throw
   (ex-info
    message
    (merge
     {:error/type :gesso.choreo.knowledge/error
      :error/kind kind}
     data))))

;; -----------------------------------------------------------------------------
;; Small validation helpers
;; -----------------------------------------------------------------------------

(defn- require-keyword!
  [label value]
  (when-not (keyword? value)
    (knowledge-error
     :invalid-value
     (str label " must be a keyword.")
     {:label label
      :value value}))
  value)

(defn- require-map!
  [label value]
  (when-not (map? value)
    (knowledge-error
     :invalid-value
     (str label " must be a map.")
     {:label label
      :value value}))
  value)

(defn- require-keyword-set!
  [label value]
  (when-not (and (set? value)
                 (every? keyword? value))
    (knowledge-error
     :invalid-value-set
     (str label " must be a set of keywords.")
     {:label label
      :value value}))
  value)

(defn- normalize-metadata
  [metadata]
  (when (some? metadata)
    (require-map!
     "Provenance metadata"
     metadata))
  metadata)

;; -----------------------------------------------------------------------------
;; Provenance
;; -----------------------------------------------------------------------------

(defn provenance?
  "True when value is a structurally valid provenance record.

   This is a shape predicate only. It does not prove that the represented
   source was truthful, authorized, deterministic, or pure."
  [value]
  (and
   (map? value)
   (contains?
    provenance-kinds
    (:kind value))

   (case (:kind value)
     :input
     (or
      (not
       (contains? value :source))
      (keyword?
       (:source value)))

     :communicated
     (and
      (keyword?
       (:from value))
      (keyword?
       (:event value))
      (or
       (not
        (contains? value :via))
       (keyword?
        (:via value))))

     :authoritative
     (let [operation?
           (contains? value :operation)

           observation?
           (or
            (contains? value :authority)
            (contains? value :observation)
            (contains? value :basis))]

       (cond
         (and operation?
              observation?)
         false

         operation?
         (keyword?
          (:operation value))

         observation?
         (and
          (keyword?
           (:authority value))
          (keyword?
           (:observation value))
          (contains? value :basis)
          (some?
           (:basis value)))

         :else
         false))

     :derived
     (and
      (keyword?
       (:rule value))
      (set?
       (:depends-on value))
      (every?
       keyword?
       (:depends-on value)))

     :asserted
     (keyword?
      (:source value))

     false)

   (or
    (not
     (contains? value :metadata))
    (map?
     (:metadata value)))))

(defn- require-provenance!
  [value]
  (when-not (provenance? value)
    (knowledge-error
     :invalid-provenance
     "Knowledge provenance is malformed."
     {:provenance value}))
  value)

(defn input-provenance
  "Construct provenance for an entry fact.

   source is optional because some executable plans may bind entry values
   without assigning a stronger semantic source identity."
  ([]
   {:kind :input})
  ([source]
   {:kind :input
    :source
    (require-keyword!
     "Input provenance source"
     source)}))

(defn communicated-provenance
  "Construct provenance for one declared communicated fact."
  ([from event]
   (communicated-provenance
    from
    event
    nil))
  ([from
    event
    {:keys [via state metadata]
     :as options}]
   (require-map!
    "Communicated provenance options"
    (or options {}))
   (cond->
    {:kind :communicated
     :from
     (require-keyword!
      "Communicated provenance from"
      from)
     :event
     (require-keyword!
      "Communicated provenance event"
      event)}

     (some? via)
     (assoc
      :via
      (require-keyword!
       "Communicated provenance via"
       via))

     (some? state)
     (assoc
      :state
      state)

     (some? metadata)
     (assoc
      :metadata
      (normalize-metadata metadata)))))

(defn authoritative-provenance
  "Construct provenance for a fact returned by one public authoritative
   semantic operation."
  ([operation]
   (authoritative-provenance
    operation
    nil))
  ([operation
    {:keys [state metadata]
     :as options}]
   (require-map!
    "Authoritative provenance options"
    (or options {}))
   (cond->
    {:kind :authoritative
     :operation
     (require-keyword!
      "Authoritative provenance operation"
      operation)}

     (some? state)
     (assoc
      :state
      state)

     (some? metadata)
     (assoc
      :metadata
      (normalize-metadata metadata)))))

(defn authoritative-observation-provenance
  "Construct provenance for a fact learned by authoritative observation/reread.

   This is deliberately distinct from authoritative-provenance, which records
   trusted execution of a public authoritative operation.

   authority names the logical authority whose current projection was observed.
   observation names the declared projection/read boundary. basis is opaque
   authoritative progression context supplied by the layer that owns ordering;
   this namespace records it but does not compare, order, or authenticate it.

   A nil basis is rejected because a bare reread/invalidation cannot silently be
   upgraded into authoritative knowledge. Arbitrary non-nil basis values remain
   opaque here."
  ([authority observation basis]
   (authoritative-observation-provenance
    authority
    observation
    basis
    nil))
  ([authority
    observation
    basis
    {:keys [state metadata]
     :as options}]
   (require-map!
    "Authoritative observation provenance options"
    (or options {}))
   (require-keyword!
    "Authoritative observation authority"
    authority)
   (require-keyword!
    "Authoritative observation identity"
    observation)
   (when (nil? basis)
     (knowledge-error
      :invalid-authoritative-observation
      "Authoritative observation requires an explicit non-nil basis."
      {:authority authority
       :observation observation
       :basis basis}))
   (cond->
    {:kind :authoritative
     :authority authority
     :observation observation
     :basis basis}

     (some? state)
     (assoc
      :state
      state)

     (some? metadata)
     (assoc
      :metadata
      (normalize-metadata metadata)))))

(defn derived-provenance
  "Construct provenance for a deterministic derivation from known facts.

   This record names the rule and dependencies. The record itself cannot prove
   that an arbitrary Clojure implementation of rule is pure or deterministic;
   that remains a checker/trusted-code obligation."
  ([rule depends-on]
   (derived-provenance
    rule
    depends-on
    nil))
  ([rule
    depends-on
    {:keys [metadata]
     :as options}]
   (require-map!
    "Derived provenance options"
    (or options {}))
   (cond->
    {:kind :derived
     :rule
     (require-keyword!
      "Derived provenance rule"
      rule)
     :depends-on
     (require-keyword-set!
      "Derived provenance :depends-on"
      depends-on)}

     (some? metadata)
     (assoc
      :metadata
      (normalize-metadata metadata)))))

(defn asserted-provenance
  "Construct provenance for an explicit assertion by source.

   :asserted records origin only. Consumers must not silently treat it as
   equivalent to :authoritative."
  ([source]
   (asserted-provenance
    source
    nil))
  ([source
    {:keys [metadata]
     :as options}]
   (require-map!
    "Asserted provenance options"
    (or options {}))
   (cond->
    {:kind :asserted
     :source
     (require-keyword!
      "Asserted provenance source"
      source)}

     (some? metadata)
     (assoc
      :metadata
      (normalize-metadata metadata)))))

;; -----------------------------------------------------------------------------
;; State construction and inspection
;; -----------------------------------------------------------------------------

(defn knowledge?
  "True when value has the basic shape of a role-local knowledge state."
  [value]
  (and
   (map? value)
   (= knowledge-type
      (:gesso.choreo/type value))
   (keyword?
    (:role value))
   (map?
    (:facts value))
   (vector?
    (:history value))))

(defn- require-knowledge!
  [value]
  (when-not (knowledge? value)
    (knowledge-error
     :invalid-knowledge
     "Expected a role-local knowledge state."
     {:value value}))
  value)

(defn empty-knowledge
  "Construct an empty knowledge state for role."
  [role]
  {:gesso.choreo/type knowledge-type
   :role
   (require-keyword!
    "Knowledge role"
    role)
   :facts {}
   :history []})

(defn role
  "Return the role that owns this knowledge state."
  [knowledge]
  (:role
   (require-knowledge!
    knowledge)))

(defn facts
  "Return all currently known facts keyed by semantic value key.

   Each fact has:

     {:value ...
      :provenance [...]}

   The provenance vector contains one or more justifications for the current
   value."
  [knowledge]
  (:facts
   (require-knowledge!
    knowledge)))

(defn fact
  "Return the current fact record for key, or nil when key is unknown."
  [knowledge key]
  (require-keyword!
   "Knowledge key"
   key)
  (get
   (facts knowledge)
   key))

(defn known?
  "True when key is currently known by this role."
  [knowledge key]
  (some?
   (fact knowledge key)))

(defn value
  "Return the current known value for key, or nil when unknown.

   Use known? when nil itself is a meaningful known value."
  [knowledge key]
  (:value
   (fact knowledge key)))

(defn provenance
  "Return the provenance vector for key, or nil when key is unknown."
  [knowledge key]
  (:provenance
   (fact knowledge key)))

(defn provenance-kinds-for
  "Return the set of provenance kinds justifying the current value of key."
  [knowledge key]
  (if-let [records
           (provenance
            knowledge
            key)]
    (set
     (map
      :kind
      records))
    #{}))

(defn history
  "Return deterministic knowledge-establishment history.

   History deliberately contains no wall-clock timestamps."
  [knowledge]
  (:history
   (require-knowledge!
    knowledge)))

(defn values
  "Return the current role-local values as a plain key->value map."
  [knowledge]
  (into
   {}
   (map
    (fn [[key fact-record]]
      [key
       (:value fact-record)]))
   (facts knowledge)))

;; -----------------------------------------------------------------------------
;; Establishment
;; -----------------------------------------------------------------------------

(defn establish
  "Establish one role-local fact with explicit provenance.

   Re-establishing the same key/value may add another provenance justification.
   Duplicate identical provenance is not repeated.

   A different value for an already-known key is rejected by default. Generic
   non-authoritative replacement may be requested with {:replace? true}; the
   replacement is recorded in history and starts a new provenance vector for the
   new current value.

   Authoritative provenance is deliberately stricter. A conflicting
   authoritative observation may not overwrite current knowledge merely because
   it arrived later or because a caller passed {:replace? true}. Choreo's
   knowledge layer does not know whether opaque authoritative basis B advances,
   equals, precedes, or is incomparable with basis A. Such an overwrite therefore
   fails with :authoritative-progression-required until a basis/progression layer
   has established that the observation is admissible."
  ([knowledge key new-value provenance]
   (establish
    knowledge
    key
    new-value
    provenance
    nil))
  ([knowledge
    key
    new-value
    provenance
    {:keys [replace?]
     :or {replace? false}
     :as options}]
   (require-knowledge!
    knowledge)
   (require-keyword!
    "Knowledge key"
    key)
   (require-provenance!
    provenance)
   (require-map!
    "Knowledge establishment options"
    (or options {}))

   (when-not (boolean? replace?)
     (knowledge-error
      :invalid-replace
      "Knowledge :replace? must be boolean."
      {:replace? replace?}))

   (let [existing
         (fact
          knowledge
          key)]

     (cond
       (nil? existing)
       (-> knowledge
           (assoc-in
            [:facts key]
            {:value new-value
             :provenance [provenance]})
           (update
            :history
            conj
            {:kind :establish
             :key key
             :value new-value
             :provenance provenance}))

       (= (:value existing)
          new-value)
       (if (some
            #(= provenance %)
            (:provenance existing))
         knowledge
         (-> knowledge
             (update-in
              [:facts key :provenance]
              conj
              provenance)
             (update
              :history
              conj
              {:kind :justify
               :key key
               :value new-value
               :provenance provenance})))

       (and replace?
            (= :authoritative
               (:kind provenance)))
       (knowledge-error
        :authoritative-progression-required
        "A conflicting authoritative observation requires an explicit authoritative basis/progression decision before it may replace current knowledge."
        {:role
         (:role knowledge)
         :key key
         :known-value
         (:value existing)
         :new-value new-value
         :provenance provenance})

       replace?
       (-> knowledge
           (assoc-in
            [:facts key]
            {:value new-value
             :provenance [provenance]})
           (update
            :history
            conj
            {:kind :replace
             :key key
             :old-value
             (:value existing)
             :value new-value
             :provenance provenance}))

       :else
       (knowledge-error
        :knowledge-conflict
        "Knowledge key already has a different current value."
        {:role
         (:role knowledge)
         :key key
         :known-value
         (:value existing)
         :new-value new-value
         :provenance provenance})))))

(defn establish-many
  "Establish every key/value in value-map with the same provenance.

   Keys are processed in stable string order so history is deterministic across
   runtimes. Options are passed to establish."
  ([knowledge value-map provenance]
   (establish-many
    knowledge
    value-map
    provenance
    nil))
  ([knowledge
    value-map
    provenance
    options]
   (require-knowledge!
    knowledge)
   (require-map!
    "Knowledge values"
    value-map)
   (require-provenance!
    provenance)

   (reduce
    (fn [knowledge key]
      (establish
       knowledge
       key
       (get value-map key)
       provenance
       options))
    knowledge
    (sort-by
     str
     (keys value-map)))))

(defn establish-inputs
  "Establish entry values with :input provenance."
  ([knowledge value-map]
   (establish-inputs
    knowledge
    value-map
    nil))
  ([knowledge value-map source]
   (establish-many
    knowledge
    value-map
    (if (some? source)
      (input-provenance
       source)
      (input-provenance)))))

(defn establish-authoritative
  "Establish outputs of one trusted authoritative realization."
  ([knowledge value-map operation]
   (establish-authoritative
    knowledge
    value-map
    operation
    nil))
  ([knowledge value-map operation options]
   (establish-many
    knowledge
    value-map
    (authoritative-provenance
     operation
     options))))

(defn establish-authoritative-observation
  "Establish values learned from an authoritative projection/reread.

   The observation provenance records logical authority, observation identity,
   and opaque basis. This function does not decide whether one basis advances
   another. Consequently, a conflicting current value still requires the
   external progression layer to authorize replacement; {:replace? true} alone
   is intentionally insufficient."
  ([knowledge
    value-map
    authority
    observation
    basis]
   (establish-authoritative-observation
    knowledge
    value-map
    authority
    observation
    basis
    nil))
  ([knowledge
    value-map
    authority
    observation
    basis
    options]
   (let [options'
         (or options {})

         provenance-options
         (select-keys
          options'
          [:state :metadata])

         establishment-options
         (select-keys
          options'
          [:replace?])]
     (require-map!
      "Authoritative observation establishment options"
      options')
     (establish-many
      knowledge
      value-map
      (authoritative-observation-provenance
       authority
       observation
       basis
       provenance-options)
      establishment-options))))

(defn establish-asserted
  "Establish explicitly asserted values.

   This is intentionally distinct from establish-authoritative."
  ([knowledge value-map source]
   (establish-asserted
    knowledge
    value-map
    source
    nil))
  ([knowledge value-map source options]
   (establish-many
    knowledge
    value-map
    (asserted-provenance
     source
     options))))

(defn establish-communicated
  "Establish only declared communicated fields present in payload.

   declared-keys is the semantic communication field set known to be permitted
   to enter receiver knowledge. Keys in payload but absent from declared-keys
   are ignored, even when the transport/message contract is open.

   This function does not validate required-vs-optional presence; message
   contract enforcement belongs to choreography semantics/projection/machine.
   It only enforces the knowledge boundary: undeclared payload fields do not
   become known merely because they crossed transport."
  ([knowledge
    from
    event
    declared-keys
    payload]
   (establish-communicated
    knowledge
    from
    event
    declared-keys
    payload
    nil))
  ([knowledge
    from
    event
    declared-keys
    payload
    {:keys [via state metadata replace?]
     :or {replace? false}
     :as options}]
   (require-knowledge!
    knowledge)
   (require-keyword-set!
    "Declared communicated keys"
    declared-keys)
   (require-map!
    "Communicated payload"
    payload)

   (let [provenance
         (communicated-provenance
          from
          event
          (cond->
           {}
           (some? via)
           (assoc :via via)
           (some? state)
           (assoc :state state)
           (some? metadata)
           (assoc :metadata metadata)))

         communicated-values
         (select-keys
          payload
          declared-keys)]

     (establish-many
      knowledge
      communicated-values
      provenance
      {:replace? replace?}))))

(defn establish-derived
  "Establish one derived fact from already-known dependency keys.

   This enforces dependency availability and records the dependency set. It
   accepts the derived value rather than executing an arbitrary function, so
   this namespace makes no false claim that Clojure code was proven pure or
   deterministic."
  ([knowledge
    key
    derived-value
    rule
    depends-on]
   (establish-derived
    knowledge
    key
    derived-value
    rule
    depends-on
    nil))
  ([knowledge
    key
    derived-value
    rule
    depends-on
    {:keys [metadata replace?]
     :or {replace? false}
     :as options}]
   (require-knowledge!
    knowledge)

   (let [depends-on'
         (require-keyword-set!
          "Derived knowledge :depends-on"
          depends-on)

         missing
         (set/difference
          depends-on'
          (set
           (keys
            (facts knowledge))))]

     (when (seq missing)
       (knowledge-error
        :unknown-dependency
        "Derived knowledge depends on facts this role does not know."
        {:role
         (:role knowledge)
         :key key
         :rule rule
         :missing missing}))

     (establish
      knowledge
      key
      derived-value
      (derived-provenance
       rule
       depends-on'
       (cond->
        {}
        (some? metadata)
        (assoc :metadata metadata)))
      {:replace? replace?}))))

;; -----------------------------------------------------------------------------
;; Explanation
;; -----------------------------------------------------------------------------

(defn explain
  "Return a compact diagnostic summary of one role's current knowledge."
  [knowledge]
  (require-knowledge!
   knowledge)
  {:role
   (:role knowledge)

   :known-keys
   (set
    (keys
     (:facts knowledge)))

   :values
   (values knowledge)

   :provenance-kinds-by-key
   (into
    {}
    (map
     (fn [key]
       [key
        (provenance-kinds-for
         knowledge
         key)]))
    (keys
     (:facts knowledge)))

   :history-count
   (count
    (:history knowledge))})
