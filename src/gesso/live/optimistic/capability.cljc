(ns gesso.live.optimistic.capability
  "Portable typed operation capabilities for optimistic UI composition.

   A capability is trusted *application configuration*, normally defined next to
   an operation-specific choreography.  It binds the semantic operation and the
   browser execution policy that should accompany that operation when a view
   renders an affordance.

   A capability is deliberately NOT authorization.  Binding one produces only
   inert request/presentation data suitable for gesso.live.ui/optimistic-action.
   The browser may modify every rendered byte.  The trusted server must still
   authenticate the principal, resolve the semantic operation through its own
   trusted registry, reread/revalidate authority, and invoke the public model
   operation.

   This namespace also deliberately does not add semantic-operation version data
   to optimistic protocol v3.  Protocol v3 is a closed wire schema; changing its
   command fields without changing the protocol version would make one version
   number denote incompatible schemas.  A future protocol/capability revision may
   carry a separate :semantic-operation compatibility identity when an operation
   actually needs that dimension.

   Capabilities therefore solve a narrower v4.5 problem now: views can consume a
   bound operation description without reproducing operation identity, plan
   selection, timeout, rollback, or replacement policy at each affordance."
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [gesso.choreo.project :as project]
   [gesso.live.optimistic.protocol :as protocol]))

(def operation-capability-type
  :gesso.live.optimistic.capability/operation)

(def capability-type-key
  :gesso.live.optimistic.capability/type)

(def capability-required-keys
  #{:operation})

(def capability-optional-keys
  #{:plan-key
    :rollback-eligible?
    :timeout-ms
    :replace-owner?
    :replace-execution?})

(def binding-required-keys
  #{:arguments
    :observed-basis})

(def binding-optional-keys
  #{:scope
    :fact-versions
    :target-id})

(def derived-policy-optional-keys
  "Capability-owned browser policy that may vary by semantic operation when
   capabilities are derived from an operation-keyed browser plan map.

   :plan-key is intentionally absent. In the canonical derived path the logical
   browser plan key *is* the semantic operation key, eliminating one duplicated
   application declaration. Applications that genuinely require another mapping
   can use operation-capability directly as the explicit escape hatch."
  (disj capability-optional-keys :plan-key))

(defn- capability-error
  [kind message data]
  (throw
   (ex-info
    message
    (merge
     {:error/type :gesso.live.optimistic.capability/error
      :error/kind kind}
     data))))

(defn- require-map!
  [label value]
  (when-not (map? value)
    (capability-error
     :invalid-shape
     (str label " must be a map.")
     {:label label
      :value value}))
  value)

(defn- require-closed-map!
  [label value required optional]
  (let [value' (require-map! label value)
        keys' (set (keys value'))
        allowed (into required optional)
        missing (set/difference required keys')
        unknown (set/difference keys' allowed)]
    (when (seq missing)
      (capability-error
       :missing-fields
       (str label " is missing required fields.")
       {:label label
        :missing missing
        :required required}))
    (when (seq unknown)
      (capability-error
       :unknown-fields
       (str label " contains unknown fields.")
       {:label label
        :unknown unknown
        :allowed allowed}))
    value'))

(defn- require-keyword!
  [label value]
  (when-not (keyword? value)
    (capability-error
     :invalid-keyword
     (str label " must be a keyword.")
     {:label label
      :value value}))
  value)

(defn- require-boolean!
  [label value]
  (when-not (boolean? value)
    (capability-error
     :invalid-boolean
     (str label " must be a boolean.")
     {:label label
      :value value}))
  value)

(defn- require-timeout!
  [value]
  (when-not (and (integer? value)
                 (<= 0 value))
    (capability-error
     :invalid-timeout
     "Optimistic capability :timeout-ms must be a non-negative integer."
     {:value value}))
  value)

(defn- require-target-id!
  [value]
  (when-not (and (string? value)
                 (not (str/blank? value)))
    (capability-error
     :invalid-target-id
     "Optimistic capability binding :target-id must be a non-blank string."
     {:value value}))
  value)

(defn operation-capability
  "Construct one trusted application-side optimistic operation capability.

   Required:

     :operation
       Public semantic operation keyword, for example :request/claim.

   Optional capability-owned browser policy:

     :plan-key
       Stable browser plan-registry key.  Defaults to :operation.

     :rollback-eligible?
     :timeout-ms
     :replace-owner?
     :replace-execution?
       Browser execution policy copied into each bound action only when
       explicitly configured.

   The returned value is tagged and closed so it can be distinguished from the
   inert per-render action map.  Constructing it does not grant authority and
   does not register the operation on the trusted server."
  [options]
  (let [options'
        (require-closed-map!
         "Optimistic operation capability"
         options
         capability-required-keys
         capability-optional-keys)
        operation
        (require-keyword!
         "Optimistic capability :operation"
         (:operation options'))
        plan-key
        (if (contains? options' :plan-key)
          (require-keyword!
           "Optimistic capability :plan-key"
           (:plan-key options'))
          operation)]
    (doseq [key [:rollback-eligible?
                 :replace-owner?
                 :replace-execution?]]
      (when (contains? options' key)
        (require-boolean!
         (str "Optimistic capability " key)
         (get options' key))))
    (when (contains? options' :timeout-ms)
      (require-timeout! (:timeout-ms options')))
    (cond->
     {capability-type-key operation-capability-type
      :operation operation
      :plan-key plan-key}
      (contains? options' :rollback-eligible?)
      (assoc :rollback-eligible? (:rollback-eligible? options'))

      (contains? options' :timeout-ms)
      (assoc :timeout-ms (:timeout-ms options'))

      (contains? options' :replace-owner?)
      (assoc :replace-owner? (:replace-owner? options'))

      (contains? options' :replace-execution?)
      (assoc :replace-execution? (:replace-execution? options')))))

(defn operation-capability?
  "True exactly for a canonical capability produced by operation-capability.

   This predicate establishes only the local configuration shape.  It does not
   establish server registration or authorization."
  [value]
  (and
   (map? value)
   (= operation-capability-type
      (get value capability-type-key))
   (try
     (= value
        (operation-capability
         (dissoc value capability-type-key)))
     (catch #?(:clj Throwable :cljs :default) _
       false))))

(defn require-operation-capability
  "Return capability when it is canonical; otherwise throw a capability error."
  [capability]
  (when-not (operation-capability? capability)
    (capability-error
     :invalid-capability
     "Expected a canonical optimistic operation capability."
     {:capability capability}))
  capability)

(defn operation-capabilities
  "Derive the canonical semantic-operation -> optimistic-capability map from an
   operation-keyed browser ExecutablePlan map.

   This is the preferred application authoring path when logical browser plan
   keys are semantic operation ids (for example :request/claim). It removes the
   need to repeat the same operation/plan-key correspondence in a second
   capability declaration.

   One-arity derives capabilities with no per-operation browser policy.

   Two-arity accepts an optional semantic-operation -> policy map. Policy maps
   may contain only capability-owned browser execution fields:

     :rollback-eligible?
     :timeout-ms
     :replace-owner?
     :replace-execution?

   :plan-key is deliberately not configurable here. The derived capability's
   :operation and :plan-key are both the logical key from browser-plans.

   Every browser-plan value must already be a canonical current ExecutablePlan.
   This establishes the local affordance -> executable-plan membership edge at
   construction time. It does not establish authorization, transport/route
   closure, or whole-application closure; those remain separate trusted/preflight
   boundaries."
  ([browser-plans]
   (operation-capabilities browser-plans {}))
  ([browser-plans policy-by-operation]
   (let [plans'
         (require-map!
          "Optimistic operation browser plans"
          browser-plans)

         policies'
         (require-map!
          "Optimistic operation capability policies"
          (or policy-by-operation {}))

         plan-operations
         (set (keys plans'))

         policy-operations
         (set (keys policies'))

         unknown-policy-operations
         (set/difference policy-operations plan-operations)]
     (when (empty? plans')
       (capability-error
        :empty-browser-plans
        "Optimistic operation capability derivation requires at least one browser ExecutablePlan."
        {}))

     (doseq [[operation plan] plans']
       (require-keyword!
        "Optimistic browser plan semantic operation"
        operation)
       (when-not (project/executable-plan? plan)
         (capability-error
          :invalid-browser-plan
          "Optimistic operation capability derivation requires canonical current ExecutablePlans."
          {:operation operation
           :plan plan})))

     (when (seq unknown-policy-operations)
       (capability-error
        :unknown-policy-operations
        "Optimistic capability policy names semantic operations absent from the browser plan map."
        {:unknown-operations unknown-policy-operations
         :available-operations plan-operations}))

     (into {}
           (map
            (fn [[operation _plan]]
              (let [policy
                    (require-closed-map!
                     "Optimistic derived operation policy"
                     (get policies' operation {})
                     #{}
                     derived-policy-optional-keys)]
                [operation
                 (operation-capability
                  (assoc policy
                         :operation operation
                         :plan-key operation))]))
            plans')))))

(defn operation-capabilities?
  "True for a canonical operation-keyed capability map produced by
   operation-capabilities.

   This predicate validates only the derived map's local closed shape: every key
   is a semantic operation keyword, every value is a canonical capability, and
   each capability's :operation and :plan-key equal its map key. It cannot by
   itself reconstruct or prove the originating browser plan map."
  [value]
  (and
   (map? value)
   (not (empty? value))
   (every?
    (fn [[operation capability]]
      (and
       (keyword? operation)
       (operation-capability? capability)
       (= operation (:operation capability))
       (= operation (:plan-key capability))))
    value)))

(defn require-operation-capabilities
  "Return capabilities when it is a canonical derived operation-capability map;
   otherwise throw a structured capability error."
  [capabilities]
  (when-not (operation-capabilities? capabilities)
    (capability-error
     :invalid-operation-capabilities
     "Expected a canonical operation-keyed optimistic capability map."
     {:capabilities capabilities}))
  capabilities)

(defn capability-for-operation
  "Resolve one semantic operation from a canonical derived capability map.

   Unknown operations fail immediately and report the closed set of available
   operation ids. This is diagnostic lookup only; it never guesses or silently
   substitutes a near operation and grants no execution authority."
  [capabilities operation]
  (let [capabilities'
        (require-operation-capabilities capabilities)
        operation'
        (require-keyword!
         "Optimistic capability semantic operation"
         operation)]
    (or
     (get capabilities' operation')
     (capability-error
      :unknown-operation
      "Optimistic capability map does not contain the requested semantic operation."
      {:operation operation'
       :available-operations (set (keys capabilities'))}))))

(defn bind
  "Bind one operation capability to per-render semantic input.

   Required binding data:

     :arguments
       Portable operation arguments.  Must be a map.

     :observed-basis
       Authoritative basis known by the rendering/view path.  Normalized through
       the shared optimistic protocol vocabulary.

   Optional per-render data:

     :scope
     :fact-versions
     :target-id

   The result is exactly the inert action-data shape consumed by
   gesso.live.ui/optimistic-action and the browser HTMX bridge.  It intentionally
   omits the capability type tag, command-id, execution-id, principal, authority,
   settlement state, and any claim of durable authorization.

   Capability-owned fields cannot be overridden by binding data because the
   binding map is closed.  This is the point of the abstraction: views supply
   the facts that vary per rendering while operation/runtime policy remains
   defined once next to the operation choreography."
  [capability binding]
  (let [capability' (require-operation-capability capability)
        binding'
        (require-closed-map!
         "Optimistic capability binding"
         binding
         binding-required-keys
         binding-optional-keys)
        arguments (:arguments binding')]
    (when-not (map? arguments)
      (capability-error
       :invalid-arguments
       "Optimistic capability binding :arguments must be a map."
       {:arguments arguments}))
    (when (contains? binding' :target-id)
      (require-target-id! (:target-id binding')))
    (let [base
          {:operation (:operation capability')
           :arguments arguments
           :observed-basis
           (protocol/normalize-basis
            :observed-basis
            (:observed-basis binding'))
           :plan-key (:plan-key capability')}
          with-capability-policy
          (reduce
           (fn [action key]
             (if (contains? capability' key)
               (assoc action key (get capability' key))
               action))
           base
           [:rollback-eligible?
            :timeout-ms
            :replace-owner?
            :replace-execution?])]
      (cond-> with-capability-policy
        (contains? binding' :scope)
        (assoc :scope
               (protocol/normalize-scope
                (:scope binding')))

        (contains? binding' :fact-versions)
        (assoc :fact-versions
               (protocol/normalize-fact-versions
                (:fact-versions binding')))

        (contains? binding' :target-id)
        (assoc :target-id (:target-id binding'))))))
