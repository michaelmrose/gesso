(ns gesso.live.schema
  "Malli schemas and validation helpers for current gesso.live boundaries.

   This namespace intentionally contains only contracts that correspond to
   live runtime or compilation boundaries that still exist. Historical API
   shapes belong in version history, not in the named schema registry: keeping
   an unenforced schema around makes the framework appear to promise semantics
   that no current consumer actually implements.

   These schemas are intended for edge validation, tests, helpful error
   messages, and API guardrails. They should not make the internal
   implementation noisy."
  (:require
   [clojure.string :as str]
   [gesso.live.progression :as progression]
   [malli.core :as m]
   [malli.error :as me]))

;; -----------------------------------------------------------------------------
;; Primitive-ish schemas
;; -----------------------------------------------------------------------------

(def Topic
  "A semantic app-level topic.

   Examples:
     :request
     :store-queue
     :demo-counter"
  keyword?)

(def ChangeKind
  "A coarse semantic change kind.

   Examples:
     :created
     :updated
     :deleted
     :changed"
  keyword?)

(def Id
  "An app-level identity value.

   Usually a string, keyword, UUID, integer, or similar stable value.

   We intentionally keep this broad because Gesso should not impose an id
   representation on apps."
  some?)

(def NonBlankString
  [:and
   string?
   [:fn {:error/message "must not be blank"}
    (fn [s]
      (not (str/blank? s)))]])

(def EventName
  "A normalized SSE event name."
  NonBlankString)

(def EventRef
  "An app-facing event reference.

   App code may provide a keyword, symbol, or string. Downstream code should
   normalize this into EventName before encoding SSE frames."
  [:or keyword? symbol? EventName])

(def Milliseconds
  "A non-negative millisecond value."
  [:and int? [:>= 0]])

(def ProgressionRequirement
  "One normalized authoritative refresh-progression requirement.

   The schema layer validates only the portable requirement shape owned by
   gesso.live.progression. It does not compare opaque bases, infer ordering, or
   accept the distinct wire representation used at transport boundaries."
  [:fn
   {:error/message "must be a normalized Gesso Live progression requirement"}
   progression/requirement?])

;; -----------------------------------------------------------------------------
;; Core live data
;; -----------------------------------------------------------------------------

(def PrimaryChange
  "A primary app-level change emitted by writer code.

   Primary changes are allowed to carry additional app/domain context. Gesso does
   not infer hidden before/after values."
  [:map
   [:topic Topic]
   [:id {:optional true} Id]
   [:change/kind {:optional true} ChangeKind]
   [:progression {:optional true} ProgressionRequirement]])

(def Invalidation
  "An expanded invalidation.

   Invalidations are what the shared source emits and what client flows match
   against.

   source/emit! should validate against this schema, not PrimaryChange."
  [:map
   [:topic Topic]
   [:id Id]
   [:change/kind {:optional true} ChangeKind]
   [:progression {:optional true} ProgressionRequirement]])

(def Subscription
  "A reader interest.

   A live fragment subscribes to a semantic topic/id pair."
  [:map
   [:topic Topic]
   [:id Id]])

(def LiveEvent
  "A normalized live event before SSE frame encoding."
  [:map
   [:event EventName]
   [:invalidation Invalidation]
   [:data {:optional true} any?]
   [:progression {:optional true} ProgressionRequirement]
   [:consistency-token {:optional true} any?]])

;; -----------------------------------------------------------------------------
;; Invalidation rules
;; -----------------------------------------------------------------------------

(def ExpansionFn
  "Function called to expand a primary change into invalidations.

   Shape:
     (fn [ctx change] ...)"
  fn?)

(def PredicateFn
  "Function called to test whether a rule applies.

   Shape:
     (fn [ctx change] ...)"
  fn?)

(def InvalidationRule
  "A generic invalidation expansion rule.

   A rule may use either or both:
     :when-topic
     :when

   If both are present, gesso.live.invalidation must define how they compose.
   Recommended first-pass behavior: both must match."
  [:and
   [:map
    [:when-topic {:optional true} Topic]
    [:when {:optional true} PredicateFn]
    [:expand ExpansionFn]]
   [:fn
    {:error/message "must include :when-topic or :when"}
    (fn [rule]
      (or (contains? rule :when-topic)
          (contains? rule :when)))]])

(def InvalidationRules
  [:sequential InvalidationRule])

(def InvalidationOptions
  [:map
   [:on-unmatched {:optional true} [:enum :keep :drop :throw]]
   [:dedupe? {:optional true} boolean?]])

;; -----------------------------------------------------------------------------
;; Current source, dispatcher, and flow option boundaries
;; -----------------------------------------------------------------------------

(def SourceOptions
  [:map
   [:id {:optional true} any?]
   [:coalesce-window-ms {:optional true} Milliseconds]
   [:on-error {:optional true} fn?]])

(def OverflowPolicy
  [:enum :block :throw :drop :coalesce])

(def DispatcherOptions
  "Options for constructing an async expansion dispatcher."
  [:map
   [:name {:optional true} NonBlankString]
   [:threads {:optional true} pos-int?]
   [:queue-size {:optional true} pos-int?]
   [:on-overflow {:optional true} OverflowPolicy]])

(def FlowForSubscriptionOptions
  [:map
   [:subscription Subscription]
   [:interested? fn?]])

(def InvalidationEventOptions
  [:map
   [:event {:optional true} EventRef]
   [:data {:optional true} any?]
   [:consistency-token {:optional true} any?]])

;; -----------------------------------------------------------------------------
;; Registry
;; -----------------------------------------------------------------------------

(def schemas
  "Named schema registry for current gesso.live contracts.

   A key belongs here only while a corresponding runtime or compilation
   boundary actually enforces the contract. Retired API shapes intentionally
   become unknown keys rather than remaining as misleading compatibility
   metadata."
  {:gesso.live/topic Topic
   :gesso.live/change-kind ChangeKind
   :gesso.live/id Id
   :gesso.live/non-blank-string NonBlankString
   :gesso.live/event-name EventName
   :gesso.live/event-ref EventRef
   :gesso.live/milliseconds Milliseconds
   :gesso.live/progression ProgressionRequirement

   :gesso.live/primary-change PrimaryChange
   :gesso.live/invalidation Invalidation
   :gesso.live/subscription Subscription
   :gesso.live/live-event LiveEvent

   :gesso.live/invalidation-rule InvalidationRule
   :gesso.live/invalidation-rules InvalidationRules
   :gesso.live/invalidation-options InvalidationOptions

   :gesso.live/source-options SourceOptions
   :gesso.live/overflow-policy OverflowPolicy
   :gesso.live/dispatcher-options DispatcherOptions
   :gesso.live/flow-for-subscription-options FlowForSubscriptionOptions
   :gesso.live/invalidation-event-options InvalidationEventOptions})

(def ^:private hot-schema-keys
  "Schemas validated on ordinary Live hot paths.

   Their Malli validators/explainers are compiled once at namespace load.
   Calling validator/explainer repeatedly for one of these named schemas returns
   the same compiled function instead of recompiling the schema."
  #{:gesso.live/primary-change
    :gesso.live/invalidation
    :gesso.live/subscription
    :gesso.live/live-event})

(def ^:private compiled-validators
  (into {}
        (map (fn [schema-key]
               [schema-key (m/validator (get schemas schema-key))]))
        hot-schema-keys))

(def ^:private compiled-explainers
  (into {}
        (map (fn [schema-key]
               [schema-key (m/explainer (get schemas schema-key))]))
        hot-schema-keys))

;; -----------------------------------------------------------------------------
;; Lookup and validation helpers
;; -----------------------------------------------------------------------------

(defn schema
  "Resolve a schema key or return the provided schema unchanged.

   Examples:
     (schema :gesso.live/invalidation)
     (schema Invalidation)"
  [schema-or-key]
  (if (keyword? schema-or-key)
    (or (get schemas schema-or-key)
        (throw (ex-info "Unknown gesso.live schema key."
                        {:schema-key schema-or-key
                         :known-schema-keys (sort (keys schemas))})))
    schema-or-key))

(defn validate
  "Return true if value conforms to schema-or-key.

   Named hot-path schemas reuse their precompiled Malli validators."
  [schema-or-key value]
  (let [schema' (schema schema-or-key)]
    (if-let [validate-fn (and (keyword? schema-or-key)
                              (get compiled-validators schema-or-key))]
      (validate-fn value)
      (m/validate schema' value))))

(defn explain-data
  "Return raw Malli explanation data for value.

   Named hot-path schemas reuse their precompiled Malli explainers."
  [schema-or-key value]
  (let [schema' (schema schema-or-key)]
    (if-let [explain-fn (and (keyword? schema-or-key)
                             (get compiled-explainers schema-or-key))]
      (explain-fn value)
      (m/explain schema' value))))

(defn humanize
  "Return a humanized Malli explanation for value."
  [schema-or-key value]
  (some-> (explain-data schema-or-key value)
          me/humanize))

(defn validate!
  "Validate value against schema-or-key.

   Returns value when valid.

   Throws ex-info with useful explanation data when invalid."
  [schema-or-key value]
  (if (validate schema-or-key value)
    value
    (let [explanation (explain-data schema-or-key value)]
      (throw
       (ex-info "Invalid gesso.live value."
                {:schema-key (when (keyword? schema-or-key)
                               schema-or-key)
                 :schema (schema schema-or-key)
                 :value value
                 :explanation explanation
                 :humanized (me/humanize explanation)})))))

(defn validator
  "Return a predicate function for schema-or-key.

   Named hot-path schemas return their namespace-load compiled validator."
  [schema-or-key]
  (let [schema' (schema schema-or-key)]
    (or (and (keyword? schema-or-key)
             (get compiled-validators schema-or-key))
        (m/validator schema'))))

(defn explainer
  "Return an explainer function for schema-or-key.

   Named hot-path schemas return their namespace-load compiled explainer."
  [schema-or-key]
  (let [schema' (schema schema-or-key)]
    (or (and (keyword? schema-or-key)
             (get compiled-explainers schema-or-key))
        (m/explainer schema'))))

;; -----------------------------------------------------------------------------
;; Convenience validators
;; -----------------------------------------------------------------------------

(defn validate-primary-change!
  [x]
  (validate! :gesso.live/primary-change x))

(defn validate-invalidation!
  [x]
  (validate! :gesso.live/invalidation x))

(defn validate-subscription!
  [x]
  (validate! :gesso.live/subscription x))

(defn validate-live-event!
  [x]
  (validate! :gesso.live/live-event x))
