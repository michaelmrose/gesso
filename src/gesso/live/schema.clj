(ns gesso.live.schema
  "Malli schemas and validation helpers for gesso.live.

   This namespace defines the small map-shaped contracts used by the live
   system. These schemas are intended for edge validation, tests, helpful error
   messages, and API guardrails.

   They should not make the internal implementation noisy."
  (:require
   [clojure.string :as str]
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

(def PositiveMilliseconds
  "A positive millisecond value."
  [:and int? [:> 0]])

(def Scopes
  "App-provided client scopes.

   Normalized client descriptors may use a set, but app hooks often naturally
   return a vector/list/etc. OOB code can normalize these to a set."
  [:or
   [:set any?]
   [:sequential any?]])

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
   [:change/kind {:optional true} ChangeKind]])

(def Invalidation
  "An expanded invalidation.

   Invalidations are what the shared source emits and what client flows match
   against.

   source/emit! should validate against this schema, not PrimaryChange."
  [:map
   [:topic Topic]
   [:id Id]
   [:change/kind {:optional true} ChangeKind]])

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
;; Source, dispatch, and flow options
;; -----------------------------------------------------------------------------

(def SourceOptions
  [:map
   [:id {:optional true} any?]
   [:coalesce-window-ms {:optional true} Milliseconds]
   [:on-error {:optional true} fn?]])

(def DispatchMode
  [:enum :sync :async])

(def OverflowPolicy
  [:enum :block :throw :drop :coalesce])

(def DispatchOptions
  "Per-emission dispatch options.

   These options describe how core/emit! should run expansion."
  [:map
   [:dispatch {:optional true} DispatchMode]
   [:dispatcher {:optional true} any?]
   [:on-overflow {:optional true} OverflowPolicy]
   [:consistency-token {:optional true} any?]
   [:ctx-data {:optional true} any?]])

(def DispatcherOptions
  "Options for constructing an async expansion dispatcher."
  [:map
   [:name {:optional true} NonBlankString]
   [:threads {:optional true} pos-int?]
   [:queue-size {:optional true} pos-int?]
   [:on-overflow {:optional true} OverflowPolicy]])

(def CoreEmitOptions
  "Options for gesso.live.core/emit!.

   core/emit! receives primary changes, expands them, and then emits expanded
   invalidations into the source."
  [:map
   [:source some?]
   [:rules {:optional true} InvalidationRules]
   [:ctx {:optional true} any?]
   [:dispatch {:optional true} DispatchMode]
   [:dispatcher {:optional true} any?]
   [:on-overflow {:optional true} OverflowPolicy]
   [:consistency-token {:optional true} any?]
   [:ctx-data {:optional true} any?]])

(def StreamHandlerOptions
  "Options for building a live SSE stream handler."
  [:map
   [:source some?]
   [:parse-subscription fn?]
   [:authorize-subscription fn?]
   [:interested? fn?]
   [:event {:optional true} EventRef]
   [:keepalive-ms {:optional true} PositiveMilliseconds]])

(def FlowForSubscriptionOptions
  [:map
   [:subscription Subscription]
   [:interested? fn?]])

(def InvalidationEventOptions
  [:map
   [:event {:optional true} EventRef]
   [:data {:optional true} any?]
   [:consistency-token {:optional true} any?]])

(def CoalesceByOptions
  [:map
   [:key-fn fn?]
   [:window-ms {:optional true} Milliseconds]])

(def IsolationOptions
  [:map
   [:on-error {:optional true} fn?]
   [:on-close {:optional true} fn?]])

;; -----------------------------------------------------------------------------
;; HTMX / fragment config
;; -----------------------------------------------------------------------------

(def FragmentConfig
  [:map
   [:subscription Subscription]
   [:fragment/id NonBlankString]
   [:fragment/src NonBlankString]
   [:fragment/swap {:optional true} NonBlankString]
   [:fragment/event {:optional true} EventRef]
   [:fragment/trigger {:optional true} NonBlankString]
   [:fragment/jitter-ms {:optional true} Milliseconds]
   [:fragment/attrs {:optional true} map?]
   [:fragment/inner-attrs {:optional true} map?]])

(def FragmentCacheOptions
  [:map
   [:key some?]
   [:ttl-ms {:optional true} PositiveMilliseconds]
   [:maximum-size {:optional true} pos-int?]])

(def FragmentSingleflightOptions
  [:map
   [:key some?]])

;; -----------------------------------------------------------------------------
;; SSE
;; -----------------------------------------------------------------------------

(def SseResponseOptions
  [:map
   [:flow some?]
   [:keepalive-ms {:optional true} PositiveMilliseconds]
   [:headers {:optional true} map?]])

(def SseFrameEvent
  [:or
   string?
   LiveEvent
   [:map
    [:event EventRef]
    [:data any?]]])

;; -----------------------------------------------------------------------------
;; OOB
;; -----------------------------------------------------------------------------

(def ClientDescriptor
  [:map
   [:client/id {:optional true} Id]
   [:client/user-id {:optional true} Id]
   [:client/scopes {:optional true} Scopes]
   [:client/connected-at {:optional true} int?]])

(def OobTarget
  [:or
   [:= :all]
   [:tuple [:= :client] Id]
   [:tuple [:= :user] Id]
   [:tuple [:= :scope] any?]])

(def OobSendOptions
  [:and
   [:map
    [:to OobTarget]
    [:fragments {:optional true} any?]
    [:oob {:optional true} any?]]
   [:fn
    {:error/message "must include :fragments or :oob"}
    (fn [m]
      (or (contains? m :fragments)
          (contains? m :oob)))]])

;; -----------------------------------------------------------------------------
;; Registry
;; -----------------------------------------------------------------------------

(def schemas
  "Named schema registry for gesso.live.

   These keys are intentionally public and stable enough to use in tests."
  {:gesso.live/topic Topic
   :gesso.live/change-kind ChangeKind
   :gesso.live/id Id
   :gesso.live/non-blank-string NonBlankString
   :gesso.live/event-name EventName
   :gesso.live/event-ref EventRef
   :gesso.live/milliseconds Milliseconds
   :gesso.live/positive-milliseconds PositiveMilliseconds
   :gesso.live/scopes Scopes

   :gesso.live/primary-change PrimaryChange
   :gesso.live/invalidation Invalidation
   :gesso.live/subscription Subscription
   :gesso.live/live-event LiveEvent

   :gesso.live/invalidation-rule InvalidationRule
   :gesso.live/invalidation-rules InvalidationRules
   :gesso.live/invalidation-options InvalidationOptions

   :gesso.live/source-options SourceOptions
   :gesso.live/dispatch-mode DispatchMode
   :gesso.live/overflow-policy OverflowPolicy
   :gesso.live/dispatch-options DispatchOptions
   :gesso.live/dispatcher-options DispatcherOptions
   :gesso.live/core-emit-options CoreEmitOptions
   :gesso.live/stream-handler-options StreamHandlerOptions
   :gesso.live/flow-for-subscription-options FlowForSubscriptionOptions
   :gesso.live/invalidation-event-options InvalidationEventOptions
   :gesso.live/coalesce-by-options CoalesceByOptions
   :gesso.live/isolation-options IsolationOptions

   :gesso.live/fragment-config FragmentConfig
   :gesso.live/fragment-cache-options FragmentCacheOptions
   :gesso.live/fragment-singleflight-options FragmentSingleflightOptions

   :gesso.live/sse-response-options SseResponseOptions
   :gesso.live/sse-frame-event SseFrameEvent

   :gesso.live/client-descriptor ClientDescriptor
   :gesso.live/oob-target OobTarget
   :gesso.live/oob-send-options OobSendOptions})

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
  "Return true if value conforms to schema-or-key."
  [schema-or-key value]
  (m/validate (schema schema-or-key) value))

(defn explain-data
  "Return raw Malli explanation data for value."
  [schema-or-key value]
  (m/explain (schema schema-or-key) value))

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
  "Return a predicate function for schema-or-key."
  [schema-or-key]
  (let [schema' (schema schema-or-key)]
    (m/validator schema')))

(defn explainer
  "Return an explainer function for schema-or-key."
  [schema-or-key]
  (let [schema' (schema schema-or-key)]
    (m/explainer schema')))

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

(defn validate-fragment-config!
  [x]
  (validate! :gesso.live/fragment-config x))
