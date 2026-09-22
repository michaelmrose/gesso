(ns gesso.live.ui
  "Hiccup convenience helpers for gesso.live.

   This namespace sits above gesso.live.htmx and below gesso.live.core.

   It owns user-facing markup helpers such as:

   - ->fragment
   - fragment-panel
   - live-script
   - post-form
   - post-button
   - anti-forgery-token
   - anti-forgery-input

   Continuity metadata is owned by gesso.live.continuity.

   Protocol-v3 optimism is rendered here only as inert server-authored
   annotation. Browser execution is owned by gesso.live.browser.optimistic and
   the HTMX bridge over the shared browser adapter. This namespace never creates
   command/execution identities, provisional DOM, timers, settlements, or
   browser execution state.

   It intentionally does not depend on gesso.live.core. Core can safely require
   this namespace and re-export its public helpers."
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [gesso.live.continuity :as continuity]
   [gesso.live.htmx :as htmx]
   [gesso.live.optimistic.capability :as optimistic.capability]
   [gesso.live.optimistic.protocol :as optimistic.protocol]))

;; -----------------------------------------------------------------------------
;; Defaults
;; -----------------------------------------------------------------------------

(def default-stream-base-url
  "/app/gesso/live/stream")

(def default-live-script-src
  "Default public path for the Gesso Live browser runtime.

   Downstream apps should include this script when they use browser-side
   Gesso Live behavior such as client-continuity capture/restore."
  "/gesso/gesso-live.js")

(def default-fragment-swap
  htmx/default-fragment-swap)

(def default-post-swap
  htmx/default-post-swap)

(def default-post-sync
  "closest [data-gesso-live-fragment]:drop")

(def default-post-include
  "Selector that includes the lightweight post-button wrapper form.

   The wrapper owns the anti-forgery input and any app-supplied hidden inputs.
   Additional :include selectors are appended rather than replacing this value."
  "closest [data-gesso-live-post]")

;; -----------------------------------------------------------------------------
;; Protocol-v3 optimistic HTMX annotation
;; -----------------------------------------------------------------------------

(def optimistic-action-attr
  "Inert server-rendered action annotation consumed by the protocol-v3 browser
   HTMX bridge. The value is portable EDN describing semantic operation input;
   it is not authorization and contains no command/execution identity."
  :data-gesso-live-optimistic)

(def optimistic-settlement-attr
  "Inert response marker consumed by the protocol-v3 browser HTMX bridge."
  :data-gesso-live-optimistic-settlement)

(def choreo-operation-option-key
  "Semantic Choreo operation option understood by operation-bound UI helpers.

   Application views use this key to name the operation an affordance actuates.
   The value is resolved through the validated operation-capability registry in
   render context; it is never treated as browser/server authority."
  :choreo/op)

(def choreo-affordance-metadata-key
  "Framework-owned Clojure metadata key marking one canonical :choreo/op
   affordance in rendered server-side Hiccup.

   The metadata value is the already-validated semantic operation keyword.
   Metadata is intentionally not emitted into HTML and is not browser authority;
   it exists so whole-application preflight can enumerate ordinary Gesso view
   declarations without an application-maintained affordance registry or a
   second rendered protocol attribute."
  ::choreo-affordance)

(def rendered-choreo-affordance-type
  :gesso.live.ui/rendered-choreo-affordance)

(def rendered-choreo-affordance-version
  1)

(def ^:private rendered-choreo-affordance-keys
  #{:gesso.live.ui/type
    :gesso.live.ui/version
    :kind
    :operation
    :plan-key
    :method
    :path
    :render-path})

(def optimistic-operation-capabilities-context-key
  "Framework-owned render-context key containing a canonical operation-keyed
   optimistic capability registry.

   Prefer with-optimistic-operation-capabilities to installing this value
   directly so malformed registries fail at the context assembly boundary."
  ::optimistic-operation-capabilities)

(def optimistic-action-required-keys
  #{:operation
    :arguments
    :observed-basis})

(def optimistic-action-optional-keys
  #{:scope
    :fact-versions
    :target-id
    :plan-key
    :rollback-eligible?
    :timeout-ms
    :replace-owner?
    :replace-execution?})

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn- ex
  [message data]
  (ex-info message data))

(defn- blank-string?
  [x]
  (and (string? x)
       (str/blank? x)))

(defn- nonblank-string?
  [x]
  (and (string? x)
       (not (str/blank? x))))

(defn- present?
  [x]
  (not (or (nil? x)
           (blank-string? x))))

(defn- require-present!
  [k value]
  (when-not (present? value)
    (throw
     (ex (str "gesso.live UI requires " k ".")
         {k value})))
  value)

(defn- normalize-name
  [x]
  (cond
    (keyword? x) (name x)
    (symbol? x)  (name x)
    (nil? x)     nil
    :else        (str x)))

(defn- subscription-token
  [subscription]
  (cond
    (nil? subscription)
    nil

    (string? subscription)
    subscription

    (keyword? subscription)
    (name subscription)

    (symbol? subscription)
    (name subscription)

    (map? subscription)
    (or (:token subscription)
        (:subscription/token subscription)
        (:id subscription)
        (some-> (:topic subscription)
                normalize-name))

    :else
    (str subscription)))

(defn- encode-query-value
  [x]
  (java.net.URLEncoder/encode (str x) "UTF-8"))

(defn- stream-url-from-token
  [base-url token]
  (str base-url
       (if (str/includes? base-url "?") "&" "?")
       "subscription="
       (encode-query-value token)))

(defn- legacy-fragment-map?
  [m]
  (or (contains? m :fragment/id)
      (contains? m :fragment/src)
      (contains? m :fragment/swap)
      (contains? m :subscription/token)))

(defn- canonical-fragment-map
  [m]
  (if (legacy-fragment-map? m)
    {:id (:fragment/id m)
     :src (:fragment/src m)
     :swap (:fragment/swap m)
     :subscription (:subscription/token m)
     :stream-url (:stream-url m)
     :stream-base-url (:stream/base-url m)
     :event (:event m)
     :trigger (:trigger m)
     :include (:include m)
     :client-continuity (:client-continuity m)
     :jitter-ms (:jitter-ms m)
     :jitter-delay-ms (:jitter-delay-ms m)
     :attrs (:attrs m)
     :root-attrs (:root-attrs m)
     :target-attrs (or (:target-attrs m)
                       (:inner-attrs m))}
    m))

(def ^:private unmanaged-fragment-refresh-option-keys
  [:trigger :jitter-ms :jitter-delay-ms])

(defn- reject-unmanaged-fragment-refresh-options!
  [fragment]
  (let [unsupported
        (into {}
              (keep
               (fn [k]
                 (let [value (get fragment k)]
                   (when (some? value)
                     [k value]))))
              unmanaged-fragment-refresh-option-keys)]
    (when (seq unsupported)
      (throw
       (ex
        (str
         "gesso.live UI managed fragments no longer accept direct HTMX "
         "refresh trigger/jitter options. Refresh intent must enter the "
         "browser adapter before HTMX requests are emitted.")
        {:unsupported-options unsupported})))
    fragment))

(defn- compact-map
  [m]
  (into {}
        (remove (comp nil? val))
        m))

(defn- include-selectors
  [include]
  (cond
    (or (nil? include)
        (false? include))
    []

    (string? include)
    (let [include' (str/trim include)]
      (when (str/blank? include')
        (throw
         (ex "gesso.live UI :include selectors must not be blank."
             {:include include})))
      [include'])

    (sequential? include)
    (mapcat include-selectors include)

    :else
    (throw
     (ex "gesso.live UI :include must be nil, false, a selector string, or a sequential collection of selector strings."
         {:include include}))))

(defn- post-include-value
  [include]
  (->> (concat [default-post-include]
               (include-selectors include))
       distinct
       (str/join ", ")))

(defn- button-children
  [{:keys [label children]}]
  (cond
    (nil? children) [label]
    (sequential? children) children
    :else [children]))

(defn- optimistic-ui-error
  [kind message data]
  (throw
   (ex-info
    message
    (merge
     {:error/type :gesso.live.ui/optimistic-error
      :error/kind kind}
     data))))

(defn with-optimistic-operation-capabilities
  "Install one canonical operation-keyed optimistic capability registry into a
   render context.

   The registry should normally come from
   gesso.live.optimistic.capability/operation-capabilities, which derives
   semantic operation identity from the operation-keyed browser ExecutablePlan
   registry. Installing it once lets view helpers bind an affordance by naming
   only :choreo/op plus per-render optimistic binding data.

   This is inert render configuration, not authorization. The browser remains
   adversarial and trusted server execution must still authenticate, authorize,
   resolve the operation through its own registry, and revalidate domain state."
  [ctx capabilities]
  (when-not (map? ctx)
    (optimistic-ui-error
     :invalid-render-context
     "gesso.live optimistic operation capabilities require a map render context."
     {:context ctx}))
  (assoc ctx
         optimistic-operation-capabilities-context-key
         (optimistic.capability/require-operation-capabilities capabilities)))

(defn- operation-capabilities-from-context
  [ctx operation]
  (when-not (map? ctx)
    (optimistic-ui-error
     :invalid-render-context
     "gesso.live semantic Choreo operation binding requires a map render context."
     {:context ctx
      :operation operation}))
  (if (contains? ctx optimistic-operation-capabilities-context-key)
    (optimistic.capability/require-operation-capabilities
     (get ctx optimistic-operation-capabilities-context-key))
    (optimistic-ui-error
     :missing-operation-capabilities
     "gesso.live semantic Choreo operation binding requires an operation-capability registry in render context."
     {:operation operation
      :context-key optimistic-operation-capabilities-context-key})))

(defn- require-optimistic-map!
  [value]
  (when-not (map? value)
    (optimistic-ui-error
     :invalid-action
     "gesso.live UI :optimistic must be a protocol-v3 action map."
     {:value value}))
  value)

(defn- require-optimistic-closed-map!
  [action]
  (let [action (require-optimistic-map! action)
        keys' (set (keys action))
        allowed (into optimistic-action-required-keys
                      optimistic-action-optional-keys)
        missing (set (remove keys' optimistic-action-required-keys))
        unknown (set (remove allowed keys'))]
    (when (seq missing)
      (optimistic-ui-error
       :missing-action-fields
       "gesso.live UI optimistic action is missing required fields."
       {:missing missing
        :required optimistic-action-required-keys
        :value action}))
    (when (seq unknown)
      (optimistic-ui-error
       :unknown-action-fields
       "gesso.live UI optimistic action contains unsupported fields."
       {:unknown unknown
        :allowed allowed
        :value action}))
    action))

(defn- require-optimistic-boolean!
  [k value]
  (when-not (instance? Boolean value)
    (optimistic-ui-error
     :invalid-action-option
     (str "gesso.live UI optimistic " k " must be boolean.")
     {:key k
      :value value}))
  value)

(defn- require-optimistic-timeout!
  [value]
  (when-not (or (nil? value)
                (and (integer? value)
                     (not (neg? value))))
    (optimistic-ui-error
     :invalid-action-option
     "gesso.live UI optimistic :timeout-ms must be nil or a non-negative integer."
     {:key :timeout-ms
      :value value}))
  value)

(defn optimistic-action
  "Validate and normalize one protocol-v3 optimistic action annotation.

   One-arity accepts the low-level inert action map. This remains useful for
   framework code, tests, and staged migration.

   Two-arity accepts a canonical gesso.live.optimistic.capability operation
   capability plus per-render binding data. This is the preferred explicit
   capability-binding path: model/choreography code owns operation identity and
   browser policy,
   while the view supplies only arguments, observed authoritative basis, scope,
   fact versions, and logical target identity.

   Neither call shape creates a command or grants authorization. In particular
   the resulting action does not contain command-id, execution-id, principal,
   authority, or settlement fields. The browser bridge allocates protocol
   identities at request time and the trusted server re-authenticates and
   re-authorizes the operation.

   Low-level action required fields:
     :operation
     :arguments
     :observed-basis

   Low-level action optional fields:
     :scope
     :fact-versions
     :target-id
     :plan-key
     :rollback-eligible?
     :timeout-ms
     :replace-owner?
     :replace-execution?"
  ([action]
   (let [action (require-optimistic-closed-map! action)
         operation (:operation action)
         arguments (:arguments action)]
     (when-not (keyword? operation)
       (optimistic-ui-error
        :invalid-operation
        "gesso.live UI optimistic :operation must be a keyword."
        {:operation operation}))
     (when-not (map? arguments)
       (optimistic-ui-error
        :invalid-arguments
        "gesso.live UI optimistic :arguments must be a map."
        {:arguments arguments}))
     (when (contains? action :target-id)
       (let [target-id (:target-id action)]
         (when-not (and (string? target-id)
                        (not (str/blank? target-id)))
           (optimistic-ui-error
            :invalid-action-option
            "gesso.live UI optimistic :target-id must be a non-blank string."
            {:key :target-id
             :value target-id}))))
     (when (contains? action :plan-key)
       (when (nil? (:plan-key action))
         (optimistic-ui-error
          :invalid-action-option
          "gesso.live UI optimistic :plan-key must not be nil when supplied."
          {:key :plan-key
           :value nil})))
     (doseq [k [:rollback-eligible?
                :replace-owner?
                :replace-execution?]]
       (when (contains? action k)
         (require-optimistic-boolean! k (get action k))))
     (when (contains? action :timeout-ms)
       (require-optimistic-timeout! (:timeout-ms action)))
     (cond->
      {:operation operation
       :arguments arguments
       :observed-basis
       (optimistic.protocol/normalize-basis
        :observed-basis
        (:observed-basis action))}
       (contains? action :scope)
       (assoc :scope
              (optimistic.protocol/normalize-scope
               (:scope action)))

       (contains? action :fact-versions)
       (assoc :fact-versions
              (optimistic.protocol/normalize-fact-versions
               (:fact-versions action)))

       (contains? action :target-id)
       (assoc :target-id (:target-id action))

       (contains? action :plan-key)
       (assoc :plan-key (:plan-key action))

       (contains? action :rollback-eligible?)
       (assoc :rollback-eligible? (:rollback-eligible? action))

       (contains? action :timeout-ms)
       (assoc :timeout-ms (:timeout-ms action))

       (contains? action :replace-owner?)
       (assoc :replace-owner? (:replace-owner? action))

       (contains? action :replace-execution?)
       (assoc :replace-execution? (:replace-execution? action)))))
  ([capability binding]
   (optimistic-action
    (optimistic.capability/bind capability binding))))
(defn- encode-portable-edn
  [label value]
  (let [encoded (pr-str value)]
    (try
      ;; The browser bridge uses cljs.reader/read-string. Requiring ordinary EDN
      ;; readability here prevents host objects from leaking into an attribute
      ;; that the browser could never reconstruct as portable semantic data.
      (edn/read-string encoded)
      encoded
      (catch Exception cause
        (throw
         (ex-info
          (str label " must be portable EDN.")
          {:error/type :gesso.live.ui/optimistic-error
           :error/kind :non-portable-edn
           :value value}
          cause))))))

(defn optimistic-action-attrs
  "Return inert HTML attrs for one protocol-v3 optimistic action.

   One-arity accepts a low-level action map. Two-arity accepts an operation
   capability plus its per-render binding. The encoded EDN is read by
   gesso.live.browser.optimistic-htmx and contains no trusted principal or
   browser-generated correlation identity."
  ([action]
   {optimistic-action-attr
    (encode-portable-edn
     "gesso.live UI optimistic action"
     (optimistic-action action))})
  ([capability binding]
   (optimistic-action-attrs
    (optimistic-action capability binding))))

(defn- mark-choreo-post-button
  [hiccup operation]
  (mapv
   (fn [node]
     (if (and (vector? node)
              (= :button (first node)))
       (with-meta
         node
         (assoc (meta node)
                choreo-affordance-metadata-key
                operation))
       node))
   hiccup))

(defn rendered-choreo-affordance?
  "True for one closed rendered Choreo affordance descriptor produced by
   rendered-choreo-affordances.

   This is a rendered UI declaration, not authority.  :path is the concrete
   hx-post coordinate present in the Hiccup node; matching that coordinate to a
   trusted route template is a later application-preflight edge."
  [value]
  (and
   (map? value)
   (= rendered-choreo-affordance-keys (set (keys value)))
   (= rendered-choreo-affordance-type
      (:gesso.live.ui/type value))
   (= rendered-choreo-affordance-version
      (:gesso.live.ui/version value))
   (= :post-button (:kind value))
   (keyword? (:operation value))
   (= (:operation value) (:plan-key value))
   (= :post (:method value))
   (nonblank-string? (:path value))
   (vector? (:render-path value))
   (every? #(and (integer? %) (not (neg? %))) (:render-path value))))

(defn- affordance-ui-error
  [kind message data]
  (throw
   (ex-info
    message
    (merge
     {:error/type :gesso.live.ui/affordance-error
      :error/kind kind}
     data))))

(defn- read-affordance-edn
  [label value render-path]
  (when-not (string? value)
    (affordance-ui-error
     :invalid-rendered-affordance-encoding
     (str label " must be an EDN string in rendered Hiccup.")
     {:render-path render-path
      :value value}))
  (try
    (edn/read-string value)
    (catch Exception cause
      (throw
       (ex-info
        (str label " contains unreadable EDN in rendered Hiccup.")
        {:error/type :gesso.live.ui/affordance-error
         :error/kind :invalid-rendered-affordance-encoding
         :render-path render-path
         :value value}
        cause)))))

(defn- node-rendered-choreo-affordance
  [node render-path]
  (when (and (vector? node)
             (contains? (meta node) choreo-affordance-metadata-key))
    (let [operation
          (get (meta node) choreo-affordance-metadata-key)]
      (when-not (and (= :button (first node))
                     (map? (second node)))
        (affordance-ui-error
         :invalid-rendered-affordance-button
         "gesso.live rendered Choreo affordance metadata must annotate a Hiccup :button with an attrs map."
         {:render-path render-path
          :operation operation
          :node node}))
      (let [attrs (second node)
            encoded-action
            (get attrs optimistic-action-attr)
            raw-action
            (read-affordance-edn
             "gesso.live rendered Choreo optimistic action"
             encoded-action
             render-path)
            action
            (try
              (optimistic-action raw-action)
              (catch Exception cause
                (throw
                 (ex-info
                  "gesso.live rendered Choreo affordance contains an invalid optimistic action."
                  {:error/type :gesso.live.ui/affordance-error
                   :error/kind :invalid-rendered-affordance-action
                   :render-path render-path
                   :operation operation
                   :action raw-action}
                  cause))))
            path (:hx-post attrs)]
        (when-not (keyword? operation)
          (affordance-ui-error
           :invalid-rendered-affordance-operation
           "gesso.live rendered Choreo affordance metadata must contain a semantic operation keyword."
           {:render-path render-path
            :operation operation}))
        (when-not (= operation (:operation action))
          (affordance-ui-error
           :rendered-affordance-operation-mismatch
           "gesso.live rendered Choreo affordance metadata does not match its optimistic action operation."
           {:render-path render-path
            :operation operation
            :action-operation (:operation action)}))
        (when-not (= operation (:plan-key action))
          (affordance-ui-error
           :rendered-affordance-plan-mismatch
           "gesso.live rendered Choreo affordance plan key does not match its semantic operation."
           {:render-path render-path
            :operation operation
            :plan-key (:plan-key action)}))
        (when-not (= "button" (:type attrs))
          (affordance-ui-error
           :invalid-rendered-affordance-button
           "gesso.live rendered Choreo POST affordance must remain a type=button node."
           {:render-path render-path
            :operation operation
            :type (:type attrs)}))
        (when-not (nonblank-string? path)
          (affordance-ui-error
           :missing-rendered-affordance-path
           "gesso.live rendered Choreo POST affordance must contain a non-blank hx-post path."
           {:render-path render-path
            :operation operation
            :path path}))
        {:gesso.live.ui/type rendered-choreo-affordance-type
         :gesso.live.ui/version rendered-choreo-affordance-version
         :kind :post-button
         :operation operation
         :plan-key (:plan-key action)
         :method :post
         :path path
         :render-path render-path}))))

(defn rendered-choreo-affordances
  "Enumerate canonical Choreo affordances from ordinary rendered Hiccup.

   post-button derives framework-owned Clojure metadata from the already-validated
   :choreo/op option.  This scanner joins that metadata to the same node's inert
   protocol-v3 action and physical hx-post coordinate, so callers do not
   maintain a second affordance registry.  The metadata is consumed before HTML
   serialization and does not become a browser protocol or authorization fact.

   The result preserves every rendered occurrence in structural Hiccup order;
   repeated buttons for the same semantic operation remain separate descriptors
   with different :render-path values.  Ordinary HTMX buttons and the lower-
   level :optimistic escape hatch are intentionally not promoted into canonical
   Choreo affordances.

   Malformed or tampered framework metadata fails closed with structured
   :gesso.live.ui/affordance-error data rather than disappearing from the scan."
  [hiccup]
  (letfn [(walk [value render-path]
            (lazy-seq
             (concat
              (when-let [affordance
                         (node-rendered-choreo-affordance value render-path)]
                [affordance])
              (when (sequential? value)
                (mapcat
                 (fn [[index child]]
                   (walk child (conj render-path index)))
                 (map-indexed vector value))))))]
    (vec (walk hiccup []))))

(defn optimistic-settlement-marker
  "Render an inert protocol-v3 settlement marker for an HTMX response.

   The marker is transport correlation only. Its settlement must already have
   been constructed by the trusted optimistic server boundary. The browser
   bridge matches command-id and execution-id before delivery."
  [settlement]
  (let [settlement'
        (optimistic.protocol/settlement
         (dissoc settlement
                 optimistic.protocol/protocol-version-key))]
    [:template
     {optimistic-settlement-attr
      (encode-portable-edn
       "gesso.live UI optimistic settlement"
       (optimistic.protocol/settlement->wire settlement'))}]))

;; -----------------------------------------------------------------------------
;; Fragment descriptor
;; -----------------------------------------------------------------------------

(defn ->fragment
  "Create an adapter-managed live fragment descriptor.

   Preferred shape:

     (live/->fragment
      {:id \"simple-shared-counter-fragment\"
       :src \"/app/demo/simple-shared-counter/fragment\"
       :subscription {:topic :demo-counter
                      :id \"global-shared-counter\"}
       :stream-url \"/app/gesso/live/stream?subscription=shared-counter\"
       :swap \"outerHTML\"})

   Legacy config maps are also accepted for migration:

     {:subscription/token \"shared-counter\"
      :fragment/id \"simple-shared-counter-fragment\"
      :fragment/src \"/app/demo/simple-shared-counter/fragment\"
      :fragment/swap \"outerHTML\"}

   Required:
     :id
     :src
     either :stream-url or :subscription / :subscription-token

   Optional:
     :stream-base-url
     :event
     :swap
     :include
     :client-continuity
     :attrs / :root-attrs
     :target-attrs / :inner-attrs

   Legacy :trigger, :jitter-ms, and :jitter-delay-ms options are rejected for
   managed fragments. Those options allowed HTMX request timing to bypass the
   browser adapter. Refresh intent now enters the adapter first and only an
   admitted :fragment/refresh effect may trigger the fragment GET.

   :event names the advisory SSE event to subscribe to. Its payload is not
   installed as fragment HTML. fragment-panel renders a stable, non-swapping
   invalidation listener that turns the named SSE message into an adapter
   invalidation.

   :client-continuity is app-facing, Clojure/data-first configuration for
   preserving browser interaction context across fragment refreshes. Examples
   include scroll anchoring, focus/caret restoration, preserved DOM islands, and
   custom capture/restore boxes. This namespace stores the config on the
   fragment descriptor and delegates continuity metadata construction to
   gesso.live.continuity.

   Markup model:
     stable outer root
       owns one EventSource, managed HTMX request attrs, fragment identity,
       and optional client-continuity metadata;

     stable invalidation listener
       subscribes to the named SSE event without owning an EventSource or GET;

     replaceable inner target
       owns only the canonical fragment DOM id plus target attrs."
  [fragment]
  (let [fragment'   (-> fragment
                        canonical-fragment-map
                        reject-unmanaged-fragment-refresh-options!)
        id'         (require-present! :id (:id fragment'))
        src'        (require-present! :src (:src fragment'))
        token       (or (:subscription/token fragment')
                        (subscription-token (:subscription fragment')))
        base-url    (or (:stream-base-url fragment')
                        default-stream-base-url)
        stream-url' (or (:stream-url fragment')
                        (when token
                          (stream-url-from-token base-url token)))]
    (when-not (present? stream-url')
      (throw
       (ex "gesso.live UI fragment requires :stream-url or :subscription."
           {:fragment fragment})))
    (merge
     {:gesso.live.ui/type :gesso.live.ui/fragment
      :id id'
      :src src'
      :stream-url stream-url'
      :subscription (:subscription fragment')
      :subscription/token token
      :event (or (:event fragment') htmx/default-event)
      :swap (or (:swap fragment') default-fragment-swap)
      :attrs {}
      :root-attrs {}
      :target-attrs {}}
     (compact-map
      {:include (:include fragment')
       :client-continuity (:client-continuity fragment')
       :attrs (:attrs fragment')
       :root-attrs (:root-attrs fragment')
       :target-attrs (:target-attrs fragment')}))))

(defn fragment?
  [x]
  (= :gesso.live.ui/fragment
     (:gesso.live.ui/type x)))

(defn ensure-fragment
  [fragment]
  (if (fragment? fragment)
    fragment
    (->fragment fragment)))

;; -----------------------------------------------------------------------------
;; Fragment markup
;; -----------------------------------------------------------------------------

(defn fragment-root-attrs
  "Build attrs for the stable outer adapter-managed live-fragment wrapper.

   The outer wrapper owns:
     - the one SSE connection for this fragment;
     - the logical fragment identity;
     - the adapter-authorized HTMX refresh request;
     - optional hx-include and client-continuity metadata.

   Its HTMX request trigger is always gesso:live-refresh. SSE events therefore
   cannot issue the GET directly; they are observed by the stable invalidation
   listener rendered by fragment-panel and normalized through
   gesso.live.browser.adapter first.

   Caller :attrs / :root-attrs remain useful for ordinary decoration and extra
   HTMX extensions, but they cannot replace the managed SSE connection, logical
   fragment identity, or hx-get/hx-trigger/hx-target/hx-swap ownership."
  [fragment]
  (let [{:keys [stream-url
                src
                swap
                include
                client-continuity
                id
                attrs
                root-attrs]} (ensure-fragment fragment)]
    (htmx/merge-attrs
     ;; Compose raw caller attrs first, then restore framework-owned SSE
     ;; connection identity. merge-sse-attrs preserves any additional hx-ext
     ;; values while guaranteeing the sse extension remains installed.
     (htmx/merge-sse-attrs
      attrs
      root-attrs
      {:sse-connect stream-url})
     (when include
       {:hx-include include})
     (when client-continuity
       (continuity/client-continuity-attrs
        {:fragment-id id
         :client-continuity client-continuity}))
     {:data-gesso-live-fragment id}
     ;; Request ownership comes last so raw attrs cannot recreate an alternate
     ;; trigger, target, swap, or source around the adapter.
     (htmx/managed-fragment-refresh-attrs
      {:src src
       :target id
       :swap swap}))))

(defn fragment-invalidation-listener-attrs
  "Build attrs for the stable, non-swapping SSE invalidation listener.

   The listener is a sibling of the replaceable fragment target. Its sole job
   is to make htmx-ext-sse subscribe to the configured named event.
   gesso.live.browser.core intercepts htmx:sseBeforeMessage for this marked
   element, prevents the direct SSE swap path, and notifies the adapter.

   hx-swap=none is defensive fallback behavior if the browser runtime is absent:
   advisory SSE payload bytes still must not become fragment HTML. The listener
   deliberately does not own sse-connect, so fragment-panel creates exactly one
   EventSource on the stable root."
  [fragment]
  (let [{:keys [id event]} (ensure-fragment fragment)]
    {:data-gesso-live-invalidation id
     :sse-swap (htmx/event-name event)
     :hx-swap "none"
     :aria-hidden "true"}))

(defn fragment-target-attrs
  "Build attrs for the replaceable inner fragment target.

   The target intentionally does not own hx-get, hx-trigger, SSE, or
   client-continuity attrs. Its :id is framework-owned because that physical id
   must match the stable root's logical fragment identity and managed hx-target.
   Caller :target-attrs may decorate the target but cannot rename it."
  [fragment]
  (let [{:keys [id target-attrs]} (ensure-fragment fragment)]
    (htmx/clean-attrs
     (merge
      target-attrs
      {:id id}))))

(defn fragment-panel
  "Render one adapter-managed live fragment panel.

   Stable outer root:
     - owns hx-ext=sse and sse-connect;
     - owns data-gesso-live-fragment;
     - owns the managed gesso:live-refresh -> hx-get path;
     - owns optional client-continuity metadata.

   Stable invalidation listener:
     - subscribes to the configured named SSE event with sse-swap;
     - is marked data-gesso-live-invalidation=<fragment-id>;
     - never owns an EventSource or an HTTP request;
     - uses hx-swap=none as fail-safe non-rendering behavior.

   Replaceable inner target:
     - owns only the canonical fragment DOM id plus target attrs.

   This shape makes the browser adapter the only path from advisory wakeup to
   fragment GET while keeping both the EventSource and invalidation listener
   stable across outerHTML replacement of the inner target."
  [fragment]
  (let [fragment' (ensure-fragment fragment)]
    [:div (fragment-root-attrs fragment')
     [:div (fragment-invalidation-listener-attrs fragment')]
     [:div (fragment-target-attrs fragment')]]))

;; -----------------------------------------------------------------------------
;; Browser runtime script helper
;; -----------------------------------------------------------------------------

(defn live-script
  "Render the Gesso Live browser runtime script tag.

   The runtime is framework-owned browser code for live-fragment behavior such as
   client-continuity capture/restore. It is intentionally separate from
   gesso-theme.js and from downstream app-owned main.js files.

   Options:
     :src
       Override script URL. Defaults to /gesso/gesso-live.js.

     :attrs
       Extra attrs merged last. Use this for cache-busting, nonce, integrity,
       crossorigin, or to override :defer."
  ([] (live-script nil))
  ([{:keys [src attrs]}]
   [:script
    (htmx/clean-attrs
     (merge
      {:src (or src default-live-script-src)
       :defer true}
      attrs))]))

;; -----------------------------------------------------------------------------
;; Anti-forgery helpers
;; -----------------------------------------------------------------------------

(defn anti-forgery-token
  "Extract a Biff/Ring-style anti-forgery token from ctx."
  [ctx]
  (or (:anti-forgery-token ctx)
      (:biff/anti-forgery-token ctx)
      (get-in ctx [:session :anti-forgery-token])))

(defn anti-forgery-input
  "Return a hidden anti-forgery input when ctx contains a token."
  [ctx]
  (when-let [token (anti-forgery-token ctx)]
    [:input {:type "hidden"
             :name "__anti-forgery-token"
             :value token}]))

;; -----------------------------------------------------------------------------
;; POST form helper
;; -----------------------------------------------------------------------------

(defn post-form-attrs
  "Build attrs for a POST form that refreshes a live fragment target.

   Required:
     :to

   Optional:
     :target
     :swap
     :attrs
     :native-action?
     :sync

   If :target is a fragment descriptor, its :id is used.

   This delegates to gesso.live.htmx/post-form-attrs. That helper intentionally
   omits native :action by default, so missed HTMX submits do not navigate to
   fragment-only mutation routes."
  [{:keys [to target swap attrs native-action? sync]
    :or {swap default-post-swap
         sync default-post-sync}}]
  (let [to'     (require-present! :to to)
        target' (if (fragment? target)
                  (:id target)
                  target)
        request (cond-> {:to to'
                         :target target'
                         :swap swap
                         :native-action? native-action?
                         :attrs attrs}
                  (some? sync) (assoc :sync sync))]
    (htmx/post-form-attrs request)))

(defn post-form
  "Render a POST form with anti-forgery input.

   This is useful when you explicitly want form semantics.

   For ordinary live buttons, prefer post-button. post-button uses type=button
   with hx-post directly on the button so missed HTMX events cannot fall back to
   native form submission."
  [ctx opts & children]
  (into
   [:form (post-form-attrs opts)]
   (concat
    (keep identity [(anti-forgery-input ctx)])
    children)))

;; -----------------------------------------------------------------------------
;; POST button helpers
;; -----------------------------------------------------------------------------

(defn- post-button-args
  [fragment-or-opts maybe-opts]
  (if maybe-opts
    (let [fragment (ensure-fragment fragment-or-opts)]
      [(assoc (or maybe-opts {})
              :target (or (:target maybe-opts)
                          (:id fragment))
              :swap (or (:swap maybe-opts)
                        (:swap fragment)
                        default-post-swap))
       fragment])
    [(or fragment-or-opts {}) nil]))

(defn- post-button-attrs
  [{:keys [to
           target
           include
           button-attrs
           request-attrs
           protocol-attrs]
    :as opts}]
  (let [swap (if (contains? opts :swap)
               (:swap opts)
               default-post-swap)
        sync (if (contains? opts :sync)
               (:sync opts)
               default-post-sync)]
    (htmx/merge-attrs
     request-attrs
     {:type "button"
      :hx-post (require-present! :to to)
      :hx-swap swap
      :hx-include (post-include-value include)}
     (when sync
       {:hx-sync sync})
     (when-let [target' (htmx/normalize-target target)]
       {:hx-target target'})
     button-attrs
     protocol-attrs)))

(defn- post-button-form-attrs
  [form-attrs]
  (htmx/merge-attrs
   form-attrs
   {:data-gesso-live-post true}))

(defn- render-post-button
  [ctx opts]
  (into
   [:form (post-button-form-attrs (:form-attrs opts))]
   (concat
    (keep identity [(anti-forgery-input ctx)])
    [(into
      [:button (post-button-attrs opts)]
      (button-children opts))])))

(defn- render-ordinary-post-button
  [ctx opts]
  (render-post-button
   ctx
   opts))

(defn- capability-shaped?
  [value]
  (and (map? value)
       (contains? value optimistic.capability/capability-type-key)))

(defn- resolve-post-button-optimistic
  [ctx opts]
  (let [operation-present?
        (contains? opts choreo-operation-option-key)

        operation
        (get opts choreo-operation-option-key)

        optimistic-present?
        (contains? opts :optimistic)

        optimistic-value
        (:optimistic opts)

        binding-present?
        (contains? opts :optimistic-binding)

        binding
        (:optimistic-binding opts)]
    (cond
      operation-present?
      (do
        (when optimistic-present?
          (optimistic-ui-error
           :conflicting-optimistic-declarations
           "gesso.live UI :choreo/op cannot be combined with the legacy/escape-hatch :optimistic declaration."
           {:operation operation
            :optimistic optimistic-value}))
        (when-not binding-present?
          (optimistic-ui-error
           :missing-optimistic-binding
           "gesso.live UI :choreo/op requires :optimistic-binding."
           {:operation operation}))
        (optimistic-action
         (optimistic.capability/capability-for-operation
          (operation-capabilities-from-context ctx operation)
          operation)
         binding))

      (or (nil? optimistic-value)
          (false? optimistic-value))
      (do
        (when binding-present?
          (optimistic-ui-error
           :orphan-optimistic-binding
           "gesso.live UI :optimistic-binding requires :choreo/op or an optimistic operation capability."
           {:optimistic optimistic-value
            :optimistic-binding binding}))
        nil)

      (capability-shaped? optimistic-value)
      (do
        (when-not binding-present?
          (optimistic-ui-error
           :missing-optimistic-binding
           "gesso.live UI optimistic operation capability requires :optimistic-binding."
           {:optimistic optimistic-value}))
        (optimistic-action optimistic-value binding))

      binding-present?
      (optimistic-ui-error
       :unexpected-optimistic-binding
       "gesso.live UI :optimistic-binding is accepted only with :choreo/op or an optimistic operation capability."
       {:optimistic optimistic-value
        :optimistic-binding binding})

      :else
      (optimistic-action optimistic-value))))

(defn post-button
  "Render a tiny HTMX POST button.

   Supported call shapes:

     (post-button
      ctx
      {:to \"/increment\"
       :target \"counter-fragment\"
       :label \"+\"})

     (post-button
      ctx
      fragment
      {:to \"/increment\"
       :label \"+\"})

   Options:

     :to
       POST target.

     :label
       Button label when :children is absent.

     :children
       Button children. A single non-sequential value is accepted.

     :target
       HTMX target. Defaults to fragment id in the 3-arity form.

     :swap
       HTMX swap. Defaults to fragment swap in the 3-arity form, otherwise
       \"innerHTML\".

     :sync
       HTMX request synchronization. Defaults to
       \"closest [data-gesso-live-fragment]:drop\". Explicit nil or false
       disables hx-sync.

     :include
       One additional hx-include selector, or a sequential collection of
       selectors. These append to the required lightweight wrapper-form
       selector.

     :form-attrs
       Extra attrs merged into the lightweight wrapper form.

     :button-attrs
       Extra attrs merged into button attrs.

     :optimistic
       Optional protocol-v3 optimistic action map, or a canonical
       gesso.live.optimistic.capability operation capability. These remain
       lower-level/migration paths. Application views should normally prefer
       :choreo/op so capability identity is resolved from assembled context.

     :choreo/op
       Preferred semantic operation declaration for application views. The
       operation is resolved through the canonical operation-capability registry
       installed in ctx with with-optimistic-operation-capabilities. A view does
       not fetch/pass a capability object and cannot choose a second plan key.
       The rendered button also carries framework-owned Clojure metadata used by
       rendered-choreo-affordances before HTML serialization. The metadata is
       not emitted to the browser and does not change authorization semantics.

     :optimistic-binding
       Required with :choreo/op and when :optimistic is an operation capability;
       rejected with a raw action map. Supplies only per-render arguments,
       observed basis, scope/fact versions, and target identity.

       The semantic-operation, explicit-capability, and raw-action forms emit
       the same inert data-gesso-live-optimistic EDN annotation.
       The browser bridge allocates command/execution identities and realizes
       optimism through the shared adapter; this helper does not render
       provisional templates or grant server authority.

   The clicked button owns hx-post. The lightweight wrapper form owns
   anti-forgery and app-supplied hidden inputs only, avoiding native-submit
   fallback if HTMX does not intercept the click."
  ([ctx opts]
   (post-button
    ctx
    opts
    nil))
  ([ctx fragment-or-opts maybe-opts]
   (let [[opts _fragment]
         (post-button-args
          fragment-or-opts
          maybe-opts)
         optimistic-action'
         (resolve-post-button-optimistic ctx opts)
         choreo-operation
         (when (contains? opts choreo-operation-option-key)
           (:operation optimistic-action'))
         opts'
         (dissoc opts :optimistic :optimistic-binding choreo-operation-option-key)]
     (if (nil? optimistic-action')
       (render-ordinary-post-button
        ctx
        opts')
       (let [rendered
             (render-post-button
              ctx
              (assoc opts'
                     :protocol-attrs
                     (htmx/merge-attrs
                      (:protocol-attrs opts')
                      (optimistic-action-attrs
                       optimistic-action'))))]
         (if choreo-operation
           (mark-choreo-post-button rendered choreo-operation)
           rendered))))))
