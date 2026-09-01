(ns gesso.live.core
  "Public orchestration facade for gesso.live.

   This namespace wires the lower-level live pieces together:

   - plain-data live model validation/compilation
   - invalidation rules
   - async dispatch
   - hot source broadcast
   - per-client flow construction
   - SSE transport startup
   - fragment render protection
   - app-facing XTDB2 consistency/progression helpers
   - app-facing HTMX/UI helpers
   - protocol-v3 optimistic capability/UI facade
   - protocol-v3 optimistic trusted-server facade

   It intentionally stays thin. The specialized namespaces still own their own
   behavior:

     gesso.live.model
       owns plain-data app live metadata, validation, graph expansion, and
       model fragment query/render helpers

     gesso.live.invalidation
       expands primary app changes into invalidations

     gesso.live.dispatch
       runs async jobs

     gesso.live.source
       owns hot Manifold broadcast

     gesso.live.flow
       owns per-client Missionary filtering/coalescing

     gesso.live.transport.sse
       owns SSE frame formatting and response stream lifecycle

     gesso.live.fragment
       owns render singleflight/cache protection and model fragment -> UI
       fragment adapters

     gesso.live.consistency.xtdb
       owns XTDB2 query/transaction consistency helpers

     gesso.live.progression.http
       owns browser-to-server progression HTTP encoding/decoding and request
       context binding

     gesso.live.htmx
       owns browser-facing raw HTMX attribute builders

     gesso.live.ui
       owns Hiccup convenience helpers for live fragments and POST controls

     gesso.live.optimistic.capability
       owns portable typed operation capabilities for optimistic UI
       composition

     gesso.live.optimistic.server
       owns the trusted protocol-v3 optimistic command boundary, operation
       registry, authority projection, and settlement-send preparation"
  (:require
   [gesso.live.consistency.xtdb :as live.xtdb]
   [gesso.live.dispatch :as dispatch]
   [gesso.live.flow :as flow]
   [gesso.live.fragment :as fragment]
   [gesso.live.htmx :as htmx]
   [gesso.live.invalidation :as invalidation]
   [gesso.live.model :as model]
   [gesso.live.optimistic.capability :as optimistic.capability]
   [gesso.live.optimistic.server :as optimistic.server]
   [gesso.live.progression :as progression]
   [gesso.live.progression.http :as progression.http]
   [gesso.live.source :as source]
   [gesso.live.synced :as synced]
   [gesso.live.transport.sse :as sse]
   [gesso.live.ui :as live.ui]))

;; -----------------------------------------------------------------------------
;; Debug
;; -----------------------------------------------------------------------------

(defmacro ^:private debug!
  "Emit a debug event only when debug-fn is truthy.

   The data expression is inside the guard, so with debugging off it is not
   evaluated."
  [debug-fn event data]
  `(when-let [f# ~debug-fn]
     (f# (assoc ~data :event ~event))))

;; -----------------------------------------------------------------------------
;; Defaults
;; -----------------------------------------------------------------------------

(def default-options
  {:source nil
   :source-options nil

   :dispatcher nil
   :dispatch-options nil

   :rules []

   :fragment-manager nil
   :fragment-options nil

   :debug-fn nil})

(def ^:private valid-emit-modes
  #{:async :sync false})

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn- now-ms
  []
  (System/currentTimeMillis))

(defn- ex
  ([message data]
   (ex-info message data))
  ([message data cause]
   (ex-info message data cause)))

(defn- opts
  [options]
  (merge default-options options))

(defn- require-fn-option!
  [options k]
  (when-let [f (get options k)]
    (when-not (fn? f)
      (throw
       (ex (str "gesso.live core " k " must be a function.")
           {k f}))))
  options)

(defn- require-map-option!
  [options k]
  (when-let [m (get options k)]
    (when-not (map? m)
      (throw
       (ex (str "gesso.live core " k " must be a map.")
           {k m}))))
  options)

(defn- require-rules-option!
  [options]
  (when-not (sequential? (:rules options))
    (throw
     (ex "gesso.live core :rules must be sequential."
         {:rules (:rules options)})))
  options)

(defn prepare-options!
  "Merge defaults and validate core options."
  [options]
  (let [options' (opts options)]
    (require-fn-option! options' :debug-fn)
    (require-map-option! options' :source-options)
    (require-map-option! options' :dispatch-options)
    (require-map-option! options' :fragment-options)
    (require-rules-option! options')
    options'))

(defn- call-safely
  [f & args]
  (when f
    (try
      (apply f args)
      (catch Exception _
        nil))))

(defn- with-inherited-debug
  "Attach the system debug function unless options explicitly contains
   :debug-fn.

   Passing {:debug-fn nil} explicitly disables inherited debugging for that
   child operation."
  [system options]
  (let [options' (or options {})]
    (if (contains? options' :debug-fn)
      options'
      (assoc options' :debug-fn (get-in system [:options :debug-fn])))))

(defn- close-created!
  [created]
  (doseq [close! (reverse @created)]
    (call-safely close!)))

(defn- create-owned-source!
  [options' created]
  (let [src (source/create (:source-options options'))]
    (swap! created conj #(source/close! src))
    src))

(defn- create-owned-dispatcher!
  [options' created]
  (let [dispatcher (dispatch/create (:dispatch-options options'))]
    (swap! created conj #(dispatch/close! dispatcher))
    dispatcher))

(defn- normalize-emit-mode
  [emit]
  (let [emit' (if (nil? emit) :async emit)]
    (when-not (contains? valid-emit-modes emit')
      (throw
       (ex "Unsupported gesso.live transact-and-notify! emit mode."
           {:emit emit
            :normalized emit'
            :valid valid-emit-modes})))
    emit'))

(defn- normalize-changes
  [{:keys [change changes]}]
  (cond
    (some? changes)
    (vec changes)

    (some? change)
    [change]

    :else
    []))

(defn- dispatch-entry-for
  [entry entry-fn change]
  (cond
    entry-fn
    (entry-fn change)

    entry
    entry

    :else
    nil))

;; -----------------------------------------------------------------------------
;; System lifecycle
;; -----------------------------------------------------------------------------

(defn create
  "Create a gesso.live system map.

   Options:
     :source
       Optional existing source. If omitted, core creates one.

     :source-options
       Options passed to source/create when :source is omitted.

     :dispatcher
       Optional existing dispatcher. If omitted, core creates one.

     :dispatch-options
       Options passed to dispatch/create when :dispatcher is omitted.

     :rules
       Invalidation expansion rules, passed through invalidation/compile-rules.

     :fragment-manager
       Optional existing fragment manager. If omitted, core creates one.

     :fragment-options
       Options passed to fragment/create when :fragment-manager is omitted.

     :debug-fn
       Optional pay-for-play debug hook.

   Core only closes source/dispatcher resources it created itself.

   Construction is exception-safe: if a later construction step fails, any
   already-created owned source/dispatcher is closed before the exception is
   rethrown."
  ([] (create nil))
  ([options]
   (let [options' (prepare-options! options)
         debug-fn (:debug-fn options')
         created (atom [])]
     (try
       ;; Compile pure configuration before creating resources where possible.
       (let [rules (invalidation/compile-rules (:rules options'))

             source-supplied? (some? (:source options'))
             dispatcher-supplied? (some? (:dispatcher options'))
             fragment-supplied? (some? (:fragment-manager options'))

             src (or (:source options')
                     (create-owned-source! options' created))

             dispatcher (or (:dispatcher options')
                            (create-owned-dispatcher! options' created))

             ;; Fragment manager currently does not own closeable resources, but
             ;; core still records whether it created it for stats/debug clarity.
             fragment-manager (or (:fragment-manager options')
                                  (fragment/create
                                   (with-inherited-debug
                                     {:options options'}
                                     (:fragment-options options'))))

             system {:source src
                     :dispatcher dispatcher
                     :rules rules
                     :fragment-manager fragment-manager
                     :options options'
                     :owned {:source (not source-supplied?)
                             :dispatcher (not dispatcher-supplied?)
                             :fragment-manager (not fragment-supplied?)}
                     :closed? (atom false)}]

         ;; Ownership has moved to the returned system. Do not auto-cleanup now.
         (reset! created [])

         (debug!
          debug-fn
          :gesso.live.core/created
          {:owned (:owned system)
           :rule-count (count rules)
           :at (now-ms)})

         system)

       (catch Throwable e
         (close-created! created)
         (throw e))))))

(defn closed?
  "Return true when the core system has been closed."
  [system]
  (true? @(:closed? system)))

(defn close!
  "Close resources owned by the core system.

   Existing source/dispatcher/fragment-manager values supplied by the caller are
   not closed by core because core does not own them.

   Fragment managers currently have no close operation; their cache can be
   cleared explicitly with clear-fragment-cache!."
  [system]
  (let [debug-fn (get-in system [:options :debug-fn])]
    (when (compare-and-set! (:closed? system) false true)
      (debug!
       debug-fn
       :gesso.live.core/closing
       {:owned (:owned system)
        :at (now-ms)})

      ;; Stop async jobs before closing the source they may publish to.
      (when (get-in system [:owned :dispatcher])
        (call-safely dispatch/close! (:dispatcher system)))

      (when (get-in system [:owned :source])
        (call-safely source/close! (:source system)))

      (debug!
       debug-fn
       :gesso.live.core/closed
       {:owned (:owned system)
        :at (now-ms)})))
  system)

(defn stats
  "Return a combined stats map for the live system."
  [system]
  {:closed? (closed? system)
   :source (source/stats (:source system))
   :fragment (fragment/stats (:fragment-manager system))})

;; -----------------------------------------------------------------------------
;; Plain-data model facade
;; -----------------------------------------------------------------------------

(def compile-live-app
  "Validate and compile app-owned live metadata.

   Re-export of gesso.live.model/compile-live-app."
  model/compile-live-app)

(def validate-live-app
  "Validate app-owned live metadata.

   Re-export of gesso.live.model/validate-live-app."
  model/validate-live-app)

(def normalize-live-app
  "Normalize app-owned live metadata.

   Re-export of gesso.live.model/normalize-live-app."
  model/normalize-live-app)

(def model-live-rules
  "Return compiled live rules from a compiled live app model.

   Re-export of gesso.live.model/live-rules.

   Named model-live-rules in the core facade to avoid ambiguity with the
   system-level :rules option."
  model/live-rules)

(def expand-change
  "Expand one primary app change into concrete runtime scopes.

   Re-export of gesso.live.model/expand-change."
  model/expand-change)

(def expand-changes
  "Expand many primary app changes into concrete runtime scopes.

   Re-export of gesso.live.model/expand-changes."
  model/expand-changes)

(def live-scope
  "Construct a runtime scope map from a compiled live app, scope name, and id.

   Named live-scope in the core facade to avoid confusion with lexical scope and
   to keep the model namespace's lower-level name available as
   gesso.live.model/scope."
  model/scope)

(def live-scope-key
  "Return the stable [:topic :id] key for a runtime live scope.

   Re-export of gesso.live.model/scope-key."
  model/scope-key)

(def scope-descriptor
  "Return a scope descriptor from a compiled live app.

   Re-export of gesso.live.model/scope-descriptor."
  model/scope-descriptor)

(def authorized-for-scope?
  "Return true if ctx is authorized for the named model scope/id.

   Re-export of gesso.live.model/authorized-for-scope?."
  model/authorized-for-scope?)

(def require-scope-authorized!
  "Throw if ctx is not authorized for the named model scope/id.

   Re-export of gesso.live.model/require-scope-authorized!."
  model/require-scope-authorized!)

(def fragment-descriptor
  "Return a model fragment descriptor from a compiled live app.

   Re-export of gesso.live.model/fragment-descriptor."
  model/fragment-descriptor)

(def fragment-scope-name
  "Return the model scope name used by a model fragment.

   Re-export of gesso.live.model/fragment-scope-name."
  model/fragment-scope-name)

(def fragment-dom-id
  "Return the DOM id for a compiled model fragment/id pair.

   Re-export of gesso.live.model/fragment-dom-id."
  model/fragment-dom-id)

(def fragment-scope-instance
  "Return the runtime scope instance for a compiled model fragment/id pair.

   Re-export of gesso.live.model/fragment-scope-instance."
  model/fragment-scope-instance)

(defn bind-request-progression
  "Decode and bind the optional browser refresh progression requirement from a
   Ring/Biff request context.

   This is the app-facing server request boundary for the progression carrier.
   The browser-supplied requirement is only a minimum-read requirement; it does
   not establish authority. Any pre-existing trusted canonical progression on
   ctx is conservatively composed with the request requirement.

   Standard query-fragment, render-fragment-node, and render-fragment-response
   calls bind automatically. Custom fragment handlers should call this once at
   their request boundary before using progression-aware read helpers such as q."
  [ctx]
  (progression.http/bind-request-progression ctx))

(defn query-fragment
  "Run a model fragment query with request progression bound into ctx.

   If ctx carries the canonical Gesso Live progression HTTP header, decode it at
   this server boundary and install the normalized requirement under
   :gesso.live/progression before the model query runs. The model layer remains
   HTTP-agnostic."
  [compiled ctx fragment-name id]
  (model/query-fragment
   compiled
   (bind-request-progression ctx)
   fragment-name
   id))

(defn render-fragment-node
  "Render a model fragment to a Hiccup/HTML node with request progression bound.

   The request transport is decoded before the fragment query executes so
   progression-aware reads cannot accidentally ignore the invalidating basis."
  [compiled ctx fragment-name id]
  (model/render-fragment-node
   compiled
   (bind-request-progression ctx)
   fragment-name
   id))

(defn render-fragment-response
  "Render a model fragment and wrap it in a Ring response.

   Browser refresh progression is decoded and installed into canonical request
   context before authorization and query/render. This keeps HTTP concerns out
   of gesso.live.model while making the standard app-facing fragment response
   path progression-safe."
  ([compiled ctx fragment-name id]
   (model/render-fragment-response
    compiled
    (bind-request-progression ctx)
    fragment-name
    id))
  ([compiled ctx fragment-name id opts]
   (model/render-fragment-response
    compiled
    (bind-request-progression ctx)
    fragment-name
    id
    opts)))

(def explain-live-app
  "Return an inspectable summary of a compiled live app model.

   Re-export of gesso.live.model/explain-live-app."
  model/explain-live-app)

;; -----------------------------------------------------------------------------
;; XTDB2 consistency/progression facade
;; -----------------------------------------------------------------------------

(defn consistency
  "Return explicit gesso.live/XTDB2 consistency from ctx.

   This delegates to gesso.live.consistency.xtdb/consistency-from and does not
   inspect or mutate shared XTDB node/DataSource state."
  [ctx]
  (live.xtdb/consistency-from ctx))

(defn progression
  "Return optional normalized authoritative progression from ctx.

   :gesso.live/progression is canonical. The XTDB adapter also accepts the
   narrow request-local :progression convenience key while callers migrate.
   This facade does not compare opaque bases."
  [ctx]
  (live.xtdb/progression-from ctx))

(defn with-consistency
  "Assoc explicit consistency onto ctx.

   The consistency value is normalized before being stored under
   :gesso.live/consistency. Passing nil or an empty consistency map stores an
   empty normalized map."
  [ctx consistency]
  (assoc ctx
         :gesso.live/consistency
         (live.xtdb/normalize-consistency consistency)))

(defn with-progression
  "Assoc one normalized authoritative progression requirement onto ctx.

   The canonical request/read-context key is :gesso.live/progression. nil means
   no requirement and removes that canonical key. This helper replaces the
   canonical value; callers that need conservative accumulation should compose
   requirements explicitly with gesso.live.progression/compose."
  [ctx progression-value]
  (if-some [progression'
            (progression/normalize-requirement progression-value)]
    (assoc ctx :gesso.live/progression progression')
    (dissoc ctx :gesso.live/progression)))

(defn attach-consistency
  "Attach explicit consistency to a change/invalidation map.

   This is plain immutable data. It is not used to mutate XTDB state.

   The attached value is intentionally namespaced so app rules can opt into
   reading it without colliding with domain keys."
  [change consistency]
  (let [consistency' (live.xtdb/normalize-consistency consistency)]
    (if (seq consistency')
      (assoc change :gesso.live/consistency consistency')
      change)))

(defn attach-progression
  "Attach one normalized authoritative progression requirement to a primary
   change/invalidation map.

   Live schemas own the unqualified :progression field. A pre-existing equal
   requirement is accepted; a different one fails closed instead of silently
   replacing authority metadata. nil leaves the map unchanged."
  [change progression-value]
  (if-some [progression'
            (progression/normalize-requirement progression-value)]
    (if (contains? change :progression)
      (let [existing
            (progression/normalize-requirement (:progression change))]
        (when-not (= existing progression')
          (throw
           (ex "Primary change progression conflicts with authoritative transaction progression."
               {:existing-progression existing
                :authoritative-progression progression'
                :change change})))
        change)
      (assoc change :progression progression'))
    change))

(defn- bind-transaction-progression!
  "Bind progression established by this XTDB transaction to one primary change.

   A caller-supplied :progression cannot stand in for missing transaction
   evidence. When transaction progression exists, attach-progression enforces
   exact agreement with any repeated value already on the change."
  [change transaction-progression]
  (if-some [transaction-progression'
            (progression/normalize-requirement transaction-progression)]
    (attach-progression change transaction-progression')
    (do
      (when (contains? change :progression)
        (throw
         (ex "Primary change may not supply progression when the transaction established none."
             {:progression (:progression change)
              :change change})))
      change)))

(defn q
  "App-facing XTDB2 read helper.

   Uses the ctx-aware consistency/progression-aware read path. Prefer this for
   live fragment reads so a ctx carrying :gesso.live/progression cannot be read
   from an XTDB snapshot older than the authoritative refresh requirement.
   Explicit consistency remains supported through the XTDB adapter."
  ([ctx query]
   (live.xtdb/q-consistent-from ctx query))
  ([ctx query opts]
   (live.xtdb/q-consistent-from ctx query opts)))

(defn- execute-tx!
  "Execute XTDB2 tx ops for Live's authoritative mutation workflows.

   This is deliberately private. Application code that needs raw XTDB
   transaction execution should use gesso.live.consistency.xtdb explicitly;
   authoritative mutations that participate in Live should normally enter
   through transact-and-notify!, live-set!, or live-swap!.

   Returns the result from gesso.live.consistency.xtdb/execute-tx-from!:

     {:tx-result ...
      :consistency ...
      :progression ...} ; when complete authoritative basis data is available

   Live keeps this helper internally because execute-tx returns tx-id and
   system-time, from which the XTDB adapter derives both per-query consistency
   and a portable authoritative progression requirement."
  ([ctx tx-ops]
   (live.xtdb/execute-tx-from! ctx tx-ops))
  ([ctx tx-ops opts]
   (live.xtdb/execute-tx-from! ctx tx-ops opts)))

;; -----------------------------------------------------------------------------
;; HTMX/raw attr facade
;; -----------------------------------------------------------------------------

(def fragment-root-attrs
  "Build attrs for the outer live wrapper.

   Re-export of gesso.live.htmx/fragment-root-attrs."
  htmx/fragment-root-attrs)

(def fragment-target-attrs
  "Build attrs for a live fragment refresh target.

   Re-export of gesso.live.htmx/fragment-target-attrs."
  htmx/fragment-target-attrs)

(def post-form-attrs
  "Build standard attrs for a POST form that refreshes a target fragment.

   Re-export of gesso.live.htmx/post-form-attrs."
  htmx/post-form-attrs)

;; -----------------------------------------------------------------------------
;; UI/Hiccup facade
;; -----------------------------------------------------------------------------

(def ->fragment
  "Create a live fragment descriptor.

   Re-export of gesso.live.ui/->fragment."
  live.ui/->fragment)

(def fragment-panel
  "Render a standard live fragment panel.

   Re-export of gesso.live.ui/fragment-panel."
  live.ui/fragment-panel)

(def post-form
  "Render a POST form with anti-forgery input.

   Re-export of gesso.live.ui/post-form."
  live.ui/post-form)

(def post-button
  "Render a tiny type=button HTMX POST control, optionally with :optimistic.

   Re-export of gesso.live.ui/post-button."
  live.ui/post-button)

(def anti-forgery-token
  "Extract an anti-forgery token from ctx.

   Re-export of gesso.live.ui/anti-forgery-token."
  live.ui/anti-forgery-token)

(def anti-forgery-input
  "Render a hidden anti-forgery input when ctx contains a token.

   Re-export of gesso.live.ui/anti-forgery-input."
  live.ui/anti-forgery-input)

;; -----------------------------------------------------------------------------
;; Optimistic protocol-v3 capability facade
;; -----------------------------------------------------------------------------

(def optimistic-capability
  "Construct one portable optimistic operation capability.

   A capability binds application-owned operation identity and browser execution
   policy for later view rendering.  It is inert configuration, not authority or
   durable authorization.

   Re-export of gesso.live.optimistic.capability/operation-capability."
  optimistic.capability/operation-capability)

(def optimistic-capability?
  "Return true for a canonical optimistic operation capability.

   This checks only local capability shape.  Trusted server registration,
   authentication, and authorization remain separate concerns.

   Re-export of gesso.live.optimistic.capability/operation-capability?."
  optimistic.capability/operation-capability?)

(def bind-optimistic-capability
  "Bind one optimistic operation capability to per-render arguments, basis,
   scope/fact versions, and target identity.

   The result is the inert protocol-v3 action-data shape consumed by post-button
   and gesso.live.ui/optimistic-action.  Binding does not create command or
   execution identity and cannot confer authority.

   Re-export of gesso.live.optimistic.capability/bind."
  optimistic.capability/bind)

;; -----------------------------------------------------------------------------
;; Optimistic protocol-v3 trusted-server facade
;; -----------------------------------------------------------------------------

(def optimistic-operation
  "Construct one trusted optimistic-operation registry entry.

   Re-export of gesso.live.optimistic.server/operation."
  optimistic.server/operation)

(def optimistic-operation?
  "Return true for a trusted optimistic-operation registry entry.

   Re-export of gesso.live.optimistic.server/operation?."
  optimistic.server/operation?)

(def optimistic-server
  "Construct one trusted optimistic protocol-v3 server boundary.

   Re-export of gesso.live.optimistic.server/server."
  optimistic.server/server)

(def optimistic-server?
  "Return true for a trusted optimistic protocol-v3 server boundary.

   Re-export of gesso.live.optimistic.server/server?."
  optimistic.server/server?)

(def decode-optimistic-command
  "Decode and validate one protocol-v3 optimistic command wire value.

   Re-export of gesso.live.optimistic.server/decode-command."
  optimistic.server/decode-command)

(def normalize-optimistic-command
  "Normalize and validate one protocol-v3 optimistic command value.

   Re-export of gesso.live.optimistic.server/normalize-command."
  optimistic.server/normalize-command)

(def begin-optimistic-command
  "Bind a validated command to trusted principal and operation state.

   Re-export of gesso.live.optimistic.server/begin-command."
  optimistic.server/begin-command)

(def optimistic-command-boundary?
  "Return true for a trusted protocol-v3 command boundary.

   Re-export of gesso.live.optimistic.server/command-boundary?."
  optimistic.server/command-boundary?)

(def optimistic-operation-context
  "Build the trusted context passed to a registered public model operation.

   Re-export of gesso.live.optimistic.server/operation-context."
  optimistic.server/operation-context)

(def optimistic-settlement-from-result
  "Construct a protocol-v3 settlement from a trusted model-operation result.

   Command and execution identities are copied from the trusted command
   boundary; operation results cannot choose them.

   Re-export of gesso.live.optimistic.server/settlement-from-result."
  optimistic.server/settlement-from-result)

(def prepare-optimistic-settlement-send
  "Advance the trusted authority projection to its settlement-send boundary.

   Re-export of gesso.live.optimistic.server/prepare-settlement-send."
  optimistic.server/prepare-settlement-send)

(def optimistic-prepared-send?
  "Return true for a prepared optimistic settlement send.

   Re-export of gesso.live.optimistic.server/prepared-send?."
  optimistic.server/prepared-send?)

(def complete-optimistic-send
  "Complete a prepared optimistic settlement-send boundary after transport
   handoff.

   This completes only the projected Choreo send boundary. It does not classify
   arbitrary HTTP, invalidation, or post-commit delivery failures as mutation
   failure.

   Re-export of gesso.live.optimistic.server/complete-settlement-send."
  optimistic.server/complete-settlement-send)

(def optimistic-completed-send?
  "Return true for a completed optimistic settlement send.

   Re-export of gesso.live.optimistic.server/completed-send?."
  optimistic.server/completed-send?)

(def run-optimistic-command
  "Run one validated protocol-v3 optimistic command through the trusted server
   boundary and registered public model operation.

   Re-export of gesso.live.optimistic.server/run-command."
  optimistic.server/run-command)

(def run-optimistic-wire-command
  "Decode one command wire value and run it through the trusted server boundary.

   Re-export of gesso.live.optimistic.server/run-wire-command."
  optimistic.server/run-wire-command)

;; -----------------------------------------------------------------------------
;; Model-backed fragment UI facade
;; -----------------------------------------------------------------------------

(def fragment->runtime-fragment
  "Build the current Gesso Live UI fragment descriptor from a compiled model
   fragment.

   Re-export of gesso.live.fragment/fragment->runtime-fragment."
  fragment/fragment->runtime-fragment)

(def model-fragment-panel
  "Render a client-side live fragment panel from a compiled model fragment.

   This intentionally does not replace the existing UI-level fragment-panel API.

   Re-export of gesso.live.fragment/model-fragment-panel."
  fragment/model-fragment-panel)

;; -----------------------------------------------------------------------------
;; Invalidation and source emission
;; -----------------------------------------------------------------------------

(defn expand
  "Expand one primary app change into invalidations using the system rules."
  [system ctx change]
  (let [debug-fn (get-in system [:options :debug-fn])
        expanded (invalidation/expand (:rules system) ctx change)]
    (debug!
     debug-fn
     :gesso.live.core/expanded
     {:change change
      :count (count expanded)
      :at (now-ms)})
    expanded))

(defn emit!
  "Emit one already-expanded invalidation directly to the system source."
  [system invalidation]
  (let [debug-fn (get-in system [:options :debug-fn])]
    (debug!
     debug-fn
     :gesso.live.core/emit
     {:invalidation invalidation
      :at (now-ms)})
    (source/emit! (:source system) invalidation)))

(defn emit-many!
  "Emit many already-expanded invalidations directly to the system source."
  [system invalidations]
  (let [debug-fn (get-in system [:options :debug-fn])]
    (debug!
     debug-fn
     :gesso.live.core/emit-many
     {:count (count invalidations)
      :at (now-ms)})
    (source/emit-many! (:source system) invalidations)))

(defn emit-expanded!
  "Expand a primary app change and emit the resulting invalidations.

   Returns the source emit-many! result."
  [system ctx change]
  (let [expanded (expand system ctx change)
        result (emit-many! system expanded)
        debug-fn (get-in system [:options :debug-fn])]
    (debug!
     debug-fn
     :gesso.live.core/emit-expanded
     {:change change
      :count (count expanded)
      :result result
      :at (now-ms)})
    result))

(defn submit-expanded!
  "Submit an async job that expands and emits one primary app change.

   This is the usual write-path helper: after an app write commits, submit an
   app-level primary change here.

   `entry` may contain dispatch metadata, but :run is owned by this helper."
  ([system ctx change]
   (submit-expanded! system ctx change nil))
  ([system ctx change entry]
   (let [debug-fn (get-in system [:options :debug-fn])
         entry' (assoc (or entry {})
                       :run
                       (fn []
                         (emit-expanded! system ctx change)))]
     (debug!
      debug-fn
      :gesso.live.core/submit-expanded
      {:change change
       :entry-keys (set (keys (or entry {})))
       :at (now-ms)})
     (dispatch/submit! (:dispatcher system) entry'))))

(def post-commit-delivery-failure-type
  "Error type used when an XTDB transaction committed successfully but the
   subsequent Live invalidation delivery/submission step failed.

   The exception always carries :commit/status :committed and
   :failure/stage :post-commit-delivery so callers cannot mistake it for a
   failed authoritative mutation."
  ::post-commit-delivery-failure)

(defn post-commit-delivery-failure?
  "Return true when value is a classified Gesso Live post-commit delivery
   failure.

   These failures mean the authoritative XTDB transaction committed. They do
   not mean the mutation rolled back. Callers may inspect ex-data for the
   committed transaction metadata, completed delivery results, and the exact
   delivery that failed."
  [value]
  (and (instance? Throwable value)
       (= post-commit-delivery-failure-type
          (:error/type (ex-data value)))
       (= :committed
          (:commit/status (ex-data value)))
       (= :post-commit-delivery
          (:failure/stage (ex-data value)))))

(defn- post-commit-delivery-error
  [cause
   tx-result-map
   ctx
   changes
   emit-mode
   delivery-index
   change
   completed-results]
  (ex
   "XTDB transaction committed, but Gesso Live post-commit invalidation delivery failed."
   (cond->
    {:error/type post-commit-delivery-failure-type
     :failure/stage :post-commit-delivery
     :commit/status :committed
     :tx-result (:tx-result tx-result-map)
     :consistency (:consistency tx-result-map)
     :ctx ctx
     :changes changes
     :emit emit-mode
     :delivery/index delivery-index
     :delivery/change change
     :delivery/completed-results completed-results}

    (:progression tx-result-map)
    (assoc :progression (:progression tx-result-map)))
   cause))

(defn- deliver-post-commit!
  "Run delivery-fn once for each attached change after a successful commit.

   A failure is rethrown as an explicitly classified post-commit delivery
   failure. Results from earlier successful deliveries are retained in ex-data
   because synchronous emission and async queue submission can both be
   partially complete when a later change fails."
  [tx-result-map ctx changes emit-mode delivery-fn]
  (loop [index 0
         remaining changes
         completed []]
    (if-let [change (first remaining)]
      (let [result
            (try
              (delivery-fn change)
              (catch Throwable cause
                (throw
                 (post-commit-delivery-error
                  cause
                  tx-result-map
                  ctx
                  changes
                  emit-mode
                  index
                  change
                  completed))))]
        (recur (inc index)
               (next remaining)
               (conj completed result)))
      completed)))

(defn transact-and-notify!
  "Common XTDB2 write + live invalidation workflow.

   Steps:
     1. execute XTDB2 tx ops with execute-tx!
     2. capture returned consistency and authoritative progression
     3. assoc consistency onto ctx and conservatively compose ctx progression
     4. attach consistency plus transaction-established progression to changes
     5. optionally emit or submit expanded invalidations
     6. classify any failure in step 5 as post-commit delivery failure
     7. return tx/ctx/change/emission metadata

   The transaction result is the authority for progression attached to emitted
   primary changes. Caller-supplied change progression may only repeat that exact
   requirement; it cannot replace it or fabricate progression when execute-tx!
   established none. Existing request-context progression is retained in the
   returned ctx by conservative composition, but it is not substituted for the
   transaction's commit requirement on invalidations.

   Arguments:
     system
       gesso.live system map.

     ctx
       app ctx or XTDB connectable-containing ctx.

     options map:
       :tx-ops
         Required XTDB2 tx ops.

       :change
         One primary app change.

       :changes
         Multiple primary app changes.

       :tx-options
         Optional XTDB tx options passed to execute-tx!.

       :emit
         :async | :sync | false. Defaults to :async.

         :async submits each change through submit-expanded!.
         :sync expands and emits each change on the caller thread.
         false performs the transaction and returns metadata without emitting.

       :entry
         Optional dispatch entry metadata used for every async submission.

       :entry-fn
         Optional function of attached change -> dispatch entry metadata.

   Returns on successful commit and delivery:
     {:tx-result ...
      :consistency ...
      :progression ... ; when execute-tx! established it
      :ctx ...
      :changes ...
      :emit ...
      :emit-results ...}

   If XTDB execution fails, the original pre-commit exception escapes. If a
   synchronous invalidation emission or asynchronous dispatch submission fails
   *after* execute-tx! returned, throws a classified exception satisfying
   post-commit-delivery-failure?. Its ex-data contains :commit/status
   :committed plus committed transaction metadata and any earlier completed
   delivery results. Such an exception must never be interpreted as mutation
   rollback."
  [system ctx {:keys [tx-ops tx-options emit entry entry-fn] :as options}]
  (when-not tx-ops
    (throw
     (ex "gesso.live transact-and-notify! requires :tx-ops."
         {:options options})))
  (let [debug-fn (get-in system [:options :debug-fn])
        emit-mode (normalize-emit-mode emit)
        changes (normalize-changes options)
        tx-result-map (execute-tx! ctx tx-ops tx-options)
        consistency' (:consistency tx-result-map)
        transaction-progression (:progression tx-result-map)
        ctx-progression (progression/compose
                         (progression ctx)
                         transaction-progression)
        ctx' (-> ctx
                 (with-consistency consistency')
                 (with-progression ctx-progression))
        changes' (mapv (fn [change]
                         (-> change
                             (attach-consistency consistency')
                             (bind-transaction-progression!
                              transaction-progression)))
                       changes)
        emit-results
        (case emit-mode
          false
          []

          :sync
          (deliver-post-commit!
           tx-result-map
           ctx'
           changes'
           emit-mode
           #(emit-expanded! system ctx' %))

          :async
          (deliver-post-commit!
           tx-result-map
           ctx'
           changes'
           emit-mode
           (fn [change]
             (submit-expanded!
              system
              ctx'
              change
              (dispatch-entry-for entry entry-fn change)))))]
    ;; Debugging is observational. A broken debug hook after commit/delivery must
    ;; not turn a committed mutation into an apparent operation failure.
    (call-safely
     debug-fn
     (assoc
      (cond-> {:tx-result (:tx-result tx-result-map)
               :consistency consistency'
               :change-count (count changes')
               :emit emit-mode
               :emit-result-count (count emit-results)
               :at (now-ms)}
        transaction-progression
        (assoc :progression transaction-progression))
      :event :gesso.live.core/transact-and-notify))
    (merge tx-result-map
           {:ctx ctx'
            :changes changes'
            :emit emit-mode
            :emit-results emit-results})))

;; -----------------------------------------------------------------------------
;; Synced value facade
;; -----------------------------------------------------------------------------

(def ->synced
  "Define one persisted live value.

   Re-export of gesso.live.synced/->synced."
  synced/->synced)

(defn- system-from
  [ctx options]
  (or (:system options)
      (:gesso.live/system options)
      (:gesso.live/system ctx)
      (:live/system ctx)
      (throw
       (ex "gesso.live synced write requires a live system."
           {:ctx-keys (when (map? ctx) (set (keys ctx)))
            :options-keys (when (map? options) (set (keys options)))
            :expected-one-of [:system
                              :gesso.live/system
                              [:ctx :gesso.live/system]
                              [:ctx :live/system]]}))))

(defn live-read
  "Read a synced value through the ctx-aware XTDB consistency path.

   Example:

     (def counter
       (live/->synced
        {:table :demo_counters
         :id \"global-shared-counter\"
         :col :demo/value
         :topic :demo-counter
         :default 0}))

     (live/live-read ctx counter)"
  ([ctx synced-value]
   (synced/live-read ctx synced-value))
  ([ctx synced-value opts]
   (synced/live-read ctx synced-value opts)))

(defn live-set!
  "Set a synced value, execute an XTDB tx, and optionally notify live subscribers.

   Requires the live system under one of:

     (:system options)
     (:gesso.live/system options)
     (:gesso.live/system ctx)
     (:live/system ctx)

   Options:
     :system
       Explicit live system.

     :emit
       :async | :sync | false. Defaults to :async.

     :tx-options
       XTDB tx options.

     :entry
       Dispatch entry metadata. Defaults to the synced descriptor's
       coalesce-key entry.

     :entry-fn
       Optional function of attached change -> dispatch entry metadata.

     :change
       Explicit primary change. Defaults to synced/change.

     :data
       Optional data included in the generated change.

     :change/kind
       Override generated change kind.

   Returns the transact-and-notify! result, assoc'd with :value."
  ([ctx synced-value value]
   (live-set! ctx synced-value value nil))
  ([ctx synced-value value options]
   (let [options' (or options {})
         system (system-from ctx options')
         change' (or (:change options')
                     (synced/change synced-value value options'))
         entry' (if (contains? options' :entry)
                  (:entry options')
                  (synced/entry synced-value))
         result (transact-and-notify!
                 system
                 ctx
                 {:tx-ops (synced/tx-ops synced-value value)
                  :change change'
                  :tx-options (:tx-options options')
                  :emit (:emit options')
                  :entry entry'
                  :entry-fn (:entry-fn options')})]
     (assoc result :value value))))

(defn- xtdb-assert-failed?
  "Return true when error or one of its causes is XTDB's ASSERT conflict.

   Guarded synced swaps use XTDB ASSERT as their compare-and-set boundary. Only
   that conflict is retryable. Post-commit delivery failures and every other
   transaction/read/application failure must escape unchanged."
  [error]
  (loop [cause error
         depth 0]
    (cond
      (nil? cause)
      false

      (= :xtdb/assert-failed
         (:xtdb.error/code (ex-data cause)))
      true

      (>= depth 32)
      false

      :else
      (recur (.getCause ^Throwable cause)
             (inc depth)))))

(defn- live-swap-read-options
  "Build query options for an authoritative guarded-swap read.

   live-swap! is a mutation primitive, so it must compute f from the current
   authoritative value rather than from a request's historical fragment basis.
   Explicit snapshot coordinates are therefore removed. Other ordinary query
   options are retained, and an explicit transaction database owns the read
   database as well."
  [options]
  (let [options' (or options {})
        read-options (dissoc (or (:read-options options') {})
                             :snapshot-time
                             :snapshot-token)
        tx-database (get-in options' [:tx-options :database])]
    (cond-> read-options
      tx-database
      (assoc :database tx-database))))

(defn- live-swap-write-connectable
  "Return the mutation connectable without misclassifying XTDB record values as ctx maps."
  [ctx]
  (if (and (map? ctx)
           (some #(contains? ctx %)
                 [:xtdb/connectable
                  :xtdb/conn
                  :xtdb/node
                  :biff.xtdb/node
                  :biff/conn
                  :biff/node]))
    (live.xtdb/connectable-from ctx)
    ctx))

(defn- live-swap-current-value
  "Read a synced value from the write connectable without request snapshot pinning."
  [ctx synced-value read-options]
  ;; synced/live-read expects a context-like value. Wrap the write connectable so
  ;; XTDB node records, which satisfy map?, are not mistaken for a context map by
  ;; consistency.xtdb/read-connectable-from.
  (synced/live-read
   {:xtdb/read-connectable (live-swap-write-connectable ctx)}
   synced-value
   read-options))

(defn live-swap!
  "Atomically apply f to the current authoritative synced value.

   Each attempt reads the value from the XTDB write connectable, applies f, and
   submits one transaction containing an ASSERT of the observed value followed
   by the replacement PUT. If another transaction wins first, XTDB aborts the
   guarded transaction and live-swap! rereads/reapplies f until one attempt
   commits. This gives the helper genuine compare-and-set swap semantics instead
   of an unguarded read/modify/write that can lose concurrent updates.

   As with clojure.core/swap!, f may be invoked more than once and therefore
   should be free of externally visible side effects. Failed ASSERT attempts do
   not publish live invalidations. Once a transaction commits, any subsequent
   publication failure remains a classified post-commit delivery failure and is
   never retried as a mutation.

   Request-scoped :gesso.live/consistency and :gesso.live/progression describe
   observation requirements and are intentionally not used as the mutation's
   read basis. :read-options may still supply ordinary XTDB query options, but
   :snapshot-time and :snapshot-token are ignored for this operation. When
   :tx-options contains :database, that database is also used for the guarded
   read.

   Other options match live-set!: :system, :emit, :tx-options, :entry,
   :entry-fn, :change, :data, and :change/kind. Generated changes describe the
   old/new values of the attempt that actually committed.

   Example:

     (live/live-swap! ctx counter inc)"
  ([ctx synced-value f]
   (live-swap! ctx synced-value f nil))
  ([ctx synced-value f options]
   (let [options' (or options {})
         system (system-from ctx options')
         read-options (live-swap-read-options options')
         entry' (if (contains? options' :entry)
                  (:entry options')
                  (synced/entry synced-value))]
     (loop []
       (let [old-value (live-swap-current-value
                        ctx
                        synced-value
                        read-options)
             new-value (f old-value)
             change-options (assoc options'
                                   :old-value old-value
                                   :new-value new-value)
             change' (or (:change options')
                         (synced/change
                          synced-value
                          new-value
                          change-options))]
         (let [[status result]
              (try
                [:committed
                 (transact-and-notify!
                  system
                  ctx
                  {:tx-ops (synced/guarded-tx-ops
                            synced-value
                            old-value
                            new-value)
                   :change change'
                   :tx-options (:tx-options options')
                   :emit (:emit options')
                   :entry entry'
                   :entry-fn (:entry-fn options')})]
                (catch Throwable error
                  (if (xtdb-assert-failed? error)
                    [:retry nil]
                    (throw error))))]
          (if (= :retry status)
            (recur)
            (assoc result :value new-value))))))))

;; -----------------------------------------------------------------------------
;; Flow and SSE
;; -----------------------------------------------------------------------------

(defn live-flow
  "Build a Missionary flow of live events for one subscription.

   Options are passed to flow/flow-for-source. :debug-fn defaults to the system
   debug hook unless explicitly supplied."
  ([system subscription]
   (live-flow system subscription nil))
  ([system subscription options]
   (let [options' (merge
                   (with-inherited-debug system options)
                   {:subscription subscription})
         debug-fn (:debug-fn options')]
     (debug!
      debug-fn
      :gesso.live.core/live-flow
      {:subscription subscription
       :at (now-ms)})
     (flow/flow-for-source (:source system) options'))))

(defn start-sse!
  "Start an SSE stream for one subscription.

   Options:
     :flow-options
       Passed to flow/flow-for-source.

     :sse-options
       Passed to transport.sse/start!.

   Returns the map from sse/start!:

     {:stream ...
      :response ...
      :cancel! ...
      :closed? ...}

   Keep this returned map and call cancel-sse! or sse/cancel! when the client
   disconnects."
  ([system subscription]
   (start-sse! system subscription nil))
  ([system subscription {:keys [flow-options sse-options] :as options}]
   (let [debug-fn (get-in system [:options :debug-fn])
         flow-options' (with-inherited-debug system flow-options)
         sse-options' (with-inherited-debug system sse-options)
         live-events (live-flow system subscription flow-options')
         started (sse/start! live-events sse-options')]
     (debug!
      debug-fn
      :gesso.live.core/sse-started
      {:subscription subscription
       :options-keys (set (keys (or options {})))
       :at (now-ms)})
     started)))

(def start-fragment-stream!
  "Start an SSE stream for a compiled model fragment.

   Re-export of gesso.live.transport.sse/start-fragment-stream!."
  sse/start-fragment-stream!)

(defn cancel-sse!
  "Cancel a started SSE stream."
  [started]
  (sse/cancel! started))

;; -----------------------------------------------------------------------------
;; Fragment render protection
;; -----------------------------------------------------------------------------

(defn fragment-manager
  "Return the system fragment manager."
  [system]
  (:fragment-manager system))

(defn fragment-key
  "Build a generic fragment key.

   Re-export of gesso.live.fragment/fragment-key."
  ([base]
   (fragment/fragment-key base))
  ([base dimensions]
   (fragment/fragment-key base dimensions)))

(defn strict-fragment-key
  "Build a stricter fragment key requiring :fragment, :scope, and :user-key.

   Re-export of gesso.live.fragment/strict-fragment-key."
  [m]
  (fragment/strict-fragment-key m))

(defn render-task
  "Return a protected fragment render task using the system fragment manager.

   Options are passed to fragment/render-task. :debug-fn defaults to the system
   debug hook unless explicitly supplied."
  ([system key render-fn]
   (render-task system key render-fn nil))
  ([system key render-fn options]
   (fragment/render-task
    (:fragment-manager system)
    key
    render-fn
    (with-inherited-debug system options))))

(defn protect-task
  "Return a protected Missionary task using the system fragment manager.

   Lower-level companion to render-task."
  ([system key task]
   (protect-task system key task nil))
  ([system key task options]
   (fragment/protect-task
    (:fragment-manager system)
    key
    task
    (with-inherited-debug system options))))

(defn clear-fragment-cache!
  "Clear all cached fragment values."
  [system]
  (fragment/clear-cache! (:fragment-manager system))
  system)

(defn clear-fragment-key!
  "Clear cached and in-flight state for one fragment key."
  [system key]
  (fragment/clear-key! (:fragment-manager system) key)
  system)

(defn purge-expired-fragments!
  "Purge expired fragment cache entries and return the number removed."
  [system]
  (fragment/purge-expired! (:fragment-manager system)))
