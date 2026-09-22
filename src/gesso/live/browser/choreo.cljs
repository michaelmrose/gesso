(ns gesso.live.browser.choreo
  "Physical browser binding for portable Gesso Choreo executions.

   Portable protocol state lives in gesso.choreo.machine. Browser semantic
   ownership, generations, stale-callback rejection, timers, and execution
   retirement live in gesso.live.browser.adapter. Imperative resource handling
   lives in gesso.live.browser.shell.

   This namespace deliberately owns none of those state machines. It binds the
   shell's three Choreo-facing physical effects to browser/application handlers:

     :machine/local
       dispatch a named role-local action (commonly a browser.fx machine)

     :machine/send
       construct the payload for the current portable send boundary

     :transport/send
       hand the adapter-constructed participant message to physical transport

   It also provides small helpers which submit normalized execution/message/
   environment/timer events to the shell. Every callback that can advance a
   portable machine carries an adapter-issued execution generation. A callback
   never looks up the current generation and then resumes it; stale generations
   are submitted unchanged so the adapter remains the sole arbiter.

   There are no namespace-global execution registries, timers, target locks,
   terminal histories, send state machines, or alternate resume paths here.
   Registration atoms contain only physical implementation configuration and are
   scoped to one runtime instance."
  (:require
   [gesso.choreo.machine :as machine]
   [gesso.live.browser.adapter :as adapter]
   [gesso.live.browser.shell :as shell]))

;; =============================================================================
;; Public identity / context vocabulary
;; =============================================================================

(def runtime-version 2)

(def runtime-type
  :gesso.live.browser.choreo/runtime)

(def execution-id-key
  :gesso.live.browser.choreo/execution-id)

(def generation-key
  :gesso.live.browser.choreo/generation)

(def effect-generation-key
  :gesso.live.browser.choreo/effect-generation)

(def action-key
  :gesso.live.browser.choreo/action)

(def message-key
  :gesso.live.browser.choreo/message)

(def execution-ref-type
  :gesso.live.browser.choreo/execution-ref)

(def option-keys
  #{:local-actions
    :fx-handlers
    :send-payload
    :transport-send
    :recover-incompatible-plan})

(def owned-effect-kinds
  #{:machine/local
    :machine/send
    :transport/send})

;; =============================================================================
;; Errors / validation
;; =============================================================================

(defn- choreo-error
  ([kind message data]
   (choreo-error kind message data nil))
  ([kind message data cause]
   (ex-info
    message
    (merge
     {:error/type :gesso.live.browser.choreo/error
      :error/kind kind}
     data)
    cause)))

(defn- require-map!
  [label value]
  (when-not (map? value)
    (throw
     (choreo-error
      :invalid-map
      (str label " must be a map.")
      {:label label
       :value value})))
  value)

(defn- require-callable!
  [label value]
  (when-not (fn? value)
    (throw
     (choreo-error
      :invalid-callable
      (str label " must be callable.")
      {:label label
       :value value})))
  value)

(defn- require-optional-callable!
  [label value]
  (when (some? value)
    (require-callable! label value))
  value)

(defn- require-keyword!
  [label value]
  (when-not (keyword? value)
    (throw
     (choreo-error
      :invalid-keyword
      (str label " must be a keyword.")
      {:label label
       :value value})))
  value)

(defn- require-non-nil!
  [label value]
  (when (nil? value)
    (throw
     (choreo-error
      :missing-value
      (str label " must be non-nil.")
      {:label label})))
  value)

(defn- require-nonnegative-integer!
  [label value]
  (when-not (and (integer? value)
                 (<= 0 value))
    (throw
     (choreo-error
      :invalid-nonnegative-integer
      (str label " must be a non-negative integer.")
      {:label label
       :value value})))
  value)

(defn- require-registration-map!
  [label value]
  (require-map! label value)
  (doseq [[id handler] value]
    (require-keyword! (str label " key") id)
    (require-callable! (str label " handler") handler))
  value)

(defn- check-option-keys!
  [options]
  (let [unknown (seq (remove option-keys (keys options)))]
    (when unknown
      (throw
       (choreo-error
        :unknown-options
        "Browser Choreo options contain unsupported keys."
        {:unknown-keys (set unknown)
         :allowed-keys option-keys}))))
  options)

;; =============================================================================
;; Runtime / execution references
;; =============================================================================

(defn runtime?
  [value]
  (and
   (map? value)
   (= runtime-type (:gesso.live.browser.choreo/type value))
   (= runtime-version (:gesso.live.browser.choreo/version value))
   (shell/shell? (:shell value))
   (some? (:local-actions value))
   (some? (:fx-handlers value))
   (some? (:send-payload value))
   (some? (:transport-send value))
   (some? (:recover-incompatible-plan value))
   (some? (:installed-handlers value))))

(defn require-runtime!
  [runtime]
  (when-not (runtime? runtime)
    (throw
     (choreo-error
      :invalid-runtime
      "Expected a Gesso Live browser Choreo runtime."
      {:runtime runtime})))
  runtime)

(defn shell-runtime
  [runtime]
  (:shell (require-runtime! runtime)))

(defn state
  "Return the shared pure browser AdapterState."
  [runtime]
  (shell/state (shell-runtime runtime)))

(defn execution-ref?
  [value]
  (and
   (map? value)
   (= execution-ref-type
      (:gesso.live.browser.choreo/type value))
   (some? (:execution-id value))
   (integer? (:generation value))
   (pos? (:generation value))))

(defn require-execution-ref!
  [value]
  (when-not (execution-ref? value)
    (throw
     (choreo-error
      :invalid-execution-ref
      "Expected an adapter-issued browser Choreo execution reference."
      {:execution-ref value})))
  value)

(defn- ->execution-ref
  [execution-id generation]
  {:gesso.live.browser.choreo/type execution-ref-type
   :execution-id execution-id
   :generation generation})

(defn execution-ref
  "Return the current adapter-issued execution reference, or nil.

   This is intended for synchronous setup/correlation. Long-lived callbacks must
   retain the reference they were originally given; they must not call this
   function later to acquire a newer generation."
  [runtime execution-id]
  (let [generation
        (adapter/execution-generation
         (state runtime)
         execution-id)]
    (when generation
      (->execution-ref execution-id generation))))

(defn execution-record
  [runtime execution-id]
  (adapter/execution (state runtime) execution-id))

(defn execution
  "Return the active portable machine execution, or nil."
  [runtime execution-id]
  (:execution (execution-record runtime execution-id)))

(defn active?
  [runtime execution-id]
  (some? (execution-record runtime execution-id)))

(defn active-execution-ids
  [runtime]
  (set (keys (:executions (state runtime)))))

;; =============================================================================
;; Physical registration
;; =============================================================================

(defn local-actions
  [runtime]
  @(:local-actions (require-runtime! runtime)))

(defn fx-handlers
  [runtime]
  @(:fx-handlers (require-runtime! runtime)))

(defn register-local-action!
  "Register one physical realization of a projected Choreo :local action.

   Handler shape is ctx -> nil|map|Promise<nil|map>. browser.fx/machine values
   satisfy this contract and receive the current :biff.fx/handlers map in ctx."
  [runtime action-id handler]
  (let [runtime (require-runtime! runtime)]
    (require-keyword! "Browser Choreo local action id" action-id)
    (require-callable! "Browser Choreo local action" handler)
    (swap! (:local-actions runtime) assoc action-id handler)
    action-id))

(defn unregister-local-action!
  [runtime action-id]
  (let [runtime (require-runtime! runtime)]
    (swap! (:local-actions runtime) dissoc action-id)
    action-id))

(defn register-fx-handler!
  "Register one Biff-style browser FX handler used by registered local machines."
  [runtime handler-id handler]
  (let [runtime (require-runtime! runtime)]
    (require-keyword! "Browser FX handler id" handler-id)
    (require-callable! "Browser FX handler" handler)
    (swap! (:fx-handlers runtime) assoc handler-id handler)
    handler-id))

(defn unregister-fx-handler!
  [runtime handler-id]
  (let [runtime (require-runtime! runtime)]
    (swap! (:fx-handlers runtime) dissoc handler-id)
    handler-id))

(defn set-send-payload-handler!
  "Install the physical payload constructor for projected send boundaries.

   Handler shape is ctx -> map|Promise<map>. The portable machine validates the
   returned semantic payload before the adapter exposes :transport/send."
  [runtime handler]
  (let [runtime (require-runtime! runtime)]
    (require-optional-callable! "Browser Choreo send-payload handler" handler)
    (reset! (:send-payload runtime) handler)
    true))

(defn send-payload-handler
  [runtime]
  @(:send-payload (require-runtime! runtime)))

(defn set-transport-handler!
  "Install the physical participant-message transport.

   Handler shape is ctx -> completion or

     {:completion completion
      :cancel!    optional-zero-arity-fn}

   and is passed through to shell transport ownership unchanged."
  [runtime handler]
  (let [runtime (require-runtime! runtime)]
    (require-optional-callable! "Browser Choreo transport handler" handler)
    (reset! (:transport-send runtime) handler)
    true))

(defn transport-handler
  [runtime]
  @(:transport-send (require-runtime! runtime)))

(defn- default-incompatible-plan-recovery!
  [_context]
  ;; A full-page reload is the conservative browser default for an incompatible
  ;; projected plan. It reconstructs transport/authentication/application state
  ;; from current authority instead of attempting to migrate suspended machine
  ;; state. Node-based semantic tests do not provide a browser location; in that
  ;; host the semantic retirement still stands and the caller can observe that a
  ;; physical reload was unavailable.
  (let [location (.-location js/globalThis)
        reload (when location (.-reload location))]
    (if (fn? reload)
      (do
        (.call reload location)
        :reload-requested)
      :reload-unavailable)))

(defn set-incompatible-plan-recovery-handler!
  "Install the browser/application recovery action for stale ExecutablePlans.

   Handler shape is context -> result. The context is delivered only *after*
   any active execution with the same logical execution-id has been
   semantically retired (or an attempted retirement has at least installed its
   adapter transition). The default handler requests a full page reload when a
   browser location is available.

   This is a physical recovery seam, not portable Choreo semantics. A handler
   must reconstruct current authority; it must not resume or migrate the stale
   machine execution."
  [runtime handler]
  (let [runtime (require-runtime! runtime)]
    (require-optional-callable!
     "Browser Choreo incompatible-plan recovery handler" handler)
    (reset!
     (:recover-incompatible-plan runtime)
     (or handler default-incompatible-plan-recovery!))
    true))

(defn incompatible-plan-recovery-handler
  [runtime]
  @(:recover-incompatible-plan (require-runtime! runtime)))

;; =============================================================================
;; Physical effect realization
;; =============================================================================

(defn- local-action-context
  [runtime effect]
  (let [action (:action effect)]
    (merge
     (:inputs action)
     {execution-id-key (:execution-id effect)
      generation-key (:generation effect)
      effect-generation-key (:effect-generation effect)
      action-key action
      :biff.fx/handlers @(:fx-handlers runtime)})))

(defn- send-context
  [effect]
  {execution-id-key (:execution-id effect)
   generation-key (:generation effect)
   effect-generation-key (:effect-generation effect)
   action-key (:action effect)})

(defn- transport-context
  [effect]
  {execution-id-key (:execution-id effect)
   generation-key (:generation effect)
   effect-generation-key (:effect-generation effect)
   message-key (:message effect)})

(defn- realize-local!
  [runtime effect]
  (let [action (:action effect)
        action-id (:action action)
        handler (get @(:local-actions runtime) action-id)]
    (when-not handler
      (throw
       (choreo-error
        :missing-local-action
        "No physical browser realization is registered for the Choreo local action."
        {:action-id action-id
         :execution-id (:execution-id effect)
         :generation (:generation effect)
         :action action})))
    (handler (local-action-context runtime effect))))

(defn- realize-send-payload!
  [runtime effect]
  (let [handler @(:send-payload runtime)]
    (when-not handler
      (throw
       (choreo-error
        :missing-send-payload-handler
        "No browser Choreo send-payload handler is installed."
        {:execution-id (:execution-id effect)
         :generation (:generation effect)
         :action (:action effect)})))
    (handler (send-context effect))))

(defn- realize-transport-send!
  [runtime effect]
  (let [handler @(:transport-send runtime)]
    (when-not handler
      (throw
       (choreo-error
        :missing-transport-handler
        "No browser Choreo participant-message transport is installed."
        {:execution-id (:execution-id effect)
         :generation (:generation effect)
         :message (:message effect)})))
    (handler (transport-context effect))))

(defn- physical-handlers
  [runtime]
  {:machine/local
   (fn [{:keys [effect]}]
     (realize-local! runtime effect))

   :machine/send
   (fn [{:keys [effect]}]
     (realize-send-payload! runtime effect))

   :transport/send
   (fn [{:keys [effect]}]
     (realize-transport-send! runtime effect))})

(defn- require-unclaimed-effect-slots!
  [shell-runtime]
  (let [existing (shell/handlers shell-runtime)
        collisions (set (filter #(contains? existing %) owned-effect-kinds))]
    (when (seq collisions)
      (throw
       (choreo-error
        :effect-handler-collision
        "Browser Choreo cannot attach because its shell effect slots are already occupied."
        {:effect-kinds collisions}))))
  shell-runtime)

(defn create
  "Attach one browser Choreo physical binding to an existing shell runtime.

   The shell remains the sole mutable owner of AdapterState and physical
   timer/transport/continuity resources. This runtime owns only per-instance
   physical handler registration.

   Options:

     :local-actions
       keyword -> local realization function

     :fx-handlers
       keyword -> Biff-style FX handler supplied to local machines

     :send-payload
       generic send-boundary payload constructor

     :transport-send
       participant-message physical transport handler

     :recover-incompatible-plan
       optional browser/application recovery action invoked after stale semantic
       ownership is revoked. Defaults to a full-page reload when available."
  ([shell-runtime]
   (create shell-runtime nil))
  ([shell-runtime options]
   (shell/require-shell! shell-runtime)
   (require-unclaimed-effect-slots! shell-runtime)
   (let [options (or options {})
         _ (require-map! "Browser Choreo options" options)
         _ (check-option-keys! options)
         local-actions (or (:local-actions options) {})
         fx-handlers (or (:fx-handlers options) {})
         _ (require-registration-map! "Browser Choreo :local-actions" local-actions)
         _ (require-registration-map! "Browser Choreo :fx-handlers" fx-handlers)
         _ (require-optional-callable! "Browser Choreo :send-payload"
                                       (:send-payload options))
         _ (require-optional-callable! "Browser Choreo :transport-send"
                                       (:transport-send options))
         _ (require-optional-callable!
            "Browser Choreo :recover-incompatible-plan"
            (:recover-incompatible-plan options))
         runtime
         {:gesso.live.browser.choreo/type runtime-type
          :gesso.live.browser.choreo/version runtime-version
          :shell shell-runtime
          :local-actions (atom local-actions)
          :fx-handlers (atom fx-handlers)
          :send-payload (atom (:send-payload options))
          :transport-send (atom (:transport-send options))
          :recover-incompatible-plan
          (atom
           (or (:recover-incompatible-plan options)
               default-incompatible-plan-recovery!))
          :installed-handlers (atom {})}
         handlers (physical-handlers runtime)]
     (doseq [[effect-kind handler] handlers]
       (shell/register-handler! shell-runtime effect-kind handler))
     (reset! (:installed-handlers runtime) handlers)
     runtime)))

(defn detach!
  "Detach only the physical effect handlers installed by this runtime.

   Adapter executions are not retired here. Lifecycle ownership belongs to the
   enclosing browser runtime; callers should retire/shutdown semantic work before
   detaching a live integration. Handler slots are removed only when they still
   contain the exact function installed by this runtime."
  [runtime]
  (let [runtime (require-runtime! runtime)
        shell-runtime (:shell runtime)
        installed @(:installed-handlers runtime)]
    (doseq [[effect-kind handler] installed]
      (when (identical? handler
                        (get (shell/handlers shell-runtime) effect-kind))
        (shell/unregister-handler! shell-runtime effect-kind)))
    (reset! (:installed-handlers runtime) {})
    :detached))

;; =============================================================================
;; Semantic event submission -- adapter remains sole arbiter
;; =============================================================================

(defn start-execution!
  "Submit an already-created portable machine execution to the browser adapter.

   Returns the shell dispatch result. If the execution remains active after
   synchronous effect interpretation, :execution-ref contains its adapter-issued
   generation. Immediate terminal execution therefore legitimately returns a nil
   reference."
  ([runtime execution-id machine-execution]
   (start-execution! runtime execution-id machine-execution nil))
  ([runtime execution-id machine-execution options]
   (let [runtime (require-runtime! runtime)
         options (or options {})
         _ (require-map! "Browser Choreo start options" options)
         allowed #{:target-id :replace-owner? :replace-execution?}
         unknown (seq (remove allowed (keys options)))]
     (when unknown
       (throw
        (choreo-error
         :unknown-start-options
         "Browser Choreo start options contain unsupported keys."
         {:unknown-keys (set unknown)
          :allowed-keys allowed})))
     (when-not (machine/execution? machine-execution)
       (throw
        (choreo-error
         :invalid-machine-execution
         "Browser Choreo requires a portable machine execution."
         {:execution machine-execution})))
     (require-non-nil! "Browser Choreo execution id" execution-id)
     (let [event
           (cond->
            {:event :execution/start
             :execution-id execution-id
             :execution machine-execution}
             (contains? options :target-id)
             (assoc :target-id (:target-id options))
             (contains? options :replace-owner?)
             (assoc :replace-owner? (:replace-owner? options))
             (contains? options :replace-execution?)
             (assoc :replace-execution? (:replace-execution? options)))
           dispatch-result (shell/dispatch! (:shell runtime) event)]
       (assoc dispatch-result
              :execution-ref (execution-ref runtime execution-id))))))

(defn retire!
  [runtime execution-ref reason]
  (let [runtime (require-runtime! runtime)
        {:keys [execution-id generation]}
        (require-execution-ref! execution-ref)]
    (shell/dispatch!
     (:shell runtime)
     {:event :execution/retire
      :execution-id execution-id
      :generation generation
      :reason reason})))

(defn- stale-plan-retirement!
  [runtime execution-id]
  (if-let [stale-ref (execution-ref runtime execution-id)]
    (try
      (retire! runtime stale-ref :incompatible-plan)
      {:status :retired
       :execution-ref stale-ref}
      (catch :default error
        ;; shell/dispatch! installs the pure AdapterState transition before it
        ;; interprets physical cleanup effects. A timer/XHR cancellation failure
        ;; therefore must not block stale-plan recovery once semantic ownership
        ;; has already disappeared. Preserve the physical failure diagnostically
        ;; and let authoritative reconstruction proceed.
        {:status (if (active? runtime execution-id)
                   :retirement-failed
                   :retired-with-cleanup-error)
         :execution-ref stale-ref
         :error error}))
    {:status :not-active
     :execution-ref nil}))

(defn- recover-incompatible-plan!
  [runtime execution-id format-status]
  (let [retirement (stale-plan-retirement! runtime execution-id)
        context {:reason :incompatible-plan
                 :execution-id execution-id
                 :format-status format-status
                 :retirement retirement}
        handler @(:recover-incompatible-plan runtime)]
    (try
      {:status :incompatible-plan
       :execution-id execution-id
       :execution-ref nil
       :format-status format-status
       :retirement retirement
       :recovery-result (handler context)}
      (catch :default error
        ;; Recovery failure cannot restore a generation that was already
        ;; semantically revoked. Surface the physical failure explicitly so the
        ;; caller/runtime can escalate while the stale execution stays powerless.
        (throw
         (choreo-error
          :incompatible-plan-recovery-failed
          "Browser recovery from an incompatible ExecutablePlan failed after stale semantic retirement."
          {:execution-id execution-id
           :format-status format-status
           :retirement retirement}
          error))))))

(defn start-plan!
  "Create a portable machine execution from executable-plan and submit it.

   :machine-options are passed only to gesso.choreo.machine/start.
   :adapter-options are passed only to start-execution!.

   A recognizable but incompatible ExecutablePlan is a browser recovery
   condition rather than a malformed-plan error. The runtime first semantically
   retires any currently active generation with execution-id, then invokes the
   configured authoritative reconstruction action. It never attempts to migrate
   or continue suspended state from the incompatible plan.

   Malformed/noncanonical plans still flow through machine/start and fail hard."
  ([runtime execution-id executable-plan]
   (start-plan! runtime execution-id executable-plan nil nil))
  ([runtime execution-id executable-plan machine-options adapter-options]
   (let [runtime (require-runtime! runtime)
         _ (require-non-nil! "Browser Choreo execution id" execution-id)
         {:keys [status] :as format-status}
         (machine/executable-plan-format-status executable-plan)]
     (case status
       :incompatible
       (recover-incompatible-plan! runtime execution-id format-status)

       :compatible
       (start-execution!
        runtime
        execution-id
        (machine/start executable-plan (or machine-options {}))
        adapter-options)

       ;; Delegate malformed-plan and impossible-classifier behavior to the
       ;; portable machine so this browser layer does not grow a second format
       ;; validator or error taxonomy.
       (start-execution!
        runtime
        execution-id
        (machine/start executable-plan (or machine-options {}))
        adapter-options)))))

(defn deliver-message!
  "Submit one participant message using the generation captured by its callback."
  [runtime execution-ref message-id envelope]
  (let [runtime (require-runtime! runtime)
        {:keys [execution-id generation]}
        (require-execution-ref! execution-ref)]
    (require-non-nil! "Browser Choreo physical message id" message-id)
    (require-map! "Browser Choreo participant envelope" envelope)
    (shell/dispatch!
     (:shell runtime)
     {:event :machine/message
      :execution-id execution-id
      :generation generation
      :message-id message-id
      :envelope envelope})))

(defn deliver-environment!
  "Submit one role-local environment event using a captured execution generation."
  [runtime execution-ref envelope]
  (let [runtime (require-runtime! runtime)
        {:keys [execution-id generation]}
        (require-execution-ref! execution-ref)]
    (require-map! "Browser Choreo environment envelope" envelope)
    (shell/dispatch!
     (:shell runtime)
     {:event :machine/environment
      :execution-id execution-id
      :generation generation
      :envelope envelope})))

(defn retry!
  [runtime execution-ref]
  (let [runtime (require-runtime! runtime)
        {:keys [execution-id generation]}
        (require-execution-ref! execution-ref)]
    (shell/dispatch!
     (:shell runtime)
     {:event :machine/retry
      :execution-id execution-id
      :generation generation})))

(defn schedule!
  "Schedule an adapter-owned timer which will later deliver envelope.

   The shell owns the physical timeout handle; this namespace never does."
  [runtime execution-ref timer-id delay-ms envelope]
  (let [runtime (require-runtime! runtime)
        {:keys [execution-id generation]}
        (require-execution-ref! execution-ref)]
    (require-non-nil! "Browser Choreo timer id" timer-id)
    (require-nonnegative-integer! "Browser Choreo timer delay" delay-ms)
    (require-map! "Browser Choreo timer envelope" envelope)
    (shell/dispatch!
     (:shell runtime)
     {:event :timer/schedule
      :execution-id execution-id
      :generation generation
      :timer-id timer-id
      :delay-ms delay-ms
      :envelope envelope})))

(defn cancel-timer!
  [runtime execution-ref timer-id]
  (let [runtime (require-runtime! runtime)
        {:keys [execution-id generation]}
        (require-execution-ref! execution-ref)]
    (require-non-nil! "Browser Choreo timer id" timer-id)
    (shell/dispatch!
     (:shell runtime)
     {:event :timer/cancel
      :execution-id execution-id
      :generation generation
      :timer-id timer-id})))

;; =============================================================================
;; Diagnostics
;; =============================================================================

(defn diagnostics
  "Return configuration and shared-shell diagnostics without host resources."
  [runtime]
  (let [runtime (require-runtime! runtime)]
    {:gesso.live.browser.choreo/type runtime-type
     :gesso.live.browser.choreo/version runtime-version
     :registered-local-actions (set (keys @(:local-actions runtime)))
     :registered-fx-handlers (set (keys @(:fx-handlers runtime)))
     :send-payload-handler? (boolean @(:send-payload runtime))
     :transport-handler? (boolean @(:transport-send runtime))
     :incompatible-plan-recovery-handler?
     (boolean @(:recover-incompatible-plan runtime))
     :attached-effect-kinds (set (keys @(:installed-handlers runtime)))
     :shell (shell/diagnostics (:shell runtime))}))
