(ns gesso.live.browser.optimistic
  "Physical browser realization for protocol-v3 optimism.

   This namespace is deliberately subordinate to the portable optimistic
   choreography, the pure browser adapter, and the browser shell.

   It owns only integration and physical-realization concerns:

   - realize the optimistic Choreo local actions through application callbacks;
   - render an adapter-derived provisional value into one logical DOM target;
   - hold an opaque structural snapshot only in the browser shell;
   - realize the terminal disposition already selected by the adapter;
   - request canonical authority through an injected Live/HTMX refresh hook;
   - normalize trusted settlement/supersession observations back into AdapterState.

   It does NOT own command semantics, settlement semantics, target ownership,
   execution generations, timeout ownership, rollback eligibility, terminal
   disposition selection, continuity generations, authoritative progression,
   participant transport, or a second optimistic execution registry.

   A structural snapshot is never itself proof of authority.  When a new
   provisional installation begins on DOM already marked provisional, the old
   DOM may be used only to compensate a failed *physical installation*.  It is
   never retained as a terminal rollback baseline.  This prevents one retired
   provisional generation from being resurrected by a later command.

   Likewise, semantic retirement does not make provisional DOM authoritative.
   Provisional markers remain until canonical markup replaces them or a safe
   rollback restores the pre-provisional structural baseline."
  (:require
   [clojure.set :as set]
   [gesso.choreo.machine :as machine]
   [gesso.live.browser.adapter :as adapter]
   [gesso.live.browser.choreo :as browser-choreo]
   [gesso.live.browser.dom :as dom]
   [gesso.live.browser.shell :as shell]
   [gesso.live.optimistic.choreo :as optimistic-choreo]
   [gesso.live.optimistic.protocol :as protocol]))

;; =============================================================================
;; Identity / public constants
;; =============================================================================

(def runtime-version 3)

(def runtime-type
  :gesso.live.browser.optimistic/runtime)

(def active-attr
  "data-gesso-optimistic-active")

(def command-attr
  "data-gesso-optimistic-command")

(def provisional-attr
  "data-gesso-optimistic-provisional")

(def default-timeout-ms
  15000)

(def owned-effect-kinds
  #{:optimistic/install-provisional
    :optimistic/finish})

(def option-keys
  #{:project-provisional
    :render-provisional
    :refresh-authority
    :resolve-target
    :process-element
    :browser-role
    :authority-role
    :derive-action
    :resolve-action})

(def start-option-keys
  #{:plan
    :command
    :target-id
    :rollback-eligible?
    :timeout-ms
    :replace-owner?
    :replace-execution?})

(def ^:private command-fact-keys
  (set/union
   optimistic-choreo/semantic-command-required-keys
   optimistic-choreo/semantic-command-optional-keys))

;; =============================================================================
;; Errors / validation
;; =============================================================================

(defn- optimistic-error
  ([kind message data]
   (optimistic-error kind message data nil))
  ([kind message data cause]
   (ex-info
    message
    (merge
     {:error/type :gesso.live.browser.optimistic/error
      :error/kind kind}
     data)
    cause)))

(defn- require-map!
  [label value]
  (when-not (map? value)
    (throw
     (optimistic-error
      :invalid-map
      (str label " must be a map.")
      {:label label
       :value value})))
  value)

(defn- require-callable!
  [label value]
  (when-not (fn? value)
    (throw
     (optimistic-error
      :invalid-callable
      (str label " must be callable.")
      {:label label
       :value value})))
  value)

(defn- require-keyword!
  [label value]
  (when-not (keyword? value)
    (throw
     (optimistic-error
      :invalid-keyword
      (str label " must be a keyword.")
      {:label label
       :value value})))
  value)

(defn- require-non-nil!
  [label value]
  (when (nil? value)
    (throw
     (optimistic-error
      :missing-value
      (str label " must be non-nil.")
      {:label label})))
  value)

(defn- require-boolean!
  [label value]
  (when-not (boolean? value)
    (throw
     (optimistic-error
      :invalid-boolean
      (str label " must be boolean.")
      {:label label
       :value value})))
  value)

(defn- require-nonnegative-integer!
  [label value]
  (when-not (and (integer? value)
                 (<= 0 value))
    (throw
     (optimistic-error
      :invalid-nonnegative-integer
      (str label " must be a non-negative integer.")
      {:label label
       :value value})))
  value)

(defn- check-keys!
  [label allowed value]
  (require-map! label value)
  (let [unknown (seq (remove allowed (keys value)))]
    (when unknown
      (throw
       (optimistic-error
        :unknown-options
        (str label " contains unsupported keys.")
        {:label label
         :unknown-keys (set unknown)
         :allowed-keys allowed}))))
  value)

(defn- promise-like?
  [value]
  (boolean
   (and value
        (fn? (.-then value)))))

(defn- require-synchronous!
  [label value]
  (when (promise-like? value)
    (throw
     (optimistic-error
      :async-physical-realization
      (str label " must complete synchronously.")
      {:label label})))
  value)

;; =============================================================================
;; Small host helpers
;; =============================================================================

(defn- default-resolve-target
  [target-id]
  (dom/by-id (str target-id)))

(defn- default-process-element
  [element]
  ;; HTMX is a foreign JavaScript library rather than Closure-managed code.
  ;; Quoted lookup is therefore required here: ordinary CLJS property access
  ;; may rename `htmx` or `process` under :advanced compilation. Preserve the
  ;; method receiver as well, so this remains valid even if HTMX eventually
  ;; relies on `this` inside process().
  (let [htmx (aget js/window "htmx")
        process-element (when htmx
                          (aget htmx "process"))]
    (when (and element process-element)
      (.call process-element htmx element)))
  element)

(defn- command-wire
  [command-id]
  (pr-str
   (protocol/command-id->wire command-id)))

(defn- ownership-token
  [{:keys [execution-id generation]}]
  ;; This is opaque physical correlation, not semantic identity reconstruction.
  ;; The exact adapter-issued generation is deliberately part of the token so a
  ;; replacement generation using the same execution-id cannot inherit physical
  ;; ownership accidentally.
  (pr-str
   {:execution (protocol/execution-id->wire execution-id)
    :generation generation}))

(defn- optimistic-marked?
  [target]
  (boolean
   (and target
        (or (dom/has-attr? target active-attr)
            (dom/has-attr? target command-attr)
            (dom/has-attr? target provisional-attr)))))

(defn- mark-provisional!
  [target effect-data]
  (dom/set-attr! target active-attr
                 (ownership-token effect-data))
  (dom/set-attr! target command-attr
                 (command-wire (:command-id effect-data)))
  (dom/set-attr! target provisional-attr "true")
  (dom/set-attr! target "aria-busy" "true")
  target)

(defn- physically-owned?
  [target effect-data resource]
  (and target
       resource
       (= (:ownership-token resource)
          (ownership-token effect-data))
       (= (:ownership-token resource)
          (dom/attr target active-attr))))

(defn- safe-rollback-baseline?
  [target]
  ;; Absence of optimistic markers is not a proof of arbitrary application
  ;; authority.  It only establishes the narrow fact required here: this
  ;; snapshot is not itself a known Gesso provisional generation.  The normal
  ;; Live/HTMX authority path remains responsible for canonical progression.
  (not (optimistic-marked? target)))

(defn- restore-install-snapshot!
  [runtime target snapshot]
  (dom/copy-element-into!
   target
   (dom/snapshot-node snapshot))
  ((:process-element runtime) target)
  target)

(defn- rethrow-install-failure!
  [runtime target install-snapshot error]
  ;; Installation is a physical effect.  If the renderer mutated the target
  ;; before failing, compensate back to the exact pre-install DOM.  This is not
  ;; semantic rollback and is safe even when that pre-install DOM was itself an
  ;; older marked provisional representation.
  (try
    (restore-install-snapshot! runtime target install-snapshot)
    (catch :default compensation-error
      (throw
       (optimistic-error
        :provisional-install-compensation-failed
        "Optimistic provisional installation failed and physical compensation also failed."
        {:original-message (or (.-message error) (str error))
         :compensation-message (or (.-message compensation-error)
                                   (str compensation-error))}
        error))))
  (throw error))

;; =============================================================================
;; Runtime structure
;; =============================================================================

(defn runtime?
  [value]
  (and
   (map? value)
   (= runtime-type
      (:gesso.live.browser.optimistic/type value))
   (= runtime-version
      (:gesso.live.browser.optimistic/version value))
   (browser-choreo/runtime? (:choreo value))
   (shell/shell? (:shell value))
   (identical?
    (:shell value)
    (browser-choreo/shell-runtime (:choreo value)))
   (some? (:installed-handlers value))
   (some? (:installed-actions value))))

(defn require-runtime!
  [value]
  (when-not (runtime? value)
    (throw
     (optimistic-error
      :invalid-runtime
      "Expected a Gesso Live browser optimistic runtime."
      {:value value})))
  value)

(defn shell-runtime
  [runtime]
  (:shell (require-runtime! runtime)))

(defn choreo-runtime
  [runtime]
  (:choreo (require-runtime! runtime)))

(defn state
  "Return the one shared pure AdapterState."
  [runtime]
  (shell/state (shell-runtime runtime)))

(defn- require-unclaimed-effect-slots!
  [shell-runtime]
  (let [existing (shell/handlers shell-runtime)
        collisions (set (filter #(contains? existing %) owned-effect-kinds))]
    (when (seq collisions)
      (throw
       (optimistic-error
        :effect-handler-collision
        "Browser optimism cannot attach because its shell effect slots are already occupied."
        {:effect-kinds collisions}))))
  shell-runtime)

(defn- require-unclaimed-local-actions!
  [choreo-runtime action-ids]
  (let [existing (browser-choreo/local-actions choreo-runtime)
        collisions (set (filter #(contains? existing %) action-ids))]
    (when (seq collisions)
      (throw
       (optimistic-error
        :local-action-collision
        "Browser optimism cannot attach because its Choreo local action ids are already occupied."
        {:action-ids collisions}))))
  choreo-runtime)

;; =============================================================================
;; Protocol-v3 local actions
;; =============================================================================

(defn- command-from-local-context
  [ctx]
  (protocol/command
   (select-keys ctx command-fact-keys)))

(defn- provisional-from-projection
  [command projection]
  (protocol/provisional
   (cond->
    {:command-id (get command protocol/command-id-key)
     :execution-id (get command protocol/execution-id-key)
     :observed-basis (get command protocol/observed-basis-key)
     :projection projection}
     (contains? command protocol/scope-key)
     (assoc :scope (get command protocol/scope-key))

     (contains? command protocol/fact-versions-key)
     (assoc :fact-versions (get command protocol/fact-versions-key)))))

(defn- derive-provisional-handler
  [runtime]
  (fn [ctx]
    (let [command (command-from-local-context ctx)
          projector (:project-provisional runtime)
          result
          (projector
           {:command command
            :arguments (get command protocol/arguments-key)
            :observed-basis (get command protocol/observed-basis-key)
            :scope (get command protocol/scope-key)})
          ->outputs
          (fn [projection]
            {optimistic-choreo/provisional-value-key
             (provisional-from-projection command projection)})]
      (if (promise-like? result)
        (.then result ->outputs)
        (->outputs result)))))

(defn- resolve-settlement-handler
  [_runtime]
  (fn [ctx]
    {optimistic-choreo/resolution-value-key
     (optimistic-choreo/settlement-resolution
      (get ctx optimistic-choreo/provisional-value-key)
      (get ctx optimistic-choreo/settlement-value-key))}))

;; =============================================================================
;; Physical provisional installation / finish
;; =============================================================================

(defn- resolve-target!
  [runtime target-id]
  (let [target ((:resolve-target runtime) target-id)]
    (dom/require-connected!
     target
     {:operation :optimistic/resolve-target
      :target-id target-id})
    target))

(defn- install-provisional-handler
  [runtime]
  (fn [{:keys [effect]}]
    (let [target (resolve-target! runtime (:target-id effect))
          install-snapshot (dom/snapshot target)
          rollback-safe? (safe-rollback-baseline? target)]
      (try
        (let [provisional (:provisional effect)
              projection (get provisional protocol/projection-key)
              rendered
              ((:render-provisional runtime)
               {:target target
                :target-id (:target-id effect)
                :provisional provisional
                :projection projection
                :command-id (:command-id effect)
                :execution-id (:execution-id effect)
                :generation (:generation effect)})
              _ (require-synchronous!
                 "Optimistic provisional renderer"
                 rendered)]
          (when (and rendered
                     (not (dom/element? rendered)))
            (throw
             (optimistic-error
              :invalid-rendered-provisional
              "Optimistic provisional renderer must return nil or one DOM element."
              {:rendered rendered
               :target-id (:target-id effect)})))
          (when (and rendered
                     (not (identical? rendered target)))
            ;; Preserve the existing physical target object.  HTMX or other
            ;; browser code may already hold it while this command is running.
            (dom/copy-element-into! target rendered))
          (mark-provisional! target effect)
          ((:process-element runtime) target)
          {:gesso.live.browser.optimistic/resource true
           :target-id (:target-id effect)
           :execution-id (:execution-id effect)
           :generation (:generation effect)
           :ownership-token (ownership-token effect)
           ;; Always retain the pre-install snapshot for *installation failure*
           ;; compensation while this function is executing.  Once installation
           ;; succeeds, only a non-provisional baseline may be used for terminal
           ;; rollback.
           :rollback-snapshot
           (when rollback-safe?
             install-snapshot)
           :rollback-baseline
           (if rollback-safe?
             :non-provisional
             :provisional)})
        (catch :default error
          (rethrow-install-failure!
           runtime target install-snapshot error))))))

(defn- optional-target
  [runtime target-id]
  (try
    ((:resolve-target runtime) target-id)
    (catch :default _
      nil)))

(defn- refresh-authority!
  ([runtime effect resource target reason]
   (refresh-authority! runtime effect resource target reason nil))
  ([runtime effect resource target reason details]
   (let [result
         ((:refresh-authority runtime)
          (merge
           {:effect effect
            :resource resource
            :target target
            :target-id (:target-id effect)
            :reason reason}
           (or details {})))]
     (require-synchronous! "Optimistic authoritative refresh" result)
     :requested)))

(defn- rollback!
  [runtime effect resource target]
  (cond
    (nil? resource)
    :missing-resource

    (not (physically-owned? target effect resource))
    :ownership-lost

    (nil? (:rollback-snapshot resource))
    :unsafe-baseline

    :else
    (do
      (dom/copy-element-into!
       target
       (dom/snapshot-node (:rollback-snapshot resource)))
      ((:process-element runtime) target)
      :rolled-back)))

(defn- recover-authority-after-unavailable-rollback!
  [runtime effect resource target rollback-result]
  ;; The adapter has already selected a rollback disposition.  This branch does
  ;; not reinterpret that semantic decision; it is conservative recovery when
  ;; the requested physical rollback cannot be realized safely because the
  ;; target was replaced, the install resource is missing, or the only captured
  ;; baseline was itself provisional.
  (let [reason
        (case rollback-result
          :ownership-lost :rollback-ownership-lost
          :unsafe-baseline :rollback-unsafe-baseline
          :missing-resource :rollback-resource-missing
          :rollback-unavailable)]
    (refresh-authority!
     runtime effect resource target reason
     {:rollback-result rollback-result})))

(defn- finish-handler
  [runtime]
  (fn [{:keys [effect resource]}]
    (let [target (optional-target runtime (:target-id effect))
          disposition (:disposition effect)]
      (case disposition
        :rollback
        (let [result (rollback! runtime effect resource target)]
          (when-not (= :rolled-back result)
            (recover-authority-after-unavailable-rollback!
             runtime effect resource target result)))

        :rollback-and-refresh
        (let [result (rollback! runtime effect resource target)]
          (refresh-authority!
           runtime effect resource target
           :rollback-and-refresh
           {:rollback-result result}))

        :refresh-authority
        ;; Keep the provisional marker in place.  Semantic retirement does not
        ;; make provisional DOM authoritative while the canonical refresh is in
        ;; flight.
        (refresh-authority!
         runtime effect resource target :refresh-authority)

        :await-authority
        ;; Direct settlement established a semantic resolution, not canonical
        ;; DOM.  The provisional representation remains explicitly provisional
        ;; until the ordinary Live/HTMX authority path replaces it.
        (refresh-authority!
         runtime effect resource target :await-authority)

        :authoritative
        ;; A trusted authoritative observation has semantically superseded this
        ;; execution.  Physical canonical installation belongs to the normal
        ;; Live/HTMX path that established/observed that authority.  If the old
        ;; provisional node is still present, leave its marker intact rather
        ;; than silently upgrading it.
        nil

        :release-only
        ;; Semantic ownership has ended.  Physical remnants are cleanup debt.
        ;; Do not erase the provisional marker or restore a snapshot merely to
        ;; make retired DOM look canonical.
        nil

        (throw
         (optimistic-error
          :unknown-disposition
          "Browser optimism received an unsupported adapter disposition."
          {:disposition disposition
           :effect effect})))
      :finished)))

;; =============================================================================
;; Construction / detachment
;; =============================================================================

(defn create
  "Attach protocol-v3 optimism to one existing browser Choreo runtime.

   Required options:

     :project-provisional
       Application semantic projector called from the Choreo local derive
       boundary. Receives {:command ... :arguments ... :observed-basis ...
       :scope ...} and returns the semantic provisional projection. It may
       return a Promise; browser.choreo/shell correlate async completion through
       adapter effect generations.

     :render-provisional
       Synchronous physical renderer. Receives the resolved DOM target plus the
       closed protocol-v3 provisional value and projection. It may mutate target
       directly and return nil/target, or return a same-root DOM element that
       Gesso copies into the existing target object.

     :refresh-authority
       Synchronous request to the ordinary Live authoritative-refresh path. It
       does not choose whether refresh is semantically required; the adapter has
       already selected the terminal disposition. The callback may initiate an
       asynchronous HTMX request but must return synchronously.

   Optional options customize target resolution, HTMX processing, role names,
   and the two optimistic Choreo local-action ids."
  ([choreo-runtime options]
   (browser-choreo/require-runtime! choreo-runtime)
   (let [options
         (check-keys!
          "Browser optimistic options"
          option-keys
          (or options {}))
         project-provisional
         (require-callable!
          "Browser optimistic :project-provisional"
          (:project-provisional options))
         render-provisional
         (require-callable!
          "Browser optimistic :render-provisional"
          (:render-provisional options))
         refresh-authority
         (require-callable!
          "Browser optimistic :refresh-authority"
          (:refresh-authority options))
         resolve-target
         (or (:resolve-target options) default-resolve-target)
         process-element
         (or (:process-element options) default-process-element)
         browser-role
         (or (:browser-role options)
             optimistic-choreo/default-browser-role)
         authority-role
         (or (:authority-role options)
             optimistic-choreo/default-authority-role)
         derive-action
         (or (:derive-action options)
             optimistic-choreo/derive-provisional-action)
         resolve-action
         (or (:resolve-action options)
             optimistic-choreo/resolve-settlement-action)
         _ (require-callable!
            "Browser optimistic :resolve-target"
            resolve-target)
         _ (require-callable!
            "Browser optimistic :process-element"
            process-element)
         _ (require-keyword!
            "Browser optimistic :browser-role"
            browser-role)
         _ (require-keyword!
            "Browser optimistic :authority-role"
            authority-role)
         _ (require-keyword!
            "Browser optimistic :derive-action"
            derive-action)
         _ (require-keyword!
            "Browser optimistic :resolve-action"
            resolve-action)
         shell-runtime (browser-choreo/shell-runtime choreo-runtime)
         _ (require-unclaimed-effect-slots! shell-runtime)
         _ (require-unclaimed-local-actions!
            choreo-runtime #{derive-action resolve-action})
         runtime
         {:gesso.live.browser.optimistic/type runtime-type
          :gesso.live.browser.optimistic/version runtime-version
          :choreo choreo-runtime
          :shell shell-runtime
          :project-provisional project-provisional
          :render-provisional render-provisional
          :refresh-authority refresh-authority
          :resolve-target resolve-target
          :process-element process-element
          :browser-role browser-role
          :authority-role authority-role
          :derive-action derive-action
          :resolve-action resolve-action
          :installed-handlers (atom {})
          :installed-actions (atom {})}
         handlers
         {:optimistic/install-provisional
          (install-provisional-handler runtime)

          :optimistic/finish
          (finish-handler runtime)}
         actions
         {derive-action (derive-provisional-handler runtime)
          resolve-action (resolve-settlement-handler runtime)}]
     (doseq [[effect-kind handler] handlers]
       (shell/register-handler! shell-runtime effect-kind handler))
     (doseq [[action-id handler] actions]
       (browser-choreo/register-local-action!
        choreo-runtime action-id handler))
     (reset! (:installed-handlers runtime) handlers)
     (reset! (:installed-actions runtime) actions)
     runtime)))

(defn detach!
  "Detach only physical handlers/actions still owned by this optimistic runtime.

   Active semantic executions are not retired here. The enclosing composed
   runtime must retire/close semantic work before physical detachment."
  [runtime]
  (let [runtime (require-runtime! runtime)
        shell-runtime (:shell runtime)
        choreo-runtime (:choreo runtime)]
    (doseq [[effect-kind handler] @(:installed-handlers runtime)]
      (when (identical? handler
                        (get (shell/handlers shell-runtime) effect-kind))
        (shell/unregister-handler! shell-runtime effect-kind)))
    (doseq [[action-id handler] @(:installed-actions runtime)]
      (when (identical? handler
                        (get (browser-choreo/local-actions choreo-runtime)
                             action-id))
        (browser-choreo/unregister-local-action!
         choreo-runtime action-id)))
    (reset! (:installed-handlers runtime) {})
    (reset! (:installed-actions runtime) {})
    :detached))

;; =============================================================================
;; Starting one optimistic execution
;; =============================================================================

(defn- normalize-command
  [command]
  (protocol/command
   (dissoc
    (require-map! "Optimistic command" command)
    protocol/protocol-version-key)))

(defn start!
  "Start one protocol-v3 optimistic browser projection.

   The caller supplies a preverified browser ExecutablePlan. This namespace does
   not verify/project at runtime and therefore cannot manufacture a different
   semantic plan in the browser.

   Provisional state is NOT supplied here. The portable Choreo local derive step
   establishes it, after which the adapter emits :optimistic/install-provisional,
   then starts any settlement timeout, then continues toward transport.

   Returns the shell dispatch result with :execution-ref when the execution
   remains active after synchronous initial effects."
  [runtime options]
  (let [runtime (require-runtime! runtime)
        options
        (check-keys!
         "Optimistic start options"
         start-option-keys
         options)
        plan
        (require-non-nil!
         "Optimistic browser ExecutablePlan"
         (:plan options))
        command (normalize-command (:command options))
        command-values (optimistic-choreo/command-values command)
        target-id
        (require-non-nil!
         "Optimistic target id"
         (:target-id options))
        rollback-eligible?
        (get options :rollback-eligible? true)
        timeout-ms
        (if (contains? options :timeout-ms)
          (:timeout-ms options)
          default-timeout-ms)
        replace-owner? (get options :replace-owner? false)
        replace-execution? (get options :replace-execution? false)
        _ (require-boolean! ":rollback-eligible?" rollback-eligible?)
        _ (when (some? timeout-ms)
            (require-nonnegative-integer! ":timeout-ms" timeout-ms))
        _ (require-boolean! ":replace-owner?" replace-owner?)
        _ (require-boolean! ":replace-execution?" replace-execution?)
        command-id (get command protocol/command-id-key)
        execution-id (get command protocol/execution-id-key)
        _
        (when-not (= (:browser-role runtime) (:role plan))
          (throw
           (optimistic-error
            :wrong-plan-role
            "Optimistic browser runtime requires the configured browser-role ExecutablePlan."
            {:expected-role (:browser-role runtime)
             :actual-role (:role plan)})))
        machine-execution
        (machine/start
         plan
         {:command-id command-id
          :execution-id execution-id
          :values command-values})
        optimistic-config
        (cond->
         {:command-id command-id
          :provisional-key optimistic-choreo/provisional-value-key
          :rollback-eligible? rollback-eligible?}
          (some? timeout-ms)
          (assoc :timeout-ms timeout-ms))
        event
        {:event :execution/start
         :execution-id execution-id
         :execution machine-execution
         :target-id target-id
         :replace-owner? replace-owner?
         :replace-execution? replace-execution?
         :optimistic optimistic-config}
        dispatch-result
        (shell/dispatch! (:shell runtime) event)]
    (assoc dispatch-result
           :command command
           :execution-ref
           (browser-choreo/execution-ref
            (:choreo runtime)
            execution-id))))

;; =============================================================================
;; Settlement / supersession normalization
;; =============================================================================

(defn- current-scope
  [runtime execution-ref]
  (let [{:keys [execution-id generation]}
        (browser-choreo/require-execution-ref! execution-ref)
        scope (adapter/optimistic-scope (state runtime) execution-id)]
    (when (and scope
               (= generation (:execution-generation scope)))
      scope)))

(defn- require-settlement-correlation!
  [execution-ref scope settlement]
  (let [{:keys [execution-id]}
        (browser-choreo/require-execution-ref! execution-ref)
        settlement-execution-id
        (get settlement protocol/execution-id-key)]
    (when-not (= execution-id settlement-execution-id)
      (throw
       (optimistic-error
        :settlement-correlation-mismatch
        "Optimistic settlement does not correlate with the supplied execution reference."
        {:execution-id execution-id
         :settlement-execution-id settlement-execution-id})))
    (when (and scope
               (not= (:command-id scope)
                     (get settlement protocol/command-id-key)))
      (throw
       (optimistic-error
        :settlement-correlation-mismatch
        "Optimistic settlement command-id does not match the current optimistic scope."
        {:execution-id execution-id
         :expected-command-id (:command-id scope)
         :settlement-command-id
         (get settlement protocol/command-id-key)})))
    settlement))

(defn settle!
  "Observe one trusted protocol-v3 settlement and, when the referenced
   generation is still current, deliver its participant message to the projected
   browser machine.

   Stale/retired execution references are submitted to the adapter unchanged so
   the adapter generation gate can classify them as stale instead of this
   integration layer throwing merely because a late callback arrived.

   Returns the settlement-observation dispatch result with :message-dispatch set
   to the subsequent machine-message dispatch when one was required."
  ([runtime execution-ref settlement]
   (settle! runtime execution-ref settlement nil))
  ([runtime execution-ref settlement message-id]
   (let [runtime (require-runtime! runtime)
         {:keys [execution-id generation]}
         (browser-choreo/require-execution-ref! execution-ref)
         scope-before (current-scope runtime execution-ref)
         settlement'
         (protocol/settlement
          (dissoc
           (require-map! "Optimistic settlement" settlement)
           protocol/protocol-version-key))
         _ (require-settlement-correlation!
            execution-ref scope-before settlement')
         resolution
         (if (map? (:provisional scope-before))
           (optimistic-choreo/settlement-resolution
            (:provisional scope-before)
            settlement')
           (get settlement' protocol/resolution-key))
         already-observed?
         (= :settlement-observed (:status scope-before))
         settlement-dispatch
         (shell/dispatch!
          (:shell runtime)
          {:event :optimistic/settlement-observed
           :execution-id execution-id
           :generation generation
           :resolution resolution
           :settlement settlement'})
         message-dispatch
         (when (and scope-before
                    (not already-observed?))
           (let [envelope
                 (machine/message
                  (:authority-role runtime)
                  (:browser-role runtime)
                  protocol/settlement-event
                  (optimistic-choreo/settlement-message-values settlement')
                  {:via :http})
                 message-id'
                 (or message-id
                     (str "gesso-optimistic-settlement-"
                          (random-uuid)))]
             (browser-choreo/deliver-message!
              (:choreo runtime)
              execution-ref
              message-id'
              envelope)))]
     (assoc settlement-dispatch
            :message-dispatch message-dispatch))))

(defn supersede!
  "Submit one trusted authoritative supersession observation.

   The caller must already have established through the trusted Live/authority
   path that this observation supersedes the provisional trajectory. Merely
   constructing protocol-shaped browser data does not establish progression.

   Stale execution references are still submitted to the adapter so its
   generation gate, not mutable DOM coincidence, decides whether the event can
   affect semantic state. Physical canonical installation remains the normal
   Live/HTMX path's responsibility."
  [runtime execution-ref authoritative-observation]
  (let [runtime (require-runtime! runtime)
        {:keys [execution-id generation]}
        (browser-choreo/require-execution-ref! execution-ref)
        authoritative'
        (protocol/authoritative
         (dissoc
          (require-map!
           "Optimistic authoritative observation"
           authoritative-observation)
          protocol/authority-key))]
    (shell/dispatch!
     (:shell runtime)
     {:event :optimistic/authoritative-superseded
      :execution-id execution-id
      :generation generation
      :authoritative authoritative'})))

(defn retire!
  [runtime execution-ref reason]
  (browser-choreo/retire!
   (:choreo (require-runtime! runtime))
   execution-ref
   reason))

;; =============================================================================
;; Diagnostics
;; =============================================================================

(defn diagnostics
  "Return host-resource-free optimistic integration diagnostics."
  [runtime]
  (let [runtime (require-runtime! runtime)]
    {:runtime-version runtime-version
     :browser-role (:browser-role runtime)
     :authority-role (:authority-role runtime)
     :derive-action (:derive-action runtime)
     :resolve-action (:resolve-action runtime)
     :attached-effect-kinds
     (set (keys @(:installed-handlers runtime)))
     :attached-local-actions
     (set (keys @(:installed-actions runtime)))
     :active-optimistic-executions
     (set (keys (:optimistic (state runtime))))}))
