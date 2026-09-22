(ns gesso.live.browser.runtime
  "Top-level composition root for the Gesso Live browser runtime.

   This namespace owns composition and aggregate lifecycle only. It does not
   introduce a second semantic runtime.

   One composed runtime contains:

     gesso.live.browser.core
       HTMX/document lifecycle normalization, continuity, and the one shared
       shell/AdapterState owner

     gesso.live.browser.choreo
       physical realization of portable Choreo effects attached to that exact
       same shell

     gesso.live.browser.optimistic (optional)
       protocol-v3 optimistic projection/settlement realization attached to the
       exact same Choreo runtime and shell

     gesso.live.browser.optimistic-htmx (optional)
       HTMX lifecycle/transport bridge attached to the exact same Core,
       optimistic runtime, Choreo runtime, and shell

   AdapterState remains owned exclusively by gesso.live.browser.shell through
   gesso.live.browser.adapter. This namespace never advances portable Choreo,
   interprets authoritative bases, chooses optimistic settlement policy, manages
   timers, stores browser resources, or creates another optimistic registry."
  (:require
   [gesso.live.browser.choreo :as choreo]
   [gesso.live.browser.core :as core]
   [gesso.live.browser.optimistic :as optimistic]
   [gesso.live.browser.optimistic-htmx :as optimistic-htmx]
   [gesso.live.browser.shell :as shell]))

;; =============================================================================
;; Identity / options
;; =============================================================================

(def runtime-version
  "1.3.0-dev")

(def runtime-type
  :gesso.live.browser.runtime/runtime)

(def option-keys
  #{:core-options
    :choreo-options
    :optimistic-options
    :optimistic-htmx-options})

(def lifecycle-states
  #{:created
    :started
    :stopped})

;; =============================================================================
;; Errors / validation
;; =============================================================================

(defn- runtime-error
  ([kind message data]
   (runtime-error kind message data nil))
  ([kind message data cause]
   (ex-info
    message
    (merge
     {:error/type :gesso.live.browser.runtime/error
      :error/kind kind}
     data)
    cause)))

(defn- require-map!
  [label value]
  (when-not (map? value)
    (throw
     (runtime-error
      :invalid-map
      (str label " must be a map.")
      {:label label
       :value value})))
  value)

(defn- check-option-keys!
  [options]
  (let [unknown
        (seq
         (remove
          option-keys
          (keys options)))]
    (when unknown
      (throw
       (runtime-error
        :unknown-options
        "Gesso Live browser runtime options contain unsupported keys."
        {:unknown-keys (set unknown)
         :allowed-keys option-keys}))))
  options)

;; =============================================================================
;; Runtime construction / accessors
;; =============================================================================

(defn runtime?
  [value]
  (and
   (map? value)
   (= runtime-type
      (:gesso.live.browser.runtime/type value))
   (= runtime-version
      (:gesso.live.browser.runtime/version value))
   (core/core? (:core value))
   (choreo/runtime? (:choreo value))
   (or (nil? (:optimistic value))
       (optimistic/runtime? (:optimistic value)))
   (identical?
    (core/shell-runtime (:core value))
    (choreo/shell-runtime (:choreo value)))
   (or
    (nil? (:optimistic value))
    (and
     (identical?
      (:choreo value)
      (optimistic/choreo-runtime (:optimistic value)))
     (identical?
      (core/shell-runtime (:core value))
      (optimistic/shell-runtime (:optimistic value)))))
   (or
    (nil? (:optimistic-htmx value))
    (and
     (:optimistic value)
     (optimistic-htmx/runtime? (:optimistic-htmx value))
     (identical?
      (:core value)
      (optimistic-htmx/core-runtime (:optimistic-htmx value)))
     (identical?
      (:optimistic value)
      (optimistic-htmx/optimistic-runtime (:optimistic-htmx value)))
     (identical?
      (:choreo value)
      (optimistic-htmx/choreo-runtime (:optimistic-htmx value)))))
   (some? (:lifecycle value))
   (some? (:public-api value))))

(defn require-runtime!
  [value]
  (when-not
   (runtime? value)
    (throw
     (runtime-error
      :invalid-runtime
      "Expected a composed Gesso Live browser runtime."
      {:value value})))
  value)

(defn core-runtime
  [runtime]
  (:core
   (require-runtime! runtime)))

(defn choreo-runtime
  [runtime]
  (:choreo
   (require-runtime! runtime)))

(defn optimistic-runtime
  "Return the optional protocol-v3 optimistic browser binding."
  [runtime]
  (:optimistic
   (require-runtime! runtime)))

(defn optimistic-htmx-runtime
  "Return the optional HTMX transport bridge for protocol-v3 optimism."
  [runtime]
  (:optimistic-htmx
   (require-runtime! runtime)))

(defn shell-runtime
  [runtime]
  (core/shell-runtime
   (core-runtime runtime)))

(defn continuity-runtime
  [runtime]
  (core/continuity-runtime
   (core-runtime runtime)))

(defn state
  "Return the one shared pure AdapterState."
  [runtime]
  (core/state
   (core-runtime runtime)))

(defn lifecycle
  [runtime]
  @(-> runtime
       require-runtime!
       :lifecycle))

(defn started?
  [runtime]
  (= :started
     (lifecycle runtime)))

(defn stopped?
  [runtime]
  (= :stopped
     (lifecycle runtime)))

;; =============================================================================
;; Composition invariants
;; =============================================================================

(defn- choreo-handler-ownership-errors
  [choreo-runtime shell-runtime]
  (let [installed-handlers
        @(:installed-handlers choreo-runtime)

        shell-handlers
        (shell/handlers shell-runtime)]

    (reduce
     (fn [errors effect-kind]
       (let [installed?
             (contains? installed-handlers effect-kind)

             shell-installed?
             (contains? shell-handlers effect-kind)

             expected-handler
             (get installed-handlers effect-kind)

             actual-handler
             (get shell-handlers effect-kind)]

         (cond
           (not installed?)
           (conj
            errors
            {:invariant :choreo-handler-ownership
             :effect-kind effect-kind
             :status :not-recorded})

           (not shell-installed?)
           (conj
            errors
            {:invariant :choreo-handler-ownership
             :effect-kind effect-kind
             :status :missing-from-shell})

           (not
            (identical?
             expected-handler
             actual-handler))
           (conj
            errors
            {:invariant :choreo-handler-ownership
             :effect-kind effect-kind
             :status :replaced-in-shell})

           :else
           errors)))
     []
     choreo/owned-effect-kinds)))

(defn- optimistic-ownership-errors
  [optimistic-runtime shell-runtime choreo-runtime]
  (when optimistic-runtime
    (let [installed-handlers
          @(:installed-handlers optimistic-runtime)

          installed-actions
          @(:installed-actions optimistic-runtime)

          shell-handlers
          (shell/handlers shell-runtime)

          choreo-actions
          (choreo/local-actions choreo-runtime)

          expected-actions
          #{(:derive-action optimistic-runtime)
            (:resolve-action optimistic-runtime)}]
      (into
       []
       (concat
        (mapcat
         (fn [effect-kind]
           (let [recorded? (contains? installed-handlers effect-kind)
                 shell-installed? (contains? shell-handlers effect-kind)
                 expected-handler (get installed-handlers effect-kind)
                 actual-handler (get shell-handlers effect-kind)]
             (cond
               (not recorded?)
               [{:invariant :optimistic-handler-ownership
                 :effect-kind effect-kind
                 :status :not-recorded}]

               (not shell-installed?)
               [{:invariant :optimistic-handler-ownership
                 :effect-kind effect-kind
                 :status :missing-from-shell}]

               (not (identical? expected-handler actual-handler))
               [{:invariant :optimistic-handler-ownership
                 :effect-kind effect-kind
                 :status :replaced-in-shell}]

               :else
               [])))
         optimistic/owned-effect-kinds)

        (mapcat
         (fn [action-id]
           (let [recorded? (contains? installed-actions action-id)
                 choreo-installed? (contains? choreo-actions action-id)
                 expected-handler (get installed-actions action-id)
                 actual-handler (get choreo-actions action-id)]
             (cond
               (not recorded?)
               [{:invariant :optimistic-action-ownership
                 :action-id action-id
                 :status :not-recorded}]

               (not choreo-installed?)
               [{:invariant :optimistic-action-ownership
                 :action-id action-id
                 :status :missing-from-choreo}]

               (not (identical? expected-handler actual-handler))
               [{:invariant :optimistic-action-ownership
                 :action-id action-id
                 :status :replaced-in-choreo}]

               :else
               [])))
         expected-actions))))))

(defn- optimistic-htmx-ownership-errors
  [bridge-runtime core-runtime choreo-runtime]
  (when bridge-runtime
    (let [recorded-observers @(:observer-handlers bridge-runtime)
          core-observers @(:event-observers core-runtime)
          expected-events (set optimistic-htmx/observed-events)
          recorded-events (set (keys recorded-observers))
          send-wrapper @(:send-payload-wrapper bridge-runtime)
          transport-wrapper @(:transport-wrapper bridge-runtime)
          current-send (choreo/send-payload-handler choreo-runtime)
          current-transport (choreo/transport-handler choreo-runtime)]
      (into
       []
       (concat
        (when (not= expected-events recorded-events)
          [{:invariant :optimistic-htmx-observer-set
            :expected expected-events
            :actual recorded-events}])

        (mapcat
         (fn [event-name]
           (let [expected-handler (get recorded-observers event-name)
                 actual-handler (get-in core-observers
                                        [event-name optimistic-htmx/observer-id])]
             (cond
               (nil? expected-handler)
               [{:invariant :optimistic-htmx-observer-ownership
                 :event-name event-name
                 :status :not-recorded}]

               (nil? actual-handler)
               [{:invariant :optimistic-htmx-observer-ownership
                 :event-name event-name
                 :status :missing-from-core}]

               (not (identical? expected-handler actual-handler))
               [{:invariant :optimistic-htmx-observer-ownership
                 :event-name event-name
                 :status :replaced-in-core}]

               :else
               [])))
         expected-events)

        (when-not (identical? send-wrapper current-send)
          [{:invariant :optimistic-htmx-send-wrapper-ownership
            :status (if current-send :replaced-in-choreo :missing-from-choreo)}])

        (when-not (identical? transport-wrapper current-transport)
          [{:invariant :optimistic-htmx-transport-wrapper-ownership
            :status (if current-transport :replaced-in-choreo :missing-from-choreo)}]))))))

(defn invariant-errors
  "Return read-only composition invariant violations.

   These checks observe lifecycle/configuration ownership only. They do not
   participate in adapter semantics and therefore cannot change the behavior of
   the executable browser state machine.

   Expected lifecycle shapes:

     :created
       core listeners absent; shell open; Choreo attached; optional optimism and
       optional optimistic HTMX bridge attached to their exact dependencies

     :started
       core listeners installed; shell open; Choreo, optional optimism, and the
       optional HTMX bridge still own the exact functions they registered

     :stopped
       core listeners absent; shell closed; bridge, optimism, and Choreo detached

   Active ownership checks compare the physical registries with the exact
   handler functions recorded by each integration runtime. Diagnostics never
   return those functions."
  [runtime]
  (let [runtime
        (require-runtime! runtime)

        core-runtime
        (:core runtime)

        choreo-runtime
        (:choreo runtime)

        optimistic-runtime
        (:optimistic runtime)

        optimistic-htmx-runtime
        (:optimistic-htmx runtime)

        core-shell
        (core/shell-runtime core-runtime)

        choreo-shell
        (choreo/shell-runtime choreo-runtime)

        optimistic-shell
        (when optimistic-runtime
          (optimistic/shell-runtime optimistic-runtime))

        optimistic-choreo
        (when optimistic-runtime
          (optimistic/choreo-runtime optimistic-runtime))

        bridge-core
        (when optimistic-htmx-runtime
          (optimistic-htmx/core-runtime optimistic-htmx-runtime))

        bridge-optimistic
        (when optimistic-htmx-runtime
          (optimistic-htmx/optimistic-runtime optimistic-htmx-runtime))

        bridge-choreo
        (when optimistic-htmx-runtime
          (optimistic-htmx/choreo-runtime optimistic-htmx-runtime))

        lifecycle-value
        @(:lifecycle runtime)

        core-diagnostics
        (core/diagnostics core-runtime)

        choreo-diagnostics
        (choreo/diagnostics choreo-runtime)

        optimistic-diagnostics
        (when optimistic-runtime
          (optimistic/diagnostics optimistic-runtime))

        optimistic-htmx-diagnostics
        (when optimistic-htmx-runtime
          (optimistic-htmx/diagnostics optimistic-htmx-runtime))

        core-started?
        (true? (:started? core-diagnostics))

        shell-closed?
        (shell/closed? core-shell)

        choreo-attached-effects
        (:attached-effect-kinds choreo-diagnostics)

        optimistic-attached-effects
        (:attached-effect-kinds optimistic-diagnostics)

        optimistic-attached-actions
        (:attached-local-actions optimistic-diagnostics)

        expected-optimistic-actions
        (when optimistic-runtime
          #{(:derive-action optimistic-runtime)
            (:resolve-action optimistic-runtime)})

        optimistic-htmx-attached?
        (true? (:attached? optimistic-htmx-diagnostics))

        active-choreo-ownership-errors
        (when (contains? #{:created :started} lifecycle-value)
          (choreo-handler-ownership-errors
           choreo-runtime
           core-shell))

        active-optimistic-ownership-errors
        (when (and optimistic-runtime
                   (contains? #{:created :started} lifecycle-value))
          (optimistic-ownership-errors
           optimistic-runtime
           core-shell
           choreo-runtime))

        active-optimistic-htmx-ownership-errors
        (when (and optimistic-htmx-runtime
                   (contains? #{:created :started} lifecycle-value))
          (optimistic-htmx-ownership-errors
           optimistic-htmx-runtime
           core-runtime
           choreo-runtime))

        base-errors
        (cond-> []
          (not (identical? core-shell choreo-shell))
          (conj
           {:invariant :one-shared-shell
            :message "Core and Choreo must share the exact same browser shell."})

          (and optimistic-runtime
               (not (identical? core-shell optimistic-shell)))
          (conj
           {:invariant :optimistic-shared-shell
            :message "Optimism must share the exact composed browser shell."})

          (and optimistic-runtime
               (not (identical? choreo-runtime optimistic-choreo)))
          (conj
           {:invariant :optimistic-shared-choreo
            :message "Optimism must attach to the exact composed Choreo runtime."})

          (and optimistic-htmx-runtime
               (nil? optimistic-runtime))
          (conj
           {:invariant :optimistic-htmx-requires-optimism
            :message "The optimistic HTMX bridge requires the composed optimistic runtime."})

          (and optimistic-htmx-runtime
               (not (identical? core-runtime bridge-core)))
          (conj
           {:invariant :optimistic-htmx-shared-core
            :message "The optimistic HTMX bridge must attach to the exact composed Core runtime."})

          (and optimistic-htmx-runtime
               (not (identical? optimistic-runtime bridge-optimistic)))
          (conj
           {:invariant :optimistic-htmx-shared-optimism
            :message "The optimistic HTMX bridge must attach to the exact composed optimistic runtime."})

          (and optimistic-htmx-runtime
               (not (identical? choreo-runtime bridge-choreo)))
          (conj
           {:invariant :optimistic-htmx-shared-choreo
            :message "The optimistic HTMX bridge must use the exact composed Choreo runtime."})

          (not (contains? lifecycle-states lifecycle-value))
          (conj
           {:invariant :known-lifecycle
            :lifecycle lifecycle-value
            :allowed lifecycle-states})

          (and (= :created lifecycle-value)
               core-started?)
          (conj
           {:invariant :created-core-not-started
            :lifecycle lifecycle-value})

          (and (= :created lifecycle-value)
               shell-closed?)
          (conj
           {:invariant :created-shell-open
            :lifecycle lifecycle-value})

          (and (= :created lifecycle-value)
               (not= choreo/owned-effect-kinds choreo-attached-effects))
          (conj
           {:invariant :created-choreo-attached
            :expected choreo/owned-effect-kinds
            :actual choreo-attached-effects})

          (and (= :started lifecycle-value)
               (not core-started?))
          (conj
           {:invariant :started-core-started
            :lifecycle lifecycle-value})

          (and (= :started lifecycle-value)
               shell-closed?)
          (conj
           {:invariant :started-shell-open
            :lifecycle lifecycle-value})

          (and (= :started lifecycle-value)
               (not= choreo/owned-effect-kinds choreo-attached-effects))
          (conj
           {:invariant :started-choreo-attached
            :expected choreo/owned-effect-kinds
            :actual choreo-attached-effects})

          (and optimistic-runtime
               (not= 2 (count expected-optimistic-actions)))
          (conj
           {:invariant :optimistic-distinct-local-actions
            :actual expected-optimistic-actions
            :message "Optimistic derive and settlement local actions must be distinct."})

          (and optimistic-runtime
               (contains? #{:created :started} lifecycle-value)
               (not= optimistic/owned-effect-kinds
                     optimistic-attached-effects))
          (conj
           {:invariant :optimistic-effects-attached
            :lifecycle lifecycle-value
            :expected optimistic/owned-effect-kinds
            :actual optimistic-attached-effects})

          (and optimistic-runtime
               (contains? #{:created :started} lifecycle-value)
               (not= expected-optimistic-actions
                     optimistic-attached-actions))
          (conj
           {:invariant :optimistic-actions-attached
            :lifecycle lifecycle-value
            :expected expected-optimistic-actions
            :actual optimistic-attached-actions})

          (and optimistic-htmx-runtime
               (contains? #{:created :started} lifecycle-value)
               (not optimistic-htmx-attached?))
          (conj
           {:invariant :optimistic-htmx-attached
            :lifecycle lifecycle-value})

          (and (= :stopped lifecycle-value)
               core-started?)
          (conj
           {:invariant :stopped-core-not-started
            :lifecycle lifecycle-value})

          (and (= :stopped lifecycle-value)
               (not shell-closed?))
          (conj
           {:invariant :stopped-shell-closed
            :lifecycle lifecycle-value})

          (and (= :stopped lifecycle-value)
               (seq choreo-attached-effects))
          (conj
           {:invariant :stopped-choreo-detached
            :actual choreo-attached-effects})

          (and optimistic-runtime
               (= :stopped lifecycle-value)
               (or (seq optimistic-attached-effects)
                   (seq optimistic-attached-actions)))
          (conj
           {:invariant :stopped-optimistic-detached
            :attached-effect-kinds optimistic-attached-effects
            :attached-local-actions optimistic-attached-actions})

          (and optimistic-htmx-runtime
               (= :stopped lifecycle-value)
               optimistic-htmx-attached?)
          (conj
           {:invariant :stopped-optimistic-htmx-detached}))]

    (into
     base-errors
     (concat active-choreo-ownership-errors
             active-optimistic-ownership-errors
             active-optimistic-htmx-ownership-errors))))

(defn invariant-clean?
  [runtime]
  (empty?
   (invariant-errors runtime)))

(defn- require-clean-composition!
  [runtime operation]
  (let [errors
        (invariant-errors runtime)]
    (when
     (seq errors)
      (throw
       (runtime-error
        :invalid-composition
        "Gesso Live browser composition invariants are violated."
        {:operation operation
         :invariant-errors errors}))))
  runtime)

;; =============================================================================
;; Runtime construction
;; =============================================================================

(defn create
  "Create one composed browser runtime without installing document listeners.

   Options:

     :core-options
       Passed unchanged to gesso.live.browser.core/create.

     :choreo-options
       Passed unchanged to gesso.live.browser.choreo/create.

     :optimistic-options
       Optional. When present, passed to gesso.live.browser.optimistic/create.
       This is the application realization seam for provisional projection,
       provisional rendering, and canonical-authority refresh.

     :optimistic-htmx-options
       Optional. When present, :optimistic-options must also be present. Passed
       to gesso.live.browser.optimistic-htmx/create. This owns only the HTMX
       command/settlement carrier and preverified plan lookup seam.

   Construction order is deliberate:

     1. Core creates continuity plus the single shell/AdapterState owner;
     2. Choreo attaches physical machine handlers to that shell;
     3. optional optimism attaches its physical handlers/local actions to that
        exact Choreo runtime and shell;
     4. optional optimistic HTMX wraps Choreo transport selection and registers
        observers through Core's existing listener seam.

   If a later stage fails, already-created stages are retired/detached in reverse
   dependency order before the original construction error is rethrown."
  ([]
   (create nil))
  ([options]
   (let [options
         (or options {})

         _
         (require-map!
          "Browser runtime options"
          options)

         _
         (check-option-keys!
          options)

         core-options
         (or (:core-options options) {})

         choreo-options
         (or (:choreo-options options) {})

         optimistic-options
         (:optimistic-options options)

         optimistic-htmx-options
         (:optimistic-htmx-options options)

         _
         (require-map!
          "Browser runtime :core-options"
          core-options)

         _
         (require-map!
          "Browser runtime :choreo-options"
          choreo-options)

         _
         (when (some? optimistic-options)
           (require-map!
            "Browser runtime :optimistic-options"
            optimistic-options))

         _
         (when (some? optimistic-htmx-options)
           (require-map!
            "Browser runtime :optimistic-htmx-options"
            optimistic-htmx-options))

         _
         (when (and (some? optimistic-htmx-options)
                    (nil? optimistic-options))
           (throw
            (runtime-error
             :optimistic-htmx-requires-optimism
             "Browser runtime :optimistic-htmx-options requires :optimistic-options."
             {})))

         core-runtime
         (core/create core-options)

         choreo-runtime*
         (atom nil)

         optimistic-runtime*
         (atom nil)

         optimistic-htmx-runtime*
         (atom nil)]

     (try
       (let [choreo-runtime
             (choreo/create
              (core/shell-runtime core-runtime)
              choreo-options)

             _
             (reset! choreo-runtime* choreo-runtime)

             optimistic-runtime
             (when (some? optimistic-options)
               (optimistic/create
                choreo-runtime
                optimistic-options))

             _
             (reset! optimistic-runtime* optimistic-runtime)

             optimistic-htmx-runtime
             (when (some? optimistic-htmx-options)
               (optimistic-htmx/create
                core-runtime
                optimistic-runtime
                optimistic-htmx-options))

             _
             (reset! optimistic-htmx-runtime* optimistic-htmx-runtime)

             runtime
             {:gesso.live.browser.runtime/type runtime-type
              :gesso.live.browser.runtime/version runtime-version
              :core core-runtime
              :choreo choreo-runtime
              :optimistic optimistic-runtime
              :optimistic-htmx optimistic-htmx-runtime
              :lifecycle (atom :created)
              :public-api (atom nil)}]

         (require-clean-composition!
          runtime
          :create)

         runtime)

       (catch :default error
         (when-let [optimistic-htmx-runtime @optimistic-htmx-runtime*]
           (try
             (optimistic-htmx/detach! optimistic-htmx-runtime)
             (catch :default _
               nil)))

         (when-let [optimistic-runtime @optimistic-runtime*]
           (try
             (optimistic/detach! optimistic-runtime)
             (catch :default _
               nil)))

         (when-let [choreo-runtime @choreo-runtime*]
           (try
             (choreo/detach! choreo-runtime)
             (catch :default _
               nil)))

         (try
           (core/stop! core-runtime)
           (catch :default _
             nil))

         (throw error))))))

;; =============================================================================
;; Lifecycle
;; =============================================================================

(defn- best-effort-detach-optimistic-htmx!
  [runtime]
  (when-let [bridge-runtime (:optimistic-htmx runtime)]
    (try
      (optimistic-htmx/detach! bridge-runtime)
      (catch :default _
        nil))))

(defn- best-effort-detach-optimistic!
  [runtime]
  (when-let [optimistic-runtime (:optimistic runtime)]
    (try
      (optimistic/detach! optimistic-runtime)
      (catch :default _
        nil))))

(defn- best-effort-detach-choreo!
  [runtime]
  (try
    (choreo/detach!
     (:choreo runtime))
    (catch :default _
      nil)))

(defn- best-effort-stop-core!
  [runtime]
  (try
    (core/stop!
     (:core runtime))
    (catch :default _
      nil)))

(defn- retire-broken-composition!
  [runtime]
  ;; Invalidate composition ownership first. Physical cleanup is deliberately
  ;; best effort and is not a prerequisite for preventing later restart.
  (reset!
   (:lifecycle runtime)
   :stopped)
  ;; Semantic shell shutdown must happen while all physical integration layers
  ;; remain attached. Detachment happens only after semantic ownership is gone.
  (best-effort-stop-core! runtime)
  (best-effort-detach-optimistic-htmx! runtime)
  (best-effort-detach-optimistic! runtime)
  (best-effort-detach-choreo! runtime)
  :stopped)

(defn start!
  "Install the composed runtime's document/HTMX listeners exactly once.

   Choreo, optional optimism, and optional optimistic HTMX physical bindings
   are already attached during create. Starting does not create another shell or
   AdapterState.

   Before acquiring document listeners, the runtime verifies the full created
   composition: one open shared shell plus exact Choreo, optimistic, and bridge
   handler/observer/wrapper ownership.

   A failed start semantically retires/shuts down the shell before detaching the
   bridge, optimism, and Choreo physical bindings. The partially-started runtime
   becomes permanently :stopped."
  [runtime]
  (let [runtime
        (require-runtime!
         runtime)

        lifecycle*
        (:lifecycle runtime)]

    (case @lifecycle*
      :started
      runtime

      :stopped
      (throw
       (runtime-error
        :already-stopped
        "A stopped Gesso Live browser runtime cannot be restarted."
        {}))

      :created
      (try
        (require-clean-composition!
         runtime
         :start)

        (core/start!
         (:core runtime))

        (reset!
         lifecycle*
         :started)

        (require-clean-composition!
         runtime
         :started)

        runtime

        (catch :default error
          (retire-broken-composition!
           runtime)
          (throw error)))

      (throw
       (runtime-error
        :invalid-lifecycle
        "Browser runtime contains an unknown lifecycle state."
        {:lifecycle
         @lifecycle*
         :allowed
         lifecycle-states})))))

(defn stop!
  "Stop the composed runtime idempotently.

   Ordering is intentional:

     1. mark the composition stopped so no reentrant caller can restart it;
     2. core/stop! removes listeners and shell/shutdown! semantically retires
        adapter-owned executions/fragments while all physical handlers remain;
     3. detach the optimistic HTMX observers/transport wrappers;
     4. detach optimism's physical effect handlers and Choreo local actions;
     5. detach Choreo's physical machine handlers last.

   Semantic retirement must precede physical detachment. Cleanup failures remain
   best-effort browser debt and cannot restore semantic ownership."
  [runtime]
  (let [runtime
        (require-runtime! runtime)

        lifecycle*
        (:lifecycle runtime)

        previous
        @lifecycle*]

    (when-not (= :stopped previous)
      (reset! lifecycle* :stopped)

      (try
        (core/stop! (:core runtime))
        (finally
          (best-effort-detach-optimistic-htmx! runtime)
          (best-effort-detach-optimistic! runtime)
          (best-effort-detach-choreo! runtime))))

    :stopped))

;; =============================================================================
;; Public composed operations
;; =============================================================================

(defn notify-fragment!
  "Submit one Live invalidation to the core sharing this runtime's adapter."
  ([runtime fragment-id]
   (core/notify-fragment!
    (core-runtime runtime)
    fragment-id))
  ([runtime fragment-id requirement]
   (core/notify-fragment!
    (core-runtime runtime)
    fragment-id
    requirement)))

(defn diagnostics
  "Return host-resource-free, read-only diagnostics for the composition.

   Diagnostic observation does not participate in adapter transitions. The
   result intentionally contains no DOM nodes, XHRs, timers, captured continuity
   values, provisional snapshots, transport handles, or registered functions."
  [runtime]
  (let [runtime
        (require-runtime! runtime)]
    {:gesso.live.browser.runtime/type runtime-type
     :gesso.live.browser.runtime/version runtime-version
     :lifecycle @(:lifecycle runtime)
     :invariant-errors (invariant-errors runtime)
     :core (core/diagnostics (:core runtime))
     :choreo (choreo/diagnostics (:choreo runtime))
     :optimistic
     (when-let [optimistic-runtime (:optimistic runtime)]
       (optimistic/diagnostics optimistic-runtime))
     :optimistic-htmx
     (when-let [bridge-runtime (:optimistic-htmx runtime)]
       (optimistic-htmx/diagnostics bridge-runtime))}))

;; =============================================================================
;; Browser-global production entry point
;; =============================================================================

(defonce ^:private default-runtime*
  (atom nil))

(defn default-runtime
  []
  @default-runtime*)

(declare shutdown!)

(defn- install-public-api!
  [runtime]
  (let [diagnostics-fn
        (fn []
          (clj->js
           (diagnostics runtime)))

        api
        #js
        {:version
         runtime-version

         ;; Preserve the existing small public spelling while making its actual
         ;; diagnostic nature explicit through the additional alias below.
         :state
         diagnostics-fn

         :diagnostics
         diagnostics-fn

         :notifyFragment
         (fn
           ([fragment-id]
            (notify-fragment!
             runtime
             fragment-id))
           ([fragment-id requirement]
            (notify-fragment!
             runtime
             fragment-id
             (js->clj
              requirement
              :keywordize-keys true))))

         :shutdown
         (fn []
           (shutdown!))}]

    (reset!
     (:public-api runtime)
     api)

    (aset
     js/window
     "gessoLive"
     api)

    true))

(defn ^:export init!
  "Create and start the process-default composed browser runtime once.

   The namespace does not auto-initialize merely by being required. The
   generated browser artifact or embedding application must call init!
   explicitly."
  ([]
   (init! nil))
  ([options]
   (or
    @default-runtime*

    (let [runtime
          (create options)]

      (if
       (compare-and-set!
        default-runtime*
        nil
        runtime)

        (try
          (start!
           runtime)

          (install-public-api!
           runtime)

          runtime

          (catch :default error
            (reset!
             default-runtime*
             nil)

            (try
              (stop!
               runtime)
              (catch :default _
                nil))

            (throw error)))

        (do
          ;; Another initializer won. This newly-created composition owns its
          ;; own shell/Choreo/optional-optimism/optional-bridge attachment and must be torn down.
          (stop!
           runtime)

          @default-runtime*))))))

(defn ^:export shutdown!
  "Stop and forget the process-default runtime.

   The public window.gessoLive object is removed only when it is still the exact
   API object installed by this runtime; unrelated replacement by application
   code is not overwritten. Public API removal is performed even if shutdown
   unexpectedly reports an error, because the composition is already logically
   retired and must not leave a callable stale handle behind."
  []
  (when-let [runtime
             @default-runtime*]

    (reset!
     default-runtime*
     nil)

    (let [installed-api
          @(:public-api runtime)]

      (try
        (stop!
         runtime)

        (finally
          (let [current-api
                (aget
                 js/window
                 "gessoLive")]

            (reset!
             (:public-api runtime)
             nil)

            (when
             (and installed-api
                  (identical?
                   installed-api
                   current-api))
              (js-delete
               js/window
               "gessoLive")))))))

  :stopped)
