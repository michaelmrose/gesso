(ns gesso.live.browser.runtime
  "Top-level composition root for the Gesso Live browser runtime.

   This namespace owns composition and lifecycle only. It does not introduce a
   second semantic runtime.

   One composed runtime contains:

     gesso.live.browser.core
       HTMX/document lifecycle normalization and one shared shell

     gesso.live.browser.continuity
       the per-core physical continuity implementation

     gesso.live.browser.choreo
       physical realization of portable Choreo effects attached to that exact
       same shell

   AdapterState remains owned exclusively by gesso.live.browser.shell through
   gesso.live.browser.adapter. This namespace never advances portable Choreo,
   interprets authoritative bases, manages timers, or stores browser resources.

   Optimism is intentionally absent from this composition root for now. The
   gesso.live.optimistic rewrite will later attach policy/effects to this
   already-stable browser composition rather than rebuilding browser ownership."
  (:require
   [gesso.live.browser.choreo :as choreo]
   [gesso.live.browser.core :as core]
   [gesso.live.browser.shell :as shell]))

;; =============================================================================
;; Identity / options
;; =============================================================================

(def runtime-version
  "1.1.0-dev")

(def runtime-type
  :gesso.live.browser.runtime/runtime)

(def option-keys
  #{:core-options
    :choreo-options})

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
   (identical?
    (core/shell-runtime (:core value))
    (choreo/shell-runtime (:choreo value)))
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

(defn invariant-errors
  "Return read-only composition invariant violations.

   These checks deliberately observe only lifecycle/configuration facts. They do
   not participate in adapter semantics and therefore cannot alter the behavior
   of the executable browser state machine.

   The expected lifecycle shapes are:

     :created
       core listeners absent, shell open, Choreo handlers attached and owned

     :started
       core listeners installed, shell open, Choreo handlers attached and owned

     :stopped
       core listeners absent, shell closed, Choreo handlers detached

   For active compositions, handler attachment means more than matching effect
   keys: every Choreo-owned shell slot must still contain the exact function
   installed by this Choreo runtime. This catches direct unregister/replacement
   that Choreo bookkeeping alone cannot observe. No handler functions are ever
   returned in diagnostics.

   The one-shared-shell check is repeated here even though runtime? structurally
   requires it because diagnostics should explain composition damage without
   requiring callers to infer it from nested child diagnostics."
  [runtime]
  (let [runtime
        (require-runtime! runtime)

        core-runtime
        (:core runtime)

        choreo-runtime
        (:choreo runtime)

        core-shell
        (core/shell-runtime core-runtime)

        choreo-shell
        (choreo/shell-runtime choreo-runtime)

        lifecycle-value
        @(:lifecycle runtime)

        core-diagnostics
        (core/diagnostics core-runtime)

        choreo-diagnostics
        (choreo/diagnostics choreo-runtime)

        core-started?
        (true? (:started? core-diagnostics))

        shell-closed?
        (shell/closed? core-shell)

        attached-effect-kinds
        (:attached-effect-kinds choreo-diagnostics)

        expected-attached
        choreo/owned-effect-kinds

        active-handler-ownership-errors
        (when
         (contains? #{:created :started} lifecycle-value)
          (choreo-handler-ownership-errors
           choreo-runtime
           core-shell))

        base-errors
        (cond-> []
      (not
       (identical?
        core-shell
        choreo-shell))
      (conj
       {:invariant :one-shared-shell
        :message "Core and Choreo must share the exact same browser shell."})

      (not
       (contains?
        lifecycle-states
        lifecycle-value))
      (conj
       {:invariant :known-lifecycle
        :lifecycle lifecycle-value
        :allowed lifecycle-states})

      (and
       (= :created lifecycle-value)
       core-started?)
      (conj
       {:invariant :created-core-not-started
        :lifecycle lifecycle-value})

      (and
       (= :created lifecycle-value)
       shell-closed?)
      (conj
       {:invariant :created-shell-open
        :lifecycle lifecycle-value})

      (and
       (= :created lifecycle-value)
       (not=
        expected-attached
        attached-effect-kinds))
      (conj
       {:invariant :created-choreo-attached
        :expected expected-attached
        :actual attached-effect-kinds})

      (and
       (= :started lifecycle-value)
       (not core-started?))
      (conj
       {:invariant :started-core-started
        :lifecycle lifecycle-value})

      (and
       (= :started lifecycle-value)
       shell-closed?)
      (conj
       {:invariant :started-shell-open
        :lifecycle lifecycle-value})

      (and
       (= :started lifecycle-value)
       (not=
        expected-attached
        attached-effect-kinds))
      (conj
       {:invariant :started-choreo-attached
        :expected expected-attached
        :actual attached-effect-kinds})

      (and
       (= :stopped lifecycle-value)
       core-started?)
      (conj
       {:invariant :stopped-core-not-started
        :lifecycle lifecycle-value})

      (and
       (= :stopped lifecycle-value)
       (not shell-closed?))
      (conj
       {:invariant :stopped-shell-closed
        :lifecycle lifecycle-value})

      (and
       (= :stopped lifecycle-value)
       (seq attached-effect-kinds))
      (conj
       {:invariant :stopped-choreo-detached
        :actual attached-effect-kinds}))]

    (into
     base-errors
     active-handler-ownership-errors)))

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

   Construction order is deliberate:

     1. core creates continuity plus the single shell/AdapterState owner;
     2. Choreo attaches its physical effect handlers to that existing shell.

   If Choreo attachment or composition validation fails, the newly created core
   shell is shut down before the construction error is rethrown."
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
         (or
          (:core-options options)
          {})

         choreo-options
         (or
          (:choreo-options options)
          {})

         _
         (require-map!
          "Browser runtime :core-options"
          core-options)

         _
         (require-map!
          "Browser runtime :choreo-options"
          choreo-options)

         core-runtime
         (core/create
          core-options)]

     (try
       (let [choreo-runtime
             (choreo/create
              (core/shell-runtime core-runtime)
              choreo-options)

             runtime
             {:gesso.live.browser.runtime/type
              runtime-type

              :gesso.live.browser.runtime/version
              runtime-version

              :core
              core-runtime

              :choreo
              choreo-runtime

              :lifecycle
              (atom :created)

              :public-api
              (atom nil)}]

         (require-clean-composition!
          runtime
          :create)

         runtime)

       (catch :default error
         (try
           (core/stop!
            core-runtime)
           (catch :default _
             nil))
         (throw error))))))

;; =============================================================================
;; Lifecycle
;; =============================================================================

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
  (best-effort-stop-core! runtime)
  (best-effort-detach-choreo! runtime)
  :stopped)

(defn start!
  "Install the composed runtime's document/HTMX listeners exactly once.

   Choreo physical handlers are already attached during create. Starting does
   not create another shell or AdapterState.

   Before acquiring document listeners, the runtime verifies that the created
   composition still has one open shared shell and its Choreo handlers attached.
   This prevents direct manipulation of a child runtime from producing a zombie
   composition that merely looks started at the top level.

   A failed core start retires/shuts down the shell and detaches Choreo before
   propagating the error. Such a partially-started runtime becomes :stopped and
   cannot be restarted."
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
        adapter-owned executions/fragments before physical cleanup;
     3. only after semantic shutdown do we detach Choreo's physical handlers.

   Detaching first would risk removing physical effect handlers while semantic
   retirement is still in progress. If core shutdown reports an error, Choreo
   detachment still runs in finally and the runtime remains permanently stopped."
  [runtime]
  (let [runtime
        (require-runtime!
         runtime)

        lifecycle*
        (:lifecycle runtime)

        previous
        @lifecycle*]

    (when-not
     (= :stopped previous)
      (reset!
       lifecycle*
       :stopped)

      (try
        (core/stop!
         (:core runtime))
        (finally
          (best-effort-detach-choreo!
           runtime))))

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
   values, or transport handles."
  [runtime]
  (let [runtime
        (require-runtime!
         runtime)]
    {:gesso.live.browser.runtime/type
     runtime-type

     :gesso.live.browser.runtime/version
     runtime-version

     :lifecycle
     @(:lifecycle runtime)

     :invariant-errors
     (invariant-errors runtime)

     :core
     (core/diagnostics
      (:core runtime))

     :choreo
     (choreo/diagnostics
      (:choreo runtime))}))

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
          ;; Another initializer won. This newly-created composition owns a
          ;; shell/Choreo attachment of its own and must be torn down.
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
