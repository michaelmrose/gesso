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
   gesso.live.optimistic rewrite will later attach its policy/effects to this
   already-stable browser composition rather than rebuilding browser ownership."
  (:require
   [gesso.live.browser.choreo :as choreo]
   [gesso.live.browser.core :as core]))

;; =============================================================================
;; Identity / options
;; =============================================================================

(def runtime-version
  "1.0.0-dev")

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

   If Choreo attachment fails, the newly created core shell is shut down before
   the construction error is rethrown."
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
              choreo-options)]

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
          (atom nil)})

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

(defn start!
  "Install the composed runtime's document/HTMX listeners exactly once.

   Choreo physical handlers are already attached during create. Starting does
   not create another shell or AdapterState.

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
        (core/start!
         (:core runtime))

        (reset!
         lifecycle*
         :started)

        runtime

        (catch :default error
          ;; Make ownership invalid immediately before best-effort physical
          ;; teardown. No caller may retry a partially-started composition.
          (reset!
           lifecycle*
           :stopped)

          (try
            (core/stop!
             (:core runtime))
            (catch :default _
              nil))

          (try
            (choreo/detach!
             (:choreo runtime))
            (catch :default _
              nil))

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
   retirement is still in progress."
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
          (try
            (choreo/detach!
             (:choreo runtime))
            (catch :default _
              nil)))))

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
  "Return host-resource-free diagnostics for the composition and its children."
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
  (let [api
        #js
        {:version
         runtime-version

         :state
         (fn []
           (clj->js
            (diagnostics runtime)))

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
   code is not overwritten."
  []
  (when-let [runtime
             @default-runtime*]

    (reset!
     default-runtime*
     nil)

    (stop!
     runtime)

    (let [installed-api
          @(:public-api runtime)

          current-api
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
         "gessoLive"))))

  :stopped)
