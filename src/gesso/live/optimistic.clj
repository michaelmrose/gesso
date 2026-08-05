(ns gesso.live.optimistic
  "Server-side API and wire protocol for Gesso Live optimistic transitions.

   This namespace owns the JVM-facing half of optimistic execution:

   - transition descriptor validation and normalization
   - server-rendered optimistic projection templates
   - browser protocol attributes
   - stable semantic scope and revision metadata
   - explicit semantic settlement markers

   It deliberately does not own:

   - application/domain transition policy
   - HTMX request controls or anti-forgery markup
   - browser execution state
   - DOM capture, projection, rollback, or reconciliation
   - authoritative rendering

   gesso.live.ui composes transition descriptors with HTMX controls.
   gesso.live.optimistic-machine owns the shared execution semantics.
   gesso.live.runtime owns irreducibly browser-specific effects."
  (:require
   [clojure.string :as str]
   [gesso.live.htmx :as htmx])
  (:import
   [java.util UUID]))

;; -----------------------------------------------------------------------------
;; Protocol identity
;; -----------------------------------------------------------------------------

(def protocol-version
  "Wire protocol version emitted by this namespace and consumed by the browser
   runtime. Increment this when markup or settlement semantics become
   incompatible."
  "1")

(def descriptor-type
  :gesso.live.optimistic/descriptor)

(def settlement-type
  :gesso.live.optimistic/settlement)

(def default-machine
  :gesso.live.optimistic/default)

(def default-template-prefix
  "gesso-optimistic-template-")

(def default-sync-strategy
  "drop")

(def default-projection-mode
  :provisional)

(def projection-modes
  "Presentation strength advertised to application CSS and diagnostics.

   :pending
     Immediate pending presentation without claiming the expected final result.

   :provisional
     Likely result rendered with visible pending/uncertain presentation.

   :full
     Final-looking speculative presentation. Use only when the expected outcome
     is sufficiently deterministic."
  #{:pending :provisional :full})

(def settlement-outcomes
  "Generic semantic outcomes understood by Gesso.

   :confirmed
     The command was applied and canonical state materially confirms the
     projection.

   :reconciled
     The command was applied, but canonical state differs because of concurrent
     activity or application policy.

   :rejected
     The application deliberately did not apply the command.

   :failed
     The server produced an explicit infrastructure/application failure result.
     Network failures that produce no response are handled by the browser
     runtime rather than by a settlement marker."
  #{:confirmed :reconciled :rejected :failed})

;; -----------------------------------------------------------------------------
;; Browser protocol names
;; -----------------------------------------------------------------------------

(def protocol-attr
  :data-gesso-optimistic-protocol)

(def machine-attr
  :data-gesso-optimistic-machine)

(def transition-attr
  :data-gesso-optimistic-transition)

(def template-attr
  :data-gesso-optimistic-template)

(def target-attr
  :data-gesso-optimistic-target)

(def scope-attr
  :data-gesso-optimistic-scope)

(def base-revision-attr
  :data-gesso-optimistic-base-revision)

(def revision-attr
  :data-gesso-optimistic-revision)

(def pending-label-attr
  :data-gesso-optimistic-label)

(def projection-mode-attr
  :data-gesso-optimistic-mode)

(def settlement-attr
  :data-gesso-optimistic-settlement)

(def execution-attr
  :data-gesso-optimistic-execution)

(def outcome-attr
  :data-gesso-optimistic-outcome)

(def command-applied-attr
  :data-gesso-optimistic-command-applied)

(def reason-attr
  :data-gesso-optimistic-reason)

(def execution-request-header
  "Lower-case Ring request header used to correlate an HTMX command with its
   browser-side optimistic execution. The browser sends the corresponding HTTP
   header `Gesso-Optimistic-Execution`."
  "gesso-optimistic-execution")

(def ^:private reserved-protocol-attrs
  [protocol-attr
   machine-attr
   transition-attr
   template-attr
   target-attr
   scope-attr
   base-revision-attr
   revision-attr
   pending-label-attr
   projection-mode-attr
   settlement-attr
   execution-attr
   outcome-attr
   command-applied-attr
   reason-attr])

;; -----------------------------------------------------------------------------
;; Validation and normalization
;; -----------------------------------------------------------------------------

(defn- ex
  [message data]
  (ex-info message data))

(defn- blank-string?
  [x]
  (and (string? x)
       (str/blank? x)))

(defn- present?
  [x]
  (not (or (nil? x)
           (blank-string? x))))

(defn- require-present!
  [k value]
  (when-not (present? value)
    (throw
     (ex (str "gesso.live optimistic config requires " k ".")
         {k value})))
  value)

(defn- require-non-blank-string!
  [k value]
  (when-not (and (string? value)
                 (not (str/blank? value)))
    (throw
     (ex (str "gesso.live optimistic " k " must be a non-blank string.")
         {k value})))
  value)

(defn- require-map!
  [k value]
  (when-not (map? value)
    (throw
     (ex (str "gesso.live optimistic " k " must be a map.")
         {k value})))
  value)

(defn- qualified-name
  [x]
  (cond
    (keyword? x)
    (if-some [namespace' (namespace x)]
      (str namespace' "/" (name x))
      (name x))

    (symbol? x)
    (str x)

    (nil? x)
    nil

    :else
    (str x)))

(defn- normalize-name
  [k value]
  (let [value' (qualified-name value)]
    (when-not (and value'
                   (not (str/blank? value')))
      (throw
       (ex (str "gesso.live optimistic " k " must not be blank.")
           {k value})))
    value'))

(defn- normalize-optional-name
  [k value]
  (when (some? value)
    (normalize-name k value)))

(defn- normalize-transition
  [transition]
  (normalize-name :transition transition))

(defn- normalize-pending-label
  [value]
  (when (some? value)
    (require-non-blank-string! :pending-label value)))

(defn- normalize-sync
  [sync]
  (cond
    (or (nil? sync)
        (false? sync))
    nil

    (and (string? sync)
         (not (str/blank? sync)))
    sync

    :else
    (throw
     (ex "gesso.live optimistic :sync must be nil, false, or a non-blank string."
         {:sync sync}))))

(defn- normalize-projection-mode
  [mode]
  (let [mode' (or mode default-projection-mode)]
    (when-not (contains? projection-modes mode')
      (throw
       (ex "gesso.live optimistic :projection-mode is invalid."
           {:projection-mode mode
            :allowed projection-modes})))
    mode'))

(defn- wire-scope
  [scope]
  (require-present! :scope scope)
  (let [scope' (if (string? scope)
                 scope
                 (pr-str scope))]
    (require-non-blank-string! :scope scope')))

(defn- normalize-revision
  [k revision]
  (when (some? revision)
    (cond
      (and (integer? revision)
           (not (neg? revision)))
      revision

      (and (string? revision)
           (not (str/blank? revision)))
      revision

      :else
      (throw
       (ex (str "gesso.live optimistic " k
                " must be a non-negative integer or non-blank string.")
           {k revision})))))

(defn- revision->wire
  [revision]
  (when (some? revision)
    (str revision)))

(defn- hiccup-tag?
  [x]
  (or (keyword? x)
      (symbol? x)
      (string? x)))

(defn- fragment-tag?
  [tag]
  (contains? #{:<> '<> "<>"} tag))

(defn- single-root-hiccup?
  [content]
  (and (vector? content)
       (hiccup-tag? (first content))
       (not (fragment-tag? (first content)))))

(defn- require-single-root!
  [content]
  (when-not (single-root-hiccup? content)
    (throw
     (ex (str "gesso.live optimistic :content must be one rooted Hiccup element "
              "and may not be a fragment or sequence.")
         {:content content})))
  content)

(defn- remove-protocol-attrs
  [attrs protocol-attrs]
  (apply dissoc attrs protocol-attrs))

;; -----------------------------------------------------------------------------
;; Projection descriptor construction
;; -----------------------------------------------------------------------------

(defn new-template-name
  "Return a fresh browser-safe optimistic template name."
  []
  (str default-template-prefix (UUID/randomUUID)))

(defn target-sync
  "Derive the default HTMX single-flight synchronization value for target.

   Examples:
     \"closest [data-card]\" => \"closest [data-card]:drop\"
     \"card-1\"              => \"#card-1:drop\""
  ([target]
   (target-sync target default-sync-strategy))
  ([target strategy]
   (let [target'   (->> target
                        (require-non-blank-string! :target)
                        htmx/normalize-target)
         strategy' (normalize-name :strategy strategy)]
     (str target' ":" strategy'))))

(defn ->optimistic
  "Create a prepared optimistic transition descriptor.

   Required:

     :transition
       Semantic transition identifier. Qualified keywords are preserved on the
       wire, e.g. :request/join becomes \"request/join\".

     :scope
       Stable semantic identity for the projected region or entity. Strings are
       emitted unchanged; other values are emitted with `pr-str` and treated by
       the browser as opaque identities.

     :target
       Selector for the existing element that the browser runtime should
       project into. Bare ids are normalized to CSS id selectors.

     :content
       Exactly one rooted Hiccup element. Its rendered root tag must match the
       existing target element's tag; the browser runtime validates this before
       installation.

   Optional:

     :machine
       Browser machine identifier. Defaults to
       :gesso.live.optimistic/default.

     :base-revision
       Canonical revision from which this projection was rendered. May be a
       non-negative integer or opaque non-blank string.

     :projection-mode
       One of :pending, :provisional, or :full. Defaults to :provisional.

     :template-name
       Explicit template identity. Normally generated automatically; useful for
       deterministic tests and diagnostics.

     :pending-label
       Immediate press/pending text such as \"Joining…\".

     :sync
       HTMX hx-sync value consumed by gesso.live.ui. When omitted, a
       target-scoped `...:drop` value is derived. Explicit nil/false disables the
       suggested sync value.

     :attrs
       Extra attrs for the HTMX request owner. Required protocol attrs always
       win over conflicting caller values.

     :template-attrs
       Extra attrs for the hidden <template>. Required protocol attrs always win
       over conflicting caller values."
  [{:keys [transition
           target
           scope
           base-revision
           content
           machine
           projection-mode
           template-name
           pending-label
           attrs
           template-attrs]
    :as opts}]
  (let [transition'      (normalize-transition transition)
        target'          (->> target
                              (require-non-blank-string! :target)
                              htmx/normalize-target)
        scope'           (wire-scope scope)
        base-revision'   (normalize-revision :base-revision base-revision)
        content'         (->> content
                              (require-present! :content)
                              require-single-root!)
        machine'         (normalize-name :machine (or machine default-machine))
        mode'            (normalize-projection-mode projection-mode)
        template-name'   (or (normalize-optional-name :template-name
                                                       template-name)
                             (new-template-name))
        pending-label'   (normalize-pending-label pending-label)
        attrs'           (require-map! :attrs (or attrs {}))
        template-attrs'  (require-map! :template-attrs (or template-attrs {}))
        sync'            (if (contains? opts :sync)
                           (normalize-sync (:sync opts))
                           (target-sync target'))]
    {:gesso.live.optimistic/type descriptor-type
     :protocol-version protocol-version
     :machine machine'
     :transition transition'
     :scope scope'
     :base-revision base-revision'
     :target target'
     :content content'
     :projection-mode mode'
     :template-name template-name'
     :pending-label pending-label'
     :sync sync'
     :attrs attrs'
     :template-attrs template-attrs'}))

(defn optimistic?
  [x]
  (and (map? x)
       (= descriptor-type
          (:gesso.live.optimistic/type x))))

(defn ensure-optimistic
  "Return a prepared descriptor unchanged, or normalize a raw options map."
  [optimistic]
  (if (optimistic? optimistic)
    optimistic
    (->optimistic optimistic)))

(defn- require-descriptor!
  [optimistic]
  (when-not (optimistic? optimistic)
    (throw
     (ex (str "gesso.live optimistic markup helpers require a prepared descriptor. "
              "Call ->optimistic once, or use render-parts with a raw options map.")
         {:optimistic optimistic})))
  optimistic)

;; -----------------------------------------------------------------------------
;; Canonical scope metadata
;; -----------------------------------------------------------------------------

(defn canonical-attrs
  "Return attrs for an authoritative rendered scope.

   The browser runtime uses these attrs to compare canonical replacements with
   pending optimistic executions. `scope` is required. `revision` is optional,
   but revision-aware reconciliation requires it."
  [{:keys [scope revision]}]
  (let [scope'    (wire-scope scope)
        revision' (normalize-revision :revision revision)]
    (htmx/clean-attrs
     {protocol-attr protocol-version
      scope-attr scope'
      revision-attr (revision->wire revision')})))

;; -----------------------------------------------------------------------------
;; Browser-facing projection markup
;; -----------------------------------------------------------------------------

(defn source-attrs
  "Build protocol attrs for the element that owns the HTMX request.

   `optimistic` must be a prepared descriptor so source attrs and template
   markup cannot accidentally receive different generated identities."
  [optimistic]
  (let [{:keys [protocol-version
                machine
                transition
                template-name
                target
                scope
                base-revision
                pending-label
                projection-mode
                attrs]} (require-descriptor! optimistic)
        attrs' (remove-protocol-attrs attrs reserved-protocol-attrs)]
    (htmx/merge-attrs
     attrs'
     {protocol-attr protocol-version
      machine-attr machine
      transition-attr transition
      template-attr template-name
      target-attr target
      scope-attr scope
      projection-mode-attr (name projection-mode)}
     (when (some? base-revision)
       {base-revision-attr (revision->wire base-revision)})
     (when pending-label
       {pending-label-attr pending-label}))))

(defn template
  "Render the hidden optimistic projection <template> for a prepared descriptor."
  [optimistic]
  (let [{:keys [protocol-version
                transition
                template-name
                scope
                projection-mode
                content
                template-attrs]} (require-descriptor! optimistic)
        template-attrs' (remove-protocol-attrs template-attrs
                                               reserved-protocol-attrs)]
    [:template
     (htmx/clean-attrs
      (merge
       template-attrs'
       {protocol-attr protocol-version
        transition-attr transition
        template-attr template-name
        scope-attr scope
        projection-mode-attr (name projection-mode)}))
     content]))

(defn render-parts
  "Return all pieces needed by a higher-level HTMX UI helper.

   :source-attrs
     Belongs on the actual HTMX request owner.

   :template
     Should be rendered near the action source or within the same stable scope.

   :sync
     Suggested hx-sync value.

   :optimistic
     Prepared descriptor for diagnostics or higher-level composition."
  [optimistic]
  (let [optimistic' (ensure-optimistic optimistic)]
    {:optimistic optimistic'
     :source-attrs (source-attrs optimistic')
     :template (template optimistic')
     :sync (:sync optimistic')}))

;; -----------------------------------------------------------------------------
;; Request execution identity
;; -----------------------------------------------------------------------------

(defn request-execution-id
  "Return the optimistic execution id supplied by the browser, when present.

   Supports the normal lower-case Ring header and explicit context injection for
   tests or custom adapters."
  [ctx]
  (or (:gesso.live.optimistic/execution-id ctx)
      (get-in ctx [:headers execution-request-header])
      (get-in ctx [:headers "Gesso-Optimistic-Execution"])
      (get-in ctx [:request :headers execution-request-header])
      (get-in ctx [:request :headers "Gesso-Optimistic-Execution"])))

;; -----------------------------------------------------------------------------
;; Explicit semantic settlement
;; -----------------------------------------------------------------------------

(defn- default-command-applied?
  [outcome]
  (contains? #{:confirmed :reconciled} outcome))

(defn ->settlement
  "Create a prepared explicit settlement descriptor.

   Required:

     :execution-id
       Browser-generated execution identity from `request-execution-id`.

     :transition
       Semantic transition identifier originally rendered on the action.

     :scope
       Stable semantic scope settled by this response.

     :outcome
       One of :confirmed, :reconciled, :rejected, or :failed.

   Optional:

     :revision
       Newest canonical revision represented by the response.

     :command-applied?
       Whether the application command took effect. Defaults to true for
       :confirmed/:reconciled and false for :rejected/:failed.

     :reason
       Opaque application reason identifier for diagnostics or application UI.
       Gesso does not interpret it.

   A semantic settlement should normally accompany authoritative rendered
   content for the affected scope, including rejection responses. Snapshot
   restoration is reserved for failures that produce no authoritative result."
  [{:keys [execution-id
           transition
           scope
           outcome
           revision
           command-applied?
           reason]
    :as opts}]
  (let [execution-id' (require-non-blank-string! :execution-id execution-id)
        transition'   (normalize-name :transition transition)
        scope'        (wire-scope scope)
        revision'     (normalize-revision :revision revision)
        reason'       (normalize-optional-name :reason reason)]
    (when-not (contains? settlement-outcomes outcome)
      (throw
       (ex "gesso.live optimistic settlement :outcome is invalid."
           {:outcome outcome
            :allowed settlement-outcomes})))
    (when (and (contains? opts :command-applied?)
               (not (instance? Boolean command-applied?)))
      (throw
       (ex "gesso.live optimistic settlement :command-applied? must be boolean."
           {:command-applied? command-applied?})))
    {:gesso.live.optimistic/type settlement-type
     :protocol-version protocol-version
     :execution-id execution-id'
     :transition transition'
     :scope scope'
     :outcome outcome
     :revision revision'
     :command-applied? (if (contains? opts :command-applied?)
                         command-applied?
                         (default-command-applied? outcome))
     :reason reason'}))

(defn settlement?
  [x]
  (and (map? x)
       (= settlement-type
          (:gesso.live.optimistic/type x))))

(defn ensure-settlement
  "Return a prepared settlement unchanged, or normalize a raw options map."
  [settlement]
  (if (settlement? settlement)
    settlement
    (->settlement settlement)))

(defn settlement-for-request
  "Create a settlement descriptor using the browser execution id from `ctx`."
  [ctx opts]
  (->settlement
   (assoc opts :execution-id (or (:execution-id opts)
                                 (request-execution-id ctx)))))

(defn settlement-marker
  "Render an inert <template> carrying explicit semantic settlement metadata.

   The browser runtime reads this marker from the HTMX response. It is not a DOM
   replacement and contains no application content."
  [settlement]
  (let [{:keys [protocol-version
                execution-id
                transition
                scope
                outcome
                revision
                command-applied?
                reason]} (ensure-settlement settlement)]
    [:template
     (htmx/clean-attrs
      {protocol-attr protocol-version
       settlement-attr "true"
       execution-attr execution-id
       transition-attr transition
       scope-attr scope
       outcome-attr (name outcome)
       revision-attr (revision->wire revision)
       command-applied-attr (if command-applied? "true" "false")
       reason-attr reason})]))

(defn with-settlement
  "Wrap authoritative response nodes with an explicit settlement marker first.

   The display-contents wrapper is suitable for responses that otherwise consist
   of OOB fragments and/or toast markup."
  [settlement & nodes]
  (into
   [:div {:style {:display "contents"}}]
   (cons (settlement-marker settlement)
         (remove nil? nodes))))
