(ns gesso.live.optimistic
  "JVM-facing rendering and request helpers for Gesso Live optimism.

   The optimistic lifecycle itself is not defined here. Its semantics live once
   in gesso.live.optimistic-choreo and are executed in the browser by the
   projected choreography machine.

   This namespace owns the server/browser wire edge:

   - validating and preparing optimistic render descriptors
   - emitting protocol-v2 source attributes
   - rendering one-root optimistic projection templates
   - marking authoritative canonical Hiccup explicitly
   - reading the browser-generated execution id from Ring context
   - constructing semantic settlement metadata
   - rendering a settlement marker together with canonical content

   Application/domain transition policy and authoritative rendering remain
   application concerns. Browser DOM/continuity/execution behavior remains in
   the ClojureScript runtime.

   Important protocol-v2 invariants enforced here:

   - scope and revision wire encoding come only from gesso.live.protocol
   - caller attrs cannot override framework protocol attrs
   - optimistic projections are never marked canonical
   - canonical authority is always explicit
   - settlement does not redundantly repeat :transition
   - :command-applied? is derived from settlement outcome rather than supplied
     independently
   - with-settlement marks its authoritative root from the settlement itself,
     so marker scope/revision and canonical scope/revision cannot disagree"
  (:require
   [clojure.string :as str]
   [gesso.live.htmx :as htmx]
   [gesso.live.protocol :as protocol])
  (:import
   [java.util UUID]))

;; -----------------------------------------------------------------------------
;; Public identities
;; -----------------------------------------------------------------------------

(def protocol-version
  protocol/optimistic-protocol-version)

(def descriptor-type
  :gesso.live.optimistic/descriptor)

(def settlement-type
  :gesso.live.optimistic/settlement)

(def default-template-prefix
  "gesso-optimistic-template-")

(def default-sync-strategy
  "drop")

(def default-projection-mode
  :provisional)

(def projection-modes
  protocol/projection-modes)

(def settlement-outcomes
  protocol/settlement-outcomes)

(def execution-request-header
  protocol/optimistic-execution-header-name)

;; Public aliases keep JVM callers from spelling protocol attrs themselves while
;; ensuring the actual vocabulary is owned by gesso.live.protocol.
(def protocol-attr
  protocol/optimistic-protocol-attr)

(def transition-attr
  protocol/optimistic-transition-attr)

(def template-attr
  protocol/optimistic-template-attr)

(def target-attr
  protocol/optimistic-target-attr)

(def scope-attr
  protocol/optimistic-scope-attr)

(def base-revision-attr
  protocol/optimistic-base-revision-attr)

(def revision-attr
  protocol/optimistic-revision-attr)

(def pending-label-attr
  protocol/optimistic-pending-label-attr)

(def projection-mode-attr
  protocol/optimistic-projection-mode-attr)

(def settlement-attr
  protocol/optimistic-settlement-attr)

(def execution-attr
  protocol/optimistic-execution-attr)

(def outcome-attr
  protocol/optimistic-outcome-attr)

(def command-applied-attr
  protocol/optimistic-command-applied-attr)

(def reason-attr
  protocol/optimistic-reason-attr)

(def canonical-attr
  protocol/optimistic-canonical-attr)

;; -----------------------------------------------------------------------------
;; Validation helpers
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
  (not
   (or (nil? x)
       (blank-string? x))))

(defn- require-present!
  [k value]
  (when-not (present? value)
    (throw
     (ex
      (str "gesso.live optimistic config requires " k ".")
      {k value})))
  value)

(defn- require-non-blank-string!
  [k value]
  (when-not
      (and (string? value)
           (not (str/blank? value)))
    (throw
     (ex
      (str "gesso.live optimistic " k
           " must be a non-blank string.")
      {k value})))
  value)

(defn- require-map!
  [k value]
  (when-not (map? value)
    (throw
     (ex
      (str "gesso.live optimistic " k
           " must be a map.")
      {k value})))
  value)

(defn- normalize-scope
  "Validate semantic scope without converting the prepared descriptor to wire
   representation.

   Keeping prepared data semantic avoids accidental double encoding when the
   same descriptor later feeds source attrs, templates, canonical attrs, or
   settlement rendering."
  [scope]
  (protocol/wire-scope scope)
  scope)

(defn- normalize-transition
  [transition]
  (protocol/normalize-name
   :transition
   transition))

(defn- normalize-pending-label
  [value]
  (when (some? value)
    (require-non-blank-string!
     :pending-label
     value)))

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
     (ex
      "gesso.live optimistic :sync must be nil, false, or a non-blank string."
      {:sync sync}))))

(defn- normalize-projection-mode
  [mode]
  (let [mode'
        (or mode
            default-projection-mode)]
    (when-not
        (contains?
         projection-modes
         mode')
      (throw
       (ex
        "gesso.live optimistic :projection-mode is invalid."
        {:projection-mode mode
         :allowed projection-modes})))
    mode'))

(defn- hiccup-tag?
  [x]
  (or (keyword? x)
      (symbol? x)
      (string? x)))

(defn- fragment-tag?
  [tag]
  (contains?
   #{:<> '<> "<>"}
   tag))

(defn- single-root-hiccup?
  [content]
  (and (vector? content)
       (hiccup-tag?
        (first content))
       (not
        (fragment-tag?
         (first content)))))

(defn- require-single-root!
  [k content]
  (when-not
      (single-root-hiccup?
       content)
    (throw
     (ex
      (str "gesso.live optimistic " k
           " must be one rooted Hiccup element and may not be a fragment or sequence.")
      {k content})))
  content)

(defn- remove-protocol-attrs
  [attrs]
  (apply
   dissoc
   attrs
   protocol/reserved-optimistic-attrs))

(defn- hiccup-parts
  "Return [tag attrs children] for one rooted Hiccup element."
  [node]
  (require-single-root!
   :node
   node)
  (let [[tag second-item & rest-items]
        node]
    (if (map? second-item)
      [tag second-item rest-items]
      [tag
       {}
       (if (nil? second-item)
         rest-items
         (cons second-item
               rest-items))])))

(defn- with-root-attrs
  "Merge attrs into one rooted Hiccup node with attrs taking precedence."
  [node attrs]
  (let [[tag existing children]
        (hiccup-parts node)]
    (into
     [tag
      (merge existing attrs)]
     children)))

;; -----------------------------------------------------------------------------
;; Projection descriptor construction
;; -----------------------------------------------------------------------------

(defn new-template-name
  "Return a fresh browser-safe optimistic template name."
  []
  (str
   default-template-prefix
   (UUID/randomUUID)))

(defn target-sync
  "Derive the default target-scoped HTMX single-flight synchronization value.

   Examples:

     \"closest [data-card]\" => \"closest [data-card]:drop\"
     \"card-1\"              => \"#card-1:drop\""
  ([target]
   (target-sync
    target
    default-sync-strategy))
  ([target strategy]
   (let [target'
         (->> target
              (require-non-blank-string!
               :target)
              htmx/normalize-target)
         strategy'
         (protocol/normalize-name
          :strategy
          strategy)]
     (str target'
          ":"
          strategy'))))

(defn ->optimistic
  "Prepare one optimistic transition descriptor.

   Required:

     :transition
       Semantic transition identifier. Qualified keywords retain their
       namespace on the browser wire.

     :scope
       Stable semantic identity of the authoritative region/entity. It remains
       semantic data in this descriptor and is encoded only when markup is
       emitted.

     :target
       Selector for the existing element receiving the projection. Bare ids are
       normalized to CSS id selectors.

     :content
       Exactly one rooted Hiccup element. The browser requires its root tag and
       explicit id (when present) to be compatible with the existing target.

   Optional:

     :base-revision
       Canonical revision the projection was rendered from. Supported values are
       JavaScript-safe non-negative integers or opaque non-blank strings.

     :projection-mode
       :pending, :provisional, or :full. Defaults to :provisional.

     :template-name
       Explicit template identity. Normally generated.

     :pending-label
       Immediate pending text such as \"Joining…\".

     :sync
       Suggested hx-sync value consumed by gesso.live.ui. Omitted means
       target-scoped :drop. Explicit nil/false disables the suggestion.

     :attrs
       Additional attrs for the HTMX request owner.

     :template-attrs
       Additional attrs for the hidden <template>.

   Framework protocol attrs always override conflicting attrs supplied by the
   caller."
  [{:keys [transition
           target
           scope
           base-revision
           content
           projection-mode
           template-name
           pending-label
           attrs
           template-attrs]
    :as opts}]
  (let [transition'
        (normalize-transition
         transition)
        target'
        (->> target
             (require-non-blank-string!
              :target)
             htmx/normalize-target)
        scope'
        (normalize-scope
         scope)
        base-revision'
        (protocol/normalize-revision
         :base-revision
         base-revision)
        content'
        (->> content
             (require-present!
              :content)
             (require-single-root!
              :content))
        mode'
        (normalize-projection-mode
         projection-mode)
        template-name'
        (or
         (protocol/normalize-optional-name
          :template-name
          template-name)
         (new-template-name))
        pending-label'
        (normalize-pending-label
         pending-label)
        attrs'
        (require-map!
         :attrs
         (or attrs {}))
        template-attrs'
        (require-map!
         :template-attrs
         (or template-attrs {}))
        sync'
        (if (contains? opts
                       :sync)
          (normalize-sync
           (:sync opts))
          (target-sync
           target'))]
    {:gesso.live.optimistic/type
     descriptor-type
     :protocol-version
     protocol-version
     :transition
     transition'
     :scope
     scope'
     :base-revision
     base-revision'
     :target
     target'
     :content
     content'
     :projection-mode
     mode'
     :template-name
     template-name'
     :pending-label
     pending-label'
     :sync
     sync'
     :attrs
     attrs'
     :template-attrs
     template-attrs'}))

(defn optimistic?
  [x]
  (and
   (map? x)
   (= descriptor-type
      (:gesso.live.optimistic/type
       x))
   (= protocol-version
      (:protocol-version x))))

(defn ensure-optimistic
  "Return a prepared protocol-v2 descriptor unchanged, or normalize raw opts."
  [optimistic]
  (if (optimistic? optimistic)
    optimistic
    (->optimistic
     optimistic)))

(defn- require-descriptor!
  [optimistic]
  (when-not
      (optimistic?
       optimistic)
    (throw
     (ex
      (str
       "gesso.live optimistic markup helpers require a prepared protocol-v2 "
       "descriptor. Call ->optimistic once, or use render-parts with raw opts.")
      {:optimistic optimistic})))
  optimistic)

;; -----------------------------------------------------------------------------
;; Canonical authority
;; -----------------------------------------------------------------------------

(defn canonical-attrs
  "Return attrs that explicitly mark authoritative server-rendered content.

   :scope is required.
   :revision is optional.

   A matching scope alone never implies authority. The explicit canonical marker
   is what permits browser reconciliation to treat the element as server truth."
  [{:keys [scope revision]}]
  (let [scope'
        (normalize-scope
         scope)
        revision'
        (protocol/normalize-revision
         :revision
         revision)]
    (htmx/clean-attrs
     {protocol-attr
      protocol-version

      scope-attr
      (protocol/wire-scope
       scope')

      revision-attr
      (protocol/revision->wire
       revision')

      canonical-attr
      "true"})))

(defn canonical
  "Mark one rooted Hiccup element as authoritative for scope/revision.

   Existing caller protocol attrs cannot disagree with the supplied semantic
   scope/revision because canonical attrs are merged last."
  [opts node]
  (let [node'
        (require-single-root!
         :canonical-content
         node)
        [tag attrs children]
        (hiccup-parts node')
        attrs'
        (merge
         (remove-protocol-attrs
          attrs)
         (canonical-attrs
          opts))]
    (into
     [tag attrs']
     children)))

;; -----------------------------------------------------------------------------
;; Browser-facing optimistic markup
;; -----------------------------------------------------------------------------

(defn source-attrs
  "Build protocol attrs for the actual HTMX request owner.

   A prepared descriptor is required so source attrs and the hidden template
   cannot accidentally receive different generated identities."
  [optimistic]
  (let [{:keys [transition
                template-name
                target
                scope
                base-revision
                pending-label
                projection-mode
                attrs]}
        (require-descriptor!
         optimistic)
        attrs'
        (remove-protocol-attrs
         attrs)]
    (htmx/merge-attrs
     attrs'
     {protocol-attr
      protocol-version

      transition-attr
      transition

      template-attr
      template-name

      target-attr
      target

      scope-attr
      (protocol/wire-scope
       scope)

      projection-mode-attr
      (name projection-mode)}
     (when (some? base-revision)
       {base-revision-attr
        (protocol/revision->wire
         base-revision)})
     (when pending-label
       {pending-label-attr
        pending-label}))))

(defn template
  "Render the hidden one-root optimistic projection template.

   The template carries identity/scope diagnostics but never canonical authority."
  [optimistic]
  (let [{:keys [transition
                template-name
                scope
                projection-mode
                content
                template-attrs]}
        (require-descriptor!
         optimistic)
        template-attrs'
        (remove-protocol-attrs
         template-attrs)]
    [:template
     (htmx/clean-attrs
      (merge
       template-attrs'
       {protocol-attr
        protocol-version

        transition-attr
        transition

        template-attr
        template-name

        scope-attr
        (protocol/wire-scope
         scope)

        projection-mode-attr
        (name projection-mode)}))
     content]))

(defn render-parts
  "Return pieces consumed by a higher-level HTMX UI helper.

   :source-attrs
     Belongs on the actual clicked/request-owning element.

   :template
     Hidden projection markup rendered near that source.

   :sync
     Suggested hx-sync value.

   :optimistic
     Prepared descriptor."
  [optimistic]
  (let [optimistic'
        (ensure-optimistic
         optimistic)]
    {:optimistic
     optimistic'
     :source-attrs
     (source-attrs
      optimistic')
     :template
     (template
      optimistic')
     :sync
     (:sync optimistic')}))

;; -----------------------------------------------------------------------------
;; Request execution identity
;; -----------------------------------------------------------------------------

(defn request-execution-id
  "Return the browser-generated optimistic execution id, when present.

   Normal Ring headers are lower-case. Explicit context injection remains
   supported for tests and custom adapters."
  [ctx]
  (or
   (:gesso.live.optimistic/execution-id
    ctx)
   (get-in
    ctx
    [:headers
     execution-request-header])
   (get-in
    ctx
    [:headers
     "Gesso-Optimistic-Execution"])
   (get-in
    ctx
    [:request
     :headers
     execution-request-header])
   (get-in
    ctx
    [:request
     :headers
     "Gesso-Optimistic-Execution"])))

;; -----------------------------------------------------------------------------
;; Semantic settlement
;; -----------------------------------------------------------------------------

(defn- command-applied-for-outcome
  [outcome]
  (contains?
   #{:confirmed
     :reconciled}
   outcome))

(defn ->settlement
  "Prepare one explicit semantic settlement.

   Required:

     :execution-id
       Browser-generated execution identity.

     :scope
       Semantic scope settled by this response.

     :outcome
       :confirmed, :reconciled, :rejected, or :failed.

   Optional:

     :revision
       Canonical semantic revision represented by the response.

     :reason
       Opaque semantic/application reason for diagnostics.

   :command-applied? is intentionally not configurable. It is derived from the
   outcome:

     confirmed/reconciled => true
     rejected/failed      => false

   Likewise :transition is intentionally absent. Settlement correlation is
   execution-id + scope in the verified choreography; repeating transition would
   introduce a redundant value that could disagree with the command."
  [{:keys [execution-id
           scope
           outcome
           revision
           reason]
    :as opts}]
  (when (contains?
         opts
         :transition)
    (throw
     (ex
      "gesso.live optimistic settlement does not accept redundant :transition."
      {:transition
       (:transition opts)})))
  (when (contains?
         opts
         :command-applied?)
    (throw
     (ex
      (str
       "gesso.live optimistic settlement does not accept :command-applied?; "
       "it is derived from :outcome.")
      {:command-applied?
       (:command-applied? opts)
       :outcome outcome})))
  (let [execution-id'
        (require-non-blank-string!
         :execution-id
         execution-id)
        scope'
        (normalize-scope
         scope)
        revision'
        (protocol/normalize-revision
         :revision
         revision)
        reason'
        (protocol/normalize-optional-name
         :reason
         reason)]
    (when-not
        (contains?
         settlement-outcomes
         outcome)
      (throw
       (ex
        "gesso.live optimistic settlement :outcome is invalid."
        {:outcome outcome
         :allowed
         settlement-outcomes})))
    {:gesso.live.optimistic/type
     settlement-type
     :protocol-version
     protocol-version
     :execution-id
     execution-id'
     :scope
     scope'
     :outcome
     outcome
     :revision
     revision'
     :command-applied?
     (command-applied-for-outcome
      outcome)
     :reason
     reason'}))

(defn settlement?
  [x]
  (and
   (map? x)
   (= settlement-type
      (:gesso.live.optimistic/type
       x))
   (= protocol-version
      (:protocol-version x))))

(defn ensure-settlement
  "Return a prepared protocol-v2 settlement unchanged, or normalize raw opts."
  [settlement]
  (if (settlement?
       settlement)
    settlement
    (->settlement
     settlement)))

(defn settlement-for-request
  "Prepare settlement using the browser execution id carried by ctx."
  [ctx opts]
  (->settlement
   (assoc
    opts
    :execution-id
    (or
     (:execution-id opts)
     (request-execution-id
      ctx)))))

(defn settlement-marker
  "Render an inert settlement marker.

   Canonical application content is separate markup. with-settlement is the
   preferred constructor because it derives canonical metadata from this same
   settlement and therefore cannot disagree on scope/revision."
  [settlement]
  (let [{:keys [execution-id
                scope
                outcome
                revision
                command-applied?
                reason]}
        (ensure-settlement
         settlement)]
    [:template
     (htmx/clean-attrs
      {protocol-attr
       protocol-version

       settlement-attr
       "true"

       execution-attr
       execution-id

       scope-attr
       (protocol/wire-scope
        scope)

       outcome-attr
       (name outcome)

       revision-attr
       (protocol/revision->wire
        revision)

       command-applied-attr
       (if command-applied?
         "true"
         "false")

       reason-attr
       reason})]))

(defn with-settlement
  "Return a settlement marker followed by exactly one authoritative root and any
   additional response nodes.

   canonical-node is automatically marked authoritative using settlement scope
   and revision. This makes it impossible for the marker and canonical root to
   disagree about those values.

   The display-contents wrapper remains suitable for OOB canonical fragments and
   auxiliary toast/diagnostic nodes."
  [settlement canonical-node & nodes]
  (let [settlement'
        (ensure-settlement
         settlement)
        canonical-node'
        (canonical
         {:scope
          (:scope settlement')
          :revision
          (:revision settlement')}
         (->> canonical-node
              (require-present!
               :canonical-content)
              (require-single-root!
               :canonical-content)))]
    (into
     [:div
      {:style
       {:display
        "contents"}}]
     (concat
      [(settlement-marker
        settlement')
       canonical-node']
      (remove nil?
              nodes)))))
