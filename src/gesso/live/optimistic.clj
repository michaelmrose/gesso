(ns gesso.live.optimistic
  "Server-rendered optimistic UI protocol helpers for gesso.live.

   This namespace owns the Clojure-facing half of the browser protocol already
   implemented by gesso-live.js:

   - optimistic template identity
   - optimistic source attrs
   - optimistic target selection
   - optional action and pending-label metadata
   - one-root <template> markup
   - target-scoped single-flight sync defaults

   It intentionally does not own:

   - HTMX POST controls or anti-forgery markup
   - application routes
   - application/domain transitions
   - visual components
   - browser snapshot, swap, rollback, or reconciliation behavior

   gesso.live.ui should compose these helpers with post-button. Application
   views should supply already-rendered optimistic content."
  (:require
   [clojure.string :as str]
   [gesso.live.htmx :as htmx])
  (:import
   [java.util UUID]))

;; -----------------------------------------------------------------------------
;; Browser protocol names
;; -----------------------------------------------------------------------------

(def descriptor-type
  :gesso.live.optimistic/descriptor)

(def template-attr
  "Source/template attribute used to associate an optimistic action with its
   server-rendered <template>."
  :data-gesso-optimistic-template)

(def action-attr
  "Optional semantic action name exposed to the browser runtime and CSS."
  :data-gesso-optimistic-action)

(def target-attr
  "Selector understood by gesso-live.js for resolving the replaceable target."
  :data-gesso-optimistic-target)

(def label-attr
  "Optional immediate press/pending label used by the browser runtime."
  :data-gesso-optimistic-label)

(def default-template-prefix
  "gesso-optimistic-template-")

(def default-sync-strategy
  "drop")

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

(defn- normalize-name
  [x]
  (cond
    (keyword? x) (name x)
    (symbol? x)  (name x)
    (nil? x)     nil
    :else        (str x)))

(defn- normalize-optional-name
  [k value]
  (when (some? value)
    (let [value' (normalize-name value)]
      (when (str/blank? value')
        (throw
         (ex (str "gesso.live optimistic " k " must not be blank.")
             {k value})))
      value')))

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

;; -----------------------------------------------------------------------------
;; Descriptor construction
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
         strategy' (normalize-optional-name :strategy strategy)]
     (when-not strategy'
       (throw
        (ex "gesso.live optimistic sync strategy is required."
            {:strategy strategy})))
     (str target' ":" strategy'))))

(defn ->optimistic
  "Create an optimistic-render descriptor.

   Required:
     :target
       Selector for the existing element that gesso-live.js should replace in
       place. Bare ids are normalized to CSS id selectors.

     :content
       Exactly one rooted Hiccup element. Its rendered root tag must match the
       current target element's tag; the browser runtime validates that at use
       time.

   Optional:
     :template-name
       Explicit template identity. Normally generated automatically. This is
       primarily useful for deterministic tests or app-owned diagnostics.

     :action
       Semantic action name, usually a keyword such as :claim.

     :pending-label
       Immediate press/pending text such as \"Claiming…\".

     :sync
       HTMX hx-sync value consumed later by gesso.live.ui. When omitted, a
       target-scoped \"...:drop\" value is derived. Explicit nil/false disables
       the suggested sync value.

     :attrs
       Extra attrs for the action source. Required optimistic protocol attrs
       always win over conflicting caller values.

     :template-attrs
       Extra attrs for the <template>. The generated template identity always
       wins over conflicting caller values."
  [{:keys [target
           content
           template-name
           action
           pending-label
           attrs
           template-attrs]
    :as opts}]
  (let [target'         (->> target
                             (require-non-blank-string! :target)
                             htmx/normalize-target)
        content'        (->> content
                             (require-present! :content)
                             require-single-root!)
        template-name'  (or (normalize-optional-name :template-name
                                                     template-name)
                            (new-template-name))
        action'         (normalize-optional-name :action action)
        pending-label'  (normalize-pending-label pending-label)
        attrs'          (require-map! :attrs (if (nil? attrs) {} attrs))
        template-attrs' (require-map! :template-attrs
                                      (if (nil? template-attrs)
                                        {}
                                        template-attrs))
        sync'           (if (contains? opts :sync)
                          (normalize-sync (:sync opts))
                          (target-sync target'))]
    {:gesso.live.optimistic/type descriptor-type
     :template-name template-name'
     :target target'
     :content content'
     :action action'
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
  "Return descriptor unchanged, or normalize a raw optimistic options map."
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
;; Browser-facing markup pieces
;; -----------------------------------------------------------------------------

(defn source-attrs
  "Build browser-protocol attrs for the element that owns the HTMX request.

   optimistic must be a prepared descriptor so source attrs and template markup
   cannot accidentally generate different identities. Caller attrs are
   preserved, but may not override Gesso's optimistic protocol attrs."
  [optimistic]
  (let [{:keys [template-name
                target
                action
                pending-label
                attrs]} (require-descriptor! optimistic)
        attrs' (apply dissoc
                      attrs
                      [template-attr action-attr target-attr label-attr])]
    (htmx/merge-attrs
     attrs'
     {template-attr template-name
      target-attr target}
     (when action
       {action-attr action})
     (when pending-label
       {label-attr pending-label}))))

(defn template
  "Render the hidden optimistic <template> associated with a prepared descriptor."
  [optimistic]
  (let [{:keys [template-name
                content
                template-attrs]} (require-descriptor! optimistic)
        template-attrs' (apply dissoc
                               template-attrs
                               [template-attr action-attr target-attr label-attr])]
    [:template
     (htmx/clean-attrs
      (merge
       template-attrs'
       {template-attr template-name}))
     content]))

(defn render-parts
  "Return all pieces needed by a higher-level UI helper.

   :source-attrs belongs on the actual HTMX request owner.
   :template should be rendered close to that source.
   :sync is the suggested hx-sync value."
  [optimistic]
  (let [optimistic' (ensure-optimistic optimistic)]
    {:optimistic optimistic'
     :source-attrs (source-attrs optimistic')
     :template (template optimistic')
     :sync (:sync optimistic')}))
