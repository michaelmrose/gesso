(ns gesso.components.accordion.core
  (:require
   [gesso.util :refer :all]
   [gesso.components.accordion.scripts :refer :all]
   [gesso.components.accordion.attr :refer :all]
   [gesso.hyperscript :refer [hs merge-script-attr attach-script-to-node attach-script-to-children-by-tag]]))

(declare accordion)

;; -----------------------------------------------------------------------------
;; Item preparation
;; -----------------------------------------------------------------------------

(defn- resolve-default-one
  "Resolves the initially open item value for a :single accordion.

   Prefers an explicit default-value. If none is supplied, falls back to
   default-index and derives the value from the indexed item."
  [items* default-value default-index]
  (cond
    (some? default-value)
    (->value default-value default-value)

    (some? default-index)
    (when-let [item (nth items* default-index nil)]
      (->value (:value item) (str "item-" (inc default-index))))

    :else nil))

(defn- resolve-default-many
  "Resolves the set of initially open item values for a :multiple accordion.

   Prefers explicit default-values. If none are supplied, falls back to
   default-indexes and derives stable values from the indexed items."
  [items* default-values default-indexes]
  (cond
    (seq default-values)
    (->> default-values
         (map #(->value % %))
         set)

    (seq default-indexes)
    (->> default-indexes
         (keep (fn [idx]
                 (when-let [item (nth items* idx nil)]
                   (->value (:value item) (str "item-" (inc idx))))))
         set)

    :else #{}))

(defn- compute-open?
  "Determines whether an item should start open.

   An item's explicit :open? value takes precedence. Otherwise the result is
   derived from the accordion type and the resolved default value or values."
  [item value type default-one default-many]
  (cond
    (contains? item :open?) (:open? item)
    (= type :single) (= value default-one)
    (= type :multiple) (contains? default-many value)
    :else false))

(defn- prepare-accordion-items
  "Normalizes item input for rendering.

   Applies item-fn when provided, ensures each item has a stable :value, and
   fills in :open? based on the accordion type and default selection options."
  [{:keys [items item-fn type
           default-value default-values
           default-index default-indexes]}]
  (let [type         (or type :multiple)
        items*       (vec (or (if (and item-fn items)
                                (map item-fn items)
                                items)
                              []))
        default-one  (resolve-default-one items* default-value default-index)
        default-many (resolve-default-many items* default-values default-indexes)]
    (map-indexed
     (fn [i item]
       (let [value (->value (:value item) (str "item-" (inc i)))]
         (assoc item
                :value value
                :open? (compute-open? item value type default-one default-many))))
     items*)))

(defn- accordion-title-node
  "Wraps trigger content in the heading element used for accordion titles.

   Uses shared heading typography classes so the title participates in the
   theme's typography scale, while inheriting the trigger's explicit primary
   color."
  [content]
  (into [:h2 {:class "accordion-title m-0 flex-1 min-w-0 text-left font-heading text-md-theme leading-tight-theme tracking-tight-theme weight-semibold-theme"}]
        (normalize-children content)))

(defn- accordion-trigger-body
  "Builds the trigger contents for a summary row.

   Renders the title content and, when chevron? is truthy, appends the standard
   accordion chevron node."
  [{:keys [content chevron?]}]
  (if chevron?
    [(accordion-title-node content)
     (accordion-chevron-node)]
    [(accordion-title-node content)]))

;; -----------------------------------------------------------------------------
;; Components
;; -----------------------------------------------------------------------------

(defn accordion-content
  "Renders the revealed body region of an accordion item.

   Supports both the map-only short form and the long form with explicit
   children."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :section
          (accordion-content-attrs class)
          attrs
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :section
          (accordion-content-attrs class)
          attrs
          children))))

(defn accordion-trigger
  "Renders an accordion summary row.

   Supports both the short form with :text and the long form with explicit
   children. Chevron rendering is enabled by default and can be disabled with
   :chevron? false."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          text     (:text props)
          chevron? (not= false (:chevron? props))]
      (el :summary
          (accordion-trigger-attrs class)
          attrs
          (accordion-trigger-body {:content [text]
                                   :chevron? chevron?})))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          chevron? (not= false (:chevron? props))]
      (el :summary
          (accordion-trigger-attrs class)
          attrs
          (accordion-trigger-body {:content children
                                   :chevron? chevron?})))))

(defn- accordion-item-short-form
  "Renders an accordion item from the map-only short form.

   Builds a details element with a generated trigger and content section from
   the supplied title, content, and item options."
  [{:keys [value title content open? disabled? trigger-opts content-opts class attrs]}]
  (el :details
      {}
      (accordion-item-attrs value open? disabled? class attrs)
      [(accordion-trigger (merge {:chevron? true
                                  :text title}
                                 trigger-opts))
       (apply accordion-content content-opts (nodes content))]))

(defn- accordion-item-long-form
  "Renders an accordion item from the long form with explicit children.

   The caller supplies the trigger and content nodes directly."
  [opts children]
  (let [{:keys [props class attrs]} (split-opts opts)
        {:keys [value open? disabled?]} props]
    (el :details
        {}
        (accordion-item-attrs value open? disabled? class attrs)
        children)))

(defn accordion-item
  "Renders a single accordion item.

   Accepts either a short-form map with title and content keys or a long form
   with explicit child nodes."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (accordion-item-short-form (assoc props :class class :attrs attrs)))
    (let [[opts children] (normalize-component-args args)]
      (accordion-item-long-form opts children))))

;; -----------------------------------------------------------------------------
;; Root render helpers
;; -----------------------------------------------------------------------------

(defn- render-prepared-items
  "Renders a prepared collection of item maps.

   Attaches the computed accordion script to each item's attrs before delegating
   to accordion-item."
  [items* script]
  (for [item items*]
    (let [item-attrs (get-in item [:attrs])]
      (accordion-item
       (assoc item :attrs (merge-script-attr item-attrs script))))))

(defn- accordion-map-form
  "Renders the accordion from the map-only root form.

   Prepares item state, builds the item script once for the current accordion
   configuration, and renders the normalized items inside the root container."
  [opts]
  (let [{:keys [props class attrs]} (split-opts opts)
        {:keys [items item-fn type
                default-value default-values
                default-index default-indexes
                collapsible?]} props
        script (accordion-script {:type type :collapsible? collapsible?})
        items* (prepare-accordion-items
                {:items items
                 :item-fn item-fn
                 :type type
                 :default-value default-value
                 :default-values default-values
                 :default-index default-index
                 :default-indexes default-indexes})]
    (el :div
        (accordion-root-attrs class)
        attrs
        (render-prepared-items items* script))))

(defn- accordion-fn-items-form
  "Supports the shorthand form where an item-fn and item collection are passed
   directly to accordion."
  [item-fn items]
  (accordion {:item-fn item-fn
              :items items}))

(defn- accordion-opts-fn-items-form
  "Supports the shorthand form where root opts, an item-fn, and an item
   collection are passed directly to accordion."
  [opts item-fn items]
  (accordion (assoc opts :item-fn item-fn :items items)))

(defn- accordion-long-form
  "Renders the long-form accordion where children are supplied explicitly.

   Attaches the computed script to any details children before rendering the
   root container."
  [opts children]
  (let [{:keys [props class attrs]} (split-opts opts)
        {:keys [type collapsible?]} props
        script   (accordion-script {:type type :collapsible? collapsible?})
        children (attach-script-to-children-by-tag children :details script)]
    (el :div
        (accordion-root-attrs class)
        attrs
        children)))

(defn accordion
  "Renders an accordion root.

   Supported call styles:

   1) Map-only:
      (accordion {:items [...]})

   2) Map-only with item-fn:
      (accordion {:items coll :item-fn (fn [x] {:value ... :title ... :content ...})})

   3) Function + items:
      (accordion (fn [x] {:value ... :title ... :content ...}) coll)

   4) Options + function + items:
      (accordion {:type :single :default-value \"item-2\"} (fn [x] ...) coll)

   5) Long form children:
      (accordion {:type :multiple} (accordion-item ...) ...)

   Options:
     :type             :single | :multiple (default :multiple)
     :default-value    initially open value for :single
     :default-values   initially open values for :multiple
     :default-index    zero-based initially open index for :single
     :default-indexes  zero-based initially open indexes for :multiple
     :collapsible?     whether a :single accordion may close its last open item
                       (default true)."
  [& args]
  (cond
    (only-map-arg? args)
    (accordion-map-form (first args))

    (and (= 2 (count args))
         (fn? (first args))
         (sequential? (second args)))
    (accordion-fn-items-form (first args) (second args))

    (and (= 3 (count args))
         (map? (first args))
         (fn? (second args))
         (sequential? (nth args 2)))
    (accordion-opts-fn-items-form (first args) (second args) (nth args 2))

    :else
    (let [[opts children] (normalize-component-args args)]
      (accordion-long-form opts children))))
