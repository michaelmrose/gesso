(ns gesso.components.accordion
  (:require
   [gesso.util :refer :all]
   [gesso.hyperscript :refer [hs]]))

;; -----------------------------------------------------------------------------
;; Hyperscript
;; -----------------------------------------------------------------------------

(defn- accordion-chevron-script []
  [[:let 'chev "first <span[data-accordion-chevron]/> in me"]
   [:if 'chev
    [[:if 'me.open
       [:set 'chev.innerHTML "'▴'"]
       [:set 'chev.innerHTML "'▾'"]]]]])

(defn- accordion-single-script
  [collapsible?]
  (if collapsible?
    [[:let 'root "closest <div[data-accordion-root]/>"]
     [:if 'me.open
      [[:for 'd "<details/> in root"
        [:if "d != me"
         [:set 'd.open false]]]]]]

    [[:let 'root "closest <div[data-accordion-root]/>"]
     [:if 'me.open
      [[:for 'd "<details/> in root"
        [:if "d != me"
         [:set 'd.open false]]]]
      [[:let 'anyOpen false]
       [:for 'd "<details/> in root"
        [:if 'd.open
         [:set 'anyOpen true]]]
       [:if "not anyOpen"
        [:set 'me.open true]]]]]))

(defn- accordion-script
  [{:keys [type collapsible?]}]
  (let [type         (or type :multiple)
        collapsible? (if (nil? collapsible?) true collapsible?)]
    (hs
     [:on :toggle
      (when (= type :single)
        (accordion-single-script collapsible?))
      (accordion-chevron-script)])))

(defn- add-accordion-script
  [attrs script]
  (if script
    (merge-attrs attrs {:_ script})
    attrs))

(defn- add-script-to-details-child
  [child script]
  (if (and script (vector? child) (= :details (first child)))
    (let [[tag maybe-attrs & kids] child
          has-attrs? (map? maybe-attrs)
          attrs      (if has-attrs? maybe-attrs {})
          kids       (if has-attrs? kids (cons maybe-attrs kids))]
      (into [tag (add-accordion-script attrs script)] kids))
    child))

(defn- add-script-to-details-children
  [children script]
  (map #(add-script-to-details-child % script) children))

;; -----------------------------------------------------------------------------
;; Item preparation
;; -----------------------------------------------------------------------------

(defn- resolve-default-one
  [items* default-value default-index]
  (cond
    (some? default-value)
    (->value default-value default-value)

    (some? default-index)
    (when-let [item (nth items* default-index nil)]
      (->value (:value item) (str "item-" (inc default-index))))

    :else nil))

(defn- resolve-default-many
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
  [item value type default-one default-many]
  (cond
    (contains? item :open?) (:open? item)
    (= type :single) (= value default-one)
    (= type :multiple) (contains? default-many value)
    :else false))

(defn- prepare-accordion-items
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

;; -----------------------------------------------------------------------------
;; Trigger helpers
;; -----------------------------------------------------------------------------

(defn- accordion-title-node
  [content class]
  (into [:h2 {:class (class-names "m-0 flex-1 min-w-0 text-left text-inherit font-inherit" class)}]
        (normalize-children content)))

(defn- accordion-chevron-node []
  [:span {:data-accordion-chevron true
          :aria-hidden "true"
          :class "text-xl leading-none shrink-0"}
   "▾"])

(defn- accordion-trigger-body
  [{:keys [content chevron?]}]
  (if chevron?
    [(accordion-title-node content nil)
     (accordion-chevron-node)]
    [(accordion-title-node content "w-full")]))

(defn- accordion-trigger-opts
  [class]
  {:class (class-names
           "cursor-pointer w-full font-medium p-4 hover:bg-accent/50 list-none flex justify-between items-center gap-3"
           class)})

;; -----------------------------------------------------------------------------
;; Components
;; -----------------------------------------------------------------------------
(declare accordion)

(defn- accordion-content-opts
  [class]
  {:class (class-names
           "border-t border-border p-4 text-sm leading-6 text-muted-foreground"
           class)})

(defn accordion-content
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :section
          (accordion-content-opts class)
          attrs
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :section
          (accordion-content-opts class)
          attrs
          children))))

(defn accordion-trigger
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          text      (:text props)
          chevron?  (not= false (:chevron? props))]
      (el :summary
          (accordion-trigger-opts class)
          attrs
          (accordion-trigger-body {:content [text]
                                   :chevron? chevron?})))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          chevron? (not= false (:chevron? props))]
      (el :summary
          (accordion-trigger-opts class)
          attrs
          (accordion-trigger-body {:content children
                                   :chevron? chevron?})))))

(defn- accordion-item-attrs
  [value open? disabled? class attrs]
  (merge-attrs
   {:class (class-names
            "border-b last:border-b-0 group"
            (when disabled? "opacity-60 pointer-events-none")
            class)
    :open (when open? true)
    :data-accordion-value (->value value "item")}
   attrs))

(defn- accordion-item-short-form
  [{:keys [value title content open? disabled? trigger-opts content-opts class attrs]}]
  (el :details
      {}
      (accordion-item-attrs value open? disabled? class attrs)
      [(accordion-trigger (merge {:chevron? true
                                  :text title}
                                 trigger-opts))
       (apply accordion-content content-opts (nodes content))]))

(defn- accordion-item-long-form
  [opts children]
  (let [{:keys [props class attrs]} (split-opts opts)
        {:keys [value open? disabled?]} props]
    (el :details
        {}
        (accordion-item-attrs value open? disabled? class attrs)
        children)))

(defn accordion-item
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (accordion-item-short-form (assoc props :class class :attrs attrs)))
    (let [[opts children] (normalize-component-args args)]
      (accordion-item-long-form opts children))))

(defn- accordion-root-attrs
  [class]
  {:class (class-names "border rounded-lg overflow-hidden shadow-sm" class)
   :data-accordion-root true})

(defn- render-prepared-items
  [items* script]
  (for [item items*]
    (let [item-attrs (get-in item [:attrs])]
      (accordion-item (assoc item :attrs (add-accordion-script item-attrs script))))))

(defn- accordion-map-form
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
  [item-fn items]
  (accordion {:item-fn item-fn :items items}))

(defn- accordion-opts-fn-items-form
  [opts item-fn items]
  (accordion (assoc opts :item-fn item-fn :items items)))

(defn- accordion-long-form
  [opts children]
  (let [{:keys [props class attrs]} (split-opts opts)
        {:keys [type collapsible?]} props
        script   (accordion-script {:type type :collapsible? collapsible?})
        children (add-script-to-details-children children script)]
    (el :div
        (accordion-root-attrs class)
        attrs
        children)))

(defn accordion
  "Accordion root.

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
     (accordion {:type :multiple} (accordion-item ...) ...)"
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
