(ns gesso.components.accordion
  (:require
   [gesso.util :refer :all]
   [gesso.hyperscript :refer [hs merge-script-attr attach-script-to-node attach-script-to-children-by-tag]]))

(declare accordion)

;; -----------------------------------------------------------------------------
;; Hyperscript
;; -----------------------------------------------------------------------------

(defn- accordion-chevron-script []
  [[:let 'chev "first <svg[data-accordion-chevron]/> in me"]
   [:if 'chev
    [[:if 'me.open
       [:set 'chev.style.transform "'rotate(180deg)'"]
       [:set 'chev.style.transform "'rotate(0deg)'"]]]]])

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

(defn- accordion-clear-active-script []
  [[:let 'root "closest <div[data-accordion-root]/>"]
   [:for 'd "<details/> in root"
    [:set 'd.dataset.active "'false'"]
    [:set 'd.style.boxShadow "''"]]])

(defn- accordion-script
  [{:keys [type collapsible?]}]
  (let [type         (or type :multiple)
        collapsible? (if (nil? collapsible?) true collapsible?)]
    (hs
     [:on :toggle
      (when (= type :single)
        (accordion-single-script collapsible?))
      (accordion-chevron-script)])))

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
;; Visual helpers
;; -----------------------------------------------------------------------------

(defn- accordion-root-attrs
  [class]
  {:class (class-names "overflow-hidden rounded-lg shadow-sm" class)
   :data-accordion-root true
   :style {:border "1px solid var(--border)"
           :background "var(--card)"}})

(defn- accordion-item-attrs
  [value open? disabled? class attrs]
  (merge-attrs
   {:class (class-names
            "group overflow-hidden last:border-b-0"
            (when disabled? "opacity-60 pointer-events-none")
            class)
    :open (when open? true)
    :data-accordion-value (->value value "item")
    :style {:border-bottom "1px solid var(--border)"}}
   attrs))

(defn- accordion-trigger-attrs
  [class]
  {:class (class-names
           "cursor-pointer w-full list-none px-4 py-6 flex items-center justify-between gap-4 outline-none"
           class)
   :style {:background "var(--muted)"
           :color "var(--primary)"
           :font-weight 600}})

(defn- accordion-content-attrs
  [class]
  {:class (class-names
           "px-4 py-6"
           class)
   :style {:border-top "1px solid var(--border)"
           :background "var(--background)"
           :color "var(--muted-foreground)"
           :font-size "0.95rem"
           :line-height "1.7"}})

(defn- accordion-chevron-node []
  [:svg {:data-accordion-chevron true
         :aria-hidden "true"
         :viewBox "0 0 20 20"
         :fill "none"
         :stroke "currentColor"
         :stroke-width "2"
         :stroke-linecap "round"
         :stroke-linejoin "round"
         :style {:width "1rem"
                 :height "1rem"
                 :display "block"
                 :flex-shrink 0
                 :transition "transform 200ms ease"}}
   [:path {:d "M6 8l4 4 4-4"}]])

(defn- accordion-title-node
  [content]
  (into [:h2 {:class "m-0 flex-1 min-w-0 text-left"
              :style {:margin 0
                      :font-size "1.1rem"
                      :font-weight 600
                      :line-height 1.35
                      :color "inherit"}}]
        (normalize-children content)))

(defn- accordion-trigger-body
  [{:keys [content chevron?]}]
  (if chevron?
    [(accordion-title-node content)
     (accordion-chevron-node)]
    [(accordion-title-node content)]))

;; -----------------------------------------------------------------------------
;; Components
;; -----------------------------------------------------------------------------

(defn accordion-content
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

;; -----------------------------------------------------------------------------
;; Root render helpers
;; -----------------------------------------------------------------------------

(defn- render-prepared-items
  [items* script]
  (for [item items*]
    (let [item-attrs (get-in item [:attrs])]
      (accordion-item
       (assoc item :attrs (merge-script-attr item-attrs script))))))

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
  (accordion {:item-fn item-fn
              :items items}))

(defn- accordion-opts-fn-items-form
  [opts item-fn items]
  (accordion (assoc opts :item-fn item-fn :items items)))

(defn- accordion-long-form
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
     (accordion {:type :multiple} (accordion-item ...) ...)

  Options:
    :type             :single | :multiple (default :multiple)
    :default-value    for :single
    :default-values   for :multiple
    :default-index    for :single, zero-based
    :default-indexes  for :multiple, zero-based
    :collapsible?     for :single (default true)"
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
