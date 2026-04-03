(ns gesso.components.sidebar
  (:require
   [gesso.components.icon :refer [icon]]
   [gesso.util :refer :all]))

(defn- normalize-region [x]
  (cond
    (keyword? x) x
    (string? x) (keyword x)
    :else x))

(defn- sidebar-item-content
  [{:keys [icon label content]}]
  (cond
    content (nodes content)
    :else
    [(when icon
       (icon icon {:size :sm}))
     [:span label]]))

(defn- render-sidebar-item
  [{:keys [href attrs] :as item}]
  (let [base-attrs (merge-attrs
                    {:class "font-body text-sm-theme leading-body weight-medium-theme"
                     :data-sidebar-link true
                     :style {:color "var(--foreground)"}}
                    attrs)]
    (if href
      (into [:a (assoc base-attrs :href href)]
            (sidebar-item-content item))
      (into [:button (merge-attrs {:type "button"} base-attrs)]
            (sidebar-item-content item)))))

(defn sidebar-section
  "Manual sidebar section.

  Short form:
    (sidebar-section {:title \"Overview\" :items [...]})

  Long form:
    (sidebar-section {:title \"Overview\"}
      ...)"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [title items]} props]
      (el :section
          {:class (class-names class)
           :data-sidebar-section true}
          attrs
          [[:div {:data-sidebar-heading true} title]
           [:div {:data-sidebar-list true}
            (for [item items]
              [:div {:key (str "sidebar-item-" (or (:id item) (:label item)))}
               (render-sidebar-item item)])]]))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [title]} props]
      (el :section
          {:class (class-names class)
           :data-sidebar-section true}
          attrs
          [[:div {:data-sidebar-heading true} title]
           [:div {:data-sidebar-list true} children]]))))

(defn sidebar-overflow-items
  "Transform sidebar sections into overflow items suitable for topbar :menu-items.

  Example:
    (sidebar-overflow-items sections)
    (sidebar-overflow-items sections {:collapse-at :md})

  Each returned item is tagged with:
    :region :sidebar
    :overflow-target :menu
    :collapse-at defaults to :md
    :category defaults to the section title"
  ([sections]
   (sidebar-overflow-items sections {}))
  ([sections {:keys [collapse-at overflow-target]}]
   (vec
    (mapcat
     (fn [{:keys [title items]}]
       (for [item items]
         (merge
          {:region :sidebar
           :collapse-at (or (:collapse-at item) collapse-at :md)
           :overflow-target (or (:overflow-target item) overflow-target :menu)
           :category (or (:category item) title)}
          item)))
     sections))))

(defn sidebar
  "Sectioned sidebar navigation.

  Options:
    :sections      [{:title ... :items [...]} ...]
    :collapse-at   :md | :sm | nil   (default :md)

  The sidebar does not know about topbar directly.
  Use sidebar-overflow-items to contribute collapsed sidebar items upward."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [sections collapse-at]} props
          collapse-at (or (normalize-region collapse-at) :md)]
      (el :aside
          {:class (class-names class)
           :data-sidebar true
           :data-collapse-at (some-> collapse-at name)}
          attrs
          (for [section sections]
            [:div {:key (str "sidebar-section-" (:title section))}
             (sidebar-section {:title (:title section)
                               :items (:items section)})])))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [collapse-at]} props
          collapse-at (or (normalize-region collapse-at) :md)]
      (el :aside
          {:class (class-names class)
           :data-sidebar true
           :data-collapse-at (some-> collapse-at name)}
          attrs
          children))))
