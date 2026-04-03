(ns gesso.components.topbar
  (:require
   [gesso.components.icon :refer [icon]]
   [gesso.util :refer :all]))

(defn- normalize-region [x]
  (cond
    (keyword? x) x
    (string? x) (keyword x)
    :else x))

(defn- normalize-item
  [idx item]
  (-> item
      (update :region #(or (normalize-region %) :right))
      (update :collapse-at normalize-region)
      (update :overflow-target normalize-region)
      (assoc :_idx idx)
      (update :priority #(or % 0))))

(defn- sort-items
  [items]
  (sort-by (juxt (comp - :priority) :_idx) items))

(defn- region-items
  [items region]
  (->> items
       (filter #(= region (:region %)))
       sort-items))

(defn- menu-candidate?
  [item]
  (or (:menu-only? item)
      (and (= :menu (:overflow-target item))
           (:collapse-at item))))

(defn- flatten-menu-source-item
  [item]
  (if (seq (:children item))
    [(assoc item :menu-only? true)]
    [item]))

(defn- menu-source-items
  [items extra-items]
  (->> (concat
        (filter menu-candidate? items)
        extra-items)
       (map-indexed normalize-item)
       (mapcat flatten-menu-source-item)
       sort-items))

(defn- add-group
  [groups item]
  (let [category (:category item)]
    (if-some [idx (first (keep-indexed (fn [i g]
                                         (when (= (:category g) category) i))
                                       groups))]
      (update-in groups [idx :items] conj item)
      (conj groups {:category category
                    :items [item]}))))

(defn- grouped-menu-items
  [items]
  (reduce add-group [] items))

(defn- link-or-button
  [tag attrs content]
  (into [tag attrs] content))

(defn- item-content
  [{:keys [icon label content]}]
  (cond
    content (nodes content)
    :else
    [(when icon
       (icon icon {:size :sm}))
     [:span label]]))

(defn- render-inline-item
  [{:keys [href attrs collapse-at] :as item}]
  (let [base-attrs (merge-attrs
                    {:class "font-body text-sm-theme leading-body weight-medium-theme"
                     :data-topbar-inline-item true
                     :data-collapse-at (some-> collapse-at name)
                     :style {:color "var(--foreground)"}}
                    attrs)]
    (if href
      (link-or-button :a
                      (assoc base-attrs :href href)
                      (item-content item))
      (link-or-button :button
                      (merge-attrs
                       {:type "button"}
                       base-attrs)
                      (item-content item)))))

(defn- render-menu-leaf
  [{:keys [href attrs collapse-at] :as item}]
  (let [base-attrs (merge-attrs
                    {:class "font-body text-base-theme leading-body"
                     :data-topbar-menu-item true
                     :data-menu-collapse-at (some-> collapse-at name)
                     :style {:color "var(--foreground)"}}
                    attrs)]
    (if href
      (link-or-button :a
                      (assoc base-attrs
                             :href href
                             :data-topbar-menu-link true)
                      (item-content item))
      (link-or-button :button
                      (merge-attrs
                       {:type "button"
                        :data-topbar-menu-link true}
                       base-attrs)
                      (item-content item)))))

(defn- render-menu-group
  [{:keys [category items]}]
  (let [group-children
        (for [item items]
          [:div {:key (str "item-" (or (:id item) (:label item) (:_idx item)))}
           (if (seq (:children item))
             [:details {:data-topbar-menu-group true}
              [:summary (:label item)]
              [:div {:data-topbar-menu-group-body true}
               (for [child (sort-items (map-indexed normalize-item (:children item)))]
                 [:div {:key (str "child-" (or (:id child) (:label child) (:_idx child)))}
                  (render-menu-leaf child)])]]
             (render-menu-leaf item))])]
    (if category
      [:details {:data-topbar-menu-group true}
       [:summary category]
       [:div {:data-topbar-menu-group-body true}
        group-children]]
      [:div {:data-topbar-menu-list true}
       group-children])))

(defn- toggle-script []
  (str
   "on click "
   "set root to closest <header[data-topbar]/> "
   "if root's @data-menu-open is 'true' "
   "set root's @data-menu-open to 'false' "
   "set my @aria-expanded to 'false' "
   "else "
   "set root's @data-menu-open to 'true' "
   "set my @aria-expanded to 'true' "
   "end"))

(defn topbar
  "Responsive top bar with inline items plus a toggled overflow menu."
  [& args]
  (let [opts (if (only-map-arg? args)
               (first args)
               (let [[opts children] (normalize-component-args args)]
                 (assoc opts :menu-extra children)))
        {:keys [props class attrs]} (split-opts opts)
        {:keys [brand items menu-items menu-title menu-extra]} props
        items        (->> items (map-indexed normalize-item) sort-items vec)
        menu-items   (->> menu-items (map-indexed normalize-item) sort-items vec)
        left-items   (region-items items :left)
        center-items (region-items items :center)
        right-items  (region-items items :right)
        overflow     (menu-source-items items menu-items)
        groups       (grouped-menu-items overflow)
        has-menu?    (seq overflow)
        brand-node   (when brand
                       (into [:div {:data-topbar-brand true}]
                             (nodes brand)))
        menu-title-node
        (when menu-title
          (into [:div {:class "font-heading text-lg-theme leading-tight-theme tracking-tight-theme weight-semibold-theme"}]
                (nodes menu-title)))
        menu-panel-node
        (when has-menu?
          (into
           [:div {:data-topbar-menu-panel true}
            [:div {:data-topbar-menu-inner true}
             menu-title-node
             (for [group groups]
               [:div {:key (str "group-" (or (:category group) "ungrouped"))}
                (render-menu-group group)])]]
           (nodes menu-extra)))]
    (el :header
        {:class (class-names class)
         :data-topbar true
         :data-menu-open "false"}
        attrs
        [[:div {:data-topbar-row true}
          [:div {:data-topbar-left true}
           brand-node
           (for [item left-items]
             [:div {:key (str "left-" (or (:id item) (:label item) (:_idx item)))}
              (render-inline-item item)])]

          [:div {:data-topbar-center true}
           (for [item center-items]
             [:div {:key (str "center-" (or (:id item) (:label item) (:_idx item)))}
              (render-inline-item item)])]

          [:div {:data-topbar-right true}
           (for [item right-items]
             [:div {:key (str "right-" (or (:id item) (:label item) (:_idx item)))}
              (render-inline-item item)])
           (when has-menu?
             [:button {:type "button"
                       :class "button-density radius-md border-theme"
                       :data-topbar-menu-toggle true
                       :aria-expanded "false"
                       :_ (toggle-script)}
              (icon "menu" {:size :md})
              [:span "Menu"]])]]

         menu-panel-node])))
