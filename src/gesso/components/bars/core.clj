(ns gesso.components.bars.core
  (:require
   [clojure.string :as str]
   [gesso.components.bars.attr :refer :all]
   [gesso.components.bars.scripts :refer :all]
   [gesso.components.dropdown-menu.core :as dropdown]
   [gesso.components.group :as group]
   [gesso.components.icon :as icon]
   [gesso.hyperscript :refer [merge-script-attr]]
   [gesso.util :refer :all]))

;; -----------------------------------------------------------------------------
;; Normalization helpers
;; -----------------------------------------------------------------------------

(def ^:private tiers
  [:default :md :sm])

(defn- slugify
  [x]
  (let [s (some-> x str str/lower-case str/trim)]
    (when (seq s)
      (-> s
          (str/replace #"\.svg$" "")
          (str/replace #"[^a-z0-9]+" "-")
          (str/replace #"(^-+|-+$)" "")))))

(defn- ensure-vec
  [x]
  (cond
    (nil? x) []
    (vector? x) x
    (sequential? x) (vec x)
    :else [x]))

(defn- normalize-keyword
  [x]
  (cond
    (keyword? x) x
    (string? x) (keyword x)
    :else x))

(defn- itemish?
  [x]
  (or (= :menu-item (:kind x))
      (string? x)
      (and (map? x)
           (not (contains? x :groups))
           (not (contains? x :items))
           (or (contains? x :text)
               (contains? x :href)
               (contains? x :action)
               (contains? x :icon)
               (contains? x :current?)))))

(defn- groupish?
  [x]
  (or (= :menu-group (:kind x))
      (and (map? x)
           (not (contains? x :groups))
           (or (contains? x :items)
               (contains? x :heading)))))

(defn- menuish?
  [x]
  (or (= :menu (:kind x))
      (and (map? x)
           (or (contains? x :groups)
               (contains? x :label)
               (contains? x :home-region)
               (contains? x :category)
               (contains? x :collapse-at)
               (contains? x :overflow-target)
               (contains? x :priority)))))

(declare normalize-menu-item)
(declare normalize-menu-group)
(declare normalize-menu)

(defn normalize-menu-item
  [x]
  (let [item (cond
               (string? x)
               {:text x}

               (map? x)
               x

               :else
               (throw (ex-info "menu-item expects a string or map"
                               {:value x})))
        text (or (:text item) (:label item))
        id   (or (:id item)
                 (slugify text)
                 (slugify (:href item))
                 "item")]
    (-> item
        (assoc :kind :menu-item
               :id id
               :text text)
        (update :attrs #(or % {})))))

(defn normalize-menu-group
  [x]
  (let [group (cond
                (nil? x)
                {:items []}

                (itemish? x)
                {:items [x]}

                (sequential? x)
                {:items x}

                (map? x)
                x

                :else
                (throw (ex-info "menu-group expects an item, a collection of items, or a map"
                                {:value x})))
        heading (:heading group)
        items   (->> (or (:items group) [])
                     ensure-vec
                     (map normalize-menu-item)
                     vec)
        id      (or (:id group)
                    (slugify heading)
                    (some-> items first :id)
                    "group")]
    (-> group
        (assoc :kind :menu-group
               :id id
               :heading heading
               :items items)
        (update :attrs #(or % {})))))

(defn normalize-menu
  [x]
  (let [menu (cond
               (itemish? x)
               {:groups [{:items [x]}]}

               (groupish? x)
               {:groups [x]}

               (sequential? x)
               {:groups x}

               (map? x)
               x

               :else
               (throw (ex-info "menu expects a menu map, a group, an item, or a collection"
                               {:value x})))
        groups (cond
                 (contains? menu :groups)
                 (:groups menu)

                 (contains? menu :items)
                 [{:heading (:heading menu)
                   :items (:items menu)}]

                 :else
                 [])
        groups (->> groups
                    ensure-vec
                    (map normalize-menu-group)
                    vec)
        label  (:label menu)
        id     (or (:id menu)
                   (slugify label)
                   (some-> groups first :id)
                   "menu")]
    (-> menu
        (assoc :kind :menu
               :id id
               :label label
               :groups groups
               :home-region (or (normalize-keyword (:home-region menu)) :center)
               :category (or (normalize-keyword (:category menu))
                             (normalize-keyword (:home-region menu))
                             :navigation)
               :collapse-at (normalize-keyword (:collapse-at menu))
               :overflow-target (or (normalize-keyword (:overflow-target menu)) :hamburger)
               :priority (or (:priority menu) 0))
        (update :attrs #(or % {})))))

(defn menu-item
  "Construct a normalized menu-item data map."
  [x]
  (normalize-menu-item x))

(defn menu-group
  "Construct a normalized menu-group data map."
  [x]
  (normalize-menu-group x))

(defn menu
  "Construct a normalized menu data map."
  [x]
  (normalize-menu x))

;; -----------------------------------------------------------------------------
;; Visibility / tier logic
;; -----------------------------------------------------------------------------

(defn- collapsed-at-tier?
  [collapse-at tier]
  (case collapse-at
    :always true
    :medium (contains? #{:md :sm} tier)
    :small (= tier :sm)
    :never false
    nil false
    false))

(defn- sidebar-visible-at-tier?
  [sidebar-collapse-at tier]
  (case sidebar-collapse-at
    :always false
    :medium (= tier :default)
    :small (not= tier :sm)
    :never true
    nil true
    true))

(defn- home-visible-at-tier?
  [menu tier sidebar-collapse-at]
  (and (if (= :sidebar (:home-region menu))
         (sidebar-visible-at-tier? sidebar-collapse-at tier)
         true)
       (not (collapsed-at-tier? (:collapse-at menu) tier))))

(defn- hamburger-visible-at-tier?
  [menu tier sidebar-collapse-at]
  (and (not (home-visible-at-tier? menu tier sidebar-collapse-at))
       (= :hamburger (:overflow-target menu))))

(defn- attach-visibility
  [menu sidebar-collapse-at]
  (assoc menu
         :home-visible
         (into {}
               (map (fn [tier]
                      [tier (home-visible-at-tier? menu tier sidebar-collapse-at)]))
               tiers)
         :hamburger-visible
         (into {}
               (map (fn [tier]
                      [tier (hamburger-visible-at-tier? menu tier sidebar-collapse-at)]))
               tiers)))

(defn- sort-menus
  [menus]
  (sort-by (fn [m]
             [(- (long (or (:priority m) 0)))
              (or (:label m) "")
              (:id m)])
           menus))

(defn- category-label
  [x]
  (-> (or x :more)
      name
      (str/replace #"-" " ")
      str/capitalize))

(defn- item-icon-node
  [x]
  (cond
    (nil? x) nil
    (string? x) (icon/icon x {:size :sm})
    (keyword? x) (icon/icon (name x) {:size :sm})
    :else x))

(defn- count-menu-items
  [menu]
  (reduce + 0 (map (comp count :items) (:groups menu))))

(defn- show-topbar-menu-label?
  [menu]
  (let [groups (:groups menu)
        group-count (count groups)
        item-count  (count-menu-items menu)]
    (and (seq (:label menu))
         (or (> group-count 1)
             (> item-count 1)
             (some :heading groups)))))

(defn- simple-topbar-menu?
  [menu]
  (let [groups (:groups menu)
        item-count (count-menu-items menu)
        first-heading (get-in menu [:groups 0 :heading])]
    (and (= 1 (count groups))
         (= 1 item-count)
         (not (seq (:label menu)))
         (not (seq first-heading)))))

(defn- menu-trigger-label
  [menu]
  (or (:label menu)
      (some-> menu :groups first :heading)
      (some-> menu :groups first :items first :text)
      "Menu"))

(defn- topbar-home-menus
  [menus region]
  (->> menus
       (filter #(= region (:home-region %)))
       sort-menus))

(defn- sidebar-home-menus
  [menus]
  (->> menus
       (filter #(= :sidebar (:home-region %)))
       sort-menus))

(defn- ordered-hamburger-categories
  [menus]
  (reduce
   (fn [acc menu]
     (let [category (:category menu)]
       (update acc category (fnil conj []) menu)))
   (array-map)
   (sort-menus menus)))

;; -----------------------------------------------------------------------------
;; Rendering helpers
;; -----------------------------------------------------------------------------

(defn- render-menu-item
  [item mode]
  (let [{:keys [text href icon current? class attrs]} item
        script    (when (= mode :hamburger)
                    (bars-close-script))
        attrs     (merge-script-attr attrs script)
        content   (concat
                   (when icon
                     [[:span {:data-bars-menu-item-icon true}
                       (item-icon-node icon)]])
                   (nodes text))
        defaults  (bars-menu-item-attrs mode current? class)
        attrs     (if href
                    attrs
                    (merge {:type "button"} attrs))]
    (if href
      (el :a defaults attrs content)
      (el :button defaults attrs content))))

(defn- render-topbar-group
  [group]
  (let [{:keys [heading items class attrs]} group]
    (el :div
        (bars-menu-group-attrs :topbar class)
        attrs
        (concat
         (when (seq heading)
           [(el :span
                (bars-menu-heading-attrs :topbar nil)
                {}
                (nodes heading))])
         [(el :div
              (bars-menu-items-attrs :topbar nil)
              {}
              (map #(render-menu-item % :topbar) items))]))))

(defn- render-topbar-simple-menu
  [menu]
  (let [{:keys [class attrs home-visible]} menu
        item (get-in menu [:groups 0 :items 0])]
    (el :div
        (bars-menu-attrs :topbar class home-visible)
        attrs
        [(render-menu-item item :topbar)])))

(defn- navigate-js
  [href]
  (str "window.location.href=" (pr-str href)))

(defn- dropdown-item-opts
  [item]
  (let [{:keys [class attrs href disabled? current?]} item
        attrs (cond-> (or attrs {})
                href (assoc :onclick (navigate-js href))
                current? (assoc :aria-current "page"))]
    (cond-> {:class (class-names
                     (when current? "weight-semibold-theme")
                     class)}
      (seq attrs) (assoc :attrs attrs)
      disabled? (assoc :disabled? true))))


(defn- dropdown-item-children
  [item]
  (let [{:keys [text icon]} item]
    [[:span {:data-bars-menu-item-content true}
      (when icon
        [:span {:data-bars-menu-item-icon true}
         (item-icon-node icon)])
      [:span text]]]))

(defn- dropdown-group-children
  [group]
  (let [{:keys [heading items]} group
        heading-node (when (seq heading)
                       [(dropdown/dropdown-menu-label {:text heading})])
        item-nodes   (map (fn [item]
                            (apply dropdown/dropdown-menu-item
                                   (dropdown-item-opts item)
                                   (dropdown-item-children item)))
                          items)]
    (concat heading-node item-nodes)))

(defn- dropdown-content-children
  [menu]
  (let [groups (:groups menu)
        last-idx (dec (count groups))]
    (mapcat
     (fn [idx group]
       (concat
        (dropdown-group-children group)
        (when (< idx last-idx)
          [(dropdown/dropdown-menu-separator)])))
     (range)
     groups)))

(defn- dropdown-trigger-child
  [menu]
  (let [label (menu-trigger-label menu)
        trigger-icon (:icon menu)
        chevron (icon/icon "chevron-down" {:size :sm})]
    (el :span
        (bars-menu-trigger-content-attrs nil)
        {}
        (concat
         (when trigger-icon
           [(el :span
                (bars-menu-trigger-icon-attrs nil)
                {}
                [(item-icon-node trigger-icon)])])
         [[:span label]]
         [(el :span
              (bars-menu-trigger-chevron-attrs nil)
              {}
              [chevron])]))))

(defn- render-topbar-dropdown-menu
  [menu]
  (let [{:keys [class attrs home-visible]} menu
        trigger (dropdown/dropdown-menu-trigger
                 (bars-menu-trigger-attrs nil)
                 (dropdown-trigger-child menu))
        content (apply dropdown/dropdown-menu-content
                       {}
                       (dropdown-content-children menu))
        node    (dropdown/dropdown-menu {}
                                      trigger
                                      content)]
    (el :div
        (bars-menu-attrs :topbar class home-visible)
        attrs
        [node])))

(defn- render-topbar-menu
  [menu]
  (if (simple-topbar-menu? menu)
    (render-topbar-simple-menu menu)
    (render-topbar-dropdown-menu menu)))

(defn- render-topbar-center-cluster
  [menus]
  (cond
    (empty? menus)
    []

    (= 1 (count menus))
    (map render-topbar-menu menus)

    :else
    [(apply group/group
            {:attached? true
             :wrap? false
             :class "items-center"}
            (map render-topbar-menu menus))]))

(defn- render-vertical-group
  [group mode]
  (let [{:keys [heading items class attrs]} group]
    (el :section
        (bars-menu-group-attrs mode class)
        attrs
        (concat
         (when (seq heading)
           [(el :div
                (bars-menu-heading-attrs mode nil)
                {}
                (nodes heading))])
         [(el :div
              (bars-menu-items-attrs mode nil)
              {}
              (map #(render-menu-item % mode) items))]))))

(defn- render-vertical-menu
  [menu mode]
  (let [{:keys [label groups class attrs]} menu
        visibility (case mode
                     :hamburger (:hamburger-visible menu)
                     (:home-visible menu))]
    (el :section
        (bars-menu-attrs mode class visibility)
        attrs
        (concat
         (when (seq label)
           [(el :div
                (bars-menu-label-attrs mode nil)
                {}
                (nodes label))])
         (map #(render-vertical-group % mode) groups)))))

(defn- render-hamburger-category
  [[category menus]]
  (let [visibility (into {}
                         (map (fn [tier]
                                [tier (boolean (some #(get-in % [:hamburger-visible tier]) menus))]))
                         tiers)]
    (el :section
        (bars-category-attrs nil visibility)
        {}
        (concat
         [(el :div
              (bars-category-label-attrs nil)
              {}
              [(category-label category)])]
         (map #(render-vertical-menu % :hamburger) menus)))))

(defn- bars-map-form
  [opts]
  (let [{:keys [props class attrs]} (split-opts opts)
        {:keys [brand menus sidebar-collapse-at content children]} props
        sidebar-collapse-at (or sidebar-collapse-at :medium)
        menus (->> menus
                   ensure-vec
                   (map normalize-menu)
                   (map #(attach-visibility % sidebar-collapse-at))
                   vec)
        left-menus (topbar-home-menus menus :leftmost)
        center-menus (topbar-home-menus menus :center)
        right-menus (topbar-home-menus menus :rightmost)
        sidebar-menus (sidebar-home-menus menus)
        hamburger-groups (ordered-hamburger-categories menus)
        has-sidebar? (boolean (seq sidebar-menus))
        has-hamburger-md? (boolean (some #(get-in % [:hamburger-visible :md]) menus))
        has-hamburger-sm? (boolean (some #(get-in % [:hamburger-visible :sm]) menus))
        root-attrs (merge-script-attr attrs (bars-root-script))
        content-nodes (concat (nodes content) (nodes children))
        header-children
        (concat
         [(el :div
              (bars-brand-attrs nil)
              {}
              (nodes brand))]
         [(el :nav
              (bars-segment-attrs :leftmost nil)
              {}
              (map render-topbar-menu left-menus))]
         [(el :nav
              (bars-segment-attrs :center nil)
              {}
              (render-topbar-center-cluster center-menus))]
         [(el :nav
              (bars-segment-attrs :rightmost nil)
              {}
              (map render-topbar-menu right-menus))]
         [(el :button
              (bars-toggle-attrs nil)
              (merge-script-attr {} (bars-toggle-script))
              [[:span {:aria-hidden "true"} "☰"]
               [:span "Menu"]])])
        hamburger-children
        [(el :div
             (bars-hamburger-inner-attrs nil)
             {}
             (map render-hamburger-category hamburger-groups))]
        body-children
        [(el :aside
             (bars-sidebar-attrs nil)
             {}
             (map #(render-vertical-menu % :sidebar) sidebar-menus))
         (el :div
             (bars-content-attrs nil)
             {}
             content-nodes)]]
    (el :div
        (bars-root-attrs class
                         {:has-sidebar? has-sidebar?
                          :sidebar-collapse-at sidebar-collapse-at
                          :has-hamburger-md? has-hamburger-md?
                          :has-hamburger-sm? has-hamburger-sm?})
        root-attrs
        [(el :header
             (bars-topbar-attrs nil)
             {}
             header-children)

         (el :section
             (bars-hamburger-panel-attrs nil)
             {}
             hamburger-children)

         (el :div
             (bars-body-attrs nil)
             {}
             body-children)])))

(defn bars
  "Responsive navigation shell.

  This first iteration owns:

  - topbar
  - optional sidebar
  - hamburger panel
  - wrapped page content

  Menus are normalized into the strict structure:

    menu -> menu-group -> menu-item

  while still allowing shorthand at author time.

  Supported menu metadata:
    :home-region     :leftmost | :center | :rightmost | :sidebar
    :category        keyword used to group hamburger output
    :collapse-at     nil | :small | :medium | :always | :never
    :overflow-target :hamburger | :disappear
    :priority        numeric sort priority

  Sidebar behavior:
    :sidebar-collapse-at => :medium (default) | :small | :never

  Simple topbar menus render directly.
  Richer topbar menus render as click-open dropdown menus."
  [& args]
  (if (only-map-arg? args)
    (bars-map-form (first args))
    (let [[opts children] (normalize-component-args args)]
      (bars-map-form (assoc opts :children children)))))
