(ns gesso.components.tabs.core
  (:require
   [gesso.util :refer :all]
   [gesso.hyperscript :refer [merge-script-attr]]
   [gesso.components.tabs.attr :refer :all]
   [gesso.components.tabs.scripts :refer :all]))

(declare tabs)

(defn- ->tabs-id-part
  "Normalizes a tab value into a stable id fragment."
  [v]
  (->value v "tab"))

(defn- root-id
  "Resolves a stable root id string."
  [attrs]
  (or (:id attrs) "tabs"))

(defn tabs-list
  "Renders the tab list container.

   The list should contain one or more tabs-trigger elements."
  [& args]
  (let [[opts children] (normalize-component-args args)
        {:keys [props class attrs]} (split-opts opts)
        orientation (or (:orientation props) :horizontal)]
    (el :nav
        (tabs-list-attrs class orientation)
        attrs
        children)))

(defn tabs-trigger
  "Renders a tab trigger button.

   Short form:
     (tabs-trigger {:value :account :text \"Account\"})

   Long form:
     (tabs-trigger {:value :account} \"Account\")

   Required:
   - :value

   Optional:
   - :disabled? true
   - :orientation :horizontal | :vertical

   A trigger is considered selected when its value matches the root's current
   selected value during initial render."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [value text selected? orientation disabled?]} props
          rid (root-id attrs)
          id-part (->tabs-id-part value)
          tab-id (str rid "-tab-" id-part)
          panel-id (str rid "-tab-" id-part "-panel")
          attrs (-> attrs
                    (dissoc :id)
                    (merge-script-attr (tabs-trigger-script)))]
      (el :button
          (tabs-trigger-attrs class value tab-id panel-id selected? orientation disabled?)
          attrs
          (nodes text)))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [value selected? orientation disabled?]} props
          rid (root-id attrs)
          id-part (->tabs-id-part value)
          tab-id (str rid "-tab-" id-part)
          panel-id (str rid "-tab-" id-part "-panel")
          attrs (-> attrs
                    (dissoc :id)
                    (merge-script-attr (tabs-trigger-script)))]
      (el :button
          (tabs-trigger-attrs class value tab-id panel-id selected? orientation disabled?)
          attrs
          children))))

(defn tabs-content
  "Renders a tab panel.

   Short form:
     (tabs-content {:value :account :children [...]})

   Long form:
     (tabs-content {:value :account} ...)

   Required:
   - :value

   Optional:
   - :selected? true during initial render
   - :orientation :horizontal | :vertical"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [value selected? orientation]} props
          rid (root-id attrs)
          id-part (->tabs-id-part value)
          tab-id (str rid "-tab-" id-part)]
      (el :div
          (tabs-content-attrs class value tab-id selected? orientation)
          (dissoc attrs :id)
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [value selected? orientation]} props
          rid (root-id attrs)
          id-part (->tabs-id-part value)
          tab-id (str rid "-tab-" id-part)]
      (el :div
          (tabs-content-attrs class value tab-id selected? orientation)
          (dissoc attrs :id)
          children))))

(defn- long-form-tabs
  [opts children]
  (let [{:keys [props class attrs]} (split-opts opts)
        {:keys [default-value orientation]} props
        orientation (or orientation :horizontal)
        current-value (some-> default-value str)
        rid (root-id attrs)
        children
        (map
         (fn [child]
           (if (vector? child)
             (let [[tag maybe-attrs & kids] child
                   has-attrs? (map? maybe-attrs)
                   child-attrs (if has-attrs? maybe-attrs {})
                   child-kids  (if has-attrs? kids (cons maybe-attrs kids))]
               (cond
                 (= tag :button)
                 (let [v (get child-attrs :data-tabs-value)
                       selected? (= (some-> v str) current-value)]
                   (into [tag
                          (assoc child-attrs
                                 :data-state (if selected? "active" "inactive")
                                 :aria-selected (if selected? "true" "false")
                                 :tabindex (if selected? "0" "-1")
                                 :data-orientation (name orientation)
                                 :id (or (:id child-attrs)
                                         (str rid "-tab-" (->tabs-id-part v))))]
                         child-kids))

                 (= tag :div)
                 (let [role (:role child-attrs)
                       v (get child-attrs :data-tabs-value)
                       selected? (= (some-> v str) current-value)
                       tab-id (str rid "-tab-" (->tabs-id-part v))]
                   (if (= role "tabpanel")
                     (into [tag
                            (assoc child-attrs
                                   :data-state (if selected? "active" "inactive")
                                   :aria-selected (if selected? "true" "false")
                                   :data-orientation (name orientation)
                                   :aria-labelledby tab-id
                                   :id (or (:id child-attrs) (str tab-id "-panel"))
                                   :hidden (when-not selected? true))]
                           child-kids)
                     child))

                 (= tag :nav)
                 (into [tag
                        (assoc child-attrs
                               :data-orientation (name orientation)
                               :aria-orientation (name orientation))]
                       child-kids)

                 :else
                 child))
             child))
         children)]
    (el :div
        (tabs-root-attrs class current-value orientation)
        (assoc attrs :id rid)
        children)))

(defn tabs
  "Renders an uncontrolled tabs root.

   Long form:
     (tabs {:default-value :account}
       (tabs-list
         (tabs-trigger {:value :account} \"Account\")
         (tabs-trigger {:value :password} \"Password\"))
       (tabs-content {:value :account} ...)
       (tabs-content {:value :password} ...))

   Options:
   - :default-value  initially selected tab value
   - :orientation    :horizontal | :vertical (default :horizontal)
   - :id             optional root id prefix used when deriving tab/panel ids

   This first pass is intentionally uncontrolled. The root stores the active
   value in the DOM and the trigger scripts update it locally.

   Possible later expansions:
   - controlled tabs with :value
   - keyboard navigation
   - activation mode support
   - short-form data-driven tabs if that proves useful"
  [& args]
  (let [[opts children] (normalize-component-args args)]
    (long-form-tabs opts children)))
