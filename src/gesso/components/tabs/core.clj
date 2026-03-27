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

   HTMX notes:
   - HTMX attributes may be passed through :attrs.
   - This is useful when a tab switch should load panel content from the server
     or update URL/application state."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [value text selected? orientation disabled?]} props
          rid      (root-id attrs)
          id-part  (->tabs-id-part value)
          tab-id   (str rid "-tab-" id-part)
          panel-id (str tab-id "-panel")
          attrs    (-> attrs
                       (dissoc :id)
                       (merge-script-attr (tabs-trigger-script)))]
      (el :button
          (tabs-trigger-attrs class value tab-id panel-id selected? orientation disabled?)
          attrs
          (nodes text)))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [value selected? orientation disabled?]} props
          rid      (root-id attrs)
          id-part  (->tabs-id-part value)
          tab-id   (str rid "-tab-" id-part)
          panel-id (str tab-id "-panel")
          attrs    (-> attrs
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
   - :orientation :horizontal | :vertical

   HTMX notes:
   - HTMX attributes may be passed through :attrs.
   - A panel can be rendered empty initially and filled by HTMX after a tab
     switch, or re-rendered from the server as a normal fragment."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [value selected? orientation]} props
          rid     (root-id attrs)
          id-part (->tabs-id-part value)
          tab-id  (str rid "-tab-" id-part)]
      (el :div
          (tabs-content-attrs class value tab-id selected? orientation)
          (dissoc attrs :id)
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [value selected? orientation]} props
          rid     (root-id attrs)
          id-part (->tabs-id-part value)
          tab-id  (str rid "-tab-" id-part)]
      (el :div
          (tabs-content-attrs class value tab-id selected? orientation)
          (dissoc attrs :id)
          children))))

(defn- rewrite-trigger
  [node current-value orientation rid]
  (if (vector? node)
    (let [[tag maybe-attrs & kids] node
          has-attrs?  (map? maybe-attrs)
          child-attrs (if has-attrs? maybe-attrs {})
          child-kids  (if has-attrs? kids (cons maybe-attrs kids))]
      (if (= tag :button)
        (let [v         (get child-attrs :data-tabs-value)
              selected? (= (some-> v str) current-value)
              tab-id    (or (:id child-attrs)
                            (str rid "-tab-" (->tabs-id-part v)))
              panel-id  (or (:aria-controls child-attrs)
                            (str tab-id "-panel"))]
          (into
           [tag
            (assoc child-attrs
                   :id tab-id
                   :aria-controls panel-id
                   :aria-selected (if selected? "true" "false")
                   :tabindex (if selected? "0" "-1")
                   :data-state (if selected? "active" "inactive")
                   :data-orientation (name orientation)
                   :style (merge
                           (:style child-attrs)
                           {:box-shadow (when selected?
                                          (if (= orientation :vertical)
                                            "inset -1px 0 0 0 currentColor, -1px 0 0 0 currentColor"
                                            "inset 0 -1px 0 0 currentColor, 0 1px 0 0 currentColor"))
                            :color (when selected? "var(--primary)")}))]
           child-kids))
        node))
    node))

(defn- rewrite-root-child
  [child current-value orientation rid]
  (if (vector? child)
    (let [[tag maybe-attrs & kids] child
          has-attrs?  (map? maybe-attrs)
          child-attrs (if has-attrs? maybe-attrs {})
          child-kids  (if has-attrs? kids (cons maybe-attrs kids))]
      (cond
        (= tag :nav)
        (into
         [tag
          (assoc child-attrs
                 :data-orientation (name orientation)
                 :aria-orientation (name orientation))]
         (map #(rewrite-trigger % current-value orientation rid) child-kids))

        (and (= tag :div)
             (= "tabpanel" (:role child-attrs)))
        (let [v         (get child-attrs :data-tabs-value)
              selected? (= (some-> v str) current-value)
              tab-id    (str rid "-tab-" (->tabs-id-part v))]
          (into
           [tag
            (assoc child-attrs
                   :data-state (if selected? "active" "inactive")
                   :aria-selected (if selected? "true" "false")
                   :data-orientation (name orientation)
                   :aria-labelledby tab-id
                   :id (or (:id child-attrs) (str tab-id "-panel"))
                   :hidden (when-not selected? true))]
           child-kids))

        :else
        child))
    child))

(defn- long-form-tabs
  [opts children]
  (let [{:keys [props class attrs]} (split-opts opts)
        {:keys [value default-value orientation]} props
        orientation   (or orientation :horizontal)
        current-value (some-> (or value default-value) str)
        rid           (root-id attrs)
        children      (map #(rewrite-root-child % current-value orientation rid) children)]
    (el :div
        (tabs-root-attrs class current-value orientation)
        (assoc attrs :id rid)
        children)))

(defn tabs
  "Renders a tabs root.

   Long form:
     (tabs {:default-value :account}
       (tabs-list
         (tabs-trigger {:value :account} \"Account\")
         (tabs-trigger {:value :password} \"Password\"))
       (tabs-content {:value :account} ...)
       (tabs-content {:value :password} ...))

   Options:
   - :default-value  initially selected tab value for local/uncontrolled usage
   - :value          server-selected initial tab value; takes precedence over
                     :default-value and is useful for HTMX or URL-driven state
   - :orientation    :horizontal | :vertical (default :horizontal)
   - :id             optional root id prefix used when deriving tab/panel ids

   HTMX notes:
   - This remains locally interactive after render.
   - For server-selected tabs, pass :value when rendering from request or URL
     state.
   - HTMX attributes generally belong on tabs-trigger or tabs-content, depending
     on whether the server should respond to tab changes or fill panel bodies."
  [& args]
  (let [[opts children] (normalize-component-args args)]
    (long-form-tabs opts children)))
