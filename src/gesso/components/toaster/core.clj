(ns gesso.components.toaster.core
  (:require
   [gesso.components.icon :as icon]
   [gesso.components.toaster.attr :as attr]
   [gesso.components.toaster.scripts :as scripts]
   [gesso.util :refer :all]))

(def variant-icons
  {"success" "check"
   "info"    "info"
   "warning" "triangle-alert"
   "danger"  "x"})

(defn toaster
  "Render the stable toaster region.

  This should usually be mounted once, outside ordinary HTMX swap targets.

  Example:
    (toaster)

    (toaster {:id \"app-toaster\"
              :position :bottom-right})"
  ([]
   (toaster {}))
  ([opts]
   (el :div
       {}
       (attr/toaster-attrs opts)
       [])))

(defn- icon-content
  [x]
  (cond
    (keyword? x)
    (icon/icon (name x) {:size :sm})

    (string? x)
    (icon/icon x {:size :sm})

    :else
    x))

(defn- resolved-icon
  [opts icon-value]
  (cond
    (false? icon-value)
    nil

    (some? icon-value)
    icon-value

    :else
    (get variant-icons (attr/toast-variant opts))))

(defn- toast-slot
  [attrs content]
  (when (some? content)
    (into [:div attrs]
          (nodes content))))

(defn- toast-icon
  [icon-value]
  (toast-slot
   (attr/toast-icon-attrs)
   (icon-content icon-value)))

(defn- toast-title
  [title]
  (toast-slot (attr/toast-title-attrs) title))

(defn- toast-description
  [description]
  (toast-slot (attr/toast-description-attrs) description))

(defn- toast-action
  [action]
  (toast-slot (attr/toast-action-attrs) action))

(defn- default-toast-content
  [opts {:keys [icon title description action]}]
  (let [icon-value (resolved-icon opts icon)]
    (normalize-children
     [(toast-icon icon-value)
      [:div (attr/toast-content-attrs)
       (toast-title title)
       (toast-description description)]
      (toast-action action)])))

(defn- close-button
  [opts]
  (when (attr/toast-close? opts)
    [:button
     (merge-attrs
      (scripts/dismiss-attrs)
      (attr/toast-close-attrs opts))
     "×"]))

(defn- toast-root-attrs
  [opts]
  (merge-attrs
   (scripts/auto-dismiss-attrs (attr/toast-duration opts))
   (attr/toast-attrs opts)))

(defn toast
  "Render a single toast item.

  Example:
    (toast {:variant :success
            :title \"Saved\"
            :description \"Your changes were saved.\"})

  Timed toast:
    (toast {:variant :success
            :title \"Saved\"
            :duration 4000})

  Custom content may be supplied as children:

    (toast {:variant :info}
      [:div \"Custom toast content\"])

  Pass :icon false to suppress the default variant icon.

  Pass :icon \"circle-check\" or another icon name to override the default."
  [& args]
  (let [[opts children]        (normalize-component-args args)
        {:keys [props]}       (split-opts opts)
        {:keys [title description
                icon action]} props
        content               (if (seq children)
                                children
                                (default-toast-content
                                 opts
                                 {:icon icon
                                  :title title
                                  :description description
                                  :action action}))]
    (el :div
        {}
        (toast-root-attrs opts)
        [(into [:div (attr/toast-body-attrs)]
               (normalize-children content))
         (close-button opts)])))

(defn render-toast-oob
  "Render an HTMX out-of-band append into a toaster region.

  Example:
    (render-toast-oob
     {:variant :success
      :title \"Saved\"
      :description \"Your changes were saved.\"})

  Timed toast:
    (render-toast-oob
     {:variant :success
      :title \"Saved\"
      :duration 4000})

  Defaults:
    :target \"app-toaster\"
    :swap   \"beforeend\""
  [opts]
  (let [{:keys [props]} (split-opts opts)
        toast-opts      (dissoc props :target :swap)]
    (el :div
        {}
        (attr/render-toast-oob-attrs opts)
        [(toast toast-opts)])))
