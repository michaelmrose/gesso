(ns gesso.components.group
  (:require [gesso.util :refer :all]))

(defn- orientation-class
  [orientation]
  (if (= orientation :vertical)
    "flex-col"
    "flex-row"))

(defn- justify-class
  [align]
  (case align
    :start "justify-start"
    :center "justify-center"
    :end "justify-end"
    :between "justify-between"
    nil))

(defn- cross-axis-class
  [orientation]
  (if (= orientation :vertical)
    "items-stretch"
    "items-center"))

(defn- wrap-class
  [wrap? attached?]
  (if attached?
    "flex-nowrap"
    (if (false? wrap?)
      "flex-nowrap"
      "flex-wrap")))

(defn group
  "Generic grouping primitive for related inline controls or content.

  Short form:
    (group {:children [...]} )

  Long form:
    (group {:align :end}
      (button {:text \"Cancel\"})
      (button {:variant :primary :text \"Save\"}))

  Options:
    :orientation  :horizontal | :vertical   (default :horizontal)
    :align        :start | :center | :end | :between
    :wrap?        whether children may wrap (default true, ignored when attached)
    :attached?    visually attach adjacent children (default false)

  Notes:
    - When :attached? is true, direct children are visually joined through CSS
      selectors on the group root.
    - Group is layout-only; it does not change button semantics or behavior."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [children orientation align wrap? attached?]} props
          orientation (or orientation :horizontal)
          attached?   (boolean attached?)]
      (el :div
          {:class (class-names
                   "flex gap-cluster"
                   (orientation-class orientation)
                   (cross-axis-class orientation)
                   (justify-class align)
                   (wrap-class wrap? attached?)
                   class)
           :data-group true
           :data-attached (if attached? "true" "false")
           :data-orientation (name orientation)}
          attrs
          (nodes children)))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [orientation align wrap? attached?]} props
          orientation (or orientation :horizontal)
          attached?   (boolean attached?)]
      (el :div
          {:class (class-names
                   "flex gap-cluster"
                   (orientation-class orientation)
                   (cross-axis-class orientation)
                   (justify-class align)
                   (wrap-class wrap? attached?)
                   class)
           :data-group true
           :data-attached (if attached? "true" "false")
           :data-orientation (name orientation)}
          attrs
          children))))
