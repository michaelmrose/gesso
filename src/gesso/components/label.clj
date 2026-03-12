(ns gesso.components.label
  (:require [gesso.util :refer :all]))

(defn label
  "Label component.

  Short form:
    (label {:text \"Email\" :for \"email\"})
    (label {:text \"Email\" :required? true})

  Long form:
    (label {:attrs {:for \"email\"}} \"Email\")"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [text for required?]} props]
      (el :label
          {:class (class-names "label" class)}
          (merge-attrs
           attrs
           (when for {:for for}))
          [(when text text)
           (when required?
             [:span {:aria-hidden "true" :class "text-destructive ml-1"} "*"])]))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [for required?]} props]
      (el :label
          {:class (class-names "label" class)}
          (merge-attrs
           attrs
           (when for {:for for}))
          [(normalize-children children)
           (when required?
             [:span {:aria-hidden "true" :class "text-destructive ml-1"} "*"])]))))
