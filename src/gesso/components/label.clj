(ns gesso.components.label
  (:require [gesso.util :refer :all]))

(defn label
  "Label component.

  Short form:
    (label {:text \"Email\" :attrs {:for \"email\"}})

  Long form:
    (label {:attrs {:for \"email\"}} \"Email\")"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :label
          {:class (class-names "label" class)}
          attrs
          (nodes (:text props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :label
          {:class (class-names "label" class)}
          attrs
          children))))
