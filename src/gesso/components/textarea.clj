(ns gesso.components.textarea
  (:require [gesso.util :refer :all]))

(defn textarea
  "Multiline textarea.

  Short form:
    (textarea {:attrs {:id \"notes\" :name \"notes\" :rows 4}})

  Long form:
    (textarea {:attrs {...}} \"Initial value\")"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :textarea
          {:class (class-names "textarea" class)}
          attrs
          (nodes (:text props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :textarea
          {:class (class-names "textarea" class)}
          attrs
          children))))
