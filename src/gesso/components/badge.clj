(ns gesso.components.badge
  (:require [gesso.util :refer :all]))

(def ^:private badge-classes
  {:default "badge"
   :primary "badge-primary"
   :secondary "badge-secondary"
   :destructive "badge-destructive"
   :outline "badge-outline"})

(defn badge
  "Badge component.

  Short form:
    (badge {:text \"Waiting\" :variant :secondary})

  Long form:
    (badge {:variant :outline} \"Claimed\")"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [text variant]} props
          variant (or variant :default)
          cls (get badge-classes variant "badge")]
      (el :span
          {:class (class-names cls class)}
          attrs
          (nodes text)))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          variant (or (:variant props) :default)
          cls (get badge-classes variant "badge")]
      (el :span
          {:class (class-names cls class)}
          attrs
          children))))
