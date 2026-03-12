(ns gesso.components.select
  (:require [gesso.util :refer :all]))

(defn select
  "Native select.

  Short form:
    (select {:options [{:value \"staff\" :label \"Staff\"}
                       {:value \"manager\" :label \"Manager\"}]
             :attrs {:name \"role\"}})

  Long form:
    (select {:attrs {:name \"role\"}}
      [:option {:value \"staff\"} \"Staff\"])"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [options]} props]
      (el :select
          {:class (class-names "select" class)}
          attrs
          (for [{:keys [value label selected disabled]} options]
            [:option
             (cond-> {:value value}
               selected (assoc :selected true)
               disabled (assoc :disabled true))
             label])))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :select
          {:class (class-names "select" class)}
          attrs
          children))))
