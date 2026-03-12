(ns gesso.components.checkbox
  (:require [gesso.util :refer :all]))

(defn checkbox
  "Checkbox input.

  Short form:
    (checkbox {:id \"done\"
               :name \"done\"
               :checked true})

  Long form:
    (checkbox {:attrs {:id \"done\" :name \"done\"}})"
  [& args]
  (let [[opts _children] (normalize-component-args args)
        {:keys [props class attrs]} (split-opts opts)
        {:keys [id name value checked disabled? required?]} props]
    (el :input
        {:class (class-names "input" class)
         :type "checkbox"}
        (merge-attrs
         attrs
         (when id {:id id})
         (when name {:name name})
         (when (some? value) {:value value})
         (when (some? checked) {:checked checked})
         (when (some? disabled?) {:disabled disabled?})
         (when (some? required?) {:required required?}))
        [])))
