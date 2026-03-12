(ns gesso.components.switch
  (:require [gesso.util :refer :all]))

(defn switch
  "Switch input.

  Short form:
    (switch {:id \"marketing\"
             :name \"marketing\"
             :checked true})

  Long form:
    (switch {:attrs {:id \"marketing\" :name \"marketing\"}})"
  [& args]
  (let [[opts _children] (normalize-component-args args)
        {:keys [props class attrs]} (split-opts opts)
        {:keys [id name value checked disabled? required?]} props]
    (el :input
        {:class (class-names "input" class)
         :type "checkbox"
         :role "switch"}
        (merge-attrs
         attrs
         (when id {:id id})
         (when name {:name name})
         (when (some? value) {:value value})
         (when (some? checked) {:checked checked})
         (when (some? disabled?) {:disabled disabled?})
         (when (some? required?) {:required required?}))
        [])))
