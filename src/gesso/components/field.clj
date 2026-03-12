(ns gesso.components.field
  (:require [gesso.util :refer :all]
            [gesso.components.label :as label]))

(defn field
  "Field wrapper for label + control + description + error.

  Short form:
    (field {:label-text \"Email\"
            :for \"email\"
            :required? true
            :control (input {:type \"email\" :id \"email\" :name \"email\"})
            :description \"We will never share it.\"
            :error nil})

  Long form:
    (field {:orientation :horizontal}
      (label/label {:text \"Email\" :for \"email\"})
      (input {:type \"email\" :id \"email\"}))"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [label-text for required? control description error orientation invalid?]} props]
      (el :div
          {:class (class-names "field" class)}
          (merge-attrs
           attrs
           (when orientation {:data-orientation (name orientation)})
           (when (some? invalid?) {:data-invalid (if invalid? "true" "false")}))
          [(when label-text
             (label/label
              {:text label-text
               :for for
               :required? required?}))
           control
           (when description [:p description])
           (when error [:div {:role "alert"} error])]))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [orientation invalid?]} props]
      (el :div
          {:class (class-names "field" class)}
          (merge-attrs
           attrs
           (when orientation {:data-orientation (name orientation)})
           (when (some? invalid?) {:data-invalid (if invalid? "true" "false")}))
          children))))
