(ns gesso.components.field
  (:require [gesso.util :refer :all]
            [gesso.components.label :as label]
            [gesso.components.text :as text]))

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
          {:class (class-names "flex flex-col gap-field" class)}
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
           (when description
             (text/text
              {:variant :caption
               :as :p
               :text description}))
           (when error
             (text/text
              {:variant :caption
               :as :div
               :attrs {:role "alert"}
               :text error}))]))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [orientation invalid?]} props]
      (el :div
          {:class (class-names "flex flex-col gap-field" class)}
          (merge-attrs
           attrs
           (when orientation {:data-orientation (name orientation)})
           (when (some? invalid?) {:data-invalid (if invalid? "true" "false")}))
          children))))
