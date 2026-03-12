(ns gesso.components.field
  (:require [gesso.util :refer :all]
            [gesso.components.label :as label]))

(defn field
  "Field wrapper for label + control + description + error.

  Short form:
    (field {:label \"Email\"
            :for \"email\"
            :control (input {:type \"email\" :attrs {:id \"email\" :name \"email\"}})
            :description \"We will never share it.\"
            :error nil})

  Long form:
    (field {}
      (label/label {:attrs {:for \"email\"}} \"Email\")
      (input {:type \"email\" :attrs {:id \"email\"}}))"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [label for control description error orientation invalid?]} props]
      (el :div
          {:class (class-names "field" class)}
          (merge-attrs
           attrs
           (when orientation {:data-orientation (name orientation)})
           (when (some? invalid?) {:data-invalid (if invalid? "true" "false")}))
          [(when label
             (if for
               (label/label {:text label :attrs {:for for}})
               (label/label {:text label})))
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
