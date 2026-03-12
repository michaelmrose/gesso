(ns gesso.components.radio-group
  (:require [gesso.util :refer :all]))



(defn radio
  "Radio input.

  Short form:
    (radio {:id \"all\"
            :name \"notify\"
            :value \"all\"
            :checked true})"
  [& args]
  (let [[opts _children] (normalize-component-args args)
        {:keys [props class attrs]} (split-opts opts)
        {:keys [id name value checked disabled? required?]} props]
    (el :input
        {:class (class-names "input" class)
         :type "radio"
         }
        (merge-attrs
         attrs
         (when id {:id id})
         (when name {:name name})
         (when (some? value) {:value value})
         (when checked {:checked true})
         (when disabled? {:disabled true})
         (when required? {:required true}))
        [])))

(defn radio-group
  "Radio group wrapper.

  Short form:
    (radio-group
      {:name \"notify\"
       :options [{:value \"all\" :label \"All\" :checked true}
                 {:value \"mentions\" :label \"Mentions\"}]
       :orientation :vertical})

  Long form:
    (radio-group {}
      ...custom radio rows...)"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [options orientation required? disabled?]} props
          group-name (:name props)
          fieldset-class (class-names
                          "grid gap-3"
                          (when (= orientation :horizontal) "grid-flow-col auto-cols-max items-center")
                          class)]
      (el :fieldset
          {:class fieldset-class}
          attrs
          (map-indexed
           (fn [i {:keys [value label checked disabled]}]
             (let [id (str (or group-name "radio-group") "-" i)]
               [:label {:class "label" :for id}
                (radio {:id id
                        :name group-name
                        :value value
                        :checked checked
                        :disabled? (or disabled disabled?)
                        :required? required?})
                label]))
           options)))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [orientation]} props
          fieldset-class (class-names
                          "grid gap-3"
                          (when (= orientation :horizontal) "grid-flow-col auto-cols-max items-center")
                          class)]
      (el :fieldset
          {:class fieldset-class}
          attrs
          children))))
