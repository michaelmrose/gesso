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
         :type "radio"}
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
          horizontal? (= orientation :horizontal)
          fieldset-class (class-names
                          "grid gap-3"
                          (when horizontal? "grid-flow-col auto-cols-max items-center")
                          class)
          option-label-class (class-names
                              "label rounded-md px-3 py-2 gap-3"
                              (when-not disabled? "cursor-pointer hover:bg-accent/50")
                              (when horizontal? "inline-flex"))]
      (el :fieldset
          {:class fieldset-class}
          attrs
          (map-indexed
           (fn [i {:keys [value label checked disabled]}]
             (let [id (str (or group-name "radio-group") "-" i)
                   option-disabled? (or disabled disabled?)]
               [:label {:class (class-names
                                option-label-class
                                (when option-disabled? "cursor-not-allowed opacity-50"))
                        :for id}
                (radio {:id id
                        :name group-name
                        :value value
                        :checked checked
                        :disabled? option-disabled?
                        :required? required?})
                [:span label]]))
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
