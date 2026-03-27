(ns gesso.components.radio-group
  (:require [gesso.util :refer :all]
            [gesso.components.text :as text]))

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
        {:class (class-names "radio input" class)
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

(defn- option-label-node
  "Renders the visible label text for a radio option using the shared text
   system."
  [label]
  (text/label-text {:text label}))

(defn- option-row-classes
  "Returns the classes for a radio option row.

   Uses the shared interactive row utilities so density changes affect the
   overall row height and padding, not just the group gap."
  [horizontal? disabled?]
  (class-names
   "interactive-row-theme flex items-center gap-inline rounded-md"
   (when horizontal? "inline-flex")
   (if disabled?
     "cursor-not-allowed opacity-50"
     "cursor-pointer hover:bg-accent/50")))

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
          group-name   (:name props)
          horizontal?  (= orientation :horizontal)
          group-class  (class-names
                        (if horizontal?
                          "flex flex-wrap items-center gap-inline"
                          "flex flex-col gap-field")
                        class)]
      (el :fieldset
          {:class group-class}
          (merge-attrs
           attrs
           (when orientation {:data-orientation (name orientation)}))
          (map-indexed
           (fn [i {:keys [value label checked disabled]}]
             (let [id               (str (or group-name "radio-group") "-" i)
                   option-disabled? (or disabled disabled?)]
               [:label {:class (option-row-classes horizontal? option-disabled?)
                        :for id
                        :aria-disabled (when option-disabled? "true")}
                (radio {:id id
                        :name group-name
                        :value value
                        :checked checked
                        :disabled? option-disabled?
                        :required? required?})
                (option-label-node label)]))
           options)))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [orientation]} props
          group-class (class-names
                       (if (= orientation :horizontal)
                         "flex flex-wrap items-center gap-inline"
                         "flex flex-col gap-field")
                       class)]
      (el :fieldset
          {:class group-class}
          (merge-attrs
           attrs
           (when orientation {:data-orientation (name orientation)}))
          children))))
