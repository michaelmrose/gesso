(ns gesso.components.radio-group
  (:require [gesso.util :refer :all]
            [gesso.components.text :as text]))

(defn radio
  "Radio input.

  Short form:
    (radio {:id \"all\"
            :name \"notify\"
            :value \"all\"
            :checked true})

  HTMX attributes may be passed through :attrs when an individual radio should
  trigger a request or swap."
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
  [label]
  (text/label-text {:text label}))

(defn- option-row-classes
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
      ...custom radio rows...)

  HTMX notes:
    - HTMX attributes may be passed through :attrs on the fieldset.
    - This is useful when changing the selected option should swap another
      region, reveal a dependent form fragment, or trigger server-side
      branching UI.
    - The fieldset includes data-radio-group=\"true\" and option rows include
      data-radio-option=\"true\" for easier targeting."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [options orientation required? disabled?]} props
          group-name  (:name props)
          horizontal? (= orientation :horizontal)
          group-class (class-names
                       (if horizontal?
                         "flex flex-wrap items-center gap-inline"
                         "flex flex-col gap-field")
                       class)]
      (el :fieldset
          {:class group-class}
          (merge-attrs
           attrs
           {:data-radio-group true}
           (when group-name {:data-name group-name})
           (when orientation {:data-orientation (name orientation)}))
          (map-indexed
           (fn [i {:keys [value label checked disabled]}]
             (let [id               (str (or group-name "radio-group") "-" i)
                   option-disabled? (or disabled disabled?)]
               [:label {:class (option-row-classes horizontal? option-disabled?)
                        :for id
                        :data-radio-option true
                        :data-value value
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
          {:keys [orientation name]} props
          group-class (class-names
                       (if (= orientation :horizontal)
                         "flex flex-wrap items-center gap-inline"
                         "flex flex-col gap-field")
                       class)]
      (el :fieldset
          {:class group-class}
          (merge-attrs
           attrs
           {:data-radio-group true}
           (when name {:data-name name})
           (when orientation {:data-orientation (name orientation)}))
          children))))
