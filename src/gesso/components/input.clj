(ns gesso.components.input
  (:require [gesso.util :refer :all]))

(defn input
  "Single-line input.

  Short form:
    (input {:type \"email\"
            :id \"email\"
            :name \"email\"
            :value \"a@b.com\"
            :placeholder \"Enter email\"
            :required? true})

  Long form:
    (input {:type \"text\" :attrs {...}})

  Uses Basecoat's .input class plus the shared control density utility.

  HTMX notes:
    - HTMX attributes may be passed through :attrs.
    - Common uses include live validation, autocomplete, search, preview, and
      incremental server-side form feedback."
  [& args]
  (let [[opts _children] (normalize-component-args args)
        {:keys [props class attrs]} (split-opts opts)
        {:keys [type id name value placeholder disabled? required? autocomplete
                readonly? checked min max step]} props
        type (or type "text")]
    (el :input
        {:class (class-names "input control-theme" class)
         :type type}
        (merge-attrs
         attrs
         {:data-input true}
         (when id {:id id})
         (when name {:name name})
         (when (some? value) {:value value})
         (when placeholder {:placeholder placeholder})
         (when autocomplete {:autocomplete autocomplete})
         (when (some? disabled?) {:disabled disabled?})
         (when (some? required?) {:required required?})
         (when (some? readonly?) {:readonly readonly?})
         (when (some? checked) {:checked checked})
         (when (some? min) {:min min})
         (when (some? max) {:max max})
         (when (some? step) {:step step}))
        [])))
