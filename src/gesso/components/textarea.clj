(ns gesso.components.textarea
  (:require [gesso.util :refer :all]))

(defn textarea
  "Multiline textarea.

  Short form:
    (textarea {:id \"notes\"
               :name \"notes\"
               :rows 4
               :placeholder \"Add notes\"
               :text \"Initial value\"})

  Long form:
    (textarea {:attrs {...}} \"Initial value\")

  Uses Basecoat's .textarea class plus the shared control density utility.

  HTMX notes:
    - HTMX attributes may be passed through :attrs.
    - Common uses include live preview, autosave, inline validation, and
      server-rendered draft/analysis feedback."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [id name rows cols placeholder text value disabled? required? readonly?]} props]
      (el :textarea
          {:class (class-names "textarea control-theme" class)}
          (merge-attrs
           attrs
           {:data-textarea true}
           (when id {:id id})
           (when name {:name name})
           (when rows {:rows rows})
           (when cols {:cols cols})
           (when placeholder {:placeholder placeholder})
           (when (some? disabled?) {:disabled disabled?})
           (when (some? required?) {:required required?})
           (when (some? readonly?) {:readonly readonly?}))
          (nodes (or value text))))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [id name rows cols placeholder disabled? required? readonly?]} props]
      (el :textarea
          {:class (class-names "textarea control-theme" class)}
          (merge-attrs
           attrs
           {:data-textarea true}
           (when id {:id id})
           (when name {:name name})
           (when rows {:rows rows})
           (when cols {:cols cols})
           (when placeholder {:placeholder placeholder})
           (when (some? disabled?) {:disabled disabled?})
           (when (some? required?) {:required required?})
           (when (some? readonly?) {:readonly readonly?}))
          children))))
