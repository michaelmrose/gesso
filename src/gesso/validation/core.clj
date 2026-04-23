(ns gesso.validation.core
  "The orchestration layer for Gesso validation."
  (:require [gesso.validation.malli :as vmalli]
            [gesso.validation.scripts :as vscripts]))

(defn field-plan
  "The primary contract for UI components.
   Takes a schema, field keyword, and the target error DOM ID.
   Returns a map containing HTML5 attributes and the Hyperscript gatekeeper."
  [schema field-kw err-id]
  (if-not schema
    {:attrs {} :script ""}
    (let [constraints (vmalli/extract-constraints schema field-kw)
          rules       (:rules constraints)
          messages    (:messages constraints)

          ;; Filter out nil values from rules so we don't render empty HTML attrs
          html5-attrs (into {} (filter (comp some? val) rules))

          ;; Generate the Hyperscript using the resolved messages
          hs-script   (vscripts/gatekeeper-script messages err-id)]

      {:attrs  html5-attrs
       :script hs-script})))
