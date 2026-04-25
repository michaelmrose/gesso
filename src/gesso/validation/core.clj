(ns gesso.validation.core
  "The orchestration layer for Gesso validation.

   This namespace is the small public contract consumed by field components.
   It combines Malli constraint extraction with generated client-side
   Hyperscript gatekeeper behavior."
  (:require
   [gesso.validation.malli :as vmalli]
   [gesso.validation.scripts :as vscripts]))

(defn- clean-attrs
  "Remove nil values from an HTML attr map.

   This prevents empty or unresolved validation attrs from being rendered onto
   controls."
  [attrs]
  (into {}
        (remove (comp nil? val))
        attrs))

(defn- normalize-field-key
  [field-key]
  (cond
    (keyword? field-key) field-key
    (string? field-key) (keyword field-key)
    :else field-key))

(defn empty-field-plan
  "A validation plan with no attrs and no client script."
  []
  {:attrs {}
   :script nil
   :constraints nil})

(defn field-plan
  "Build the validation plan for a single field.

   Inputs:
     schema    Malli schema, usually a top-level :map schema.
     field-key Direct field key inside the schema.
     err-id    DOM id of the field's error container.

   Returns:
     {:attrs       HTML5 validation attrs to merge onto the control
      :script      Hyperscript gatekeeper script, or nil
      :constraints Raw extracted validation constraints}

   The field component should merge :attrs into the control and append :script
   to the control's existing :_ attribute when present."
  [schema field-key err-id]
  (if-not schema
    (empty-field-plan)
    (let [field-key   (normalize-field-key field-key)
          constraints (vmalli/extract-constraints schema field-key)
          attrs       (clean-attrs (:rules constraints))
          script      (when (and err-id (seq attrs))
                        (vscripts/gatekeeper-script
                         (:messages constraints)
                         err-id))]
      {:attrs attrs
       :script script
       :constraints constraints})))
