(ns gesso.components.form.attr
  "Manages HTMX validation attributes for the form container.")

(defn validation-attrs
  "Generates the HTMX directives required to debounce validation requests
   and prevent cross-field race conditions."
  [validate-url]
  {:hx-trigger "validateField delay:250ms"
   :hx-sync "closest [data-field]:drop"})
