(ns gesso.components.textarea
  (:require [gesso.util :refer :all]))

(defn- spellcheck-value
  [x]
  (cond
    (true? x) "true"
    (false? x) "false"
    :else x))

(defn- normalize-textarea-props
  [props]
  (cond-> (dissoc props :text :value :disabled? :required? :readonly?)
    (contains? props :spellcheck)
    (assoc :spellcheck (spellcheck-value (:spellcheck props)))

    (contains? props :disabled?)
    (assoc :disabled (:disabled? props))

    (contains? props :required?)
    (assoc :required (:required? props))

    (contains? props :readonly?)
    (assoc :readonly (:readonly? props))))

(defn- textarea-content
  [props children short-form?]
  (if short-form?
    (nodes (or (:value props) (:text props)))
    children))

(defn textarea
  "Multiline textarea.

  Short form:
    (textarea {:id \"notes\"
               :name \"notes\"
               :rows 4
               :placeholder \"Add notes\"
               :text \"Initial value\"})

    (textarea {:name \"notes\"
               :value \"Initial value\"})

  Long form:
    (textarea {:attrs {...}} \"Initial value\")

  Any non-component props are passed through as HTML attributes.

  Component content props:
    :value
    :text

  Gesso aliases:
    :disabled? -> :disabled
    :required? -> :required
    :readonly? -> :readonly

  Escape hatch:
    :attrs is merged last, so callers can override low-level attrs when needed.

  Uses Basecoat's .textarea class plus the shared control density utility.

  HTMX notes:
    - HTMX attributes may be passed through directly or through :attrs.
    - Common uses include live preview, autosave, inline validation, and
      server-rendered draft/analysis feedback."
  [& args]
  (let [short-form?                  (only-map-arg? args)
        [opts children]              (normalize-component-args args)
        {:keys [props class attrs]}  (split-opts opts)
        textarea-attrs               (normalize-textarea-props props)
        content                      (textarea-content props children short-form?)]
    (el :textarea
        {:class (class-names "textarea control-theme" class)}
        (merge-attrs
         {:data-textarea true}
         textarea-attrs
         attrs)
        content)))
