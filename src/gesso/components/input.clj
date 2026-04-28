(ns gesso.components.input
  (:require [gesso.util :refer :all]))

(defn- spellcheck-value
  [x]
  (cond
    (true? x) "true"
    (false? x) "false"
    :else x))

(defn- normalize-input-props
  [props]
  (let [type (or (:type props) "text")]
    (cond-> (-> props
                (dissoc :disabled? :required? :readonly?)
                (assoc :type type)
                (update :spellcheck spellcheck-value))
      (contains? props :disabled?)
      (assoc :disabled (:disabled? props))

      (contains? props :required?)
      (assoc :required (:required? props))

      (contains? props :readonly?)
      (assoc :readonly (:readonly? props)))))

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

  Any non-component props are passed through as HTML attributes.

  Gesso aliases:
    :disabled? -> :disabled
    :required? -> :required
    :readonly? -> :readonly

  Escape hatch:
    :attrs is merged last, so callers can override low-level attrs when needed.

  Uses Basecoat's .input class plus the shared control density utility.

  HTMX notes:
    - HTMX attributes may be passed through directly or through :attrs.
    - Common uses include live validation, autocomplete, search, preview, and
      incremental server-side form feedback."
  [& args]
  (let [[opts _children] (normalize-component-args args)
        {:keys [props class attrs]} (split-opts opts)
        input-attrs (normalize-input-props props)]
    (el :input
        {:class (class-names "input control-theme" class)}
        (merge-attrs
         {:data-input true}
         input-attrs
         attrs)
        [])))
