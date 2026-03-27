(ns gesso.components.field
  (:require [clojure.string :as str]
            [gesso.util :refer :all]
            [gesso.components.label :as label]
            [gesso.components.text :as text]))

(defn- description-id
  [for]
  (when for
    (str for "-description")))

(defn- error-id
  [for]
  (when for
    (str for "-error")))

(defn- token-list
  [& xs]
  (let [tokens (->> xs
                    (remove nil?)
                    (mapcat #(str/split (str %) #"\s+"))
                    (remove str/blank?)
                    distinct)]
    (when (seq tokens)
      (str/join " " tokens))))

(defn- annotate-control
  "Adds stable field-related accessibility attrs to a Hiccup control node when
   possible. Non-Hiccup controls are returned unchanged.

   When a :for value is present on the field:
   - description gets <for>-description
   - error gets <for>-error

   The control is then wired with aria-describedby and aria-errormessage as
   appropriate so HTMX-swapped field fragments remain accessible."
  [control {:keys [desc-id err-id invalid?]}]
  (if (and (vector? control) (keyword? (first control)))
    (let [[tag maybe-attrs & rest] control
          has-attrs?   (map? maybe-attrs)
          control-attrs (if has-attrs? maybe-attrs {})
          children      (if has-attrs? rest (cons maybe-attrs rest))
          describedby   (token-list (:aria-describedby control-attrs)
                                    desc-id
                                    (when invalid? err-id))
          control-attrs (cond-> (merge-attrs control-attrs {:data-field-control true})
                          invalid? (assoc :aria-invalid "true")
                          describedby (assoc :aria-describedby describedby)
                          (and invalid? err-id) (assoc :aria-errormessage err-id))]
      (into [tag control-attrs] children))
    control))

(defn- control-row
  "Wraps the control and optional indicator in a small inline layout when an
   indicator is supplied. This works well for HTMX inline validation spinners
   and similar affordances."
  [control indicator]
  (if indicator
    [:div {:class "flex items-center gap-inline"}
     control
     indicator]
    control))

(defn field
  "Field wrapper for label + control + description + error.

  Short form:
    (field {:label-text \"Email\"
            :for \"email\"
            :required? true
            :control (input {:type \"email\" :id \"email\" :name \"email\"})
            :description \"We will never share it.\"
            :error nil})

  Long form:
    (field {:orientation :horizontal}
      (label/label {:text \"Email\" :for \"email\"})
      (input {:type \"email\" :id \"email\"}))

  Extra short-form options:
    :indicator  A node rendered beside the control, useful for HTMX indicators.
    :valid?     Marks the field as valid with data-valid=\"true\".
    :invalid?   Marks the field as invalid with data-invalid=\"true\".
                Error content also implies invalid state.

  HTMX notes:
    - The field root is intended to be a good fragment boundary for inline
      validation and partial form updates.
    - HTMX attributes usually belong on the control itself, passed through that
      control's :attrs.
    - When :for is present, description and error ids are derived from it and
      attached to the control where possible."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [label-text for required? control indicator description error
                  orientation valid? invalid?]} props
          invalid?    (boolean (or invalid? error))
          desc-id     (description-id for)
          err-id      (error-id for)
          control     (annotate-control control {:desc-id desc-id
                                                 :err-id err-id
                                                 :invalid? invalid?})]
      (el :div
          {:class (class-names "flex flex-col gap-field" class)}
          (merge-attrs
           attrs
           {:data-field true}
           (when for {:data-field-for for})
           (when orientation {:data-orientation (name orientation)})
           (when valid? {:data-valid "true"})
           (when invalid? {:data-invalid "true"}))
          [(when label-text
             (label/label
              {:text label-text
               :for for
               :required? required?}))
           (control-row control indicator)
           (when description
             (text/text
              {:variant :caption
               :as :p
               :attrs (merge-attrs
                       (when desc-id {:id desc-id}))
               :text description}))
           (when error
             (text/text
              {:variant :caption
               :as :div
               :attrs (merge-attrs
                       {:role "alert"
                        :aria-live "polite"}
                       (when err-id {:id err-id}))
               :style {:color "var(--destructive)"}
               :text error}))]))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [orientation valid? invalid? for error]} props
          invalid? (boolean (or invalid? error))]
      (el :div
          {:class (class-names "flex flex-col gap-field" class)}
          (merge-attrs
           attrs
           {:data-field true}
           (when for {:data-field-for for})
           (when orientation {:data-orientation (name orientation)})
           (when valid? {:data-valid "true"})
           (when invalid? {:data-invalid "true"}))
          children))))
