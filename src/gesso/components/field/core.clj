(ns gesso.components.field.core
  "Field wrapper for label, control, description, error, and validation wiring."
  (:require
   [gesso.components.field.attr :as attr]
   [gesso.components.field.scripts :as field-scripts]
   [gesso.components.label :as label]
   [gesso.components.text :as text]
   [gesso.validation.core :as validation]
   [gesso.util :refer :all]))

(defn- validation-plan?
  [plan]
  (boolean
   (or (seq (:attrs plan))
       (seq (:script plan)))))

(defn- join-scripts
  [a b]
  (cond
    (and (seq a) (seq b)) (str a "\n" b)
    (seq a) a
    (seq b) b
    :else nil))

(defn- add-recovery-script
  [plan err-id error-target?]
  (let [recovery-script (when error-target?
                          (field-scripts/ui-recovery-script err-id))]
    (if recovery-script
      (update plan :script join-scripts recovery-script)
      plan)))

(defn- plan-for-field
  [{:keys [schema field-key for]} {:keys [err-id]}]
  (if schema
    (validation/field-plan schema (or field-key for) err-id)
    (validation/empty-field-plan)))

(defn- required-from-plan?
  [plan]
  (boolean
   (or (get-in plan [:attrs :required])
       (get-in plan [:constraints :required]))))

(defn- render-label
  [{:keys [label-text required?]} ids plan]
  (when label-text
    (label/label
     {:text label-text
      :for (:id ids)
      :required? (boolean (or required?
                              (required-from-plan? plan)))})))

(defn- render-description
  [description ids]
  (when description
    (text/text
     {:variant :caption
      :as :p
      :attrs (attr/description-attrs nil ids)
      :text description})))

(defn- render-error
  [error ids error-target?]
  (when error-target?
    (text/text
     {:variant :caption
      :as :div
      :attrs (attr/error-attrs nil
                               (assoc ids :hidden? (not error)))
      :text error})))

(defn- control-row
  [control indicator]
  (if indicator
    (el :div
        (attr/control-row-attrs nil)
        {}
        [control indicator])
    control))

(defn- error-target?
  "Whether this field should render a stable error container.

   Active errors obviously need it.

   Local browser validation needs it because the gatekeeper writes into it.

   Schema-backed fields also need it, even when there are no local browser attrs,
   so server-side HTMX OOB validation has a stable target."
  [{:keys [schema]} ids plan error]
  (boolean
   (and (:err-id ids)
        (or error
            schema
            (validation-plan? plan)))))

(defn- render-short-form
  [opts]
  (let [{:keys [props class attrs]} (split-opts opts)
        {:keys [for control indicator description error
                orientation valid? invalid?] :as props} props
        ids           (attr/derive-ids for)
        plan          (plan-for-field props ids)
        error-target? (error-target? props ids plan error)
        plan          (add-recovery-script plan (:err-id ids) error-target?)
        invalid?      (boolean (or invalid? error))
        root-state    {:for for
                       :orientation orientation
                       :valid? valid?
                       :invalid? invalid?}
        control       (attr/annotate-control
                       control
                       ids
                       plan
                       {:description? (boolean description)
                        :error? (boolean error)
                        :invalid? invalid?
                        :error-target? error-target?})]
    (el :div
        (attr/field-root-attrs class attrs root-state)
        {}
        [(render-label props ids plan)
         (control-row control indicator)
         (render-description description ids)
         (render-error error ids error-target?)])))

(defn- render-long-form
  [opts children]
  (let [{:keys [props class attrs]} (split-opts opts)
        {:keys [for orientation valid? invalid? error]} props
        invalid? (boolean (or invalid? error))]
    (el :div
        (attr/field-root-attrs
         class
         attrs
         {:for for
          :orientation orientation
          :valid? valid?
          :invalid? invalid?})
        {}
        children)))

(defn field
  "Field wrapper for label + control + description + error.

  Short form:
    (field {:label-text \"Email\"
            :for :email
            :schema UserSchema
            :control (input {:type \"email\" :name \"email\"})
            :description \"We will never share it.\"
            :error nil})

  If :schema is supplied, the field derives a validation plan and annotates the
  control with HTML5 validation attrs, ARIA wiring, and a Hyperscript
  gatekeeper when local browser validation attrs exist.

  Schema-backed fields always render a stable hidden error container when an
  error id can be derived, even if the schema produces no local browser attrs.
  This gives server-side HTMX OOB validation a target.

  Use :field-key when the schema key differs from the rendered DOM id:

    (field {:for \"user-email\"
            :field-key :email
            :schema UserSchema
            :control ...})

  Long form:
    (field {:orientation :horizontal}
      (label/label {:text \"Email\" :for \"email\"})
      (input {:type \"email\" :id \"email\"}))

  Long form is a structural wrapper only. It does not inspect or mutate child
  controls."
  [& args]
  (if (only-map-arg? args)
    (render-short-form (first args))
    (let [[opts children] (normalize-component-args args)]
      (render-long-form opts children))))
