(ns gesso.components.field.attr
  "Attribute helpers for the Gesso field component.

   This namespace owns stable field ids, ARIA wiring, and safe annotation of
   user-provided Hiccup controls."
  (:require
   [clojure.string :as str]
   [gesso.util :refer [class-names hiccup-element? merge-attrs]]))

(defn- ->id-part
  [x]
  (cond
    (nil? x) nil
    (keyword? x) (name x)
    (symbol? x) (name x)
    :else (str x)))

(defn token-list
  "Build an ARIA token list from possibly nil/string/sequential values."
  [& xs]
  (let [tokens (->> xs
                    (mapcat (fn [x]
                              (cond
                                (nil? x) []
                                (sequential? x) x
                                :else [x])))
                    (map str)
                    (mapcat #(str/split % #"\s+"))
                    (remove str/blank?)
                    distinct)]
    (when (seq tokens)
      (str/join " " tokens))))

(defn derive-ids
  "Derive stable ids from a field/control id.

   Example:
     (derive-ids :email)

   Returns:
     {:id \"email\"
      :desc-id \"email-description\"
      :err-id \"email-error\"}

   If for-id is nil, all ids are nil."
  [for-id]
  (let [id (->id-part for-id)]
    {:id id
     :desc-id (when id (str id "-description"))
     :err-id (when id (str id "-error"))}))

(defn- validation-plan?
  [plan]
  (boolean
   (or (seq (:attrs plan))
       (seq (:script plan)))))

(defn build-aria
  "Build ARIA attrs for a control.

   Options:
     :existing-describedby  existing aria-describedby value from the control
     :description?          whether a description container is rendered
     :error?                whether an active error is currently rendered
     :invalid?              whether the field is currently invalid
     :validation?           whether local browser validation behavior exists
     :error-target?         whether a stable error target exists for server/OOB errors

   When validation? or error-target? is true, aria-errormessage is attached up
   front so later scripts/OOB updates can mark the control invalid without also
   needing to patch ARIA relationships."
  [{:keys [desc-id err-id]}
   {:keys [existing-describedby description? error? invalid?
           validation? error-target?]}]
  (let [invalid?      (boolean invalid?)
        active-error? (boolean (or error? invalid?))
        error-link?   (boolean (or validation? error-target? active-error?))
        describedby   (token-list
                        existing-describedby
                        (when description? desc-id)
                        (when active-error? err-id))]
    (cond-> {}
      describedby
      (assoc :aria-describedby describedby)

      (and err-id error-link?)
      (assoc :aria-errormessage err-id)

      error-link?
      (assoc :aria-invalid (if invalid? "true" "false")))))

(defn- join-scripts
  [existing script]
  (cond
    (and (seq existing) (seq script))
    (str existing "\n" script)

    (seq existing)
    existing

    (seq script)
    script

    :else
    nil))

(defn- merge-script
  [attrs script]
  (let [script' (join-scripts (:_ attrs) script)]
    (cond-> attrs
      script' (assoc :_ script'))))

(defn- split-control-node
  [control]
  (let [[tag maybe-attrs & rest] control
        has-attrs? (map? maybe-attrs)
        attrs      (if has-attrs? maybe-attrs {})
        children   (if has-attrs? rest (cons maybe-attrs rest))]
    {:tag tag
     :attrs attrs
     :children children}))

(defn annotate-control
  "Add field, validation, and accessibility attrs to a Hiccup control.

   Arguments:
     control  Hiccup element vector, usually input/textarea/select.
     ids      result of derive-ids.
     plan     result of gesso.validation.core/field-plan.
     state    map with :invalid?, :description?, :error?, and :error-target?.

   The control id is forced to (:id ids) when present so labels, descriptions,
   errors, and server OOB validation all agree on one stable convention.

   Existing control scripts are preserved and the validation script is appended."
  [control ids plan state]
  (if (hiccup-element? control)
    (let [{:keys [tag attrs children]} (split-control-node control)
          validation?       (validation-plan? plan)
          validation-attrs  (:attrs plan)
          validation-script (:script plan)
          aria              (build-aria ids
                                        {:existing-describedby (:aria-describedby attrs)
                                         :description? (:description? state)
                                         :error? (:error? state)
                                         :invalid? (:invalid? state)
                                         :validation? validation?
                                         :error-target? (:error-target? state)})
          merged-attrs      (merge-attrs
                              attrs
                              validation-attrs
                              aria
                              {:data-field-control true}
                              (when-let [id (:id ids)]
                                {:id id}))
          merged-attrs      (merge-script merged-attrs validation-script)]
      (into [tag merged-attrs] children))
    control))

(defn field-root-attrs
  [class attrs {:keys [for orientation valid? invalid?]}]
  (merge-attrs
   {:class (class-names "flex flex-col gap-field" class)
    :data-field true}
   attrs
   (when for
     {:data-field-for (->id-part for)})
   (when orientation
     {:data-orientation (name orientation)})
   (when valid?
     {:data-valid "true"})
   (when invalid?
     {:data-invalid "true"})))

(defn control-row-attrs
  [class]
  {:class (class-names "flex items-center gap-inline" class)
   :data-field-control-row true})

(defn description-attrs
  [class {:keys [desc-id]}]
  (merge-attrs
   {:class (class-names class)
    :data-field-description true}
   (when desc-id
     {:id desc-id})))

(defn error-attrs
  [class {:keys [err-id hidden?]}]
  (merge-attrs
   {:class (class-names (when hidden? "hidden") class)
    :data-field-error true
    :role "alert"
    :aria-live "polite"
    :style {:color "var(--destructive)"}}
   (when err-id
     {:id err-id})))
