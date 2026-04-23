(ns gesso.validation.malli
  "The translation engine between Malli schemas and HTML5/UI representations."
  (:require [malli.core :as m]
            [malli.util :as mu]))

;; Maps Malli property keys to standard HTML5 attribute names
(def ^:private html5-prop-map
  {:min :minlength
   :max :maxlength
   :re  :pattern})

;; Fallback messages if the schema author didn't provide custom ones
(def ^:private default-messages
  {:minlength "Value is too short."
   :maxlength "Value is too long."
   :pattern   "Format is invalid."
   :required  "This field is required."})

(defn- extract-field-schema
  "Safely attempts to extract a specific field's schema from a larger map schema."
  [schema field-kw]
  (try
    (mu/get schema field-kw)
    (catch Exception _
      ;; mu/get throws if the schema isn't a map or the key isn't found
      nil)))

(defn extract-constraints
  "Walks a Malli schema to extract HTML5-compatible rules and custom error messages
   for a specific field keyword.

   Looks for custom errors in this order:
   1. :gesso.error/<type> (e.g., :gesso.error/min)
   2. :error/<type>       (e.g., :error/min)
   3. Gesso library defaults"
  [schema field-kw]
  (if-let [field-schema (extract-field-schema schema field-kw)]
    (let [props    (m/properties field-schema)
          type-tag (m/type field-schema)

          ;; 1. Build the rules map for HTML5 attributes
          rules    (reduce-kv
                    (fn [acc k v]
                      (if-let [html5-k (get html5-prop-map k)]
                        ;; HTML5 patterns must be strings, not Clojure regex objects
                        (assoc acc html5-k (if (= k :re) (str v) v))
                        acc))
                    ;; In Malli, map keys are required unless wrapped in :optional
                    ;; We default to true here, but a robust implementation might need
                    ;; to check the parent map's AST to be 100% sure.
                    {:required? true}
                    props)

          ;; 2. Build the messages map corresponding to the rules
          messages (reduce
                    (fn [acc rule-k]
                      (let [;; Map HTML5 key back to Malli key for lookup (e.g., :minlength -> :min)
                            malli-k (get (clojure.set/map-invert html5-prop-map) rule-k)
                            msg     (or (get props (keyword "gesso.error" (name malli-k)))
                                        (get props (keyword "error" (name malli-k)))
                                        (get default-messages rule-k))]
                        (if msg (assoc acc rule-k msg) acc)))
                    {}
                    (keys (dissoc rules :required?)))]

      {:rules    (dissoc rules :required?)
       :required (:required? rules)
       :messages (assoc messages :required
                        (or (:gesso.error/required props)
                            (:error/message props)
                            (:required default-messages)))
       :type     type-tag})

    ;; Fallback if the field isn't in the schema
    {:rules {} :required false :messages {} :type nil}))
