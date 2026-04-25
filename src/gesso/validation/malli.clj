(ns gesso.validation.malli
  "The translation engine between Malli schemas and HTML5/UI representations."
  (:require
   [malli.core :as m]))

(def ^:private default-messages
  {:minlength "Value is too short."
   :maxlength "Value is too long."
   :pattern   "Format is invalid."
   :min       "Value is too small."
   :max       "Value is too large."
   :required  "This field is required."})

(def ^:private string-like-types
  #{:string})

(def ^:private numeric-like-types
  #{:int :int8 :int16 :int32 :int64
    :long :float :double :number :integer
    :nat :pos-int :neg-int :pos :neg})

(defn- schema-form
  [schema]
  (try
    (m/form schema)
    (catch Exception _
      schema)))

(defn- map-entry-parts
  [entry]
  (cond
    (and (vector? entry) (= 2 (count entry)))
    {:key (nth entry 0)
     :entry-props {}
     :field-form (nth entry 1)}

    (and (vector? entry) (= 3 (count entry)) (map? (nth entry 1)))
    {:key (nth entry 0)
     :entry-props (nth entry 1)
     :field-form (nth entry 2)}

    :else
    nil))

(defn- map-entries
  [schema]
  (let [form (schema-form schema)]
    (when (and (vector? form)
               (= :map (first form)))
      (let [[_ maybe-props & entries] form]
        (if (map? maybe-props)
          entries
          (cons maybe-props entries))))))

(defn- field-entry
  [schema field-kw]
  (some (fn [entry]
          (let [{:keys [key] :as parts} (map-entry-parts entry)]
            (when (= key field-kw)
              parts)))
        (map-entries schema)))

(defn- form-props
  [form]
  (if (and (vector? form) (map? (second form)))
    (second form)
    {}))

(defn- form-children
  [form]
  (cond
    (and (vector? form) (map? (second form)))
    (drop 2 form)

    (vector? form)
    (drop 1 form)

    :else
    nil))

(defn- unwrap-maybe
  "Unwraps simple [:maybe ...] schemas.

   Returns:
     {:form          inner-form
      :nilable?      boolean
      :wrapper-props merged-props-from-wrappers}"
  [field-form]
  (loop [form (schema-form field-form)
         nilable? false
         wrapper-props {}]
    (if (and (vector? form)
             (= :maybe (first form)))
      (let [props (form-props form)
            inner (first (form-children form))]
        (recur inner
               true
               (merge wrapper-props props)))
      {:form form
       :nilable? nilable?
       :wrapper-props wrapper-props})))

(defn- incompatible-pattern-reasons
  [pattern]
  (cond-> []
    (re-find #"\\[AGZz]" pattern)
    (conj "Java anchors like \\A, \\G, \\Z, and \\z are not portable to HTML pattern regexes.")

    (re-find #"\\Q|\\E" pattern)
    (conj "Java quote escapes \\Q and \\E are not supported in HTML pattern regexes.")

    (re-find #"\(\?>" pattern)
    (conj "Java atomic groups like (?>...) are not supported in JavaScript/HTML pattern regexes.")

    (re-find #"\(\?[idmsuxU-]" pattern)
    (conj "Java inline regex flags like (?i) are not supported in HTML pattern attributes.")

    (re-find #"\+\+|\*\+|\?\+|\{\d+(,\d*)?\}\+" pattern)
    (conj "Java possessive quantifiers like *+, ++, ?+, and {m,n}+ are not supported in HTML pattern regexes.")

    (re-find #"&&" pattern)
    (conj "Java character-class intersection using && is not portable to HTML pattern regexes.")

    (re-find #"\\R" pattern)
    (conj "Java \\R linebreak matching is not supported in HTML pattern regexes.")))

(defn- assert-browser-compatible-pattern!
  [pattern]
  (let [reasons (incompatible-pattern-reasons pattern)]
    (when (seq reasons)
      (throw
       (ex-info
        "Malli :re cannot be safely emitted as an HTML pattern."
        {:pattern pattern
         :reasons reasons
         :hint "Provide :gesso.html/pattern for browser validation, or simplify the regex."}))))
  pattern)

(defn- java-regex->pattern
  [v]
  (let [pattern (cond
                  (instance? java.util.regex.Pattern v)
                  (.pattern ^java.util.regex.Pattern v)

                  (string? v)
                  v

                  :else
                  (str v))]
    (assert-browser-compatible-pattern! pattern)))

(defn- browser-pattern
  [props]
  (or (:gesso.html/pattern props)
      (:html/pattern props)
      (when-let [re (:re props)]
        (java-regex->pattern re))))

(defn- rule-key
  [type-tag prop-k]
  (case prop-k
    :min (if (contains? string-like-types type-tag) :minlength :min)
    :max (if (contains? string-like-types type-tag) :maxlength :max)
    :pattern :pattern
    nil))

(defn- supported-prop?
  [type-tag prop-k]
  (case prop-k
    :min (or (contains? string-like-types type-tag)
             (contains? numeric-like-types type-tag))
    :max (or (contains? string-like-types type-tag)
             (contains? numeric-like-types type-tag))
    :pattern true
    false))

(defn- build-source-props
  [props]
  (cond-> (dissoc props :re)
    (browser-pattern props)
    (assoc :pattern (browser-pattern props))))

(defn- build-rules
  [type-tag props required?]
  (let [source-props (build-source-props props)
        rules        (reduce-kv
                      (fn [acc prop-k v]
                        (if (supported-prop? type-tag prop-k)
                          (assoc acc (rule-key type-tag prop-k) v)
                          acc))
                      {}
                      source-props)]
    (cond-> rules
      required? (assoc :required true))))

(defn- message-aliases
  [rule-k]
  (case rule-k
    :minlength [:minlength :min]
    :maxlength [:maxlength :max]
    :pattern   [:pattern :re]
    [rule-k]))

(defn- specific-message
  [props msg-k]
  (or (get props (keyword "gesso.error" (name msg-k)))
      (get props (keyword "error" (name msg-k)))))

(defn- fallback-message
  [props rule-k]
  (or (some #(specific-message props %) (message-aliases rule-k))
      (:error/message props)
      (get default-messages rule-k)))

(defn- build-messages
  [rules props]
  (reduce-kv
   (fn [acc rule-k _]
     (if-let [msg (fallback-message props rule-k)]
       (assoc acc rule-k msg)
       acc))
   {}
   rules))

(defn extract-constraints
  "Walk a top-level Malli map schema to extract HTML5-compatible rules and
   custom error messages for one direct field keyword.

   Browser pattern policy:
     :gesso.html/pattern or :html/pattern wins over :re.

   This allows :re to remain the backend/Malli validation truth while the HTML
   pattern is used only as a browser-side shortcut for obviously bad input."
  [schema field-kw]
  (if-let [{:keys [entry-props field-form]} (field-entry schema field-kw)]
    (let [{:keys [form nilable? wrapper-props]} (unwrap-maybe field-form)
          field-schema (m/schema form)
          inner-props  (m/properties field-schema)
          props        (merge wrapper-props inner-props)
          type-tag     (m/type field-schema)
          required?    (and (not (:optional entry-props))
                            (not nilable?))
          rules        (build-rules type-tag props required?)
          messages     (build-messages rules props)]
      {:rules rules
       :required required?
       :messages messages
       :type type-tag
       :nilable? nilable?})
    {:rules {}
     :required false
     :messages {}
     :type nil
     :nilable? false}))
