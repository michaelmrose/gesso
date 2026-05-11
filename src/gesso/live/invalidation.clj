(ns gesso.live.invalidation
  "App-level invalidation expansion for gesso.live.

   This namespace turns primary changes into expanded invalidations.

   It does not:
   - inspect database writes
   - infer old/new state
   - emit to the source
   - know about SSE
   - know about Missionary
   - know about XTDB

   Writers declare primary changes.
   App rules declare what those changes imply.
   This namespace applies those rules."
  (:require
   [gesso.live.schema :as schema]))

;; -----------------------------------------------------------------------------
;; Defaults
;; -----------------------------------------------------------------------------

(def default-options
  {:on-unmatched :keep
   :dedupe? true
   :validate? true
   :validate-rules? true})

(def ^:private compiled-rules-key
  :gesso.live/compiled-rules?)

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn- opts
  [options]
  (merge default-options options))

(defn- ex
  [message data]
  (ex-info message data))

(defn compiled-rules?
  "Return true if rules were produced by compile-rules."
  [rules]
  (true? (get (meta rules) compiled-rules-key)))

(defn compile-rules
  "Validate and freeze invalidation rules.

   This is the preferred app setup path:

     (def rules
       (invalidation/compile-rules
        [{:when-topic :request
          :expand expand-request}]))

   compile-rules keeps debugging friendly because malformed rules fail at
   setup/REPL time instead of during a random live event."
  [rules]
  (let [rules' (vec (or rules []))]
    (schema/validate! :gesso.live/invalidation-rules rules')
    (with-meta rules' {compiled-rules-key true})))

(defn normalize-rules
  "Return a vector of rules.

   Raw rules are validated by default.
   Compiled rules are trusted.

   Options:
     :validate-rules? boolean

   This lets REPL/tests pass raw rules while production/app setup can compile
   once and avoid repeated validation."
  ([rules]
   (normalize-rules rules nil))
  ([rules options]
   (let [{:keys [validate-rules?]} (opts options)]
     (cond
       (compiled-rules? rules)
       rules

       validate-rules?
       (compile-rules rules)

       :else
       (vec (or rules []))))))

(defn- validate-primary-change!
  [change]
  (schema/validate-primary-change! change))

(defn- validation-error-data
  [schema-key value]
  {:schema-key schema-key
   :value value
   :humanized (schema/humanize schema-key value)
   :explanation (schema/explain-data schema-key value)})

(defn- valid-invalidation?
  [x]
  (schema/validate :gesso.live/invalidation x))

(defn- validate-invalidation-with-context!
  [invalidation context]
  (if (valid-invalidation? invalidation)
    invalidation
    (throw
     (ex "Invalid gesso.live invalidation."
         (merge context
                (validation-error-data :gesso.live/invalidation
                                       invalidation))))))

(defn- maybe-validate-invalidation!
  [validate? invalidation context]
  (if validate?
    (validate-invalidation-with-context! invalidation context)
    invalidation))

(defn- topic-matches?
  [rule change]
  (if (contains? rule :when-topic)
    (= (:when-topic rule) (:topic change))
    true))

(defn- predicate-matches?
  [rule ctx change]
  (if-let [pred (:when rule)]
    (try
      (boolean (pred ctx change))
      (catch Exception e
        (throw
         (ex-info "gesso.live invalidation rule predicate failed."
                  {:rule rule
                   :change change}
                  e))))
    true))

(defn rule-matches?
  "Return true if rule applies to change.

   If a rule contains both :when-topic and :when, both must match."
  [rule ctx change]
  (and (topic-matches? rule change)
       (predicate-matches? rule ctx change)))

(defn matching-rules
  "Return the rules that apply to change.

   Rules are evaluated in order."
  [rules ctx change]
  (->> rules
       (filter #(rule-matches? % ctx change))
       vec))

(defn- normalize-expansion-output
  [rule change output]
  (cond
    (nil? output)
    []

    (map? output)
    [output]

    (coll? output)
    (->> output
         (remove nil?)
         vec)

    :else
    (throw
     (ex "Invalid gesso.live invalidation expansion output."
         {:rule rule
          :change change
          :output output}))))

(defn- apply-rule
  [rule ctx change {:keys [validate?]}]
  (let [output (try
                 ((:expand rule) ctx change)
                 (catch Exception e
                   (throw
                    (ex-info "gesso.live invalidation rule expansion failed."
                             {:rule rule
                              :change change}
                             e))))
        raw    (normalize-expansion-output rule change output)]
    (mapv (fn [invalidation]
            (maybe-validate-invalidation!
             validate?
             invalidation
             {:rule rule
              :change change
              :output output}))
          raw)))

(defn- keep-unmatched
  [change {:keys [validate?]}]
  (if (or (not validate?)
          (valid-invalidation? change))
    [change]
    (throw
     (ex "Unmatched primary change cannot be kept as an invalidation."
         (merge
          {:change change
           :reason :primary-change-is-not-valid-invalidation
           :hint "Use :on-unmatched :drop/:throw, add an :id such as :main, or add an expansion rule."}
          (validation-error-data :gesso.live/invalidation change))))))

(defn- unmatched-output
  [change options]
  (case (:on-unmatched options)
    :keep
    (keep-unmatched change options)

    :drop
    []

    :throw
    (throw
     (ex "No gesso.live invalidation rule matched primary change."
         {:change change
          :on-unmatched (:on-unmatched options)}))

    (throw
     (ex "Unsupported gesso.live unmatched invalidation policy."
         {:change change
          :on-unmatched (:on-unmatched options)}))))

(defn- dedupe-invalidations
  [invalidations]
  (vec (distinct invalidations)))

(defn- finalize-invalidations
  [invalidations {:keys [dedupe?]}]
  (if dedupe?
    (dedupe-invalidations invalidations)
    (vec invalidations)))

(defn- validate-options!
  [options]
  ;; schema.clj intentionally captures the public invalidation options. validate?
  ;; and validate-rules? are implementation/debug switches in this namespace.
  (schema/validate! :gesso.live/invalidation-options
                    (select-keys options [:on-unmatched :dedupe?]))
  options)

;; -----------------------------------------------------------------------------
;; Public API
;; -----------------------------------------------------------------------------

(defn expand
  "Expand one primary change into canonical invalidations.

   Arity:
     (expand rules ctx change)
     (expand rules ctx change options)

   Rule shape:
     {:when-topic :request
      :expand (fn [ctx change] ...)}

     {:when (fn [ctx change] ...)
      :expand (fn [ctx change] ...)}

   If a rule has both :when-topic and :when, both must match.

   Expansion function output may be:
   - nil
   - one invalidation map
   - a collection of invalidation maps

   Options:
     :on-unmatched     :keep | :drop | :throw
     :dedupe?          boolean
     :validate?        boolean
     :validate-rules?  boolean

   Default:
     {:on-unmatched :keep
      :dedupe? true
      :validate? true
      :validate-rules? true}

   Important:

   :on-unmatched :keep means the primary change is reused as an invalidation.
   Therefore the primary change must also satisfy the invalidation schema. In
   practice that means it needs at least :topic and :id."
  ([rules ctx change]
   (expand rules ctx change nil))
  ([rules ctx change options]
   (let [options' (validate-options! (opts options))
         change'  (validate-primary-change! change)
         rules'   (normalize-rules rules options')
         matches  (matching-rules rules' ctx change')
         expanded (if (seq matches)
                    (mapcat #(apply-rule % ctx change' options') matches)
                    (unmatched-output change' options'))]
     (finalize-invalidations expanded options'))))

(defn expand-many
  "Expand many primary changes into canonical invalidations.

   Arity:
     (expand-many rules ctx changes)
     (expand-many rules ctx changes options)

   The final result is optionally deduped across all expanded changes."
  ([rules ctx changes]
   (expand-many rules ctx changes nil))
  ([rules ctx changes options]
   (let [options'           (validate-options! (opts options))
         rules'             (normalize-rules rules options')
         per-change-options (assoc options'
                                   :dedupe? false
                                   :validate-rules? false)
         expanded           (mapcat #(expand rules' ctx % per-change-options)
                                    changes)]
     (finalize-invalidations expanded options'))))

(defn expansion-rule
  "Construct and validate a topic-based expansion rule.

   This is a small convenience for app code and tests."
  [topic expand-fn]
  (first
   (compile-rules
    [{:when-topic topic
      :expand expand-fn}])))

(defn predicate-rule
  "Construct and validate a predicate-based expansion rule.

   This is a small convenience for app code and tests."
  [pred expand-fn]
  (first
   (compile-rules
    [{:when pred
      :expand expand-fn}])))

(defn simple-invalidation
  "Build a canonical invalidation map.

   Optional kind defaults to :updated."
  ([topic id]
   (simple-invalidation topic id :updated))
  ([topic id kind]
   (validate-invalidation-with-context!
    {:topic topic
     :id id
     :change/kind kind}
    {:topic topic
     :id id
     :change/kind kind})))
