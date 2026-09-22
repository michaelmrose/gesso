(ns gesso.choreo.preflight
  "JVM-side whole-registry preflight for Gesso Choreo executable plans.

   Projection proves properties of one choreography and one role-local
   ExecutablePlan at a time. Applications additionally assemble logical
   operation registries, browser bundles, generated artifacts, and runtime role
   ownership across namespace boundaries. Those composition contracts need one
   reusable build-time gate rather than application-specific macros.

   This namespace validates the first deliberately small slice of that assembly
   boundary:

   - every logical registry key is a keyword;
   - every registered value is a canonical current ExecutablePlan;
   - required logical keys and actual plan keys agree exactly when declared;
   - plans may be constrained to one physical role, or to one expected role;
   - generated/downstream artifacts may supply expected SHA-256 ExecutablePlan
     digests, making stale plan registries a build-time error;
   - successful preflight emits one closed portable PlanRegistry value suitable
     for compile-time embedding into ClojureScript.

   Preflight does not reinterpret choreography semantics, authorization,
   authoritative knowledge, transport policy, or UI affordance policy. Those
   belong to later assembly checks built on top of this registry boundary."
  (:require
   [clojure.set :as set]
   [gesso.choreo.artifact :as artifact]
   [gesso.choreo.project :as project]))

;; -----------------------------------------------------------------------------
;; Identity
;; -----------------------------------------------------------------------------

(def preflight-version
  1)

(def report-type
  :gesso.choreo.preflight/report)

(def plan-registry-type
  :gesso.choreo.preflight/plan-registry)

(def ^:private report-keys
  #{:gesso.choreo/type
    :gesso.choreo/version
    :valid?
    :errors
    :warnings
    :inputs
    :analysis})

(def ^:private analysis-keys
  #{:name
    :plan-keys
    :roles
    :digests})

(def ^:private plan-registry-keys
  #{:gesso.choreo/type
    :gesso.choreo/version
    :name
    :roles
    :plans
    :digests})

(def ^:private option-keys
  #{:name
    :plans
    :required-keys
    :single-role?
    :expected-role
    :expected-digests
    :allow-empty?})

;; -----------------------------------------------------------------------------
;; Errors / helpers
;; -----------------------------------------------------------------------------

(defn- preflight-error
  [kind message data]
  (ex-info
   message
   (merge
    {:error/type :gesso.choreo.preflight/error
     :error/kind kind}
    data)))

(defn- issue
  [kind message data]
  (merge
   {:kind kind
    :message message}
   data))

(defn- sha-256-hex?
  [value]
  (and
   (string? value)
   (boolean
    (re-matches #"[0-9a-f]{64}"
                value))))

(defn- keyword-set?
  [value]
  (and
   (set? value)
   (every? keyword? value)))

(defn- keyword-digest-map?
  [value]
  (and
   (map? value)
   (every?
    (fn [[logical-key digest]]
      (and
       (keyword? logical-key)
       (sha-256-hex? digest)))
    value)))

(defn- validate-options!
  [options]
  (when-not (map? options)
    (throw
     (preflight-error
      :invalid-options
      "Gesso Choreo plan-registry preflight options must be a map."
      {:options options})))

  (let [unknown
        (set/difference
         (set (keys options))
         option-keys)]
    (when (seq unknown)
      (throw
       (preflight-error
        :unknown-options
        "Gesso Choreo plan-registry preflight received unsupported options."
        {:unknown-keys unknown
         :allowed-keys option-keys}))))

  (when-not (contains? options :plans)
    (throw
     (preflight-error
      :missing-plans
      "Gesso Choreo plan-registry preflight requires :plans."
      {:options options})))

  (when-not (or (nil? (:name options))
                (keyword? (:name options)))
    (throw
     (preflight-error
      :invalid-name
      "Gesso Choreo plan-registry :name must be nil or a keyword."
      {:name (:name options)})))

  (when-not (or (nil? (:required-keys options))
                (keyword-set? (:required-keys options)))
    (throw
     (preflight-error
      :invalid-required-keys
      "Gesso Choreo plan-registry :required-keys must be nil or a set of keywords."
      {:required-keys (:required-keys options)})))

  (when-not (or (nil? (:expected-role options))
                (keyword? (:expected-role options)))
    (throw
     (preflight-error
      :invalid-expected-role
      "Gesso Choreo plan-registry :expected-role must be nil or a keyword."
      {:expected-role (:expected-role options)})))

  (doseq [flag [:single-role? :allow-empty?]]
    (when (and (contains? options flag)
               (not (boolean? (get options flag))))
      (throw
       (preflight-error
        :invalid-boolean-option
        "Gesso Choreo plan-registry boolean option must be true or false."
        {:option flag
         :value (get options flag)}))))

  ;; A malformed expected digest declaration is API misuse rather than a stale
  ;; artifact. Reject it before comparing anything with executable content.
  (when-not (or (nil? (:expected-digests options))
                (keyword-digest-map? (:expected-digests options)))
    (throw
     (preflight-error
      :invalid-expected-digests
      "Gesso Choreo plan-registry :expected-digests must be nil or a keyword-keyed map of lowercase SHA-256 hex digests."
      {:expected-digests (:expected-digests options)})))

  options)

(defn- plan-format-issue
  [logical-key plan]
  (let [{:keys [status] :as format-status}
        (project/executable-plan-format-status plan)]
    (case status
      :compatible
      nil

      :incompatible
      (issue
       :unsupported-executable-plan-version
       "Plan registry contains a recognizable ExecutablePlan whose format version is unsupported."
       {:plan-key logical-key
        :format-status format-status})

      :invalid
      (issue
       :invalid-executable-plan
       "Plan registry contains a malformed or non-canonical ExecutablePlan."
       {:plan-key logical-key
        :format-status format-status})

      (issue
       :unknown-executable-plan-format-status
       "ExecutablePlan format classifier returned an unknown status."
       {:plan-key logical-key
        :format-status format-status}))))

(defn- canonical-plans
  [plans]
  (into {}
        (keep
         (fn [[logical-key plan]]
           (when (and
                  (keyword? logical-key)
                  (project/executable-plan? plan))
             [logical-key plan])))
        plans))

(defn- plan-digests
  [plans]
  (into
   (sorted-map)
   (map
    (fn [[logical-key plan]]
      [logical-key
       (artifact/executable-digest plan)]))
   plans))

(defn- logical-key-errors
  [plans]
  (->> plans
       keys
       (remove keyword?)
       (sort-by pr-str)
       (mapv
        (fn [logical-key]
          (issue
           :invalid-plan-key
           "Plan registry keys must be keywords."
           {:plan-key logical-key})))))

(defn- format-errors
  [plans]
  (->> plans
       (sort-by (comp pr-str key))
       (keep
        (fn [[logical-key plan]]
          (when (keyword? logical-key)
            (plan-format-issue logical-key plan))))
       vec))

(defn- coverage-errors
  [required-keys actual-keys]
  (if (nil? required-keys)
    []
    (let [missing
          (set/difference required-keys actual-keys)

          unexpected
          (set/difference actual-keys required-keys)]
      (cond-> []
        (seq missing)
        (conj
         (issue
          :missing-required-plans
          "Plan registry is missing required logical plan keys."
          {:missing-keys missing}))

        (seq unexpected)
        (conj
         (issue
          :unexpected-plans
          "Plan registry contains logical plan keys that were not declared as required."
          {:unexpected-keys unexpected}))))))

(defn- role-errors
  [roles {:keys [single-role? expected-role]}]
  (cond-> []
    (and single-role?
         (> (count roles) 1))
    (conj
     (issue
      :multiple-physical-roles
      "Plan registry requires one physical role but contains plans for multiple roles."
      {:roles roles}))

    (and expected-role
         (seq roles)
         (not= #{expected-role} roles))
    (conj
     (issue
      :unexpected-plan-role
      "Plan registry does not exclusively use its declared expected role."
      {:expected-role expected-role
       :actual-roles roles}))))

(defn- digest-errors
  [actual-digests expected-digests]
  (if (nil? expected-digests)
    []
    (let [actual-keys
          (set (keys actual-digests))

          expected-keys
          (set (keys expected-digests))

          missing-declarations
          (set/difference actual-keys expected-keys)

          stale-declarations
          (set/difference expected-keys actual-keys)

          shared
          (set/intersection actual-keys expected-keys)

          mismatches
          (->> shared
               (keep
                (fn [logical-key]
                  (let [actual
                        (get actual-digests logical-key)

                        expected
                        (get expected-digests logical-key)]
                    (when-not (= actual expected)
                      {:plan-key logical-key
                       :expected-digest expected
                       :actual-digest actual}))))
               (sort-by (comp pr-str :plan-key))
               vec)]
      (cond-> []
        (seq missing-declarations)
        (conj
         (issue
          :missing-expected-digests
          "Expected digest registry is missing current logical plan keys."
          {:missing-keys missing-declarations}))

        (seq stale-declarations)
        (conj
         (issue
          :stale-expected-digests
          "Expected digest registry contains logical plan keys that no longer exist."
          {:stale-keys stale-declarations}))

        (seq mismatches)
        (conj
         (issue
          :executable-plan-digest-mismatch
          "One or more ExecutablePlans no longer match the expected generated-artifact digests."
          {:mismatches mismatches}))))))

;; -----------------------------------------------------------------------------
;; Public preflight
;; -----------------------------------------------------------------------------

(defn check-plan-registry
  "Return a closed preflight report for one logical ExecutablePlan registry.

   Options:

     :name
       Optional keyword used only for registry identity/diagnostics.

     :plans
       Required logical-key -> ExecutablePlan map.

     :required-keys
       Optional exact set of logical keys the registry must contain. Both
       missing and unexpected plans are errors.

     :single-role?
       When true, all canonical plans must project to one physical role.

     :expected-role
       Optional role that every canonical plan must use. This is stronger than
       merely asking for one role because it also rejects one consistent but
       wrong role.

     :expected-digests
       Optional logical-key -> SHA-256 digest map captured by a downstream
       generated artifact. Exact key and digest agreement is required. This is
       the stale-generated-artifact preflight boundary.

     :allow-empty?
       Defaults false. Set true only for infrastructure that intentionally
       permits an empty registry.

   Invalid option shapes throw immediately because they are checker API misuse.
   Registry/application defects are returned in :errors so callers may inspect
   all known failures in one pass."
  [options]
  (let [{:keys
         [name
          plans
          required-keys
          expected-digests
          allow-empty?]
         :or {allow-empty? false}
         :as options'}
        (validate-options! options)

        plans-map?
        (map? plans)

        plans'
        (if plans-map?
          plans
          {})

        key-errors
        (if plans-map?
          (logical-key-errors plans')
          [(issue
            :invalid-plan-registry
            "Plan registry :plans must be a map."
            {:plans plans})])

        plan-format-errors
        (if plans-map?
          (format-errors plans')
          [])

        canonical
        (if plans-map?
          (canonical-plans plans')
          {})

        actual-keys
        (set (keys canonical))

        roles
        (set (map :role (vals canonical)))

        digests
        (plan-digests canonical)

        empty-errors
        (if (or allow-empty?
                (seq plans'))
          []
          [(issue
            :empty-plan-registry
            "Plan registry must contain at least one ExecutablePlan."
            {})])

        errors
        (vec
         (concat
          key-errors
          plan-format-errors
          empty-errors
          (coverage-errors required-keys actual-keys)
          (role-errors roles options')
          (digest-errors digests expected-digests)))]

    {:gesso.choreo/type
     report-type

     :gesso.choreo/version
     preflight-version

     :valid?
     (empty? errors)

     :errors
     errors

     :warnings
     []

     :inputs
     options'

     :analysis
     {:name name
      :plan-keys actual-keys
      :roles roles
      :digests digests}}))

(defn report?
  "True only when value is exactly the current plan-registry preflight report
   derivable from its embedded checker inputs.

   Recognition deliberately re-runs check-plan-registry. A caller cannot forge
   a positive report by editing plan coverage, roles, digests, name, errors, or
   validity while retaining a plausible report shape."
  [value]
  (and
   (map? value)
   (= report-keys
      (set (keys value)))
   (= report-type
      (:gesso.choreo/type value))
   (= preflight-version
      (:gesso.choreo/version value))
   (boolean? (:valid? value))
   (vector? (:errors value))
   (vector? (:warnings value))
   (map? (:inputs value))
   (map? (:analysis value))
   (= analysis-keys
      (set (keys (:analysis value))))
   (set? (get-in value [:analysis :plan-keys]))
   (set? (get-in value [:analysis :roles]))
   (map? (get-in value [:analysis :digests]))
   (= (:valid? value)
      (empty? (:errors value)))
   (try
     (= value
        (check-plan-registry (:inputs value)))
     (catch Exception _
       false))))

(defn valid?
  "True only for a recognized successful plan-registry preflight report."
  [report]
  (and
   (report? report)
   (true? (:valid? report))))

(defn plan-registry?
  "True when value is a closed current PlanRegistry whose logical keys, roles,
   canonical ExecutablePlans, and recorded digests agree exactly."
  [value]
  (and
   (map? value)
   (= plan-registry-keys
      (set (keys value)))
   (= plan-registry-type
      (:gesso.choreo/type value))
   (= preflight-version
      (:gesso.choreo/version value))
   (or (nil? (:name value))
       (keyword? (:name value)))
   (set? (:roles value))
   (every? keyword? (:roles value))
   (map? (:plans value))
   (every?
    (fn [[logical-key plan]]
      (and
       (keyword? logical-key)
       (project/executable-plan? plan)))
    (:plans value))
   (= (:roles value)
      (set (map :role
                (vals (:plans value)))))
   (keyword-digest-map?
    (:digests value))
   (= (set (keys (:plans value)))
      (set (keys (:digests value))))
   (= (:digests value)
      (plan-digests (:plans value)))))

(defn require-plan-registry!
  "Run plan-registry preflight and return one closed portable PlanRegistry.

   All discovered registry defects are attached to the thrown ExceptionInfo so
   build tooling can report the complete preflight result rather than failing
   one plan at a time."
  [options]
  (let [report
        (check-plan-registry options)]
    (when-not (valid? report)
      (throw
       (preflight-error
        :plan-registry-preflight-failed
        "Gesso Choreo plan-registry preflight failed."
        {:preflight report})))

    (let [{:keys [name plans]}
          options

          canonical
          (canonical-plans plans)

          registry
          {:gesso.choreo/type
           plan-registry-type

           :gesso.choreo/version
           preflight-version

           :name
           name

           :roles
           (get-in report [:analysis :roles])

           :plans
           canonical

           :digests
           (get-in report [:analysis :digests])}]
      (when-not (plan-registry? registry)
        (throw
         (preflight-error
          :invalid-emitted-plan-registry
          "Gesso Choreo preflight produced an internally inconsistent PlanRegistry."
          {:registry registry
           :preflight report})))
      registry)))

(defn explain
  "Return a compact stable summary of a successful PlanRegistry or a preflight
   report."
  [value]
  (cond
    (plan-registry? value)
    {:type plan-registry-type
     :version preflight-version
     :name (:name value)
     :plan-count (count (:plans value))
     :plan-keys (set (keys (:plans value)))
     :roles (:roles value)
     :digests (:digests value)}

    (report? value)
    {:type report-type
     :version preflight-version
     :valid? (:valid? value)
     :error-count (count (:errors value))
     :warning-count (count (:warnings value))
     :name (get-in value [:analysis :name])
     :plan-count (count (get-in value [:analysis :plan-keys]))
     :roles (get-in value [:analysis :roles])}

    :else
    (throw
     (preflight-error
      :unsupported-explain-value
      "Expected a Gesso Choreo PlanRegistry or preflight report."
      {:value value}))))
