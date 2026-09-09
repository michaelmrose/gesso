(ns gesso.live.browser.preflight
  "JVM-side preflight for one Gesso Live browser runtime assembly.

   gesso.choreo.preflight closes and validates a logical ExecutablePlan
   registry. This namespace checks the next composition boundary: whether that
   registry can be installed coherently into one physical browser runtime.

   The checker deliberately reasons only about build-time facts that Gesso can
   know without pretending that a browser has already executed JavaScript:

   - the supplied PlanRegistry is a current untampered Choreo registry;
   - the runtime-visible plan set is exact and non-empty;
   - all plans project to one physical browser role;
   - the declared browser role agrees with that projected role;
   - protocol-v3 optimism and the optimistic HTMX bridge form a closed feature
     dependency graph;
   - optimistic command transport agrees with those enabled features;
   - successful preflight emits one closed BrowserAssemblyManifest carrying the
     exact plan registry and an explicit bootstrap contract.

   The bootstrap contract is intentionally not represented as a caller-provided
   boolean such as :runtime-started?. Static preflight cannot prove that a page
   executed JavaScript. Instead the emitted manifest requires the canonical
   gesso.live.browser.runtime/init! entrypoint. Later build/bootstrap tooling can
   consume this manifest directly, eliminating an independent application-owned
   startup declaration.

   This namespace does not reinterpret Choreo semantics, application
   authorization, DOM affordance policy, authoritative database knowledge, or
   HTMX event behavior. Those are separate proof/preflight layers."
  (:require
   [clojure.set :as set]
   [gesso.choreo.preflight :as choreo-preflight]))

;; -----------------------------------------------------------------------------
;; Identity
;; -----------------------------------------------------------------------------

(def preflight-version
  1)

(def report-type
  :gesso.live.browser.preflight/report)

(def assembly-manifest-type
  :gesso.live.browser.preflight/assembly-manifest)

(def canonical-bootstrap-entrypoint
  'gesso.live.browser.runtime/init!)

(def command-transports
  #{:none
    :htmx
    :custom})

(def ^:private report-keys
  #{:gesso.live.browser/type
    :gesso.live.browser/version
    :valid?
    :errors
    :warnings
    :inputs
    :analysis})

(def ^:private analysis-keys
  #{:name
    :browser-role
    :plan-registry-name
    :plan-keys
    :required-plan-keys
    :plan-roles
    :features
    :optimistic-command-transport
    :bootstrap-entrypoint})

(def ^:private bootstrap-contract-keys
  #{:required?
    :entrypoint})

(def ^:private assembly-manifest-keys
  #{:gesso.live.browser/type
    :gesso.live.browser/version
    :name
    :plan-registry
    :browser-role
    :required-plan-keys
    :features
    :optimistic-command-transport
    :bootstrap})

(def ^:private option-keys
  #{:name
    :plan-registry
    :browser-role
    :required-plan-keys
    :optimistic?
    :optimistic-htmx?
    :optimistic-command-transport})

(def ^:private feature-keys
  #{:optimistic
    :optimistic-htmx})

;; -----------------------------------------------------------------------------
;; Errors / validation helpers
;; -----------------------------------------------------------------------------

(defn- preflight-error
  [kind message data]
  (ex-info
   message
   (merge
    {:error/type :gesso.live.browser.preflight/error
     :error/kind kind}
    data)))

(defn- issue
  [kind message data]
  (merge
   {:kind kind
    :message message}
   data))

(defn- keyword-set?
  [value]
  (and
   (set? value)
   (every? keyword? value)))

(defn- validate-options!
  [options]
  (when-not (map? options)
    (throw
     (preflight-error
      :invalid-options
      "Gesso Live browser assembly preflight options must be a map."
      {:options options})))

  (let [unknown
        (set/difference
         (set (keys options))
         option-keys)]
    (when (seq unknown)
      (throw
       (preflight-error
        :unknown-options
        "Gesso Live browser assembly preflight received unsupported options."
        {:unknown-keys unknown
         :allowed-keys option-keys}))))

  (when-not (contains? options :plan-registry)
    (throw
     (preflight-error
      :missing-plan-registry
      "Gesso Live browser assembly preflight requires :plan-registry."
      {:options options})))

  (when-not (contains? options :browser-role)
    (throw
     (preflight-error
      :missing-browser-role
      "Gesso Live browser assembly preflight requires :browser-role."
      {:options options})))

  (when-not (or (nil? (:name options))
                (keyword? (:name options)))
    (throw
     (preflight-error
      :invalid-name
      "Gesso Live browser assembly :name must be nil or a keyword."
      {:name (:name options)})))

  (when-not (keyword? (:browser-role options))
    (throw
     (preflight-error
      :invalid-browser-role
      "Gesso Live browser assembly :browser-role must be a keyword."
      {:browser-role (:browser-role options)})))

  (when-not (or (nil? (:required-plan-keys options))
                (keyword-set? (:required-plan-keys options)))
    (throw
     (preflight-error
      :invalid-required-plan-keys
      "Gesso Live browser assembly :required-plan-keys must be nil or a set of keywords."
      {:required-plan-keys (:required-plan-keys options)})))

  (doseq [flag [:optimistic? :optimistic-htmx?]]
    (when (and (contains? options flag)
               (not (boolean? (get options flag))))
      (throw
       (preflight-error
        :invalid-boolean-option
        "Gesso Live browser assembly boolean option must be true or false."
        {:option flag
         :value (get options flag)}))))

  (when-not (or (nil? (:optimistic-command-transport options))
                (contains?
                 command-transports
                 (:optimistic-command-transport options)))
    (throw
     (preflight-error
      :invalid-optimistic-command-transport
      "Gesso Live browser assembly :optimistic-command-transport is unsupported."
      {:transport (:optimistic-command-transport options)
       :allowed command-transports})))

  options)

(defn- registry-errors
  [registry required-plan-keys browser-role]
  (if-not (choreo-preflight/plan-registry? registry)
    [(issue
      :invalid-plan-registry
      "Browser assembly requires a current untampered Gesso Choreo PlanRegistry."
      {:plan-registry registry})]
    (let [actual-plan-keys
          (set (keys (:plans registry)))

          required-plan-keys'
          (or required-plan-keys
              actual-plan-keys)

          missing-plan-keys
          (set/difference
           required-plan-keys'
           actual-plan-keys)

          unexpected-plan-keys
          (set/difference
           actual-plan-keys
           required-plan-keys')

          roles
          (:roles registry)]
      (cond-> []
        (empty? actual-plan-keys)
        (conj
         (issue
          :empty-browser-plan-registry
          "Browser assembly requires at least one executable plan."
          {}))

        (> (count roles) 1)
        (conj
         (issue
          :multiple-browser-plan-roles
          "One physical browser runtime cannot own ExecutablePlans projected for multiple browser roles."
          {:roles roles}))

        (and (= 1 (count roles))
             (not= #{browser-role} roles))
        (conj
         (issue
          :browser-role-plan-mismatch
          "Browser runtime role does not match the role projected into its ExecutablePlans."
          {:browser-role browser-role
           :plan-roles roles}))

        (seq missing-plan-keys)
        (conj
         (issue
          :missing-browser-plans
          "Browser assembly is missing required logical ExecutablePlans."
          {:missing-keys missing-plan-keys}))

        (seq unexpected-plan-keys)
        (conj
         (issue
          :unexpected-browser-plans
          "Browser assembly PlanRegistry contains logical plans outside its declared runtime-visible set."
          {:unexpected-keys unexpected-plan-keys}))))))

(defn- feature-errors
  [{:keys
    [optimistic?
     optimistic-htmx?
     optimistic-command-transport]}]
  (cond-> []
    (and optimistic-htmx?
         (not optimistic?))
    (conj
     (issue
      :optimistic-htmx-requires-optimism
      "The optimistic HTMX bridge requires the protocol-v3 optimistic browser runtime."
      {}))

    (and optimistic?
         (= :none optimistic-command-transport))
    (conj
     (issue
      :optimism-requires-command-transport
      "Protocol-v3 browser optimism requires an optimistic command transport."
      {}))

    (and (not optimistic?)
         (not= :none optimistic-command-transport))
    (conj
     (issue
      :command-transport-without-optimism
      "An optimistic command transport cannot be enabled when browser optimism is disabled."
      {:transport optimistic-command-transport}))

    (and optimistic-htmx?
         (not= :htmx optimistic-command-transport))
    (conj
     (issue
      :optimistic-htmx-requires-htmx-transport
      "The optimistic HTMX bridge requires :htmx as the optimistic command transport."
      {:transport optimistic-command-transport}))

    (and (= :htmx optimistic-command-transport)
         (not optimistic-htmx?))
    (conj
     (issue
      :htmx-transport-requires-optimistic-htmx
      "The :htmx optimistic command transport requires the optimistic HTMX bridge."
      {}))))

(defn- bootstrap-contract?
  [value]
  (and
   (map? value)
   (= bootstrap-contract-keys
      (set (keys value)))
   (true? (:required? value))
   (= canonical-bootstrap-entrypoint
      (:entrypoint value))))

(defn- normalized-options
  [options]
  (let [optimistic?
        (get options :optimistic? false)

        optimistic-htmx?
        (get options :optimistic-htmx? false)

        optimistic-command-transport
        (or (:optimistic-command-transport options)
            (cond
              optimistic-htmx?
              :htmx

              optimistic?
              :custom

              :else
              :none))]
    (assoc options
           :optimistic? optimistic?
           :optimistic-htmx? optimistic-htmx?
           :optimistic-command-transport optimistic-command-transport)))

;; -----------------------------------------------------------------------------
;; Public preflight
;; -----------------------------------------------------------------------------

(defn check-browser-assembly
  "Return a closed build-time report for one browser runtime assembly.

   Required options:

     :plan-registry
       A closed value emitted by gesso.choreo.preflight/require-plan-registry!.

     :browser-role
       The one physical Choreo participant owned by this browser runtime.

   Optional options:

     :name
       Keyword identity used only for diagnostics/artifact identity.

     :required-plan-keys
       Exact runtime-visible logical plan set. Defaults to every plan currently
       in :plan-registry. Missing and unexpected keys are both assembly drift.

     :optimistic?
       Whether gesso.live.browser.optimistic is required. Defaults false.

     :optimistic-htmx?
       Whether gesso.live.browser.optimistic-htmx is required. Defaults false.
       This implies :optimistic? and :htmx command transport.

     :optimistic-command-transport
       One of :none, :htmx, or :custom. When omitted it defaults coherently from
       the feature declaration (:htmx for optimistic HTMX, :custom for optimism
       without that bridge, otherwise :none).

   Option-shape mistakes throw immediately. Cross-file/application assembly
   defects are accumulated in :errors so a build can report all known drift in
   one pass."
  [options]
  (let [{:keys
         [name
          plan-registry
          browser-role
          required-plan-keys
          optimistic?
          optimistic-htmx?
          optimistic-command-transport]
         :as options'}
        (-> options
            validate-options!
            normalized-options)

        registry-valid?
        (choreo-preflight/plan-registry? plan-registry)

        actual-plan-keys
        (if registry-valid?
          (set (keys (:plans plan-registry)))
          #{})

        required-plan-keys'
        (or required-plan-keys
            actual-plan-keys)

        plan-roles
        (if registry-valid?
          (:roles plan-registry)
          #{})

        errors
        (vec
         (concat
          (registry-errors
           plan-registry
           required-plan-keys
           browser-role)
          (feature-errors options')))]

    {:gesso.live.browser/type
     report-type

     :gesso.live.browser/version
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
      :browser-role browser-role
      :plan-registry-name
      (when registry-valid?
        (:name plan-registry))
      :plan-keys actual-plan-keys
      :required-plan-keys required-plan-keys'
      :plan-roles plan-roles
      :features
      (cond-> #{}
        optimistic?
        (conj :optimistic)

        optimistic-htmx?
        (conj :optimistic-htmx))
      :optimistic-command-transport
      optimistic-command-transport
      :bootstrap-entrypoint
      canonical-bootstrap-entrypoint}}))

(defn report?
  "True only when value is exactly the current browser-assembly report
   derivable from its embedded preflight inputs.

   Recognition deliberately re-runs check-browser-assembly. A caller cannot
   forge a positive report by editing plan coverage, runtime role, feature or
   transport summaries, bootstrap metadata, errors, or validity while retaining
   a plausible report shape."
  [value]
  (and
   (map? value)
   (= report-keys
      (set (keys value)))
   (= report-type
      (:gesso.live.browser/type value))
   (= preflight-version
      (:gesso.live.browser/version value))
   (boolean? (:valid? value))
   (vector? (:errors value))
   (vector? (:warnings value))
   (map? (:inputs value))
   (map? (:analysis value))
   (= analysis-keys
      (set (keys (:analysis value))))
   (set? (get-in value [:analysis :plan-keys]))
   (set? (get-in value [:analysis :required-plan-keys]))
   (set? (get-in value [:analysis :plan-roles]))
   (set? (get-in value [:analysis :features]))
   (= (:valid? value)
      (empty? (:errors value)))
   (try
     (= value
        (check-browser-assembly (:inputs value)))
     (catch Exception _
       false))))

(defn valid?
  "True only for a recognized successful browser assembly report."
  [report]
  (and
   (report? report)
   (true? (:valid? report))))

(defn assembly-manifest?
  "True when value is a closed current BrowserAssemblyManifest whose exact
   PlanRegistry, runtime role, runtime-visible plan set, feature closure,
   transport declaration, and bootstrap contract remain internally coherent."
  [value]
  (and
   (map? value)
   (= assembly-manifest-keys
      (set (keys value)))
   (= assembly-manifest-type
      (:gesso.live.browser/type value))
   (= preflight-version
      (:gesso.live.browser/version value))
   (or (nil? (:name value))
       (keyword? (:name value)))
   (choreo-preflight/plan-registry?
    (:plan-registry value))
   (keyword? (:browser-role value))
   (keyword-set? (:required-plan-keys value))
   (set? (:features value))
   (set/subset? (:features value)
                feature-keys)
   (contains?
    command-transports
    (:optimistic-command-transport value))
   (bootstrap-contract?
    (:bootstrap value))
   (let [report
         (check-browser-assembly
          {:name (:name value)
           :plan-registry (:plan-registry value)
           :browser-role (:browser-role value)
           :required-plan-keys (:required-plan-keys value)
           :optimistic?
           (contains? (:features value)
                      :optimistic)
           :optimistic-htmx?
           (contains? (:features value)
                      :optimistic-htmx)
           :optimistic-command-transport
           (:optimistic-command-transport value)})]
     (valid? report))))

(defn require-browser-assembly!
  "Run browser assembly preflight and return one closed manifest.

   The returned manifest is intended to become the single build-time input for
   later browser-entrypoint generation. In particular, application code should
   not separately maintain a second list of plan keys, browser role, optimism
   features, or runtime bootstrap entrypoint."
  [options]
  (let [options'
        (-> options
            validate-options!
            normalized-options)

        report
        (check-browser-assembly options')]
    (when-not (valid? report)
      (throw
       (preflight-error
        :browser-assembly-preflight-failed
        "Gesso Live browser assembly preflight failed."
        {:preflight report})))

    (let [{:keys
           [name
            plan-registry
            browser-role
            optimistic?
            optimistic-htmx?
            optimistic-command-transport]}
          options'

          manifest
          {:gesso.live.browser/type
           assembly-manifest-type

           :gesso.live.browser/version
           preflight-version

           :name
           name

           :plan-registry
           plan-registry

           :browser-role
           browser-role

           :required-plan-keys
           (get-in report
                   [:analysis :required-plan-keys])

           :features
           (cond-> #{}
             optimistic?
             (conj :optimistic)

             optimistic-htmx?
             (conj :optimistic-htmx))

           :optimistic-command-transport
           optimistic-command-transport

           :bootstrap
           {:required? true
            :entrypoint canonical-bootstrap-entrypoint}}]
      (when-not (assembly-manifest? manifest)
        (throw
         (preflight-error
          :invalid-emitted-browser-assembly
          "Gesso Live browser preflight produced an internally inconsistent BrowserAssemblyManifest."
          {:manifest manifest
           :preflight report})))
      manifest)))

(defn explain
  "Return a compact stable summary of a browser assembly report or manifest."
  [value]
  (cond
    (assembly-manifest? value)
    {:type assembly-manifest-type
     :version preflight-version
     :name (:name value)
     :browser-role (:browser-role value)
     :plan-count (count (:required-plan-keys value))
     :plan-keys (:required-plan-keys value)
     :features (:features value)
     :optimistic-command-transport
     (:optimistic-command-transport value)
     :bootstrap-entrypoint
     (get-in value [:bootstrap :entrypoint])}

    (report? value)
    {:type report-type
     :version preflight-version
     :valid? (:valid? value)
     :error-count (count (:errors value))
     :warning-count (count (:warnings value))
     :name (get-in value [:analysis :name])
     :browser-role (get-in value [:analysis :browser-role])
     :plan-count (count (get-in value [:analysis :plan-keys]))
     :features (get-in value [:analysis :features])
     :optimistic-command-transport
     (get-in value [:analysis :optimistic-command-transport])}

    :else
    (throw
     (preflight-error
      :unsupported-explain-value
      "Gesso Live browser preflight cannot explain this value."
      {:value value}))))
