(ns gesso.live.browser.bootstrap
  "Browser-side consumer for one preflighted Gesso Live BrowserAssemblyManifest.

   Build-time code in gesso.live.browser.entrypoint emits one closed manifest
   containing the exact verified ExecutablePlans, physical browser role,
   enabled runtime features, optimistic command transport, and canonical
   bootstrap contract. This namespace is the one runtime consumer of that
   manifest.

   Applications provide only physical realization callbacks/options that cannot
   be derived from portable Choreo data (for example provisional projection,
   DOM rendering, and authoritative refresh). They do not provide a second plan
   registry, browser role, plan resolver, feature declaration, command
   transport declaration, or runtime entrypoint.

   For protocol-v3 HTMX optimism, the :plan-for callback is derived directly
   from the manifest's closed PlanRegistry. A rendered action therefore cannot
   select an application-maintained plan registry that drifted from the one
   verified and embedded by the compiler bridge.

   This namespace also prevents silently adopting a process-default browser
   runtime initialized outside the manifest consumer. Such a runtime cannot be
   proven to correspond to the embedded assembly and is rejected rather than
   reused."
  (:require
   [clojure.set :as set]
   [gesso.choreo.machine :as machine]
   [gesso.live.browser.runtime :as runtime]))

;; =============================================================================
;; Portable manifest vocabulary
;; =============================================================================

(def bootstrap-version
  1)

(def assembly-manifest-type
  :gesso.live.browser.preflight/assembly-manifest)

(def assembly-manifest-version
  1)

(def plan-registry-type
  :gesso.choreo.preflight/plan-registry)

(def plan-registry-version
  1)

(def canonical-bootstrap-entrypoint
  'gesso.live.browser.runtime/init!)

(def ^:private manifest-keys
  #{:gesso.live.browser/type
    :gesso.live.browser/version
    :name
    :plan-registry
    :browser-role
    :required-plan-keys
    :features
    :optimistic-command-transport
    :bootstrap})

(def ^:private plan-registry-keys
  #{:gesso.choreo/type
    :gesso.choreo/version
    :name
    :roles
    :plans
    :digests})

(def ^:private bootstrap-contract-keys
  #{:required?
    :entrypoint})

(def ^:private supported-features
  #{:optimistic
    :optimistic-htmx})

(def ^:private supported-command-transports
  #{:none
    :htmx
    :custom})

;; =============================================================================
;; Runtime realization vocabulary
;; =============================================================================

(def ^:private realization-option-keys
  #{:core-options
    :choreo-options
    :optimistic-options
    :optimistic-htmx-options})

(def ^:private manifest-owned-optimistic-option-keys
  #{:browser-role})

(def ^:private manifest-owned-optimistic-htmx-option-keys
  #{:plan-for})

(defonce ^:private active-manifest*
  (atom nil))

;; =============================================================================
;; Errors / validation
;; =============================================================================

(defn- bootstrap-error
  ([kind message data]
   (bootstrap-error kind message data nil))
  ([kind message data cause]
   (ex-info
    message
    (merge
     {:error/type :gesso.live.browser.bootstrap/error
      :error/kind kind}
     data)
    cause)))

(defn- keyword-set?
  [value]
  (and
   (set? value)
   (every? keyword? value)))

(defn- sha-256-hex?
  [value]
  (and
   (string? value)
   (boolean
    (re-matches #"[0-9a-f]{64}" value))))

(defn- digest-map?
  [value]
  (and
   (map? value)
   (every?
    (fn [[plan-key digest]]
      (and
       (keyword? plan-key)
       (sha-256-hex? digest)))
    value)))

(defn- require-map!
  [label value]
  (when-not (map? value)
    (throw
     (bootstrap-error
      :invalid-map
      (str label " must be a map.")
      {:label label
       :value value})))
  value)

(defn- check-keys!
  [label allowed value]
  (require-map! label value)
  (let [unknown
        (set/difference
         (set (keys value))
         allowed)]
    (when (seq unknown)
      (throw
       (bootstrap-error
        :unknown-options
        (str label " contains unsupported keys.")
        {:label label
         :unknown-keys unknown
         :allowed-keys allowed}))))
  value)

(defn- bootstrap-contract?
  [value]
  (and
   (map? value)
   (= bootstrap-contract-keys
      (set (keys value)))
   (true? (:required? value))
   (= canonical-bootstrap-entrypoint
      (:entrypoint value))))

(defn- canonical-plan-registry?
  [registry]
  (and
   (map? registry)
   (= plan-registry-keys
      (set (keys registry)))
   (= plan-registry-type
      (:gesso.choreo/type registry))
   (= plan-registry-version
      (:gesso.choreo/version registry))
   (or (nil? (:name registry))
       (keyword? (:name registry)))
   (keyword-set? (:roles registry))
   (map? (:plans registry))
   (seq (:plans registry))
   (every?
    (fn [[plan-key plan]]
      (and
       (keyword? plan-key)
       (= :compatible
          (:status
           (machine/executable-plan-format-status plan)))))
    (:plans registry))
   (= (:roles registry)
      (set
       (map :role
            (vals (:plans registry)))))
   (digest-map? (:digests registry))
   (= (set (keys (:plans registry)))
      (set (keys (:digests registry))))))

(defn manifest?
  "True when value has the closed portable shape required by the browser
   manifest consumer.

   Build-time preflight remains authoritative for SHA-256 digest computation.
   Browser validation checks the embedded registry's closed shape, current plan
   format, exact plan coverage, physical role, feature closure, transport, and
   bootstrap contract before runtime initialization."
  [value]
  (and
   (map? value)
   (= manifest-keys
      (set (keys value)))
   (= assembly-manifest-type
      (:gesso.live.browser/type value))
   (= assembly-manifest-version
      (:gesso.live.browser/version value))
   (or (nil? (:name value))
       (keyword? (:name value)))
   (canonical-plan-registry?
    (:plan-registry value))
   (keyword? (:browser-role value))
   (keyword-set? (:required-plan-keys value))
   (seq (:required-plan-keys value))
   (= (:required-plan-keys value)
      (set (keys (get-in value [:plan-registry :plans]))))
   (= #{(:browser-role value)}
      (get-in value [:plan-registry :roles]))
   (set? (:features value))
   (set/subset? (:features value)
                supported-features)
   (contains?
    supported-command-transports
    (:optimistic-command-transport value))
   (let [optimistic?
         (contains? (:features value) :optimistic)

         optimistic-htmx?
         (contains? (:features value) :optimistic-htmx)

         transport
         (:optimistic-command-transport value)]
     (and
      (or (not optimistic-htmx?)
          optimistic?)
      (or (not optimistic?)
          (not= :none transport))
      (or optimistic?
          (= :none transport))
      (or (not optimistic-htmx?)
          (= :htmx transport))
      (or (not= :htmx transport)
          optimistic-htmx?)))
   (bootstrap-contract?
    (:bootstrap value))))

(defn require-manifest!
  [manifest]
  (when-not (manifest? manifest)
    (throw
     (bootstrap-error
      :invalid-assembly-manifest
      "Expected a current closed Gesso Live BrowserAssemblyManifest."
      {:manifest manifest})))
  manifest)

(defn- require-realization-options!
  [options]
  (let [options
        (or options {})]
    (check-keys!
     "Browser assembly realization options"
     realization-option-keys
     options)

    (doseq [option-key realization-option-keys
            :when (contains? options option-key)]
      (require-map!
       (str "Browser assembly " option-key)
       (get options option-key)))

    options))

(defn- reject-manifest-owned-options!
  [label owned supplied]
  (let [collisions
        (set/intersection
         owned
         (set (keys supplied)))]
    (when (seq collisions)
      (throw
       (bootstrap-error
        :manifest-owned-option-override
        "Application realization options may not override manifest-owned browser assembly facts."
        {:label label
         :manifest-owned-keys owned
         :attempted-overrides collisions}))))
  supplied)

;; =============================================================================
;; Manifest-derived plan lookup / runtime options
;; =============================================================================

(defn plan-for
  "Return the exact preflighted ExecutablePlan for logical plan-key.

   Missing/non-keyword plan keys fail closed. The caller cannot supply a second
   plan registry through this API."
  [manifest plan-key]
  (let [manifest
        (require-manifest! manifest)]
    (when-not (keyword? plan-key)
      (throw
       (bootstrap-error
        :invalid-plan-key
        "Browser assembly plan lookup requires a keyword logical plan key."
        {:plan-key plan-key})))
    (or
     (get-in manifest [:plan-registry :plans plan-key])
     (throw
      (bootstrap-error
       :unregistered-plan-key
       "Browser action requested a logical ExecutablePlan that is not present in the preflighted BrowserAssemblyManifest."
       {:plan-key plan-key
        :available-plan-keys
        (:required-plan-keys manifest)})))))

(defn plan-resolver
  "Return the canonical optimistic HTMX plan resolver for manifest.

   The bridge supplies a map containing :plan-key. The resolver ignores all
   other physical fields and resolves only against the embedded PlanRegistry."
  [manifest]
  (let [manifest
        (require-manifest! manifest)]
    (fn [{:keys [plan-key]}]
      (plan-for manifest plan-key))))

(defn runtime-options
  "Derive gesso.live.browser.runtime/init! options from one manifest and the
   application's physical realization callbacks.

   Manifest-owned facts are injected rather than accepted from application
   options:

   - optimistic :browser-role;
   - optimistic HTMX :plan-for;
   - whether optimism / optimistic HTMX exist at all.

   Required callback validation remains delegated to the concrete runtime
   constructors, which own those physical contracts."
  ([manifest]
   (runtime-options manifest nil))
  ([manifest realization-options]
   (let [manifest
         (require-manifest! manifest)

         realization-options
         (require-realization-options!
          realization-options)

         features
         (:features manifest)

         optimistic?
         (contains? features :optimistic)

         optimistic-htmx?
         (contains? features :optimistic-htmx)

         supplied-optimistic
         (or (:optimistic-options realization-options) {})

         supplied-optimistic-htmx
         (or (:optimistic-htmx-options realization-options) {})

         _
         (when (and (not optimistic?)
                    (contains? realization-options :optimistic-options))
           (throw
            (bootstrap-error
             :optimistic-options-without-feature
             "Application supplied optimistic realization options but the BrowserAssemblyManifest does not enable optimism."
             {})))

         _
         (when (and optimistic?
                    (not (contains? realization-options :optimistic-options)))
           (throw
            (bootstrap-error
             :missing-optimistic-realization
             "BrowserAssemblyManifest enables optimism but application realization options do not provide :optimistic-options."
             {})))

         _
         (when (and (not optimistic-htmx?)
                    (contains? realization-options :optimistic-htmx-options))
           (throw
            (bootstrap-error
             :optimistic-htmx-options-without-feature
             "Application supplied optimistic HTMX realization options but the BrowserAssemblyManifest does not enable the optimistic HTMX bridge."
             {})))

         _
         (reject-manifest-owned-options!
          :optimistic-options
          manifest-owned-optimistic-option-keys
          supplied-optimistic)

         _
         (reject-manifest-owned-options!
          :optimistic-htmx-options
          manifest-owned-optimistic-htmx-option-keys
          supplied-optimistic-htmx)]

     (cond->
      {:core-options
       (or (:core-options realization-options) {})

       :choreo-options
       (or (:choreo-options realization-options) {})}

       optimistic?
       (assoc
        :optimistic-options
        (assoc supplied-optimistic
               :browser-role
               (:browser-role manifest)))

       optimistic-htmx?
       (assoc
        :optimistic-htmx-options
        (assoc supplied-optimistic-htmx
               :plan-for
               (plan-resolver manifest)))))))

;; =============================================================================
;; Process-default bootstrap ownership
;; =============================================================================

(defn active-manifest
  "Return the BrowserAssemblyManifest currently owned by this consumer, or nil."
  []
  @active-manifest*)

(defn- runtime-corresponds-to-manifest?
  [browser-runtime manifest]
  (and
   (runtime/runtime? browser-runtime)
   (runtime/started? browser-runtime)
   (= (contains? (:features manifest) :optimistic)
      (some? (runtime/optimistic-runtime browser-runtime)))
   (= (contains? (:features manifest) :optimistic-htmx)
      (some? (runtime/optimistic-htmx-runtime browser-runtime)))
   (or
    (not (contains? (:features manifest) :optimistic))
    (= (:browser-role manifest)
       (:browser-role
        (runtime/optimistic-runtime browser-runtime))))))

(defn init!
  "Initialize the process-default Gesso browser runtime from one closed manifest.

   Applications should call this boundary instead of runtime/init! directly.
   Repeated initialization with the same manifest is idempotent. A different
   manifest, or a process-default runtime that was initialized outside this
   consumer, is rejected because its correspondence to the embedded assembly
   cannot be established."
  ([manifest]
   (init! manifest nil))
  ([manifest realization-options]
   (let [manifest
         (require-manifest! manifest)

         active
         @active-manifest*

         existing-runtime
         (runtime/default-runtime)]

     (when (and active
                (not= active manifest))
       (throw
        (bootstrap-error
         :different-assembly-already-owned
         "A different BrowserAssemblyManifest is already owned by the process-default Gesso browser bootstrap."
         {:active-manifest-name (:name active)
          :requested-manifest-name (:name manifest)})))

     (when (and (nil? active)
                existing-runtime)
       (throw
        (bootstrap-error
         :unmanaged-default-runtime
         "The process-default Gesso browser runtime was initialized outside the BrowserAssemblyManifest consumer and cannot be adopted safely."
         {})))

     (if existing-runtime
       (do
         (when-not
          (runtime-corresponds-to-manifest?
           existing-runtime
           manifest)
           (throw
            (bootstrap-error
             :runtime-manifest-mismatch
             "Existing process-default Gesso browser runtime does not correspond to the owned BrowserAssemblyManifest."
             {:manifest-name (:name manifest)})))
         existing-runtime)

       (let [options
             (runtime-options
              manifest
              realization-options)

             browser-runtime
             (runtime/init! options)]
         (when-not
          (runtime-corresponds-to-manifest?
           browser-runtime
           manifest)
           (try
             (runtime/shutdown!)
             (catch :default _
               nil))
           (throw
            (bootstrap-error
             :initialized-runtime-manifest-mismatch
             "Gesso browser runtime initialized but does not correspond to its BrowserAssemblyManifest."
             {:manifest-name (:name manifest)})))
         (reset! active-manifest* manifest)
         browser-runtime)))))

(defn shutdown!
  "Release bootstrap ownership and stop the process-default browser runtime."
  []
  (reset! active-manifest* nil)
  (runtime/shutdown!))
