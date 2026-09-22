(ns gesso.live.optimistic.preflight
  "JVM-side preflight joining optimistic browser operation realization to the
   trusted optimistic server registry.

   Earlier Gesso layers establish, independently:

     semantic operation -> optimistic capability -> browser ExecutablePlan

   and:

     semantic operation -> trusted optimistic.server operation -> authority plan

   This namespace closes the correspondence between those two sides before an
   HTTP request exists.  For every exposed optimistic operation it requires:

   - one current untampered BrowserAssemblyManifest with optimism enabled;
   - one canonical operation capability naming a plan present in that manifest;
   - one trusted optimistic.server operation under the same semantic operation;
   - exact equality between that server operation's authority ExecutablePlan and
     the authority projection implied by its own choreography identity/roles;
   - exact browser-role agreement; and
   - exact equality between the manifest's browser ExecutablePlan and the
     browser projection implied by the trusted server operation's choreography
     identity/roles.

   Successful preflight emits one closed OptimisticOperationAssembly.  The
   assembly retains the exact trusted operation-capability registry that defined
   the exposed semantic operation set, then carries a route requirement for each
   operation: semantic operation plus the already-preflighted command transport.
   Recognition therefore cannot silently drop one realization and its matching
   route requirement while leaving the source capability declaration intact. A
   later route layer can consume those requirements without asking the
   application to repeat transport identity.

   This layer deliberately does NOT claim route closure, authentication,
   authorization, required/supplied execution-capability closure, model
   correctness, settlement/progression closure, or Live acquisition closure.
   Those remain later application-assembly edges."
  (:require
   [clojure.set :as set]
   [gesso.choreo.artifact :as choreo-artifact]
   [gesso.live.browser.preflight :as browser-preflight]
   [gesso.live.optimistic.capability :as capability]
   [gesso.live.optimistic.choreo :as optimistic-choreo]
   [gesso.live.optimistic.server :as optimistic-server]))

;; =============================================================================
;; Identity
;; =============================================================================

(def preflight-version 4)

(def report-type
  :gesso.live.optimistic.preflight/report)

(def operation-assembly-type
  :gesso.live.optimistic.preflight/operation-assembly)

(def route-requirement-type
  :gesso.live.optimistic.preflight/route-requirement)

(def ^:private report-keys
  #{:gesso.live.optimistic.preflight/type
    :gesso.live.optimistic.preflight/version
    :valid?
    :errors
    :warnings
    :inputs
    :analysis})

(def ^:private analysis-keys
  #{:name
    :browser-assembly-name
    :browser-role
    :command-transport
    :browser-operations
    :registered-server-operations})

(def ^:private assembly-keys
  #{:gesso.live.optimistic.preflight/type
    :gesso.live.optimistic.preflight/version
    :name
    :browser-assembly
    :operation-capabilities
    :operations
    :route-requirements})

(def ^:private operation-realization-keys
  #{:operation
    :plan-key
    :choreography-name
    :browser-role
    :authority-role
    :browser-plan-digest
    :authority-plan-digest
    :command-transport})

(def ^:private route-requirement-keys
  #{:gesso.live.optimistic.preflight/type
    :gesso.live.optimistic.preflight/version
    :operation
    :transport})

(def ^:private option-keys
  #{:name
    :browser-assembly
    :operation-capabilities
    :server-operations})

;; =============================================================================
;; Errors / validation helpers
;; =============================================================================

(defn- preflight-error
  [kind message data]
  (ex-info
   message
   (merge
    {:error/type :gesso.live.optimistic.preflight/error
     :error/kind kind}
    data)))

(defn- issue
  [kind message data]
  (merge
   {:kind kind
    :message message}
   data))

(defn- require-map!
  [label value]
  (when-not (map? value)
    (throw
     (preflight-error
      :invalid-shape
      (str label " must be a map.")
      {:label label
       :value value})))
  value)

(defn- validate-options!
  [options]
  (let [options' (require-map! "Optimistic operation preflight options" options)
        unknown (set/difference (set (keys options')) option-keys)]
    (when (seq unknown)
      (throw
       (preflight-error
        :unknown-option-keys
        "Optimistic operation preflight options contain unknown keys."
        {:unknown-keys unknown
         :allowed-keys option-keys})))
    (doseq [required [:browser-assembly
                      :operation-capabilities
                      :server-operations]]
      (when-not (contains? options' required)
        (throw
         (preflight-error
          :missing-option-key
          "Optimistic operation preflight is missing a required option."
          {:missing-key required
           :required-keys
           #{:browser-assembly
             :operation-capabilities
             :server-operations}}))))
    (when-not (or (nil? (:name options'))
                  (keyword? (:name options')))
      (throw
       (preflight-error
        :invalid-name
        "Optimistic operation assembly :name must be nil or a keyword."
        {:name (:name options')})))
    options'))

(defn- canonical-capability-registry?
  [value]
  (and
   (map? value)
   (not (empty? value))
   (every?
    (fn [[operation entry]]
      (and
       (keyword? operation)
       (capability/operation-capability? entry)
       (= operation (:operation entry))))
    value)))

(defn- canonical-server-operation-registry?
  [value]
  (and
   (map? value)
   (not (empty? value))
   (every?
    (fn [[operation entry]]
      (and
       (keyword? operation)
       (optimistic-server/operation? entry)
       (= operation (:operation entry))))
    value)))

(defn- operation-choreo-options
  [server-operation]
  {:name (:name server-operation)
   :operation (:operation server-operation)
   :browser-role (:browser-role server-operation)
   :authority-role (:authority-role server-operation)})

(defn- expected-browser-plan
  [server-operation]
  (optimistic-choreo/command-plan
   (operation-choreo-options server-operation)
   (:browser-role server-operation)))

(defn- expected-authority-plan
  [server-operation]
  (optimistic-choreo/command-plan
   (operation-choreo-options server-operation)
   (:authority-role server-operation)))

(defn- browser-plan
  [browser-assembly plan-key]
  (get-in browser-assembly
          [:plan-registry :plans plan-key]))

(defn- browser-plan-digest
  [browser-assembly plan-key]
  (get-in browser-assembly
          [:plan-registry :digests plan-key]))

(defn- realization-projected-plans
  [operation realization]
  (try
    (let [options
          {:name (:choreography-name realization)
           :operation operation
           :browser-role (:browser-role realization)
           :authority-role (:authority-role realization)}]
      {:browser
       (optimistic-choreo/command-plan
        options
        (:browser-role realization))

       :authority
       (optimistic-choreo/command-plan
        options
        (:authority-role realization))})
    (catch clojure.lang.ExceptionInfo _
      nil)))

;; =============================================================================
;; Per-operation correspondence
;; =============================================================================

(defn- operation-errors
  [browser-assembly server-operations operation op-capability]
  (let [plan-key (:plan-key op-capability)
        plan (browser-plan browser-assembly plan-key)
        server-operation (get server-operations operation)
        manifest-role (:browser-role browser-assembly)]
    (cond-> []
      (not (contains? (:required-plan-keys browser-assembly) plan-key))
      (conj
       (issue
        :operation-plan-not-runtime-visible
        "Optimistic operation capability selects a browser plan outside the BrowserAssemblyManifest runtime-visible set."
        {:operation operation
         :plan-key plan-key
         :available-plan-keys (:required-plan-keys browser-assembly)}))

      (nil? plan)
      (conj
       (issue
        :missing-operation-browser-plan
        "Optimistic operation capability selects a browser plan absent from the BrowserAssemblyManifest PlanRegistry."
        {:operation operation
         :plan-key plan-key
         :available-plan-keys
         (set (keys (get-in browser-assembly [:plan-registry :plans])))}))

      (nil? server-operation)
      (conj
       (issue
        :missing-trusted-server-operation
        "Browser-exposed optimistic operation is absent from the trusted server operation registry."
        {:operation operation
         :registered-server-operations (set (keys server-operations))}))

      (and server-operation
           (not= (:authority-plan server-operation)
                 (expected-authority-plan server-operation)))
      (conj
       (issue
        :server-authority-plan-correspondence-mismatch
        "Trusted server operation authority ExecutablePlan does not exactly match the authority projection implied by its choreography identity and roles."
        {:operation operation
         :choreography-name (:name server-operation)
         :authority-role (:authority-role server-operation)}))

      (and server-operation
           (not= manifest-role (:browser-role server-operation)))
      (conj
       (issue
        :server-operation-browser-role-mismatch
        "Trusted server operation and BrowserAssemblyManifest disagree on the physical browser role."
        {:operation operation
         :assembly-browser-role manifest-role
         :server-operation-browser-role (:browser-role server-operation)}))

      (and plan
           server-operation
           (= manifest-role (:browser-role server-operation))
           (not= plan (expected-browser-plan server-operation)))
      (conj
       (issue
        :browser-server-plan-correspondence-mismatch
        "Browser ExecutablePlan does not exactly match the browser projection implied by the trusted server operation."
        {:operation operation
         :plan-key plan-key
         :choreography-name (:name server-operation)
         :browser-role manifest-role})))))

(defn- operation-realization
  [browser-assembly server-operation operation op-capability]
  (let [plan-key (:plan-key op-capability)]
    {:operation operation
     :plan-key plan-key
     :choreography-name (:name server-operation)
     :browser-role (:browser-role server-operation)
     :authority-role (:authority-role server-operation)
     :browser-plan-digest (browser-plan-digest browser-assembly plan-key)
     :authority-plan-digest
     (choreo-artifact/executable-digest
      (:authority-plan server-operation))
     :command-transport (:optimistic-command-transport browser-assembly)}))

(defn- route-requirement
  [browser-assembly operation]
  {:gesso.live.optimistic.preflight/type route-requirement-type
   :gesso.live.optimistic.preflight/version preflight-version
   :operation operation
   :transport (:optimistic-command-transport browser-assembly)})

;; =============================================================================
;; Public report
;; =============================================================================

(defn check-operation-assembly
  "Return a structured preflight report joining browser optimistic operations to
   their trusted server operation realizations.

   Required options:

     :browser-assembly
       Current BrowserAssemblyManifest.

     :operation-capabilities
       Semantic operation -> canonical operation capability.  Unlike the derived
       convenience registry, this preflight also accepts the explicit escape-hatch
       form where :plan-key differs from :operation, provided the map key equals
       the capability's semantic :operation.

     :server-operations
       Semantic operation -> prepared optimistic.server operation entry.

   Optional :name is diagnostic/application-slice identity.

   The server registry may contain additional trusted operations that are not
   browser-exposed.  Every browser-exposed capability, however, must have a
   trusted server realization and exact projected-plan correspondence."
  [options]
  (let [{:keys [name
                browser-assembly
                operation-capabilities
                server-operations]
         :as options'}
        (validate-options! options)

        browser-valid?
        (browser-preflight/assembly-manifest? browser-assembly)

        capabilities-valid?
        (canonical-capability-registry? operation-capabilities)

        server-operations-valid?
        (canonical-server-operation-registry? server-operations)

        base-errors
        (cond-> []
          (not browser-valid?)
          (conj
           (issue
            :invalid-browser-assembly
            "Optimistic operation preflight requires a current untampered BrowserAssemblyManifest."
            {:browser-assembly browser-assembly}))

          (and browser-valid?
               (not (contains? (:features browser-assembly) :optimistic)))
          (conj
           (issue
            :browser-assembly-without-optimism
            "Browser-exposed optimistic operations require :optimistic in the BrowserAssemblyManifest feature set."
            {:features (:features browser-assembly)}))

          (and browser-valid?
               (= :none (:optimistic-command-transport browser-assembly)))
          (conj
           (issue
            :browser-assembly-without-command-transport
            "Browser-exposed optimistic operations require a command transport."
            {:transport (:optimistic-command-transport browser-assembly)}))

          (not capabilities-valid?)
          (conj
           (issue
            :invalid-operation-capabilities
            "Optimistic operation preflight requires a non-empty canonical semantic-operation capability registry."
            {:operation-capabilities operation-capabilities}))

          (not server-operations-valid?)
          (conj
           (issue
            :invalid-server-operations
            "Optimistic operation preflight requires a non-empty canonical trusted server operation registry."
            {:server-operations server-operations})))

        correspondence-errors
        (if (and browser-valid?
                 capabilities-valid?
                 server-operations-valid?)
          (vec
           (mapcat
            (fn [[operation op-capability]]
              (operation-errors
               browser-assembly
               server-operations
               operation
               op-capability))
            (sort-by (comp pr-str key)
                     operation-capabilities)))
          [])

        errors
        (vec (concat base-errors correspondence-errors))]
    {:gesso.live.optimistic.preflight/type report-type
     :gesso.live.optimistic.preflight/version preflight-version
     :valid? (empty? errors)
     :errors errors
     :warnings []
     :inputs options'
     :analysis
     {:name name
      :browser-assembly-name
      (when browser-valid? (:name browser-assembly))
      :browser-role
      (when browser-valid? (:browser-role browser-assembly))
      :command-transport
      (when browser-valid?
        (:optimistic-command-transport browser-assembly))
      :browser-operations
      (if capabilities-valid?
        (set (keys operation-capabilities))
        #{})
      :registered-server-operations
      (if server-operations-valid?
        (set (keys server-operations))
        #{})}}))

(defn report?
  "True only when value is exactly the current operation-assembly report
   derivable from its embedded preflight inputs.

   Recognition deliberately re-runs check-operation-assembly.  A caller cannot
   forge a positive report by editing the derived operation sets, transport,
   browser identity, errors, or validity while retaining a plausible report
   shape."
  [value]
  (and
   (map? value)
   (= report-keys (set (keys value)))
   (= report-type
      (:gesso.live.optimistic.preflight/type value))
   (= preflight-version
      (:gesso.live.optimistic.preflight/version value))
   (boolean? (:valid? value))
   (vector? (:errors value))
   (vector? (:warnings value))
   (map? (:inputs value))
   (map? (:analysis value))
   (= analysis-keys
      (set (keys (:analysis value))))
   (set? (get-in value [:analysis :browser-operations]))
   (set? (get-in value [:analysis :registered-server-operations]))
   (= (:valid? value)
      (empty? (:errors value)))
   (try
     (= value
        (check-operation-assembly (:inputs value)))
     (catch Exception _
       false))))

(defn valid?
  "True only for a recognized successful operation-assembly report."
  [report]
  (and (report? report)
       (true? (:valid? report))))

;; =============================================================================
;; Closed successful product
;; =============================================================================

(defn route-requirement?
  "True for one canonical downstream route requirement emitted by this layer."
  [value]
  (and
   (map? value)
   (= route-requirement-keys (set (keys value)))
   (= route-requirement-type
      (:gesso.live.optimistic.preflight/type value))
   (= preflight-version
      (:gesso.live.optimistic.preflight/version value))
   (keyword? (:operation value))
   (contains? browser-preflight/command-transports
              (:transport value))
   (not= :none (:transport value))))

(defn operation-assembly?
  "True when value is a closed current OptimisticOperationAssembly.

   The emitted realization summary retains exactly the static choreography facts
   needed to reconstruct both projected sides plus a digest of the exact trusted
   authority plan accepted during preflight. Recognition therefore rechecks exact
   browser-plan correspondence against the embedded manifest and exact authority
   projection correspondence against that digest while deliberately omitting
   execute! functions and any claim of runtime authority. The embedded canonical
   operation-capability registry is the trusted source declaration for the exposed
   operation set; recognition requires the derived realization set to match it
   exactly."
  [value]
  (and
   (map? value)
   (= assembly-keys (set (keys value)))
   (= operation-assembly-type
      (:gesso.live.optimistic.preflight/type value))
   (= preflight-version
      (:gesso.live.optimistic.preflight/version value))
   (or (nil? (:name value))
       (keyword? (:name value)))
   (browser-preflight/assembly-manifest?
    (:browser-assembly value))
   (canonical-capability-registry?
    (:operation-capabilities value))
   (map? (:operations value))
   (not (empty? (:operations value)))
   (= (set (keys (:operation-capabilities value)))
      (set (keys (:operations value))))
   (every?
    (fn [[operation realization]]
      (let [projected
            (when (map? realization)
              (realization-projected-plans operation realization))]
        (and
         (keyword? operation)
         (map? realization)
         (= operation-realization-keys
            (set (keys realization)))
         (= operation (:operation realization))
         (= (:plan-key (get (:operation-capabilities value) operation))
            (:plan-key realization))
         (keyword? (:plan-key realization))
         (keyword? (:choreography-name realization))
         (keyword? (:browser-role realization))
         (keyword? (:authority-role realization))
         (string? (:browser-plan-digest realization))
         (string? (:authority-plan-digest realization))
         projected
         (= (:authority-plan-digest realization)
            (choreo-artifact/executable-digest (:authority projected)))
         (= (:browser-role (:browser-assembly value))
            (:browser-role realization))
         (= (:optimistic-command-transport (:browser-assembly value))
            (:command-transport realization))
         (contains? (:required-plan-keys (:browser-assembly value))
                    (:plan-key realization))
         (= (:browser-plan-digest realization)
            (browser-plan-digest
             (:browser-assembly value)
             (:plan-key realization)))
         (= (browser-plan
             (:browser-assembly value)
             (:plan-key realization))
            (:browser projected)))))
    (:operations value))
   (map? (:route-requirements value))
   (= (set (keys (:operations value)))
      (set (keys (:route-requirements value))))
   (every?
    (fn [[operation requirement]]
      (and
       (= operation (:operation requirement))
       (route-requirement? requirement)
       (= (:optimistic-command-transport (:browser-assembly value))
          (:transport requirement))))
    (:route-requirements value))))

(defn require-operation-assembly!
  "Run optimistic browser/server correspondence preflight and return one closed
   OptimisticOperationAssembly.

   The returned route requirements are deliberately obligations, not evidence
   that routes exist.  A later route-preflight layer should consume them and
   establish transport -> trusted route -> server operation closure."
  [options]
  (let [{:keys [name
                browser-assembly
                operation-capabilities
                server-operations]
         :as options'}
        (validate-options! options)

        report
        (check-operation-assembly options')]
    (when-not (valid? report)
      (throw
       (preflight-error
        :operation-assembly-preflight-failed
        "Gesso optimistic operation assembly preflight failed."
        {:preflight report})))
    (let [operations
          (into {}
                (map
                 (fn [[operation op-capability]]
                   [operation
                    (operation-realization
                     browser-assembly
                     (get server-operations operation)
                     operation
                     op-capability)]))
                operation-capabilities)

          route-requirements
          (into {}
                (map
                 (fn [operation]
                   [operation
                    (route-requirement
                     browser-assembly
                     operation)]))
                (keys operation-capabilities))

          assembly
          {:gesso.live.optimistic.preflight/type operation-assembly-type
           :gesso.live.optimistic.preflight/version preflight-version
           :name name
           :browser-assembly browser-assembly
           :operation-capabilities operation-capabilities
           :operations operations
           :route-requirements route-requirements}]
      (when-not (operation-assembly? assembly)
        (throw
         (preflight-error
          :invalid-emitted-operation-assembly
          "Optimistic operation preflight produced an internally inconsistent operation assembly."
          {:assembly assembly
           :preflight report})))
      assembly)))

(defn explain
  "Return a compact stable summary of an operation-assembly report or assembly."
  [value]
  (cond
    (operation-assembly? value)
    {:type operation-assembly-type
     :version preflight-version
     :name (:name value)
     :browser-assembly-name (get-in value [:browser-assembly :name])
     :browser-role (get-in value [:browser-assembly :browser-role])
     :command-transport
     (get-in value [:browser-assembly :optimistic-command-transport])
     :operations (set (keys (:operations value)))
     :route-requirements (:route-requirements value)}

    (report? value)
    {:type report-type
     :version preflight-version
     :valid? (:valid? value)
     :error-kinds (set (map :kind (:errors value)))
     :analysis (:analysis value)}

    :else
    (throw
     (preflight-error
      :not-explainable
      "Expected an optimistic operation preflight report or operation assembly."
      {:value value}))))
