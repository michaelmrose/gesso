(ns gesso.live.application-preflight
  "JVM-side whole-application backbone preflight for one assembled Gesso Live
   application.

   Earlier preflight layers deliberately close one relation at a time.  By the
   time an OperationAcquisitionAssembly exists it already nests the exact chain:

     BrowserAssemblyManifest
       -> optimistic operation correspondence
       -> trusted transport/route correspondence
       -> concrete prepared server + execution capabilities
       -> settlement/progression contracts
       -> trusted semantic publication declarations
       -> compiled Live invalidation topology
       -> authoritative acquisition realizations

   This namespace composes that chain with the remaining physical browser-build
   fact: the exact generated JavaScript artifact currently on disk.  Callers do
   not repeat operation registries, routes, capabilities, settlement contracts,
   publication topics, affected fragments, or acquisition obligations.

   A successful ApplicationAssembly is deliberately described as a *runtime
   backbone* closure, not yet as complete whole-application closure.  Gesso does
   not currently have a static enumerable registry of every rendered Choreo
   affordance, so the edge:

     rendered affordance -> semantic operation

   remains an explicit open obligation at this layer.  Runtime view composition
   still fails unknown :choreo/op bindings locally, but that is not equivalent to
   proving ahead of rendering that every application affordance has been
   enumerated.

   ApplicationAssembly is also intentionally physical rather than portable:
   recognition re-verifies the current artifact bytes and receipt against the
   nested BrowserAssemblyManifest.  If the generated file or receipt changes,
   the previously emitted assembly no longer recognizes as current.

   The guarantee is therefore verified assembly relative to named trusted
   application declarations and physical boundaries.  It is not the v4.5
   universal projection/refinement theorem, does not prove arbitrary handler or
   query implementations, and does not turn browser metadata into authority."
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [gesso.live.browser.build :as browser-build]
   [gesso.live.operation-acquisition-preflight :as operation-acquisition]
   [gesso.live.optimistic.execution-preflight :as execution]))

;; =============================================================================
;; Identity / closed vocabulary
;; =============================================================================

(def preflight-version 1)

(def report-type
  :gesso.live.application-preflight/report)

(def application-assembly-type
  :gesso.live.application-preflight/application-assembly)

(def backbone-guarantee
  :application-runtime-backbone-preflight-closed)

(def ^:private option-keys
  #{:name
    :operation-acquisition-assembly
    :browser-artifact-path
    :browser-receipt-path})

(def ^:private report-keys
  #{:gesso.live.application-preflight/type
    :gesso.live.application-preflight/version
    :valid?
    :errors
    :warnings
    :analysis})

(def ^:private analysis-keys
  #{:name
    :operation-acquisition-assembly
    :browser-artifact-path
    :browser-receipt-path
    :operations
    :artifact-receipt
    :open-obligations
    :trusted-assumptions})

(def ^:private assembly-keys
  #{:gesso.live.application-preflight/type
    :gesso.live.application-preflight/version
    :name
    :operation-acquisition-assembly
    :browser-artifact-path
    :browser-receipt-path})

(def ^:private affordance-open-obligation
  {:kind :static-affordance-closure-not-yet-modeled
   :edge :rendered-affordance->semantic-operation
   :status :open
   :message
   "Gesso does not yet have a static enumerable registry of every rendered Choreo affordance; runtime :choreo/op resolution is fail-closed but cannot yet discharge whole-application affordance enumeration during preflight."})

(def ^:private trusted-assumptions
  #{:application-publication-declarations-match-model-publication
    :trusted-command-route-declarations-match-installed-handlers
    :trusted-acquisition-route-declarations-match-installed-handlers
    :browser-artifact-emitted-through-gesso-owned-application-build
    :clojurescript-compiler-realizes-generated-entrypoint
    :authenticated-principal-binding-is-correct
    :public-model-operation-contracts-hold
    :xtdb-transaction-and-consistency-contracts-hold})

;; =============================================================================
;; Errors / option validation
;; =============================================================================

(defn- preflight-error
  [kind message data]
  (ex-info
   message
   (merge
    {:error/type :gesso.live.application-preflight/error
     :error/kind kind}
    data)))

(defn- issue
  [kind message data]
  (merge
   {:kind kind
    :message message}
   data))

(defn- closed-map?
  [expected value]
  (and
   (map? value)
   (= expected (set (keys value)))))

(defn- nonblank-string?
  [value]
  (and
   (string? value)
   (not (str/blank? value))))

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
  (let [options' (require-map! "Application preflight options" options)
        unknown (set/difference (set (keys options')) option-keys)]
    (when (seq unknown)
      (throw
       (preflight-error
        :unknown-option-keys
        "Application preflight options contain unknown keys."
        {:unknown-keys unknown
         :allowed-keys option-keys})))
    (doseq [required [:operation-acquisition-assembly :browser-artifact-path]]
      (when-not (contains? options' required)
        (throw
         (preflight-error
          :missing-option
          "Application preflight is missing a required option."
          {:required required
           :provided-keys (set (keys options'))}))))
    (when (and (contains? options' :name)
               (some? (:name options'))
               (not (keyword? (:name options'))))
      (throw
       (preflight-error
        :invalid-name
        "Application preflight :name must be nil or a keyword."
        {:name (:name options')})))
    (when-not (nonblank-string? (:browser-artifact-path options'))
      (throw
       (preflight-error
        :invalid-browser-artifact-path
        "Application preflight :browser-artifact-path must be a non-blank string."
        {:browser-artifact-path (:browser-artifact-path options')})))
    (when (and (contains? options' :browser-receipt-path)
               (some? (:browser-receipt-path options'))
               (not (nonblank-string? (:browser-receipt-path options'))))
      (throw
       (preflight-error
        :invalid-browser-receipt-path
        "Application preflight :browser-receipt-path must be nil or a non-blank string."
        {:browser-receipt-path (:browser-receipt-path options')})))
    options'))

;; =============================================================================
;; Nested assembly navigation / derivation
;; =============================================================================

(defn- execution-assembly
  [operation-acquisition-assembly]
  (:execution-assembly operation-acquisition-assembly))

(defn- route-assembly
  [operation-acquisition-assembly]
  (:route-assembly
   (execution-assembly operation-acquisition-assembly)))

(defn- operation-assembly
  [operation-acquisition-assembly]
  (:operation-assembly
   (route-assembly operation-acquisition-assembly)))

(defn- browser-assembly
  [operation-acquisition-assembly]
  (:browser-assembly
   (operation-assembly operation-acquisition-assembly)))

(defn- acquisition-assembly
  [operation-acquisition-assembly]
  (:acquisition-assembly operation-acquisition-assembly))

(defn- route-realizations-by-operation
  [route-assembly']
  (reduce-kv
   (fn [acc route-id realization]
     (update
      acc
      (:operation realization)
      (fnil conj [])
      {:route-id route-id
       :method (:method realization)
       :path (:path realization)
       :required-transport (:required-transport realization)}))
   (sorted-map)
   (:routes route-assembly')))

(declare application-assembly?)

(defn operation-summary
  "Return one completely derived operation-keyed explanation for a current
   OperationAcquisitionAssembly.

   This is deliberately a view over nested closed products, not another stored
   registry.  It exposes enough of the current application backbone for tooling
   and LLM-assisted repair without forcing a consumer to reconstruct the chain
   manually from several namespaces."
  [operation-acquisition-assembly]
  (when-not
   (operation-acquisition/operation-acquisition-assembly?
    operation-acquisition-assembly)
    (throw
     (preflight-error
      :invalid-operation-acquisition-assembly
      "Application operation summary requires a current OperationAcquisitionAssembly."
      {:operation-acquisition-assembly operation-acquisition-assembly})))
  (let [execution' (execution-assembly operation-acquisition-assembly)
        route' (route-assembly operation-acquisition-assembly)
        operation' (operation-assembly operation-acquisition-assembly)
        acquisition' (acquisition-assembly operation-acquisition-assembly)
        routes-by-operation (route-realizations-by-operation route')
        execution-capabilities (execution/execution-capabilities execution')
        settlement-contracts (execution/settlement-contracts execution')
        publication (execution/published-change-topics execution')
        affected-scopes
        (operation-acquisition/affected-scopes operation-acquisition-assembly)
        affected-fragments
        (operation-acquisition/affected-fragments operation-acquisition-assembly)]
    (into
     (sorted-map)
     (map
      (fn [operation]
        (let [realization (get-in operation' [:operations operation])
              route-requirement
              (get-in operation' [:route-requirements operation])]
          [operation
           {:choreography-name (:choreography-name realization)
            :browser-role (:browser-role realization)
            :authority-role (:authority-role realization)
            :browser-plan-key (:plan-key realization)
            :browser-plan-digest (:browser-plan-digest realization)
            :authority-plan-digest (:authority-plan-digest realization)
            :trusted-operation-name
            (get-in execution' [:server :operations operation :name])
            :command-transport (:transport route-requirement)
            :routes (vec (sort-by (juxt :path :route-id)
                                  (get routes-by-operation operation [])))
            :execution-capabilities (get execution-capabilities operation)
            :settlement-contract (get settlement-contracts operation)
            :published-change-topics (get publication operation)
            :affected-scopes (get affected-scopes operation #{})
            :affected-fragments (get affected-fragments operation #{})
            :authoritative-acquisitions
            (into
             (sorted-map)
             (keep
              (fn [fragment]
                (when-let [acquisition-realization
                           (get-in acquisition' [:realizations fragment])]
                  [fragment
                   {:scope (:scope acquisition-realization)
                    :fragment-route
                    (select-keys (:fragment-route acquisition-realization)
                                 [:method :path :boundary])
                    :stream-route
                    (select-keys (:stream-route acquisition-realization)
                                 [:method :path :boundary])}]))
              (sort-by pr-str (get affected-fragments operation #{}))))}]))
      (sort-by pr-str (keys (:operations operation')))))))

(defn open-obligations
  "Return the currently known whole-application obligations that are not yet
   discharged by the modeled runtime-backbone preflight.

   v622 intentionally reports the missing static affordance-enumeration edge
   instead of demanding a duplicate application-authored affordance registry."
  [application-or-operation-acquisition]
  (cond
    (application-assembly? application-or-operation-acquisition)
    [affordance-open-obligation]

    (operation-acquisition/operation-acquisition-assembly?
     application-or-operation-acquisition)
    [affordance-open-obligation]

    :else
    (throw
     (preflight-error
      :invalid-application-input
      "Open-obligation inspection requires a current ApplicationAssembly or OperationAcquisitionAssembly."
      {:value application-or-operation-acquisition}))))

(defn- verify-browser-artifact
  [manifest artifact-path receipt-path]
  (try
    {:receipt
     (browser-build/verify-generated-artifact!
      manifest
      artifact-path
      (cond-> {}
        receipt-path
        (assoc :receipt-path receipt-path)))}
    (catch clojure.lang.ExceptionInfo error
      {:error
       (issue
        :browser-artifact-verification-failed
        "Application browser artifact does not currently correspond to the nested BrowserAssemblyManifest."
        {:browser-artifact-path artifact-path
         :browser-receipt-path receipt-path
         :cause-type (:error/type (ex-data error))
         :cause-kind (:error/kind (ex-data error))
         :cause-data (dissoc (ex-data error) :error/type :error/kind)})})
    (catch Throwable error
      {:error
       (issue
        :browser-artifact-verification-failed
        "Application browser artifact verification failed at the physical build boundary."
        {:browser-artifact-path artifact-path
         :browser-receipt-path receipt-path
         :exception-class (str (class error))
         :exception-message (.getMessage error)})})))

;; =============================================================================
;; Application-backbone report
;; =============================================================================

(defn check-application-assembly
  "Check the currently modeled whole-application runtime backbone.

   Required inputs:

     :operation-acquisition-assembly
       The closed semantic/runtime chain from browser operation through trusted
       execution, settlement publication, Live invalidation, and authoritative
       reacquisition.

     :browser-artifact-path
       The exact generated JavaScript artifact to verify physically against the
       BrowserAssemblyManifest nested in the supplied assembly.

   Optional :browser-receipt-path selects a non-default receipt path.

   A valid report means every *modeled runtime-backbone edge* is closed and the
   browser artifact is current.  It does not mean whole-application affordance
   enumeration is complete; see :open-obligations."
  [options]
  (let [{:keys [name
                operation-acquisition-assembly
                browser-artifact-path
                browser-receipt-path]}
        (validate-options! options)

        operation-acquisition-valid?
        (operation-acquisition/operation-acquisition-assembly?
         operation-acquisition-assembly)

        manifest
        (when operation-acquisition-valid?
          (browser-assembly operation-acquisition-assembly))

        artifact-verification
        (when manifest
          (verify-browser-artifact
           manifest
           browser-artifact-path
           browser-receipt-path))

        artifact-error
        (:error artifact-verification)

        errors
        (cond-> []
          (not operation-acquisition-valid?)
          (conj
           (issue
            :invalid-operation-acquisition-assembly
            "Application preflight requires a current untampered OperationAcquisitionAssembly."
            {:operation-acquisition-assembly operation-acquisition-assembly}))

          artifact-error
          (conj artifact-error))

        operation-summary'
        (if operation-acquisition-valid?
          (operation-summary operation-acquisition-assembly)
          (sorted-map))

        unhandled-topics
        (if operation-acquisition-valid?
          (:unhandled-published-change-topics
           (operation-acquisition/explain operation-acquisition-assembly))
          {})

        warnings
        (cond->
         [(issue
           :static-affordance-closure-not-yet-modeled
           "Static whole-application affordance enumeration is not yet modeled; this obligation remains open even when runtime-backbone preflight succeeds."
           {:edge :rendered-affordance->semantic-operation})]
          (seq unhandled-topics)
          (conj
           (issue
            :published-change-topics-not-consumed-by-live-app
            "Some trusted semantic publication topics are not consumed by this compiled Live application."
            {:topics-by-operation unhandled-topics})))

        obligations
        [affordance-open-obligation]]
    {:gesso.live.application-preflight/type report-type
     :gesso.live.application-preflight/version preflight-version
     :valid? (empty? errors)
     :errors (vec errors)
     :warnings (vec warnings)
     :analysis
     {:name name
      :operation-acquisition-assembly operation-acquisition-assembly
      :browser-artifact-path browser-artifact-path
      :browser-receipt-path browser-receipt-path
      :operations operation-summary'
      :artifact-receipt (:receipt artifact-verification)
      :open-obligations obligations
      :trusted-assumptions trusted-assumptions}}))

(defn report?
  "True only when value is exactly the current application report derivable from
   its embedded inputs and current physical browser artifact.

   Recognition intentionally re-hashes/re-verifies the browser artifact. A
   formerly valid report becomes unrecognized when artifact bytes or receipt
   metadata change."
  [value]
  (and
   (closed-map? report-keys value)
   (= report-type
      (:gesso.live.application-preflight/type value))
   (= preflight-version
      (:gesso.live.application-preflight/version value))
   (boolean? (:valid? value))
   (vector? (:errors value))
   (vector? (:warnings value))
   (closed-map? analysis-keys (:analysis value))
   (map? (get-in value [:analysis :operations]))
   (vector? (get-in value [:analysis :open-obligations]))
   (set? (get-in value [:analysis :trusted-assumptions]))
   (= (:valid? value)
      (empty? (:errors value)))
   (try
     (let [{:keys [name
                   operation-acquisition-assembly
                   browser-artifact-path
                   browser-receipt-path]}
           (:analysis value)]
       (= value
          (check-application-assembly
           {:name name
            :operation-acquisition-assembly operation-acquisition-assembly
            :browser-artifact-path browser-artifact-path
            :browser-receipt-path browser-receipt-path})))
     (catch Exception _
       false))))

(defn valid?
  "True only for a recognized report whose modeled runtime-backbone edges are
   closed. Open obligations outside the currently modeled backbone remain
   visible in :analysis and do not silently disappear."
  [report]
  (and
   (report? report)
   (true? (:valid? report))))

;; =============================================================================
;; Closed current physical application product
;; =============================================================================

(defn application-assembly?
  "True for one current ApplicationAssembly whose nested semantic/runtime
   backbone remains closed and whose physical browser artifact still verifies.

   This predicate performs physical file verification by design. Application
   assemblies are build/start-time currentness objects, not portable certificates."
  [value]
  (and
   (closed-map? assembly-keys value)
   (= application-assembly-type
      (:gesso.live.application-preflight/type value))
   (= preflight-version
      (:gesso.live.application-preflight/version value))
   (or (nil? (:name value))
       (keyword? (:name value)))
   (operation-acquisition/operation-acquisition-assembly?
    (:operation-acquisition-assembly value))
   (nonblank-string? (:browser-artifact-path value))
   (or (nil? (:browser-receipt-path value))
       (nonblank-string? (:browser-receipt-path value)))
   (try
     (valid?
      (check-application-assembly
       {:name (:name value)
        :operation-acquisition-assembly
        (:operation-acquisition-assembly value)
        :browser-artifact-path (:browser-artifact-path value)
        :browser-receipt-path (:browser-receipt-path value)}))
     (catch Exception _
       false))))

(defn require-application-assembly!
  "Require the currently modeled application runtime backbone and return one
   closed physical ApplicationAssembly.

   Success does not erase known whole-application obligations. Use
   open-obligations or explain to see the static affordance-enumeration edge that
   remains intentionally open in v622."
  [options]
  (let [{:keys [name
                operation-acquisition-assembly
                browser-artifact-path
                browser-receipt-path]
         :as options'}
        (validate-options! options)

        report
        (check-application-assembly options')]
    (when-not (valid? report)
      (throw
       (preflight-error
        :application-backbone-preflight-failed
        "Gesso whole-application runtime-backbone preflight failed."
        {:preflight report})))
    (let [assembly
          {:gesso.live.application-preflight/type application-assembly-type
           :gesso.live.application-preflight/version preflight-version
           :name name
           :operation-acquisition-assembly operation-acquisition-assembly
           :browser-artifact-path browser-artifact-path
           :browser-receipt-path browser-receipt-path}]
      (when-not (application-assembly? assembly)
        (throw
         (preflight-error
          :invalid-emitted-application-assembly
          "Application preflight produced an internally inconsistent or no-longer-current assembly."
          {:assembly assembly
           :preflight report})))
      assembly)))

(defn explain-operation
  "Explain one route-exposed operation through the assembled application
   backbone without requiring callers to traverse nested preflight products."
  [application-assembly operation]
  (when-not (application-assembly? application-assembly)
    (throw
     (preflight-error
      :invalid-application-assembly
      "Operation explanation requires a current ApplicationAssembly."
      {:application-assembly application-assembly})))
  (let [summary
        (operation-summary
         (:operation-acquisition-assembly application-assembly))]
    (when-not (contains? summary operation)
      (throw
       (preflight-error
        :unknown-operation
        "Application operation explanation references an operation outside the assembled route-exposed application slice."
        {:operation operation
         :available-operations (set (keys summary))})))
    (assoc
     (get summary operation)
     :operation operation
     :guarantee backbone-guarantee)))

(defn explain
  "Return one compact whole-application runtime-backbone explanation.

   The explanation intentionally separates:

     :guarantee        what the modeled assembly currently closes;
     :open-obligations what still lacks a whole-application preflight model;
     :trusted-assumptions facts outside the proved/assembly-verified core."
  [value]
  (cond
    (application-assembly? value)
    (let [report
          (check-application-assembly
           {:name (:name value)
            :operation-acquisition-assembly
            (:operation-acquisition-assembly value)
            :browser-artifact-path (:browser-artifact-path value)
            :browser-receipt-path (:browser-receipt-path value)})]
      {:type application-assembly-type
       :version preflight-version
       :name (:name value)
       :guarantee backbone-guarantee
       :operations (get-in report [:analysis :operations])
       :browser-artifact
       {:path (:browser-artifact-path value)
        :receipt-path
        (or (:browser-receipt-path value)
            (browser-build/receipt-path (:browser-artifact-path value)))
        :receipt (get-in report [:analysis :artifact-receipt])}
       :live-app-name
       (get-in value
               [:operation-acquisition-assembly
                :acquisition-assembly
                :live-app
                :name])
       :open-obligations (get-in report [:analysis :open-obligations])
       :trusted-assumptions (get-in report [:analysis :trusted-assumptions])
       :warnings (:warnings report)})

    (report? value)
    {:type report-type
     :version preflight-version
     :valid? (:valid? value)
     :errors (:errors value)
     :warnings (:warnings value)
     :analysis
     (select-keys
      (:analysis value)
      [:name
       :browser-artifact-path
       :browser-receipt-path
       :operations
       :open-obligations
       :trusted-assumptions])}

    :else
    (throw
     (preflight-error
      :unrecognized-value
      "Expected an application preflight report or current ApplicationAssembly."
      {:value value}))))
