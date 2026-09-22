(ns gesso.live.optimistic.route-preflight
  "JVM-side preflight for optimistic transport -> declared trusted route closure.

   gesso.live.optimistic.preflight closes browser operation realization against
   the trusted optimistic server registry and emits one route requirement per
   browser-exposed semantic operation.  This namespace consumes those derived
   requirements and checks the next physical application edge:

     operation/plan -> command transport -> declared route capability

   Applications supply only physical route facts that cannot be derived from
   Choreo itself: route identity, HTTP method/path, semantic operation, and the
   transports that the route declares it accepts.  The required transport is
   never restated by this layer; it comes from the already-verified
   OptimisticOperationAssembly.

   Successful preflight emits one closed OptimisticRouteAssembly containing only
   the route realizations relevant to the browser-exposed operation set.  A
   larger application may therefore provide route capabilities for additional
   server-only or other-browser operations without forcing them into this
   assembly.

   This layer deliberately does NOT inspect or prove arbitrary Ring/Reitit/Biff
   handler bodies.  A route capability is an application-owned declaration that
   a concrete HTTP entrypoint accepts a transport for a semantic operation.  The
   checker establishes that every browser route obligation has at least one such
   compatible declaration and that the operation already resolves to the trusted
   server realization closed by optimistic.preflight.  Handler construction,
   authenticated principal binding, required/supplied execution capabilities,
   model correctness, settlement/progression closure, and Live acquisition
   remain later edges."
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [gesso.live.browser.preflight :as browser-preflight]
   [gesso.live.optimistic.preflight :as operation-preflight]))

;; =============================================================================
;; Identity
;; =============================================================================

(def preflight-version 2)

(def report-type
  :gesso.live.optimistic.route-preflight/report)

(def route-capability-type
  :gesso.live.optimistic.route-preflight/route-capability)

(def route-assembly-type
  :gesso.live.optimistic.route-preflight/route-assembly)

(def ^:private report-keys
  #{:gesso.live.optimistic.route-preflight/type
    :gesso.live.optimistic.route-preflight/version
    :valid?
    :errors
    :warnings
    :inputs
    :analysis})

(def ^:private analysis-keys
  #{:name
    :operation-assembly-name
    :required-operations
    :required-transports
    :declared-route-ids
    :declared-route-operations})

(def ^:private route-capability-keys
  #{:gesso.live.optimistic.route-preflight/type
    :gesso.live.optimistic.route-preflight/version
    :operation
    :method
    :path
    :transports})

(def ^:private route-capability-option-keys
  #{:operation
    :method
    :path
    :transports})

(def ^:private route-realization-keys
  #{:route-id
    :operation
    :method
    :path
    :accepted-transports
    :required-transport})

(def ^:private assembly-keys
  #{:gesso.live.optimistic.route-preflight/type
    :gesso.live.optimistic.route-preflight/version
    :name
    :operation-assembly
    :routes})

(def ^:private option-keys
  #{:name
    :operation-assembly
    :route-capabilities})

;; =============================================================================
;; Errors / primitive validation
;; =============================================================================

(defn- preflight-error
  [kind message data]
  (ex-info
   message
   (merge
    {:error/type :gesso.live.optimistic.route-preflight/error
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

(defn- nonblank-string?
  [value]
  (and (string? value)
       (not (str/blank? value))))

(defn- command-transport?
  [value]
  (and
   (contains? browser-preflight/command-transports value)
   (not= :none value)))

(defn- command-transport-set?
  [value]
  (and
   (set? value)
   (not (empty? value))
   (every? command-transport? value)))

;; =============================================================================
;; Route capability declarations
;; =============================================================================

(defn route-capability
  "Construct one canonical application-owned route capability declaration.

   Required:

     :operation
       Semantic operation accepted by this route.

     :method
       Physical HTTP/Ring method keyword.

     :path
       Non-blank route/path template.  Gesso deliberately treats it as an opaque
       physical coordinate; matching concrete runtime URLs to templates is a
       later affordance/route edge.

     :transports
       Non-empty set of Gesso command transports accepted by the route.  :none is
       never a route capability.

   The transport required by a browser assembly is NOT supplied here as a
   separate `:required-transport` option.  Route preflight derives that fact from
   the OptimisticOperationAssembly and checks membership in this declared set."
  [options]
  (let [options' (require-map! "Optimistic route capability" options)
        keys' (set (keys options'))
        missing (set/difference route-capability-option-keys keys')
        unknown (set/difference keys' route-capability-option-keys)]
    (when (seq missing)
      (throw
       (preflight-error
        :missing-route-capability-key
        "Optimistic route capability is missing required keys."
        {:missing-keys missing
         :required-keys route-capability-option-keys})))
    (when (seq unknown)
      (throw
       (preflight-error
        :unknown-route-capability-key
        "Optimistic route capability contains unknown keys."
        {:unknown-keys unknown
         :allowed-keys route-capability-option-keys})))
    (let [{:keys [operation method path transports]} options']
      (when-not (keyword? operation)
        (throw
         (preflight-error
          :invalid-route-operation
          "Optimistic route capability :operation must be a keyword."
          {:operation operation})))
      (when-not (keyword? method)
        (throw
         (preflight-error
          :invalid-route-method
          "Optimistic route capability :method must be a keyword."
          {:method method
           :operation operation})))
      (when-not (nonblank-string? path)
        (throw
         (preflight-error
          :invalid-route-path
          "Optimistic route capability :path must be a non-blank string."
          {:path path
           :operation operation})))
      (when-not (command-transport-set? transports)
        (throw
         (preflight-error
          :invalid-route-transports
          "Optimistic route capability :transports must be a non-empty set of supported command transports excluding :none."
          {:transports transports
           :supported-transports
           (disj browser-preflight/command-transports :none)
           :operation operation})))
      {:gesso.live.optimistic.route-preflight/type route-capability-type
       :gesso.live.optimistic.route-preflight/version preflight-version
       :operation operation
       :method method
       :path path
       :transports transports})))

(defn route-capability?
  "True for one closed current optimistic route capability declaration."
  [value]
  (and
   (map? value)
   (= route-capability-keys (set (keys value)))
   (= route-capability-type
      (:gesso.live.optimistic.route-preflight/type value))
   (= preflight-version
      (:gesso.live.optimistic.route-preflight/version value))
   (keyword? (:operation value))
   (keyword? (:method value))
   (nonblank-string? (:path value))
   (command-transport-set? (:transports value))))

(defn route-capabilities
  "Construct a closed route-id -> RouteCapability registry from plain options.

   Route ids are stable physical application identities.  The registry may be a
   superset of the browser-exposed operation set; route preflight selects only
   routes relevant to the supplied OptimisticOperationAssembly."
  [routes]
  (let [routes' (require-map! "Optimistic route capability registry" routes)]
    (when (empty? routes')
      (throw
       (preflight-error
        :empty-route-capabilities
        "Optimistic route capability registry must not be empty."
        {})))
    (into {}
          (map
           (fn [[route-id declaration]]
             (when-not (keyword? route-id)
               (throw
                (preflight-error
                 :invalid-route-id
                 "Optimistic route capability registry keys must be keywords."
                 {:route-id route-id})))
             [route-id
              (if (route-capability? declaration)
                declaration
                (route-capability declaration))]))
          routes')))

(defn route-capabilities?
  "True for a non-empty canonical route capability registry."
  [value]
  (and
   (map? value)
   (not (empty? value))
   (every?
    (fn [[route-id capability]]
      (and
       (keyword? route-id)
       (route-capability? capability)))
    value)))

;; =============================================================================
;; Assembly validation helpers
;; =============================================================================

(defn- validate-options!
  [options]
  (let [options' (require-map! "Optimistic route preflight options" options)
        unknown (set/difference (set (keys options')) option-keys)]
    (when (seq unknown)
      (throw
       (preflight-error
        :unknown-option-keys
        "Optimistic route preflight options contain unknown keys."
        {:unknown-keys unknown
         :allowed-keys option-keys})))
    (doseq [required [:operation-assembly :route-capabilities]]
      (when-not (contains? options' required)
        (throw
         (preflight-error
          :missing-option-key
          "Optimistic route preflight is missing a required option."
          {:missing-key required
           :required-keys #{:operation-assembly :route-capabilities}}))))
    (when-not (or (nil? (:name options'))
                  (keyword? (:name options')))
      (throw
       (preflight-error
        :invalid-name
        "Optimistic route assembly :name must be nil or a keyword."
        {:name (:name options')})))
    options'))

(defn- route-endpoint
  [capability]
  [(:method capability) (:path capability)])

(defn- duplicate-endpoints
  [route-capabilities]
  (->> route-capabilities
       (group-by (comp route-endpoint val))
       (keep
        (fn [[endpoint entries]]
          (when (> (count entries) 1)
            {:endpoint endpoint
             :route-ids (set (map key entries))
             :operations (set (map (comp :operation val) entries))})))
       vec))

(defn- declared-routes-for-operation
  [route-capabilities operation]
  (into {}
        (filter
         (fn [[_ capability]]
           (= operation (:operation capability))))
        route-capabilities))

(defn- compatible-routes
  [routes required-transport]
  (into {}
        (filter
         (fn [[_ capability]]
           (contains? (:transports capability) required-transport)))
        routes))

(defn- operation-route-errors
  [operation route-requirement route-capabilities]
  (let [required-transport (:transport route-requirement)
        declared (declared-routes-for-operation route-capabilities operation)
        compatible (compatible-routes declared required-transport)]
    (cond
      (empty? declared)
      [(issue
        :missing-trusted-route
        "Browser-exposed optimistic operation has no declared route capability."
        {:operation operation
         :required-transport required-transport
         :available-route-ids (set (keys route-capabilities))
         :available-route-operations
         (set (map (comp :operation val) route-capabilities))})]

      (empty? compatible)
      [(issue
        :incompatible-route-transport
        "Declared routes for the optimistic operation do not accept the transport required by the browser operation assembly."
        {:operation operation
         :required-transport required-transport
         :route-capabilities
         (into {}
               (map
                (fn [[route-id capability]]
                  [route-id
                   {:method (:method capability)
                    :path (:path capability)
                    :transports (:transports capability)}]))
               declared)})]

      :else
      [])))

(defn- route-realization
  [route-id capability required-transport]
  {:route-id route-id
   :operation (:operation capability)
   :method (:method capability)
   :path (:path capability)
   :accepted-transports (:transports capability)
   :required-transport required-transport})

(defn- selected-route-realizations
  [operation-assembly route-capabilities]
  (into {}
        (mapcat
         (fn [[operation requirement]]
           (let [required-transport (:transport requirement)]
             (for [[route-id capability]
                   (compatible-routes
                    (declared-routes-for-operation route-capabilities operation)
                    required-transport)]
               [route-id
                (route-realization
                 route-id
                 capability
                 required-transport)]))))
        (:route-requirements operation-assembly)))

;; =============================================================================
;; Preflight report
;; =============================================================================

(defn check-route-assembly
  "Check optimistic operation -> transport -> declared route closure.

   Returns a structured report and never throws for ordinary assembly
   contradictions.  Malformed checker options themselves fail immediately."
  [options]
  (let [{:keys [name operation-assembly route-capabilities]}
        (validate-options! options)

        operation-assembly-valid?
        (operation-preflight/operation-assembly? operation-assembly)

        route-capabilities-valid?
        (route-capabilities? route-capabilities)

        duplicates
        (if route-capabilities-valid?
          (duplicate-endpoints route-capabilities)
          [])

        base-errors
        (cond-> []
          (not operation-assembly-valid?)
          (conj
           (issue
            :invalid-operation-assembly
            "Optimistic route preflight requires a current untampered OptimisticOperationAssembly."
            {:operation-assembly operation-assembly}))

          (not route-capabilities-valid?)
          (conj
           (issue
            :invalid-route-capabilities
            "Optimistic route preflight requires a non-empty canonical route capability registry."
            {:route-capabilities route-capabilities}))

          (seq duplicates)
          (conj
           (issue
            :duplicate-route-endpoint
            "Multiple optimistic route capabilities declare the same HTTP method/path endpoint."
            {:duplicates duplicates})))

        closure-errors
        (if (and operation-assembly-valid?
                 route-capabilities-valid?)
          (vec
           (mapcat
            (fn [[operation requirement]]
              (operation-route-errors
               operation
               requirement
               route-capabilities))
            (sort-by (comp pr-str key)
                     (:route-requirements operation-assembly))))
          [])

        errors
        (vec (concat base-errors closure-errors))]
    {:gesso.live.optimistic.route-preflight/type report-type
     :gesso.live.optimistic.route-preflight/version preflight-version
     :valid? (empty? errors)
     :errors errors
     :warnings []
     :inputs options
     :analysis
     {:name name
      :operation-assembly-name
      (when operation-assembly-valid? (:name operation-assembly))
      :required-operations
      (if operation-assembly-valid?
        (set (keys (:route-requirements operation-assembly)))
        #{})
      :required-transports
      (if operation-assembly-valid?
        (into {}
              (map
               (fn [[operation requirement]]
                 [operation (:transport requirement)]))
              (:route-requirements operation-assembly))
        {})
      :declared-route-ids
      (if route-capabilities-valid?
        (set (keys route-capabilities))
        #{})
      :declared-route-operations
      (if route-capabilities-valid?
        (set (map (comp :operation val) route-capabilities))
        #{})}}))

(defn report?
  "True only when value is exactly the current route-preflight report derivable
   from its embedded preflight inputs.

   Recognition deliberately re-runs check-route-assembly. A caller cannot forge
   a positive report by editing derived operation sets, transport requirements,
   declared route summaries, errors, or validity while retaining a plausible
   report shape."
  [value]
  (and
   (map? value)
   (= report-keys (set (keys value)))
   (= report-type
      (:gesso.live.optimistic.route-preflight/type value))
   (= preflight-version
      (:gesso.live.optimistic.route-preflight/version value))
   (boolean? (:valid? value))
   (vector? (:errors value))
   (vector? (:warnings value))
   (map? (:inputs value))
   (map? (:analysis value))
   (= analysis-keys
      (set (keys (:analysis value))))
   (set? (get-in value [:analysis :required-operations]))
   (map? (get-in value [:analysis :required-transports]))
   (set? (get-in value [:analysis :declared-route-ids]))
   (set? (get-in value [:analysis :declared-route-operations]))
   (= (:valid? value)
      (empty? (:errors value)))
   (try
     (= value
        (check-route-assembly (:inputs value)))
     (catch Exception _
       false))))

(defn valid?
  "True only for a recognized successful route-preflight report."
  [report]
  (and (report? report)
       (true? (:valid? report))))

;; =============================================================================
;; Closed successful product
;; =============================================================================

(defn- route-realization?
  [operation-assembly route-id value]
  (let [operation (:operation value)
        requirement
        (get-in operation-assembly [:route-requirements operation])]
    (and
     (map? value)
     (= route-realization-keys (set (keys value)))
     (= route-id (:route-id value))
     (keyword? route-id)
     (keyword? operation)
     (keyword? (:method value))
     (nonblank-string? (:path value))
     (command-transport-set? (:accepted-transports value))
     (operation-preflight/route-requirement? requirement)
     (= operation (:operation requirement))
     (= (:transport requirement)
        (:required-transport value))
     (contains? (:accepted-transports value)
                (:required-transport value)))))

(defn route-assembly?
  "True when value is a closed current OptimisticRouteAssembly.

   Recognition rechecks the embedded OptimisticOperationAssembly and every
   operation/transport/route relation.  Route capability declarations remain
   physical application facts rather than machine-derived semantics; this
   predicate therefore establishes internal assembly closure, not correctness of
   arbitrary HTTP handler implementation."
  [value]
  (and
   (map? value)
   (= assembly-keys (set (keys value)))
   (= route-assembly-type
      (:gesso.live.optimistic.route-preflight/type value))
   (= preflight-version
      (:gesso.live.optimistic.route-preflight/version value))
   (or (nil? (:name value))
       (keyword? (:name value)))
   (operation-preflight/operation-assembly?
    (:operation-assembly value))
   (map? (:routes value))
   (not (empty? (:routes value)))
   (every?
    (fn [[route-id realization]]
      (route-realization?
       (:operation-assembly value)
       route-id
       realization))
    (:routes value))
   (empty?
    (duplicate-endpoints
     (into {}
           (map
            (fn [[route-id realization]]
              [route-id
               {:method (:method realization)
                :path (:path realization)}]))
           (:routes value))))
   (every?
    (fn [[operation requirement]]
      (some
       (fn [[_ realization]]
         (and
          (= operation (:operation realization))
          (= (:transport requirement)
             (:required-transport realization))))
       (:routes value)))
    (:route-requirements (:operation-assembly value)))))

(defn require-route-assembly!
  "Run optimistic route closure preflight and return one closed
   OptimisticRouteAssembly.

   The resulting product proves/checks only declared route capability closure:
   every browser-exposed optimistic operation has at least one route declaration
   accepting its already-derived command transport, and that operation is already
   closed against the trusted server registry by the embedded operation
   assembly."
  [options]
  (let [{:keys [name operation-assembly route-capabilities]
         :as options'}
        (validate-options! options)

        report
        (check-route-assembly options')]
    (when-not (valid? report)
      (throw
       (preflight-error
        :route-assembly-preflight-failed
        "Gesso optimistic route assembly preflight failed."
        {:preflight report})))
    (let [assembly
          {:gesso.live.optimistic.route-preflight/type route-assembly-type
           :gesso.live.optimistic.route-preflight/version preflight-version
           :name name
           :operation-assembly operation-assembly
           :routes
           (selected-route-realizations
            operation-assembly
            route-capabilities)}]
      (when-not (route-assembly? assembly)
        (throw
         (preflight-error
          :invalid-emitted-route-assembly
          "Optimistic route preflight produced an internally inconsistent route assembly."
          {:assembly assembly
           :preflight report})))
      assembly)))

(defn routes-for-operation
  "Return the closed route realizations for one semantic operation.

   Unknown operations return an empty vector; callers that need fail-closed
   operation lookup should check membership in the embedded operation assembly
   first."
  [route-assembly operation]
  (when-not (route-assembly? route-assembly)
    (throw
     (preflight-error
      :invalid-route-assembly
      "Expected a current OptimisticRouteAssembly."
      {:route-assembly route-assembly})))
  (->> (:routes route-assembly)
       vals
       (filter #(= operation (:operation %)))
       (sort-by (juxt :method :path :route-id))
       vec))

(defn explain
  "Return a compact stable summary of a route-preflight report or assembly."
  [value]
  (cond
    (route-assembly? value)
    {:type route-assembly-type
     :version preflight-version
     :name (:name value)
     :operation-assembly-name (get-in value [:operation-assembly :name])
     :operations
     (set (keys (get-in value [:operation-assembly :route-requirements])))
     :routes (:routes value)}

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
      "Expected an optimistic route preflight report or route assembly."
      {:value value}))))
