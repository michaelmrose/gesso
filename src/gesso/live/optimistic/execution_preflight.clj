(ns gesso.live.optimistic.execution-preflight
  "JVM-side preflight joining trusted HTTP route closure to one concrete
   optimistic server execution boundary.

   Earlier Gesso layers establish:

     semantic operation
       -> browser/server operation correspondence
       -> required command transport
       -> compatible declared trusted route

   and optimistic.server establishes independently:

     trusted operation
       -> required execution capabilities
       -> concrete server boundary supplies those capabilities

   This namespace closes those two facts against the *same prepared server*.
   For every route-exposed optimistic operation it requires:

   - one current untampered OptimisticRouteAssembly;
   - one current prepared optimistic.server adapter;
   - the prepared server contains the same semantic operation;
   - its choreography identity, browser role, authority role, and exact
     authority ExecutablePlan agree with the operation realization already
     accepted upstream; and
   - the prepared server itself remains execution-capability closed.

   Successful preflight emits one OptimisticExecutionAssembly containing the
   actual prepared server boundary plus derived, inspectable execution
   capabilities and settlement contracts.  Keeping the prepared server in the
   JVM-side product lets recognition recheck the exact trusted operation entries
   rather than trusting copied summaries.

   Settlement contracts are carried outward by derivation, not restatement.  If
   a prepared operation constrains :confirmed to a specific domain outcome,
   committed provenance, and authoritative progression, the execution assembly
   exposes exactly that canonical contract for later whole-application closure.
   An operation with no contract remains explicitly unconstrained here.

   This layer does NOT prove that an application-declared capability provider is
   correct, that trusted commit/progression evidence is truthful, that the
   current principal is authorized by domain policy, that an arbitrary Ring/Biff
   handler body invokes this server, or that Live acquisition is closed.  Those
   remain trusted/runtime facts or later application-assembly edges."
  (:require
   [clojure.set :as set]
   [gesso.choreo.artifact :as choreo-artifact]
   [gesso.live.optimistic.preflight :as operation-preflight]
   [gesso.live.optimistic.route-preflight :as route-preflight]
   [gesso.live.optimistic.server :as optimistic-server]))

;; =============================================================================
;; Identity
;; =============================================================================

(def preflight-version 2)

(def report-type
  :gesso.live.optimistic.execution-preflight/report)

(def execution-assembly-type
  :gesso.live.optimistic.execution-preflight/execution-assembly)

(def ^:private report-keys
  #{:gesso.live.optimistic.execution-preflight/type
    :gesso.live.optimistic.execution-preflight/version
    :valid?
    :errors
    :warnings
    :analysis})

(def ^:private assembly-keys
  #{:gesso.live.optimistic.execution-preflight/type
    :gesso.live.optimistic.execution-preflight/version
    :name
    :route-assembly
    :server
    :execution-capabilities
    :settlement-contracts})

(def ^:private capability-summary-keys
  #{:supplied
    :required-by-operation})

(def ^:private option-keys
  #{:name
    :route-assembly
    :server})

;; =============================================================================
;; Errors / validation helpers
;; =============================================================================

(defn- preflight-error
  [kind message data]
  (ex-info
   message
   (merge
    {:error/type :gesso.live.optimistic.execution-preflight/error
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
  (let [options' (require-map! "Optimistic execution preflight options" options)
        unknown (set/difference (set (keys options')) option-keys)]
    (when (seq unknown)
      (throw
       (preflight-error
        :unknown-option-keys
        "Optimistic execution preflight options contain unknown keys."
        {:unknown-keys unknown
         :allowed-keys option-keys})))
    (doseq [required [:route-assembly :server]]
      (when-not (contains? options' required)
        (throw
         (preflight-error
          :missing-option-key
          "Optimistic execution preflight is missing a required option."
          {:missing-key required
           :required-keys #{:route-assembly :server}}))))
    (when-not (or (nil? (:name options'))
                  (keyword? (:name options')))
      (throw
       (preflight-error
        :invalid-name
        "Optimistic execution assembly :name must be nil or a keyword."
        {:name (:name options')})))
    options'))

;; =============================================================================
;; Upstream/static correspondence helpers
;; =============================================================================

(defn- operation-realizations
  [route-assembly]
  (get-in route-assembly [:operation-assembly :operations]))

(defn- operation-realization
  [route-assembly operation]
  (get (operation-realizations route-assembly) operation))

(defn- server-operation
  [prepared-server operation]
  (get-in prepared-server [:operations operation]))

(defn- server-operation-authority-digest
  [prepared-operation]
  (when (optimistic-server/operation? prepared-operation)
    (choreo-artifact/executable-digest
     (:authority-plan prepared-operation))))

(defn- static-operation-corresponds?
  [route-realization prepared-operation]
  (and
   (map? route-realization)
   (optimistic-server/operation? prepared-operation)
   (= (:operation route-realization)
      (:operation prepared-operation))
   (= (:choreography-name route-realization)
      (:name prepared-operation))
   (= (:browser-role route-realization)
      (:browser-role prepared-operation))
   (= (:authority-role route-realization)
      (:authority-role prepared-operation))
   (= (:authority-plan-digest route-realization)
      (server-operation-authority-digest prepared-operation))))

(defn- required-operations
  [route-assembly]
  (set (keys (operation-realizations route-assembly))))

(defn- execution-capability-summary
  [route-assembly prepared-server]
  {:supplied
   (optimistic-server/server-supplied-capabilities prepared-server)

   :required-by-operation
   (into {}
         (map
          (fn [operation]
            [operation
             (optimistic-server/operation-required-capabilities
              (server-operation prepared-server operation))]))
         (sort-by pr-str (required-operations route-assembly)))})

(defn- capability-summary?
  [route-assembly prepared-server value]
  (and
   (map? value)
   (= capability-summary-keys (set (keys value)))
   (optimistic-server/execution-capabilities? (:supplied value))
   (map? (:required-by-operation value))
   (= value
      (execution-capability-summary route-assembly prepared-server))))

(defn- settlement-contract-summary
  [route-assembly prepared-server]
  (into {}
        (map
         (fn [operation]
           [operation
            (optimistic-server/operation-settlement-contract
             (server-operation prepared-server operation))]))
        (sort-by pr-str (required-operations route-assembly))))

(defn- settlement-contract-summary?
  [route-assembly prepared-server value]
  (and
   (map? value)
   (= value
      (settlement-contract-summary route-assembly prepared-server))))

;; =============================================================================
;; Per-operation correspondence
;; =============================================================================

(defn- operation-errors
  [route-assembly prepared-server operation]
  (let [realization (operation-realization route-assembly operation)
        prepared-operation (server-operation prepared-server operation)]
    (cond-> []
      (nil? prepared-operation)
      (conj
       (issue
        :missing-server-boundary-operation
        "Route-exposed optimistic operation is absent from the prepared server boundary."
        {:operation operation
         :registered-server-operations
         (set (keys (:operations prepared-server)))}))

      (and prepared-operation
           (not (static-operation-corresponds?
                 realization
                 prepared-operation)))
      (conj
       (issue
        :server-boundary-operation-correspondence-mismatch
        "Prepared server operation does not exactly match the trusted operation realization already closed upstream."
        {:operation operation
         :expected
         {:choreography-name (:choreography-name realization)
          :browser-role (:browser-role realization)
          :authority-role (:authority-role realization)
          :authority-plan-digest (:authority-plan-digest realization)}
         :actual
         {:choreography-name (:name prepared-operation)
          :browser-role (:browser-role prepared-operation)
          :authority-role (:authority-role prepared-operation)
          :authority-plan-digest
          (server-operation-authority-digest prepared-operation)}})))))

;; =============================================================================
;; Public report
;; =============================================================================

(defn check-execution-assembly
  "Return a structured report joining route closure to one concrete prepared
   optimistic server boundary.

   Required options:

     :route-assembly
       Current OptimisticRouteAssembly.

     :server
       Current prepared optimistic.server adapter.  Because server? includes the
       required/supplied execution-capability relation, accepting this value also
       requires every registered operation to remain capability-closed.

   Optional :name identifies the application slice diagnostically.

   The server may contain additional trusted operations that are not exposed by
   the route assembly.  Every route-exposed operation must, however, correspond
   exactly to the same prepared trusted operation on this concrete server."
  [options]
  (let [{:keys [name route-assembly server]}
        (validate-options! options)

        route-assembly-valid?
        (route-preflight/route-assembly? route-assembly)

        server-valid?
        (optimistic-server/server? server)

        base-errors
        (cond-> []
          (not route-assembly-valid?)
          (conj
           (issue
            :invalid-route-assembly
            "Optimistic execution preflight requires a current untampered OptimisticRouteAssembly."
            {:route-assembly route-assembly}))

          (not server-valid?)
          (conj
           (issue
            :invalid-server-boundary
            "Optimistic execution preflight requires a current prepared optimistic server boundary with closed execution capabilities."
            {:server server})))

        correspondence-errors
        (if (and route-assembly-valid? server-valid?)
          (vec
           (mapcat
            (fn [operation]
              (operation-errors route-assembly server operation))
            (sort-by pr-str (required-operations route-assembly))))
          [])

        errors
        (vec (concat base-errors correspondence-errors))]
    {:gesso.live.optimistic.execution-preflight/type report-type
     :gesso.live.optimistic.execution-preflight/version preflight-version
     :valid? (empty? errors)
     :errors errors
     :warnings []
     :analysis
     {:name name
      :route-assembly-name
      (when route-assembly-valid? (:name route-assembly))
      :required-operations
      (if route-assembly-valid?
        (required-operations route-assembly)
        #{})
      :registered-server-operations
      (if server-valid?
        (set (keys (:operations server)))
        #{})
      :supplied-capabilities
      (if server-valid?
        (optimistic-server/server-supplied-capabilities server)
        #{})
      :required-capabilities-by-operation
      (if (and route-assembly-valid? server-valid?)
        (into {}
              (keep
               (fn [operation]
                 (when-let [prepared-operation
                            (server-operation server operation)]
                   [operation
                    (optimistic-server/operation-required-capabilities
                     prepared-operation)])))
              (sort-by pr-str (required-operations route-assembly)))
        {})
      :settlement-contracts-by-operation
      (if (and route-assembly-valid? server-valid?)
        (into {}
              (keep
               (fn [operation]
                 (when-let [prepared-operation
                            (server-operation server operation)]
                   [operation
                    (optimistic-server/operation-settlement-contract
                     prepared-operation)])))
              (sort-by pr-str (required-operations route-assembly)))
        {})}}))

(defn report?
  "True when value has the closed shape of a current execution-preflight report
   and :valid? agrees with its error vector."
  [value]
  (and
   (map? value)
   (= report-keys (set (keys value)))
   (= report-type
      (:gesso.live.optimistic.execution-preflight/type value))
   (= preflight-version
      (:gesso.live.optimistic.execution-preflight/version value))
   (boolean? (:valid? value))
   (vector? (:errors value))
   (vector? (:warnings value))
   (map? (:analysis value))
   (= (:valid? value)
      (empty? (:errors value)))))

(defn valid?
  "True only for a recognized successful execution-preflight report."
  [report]
  (and (report? report)
       (true? (:valid? report))))

;; =============================================================================
;; Closed successful product
;; =============================================================================

(defn execution-assembly?
  "True when value is a closed current OptimisticExecutionAssembly.

   Recognition rechecks the embedded route assembly and the actual prepared
   server boundary, then re-runs exact route-exposed operation correspondence.
   The capability and settlement-contract summaries are derived from that
   embedded server and must match it exactly; neither is an independently
   authorable second registry."
  [value]
  (and
   (map? value)
   (= assembly-keys (set (keys value)))
   (= execution-assembly-type
      (:gesso.live.optimistic.execution-preflight/type value))
   (= preflight-version
      (:gesso.live.optimistic.execution-preflight/version value))
   (or (nil? (:name value))
       (keyword? (:name value)))
   (route-preflight/route-assembly? (:route-assembly value))
   (optimistic-server/server? (:server value))
   (empty?
    (mapcat
     (fn [operation]
       (operation-errors
        (:route-assembly value)
        (:server value)
        operation))
     (sort-by pr-str
              (required-operations (:route-assembly value)))))
   (capability-summary?
    (:route-assembly value)
    (:server value)
    (:execution-capabilities value))
   (settlement-contract-summary?
    (:route-assembly value)
    (:server value)
    (:settlement-contracts value))))

(defn require-execution-assembly!
  "Run execution-boundary preflight and return one closed
   OptimisticExecutionAssembly.

   The returned JVM-side product carries the concrete prepared server so later
   application assembly does not have to rediscover which capability-closed
   trusted boundary was checked against the route graph."
  [options]
  (let [{:keys [name route-assembly server]
         :as options'}
        (validate-options! options)

        report
        (check-execution-assembly options')]
    (when-not (valid? report)
      (throw
       (preflight-error
        :execution-assembly-preflight-failed
        "Gesso optimistic execution assembly preflight failed."
        {:preflight report})))
    (let [assembly
          {:gesso.live.optimistic.execution-preflight/type execution-assembly-type
           :gesso.live.optimistic.execution-preflight/version preflight-version
           :name name
           :route-assembly route-assembly
           :server server
           :execution-capabilities
           (execution-capability-summary route-assembly server)
           :settlement-contracts
           (settlement-contract-summary route-assembly server)}]
      (when-not (execution-assembly? assembly)
        (throw
         (preflight-error
          :invalid-emitted-execution-assembly
          "Optimistic execution preflight produced an internally inconsistent execution assembly."
          {:assembly assembly
           :preflight report})))
      assembly)))

(defn execution-capabilities
  "Return the exact derived capability summary for a current execution assembly."
  [execution-assembly]
  (when-not (execution-assembly? execution-assembly)
    (throw
     (preflight-error
      :invalid-execution-assembly
      "Expected a current OptimisticExecutionAssembly."
      {:execution-assembly execution-assembly})))
  (:execution-capabilities execution-assembly))

(defn settlement-contracts
  "Return the exact operation-keyed settlement-contract summary for a current
   execution assembly.  Every route-exposed operation is present; nil means the
   trusted operation intentionally declares no settlement constraint yet."
  [execution-assembly]
  (when-not (execution-assembly? execution-assembly)
    (throw
     (preflight-error
      :invalid-execution-assembly
      "Expected a current OptimisticExecutionAssembly."
      {:execution-assembly execution-assembly})))
  (:settlement-contracts execution-assembly))

(defn explain
  "Return a compact stable summary of an execution-preflight report or assembly."
  [value]
  (cond
    (execution-assembly? value)
    {:type execution-assembly-type
     :version preflight-version
     :name (:name value)
     :route-assembly-name (get-in value [:route-assembly :name])
     :operations (required-operations (:route-assembly value))
     :execution-capabilities (:execution-capabilities value)
     :settlement-contracts (:settlement-contracts value)}

    (report? value)
    {:type report-type
     :version preflight-version
     :valid? (:valid? value)
     :errors (:errors value)
     :warnings (:warnings value)
     :analysis (:analysis value)}

    :else
    (throw
     (preflight-error
      :unrecognized-value
      "Expected an optimistic execution-preflight report or execution assembly."
      {:value value}))))
