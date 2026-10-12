(ns gesso.live.application-preflight.operation-summary
  "Source-derived, operation-keyed explanation of a current
   OperationAcquisitionAssembly.

   This internal module only reads already-admitted operation, route, execution,
   browser, and acquisition products and their freshly checked report. It does
   not own ApplicationAssembly recognition, physical artifact verification,
   rendered-surface correspondence, or the application-level assurance report.
   Describing an operation here grants no runtime authority and asserts no
   correctness for opaque model/policy code."
  (:require
   [gesso.live.operation-acquisition-preflight :as operation-acquisition]))

(def ^:private operation-acquisition-assembly-keys
  #{:gesso.live.operation-acquisition-preflight/type
    :gesso.live.operation-acquisition-preflight/version
    :name
    :execution-assembly
    :acquisition-assembly})

(defn- preflight-error
  [kind message data]
  (ex-info
   message
   (merge
    {:error/type :gesso.live.application-preflight/error
     :error/kind kind}
    data)))

(defn- closed-map?
  [expected value]
  (and
   (map? value)
   (= expected (set (keys value)))))

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

(defn browser-assembly
  [operation-acquisition-assembly]
  (:browser-assembly
   (operation-assembly operation-acquisition-assembly)))

(defn- acquisition-assembly
  [operation-acquisition-assembly]
  (:acquisition-assembly operation-acquisition-assembly))

(defn- required-capabilities-for-operation
  "Project a concrete operation's prerequisites from the *checked* execution
   assembly. The canonical product nests the per-operation sets beneath
   :required-by-operation; :supplied is a separate server-wide set and must
   never be mistaken for an operation's requirements.

   Keep the earlier flat projection as compatibility for callers that provide
   the legacy shape in controlled summary fixtures. Canonical execution
   assemblies always use the nested shape; this helper never treats either
   shape as evidence that the declared capabilities are actually satisfied by
   opaque host services beyond execution-preflight's stated contract."
  [execution-capabilities operation]
  (if (contains? execution-capabilities :required-by-operation)
    (get-in execution-capabilities [:required-by-operation operation])
    (get execution-capabilities operation)))

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

(defn operation-acquisition-assembly-shape?
  [value]
  (and
   (closed-map? operation-acquisition-assembly-keys value)
   (= operation-acquisition/operation-acquisition-assembly-type
      (:gesso.live.operation-acquisition-preflight/type value))
   (= operation-acquisition/preflight-version
      (:gesso.live.operation-acquisition-preflight/version value))
   (or (nil? (:name value))
       (keyword? (:name value)))))

(defn current-operation-acquisition-report
  "Return one freshly derived successful operation-acquisition report when the
   supplied closed-product shape and both embedded upstream assemblies remain
   current.  This internal helper deliberately uses the fresh report returned by
   check-operation-acquisition directly instead of asking report?/valid? to
   re-derive the same graph a second time."
  [value]
  (when (operation-acquisition-assembly-shape? value)
    (try
      (let [report
            (operation-acquisition/check-operation-acquisition
             {:name (:name value)
              :execution-assembly (:execution-assembly value)
              :acquisition-assembly (:acquisition-assembly value)})]
        (when (true? (:valid? report))
          report))
      (catch Exception _
        nil))))

(defn operation-summary-from-report
  [operation-acquisition-assembly operation-acquisition-report]
  (let [execution' (execution-assembly operation-acquisition-assembly)
        route' (route-assembly operation-acquisition-assembly)
        operation' (operation-assembly operation-acquisition-assembly)
        acquisition' (acquisition-assembly operation-acquisition-assembly)
        routes-by-operation (route-realizations-by-operation route')
        execution-capabilities (:execution-capabilities execution')
        settlement-contracts (:settlement-contracts execution')
        publication
        (get-in operation-acquisition-report
                [:analysis :published-change-topics-by-operation])
        affected-scopes
        (get-in operation-acquisition-report
                [:analysis :affected-scopes-by-operation])
        affected-fragments
        (get-in operation-acquisition-report
                [:analysis :affected-fragments-by-operation])]
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
            :execution-capabilities
            (required-capabilities-for-operation execution-capabilities operation)
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

;; Preserve the existing derivation and exception shapes. The calling facade
;; can delegate to these entry points without recomputing a report or creating
;; an independent registry of recognized application operations.

(defn operation-summary
  "Return one completely derived operation-keyed explanation for a current
   OperationAcquisitionAssembly.

   This is deliberately a view over nested closed products, not another stored
   registry.  It exposes enough of the current application backbone for tooling
   and LLM-assisted repair without forcing a consumer to reconstruct the chain
   manually from several namespaces.

   v623 derives one current operation-acquisition report and reuses its analysis
   instead of recursively re-recognizing the same nested assemblies through
   several public accessors."
  [operation-acquisition-assembly]
  (if-let [report
           (current-operation-acquisition-report
            operation-acquisition-assembly)]
    (operation-summary-from-report
     operation-acquisition-assembly
     report)
    (throw
     (preflight-error
      :invalid-operation-acquisition-assembly
      "Application operation summary requires a current OperationAcquisitionAssembly."
      {:operation-acquisition-assembly operation-acquisition-assembly}))))
