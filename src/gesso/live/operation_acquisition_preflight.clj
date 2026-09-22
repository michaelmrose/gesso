(ns gesso.live.operation-acquisition-preflight
  "JVM-side preflight joining route-exposed optimistic operation publication
   declarations to one closed Gesso Live authoritative-acquisition assembly.

   Earlier layers establish independently:

     OptimisticExecutionAssembly
       operation -> trusted semantic Live publication topics

   and:

     AuthoritativeAcquisitionAssembly
       compiled Live invalidation graph
         -> graph-reachable scopes/fragments
         -> progression-bound managed acquisition realizations

   This namespace joins those closed products without asking applications to
   maintain an operation -> fragment registry.  For every route-exposed
   optimistic operation it derives:

     published change topic
       -> compiled Live invalidation targets
       -> affected semantic fragments
       -> already-closed authoritative acquisition realizations

   A nil publication declaration is an open application-assembly obligation and
   therefore fails this preflight.  An explicitly empty set is closed and means
   that the trusted operation declares no semantic Live publication.  A
   published topic absent from this compiled Live app is not structurally wrong:
   it may be consumed elsewhere, so it is surfaced as a warning rather than
   invented into the graph or rejected.

   Successful preflight emits one OperationAcquisitionAssembly containing only
   the two exact closed upstream products plus an optional diagnostic name.  All
   operation impact summaries are derived on demand; no second publication,
   operation -> fragment, or acquisition registry is stored.

   This remains verified assembly relative to trusted application publication
   declarations and trusted physical acquisition routes.  It does not prove that
   arbitrary model code actually emits its declared topics, that every published
   topic should affect this Live app, or that eventual network delivery occurs."
  (:require
   [clojure.set :as set]
   [gesso.live.acquisition-preflight :as acquisition]
   [gesso.live.optimistic.execution-preflight :as execution]))

;; =============================================================================
;; Identity
;; =============================================================================

(def preflight-version 1)

(def report-type
  :gesso.live.operation-acquisition-preflight/report)

(def operation-acquisition-assembly-type
  :gesso.live.operation-acquisition-preflight/operation-acquisition-assembly)

(def ^:private option-keys
  #{:name
    :execution-assembly
    :acquisition-assembly})

(def ^:private report-keys
  #{:gesso.live.operation-acquisition-preflight/type
    :gesso.live.operation-acquisition-preflight/version
    :valid?
    :errors
    :warnings
    :analysis})

(def ^:private analysis-keys
  #{:name
    :execution-assembly
    :acquisition-assembly
    :operations
    :published-change-topics-by-operation
    :known-live-change-topics
    :affected-scopes-by-operation
    :affected-fragments-by-operation
    :unhandled-published-change-topics-by-operation})

(def ^:private assembly-keys
  #{:gesso.live.operation-acquisition-preflight/type
    :gesso.live.operation-acquisition-preflight/version
    :name
    :execution-assembly
    :acquisition-assembly})

;; =============================================================================
;; Errors / validation
;; =============================================================================

(defn- preflight-error
  [kind message data]
  (ex-info
   message
   (merge
    {:error/type :gesso.live.operation-acquisition-preflight/error
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
  (let [options' (require-map! "Operation acquisition preflight options" options)
        unknown (set/difference (set (keys options')) option-keys)]
    (when (seq unknown)
      (throw
       (preflight-error
        :unknown-option-keys
        "Operation acquisition preflight options contain unknown keys."
        {:unknown-keys unknown
         :allowed-keys option-keys})))
    (doseq [required [:execution-assembly :acquisition-assembly]]
      (when-not (contains? options' required)
        (throw
         (preflight-error
          :missing-option
          "Operation acquisition preflight is missing a required option."
          {:required required
           :provided-keys (set (keys options'))}))))
    (when (and (contains? options' :name)
               (some? (:name options'))
               (not (keyword? (:name options'))))
      (throw
       (preflight-error
        :invalid-name
        "Operation acquisition preflight :name must be nil or a keyword."
        {:name (:name options')})))
    options'))

;; =============================================================================
;; Derived operation -> acquisition relation
;; =============================================================================

(defn- known-live-change-topics
  [acquisition-assembly]
  (set
   (keys
    (:graph (:live-app acquisition-assembly)))))

(defn- obligations
  [acquisition-assembly]
  (:obligations acquisition-assembly))

(defn- affected-scopes-from-graph
  [acquisition-assembly published-topics]
  (if (set? published-topics)
    (let [graph
          (:graph (:live-app acquisition-assembly))]
      (into
       #{}
       (mapcat
        (fn [change-topic]
          (map :scope (get graph change-topic []))))
       published-topics))
    #{}))

(defn- affected-obligations
  [acquisition-assembly published-topics]
  (if (set? published-topics)
    (into
     (sorted-map)
     (filter
      (fn [[_ obligation]]
        (seq
         (set/intersection
          published-topics
          (:change-topics obligation)))))
     (obligations acquisition-assembly))
    (sorted-map)))

(defn- operation-impact
  [acquisition-assembly published-topics]
  (let [affected-obligations'
        (affected-obligations acquisition-assembly published-topics)]
    {:scopes
     ;; Scope impact comes directly from the compiled Live invalidation graph.
     ;; Do not derive it from acquisition obligations: a graph may legitimately
     ;; target a semantic scope for which this application currently projects no
     ;; managed fragment. Such a scope is still affected even though it creates
     ;; no authoritative-acquisition obligation.
     (affected-scopes-from-graph acquisition-assembly published-topics)

     :fragments
     ;; Fragment impact is the projection subset that has graph-derived managed
     ;; acquisition obligations. A scope with no projected fragment therefore
     ;; contributes no fragment here, exactly as intended.
     (set (keys affected-obligations'))}))

(defn- impact-summary
  [execution-assembly acquisition-assembly]
  (let [publication
        (execution/published-change-topics execution-assembly)]
    (into
     (sorted-map)
     (map
      (fn [[operation published-topics]]
        [operation
         (operation-impact acquisition-assembly published-topics)]))
     publication)))

(defn- unhandled-topic-summary
  [execution-assembly acquisition-assembly]
  (let [known
        (known-live-change-topics acquisition-assembly)]
    (into
     (sorted-map)
     (keep
      (fn [[operation published-topics]]
        (when (set? published-topics)
          (let [unhandled (set/difference published-topics known)]
            (when (seq unhandled)
              [operation unhandled])))))
     (execution/published-change-topics execution-assembly))))

(defn check-operation-acquisition
  "Check route-exposed operation publication closure through one compiled Live
   acquisition assembly.

   Callers provide only the already-closed OptimisticExecutionAssembly and
   AuthoritativeAcquisitionAssembly.  Gesso derives operation impact from the
   trusted publication declarations and compiled Live invalidation topology.

   Fatal structural omission:

     route-exposed operation has nil publication declaration

   Explicit #{} is closed and means no semantic Live publication.  Published
   topics absent from this particular Live graph are warnings because this layer
   cannot infer that every semantic publication must be consumed by this Live
   application."
  [options]
  (let [{:keys [name execution-assembly acquisition-assembly]}
        (validate-options! options)

        execution-valid?
        (execution/execution-assembly? execution-assembly)

        acquisition-valid?
        (acquisition/acquisition-assembly? acquisition-assembly)

        publication
        (if execution-valid?
          (execution/published-change-topics execution-assembly)
          {})

        operations
        (set (keys publication))

        known-topics
        (if acquisition-valid?
          (known-live-change-topics acquisition-assembly)
          #{})

        impacts
        (if (and execution-valid? acquisition-valid?)
          (impact-summary execution-assembly acquisition-assembly)
          (sorted-map))

        unhandled
        (if (and execution-valid? acquisition-valid?)
          (unhandled-topic-summary execution-assembly acquisition-assembly)
          (sorted-map))

        base-errors
        (cond-> []
          (not execution-valid?)
          (conj
           (issue
            :invalid-execution-assembly
            "Operation acquisition preflight requires a current untampered OptimisticExecutionAssembly."
            {:execution-assembly execution-assembly}))

          (not acquisition-valid?)
          (conj
           (issue
            :invalid-acquisition-assembly
            "Operation acquisition preflight requires a current untampered AuthoritativeAcquisitionAssembly."
            {:acquisition-assembly acquisition-assembly})))

        publication-errors
        (if execution-valid?
          (mapv
           (fn [operation]
             (issue
              :undeclared-operation-publication
              "Route-exposed optimistic operation has no declared semantic Live publication relation."
              {:operation operation
               :published-change-topics nil
               :expected "an explicit keyword set; use #{} when the operation publishes no semantic Live changes"}))
           (sort-by pr-str
                    (keep
                     (fn [[operation published-topics]]
                       (when (nil? published-topics)
                         operation))
                     publication)))
          [])

        warnings
        (if (and execution-valid? acquisition-valid?)
          (mapv
           (fn [[operation topics]]
             (issue
              :published-change-topic-not-consumed-by-live-app
              "Trusted operation publishes semantic change topics that this compiled Live application does not consume."
              {:operation operation
               :topics topics
               :known-live-change-topics known-topics}))
           unhandled)
          [])

        errors
        (vec (concat base-errors publication-errors))

        affected-scopes
        (into
         (sorted-map)
         (map (fn [[operation impact]] [operation (:scopes impact)]))
         impacts)

        affected-fragments
        (into
         (sorted-map)
         (map (fn [[operation impact]] [operation (:fragments impact)]))
         impacts)]
    {:gesso.live.operation-acquisition-preflight/type report-type
     :gesso.live.operation-acquisition-preflight/version preflight-version
     :valid? (empty? errors)
     :errors errors
     :warnings warnings
     :analysis
     {:name name
      :execution-assembly execution-assembly
      :acquisition-assembly acquisition-assembly
      :operations operations
      :published-change-topics-by-operation publication
      :known-live-change-topics known-topics
      :affected-scopes-by-operation affected-scopes
      :affected-fragments-by-operation affected-fragments
      :unhandled-published-change-topics-by-operation unhandled}}))

(defn report?
  "True only when value is exactly the current report derivable from its
   embedded closed upstream assemblies."
  [value]
  (and
   (closed-map? report-keys value)
   (= report-type
      (:gesso.live.operation-acquisition-preflight/type value))
   (= preflight-version
      (:gesso.live.operation-acquisition-preflight/version value))
   (boolean? (:valid? value))
   (vector? (:errors value))
   (vector? (:warnings value))
   (closed-map? analysis-keys (:analysis value))
   (set? (get-in value [:analysis :operations]))
   (map? (get-in value [:analysis :published-change-topics-by-operation]))
   (set? (get-in value [:analysis :known-live-change-topics]))
   (map? (get-in value [:analysis :affected-scopes-by-operation]))
   (map? (get-in value [:analysis :affected-fragments-by-operation]))
   (map? (get-in value [:analysis :unhandled-published-change-topics-by-operation]))
   (= (:valid? value)
      (empty? (:errors value)))
   (try
     (let [{:keys [name execution-assembly acquisition-assembly]}
           (:analysis value)]
       (= value
          (check-operation-acquisition
           {:name name
            :execution-assembly execution-assembly
            :acquisition-assembly acquisition-assembly})))
     (catch Exception _
       false))))

(defn valid?
  "True only for a recognized successful operation-acquisition report."
  [report]
  (and
   (report? report)
   (true? (:valid? report))))

;; =============================================================================
;; Closed successful product
;; =============================================================================

(defn operation-acquisition-assembly?
  "True for one closed current operation -> Live acquisition correspondence.

   Recognition rechecks both embedded upstream assemblies and re-runs the join.
   No derived operation-impact registry is stored and therefore none can be
   independently relabeled or become stale."
  [value]
  (and
   (closed-map? assembly-keys value)
   (= operation-acquisition-assembly-type
      (:gesso.live.operation-acquisition-preflight/type value))
   (= preflight-version
      (:gesso.live.operation-acquisition-preflight/version value))
   (or (nil? (:name value))
       (keyword? (:name value)))
   (execution/execution-assembly? (:execution-assembly value))
   (acquisition/acquisition-assembly? (:acquisition-assembly value))
   (try
     (valid?
      (check-operation-acquisition
       {:name (:name value)
        :execution-assembly (:execution-assembly value)
        :acquisition-assembly (:acquisition-assembly value)}))
     (catch Exception _
       false))))

(defn require-operation-acquisition-assembly!
  "Require operation publication -> Live authoritative-acquisition closure and
   return one closed assembly containing only the exact two upstream products."
  [options]
  (let [{:keys [name execution-assembly acquisition-assembly]
         :as options'}
        (validate-options! options)

        report
        (check-operation-acquisition options')]
    (when-not (valid? report)
      (throw
       (preflight-error
        :operation-acquisition-preflight-failed
        "Gesso operation -> authoritative-acquisition preflight failed."
        {:preflight report})))
    (let [assembly
          {:gesso.live.operation-acquisition-preflight/type
           operation-acquisition-assembly-type
           :gesso.live.operation-acquisition-preflight/version
           preflight-version
           :name name
           :execution-assembly execution-assembly
           :acquisition-assembly acquisition-assembly}]
      (when-not (operation-acquisition-assembly? assembly)
        (throw
         (preflight-error
          :invalid-emitted-operation-acquisition-assembly
          "Operation acquisition preflight produced an internally inconsistent assembly."
          {:assembly assembly
           :preflight report})))
      assembly)))

(defn affected-fragments
  "Return the exact route-exposed operation -> affected managed fragment map for
   a current operation-acquisition assembly."
  [assembly]
  (when-not (operation-acquisition-assembly? assembly)
    (throw
     (preflight-error
      :invalid-operation-acquisition-assembly
      "Expected a current OperationAcquisitionAssembly."
      {:assembly assembly})))
  (get-in
   (check-operation-acquisition
    {:name (:name assembly)
     :execution-assembly (:execution-assembly assembly)
     :acquisition-assembly (:acquisition-assembly assembly)})
   [:analysis :affected-fragments-by-operation]))

(defn affected-scopes
  "Return the exact route-exposed operation -> affected Live scope map for a
   current operation-acquisition assembly."
  [assembly]
  (when-not (operation-acquisition-assembly? assembly)
    (throw
     (preflight-error
      :invalid-operation-acquisition-assembly
      "Expected a current OperationAcquisitionAssembly."
      {:assembly assembly})))
  (get-in
   (check-operation-acquisition
    {:name (:name assembly)
     :execution-assembly (:execution-assembly assembly)
     :acquisition-assembly (:acquisition-assembly assembly)})
   [:analysis :affected-scopes-by-operation]))

(defn explain
  "Return a compact stable summary of an operation-acquisition report or closed
   assembly."
  [value]
  (cond
    (operation-acquisition-assembly? value)
    (let [report
          (check-operation-acquisition
           {:name (:name value)
            :execution-assembly (:execution-assembly value)
            :acquisition-assembly (:acquisition-assembly value)})]
      {:type operation-acquisition-assembly-type
       :version preflight-version
       :name (:name value)
       :operations (get-in report [:analysis :operations])
       :published-change-topics
       (get-in report [:analysis :published-change-topics-by-operation])
       :affected-scopes
       (get-in report [:analysis :affected-scopes-by-operation])
       :affected-fragments
       (get-in report [:analysis :affected-fragments-by-operation])
       :unhandled-published-change-topics
       (get-in report [:analysis :unhandled-published-change-topics-by-operation])
       :guarantee
       :operation-publication-to-authoritative-acquisition-preflight-closed})

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
       :operations
       :published-change-topics-by-operation
       :known-live-change-topics
       :affected-scopes-by-operation
       :affected-fragments-by-operation
       :unhandled-published-change-topics-by-operation])}

    :else
    (throw
     (preflight-error
      :unrecognized-value
      "Expected an operation-acquisition preflight report or closed assembly."
      {:value value}))))
