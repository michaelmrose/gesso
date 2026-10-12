(ns gesso.live.application-preflight.semantic-audit
  "Pure, deliberately scope-limited semantic audit projection for an application
   that has already passed canonical ApplicationAssembly preflight.

   This module reads the operation summaries, trusted assumptions, and open
   obligations in the *freshly checked* application report supplied by its
   caller. It has no authority to recognize an assembly, verify a browser
   artifact, check an XTDB transaction, or certify a policy predicate.

   In particular, the current operation-summary product exposes the trusted
   model operation's name, not its Clojure predicates, source-level rule
   dependencies, or atomic transaction guards. Consequently an empty rule
   catalog would be misleading. This projection reports those portions as
   *not inspected*, and derives operation-local opaque conformance boundaries
   instead. It also identifies overlaps between *declared* Live effects from
   the checked operation summary. Such overlap is not an inferred business-rule,
   shared-state, or transaction dependency. Future source-owned rule declarations
   can enrich the same view
   without making this projection a separately maintained policy registry.")

(def ^:private operation-card-fields
  [:choreography-name
   :browser-role
   :authority-role
   :browser-plan-key
   :browser-plan-digest
   :authority-plan-digest
   :trusted-operation-name
   :command-transport
   :routes
   :execution-capabilities
   :settlement-contract
   :published-change-topics
   :affected-scopes
   :affected-fragments
   :authoritative-acquisitions
   :rendered-affordances])

(def ^:private opaque-model-premise
  :public-model-operation-contracts-hold)

(defn- invalid-report!
  [reason value]
  (throw
   (ex-info
    "Semantic audit requires a successful, freshly checked application-preflight report supplied by the application coordinator."
    {:error/type :gesso.live.application-preflight/error
     :error/kind :invalid-semantic-audit-input
     :reason reason
     :value value})))

(defn- require-report-shape!
  [report]
  ;; A successful shape is a *precondition*, never evidence of authenticity.
  ;; The coordinator must rederive the report from admitted source products.
  (when-not (and (map? report)
                 (= :gesso.live.application-preflight/report
                    (:gesso.live.application-preflight/type report))
                 (= 3 (:gesso.live.application-preflight/version report))
                 (true? (:valid? report))
                 (map? (:analysis report))
                 (map? (get-in report [:analysis :operations]))
                 (set? (get-in report [:analysis :trusted-assumptions]))
                 (sequential? (get-in report [:analysis :open-obligations])))
    (invalid-report! :unsupported-or-unsuccessful-report report))
  (when-not (contains? (get-in report [:analysis :trusted-assumptions])
                       opaque-model-premise)
    (invalid-report! :missing-declared-model-conformance-premise report))
  report)

(defn- ordered-operations
  [operations]
  ;; Operation keys are admitted identifiers, not an ordering guarantee.
  (sort-by (comp pr-str key) operations))

(def ^:private declared-live-impact-fields
  [:published-change-topics :affected-scopes :affected-fragments])

(defn- shared-declared-values
  [left right field]
  ;; nil or an absent declaration cannot establish either presence *or absence*
  ;; of real-world effects.  Only values explicitly present in two admitted
  ;; sets can witness overlap in the declared Live impact of these operations.
  (let [a (get left field)
        b (get right field)]
    (if (and (set? a) (set? b))
      (->> a (filter b) (sort-by pr-str) vec)
      [])))

(defn- declared-live-overlaps
  [operation summary ordered-operation-summaries]
  ;; This is an impact-analysis aid, not a policy or transaction dependency
  ;; graph.  In particular, two operations that refresh the same fragment may
  ;; govern unrelated records, and opaque model code may create other effects.
  {:status :derived-from-checked-declarations
   :scope :shared-declared-live-impact-only
   :does-not-establish
   #{:business-rule-dependency
     :shared-authoritative-state
     :transaction-conflict
     :actual-model-publication
     :absence-of-undeclared-effects}
   :operations
   (into
    []
    (keep
     (fn [[other-operation other-summary]]
       (when-not (= operation other-operation)
         (let [[topics scopes fragments]
               (mapv #(shared-declared-values summary other-summary %)
                     declared-live-impact-fields)]
           (when (or (seq topics) (seq scopes) (seq fragments))
             {:operation other-operation
              :derived-from
              [[:analysis :operations operation]
               [:analysis :operations other-operation]]
              :shared-published-change-topics topics
              :shared-affected-scopes scopes
              :shared-affected-fragments fragments})))))
    ordered-operation-summaries)})

;; Coverage concerns the *reported* Live-impact sets, not the Clojure model's
;; actual effects. An operation summary may synthesize an empty set as a
;; default, so an empty reported set must not be described as an explicit
;; negative declaration by the application author.
(defn- reported-set-state
  [summary field]
  (let [value (get summary field)]
    (cond
      (not (contains? summary field)) :not-reported
      (nil? value) :reported-nil
      (not (set? value)) :unsupported-reported-shape
      (empty? value) :reported-empty-set
      :else :reported-nonempty-set)))

(defn- reported-set?
  [state]
  (contains? #{:reported-empty-set :reported-nonempty-set} state))

(defn- reported-live-field-coverage
  [summary operation field]
  (let [state (reported-set-state summary field)]
    (cond-> {:status state
             :derived-from [:analysis :operations operation field]}
      (reported-set? state)
      (assoc :reported-count (count (get summary field))))))

(defn- reported-live-comparison
  [operation summary other-operation other-summary]
  (let [fields (into (sorted-map)
                     (map (fn [field]
                            (let [left (reported-set-state summary field)
                                  right (reported-set-state other-summary field)]
                              [field {:left left
                                      :right right
                                      :comparable? (and (reported-set? left)
                                                        (reported-set? right))}])))
                     declared-live-impact-fields)
        comparable (into #{} (keep (fn [[field x]]
                                    (when (:comparable? x) field))) fields)
        complete? (= (count comparable) (count declared-live-impact-fields))
        any-shared? (boolean
                     (some (fn [field]
                             (seq (shared-declared-values summary other-summary field)))
                           declared-live-impact-fields))]
    {:operation other-operation
     :derived-from [[:analysis :operations operation]
                    [:analysis :operations other-operation]]
     :fields fields
     :status (cond
               complete? :all-reported-sets-comparable
               (seq comparable) :partially-comparable
               :else :no-comparable-reported-sets)
     :comparable-fields comparable
     ;; Three-valued result: observed overlap disproves no-overlap even when
     ;; other dimensions are missing; otherwise an incomplete comparison
     ;; remains unknown, never an assertion of independence.
     :no-shared-reported-values?
     (cond
       any-shared? false
       complete? true
       :else :unknown)}))

(defn- reported-live-declaration-coverage
  [operation summary ordered-operation-summaries]
  (let [fields (into (sorted-map)
                     (map (fn [field]
                            [field (reported-live-field-coverage
                                    summary operation field)]))
                     declared-live-impact-fields)]
    {:status :derived-from-checked-operation-summary
     :scope :presence-and-shape-of-reported-live-sets-only
     :does-not-establish
     #{:explicit-source-declaration
       :actual-model-effects
       :absence-of-undeclared-effects
       :business-rule-independence
       :transactional-independence}
     :fields fields
     :all-fields-reported-as-sets?
     (every? (comp reported-set? :status val) fields)
     :comparisons
     (into []
           (keep (fn [[other-operation other-summary]]
                   (when-not (= operation other-operation)
                     (reported-live-comparison operation summary
                                               other-operation other-summary))))
           ordered-operation-summaries)}))

(defn- operation-card
  [[operation summary] ordered-operation-summaries]
  {:operation operation
   :derived-from [:analysis :operations operation]
   :contract (select-keys summary operation-card-fields)
   :declared-live-overlaps
   (declared-live-overlaps operation summary ordered-operation-summaries)
   :reported-live-declaration-coverage
   (reported-live-declaration-coverage operation summary ordered-operation-summaries)
   :model-policy-inspection :opaque-host-implementation
   :model-policy-premise opaque-model-premise})

(defn- opaque-model-boundary
  [[operation summary]]
  {:subject [:operation operation :model-implementation-conformance]
   :operation operation
   :trusted-operation-name (:trusted-operation-name summary)
   :premise opaque-model-premise
   :class :trusted
   :why-opaque
   :host-model-predicates-and-transaction-guards-not-in-operation-summary
   :source-of-evidence :trusted-application-model-contract
   :discharged? false})

(defn- assurance-coverage
  [[operation _]]
  [{:subject [:operation operation :runtime-backbone-closure]
    :class :assembly-verified
    :evidence :current-application-preflight-report
    :scope :declared-operation-route-execution-settlement-acquisition-edges}
   {:subject [:operation operation :model-implementation-conformance]
    :class :trusted
    :premise opaque-model-premise
    :scope :opaque-authoritative-model-code}])

(defn audit-from-current-report
  "Derive the currently inspectable semantic audit surface.

   PRECONDITION: the caller has *just* obtained this successful report through
   canonical currentness validation. Passing a forged map does not certify it.
   This function does not check that precondition, beyond rejecting obviously
   unsuccessful or malformed inputs.

   The operation cards and per-operation conformance premises are derived only
   from existing checked report fields. Rules, principal/authorization policy,
   disclosure rules, lifecycle restrictions, and implementation dependency
   graphs are not yet present in this input; they remain explicitly unknown,
   never counted as zero or mechanically verified. The operation cards can
   identify shared declared publication topics, affected scopes and fragments;
   those relationships do not imply shared authoritative state, actual model
   emission, a domain rule dependency, or the absence of other effects.
   Reported-set coverage distinguishes a typed empty set from missing, nil, or
   malformed report values; it does not assert that the original app explicitly
   declared these sets or that missing real-world effects have been ruled out."
  [report]
  (let [report' (require-report-shape! report)
        analysis (:analysis report')
        operations (ordered-operations (:operations analysis))
        cards (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
                    (map (fn [[operation :as entry]]
                           [operation (operation-card entry operations)]))
                    operations)
        boundaries (mapv opaque-model-boundary operations)
        coverage (vec (mapcat assurance-coverage operations))]
    {:inspection-scope
     {:source :freshly-checked-application-preflight-report
      :inspected
      #{:declared-operation-identities
        :operation-runtime-backbone-closure
        :trusted-operation-names
        :declared-routes
        :settlement-and-authoritative-acquisition-relations}
      :not-inspected
      #{:arbitrary-clojure-model-bodies
        :authorization-and-disclosure-predicates
        :business-rule-meaning-and-exceptions
        :transaction-guard-implementation
        :cross-operation-domain-invariants
        :complete-application-render-enumeration}
      :complete-business-policy-inventory? false
      :zero-undetected-policy-escapes-established? false}
     :operation-count (count operations)
     :operation-cards cards
     :business-rule-catalog
     {:status :not-derivable-from-current-operation-summary
      :count :unknown
      :rules []}
     :authorization-disclosure-view
     {:status :not-inspected
      :reason :opaque-application-model-policy}
     :lifecycle-transition-view
     {:status :not-inspected
      :reason :no-admitted-domain-transition-relation-in-this-report}
     :rule-dependencies
     {:status :not-inspected
      :reason :no-admitted-rule-dependency-graph-in-this-report}
     :opaque-model-boundaries boundaries
     :application-trusted-assumptions
     (vec (sort-by pr-str (:trusted-assumptions analysis)))
     :open-obligations (vec (:open-obligations analysis))
     :assurance-coverage coverage}))
