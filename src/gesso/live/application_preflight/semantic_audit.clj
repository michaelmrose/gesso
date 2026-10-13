(ns gesso.live.application-preflight.semantic-audit
  "Read-only, deliberately scope-limited semantic audit for an application
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
   without making this projection a separately maintained policy registry.

   v794 inspects registered executors' optional durable-resolution metadata,
   which is advisory and unauthenticated. v799 additionally consults the
   v798 local, private adapter-construction registry. Only the exact registered
   executor can carry that bounded construction evidence; copied metadata and
   wrappers do not inherit it. This does not certify model callbacks, guards,
   authorization, durable replay, or runtime business-rule correctness."
  (:require [clojure.set :as set]
            [gesso.live.optimistic.resolved-operation :as live-resolved]))

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

;; The checked construction already distinguishes the three Live-impact
;; fields more strongly than simple set-shape inspection:
;;
;; - publication topics are operation-owned metadata. optimistic.server/operation
;;   preserves omission as nil and never supplies an implicit empty set. The
;;   operation-acquisition gate rejects nil, requiring a set (including #{}).
;; - affected scopes are computed from the compiled Live invalidation graph.
;; - affected fragments are computed from the matching authoritative-acquisition
;;   obligations. An empty derived set is not an explicit negative declaration
;;   by application code.
;;
;; This is a description of the *known construction path* under the caller's
;; current-report precondition. It does not verify the opaque execute! function,
;; nor does it establish whether a set was written literally by a developer.
(def ^:private live-impact-construction-origins
  {:published-change-topics
   {:origin :prepared-optimistic-operation-metadata
    :derivation :carried-through-execution-and-operation-acquisition
    :source-empty-default? false}
   :affected-scopes
   {:origin :compiled-live-invalidation-graph
    :derivation :projected-from-reported-publication-topics}
   :affected-fragments
   {:origin :compiled-authoritative-acquisition-obligations
    :derivation :projected-from-reported-publication-topics}})

(defn- reported-live-impact-provenance
  [operation summary]
  {:status :classified-from-known-construction-path
   :scope :reported-live-impact-origin-only
   :requires :current-canonical-application-preflight-report
   :does-not-establish
   #{:developer-wrote-an-explicit-source-literal
     :actual-model-publication
     :complete-model-effects
     :transactional-independence
     :business-rule-dependency}
   :fields
   (into
    (sorted-map)
    (map
     (fn [field]
       (let [state (reported-set-state summary field)
             available? (reported-set? state)]
         [field
          (merge
           {:reported-state state
            :derived-from [:analysis :operations operation field]
            :status (if available?
                      :construction-origin-classified
                      :not-established)
            :origin (if available?
                      (get-in live-impact-construction-origins [field :origin])
                      :unknown)}
           (when available?
             (select-keys (get live-impact-construction-origins field)
                          [:derivation :source-empty-default?])))]))
     declared-live-impact-fields))})

;; Source-derived construction inspection. Metadata is user-copyable and
;; untrusted. The v798 Live adapter exposes separate local construction
;; provenance for the EXACT function object emitted by build-executor. We may
;; describe that construction, but must never promote the supplied v792
;; callback, opaque policy, transaction guards or settlement correspondence to
;; a proved or certified guarantee. The report passed to this function must
;; already be freshly checked by the application coordinator.
(def ^:private resolution-metadata-keys
  #{:gesso.model/operation-id
    :gesso.model/resolution
    :gesso.model/assurance-scope})

(def ^:private resolution-executor-path
  [:analysis :operation-acquisition-assembly
   :execution-assembly :server :operations])

(defn- scope-shape?
  [scope]
  (and (map? scope)
       (= :gesso.model.resolved-operation/operation-v1
          (:construction scope))
       (set? (:checked scope))
       (set? (:assumed scope))
       (set? (:not-established scope))))

(defn- adapter-registry-match?
  [operation evidence]
  ;; A result comes exclusively from the adapter's private registry; verify
  ;; the advertised version and operation anyway. Evidence for a different
  ;; registered operation must never be silently transferred to this card.
  (and (map? evidence)
       (= :constructed-by-live-resolved-operation-adapter
          (:status evidence))
       (= :private-runtime-construction-registry
          (:evidence-source evidence))
       (= operation (:operation evidence))
       (= (:construction live-resolved/assurance-scope)
          (:construction evidence))
       (= live-resolved/assurance-scope
          (:adapter-assurance-scope evidence))))

(defn- operation-resolution-indication
  [report operation]
  (let [path (conj resolution-executor-path operation :execute!)
        executor (get-in report path)
        metadata (when (instance? clojure.lang.IMeta executor)
                   (meta executor))
        present (if (map? metadata)
                  (set/intersection resolution-metadata-keys
                                    (set (keys metadata)))
                  #{})
        metadata-present? (boolean (seq present))
        claimed-operation (:gesso.model/operation-id metadata)
        claimed-kind (:gesso.model/resolution metadata)
        claimed-scope (:gesso.model/assurance-scope metadata)
        complete? (= present resolution-metadata-keys)
        consistent? (and complete?
                         (= operation claimed-operation)
                         (= :durable-command-v1 claimed-kind)
                         (scope-shape? claimed-scope))
        local-evidence (live-resolved/construction-evidence executor)
        registry-match? (adapter-registry-match? operation local-evidence)
        ;; Both source claims and runtime construction identity must agree.
        ;; Runtime construction provenance says nothing about the policy
        ;; predicates inside the application-supplied resolved-operation.
        recognized? (boolean (and consistent? registry-match?))]
    (cond->
     {:status (cond
                (not (fn? executor)) :no-inspectable-registered-executor
                (and local-evidence (not registry-match?))
                :registered-adapter-operation-mismatch
                (and local-evidence (not consistent?))
                :inconsistent-resolution-metadata
                recognized? :recognized-live-resolved-operation-adapter
                (not metadata-present?) :no-resolution-construction-metadata
                consistent? :consistent-unverified-resolution-metadata
                :else :inconsistent-resolution-metadata)
      :derived-from path
      :evidence-class (if recognized?
                        :private-runtime-construction-registry
                        :untrusted-function-metadata)
      :operation-id-consistent? (when metadata-present?
                                  (= operation claimed-operation))
      :adapter-construction-recognized? recognized?
      :adapter-construction-status
      (cond
        recognized? :exact-executor-identity-recognized
        (and local-evidence (not registry-match?))
        :registered-operation-mismatch
        (some? local-evidence) :inconsistent-registered-metadata
        :else :not-established)
      ;; The local registry attests builder invocation only. It is not an
      ;; authenticated portable certificate or evidence that the application
      ;; executed any domain command correctly.
      :construction-authenticated? false
      :execution-adapter-correspondence :not-established
      :domain-invariants :not-established
      :command-replay-guarantee :not-established
      :scope (if recognized?
               :local-adapter-construction-only
               :metadata-indication-only)}
      consistent?
      (assoc :claimed-construction (:construction claimed-scope)
             :claimed-checks (:checked claimed-scope)
             :claimed-assumptions (:assumed claimed-scope)
             :claimed-unestablished (:not-established claimed-scope))
      recognized?
      (assoc :adapter-construction-evidence
             {:source :private-runtime-construction-registry
              :operation operation
              :construction (:construction local-evidence)
              :adapter-assurance-scope
              (:adapter-assurance-scope local-evidence)
              :model-operation-origin :unverified-function-metadata
              :domain-invariants :not-established
              :application-execution-correspondence :not-established}))))

(defn- operation-card
  [report [operation summary] ordered-operation-summaries]
  {:operation operation
   :derived-from [:analysis :operations operation]
   :contract (select-keys summary operation-card-fields)
   :declared-live-overlaps
   (declared-live-overlaps operation summary ordered-operation-summaries)
   :reported-live-declaration-coverage
   (reported-live-declaration-coverage operation summary ordered-operation-summaries)
   :reported-live-impact-provenance
   (reported-live-impact-provenance operation summary)
   :resolved-operation-construction
   (operation-resolution-indication report operation)
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
   declared these sets or that missing real-world effects have been ruled out.
   Live-impact provenance separately identifies which reported values originate
   in trusted operation metadata and which are derived from the Live graph and
   acquisition obligations. It requires a current report and does not prove
   that any Clojure model actually publishes or accesses the reported values.
   A registered executor may carry v792 durable-resolution metadata. Such
   metadata remains untrusted. An exact executor built through the v798 Live
   adapter additionally has local, private-registry construction evidence,
   which establishes neither the correctness of its supplied callback nor
   authorization, replay, settlement correspondence, or domain policy."
  [report]
  (let [report' (require-report-shape! report)
        analysis (:analysis report')
        operations (ordered-operations (:operations analysis))
        cards (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
                    (map (fn [[operation :as entry]]
                           [operation (operation-card report' entry operations)]))
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
