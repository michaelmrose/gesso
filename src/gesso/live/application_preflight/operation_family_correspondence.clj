(ns gesso.live.application-preflight.operation-family-correspondence
  "Bound a finite operation-family comparison to a CURRENT registered Gesso
   application and to an ACTUAL executor-bound diagnostic execution.

   build-comparator first checks the complete registered family-ID inventory
   through operation-family-coverage/check-registered-families, including its
   bounded state-dependent invariant exploration. Only a passing finite model
   can be admitted. The returned comparison function closes over the EXACT
   admitted family callbacks, finite choices, and registered executor objects;
   callers cannot supply replacement callbacks at observation time.

   A comparison accepts only an opaque handle issued by the controlled Live
   resolution-assurance runner. It rechecks the application's physical assembly,
   requires unchanged executor identities, locates the handle's exact executed
   adapter, verifies that its principal and arguments belong to the admitted
   finite family domain, then uses the v826 submitted-plan comparator.

   The family construction and its initial abstract state are STILL authored by
   the developer. Closure of family declarations does not prove their truth,
   purity, policy completeness or representativeness. The bounded invariant
   run need not have started from the observed XTDB state. Each comparison now
   separately reports whether the declared finite ROOT state exactly equals
   the captured pre-transaction document map of this one controlled attempt;
   no modeled transitions are replayed and no state data are disclosed. Such
   equality is about the captured dependency documents, not a pinned XTDB
   snapshot, another reachable state, or the physical execution history.
   The final result is local diagnostic evidence for ONE observed attempt,
   not a proof certificate,
   authenticated database history, complete application semantics, or general
   refinement. No business callbacks or XTDB transactions are run by this
   module except the admitted finite checks at construction time."
  (:require
   [clojure.set :as set]
   [gesso.choreo.identity :as identity]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.operation-family-coverage :as coverage]
   [gesso.live.optimistic.resolution-assurance :as execution]
   [gesso.model.operation-family :as families]
   [gesso.model.resolution-event-observation :as correspondence]))

(def assurance-scope
  {:checker :gesso.live.application-preflight.operation-family-correspondence/registered-attempt-v1
   :checked #{:current-source-derived-operation-registry
              :complete-declared-operation-id-coverage
              :bounded-declared-state-dependent-invariant-check
              :closed-admitted-family-callback-selection
              :current-registered-executor-object-identity
              :exact-executor-bound-physical-evidence-handle
              :exact-declared-principal-and-argument-choice
              :modeled-enabledness-and-submitted-transaction-plan-match
              :declared-root-vs-observed-pretransaction-state-value-equality}
   :assumed #{:truthful-pure-total-family-and-invariant-callbacks
              :correct-current-application-assembly
              :representative-finite-initial-state-and-input-domains
              :reliable-process-local-runtime-construction-and-evidence-registries
              :controlled-execution-observation-and-unpinned-xtdb-reads}
   :not-established #{:application-origin-of-authored-family-callbacks
                      :domain-or-principal-completeness
                      :correctness-or-completeness-of-business-policy-guards
                      :invariants-of-states-outside-the-finite-model
                      :equality-of-modeled-and-observed-initial-states
                      :authenticated-xtdb-history-or-global-isolation
                      :correctness-of-projection-callbacks
                      :unbounded-execution-refinement
                      :whole-application-certification}})

(def ^:private executor-path
  [:analysis :operation-acquisition-assembly
   :execution-assembly :server :operations])

(def ^:private assembly-option-keys
  [:name :operation-acquisition-assembly
   :browser-artifact-path :browser-receipt-path :rendered-surfaces])

(defn- fail! [kind details]
  (throw (ex-info
          "Registered family correspondence requires a current checked assembly, admitted choices, and genuine physical evidence."
          {:error/type ::invalid-family-correspondence
           :error/kind kind
           :error/details details})))

(defn- current-executors! [assembly expected]
  ;; The report is freshly derived from the supplied assembly's OWN backbone;
  ;; neither an application-provided list of executor IDs nor a detached
  ;; semantic-audit report is a substitute for this check.
  (when-not (application/application-assembly? assembly)
    (fail! :stale-application-assembly {}))
  (let [options (select-keys assembly assembly-option-keys)
        report (application/check-application-assembly options)
        registry (get-in report executor-path)
        summary (set (keys (get-in report [:analysis :operations])))]
    (when-not (and (application/valid? report)
                   (map? registry)
                   (= expected summary)
                   (= expected (set (keys registry)))
                   (every? (fn [[_ entry]] (fn? (:execute! entry))) registry))
      (fail! :registered-executor-registry-changed
             {:expected-operation-ids expected
              :reported-operation-ids summary
              :executor-operation-ids (when (map? registry)
                                        (set (keys registry)))}))
    (into {} (map (fn [[operation entry]] [operation (:execute! entry)])) registry)))

(defn- principal-choice? [declared observed]
  (or (= declared observed)
      (and (identity/principal? observed)
           (= declared (identity/raw-value observed)))))

(defn build-comparator
  "Admit complete finite registered families, then return a ONE-ARGUMENT
   diagnostic comparator `(fn [opaque-execution-handle] result)`.

   Construction performs the v824 registered-ID check and bounded invariant
   exploration. A counterexample PREVENTS construction. The returned function
   freezes the chosen family maps and exact registered executors in its closure.

   At each use the current assembly is checked again. The handle MUST belong
   to one of those unchanged executors in v815's private execution registry.
   Its bound command's principal and argument map must match exactly one
   admitted finite choice. Comparison delegates to v826; mismatching plans
   return an explicit counterexample, not an assurance success.

   In addition to the existing observed-state plan check, the result describes
   exact Clojure value equality between the declared finite ROOT state and
   the controlled observer's pre-transaction dependency-document map. This
   comparison is redacted and does not replay models or equate arbitrary
   reachable tree nodes with XTDB. A matched root is NOT evidence of pinned
   snapshots, authenticated history, or subsequent trace refinement.

   This is intentionally not an application startup certificate. Calling this
   again with different authored families creates a DIFFERENT diagnostic
   comparator; the framework does not claim to derive callback logic from
   arbitrary production Clojure."
  [assembly initial-state declared-families rules options]
  (let [verdict (coverage/check-registered-families
                 assembly initial-state declared-families rules options)]
    (when-not (= :holds-in-explored-model (:status verdict))
      (fail! :modeled-invariant-violation
             {:status (:status verdict)
              :violated-rule-ids (mapv :rule-id (:violations verdict))}))
    (let [expanded (families/expand-families declared-families)
          original-ids (set (get-in verdict [:registry-coverage
                                             :registered-operation-ids]))
          originals (current-executors! assembly original-ids)
          selected-families (into {} (map (juxt :id identity)) declared-families)
          selections (:choices expanded)]
      (fn [handle]
        (let [current (current-executors! assembly original-ids)]
          (when-not (every? (fn [[operation executor]]
                              (identical? executor (get current operation)))
                            originals)
            (fail! :registered-executor-replaced {}))
          (let [matches (keep (fn [[operation executor]]
                                (when-let [record
                                           (execution/inspect-evidence-for-executor
                                            handle executor)]
                                  [operation record]))
                              current)]
            (when-not (= 1 (count matches))
              (fail! :unrecognized-executor-bound-evidence
                     {:matching-registered-executors (count matches)}))
            (let [[operation record] (first matches)
                  binding (:binding record)
                  choices (filterv
                           (fn [choice]
                             (and (= operation (:family-id choice))
                                  (= (:arguments binding) (:arguments choice))
                                  (principal-choice? (:principal choice)
                                                     (:principal binding))))
                           selections)]
              (when-not (= 1 (count choices))
                (fail! :execution-outside-admitted-family-domain
                       {:operation operation
                        :matched-choices (count choices)}))
              (let [choice (first choices)
                    family (get selected-families operation)
                    comparison (correspondence/compare-family-preparation
                                (:observation record)
                                (select-keys family [:enabled? :prepare])
                                (:principal choice) (:arguments choice))]
                (select-keys
                 (assoc comparison
                        ;; The trusted observation supplies the captured
                        ;; dependency-document state *before* the physical
                        ;; transaction. Compare with the exact declared root;
                        ;; do not infer this from an operation/outcome match
                        ;; in the separately explored finite tree.
                        :declared-root-state-comparison
                        {:status (if (= initial-state
                                        (get-in record [:observation :initial-state]))
                                   :matches-captured-pretransaction-state
                                   :differs-from-captured-pretransaction-state)
                         :relation :exact-clojure-state-map-equality
                         :scope :declared-model-root-vs-controlled-dependency-snapshot
                         :non-root-state-correspondence :not-established
                         :production-trace-refinement :not-established}
                        :operation operation
                        :family-instance-id (:instance-id choice)
                        :registered-coverage :exact
                        :bounded-invariant-model :holds-in-explored-model
                        :executor-correspondence :same-current-registered-executor
                        :assurance-scope assurance-scope)
                 [:status :reason :operation :family-instance-id
                  :physical-status :matching-guards? :matching-commands?
                  :declared-root-state-comparison
                  :registered-coverage :bounded-invariant-model
                  :executor-correspondence :assurance-scope])))))))))
