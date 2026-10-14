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
   no modeled transitions are replayed and no state data are disclosed. A
   pure single-step reference comparison can also report whether the final
   captured dependency documents equal the modeled root's after-state, after
   excluding the separate durable command-result record. Such equality is
   about controlled unpinned reads, not a pinned XTDB snapshot, another
   reachable state, or the physical execution history.
   The final result is local diagnostic evidence for ONE observed attempt,
   not a proof certificate,
   authenticated database history, complete application semantics, or general
   refinement. compare-observation-pair! also compares two genuine captured
   dependency snapshots without inferring their chronology or causality.
   For EACH executor-bound observation, a separate local reference step can
   now start from its captured pretransaction dependency map, even when that
   map differs from the declared finite MODEL root. This checks only whether
   the admitted submitted domain plan, evaluated once by the PURE reference
   model, agrees with the controlled captured after-read. It does NOT equate
   that state with a reachable model-tree node or establish execution history.
   No business callbacks or XTDB transactions are run by this module except
   the admitted finite checks at construction time."
  (:require
   [clojure.set :as set]
   [gesso.choreo.identity :as identity]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.operation-family-coverage :as coverage]
   [gesso.live.optimistic.resolution-assurance :as execution]
   [gesso.model.operation-family :as families]
   [gesso.model.resolution-event-observation :as correspondence]
   [gesso.model.tx-model :as tx-model]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]))

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
              :declared-root-vs-observed-pretransaction-state-value-equality
              :single-root-step-vs-observed-posttransaction-dependency-values
              :single-captured-input-step-vs-observed-posttransaction-dependency-values}
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
                      :posttransaction-equality-for-non-root-model-states
                      :local-step-equality-is-not-reachable-model-node-equality
                      :local-step-equality-does-not-prove-dependency-completeness
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

(defn- compare-root-poststate!
  "Compare exactly ONE serial reference step against the final dependency
   documents captured by the same trusted controlled execution. The observed
   durable command-result document is separate from the domain plan and must
   not be confused with a modeled domain write.

   Never replay model callbacks, read XTDB, or expose documents. Equality is
   reported only when the declared model root matches the captured before
   snapshot, its family plan matches the physical submitted domain plan, and
   the reference/physical outcome agrees. This is not a pinned snapshot,
   proof of isolation, or correspondence for non-root model transitions."
  [root-state observation record comparison root-matched?]
  (let [physical (:physical-status comparison)
        result-scope {:scope :one-controlled-serial-root-step-vs-captured-dependencies
                      :relation :exact-clojure-domain-document-map-equality
                      :poststate-data-exposed? false
                      :pinned-xtdb-snapshot :not-established
                      :production-trace-refinement :not-established}]
    (cond
      (not= :matched (:status comparison))
      (assoc result-scope :status :submitted-plan-not-matched)

      (not root-matched?)
      (assoc result-scope :status :declared-root-not-established-equal-to-observation)

      (not (#{:committed :rejected} physical))
      (assoc result-scope :status :physical-outcome-not-reported)

      :else
      (let [submitted (tx-model/admitted-plan
                       (get-in observation [:submitted-incorporation :plan]))
            ;; The exact canonical durable record create is validated by the
            ;; controlled family comparator before this function is called.
            domain (tx-model/admitted-plan
                    (update submitted :commands
                            (fn [commands]
                              (filterv #(not= resolution/command-entity-type
                                               (first (command/target %)))
                                       commands))))
            step (tx-model/step root-state domain)
            actual-state (get-in observation [:observations :state])
            _ (tx-model/require-state actual-state)
            command-target [resolution/command-entity-type (:xt/id record)]]
        (cond
          (not= physical (:status step))
          (assoc result-scope :status :modeled-and-physical-outcomes-disagree)

          (= (:after step) (dissoc actual-state command-target))
          (assoc result-scope :status :matches-captured-posttransaction-dependencies)

          :else
          (assoc result-scope :status :differs-from-captured-posttransaction-dependencies))))))

(defn- compare-captured-local-poststate!
  "Perform the SAME pure serial reference-step check as the root-specific
   comparison, but start from this execution's captured dependency state.

   The previously verified family callback/observed plan correspondence is a
   prerequisite for a positive result; this does not re-run the callback.
   A matched result is about one unpinned observed before/after pair, not
   equality with a non-root finite-model node, complete read coverage,
   authenticated chronology, or a refinement proof. No document is returned."
  [observation record comparison root-matched? root-poststate]
  (let [result (if root-matched?
                 ;; The declared root is exactly the captured before-state.
                 ;; Reuse its already computed pure reference step instead of
                 ;; evaluating the same step twice for a single comparison.
                 root-poststate
                 (let [captured-before (:initial-state observation)
                       _ (tx-model/require-state captured-before)]
                   (compare-root-poststate! captured-before observation record
                                                 comparison true)))]
    (assoc result
           :scope :one-controlled-serial-step-from-captured-pretransaction-dependencies
           :input-source :controlled-observation-pretransaction-dependencies
           :declared-model-root-equality-required? false
           :additional-model-callbacks? false
           :additional-xtdb-reads? false
           :complete-dependency-coverage :not-established
           :equality-with-reachable-non-root-model-state :not-established
           :authenticated-execution-history :not-established)))

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

   An additional root-poststate result compares the existing controlled
   after-read to one reference step of the admitted submitted domain plan,
   excluding the durable command-result record. It performs no extra reads,
   physical submission, or application callback invocation. Neither before-
   nor after-state equality is a global snapshot, isolation or refinement proof.
   The separate captured-local-poststate result evaluates one pure reference
   step from THIS observation's pretransaction dependency documents even when
   the declared finite model root differs. It is not a claimed match with a
   non-root modeled state or the next step of an authenticated history.

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
                                (:principal choice) (:arguments choice))
                    root-matched? (= initial-state
                                     (get-in record [:observation :initial-state]))
                    command-record (get-in record
                                           [:observation :submitted-incorporation :record])
                    poststate (compare-root-poststate!
                               initial-state (:observation record)
                               command-record comparison root-matched?)
                    captured-local-poststate
                    (compare-captured-local-poststate!
                     (:observation record) command-record comparison
                     root-matched? poststate)]
                (select-keys
                 (assoc comparison
                        :declared-root-poststate-comparison poststate
                        :captured-local-poststate-comparison captured-local-poststate
                        ;; The trusted observation supplies the captured
                        ;; dependency-document state *before* the physical
                        ;; transaction. Compare with the exact declared root;
                        ;; do not infer this from an operation/outcome match
                        ;; in the separately explored finite tree.
                        :declared-root-state-comparison
                        {:status (if root-matched?
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
                  :declared-root-poststate-comparison
                  :captured-local-poststate-comparison
                  :registered-coverage :bounded-invariant-model
                  :executor-correspondence :assurance-scope])))))))))

(defn compare-observation-pair!
  "Compare the captured dependency documents at the boundary between TWO
   distinct, genuinely executor-bound diagnostic observations of a CURRENT
   application assembly.

   The caller supplies the order. Each opaque handle must resolve to exactly
   one presently registered executor; detached reports, old executors, and
   repeated handles cannot substitute. This function inspects already captured
   values: it does not run modeled callbacks, execute transactions, or read
   XTDB. The earlier after-read has only its canonical command-result document
   removed; other domain documents and their keys are compared exactly to the
   later before-read. Both captured dependency key sets must be nonempty and
   equal before document-value equality is considered.

   Equal captured values are *not* evidence of observed chronology, absence
   of intervening writes, consistent/pinned snapshots, a complete dependency
   inventory, or model trace refinement. The returned report is redacted and
   cannot authorize an operation or certify an application."
  [assembly earlier-handle later-handle]
  (when (or (nil? earlier-handle)
            (nil? later-handle)
            (identical? earlier-handle later-handle))
    (fail! :invalid-observation-pair
           {:reason :requires-two-distinct-opaque-evidence-handles}))
  ;; Never trust a caller-supplied executor list or operation label. Rebuild
  ;; the registry from the exact assembly and check its physical currentness.
  (when-not (application/application-assembly? assembly)
    (fail! :stale-application-assembly {}))
  (let [options (select-keys assembly assembly-option-keys)
        report (application/check-application-assembly options)
        operations (get-in report [:analysis :operations])
        ids (if (map? operations) (set (keys operations)) #{})
        executors (current-executors! assembly ids)
        inspect!
        (fn [handle]
          (let [matches
                (keep (fn [[operation executor]]
                        (when-let [result
                                   (execution/inspect-evidence-for-executor
                                    handle executor)]
                          [operation result]))
                      executors)]
            (when-not (= 1 (count matches))
              (fail! :unrecognized-executor-bound-evidence
                     {:matching-registered-executors (count matches)}))
            (second (first matches))))
        earlier (inspect! earlier-handle)
        later (inspect! later-handle)
        earlier-observation (:observation earlier)
        later-observation (:observation later)
        command-record (get-in earlier-observation
                               [:submitted-incorporation :record])
        _ (resolution/require-record command-record)
        command-target [resolution/command-entity-type (:xt/id command-record)]
        earlier-after (get-in earlier-observation [:observations :state])
        later-before (:initial-state later-observation)
        _ (tx-model/require-state earlier-after)
        _ (tx-model/require-state later-before)
        earlier-domain (dissoc earlier-after command-target)
        earlier-keys (set (keys earlier-domain))
        later-keys (set (keys later-before))
        scope-matches? (= earlier-keys later-keys)
        status (cond
                 (or (empty? earlier-keys) (empty? later-keys))
                 :no-common-nonempty-captured-domain

                 (not scope-matches?)
                 :captured-dependency-key-sets-differ

                 (= earlier-domain later-before)
                 :captured-domain-documents-match

                 :else
                 :captured-domain-documents-differ)]
    {:status status
     :scope :two-caller-ordered-executor-bound-dependency-observations
     :relation :exact-clojure-captured-domain-document-map-equality
     :dependency-key-set-comparison
     (if scope-matches? :same-captured-keys :different-captured-keys)
     :state-contents-exposed? false
     :comparison-ran-model-callbacks? false
     :additional-xtdb-reads? false
     :observed-execution-chronology :not-established
     :no-intervening-writes :not-established
     :pinned-xtdb-snapshot :not-established
     :complete-domain-dependency-coverage :not-established
     :production-trace-refinement :not-established
     :application-certification :not-established}))
