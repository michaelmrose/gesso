(ns gesso.live.application-preflight.formal-family
  "Bind a selected prepared finite operation-family subject to a checked
   application assembly and subsequently to one EXACT executor-bound attempt.

   This is a deliberately local, non-certifying bridge. At admission it uses
   the existing registered family comparator, which freshly checks operation
   IDs, explores authored state-dependent invariants, and closes over the
   actual registered executor objects. The *same* family declarations also
   construct an admitted formal-family/prepared-competition subject, retaining
   the originally prepared domain plans.

   At observation time, the existing comparator checks current executor
   identity, the opaque controlled Live evidence handle, choice membership,
   submitted-plan equality against the application callbacks, and captured
   pre/post-state equality. This bridge ADDITIONALLY compares the authenticated
   submitted domain plan against the *frozen plan used in the formal subject*.
   Thus a changing or impure preparation callback cannot silently connect a
   different observed plan to an earlier prepared finite model.

   The source family's callbacks, state, principal domain, policy and the
   actual registered executor's domain logic remain separate trust surfaces;
   the bridge cannot prove that arbitrary Clojure functions are identical.
   It does not establish finite-domain completeness, XTDB refinement, a
   complete reachable state graph, an actual TLC verdict, or certification.
   :compare-construction-input! additionally checks the model-input slice
   against the input values captured at the SAME executor's actual construct!
   call site. It refuses unmatched frozen plans and an independently substituted
   paired family. This is conditional field equality, not proof that all
   authorization dependencies were modeled or that the paired functions came
   from a trusted construction registry.

   The unredacted :subject is LOCAL ONLY; the comparator's return value is
   redacted and must not grant authorization or be sent to the browser."
  (:require
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.operation-family-correspondence :as correspondence]
   [gesso.live.optimistic.resolution-assurance :as execution]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.formal-family :as formal-family]
   [gesso.model.resolved-family :as paired]
   [gesso.model.tx-model :as tx-model]))

(def assurance-scope
  {:construction :gesso.live.application-preflight.formal-family/bound-prepared-competition-v1
   :checked #{:current-registered-operation-id-coverage
              :admitted-model-invariant-exploration
              :complete-enabled-selected-prestate-choices
              :canonical-frozen-formal-plan-for-each-choice
              :same-current-registered-executor-on-controlled-observation
              :exact-opaque-executor-evidence-handle
              :observed-choice-matches-finite-declaration
              :observed-domain-plan-matches-frozen-formal-plan
              :selected-root-and-captured-pre-poststate-value-comparison
              :executor-bound-construction-input-provenance
              :modeled-field-equality-against-real-construction-input
              :frozen-shared-family-identity-before-input-comparison}
   :assumed #{:correct-current-application-assembly
              :pure-deterministic-truthful-authored-family-callbacks
              :truthful-complete-selected-prestate-and-finite-choice-domains
              :trusted-live-xtdb-observation-and-executor-registries
              :serialized-atomic-reference-semantics
              :truthful-shared-construction-pair-and-model-input-callback}
   :not-established #{:identity-of-authored-callbacks-and-production-executor-logic
                      :completeness-of-domain-state-principal-or-argument-universe
                      :unbounded-invariant-preservation
                      :state-dependent-repreparation-at-successor-states
                      :authenticated-or-isolated-xtdb-history
                      :universal-server-refinement
                      :authorization-or-business-policy-correctness
                      :browser-server-correspondence
                      :completeness-of-modeled-construction-input-fields
                      :source-identity-of-shared-construction-pair
                      :recife-tlc-verdict
                      :application-certification}})

(def ^:private assembly-option-keys
  [:name :operation-acquisition-assembly
   :browser-artifact-path :browser-receipt-path :rendered-surfaces])

(def ^:private executor-path
  [:analysis :operation-acquisition-assembly
   :execution-assembly :server :operations])

(defn- fail! [kind]
  ;; Do not expose observed command bindings, documents, or callback values.
  (throw (ex-info
          "The prepared formal competition cannot be bound to this registered execution."
          {:error/type ::invalid-bound-formal-family
           :error/kind kind})))

(defn- observed-domain-plan
  "The existing comparator has already authenticated the one canonical
   command-record incorporation and its binding before this is called. Strip
   only the command ledger create; all application writes and guards remain."
  [observation]
  (let [submitted (tx-model/admitted-plan
                   (get-in observation [:submitted-incorporation :plan]))]
    (tx-model/admitted-plan
     (update submitted :commands
             (fn [commands]
               (filterv #(not= resolution/command-entity-type
                                (first (command/target %)))
                        commands))))))

(defn- current-execution! [assembly operation handle]
  ;; Re-derive a physically checked assembly; never accept a detached report
  ;; or a caller-provided executor. The prior comparator also requires the
  ;; captured EXACT objects to remain current, not merely matching keywords.
  (let [report (application/check-application-assembly
                (select-keys assembly assembly-option-keys))
        registered (get-in report executor-path)
        executor (get-in registered [operation :execute!])]
    (when-not (and (application/valid? report) (fn? executor))
      (fail! :registered-executor-unavailable))
    (if-let [result (execution/inspect-evidence-for-executor handle executor)]
      (assoc result :executor executor)
      (fail! :unrecognized-executor-bound-evidence))))

(defn bound-prepared-competition
  "Build a selected-prestate finite TLC subject tied to the current registered
   application operation inventory, returning a LOCAL diagnostic closure.

   (bound-prepared-competition assembly initial-state families rules options)

   :subject is supplied to formal-check/check-prepared! separately. Merely
   building it does NOT run TLC or prove any safety obligation. Every declared
   principal/argument choice must be enabled at the selected root. The bound
   family comparator additionally explores the state-dependent model using
   the supplied finite reachability budget and authored invariant rules.

   :compare-execution! accepts ONLY an opaque execution evidence handle from
   resolution-assurance/run-and-check!. On correspondence it returns a small
   redacted root/plan comparison. A contradictory or changing preparation is
   a :counterexample, not a successful proof. False or stale evidence fails
   with a typed exception through the preexisting exact-executor boundary.

   :compare-construction-input! accepts [paired-family opaque-handle].
   The pair must be the same family map object frozen at admission and must
   expose the shared-construction comparison. The opaque handle must pass
   the frozen plan, selected-root and exact-current-executor checks first.
   Only then is the original, sensitive call-site witness obtained from the
   process-local executor registry and compared against modeled context fields
   and semantic command binding. The result contains NO private values.
   Omitted production context fields are counted, NOT proved irrelevant.

   The caller must keep the returned subject private. Neither a checked
   finite model nor a matched real execution alone certifies an application."
  [assembly initial-state families rules options]
  (let [compare! (correspondence/build-comparator
                  assembly initial-state families rules options)
        {:keys [subject choices choice-count]}
        (formal-family/prepared-competition initial-state families)
        family-by-id (into {} (map (juxt :id identity)) families)
        by-instance (into {}
                          (map-indexed (fn [idx choice]
                                         [(:instance-id choice)
                                          {:choice choice
                                           :plan (nth (:plans subject) idx)}])
                                       choices))]
    (when-not (= (count by-instance) choice-count (count (:plans subject)))
      (fail! :incomplete-prepared-instance-index))
    (letfn [(compare-execution! [handle]
              (let [comparison (compare! handle)
                    instance (:family-instance-id comparison)
                    admitted (get by-instance instance)
                    base {:scope assurance-scope
                          :choice-instance-id instance
                          :operation (:operation comparison)
                          :selected-prestate-only? true
                          :tla-check :not-run
                          :application-certification :not-established}]
                (when-not admitted
                  (fail! :execution-outside-frozen-formal-choices))
                (if-not (= :matched (:status comparison))
                  (assoc base :status :counterexample
                         :reason :declared-family-does-not-match-observed-execution)
                  (let [record (current-execution! assembly (:operation comparison) handle)
                        observed (observed-domain-plan (:observation record))
                        frozen (:plan admitted)
                        root? (= :matches-captured-pretransaction-state
                                 (get-in comparison [:declared-root-state-comparison :status]))
                        after? (= :matches-captured-posttransaction-dependencies
                                  (get-in comparison [:declared-root-poststate-comparison :status]))]
                    (cond
                      (not= observed frozen)
                      (assoc base :status :counterexample
                             :reason :observed-plan-differs-from-frozen-formal-subject)

                      (not root?)
                      (assoc base :status :counterexample
                             :reason :selected-root-differs-from-observed-prestate)

                      (not after?)
                      (assoc base :status :counterexample
                             :reason :selected-root-step-differs-from-observed-poststate)

                      :else
                      (assoc base :status :matched-selected-root-execution
                             :reason :registered-executor-and-frozen-plan-correspondence))))))
            (compare-construction-input! [pair handle]
              (let [comparison (compare-execution! handle)
                    operation (:operation comparison)
                    admitted (get by-instance (:choice-instance-id comparison))
                    choice (:choice admitted)
                    original-family (get family-by-id (:family-id choice))]
                (when-not (and (map? pair)
                               (= :gesso.model.resolved-family/shared-construction-v1
                                  (:kind pair))
                               (= paired/assurance-scope (:scope pair))
                               (= operation (:operation pair))
                               (identical? original-family (:family pair))
                               (fn? (:compare-production-input! pair)))
                  (fail! :unrecognized-frozen-shared-family))
                (if-not (= :matched-selected-root-execution (:status comparison))
                  {:status :counterexample
                   :reason :frozen-execution-correspondence-not-established
                   :operation operation
                   :scope assurance-scope
                   :input-origin :not-inspected
                   :application-certification :not-established}
                  (let [record (current-execution! assembly operation handle)
                        ;; The identity check is made against the current
                        ;; registered executor, never a caller-supplied adapter.
                        executor (:executor record)
                        _ (when-not (fn? executor)
                            (fail! :registered-executor-unavailable))
                        witness (execution/inspect-construction-input-for-executor
                                 handle executor)]
                    (when-not (and (= :executor-bound-construction-input
                                      (:status witness))
                                   (= :trusted-local-call-site-capture
                                      (:input-origin witness))
                                   (map? (:context witness))
                                   (map? (:binding witness)))
                      (fail! :unavailable-executor-bound-construction-input))
                    (let [result (try
                                   ((:compare-production-input! pair)
                                    initial-state (:principal choice)
                                    (:arguments choice)
                                    (:context witness) (:binding witness))
                                   (catch Exception e
                                     ;; Do not leak callback exception details,
                                     ;; private command bindings or context.
                                     (if (= :gesso.model.resolved-family/invalid-resolved-family
                                            (:error/type (ex-data e)))
                                       nil
                                       (fail! :construction-input-comparison-failed))))]
                      (if (= :matched-declared-input-fields (:status result))
                        {:status :matched-executor-bound-declared-input-fields
                         :operation operation
                         :choice-instance-id (:instance-id choice)
                         :scope assurance-scope
                         :input-origin :exact-registered-executor-call-site
                         :context-fields-compared (:context-fields-compared result)
                         :unmodeled-production-context-fields
                         (:unmodeled-production-context-fields result)
                         :command-id-comparison (:command-id-comparison result)
                         :model-input-coverage :not-established
                         :tla-check :not-run
                         :application-certification :not-established}
                        {:status :counterexample
                         :reason :modeled-input-differs-from-executor-bound-construction-input
                         :operation operation
                         :scope assurance-scope
                         :input-origin :exact-registered-executor-call-site
                         :model-input-coverage :not-established
                         :tla-check :not-run
                         :application-certification :not-established}))))))]
      {:kind :gesso.live.application-preflight.formal-family/bound-prepared-competition-v1
       :scope assurance-scope
       :choice-count choice-count
       :subject subject
       :formal-check :not-run
       :application-certification :not-established
       :compare-execution! compare-execution!
       :compare-construction-input! compare-construction-input!})))
