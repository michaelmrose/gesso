(ns gesso.assurance.proof.propositional-derivation-soundness
  "Compositional, source-relative derivation soundness (v1339).

   Mathematical argument, by induction on the finite source step order:
   Let G_i denote the exact ORIGINAL named premises inherited by step i,
   and C_i its ORIGINAL conclusion. Base cases are either a selected open
   premise or a premise-free valid inference. For a rule citing earlier
   steps j, their induction hypotheses state G_j |= C_j. Therefore every
   valuation satisfying the union of G_j makes each cited C_j true. If the
   local rule has been exhaustively established to preserve Boolean truth,
   the same valuation makes C_i true. Thus G_i |= C_i. The final step
   gives G_final |= goal.

   This argument is *compositional*: each individual rule instance must be
   checked across every valuation of its LOCAL opaque atoms, but the entire
   derivation need not fit into one global valuation enumeration budget.
   A separate global enumeration is diagnostic, never an admission shortcut.

   This module RERUNS v1335 on original source (which itself reruns the
   production kernel and independent local audits); it accepts no cached
   reports, proofs, certificates, or caller-supplied dependency claims.
   Exact inherited-premise propagation is independently reconstructed from
   source step references. Each local implication is also independently
   re-evaluated from original source conclusions before composition. Local vacuity and incomplete checks remain open.

   Critical boundary: the mathematical induction is conditional on the
   supplied source really matching the seven rule schemata and on sound
   reference evaluation. It does NOT universally establish correctness of
   Clojure implementation, typed-atom meaning, normalization, physical
   execution, or certificate admission. In particular, a source-relative
   inductive argument must never be presented as a deployed-app theorem."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.propositional-kernel-correspondence :as correspondence]
            [gesso.assurance.proof.propositional-semantics :as semantics]))

(def derivation-soundness-version 1)
(def report-type
  :gesso.assurance.proof.propositional-derivation-soundness/conditional-induction)

(defn- fail!
  [reason at details]
  (throw (ex-info "Invalid Gesso propositional derivation induction source."
                  (merge {:error/type ::invalid-derivation-induction
                          :reason reason
                          :at at}
                         details))))

(defn- inherited-premises!
  "Independently derive the premise ID set from ORIGINAL step references.
   In particular, a premise step introduces an OPEN assumption; no rule
   silently discharges one. Earlier references are mandatory for a derived
   step, and proof order is not inferred from a copyable audit report."
  [step prior at]
  (case (:rule step)
    :premise #{(:premise step)}
    (:truth-introduction :implication-reflexivity) #{}
    (let [refs (:from step)]
      (when-not (vector? refs)
        (fail! :missing-step-references (conj at :from) {}))
      (reduce (fn [known id]
                (let [entry (get prior id)]
                  (when-not entry
                    (fail! :unknown-or-forward-source (conj at :from)
                           {:reference id}))
                  (set/union known (:premises entry))))
              #{} refs))))

(defn- independent-local-status!
  "Re-evaluate the RAW local implication under v1331, without asking the
   v1332 obligation generator or v1333 auditor to supply its conclusion.
   This is a second check on the v1335 report, not a new inference rule."
  [step prior premises at]
  (let [hypotheses
        (case (:rule step)
          :premise {(:premise step) (get premises (:premise step))}
          (:truth-introduction :implication-reflexivity) {}
          (into {} (map (fn [id]
                          (let [parent (get prior id)]
                            (when-not parent
                              (fail! :unknown-or-forward-source
                                     (conj at :from) {:reference id}))
                            [id (:conclusion parent)]))
                        (:from step))))
        decision (semantics/enumerate-entailment!
                  hypotheses (:conclusion step))]
    {:independent-local-evaluation (:result decision)
     :independent-local-status
     (case (:result decision)
       :entails :complete-for-instance
       :vacuously-entails :vacuous-hypotheses
       :not-checked :not-checked
       :counterexample :counterexample
       (fail! :unexpected-reference-result at
              {:result (:result decision)}))}))

(defn- inductive-ledger!
  "Relate every independently audited local obligation to the theorem's
   stepwise premise-inheritance invariant. Reject malformed / substituted
   correspondence records; no caller-provided record can be passed here."
  [source audit]
  (let [original (:steps source)
        reports (:step-reports audit)]
    (when-not (and (vector? original) (vector? reports)
                   (= (count original) (count reports))
                   (= (count original) (:checked-step-count audit)))
      (fail! :source-report-cardinality-mismatch [:steps] {}))
    (reduce (fn [{:keys [prior records]} [i step report]]
              (let [at [:steps i]
                    id (:id step)
                    inherited (inherited-premises! step prior at)
                    independent (independent-local-status!
                                 step prior (:premises source) at)
                    expected {:id id :rule (:rule step)
                              :conclusion (:conclusion step)
                              :premises inherited :source-index i}]
                (when (contains? prior id)
                  (fail! :duplicate-step-id (conj at :id) {:step-id id}))
                (when-not (= expected (select-keys report (keys expected)))
                  (fail! :source-report-step-mismatch at
                         {:step-id id}))
                (when-not (= (:independent-local-status independent)
                             (:local-semantic-status report))
                  (fail! :independent-local-audit-mismatch at
                         {:independent (:independent-local-status independent)
                          :reported (:local-semantic-status report)}))
                (when-not (#{:complete-for-instance
                             :vacuous-hypotheses
                             :not-checked
                             :counterexample}
                           (:local-semantic-status report))
                  (fail! :invalid-local-audit-status at
                         {:status (:local-semantic-status report)}))
                (let [record (assoc expected
                                    :independent-local-evaluation
                                    (:independent-local-evaluation independent)
                                    :local-semantic-status
                                    (:local-semantic-status report)
                                    :inductive-consequence
                                    (if (= :complete-for-instance
                                           (:local-semantic-status report))
                                      :conditional-on-the-local-rule-and-parent-steps
                                      :not-established))]
                  {:prior (assoc prior id record)
                   :records (conj records record)})))
            {:prior {} :records []}
            (map vector (range) original reports))))

(defn- checked-global
  "A separate direct global semantic check, for small enough formulas.
   Importantly, over-budget global checking does NOT turn a valid local
   induction into a false theorem, or turn an open local rule into success."
  [source]
  (try
    (let [result (semantics/enumerate-entailment!
                  (:premises source) (:goal source))]
      (select-keys result [:result :reason :atom-count :formula-nodes
                           :valuations-checked :valuation :valuation-index
                           :premise-satisfying-valuations
                           :maximum-atoms :maximum-work]))
    (catch clojure.lang.ExceptionInfo e
      ;; A finite derivation can contain more connective nodes in total than
      ;; the *single-query* global reference interpreter permits. The local
      ;; step audit remains the compositional evidence; only the optional
      ;; whole-formula truth-table cross-check is lost.
      (let [reason (:reason (ex-data e))]
        (if (#{:formula-node-limit :opaque-node-limit :formula-depth-limit}
             reason)
          {:result :not-checked :reason reason :valuations-checked 0}
          (throw e))))))

(defn- base-report [audit]
  {:type report-type
   :version derivation-soundness-version
   :scope :source-relative-compositional-opaque-boolean-induction
   :source-fingerprint (:kernel-source-fingerprint audit)
   :source-correspondence :v1335-rerun-from-original-source
   :local-semantics :v1331-v1333-complete-opaque-boolean-valuations
   :inference-obligations :v1332-seven-closed-rule-schemata
   :rule-schema-universal-soundness :not-established
   :kernel-implementation-universal-soundness :not-established
   :normalization-denotational-preservation :not-established
   :typed-atomic-semantics :not-interpreted
   :runtime-refinement :not-established
   :assumption-discharge :not-performed
   :certificate :not-issued
   :admission :not-admitted
   :authority :none})

(defn check-induction!
  "Check the conditional, source-specific STEP INDUCTION ARGUMENT.

   When all local rule instances were exhaustively and non-vacuously checked,
   and exact source-level dependencies agree with the production kernel,
   returns :conditional-source-derivation-soundness. This judgment means:
   for EVERY opaque Boolean valuation of ALL atoms of the ORIGINAL source,
   if its returned named OPEN premises are true, its goal is true, CONDITIONAL
   on the checked local-instance semantics and the supplied correspondence.

   It is not a universal theorem about Gesso's source checker or any typed
   atomic predicates. Local vacuity, budget exhaustion, and counterexamples
   explicitly block the compositional success status. No authority is issued.
   Calling this function on a prior report is invalid: ORIGINAL source is
   always rechecked through the complete v1335 audit path."
  [source]
  (let [audit (correspondence/audit-derivation! source)
        base (base-report audit)]
    (if (and (= :not-checked (:status audit))
             (= :step-budget (:reason audit)))
      (assoc base
             :status :not-checked
             :reason (:reason audit)
             :source-step-count (:source-step-count audit)
             :checked-step-count 0
             :step-induction []
             :local-status-counts {}
             :open-premises (:kernel-open-premises audit)
             :whole-derivation-claim :not-established
             :whole-derivation-check
             {:result :not-checked :reason :step-budget
              :valuations-checked 0})
      (let [{:keys [records]} (inductive-ledger! source audit)
            last-record (peek records)
            open (:premises last-record)
            statuses (frequencies (map :local-semantic-status records))
            _ (when-not (and (= (:conclusion last-record) (:goal source))
                             (= open (:kernel-open-premises audit))
                             (= :all-step-shapes-match
                                (:source-rule-correspondence audit))
                             (= :exact-match
                                (:open-premise-correspondence audit)))
                (fail! :final-correspondence-mismatch [:goal] {}))
            global (checked-global source)
            all-local-complete? (= (count records)
                                   (get statuses :complete-for-instance 0))
            _ (when (and all-local-complete?
                         (= :counterexample (:result global)))
                (fail! :induction-global-semantics-disagree [:goal]
                       {:counterexample (:valuation global)}))
            status (cond
                     (= :counterexample (:result global)) :counterexample
                     (pos? (get statuses :counterexample 0)) :counterexample
                     (pos? (get statuses :not-checked 0)) :not-checked
                     (pos? (get statuses :vacuous-hypotheses 0)) :vacuous-local-hypotheses
                     (= :vacuously-entails (:result global)) :vacuous-global-premises
                     all-local-complete? :conditional-source-derivation-soundness
                     :else :not-checked)]
        (assoc base
               :status status
               :source-step-count (count records)
               :checked-step-count (count records)
               :step-induction records
               :open-premises open
               :open-premise-ids (vec (sort open))
               :local-status-counts statuses
               :induction-step-rule :source-hypotheses-plus-valid-local-inference
               :whole-derivation-claim (if (= :conditional-source-derivation-soundness status)
                                         :all-opaque-valuations-conditional-on-open-premises
                                         :not-established)
               :whole-derivation-check global)))))
