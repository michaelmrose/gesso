(ns gesso.assurance.proof.propositional-rule-schema-audit
  "Independent, exhaustive *instance-level* Boolean audit of the seven
   source-owned local inference obligations (v1333).

   Given an ORIGINAL v1332 rule-instance source, instantiate its local
   obligation and use v1331's independent propositional semantics to test
   every total valuation of its opaque atoms, within a stated budget.

   This checks the local semantic implication, not the correctness of the
   obligation generator, the implementation of the production kernel, or the
   universal schematic rule for unbounded formulas/arity. A separate
   counterfactual entry point tests deliberately perturbed hypotheses or
   conclusions; its results can NEVER be attributed to an admitted rule.

   The audit does not call the proof kernel, production proposition normalizer,
   typed-rule evaluator, certificate checker, or any runtime adapter. It never
   issues certificates, discharges inherited premises, or confers authority."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.propositional-rule-obligations :as obligations]
            [gesso.assurance.proof.propositional-semantics :as semantics]))

(def audit-version 1)
(def report-type
  :gesso.assurance.proof.propositional-rule-schema-audit/instance-audit)
(def catalog-report-type
  :gesso.assurance.proof.propositional-rule-schema-audit/catalog-audit)
(def hypothetical-report-type
  :gesso.assurance.proof.propositional-rule-schema-audit/hypothetical-audit)

(def ^:private maximum-instances-per-rule 16)
(def ^:private expected-rules
  #{:premise :truth-introduction :implication-reflexivity
    :conjunction-introduction :conjunction-elimination
    :implication-elimination :disjunction-introduction})

(defn- reject!
  [reason at details]
  (throw (ex-info "Invalid propositional rule-schema audit source."
                  (merge {:error/type ::invalid-source
                          :reason reason
                          :at at}
                         details))))

(defn- plain-map?
  [x]
  (or (instance? clojure.lang.PersistentHashMap x)
      (instance? clojure.lang.PersistentArrayMap x)))

(defn- plain-vector?
  [x]
  (instance? clojure.lang.APersistentVector x))

(defn- no-meta!
  [x at]
  (when (and (instance? clojure.lang.IObj x) (some? (meta x)))
    (reject! :metadata-not-allowed at {})))

(defn- exact-map!
  [source expected at]
  (when-not (plain-map? source)
    (reject! :expected-plain-map at {}))
  (no-meta! source at)
  (when-not (= expected (set (keys source)))
    (reject! :wrong-keys at
             {:missing (set/difference expected (set (keys source)))
              :unexpected (set/difference (set (keys source)) expected)}))
  source)

(defn- status
  [semantic-result]
  (case semantic-result
    :entails :complete-for-instance
    :vacuously-entails :vacuous-hypotheses
    :counterexample :counterexample
    :not-checked :not-checked
    (reject! :unexpected-semantic-result [:semantic-result]
             {:result semantic-result})))

(defn- evidence
  [checked]
  ;; Retain the oracle's explicit budget explanation or concrete valuation.
  ;; Never turn the absence of a counterexample under a budget failure into
  ;; success. Counterexamples concern the local Boolean implication only.
  (select-keys checked
               [:result :reason :atom-count :formula-nodes
                :valuations-checked :valuation :valuation-index
                :premise-satisfying-valuations
                :maximum-atoms :maximum-work]))

(defn- base-report
  [type]
  {:type type
   :version audit-version
   :scope :individual-source-owned-opaque-boolean-obligations
   :semantic-language semantics/interpretation-type
   :typed-atomic-semantics :not-interpreted
   :source-rule-construction :trusted-v1332-generator-not-independently-proved
   :production-kernel-correspondence :not-established
   :universal-schema-soundness :not-established
   :premise-discharge :not-performed
   :certificate :not-issued
   :admission :not-admitted
   :authority :none})

(defn audit-instance!
  "Check ONE v1332 source rule, not a supplied derivation or cached result.

   Status :complete-for-instance means that the precise local implication
   holds at EVERY opaque Boolean valuation of THIS finite source formula.
   :vacuous-hypotheses means no valuation satisfies all local hypotheses;
   it is NOT considered positive evidence of a non-vacuous inference.
   :counterexample gives a concrete valuation falsifying the implication.
   :not-checked means the independent evaluator reached a budget limit.

   In particular, the result proves neither the general rule schema nor that
   the production kernel implements that schema correctly. :premise remains
   an OPEN assumption; other steps inherit their sources' open premises."
  [source]
  (let [obligation (obligations/instantiate! source)
        checked (semantics/enumerate-entailment!
                 (:hypotheses obligation) (:conclusion obligation))]
    (merge (base-report report-type)
           {:rule (:rule obligation)
            :dependency-policy (:dependency-policy obligation)
            :side-condition (:side-condition obligation)
            :hypothesis-order (:hypothesis-order obligation)
            :hypotheses (:hypotheses obligation)
            :conclusion (:conclusion obligation)
            :open-premises (:open-premises obligation)
            :semantic-status (status (:result checked))
            :checked-semantics (evidence checked)})))

(defn audit-hypothetical!
  "Audit explicit COUNTERFACTUAL local hypotheses and conclusion.

   This diagnostic is useful for mutation/negative-control tests. The caller
   supplies both sides directly, so no result can be attributed to a valid
   source rule, an admitted kernel step, or the v1332 generator. The input
   requires EXACTLY {:hypotheses {qualified-id formula ...}
                      :conclusion formula}.

   This uses the same reference semantics as audit-instance!, but has no
   rule identity, inherited premise interpretation, or admission power."
  [hypothetical]
  (exact-map! hypothetical #{:hypotheses :conclusion} [])
  (let [checked (semantics/enumerate-entailment!
                 (:hypotheses hypothetical) (:conclusion hypothetical))]
    (-> (base-report hypothetical-report-type)
        (assoc :scope :counterfactual-opaque-boolean-entailment-only
               :source-rule-construction :not-claimed
               :rule :none
               :hypotheses (:hypotheses hypothetical)
               :conclusion (:conclusion hypothetical)
               :semantic-status (status (:result checked))
               :checked-semantics (evidence checked)))))

(defn- catalog-source!
  [catalog]
  (exact-map! catalog expected-rules [])
  ;; The keys are a separate, closed inventory (not copied from production
  ;; kernel). This rejects partial and dynamically extended 'proof systems'.
  (doseq [rule (sort expected-rules)]
    (let [instances (get catalog rule)]
      (when-not (and (plain-vector? instances)
                     (<= 1 (count instances) maximum-instances-per-rule))
        (reject! :invalid-rule-instances [rule]
                 {:minimum 1 :maximum maximum-instances-per-rule}))
      (no-meta! instances [rule])
      (doseq [[i source] (map-indexed vector instances)]
        (when-not (plain-map? source)
          (reject! :invalid-instance-source [rule i] {}))
        (no-meta! source [rule i])
        (when-not (= rule (:rule source))
          (reject! :wrong-rule-for-catalog-entry [rule i]
                   {:expected rule :actual (:rule source)})))))
  catalog)

(defn audit-catalog!
  "Audit a source-provided family of test instances for ALL seven rules.

   Every rule must have 1..16 original source instances. The result reports
   exactly which instances passed complete finite Boolean enumeration and
   which were vacuous, inconclusive, or counterexamples. It does NOT quantify
   over every possible formula, every arity, or every implementation behavior.
   The catalog may only establish sample-instance coverage, NOT S2 soundness."
  [catalog]
  (catalog-source! catalog)
  (let [reports
        (into (sorted-map)
              (for [rule (sort expected-rules)]
                [rule (mapv audit-instance! (get catalog rule))]))
        statuses (frequencies (map :semantic-status
                                  (mapcat val reports)))
        total (reduce + 0 (vals statuses))
        result (cond
                 (pos? (get statuses :counterexample 0)) :counterexample
                 (pos? (get statuses :not-checked 0)) :not-checked
                 (pos? (get statuses :vacuous-hypotheses 0)) :vacuous-instances
                 :else :all-supplied-instances-complete)]
    (merge (base-report catalog-report-type)
           {:scope :seven-rule-sample-catalog-only
            :rules (vec (sort expected-rules))
            :instances-checked total
            :status-counts statuses
            :result result
            :instance-reports reports})))
