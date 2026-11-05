(ns gesso.assurance.proof.propositional-kernel-correspondence
  "Source-relative correspondence audit for accepted kernel derivations (v1335).

   Re-run the production kernel on ORIGINAL source data, then INDEPENDENTLY
   reconstruct each rule's inputs, exact local conclusion and inherited open
   premises from the unnormalized source. Compare them with the v1332 rule
   obligations and audit their opaque-Boolean truth under v1331 semantics.

   This detects discrepancies in admitted source-step construction. It is NOT
   a proof that normalization preserves meaning, that the production kernel
   always follows these rules, or that the seven rule schemas are universally
   sound. Every exhaustive valuation claim concerns only one finite source
   instance. Reports are copyable diagnostics, never certificates or authority.

   Deliberately do not call private kernel rule helpers, introspect a cached
   checker report, or reuse the production normalizer to construct obligations."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.kernel :as kernel]
            [gesso.assurance.proof.propositional-rule-obligations :as obligations]
            [gesso.assurance.proof.propositional-rule-schema-audit :as audit]))

(def correspondence-version 1)
(def report-type
  :gesso.assurance.proof.propositional-kernel-correspondence/source-audit)
(def ^:private maximum-step-audits 48)

;; An independently spelled, CLOSED inventory of allowed source-step keys.
;; The production kernel's private rule table is intentionally not consulted.
(def ^:private step-keys
  {:premise #{:id :rule :premise :conclusion}
   :truth-introduction #{:id :rule :conclusion}
   :implication-reflexivity #{:id :rule :conclusion}
   :conjunction-introduction #{:id :rule :from :conclusion}
   :conjunction-elimination #{:id :rule :from :index :conclusion}
   :implication-elimination #{:id :rule :from :conclusion}
   :disjunction-introduction #{:id :rule :from :index :conclusion}})

(defn- reject!
  [reason at details]
  (throw (ex-info "Invalid Gesso propositional kernel correspondence source."
                  (merge {:error/type ::invalid-correspondence
                          :reason reason :at at}
                         details))))

(defn- plain-map?
  [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

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

(defn- qualified-keyword?
  [x]
  (and (keyword? x) (some? (namespace x))))

(defn- source-steps!
  [step previous expected-count at]
  (let [ids (:from step)]
    (when-not (and (plain-vector? ids)
                   (if (= expected-count :two-or-more)
                     (<= 2 (count ids) 127)
                     (= expected-count (count ids))))
      (reject! :invalid-step-sources (conj at :from)
               {:expected expected-count}))
    (no-meta! ids (conj at :from))
    (mapv (fn [id]
            (when-not (qualified-keyword? id)
              (reject! :invalid-source-id (conj at :from) {:id id}))
            (or (get previous id)
                (reject! :unknown-or-forward-source (conj at :from)
                         {:id id})))
          ids)))

(defn- rule-instance!
  [step previous premises at]
  (let [rule (:rule step)
        conclusion (:conclusion step)]
    (exact-map! step (or (get step-keys rule)
                         (reject! :unknown-rule (conj at :rule) {:rule rule}))
                at)
    (when-not (qualified-keyword? (:id step))
      (reject! :invalid-step-id (conj at :id) {}))
    (case rule
      :premise
      (let [id (:premise step)]
        (when-not (and (qualified-keyword? id) (contains? premises id))
          (reject! :invalid-premise-reference (conj at :premise)
                   {:premise id}))
        {:input {:rule rule :premise-id id :formula (get premises id)}
         :sources [] :inherited #{id}})

      :truth-introduction
      {:input {:rule rule} :sources [] :inherited #{}}

      :implication-reflexivity
      (let [formula (when (and (plain-vector? conclusion)
                               (= :implies (first conclusion))
                               (= 3 (count conclusion)))
                      (second conclusion))]
        (when-not formula
          (reject! :expected-implication (conj at :conclusion) {}))
        {:input {:rule rule :formula formula} :sources [] :inherited #{}})

      :conjunction-introduction
      (let [sources (source-steps! step previous :two-or-more at)]
        {:input {:rule rule :sources (mapv :conclusion sources)}
         :sources sources
         :inherited (reduce set/union #{} (map :premises sources))})

      :conjunction-elimination
      (let [sources (source-steps! step previous 1 at)]
        {:input {:rule rule :conjunction (:conclusion (first sources))
                 :index (:index step)}
         :sources sources :inherited (:premises (first sources))})

      :implication-elimination
      (let [sources (source-steps! step previous 2 at)]
        {:input {:rule rule :implication (:conclusion (first sources))
                 :antecedent (:conclusion (second sources))}
         :sources sources
         :inherited (reduce set/union #{} (map :premises sources))})

      :disjunction-introduction
      (let [sources (source-steps! step previous 1 at)]
        {:input {:rule rule :source (:conclusion (first sources))
                 :disjunction conclusion :index (:index step)}
         :sources sources :inherited (:premises (first sources))}))))

(defn- audit-step!
  [step previous premises index]
  (let [at [:steps index]
        id (:id step)]
    (when (contains? previous id)
      (reject! :duplicate-step-id (conj at :id) {:id id}))
    (let [{:keys [input inherited]} (rule-instance! step previous premises at)
          obligation (obligations/instantiate! input)
          actual (:conclusion step)]
      ;; This comparison is independent of the kernel's normalized formulas.
      ;; The v1332 obligation generator derives, not accepts, its conclusion.
      (when-not (= (:conclusion obligation) actual)
        (reject! :source-rule-conclusion-mismatch (conj at :conclusion)
                 {:rule (:rule step)}))
      (when (and (= :premise (:rule step))
                 (not= (:open-premises obligation) inherited))
        (reject! :premise-dependency-mismatch at {}))
      (let [result (audit/audit-instance! input)]
        (when-not (and (= (:rule step) (:rule result))
                       (= actual (:conclusion result)))
          (reject! :oracle-source-mismatch at {}))
        {:id id :rule (:rule step) :conclusion actual
         :premises inherited
         :local-semantic-status (:semantic-status result)
         :local-semantic-evidence (:checked-semantics result)
         :dependency-policy (:dependency-policy result)
         :source-index index}))))

(defn- report-base
  [checked]
  {:type report-type
   :version correspondence-version
   :scope :source-relative-propositional-kernel-correspondence
   :kernel-source-fingerprint (:fingerprint checked)
   :kernel-version kernel/kernel-version
   :semantic-rule-source :v1332-closed-obligation-generator
   :semantic-interpreter :v1331-opaque-boolean-reference
   :source-normalization-soundness :not-established
   :universal-implementation-correspondence :not-established
   :universal-rule-soundness :not-established
   :typed-atomic-semantics :not-interpreted
   :physical-runtime-correspondence :not-established
   :assumption-discharge :not-performed
   :certificate :not-issued
   :admission :not-admitted
   :authority :none})

(defn audit-derivation!
  "Recheck one ORIGINAL derivation against the kernel and v1332/v1333 rules.

   For <=48 steps, independently rebuild every LOCAL source-rule obligation,
   compare each derived conclusion with the admitted source step, propagate
   original premise dependencies, and enumerate local Boolean valuations.
   No supplied reports, normalized proof steps, source fingerprints, or
   certificates are accepted as input.

   An accepted kernel proof with mismatched source-rule construction throws.
   A bounded, vacuous, or unchecked local audit stays explicitly non-complete.
   Even full completion here proves neither universal schema soundness nor
   normalization/runtime correctness."
  [source]
  ;; Run production admission ONCE on the unchanged original source, never on
  ;; a source reconstructed from its own descriptive report.
  (let [checked (kernel/check-derivation! source)
        _ (exact-map! source #{:subject :variables :premises :steps :goal} [])
        {:keys [steps premises goal]} source
        count-steps (count steps)
        base (assoc (report-base checked)
                    :source-step-count count-steps
                    :kernel-open-premises (:open-premises checked))]
    (if (> count-steps maximum-step-audits)
      (assoc base
             :status :not-checked
             :reason :step-budget
             :maximum-step-audits maximum-step-audits
             :checked-step-count 0
             :step-reports []
             :source-rule-correspondence :not-checked)
      (let [ledger (reduce (fn [prior [index step]]
                             (assoc prior (:id step)
                                    (audit-step! step prior premises index)))
                           {}
                           (map-indexed vector steps))
            reports (mapv (fn [step] (get ledger (:id step))) steps)
            last-step (last reports)
            derived (:premises last-step)
            statuses (frequencies (map :local-semantic-status reports))]
        (when-not (= (:conclusion last-step) goal)
          (reject! :source-goal-mismatch [:goal] {}))
        (when-not (= derived (:open-premises checked))
          (reject! :kernel-premise-correspondence-mismatch [:premises]
                   {:derived derived :kernel (:open-premises checked)}))
        (assoc base
               :checked-step-count (count reports)
               :source-rule-correspondence :all-step-shapes-match
               :open-premise-correspondence :exact-match
               :status-counts statuses
               :status (cond
                         (pos? (get statuses :counterexample 0)) :counterexample
                         (pos? (get statuses :not-checked 0)) :not-checked
                         (pos? (get statuses :vacuous-hypotheses 0)) :vacuous-instances
                         :else :complete-for-supplied-source-instances)
               :step-reports reports)))))
