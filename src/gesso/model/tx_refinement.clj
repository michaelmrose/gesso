(ns gesso.model.tx-refinement
  "A deliberately bounded, pure observation relation between the serial
   gesso.model.tx-model transition and documents read from XTDB2.

   This module does NOT run XTDB or create a proof/certification artifact.
   Callers must execute the production transaction and independently read
   every specified target, before and after execution. Observations represent
   *queried and absent* by an explicit nil at that target; omitting a target
   means it was not observed and is rejected.

   Within this limited comparison, XTDB2's omission of top-level nil-valued
   fields is made explicit. Nested collections, scalar conversions, and SQL
   NULL semantics are not normalized or generally verified. In particular,
   agreement of supplied observations does not establish that they were
   collected honestly, that writes serialize, or that all policy inputs were
   identified. Formal step refinement requires separate backend assumptions
   and evidence beyond this finite observation relation."
  (:require
   [clojure.set :as set]
   [gesso.model.command :as command]
   [gesso.model.tx-model :as model]))

(def comparison-scope
  {:relation :gesso.model.tx/physical-observation-v1
   :checked #{:explicit-target-coverage
              :top-level-nil-field-projection
              :reference-prestate-observation
              :reference-outcome-status
              :recognized-xtdb-assertion-conflict
              :reference-poststate-observation}
   :assumed #{:independent-before-and-after-physical-reads
              :isolated-read-transaction-read-window
              :read-your-committed-write
              :serialized-atomic-commit
              :production-plan-lowering
              :honest-execution-classification}
   :not-established #{:general-xtdb-step-refinement
                      :database-isolation-and-temporal-history
                      :physical-read-completeness-outside-declared-targets
                      :writer-and-dependency-closure
                      :all-scalar-and-nested-value-serialization
                      :authorization-and-business-invariants
                      :durable-command-incorporation-and-replay
                      :browser-or-choreography-correspondence}})

(defn- fail!
  [kind message details]
  (throw (ex-info message {:error/type kind :error/details details})))

(defn- target?
  [target]
  (and (vector? target)
       (= 2 (count target))
       (keyword? (first target))
       (some? (second target))))

(defn require-targets
  "A nonempty vector of unique [table id] pairs; its complete set is the
   declared physical observation domain for this comparison."
  [targets]
  (when-not (and (vector? targets)
                 (seq targets)
                 (every? target? targets)
                 (= (count targets) (count (distinct targets))))
    (fail! ::invalid-targets
           "The observation domain must contain unique, valid targets."
           {:targets targets}))
  targets)

(defn project-model-state
  "Project a reference state to the representation observed with XTDB2's
   SELECT * for the admitted document subset. Absent documents remain absent;
   only *top-level* nil-valued fields are omitted. Nested values and all
   non-nil scalar values are left unchanged for strict equality comparison.
   This is not a general-purpose XTDB serializer."
  [reference-state]
  (into {}
        (map (fn [[target doc]]
               [target (into {} (remove (comp nil? val)) doc)]))
        (model/require-state reference-state)))

(defn observation-state
  "Convert a COMPLETE observation {target -> document-or-nil} to the same
   absent-key state convention as the reference model. A nil document means
   the requested target was queried and found absent. An omitted entry does
   NOT mean absence. Document identity and non-nil top-level field presence
   are required exactly as expected from a SELECT * physical observation."
  [targets observation]
  (let [domain (set (require-targets targets))]
    (when-not (and (map? observation)
                   (= domain (set (keys observation))))
      (fail! ::incomplete-observation
             "Every tracked target must have an explicit document-or-nil observation."
             {:missing (if (map? observation)
                         (set/difference domain (set (keys observation)))
                         domain)
              :unexpected (if (map? observation)
                            (set/difference (set (keys observation)) domain)
                            #{})}))
    (doseq [[target document] observation
            :when (some? document)]
      (when-not (and (map? document)
                     (= (second target) (:xt/id document))
                     (every? some? (vals document)))
        (fail! ::invalid-physical-document
               "Observed XTDB documents must have matching identities and no top-level nil fields."
               {:target target})))
    (into {} (remove (comp nil? val)) observation)))

(defn state-relation
  "Return a *bounded observational* relation witness for a model state and
   complete physical reads over `targets`. Malformed or partial observations
   fail closed; legitimate state disagreements return :relates? false.

   Any model keys beyond `targets` are rejected: silently dropping modeled
   documents would make a partial read look like an application-wide proof."
  [targets reference-state physical-observation]
  (let [domain (set (require-targets targets))
        expected (project-model-state reference-state)]
    (when-not (set/subset? (set (keys expected)) domain)
      (fail! ::untracked-model-state
             "Reference state contains a document outside the observation domain."
             {:untracked (set/difference (set (keys expected)) domain)}))
    (let [observed (observation-state targets physical-observation)
          mismatched (->> targets
                          (filter (fn [target]
                                    (not= (get expected target ::absent)
                                          (get observed target ::absent))))
                          vec)]
      {:relates? (empty? mismatched)
       :mismatched-targets mismatched
       :tracked-target-count (count targets)})))

(defn- required-footprint
  [plan prediction]
  (set (concat (keys (:before prediction))
               (keys (:after prediction))
               (map :target (:checks prediction))
               (map command/target (:commands plan)))))

(defn compare-step
  "Compare one *supplied* physical transition with the admitted serial model.

   Input keys are exactly:
     :reference-before  finite model state
     :plan              admitted gesso.model.tx plan
     :targets           complete tracked-target vector
     :observed-before   {target -> physical doc or nil (queried absent)}
     :execution         {:status :committed} or
                        {:status :rejected :error/code :xtdb/assert-failed}
     :observed-after    complete post-execution observation of same targets

   All read dependencies, write targets, and represented model documents must
   be tracked even when a command is rejected. This protects against vacuous
   agreement caused by accidentally omitting a write target. The caller must
   perform physical reads and classify real XTDB errors; these input maps
   are not themselves authenticated proof evidence.

   Returns :corresponds? and individually inspectable checks. Disagreements
   are DATA, not successful certificates. Malformed or incomplete observations,
   unsupported plans, and unrecognized outcome statuses fail closed."
  [input]
  (let [required #{:reference-before :plan :targets :observed-before
                   :execution :observed-after}]
    (when-not (and (map? input) (= required (set (keys input))))
      (fail! ::invalid-input
             "A correspondence comparison needs all six explicit inputs."
             {:missing (set/difference required (set (keys input)))
              :unexpected (set/difference (set (keys input)) required)})))
  (let [{:keys [reference-before plan targets observed-before
                execution observed-after]} input
        targets (require-targets targets)
        plan (model/admitted-plan plan)
        prediction (model/step reference-before plan)
        required (required-footprint plan prediction)
        untracked (set/difference required (set targets))]
    (when (seq untracked)
      (fail! ::untracked-dependency
             "Observation targets omit a model read dependency or write target."
             {:untracked untracked}))
    (when-not (and (map? execution)
                   (#{:committed :rejected} (:status execution)))
      (fail! ::invalid-execution
             "Execution must declare either :committed or :rejected."
             {:execution execution}))
    (let [before (state-relation targets (:before prediction) observed-before)
          after (state-relation targets (:after prediction) observed-after)
          status? (= (:status prediction) (:status execution))
          conflict? (if (= :rejected (:status execution))
                      (= :xtdb/assert-failed (:error/code execution))
                      (not (contains? execution :error/code)))
          checks {:prestate (:relates? before)
                  :outcome-status status?
                  :recognized-conflict conflict?
                  :poststate (:relates? after)}]
      {:corresponds? (every? true? (vals checks))
       :checks checks
       :prestate-mismatches (:mismatched-targets before)
       :poststate-mismatches (:mismatched-targets after)
       :expected-status (:status prediction)
       :observed-status (:status execution)
       :failed-precondition-kind (get-in prediction [:failed-precondition :kind])
       :tracked-target-count (count targets)
       :scope comparison-scope})))
