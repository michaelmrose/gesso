(ns gesso.model.transition-conformance
  "v1300: compare ONE bounded v1298 persisted XTDB observation against both
   the canonical serial transaction model and the original v1233 guarded
   transition associated with a v1251 certified-plan construction.

   This is diagnostic correspondence for observed data, NOT a verified proof:
   the v1298 observation is a copyable map. This checker cannot authenticate
   its provenance, the actual submitted transaction payload, the internal
   XTDB read state, or all physical writers. It does not issue an executor,
   certificate, or application admission. The caller must separately retain
   the originally issued plan and its installed handler binding.

   One domain update plus the durable command-record creation is supported.
   Physical absence/presence, all mapped fields, and frame equality matter;
   unsupported or malformed source constructions fail closed. A valid but
   divergent observed physical state yields :corresponds? false."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.transition :as abstract]
            [gesso.assurance.transition :as construction]
            [gesso.model.certified-plan :as certified-plan]
            [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.transition-observation :as observation]
            [gesso.model.tx-model :as model]
            [gesso.model.tx-refinement :as refinement]))

(def conformance-version 1)
(def report-type :gesso.model.transition-conformance/observed-abstract-step)
(def comparison-scope
  {:evidence-class :diagnostic-bounded-observed-transition-comparison
   :checked #{:exact-v1298-observation-description
              :declared-plan-and-command-record-consistency
              :canonical-serial-transaction-model-outcome
              :physical-domain-prestate-matches-declared-candidate
              :physical-domain-complete-field-projection
              :typed-abstract-guard-assignment-and-frame
              :observed-physical-poststate-matches-model}
   :assumed #{:honest-v1298-observation-origin
              :trusted-declared-plan-is-submitted-plan
              :isolated-physical-ledger-and-snapshot-window
              :field-map-identifies-full-relevant-document-projection}
   :not-established #{:authenticated-xtdb-submission-or-observation-provenance
                      :transaction-internal-prestate-correspondence
                      :all-production-writer-coverage
                      :universal-xtdb-refinement
                      :authorization-and-business-policy-correctness
                      :independent-checker-soundness
                      :application-invariant-proof-or-certification
                      :runtime-transaction-authority}})

(def ^:private source-keys
  #{:observation :construction :field-map :inputs :target :command-operation})
(def ^:private observation-keys
  #{:type :version :scope :evidence-class :observed-targets
    :declared-plan :observed-before :observed-after :command-target
    :persisted-command-record :command-binding :physical-tx-id
    :physical-status :ledger-window :plan-footprint :witness-provenance
    :abstract-transition-correspondence :untracked-writer-coverage
    :certification :authority})

(defn- reject! [reason at]
  ;; Preserve type and location without embedding potentially private documents.
  (throw (ex-info "Invalid observed abstract/physical transition comparison."
                  {:error/type ::invalid-transition-conformance
                   :reason reason :at at})))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- exact! [x required at]
  (when-not (and (plain-map? x) (nil? (meta x))
                 (= required (set (keys x))))
    (reject! :invalid-source-shape at))
  x)

(defn- observed! [w]
  (exact! w observation-keys [:observation])
  (when-not (and (= observation/observation-type (:type w))
                 (= observation/observation-version (:version w))
                 (= observation/observation-scope (:scope w))
                 (= :bounded-controlled-xtdb-commit-observation
                    (:evidence-class w))
                 (= :committed (:physical-status w))
                 (nat-int? (:physical-tx-id w))
                 (= :adjacent-and-terminal-in-observed-snapshots
                    (:ledger-window w))
                 (= :exact-for-declared-incorporation (:plan-footprint w))
                 (= :caller-linked-not-independently-authenticated
                    (:witness-provenance w))
                 (= :not-established (:abstract-transition-correspondence w))
                 (= :not-established (:untracked-writer-coverage w))
                 (= :none (:certification w))
                 (= :none (:authority w)))
    (reject! :invalid-observation-claims [:observation]))
  (let [plan (model/admitted-plan (:declared-plan w))
        targets (set (map command/target (:commands plan)))
        all-targets (set (concat targets
                                 (map :target (:checks (model/step {} plan)))))
        recorded (resolution/require-record (:persisted-command-record w))
        record-target [resolution/command-entity-type (:xt/id recorded)]
        matching (filterv #(= record-target (command/target %)) (:commands plan))]
    (when-not (and (instance? clojure.lang.PersistentHashSet
                              (:observed-targets w))
                   (nil? (meta (:observed-targets w)))
                   (plain-map? (:observed-before w))
                   (nil? (meta (:observed-before w)))
                   (plain-map? (:observed-after w))
                   (nil? (meta (:observed-after w)))
                   (= all-targets (:observed-targets w))
                   (= all-targets (set (keys (:observed-before w))))
                   (= all-targets (set (keys (:observed-after w))))
                   (= record-target (:command-target w))
                   (= 1 (count matching))
                   (command/create? (first matching))
                   (= recorded (command/after (first matching)))
                   (nil? (get (:observed-before w) record-target))
                   (= recorded (get (:observed-after w) record-target))
                   (= (:gesso.command/binding recorded) (:command-binding w)))
      (reject! :inconsistent-observed-footprint-or-command
               [:observation :declared-plan]))
    {:plan plan :targets all-targets :record-target record-target}))

(defn- projected [doc field-map]
  (reduce-kv (fn [m state-field physical-field]
               (if (contains? doc physical-field)
                 (assoc m state-field (get doc physical-field))
                 m))
             {} field-map))

(defn- physical-shape? [doc target field-map state-types]
  (and (plain-map? doc) (nil? (meta doc))
       (= (second target) (:xt/id doc))
       (every? some? (vals doc))
       (let [allowed (conj (set (vals field-map)) :xt/id)
             required (conj (into #{}
                                  (keep (fn [[field physical]]
                                          (when-not (:optional? (get state-types field))
                                            physical)))
                                  field-map)
                            :xt/id)
             actual (set (keys doc))]
         (and (set/subset? required actual)
              (set/subset? actual allowed)))))

(defn- abstract-observation [transition before inputs after field-map]
  (try
    (abstract/observe-transition! transition
                                  (projected before field-map)
                                  inputs
                                  (projected after field-map))
    (catch clojure.lang.ExceptionInfo e
      ;; A real document may contain a wrong type. That is a mismatch, not
      ;; proof that the whole observation source has been authenticated.
      (if (or (= :gesso.assurance.proof.transition/invalid-transition
                 (:error/type (ex-data e)))
              (and (= :gesso.assurance.rule-eval/invalid-rule-evaluation
                      (:error/type (ex-data e)))
                   (contains? #{:wrong-binding-type :missing-binding}
                              (:reason (ex-data e)))))
        nil
        (throw e)))))

(defn check-observed-transition!
  "Compare the supplied v1298 observation with its ORIGINAL typed construction.
   Complete valid observations that disagree with the declared abstract step
   return :corresponds? false and separate diagnostic checks. Malformed
   sources, incomplete physical footprints, and mismatched plan descriptions
   are rejected. Never uses caller-supplied :valid? or :proved? reports."
  [source]
  (exact! source source-keys [])
  (let [{:keys [construction field-map inputs target command-operation]} source
        w (:observation source)
        {:keys [plan targets record-target]} (observed! w)
        domain (filterv #(not= record-target (command/target %))
                        (:commands plan))]
    (when-not (and (= 1 (count domain))
                   (command/update? (first domain))
                   (= target (command/target (first domain)))
                   (= command-operation (command/operation (first domain))))
      (reject! :unsupported-domain-write-footprint [:declared-plan]))
    (let [domain-command (first domain)
          _ (when (seq (:assertions plan))
              (reject! :unsupported-physical-assertion [:declared-plan]))
          candidate-plan {:commands domain :guards (:guards plan)
                          :emit (:emit plan)}
          candidate (certified-plan/check-candidate!
                     {:construction construction
                      :field-map field-map
                      :command-operation command-operation
                      :target target
                      :current-document (command/before domain-command)
                      :inputs inputs
                      :plan candidate-plan})
          built (construction/build-guarded-replacement! construction)
          expected-before (command/before domain-command)
          actual-before (get (:observed-before w) target)
          actual-after (get (:observed-after w) target)
          state-types (:state-types construction)
          shape-before? (physical-shape? actual-before target field-map state-types)
          shape-after? (physical-shape? actual-after target field-map state-types)
          before-match? (and shape-before?
                             (= (get (refinement/project-model-state
                                             {target expected-before}) target)
                                actual-before))
          reference-before (refinement/observation-state
                            (vec targets) (:observed-before w))
          physical-comparison (refinement/compare-step
                               {:reference-before reference-before
                                :plan plan
                                :targets (vec targets)
                                :observed-before (:observed-before w)
                                :execution {:status :committed}
                                :observed-after (:observed-after w)})
          abstract-result (when (and shape-before? shape-after?)
                            (abstract-observation (:transition built)
                                                  actual-before inputs actual-after
                                                  field-map))
          checks {:declared-prestate before-match?
                  :complete-physical-domain-projection
                  (boolean (and shape-before? shape-after?))
                  :abstract-guard (boolean (:guard-holds? abstract-result))
                  :abstract-assignments (boolean (:assignments-hold? abstract-result))
                  :abstract-frame (boolean (:frame-holds? abstract-result))
                  :canonical-serial-commit
                  (and (= :committed (:expected-status physical-comparison))
                       (= :committed (:observed-status physical-comparison)))
                  :model-poststate (:poststate (:checks physical-comparison))
                  :command-incorporation
                  (= (get (:observed-after w) record-target)
                     (:persisted-command-record w))}]
      (when-not (and (= certified-plan/witness-type (:type candidate))
                     (= :pre-submit-candidate-checked (:checking candidate))
                     (= :none (:authority candidate)))
        (reject! :invalid-original-plan-candidate [:construction]))
      {:type report-type
       :version conformance-version
       :evidence-class :diagnostic-bounded-observed-transition-comparison
       :checking :source-derived-model-and-typed-abstract-relation
       :checks checks
       :corresponds? (every? true? (vals checks))
       :operation (get-in construction [:subject :id])
       :transition-fingerprint (:transition-fingerprint built)
       :subject-fingerprint (:subject-fingerprint built)
       :physical-tx-id (:physical-tx-id w)
       :observed-target-count (count targets)
       :model-mismatched-target-count
       (count (:poststate-mismatches physical-comparison))
       :observation-provenance :caller-supplied-copyable-v1298-report
       :general-refinement :not-established
       :certification :none
       :transaction-authority :none
       :scope comparison-scope})))
