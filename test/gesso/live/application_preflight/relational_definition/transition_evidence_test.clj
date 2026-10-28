(ns gesso.live.application-preflight.relational-definition.transition-evidence-test
  "Relational definition: transition evidence.

   Extracted from the 171-test regression suite without changing tests or assertions."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.application-preflight.fixture :as fixture]
   [gesso.live.application-preflight.relational-application :as relational-app]
   [gesso.live.application-preflight.relational-rule-coverage :as relations]
   [gesso.live.application-preflight.relational-definition :as sut]
   [gesso.live.application-preflight.relational-definition.support :refer [counter-key initial-state family relational-model definition error-data error-kind relational-audit finite-exploration modeled-operation-exercise stale-guard-definition modeled-transition-tree with-stubbed-transition-comparison comparison-for-edge prefix-context-for!]]))

(deftest transition-tree-explanation-has-an-exact-redacted-boundary
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [handle (sut/construct! (definition options))
            tree (modeled-transition-tree handle)
            edges (:edges tree)
            summary (finite-exploration handle)
            expected-edge-keys #{:node-id :parent-node-id :depth
                                 :model-instance-id :registered-operation-id
                                 :status}]
        (is (= #{:kind :status :scope :root-node-id :node-count :edges
                 :state-identity :domain-completeness
                 :production-correspondence :not-established}
               (set (keys tree))))
        (is (= :bounded-reachable-transition-prefix-tree (:kind tree)))
        (is (= :observed-in-declared-bounded-model (:status tree)))
        (is (= :declared-finite-operation-family-reachable-states (:scope tree)))
        (is (zero? (:root-node-id tree)))
        (is (= 13 (:node-count tree)))
        (is (= 12 (count edges)))
        (is (= (:node-count tree) (:visited-state-occurrences summary)))
        (is (= (count edges) (:attempted-transitions summary)))
        (is (= :occurrences-not-deduplicated (:state-identity tree)))
        (is (every? #(= expected-edge-keys (set (keys %))) edges))
        (is (= (vec (range 1 13)) (mapv :node-id edges)))
        (is (= :not-established (:domain-completeness tree)))
        (is (= :not-established (:production-correspondence tree)))
        (is (contains? (:not-established tree)
                       :production-implementation-correspondence))))))

(deftest transition-tree-preserves-exploration-order-and-parent-depths
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [handle (sut/construct! (definition options))
            edges (:edges (modeled-transition-tree handle))
            depth-by-id (into {0 0} (map (juxt :node-id :depth)) edges)
            registered (:registered-operation-ids (sut/inspect handle))
            declared-order (mapv :id (get-in (definition options)
                                             [:relational-model :families]))]
        (is (= [1 1 1] (mapv :depth (take 3 edges))))
        (is (= [0 0 0] (mapv :parent-node-id (take 3 edges))))
        (is (= declared-order (mapv :registered-operation-id (take 3 edges))))
        (is (= (set registered) (set declared-order)))
        (is (= (vec (repeat 9 2)) (mapv :depth (drop 3 edges))))
        (is (= (vec (mapcat #(repeat 3 %) [1 2 3]))
               (mapv :parent-node-id (drop 3 edges))))
        (is (every? (fn [{:keys [parent-node-id depth]}]
                      (= (inc (get depth-by-id parent-node-id)) depth))
                    edges))
        (is (= #{:committed} (set (map :status edges))))
        ;; The finite search records occurrences, not a deduplicated graph:
        ;; paths A->B and B->A are both present.
        (is (= 13 (count (conj (mapv :node-id edges) 0))))
        (is (= (modeled-transition-tree handle)
               (modeled-transition-tree handle)))))))

(deftest transition-tree-uses-registered-operation-identities-and-counts
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [handle (sut/construct! (definition options))
            audit (get (relational-audit handle) :relational-assurance)
            tree (:modeled-transition-tree audit)
            exercise (:modeled-operation-exercise audit)
            instances (:modeled-instance-exercise exercise)
            mapped (into {} (map (juxt :model-instance-id
                                      :registered-operation-id)) instances)]
        (is (= (count instances) (count mapped)))
        (is (every? (fn [{:keys [model-instance-id
                                  registered-operation-id]}]
                      (= registered-operation-id (get mapped model-instance-id)))
                    (:edges tree)))
        (doseq [{:keys [model-instance-id committed-attempts
                        rejected-attempts]} instances]
          (let [edges (filter #(= model-instance-id (:model-instance-id %))
                              (:edges tree))]
            (is (= committed-attempts
                   (count (filter #(= :committed (:status %)) edges))))
            (is (= rejected-attempts
                   (count (filter #(= :rejected (:status %)) edges))))))
        (is (= (set (:registered-operation-ids (sut/inspect handle)))
               (set (map :registered-operation-id (:edges tree)))))))))

(deftest completely-disabled-model-has-only-a-root-occurrence
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [declared (update-in (definition options)
                                [:relational-model :families]
                                (fn [families]
                                  (mapv (fn [family]
                                          (assoc family
                                                 :enabled? (constantly false)
                                                 :prepare (fn [& _]
                                                            (throw (ex-info
                                                                    "disabled callback ran" {})))))
                                        families)))
            handle (sut/construct! declared)
            tree (modeled-transition-tree handle)
            summary (finite-exploration handle)]
        (is (= 0 (:root-node-id tree)))
        (is (= 1 (:node-count tree)))
        (is (= [] (:edges tree)))
        (is (zero? (:attempted-transitions summary)))
        (is (true? (:frontier-exhausted? summary)))
        (is (= [] (filter :ever-attempted-in-explored-states?
                          (:registered-operation-exercise
                           (modeled-operation-exercise handle)))))))))

(deftest transition-tree-respects-declared-depth-one-boundary
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [declared (assoc-in (definition options)
                               [:relational-model :options :max-depth] 1)
            handle (sut/construct! declared)
            tree (modeled-transition-tree handle)
            summary (finite-exploration handle)]
        (is (= 4 (:node-count tree)))
        (is (= 3 (count (:edges tree))))
        (is (= [0 0 0] (mapv :parent-node-id (:edges tree))))
        (is (= [1 1 1] (mapv :depth (:edges tree))))
        (is (true? (:depth-limited? summary)))
        (is (false? (:frontier-exhausted? summary)))
        (is (= :not-established (:domain-completeness tree)))))))

(deftest rejected-model-attempt-appears-in-tree-without-pretending-to-commit
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [handle (sut/construct!
                    (stale-guard-definition (definition options)))
            tree (modeled-transition-tree handle)
            edges (:edges tree)
            claim-edge (first (filter #(= :request/claim
                                          (:registered-operation-id %)) edges))
            claim-exercise (first
                            (filter #(= :request/claim
                                        (:registered-operation-id %))
                                    (:registered-operation-exercise
                                     (modeled-operation-exercise handle))))]
        (is (= 3 (count edges)))
        (is (= :rejected (:status claim-edge)))
        (is (= 1 (:rejected-attempts claim-exercise)))
        (is (zero? (:committed-attempts claim-exercise)))
        (is (= 2 (count (filter #(= :committed (:status %)) edges))))
        (is (= :not-established (:production-correspondence tree)))))))

(deftest transition-tree-explanation-redacts-principals-arguments-and-state
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [secret-state "v860-private-state-payload"
            secret-principal :helper/private-v860-token
            secret-argument "v860-private-argument-token"
            declared (-> (definition options)
                         (assoc-in [:relational-model :initial-state
                                    counter-key :private-token] secret-state)
                         (assoc-in [:relational-model :families 0 :principals]
                                   [:helper/a secret-principal])
                         (assoc-in [:relational-model :families 0 :arguments]
                                   [{:delta 1 :private secret-argument}])
                         (assoc-in [:relational-model :options :max-depth] 1))
            handle (sut/construct! declared)
            tree (modeled-transition-tree handle)
            rendered (pr-str tree)
            instances (:modeled-instance-exercise
                       (modeled-operation-exercise handle))]
        (is (= 4 (count instances)))
        (is (= 4 (count (:edges tree))))
        (is (every? #(contains? % :model-instance-id) (:edges tree)))
        (is (not (.contains rendered secret-state)))
        (is (not (.contains rendered (name secret-principal))))
        (is (not (.contains rendered secret-argument)))
        (is (not (.contains rendered "#object[")))
        (is (not (.contains rendered ":initial-state")))
        (is (not (.contains rendered ":prepare")))
        (is (not (.contains rendered ":arguments")))))))

(deftest transition-tree-is-inert-and-not-an-admission-capability
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [calls (atom 0)
            declared (update-in (definition options)
                                [:relational-model :families]
                                (fn [families]
                                  (mapv (fn [family]
                                          (update family :prepare
                                                  (fn [prepare]
                                                    (fn [& args]
                                                      (swap! calls inc)
                                                      (apply prepare args)))))
                                        families)))
            handle (sut/construct! declared)
            before @calls
            tree (modeled-transition-tree handle)
            explanation (sut/explain handle)
            forged (assoc-in explanation
                             [:semantic-audit :relational-assurance
                              :modeled-transition-tree :production-correspondence]
                             :proved)]
        (is (pos? before))
        (is (= before @calls))
        (is (= tree (modeled-transition-tree handle)))
        (is (= before @calls))
        (doseq [candidate [tree explanation forged]]
          (is (= :unknown-construction (error-kind #(sut/explain candidate))))
          (is (= :unknown-construction (error-kind #(sut/inspect candidate)))))
        (is (= tree (modeled-transition-tree handle)))))))

(deftest transition-tree-evidence-tampering-fails-before-physical-comparator
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [declared (definition options)
            original (relational-app/require-relational-application!
                      (:application-options declared)
                      (:relational-model declared))
            mutations [[:missing-tree #(update % :finite-relational-verdict
                                              dissoc :transition-tree)]
                       [:missing-edge #(update-in % [:finite-relational-verdict
                                                     :transition-tree :edges] pop)]
                       [:extra-edge #(update-in % [:finite-relational-verdict
                                                   :transition-tree :edges]
                                                conj {:node-id 99})]
                       [:incorrect-outcome #(assoc-in % [:finite-relational-verdict
                                                         :transition-tree :edges 0
                                                         :status] :rejected)]
                       [:invented-outcome #(assoc-in % [:finite-relational-verdict
                                                        :transition-tree :edges 0
                                                        :status] :pending)]
                       [:forged-model-choice #(assoc-in % [:finite-relational-verdict
                                                           :transition-tree :edges 0
                                                           :operation-id]
                                                        :forged/operation)]
                       [:wrong-parent #(assoc-in % [:finite-relational-verdict
                                                    :transition-tree :edges 1
                                                    :parent-node-id] 999)]
                       [:wrong-depth #(assoc-in % [:finite-relational-verdict
                                                   :transition-tree :edges 1
                                                   :depth] 99)]
                       [:duplicate-node-id #(assoc-in % [:finite-relational-verdict
                                                         :transition-tree :edges 1
                                                         :node-id] 1)]
                       [:incorrect-node-count #(assoc-in % [:finite-relational-verdict
                                                             :transition-tree :node-count] 500)]
                       [:unknown-edge-key #(assoc-in % [:finite-relational-verdict
                                                        :transition-tree :edges 0
                                                        :trusted?] true)]]
            comparator-calls (atom 0)]
        (is (= 12 (count (get-in original
                                 [:finite-relational-verdict :transition-tree
                                  :edges]))))
        (with-redefs [relations/build-comparator
                      (fn [& _]
                        (swap! comparator-calls inc)
                        (throw (ex-info "invalid comparator construction" {})))]
          (doseq [[label mutation] mutations]
            (testing (name label)
              (with-redefs [relational-app/require-relational-application!
                            (fn [& _] (mutation original))]
                (let [error (error-data #(sut/construct! declared))]
                  (is (= :incomplete-modeled-transition-tree (:error/kind error)))
                  (is (not (contains? (:error/details error) :initial-state)))
                  (is (not (contains? (:error/details error) :families)))))))
          (is (zero? @comparator-calls)))))))

(deftest transition-tree-explanation-rejects-stale-browser-artifacts
  (fixture/with-closed-application
    (fn [{:keys [options artifact-path]}]
      (let [handle (sut/construct! (definition options))]
        (is (= :bounded-reachable-transition-prefix-tree
               (:kind (modeled-transition-tree handle))))
        (spit artifact-path "v860 changed after admission\n")
        (is (= :stale-application-assembly
               (error-kind #(sut/explain handle))))
        (is (= :stale-application-assembly
               (error-kind #(sut/inspect handle))))))))


;; v862: v861's compare! adds a NON-AUTHORIZING link from executor-bound
;; comparison metadata to nodes already visited by the admitted finite model.
;; For these integration tests, stub ONLY the downstream physical comparator;
;; the application assembly, opaque handle, finite checker, registry identities,
;; and tree are all genuine. These stubs are NOT physical XTDB evidence tests.

(deftest physical-comparison-locates-exact-admitted-choice-and-outcome-occurrences
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison comparator-calls
                 comparator-evidence]}]
      (let [edge (first (:edges tree))
            stub-result (comparison-for-edge edge)
            expected-nodes (->> (:edges tree)
                                (filter #(and (= (:model-instance-id edge)
                                                 (:model-instance-id %))
                                              (= (:status edge) (:status %))))
                                (mapv :node-id))
            evidence (Object.)]
        (reset! physical-comparison stub-result)
        (let [result (sut/compare! handle evidence)
              context (:modeled-transition-context result)]
          (is (= stub-result (dissoc result :modeled-transition-context)))
          (is (= :same-choice-and-outcome-explored (:status context)))
          (is (= expected-nodes (:modeled-transition-node-ids context)))
          (is (= #{:admitted-model-choice :physical-outcome}
                 (:matching-dimensions context)))
          (is (= :declared-finite-operation-family-reachable-states
                 (:scope context)))
          (is (= :not-established
                 (:modeled-state-equals-observed-state? context)))
          (is (= :not-established (:production-trace-refinement context)))
          (is (contains? (:not-established context)
                         :production-sequence-or-concurrency-correspondence))
          (is (= 1 @comparator-calls))
          (is (identical? evidence (first @comparator-evidence))))))))

(deftest comparison-matches-the-synthetic-choice-not-just-operation-name
  (with-stubbed-transition-comparison
    (fn [options]
      (-> (definition options)
          (assoc-in [:relational-model :families 0 :principals]
                    [:helper/a :helper/hidden-b])))
    (fn [{:keys [handle tree physical-comparison]}]
      (let [claim-edges (filterv #(= :request/claim
                                    (:registered-operation-id %))
                                 (:edges tree))
            choices (distinct (map :model-instance-id claim-edges))]
        (is (= 2 (count choices)))
        (doseq [instance-id choices]
          (let [edge (first (filter #(= instance-id (:model-instance-id %))
                                    claim-edges))
                expected (->> claim-edges
                              (filter #(and (= instance-id (:model-instance-id %))
                                            (= :committed (:status %))))
                              (mapv :node-id))]
            (reset! physical-comparison (comparison-for-edge edge))
            (is (= expected
                   (get-in (sut/compare! handle (Object.))
                           [:modeled-transition-context
                            :modeled-transition-node-ids])))))))))

(deftest comparator-match-with-unexplored-outcome-keeps-comparison-verdict
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            other-outcome (if (= :committed (:status edge)) :rejected :committed)
            stub-result (assoc (comparison-for-edge edge)
                               :physical-status other-outcome)]
        (reset! physical-comparison stub-result)
        (let [result (sut/compare! handle (Object.))
              context (:modeled-transition-context result)]
          (is (= stub-result (dissoc result :modeled-transition-context)))
          (is (= :matched (:status result)))
          (is (= :same-choice-and-outcome-not-explored (:status context)))
          (is (= [] (:modeled-transition-node-ids context)))
          (is (= #{:admitted-model-choice :physical-outcome}
                 (:matching-dimensions context)))
          (is (= :not-established (:production-trace-refinement context))))))))

(deftest comparator-counterexample-without-physical-outcome-remains-a-counterexample
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            stub-result (-> (comparison-for-edge edge)
                            (dissoc :physical-status)
                            (assoc :status :counterexample
                                   :reason :actual-execution-while-model-disabled))]
        (reset! physical-comparison stub-result)
        (let [result (sut/compare! handle (Object.))
              context (:modeled-transition-context result)]
          (is (= stub-result (dissoc result :modeled-transition-context)))
          (is (= :counterexample (:status result)))
          (is (= :physical-outcome-not-reported (:status context)))
          (is (= [] (:modeled-transition-node-ids context)))
          (is (= #{:admitted-model-choice} (:matching-dimensions context)))
          (is (= :not-established
                 (:modeled-state-equals-observed-state? context))))))))

(deftest physically-rejected-outcome-links-to-a-rejected-modeled-occurrence
  (with-stubbed-transition-comparison
    (fn [options] (stale-guard-definition (definition options)))
    (fn [{:keys [handle tree physical-comparison]}]
      (let [rejected (filterv #(= :rejected (:status %)) (:edges tree))
            edge (first rejected)]
        (is (seq rejected))
        (reset! physical-comparison (comparison-for-edge edge))
        (let [result (sut/compare! handle (Object.))
              context (:modeled-transition-context result)]
          (is (= :matched (:status result)))
          (is (= :same-choice-and-outcome-explored (:status context)))
          (is (= (mapv :node-id
                       (filter #(= (:model-instance-id edge)
                                   (:model-instance-id %)) rejected))
                 (:modeled-transition-node-ids context)))
          (is (= :not-established (:production-trace-refinement context))))))))

(deftest executor-bound-identity-mismatch-rejects-a-forged-comparison
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison comparator-calls]}]
      (let [edge (first (:edges tree))
            correct (comparison-for-edge edge)]
        (doseq [mutant [(assoc correct :operation :invented/operation)
                        (assoc correct :operation nil)
                        (assoc correct :family-instance-id :invented/choice)
                        (assoc correct :family-instance-id nil)
                        (dissoc correct :family-instance-id)]]
          (reset! physical-comparison mutant)
          (is (= :invalid-execution-transition-identity
                 (error-kind #(sut/compare! handle (Object.))))))
        (is (= 5 @comparator-calls))))))

(deftest unsupported-physical-outcomes-fail-closed-without-promoting-unknown
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            correct (comparison-for-edge edge)]
        (doseq [invalid [:pending :unknown :aborted "committed" false]]
          (reset! physical-comparison (assoc correct :physical-status invalid))
          (is (= :invalid-execution-transition-identity
                 (error-kind #(sut/compare! handle (Object.))))))
        ;; A genuinely missing outcome is not the same as a made-up outcome.
        (reset! physical-comparison (assoc correct :physical-status nil))
        (is (= :physical-outcome-not-reported
               (get-in (sut/compare! handle (Object.))
                       [:modeled-transition-context :status])))))))

(deftest modeled-transition-link-is-redacted-inert-and-non-authorizing
  (with-stubbed-transition-comparison
    (fn [options]
      (-> (definition options)
          (assoc-in [:relational-model :initial-state counter-key :private]
                    "v862-secret-state")
          (assoc-in [:relational-model :families 0 :principals]
                    [:helper/v862-secret-principal])
          (assoc-in [:relational-model :families 0 :arguments]
                    [{:delta 1 :private "v862-secret-argument"}])))
    (fn [{:keys [handle tree physical-comparison comparator-calls]}]
      (let [edge (first (:edges tree))
            comparison (comparison-for-edge edge)]
        (reset! physical-comparison comparison)
        (let [first-result (sut/compare! handle (Object.))
              context (:modeled-transition-context first-result)
              serialized (pr-str context)
              forged (assoc context :production-trace-refinement :proved)]
          (is (= 1 @comparator-calls))
          (is (= context (:modeled-transition-context
                          (sut/compare! handle (Object.)))))
          (is (= 2 @comparator-calls))
          (is (not (.contains serialized "v862-secret-state")))
          (is (not (.contains serialized "v862-secret-principal")))
          (is (not (.contains serialized "v862-secret-argument")))
          (is (not (.contains serialized "#object[")))
          (is (not (.contains serialized ":initial-state")))
          (is (not (.contains serialized ":prepare")))
          (is (= :unknown-construction (error-kind #(sut/explain context))))
          (is (= :unknown-construction (error-kind #(sut/inspect forged))))
          (is (= :unknown-construction
                 (error-kind #(sut/compare! forged (Object.))))))))))

(deftest current-assembly-is-required-before-comparison-or-transition-linking
  (fixture/with-closed-application
    (fn [{:keys [options artifact-path]}]
      (let [calls (atom 0)]
        (with-redefs [relations/build-comparator
                      (fn [& _]
                        (fn [_] (swap! calls inc)
                          (throw (ex-info "Must not compare stale assembly" {}))))]
          (let [handle (sut/construct! (definition options))]
            (spit artifact-path "v862 stale browser artifact\n")
            (is (= :stale-application-assembly
                   (error-kind #(sut/compare! handle (Object.)))))
            (is (zero? @calls))))))))


;; v864: v863 makes the already bounded and redacted occurrence tree useful as
;; MODEL-ONLY predecessor histories for candidate observed comparisons.
;; These tests exercise the public API on genuine admitted constructions; only
;; the downstream physical comparator is stubbed. They do NOT establish a real
;; XTDB execution trace or a simulation/refinement relation.

(deftest candidate-prefix-reconstructs-the-full-depth-two-model-history
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison comparator-calls]}]
      (let [by-id (into {} (map (juxt :node-id identity)) (:edges tree))
            deep (first (filter #(= 2 (:depth %)) (:edges tree)))
            parent (get by-id (:parent-node-id deep))
            expected-steps (mapv #(select-keys % [:node-id :parent-node-id :depth
                                                  :model-instance-id
                                                  :registered-operation-id :status])
                                 [parent deep])]
        (is (some? deep))
        (is (= 1 (:depth parent)))
        (let [ctx (prefix-context-for! handle tree physical-comparison deep)
              witness (first (filter #(= (:node-id deep) (:matched-node-id %))
                                     (:modeled-prefix-witnesses ctx)))]
          (is (= :same-choice-and-outcome-explored (:status ctx)))
          (is (= (:node-id deep) (:matched-node-id witness)))
          (is (= expected-steps (:steps witness)))
          (is (= [1 2] (mapv :depth (:steps witness))))
          (is (= (:root-node-id tree)
                 (:parent-node-id (first (:steps witness)))))
          (is (= :modeled-occurrence-tree-only (:prefix-evidence ctx)))
          (is (= :not-established (:production-trace-refinement ctx)))
          (is (= 1 @comparator-calls)))))))

(deftest every-matching-occurrence-gets-a-distinct-correct-model-prefix
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edges (:edges tree)
            matched (first edges)
            matching (filterv #(and (= (:model-instance-id matched)
                                      (:model-instance-id %))
                                    (= (:status matched) (:status %))) edges)
            by-id (into {} (map (juxt :node-id identity)) edges)
            ctx (prefix-context-for! handle tree physical-comparison matched)
            witnesses (:modeled-prefix-witnesses ctx)]
        (is (> (count matching) 1))
        (is (= (mapv :node-id matching) (:modeled-transition-node-ids ctx)))
        (is (= (mapv :node-id matching) (mapv :matched-node-id witnesses)))
        (is (= (count matching) (count witnesses)))
        (doseq [{:keys [matched-node-id steps]} witnesses]
          (is (= (count steps) (:depth (by-id matched-node-id))))
          (is (= (vec (range 1 (inc (count steps)))) (mapv :depth steps)))
          (is (= matched-node-id (:node-id (last steps))))
          (is (= (:root-node-id tree) (:parent-node-id (first steps))))
          (doseq [[prior current] (partition 2 1 steps)]
            (is (= (:node-id prior) (:parent-node-id current)))))
        (is (= :not-established (:modeled-state-equals-observed-state? ctx)))))))

(deftest prefix-witnesses-are-scoped-to-the-exact-expanded-model-choice
  (with-stubbed-transition-comparison
    (fn [options]
      (assoc-in (definition options) [:relational-model :families 0 :principals]
                [:helper/a :helper/other]))
    (fn [{:keys [handle tree physical-comparison]}]
      (let [claim-edges (filterv #(= :request/claim
                                    (:registered-operation-id %)) (:edges tree))
            choices (distinct (map :model-instance-id claim-edges))]
        (is (= 2 (count choices)))
        (doseq [choice choices]
          (let [edge (first (filter #(= choice (:model-instance-id %)) claim-edges))
                ctx (prefix-context-for! handle tree physical-comparison edge)
                witnesses (:modeled-prefix-witnesses ctx)]
            (is (seq witnesses))
            (is (= (->> claim-edges
                        (filter #(and (= choice (:model-instance-id %))
                                      (= (:status edge) (:status %))))
                        (mapv :node-id))
                   (mapv :matched-node-id witnesses)))
            (is (every? #(= choice (-> % :steps last :model-instance-id))
                        witnesses))
            (is (= :not-established (:production-trace-refinement ctx)))))))))

(deftest rejected-modeled-attempts-produce-rejected-terminal-prefix-steps
  (with-stubbed-transition-comparison
    (fn [options] (stale-guard-definition (definition options)))
    (fn [{:keys [handle tree physical-comparison]}]
      (let [rejected (filterv #(= :rejected (:status %)) (:edges tree))
            edge (first rejected)]
        (is (seq rejected))
        (let [ctx (prefix-context-for! handle tree physical-comparison edge)]
          (is (= :same-choice-and-outcome-explored (:status ctx)))
          (is (seq (:modeled-prefix-witnesses ctx)))
          (doseq [{:keys [matched-node-id steps]} (:modeled-prefix-witnesses ctx)]
            (is (= matched-node-id (:node-id (last steps))))
            (is (= :rejected (:status (last steps)))))
          (is (contains? (:not-established ctx)
                         :model-prefix-is-not-observed-physical-history)))))))

(deftest missing-physical-outcome-does-not-fabricate-model-history
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            counterexample (-> (comparison-for-edge edge)
                               (dissoc :physical-status)
                               (assoc :status :counterexample
                                      :reason :observed-while-model-disabled))]
        (reset! physical-comparison counterexample)
        (let [result (sut/compare! handle (Object.))
              ctx (:modeled-transition-context result)]
          (is (= counterexample (dissoc result :modeled-transition-context)))
          (is (= :counterexample (:status result)))
          (is (= :physical-outcome-not-reported (:status ctx)))
          (is (= [] (:modeled-transition-node-ids ctx)))
          (is (= [] (:modeled-prefix-witnesses ctx)))
          (is (= #{:admitted-model-choice} (:matching-dimensions ctx)))
          (is (= :not-established (:production-trace-refinement ctx))))))))

(deftest unvisited-choice-outcome-has-no-prefix-but-preserves-comparison
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            opposite (if (= :committed (:status edge)) :rejected :committed)
            comparison (assoc (comparison-for-edge edge) :physical-status opposite)]
        (reset! physical-comparison comparison)
        (let [result (sut/compare! handle (Object.))
              ctx (:modeled-transition-context result)]
          (is (= comparison (dissoc result :modeled-transition-context)))
          (is (= :matched (:status result)))
          (is (= :same-choice-and-outcome-not-explored (:status ctx)))
          (is (= [] (:modeled-transition-node-ids ctx)))
          (is (= [] (:modeled-prefix-witnesses ctx)))
          (is (= #{:admitted-model-choice :physical-outcome}
                 (:matching-dimensions ctx)))
          (is (= :not-established (:modeled-state-equals-observed-state? ctx))))))))

(deftest prefix-witnesses-are-inert-redacted-and-not-admission-handles
  (with-stubbed-transition-comparison
    (fn [options]
      (-> (definition options)
          (assoc-in [:relational-model :initial-state counter-key :private]
                    "v864-hidden-state")
          (assoc-in [:relational-model :families 0 :principals]
                    [:helper/v864-secret-principal])
          (assoc-in [:relational-model :families 0 :arguments]
                    [{:delta 1 :private "v864-secret-argument"}])))
    (fn [{:keys [handle tree physical-comparison comparator-calls]}]
      (let [edge (first (:edges tree))
            comparison (comparison-for-edge edge)]
        (reset! physical-comparison comparison)
        (let [one (sut/compare! handle (Object.))
              two (sut/compare! handle (Object.))
              ctx (:modeled-transition-context one)
              witnesses (:modeled-prefix-witnesses ctx)
              printed (pr-str witnesses)]
          (is (= ctx (:modeled-transition-context two)))
          (is (= 2 @comparator-calls))
          (is (seq witnesses))
          (is (not (re-find #"v864-hidden-state|v864-secret-principal|v864-secret-argument" printed)))
          (is (not (re-find #"#object\[|:initial-state|:prepare|:arguments|:principals|:commands" printed)))
          (is (= :unknown-construction (error-kind #(sut/explain witnesses))))
          (is (= :unknown-construction
                 (error-kind #(sut/compare! (first witnesses) (Object.)))))
          (is (= :not-established (:production-trace-refinement ctx)))
          (is (contains? (:not-established ctx)
                         :production-sequence-or-concurrency-correspondence)))))))

(deftest stale-admitted-assembly-prevents-comparison-and-prefix-construction
  (fixture/with-closed-application
    (fn [{:keys [options artifact-path]}]
      (let [comparator-calls (atom 0)]
        (with-redefs [relations/build-comparator
                      (fn [& _]
                        (fn [_] (swap! comparator-calls inc)
                          (throw (ex-info "stale assembly must not compare" {}))))]
          (let [handle (sut/construct! (definition options))]
            (spit artifact-path "v864 invalidated browser artifact\n")
            (is (= :stale-application-assembly
                   (error-kind #(sut/compare! handle (Object.)))))
            (is (zero? @comparator-calls))))))))


;; v868: public root-step correspondence checks. The controlled comparator
;; stubs below isolate the classification logic; only the final test uses
;; genuine executor-bound XTDB evidence. Neither kind proves trace refinement.

