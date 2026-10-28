(ns gesso.live.application-preflight.relational-definition.sequence-test
  "Relational definition: sequence.

   Extracted from the 171-test regression suite without changing tests or assertions."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.application-preflight.relational-definition :as sut]
   [gesso.live.application-preflight.relational-definition.support :refer [counter-key initial-state relational-model definition error-kind modeled-transition-tree comparison-for-edge with-sequence-comparator set-sequence-response! sequence-root-edge sequence-child-edge]]
   [gesso.live.application-preflight.relational-definition.xtdb-fixture :refer [with-real-xtdb-root-step]]))

(deftest v876-matching-two-step-observations-reconstruct-an-explored-rooted-prefix
  (with-sequence-comparator
    definition
    (fn [{:keys [handle tree responses calls]}]
      (let [first-edge (sequence-root-edge tree)
            second-edge (sequence-child-edge tree first-edge)
            first-evidence (Object.)
            second-evidence (Object.)]
        (is (some? second-edge))
        (set-sequence-response! responses first-evidence first-edge)
        (set-sequence-response! responses second-evidence second-edge)
        (let [report (sut/compare-observation-sequence!
                      handle [first-evidence second-evidence])
              [one two] (:steps report)
              ending (set (:matching-final-modeled-node-ids report))
              witnesses (:modeled-prefix-witnesses report)]
          (is (= :caller-ordered-choice-outcome-path-explored (:status report)))
          (is (= 2 (:observation-count report)))
          (is (= 2 (:model-max-depth report)))
          (is (= [first-evidence second-evidence] @calls))
          (is (= [0 1] (mapv :step-index (:steps report))))
          (is (= [(:registered-operation-id first-edge)
                  (:registered-operation-id second-edge)]
                 (mapv :registered-operation-id (:steps report))))
          (is (= [(:model-instance-id first-edge)
                  (:model-instance-id second-edge)]
                 (mapv :model-instance-id (:steps report))))
          (is (= [(:status first-edge) (:status second-edge)]
                 (mapv :physical-outcome (:steps report))))
          (is (contains? (set (:reachable-modeled-node-ids one))
                         (:node-id first-edge)))
          (is (contains? ending (:node-id second-edge)))
          (is (= ending (set (map :matched-node-id witnesses))))
          (is (every? #(= [1 2] (mapv :depth (:steps %))) witnesses))
          (is (every? (fn [{:keys [steps]}]
                        (= (:node-id (first steps))
                           (:parent-node-id (second steps))))
                      witnesses))
          (is (= :caller-supplied-not-observed-chronology
                 (:source-order report)))
          (is (= :not-established (:physical-inter-step-state-continuity report)))
          (is (= :not-established (:production-trace-refinement report))))))))

(deftest v876-model-transition-order-is-not-an-unordered-bag-of-operations
  (let [gated (fn [options]
                (-> (definition options)
                    (assoc-in [:relational-model :families 0 :enabled?]
                              (fn [state _ _] (zero? (:n (get state counter-key)))))
                    (assoc-in [:relational-model :families 1 :enabled?]
                              (fn [state _ _] (pos? (:n (get state counter-key)))))
                    (assoc-in [:relational-model :families 2 :enabled?]
                              (fn [& _] false))))]
    (with-sequence-comparator
      gated
      (fn [{:keys [handle tree responses calls]}]
        (let [root-edge (sequence-root-edge tree)
              deep-edge (sequence-child-edge tree root-edge)
              a (Object.) b (Object.)]
          (is (= :request/claim (:registered-operation-id root-edge)))
          (is (= :request/reassign (:registered-operation-id deep-edge)))
          (set-sequence-response! responses a root-edge)
          (set-sequence-response! responses b deep-edge)
          (let [forward (sut/compare-observation-sequence! handle [a b])
                reverse (sut/compare-observation-sequence! handle [b a])]
            (is (= :caller-ordered-choice-outcome-path-explored
                   (:status forward)))
            (is (= :caller-ordered-choice-outcome-path-not-explored
                   (:status reverse)))
            (is (empty? (:matching-final-modeled-node-ids reverse)))
            (is (empty? (:modeled-prefix-witnesses reverse)))
            (is (= [a b b a] @calls))))))))

(deftest v876-same-operation-evidence-requires-two-distinct-handles
  (with-sequence-comparator
    definition
    (fn [{:keys [handle tree responses calls]}]
      (let [edge (sequence-root-edge tree)
            a (Object.) b (Object.)]
        (set-sequence-response! responses a edge)
        (set-sequence-response! responses b edge)
        (is (= :invalid-observation-sequence
               (error-kind #(sut/compare-observation-sequence! handle [a a]))))
        (is (empty? @calls))
        (let [report (sut/compare-observation-sequence! handle [a b])]
          (is (= :caller-ordered-choice-outcome-path-explored (:status report)))
          (is (= 2 (:observation-count report)))
          (is (= [a b] @calls)))))))

(deftest v876-invalid-sequence-shapes-are-rejected-before-comparison
  (with-sequence-comparator
    definition
    (fn [{:keys [handle calls]}]
      (let [a (Object.) b (Object.) c (Object.)]
        (doseq [invalid [nil [] [nil] [a nil] [a a]
                         [a b c] (list a b) #{a b} {:a a} "ab"]]
          (is (= :invalid-observation-sequence
                 (error-kind #(sut/compare-observation-sequence!
                                handle invalid)))))
        (is (empty? @calls))))))

(deftest v876-plan-mismatch-remains-a-mismatch-even-if-outcome-matches
  (with-sequence-comparator
    definition
    (fn [{:keys [handle tree responses calls]}]
      (let [edge (sequence-root-edge tree)
            a (Object.) b (Object.)]
        (set-sequence-response! responses a edge)
        (swap! responses assoc b (assoc (comparison-for-edge edge)
                                        :status :counterexample))
        (let [report (sut/compare-observation-sequence! handle [a b])]
          (is (= :one-or-more-submitted-plans-not-matched (:status report)))
          (is (= [:matched :counterexample]
                 (mapv :plan-comparison-status (:steps report))))
          (is (empty? (:matching-final-modeled-node-ids report)))
          (is (empty? (:modeled-prefix-witnesses report)))
          (is (= [a b] @calls)))))))

(deftest v876-missing-physical-outcome-cannot-manufacture-model-transitions
  (with-sequence-comparator
    definition
    (fn [{:keys [handle tree responses]}]
      (let [edge (sequence-root-edge tree)
            a (Object.) b (Object.)]
        (set-sequence-response! responses a edge)
        (swap! responses assoc b (dissoc (comparison-for-edge edge)
                                         :physical-status))
        (let [report (sut/compare-observation-sequence! handle [a b])]
          (is (= :one-or-more-physical-outcomes-unavailable (:status report)))
          (is (= :matched (:plan-comparison-status (second (:steps report)))))
          (is (nil? (:physical-outcome (second (:steps report)))))
          (is (empty? (:matching-final-modeled-node-ids report)))
          (is (empty? (:modeled-prefix-witnesses report))))))))

(deftest v876-invalid-executor-bound-comparison-identity-fails-closed
  (with-sequence-comparator
    definition
    (fn [{:keys [handle tree responses calls]}]
      (let [edge (sequence-root-edge tree)
            a (Object.) b (Object.)]
        (set-sequence-response! responses a edge)
        (swap! responses assoc b
               (assoc (comparison-for-edge edge) :operation :fabricated/action))
        (is (= :invalid-execution-transition-identity
               (error-kind #(sut/compare-observation-sequence! handle [a b]))))
        (is (= [a b] @calls))))))

(deftest v876-sequence-results-are-redacted-inert-and-non-authorizing
  (with-sequence-comparator
    (fn [options]
      (-> (definition options)
          (assoc-in [:relational-model :initial-state counter-key :private]
                    "v876-secret-document")
          (assoc-in [:relational-model :families 0 :principals]
                    [:v876-secret-principal])
          (assoc-in [:relational-model :families 0 :arguments]
                    [{:delta 1 :private "v876-secret-argument"}])))
    (fn [{:keys [handle tree responses calls]}]
      (let [edge (sequence-root-edge tree)
            a (Object.) b (Object.)]
        (set-sequence-response! responses a edge)
        (set-sequence-response! responses b edge)
        (let [report (sut/compare-observation-sequence! handle [a b])
              printed (pr-str report)
              forged (assoc report :production-trace-refinement :proved)]
          (is (= :caller-ordered-choice-outcome-path-explored (:status report)))
          (is (not (re-find #"v876-secret-document|v876-secret-principal|v876-secret-argument|#object\["
                            printed)))
          (is (not (re-find #":initial-state|:commands|:guards|:prepare|:arguments|:principals"
                            printed)))
          (is (= :not-established (:physical-observation-order report)))
          (is (= :not-established (:pinned-xtdb-snapshot report)))
          (is (= :not-established (:application-certification report)))
          (is (= :unknown-construction (error-kind #(sut/inspect report))))
          (is (= :unknown-construction
                 (error-kind #(sut/compare! forged (Object.)))))
          (is (= [a b] @calls)))))))

(deftest v876-stale-assembly-rejects-sequence-before-observing-evidence
  (with-sequence-comparator
    definition
    (fn [{:keys [handle artifact-path calls]}]
      (spit artifact-path "v876-stale-runtime\n")
      (is (= :stale-application-assembly
             (error-kind #(sut/compare-observation-sequence! handle [(Object.)]))))
      (is (empty? @calls)))))

(deftest v876-genuine-executor-bound-xtdb-evidence-matches-one-rooted-model-step
  (with-real-xtdb-root-step
    (fn [{:keys [handle evidence request-id]}]
      (let [tree (modeled-transition-tree handle)
            result (sut/compare-observation-sequence! handle [evidence])
            root (:root-node-id tree)
            root-commits (->> (:edges tree)
                              (filter #(and (= root (:parent-node-id %))
                                            (= :request/claim
                                               (:registered-operation-id %))
                                            (= :committed (:status %))))
                              (mapv :node-id))]
        (is (= :caller-ordered-choice-outcome-path-explored (:status result)))
        (is (= 1 (:observation-count result)))
        (is (= 1 (:model-max-depth result)))
        (is (= root-commits (:matching-final-modeled-node-ids result)))
        (is (= :request/claim (get-in result [:steps 0 :registered-operation-id])))
        (is (= :committed (get-in result [:steps 0 :physical-outcome])))
        (is (= :matched (get-in result [:steps 0 :plan-comparison-status])))
        (is (= :not-established (:physical-observation-order result)))
        (is (= :not-established (:physical-inter-step-state-continuity result)))
        (is (= :not-established (:production-trace-refinement result)))
        (is (not (.contains (pr-str result) request-id)))))))

;; v880: the opt-in v879 join reports bounded model-path feasibility beside
;; controlled captured-dependency adjacency. These are distinct observations.
;; None authenticates caller order, excludes intervening writes, or proves
;; a concrete production trace. Stub the PAIR comparator for classification;
;; the single-observation integration below uses real executor-issued XTDB
;; evidence, and the physical pair comparator is tested in its own namespace.

