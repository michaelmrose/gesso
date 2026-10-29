(ns gesso.live.application-preflight.relational-definition.local-step-test
  "Relational definition: local step.

   Tests the v874 redacted :local-step-observation synthesized from an
   executor-bound root-step comparison and controlled domain after-read.
   A matching local step establishes only one finite modeled ROOT step,
   never pinned snapshots, non-root correspondence, general production
   trace refinement, or application certification. The real-XTDB case
   checks the executor-bound route; controlled comparator cases test
   matching, unknown, mismatch, and counterexample behavior.

   Extracted from the 171-test regression suite without changing tests or assertions."
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.live.application-preflight.fixture :as fixture]
   [gesso.live.application-preflight.relational-rule-coverage :as relations]
   [gesso.live.application-preflight.relational-definition :as sut]
   [gesso.live.application-preflight.relational-definition.support :refer [counter-key definition error-kind modeled-transition-tree with-stubbed-transition-comparison comparison-for-edge matching-root-observation root-step-for! v872-matched-poststate v874-established-dimensions v874-local-for!]]
   [gesso.live.application-preflight.relational-definition.xtdb-fixture :refer [with-real-xtdb-root-step]]))

(deftest v874-real-xtdb-execution-establishes-local-step-observation
  (with-real-xtdb-root-step
    (fn [{:keys [handle evidence request-id]}]
      (let [result (sut/compare! handle evidence)
            root-step (get-in result [:modeled-transition-context
                                      :root-step-correspondence])
            local (:local-step-observation root-step)]
        (is (= :matched (:status result)))
        (is (= :matching-root-step-explored (:status root-step)))
        (is (= :matches-captured-posttransaction-dependencies
               (get-in root-step [:captured-posttransaction-dependency-comparison
                                  :status])))
        (is (= :matching-captured-root-step-before-and-after (:status local)))
        (is (= (:matched-root-transition-node-ids root-step)
               (:matched-root-transition-node-ids local)))
        (is (seq (:matched-root-transition-node-ids local)))
        (is (= v874-established-dimensions (:established-dimensions local)))
        (is (= :one-executor-bound-attempt-vs-one-modeled-root-step
               (:scope local)))
        (is (= :bounded-controlled-execution-observation
               (:evidence-class local)))
        (is (false? (:state-contents-exposed? local)))
        (is (every? #(= :not-established (get local %))
                    [:pinned-xtdb-snapshot :non-root-step-correspondence
                     :physical-trace-refinement :application-certification]))
        (is (not (.contains (pr-str local) request-id)))))))

(deftest v874-matching-controlled-root-and-poststate-produce-exact-scope
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison comparator-calls]}]
      (let [edge (first (:edges tree))
            root-step (root-step-for!
                       handle physical-comparison edge
                       {:declared-root-poststate-comparison v872-matched-poststate})
            local (:local-step-observation root-step)]
        (is (= :matching-captured-root-step-before-and-after (:status local)))
        (is (= (:matched-root-transition-node-ids root-step)
               (:matched-root-transition-node-ids local)))
        (is (= v874-established-dimensions (:established-dimensions local)))
        (is (= 1 @comparator-calls))
        (is (= #{:status :matched-root-transition-node-ids
                 :established-dimensions :scope :evidence-class
                 :state-contents-exposed? :pinned-xtdb-snapshot
                 :non-root-step-correspondence :physical-trace-refinement
                 :application-certification}
               (set (keys local))))))))

(deftest v874-missing-after-read-is-unknown-not-a-matching-step
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            root-step (root-step-for! handle physical-comparison edge {})
            local (:local-step-observation root-step)]
        (is (= :matching-root-step-explored (:status root-step)))
        (is (= :captured-poststate-not-reported
               (get-in root-step [:captured-posttransaction-dependency-comparison
                                  :status])))
        (is (= :captured-poststate-not-established (:status local)))
        (is (= [] (:matched-root-transition-node-ids local)))
        (is (= #{} (:established-dimensions local)))))))

(deftest v874-different-poststate-blocks-combined-match
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            local (v874-local-for!
                   handle physical-comparison edge
                   {:declared-root-poststate-comparison
                    (assoc v872-matched-poststate :status
                           :differs-from-captured-posttransaction-dependencies)})]
        (is (= :captured-poststate-differs (:status local)))
        (is (= [] (:matched-root-transition-node-ids local)))
        (is (= #{} (:established-dimensions local)))
        (is (= :not-established (:physical-trace-refinement local)))))))

(deftest v874-different-modeled-and-physical-outcomes-block-match
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            local (v874-local-for!
                   handle physical-comparison edge
                   {:declared-root-poststate-comparison
                    (assoc v872-matched-poststate :status
                           :modeled-and-physical-outcomes-disagree)})]
        (is (= :modeled-and-physical-outcomes-disagree (:status local)))
        (is (empty? (:matched-root-transition-node-ids local)))
        (is (empty? (:established-dimensions local)))))))

(deftest v874-root-mismatch-cannot-borrow-poststate-agreement
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            root-step (root-step-for!
                       handle physical-comparison edge
                       {:declared-root-state-comparison
                        (assoc matching-root-observation :status
                               :differs-from-captured-pretransaction-state)
                        :declared-root-poststate-comparison v872-matched-poststate})
            local (:local-step-observation root-step)]
        (is (= :declared-root-not-established-equal-to-observation
               (:status root-step)))
        (is (= :root-step-not-established (:status local)))
        (is (empty? (:matched-root-transition-node-ids local)))
        (is (empty? (:established-dimensions local)))))))

(deftest v874-plan-counterexample-is-never-rehabilitated
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            source (merge (comparison-for-edge edge)
                          {:status :counterexample
                           :reason :modeled-and-submitted-plans-disagree
                           :declared-root-state-comparison matching-root-observation
                           :declared-root-poststate-comparison v872-matched-poststate})
            _ (reset! physical-comparison source)
            compared (sut/compare! handle (Object.))
            local (get-in compared [:modeled-transition-context
                                    :root-step-correspondence
                                    :local-step-observation])]
        (is (= :counterexample (:status compared)))
        (is (= :modeled-and-submitted-plans-disagree (:reason compared)))
        (is (= source (dissoc compared :modeled-transition-context)))
        (is (= :root-step-not-established (:status local)))
        (is (empty? (:matched-root-transition-node-ids local)))))))

(deftest v874-unknown-physical-outcome-does-not-create-local-step
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            root-step (root-step-for!
                       handle physical-comparison edge
                       {:physical-status nil
                        :declared-root-poststate-comparison v872-matched-poststate})
            local (:local-step-observation root-step)]
        (is (= :physical-outcome-not-reported (:status root-step)))
        (is (= :root-step-not-established (:status local)))
        (is (empty? (:established-dimensions local)))))))

(deftest v874-deeper-model-transition-is-not-root-correspondence
  (with-stubbed-transition-comparison
    (fn [options]
      (assoc-in (definition options) [:relational-model :families 0 :enabled?]
                (fn [state _ _] (pos? (:n (get state counter-key))))))
    (fn [{:keys [handle tree physical-comparison]}]
      (let [root (:root-node-id tree)
            edge (first (filter #(and (= :request/claim
                                        (:registered-operation-id %))
                                      (not= root (:parent-node-id %)))
                                (:edges tree)))]
        (is (some? edge))
        (when edge
          (let [root-step (root-step-for!
                           handle physical-comparison edge
                           {:declared-root-poststate-comparison
                            v872-matched-poststate})
                local (:local-step-observation root-step)]
            (is (= :same-choice-outcome-not-explored-at-root
                   (:status root-step)))
            (is (= :root-step-not-established (:status local)))
            (is (empty? (:matched-root-transition-node-ids local)))))))))

(deftest v874-invalid-controlled-after-read-rejects-comparison
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison comparator-calls]}]
      (let [edge (first (:edges tree))
            source (merge (comparison-for-edge edge)
                          {:declared-root-state-comparison matching-root-observation})
            malformed [(assoc v872-matched-poststate :scope :fabricated)
                       (assoc v872-matched-poststate :poststate-data-exposed? true)
                       (assoc v872-matched-poststate :status :invented)
                       (assoc v872-matched-poststate :pinned-xtdb-snapshot :proven)]]
        (doseq [poststate malformed]
          (reset! physical-comparison
                  (assoc source :declared-root-poststate-comparison poststate))
          (is (= :invalid-controlled-root-poststate-evidence
                 (error-kind #(sut/compare! handle (Object.))))))
        (is (= (count malformed) @comparator-calls))))))

(deftest v874-local-step-report-is-inert-redacted-and-freshness-bound
  (fixture/with-closed-application
    (fn [{:keys [options artifact-path]}]
      (let [comparison (atom nil)
            calls (atom 0)]
        (with-redefs [relations/build-comparator
                      (fn [& _] (fn [_] (swap! calls inc) @comparison))]
          (let [private-definition
                (-> (definition options)
                    (assoc-in [:relational-model :initial-state counter-key :private]
                              "v874-sensitive-root")
                    (assoc-in [:relational-model :families 0 :arguments]
                              [{:delta 1 :private "v874-sensitive-args"}]))
                handle (sut/construct! private-definition)
                edge (first (:edges (modeled-transition-tree handle)))
                _ (reset! comparison
                          (merge (comparison-for-edge edge)
                                 {:declared-root-state-comparison
                                  matching-root-observation
                                  :declared-root-poststate-comparison
                                  v872-matched-poststate}))
                first-report (sut/compare! handle (Object.))
                second-report (sut/compare! handle (Object.))
                local (get-in first-report
                              [:modeled-transition-context
                               :root-step-correspondence
                               :local-step-observation])]
            (is (= :matching-captured-root-step-before-and-after
                   (:status local)))
            (is (= local (get-in second-report
                                 [:modeled-transition-context
                                  :root-step-correspondence
                                  :local-step-observation])))
            (is (not (re-find #"v874-sensitive-root|v874-sensitive-args|#object\["
                              (pr-str local))))
            (is (= :unknown-construction (error-kind #(sut/explain local))))
            (is (= :unknown-construction
                   (error-kind #(sut/compare! local (Object.)))))
            (is (= 2 @calls))
            (spit artifact-path "v874-expired-artifact\n")
            (is (= :stale-application-assembly
                   (error-kind #(sut/compare! handle (Object.)))))
            (is (= 2 @calls))))))))

