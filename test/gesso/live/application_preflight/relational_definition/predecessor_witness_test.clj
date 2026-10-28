(ns gesso.live.application-preflight.relational-definition.predecessor-witness-test
  "Relational definition: predecessor witness.

   Extracted from the 171-test regression suite without changing tests or assertions."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.application-preflight.fixture :as fixture]
   [gesso.live.application-preflight.relational-definition :as sut]
   [xtdb.api :as xt]
   [gesso.live.application-preflight.relational-definition.support :refer [initial-state definition error-kind modeled-transition-tree v892-root-and-committed-claim-child]]
   [gesso.live.application-preflight.relational-definition.xtdb-fixture :refer [with-real-xtdb-root-step v892-with-authentic-nonroot-predecessor]]))

(deftest v892-genuine-xtdb-prestate-matches-private-model-root
  (with-real-xtdb-root-step
    (fn [{:keys [handle evidence request-id]}]
      (let [{:keys [root child-id]} (v892-root-and-committed-claim-child handle)
            report (sut/compare-observation-with-modeled-predecessor!
                    handle root evidence)]
        (is (= :matches-modeled-predecessor-state (:status report)))
        (is (= root (:modeled-predecessor-node-id report)))
        (is (= [child-id] (:possible-modeled-successor-node-ids report)))
        (is (= :matches-modeled-occurrence-state
               (:modeled-predecessor-state-comparison report)))
        (is (= :matched (:submitted-plan-comparison-status report)))
        (is (= :committed (:physical-outcome report)))
        (is (= :one-admitted-modeled-predecessor-vs-one-executor-bound-prestate
               (:scope report)))
        (is (= :exact-clojure-captured-dependency-document-map-equality
               (:relation report)))
        (is (not (.contains (pr-str report) request-id)))))))

(deftest v892-genuine-xtdb-nonroot-occurrence-differs-from-captured-prestate
  (with-real-xtdb-root-step
    (fn [{:keys [handle evidence]}]
      (let [{:keys [child-id]} (v892-root-and-committed-claim-child handle)
            report (sut/compare-observation-with-modeled-predecessor!
                    handle child-id evidence)]
        (is (= :no-corresponding-modeled-transition (:status report)))
        (is (= :differs-from-modeled-occurrence-state
               (:modeled-predecessor-state-comparison report)))
        (is (empty? (:possible-modeled-successor-node-ids report)))
        (is (= child-id (:modeled-predecessor-node-id report)))
        (is (= :matched (:submitted-plan-comparison-status report)))
        (is (not= :matches-modeled-predecessor-state (:status report)))))))

(deftest v892-unknown-modeled-nodes-cannot-be-used-as-witnesses
  (with-real-xtdb-root-step
    (fn [{:keys [handle evidence]}]
      (let [{:keys [root]} (v892-root-and-committed-claim-child handle)]
        (doseq [node [nil -1 :fabricated (+ 1000000 root)]]
          (is (= :invalid-occurrence-state-witness
                 (error-kind
                  #(sut/compare-observation-with-modeled-predecessor!
                    handle node evidence)))))))))

(deftest v892-forged-observations-and-public-reports-have-no-evidence-authority
  (with-real-xtdb-root-step
    (fn [{:keys [handle]}]
      (let [{:keys [root]} (v892-root-and-committed-claim-child handle)]
        (doseq [fake [nil (Object.) {} (sut/inspect handle)
                      (sut/explain handle)
                      {:operation :request/claim :status :matched}]]
          (is (= :unrecognized-executor-bound-evidence
                 (error-kind
                  #(sut/compare-observation-with-modeled-predecessor!
                    handle root fake)))))))))

(deftest v892-modeled-predecessor-report-is-redacted-inert-and-non-authorizing
  (with-real-xtdb-root-step
    (fn [{:keys [handle evidence request-id]}]
      (let [{:keys [root]} (v892-root-and-committed-claim-child handle)
            report (sut/compare-observation-with-modeled-predecessor!
                    handle root evidence)
            printed (pr-str report)]
        (is (false? (:modeled-state-data-exposed? report)))
        (is (false? (:observed-state-data-exposed? report)))
        (is (false? (:additional-model-callbacks-for-witness? report)))
        (is (false? (:additional-xtdb-reads? report)))
        (is (= :not-authenticated (:caller-supplied-order report)))
        (doseq [dimension [:modeled-predecessor-reached-in-production
                           :pinned-xtdb-snapshot :complete-dependency-coverage
                           :production-trace-refinement :application-certification]]
          (is (= :not-established (get report dimension))))
        (is (not (re-find #":initial-state|:before|:after|:prepare|#object\["
                          printed)))
        (is (not (.contains printed request-id)))
        (is (= :unknown-construction (error-kind #(sut/inspect report))))
        (is (= :unknown-construction
               (error-kind
                #(sut/compare-observation-with-modeled-predecessor!
                  report root evidence))))))))

(deftest v892-plan-mismatch-cannot-be-overridden-by-equal-private-prestate
  (with-real-xtdb-root-step
    (fn [{:keys [handle evidence]}]
      (let [{:keys [root]} (v892-root-and-committed-claim-child handle)
            actual (sut/compare! handle evidence)]
        (with-redefs [sut/compare! (fn [_ _]
                                     (assoc actual :status :counterexample))]
          (let [report (sut/compare-observation-with-modeled-predecessor!
                        handle root evidence)]
            (is (= :submitted-plan-not-matched (:status report)))
            (is (= :matches-modeled-occurrence-state
                   (:modeled-predecessor-state-comparison report)))
            (is (= :counterexample (:submitted-plan-comparison-status report)))
            (is (seq (:possible-modeled-successor-node-ids report)))))))))

(deftest v892-unknown-outcome-never-establishes-an-outgoing-transition
  (with-real-xtdb-root-step
    (fn [{:keys [handle evidence]}]
      (let [{:keys [root]} (v892-root-and-committed-claim-child handle)
            actual (sut/compare! handle evidence)]
        (with-redefs [sut/compare! (fn [_ _]
                                     (assoc actual :physical-status nil))]
          (let [report (sut/compare-observation-with-modeled-predecessor!
                        handle root evidence)]
            (is (= :physical-outcome-unavailable (:status report)))
            (is (nil? (:physical-outcome report)))
            (is (= :matches-modeled-occurrence-state
                   (:modeled-predecessor-state-comparison report)))
            (is (empty? (:possible-modeled-successor-node-ids report)))))))))

(deftest v892-observed-choice-must-have-modeled-outgoing-edge
  (with-real-xtdb-root-step
    (fn [{:keys [handle evidence]}]
      (let [{:keys [root]} (v892-root-and-committed-claim-child handle)
            actual (sut/compare! handle evidence)]
        (with-redefs [sut/compare! (fn [_ _]
                                     (assoc actual :family-instance-id
                                            :not-an-admitted-instance))]
          (let [report (sut/compare-observation-with-modeled-predecessor!
                        handle root evidence)]
            (is (= :no-corresponding-modeled-transition (:status report)))
            (is (= :matches-modeled-occurrence-state
                   (:modeled-predecessor-state-comparison report)))
            (is (= :matched (:submitted-plan-comparison-status report)))
            (is (empty? (:possible-modeled-successor-node-ids report)))))))))

(deftest v892-observation-witness-lookup-uses-captured-state-without-xtdb-reread
  (with-real-xtdb-root-step
    (fn [{:keys [handle evidence]}]
      (let [{:keys [root]} (v892-root-and-committed-claim-child handle)
            q-calls (atom 0)]
        (with-redefs [xt/q (fn [& _]
                            (swap! q-calls inc)
                            (throw (ex-info "Unexpected extra XTDB query" {})))]
          (let [report (sut/compare-observation-with-modeled-predecessor!
                        handle root evidence)]
            (is (= :matches-modeled-predecessor-state (:status report)))
            (is (zero? @q-calls))
            (is (false? (:additional-xtdb-reads? report)))))))))

(deftest v892-stale-construction-rejects-before-consulting-evidence
  (fixture/with-closed-application
    (fn [{:keys [options artifact-path]}]
      (let [handle (sut/construct! (definition options))
            root (:root-node-id (modeled-transition-tree handle))
            compared? (atom false)]
        (spit artifact-path "v892-stale-construction\n")
        (with-redefs [sut/compare! (fn [& _]
                                     (reset! compared? true)
                                     (throw (ex-info "Should not inspect evidence" {})))]
          (is (= :stale-application-assembly
                 (error-kind
                  #(sut/compare-observation-with-modeled-predecessor!
                    handle root (Object.)))))
          (is (false? @compared?)))))))

(deftest v892-invalid-construction-has-no-private-state-witness-authority
  (doseq [fake [nil {} (Object.) :root]]
    (is (= :unknown-construction
           (error-kind
            #(sut/compare-observation-with-modeled-predecessor!
              fake 0 (Object.)))))))

(deftest v892-genuine-xtdb-prestate-matches-a-specific-nonroot-model-occurrence
  (v892-with-authentic-nonroot-predecessor
    (fn [{:keys [handle evidence request-id]}]
      (let [tree (modeled-transition-tree handle)
            root (:root-node-id tree)
            prior-edge (first (filter (fn [edge]
                                        (and (= root (:parent-node-id edge))
                                             (= :request/reassign (:registered-operation-id edge))
                                             (= :committed (:status edge))))
                                      (:edges tree)))
            prior-node (:node-id prior-edge)
            matched (sut/compare-observation-with-modeled-predecessor!
                     handle prior-node evidence)
            unmatched (sut/compare-observation-with-modeled-predecessor!
                       handle root evidence)]
        (is (some? prior-edge))
        (is (= :matches-modeled-predecessor-state (:status matched)))
        (is (= :matches-modeled-occurrence-state
               (:modeled-predecessor-state-comparison matched)))
        (is (= :committed (:physical-outcome matched)))
        (is (seq (:possible-modeled-successor-node-ids matched)))
        (is (= :differs-from-modeled-occurrence-state
               (:modeled-predecessor-state-comparison unmatched)))
        (is (= :no-corresponding-modeled-transition (:status unmatched)))
        (is (not (.contains (pr-str matched) request-id)))
        (is (= :not-established (:modeled-predecessor-reached-in-production matched)))))))
