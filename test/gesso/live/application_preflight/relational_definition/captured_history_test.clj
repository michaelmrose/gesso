(ns gesso.live.application-preflight.relational-definition.captured-history-test
  "Relational definition: captured history.

   Extracted from the 171-test regression suite without changing tests or assertions."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.application-preflight.relational-definition :as sut]
   [gesso.live.application-preflight.relational-definition.support :refer [counter-key initial-state family relational-model definition error-kind comparison-for-edge with-v880-adjacency with-v882-root-anchor v882-anchor v886-matching-captured-local-step v886-local-joint v886-set-local!]]
   [gesso.live.application-preflight.relational-definition.xtdb-fixture :refer [with-real-xtdb-root-step]]))

(deftest v880-admitted-model-path-and-matching-captured-adjacency-are-separately-reported
  (with-v880-adjacency
    (fn [{:keys [handle a b calls pair-calls]}]
      (let [r (sut/compare-observation-sequence-with-captured-adjacency!
               handle [a b])
            captured (:captured-dependency-adjacency r)
            joined (:joint-path-and-captured-adjacency r)]
        (is (= :caller-ordered-choice-outcome-path-explored (:status r)))
        (is (= :all-captured-adjacencies-match (:status captured)))
        (is (= :modeled-path-and-captured-adjacencies-match (:status joined)))
        (is (= 2 (:observation-count r)))
        (is (= 1 (:pair-count captured)))
        (is (= [{:earlier-step-index 0 :later-step-index 1
                 :status :captured-domain-documents-match
                 :dependency-key-set-comparison :same-captured-keys}]
               (:pairs captured)))
        (is (= [a b] @calls))
        (is (= 1 (count @pair-calls)))
        (is (identical? a (:earlier (first @pair-calls))))
        (is (identical? b (:later (first @pair-calls))))
        (is (= :not-established (:chronology captured)))
        (is (= :not-established (:complete-dependency-coverage captured)))
        (is (= :not-established (:observed-execution-chronology joined)))
        (is (= :not-established (:absence-of-intervening-writes joined)))
        (is (= :not-established (:production-trace-refinement joined)))
        (is (= :not-established (:application-certification joined)))))))

(deftest v880-different-captured-values-do-not-change-modeled-path-verdict
  (with-v880-adjacency
    (fn [{:keys [handle a b pair-response]}]
      (reset! pair-response {:status :captured-domain-documents-differ
                             :dependency-key-set-comparison :same-captured-keys})
      (let [r (sut/compare-observation-sequence-with-captured-adjacency! handle [a b])]
        (is (= :caller-ordered-choice-outcome-path-explored (:status r)))
        (is (seq (:matching-final-modeled-node-ids r)))
        (is (= :one-or-more-captured-adjacencies-differ
               (get-in r [:captured-dependency-adjacency :status])))
        (is (= :modeled-path-without-captured-adjacency-agreement
               (get-in r [:joint-path-and-captured-adjacency :status])))))))

(deftest v880-incompatible-dependency-domains-are-never-equal
  (with-v880-adjacency
    (fn [{:keys [handle a b pair-response]}]
      (reset! pair-response {:status :captured-dependency-key-sets-differ
                             :dependency-key-set-comparison :different-captured-keys})
      (let [r (sut/compare-observation-sequence-with-captured-adjacency! handle [a b])]
        (is (= :captured-adjacency-equality-not-established
               (get-in r [:captured-dependency-adjacency :status])))
        (is (= :different-captured-keys
               (get-in r [:captured-dependency-adjacency :pairs 0
                          :dependency-key-set-comparison])))
        (is (= :modeled-path-without-captured-adjacency-agreement
               (get-in r [:joint-path-and-captured-adjacency :status])))))))

(deftest v880-empty-captured-domains-remain-inconclusive
  (with-v880-adjacency
    (fn [{:keys [handle a b pair-response]}]
      (reset! pair-response {:status :no-common-nonempty-captured-domain
                             :dependency-key-set-comparison :same-captured-keys})
      (let [r (sut/compare-observation-sequence-with-captured-adjacency! handle [a b])]
        (is (= :captured-adjacency-equality-not-established
               (get-in r [:captured-dependency-adjacency :status])))
        (is (= :modeled-path-without-captured-adjacency-agreement
               (get-in r [:joint-path-and-captured-adjacency :status])))))))

(deftest v880-captured-equality-cannot-rescue-unexplored-model-sequence
  (with-v880-adjacency
    (fn [options]
      (-> (definition options)
          (assoc-in [:relational-model :families 0 :enabled?]
                    (fn [state _ _] (zero? (:n (get state counter-key)))))
          (assoc-in [:relational-model :families 1 :enabled?]
                    (fn [state _ _] (pos? (:n (get state counter-key)))))
          (assoc-in [:relational-model :families 2 :enabled?]
                    (fn [& _] false))))
    (fn [{:keys [handle a b pair-calls]}]
      (let [r (sut/compare-observation-sequence-with-captured-adjacency! handle [b a])]
        (is (= :caller-ordered-choice-outcome-path-not-explored (:status r)))
        (is (= :all-captured-adjacencies-match
               (get-in r [:captured-dependency-adjacency :status])))
        (is (= :modeled-path-not-established
               (get-in r [:joint-path-and-captured-adjacency :status])))
        (is (= 1 (count @pair-calls)))))))

(deftest v880-plan-counterexample-stays-a-counterexample-even-with-adjacency
  (with-v880-adjacency
    (fn [{:keys [handle a b second-edge responses pair-calls]}]
      (swap! responses assoc b (assoc (comparison-for-edge second-edge)
                                      :status :counterexample))
      (let [r (sut/compare-observation-sequence-with-captured-adjacency!
               handle [a b])]
        (is (= :one-or-more-submitted-plans-not-matched (:status r)))
        (is (= :all-captured-adjacencies-match
               (get-in r [:captured-dependency-adjacency :status])))
        (is (= :modeled-path-not-established
               (get-in r [:joint-path-and-captured-adjacency :status])))
        (is (= 1 (count @pair-calls)))))))

(deftest v880-single-observation-never-produces-vacuous-adjacency-success
  (with-v880-adjacency
    (fn [{:keys [handle a pair-calls]}]
      (let [r (sut/compare-observation-sequence-with-captured-adjacency!
               handle [a])]
        (is (= :caller-ordered-choice-outcome-path-explored (:status r)))
        (is (= :no-adjacent-observations
               (get-in r [:captured-dependency-adjacency :status])))
        (is (= 0 (get-in r [:captured-dependency-adjacency :pair-count])))
        (is (= [] (get-in r [:captured-dependency-adjacency :pairs])))
        (is (= :no-adjacent-observations
               (get-in r [:joint-path-and-captured-adjacency :status])))
        (is (empty? @pair-calls))))))

(deftest v880-invalid-or-repeated-handle-rejected-before-pair-inspection
  (with-v880-adjacency
    (fn [{:keys [handle a pair-calls calls]}]
      (doseq [bad [nil [] [nil] [a a] [a nil] (list a)]]
        (is (= :invalid-observation-sequence
               (error-kind #(sut/compare-observation-sequence-with-captured-adjacency!
                              handle bad)))))
      (is (empty? @pair-calls))
      (is (empty? @calls)))))

(deftest v880-adjacency-diagnostic-is-redacted-inert-and-not-an-authority-handle
  (with-v880-adjacency
    (fn [{:keys [handle a b pair-calls]}]
      (let [r (sut/compare-observation-sequence-with-captured-adjacency!
               handle [a b])
            text (pr-str r)
            bogus (assoc-in r [:joint-path-and-captured-adjacency :status]
                            :proved)]
        (is (false? (get-in r [:joint-path-and-captured-adjacency
                               :state-contents-exposed?])))
        (is (not (re-find #"#object\[|:initial-state|:submitted-incorporation|:commands|:arguments|:principals" text)))
        (is (= :unknown-construction (error-kind #(sut/explain bogus))))
        (is (= :unknown-construction (error-kind #(sut/compare! r a))))
        (is (= 1 (count @pair-calls)))))))

(deftest v880-stale-artifact-blocks-comparison-before-adjacency-inspection
  (with-v880-adjacency
    (fn [{:keys [handle a b artifact-path pair-calls calls]}]
      (spit artifact-path "v880-stale-runtime\n")
      (is (= :stale-application-assembly
             (error-kind #(sut/compare-observation-sequence-with-captured-adjacency!
                            handle [a b]))))
      (is (empty? @pair-calls))
      (is (empty? @calls)))))

(deftest v880-genuine-xtdb-executor-observation-has-no-imaginary-second-step
  (with-real-xtdb-root-step
    (fn [{:keys [handle evidence request-id]}]
      (let [r (sut/compare-observation-sequence-with-captured-adjacency!
               handle [evidence])]
        (is (= :caller-ordered-choice-outcome-path-explored (:status r)))
        (is (= :committed (get-in r [:steps 0 :physical-outcome])))
        (is (= :matched (get-in r [:steps 0 :plan-comparison-status])))
        (is (= :no-adjacent-observations
               (get-in r [:captured-dependency-adjacency :status])))
        (is (= [] (get-in r [:captured-dependency-adjacency :pairs])))
        (is (= :no-adjacent-observations
               (get-in r [:joint-path-and-captured-adjacency :status])))
        (is (= :not-established
               (get-in r [:joint-path-and-captured-adjacency
                          :observed-execution-chronology])))
        (is (not (.contains (pr-str r) request-id)))))))


;; v882: the v881 root-anchor join reuses the first compare! result. These
;; classification tests stub the downstream controlled comparator and pair
;; comparator, but keep real admitted application, family and transition-tree
;; construction. The last test uses executor-issued embedded XTDB evidence.
;; A matching root anchor is a bounded diagnostic, never authenticated
;; chronology, pinned XTDB isolation, or production trace refinement.

(deftest v882-matched-first-root-step-and-adjacency-are-reported-together
  (with-v882-root-anchor
    (fn [{:keys [handle a b calls pair-calls]}]
      (let [report (sut/compare-observation-sequence-with-captured-adjacency!
                    handle [a b])
            anchor (v882-anchor report)]
        (is (= :initial-root-step-and-captured-adjacencies-match (:status anchor)))
        (is (= :matching-captured-root-step-before-and-after
               (:first-observation-local-root-step-status anchor)))
        (is (= :matching-captured-root-step-before-and-after
               (get-in report [:first-observation-local-root-step :status])))
        (is (= :caller-ordered-choice-outcome-path-explored (:status report)))
        (is (= :modeled-path-and-captured-adjacencies-match
               (get-in report [:joint-path-and-captured-adjacency :status])))
        (is (= :all-captured-adjacencies-match
               (get-in report [:captured-dependency-adjacency :status])))
        (is (= [a b] @calls))
        (is (= 1 (count @pair-calls)))
        (is (= :already-compared-first-executor-bound-observation (:source anchor)))
        (is (false? (:additional-model-callbacks? anchor)))
        (is (false? (:additional-xtdb-reads? anchor)))))))

(deftest v882-missing-first-poststate-cannot-establish-a-root-anchor
  (with-v882-root-anchor
    (fn [{:keys [handle a b responses]}]
      (swap! responses update a dissoc :declared-root-poststate-comparison)
      (let [report (sut/compare-observation-sequence-with-captured-adjacency!
                    handle [a b])]
        (is (= :initial-controlled-root-step-not-established
               (:status (v882-anchor report))))
        (is (= :captured-poststate-not-established
               (get-in report [:first-observation-local-root-step :status])))
        (is (= :modeled-path-and-captured-adjacencies-match
               (get-in report [:joint-path-and-captured-adjacency :status])))))))

(deftest v882-mismatched-initial-state-is-not-rehabilitated-by-adjacency
  (with-v882-root-anchor
    (fn [{:keys [handle a b responses]}]
      (swap! responses assoc-in [a :declared-root-state-comparison :status]
             :differs-from-captured-pretransaction-state)
      (let [report (sut/compare-observation-sequence-with-captured-adjacency!
                    handle [a b])]
        (is (= :initial-controlled-root-step-not-established
               (:status (v882-anchor report))))
        (is (= :all-captured-adjacencies-match
               (get-in report [:captured-dependency-adjacency :status])))
        (is (= :caller-ordered-choice-outcome-path-explored (:status report)))))))

(deftest v882-mismatched-first-poststate-is-not-a-successful-root-anchor
  (with-v882-root-anchor
    (fn [{:keys [handle a b responses]}]
      (swap! responses assoc-in [a :declared-root-poststate-comparison :status]
             :differs-from-captured-posttransaction-dependencies)
      (let [report (sut/compare-observation-sequence-with-captured-adjacency!
                    handle [a b])]
        (is (= :initial-controlled-root-step-not-established
               (:status (v882-anchor report))))
        (is (= :captured-poststate-differs
               (get-in report [:first-observation-local-root-step :status])))
        (is (= :all-captured-adjacencies-match
               (get-in report [:captured-dependency-adjacency :status])))))))

(deftest v882-unmatched-modeled-path-precedes-any-root-anchor-claim
  (with-v882-root-anchor
    (fn [{:keys [handle a b responses]}]
      (swap! responses assoc-in [b :status] :counterexample)
      (let [report (sut/compare-observation-sequence-with-captured-adjacency!
                    handle [a b])]
        (is (= :one-or-more-submitted-plans-not-matched (:status report)))
        (is (= :modeled-path-not-established
               (:status (v882-anchor report))))
        (is (= :matching-captured-root-step-before-and-after
               (get-in report [:first-observation-local-root-step :status])))
        (is (= :all-captured-adjacencies-match
               (get-in report [:captured-dependency-adjacency :status])))))))

(deftest v882-captured-state-disagreement-remains-distinct-from-root-agreement
  (with-v882-root-anchor
    (fn [{:keys [handle a b pair-response]}]
      (reset! pair-response {:status :captured-domain-documents-differ
                             :dependency-key-set-comparison :same-captured-keys})
      (let [report (sut/compare-observation-sequence-with-captured-adjacency!
                    handle [a b])]
        (is (= :matching-captured-root-step-before-and-after
               (get-in report [:first-observation-local-root-step :status])))
        (is (= :captured-adjacency-equality-not-established
               (:status (v882-anchor report))))
        (is (= :one-or-more-captured-adjacencies-differ
               (get-in report [:captured-dependency-adjacency :status])))
        (is (= :modeled-path-without-captured-adjacency-agreement
               (get-in report [:joint-path-and-captured-adjacency :status])))))))

(deftest v882-different-or-empty-dependency-domains-cannot-anchor-a-history
  (with-v882-root-anchor
    (fn [{:keys [handle a b pair-response]}]
      (doseq [pair [{:status :captured-dependency-key-sets-differ
                     :dependency-key-set-comparison :different-captured-keys}
                    {:status :no-common-nonempty-captured-domain
                     :dependency-key-set-comparison :same-captured-keys}]]
        (reset! pair-response pair)
        (let [report (sut/compare-observation-sequence-with-captured-adjacency!
                      handle [a b])]
          (is (= :captured-adjacency-equality-not-established
                 (:status (v882-anchor report))))
          (is (= (:status pair)
                 (get-in report [:captured-dependency-adjacency :pairs 0 :status])))
          (is (= :caller-ordered-choice-outcome-path-explored
                 (:status report))))))))

(deftest v882-one-real-rooted-observation-has-no-invented-adjacency
  (with-real-xtdb-root-step
    (fn [{:keys [handle evidence request-id]}]
      (let [report (sut/compare-observation-sequence-with-captured-adjacency!
                    handle [evidence])
            anchor (v882-anchor report)]
        (is (= :matching-captured-root-step-before-and-after
               (:first-observation-local-root-step-status anchor)))
        (is (= :no-adjacent-observations (:status anchor)))
        (is (= :caller-ordered-choice-outcome-path-explored (:status report)))
        (is (= :no-adjacent-observations
               (get-in report [:captured-dependency-adjacency :status])))
        (is (= :not-established (:observed-execution-chronology anchor)))
        (is (not (.contains (pr-str report) request-id)))))))

(deftest v882-no-reported-outcome-does-not-fabricate-a-rooted-transition
  (with-v882-root-anchor
    (fn [{:keys [handle a b responses]}]
      (swap! responses assoc-in [a :physical-status] nil)
      (let [report (sut/compare-observation-sequence-with-captured-adjacency!
                    handle [a b])]
        (is (= :one-or-more-physical-outcomes-unavailable (:status report)))
        (is (= :modeled-path-not-established (:status (v882-anchor report))))
        (is (= :root-step-not-established
               (get-in report [:first-observation-local-root-step :status])))))))

(deftest v882-root-anchor-report-is-redacted-inert-and-does-not-prove-history
  (with-v882-root-anchor
    (fn [{:keys [handle a b calls pair-calls]}]
      (let [report (sut/compare-observation-sequence-with-captured-adjacency!
                    handle [a b])
            anchor (v882-anchor report)
            visible (pr-str anchor)]
        (is (= :bounded-root-step-plus-captured-adjacency-diagnostic
               (:evidence-class anchor)))
        (is (false? (:state-contents-exposed? anchor)))
        (is (every? #(= :not-established (get anchor %))
                    [:observed-execution-chronology
                     :absence-of-intervening-writes
                     :complete-dependency-coverage
                     :pinned-xtdb-snapshot
                     :production-trace-refinement
                     :application-certification]))
        (is (not (re-find #"#object\[|:initial-state|:commands|:arguments|:principals" visible)))
        (is (= :unknown-construction
               (error-kind #(sut/explain anchor))))
        (is (= :unknown-construction
               (error-kind #(sut/compare! anchor a))))
        (is (= [a b] @calls))
        (is (= 1 (count @pair-calls)))))))

(deftest v882-stale-assembly-blocks-root-anchored-comparison-before-observation
  (with-v882-root-anchor
    (fn [{:keys [handle a b artifact-path calls pair-calls]}]
      (spit artifact-path "v882-stale-physical-artifact\n")
      (is (= :stale-application-assembly
             (error-kind #(sut/compare-observation-sequence-with-captured-adjacency!
                            handle [a b]))))
      (is (empty? @calls))
      (is (empty? @pair-calls)))))


;; v886: v885 joins individual executor-bound captured-local-step outcomes
;; with a rooted finite-model path and captured dependency adjacency.  These
;; are separate bounded diagnostics; none authenticates execution chronology
;; or identifies non-root concrete states with nodes of the finite model.

(deftest v886-two-matching-local-steps-and-adjacency-form-scoped-diagnostic
  (with-v880-adjacency
    (fn [{:keys [handle a b responses calls pair-calls]}]
      (v886-set-local! responses a v886-matching-captured-local-step)
      (v886-set-local! responses b v886-matching-captured-local-step)
      (let [report (sut/compare-observation-sequence-with-captured-adjacency!
                    handle [a b])
            local (v886-local-joint report)]
        (is (= :caller-ordered-choice-outcome-path-explored (:status report)))
        (is (= :modeled-path-and-captured-local-steps-and-adjacencies-match
               (:status local)))
        (is (= [:matches-captured-posttransaction-dependencies
                :matches-captured-posttransaction-dependencies]
               (:captured-local-poststate-statuses local)))
        (is (= :all-captured-adjacencies-match
               (get-in report [:captured-dependency-adjacency :status])))
        (is (= [a b] @calls))
        (is (= 1 (count @pair-calls)))
        (is (= :already-checked-individual-controlled-observations
               (:source local)))
        (is (= :caller-ordered-captured-local-steps-vs-modeled-path-and-adjacency
               (:scope local)))
        (is (false? (:additional-model-callbacks? local)))
        (is (false? (:additional-xtdb-reads? local)))
        (is (false? (:state-contents-exposed? local)))))))

(deftest v886-legacy-comparison-without-local-step-evidence-remains-inconclusive
  (with-v880-adjacency
    (fn [{:keys [handle a b]}]
      (let [report (sut/compare-observation-sequence-with-captured-adjacency!
                    handle [a b])
            local (v886-local-joint report)]
        (is (= :caller-ordered-choice-outcome-path-explored (:status report)))
        (is (= :modeled-path-and-captured-adjacencies-match
               (get-in report [:joint-path-and-captured-adjacency :status])))
        (is (= [:not-reported :not-reported]
               (:captured-local-poststate-statuses local)))
        (is (= :one-or-more-captured-local-steps-not-established
               (:status local)))))))

(deftest v886-mismatched-or-rejected-local-poststate-cannot-claim-joint-match
  (with-v880-adjacency
    (fn [{:keys [handle a b responses]}]
      (v886-set-local! responses a v886-matching-captured-local-step)
      (doseq [failed-status [:differs-from-captured-posttransaction-dependencies
                            :modeled-and-physical-outcomes-disagree
                            :submitted-plan-not-matched
                            :physical-outcome-not-reported]]
        (v886-set-local! responses b
                         (assoc v886-matching-captured-local-step
                                :status failed-status))
        (let [report (sut/compare-observation-sequence-with-captured-adjacency!
                      handle [a b])
              local (v886-local-joint report)]
          (is (= :one-or-more-captured-local-steps-not-established
                 (:status local)))
          (is (= [:matches-captured-posttransaction-dependencies failed-status]
                 (:captured-local-poststate-statuses local)))
          (is (= :all-captured-adjacencies-match
                 (get-in report [:captured-dependency-adjacency :status]))))))))

(deftest v886-an-unexplored-model-path-stays-unexplored-despite-local-matches
  (with-v880-adjacency
    (fn [{:keys [handle a b responses]}]
      (v886-set-local! responses a v886-matching-captured-local-step)
      (v886-set-local! responses b
                       (assoc v886-matching-captured-local-step
                              :status :submitted-plan-not-matched))
      (swap! responses assoc-in [b :status] :counterexample)
      (let [report (sut/compare-observation-sequence-with-captured-adjacency!
                    handle [a b])]
        (is (= :one-or-more-submitted-plans-not-matched (:status report)))
        (is (= :modeled-path-not-established
               (:status (v886-local-joint report))))
        (is (= :all-captured-adjacencies-match
               (get-in report [:captured-dependency-adjacency :status])))))))

(deftest v886-agreeing-local-steps-do-not-override-captured-state-disagreement
  (with-v880-adjacency
    (fn [{:keys [handle a b responses pair-response]}]
      (v886-set-local! responses a v886-matching-captured-local-step)
      (v886-set-local! responses b v886-matching-captured-local-step)
      (reset! pair-response {:status :captured-domain-documents-differ
                             :dependency-key-set-comparison :same-captured-keys})
      (let [report (sut/compare-observation-sequence-with-captured-adjacency!
                    handle [a b])]
        (is (= :captured-adjacency-equality-not-established
               (:status (v886-local-joint report))))
        (is (= :one-or-more-captured-adjacencies-differ
               (get-in report [:captured-dependency-adjacency :status])))
        (is (= [:matches-captured-posttransaction-dependencies
                :matches-captured-posttransaction-dependencies]
               (get-in report [:captured-local-steps-and-adjacency
                               :captured-local-poststate-statuses])))))))

(deftest v886-different-or-empty-captured-dependency-domains-prevent-joint-match
  (with-v880-adjacency
    (fn [{:keys [handle a b responses pair-response]}]
      (v886-set-local! responses a v886-matching-captured-local-step)
      (v886-set-local! responses b v886-matching-captured-local-step)
      (doseq [pair [{:status :captured-dependency-key-sets-differ
                     :dependency-key-set-comparison :different-captured-keys}
                    {:status :no-common-nonempty-captured-domain
                     :dependency-key-set-comparison :same-captured-keys}]]
        (reset! pair-response pair)
        (let [report (sut/compare-observation-sequence-with-captured-adjacency!
                      handle [a b])]
          (is (= :captured-adjacency-equality-not-established
                 (:status (v886-local-joint report))))
          (is (= (:status pair)
                 (get-in report [:captured-dependency-adjacency :pairs 0
                                 :status]))))))))

(deftest v886-root-anchor-is-independent-of-subsequent-local-step-matches
  (with-v882-root-anchor
    (fn [{:keys [handle a b responses]}]
      (v886-set-local! responses a v886-matching-captured-local-step)
      (v886-set-local! responses b v886-matching-captured-local-step)
      (let [matching (sut/compare-observation-sequence-with-captured-adjacency!
                      handle [a b])]
        (is (= :initial-root-step-and-captured-adjacencies-match
               (get-in matching [:initial-root-step-and-captured-adjacency :status])))
        (is (= :modeled-path-and-captured-local-steps-and-adjacencies-match
               (:status (v886-local-joint matching)))))
      (swap! responses update a dissoc :declared-root-poststate-comparison)
      (let [without-root (sut/compare-observation-sequence-with-captured-adjacency!
                          handle [a b])]
        (is (= :initial-controlled-root-step-not-established
               (get-in without-root [:initial-root-step-and-captured-adjacency :status])))
        (is (= :modeled-path-and-captured-local-steps-and-adjacencies-match
               (:status (v886-local-joint without-root))))))))

(deftest v886-single-observation-never-invents-captured-adjacency
  (with-v880-adjacency
    (fn [{:keys [handle a responses pair-calls]}]
      (v886-set-local! responses a v886-matching-captured-local-step)
      (let [report (sut/compare-observation-sequence-with-captured-adjacency!
                    handle [a])]
        (is (= :no-adjacent-observations
               (:status (v886-local-joint report))))
        (is (= [:matches-captured-posttransaction-dependencies]
               (:captured-local-poststate-statuses (v886-local-joint report))))
        (is (= 0 (count @pair-calls)))
        (is (= 1 (:observation-count report)))))))

(deftest v886-invalid-controlled-local-step-metadata-rejects-before-pair-comparison
  (with-v880-adjacency
    (fn [{:keys [handle a b responses calls pair-calls]}]
      (doseq [invalid [(assoc v886-matching-captured-local-step :scope :untrusted)
                       (assoc v886-matching-captured-local-step :relation :hash-equality)
                       (assoc v886-matching-captured-local-step :status :unknown)
                       (assoc v886-matching-captured-local-step :poststate-data-exposed? true)
                       (assoc v886-matching-captured-local-step :additional-xtdb-reads? true)
                       (assoc v886-matching-captured-local-step :pinned-xtdb-snapshot :proved)
                       (assoc v886-matching-captured-local-step :production-trace-refinement :proved)
                       (dissoc v886-matching-captured-local-step :input-source)]]
        (v886-set-local! responses a invalid)
        (is (= :invalid-captured-local-step-evidence
               (error-kind #(sut/compare-observation-sequence-with-captured-adjacency!
                              handle [a b])))))
      (is (empty? @pair-calls))
      (is (= (* 8 2) (count @calls)))
      (v886-set-local! responses a v886-matching-captured-local-step)
      (v886-set-local! responses b v886-matching-captured-local-step)
      (is (= :modeled-path-and-captured-local-steps-and-adjacencies-match
             (:status (v886-local-joint
                       (sut/compare-observation-sequence-with-captured-adjacency!
                        handle [a b]))))))))

(deftest v886-success-requires-consistent-plan-outcome-and-local-state-evidence
  (with-v880-adjacency
    (fn [{:keys [handle a b responses pair-calls]}]
      (v886-set-local! responses a v886-matching-captured-local-step)
      (v886-set-local! responses b v886-matching-captured-local-step)
      (swap! responses assoc-in [b :status] :counterexample)
      (is (= :invalid-captured-local-step-evidence
             (error-kind #(sut/compare-observation-sequence-with-captured-adjacency!
                            handle [a b]))))
      (is (empty? @pair-calls)))))

(deftest v886-joined-report-remains-redacted-and-cannot-authorize
  (with-v880-adjacency
    (fn [{:keys [handle a b responses calls pair-calls]}]
      (v886-set-local! responses a v886-matching-captured-local-step)
      (v886-set-local! responses b v886-matching-captured-local-step)
      (let [report (sut/compare-observation-sequence-with-captured-adjacency!
                    handle [a b])
            local (v886-local-joint report)
            printed (pr-str local)]
        (is (every? #(= :not-established (get local %))
                    [:equality-with-reachable-non-root-model-nodes
                     :execution-chronology :absence-of-intervening-writes
                     :complete-dependency-coverage :pinned-xtdb-snapshot
                     :production-trace-refinement :application-certification]))
        (is (not (re-find #"#object\[|:initial-state|:commands|:arguments|:principals"
                          printed)))
        (is (= :unknown-construction (error-kind #(sut/explain local))))
        (is (= :unknown-construction (error-kind #(sut/compare! local a))))
        (is (= [a b] @calls))
        (is (= 1 (count @pair-calls)))))))

(deftest v886-stale-assembly-rejects-before-inspecting-local-or-pair-evidence
  (with-v880-adjacency
    (fn [{:keys [handle a b responses artifact-path calls pair-calls]}]
      (v886-set-local! responses a v886-matching-captured-local-step)
      (v886-set-local! responses b v886-matching-captured-local-step)
      (spit artifact-path "v886-stale-browser-runtime\n")
      (is (= :stale-application-assembly
             (error-kind #(sut/compare-observation-sequence-with-captured-adjacency!
                            handle [a b]))))
      (is (empty? @calls))
      (is (empty? @pair-calls)))))

(deftest v886-existing-path-only-sequence-api-keeps-its-previous-contract
  (with-v880-adjacency
    (fn [{:keys [handle a b responses pair-calls]}]
      (v886-set-local! responses a v886-matching-captured-local-step)
      (v886-set-local! responses b v886-matching-captured-local-step)
      (let [path (sut/compare-observation-sequence! handle [a b])]
        (is (= :caller-ordered-choice-outcome-path-explored (:status path)))
        (is (= [:matches-captured-posttransaction-dependencies
                :matches-captured-posttransaction-dependencies]
               (mapv :captured-local-poststate-status (:steps path))))
        (is (not (contains? path :captured-local-steps-and-adjacency)))
        (is (not (contains? path :captured-dependency-adjacency)))
        (is (empty? @pair-calls))))))

(deftest v886-real-executor-bound-xtdb-single-step-is-not-a-history
  (with-real-xtdb-root-step
    (fn [{:keys [handle evidence request-id]}]
      (let [report (sut/compare-observation-sequence-with-captured-adjacency!
                    handle [evidence])
            local (v886-local-joint report)]
        (is (= :caller-ordered-choice-outcome-path-explored (:status report)))
        (is (= [:matches-captured-posttransaction-dependencies]
               (:captured-local-poststate-statuses local)))
        (is (= :no-adjacent-observations (:status local)))
        (is (= :matching-captured-root-step-before-and-after
               (get-in report [:first-observation-local-root-step :status])))
        (is (= :no-adjacent-observations
               (get-in report [:captured-dependency-adjacency :status])))
        (is (= :not-established (:execution-chronology local)))
        (is (not (.contains (pr-str local) request-id)))))))


;; v892: v891 joins the construction's ORIGINAL private finite occurrence
;; witness with a REAL executor-bound, captured pretransaction state.  These
;; tests do not mistake a matching state for a witnessed execution chronology,
;; a plan match, a particular successor, or completeness of captured reads.

