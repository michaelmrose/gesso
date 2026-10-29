(ns gesso.live.application-preflight.relational-definition.root-poststate-test
  "Relational definition: root poststate.

   Checks one controlled executor-bound root transition against an admitted
   finite model root, including exact pretransaction and posttransaction
   dependency observations. A matching root and poststate are bounded
   evidence, not a proof of arbitrary physical trace refinement or
   application certification. Failure and stale-evidence cases must
   continue to withhold stronger conclusions.

   Extracted from the 171-test regression suite without changing tests or assertions."
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.live.application-preflight.fixture :as fixture]
   [gesso.live.application-preflight.relational-rule-coverage :as relations]
   [gesso.live.application-preflight.relational-definition :as sut]
   [gesso.live.application-preflight.relational-definition.support :refer [counter-key initial-state relational-model definition error-kind modeled-transition-tree with-stubbed-transition-comparison comparison-for-edge matching-root-observation root-step-for! v872-matched-poststate v872-root-poststate-for!]]
   [gesso.live.application-preflight.relational-definition.xtdb-fixture :refer [with-real-xtdb-root-step]]))

(deftest matching-root-child-requires-all-four-admitted-dimensions
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [root (:root-node-id tree)
            edge (first (filter #(= root (:parent-node-id %)) (:edges tree)))
            report (root-step-for! handle physical-comparison edge {})]
        (is (= :matching-root-step-explored (:status report)))
        (is (some #{(:node-id edge)}
                  (:matched-root-transition-node-ids report)))
        (is (every? (fn [node-id]
                      (some #(and (= node-id (:node-id %))
                                  (= root (:parent-node-id %)))
                            (:edges tree)))
                    (:matched-root-transition-node-ids report)))
        (is (= #{:admitted-operation-choice
                 :definitive-physical-outcome
                 :exact-declared-root-vs-captured-dependencies
                 :exact-observed-domain-preparation-plan}
               (:evidence-dimensions report)))
        (is (= :single-controlled-execution-versus-declared-finite-model-root
               (:scope report)))
        (is (= :not-established
               (:modeled-poststate-equals-observed-poststate? report)))
        (is (= :not-established (:physical-trace-refinement report)))
        (is (contains? (:not-established report)
                       :posttransaction-model-state-equality))))))

(deftest wrong-declared-root-never-qualifies-as-root-step
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            report (root-step-for!
                    handle physical-comparison edge
                    {:declared-root-state-comparison
                     (assoc matching-root-observation
                            :status :differs-from-captured-pretransaction-state)})]
        (is (= :declared-root-not-established-equal-to-observation
               (:status report)))
        (is (= [] (:matched-root-transition-node-ids report)))
        (is (= #{} (:evidence-dimensions report)))))))

(deftest incomplete-or-wrongly-scoped-root-evidence-never-qualifies
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))]
        (doseq [root [nil {}
                      (dissoc matching-root-observation :relation)
                      (assoc matching-root-observation :scope :unbound-root)
                      (assoc matching-root-observation :relation :hash-equality)]]
          (let [report (root-step-for!
                        handle physical-comparison edge
                        {:declared-root-state-comparison root})]
            (is (= :declared-root-not-established-equal-to-observation
                   (:status report)))
            (is (empty? (:matched-root-transition-node-ids report)))
            (is (empty? (:evidence-dimensions report)))))))))

(deftest mismatched-submitted-plan-remains-a-counterexample
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            comparator (merge (comparison-for-edge edge)
                              {:status :counterexample
                               :reason :modeled-and-submitted-plans-disagree
                               :declared-root-state-comparison matching-root-observation})]
        (reset! physical-comparison comparator)
        (let [actual (sut/compare! handle (Object.))
              report (get-in actual
                             [:modeled-transition-context :root-step-correspondence])]
          (is (= :counterexample (:status actual)))
          (is (= :modeled-and-submitted-plans-disagree (:reason actual)))
          (is (= comparator (dissoc actual :modeled-transition-context)))
          (is (= :submitted-plan-not-matched (:status report)))
          (is (empty? (:matched-root-transition-node-ids report))))))))

(deftest missing-physical-outcome-cannot-produce-a-root-step
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            report (root-step-for! handle physical-comparison edge
                                   {:physical-status nil})]
        (is (= :physical-outcome-not-reported (:status report)))
        (is (= [] (:matched-root-transition-node-ids report)))
        (is (= #{} (:evidence-dimensions report)))))))

(deftest matching-deeper-transition-does-not-imply-matching-root-step
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
          (let [report (root-step-for! handle physical-comparison edge {})]
            (is (= :same-choice-outcome-not-explored-at-root
                   (:status report)))
            (is (empty? (:matched-root-transition-node-ids report)))
            (is (empty? (:evidence-dimensions report)))
            (is (= :not-established (:physical-trace-refinement report)))))))))

(deftest unexplored-outcome-preserves-matched-plan-verdict
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            opposite (if (= :committed (:status edge)) :rejected :committed)
            report (root-step-for! handle physical-comparison edge
                                   {:physical-status opposite})]
        (is (= :same-choice-outcome-not-explored-at-root (:status report)))
        (is (= [] (:matched-root-transition-node-ids report)))
        (is (= #{} (:evidence-dimensions report)))))))

(deftest root-step-report-is-inert-redacted-and-currentness-bound
  (fixture/with-closed-application
    (fn [{:keys [options artifact-path]}]
      (let [comparison (atom nil)
            comparator-calls (atom 0)]
        (with-redefs [relations/build-comparator
                      (fn [& _]
                        (fn [_]
                          (swap! comparator-calls inc)
                          @comparison))]
          (let [model (-> (definition options)
                          (assoc-in [:relational-model :initial-state counter-key :secret]
                                    "v868-secret-state")
                          (assoc-in [:relational-model :families 0 :arguments]
                                    [{:delta 1 :secret "v868-secret-input"}]))
                handle (sut/construct! model)
                edge (first (:edges (modeled-transition-tree handle)))
                _ (reset! comparison
                          (assoc (comparison-for-edge edge)
                                 :declared-root-state-comparison
                                 matching-root-observation))
                one (sut/compare! handle (Object.))
                two (sut/compare! handle (Object.))
                report (get-in one [:modeled-transition-context
                                    :root-step-correspondence])]
            (is (= report
                   (get-in two [:modeled-transition-context
                                :root-step-correspondence])))
            (is (= 2 @comparator-calls))
            (is (not (re-find #"v868-secret-state|v868-secret-input|#object\["
                              (pr-str report))))
            (is (= :unknown-construction (error-kind #(sut/explain report))))
            (is (= :unknown-construction
                   (error-kind #(sut/compare! report (Object.)))))
            (spit artifact-path "v868-invalid-runtime-artifact\n")
            (is (= :stale-application-assembly
                   (error-kind #(sut/compare! handle (Object.)))))
            (is (= 2 @comparator-calls))))))))

(deftest genuine-xtdb-execution-can-correspond-to-admitted-model-root-step
  (with-real-xtdb-root-step
    (fn [{:keys [handle evidence request-id]}]
      (let [result (sut/compare! handle evidence)
            context (:modeled-transition-context result)
            root-step (:root-step-correspondence context)
            tree (modeled-transition-tree handle)
            root (:root-node-id tree)
            children (->> (:edges tree)
                          (filterv #(and (= root (:parent-node-id %))
                                         (= :request/claim
                                            (:registered-operation-id %))
                                         (= :committed (:status %))))
                          (mapv :node-id))]
        (is (= :matched (:status result)))
        (is (= :committed (:physical-status result)))
        (is (= :matches-captured-pretransaction-state
               (get-in result [:declared-root-state-comparison :status])))
        (is (= :matching-root-step-explored (:status root-step)))
        (is (= children (:matched-root-transition-node-ids root-step)))
        (is (seq children))
        (is (= :not-established
               (:modeled-poststate-equals-observed-poststate? root-step)))
        (is (= :not-established (:physical-trace-refinement root-step)))
        (is (not (re-find (re-pattern (java.util.regex.Pattern/quote request-id))
                          (pr-str root-step))))))))

;; v872: v871 joins the executor-bound root-poststate result to the admitted
;; finite ROOT child. The nested result is diagnostic, not an alternative
;; comparator verdict or proof of pinned snapshots / trace refinement. Stub
;; only the downstream comparator for branch tests; verify the complete
;; executor-bound path separately against actual embedded XTDB.

(deftest v872-real-xtdb-poststate-completes-admitted-root-step
  (with-real-xtdb-root-step
    (fn [{:keys [handle evidence request-id]}]
      (let [comparison (sut/compare! handle evidence)
            root-step (get-in comparison
                              [:modeled-transition-context :root-step-correspondence])
            post (:captured-posttransaction-dependency-comparison root-step)
            source (:declared-root-poststate-comparison comparison)]
        (is (= :matched (:status comparison)))
        (is (= :committed (:physical-status comparison)))
        (is (= :matching-root-step-explored (:status root-step)))
        (is (seq (:matched-root-transition-node-ids root-step)))
        (is (= :matches-captured-posttransaction-dependencies (:status source)))
        (is (= (:status source) (:status post)))
        (is (= :one-controlled-root-step-vs-captured-domain-dependencies
               (:scope post)))
        (is (= :exact-clojure-domain-document-map-equality (:relation post)))
        (is (true? (:observed-dependencies-only? post)))
        (is (false? (:domain-state-exposed? post)))
        (is (= :not-established (:pinned-xtdb-snapshot post)))
        (is (= :not-established (:physical-trace-refinement post)))
        (is (= :not-established (:modeled-poststate-equals-observed-poststate?
                                 root-step)))
        (is (= :not-established (:physical-trace-refinement root-step)))
        (is (not (.contains (pr-str post) request-id)))
        (is (not (re-find #":initial-state|:commands|:guards|#object\["
                          (pr-str post))))))))

(deftest v872-absent-poststate-is-unknown-not-equality
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            post (v872-root-poststate-for! handle physical-comparison edge {})]
        (is (= :captured-poststate-not-reported (:status post)))
        (is (= :one-controlled-root-step-vs-captured-domain-dependencies
               (:scope post)))
        (is (= :not-established (:physical-trace-refinement post)))
        (is (false? (:domain-state-exposed? post)))))))

(deftest v872-present-matching-poststate-preserves-comparison-verdict
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison comparator-calls]}]
      (let [edge (first (:edges tree))
            source (merge (comparison-for-edge edge)
                          {:declared-root-state-comparison matching-root-observation
                           :declared-root-poststate-comparison v872-matched-poststate})]
        (reset! physical-comparison source)
        (let [result (sut/compare! handle (Object.))
              root-step (get-in result
                                [:modeled-transition-context :root-step-correspondence])
              post (:captured-posttransaction-dependency-comparison root-step)]
          (is (= source (dissoc result :modeled-transition-context)))
          (is (= :matching-root-step-explored (:status root-step)))
          (is (= :matches-captured-posttransaction-dependencies (:status post)))
          (is (= :exact-clojure-domain-document-map-equality (:relation post)))
          (is (= 1 @comparator-calls))
          (is (not (contains? post :documents)))
          (is (not (contains? root-step :posttransaction-documents))))))))

(deftest v872-differing-poststate-cannot-be-promoted-by-matching-root
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            post (v872-root-poststate-for!
                  handle physical-comparison edge
                  {:declared-root-poststate-comparison
                   (assoc v872-matched-poststate
                          :status :differs-from-captured-posttransaction-dependencies)})]
        (is (= :differs-from-captured-posttransaction-dependencies (:status post)))
        (is (= :not-established (:pinned-xtdb-snapshot post)))
        (is (false? (:domain-state-exposed? post)))))))

(deftest v872-disagreeing-outcomes-are-preserved-not-interpreted-as-equality
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            post (v872-root-poststate-for!
                  handle physical-comparison edge
                  {:declared-root-poststate-comparison
                   (assoc v872-matched-poststate
                          :status :modeled-and-physical-outcomes-disagree)})]
        (is (= :modeled-and-physical-outcomes-disagree (:status post)))
        (is (= :exact-clojure-domain-document-map-equality (:relation post)))
        (is (= :not-established (:physical-trace-refinement post)))))))

(deftest v872-root-mismatch-withholds-poststate-even-if-comparator-says-match
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            root-step (root-step-for!
                       handle physical-comparison edge
                       {:declared-root-state-comparison
                        (assoc matching-root-observation
                               :status :differs-from-captured-pretransaction-state)
                        :declared-root-poststate-comparison v872-matched-poststate})]
        (is (= :declared-root-not-established-equal-to-observation
               (:status root-step)))
        (is (= :modeled-root-step-not-established
               (get-in root-step
                       [:captured-posttransaction-dependency-comparison :status])))
        (is (empty? (:matched-root-transition-node-ids root-step)))))))

(deftest v872-counterexample-plan-cannot-be-rehabilitated-by-poststate
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            source (merge (comparison-for-edge edge)
                          {:status :counterexample
                           :reason :modeled-and-submitted-plans-disagree
                           :declared-root-state-comparison matching-root-observation
                           :declared-root-poststate-comparison v872-matched-poststate})]
        (reset! physical-comparison source)
        (let [result (sut/compare! handle (Object.))
              root-step (get-in result
                                [:modeled-transition-context :root-step-correspondence])]
          (is (= :counterexample (:status result)))
          (is (= :modeled-and-submitted-plans-disagree (:reason result)))
          (is (= source (dissoc result :modeled-transition-context)))
          (is (= :submitted-plan-not-matched (:status root-step)))
          (is (= :modeled-root-step-not-established
                 (get-in root-step
                         [:captured-posttransaction-dependency-comparison :status]))))))))

(deftest v872-no-definitive-physical-outcome-withholds-poststate
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            root-step (root-step-for!
                       handle physical-comparison edge
                       {:physical-status nil
                        :declared-root-poststate-comparison v872-matched-poststate})]
        (is (= :physical-outcome-not-reported (:status root-step)))
        (is (= :modeled-root-step-not-established
               (get-in root-step
                       [:captured-posttransaction-dependency-comparison :status])))))))

(deftest v872-deeper-only-transition-does-not-establish-root-poststate
  (with-stubbed-transition-comparison
    (fn [options]
      (assoc-in (definition options) [:relational-model :families 0 :enabled?]
                (fn [state _ _]
                  (pos? (:n (get state counter-key))))))
    (fn [{:keys [handle tree physical-comparison]}]
      (let [root (:root-node-id tree)
            edge (first (filter #(and (= :request/claim
                                        (:registered-operation-id %))
                                      (not= root (:parent-node-id %)))
                                (:edges tree)))]
        (is (some? edge))
        (when edge
          (let [post (v872-root-poststate-for!
                      handle physical-comparison edge
                      {:declared-root-poststate-comparison v872-matched-poststate})]
            (is (= :modeled-root-step-not-established (:status post)))
            (is (false? (:domain-state-exposed? post)))))))))

(deftest v872-invalid-controlled-poststate-metadata-fails-closed
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison comparator-calls]}]
      (let [edge (first (:edges tree))
            original (merge (comparison-for-edge edge)
                            {:declared-root-state-comparison matching-root-observation})
            invalid [(assoc v872-matched-poststate :scope :untrusted)
                     (assoc v872-matched-poststate :relation :hash-equality)
                     (assoc v872-matched-poststate :status :unrecognized)
                     (assoc v872-matched-poststate :poststate-data-exposed? true)
                     (assoc v872-matched-poststate :pinned-xtdb-snapshot :proven)
                     (assoc v872-matched-poststate :production-trace-refinement :proved)
                     (dissoc v872-matched-poststate :poststate-data-exposed?)
                     (dissoc v872-matched-poststate :scope)]]
        (doseq [post invalid]
          (reset! physical-comparison
                  (assoc original :declared-root-poststate-comparison post))
          (is (= :invalid-controlled-root-poststate-evidence
                 (error-kind #(sut/compare! handle (Object.))))))
        (is (= (count invalid) @comparator-calls))
        (reset! physical-comparison
                (assoc original :declared-root-poststate-comparison v872-matched-poststate))
        (is (= :matches-captured-posttransaction-dependencies
               (:status (v872-root-poststate-for!
                         handle physical-comparison edge
                         {:declared-root-poststate-comparison
                          v872-matched-poststate}))))))))

(deftest v872-poststate-report-cannot-authorize-or-survive-stale-assembly
  (fixture/with-closed-application
    (fn [{:keys [options artifact-path]}]
      (let [comparison (atom nil)
            calls (atom 0)]
        (with-redefs [relations/build-comparator
                      (fn [& _]
                        (fn [_]
                          (swap! calls inc)
                          @comparison))]
          (let [handle (sut/construct! (definition options))
                tree (modeled-transition-tree handle)
                edge (first (:edges tree))]
            (reset! comparison
                    (merge (comparison-for-edge edge)
                           {:declared-root-state-comparison matching-root-observation
                            :declared-root-poststate-comparison v872-matched-poststate}))
            (let [one (sut/compare! handle (Object.))
                  post (get-in one
                               [:modeled-transition-context
                                :root-step-correspondence
                                :captured-posttransaction-dependency-comparison])]
              (is (= :matches-captured-posttransaction-dependencies
                     (:status post)))
              (is (not (map? handle)))
              (is (= :unknown-construction (error-kind #(sut/explain post))))
              (is (= :unknown-construction
                     (error-kind #(sut/compare! post (Object.)))))
              (is (= 1 @calls))
              (spit artifact-path "v872-invalidated-artifact\n")
              (is (= :stale-application-assembly
                     (error-kind #(sut/compare! handle (Object.)))))
              (is (= :stale-application-assembly
                     (error-kind #(sut/explain handle))))
              (is (= 1 @calls)))))))))

