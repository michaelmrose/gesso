(ns gesso.live.application-preflight.semantic-audit-integration-test
  "Public-boundary regression tests for the source-derived semantic audit.

   These tests exercise the public application explanation path using real
   operation-acquisition assemblies and physical browser artifact receipts.
   The audit remains a report-derived description, not a certificate of
   arbitrary host-language policy or deployment enforcement."
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.set :as set]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.fixture
    :refer [error-kind with-closed-application]]
   [gesso.live.application-preflight.semantic-audit :as semantic-audit]))

(def ^:private model-premise
  :public-model-operation-contracts-hold)

(def ^:private operation-fields
  [:choreography-name
   :browser-role
   :authority-role
   :browser-plan-key
   :browser-plan-digest
   :authority-plan-digest
   :trusted-operation-name
   :command-transport
   :routes
   :execution-capabilities
   :settlement-contract
   :published-change-topics
   :affected-scopes
   :affected-fragments
   :authoritative-acquisitions
   :rendered-affordances])

(deftest current-assembly-and-report-expose-one-derived-audit
  (with-closed-application
    (fn [{:keys [assembly report]}]
      (let [from-assembly (application/explain assembly)
            from-report (application/explain report)
            audit (:semantic-audit from-assembly)]
        (is (true? (:valid? report)))
        (is (map? audit))
        (is (= audit (:semantic-audit from-report)))
        (is (= audit (semantic-audit/audit-from-current-report report)))
        (is (= :freshly-checked-application-preflight-report
               (get-in audit [:inspection-scope :source])))
        (is (not (contains? (:analysis report) :semantic-audit)))
        (is (not (contains? (:analysis from-report) :semantic-audit)))
        (is (not (contains? assembly :semantic-audit)))))))

(deftest operation-cards-come-from-the-admitted-report
  (with-closed-application
    (fn [{:keys [assembly report]}]
      (let [summaries (get-in report [:analysis :operations])
            audit (:semantic-audit (application/explain assembly))
            cards (:operation-cards audit)]
        (is (= 3 (:operation-count audit)))
        (is (= (set (keys summaries)) (set (keys cards))))
        (doseq [[operation summary] summaries]
          (let [card (get cards operation)]
            (testing (str "operation " operation)
              (is (= operation (:operation card)))
              (is (= [:analysis :operations operation]
                     (:derived-from card)))
              (is (= (select-keys summary operation-fields)
                     (:contract card)))
              (is (= :opaque-host-implementation
                     (:model-policy-inspection card)))
              (is (= model-premise (:model-policy-premise card))))))))))

(deftest model-conformance-is-explicitly-trusted-per-operation
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [audit (:semantic-audit (application/explain assembly))
            cards (:operation-cards audit)
            boundaries (:opaque-model-boundaries audit)]
        (is (= (count cards) (count boundaries)))
        (is (= (set (keys cards)) (set (map :operation boundaries))))
        (doseq [{:keys [operation subject premise class why-opaque
                        source-of-evidence discharged? trusted-operation-name]}
                boundaries]
          (testing (str "opaque conformance of " operation)
            (is (= [:operation operation :model-implementation-conformance]
                   subject))
            (is (= model-premise premise))
            (is (= :trusted class))
            (is (false? discharged?))
            (is (= :host-model-predicates-and-transaction-guards-not-in-operation-summary
                   why-opaque))
            (is (= :trusted-application-model-contract source-of-evidence))
            (is (= (get-in cards [operation :contract :trusted-operation-name])
                   trusted-operation-name))))))))

(deftest coverage-does-not-conflate-assembly-with-model-proofs
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [audit (:semantic-audit (application/explain assembly))
            coverage (:assurance-coverage audit)
            operations (keys (:operation-cards audit))]
        (is (= (* 2 (count operations)) (count coverage)))
        (is (= #{:assembly-verified :trusted} (set (map :class coverage))))
        (doseq [operation operations]
          (let [entries (filter #(= operation (second (:subject %))) coverage)
                by-class (into {} (map (juxt :class identity)) entries)]
            (is (= 2 (count entries)))
            (is (= [:operation operation :runtime-backbone-closure]
                   (get-in by-class [:assembly-verified :subject])))
            (is (= :current-application-preflight-report
                   (get-in by-class [:assembly-verified :evidence])))
            (is (= :declared-operation-route-execution-settlement-acquisition-edges
                   (get-in by-class [:assembly-verified :scope])))
            (is (= [:operation operation :model-implementation-conformance]
                   (get-in by-class [:trusted :subject])))
            (is (= model-premise (get-in by-class [:trusted :premise])))
            (is (= :opaque-authoritative-model-code
                   (get-in by-class [:trusted :scope])))))))))

(deftest unknown-policy-inventory-must-not-be-reported-as-zero
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [audit (:semantic-audit (application/explain assembly))
            scope (:inspection-scope audit)
            catalog (:business-rule-catalog audit)]
        (is (= :not-derivable-from-current-operation-summary
               (:status catalog)))
        (is (= :unknown (:count catalog)))
        (is (= [] (:rules catalog)))
        (is (false? (:complete-business-policy-inventory? scope)))
        (is (false? (:zero-undetected-policy-escapes-established? scope)))
        (is (contains? (:not-inspected scope) :arbitrary-clojure-model-bodies))
        (is (contains? (:not-inspected scope) :authorization-and-disclosure-predicates))
        (is (contains? (:not-inspected scope) :transaction-guard-implementation))
        (is (contains? (:not-inspected scope) :cross-operation-domain-invariants))
        (is (contains? (:inspected scope) :operation-runtime-backbone-closure))
        (is (empty? (set/intersection (:inspected scope)
                                               (:not-inspected scope))))))))

(deftest missing-policy-evidence-stays-explicit-across-report-views
  (with-closed-application
    (fn [{:keys [assembly report]}]
      (let [audit (:semantic-audit (application/explain assembly))]
        (is (= {:status :not-inspected
                :reason :opaque-application-model-policy}
               (:authorization-disclosure-view audit)))
        (is (= {:status :not-inspected
                :reason :no-admitted-domain-transition-relation-in-this-report}
               (:lifecycle-transition-view audit)))
        (is (= {:status :not-inspected
                :reason :no-admitted-rule-dependency-graph-in-this-report}
               (:rule-dependencies audit)))
        (is (= (vec (sort-by pr-str (get-in report [:analysis :trusted-assumptions])))
               (:application-trusted-assumptions audit)))
        (is (some #{model-premise} (:application-trusted-assumptions audit)))
        (is (= (get-in report [:analysis :open-obligations])
               (:open-obligations audit)))
        (is (every? #(not= :proved (:class %))
                    (:assurance-coverage audit)))))))

(deftest audit-does-not-change-operation-explanation-contract
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [one (application/explain-operation assembly :request/claim)
            whole (application/explain assembly)]
        (is (= :request/claim (:operation one)))
        (is (not (contains? one :semantic-audit)))
        (is (contains? whole :semantic-audit))
        (is (= :unknown-operation
               (error-kind
                #(application/explain-operation assembly :request/unknown))))))))

(deftest report-derivation-is-deterministic-and-inputs-stay-immutable
  (with-closed-application
    (fn [{:keys [assembly report]}]
      (let [before-report report
            before-assembly assembly
            one (:semantic-audit (application/explain assembly))
            two (:semantic-audit (application/explain assembly))
            three (semantic-audit/audit-from-current-report report)]
        (is (= one two three))
        (is (= before-report report))
        (is (= before-assembly assembly))
        (is (= (vec (sort-by pr-str (keys (:operation-cards one))))
               (vec (keys (:operation-cards one)))))
        (is (= (vec (sort-by (comp pr-str :operation)
                             (:opaque-model-boundaries one)))
               (:opaque-model-boundaries one)))))))

(deftest invalid-audit-inputs-fail-without-success-claims
  (let [skeleton {:gesso.live.application-preflight/type
                  :gesso.live.application-preflight/report
                  :gesso.live.application-preflight/version 3
                  :valid? true
                  :analysis {:operations {}
                             :trusted-assumptions #{model-premise}
                             :open-obligations []}}]
    (doseq [[label value reason]
            [[:nil nil :unsupported-or-unsuccessful-report]
             [:unsuccessful (assoc skeleton :valid? false)
              :unsupported-or-unsuccessful-report]
             [:wrong-version
              (assoc skeleton :gesso.live.application-preflight/version 999)
              :unsupported-or-unsuccessful-report]
             [:missing-assumption
              (assoc-in skeleton [:analysis :trusted-assumptions] #{})
              :missing-declared-model-conformance-premise]
             [:wrong-operations
              (assoc-in skeleton [:analysis :operations] [])
              :unsupported-or-unsuccessful-report]
             [:wrong-obligations
              (assoc-in skeleton [:analysis :open-obligations] {})
              :unsupported-or-unsuccessful-report]]]
      (testing (name label)
        (let [data (try
                     (semantic-audit/audit-from-current-report value)
                     nil
                     (catch clojure.lang.ExceptionInfo e (ex-data e)))]
          (is (= :invalid-semantic-audit-input (:error/kind data)))
          (is (= reason (:reason data))))))))

(deftest forged-application-and-report-evidence-do-not-gain-audits
  (with-closed-application
    (fn [{:keys [assembly report]}]
      (let [forged-report (assoc-in report [:analysis :trusted-assumptions] #{})
            forged-operations (assoc-in report [:analysis :operations] {})
            forged-assembly (assoc assembly :browser-artifact-path "/nonexistent/gesso-live.js")]
        (is (= :unrecognized-value
               (error-kind #(application/explain forged-report))))
        (is (= :unrecognized-value
               (error-kind #(application/explain forged-operations))))
        (is (= :unrecognized-value
               (error-kind #(application/explain forged-assembly))))
        (is (not (contains? assembly :semantic-audit)))
        (is (not (contains? report :semantic-audit)))))))

(deftest physical-artifact-replacement-invalidates-previous-evidence
  (with-closed-application
    (fn [{:keys [assembly report artifact-path]}]
      (is (map? (:semantic-audit (application/explain assembly))))
      (is (map? (:semantic-audit (application/explain report))))
      (spit artifact-path "// changed after recording the browser artifact receipt\n")
      (is (= :unrecognized-value
             (error-kind #(application/explain assembly))))
      (is (= :unrecognized-value
             (error-kind #(application/explain report)))))))

(deftest recognized-unsuccessful-report-does-not-get-successful-audit
  (with-closed-application
    (fn [{:keys [options]}]
      (let [bad (application/check-application-assembly
                 (assoc options :browser-artifact-path "/nonexistent/gesso-live.js"))]
        (is (false? (:valid? bad)))
        (is (application/report? bad))
        (let [explanation (application/explain bad)]
          (is (not (contains? explanation :semantic-audit)))
          (is (not= :application-runtime-backbone-preflight-closed
                    (:guarantee explanation))))))))


;; Regression for v778: the execution assembly places operation-specific
;; prerequisites beneath :required-by-operation. The public report and semantic
;; audit must preserve those checked prerequisites, never the global :supplied
;; capability set in their place.
(deftest canonical-execution-capabilities-reach-all-public-explanations
  (with-closed-application
    (fn [{:keys [assembly report operation-acquisition-assembly]}]
      (let [capabilities (get-in operation-acquisition-assembly
                                 [:execution-assembly :execution-capabilities])
            required (:required-by-operation capabilities)
            supplied (:supplied capabilities)
            report-operations (get-in report [:analysis :operations])
            assembly-explanation (application/explain assembly)
            report-explanation (application/explain report)
            cards (get-in assembly-explanation [:semantic-audit :operation-cards])]
        (is (true? (:valid? report)))
        (is (= #{:authenticated-principal} supplied))
        (is (= #{:request/claim :request/reassign :request/cancel}
               (set (keys required))))
        (is (= (set (keys required)) (set (keys report-operations))))
        (is (= (set (keys required)) (set (keys cards))))
        (is (= (:semantic-audit assembly-explanation)
               (:semantic-audit report-explanation)))
        (doseq [[operation prerequisites] required]
          (testing (str "canonical execution prerequisites for " operation)
            (is (set? prerequisites))
            (is (contains? prerequisites :authenticated-principal))
            (is (= prerequisites
                   (get-in report-operations [operation :execution-capabilities])))
            (is (= prerequisites
                   (get-in assembly-explanation
                           [:operations operation :execution-capabilities])))
            (is (= prerequisites
                   (get-in report-explanation
                           [:analysis :operations operation :execution-capabilities])))
            (is (= prerequisites
                   (get-in cards [operation :contract :execution-capabilities])))
            (is (= prerequisites
                   (:execution-capabilities
                    (application/explain-operation assembly operation))))))
        ;; Prerequisites describe construction, not proof that opaque host
        ;; predicates, services, and transaction guards meet those contracts.
        (is (= :unknown
               (get-in assembly-explanation
                       [:semantic-audit :business-rule-catalog :count])))
        (is (every? (fn [card]
                      (= :opaque-host-implementation
                         (:model-policy-inspection card)))
                    (vals cards)))))))

(deftest forged-execution-capability-evidence-cannot-gain-a-public-audit
  (with-closed-application
    (fn [{:keys [assembly report]}]
      (let [forged-assembly
            (assoc-in assembly
                      [:operation-acquisition-assembly
                       :execution-assembly
                       :execution-capabilities
                       :required-by-operation
                       :request/claim]
                      #{:fixture/forged-capability})
            forged-report
            (assoc-in report
                      [:analysis :operations :request/claim
                       :execution-capabilities]
                      #{:fixture/forged-capability})]
        (is (= :unrecognized-value
               (error-kind #(application/explain forged-assembly))))
        (is (= :unrecognized-value
               (error-kind #(application/explain forged-report))))
        (is (= :invalid-application-assembly
               (error-kind
                #(application/explain-operation forged-assembly
                                                :request/claim))))))))
