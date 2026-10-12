(ns gesso.live.application-preflight.assurance-integration-test
  "Integration tests of application-level assurance explanations and their
   deployment-enforcement boundaries. Test bodies are copied unchanged from
   gesso.live.application-preflight-test (v760). Original tests remain there
   until the subsequent removal revision."
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.core :as g]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.fixture
    :refer [error-kind render-context rendered-operation-button
            standard-operations with-closed-application
            with-surfaced-application]]))

;; Shared by the remaining deployment-attestation assurance test.
(def module-test-modules
  [application/application-handler-module])

(defn canonical-start-module-handler
  [_request]
  (g/html-response [:main [:p "module-handler"]]))

;; =============================================================================
;; v640 consolidated application explanation contract
;; =============================================================================

(defn- escape-hatches-by-kind
  [explanation]
  (into {}
        (map (juxt :kind identity))
        (:explicit-escape-hatches explanation)))

(deftest consolidated-explanation-separates-closure-enforcement-obligations-assumptions-and-escapes
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [explanation (application/explain assembly)
            enforcement (:canonical-html-enforcement explanation)
            obligations (:open-obligations explanation)
            assumptions (:trusted-assumptions explanation)
            escapes (:explicit-escape-hatches explanation)
            escape-kinds (set (map :kind escapes))]
        (is (= :application-runtime-backbone-preflight-closed
               (:guarantee explanation)))
        (is (= :available-on-canonical-gesso-biff-startup
               (:status enforcement)))
        (is (= :gesso.live.application-preflight/start-biff-application!
               (:startup-boundary enforcement)))
        (is (= :biff.ring/handler (:biff-handler-key enforcement)))
        (is (= :gesso.live.application-preflight/wrap-application-handler
               (:handler-boundary enforcement)))
        (is (= :gesso.http/html-response (:response-boundary enforcement)))
        (is (= :rendered-surface-pre-browser-preflight-closed
               (:per-render-guarantee enforcement)))
        (is (= :not-carried-by-application-assembly
               (:installation-proof enforcement)))
        (is (= #{:direct-biff-start-bypass
                 :later-handler-replacement
                 :server-ignores-biff-ring-handler
                 :pre-serialized-html-ring-response}
               escape-kinds))
        (is (= #{:static-affordance-closure-not-yet-modeled}
               (set (map :kind obligations))))
        (is (set? assumptions))
        (is (not-any? assumptions escape-kinds))
        (is (not-any? (set (map :kind obligations)) escape-kinds))
        (is (not (contains? assembly :canonical-html-enforcement)))
        (is (not (contains? assembly :explicit-escape-hatches)))
        (is (not (contains? assembly :deployment-proof)))))))

(deftest historical-obligation-identifiers-remain-stable-but-messages-describe-current-deployment-boundary
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [obligation (first (application/open-obligations assembly))]
        (is (= :static-affordance-closure-not-yet-modeled (:kind obligation)))
        (is (= :rendered-affordance->semantic-operation (:edge obligation)))
        (is (= :open (:status obligation)))
        (is (re-find #"Canonical Gesso/Biff startup"
                     (:message obligation)))
        (is (re-find #"ApplicationAssembly alone"
                     (:message obligation)))
        (is (re-find #"deployment"
                     (:message obligation))))))
  (let [operations (standard-operations)
        ctx (render-context operations)
        surfaces
        {:board
         [:main
          (rendered-operation-button
           ctx
           :request/claim
           "/operations/request/claim")]}]
    (with-surfaced-application
      surfaces
      (fn [{:keys [assembly]}]
        (let [obligation (first (application/open-obligations assembly))]
          (is (= :rendered-surface-enumeration-completeness-not-yet-modeled
                 (:kind obligation)))
          (is (= :application->rendered-surfaces (:edge obligation)))
          (is (= :open (:status obligation)))
          (is (re-find #"Canonical Gesso/Biff startup"
                       (:message obligation)))
          (is (re-find #"ApplicationAssembly alone"
                       (:message obligation)))
          (is (re-find #"raw-response/handler-replacement escape hatches"
                       (:message obligation))))))))

(deftest application-and-operation-explanations-share-one-canonical-enforcement-contract
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [application-explanation (application/explain assembly)
            claim (application/explain-operation assembly :request/claim)
            enforcement (:canonical-html-enforcement application-explanation)]
        (is (= enforcement (:canonical-html-enforcement claim)))
        (is (= :application-runtime-backbone-preflight-closed
               (:guarantee claim)))
        (is (= :request/claim (:operation claim)))
        (is (= :not-carried-by-application-assembly
               (get-in claim [:canonical-html-enforcement :installation-proof])))
        (is (not (contains? claim :deployment-proof)))
        (is (not (contains? claim :explicit-escape-hatches)))))))

(deftest explanation-of-recognized-report-carries-enforcement-and-escape-boundaries-without-promoting-them-into-analysis
  (with-closed-application
    (fn [{:keys [report assembly]}]
      (let [from-report (application/explain report)
            from-assembly (application/explain assembly)]
        (is (= (:canonical-html-enforcement from-assembly)
               (:canonical-html-enforcement from-report)))
        (is (= (:explicit-escape-hatches from-assembly)
               (:explicit-escape-hatches from-report)))
        (is (= (:open-obligations from-assembly)
               (get-in from-report [:analysis :open-obligations])))
        (is (= (:trusted-assumptions from-assembly)
               (get-in from-report [:analysis :trusted-assumptions])))
        (is (not (contains? (:analysis from-report)
                            :canonical-html-enforcement)))
        (is (not (contains? (:analysis from-report)
                            :explicit-escape-hatches)))
        (is (not (contains? (:analysis from-report)
                            :deployment-proof)))))))

(deftest application-report-recognition-rejects-obligation-and-assumption-tampering-after-consolidation
  (with-closed-application
    (fn [{:keys [report]}]
      (let [forged-obligation
            (assoc-in report
                      [:analysis :open-obligations 0 :status]
                      :closed)
            forged-message
            (assoc-in report
                      [:analysis :open-obligations 0 :message]
                      "deployment proven")
            forged-assumptions
            (assoc-in report
                      [:analysis :trusted-assumptions]
                      #{})]
        (is (application/report? report))
        (is (not (application/report? forged-obligation)))
        (is (not (application/report? forged-message)))
        (is (not (application/report? forged-assumptions)))
        (is (= :unrecognized-value
               (error-kind #(application/explain forged-obligation))))
        (is (= :unrecognized-value
               (error-kind #(application/explain forged-message))))
        (is (= :unrecognized-value
               (error-kind #(application/explain forged-assumptions))))))))

(deftest canonical-startup-does-not-mutate-application-assembly-into-a-deployment-attestation
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [before (application/explain assembly)
            started
            (application/start-biff-application!
             assembly
             {:biff.ring/handler canonical-start-module-handler
              :fixture/started true}
             #'module-test-modules
             [application/application-handler-module-id])
            after (application/explain assembly)]
        (is (true? (:fixture/started started)))
        (is (fn? (:biff.ring/handler started)))
        (is (= before after))
        (is (= :not-carried-by-application-assembly
               (get-in after
                       [:canonical-html-enforcement
                        :installation-proof])))
        (is (= #{:direct-biff-start-bypass
                 :later-handler-replacement
                 :server-ignores-biff-ring-handler
                 :pre-serialized-html-ring-response}
               (set (map :kind (:explicit-escape-hatches after)))))
        (is (= :open
               (:status (first (:open-obligations after)))))
        (is (not (contains? assembly :deployment-proof)))))))

(deftest consolidated-escape-hatches-have-distinct-explicit-boundaries
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [by-kind (escape-hatches-by-kind (application/explain assembly))]
        (is (= :application-startup
               (get-in by-kind [:direct-biff-start-bypass :boundary])))
        (is (= :biff-module-start-order
               (get-in by-kind [:later-handler-replacement :boundary])))
        (is (= :server-integration
               (get-in by-kind [:server-ignores-biff-ring-handler :boundary])))
        (is (= :html-serialization
               (get-in by-kind [:pre-serialized-html-ring-response :boundary])))
        (is (every? string? (map :description (vals by-kind))))
        (is (every? seq (map :description (vals by-kind))))))))

