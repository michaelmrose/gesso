(ns gesso.live.application-preflight.contradiction-integration-test
  "Integration tests for the original v641 whole-application contradiction
   campaign, migrated unchanged from application_preflight_test.clj (v762).
   This campaign distinguishes structural contradictions from trusted physical
   boundaries and explicitly documented deployment escape hatches.
   The original tests remain in place until the v764 removal revision."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.fixture
    :refer [application-options child-path error-data error-kind error-kinds
            operation-acquisition-assembly record-artifact! render-context
            rendered-operation-button standard-operations trusted-operation
            with-closed-application with-temp-dir]]))

;; =============================================================================
;; v641 whole-application contradiction campaign
;; =============================================================================

(defn- contradiction-report
  [operation-acquisition-assembly artifact-path]
  (application/check-application-assembly
   (application-options operation-acquisition-assembly artifact-path)))

(defn- assert-structural-contradiction-rejected!
  [report]
  (is (application/report? report))
  (is (not (application/valid? report)))
  (is (contains? (error-kinds report)
                 :invalid-operation-acquisition-assembly)))

(deftest whole-application-campaign-rejects-browser-plan-correspondence-corruption
  (with-closed-application
    (fn [{:keys [operation-acquisition-assembly artifact-path]}]
      (let [tampered
            (assoc-in
             operation-acquisition-assembly
             [:execution-assembly
              :route-assembly
              :operation-assembly
              :operations
              :request/claim
              :browser-plan-digest]
             "forged-browser-plan-digest")
            report (contradiction-report tampered artifact-path)]
        (assert-structural-contradiction-rejected! report)
        (is (= :application-backbone-preflight-failed
               (error-kind
                #(application/require-application-assembly!
                  (application-options tampered artifact-path)))))))))

(deftest whole-application-campaign-rejects-execution-capability-summary-corruption
  (with-closed-application
    (fn [{:keys [operation-acquisition-assembly artifact-path]}]
      (let [tampered
            (update-in
             operation-acquisition-assembly
             [:execution-assembly
              :execution-capabilities
              :request/claim]
             conj
             :forged-capability)
            report (contradiction-report tampered artifact-path)]
        (assert-structural-contradiction-rejected! report)
        (is (= {}
               (get-in report [:analysis :operations])))))))

(deftest whole-application-campaign-rejects-settlement-contract-corruption
  (with-closed-application
    (fn [{:keys [operation-acquisition-assembly artifact-path]}]
      (let [tampered
            (assoc-in
             operation-acquisition-assembly
             [:execution-assembly
              :settlement-contracts
              :request/claim]
             {:forged :settlement-contract})
            report (contradiction-report tampered artifact-path)]
        (assert-structural-contradiction-rejected! report)
        (is (= {}
               (get-in report [:analysis :operations])))))))

(deftest whole-application-campaign-treats-command-route-path-as-trusted-until-a-rendered-affordance-disagrees
  (with-closed-application
    (fn [{:keys [operation-acquisition-assembly artifact-path]}]
      (let [new-path "/operations/request/claim-v2"
            changed
            (assoc-in
             operation-acquisition-assembly
             [:execution-assembly
              :route-assembly
              :routes
              :request/claim
              :path]
             new-path)
            report (contradiction-report changed artifact-path)
            assembly
            (application/require-application-assembly!
             (application-options changed artifact-path))
            ctx (render-context (standard-operations))
            old-surface
            [:main
             (rendered-operation-button
              ctx :request/claim "/operations/request/claim")]
            new-surface
            [:main
             (rendered-operation-button ctx :request/claim new-path)]]
        (testing "a well-formed route declaration is an application-owned physical fact"
          (is (application/valid? report))
          (is (= new-path
                 (get-in (application/explain-operation assembly :request/claim)
                         [:routes 0 :path]))))
        (testing "the independent rendered affordance makes an old physical URL contradictory"
          (is (= #{:rendered-affordance-path-mismatch}
                 (error-kinds
                  (application/check-rendered-surface
                   assembly :old-request-board old-surface))))
          (is (true?
               (:valid?
                (application/check-rendered-surface
                 assembly :new-request-board new-surface)))))))))

(deftest whole-application-campaign-keeps-acquisition-url-as-an-explicit-trusted-physical-boundary
  (with-closed-application
    (fn [{:keys [operation-acquisition-assembly artifact-path]}]
      (let [new-path "/fragments/request-list-v2"
            changed
            (assoc-in
             operation-acquisition-assembly
             [:acquisition-assembly
              :realizations
              :request-list
              :fragment-route
              :path]
             new-path)
            report (contradiction-report changed artifact-path)
            assembly
            (application/require-application-assembly!
             (application-options changed artifact-path))
            claim (application/explain-operation assembly :request/claim)
            explanation (application/explain assembly)]
        (is (application/valid? report))
        (is (= new-path
               (get-in claim
                       [:authoritative-acquisitions
                        :request-list
                        :fragment-route
                        :path])))
        (is (contains? (:trusted-assumptions explanation)
                       :trusted-acquisition-route-declarations-match-installed-handlers))))))

(deftest whole-application-campaign-rejects-rendered-affordance-physical-route-contradiction-before-browser-delivery
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [ctx (render-context (standard-operations))
            surface
            [:main
             (rendered-operation-button
              ctx
              :request/claim
              "/operations/request/not-claim")]
            rendered? (atom false)
            error
            (error-data
             #(application/checked-rendered-response!
               assembly
               :contradictory-request-board
               (fn [_]
                 (reset! rendered? true)
                 {:status 200})
               surface))]
        (is (= :rendered-surface-preflight-failed
               (:error/kind error)))
        (is (= #{:rendered-affordance-path-mismatch}
               (set (map :kind (get-in error [:preflight :errors])))))
        (is (false? @rendered?))))))

(deftest whole-application-campaign-stale-artifact-destroys-previously-credible-closure
  (with-closed-application
    (fn [{:keys [artifact-path report assembly]}]
      (is (application/valid? report))
      (is (application/application-assembly? assembly))
      (spit artifact-path
            "console.log('v641 contradictory artifact');\n"
            :encoding "UTF-8")
      (is (not (application/report? report)))
      (is (not (application/application-assembly? assembly)))
      (is (= :invalid-application-handler-assembly
             (error-kind
              #(application/wrap-application-handler
                assembly
                (fn [_] {:status 200}))))))))

(deftest whole-application-campaign-does-not-misclassify-a-new-valid-trusted-publication-declaration-as-forgery
  (let [operations
        (assoc
         (standard-operations)
         :request/claim
         (trusted-operation
          :request/claim
          {:published-change-topics #{:audit}}))
        operation-acquisition'
        (operation-acquisition-assembly operations)]
    (with-temp-dir
     (fn [dir]
       (let [artifact-path (child-path dir "trusted-publication.js")]
         (record-artifact! operation-acquisition' artifact-path)
         (let [report
               (application/check-application-assembly
                (application-options operation-acquisition' artifact-path))
               assembly
               (application/require-application-assembly!
                (application-options operation-acquisition' artifact-path))
               claim (application/explain-operation assembly :request/claim)]
           (is (application/valid? report))
           (is (= #{:audit} (:published-change-topics claim)))
           (is (= #{:audit-scope} (:affected-scopes claim)))
           (is (= #{} (:affected-fragments claim)))
           (is (contains?
                (:trusted-assumptions (application/explain assembly))
                :application-publication-declarations-match-model-publication))))))))

(deftest whole-application-campaign-keeps-pre-serialized-raw-ring-html-an-explicit-escape-hatch
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [raw-response
            {:status 200
             :headers {"content-type" "text/html; charset=utf-8"}
             :body "<button data-forged='true'>raw html</button>"}
            wrapped
            (application/wrap-application-handler
             assembly
             (fn [_] raw-response))
            response (wrapped {:uri "/raw"})
            explanation (application/explain assembly)]
        (is (= raw-response response))
        (is (contains?
             (set (map :kind (:explicit-escape-hatches explanation)))
             :pre-serialized-html-ring-response))
        (is (= :not-carried-by-application-assembly
               (get-in explanation
                       [:canonical-html-enforcement :installation-proof])))))))

