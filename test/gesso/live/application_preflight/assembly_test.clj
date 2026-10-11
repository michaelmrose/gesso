(ns gesso.live.application-preflight.assembly-test
  "Integration tests for closed application assemblies and their assurance
   boundaries. These tests are moved unchanged from the original
   gesso.live.application-preflight-test namespace; they use the shared
   fixture but exercise the public application preflight API."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.fixture
    :refer [application-options child-path error-data error-kind error-kinds
            standard-operation-acquisition warning-kinds with-closed-application
            with-temp-dir]]
   [gesso.live.browser.build :as browser-build]
   [gesso.live.operation-acquisition-preflight :as operation-acquisition]))

;; =============================================================================
;; Canonical whole-application backbone closure
;; =============================================================================

(deftest successful-application-assembly-closes-runtime-backbone-but-keeps-affordance-obligation-open
  (with-closed-application
    (fn [{:keys [operation-acquisition-assembly report assembly]}]
      (let [explanation (application/explain assembly)
            obligations (application/open-obligations assembly)]
        (testing "the report and emitted physical product are current"
          (is (application/report? report))
          (is (application/valid? report))
          (is (application/application-assembly? assembly))
          (is (= [] (:errors report))))

        (testing "the whole-application affordance edge remains explicitly open"
          (is (= 1 (count obligations)))
          (is (= :static-affordance-closure-not-yet-modeled
                 (:kind (first obligations))))
          (is (= :rendered-affordance->semantic-operation
                 (:edge (first obligations))))
          (is (= :open (:status (first obligations))))
          (is (= obligations
                 (get-in report [:analysis :open-obligations])))
          (is (= obligations (:open-obligations explanation)))
          (is (= #{:static-affordance-closure-not-yet-modeled
                   :published-change-topics-not-consumed-by-live-app}
                 (warning-kinds report))))

        (testing "the guarantee is deliberately narrower than whole-application closure"
          (is (= :application-runtime-backbone-preflight-closed
                 (:guarantee explanation)))
          (is (not= :whole-application-preflight-closed
                    (:guarantee explanation))))

        (testing "the application assembly stores no duplicated derived operation registry"
          (is (= operation-acquisition-assembly
                 (:operation-acquisition-assembly assembly)))
          (is (= #{:gesso.live.application-preflight/type
                   :gesso.live.application-preflight/version
                   :name
                   :operation-acquisition-assembly
                   :browser-artifact-path
                   :browser-receipt-path}
                 (set (keys assembly))))
          (is (not (contains? assembly :operations)))
          (is (not (contains? assembly :open-obligations)))
          (is (not (contains? assembly :trusted-assumptions))))))))

(deftest operation-explanation-reaches-physical-command-and-authoritative-acquisition-routes
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [claim (application/explain-operation assembly :request/claim)
            reassign (application/explain-operation assembly :request/reassign)
            cancel (application/explain-operation assembly :request/cancel)]
        (testing "claim closes browser/server/publication/acquisition chain"
          (is (= :request/claim (:operation claim)))
          (is (= :application-runtime-backbone-preflight-closed
                 (:guarantee claim)))
          (is (= :htmx (:command-transport claim)))
          (is (= [{:route-id :request/claim
                   :method :post
                   :path "/operations/request/claim"
                   :required-transport :htmx}]
                 (:routes claim)))
          (is (= #{:request} (:published-change-topics claim)))
          (is (= #{:request-toolbar :request-list}
                 (:affected-scopes claim)))
          (is (= #{:request-toolbar :request-list}
                 (:affected-fragments claim)))
          (is (= #{:request-toolbar :request-list}
                 (set (keys (:authoritative-acquisitions claim)))))
          (is (= {:method :get
                  :path "/fragments/request-list"
                  :boundary :gesso.live.core/render-fragment-response}
                 (get-in claim
                         [:authoritative-acquisitions
                          :request-list
                          :fragment-route])))
          (is (= {:method :get
                  :path "/streams/request-list"
                  :boundary :gesso.live.transport.sse/start-fragment-stream!}
                 (get-in claim
                         [:authoritative-acquisitions
                          :request-list
                          :stream-route]))))

        (testing "semantic scope impact remains richer than managed fragment impact"
          (is (= #{:request-toolbar :request-list :audit-scope}
                 (:affected-scopes reassign)))
          (is (= #{:request-toolbar :request-list}
                 (:affected-fragments reassign))))

        (testing "explicit no-publication operation has no authoritative acquisition"
          (is (= #{} (:published-change-topics cancel)))
          (is (= #{} (:affected-scopes cancel)))
          (is (= #{} (:affected-fragments cancel)))
          (is (= {} (:authoritative-acquisitions cancel))))))))

;; =============================================================================
;; Physical currentness
;; =============================================================================

(deftest changing-generated-artifact-invalidates-report-assembly-and-inspection
  (with-closed-application
    (fn [{:keys [artifact-path report assembly]}]
      (is (application/report? report))
      (is (application/application-assembly? assembly))

      (spit artifact-path
            "console.log('tampered after application assembly');\n"
            :encoding "UTF-8")

      (is (not (application/report? report)))
      (is (not (application/valid? report)))
      (is (not (application/application-assembly? assembly)))
      (is (= :invalid-application-input
             (error-kind #(application/open-obligations assembly))))
      (is (= :invalid-application-assembly
             (error-kind #(application/explain-operation
                           assembly
                           :request/claim))))
      (is (= :unrecognized-value
             (error-kind #(application/explain assembly)))))))

(deftest changing-default-receipt-invalidates-currentness-without-changing-artifact
  (with-closed-application
    (fn [{:keys [artifact-path report assembly]}]
      (let [receipt-path (browser-build/receipt-path artifact-path)]
        (spit receipt-path "{:tampered true}\n" :encoding "UTF-8")
        (is (not (application/report? report)))
        (is (not (application/application-assembly? assembly)))
        (is (= :browser-artifact-verification-failed
               (first (error-kinds
                       (application/check-application-assembly
                        {:operation-acquisition-assembly
                         (:operation-acquisition-assembly assembly)
                         :browser-artifact-path artifact-path})))))))))

(deftest custom-receipt-path-is-a-first-class-currentness-boundary
  (with-closed-application
    {:custom-receipt? true}
    (fn [{:keys [artifact-path receipt-path report assembly]}]
      (let [explanation (application/explain assembly)
            default-path (browser-build/receipt-path artifact-path)]
        (is (application/report? report))
        (is (application/application-assembly? assembly))
        (is (= receipt-path (:browser-receipt-path assembly)))
        (is (= receipt-path
               (get-in explanation [:browser-artifact :receipt-path])))
        (is (.isFile (java.io.File. receipt-path)))
        (is (not (.exists (java.io.File. default-path))))

        (spit receipt-path "{:tampered :custom-receipt}\n" :encoding "UTF-8")
        (is (not (application/report? report)))
        (is (not (application/application-assembly? assembly)))))))

(deftest missing-physical-artifact-fails-with-derived-semantic-analysis-still-visible
  (with-temp-dir
    (fn [dir]
      (let [operation-acquisition' @standard-operation-acquisition
            artifact-path (child-path dir "missing.js")
            report
            (application/check-application-assembly
             (application-options operation-acquisition' artifact-path))]
        (is (application/report? report))
        (is (not (application/valid? report)))
        (is (= #{:browser-artifact-verification-failed}
               (error-kinds report)))
        (is (= #{:request/claim :request/reassign :request/cancel}
               (set (keys (get-in report [:analysis :operations])))))
        (is (= :application-backbone-preflight-failed
               (error-kind
                #(application/require-application-assembly!
                  (application-options
                   operation-acquisition'
                   artifact-path)))))))))

;; =============================================================================
;; Closed report and assembly tamper resistance
;; =============================================================================

(deftest derived-analysis-tampering-invalidates-recognized-report
  (with-closed-application
    (fn [{:keys [report]}]
      (doseq [tampered
              [(assoc-in report
                         [:analysis :operations :request/claim :affected-fragments]
                         #{:request-list})
               (assoc-in report
                         [:analysis :operations :request/claim :published-change-topics]
                         #{:audit})
               (assoc-in report
                         [:analysis :open-obligations]
                         [])
               (assoc-in report
                         [:analysis :trusted-assumptions]
                         #{:invented-assumption})
               (assoc report :warnings [])]]
        (is (not (application/report? tampered)))
        (is (not (application/valid? tampered)))))))

(deftest failed-report-cannot-be-forged-positive-by-clearing-errors
  (with-temp-dir
    (fn [dir]
      (let [operation-acquisition' @standard-operation-acquisition
            artifact-path (child-path dir "missing.js")
            report
            (application/check-application-assembly
             (application-options operation-acquisition' artifact-path))
            forged (assoc report :valid? true :errors [])]
        (is (application/report? report))
        (is (not (application/valid? report)))
        (is (not (application/report? forged)))
        (is (not (application/valid? forged)))))))

(deftest closed-application-assembly-rejects-duplicated-derived-fields
  (with-closed-application
    (fn [{:keys [assembly]}]
      (doseq [tampered
              [(assoc assembly
                      :operations
                      (:operations (application/explain assembly)))
               (assoc assembly
                      :open-obligations
                      (application/open-obligations assembly))
               (assoc assembly
                      :trusted-assumptions
                      (:trusted-assumptions (application/explain assembly)))]]
        (is (not (application/application-assembly? tampered)))
        (is (= :invalid-application-input
               (error-kind #(application/open-obligations tampered))))))))

(deftest nested-operation-acquisition-tampering-invalidates-report-and-assembly
  (with-closed-application
    (fn [{:keys [report assembly]}]
      (let [tampered-operation-acquisition
            (assoc (:operation-acquisition-assembly assembly)
                   :name
                   "not-a-keyword")
            tampered-assembly
            (assoc assembly
                   :operation-acquisition-assembly
                   tampered-operation-acquisition)
            tampered-report
            (assoc-in report
                      [:analysis :operation-acquisition-assembly]
                      tampered-operation-acquisition)]
        (is (not (operation-acquisition/operation-acquisition-assembly?
                  tampered-operation-acquisition)))
        (is (not (application/application-assembly? tampered-assembly)))
        (is (not (application/report? tampered-report)))
        (is (= :invalid-application-input
               (error-kind
                #(application/open-obligations tampered-assembly))))))))

(deftest nested-execution-or-acquisition-tampering-propagates-through-unified-assembly
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [operation-acquisition'
            (:operation-acquisition-assembly assembly)
            bad-execution
            (assoc (:execution-assembly operation-acquisition')
                   :name
                   "not-a-keyword")
            bad-acquisition
            (assoc (:acquisition-assembly operation-acquisition')
                   :name
                   "not-a-keyword")]
        (doseq [bad-operation-acquisition
                [(assoc operation-acquisition'
                        :execution-assembly bad-execution)
                 (assoc operation-acquisition'
                        :acquisition-assembly bad-acquisition)]]
          (is (not (operation-acquisition/operation-acquisition-assembly?
                    bad-operation-acquisition)))
          (is (= #{:invalid-operation-acquisition-assembly}
                 (error-kinds
                  (application/check-application-assembly
                   {:operation-acquisition-assembly bad-operation-acquisition
                    :browser-artifact-path (:browser-artifact-path assembly)})))))))))

;; =============================================================================
;; Explanation, obligations, warnings, and option boundaries
;; =============================================================================

(deftest open-obligations-is-identical-before-and-after-physical-application-assembly
  (with-closed-application
    (fn [{:keys [operation-acquisition-assembly assembly]}]
      (is (= (application/open-obligations operation-acquisition-assembly)
             (application/open-obligations assembly)))
      (is (= :static-affordance-closure-not-yet-modeled
             (:kind
              (first
               (application/open-obligations
                operation-acquisition-assembly))))))))

(deftest trusted-assumptions-remain-separate-from-open-obligations-and-guarantee
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [explanation (application/explain assembly)
            assumptions (:trusted-assumptions explanation)
            obligations (:open-obligations explanation)]
        (is (set? assumptions))
        (is (contains? assumptions
                       :application-publication-declarations-match-model-publication))
        (is (contains? assumptions
                       :trusted-command-route-declarations-match-installed-handlers))
        (is (contains? assumptions
                       :trusted-acquisition-route-declarations-match-installed-handlers))
        (is (= :application-runtime-backbone-preflight-closed
               (:guarantee explanation)))
        (is (= :open (:status (first obligations))))
        (is (not (contains? assumptions
                            :static-affordance-closure-not-yet-modeled)))))))

(deftest unconsumed-publication-warning-is-lifted-without-becoming-an-open-acquisition-error
  (with-closed-application
    (fn [{:keys [report assembly]}]
      (let [warning
            (first
             (filter
              #(= :published-change-topics-not-consumed-by-live-app
                  (:kind %))
              (:warnings report)))]
        (is (application/valid? report))
        (is (= #{:published-change-topics-not-consumed-by-live-app
                 :static-affordance-closure-not-yet-modeled}
               (warning-kinds report)))
        (is (= {:request/reassign #{:external/audit}}
               (:topics-by-operation warning)))
        (is (= :application-runtime-backbone-preflight-closed
               (:guarantee (application/explain assembly))))))))

(deftest unknown-operation-explanation-fails-with-closed-available-set
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [data
            (error-data
             #(application/explain-operation assembly :request/unknown))]
        (is (= :unknown-operation (:error/kind data)))
        (is (= :request/unknown (:operation data)))
        (is (= #{:request/claim :request/reassign :request/cancel}
               (:available-operations data)))))))

(deftest malformed-options-fail-before-physical-verification
  (let [operation-acquisition' @standard-operation-acquisition]
    (is (= :invalid-shape
           (error-kind #(application/check-application-assembly nil))))
    (is (= :missing-option
           (error-kind
            #(application/check-application-assembly
              {:browser-artifact-path "/tmp/unused.js"}))))
    (is (= :missing-option
           (error-kind
            #(application/check-application-assembly
              {:operation-acquisition-assembly operation-acquisition'}))))
    (is (= :unknown-option-keys
           (error-kind
            #(application/check-application-assembly
              {:operation-acquisition-assembly operation-acquisition'
               :browser-artifact-path "/tmp/unused.js"
               :surprise true}))))
    (is (= :invalid-name
           (error-kind
            #(application/check-application-assembly
              {:name "not-a-keyword"
               :operation-acquisition-assembly operation-acquisition'
               :browser-artifact-path "/tmp/unused.js"}))))
    (is (= :invalid-browser-artifact-path
           (error-kind
            #(application/check-application-assembly
              {:operation-acquisition-assembly operation-acquisition'
               :browser-artifact-path "  "}))))
    (is (= :invalid-browser-receipt-path
           (error-kind
            #(application/check-application-assembly
              {:operation-acquisition-assembly operation-acquisition'
               :browser-artifact-path "/tmp/unused.js"
               :browser-receipt-path "  "}))))))
