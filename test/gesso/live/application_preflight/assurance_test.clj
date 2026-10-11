(ns gesso.live.application-preflight.assurance-test
  "Direct regression tests for whole-application assurance explanations.

   This suite tests formatting and admitted-coverage boundaries on already
   supplied reports. It intentionally does not certify report currentness,
   ApplicationAssembly authority, physical browser bytes, or Clojure policy.
   Those remain obligations of the application-preflight coordinator."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.application-preflight.assurance :as assurance]
   [gesso.live.application-preflight.biff :as biff-lifecycle]
   [gesso.live.browser.build :as browser-build]))

(def ^:private surface-fixture
  {:request-board [:div [:button "Claim"]]
   :toolbar [:section "Toolbar"]})

(def ^:private operations-fixture
  {:request/claim {:command-route {:method :post
                                   :path "/requests/:id/claim"}
                   :published-change-topics #{:request}}
   :request/cancel {:command-route {:method :post
                                    :path "/requests/:id/cancel"}
                    :published-change-topics #{:request}}})

(def ^:private affordances-fixture
  [{:operation :request/claim :surface :request-board}
   {:operation :request/cancel :surface :toolbar}])

(def ^:private successful-closure
  {:status :closed-relative-to-supplied-rendered-surfaces
   :guarantee assurance/rendered-affordance-guarantee
   :surface-count 2
   :affordance-count 2})

(def ^:private report-fixture
  {:type assurance/report-type
   :version assurance/preflight-version
   :valid? true
   :errors []
   :warnings [{:kind :static-affordance-closure-not-yet-modeled}]
   :analysis
   {:name :fixture/request-app
    :browser-artifact-path "resources/public/js/gesso-live.js"
    :browser-receipt-path "resources/public/js/gesso-live.js.receipt.edn"
    :artifact-receipt {:sha256 "fixture-digest"}
    :operations operations-fixture
    :rendered-surfaces surface-fixture
    :affordances affordances-fixture
    :affordance-closure successful-closure
    :open-obligations [assurance/rendered-surface-completeness-open-obligation]
    :trusted-assumptions assurance/trusted-assumptions
    :private-source-product {:forged-or-sensitive "do not explain"}
    :extra/opaque {:internal true}}
   :operation-acquisition-assembly {:secret :not-public}})

(def ^:private assembly-fixture
  {:name :fixture/request-app
   :browser-artifact-path "resources/public/js/gesso-live.js"
   :browser-receipt-path "resources/public/js/gesso-live.js.receipt.edn"
   :operation-acquisition-assembly
   {:acquisition-assembly {:live-app {:name :fixture/live}}}
   :private-model {:trusted-callback 'not-for-explanation}})

(defn- exception-info-of [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo error
      error)))

(deftest published-guarantee-identifiers-are-stable
  (is (= :application-runtime-backbone-preflight-closed
         assurance/backbone-guarantee))
  (is (= :supplied-rendered-affordances-preflight-closed
         assurance/rendered-affordance-guarantee))
  (is (= :rendered-surface-pre-browser-preflight-closed
         assurance/rendered-surface-guarantee))
  (is (= 3 assurance/preflight-version)))

(deftest published-types-are-not-new-authority
  (is (= :gesso.live.application-preflight/application-assembly
         assurance/application-assembly-type))
  (is (= :gesso.live.application-preflight/report
         assurance/report-type)))

(deftest canonical-html-enforcement-references-actual-biff-module
  (is (= biff-lifecycle/application-handler-module-id
         (:biff-module-id assurance/canonical-html-enforcement)))
  (is (= biff-lifecycle/biff-ring-handler-key
         (:biff-handler-key assurance/canonical-html-enforcement)))
  (is (= assurance/rendered-surface-guarantee
         (:per-render-guarantee assurance/canonical-html-enforcement)))
  (is (= :not-carried-by-application-assembly
         (:installation-proof assurance/canonical-html-enforcement))))

(deftest canonical-enforcement-describes-entrypoint-boundaries
  (is (= :gesso.live.application-preflight/start-biff-application!
         (:startup-boundary assurance/canonical-html-enforcement)))
  (is (= :gesso.live.application-preflight/wrap-application-handler
         (:handler-boundary assurance/canonical-html-enforcement)))
  (is (= :gesso.http/html-response
         (:response-boundary assurance/canonical-html-enforcement))))

(deftest escape-hatches-are-explicit-and-distinct
  (is (= #{:direct-biff-start-bypass
           :later-handler-replacement
           :server-ignores-biff-ring-handler
           :pre-serialized-html-ring-response}
         (set (map :kind assurance/explicit-escape-hatches))))
  (is (every? #(and (keyword? (:boundary %))
                    (string? (:description %))
                    (seq (:description %)))
              assurance/explicit-escape-hatches))
  (is (= 4 (count assurance/explicit-escape-hatches))))

(deftest trusted-premises-are-not-claimed-guarantees
  (is (= #{:application-publication-declarations-match-model-publication
           :trusted-command-route-declarations-match-installed-handlers
           :trusted-acquisition-route-declarations-match-installed-handlers
           :browser-artifact-emitted-through-gesso-owned-application-build
           :clojurescript-compiler-realizes-generated-entrypoint
           :authenticated-principal-binding-is-correct
           :public-model-operation-contracts-hold
           :xtdb-transaction-and-consistency-contracts-hold}
         assurance/trusted-assumptions))
  (is (not (contains? assurance/trusted-assumptions
                      assurance/backbone-guarantee))))

(deftest missing-rendered-snapshot-has-an-open-obligation
  (is (= [assurance/affordance-open-obligation]
         (assurance/open-obligations-for-surfaces nil)))
  (is (= :open (:status (first (assurance/open-obligations-for-surfaces nil)))))
  (is (= :rendered-affordance->semantic-operation
         (:edge (first (assurance/open-obligations-for-surfaces nil))))))

(deftest supplied-snapshot-still-has-a-completeness-obligation
  (doseq [surfaces [{} surface-fixture [] false]]
    (testing (pr-str surfaces)
      (is (= [assurance/rendered-surface-completeness-open-obligation]
             (assurance/open-obligations-for-surfaces surfaces)))
      (is (= :open (:status (first (assurance/open-obligations-for-surfaces surfaces))))))))

(deftest obligation-kinds-and-edges-remain-distinct
  (is (= :static-affordance-closure-not-yet-modeled
         (:kind assurance/affordance-open-obligation)))
  (is (= :rendered-surface-enumeration-completeness-not-yet-modeled
         (:kind assurance/rendered-surface-completeness-open-obligation)))
  (is (= :application->rendered-surfaces
         (:edge assurance/rendered-surface-completeness-open-obligation)))
  (is (not= assurance/affordance-open-obligation
            assurance/rendered-surface-completeness-open-obligation)))

(deftest no-snapshot-warning-describes-unclosed-coverage
  (let [warnings (assurance/warnings nil nil)]
    (is (= 1 (count warnings)))
    (is (= :static-affordance-closure-not-yet-modeled
           (:kind (first warnings))))
    (is (= :rendered-affordance->semantic-operation
           (:edge (first warnings))))
    (is (string? (:message (first warnings))))))

(deftest empty-snapshot-is-still-a-supplied-snapshot
  (let [warnings (assurance/warnings {} {})]
    (is (= 1 (count warnings)))
    (is (= :rendered-surface-enumeration-completeness-not-yet-modeled
           (:kind (first warnings))))
    (is (= #{} (:surface-names (first warnings))))))

(deftest named-snapshot-warning-retains-exact-surface-set
  (let [warning (first (assurance/warnings surface-fixture nil))]
    (is (= #{:request-board :toolbar} (:surface-names warning)))
    (is (= :application->rendered-surfaces (:edge warning)))))

(deftest unhandled-publication-topics-get-a-separate-warning
  (let [unhandled {:request/claim #{:audit :notifications}}
        warnings (assurance/warnings surface-fixture unhandled)]
    (is (= [:rendered-surface-enumeration-completeness-not-yet-modeled
            :published-change-topics-not-consumed-by-live-app]
           (mapv :kind warnings)))
    (is (= unhandled (:topics-by-operation (second warnings))))
    (is (= #{:request-board :toolbar} (:surface-names (first warnings))))))

(deftest empty-unhandled-topics-do-not-invent-a-publication-warning
  (doseq [topics [nil {} [] #{}]]
    (is (= 1 (count (assurance/warnings nil topics))))))

(deftest no-supplied-surfaces-cannot-claim-affordance-closure
  (is (= {:status :not-modeled
          :guarantee nil
          :surface-count 0
          :affordance-count 0}
         (assurance/affordance-closure nil
                                       {:errors []} []
                                       {:resolution-errors []}
                                       affordances-fixture))))

(deftest valid-supplied-surfaces-have-scope-limited-guarantee
  (is (= successful-closure
         (assurance/affordance-closure surface-fixture
                                       {:errors []} []
                                       {:resolution-errors []}
                                       affordances-fixture)))
  (is (= :closed-relative-to-supplied-rendered-surfaces
         (:status successful-closure))))

(deftest scan-errors-withhold-affordance-guarantee
  (let [result (assurance/affordance-closure
                surface-fixture
                {:errors [{:kind :rendered-affordance-scan-failed}]}
                [] {:resolution-errors []} affordances-fixture)]
    (is (= :failed (:status result)))
    (is (nil? (:guarantee result)))
    (is (= 2 (:surface-count result)))))

(deftest anonymous-semantic-route-errors-withhold-affordance-guarantee
  (let [result (assurance/affordance-closure
                surface-fixture
                {:errors []}
                [{:kind :anonymous-semantic-route}]
                {:resolution-errors []}
                affordances-fixture)]
    (is (= :failed (:status result)))
    (is (nil? (:guarantee result)))
    (is (= 2 (:affordance-count result)))))

(deftest affordance-resolution-errors-withhold-guarantee
  (let [result (assurance/affordance-closure
                surface-fixture
                {:errors []} []
                {:resolution-errors [{:kind :unknown-operation}]}
                affordances-fixture)]
    (is (= :failed (:status result)))
    (is (nil? (:guarantee result)))))

(deftest no-error-channel-may-hide-another-channel
  (doseq [[scan-errors route-errors resolution-errors]
          [[[:bad-scan] [] []]
           [[] [:bad-route] []]
           [[] [] [:bad-resolution]]
           [[:bad-scan] [:bad-route] [:bad-resolution]]]]
    (is (= :failed
           (:status (assurance/affordance-closure
                     surface-fixture {:errors scan-errors}
                     route-errors {:resolution-errors resolution-errors}
                     []))))))

(deftest explanation-uses-only-given-operation-summary
  (let [result (assurance/explain-operation-from-report
                report-fixture :request/claim)]
    (is (= :request/claim (:operation result)))
    (is (= (:request/claim operations-fixture)
           (select-keys result [:command-route :published-change-topics])))
    (is (= assurance/backbone-guarantee (:guarantee result)))
    (is (= assurance/rendered-affordance-guarantee
           (:affordance-guarantee result)))
    (is (= assurance/canonical-html-enforcement
           (:canonical-html-enforcement result)))))

(deftest unknown-operation-is-rejected-with-available-identities
  (let [error (exception-info-of
               #(assurance/explain-operation-from-report report-fixture
                                                       :request/archive))]
    (is (instance? clojure.lang.ExceptionInfo error))
    (is (= :gesso.live.application-preflight/error
           (:error/type (ex-data error))))
    (is (= :unknown-operation (:error/kind (ex-data error))))
    (is (= :request/archive (:operation (ex-data error))))
    (is (= #{:request/claim :request/cancel}
           (:available-operations (ex-data error))))))

(deftest operation-explanation-reflects-missing-affordance-evidence
  (let [without-evidence (assoc-in report-fixture
                                   [:analysis :affordance-closure :guarantee]
                                   nil)
        result (assurance/explain-operation-from-report
                without-evidence :request/claim)]
    (is (nil? (:affordance-guarantee result)))
    (is (= assurance/backbone-guarantee (:guarantee result)))))

(deftest application-explanation-retains-reported-obligations
  (let [explanation (assurance/explain-application-from-report
                     assembly-fixture report-fixture)]
    (is (= assurance/application-assembly-type (:type explanation)))
    (is (= assurance/preflight-version (:version explanation)))
    (is (= :fixture/request-app (:name explanation)))
    (is (= operations-fixture (:operations explanation)))
    (is (= #{:request-board :toolbar} (:rendered-surface-names explanation)))
    (is (= successful-closure (:affordance-closure explanation)))
    (is (= (:open-obligations (:analysis report-fixture))
           (:open-obligations explanation)))
    (is (= assurance/trusted-assumptions (:trusted-assumptions explanation)))
    (is (= assurance/canonical-html-enforcement
           (:canonical-html-enforcement explanation)))
    (is (= assurance/explicit-escape-hatches
           (:explicit-escape-hatches explanation)))))

(deftest application-explanation-derives-live-name-from-acquisition-product
  (is (= :fixture/live
         (:live-app-name (assurance/explain-application-from-report
                           assembly-fixture report-fixture))))
  (is (= #{}
         (:rendered-surface-names
          (assurance/explain-application-from-report
           assembly-fixture
           (assoc-in report-fixture [:analysis :rendered-surfaces] nil))))))

(deftest browser-artifact-explanation-retains-receipt-evidence
  (let [explanation (assurance/explain-application-from-report
                     assembly-fixture report-fixture)]
    (is (= "resources/public/js/gesso-live.js"
           (get-in explanation [:browser-artifact :path])))
    (is (= "resources/public/js/gesso-live.js.receipt.edn"
           (get-in explanation [:browser-artifact :receipt-path])))
    (is (= {:sha256 "fixture-digest"}
           (get-in explanation [:browser-artifact :receipt])))))

(deftest default-browser-receipt-path-is-derived-not-invented
  (let [calls (atom [])
        assembly (dissoc assembly-fixture :browser-receipt-path)
        path "fixture/gesso-live.js"
        result (with-redefs [browser-build/receipt-path
                             (fn [received]
                               (swap! calls conj received)
                               "fixture/receipt.edn")]
                 (assurance/explain-application-from-report
                  (assoc assembly :browser-artifact-path path)
                  report-fixture))]
    (is (= [path] @calls))
    (is (= "fixture/receipt.edn"
           (get-in result [:browser-artifact :receipt-path])))))

(deftest explicit-receipt-path-does-not-call-default-deriver
  (let [calls (atom 0)
        result (with-redefs [browser-build/receipt-path
                             (fn [_]
                               (swap! calls inc)
                               (throw (ex-info "unexpected" {})))]
                 (assurance/explain-application-from-report
                  assembly-fixture report-fixture))]
    (is (= 0 @calls))
    (is (= (:browser-receipt-path assembly-fixture)
           (get-in result [:browser-artifact :receipt-path])))))

(deftest application-explanation-does-not-leak-private-assembly-data
  (let [explanation (assurance/explain-application-from-report
                     assembly-fixture report-fixture)]
    (is (not (contains? explanation :private-model)))
    (is (not (contains? explanation :operation-acquisition-assembly)))
    (is (not (contains? explanation :private-source-product)))
    (is (not= explanation assembly-fixture))))

(deftest report-explanation-exposes-a-closed-analysis-projection
  (let [explanation (assurance/explain-report-from-report report-fixture)]
    (is (= assurance/report-type (:type explanation)))
    (is (= assurance/preflight-version (:version explanation)))
    (is (= true (:valid? explanation)))
    (is (= (:errors report-fixture) (:errors explanation)))
    (is (= (:warnings report-fixture) (:warnings explanation)))
    (is (= #{:name :browser-artifact-path :browser-receipt-path
             :operations :rendered-surfaces :affordances
             :affordance-closure :open-obligations :trusted-assumptions}
           (set (keys (:analysis explanation)))))
    (is (not (contains? (:analysis explanation) :artifact-receipt)))
    (is (not (contains? (:analysis explanation) :private-source-product)))
    (is (not (contains? explanation :operation-acquisition-assembly)))))

(deftest report-explanation-retains-failed-status-and-errors
  (let [errors [{:kind :bad-authoritative-operation :details :fixture}]
        report (assoc report-fixture :valid? false :errors errors)
        explanation (assurance/explain-report-from-report report)]
    (is (false? (:valid? explanation)))
    (is (= errors (:errors explanation)))
    (is (= assurance/canonical-html-enforcement
           (:canonical-html-enforcement explanation)))
    (is (= assurance/explicit-escape-hatches
           (:explicit-escape-hatches explanation)))))

(deftest reporting-is-deterministic-and-does-not-mutate-input
  (let [assembly-before assembly-fixture
        report-before report-fixture
        warnings-before surface-fixture
        one (assurance/explain-application-from-report assembly-fixture report-fixture)
        two (assurance/explain-application-from-report assembly-fixture report-fixture)]
    (is (= one two))
    (is (= (assurance/explain-report-from-report report-fixture)
           (assurance/explain-report-from-report report-fixture)))
    (is (= (assurance/warnings surface-fixture {:request/claim #{:audit}})
           (assurance/warnings surface-fixture {:request/claim #{:audit}})))
    (is (= assembly-before assembly-fixture))
    (is (= report-before report-fixture))
    (is (= warnings-before surface-fixture))))
