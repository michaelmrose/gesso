(ns gesso.live.application-preflight.submitter-override-test
  "Application-level regression tests for native HTML submitter overrides.
   These tests are migrated verbatim from application_preflight_test.clj v754;
   the shared fixture owns assembly and browser artifact construction."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.fixture
    :refer [custom-route-capabilities error-data error-kinds
            render-context rendered-operation-button standard-operations
            with-closed-application with-surfaced-application]]))

;; =============================================================================
;; v719 adversarial regression: HTML submitter overrides (v718 implementation)
;; =============================================================================
;; These test actual application assembly and the checked response boundary. The
;; source derives the effective method/action from the owning HTML form and the
;; submitter's formaction/formmethod. These tests deliberately do not claim that
;; implicit document URLs, JavaScript submissions, or unsupplied DOM are checked.

(deftest anonymous-inline-submitters-cannot-override-to-semantic-post-route
  (let [claim "/operations/request/claim"
        queried (str claim "?from=inline#after")
        rendered
        [:main
         [:form {:method "POST" :action "/ordinary/save"}
          [:button {:formaction claim} "Default submit button"]]
         [:form {:method :get :action "/ordinary/view"}
          [:input {:type "submit" :formmethod "PoSt" :formaction queried}]]
         [:form {:method "POST" :action "/ordinary/save"}
          [:input {:type "image" :formaction claim}]]
         [:form {:method "GET" :action claim}
          [:button {:formmethod :post} "Inherited action"]]]]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (application/report? report))
        (is (nil? assembly))
        (is (not (application/valid? report)))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (is (= [{:method :post
                 :path claim
                 :render-path [1 2]
                 :form-render-path [1]
                 :submitter-overrides #{:formaction}}
                {:method :post
                 :path queried
                 :render-path [2 2]
                 :form-render-path [2]
                 :submitter-overrides #{:formaction :formmethod}}
                {:method :post
                 :path claim
                 :render-path [3 2]
                 :form-render-path [3]
                 :submitter-overrides #{:formaction}}
                {:method :post
                 :path claim
                 :render-path [4 2]
                 :form-render-path [4]
                 :submitter-overrides #{:formmethod}}]
               (mapv #(select-keys % [:method :path :render-path
                                       :form-render-path :submitter-overrides])
                     (:errors report))))
        (is (every? #(= :native-submitter (:request-source %))
                    (:errors report)))
        (is (every? #(= #{:request/claim} (:candidate-operations %))
                    (:errors report)))))))

(deftest detached-submitters-resolve-explicit-form-owner-not-dom-parent
  (let [claim "/operations/request/claim"
        rendered
        [:main
         [:form#claim-form {:method "GET" :action claim}]
         [:form {:id "ordinary-form" :method "POST" :action "/ordinary/save"}]
         [:form {:id "other-parent" :method "GET" :action "/ordinary/parent"}
          [:button {:form "claim-form" :formmethod "POST"}
           "Use external owner"]]
         [:input {:type "submit" :form "ordinary-form"
                  :formaction claim :value "Detached submit"}]]]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (is (= [{:request-source :native-submitter
                 :method :post
                 :path claim
                 :render-path [3 2]
                 :form-render-path [1]
                 :submitter-overrides #{:formmethod}}
                {:request-source :native-submitter
                 :method :post
                 :path claim
                 :render-path [4]
                 :form-render-path [2]
                 :submitter-overrides #{:formaction}}]
               (mapv #(select-keys % [:request-source :method :path
                                       :render-path :form-render-path
                                       :submitter-overrides])
                     (:errors report))))))))

(deftest submitter-check-is-method-sensitive-for-parameterized-semantic-routes
  (let [operations
        {:request/claim (get (standard-operations) :request/claim)}
        routes
        (custom-route-capabilities
         {:fixture/claim-get
          {:operation :request/claim
           :method :get
           :path "/requests/:request-id/claim"
           :transports #{:htmx}}})
        concrete "/requests/request-42/claim?from=submitter#section"
        rendered
        [:main
         [:form {:method :post :action "/ordinary/save"}
          [:button {:formaction concrete :formmethod "GET"} "Matching GET"]]
         [:form {:method :post :action "/ordinary/save"}
          [:button {:formaction concrete} "Nonmatching POST"]]
         [:form {:method :get :action "/ordinary/read"}
          [:button {:formaction concrete :formmethod "dialog"} "Dialog"]]]]
    (with-surfaced-application
      {:operations operations
       :route-capabilities routes
       :rendered-surfaces {:request-board rendered}}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (is (= 1 (count (:errors report))))
        (is (= {:request-source :native-submitter
                :method :get
                :path concrete
                :render-path [1 2]
                :form-render-path [1]
                :submitter-overrides #{:formaction :formmethod}
                :candidate-operations #{:request/claim}}
               (select-keys (first (:errors report))
                            [:request-source :method :path :render-path
                             :form-render-path :submitter-overrides
                             :candidate-operations])))))))

(deftest non-submitting-controls-and-unowned-submitters-remain-admitted
  (let [claim "/operations/request/claim"
        rendered
        [:main
         [:form {:method :post :action "/ordinary/save"}
          [:button {:type "button" :formaction claim} "Ordinary button"]
          [:button {:type :reset :formaction claim} "Reset"]
          [:input {:type "button" :formaction claim}]
          [:input {:type "text" :formaction claim}]
          [:button {:type "submit" :formaction "/ordinary/submit"} "Submit"]]
         [:form {:method :get :action "/ordinary/lookup"}
          [:button {:type "submit" :formaction claim} "GET only"]]
         [:form {:method :dialog :action claim}
          [:button {:formmethod :dialog} "Dialog only"]]
         [:button {:type :submit :form "missing" :formaction claim}
          "Unknown form owner"]
         [:input {:type "submit" :formaction claim}]
         [:button {:hx-post "/ordinary/htmx"} "Ordinary HTMX"]]]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (application/valid? report))
        (is (application/application-assembly? assembly))
        (is (empty? (:errors report)))))))

(deftest submitter-override-blocks-dynamic-renderer-before-html-delivery
  (with-closed-application
    (fn [{:keys [assembly]}]
      (doseq [[scenario rendered expected-overrides]
              [[:action-override
                [:form {:method :post :action "/ordinary/save"}
                 [:button {:formaction "/operations/request/claim"} "Claim"]]
                #{:formaction}]
               [:method-override
                [:form {:method :get :action "/operations/request/claim"}
                 [:button {:formmethod :post} "Claim"]]
                #{:formmethod}]
               [:detached-override
                [:main
                 [:form#claim {:method :get
                               :action "/operations/request/claim"}]
                 [:input {:type "submit" :form "claim" :formmethod :post}]]
                #{:formmethod}]]]
        (testing (name scenario)
          (let [calls (atom 0)
                report (application/check-rendered-surface
                        assembly :request-board rendered)
                data (error-data
                      #(application/checked-rendered-response!
                        assembly :request-board
                        (fn [_]
                          (swap! calls inc)
                          {:status 200})
                        rendered))]
            (is (application/rendered-surface-report? report))
            (is (not (application/rendered-surface-valid? report)))
            (is (= #{:rendered-semantic-route-without-choreo-operation}
                   (error-kinds report)))
            (is (= :native-submitter
                   (:request-source (first (:errors report)))))
            (is (= expected-overrides
                   (:submitter-overrides (first (:errors report)))))
            (is (= :rendered-surface-preflight-failed (:error/kind data)))
            (is (= #{:rendered-semantic-route-without-choreo-operation}
                   (error-kinds (:preflight data))))
            (is (zero? @calls))))))))

(deftest canonical-choreo-and-ordinary-submitters-coexist-on-valid-surface
  (let [ctx (render-context (standard-operations))
        canonical (rendered-operation-button
                   ctx :request/claim "/operations/request/claim")
        rendered
        [:main
         canonical
         [:form {:id "ordinary" :method :post :action "/ordinary/save"}
          [:button {:formaction "/ordinary/alternate"} "Alternate save"]
          [:input {:type "submit" :formmethod :get
                   :formaction "/operations/request/claim"}]]
         [:button {:form "ordinary" :formaction "/ordinary/detached"}
          "Detached save"]]]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (application/valid? report))
        (is (application/application-assembly? assembly))
        (is (empty? (:errors report)))
        (is (= [:request/claim]
               (mapv :operation (get-in report [:analysis :affordances]))))))))

