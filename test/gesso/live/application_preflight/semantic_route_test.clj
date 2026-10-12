(ns gesso.live.application-preflight.semantic-route-test
  "Application-level regression tests for semantic HTTP route identity. These
   tests are migrated verbatim from application_preflight_test.clj v752;
   the shared fixture owns their application and artifact construction."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.fixture
    :refer [custom-route-capabilities error-data error-kinds
            render-context rendered-operation-button standard-operations
            with-closed-application with-surfaced-application]]))

;; =============================================================================
;; v627 rendered-surface -> affordance -> trusted-route closure
;; =============================================================================


;; =============================================================================
;; Rendered-surface integration (shared fixture construction)
;; =============================================================================

(deftest anonymous-post-to-parameterized-semantic-route-is-rejected-before-application-assembly
  (let [operations
        {:request/claim
         (get (standard-operations) :request/claim)}
        routes
        (custom-route-capabilities
         {:humanhelp/claim
          {:operation :request/claim
           :path "/app/requests/:request-id/claim"
           :transports #{:htmx}}})
        concrete-path
        "/app/requests/01a0702a-302d-7604-a880-7b421af742a0/claim"
        surfaces
        {:request-board
         [:button {:hx-post concrete-path} "Claim"]}]
    (with-surfaced-application
      {:operations operations
       :route-capabilities routes
       :rendered-surfaces surfaces}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (not (application/valid? report)))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (let [error (first (:errors report))]
          (is (= :request-board (:surface error)))
          (is (= [] (:render-path error)))
          (is (= :post (:method error)))
          (is (= concrete-path (:path error)))
          (is (= #{:request/claim} (:candidate-operations error)))
          (is (= [{:operation :request/claim
                   :route-id :humanhelp/claim
                   :method :post
                   :route-template "/app/requests/:request-id/claim"
                   :required-transport :htmx}]
                 (:matching-routes error))))))))

(deftest dynamic-pre-browser-boundary-rejects-silent-semantic-route-downgrade-before-rendering
  (let [rendered
        [:main
         [:button {:hx-post "/operations/request/claim"} "Claim"]]
        calls (atom [])]
    (with-closed-application
      (fn [{:keys [assembly]}]
        (let [report
              (application/check-rendered-surface
               assembly
               :request-board
               rendered)
              data
              (error-data
               #(application/checked-rendered-response!
                 assembly
                 :request-board
                 (fn [node]
                   (swap! calls conj node)
                   {:status 200 :body node})
                 rendered))]
          (is (application/rendered-surface-report? report))
          (is (not (application/rendered-surface-valid? report)))
          (is (= #{:rendered-semantic-route-without-choreo-operation}
                 (error-kinds report)))
          (is (= :rendered-surface-preflight-failed (:error/kind data)))
          (is (= #{:rendered-semantic-route-without-choreo-operation}
                 (error-kinds (:preflight data))))
          (is (empty? @calls)))))))

(deftest semantic-route-identity-guard-does-not-ban-ordinary-htmx-or-canonical-choreo-affordances
  (let [operations (standard-operations)
        ctx (render-context operations)
        ordinary
        [:main
         [:button {:hx-post "/ordinary/save"} "Save"]]
        canonical
        [:main
         (rendered-operation-button
          ctx
          :request/claim
          "/operations/request/claim")]]
    (with-closed-application
      (fn [{:keys [assembly]}]
        (let [ordinary-report
              (application/check-rendered-surface
               assembly
               :ordinary-form
               ordinary)
              canonical-report
              (application/check-rendered-surface
               assembly
               :request-board
               canonical)]
          (is (application/rendered-surface-valid? ordinary-report))
          (is (empty? (:errors ordinary-report)))
          (is (application/rendered-surface-valid? canonical-report))
          (is (empty? (:errors canonical-report)))
          (is (= :request/claim
                 (get-in canonical-report
                         [:analysis :affordances 0 :operation]))))))))

(deftest anonymous-post-does-not-collide-with-semantic-route-of-a-different-http-method
  (let [operations
        {:request/claim
         (get (standard-operations) :request/claim)}
        routes
        (custom-route-capabilities
         {:request/claim-put
          {:operation :request/claim
           :method :put
           :path "/requests/:request-id/claim"
           :transports #{:htmx}}})]
    (with-surfaced-application
      {:operations operations
       :route-capabilities routes
       :rendered-surfaces
       {:request-board
        [:button {:hx-post "/requests/request-1/claim"} "Ordinary POST"]}}
      (fn [{:keys [report assembly]}]
        (is (application/valid? report))
        (is (application/application-assembly? assembly))
        (is (empty? (:errors report)))))))

;; =============================================================================
;; v717 adversarial regression: explicit non-Choreo semantic route invocations
;; =============================================================================
;; These checks exercise the public assembly and response boundaries, rather
;; than asserting only the private Hiccup scanner's behavior. They cover the
;; explicit request coordinates enumerated by v716. Implicit native form actions
;; and JavaScript request construction remain separate unclosed cases; neither
;; is certified by these tests. Submitter overrides are covered below (v719).

(deftest anonymous-htmx-methods-cannot-invoke-matching-semantic-routes
  (let [methods [:get :post :put :patch :delete]
        path "/requests/request-42/claim?from=board#pending"
        operations
        {:request/claim (get (standard-operations) :request/claim)}
        routes
        (custom-route-capabilities
         (into {}
               (map
                (fn [method]
                  [(keyword "fixture" (str "claim-" (name method)))
                   {:operation :request/claim
                    :method method
                    :path "/requests/:request-id/claim"
                    :transports #{:htmx}}])
                methods)))
        rendered
        (into [:main]
              (map
               (fn [method]
                 [:button {(keyword (str "hx-" (name method))) path}
                  (name method)])
               methods))]
    (with-surfaced-application
      {:operations operations
       :route-capabilities routes
       :rendered-surfaces {:request-board rendered}}
      (fn [{:keys [report assembly]}]
        (is (application/report? report))
        (is (nil? assembly))
        (is (not (application/valid? report)))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (is (= (count methods) (count (:errors report))))
        (is (= (mapv
                (fn [index method]
                  {:surface :request-board
                   :render-path [index]
                   :method method
                   :path path
                   :request-source (keyword (str "hx-" (name method)))
                   :candidate-operations #{:request/claim}})
                (range 1 (inc (count methods)))
                methods)
               (mapv
                #(select-keys % [:surface :render-path :method :path
                                 :request-source :candidate-operations])
                (:errors report))))
        (is (= (set methods)
               (set (map (comp :method first :matching-routes)
                         (:errors report)))))))))

(deftest anonymous-native-post-forms-are-checked-with-nested-hiccup-tags
  (let [claim "/operations/request/claim"
        queried (str claim "?from=board#confirm")
        rendered
        [:main
         [:form#claim.panel {:method "PoSt" :action claim}
          [:button {:type "submit"} "Claim"]]
         [:section
          ["form" {:method :POST :action queried}
           [:input {:type "submit" :value "Claim again"}]]]
         [:form {:method :post :action "/unrelated/save"}
          [:button {:type "submit"} "Save"]]
         [:form {:method :get :action claim}
          [:button {:type "submit"} "Read"]]
         [:form {:method "POST"}
          [:button {:type "submit"} "Implicit action"]]]]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (application/report? report))
        (is (nil? assembly))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (is (= [{:surface :request-board
                 :render-path [1]
                 :method :post
                 :path claim
                 :request-source :native-form}
                {:surface :request-board
                 :render-path [2 1]
                 :method :post
                 :path queried
                 :request-source :native-form}]
               (mapv
                #(select-keys % [:surface :render-path :method :path
                                 :request-source])
                (:errors report))))
        (is (every? #(= #{:request/claim} (:candidate-operations %))
                    (:errors report)))))))

(deftest anonymous-physical-method-is-matched-not-just-the-semantic-url
  (let [operations
        {:request/claim (get (standard-operations) :request/claim)}
        routes
        (custom-route-capabilities
         {:fixture/claim-put
          {:operation :request/claim
           :method :put
           :path "/requests/:request-id/claim"
           :transports #{:htmx}}})
        rendered
        [:main
         [:button {:hx-post "/requests/42/claim"} "Nonmatching POST"]
         [:form {:method "POST" :action "/requests/42/claim"}
          [:button {:type "submit"} "Nonmatching form"]]
         [:button {:hx-get "/requests/42/claim"} "Nonmatching GET"]
         [:button {:hx-put "/requests/42/claim"} "Matching PUT"]]]
    (with-surfaced-application
      {:operations operations
       :route-capabilities routes
       :rendered-surfaces {:request-board rendered}}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (is (= 1 (count (:errors report))))
        (is (= {:method :put
                :request-source :hx-put
                :render-path [4]
                :candidate-operations #{:request/claim}}
               (select-keys (first (:errors report))
                            [:method :request-source :render-path
                             :candidate-operations])))))))

(deftest native-post-and-anonymous-htmx-collisions-block-renderer
  (with-closed-application
    (fn [{:keys [assembly]}]
      (doseq [[case-name rendered expected-source]
              [[:native-post
                [:form {:method "POST" :action "/operations/request/claim"}
                 [:button {:type "submit"} "Claim"]]
                :native-form]
               [:anonymous-htmx
                [:button {:hx-post "/operations/request/claim"} "Claim"]
                :hx-post]]]
        (testing (name case-name)
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
            (is (= expected-source (:request-source (first (:errors report)))))
            (is (= :rendered-surface-preflight-failed (:error/kind data)))
            (is (= #{:rendered-semantic-route-without-choreo-operation}
                   (error-kinds (:preflight data))))
            (is (zero? @calls))))))))

(deftest ordinary-physical-requests-and-canonical-choreo-remain-admitted
  (let [ctx (render-context (standard-operations))
        canonical (rendered-operation-button
                   ctx :request/claim "/operations/request/claim")
        rendered
        [:main
         canonical
         [:button {:hx-get "/ordinary/list"} "List"]
         [:button {:hx-post "/ordinary/save"} "Save"]
         [:button {:hx-put "/ordinary/replace"} "Replace"]
         [:button {:hx-patch "/ordinary/edit"} "Edit"]
         [:button {:hx-delete "/ordinary/remove"} "Remove"]
         [:form {:method :post :action "/ordinary/form"}
          [:button {:type "submit"} "Submit"]]
         [:form {:method "GET" :action "/operations/request/claim"}
          [:button {:type "submit"} "Nonmatching native GET"]]]]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (application/valid? report))
        (is (application/application-assembly? assembly))
        (is (empty? (:errors report)))
        (is (= :closed-relative-to-supplied-rendered-surfaces
               (get-in report [:analysis :affordance-closure :status])))
        (is (= [:request/claim]
               (mapv :operation (get-in report [:analysis :affordances]))))))))
