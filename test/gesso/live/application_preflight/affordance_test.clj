(ns gesso.live.application-preflight.affordance-test
  "Direct regression tests for Choreo affordance/physical-route correspondence.

   The module consumes already-scanned physical request coordinates and
   declared Choreo affordances. It neither parses Hiccup nor proves that an
   application's operation declarations or routes are authoritative. Tests
   exercise its actual contract without constructing a whole application."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.application-preflight.affordance :as affordance]))

(def ^:private claim-route
  {:route-id :route/claim
   :method :post
   :path "/requests/:request-id/claim"
   :required-transport :htmx})

(def ^:private cancel-route
  {:route-id :route/cancel
   :method :post
   :path "/requests/:request-id/cancel"
   :required-transport :htmx})

(def ^:private view-route
  {:route-id :route/view
   :method :get
   :path "/requests/:request-id"
   :required-transport :htmx})

(def ^:private operations
  (sorted-map
   :request/cancel {:browser-plan-key :plan/cancel
                    :routes [cancel-route]}
   :request/claim {:browser-plan-key :plan/claim
                   :routes [claim-route]}
   :request/view {:browser-plan-key :plan/view
                  :routes [view-route]}))

(def ^:private canonical-claim
  {:surface :board
   :render-path [2 1]
   :operation :request/claim
   :plan-key :plan/claim
   :method :post
   :path "/requests/42/claim"})

(def ^:private marked-claim-request
  {:surface :board
   :render-path [2 1]
   :method :post
   :path "/requests/42/claim"
   :request-source :hx-post
   :choreo-operation-declared? true})

(defn- match-route
  [operation-summary method path]
  (affordance/semantic-route-matches
   operation-summary {:method method :path path}))

(defn- identity-errors
  [coordinates affordances]
  (affordance/semantic-route-identity-errors operations coordinates affordances))

(defn- resolve-one
  [operation-summary proposed]
  (affordance/resolve-rendered-affordances operation-summary [proposed]))

(deftest semantic-routes-resolve-physical-method-and-template
  (is (= [{:operation :request/claim
           :route-id :route/claim
           :method :post
           :route-template "/requests/:request-id/claim"
           :required-transport :htmx}]
         (match-route operations :post "/requests/42/claim")))
  (is (= [:route/view]
         (mapv :route-id (match-route operations :get "/requests/42"))))
  (is (= [:route/cancel]
         (mapv :route-id (match-route operations :post "/requests/42/cancel")))))

(deftest semantic-route-suffixes-do-not-alter-selection
  (doseq [path ["/requests/42/claim?source=board"
                "/requests/42/claim#selected"
                "/requests/42/claim?source=board#selected"]]
    (testing path
      (is (= [:route/claim]
             (mapv :route-id (match-route operations :post path))))))
  (is (= [:route/view]
         (mapv :route-id (match-route operations :get "/requests/42?sort=created")))))

(deftest semantic-route-match-is-strict-about-physical-method
  (doseq [method [:get :put :patch :delete]]
    (testing (name method)
      (is (empty? (match-route operations method "/requests/42/claim")))))
  (is (empty? (match-route operations :post "/requests/42"))))

(deftest semantic-route-placeholders-require-exact-nonempty-segments
  (doseq [path ["/requests//claim"
                "/requests/42/claim/"
                "/requests/42/claim/more"
                "/requests/42"
                "/request/42/claim"
                "/REQUESTS/42/claim"]]
    (testing path
      (is (empty? (match-route operations :post path)))))
  (is (= [:route/claim]
         (mapv :route-id
               (match-route operations :post "/requests/abc-123/claim")))))

(deftest semantic-route-template-vocabulary-does-not-invent-router-features
  (let [summary
        {:request/example
         {:routes [{:route-id :literal-star
                    :method :get :path "/files/*"}
                   {:route-id :literal-colon
                    :method :get :path "/files/:"}
                   {:route-id :parameter
                    :method :get :path "/files/:file"}]}}]
    (is (= [:parameter]
           (mapv :route-id (match-route summary :get "/files/report"))))
    (is (= [:literal-star :parameter]
           (mapv :route-id (match-route summary :get "/files/*"))))
    (is (= [:literal-colon :parameter]
           (mapv :route-id (match-route summary :get "/files/:"))))
    (is (empty? (match-route summary :get "/files/report/extra")))))

(deftest overlapping-semantic-routes-are-all-reported
  (let [summary
        (sorted-map
         :a/claim {:routes [{:route-id :a :method :post
                             :path "/requests/:id/claim"}]}
         :b/claim {:routes [{:route-id :b :method :post
                             :path "/requests/:request-id/claim"}]})
        matching (match-route summary :post "/requests/42/claim")]
    (is (= [:a/claim :b/claim] (mapv :operation matching)))
    (is (= [:a :b] (mapv :route-id matching)))
    (is (= #{"/requests/:id/claim" "/requests/:request-id/claim"}
           (set (map :route-template matching))))))

(deftest anonymous-semantic-route-request-is-rejected-with-location-and-candidates
  (let [coordinate
        {:surface :request-card
         :render-path [3 2]
         :method :post
         :path "/requests/42/claim?origin=board"
         :request-source :native-submitter
         :form-render-path [3]
         :submitter-overrides #{:formmethod :formaction}
         :choreo-operation-declared? false}
        errors (identity-errors [coordinate] [])
        error (first errors)]
    (is (= 1 (count errors)))
    (is (= :rendered-semantic-route-without-choreo-operation (:kind error)))
    (is (= :request-card (:surface error)))
    (is (= [3 2] (:render-path error)))
    (is (= :post (:method error)))
    (is (= "/requests/42/claim?origin=board" (:path error)))
    (is (= :native-submitter (:request-source error)))
    (is (= [3] (:form-render-path error)))
    (is (= #{:formmethod :formaction} (:submitter-overrides error)))
    (is (= #{:request/claim} (:candidate-operations error)))
    (is (= [:route/claim] (mapv :route-id (:matching-routes error))))
    (is (string? (:message error)))))

(deftest anonymous-unrelated-requests-are-permitted
  (let [coordinates
        [{:surface :board :render-path [1] :method :post
          :path "/session/login" :request-source :hx-post}
         {:surface :board :render-path [2] :method :get
          :path "/requests/42/claim" :request-source :hx-get}
         {:surface :board :render-path [3] :method :post
          :path "/requests/42/other" :request-source :native-form}]]
    (is (empty? (identity-errors coordinates [])))
    (is (= 0 (count (identity-errors [] []))))))

(deftest exact-canonical-request-may-use-its-choreo-mark
  (is (empty? (identity-errors [marked-claim-request] [canonical-claim])))
  (is (empty? (identity-errors
               [(assoc marked-claim-request :surface :details
                       :render-path [9])]
               [(assoc canonical-claim :surface :details
                       :render-path [9])]))))

(deftest canonical-mark-cannot-exempt-additional-or-altered-requests
  (let [cases
        [{:change {:method :get :request-source :hx-get}
          :expected-matches []}
         {:change {:method :delete :path "/unrelated"
                   :request-source :hx-delete}
          :expected-matches []}
         {:change {:path "/requests/42/cancel"}
          :expected-matches [:route/cancel]}
         {:change {:path "/requests/42/claim?extra=1"}
          :expected-matches [:route/claim]}
         {:change {:request-source :native-form}
          :expected-matches [:route/claim]}]]
    (doseq [{:keys [change expected-matches]} cases]
      (testing (pr-str change)
        (let [error (first (identity-errors
                            [(merge marked-claim-request change)]
                            [canonical-claim]))]
          (is (= :rendered-choreo-physical-request-mismatch (:kind error)))
          (is (= change
                 (select-keys error (keys change))))
          (is (= [canonical-claim] (:canonical-affordances error)))
          (is (= expected-matches
                 (mapv :route-id (:matching-routes error)))))))))

(deftest canonical-mark-is-specific-to-surface-and-occurrence
  (let [wrong-surface
        (assoc canonical-claim :surface :details)
        wrong-location
        (assoc canonical-claim :render-path [9])]
    (is (= :rendered-choreo-physical-request-mismatch
           (:kind (first (identity-errors [marked-claim-request]
                                          [wrong-surface])))))
    (is (= :rendered-choreo-physical-request-mismatch
           (:kind (first (identity-errors [marked-claim-request]
                                          [wrong-location])))))
    (is (= :rendered-choreo-physical-request-mismatch
           (:kind (first (identity-errors
                          [marked-claim-request]
                          [canonical-claim canonical-claim])))))))

(deftest multiple-canonical-and-anonymous-coordinates-remain-independent
  (let [other
        (assoc canonical-claim
               :surface :details :render-path [1]
               :path "/requests/7/claim")
        requests
        [marked-claim-request
         (assoc marked-claim-request :surface :details :render-path [1]
                :path "/requests/7/claim")
         {:surface :board :render-path [8] :method :post
          :path "/requests/9/claim" :request-source :hx-post
          :choreo-operation-declared? false}]
        errors (identity-errors requests [canonical-claim other])]
    (is (= 1 (count errors)))
    (is (= :rendered-semantic-route-without-choreo-operation
           (:kind (first errors))))
    (is (= [8] (:render-path (first errors))))))

(deftest semantic-route-enrichment-preserves-existing-scan-error
  (let [scan-error
        {:kind :rendered-affordance-scan-failed
         :message "Not valid Choreo metadata"
         :surface :board :render-path [99]
         :cause-data {:type :missing-plan}}
        coordinates
        [{:surface :board :render-path [3] :method :get
          :path "/requests/42"}
         {:surface :details :render-path [1] :method :post
          :path "/requests/7/cancel"}
         {:surface :board :render-path [1] :method :post
          :path "/requests/42/claim"}
         {:surface :board :render-path [4] :method :get
          :path "/ordinary"}]
        enriched
        (affordance/enrich-render-scan-errors-with-semantic-routes
         operations coordinates [scan-error])
        error (first enriched)
        collisions (:matching-semantic-routes error)]
    (is (= 1 (count enriched)))
    (is (= (dissoc scan-error :matching-semantic-routes)
           (dissoc error :matching-semantic-routes)))
    (is (= [:route/claim :route/view]
           (mapv :route-id collisions)))
    (is (= [[1] [3]] (mapv :render-path collisions)))
    (is (= [:post :get] (mapv :rendered-method collisions)))
    (is (= ["/requests/42/claim" "/requests/42"]
           (mapv :rendered-path collisions)))
    (is (every? #(= :board (:surface %)) collisions))
    (is (every? #(contains? % :operation) collisions))))

(deftest semantic-route-enrichment-leaves-unrelated-surfaces-unchanged
  (let [errors [{:kind :bad :surface :search :render-path [2]}
                {:kind :bad :surface :board :render-path [3]}]
        coordinates [{:surface :board :render-path [4] :method :post
                      :path "/requests/42/claim"}]
        found (affordance/enrich-render-scan-errors-with-semantic-routes
               operations coordinates errors)]
    (is (= (first errors) (first found)))
    (is (= [:route/claim]
           (mapv :route-id (:matching-semantic-routes (second found)))))
    (is (= errors
           (affordance/enrich-render-scan-errors-with-semantic-routes
            operations [] errors)))))

(deftest unknown-rendered-operation-is-not-silently-dropped
  (let [unknown (assoc canonical-claim :operation :request/unknown)
        result (resolve-one operations unknown)
        error (first (:errors result))]
    (is (= [unknown] (:affordances result)))
    (is (= :rendered-affordance-unknown-operation (:kind error)))
    (is (= :request/unknown (:operation error)))
    (is (= (set (keys operations)) (:available-operations error)))
    (is (= :board (:surface error)))
    (is (= [2 1] (:render-path error)))))

(deftest mismatched-browser-plan-is-rejected-before-route-checks
  (let [invalid (assoc canonical-claim
                       :plan-key :plan/forged
                       :method :delete :path "/not-a-route")
        result (resolve-one operations invalid)
        error (first (:errors result))]
    (is (= [invalid] (:affordances result)))
    (is (= :rendered-affordance-plan-mismatch (:kind error)))
    (is (= :plan/forged (:affordance-plan-key error)))
    (is (= :plan/claim (:assembled-plan-key error)))
    (is (= :request/claim (:operation error)))
    (is (= 1 (count (:errors result))))))

(deftest mismatched-method-is-rejected-before-path-checks
  (let [invalid (assoc canonical-claim :method :get :path "/other")
        result (resolve-one operations invalid)
        error (first (:errors result))]
    (is (= [invalid] (:affordances result)))
    (is (= :rendered-affordance-method-mismatch (:kind error)))
    (is (= :get (:method error)))
    (is (= [claim-route] (:declared-routes error)))
    (is (= 1 (count (:errors result))))))

(deftest mismatched-route-path-is-rejected-with-method-specific-templates
  (let [invalid (assoc canonical-claim :path "/requests/42/transfer")
        result (resolve-one operations invalid)
        error (first (:errors result))]
    (is (= [invalid] (:affordances result)))
    (is (= :rendered-affordance-path-mismatch (:kind error)))
    (is (= "/requests/42/transfer" (:path error)))
    (is (= ["/requests/:request-id/claim"] (:route-templates error)))
    (is (= :post (:method error)))))

(deftest ambiguous-route-realizations-are-rejected-instead-of-chosen
  (let [other-route (assoc claim-route
                           :route-id :route/claim-duplicate
                           :path "/requests/:id/claim")
        summary (assoc-in operations [:request/claim :routes]
                          [claim-route other-route])
        result (resolve-one summary canonical-claim)
        error (first (:errors result))]
    (is (= [canonical-claim] (:affordances result)))
    (is (= :ambiguous-rendered-affordance-route (:kind error)))
    (is (= :request/claim (:operation error)))
    (is (= [claim-route other-route] (:matching-routes error)))
    (is (= 1 (count (:errors result))))))

(deftest successfully-resolved-affordance-carries-trusted-route-fields
  (let [result (resolve-one operations canonical-claim)
        resolved (first (:affordances result))]
    (is (empty? (:errors result)))
    (is (= 1 (count (:affordances result))))
    (is (= (assoc canonical-claim
                  :route-id :route/claim
                  :route-template "/requests/:request-id/claim"
                  :required-transport :htmx)
           resolved))
    (is (= :plan/claim (:plan-key resolved)))
    (is (= "/requests/42/claim" (:path resolved)))))

(deftest mixed-affordance-resolution-preserves-input-order-and-errors
  (let [valid-view {:surface :details :render-path [4]
                    :operation :request/view :plan-key :plan/view
                    :method :get :path "/requests/42"}
        wrong-plan (assoc canonical-claim :plan-key :plan/not-claim)
        proposals [canonical-claim wrong-plan valid-view]
        result (affordance/resolve-rendered-affordances operations proposals)]
    (is (= 3 (count (:affordances result))))
    (is (= (mapv :operation proposals)
           (mapv :operation (:affordances result))))
    (is (= [true false true]
           (mapv #(contains? % :route-id) (:affordances result))))
    (is (= [:rendered-affordance-plan-mismatch]
           (mapv :kind (:errors result))))
    (is (= [2 1] (:render-path (first (:errors result)))))))

(deftest operation-summary-enrichment-groups-affordances-by-operation
  (let [claim-2 (assoc canonical-claim :surface :details :render-path [7]
                       :path "/requests/7/claim")
        cancel (assoc canonical-claim
                      :operation :request/cancel :plan-key :plan/cancel
                      :path "/requests/9/cancel")
        enriched
        (affordance/enrich-operation-summary-with-affordances
         operations [claim-2 cancel canonical-claim])]
    (is (= (keys operations) (keys enriched)))
    (is (= [claim-2 canonical-claim]
           (get-in enriched [:request/claim :rendered-affordances])))
    (is (= [cancel]
           (get-in enriched [:request/cancel :rendered-affordances])))
    (is (= []
           (get-in enriched [:request/view :rendered-affordances])))
    (is (= :plan/view
           (get-in enriched [:request/view :browser-plan-key])))
    (is (instance? clojure.lang.Sorted enriched))))

(deftest operation-summary-enrichment-does-not-admit-unknown-operations
  (let [unknown (assoc canonical-claim :operation :not/assembled)
        enriched (affordance/enrich-operation-summary-with-affordances
                  operations [unknown])]
    (is (= (keys operations) (keys enriched)))
    (is (not (contains? enriched :not/assembled)))
    (is (every? (comp empty? :rendered-affordances val) enriched))
    (is (= [] (:rendered-affordances
               (:request/claim
                (affordance/enrich-operation-summary-with-affordances
                 operations [])))))))
