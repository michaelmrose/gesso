(ns gesso.live.application-preflight.physical-request-integration-test
  "Application-level regressions for physical Hiccup requests and exact
   canonical Choreo request correspondence. Migrated from application_preflight_test.clj
   v756; the occurrence-local test is made top-level without changing assertions."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.fixture
    :refer [custom-route-capabilities error-data error-kinds
            render-context rendered-operation-button standard-operations
            with-closed-application with-surfaced-application]]))

;; =============================================================================
;; v721 adversarial regression: Hiccup forms without attrs and HTML button types
;; =============================================================================
;; v720 closes two scanner omissions: a form ID supplied through Hiccup #id
;; shorthand must be usable as a detached submitter's owner even if the form
;; has no attrs map; and invalid button type values have HTML's Submit default.
;; These checks use application assembly and the checked rendered boundary.
;; They do not claim to inspect implicit document actions or unsupplied DOM.

(deftest unadorned-hiccup-form-ids-are-resolved-for-detached-submitters
  (let [claim "/operations/request/claim"
        rendered
        [:main
         [:form#first-owner]
         [:button {:form "first-owner" :formmethod "POST"
                   :formaction claim}
          "First"]
         ["form#second-owner" [:p "Unadorned string tag"]]
         [:section
          [:input {:type "submit" :form "second-owner"
                   :formmethod :post :formaction claim}]]
         [:form#third-owner
          [:button {:formmethod "post" :formaction claim} "Inline"]]]]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (application/report? report))
        (is (nil? assembly))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (is (= [{:request-source :native-submitter
                 :method :post
                 :path claim
                 :render-path [2]
                 :form-render-path [1]
                 :submitter-overrides #{:formaction :formmethod}}
                {:request-source :native-submitter
                 :method :post
                 :path claim
                 :render-path [4 1]
                 :form-render-path [3]
                 :submitter-overrides #{:formaction :formmethod}}
                {:request-source :native-submitter
                 :method :post
                 :path claim
                 :render-path [5 1]
                 :form-render-path [5]
                 :submitter-overrides #{:formaction :formmethod}}]
               (mapv #(select-keys % [:request-source :method :path
                                       :render-path :form-render-path
                                       :submitter-overrides])
                     (:errors report))))
        (is (every? #(= #{:request/claim} (:candidate-operations %))
                    (:errors report)))))))

(deftest invalid-button-types-default-to-submission-on-semantic-post-routes
  (let [claim "/operations/request/claim"
        cases [["" :empty]
               ["unknown" :unknown]
               ["SUBMITT" :misspelled]
               [:other :keyword]
               [42 :non-text]]
        rendered
        (into [:main
               [:form {:method :post :action "/ordinary/save"}
                [:button {:type :reset :formaction claim} "Reset"]
                [:button {:type :button :formaction claim} "Non-submit"]]]
              (map (fn [[button-type label]]
                     [:form {:method :post :action "/ordinary/save"}
                      [:button {:type button-type :formaction claim}
                       (name label)]])
                   cases))]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (not (application/valid? report)))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (is (= (count cases) (count (:errors report))))
        (is (= (mapv (fn [index]
                       {:request-source :native-submitter
                        :method :post
                        :path claim
                        :render-path [(+ index 2) 2]
                        :form-render-path [(+ index 2)]
                        :submitter-overrides #{:formaction}})
                     (range (count cases)))
               (mapv #(select-keys % [:request-source :method :path
                                       :render-path :form-render-path
                                       :submitter-overrides])
                     (:errors report))))))))

(deftest default-get-and-nonsubmit-controls-do-not-create-false-collisions
  (let [claim "/operations/request/claim"
        rendered
        [:main
         [:form#default-get
          [:button {:formaction claim} "GET does not match semantic POST"]]
         [:button {:form "default-get" :formaction claim}
          "Detached GET does not match"]
         [:form#ordinary
          [:button {:formmethod "post" :formaction "/ordinary/save"}
           "Ordinary POST"]]
         [:form {:method :post :action "/ordinary/save"}
          [:button {:type :reset :formaction claim} "Reset"]
          [:button {:type "BuTtOn" :formaction claim} "Button"]
          [:input {:type :text :formaction claim}]]
         [:button {:form "unknown" :type "invalid" :formaction claim}
          "No form owner"]]
        canonical
        (rendered-operation-button
         (render-context (standard-operations))
         :request/claim claim)]
    (with-surfaced-application
      {:request-board [:section canonical rendered]}
      (fn [{:keys [report assembly]}]
        (is (application/report? report))
        (is (application/valid? report))
        (is (application/application-assembly? assembly))
        (is (empty? (:errors report)))
        (is (= [:request/claim]
               (mapv :operation (get-in report [:analysis :affordances]))))))))

(deftest hiccup-owner-and-invalid-type-reject-before-renderer-executes
  (with-closed-application
    (fn [{:keys [assembly]}]
      (doseq [[scenario rendered expected-path expected-form-path]
              [[:unadorned-owner
                [:main
                 [:form#owner]
                 [:button {:form "owner" :formmethod "POST"
                           :formaction "/operations/request/claim"} "Claim"]]
                [2] [1]]
               [:invalid-button-type
                [:main
                 [:form {:method :post :action "/ordinary/save"}
                  [:button {:type "unrecognized"
                            :formaction "/operations/request/claim"} "Claim"]]]
                [1 2] [1]]]]
        (testing (name scenario)
          (let [render-calls (atom 0)
                report (application/check-rendered-surface
                        assembly :request-board rendered)
                data (error-data
                      #(application/checked-rendered-response!
                        assembly :request-board
                        (fn [_]
                          (swap! render-calls inc)
                          {:status 200 :body "must-not-render"})
                        rendered))
                error (first (:errors report))]
            (is (application/rendered-surface-report? report))
            (is (not (application/rendered-surface-valid? report)))
            (is (= #{:rendered-semantic-route-without-choreo-operation}
                   (error-kinds report)))
            (is (= :native-submitter (:request-source error)))
            (is (= expected-path (:render-path error)))
            (is (= expected-form-path (:form-render-path error)))
            (is (= :rendered-surface-preflight-failed (:error/kind data)))
            (is (= #{:rendered-semantic-route-without-choreo-operation}
                   (error-kinds (:preflight data))))
            (is (zero? @render-calls))))))))

;; =============================================================================
;; v723 adversarial regression: string-keyed request attrs and duplicate attrs
;; =============================================================================
;; Rum accepts both string and keyword HTML attribute keys. v722 closes the
;; semantic-route collision bypass created when the scanner previously ignored
;; string-keyed request attributes. Both spellings are checked; duplicates
;; fail closed even when their values agree. These tests exercise the public
;; application assembly and dynamic pre-browser response boundaries, without
;; claiming completeness for JavaScript-generated or unsupplied DOM.

(deftest string-keyed-htmx-requests-cannot-bypass-semantic-routes
  (let [methods [:get :post :put :patch :delete]
        path "/requests/request-42/claim?from=string#pending"
        operations {:request/claim (get (standard-operations) :request/claim)}
        routes (custom-route-capabilities
                (into {}
                      (map (fn [method]
                             [(keyword "fixture" (str "string-claim-" (name method)))
                              {:operation :request/claim
                               :method method
                               :path "/requests/:request-id/claim"
                               :transports #{:htmx}}])
                           methods)))
        rendered (into [:main]
                       (map (fn [method]
                              [:button {(str "hx-" (name method)) path}
                               (name method)])
                            methods))]
    (with-surfaced-application
      {:operations operations
       :route-capabilities routes
       :rendered-surfaces {:request-board rendered}}
      (fn [{:keys [report assembly]}]
        (is (application/report? report))
        (is (nil? assembly))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (is (= (mapv (fn [index method]
                       {:surface :request-board
                        :render-path [index]
                        :method method
                        :path path
                        :request-source (keyword (str "hx-" (name method)))
                        :candidate-operations #{:request/claim}})
                     (range 1 (inc (count methods))) methods)
               (mapv #(select-keys % [:surface :render-path :method :path
                                       :request-source :candidate-operations])
                     (:errors report))))))))

(deftest string-and-mixed-keyed-native-form-submissions-are-checked
  (let [claim "/operations/request/claim"
        rendered
        [:main
         [:form {"method" "POST" "action" claim}
          [:button {"type" "submit"} "Native"]]
         [:form {"id" "detached-owner" :method :post
                 "action" "/ordinary/save"}]
         [:button {"form" "detached-owner" "formaction" claim
                   "type" "submit"} "Detached"]
         [:form {:method :get "action" "/ordinary/list"}
          [:input {"type" "submit" "formmethod" "POST"
                   "formaction" claim}]]
         [:form {"method" "GET" :action claim}
          [:button "Nonmatching GET"]]]]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (is (= [{:request-source :native-form
                 :method :post :path claim :render-path [1]}
                {:request-source :native-submitter
                 :method :post :path claim :render-path [3]
                 :form-render-path [2]
                 :submitter-overrides #{:formaction}}
                {:request-source :native-submitter
                 :method :post :path claim :render-path [4 2]
                 :form-render-path [4]
                 :submitter-overrides #{:formaction :formmethod}}]
               (mapv #(select-keys % [:request-source :method :path
                                       :render-path :form-render-path
                                       :submitter-overrides])
                     (:errors report))))))))

(deftest duplicate-keyword-string-html-attributes-are-always-ambiguous
  (let [attributes ["hx-get" "hx-post" "hx-put" "hx-patch" "hx-delete"
                    "id" "method" "action" "form" "formaction"
                    "formmethod" "type"]
        rendered
        (into [:main]
              (mapcat (fn [attribute]
                        [[:div {(keyword attribute) "/ordinary/no-op"
                                attribute "/ordinary/no-op"}]
                         [:div {(keyword attribute) "/ordinary/first"
                                attribute "/ordinary/second"}]])
                      attributes))]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (application/report? report))
        (is (nil? assembly))
        (is (= #{:rendered-ambiguous-html-attribute}
               (error-kinds report)))
        (is (= (* 2 (count attributes)) (count (:errors report))))
        (is (= (mapv (fn [index attribute]
                       {:surface :request-board
                        :render-path [(inc index)]
                        :attribute (keyword attribute)})
                     (range (* 2 (count attributes)))
                     (mapcat #(repeat 2 %) attributes))
               (mapv #(select-keys % [:surface :render-path :attribute])
                     (:errors report))))))))

(deftest duplicate-html-attributes-on-canonical-choreo-are-rejected
  (let [ctx (render-context (standard-operations))
        canonical (rendered-operation-button
                   ctx :request/claim "/operations/request/claim")
        button-index (first (keep-indexed
                             (fn [index node]
                               (when (and (vector? node)
                                          (= :button (first node))) index))
                             canonical))
        ambiguous (update-in canonical [button-index 1]
                             assoc "hx-post" "/operations/request/claim")]
    (is (some? button-index))
    (with-surfaced-application
      {:request-board [:main ambiguous]}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (not (application/valid? report)))
        (is (contains? (error-kinds report)
                       :rendered-ambiguous-html-attribute))
        (is (some (fn [error]
                    (and (= :rendered-ambiguous-html-attribute (:kind error))
                         (= :request-board (:surface error))
                         (= :hx-post (:attribute error))
                         (= [1 button-index] (:render-path error))))
                  (:errors report)))))))

(deftest string-keyed-requests-and-duplicate-attrs-fail-before-rendering
  (with-closed-application
    (fn [{:keys [assembly]}]
      (doseq [[scenario rendered expected-kind]
              [[:string-htmx
                [:button {"hx-post" "/operations/request/claim"} "Claim"]
                :rendered-semantic-route-without-choreo-operation]
               [:string-native-post
                [:form {"method" "POST" "action" "/operations/request/claim"}
                 [:button "Claim"]]
                :rendered-semantic-route-without-choreo-operation]
               [:duplicate-ordinary-attrs
                [:div {:id "unique" "id" "unique"}]
                :rendered-ambiguous-html-attribute]]]
        (testing (name scenario)
          (let [calls (atom 0)
                report (application/check-rendered-surface
                        assembly :request-board rendered)
                failure (error-data
                         #(application/checked-rendered-response!
                           assembly :request-board
                           (fn [_]
                             (swap! calls inc)
                             {:status 200})
                           rendered))]
            (is (application/rendered-surface-report? report))
            (is (not (application/rendered-surface-valid? report)))
            (is (contains? (error-kinds report) expected-kind))
            (is (= :rendered-surface-preflight-failed (:error/kind failure)))
            (is (contains? (error-kinds (:preflight failure)) expected-kind))
            (is (zero? @calls))))))))

(deftest string-keyed-ordinary-requests-coexist-with-canonical-choreo
  (let [claim "/operations/request/claim"
        canonical (rendered-operation-button
                   (render-context (standard-operations)) :request/claim claim)
        rendered
        [:main canonical
         [:button {"hx-get" "/ordinary/list"} "List"]
         [:button {"hx-post" "/ordinary/save"} "Save"]
         [:button {"hx-put" "/ordinary/replace"} "Replace"]
         [:button {"hx-patch" "/ordinary/edit"} "Edit"]
         [:button {"hx-delete" "/ordinary/delete"} "Delete"]
         [:form {"id" "form-1" "method" "POST"
                 "action" "/ordinary/save"}
          [:button {"type" "submit"} "Submit"]]
         [:button {"form" "form-1" "formaction" "/ordinary/other"}
          "Detached save"]
         [:form {"method" "GET" "action" claim}
          [:button "Nonmatching GET"]]]]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (application/report? report))
        (is (application/valid? report))
        (is (application/application-assembly? assembly))
        (is (empty? (:errors report)))
        (is (= [:request/claim]
               (mapv :operation (get-in report [:analysis :affordances]))))))))

;; =============================================================================
;; v725 adversarial regression: canonical physical request correspondence
;; =============================================================================
;; A validated :choreo/op only admits the corresponding physical hx-post on the
;; marked button. It does not whitelist other request attributes on that node.
;; These scenarios check the public static assembly and dynamic pre-render
;; boundary, including method/path evidence and healthy ordinary requests.

(defn- add-request-attrs-to-canonical-button
  "Mutate Hiccup test data without rebuilding (or inventing) Choreo metadata."
  [canonical attrs]
  (let [button-index
        (first
         (keep-indexed
          (fn [index node]
            (when (and (vector? node)
                       (= :button (first node)))
              index))
          canonical))]
    (assert (some? button-index) "Expected a canonical Gesso POST button")
    (update-in canonical [button-index 1] merge attrs)))

(deftest canonical-mark-does-not-exempt-additional-htmx-methods
  (let [canonical (rendered-operation-button
                   (render-context (standard-operations))
                   :request/claim "/operations/request/claim")
        extra-requests
        {:hx-get "/ordinary/read"
         :hx-put "/ordinary/replace"
         :hx-patch "/ordinary/update"
         :hx-delete "/ordinary/remove"}
        rendered [:main (add-request-attrs-to-canonical-button
                         canonical extra-requests)]]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (application/report? report))
        (is (nil? assembly))
        (is (= #{:rendered-choreo-physical-request-mismatch}
               (error-kinds report)))
        (let [errors (:errors report)]
          (is (= 4 (count errors)))
          (is (= [[:get :hx-get "/ordinary/read"]
                  [:put :hx-put "/ordinary/replace"]
                  [:patch :hx-patch "/ordinary/update"]
                  [:delete :hx-delete "/ordinary/remove"]]
                 (mapv (juxt :method :request-source :path) errors)))
          (is (every? #(= :request-board (:surface %)) errors))
          (is (= 1 (count (set (map :render-path errors)))))
          (is (every? #(= [{:operation :request/claim
                            :method :post
                            :path "/operations/request/claim"}]
                          (mapv (fn [a]
                                  (select-keys a [:operation :method :path]))
                                (:canonical-affordances %)))
                      errors))
          (is (every? (comp empty? :matching-routes) errors)))))))

(deftest canonical-extra-semantic-route-carries-matching-route-evidence
  (let [operations (standard-operations)
        routes (custom-route-capabilities
                {:request/claim
                 {:operation :request/claim
                  :method :post :path "/operations/request/claim"}
                 :request/cancel-read
                 {:operation :request/cancel
                  :method :get :path "/operations/request/cancel"}
                 :request/reassign
                 {:operation :request/reassign
                  :method :post :path "/operations/request/reassign"}})
        canonical (rendered-operation-button
                   (render-context operations)
                   :request/claim "/operations/request/claim")
        offending (add-request-attrs-to-canonical-button
                   canonical {:hx-get "/operations/request/cancel?from=claim"})]
    (with-surfaced-application
      {:operations operations
       :route-capabilities routes
       :rendered-surfaces {:request-board [:main offending]}}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (= #{:rendered-choreo-physical-request-mismatch}
               (error-kinds report)))
        (let [error (first (:errors report))]
          (is (= :hx-get (:request-source error)))
          (is (= :get (:method error)))
          (is (= "/operations/request/cancel?from=claim" (:path error)))
          (is (= [{:operation :request/cancel
                   :route-id :request/cancel-read
                   :method :get
                   :route-template "/operations/request/cancel"
                   :required-transport :htmx}]
                 (:matching-routes error)))
          (is (= :request/claim
                 (:operation (first (:canonical-affordances error))))))))))

(deftest canonical-physical-request-mismatches-are-occurrence-local
  (let [ctx (render-context (standard-operations))
        canonical (rendered-operation-button
                   ctx :request/claim "/operations/request/claim")
        bad-a (add-request-attrs-to-canonical-button
               canonical {:hx-get "/ordinary/first"})
        bad-b (add-request-attrs-to-canonical-button
               canonical {"hx-delete" "/ordinary/second"})]
    (with-surfaced-application
      {:alpha [:main canonical bad-a]
       :beta [:section bad-b canonical]}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (= #{:rendered-choreo-physical-request-mismatch}
               (error-kinds report)))
        (is (= [[:alpha [2 3] :hx-get "/ordinary/first"]
                [:beta [1 3] :hx-delete "/ordinary/second"]]
               (mapv (juxt :surface :render-path :request-source :path)
                     (:errors report))))
        (is (= 4 (count (get-in report [:analysis :affordances]))))
        (is (= #{:request/claim}
               (set (map :operation (get-in report [:analysis :affordances])))))))))

(deftest canonical-extra-request-fails-before-html-rendering
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [ctx (render-context (standard-operations))
            canonical (rendered-operation-button
                       ctx :request/claim "/operations/request/claim")
            invalid [:main (add-request-attrs-to-canonical-button
                            canonical {:hx-delete "/ordinary/delete"})]
            valid [:main canonical [:button {:hx-get "/ordinary/list"} "List"]]
            calls (atom 0)
            invalid-report (application/check-rendered-surface
                            assembly :request-board invalid)
            failure (error-data
                     #(application/checked-rendered-response!
                       assembly :request-board
                       (fn [_]
                         (swap! calls inc)
                         {:status 200})
                       invalid))
            valid-report (application/check-rendered-surface
                          assembly :request-board valid)
            response (application/checked-rendered-response!
                      assembly :request-board
                      (fn [_]
                        (swap! calls inc)
                        {:status 200 :body "accepted"})
                      valid)]
        (is (application/rendered-surface-report? invalid-report))
        (is (= #{:rendered-choreo-physical-request-mismatch}
               (error-kinds invalid-report)))
        (is (= :rendered-surface-preflight-failed (:error/kind failure)))
        (is (= #{:rendered-choreo-physical-request-mismatch}
               (error-kinds (:preflight failure))))
        (is (application/rendered-surface-valid? valid-report))
        (is (= {:status 200 :body "accepted"} response))
        (is (= 1 @calls))))))

(deftest canonical-mark-does-not-hide-anonymous-sibling-semantic-route
  (let [ctx (render-context (standard-operations))
        canonical (rendered-operation-button
                   ctx :request/claim "/operations/request/claim")
        rendered [:main
                  canonical
                  [:button {:hx-post "/operations/request/claim"}
                   "Anonymous claim"]
                  [:button {:hx-get "/ordinary/list"} "Ordinary GET"]]]
    (with-surfaced-application
      {:request-board rendered}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (= #{:rendered-semantic-route-without-choreo-operation}
               (error-kinds report)))
        (is (= [{:surface :request-board
                 :render-path [2]
                 :method :post
                 :path "/operations/request/claim"
                 :request-source :hx-post
                 :candidate-operations #{:request/claim}}]
               (mapv #(select-keys % [:surface :render-path :method :path
                                       :request-source :candidate-operations])
                     (:errors report))))))))

