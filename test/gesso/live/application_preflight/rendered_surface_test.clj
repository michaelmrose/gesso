(ns gesso.live.application-preflight.rendered-surface-test
  "Direct regression tests for rendered-surface enumeration and semantic analysis.

   These tests deliberately use the real Hiccup request scanner, UI Choreo
   affordance scanner, and route correspondence checker, without assembling an
   application or supplying trusted authority. A successful result means the
   supplied surface matches the supplied operation summary; it is not proof
   that either input is complete, current, or authorized."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.application-preflight.rendered-surface :as surface]
   [gesso.live.ui :as ui]))

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

(def ^:private operations
  (sorted-map
   :request/cancel {:browser-plan-key :request/cancel
                    :routes [cancel-route]}
   :request/claim {:browser-plan-key :request/claim
                   :routes [claim-route]}))

(defn- canonical-button
  ([operation path]
   (canonical-button operation path {}))
  ([operation path extra-attrs]
   ;; The marker is framework-owned metadata, NOT an HTML attribute or
   ;; authorization token. The action is encoded with the public UI helper.
   (with-meta
     [:button
      (merge {:type "button"
              :hx-post path}
             (ui/optimistic-action-attrs
              {:operation operation
               :plan-key operation
               :arguments {:request-id 42}
               :observed-basis {:tx-id 42}})
             extra-attrs)
      "Act"]
     {ui/choreo-affordance-metadata-key operation})))

(defn- scan
  [surfaces]
  (surface/scan-rendered-surfaces surfaces))

(defn- analyze
  ([surfaces]
   (analyze operations surfaces))
  ([operation-summary surfaces]
   (surface/analyze-rendered-surfaces operation-summary surfaces)))

(defn- kinds
  [errors]
  (mapv :kind errors))

(deftest nil-snapshot-is-absence-of-evidence-not-an-error
  (let [expected {:affordances [] :request-coordinates [] :errors []}
        result (analyze nil)]
    (is (= expected (scan nil)))
    (is (= expected (:scan result)))
    (is (= [] (:affordances result)))
    (is (= [] (:scan-errors result)))
    (is (= [] (:semantic-route-errors result)))
    (is (= [] (:resolution-errors result)))
    (is (= [] (:errors result)))))

(deftest empty-snapshot-and-ordinary-markup-are-inert
  (doseq [surfaces [{} {:board [:main [:h1 "Hello"]]}]]
    (testing (pr-str surfaces)
      (let [result (analyze surfaces)]
        (is (= [] (get-in result [:scan :affordances])))
        (is (= [] (get-in result [:scan :request-coordinates])))
        (is (= [] (:errors result)))
        (is (= [] (:affordances result)))))))

(deftest surfaces-are-visited-in-stable-name-order
  (let [surfaces (array-map
                  :zeta [:button {:hx-post "/other/z"} "Z"]
                  :alpha [:button {:hx-get "/other/a"} "A"]
                  :middle [:button {:hx-delete "/other/m"} "M"])
        expected [[:alpha :get "/other/a"]
                  [:middle :delete "/other/m"]
                  [:zeta :post "/other/z"]]]
    (is (= expected
           (mapv (juxt :surface :method :path)
                 (:request-coordinates (scan surfaces)))))
    (is (= (:scan (analyze surfaces)) (scan surfaces)))))

(deftest scanner-preserves-physical-method-path-source-and-location
  (let [found (:request-coordinates
               (scan {:board [:main
                              [:section
                               [:button {"hx-put" "/requests/42"} "Put"]]
                              [:form {:method "post"
                                      :action "/requests/42/claim"}
                               [:input {:type "text"}]]]}))]
    (is (= [[:board [1 1] :put "/requests/42" :hx-put]
            [:board [2] :post "/requests/42/claim" :native-form]]
           (mapv (juxt :surface :render-path :method :path :request-source)
                 found)))
    (is (= [false false] (mapv :choreo-operation-declared? found)))))

(deftest canonical-hiccup-is-scanned-with-matching-physical-coordinate
  (let [found (scan {:board [:main
                            (canonical-button :request/claim
                                              "/requests/42/claim")]})
        affordance (first (:affordances found))
        coordinate (first (:request-coordinates found))]
    (is (empty? (:errors found)))
    (is (= 1 (count (:affordances found))))
    (is (= 1 (count (:request-coordinates found))))
    (is (= :request/claim (:operation affordance)))
    (is (= :request/claim (:plan-key affordance)))
    (is (= :post (:method affordance)))
    (is (= "/requests/42/claim" (:path affordance)))
    (is (= [:board [1]] ((juxt :surface :render-path) affordance)))
    (is (= [:board [1]] ((juxt :surface :render-path) coordinate)))
    (is (= :hx-post (:request-source coordinate)))
    (is (true? (:choreo-operation-declared? coordinate)))))

(deftest canonical-request-resolves-to-trusted-route-relative-to-summary
  (let [result (analyze {:board [:main
                                 (canonical-button :request/claim
                                                   "/requests/42/claim?from=board")]})
        affordance (first (:affordances result))]
    (is (empty? (:errors result)))
    (is (= :route/claim (:route-id affordance)))
    (is (= "/requests/:request-id/claim" (:route-template affordance)))
    (is (= :htmx (:required-transport affordance)))
    (is (= "/requests/42/claim?from=board" (:path affordance)))
    (is (= 1 (count (:affordances result))))))

(deftest repeated-canonical-buttons-remain-separate-occurrences
  (let [result (analyze
                {:board [:main
                         (canonical-button :request/claim "/requests/42/claim")
                         [:aside
                          (canonical-button :request/claim "/requests/43/claim")]]})]
    (is (= [] (:errors result)))
    (is (= [[1] [2 1]]
           (mapv :render-path (:affordances result))))
    (is (= ["/requests/42/claim" "/requests/43/claim"]
           (mapv :path (:affordances result))))
    (is (= [:route/claim :route/claim]
           (mapv :route-id (:affordances result))))))

(deftest canonical-affordances-on-different-surfaces-retain-provenance
  (let [result (analyze
                {:z-toolbar (canonical-button :request/cancel
                                             "/requests/42/cancel")
                 :a-board (canonical-button :request/claim
                                            "/requests/42/claim")})]
    (is (empty? (:errors result)))
    (is (= [[:a-board :request/claim :route/claim]
            [:z-toolbar :request/cancel :route/cancel]]
           (mapv (juxt :surface :operation :route-id)
                 (:affordances result))))
    (is (= [[:a-board []] [:z-toolbar []]]
           (mapv (juxt :surface :render-path)
                 (:request-coordinates (:scan result)))))))

(deftest anonymous-requests-to-semantic-routes-are-rejected
  (let [result (analyze
                {:board [:main
                         [:button {:hx-post "/requests/42/claim"} "Claim"]]})
        error (first (:semantic-route-errors result))]
    (is (= [:rendered-semantic-route-without-choreo-operation]
           (kinds (:errors result))))
    (is (= :board (:surface error)))
    (is (= [1] (:render-path error)))
    (is (= :post (:method error)))
    (is (= "/requests/42/claim" (:path error)))
    (is (= #{:request/claim} (:candidate-operations error)))
    (is (= [:route/claim] (mapv :route-id (:matching-routes error))))
    (is (empty? (:affordances result)))))

(deftest ordinary-requests-are-not-forbidden-by-unrelated-semantic-routes
  (let [result (analyze
                {:board [:main
                         [:button {:hx-post "/session/login"} "Login"]
                         [:button {:hx-get "/requests/42/claim"} "Read"]]})]
    (is (= [] (:errors result)))
    (is (= [[:post "/session/login"]
            [:get "/requests/42/claim"]]
           (mapv (juxt :method :path)
                 (get-in result [:scan :request-coordinates]))))))

(deftest anonymous-native-post-form-has-the-same-route-check
  (let [result (analyze
                {:board [:form {:method "post"
                                :action "/requests/42/claim"}
                         [:input {:type "text"}]]})
        error (first (:semantic-route-errors result))]
    (is (= [:rendered-semantic-route-without-choreo-operation]
           (kinds (:errors result))))
    (is (= :native-form (:request-source error)))
    (is (= [] (:render-path error)))))

(deftest native-submitter-override-is-inspected-in-actual-surface
  (let [result (analyze
                {:board [:main
                         [:form#remote {:method "get" :action "/ordinary"}]
                         [:button {:form "remote"
                                   :formaction "/requests/42/claim"
                                   :formmethod "post"}
                          "Claim"]]})
        error (first (:semantic-route-errors result))]
    (is (= [:rendered-semantic-route-without-choreo-operation]
           (kinds (:errors result))))
    (is (= :native-submitter (:request-source error)))
    (is (= [2] (:render-path error)))
    (is (= [1] (:form-render-path error)))
    (is (= #{:formaction :formmethod} (:submitter-overrides error)))))

(deftest canonical-mark-does-not-exempt-an-additional-request
  (let [result (analyze
                {:board (canonical-button :request/claim
                                          "/requests/42/claim"
                                          {:hx-delete "/ordinary/side-effect"})})
        error (first (:semantic-route-errors result))]
    (is (= [:rendered-choreo-physical-request-mismatch]
           (kinds (:errors result))))
    (is (= :hx-delete (:request-source error)))
    (is (= :delete (:method error)))
    (is (= "/ordinary/side-effect" (:path error)))
    (is (= :request/claim
           (:operation (first (:canonical-affordances error)))))
    (is (= :route/claim (:route-id (first (:affordances result)))))))

(deftest canonical-mark-does-not-conceal-an-anonymous-sibling
  (let [result (analyze
                {:board [:main
                         (canonical-button :request/claim "/requests/42/claim")
                         [:button {:hx-post "/requests/43/claim"} "Anonymous"]]})]
    (is (= [:rendered-semantic-route-without-choreo-operation]
           (kinds (:errors result))))
    (is (= [2] (:render-path (first (:errors result)))))
    (is (= 1 (count (:affordances result))))))

(deftest invalid-rendered-metadata-fails-closed-and-keeps-physical-request
  (let [malformed
        (with-meta
          [:button {:hx-post "/requests/42/claim"} "Claim"]
          {ui/choreo-affordance-metadata-key :request/claim})
        result (analyze {:board [:main malformed]})
        scan-error (first (:scan-errors result))]
    (is (= [:rendered-affordance-scan-failed]
           (kinds (:scan-errors result))))
    (is (= :gesso.live.ui/affordance-error (:cause-type scan-error)))
    (is (= :invalid-rendered-affordance-encoding (:cause-kind scan-error)))
    (is (= :board (:surface scan-error)))
    (is (= [[:post "/requests/42/claim"]]
           (mapv (juxt :method :path)
                 (get-in result [:scan :request-coordinates]))))
    (is (= [:route/claim]
           (mapv :route-id (:matching-semantic-routes scan-error))))
    ;; The existing scanner intentionally records a scan failure rather than
    ;; inventing a valid canonical affordance from malformed metadata.
    (is (empty? (:affordances result)))))

(deftest broken-surface-does-not-hide-another-surface
  (let [malformed
        (with-meta
          [:button {:hx-post "/requests/42/claim"} "Forged"]
          {ui/choreo-affordance-metadata-key :request/claim})
        result (analyze
                {:a-broken malformed
                 :z-good (canonical-button :request/cancel
                                            "/requests/42/cancel")})]
    (is (= [:rendered-affordance-scan-failed]
           (kinds (:scan-errors result))))
    (is (= :a-broken (:surface (first (:scan-errors result)))))
    (is (= [[:z-good :request/cancel :route/cancel]]
           (mapv (juxt :surface :operation :route-id)
                 (:affordances result))))
    (is (= [:a-broken :z-good]
           (mapv :surface (get-in result [:scan :request-coordinates]))))))

(deftest ambiguous-keyword-string-attributes-are-reportable
  (let [result (analyze
                {:board [:button {:hx-post "/requests/42/claim"
                                  "hx-post" "/other"} "Ambiguous"]})]
    (is (= [:rendered-ambiguous-html-attribute]
           (kinds (:scan-errors result))))
    (is (= :board (:surface (first (:scan-errors result)))))
    (is (= [:rendered-ambiguous-html-attribute
            :rendered-semantic-route-without-choreo-operation]
           (kinds (:errors result))))
    (is (= 1 (count (get-in result [:scan :request-coordinates]))))))

(deftest unknown-operation-is-rejected-without-dropping-occurrence
  (let [result (analyze
                {:board (canonical-button :request/archive
                                          "/requests/42/archive")})]
    (is (= [:rendered-affordance-unknown-operation]
           (kinds (:resolution-errors result))))
    (is (= #{:request/claim :request/cancel}
           (:available-operations (first (:errors result)))))
    (is (= :request/archive (:operation (first (:affordances result)))))))

(deftest mismatched-browser-plan-is-reported-at-resolution-boundary
  (let [result (analyze
                (assoc-in operations [:request/claim :browser-plan-key]
                          :plan/different)
                {:board (canonical-button :request/claim "/requests/42/claim")})]
    (is (= [:rendered-affordance-plan-mismatch]
           (kinds (:resolution-errors result))))
    (is (= :request/claim
           (:affordance-plan-key (first (:resolution-errors result)))))
    (is (= :plan/different
           (:assembled-plan-key (first (:resolution-errors result)))))))

(deftest path-mismatch-is-reported-at-resolution-boundary
  (let [result (analyze
                {:board (canonical-button :request/claim "/requests/42/unknown")})]
    (is (= [:rendered-affordance-path-mismatch]
           (kinds (:resolution-errors result))))
    (is (= ["/requests/:request-id/claim"]
           (:route-templates (first (:resolution-errors result)))))))

(deftest scanner-records-unexpected-failures-without-losing-requests
  (with-redefs [ui/rendered-choreo-affordances
                (fn [_] (throw (IllegalStateException. "test scanner failure")))]
    (let [result (analyze
                  {:board [:button {:hx-post "/requests/42/claim"} "Claim"]})
          error (first (:scan-errors result))]
      (is (= [:rendered-affordance-scan-failed] (kinds (:scan-errors result))))
      (is (= "class java.lang.IllegalStateException" (:exception-class error)))
      (is (= "test scanner failure" (:exception-message error)))
      (is (= [[:post "/requests/42/claim"]]
             (mapv (juxt :method :path)
                   (get-in result [:scan :request-coordinates]))))
      (is (= [:route/claim]
             (mapv :route-id (:matching-semantic-routes error)))))))

(deftest error-vectors-are-a-stable-concatenation-of-stages
  (let [malformed
        (with-meta
          [:button {:hx-post "/requests/42/claim"} "Invalid"]
          {ui/choreo-affordance-metadata-key :request/claim})
        result (analyze
                (assoc-in operations [:request/cancel :browser-plan-key]
                          :plan/wrong)
                {:a [:main malformed
                     [:button {:hx-post "/requests/43/cancel"} "Anonymous"]]
                 :b (canonical-button :request/cancel
                                       "/requests/42/cancel")})]
    (is (seq (:scan-errors result)))
    (is (seq (:semantic-route-errors result)))
    (is (seq (:resolution-errors result)))
    (is (= (vec (concat (:scan-errors result)
                        (:semantic-route-errors result)
                        (:resolution-errors result)))
           (:errors result)))
    (is (= [:rendered-affordance-scan-failed
            :rendered-choreo-physical-request-mismatch
            :rendered-semantic-route-without-choreo-operation
            :rendered-affordance-plan-mismatch]
           (kinds (:errors result))))))

(deftest analysis-does-not-mutate-supplied-hiccup-or-operations
  (let [surfaces {:board [:main
                          (canonical-button :request/claim "/requests/42/claim")
                          [:button {:hx-get "/ordinary"} "Read"]]}
        original-surfaces surfaces
        original-operations operations
        first-result (analyze surfaces)]
    (is (= first-result (analyze surfaces)))
    (is (= original-surfaces surfaces))
    (is (= original-operations operations))
    (is (vector? (:errors first-result)))
    (is (vector? (:affordances first-result)))
    (is (empty? (:errors first-result)))))
