(ns gesso.live.application-preflight.rendered-response-test
  "Integration tests for validation of actual Hiccup response surfaces before
   rendering and browser delivery.  The test bodies are copied unchanged from
   gesso.live.application-preflight-test (v750) and use the shared fixture."
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.fixture
    :refer [error-data error-kind error-kinds render-context
            rendered-operation-button standard-operations
            with-closed-application with-surfaced-application]]))

;; =============================================================================
;; Dynamic pre-browser rendered-surface boundary
;; =============================================================================

(deftest dynamic-rendered-surface-report-closes-one-actual-surface
  (let [operations (standard-operations)
        ctx (render-context operations)
        rendered
        [:main
         (rendered-operation-button
          ctx
          :request/claim
          "/operations/request/claim")]]
    (with-closed-application
      (fn [{:keys [assembly]}]
        (let [report
              (application/check-rendered-surface
               assembly
               :request-board
               rendered)]
          (is (application/rendered-surface-report? report))
          (is (application/rendered-surface-valid? report))
          (is (= :rendered-surface-pre-browser-preflight-closed
                 (get-in report [:analysis :guarantee])))
          (is (= :request-board
                 (get-in report [:analysis :surface])))
          (is (= rendered
                 (get-in report [:analysis :rendered])))
          (let [affordance (first (get-in report [:analysis :affordances]))]
            (is (= :gesso.live.ui/rendered-choreo-affordance
                   (:gesso.live.ui/type affordance)))
            (is (= 1 (:gesso.live.ui/version affordance)))
            (is (= :request-board (:surface affordance)))
            (is (= :post-button (:kind affordance)))
            (is (= :request/claim (:operation affordance)))
            (is (= :request/claim (:plan-key affordance)))
            (is (= :post (:method affordance)))
            (is (= "/operations/request/claim" (:path affordance)))
            (is (= [1 3] (:render-path affordance)))
            (is (= :request/claim (:route-id affordance)))
            (is (= "/operations/request/claim" (:route-template affordance)))
            (is (= :htmx (:required-transport affordance)))))))))

(deftest checked-rendered-response-validates-before-calling-renderer-exactly-once
  (let [operations (standard-operations)
        ctx (render-context operations)
        rendered
        [:main
         (rendered-operation-button
          ctx
          :request/claim
          "/operations/request/claim")]
        calls (atom [])]
    (with-closed-application
      (fn [{:keys [assembly]}]
        (let [response
              (application/checked-rendered-response!
               assembly
               :request-board
               (fn [node]
                 (swap! calls conj node)
                 {:status 200 :body node})
               rendered)]
          (is (= [rendered] @calls))
          (is (= {:status 200 :body rendered} response)))))))

(deftest checked-rendered-response-never-calls-renderer-on-affordance-failure
  (let [operations (standard-operations)
        ctx (render-context operations)
        rendered
        [:main
         (rendered-operation-button
          ctx
          :request/claim
          "/wrong/claim")]
        calls (atom 0)]
    (with-closed-application
      (fn [{:keys [assembly]}]
        (let [data
              (error-data
               #(application/checked-rendered-response!
                 assembly
                 :request-board
                 (fn [_]
                   (swap! calls inc)
                   {:status 200})
                 rendered))]
          (is (= :rendered-surface-preflight-failed (:error/kind data)))
          (is (= 0 @calls))
          (is (= #{:rendered-affordance-path-mismatch}
                 (error-kinds (:preflight data)))))))))

(deftest invalid-or-malformed-render-boundary-input-never-reaches-renderer
  (let [operations (standard-operations)
        ctx (render-context operations)
        rendered
        [:main
         (rendered-operation-button
          ctx
          :request/claim
          "/operations/request/claim")]]
    (with-closed-application
      (fn [{:keys [assembly]}]
        (doseq [[surface node expected]
                [["request-board" rendered :invalid-rendered-surface-name]
                 [nil rendered :invalid-rendered-surface-name]
                 [:request-board nil :invalid-rendered-surface]]]
          (let [calls (atom 0)]
            (is (= expected
                   (error-kind
                    #(application/checked-rendered-response!
                      assembly
                      surface
                      (fn [_]
                        (swap! calls inc)
                        {:status 200})
                      node))))
            (is (= 0 @calls))))))))

(deftest non-callable-response-renderer-fails-before-browser-delivery
  (let [operations (standard-operations)
        ctx (render-context operations)
        rendered
        [:main
         (rendered-operation-button
          ctx
          :request/claim
          "/operations/request/claim")]]
    (with-closed-application
      (fn [{:keys [assembly]}]
        (is (= :invalid-response-renderer
               (error-kind
                #(application/checked-rendered-response!
                  assembly
                  :request-board
                  42
                  rendered))))))))

(deftest dynamic-surface-need-not-have-appeared-in-static-render-snapshot
  (let [operations (standard-operations)
        ctx (render-context operations)
        snapshot
        {:board
         [:main
          (rendered-operation-button
           ctx
           :request/claim
           "/operations/request/claim")]}
        later-render
        [:aside
         (rendered-operation-button
          ctx
          :request/reassign
          "/operations/request/reassign")]]
    (with-surfaced-application
      snapshot
      (fn [{:keys [assembly]}]
        (let [report
              (application/check-rendered-surface
               assembly
               :assignment-panel
               later-render)]
          (is (application/rendered-surface-valid? report))
          (is (= :assignment-panel
                 (get-in report [:analysis :surface])))
          (is (= :request/reassign
                 (get-in report [:analysis :affordances 0 :operation])))
          (is (= :request/reassign
                 (get-in report [:analysis :affordances 0 :route-id]))))))))

(deftest unsurfaced-application-can-enforce-an-actual-dynamic-render
  (let [operations (standard-operations)
        ctx (render-context operations)
        rendered
        [:main
         (rendered-operation-button
          ctx
          :request/cancel
          "/operations/request/cancel")]]
    (with-closed-application
      (fn [{:keys [assembly]}]
        (is (= #{:static-affordance-closure-not-yet-modeled}
               (set (map :kind (application/open-obligations assembly)))))
        (let [report
              (application/require-rendered-surface!
               assembly
               :request-board
               rendered)]
          (is (application/rendered-surface-valid? report))
          (is (= :request/cancel
                 (get-in report [:analysis :affordances 0 :operation])))
          (is (= #{}
                 (get-in report
                         [:analysis :affordances 0 :publication-topics]
                         #{}))))))))

(deftest rendered-surface-report-recognizer-rejects-derived-and-input-tampering
  (let [operations (standard-operations)
        ctx (render-context operations)
        rendered
        [:main
         (rendered-operation-button
          ctx
          :request/claim
          "/operations/request/claim")]]
    (with-closed-application
      (fn [{:keys [assembly]}]
        (let [report
              (application/require-rendered-surface!
               assembly
               :request-board
               rendered)]
          (is (application/rendered-surface-report? report))
          (doseq [forged
                  [(assoc-in report
                             [:analysis :guarantee]
                             :whole-application-preflight-closed)
                   (assoc-in report
                             [:analysis :affordances 0 :route-template]
                             "/forged")
                   (assoc-in report
                             [:analysis :surface]
                             :forged-surface)
                   (assoc-in report
                             [:analysis :rendered]
                             [:main "forged"])
                   (assoc report :valid? false)
                   (assoc report :extra :forged)]]
            (is (not (application/rendered-surface-report? forged)))))))))

(deftest stale-browser-artifact-invalidates-rendered-surface-report-and-blocks-response
  (let [operations (standard-operations)
        ctx (render-context operations)
        rendered
        [:main
         (rendered-operation-button
          ctx
          :request/claim
          "/operations/request/claim")]]
    (with-closed-application
      (fn [{:keys [assembly artifact-path]}]
        (let [report
              (application/require-rendered-surface!
               assembly
               :request-board
               rendered)
              calls (atom 0)]
          (is (application/rendered-surface-report? report))
          (spit artifact-path "console.log('stale');\n" :encoding "UTF-8")
          (is (not (application/rendered-surface-report? report)))
          (is (= :rendered-surface-preflight-failed
                 (error-kind
                  #(application/checked-rendered-response!
                    assembly
                    :request-board
                    (fn [_]
                      (swap! calls inc)
                      {:status 200})
                    rendered))))
          (is (= 0 @calls)))))))

(deftest stale-custom-receipt-invalidates-rendered-surface-report
  (let [operations (standard-operations)
        ctx (render-context operations)
        rendered
        [:main
         (rendered-operation-button
          ctx
          :request/claim
          "/operations/request/claim")]]
    (with-closed-application
      {:custom-receipt? true}
      (fn [{:keys [assembly receipt-path]}]
        (let [report
              (application/require-rendered-surface!
               assembly
               :request-board
               rendered)]
          (is (application/rendered-surface-report? report))
          (spit receipt-path "{:tampered :dynamic-receipt}\n" :encoding "UTF-8")
          (is (not (application/rendered-surface-report? report)))
          (let [failed
                (application/check-rendered-surface
                 assembly
                 :request-board
                 rendered)]
            (is (not (application/rendered-surface-valid? failed)))
            (is (contains? (error-kinds failed)
                           :invalid-application-assembly))))))))

(deftest dynamic-surface-validation-does-not-erase-producer-coverage-obligation
  (let [operations (standard-operations)
        ctx (render-context operations)
        rendered
        [:main
         (rendered-operation-button
          ctx
          :request/claim
          "/operations/request/claim")]]
    (with-surfaced-application
      {:board rendered}
      (fn [{:keys [assembly]}]
        (is (application/rendered-surface-valid?
             (application/check-rendered-surface
              assembly
              :board
              rendered)))
        (is (= #{:rendered-surface-enumeration-completeness-not-yet-modeled}
               (set (map :kind (application/open-obligations assembly)))))
        (is (= :application-runtime-backbone-preflight-closed
               (:guarantee (application/explain assembly))))))))
