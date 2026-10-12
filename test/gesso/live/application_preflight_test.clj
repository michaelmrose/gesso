(ns gesso.live.application-preflight-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.fixture
    :refer [error-kind error-kinds warning-kinds with-temp-dir child-path
            trusted-operation standard-operation-acquisition record-artifact!
            application-options render-context rendered-operation-button
            custom-route-capabilities with-surfaced-application
            standard-operations route-capabilities]]
   [gesso.live.ui :as ui]))

(deftest malformed-framework-affordance-metadata-remains-fail-closed
  (let [forged
        (with-meta
          [:button {:hx-post "/operations/request/claim"} "Claim"]
          {ui/choreo-affordance-metadata-key :request/claim})]
    (with-surfaced-application
      {:request-board forged}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (not (application/valid? report)))
        ;; v661 already refuses malformed framework metadata rather than letting
        ;; the node disappear into ordinary HTMX.  A later hardening revision may
        ;; additionally retain the semantic-route collision diagnostic alongside
        ;; this scanner-local cause; this regression deliberately forbids only
        ;; accidental acceptance.
        (is (contains? (error-kinds report)
                       :rendered-affordance-scan-failed))
        (let [error
              (first
               (filter #(= :rendered-affordance-scan-failed (:kind %))
                       (:errors report)))]
          (is (= :gesso.live.ui/affordance-error (:cause-type error)))
          (is (= :invalid-rendered-affordance-encoding (:cause-kind error))))))))

(deftest supplied-rendered-surfaces-close-every-discovered-affordance-relative-to-the-snapshot
  (let [operations (standard-operations)
        ctx (render-context operations)
        surfaces
        {:request-board
         [:main
          (rendered-operation-button ctx :request/claim "/operations/request/claim")
          (rendered-operation-button ctx :request/cancel "/operations/request/cancel")]
         :request-toolbar
         [:nav
          (rendered-operation-button ctx :request/reassign "/operations/request/reassign")]}]
    (with-surfaced-application
      surfaces
      (fn [{:keys [report assembly]}]
        (let [explanation (application/explain assembly)
              obligation (first (application/open-obligations assembly))]
          (is (application/valid? report))
          (is (application/application-assembly? assembly))
          (is (= :closed-relative-to-supplied-rendered-surfaces
                 (get-in report [:analysis :affordance-closure :status])))
          (is (= application/rendered-affordance-guarantee
                 (get-in report [:analysis :affordance-closure :guarantee])))
          (is (= 2 (get-in report [:analysis :affordance-closure :surface-count])))
          (is (= 3 (get-in report [:analysis :affordance-closure :affordance-count])))
          (is (= #{:request-board :request-toolbar}
                 (:rendered-surface-names explanation)))
          (is (= :rendered-surface-enumeration-completeness-not-yet-modeled
                 (:kind obligation)))
          (is (= :application->rendered-surfaces (:edge obligation)))
          (is (= :open (:status obligation)))
          (is (= #{:rendered-surface-enumeration-completeness-not-yet-modeled
                   :published-change-topics-not-consumed-by-live-app}
                 (warning-kinds report)))
          (is (not= :whole-application-preflight-closed
                    (:guarantee explanation))))))))

(deftest repeated-operation-affordances-across-surfaces-remain-distinct-occurrences
  (let [operations (standard-operations)
        ctx (render-context operations)
        surfaces
        {:alpha
         [:section
          (rendered-operation-button ctx :request/claim "/operations/request/claim")]
         :beta
         [:section
          (rendered-operation-button ctx :request/claim "/operations/request/claim")
          (rendered-operation-button ctx :request/claim "/operations/request/claim")]}]
    (with-surfaced-application
      surfaces
      (fn [{:keys [assembly]}]
        (let [claim (application/explain-operation assembly :request/claim)
              affordances (:rendered-affordances claim)]
          (is (= 3 (count affordances)))
          (is (= [:alpha :beta :beta] (mapv :surface affordances)))
          (is (= 3 (count (set (map (juxt :surface :render-path) affordances)))))
          (is (every? #(= :request/claim (:operation %)) affordances))
          (is (every? #(= :request/claim (:route-id %)) affordances)))))))

(deftest parameterized-route-template-matches-concrete-rendered-path-and-ignores-query-fragment
  (let [operations (standard-operations)
        ctx (render-context operations)
        routes
        (custom-route-capabilities
         {:request/claim
          {:operation :request/claim
           :path "/requests/:request-id/claim"}
          :request/reassign
          {:operation :request/reassign
           :path "/operations/request/reassign"}
          :request/cancel
          {:operation :request/cancel
           :path "/operations/request/cancel"}})
        surfaces
        {:board
         [:main
          (rendered-operation-button
           ctx
           :request/claim
           "/requests/request-1/claim?from=board#card")]}]
    (with-surfaced-application
      {:operations operations
       :route-capabilities routes
       :rendered-surfaces surfaces}
      (fn [{:keys [report assembly]}]
        (is (application/valid? report))
        (let [affordance
              (first (:rendered-affordances
                      (application/explain-operation assembly :request/claim)))]
          (is (= "/requests/request-1/claim?from=board#card" (:path affordance)))
          (is (= "/requests/:request-id/claim" (:route-template affordance)))
          (is (= :request/claim (:route-id affordance)))
          (is (= :htmx (:required-transport affordance))))))))

(deftest rendered-affordance-wrong-concrete-path-fails-with-local-route-evidence
  (let [operations (standard-operations)
        ctx (render-context operations)
        routes
        (custom-route-capabilities
         {:request/claim
          {:operation :request/claim
           :path "/requests/:request-id/claim"}
          :request/reassign
          {:operation :request/reassign
           :path "/operations/request/reassign"}
          :request/cancel
          {:operation :request/cancel
           :path "/operations/request/cancel"}})
        surfaces
        {:board
         [:main
          (rendered-operation-button
           ctx
           :request/claim
           "/requests/request-1/cancel")]}]
    (with-surfaced-application
      {:operations operations
       :route-capabilities routes
       :rendered-surfaces surfaces}
      (fn [{:keys [report assembly]}]
        (is (nil? assembly))
        (is (not (application/valid? report)))
        (is (= #{:rendered-affordance-path-mismatch}
               (error-kinds report)))
        (let [error (first (:errors report))]
          (is (= :board (:surface error)))
          (is (= :request/claim (:operation error)))
          (is (= :post (:method error)))
          (is (= "/requests/request-1/cancel" (:path error)))
          (is (= ["/requests/:request-id/claim"] (:route-templates error))))))))

(deftest rendered-affordance-method-mismatch-is-distinct-from-path-mismatch
  (let [operations (standard-operations)
        ctx (render-context operations)
        routes
        (custom-route-capabilities
         {:request/claim
          {:operation :request/claim
           :method :put
           :path "/requests/:request-id/claim"}
          :request/reassign
          {:operation :request/reassign
           :path "/operations/request/reassign"}
          :request/cancel
          {:operation :request/cancel
           :path "/operations/request/cancel"}})
        surfaces
        {:board
         [:main
          (rendered-operation-button
           ctx
           :request/claim
           "/requests/request-1/claim")]}]
    (with-surfaced-application
      {:operations operations
       :route-capabilities routes
       :rendered-surfaces surfaces}
      (fn [{:keys [report]}]
        (is (not (application/valid? report)))
        (is (= #{:rendered-affordance-method-mismatch}
               (error-kinds report)))
        (let [error (first (:errors report))]
          (is (= :post (:method error)))
          (is (= :put (get-in error [:declared-routes 0 :method]))))))))

(deftest rendered-affordance-for-operation-outside-assembled-slice-fails-with-available-operations
  (let [operations (standard-operations)
        extra-operation
        (trusted-operation :request/archive {:published-change-topics #{:request}})
        render-operations (assoc operations :request/archive extra-operation)
        ctx (render-context render-operations)
        surfaces
        {:board
         [:main
          (rendered-operation-button
           ctx
           :request/archive
           "/operations/request/archive")]}]
    (with-surfaced-application
      surfaces
      (fn [{:keys [report]}]
        (is (not (application/valid? report)))
        (is (= #{:rendered-affordance-unknown-operation}
               (error-kinds report)))
        (let [error (first (:errors report))]
          (is (= :request/archive (:operation error)))
          (is (= #{:request/claim :request/reassign :request/cancel}
                 (:available-operations error))))))))

(deftest ambiguous-trusted-route-templates-fail-at-rendered-affordance-join
  (let [operations (standard-operations)
        ctx (render-context operations)
        routes
        (custom-route-capabilities
         {:request/claim-by-request-id
          {:operation :request/claim
           :path "/requests/:request-id/claim"}
          :request/claim-by-id
          {:operation :request/claim
           :path "/requests/:id/claim"}
          :request/reassign
          {:operation :request/reassign
           :path "/operations/request/reassign"}
          :request/cancel
          {:operation :request/cancel
           :path "/operations/request/cancel"}})
        surfaces
        {:board
         [:main
          (rendered-operation-button
           ctx
           :request/claim
           "/requests/request-1/claim")]}]
    (with-surfaced-application
      {:operations operations
       :route-capabilities routes
       :rendered-surfaces surfaces}
      (fn [{:keys [report]}]
        (is (not (application/valid? report)))
        (is (= #{:ambiguous-rendered-affordance-route}
               (error-kinds report)))
        (let [error (first (:errors report))]
          (is (= 2 (count (:matching-routes error))))
          (is (= #{:request/claim-by-request-id :request/claim-by-id}
                 (set (map :route-id (:matching-routes error))))))))))

(deftest one-malformed-surface-does-not-hide-valid-affordances-from-other-surfaces
  (let [operations (standard-operations)
        ctx (render-context operations)
        valid-button
        (rendered-operation-button ctx :request/claim "/operations/request/claim")
        bad-button
        (rendered-operation-button ctx :request/cancel "/wrong/cancel")
        surfaces {:good [:main valid-button]
                  :bad [:main bad-button]}]
    (with-surfaced-application
      surfaces
      (fn [{:keys [report]}]
        (is (not (application/valid? report)))
        (is (= #{:rendered-affordance-path-mismatch}
               (error-kinds report)))
        (is (= :bad (:surface (first (:errors report)))))
        (let [affordances (get-in report [:analysis :affordances])
              good (some #(when (= :good (:surface %)) %) affordances)
              bad (some #(when (= :bad (:surface %)) %) affordances)]
          (is (= :request/claim (:operation good)))
          (is (= :request/claim (:route-id good)))
          (is (= :request/cancel (:operation bad)))
          (is (nil? (:route-id bad))))))))

(deftest malformed-canonical-affordance-in-one-surface-fails-scan-with-surface-local-cause
  (let [operations (standard-operations)
        ctx (render-context operations)
        good (rendered-operation-button ctx :request/claim "/operations/request/claim")
        canonical (rendered-operation-button ctx :request/cancel "/operations/request/cancel")
        button-index
        (first
         (keep-indexed
          (fn [index node]
            (when (and (vector? node) (= :button (first node))) index))
          canonical))
        malformed
        (update canonical button-index
                (fn [node]
                  (with-meta node
                    (assoc (meta node)
                           ui/choreo-affordance-metadata-key
                           :request/claim))))
        surfaces {:good [:main good]
                  :bad [:main malformed]}]
    (with-surfaced-application
      surfaces
      (fn [{:keys [report]}]
        (is (not (application/valid? report)))
        (testing "malformed metadata and its uncertified physical action both fail closed"
          (is (= [:rendered-affordance-scan-failed
                  :rendered-choreo-physical-request-mismatch]
                 (mapv :kind (:errors report))))
          (let [[scan-error physical-error] (:errors report)]
            (is (= :bad (:surface scan-error)))
            (is (= :gesso.live.ui/affordance-error (:cause-type scan-error)))
            (is (keyword? (:cause-kind scan-error)))
            (is (= :bad (:surface physical-error)))
            (is (= :post (:method physical-error)))
            (is (= :hx-post (:request-source physical-error)))
            (is (= "/operations/request/cancel" (:path physical-error)))
            (is (empty? (:canonical-affordances physical-error)))
            (is (= #{:request/cancel}
                   (set (map :operation (:matching-routes physical-error)))))))
        (testing "the valid surface remains independently discoverable"
          (is (= :request/claim
                 (:operation
                  (some #(when (= :good (:surface %)) %)
                        (get-in report [:analysis :affordances]))))))))))

(deftest rendered-surface-report-and-assembly-tampering-fail-rederivation
  (let [operations (standard-operations)
        ctx (render-context operations)
        surfaces
        {:board
         [:main
          (rendered-operation-button ctx :request/claim "/operations/request/claim")]}]
    (with-surfaced-application
      surfaces
      (fn [{:keys [report assembly]}]
        (let [forged-affordance-report
              (assoc-in report
                        [:analysis :affordances 0 :route-template]
                        "/forged")
              forged-closure-report
              (assoc-in report
                        [:analysis :affordance-closure :status]
                        :whole-application-closed)
              tampered-surface
              {:board
               [:main
                (rendered-operation-button
                 ctx
                 :request/claim
                 "/wrong/claim")]}
              tampered-assembly
              (assoc assembly :rendered-surfaces tampered-surface)]
          (is (not (application/report? forged-affordance-report)))
          (is (not (application/report? forged-closure-report)))
          (is (not (application/application-assembly? tampered-assembly)))
          (is (= :invalid-application-input
                 (error-kind #(application/open-obligations tampered-assembly)))))))))

(deftest surfaced-assembly-stores-render-snapshot-not-derived-affordance-registry
  (let [operations (standard-operations)
        ctx (render-context operations)
        surfaces
        {:board
         [:main
          (rendered-operation-button ctx :request/claim "/operations/request/claim")]}]
    (with-surfaced-application
      surfaces
      (fn [{:keys [assembly]}]
        (is (= surfaces (:rendered-surfaces assembly)))
        (is (= #{:gesso.live.application-preflight/type
                 :gesso.live.application-preflight/version
                 :name
                 :operation-acquisition-assembly
                 :browser-artifact-path
                 :browser-receipt-path
                 :rendered-surfaces}
               (set (keys assembly))))
        (is (not (contains? assembly :affordances)))
        (is (not (contains? assembly :affordance-closure)))
        (is (not (contains? assembly :operations)))
        (is (not (application/application-assembly?
                  (assoc assembly :affordances []))))))))

(deftest rendered-surfaces-option-must-be-non-empty-keyed-snapshot
  (with-temp-dir
    (fn [dir]
      (let [operation-acquisition' @standard-operation-acquisition
            artifact-path (child-path dir "gesso-live.js")]
        (record-artifact! operation-acquisition' artifact-path)
        (doseq [rendered-surfaces
                [{}
                 {"not-keyword" [:main]}
                 {:board nil}]]
          (is (= :invalid-rendered-surfaces
                 (error-kind
                  #(application/check-application-assembly
                    (assoc
                     (application-options operation-acquisition' artifact-path)
                     :rendered-surfaces rendered-surfaces))))))))))

(deftest custom-receipt-currentness-remains-fail-closed-with-rendered-surfaces
  (let [operations (standard-operations)
        ctx (render-context operations)
        surfaces
        {:board
         [:main
          (rendered-operation-button ctx :request/claim "/operations/request/claim")]}]
    (with-surfaced-application
      {:operations operations
       :route-capabilities (route-capabilities operations)
       :rendered-surfaces surfaces
       :custom-receipt? true}
      (fn [{:keys [receipt-path report assembly]}]
        (is (application/valid? report))
        (is (application/application-assembly? assembly))
        (spit receipt-path "{:tampered :surface-receipt}\n" :encoding "UTF-8")
        (is (not (application/report? report)))
        (is (not (application/application-assembly? assembly)))))))
