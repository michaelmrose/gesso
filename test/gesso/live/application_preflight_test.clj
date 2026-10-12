(ns gesso.live.application-preflight-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.acquisition-preflight :as acquisition]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.fixture
    :refer [error-data error-kind error-kinds warning-kinds with-temp-dir child-path
            html-response generic-query generic-render acquisition-assembly trusted-principal trusted-operation
            operation-assembly operation-acquisition-assembly standard-operation-acquisition record-artifact! application-options
            with-closed-application rendered-basis render-context rendered-operation-button custom-route-capabilities with-surfaced-application
            allow? standard-operations browser-plan browser-assembly
            route-capabilities execution-assembly]]
   [gesso.live.browser.build :as browser-build]
   [gesso.live.browser.preflight :as browser-preflight]
   [gesso.live.model :as model]
   [gesso.live.operation-acquisition-preflight :as operation-acquisition]
   [gesso.live.ui :as ui]
   [gesso.live.optimistic.capability :as capability]
   [gesso.live.optimistic.execution-preflight :as execution-preflight]
   [gesso.live.optimistic.preflight :as operation-preflight]
   [gesso.live.optimistic.route-preflight :as route-preflight]
   [gesso.live.optimistic.server :as server]))

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

;; =============================================================================
;; v642 whole-application cross-product substitution campaign
;; =============================================================================

(defn- variant-operations
  []
  {:request/claim
   (trusted-operation
    :request/claim
    {:name :fixture-b/request-claim
     :authority-role :authority-b
     :published-change-topics #{:request}})

   :request/reassign
   (trusted-operation
    :request/reassign
    {:name :fixture-b/request-reassign
     :authority-role :authority-b
     :published-change-topics
     #{:request :request-assignment :audit :external/audit}})

   :request/cancel
   (trusted-operation
    :request/cancel
    {:name :fixture-b/request-cancel
     :authority-role :authority-b
     :published-change-topics #{}})})

(defn- compiled-live-b
  []
  (model/compile-live-app
   {:response html-response
    :scopes
    {:request-toolbar
     {:topic :fixture-b/request-toolbar
      :id-key :request/location-id
      :authorized? allow?}

     :request-list
     {:topic :fixture-b/request-list
      :id-key :request/location-id
      :authorized? allow?}

     :audit-scope
     {:topic :fixture-b/audit
      :id-key :request/location-id
      :authorized? allow?}}

    :graph
    {:request
     [{:scope :request-toolbar
       :id-key :request/location-id}
      {:scope :request-list
       :id-key :request/location-id}]

     :request-assignment
     [{:scope :request-list
       :id-key :request/location-id}]

     :audit
     [{:scope :audit-scope
       :id-key :request/location-id}]}

    :fragments
    {:request-toolbar
     {:scope :request-toolbar
      :id-fn (fn [id] (str "request-toolbar-b-" id))
      :query generic-query
      :render generic-render
      :swap :outerHTML}

     :request-list
     {:scope :request-list
      :id-fn (fn [id] (str "request-list-b-" id))
      :query generic-query
      :render generic-render
      :swap :outerHTML}}}))

(defn- acquisition-assembly-b
  []
  (let [live-app (compiled-live-b)]
    (acquisition/require-acquisition-assembly!
     {:name :fixture-b/live-acquisition
      :live-app live-app
      :realizations
      {:request-toolbar
       (acquisition/require-acquisition-realization!
        {:name :fixture-b/request-toolbar-acquisition
         :live-app live-app
         :fragment :request-toolbar
         :fragment-route
         (acquisition/fragment-route
          {:fragment :request-toolbar
           :path "/b/fragments/request-toolbar"})
         :stream-route
         (acquisition/stream-route
          {:fragment :request-toolbar
           :path "/b/streams/request-toolbar"})})
       :request-list
       (acquisition/require-acquisition-realization!
        {:name :fixture-b/request-list-acquisition
         :live-app live-app
         :fragment :request-list
         :fragment-route
         (acquisition/fragment-route
          {:fragment :request-list
           :path "/b/fragments/request-list"})
         :stream-route
         (acquisition/stream-route
          {:fragment :request-list
           :path "/b/streams/request-list"})})}})))

(defn- operation-acquisition-b
  []
  (operation-acquisition/require-operation-acquisition-assembly!
   {:name :fixture-b/application-operation-acquisition
    :execution-assembly (execution-assembly (variant-operations))
    :acquisition-assembly (acquisition-assembly-b)}))

(deftest cross-product-campaign-establishes-two-independently-valid-distinct-products
  (let [a @standard-operation-acquisition
        b (operation-acquisition-b)]
    (is (operation-acquisition/operation-acquisition-assembly? a))
    (is (operation-acquisition/operation-acquisition-assembly? b))
    (is (not= (get-in a [:execution-assembly :route-assembly :operation-assembly :browser-assembly])
              (get-in b [:execution-assembly :route-assembly :operation-assembly :browser-assembly])))
    (is (not= (:acquisition-assembly a) (:acquisition-assembly b)))
    (is (= (execution-preflight/published-change-topics (:execution-assembly a))
           (execution-preflight/published-change-topics (:execution-assembly b))))))

(deftest cross-product-campaign-rejects-browser-assembly-from-another-valid-operation-product
  (let [a @standard-operation-acquisition
        b (operation-acquisition-b)
        browser-b
        (get-in b [:execution-assembly :route-assembly :operation-assembly :browser-assembly])
        spliced
        (assoc-in
         a
         [:execution-assembly :route-assembly :operation-assembly :browser-assembly]
         browser-b)]
    (is (browser-preflight/assembly-manifest? browser-b))
    (is (not (operation-preflight/operation-assembly?
              (get-in spliced [:execution-assembly :route-assembly :operation-assembly]))))
    (is (not (operation-acquisition/operation-acquisition-assembly? spliced)))))

(deftest cross-product-campaign-rejects-route-assembly-from-another-valid-execution-product
  (let [a @standard-operation-acquisition
        b (operation-acquisition-b)
        route-b (get-in b [:execution-assembly :route-assembly])
        spliced (assoc-in a [:execution-assembly :route-assembly] route-b)]
    (is (route-preflight/route-assembly? route-b))
    (is (not (execution-preflight/execution-assembly?
              (:execution-assembly spliced))))
    (is (not (operation-acquisition/operation-acquisition-assembly? spliced)))))

(deftest cross-product-campaign-rejects-prepared-server-from-another-valid-execution-product
  (let [a @standard-operation-acquisition
        b (operation-acquisition-b)
        server-b (get-in b [:execution-assembly :server])
        spliced (assoc-in a [:execution-assembly :server] server-b)]
    (is (server/server? server-b))
    (is (not (execution-preflight/execution-assembly?
              (:execution-assembly spliced))))
    (is (not (operation-acquisition/operation-acquisition-assembly? spliced)))))

(deftest cross-product-campaign-rejects-fragment-realization-from-another-compiled-live-application
  (let [a @standard-operation-acquisition
        b (operation-acquisition-b)
        realization-b
        (get-in b [:acquisition-assembly :realizations :request-list])
        spliced
        (assoc-in
         a
         [:acquisition-assembly :realizations :request-list]
         realization-b)]
    (is (acquisition/acquisition-realization? realization-b))
    (is (not (acquisition/acquisition-assembly?
              (:acquisition-assembly spliced))))
    (is (not (operation-acquisition/operation-acquisition-assembly? spliced)))))

(deftest cross-product-campaign-rejects-valid-artifact-from-a-different-browser-product
  (with-temp-dir
    (fn [dir]
      (let [a @standard-operation-acquisition
            b (operation-acquisition-b)
            artifact-a (child-path dir "application-a.js")]
        (record-artifact! a artifact-a)
        (let [report
              (application/check-application-assembly
               (application-options b artifact-a))]
          (is (application/report? report))
          (is (not (application/valid? report)))
          (is (contains? (error-kinds report)
                         :browser-artifact-verification-failed)))))))

(deftest cross-product-campaign-allows-semantically-compatible-execution-live-recomposition
  (let [a @standard-operation-acquisition
        b (operation-acquisition-b)
        recomposed
        (operation-acquisition/require-operation-acquisition-assembly!
         {:name :fixture/recomposed-b-execution-a-live
          :execution-assembly (:execution-assembly b)
          :acquisition-assembly (:acquisition-assembly a)})]
    (is (operation-acquisition/operation-acquisition-assembly? recomposed))
    (is (= (execution-preflight/published-change-topics (:execution-assembly b))
           (execution-preflight/published-change-topics (:execution-assembly recomposed))))
    (is (= #{:request-toolbar :request-list}
           (get (operation-acquisition/affected-fragments recomposed)
                :request/claim)))
    (is (= (:acquisition-assembly a)
           (:acquisition-assembly recomposed)))))

(deftest cross-product-campaign-allows-semantically-compatible-live-substitution-with-different-physical-routes
  (let [a @standard-operation-acquisition
        b (operation-acquisition-b)
        recomposed
        (operation-acquisition/require-operation-acquisition-assembly!
         {:name :fixture/recomposed-a-execution-b-live
          :execution-assembly (:execution-assembly a)
          :acquisition-assembly (:acquisition-assembly b)})]
    (is (operation-acquisition/operation-acquisition-assembly? recomposed))
    (is (= #{:request-toolbar :request-list}
           (get (operation-acquisition/affected-fragments recomposed)
                :request/claim)))
    (is (= "/b/fragments/request-list"
           (get-in recomposed
                   [:acquisition-assembly
                    :realizations
                    :request-list
                    :fragment-route
                    :path])))))
