(ns gesso.live.ui-optimistic-test
  (:require
   [clojure.edn :as edn]
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.identity :as identity]
   [gesso.choreo.project :as project]
   [gesso.live.optimistic.capability :as capability]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.ui :as ui]
   [rum.core :as rum]))

;; -----------------------------------------------------------------------------
;; Fixtures / Hiccup helpers
;; -----------------------------------------------------------------------------

(def ctx
  {:anti-forgery-token "anti-forgery-token"})

(def basis
  {:tx-id 42
   :system-time "2026-08-25T01:00:00Z"})

(def newer-basis
  {:tx-id 43
   :system-time "2026-08-25T01:00:01Z"})

(def request-scope
  [:request "request-1"])

(def optimistic-action
  {:operation :request/claim
   :arguments {:request-id "request-1"}
   :observed-basis basis
   :scope request-scope
   :fact-versions {:request/status 9}
   :target-id "request-card-request-1"
   :plan-key :request/claim
   :rollback-eligible? true
   :timeout-ms 5000
   :replace-owner? true
   :replace-execution? false})

(def claim-capability
  (capability/operation-capability
   {:operation :request/claim
    :plan-key :request/claim
    :rollback-eligible? true
    :timeout-ms 5000
    :replace-owner? true
    :replace-execution? false}))

(defn- one-role-plan
  [operation]
  (project/project
   (choreo/->choreography
    {:name :example/ui-operation-binding
     :initial :run
     :states
     {:run
      (choreo/local :browser operation :done)

      :done
      (choreo/return :done)}})
   :browser))

(def browser-plans
  {:request/claim
   (one-role-plan :request/claim)

   :request/cancel
   (one-role-plan :request/cancel)})

(def derived-capabilities
  (capability/operation-capabilities
   browser-plans
   {:request/claim
    {:rollback-eligible? true
     :timeout-ms 5000
     :replace-owner? true
     :replace-execution? false}}))

(def operation-ctx
  (ui/with-optimistic-operation-capabilities
   ctx
   derived-capabilities))

(def optimistic-binding
  {:arguments {:request-id "request-1"}
   :observed-basis basis
   :scope request-scope
   :fact-versions {:request/status 9}
   :target-id "request-card-request-1"})

(def command-id
  (identity/command-id "command-42"))

(def execution-id
  (identity/execution-id "execution-7"))

(def authoritative
  (protocol/authoritative
   {:presence :present
    :basis newer-basis
    :projection {:request/status :claimed
                 :request/claimed-by "helper-1"}}))

(def settlement
  (protocol/settlement
   {:command-id command-id
    :execution-id execution-id
    :resolution :confirmed
    :authoritative authoritative
    :outcome :request/claimed}))

(defn- error-data
  [f]
  (try
    (f)
    nil
    (catch Throwable ex
      (ex-data ex))))

(defn- error-kind
  [f]
  (:error/kind (error-data f)))

(defn- element-children
  [hiccup]
  (let [xs (rest hiccup)]
    (if (map? (first xs))
      (rest xs)
      xs)))

(defn- direct-child-by-tag
  [hiccup tag]
  (some #(when (and (vector? %)
                    (= tag (first %)))
           %)
        (element-children hiccup)))

(defn- button
  [hiccup]
  (direct-child-by-tag hiccup :button))

(defn- anti-forgery-input
  [hiccup]
  (some
   #(when (and (vector? %)
               (= :input (first %))
               (= "__anti-forgery-token"
                  (get-in % [1 :name])))
      %)
   (element-children hiccup)))

(defn- optimistic-attrs
  [hiccup]
  (second (button hiccup)))

(defn- decoded-action
  [hiccup]
  (some-> (get (optimistic-attrs hiccup)
               ui/optimistic-action-attr)
          edn/read-string))

;; -----------------------------------------------------------------------------
;; Ordinary post-button behavior remains ordinary
;; -----------------------------------------------------------------------------

(deftest ordinary-post-button-remains-unchanged-test
  (let [markup
        (ui/post-button
         ctx
         {:to "/increment"
          :target "counter-fragment"
          :label "+"})
        attrs (optimistic-attrs markup)]
    (is (= :form (first markup)))
    (is (= true (get-in markup [1 :data-gesso-live-post])))
    (is (= [:input
            {:type "hidden"
             :name "__anti-forgery-token"
             :value "anti-forgery-token"}]
           (anti-forgery-input markup)))
    (is (= "button" (:type attrs)))
    (is (= "/increment" (:hx-post attrs)))
    (is (= "#counter-fragment" (:hx-target attrs)))
    (is (= "innerHTML" (:hx-swap attrs)))
    (is (= ui/default-post-include (:hx-include attrs)))
    (is (= ui/default-post-sync (:hx-sync attrs)))
    (is (not (contains? attrs ui/optimistic-action-attr)))
    (is (nil? (direct-child-by-tag markup :template)))))

(deftest nil-or-false-optimism-emits-no-protocol-annotation-test
  (doseq [optimistic-value [nil false]]
    (let [markup
          (ui/post-button
           ctx
           {:to "/increment"
            :label "+"
            :optimistic optimistic-value})]
      (is (nil? (get (optimistic-attrs markup)
                     ui/optimistic-action-attr)))
      (is (nil? (direct-child-by-tag markup :template))))))

(deftest post-button-include-remains-additive-test
  (is (= "closest [data-gesso-live-post], #board-state, #selection-state"
         (:hx-include
          (optimistic-attrs
           (ui/post-button
            ctx
            {:to "/increment"
             :label "+"
             :include ["#board-state"
                       ["#selection-state"
                        "#board-state"]]}))))))


;; -----------------------------------------------------------------------------
;; Semantic Choreo operation binding
;; -----------------------------------------------------------------------------

(deftest choreo-operation-path-is-render-equivalent-to-explicit-capability-test
  (let [explicit
        (ui/post-button
         ctx
         {:to "/claim"
          :target "request-card-request-1"
          :swap "outerHTML"
          :include "#request-board-state"
          :label "Claim"
          :optimistic claim-capability
          :optimistic-binding optimistic-binding})
        semantic
        (ui/post-button
         operation-ctx
         {:to "/claim"
          :target "request-card-request-1"
          :swap "outerHTML"
          :include "#request-board-state"
          :label "Claim"
          :choreo/op :request/claim
          :optimistic-binding optimistic-binding})]
    (is (= explicit semantic))
    (is (= optimistic-action (decoded-action semantic)))
    (is (= :request/claim
           (:operation (decoded-action semantic))))
    (is (= :request/claim
           (:plan-key (decoded-action semantic))))
    (doseq [authority-key [:principal
                           :authority
                           :authorities
                           :command-id
                           :execution-id
                           :settlement]]
      (is (not (contains? (decoded-action semantic) authority-key))))))

(deftest operation-capability-context-installation-is-closed-test
  (let [installed
        (ui/with-optimistic-operation-capabilities
         (assoc ctx :app/value 7)
         derived-capabilities)]
    (is (= 7 (:app/value installed)))
    (is (= derived-capabilities
           (get installed
                ui/optimistic-operation-capabilities-context-key))))

  (testing "render context itself must be a map"
    (doseq [bad-context [nil [] :context]]
      (let [data
            (error-data
             #(ui/with-optimistic-operation-capabilities
               bad-context
               derived-capabilities))]
        (is (= :gesso.live.ui/optimistic-error
               (:error/type data)))
        (is (= :invalid-render-context
               (:error/kind data))))))

  (testing "malformed registries fail when installed, before any affordance renders"
    (doseq [bad-capabilities
            [nil
             {}
             {:request/claim
              (assoc claim-capability
                     :operation :request/cancel)}]]
      (let [data
            (error-data
             #(ui/with-optimistic-operation-capabilities
               ctx
               bad-capabilities))]
        (is (= :gesso.live.optimistic.capability/error
               (:error/type data)))
        (is (= :invalid-operation-capabilities
               (:error/kind data)))))))

(deftest choreo-operation-binding-requires-an-installed-capability-registry-test
  (let [data
        (error-data
         #(ui/post-button
           ctx
           {:to "/claim"
            :label "Claim"
            :choreo/op :request/claim
            :optimistic-binding optimistic-binding}))]
    (is (= :gesso.live.ui/optimistic-error
           (:error/type data)))
    (is (= :missing-operation-capabilities
           (:error/kind data)))
    (is (= :request/claim (:operation data)))
    (is (= ui/optimistic-operation-capabilities-context-key
           (:context-key data))))

  (testing "a directly tampered framework context key is revalidated at use"
    (let [data
          (error-data
           #(ui/post-button
             (assoc ctx
                    ui/optimistic-operation-capabilities-context-key
                    {:request/claim
                     (assoc claim-capability
                            :plan-key :request/cancel)})
             {:to "/claim"
              :label "Claim"
              :choreo/op :request/claim
              :optimistic-binding optimistic-binding}))]
      (is (= :gesso.live.optimistic.capability/error
             (:error/type data)))
      (is (= :invalid-operation-capabilities
             (:error/kind data))))))

(deftest choreo-operation-lookup-is-exact-and-exposes-the-closed-search-space-test
  (let [data
        (error-data
         #(ui/post-button
           operation-ctx
           {:to "/claim"
            :label "Claim"
            :choreo/op :request/claime
            :optimistic-binding optimistic-binding}))]
    (is (= :gesso.live.optimistic.capability/error
           (:error/type data)))
    (is (= :unknown-operation
           (:error/kind data)))
    (is (= :request/claime
           (:operation data)))
    (is (= #{:request/claim :request/cancel}
           (:available-operations data))))

  (testing "semantic operation ids remain closed keywords and are never coerced"
    (let [data
          (error-data
           #(ui/post-button
             operation-ctx
             {:to "/claim"
              :label "Claim"
              :choreo/op "request/claim"
              :optimistic-binding optimistic-binding}))]
      (is (= :gesso.live.optimistic.capability/error
             (:error/type data)))
      (is (= :invalid-keyword
             (:error/kind data)))
      (is (= "request/claim" (:value data))))))

(deftest choreo-operation-binding-is-required-and-cannot-redeclare-plan-policy-test
  (testing ":choreo/op always requires per-render binding data"
    (let [data
          (error-data
           #(ui/post-button
             operation-ctx
             {:to "/claim"
              :label "Claim"
              :choreo/op :request/claim}))]
      (is (= :gesso.live.ui/optimistic-error
             (:error/type data)))
      (is (= :missing-optimistic-binding
             (:error/kind data)))
      (is (= :request/claim (:operation data)))))

  (testing "binding cannot smuggle capability-owned plan/policy fields"
    (doseq [[key value]
            [[:plan-key :request/cancel]
             [:rollback-eligible? false]
             [:timeout-ms 1]
             [:principal :forged]]]
      (let [data
            (error-data
             #(ui/post-button
               operation-ctx
               {:to "/claim"
                :label "Claim"
                :choreo/op :request/claim
                :optimistic-binding
                (assoc optimistic-binding key value)}))]
        (is (= :gesso.live.optimistic.capability/error
               (:error/type data)))
        (is (= :unknown-fields
               (:error/kind data)))
        (is (= #{key} (:unknown data)))))))

(deftest choreo-operation-and-legacy-optimistic-declarations-are-mutually-exclusive-test
  (doseq [legacy-value [nil false claim-capability optimistic-action]]
    (let [data
          (error-data
           #(ui/post-button
             operation-ctx
             {:to "/claim"
              :label "Claim"
              :choreo/op :request/claim
              :optimistic legacy-value
              :optimistic-binding optimistic-binding}))]
      (is (= :gesso.live.ui/optimistic-error
             (:error/type data)))
      (is (= :conflicting-optimistic-declarations
             (:error/kind data)))
      (is (= :request/claim (:operation data)))
      (is (= legacy-value (:optimistic data))))))

(deftest fragment-call-shape-supports-semantic-choreo-operation-binding-test
  (let [fragment
        (ui/->fragment
         {:id "request-list"
          :src "/app/fragments/requests"
          :stream-url "/app/streams/requests"
          :swap "outerHTML"})
        markup
        (ui/post-button
         operation-ctx
         fragment
         {:to "/claim"
          :label "Claim"
          :choreo/op :request/claim
          :optimistic-binding optimistic-binding})
        attrs (optimistic-attrs markup)]
    (is (= "#request-list" (:hx-target attrs)))
    (is (= "outerHTML" (:hx-swap attrs)))
    (is (= optimistic-action (decoded-action markup)))))

;; -----------------------------------------------------------------------------
;; Rendered Choreo affordance enumeration / preflight metadata
;; -----------------------------------------------------------------------------

(defn- canonical-claim-button
  ([]
   (canonical-claim-button "/claim"))
  ([path]
   (ui/post-button
    operation-ctx
    {:to path
     :label "Claim"
     :choreo/op :request/claim
     :optimistic-binding optimistic-binding})))

(defn- explicit-claim-button
  [path]
  (ui/post-button
   ctx
   {:to path
    :label "Claim"
    :optimistic claim-capability
    :optimistic-binding optimistic-binding}))

(defn- button-index
  [markup]
  (some
   (fn [[index node]]
     (when (and (vector? node)
                (= :button (first node)))
       index))
   (map-indexed vector markup)))

(defn- update-button
  [markup f]
  (let [index (button-index markup)]
    (assoc markup index (f (nth markup index)))))

(defn- update-button-attrs
  [markup f]
  (update-button
   markup
   (fn [node]
     (assoc node 1 (f (second node))))))

(defn- affordance-error-data
  [markup]
  (error-data #(ui/rendered-choreo-affordances markup)))

(deftest rendered-choreo-affordance-is-derived-from-canonical-post-button-test
  (let [markup (canonical-claim-button)
        descriptor (first (ui/rendered-choreo-affordances markup))
        button-node (button markup)]
    (is (= 1 (count (ui/rendered-choreo-affordances markup))))
    (is (= {:gesso.live.ui/type ui/rendered-choreo-affordance-type
            :gesso.live.ui/version ui/rendered-choreo-affordance-version
            :kind :post-button
            :operation :request/claim
            :plan-key :request/claim
            :method :post
            :path "/claim"
            :render-path [3]}
           descriptor))
    (is (ui/rendered-choreo-affordance? descriptor))
    (is (= :request/claim
           (get (meta button-node)
                ui/choreo-affordance-metadata-key)))
    (is (= :request/claim (:operation (decoded-action markup))))
    (is (= :request/claim (:plan-key (decoded-action markup))))))

(deftest rendered-choreo-affordances-preserve-occurrences-and-structural-order-test
  (let [first-button (canonical-claim-button "/requests/1/claim")
        second-button (canonical-claim-button "/requests/2/claim")
        tree [:main
              [:section first-button]
              [:aside
               [:div second-button]
               first-button]]
        affordances (ui/rendered-choreo-affordances tree)]
    (is (= 3 (count affordances)))
    (is (= ["/requests/1/claim"
            "/requests/2/claim"
            "/requests/1/claim"]
           (mapv :path affordances)))
    (is (= [[1 1 3]
            [2 1 1 3]
            [2 2 3]]
           (mapv :render-path affordances)))
    (is (every? ui/rendered-choreo-affordance? affordances))))

(deftest only-canonical-choreo-operation-path-is-promoted-to-affordance-test
  (let [ordinary
        (ui/post-button ctx {:to "/ordinary" :label "Ordinary"})
        raw
        (ui/post-button
         ctx
         {:to "/raw"
          :label "Raw"
          :optimistic optimistic-action})
        explicit
        (explicit-claim-button "/explicit")
        canonical
        (canonical-claim-button "/canonical")
        affordances
        (ui/rendered-choreo-affordances
         [:div ordinary raw explicit canonical])]
    (is (= 1 (count affordances)))
    (is (= :request/claim (:operation (first affordances))))
    (is (= "/canonical" (:path (first affordances))))
    (is (nil? (get (meta (button ordinary)) ui/choreo-affordance-metadata-key)))
    (is (nil? (get (meta (button raw)) ui/choreo-affordance-metadata-key)))
    (is (nil? (get (meta (button explicit)) ui/choreo-affordance-metadata-key)))
    (is (= :request/claim
           (get (meta (button canonical)) ui/choreo-affordance-metadata-key)))))

(deftest canonical-affordance-metadata-does-not-change-rendered-html-test
  (let [canonical (canonical-claim-button)
        explicit (explicit-claim-button "/claim")]
    ;; Clojure equality deliberately ignores metadata, preserving the existing
    ;; render-equivalence contract.
    (is (= canonical explicit))
    (is (some? (get (meta (button canonical)) ui/choreo-affordance-metadata-key)))
    (is (nil? (get (meta (button explicit)) ui/choreo-affordance-metadata-key)))
    (is (= (rum/render-static-markup canonical)
           (rum/render-static-markup explicit)))
    (is (not (.contains
              (rum/render-static-markup canonical)
              "choreo-affordance")))))

(deftest rendered-choreo-affordance-predicate-is-closed-test
  (let [descriptor (first (ui/rendered-choreo-affordances
                           (canonical-claim-button)))]
    (is (ui/rendered-choreo-affordance? descriptor))
    (doseq [[k v]
            [[:operation "request/claim"]
             [:plan-key :request/cancel]
             [:method :get]
             [:path ""]
             [:render-path [-1]]
             [:kind :ordinary-button]
             [:gesso.live.ui/version 2]]]
      (is (not (ui/rendered-choreo-affordance? (assoc descriptor k v)))))
    (is (not (ui/rendered-choreo-affordance?
              (assoc descriptor :extra :forged))))))

(deftest tampered-affordance-operation-metadata-fails-closed-test
  (let [markup
        (update-button
         (canonical-claim-button)
         #(with-meta
            %
            (assoc (meta %)
                   ui/choreo-affordance-metadata-key
                   :request/cancel)))
        data (affordance-error-data markup)]
    (is (= :gesso.live.ui/affordance-error (:error/type data)))
    (is (= :rendered-affordance-operation-mismatch (:error/kind data)))
    (is (= :request/cancel (:operation data)))
    (is (= :request/claim (:action-operation data)))
    (is (= [3] (:render-path data)))))

(deftest malformed-affordance-operation-metadata-fails-before-correspondence-test
  (let [markup
        (update-button
         (canonical-claim-button)
         #(with-meta
            %
            (assoc (meta %)
                   ui/choreo-affordance-metadata-key
                   "request/claim")))
        data (affordance-error-data markup)]
    (is (= :gesso.live.ui/affordance-error (:error/type data)))
    (is (= :invalid-rendered-affordance-operation (:error/kind data)))
    (is (= "request/claim" (:operation data)))
    (is (= [3] (:render-path data)))))

(deftest tampered-affordance-plan-key-fails-closed-test
  (let [markup
        (update-button-attrs
         (canonical-claim-button)
         (fn [attrs]
           (assoc attrs
                  ui/optimistic-action-attr
                  (pr-str
                   (assoc (edn/read-string
                           (get attrs ui/optimistic-action-attr))
                          :plan-key :request/cancel)))))
        data (affordance-error-data markup)]
    (is (= :gesso.live.ui/affordance-error (:error/type data)))
    (is (= :rendered-affordance-plan-mismatch (:error/kind data)))
    (is (= :request/claim (:operation data)))
    (is (= :request/cancel (:plan-key data)))
    (is (= [3] (:render-path data)))))

(deftest malformed-affordance-action-encoding-fails-closed-test
  (doseq [[encoded expected-kind]
          [[nil :invalid-rendered-affordance-encoding]
           [42 :invalid-rendered-affordance-encoding]
           ["{" :invalid-rendered-affordance-encoding]]]
    (let [markup
          (update-button-attrs
           (canonical-claim-button)
           #(assoc % ui/optimistic-action-attr encoded))
          data (affordance-error-data markup)]
      (is (= :gesso.live.ui/affordance-error (:error/type data)))
      (is (= expected-kind (:error/kind data)))
      (is (= [3] (:render-path data))))))

(deftest malformed-affordance-action-fails-closed-test
  (let [markup
        (update-button-attrs
         (canonical-claim-button)
         #(assoc % ui/optimistic-action-attr
                 (pr-str {:operation :request/claim})))
        data (affordance-error-data markup)]
    (is (= :gesso.live.ui/affordance-error (:error/type data)))
    (is (= :invalid-rendered-affordance-action (:error/kind data)))
    (is (= :request/claim (:operation data)))
    (is (= [3] (:render-path data)))
    (is (= {:operation :request/claim} (:action data)))))

(deftest affordance-metadata-on-non-button-node-fails-closed-test
  (doseq [node
          [(with-meta [:a {:href "/claim"} "Claim"]
             {ui/choreo-affordance-metadata-key :request/claim})
           (with-meta [:button "Claim"]
             {ui/choreo-affordance-metadata-key :request/claim})]]
    (let [data (affordance-error-data [:div node])]
      (is (= :gesso.live.ui/affordance-error (:error/type data)))
      (is (= :invalid-rendered-affordance-button (:error/kind data)))
      (is (= :request/claim (:operation data)))
      (is (= [1] (:render-path data))))))

(deftest tampered-affordance-button-type-fails-closed-test
  (let [markup
        (update-button-attrs
         (canonical-claim-button)
         #(assoc % :type "submit"))
        data (affordance-error-data markup)]
    (is (= :gesso.live.ui/affordance-error (:error/type data)))
    (is (= :invalid-rendered-affordance-button (:error/kind data)))
    (is (= "submit" (:type data)))
    (is (= :request/claim (:operation data)))
    (is (= [3] (:render-path data)))))

(deftest missing-or-blank-affordance-post-path-fails-closed-test
  (doseq [path [nil "" "   "]]
    (let [markup
          (update-button-attrs
           (canonical-claim-button)
           #(assoc % :hx-post path))
          data (affordance-error-data markup)]
      (is (= :gesso.live.ui/affordance-error (:error/type data)))
      (is (= :missing-rendered-affordance-path (:error/kind data)))
      (is (= path (:path data)))
      (is (= :request/claim (:operation data)))
      (is (= [3] (:render-path data))))))

(deftest malformed-canonical-descendant-is-not-silently-skipped-test
  (let [good (canonical-claim-button "/good")
        bad
        (update-button-attrs
         (canonical-claim-button "/bad")
         #(dissoc % :hx-post))
        tree [:main good [:section bad] good]
        data (affordance-error-data tree)]
    (is (= :gesso.live.ui/affordance-error (:error/type data)))
    (is (= :missing-rendered-affordance-path (:error/kind data)))
    (is (= [2 1 3] (:render-path data)))
    (is (= :request/claim (:operation data)))))

;; -----------------------------------------------------------------------------
;; Protocol-v3 action annotation
;; -----------------------------------------------------------------------------

(deftest optimistic-capability-path-produces-the-same-v3-action-test
  (let [bound (capability/bind claim-capability optimistic-binding)]
    (is (= optimistic-action bound))
    (is (= bound
           (ui/optimistic-action claim-capability optimistic-binding)))
    (is (= (ui/optimistic-action optimistic-action)
           (ui/optimistic-action claim-capability optimistic-binding)))
    (is (= (ui/optimistic-action-attrs optimistic-action)
           (ui/optimistic-action-attrs claim-capability optimistic-binding)))))

(deftest post-button-capability-path-is-render-equivalent-to-bound-action-test
  (let [raw
        (ui/post-button
         ctx
         {:to "/claim"
          :target "request-card-request-1"
          :swap "outerHTML"
          :include "#request-board-state"
          :label "Claim"
          :optimistic optimistic-action})
        bound
        (ui/post-button
         ctx
         {:to "/claim"
          :target "request-card-request-1"
          :swap "outerHTML"
          :include "#request-board-state"
          :label "Claim"
          :optimistic claim-capability
          :optimistic-binding optimistic-binding})]
    (is (= raw bound))
    (is (= optimistic-action (decoded-action bound)))
    (is (nil? (direct-child-by-tag bound :template)))))

(deftest post-button-capability-binding-ownership-is-unambiguous-test
  (testing "a capability must be accompanied by per-render binding data"
    (is (= :missing-optimistic-binding
           (error-kind
            #(ui/post-button
              ctx
              {:to "/claim"
               :label "Claim"
               :optimistic claim-capability})))))

  (testing "binding data cannot be supplied without a capability"
    (doseq [optimistic-value [nil false]]
      (is (= :orphan-optimistic-binding
             (error-kind
              #(ui/post-button
                ctx
                {:to "/claim"
                 :label "Claim"
                 :optimistic optimistic-value
                 :optimistic-binding optimistic-binding}))))))

  (testing "raw action maps cannot be mixed with capability binding data"
    (is (= :unexpected-optimistic-binding
           (error-kind
            #(ui/post-button
              ctx
              {:to "/claim"
               :label "Claim"
               :optimistic optimistic-action
               :optimistic-binding optimistic-binding})))))

  (testing "a tagged but tampered capability is rejected by the capability owner"
    (let [data
          (error-data
           #(ui/post-button
             ctx
             {:to "/claim"
              :label "Claim"
              :optimistic (assoc claim-capability :operation "request/claim")
              :optimistic-binding optimistic-binding}))]
      (is (= :gesso.live.optimistic.capability/error
             (:error/type data)))
      (is (= :invalid-capability
             (:error/kind data))))))

(deftest fragment-call-shape-supports-capability-binding-test
  (let [fragment
        (ui/->fragment
         {:id "request-list"
          :src "/app/fragments/requests"
          :stream-url "/app/streams/requests"
          :swap "outerHTML"})
        markup
        (ui/post-button
         ctx
         fragment
         {:to "/claim"
          :label "Claim"
          :optimistic claim-capability
          :optimistic-binding optimistic-binding})
        attrs (optimistic-attrs markup)]
    (is (= "#request-list" (:hx-target attrs)))
    (is (= "outerHTML" (:hx-swap attrs)))
    (is (= optimistic-action (decoded-action markup)))))

(deftest optimistic-action-normalizes-the-closed-v3-binding-test
  (is (= optimistic-action
         (ui/optimistic-action optimistic-action)))

  (testing "required semantic fields remain required"
    (doseq [k ui/optimistic-action-required-keys]
      (is (= :missing-action-fields
             (error-kind
              #(ui/optimistic-action
                (dissoc optimistic-action k)))))))

  (testing "unsupported browser/security/correlation fields are rejected"
    (doseq [[k value]
            [[:command-id command-id]
             [:execution-id execution-id]
             [:principal (identity/principal "helper-1")]
             [:authority :browser]
             [:settlement settlement]
             [:template-name "old-v2-template"]
             [:transition :request/claim]
             [:content [:div "old projection"]]]]
      (is (= :unknown-action-fields
             (error-kind
              #(ui/optimistic-action
                (assoc optimistic-action k value))))))))

(deftest optimistic-action-validates-value-kinds-test
  (is (= :invalid-operation
         (error-kind
          #(ui/optimistic-action
            (assoc optimistic-action :operation "request/claim")))))
  (is (= :invalid-arguments
         (error-kind
          #(ui/optimistic-action
            (assoc optimistic-action :arguments [:request-id "request-1"])))))
  (is (= :invalid-action-option
         (error-kind
          #(ui/optimistic-action
            (assoc optimistic-action :target-id "   ")))))
  (is (= :invalid-action-option
         (error-kind
          #(ui/optimistic-action
            (assoc optimistic-action :plan-key nil)))))
  (doseq [k [:rollback-eligible? :replace-owner? :replace-execution?]]
    (is (= :invalid-action-option
           (error-kind
            #(ui/optimistic-action
              (assoc optimistic-action k :yes))))))
  (doseq [timeout [-1 2.5 "5000"]]
    (is (= :invalid-action-option
           (error-kind
            #(ui/optimistic-action
              (assoc optimistic-action :timeout-ms timeout)))))))

(deftest optimistic-action-delegates-portable-protocol-normalization-test
  (is (= :missing-basis
         (error-kind
          #(ui/optimistic-action
            (assoc optimistic-action :observed-basis nil)))))
  (is (= :invalid-scope
         (error-kind
          #(ui/optimistic-action
            (assoc optimistic-action :scope "   ")))))
  (is (= :invalid-fact-version-key
         (error-kind
          #(ui/optimistic-action
            (assoc optimistic-action :fact-versions {"request/status" 9}))))))

(deftest optimistic-action-attrs-are-portable-edn-not-a-command-test
  (let [attrs (ui/optimistic-action-attrs optimistic-action)
        encoded (get attrs ui/optimistic-action-attr)
        decoded (edn/read-string encoded)]
    (is (= {ui/optimistic-action-attr encoded}
           attrs))
    (is (= optimistic-action decoded))
    (is (not (contains? decoded :command-id)))
    (is (not (contains? decoded :execution-id)))
    (is (not (contains? decoded :principal)))
    (is (not (contains? decoded :authority)))
    (is (not (contains? decoded :settlement)))))

(deftest optimistic-action-attrs-reject-non-portable-host-values-test
  (let [host-value (java.time.Instant/parse "2026-08-25T01:00:00Z")]
    (is (= :non-portable-edn
           (error-kind
            #(ui/optimistic-action-attrs
              (assoc optimistic-action
                     :observed-basis
                     {:tx-id 42
                      :system-time host-value})))))))

(deftest post-button-emits-only-one-inert-v3-action-annotation-test
  (let [markup
        (ui/post-button
         ctx
         {:to "/app/requests/request-1/claim"
          :swap "none"
          :include "#request-board-state"
          :label "Claim"
          :button-attrs
          {:class "primary-button"
           :data-humanhelp-action "claim"}
          :optimistic optimistic-action})
        attrs (optimistic-attrs markup)]
    (is (= "/app/requests/request-1/claim" (:hx-post attrs)))
    (is (= "none" (:hx-swap attrs)))
    (is (= "closest [data-gesso-live-post], #request-board-state"
           (:hx-include attrs)))
    (is (= ui/default-post-sync (:hx-sync attrs)))
    (is (= "primary-button" (:class attrs)))
    (is (= "claim" (:data-humanhelp-action attrs)))
    (is (= optimistic-action (decoded-action markup)))

    (testing "the wrapper owns neither request nor protocol semantics"
      (is (nil? (get-in markup [1 :hx-post])))
      (is (nil? (get-in markup [1 ui/optimistic-action-attr]))))

    (testing "protocol v3 renders no speculative template into server Hiccup"
      (is (nil? (direct-child-by-tag markup :template))))))

(deftest framework-action-annotation-overrides-conflicting-caller-attrs-test
  (let [markup
        (ui/post-button
         ctx
         {:to "/claim"
          :label "Claim"
          :button-attrs
          {ui/optimistic-action-attr "{:operation :attacker/forged}"
           :data-app-owned "still-here"}
          :optimistic optimistic-action})
        attrs (optimistic-attrs markup)]
    (is (= "still-here" (:data-app-owned attrs)))
    (is (= optimistic-action (decoded-action markup)))))

(deftest optimism-does-not-rewrite-ordinary-htmx-request-mechanics-test
  (let [markup
        (ui/post-button
         ctx
         {:to "/real-endpoint"
          :target "authoritative-target"
          :swap "outerHTML"
          :sync "closest form:abort"
          :label "Claim"
          :button-attrs
          {:hx-post "/caller-endpoint"
           :hx-target "#caller-target"
           :hx-swap "none"
           :hx-include "#caller-include"
           :hx-sync "caller:drop"}
          :optimistic optimistic-action})
        attrs (optimistic-attrs markup)]
    ;; Existing post-button semantics deliberately allow ordinary button attrs
    ;; to override request attrs. The optimistic annotation does not alter that
    ;; precedence; it only protects its own protocol binding.
    (is (= "/caller-endpoint" (:hx-post attrs)))
    (is (= "#caller-target" (:hx-target attrs)))
    (is (= "none" (:hx-swap attrs)))
    (is (= "#caller-include" (:hx-include attrs)))
    (is (= "caller:drop" (:hx-sync attrs)))
    (is (= optimistic-action (decoded-action markup)))))

(deftest explicit-sync-disable-remains-supported-with-optimism-test
  (doseq [sync [nil false]]
    (is (nil?
         (:hx-sync
          (optimistic-attrs
           (ui/post-button
            ctx
            {:to "/claim"
             :label "Claim"
             :sync sync
             :optimistic optimistic-action})))))))

(deftest fragment-call-shape-controls-authoritative-target-without-changing-action-test
  (let [fragment
        (ui/->fragment
         {:id "request-list"
          :src "/app/fragments/requests"
          :stream-url "/app/streams/requests"
          :swap "outerHTML"})
        markup
        (ui/post-button
         ctx
         fragment
         {:to "/claim"
          :label "Claim"
          :optimistic optimistic-action})
        attrs (optimistic-attrs markup)]
    (is (= "#request-list" (:hx-target attrs)))
    (is (= "outerHTML" (:hx-swap attrs)))
    (is (= optimistic-action (decoded-action markup)))))

(deftest invalid-optimistic-value-fails-closed-test
  (doseq [value [true "claim" [:request/claim]]]
    (is (= :invalid-action
           (error-kind
            #(ui/post-button
              ctx
              {:to "/claim"
               :label "Claim"
               :optimistic value}))))))

;; -----------------------------------------------------------------------------
;; Trusted settlement response marker
;; -----------------------------------------------------------------------------

(deftest optimistic-settlement-marker-contains-one-closed-wire-settlement-test
  (let [marker (ui/optimistic-settlement-marker settlement)
        attrs (second marker)
        encoded (get attrs ui/optimistic-settlement-attr)
        wire (edn/read-string encoded)]
    (is (= :template (first marker)))
    (is (= 2 (count marker)))
    (is (= settlement
           (protocol/wire->settlement wire)))
    (is (= (protocol/settlement->wire settlement)
           wire))
    (is (= command-id
           (protocol/wire->command-id (:command-id wire))))
    (is (= execution-id
           (protocol/wire->execution-id (:execution-id wire))))))

(deftest optimistic-settlement-marker-validates-before-rendering-test
  (is (= :unknown-fields
         (error-kind
          #(ui/optimistic-settlement-marker
            (assoc settlement :browser-authority true)))))
  (is (= :invalid-settlement-resolution
         (error-kind
          #(ui/optimistic-settlement-marker
            (assoc settlement :resolution :superseded))))))

(deftest settlement-marker-does-not-turn-response-content-into-authority-test
  (let [marker (ui/optimistic-settlement-marker settlement)
        attrs (second marker)]
    (is (= #{ui/optimistic-settlement-attr}
           (set (keys attrs))))
    (is (nil? (nth marker 2 nil)))
    (is (not (contains? attrs :hx-swap-oob)))
    (is (not (contains? attrs :data-gesso-live-optimistic)))))

;; -----------------------------------------------------------------------------
;; Protocol-v2 UI surface stays dead
;; -----------------------------------------------------------------------------

(deftest protocol-v2-ui-surface-is-not-reintroduced-test
  (doseq [sym ['optimistic-template
               'canonical
               'canonical-attrs
               'optimistic-post-button
               'projection-template]]
    (is (nil? (ns-resolve 'gesso.live.ui sym))))

  (let [markup
        (ui/post-button
         ctx
         {:to "/claim"
          :label "Claim"
          :optimistic optimistic-action})
        text (pr-str markup)]
    (doseq [obsolete ["template-name"
                      "base-revision"
                      "pending-label"
                      "projection-mode"
                      "data-gesso-optimistic-protocol"
                      "data-gesso-optimistic-template"
                      "data-gesso-optimistic-transition"]]
      (is (not (.contains text obsolete))))))
