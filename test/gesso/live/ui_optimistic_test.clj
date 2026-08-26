(ns gesso.live.ui-optimistic-test
  (:require
   [clojure.edn :as edn]
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.identity :as identity]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.ui :as ui]))

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
;; Protocol-v3 action annotation
;; -----------------------------------------------------------------------------

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
