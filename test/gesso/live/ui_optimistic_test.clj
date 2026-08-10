(ns gesso.live.ui-optimistic-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.optimistic.server :as optimistic]
   [gesso.live.ui :as ui]))

;; -----------------------------------------------------------------------------
;; Fixtures / Hiccup helpers
;; -----------------------------------------------------------------------------

(def ctx
  {:anti-forgery-token "anti-forgery-token"})

(def request-scope
  [:request "request-1"])

(def request-wire-scope
  (protocol/wire-scope request-scope))

(def pending-card
  [:details
   {:data-request-card "request-1"
    :open true}
   [:summary "Claimed"]
   [:div "Confirming…"]])

(def optimistic-config
  {:template-name "request-1-claim"
   :transition :request/claim
   :scope request-scope
   :base-revision 7
   :target "closest [data-request-card]"
   :pending-label "Claiming…"
   :content pending-card})

(defn- form-attrs
  [hiccup]
  (second hiccup))

(defn- form-children
  [hiccup]
  (drop 2 hiccup))

(defn- child-by-tag
  [hiccup tag]
  (some #(when (and (vector? %)
                    (= tag (first %)))
           %)
        (form-children hiccup)))

(defn- button
  [hiccup]
  (child-by-tag hiccup :button))

(defn- template
  [hiccup]
  (child-by-tag hiccup :template))

(defn- anti-forgery-input
  [hiccup]
  (some
   #(when (and (vector? %)
               (= :input (first %))
               (= "__anti-forgery-token"
                  (get-in % [1 :name])))
      %)
   (form-children hiccup)))

;; -----------------------------------------------------------------------------
;; Ordinary post-button behavior remains unchanged
;; -----------------------------------------------------------------------------

(deftest post-button-preserves-existing-defaults-test
  (let [markup
        (ui/post-button
         ctx
         {:to "/increment"
          :target "counter-fragment"
          :label "+"})
        button' (button markup)]
    (testing "post-button uses a lightweight anti-forgery wrapper"
      (is (= :form (first markup)))
      (is (= true
             (:data-gesso-live-post
              (form-attrs markup))))
      (is (= [:input
              {:type "hidden"
               :name "__anti-forgery-token"
               :value "anti-forgery-token"}]
             (anti-forgery-input markup))))

    (testing "the actual button owns the HTMX request"
      (is (= {:type "button"
              :hx-post "/increment"
              :hx-swap "innerHTML"
              :hx-include "closest [data-gesso-live-post]"
              :hx-sync "closest [data-gesso-live-fragment]:drop"
              :hx-target "#counter-fragment"}
             (second button')))
      (is (= "+" (nth button' 2))))

    (testing "ordinary post buttons emit no optimistic template"
      (is (nil? (template markup))))))

(deftest post-button-nil-or-false-optimistic-is-ordinary-test
  (doseq [optimistic-value [nil false]]
    (let [markup
          (ui/post-button
           ctx
           {:to "/increment"
            :label "+"
            :optimistic optimistic-value})
          button-attrs (second (button markup))]
      (is (nil?
           (get button-attrs protocol/template-attr)))
      (is (nil? (template markup)))
      (is (= ui/default-post-sync
             (:hx-sync button-attrs))))))

(deftest post-button-additive-include-test
  (testing "one additional selector is appended to the wrapper-form selector"
    (is (= "closest [data-gesso-live-post], #board-state"
           (get-in
            (ui/post-button
             ctx
             {:to "/increment"
              :label "+"
              :include "#board-state"})
            [3 1 :hx-include]))))

  (testing "nested selector collections are flattened and deduplicated"
    (is (= "closest [data-gesso-live-post], #board-state, #selection-state"
           (get-in
            (ui/post-button
             ctx
             {:to "/increment"
              :label "+"
              :include
              ["#board-state"
               ["#selection-state"
                "#board-state"]]})
            [3 1 :hx-include])))))

(deftest post-button-include-validation-test
  (testing "blank selectors are rejected"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":include selectors must not be blank"
         (ui/post-button
          ctx
          {:to "/increment"
           :label "+"
           :include "   "}))))

  (testing "non-string selector values are rejected"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":include must be"
         (ui/post-button
          ctx
          {:to "/increment"
           :label "+"
           :include :board-state})))))

;; -----------------------------------------------------------------------------
;; Optimistic button integration
;; -----------------------------------------------------------------------------

(deftest post-button-with-optimistic-config-test
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
          :optimistic optimistic-config})
        button' (button markup)
        button-attrs (second button')
        template' (template markup)]
    (testing "the actual type=button control is the request owner"
      (is (= :button (first button')))
      (is (= "button" (:type button-attrs)))
      (is (= "/app/requests/request-1/claim"
             (:hx-post button-attrs)))
      (is (= "none" (:hx-swap button-attrs)))
      (is (= "closest [data-gesso-live-post], #request-board-state"
             (:hx-include button-attrs)))
      (is (= "closest [data-request-card]:drop"
             (:hx-sync button-attrs)))
      (is (= "primary-button"
             (:class button-attrs)))
      (is (= "claim"
             (:data-humanhelp-action button-attrs))))

    (testing "protocol-v2 command attrs live on that request owner"
      (is (= "2"
             (get button-attrs
                  protocol/protocol-attr)))
      (is (= "request/claim"
             (get button-attrs
                  protocol/transition-attr)))
      (is (= "request-1-claim"
             (get button-attrs
                  protocol/template-attr)))
      (is (= "closest [data-request-card]"
             (get button-attrs
                  protocol/target-attr)))
      (is (= request-wire-scope
             (get button-attrs
                  protocol/scope-attr)))
      (is (= "i:7"
             (get button-attrs
                  protocol/base-revision-attr)))
      (is (= "Claiming…"
             (get button-attrs
                  protocol/pending-label-attr)))
      (is (= "provisional"
             (get button-attrs
                  protocol/projection-mode-attr))))

    (testing "the matching projection template is a sibling in the same wrapper form"
      (is (= :template (first template')))
      (is (= pending-card (nth template' 2)))
      (is (= {:data-gesso-optimistic-protocol "2"
              :data-gesso-optimistic-transition "request/claim"
              :data-gesso-optimistic-template "request-1-claim"
              :data-gesso-optimistic-scope request-wire-scope
              :data-gesso-optimistic-mode "provisional"}
             (second template'))))))

(deftest post-button-wrapper-does-not-own-request-or-optimistic-protocol-test
  (let [markup
        (ui/post-button
         ctx
         {:to "/claim"
          :label "Claim"
          :optimistic optimistic-config})
        wrapper-attrs (form-attrs markup)]
    (testing "wrapper form owns only wrapper/app attrs, not the HTMX POST"
      (is (= true
             (:data-gesso-live-post wrapper-attrs)))
      (is (nil? (:hx-post wrapper-attrs)))
      (is (nil? (:hx-target wrapper-attrs)))
      (is (nil? (:hx-sync wrapper-attrs))))

    (testing "wrapper form carries no optimistic protocol attributes"
      (doseq [k protocol/reserved-attrs]
        (is (not (contains? wrapper-attrs k)))))))

(deftest post-button-optimistic-inherits-top-level-target-test
  (let [markup
        (ui/post-button
         ctx
         {:to "/claim"
          :target "closest [data-request-card]"
          :label "Claim"
          :optimistic
          {:template-name "request-1-claim"
           :transition :request/claim
           :scope request-scope
           :base-revision 7
           :pending-label "Claiming…"
           :content pending-card}})
        button-attrs (second (button markup))]
    (testing "top-level target remains the HTMX target"
      (is (= "closest [data-request-card]"
             (:hx-target button-attrs))))

    (testing "missing optimistic target inherits the top-level target"
      (is (= "closest [data-request-card]"
             (get button-attrs
                  protocol/target-attr)))
      (is (= "closest [data-request-card]:drop"
             (:hx-sync button-attrs))))))

(deftest post-button-allows-distinct-optimistic-target-test
  (let [markup
        (ui/post-button
         ctx
         {:to "/claim"
          :target "request-list"
          :label "Claim"
          :optimistic optimistic-config})
        button-attrs (second (button markup))]
    (testing "authoritative response target and speculative projection target may differ"
      (is (= "#request-list"
             (:hx-target button-attrs)))
      (is (= "closest [data-request-card]"
             (get button-attrs
                  protocol/target-attr)))
      (is (= "closest [data-request-card]:drop"
             (:hx-sync button-attrs))))))

(deftest post-button-protects-optimistic-protocol-attrs-test
  (let [wrong-attrs
        (merge
         {:class "primary"
          :data-app-owned "still-here"}
         (zipmap protocol/reserved-attrs
                 (repeat "wrong")))
        markup
        (ui/post-button
         ctx
         {:to "/claim"
          :label "Claim"
          :button-attrs wrong-attrs
          :optimistic optimistic-config})
        button-attrs (second (button markup))]
    (testing "ordinary app attrs survive"
      (is (= "primary" (:class button-attrs)))
      (is (= "still-here"
             (:data-app-owned button-attrs))))

    (testing "framework command attrs override conflicting app values"
      (is (= "2"
             (get button-attrs
                  protocol/protocol-attr)))
      (is (= "request/claim"
             (get button-attrs
                  protocol/transition-attr)))
      (is (= "request-1-claim"
             (get button-attrs
                  protocol/template-attr)))
      (is (= "closest [data-request-card]"
             (get button-attrs
                  protocol/target-attr)))
      (is (= request-wire-scope
             (get button-attrs
                  protocol/scope-attr)))
      (is (= "i:7"
             (get button-attrs
                  protocol/base-revision-attr)))
      (is (= "Claiming…"
             (get button-attrs
                  protocol/pending-label-attr)))
      (is (= "provisional"
             (get button-attrs
                  protocol/projection-mode-attr))))

    (testing "settlement/canonical-only attrs cannot be smuggled onto a command source"
      (doseq [k [protocol/revision-attr
                 protocol/settlement-attr
                 protocol/execution-attr
                 protocol/outcome-attr
                 protocol/command-applied-attr
                 protocol/reason-attr
                 protocol/canonical-attr]]
        (is (not (contains? button-attrs k)))))))

(deftest post-button-optimistic-sync-override-test
  (testing "explicit UI sync overrides the descriptor recommendation"
    (is (= "closest form:abort"
           (get-in
            (ui/post-button
             ctx
             {:to "/claim"
              :label "Claim"
              :sync "closest form:abort"
              :optimistic optimistic-config})
            [3 1 :hx-sync]))))

  (testing "explicit nil or false disables hx-sync"
    (is (nil?
         (get-in
          (ui/post-button
           ctx
           {:to "/claim"
            :label "Claim"
            :sync nil
            :optimistic optimistic-config})
          [3 1 :hx-sync])))

    (is (nil?
         (get-in
          (ui/post-button
           ctx
           {:to "/claim"
            :label "Claim"
            :sync false
            :optimistic optimistic-config})
          [3 1 :hx-sync])))))

(deftest post-button-optimistic-fragment-call-shape-test
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
          :optimistic optimistic-config})
        button-attrs (second (button markup))]
    (testing "three-arity fragment conveniences still control authoritative response handling"
      (is (= "#request-list"
             (:hx-target button-attrs)))
      (is (= "outerHTML"
             (:hx-swap button-attrs))))

    (testing "optimistic target-local synchronization still wins by default"
      (is (= "closest [data-request-card]:drop"
             (:hx-sync button-attrs))))))

(deftest post-button-accepts-prepared-optimistic-descriptor-test
  (let [prepared
        (optimistic/->optimistic
         optimistic-config)
        markup
        (ui/post-button
         ctx
         {:to "/claim"
          :label "Claim"
          :optimistic prepared})]
    (is (= "request-1-claim"
           (get-in (button markup)
                   [1 protocol/template-attr])))
    (is (= request-wire-scope
           (get-in (button markup)
                   [1 protocol/scope-attr])))
    (is (= pending-card
           (nth (template markup) 2)))))

(deftest post-button-rejects-invalid-optimistic-value-test
  (testing ":optimistic accepts only a raw options map or prepared descriptor"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":optimistic must be"
         (ui/post-button
          ctx
          {:to "/claim"
           :label "Claim"
           :optimistic true})))))

;; -----------------------------------------------------------------------------
;; Wrapper and request-attribute precedence
;; -----------------------------------------------------------------------------

(deftest wrapper-marker-cannot-be-overridden-test
  (is (= true
         (get-in
          (ui/post-button
           ctx
           {:to "/increment"
            :label "+"
            :form-attrs
            {:data-gesso-live-post false}})
          [1 :data-gesso-live-post]))))

(deftest button-request-mechanics-cannot-be-accidentally-displaced-by-optimistic-metadata-test
  (let [markup
        (ui/post-button
         ctx
         {:to "/real-endpoint"
          :target "authoritative-target"
          :swap "outerHTML"
          :label "Claim"
          :button-attrs
          {:hx-post "/wrong-endpoint"
           :hx-target "#wrong-target"
           :hx-swap "none"
           :hx-include "#wrong-include"
           :hx-sync "wrong:abort"}
          :optimistic optimistic-config})
        button-attrs (second (button markup))]
    ;; Ordinary button attrs intentionally remain caller-overridable in UI today.
    ;; This test documents that protocol protection is separate from HTMX request
    ;; mechanics: optimistic metadata must not itself rewrite those app choices.
    (is (= "/wrong-endpoint"
           (:hx-post button-attrs)))
    (is (= "#wrong-target"
           (:hx-target button-attrs)))
    (is (= "none"
           (:hx-swap button-attrs)))
    (is (= "#wrong-include"
           (:hx-include button-attrs)))
    (is (= "wrong:abort"
           (:hx-sync button-attrs)))

    (testing "optimistic semantic identity remains framework-authoritative"
      (is (= "request/claim"
             (get button-attrs
                  protocol/transition-attr)))
      (is (= request-wire-scope
             (get button-attrs
                  protocol/scope-attr)))
      (is (= "closest [data-request-card]"
             (get button-attrs
                  protocol/target-attr))))))
