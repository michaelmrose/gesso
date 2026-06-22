(ns gesso.live.ui-optimistic-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.ui :as ui]))

(def ctx
  {:anti-forgery-token "anti-forgery-token"})

(def pending-card
  [:details
   {:data-request-card "request-1"
    :open true}
   [:summary "Claimed"]
   [:div "Confirming…"]])

(def optimistic-config
  {:template-name "request-1-claim"
   :target "closest [data-request-card]"
   :action :claim
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
  (child-by-tag hiccup :input))

(deftest post-button-preserves-existing-defaults-test
  (let [markup (ui/post-button
                ctx
                {:to "/increment"
                 :target "counter-fragment"
                 :label "+"})
        button' (button markup)]
    (testing "post-button still uses a lightweight anti-forgery wrapper"
      (is (= :form (first markup)))
      (is (= true
             (:data-gesso-live-post (form-attrs markup))))
      (is (= [:input
              {:type "hidden"
               :name "__anti-forgery-token"
               :value "anti-forgery-token"}]
             (anti-forgery-input markup))))

    (testing "the actual button owns the HTMX POST"
      (is (= {:type "button"
              :hx-post "/increment"
              :hx-swap "innerHTML"
              :hx-include "closest [data-gesso-live-post]"
              :hx-sync "closest [data-gesso-live-fragment]:drop"
              :hx-target "#counter-fragment"}
             (second button')))
      (is (= "+" (nth button' 2))))))

(deftest post-button-additive-include-test
  (testing "one additional selector is appended to the wrapper form selector"
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
              :include ["#board-state"
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

(deftest optimistic-post-button-test
  (let [markup
        (ui/optimistic-post-button
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
        template' (template markup)]
    (testing "the request owner is the actual type=button control"
      (is (= :button (first button')))
      (is (= "button" (get-in button' [1 :type])))
      (is (= "/app/requests/request-1/claim"
             (get-in button' [1 :hx-post])))
      (is (= "none" (get-in button' [1 :hx-swap])))
      (is (= "closest [data-gesso-live-post], #request-board-state"
             (get-in button' [1 :hx-include])))
      (is (= "closest [data-request-card]:drop"
             (get-in button' [1 :hx-sync])))
      (is (= "primary-button"
             (get-in button' [1 :class])))
      (is (= "claim"
             (get-in button' [1 :data-humanhelp-action]))))

    (testing "optimistic protocol attrs are attached to that button"
      (is (= "request-1-claim"
             (get-in button' [1 :data-gesso-optimistic-template])))
      (is (= "closest [data-request-card]"
             (get-in button' [1 :data-gesso-optimistic-target])))
      (is (= "claim"
             (get-in button' [1 :data-gesso-optimistic-action])))
      (is (= "Claiming…"
             (get-in button' [1 :data-gesso-optimistic-label]))))

    (testing "the matching template is a sibling inside the same form"
      (is (= [:template
              {:data-gesso-optimistic-template "request-1-claim"}
              pending-card]
             template')))))

(deftest optimistic-post-button-protects-protocol-attrs-test
  (let [markup
        (ui/optimistic-post-button
         ctx
         {:to "/claim"
          :label "Claim"
          :button-attrs
          {:data-gesso-optimistic-template "wrong-template"
           :data-gesso-optimistic-target "#wrong-target"
           :data-gesso-optimistic-action "wrong-action"
           :data-gesso-optimistic-label "Wrong label"}
          :optimistic optimistic-config})
        attrs (second (button markup))]
    (is (= "request-1-claim"
           (:data-gesso-optimistic-template attrs)))
    (is (= "closest [data-request-card]"
           (:data-gesso-optimistic-target attrs)))
    (is (= "claim"
           (:data-gesso-optimistic-action attrs)))
    (is (= "Claiming…"
           (:data-gesso-optimistic-label attrs)))))

(deftest optimistic-post-button-sync-override-test
  (testing "an explicit UI sync overrides the descriptor recommendation"
    (is (= "closest form:abort"
           (get-in
            (ui/optimistic-post-button
             ctx
             {:to "/claim"
              :label "Claim"
              :sync "closest form:abort"
              :optimistic optimistic-config})
            [3 1 :hx-sync]))))

  (testing "explicit nil or false disables hx-sync"
    (is (nil?
         (get-in
          (ui/optimistic-post-button
           ctx
           {:to "/claim"
            :label "Claim"
            :sync nil
            :optimistic optimistic-config})
          [3 1 :hx-sync])))

    (is (nil?
         (get-in
          (ui/optimistic-post-button
           ctx
           {:to "/claim"
            :label "Claim"
            :sync false
            :optimistic optimistic-config})
          [3 1 :hx-sync])))))

(deftest optimistic-post-button-fragment-call-shape-test
  (let [fragment
        (ui/->fragment
         {:id "request-list"
          :src "/app/fragments/requests"
          :stream-url "/app/streams/requests"
          :swap "outerHTML"})
        markup
        (ui/optimistic-post-button
         ctx
         fragment
         {:to "/claim"
          :label "Claim"
          :optimistic optimistic-config})
        attrs (second (button markup))]
    (testing "the existing three-arity fragment conveniences still apply"
      (is (= "#request-list" (:hx-target attrs)))
      (is (= "outerHTML" (:hx-swap attrs))))

    (testing "optimistic target-local synchronization still wins by default"
      (is (= "closest [data-request-card]:drop"
             (:hx-sync attrs))))))

(deftest optimistic-post-button-requires-config-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"requires :optimistic"
       (ui/optimistic-post-button
        ctx
        {:to "/claim"
         :label "Claim"}))))

(deftest wrapper-marker-cannot-be-overridden-test
  (is (= true
         (get-in
          (ui/post-button
           ctx
           {:to "/increment"
            :label "+"
            :form-attrs {:data-gesso-live-post false}})
          [1 :data-gesso-live-post]))))
