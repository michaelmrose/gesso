(ns gesso.live.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.core :as live]))

(defn fragment-opts
  ([] (fragment-opts {}))
  ([overrides]
   (merge
    {:id "request-panel"
     :src "/app/requests/request-1/fragment"
     :stream-url "/gesso/live/stream?subscription=request-1"}
    overrides)))

(defn token-ctx
  ([] (token-ctx "token-1"))
  ([token]
   {:anti-forgery-token token}))

(defn biff-token-ctx
  ([] (biff-token-ctx "biff-token-1"))
  ([token]
   {:biff/anti-forgery-token token}))

(defn expected-fragment-root-attrs
  ([] (expected-fragment-root-attrs {}))
  ([overrides]
   (merge
    {:hx-ext "sse"
     :sse-connect "/gesso/live/stream?subscription=request-1"}
    overrides)))

(defn expected-fragment-target-attrs
  ([] (expected-fragment-target-attrs {}))
  ([overrides]
   (merge
    {:id "request-panel"
     :hx-get "/app/requests/request-1/fragment"
     :hx-trigger "load, sse:live-update"
     :hx-swap "outerHTML"}
    overrides)))

(defn expected-fragment
  ([] (expected-fragment {} {}))
  ([root-overrides target-overrides]
   [:div (expected-fragment-root-attrs root-overrides)
    [:div (expected-fragment-target-attrs target-overrides)]]))

(defn expected-anti-forgery-input
  ([] (expected-anti-forgery-input "token-1"))
  ([token]
   [:input {:type "hidden"
            :name "__anti-forgery-token"
            :value token}]))

(defn expected-post-form
  ([] (expected-post-form {} []))
  ([attr-overrides children]
   (into
    [:form (merge
            {:method "post"
             :action "/app/demo/shared-counter/increment"
             :hx-post "/app/demo/shared-counter/increment"
             :hx-target "#shared-counter-fragment"
             :hx-swap "innerHTML"}
            attr-overrides)]
    children)))

(defn expected-post-button
  ([] (expected-post-button {} []))
  ([form-attr-overrides button-contents]
   (expected-post-form
    form-attr-overrides
    [(expected-anti-forgery-input "token-1")
     (into
      [:button {:type "submit"}]
      (or (seq button-contents) ["+"]))])))

(deftest fragment-root-attrs-test
  (let [opts (fragment-opts)
        actual (live/fragment-root-attrs opts)
        expected (expected-fragment-root-attrs)]
    (is (= expected actual))))

(deftest fragment-target-attrs-test
  (let [opts (fragment-opts)
        actual (live/fragment-target-attrs opts)
        expected (expected-fragment-target-attrs)]
    (is (= expected actual))))

(deftest fragment-target-attrs-customizations-test
  (let [opts (fragment-opts
              {:event "request-changed"
               :swap "innerHTML"
               :trigger "revealed"
               :inner-attrs {:class "w-full"}})
        actual (live/fragment-target-attrs opts)
        expected (expected-fragment-target-attrs
                  {:hx-trigger "revealed, sse:request-changed"
                   :hx-swap "innerHTML"
                   :class "w-full"})]
    (is (= expected actual))))

(deftest fragment-test
  (let [opts (fragment-opts)
        actual (live/fragment opts)
        expected (expected-fragment)]
    (is (= expected actual))))

(deftest anti-forgery-token-test
  (testing "prefers plain anti-forgery token"
    (let [ctx {:anti-forgery-token "plain-token"
               :biff/anti-forgery-token "biff-token"}]
      (is (= "plain-token"
             (live/anti-forgery-token ctx)))))

  (testing "falls back to biff token"
    (let [ctx (biff-token-ctx "biff-token")]
      (is (= "biff-token"
             (live/anti-forgery-token ctx)))))

  (testing "returns nil when absent"
    (is (nil? (live/anti-forgery-token {})))))

(deftest anti-forgery-input-test
  (testing "renders hidden input when token present"
    (let [ctx (token-ctx "token-1")
          actual (live/anti-forgery-input ctx)
          expected (expected-anti-forgery-input "token-1")]
      (is (= expected actual))))

  (testing "returns nil when token absent"
    (is (nil? (live/anti-forgery-input {})))))

(deftest normalize-target-test
  (is (nil? (live/normalize-target nil)))
  (is (= "#shared-counter-fragment"
         (live/normalize-target "shared-counter-fragment")))
  (is (= "#already-id"
         (live/normalize-target "#already-id")))
  (is (= ".some-class"
         (live/normalize-target ".some-class")))
  (is (= "this"
         (live/normalize-target "this")))
  (is (= "closest [hx-ext='sse']"
         (live/normalize-target "closest [hx-ext='sse']"))))

(deftest post-form-attrs-test
  (testing "builds standard attrs with default swap"
    (let [opts {:to "/app/demo/shared-counter/increment"
                :target "shared-counter-fragment"}
          actual (live/post-form-attrs opts)
          expected {:method "post"
                    :action "/app/demo/shared-counter/increment"
                    :hx-post "/app/demo/shared-counter/increment"
                    :hx-target "#shared-counter-fragment"
                    :hx-swap "innerHTML"}]
      (is (= expected actual))))

  (testing "allows custom swap and extra attrs"
    (let [opts {:to "/x"
                :target "frag"
                :swap "outerHTML"
                :attrs {:class "my-form"}}
          actual (live/post-form-attrs opts)
          expected {:method "post"
                    :action "/x"
                    :hx-post "/x"
                    :hx-target "#frag"
                    :hx-swap "outerHTML"
                    :class "my-form"}]
      (is (= expected actual))))

  (testing "allows omitted target"
    (let [opts {:to "/x"}
          actual (live/post-form-attrs opts)
          expected {:method "post"
                    :action "/x"
                    :hx-post "/x"
                    :hx-swap "innerHTML"}]
      (is (= expected actual)))))

(deftest post-form-test
  (testing "includes anti-forgery input when token exists"
    (let [ctx (token-ctx "token-1")
          opts {:to "/app/demo/shared-counter/increment"
                :target "shared-counter-fragment"}
          actual (live/post-form ctx opts [:span "Increment"])
          expected (expected-post-form
                    {}
                    [(expected-anti-forgery-input "token-1")
                     [:span "Increment"]])]
      (is (= expected actual))))

  (testing "omits anti-forgery input when token missing"
    (let [ctx {}
          opts {:to "/x"}
          actual (live/post-form ctx opts [:span "No token"])
          expected [:form {:method "post"
                           :action "/x"
                           :hx-post "/x"
                           :hx-swap "innerHTML"}
                    [:span "No token"]]]
      (is (= expected actual)))))

(deftest post-button-test
  (testing "renders a standard button form with label"
    (let [ctx (token-ctx "token-1")
          opts {:to "/app/demo/shared-counter/increment"
                :target "shared-counter-fragment"
                :label "+"}
          actual (live/post-button ctx opts)
          expected (expected-post-button)]
      (is (= expected actual))))

  (testing "renders with explicit children and merged attrs"
    (let [ctx (token-ctx "token-2")
          opts {:to "/x"
                :target "frag"
                :swap "outerHTML"
                :form-attrs {:class "my-form"}
                :button-attrs {:class "btn"}
                :children [[:span "Go"]]}
          actual (live/post-button ctx opts)
          expected [:form {:method "post"
                           :action "/x"
                           :hx-post "/x"
                           :hx-target "#frag"
                           :hx-swap "outerHTML"
                           :class "my-form"}
                    (expected-anti-forgery-input "token-2")
                    [:button {:type "submit"
                              :class "btn"}
                     [:span "Go"]]]]
      (is (= expected actual)))))

(deftest current-consistency-token-test
  (is (nil? (live/current-consistency-token {}))))

(deftest live-request?-test
  (is (false? (live/live-request? {}))))

(deftest build-event-test
  (let [actual (live/build-event
                {:changed {:entity/type :request
                           :entity/id "req-123"}})
        expected {:event "live-update"
                  :changed {:entity/type :request
                            :entity/id "req-123"}
                  :consistency-token nil
                  :data nil}]
    (is (= expected actual))))

(deftest build-event-custom-event-test
  (let [actual (live/build-event
                {:event "request-changed"
                 :changed {:entity/type :request
                           :entity/id "req-123"}
                 :consistency-token [:tx 7]
                 :data {:reason :claim}})
        expected {:event "request-changed"
                  :changed {:entity/type :request
                            :entity/id "req-123"}
                  :consistency-token [:tx 7]
                  :data {:reason :claim}}]
    (is (= expected actual))))
