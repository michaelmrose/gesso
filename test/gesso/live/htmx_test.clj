(ns gesso.live.htmx-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.htmx :as htmx]))

(deftest token-header-name-test
  (is (= "x-gesso-live-consistency-token"
         (htmx/token-header-name))))

(deftest normalize-target-test
  (is (nil? (htmx/normalize-target nil)))
  (is (= "#shared-counter-fragment"
         (htmx/normalize-target "shared-counter-fragment")))
  (is (= "#already-id"
         (htmx/normalize-target "#already-id")))
  (is (= ".some-class"
         (htmx/normalize-target ".some-class")))
  (is (= "this"
         (htmx/normalize-target "this")))
  (is (= "closest [hx-ext='sse']"
         (htmx/normalize-target "closest [hx-ext='sse']")))
  (is (= "find .child"
         (htmx/normalize-target "find .child"))))

(deftest fragment-root-attrs-test
  (is (= {:hx-ext "sse"
          :sse-connect "/gesso/live/stream?subscription=request-1"}
         (htmx/fragment-root-attrs
          {:stream-url "/gesso/live/stream?subscription=request-1"}))))

(deftest fragment-trigger-test
  (testing "uses defaults"
    (is (= "load, sse:live-update"
           (htmx/fragment-trigger {}))))

  (testing "uses explicit event and trigger"
    (is (= "revealed, sse:request-changed"
           (htmx/fragment-trigger
            {:event "request-changed"
             :trigger "revealed"})))))

(deftest fragment-target-attrs-test
  (testing "uses defaults"
    (is (= {:id "request-panel"
            :hx-get "/app/requests/request-1/fragment"
            :hx-trigger "load, sse:live-update"
            :hx-swap "outerHTML"}
           (htmx/fragment-target-attrs
            {:id "request-panel"
             :src "/app/requests/request-1/fragment"}))))

  (testing "uses explicit event, swap, and trigger"
    (is (= {:id "request-panel"
            :hx-get "/app/requests/request-1/fragment"
            :hx-trigger "revealed, sse:request-changed"
            :hx-swap "innerHTML"}
           (htmx/fragment-target-attrs
            {:id "request-panel"
             :src "/app/requests/request-1/fragment"
             :event "request-changed"
             :swap "innerHTML"
             :trigger "revealed"})))))

(deftest post-form-attrs-test
  (testing "builds standard attrs with default swap"
    (is (= {:method "post"
            :action "/app/demo/shared-counter/increment"
            :hx-post "/app/demo/shared-counter/increment"
            :hx-target "#shared-counter-fragment"
            :hx-swap "innerHTML"}
           (htmx/post-form-attrs
            {:to "/app/demo/shared-counter/increment"
             :target "shared-counter-fragment"}))))

  (testing "allows custom swap and extra attrs"
    (is (= {:method "post"
            :action "/x"
            :hx-post "/x"
            :hx-target "#frag"
            :hx-swap "outerHTML"
            :class "my-form"}
           (htmx/post-form-attrs
            {:to "/x"
             :target "frag"
             :swap "outerHTML"
             :attrs {:class "my-form"}}))))

  (testing "allows omitted target"
    (is (= {:method "post"
            :action "/x"
            :hx-post "/x"
            :hx-swap "innerHTML"}
           (htmx/post-form-attrs {:to "/x"}))))

  (testing "preserves selector targets"
    (is (= {:method "post"
            :action "/x"
            :hx-post "/x"
            :hx-target "closest [data-live-root]"
            :hx-swap "innerHTML"}
           (htmx/post-form-attrs
            {:to "/x"
             :target "closest [data-live-root]"})))))
