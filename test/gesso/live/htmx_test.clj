(ns gesso.live.htmx-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.htmx :as htmx]))

(deftest token-header-name-test
  (testing "returns the canonical live consistency token header"
    (is (= "x-gesso-live-consistency-token"
           (htmx/token-header-name)))))

(deftest fragment-root-attrs-test
  (testing "builds SSE wrapper attrs from stream-url"
    (is (= {:hx-ext "sse"
            :sse-connect "/gesso/live/stream?subscription=abc"}
           (htmx/fragment-root-attrs
            {:stream-url "/gesso/live/stream?subscription=abc"})))))

(deftest fragment-target-attrs-test
  (testing "builds target attrs"
    (is (= {:id "request-panel"
            :hx-get "/app/requests/req-123/fragment"
            :hx-trigger "load, sse:live-update"
            :hx-swap "outerHTML"}
           (htmx/fragment-target-attrs
            {:id "request-panel"
             :src "/app/requests/req-123/fragment"
             :event "live-update"
             :swap "outerHTML"
             :trigger "load"}))))

  (testing "supports alternate trigger, event, and swap"
    (is (= {:id "request-panel"
            :hx-get "/app/requests/req-123/fragment"
            :hx-trigger "revealed, sse:custom-event"
            :hx-swap "innerHTML"}
           (htmx/fragment-target-attrs
            {:id "request-panel"
             :src "/app/requests/req-123/fragment"
             :event "custom-event"
             :swap "innerHTML"
             :trigger "revealed"})))))
