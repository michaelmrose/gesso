(ns gesso.live.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.core :as live]
   [gesso.live.bus :as bus]))

(def test-matcher
  {:subscription->entries (fn [_subscription] [])
   :changed->entries (fn [_ctx _changed] [])})

(defn test-bus []
  (bus/memory-bus test-matcher))

(defn test-ctx
  ([] (test-ctx {}))
  ([m]
   (merge {:gesso.live/bus (test-bus)}
          m)))

(deftest fragment-root-attrs-test
  (testing "builds root attrs and merges caller attrs"
    (is (= {:hx-ext "sse"
            :sse-connect "/stream"
            :class "x"}
           (live/fragment-root-attrs
            {:id "a"
             :src "/frag"
             :stream-url "/stream"
             :attrs {:class "x"}}))))

  (testing "requires stream-url"
    (is (thrown? clojure.lang.ExceptionInfo
                 (live/fragment-root-attrs
                  {:id "a"
                   :src "/frag"})))))

(deftest fragment-target-attrs-test
  (testing "builds target attrs with defaults"
    (is (= {:id "request-panel"
            :hx-get "/frag"
            :hx-trigger "load, sse:live-update"
            :hx-swap "outerHTML"}
           (live/fragment-target-attrs
            {:id "request-panel"
             :src "/frag"
             :stream-url "/stream"}))))

  (testing "merges caller inner attrs"
    (is (= {:id "request-panel"
            :hx-get "/frag"
            :hx-trigger "load, sse:live-update"
            :hx-swap "outerHTML"
            :class "grow"}
           (live/fragment-target-attrs
            {:id "request-panel"
             :src "/frag"
             :stream-url "/stream"
             :inner-attrs {:class "grow"}}))))

  (testing "respects custom event swap and trigger"
    (is (= {:id "request-panel"
            :hx-get "/frag"
            :hx-trigger "revealed, sse:custom-event"
            :hx-swap "innerHTML"}
           (live/fragment-target-attrs
            {:id "request-panel"
             :src "/frag"
             :stream-url "/stream"
             :event "custom-event"
             :swap "innerHTML"
             :trigger "revealed"}))))

  (testing "requires id"
    (is (thrown? clojure.lang.ExceptionInfo
                 (live/fragment-target-attrs
                  {:src "/frag"
                   :stream-url "/stream"}))))

  (testing "requires src"
    (is (thrown? clojure.lang.ExceptionInfo
                 (live/fragment-target-attrs
                  {:id "request-panel"
                   :stream-url "/stream"}))))

  (testing "requires stream-url"
    (is (thrown? clojure.lang.ExceptionInfo
                 (live/fragment-target-attrs
                  {:id "request-panel"
                   :src "/frag"})))))

(deftest fragment-test
  (testing "renders minimal live fragment hiccup"
    (is (= [:div
            {:hx-ext "sse"
             :sse-connect "/stream"}
            [:div
             {:id "request-panel"
              :hx-get "/frag"
              :hx-trigger "load, sse:live-update"
              :hx-swap "outerHTML"}]]
           (live/fragment
            {:id "request-panel"
             :src "/frag"
             :stream-url "/stream"}))))

  (testing "passes through custom attrs"
    (is (= [:div
            {:hx-ext "sse"
             :sse-connect "/stream"
             :class "outer"}
            [:div
             {:id "request-panel"
              :hx-get "/frag"
              :hx-trigger "load, sse:live-update"
              :hx-swap "outerHTML"
              :class "inner"}]]
           (live/fragment
            {:id "request-panel"
             :src "/frag"
             :stream-url "/stream"
             :attrs {:class "outer"}
             :inner-attrs {:class "inner"}})))))

(deftest current-consistency-token-test
  (testing "reads token from header"
    (is (= [:tx 9]
           (live/current-consistency-token
            {:headers {"x-gesso-live-consistency-token" [:tx 9]}}))))

  (testing "falls back to params"
    (is (= [:tx 8]
           (live/current-consistency-token
            {:params {:consistency-token [:tx 8]}}))))

  (testing "returns nil when absent"
    (is (nil? (live/current-consistency-token {})))))

(deftest live-request-test
  (testing "true when token exists"
    (is (true? (live/live-request?
                {:headers {"x-gesso-live-consistency-token" [:tx 9]}}))))

  (testing "false when token missing"
    (is (false? (live/live-request? {})))))

(deftest build-event-test
  (testing "uses default event name"
    (is (= {:event "live-update"
            :changed {:entity/type :request
                      :entity/id "req-1"}
            :consistency-token nil
            :data nil}
           (live/build-event
            {:changed {:entity/type :request
                       :entity/id "req-1"}}))))

  (testing "preserves provided event token and data"
    (is (= {:event "custom"
            :changed {:entity/type :request
                      :entity/id "req-2"}
            :consistency-token [:tx 2]
            :data {:reason :test}}
           (live/build-event
            {:changed {:entity/type :request
                       :entity/id "req-2"}
             :event "custom"
             :consistency-token [:tx 2]
             :data {:reason :test}}))))

  (testing "requires changed"
    (is (thrown? clojure.lang.ExceptionInfo
                 (live/build-event {})))))

(deftest publish-change-test
  (testing "publishes normalized event and returns it"
    (let [ctx {:gesso.live/bus (test-bus)}
          event (live/publish-change!
                 ctx
                 {:changed {:entity/type :request
                            :entity/id "req-1"
                            :change/kind :updated}})]
      (is (= {:event "live-update"
              :changed {:entity/type :request
                        :entity/id "req-1"
                        :change/kind :updated}
              :consistency-token nil
              :data nil}
             event)))))

(deftest publish-change-requires-bus-test
  (testing "throws when bus is missing from ctx"
    (is (thrown? clojure.lang.ExceptionInfo
                 (live/publish-change!
                  {}
                  {:changed {:entity/type :request
                             :entity/id "req-1"}})))))

(deftest fragment-required-keys-test
  (testing "requires id"
    (is (thrown? clojure.lang.ExceptionInfo
                 (live/fragment
                  {:src "/frag"
                   :stream-url "/stream"}))))

  (testing "requires src"
    (is (thrown? clojure.lang.ExceptionInfo
                 (live/fragment
                  {:id "request-panel"
                   :stream-url "/stream"}))))

  (testing "requires stream-url"
    (is (thrown? clojure.lang.ExceptionInfo
                 (live/fragment
                  {:id "request-panel"
                   :src "/frag"})))))
