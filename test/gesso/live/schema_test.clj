(ns gesso.live.schema-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.schema :as schema]))

;; -----------------------------------------------------------------------------
;; Schema lookup
;; -----------------------------------------------------------------------------

(deftest schema-lookup-test
  (testing "known schema keys resolve"
    (is (some? (schema/schema :gesso.live/primary-change)))
    (is (some? (schema/schema :gesso.live/invalidation)))
    (is (some? (schema/schema :gesso.live/subscription)))
    (is (some? (schema/schema :gesso.live/fragment-config))))

  (testing "unknown schema keys throw useful errors"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unknown gesso.live schema key"
         (schema/schema :gesso.live/nope)))))

;; -----------------------------------------------------------------------------
;; Primitive-ish schemas
;; -----------------------------------------------------------------------------

(deftest primitive-schema-test
  (testing "topics and ids validate"
    (is (schema/validate :gesso.live/topic :request))
    (is (not (schema/validate :gesso.live/topic "request")))

    (is (schema/validate :gesso.live/id "req-1"))
    (is (schema/validate :gesso.live/id 123))
    (is (schema/validate :gesso.live/id :main))
    (is (not (schema/validate :gesso.live/id nil))))

  (testing "event names are normalized non-blank strings"
    (is (schema/validate :gesso.live/event-name "live-update"))
    (is (not (schema/validate :gesso.live/event-name "")))
    (is (not (schema/validate :gesso.live/event-name "   ")))
    (is (not (schema/validate :gesso.live/event-name :live-update))))

  (testing "event refs allow app-facing keyword, symbol, or string values"
    (is (schema/validate :gesso.live/event-ref "live-update"))
    (is (schema/validate :gesso.live/event-ref :live-update))
    (is (schema/validate :gesso.live/event-ref 'live-update))
    (is (not (schema/validate :gesso.live/event-ref ""))))

  (testing "milliseconds are non-negative and positive milliseconds are positive"
    (is (schema/validate :gesso.live/milliseconds 0))
    (is (schema/validate :gesso.live/milliseconds 15000))
    (is (not (schema/validate :gesso.live/milliseconds -1)))

    (is (schema/validate :gesso.live/positive-milliseconds 1))
    (is (not (schema/validate :gesso.live/positive-milliseconds 0)))
    (is (not (schema/validate :gesso.live/positive-milliseconds -1))))

  (testing "scopes may be a set or a sequential collection"
    (is (schema/validate :gesso.live/scopes #{[:user "u1"]}))
    (is (schema/validate :gesso.live/scopes [[:user "u1"]]))
    (is (not (schema/validate :gesso.live/scopes {:user "u1"})))))

;; -----------------------------------------------------------------------------
;; Core live data
;; -----------------------------------------------------------------------------

(deftest primary-change-schema-test
  (testing "minimal primary changes validate"
    (is (schema/validate
         :gesso.live/primary-change
         {:topic :request
          :id "req-1"
          :change/kind :updated})))

  (testing "primary changes may omit id and change kind"
    (is (schema/validate
         :gesso.live/primary-change
         {:topic :global-announcement})))

  (testing "primary changes may carry extra app context"
    (is (schema/validate
         :gesso.live/primary-change
         {:topic :request
          :id "req-1"
          :change/kind :updated
          :request {:xt/id "req-1"
                    :request/status :done}})))

  (testing "primary changes require a topic"
    (is (not
         (schema/validate
          :gesso.live/primary-change
          {:id "req-1"
           :change/kind :updated}))))

  (testing "primary change topic must be a keyword"
    (is (not
         (schema/validate
          :gesso.live/primary-change
          {:topic "request"
           :id "req-1"
           :change/kind :updated})))))

(deftest invalidation-schema-test
  (testing "expanded invalidations validate"
    (is (schema/validate
         :gesso.live/invalidation
         {:topic :store-queue
          :id "store-1"
          :change/kind :updated})))

  (testing "change kind is optional"
    (is (schema/validate
         :gesso.live/invalidation
         {:topic :store-queue
          :id "store-1"})))

  (testing "expanded invalidations require id"
    (is (not
         (schema/validate
          :gesso.live/invalidation
          {:topic :store-queue
           :change/kind :updated}))))

  (testing "expanded invalidations require topic"
    (is (not
         (schema/validate
          :gesso.live/invalidation
          {:id "store-1"
           :change/kind :updated}))))

  (testing "source/emit! should use invalidations, not primary changes"
    (is (schema/validate
         :gesso.live/primary-change
         {:topic :request}))

    (is (not
         (schema/validate
          :gesso.live/invalidation
          {:topic :request})))))

(deftest subscription-schema-test
  (testing "subscriptions validate"
    (is (schema/validate
         :gesso.live/subscription
         {:topic :demo-counter
          :id "global-shared-counter"})))

  (testing "subscriptions require topic and id"
    (is (not
         (schema/validate
          :gesso.live/subscription
          {:topic :demo-counter})))

    (is (not
         (schema/validate
          :gesso.live/subscription
          {:id "global-shared-counter"})))))

(deftest live-event-schema-test
  (testing "live events validate"
    (is (schema/validate
         :gesso.live/live-event
         {:event "live-update"
          :invalidation {:topic :demo-counter
                         :id "global-shared-counter"
                         :change/kind :updated}})))

  (testing "live events may carry data and consistency tokens"
    (is (schema/validate
         :gesso.live/live-event
         {:event "live-update"
          :invalidation {:topic :demo-counter
                         :id "global-shared-counter"
                         :change/kind :updated}
          :data {:reason :test}
          :consistency-token "token-1"})))

  (testing "live event event names must be normalized strings"
    (is (not
         (schema/validate
          :gesso.live/live-event
          {:event :live-update
           :invalidation {:topic :demo-counter
                          :id "global-shared-counter"
                          :change/kind :updated}}))))

  (testing "live events require valid invalidations"
    (is (not
         (schema/validate
          :gesso.live/live-event
          {:event "live-update"
           :invalidation {:topic :demo-counter}})))))

;; -----------------------------------------------------------------------------
;; Invalidation rules
;; -----------------------------------------------------------------------------

(deftest invalidation-rule-schema-test
  (testing "topic-based rules validate"
    (is (schema/validate
         :gesso.live/invalidation-rule
         {:when-topic :request
          :expand (fn [_ctx change]
                    [change])})))

  (testing "predicate-based rules validate"
    (is (schema/validate
         :gesso.live/invalidation-rule
         {:when (fn [_ctx change]
                  (= :request (:topic change)))
          :expand (fn [_ctx change]
                    [change])})))

  (testing "rules may include both :when-topic and :when"
    (is (schema/validate
         :gesso.live/invalidation-rule
         {:when-topic :request
          :when (fn [_ctx change]
                  (= :updated (:change/kind change)))
          :expand (fn [_ctx change]
                    [change])})))

  (testing "rules require either :when-topic or :when"
    (is (not
         (schema/validate
          :gesso.live/invalidation-rule
          {:expand (fn [_ctx change]
                     [change])}))))

  (testing "rules require an expansion function"
    (is (not
         (schema/validate
          :gesso.live/invalidation-rule
          {:when-topic :request})))))

(deftest invalidation-options-schema-test
  (testing "invalidation options validate"
    (is (schema/validate
         :gesso.live/invalidation-options
         {:on-unmatched :keep
          :dedupe? true})))

  (testing "on-unmatched is constrained"
    (is (not
         (schema/validate
          :gesso.live/invalidation-options
          {:on-unmatched :explode})))))

;; -----------------------------------------------------------------------------
;; Source, dispatch, core, and stream options
;; -----------------------------------------------------------------------------

(deftest source-and-dispatch-options-test
  (testing "source options validate"
    (is (schema/validate
         :gesso.live/source-options
         {:id :app/live
          :coalesce-window-ms 50})))

  (testing "source coalesce window cannot be negative"
    (is (not
         (schema/validate
          :gesso.live/source-options
          {:coalesce-window-ms -1}))))

  (testing "dispatch options validate"
    (is (schema/validate
         :gesso.live/dispatch-options
         {:dispatch :sync}))

    (is (schema/validate
         :gesso.live/dispatch-options
         {:dispatch :async
          :dispatcher :fake-dispatcher
          :on-overflow :throw
          :consistency-token "token-1"
          :ctx-data {:user/id "u1"}})))

  (testing "dispatch mode is constrained"
    (is (not
         (schema/validate
          :gesso.live/dispatch-options
          {:dispatch :eventually}))))

  (testing "dispatcher construction options validate"
    (is (schema/validate
         :gesso.live/dispatcher-options
         {:name "gesso-live-expansion"
          :threads 4
          :queue-size 1024
          :on-overflow :throw})))

  (testing "dispatcher options reject blank names and invalid sizes"
    (is (not
         (schema/validate
          :gesso.live/dispatcher-options
          {:name ""})))

    (is (not
         (schema/validate
          :gesso.live/dispatcher-options
          {:threads 0})))

    (is (not
         (schema/validate
          :gesso.live/dispatcher-options
          {:queue-size 0})))))

(deftest core-and-stream-options-test
  (testing "core emit options validate"
    (is (schema/validate
         :gesso.live/core-emit-options
         {:source :fake-source
          :rules [{:when-topic :request
                   :expand (fn [_ctx change]
                             [change])}]
          :ctx {:request/id "r1"}
          :dispatch :sync})))

  (testing "core emit options require source"
    (is (not
         (schema/validate
          :gesso.live/core-emit-options
          {:rules []}))))

  (testing "stream handler options validate"
    (is (schema/validate
         :gesso.live/stream-handler-options
         {:source :fake-source
          :parse-subscription (fn [_ctx raw]
                                raw)
          :authorize-subscription (fn [_ctx _sub]
                                    true)
          :interested? (fn [sub invalidation]
                         (= (select-keys sub [:topic :id])
                            (select-keys invalidation [:topic :id])))
          :event :live-update
          :keepalive-ms 15000})))

  (testing "stream handler options require handler fns"
    (is (not
         (schema/validate
          :gesso.live/stream-handler-options
          {:source :fake-source
           :parse-subscription identity
           :authorize-subscription (fn [_ctx _sub] true)})))))

;; -----------------------------------------------------------------------------
;; Flow options
;; -----------------------------------------------------------------------------

(deftest flow-options-test
  (testing "flow subscription options validate"
    (is (schema/validate
         :gesso.live/flow-for-subscription-options
         {:subscription {:topic :demo-counter
                         :id "global-shared-counter"}
          :interested? (fn [_sub _invalidation]
                         true)})))

  (testing "invalidation event options accept app-facing event refs"
    (is (schema/validate
         :gesso.live/invalidation-event-options
         {:event :live-update
          :data {:reason :test}})))

  (testing "coalesce-by options validate"
    (is (schema/validate
         :gesso.live/coalesce-by-options
         {:key-fn #(select-keys % [:topic :id])
          :window-ms 50})))

  (testing "isolation options validate"
    (is (schema/validate
         :gesso.live/isolation-options
         {:on-error (fn [_e] nil)
          :on-close (fn [] nil)}))))

;; -----------------------------------------------------------------------------
;; Fragment config and fragment performance helpers
;; -----------------------------------------------------------------------------

(deftest fragment-options-test
  (testing "fragment configs validate"
    (is (schema/validate
         :gesso.live/fragment-config
         {:subscription {:topic :demo-counter
                         :id "global-shared-counter"}
          :fragment/id "simple-shared-counter-fragment"
          :fragment/src "/app/demo/simple-shared-counter/fragment"
          :fragment/swap "innerHTML"
          :fragment/event :live-update
          :fragment/jitter-ms 250
          :fragment/attrs {:class "wrapper"}
          :fragment/inner-attrs {:class "target"}})))

  (testing "fragment configs require subscription, id, and src"
    (is (not
         (schema/validate
          :gesso.live/fragment-config
          {:fragment/id "x"
           :fragment/src "/fragment"})))

    (is (not
         (schema/validate
          :gesso.live/fragment-config
          {:subscription {:topic :demo-counter
                          :id "global-shared-counter"}
           :fragment/src "/fragment"})))

    (is (not
         (schema/validate
          :gesso.live/fragment-config
          {:subscription {:topic :demo-counter
                          :id "global-shared-counter"}
           :fragment/id "x"}))))

  (testing "fragment cache options require non-nil key"
    (is (schema/validate
         :gesso.live/fragment-cache-options
         {:key [:store-queue "store-1" :manager]
          :ttl-ms 250
          :maximum-size 1024}))

    (is (not
         (schema/validate
          :gesso.live/fragment-cache-options
          {:key nil
           :ttl-ms 250}))))

  (testing "fragment singleflight options require non-nil key"
    (is (schema/validate
         :gesso.live/fragment-singleflight-options
         {:key [:store-queue "store-1"]}))

    (is (not
         (schema/validate
          :gesso.live/fragment-singleflight-options
          {:key nil})))))

;; -----------------------------------------------------------------------------
;; SSE
;; -----------------------------------------------------------------------------

(deftest sse-schema-test
  (testing "SSE response options require a flow"
    (is (schema/validate
         :gesso.live/sse-response-options
         {:flow :fake-flow
          :keepalive-ms 15000
          :headers {"x-test" "yes"}}))

    (is (not
         (schema/validate
          :gesso.live/sse-response-options
          {:flow nil}))))

  (testing "SSE frame events allow frames, live events, and event/data maps"
    (is (schema/validate
         :gesso.live/sse-frame-event
         "event: live-update\ndata: {}\n\n"))

    (is (schema/validate
         :gesso.live/sse-frame-event
         {:event "live-update"
          :invalidation {:topic :demo-counter
                         :id "global-shared-counter"
                         :change/kind :updated}}))

    (is (schema/validate
         :gesso.live/sse-frame-event
         {:event :client-oob
          :data "<div hx-swap-oob=\"true\"></div>"}))))

;; -----------------------------------------------------------------------------
;; OOB
;; -----------------------------------------------------------------------------

(deftest oob-schema-test
  (testing "client descriptors validate with set or sequential scopes"
    (is (schema/validate
         :gesso.live/client-descriptor
         {:client/id "client-1"
          :client/user-id "user-1"
          :client/scopes #{[:user "user-1"]
                           [:store "store-1"]}
          :client/connected-at 123}))

    (is (schema/validate
         :gesso.live/client-descriptor
         {:client/user-id "user-1"
          :client/scopes [[:user "user-1"]
                          [:store "store-1"]]})))

  (testing "OOB targets validate"
    (is (schema/validate :gesso.live/oob-target :all))
    (is (schema/validate :gesso.live/oob-target [:client "client-1"]))
    (is (schema/validate :gesso.live/oob-target [:user "user-1"]))
    (is (schema/validate :gesso.live/oob-target [:scope [:store "store-1"]]))
    (is (not (schema/validate :gesso.live/oob-target [:team "team-1"]))))

  (testing "OOB send options require :fragments or :oob"
    (is (schema/validate
         :gesso.live/oob-send-options
         {:to :all
          :fragments [[:div "hello"]]}))

    (is (schema/validate
         :gesso.live/oob-send-options
         {:to [:user "user-1"]
          :oob [:div "hello"]}))

    (is (not
         (schema/validate
          :gesso.live/oob-send-options
          {:to :all})))))

;; -----------------------------------------------------------------------------
;; Validation helper behavior
;; -----------------------------------------------------------------------------

(deftest validation-helper-test
  (testing "validate! returns the original value when valid"
    (let [value {:topic :demo-counter
                 :id "global-shared-counter"
                 :change/kind :updated}]
      (is (= value
             (schema/validate! :gesso.live/invalidation value)))))

  (testing "validate! throws ex-info when invalid"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid gesso.live value"
         (schema/validate!
          :gesso.live/invalidation
          {:topic :demo-counter}))))

  (testing "humanize returns useful explanation data for invalid values"
    (is (some?
         (schema/humanize
          :gesso.live/invalidation
          {:topic :demo-counter}))))

  (testing "validator returns a reusable predicate"
    (let [valid-invalidation? (schema/validator :gesso.live/invalidation)]
      (is (valid-invalidation?
           {:topic :demo-counter
            :id "global-shared-counter"}))
      (is (not
           (valid-invalidation?
            {:topic :demo-counter})))))

  (testing "explainer returns a reusable explainer"
    (let [explain-invalidation (schema/explainer :gesso.live/invalidation)]
      (is (nil?
           (explain-invalidation
            {:topic :demo-counter
             :id "global-shared-counter"})))
      (is (some?
           (explain-invalidation
            {:topic :demo-counter}))))))

;; -----------------------------------------------------------------------------
;; Convenience validators
;; -----------------------------------------------------------------------------

(deftest convenience-validator-test
  (testing "convenience validators return valid values"
    (is (= {:topic :request}
           (schema/validate-primary-change! {:topic :request})))

    (is (= {:topic :request
            :id "req-1"}
           (schema/validate-invalidation!
            {:topic :request
             :id "req-1"})))

    (is (= {:topic :request
            :id "req-1"}
           (schema/validate-subscription!
            {:topic :request
             :id "req-1"})))

    (is (= {:event "live-update"
            :invalidation {:topic :request
                           :id "req-1"}}
           (schema/validate-live-event!
            {:event "live-update"
             :invalidation {:topic :request
                            :id "req-1"}})))))
