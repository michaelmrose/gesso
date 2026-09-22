(ns gesso.live.schema-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.progression :as progression]
   [gesso.live.schema :as schema]))

;; -----------------------------------------------------------------------------
;; Schema lookup
;; -----------------------------------------------------------------------------

(def ^:private retired-schema-keys
  #{:gesso.live/positive-milliseconds
    :gesso.live/scopes
    :gesso.live/dispatch-mode
    :gesso.live/dispatch-options
    :gesso.live/core-emit-options
    :gesso.live/stream-handler-options
    :gesso.live/coalesce-by-options
    :gesso.live/isolation-options
    :gesso.live/fragment-config
    :gesso.live/fragment-cache-options
    :gesso.live/fragment-singleflight-options
    :gesso.live/sse-response-options
    :gesso.live/sse-frame-event
    :gesso.live/client-descriptor
    :gesso.live/oob-target
    :gesso.live/oob-send-options})

(deftest schema-lookup-test
  (testing "current runtime boundary schema keys resolve"
    (doseq [schema-key [:gesso.live/primary-change
                        :gesso.live/invalidation
                        :gesso.live/subscription
                        :gesso.live/live-event
                        :gesso.live/progression
                        :gesso.live/invalidation-rules
                        :gesso.live/invalidation-options
                        :gesso.live/source-options
                        :gesso.live/dispatcher-options
                        :gesso.live/flow-for-subscription-options
                        :gesso.live/invalidation-event-options]]
      (is (some? (schema/schema schema-key))
          (str "current schema should resolve: " schema-key))))

  (testing "retired schemas no longer advertise unenforced or obsolete contracts"
    (doseq [schema-key retired-schema-keys]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Unknown gesso.live schema key"
           (schema/schema schema-key))
          (str "retired schema should not resolve: " schema-key))))

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

  (testing "milliseconds are non-negative"
    (is (schema/validate :gesso.live/milliseconds 0))
    (is (schema/validate :gesso.live/milliseconds 15000))
    (is (not (schema/validate :gesso.live/milliseconds -1)))))

;; -----------------------------------------------------------------------------
;; Authoritative progression
;; -----------------------------------------------------------------------------

(deftest progression-requirement-schema-test
  (let [basis {:authority :xtdb
               :tx-id "42"}
        requirement (progression/requirement basis)
        wire (progression/requirement->wire requirement)]
    (testing "normalized internal progression requirements validate"
      (is (schema/validate :gesso.live/progression requirement))
      (is (= requirement
             (schema/validate! :gesso.live/progression requirement))))

    (testing "wire progression is not accepted at the internal schema boundary"
      (is (not (schema/validate :gesso.live/progression wire))))

    (testing "raw bases and malformed requirement-like maps do not validate"
      (is (not (schema/validate :gesso.live/progression basis)))
      (is (not
           (schema/validate
            :gesso.live/progression
            {:gesso.live.progression/type progression/requirement-type
             :gesso.live.progression/version progression/progression-version
             :bases [basis]})))
      (is (not
           (schema/validate
            :gesso.live/progression
            {:gesso.live.progression/type progression/requirement-type
             :gesso.live.progression/version progression/progression-version
             :bases #{}}))))))

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

  (testing "primary changes may carry normalized progression"
    (let [requirement (progression/requirement {:authority :xtdb
                                                :tx-id "42"})]
      (is (schema/validate
           :gesso.live/primary-change
           {:topic :request
            :id "req-1"
            :progression requirement}))
      (is (not
           (schema/validate
            :gesso.live/primary-change
            {:topic :request
             :id "req-1"
             :progression (progression/requirement->wire requirement)})))))

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

  (testing "invalidations may carry normalized progression"
    (let [requirement (progression/requirement {:authority :xtdb
                                                :tx-id "42"})]
      (is (schema/validate
           :gesso.live/invalidation
           {:topic :store-queue
            :id "store-1"
            :progression requirement}))
      (is (not
           (schema/validate
            :gesso.live/invalidation
            {:topic :store-queue
             :id "store-1"
             :progression (progression/requirement->wire requirement)})))))

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

  (testing "live events may carry normalized progression"
    (let [requirement (progression/requirement {:authority :xtdb
                                                :tx-id "42"})]
      (is (schema/validate
           :gesso.live/live-event
           {:event "live-update"
            :invalidation {:topic :demo-counter
                           :id "global-shared-counter"
                           :progression requirement}
            :progression requirement}))
      (is (not
           (schema/validate
            :gesso.live/live-event
            {:event "live-update"
             :invalidation {:topic :demo-counter
                            :id "global-shared-counter"}
             :progression (progression/requirement->wire requirement)})))))

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
;; Current runtime option boundaries
;; -----------------------------------------------------------------------------

(deftest source-and-dispatcher-options-test
  (testing "source options validate"
    (is (schema/validate
         :gesso.live/source-options
         {:id :app/live
          :coalesce-window-ms 50}))

    (is (not
         (schema/validate
          :gesso.live/source-options
          {:coalesce-window-ms -1}))))

  (testing "dispatcher construction options validate"
    (is (schema/validate
         :gesso.live/dispatcher-options
         {:name "gesso-live-expansion"
          :threads 4
          :queue-size 1024
          :on-overflow :throw}))

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

(deftest flow-options-test
  (testing "flow subscription options validate"
    (is (schema/validate
         :gesso.live/flow-for-subscription-options
         {:subscription {:topic :demo-counter
                         :id "global-shared-counter"}
          :interested? (fn [_sub _invalidation]
                         true)})))

  (testing "invalidation event options accept the current event/data contract"
    (is (schema/validate
         :gesso.live/invalidation-event-options
         {:event :live-update
          :data {:reason :test}}))))

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

  (testing "hot-path validators and explainers are compiled once and reused"
    (doseq [schema-key [:gesso.live/primary-change
                        :gesso.live/invalidation
                        :gesso.live/subscription
                        :gesso.live/live-event]]
      (is (identical? (schema/validator schema-key)
                      (schema/validator schema-key))
          (str "validator should be reused for " schema-key))
      (is (identical? (schema/explainer schema-key)
                      (schema/explainer schema-key))
          (str "explainer should be reused for " schema-key))))

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
