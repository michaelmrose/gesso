(ns gesso.live.invalidation-test
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.live.invalidation :as invalidation]))

;; -----------------------------------------------------------------------------
;; Fixtures
;; -----------------------------------------------------------------------------

(def request-change
  {:topic :request
   :id "req-1"
   :change/kind :updated})

(def global-change-without-id
  {:topic :global-announcement
   :change/kind :updated})

(def request-invalidation
  {:topic :request
   :id "req-1"
   :change/kind :updated})

(def store-invalidation
  {:topic :store-queue
   :id "store-1"
   :change/kind :updated})

(def manager-invalidation
  {:topic :manager-dashboard
   :id "store-1"
   :change/kind :updated})

(def request-rule
  {:when-topic :request
   :expand (fn [_ctx change]
             [{:topic :request
               :id (:id change)
               :change/kind (:change/kind change)}
              store-invalidation])})

(def updated-request-rule
  {:when-topic :request
   :when (fn [_ctx change]
           (= :updated (:change/kind change)))
   :expand (fn [_ctx _change]
             [manager-invalidation])})

(def never-rule
  {:when-topic :never
   :expand (fn [_ctx change]
             [change])})

(def ctx
  {:app/name :test})

;; -----------------------------------------------------------------------------
;; Rule compilation
;; -----------------------------------------------------------------------------

(deftest compile-rules-marks-rules-as-compiled-test
  (let [rules (invalidation/compile-rules [request-rule])]
    (is (vector? rules))
    (is (invalidation/compiled-rules? rules))
    (is (= [request-rule] rules))))

(deftest compile-rules-rejects-invalid-rule-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Invalid gesso.live value"
       (invalidation/compile-rules
        [{:when-topic :request}]))))

(deftest normalize-rules-validates-raw-rules-test
  (let [rules (invalidation/normalize-rules [request-rule])]
    (is (vector? rules))
    (is (invalidation/compiled-rules? rules))))

(deftest normalize-rules-can-skip-validation-test
  (let [bad-rules [{:when-topic :request}]
        rules (invalidation/normalize-rules bad-rules {:validate-rules? false})]
    (is (= bad-rules rules))
    (is (not (invalidation/compiled-rules? rules)))))

;; -----------------------------------------------------------------------------
;; Rule matching
;; -----------------------------------------------------------------------------

(deftest topic-rule-matches-topic-test
  (is (invalidation/rule-matches? request-rule ctx request-change))
  (is (not (invalidation/rule-matches? never-rule ctx request-change))))

(deftest predicate-rule-matches-predicate-test
  (let [rule {:when (fn [_ctx change]
                      (= "req-1" (:id change)))
              :expand (fn [_ctx change]
                        [change])}]
    (is (invalidation/rule-matches? rule ctx request-change))))

(deftest rule-with-topic-and-predicate-requires-both-test
  (is (invalidation/rule-matches? updated-request-rule ctx request-change))

  (let [created-change (assoc request-change :change/kind :created)]
    (is (not (invalidation/rule-matches? updated-request-rule ctx created-change)))))

(deftest predicate-failure-is-wrapped-with-context-test
  (let [rule {:when (fn [_ctx _change]
                      (throw (RuntimeException. "predicate exploded")))
              :expand (fn [_ctx change]
                        [change])}]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"predicate failed"
         (invalidation/rule-matches? rule ctx request-change)))))

(deftest matching-rules-returns-rules-in-order-test
  (let [rules [never-rule request-rule updated-request-rule]
        matches (invalidation/matching-rules rules ctx request-change)]
    (is (= [request-rule updated-request-rule] matches))))

;; -----------------------------------------------------------------------------
;; Single change expansion
;; -----------------------------------------------------------------------------

(deftest expand-applies-matching-topic-rule-test
  (let [result (invalidation/expand [request-rule] ctx request-change)]
    (is (= [request-invalidation store-invalidation] result))))

(deftest expand-applies-all-matching-rules-in-order-test
  (let [rules [request-rule updated-request-rule]
        result (invalidation/expand rules ctx request-change)]
    (is (= [request-invalidation store-invalidation manager-invalidation]
           result))))

(deftest expand-accepts-compiled-rules-test
  (let [rules (invalidation/compile-rules [request-rule])
        result (invalidation/expand rules ctx request-change)]
    (is (= [request-invalidation store-invalidation] result))))

(deftest expand-normalizes-single-map-output-test
  (let [rule {:when-topic :request
              :expand (fn [_ctx _change]
                        store-invalidation)}
        result (invalidation/expand [rule] ctx request-change)]
    (is (= [store-invalidation] result))))

(deftest expand-normalizes-nil-output-to-empty-test
  (let [rule {:when-topic :request
              :expand (fn [_ctx _change]
                        nil)}
        result (invalidation/expand [rule] ctx request-change)]
    (is (= [] result))))

(deftest expand-removes-nil-items-from-rule-output-test
  (let [rule {:when-topic :request
              :expand (fn [_ctx _change]
                        [nil store-invalidation nil])}
        result (invalidation/expand [rule] ctx request-change)]
    (is (= [store-invalidation] result))))

(deftest expand-rejects-non-collection-rule-output-test
  (let [rule {:when-topic :request
              :expand (fn [_ctx _change]
                        42)}]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid gesso.live invalidation expansion output"
         (invalidation/expand [rule] ctx request-change)))))

(deftest expand-wraps-rule-expansion-failure-test
  (let [rule {:when-topic :request
              :expand (fn [_ctx _change]
                        (throw (RuntimeException. "boom")))}]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"rule expansion failed"
         (invalidation/expand [rule] ctx request-change)))))

(deftest expand-invalid-rule-output-includes-context-test
  (let [rule {:when-topic :request
              :expand (fn [_ctx _change]
                        {:topic :store-queue})}
        ex (try
             (invalidation/expand [rule] ctx request-change)
             nil
             (catch clojure.lang.ExceptionInfo e
               e))]
    (is (some? ex))
    (is (= rule (:rule (ex-data ex))))
    (is (= request-change (:change (ex-data ex))))
    (is (= :gesso.live/invalidation (:schema-key (ex-data ex))))))

;; -----------------------------------------------------------------------------
;; Unmatched changes
;; -----------------------------------------------------------------------------

(deftest unmatched-keep-keeps-valid-primary-change-test
  (let [result (invalidation/expand [] ctx request-change)]
    (is (= [request-change] result))))

(deftest unmatched-keep-rejects-primary-change-without-id-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"cannot be kept as an invalidation"
       (invalidation/expand [] ctx global-change-without-id))))

(deftest unmatched-drop-drops-primary-change-test
  (let [result (invalidation/expand [] ctx global-change-without-id
                                    {:on-unmatched :drop})]
    (is (= [] result))))

(deftest unmatched-throw-throws-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"No gesso.live invalidation rule matched"
       (invalidation/expand [] ctx request-change
                            {:on-unmatched :throw}))))

(deftest unsupported-unmatched-policy-throws-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Invalid gesso.live value"
       (invalidation/expand [] ctx request-change
                            {:on-unmatched :explode}))))

;; -----------------------------------------------------------------------------
;; Dedupe and validation options
;; -----------------------------------------------------------------------------

(deftest expand-dedupes-by-default-test
  (let [rule {:when-topic :request
              :expand (fn [_ctx _change]
                        [store-invalidation store-invalidation])}
        result (invalidation/expand [rule] ctx request-change)]
    (is (= [store-invalidation] result))))

(deftest expand-can-preserve-duplicates-test
  (let [rule {:when-topic :request
              :expand (fn [_ctx _change]
                        [store-invalidation store-invalidation])}
        result (invalidation/expand [rule] ctx request-change
                                    {:dedupe? false})]
    (is (= [store-invalidation store-invalidation] result))))

(deftest validate-false-allows-invalid-rule-output-test
  (let [invalid-output {:topic :store-queue}
        rule {:when-topic :request
              :expand (fn [_ctx _change]
                        invalid-output)}
        result (invalidation/expand [rule] ctx request-change
                                    {:validate? false})]
    (is (= [invalid-output] result))))

(deftest validate-false-allows-unmatched-primary-change-without-id-test
  (let [result (invalidation/expand [] ctx global-change-without-id
                                    {:validate? false})]
    (is (= [global-change-without-id] result))))

;; -----------------------------------------------------------------------------
;; Many-change expansion
;; -----------------------------------------------------------------------------

(deftest expand-many-expands-all-changes-test
  (let [change-2 (assoc request-change :id "req-2")
        rule {:when-topic :request
              :expand (fn [_ctx change]
                        [{:topic :request
                          :id (:id change)
                          :change/kind :updated}])}
        result (invalidation/expand-many [rule] ctx [request-change change-2])]
    (is (= [{:topic :request
             :id "req-1"
             :change/kind :updated}
            {:topic :request
             :id "req-2"
             :change/kind :updated}]
           result))))

(deftest expand-many-dedupes-across-changes-by-default-test
  (let [rule {:when-topic :request
              :expand (fn [_ctx _change]
                        [store-invalidation])}
        changes [request-change (assoc request-change :id "req-2")]
        result (invalidation/expand-many [rule] ctx changes)]
    (is (= [store-invalidation] result))))

(deftest expand-many-can-preserve-duplicates-test
  (let [rule {:when-topic :request
              :expand (fn [_ctx _change]
                        [store-invalidation])}
        changes [request-change (assoc request-change :id "req-2")]
        result (invalidation/expand-many [rule] ctx changes
                                         {:dedupe? false})]
    (is (= [store-invalidation store-invalidation] result))))

;; -----------------------------------------------------------------------------
;; Convenience constructors
;; -----------------------------------------------------------------------------

(deftest expansion-rule-builds-topic-rule-test
  (let [rule (invalidation/expansion-rule :request
                                          (fn [_ctx change]
                                            [change]))]
    (is (= :request (:when-topic rule)))
    (is (fn? (:expand rule)))))

(deftest predicate-rule-builds-predicate-rule-test
  (let [rule (invalidation/predicate-rule
              (fn [_ctx change]
                (= :request (:topic change)))
              (fn [_ctx change]
                [change]))]
    (is (fn? (:when rule)))
    (is (fn? (:expand rule)))))

(deftest simple-invalidation-builds-canonical-updated-invalidation-test
  (let [result (invalidation/simple-invalidation :request "req-1")]
    (is (= {:topic :request
            :id "req-1"
            :change/kind :updated}
           result))))

(deftest simple-invalidation-accepts-custom-change-kind-test
  (let [result (invalidation/simple-invalidation :request "req-1" :deleted)]
    (is (= {:topic :request
            :id "req-1"
            :change/kind :deleted}
           result))))

(deftest simple-invalidation-rejects-missing-id-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Invalid gesso.live invalidation"
       (invalidation/simple-invalidation :request nil))))
