(ns gesso.live.integration-test
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.live.dispatch :as dispatch]
   [gesso.live.invalidation :as invalidation]
   [gesso.live.source :as source]
   [manifold.stream :as s]))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(def timeout-ms 1000)

(def ctx
  {:app/name :integration-test})

(def request-change
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

(defn take-value
  [stream]
  (deref (s/take! stream) timeout-ms ::timeout))

(defn close-source
  [src]
  (source/close! src))

(defn close-dispatcher
  [dispatcher]
  (dispatch/close! dispatcher))

(defn request-rules
  []
  (invalidation/compile-rules
   [{:when-topic :request
     :expand (fn [_ctx change]
               [{:topic :request
                 :id (:id change)
                 :change/kind (:change/kind change)}
                store-invalidation
                manager-invalidation])}]))

(defn emit-expanded!
  [src rules change]
  (let [expanded (invalidation/expand rules ctx change)]
    (source/emit-many! src expanded)))

;; -----------------------------------------------------------------------------
;; invalidation.clj + source.clj
;; -----------------------------------------------------------------------------

(deftest expanded-invalidations-emit-through-source-test
  (let [src (source/create {:id :integration/source})
        tap (source/changes src)
        rules (request-rules)]
    (try
      (let [result (emit-expanded! src rules request-change)]
        (is (= :emitted-many (:status result)))
        (is (= 3 (:count result)))
        (is (= request-change (take-value tap)))
        (is (= store-invalidation (take-value tap)))
        (is (= manager-invalidation (take-value tap))))
      (finally
        (close-source src)))))

(deftest unmatched-valid-primary-change-can-emit-through-source-test
  (let [src (source/create)
        tap (source/changes src)]
    (try
      (let [expanded (invalidation/expand [] ctx request-change)
            result (source/emit-many! src expanded)]
        (is (= [request-change] expanded))
        (is (= :emitted-many (:status result)))
        (is (= request-change (take-value tap))))
      (finally
        (close-source src)))))

(deftest unmatched-invalid-primary-change-does-not-reach-source-test
  (let [src (source/create)
        tap (source/changes src)
        change {:topic :global-announcement
                :change/kind :updated}]
    (try
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"cannot be kept as an invalidation"
           (emit-expanded! src [] change)))
      (is (= ::timeout (take-value tap)))
      (is (= 0 (:emitted-count (source/stats src))))
      (finally
        (close-source src)))))

;; -----------------------------------------------------------------------------
;; dispatch.clj + invalidation.clj + source.clj
;; -----------------------------------------------------------------------------

(deftest async-dispatch-can-expand-and-emit-through-source-test
  (let [src (source/create)
        tap (source/changes src)
        rules (request-rules)
        done (promise)
        dispatcher (dispatch/create {:threads 1
                                     :queue-size 8
                                     :on-overflow :throw
                                     :on-error (fn [error entry]
                                                 (deliver done {:error error
                                                                :entry entry}))})]
    (try
      (dispatch/submit!
       dispatcher
       {:run (fn []
               (emit-expanded! src rules request-change)
               (deliver done :ok))})

      (is (= :ok (deref done timeout-ms ::timeout)))
      (is (= request-change (take-value tap)))
      (is (= store-invalidation (take-value tap)))
      (is (= manager-invalidation (take-value tap)))
      (finally
        (close-dispatcher dispatcher)
        (close-source src)))))

(deftest async-dispatch-records-expansion-failure-test
  (let [src (source/create)
        bad-change {:topic :global-announcement
                    :change/kind :updated}
        done (promise)
        dispatcher (dispatch/create {:threads 1
                                     :queue-size 8
                                     :on-overflow :throw
                                     :on-error (fn [error entry]
                                                 (deliver done {:error error
                                                                :entry entry}))})]
    (try
      (dispatch/submit!
       dispatcher
       {:run (fn []
               (emit-expanded! src [] bad-change))})

      (let [result (deref done timeout-ms ::timeout)]
        (is (not= ::timeout result))
        (is (some? (:error result)))
        (is (some? (:entry result)))
        (is (= 0 (:emitted-count (source/stats src)))))
      (finally
        (close-dispatcher dispatcher)
        (close-source src)))))
