(ns gesso.live.source-test
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.live.source :as source]
   [manifold.stream :as s]))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(def timeout-ms 1000)

(def invalidation-1
  {:topic :request
   :id "req-1"
   :change/kind :updated})

(def invalidation-2
  {:topic :store-queue
   :id "store-1"
   :change/kind :updated})

(def invalid-invalidation
  {:topic :request})

(defn take-value
  [stream]
  (deref (s/take! stream) timeout-ms ::timeout))

(defn sleep-briefly
  []
  (Thread/sleep 25))

;; -----------------------------------------------------------------------------
;; Creation and stats
;; -----------------------------------------------------------------------------

(deftest create-builds-open-source-test
  (let [src (source/create {:id :test/source})]
    (try
      (let [stats (source/stats src)]
        (is (= :test/source (:id stats)))
        (is (= false (:closed? stats)))
        (is (= 0 (:tap-count stats)))
        (is (= 0 (:emitted-count stats)))
        (is (= 0 (:attempted-count stats)))
        (is (= 0 (:error-count stats))))
      (finally
        (source/close! src)))))

(deftest create-generates-id-when-missing-test
  (let [src (source/create)]
    (try
      (is (some? (:id (source/stats src))))
      (finally
        (source/close! src)))))

(deftest create-preserves-coalesce-window-metadata-test
  (let [src (source/create {:coalesce-window-ms 50})]
    (try
      (is (= 50 (:coalesce-window-ms (source/stats src))))
      (finally
        (source/close! src)))))

(deftest create-rejects-invalid-options-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Invalid gesso.live value"
       (source/create {:coalesce-window-ms -1}))))

(deftest create-rejects-invalid-on-error-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Invalid gesso.live value"
       (source/create {:on-error :not-a-function}))))

;; -----------------------------------------------------------------------------
;; Lifecycle
;; -----------------------------------------------------------------------------

(deftest close-marks-source-closed-test
  (let [src (source/create)]
    (is (= :closed (source/close! src)))
    (is (source/closed? src))))

(deftest close-is-idempotent-test
  (let [src (source/create)]
    (is (= :closed (source/close! src)))
    (is (= :closed (source/close! src)))
    (is (source/closed? src))))

(deftest changes-after-close-throws-test
  (let [src (source/create)]
    (source/close! src)
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"source is closed"
         (source/changes src)))))

(deftest emit-after-close-throws-test
  (let [src (source/create)]
    (source/close! src)
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"source is closed"
         (source/emit! src invalidation-1)))))

(deftest close-closes-active-taps-test
  (let [src (source/create)
        tap (source/changes src)]
    (source/close! src)
    (is (s/closed? tap))
    (is (= 0 (:tap-count (source/stats src))))))

;; -----------------------------------------------------------------------------
;; Taps
;; -----------------------------------------------------------------------------

(deftest changes-registers-tap-test
  (let [src (source/create)]
    (try
      (let [tap (source/changes src)]
        (is (some? tap))
        (is (= 1 (:tap-count (source/stats src)))))
      (finally
        (source/close! src)))))

(deftest closing-tap-removes-it-from-source-test
  (let [src (source/create)
        tap (source/changes src)]
    (try
      (is (= 1 (:tap-count (source/stats src))))
      (s/close! tap)
      (sleep-briefly)
      (is (= 0 (:tap-count (source/stats src))))
      (finally
        (source/close! src)))))

(deftest closed-tap-is-cleaned-up-on-emit-test
  (let [src (source/create)
        tap (source/changes src)]
    (try
      (s/close! tap)
      (sleep-briefly)
      (let [result (source/emit! src invalidation-1)]
        (is (= 0 (:tap-count result)))
        (is (= 0 (:attempted result))))
      (finally
        (source/close! src)))))

;; -----------------------------------------------------------------------------
;; Emission
;; -----------------------------------------------------------------------------

(deftest emit-with-no-taps-records-no-attempts-test
  (let [src (source/create {:id :test/source})]
    (try
      (let [result (source/emit! src invalidation-1)]
        (is (= :emitted (:status result)))
        (is (= :test/source (:source/id result)))
        (is (= 0 (:tap-count result)))
        (is (= 0 (:attempted result)))
        (is (= invalidation-1 (:invalidation result))))
      (finally
        (source/close! src)))))

(deftest emit-increments-emitted-count-test
  (let [src (source/create)]
    (try
      (source/emit! src invalidation-1)
      (is (= 1 (:emitted-count (source/stats src))))
      (finally
        (source/close! src)))))

(deftest emit-to-one-tap-test
  (let [src (source/create)
        tap (source/changes src)]
    (try
      (let [result (source/emit! src invalidation-1)]
        (is (= 1 (:tap-count result)))
        (is (= 1 (:attempted result)))
        (is (= invalidation-1 (take-value tap))))
      (finally
        (source/close! src)))))

(deftest emit-to-many-taps-test
  (let [src (source/create)
        tap-a (source/changes src)
        tap-b (source/changes src)]
    (try
      (let [result (source/emit! src invalidation-1)]
        (is (= 2 (:tap-count result)))
        (is (= 2 (:attempted result)))
        (is (= invalidation-1 (take-value tap-a)))
        (is (= invalidation-1 (take-value tap-b))))
      (finally
        (source/close! src)))))

(deftest emit-increments-attempted-count-test
  (let [src (source/create)
        tap-a (source/changes src)
        tap-b (source/changes src)]
    (try
      (source/emit! src invalidation-1)
      (is (= invalidation-1 (take-value tap-a)))
      (is (= invalidation-1 (take-value tap-b)))
      (is (= 2 (:attempted-count (source/stats src))))
      (finally
        (source/close! src)))))

(deftest emit-rejects-primary-change-without-id-test
  (let [src (source/create)]
    (try
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid gesso.live value"
           (source/emit! src invalid-invalidation)))
      (is (= 0 (:emitted-count (source/stats src))))
      (finally
        (source/close! src)))))

;; -----------------------------------------------------------------------------
;; Emit many
;; -----------------------------------------------------------------------------

(deftest emit-many-emits-all-invalidations-test
  (let [src (source/create)
        tap (source/changes src)]
    (try
      (let [result (source/emit-many! src [invalidation-1 invalidation-2])]
        (is (= :emitted-many (:status result)))
        (is (= 2 (:count result)))
        (is (= 2 (count (:results result))))
        (is (= invalidation-1 (take-value tap)))
        (is (= invalidation-2 (take-value tap))))
      (finally
        (source/close! src)))))

(deftest emit-many-validates-before-emission-test
  (let [src (source/create)
        tap (source/changes src)]
    (try
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid gesso.live value"
           (source/emit-many! src [invalidation-1 invalid-invalidation])))
      (is (= 0 (:emitted-count (source/stats src))))
      (is (= ::timeout (take-value tap)))
      (finally
        (source/close! src)))))

(deftest emit-many-after-close-throws-test
  (let [src (source/create)]
    (source/close! src)
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"source is closed"
         (source/emit-many! src [invalidation-1])))))

;; -----------------------------------------------------------------------------
;; Errors
;; -----------------------------------------------------------------------------

(deftest errors-start-empty-test
  (let [src (source/create)]
    (try
      (is (= [] (source/errors src)))
      (finally
        (source/close! src)))))

(deftest clear-errors-returns-cleared-test
  (let [src (source/create)]
    (try
      (is (= :cleared (source/clear-errors! src)))
      (is (= [] (source/errors src)))
      (finally
        (source/close! src)))))
