(ns gesso.live.dispatch-test
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.live.dispatch :as dispatch])
  (:import
   [java.util.concurrent CountDownLatch TimeUnit]))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(def timeout-ms 1000)

(defn await-latch
  [^CountDownLatch latch]
  (.await latch timeout-ms TimeUnit/MILLISECONDS))

(defn release
  [^CountDownLatch latch]
  (.countDown latch))

(defn promise-value
  [p]
  (deref p timeout-ms ::timeout))

(defn close
  [dispatcher]
  (dispatch/close! dispatcher))

;; -----------------------------------------------------------------------------
;; Creation and lifecycle
;; -----------------------------------------------------------------------------

(deftest create-builds-open-dispatcher-test
  (let [d (dispatch/create {:name "test-dispatch"
                            :threads 1
                            :queue-size 4
                            :on-overflow :throw})]
    (try
      (let [s (dispatch/stats d)]
        (is (not (dispatch/closed? d)))
        (is (= "test-dispatch" (:name s)))
        (is (= 1 (:threads s)))
        (is (= 4 (:queue-capacity s))))
      (finally
        (close d)))))

(deftest create-rejects-invalid-options-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Invalid gesso.live value"
       (dispatch/create {:threads 0}))))

(deftest create-rejects-invalid-on-error-hook-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #":on-error must be a function"
       (dispatch/create {:on-error :not-a-function}))))

(deftest close-marks-dispatcher-closed-test
  (let [d (dispatch/create {:threads 1 :queue-size 4})]
    (is (= :closed (dispatch/close! d)))
    (is (dispatch/closed? d))))

(deftest submit-after-close-throws-test
  (let [d (dispatch/create {:threads 1 :queue-size 4})]
    (dispatch/close! d)
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"dispatcher is closed"
         (dispatch/submit! d #(identity :nope))))))

;; -----------------------------------------------------------------------------
;; Sync dispatch
;; -----------------------------------------------------------------------------

(deftest run-sync-runs-function-job-test
  (let [result (dispatch/run-sync! (fn [] 42))]
    (is (= {:status :ran :result 42} result))))

(deftest run-sync-runs-map-job-test
  (let [result (dispatch/run-sync! {:run (fn [] :ok)})]
    (is (= {:status :ran :result :ok} result))))

(deftest run-sync-throws-job-failures-test
  (is (thrown-with-msg?
       RuntimeException
       #"boom"
       (dispatch/run-sync! {:run (fn []
                                   (throw (RuntimeException. "boom")))}))))

(deftest run-sync-rejects-invalid-job-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"job must be a function"
       (dispatch/run-sync! {:not-run identity}))))

(deftest dispatch-sync-runs-job-test
  (let [result (dispatch/dispatch! {:dispatch :sync}
                                   {:run (fn [] :ran)})]
    (is (= {:status :ran :result :ran} result))))

;; -----------------------------------------------------------------------------
;; Async submission
;; -----------------------------------------------------------------------------

(deftest submit-runs-async-job-test
  (let [p (promise)
        d (dispatch/create {:threads 1 :queue-size 4})]
    (try
      (let [result (dispatch/submit! d #(deliver p :ran))]
        (is (= :submitted (:status result)))
        (is (= :ran (promise-value p))))
      (finally
        (close d)))))

(deftest dispatch-async-submits-job-test
  (let [p (promise)
        d (dispatch/create {:threads 1 :queue-size 4})]
    (try
      (let [result (dispatch/dispatch! {:dispatch :async :dispatcher d}
                                       {:run (fn [] (deliver p :ran))})]
        (is (= :submitted (:status result)))
        (is (= :ran (promise-value p))))
      (finally
        (close d)))))

(deftest dispatch-async-requires-dispatcher-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"requires :dispatcher"
       (dispatch/dispatch! {:dispatch :async}
                           {:run (fn [] :ran)}))))

(deftest dispatch-rejects-unknown-mode-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Unsupported gesso.live dispatch mode"
       (dispatch/dispatch! {:dispatch :later}
                           {:run (fn [] :ran)}))))

;; -----------------------------------------------------------------------------
;; Async errors
;; -----------------------------------------------------------------------------

(deftest async-job-errors-are-recorded-test
  (let [p (promise)
        d (dispatch/create {:threads 1
                            :queue-size 4
                            :on-error (fn [error entry]
                                        (deliver p {:error error
                                                    :entry entry}))})]
    (try
      (dispatch/submit! d {:run (fn []
                                  (throw (RuntimeException. "boom")))})
      (let [result (promise-value p)]
        (is (not= ::timeout result))
        (is (= "boom" (ex-message (:error result))))
        (is (= 1 (count (dispatch/errors d)))))
      (finally
        (close d)))))

(deftest async-job-error-calls-dispatcher-hook-test
  (let [p (promise)
        d (dispatch/create {:threads 1
                            :queue-size 4
                            :on-error (fn [error entry]
                                        (deliver p {:error error
                                                    :entry entry}))})]
    (try
      (dispatch/submit! d {:run (fn []
                                  (throw (RuntimeException. "hook boom")))})
      (let [result (promise-value p)]
        (is (not= ::timeout result))
        (is (= "hook boom" (ex-message (:error result))))
        (is (some? (get-in result [:entry :job-id]))))
      (finally
        (close d)))))

(deftest async-job-error-calls-job-hook-test
  (let [p (promise)
        d (dispatch/create {:threads 1 :queue-size 4})]
    (try
      (dispatch/submit! d {:run (fn []
                                  (throw (RuntimeException. "job hook boom")))
                           :on-error (fn [error entry]
                                       (deliver p {:error error
                                                   :entry entry}))})
      (let [result (promise-value p)]
        (is (not= ::timeout result))
        (is (= "job hook boom" (ex-message (:error result))))
        (is (some? (get-in result [:entry :job-id]))))
      (finally
        (close d)))))

(deftest clear-errors-clears-recorded-errors-test
  (let [p (promise)
        d (dispatch/create {:threads 1
                            :queue-size 4
                            :on-error (fn [_error _entry]
                                        (deliver p :error-recorded))})]
    (try
      (dispatch/submit! d {:run (fn []
                                  (throw (RuntimeException. "boom")))})
      (is (= :error-recorded (promise-value p)))
      (is (= 1 (count (dispatch/errors d))))
      (is (= :cleared (dispatch/clear-errors! d)))
      (is (= [] (dispatch/errors d)))
      (finally
        (close d)))))

;; -----------------------------------------------------------------------------
;; Overflow :throw
;; -----------------------------------------------------------------------------

(deftest throw-overflow-throws-when-queue-is-full-test
  (let [started (CountDownLatch. 1)
        release-job (CountDownLatch. 1)
        d (dispatch/create {:threads 1
                            :queue-size 1
                            :on-overflow :throw})]
    (try
      (dispatch/submit! d {:run (fn []
                                  (release started)
                                  (await-latch release-job))})
      (is (await-latch started))
      (dispatch/submit! d {:run (fn [] :queued)})
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"queue is full"
           (dispatch/submit! d {:run (fn [] :overflow)})))
      (finally
        (release release-job)
        (close d)))))

;; -----------------------------------------------------------------------------
;; Overflow :drop
;; -----------------------------------------------------------------------------

(deftest drop-overflow-drops-new-job-when-queue-is-full-test
  (let [started (CountDownLatch. 1)
        release-job (CountDownLatch. 1)
        dropped-p (promise)
        d (dispatch/create {:threads 1
                            :queue-size 1
                            :on-overflow :drop})]
    (try
      (dispatch/submit! d {:run (fn []
                                  (release started)
                                  (await-latch release-job))})
      (is (await-latch started))
      (dispatch/submit! d {:run (fn [] :queued)})
      (let [result (dispatch/submit! d {:run (fn [] :dropped)
                                        :on-drop #(deliver dropped-p %)})]
        (is (= :dropped (:status result)))
        (is (= :queue-full (:reason result)))
        (is (= :queue-full (:reason (promise-value dropped-p)))))
      (finally
        (release release-job)
        (close d)))))

;; -----------------------------------------------------------------------------
;; Overflow :coalesce
;; -----------------------------------------------------------------------------

(deftest coalesce-requires-coalesce-key-test
  (let [d (dispatch/create {:threads 1
                            :queue-size 4
                            :on-overflow :coalesce})]
    (try
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"require :coalesce-key"
           (dispatch/submit! d {:run (fn [] :no-key)})))
      (finally
        (close d)))))

(deftest coalesce-replaces-queued-job-with-same-key-test
  (let [started (CountDownLatch. 1)
        release-job (CountDownLatch. 1)
        done (CountDownLatch. 2)
        dropped-p (promise)
        ran (atom [])
        d (dispatch/create {:threads 1
                            :queue-size 10
                            :on-overflow :coalesce})]
    (try
      (dispatch/submit! d {:coalesce-key :block
                           :run (fn []
                                  (release started)
                                  (await-latch release-job)
                                  (swap! ran conj :block)
                                  (release done))})
      (is (await-latch started))
      (dispatch/submit! d {:coalesce-key :same
                           :run (fn []
                                  (swap! ran conj :old)
                                  (release done))
                           :on-drop #(deliver dropped-p %)})
      (dispatch/submit! d {:coalesce-key :same
                           :run (fn []
                                  (swap! ran conj :new)
                                  (release done))})
      (release release-job)
      (is (await-latch done))
      (is (= [:block :new] @ran))
      (is (= :coalesced (:reason (promise-value dropped-p))))
      (finally
        (release release-job)
        (close d)))))

(deftest coalesce-does-not-cancel-running-job-test
  (let [started (CountDownLatch. 1)
        release-job (CountDownLatch. 1)
        done (CountDownLatch. 2)
        ran (atom [])
        d (dispatch/create {:threads 1
                            :queue-size 10
                            :on-overflow :coalesce})]
    (try
      (dispatch/submit! d {:coalesce-key :same
                           :run (fn []
                                  (release started)
                                  (await-latch release-job)
                                  (swap! ran conj :running)
                                  (release done))})
      (is (await-latch started))
      (dispatch/submit! d {:coalesce-key :same
                           :run (fn []
                                  (swap! ran conj :queued)
                                  (release done))})
      (release release-job)
      (is (await-latch done))
      (is (= [:running :queued] @ran))
      (finally
        (release release-job)
        (close d)))))

;; -----------------------------------------------------------------------------
;; Shutdown drop behavior
;; -----------------------------------------------------------------------------

(deftest close-drops-queued-jobs-test
  (let [started (CountDownLatch. 1)
        release-job (CountDownLatch. 1)
        dropped-p (promise)
        d (dispatch/create {:threads 1
                            :queue-size 4
                            :on-overflow :throw})]
    (dispatch/submit! d {:run (fn []
                                (release started)
                                (await-latch release-job))})
    (is (await-latch started))
    (dispatch/submit! d {:run (fn [] :queued)
                         :on-drop #(deliver dropped-p %)})
    (dispatch/close! d)
    (is (= :dispatcher-closed (:reason (promise-value dropped-p))))
    (is (dispatch/closed? d))
    (release release-job)))

;; -----------------------------------------------------------------------------
;; Stats
;; -----------------------------------------------------------------------------

(deftest stats-report-basic-status-test
  (let [d (dispatch/create {:name "stats-test"
                            :threads 1
                            :queue-size 4
                            :on-overflow :drop})]
    (try
      (let [s (dispatch/stats d)]
        (is (= "stats-test" (:name s)))
        (is (= 1 (:threads s)))
        (is (= 4 (:queue-capacity s)))
        (is (= 0 (:queue-size s)))
        (is (= 0 (:pending-keys s)))
        (is (= 0 (:error-count s)))
        (is (= false (:closed? s)))
        (is (= :drop (:on-overflow s))))
      (finally
        (close d)))))
