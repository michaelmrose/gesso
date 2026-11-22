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

(deftest throwing-dispatcher-error-hook-does-not-kill-worker-test
  ;; Error reporting is application-supplied code. An AssertionError in that
  ;; callback must not kill the only dispatcher worker and strand queued work.
  (let [hook-called (promise)
        follow-up (promise)
        d (dispatch/create {:threads 1 :queue-size 4
                            :on-error (fn [_ _]
                                        (deliver hook-called :called)
                                        (throw (AssertionError. "hook failure")))})]
    (try
      (is (= :submitted (:status
                         (dispatch/submit! d
                           {:run #(throw (RuntimeException. "job failure"))}))))
      (is (= :called (promise-value hook-called)))
      (is (= :submitted (:status
                         (dispatch/submit! d {:run #(deliver follow-up :ran)}))))
      (is (= :ran (promise-value follow-up))
          "Subsequent work must not be stranded by callback failure")
      (is (= 1 (count (dispatch/errors d))))
      (finally (close d)))))

(deftest throwing-job-error-hook-does-not-kill-worker-test
  (let [hook-called (promise)
        follow-up (promise)
        d (dispatch/create {:threads 1 :queue-size 4})]
    (try
      (dispatch/submit! d
        {:run #(throw (RuntimeException. "job failure"))
         :on-error (fn [_ _]
                     (deliver hook-called :called)
                     (throw (AssertionError. "job callback failure")))})
      (is (= :called (promise-value hook-called)))
      (dispatch/submit! d {:run #(deliver follow-up :ran)})
      (is (= :ran (promise-value follow-up))
          "A failing job hook must not retire a healthy worker")
      (finally (close d)))))

(deftest throwing-coalesce-drop-hook-does-not-obscure-accepted-submission-test
  ;; The replacement is already published before on-drop runs. A callback
  ;; failure cannot turn a committed submission into a misleading exception.
  (let [started (CountDownLatch. 1)
        release-running (CountDownLatch. 1)
        replacement-ran (promise)
        drop-called (promise)
        d (dispatch/create {:threads 1 :queue-size 2
                            :on-overflow :coalesce})]
    (try
      (dispatch/submit! d {:coalesce-key :busy
                           :run #(do (release started)
                                     (await-latch release-running))})
      (is (await-latch started))
      (dispatch/submit! d {:coalesce-key :pending
                           :run (fn [] :old)
                           :on-drop (fn [_]
                                      (deliver drop-called :called)
                                      (throw (AssertionError. "drop hook failure")))})
      (is (= :submitted
             (:status (dispatch/submit! d
                         {:coalesce-key :pending
                          :run #(deliver replacement-ran :ran)})))
          "A committed replacement must return :submitted despite a bad hook")
      (is (= :called (promise-value drop-called)))
      (release release-running)
      (is (= :ran (promise-value replacement-ran)))
      (finally
        (release release-running)
        (close d)))))

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

(deftest interrupted-async-job-is-visible-to-dispatcher-error-hook-test
  ;; An InterruptedException raised by user work clears the worker interrupt
  ;; bit if the work was blocked. Recording that error must restore the signal
  ;; before notifying its error hook, without abandoning later queued work.
  (let [cause (InterruptedException. "job interrupted")
        observed (promise)
        next-job (promise)
        d (dispatch/create
           {:threads 1 :queue-size 4
            :on-error (fn [error _entry]
                        (let [interrupted?
                              (.isInterrupted (Thread/currentThread))]
                          (Thread/interrupted)
                          (deliver observed [error interrupted?])))})]
    (try
      (is (= :submitted
             (:status (dispatch/submit! d {:run #(throw cause)}))))
      (let [result (promise-value observed)]
        (is (not= ::timeout result))
        (when (vector? result)
          (is (identical? cause (first result)))
          (is (true? (second result))
              "Error hooks must observe the interrupted job's signal")))
      (dispatch/submit! d {:run #(deliver next-job :ran)})
      (is (= :ran (promise-value next-job))
          "Interrupted work must not permanently strand a healthy worker")
      (is (identical? cause (:error (first (dispatch/errors d)))))
      (finally
        (close d)))))

(deftest interrupted-dispatcher-error-hook-preserves-signal-for-job-hook-test
  ;; A suppressed dispatcher-level error hook must not hide its interruption
  ;; from the independent job-level error reporter running on that thread.
  (let [cause (RuntimeException. "ordinary job failure")
        observed (promise)
        follow-up (promise)
        d (dispatch/create
           {:threads 1 :queue-size 4
            :on-error (fn [_ _]
                        (throw (InterruptedException.
                                "dispatcher error hook interrupted")))})]
    (try
      (dispatch/submit!
       d {:run #(throw cause)
          :on-error (fn [error _entry]
                      (let [interrupted?
                            (.isInterrupted (Thread/currentThread))]
                        (Thread/interrupted)
                        (deliver observed [error interrupted?])))})
      (let [result (promise-value observed)]
        (is (not= ::timeout result))
        (when (vector? result)
          (is (identical? cause (first result)))
          (is (true? (second result))
              "Swallowed hook interrupts must reach the following hook")))
      (dispatch/submit! d {:run #(deliver follow-up :ran)})
      (is (= :ran (promise-value follow-up)))
      (is (identical? cause (:error (first (dispatch/errors d)))))
      (finally
        (close d)))))

(deftest interrupted-overflow-drop-hook-preserves-submitter-interrupt-test
  ;; Overflow reporting is intentionally best-effort. An interrupted on-drop
  ;; hook must not change :dropped into a false submission exception, but it
  ;; must retain the caller's interrupt flag.
  (let [started (CountDownLatch. 1)
        release-running (CountDownLatch. 1)
        next-job (promise)
        d (dispatch/create {:threads 1 :queue-size 1 :on-overflow :drop})]
    (try
      (dispatch/submit! d {:run #(do (release started)
                                    (await-latch release-running))})
      (is (await-latch started))
      (dispatch/submit! d {:run #(deliver next-job :ran)})
      (Thread/interrupted)
      (let [result (dispatch/submit!
                    d {:run #(throw (AssertionError. "must be dropped"))
                       :on-drop (fn [_]
                                  (throw (InterruptedException.
                                          "drop callback interrupted")))})
            interrupted? (.isInterrupted (Thread/currentThread))]
        (Thread/interrupted)
        (is (= :dropped (:status result)))
        (is (= :queue-full (:reason result)))
        (is interrupted? "A swallowed drop-hook interrupt must survive"))
      (release release-running)
      (is (= :ran (promise-value next-job)))
      (finally
        (Thread/interrupted)
        (release release-running)
        (close d)))))

(deftest interrupted-shutdown-drop-hook-preserves-close-and-interrupt-test
  ;; Closing retires queued work and invokes its on-drop hook *before* joining
  ;; workers. An interrupted callback must not make the subsequent join throw
  ;; or prevent close! from finishing its already-committed cleanup.
  (let [started (CountDownLatch. 1)
        release-running (CountDownLatch. 1)
        dropped (promise)
        d (dispatch/create {:threads 1 :queue-size 1})]
    (try
      ;; Deliberately keep the worker alive through shutdown interruption.
      ;; Otherwise Thread.join may return without an interruptible wait.
      (dispatch/submit! d {:run #(do (release started)
                                    (loop []
                                      (when-not (try
                                                  (.await release-running
                                                          50 TimeUnit/MILLISECONDS)
                                                  (catch InterruptedException _
                                                    false))
                                        (recur))))})
      (is (await-latch started))
      (dispatch/submit! d {:run (fn [] :must-not-run)
                           :on-drop (fn [info]
                                      (deliver dropped info)
                                      (throw (InterruptedException.
                                              "shutdown callback interrupted")))})
      (Thread/interrupted)
      (let [outcome (try
                      (dispatch/close! d)
                      (catch Throwable e e))
            interrupted? (.isInterrupted (Thread/currentThread))]
        (Thread/interrupted)
        (is (= :closed outcome)
            "An interrupted drop hook must not abort worker shutdown")
        (is interrupted? "Shutdown must retain the caller interrupt signal")
        (is (= :dispatcher-closed (:reason (promise-value dropped))))
        (is (dispatch/closed? d)))
      (finally
        (Thread/interrupted)
        (release release-running)
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

(deftest close-unblocks-blocked-submit-without-stranding-work-test
  (let [started (CountDownLatch. 1)
        release-running (CountDownLatch. 1)
        dropped (promise)
        d (dispatch/create {:threads 1
                            :queue-size 1
                            :on-overflow :block})]
    (try
      (dispatch/submit! d {:run (fn []
                                  (release started)
                                  (.await release-running))})
      (is (await-latch started))
      (dispatch/submit! d {:run (fn [] :queued)
                           :on-drop #(deliver dropped %)})
      (let [attempt (future
                      (try
                        (dispatch/submit! d {:run (fn [] :stranded)})
                        (catch clojure.lang.ExceptionInfo e
                          (ex-message e))))]
        (is (not (realized? attempt)))
        (is (= :closed (dispatch/close! d)))
        (is (= "gesso.live dispatcher is closed."
               (deref attempt timeout-ms ::timed-out))
            "a blocked caller must not enqueue new work after close")
        (is (= :dispatcher-closed (:reason (promise-value dropped))))
        (is (zero? (.size ^java.util.concurrent.ArrayBlockingQueue
                          (:queue d)))))
      (finally
        (release release-running)
        (close d)))))

(deftest close-racing-with-nonblocking-submit-never-strands-job-test
  ;; Force shutdown between the initial open check and queue insertion. The
  ;; original implementation accepted an orphaned job after close! returned.
  (doseq [policy [:throw :drop :coalesce]]
    (let [d (dispatch/create {:threads 1
                              :queue-size 2
                              :on-overflow policy})
          checked (promise)
          resume (CountDownLatch. 1)
          first-check? (atom true)
          original (var-get #'gesso.live.dispatch/ensure-open!)]
      (try
        (with-redefs-fn
          {#'gesso.live.dispatch/ensure-open!
           (fn [dispatcher]
             (let [result (original dispatcher)]
               (when (compare-and-set! first-check? true false)
                 (deliver checked :open)
                 (.await resume))
               result))}
          (fn []
            (let [attempt (future
                            (try
                              (dispatch/submit! d {:coalesce-key :after-close
                                                   :run (fn [] :orphan)})
                              (catch clojure.lang.ExceptionInfo e
                                (ex-message e))))]
              (is (= :open (promise-value checked)))
              (is (= :closed (dispatch/close! d)))
              (release resume)
              (is (= "gesso.live dispatcher is closed."
                     (deref attempt timeout-ms ::timed-out))
                  (str "closed dispatcher accepted submission under " policy))
              (is (zero? (.size ^java.util.concurrent.ArrayBlockingQueue
                                (:queue d)))))))
        (finally
          (release resume)
          (close d))))))

(deftest public-job-cannot-spoof-internal-coalesce-slot-test
  ;; Ordinary caller metadata must not be interpreted as an internal queue
  ;; slot. Previously this valid :run job killed a worker via (deref nil),
  ;; leaving subsequently submitted work stranded on that dispatcher.
  (doseq [policy [:throw :block :drop :coalesce]]
    (let [first-done (promise)
          second-done (promise)
          d (dispatch/create {:threads 1
                              :queue-size 4
                              :on-overflow policy})]
      (try
        (is (= :submitted
               (:status (dispatch/submit!
                         d {:dispatch/type :coalesce-slot
                            :coalesce-key :caller-metadata
                            :run #(deliver first-done :first-ran)})))
            (str policy " should accept a valid user job"))
        (is (= :first-ran (promise-value first-done))
            (str policy " must run caller job, not treat it as an internal slot"))
        (is (= :submitted
               (:status (dispatch/submit!
                         d {:coalesce-key :follow-up
                            :run #(deliver second-done :second-ran)}))))
        (is (= :second-ran (promise-value second-done))
            (str policy " worker must remain healthy"))
        (finally
          (close d))))))

(deftest worker-can-close-own-dispatcher-without-error-test
  ;; A legitimate job may initiate shutdown. close! must not interrupt or
  ;; join the current worker, otherwise the call raises InterruptedException
  ;; and is incorrectly recorded as an async job failure.
  (let [result (promise)
        d (dispatch/create {:threads 2 :queue-size 4})]
    (try
      (is (= :submitted
             (:status (dispatch/submit! d
                                        {:run #(deliver result (dispatch/close! d))}))))
      (is (= :closed (promise-value result))
          "worker-initiated close! should return normally")
      (is (dispatch/closed? d))
      (is (empty? (dispatch/errors d))
          "shutdown is not an async job failure")
      (finally
        (dispatch/close! d)))))


;; A drop callback is application code and may need the dispatcher itself or
;; another thread acquiring its lifecycle lock. Do not invoke it while that
;; lock is held: even a bounded wait can strand unrelated submissions.
(deftest coalesced-drop-hook-allows-concurrent-submission-test
  (let [started (CountDownLatch. 1)
        release-running (CountDownLatch. 1)
        hook-result (promise)
        d (dispatch/create {:threads 1 :queue-size 2 :on-overflow :coalesce})]
    (try
      (dispatch/submit! d {:coalesce-key :busy
                           :run (fn [] (release started)
                                  (await-latch release-running))})
      (is (await-latch started))
      (dispatch/submit! d {:coalesce-key :item
                           :run (fn [] :old)
                           :on-drop (fn [_]
                                      (let [attempt (future
                                                      (:status (dispatch/submit!
                                                                d {:coalesce-key :other
                                                                   :run (fn [] :other)})))]
                                        (deliver hook-result
                                                 (deref attempt timeout-ms ::timeout))))})
      (is (= :submitted (:status (dispatch/submit!
                                  d {:coalesce-key :item :run (fn [] :new)}))))
      (is (= :submitted (promise-value hook-result))
          "coalesced on-drop must not hold the dispatch lock while waiting on a submitter")
      (finally
        (release release-running)
        (dispatch/close! d)))))

(deftest overflow-drop-hook-allows-independent-queue-inspection-test
  (let [started (CountDownLatch. 1)
        release-running (CountDownLatch. 1)
        hook-result (promise)
        d (dispatch/create {:threads 1 :queue-size 1 :on-overflow :drop})]
    (try
      (dispatch/submit! d {:run (fn [] (release started)
                                    (await-latch release-running))})
      (is (await-latch started))
      (dispatch/submit! d {:run (fn [] :queued)})
      (is (= :dropped (:status
                      (dispatch/submit! d
                                        {:run (fn [] :overflow)
                                         :on-drop
                                         (fn [_]
                                           (let [attempt
                                                 (future
                                                   (locking (:coalesce-lock d)
                                                     :lock-available))]
                                             (deliver hook-result
                                                      (deref attempt timeout-ms ::timeout))))}))))
      (is (= :lock-available (promise-value hook-result))
          "overflow :on-drop must not hold the queue lifecycle lock")
      (finally
        (release release-running)
        (dispatch/close! d)))))

(deftest shutdown-drop-hook-allows-independent-queue-inspection-test
  (let [started (CountDownLatch. 1)
        release-running (CountDownLatch. 1)
        hook-result (promise)
        d (dispatch/create {:threads 1 :queue-size 1 :on-overflow :throw})]
    (try
      (dispatch/submit! d {:run (fn [] (release started)
                                    (await-latch release-running))})
      (is (await-latch started))
      (dispatch/submit! d {:run (fn [] :queued)
                           :on-drop (fn [_]
                                      (let [attempt
                                            (future
                                              (locking (:coalesce-lock d)
                                                :lock-available))]
                                        (deliver hook-result
                                                 (deref attempt timeout-ms ::timeout))))})
      (is (= :closed (dispatch/close! d)))
      (is (= :lock-available (promise-value hook-result))
          "shutdown :on-drop must run after queue and pending-index retirement")
      (is (= 0 (.size ^java.util.concurrent.ArrayBlockingQueue (:queue d))))
      (finally
        (release release-running)
        (dispatch/close! d)))))

(deftest shutdown-interrupts-workers-before-drop-hooks-test
  ;; Dropped-job callbacks are external application code. A callback may wait
  ;; for a running worker to terminate before releasing its own resources.
  ;; Shutdown must request the worker stop before invoking that callback.
  (let [started (CountDownLatch. 1)
        release-running (CountDownLatch. 1)
        worker-stopped (promise)
        observed-in-hook (promise)
        d (dispatch/create {:threads 1 :queue-size 1 :on-overflow :throw})]
    (try
      (dispatch/submit!
       d {:run (fn []
                  (release started)
                  (try
                    (.await release-running)
                    (catch InterruptedException _
                      nil)
                    (finally
                      (deliver worker-stopped :stopped))))})
      (is (await-latch started))
      (dispatch/submit!
       d {:run (fn [] :queued)
          :on-drop (fn [_]
                     (deliver observed-in-hook
                              (deref worker-stopped timeout-ms ::timeout)))})
      (is (= :closed (dispatch/close! d)))
      (is (= :stopped (promise-value observed-in-hook))
          "shutdown must interrupt the runner before invoking drop hooks")
      (is (= :stopped (promise-value worker-stopped)))
      (finally
        (release release-running)
        (dispatch/close! d)))))

(deftest close-drops-dequeued-but-unstarted-work-test
  ;; A worker can take a job from the queue and pause before invoking its
  ;; runner. Once close! wins, that still-unstarted job must be retired and
  ;; notified exactly once, including a coalesce slot removed from the queue.
  (doseq [policy [:throw :block :drop :coalesce]]
    (let [dequeued (CountDownLatch. 1)
          resume? (atom false)
          ran (promise)
          dropped (promise)
          original (var-get #'gesso.live.dispatch/run-queue-item!)]
      (with-redefs-fn
        {#'gesso.live.dispatch/run-queue-item!
         (fn [dispatcher item]
           (release dequeued)
           ;; Interrupting the worker must not accidentally release this
           ;; deterministic pre-execution barrier.
           (while (not @resume?)
             (Thread/yield))
           (original dispatcher item))}
        (fn []
          (let [d (dispatch/create {:threads 1
                                    :queue-size 2
                                    :on-overflow policy})]
            (try
              (is (= :submitted
                     (:status (dispatch/submit!
                               d {:coalesce-key :pending
                                  :run #(deliver ran :ran)
                                  :on-drop #(deliver dropped %)})))
                  (str policy " submitted before shutdown"))
              (is (await-latch dequeued)
                  (str policy " worker dequeued before shutdown"))
              (let [shutdown (future (dispatch/close! d))]
                (try
                  (is (loop [attempts 500]
                        (cond
                          (and (dispatch/closed? d)
                               (zero? (:pending-keys (dispatch/stats d)))) true
                          (zero? attempts) false
                          :else (do (Thread/sleep 2)
                                    (recur (dec attempts)))))
                      (str policy " queue retirement completes"))
                  (finally
                    (reset! resume? true)))
                (is (= :closed (deref shutdown 2500 ::timeout))
                    (str policy " shutdown completes")))
              (is (= :dispatcher-closed (:reason (promise-value dropped)))
                  (str policy " unstarted job receives shutdown drop"))
              (is (not (realized? ran))
                  (str policy " unstarted job must never execute after close"))
              (finally
                (reset! resume? true)
                (dispatch/close! d)))))))))
