(ns gesso.live.outbox.history-scheduler
  "Single-threaded, paced history observation. Never interrupts a running
   audit on shutdown, and never queues catch-up work faster than completion."
  (:import [java.util.concurrent ScheduledThreadPoolExecutor ThreadFactory
            TimeUnit RejectedExecutionException]))

(defn valid-options? [opts]
  (and (map? opts)
       (= #{:catch-up-ms :idle-ms :failure-ms :max-failure-ms} (set (keys opts)))
       (every? #(and (integer? %) (<= 1 % 3600000)) (vals opts))
       (<= (:catch-up-ms opts) (:idle-ms opts))
       (<= (:failure-ms opts) (:max-failure-ms opts))))

(defn start!
  "Schedule one attempt at a time with delay measured after completion.
   Busy owner calls defer rather than count as audit failures. Exceptions
   use capped exponential backoff; interruption or fatal errors retire
   the scheduler. The caller owns the separate controller admission fence."
  [opts advance! open? initial-report]
  (when-not (valid-options? opts)
    (throw (ex-info "Invalid history schedule." {:reason :invalid-history-schedule})))
  (let [{:keys [catch-up-ms idle-ms failure-ms max-failure-ms]} opts
        executor (ScheduledThreadPoolExecutor.
                  1 (reify ThreadFactory
                      (newThread [_ runnable]
                        (doto (Thread. runnable "gesso-history-audit") (.setDaemon true)))))
        state (atom {:phase :waiting :consecutive-failures 0 :last-outcome :initial
                     :failure-delay-ms nil :next-delay-ms nil})
        delay-for (fn [report] (if (:caught-up? report) idle-ms catch-up-ms))]
    (.setExecuteExistingDelayedTasksAfterShutdownPolicy executor false)
    (.setRemoveOnCancelPolicy executor true)
    (letfn [(retire! [outcome]
              (swap! state assoc :phase :stopped :last-outcome outcome :next-delay-ms nil)
              (.shutdown executor))
            (enqueue! [delay]
              (when (and (open?) (not (.isShutdown executor)))
                (swap! state assoc :phase :waiting :next-delay-ms delay)
                (try
                  (.schedule executor ^Runnable (fn [] (run!)) (long delay) TimeUnit/MILLISECONDS)
                  (catch RejectedExecutionException e
                    (when-not (.isShutdown executor) (throw e))))))
            (run! []
              (when (open?)
                (swap! state assoc :phase :running :next-delay-ms nil)
                (let [delay (try
                              (let [report (advance!)]
                                (swap! state assoc :last-outcome :success :consecutive-failures 0
                                       :failure-delay-ms nil)
                                (delay-for report))
                              (catch InterruptedException _
                                (.interrupt (Thread/currentThread))
                                (retire! :interrupted)
                                nil)
                              (catch Exception e
                                (case (:reason (ex-data e))
                                  :history-controller-closed nil
                                  :history-controller-busy
                                  (do (swap! state assoc :last-outcome :busy) idle-ms)
                                  (let [delay (min max-failure-ms
                                                   (if-let [prior (:failure-delay-ms @state)]
                                                     (* 2 prior) failure-ms))]
                                    (swap! state #(-> % (assoc :last-outcome :failed :failure-delay-ms delay)
                                                      (update :consecutive-failures inc)))
                                    delay)))
                              (catch Error e
                                (retire! :fatal)
                                (throw e)))]
                  (when delay (enqueue! delay)))))]
      (try
        (enqueue! (delay-for initial-report))
        (catch Throwable e (.shutdown executor) (throw e)))
      {:status #(if (.isTerminated executor)
                  (assoc @state :phase :stopped :next-delay-ms nil) @state)
       :stop! (fn [timeout-nanos]
                (.shutdown executor)
                (try
                  (let [drained? (.awaitTermination executor (long timeout-nanos) TimeUnit/NANOSECONDS)]
                    (when drained? (swap! state assoc :phase :stopped :next-delay-ms nil))
                    drained?)
                  (catch InterruptedException e
                    (.interrupt (Thread/currentThread))
                    (throw e))))})))
