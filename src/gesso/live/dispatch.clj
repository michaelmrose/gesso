(ns gesso.live.dispatch
  "Bounded dispatch for gesso.live expansion work.

   This namespace owns the async boundary for potentially slow expansion jobs.

   It does not know about:
   - invalidation rules
   - sources
   - SSE
   - HTMX
   - XTDB
   - Missionary

   It only runs supplied jobs either synchronously or on a bounded worker pool."
  (:require
   [gesso.live.schema :as schema])
  (:import
   [java.util.concurrent ArrayBlockingQueue]
   [java.util.concurrent.atomic AtomicLong]))

;; -----------------------------------------------------------------------------
;; Defaults
;; -----------------------------------------------------------------------------

(def default-options
  {:name "gesso-live-dispatch"
   :threads 4
   :queue-size 1024
   :on-overflow :throw
   :on-error nil})

(def ^:private close-timeout-ms
  1000)

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn- ex
  [message data]
  (ex-info message data))

(defn- opts
  [options]
  (merge default-options options))

(defn- validate-options!
  [options]
  (schema/validate! :gesso.live/dispatcher-options
                    (select-keys options
                                 [:name :threads :queue-size :on-overflow]))
  (when-let [on-error (:on-error options)]
    (when-not (fn? on-error)
      (throw
       (ex "gesso.live dispatcher :on-error must be a function."
           {:on-error on-error}))))
  options)

(defn- now-ms
  []
  (System/currentTimeMillis))

(defn- call-safely
  [f & args]
  (when f
    (try
      (apply f args)
      (catch Exception _
        nil))))

(defn- normalize-job
  [job id]
  (let [job' (cond
               (fn? job)
               {:run job}

               (and (map? job) (fn? (:run job)))
               job

               :else
               (throw
                (ex "gesso.live dispatch job must be a function or a map with :run."
                    {:job job})))]
    (assoc job'
           :dispatch/job-id id
           :dispatch/enqueued-at (now-ms))))

(defn- closed-error
  [dispatcher]
  (ex "gesso.live dispatcher is closed."
      {:dispatcher/id (:id dispatcher)
       :name (:name dispatcher)}))

(defn- queue-full-error
  [dispatcher job]
  (ex "gesso.live dispatcher queue is full."
      {:dispatcher/id (:id dispatcher)
       :name (:name dispatcher)
       :on-overflow (:on-overflow dispatcher)
       :queue-size (.size ^ArrayBlockingQueue (:queue dispatcher))
       :queue-capacity (:queue-size dispatcher)
       :job-id (:dispatch/job-id job)
       :coalesce-key (:coalesce-key job)}))

(defn- ensure-open!
  [dispatcher]
  (when @(:closed? dispatcher)
    (throw (closed-error dispatcher)))
  dispatcher)

;; -----------------------------------------------------------------------------
;; Error/drop recording
;; -----------------------------------------------------------------------------

(defn errors
  "Return a vector of async job errors recorded by dispatcher."
  [dispatcher]
  @(:errors dispatcher))

(defn clear-errors!
  "Clear recorded async job errors."
  [dispatcher]
  (reset! (:errors dispatcher) [])
  :cleared)

(defn- record-error!
  [dispatcher job error]
  (let [entry {:job-id (:dispatch/job-id job)
               :coalesce-key (:coalesce-key job)
               :error error
               :recorded-at (now-ms)}]
    (swap! (:errors dispatcher) conj entry)
    (call-safely (:on-error dispatcher) error entry)
    (call-safely (:on-error job) error entry)
    entry))

(defn- drop-info
  [job reason]
  {:job-id (:dispatch/job-id job)
   :coalesce-key (:coalesce-key job)
   :reason reason
   :dropped-at (now-ms)})

(defn- notify-drop!
  [job reason]
  (let [info (drop-info job reason)]
    (call-safely (:on-drop job) info)
    info))

;; -----------------------------------------------------------------------------
;; Coalesce slots
;; -----------------------------------------------------------------------------

(defn- coalesce-slot
  [key job]
  {:dispatch/type :coalesce-slot
   :coalesce-key key
   :job* (atom job)})

(defn- coalesce-slot?
  [x]
  (= :coalesce-slot (:dispatch/type x)))

(defn- slot-job
  [slot]
  @(:job* slot))

(defn- replace-slot-job!
  "Replace the latest job in a queued coalesce slot.

   Returns the old job so the caller can notify it as dropped. This does not
   touch the queue and therefore cannot lose both old and new jobs due to queue
   capacity races."
  [slot job]
  (let [job* (:job* slot)
        old  @job*]
    (reset! job* job)
    old))

(defn- remove-pending-slot!
  [dispatcher slot]
  (let [key (:coalesce-key slot)]
    (locking (:coalesce-lock dispatcher)
      (swap! (:pending-by-key dispatcher)
             (fn [pending]
               (if (identical? slot (get pending key))
                 (dissoc pending key)
                 pending))))))

;; -----------------------------------------------------------------------------
;; Worker loop
;; -----------------------------------------------------------------------------

(defn- remove-pending-job!
  "Compatibility cleanup for normal job maps.

   Coalesced jobs are represented by slots in pending-by-key, so normal jobs
   usually have nothing to remove. This function is still harmless for direct
   job maps and keeps run-job! robust."
  [dispatcher job]
  (when-let [k (:coalesce-key job)]
    (locking (:coalesce-lock dispatcher)
      (swap! (:pending-by-key dispatcher)
             (fn [pending]
               (let [entry (get pending k)]
                 (if (and (map? entry)
                          (not (coalesce-slot? entry))
                          (= (:dispatch/job-id job)
                             (:dispatch/job-id entry)))
                   (dissoc pending k)
                   pending)))))))

(defn- run-job!
  [dispatcher job]
  (remove-pending-job! dispatcher job)
  (try
    ((:run job))
    (catch Throwable e
      (record-error! dispatcher job e))))

(defn- run-coalesce-slot!
  [dispatcher slot]
  ;; Remove the slot from the pending index before reading/running its latest
  ;; job. After this point, later submissions with the same key create a fresh
  ;; slot rather than replacing the job that is about to run.
  (remove-pending-slot! dispatcher slot)
  (run-job! dispatcher (slot-job slot)))

(defn- run-queue-item!
  [dispatcher item]
  (if (coalesce-slot? item)
    (run-coalesce-slot! dispatcher item)
    (run-job! dispatcher item)))

(defn- worker-loop
  [dispatcher]
  (let [queue ^ArrayBlockingQueue (:queue dispatcher)]
    (loop []
      (when-not @(:closed? dispatcher)
        (let [item (try
                     (.take queue)
                     (catch InterruptedException _
                       nil))]
          (when item
            (run-queue-item! dispatcher item))
          (recur))))))

(defn- start-worker!
  [dispatcher n]
  (let [thread (Thread. ^Runnable #(worker-loop dispatcher)
                        (str (:name dispatcher) "-" n))]
    (.setDaemon thread true)
    (.start thread)
    thread))

;; -----------------------------------------------------------------------------
;; Creation/lifecycle
;; -----------------------------------------------------------------------------

(defn create
  "Create a bounded dispatcher.

   Options:
     :name         string, used for worker thread names
     :threads      positive integer
     :queue-size   positive integer
     :on-overflow  :throw | :block | :drop | :coalesce
     :on-error     optional dispatcher-level error hook

   Overflow policies:
     :throw
       Throw when the queue is full.

     :block
       Block the caller until queue space is available. This can block a web
       request thread if used there.

     :drop
       Drop the new job when the queue is full and call its :on-drop hook.

     :coalesce
       Requires jobs to provide :coalesce-key.

       The queue contains one coalesce slot per pending key. Submitting another
       job for the same key replaces the latest job in that slot without removing
       anything from the queue. This avoids the data-loss race caused by
       remove-old-then-offer-new logic.

       This only coalesces queued jobs. It does not cancel or replace a job that
       has already started running."
  ([] (create nil))
  ([options]
   (let [options'   (validate-options! (opts options))
         id         (random-uuid)
         queue      (ArrayBlockingQueue. (:queue-size options'))
         counter    (AtomicLong. 0)
         dispatcher {:id id
                     :name (:name options')
                     :threads (:threads options')
                     :queue-size (:queue-size options')
                     :on-overflow (:on-overflow options')
                     :on-error (:on-error options')
                     :queue queue
                     :counter counter
                     :closed? (atom false)
                     :errors (atom [])
                     ;; For :coalesce, this maps coalesce-key -> coalesce slot.
                     ;; A slot contains an atom holding the latest queued job.
                     :pending-by-key (atom {})
                     :coalesce-lock (Object.)}
         workers    (mapv #(start-worker! dispatcher %)
                           (range (:threads options')))]
     (assoc dispatcher :workers workers))))

(defn closed?
  "Return true if dispatcher has been closed."
  [dispatcher]
  (true? @(:closed? dispatcher)))

(defn- queued-item-job
  [item]
  (if (coalesce-slot? item)
    (slot-job item)
    item))

(defn- drain-queued-jobs!
  [dispatcher reason]
  (locking (:coalesce-lock dispatcher)
    (let [queue ^ArrayBlockingQueue (:queue dispatcher)]
      (loop [dropped []
             item (.poll queue)]
        (if item
          (let [job (queued-item-job item)]
            (recur (conj dropped (notify-drop! job reason))
                   (.poll queue)))
          (do
            (reset! (:pending-by-key dispatcher) {})
            dropped))))))

(defn close!
  "Close dispatcher, drop queued jobs, and interrupt worker threads.

   This is intentionally a shutdown operation, not a drain-until-complete
   operation. Jobs already running may finish, be interrupted, or record an error
   depending on what the job function does."
  [dispatcher]
  (when (compare-and-set! (:closed? dispatcher) false true)
    (drain-queued-jobs! dispatcher :dispatcher-closed)
    (doseq [^Thread worker (:workers dispatcher)]
      (.interrupt worker))
    (doseq [^Thread worker (:workers dispatcher)]
      (.join worker close-timeout-ms)))
  :closed)

(defn stats
  "Return a small dispatcher status map."
  [dispatcher]
  {:id (:id dispatcher)
   :name (:name dispatcher)
   :threads (:threads dispatcher)
   :queue-size (.size ^ArrayBlockingQueue (:queue dispatcher))
   :queue-capacity (:queue-size dispatcher)
   :pending-keys (count @(:pending-by-key dispatcher))
   :error-count (count @(:errors dispatcher))
   :closed? (closed? dispatcher)
   :on-overflow (:on-overflow dispatcher)})

;; -----------------------------------------------------------------------------
;; Enqueue policies
;; -----------------------------------------------------------------------------

(defn- next-job
  [dispatcher job]
  (normalize-job job (.incrementAndGet ^AtomicLong (:counter dispatcher))))

(defn- submitted
  [job]
  {:status :submitted
   :job-id (:dispatch/job-id job)
   :coalesce-key (:coalesce-key job)})

(defn- dropped
  [job reason]
  (notify-drop! job reason)
  {:status :dropped
   :reason reason
   :job-id (:dispatch/job-id job)
   :coalesce-key (:coalesce-key job)})

(defn- enqueue-throw!
  [dispatcher job]
  (let [queue ^ArrayBlockingQueue (:queue dispatcher)]
    (if (.offer queue job)
      (submitted job)
      (throw (queue-full-error dispatcher job)))))

(defn- enqueue-block!
  [dispatcher job]
  (let [queue ^ArrayBlockingQueue (:queue dispatcher)]
    (try
      (.put queue job)
      (submitted job)
      (catch InterruptedException e
        (.interrupt (Thread/currentThread))
        (throw
         (ex-info "Interrupted while enqueueing gesso.live dispatch job."
                  {:job-id (:dispatch/job-id job)}
                  e))))))

(defn- enqueue-drop!
  [dispatcher job]
  (let [queue ^ArrayBlockingQueue (:queue dispatcher)]
    (if (.offer queue job)
      (submitted job)
      (dropped job :queue-full))))

(defn- enqueue-new-coalesce-slot!
  [dispatcher key job]
  (let [queue ^ArrayBlockingQueue (:queue dispatcher)
        slot  (coalesce-slot key job)]
    (if (.offer queue slot)
      (do
        (swap! (:pending-by-key dispatcher) assoc key slot)
        {:status :submitted
         :job job
         :old-job nil})
      {:status :queue-full
       :job job
       :old-job nil})))

(defn- enqueue-coalesce!
  [dispatcher job]
  (let [key (:coalesce-key job)]
    (when-not key
      (throw
       (ex "Coalescing dispatch jobs require :coalesce-key."
           {:job-id (:dispatch/job-id job)
            :job job})))
    (let [{:keys [status old-job]}
          (locking (:coalesce-lock dispatcher)
            (if-let [slot (get @(:pending-by-key dispatcher) key)]
              {:status :submitted
               :job job
               :old-job (replace-slot-job! slot job)}
              (enqueue-new-coalesce-slot! dispatcher key job)))]
      (case status
        :submitted
        (do
          (when old-job
            (notify-drop! old-job :coalesced))
          (submitted job))

        :queue-full
        (throw (queue-full-error dispatcher job))))))

;; -----------------------------------------------------------------------------
;; Public submission API
;; -----------------------------------------------------------------------------

(defn submit!
  "Submit a job to dispatcher.

   Job may be:
     (fn [] ...)
     {:run fn
      :coalesce-key optional-key
      :on-error optional-fn
      :on-drop optional-fn}

   Returns:
     {:status :submitted ...}
     {:status :dropped ...}

   Throws if dispatcher is closed, the job is invalid, or overflow policy throws."
  [dispatcher job]
  (ensure-open! dispatcher)
  (let [job' (next-job dispatcher job)]
    (case (:on-overflow dispatcher)
      :throw
      (enqueue-throw! dispatcher job')

      :block
      (enqueue-block! dispatcher job')

      :drop
      (enqueue-drop! dispatcher job')

      :coalesce
      (enqueue-coalesce! dispatcher job')

      (throw
       (ex "Unsupported gesso.live dispatch overflow policy."
           {:on-overflow (:on-overflow dispatcher)})))))

(defn run-sync!
  "Run job immediately on the caller's thread.

   Job may be a function or a map with :run.

   Returns:
     {:status :ran
      :result ...}

   Unlike submit!, synchronous failures are thrown to the caller."
  [job]
  (let [job' (normalize-job job 0)]
    {:status :ran
     :result ((:run job'))}))

(defn dispatch!
  "Run or submit a job according to options.

   Options:
     :dispatch    :sync | :async
     :dispatcher  required for :async

   :sync runs on the caller thread.
   :async submits to the supplied dispatcher."
  [{:keys [dispatch dispatcher] :or {dispatch :sync}} job]
  (case dispatch
    :sync
    (run-sync! job)

    :async
    (if dispatcher
      (submit! dispatcher job)
      (throw
       (ex "Async gesso.live dispatch requires :dispatcher."
           {:dispatch dispatch
            :job job})))

    (throw
     (ex "Unsupported gesso.live dispatch mode."
         {:dispatch dispatch
          :job job}))))
