(ns gesso.live.outbox.lifecycle
  "Explicit single-node scheduler for bounded outbox processing cycles.

   Hosts must opt in with start!, retain the returned handle, and call stop!
   BEFORE closing XTDB or the Live source. Exactly one daemon thread runs
   cycles sequentially. A shutdown request wakes idle polling and prevents
   *future cycles*, but never interrupts a running cycle (and therefore never
   interrupts its physical write or external Live emission).

   A failed or uncertain cycle HALTS scheduling. status exposes the last
   completed cycle, in-flight lease/partial work on failure, and an explicit
   requirement to reconcile XTDB before restarting. No automatic retry is
   authorized after indeterminate delivery or acknowledgment.

   This is an opt-in in-process lifecycle, not installed Biff startup, a
   process-restart durability demonstration, or SSE/browser convergence.
   Discovery and acquisition enforce the installed source before acquiring
   its intents. Source identity still originates in the trusted host and is
   not an independent proof of writer authorization.
   External sends can race with reclaimed leases; delivery is at least once
   compatible, not exactly once. No global cross-process leader is created."
  (:require [clojure.string :as str]
            [gesso.live.outbox.cycle :as cycle])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(def ^:private allowed-options
  #{:worker-id :lease-millis :max-items :poll-ms})
(def ^:private default-lease-millis 60000)
(def ^:private default-max-items 32)
(def ^:private default-poll-ms 250)

(defrecord Lifecycle [state stop-signal done thread])

(defn- reject! [reason data]
  (throw (ex-info "Invalid Live outbox lifecycle."
                  (assoc data :error/type :gesso.live.outbox/invalid-lifecycle
                         :reason reason))))

(defn validate-worker-options!
  "Validate trusted worker scheduling settings without creating resources.

   Biff startup can invoke this before starting any provider modules, so an
   invalid worker option cannot leak an already-started XTDB/Live dependency
   merely because Biff does not unwind modules when a later start throws."
  [opts]
  (when-not (map? opts)
    (reject! :invalid-options {:options opts}))
  (when-let [other (seq (remove allowed-options (keys opts)))]
    (reject! :unknown-options {:fields (vec other)}))
  (when-not (and (string? (:worker-id opts))
                 (not (str/blank? (:worker-id opts))))
    (reject! :invalid-worker-id {:worker-id (:worker-id opts)}))
  (let [lease (get opts :lease-millis default-lease-millis)
        max-items (get opts :max-items default-max-items)
        poll (get opts :poll-ms default-poll-ms)]
    (when-not (and (integer? lease) (<= 1 lease 300000))
      (reject! :invalid-lease-millis {:lease-millis lease}))
    (when-not (and (integer? max-items) (<= 1 max-items 128))
      (reject! :invalid-max-items {:max-items max-items}))
    (when-not (and (integer? poll) (<= 10 poll 60000))
      (reject! :invalid-poll-ms {:poll-ms poll})))
  opts)

(defn- validate! [system ctx expected-source-id opts]
  (validate-worker-options! opts)
  (when-not (and (string? expected-source-id)
                 (not (str/blank? expected-source-id)))
    (reject! :invalid-source-id {:source-id expected-source-id}))
  (when-not (and (map? system) (some? (:source system)))
    (reject! :invalid-system {}))
  (when-not (map? ctx)
    (reject! :invalid-context {}))
  opts)

(defn- require-handle! [handle]
  (when-not (instance? Lifecycle handle)
    (reject! :invalid-handle {}))
  handle)

(defn status
  "Thread-safe immutable snapshot of one lifecycle's observed state.

   :failed includes a bounded diagnostic envelope with the completed results
   and in-flight lease data supplied by process-cycle!, when available. The
   envelope is an observation, NOT authority to replay external effects.
   A :stopping worker may still be inside a running physical operation."
  [handle]
  @(.-state ^Lifecycle (require-handle! handle)))

(defn- failure-report [error]
  (let [data (ex-data error)]
    (merge {:exception-class (.getName (class error))
            :message (.getMessage ^Throwable error)
            :error/type (or (:error/type data) :gesso.live.outbox/unexpected-worker-failure)
            :rescan-required? true}
           (select-keys data
                        [:phase :reason :completed :completed-count
                         :last-completed-cursor :in-flight
                         :source-acceptance :original-error/type
                         :delivery-status :acknowledgment-status]))))

(defn- require-cycle-result! [result]
  (when-not (and (map? result)
                 (= :cycle-completed (:status result))
                 (integer? (:processed result))
                 (<= 0 (:processed result) 128)
                 (integer? (:dispatch-accepted result))
                 (<= 0 (:dispatch-accepted result) (:processed result))
                 (#{:observed-end :page-budget} (:halt-reason result))
                 (if (= :page-budget (:halt-reason result))
                   (uuid? (:next-after result))
                   (nil? (:next-after result))))
    (reject! :invalid-cycle-result {:cycle-status (:status result)}))
  result)

(defn- run-loop! [node system ctx expected-source-id opts state stop-signal done]
  (try
    (loop [cursor nil]
      (if (zero? (.getCount ^CountDownLatch stop-signal))
        (swap! state assoc :status :stopped :in-cycle? false :cursor nil)
        (do
          (swap! state assoc :status :running :in-cycle? true :cursor cursor)
          (let [cycle-opts (cond-> {:worker-id (:worker-id opts)
                                    :lease-millis (get opts :lease-millis default-lease-millis)
                                    :max-items (get opts :max-items default-max-items)}
                             cursor (assoc :after cursor))
                result (require-cycle-result!
                        (cycle/process-cycle! node system ctx expected-source-id cycle-opts))
                next-cursor (when (= :page-budget (:halt-reason result))
                              (:next-after result))]
            (swap! state (fn [s]
                           (-> s
                               (assoc :status :idle :in-cycle? false
                                      :cursor next-cursor :last-cycle result)
                               (update :cycles inc)
                               (update :total-processed + (:processed result))
                               (update :total-dispatch-accepted
                                       + (:dispatch-accepted result)))))
            ;; Cooperative stop wakes the idle delay without interrupting a
            ;; running transaction. Resume the same scan only when the
            ;; previous cycle ended at a page budget, never after a failure.
            (.await ^CountDownLatch stop-signal
                    (long (get opts :poll-ms default-poll-ms))
                    TimeUnit/MILLISECONDS)
            (recur next-cursor)))))
    (catch Throwable error
      ;; Never make an unknown post-dispatch result look like a safe retry.
      (swap! state assoc
             :status :failed :in-cycle? false :cursor nil
             :failure (failure-report error)))
    (finally
      (.countDown ^CountDownLatch done))))

(defn start!
  "Start one explicitly owned polling lifecycle (one daemon thread).

   Options: {:worker-id nonblank-string, :lease-millis 1..300000,
   :max-items 1..128, :poll-ms 10..60000}. Defaults are 60 seconds,
   32 items per processing cycle and 250ms delay between cycles. The first
   cycle starts without waiting. A page-budget cursor continues into the
   next cycle; when a sweep reaches the observed end, it resets to the
   beginning so new work behind a cursor is eventually discovered.

   The caller owns node, Live system and ctx. Do not close those resources
   until stop! reports :drained. A failed lifecycle never silently retries.
   Starting a new worker after failure requires independent XTDB reconciliation
   and explicit host action. Multiple start! calls create independent workers;
   each one's processing is sequential, but no global singleton is asserted."
  [node system ctx expected-source-id opts]
  (validate! system ctx expected-source-id opts)
  (let [state (atom {:status :starting
                     :worker-id (:worker-id opts)
                     :source-id expected-source-id
                     :stop-requested? false
                     :in-cycle? false
                     :cursor nil
                     :cycles 0
                     :total-processed 0
                     :total-dispatch-accepted 0
                     :last-cycle nil
                     :failure nil})
        stop-signal (CountDownLatch. 1)
        done (CountDownLatch. 1)
        thread (Thread. ^Runnable
                        (fn [] (run-loop! node system ctx expected-source-id
                                          opts state stop-signal done))
                        (str "gesso-live-outbox-" (:worker-id opts)))
        handle (->Lifecycle state stop-signal done thread)]
    (.setDaemon thread true)
    (.start thread)
    handle))

(defn stop!
  "Request cooperative shutdown; optionally await one current cycle.

   Returns {:outcome :drained|:failed|:timed-out :snapshot ...}. A timed-out
   result explicitly means the worker may still hold a lease or be sending;
   the host MUST keep XTDB and Live open and try stop! again before teardown.
   No interruption or forced cancellation of in-flight work is performed.
   Repeated stop! calls are safe; failure status remains visible after stop.

   Timeout in milliseconds must be between 0 and 120000 (default 5000).
   Calling from the worker thread itself is rejected to avoid self-wait."
  ([handle] (stop! handle 5000))
  ([handle timeout-ms]
   (require-handle! handle)
   (when-not (and (integer? timeout-ms) (<= 0 timeout-ms 120000))
     (reject! :invalid-stop-timeout {:timeout-ms timeout-ms}))
   (when (identical? (Thread/currentThread) (.-thread ^Lifecycle handle))
     (reject! :self-stop {}))
   (swap! (.-state ^Lifecycle handle) assoc :stop-requested? true)
   (.countDown ^CountDownLatch (.-stop-signal ^Lifecycle handle))
   (let [finished? (try
                     (.await ^CountDownLatch (.-done ^Lifecycle handle)
                             (long timeout-ms) TimeUnit/MILLISECONDS)
                     (catch InterruptedException error
                       ;; The cooperative stop request is already committed.
                       ;; A caller-side interrupted wait cannot establish
                       ;; drain, and must retain JVM cancellation for the
                       ;; owner to retry with this same lifecycle handle.
                       (.interrupt (Thread/currentThread))
                       (throw error)))
         snapshot (status handle)]
     {:outcome (cond
                 (not finished?) :timed-out
                 (= :failed (:status snapshot)) :failed
                 :else :drained)
      :snapshot snapshot})))
