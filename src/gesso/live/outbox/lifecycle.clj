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
            [gesso.live.outbox.cycle :as cycle]
            [gesso.live.outbox.reconciliation :as reconciliation])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(def ^:private allowed-options
  #{:worker-id :lease-millis :max-items :poll-ms})
(def ^:private default-lease-millis 60000)
(def ^:private default-max-items 32)
(def ^:private default-poll-ms 250)

(defrecord Lifecycle [state stop-signal done thread node expected-source-id
                      system ctx restart-claimed successor retired])

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

(defn inspect-failure!
  "Read-only physical observation of one stopped failed lifecycle's cycle debt.

   Bind the read to the *original* XTDB provider and expected Live source of
   this managed worker, not independently supplied source/lease credentials.
   The scheduler must have exited its failed cycle before a recovery read is
   allowed. A generic lifecycle failure is NOT an outbox cycle-interruption
   report and cannot be interpreted as retained acquisition/dispatch debt.

   The result of reconciliation/inspect! remains time-sensitive diagnostic
   evidence only: it does not resume the lifecycle, grant lease authority,
   replay a send, certify subscriber delivery or establish safe worker
   restart. The owner still controls physical provider teardown and any
   subsequent independently qualified recovery procedure."
  [handle]
  (require-handle! handle)
  (let [snapshot (status handle)]
    (when-not (= :failed (:status snapshot))
      (reject! :not-failed-lifecycle {:status (:status snapshot)}))
    (when-not (zero? (.getCount ^CountDownLatch (.-done ^Lifecycle handle)))
      (reject! :failed-worker-not-finished {}))
    (when-not (= :gesso.live.outbox/cycle-interrupted
                 (get-in snapshot [:failure :error/type]))
      (reject! :not-cycle-interruption
               {:failure-type (get-in snapshot [:failure :error/type])}))
    (reconciliation/inspect! (.-node ^Lifecycle handle)
                             (.-expected-source-id ^Lifecycle handle)
                             (:failure snapshot))))

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

(defn- advancing-cycle-ids?
  "Cycle result identities must follow XTDB's unsigned UUID keyset order."
  [after results]
  (let [ids (mapv :intent-id results)
        ordered (cond->> ids after (cons after))]
    (and (every? uuid? ids)
         (every? (fn [[left right]]
                   (neg? (compare (str left) (str right))))
                 (partition 2 1 ordered)))))

(defn- require-cycle-result! [result requested-worker-id after max-items]
  ;; The lifecycle publishes totals and resumes keyset pagination based on
  ;; this result. A malformed successful cycle cannot certify progress or
  ;; advance the cursor merely because it reports :cycle-completed.
  (let [results (:results result)
        n (when (vector? results) (count results))
        counts (when (vector? results)
                 (frequencies (map :acquisition-status results)))
        dispatch-count (when (vector? results)
                         (count (filter #(= :dispatch-accepted (:status %))
                                        results)))
        valid-entry? (fn [item]
                       (and (map? item)
                            (case (:acquisition-status item)
                              (:acquired :reclaimed)
                              (and (= :dispatch-accepted (:status item))
                                   ;; Completion may update counters and the
                                   ;; scheduler cursor only for THIS worker.
                                   (= requested-worker-id (:worker-id item))
                                   (uuid? (:lease-id item))
                                   (uuid? (:receipt-id item))
                                   (some? (:progression item)))
                              (:contended :unavailable)
                              (nil? (:status item))
                              false)))]
    (when-not
     (and (map? result)
          (= :cycle-completed (:status result))
          (= requested-worker-id (:worker-id result))
          (vector? results)
          (<= n max-items)
          (= n (:processed result))
          (every? valid-entry? results)
          (advancing-cycle-ids? after results)
          (= dispatch-count (:dispatch-accepted result))
          (= (get counts :acquired 0) (:acquired result))
          (= (get counts :reclaimed 0) (:reclaimed result))
          (= (get counts :contended 0) (:contended result))
          (= (get counts :unavailable 0) (:unavailable result))
          (#{:observed-end :page-budget} (:halt-reason result))
          (if (= :page-budget (:halt-reason result))
            (and (pos? n)
                 (= (-> results peek :intent-id) (:next-after result)))
            (nil? (:next-after result))))
      (reject! :invalid-cycle-result {:cycle-status (:status result)}))
    result))

(def ^:private ^ThreadLocal worker-origin (ThreadLocal.))
(def ^:private original-process-cycle cycle/process-cycle!)

(defn current-worker-origin
  "Observe a synchronous scheduled cycle for the exact connectable and
   configured source/worker names, on its own scheduler thread. The
   lifecycle reference is process-local; it is not a credential. This
   describes the active cycle, not the origin of an individual write,
   delivery, installed host, or independently authenticated worker."
  [node source-id worker-id]
  (let [scope (.get worker-origin)]
    (when (and scope
               (identical? original-process-cycle cycle/process-cycle!)
               (identical? node (:node scope))
               (= source-id (:source-id scope))
               (= worker-id (:worker-id scope)))
      (dissoc scope :node))))

(defn- process-scheduled-cycle!
  [node system ctx source-id opts lifecycle-ref cycle-number]
  (let [prior (.get worker-origin)
        process! cycle/process-cycle!]
    (.remove worker-origin)
    (try
      (when (identical? original-process-cycle process!)
        (.set worker-origin {:node node :source-id source-id
                             :worker-id (:worker-id opts)
                             :lifecycle-ref lifecycle-ref :cycle-number cycle-number
                             :origin :scheduled-local-cycle
                             :authentication :not-established}))
      (process! node system ctx source-id opts)
      (finally
        (if prior (.set worker-origin prior) (.remove worker-origin))))))

(defn- run-loop! [node system ctx expected-source-id opts state stop-signal done lifecycle-ref]
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
                        (process-scheduled-cycle! node system ctx expected-source-id cycle-opts
                                                  lifecycle-ref (inc (:cycles @state)))
                        (:worker-id opts) cursor (:max-items cycle-opts))
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
        lifecycle-ref (random-uuid)
        stop-signal (CountDownLatch. 1)
        done (CountDownLatch. 1)
        thread (Thread. ^Runnable
                        (fn [] (run-loop! node system ctx expected-source-id
                                          opts state stop-signal done lifecycle-ref))
                        (str "gesso-live-outbox-" (:worker-id opts)))
        handle (->Lifecycle state stop-signal done thread
                            node expected-source-id system ctx
                            (atom false) (atom nil) (atom false))]
    (.setDaemon thread true)
    (.start thread)
    handle))

(defn- stop-owned!
  [handle timeout-ms]
  (require-handle! handle)
  (when-not (and (integer? timeout-ms) (<= 0 timeout-ms 120000))
    (reject! :invalid-stop-timeout {:timeout-ms timeout-ms}))
  (when (identical? (Thread/currentThread) (.-thread ^Lifecycle handle))
    (reject! :self-stop {}))
  ;; The predecessor is also the lifetime owner of any explicitly restarted
  ;; worker. Holding this lock while finding and draining the successor
  ;; excludes a concurrent restart that could otherwise escape host teardown.
  (locking handle
    (swap! (.-state ^Lifecycle handle) assoc :stop-requested? true)
    (.countDown ^CountDownLatch (.-stop-signal ^Lifecycle handle))
    (let [finished? (try
                      (.await ^CountDownLatch (.-done ^Lifecycle handle)
                              (long timeout-ms) TimeUnit/MILLISECONDS)
                      (catch InterruptedException error
                        (.interrupt (Thread/currentThread))
                        (throw error)))
          snapshot (status handle)
          own-outcome (if (= :failed (:status snapshot)) :failed :drained)]
      (if-not finished?
        {:outcome :timed-out :snapshot snapshot}
        (if-let [child @(.-successor ^Lifecycle handle)]
          (let [child-result (stop-owned! child timeout-ms)]
            (if (= :timed-out (:outcome child-result))
              {:outcome :timed-out :snapshot snapshot
               :successor-outcome :timed-out}
              {:outcome own-outcome :snapshot snapshot
               :successor-outcome (:outcome child-result)}))
          {:outcome own-outcome :snapshot snapshot})))))

(defn stop!
  "Request cooperative shutdown and await ALL descendants created by an
   explicit restart. A timeout on any successor is a timeout for the original
   owner too: the host must not close shared XTDB/Live dependencies.

   This is a drain, not permanent retirement. A previously failed, stopped
   lifecycle can still be explicitly restarted afterwards. An owner closing
   providers MUST call retire! instead, which atomically prevents later
   restarts as well as draining the entire restart lineage."
  ([handle] (stop! handle 5000))
  ([handle timeout-ms]
   (stop-owned! handle timeout-ms)))

(defn- retire-chain!
  [handle]
  (locking handle
    (reset! (.-retired ^Lifecycle handle) true)
    (when-let [child @(.-successor ^Lifecycle handle)]
      (retire-chain! child))))

(defn retire!
  "Permanently retire a managed lifecycle AND every restart descendant
   before their provider can close. New successors are rejected once any
   relevant handle is retired. Return :timed-out while ANY descendant still
   runs; callers must keep dependencies open and retry retiring the SAME
   original handle. After successful retirement, no successor can escape.

   This is the correct stop boundary for a Biff-managed lifecycle. Ordinary
   stop! remains a non-retiring join for explicit recovery operators."
  ([handle] (retire! handle 5000))
  ([handle timeout-ms]
   (require-handle! handle)
   (when-not (and (integer? timeout-ms) (<= 0 timeout-ms 120000))
     (reject! :invalid-stop-timeout {:timeout-ms timeout-ms}))
   (when (identical? (Thread/currentThread) (.-thread ^Lifecycle handle))
     (reject! :self-stop {}))
   (locking handle
     (retire-chain! handle)
     (stop-owned! handle timeout-ms))))

(defn- prepare-restart!
  "Inspect the original failed lifecycle before a deliberate successor starts.
   Require every reported completed dispatch to retain its own physical
   acknowledgment. Contended/unavailable prefix entries are not sends."
  [handle opts]
  (require-handle! handle)
  (validate-worker-options! opts)
  (let [snapshot (status handle)]
    (when (= (:worker-id snapshot) (:worker-id opts))
      (reject! :reused-worker-id {:worker-id (:worker-id opts)})))
  (when @(.-retired ^Lifecycle handle)
    (reject! :retired-lifecycle {}))
  (when @(.-restart-claimed ^Lifecycle handle)
    (reject! :already-restarted {}))
  (let [observed (inspect-failure! handle)
        completed (:completed-observations observed)]
    (when-not (and (vector? completed)
                   (every? #(#{:matching-acknowledgment-recorded
                                :no-dispatch-claim} (:status %)) completed))
      (reject! :recovery-not-qualified
               {:in-flight-status (:status observed)
                :completed-statuses (when (vector? completed)
                                      (mapv :status completed))}))
    observed))

(defn- start-reconciled! [handle opts observed recovery-mode]
  ;; Serialize successor creation with host teardown. Biff retirement takes
  ;; the SAME monitor, so a successor cannot be created after retirement or
  ;; escape between start! and the owner's successor registration.
  (locking handle
    (when @(.-retired ^Lifecycle handle)
      (reject! :retired-lifecycle {}))
    (when-not (compare-and-set! (.-restart-claimed ^Lifecycle handle) false true)
      (reject! :already-restarted {}))
    ;; The reservation stays sticky if thread construction/start fails. Such
    ;; a failure is not evidence that no worker escaped to the host.
    (let [child (start! (.-node ^Lifecycle handle)
                        (.-system ^Lifecycle handle)
                        (.-ctx ^Lifecycle handle)
                        (.-expected-source-id ^Lifecycle handle)
                        opts)]
      (reset! (.-successor ^Lifecycle handle) child)
      {:handle child
       :recovery-mode recovery-mode
       :reconciliation observed})))

(defn restart-recorded!
  "Explicitly restart a halted worker only when the failed in-flight
   acknowledgment AND all previously completed dispatch acknowledgments are
   still physically recorded with matching lease and receipt identities.

   The successor uses the original provider, source and context, a NEW worker
   ID and a fresh full discovery scan. Restart is one-shot even under races.
   No external send, automatic retry, or browser-delivery proof is inferred.
   The returned handle must be drained before shared provider teardown."
  [handle opts]
  (let [observed (prepare-restart! handle opts)]
    (when-not (= :matching-acknowledgment-recorded (:status observed))
      (reject! :recovery-not-qualified
               {:in-flight-status (:status observed)
                :completed-statuses (mapv :status
                                          (:completed-observations observed))}))
    (start-reconciled! handle opts observed :physically-recorded)))

(defn restart-after-acquisition!
  "Explicit recovery when a cycle stopped BEFORE dispatching its next intent.

   Only :acquisition/:acquisition-exception failures qualify. The interrupted
   cycle cannot have called its delivery adapter for this in-flight attempt;
   a physical lease might nevertheless have committed before its response
   was lost. Prior completed dispatches must still have matching physical
   worker/lease/receipt evidence. If a reported lease exists, it must come
   from the failed worker's verified acquisition-attempt or rejected-page
   reporting boundary, never from a delivery/receipt claim.

   A replacement uses the same trusted provider/source/context but starts a
   NEW full source-scoped scan with a new worker ID. Pending intents can be
   acquired; unacknowledged physical leases become eligible only after XTDB's
   transaction-time expiry and guarded reclaim. This operation NEVER directly
   dispatches an unverified lease and NEVER assumes that acquisition committed.
   It is an explicit host action, not a generic retry of uncertain sends.
   Like all restarts it owns a single successor through the original handle."
  [handle opts]
  (let [observed (prepare-restart! handle opts)
        failure (:failure (status handle))
        claim (:in-flight failure)
        original-id (:worker-id (status handle))
        acquisition-evidence?
        (or (nil? claim)
            (and (= original-id (:worker-id claim))
                 (not (contains? claim :receipt-id))
                 (not (contains? claim :receipt-evidence))
                 (or (nil? (:source-id claim))
                     (= (:source-id (status handle)) (:source-id claim)))
                 (case (:evidence claim)
                   :unverified-acquisition-attempt
                   (#{:acquire-pending :reclaim-expired}
                    (:acquisition-action claim))
                   :unverified-worker-page
                   (#{:acquired :reclaimed} (:acquisition-status claim))
                   false)))]
    (when-not (and (= :acquisition (:phase failure))
                   (= :acquisition-exception (:reason failure))
                   acquisition-evidence?)
      (reject! :recovery-not-qualified
               {:phase (:phase failure)
                :reason (:reason failure)
                :in-flight-status (:status observed)
                :acquisition-evidence? (boolean acquisition-evidence?)}))
    (start-reconciled! handle opts observed :acquisition-rescan)))

(defn restart-with-redelivery!
  "Explicit at-least-once recovery after an uncertain advisory Live send.

   This is NOT a generic retry of failed operations. It is permitted only for
   an exited, failed outbox DELIVERY/ACKNOWLEDGMENT cycle where a physically
   leased in-flight intent still has the EXACT original lease and worker,
   and all prior completed dispatches retain matching XTDB acknowledgments.
   A source-emission failure may have sent zero, one, or many notifications:
   the new worker may issue a DUPLICATE invalidation after lease expiry.

   Only outbox refresh notifications are eligible for replay by the existing
   worker's source/purpose checks; they must be advisory/idempotent, with
   authoritative reads reacquired by consumers. The successor starts from a
   NEW full source-scoped scan and may wait for the old lease to expire. XTDB
   discovery and guarded transaction-time CAS (not this non-atomic inspection)
   decide whether it can actually reclaim and send. Competing workers and
   unknown source sends remain possible; no exactly-once or browser receipt
   is claimed. This operation requires explicit host invocation, never a
   silent retry, and reserves the failed handle against a second successor."
  [handle opts]
  (let [observed (prepare-restart! handle opts)
        failure (:failure (status handle))
        in-flight (:in-flight failure)]
    (when-not (and (= :same-lease-active (:status observed))
                   (= :leased (:observed-status observed))
                   (#{:delivery :acknowledgment} (:phase failure))
                   (#{:acquired :reclaimed}
                    (:acquisition-status in-flight)))
      (reject! :recovery-not-qualified
               {:in-flight-status (:status observed)
                :phase (:phase failure)
                :completed-statuses (mapv :status
                                          (:completed-observations observed))}))
    (start-reconciled! handle opts observed :at-least-once-invalidation)))
