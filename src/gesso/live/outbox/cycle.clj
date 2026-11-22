(ns gesso.live.outbox.cycle
  "Bounded, synchronous discovery -> guarded acquisition -> Live source
   dispatch -> fenced acknowledgment for a single-node outbox.

   Process ONE candidate at a time, instead of leasing an entire batch
   before dispatching. An error stops the cycle immediately and carries
   completed records and any in-flight lease in typed exception data. The
   caller must reconcile physical XTDB state; an interrupted cycle's cursor
   is NOT safe to interpret as evidence that earlier work was delivered.

   This is an explicitly invoked processing cycle, not an installed loop,
   daemon, startup integration or restart-persistence test. The dispatcher
   reports source acceptance rather than SSE/browser delivery. Discovery
   and acquisition enforce the caller-installed source before leasing work.
   External sends can still race with lease reclamation; recovery and
   convergence obligations remain open."
  (:require [clojure.string :as str]
            [gesso.live.outbox.delivery :as delivery]
            [gesso.live.outbox.worker :as worker]))

(def ^:private allowed-options
  #{:worker-id :lease-millis :max-items :after})
(def ^:private default-lease-millis 60000)
(def ^:private default-max-items 32)

(defn- reject!
  [reason data]
  (throw (ex-info "Invalid Live outbox processing cycle."
                  (assoc data :error/type :gesso.live.outbox/invalid-cycle
                         :reason reason))))

(defn- validate!
  [system ctx expected-source-id opts]
  (when-not (map? opts)
    (reject! :invalid-options {:options opts}))
  (when-let [unknown (seq (remove allowed-options (keys opts)))]
    (reject! :unknown-options {:fields (vec unknown)}))
  (when-not (and (string? (:worker-id opts))
                 (not (str/blank? (:worker-id opts))))
    (reject! :invalid-worker-id {:worker-id (:worker-id opts)}))
  (when-not (and (string? expected-source-id)
                 (not (str/blank? expected-source-id)))
    (reject! :invalid-source-id {:source-id expected-source-id}))
  (when-not (and (map? system) (some? (:source system)))
    (reject! :invalid-system {}))
  (when-not (map? ctx)
    (reject! :invalid-context {}))
  (let [duration (get opts :lease-millis default-lease-millis)
        max-items (get opts :max-items default-max-items)]
    (when-not (and (integer? duration) (<= 1 duration 300000))
      (reject! :invalid-lease-millis {:lease-millis duration}))
    (when-not (and (integer? max-items) (<= 1 max-items 128))
      (reject! :invalid-max-items {:max-items max-items})))
  (when (and (contains? opts :after) (not (uuid? (:after opts))))
    (reject! :invalid-cursor {:after (:after opts)}))
  opts)

(defn- interrupted!
  [phase reason completed cursor in-flight details cause]
  ;; Acquisition or dispatch can throw after an interruptible JVM operation
  ;; has cleared its interrupt flag. The reconciliation envelope must retain
  ;; that cancellation signal without changing its cause or lease accounting.
  (when (instance? InterruptedException cause)
    (.interrupt (Thread/currentThread)))
  (throw
   (ex-info "Live outbox processing cycle interrupted; reconcile XTDB before retry."
            (merge {:error/type :gesso.live.outbox/cycle-interrupted
                    :phase phase
                    :reason reason
                    :completed completed
                    :completed-count (count completed)
                    :last-completed-cursor cursor
                    :in-flight in-flight
                    :rescan-required? true}
                   details)
            cause)))

(defn- validate-page!
  [page]
  ;; worker/acquire-sweep! is a trusted internal entry point, but a malformed
  ;; result must not allow an unaccounted acquisition to be treated as done.
  (when-not (and (map? page)
                 (vector? (:results page))
                 (= (count (:results page)) (:attempted page))
                 (<= (:attempted page) 1)
                 (#{:page-budget :observed-end} (:halt-reason page))
                 (if (= :page-budget (:halt-reason page))
                   (and (= 1 (:attempted page))
                        (uuid? (:next-after page)))
                   (nil? (:next-after page))))
    (reject! :invalid-worker-page {:page page}))
  page)

(defn- attempt-result!
  [node system ctx expected-source-id acquired completed cursor]
  (let [{:keys [intent-id lease-id status]} acquired
        in-flight {:intent-id intent-id
                   :lease-id lease-id
                   :worker-id (:worker-id acquired)
                   :acquisition-status status}]
    (cond
      (#{:contended :unavailable} status)
      {:intent-id intent-id :acquisition-status status
       :reason (:reason acquired)}

      (#{:acquired :reclaimed} status)
      (let [delivered (try
                        (delivery/dispatch-acquired!
                         node system ctx acquired expected-source-id)
                        (catch Throwable e
                          (interrupted! :delivery :delivery-exception
                                        completed cursor in-flight
                                        {:original-error/type (:error/type (ex-data e))
                                         :source-acceptance
                                         (if (= :gesso.live.outbox/post-dispatch-ack-unknown
                                                (:error/type (ex-data e)))
                                           :reported
                                           :unknown)}
                                        e)))]
        (when-not (= :dispatch-accepted (:status delivered))
          (interrupted! :acknowledgment :not-dispatch-accepted
                        completed cursor in-flight
                        {:source-acceptance
                         (if (= :dispatched-unacknowledged (:status delivered))
                           :reported :unknown)
                         :delivery-status (:status delivered)
                         :acknowledgment-status
                         (get-in delivered [:acknowledgment :status])}
                        nil))
        {:intent-id intent-id
         :acquisition-status status
         :status :dispatch-accepted
         :lease-id lease-id
         :receipt-id (:receipt-id delivered)
         :progression (:progression delivered)})

      :else
      (interrupted! :acquisition :unexpected-acquisition-status
                    completed cursor in-flight {:status status} nil))))

(defn- summary
  [worker-id results reason next-after]
  {:status :cycle-completed
   :worker-id worker-id
   :processed (count results)
   :acquired (count (filter #(= :acquired (:acquisition-status %)) results))
   :reclaimed (count (filter #(= :reclaimed (:acquisition-status %)) results))
   :dispatch-accepted (count (filter #(= :dispatch-accepted (:status %)) results))
   :contended (count (filter #(= :contended (:acquisition-status %)) results))
   :unavailable (count (filter #(= :unavailable (:acquisition-status %)) results))
   :results results
   :halt-reason reason
   :next-after next-after})

(defn process-cycle!
  "Execute one finite cycle for the explicitly installed Live source.

   Each iteration invokes worker/acquire-sweep! with ONE candidate per page
   and one page per call, then immediately invokes the v2096 synchronous
   delivery adapter on a successful physical lease. Therefore a delivery
   error cannot leave a subsequent page of newly acquired work stranded.

   Trusted options: {:worker-id string :lease-millis 1..300000
   :max-items 1..128 :after optional-UUID}. Defaults: 60s and 32 items.
   The caller supplies its Live system, rule ctx and expected source-id.

   A completed cycle returns finite results, counters and :next-after only
   when the observed page budget ends with more work ahead. It is NOT a
   snapshot-stable scan; a later cycle must restart without :after to find
   newly eligible identities behind its cursor.

   An acquisition or delivery failure throws :cycle-interrupted with all
   successfully completed preceding results, the last completed cursor,
   the in-flight lease when known, and the original cause. This is an
   operator-facing failure envelope, NOT permission to assume the last
   send failed, or to automatically replay it. A lease can expire and be
   rediscovered; duplicate invalidations remain possible and safe only
   to the extent that the final consumer refetches authoritative state."
  [node system ctx expected-source-id opts]
  (validate! system ctx expected-source-id opts)
  (let [worker-id (:worker-id opts)
        lease-millis (get opts :lease-millis default-lease-millis)
        max-items (get opts :max-items default-max-items)]
    (loop [cursor (:after opts)
           remaining max-items
           completed []]
      (let [work-opts (cond-> {:worker-id worker-id
                               :source-id expected-source-id
                               :lease-millis lease-millis
                               :page-size 1
                               :max-pages 1}
                        cursor (assoc :after cursor))
            page (try
                   (validate-page! (worker/acquire-sweep! node work-opts))
                   (catch Throwable e
                     (interrupted! :acquisition :acquisition-exception
                                   completed cursor nil
                                   {:original-error/type (:error/type (ex-data e))}
                                   e)))]
        (if (empty? (:results page))
          (summary worker-id completed :observed-end nil)
          (let [acquired (first (:results page))
                result (attempt-result! node system ctx expected-source-id
                                        acquired completed cursor)
                done (conj completed result)
                more? (= :page-budget (:halt-reason page))
                next-after (:next-after page)]
            (if (and more? (> remaining 1))
              (recur next-after (dec remaining) done)
              (summary worker-id done
                       (if more? :page-budget :observed-end)
                       (when more? next-after)))))))))
