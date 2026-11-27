(ns gesso.live.outbox.acquisition
  "Single-node, opt-in physical acquisition of a pending Live outbox intent.

   A claim reads one CURRENT XTDB row after awaiting the selected node's
   freshly captured submitted frontier, checks that it is a version-1 pending
   intent, and submits one transaction containing a provider-system-time
   compare-and-swap ASSERT followed by a replacement document with a lease.
   A stale claimant cannot overwrite an already leased or otherwise changed
   record through this path. Initial acquisition also checks the proposed
   deadline against XTDB transaction time: already-expired or over-five-minute
   leases fail before any lease row is written. The lease remains durable
   after a process crash.

   Expired leases can be reclaimed with an XTDB transaction-time clock guard,
   an observed system-time CAS, and a NEW lease token. A separate fence
   constructor lets a future acknowledgment transaction demand that its
   original lease still owns the physical row at the time of its write.

   An optional trusted :source-id fences either acquisition against the
   currently read source under the same system-time CAS; scheduled workers
   always supply it. Direct low-level callers may omit that extra scope.

   This does NOT scan for work, deliver notices, acknowledge delivery, fence
   external side effects, authenticate the caller, or prove arbitrary writer
   closure. A new claimant may duplicate an external send performed before the
   old claimant crashed. :now and worker identity are trusted host inputs,
   while initial and recovery lease deadlines, and recovery expiry
   eligibility, are additionally checked against XTDB's clock."
  (:require [clojure.string :as str]
            [gesso.live.consistency.xtdb :as live.xtdb]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.read-frontier :as frontier])
  (:import [java.time Duration Instant ZonedDateTime]))

(def ^:private max-lease-millis 300000)
(def ^:private claim-keys
  #{:intent-id :worker-id :lease-id :now :lease-until :source-id})
(def ^:private read-current-sql
  "SELECT *, _system_from FROM gesso_live_outbox WHERE _id = ?")
(def ^:private guard-system-from-sql
  "ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ?)")
(def ^:private guard-expired-system-from-sql
  (str "ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox "
       "WHERE _id = ? AND _system_from = ? AND CURRENT_TIMESTAMP >= ?)"))
(def ^:private guard-active-system-from-sql
  (str "ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox "
       "WHERE _id = ? AND _system_from = ? AND CURRENT_TIMESTAMP < ?)"))
(def ^:private guard-new-deadline-sql
  "ASSERT CURRENT_TIMESTAMP < ?")
(def ^:private guard-max-new-deadline-sql
  "ASSERT CURRENT_TIMESTAMP + INTERVAL '5' MINUTE >= ?")
(def ^:private lease-fence-keys #{:intent-id :lease-id :worker-id})

(defn- restore-interrupt!
  "Retain JVM cancellation when a physical lease transaction throws after
   an interruptible operation has cleared the calling thread's flag."
  [cause]
  (when (instance? InterruptedException cause)
    (.interrupt (Thread/currentThread))))

(defn- reject!
  [reason data]
  (throw (ex-info "Invalid Gesso Live outbox acquisition."
                  (assoc data :error/type :gesso.live.outbox/invalid-acquisition
                         :reason reason))))

(defn- validate-claim!
  [claim]
  (when-not (map? claim)
    (reject! :invalid-claim {:claim claim}))
  (when-let [unexpected (seq (remove claim-keys (keys claim)))]
    (reject! :unknown-fields {:fields (vec unexpected)}))
  (when-not (uuid? (:intent-id claim))
    (reject! :invalid-intent-id {:intent-id (:intent-id claim)}))
  (when-not (and (string? (:worker-id claim))
                 (not (str/blank? (:worker-id claim))))
    (reject! :invalid-worker-id {:worker-id (:worker-id claim)}))
  (when (contains? claim :source-id)
    (when-not (and (string? (:source-id claim))
                   (not (str/blank? (:source-id claim))))
      (reject! :invalid-source-id {:source-id (:source-id claim)})))
  (when-not (uuid? (:lease-id claim))
    (reject! :invalid-lease-id {:lease-id (:lease-id claim)}))
  (when-not (and (instance? Instant (:now claim))
                 (instance? Instant (:lease-until claim)))
    (reject! :invalid-clock {:now (:now claim)
                             :lease-until (:lease-until claim)}))
  (let [millis (.toMillis (Duration/between ^Instant (:now claim)
                                              ^Instant (:lease-until claim)))]
    (when-not (<= 1 millis max-lease-millis)
      (reject! :invalid-lease-duration {:millis millis})))
  claim)

(defn- current-intent!
  [node intent-id]
  (let [rows (frontier/q-from! node [read-current-sql intent-id])]
    ;; XTDB q returns a vector (including [] for an observed missing row).
    ;; Nil, maps, sequences, and non-map entries do not establish that the
    ;; intent is absent. Never turn a malformed physical read into a safe
    ;; :unavailable/:missing result or an authoritative missing-lease fence.
    (when-not (and (vector? rows) (every? map? rows))
      (reject! :invalid-physical-current-page
               {:intent-id intent-id
                :physical-type (some-> rows class .getName)}))
    (when (> (count rows) 1)
      (reject! :ambiguous-current-row {:intent-id intent-id}))
    (first rows)))

(defn- require-intent!
  [intent intent-id]
  (when-not (and (= intent-id (:xt/id intent))
                 (= outbox/intent-type (:gesso.live.outbox/type intent))
                 (= outbox/intent-version (:gesso.live.outbox/version intent))
                 (instance? ZonedDateTime (:xt/system-from intent))
                 (string? (:gesso.live.outbox/source-id intent))
                 (qualified-keyword? (:gesso.live.outbox/operation intent))
                 (qualified-keyword? (:gesso.live.outbox/purpose intent)))
    (reject! :invalid-physical-intent {:intent-id intent-id}))
  intent)

(defn- source-mismatch?
  [intent claim]
  (and (contains? claim :source-id)
       (not= (:source-id claim) (:gesso.live.outbox/source-id intent))))

(defn- acquire-ops
  [intent {:keys [intent-id worker-id lease-id lease-until]}]
  (let [original-system-from (:xt/system-from intent)
        claimed (-> intent
                    (dissoc :xt/system-from)
                    (assoc :gesso.live.outbox/status :leased
                           :gesso.live.outbox/lease-id lease-id
                           :gesso.live.outbox/lease-owner worker-id
                           :gesso.live.outbox/lease-until lease-until))]
    [[guard-system-from-sql intent-id original-system-from]
     ;; Host-relative duration validation is insufficient: the host clock
     ;; could be stale or fast. Check both bounds in the actual XTDB tx.
     [guard-new-deadline-sql lease-until]
     [guard-max-new-deadline-sql lease-until]
     [:put-docs outbox/intent-table claimed]]))

(def ^:private ^ThreadLocal maintenance-handoff (ThreadLocal.))
(def ^:private original-execute live.xtdb/execute-tx!)

(defn claim-maintenance-handoff!
  "Consume the exact synchronous constructor-to-adapter handoff once.
   This is local submission observation, not admission or a commit receipt."
  [node ops]
  (let [scope (.get maintenance-handoff)]
    (when (and scope (identical? original-execute live.xtdb/execute-tx!)
               (identical? node (:node scope)) (identical? ops (:ops scope)))
      (.remove maintenance-handoff)
      (:maintenance scope))))

(defn- execute-maintenance! [node maintenance ops]
  (let [prior (.get maintenance-handoff)
        execute! live.xtdb/execute-tx!]
    (.remove maintenance-handoff)
    (try
      (when (identical? original-execute execute!)
        (.set maintenance-handoff {:node node :ops ops :maintenance maintenance}))
      (execute! node ops)
      (finally
        (if prior (.set maintenance-handoff prior) (.remove maintenance-handoff))))))

(defn acquire-pending!
  "Attempt a single CAS acquisition of one pending intent from a live XTDB node.

   :intent-id and :lease-id are UUIDs, :worker-id is nonblank, :source-id
   is an optional caller-installed source fence, and :now and
   :lease-until are java.time.Instant values. Lease duration must be 1ms to
   5min relative to the supplied :now, AND the deadline must be within
   (CURRENT_TIMESTAMP, CURRENT_TIMESTAMP + 5min] in the XTDB transaction.
   Inputs must be supplied by a trusted single-node worker, not a request
   parameter. Neither a lease nor a pre-commit query authorizes
   publication independently of the installed worker and source checks.

   Returns {:status :acquired, :tx-result ..., :consistency ...} on the actual
   XTDB commit; {:status :unavailable, :reason :missing | :not-pending} for
   observations not eligible for acquisition; and {:status :contended} only
   when XTDB rejects the guarded transaction with :xtdb/assert-failed,
   including stale or excessive deadlines relative to the provider clock.

   Other failures, including any indeterminate post-commit result, propagate;
   they MUST NOT be reclassified as a safe rejection or permission to retry
   externally visible work. No post-commit delivery is performed here."
  [node claim]
  (validate-claim! claim)
  (let [intent-id (:intent-id claim)
        row (current-intent! node intent-id)]
    (cond
      (nil? row)
      {:status :unavailable :reason :missing :intent-id intent-id}

      :else
      (let [intent (require-intent! row intent-id)]
        (cond
          (source-mismatch? intent claim)
          {:status :unavailable :reason :source-mismatch :intent-id intent-id}

          (not= :pending (:gesso.live.outbox/status intent))
          {:status :unavailable :reason :not-pending :intent-id intent-id}

          :else
          (let [tx-ops (acquire-ops intent claim)]
            (try
              (let [result (execute-maintenance! node :lease-acquisition tx-ops)]
                (merge result
                       {:status :acquired
                        :intent-id intent-id
                        :lease-id (:lease-id claim)
                        :worker-id (:worker-id claim)
                        :lease-until (:lease-until claim)}))
              (catch Throwable cause
                ;; Only a provider assertion is a known contention result;
                ;; an interruption cannot silently authorize retry.
                (restore-interrupt! cause)
                (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data cause)))
                  {:status :contended :intent-id intent-id}
                  (throw cause))))))))))


(defn- require-leased!
  "Reject malformed physical lease metadata rather than reclaiming unknown
   state or silently treating it as a new pending intent."
  [intent intent-id]
  (when-not (and (uuid? (:gesso.live.outbox/lease-id intent))
                 (string? (:gesso.live.outbox/lease-owner intent))
                 (not (str/blank? (:gesso.live.outbox/lease-owner intent)))
                 (instance? ZonedDateTime (:gesso.live.outbox/lease-until intent)))
    (reject! :invalid-physical-lease {:intent-id intent-id}))
  intent)

(defn- require-fence!
  [fence]
  (when-not (map? fence)
    (reject! :invalid-fence {:fence fence}))
  (when-let [extra (seq (remove lease-fence-keys (keys fence)))]
    (reject! :unknown-fence-fields {:fields (vec extra)}))
  (when-not (uuid? (:intent-id fence))
    (reject! :invalid-intent-id {:intent-id (:intent-id fence)}))
  (when-not (uuid? (:lease-id fence))
    (reject! :invalid-lease-id {:lease-id (:lease-id fence)}))
  (when-not (and (string? (:worker-id fence))
                 (not (str/blank? (:worker-id fence))))
    (reject! :invalid-worker-id {:worker-id (:worker-id fence)}))
  fence)

(defn active-lease-guard-ops
  "Construct physical lease fencing for inclusion in a LATER XTDB transaction.

   The caller provides a trusted {:intent-id UUID :lease-id UUID :worker-id
   string}. An authoritative current read must match all three identifiers,
   the required publication intent, and :leased state. The returned single
   ASSERT verifies the same XTDB _system_from and a still-live deadline at
   the *actual transaction*, so an expired or replaced lease cannot change
   the outbox via a transaction that includes this guard before its writes.

   This is a GUARD CONSTRUCTOR, NOT an endorsement of external side effects.
   Do not execute its ASSERT separately before publishing to SSE: another
   claimant might acquire the intent after that check and before the send.
   Include this guard and any future acknowledgment DML in ONE transaction.
   Actual delivery, acknowledgment semantics, and external-effect fencing
   remain outside this revision."
  [node fence]
  (require-fence! fence)
  (let [{:keys [intent-id lease-id worker-id]} fence
        row (current-intent! node intent-id)]
    (when-not row
      (reject! :missing-lease {:intent-id intent-id}))
    (let [intent (require-intent! row intent-id)]
      (when-not (= :leased (:gesso.live.outbox/status intent))
        (reject! :not-leased {:intent-id intent-id}))
      (require-leased! intent intent-id)
      (when-not (and (= lease-id (:gesso.live.outbox/lease-id intent))
                     (= worker-id (:gesso.live.outbox/lease-owner intent)))
        (reject! :stale-lease {:intent-id intent-id}))
      [[guard-active-system-from-sql
        intent-id (:xt/system-from intent)
        (:gesso.live.outbox/lease-until intent)]])))

(defn reclaim-expired!
  "Reacquire a formerly leased intent only after its old lease has expired.

   Uses the same validated claim format as acquire-pending!, and requires a
   *different* UUID lease token from the old owner. With the row in :leased
   state, its version-1 metadata is validated and a single XTDB transaction
   atomically checks: (1) the originally read _system_from is still current,
   (2) the ORIGINAL deadline has passed according to XTDB CURRENT_TIMESTAMP,
   (3) the new deadline is in the future according to XTDB, and (4) it is no
   more than five minutes into XTDB's future. Then the row is replaced with
   the new worker, lease token, and deadline, retaining its publication data.

   Returns :reclaimed on actual commit, :unavailable for missing/not-leased,
   or :contended for an XTDB ASSERT rejection (including not-yet-expired or
   stale/preempted reads). Unknown and post-commit errors propagate. There is
   no implicit send or acknowledgment, and recovery can cause duplicate sends.
   A worker must apply active-lease-guard-ops in its later STATE transition;
   even that does not fence an external side effect such as an SSE send."
  [node claim]
  (validate-claim! claim)
  (let [{:keys [intent-id lease-id worker-id lease-until]} claim
        row (current-intent! node intent-id)]
    (cond
      (nil? row)
      {:status :unavailable :reason :missing :intent-id intent-id}

      :else
      (let [intent (require-intent! row intent-id)]
        (cond
          (source-mismatch? intent claim)
          {:status :unavailable :reason :source-mismatch :intent-id intent-id}

          (not= :leased (:gesso.live.outbox/status intent))
          {:status :unavailable :reason :not-leased :intent-id intent-id}

          :else
          (do
            (require-leased! intent intent-id)
            (when (= lease-id (:gesso.live.outbox/lease-id intent))
              (reject! :reused-lease-id {:intent-id intent-id}))
            (let [new-intent (-> intent
                                 (dissoc :xt/system-from)
                                 (assoc :gesso.live.outbox/status :leased
                                        :gesso.live.outbox/lease-id lease-id
                                        :gesso.live.outbox/lease-owner worker-id
                                        :gesso.live.outbox/lease-until lease-until))
                  ops [[guard-expired-system-from-sql
                        intent-id (:xt/system-from intent)
                        (:gesso.live.outbox/lease-until intent)]
                       [guard-new-deadline-sql lease-until]
                       [guard-max-new-deadline-sql lease-until]
                       [:put-docs outbox/intent-table new-intent]]]
              (try
                (merge (execute-maintenance! node :expired-lease-reclamation ops)
                       {:status :reclaimed
                        :intent-id intent-id
                        :lease-id lease-id
                        :worker-id worker-id
                        :lease-until lease-until})
                (catch Throwable cause
                  (restore-interrupt! cause)
                  (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data cause)))
                    {:status :contended :intent-id intent-id}
                    (throw cause)))))))))))
