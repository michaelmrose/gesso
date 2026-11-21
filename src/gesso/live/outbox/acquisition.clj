(ns gesso.live.outbox.acquisition
  "Single-node, opt-in physical acquisition of a pending Live outbox intent.

   A claim reads one CURRENT XTDB row, checks that it is a version-1 pending
   intent, and submits one transaction containing a provider-system-time
   compare-and-swap ASSERT followed by a replacement document with a lease.
   A stale claimant cannot overwrite an already leased or otherwise changed
   record through this path. The lease remains durable after a process crash.

   This does NOT scan for pending work, reclaim expired leases, deliver notices,
   acknowledge delivery, fence external effects, authenticate the caller, or
   prove closure of arbitrary XTDB writers. The supplied clock and worker
   identity are trusted host inputs. No post-commit delivery is implied."
  (:require [clojure.string :as str]
            [gesso.live.consistency.xtdb :as live.xtdb]
            [gesso.live.outbox :as outbox]
            [xtdb.api :as xt])
  (:import [java.time Duration Instant ZonedDateTime]))

(def ^:private max-lease-millis 300000)
(def ^:private claim-keys
  #{:intent-id :worker-id :lease-id :now :lease-until})
(def ^:private read-current-sql
  "SELECT *, _system_from FROM gesso_live_outbox WHERE _id = ?")
(def ^:private guard-system-from-sql
  "ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ?)")

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
  (let [rows (xt/q node [read-current-sql intent-id])]
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
     [:put-docs outbox/intent-table claimed]]))

(defn acquire-pending!
  "Attempt a single CAS acquisition of one pending intent from a live XTDB node.

   :intent-id and :lease-id are UUIDs, :worker-id is nonblank, and :now and
   :lease-until are java.time.Instant values. Lease duration must be 1ms to
   5min. Inputs must be supplied by a trusted single-node worker, not a
   request parameter. Neither a lease nor a pre-commit query authorizes
   publication independently of the installed worker and source checks.

   Returns {:status :acquired, :tx-result ..., :consistency ...} on the actual
   XTDB commit; {:status :unavailable, :reason :missing | :not-pending} for
   observations not eligible for acquisition; and {:status :contended} only
   when XTDB rejects the guarded transaction with :xtdb/assert-failed.

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
        (if (not= :pending (:gesso.live.outbox/status intent))
          {:status :unavailable :reason :not-pending :intent-id intent-id}
          (let [tx-ops (acquire-ops intent claim)]
            (try
              (let [result (live.xtdb/execute-tx! node tx-ops)]
                (merge result
                       {:status :acquired
                        :intent-id intent-id
                        :lease-id (:lease-id claim)
                        :worker-id (:worker-id claim)
                        :lease-until (:lease-until claim)}))
              (catch Throwable cause
                (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data cause)))
                  {:status :contended :intent-id intent-id}
                  (throw cause))))))))))
