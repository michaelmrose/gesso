(ns gesso.live.outbox.reconciliation
  "Read-only reconciliation of an interrupted single-node outbox cycle.

   The cycle/lifecycle reports an in-flight lease and, sometimes, an
   *unverified* submitted acknowledgment receipt. Inspect the current XTDB
   row rather than interpreting either report as a completed delivery.
   This API never sends, leases, acknowledges, or grants permission to resume
   a failed lifecycle. The observed row can change after the read; a caller
   still needs an independently fenced worker restart/recovery policy.

   A recorded matching receipt establishes only a durable XTDB
   :dispatch-accepted state for that lease. It does not establish SSE delivery,
   browser adoption, or exactly-once external effects."
  (:require [clojure.string :as str]
            [gesso.live.outbox :as outbox]
            [xtdb.api :as xt])
  (:import [java.time ZonedDateTime]))

(def ^:private current-sql
  "SELECT *, _system_from FROM gesso_live_outbox WHERE _id = ?")

(defn- reject!
  [reason details]
  (throw (ex-info "Invalid Live outbox reconciliation request or physical row."
                  (assoc details :error/type
                         :gesso.live.outbox/invalid-reconciliation
                         :reason reason))))

(defn- valid-worker? [x]
  (and (string? x) (not (str/blank? x))))

(defn- require-envelope! [expected-source-id failure]
  (when-not (valid-worker? expected-source-id)
    (reject! :invalid-source-id {}))
  (when-not (and (map? failure)
                 (= :gesso.live.outbox/cycle-interrupted (:error/type failure))
                 (true? (:rescan-required? failure)))
    (reject! :invalid-interruption-envelope {}))
  (let [claim (:in-flight failure)]
    (when (some? claim)
      (when-not (and (map? claim)
                     (uuid? (:intent-id claim))
                     (uuid? (:lease-id claim))
                     (valid-worker? (:worker-id claim)))
        (reject! :invalid-in-flight {}))
      (let [receipt (:receipt-id claim)
            evidence (:receipt-evidence claim)]
        (when-not (or (and (nil? receipt) (nil? evidence))
                      (and (uuid? receipt)
                           (= :unverified-post-dispatch-receipt evidence)))
          (reject! :invalid-reported-receipt {}))))
    claim))

(defn- current-intent! [node intent-id]
  (let [rows (try
               (xt/q node [current-sql intent-id])
               (catch Throwable cause
                 ;; The provider may clear the JVM interrupt flag before
                 ;; throwing. A failed read must never become an empty page.
                 (when (instance? InterruptedException cause)
                   (.interrupt (Thread/currentThread)))
                 (throw cause)))]
    (when-not (and (vector? rows) (<= (count rows) 1)
                   (every? map? rows))
      (reject! :invalid-physical-current-page {:intent-id intent-id}))
    (first rows)))

(defn- require-row! [row intent-id expected-source-id]
  (when-not (and (= intent-id (:xt/id row))
                 (= outbox/intent-type (:gesso.live.outbox/type row))
                 (= outbox/intent-version (:gesso.live.outbox/version row))
                 (instance? ZonedDateTime (:xt/system-from row))
                 (qualified-keyword? (:gesso.live.outbox/operation row))
                 (qualified-keyword? (:gesso.live.outbox/purpose row))
                 (#{:pending :leased :dispatch-accepted}
                  (:gesso.live.outbox/status row)))
    (reject! :invalid-physical-intent {:intent-id intent-id}))
  (when-not (= expected-source-id (:gesso.live.outbox/source-id row))
    (reject! :source-mismatch {:intent-id intent-id}))
  (when (#{:leased :dispatch-accepted} (:gesso.live.outbox/status row))
    (when-not (and (uuid? (:gesso.live.outbox/lease-id row))
                   (valid-worker? (:gesso.live.outbox/lease-owner row))
                   (instance? ZonedDateTime (:gesso.live.outbox/lease-until row)))
      (reject! :invalid-physical-lease {:intent-id intent-id})))
  (when (= :dispatch-accepted (:gesso.live.outbox/status row))
    (when-not (and (uuid? (:gesso.live.outbox/dispatch-receipt-id row))
                   (qualified-keyword? (:gesso.live.outbox/dispatch-adapter row)))
      (reject! :invalid-physical-receipt {:intent-id intent-id})))
  row)

(defn- disposition [row claim]
  (let [state (:gesso.live.outbox/status row)
        same-lease? (and (= (:lease-id claim)
                            (:gesso.live.outbox/lease-id row))
                         (= (:worker-id claim)
                            (:gesso.live.outbox/lease-owner row)))]
    (case state
      :pending :pending-after-interruption
      :leased (if same-lease? :same-lease-active :lease-replaced)
      :dispatch-accepted
      (cond
        (not same-lease?) :other-lease-recorded
        (nil? (:receipt-id claim)) :terminal-without-receipt-correlation
        (= (:receipt-id claim)
           (:gesso.live.outbox/dispatch-receipt-id row))
        :matching-acknowledgment-recorded
        :else :other-receipt-recorded))))

(defn inspect!
  "Inspect one interrupted cycle's in-flight lease against current XTDB state.

   `failure` is the exception data from cycle/process-cycle! (or the
   lifecycle's retained failure report); `expected-source-id` comes from
   the installed trusted host, not from the report. No lease identity means
   :unidentified without a physical query, requiring a full source rescan.

   A matching terminal receipt requires exact lease/worker identity and a
   correctly tagged, UUID *reported* receipt. It observes a current durable
   acknowledgment row, NOT the originating source send or browser receipt.
   Every return includes :rescan-required? true; none authorizes immediate
   retry, replay, or resumption. Reclaimed leases and terminal receipts may
   have changed again by the time the caller takes further action."
  [node expected-source-id failure]
  (let [claim (require-envelope! expected-source-id failure)]
    (if (nil? claim)
      {:status :unidentified :rescan-required? true}
      (let [intent-id (:intent-id claim)
            row (current-intent! node intent-id)]
        (if (nil? row)
          {:status :missing :intent-id intent-id
           :rescan-required? true}
          (let [row (require-row! row intent-id expected-source-id)]
            {:status (disposition row claim)
             :intent-id intent-id
             :observed-status (:gesso.live.outbox/status row)
             :observed-system-from (:xt/system-from row)
             :rescan-required? true}))))))
