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
            [gesso.live.outbox.read-frontier :as frontier])
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
  (let [rows (frontier/q-from! node [current-sql intent-id])]
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

(def ^:private max-completed 128)

(defn- require-completed!
  "Validate the cycle's bounded completed prefix before reading ANY physical
   row. A completed claim is a trusted cycle report, not an independently
   authenticated transaction or proof of client delivery."
  [failure]
  (when (contains? failure :completed)
    (let [completed (:completed failure)
          ids (when (vector? completed) (mapv :intent-id completed))]
      (when-not
       (and (vector? completed)
            (<= (count completed) max-completed)
            (= (count completed) (:completed-count failure))
            (every? uuid? ids)
            (every? (fn [[a b]] (neg? (compare (str a) (str b))))
                    (partition 2 1 ids))
            (or (empty? ids)
                (= (peek ids) (:last-completed-cursor failure)))
            (every?
             (fn [item]
               (and (map? item)
                    (case (:acquisition-status item)
                      (:acquired :reclaimed)
                      (and (= :dispatch-accepted (:status item))
                           (valid-worker? (:worker-id item))
                           (uuid? (:lease-id item))
                           (uuid? (:receipt-id item))
                           (some? (:progression item)))
                      (:contended :unavailable)
                      (and (nil? (:status item))
                           (not-any? #(contains? item %)
                                     [:lease-id :receipt-id]))
                      false)))
             completed))
        (reject! :invalid-completed-prefix {}))
      completed)))

(defn- inspect-completed-entry!
  [node expected-source-id entry]
  (let [intent-id (:intent-id entry)]
    (if (nil? (:status entry))
      ;; A contended/unavailable acquisition did not produce a completed
      ;; dispatch. Do not infer its current physical state or issue a read.
      {:intent-id intent-id :status :no-dispatch-claim}
      (let [row (current-intent! node intent-id)]
        (if (nil? row)
          {:intent-id intent-id :status :missing}
          (let [row (require-row! row intent-id expected-source-id)
                status (:gesso.live.outbox/status row)
                matched? (and (= :dispatch-accepted status)
                              ;; The physical lease owner is part of the
                              ;; acknowledged dispatch identity. A reused
                              ;; token/receipt under another owner must not
                              ;; certify the completed prefix for restart.
                              (= (:worker-id entry)
                                 (:gesso.live.outbox/lease-owner row))
                              (= (:lease-id entry)
                                 (:gesso.live.outbox/lease-id row))
                              (= (:receipt-id entry)
                                 (:gesso.live.outbox/dispatch-receipt-id row)))]
            {:intent-id intent-id
             :status (cond
                       matched? :matching-acknowledgment-recorded
                       (= :dispatch-accepted status)
                       :different-acknowledgment-recorded
                       :else :not-terminal)
             :observed-status status
             :observed-system-from (:xt/system-from row)}))))))

(defn inspect!
  "Inspect the interrupted cycle's bounded completed prefix and in-flight
   lease against CURRENT physical XTDB rows after awaiting a freshly captured
   submitted log frontier for each read. Neither a completed cycle report
   nor a matching stored acknowledgment establishes SSE/browser delivery.

   When :completed is present, check every reported entry in UUID order.
   The resulting :completed-observations are separate, non-atomic current-row
   reads: even matching records cannot justify a release/replay decision.
   :contended/:unavailable entries were not dispatch claims and are not read.

   No in-flight identity yields :unidentified but may still provide physical
   evidence of a previously completed prefix. The caller must independently
   qualify any restart, full rescan, or lease reclaim after inspection."
  [node expected-source-id failure]
  (let [claim (require-envelope! expected-source-id failure)
        completed (require-completed! failure)
        observed (when (some? completed)
                   (mapv #(inspect-completed-entry! node expected-source-id %)
                         completed))
        in-flight
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
                 :rescan-required? true}))))]
    (cond-> in-flight
      (some? completed) (assoc :completed-observations observed))))
