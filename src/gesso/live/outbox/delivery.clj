(ns gesso.live.outbox.delivery
  "Synchronous adapter from a PHYSICAL Live outbox acquisition to the
   existing Live dispatcher/source and the guarded dispatch acknowledgment.

   This adapter does not enqueue an in-memory job: dispatcher queue acceptance
   can be lost on shutdown. It uses dispatch/run-sync! and records acceptance
   only after the Live source accepts the expanded invalidations. The source
   is itself ephemeral: neither SSE transport nor browser receipt is proved.

   The refresh basis is the actual successful XTDB LEASE-ACQUISITION receipt,
   which is later than the domain write that incorporated the outbox intent.
   This is intentionally more conservative than the original domain basis.
   The receipt must match the current outbox row's physical _system_from;
   fabricated or mismatched receipt/progression combinations are refused.

   External sends cannot be transactionally fenced. A concurrent reclaim can
   race with this source emission; a subsequent acknowledgment must still
   check its physical lease. Repeated invalidations are allowed. Successful
   acknowledgment proves source acceptance plus a terminal XTDB record, NOT
   durable delivery, complete source identity certification, or browser use."
  (:require [clojure.string :as str]
            [gesso.live.consistency.xtdb :as live.xtdb]
            [gesso.live.core :as live]
            [gesso.live.dispatch :as dispatch]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acknowledgment :as acknowledgment]
            [gesso.live.progression :as progression]
            [gesso.live.source :as source]
            [gesso.live.outbox.read-frontier :as frontier])
  (:import [java.time Instant ZonedDateTime]))

(def ^:private select-current-sql
  "SELECT *, _system_from FROM gesso_live_outbox WHERE _id = ?")

(defn- reject!
  [reason details]
  (throw (ex-info "Live outbox delivery not admitted."
                  (assoc details
                         :error/type :gesso.live.outbox/invalid-delivery
                         :reason reason))))

(defn- validate-acquisition!
  [acquisition expected-source-id]
  (when-not (map? acquisition)
    (reject! :invalid-acquisition {:value acquisition}))
  (when-not (#{:acquired :reclaimed} (:status acquisition))
    (reject! :not-acquired {:status (:status acquisition)}))
  (when-not (uuid? (:intent-id acquisition))
    (reject! :invalid-intent-id {:intent-id (:intent-id acquisition)}))
  (when-not (uuid? (:lease-id acquisition))
    (reject! :invalid-lease-id {:lease-id (:lease-id acquisition)}))
  (when-not (and (string? (:worker-id acquisition))
                 (not (str/blank? (:worker-id acquisition))))
    (reject! :invalid-worker-id {:worker-id (:worker-id acquisition)}))
  (when-not (and (string? expected-source-id)
                 (not (str/blank? expected-source-id)))
    (reject! :invalid-expected-source {:source-id expected-source-id}))
  (when-not (and (map? (:consistency acquisition))
                 (instance? Instant (get-in acquisition [:consistency :system-time]))
                 (= (:consistency acquisition)
                    (live.xtdb/tx-result-consistency (:tx-result acquisition)))
                 (some? (:progression acquisition))
                 (= (:progression acquisition)
                    (live.xtdb/tx-result-progression (:tx-result acquisition))))
    (reject! :invalid-acquisition-basis
             {:intent-id (:intent-id acquisition)}))
  (progression/require-requirement! (:progression acquisition))
  acquisition)

(defn- require-current-intent!
  [node acquisition expected-source-id]
  (let [intent-id (:intent-id acquisition)
        rows (frontier/q-from! node [select-current-sql intent-id])]
    ;; XTDB q returns a vector of maps (or [] for an observed absence).
    ;; An arbitrary countable or seq of valid-looking rows is not a physical
    ;; query result. In particular, it must not authorize source emission.
    (when-not (and (vector? rows) (every? map? rows))
      (reject! :invalid-physical-current-page
               {:intent-id intent-id
                :physical-type (some-> rows class .getName)}))
    (when-not (= 1 (count rows))
      (reject! :missing-or-ambiguous-intent {:intent-id intent-id
                                             :row-count (count rows)}))
    (let [row (first rows)
          system-from (:xt/system-from row)]
      (when-not (and (= intent-id (:xt/id row))
                     (= outbox/intent-type (:gesso.live.outbox/type row))
                     (= outbox/intent-version (:gesso.live.outbox/version row))
                     (= :leased (:gesso.live.outbox/status row))
                     (= (:lease-id acquisition)
                        (:gesso.live.outbox/lease-id row))
                     (= (:worker-id acquisition)
                        (:gesso.live.outbox/lease-owner row))
                     (instance? ZonedDateTime system-from)
                     (= (.toInstant ^ZonedDateTime system-from)
                        (get-in acquisition [:consistency :system-time])))
        (reject! :stale-or-unsupported-lease {:intent-id intent-id}))
      (when-not (= expected-source-id (:gesso.live.outbox/source-id row))
        (reject! :source-mismatch {:intent-id intent-id}))
      (when-not (and (= :gesso.live/refresh (:gesso.live.outbox/purpose row))
                     (qualified-keyword? (:gesso.live.outbox/operation row))
                     (keyword? (:gesso.live.outbox/topic row))
                     (or (string? (:gesso.live.outbox/subject-id row))
                         (keyword? (:gesso.live.outbox/subject-id row))
                         (integer? (:gesso.live.outbox/subject-id row))
                         (uuid? (:gesso.live.outbox/subject-id row)))
                     (or (nil? (:gesso.live.outbox/change-kind row))
                         (keyword? (:gesso.live.outbox/change-kind row))))
        (reject! :unsupported-intent {:intent-id intent-id}))
      row)))

(defn- restored-change
  [row requirement]
  (cond-> {:topic (:gesso.live.outbox/topic row)
           :id (:gesso.live.outbox/subject-id row)
           :progression requirement}
    (:gesso.live.outbox/change-kind row)
    (assoc :change/kind (:gesso.live.outbox/change-kind row))))

(defn- verify-source-acceptance!
  [dispatch-result expected-count intent-id]
  (let [source-result (:result dispatch-result)]
    (when-not (and (= :ran (:status dispatch-result))
                   (= :emitted-many (:status source-result))
                   (= expected-count (:count source-result))
                   (= expected-count (count (:results source-result)))
                   (every? #(#{:emitted :queued} (:status %))
                           (:results source-result)))
      (reject! :unverified-dispatch-acceptance
               {:intent-id intent-id
                :dispatch-status (:status dispatch-result)}))
    source-result))

(def ^:private original-run-sync dispatch/run-sync!)
(def ^:private original-live-emit-many live/emit-many!)
(def ^:private original-source-emit-many source/emit-many!)
(def ^:private original-source-emit source/emit!)

(defn source-boundaries-current?
  "Check installed synchronous dispatch and source function identities."
  []
  (and (identical? original-run-sync dispatch/run-sync!)
       (identical? original-live-emit-many live/emit-many!)
       (identical? original-source-emit-many source/emit-many!)
       (identical? original-source-emit source/emit!)))

(defn- run-source! [system expanded intent-id expected-source-id]
  ;; Verify the actual callback return, not a structurally plausible dispatch
  ;; report. The source functions are trusted in-process boundaries, not a
  ;; security sandbox against arbitrary application code or transient Var swaps.
  (let [run! dispatch/run-sync!
        emit! live/emit-many!
        instance (:source system)
        source-current? #(and (identical? original-live-emit-many emit!)
                              (identical? original-live-emit-many live/emit-many!)
                              (identical? original-source-emit-many source/emit-many!)
                              (identical? original-source-emit source/emit!))
        current-before? (source-current?)
        owner (Thread/currentThread)
        invocations (atom 0)
        returned (atom nil)
        result (run! {:run (fn []
                             (when-not (and (= 1 (swap! invocations inc))
                                            (identical? owner (Thread/currentThread)))
                               (reject! :unverified-dispatch-acceptance {:intent-id intent-id}))
                             (let [accepted (emit! system expanded)]
                               (reset! returned accepted)
                               accepted))})
        accepted (verify-source-acceptance! result (count expanded) intent-id)]
    (when-not (and (= 1 @invocations) (identical? @returned accepted)
                   current-before? (source-current?)
                   (= (:id instance) (:source/id accepted))
                   (every? #(= (:id instance) (:source/id %)) (:results accepted))
                   (= (vec expanded) (mapv :invalidation (:results accepted))))
      (reject! :unverified-dispatch-acceptance {:intent-id intent-id}))
    {:result accepted
     :observation (when (and (identical? original-run-sync run!)
                             (identical? original-run-sync dispatch/run-sync!))
                    {:source-id expected-source-id
                     :source-instance-id (:id instance)
                     :count (:count accepted)
                     :statuses (mapv :status (:results accepted))
                     :evidence :matched-synchronous-source-return})}))

(defn- committed-acknowledgment-evidence?
  "A status label is not an XTDB commit receipt. Check the complete returned
   transaction coordinates and that the receipt follows the acquired lease.
   This verifies a trusted adapter result's *internal contract*; it does not
   authenticate an arbitrary caller-supplied or forged transaction key."
  [ack acquisition]
  (let [ack-consistency (:consistency ack)
        lease-consistency (:consistency acquisition)]
    (try
      (and (map? ack-consistency)
           (integer? (:tx-id ack-consistency))
           (instance? Instant (:system-time ack-consistency))
           (integer? (:tx-id lease-consistency))
           (> (:tx-id ack-consistency) (:tx-id lease-consistency))
           (not (.isBefore ^Instant (:system-time ack-consistency)
                           ^Instant (:system-time lease-consistency)))
           (= ack-consistency
              (live.xtdb/tx-result-consistency (:tx-result ack)))
           (some? (:progression ack))
           (= (:progression ack)
              (live.xtdb/tx-result-progression (:tx-result ack))))
      (catch Throwable e
        (when (instance? InterruptedException e)
          (.interrupt (Thread/currentThread)))
        false))))

(defn- require-acknowledgment-result!
  "Classify a physical acknowledgment adapter result only after source
   emission. A positive label must match the exact guarded submission;
   contradictory or unrecognized results are POST-DISPATCH uncertainty,
   never a successful terminal receipt or a safe pre-dispatch rejection."
  [ack acquisition receipt-id]
  (let [intent-id (:intent-id acquisition)
        lease-id (:lease-id acquisition)
        known-negative?
        (and (#{:contended :unavailable} (:status ack))
             ;; Neither negative adapter result grants a lease or receipt.
             ;; Returning credentials alongside a negative status is
             ;; contradictory physical evidence, not a known rejection.
             (not-any? #(contains? ack %)
                       [:lease-id :receipt-id :worker-id])
             (case (:status ack)
               :contended (not (contains? ack :reason))
               :unavailable (#{:missing :not-leased} (:reason ack))
               false))]
  (when-not
   (and (map? ack)
        (= intent-id (:intent-id ack))
        (or (and (= :acknowledged (:status ack))
                 (= lease-id (:lease-id ack))
                 (= receipt-id (:receipt-id ack))
                 (committed-acknowledgment-evidence? ack acquisition))
            known-negative?))
    (throw (ex-info
            "Live source accepted an invalidation but XTDB acknowledgment returned inconsistent evidence."
            {:error/type :gesso.live.outbox/post-dispatch-ack-unknown
             :reason :invalid-acknowledgment-result
             :intent-id intent-id
             :lease-id lease-id
             :receipt-id receipt-id
             :dispatch/status :accepted
             :acknowledgment/status :unknown
             :reported-acknowledgment-status (:status ack)})))
  ack))

(def ^:private ^ThreadLocal acknowledgment-call (ThreadLocal.))
(def ^:private original-acknowledge acknowledgment/acknowledge-dispatch!)

(defn claim-acknowledgment-call!
  "Consume the exact delivery-to-acknowledgment call and its observed source return."
  [node submission]
  (let [scope (.get acknowledgment-call)]
    (when (and scope (identical? node (:node scope))
               (identical? submission (:submission scope))
               (identical? original-acknowledge acknowledgment/acknowledge-dispatch!))
      (.remove acknowledgment-call)
      (when (:source-acceptance scope)
        (select-keys scope [:acquisition :source-acceptance])))))

(defn- acknowledge-for-delivery! [node acquisition submission source-acceptance]
  (let [prior (.get acknowledgment-call)
        execute! acknowledgment/acknowledge-dispatch!]
    (.remove acknowledgment-call)
    (try
      (when (identical? original-acknowledge execute!)
        (.set acknowledgment-call {:node node :acquisition acquisition :submission submission
                                    :source-acceptance source-acceptance}))
      (execute! node submission)
      (finally
        (if prior (.set acknowledgment-call prior) (.remove acknowledgment-call))))))

(defn dispatch-acquired!
  "Synchronously emit one acquired/reclaimed intent, then acknowledge it.

   Inputs are the full, trusted XTDB acquisition RESULT (including its actual
   :tx-result, :consistency and :progression), a Gesso Live system, an ordinary
   trusted rule-expansion ctx, and the explicitly installed expected source-id.
   Only :gesso.live/refresh intents are admitted. Source identity equality
   is a caller-installed assertion, not proof that all physical writers used
   the admitted operation. The acquisition's provider-system-time must equal
   the currently leased outbox row's system-time exactly.

   No async queue is used. A source-acceptance result is not an SSE/browser
   receipt. Source exceptions or rejected dispatch do NOT acknowledge; a
   later expiry permits replay. If acceptance happened but the XTDB ack fails
   or its result becomes unknown, the exception is explicitly classified as
   POST-DISPATCH uncertainty; do not assume the invalidation was not sent.
   If a known lease conflict prevents ack, return :dispatched-unacknowledged.
   All emitted invalidations carry the lease transaction's trusted basis,
   establishing a conservative refresh requirement AFTER the domain write."
  [node system ctx acquisition expected-source-id]
  (validate-acquisition! acquisition expected-source-id)
  (let [intent-id (:intent-id acquisition)
        row (require-current-intent! node acquisition expected-source-id)
        change (restored-change row (:progression acquisition))
        expanded (live/expand system ctx change)]
    (when-not (seq expanded)
      (reject! :no-invalidations {:intent-id intent-id}))
    ;; This execution cannot disappear from an async queue after an ack. Still,
    ;; source acceptance is not proof that a browser actually got the event.
    (let [{source-result :result source-acceptance :observation}
          (try
            (run-source! system expanded intent-id expected-source-id)
            (catch Throwable cause
              ;; Source/emit-many! is not atomic with close! or a mid-batch
              ;; failure. The source may already have accepted a prefix even
              ;; when the job throws or its final acceptance report is
              ;; contradictory. Keep the attempted lease for reconciliation;
              ;; never reinterpret this as a safe, definitely unsent failure.
              (when (instance? InterruptedException cause)
                (.interrupt (Thread/currentThread)))
              (throw (ex-info
                      "Live source emission has an unknown physical outcome; reconcile the outbox before retry."
                      {:error/type :gesso.live.outbox/source-emission-unknown
                       :reason (or (:reason (ex-data cause))
                                   :source-emission-exception)
                       :intent-id intent-id
                       :lease-id (:lease-id acquisition)
                       :dispatch/status :unknown
                       :source-acceptance :unknown
                       :acknowledgment/status :not-attempted
                       :rescan-required? true
                       :original-error/type (:error/type (ex-data cause))}
                      cause))))
          receipt {:receipt-id (random-uuid)
                   :adapter :gesso.live/synchronous-source
                   :result :accepted}
          fenced {:intent-id intent-id
                  :lease-id (:lease-id acquisition)
                  :worker-id (:worker-id acquisition)
                  :receipt receipt}
          ack (try
                (acknowledge-for-delivery! node acquisition fenced source-acceptance)
                (catch Throwable e
                  ;; An interrupted physical acknowledgment has no known
                  ;; terminal outcome. Preserve JVM cancellation while
                  ;; reporting POST-DISPATCH uncertainty, not a safe retry.
                  (when (instance? InterruptedException e)
                    (.interrupt (Thread/currentThread)))
                  (throw (ex-info "Live source accepted an invalidation before XTDB acknowledgment failed."
                                  {:error/type :gesso.live.outbox/post-dispatch-ack-unknown
                                   :intent-id intent-id
                                   :lease-id (:lease-id acquisition)
                                   :receipt-id (:receipt-id receipt)
                                   :dispatch/status :accepted
                                   :acknowledgment/status :unknown}
                                  e))))]
      (require-acknowledgment-result!
       ack acquisition (:receipt-id receipt))
      (if (= :acknowledged (:status ack))
        {:status :dispatch-accepted
         :intent-id intent-id
         :lease-id (:lease-id acquisition)
         :receipt-id (:receipt-id receipt)
         :progression (:progression acquisition)
         :source-result source-result
         :acknowledgment ack}
        {:status :dispatched-unacknowledged
         :intent-id intent-id
         :lease-id (:lease-id acquisition)
         :receipt-id (:receipt-id receipt)
         :acknowledgment ack
         :source-result source-result}))))
