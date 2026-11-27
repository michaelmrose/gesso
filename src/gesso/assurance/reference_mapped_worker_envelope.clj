(ns gesso.assurance.reference-mapped-worker-envelope
  "Local comparison of a selected maintenance sequence, not cut issuance."
  (:require [gesso.assurance.reference-retained-acquisition :as acquisition]
            [gesso.assurance.xtdb-envelope :as envelope]
            [gesso.live.outbox.worker-capture :as capture])
  (:import [java.time Instant]
           [java.time.temporal ChronoUnit]
           [xtdb.api TransactionKey]))

(def profile :selected-issued-worker-submission-correspondence-v2)
(def ^:private original-host-matcher capture/worker-outcome-capture-matches?)
(def ^:private original-acquisition-reader acquisition/read-acquisition!)
(def ^:private original-envelope-check envelope/check-worker-attempt-envelope!)

(defn boundaries-current? []
  (and (identical? original-host-matcher capture/worker-outcome-capture-matches?)
       (identical? original-acquisition-reader acquisition/read-acquisition!)
       (identical? original-envelope-check envelope/check-worker-attempt-envelope!)))

(defn- report [status reason]
  (cond-> {:profile profile :status status :authority :none :certificate :not-issued
           :worker-authentication :not-established :dispatch-authentication :not-established
           :provider-refinement :not-established :complete-writer-coverage :not-established}
    reason (assoc :reason reason)))

(defn- match-local-receipt? [maintenance admission row]
  (if (= :dispatch-acknowledgment maintenance)
    (and (uuid? (:gesso.live.outbox/lease-id row))
         (uuid? (:gesso.live.outbox/dispatch-receipt-id row))
         (= :gesso.live/synchronous-source (:gesso.live.outbox/dispatch-adapter row))
         (= {:intent-id (:xt/id row)
             :lease-id (:gesso.live.outbox/lease-id row)
             :worker-id (:gesso.live.outbox/lease-owner row)
             :receipt-id (:gesso.live.outbox/dispatch-receipt-id row)
             :adapter (:gesso.live.outbox/dispatch-adapter row)
             :result :accepted
             :evidence :matched-local-delivery-handoff}
            (:local-dispatch-receipt admission)))
    (not (contains? admission :local-dispatch-receipt))))

(defn- match-source-acceptance? [source-id instance-id maintenance admission]
  (if (= :dispatch-acknowledgment maintenance)
    (let [observation (:local-source-acceptance admission)
          receipt (:local-dispatch-receipt admission)
          identities [:intent-id :lease-id :worker-id :receipt-id]]
      (and (map? observation)
           (= #{:source-id :source-instance-id :count :statuses :evidence
                :intent-id :lease-id :worker-id :receipt-id} (set (keys observation)))
           (= (select-keys receipt identities) (select-keys observation identities))
           (= source-id (:source-id observation))
           ;; The installed lineage supplies the runtime ID independently of
           ;; the captured call. Limit evidence to losslessly encodable IDs.
           (or (uuid? instance-id) (string? instance-id) (keyword? instance-id)
               (integer? instance-id) (boolean? instance-id))
           (= instance-id (:source-instance-id observation))
           (= :matched-synchronous-source-return (:evidence observation))
           (pos-int? (:count observation))
           (vector? (:statuses observation))
           (= (:count observation) (count (:statuses observation)))
           (every? #{:emitted :queued} (:statuses observation))))
    (not (contains? admission :local-source-acceptance))))

(defn- match-call? [source-id instance-id expected retained call]
  (let [{:keys [tx-ops tx-result admission]} call
        origin (:worker-origin admission)
        put (when (vector? tx-ops) (peek tx-ops))
        row (when (and (vector? put) (= 3 (count put))
                       (= [:put-docs :gesso_live_outbox] (subvec put 0 2)))
              (peek put))]
    (and retained (map? row)
         (= :execute-tx (:api call)) (= :returned (:outcome call))
         (= {} (:tx-options call)) (instance? TransactionKey tx-result)
         (= (:tx-id expected) (:tx-id retained) (.getTxId ^TransactionKey tx-result))
         (= (:system-time retained) (.getSystemTime ^TransactionKey tx-result))
         (= :worker (:kind admission))
         (= (:intent-id expected) (:intent-id admission) (:xt/id row))
         (= (:maintenance expected) (:maintenance origin))
         (= (case (:maintenance expected)
              (:lease-acquisition :expired-lease-reclamation) :leased
              :dispatch-acknowledgment :dispatch-accepted
              nil)
            (:status admission) (:gesso.live.outbox/status row))
         (= (:command-id admission) (:gesso.live.outbox/command-id row))
         (= source-id (:source-id origin) (:gesso.live.outbox/source-id row))
         (= (:worker-id origin) (:gesso.live.outbox/lease-owner row))
         (= :matched-scheduled-maintenance (:call-chain origin))
         (= :scheduled-local-cycle (:origin origin))
         (= :not-established (:authentication origin))
         (uuid? (:lifecycle-ref origin))
         (pos-int? (:cycle-number origin))
         (match-local-receipt? (:maintenance expected) admission row)
         (match-source-acceptance? source-id instance-id (:maintenance expected) admission))))

(defn- provider-worker-ops [maintenance ops]
  ;; XTDB 2.2.0-beta1 encodes the new Instant deadline in microseconds,
  ;; both in the two generated SQL guards and in the replacement document.
  ;; Project only this field of a lease acquisition/reclaim. Preserve the
  ;; original captured vector and all other fields, including old deadlines.
  (if (#{:lease-acquisition :expired-lease-reclamation} maintenance)
    (let [path [(dec (count ops)) 2 :gesso.live.outbox/lease-until]
          deadline (get-in ops path)
          guards (subvec ops 1 (dec (count ops)))]
      (when (and (instance? Instant deadline)
                 (= guards [["ASSERT CURRENT_TIMESTAMP < ?" deadline]
                            ["ASSERT CURRENT_TIMESTAMP + INTERVAL '5' MINUTE >= ?" deadline]]))
        (let [encoded (.truncatedTo ^Instant deadline ChronoUnit/MICROS)]
          (-> ops (assoc-in [1 1] encoded) (assoc-in [2 1] encoded)
              (assoc-in path encoded)))))
    ops))

(defn- compare-calls! [node source-id instance-id sample selected calls]
  (let [by-id (into {} (map (juxt :tx-id identity)) (:replay-inputs sample))]
    (if-not (and (vector? selected) (seq selected) (vector? calls)
                 (= (count selected) (count calls)))
      (report :mismatch :worker-capture-selection-mismatch)
      (loop [pairs (seq (map vector selected calls)) matched []]
        (if-let [[expected call] (first pairs)]
          (if-not (match-call? source-id instance-id expected (get by-id (:tx-id expected)) call)
            (report :mismatch :worker-capture-call-mismatch)
            (let [projected (provider-worker-ops (:maintenance expected) (:tx-ops call))
                  checked (if projected
                            (original-envelope-check node (:tx-id expected) projected true)
                            (report :mismatch :invalid-worker-deadline-projection))]
              (if-not (and (= :matched (:status checked)) (= (:tx-id expected) (:tx-id checked))
                           (true? (:committed? checked)))
                (report (if (#{:unsupported :unavailable :mismatch} (:status checked))
                          (:status checked) :unsupported)
                        (or (:reason checked) :invalid-worker-envelope-comparison))
                (recur (next pairs)
                       (conj matched (cond-> (assoc expected :committed? true
                                                    :system-time (get-in by-id [(:tx-id expected) :system-time])
                                                    :worker-origin (get-in call [:admission :worker-origin]))
                                       (= :dispatch-acknowledgment (:maintenance expected))
                                       (assoc :local-dispatch-receipt (get-in call [:admission :local-dispatch-receipt])
                                              :local-source-acceptance (get-in call [:admission :local-source-acceptance]))))))))
          (assoc (report :matched nil)
                 :maintenance-correspondence :matched-issued-worker-retained-transactions
                 :payload-correspondence :matched-xtdb-microsecond-worker-envelope
                 :dispatch-receipt-correspondence (if (some :local-dispatch-receipt matched)
                                                    :matched-local-handoff-retained-acknowledgments
                                                    :not-applicable)
                 :source-acceptance-correspondence (if (some :local-source-acceptance matched)
                                                     :matched-local-source-return-retained-acknowledgments
                                                     :not-applicable)
                 :selected-maintenance matched))))))

(defn compare-issued-selection!
  "Match a supplied selection to an exact host-issued capture and issued
   retained sample. The caller must independently establish the selection
   and bracket the complete sample. This descriptive comparison alone does
   not establish which maintenance transactions belong to a recovery cut."
  [system retained-acquisition source-id selected worker-capture]
  (let [node (:xtdb/connectable system)]
    (cond
      (not (boundaries-current?)) (report :unsupported :worker-comparison-boundary-replaced)
      (not (original-host-matcher system worker-capture)) (report :unsupported :unissued-worker-capture)
      :else
      (if-let [sample (original-acquisition-reader node retained-acquisition)]
        (let [checked (compare-calls! node source-id
                                      (get-in system [:gesso.live.outbox/managed-handle :system :source :id])
                                      sample selected (:calls worker-capture))]
          (cond
            (not (boundaries-current?)) (report :unsupported :worker-comparison-boundary-replaced)
            (not (original-host-matcher system worker-capture)) (report :unsupported :unissued-worker-capture)
            (= :matched (:status checked))
            (assoc checked :retained-payload-fingerprint (:payload-fingerprint sample)
                   :worker-observation-provenance :matched-local-issued-host-capture
                   :worker-lineage-correspondence :matched-issued-managed-lineage)
            :else checked))
        (report :unsupported :unissued-retained-acquisition)))))
