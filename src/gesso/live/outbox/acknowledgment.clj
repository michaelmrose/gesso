(ns gesso.live.outbox.acknowledgment
  "Fenced recording of an adapter-reported Live outbox dispatch acceptance.

   This API is for a trusted single-node delivery worker AFTER its adapter
   reports accepting a notification for dispatch. It does not call that
   adapter, establish its authenticity, prove an SSE frame was transmitted,
   or claim that a browser observed the change. The terminal state is
   :dispatch-accepted, deliberately not :delivered.

   Before any write, validate the exact lease token, worker, original
   publication identity and bounded receipt shape. Then use an XTDB
   system-time CAS and provider-time lease guard in the SAME transaction as
   the terminal state change. A reclaimed or expired lease cannot commit
   the acceptance. A failure to receive a definitive transaction result
   is propagated, never reclassified as an unambiguous rejection.

   A crash after an adapter sends but before this state commits can produce
   a repeated notification after recovery. This is at-least-once-compatible
   bookkeeping, not exactly-once external delivery or source certification."
  (:require [clojure.string :as str]
            [gesso.live.consistency.xtdb :as live.xtdb]
            [gesso.live.outbox :as outbox]
            [xtdb.api :as xt])
  (:import [java.time ZonedDateTime]))

(def ^:private submission-keys
  #{:intent-id :lease-id :worker-id :receipt})
(def ^:private receipt-keys
  #{:receipt-id :adapter :result})
(def ^:private current-sql
  "SELECT *, _system_from FROM gesso_live_outbox WHERE _id = ?")
(def ^:private active-lease-guard-sql
  (str "ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox "
       "WHERE _id = ? AND _system_from = ? AND CURRENT_TIMESTAMP < ?)"))

(defn- reject!
  [reason info]
  (throw (ex-info "Invalid Gesso Live outbox dispatch acknowledgment."
                  (assoc info
                         :error/type :gesso.live.outbox/invalid-acknowledgment
                         :reason reason))))

(defn- validate-submission!
  [submission]
  (when-not (map? submission)
    (reject! :invalid-submission {:value submission}))
  (when-let [extra (seq (remove submission-keys (keys submission)))]
    (reject! :unknown-fields {:fields (vec extra)}))
  (when-not (uuid? (:intent-id submission))
    (reject! :invalid-intent-id {:value (:intent-id submission)}))
  (when-not (uuid? (:lease-id submission))
    (reject! :invalid-lease-id {:value (:lease-id submission)}))
  (when-not (and (string? (:worker-id submission))
                 (not (str/blank? (:worker-id submission))))
    (reject! :invalid-worker-id {:value (:worker-id submission)}))
  (let [receipt (:receipt submission)]
    (when-not (map? receipt)
      (reject! :invalid-receipt {:value receipt}))
    (when-let [extra (seq (remove receipt-keys (keys receipt)))]
      (reject! :unknown-receipt-fields {:fields (vec extra)}))
    (when-not (uuid? (:receipt-id receipt))
      (reject! :invalid-receipt-id {:value (:receipt-id receipt)}))
    (when-not (qualified-keyword? (:adapter receipt))
      (reject! :invalid-adapter {:value (:adapter receipt)}))
    (when-not (= :accepted (:result receipt))
      (reject! :not-accepted {:value (:result receipt)})))
  submission)

(defn- current-intent!
  [node intent-id]
  (let [rows (xt/q node [current-sql intent-id])]
    (when (> (count rows) 1)
      (reject! :ambiguous-current-row {:intent-id intent-id}))
    (first rows)))

(defn- require-physical-intent!
  [intent intent-id]
  (when-not (and (= intent-id (:xt/id intent))
                 (= outbox/intent-type (:gesso.live.outbox/type intent))
                 (= outbox/intent-version (:gesso.live.outbox/version intent))
                 (instance? ZonedDateTime (:xt/system-from intent))
                 (string? (:gesso.live.outbox/source-id intent))
                 (not (str/blank? (:gesso.live.outbox/source-id intent)))
                 (qualified-keyword? (:gesso.live.outbox/operation intent))
                 (qualified-keyword? (:gesso.live.outbox/purpose intent)))
    (reject! :invalid-physical-intent {:intent-id intent-id}))
  intent)

(defn- require-physical-lease!
  [intent intent-id]
  (when-not (and (uuid? (:gesso.live.outbox/lease-id intent))
                 (string? (:gesso.live.outbox/lease-owner intent))
                 (not (str/blank? (:gesso.live.outbox/lease-owner intent)))
                 (instance? ZonedDateTime (:gesso.live.outbox/lease-until intent)))
    (reject! :invalid-physical-lease {:intent-id intent-id}))
  intent)

(defn acknowledge-dispatch!
  "Record a trusted adapter's reported acceptance under the live XTDB lease.

   Submission is a closed map:
   {:intent-id UUID :lease-id UUID :worker-id nonblank-string
    :receipt {:receipt-id UUID :adapter qualified-keyword :result :accepted}}.

   Only a CURRENT, version-1 :leased intent with identical worker and lease
   may transition to :dispatch-accepted. The actual XTDB transaction must
   still see the same system-time revision and an unexpired lease, otherwise
   it rejects with :xtdb/assert-failed and this returns :contended. Existing
   terminal/non-leased records return :unavailable, while a mismatched
   lease token is rejected as :stale-lease.

   The durable record preserves original intent, lease identity, and source,
   and adds only receipt ID and adapter classification. The receipt is an
   assertion from a TRUSTED caller, not verified adapter provenance; it must
   never be populated directly from untrusted HTTP request parameters.
   The result :acknowledged refers only to committing this state transition.
   No delivery to browser, client acknowledgment, or exactly-once claim is
   made. Unknown or uncertain physical transaction outcomes propagate."
  [node submission]
  (validate-submission! submission)
  (let [{:keys [intent-id lease-id worker-id receipt]} submission
        row (current-intent! node intent-id)]
    (cond
      (nil? row)
      {:status :unavailable :reason :missing :intent-id intent-id}

      :else
      (let [intent (require-physical-intent! row intent-id)]
        (if (not= :leased (:gesso.live.outbox/status intent))
          {:status :unavailable :reason :not-leased :intent-id intent-id}
          (do
            (require-physical-lease! intent intent-id)
            (when-not (and (= lease-id (:gesso.live.outbox/lease-id intent))
                           (= worker-id (:gesso.live.outbox/lease-owner intent)))
              (reject! :stale-lease {:intent-id intent-id}))
            (let [updated (-> intent
                              (dissoc :xt/system-from)
                              (assoc :gesso.live.outbox/status :dispatch-accepted
                                     :gesso.live.outbox/dispatch-receipt-id
                                     (:receipt-id receipt)
                                     :gesso.live.outbox/dispatch-adapter
                                     (:adapter receipt)))
                  ops [[active-lease-guard-sql
                        intent-id (:xt/system-from intent)
                        (:gesso.live.outbox/lease-until intent)]
                       [:put-docs outbox/intent-table updated]]]
              (try
                (merge (live.xtdb/execute-tx! node ops)
                       {:status :acknowledged
                        :intent-id intent-id
                        :lease-id lease-id
                        :receipt-id (:receipt-id receipt)})
                (catch Throwable cause
                  ;; A physical transaction may have cleared the interrupted
                  ;; flag before throwing. Preserve cancellation even while
                  ;; retaining the exact exception and uncertain outcome.
                  (when (instance? InterruptedException cause)
                    (.interrupt (Thread/currentThread)))
                  (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data cause)))
                    {:status :contended :intent-id intent-id}
                    (throw cause)))))))))))
