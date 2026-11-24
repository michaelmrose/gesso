(ns gesso.live.outbox.recovery-audit
  "Source-scoped, read-only inventory of durable outbox debt after host loss.

   Unlike eligible-work discovery, a cold host's recovery audit includes
   pending intents, still-leased or expired intents, and terminal XTDB
   acknowledgments. It does not need an in-memory interrupted-cycle report.
   Its expiry observation uses XTDB query-time CURRENT_TIMESTAMP, not the
   caller's clock, but is not transaction-time reclaim authority.

   Pagination is bounded and ordered by the physical unsigned UUID keyset.
   Each page awaits a newly captured submitted log frontier on its selected
   node. A page is one current read, NOT a snapshot of other pages or a claim that
   the outbox did not change during scanning. Start a NEW scan from nil after
   any continuation if completeness matters. This API neither acquires nor
   sends nor acknowledges, does not authorize an installed source or client,
   and never proves browser adoption or exactly-once delivery."
  (:require [clojure.string :as str]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.read-frontier :as frontier])
  (:import [java.time ZonedDateTime]))

(def ^:private allowed-keys #{:source-id :limit :after})
(def ^:private default-page-size 64)
(def ^:private max-page-size 128)

(def ^:private read-sql
  (str "SELECT *, _system_from, "
       "(gesso$live$outbox$lease_until <= CURRENT_TIMESTAMP) "
       "AS outbox_lease_expired FROM gesso_live_outbox "
       "WHERE gesso$live$outbox$type = ? "
       "AND gesso$live$outbox$version = ? "
       "AND gesso$live$outbox$source_id = ? "))

(defn- reject! [reason details]
  (throw (ex-info "Invalid Live outbox cold-host recovery audit."
                  (assoc details :error/type
                         :gesso.live.outbox/invalid-recovery-audit
                         :reason reason))))

(defn- valid-worker? [x]
  (and (string? x) (not (str/blank? x))))

(defn- validate! [opts]
  (when-not (map? opts)
    (reject! :invalid-options {}))
  (when (seq (remove allowed-keys (keys opts)))
    (reject! :unknown-fields {:keys (vec (remove allowed-keys (keys opts)))}))
  (when-not (valid-worker? (:source-id opts))
    (reject! :invalid-source-id {}))
  (when-not (and (integer? (get opts :limit default-page-size))
                 (<= 1 (get opts :limit default-page-size) max-page-size))
    (reject! :invalid-limit {}))
  (when (and (contains? opts :after)
             (not (uuid? (:after opts))))
    (reject! :invalid-cursor {}))
  opts)

(defn- query [opts]
  (let [sql (str read-sql
                 (when (contains? opts :after) "AND _id > ? ")
                 "ORDER BY _id LIMIT "
                 (inc (get opts :limit default-page-size)))]
    (cond-> [sql outbox/intent-type outbox/intent-version (:source-id opts)]
      (contains? opts :after) (conj (:after opts)))))

(defn- valid-lease? [row]
  (and (uuid? (:gesso.live.outbox/lease-id row))
       (valid-worker? (:gesso.live.outbox/lease-owner row))
       (instance? ZonedDateTime (:gesso.live.outbox/lease-until row))))

(defn- observed! [source-id row]
  (when-not (and (map? row)
                 (uuid? (:xt/id row))
                 (= source-id (:gesso.live.outbox/source-id row))
                 (= outbox/intent-type (:gesso.live.outbox/type row))
                 (= outbox/intent-version (:gesso.live.outbox/version row))
                 (instance? ZonedDateTime (:xt/system-from row))
                 (qualified-keyword? (:gesso.live.outbox/operation row))
                 (qualified-keyword? (:gesso.live.outbox/purpose row)))
    (reject! :invalid-physical-intent {}))
  (let [id (:xt/id row)
        status (:gesso.live.outbox/status row)
        lease? (valid-lease? row)
        expires? (:outbox-lease-expired row)
        receipt (:gesso.live.outbox/dispatch-receipt-id row)]
    (when-not
     (case status
       :pending (and (nil? (:gesso.live.outbox/lease-id row))
                     (nil? (:gesso.live.outbox/lease-owner row))
                     (nil? (:gesso.live.outbox/lease-until row))
                     (nil? receipt))
       :leased (and lease? (boolean? expires?) (nil? receipt))
       :dispatch-accepted
       (and lease? (uuid? receipt)
            (qualified-keyword? (:gesso.live.outbox/dispatch-adapter row)))
       false)
      (reject! :invalid-physical-status {:intent-id id :status status}))
    (cond-> {:intent-id id
             :physical-status status
             :observed-system-from (:xt/system-from row)
             :observation
             (case status
               :pending :pending-unclaimed
               :leased (if expires? :expired-lease-at-read :active-lease-at-read)
               :dispatch-accepted :acknowledgment-recorded)}
      lease?
      (assoc :lease-id (:gesso.live.outbox/lease-id row)
             :lease-owner (:gesso.live.outbox/lease-owner row)
             :lease-until (:gesso.live.outbox/lease-until row))
      (= status :leased)
      (assoc :lease-expired-at-read? expires?)
      (= status :dispatch-accepted)
      (assoc :receipt-id receipt))))

(defn inspect-source-page!
  "Read one source-scoped, bounded, current XTDB page after a process crash.

   Options: {:source-id REQUIRED nonblank-string, :limit 1..128, :after UUID}.
   Default limit is 64; at most limit+1 physical rows are read. Returns a
   validated :entries vector, :has-more?, :next-after and :rescan-required?.

   :expired-lease-at-read is an observation only. A *later* XTDB CAS must
   establish actual provider-time expiry and ownership before dispatch.
   :acknowledgment-recorded says XTDB accepted a dispatch receipt, NOT that a
   client got an SSE frame. The result never contains a replay instruction.
   No credential from an audit page may be used as an acquired lease."
  [node opts]
  (validate! opts)
  (let [limit (get opts :limit default-page-size)
        rows (frontier/q-from! node (query opts))]
    (when-not (vector? rows)
      (reject! :invalid-physical-page {}))
    (when (> (count rows) (inc limit))
      (reject! :oversized-physical-page {}))
    ;; Check lookahead too; a malformed row must not mint a valid cursor.
    (let [observed (mapv #(observed! (:source-id opts) %) rows)
          ids (mapv :intent-id observed)
          ordered (cond->> ids (:after opts) (cons (:after opts)))]
      (when-not (every? (fn [[a b]] (neg? (compare (str a) (str b))))
                        (partition 2 1 ordered))
        (reject! :invalid-physical-order {}))
      (let [has-more? (> (count observed) limit)
            entries (vec (take limit observed))]
        {:source-id (:source-id opts)
         :entries entries
         :has-more? has-more?
         :next-after (when has-more? (:intent-id (peek entries)))
         :rescan-required? true}))))
