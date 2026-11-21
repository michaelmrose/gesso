(ns gesso.live.outbox.discovery
  "Bounded, provider-time discovery of persisted single-node Live work.

   A newly started worker can enumerate current pending publication intents
   and leases which have expired according to XTDB's query-time clock,
   without retaining or receiving intent IDs from a prior process.

   Discovery does NOT claim an intent, authorize a sender, or establish a
   snapshot-stable work queue. The caller must subsequently use the physical
   CAS in gesso.live.outbox.acquisition to claim or reclaim each candidate.
   Changes concurrent with a scan may move into/out of eligibility, and new
   IDs behind a cursor are only seen after a new sweep from its beginning.
   Source-scoped discovery is required: a worker never scans another installed
   source. Source identity is caller-declared and not an authorization proof.
   No delivery, acknowledgment, restart-persistence, or liveness guarantee is
   asserted by this query API."
  (:require [clojure.string :as str]
            [gesso.live.outbox :as outbox]
            [xtdb.api :as xt]))

(def ^:private max-page-size 128)
(def ^:private default-page-size 64)
(def ^:private allowed-keys #{:source-id :limit :after})

(def ^:private selection-sql
  ;; The namespaced XTQL :gesso.live.outbox/status column is addressed by
  ;; its XTDB SQL-mapped identifier gesso$live$outbox$status. All SQL names
  ;; here are fixed framework literals; only values are caller parameters.
  (str "SELECT _id, gesso$live$outbox$status AS outbox_status, gesso$live$outbox$source_id AS outbox_source_id "
       "FROM gesso_live_outbox "
       "WHERE gesso$live$outbox$type = ? "
       "AND gesso$live$outbox$version = ? "
       "AND gesso$live$outbox$source_id = ? "
       "AND (gesso$live$outbox$status = ? "
       "OR (gesso$live$outbox$status = ? "
       "AND gesso$live$outbox$lease_until <= CURRENT_TIMESTAMP)) "))

(defn- reject!
  [reason info]
  (throw (ex-info "Invalid Gesso Live outbox discovery request."
                  (assoc info :error/type :gesso.live.outbox/invalid-discovery
                         :reason reason))))

(defn- validate-options!
  [opts]
  (when-not (map? opts)
    (reject! :invalid-options {:value opts}))
  (when-let [unknown (seq (remove allowed-keys (keys opts)))]
    (reject! :unknown-fields {:fields (vec unknown)}))
  (when-not (and (string? (:source-id opts))
                 (not (str/blank? (:source-id opts))))
    (reject! :invalid-source-id {:source-id (:source-id opts)}))
  (let [limit (get opts :limit default-page-size)]
    (when-not (and (integer? limit) (<= 1 limit max-page-size))
      (reject! :invalid-limit {:limit limit})))
  (when (and (contains? opts :after) (not (uuid? (:after opts))))
    (reject! :invalid-cursor {:cursor (:after opts)}))
  opts)

(defn- query-args
  [opts]
  (cond-> [outbox/intent-type outbox/intent-version (:source-id opts)
           :pending :leased]
    (contains? opts :after) (conj (:after opts))))

(defn- query-sql
  [{:keys [limit] :as opts}]
  (str selection-sql
       (when (contains? opts :after) "AND _id > ? ")
       ;; :limit is a validated small integer, never untrusted SQL text.
       "ORDER BY _id LIMIT " (inc (or limit default-page-size))))

(defn- candidate!
  [source-id row]
  (when-not (and (map? row)
                 (uuid? (:xt/id row))
                 (= source-id (:outbox-source-id row))
                 (#{:pending :leased} (:outbox-status row)))
    (reject! :invalid-physical-candidate {:row row}))
  {:intent-id (:xt/id row)
   :action (if (= :pending (:outbox-status row))
             :acquire-pending
             :reclaim-expired)})

(defn discover-eligible!
  "Read one bounded page of eligible persisted publication intent IDs.

   Options are {:source-id REQUIRED-nonblank-string :limit 1..128
   :after UUID}, with default limit 64 and no cursor. The source constraint
   is evaluated by XTDB before pagination. No unscoped discovery is allowed.
   Only version-1 outbox records with :pending status, or
   :leased records expired by XTDB CURRENT_TIMESTAMP, qualify. Delivered or
   nonmatching records never qualify. The query runs against XTDB's CURRENT
   view and orders by _id; its values are passed as SQL parameters.

   Returns {:candidates [{:intent-id UUID :action :acquire-pending |
   :reclaim-expired} ...] :has-more? boolean :next-after UUID-or-nil}.
   The next cursor is supplied only if another page was visible at query time.
   When a sweep ends, the caller MUST restart from no cursor to discover
   work inserted or made eligible behind its cursor.

   Source equality is rechecked during worker acquisition against the actual
   physical row, with the same system-time CAS, in case the source changed
   after discovery. The result is an advisory *discovery*, not a claim.
   Only a later guarded acquire-pending! or reclaim-expired! transaction
   establishes ownership;
   stale discoveries may safely become unavailable or contended. There is no
   snapshot spanning multiple pages, guarantee of fair scheduling, or proof
   of durability across a physical process restart in this revision."
  ([node] (discover-eligible! node {}))
  ([node opts]
   (validate-options! opts)
   (let [limit (get opts :limit default-page-size)
         rows (xt/q node (into [(query-sql opts)] (query-args opts)))
         has-more? (> (count rows) limit)
         candidates (mapv #(candidate! (:source-id opts) %) (take limit rows))]
     (when-not (= (count candidates)
                  (count (distinct (map :intent-id candidates))))
       (reject! :duplicate-current-identities {:candidates candidates}))
     {:candidates candidates
      :has-more? has-more?
      :next-after (when has-more? (:intent-id (peek candidates)))})))
