(ns gesso.live.outbox.worker
  "Bounded, synchronous single-node sweep for advisory outbox discovery and
   physically guarded XTDB acquisition. It DOES NOT dispatch or acknowledge
   notifications, spawn a background worker, or claim that its scan is a
   snapshot of the entire queue.

   A trusted host provides BOTH :worker-id and a mandatory :source-id.
   Discovery filters by that source before pagination, and the physical
   acquisition checks it again under a system-time CAS to close read races.
   Candidate IDs come only from XTDB's validated discovery API. Each attempt receives a fresh lease token; only
   an actual successful acquisition/reclaim makes that token usable. The
   underlying XTDB transaction (not the host clock) validates lease bounds.

   A sweep is bounded by :page-size and :max-pages, and may be resumed using
   :next-after only when :halt-reason is :page-budget. New or newly eligible
   entries behind the cursor require a subsequent sweep from the beginning.
   Errors and indeterminate outcomes propagate: a caller MUST NOT assume an
   interrupted sweep left every earlier acquisition uncommitted."
  (:require [clojure.string :as str]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.discovery :as discovery])
  (:import [java.time Instant]))

(def ^:private allowed-keys
  #{:worker-id :source-id :lease-millis :page-size :max-pages :after})
(def ^:private default-lease-millis 60000)
(def ^:private default-page-size 32)
(def ^:private default-max-pages 4)
(def ^:private max-lease-millis 300000)
(def ^:private max-page-size 128)
(def ^:private max-pages-limit 16)

(defn- reject!
  [reason data]
  (throw (ex-info "Invalid Gesso Live outbox worker request."
                  (assoc data
                         :error/type :gesso.live.outbox/invalid-worker-request
                         :reason reason))))

(defn- validate-options!
  [opts]
  (when-not (map? opts)
    (reject! :invalid-options {:options opts}))
  (when-let [unknown (seq (remove allowed-keys (keys opts)))]
    (reject! :unknown-fields {:fields (vec unknown)}))
  (when-not (and (string? (:worker-id opts))
                 (not (str/blank? (:worker-id opts))))
    (reject! :invalid-worker-id {:worker-id (:worker-id opts)}))
  (when-not (and (string? (:source-id opts))
                 (not (str/blank? (:source-id opts))))
    (reject! :invalid-source-id {:source-id (:source-id opts)}))
  (let [duration (get opts :lease-millis default-lease-millis)
        page-size (get opts :page-size default-page-size)
        max-pages (get opts :max-pages default-max-pages)]
    (when-not (and (integer? duration) (<= 1 duration max-lease-millis))
      (reject! :invalid-lease-millis {:lease-millis duration}))
    (when-not (and (integer? page-size) (<= 1 page-size max-page-size))
      (reject! :invalid-page-size {:page-size page-size}))
    (when-not (and (integer? max-pages) (<= 1 max-pages max-pages-limit))
      (reject! :invalid-max-pages {:max-pages max-pages})))
  (when (and (contains? opts :after) (not (uuid? (:after opts))))
    (reject! :invalid-cursor {:after (:after opts)}))
  opts)

(defn- ascending-uuid-identities?
  "Verify that every returned identity advances the XTDB SQL keyset cursor.
   SQL UUID ordering follows the unsigned canonical UUID representation;
   Java UUID.compareTo sorts its signed long fields and differs at 0x8000..."
  [after identities]
  (let [ordered (cond->> identities after (cons after))]
    (every? (fn [[left right]]
              (neg? (compare (str left) (str right))))
            (partition 2 1 ordered))))

(defn- validate-discovery-page!
  "Fail closed if a discovery response violates its mandatory page contract.
   Validate the ENTIRE advisory page before its first physical acquisition,
   so malformed later candidates cannot leave an unreported committed prefix."
  [request page]
  (let [candidates (:candidates page)
        more? (:has-more? page)
        cursor (:next-after page)
        identities (when (vector? candidates)
                     (mapv :intent-id candidates))]
    (when-not
     (and (map? page)
          (vector? candidates)
          (<= (count candidates) (:limit request))
          (every? (fn [candidate]
                    (and (map? candidate)
                         (uuid? (:intent-id candidate))
                         (#{:acquire-pending :reclaim-expired}
                          (:action candidate))))
                  candidates)
          (= (count identities) (count (distinct identities)))
          ;; A page behind :after, or in decreasing order, can silently skip
          ;; work or drive a paged sweep backward even if its cursor differs.
          ;; Validate against the same ascending UUID order as XTDB's SQL.
          (ascending-uuid-identities? (:after request) identities)
          (or (true? more?) (false? more?))
          (if more?
            (and (= (:limit request) (count candidates))
                 (uuid? cursor)
                 (= cursor (peek identities))
                 (not= cursor (:after request)))
            (nil? cursor)))
      (reject! :invalid-discovery-page
               {:request request :page page}))
    page))

(defn- attempt!
  [node worker-id source-id lease-millis {:keys [intent-id action]}]
  (let [now (Instant/now)
        claim {:intent-id intent-id
               :source-id source-id
               :worker-id worker-id
               :lease-id (random-uuid)
               :now now
               :lease-until (.plusMillis now (long lease-millis))}
        outcome
        (case action
          :acquire-pending (acquisition/acquire-pending! node claim)
          :reclaim-expired (acquisition/reclaim-expired! node claim)
          (reject! :unknown-action {:action action :intent-id intent-id}))
        status (:status outcome)]
    ;; An unrecognized result must not be treated as safely unavailable.
    (when-not (or (and (= action :acquire-pending) (= :acquired status))
                  (and (= action :reclaim-expired) (= :reclaimed status))
                  (#{:contended :unavailable} status))
      (reject! :unexpected-acquisition-result
               {:intent-id intent-id :action action :status status}))
    (assoc outcome :action action)))

(defn acquire-sweep!
  "Discover and attempt to acquire a finite number of current outbox intents.

   Trusted options: {:worker-id nonblank-string, :source-id REQUIRED
   nonblank-string, :lease-millis 1..300000,
   :page-size 1..128, :max-pages 1..16, :after optional-UUID}.
   Defaults: 60-second lease, 32 candidates/page, 4 pages/sweep. No sleep,
   timer, delivery callback, or automatic retry is installed.

   Each discovery page is read from XTDB; its candidates undergo exactly one
   CAS acquisition/reclaim attempt each. A failed/stale discovery is reported
   as :contended or :unavailable, never as delivery success. Results include
   lease credentials ONLY for successful physical acquisitions, and a finite
   :next-after cursor when the page budget ended before the observed end.
   :observed-end is NOT proof that the queue is empty across time.

   Any unexpected exception propagates, potentially AFTER earlier physical
   claims committed. The caller must rescan the database to reconcile an
   interrupted/indeterminate sweep rather than blindly resubmit effects.
   Neither receipt processing nor lease acknowledgment is implemented here."
  [node opts]
  (validate-options! opts)
  (let [{:keys [worker-id source-id]} opts
        lease-millis (get opts :lease-millis default-lease-millis)
        page-size (get opts :page-size default-page-size)
        max-pages (get opts :max-pages default-max-pages)]
    (loop [after (:after opts)
           remaining max-pages
           page-count 0
           results []]
      (let [request (cond-> {:limit page-size :source-id source-id}
                      after (assoc :after after))
            page (validate-discovery-page!
                  request (discovery/discover-eligible! node request))
            claimed (mapv #(attempt! node worker-id source-id lease-millis %)
                          (:candidates page))
            results' (into results claimed)
            count' (inc page-count)
            more? (:has-more? page)
            next-after (:next-after page)]
        (when (and more? (not (uuid? next-after)))
          (reject! :invalid-discovery-continuation
                   {:page-count count' :next-after next-after}))
        (if (and more? (> remaining 1))
          (recur next-after (dec remaining) count' results')
          {:worker-id worker-id
           :pages-scanned count'
           :attempted (count results')
           :acquired (count (filter #(= :acquired (:status %)) results'))
           :reclaimed (count (filter #(= :reclaimed (:status %)) results'))
           :contended (count (filter #(= :contended (:status %)) results'))
           :unavailable (count (filter #(= :unavailable (:status %)) results'))
           :results results'
           :halt-reason (if more? :page-budget :observed-end)
           :next-after (when more? next-after)})))))
