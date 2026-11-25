(ns gesso.live.outbox.maintenance-policy
  "Independent exact-footprint checks for the selected host's infrastructure
   writes. Reads the current physical outbox row before admission; the CAS
   in the submitted transaction protects the interval after that read.
   A changed row before admission is refused locally, not reported as an
   XTDB abort. Receipt provenance remains a trusted worker obligation."
  (:require [gesso.live.outbox :as outbox]
            [gesso.live.outbox.read-frontier :as frontier])
  (:import [java.time Instant ZonedDateTime]))

(defn- manifest-admission [source-id descriptor ops]
  (when (and (string? descriptor)
             (= ops [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_publication_manifests WHERE _id = ?)" source-id]
                     ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE gesso$live$outbox$source_id = ?)" source-id]
                     [:put-docs :gesso_publication_manifests
                      {:xt/id source-id :profile-version 1 :descriptor descriptor}]]))
    {:kind :manifest}))

(defn- valid-lease? [row]
  (and (uuid? (:gesso.live.outbox/lease-id row))
       (string? (:gesso.live.outbox/lease-owner row))
       (instance? ZonedDateTime (:gesso.live.outbox/lease-until row))))

(defn- lease-replacement? [old row worker-id guards]
  (let [id (:xt/id old)
        status (:gesso.live.outbox/status old)
        lease-id (:gesso.live.outbox/lease-id row)
        deadline (:gesso.live.outbox/lease-until row)
        cas (case status
              :pending ["ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ?)"
                        id (:xt/system-from old)]
              :leased (when (and (valid-lease? old)
                                 (not= lease-id (:gesso.live.outbox/lease-id old)))
                        ["ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ? AND CURRENT_TIMESTAMP >= ?)"
                         id (:xt/system-from old) (:gesso.live.outbox/lease-until old)])
              nil)]
    (and cas (uuid? lease-id) (instance? Instant deadline)
         (= guards [cas ["ASSERT CURRENT_TIMESTAMP < ?" deadline]
                    ["ASSERT CURRENT_TIMESTAMP + INTERVAL '5' MINUTE >= ?" deadline]])
         (= row (assoc (dissoc old :xt/system-from)
                       :gesso.live.outbox/status :leased
                       :gesso.live.outbox/lease-id lease-id
                       :gesso.live.outbox/lease-owner worker-id
                       :gesso.live.outbox/lease-until deadline)))))

(defn- acknowledgment? [old row worker-id guards]
  (let [receipt (:gesso.live.outbox/dispatch-receipt-id row)
        adapter (:gesso.live.outbox/dispatch-adapter row)]
    (and (= :leased (:gesso.live.outbox/status old))
         (valid-lease? old)
         (= worker-id (:gesso.live.outbox/lease-owner old))
         (uuid? receipt) (qualified-keyword? adapter)
         (= guards [["ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ? AND CURRENT_TIMESTAMP < ?)"
                     (:xt/id old) (:xt/system-from old) (:gesso.live.outbox/lease-until old)]])
         (= row (assoc (dissoc old :xt/system-from)
                       :gesso.live.outbox/status :dispatch-accepted
                       :gesso.live.outbox/dispatch-receipt-id receipt
                       :gesso.live.outbox/dispatch-adapter adapter)))))

(defn- worker-admission [node source-id worker-id ops row]
  (let [id (:xt/id row) status (:gesso.live.outbox/status row)]
    (when (and (uuid? id) (= source-id (:gesso.live.outbox/source-id row))
               (= worker-id (:gesso.live.outbox/lease-owner row))
               (#{:leased :dispatch-accepted} status))
      (let [rows (frontier/q-from! node ["SELECT *, _system_from FROM gesso_live_outbox WHERE _id = ?" id])
            old (when (and (vector? rows) (= 1 (count rows))) (first rows))]
        (when (and (map? old) (= id (:xt/id old))
                   (= outbox/intent-type (:gesso.live.outbox/type old))
                   (= outbox/intent-version (:gesso.live.outbox/version old))
                   (= source-id (:gesso.live.outbox/source-id old))
                   (instance? ZonedDateTime (:xt/system-from old))
                   (case status
                     :leased (lease-replacement? old row worker-id (pop ops))
                     :dispatch-accepted (acknowledgment? old row worker-id (pop ops))))
          {:kind :worker :intent-id id :status status
           :command-id (:gesso.live.outbox/command-id row)})))))

(defn admit
  "Return an infrastructure classification only for an exact manifest
   establishment, acquisition/reclaim, or acknowledgment. Never dispatches,
   submits or reports a commit. No arbitrary maintenance callback."
  [node {:keys [source-id worker-id descriptor]} ops]
  (when (and (vector? ops) (seq ops))
    (let [op (peek ops)]
      (when (and (vector? op) (= 3 (count op)) (= :put-docs (first op)) (map? (peek op)))
        (case (second op)
          :gesso_publication_manifests (manifest-admission source-id descriptor ops)
          :gesso_live_outbox (worker-admission node source-id worker-id ops (peek op))
          nil)))))
