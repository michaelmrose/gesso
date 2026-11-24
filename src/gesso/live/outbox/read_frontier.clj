(ns gesso.live.outbox.read-frontier
  "Explicit submitted-log read frontier for selected single-node durable
   observations. Not a transaction snapshot or a proof of noncommit."
  (:require [gesso.live.consistency.xtdb :as xtdb]
            [xtdb.api :as xt]
            [xtdb.basis :as basis]))

(defn query-options!
  "Capture one node-owned submitted frontier and encode its await token.
   Fail closed on malformed observations; do not rely on remembered client
   await tokens, or substitute the latest completed snapshot."
  [node]
  (try
    (let [submitted (:latest-submitted-msg-ids (xt/status node))]
      (when-not (and (map? submitted) (contains? submitted xtdb/default-database-name)
                     (every? (fn [[db ids]]
                               (and (string? db) (seq db)
                                    (vector? ids) (seq ids) (every? integer? ids)))
                             submitted))
        (throw (ex-info "Cannot establish the durable submitted log frontier."
                        {:error/type ::invalid-frontier :reason :invalid-submitted-log-frontier})))
      {:await-token (basis/->tx-basis-str submitted)})
    (catch InterruptedException e
      (.interrupt (Thread/currentThread))
      (throw e))))

(defn q-from!
  "Read using a freshly captured frontier from the same selected connectable.
   The selected local-node profile requires a status-capable connectable.
   A query may see newer work, and absence does not exclude in-flight work."
  [ctx query]
  (let [node (xtdb/connectable-from ctx)]
    (try
      (xtdb/q-from node query (query-options! node))
      (catch InterruptedException e
        (.interrupt (Thread/currentThread))
        (throw e)))))

(defn q-with-basis!
  "Read a trusted projection query at one explicit completed transaction basis.
   First await the submitted frontier on the selected local node, then obtain
   a completed basis from that same node. Pin the actual query to that basis,
   retaining the await token. Returns {:basis ... :rows ...}; never invents
   a basis for an empty node. This is a single-query system-time observation,
   not a multi-query valid-time snapshot or proof of application projection."
  [ctx query]
  (let [node (xtdb/connectable-from ctx)]
    (try
      (let [opts (query-options! node)
            ready (xtdb/q-from node ["SELECT 1 AS ready"] opts)]
        (when-not (= [{:ready 1}] ready)
          (throw (ex-info "Cannot establish projection readiness."
                          {:error/type ::invalid-projection-read
                           :reason :invalid-readiness-observation})))
        (let [completed (xtdb/latest-completed-basis node)]
          (when-not completed
            (throw (ex-info "No completed transaction basis for projection."
                            {:error/type ::invalid-projection-read
                             :reason :missing-completed-basis})))
          {:basis completed
           :rows (xtdb/q-from node query
                              (assoc opts :snapshot-token
                                     (xtdb/basis-snapshot-token completed)))}))
      (catch InterruptedException e
        (.interrupt (Thread/currentThread))
        (throw e)))))
