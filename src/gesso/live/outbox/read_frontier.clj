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
