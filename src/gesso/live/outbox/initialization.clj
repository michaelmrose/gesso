(ns gesso.live.outbox.initialization
  "Durable one-time initialization for the selected single-node host.
   A source marker and exact seed writes commit atomically. Unmarked
   nonempty seed tables or current command/outbox rows refuse enrollment.
   This is not migration or protection against unmediated writers."
  (:require [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.outbox.read-frontier :as frontier]))

(def table :gesso_initializations)

(defn- marker [source-id]
  {:xt/id source-id :profile-version 1})

(defn initialized?
  "Await the node-owned submitted frontier and require an exact marker.
   An existing marker is permanent for this source; changed seed config
   is not a migration request and must never cause reinitialization."
  [node source-id]
  (let [rows (frontier/q-from! node ["SELECT * FROM gesso_initializations WHERE _id = ?" source-id])]
    (cond
      (= [] rows) false
      (= [(marker source-id)] rows) true
      :else (throw (ex-info "Invalid durable initialization marker."
                            {:reason :invalid-initialization-marker :source-id source-id})))))

(defn transaction
  "Build the exact guarded initialization transaction. The selected seed
   has already passed writer-policy validation of canonical put tables.
   This checks current emptiness, not historical absence or adoption of
   an existing application. The selected host owns the database."
  [source-id seed]
  (when seed
    (let [tables (sort (into #{:gesso_commands :gesso_live_outbox} (map second) seed))]
      (into (into [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_initializations WHERE _id = ?)" source-id]]
                  (map (fn [t] [(str "ASSERT 0 = (SELECT COUNT(*) FROM " (name t) ")")]) tables))
            (conj seed [:put-docs table (marker source-id)])))))

(defn initialize!
  "Called after manifest compatibility, before Live/worker/HTTP startup.
   Skip on a durable marker; otherwise execute the guarded seed and marker
   atomically. Any submission or assertion failure aborts host startup."
  [node source-id seed]
  (when (and seed (not (initialized? node source-id)))
    (xtdb/execute-tx! node (transaction source-id seed)))
  nil)
