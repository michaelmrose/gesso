(ns gesso.live.outbox.initialization
  "Durable one-time initialization for the selected single-node host.
   Commit the seed and its exact source-relative evidence marker atomically.
   This is not migration, physical refinement or protection against raw writers."
  (:require [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.outbox.read-frontier :as frontier]))

(def table :gesso_initializations)
(def ^:private evidence-keys
  #{:catalog-fingerprint :case-fingerprint :initialization-fingerprint})

(defn marker
  "Build a data-only v2 marker from a checked initialization report. The
   installed policy, not this copyable record, supplies submission authority."
  [source-id checked]
  (let [evidence (select-keys checked evidence-keys)]
    (when-not (and (= evidence-keys (set (keys evidence)))
                   (every? #(and (string? %) (re-matches #"sha256:[0-9a-f]{64}" %))
                           (vals evidence)))
      (throw (ex-info "Incomplete initialization evidence."
                      {:reason :invalid-initialization-evidence})))
    (assoc evidence :xt/id source-id :profile-version 2)))

(defn initialized?
  "An exact current marker is required on restart. A changed catalog,
   mapping, seed, legacy marker or omitted initializer is not migration.
   A missing marker is not evidence of historical absence."
  [node source-id checked]
  (let [rows (frontier/q-from! node ["SELECT * FROM gesso_initializations WHERE _id = ?" source-id])]
    (cond
      (= [] rows) false
      (and checked (= [(marker source-id checked)] rows)) true
      :else (throw (ex-info "Incompatible durable initialization marker."
                            {:reason :initialization-evidence-mismatch :source-id source-id})))))

(defn transaction
  "Build the exact guarded initialization transaction from the selected
   seed and checked report. The installed policy independently derives both."
  [source-id seed mapped-tables checked]
  (when seed
    (let [tables (sort (into (into #{:gesso_commands :gesso_live_outbox} mapped-tables) (map second) seed))]
      (into (into [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_initializations WHERE _id = ?)" source-id]]
                  (map (fn [t] [(str "ASSERT 0 = (SELECT COUNT(*) FROM " (name t) ")")]) tables))
            (conj seed [:put-docs table (marker source-id checked)])))))

(defn initialize!
  "Check the durable evidence before starting services, even when the
   initializer was omitted. A seedless unmarked host is not certified."
  [node source-id seed mapped-tables checked]
  (let [initialized (initialized? node source-id checked)]
    (when (and seed (not initialized))
      (xtdb/execute-tx! node (transaction source-id seed mapped-tables checked))))
  nil)
