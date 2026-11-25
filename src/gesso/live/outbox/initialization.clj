(ns gesso.live.outbox.initialization
  "Durable one-time initialization for the selected single-node host.
   Commit the seed and its exact source-relative evidence marker atomically.
   This is not migration, physical refinement or protection against raw writers."
  (:require [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.outbox.read-frontier :as frontier]
            [xtdb.basis :as basis])
  (:import [java.time ZonedDateTime]))

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

(defn- marker-time! [rows source-id checked]
  (if (= [] rows)
    nil
    (let [row (first rows)
          time (:xt/system-from row)]
      (when-not (and (vector? rows) (= 1 (count rows)) checked
                     (instance? ZonedDateTime time)
                     (= (marker source-id checked) (dissoc row :xt/system-from)))
        (throw (ex-info "Incompatible durable initialization marker."
                        {:reason :initialization-evidence-mismatch :source-id source-id})))
      (.toInstant ^ZonedDateTime time))))

(def ^:private marker-query
  "SELECT *, _system_from FROM gesso_initializations WHERE _id = ?")

(defn initialized?
  "An exact current marker is required on restart. A changed catalog,
   mapping, seed, legacy marker or omitted initializer is not migration.
   A missing marker is not evidence of historical absence."
  [node source-id checked]
  (some? (marker-time! (frontier/q-from! node [marker-query source-id]) source-id checked)))

(defn transaction
  "Build the exact guarded initialization transaction from the selected
   seed and checked report. The installed policy independently derives both."
  [source-id seed mapped-tables checked]
  (when seed
    (let [tables (sort (into (into #{:gesso_commands :gesso_live_outbox :gesso_transition_evidence} mapped-tables) (map second) seed))]
      (into (into [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_initializations WHERE _id = ?)" source-id]]
                  (map (fn [t] [(str "ASSERT 0 = (SELECT COUNT(*) FROM " (name t) ")")]) tables))
            (conj seed [:put-docs table (marker source-id checked)])))))

(defn- readback!
  [node source-id mapped-tables checked time token]
  ;; Fix system and valid time for every query, including absent tables.
  ;; A later mutation must not change the observation of initialization.
  (let [opts (assoc (frontier/query-options! node)
                    :snapshot-token token :current-time time)
        read! #(xtdb/q-from node % opts)
        observed-time (marker-time! (read! [marker-query source-id]) source-id checked)]
    (when-not (= time observed-time)
      (throw (ex-info "Initialization marker is not at the observed commit time."
                      {:reason :initialization-readback-marker-mismatch})))
    (let [journal (read! ["SELECT * FROM gesso_commands"])
          outbox (read! ["SELECT * FROM gesso_live_outbox"])]
      (when-not (and (= [] journal) (= [] outbox)
                     (= [] (read! ["SELECT * FROM gesso_transition_evidence"])))
        (throw (ex-info "Initialization did not establish empty infrastructure."
                        {:reason :initialization-readback-infrastructure-mismatch})))
      {:system-time time
       :document-rows (into {} (map (fn [table]
                                      (when-not (and (simple-keyword? table)
                                                     (re-matches #"[a-z][a-z0-9_]*" (name table)))
                                        (throw (ex-info "Invalid readback table."
                                                        {:reason :invalid-initialization-readback-table})))
                                      [table (read! [(str "SELECT * FROM " (name table))])]))
                            (sort mapped-tables))
       :journal-rows journal})))

(defn initialize-and-readback!
  "Read all mapped tables and empty infrastructure at the seed commit's
   system and valid time. Fresh initialization uses the actual execute
   receipt; restart uses the exact durable marker's physical system-from.
   No transaction id is inferred on restart. The caller must still project
   and compare the rows with the issued initial constructor. A seedless
   unmarked host returns nil and has no initialization evidence."
  [node source-id seed mapped-tables checked]
  (let [prior (marker-time! (frontier/q-from! node [marker-query source-id]) source-id checked)]
    (cond
      prior
      (readback! node source-id mapped-tables checked prior
                 (basis/->time-basis-str {xtdb/default-database-name [prior]}))

      seed
      (let [receipt (xtdb/execute-tx! node (transaction source-id seed mapped-tables checked))
            committed (xtdb/tx-result-basis (:tx-result receipt))]
        (when-not committed
          (throw (ex-info "Initialization has no complete commit basis."
                          {:reason :missing-initialization-commit-basis})))
        (readback! node source-id mapped-tables checked
                   (xtdb/basis-system-time committed) (xtdb/basis-snapshot-token committed)))

      :else nil)))

(defn initialize!
  "Compatibility entry point; check the durable evidence and read back the
   selected physical state, but do not claim semantic correspondence."
  [node source-id seed mapped-tables checked]
  (initialize-and-readback! node source-id seed mapped-tables checked)
  nil)
