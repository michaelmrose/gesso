(ns gesso.live.outbox.history-coverage
  "Bounded, read-only audit of the selected mapped host's visible ledger.
   A mapped stutter is not a proof of an infrastructure writer's authority.
   No claim is made about valid-time evolution between ledger observations,
   hidden tables, transaction-internal reads, or external writer closure."
  (:require [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.outbox.physical-correspondence :as correspondence]
            [gesso.live.outbox.maintenance-policy :as maintenance]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.read-frontier :as frontier]
            [gesso.model.inductive-plan :as inductive]
            [xtdb.basis :as basis])
  (:import [java.time ZonedDateTime]))

(defn- reject! [reason]
  (throw (ex-info "Cannot establish bounded mapped history coverage."
                  {:reason reason})))

(defn- ledger-time [row]
  (when (instance? ZonedDateTime (:system-time row))
    (.toInstant ^ZonedDateTime (:system-time row))))

(defn- index! [rows]
  (when-not (and (vector? rows) (every? #(and (map? %) (some? (:xt/id %))) rows)
                 (= (count rows) (count (set (map :xt/id rows)))))
    (reject! :invalid-history-rows))
  (into {} (map (juxt :xt/id identity)) rows))

(defn- snapshot! [node tables opts system-time valid-time]
  (let [opts (assoc opts :snapshot-token (basis/->time-basis-str {xtdb/default-database-name [system-time]})
                         :current-time valid-time)
        read! (fn [table]
                (index! (xtdb/q-from node [(str "SELECT *, _system_from FROM " (name table))] opts)))]
    {:documents (into {} (map (fn [table] [table (read! table)])) tables)
     :journal (read! :gesso_commands)
     :evidence (read! :gesso_transition_evidence)
     :outbox (read! :gesso_live_outbox)
     :manifests (read! :gesso_publication_manifests)
     :initialization (read! :gesso_initializations)}))

(defn- plain-rows [index]
  (mapv #(dissoc % :xt/system-from) (vals index)))

(defn- docs [snapshot]
  (into {} (map (fn [[table rows]] [table (plain-rows rows)])) (:documents snapshot)))

(defn- appended-id [before after]
  (let [added (remove #(contains? before %) (keys after))]
    (when (and (= 1 (count added)) (= before (dissoc after (first added))))
      (first added))))

(defn- written-at? [time row]
  (let [written (:xt/system-from row)]
    (and (instance? ZonedDateTime written) (= time (.toInstant ^ZonedDateTime written)))))

(defn- publication-append? [source-id time command-id before after]
  (let [added (apply dissoc after (keys before))]
    (and (= before (apply dissoc after (keys added)))
         (every? (fn [[id row]]
                   (and (uuid? id) (written-at? time row)
                        (= outbox/intent-type (:gesso.live.outbox/type row))
                        (= outbox/intent-version (:gesso.live.outbox/version row))
                        (= :pending (:gesso.live.outbox/status row))
                        (= source-id (:gesso.live.outbox/source-id row))
                        (= command-id (:gesso.live.outbox/command-id row)))) added))))

(defn- worker-step [source-id time before after]
  (let [old (:outbox before) new (:outbox after)
        changed (filter #(not= (get old %) (get new %)) (keys old))]
    (when (and (= (dissoc before :outbox) (dissoc after :outbox))
               (= (set (keys old)) (set (keys new))) (= 1 (count changed)))
      (let [id (first changed) row (get old id)]
        (when (contains? (:journal before) (:gesso.live.outbox/command-id row))
          (when-let [kind (maintenance/observe-worker-transition source-id time row (get new id))]
            {:classification :verified-infrastructure-transition :maintenance kind :intent-id id}))))))

(defn- classify-step! [node admission source-id tables opts prior current]
  (let [time (ledger-time current)
        previous-time (ledger-time prior)
        before (snapshot! node tables opts previous-time time)
        after (snapshot! node tables opts time time)
        base {:tx-id (:xt/id current) :system-time time :committed? (:committed current)}
        id (appended-id (:journal before) (:journal after))
        worker (when (:committed current) (worker-step source-id time before after))]
    (cond
      (= before after)
      (assoc base :classification (if (:committed current) :observed-mapped-stutter :aborted-stutter))

      (not (:committed current))
      (assoc base :classification :unclassified :reason :aborted-step-changed-observed-rows)

      worker (merge base worker)

      (and id (= id (appended-id (:evidence before) (:evidence after)))
           (= (:initialization before) (:initialization after))
           (= (:manifests before) (:manifests after))
           (publication-append? source-id time id (:outbox before) (:outbox after)))
      (let [command (get-in after [:journal id])
            witness (get-in after [:evidence id])]
        (try
          (when-not (every? #(and (instance? ZonedDateTime %)
                                 (= time (.toInstant ^ZonedDateTime %)))
                            [(:xt/system-from command) (:xt/system-from witness)])
            (reject! :history-command-evidence-time-mismatch))
          (let [evidence (dissoc witness :xt/system-from)]
            (inductive/check-mapped-prestate! admission evidence (docs before) (plain-rows (:journal before)))
            (inductive/check-historical-mapped-poststate!
             admission (dissoc command :xt/system-from) evidence (docs after) (plain-rows (:journal after))))
          (assoc base :classification :verified-mapped-transition :command-id id)
          (catch clojure.lang.ExceptionInfo e
            (assoc base :classification :unclassified :reason (or (:reason (ex-data e)) :transition-check-failed)))))

      :else (assoc base :classification :unclassified :reason :unexplained-observed-row-change))))

(defn audit!
  "Audit at most max-steps (1..128) following the exact active policy's
   checked initialization through a pinned completed basis. Refuse an
   oversized or ambiguous interval rather than returning a partial success.
   Every ledger step is classified, including aborts and unexplained changes.
   Stutters compare mapped and infrastructure rows at the step's valid
   time. Worker replacements check the historical lease/receipt relation;
   neither establishes actual writer footprints."
  [connectable admission max-steps]
  (when-not (and (integer? max-steps) (<= 1 max-steps 128))
    (reject! :invalid-history-step-limit))
  (let [node (xtdb/connectable-from connectable)
        token (xtdb/current-submission-policy-token node)
        initial (correspondence/initialization-observation node admission)
        _ (when-not initial (reject! :missing-history-initialization))
        tables (into #{} (keep (fn [[kind table]]
                                 (when (#{:document :nullable-document} kind) table)))
                     (vals (:target-map (inductive/describe-mapped-admission admission))))
        _ (when-not (every? #(and (simple-keyword? %) (re-matches #"[a-z][a-z0-9_]*" (name %))) tables)
            (reject! :invalid-history-table))
        {:keys [basis rows]} (frontier/q-with-basis!
                             node [(str "SELECT _id, committed, system_time FROM xt.txs WHERE system_time >= ? ORDER BY _id LIMIT " (+ 2 max-steps))
                                   (:system-time initial)])
        times (mapv ledger-time rows)
        ids (mapv :xt/id rows)]
    (when (> (count rows) (inc max-steps)) (reject! :history-step-limit-exceeded))
    (when-not (and (vector? rows) (seq rows) (every? some? times)
                   (every? integer? ids) (every? #(boolean? (:committed %)) rows)
                   (true? (:committed (first rows))) (= (:system-time initial) (first times))
                   (= (xtdb/basis-system-time basis) (peek times)) (= (xtdb/basis-tx-id basis) (peek ids))
                   (every? (fn [[a b]] (< a b)) (partition 2 1 ids))
                   (every? (fn [[a b]] (neg? (compare a b))) (partition 2 1 times)))
      (reject! :ambiguous-history-ledger))
    (let [opts (frontier/query-options! node)
          steps (mapv (fn [[prior current]] (classify-step! node admission (:source-id initial) tables opts prior current))
                      (partition 2 1 rows))
          unresolved (filterv #(= :unclassified (:classification %)) steps)]
      (when-not (and (identical? token (xtdb/current-submission-policy-token node))
                     (= initial (correspondence/initialization-observation node admission)))
        (reject! :changed-history-policy))
      {:profile :selected-mapped-ledger-coverage-v1 :authority :none
       :status (if (seq unresolved) :unclassified-steps :observed-interval-checked)
       :initialization initial :through-basis basis :steps steps
       :open-obligations #{:transaction-internal-read-basis-refinement
                           :valid-time-continuity-between-steps
                           :infrastructure-writer-footprint-coverage
                           :historical-worker-and-receipt-authentication
                           :unrestricted-physical-writer-closure
                           :principal-authentication-and-business-authorization
                           :complete-application-refinement}})))
