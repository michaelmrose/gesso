(ns gesso.live.outbox.publication-manifest
  "Persistent compatibility fence for the selected single-subject publication
   profile. This is not a migration tool or a fence against arbitrary writers."
  (:require [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.model :as model]
            [gesso.live.outbox.read-frontier :as frontier]))

(defn- keyword-parts [k]
  [(namespace k) (name k)])

(defn descriptor
  "Stable v1 description of the actual unconditional expansion vocabulary.
   Sort topic entries, preserve target order (including deduplication order),
   and encode portable keyword components and strings. No functions or process identities."
  [compiled subjects]
  (binding [*print-length* nil *print-level* nil *print-meta* false
            *print-dup* false *print-readably* true *print-namespace-maps* false]
    (pr-str
     (mapv (fn [[topic targets]]
             [(keyword-parts topic) (keyword-parts (get subjects topic))
              (mapv (fn [{:keys [scope]}]
                      (let [{:keys [topic label]} (model/scope-descriptor compiled scope)]
                        [(keyword-parts scope) (keyword-parts topic) label]))
                    targets)])
           (sort-by (comp keyword-parts key) (:graph compiled))))))

(defn- reject! [reason source-id]
  (throw (ex-info "Durable publication manifest is incompatible with startup."
                  {:error/type :gesso.live.outbox/invalid-publication-manifest
                   :reason reason :source-id source-id})))

(defn- startup-read-options! [node source-id]
  (try (frontier/query-options! node)
       (catch clojure.lang.ExceptionInfo e
         (if (= ::frontier/invalid-frontier (:error/type (ex-data e)))
           (reject! :invalid-startup-log-frontier source-id)
           (throw e)))))

(defn require-compatible!
  "Before starting Live or its worker, check the durable manifest for this
   source, waiting for the captured submitted log frontier even on hosts
   without a selected profile. Establish a new manifest only if no previous source intent exists;
   do not infer the meaning of old unclassified intents. A non-selected host
   must not consume a previously classified source. Establishment is guarded
   in one XTDB transaction; a competing establishment fails startup rather
   than retrying or overwriting. The manifest survives subsequent startup
   failure deliberately. This assumes exclusive single-node host ownership
   and trusted writers, not an online multi-deployment migration protocol."
  [node source-id selected]
  (let [opts (startup-read-options! node source-id)
        rows (xtdb/q node ["SELECT _id, profile_version, descriptor FROM gesso_publication_manifests WHERE _id = ?" source-id] opts)
        expected {:xt/id source-id :profile-version 1 :descriptor selected}]
    (when-not (and (vector? rows) (<= (count rows) 1))
      (reject! :invalid-manifest-observation source-id))
    (cond
      (seq rows)
      (when-not (and selected (= expected (first rows)))
        (reject! :publication-manifest-mismatch source-id))

      selected
      (xtdb/execute-tx!
       node [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_publication_manifests WHERE _id = ?)" source-id]
             ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE gesso$live$outbox$source_id = ?)" source-id]
             [:put-docs :gesso_publication_manifests expected]]))
    nil))
