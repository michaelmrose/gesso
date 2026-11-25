(ns gesso.live.outbox.writer-policy
  "Restricted synchronous adapter policy for a selected mapped executor.
   Domain submissions require a genuine framed handoff; initialization is
   exact and durably fenced. Maintenance is confined to a single infrastructure
   put with framework-owned exact-footprint and physical row checks.
   This is local adapter mediation, not closure over raw XTDB writers."
  (:require [gesso.live.outbox.model-publication :as publication]
            [gesso.live.outbox.maintenance-policy :as maintenance]
            [gesso.live.outbox.initialization :as initialization]
            [gesso.model.inductive-plan :as inductive]))

(def ^:private maintenance-kinds
  {:gesso_publication_manifests :manifest
   :gesso_live_outbox :worker})

(def ^:private reserved-tables
  (into #{:gesso_commands initialization/table} (keys maintenance-kinds)))

(defn- reject! [reason]
  (throw (ex-info "Invalid mapped writer policy." {:reason reason})))

(defn- put-op? [op]
  (and (vector? op) (<= 3 (count op)) (= :put-docs (first op))
       (simple-keyword? (second op))
       (re-matches #"[a-z][a-z0-9_]*" (name (second op)))
       (every? map? (subvec op 2))))

(defn- initializer? [ops]
  (and (vector? ops) (seq ops)
       (every? #(and (put-op? %)
                     (not (contains? reserved-tables (second %)))) ops)))

(defn validate-config!
  "Validate the exact selected policy inputs before acquiring a node.
   The optional initializer is an exact trusted vector, not a general
   writer grant. The framework adds the durable marker and guards.
   The observer is trusted."
  [{:keys [admission initializer observe!] :as config}]
  (when-not (and (map? config)
                 (= #{:admission :initializer :observe!} (set (keys config)))
                 (or (nil? initializer) (initializer? initializer))
                 (fn? observe!))
    (reject! :invalid-mapped-writer-policy))
  (let [{:keys [target-map auxiliary-tables]} (inductive/describe-mapped-admission admission)]
    ;; Domain coordinates cannot declare infrastructure as application state.
    ;; Only the canonical outbox is an auxiliary effect in this host profile;
    ;; the command journal has its own checked journal-slot coordinates.
    (when (some (fn [[kind table]]
                  (and (#{:document :nullable-document} kind)
                       (contains? reserved-tables table))) (vals target-map))
      (reject! :reserved-mapped-document-table))
    (when-not (every? #{:gesso_live_outbox} auxiliary-tables)
      (reject! :unsupported-mapped-auxiliary-table)))
  config)

(defn- mapped-operations? [ops]
  ;; The issuer checks assertion/put order and the exact original envelope.
  ;; At the installed boundary additionally require canonical table spellings
  ;; and keep manifest and initialization writes out of the mapped path.
  (let [[_ puts] (split-with #(and (vector? %) (string? (first %))) ops)]
    (and (seq puts)
         (every? #(and (put-op? %)
                       (not (#{initialization/table :gesso_publication_manifests}
                             (second %)))) puts))))

(defn build-policy!
  "Bind admission to the owned connectable. Return the literal-boolean
   predicate expected by the XTDB adapter. Maintenance is bound to the
   host-owned source, worker and descriptor. Observer failure refuses
   submission. Initialization is consumed locally on admission and
   durably on commit of the seed and its marker."
  [connectable config infrastructure]
  (let [{:keys [admission initializer observe!]} (validate-config! config)
        initialized? (atom false)
        source-id (:source-id infrastructure)
        seed-ops (initialization/transaction source-id initializer)]
    (when-not connectable (reject! :missing-mapped-writer-connectable))
    (fn [{:keys [api tx-ops tx-options] :as envelope}]
      (boolean
       (when (and (= :execute-tx api) (empty? tx-options)
                  (vector? tx-ops) (seq tx-ops))
         (let [kind (or (when-let [{:keys [identity candidate]}
                                  (when (mapped-operations? tx-ops)
                                    (publication/claim-framed-submission! admission connectable envelope))]
                          (assoc identity :kind :mapped-operation :candidate candidate))
                        (when (and seed-ops (= seed-ops tx-ops)
                                   (not @initialized?)
                                   (not (initialization/initialized? connectable source-id))
                                   (compare-and-set! initialized? false true))
                          {:kind :initializer})
                        (maintenance/admit connectable infrastructure tx-ops))]
           (when kind
             (observe! kind)
             true)))))))
