(ns gesso.live.outbox.writer-policy
  "Restricted synchronous adapter policy for a selected mapped executor.
   Domain submissions require a genuine framed handoff; initialization is
   exact and one-use. Maintenance is confined to a single infrastructure
   put with framework-owned exact-footprint and physical row checks.
   This is local adapter mediation, not closure over raw XTDB writers."
  (:require [gesso.live.outbox.model-publication :as publication]
            [gesso.live.outbox.maintenance-policy :as maintenance]
            [gesso.model.inductive-plan :as inductive]))

(def ^:private maintenance-kinds
  {:gesso_publication_manifests :manifest
   :gesso_live_outbox :worker})

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
                     (not= :gesso_commands (second %))
                     (not (contains? maintenance-kinds (second %)))) ops)))

(defn validate-config!
  "Validate the exact selected policy inputs before acquiring a node.
   The optional initializer is an exact trusted vector, not a general
   writer grant. The observer is trusted."
  [{:keys [admission initializer observe!] :as config}]
  (when-not (and (map? config)
                 (= #{:admission :initializer :observe!} (set (keys config)))
                 (or (nil? initializer) (initializer? initializer))
                 (fn? observe!))
    (reject! :invalid-mapped-writer-policy))
  (inductive/describe-mapped-admission admission)
  config)

(defn build-policy!
  "Bind admission to the owned connectable. Return the literal-boolean
   predicate expected by the XTDB adapter. Maintenance is bound to the
   host-owned source, worker and descriptor. Observer failure refuses
   submission. Initialization is consumed on admission, not on commit."
  [connectable config infrastructure]
  (let [{:keys [admission initializer observe!]} (validate-config! config)
        initialized? (atom false)]
    (when-not connectable (reject! :missing-mapped-writer-connectable))
    (fn [{:keys [api tx-ops tx-options] :as envelope}]
      (boolean
       (when (and (= :execute-tx api) (empty? tx-options)
                  (vector? tx-ops) (seq tx-ops))
         (let [kind (or (when-let [{:keys [identity candidate]}
                                  (publication/claim-framed-submission! admission connectable envelope)]
                          (assoc identity :kind :mapped-operation :candidate candidate))
                        (when (and initializer (= initializer tx-ops)
                                   (compare-and-set! initialized? false true))
                          {:kind :initializer})
                        (maintenance/admit connectable infrastructure tx-ops))]
           (when kind
             (observe! kind)
             true)))))))
