(ns gesso.live.outbox.writer-policy
  "Restricted synchronous adapter policy for a selected mapped executor.
   Domain submissions require a genuine framed handoff; initialization is
   exact and durably fenced. Maintenance is confined to a single infrastructure
   put with framework-owned exact-footprint and physical row checks.
   This is local adapter mediation, not closure over raw XTDB writers."
  (:require [gesso.live.outbox.model-publication :as publication]
            [gesso.live.consistency.xtdb :as xtdb]
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
  (and (vector? ops)
       (every? #(and (put-op? %)
                     (not (contains? reserved-tables (second %)))) ops)))

(defn validate-config!
  "Validate the exact selected policy inputs before acquiring a node.
   The initializer is an explicit vector (possibly empty), not a general
   writer grant. The framework checks Init and adds the durable marker
   and guards. An omitted initializer is not an enrollment mechanism.
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
  (when (nil? initializer) (reject! :missing-mapped-initializer))
  (inductive/check-mapped-initialization! admission initializer)
  config)

(defn- mapped-tables [admission]
  (into #{} (keep (fn [[kind table]]
                    (when (#{:document :nullable-document} kind) table)))
        (vals (:target-map (inductive/describe-mapped-admission admission)))))

(defn initialize!
  "Initialize and check the original abstract state from a pinned physical
   readback. Repeat that check at the marker's historical time on restart;
   a compatible marker alone cannot bypass a failed readback."
  [node source-id {:keys [admission initializer]}]
  (when (nil? initializer) (reject! :missing-mapped-initializer))
  (let [readback (initialization/initialize-and-readback!
                  node source-id initializer (mapped-tables admission)
                  (inductive/check-mapped-initialization! admission initializer))]
    (inductive/check-mapped-initialization-readback!
     admission initializer (:document-rows readback) (:journal-rows readback)))
  nil)

(defn- mapped-operations? [ops]
  ;; The issuer checks assertion/put order and the exact original envelope.
  ;; At the installed boundary additionally require canonical table spellings
  ;; and keep manifest and initialization writes out of the mapped path.
  (let [[_ puts] (split-with #(and (vector? %) (string? (first %))) ops)]
    (and (seq puts)
         (every? #(and (put-op? %)
                       (not (#{initialization/table :gesso_publication_manifests}
                             (second %)))) puts))))

;; Registration and activation are process-local owner responsibilities. A
;; copied initialization report is not an activation credential.
(def ^:private policies (java.util.WeakHashMap.))

(defn activate-policy!
  "Enable mapped writes only after the exact installed policy's captured
   initializer has passed pinned physical readback. Failure leaves that
   policy closed; restart must construct a new policy and recheck history.
   This is not proof of all intervening or future physical writes."
  [policy]
  (let [{:keys [connectable config source-id phase] :as entry}
        (locking policies (.get policies policy))
        token (when entry (xtdb/submission-policy-token connectable policy))]
    (when-not token
      (reject! :uninstalled-mapped-writer-policy))
    (when-not (compare-and-set! phase :pending :checking)
      (reject! :invalid-mapped-writer-activation))
    (try
      (initialize! connectable source-id config)
      (when-not (xtdb/submission-policy-current? connectable token)
        (reject! :uninstalled-mapped-writer-policy))
      (reset! phase {:registration token})
      nil
      (catch Throwable e
        (reset! phase :failed)
        (throw e)))))

(defn build-policy!
  "Bind admission to the owned connectable. Return the literal-boolean
   predicate expected by the XTDB adapter. Maintenance is bound to the
   host-owned source, worker and descriptor. Observer failure refuses
   submission. Initialization is consumed locally on admission and
   durably on commit of the seed and its marker. Mapped operations
   remain disabled until activate-policy! checks the captured initializer."
  [connectable config infrastructure]
  (let [{:keys [admission initializer observe!]} (validate-config! config)
        initialized? (atom false)
        phase (atom :pending)
        source-id (:source-id infrastructure)
        checked-init (inductive/check-mapped-initialization! admission initializer)
        seed-ops (initialization/transaction source-id initializer (mapped-tables admission) checked-init)]
    (when-not connectable (reject! :missing-mapped-writer-connectable))
    (let [policy (fn [{:keys [api tx-ops tx-options] :as envelope}]
      (boolean
       (when (and (= :execute-tx api) (empty? tx-options)
                  (vector? tx-ops) (seq tx-ops))
         (let [kind (or (when-let [{:keys [identity candidate]}
                                  (when (and (xtdb/submission-policy-current? connectable (:registration @phase))
                                             (mapped-operations? tx-ops))
                                    (publication/claim-framed-submission! admission connectable envelope))]
                          (assoc identity :kind :mapped-operation :candidate candidate))
                        (when (and seed-ops (= seed-ops tx-ops)
                                   (not @initialized?)
                                   (not (initialization/initialized? connectable source-id checked-init))
                                   (compare-and-set! initialized? false true))
                          {:kind :initializer})
                        (maintenance/admit connectable infrastructure tx-ops))]
           (when kind
             (observe! kind)
             true)))))]
      (locking policies
        (.put policies policy {:connectable connectable :config config
                               :source-id source-id :phase phase}))
      policy)))
