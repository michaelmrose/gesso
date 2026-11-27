(ns gesso.live.outbox.writer-policy
  "Restricted synchronous adapter policy for a selected mapped executor.
   Domain submissions require a genuine framed handoff; initialization is
   exact and durably fenced. Maintenance is confined to a single infrastructure
   put with framework-owned exact-footprint and physical row checks.
   This is local adapter mediation, not closure over raw XTDB writers."
  (:require [gesso.live.outbox.model-publication :as publication]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.outbox.maintenance-policy :as maintenance]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.acknowledgment :as acknowledgment]
            [gesso.live.outbox.lifecycle :as lifecycle]
            [gesso.live.outbox.cycle :as cycle]
            [gesso.live.outbox.worker :as worker]
            [gesso.live.outbox.delivery :as delivery]
            [gesso.live.outbox.initialization :as initialization]
            [gesso.live.outbox.publication-manifest :as manifest]
            [gesso.live.outbox.physical-correspondence :as correspondence]
            [gesso.model.inductive-plan :as inductive]))

(def ^:private maintenance-kinds
  {:gesso_publication_manifests :manifest
   :gesso_live_outbox :worker})

(def ^:private reserved-tables
  (into #{:gesso_commands :gesso_transition_evidence initialization/table} (keys maintenance-kinds)))

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

(defn- initialize-observation!
  "Initialize and check the original abstract state from a pinned physical
   readback. Repeat that check at the marker's historical time on restart;
   a compatible marker alone cannot bypass a failed readback."
  [node source-id {:keys [admission initializer]}]
  (when (nil? initializer) (reject! :missing-mapped-initializer))
  (let [checked (inductive/check-mapped-initialization! admission initializer)
        readback (initialization/initialize-and-readback!
                  node source-id initializer (mapped-tables admission) checked)
        observed (inductive/check-mapped-initialization-readback!
                  admission initializer (:document-rows readback) (:journal-rows readback))
        retained (initialization/check-retained-initialization!
                  node source-id initializer (mapped-tables admission) checked readback)]
    (merge (select-keys checked [:catalog-fingerprint :case-fingerprint])
           (select-keys observed [:initialization-fingerprint :physical-correspondence :authority])
           retained
           {:source-id source-id :system-time (:system-time readback)})))

(defn initialize!
  "Check the selected initializer without opening a writer policy."
  [node source-id config]
  (initialize-observation! node source-id config)
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
  (let [{:keys [connectable config source-id descriptor phase] :as entry}
        (locking policies (.get policies policy))
        token (when entry (xtdb/submission-policy-token connectable policy))]
    (when-not token
      (reject! :uninstalled-mapped-writer-policy))
    (when-not (compare-and-set! phase :pending :checking)
      (reject! :invalid-mapped-writer-activation))
    (try
      (let [establishment (manifest/check-retained-establishment! connectable source-id descriptor)
            initial (initialize-observation! connectable source-id config)
            observation (merge initial (manifest/require-initialization-root! connectable source-id establishment initial))]
        (when-not (xtdb/submission-policy-current? connectable token)
          (reject! :uninstalled-mapped-writer-policy))
        (correspondence/record-activation! connectable policy token (:admission config) observation
                                          {:initializer (:initializer config) :descriptor descriptor})
        (reset! phase {:registration token :activation observation}))
      nil
      (catch Throwable e
        (reset! phase :failed)
        (throw e)))))

(def ^:private original-worker-origin lifecycle/current-worker-origin)
(def ^:private original-acquisition-handoff acquisition/claim-maintenance-handoff!)
(def ^:private original-acknowledgment-handoff acknowledgment/claim-maintenance-handoff!)

(def ^:private original-acquisition-call worker/claim-acquisition-call!)
(def ^:private original-acknowledgment-call delivery/claim-acknowledgment-call!)
(def ^:private original-cycle-call cycle/claim-maintenance-call!)

(defn- observe-maintenance-origin
  "Add local provenance only after independent exact maintenance admission.
   Scope does not establish the installed host owner or dispatch provenance."
  [node {:keys [source-id worker-id]} ops kind]
  (if (and (= :worker (:kind kind))
           (identical? original-worker-origin lifecycle/current-worker-origin)
           (identical? original-acquisition-handoff acquisition/claim-maintenance-handoff!)
           (identical? original-acknowledgment-handoff acknowledgment/claim-maintenance-handoff!)
           (identical? original-acquisition-call worker/claim-acquisition-call!)
           (identical? original-acknowledgment-call delivery/claim-acknowledgment-call!)
           (identical? original-cycle-call cycle/claim-maintenance-call!))
    (let [{:keys [maintenance submission]} (case (:status kind)
                                             :leased (original-acquisition-handoff node ops)
                                             :dispatch-accepted (original-acknowledgment-handoff node ops)
                                             nil)
          phase (if (= :dispatch-acknowledgment maintenance) :delivery :acquisition)
          input (when maintenance
                  (if (= :delivery phase)
                    (original-acknowledgment-call node submission)
                    (original-acquisition-call node maintenance submission)))
          cycle-opts (when input (original-cycle-call node phase input))
          origin (when cycle-opts (original-worker-origin node source-id worker-id cycle-opts))]
      (cond-> kind origin (assoc :worker-origin (assoc origin :maintenance maintenance
                                                       :call-chain :matched-scheduled-maintenance))))
    kind))

(defn build-policy!
  "Bind admission to the owned connectable. Return the literal-boolean
   predicate expected by the XTDB adapter. Maintenance is bound to the
   host-owned source, worker and descriptor. Observer failure refuses
   submission. Initialization is consumed locally on admission and
   durably on commit of the seed and its marker. Mapped operations
   and worker maintenance remain disabled until activate-policy! checks the captured initializer."
  [connectable config infrastructure]
  (let [{:keys [admission initializer observe!]} (validate-config! config)
        initialized? (atom false)
        phase (atom :pending)
        source-id (:source-id infrastructure)
        description (inductive/describe-mapped-admission admission)
        declared-publication? (contains? description :publication-derivations)
        declared-residual? (contains? description :residual-assertion-derivations)
        checked-init (inductive/check-mapped-initialization! admission initializer)
        seed-ops (initialization/transaction source-id initializer (mapped-tables admission) checked-init)]
    (when-not connectable (reject! :missing-mapped-writer-connectable))
    (let [policy (fn installed-policy [{:keys [api tx-ops tx-options] :as envelope}]
      (boolean
       (when (and (= :execute-tx api) (empty? tx-options)
                  (vector? tx-ops) (seq tx-ops))
         (let [kind (if-let [{:keys [identity candidate]}
                             (when (and (xtdb/submission-policy-current? connectable (:registration @phase))
                                        (mapped-operations? tx-ops))
                               (publication/claim-framed-submission! admission connectable envelope))]
                      ;; Only the genuine, consumed handoff can carry this check.
                      ;; A failed mapped handoff cannot fall through to maintenance.
                      (when (and (or (not declared-publication?)
                                     (and (= :matched (:publication-expectation candidate))
                                          (= source-id (:publication-source-id candidate))))
                                 (or (not declared-residual?)
                                     (= :matched (:residual-assertion-expectation candidate))))
                        (assoc identity :kind :mapped-operation :candidate candidate))
                      (or (when (and seed-ops (= seed-ops tx-ops)
                                     (not @initialized?)
                                     (not (initialization/initialized? connectable source-id checked-init))
                                     (compare-and-set! initialized? false true))
                            {:kind :initializer})
                          ;; Manifest establishment precedes activation. Worker
                          ;; maintenance requires the current activated generation.
                          (when-let [maintenance-kind (maintenance/admit connectable infrastructure tx-ops)]
                            (when (or (= :manifest (:kind maintenance-kind))
                                      (and (= :worker (:kind maintenance-kind))
                                           (xtdb/submission-policy-current? connectable (:registration @phase))))
                              (observe-maintenance-origin connectable infrastructure tx-ops maintenance-kind)))))]
           (when kind
             (observe! kind)
             (when (:worker-origin kind)
               (xtdb/observe-policy-admission! installed-policy envelope kind))
             true)))))]
      (locking policies
        (.put policies policy {:connectable connectable :config config
                               :source-id source-id :descriptor (:descriptor infrastructure) :phase phase}))
      policy)))


(defn activated-policy-root
  "Read the actual mapped policy's current generation and captured root.
   Neither a registered arbitrary predicate nor an independently recorded
   activation can substitute for the completed mapped-policy phase.
   Trusted in-process diagnostics only; not a grant or a writer inventory."
  [connectable admission token]
  (when (and token (xtdb/submission-policy-current? connectable token))
    (let [entry (locking policies
                  (some (fn [[policy {:keys [config phase] :as entry}]]
                          (when (and (identical? admission (:admission config))
                                     (identical? token (:registration @phase))
                                     (identical? token (xtdb/submission-policy-token connectable policy)))
                            entry)) policies))]
      (when entry
        (let [{:keys [config descriptor source-id phase]} entry
              snapshot @phase
              root (correspondence/activation-root connectable admission token)]
          (when (and (identical? token (:registration snapshot))
                     (= (:activation snapshot) (:observation root))
                     (= source-id (get-in root [:observation :source-id]))
                     (= {:initializer (:initializer config) :descriptor descriptor} (:root root))
                     (xtdb/submission-policy-current? connectable token))
            root))))))
