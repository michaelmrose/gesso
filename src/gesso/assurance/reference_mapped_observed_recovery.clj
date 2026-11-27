(ns gesso.assurance.reference-mapped-observed-recovery
  "Compose local issued submission observation and retained recovery correspondence."
  (:require [gesso.assurance.assumption :as assumption]
            [gesso.assurance.inductive-report :as inductive-report]
            [gesso.assurance.reference-historical-cut :as cut]
            [gesso.assurance.reference-mapped-evidence :as evidence]
            [gesso.assurance.reference-mapped-submission :as submission]
            [gesso.assurance.reference-mapped-worker-envelope :as worker-envelope]
            [gesso.assurance.reference-provider :as provider]
            [gesso.assurance.reference-storage-identity :as storage]
            [gesso.live.outbox.initialization :as initialization]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.live.outbox.worker-capture :as worker-capture]
            [gesso.model.inductive-plan :as induction]))

(def profile :selected-issued-observed-mapped-recovery-v1)

(def interleaved-profile :selected-issued-observed-mapped-worker-interleaved-recovery-v1)

(defn- compare-selected*
  [node tables admission source outcome-declarations steps handle interleaved?]
  (let [base {:observed-recovery-profile (if interleaved? interleaved-profile profile) :authority :none :certificate :not-issued
              :provider-refinement :not-established :source-provenance :not-established
              :outcome-provenance :not-established :submission-provenance :not-established
              :aborted-receipt-identity :not-established :observation-storage-identity :not-established}
        fail #(assoc base :status %1 :reason %2)
        required #{:original :targets :docs :journal :expectation :prefix-count
                   :spec :context :source-id :declarations :residual-declarations}]
    (if-not (and (vector? steps) (nil? (meta steps)) (= 3 (count steps))
                 (every? #(and (map? %) (nil? (meta %)) (= required (set (keys %)))
                               (map? (:expectation %)) (map? (:spec %))) steps))
      (fail :unsupported :invalid-observed-recovery-source)
      (let [observed (submission/compare-observed! admission steps handle)]
        (cond
          (not= :matched (:status observed)) (fail (:status observed) (:reason observed))
          (not (and (= submission/observed-profile (:observed-profile observed))
                    (= :matched-local-issued-policy-window (:observation-origin observed))
                    (= :matched-observed-checked-candidates (:framed-handoffs observed))
                    (= :matched-selected-retained-operations (:adapter-payloads observed))
                    (= :matched-selected-commit-identities (:returned-transaction-keys observed))
                    (= :observed-throw-without-transaction-key (:aborted-call observed))))
          (fail :unsupported :invalid-observed-recovery-submission-comparison)
          :else
          (let [retained ((if interleaved?
                           evidence/compare-local-processed-retained-issued-executor-worker-interleaved-recovery!
                           evidence/compare-local-processed-retained-issued-executor-recovery!)
                          node tables admission source outcome-declarations steps)]
            (cond
              (not= :matched (:status retained)) (fail (:status retained) (:reason retained))
              (not (and (= (if interleaved? evidence/issued-executor-interleaved-recovery-profile
                                             evidence/issued-executor-recovery-profile)
                           (:issued-executor-recovery-profile retained))
                        (= :matched-issued-declarations (:issued-admission-binding retained))
                        (= :matched-issued-closed-construction (:closed-decoder-binding retained))
                        (= (mapv #(get-in % [:expectation :tx-id]) steps) (:selected-tx-ids retained))
                        (= [:committed :aborted :committed] (:selected-commitments retained))
                        (= :matched-retained-journal-trajectory (:recovery-linkage retained))
                        (or (not interleaved?)
                            (and (= provider/mapped-interleaved-recovery-profile (:mapped-recovery-profile retained))
                                 (vector? (:intervening-maintenance retained))
                                 (integer? (:replayed-transaction-count retained))))))
              (fail :unsupported :invalid-observed-recovery-retained-comparison)
              :else (merge retained
                           (select-keys observed [:observed-profile :observation-origin :framed-handoffs
                                                 :adapter-payloads :returned-transaction-keys :aborted-call
                                                 :command-origin-correspondence :selected-command-origins
                                                 :abort-recovery-execution :command-authentication])
                           base {:status :matched :submission-recovery-linkage :matched-local-observed-retained-steps}
                           (when interleaved? {:worker-observation-provenance :not-established})))))))))

(defn compare-selected!
  "Check one locally issued submission observation and the issued recovery
   comparison against exactly the same selected steps. Do not expose a
   positive subcomparison when either side fails. Historical observation
   does not authenticate the provider, prove storage identity across reopen,
   establish an aborted receipt identity, or cover other writers."
  [node tables admission source outcome-declarations steps handle]
  (compare-selected* node tables admission source outcome-declarations steps handle false))

(defn compare-worker-interleaved!
  "Match the three observed mapped submissions against the full retained
   worker-interleaved history. Worker envelopes are checked from history,
   not attributed to the synchronous submission observation."
  [node tables admission source outcome-declarations steps handle]
  (compare-selected* node tables admission source outcome-declarations steps handle true))

(def activated-profile :selected-activated-observed-mapped-recovery-v1)

(def activated-interleaved-profile :selected-activated-observed-mapped-worker-interleaved-recovery-v1)

(defn- compare-activated*
  [node tables admission source outcome-declarations steps handle interleaved?]
  (let [base {:activated-recovery-profile (if interleaved? activated-interleaved-profile activated-profile) :authority :none :certificate :not-issued
              :provider-refinement :not-established :observation-storage-identity :not-established
              :submission-provenance :not-established :aborted-receipt-identity :not-established}
        fail #(assoc base :status %1 :reason %2)]
    (try
      (let [{:keys [activation-root] :as observed} (publication/read-framed-submission-observation! admission handle)
            {:keys [observation root]} activation-root
            {:keys [source-id manifest-tx-id initialization-tx-id system-time]} observation
            {:keys [initializer descriptor]} root]
        (cond
          (nil? observed) (fail :unsupported :unissued-framed-submission-observation)
          (not (and (= :matched (:manifest-envelope observation) (:initialization-envelope observation))
                    (= :checked-supplied-projection (:physical-correspondence observation))
                    (string? source-id) (string? descriptor) (vector? initializer)
                    (integer? manifest-tx-id) (integer? initialization-tx-id)
                    (< manifest-tx-id initialization-tx-id)
                    (instance? java.time.Instant system-time)))
          (fail :unsupported :missing-selected-activation-root)
          (not (and (vector? steps) (= 3 (count steps)) (every? #(= source-id (:source-id %)) steps)))
          (fail :mismatch :selected-activation-source-mismatch)
          (not (and (set? tables) (every? tables [:gesso_initializations :gesso_publication_manifests])))
          (fail :unsupported :incomplete-activation-root-scope)
          :else
          (let [checked (induction/check-mapped-initialization! admission initializer)
                fingerprints [:catalog-fingerprint :case-fingerprint :initialization-fingerprint]
                manifest {:tx-id manifest-tx-id
                          :ops [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_publication_manifests WHERE _id = ?)" source-id]
                                ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE gesso$live$outbox$source_id = ?)" source-id]
                                [:put-docs :gesso_publication_manifests {:xt/id source-id :profile-version 1 :descriptor descriptor}]]}
                init {:tx-id initialization-tx-id :system-time system-time
                      :ops (initialization/transaction source-id initializer (:tables checked) checked)}]
            (if-not (= (select-keys checked fingerprints) (select-keys observation fingerprints))
              (fail :mismatch :selected-activation-initialization-mismatch)
              (let [r (provider/compare-with-retained-root!
                       node manifest init (get-in steps [0 :expectation :tx-id])
                       #(compare-selected* node tables admission source outcome-declarations steps handle interleaved?))]
                (if (and (= :matched (:status r)) (= (if interleaved? interleaved-profile profile) (:observed-recovery-profile r))
                         (= :matched-local-observed-retained-steps (:submission-recovery-linkage r)))
                  (merge r base {:status :matched :activation-root-binding :matched-local-activated-retained-root
                                 :initialization-tx-id initialization-tx-id :manifest-tx-id manifest-tx-id})
                  (fail (if (= :matched (:status r)) :unsupported (:status r))
                        (or (:reason r) :invalid-activated-recovery-comparison))))))))
      (catch clojure.lang.ExceptionInfo e
        (fail :unsupported (or (:reason (ex-data e)) :invalid-activated-recovery-source))))))

(defn compare-activated!
  "Require an observation captured under the same locally recorded activated
   writer generation, and its exact manifest and initialization as the first
   two retained transactions immediately preceding the selected recovery.
   Reconstruct the seed envelope from the issued admission. Historical root
   matching is not durable storage authentication or full host refinement."
  [node tables admission source outcome-declarations steps handle]
  (compare-activated* node tables admission source outcome-declarations steps handle false))

(defn compare-activated-worker-interleaved!
  "Bind the activated initialization root and the three locally observed
   mapped submissions to one complete worker-interleaved retained sample.
   The root must still immediately precede the first selected submission."
  [node tables admission source outcome-declarations steps handle]
  (compare-activated* node tables admission source outcome-declarations steps handle true))

(def local-acquisition-profile :selected-local-origin-retained-recovery-v1)

(def ^:private original-origin-matcher publication/framed-submission-origin-matches?)
(def ^:private original-observation-reader publication/read-framed-submission-observation!)
(def ^:private original-acquisition-comparison provider/compare-with-issued-retained-acquisition!)
(def ^:private original-activated-comparison compare-activated*)

(defn- local-boundaries-current? []
  (and (identical? original-origin-matcher publication/framed-submission-origin-matches?)
       (identical? original-observation-reader publication/read-framed-submission-observation!)
       (identical? original-acquisition-comparison provider/compare-with-issued-retained-acquisition!)
       (identical? original-activated-comparison compare-activated*)))

(defn compare-local-activated!
  "Require the exact issued original observation and retained acquisition
   to originate at the supplied node. Compare the activated recovery against
   the entire issued sample while independently reobserving its contents.
   Preserve the historical content-only APIs for reopened stores. Local
   matching does not authenticate writer provenance or establish continuity."
  [node tables admission source outcome-declarations steps observation acquisition interleaved?]
  (let [base {:local-acquisition-profile local-acquisition-profile
              :authority :none :certificate :not-issued
              :observation-storage-identity :not-established
              :authorized-historical-lineage :not-established}
        fail #(assoc base :status %1 :reason %2)]
    (cond
      (not (boolean? interleaved?)) (fail :unsupported :invalid-local-recovery-mode)
      (not (local-boundaries-current?))
      (fail :unsupported :local-recovery-boundary-replaced)
      (not (original-origin-matcher admission observation node))
      (fail :unsupported :submission-origin-mismatch)
      :else
      (let [r (original-acquisition-comparison
               node acquisition
               #(original-activated-comparison node tables admission source outcome-declarations steps observation interleaved?))]
        (cond
          (not (local-boundaries-current?)) (fail :unsupported :local-recovery-boundary-replaced)
          (not= :matched (:status r)) (fail (:status r) (:reason r))
          (not (original-origin-matcher admission observation node))
          (fail :unsupported :submission-origin-mismatch)
          (not (and (= (if interleaved? activated-interleaved-profile activated-profile) (:activated-recovery-profile r))
                    (= :matched-local-activated-retained-root (:activation-root-binding r))
                    (= :matched-local-observed-retained-steps (:submission-recovery-linkage r))))
          (fail :unsupported :invalid-local-recovery-comparison)
          :else (merge r base {:local-acquisition-binding :matched-issued-origin-and-retained-sample}))))))

(def recovery-cut-profile :selected-issued-local-recovery-cut-v1)
(deftype RecoveryCutObservation [])
(def ^:private recovery-cuts (java.util.WeakHashMap.))
(def ^:private original-local-comparison compare-local-activated!)
(def ^:private original-cut-derivation cut/derive-cut!)
(def ^:private original-admission-description induction/describe-mapped-admission)
(def ^:private original-induction-recheck induction/recheck-mapped-induction!)
(def ^:private original-worker-comparison worker-envelope/compare-issued-selection!)
(def ^:private original-worker-boundaries worker-envelope/boundaries-current?)
(def ^:private original-worker-capture-match worker-capture/worker-outcome-capture-matches?)

(defn- worker-boundaries-current? []
  (and (identical? original-worker-capture-match worker-capture/worker-outcome-capture-matches?)
       (identical? original-worker-comparison worker-envelope/compare-issued-selection!)
       (identical? original-worker-boundaries worker-envelope/boundaries-current?)
       (original-worker-boundaries)))

(defn- cut-boundaries-current? []
  (and (local-boundaries-current?)
       (identical? original-local-comparison compare-local-activated!)
       (identical? original-cut-derivation cut/derive-cut!)
       (identical? original-admission-description induction/describe-mapped-admission)
       (identical? original-induction-recheck induction/recheck-mapped-induction!)))

(def ^:private original-recovery-keys
  [:activated-recovery-profile :observed-recovery-profile :local-acquisition-profile
   :local-acquisition-binding :activation-root-binding :submission-recovery-linkage
   :manifest-tx-id :initialization-tx-id :selected-tx-ids :selected-commitments
   :recovery-linkage :provider-refinement :source-provenance :outcome-provenance
   :submission-provenance :aborted-receipt-identity :worker-observation-provenance
   :command-origin-correspondence :selected-command-origins :abort-recovery-execution
   :command-authentication :worker-correspondence])

(defn- checked-abstract-induction [admission]
  ;; Reports are descriptive data. Only the captured checker over the exact
  ;; issued admission and its original live handles may supply this report.
  (try
    (let [report (original-induction-recheck admission)]
      (when (and (= inductive-report/report-type (:type report))
                 (= inductive-report/report-version (:version report))
                 (= :original-sources-and-both-live-handles-rechecked (:checking report))
                 (= :source-bound-conditional-abstract-safety (:verdict report))
                 (= :closed-declared-abstract-machine-only (:scope report))
                 (= :all-original-obligations-remain-open
                    (get-in report [:classification :original-obligations :status]))
                 (= :not-performed (:assumption-discharge report))
                 (= :not-established (:production-certification report))
                 (= :none (:authority report) (:transaction-authority report)))
        report))
    (catch clojure.lang.ExceptionInfo _ nil)))

(defn- recovery-subject [admission source steps comparison abstract]
  (let [description (original-admission-description admission)
        fingerprints (select-keys description [:catalog-fingerprint :induction-fingerprint])
        operations (mapv (fn [step]
                           (let [id (get-in step [:spec :alternative])
                                 mapping (get-in description [:operation-mappings id])]
                             (when (and mapping (= id (get-in step [:original :subject :id])))
                               {:alternative-id id
                                :command-operation (get-in step [:spec :binding :operation])
                                :subject-fingerprint (:subject-fingerprint mapping)
                                :transition-fingerprint (:transition-fingerprint mapping)
                                :mapping-fingerprint (:fingerprint mapping)}))) steps)]
    (when (and abstract
               (= (get-in source [:inductive-source :catalog :id]) (:catalog-id abstract))
               (= fingerprints (select-keys (:identity abstract) (keys fingerprints)))
               (= (:original-open-obligations description)
                  (get-in abstract [:classification :original-obligations :by-operation]))
               (= :matched-issued-declarations (:issued-admission-binding comparison))
               (= (:catalog-fingerprint fingerprints) (:issued-catalog-fingerprint comparison))
               (= (:induction-fingerprint fingerprints) (:issued-induction-fingerprint comparison))
               (every? some? operations))
      (assoc fingerprints :catalog-id (get-in source [:inductive-source :catalog :id])
             :abstract-induction-fingerprint (:fingerprint abstract)
             :abstract-induction-identity (:identity abstract)
             :selected-operations operations))))

(defn- capture-local-recovery-cut*
  [node tables admission source outcome-declarations steps observation acquisition interleaved?
   worker? system worker-capture]
  (let [base {:recovery-cut-profile recovery-cut-profile
              :authority :none :certificate :not-issued
              :observation-storage-identity :not-established
              :authorized-historical-lineage :not-established}
        fail #(assoc base :status %1 :reason %2)]
    (cond
      (not (and (cut-boundaries-current?) (or (not worker?) (worker-boundaries-current?))))
      (fail :unsupported :recovery-cut-boundary-replaced)
      (and worker? (not (original-worker-capture-match system worker-capture)))
      (fail :unsupported :unissued-worker-capture)
      :else
      (let [r (if worker?
                (original-acquisition-comparison
                 node acquisition
                 (fn []
                   (let [local (original-local-comparison node tables admission source outcome-declarations
                                                          steps observation acquisition true)]
                     (if (= :matched (:status local))
                       (let [checked (original-worker-comparison system acquisition (:source-id (first steps))
                                                                 (:intervening-maintenance local) worker-capture)]
                         (if (= :matched (:status checked))
                           (assoc local :worker-observation-provenance (:worker-observation-provenance checked)
                                  :worker-correspondence (assoc checked :selected-tx-ids (:selected-tx-ids local)))
                           checked))
                       local))))
                (original-local-comparison node tables admission source outcome-declarations
                                           steps observation acquisition interleaved?))]
        (cond
          (not (and (cut-boundaries-current?) (or (not worker?) (worker-boundaries-current?))))
          (fail :unsupported :recovery-cut-boundary-replaced)
          (not= :matched (:status r)) (fail (:status r) (:reason r))
          (not (and (= local-acquisition-profile (:local-acquisition-profile r))
                    (= :matched-issued-origin-and-retained-sample (:local-acquisition-binding r))))
          (fail :unsupported :invalid-local-recovery-comparison)
          :else
          (let [abstract (checked-abstract-induction admission)
                subject (recovery-subject admission source steps r abstract)
                derived (original-cut-derivation node acquisition)]
            (cond
              (not (and (cut-boundaries-current?) (or (not worker?) (worker-boundaries-current?))))
              (fail :unsupported :recovery-cut-boundary-replaced)
              (and worker? (not (original-worker-capture-match system worker-capture)))
              (fail :unsupported :unissued-worker-capture)
              (nil? abstract) (fail :unsupported :invalid-abstract-induction)
              (nil? subject) (fail :unsupported :invalid-recovery-subject)
              (not= :derived (:status derived)) (fail (:status derived) (:reason derived))
              (and worker?
                   (not (and (= :matched-local-issued-host-capture (:worker-observation-provenance r))
                             (= :matched-issued-worker-retained-transactions
                                (get-in r [:worker-correspondence :maintenance-correspondence]))
                             (= (:selected-tx-ids r) (get-in r [:worker-correspondence :selected-tx-ids]))
                             (= (get-in derived [:historical-cut :payload-fingerprint])
                                (get-in r [:worker-correspondence :retained-payload-fingerprint])))))
              (fail :unsupported :invalid-worker-cut-correspondence)
              (not (original-origin-matcher admission observation node))
              (fail :unsupported :submission-origin-mismatch)
              :else
              (let [handle (RecoveryCutObservation.)]
                (locking recovery-cuts
                  (.put recovery-cuts handle {:admission admission :observation observation
                                              :historical-cut (:historical-cut derived)
                                              :original-recovery (select-keys r original-recovery-keys)
                                              :judgment-subject subject
                                              :command-observation-ref (str (random-uuid))
                                              :abstract-induction abstract
                                              :publication-source-id (:source-id (first steps))}))
                (merge r base {:recovery-cut-observation handle
                               :recovery-cut-binding :matched-local-recovery-cut})))))))))

(defn capture-local-recovery-cut!
  "Issue a historical cut only after the exact observation and acquisition
   match through the independent local recovery comparison. Derive the cut
   from that same issued sample, not a supplied comparison or declared cut.
   The handle survives node closure in this process, not process restart.
   It is not a durable receipt or authenticated lifecycle binding."
  [node tables admission source outcome-declarations steps observation acquisition interleaved?]
  (capture-local-recovery-cut* node tables admission source outcome-declarations steps observation
                               acquisition interleaved? false nil nil))

(defn capture-local-worker-recovery-cut!
  "Issue a worker-interleaved cut only after independently rechecking the
   exact host-issued maintenance capture and original command observation
   against the same issued retained acquisition. Never accept a supplied
   comparison. The cut carries local historical correspondence only;
   worker, dispatch and provider authentication remain open."
  [system tables admission source outcome-declarations steps observation acquisition worker-capture]
  (capture-local-recovery-cut* (:xtdb/connectable system) tables admission source outcome-declarations
                               steps observation acquisition true true system worker-capture))

(defn read-local-recovery-cut!
  "Read sensitive historical cut material for the exact issued admission
   and observation. No node or writer capability is returned; no fresh
   physical comparison or durable identity is implied. Do not publish."
  [admission observation handle]
  (when (instance? RecoveryCutObservation handle)
    (locking recovery-cuts
      (let [entry (.get recovery-cuts handle)]
        (when (and entry (identical? admission (:admission entry))
                   (identical? observation (:observation entry)))
          (:historical-cut entry))))))

(def conditional-profile :selected-conditional-historical-recovery-v1)
(deftype ConditionalRecovery [])
(def ^:private conditional-recoveries (java.util.WeakHashMap.))
(def ^:private original-prefix-comparison cut/compare-prefix-cut!)
(def ^:private original-premise-selector storage/describe-selected-premises)
(def ^:private original-obligation-description storage/describe-obligations)
(def ^:private policy-keys
  #{:comparison-id :relation :identity-policy :origin-boundary
    :acquisition-boundary :identity-boundary})

(defn- conditional-report [status]
  {:conditional-recovery-profile conditional-profile :status status
   :observation-storage-identity :not-established
   :authorized-historical-lineage :not-established
   :authority :none :certificate :not-issued})

(defn- conditional-boundaries-current? []
  (and (identical? original-prefix-comparison cut/compare-prefix-cut!)
       (identical? original-premise-selector storage/describe-selected-premises)
       (identical? original-obligation-description storage/describe-obligations)))

(defn prepare-conditional-recovery!
  "Prepare a historical comparison of the exact locally issued original cut
   and target acquisition. The caller declares only the lifecycle policy;
   the cut, source and fresh local endpoint references are derived here.
   A matching copy may prepare a conditional comparison, but cannot thereby
   establish lifecycle authority. No current-state or durable receipt claim."
  [admission observation original-cut node target policy]
  (let [fail #(assoc (conditional-report %1) :reason %2)
        origin (when (instance? RecoveryCutObservation original-cut)
                 (locking recovery-cuts (.get recovery-cuts original-cut)))]
    (cond
      (not (conditional-boundaries-current?))
      (fail :unsupported :conditional-recovery-boundary-replaced)
      (not (and origin (identical? admission (:admission origin))
                (identical? observation (:observation origin))))
      (fail :unsupported :unissued-recovery-cut)
      (not (and (map? policy) (not (record? policy)) (nil? (meta policy))
                (= policy-keys (set (keys policy)))))
      (fail :unsupported :invalid-lifecycle-policy)
      :else
      (let [scope (assoc policy
                         :publication-source-id (:publication-source-id origin)
                         :origin-observation-ref (str (random-uuid))
                         :retained-acquisition-ref (str (random-uuid))
                         :historical-cut (:historical-cut origin))]
        (try
          (original-obligation-description scope)
          (let [r (original-prefix-comparison (:historical-cut origin) node target)]
            (cond
              (not (conditional-boundaries-current?))
              (fail :unsupported :conditional-recovery-boundary-replaced)
              (not= :matched (:status r)) (fail (:status r) (:reason r))
              (not (contains? #{:exact-content :prefix-content} (:cut-correspondence r)))
              (fail :unsupported :invalid-prefix-comparison)
              :else
              (let [handle (ConditionalRecovery.)]
                (locking conditional-recoveries
                  (.put conditional-recoveries handle
                        {:scope scope :cut-correspondence (:cut-correspondence r)
                         :original-cut original-cut :target target
                         :original-recovery (:original-recovery origin)
                         :judgment-subject (:judgment-subject origin)
                         :command-observation-ref (:command-observation-ref origin)
                         :abstract-induction (:abstract-induction origin)
                         :target-node (java.lang.ref.WeakReference. node)
                         :admission admission}))
                (assoc (conditional-report :prepared) :conditional-recovery handle))))
          (catch clojure.lang.ExceptionInfo e
            (fail :unsupported (or (:reason (ex-data e)) :invalid-lifecycle-policy))))))))

(defn- conditional-entry [handle]
  (when (instance? ConditionalRecovery handle)
    (locking conditional-recoveries (.get conditional-recoveries handle))))

(defn read-conditional-recovery-scope!
  "Read sensitive declared scope of a prepared comparison. Endpoint refs
   are fresh process-local labels, not durable identifiers. Do not publish."
  [handle]
  (:scope (conditional-entry handle)))

(defn select-conditional-recovery-premises!
  "Compose the already compared historical endpoints with their exact open
   lifecycle premises. No caller-supplied content report is accepted.
   The returned canonical obligations contain sensitive cut material and
   are for internal use only. Conditional does not mean accepted or proved."
  [handle offered]
  (let [fail #(assoc (conditional-report :unsupported) :reason %)]
    (if-not (conditional-boundaries-current?)
      (fail :conditional-recovery-boundary-replaced)
      (if-let [{:keys [scope cut-correspondence]} (conditional-entry handle)]
        (try
          (let [r (original-premise-selector scope offered)]
            (if-not (conditional-boundaries-current?)
              (fail :conditional-recovery-boundary-replaced)
              (merge r (conditional-report :conditional)
                     {:cut-correspondence cut-correspondence
                      :historical-assurance :conditional-on-open-premises})))
          (catch clojure.lang.ExceptionInfo e
            (fail (or (:reason (ex-data e)) :invalid-premise-selection))))
        (fail :unissued-conditional-recovery)))))

(def ^:private original-deployment-acceptance storage/describe-deployment-acceptance)
(def ^:private original-assumption-description assumption/describe-assumption)

(def command-authentication-profile :selected-command-authentication-premises-v1)

(defn- command-authentication-premises
  "Describe outstanding authentication obligations for one issued cut.
   The original-cut reference identifies the local invocation observations;
   the cut and ordered transaction IDs identify the retained command payloads.
   Neither is a credential or proof of who invoked a command. Keep these
   premises separate from the selected storage-trust obligations."
  [{:keys [scope original-recovery judgment-subject command-observation-ref]}]
  (let [id :reference-command/selected-execution
        principal-id :reference-command/principal-resolution
        attribution-id :reference-command/command-attribution
        interface {:profile command-authentication-profile
                   :publication-source-id (:publication-source-id scope)
                   :historical-cut (:historical-cut scope)
                   :command-observation-ref command-observation-ref
                   :judgment-subject judgment-subject
                   :selected-tx-ids (:selected-tx-ids original-recovery)
                   :selected-commitments (:selected-commitments original-recovery)
                   :origin-correspondence (:command-origin-correspondence original-recovery)}
        describe (fn [premise proposition boundary dependencies origin]
                   (original-assumption-description
                    {:id premise :subject {:kind :runtime :id id :interface interface}
                     :scope {:kind :runtime :id id} :proposition [proposition]
                     :parameters {} :semantic-context {:profile command-authentication-profile}
                     :dependencies dependencies :origin origin
                     :implementation-boundary boundary}))
        obligations [(describe principal-id :reference-command/authenticate-resolved-principals
                               :gesso.live.optimistic.server/principal-fn #{} :environment)
                     (describe attribution-id :reference-command/authenticate-selected-command-executions
                               :gesso.live.optimistic.server/registered-execution
                               #{principal-id} :implementation-correspondence)]]
    {:profile command-authentication-profile
     :scope interface
     :selected-command-origins (:selected-command-origins original-recovery)
     :abort-recovery-execution (:abort-recovery-execution original-recovery)
     :obligations obligations
     :open-premise-fingerprints (set (map :premise-fingerprint obligations))
     :premise-acceptance :not-performed
     :command-authentication :not-established
     :authority :none :certificate :not-issued}))

(defn- acceptance-boundaries-current? []
  (and (cut-boundaries-current?)
       (conditional-boundaries-current?)
       (identical? original-deployment-acceptance storage/describe-deployment-acceptance)
       (identical? original-assumption-description assumption/describe-assumption)))

(def judgment-profile :selected-conditional-activated-recovery-judgment-v4)

(defn- declare-recovery-trust* [entry declaration]
  (let [fail #(assoc (conditional-report :unsupported) :reason %)]
    (if-not (and (acceptance-boundaries-current?)
                 (or (nil? (get-in entry [:original-recovery :worker-correspondence]))
                     (worker-boundaries-current?)))
      (fail :conditional-recovery-boundary-replaced)
      (if-let [{:keys [scope cut-correspondence original-recovery judgment-subject
                       admission abstract-induction]} entry]
        (try
          (let [r (original-deployment-acceptance scope declaration)
                fresh (checked-abstract-induction admission)
                command-premises (command-authentication-premises entry)]
            (cond
              (not (and (acceptance-boundaries-current?)
                        (or (nil? (:worker-correspondence original-recovery))
                            (worker-boundaries-current?))))
              (fail :conditional-recovery-boundary-replaced)
              (not (and abstract-induction (= abstract-induction fresh)))
              (fail :abstract-induction-mismatch)
              :else
              (merge r (conditional-report :conditional)
                     {:judgment-profile judgment-profile
                      :judgment-scope scope
                      :judgment-subject judgment-subject
                      :abstract-induction-binding :matched-original-issued-induction
                      :abstract-induction abstract-induction
                      :original-recovery original-recovery
                      :command-authentication-premises command-premises
                      :cut-correspondence cut-correspondence
                      :historical-assurance :conditional-on-declared-trust
                      :open-premise-fingerprints (set (map :premise-fingerprint (:obligations r)))
                      :proof-discharge :not-performed})))
          (catch clojure.lang.ExceptionInfo e
            (fail (or (:reason (ex-data e)) :invalid-deployment-acceptance))))
        (fail :unissued-conditional-recovery)))))

(defn declare-conditional-recovery-trust!
  "Compose an explicit deployment trust declaration with an issued historical
   comparison, rederiving the exact selected premises from its registry.
   This is an unauthenticated declaration, not an assertion that a controller
   has actually authorized the lifecycle. Compose the selected original
   activated trajectory with its issued historical comparison and exact
   declared trust. Recheck the original issued abstract induction and
   retain its entire report, including all open obligations. Describe the
   original command authentication premises separately: storage trust
   neither accepts nor discharges them. Host adoption is recorded by the host
   boundary, not by this declaration. Open premises remain open. No
   Claim invariant theorem or universal refinement is established.
   The four-argument form additionally requires the exact original issued
   admission and expected catalog, abstract evidence and ordered-operation
   identities. The result
   is sensitive internal material, not a receipt or credential."
  ([handle declaration]
   (declare-recovery-trust* (conditional-entry handle) declaration))
  ([handle admission expected-subject declaration]
   (let [entry (conditional-entry handle)
         fail #(assoc (conditional-report :unsupported) :reason %)]
     (cond
       (not (acceptance-boundaries-current?)) (fail :conditional-recovery-boundary-replaced)
       (nil? entry) (fail :unissued-conditional-recovery)
       (not (identical? admission (:admission entry))) (fail :recovery-admission-mismatch)
       (not= expected-subject (:judgment-subject entry)) (fail :recovery-subject-mismatch)
       :else (declare-recovery-trust* entry declaration)))))

(defn conditional-recovery-target-matches?
  "Match the exact prepared target, original admission and publication source.
   This historical local predicate exposes no provider or writer authority."
  [handle node admission source-id]
  (boolean
   (when-let [entry (conditional-entry handle)]
     (and (some? node) (some? admission)
          (identical? node (.get ^java.lang.ref.WeakReference (:target-node entry)))
          (identical? admission (:admission entry))
          (= source-id (get-in entry [:scope :publication-source-id]))))))
