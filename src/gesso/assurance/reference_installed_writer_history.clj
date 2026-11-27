(ns gesso.assurance.reference-installed-writer-history
  "Conditional classification of the exact issued retained transaction cut."
  (:require [gesso.assurance.reference-clm-r2-binding :as clm-r2]
            [gesso.assurance.reference-installed-writer-classification :as installed]
            [gesso.assurance.reference-mapped-observed-recovery :as recovery]
            [gesso.assurance.reference-retained-acquisition :as retained]
            [gesso.assurance.reference-writer-temporal-coverage :as temporal]
            [gesso.assurance.reference-selected-refinement :as refinement]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.live.outbox.writer-policy :as policy]))

(def profile :conditional-installed-retained-writer-history-v1)
(def ^:private classify installed/derive!)
(def ^:private capture recovery/capture-local-worker-recovery-cut!)
(def ^:private read-cut recovery/read-local-recovery-cut!)
(def ^:private read-acquisition retained/read-acquisition!)
(def ^:private check-temporal temporal/check-selection!)
(def ^:private read-invariant recovery/read-local-recovery-invariant-evidence!)
(def ^:private compose-refinement refinement/compose!)
(def ^:private current-token xtdb/current-submission-policy-token)
(def ^:private active-root policy/activated-policy-root)
(def ^:private owned-state xtdb/owned-submission-policy-state)
(def ^:private read-observation publication/read-framed-submission-observation!)
(def ^:private qualified-histories (java.util.WeakHashMap.))
(def ^:private derive-clm-r2 clm-r2/derive!)
(def ^:private declare-trust recovery/declare-conditional-recovery-trust!)
(def refined-judgment-profile :selected-conditional-activated-recovery-refinement-judgment-v1)

(defn- reject! [reason]
  (throw (ex-info "Unqualified installed writer history" {:reason reason})))

(defn- boundaries! []
  (when-not (and (identical? classify installed/derive!)
                 (identical? capture recovery/capture-local-worker-recovery-cut!)
                 (identical? read-cut recovery/read-local-recovery-cut!)
                 (identical? read-acquisition retained/read-acquisition!)
                 (identical? check-temporal temporal/check-selection!)
                 (identical? read-invariant recovery/read-local-recovery-invariant-evidence!)
                 (identical? compose-refinement refinement/compose!)
                 (identical? current-token xtdb/current-submission-policy-token)
                 (identical? active-root policy/activated-policy-root)
                 (identical? owned-state xtdb/owned-submission-policy-state)
                 (identical? read-observation publication/read-framed-submission-observation!))
    (reject! :installed-history-boundary-replaced)))

(defn- partition! [classified steps compared cut]
  (let [{:keys [manifest-tx-id initialization-tx-id source-id]} (:activation-root classified)
        ids (:transaction-ids cut)
        selected (:selected-tx-ids compared)
        commitments (:selected-commitments compared)
        worker (:worker-correspondence compared)
        branches (:reviewed-policy-branches classified)
        identities (:alternative-identities classified)
        branch-for {:lease-acquisition :lease-acquisition
                    :expired-lease-reclamation :lease-reclaim
                    :dispatch-acknowledgment :dispatch-acknowledgment}]
    (when-not (and (= :conditional-current-installed-writer-classification (:status classified))
                   (= :matched (:status compared))
                   (= :matched-local-recovery-cut (:recovery-cut-binding compared))
                   (= :matched-local-activated-retained-root (:activation-root-binding compared))
                   (= :matched-issued-origin-and-retained-sample (:local-acquisition-binding compared))
                   (= :matched-local-issued-host-capture (:worker-observation-provenance compared))
                   (= :matched-issued-worker-retained-transactions (:maintenance-correspondence worker))
                   (= :matched-issued-managed-lineage (:worker-lineage-correspondence worker))
                   (string? (:payload-fingerprint cut))
                   (= (:payload-fingerprint cut) (:retained-payload-fingerprint worker))
                   (= 3 (count steps) (count selected))
                   (= selected (:selected-tx-ids worker) (mapv #(get-in % [:expectation :tx-id]) steps))
                   (= [:committed :aborted :committed] commitments)
                   (= (:intervening-maintenance compared)
                      (mapv #(select-keys % [:tx-id :intent-id :maintenance]) (:selected-maintenance worker)))
                   (= manifest-tx-id (:manifest-tx-id compared))
                   (= initialization-tx-id (:initialization-tx-id compared))
                   (vector? ids) (<= 5 (count ids)) (every? integer? ids) (apply < ids)
                   (= [manifest-tx-id initialization-tx-id] (subvec ids 0 2)))
      (reject! :installed-history-evidence-mismatch))
    (let [commands (mapv (fn [step id commitment]
                           (let [alternative (get-in step [:spec :alternative])
                                 identity (get identities alternative)]
                             (when-not (and identity (= alternative (get-in step [:original :subject :id]))
                                            (= source-id (:source-id step))
                                            (= (:catalog-fingerprint classified) (:catalog-fingerprint identity)
                                               (get-in step [:context :catalog-fingerprint]))
                                            (= (:induction-fingerprint classified) (get-in step [:context :induction-fingerprint]))
                                            (seq (get-in step [:original :writes]))
                                            (= :conditional-abstract-transition (get-in branches [:mapped-operation :effect])))
                               (reject! :installed-history-alternative-mismatch))
                             {:tx-id id :class (if (= :committed commitment) :committed-mapped :aborted-attempt)
                              :alternative identity})) steps selected commitments)
          workers (mapv (fn [{:keys [tx-id maintenance committed?] :as w}]
                          (let [branch (branch-for maintenance)]
                            (when-not (and branch (true? committed?)
                                           (= :conditional-mapped-stutter (get-in branches [branch :effect]))
                                           (= source-id (get-in w [:worker-origin :source-id])))
                              (reject! :installed-history-maintenance-mismatch))
                            {:tx-id tx-id :class :committed-worker :policy-branch branch}))
                        (:selected-maintenance worker))
          all (vec (sort-by :tx-id (concat commands workers)))]
      ;; Exact sequence equality rejects duplicates, gaps and unclassified
      ;; suffixes. Source offsets need not be numerically consecutive.
      (when-not (= (subvec ids 2) (mapv :tx-id all))
        (reject! :unclassified-retained-transaction))
      all)))

(defn derive!
  "Recheck issued recovery against the current mapped policy root. Never
   accept supplied reports. The cut covers retained transaction IDs, not
   every possible writer, temporal boundary, observation or future event.
   Historical command origin does not establish current-generation origin.
   Returns sensitive existing recovery diagnostics; do not publish."
  [system tables admission source declarations fingerprint cases steps observation acquisition worker-capture]
  (let [base {:profile profile :authority :none :certificate :not-issued
              :proof-discharge :not-performed :writer-coverage :not-established
              :universal-refinement :not-established :temporal-coverage :not-established}]
    (try
      (boundaries!)
      (let [node (:xtdb/connectable system)
            token (current-token node)
            root (when token (active-root node admission token))]
        (when-not (and root (= root (:activation-root (read-observation admission observation))))
          (reject! :installed-history-root-mismatch))
        (let [classified (classify node admission source declarations fingerprint cases)]
          (when-not (and (= :conditional-current-installed-writer-classification (:status classified))
                         (= :open (:owned-policy-phase classified))
                         (= :open (:phase (owned-state node token))))
            (reject! :unqualified-installed-history-policy))
          (let [compared (capture system tables admission source declarations steps observation acquisition worker-capture)
                cut (read-cut admission observation (:recovery-cut-observation compared))
                partition (partition! classified steps compared cut)
                sample (read-acquisition node acquisition)
                _ (when-not (and sample (= cut (select-keys sample (keys cut))))
                    (reject! :installed-history-temporal-sample-mismatch))
                temporal-report (check-temporal (:replay-inputs sample) cut partition
                                                (:protected-tables classified))
                refinement-report (compose-refinement source declarations fingerprint cases classified cut partition temporal-report
                                                      (read-invariant admission observation (:recovery-cut-observation compared)))]
            (boundaries!)
            (when-not (and (identical? token (current-token node))
                           (= root (active-root node admission token)))
              (reject! :installed-history-generation-changed))
            (when-not (= :open (:phase (owned-state node token)))
              (reject! :installed-history-owned-policy-not-open))
            (let [result (merge base {:status :conditional-installed-retained-writer-history
                         :scope :selected-issued-post-initialization-transaction-cut
                         :retained-transaction-classification :complete-at-selected-cut
                         :lifecycle-evidence :open-owned-generation-at-qualification
                         :post-initialization-steps partition
                         :selected-temporal-coverage temporal-report
                         :selected-physical-refinement refinement-report
                         :remaining-conditions (into (into (into (:remaining-conditions classified) (:remaining-conditions temporal-report)) (:remaining-conditions refinement-report))
                                                     #{:historical-generation-and-continuous-mediation
                                                       :provider-and-retained-source-completeness
                                                       :complete-read-only-and-temporal-observation-coverage})
                         :recovery compared})]
              ;; Do not retain the key (the cut handle) in its own weak-map value.
              (locking qualified-histories
                (.put qualified-histories (:recovery-cut-observation compared)
                      {:admission admission :observation observation
                       :source source :declarations declarations :fingerprint fingerprint :cases cases
                       :owner (java.lang.ref.WeakReference. system) :tables tables :steps steps
                       :acquisition acquisition :worker-capture worker-capture
                       :generation token :activation root
                       :history (dissoc result :recovery)}))
              result))))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-installed-history))))))


(defn- judgment-boundaries! []
  (boundaries!)
  (when-not (and (identical? derive-clm-r2 clm-r2/derive!)
                 (identical? declare-trust recovery/declare-conditional-recovery-trust!))
    (reject! :refined-judgment-boundary-replaced)))

(defn- qualified-history [admission observation handle]
  (locking qualified-histories
    (let [entry (.get qualified-histories handle)]
      (when (and entry (identical? admission (:admission entry))
                 (identical? observation (:observation entry)))
        entry))))

(defn read-qualified-history!
  "Read the exact issued qualification for its original owner, inputs,
   acquisition and worker capture while the original policy is open."
  [system tables admission source declarations steps observation acquisition worker-capture handle]
  (boundaries!)
  (let [entry (qualified-history admission observation handle)
        node (:xtdb/connectable system)
        token (current-token node)
        cut (read-cut admission observation handle)
        sample (read-acquisition node acquisition)]
    (when-not (and entry (identical? system (.get ^java.lang.ref.WeakReference (:owner entry)))
                   (= tables (:tables entry)) (= steps (:steps entry))
                   (= source (:source entry)) (= declarations (:declarations entry))
                   (identical? acquisition (:acquisition entry))
                   (identical? worker-capture (:worker-capture entry))
                   token (identical? token (:generation entry))
                   (= :open (:phase (owned-state node token)))
                   (= (:activation entry) (active-root node admission token))
                   cut sample (= cut (select-keys sample (keys cut))))
      (reject! :unissued-qualified-writer-history))
    (boundaries!)
    {:history (:history entry) :cut cut}))

(defn join-configured-envelope!
  "Internal composition over rederived configured correspondence and an
   issued history read by the owner boundary. Not a public authority."
  [qualified physical root sample]
  (let [{:keys [history cut]} qualified
        partition (:post-initialization-steps history)
        refinement (:selected-physical-refinement history)
        temporal (:selected-temporal-coverage history)
        ids (:transaction-ids cut)
        commands (:abstract-steps physical)
        maintenance (get-in physical [:maintenance-correspondence :steps])
        branch-for {:lease-acquisition :lease-acquisition
                    :expired-lease-reclamation :lease-reclaim
                    :dispatch-acknowledgment :dispatch-acknowledgment}
        expected (vec (sort-by :tx-id
                              (concat (map (fn [s]
                                             {:tx-id (:tx-id s) :alternative (:identity s)
                                              :class (case (:relation s)
                                                       :committed-abstract-transition :committed-mapped
                                                       :aborted-stutter :aborted-attempt nil)}) commands)
                                      (map (fn [s] {:tx-id (:tx-id s) :class :committed-worker
                                                    :policy-branch (branch-for (:maintenance s))}) maintenance))))]
    (when-not (and (= :conditional-installed-retained-writer-history (:status history))
                   (= :complete-at-selected-cut (:retained-transaction-classification history))
                   (= :conditional-selected-physical-refinement (:status refinement))
                   (= :conditional-selected-diagonal-coverage (:status temporal))
                   (= :not-performed (:proof-discharge history) (:proof-discharge refinement) (:proof-discharge temporal))
                   (= :not-established (:writer-coverage history) (:universal-refinement history) (:temporal-coverage history))
                   (= cut (select-keys sample (keys cut)))
                   (= (:payload-fingerprint cut) (get-in physical [:maintenance-correspondence :retained-payload-fingerprint]))
                   (= [(:manifest-tx-id root) (:initialization-tx-id root)] (subvec ids 0 2))
                   (= (:selected-tx-ids physical) (mapv :tx-id commands))
                   (= (subvec ids 2) (mapv :tx-id expected) (mapv :tx-id partition)
                      (mapv :tx-id (:steps refinement)) (mapv :to-tx-id (:intervals temporal)))
                   (= expected partition (mapv #(dissoc % :schema :relation) (:steps refinement)))
                   (every? :class expected)
                   (every? :policy-branch (filter #(= :committed-worker (:class %)) expected))
                   (= (mapv :schema (:steps refinement)) (mapv :schema (:intervals temporal)))
                   (= (mapv :relation (:steps refinement))
                      (mapv #(case (:class %) :committed-worker :maintenance-stutter
                                             :committed-mapped :committed-abstract-transition
                                             :aborted-attempt :aborted-stutter nil) expected)))
      (reject! :configured-writer-history-mismatch))
    {:status :conditional-configured-selected-writer-history
     :scope :complete-selected-post-initialization-prefix
     :historical-cut cut :post-initialization-steps partition
     :selected-temporal-coverage temporal :selected-physical-refinement refinement
     :remaining-conditions (:remaining-conditions history)
     :writer-coverage :not-established :temporal-coverage :not-established
     :universal-refinement :not-established :proof-discharge :not-performed
     :authority :none :certificate :not-issued}))

(defn- join-judgment! [history cut checked binding judgment]
  (let [refinement (:selected-physical-refinement history)
        subject (:judgment-subject judgment)
        register (:original-open-obligation-ids-by-operation binding)
        by-operation (:original-obligations-by-operation binding)
        inventory (:obligation-inventory judgment)
        selected (filterv :alternative (:steps refinement))]
    (when-not (and (= :conditional (:status judgment))
                   (= recovery/judgment-profile (:judgment-profile judgment))
                   (= :conditional-installed-retained-writer-history (:status history))
                   (= :conditional-selected-physical-refinement (:status refinement))
                   (= :conditional-original-clm-r2-binding (:status binding))
                   (= :open (:original-obligation-status binding))
                   (= :not-performed (:proof-discharge history) (:proof-discharge refinement)
                      (:proof-discharge binding) (:proof-discharge judgment) (:proof-discharge inventory))
                   (= :not-performed (:premise-acceptance binding))
                   (= :not-established (:universal-refinement history) (:universal-refinement refinement)
                      (:universal-refinement binding) (:universal-refinement inventory))
                   (= :not-established (:writer-coverage history) (:temporal-coverage history)
                      (:replay-observation-coverage refinement))
                   (some? checked) (= checked (:selected-invariant-evidence judgment))
                   (= :checked (:status checked)) (string? (:recovery-cut-ref checked))
                   (= cut (:historical-cut checked) (get-in judgment [:judgment-scope :historical-cut]))
                   (= (:recovery-cut-ref checked) (get-in judgment [:physical-refinement-premises :scope :recovery-cut-ref]))
                   (= subject (:judgment-subject checked))
                   (= (:catalog-fingerprint binding) (:catalog-fingerprint subject))
                   (= (:induction-fingerprint binding) (:induction-fingerprint subject))
                   (= (:abstract-report-fingerprint binding) (:abstract-induction-fingerprint subject)
                      (get-in inventory [:abstract :report-fingerprint]))
                   (= register (get-in inventory [:abstract :original-obligations :by-operation]))
                   (= (:abstract-open-assumptions binding) (get-in inventory [:abstract :open]))
                   (= (subvec (:transaction-ids cut) 2) (mapv :tx-id (:steps refinement)))
                   (= (get-in judgment [:original-recovery :selected-tx-ids]) (mapv :tx-id selected))
                   (= (get-in judgment [:original-recovery :selected-commitments])
                      (mapv #(case (:class %) :committed-mapped :committed :aborted-attempt :aborted nil) selected))
                   (= (mapv #(select-keys (:alternative %) [:alternative-id :mapping-fingerprint :transition-fingerprint]) selected)
                      (mapv #(select-keys % [:alternative-id :mapping-fingerprint :transition-fingerprint]) (:selected-operations subject)))
                   (every? #(= (:alternative %) (get-in by-operation [(get-in % [:alternative :alternative-id]) :identity])) selected)
                   (= (:declared-replay-schemas refinement)
                      (set (keys (get-in binding [:argument :schemas :gesso.assurance.reference-admitted-step-argument/replay :alternatives])))))
      (reject! :refined-judgment-cut-or-subject-mismatch))
    {:status :conditional :judgment-profile refined-judgment-profile
     :scope :original-issued-selected-history-and-separate-general-argument
     :historical-judgment judgment
     :judgment-subject subject :historical-cut cut :recovery-cut-ref (:recovery-cut-ref checked)
     :original-clm-r2-binding binding :selected-physical-refinement refinement
     :selected-temporal-coverage (:selected-temporal-coverage history)
     :obligation-inventory inventory
     :selected-remaining-conditions (:remaining-conditions history)
     :general-remaining-conditions (:remaining-conditions binding)
     :condition-scope :separate-selected-and-universal-no-implication
     :universal-refinement :not-established :writer-coverage :not-established
     :temporal-coverage :not-established :proof-discharge :not-performed
     :new-premise-acceptance :not-performed :authority :none :certificate :not-issued}))

(defn declare-refined-recovery-trust!
  "Compose a separately issued writer-history cut with the exact prepared
   historical judgment. The declaration is the original storage trust
   declaration only; it grants no new authority. Re-derive the general
   CLM-R2 binding from the captured original admission and source. The
   original v12 judgment is preserved in full. No supplied diagnostic
   report or matching-content cut may substitute for the issued cut."
  [admission observation cut-handle conditional-handle declaration]
  (try
    (judgment-boundaries!)
    (let [{:keys [source declarations fingerprint cases history] :as entry}
          (qualified-history admission observation cut-handle)]
      (when-not entry (reject! :unissued-qualified-writer-history))
      (let [cut (read-cut admission observation cut-handle)
            checked (read-invariant admission observation cut-handle)
            judgment (declare-trust conditional-handle declaration)]
        ;; Reject the wrong comparison before rechecking the expensive general argument.
        (when-not (and cut checked (= :conditional (:status judgment))
                       (= checked (:selected-invariant-evidence judgment)))
          (reject! :refined-judgment-cut-or-subject-mismatch))
        (let [binding (derive-clm-r2 admission source declarations fingerprint cases)
              result (join-judgment! history cut checked binding judgment)]
          (judgment-boundaries!)
          result)))
    (catch clojure.lang.ExceptionInfo e
      {:status :not-established :reason (or (:reason (ex-data e)) :unqualified-refined-judgment)
       :proof-discharge :not-performed :authority :none :certificate :not-issued})))
