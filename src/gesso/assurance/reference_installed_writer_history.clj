(ns gesso.assurance.reference-installed-writer-history
  "Conditional classification of the exact issued retained transaction cut."
  (:require [gesso.assurance.reference-installed-writer-classification :as installed]
            [gesso.assurance.reference-mapped-observed-recovery :as recovery]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.live.outbox.writer-policy :as policy]))

(def profile :conditional-installed-retained-writer-history-v1)
(def ^:private classify installed/derive!)
(def ^:private capture recovery/capture-local-worker-recovery-cut!)
(def ^:private read-cut recovery/read-local-recovery-cut!)
(def ^:private current-token xtdb/current-submission-policy-token)
(def ^:private active-root policy/activated-policy-root)
(def ^:private read-observation publication/read-framed-submission-observation!)

(defn- reject! [reason]
  (throw (ex-info "Unqualified installed writer history" {:reason reason})))

(defn- boundaries! []
  (when-not (and (identical? classify installed/derive!)
                 (identical? capture recovery/capture-local-worker-recovery-cut!)
                 (identical? read-cut recovery/read-local-recovery-cut!)
                 (identical? current-token xtdb/current-submission-policy-token)
                 (identical? active-root policy/activated-policy-root)
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
          (when-not (= :conditional-current-installed-writer-classification (:status classified))
            (reject! :unqualified-installed-history-policy))
          (let [compared (capture system tables admission source declarations steps observation acquisition worker-capture)
                cut (read-cut admission observation (:recovery-cut-observation compared))
                partition (partition! classified steps compared cut)]
            (boundaries!)
            (when-not (and (identical? token (current-token node))
                           (= root (active-root node admission token)))
              (reject! :installed-history-generation-changed))
            (merge base {:status :conditional-installed-retained-writer-history
                         :scope :selected-issued-post-initialization-transaction-cut
                         :retained-transaction-classification :complete-at-selected-cut
                         :post-initialization-steps partition
                         :remaining-conditions (into (:remaining-conditions classified)
                                                     #{:historical-generation-and-continuous-mediation
                                                       :provider-and-retained-source-completeness
                                                       :complete-read-only-and-temporal-observation-coverage})
                         :recovery compared}))))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-installed-history))))))
