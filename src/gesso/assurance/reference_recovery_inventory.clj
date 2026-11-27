(ns gesso.assurance.reference-recovery-inventory
  "Descriptive inventory of the exact selected judgment's declared obligations.")

(def profile :selected-recovery-obligation-inventory-v1)

(defn- inventory-error! [reason]
  (throw (ex-info "Invalid composed recovery obligation inventory." {:reason reason})))

(defn- dependency-order! [dependencies]
  (loop [remaining dependencies ordered []]
    (if (empty? remaining)
      ordered
      (let [ready (sort (keep (fn [[id deps]] (when (empty? deps) id)) remaining))]
        (when-not (seq ready) (inventory-error! :cyclic-or-missing-premise-dependency))
        (recur (into {} (map (fn [[id deps]] [id (reduce disj deps ready)]))
                     (apply dissoc remaining ready))
               (into ordered ready))))))

(defn describe-inventory
  "Index every declared premise without treating a symbolic abstract obligation
   as a canonical premise fingerprint. Only storage declarations are accepted;
   none is discharged. Dependency checks are structural and family-local, not
   a proof of cross-family implication or universal refinement. Internal only."
  [abstract storage command worker physical]
  (let [classification (:classification abstract)
        open (:open classification)
        originals (:original-obligations classification)
        families {:storage storage :command command :worker worker :physical physical}
        accepted (:trusted-assumptions storage)
        entries (mapv (fn [[family report]]
                        (let [obligations (:obligations report)
                              fingerprints (set (map :premise-fingerprint obligations))]
                          (when-not (and (vector? obligations)
                                         (= fingerprints (if (= :storage family) accepted
                                                           (:open-premise-fingerprints report)))
                                         (= (:premise-acceptance report)
                                            (if (= :storage family) :explicit-declaration-only :not-performed))
                                         (every? #(and (= :open (:discharge-state %))
                                                       (= :not-admitted (:admission %))
                                                       (= :not-performed (:checking %))
                                                       (string? (:premise-fingerprint %))
                                                       (keyword? (get-in % [:assumption :id]))
                                                       (set? (get-in % [:assumption :dependencies]))) obligations))
                            (inventory-error! :invalid-premise-family))
                          [family obligations])) families)
        all (vec (mapcat second entries))
        by-id (into {} (map (fn [d] [(get-in d [:assumption :id]) (:premise-fingerprint d)])) all)
        fingerprints (set (vals by-id))]
    (when-not (= (count all) (count by-id) (count fingerprints))
      (inventory-error! :duplicate-premise-identity))
    (when-not (and (= :all-original-obligations-remain-open (:status originals))
                   (= (:declared-operation-ids abstract) (set (keys (:by-operation originals))))
                   (set? (:assumptions open))
                   (map? (:assumption-classes open))
                   (every? set? (vals (:assumption-classes open)))
                   (= (:assumptions open) (into #{} cat (vals (:assumption-classes open))))
                   (= (count (:assumptions open)) (reduce + (map count (vals (:assumption-classes open)))))
                   (set? (:certificate-premises open))
                   (= (:certificate-premises open) (get-in classification [:certificate :open-premises])))
      (inventory-error! :invalid-abstract-obligation-inventory))
    (let [groups (into {} (map (fn [[family obligations]]
                                 [family (set (map :premise-fingerprint obligations))])) entries)
          index (into {} (for [[family obligations] entries
                              d obligations
                              :let [fp (:premise-fingerprint d)
                                    id (get-in d [:assumption :id])
                                    deps (set (map by-id (get-in d [:assumption :dependencies])))]]
                          (do (when-not (every? (get groups family) deps)
                                (inventory-error! :missing-or-cross-family-premise-dependency))
                              [fp {:family family :id id :dependency-fingerprints deps
                                   :acceptance (if (contains? accepted fp) :explicit-declaration-only :not-performed)
                                   :discharge-state :open}])))]
      {:profile profile
       :scope :selected-historical-judgment-only
       :abstract {:report-fingerprint (:fingerprint abstract)
                  :checked-abstract (:checked-abstract classification)
                  :open-abstract (:open-abstract classification)
                  :certificate (:certificate classification)
                  :dependency-closure (:dependency-closure classification)
                  :original-obligations originals :open open
                  :unsupported (:unsupported classification)
                  :premise-acceptance :not-performed}
       :premise-families groups
       :premises-by-fingerprint index
       :dependency-order (dependency-order! (into {} (map (fn [[fp v]] [fp (:dependency-fingerprints v)])) index))
       :dependency-classification :declared-family-local-structure-only
       :cross-family-implications :not-established
       :all-described-open-premise-fingerprints fingerprints
       :deployment-declared-premise-fingerprints accepted
       :unaccepted-premise-fingerprints (reduce disj fingerprints accepted)
       :deployment-acceptance (:deployment-acceptance storage)
       :proof-discharge :not-performed
       :universal-refinement :not-established
       :authority :none :certificate :not-issued})))

(def transfer-profile :selected-physical-invariant-transfer-v1)

(def ^:private transfer-premise-ids
  {:faithful-pinned-provider-readbacks :reference-physical/snapshot-continuity
   :faithful-authoritative-state-projection :reference-physical/authoritative-projection
   :faithful-invariant-expression-semantics :reference-physical/invariant-expression-semantics})

(defn describe-transfer-binding
  "Relate the original issued finite conclusion to the exact open physical
   premise inventory. This is a dependency declaration, not a derivation of
   the premises or a proof that these are sufficient for CLM-R2. Internal only;
   callers must supply the original registry entry and freshly built inventory."
  [checked physical inventory abstract]
  (let [base {:profile transfer-profile :scope :original-selected-invariant-coordinates-only
              :proof-discharge :not-performed :premise-acceptance :not-performed
              :universal-refinement :not-established :authority :none :certificate :not-issued}
        conclusion (:conditional-physical-invariant checked)
        interface (:scope physical)
        obligations (:obligations physical)
        by-id (into {} (map (fn [d] [(get-in d [:assumption :id]) d])) obligations)
        by-fp (into {} (map (juxt :premise-fingerprint identity)) obligations)
        index (:premises-by-fingerprint inventory)
        group (get-in inventory [:premise-families :physical])
        binding (into {} (map (fn [[condition id]]
                               [condition (:premise-fingerprint (get by-id id))])) transfer-premise-ids)
        fail #(assoc base :status :not-established :reason %)]
    (cond
      (not (and (= :checked (:status checked)) (= :conditional (:status conclusion))
                (= :declared-invariants-hold-on-selected-physical-projections (:conclusion conclusion))
                (= (set (keys transfer-premise-ids)) (:conditional-on conclusion))
                (= :matched (get-in checked [:physical-readbacks :status]))
                (seq (:checked-coordinates conclusion))
                (= (:checked-coordinates conclusion) (get-in checked [:physical-readbacks :checked-coordinates]))
                (seq (:invariant-ids conclusion))
                (= (:invariant-ids checked) (:invariant-ids conclusion)
                   (:declared-invariant-ids abstract))))
      (fail :unqualified-finite-physical-conclusion)

      (not (and (= profile (:profile inventory))
                (= :selected-historical-cut-only (:scope-kind interface))
                (string? (:recovery-cut-ref checked))
                (= (:recovery-cut-ref checked) (:recovery-cut-ref interface))
                (some? (:historical-cut checked)) (= (:historical-cut checked) (:historical-cut interface))
                (some? (:judgment-subject checked)) (= (:judgment-subject checked) (:judgment-subject interface))
                (= (:catalog-fingerprint checked) (get-in interface [:judgment-subject :catalog-fingerprint]))
                (= (:retained-payload-fingerprint checked) (get-in interface [:historical-cut :payload-fingerprint]))
                (= (:selected-tx-ids checked) (:selected-tx-ids interface))
                (string? (:fingerprint abstract))
                (= (:fingerprint abstract) (get-in inventory [:abstract :report-fingerprint])
                   (get-in interface [:judgment-subject :abstract-induction-fingerprint]))
                (= (count obligations) (count by-id) (count by-fp))
                (= (set (keys by-fp)) group (:open-premise-fingerprints physical))
                (every? string? (vals binding))
                (every? (fn [[fp d]]
                          (let [entry (get index fp)]
                            (and (= interface (get-in d [:assumption :subject :interface]))
                                 (= :open (:discharge-state d) (:discharge-state entry))
                                 (= :not-performed (:acceptance entry))
                                 (= :physical (:family entry))
                                 (= (get-in d [:assumption :id]) (:id entry))
                                 (= (set (map #(get-in by-id [% :premise-fingerprint])
                                              (get-in d [:assumption :dependencies])))
                                    (:dependency-fingerprints entry))
                                 (contains? (:unaccepted-premise-fingerprints inventory) fp)
                                 (contains? (:all-described-open-premise-fingerprints inventory) fp)))) by-fp)))
      (fail :unqualified-original-transfer-premises)

      :else
      (let [required (reduce (fn [seen fp]
                               (if (or (some #{fp} (vals binding))
                                       (some #(contains? (get-in index [% :dependency-fingerprints]) fp) seen))
                                 (conj seen fp) seen))
                             #{} (reverse (:dependency-order inventory)))]
        (if (and (every? required (vals binding))
                 (every? group required)
                 (every? #(every? required (get-in index [% :dependency-fingerprints])) required))
          (assoc base :status :bound
                 :recovery-cut-ref (:recovery-cut-ref checked)
                 :abstract-report-fingerprint (:fingerprint abstract)
                 :invariant-ids (:invariant-ids conclusion)
                 :condition-premise-fingerprints binding
                 :required-open-premise-fingerprints required
                 :dependency-order (filterv required (:dependency-order inventory))
                 :dependency-classification :declared-transfer-requirements-only
                 :sufficiency :not-established)
          (fail :incomplete-transfer-dependency-closure))))))
