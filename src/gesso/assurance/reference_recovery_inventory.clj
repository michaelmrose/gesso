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
