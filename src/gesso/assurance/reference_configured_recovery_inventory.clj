(ns gesso.assurance.reference-configured-recovery-inventory
  "Internal structural inventory for an already checked owner recovery join.
   Neither a standalone admission boundary nor a proof of any premise."
  (:require [clojure.set :as set]
            [gesso.assurance.assumption :as assumption]
            [gesso.assurance.reference-command-authority :as command-authority]
            [gesso.assurance.reference-command-attribution-argument :as attribution]
            [gesso.live.outbox.http-route :as route]))

(def profile :configured-selected-recovery-obligation-inventory-v1)
(def ^:private original-describe assumption/describe-assumption)

(defn- require! [condition reason]
  (when-not condition
    (throw (ex-info "Unqualified configured recovery inventory" {:reason reason}))))

(defn- implementation-family! [family report descriptions]
  (let [by-id (into {} (map (fn [d] [(get-in d [:assumption :id]) d])) descriptions)
        fps (set (map :premise-fingerprint descriptions))]
    (require! (and (= (count descriptions) (count by-id) (count fps))
                   (= fps (:trusted-assumptions report))
                   (= :configured-local-host (:deployment-adoption report))
                   (= :explicit-declaration-only (:premise-acceptance report))
                   (= :not-performed (:proof-discharge report))
                   (every? #(= % (original-describe (:assumption %))) descriptions))
              :configured-inventory-implementation-premises)
    (let [index (into {} (for [d descriptions
                              :let [deps (get-in d [:assumption :dependencies])]]
                          (do (require! (every? by-id deps) :configured-inventory-missing-dependency)
                              [(:premise-fingerprint d)
                               {:family family :id (get-in d [:assumption :id])
                                :dependency-fingerprints (set (map #(get-in by-id [% :premise-fingerprint]) deps))
                                :acceptance :explicit-declaration-only :discharge-state :open}])))]
      {:fingerprints fps :index index :by-id by-id})))

(defn- dependency-order! [index]
  (loop [remaining (into {} (map (fn [[fp row]] [fp (:dependency-fingerprints row)])) index)
         ordered []]
    (if (empty? remaining) ordered
      (let [ready (sort (keep (fn [[fp deps]] (when (empty? deps) fp)) remaining))]
        (require! (seq ready) :configured-inventory-invalid-dependencies)
        (recur (into {} (map (fn [[fp deps]] [fp (reduce disj deps ready)]))
                     (apply dissoc remaining ready))
               (into ordered ready))))))

(defn compose!
  "Index the exact historical and configured implementation premises supplied
   by the owner-locked join. Preserve selected and general conditions separately.
   This pure structural helper does not establish the provenance of its inputs."
  [configured physical history refined adopted]
  (let [binding (:original-clm-r2-binding configured)
        original (:obligation-inventory refined)
        general (:remaining-conditions binding)
        historical (:historical-judgment refined)
        storage-fps (:trusted-assumptions adopted)
        writer (implementation-family! :writer-isolation (:writer-isolation configured)
                                       (get-in configured [:writer-isolation :obligations]))
        provider (implementation-family! :provider-trust (:provider-trust configured)
                                          (vals (get-in configured [:provider-trust :premises])))
        original-index (:premises-by-fingerprint original)
        original-fps (:all-described-open-premise-fingerprints original)
        writer-fps (:fingerprints writer) provider-fps (:fingerprints provider)
        adopted-fps (set/union storage-fps writer-fps provider-fps)
        links (:condition-links-by-contract configured)
        writer-links (:writer-isolation links) provider-links (:provider-trust links)
        selected-physical (:remaining-conditions physical)
        selected-history (:remaining-conditions history)
        selected (set/union selected-physical selected-history)
        groups (assoc (:premise-families original) :writer-isolation writer-fps :provider-trust provider-fps)
        index (merge original-index (:index writer) (:index provider))
        all (set (keys index))]
    (require! (and (identical? original-describe assumption/describe-assumption)
                   (= :conditional-configured-implementation-argument (:status configured))
                   (= :conditional-original-clm-r2-binding (:status binding))
                   (= :conditional-configured-selected-writer-history (:status history))
                   (= :conditional-physical-source-envelope-recovery (:status physical))
                   (= :conditional (:status refined) (:status historical) (:status adopted))
                   (= :configured-local-host (:deployment-adoption adopted))
                   (= (dissoc adopted :deployment-adoption) historical)
                   (= binding (:original-clm-r2-binding refined))
                   (= (:abstract-report-fingerprint binding) (get-in original [:abstract :report-fingerprint]))
                   (= (:original-open-obligation-ids-by-operation binding)
                      (get-in original [:abstract :original-obligations :by-operation]))
                   (= (:historical-cut history) (:historical-cut refined) (get-in adopted [:judgment-scope :historical-cut]))
                   (= general (:remaining-conditions configured) (:general-remaining-conditions refined)
                      (get-in physical [:general-argument :general-conditions]))
                   (set? general) (set? selected-physical) (set? selected-history)
                   (= selected-history (:selected-remaining-conditions refined))
                   (set/subset? general selected-physical)
                   (= storage-fps (get-in original [:premise-families :storage])
                      (:deployment-declared-premise-fingerprints original))
                   (= storage-fps (set (map :premise-fingerprint (:obligations adopted))))
                   (every? #(= % (original-describe (:assumption %))) (:obligations adopted))
                   (= 3 (count storage-fps)) (= 5 (count writer-fps)) (= 8 (count provider-fps))
                   (= 16 (count adopted-fps))
                   (= (:trusted-assumptions-by-contract configured)
                      {:writer-isolation writer-fps :provider-trust provider-fps})
                   (= original-fps (set (keys original-index)))
                   (= (count all) (+ (count original-fps) (count writer-fps) (count provider-fps)))
                   (= all (into #{} cat (vals groups)))
                   (= (count all) (reduce + (map count (vals groups))))
                   (= (count all) (count (set (map :id (vals index)))))
                   (= :related-not-entailing (:condition-link-semantics configured))
                   (set? writer-links) (map? provider-links)
                   (set/subset? (set/union writer-links (set (keys provider-links))) general)
                   (every? #(and (set? %) (seq %) (every? (:by-id provider) %)) (vals provider-links))
                   (= :not-performed (:proof-discharge configured) (:proof-discharge physical)
                      (:proof-discharge history) (:proof-discharge refined) (:proof-discharge original))
                   (every? (fn [[fp {:keys [family dependency-fingerprints acceptance discharge-state]}]]
                             (and (= :open discharge-state) (contains? (get groups family) fp)
                                  (set? dependency-fingerprints) (every? (get groups family) dependency-fingerprints)
                                  (= acceptance (if (adopted-fps fp) :explicit-declaration-only :not-performed)))) index))
              :configured-inventory-source-or-scope-mismatch)
    {:profile profile :status :conditional-configured-obligation-inventory
     :subject (:subject configured) :activation-root (:activation-root configured)
     :historical-cut (:historical-cut history)
     :abstract-report-fingerprint (:abstract-report-fingerprint binding)
     :original-open-obligations (:original-open-obligation-ids-by-operation binding)
     :original-historical-inventory original
     :premise-families groups :premises-by-fingerprint index
     :dependency-order (dependency-order! index)
     :adopted-by-contract {:storage-lineage storage-fps :writer-isolation writer-fps :provider-trust provider-fps}
     :adoption-scopes {:storage-lineage (:judgment-scope adopted)
                       :writer-isolation (get-in configured [:writer-isolation :scope])
                       :provider-trust (get-in configured [:provider-trust :scope])}
     :all-described-open-premise-fingerprints all
     :deployment-declared-premise-fingerprints adopted-fps
     :unaccepted-premise-fingerprints (set/difference all adopted-fps)
     :general-condition-links links
     :storage-relevance {:scope :selected-historical-comparison-only
                         :premise-fingerprints storage-fps
                         :general-condition-links :not-derived}
     :condition-link-semantics :related-not-entailing
     :remaining-general-conditions general
     :remaining-selected-physical-conditions selected-physical
     :remaining-selected-history-conditions selected-history
     :remaining-conditions selected
     :dependency-classification :declared-family-local-structure-only
     :cross-family-implications :not-established
     :proof-discharge :not-performed :universal-refinement :not-established
     :authority :none :certificate :not-issued}))

(def command-authority-profile :configured-selected-recovery-obligation-inventory-v3)
(def ^:private original-command-description command-authority/describe!)
(def ^:private original-authority-acceptance? route/authority-acceptance?)
(def ^:private original-authority-binding? route/authority-binding?)
(def ^:private original-attribution attribution/derive!)

(defn extend-command-authority!
  "Extend an already composed inventory with the original owner's adopted
   command authority contract. Structural join only; no provenance or proof."
  [inventory refined adopted]
  (let [command (get-in refined [:historical-judgment :command-authentication-premises])
        binding (:installation-binding adopted)
        declaration (:deployment-acceptance adopted)
        fps (:trusted-assumptions adopted)
        original (:original-historical-inventory inventory)
        original-command (get-in original [:premise-families :command])
        index (:premises-by-fingerprint inventory)
        all (:all-described-open-premise-fingerprints inventory)
        accepted (:deployment-declared-premise-fingerprints inventory)]
    (require! (and (identical? original-describe assumption/describe-assumption)
                   (identical? original-command-description command-authority/describe!)
                   (identical? original-authority-acceptance? route/authority-acceptance?)
                   (identical? original-authority-binding? route/authority-binding?)
                   (identical? original-attribution attribution/derive!)
                   (= profile (:profile inventory))
                   (= :conditional-configured-obligation-inventory (:status inventory))
                   (= original (:obligation-inventory refined))
                   (= (:historical-cut inventory) (:historical-cut refined)
                      (get-in command [:scope :historical-cut]))
                   (= :conditional (:status refined) (get-in refined [:historical-judgment :status]))
                   (= original-command (:open-premise-fingerprints command))
                   (= 2 (count original-command))
                   (every? (:unaccepted-premise-fingerprints inventory) original-command)
                   (= 16 (count accepted))
                   (original-authority-binding? binding)
                   (original-authority-acceptance? declaration)
                   (every? #(= binding (get-in % [:http-route :authority-binding]))
                           (:selected-command-origins command)))
              :configured-inventory-command-subject-mismatch)
    (let [description (original-command-description command (:policy binding))
          expected (assoc description :status :conditional-configured-command-authority
                          :subject-provenance :matched-original-issued-cut
                          :configured-authority :matched-current-local-installation
                          :installation-binding binding :deployment-adoption :configured-local-host
                          :premise-acceptance :explicit-declaration-only
                          :trusted-assumptions (:open-premise-fingerprints description)
                          :deployment-acceptance declaration)
          family (implementation-family! :command-authority adopted (:obligations adopted))
          combined (merge index (:index family))]
      (require! (and (= expected adopted)
                     (= fps (:fingerprints family))
                     (= 2 (count fps))
                     (empty? (set/intersection all fps))
                     (= (+ (count all) 2) (count combined) (count (set (map :id (vals combined))))))
                :configured-inventory-command-authority-mismatch)
      (-> inventory
          (assoc :profile command-authority-profile
                 :conditional-command-attribution (original-attribution command adopted)
                 :premises-by-fingerprint combined
                 :dependency-order (dependency-order! combined)
                 :command-authority-relevance {:scope :original-selected-command-only
                                               :premise-fingerprints fps
                                               :original-command-premises original-command
                                               :general-condition-links :not-derived}
                 :all-described-open-premise-fingerprints (set/union all fps)
                 :deployment-declared-premise-fingerprints (set/union accepted fps))
          (assoc-in [:premise-families :command-authority] fps)
          (assoc-in [:adopted-by-contract :command-authority] fps)
          (assoc-in [:adoption-scopes :command-authority] (:scope adopted))))))
