(ns gesso.assurance.reference-storage-identity
  "Describe open durable-source obligations, not evidence of their truth."
  (:require [clojure.string :as str]
            [gesso.assurance.assumption :as assumption]))

(def profile :selected-retained-storage-identity-obligations-v2)

(def relations
  "Historical relations only. Neither grants current writer authority."
  #{:same-store-incarnation :authorized-historical-lineage})

(def ^:private required-keys
  #{:comparison-id :publication-source-id :origin-observation-ref
    :retained-acquisition-ref :relation :identity-policy
    :origin-boundary :acquisition-boundary :identity-boundary :historical-cut})

(def cut-profile
  "Selected local framed payload binding format, not an identity credential."
  :xtdb-2.2.0-beta1/selected-local-payload-binding-v1)

(def ^:private cut-keys
  #{:payload-binding-profile :record-offsets :transaction-ids
    :processed-message-id :payload-fingerprint})

(defn- offset? [x]
  (and (integer? x) (<= 0 x Long/MAX_VALUE)))

(defn- ordered-offsets? [xs maximum]
  (and (vector? xs) (nil? (meta xs)) (<= (count xs) maximum)
       (every? offset? xs)
       (every? (fn [[a b]] (< a b)) (partition 2 1 xs))))

(defn historical-cut?
  "Validate a declared cut shape only; no acquisition or identity evidence."
  [cut]
  (and (map? cut) (not (record? cut)) (nil? (meta cut))
       (= cut-keys (set (keys cut)))
       (= cut-profile (:payload-binding-profile cut))
       (let [{:keys [record-offsets transaction-ids processed-message-id
                     payload-fingerprint]} cut]
         (and (ordered-offsets? record-offsets 1024)
              (ordered-offsets? transaction-ids 256)
              (or (empty? record-offsets) (= 0 (first record-offsets)))
              (= transaction-ids (filterv (set transaction-ids) record-offsets))
              (integer? processed-message-id)
              (= processed-message-id (or (peek record-offsets) -1))
              (string? payload-fingerprint)
              (boolean (re-matches #"[0-9a-f]{64}" payload-fingerprint))))))

(defn- named? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- reference? [x]
  (and (string? x) (not (str/blank? x)) (<= (count x) 1024)))

(defn- reject! [reason]
  (throw (ex-info "Invalid storage identity obligation scope."
                  {:error/type ::invalid-scope :reason reason})))

(defn describe-obligations
  "Describe the exact open premises of one historical source comparison.

   References and boundary names are DECLARATIONS, not authenticated IDs,
   handles, samples, identity witnesses or proof terms. The eventual trusted
   checker must bind both actual observations (including the whole acquired
   sample) to their storage incarnations, then establish the selected
   relation at the observed cut under the named identity policy. An
   authorized historical lineage permits authorized restore/replica descent
   preserving the relevant prefix; it neither asserts the same physical
   incarnation nor authorizes promotion or writes. An incarnation denotes
   one operationally identified store, not a fixed set of filesystem inodes.

   The required historical cut declares the ordered complete prefix (including
   maintenance records and all transaction identities, not just commits), its
   fully processed frontier and the selected framed payload digest. A target
   acquisition may extend that prefix; its actual binding and prefix
   preservation must be checked separately. The digest is sensitive internal
   comparison material; do not publish this scope or its premise descriptions
   as public reports or receipts. Shape checks do not establish content,
   completeness, transaction outcomes or the truth of any lifecycle premise.

   No provider call, observation lookup, lineage validation or discharge is
   performed. A copied description is equally unadmitted. Do not use this
   result as a positive field of the retained-content diagnostic."
  [scope]
  (when-not (and (map? scope) (not (record? scope)) (nil? (meta scope))
                 (= required-keys (set (keys scope))))
    (reject! :invalid-obligation-scope))
  (when-not (every? #(named? (get scope %))
                    [:comparison-id :identity-policy :origin-boundary
                     :acquisition-boundary :identity-boundary])
    (reject! :invalid-boundary-or-policy-name))
  (when-not (every? #(reference? (get scope %))
                    [:publication-source-id :origin-observation-ref :retained-acquisition-ref])
    (reject! :invalid-observation-reference))
  (when-not (contains? relations (:relation scope))
    (reject! :unsupported-storage-identity-relation))
  (when-not (historical-cut? (:historical-cut scope))
    (reject! :invalid-historical-cut))
  (let [subject {:kind :runtime :id (:comparison-id scope)
                 :interface (assoc scope :profile profile)}
        assumption-scope {:kind :runtime :id (:comparison-id scope)}
        origin-id :reference-storage/origin-binding
        acquisition-id :reference-storage/acquisition-binding
        relation-id (case (:relation scope)
                      :same-store-incarnation :reference-storage/same-incarnation
                      :authorized-historical-lineage :reference-storage/authorized-lineage)
        describe (fn [id proposition boundary dependencies origin]
                   (assumption/describe-assumption
                    {:id id :subject subject :scope assumption-scope
                     :proposition proposition :parameters {}
                     :semantic-context {:profile profile :identity-policy (:identity-policy scope)}
                     :dependencies dependencies :origin origin
                     :implementation-boundary boundary}))]
    {:profile profile
     :relation (:relation scope)
     :obligations [(describe origin-id
                             [:reference-storage/bind-actual-origin-observation]
                             (:origin-boundary scope) #{} :implementation-correspondence)
                   (describe acquisition-id
                             [:reference-storage/bind-complete-retained-acquisition]
                             (:acquisition-boundary scope) #{} :implementation-correspondence)
                   (describe relation-id
                             [relation-id :reference-storage/at-observed-cut]
                             (:identity-boundary scope) #{origin-id acquisition-id} :environment)]
     :observation-storage-identity :not-established
     :authorized-historical-lineage :not-established
     :authority :none :certificate :not-issued}))

(def selection-profile :selected-open-storage-premises-v1)

(defn- reject-selection! [reason]
  (throw (ex-info "Invalid selection of open storage identity premises."
                  {:error/type ::invalid-selection :reason reason})))

(defn describe-selected-premises
  "Compose the exact, dependency-closed selection for a declared scope.

   Offered premises must be raw assumption declarations, not reports,
   fingerprints, handles or verdicts. Re-derive the required premises from
   scope, require each exactly once and match them structurally using the
   assumption checker. Return canonical required descriptions, independent
   of input order. Selection expresses only the proposed conditional
   premises, not their acceptance by a deployment or their discharge.

   Scope and selection remain sensitive declarative material. This function
   neither accepts a historical comparison nor authenticates a provider,
   lifecycle event, writer, or identity relation."
  [scope offered]
  (let [description (describe-obligations scope)
        required (mapv :assumption (:obligations description))
        ids (set (map :id required))]
    (when-not (and (vector? offered) (nil? (meta offered))
                   (<= (count offered) (count required))
                   (every? #(and (map? %) (not (record? %)) (nil? (meta %))) offered))
      (reject-selection! :invalid-premise-selection))
    (let [offered-ids (mapv :id offered)
          selected-ids (set offered-ids)]
      (when-not (= (count offered-ids) (count selected-ids))
        (reject-selection! :duplicate-premise))
      (when-not (= ids selected-ids)
        (reject-selection! :incorrect-premise-set))
      (let [by-id (into {} (map (juxt :id identity)) offered)]
        (doseq [premise required]
          (assumption/match-premise! premise (get by-id (:id premise)))
          (when-not (every? selected-ids (:dependencies premise))
            (reject-selection! :missing-premise-dependency)))
        (assoc description
               :selection-profile selection-profile
               :selection :exact-structural-only
               :dependency-closure :complete-for-declared-premises
               :premise-acceptance :not-performed)))))

(def acceptance-profile :declared-deployment-storage-trust-v1)

(defn describe-deployment-acceptance
  "Describe an explicit deployment declaration of trust in the exact open
   premises. The deployment name is a declaration, not an authenticated
   principal. The caller must obtain this declaration at its trusted
   deployment boundary; this function cannot establish that provenance.
   Preserve the canonical open obligations and their dependency closure.
   No checking, admission, discharge, identity or writer authority follows."
  [scope declaration]
  (when-not (and (map? declaration) (not (record? declaration))
                 (nil? (meta declaration))
                 (= #{:deployment-id :identity-policy :acceptance :premises}
                    (set (keys declaration)))
                 (named? (:deployment-id declaration))
                 (= :trust-open-premises (:acceptance declaration)))
    (reject-selection! :invalid-deployment-acceptance))
  (when-not (= (:identity-policy scope) (:identity-policy declaration))
    (reject-selection! :acceptance-policy-mismatch))
  (let [selected (describe-selected-premises scope (:premises declaration))]
    (assoc selected
           :acceptance-profile acceptance-profile
           :premise-acceptance :explicit-declaration-only
           :deployment-acceptance {:deployment-id (:deployment-id declaration)
                                   :identity-policy (:identity-policy declaration)
                                   :authentication :not-established}
           :trusted-assumptions (set (map :premise-fingerprint (:obligations selected))))))
