(ns gesso.assurance.reference-installed-writer-classification
  "Bind the reviewed classification to one current mapped-policy generation.
   The result is diagnostic, not a durable capability or physical closure."
  (:require [gesso.assurance.reference-writer-classification :as classification]
            [gesso.assurance.reference-mapped-decoder :as decoder]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.outbox.writer-policy :as policy]
            [gesso.model.inductive-plan :as induction]))

(def profile :conditional-installed-writer-classification-v1)
(def ^:private original-classification classification/derive!)
(def ^:private original-decoder decoder/compare-issued-closed!)
(def ^:private original-admission induction/require-current-mapped-admission!)
(def ^:private original-init induction/check-mapped-initialization!)
(def ^:private original-token xtdb/current-submission-policy-token)
(def ^:private original-root policy/activated-policy-root)

(defn- boundaries! []
  (when-not (and (identical? original-classification classification/derive!)
                 (identical? original-decoder decoder/compare-issued-closed!)
                 (identical? original-admission induction/require-current-mapped-admission!)
                 (identical? original-init induction/check-mapped-initialization!)
                 (identical? original-token xtdb/current-submission-policy-token)
                 (identical? original-root policy/activated-policy-root))
    (throw (ex-info "Substituted installed classification boundary" {:reason :installed-classification-boundary-replaced}))))

(defn- reject! [reason]
  (throw (ex-info "Unqualified installed writer classification" {:reason reason})))

(defn derive!
  "Recheck the exact issued admission, registered decoder and mapping, then
   match the live policy's captured manifest/initializer cut. Recheck the
   generation after the derivation. No independent retained-history survey,
   future activation guarantee, or proof of absence of bypass writers."
  [node admission source declarations fingerprint cases]
  (let [base {:profile profile :authority :none :certificate :not-issued
              :proof-discharge :not-performed :writer-coverage :not-established
              :universal-refinement :not-established}]
    (try
      (boundaries!)
      (let [token (original-token node)
            {:keys [observation root] :as activation} (when token (original-root node admission token))]
        (when-not activation (reject! :missing-current-mapped-activation))
        (let [description (original-admission admission source)
              decoded (original-decoder admission source declarations)
              classified (original-classification source declarations fingerprint cases)
              mappings (:operation-mappings description)
              identities (:alternative-identities classified)
              {:keys [initializer descriptor]} root
              {:keys [source-id manifest-tx-id initialization-tx-id system-time]} observation
              checked (original-init admission initializer)]
          (when-not (and (= :conditional-declared-writer-classification (:status classified))
                         (= :complete (:declared-alternative-coverage classified))
                         (= :matched (:status decoded))
                         (= :matched-issued-closed-construction (:closed-decoder-binding decoded))
                         (= fingerprint (:catalog-fingerprint description) (:catalog-fingerprint classified)
                            (:catalog-fingerprint decoded) (:catalog-fingerprint observation))
                         (= (:induction-fingerprint description) (:induction-fingerprint decoded))
                         (= (:target-map source) (:target-map description))
                         (= (set (keys mappings)) (set (keys identities)))
                         (every? (fn [[id m]]
                                   (and (= (:fingerprint m) (get-in identities [id :mapping-fingerprint]))
                                        (= (:transition-fingerprint m) (get-in identities [id :transition-fingerprint])))) mappings)
                         (= (:protected-tables classified) (conj (:tables checked) :gesso_commands))
                         (every? #{:gesso_live_outbox} (:auxiliary-tables description)))
            (reject! :installed-catalog-or-footprint-mismatch))
          (when-not (and (= :matched (:manifest-envelope observation) (:initialization-envelope observation))
                         (= :checked-supplied-projection (:physical-correspondence observation))
                         (string? source-id) (seq source-id) (string? descriptor) (seq descriptor)
                         (integer? manifest-tx-id) (integer? initialization-tx-id)
                         (<= 0 manifest-tx-id) (< manifest-tx-id initialization-tx-id)
                         (instance? java.time.Instant system-time)
                         (= (select-keys checked [:catalog-fingerprint :case-fingerprint :initialization-fingerprint])
                            (select-keys observation [:catalog-fingerprint :case-fingerprint :initialization-fingerprint])))
            (reject! :installed-initialization-root-mismatch))
          (boundaries!)
          (when-not (and (identical? token (original-token node))
                         (= activation (original-root node admission token)))
            (reject! :installed-generation-changed))
          (merge base {:status :conditional-current-installed-writer-classification
                       :scope :one-current-local-mapped-policy-generation
                       :catalog-fingerprint fingerprint :induction-fingerprint (:induction-fingerprint description)
                       :declared-alternative-coverage :complete
                       :alternative-identities identities :protected-tables (:protected-tables classified)
                       :reviewed-policy-branches (:reviewed-policy-branches classified)
                       :activation-root (select-keys observation [:source-id :manifest-tx-id :initialization-tx-id
                                                                  :system-time :initialization-fingerprint])
                       :remaining-conditions (into (:remaining-conditions classified)
                                                   #{:faithful-captured-activation-and-retained-root
                                                     :complete-post-initialization-transaction-and-observation-coverage
                                                     :continued-exact-policy-mediation})})))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-installed-classification))))))
