(ns gesso.assurance.reference-clm-r2-binding
  "Original CLM-R2 requirement binding for the issued closed Claim fixture.
   No selected history, proof discharge or provider theorem."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.operation-set :as catalog]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.reference-admitted-step-argument :as admitted]
            [gesso.assurance.reference-mapped-decoder :as decoder]
            [gesso.model.inductive-plan :as induction]))

(def profile :conditional-clm-r2-original-obligation-binding-v1)
(def ^:private original-admission induction/require-current-mapped-admission!)
(def ^:private original-induction induction/recheck-mapped-induction!)
(def ^:private original-catalog catalog/check-operation-set!)
(def ^:private original-obligations obligation/derive-obligations!)
(def ^:private original-argument admitted/derive!)
(def ^:private original-decoder decoder/compare-issued-closed!)

(def ^:private ledger-target
  {:id :CLM-R2
   :statement "Every actual admitted XTDB Claim commit/rejection refines one abstract Claim step or stutters"
   :scope :physical-transaction-isolation-model-lowering-and-humanhelp-callback})

;; Relevance links, not entailments or sufficient premise sets.
(def ^:private related-conditions
  {[:implementation :production-transition-correspondence]
   #{:faithful-original-abstract-transition-semantics
     :every-physical-step-instantiates-qualified-schema
     :complete-admitted-writer-and-temporal-boundary-coverage}
   [:implementation :current-runtime-authorization]
   #{:authenticated-exact-command-binding-and-permitted-disclosure
     :original-typed-guard-holds-and-inputs-admitted}
   [:implementation :atomic-transaction-enforcement]
   #{:successful-guard-establishes-exact-prepared-image
     :commit-applies-exact-source-derived-put-image
     :abort-preserves-complete-mapped-image}
   [:implementation :physical-effect-containment]
   #{:disjoint-supplemental-effects-and-physical-identifiers
     :maintenance-exact-footprint-and-provider-effect-correspondence
     :no-replay-attributed-mutations-or-submissions
     :all-physical-writers-mediated-by-exact-installed-policy}
   [:implementation :runtime-type-correspondence]
   #{:well-typed-complete-projected-endpoints}
   [:assumption :declared-state-projection-coverage]
   #{:faithful-functional-authoritative-projection
     :faithful-declared-coordinate-meaning}})

(defn- reject! [reason]
  (throw (ex-info "Unqualified original CLM-R2 binding" {:reason reason})))

(defn- boundaries! []
  (when-not (and (identical? original-admission induction/require-current-mapped-admission!)
                 (identical? original-induction induction/recheck-mapped-induction!)
                 (identical? original-catalog catalog/check-operation-set!)
                 (identical? original-obligations obligation/derive-obligations!)
                 (identical? original-argument admitted/derive!)
                 (identical? original-decoder decoder/compare-issued-closed!))
    (reject! :clm-r2-boundary-replaced)))

(defn- bind-register! [source description abstract checked argument]
  (let [original (get-in source [:inductive-source :catalog])
        fp (:fingerprint checked)
        ids (:declared-operation-ids checked)
        register (:open-obligation-ids-by-operation checked)
        schemas (:schemas argument)
        alternatives (merge (get-in schemas [::admitted/committed :alternatives])
                            (get-in schemas [::admitted/replay :alternatives]))
        conditions (:remaining-conditions argument)
        mappings (:operation-mappings description)
        rows (into {} (map (juxt :operation-id identity)) (:operation-results checked))]
    (when-not (and (= :fixture/claim-epoch (:id original) (:catalog-id checked) (:catalog-id abstract))
                   (= :declared-abstract-operation-set-checked (:proof-status checked))
                   (= :original-sources-and-both-live-handles-rechecked (:checking abstract))
                   (= :source-bound-conditional-abstract-safety (:verdict abstract))
                   (= :all-original-obligations-remain-open (get-in abstract [:classification :original-obligations :status]))
                   (= :not-performed (:assumption-discharge abstract) (:proof-discharge argument))
                   (= :not-established (:production-certification abstract) (:universal-refinement argument)
                      (:writer-coverage argument) (:temporal-coverage argument))
                   (= :conditional-general-admitted-step-derivation (:status argument))
                   (= :assumed-not-derived (get-in argument [:case-split :coverage]))
                   (= :original-abstract-transition-or-projected-stutter (get-in argument [:quantification :consequent]))
                   (= :separate-obligation (get-in argument [:quantification :initial-root]))
                   (= fp (:catalog-fingerprint description) (:catalog-fingerprint argument)
                      (get-in abstract [:identity :catalog-fingerprint]))
                   (= (:induction-fingerprint description) (get-in abstract [:identity :induction-fingerprint]))
                   (= (:target-map source) (:target-map description))
                   (= ids (:declared-operation-ids abstract) (set (keys rows))
                      (set (keys mappings)) (set (keys alternatives)) (set (keys register)))
                   (= register (:original-open-obligations description)
                      (get-in abstract [:classification :original-obligations :by-operation]))
                   (set? conditions) (every? conditions (mapcat identity (vals related-conditions))))
      (reject! :clm-r2-original-register-mismatch))
    (let [bindings
          (into {} (map (fn [{:keys [id construction]}]
                          (let [ledger (original-obligations {:construction construction
                                                              :initial-condition (:initial-condition original)
                                                              :invariants (:invariants original)})
                                row (rows id) mapping (mappings id)
                                alternative (:identity (alternatives id))
                                descriptions (into {} (map (juxt :id identity))
                                                   (mapcat val (:obligations ledger)))]
                            (when-not (and (= :all-obligations-open (:status ledger))
                                           (= (:fingerprint ledger) (:obligation-fingerprint row))
                                           (= (:transition-fingerprint ledger) (:transition-fingerprint row)
                                              (:transition-fingerprint mapping) (:transition-fingerprint alternative))
                                           (= (:subject-fingerprint ledger) (:subject-fingerprint row) (:subject-fingerprint mapping))
                                           (= (:fingerprint mapping) (:mapping-fingerprint alternative))
                                           (= id (:alternative-id alternative)) (= fp (:catalog-fingerprint alternative))
                                           (= (register id) (:open-obligation-ids ledger) (set (keys descriptions)))
                                           (every? #(= :open (:status %)) (vals descriptions))
                                           (every? descriptions (keys related-conditions)))
                              (reject! :clm-r2-alternative-obligation-mismatch))
                            [id {:identity alternative :obligation-fingerprint (:fingerprint ledger)
                                 :open-obligations descriptions
                                 :related-condition-ids related-conditions
                                 :unlinked-obligation-ids (set/difference (set (keys descriptions)) (set (keys related-conditions)))}]))
                       (:operations original)))]
      {:original-obligations-by-operation bindings
       :original-open-obligation-ids-by-operation register
       :abstract-report-fingerprint (:fingerprint abstract)
       :abstract-open-assumptions (get-in abstract [:classification :open])
       :unlinked-general-conditions (set/difference conditions (into #{} cat (vals related-conditions)))
       :remaining-conditions conditions})))

(defn derive!
  "Recheck original issued source, decoder, obligations and general argument.
   No finite execution evidence is an input. The original CLM-R2 target,
   including real HumanHelp callback correspondence, remains open."
  [admission source declarations fingerprint cases]
  (let [base {:profile profile :target ledger-target :scope :issued-closed-claim-fixture-only
              :original-obligation-status :open :condition-link-semantics :related-not-entailing
              :original-target-coverage :not-established :universal-refinement :not-established
              :physical-evidence :not-consumed :proof-discharge :not-performed
              :premise-acceptance :not-performed :authority :none :certificate :not-issued}]
    (try
      (boundaries!)
      (let [description (original-admission admission source)
            abstract (original-induction admission)
            checked (original-catalog (get-in source [:inductive-source :catalog]))
            decoded (original-decoder admission source declarations)
            argument (original-argument source declarations fingerprint cases)]
        (when-not (and (= :matched (:status decoded))
                       (= :matched-issued-closed-construction (:closed-decoder-binding decoded))
                       (= fingerprint (:catalog-fingerprint decoded) (:fingerprint checked))
                       (= (:induction-fingerprint description) (:induction-fingerprint decoded))
                       (seq cases) (every? #(= :request/claim (second %)) (keys cases)))
          (reject! :clm-r2-decoder-or-command-mismatch))
        (let [binding (bind-register! source description abstract checked argument)]
          (boundaries!)
          (merge base binding {:status :conditional-original-clm-r2-binding
                               :catalog-fingerprint fingerprint
                               :induction-fingerprint (:induction-fingerprint description)
                               :argument argument})))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-clm-r2-binding))))))
