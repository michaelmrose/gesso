(ns gesso.assurance.reference-admitted-step-argument
  "Conditional schema over arbitrary admitted steps, not a provider theorem."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.propositional-semantics :as logic]
            [gesso.assurance.reference-writer-classification :as classification]
            [gesso.assurance.reference-source-transition-argument :as commit]
            [gesso.assurance.reference-read-only-replay-argument :as replay]
            [gesso.assurance.reference-transition-argument :as generic]))

(def profile :conditional-general-admitted-step-argument-v1)
(def ^:private original-classify classification/derive!)
(def ^:private original-commit commit/derive!)
(def ^:private original-replay replay/derive!)
(def ^:private original-generic generic/derive!)
(def ^:private original-entailment logic/enumerate-entailment!)

(defn- reject! [reason]
  (throw (ex-info "Unqualified general admitted-step argument" {:reason reason})))

(defn- boundaries! []
  (when-not (and (identical? original-classify classification/derive!)
                 (identical? original-commit commit/derive!)
                 (identical? original-replay replay/derive!)
                 (identical? original-generic generic/derive!)
                 (identical? original-entailment logic/enumerate-entailment!))
    (reject! :admitted-step-boundary-replaced)))

(defn- atom-for [id] [:atom {:readers #{} :expression [:opaque id]}])

(defn- exhaustive-split! [ids]
  ;; The case atoms mean *qualified* instances of the schemas, not labels.
  ;; Exhaustiveness and the typed interpretation are not established by SAT.
  (let [atoms (mapv atom-for (sort ids))
        goal (atom-for :abstract-transition-or-projected-stutter)
        premises (into {::coverage (into [:or] atoms)}
                       (map (fn [id] [id [:implies (atom-for id) goal]])) ids)
        result (original-entailment premises goal)]
    (when-not (and (= :entails (:result result))
                   (= (dec (bit-shift-left 1 (count ids))) (:premise-satisfying-valuations result)))
      (reject! :admitted-step-case-split-not-derived))
    (when-not (every? #(= :counterexample (:result (original-entailment (dissoc premises %) goal)))
                      (keys premises))
      (reject! :admitted-step-missing-premise-not-detected))
    {:premises premises :goal goal :entailment result
     :coverage :assumed-not-derived :typed-atomic-semantics :not-interpreted
     :every-premise-necessary? true}))

(defn- stutter [condition]
  ;; For the very same functional projection, substituting the assumed
  ;; equality of endpoint images yields reflexivity. This is not an
  ;; observation of the physical endpoints or a proof of that equality.
  {:goal [:= [:project :before] [:project :after]]
   :equalities {:after :before}
   :normalized-goal [:= [:project :before] [:project :before]]
   :conditions #{condition :faithful-functional-authoritative-projection}
   :status :conditional-projected-stutter})

(defn- compose! [c w r g]
  (let [writers (:alternatives w) readers (:alternatives r)
        branches (:reviewed-policy-branches c)
        maintenance #{:lease-acquisition :lease-reclaim :dispatch-acknowledgment}
        identities (into {} (map (fn [[id a]] [id (:identity a)])) (merge writers readers))
        generic-cases (:cases g)]
    (when-not (and (= :conditional-declared-writer-classification (:status c))
                   (= :complete (:declared-alternative-coverage c))
                   (= :conditional-catalog-commit-composition (:status w))
                   (= :conditional-catalog-replay-derivation (:status r))
                   (= :conditional-equational-derivation (:status g))
                   (= (:catalog-fingerprint c) (:catalog-fingerprint w) (:catalog-fingerprint r))
                   (= :not-performed (:proof-discharge c) (:proof-discharge w) (:proof-discharge r) (:proof-discharge g))
                   (seq writers) (seq readers)
                   (empty? (set/intersection (set (keys writers)) (set (keys readers))))
                   (= (set (keys writers)) (:journal-writing-alternatives c))
                   (= (set (keys readers)) (:read-only-alternatives c) (:outside-fragment w))
                   (= identities (:alternative-identities c))
                   (every? #(= :conditional-committed-transition-derivation (:status %)) (vals writers))
                   (every? #(= :conditional-read-only-transition-derivation (:status %)) (vals readers))
                   (= :initial-root (get-in branches [:initializer :effect]))
                   (= :infrastructure-establishment (get-in branches [:manifest :effect]))
                   (= :conditional-abstract-transition (get-in branches [:mapped-operation :effect]))
                   (= (into maintenance [:initializer :manifest :mapped-operation]) (set (keys branches)))
                   (every? #(= :conditional-mapped-stutter (get-in branches [% :effect])) maintenance)
                   (every? #(= :derived-by-equality-substitution (:status %)) (vals generic-cases))
                   (= #{:committed-abstract-transition :aborted-stutter :maintenance-stutter} (set (keys generic-cases))))
      (reject! :admitted-step-schema-mismatch))
    (let [schemas {::committed {:conclusion :original-abstract-transition :alternatives writers
                                :conditions (:remaining-conditions w)}
                   ::aborted {:conclusion :projected-stutter :argument (:aborted-stutter generic-cases)
                              :conditions (get-in generic-cases [:aborted-stutter :conditions])}
                   ::maintenance {:conclusion :projected-stutter :branches maintenance
                                  :argument (:maintenance-stutter generic-cases)
                                  :conditions (conj (get-in generic-cases [:maintenance-stutter :conditions])
                                                    :maintenance-exact-footprint-and-provider-effect-correspondence)}
                   ::replay {:conclusion :original-read-only-transition :alternatives readers
                             :conditions (:remaining-conditions r) :scope :single-qualified-read-cut}
                   ::refusal {:conclusion :projected-stutter
                              :argument (stutter :local-refusal-preserves-complete-attributed-image)
                              :conditions #{:local-refusal-preserves-complete-attributed-image
                                            :no-local-refusal-attributed-mutations-or-submissions
                                            :faithful-functional-authoritative-projection}
                              :scope :single-attributed-observation-cut}
                   ::time-advance {:conclusion :projected-stutter
                                   :argument (stutter :protected-image-constant-across-time-advance)
                                   :conditions #{:protected-image-constant-across-time-advance
                                                 :faithful-complete-clock-and-valid-time-boundaries
                                                 :faithful-functional-authoritative-projection}}}
          coverage #{:complete-admitted-writer-and-temporal-boundary-coverage
                     :every-physical-step-instantiates-qualified-schema
                     :exact-actual-endpoint-and-attributed-observation-boundaries
                     :faithful-original-abstract-transition-semantics
                     :well-typed-complete-projected-endpoints
                     :continuous-exact-installed-policy-mediation}]
      {:schemas schemas :case-split (exhaustive-split! (keys schemas))
       :quantification {:domain :arbitrary-post-initialization-admitted-step-or-attributed-observation
                        :parameters [:physical-before :physical-after :step-identity :original-inputs :time-cut]
                        :antecedent :exhaustive-qualified-schema-instantiation
                        :consequent :original-abstract-transition-or-projected-stutter
                        :initial-root :separate-obligation}
       :coverage-conditions coverage
       :remaining-conditions (set/union coverage (:remaining-conditions c) (:remaining-conditions w)
                                        (:remaining-conditions r)
                                        (into #{} (mapcat :conditions) (vals schemas)))})))

(defn derive!
  "Re-derive source schemas without consuming finite execution evidence.
   The implication is general only under the stated universally required
   hypotheses. Neither exhaustiveness nor provider/loaded-code semantics
   nor a CLM-R2 implementation theorem is established."
  [source declarations fingerprint cases]
  (let [base {:profile profile :scope :conditional-post-initialization-step-schema
              :universal-refinement :not-established :writer-coverage :not-established
              :temporal-coverage :not-established :proof-discharge :not-performed
              :authority :none :certificate :not-issued}]
    (try
      (boundaries!)
      (let [args [source declarations fingerprint cases]
            result (compose! (apply original-classify args) (apply original-commit args)
                             (apply original-replay args) (original-generic))]
        (boundaries!)
        (merge base result {:status :conditional-general-admitted-step-derivation
                            :catalog-fingerprint fingerprint}))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-admitted-step))))))
