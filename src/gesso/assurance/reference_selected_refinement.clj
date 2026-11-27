(ns gesso.assurance.reference-selected-refinement
  "Compose selected physical evidence with conditional source schemas.
   Internal diagnostic only; supplied reports confer no authority."
  (:require [clojure.set :as set]
            [gesso.assurance.reference-source-transition-argument :as commit]
            [gesso.assurance.reference-read-only-replay-argument :as replay]
            [gesso.assurance.reference-transition-argument :as generic]
            [gesso.assurance.reference-transition-refinement :as refinement]
            [gesso.assurance.reference-writer-temporal-coverage :as temporal]))

(def profile :conditional-selected-physical-refinement-v1)
(def ^:private derive-commit commit/derive!)
(def ^:private derive-replay replay/derive!)
(def ^:private derive-generic generic/derive!)

(defn- reject! [reason]
  (throw (ex-info "Unqualified selected refinement composition" {:reason reason})))

(defn- boundaries! []
  (when-not (and (identical? derive-commit commit/derive!)
                 (identical? derive-replay replay/derive!)
                 (identical? derive-generic generic/derive!))
    (reject! :refinement-composition-boundary-replaced)))

(defn compose!
  "Join the exact issued cut's boundary diagnostics to the classified
   suffix and diagonal intervals. The caller must supply rederived issued
   evidence, not arbitrary reports. Replay schemas cover declarations,
   not observed replay requests. No premise discharge or universal proof."
  [source declarations fingerprint cases classified cut partition intervals checked]
  (boundaries!)
  (let [w (derive-commit source declarations fingerprint cases)
        r (derive-replay source declarations fingerprint cases)
        g (derive-generic)
        alternatives (merge (:alternatives w) (:alternatives r))
        physical (:physical-readbacks checked)
        observed (:transition-correspondence physical)
        ids (:transaction-ids cut)
        initial (second ids)
        spans (:intervals intervals)
        points (into [{:tx-id initial :side :initial
                       :system-time (:from (first spans)) :valid-time (:from (first spans))}]
                     (mapcat (fn [{:keys [to-tx-id from to]}]
                               [{:tx-id to-tx-id :side :prestate :system-time from :valid-time to}
                                {:tx-id to-tx-id :side :poststate :system-time to :valid-time to}])) spans)
        observed-steps (:steps observed)]
    (when-not (and (= :conditional-current-installed-writer-classification (:status classified))
                   (= :conditional-catalog-commit-composition (:status w))
                   (= :conditional-catalog-replay-derivation (:status r))
                   (= :conditional-equational-derivation (:status g))
                   (= fingerprint (:catalog-fingerprint classified) (:catalog-fingerprint w)
                      (:catalog-fingerprint r) (:catalog-fingerprint checked))
                   (= (:alternative-identities classified)
                      (into {} (map (fn [[id a]] [id (:identity a)])) alternatives))
                   (= :checked (:status checked)) (= :matched (:status physical))
                   (= cut (:historical-cut checked))
                   (= (:payload-fingerprint cut) (:retained-payload-fingerprint checked))
                   (= refinement/selected-profile (:profile observed))
                   (= :conditional-selected-transition-correspondence (:status observed))
                   (= :exact-complete-mapped-tables (:row-correspondence observed))
                   (= :exact-retained-expectation-with-qualified-issued-envelope-premise (:envelope-binding observed))
                   (= (:target-map source) (:field-coordinates observed))
                   (= temporal/profile (:profile intervals))
                   (= :conditional-selected-diagonal-coverage (:status intervals))
                   (= (:protected-tables classified) (:protected-tables intervals))
                   (seq spans) (= (count partition) (count spans) (count observed-steps))
                   (= (subvec ids 2) (mapv :tx-id partition) (mapv :to-tx-id spans) (mapv :tx-id observed-steps))
                   (= (vec (butlast (subvec ids 1))) (mapv :from-tx-id spans))
                   (= (mapv :to (butlast spans)) (mapv :from (rest spans)))
                   (= points (:checked-coordinates physical) (:checked-coordinates observed))
                   (= (subvec ids 1) (mapv :tx-id (:checked-boundaries checked)))
                   (= :not-performed (:proof-discharge w) (:proof-discharge r) (:proof-discharge g)
                      (:proof-discharge observed) (:proof-discharge intervals)))
      (reject! :refinement-composition-evidence-mismatch))
    (let [steps (mapv
                 (fn [step interval observation]
                   (let [id (get-in step [:alternative :alternative-id])
                         argument (get-in w [:alternatives id])
                         [schema relation effect] (case (:class step)
                                                    :committed-mapped [:source-bound-committed-transition :committed-abstract-transition :exact-source-derived-puts]
                                                    :aborted-attempt [:aborted-stutter :aborted-stutter :exact-row-stutter]
                                                    :committed-worker [:maintenance-stutter :maintenance-stutter :exact-mapped-row-stutter]
                                                    (reject! :unknown-refinement-class))]
                     (when-not (and (= schema (:schema interval))
                                    (= :none (:intervening-protected-boundaries interval))
                                    (= relation (:relation observation)) (= effect (:checked-row-effect observation))
                                    (if (= :committed-worker (:class step))
                                      (and (nil? (:alternative-id observation))
                                           (= :conditional-mapped-stutter (get-in classified [:reviewed-policy-branches (:policy-branch step) :effect])))
                                      (and argument (= (:alternative step) (:identity argument))
                                           (= id (:alternative-id observation))))
                                    (if (= :committed-mapped (:class step))
                                      (= :conditional-committed-transition-derivation (:status argument))
                                      (= :derived-by-equality-substitution (get-in g [:cases relation :status]))))
                       (reject! :refinement-composition-step-mismatch))
                     (assoc step :schema schema :relation relation)))
                 partition spans observed-steps)]
      (boundaries!)
      {:profile profile :status :conditional-selected-physical-refinement
       :scope :selected-initialization-to-last-transaction-diagonal
       :steps steps :declared-replay-schemas (set (keys (:alternatives r)))
       :replay-observation-coverage :not-established :universal-refinement :not-established
       :writer-coverage :not-established :proof-discharge :not-performed :authority :none :certificate :not-issued
       :remaining-conditions (set/union (:remaining-conditions classified) (:remaining-conditions w)
                                        (:remaining-conditions r) (:remaining-conditions intervals)
                                        (:conditions observed)
                                        #{:faithful-selected-physical-and-temporal-evidence
                                          :qualified-initialization-and-continuous-mediation})})))
