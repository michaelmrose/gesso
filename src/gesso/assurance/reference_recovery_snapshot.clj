(ns gesso.assurance.reference-recovery-snapshot
  "Compose original selected snapshot observations with open premises.
   This is internal issued-cut evidence, not provider authentication."
  (:require [gesso.assurance.reference-recovery-invariant :as invariant]
            [gesso.assurance.reference-recovery-inventory :as inventory]
            [gesso.assurance.reference-recovery-expression :as expression]
            [gesso.assurance.reference-recovery-projection :as projection])
  (:import [java.time Instant]))

(def profile :selected-snapshot-premise-support-v1)
(def composition-profile :selected-conditional-physical-support-v1)

(defn- base [p]
  {:profile p :scope :original-selected-invariant-coordinates-only
   :premise-acceptance :not-performed :proof-discharge :not-performed
   :universal-refinement :not-established :authority :none :certificate :not-issued})

(defn- open? [inventory transfer fp id dependencies]
  (and (string? fp)
       (contains? (:required-open-premise-fingerprints transfer) fp)
       (contains? (:unaccepted-premise-fingerprints inventory) fp)
       (contains? (get-in inventory [:premise-families :physical]) fp)
       (= {:id id :family :physical :dependency-fingerprints dependencies
           :acceptance :not-performed :discharge-state :open}
          (get-in inventory [:premises-by-fingerprint fp]))))

(defn- points [ledger]
  (let [{:keys [tx-id system-time]} (second ledger)]
    (into [{:tx-id tx-id :side :initial :system-time system-time :valid-time system-time}]
          (mapcat (fn [[prior tx]]
                    [{:tx-id (:tx-id tx) :side :prestate :system-time (:system-time prior)
                      :valid-time (:system-time tx)}
                     {:tx-id (:tx-id tx) :side :poststate :system-time (:system-time tx)
                      :valid-time (:system-time tx)}]))
          (partition 2 1 (rest ledger)))))

(defn bind-premise
  "Recheck the selected ledger and coordinate sequence against the original
   cut and physical premise interface. No new provider read or trust grant."
  [checked transfer inventory abstract physical]
  (let [b (base profile)
        snapshot (get-in checked [:physical-readbacks :snapshot-correspondence])
        interface (:scope physical)
        subject (:judgment-subject checked)
        ledger (:ledger snapshot)
        boundaries (:checked-boundaries checked)
        selected (:selected-tx-ids interface)
        commitments (:selected-commitments interface)
        maintenance (mapv :tx-id (:selected-maintenance interface))
        by-id (into {} (map (fn [d] [(get-in d [:assumption :id]) (:premise-fingerprint d)])) (:obligations physical))
        source (by-id :reference-physical/source-log)
        outcome (by-id :reference-physical/ledger-outcome)
        fp (by-id :reference-physical/snapshot-continuity)
        snapshot-time (:snapshot-system-time snapshot)]
    (if (and (= :checked (:status checked)) (= :bound (:status transfer))
             (= inventory/transfer-profile (:profile transfer))
             (= inventory/profile (:profile inventory))
             (= invariant/physical-profile (get-in checked [:physical-readbacks :profile]))
             (= :matched (get-in checked [:physical-readbacks :status]))
             (= invariant/snapshot-profile (:profile snapshot)) (= :matched (:status snapshot))
             (= :selected-pinned-provider-observations-only (:scope snapshot))
             (= :matched-before-and-after (:token-echo snapshot))
             (= :matched-prior-poststate-to-next-prestate (:valid-time-continuity snapshot))
             (= "xtdb" (:database snapshot))
             (= :not-established (:provider-refinement snapshot))
             (= :not-performed (:proof-discharge snapshot) (:proof-discharge transfer) (:proof-discharge inventory))
             (= :none (:authority snapshot)) (= :not-issued (:certificate snapshot))
             (= :selected-historical-cut-only (:scope-kind interface))
             (string? (:recovery-cut-ref checked))
             (= (:recovery-cut-ref checked) (:recovery-cut-ref transfer) (:recovery-cut-ref interface))
             (= subject (:judgment-subject interface))
             (= (:historical-cut checked) (:historical-cut interface))
             (string? (:retained-payload-fingerprint checked))
             (= (:retained-payload-fingerprint checked) (get-in interface [:historical-cut :payload-fingerprint]))
             (string? (:catalog-fingerprint checked))
             (= (:catalog-fingerprint checked) (:catalog-fingerprint subject) (get-in abstract [:identity :catalog-fingerprint]))
             (= (:fingerprint abstract) (:abstract-induction-fingerprint subject)
                (:abstract-report-fingerprint transfer) (get-in inventory [:abstract :report-fingerprint]))
             (seq (:invariant-ids checked))
             (= (:invariant-ids checked) (:invariant-ids transfer) (:declared-invariant-ids abstract))
             (= selected (:selected-tx-ids checked)) (= 3 (count selected))
             (= [:committed :aborted :committed] commitments)
             (every? integer? (concat [(:manifest-tx-id interface) (:initialization-tx-id interface)] selected maintenance))
             (vector? ledger) (vector? boundaries)
             (let [ids (into [(:manifest-tx-id interface) (:initialization-tx-id interface)] (sort (into selected maintenance)))
                   cut-ids (get-in interface [:historical-cut :transaction-ids])
                   times (mapv :system-time ledger)]
               (and (apply < ids) (vector? cut-ids)
                    (= ids (vec (take (count ids) cut-ids)) (mapv :tx-id ledger))
                    (= (subvec ids 1) (mapv :tx-id boundaries))
                    (= (mapv #(if (= % (second selected)) false true) ids) (mapv :committed? ledger))
                    (= (into [true] (map #(not= :aborted-stutter (:relation %)) boundaries)) (mapv :committed? ledger))
                    (every? #(instance? Instant %) times) (instance? Instant snapshot-time)
                    (every? (fn [[a b]] (.isBefore ^Instant a ^Instant b)) (partition 2 1 times))
                    (not (.isBefore ^Instant snapshot-time ^Instant (peek times)))
                    (= (points ledger) (:checked-coordinates snapshot)
                       (get-in checked [:physical-readbacks :checked-coordinates])
                       (get-in checked [:conditional-physical-invariant :checked-coordinates]))))
             (= fp (get-in transfer [:condition-premise-fingerprints :faithful-pinned-provider-readbacks]))
             (= 3 (count (set [source outcome fp])))
             (open? inventory transfer source :reference-physical/source-log #{})
             (open? inventory transfer outcome :reference-physical/ledger-outcome #{source})
             (open? inventory transfer fp :reference-physical/snapshot-continuity #{outcome}))
      (assoc b :status :conditional-premise-support :premise-state :open
             :premise-fingerprint fp :source-log-premise-fingerprint source :ledger-outcome-premise-fingerprint outcome
             :required-open-premise-fingerprints #{source outcome fp}
             :recovery-cut-ref (:recovery-cut-ref checked) :catalog-fingerprint (:catalog-fingerprint checked)
             :invariant-ids (:invariant-ids checked) :checked-coordinates (:checked-coordinates snapshot)
             :remaining-conditions #{:faithful-retained-source-log-decoding-and-retention
                                     :faithful-provider-ledger-identity-and-outcome
                                     :faithful-provider-snapshot-and-valid-time-query-semantics
                                     :faithful-provider-token-decoder-and-loaded-runtime})
      (assoc b :status :not-established :reason :unqualified-original-snapshot-premise))))

(defn compose-support
  "Compose condition-specific support without inferring premise truth. All
   five dependencies remain open even when every support report qualifies."
  [checked transfer inventory snapshot projection expression]
  (let [b (base composition-profile)
        supports {:faithful-pinned-provider-readbacks snapshot
                  :faithful-authoritative-state-projection projection
                  :faithful-invariant-expression-semantics expression}
        binding (:condition-premise-fingerprints transfer)
        required (:required-open-premise-fingerprints transfer)
        conclusion (:conditional-physical-invariant checked)
        coordinates (:checked-coordinates conclusion)]
    (if (and (= :bound (:status transfer)) (= inventory/transfer-profile (:profile transfer))
             (= invariant/conditional-physical-profile (:profile conclusion)) (= :conditional (:status conclusion))
             (= :declared-invariants-hold-on-selected-physical-projections (:conclusion conclusion))
             (= (set (keys supports)) (:conditional-on conclusion) (set (keys binding)))
             (= (:recovery-cut-ref checked) (:recovery-cut-ref transfer))
             (= (:invariant-ids checked) (:invariant-ids conclusion) (:invariant-ids transfer))
             (seq coordinates) (= coordinates (:checked-coordinates snapshot) (:checked-coordinates projection))
             (= profile (:profile snapshot)) (= projection/binding-profile (:profile projection))
             (= expression/binding-profile (:profile expression))
             (= (:premise-fingerprint snapshot) (:snapshot-premise-fingerprint projection))
             (set? required) (set? (:required-open-premise-fingerprints snapshot))
             (= required (into (:required-open-premise-fingerprints snapshot) (vals binding)))
             (= 5 (count required))
             (open? inventory transfer (:source-log-premise-fingerprint snapshot) :reference-physical/source-log #{})
             (open? inventory transfer (:ledger-outcome-premise-fingerprint snapshot) :reference-physical/ledger-outcome
                    #{(:source-log-premise-fingerprint snapshot)})
             (open? inventory transfer (:premise-fingerprint snapshot) :reference-physical/snapshot-continuity
                    #{(:ledger-outcome-premise-fingerprint snapshot)})
             (open? inventory transfer (:premise-fingerprint projection) :reference-physical/authoritative-projection
                    #{(:premise-fingerprint snapshot)})
             (open? inventory transfer (:premise-fingerprint expression) :reference-physical/invariant-expression-semantics #{})
             (= :not-performed (:proof-discharge transfer) (:proof-discharge inventory) (:proof-discharge conclusion))
             (every? (fn [[condition report]]
                       (and (= :conditional-premise-support (:status report))
                            (= :open (:premise-state report))
                            (= (binding condition) (:premise-fingerprint report))
                            (= (:recovery-cut-ref checked) (:recovery-cut-ref report))
                            (= (:catalog-fingerprint checked) (:catalog-fingerprint report))
                            (= (:invariant-ids checked) (:invariant-ids report))
                            (= :not-performed (:premise-acceptance report) (:proof-discharge report))
                            (= :none (:authority report)) (= :not-issued (:certificate report))
                            (set? (:remaining-conditions report)) (seq (:remaining-conditions report)))) supports))
      (assoc b :status :conditional-support-composed
             :conclusion (:conclusion conclusion) :recovery-cut-ref (:recovery-cut-ref checked)
             :catalog-fingerprint (:catalog-fingerprint checked) :invariant-ids (:invariant-ids checked)
             :checked-coordinates coordinates :condition-premise-fingerprints binding
             :required-open-premise-fingerprints required
             :remaining-conditions-by-transfer-condition (into {} (map (fn [[k r]] [k (:remaining-conditions r)])) supports)
             :sufficiency :not-established)
      (assoc b :status :not-established :reason :unqualified-physical-support-composition))))
