(ns gesso.assurance.reference-recovery-snapshot-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-recovery-invariant :as invariant]
            [gesso.assurance.reference-recovery-inventory :as inventory]
            [gesso.assurance.reference-recovery-snapshot :as snapshot]
            [gesso.assurance.reference-recovery-projection :as projection]
            [gesso.assurance.reference-recovery-expression :as expression])
  (:import [java.time Instant]))

(defn- fixture []
  (let [ids [0 1 2 3 4 5]
        ledger (mapv (fn [id] {:tx-id id :system-time (.plusSeconds Instant/EPOCH id)
                               :committed? (not= 4 id)}) ids)
        points (into [{:tx-id 1 :side :initial :system-time (.plusSeconds Instant/EPOCH 1)
                       :valid-time (.plusSeconds Instant/EPOCH 1)}]
                     (mapcat (fn [id] [{:tx-id id :side :prestate :system-time (.plusSeconds Instant/EPOCH (dec id))
                                        :valid-time (.plusSeconds Instant/EPOCH id)}
                                       {:tx-id id :side :poststate :system-time (.plusSeconds Instant/EPOCH id)
                                        :valid-time (.plusSeconds Instant/EPOCH id)}])) (range 2 6))
        cut {:transaction-ids (conj ids 6) :payload-fingerprint "payload"}
        subject {:catalog-fingerprint "catalog" :abstract-induction-fingerprint "induction"}
        invariants #{:fixture/invariant}
        bindings {:faithful-pinned-provider-readbacks "snapshot"
                  :faithful-authoritative-state-projection "projection"
                  :faithful-invariant-expression-semantics "expression"}
        required #{"source" "ledger" "snapshot" "projection" "expression"}
        premises {"source" [:reference-physical/source-log #{}]
                  "ledger" [:reference-physical/ledger-outcome #{"source"}]
                  "snapshot" [:reference-physical/snapshot-continuity #{"ledger"}]
                  "projection" [:reference-physical/authoritative-projection #{"snapshot"}]
                  "expression" [:reference-physical/invariant-expression-semantics #{}]}
        interface {:scope-kind :selected-historical-cut-only :recovery-cut-ref "cut" :historical-cut cut
                   :judgment-subject subject :manifest-tx-id 0 :initialization-tx-id 1
                   :selected-tx-ids [2 4 5] :selected-commitments [:committed :aborted :committed]
                   :selected-maintenance [{:tx-id 3}]}
        support {:status :conditional-premise-support :premise-state :open :proof-discharge :not-performed
                 :premise-acceptance :not-performed :authority :none :certificate :not-issued
                 :recovery-cut-ref "cut" :catalog-fingerprint "catalog" :invariant-ids invariants
                 :remaining-conditions #{:fixture/open-condition}}]
    {:checked {:status :checked :recovery-cut-ref "cut" :historical-cut cut :judgment-subject subject
               :catalog-fingerprint "catalog" :retained-payload-fingerprint "payload" :invariant-ids invariants
               :selected-tx-ids [2 4 5]
               :checked-boundaries (mapv #(hash-map :tx-id %1 :relation %2) (rest ids)
                                         [:checked-initial-projection :committed-abstract-transition :maintenance-stutter
                                          :aborted-stutter :committed-abstract-transition])
               :conditional-physical-invariant {:profile invariant/conditional-physical-profile :status :conditional
                                                :conclusion :declared-invariants-hold-on-selected-physical-projections
                                                :conditional-on (set (keys bindings)) :checked-coordinates points
                                                :invariant-ids invariants :proof-discharge :not-performed}
               :physical-readbacks {:profile invariant/physical-profile :status :matched :checked-coordinates points
                                    :snapshot-correspondence {:profile invariant/snapshot-profile :status :matched
                                                              :scope :selected-pinned-provider-observations-only
                                                              :database "xtdb" :token-echo :matched-before-and-after
                                                              :valid-time-continuity :matched-prior-poststate-to-next-prestate
                                                              :snapshot-system-time (.plusSeconds Instant/EPOCH 6)
                                                              :ledger ledger :checked-coordinates points
                                                              :provider-refinement :not-established :proof-discharge :not-performed
                                                              :authority :none :certificate :not-issued}}}
     :transfer {:profile inventory/transfer-profile :status :bound :proof-discharge :not-performed
                :recovery-cut-ref "cut" :abstract-report-fingerprint "induction" :invariant-ids invariants
                :condition-premise-fingerprints bindings :required-open-premise-fingerprints required}
     :inventory {:profile inventory/profile :proof-discharge :not-performed :abstract {:report-fingerprint "induction"}
                 :premise-families {:physical required} :unaccepted-premise-fingerprints required
                 :premises-by-fingerprint (into {} (map (fn [[fp [id deps]]]
                                                         [fp {:family :physical :id id :dependency-fingerprints deps
                                                              :acceptance :not-performed :discharge-state :open}])) premises)}
     :abstract {:fingerprint "induction" :identity {:catalog-fingerprint "catalog"} :declared-invariant-ids invariants}
     :physical {:scope interface :obligations (mapv (fn [[fp [id _]]] {:assumption {:id id} :premise-fingerprint fp}) premises)}
     :projection (assoc support :profile projection/binding-profile :premise-fingerprint "projection"
                        :snapshot-premise-fingerprint "snapshot" :checked-coordinates points)
     :expression (assoc support :profile expression/binding-profile :premise-fingerprint "expression")}))

(defn- bind [{:keys [checked transfer inventory abstract physical]}]
  (snapshot/bind-premise checked transfer inventory abstract physical))

(defn- compose [{:keys [checked transfer inventory projection expression]} support]
  (snapshot/compose-support checked transfer inventory support projection expression))

(deftest snapshot-binding-requires-original-ledger-and-open-chain
  (let [f (fixture) r (bind f)]
    (is (= :conditional-premise-support (:status r)) (pr-str r))
    (is (= #{"source" "ledger" "snapshot"} (:required-open-premise-fingerprints r)))
    (is (= :open (:premise-state r)))
    (is (= :not-performed (:premise-acceptance r) (:proof-discharge r)))
    (doseq [bad [(assoc-in f [:checked :recovery-cut-ref] "foreign")
                 (assoc-in f [:checked :historical-cut :transaction-ids] [0 1 2 4 5])
                 (assoc-in f [:checked :physical-readbacks :snapshot-correspondence :ledger 4 :committed?] true)
                 (assoc-in f [:checked :physical-readbacks :snapshot-correspondence :ledger 4 :system-time] Instant/EPOCH)
                 (assoc-in f [:checked :physical-readbacks :snapshot-correspondence :snapshot-system-time] Instant/EPOCH)
                 (assoc-in f [:checked :physical-readbacks :snapshot-correspondence :token-echo] :unknown)
                 (update-in f [:checked :physical-readbacks :snapshot-correspondence :checked-coordinates] pop)
                 (assoc-in f [:checked :conditional-physical-invariant :checked-coordinates 1 :valid-time] Instant/EPOCH)
                 (assoc-in f [:inventory :premises-by-fingerprint "source" :discharge-state] :discharged)
                 (assoc-in f [:inventory :premises-by-fingerprint "ledger" :acceptance] :explicit-declaration-only)
                 (assoc-in f [:inventory :premises-by-fingerprint "snapshot" :dependency-fingerprints] #{})
                 (update-in f [:transfer :required-open-premise-fingerprints] disj "source")
                 (assoc-in f [:abstract :fingerprint] "foreign")]]
      (let [bad (bind bad)]
        (is (= :not-established (:status bad)))
        (is (not (contains? bad :premise-fingerprint)))))))

(deftest compose-support-keeps-each-condition-and-all-open-dependencies
  (let [f (fixture) support (bind f) r (compose f support)]
    (is (= :conditional-support-composed (:status r)) (pr-str r))
    (is (= (get-in f [:transfer :required-open-premise-fingerprints]) (:required-open-premise-fingerprints r)))
    (is (= (:remaining-conditions support) (get-in r [:remaining-conditions-by-transfer-condition :faithful-pinned-provider-readbacks])))
    (is (= :not-established (:sufficiency r) (:universal-refinement r)))
    (is (= :not-performed (:premise-acceptance r) (:proof-discharge r)))
    (doseq [[bad s] [[f (assoc support :status :not-established)]
                     [f (assoc support :remaining-conditions #{})]
                     [f (assoc support :recovery-cut-ref "foreign")]
                     [(assoc-in f [:expression :premise-fingerprint] "projection") support]
                     [(assoc-in f [:projection :snapshot-premise-fingerprint] "foreign") support]
                     [(assoc-in f [:projection :checked-coordinates] []) support]
                     [(assoc-in f [:expression :premise-state] :discharged) support]
                     [(assoc-in f [:inventory :premises-by-fingerprint "expression" :discharge-state] :discharged) support]
                     [(update-in f [:transfer :required-open-premise-fingerprints] conj "extra") support]]]
      (let [bad (compose bad s)]
        (is (= :not-established (:status bad)))
        (is (not (contains? bad :conclusion)))))))
