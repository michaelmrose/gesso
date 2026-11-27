(ns gesso.assurance.reference-transition-argument-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-transition-argument :as argument]
            [gesso.assurance.reference-transition-refinement :as refinement]
            [gesso.assurance.reference-transition-refinement-test :as fixture]
            [gesso.assurance.reference-recovery-inventory :as inventory]
            [gesso.assurance.reference-mapped-observed-recovery :as recovery]
            [gesso.assurance.proof.propositional-semantics :as logic]))

(deftest conditional-equational-simulation-and-case-coverage
  (let [r (argument/derive!)
        cases (:cases r)
        commit (:committed-abstract-transition cases)
        abort (:aborted-stutter cases)]
    (is (= :conditional-equational-derivation (:status r)) (pr-str r))
    (is (= #{:committed-abstract-transition :aborted-stutter :maintenance-stutter} (set (keys cases))))
    (is (contains? (:facts commit) (:normalized-goal commit)))
    (is (= [:= [:project :actual-before] [:project :actual-before]] (:normalized-goal abort)))
    (is (= :assumed-not-derived (get-in r [:case-split :coverage])))
    (is (= 16 (get-in r [:case-split :entailment :valuations-checked])))
    (is (= 7 (get-in r [:case-split :entailment :premise-satisfying-valuations])))
    (is (= :not-established (:universal-refinement r)))
    (is (= :not-performed (:proof-discharge r)))
    (doseq [bad [(update commit :equalities dissoc :actual-before)
                 (update commit :equalities dissoc :actual-after)
                 (assoc commit :facts #{})
                 (assoc-in commit [:goal 1] :other-alternative)
                 (assoc-in commit [:goal 2] :other-inputs)
                 (assoc abort :equalities {})
                 (assoc-in abort [:equalities :actual-after] :prepared)]]
      (is (= :equational-goal-not-derived
             (try (#'argument/check-case! bad)
                  (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))
  (with-redefs [logic/enumerate-entailment! (constantly {:result :entails})]
    (is (= :argument-boundary-replaced (:reason (argument/derive!))))))

(defn- adjacent [f]
  (let [without-maintenance #(filterv (fn [entry] (not= 3 (:tx-id entry))) %)
        ledger (without-maintenance (:ledger f))
        points (#'refinement/selected-points ledger)
        by-side (into {} (map (fn [[point image]] [[(:tx-id point) (:side point)] image])) (:images f))]
    (-> f (update :inputs without-maintenance)
        (assoc :ledger ledger :points points
               :images (into {} (map (fn [point] [point (by-side [(:tx-id point) (:side point)])])) points))
        (update-in [:checked :checked-boundaries] without-maintenance)
        (assoc-in [:comparison :intervening-maintenance] []))))

(defn- selection [maintenance?]
  (let [f (cond-> (#'fixture/selection-fixture) (not maintenance?) adjacent)
        report (refinement/check-selected! f)
        checked (:checked f)
        ids #{:fixture/invariant}
        subject {:catalog-fingerprint "catalog" :abstract-induction-fingerprint "induction"
                 :selected-operations (mapv #(hash-map :alternative-id (get-in % [:spec :alternative])) (:steps f))}
        cut {:payload-fingerprint "history"}
        physical (#'recovery/physical-refinement-premises
                  {:scope {:publication-source-id "claim" :historical-cut cut}
                   :command-observation-ref "cut" :judgment-subject subject
                   :original-recovery {:manifest-tx-id 0 :initialization-tx-id 1
                                       :selected-tx-ids [2 4 5] :selected-commitments [:committed :aborted :committed]
                                       :intervening-maintenance (if maintenance? [{:tx-id 3 :maintenance :lease-acquisition :intent-id (random-uuid)}] [])}})
        abstract {:fingerprint "induction" :declared-invariant-ids ids :declared-operation-ids #{:fixture/op}
                  :classification {:original-obligations {:status :all-original-obligations-remain-open
                                                          :by-operation {:fixture/op #{:fixture/opaque}}}
                                   :certificate {:open-premises #{:fixture/induction}}
                                   :open {:assumptions #{:fixture/meta} :assumption-classes {:meta #{:fixture/meta}}
                                          :certificate-premises #{:fixture/induction}}}}
        empty-family {:obligations [] :open-premise-fingerprints #{} :premise-acceptance :not-performed}
        storage {:obligations [] :trusted-assumptions #{} :premise-acceptance :explicit-declaration-only}
        inv (inventory/describe-inventory abstract storage empty-family empty-family physical)
        checked (assoc checked :recovery-cut-ref "cut" :historical-cut cut :judgment-subject subject
                       :catalog-fingerprint "catalog" :retained-payload-fingerprint "history" :invariant-ids ids
                       :physical-readbacks {:status :matched :checked-coordinates (:points f) :transition-correspondence report}
                       :conditional-physical-invariant {:status :conditional
                                                        :conclusion :declared-invariants-hold-on-selected-physical-projections
                                                        :conditional-on #{:faithful-pinned-provider-readbacks :faithful-authoritative-state-projection
                                                                          :faithful-invariant-expression-semantics}
                                                        :checked-coordinates (:points f) :invariant-ids ids})]
    {:checked checked :physical physical :inventory inv :abstract abstract}))

(defn- bind [{:keys [checked physical inventory abstract]}]
  (argument/bind-selected! checked physical inventory abstract))

(deftest selected-binding-does-not-generalize-premises
  (let [f (selection true) r (bind f)
        by-id (into {} (map (juxt (comp :id :assumption) :premise-fingerprint)) (get-in f [:physical :obligations]))
        links (:related-selected-premise-fingerprints r)]
    (is (= :conditional-selected-argument-bound (:status r)) (pr-str r))
    (is (= #{(by-id :reference-physical/guarded-atomic-write)} (:successful-guard-establishes-exact-prepared-image links)))
    (is (= #{(by-id :reference-physical/abort-atomicity)} (:abort-preserves-complete-mapped-image links)))
    (is (= #{(by-id :reference-physical/writer-coverage)} (:complete-admitted-writer-and-temporal-boundary-coverage links)))
    (is (= #{:source-derived-put-image-satisfies-original-abstract-relation :faithful-original-abstract-transition-semantics}
           (:conditions-without-direct-selected-binding r)))
    (is (= (get-in f [:physical :open-premise-fingerprints]) (:original-open-physical-premise-fingerprints r)))
    (is (= :not-established (:selected-premises-entail-general-conditions r) (:universal-refinement r)))
    (is (= :not-performed (:premise-acceptance r) (:proof-discharge r)))
    (doseq [bad [(assoc-in f [:checked :recovery-cut-ref] "foreign")
                 (assoc-in f [:checked :physical-readbacks :transition-correspondence :steps 0 :checked-row-effect] :other)
                 (assoc-in f [:checked :physical-readbacks :transition-correspondence :steps 0 :alternative-id] :fixture/foreign)
                 (assoc-in f [:checked :physical-readbacks :transition-correspondence :steps 2 :relation] :committed-abstract-transition)
                 (update-in f [:checked :physical-readbacks :transition-correspondence :checked-coordinates] pop)
                 (assoc-in f [:checked :physical-readbacks :transition-correspondence :proof-discharge] :performed)
                 (assoc-in f [:inventory :premises-by-fingerprint (by-id :reference-physical/guarded-atomic-write) :discharge-state] :discharged)
                 (assoc-in f [:inventory :premises-by-fingerprint (by-id :reference-physical/writer-coverage) :dependency-fingerprints] #{})
                 (assoc-in f [:inventory :premises-by-fingerprint (by-id :reference-physical/abort-atomicity) :acceptance] :explicit-declaration-only)]]
      (let [bad (bind bad)]
        (is (= :not-established (:status bad)))
        (is (not (contains? bad :related-selected-premise-fingerprints)))))
    (with-redefs [inventory/describe-transfer-binding (constantly {:status :bound})]
      (is (= :argument-boundary-replaced (:reason (bind f)))))))

(deftest adjacent-selected-history-does-not-cover-future-maintenance
  (let [f (selection false) r (bind f)]
    (is (= :conditional-selected-argument-bound (:status r)) (pr-str r))
    (is (= 10 (count (:original-open-physical-premise-fingerprints r))))
    (is (= #{:source-derived-put-image-satisfies-original-abstract-relation
             :faithful-original-abstract-transition-semantics
             :qualified-maintenance-preserves-complete-mapped-image :faithful-maintenance-attribution}
           (:conditions-without-direct-selected-binding r)))
    (is (= :not-established (:selected-premises-entail-general-conditions r)))))
